=head1 NAME

Mac::PropertyList::SAX - work with Mac plists at a low level, fast

=cut

package Mac::PropertyList::SAX;

=head1 SYNOPSIS

See L<Mac::PropertyList>

=head1 DESCRIPTION

L<Mac::PropertyList> is useful, but very slow on large files because it does
XML parsing itself, intead of handing it off to a dedicated parser. This module
uses L<XML::SAX::ParserFactory> to select a parser capable of doing the heavy
lifting, reducing parsing time on large files by a factor of 30 or more.

This module does not replace Mac::PropertyList: it depends on it for some
package definitions and plist printing routines. You should, however, be able
to replace all "use Mac::PropertyList" lines with "use Mac::PropertyList::SAX"
and notice an immediate improvement in performance on large input files.

Performance will depend largely on the parser that L<XML::SAX::ParserFactory>
selects for you; if you have not installed L<XML::SAX::Expat> or another fast
parser, the default L<XML::SAX::PurePerl> parser will be used, which may give
I<worse> performance than L<Mac::PropertyList>. See L<XML::SAX::ParserFactory>
for information on how to set which parser is used.

=cut

use strict;
use warnings;

# Passthrough function
use Mac::PropertyList qw(plist_as_string);
use UNIVERSAL::isa qw(isa);
use XML::SAX::ParserFactory;

use base qw(Exporter);

our @EXPORT_OK = qw(
    parse_plist 
    parse_plist_fh
    parse_plist_file
    plist_as_string
    create_from_ref
    create_from_hash
    create_from_array
);

our %EXPORT_TAGS = (
    all    => \@EXPORT_OK,
    create => [ qw(create_from_ref create_from_hash create_from_array plist_as_string) ],
    parse  => [ qw(parse_plist parse_plist_fh parse_plist_file) ],
);

=head1 VERSION

Version 0.64

=cut

our $VERSION = '0.64';

=head1 EXPORTS

By default, no functions are exported. Specify individual functions to export
as usual, or use the tags ':all', ':create', and ':parse' for the appropriate
sets of functions (':create' includes the create* functions as well as
plist_as_string; ':parse' includes the parse* functions).

=head1 FUNCTIONS

=over 4

=item parse_plist_file

See L<Mac::PropertyList/parse_plist_file>

=cut

sub parse_plist_file {
    my $file = shift;

    return parse_plist_fh($file) if ref $file;

    unless(-e $file) {
        carp("parse_plist_file: file [$file] does not exist!");
        return;
    }

    parse_plist_fh(do { local $/; open my($fh), $file; $fh });
}

=item parse_plist_fh

See L<Mac::PropertyList/parse_plist_fh>

=cut

sub parse_plist_fh { my $fh = shift; parse_plist(do { local $/; <$fh> }) }

=item parse_plist

See L<Mac::PropertyList/parse_plist>

=cut

sub parse_plist {
    my ($data) = @_;

    my $handler = Mac::PropertyList::SAX::Handler->new;
    XML::SAX::ParserFactory->parser(Handler => $handler)->parse_string($data);

    $handler->{struct}
}

=item create_from_ref( HASH_REF | ARRAY_REF )

Create a plist from an array or hash reference.

The values of the hash can be simple scalars or references. References are
handled recursively, and Mac::PropertyList objects are output correctly.
All plain scalars are treated as strings (use Mac::PropertyList objects to
represent other types of scalars).

Returns a string representing the hash in the plist format.

=cut

sub create_from_ref {
    # use "real" local subs to protect internals
    local *_handle_value = sub {
        my ($val) = @_;

        local *_handle_hash = sub {
            my ($hash) = @_;
            Mac::PropertyList::SAX::dict->write_open,
                (map { "\t$_" } map {
                    Mac::PropertyList::SAX::dict->write_key($_),
                    _handle_value($hash->{$_}) } keys %$hash),
                Mac::PropertyList::SAX::dict->write_close
        };

        local *_handle_array = sub {
            my ($array) = @_;
            Mac::PropertyList::SAX::array->write_open,
                (map { "\t$_" } map { _handle_value($_) } @$array),
                Mac::PropertyList::SAX::array->write_close
        };

        # We could hand off serialization of all Mac::PropertyList::Item objects
        # but there is no 'write' method defined for it (though all its
        # subclasses have one). Let's just handle Scalars, which are safe.
           if (isa $val, 'Mac::PropertyList::Scalar') { $val->write }
        elsif (isa $val,                      'HASH') { _handle_hash ($val) }
        elsif (isa $val,                     'ARRAY') { _handle_array($val) }
        else { Mac::PropertyList::SAX::string->new($val)->write }
    };

    $Mac::PropertyList::XML_head .
        (join "\n", _handle_value(shift)) . "\n" .
        $Mac::PropertyList::XML_foot;
}

=item create_from_hash( HASH_REF )

Provided for backward compatibility with Mac::PropertyList: aliases
create_from_ref.

=cut

*create_from_hash = \&create_from_ref;

=item create_from_array( ARRAY_REF )

Provided for backward compatibility with Mac::PropertyList: aliases
create_from_ref.

=cut

*create_from_array = \&create_from_ref;

package Mac::PropertyList::SAX::Handler;

use strict;
use warnings;
# State definitions
use enum qw(S_EMPTY S_TOP S_FREE S_DICT S_ARRAY S_KEY S_TEXT);

use Carp qw(carp croak);
# Make use strict 'vars' usable with Alias
use Alias qw(attr); $Alias::AttrPrefix = 'main::';
use MIME::Base64;

# Element-name definitions
use constant +{ qw( ROOT  plist
                    KEY   key
                    DATA  data
                    DICT  dict
                    ARRAY array ) };

use base qw(XML::SAX::Base);

# From the plist DTD
our (%types, %simple_types, %complex_types, %numerical_types);
{
    my @complex_types   = (DICT, ARRAY);
    my @numerical_types = qw(real integer true false);
    my @simple_types    = qw(data date real integer string true false);
    my @types           = (@complex_types, @numerical_types, @simple_types);

    my $atoh = sub { map { $_ => 1 } @_ };

    %types           = $atoh->(@          types);
    %simple_types    = $atoh->(@   simple_types);
    %complex_types   = $atoh->(@  complex_types);
    %numerical_types = $atoh->(@numerical_types);
}

sub new {
    my %args = (
        accum   => "",
        context => S_EMPTY,
        key     => undef,
        stack   => [ ],
        struct  => undef,
    );

    shift->SUPER::new(%args, @_)
}

sub start_element {
    my $self = attr shift;
    my ($data) = @_;
    my $name = $data->{Name};

    # State transition definitions
         if ($::context == S_EMPTY and $name eq ROOT) {
             $::context  = S_TOP;
    } elsif ($::context == S_TOP or $types{$name} or $name eq KEY) {
        push @::stack, {
            key     => $::key,
            context => $::context,
            struct  => $::struct,
        };

        if ($name eq DICT) {
            $::struct = Mac::PropertyList::SAX::dict->new;
            $::context = S_DICT;
            undef $::key;
        } elsif ($name eq ARRAY) {
            $::struct = Mac::PropertyList::SAX::array->new;
            $::context = S_ARRAY;
        }
        elsif ($simple_types{$name}      ) { $::context = S_TEXT }
        elsif (              $name eq KEY) { $::context = S_KEY  }
        elsif (       $types{$name}      ) { $::context = S_FREE }
        else { croak "Top-level element '$name' in plist is not recognized" }
    } else {
        croak "Received invalid start element '$name'";
    }
}

sub end_element {
    my $self = attr shift;
    my ($data) = @_;
    my $name = $data->{Name};

    if ($name eq ROOT) {
        # Discard plist element
    } elsif (@::stack) {
        my $elt = pop @::stack;

        my $value = $::struct;
        ($::struct, $::key, $::context) = @{$elt}{qw(struct key context)};

        if ($simple_types{$name}) {
            # Wrap accumulated character data in an object
            $value = "Mac::PropertyList::SAX::$name"->new(
                $name eq DATA ? MIME::Base64::decode_base64($::accum)
                              : $::accum);

            undef $::accum;
        } elsif ($name eq KEY) {
            $::key = $::accum;
            undef $::accum;
        }

           if ($::context == S_DICT ) {       $::struct->{$::key} = $value }
        elsif ($::context == S_ARRAY) { push @$::struct,            $value }
        elsif ($::context == S_TEXT
            or $::context == S_FREE
            or $::context == S_TOP  ) {       $::struct           = $value }
    } else {
        croak "End-element received when stack already empty";
    }
}

sub characters {
    my $self = attr shift;
    my ($data) = @_;
    $::accum .= $data->{Data} if $::context == S_TEXT or $::context == S_KEY;
}

# Convenient subclasses
package Mac::PropertyList::SAX::array;
use base qw(Mac::PropertyList::array);
use overload '""' => sub { $_[0]->as_basic_data };
package Mac::PropertyList::SAX::dict;
use base qw(Mac::PropertyList::dict);
use overload '""' => sub { $_[0]->as_basic_data };
package Mac::PropertyList::SAX::Scalar;
use base qw(Mac::PropertyList::Scalar);
use overload '""' => sub { $_[0]->as_basic_data };
package Mac::PropertyList::SAX::date;
use base qw(Mac::PropertyList::SAX::Scalar Mac::PropertyList::date);
package Mac::PropertyList::SAX::real;
use base qw(Mac::PropertyList::SAX::Scalar Mac::PropertyList::real);
package Mac::PropertyList::SAX::integer;
use base qw(Mac::PropertyList::SAX::Scalar Mac::PropertyList::integer);
package Mac::PropertyList::SAX::string;
use base qw(Mac::PropertyList::SAX::Scalar Mac::PropertyList::string);
package Mac::PropertyList::SAX::data;
use base qw(Mac::PropertyList::SAX::Scalar Mac::PropertyList::data);
package Mac::PropertyList::SAX::Boolean;
use base qw(Mac::PropertyList::Boolean);
use overload '""' => sub { $_[0]->value };
package Mac::PropertyList::SAX::true;
use base qw(Mac::PropertyList::SAX::Boolean Mac::PropertyList::true);
package Mac::PropertyList::SAX::false;
use base qw(Mac::PropertyList::SAX::Boolean Mac::PropertyList::false);

1;

__END__

=back

=head1 BUGS / CAVEATS

Ampersands encoded (for example, as '&#38;') in the original property list will
be decoded by the XML parser in this module; L<Mac::PropertyList> leaves them
as-is. Also, accented/special characters are converted into '\x{ff}' sequences
by the XML parser, but are preserved in their original encoding by
L<Mac::PropertyList>.

Unlike Mac::PropertyList and old versions (< 0.60) of Mac::PropertyList::SAX,
this module does not trim leading and trailing whitespace from plist elements.
The difference in behavior is thought to be rarely noticeable; in any case, I
believe this module's current behavior is the more correct. Any documentation
that covers this problem would be appreciated.

The behavior of create_from_hash and create_from_array has changed: these
functions (which are really just aliases to the new create_from_ref function)
are now capable of recursively serializing complex data structures. For inputs
that Mac::PropertyList's create_from_* functions handled, the output should be
the same, but the reverse is not true.

=head1 SUPPORT

Please contact the author with bug reports or feature requests.

=head1 AUTHOR

Darren M. Kulp, C<< <kulp @ cpan.org> >>

=head1 THANKS

brian d foy, who created the L<Mac::PropertyList> module whose tests were
appropriated for this module.

=head1 SEE ALSO

L<Mac::PropertyList>, the inspiration for this module.

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2007 by Darren Kulp

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.

=cut

# vi: set et ts=4 sw=4: #
