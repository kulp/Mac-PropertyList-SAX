=head1 NAME

Mac::PropertyList::SAX - work with Mac plists at a low level (with real XML
parsers)

=cut

package Mac::PropertyList::SAX;

=head1 SYNOPSIS

See L<Mac::PropertyList>

=head1 DESCRIPTION

L<Mac::PropertyList> is useful, but very slow on large files because it does
the parsing by itself, intead of handing off the actual XML parsing to a
dedicated parser. This module uses L<XML::SAX::ParserFactory> to select a
parser capable of doing the heavy lifting, reducing parsing time on large files
by up to 97%. This is especially useful for iTunes library plists which may be
megabytes in size and contain thousands of entries in dozens of dictionaries.

This module does not, however, replace Mac::PropertyList; in fact, it depends
on it for several package definitions and the plist creation routines.

Be aware that performance will depend largely on the parser that
L<XML::SAX::ParserFactory> selects for you; if you have not installed
L<XML::SAX::Expat> or another fast parser, the default L<XML::SAX::PurePerl>
parser will be used; this will probably give I<worse> performance than
L<Mac::PropertyList>, so ensure that a fast parser is being used before you
complain to me about performance :-). See L<XML::SAX::ParserFactory> for
information on how to set which parser is used.

=cut

use strict;
use warnings;

# Passthrough functions
use Mac::PropertyList qw(
	plist_as_string 
);
use XML::SAX::ParserFactory;

use base qw(Exporter);
use vars qw($VERSION @EXPORT_OK %EXPORT_TAGS);
@EXPORT_OK = qw(
	parse_plist 
	parse_plist_fh
	parse_plist_file
	plist_as_string
	create_from_ref
	create_from_hash
	create_from_array
);

%EXPORT_TAGS = (
	'all' => \@EXPORT_OK,
);
	
=head1 VERSION

Version 0.07

=cut

$VERSION = '0.07';

=head1 FUNCTIONS

=over 4

=item parse_plist_file

See L<Mac::PropertyList/parse_plist_file>

=cut
sub parse_plist_file {
	open my($fh), $_[0];
	parse_plist_fh($fh);
}

=item parse_plist_fh

See L<Mac::PropertyList/parse_plist_fh>

=cut
sub parse_plist_fh { my ($fh) = @_;	parse_plist(do { local $/; <$fh> }) }

=item parse_plist

See L<Mac::PropertyList/parse_plist>

=cut
sub parse_plist { _parse($_[0]) }

=item _parse

Parsing method called by parse_plist_* (internal use only)

=cut
sub _parse {
	my ($data) = @_;
	my $handler = Mac::PropertyList::SAX::Handler->new;
	my $parser = XML::SAX::ParserFactory->parser(Handler => $handler);
	$parser->parse_string($data);
	$handler->{struct};
}

=item create_from_ref( HASH_REF | ARRAY_REF )

Create a plist dictionary from an array or hash reference.

The values of the hash can be simple scalars or references. References are
handled recursively. Reference trees containing Mac::PropertyList objects
will be handled correctly (use case: easily combining parsed plists with
"regular" Perl data). All scalars are treated as strings (use Mac::PropertyList
objects to represent integers or other types of scalars).

Returns a string representing the hash in the plist format.

=cut

sub create_from_ref {
	$Mac::PropertyList::XML_head .
		(join "\n", _handle_value(shift)) . "\n" .
		$Mac::PropertyList::XML_foot;

	sub _handle_value {
		my ($val) = @_;

		# We could hand off serialization of all Mac::PropertyList::Item objects
		# but there is no 'write' method defined for it (though all its
		# subclasses have one). Let's just handle Scalars, which are safe.
		   if (UNIVERSAL::isa($val, 'Mac::PropertyList::Scalar')) { $val->write }
		elsif (UNIVERSAL::isa($val,                      'HASH')) {  _hash_recurse($val) }
		elsif (UNIVERSAL::isa($val,                     'ARRAY')) { _array_recurse($val) }
		else { Mac::PropertyList::string->new($val)->write }
	}

	sub _hash_recurse {
		my ($hash) = @_;
		Mac::PropertyList::dict->write_open,
			(map { "\t$_" } map {
				Mac::PropertyList::dict->write_key($_),
				_handle_value($hash->{$_}) } keys %$hash),
			Mac::PropertyList::dict->write_close
	}

	sub _array_recurse {
		my ($array) = @_;
		Mac::PropertyList::array->write_open,
			(map { "\t$_" } map { _handle_value($_) } @$array),
			Mac::PropertyList::array->write_close
	}
}

=item create_from_hash( HASH_REF )

Provided for backward compatibility with Mac::PropertyList: merely an alias to
create_from_ref.

=cut

sub create_from_hash  { &create_from_ref(@_) }

=item create_from_array( ARRAY_REF )

Provided for backward compatibility with Mac::PropertyList: merely an alias to
create_from_ref.

=cut

sub create_from_array { &create_from_ref(@_) }

package Mac::PropertyList::Scalar;
use overload '""' => sub { shift->as_basic_data };

package Mac::PropertyList::SAX::Handler;

use strict;
use warnings;
use enum qw(EMPTY TOP FREE DICT ARRAY);

use Carp qw(carp croak);
use MIME::Base64;
use Text::Trim;

use base qw(XML::SAX::Base);

sub new {
	my %args = (
		ROOT			=> 'plist',
		accum			=> "",
		context			=> EMPTY,
		key				=> undef,
		stack			=> [ ],
		struct			=> undef,
		# From the plist DTD
		complex_types	=> [ qw(array dict) ],
		numerical_types	=> [ qw(real integer true false) ],
		simple_types	=> [ qw(data date real integer string true false) ],
		types			=> [ qw(array data date dict real integer string true false) ],
	);
	shift->SUPER::new(%args, @_);
}

sub start_element {
	my $self = shift;
	my ($data) = @_;

	if ($self->{context} == EMPTY && $data->{Name} eq $self->{ROOT}) {
		$self->{context} = TOP;
	} elsif ($self->{context} == TOP) {
		push @{ $self->{stack} }, { context	=> TOP };

		if (!_in($data->{Name}, @{ $self->{types} })) {
			croak "Top-level element in plist is not a recognized type";
		} elsif ($data->{Name} eq 'dict') {
			$self->{struct} = Mac::PropertyList::dict->new;
			$self->{context} = DICT;
		} elsif ($data->{Name} eq 'array') {
			$self->{struct} = Mac::PropertyList::array->new;
			$self->{context} = ARRAY;
		} else {
			$self->{context} = FREE;
		}
	} elsif (_in($data->{Name}, @{ $self->{complex_types} })) {
		push @{ $self->{stack} }, {
			key		=> $self->{key},
			context	=> $self->{context},
			struct	=> $self->{struct},
		};
		if ($data->{Name} eq 'array') {
			$self->{struct} = Mac::PropertyList::array->new;
			$self->{context} = ARRAY;
		} elsif ($data->{Name} eq 'dict') {
			$self->{struct} = Mac::PropertyList::dict->new;
			$self->{context} = DICT;
			undef $self->{key};
		}
	} elsif ($data->{Name} ne 'key'
			and !_in($data->{Name}, @{ $self->{simple_types} })) {
		# If not a key or a simple value (which require no action here), die
		croak "Received invalid start element $data";
	}
}

sub end_element {
	my $self = shift;
	my ($data) = @_;

	if ($data->{Name} eq $self->{ROOT}) {
		# Discard plist element
	} elsif ($data->{Name} eq 'key') {
		$self->{key} = trim $self->{accum};
		$self->{accum} = "";
	} else {
		if (_in($data->{Name}, @{ $self->{complex_types} })) {
			my $elt = pop @{ $self->{stack} };
			if ($elt->{context} != TOP) {
				my $oldstruct = $self->{struct};
				($self->{struct}, $self->{key}, $self->{context}) = @{$elt}{qw(struct key context)};
				if ($self->{context} == DICT) {
					$self->{struct}->{$self->{key}} = $oldstruct;
				} elsif ($self->{context} == ARRAY) {
					push @{ $self->{struct} }, $oldstruct;
				}
				undef $self->{key};
			}
		} else {
			# Get accumulated character data
			my $value = $self->{accum};
			if ($data->{Name} eq 'data') {
				$value = MIME::Base64::decode_base64($value);
			} else {
				# TODO: Is leading/trailing whitespace is ever significant?
				$value = trim $value;
			}
			# Wrap in an object
			$value = "Mac::PropertyList::$data->{Name}"->new($value);
			if ($self->{context} == DICT) {
				$self->{struct}{$self->{key}} = $value;
			} elsif ($self->{context} == ARRAY) {
				push @{ $self->{struct} }, $value;
			} elsif ($self->{context} == FREE) {
				$self->{struct} = $value;
			}
		}
		$self->{accum} = "";
	}
}

sub characters { shift->{accum} .= shift->{Data} }

sub _in {
	my $item = shift;
	scalar grep { $_ } map { $item eq $_ } @_;
}

1;

__END__

=back

=head1 BUGS / TODO

Behavior is not I<exactly> the same as L<Mac::PropertyList>'s; specifically, in
the case of special characters, such as accented characters and ampersands.
Ampersands encoded (for example, as '&#38;') in the original property list will
be decoded by the XML parser in this module; L<Mac::PropertyList> leaves them
as-is. Also, accented/special characters are converted into '\x{ff}' sequences
by the XML parser in this module, but are preserved in their original encoding
by L<Mac::PropertyList>. The differences may be evident when creating a plist
file from a parsed data structure, but this has not yet been tested.

Please report any bugs or feature requests to C<bug-mac-propertylist-sax at
rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Mac-PropertyList-SAX>.  I will
be notified, and then you'll automatically be notified of progress on your bug
as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Mac::PropertyList::SAX

You can also look for information at:

=over 4

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Mac-PropertyList-SAX>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Mac-PropertyList-SAX>

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Mac-PropertyList-SAX>

=item * Search CPAN

L<http://search.cpan.org/dist/Mac-PropertyList-SAX>

=back

=head1 AUTHOR

Darren M. Kulp, C<< <darren at kulp.ch> >>

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

