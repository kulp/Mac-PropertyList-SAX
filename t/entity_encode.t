use Test::More tests => 5;

use Mac::PropertyList::SAX;

{
    my @input = (
        "<&>'",
        [ qw(& ' < > ") ],
        {
            c => [ '"', '"two"' ],
            f => {
                "'" => Mac::PropertyList::SAX::true->new,
                '&&&amp;' => 1,
                '><' => [ { a => ' foo & bar << 3 ' } ],
            },
        },
    );
    my @output = (
        "&lt;&amp;&gt;&apos;",
        [ qw(&amp; &apos; &lt; &gt; &quot;) ],
        {
            c => [ '&quot;', '&quot;two&quot;' ],
            f => {
                '&apos;' => Mac::PropertyList::SAX::true->new,
                '&amp;&amp;&amp;amp;' => 1,
                '&gt;&lt;' => [ { a => ' foo &amp; bar &lt;&lt; 3 ' } ],
            },
        },
    );


    while (defined(my $input = shift @input)) {
        my $output = shift @output;
        my $string = Mac::PropertyList::SAX::create_from_ref($input);
        my $parsed = Mac::PropertyList::SAX::parse_plist_string($string);

        is_deeply($parsed, $output, "XML entity encoding");
    }
}

{
    use Encode;
    my $parsed = Mac::PropertyList::SAX::parse_plist_file 'plists/test2.plist';
    is_deeply($parsed, { 'only' => [ '< & >', '< & >', '< & >', decode("utf-8",'☹') ] }, 'XML entity encoding from file');

    local $/;
    open $fh, 'plists/test3.plist';
    my $plist_str = <$fh>;
    is(Mac::PropertyList::SAX::plist_as_string($parsed), $plist_str, 'XML entity re-encoding to string');
}
