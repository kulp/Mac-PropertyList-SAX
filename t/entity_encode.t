use Test::More tests => 3;

use Mac::PropertyList::SAX;

my @structures = (
    "<&>'",
    [ qw(& ' < > ") ],
    {
        c => [ '"', '"two"' ],
        f => {
            "'" => Mac::PropertyList::SAX::true->new,
            '&&&amp;' => 1,
            '><' => [ { a => ' foo & bar << 3 ' }, ],
        },
    },
);

for my $structure (@structures) {
    my $string = Mac::PropertyList::SAX::create_from_ref($structure);
    my $parsed = Mac::PropertyList::SAX::parse_plist_string($string);

    is_deeply($parsed, $structure, "XML entity encoding");
}

