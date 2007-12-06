# Stolen from Mac::PropertyList (by comdog) for use in Mac::PropertyList::SAX (by kulp)

use Test::More tests => 1;

use Mac::PropertyList::SAX;

my $expected = do { local $/; open my($fh), 'plists/test0.plist'; <$fh> };

my $structure = {
	a => 'b',
	c => [ 'd', 'e' ],
	f => {
		g => 'h',
		i => 1,
		j => [
			{ a => 'b' },
			2,
			"x",
		],
	},
};

my $string = Mac::PropertyList::SAX::create_from_ref($structure);
my $parsed = Mac::PropertyList::SAX::parse_plist_string($string);

is_deeply($parsed, $structure, "recursive serialization / deserialization");

