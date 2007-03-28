# Stolen from Mac::PropertyList (by comdog) for use in Mac::PropertyList::SAX (by kulp)

use Test::More tests => 1;

use Mac::PropertyList::SAX;

my $expected = <<'HERE';
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
	<key>c</key>
	<array>
		<string>d</string>
		<string>e</string>
	</array>
	<key>a</key>
	<string>b</string>
	<key>f</key>
	<dict>
		<key>g</key>
		<string>h</string>
		<key>j</key>
		<array>
			<dict>
				<key>a</key>
				<string>b</string>
			</dict>
			<string>2</string>
			<string>x</string>
		</array>
		<key>i</key>
		<string>1</string>
	</dict>
</dict>
</plist>
HERE

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

is($string, $expected, "recursive serialization");
