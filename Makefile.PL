use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'Mac::PropertyList::SAX',
    AUTHOR              => 'Darren M. Kulp <darren@kulp.ch>',
    VERSION_FROM        => 'lib/Mac/PropertyList/SAX.pm',
    ABSTRACT_FROM       => 'lib/Mac/PropertyList/SAX.pm',
    PL_FILES            => {},
    PREREQ_PM => {
        'Carp' => 0,
        'Exporter' => 0,
        'MIME::Base64' => 0,
        'Mac::PropertyList' => 0,
        'Test::More' => 0,
        'Text::Trim' => 0,
        'XML::SAX::Base' => 0,
        'XML::SAX::ParserFactory' => 0,
        'enum' => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'Mac-PropertyList-SAX-*' },
);
