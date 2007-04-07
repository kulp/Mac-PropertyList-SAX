use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'Mac::PropertyList::SAX',
    AUTHOR              => 'Darren M. Kulp <darren@kulp.ch>',
    VERSION_FROM        => 'lib/Mac/PropertyList/SAX.pm',
    ABSTRACT_FROM       => 'lib/Mac/PropertyList/SAX.pm',
    LICENSE             => 'perl',
    PL_FILES            => {},
    PREREQ_PM => {
        'Alias'                        => 0,
        'Carp'                         => 0,
        'constant'                     => 0,
        'enum'                         => 0,
        'Exporter'                     => 0,
        'Mac::PropertyList'            => 0,
        'MIME::Base64'                 => 0,
        'Test::More'                   => 0,
        'UNIVERSAL::isa'               => 0,
        'XML::SAX::Base'               => 0,
        'XML::SAX::ParserFactory'      => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'Mac-PropertyList-SAX-*' },
);
