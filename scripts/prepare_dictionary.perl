use strict;
use warnings;
use v5.16.3;

print "module Dictionary (dictionaryWords) where\n\n";

print "dictionaryWords :: [String]\n";
print "dictionaryWords = [\n";

while (<>) {
    chomp;

    die "fail" if /"/;
    
    print '    "' . $_ . '",' . "\n";
}

print "]";
