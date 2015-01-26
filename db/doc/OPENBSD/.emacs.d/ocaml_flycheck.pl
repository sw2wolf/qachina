#!/usr/bin/env perl
# ocaml_flycheck.pl

use strict;
use warnings;

### Please rewrite the following 2 variables 
### ($ocamlc, @ocamlc_options)

my $ocamlc = 'ocamlc';          # where is ocamlc
my @ocamlc_options  = ('-c -thread unix.cma threads.cma graphics.cma'); # e.g. ('-fglasgow-exts');
my @ocamlc_packages = ();

### the following should not been edited ###

use File::Temp qw /tempfile tempdir/;
File::Temp->safe_level( File::Temp::HIGH );

my ($source, $base_dir) = @ARGV;

my @command = ($ocamlc);

while (@ocamlc_options) {
  push(@command, shift @ocamlc_options);
}

push (@command,    $source);

while (@ocamlc_packages) {
  push(@command, '-package');
  push(@command, shift @ocamlc_packages);
}

my $dir = tempdir( CLEANUP => 1 );
my ($fh, $filename) = tempfile( DIR => $dir );

system("@command >$filename 2>&1");

open(MESSAGE, $filename);
my $column = "";
while (<MESSAGE>) {
  # example message  {File "robocupenv.ml", line 133, characters 6-10:
  if (/^File "(\S+\.ml[yilp]?)", line (\d+), characters (\d+)-(\d+):\s?(.*)/) {
    print $column;
    my $error = (<MESSAGE>);       # get the next line
    chomp $error;
    print "\n"; 
    print "$1:$2:$3:";
    $column = " [$3-$4]";
    if ($error =~ /Warning(.*)/) {
	    print "$error";
    } else {
	    print "$error ";
    }
    next;
  }
  if (/\s+(.*)/) {
    my $rest = $1;
    chomp $rest;
    print $rest;
    print " ";
    next;
  }
}

close($fh);
print "$column\n";
