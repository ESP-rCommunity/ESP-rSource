#!/usr/bin/perl
use lib "/home/ibeausol/perl/lib/perl5/site_perl/5.8.8";
use Tie::Handle::CSV;
use strict;

# .csv files created by bps runs.
my $REFcsv  = "SL5_reference.csv";
my $TESTcsv = "SL5_test.csv";

# Determine number of lines in .csv files.
# (There is probably a more efficient way to do this.)
open (temp, "wc -l $REFcsv |");
my $wc_output = <temp>;
my $rows; my $rest;
($rows, $rest) = split(/ /,$wc_output,2);

# Results file that will be read in by xmgrace.
open(grace, ">compare.dat");

# Indicate .csv column numbers for variables of interest.
# Note that array indexing starts at zero rather than 1,
# hence the '-1'.
my $Qhx               = 2-1;
my $Twater_out        = 15-1;
my $Tgas_out          = 16-1;
my $TPEMHX_stack_side = 6-1;
my $TPEMHX_hr_side    = 8-1;
my @vars=($Qhx, $Twater_out, $Tgas_out, $TPEMHX_stack_side, $TPEMHX_hr_side);

# Analyze each variable of interest.
print "\n---------------------------------------\n";
print "Commencing analysis.\n";
print "---------------------------------------\n\n";
my $loop;
for ($loop=0; $loop<=$#vars; $loop++)
		{
  # Open these files for parsing using Tie::Handle::CSV.
  my $ref_results  = Tie::Handle::CSV->new($REFcsv , header=>0);
  my $test_results = Tie::Handle::CSV->new($TESTcsv, header=>0);

  # Echo the header row for the column being analyzed.
  print "Analyzing ", +(scalar <$ref_results>)->[$vars[$loop]], "\n";
  # Don't need to display the following.
  my $junk +(scalar <$test_results>)->[$vars[$loop]];
  my $junk +(scalar <$ref_results>)->[$vars[$loop]];
  my $junk +(scalar <$test_results>)->[$vars[$loop]];

  # Now loop thorugh the data rows in the file.
  my $sum   = 0;
  my $min   = 1;
  my $max   = 1;
  my $ratio = 0;
  my $count;

  for ($count=1; $count<=$rows-2; $count++)
		  {
    my $ref  = +(scalar <$ref_results> )->[$vars[$loop]], "\n";
    my $test = +(scalar <$test_results>)->[$vars[$loop]], "\n";
		  print grace $ref, $test, "\n";

  		$ratio = $test / $ref;
		  $sum = $sum + $ratio;
  		if ($ratio < $min) { $min = $ratio };
		  if ($ratio > $max) { $max = $ratio };
    }

  print "Analyzed $rows data points \n";
  print "Average ratio = ", $sum/($count-1), "\n";
  print "Minimum ratio = $min \n";
  print "Maximum ratio = $max \n\n";

# Close files.
		close $ref_results;
		close $test_results

}

print "---------------------------------------\n";
print "Analysis completed.\n";
print "---------------------------------------\n\n";
