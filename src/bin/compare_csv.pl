#!/usr/bin/perl

# compare_csv.pl
#
# see definition of $msg, below, for information

use Cwd;
use Math::Trig;

$msg="
 compare_csv.pl:

 This perl script will compare two or more csv files and report on the 
 differences in the values contained within.

 USAGE:

 comp.pl [-sVALUE -tVALUE -oOUTPUT_FILE -v] file_1 file_2 file_3 ...

 OPTIONS:

   -sVALUE       - Sets the suppression threshold. Absolute errors
                   below this amount will be ignored. Default: 0.

   -tValue       - Sets the tolerance for close-to-zero comparisons.
                   Default: 0.001.

   -oOUTPUT_FILE - Specifies a target file for results. If none is
                   provided, results will be sent to the screen.

   -lVALUE       - number of lines containing meta data (ie:
                   variable names and units) Default: 2

   -v            - Specifies verbose output. Reports results for all
                   columns.


 SYNOPSYS:

 compare_csv.pl reads the contents of two or more
 comma-separated-value files, and looks for differences.  The first
 file is assumed to be the reference file, and the subsequent files
 are assumed to be test files. compare_csv.pl will extract the values
 contained under each column in the reference file, and compare them
 with the values contained under similarly titled columns in each of
 the test files. The column exhibiting the maximum normailzed error 
 in each file will be identified. The maximum normalized error is 
 calculated as follows:


 Absolute_error = absolute value ( test_file_value - ref_file_value )

 Max_absolute_error = MAX (  Absolute_errors )

                                  Max_absolute_error
 Normalized_error = ------------------------------------------------
                    MAX ( ref_file_values ) - MIN ( ref_file_values)


";

#initialize options to defaults 
$debug_me = 0;

$curr_dir = getcwd();

$first_file = 1;

#close-to-zero comparison:

$small = 0.001;

$supress_threshold = 0.0;

$output_file = "";

$verbose = 0;

$meta_lines = 2;

if (!@ARGV){

  die $msg;
}

foreach $arg (@ARGV){
  if ($arg=~/--?h/){
   die $msg;
  }elsif ($arg =~/^-d/ ){
    # debugging on.
    $debug_me = 1;
  }elsif ($arg =~/^-t/ ){
    $small = $arg;
    $small =~ s/-t[\s]*//g;
  }elsif ($arg =~/^-o/ ){
    $output_file = $arg;
    $output_file =~ s/-o[\s]*//g;
  }elsif ($arg =~/^-l/ ){
    $meta_lines = $arg;
    $meta_lines =~ s/-l[\s]*//g;
  }elsif ($arg =~/^-s/ ){
    $suppress_threshold = $arg;
    $suppress_threshold =~ s/-s[\s]*//g;
  }elsif ($arg =~/^-v/ ){
    $verbose = 1;
  }else{
    push @files, $arg;
    if ($first_file) {
      $ref_file = $arg;
      $first_file = 0;
      $debug .= "-Reference file: $ref_file \n";
    }
  }
}

# Read files into one massive hash
print "opening:\n";
foreach $file (@files){
  open(SOURCE_FILE,"$file") or die("Could not open $file");
  $line_count = 0;
  while ($line = <SOURCE_FILE> ) {
    if (eval($file eq $ref_file)){
      print "\r -> $file (reference file), Line: $line_count";
    }else{
      print "\r -> $file, Line: $line_count";
    }
    $line =~ s/\s//g;  # nuke white space
    $line =~ s/,$//g;  # nuke trailing white space
    push @file_lines, $line;
    $line_count++;
  }
  $line_count++;
  print "\n";
  $all_lines{$file} = [ @file_lines ];
  undef @file_lines;
}

close(SOURCE_FILE);

# get file headers from first row of reference file
print "reading headers\n";
@headers = split /,/, $all_lines{$ref_file}[0];


#link headers to units
$column_count =0;
if($meta_lines > 2){
  @unit_temp   = split /,/, $all_lines{$ref_file}[1];
  foreach $header (@headers){
    $units{$header} = $unit_temp[$column_count];
  }
}

if ($debug_me){
  $debug .=  "-Reference file headers:\n";
  foreach $header (@headers){
    $debug .= "  $header $units{$header} \n";
  }
}

# take master line-by-line hash, and break it
# into a bigger hash that contains each value 
# for each header in each file

print "extracting values:\n";
for $file ( keys %all_lines ){
  @file_lines = @{ $all_lines{$file} };
  $line_count = 1;
  $row_count = 0;
  foreach $line (@file_lines){
    print "\r -> $file, Row: $row_count                             ";
    if ($line_count > $meta_lines ){
      @file_values = split /,/, $line;
      $column_count = 0;
      foreach $value (@file_values) {
	$header = $headers[$column_count];
	$all_values{$file}{$headers[$column_count]}[$row_count] = $value;

	#$debug .= "-saving: \n ->$file \n ->$header\n ->row:$row_count\n ->column:$column_count ->Value: $value\n";

	$column_count++;
      }
      $row_count++;
    }
    $line_count++;
  }
  print "\n";
  if ($file == $ref_file ){
    $ref_file_row_count = $row_count;
  }
}



$row_count = 0;
$column_count = 0;

# compare each value in each header in each row to that 
# found in reference file 

print "Comparing files:\n";
# row-loop: row in reference file empty?
while ($all_values{$ref_file}{$headers[0]}[$row_count]){
  $line_count = $row_count+1;
  print "\r -> Line: $line_count / $ref_file_row_count                            ";
  # column-loop: process each header in referece file
  foreach $header (@headers){
    # file loop: compare each file value to reference file.
    for $file ( keys %all_values ){
      if (  !$all_values{$file}{$header}[0] ){
	$missing_headers{$file}{$header} = 1;
      }else{

	# extract value
	$value = $all_values{$file}{$header}[$row_count];

	# calculate average
	$average{$file}{$header} =   $average{$file}{$header}
	  + $value / $ref_file_row_count;

	# update maximim.
	if ( $value > $max{$file}{$header} || $row_count == 0 ) {
	  $max{$file}{$header} = $value;
	  $max_row{$file}{$header} = $row_count + 3;
	}
	# update minimum
	if ( $value < $min{$file}{$header} || $row_count == 0 ) {
	  $min{$file}{$header} = $value;
	  $min_row{$file}{$header} = $row_count + 3;
	}

	# perform error operations - skip reference file
	if (eval($file ne $ref_file)) {

	  # get reference value
	  $ref_value = $all_values{$ref_file}{$header}[$row_count];

	  # calculate absolute error 
	  $abs_error = $ref_value - $value;
	
	  if (abs($abs_error) > $suppress_threshold || !$suppress_threshold) {


	    $average_abs_error{$file}{$header} =   $average_abs_error{$file}{$header}
	      + $abs_error / $ref_file_row_count;
	
	    $average_rel_error{$file}{$header} =   $average_rel_error{$file}{$header}
	      + $rel_error / $ref_file_row_count;
	
	    # update maximim error.
	    if ( abs($abs_error) > abs($max_abs_error{$file}{$header}) ||
		 $row_count == 0 ) {
	      $max_abs_error{$file}{$header} = $abs_error;
	      $max_abs_error_row{$file}{$header} = $row_count + 3;
	    }

	    # update minimum
	    if ( abs($abs_error) < abs($min_abs_error{$file}{$header}) ||
		 $row_count == 0 ) {
	      $min_abs_error{$file}{$header} = $abs_error;
	      $min_abs_error_row{$file}{$header} = $row_count + 3;
	    }


	    # update global maximum
	    if ( abs($abs_error) > $global_abs_max{$file} ||
		 !$global_abs_max{$file} ) {
	      $global_abs_max{$file} = abs($abs_error);
	      $global_abs_line{$file} = $row_count+3;
	      $global_abs_head{$file} = $header;
	    }



	  }
	}
      }
    }
  }
  $row_count++;
}


# Prepare output
$output .= "\n\n --- RESULTS ------------------ \n";
$output .= "Data rows in refernce file:  $ref_file_row_count   \n\n";
$output .= "Maximum normalized error in test files: \n";
for $file ( keys %global_abs_head ){
  $header = $global_abs_head{$file};
  prep_output("summary");
}


if ($verbose){

  $output .= "\n Summary results for each header follow: ";

  foreach $header (@headers) {
    $output .= "\n\n HEADER: $header\n";
    $file = $ref_file;
    prep_output();
    $file = "";
    for $file ( keys %average ) {
      if (eval($file ne $ref_file)) {
	prep_output();
      }
    }
  }
}


if ($output_file){
  open(OUT_FILE,">$output_file") or die("Could not open $output_file");
  print OUT_FILE $output;
  print "\n";
}else{
  print $output;
}


if($debug_me){
  print "--- DEBUGGING OUTPUT:    --- \n$debug \n--- DEBUGGING OUTPUT: END ---";
}


sub prep_output(&$){
  my ($mode) =  @_;

  if (eval($file eq $ref_file)){
    $ref_txt =  "(reference)";
  }else{
    $ref_txt =  "";
  }
  if ($missing_headers{$file}{$header}){
    $missing_txt = " !! Header not found !! ";
  }else{
    $missing_txt = "";
  }

  $output .= " -> file: $file $ref_txt $missing_txt \n";
  if(! $missing_txt ){
    if ($mode !~ /summary/  ){
      $output .= "    Average value:            $average{$file}{$header} $units{$header}\n";
      $output .= "    Maximum value:            $max{$file}{$header} $units{$header} @ line # $max_row{$file}{$header}\n";
      $output .= "    Minimum value             $min{$file}{$header} $units{$header} @ line # $min_row{$file}{$header}\n";
    }
    if (!$ref_txt){

      $output .= "    Average error:            $average_abs_error{$file}{$header} $units{$header}\n";
      $output .= "    Maximum error:            $max_abs_error{$file}{$header} $units{$header} @ line # $max_abs_error_row{$file}{$header}\n";
      $output .= "    Mininum error:            $min_abs_error{$file}{$header} $units{$header} @ line # $min_abs_error_row{$file}{$header}\n";

      $range = $max{$ref_file}{$header}-$min{$ref_file}{$header};
      if( $range > $small ){
	$normal_max_err = $max_abs_error{$file}{$header} / $range * 100;
	$normal_min_err = $min_abs_error{$file}{$header} / $range * 100;
	$normal_avg_err = $min_abs_error{$file}{$header} / $range * 100;
	$output .= "    Normalized average error: $normal_avg_err  (% of maximum change in variable)\n";
	$output .= "    Normalized maximim error: $normal_max_err  (% of maximum change in variable)\n";
	$output .= "    Normalized minimum error: $normal_min_err  (% of maximum change in variable)\n";
      }else{
	$output .= "    Normalized Error: Variable range ($range) is too small.\n"
      }
    }
  }
}



