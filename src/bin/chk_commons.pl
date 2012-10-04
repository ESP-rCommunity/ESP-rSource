#!/usr/bin/perl
#
# ------------------------------------------------------------------------------
# chk_commons.pl
#
# Written by Alex Ferguson - June 30 2004.
# Copyright: CETC / NRCan 2004.
#
# ....
# ABSTRACT:
#
# This perl routine can be used to check for number, name, type and dimension
# conformance between separate instances of fortran common blocks.
#
# ....
# SYNOPSYS:
#
# When run, chk_commons will recursively search for all fortran files (*.f or *.F)
# beginning in the current directory and progressing through the directory tree
# (excluding CVS folders). The routine will:
#
#  - Read all fortran files (and appropriate header files, if necessary)
#  - Identify all f77 style 'common/X/a,b,c...' statements
#  - Identify typecasting statements that type/dimension variables a,b,c...
#    *within* the current function/subroutine/program
#  - Compare all common blocks for agreement. Blocks will be flagged that
#     - reference dissimilar numbers of variables
#     - reference dissimilar variable names
#     - reference dissimilarly typed variables
#     - reference dissimilarly dimensioned arrays or chararacter variables
#
# ....
# USAGE:
#
# Run chk_commons.pl from the top-level directory containing your fortran 
# program.  The following command-line arguements are supported:
#
#    '-a' : switch to output a list of all commonblocks, regardless of
#           whether or not a mismatch was found (default=no)
#
#    '-I/path/to/include/folder' : switch to change include folder path
#          (default = './')
#
#    '-s' : suppress ESRU commons known to have naming mismatches.
#
# Checking of certian common blocks can be suppressed by adding them to the 
# $suppress_these_blocks variable
#
# ....
# OUTPUT:
#
# chk_commons will produce a listing of all common blocks for which mismatches
# in declaration were found (or optoinally, all common blocks if the '-a' switch has
# been specified). Typical output looks like:
#
#--- COMMON: X  --------
# > Instance: "X/a(dim1)[r],b(dim2)[i],c[c72]"
# > Found at:
#     -> /file_1.F : Line 21 (procedure routine_1)
#     -> /file_1.F : Line 99 (procedure routine_2)
#
# > Instance: "X/a(dim1)[r],b(dim3)[r],c[c24],d"
# > Found at:
#     -> /file_3.F : Line 23 (procedure routine_3)
#
# > 2 instances found.
#
# Each variable in the common block is suffixed using a flag (r/i/l/c*X) that indicates
# the variable's type (real/integer/logical in the current context
#
#
# ....
# KNOWN BUGS / LIMITATIONS:
#
# - There is presently no support for 'DIMENSION' statements.
# - There is presently no support for 'DOUBLE' variable types, and explicitly sized
#   real/integer variables (real*8...), although explicitly-sized character typing
#   is supported.
# - chk_commons assumes that all files in the directory-tree comprise the same program.
#
# ------------------------------------------------------------------------------

use File::Find;
use overload;
use Math::Trig;
use Cwd;

# ESRU commons known to have naming mismatches
$suppress_these_blocks =   "c13ps pcval senact coe34 prec9 coe31s hybe piter pers mfcfd ";

#initialize options to defaults 
$debug_me = 0;
$output_all = 0;
$include_dir = "./";
$suppress_known = 0;
foreach $arg (@ARGV){
  if ($arg =~/^-[as]*d/ ){
    # debugging on.
    $debug_me = 1;
  }
  if ($arg =~/^-[ds]*a/ ){
    # output all commons
    $output_all = 1;
  }
  if ($arg =~/^-[da]*s/ ){
    # supress known mismatches
    $suppress_known  = 1;
  }

  if ($arg =~/^-I/ ){
    # get include directory
    $include_dir = $arg;
    $include_dir =~ s/^-(.*)I//g;
    #strip trailing slash, if it exists
    $include_dir =~ s/\/$//g;
  }

}

if ($output_all ){
  # turn suppression off
  $suppress_known = 0;
}

$debug = "";

$dir = getcwd();
if ($include_dir =~ /^\//){}
else{
  $include_dir = "$dir/$include_dir";
}


find( sub{
	# move on to next file if (1) file is a directory, or
	# (2) file is not readable
	return if -d;
	return unless -r;
	return if $File::Find::name =~ m/CVS./;
	if ($File::Find::name =~ m/.+\.[Ff]$/) {
	  extractor($File::Find::name);
	}
      },  $dir );


processor();

sub processor{

  my ($location, $common_statement, $context);


  # get specific info about commons
  while ( ($location,$common_statement) = each %line_by_line ) {


    $debug .= "- $location \n";
    $debug .= "COMMON $common_statement \n";

    # common:
    my($common_variables) = $common_statement;
    my($common_block)  =    $common_statement;

    $common_variables =~ s/^[a-z0-9_]*\/(.+)$/$1/g;
    $common_block =~ s/^([a-z0-9_]*)\/(.+)$/$1/g;


    $context = $line_context{$location};
    $Typecasting = $function_decs{$context};

    # add typecasting info to commonblock
    $common_variables = resolve_common($common_variables,$Typecasting,$Dimensions);
    $common_statement = "$common_block/$common_variables";

    $debug .= "vvv $common_statement \n\n";
    #nuke ';'
    $common_statement =~ s/;/,/g;
    # group like commons in commons hash
    $commons{$common_statement} .= "! $location (procedure $context) !";

  }



  # identify mismatches.
  while ( ($common, $declarations ) = each %commons ) {
    # get the common name 
    $common_name = $common;
    $common_vbls = $common;
    $common_name =~ s/^([^\/]*)\/[\w\W]*/$1/g;
    $common_vbls =~ s/^[^\/]*\/([\w\W]*)/$1/g;
    $debug .= " $common ---> $common_vbls\n";



    if ( $common_definitions{$common_name} =~ /$common_vbls/ ) {
    } else {
      # separate dissililar instances with '!'
      $common_definitions{$common_name} .= "$common_vbls!";
      # $debug .= " test mismatch: $common_name ->  $common_definitions{$common_name} \n";
    }
  }


  # prepare results / header
  $output = "----------------------------------------------------\n";
  if (!$output_all) {
    $output.= "        MISMATCHES in ESP-r common blocks           \n";
  } else {
    $output.= "               ESP-r common blocks           \n";
  }
  $output .= "----------------------------------------------------\n";

  # / statistics
  $output .= "\n";
  $output .= " Parsed: $line_count lines \n";
  $output .= " Files included from $include_dir \n\n";


  while ( ($common_name, $common_vbls) = each %common_definitions ) {
    # get rid of trailing '!'
    $common_vbls =~ s/!$//g;
    # break definition instances
    @instances = split /!/ , $common_vbls;
    # check to see if multiple instances exist
    if ( ($common_vbls =~ /!/ || $output_all ) && 
        !(($suppress_these_blocks =~ /$common_name/ )&&$suppress_known )) {
      #multiple instance exists!
      $output .= "\n--- COMMON: $common_name --------\n";
      $instance_count = 0;
      for $instance (@instances) {
	# build common block string
	$common_block_statement = "$common_name/$instance";
	# get locations for instance
	$locations = $commons{$common_block_statement};
	# replace leading ! in $locations w/ formatting
	$locations =~ s/^!|!!/\n      ->/g;
	# replace itermettent ! in $locations w/ formatting & linefeed
	#$locations =~ s/!!/\n      ->/g;	
	# replace trailing ! in $locations w/ linefeed
	$locations =~ s/!$/\n/g;
	$output .= " > Instance: \"$common_block_statement\"\n";
	$output .= " > Found at: $locations \n";
	$instance_count++;
      }
      $output.= " > $instance_count instances found.\n";
    } else {
      # do nothing
    }
  }

  if ($debug_me) {
    print "$debug \n";
  }

  print $output;

  return;
}


################
# RESOLVE COMMON
################

sub resolve_common(&$){
  my ($common_string, $Typcasting,$Dimensions) =@_;
  my ($return_common)="";
  my(@common_variables) =  split /,/,  $common_string;
  $debug .= " r-- $common_string\n";
  $debug .= " r-- $Typcasting \n";
  foreach $vbl (@common_variables) {
    $vbl_short = $vbl;
    $vbl_short =~ s/^([a-z_0-9]*)(\(.+\))$/$1/g;
    $debug .= " : $vbl_short \n";
    if ( $Typcasting =~ /(^|,)$vbl_short([^a-z0-9_]|$)/ ){
     # variable has been typecast
     $vbl_type = $Typecasting;
     #$vbl_type =~ s/^.*,$vbl_short([^,]*).*$/$1/g;
     $vbl_type =~ s/(^|^.*,)$vbl_short([^,a-z0-9_]{1}?[^,]*).*$/$2/g;


     $debug .= "    $vbl -> $vbl_type \n";

     $vbl .=$vbl_type;

     # check for duplicity in character 
     # variable declarations X*12[c0]
     if ($vbl =~/([^\*]+?)\*[0-9]+?\[(c|c0)\]/){
       $vbl =~ s/\*([0-9]+?)\[(c0|c)\]/[c$1]/g;
     }elsif ($vbl =~/([^\*]+?)\*[0-9]+?\[(c[0-9]*)\]/){
       $vbl =~ s/\*([0-9]+?)\[(c[0-9]*)\]/[$2]/g;
     }

   } else {

     #implicit typing!
     if ($vbl =~ /^[i-n]{1}/){
       $vbl .= "[i]";
     }else{
       $vbl .= "[r]";
     }


   }




    $return_common .= "$vbl,";
  }
  $return_common =~ s/,$//g;



  return $return_common;
}

#########
# EXTRACTOR
#########

sub extractor(&$){

  # gets commons/declarations and puts them into handy hashes

  my ($full_path) =@_;
  my ($statement_open);
  #read file & include files
  my ($line, @lines,$inc_file, $new_inc_file);
  @lines = get_lines($full_path);

  # remove root from path
  $file_name = $full_path;
  $file_name =~ s/$dir//g;

  $gCurr_function = "";

  my($line_number) = 1;

  my($line_buffer) = "";
  my($number_buffer) = "";

  foreach $line (@lines){

    # check to see if line is from include file
    if ($line =~ /^>[^<]+?</ ){
      #print "$line";
      $new_inc_file = $line;
      $new_inc_file =~ s/^>([^<]+?)<.*$/$1/g;
      $line =~ s/^>([^<]+?)<//g;
      $new_inc_file  =~ s/[\s]*//g;
      #print "$inc_file =~ $new_inc_file \n";

      if ($inc_file =~ /$new_inc_file/){
	# file is not new
      }else{
	#print "	$inc_file = $new_inc_file \n";
	$inc_file = $new_inc_file;
	$inc_file =~ s/[\s]*//g;
	$inc_number = 1;
	#increment line number to account for include statement 
	#in original file
	$line_number++;
      }

    }else{
      $inc_number = 1;
      $inc_file = "";
    }
    if ($line =~/^[Cc!]/) {

    } else {

      #convert to lower case
      $line = lc($line);

      # strip trailing comments
      $line =~ s/![\w\W]*//g;
      #replace tabs with spaces
      $line =~ s/\t/      /g;

      # nuke all leading characters EXCEPT line continuation character
      $line =~ s/^[\w\W]{5}//g;
      #check for newline
      if ($line =~/^[\s]{1}.*/) {	
	# this is a new line:
	$new_line = 1;
      } else {
	$new_line = 0;
      }

      # get rid of first character
      $line =~ s/^[\W\w]{1}//g;
      # condense remaining white space
      $line =~ s/[\s]*//g;

      # check for function change, and set name as appropriate
      check_function($line);

      # if we're in the context of a function
      if ($gCurr_function){
	if ( $new_line ){

	  # check to see if statement is currently open
	  if( $statement_open ) {
	    #close statement & save buffer in appropriate location
	    $statement_open = 0;
	    if ( $gType ) {
	      $gCurr_function_decs .= "$line_buffer,";
	    }
	    if ( $gDimension ){
	      $gCurr_function_dims .= "$line_buffer,";
	    }
	    if ( $gCommon ) {
	      if ($inc_file) {
		$location = "$inc_file : Line $number_buffer";
	      }else{
		$location = "$file_name : Line $number_buffer";
	      }
	      $line_by_line{ $location } =  $line_buffer;
	      $line_context{ $location } =  $gCurr_function;
	    }

	  }



	  # is this a common block, declaration, or dimensioning statement?
	  $statement_open =  parse_for_statements($line);

	  if( $statement_open ){
	    # strip real/integer/character/dimension/common keywords 
	    $line =~ s/(real|integer|logical|common\/)//g;
	    $line =~ s/:://g;
	    $line =~ s/dimension\([0-9]*\)//g;
	    $line =~ s/character(\*[0-9]*)?//g;

	    $line = append_data($line);
	    $line_buffer = $line;
	    if ($inc_file){
	      $number_buffer = $inc_number;
	    }else{
	      $number_buffer = $line_number;
	    }
	  }

	}elsif ( $statement_open ) {
	  # line is a continuation of previous line
	  $line = append_data($line);
	  $line_buffer .= $line;
	
	}

      }

    }
    if($inc_file) {
      $inc_number++;
    }else{
      $line_number++;
    }
    # master line count
    $line_count++;
  }
}

sub get_lines(&$){
  #pulls files (including include files) into array


  # path to current file 
  my ($full_path) = @_;

  # remove root from path
  my ($file_name) = $full_path;
  $file_name =~ s/$dir//g;

  my($inc_file,@lines);

  #$debug .= 
    print " -> reading $file_name ( $full_path )\n";

  open(SOURCE_FILE,"$full_path") or die("Could not open $full_path");

#  my ($line_number)=1;
#  my ($common_block_open)=0;
#  my ($function_open) = 0;
#  my ($declaration_open) = 0;
#  my ($line_buffer) = "";
#  my ($function_buffer) = "";
  $debug .= "opening: $full_path\n";
  while ($line = <SOURCE_FILE> ) {
    #nuke linefeeds
    $line =~ s/\n\r\f//g;

    #check for include statements. Assume include files don't reference other
    #include files
    if ($line =~ /^#include[\s]*"/ ){
      $inc_file = $line;
      $inc_file =~ s/^#include[\s]*"([A-Za-z._0-9]*)"/$1/g;
      $inc_file =~ s/\s//g;
      $inc_file = "$include_dir/$inc_file";
      $debug .= " opening $line -  $inc_file \n";
      #push @lines, "\>$inc_file\<C Code included from $inc_file : START";
      open(inc_file,"$inc_file") or die("Could not open $inc_file");
      while ($inc_line = <inc_file>) {
	push @lines, "\>$inc_file\<$inc_line";
      }
      close (inc_file);
      #push @lines, "\>$inc_file\<C Code included from $inc_file : END";
    }else{
      push @lines, $line;
    }
  }

  close(SOURCE_FILE);

#  output - debug
#  foreach $line (@lines){
#    $debug .= "$line \n"
#  }

  return @lines;

}




sub check_function(&$){
  my ($line) = @_;
  #$debug .= "$line\n";
  if ( $line =~ /^(real|integer|logical|character|)?(function|subroutine|program)/ ){
    # open a new function (global variable)
    $gCurr_function = $line;
    $gCurr_function =~ s/^(real|integer|character|logical)?(function|subroutine|program)([a-zA-Z0-9_]*)[\(\s]*.*/$3/g;
    $gCurr_function =~ s/\s//g;
    $gCurr_function =~ eval($name);
    $gCurr_funct_dec = "";
    $gCurr_funct_dim = "";

  }elsif( $line =~ /^(end|return)$/ ||
	  $line =~ /^endsubroutine/  ||
	  $line =~ /^endfuncition/ ){
    #  save dimension / declaration buffers

    if($gCurr_function){
      $function_dims{$gCurr_function} = $gCurr_function_dims;
      $function_decs{$gCurr_function} = $gCurr_function_decs;
      $gCurr_function = "";
      $gCurr_function_dims = "";
      $gCurr_function_decs = "";


    }
  }

 
  return ;

}


sub parse_for_statements(&$){
  my ($line) = @_;
  $gCommon = 0;
  $gDimesion = 0;
  $gType = 0;

  $line_is_interesting = 1;
  if ($line =~ /(real|integer|character|logical)function/ ) {
    # skip function declarations
     $line_is_interesting = 0;
  }elsif ($line =~ /^common\//){
    $gCommon = 1;
  }elsif ($line =~ /^dimension/){
    $gDimension = 1;
  }elsif( $line =~ /^real/ ){
    $gType = 'r';
  }elsif ( $line =~ /^integer/ ){
    $gType = 'i';
  }elsif ( $line =~ /^logical/ ){
    $gType = 'l';
  }elsif ( $line =~ /^character/ ){

    $gType = $line;
    $gType =~ s/^character\*?([0-9]*)?.*$/$1/g;

    if (!$gType) {$gType =0;}
    $gType = "c".eval($gType);

   }else{
    $line_is_interesting = 0;

  }
  if($gType && $line =~ /dimension/ ){
    # do something here

  }
  return $line_is_interesting;
}


sub append_data(&$){
  # attach gType info to end of each variable

  my($line) = @_;

  $debug .= "->$line\n";

  if ( $gType ) {
    $line  =~ s/([a-z0-9_]+?)(\([a-z0-9_,]+?\))?(,|$)/$1$2\[$gType\]$3/g;
  }
  $debug .= ">>$line  \[ $gType ]\n\n";

  # reformat embedded commas
  while ($line =~ /\([^\)]+,[^\)]+\)/){
    $line =~ s/(\([^\)]+),([^\)]+\))/$1;$2/g;
  }


  return $line;


}

