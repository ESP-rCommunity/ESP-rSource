#!/usr/bin/perl
# 
#====================================================================
#====================================================================
#====================================================================
#====================================================================
#====================================================================
#
# automated_tests.pl
# Author:    Alex Ferguson
# Date:      June 15, 2007
# Copyright: Natural Resources Canada 2007
#
# Requirements
#   - Perl 4+
#   - subversion client
#   - mail server (mailhost) 
#   - Perl NET\:\:smtp
#
# This script is free software; you can redistribute it and/or modify 
# it under the same terms as Perl itself. 
#
#--------------------------------------------------------------------
# SYNOPSYS: For description, read definition of synopsys varaible, 
# below.
#--------------------------------------------------------------------

#===================================================================
#====================================================================
#====================================================================
#====================================================================
#====================================================================

use Cwd;
use warnings;
use strict;

#--------------------------
# Prototypes:
sub echo_config();
sub stream_out($);
sub mail_message($$$$$);
sub main();
sub execute($);
sub parse_forcheck($);
sub fatalerror($);
sub buildESPr($$$);
sub passfail($);
#--------------------------

#------------------------------------------------------------
# Default values
#------------------------------------------------------------

# List of esp-r binaries to be tested, and 
# corresponding source folders
my %binlist = ( "aco"   =>   "esruaco",     
                "b2e"   =>   "esrub2e",     
                "bps"   =>   "esrubps",
                "c2e"   =>   "esruc2e",     
                "cfg"   =>   "esrucfg",     
                "clm"   =>   "esruclm",     
                "dbm"   =>   "esrudbm",     
                "dfs"   =>   "esrudfs",     
                "e2r"   =>   "esrue2r",     
                "ecnv"  =>   "esrucnv",     
                "eco"   =>   "esrueco",     
                "grd"   =>   "esrugrd",     
                "ish"   =>   "esruish",     
                "mfs"   =>   "esrumfs",     
                "mld"   =>   "esrumld",     
                "mrt"   =>   "esrumrt",     
                "pdb"   =>   "esrupdb",
                "prj"   =>   "esruprj",     
                "res"   =>   "esrures",          
                "tdf"   =>   "esrutdf",         
                "vew"   =>   "esruvew" );         
                          
my %binalias = ( "vew"    =>   "viewer",
                 "mrt"    =>   "espvwf" );                          
                          
my $Test_base_URL="https://svn2.cvsdude.com/espr/esp-r";
             
             
#Mailhost to use....only available within NRCan. 
my $smtp_server="Mailhost.nrcan.gc.ca";
             
# Default SVN source
my $def_ref_rev="development_branch\@r971";
my $def_test_rev="development_branch\@HEAD";             
             
# Target directories for reference and test repositories             
             
# Output files
my $output_file = "automated_tester_output.txt";
             
my $echo=0;             
             
# Default verbosity 
my $verbose = 0;
my $veryverbose = 0;             
             
# Flags for components of testing.
my $test_forcheck   = 1;
my $test_builds     = 1;
my $test_regression = 1;

#------------------------------------------------------------
# Synopsys
#------------------------------------------------------------ 
my $synopsys= "
 
 automated_tests.pl 
 
 USAGE:
   
    automated_tests.pl [options] 

 SYNOPSYS:
 
 automated_tests.pl automates numerous ESP-r quality assurance tests.
 These include:
 
    - Static analysis of the ESP-r fortan source base using the
      Forcheck analyzer.
    - Compilation testing of all ESP-r binaries using all three 
      graphics libraries (X11, GTK, and X-less).
    - Regression testing using the ESP-r regression test suite.
 
 When run without any options, the script will check a reference
 copy of ESP-r and test it against a reference copy, and write the
 results into the file $output_file. The default 
 reference and test copies are:
   - reference: $def_ref_rev
   - test:      $def_test_rev
 
 OPTIONS:
 
   -b, --branch abc[\@rX]: 
       Use the provided branch/version instead
       of the defaults. automated_tests.pl expects the branch to be
       specified in 'abc[\@rX]' format, where 'abc' describes the branch
       name, and the optional arguement 'X' describes the requested
       revision number.

       If a single branch arguement is provided, its value will be used
       instead of the default test branch. If two branch arguements are
       specified, the first will be used in leu of the default reference
       branch, and the second will be used in leu of the default test
       version.

       automated_tests.pl permits some leeway when specifying the
       branch; 'trunk', 'trunk\@r1000', 'development_branch\@HEAD', and
       'branches/development_branch' are all valid.


   -a, --addresses abc\@xyz.net,def\@xyx.net,...:
       Mail the results to specified comma-separated recepient list. This
       option requires access a valid SMTP server.


   --smtp mailhost.xyz.net:
       Use the spedified mailhost when sending mail, instead of the 
       default ($smtp_server).
       
   -v: 
       Verbose output; print test progress to the buffer.
       
   -vv:
       Very verbose output; print command messages to the buffer.


  EXAMPLES:
  
    Test default versions ($def_ref_rev, 
    $def_test_rev) and follow progress on screen:
    
        \$ \./automated_tests.pl -v
      
    Test default versions and mail results to abc\@xyz.net:
    
        \$ \./automated_tests.pl -a abc\@xyz.net
        
    Test revision 1123 on development branch against default
    reference version:
        
        \$ \./automated_tests.pl -b development_branch\@r1123
        
    Test most recent version of branch 'abc' against default 
    reference version:

        \$ \./automated_tests.pl -b abc
        
    Test revison '1220' on branch 'abc' against the same 
    revision on branch 'ijk'    
        
        \$ \./automated_tests.pl -b abc\@1220 -b ijk\@1220
        
  
  NOTES: Send comments, questions and bug reports to 
    aferguso\@nrcan.gc.ca.
        
 ";
             
             
            
#------------------------------------------------------------
# Process command line arguements
#------------------------------------------------------------
# Compress command line arguements into a single string for 
# search and replace operations 
my $cmd_arguements; 

my @version_stack;
my %revisions;
my @addresses;


if ( @ARGV ){

  foreach my $arg (@ARGV){
    $cmd_arguements .= " $arg ";
  }
  
  # Compress white space, and convert to ';'
  $cmd_arguements =~ s/\s+/ /g;
  $cmd_arguements =~ s/\s+/;/g;
  
  # Convert short hand arguements into longhand
  $cmd_arguements =~ s/-a;/--addresses;/g;
  $cmd_arguements =~ s/-b;/--branch;/g;
  $cmd_arguements =~ s/-h;/--help;/g;
  $cmd_arguements =~ s/-v;/--verbose;/g;
  $cmd_arguements =~ s/-vv;/--very-verbose;/g;
  
  # Collate options expecting arguements ---
  #   transform ';' into ':'
  $cmd_arguements =~ s/--smtp;/--smtp:/g;
  $cmd_arguements =~ s/--branch;/--branch:/g;
  $cmd_arguements =~ s/--addresses;/--addresses:/g;
  
  # If any options expecting arguements are followed by other
  # options, insert empty arguement:
  $cmd_arguements =~ s/:-/:;-/;
      
  # remove leading and trailing ;'s
  $cmd_arguements =~ s/^;//g;
  $cmd_arguements =~ s/;$//g;
  
  # split processed arguements back into array
  my @processed_args = split /;/, $cmd_arguements;
  
 
  
  # Intrepret arguements
  foreach my $arg (@processed_args){     
    SWITCH:
    {
      # Help 
      if ( $arg =~ /^--help/ ){
        print $synopsys;
        die();
        last SWITCH;
      }
      
      # Revision for testing 
      if ( $arg =~ /^--branch:/){
        my $revision=$arg;
        $revision =~ s/--branch://g;
        # --branch option must include an arguement
        if ( ! $revision ) { fatalerror("A branch must be provided with the -branch (-b) option!\n"); }
        
        # branch can be specified with optional @revision. If not, 
        #  append @HEAD.
        if ( $revision !~ /.+\@.+/ ){
          $revision .= "\@HEAD";
        }
        # Push revision on to stack
        push @version_stack, $revision;
        last SWITCH;
      }
      
      # Skip portions of test:
      if ($arg =~ /^--skip/ ){
        if ( $arg =~/^--skip-forcheck/ )  { $test_forcheck   = 0; }
        if ( $arg =~/^--skip-builds/ )    { $test_builds     = 0; }
        if ( $arg =~/^--skip-regression/ ){ $test_regression = 0; }
   
        last SWITCH;
      
      }
      
      
      # Destination mail
      if ($arg =~ /^--addresses/ ){
        $arg =~ s/--addresses://g;
        # Address must include arguement.
        if ( ! $arg ) { fatalerror("An email address must be provided with the -addresses (-a) option!\n"); }
        push @addresses, split /,/, $arg;
        last SWITCH;
      
      }
      
      # SMTP server 
      if ($arg =~ /^--smtp/ ){
        $arg =~ s/--smtp://g;
        # Address must include arguement.
        if ( ! $arg ) { fatalerror("An email address must be provided with the -addresses (-a) option!\n"); }
        $smtp_server = $arg; 
        last SWITCH;
      }
      
      # Debugging:
      if ( $arg =~ /^--echo/ ){
        $echo = 1; 
        last SWITCH;
      }
      
      # Verbosity
      if ( $arg =~ /^--verbose/ ){
        # stream out progess messages
        $verbose = 1;
        last SWITCH;
      }
      if ( $arg =~ /^--very_verbose/ ){
        # steam out all messages
        $verbose = 1;
        $veryverbose = 1;
        last SWITCH;
      } 
    }
  }
}
     
# Append default revisions to %revisions hash, if necessary
for my $count ( scalar(@version_stack) ){
  SWITCH:{
    if ( $count == 0 ){
      # Push default reference and test version_stack onto stack
      %revisions= (  "reference" => $def_ref_rev, 
                     "test"      => $def_test_rev );
                   
      last SWITCH;
    }
    if ( $count == 1 ){
      # Place default reference revision at front of stack
      %revisions= (  "reference" => $def_ref_rev, 
                     "test"      => $version_stack[0] );
      
      last SWITCH;
    }
    if ( $count == 2 ){
      # Use provided revisions
      %revisions= (  "reference" => $version_stack[0], 
                     "test"      => $version_stack[1] );
      
      last SWITCH;
    }    
    if ( $count > 2 ){
      # Too many version_stack:
      fatalerror("A maximum of two branches may be specified, but $count branches were provided!\n");
      last SWITCH;
    }
    
  }

}

# Coerse branch names into /branches/name format.
while ( my  ( $key, $branch ) = each %revisions ){
  if ( $branch !~ /^branches/i && $branch !~ /^trunk/i ){
      $revisions{$key} = "branches/$branch";
  }
}
      


#------------------------------------------------------------
# Finished command argument processing and error trapping.
# Initialize variables.
#------------------------------------------------------------             

# Save current path 
my $path = getcwd();              

# Initialize results buffer, and set test status to 'pass'. 
my $results; 
my $res_header = "Automated test of ESP-r system\n";
$res_header .= "Testing commenced on ".`date`."\n";

my $global_fail = 0;

#Find a suitable folder to do testing in:

my $TestFolder_root="/tmp/.test_esp-r";
my $TestFolder=$TestFolder_root;
my $count = 0;
while ( -d $TestFolder ) {
  $count++;
  $TestFolder="$TestFolder_root\_$count";
}

#Username that sends e-mail. 
my $mail_from= $ENV{USER}.'@esp-r.net';

# Initialize results buffer


# Dump configuration to screen, if running in verbose mode.
if ( $verbose ) { echo_config(); }
if ( $echo )  { die echo_config(); }

#------------------------------------------------------------
# Prepare test directory and checkout code.
#------------------------------------------------------------

# Create test directory, and move to path.
stream_out("\nPreparing test directory $TestFolder...");
execute ("rm -fr $TestFolder");
execute ("mkdir $TestFolder");
chdir("$TestFolder");
stream_out("Done\n");

# Loop through revisions, and check out working copies

my %src_dirs;

while ( my ($key, $revision ) = each  %revisions ) {

  # Label source directories "src_1, src_2", and save in hash
  $src_dirs{$key} ="src_$key";
  
  # Progress update 
  stream_out("Checking out $revision from ESP-r central...");
  
  # Split revision from format branch@rev into 'branch' and 'rev'.
  my ( $branch, $rev ) = split /\@/, $revision; 
  
  # Force rev into -rXXX format, unless 'rev' = 'HEAD'.
  for ($rev) {
    SWITCH: {
      if ( /^r/ )       { $rev ="-$rev";  last SWITCH; }
      if ( /HEAD/ )     { $rev ="";       last SWITCH; }
      if ( /[0-9]+/ )   { $rev ="-r$rev"; last SWITCH; }
      fatalerror{"Invalid revision $revision!\n"};
    }
  }
  
  # Pull down source.
  if ( ! -d $src_dirs{$key} ){
    execute("svn co $Test_base_URL/$branch $rev $src_dirs{$key}");
  }
  stream_out("Done\n");
}

#------------------------------------------------------------
# Now prepare codes for forcheck static analysis. Unfortunately,
# we must compile both versions to ensure that all the appropriate
# source files are linked into the target directories. Otherwise,
# forcheck will not analyze the entire code base. 
#------------------------------------------------------------
# Forcheck pass-fail flag
my $forcheck_fail = 0; 

if ( $test_forcheck ){

  my %forcheck_delta;

  while ( my ($key, $revision ) = each  %revisions ) {
    
    # Move to src directory. 
    chdir("$TestFolder/$src_dirs{$key}/src/");
    
    # Progress update 
    stream_out("Building $key ($revision) version of ESP-r for use with Forcheck.");
    # Build X11 debugging version. 
    buildESPr("default","debug","onebyone");
    stream_out(" Done\n");
    
  }
  
  #-----------------------------------------------------------
  # Update FCKCNF path to reflect location of esp-r.cnf file.
  # (but save the original version so we can reset the path 
  #  later)
  #-----------------------------------------------------------
  my $orig_FCKCNF = $ENV{FCKCNF};
  stream_out("Old FCKCNF path: $ENV{FCKCNF}\n");
  $ENV{FCKCNF} = "$path/esp-r.cnf";
  stream_out("New FCKCNF path: $ENV{FCKCNF}\n");
  
  
  #-----------------------------------------------------------
  # Now exercise forcheck on each binary, for both the 
  # test and reference versions.
  #-----------------------------------------------------------
  
 
  my %forcheck_output = ();
  my $forcheck_summary = "";
  
  # Loop through revisions...
  while ( my ($key, $revision ) = each  %revisions ) {
  
    # progress update
    stream_out("Running Forcheck static analyzer on $key version.");
  
    # Loop through list of esp-r binaries
    while ( my ( $bin, $folder ) = each %binlist ){
    
      # Path to relevant esp-r source for given binary.
      my $folder = $binlist{$bin};
      
      # Pseudo-progress meter.
      stream_out ".";
      
      # Move to appropriate folder.
      chdir("$TestFolder/$src_dirs{$key}/src/$folder");
        
      # Run forcheck, and parse output. Use system command, because
      # execute function may redirect output to /dev/null, depending on 
      # verbosity.
      system ("forchk -I ../include *.F ../lib/esru_blk.F ../lib/esru_libX11.F ../esru_ask.F  > forcheck_$bin.out 2>&1 " ); 
      
      # Save location of forcheck output
      $forcheck_output{$bin}{$key} = "$TestFolder/$src_dirs{$key}/src/$folder/forcheck_$bin.out";
        
      
    }
    stream_out(" Done\n");
  }
  
  
  # If we want to test in the archive in the future, we'll need
  # to add some code here to populate $forcheck_output{$bin}{"reference"}
  # hash with the paths to the archive forcheck_$bin.out files.
  
  # Place static analysis header in results output 
  $results .= "========= RESULTS FROM STATIC ANALYSIS =========\n";
  
  my $forcheck_details ="";
  
  # Now, loop through binaries, and compare 
  
  stream_out("Comparing Forcheck output.");
  
  foreach my $bin ( sort keys %binlist ){
    
    
    # read forcheck output
    my %old_msgs = parse_forcheck ( `cat $forcheck_output{$bin}{"reference"}`);
    my %new_msgs = parse_forcheck ( `cat $forcheck_output{$bin}{"test"}`);
  
    # Create new hash for compiling old and new messages.
    my %all_msgs = ();
    
    # Push all codes on to stack for processing.
    my @all_codes = ();
    push @all_codes, keys %old_msgs, keys %new_msgs; 
    
    # loop through all codes, and append counts to all_messages
    foreach my $code ( @all_codes ){
      if ( ! defined ( $all_msgs{$code} ) ){
        if ( ! defined ( $old_msgs{$code} ) ){ $old_msgs{$code} = "0"; }
        if ( ! defined ( $new_msgs{$code} ) ){ $new_msgs{$code} = "0"; }
        $all_msgs{$code} = "$old_msgs{$code}:$new_msgs{$code}";
      }
    }
    
    # Arrays for storing error, warning, informational messages.
    my @error_codes   = ();
    my @warning_codes = ();
    my @info_codes    = (); 
    my @total_counts  = ();  # <- already declared!
  
    # initialize flag for pass/fail
    my $bin_fail = 0; 
    my $status;
  
    # Build lists of errors/warnings/info msgs for which the number of 
    # instances do not match between old and new versions
    while ( my ( $code, $count ) = each %all_msgs ){
    
      # Split old and new count.
      my ($old_count, $new_count)= split /:/, $count;
      if ( $old_count != $new_count ) {
        
        # Stuff code into appropriate array. Differences in errors 
        # and warnings produce failures.
        for ( $code ) {
          SWITCH: {
            if ( /^Err\./  )  { 
                                push @error_codes, $code;  
                                $bin_fail   = 1;    
                                last SWITCH; 
                              } 
            if ( /^Warn\./ )  {
                                push @warning_codes, $code;
                                $bin_fail   = 1;    
                                last SWITCH; 
                              } 
            if ( /^Info\./ )  { 
                                push @info_codes, $code;       
                                last SWITCH; 
                              } 
          }
        }
      }
      
      # Get summary information: Are there more warnings, errors & 
      # info messages, or less?
      for ($code){
        my $class;
        SWITCH: {
          if ( /^Total error messages/   )       { $class = "error";   last SWITCH; }
          if ( /^Total warning messages/ )       { $class = "warning"; last SWITCH; }
          if ( /^Total informational messages/ ) { $class = "info";    last SWITCH; }
          $class = "none";
        }  
          
        if ( $class !~ /none/ ){
          if ( $old_count > $new_count ) { $forcheck_delta{$bin}{$class} = "(-)"; }
          if ( $old_count < $new_count )  { $forcheck_delta{$bin}{$class} = "(+)"; }
          if ( $old_count == $new_count )  { $forcheck_delta{$bin}{$class} = "( )"; }
        }
      }
      
      
      
    }
      
    # Check if forcheck failed on this binary, and update
    # flags as appropraiate.
    if ( $bin_fail ){
      
      $forcheck_fail = 1;
      $status="Failed";
    
    }else{
      
      $status = "Passed";
    
    }
    
    # Prepare output 
    $forcheck_details .= "\n - binary $bin: $status\n";
    
    # Summarize results
    $forcheck_details .= "   Total errors:     reference $old_msgs{\"Total error messages\"}, test $new_msgs{\"Total error messages\"} \n";
    $forcheck_details .= "   Total warnings:   reference $old_msgs{\"Total warning messages\"}, test $new_msgs{\"Total warning messages\"} \n";
    $forcheck_details .= "   Total info msgs:  reference $old_msgs{\"Total informational messages\"}, test $new_msgs{\"Total informational messages\"} \n";
    
    # Now append unmatched errors to result string 
    if ( $bin_fail ){
      $forcheck_details .= "   Summary of differences:\n";
    }
    foreach my $code ( sort @error_codes, sort @warning_codes, sort @info_codes ){
      $forcheck_details .= "     -> $code [ Instances: reference $old_msgs{$code}, test $new_msgs{$code} ]\n";
    }

  
    # pseudo-progress meter:
    stream_out(".");
  }
  
  # Prepare output: Summary table
  $results .= "\n";
  $results .= "Summary of static analysis: Changes in number of\n";
  $results .= "reported errors, warnings and informational messages.\n";
  $results .= "-------------------------------------------------\n";
  $results .= "Binary                # Err.    # Warn.    #Info.\n";
  $results .= "-------------------------------------------------\n";
  foreach my $bin (sort keys %binlist){
    my $pad = "";
    if ( $bin !~ /ecnv/){ $pad = " ";}
    $results .= "$bin$pad                    ".$forcheck_delta{$bin}{"error"}."       ".
                                          $forcheck_delta{$bin}{"warning"}."       ".
                                          $forcheck_delta{$bin}{"info"}."\n";
  }                                      
  $results .= "-------------------------------------------------\n\n";
    
  $results .= $forcheck_details;
  
  stream_out(" Done\n");
  
      
  # Reset FCKCNF
  $ENV{FCKCNF} = $orig_FCKCNF;
  stream_out("Reset FCKCNF path: $ENV{FCKCNF}\n");
}

#-----------------------------------------------------------
# Finished static analysis. Now test compile of test version
# with X11, gtk and noX versions 
#-----------------------------------------------------------
my %build_result; 
my $X11_fail = 0;
my $GTK_fail = 0;
my $noX_fail = 0;
my $build_fail = 0;
if ($test_builds){

  $results .= "\n\n========= RESULTS FROM COMPILATION TESTS =======\n\n";
  

  
  # X11 version
  stream_out("\nBuilding X11 version of ESP-r.");
  chdir("$TestFolder/$src_dirs{\"test\"}/src");
  execute("make clean");  
  %build_result = buildESPr("X11","clean","onebyone");
  if (  $build_result{"fail"}  ) { $X11_fail = 1; }
  $results .= $build_result{"msg"};
  stream_out("Done\n");
  
  # GTK version
  stream_out("\nBuilding GTK version of ESP-r.");
  chdir("$TestFolder/$src_dirs{\"test\"}/src");
  execute("make clean");
  %build_result = buildESPr("GTK","clean","onebyone");
  if (  $build_result{"fail"}  ) { $GTK_fail = 1; }
  $results .= $build_result{"msg"};
  stream_out("Done\n");
  
  # noX version
  stream_out("\nBuilding noX version of ESP-r.");
  chdir("$TestFolder/$src_dirs{\"test\"}/src");
  execute("make clean");
  %build_result = buildESPr("noX","clean","onebyone");  
  if (  $build_result{"fail"}  ) { $noX_fail = 1; }
  $results .= $build_result{"msg"};
  stream_out("Done\n");
  
  if ( $X11_fail || $GTK_fail || $noX_fail ){$build_fail = 1;}
}

#-----------------------------------------------------------
# Finished static analysis. Now test compile of test version
# with X11, gtk and noX versions 
#-----------------------------------------------------------
my $regression_fail = 0;
if ( $test_regression ) {
  #---------------------------------------
  # Run ESP-r over test suite, using 
  # the default version 
  #---------------------------------------
    
  # Build a clean copy of esp-r to ensure databases are available.
  while ( my ($key, $revision ) = each  %revisions ) {
    stream_out("\nBuilding $key ($revision) version of ESP-r for use with tester.pl...");
    chdir ("$TestFolder/$src_dirs{$key}/src/");
    execute("make clean");
    buildESPr("default","clean","together");  
    execute("mv $TestFolder/esp-r $TestFolder/esp-r_$key");
    stream_out(" Done\n");
  }
  
  # Now run test suite, using test version of tester.pl
  stream_out("Running regression test...");
  my $ref_esp = "$TestFolder/esp-r_reference/bin/";
  my $test_esp = "$TestFolder/esp-r_test/bin/";
  
  chdir ("$TestFolder/$src_dirs{\"test\"}/tester/scripts");
  execute ("./tester.pl $ref_esp/bps $test_esp/bps  -d $TestFolder/esp-r --ref_res $ref_esp --test_res $test_esp -p $TestFolder/$src_dirs{\"test\"}/tester/");
  
  # Digest results
  $results .= "\n\n========= RESULTS FROM REGRESSION TEST =========\n\n";
  # Digest test report 
  my $regression_results = `cat bps_test_report.txt`;
  if ( $regression_results =~ /^ Overall result: Fail\./ ) {
    $regression_fail = 1;
  }
  $results .= `cat bps_test_report.txt`;
  stream_out("Done\n");
}

#---------------------------------------
# Format output 
#---------------------------------------

my $output; 
$output = "$res_header";

$output .= "Test summary:\n";
$output .= "  - Reference version: $revisions{\"reference\"}\n";
$output .= "  - Test version:      $revisions{\"test\"}\n";
$output .= "------------------------------------------------\n";
$output .= "TEST                                    RESULT\n";
$output .= "------------------------------------------------\n";
$output .= "Static analyis (Forcheck)                 ".passfail($forcheck_fail)."\n";
$output .= "Compilation: X11 build                    ".passfail($X11_fail)."\n";
$output .= "             GTK build                    ".passfail($GTK_fail)."\n";
$output .= "             X-less build                 ".passfail($noX_fail)."\n";
$output .= "Regression test                           ".passfail($regression_fail)."\n";
$output .= "------------------------------------------------\n";

$output .= "\n$results\n";


  
#================================================
# Mail out results. 
#================================================

if ( scalar(@addresses) > 0 ){

  stream_out("Mailing results...");
  
  my ($subject);
  
  if ( $forcheck_fail || $build_fail || $regression_fail  ){
    $subject = "ESP-r automated test: Fail"
  }else{
    $subject = "ESP-r automated test: Pass"
  }
  
  foreach my $address (@addresses){
    mail_message($smtp_server,$address,$mail_from,$subject,$output);
  }
  stream_out("Done\n");
}

#================================================
# Write report to disk
#================================================
chdir ($path);
open ( OUTPUT_FILE, ">$output_file") or 
   die( "Could not open $output_file for writing!\n");
print OUTPUT_FILE $output;
close(OUTPUT_FILE);

#================================================
# Clean up
#================================================
execute("rm -fr $TestFolder");

#-------------------------------------------------------------------
# Pass/fail: turn test result into readable string
#-------------------------------------------------------------------
sub passfail($){
  my ($result) =@_;
  my $string;
  if ($result){ 
    $string = "X"; 
  }else{
    $string = "-"; 
  }
  return $string;
}
#-------------------------------------------------------------------
# Compile ESP-r with specified Xlibs, debug state. 
#   - $build=onebyone causes each esp-r binary to be built 
#     individually, and the resulting target tested for completeness.
#-------------------------------------------------------------------
sub buildESPr($$$){
  
  my($xLibs,$state,$build) =@_;
  
  
  my $Debug_flag;
  
  # Set debug flag
  if ( $state =~ /clean/ ) { 
    $Debug_flag =""; 
  }else{
    $Debug_flag ="--debug";
  }
  
  if ( $xLibs =~ /default/ ){
    $xLibs = "";
  }else{
    $xLibs = "--$xLibs";
  }
  
  # Empty target folder 
  execute("rm -fr $TestFolder/esp-r");
  
  # Status buffers 
     
  my %result = ( "fail" => 0, "msg" => "" );
  my $err_msg = "";
  my $fail="0";
  
  if ( $build =~ /onebyone/ ){
    # Test each binary separately, omit training and databases.
    foreach my $bin (sort keys %binlist){
      
      my $target;
      # Some esp-r binaries are renamed from the make target to some 
      # other name. if binalias{bin} exists, search for that binary instead.
      if ( defined ( $binalias{$bin} ) ){
        
        $target = $binalias{$bin};
      
      }else{
        
        $target = $bin;
      
      }
      # pseudo-progress meter...
      stream_out(".");
      
      # Build executable. It would be to recover the output from this 
      # command and report it to the user, if a failure occurs.
      my $debug_temp = `./Install -d $TestFolder --xml --silent $xLibs $Debug_flag --no_dbs --no_training --force $bin 2>&1`;
      
      # Test if target was created, and if not, note failure.
      if ( ! -r "$TestFolder/esp-r/bin/$target"      && 
           ! -r "$TestFolder/esp-r/bin/$target.exe"     ){
        
        $result{"fail"} = 1; 
        
        $err_msg .= "    -> Binary $target could not be built.\n";

      }
    }
  }else{
  
    # Build all at once, omit training files.
    execute("./Install -d $TestFolder --xml --silent $xLibs $Debug_flag --no_training --force");
  }
  
  
  # Return result
  my $status;
  
  if ( $build_result{"fail"} ){ 
    
    $status = "Failed"; 
  
  }else{
    
    $status = "Passed"; 
  }
  
  $result{"msg"} = "\n - Compilation of $xLibs version of ESP-r: $status\n$err_msg";
  
  return %result;
}


#----------------------------------------------
# Parse forcheck output 
#----------------------------------------------
sub parse_forcheck($){
  # Get section after --messages presented header...
  
  my ($output)=@_;
  
  my %shorthand = ( "I" => "Info.",
                    "W" => "Warn.",
                    "E" => "Err. " );
  
  # Strip everything before the '-- messages presented:' string
    
  $output =~ s/.*-- messages presented://sg;
  
 
  
  # Eliminate empty lines
  $output =~ s/^\s+//gm;
  my @lines = split /\n/, $output;
  
  my (%messages);
  
  foreach my $line (@lines){
    

    if ($line =~ /number of error messages:/ ){
      my $error_messages = $line;
      $error_messages =~ s/number of error messages:\s+//g;
      $messages{"Total error messages"}=$error_messages;
    }elsif ($line =~ /number of warnings:/ ){
      my $warning_messages = $line;
      $warning_messages =~ s/number of warnings:\s+//g;
      $messages{"Total warning messages"}=$warning_messages;
    }elsif ($line =~ /number of informative messages:/ ){
      my $info_messages = $line;
      $info_messages =~ s/number of informative messages:\s+//g;
      $messages{"Total informational messages"}=$info_messages;
    }else{
      my ($count,$code,$desc) = split /\]|\[/, $line;
      $code =~ s/^ *//g;
      my ($number,$type)= split / +/, $code;
      $count =~ s/x//g;
      $count =~ s/\s//g;
      $messages{"$shorthand{$type} #$number - $desc"}=$count;
    }
    
  }
  return %messages;
}

#-------------------------------------------------------------------
# Optionally write text to buffer
#-------------------------------------------------------------------
sub stream_out($){
  my($txt) = @_;
  if ($verbose || $veryverbose){
    print $txt;
  }
}

#--------------------------------------------------------------------
# Perform system commands with optional redirection
#--------------------------------------------------------------------
sub execute($){
  my($command) =@_;
  my $result;
  if ($veryverbose){
    stream_out ("\n  -> Executing: $command\n");
    $result = system("$command");
  }else{
    # we might one-day dump to a log file here 
    $result = system("$command >/dev/null 2>&1");
  }
  return $result;
}

#----------------------------------------------
# Report fatal error and kill test
#----------------------------------------------
sub fatalerror($){
  my ($err_msg) = @_;

  if ( $verbose || $veryverbose ){
    print echo_config();
  }
  print "\ntester.pl -> Fatal error: \n";
  print " >>> $err_msg \n\n";
  die;
}
#----------------------------------------------
# Echo configuration
#----------------------------------------------
sub echo_config(){
  print("Configuration:\n");
  print("  - Working directory: $TestFolder\n");
  print("  - svn_source:        $revisions{\"reference\"}, $revisions{\"test\"}\n");
  print("  - email destination: @addresses\n");
}


#----------------------------------------------
# Mail results
#----------------------------------------------
sub mail_message($$$$$){
  my $worked;
  
  if ( eval "require Net::SMTP;" ) {
  
    my ($smtp_server,$To,$From,$Subject,$Message) = @_;
    my $smtp;
  
    if ( $smtp = Net::SMTP->new($smtp_server) )
    {
        # use the sender's address here
        $smtp->mail($From);
        # recipient's address	
        $smtp->to($To);        # recipient's address
        # Start the mail
        $smtp->data();
            
        # Send the header.
        $smtp->datasend("To: $To\n");
        $smtp->datasend("From: $From\n");
        $smtp->datasend("Subject: $Subject\n\n");
        
        # Send the body.
        $smtp->datasend("$Message\n");
        
        # Finish sending the mail
        $smtp->dataend();
        # Close the SMTP connection.
        $smtp->quit();
        #Set return status.
        $worked = 1;            
        # A successful message.	
    }
    else
    {
        #E-mail failed. Tell user.
        print "Socket Connection to SMTP server $smtp_server failed!\n";
        print "Mail send failed!\n";
        print "Verify the following.\n";
        print "SMTP server = $smtp_server \n";
        print "Email list = $To \n";
        #Set return status.
        $worked = 0 ;
    }
  }
  else
  {
    fatalerror "NET::smtp not available. Cannot mail\n";
  }
  #Return status. 
  return $worked;
}

