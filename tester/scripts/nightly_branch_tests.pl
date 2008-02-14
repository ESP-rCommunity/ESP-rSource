#!/usr/bin/perl
# 
#====================================================================
#====================================================================
#====================================================================
#====================================================================
#====================================================================
#
# nightly_branch_tests.pl
# Author:    Alex Ferguson
# Date:      June 29, 2007
# Copyright: Natural Resources Canada 2007
#
# Requirements
#   - Perl 4+
#   - subversion client
#   - mail server (mailhost) 
#   - Perl NET\:\:smtp
#
# DEPENDENCIES:
#
#   - Working svn, gcc/g77, forcheck
#   - automated_tests.pl, tester.pl scripts found in the 
#     tester/scripts folder
#     Working smtp server 
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

#--------------------------------------------------------------------
# Prototypes
#--------------------------------------------------------------------
sub echo_config();
sub stream_out($);
sub execute($);
sub fatalerror($);
sub test_branch($);
sub read_data_file($);
sub update_data_file($);
sub LockFile($$);
#--------------------------------------------------------------------
# Declare variables and defaults
#--------------------------------------------------------------------

my %gBranches;                 # List of branches to test
my %gMembers;                  # List of people results should be 
                               #    mailed to.
                              
                              
my $gBaseURL;                  # Repository base url
                              
my $gVerbose = 0;              # Verbose output
my $gVeryVerbose = 0;          # Very verbose output

my $gInputFile="nightly_branch_tests.dat";  # Input file

my $gSMTP_Server="Mailhost.nrcan.gc.ca";    # SMTP server

my $gTestOptions="-v";         # Generic test options.

my @gInput;                    # Buffer to store configuration file

my $debug = 0;

#--------------------------------------------------------------------
# SYNOPSYS 
#--------------------------------------------------------------------

my $synopsys = " 

 nightly_branch_tests.pl

 USAGE: 
 
   nightly_branch_tests.pl [options] [input file]
   
 OPTIONS 
 
   -h, --help: Display this message and quit 
   
   -v: report progress to screen (verbose mode)
   
   -vv: print test messages to screen (very-verbose mode)

 SYNOPSYS:
 
   nightly_branch_tests.pl will parse the branch list provided in the
   input file, and check to see if each branch has been updated since
   the last test was performed, and if so, invoke the static analysis
   build and regression tests contained in automated_tests.pl.
   
   If no input file is provided at run time, the script searches
   for $gInputFile.
  
";

#--------------------------------------------------------------------
# Process command line arguements. 
#--------------------------------------------------------------------

my $cmd_arguements; 

if ( @ARGV ){

  foreach my $arg (@ARGV){
    $cmd_arguements .= " $arg ";
  }
  
  # Compress white space, and convert to ';'
  $cmd_arguements =~ s/\s+/ /g;
  $cmd_arguements =~ s/\s+/;/g;
  
  # Convert short-hand arguements into long hand 
  $cmd_arguements =~ s/-h;/--help;/g;
  $cmd_arguements =~ s/-v;/--verbose;/g;
  $cmd_arguements =~ s/-vv;/--very-verbose;/g;
  
  
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
      
      # Verbosity
      if ( $arg =~ /^--verbose/ ){
        # stream out progess messages
        $gVerbose = 1;
        last SWITCH;
      }
      if ( $arg =~ /^--very-verbose/ ){
        # steam out all messages
        $gVerbose = 1;
        $gVeryVerbose = 1;
        last SWITCH;
      }
      
      # If arguement is not prefixed with '-', assume it's an 
      # input file 
      if ( $arg =~ /^[^-]/ ){
        $gInputFile = $arg;
        last SWITCH; 
      }
            
      if ( $arg =~ /^--debug/ ){
        $debug = 1;
        last SWITCH;
      }
            
      fatalerror ("Unknown arguement $arg"); 
      LockFile ($gInputFile, "unlock");
    }
  }
}

# Attempt to obtain lock on file 

stream_out (`date`);

if ( LockFile ( $gInputFile, "lock" ) ) {

  # Read input file.
  read_data_file($gInputFile);
  
  # Possibly dump configuration to buffer
  echo_config();
  
  # Run tests
  foreach my $branch ( keys %gBranches ){
    
    test_branch($branch);
  
  }
  
  # Update 'last tested revision' numbers in configurtion file with
  # most recent revision 
  update_data_file($gInputFile);
  
  # Unlock file
  LockFile( $gInputFile, "unlock" )

}else{
  
  stream_out ("$gInputFile locked; no tests performed.\n");
  
}

die;

#-------------------------------------------------------------------
# Read input file, and attempt to update 'locked' flag 
#-------------------------------------------------------------------
sub LockFile($$){

  my ($input_file, $action) = @_;
  
  my $FileLockedOk = 0;
  
  # Parse file

  
  open (INPUT, "$input_file" ) or fatalerror ("Could not open $input_file for reading\n");
    
  my ($output) = "";
    
  while ( my $line = <INPUT> ){
    
    my $line_copy = $line;
    
    # Strip comments and leading spaces from copy 
    $line_copy =~ s/#.*$//g;
    $line_copy =~ s/^\s*//g;
    $line_copy =~ s/\s+//g;
      
    # If there's anything left, check if line matches '*FILE-LOCK OPEN'
    if ( $line_copy =~ /[^\s]/ ){
    
      
      if ( $action =~ /lock/ ){
          
            if ( $line_copy =~ /\*FILE-LOCK,UNLOCKED/ ){
        
            # Lock file 
              $line =~ s/UNLOCKED/LOCKED/g;
              
              $FileLockedOk = 1;
              
            }
            
      }
      
      if ( $action =~ /unlock/ ){
          
            if ( $line_copy =~ /\*FILE-LOCK,LOCKED/ ){
        
            # Lock file 
              $line =~ s/LOCKED/UNLOCKED/g;
              
              $FileLockedOk = 1;
              
            }
                        
      }

      
    }
    
    # Append line to output
    $output .= $line;
  
  }
  
  # Close input, and open output 
  
  close (INPUT);


  open (OUTPUT, ">$input_file" ) or fatalerror ("Could not open $input_file for writing\n");
  
  die;
  
  print OUTPUT $output;

  close (OUTPUT);

  return $FileLockedOk; 

}


#-------------------------------------------------------------------
# Test specified branch
#-------------------------------------------------------------------
sub test_branch($){
  
  my ($branch) = @_;
  
  # Get last changed revision from svn info
  
  
  
  my $last_revision = `svn info $gBaseURL/$gBranches{$branch}{"name"} | grep "Last Changed Rev:" `;
  
  $last_revision =~ s/Last Changed Rev://g;
  $last_revision =~ s/\s*//g;
  
  
  if ( ! $last_revision ){
    my $svn_out = `svn info`;
    stream_out ("SVN INFO: $svn_out \n");
    fatalerror ("Unable to collect output from command `svn info`");
    LockFile ($gInputFile, "unlock");
  }
    
  $gBranches{$branch}{"test_rev"} = $last_revision;
  
  
  # rX has been specified in input file, test against revision 
  # prior to last-changed revision. Otherwise, use specified 
  # revsion.
  my $old_rev;
  if ( $gBranches{$branch}{"ref_rev"} =~ /^X$/ ){
    $old_rev = $last_revision - 1;
  }else{ 
    $old_rev = $gBranches{$branch}{"ref_rev"};
  }
  
  
  # If last changed revision is newer than last tested revision, 
  # perform tests.   
  if ( eval ("$old_rev < $gBranches{$branch}{\"test_rev\"}") ){
    
    stream_out ("Testing $branch\@r$old_rev vs. $branch\@r$gBranches{$branch}{\"test_rev\"}\n");
    
    # Determine which tests should be performed, and add '--skip-XXX' if necessary
    my $local_test_options = "";
    
    if ( $gBranches{$branch}{"tests"} !~ "STATIC" )      { $local_test_options .= " --skip-forcheck ";  }
    if ( $gBranches{$branch}{"tests"} !~ "BUILD" )       { $local_test_options .= " --skip-builds ";    }
    if ( $gBranches{$branch}{"tests"} !~ "REGRESSION" )  { $local_test_options .= " --skip-regression ";}
    
    # Build url arguements 
    my $URL_1 = $gBranches{$branch}{"name"}."\@".$old_rev;
    my $URL_2 = $gBranches{$branch}{"name"}."\@".$gBranches{$branch}{"test_rev"};
    
    # Collate email addresses 
    my ( @members ) = split /:/, $gBranches{$branch}{"members"};
    
    my $addresses = " -a " ; 

    if ( ! $debug ){
      foreach my $member ( @members ) {
        $addresses .= $gMembers{$member}.",";
      }
    }else{
      $addresses .= "aferguso\@nrcan.gc.ca";
    }
    
    # Strip trailing , off of address list
    $addresses =~ s/,$//g;

    # Escape arguement string.

    my $extra_args = "";
    my $extra_options = "";
    if ( $gBranches{$branch}{"args"} ) {
      $extra_args = "\\\"$gBranches{$branch}{\"args\"}\\\"";
      $extra_options  ="--test-build-args=$extra_args";
      $extra_options .=" --ref-build-args=$extra_args";
    }
    execute("./automated_tests.pl $gTestOptions $local_test_options $addresses -b $URL_1 -b $URL_2 $extra_options");

    # Set flag to update branch entry in input file 
    $gBranches{$branch}{"updated"} = 1;
  
  }else{
    # No changes on branch. Don't test, or update config file.  
    $gBranches{$branch}{"updated"} = 0;
    
    stream_out ("Skipping $branch\@r$old_rev: No change.\n");
    
  }
  
  return;
}

#--------------------------------------------------------------------
# Update configuration file 
#--------------------------------------------------------------------

sub update_data_file($){
  
  my ($input_file) = @_;
  my $Output; 
  
  my $Branches_open = 0; 
  
  
  # Loop through file, and update branch entries as necessary
  for my $line ( @gInput ){
    
    # Open/close branch blocks as necessary
    if ( $line =~ /^\*BRANCHES\s*$/ )      { $Branches_open = 1; }
    if ( $line =~ /^\*BRANCHES-END\s*$/ )  { $Branches_open = 0; }
    
    # If line corresponds to branch entry, perform substitution 
    if ( $Branches_open &&  $line !~ /^\*/ ){
      
      # Make a copy 
      my $line_copy = $line;
      
      # Strip comments and leading spaces from copy 
      $line_copy =~ s/#.*$//g;
      $line_copy =~ s/^\s*//g;
      $line_copy =~ s/\s+//g;
      
      # If there's anything left, perfrom seach and replace 
      if ( $line_copy =~ /[^\s]/ ){
      
        # Get particulars from copy : 
        my ($alias,$name,$old_rev,$members,$tests) = split /,/, $line_copy;
  
        # if branch has been updated, update line with new revision 
        # number.
        if ( $gBranches{$alias}{"updated"} ){
        
          # Get updated branch number
          my $new_rev = $gBranches{$alias}{"test_rev"};
          
          stream_out("Updating revision data in $alias: $old_rev -> r$new_rev\n");

          # Update revision on line
          $line =~ s/(\s|,)$old_rev(\s|,)/$1r$new_rev$2/g;
        }
      }
    }
    
    # Append line to output buffer
    
    $Output .= $line;
    
  }
  
  # Print out to file 
  open (OUTPUT, ">$input_file");
  print OUTPUT $Output; 
  close (OUTPUT);
  
  return;
  
}


#--------------------------------------------------------------------
# Open input file and parse contents
#--------------------------------------------------------------------
sub read_data_file($){
  
  my ($input_file) = @_;
  
  # Flags for blocks.
  my ($Addresses_open,$Branches_open) = (0,0);
  
  if ( ! -r $input_file ) {
  
    fatalerror ("Could not open input file \"$input_file\" for reading!");
    LockFile ($gInputFile, "unlock");
    
  }else{
  
  # Parse file
  
    open (INPUT, "$input_file" );
    
    while ( my $line = <INPUT> ){
      
      # Save line for use later 
      push @gInput, $line; 

      # if line contains quotes, escape them and embedded spaces.

      while ( $line =~ /"/ ){
        my $quote_sting = $line;

        $quote_sting =~ s/^[^"]*("[^"]*").*$/$1/;

        my $convert_string = $quote_sting;
       
        # convert quoted spaces into '^~'s:
        $convert_string =~ s/ /^~/g;

        # convert quotes into '###':
        $convert_string =~ s/"/###/g;

        # Substitute back into command arguement string
        $line =~ s/$quote_sting/$convert_string/;

        
      }

      # Delete '###' character
      $line =~ s/###//g;
      
      # Strip out comments (beginning with #)
      $line =~ s/#.*$//g;
      $line =~ s/^\s*//g;

      # Eliminate white space
      $line =~ s/\s+//g;
      # If there's anything left, parse contents
      
      if ( $line =~ /[^\s]/ ){
        # Open/close address and branch blocks as necessary
        if ( $line =~ /^\*ADDRESSES\s*$/ )     { $Addresses_open = 1; }
        if ( $line =~ /^\*ADDRESSES-END\s*$/ ) { $Addresses_open = 0; }
        if ( $line =~ /^\*BRANCHES\s*$/ )      { $Branches_open  = 1; }
        if ( $line =~ /^\*BRANCHES-END\s*$/ )  { $Branches_open  = 0; }
        
        # Parse base URL 
        if ( $line =~ /^\*BASE-URL/ ){
          ( my $dummy, $gBaseURL ) = split /,/, $line;
        }
        
        if ( $line =~/^\*SMTP/ ){
          ( my $dummy, $gSMTP_Server ) = split /,/, $line;
        }
        
        # Parse email addresses
        if ( $Addresses_open && $line !~ /^\*/ ) {
          my ($name,$address) = split /,/, $line;
          $gMembers{$name} = $address;
        }
        
        # Parse branches
        if ( $Branches_open && $line !~ /^\*/ ){

          # Initialize branch prop vars as empty strings 
          my ($alias,$name,$rev,$members,$tests,$args);


          # Strip leading 'r' from revision number
          my @contents = split /,/, $line;

          $alias       = ( defined ( $contents[0] ) ) ?  $contents[0] : "";
          $name        = ( defined ( $contents[1] ) ) ?  $contents[1] : "";
          $rev         = ( defined ( $contents[2] ) ) ?  $contents[2] : "";
          $members     = ( defined ( $contents[3] ) ) ?  $contents[3] : "";
          $tests       = ( defined ( $contents[4] ) ) ?  $contents[4] : "";
          $args        = ( defined ( $contents[5] ) ) ?  $contents[5] : "";

          $rev =~ s/r//g;

          # Turn '^~' in arg back into spaces
          $args =~ s/\^~/ /g;                  
          
          # Store branch info
          $gBranches{$alias}{"name"}     = $name;
          $gBranches{$alias}{"ref_rev"}  = $rev;
          $gBranches{$alias}{"members"}  = $members;
          $gBranches{$alias}{"tests"}    = $tests;
          $gBranches{$alias}{"args"}     = $args;
        
        }
      
      }
    
    }
    
    close (INPUT);
  
  }
  return;
}
#-------------------------------------------------------------------
# Optionally dump configuration to screen
#-------------------------------------------------------------------
sub echo_config(){
  stream_out("\nnightly_branch_tests.pl Configuration:\n\n");
  stream_out("  - input file:      $gInputFile \n");
  stream_out("  - SMTP server:     $gSMTP_Server \n");
  stream_out("\n  - email targets:   ");
  while ( my( $name, $address )= each %gMembers ){
    stream_out("$name ($address)\n                     ");
  }
  stream_out("\n  - branches:        ");
  foreach my $branch ( keys %gBranches ){
    stream_out($gBranches{$branch}{"name"}."\@"
                   .$gBranches{$branch}{"ref_rev"}." (tests - "
                   .$gBranches{$branch}{"tests"}.") :: mailto: ->"
                   .$gBranches{$branch}{"members"}." :: extra args ("
                   .$gBranches{$branch}{"args"}
                   .")\n                     ");
  }
  stream_out("\n");
  return;
}

#-------------------------------------------------------------------
# Optionally write text to buffer
#-------------------------------------------------------------------
sub stream_out($){
  my($txt) = @_;
  if ($gVerbose || $gVeryVerbose){
    print $txt;
  }
  return;
}

#--------------------------------------------------------------------
# Perform system commands with optional redirection
#--------------------------------------------------------------------
sub execute($){
  my($command) =@_;
  my $result;
  if ($gVeryVerbose){
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
  print "\ntester.pl -> Fatal error: \n";
  print " >>> $err_msg \n\n";
  die;
}

            
                              