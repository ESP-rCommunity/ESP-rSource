#!/usr/bin/perl
# 
#====================================================================
#====================================================================
#====================================================================
#====================================================================
#====================================================================
#
# test_queue.pl
# Author:    Alex Ferguson
# Date:      June 19,2008
# Copyright: Natural Resources Canada 2008
#
# Requirements
#   - Perl 4+
#   - subversion client

# This script is free software; you can redistribute it and/or modify 
# it under the same terms as Perl itself. 
#
#--------------------------------------------------------------------
# SYNOPSYS: For description, read definition of synopsys varaible, 
# below.
#--------------------------------------------------------------------# 
#
#
#

# Pragmas
use Cwd;
use warnings;
use strict 'vars';

# Prototypes
sub fatalerror($);
sub execute($);
sub stream_out($);
sub mail_message($$$$$);
# Global vars
my @gTests;

#Mailhost to use....only available within NRCan. 
my $smtp_server="Mailhost.nrcan.gc.ca";

my $mail_from="cookie_monster-noreply\@nrcan.gc.ca";

my $date =   `date`;

# Synopsys/help message
my $synopsys= "
 
 test_queue.pl 
 
 USAGE:
   
    test_queue.pl 

 SYNOPSYS:
 
 This script checks-out and parses the content of the svn module
 https://esp-r.net/espr/requests/test_queue, and invokes the tests
 specified in the test_queue.input file.
 
 
 OPTIONS:
 
   -v, -verbose: Progress reported to screen.

   -vv, -very-verbose: Low-level activity reported to screen

  NOTES: Send comments, questions and bug reports to 
    aferguso\@nrcan.gc.ca.

";

#------------------------------------------------------------
# Process command line arguements
#------------------------------------------------------------
# Compress command line arguements into a single string for 
# search and replace operations 
my $cmd_arguements;
my $verbose = 0;
my $very_verbose = 0;

if ( @ARGV ){

  foreach my $arg (@ARGV){

    # Aliases
    $arg =~ s/^-vv/--very-verbose/g;
    $arg =~ s/^-v/--verbose/g;

    
    SWITCH:
    {
      # Verbosity
      if ( $arg =~ /^--verbose/ ){
        # stream out progess messages
        $verbose = 1;
        last SWITCH;
      }
      if ( $arg =~ /^--very-verbose/ ){
        # steam out all messages
        $verbose = 1;
        $very_verbose = 1;
        last SWITCH;
      }

      fatalerror("Arguement $arg is not supported!\n");

    }

  }

}


#-------------------------------------------------------------------
# Lock test queue file
#-------------------------------------------------------------------
execute("svn lock --username aferguso https://esp-r.net/espr/requests/test_queue/test_queue.input -m \"test_queue.pl: parsing file\"");

#-------------------------------------------------------------------
# Check-out file
#-------------------------------------------------------------------
execute("svn checkout --username aferguso https://esp-r.net/espr/requests/test_queue");

#-------------------------------------------------------------------
# Read file 
#-------------------------------------------------------------------
my $file_changed = 0;
my @output;
open (INPUT, "test_queue/test_queue.input" ) or fatalerror ("Could not open test_queue/test_queue.input for reading\n");

while ( my $line = <INPUT> ){

  # Strip comments and leading spaces from copy
  my $line_copy = $line;
  $line_copy =~ s/#.*$//g;
  $line_copy =~ s/\s+//g;
  # Parse line
  if ( $line_copy ){

    stream_out(" > parsing $line");
  
    my ( $target1, $target2, $address, $tests, $build_opts ) = split /,/, $line;

    my $skip_tests = "--skip-forcheck --skip-builds --skip-regression";

    if ( $tests =~ /FORCHECK/ )  { $skip_tests =~ s/--skip-forcheck//g; }
    if ( $tests =~ /BUILDS/ )    { $skip_tests =~ s/--skip-builds//g;   }
    if ( $tests =~ /REGRESSION/ ){ $skip_tests =~ s/--skip-regression//g;    }
  

    my $cmd = "nice --adjustment 19 ./automated_tests.pl -v ".
         "--branch $target1 ".
         "--branch $target2 ".
         "--addresses $address $skip_tests";


    my $msg =  "  Automated ESP-r testing: $date\n"
              ."  Thank-you for using the automated ESP-r test system.  I've begun\n"
              ."  processing your request, which may take several hours. I'll email\n"
              ."  you the results when I'm finished.\n"
              ."  \n"
              ."  Configuration:\n"
              ."    - Reference version:   $target1\n"
              ."    - Test version:        $target2\n"
              ."    - Requested tests:     $tests\n"
              ."    - Email destination:   $address\n"
              ."  \n"
              ."  Test command:\n"
              ."    $cmd\n"
              ."\n";

    my $cmd = "./automated_tests.pl -v ".
         "--branch $target1 ".
         "--branch $target2 ".
         "--addresses $address,aferguso\@nrcan.gc.ca $skip_tests";


    push ( @gTests,
           {
             email_dest   => $address,
             req_tests    => $tests,
             options      => $build_opts,
             cmd          => $cmd,
             msg          => $msg
           }
         );

    # if line was parsed, delete from output.
    $line = "";
    $file_changed = 1;
  }
  push (@output, $line);
}

close (INPUT);

#-------------------------------------------------------------------
# Write out empty version of file.
#-------------------------------------------------------------------

if ( $file_changed ){

  open (OUTPUT, ">test_queue/test_queue.input" ) or fatalerror ("Could not open test_queue/test_queue.input for writing\n");

  foreach my $line ( @output ){
    print OUTPUT $line;
  }
  close(OUTPUT);
  execute("svn unlock --username aferguso https://esp-r.net/espr/requests/test_queue/test_queue.input");
  execute("svn commit --username aferguso test_queue/test_queue.input -m \"test_queue.pl: updating file\"");
}

#-------------------------------------------------------------------
# SVN Clean-up.
#-------------------------------------------------------------------
execute("svn cleanup test_queue/");
execute("rm -fr test_queue");
if ( ! $file_changed ) {
  execute("svn unlock --username aferguso https://esp-r.net/espr/requests/test_queue/test_queue.input");
}

#-------------------------------------------------------------------
# Notify user that we're going to run tests, and let then see the command.
#-------------------------------------------------------------------

foreach my $testhash ( @gTests ){

  my %test = %$testhash;

  my $subject= "ESP-r on-demand automated test: request acknowledged";
  my $address= $test{"email_dest"};

  mail_message($smtp_server,$address,$mail_from,$subject,$test{msg});
}

#-------------------------------------------------------------------
# Now invoke automated_tests.pl using parsed options 
#-------------------------------------------------------------------

foreach my $testhash ( @gTests ){

  my %test = %$testhash;

  my $result = `$test{cmd}`;

  mail_message($smtp_server,
               "aferguso\@nrcan.gc.ca",
               $mail_from,
               "ESP-r automated test --- progress report",
               $result
               );
  

}

die;

#-------------------------------------------------------------------
# Optionally write text to buffer
#-------------------------------------------------------------------
sub stream_out($){
  my($txt) = @_;
  if ($verbose){
    print $txt;
  }
}

#--------------------------------------------------------------------
# Perform system commands with optional redirection
#--------------------------------------------------------------------
sub execute($){
  my($command) =@_;
  my $result;
  if ($very_verbose){print "\n > executing $command \n";}
  $result = system($command);
  return $result;
}


#-------------------------------------------------------------------
# Display a fatal error and quit.
#-------------------------------------------------------------------

sub fatalerror($){
  my ($err_msg) = @_;

  if ($very_verbose ){
    #print echo_config();
  }
  print "\ntest_que.pl -> Fatal error: \n";
  print " >>> $err_msg \n\n";
  die;
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


