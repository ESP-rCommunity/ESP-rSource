#!/usr/bin/perl
# 
#====================================================================
#====================================================================
#====================================================================
#====================================================================
#====================================================================
#
# esp-r_crontest.pl
# Author: Phylroy Lopez
# Date: Jan 1, 2007
# Copyright: Natural Resources Canada 2007
#
#--------------------------------------------------------------------
# SYNOPSYS:
#--------------------------------------------------------------------
# This script will run the esp-r standard tests and email results to a recipient list.
# To use this script, simply execute anywhere on a perl/bash based system. This requires the NET::smtp package.
# This will download and compile ESP-r on in your /tmp/.test_esp-r folder and run the standard test scripts against what is stored in 
# http://132.156.178.22/espr/espr-test-results/
# Note a full distribution of ESP-r is not required. This script will download, compile and run the tests automatically. 
#
#
# This script is free software; you can redistribute it and/or modify 
# it under the same terms as Perl itself. 
#
#Usage:

# To run the test and submit the test results to an e-mail address simply use the following syntax. 
#  ./esp-r_crontest.pl <branch> <platform> <email adress>

#Example
#  ./esp-r_crontest.pl branch/development_branch linux ibeausol@nrcan.gc.ca  

# Other branches and platforms can be tested against by changing the $branch and $platform variables. 
# Lists of platforms are only available to NRCan at the moment.
#Future work could include doing a test between two branches, or a revisions  and not requiring an archive.
#Requirements
# Perl 4+
# subversion client
# mail server (mailhost) 
# Perl NET:smtp
#====================================================================
#====================================================================
#====================================================================
#====================================================================
#====================================================================
#Branch name 
$branch_name=$ARGV[0];
#Platform archive to be tested against. 
$Platform=$ARGV[1];
#Notification list.
$mail_to=$ARGV[2];

#If for some reason you wish to test against trunk..
if ($branch_name eq "trunk")
{
 $subbranch_name = "trunk";
}
else
{
$branch_name=~m/(\w*)\/(\w*)/;
$subbranch_name =$2;
}
#print $branch_name."\n".$subbranch_name."\n".$Platform."\n".$mail_to."\n";
#Folder to do testing in.
$TestFolder="/tmp/.test_esp-r";
#Mailhost to use....only available within NRCan. 
$smtp_server="Mailhost.nrcan.gc.ca";
#Username that sends e-mail. 
$mail_from= $ENV{USER}.'@esp-r.net';
sub mail_message($$$$$){
if ( eval "require Net::SMTP;" ) {


my ($smtp_server,$To,$From,$Subject,$Message) = @_;
my $smtp;
my $worked;
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
    $smtp->datasend("Forcheck Results\n$Message\n");
    
    # Finish sending the mail
    $smtp->dataend();
    # Close the SMTP connection.
    $smtp->quit();
    #Set return status.
    $worked = 1;            
    # A successful message.	
    print "Mail sent to $To via $smtp_server.\n"
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
print "NET::smtp not available. Cannot mail\n";
}
#Return status. 
return $worked;
 }

sub main()
{
system ("rm -fr $TestFolder");
system ("mkdir $TestFolder");
chdir("$TestFolder");

system("svn co https://svn2.cvsdude.com/espr/esp-r/$branch_name/ > /dev/null");
system("svn co https://132.156.178.22/espr/espr-test-results/$Platform/ > /dev/null");
chdir("$TestFolder/$subbranch_name/src/");


#Compile Esp-r. 
system("./Install -d $TestFolder --xml --silent --debug --force > /dev/null");


#Test Results, and email to list. 
chdir("$TestFolder/$subbranch_name/tester/scripts/");
system("./tester.pl $TestFolder/esp-r/bin/bps  --historical_archive $TestFolder/$Platform/historical_archive.tar.gz --mailto $mail_to  --smtp_server $smtp_server");

#Forcheck
chdir("$TestFolder/$subbranch_name/src/esrubps");
system("forchk -I ../include *.F > forchk.out");

open(INFILE, "forchk.out") or die "can't open forchk.out !";
undef $/;
$_ = <INFILE>;
$list = $_ ;
$list =~m/ number of error messages:\s*(\d*)/;
$errors = $1;
$list =~m/ number of warnings:\s*(\d*)/;
$warnings = $1;
$list =~m/ number of informative messages:\s*(\d*)/;
$information = $1;

$data =  "Errors = $errors : Warnings = $warnings : Information = $information\n";

open(INFILE2, "$TestFolder/$Platform/forchk.out") or die "can't open $TestFolder/$Platform/forchk.out !";
undef $/;
$_ = <INFILE2>;
$list = $_ ;
$list =~m/ number of error messages:\s*(\d*)/;
$old_errors = $1;
$list =~m/ number of warnings:\s*(\d*)/;
$old_warnings = $1;
$list =~m/ number of informative messages:\s*(\d*)/;
$old_information = $1;

if ( $old_errors == $errors )
{
$data = "Errors = $errors : Warnings = $warnings : Information = $information\nOld Errors = $old_errors : Old Warnings = $old_warnings : Old Information = $old_information\n";
#print $data;
mail_message($smtp_server,$mail_to,$mail_from,"ESP-R_Test:Forcheck passed",$data);
}
else
{
$data =  "Errors = $errors : Warnings = $warnings : Information = $information\nOld Errors = $old_errors : Old Warnings = $old_warnings : Old Information = $old_information\n";
#print $data;
mail_message($smtp_server,$mail_to,$mail_from,"ESP-R_Test:Forcheck failed",$data);
}

}

main();
