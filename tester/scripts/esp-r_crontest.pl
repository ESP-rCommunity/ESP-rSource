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

if ( ! @ARGV ){
 $synopsys= "
 This script will run the esp-r standard tests and email results to a
 recipient list.  To use this script, simply execute anywhere on
 a perl/bash based system. This requires the NET::smtp package.
 This will download and compile ESP-r on in your /tmp/.test_esp-r
 folder and run the standard test scripts against what is stored in
 http://132.156.178.22/espr/espr-test-results/. Note a full distribution
 of ESP-r is not required. This script will download, compile and run
 the tests automatically.

 This script is free software; you can redistribute it and/or modify 
 it under the same terms as Perl itself. 

 Usage:

   To run the test and submit the test results to an e-mail address simply
   use the following syntax.

  ./esp-r_crontest.pl <branch> <platform> <email adress>

  Example
  ./esp-r_crontest.pl branches/development_branch linux ibeausol\@nrcan.gc.ca  

 Other branches and platforms can be tested against by changing the
 \$branch and \$platform variables.  Lists of platforms are only available
 to NRCan at the moment.  Future work could include doing a test between
 two branches, or a revisions and not requiring an archive.

 Requirements
   - Perl 4+
   - subversion client
   - mail server (mailhost) 
   - Perl NET\:\:smtp

 ";
 
 print $synopsys;
 die();

}
 
#===================================================================
#====================================================================
#====================================================================
#====================================================================
#====================================================================

use Cwd;

#--------------------------
# Prototypes:
sub stream_out($);
sub mail_message($$$$$);
sub main();
sub execute($);
sub parse_forcheck($);
#--------------------------

# List of esp-r binaries to be tested, and 
# corresponding source folders
%binlist = ( "aco"   =>   "esruaco",     
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
                          
%binalias = ( "vew"    =>   "viewer",
              "mrt"    =>   "espvwf" );                          
                          
$Test_base_URL="https://svn2.cvsdude.com/espr/esp-r/";
             
             
#Mailhost to use....only available within NRCan. 
$smtp_server="Mailhost.nrcan.gc.ca";
             
# Target directories for reference and test repositories             
             
# Collect arguements from @ARGV. This implementation 
# is really ugly, and can be improved.
#Branch name                
$branch_name=$ARGV[0];      
#Platform archive to be tested against. 
$Platform=$ARGV[1];
#Comma separated notification list.         
@addresses = split /,/, $ARGV[2];          
#Vebosity 
if ( $ARGV[3] =~ /-vv/ ){
  $veryverbose = 1;
  $verbose = 1;
}elsif ( $ARGV[3] =~ /-v/ ){
  $verbose = 1;
  $veryverbose = 0
}else{
  $verbose = 0;
  $veryverbose = 0;
}
$verbose =1 ;
$veryverbose = 1;
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

#Find a suitable folder to do testing in:

$TestFolder_root="/tmp/.test_esp-r";
$TestFolder=$TestFolder_root;
$count = 0;
while ( -d $TestFolder ) {
  $count++;
  $TestFolder="$TestFolder_root\_$count";
}


#Username that sends e-mail. 
$mail_from= $ENV{USER}.'@esp-r.net';

# Initialize results buffer
$results  = "Automated test of ESP-r system\n";
$results .= "Testing commenced on ".`date`."\n";

# Report configuration
stream_out("Configuration:\n");
stream_out("  - Working directory: $TestFolder\n");
stream_out("  - svn_source:        $branch_name\n");
stream_out("  - archive platform:  $Platform\n");
stream_out("  - email destination: @addresses\n");


main();

sub main()
{
  # Save current path 
  $path = getcwd(); 
  $fail = 0; 
  
  # Create test directory
  stream_out("\nPreparing test directory $TestFolder...");
  execute ("rm -fr $TestFolder");
  execute ("mkdir $TestFolder");
  chdir("$TestFolder");
  stream_out("Done\n");
  
  stream_out("Current path: ".getcwd()."\n");
  
  # Check out code
  stream_out("\nChecking out $branch_name from ESP-r central...");
  execute("svn co $Test_base_URL/$branch_name/");
  stream_out("Done\n");
  
  # Check out archived results
  stream_out("\nChecking out archived results for $Platform...");
  execute("svn co https://132.156.178.22/espr/espr-test-results/$Platform/");
  stream_out("Done\n");
  
  #-----------------------------------------------------
  # Forcheck Analysis
  #-----------------------------------------------------
  
  chdir("$TestFolder/$subbranch_name/src/");
  stream_out("Current path: ".getcwd()."\n");
  # Build a copy of ESP-r
  stream_out("\nBuilding X11 version of ESP-r for use with Forcheck.");
  buildESPr("X11","debug","onebyone");
  stream_out(" Done\n");



  
  chdir("$TestFolder/$subbranch_name/src/esrubps");
  stream_out("Current path: ".getcwd()."\n");
  
  # Save user's existing FCKCNF variable
  $orig_FCKCNF = $ENV{FCKCNF};
  stream_out("Old FCKCNF path: $ENV{FCKCNF}\n");
  $ENV{FCKCNF} = "$path/esp-r.cnf";
  stream_out("New FCKCNF path: $ENV{FCKCNF}\n");
  
  stream_out("Path to forchk binary".`which forchk`."\n");
  
  stream_out("\nRunning Forcheck static analyzer.");
  $results .= "========= RESULTS FROM STATIC ANALYSIS =========\n";
  
  # Invoke forcheck for each ESP-r binary
  foreach $bin ( sort keys %binlist ){
    
    $folder = $binlist{$bin};
    
    $Status = "Passed"; 
    
    stream_out ".";
    
    # Move to appropriate folder.
    chdir("$TestFolder/$subbranch_name/src/$folder");
       
    # Run forcheck, and parse output. Use system command, because
    # execute function may redirect output to /dev/null, depending on 
    # verbosity
    system ("forchk -I ../include *.F ../lib/esru_blk.F ../lib/esru_libX11.F ../esru_ask.F  > forcheck_$bin.out 2>&1 " ); 
    
    
    %new_msgs = parse_forcheck( `cat forcheck_$bin.out` );
   
    # Parse results in archive 
    %old_msgs = parse_forcheck( `cat $TestFolder/$Platform/forcheck_$bin.out` );
    
    # Build master hash containing number of occurances for both old and new errors
    
    %all_msgs = (); 

    while ( ( $code, $count ) = each %old_msgs ){
      if ( ! defined( $new_msgs{$code} ) ){ 
        $all_msgs{$code} = "$count:0";
        $new_msgs{$code} = "-";
      }else{
        $all_msgs{$code} = "$count:";
      }
    }
    while ( ( $code, $count ) = each %new_msgs ){
      if ( ! defined( $old_msgs{$code} ) ){ 
        $all_msgs{$code} = "0:$count";
        $old_msgs{$code} = "-";
      }else{
        $all_msgs{$code} .= "$count";
      }
    }
  
    # Empty code arrays 
    @error_codes   = ();
    @warning_codes = ();
    @info_codes    = (); 
    @total_codes   = (); 
  
    # Build lists of errors/warnings/info msgs for which the number of 
    # instances do not match between old and new bins. 
    while ( ( $code, $count ) = each %all_msgs ){
    
      ($old_count, $new_count)= split /:/, $count;
      
      if ( /^Total\./ ) { 
        push @Total_codes, $code;
      } 
      
      if ( $old_count != $new_count ) {
        
        # Set flags
        $Status = "Failed";
        $fail   = 1;
        
        # Stuff code into appropriate array
        for ( $code ) {
          SWITCH: {
           if ( /^Err\./  )  { push @error_codes, $code;      last SWITCH; } 
           if ( /^Warn\./ )  { push @warning_codes, $code;    last SWITCH; } 
           if ( /^Info\./ )  { push @info_codes, $code;       last SWITCH; } 
          }
        }
      }
      
    }
    # Prepare output 
    $results .= "\n - binary $bin: $Status\n";
    
    # Summarize results
    $results .= "   Total errors:     Archive $old_msgs{\"Total error messages\"}, $branch_name $new_msgs{\"Total error messages\"} \n";
    $results .= "   Total warnings:   Archive $old_msgs{\"Total warning messages\"}, $branch_name $new_msgs{\"Total warning messages\"} \n";
    $results .= "   Total info msgs:  Archive $old_msgs{\"Total informational messages\"}, $branch_name $new_msgs{\"Total informational messages\"} \n";
    
    # Now append unmatched errors to result string 
    if ($Status =~ /Failed/){
      $results .= "   Summary of differences:\n";
    }
    foreach $code ( sort @error_codes ){
      $results .= "     -> $code [ Instances: Archive $old_msgs{$code}, $branch_name $new_msgs{$code} ]\n";
    }
    foreach $code ( sort @warning_codes ){
      $results .= "     -> $code [ Instances: Archive $old_msgs{$code}, $branch_name $new_msgs{$code} ]\n";
    }
    foreach $code ( sort @info_codes ){
      $results .= "     -> $code [ Instances: Archive $old_msgs{$code}, $branch_name $new_msgs{$code} ]\n";
    }
    
  }
  
  stream_out("Done\n");
    
  # Reset FCKCNF
  $ENV{FCKCNF} = $orig_FCKCNF;
  stream_out("Reset FCKCNF path: $ENV{FCKCNF}\n");
  
  #---------------------------------------
  # Build ESP-r 
  #---------------------------------------
  
  #Compile Esp-r. 
 
  $results .= "\n\n========= RESULTS FROM COMPILATION TESTS =======\n\n";
  
  chdir("$TestFolder/$subbranch_name/src/");
  execute("make clean");  
  stream_out("\nBuilding X11 version of ESP-r.");
  $results .= buildESPr("X11","clean","onebyone");
  stream_out("Done\n");
   
  chdir("$TestFolder/$subbranch_name/src/");
  execute("make clean");
  stream_out("\nBuilding GTK version of ESP-r.");
  $results .= buildESPr("GTK","clean","onebyone");
  stream_out("Done\n");
  
  chdir("$TestFolder/$subbranch_name/src/");
  execute("make clean");
  stream_out("\nBuilding noX version of ESP-r.");
  $results .= buildESPr("noX","clean","onebyone");  
  stream_out("Done\n");
 
  # Set global failure flag if compilation broke.
  if ( $results =~ /failed/ ){ $fail = 1; }
 
  #--------------------------------------- 
  # Run ESP-r over test suite, using 
  # the noX version built previously
  #--------------------------------------- 
  #Test Results, and email to list. 
  
  
  # Build a clean copy of esp-r to ensure databases are available.
  chdir("$TestFolder/$subbranch_name/src/");
  execute("make clean");
  stream_out("\nBuilding noX version of ESP-r for regression test");
  buildESPr("noX","clean","together");  
  stream_out("Done\n");
  
  stream_out("\nExercising bps over test suite...");
  chdir("$TestFolder/$subbranch_name/tester/scripts/");
  execute("./tester.pl $TestFolder/esp-r/bin/bps  --historical_archive $TestFolder/$Platform/historical_archive.tar.gz -d $TestFolder/esp-r -p $TestFolder/$subbranch_name/tester/");
  
  $results .= "\n\n========= RESULTS FROM REGRESSION TEST =========\n\n";
  # Digest test report 
  $results .= `cat bps_test_report.txt`;
  stream_out("Done\n");
  
  #================================================
  # Mail out results. 
  #================================================
  stream_out("Mailing results...");
   
  if ( $fail ){
    $subject = "ESP-r automated test: Fail"
  }else{
    $subject = "ESP-r automated test: Pass"
  }
  
  foreach $address (@addresses){
    mail_message($smtp_server,$address,$mail_from,$subject,$results);
  }
  stream_out("Done\n");

  system("echo @addresses > ~/cron_output.txt");
  system("echo $results >> ~/cron_output.txt");

  stream_out("Deleting working directory $TestFolder...\n");
  execute("rm -fr $TestFolder");
  stream_out("Done\n");
  
  if ( $verbose ) {
  
    stream_out ("\n\n$results\n\n");
  
  }
}



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
    $smtp->datasend("$Message\n");
    
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

#-------------------------------------------------------------------
# Compile ESP-r with specified Xlibs, debug state...
#-------------------------------------------------------------------
sub buildESPr($$$){
  
  my($xLibs,$state,$build) =@_;
  
  # Set debug flag
  if ( $state =~ /clean/ ) { 
    $Debug_flag =""; 
  }else{
    $Debug_flag ="--debug"
  }
  
  
  # Empty target folder 
  execute("rm -fr $TestFolder/esp-r");
  
  # Compile ESP-r
    
  $err_msg = "";
  $Status="Passed"; 
  
  
  if ( $build =~ /onebyone/ ){
    # Test each binary separately, omit training and databases.
    foreach $bin (sort keys %binlist){
        
      if ( defined ( $binalias{$bin} ) ){
        $target = $binalias{$bin};
      }else{
        $target = $bin;
      }
      stream_out(".");
      $debug_temp = `./Install -d $TestFolder --xml --silent --$xLibs $Debug_flag --no_dbs --no_training --force $bin 2>&1`;
      if ( ! -r "$TestFolder/esp-r/bin/$target"      && 
           ! -r "$TestFolder/esp-r/bin/$target.exe"     ){
        $Status="Failed";    
        $err_msg .= "    -> Binary $target could not be built.\n";
        $debug .= " ERROR: Target: $TestFolder/esp-r/bin/$target: ".`ls -la $TestFolder/esp-r/bin/$target`;
        $debug .= "\n $debug_temp \n\n";
      }
    }
  }else{
    # Build all at once, omit training files.
    execute("./Install -d $TestFolder --xml --silent --$xLibs $Debug_flag --no_training --force");
  }
  
  # Return result
  return "\n - Compilation of $xLibs version of ESP-r: $Status\n$err_msg";
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
    print ("\n  -> Executing: $command\n");
    $result = system("$command");
  }else{
    # we might one-day dump to a log file here 
    $result = system("$command >/dev/null");
  }
  return $result;
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
  $output =~ s/.*    -- messages presented://sg;
  
  # Eliminate empty lines
  $output =~ s/^\s+//gm;
  
  @lines = split /\n/, $output;
  
  my (%messages);
  
  foreach $line (@lines){
    if ($line =~ /number of error messages:/ ){
      $error_messages = $line;
      $error_messages =~ s/number of error messages:\s+//g;
      $messages{"Total error messages"}=$error_messages;
    }elsif ($line =~ /number of warnings:/ ){
      $warning_messages = $line;
      $warning_messages =~ s/number of warnings:\s+//g;
      $messages{"Total warning messages"}=$warning_messages;
    }elsif ($line =~ /number of informative messages:/ ){
      $info_messages = $line;
      $info_messages =~ s/number of informative messages:\s+//g;
      $messages{"Total informational messages"}=$info_messages;
    }else{
      ($count,$code,$desc) = split /\]|\[/, $line;
      ($number,$type)= split / +/, $code;
      $count =~ s/x//g;
      $count =~ s/\s//g;
      $messages{"$shorthand{$type} #$number - $desc"}=$count;
    }
  }
  return %messages;
}