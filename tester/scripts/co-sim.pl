#-------------------------------------------------------------------
# Dependencies
#-------------------------------------------------------------------
use Switch;
use File::Copy;
use File::Spec;
use Time::HiRes qw(gettimeofday tv_interval);
	
use constant VERSION_LINE => 0;			# Line number of harmonizer input file with version number
use constant HARMONIZER_DLL_LINE => 2;	# Line number of harmonizer input file with location of harmonizer dll
use constant BPS_DLL_LINE => 4;			# Line number of harmonizer input file with location of bps dll
use constant TRNSYS_DLL_LINE => 6;		# Line number of harmonizer input file with location of TRNSYS dll
use constant BPS_CL_LINE  => 8;			# Line number of harmonizer input file with command line for bps
use constant TRNSYS_DCK_LINE => 10;		# Line number of harmonizer input file with location and name of TRNSYS deck file
use constant CONVERGENCE_LINE => 12;	# Line number of harmonizer input file with convergence criteria
#-------------------------------------------------------------------
# Scope global variables. All variables beginning with a 'g' are
#-------------------------------------------------------------------
my %gTest_cosim_paths;  # paths associated with co-simulation testing
my %gTest_cosim_files;  # paths associated with co-simulation models
my %gCosim_Run_Times;   # simulation run times

my $gTempESPrPath; #path to temporary local model folder
my $gHomePath; #path to temporary local model folder

#-------------------------------------------------------------------
# Process each test case
#-------------------------------------------------------------------

sub process_cosim_test($$){
 
  my ($har_file, $archive_path) = @_;
  #make sure the file is a unix file - otherwise chomp() behaves very unpredicably.
  execute("dos2unix $har_file");
  #check the contents of the harmonizer input file (do dlls etc. exisit)
  print("checking harmonizer file \n");
  check_harmonizer_file($har_file);
  #set up and run the co-simulation
  print("testing co-sim \n");
  test_cosimulation($har_file, $archive_path);
}
#-------------------------------------------------------------------
# write a summary file
#-------------------------------------------------------------------

sub write_summary_file($$$$){
	
  my ($har_path, $archive_path, $date) = @_;
  
  # Scope variable storing all output
  my @output = ();

  push @output, " co-sim.pl Test Report";
  push @output, " $date";
  push @output, " ";

  push @output, " ";
  push @output, " Testing done on .har files in $har_path directory";

  foreach my $har_file (%gCosim_Run_Times){
	  my $file = $har_file;
	  #remove path
	  $file =~ s/$har_path//gi;
	  push @output, " $file";
  }
  print("writing co-sim_summary_report\n");

  open (REPORT, ">co-sim_summary_report.txt");
  foreach my $line ( @output ){
    print REPORT "$line \n"; 
  }
  close (REPORT);
}

	

	


#-------------------------------------------------------------------
# Open .har file and check to see that the specified files and dlls exist
#-------------------------------------------------------------------
sub check_harmonizer_file($){
 
  my ($har_file) = @_;
  $linecount = 0;
  stream_out("\n\n > TESTING: co-simulation specified in $har_file \n\n");
  #get current path to prepend to lines 7 & 9 to give full path to cfg & dck files
  $gHomePath = getcwd();
  open (HAR_FILE, $har_file) or fatalerror("Could not open $har_file for reading!");
   		#process file one line at a time.
        while ( my $line = <HAR_FILE> ) {
	        chomp($line);
	        #check that the dlls & deck file exsist.
	        if (($linecount==HARMONIZER_DLL_LINE)||($linecount==BPS_DLL_LINE)||($linecount==TRNSYS_DLL_LINE)){
		        check_file_exists($line); 
				SWITCH: {
					$linecount == HARMONIZER_DLL_LINE && do{$gTest_cosim_paths{'harmonizer'} = $line; last SWITCH;};
					$linecount == BPS_DLL_LINE && do{$gTest_cosim_paths{'bps'} = $line; last SWITCH;};
					$linecount == TRNSYS_DLL_LINE && do{$gTest_cosim_paths{'trnsys'} = $line; last SWITCH;};
				}
			}
	        if ($linecount==BPS_CL_LINE){
		        #remove initial "bps -file "
		        $line =~ s/bps -file / /gi;
		        #remove initial "-mode text -p test silent"
		        $line =~ s/-mode text -p test silent/ /gi;
			    #remove whitespace from the start and end of the string
			    $line =~ s/^\s+//;
				$line =~ s/\s+$//;
  		        #get rid of \n
		        chomp($line);
		        #add current path to partial path
		        $gTest_cosim_files{'cfgfile'} = $gHomePath.$line;
		        #check that the cfg file exists.
		        check_file_exists($gTest_cosim_files{'cfgfile'}); 
		        
	        }
	        if ($linecount==TRNSYS_DCK_LINE){
		        #add current path to partial path
				$gTest_cosim_files{'deckfile'} = $gHomePath.$line; 
		        #check that the dck file exists.
		        check_file_exists($gTest_cosim_files{'deckfile'}); 
	        }
	        $linecount++;
        }
        # Close file
        close(HAR_FILE);
        # set the correct paths (i.e. remove the dlls from the paths and set as msys paths)
        $gTest_cosim_paths{'harmonizer'} =~ s/harmonizer.dll//gi;
        $gTest_cosim_paths{'harmonizer'} = clean_path($gTest_cosim_paths{'harmonizer'});
        $gTest_cosim_paths{'bps'} =~ s/bps.dll//gi;
        $gTest_cosim_paths{'bps'} = clean_path($gTest_cosim_paths{'bps'});
        $gTest_cosim_paths{'trnsys'} =~ s/trndll.dll//gi;
        $gTest_cosim_paths{'trnsys'} = clean_path($gTest_cosim_paths{'trnsys'});
		$gTest_cosim_files{'deckfile'} = clean_path($gTest_cosim_files{'deckfile'});
        $gTest_cosim_paths{'deckfile'} = extract_path_to_file($gTest_cosim_files{'deckfile'},1);
		$gTest_cosim_files{'cfgfile'} = clean_path($gTest_cosim_files{'cfgfile'});
	    $gTest_cosim_paths{'espr_model'} = extract_path_to_file($gTest_cosim_files{'cfgfile'},2);

#        print(" harmonizer:  $gTest_cosim_paths{'harmonizer'} \n");
#        print(" bps:  $gTest_cosim_paths{'bps'} \n");
#        print(" trnsys:  $gTest_cosim_paths{'trnsys'} \n");
#        print(" deckfile:  $gTest_cosim_files{'deckfile'} \n");
#        print(" deckfile path:  $gTest_cosim_paths{'deckfile'} \n");
#	    print(" cfgfile:  $gTest_cosim_files{'cfgfile'} \n"); 
#	    print(" espr_model:  $gTest_cosim_paths{'espr_model'} \n\n\n\n");
    
}
#-------------------------------------------------------------------
# Check to see that the specified file/dll exists
#-------------------------------------------------------------------
sub check_file_exists($){
	my ($filename) = @_;
    # check the status of the dll
    if (! -e $filename){
        fatalerror("file $filename does not exist.");
#        print("file $filename does not exist.\n");
    }
}

#----------------------------------------------------------------------
# Substitute backslashes with forward slashes.
# (escape the backslash and slash with backslash)
#----------------------------------------------------------------------
sub clean_path ($){

  my ( $path ) = @_;
  #replace back slash with forward slash
  $path =~ s/\\/\//g;
  return $path;
}

#----------------------------------------------------------------------
# Extract the path to the file
#----------------------------------------------------------------------
sub extract_path_to_file($$){

  my ( $file_path, $extract_length) = @_;
  #seperate the string by its slashes
  my @parts = split '/', $file_path; 
  #remake the path to the model by removing the last part of the string
  $length = @parts;
  $count = 0;
  my $path;
  while ($count < $length-$extract_length){
  	$path = $path.@parts[$count].'/';
  	$count++;
	}
  chop $path;
  return $path;
}

#-------------------------------------------------------------------
# test_cosimulation sets up and runs the co-simulation specified by 
# the .har file.  Since bps can't handle long path names, the ESP-r 
# model needs to be relocated and the harmonizerfile rewritten to 
# specify the new location.
#-------------------------------------------------------------------
sub test_cosimulation($){
  
  my ($har_file, $archive_path) = @_;
   
  #copy .har file to path where harmonizer dll is 
  $file_name = extract_har_file($har_file);
  $temp_name = $gTest_cosim_paths{'harmonizer'}.'temp_'.$file_name;
  execute("cp $file_name $temp_name ");
#  print("cp $file_name $temp_name \n");

  #make a new temp folder to copy ESP-r model into.
  $gTempESPrPath = substr($gTest_cosim_paths{'bps'}, 0, 3).'local_models/';
  execute("mkdir $gTempESPrPath");
# print("mkdir $gTempESPrPath\n");

  #copy ESP-r model folder (-fr) to a temp folder 
  execute("cp -fr $gTest_cosim_paths{'espr_model'} $gTempESPrPath");
#  print("cp -fr $gTest_cosim_paths{'espr_model'} $gTempESPrPath\n");

  #ensure any old out.csv files are deleted
  $olddb3file = $gTempESPrPath.'out.db3';
  if (-e $olddb3file) {
	  execute("rm $olddb3file");
  }
  $oldcsvfile = $gTempESPrPath.'out.csv';
  if (-e $oldcsvfile){
	   execute("rm $oldcsvfile");
   }

  #extract the latter part of the path to the cfg file (e.g. /ESP-r/cfg/yyy.cfg).
  $copy_path = extract_path_to_cfg($gTest_cosim_files{'cfgfile'});
  my $temp_espr_model = $gTempESPrPath.$copy_path;
#  print("temp_espr_model $temp_espr_model\n");
  #modify $temp_espr_model for use in harmonizer file (i.e. C:/xxx/yyy.cfg format)
  $temp_espr_model =~ s/\//\\/g;#replace slashes
  #open copied .har file and rewrite the bps command line to point to the temporary model. 
  chdir($gTest_cosim_paths{'harmonizer'});
#  print("temp_espr_model $temp_espr_model\n");
  modify_har_file($temp_name, $temp_espr_model);
  
  my ( $time_start, $time_end, $user_time, $run_time);
  print("executing 'harmonizer.exe $temp_name'\n");
  #time the co-simulation.
  $time_start = [gettimeofday];
  #run the co-simulation.
  execute("./harmonizer.exe $temp_name");
  print("moving results file \n");
  #get the elapsed time
  $elapsed_time = tv_interval ($time_start);
  # Save run-time data
  $gCosim_Run_Times{"$har_file"} = $elapsed_time;
 
  # Prep message for reporting to screen.
  $run_time = "($gCosim_Run_Times{$har_file} seconds on CPU)";
  stream_out("done. $run_time\n");

  
    #find co-sim results files and move them to archive folder for future zipping
  find_and_move_results($har_file, $archive_path);


  #seek clarification on 1.save levels 2.shading stuff
}
  
#----------------------------------------------------------------------
# Extract the .har file from its path
#----------------------------------------------------------------------
sub extract_har_file($){

  my ( $file_path ) = @_;
  $file_path =~ s/(^|.*\/)([^\*\/]*)\.har$/$2/g;
  $file_path = $file_path.'.har';
  return $file_path;
}


#----------------------------------------------------------------------
# Extract the ESP-r model folder
#----------------------------------------------------------------------
sub extract_path_to_cfg($){

  my ( $file_path ) = @_;
  #seperate the string by its slashes
  my @parts = split '/', $file_path; 
  $length = @parts;
  #remake the cfg_folder string from the last three parts of the path.
  $cfg_folder = @parts[$length-3].'/'.@parts[$length-2].'/'.@parts[$length-1];
  #get the cfg file.
  return $cfg_folder;
}

#----------------------------------------------------------------------
# Rewrite the location of the bps model in the .har file 
#----------------------------------------------------------------------
sub modify_har_file($$){
  my ($har_file, $new_path) = @_;
  $linecount = 0;
  $newline = '';
  # Open file and read contents 
  open (HAR_FILE, $har_file) or fatalerror("Could not open $har_file for reading!");
  		#read file one line at a time & save in a temporary array
        while ( my $line = <HAR_FILE> ) {
	        if ($linecount==BPS_CL_LINE){
		        #write new location of the cfg file in bps command line.
		        $cmdline = replace_path_in_string($line,$new_path);
             	$file_contents[$linecount] = $cmdline;
	        }
	        elsif ($linecount==TRNSYS_DCK_LINE){
				#replace back slash with forward slash
  				$line =~ s/\\/\//g;
  				#prepend current path
				$winpath = $gHomePath.$line;
		        #convert to windows path  
				$winpath = `cygpath -w $winpath`;
             	$file_contents[$linecount] = $winpath;
	        }
	        else{
	        	 $file_contents[$linecount] = $line;
        	 }
	        $linecount++;
        }
        # Close file
        close(HAR_FILE);
    # Reopen file with status 'new' and write out contents.
    open(WRITE_FILE, ">$har_file") or fatalerror("Could not open $file for writing!");
        foreach my $line ( @file_contents ){
          print WRITE_FILE $line;
#          print("$line");
        }
        close(WRITE_FILE);
}

#----------------------------------------------------------------------
# Replace the path in the bps command line.
#----------------------------------------------------------------------
sub replace_path_in_string($$){
  my ( $path1,$path2 ) = @_;
  #seperate the string by its spaces
  my @parts1 = split ' ', $path1; 
  $count1 = @parts1;
  $count = 0;
  $newpath = '';
  while ( $count  < $count1 ) {
	if ($count != 2){
  		$newpath =  $newpath.@parts1[$count].' ';
	}
	else{
  		$newpath =  $newpath.$path2.' ';
		}
  	$count++;
  }
  $newpath =  $newpath."\n";
  return $newpath; 
}
#--------------------------------------------------------------------
# Find results files out.xml and TRNSYS .out file, and move them to 
# a common results folder.  Then rename them according to co-simulation 
# .har file in preperation for tar & ziping the folder once all the 
# results hae been collected.
#--------------------------------------------------------------------
sub find_and_move_results($$){
	
    my ($har_file,$archive_path ) = @_;
    #rename out.csv to a unique name related to the .har file name
	execute ("mkdir $archive_path");
	#Get the destination path
	$harmonizer_name = extract_har_file($har_file);
	$destination = $archive_path.'/'.$harmonizer_name;
	$destination_csv = $destination;
    #replace 'har' with 'csv'
    $destination_csv =~ s/\.har/\.csv/g;
	#the out.csv file is wherethe cfg file is.    
	$csv_file1 = $gTempESPrPath.'ESP-r/cfg/out.csv';;
	if (-e $csv_file1){
		copy($csv_file1, $destination_csv) or print "File $csv_file1 cannot be copied to $destination_csv.\n";
#		print("copy $csv_file1 to $destination_csv\n");
	}
    #remove temporary folder.
    execute("rm -fr $gTempESPrPath");
	#now look for the TRNSYS output file
    $trnsys_out_file = $gTest_cosim_files{'deckfile'};
    #replace 'dck' with 'out'
 	$trnsys_out_file =~ s/\.dck/\.out/g;
	#if the file exists copy it  
	if (-e $trnsys_out_file){
 	    $destination_out = $destination;
	    #replace 'har' with 'out'
 	    $destination_out =~ s/\.har/\.out/g;
		copy($trnsys_out_file, $destination_out) or print "File $trnsys_out_file cannot be copied to $destination_out.\n";
#		print("copied $trnsys_out_file to $destination_out.\n");
	}
	else{
		print ("Did not find $trnsys_out_file file in $gTest_cosim_paths{'deckfile'}\n");
	}	
}
 
#--------------------------------------------------------------------
# Create a new archive, tar & gzip files.
#--------------------------------------------------------------------
sub create_historical_cosim_archive($$$){
	
  my ($archive_path,$tar_command,$zip_command) = @_;
  chdir($archive_path);
  # Tar and zip historical archive file. First, move to parent directory,
  # and collect relative name for archive (this is necessary because
  # tar behaves differently depending if the supplied path is relative
  # or absolute).


  $archive_file_name = $archive_path;
  $archive_file_name =~ s/^.*\///g;
  execute("$tar_command $archive_file_name.tar $archive_file_name ");


  # Compress archive
  execute("$zip_command $archive_file_name\.tar");

}
#-------------------------------------------------------------------
# Compare reference and test results for a model
#-------------------------------------------------------------------
sub compare_cosim_results_files($$){

  my ($file1, $file2) = @_;
  
  # check if files differ.
  if ( ! compare( $reference_file, $test_file ) ){
       # files are identical 
            $gTest_Results{"$folder/$model"}{$extention} = "pass";
      }else{
       # Files differ 
            $gTest_Results{"$folder/$model"}{$extention} = "fail";
	  }
    # Compute average CPU runtimes for all save-levels
    # and determine increase / reduction. Run times are
    # not available in very verbose mode.
    if ( $gTest_params{"test_efficiency"} ){
      
      foreach my $version ( ("reference", "test" ) ){

        # Reset intitial value.
        $gRun_Times{"$folder/$model"}{"avg"}{$version} = 0;
        
        foreach my $level (@gSave_levels){

          
          $gRun_Times{"$folder/$model"}{"avg"}{$version} =
            $gRun_Times{"$folder/$model"}{"avg"}{$version}
            + $gRun_Times{"$folder/$model"}{$level}{$version}/
             ( scalar(@gSave_levels) ) ;

        }
      }

      if ( $gRun_Times{"$folder/$model"}{"avg"}{"reference"} > 0 ){
        $gRun_Times{"$folder/$model"}{"chg"} =
          ( $gRun_Times{"$folder/$model"}{"avg"}{"test"} -
            $gRun_Times{"$folder/$model"}{"avg"}{"reference"} ) /
            $gRun_Times{"$folder/$model"}{"avg"}{"reference"} * 100;
            
      }else{
        $gRun_Times{"$folder/$model"}{"chg"} = "N/A";
      }
      
    }else{
        $gRun_Times{"$folder/$model"}{"chg"} = "N/A"
    }

  # Report overall test result to buffer
  stream_out (
                sprintf ("    - %-20s %s \n","Overall:",
                          $gTest_Results{"$folder/$model"}
                          {"overall"})
                             
  );
}



#-------------------------------------------------------------------
# Create a report describing the test results. 
#-------------------------------------------------------------------
sub create_cosim_report(){
  
  # move back to master path
  chdir $gTest_paths{"master"};
  
  # Scope variable storing all output
  my @output = ();
  my ($current_line, $current_rule);  

  push @output, " tester.pl Test Report";
  push @output, " Testing commenced on $gSys_params{\"date\"} $gSys_params{\"time\"}";
  push @output, " ";

  push @output, " ";
  push @output, " Test parameters:";
  
  if ( $gTest_params{"single_case"} ){
    push @output, "  - Test case:              <>$gTest_paths{\"single_case\"}";
  }else{
    push @output, "  - Test suite path:        <>$gTest_paths{\"test_suite\"}";
  }
  if ( $gTest_params{"compare_to_archive"} ){
    push @output, "  - Reference archive:      <>$gTest_paths{\"old_archive\"}";
  }
  push @output, "  - Abbreviated runs:       <>".is_empty($gTest_params{"abbreviated_runs"});
  if ( $gTest_params{"abbreviated_runs"} ) {
    push @output, "  - Abbreviated run period: <>$gTest_params{\"abbreviated_run_period\"}";
  }

  push @output, " ";

  if ( $gTest_params{"compare_to_archive"} ) {
    push @output, " Reference System Information:";
    push @output, "  - Username:               <>$gRef_Sys_params{\"username\"}";
    push @output, "  - Host:                   <>$gRef_Sys_params{\"hostname\"}";
    push @output, "  - Platform:               <>$gRef_Sys_params{\"sys_type\"}";
    push @output, "  - Operating system:       <>$gRef_Sys_params{\"os_type\"}";
  }
  push @output, " Test System Information:";
  push @output, "  - Username:               <>$gSys_params{\"username\"}";
  push @output, "  - Host:                   <>$gSys_params{\"hostname\"}";
  push @output, "  - Platform:               <>$gSys_params{\"sys_type\"}";
  push @output, "  - Operating system:       <>$gSys_params{\"os_type\"}";

  push @output, " ";
  push @output, " bps binaries:";
  push @output, "  - Path:                   <>(reference) <> $gRef_Sys_params{\"hostname\"}:$gRef_Test_params{\"test_binary\"}";
  push @output, "                            <>(test)      <> $gSys_params{\"hostname\"}:$gTest_params{\"test_binary\"}";
  
  push @output, "  - SVN source:             <>(reference) <> $gRef_Test_params{\"test_bin_svn_src\"}";
  push @output, "                            <>(test)      <> $gTest_params{\"test_bin_svn_src\"}";

  push @output, "  - Compilers:              <>(reference) <> $gRef_Test_params{\"test_bin_compilers\"}";
  push @output, "                            <>(test)      <> $gTest_params{\"test_bin_compilers\"}";

  push @output, "  - Graphics library:       <>(reference) <> $gRef_Test_params{\"test_bin_graphics_lib\"}";
  push @output, "                            <>(test)      <> $gTest_params{\"test_bin_graphics_lib\"}";

  push @output, "  - XML support:            <>(reference) <> $gRef_Test_params{\"test_bin_xml_support\"}";
  push @output, "                            <>(test)      <> $gTest_params{\"test_bin_xml_support\"}";

  push @output, "  - Modifiation date:       <>(reference) <> $gRef_Test_params{\"test_bin_mod_date\"}";
  push @output, "                            <>(test)      <> $gTest_params{\"test_bin_mod_date\"}";
  push @output, "  - MD5 Checksum:           <>(reference) <> $gRef_Test_params{\"test_bin_md5sum\"}";
  push @output, "                            <>(test)      <> $gTest_params{\"test_bin_md5sum\"}";
  if ( compare ( $gTest_params{"test_binary"}, $gRef_Test_params{"test_binary"}) ){
    push @output, "                            <>            <> (files differ)";
  }else{
    push @output, "                            <>            <> (files are identical)";
  }
  push @output, " ";

  my $out_string = " Compared output:";
  foreach my $extention (sort keys %gTest_ext){
    if ( $gTest_ext{$extention} ){
      $out_string .= " .$extention";
    }
  }
  push @output, "$out_string files";


  # Header for report

  my $folder_length = 30;
  my $model_length = 50; 


  # Prepare summary
  if ( $gAll_tests_pass ){
    push @output, " Overall result: Pass.";
  }else{
    push @output, " Overall result: Fail.";
  }
  push @output, " ";
  push @output, " Summary of test results:";
  push @output, "   - '-' indicates test case passes";
  push @output, "   - 'X' indicates test case fails";
  push @output, "   - '.' indicates files were not produced, or were not compared";
  $current_rule = " ^^^^^^^^^^^^^^^^^^^^"
                  ."^^^^^^^^^^^^^^^^^^^^"
                  ."^^^^^^^^^^^^^^^^^^^^"
                  ."^^^^^^^^^^^^^^^^^^^^"
                  ."^^^^^^^^^^^^^^^^^^^^"
                  ."^^^^^^^^^^^^^^^^^^^^"
                  ."^^^^^^^^^^^^";
  push @output, $current_rule;
  push @output, sprintf (" %\-".$folder_length."s<>  %-".$model_length."s<> .xml <> .data<> .csv <> .h3k <> overall<> dt-CPU(%%)", "Folder", "Model");
  push @output, $current_rule;

  # Loop throug results, and report to buffer
  foreach my $test ( sort keys %gTest_Results ){
    my ($folder,$model) = split /\//, $test;
    my $xml_pass  = result_to_string($test,"xml");
    my $csv_pass  = result_to_string($test,"csv");
    my $h3k_pass  = result_to_string($test,"h3k");
    my $data_pass = result_to_string($test,"data");
    my $overall_pass = result_to_string($test,"overall");
    my $cpu_change = $gRun_Times{"$folder/$model"}{"chg"};
    # add extra space to align +ive and -ive CPU runtime changes
    my $spacer = "";
    if ( $gTest_params{"test_efficiency"} && $cpu_change !~ /N\/A/){
      if ( $cpu_change > 0 ){$spacer=" ";}
      $cpu_change = sprintf($spacer."%-10.2g", $cpu_change);
    }else{
      $cpu_change = "N/A";
    }
    
    push @output, sprintf (" %\-".$folder_length."s<>  %-".$model_length."s<>   %1s  <>   %1s  <>   %1s  <>   %1s  <>    %1s    <> ".$spacer."%-10s  ", $folder, $model, $xml_pass, $data_pass, $csv_pass, $h3k_pass, $overall_pass, $cpu_change);
  }
  push @output, $current_rule;
  push @output, "  ";

  if ( $gTest_params{"test_efficiency"} ){
    push @output, " Parameter dt-CPU describes the percent change in simulation CPU ";
    push @output, " runtime between the reference and test versions of bps.";
    push @output, "   - When different versions of bps are exercised on the same";
    push @output, "     machine, dt-CPU is a measure of the relative efficieny of";
    push @output, "     the ESP-r source code. ";
    push @output, "   - When the same version of bps is exercised on different ";
    push @output, "     machines, dt-CPU is a measure of the comparative performance";
    push @output, "     of ESP-r on different hardware and operating systems.";
  }else{
    push @output, " Efficiency testing disabled.";
  }
  push @output, "  ";
  push @output, " =========== Comparison of XML results ================= ";
  # Detailed report of XML output comparison
  if ( $gTest_ext{"xml"} && ! $gXML_Report_needed ){
    push @output, " ";
    push @output, " No differences were found in XML output. Detailed report unnecessary. ";
    push @output, " ";
  }elsif ( $gTest_ext{"xml"} ){

    my $current_folder = "";
    my $current_model  = "";

    
    push @output, " XML output: Detailed report ";
    push @output, " Maximum observed error:";
    foreach my $unit ( @gReportUnitComp ){
      if ( defined ( $gMax_difference{"global"}{"$unit"} ) ){
        push @output, "  -> Units:  $unit ";
        push @output, "       - Folder:                 <> $gMax_difference{\"global\"}{\"$unit\"}{\"folder\"}";
        push @output, "       - Model:                  <> $gMax_difference{\"global\"}{\"$unit\"}{\"model\"}.cfg";
        push @output, "       - Element:                <> $gMax_difference{\"global\"}{\"$unit\"}{\"element_path\"} ($gMax_difference{\"global\"}{\"$unit\"}{\"attribute\"})";
    
        push @output, "       - Difference:             <> ".sprintf("%\-12s%\-10s<>(<>%\-12s%%)",
                                                               format_my_number($gMax_difference{"global"}{"$unit"}{"absolute"},15,"%-10.5g"),
                                                               $unit,
                                                               format_my_number($gMax_difference{"global"}{"$unit"}{"relative"},15,"%-10.5g"),
                                                             );
      }
    }
    push @output, " ";
    push @output, "  -> Tolerances for comparisons: ";
    $current_rule = " ^^^^^^^^^^^^^^^^^^^^"
                   ."^^^^^^^^^^^^^^^^^^^^"
                   ."^^^^^^^^^^^^^^^^^^^^";
    push @output, $current_rule;                   
    push @output, sprintf ( " %-30s<>%-30s", "Units", "Tolerance" );
    push @output, $current_rule;
    foreach my $unit ( sort keys %gTolerance ){
      push @output, sprintf ( " %-30s<>%-30s", $unit,
            ( defined ($gTolerance{$unit}) && defined ($gTolerance{$unit}) > 0)?
            format_my_number ($gTolerance{$unit},30,"%10.5g")
            : "            ---            ");     
    }
    push @output, $current_rule;
    push @output, " ";


    # Create horizontal rule. 
    $current_rule = " ";
    for ( my $ii = 0; $ii<186; $ii++){
         $current_rule .= "^";
    }
   
    foreach my $failure ( sort keys %gXML_Failures ){
      my ($folder,$model,$element_path,$attribute) = split /;/, $failure;

      # Add header row, if this is a new model or folder.
      if ( $folder ne $current_folder || $model ne $current_model){

        if ( $current_folder ) {
          # Add footer, if necessary.
          push @output, $current_rule;
          push @output, " ";
        }
      
        $current_folder = $folder;
        $current_model = $model;

        push @output, " TEST CASE $model ($folder)";
        push @output, "  - Folder:                     <> $folder";
        push @output, "  - Model:                      <> $model.cfg";
        foreach my $unit ( @gReportUnitComp ){
          if ( defined ( $gMax_difference{"$folder;$model"}{"$unit"} ) ){
            push @output, sprintf("%\-29s<>%\-12s<>%\-10s(%\-12s%%)","  - MAX error ($unit)"
                                                                    ,format_my_number($gMax_difference{"$folder;$model"}{"$unit"}{"absolute"},15,"%\-10.5g")
                                                                    ,$unit
                                                                    ,format_my_number($gMax_difference{"$folder;$model"}{"$unit"}{"relative"},15,"%\-10.5g")
                                                                   )
                                                             ." - observed in:<> $gMax_difference{\"$folder;$model\"}{\"$unit\"}{\"element_path\"} ($gMax_difference{\"$folder;$model\"}{\"$unit\"}{\"attribute\"})";
          }
        }
        push @output, $current_rule;
        $current_line  = sprintf  (" %\-80s<>%\-20s[]",
                                   "Elements exhibiting differences", "Units");
        $current_line .= sprintf  ("%\-20s<>%\-20s<>%\-20s<>%\-20s[]",
                                   "Relative", "  Absolute", "    Reference", "    Test");
        push @output, $current_line;
        
        $current_line  = sprintf  (" %\-80s<>%\-20s[]"," ", " ");
        $current_line .= sprintf  ("%\-20s<>%\-20s<>%\-20s<>%\-20s[]",
                                   "Difference (%)", "  Difference", "    Value", "    Value");
       

        push @output, $current_line;
        push @output, $current_rule;
      }

      $current_line  = sprintf (" %\-80s<>%\-20s[]", "$element_path ($attribute)", $gXML_Failures{$failure}{"units"});

      $current_line .= sprintf ("%15s%5s<>%15s%5s<>%15s%5s<>%15s%5s[]",
                                format_my_number(
                                        $gXML_Failures{$failure}{"difference"}{"relative"},15,"%10.5g"),
                                "",
                                format_my_number(
                                        $gXML_Failures{$failure}{"difference"}{"absolute"},15,"%10.5g"),
                                "",
                                format_my_number(
                                        $gXML_Failures{$failure}{"difference"}{"reference_value"},15,"%10.5g"),
                                "",
                                format_my_number(
                                        $gXML_Failures{$failure}{"difference"}{"test_value"},15,"%10.5g"),
                                ""
                                );

      push @output, $current_line;


                     
    }

    # Add footer for last model
    push @output, $current_rule;
    push @output, " ";
    
  }


  if ( $gTest_params{'run_callgrind'} ){
    push @output, " ";
    push @output, " =========== Comparison of Callgrind results =========== ";
    push @output, " ";
 

    # Loop through models, and note changes

    foreach my $key ( sort keys %gCallGrindResults ){
      my ($folder,$model) = split /\//, $key;

      push @output, " Model:  $model";
      push @output, " Folder: $folder";
      push @output, sprintf(" Instructions - Reference version : %20s",
                              format_instruction_count($gCallGrindResults{$key}{'reference'}{'all procedures'}{'total_instructions'}));
      push @output, sprintf(" Instructions - test version :      %20s",
                              format_instruction_count($gCallGrindResults{$key}{'test'}{'all procedures'}{'total_instructions'}));
      push @output, sprintf(" Difference :                       %20s",
                              format_instruction_count($gCallGrindResults{$key}{'deltas'}{'all procedures'}{'diff'}));
      push @output, sprintf (" Change in number of instructions: %.1f  %%",
                             $gCallGrindResults{$key}{'deltas'}{'all procedures'}{'percent'} );

      $current_rule = " ^^^^^^^^^^^^^^^^^^^^"
                    ."^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
                    ."^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^";

      my $sub_rule = $current_rule;
      $sub_rule =~ s/\^/./g;


      push @output, $current_rule;

      $current_line = sprintf(" %-50s<>  %-20s<>  %20s<>  %-40s", "Procedure", "Change in # of inst.", "% of all inst.", "Called from");

      push @output, $current_line;

      push @output, $current_rule;


      my @procedure_order = @{$gCallGrindResults{$key}{"procedures_assorted_desc"}};
      my $first = 1;
      foreach my $procedure ( @procedure_order ){

        if ( ! IsLowLevel($procedure) ){

          # Get change in insturctions
          my $ir_diff = $gCallGrindResults{$key}{"deltas"}{$procedure}{"diff"};

          # only output results if instruction count has changed.
          if ( $ir_diff != 0 ) {

            if ( ! $first )  {
              push @output,$sub_rule;
              
            }
            $first = 0;

            # Fill an array containing a wrapped version of the string name.
            # We do this because some procedure names are very long, and
            # cannot be accomidated in a readable table. 
            my @name = ();
            my $str_start = 0;
            my $str_end = length($procedure);

            while ( $str_start < $str_end  ){

              my $remaining_string = substr $procedure, $str_start;
              # Cut string into 40-character segments, and push on to array
              my $strlength = length($remaining_string) < 40 ? length($remaining_string): 40;
              my $substr = $str_start == 0 ? " " : "   ";
              $substr .= (substr $procedure, $str_start, $strlength);
              push @name,  $substr;
              $str_start += 40;
            }
            

            # Fill an array containing all the references for this
            # procedure in the test version --- that is, the procedures
            # that call this routine, and the number of times they call them.
            my @references = (); 

            my $reference_hash = $gCallGrindResults{$key}{"test"}{$procedure}{"references"};

            foreach my $reference ( sort keys %{$reference_hash} ){

              # Truncate calling procedure name at 60 characters
              # and append "..." if necesary
              my $sub_string = length($reference) < 60 ? $reference : ( substr $reference, 0, 60)."...";

              # Add calling routine name and call count to array of references.
              push @references, "$sub_string $gCallGrindResults{$key}{'test'}{$procedure}{'references'}{$reference}{'callcount'}";

            }

            # Get readable string describing the total change in intstructions to this
            # procedure 
            my $inst_delta = format_instruction_count($gCallGrindResults{$key}{'deltas'}{$procedure}{'diff'});
            
            my $inst_frac  = sprintf ("%.2f",$gCallGrindResults{$key}{'test'}{$procedure}{"local_instructions"}
                                                / $gCallGrindResults{$key}{'test'}{'all procedures'}{'total_instructions'} * 100);

            while ( scalar( @name ) != 0 || scalar ( @references ) != 0 ){

              my $name_str = scalar ( @name ) ? shift @name : "";
              my $call_str = scalar ( @references ) ? shift @references : "";

              push @output, sprintf("%-50s<>  %20s<>  %20s<>  %-60s",
                                    $name_str, $inst_delta, $inst_frac, $call_str ) ;

              $inst_delta = "";
              $inst_frac= "";

            }
          }
        }
      }

      push @output, $current_rule;
      push @output, "";
      push @output, "";

    }

  }

  # Write report to disk
  if ( $gTest_params{"report_format"} =~ /csv/ ){
    $gTest_params{"report_file"} .= ".csv"
  }elsif ( $gTest_params{"report_format"} =~ /ascii/ ){
    $gTest_params{"report_file"} .= ".txt"
  }

  open (REPORT, ">$gTest_params{\"report_file\"}");
  foreach my $line ( @output ){
    if ($gTest_params{"report_format"} =~ /csv/ ){
      # Comma separated file. Remove ascii table characters (^, |)
      $line =~ s/\|//g;
      $line =~ s/\^//g;
      # Nuke extraneous white space
      $line =~ s/ +/ /g;
      #Replace <> and [] with ,
      $line =~ s/<>/,/g;
      $line =~ s/\[\]/,/g;
    }elsif ( $gTest_params{"report_format"} =~ /ascii/ ){
      # Replace ^ with -
      $line =~ s/\^/-/g;
      # replace <> with ' '
      $line =~ s/<>//g;
      # replace [] with '|'
      $line =~ s/\[\]/|/g;
    }

    print REPORT "$line \n"; 
   
  }

  close (REPORT);

}




# Note that you need the 1; at the end of the file. 
# This is because Perl needs the last expression in the file to return a true value. 
1;
