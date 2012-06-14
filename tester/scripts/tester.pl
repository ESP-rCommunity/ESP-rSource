#!/usr/bin/perl
#====================================================================
#====================================================================
#====================================================================
#====================================================================
#====================================================================
#
# tester.pl
# Author: Alex Ferguson
#
# Copyright: Natural Resources Canada 2005-2012
#
# './tester.pl --help' for details. 
#
#--------------------------------------------------------------------
# SYNOPSYS:
#--------------------------------------------------------------------
#
# tester.pl will exercise bps over a variety of test cases and
# examine its output files for differences in simulation predictions.
# A detailed list of arguements and usage cases is provided in 
# the definition of $Help_msg text, below.
# 
# Traditionally, tester.pl compared the out.xml and out.csv files 
# produced in a save-level 5 simulation. It performed a simple diff
# for out.csv output, but it also parsed the out.xml files and 
# reported a list of specific varibles that differed (and by how 
# much). 
#
# This script is free software; you can redistribute it and/or modify 
# it under the same terms as Perl itself.  
#
#====================================================================
#====================================================================
#====================================================================
#====================================================================
#====================================================================


#-------------------------------------------------------------------
# Perl Dependencies
#-------------------------------------------------------------------
use warnings;
use strict;
use File::Compare;
use Cwd;
use Cwd 'chdir';
use File::Find;
use Math::Trig;

# Note that the script also uses the "Simple.pm" package if 
# XML files are to be tested. This package is loaded further below. 

#-------------------------------------------------------------------
# Prototypes 
#-------------------------------------------------------------------


sub InitializeParams();
sub ProcessCmdArgs($);
sub GetBinInfo(); 
sub ValidateArchives(); 
sub PrepLocalFolders(); 
sub process_case($);
sub invoke_tests($$$$$);
sub move_simulation_results($$$$$);
sub CollectCallGrindResults($$$);
sub create_report();
sub compare_results($$);
sub CollectSummaryResults($$); 
sub CompareNumericalResults($$$); 
sub DelTempFiles(); 
sub CleanParams(); 
sub fatalerror($);
sub is_empty($);
sub is_true_false($);
sub execute($);
sub log_msg($);
sub resolve_path($);
sub format_instruction_count($);
sub stream_out($);
sub echo_config();
sub IsLowLevel($);
sub parse_hash_recursively();
sub create_historical_archive();
sub process_historical_archive($); 

# These next two functions help sort the values within a hash. 

#----------------------------------------------------------------------#
#  FUNCTION:  hashValueAscendingNum                                    #
#                                                                      #
#  PURPOSE:   Help sort a hash by the hash 'value', not the 'key'.     #
#             Values are returned in ascending numeric order (lowest   #
#             to highest).                                             #
#----------------------------------------------------------------------#

sub hashValueAscendingNum {
   my %hash;
   $hash{$a}{'diff'} <=> $hash{$b}{'diff'};
}


#----------------------------------------------------------------------#
#  FUNCTION:  hashValueDescendingNum                                   #
#                                                                      #
#  PURPOSE:   Help sort a hash by the hash 'value', not the 'key'.     #
#             Values are returned in descending numeric order          #
#             (highest to lowest).                                     #
#----------------------------------------------------------------------#

sub hashValueDescendingNum {
   my %hash;
   $hash{$b}{'diff'} <=> $hash{$a}{'diff'};
}


#-------------------------------------------------------------------
# Scope global variables. All variables beginning with a 'g' are
# considered global, and may be used in subordinate routines.
#-------------------------------------------------------------------
our %gSys_params;           # system parameters
our %gTest_params;          # test parameters
our %gRef_Test_params;      # parameters from archived reference case
our %gRef_Sys_params;       # Refence system parameters
our %gTest_paths;           # paths associated with testing
our %gTestable_files;
our %gTest_Results;
our %gNum_Failures;
our %gTest_ext;             # Testable exentions

our $gRefXML;               # Object containing reference XML file
our $gTestXML;              # Object containing reference XML file

our @gCurrent_path;         # Stacks used to traverse XML file
our %gElements;             #   quickly

our %gNum_Compare_Vars;     # Variables suitable for numerical
						   #   comparison in xml output .

our %gCallGrindResults;     # Results from call-grind comparisons

our %gMax_difference;       # array containing maximum differences

our %gTolerance;            # Tolerance(s) for comparisons
our $gSmall;                # Tolerance for close-to-zero comparisons
						   #   (used in div/0 error trap)

our @gDumped_parameters;    # Array containing list of test parameters
						    #    to be included in archive configuration
						    #    file

our @gProcessed_args;       # Array containing arguements as intrepreted by 
                            #    tester.pl. 							
							
our $gNum_Report_needed=0;  # Flag indicating differences were found
						    #    in the xml, and a report should be
						    #    created.

our %gRun_Times;            # Run times for simulation
													  
our $gAll_tests_pass = 1;   # Flag indicating result of all tests.
						    #   (defaulted to 'pass', set to 'fail'
						    #    on first failure).
our @gReportUnitComp;
													  
our $gProcessed_Archive_count=0; # number of archives processed.

our $Help_msg; 

our @Env_Paths; 

our @gSave_levels; # Save-levels to be tested (typically 4 and/or 5) 

our ($gTmp_Test_archive, $gTmp_Ref_archive);

# System time. 
our ($Sec,$Min,$Hour,$Mday,$Month,$Year,$Wd,$Yday,$Isdst);


#-------------------------------------------------------------------
# Start of procedures
#-------------------------------------------------------------------

InitializeParams();  # Initialize parameters to default values and 
                     # collect system information. 

					 
				 
				
# dump text and quit, if no arguement given. Otherwise, process arguements. 
if (!@ARGV){
  print $Help_msg;      # $Help_msg is defined in InitializeParams().
  die;
}else{
  ProcessCmdArgs(\@ARGV); # Parse and trap command-line arguements.		
                          # Resolve relevant paths to bps/ish binaries, 
                          # and/or historical archives						  
}				
	
# Tell user that we're doing something.   
stream_out("\n tester.pl $gSys_params{'date'} $gSys_params{'time'}\n\n");					 


#-----------------------------------------------------------------------
# Collect info about the bps binaries to be tested. 	
# (includes date, path, md5sum, and output of 'bps -buildinfo').
#-----------------------------------------------------------------------                 
GetBinInfo(); 	        

#-----------------------------------------------------------------------
# If comparisons are made with historical archives (.tar.gz), open and 
# validate content of those archives. 
#-----------------------------------------------------------------------
# U						  
if ( $gTest_params{"compare_to_archive"} ||     
     $gTest_params{"compare_two_archives"}) {

	ValidateArchives(); 
	
}


#-----------------------------------------------------------------------
# Set save-levels according to the extentions that are to be tested. 
# Note: during comparisons to historical archive s $gTest_ext may have been 
# adjusted if all specified extentions are not present in the archive. 
#-----------------------------------------------------------------------

if ($gTest_ext{"data"}) {
  push @gSave_levels, 4;
}

if ($gTest_ext{"h3k"}     ||
    $gTest_ext{"xml"}     ||
  	$gTest_ext{"csv"}     ||
	  $gTest_ext{"summary"} ||
    $gTest_ext{"fcts"}       ){
  push @gSave_levels, 5;
}

if ($gTest_ext{"xml"}     ){

	# Load Simple package --- needed to parse xml output.
	# Simple.pm is not part of the standard perl distribution,
	# and must be shipped with tester.pl. It can be
	# redistributed under the GPL-like perl artistic licence.
	BEGIN{
	  require "Simple.pm" 
         or fatalerror ("Could not find Perl module Simple.pm (needed to parse out.xml files)\n");
	  Module->import();
	}
}


#-----------------------------------------------------------------------
# Finished parsing/error trapping.
# If requested, spit configuration out to buffer
#-----------------------------------------------------------------------

CleanParams();     # Strip any unneeded white space from 

# Spit out configuration and quit, if prompted.
if ( $gTest_params{"echo_config"} ) {
  print echo_config();
  die;
}

#-----------------------------------------------------------------------
# Prepare results/simulation folders
#-----------------------------------------------------------------------
PrepLocalFolders(); 

#-----------------------------------------------------------------------
# All preparation work is done. Start running tests! 
# This loop searches for .cfg files in the specified test path 
# (or at ../test_suite if no path specified), and invokes routine 
# process_case when it finds one. 
#-----------------------------------------------------------------------

if ( ! $gTest_params{"compare_two_archives"} ){

  # recursively search through test-case path for cfg files.
  find( sub{
        # move on to next file if (1) file is a directory,
        # (2) file is not readable, or (3) file is not
        # cfg file.
        return if -d;
        return unless -r;
        return unless $File::Find::name =~ m/\.cfg$/;
		  # file is a .cfg file. Run the case!
          process_case($File::Find::name);
        },  $gTest_paths{"test_suite"} );
  

}else{
  # we're comparing two historical archives. There is no need to search for 
  # cfg files or run any tests. Collect a list of files we can compare, 
  # and call function compare_results()
  foreach my $key ( keys %gTestable_files ){

    my ($folder, $model) = split /\//, $key;

    stream_out(" > TESTING: $model (in folder  $folder)\n");
    stream_out("   Comparing files from archives.\n");

    compare_results($model, $folder);

  }

}
#-----------------------------------------------------------------------
# Create test report. 
#-----------------------------------------------------------------------
if (   $gTest_params{"create_report"} &&
     ( $gTest_params{"compare_versions"}    ||
       $gTest_params{"compare_to_archive"}  ||
       $gTest_params{"compare_two_archives"}   ) ){
  create_report();
}

#-----------------------------------------------------------------------
# If a historical archive is to be created, write out test configuration
# file to archive, tar & gzip output files
#-----------------------------------------------------------------------
if ( $gTest_params{"create_archive"} ){
  create_historical_archive();
}

#-----------------------------------------------------------------------
# Clean up
#-----------------------------------------------------------------------

DelTempFiles();

# That's it!


####################################################################
####################################################################
##############                                     #################
##############        HIGH-LEVEL FUNCTIONS         #################
##############                                     #################
####################################################################
####################################################################

#-------------------------------------------------------------------
# Initalize default parameters and collect system info. 
#-------------------------------------------------------------------

sub InitializeParams(){	
	
														  
	#-------------------------------------------------------------------
	# Help text. Dumped if help requested, or if no arguements supplied.
	#-------------------------------------------------------------------
	our $Help_msg = "

	 tester.pl

	 USAGE: tester.pl [OPTIONS] reference_bps test_bps
			tester.pl [OPTIONS] -a <reference_archive> test_bps
			tester.pl [OPTIONS] -a <reference_archive> -a <test_archive>
	 
	 ABSTRACT:
	 ---------
	 
	 tester.pl will exercise different bps executables over a
	 variety of input files, and report on differences in the
	 simulator output. tester.pl can also be used to create
	 an archive of simulation results, and to compare results
	 with those produced by a previous version of the simulator.

	 Results are written to a file called bps_test_report.txt,
	 located in the invocation directory.

	 
	 OPTIONS:
	 --------

		-h, --help: Print this message and quit.

		-p <PATH>, --path <PATH>: Search for test cases in <PATH>.
			 tester.pl will attempt to run bps on any '.cfg' files
			 found within the provided path. Default: ../test_suite/

		-c <FILE>, --case <FILE>: Run tests on a single model described
			 by FILE.

		-d <PATH>, --databases <PATH>: Use standard ESP-r databases
			 and climate files located in <PATH> when running simulations.
			 Default: /usr/esru/esp-r

		-a <FILE>, --historical_archive <FILE>: Compare results to the
			 historical archive contained in <FILE>. If specified,
			 tester.pl will uncompress the <FILE> archive and
			 compare simulation output to archived results. When
			 comparing results to historical archive, tester.pl
			 expects only bps executable to be specified.

		--create_historical_archive <FILE>: Create a historical archive
			 named <FILE>. tester.pl will save simulation output in
			 a tar/gzipped file called <FILE>. If only one bps
			 executable is specified, tester.pl will run the simulations
			 using the specified bps and then quit. If two executables
			 are provided, tester.pl will compare the output from both
			 and save the output from the new bps (ie. the second one
			 in the command arguments) in an archive.

		--ref_loc <PATH>: Use the res and ish applications found at <PATH>
			 when running the reference version of bps.

		--test_loc <PATH>: Use the res and ish applications found at <path>
			 when running the test version of bps.
			 
		--no_csv: Don't test csv files.
		
		--no_data: Don't test data files created from res output.

		--no_h3k: Don't test h3k files.
		
		--no_xml: Don't test XML files.

		-run_callgrind: Run callgrind to test code efficiency. Note:
			 this option significantly slows testing. It should be
			 invoked with with the '-p <path>' option, where <path>
			 denotes the path to a single .cfg file in the test-suite
			 folder. If run over the entire test suite, call-grind
			 analysis may take days.

			 If bps binaries are provided with symbolic debugging
			 information (that is, they are built with the --debug
			 option), callgrind analysis will provide estimates on the
			 comparative increase or decrease in code efficiency on
			 a procedure-by-procedure basis.

			 Callgrind analysis requires both the valgrind and
			 callgrind_annotate tools.

		--adj_tol <VALUE>: Scale the test's comparison tolerances by
			 <VALUE>.
		
		--save_results: Optionally save simulation output for manual
			 comparison. If specified, the simulation output will
			 be saved in a folder called 'results_output', in
			 the current directory.

		--short: Run abbreviated simulations. If specified, tester.pl
			 will run each simulation for one day in January. By default,
			 the tester will run each test-case over the period described
			 by its 'test' simulation preset

		--csv_output: Output report in comma-separated-variable format
			 for compatability with spreadsheet software.          

		--diff_data: Pause testing and perform a diff of .data files
			 when reference and test .data files are found to differ.
			 This option causes tester.pl to behave like the legacy
			 ESP-r 'TEST' QA tool.
			 
		--diff_tool: Command-line arguement specifiying the third-party
			 tool that should be used when comparing .data files. 
			 Default: 'diff -iw'

		-v, --verbose: Report progress and results to the buffer.

		-vv, --very_verbose: Report simulation messages and low-level
			 activity to the buffer. Note: Code efficiency will not be
			 tested when the very-verbose option is active. 

		--echo: Report test configuration and quit.

		
	 SYNOPSIS:
	 ---------

	 tester.pl automates development and pre-release regression testing
	 of ESP-r's bps binary. tester.pl can:

		- compare results from two separate bps binaries,
		- create an archive of results suitable for cross-platform
		  comparisons and comparisons to future binaries, and
		- comparing results from a bps binary with results obtained
		  on other platforms and using older versions of bps.

	 OVERVIEW:
		  
	 When invoked, tester.pl will search for cfg files located in the 
	 test suite (../test_suite by default.)  tester.pl makes a copy
	 of each model, invokes bps, and compares the simulation results
	 to i) results obtained from another version of bps, or ii) an
	 archived result set. tester.pl will also create a new result set
	 archive for future use if the '--create_historical_archive' option
	 is invoked.

	 ARCHIVED RESULT SETS:

	 tester.pl supports creation of and comparisons with archived result
	 sets. This feature eases cross-platform comparisons and comparisons
	 with older versions of the simulator.

	 tester.pl saves key simulation parameters (such as the simulation
	 duration) along with the simulation results when creating a results
	 archive. If future comparisons are undertaken using options that
	 conflict with these parameters, the

	 RESULTS COMPARISON:
	 
	 tester.pl supports comparison of the following files:

		- *.data files produced by the ANALYSE script, which
		  post-processes the building results libraries
		  generated during a save-level 4 run and produces
		  a summary ASCII report. 

		- out.csv files produced by H3Kreports that contain
		  time-step output from the climate, building, plant
		  and electrical domains,

		- out.xml files produced by H3Kreports that contain
		  summary and integrated data from the climate,
		  building, plant and electrical domains.

		- *.h3k files, that summarize the output from the
		  idealized hvac models.

		- *.fcts files, that contain time-step output from
		  the pre-IEA/ECBCS Annex 42 fuel cell model.

	 The *.data, out.csv, *.h3k and *.fcts files are merely checked
	 for binary equality --- tester.pl considers the test to 'pass'
	 if the files are identical, and any differences in the files
	 will cause the test to fail.

	 However, tester.pl supports more detailed examination of the
	 out.xml files. These files are parsed and their contents
	 compared on a metric-by-metric basis.  The files are assumed
	 to fail of the absolute or relative difference between any metric
	 exceeds specified tolerances. It's also possible to adjust the
	 tolerances used in this comparison using the '--adj_tol' command-
	 line option.

	 In addition, tester.pl records the CPU runtime required for each
	 simulation, and reports on the relative increase or decrease in
	 simulation runtime between the reference and test version of bps.
		  
	 REPORTING:

	 tester.pl provides output in aASCIItext and comma-separated-value
	 formats. The test report is written to the bps_test_report.txt file
	 (or bps_test_report.csv, if the '--report_csv' option is specified.)

	 test.pl can also be configured to report progress and and results
	 to screen using the '--verbose' option.

	 ADDING NEW MODELS:

	 tester.pl locates its own input models. To add new models to the
	 test suite, the model files need only be copied into the test
	 suite directory (../test_suite). For compatability, the models
	 must contain a simulation preset named 'test'.

	 A representative simulation period should be chosen that adequately
	 exercises the model, but does not uunnecessarilylengthen simulation
	 time. Models sensitive to climate conditions should be exercised
	 using two different cfg files --- one ppertainingto a week in
	 January, and another to a week in July.

	 tester.pl also supports testing with models that are not in the
	 default test suite. Alternate test suites can be specified using the
	 '--path' option, and a single test case can be specified using the 
	 '--case' option.
					 
	 EXAMPLES:
	 ---------

		Exercise two versions of bps over the files in ../test_suite,
		compare results from save-levels 4 and 5:

		  \$ ./tester.pl /path/to/reference/bps /path/to/test/bps

		Exercise two versions of bps over the files in ../test_suite,
		compare results from save-levels 4 and 5, and save results from
		new bps binary in an archive:

		  \$ ./tester.pl /path/to/reference/bps /path/to/test/bps
				--create_historical_archive historical_achive.tar.gz

		Create an archive using a specified version of bps:

		  \$ ./tester.pl /path/to/reference/bps
				--create_historical_archive historical_achive.tar.gz

		Compare the results from a bps binary with those stored
		in an archived result set:

		  \$ ./tester.pl /path/to/test/bps
				--historical_archive historical_achive.tar.gz

		Exercise tester over a single day for a given ESP-r model,
		compare to historical archive, and follow results on screen.

		  \$ ./tester.pl/ /path/to/test/bps
				--historical_archive historical_achive.tar.gz
				--case /path/to/cfg/file.cfg -v

		Use non-standard databases located in ~/esp-r/:

		  \$ ./tester.pl /path/to/reference/bps /path/to/test/bps
				--databases ~/esp-r/
				
	";
	 


	($Sec,$Min,$Hour,$Mday,$Month,$Year,$Wd,$Yday,$Isdst)=localtime(time);

	$Year  += 1900;
	$Month += 1;

	if ( $Mday  < 10 ){ $Mday  = "0$Mday"; }
	if ( $Hour  < 10 ){ $Hour  = "0$Hour"; }
	if ( $Min   < 10 ){ $Min   = "0$Min";  }
	if ( $Sec   < 10 ){ $Sec   = "0$Sec";  }
	if ( $Month < 10 ){ $Month = "0$Month";}

	$gSys_params{'date'} = "$Mday/$Month/$Year";
	$gSys_params{'time'} = "$Hour:$Min:$Sec";

	$gSys_params{'sys_type'} = `uname -m`;
	$gSys_params{'os_type'}  = `uname -s`.":".`uname -r`;
	$gSys_params{'username'} = $ENV{'USER'};
	$gSys_params{'hostname'} = `uname -n`;

	#-------------------------------------------------------------------
	# Convert uname ouptut to a managable keyword.
	#  - do case insensitive pattern match on 'os
	#-------------------------------------------------------------------

	for ( $gSys_params{"os_type"} ){
	  SWITCH:{
		if (/linux/i)   { $gSys_params{'os_keyword'} = "linux";   last SWITCH; }
		if (/cygwin/i)  { $gSys_params{'os_keyword'} = "cygwin";  last SWITCH; }
		if (/sunos/i)   { $gSys_params{'os_keyword'} = "sun";     last SWITCH; }
		if (/darwin/i)  { $gSys_params{'os_keyword'} = "osx";     last SWITCH; }
		if (/mingw/i)   { $gSys_params{'os_keyword'} = "mingw";   last SWITCH; }
	  }
	}
  
	#-------------------------------------------------------------------
	# Assign system specific commands as necessary
	#-------------------------------------------------------------------

	# commands for compressing/decompressing files
	# Assume GNU zip
	$gSys_params{'zip_command'}   = "gzip ";
	$gSys_params{'unzip_command'} = "gunzip -c";



	# Assume posix tar is available.
	$gSys_params{'tar_command'}    = "tar cf";
	$gSys_params{'untar_command'}  = "tar xf";

	#-------------------------------------------------------------------
	# Set default paths
	#-------------------------------------------------------------------
	$gTest_paths{'master'} = getcwd();

	# Default paths
	$gTest_paths{'local_models'} = "./local_models";         # path to local models
	$gTest_paths{'results'}      = "./results_output";       # path to results folder
	$gTest_paths{'test_suite'}   = "../test_suite/";         # path to test suite
	$gTest_paths{'single_case'}  = "";                       # Path to an individual test case
	$gTest_paths{'esp-r'}        = "/usr/esru/esp-r/bin";    # path to standard esp-r
	$gTest_paths{'helper_apps'}  = ".;../../validation/QA/benchmark_model/cfg";
															 # paths in which to find helper apps
															 #   helper scripts.
	$gTest_paths{'default_dbs'}  = "/usr/esru/esp-r";
															 # Path to default databases.                                                         

	$gTest_paths{'user_databases'}= "";                      # path to user-specified databases

	$gTest_paths{'new_archive'} = "";   # Paths to old and new historical
	$gTest_paths{'old_archive'} = "";   #   results archive.
												  
	# Get environment paths
	@Env_Paths = split /:|;/, "./;".$ENV{PATH};

	# add empty array location to allow absolute paths to be specified too!
	push @Env_Paths, $gTest_paths{'master'};
	push @Env_Paths, "";
	push @Env_Paths, " ";

	#-------------------------------------------------------------------
	# Set default options
	#-------------------------------------------------------------------
	$gTest_params{'echo_config'}        = 0; # Report configuration to buffer
	$gTest_params{'single_case'}        = 0; # Test a single case (.cfg file)
	$gTest_params{'compare_to_archive'} = 0; # compare results to historical archive
	$gTest_params{'compare_two_archives'} = 0; # compare two historical archives
	$gTest_params{'create_archive'}     = 0; # create historical archive for future
											 #    comparisons
	$gTest_params{'compare_versions'}   = 1; # compare 2 bps executables
	$gTest_params{'verbosity'}          = "quiet"; # How loud should the tester be?

	$gTest_params{'user databases'}     = 0; # Use user specified databases

	$gTest_params{'save_output'}  = 0;       # Save results output.

	$gTest_params{'logfile'} =">/dev/null 2>&1";     # destination for log messages 

	$gTest_params{'period_name'}  ="test";   # name of test period

	$gTest_params{'abbreviated_runs'} = 0;                # Flag for short simulations
	$gTest_params{'abbreviated_run_period'} = "1 1 2 1";  # Start & end date for short simulations.

	$gTest_params{'default_version_#'} = "1.1";        # Default, minumum and maximum
	$gTest_params{'min_version_#'} = "1.0";            #  supported configuration file
	$gTest_params{'max_version_#'} = "1.1";            #  version #'s

	$gTest_params{'create_report'} = 1;                   # Flag to create a report.
	$gTest_params{'report_file'} = "bps_test_report"; # Default report name

	$gTest_params{'configuration_file'} = "configuration_file.txt";
														  # historical archive
														  #    configuration file name 

	$gTest_params{'test_efficiency'} = 1;          # Flag indicating efficiency should be
												   # tessted.

	$gTest_params{'test_eff_arch_version'} = "1.1"; # Earliest historical archive
													# version supporting efficiency
													# test data.                                         
	$gTest_params{'run_callgrind'} = 0;
	$gTest_params{"report_format"} = "ascii";    # Format for report.
														  
	$gTest_params{"diff_data_files"} = 0;        # Optionally perform a diff of 
												 # data files 
												 
	$gTest_params{"third_party_diff_cmd"} = "diff -iw";  # Command to dump out diff

														  
	# list of parameters that should be dumped into the configuration file
	@gDumped_parameters = ( "abbreviated_runs",
							"abbreviated_run_period",
							"test_binary",
							"test_bin_mod_date",
							"test_bin_md5sum",
							"test_bin_svn_src",
							"test_bin_compilers",
							"test_bin_graphics_lib",
							"test_bin_xml_support"
						  );

	# List of variables in the XML output that can be compared.                          
	%gNum_Compare_Vars     = ( "min"             => 1,
							   "max"             => 1,
							   "active_average"  => 1,
							   "active_steps"    => 1,
							   "total_average"   => 1,
							   "content"         => 1 ) ;

	# List of extentions associated with testable output files
	%gTest_ext = (    "xml" => 0,
					  "csv" => 1,
					  "summary" => 1,
					  "h3k" => 0,
					  "data" => 0,
					  "fcts" => 0,
					  "callgrind" => 0
				 );
														  
	# value for close-to-zero comparisons
	$gSmall = 1E-10;

	# Tolerances for comparison. Should these data be
	# stored in some sort of test configuration file?
	%gTolerance =    ( "W"       => 1.0,
					   "oC"      => 0.1,
					   "GJ"      => 1.0E-03,
					   "-"       => 1.0E-03,
					   "%"       => 1.0E-01,
					   "V"       => 1.0E-03,
					   "A"       => 1.0E-03,
					   "kg/s"    => 1.0E-03,
					   "degrees" => 1.0,
					   "m/s"     => 1.0E-03,
					   "W/m2"    => 0.1 );
					   
					   

	# Aliases for other plausable units
	$gTolerance{"K"}               = $gTolerance{"oC"};
	$gTolerance{"C"}               = $gTolerance{"oC"};
	$gTolerance{"MW"}              = $gTolerance{"W"}    / 1.0E06;
	$gTolerance{"kW"}              = $gTolerance{"W"}    / 1.0E03;
	$gTolerance{"g/s"}             = $gTolerance{"kg/s"} * 1.0E03;
	$gTolerance{"dimensionless"}   = $gTolerance{"%"};
	$gTolerance{"relative"}        = $gTolerance{"%"};


	# Array containing list of units for which comparisons should
	# be reported - subset of gTolerance

	@gReportUnitComp =( "W", "oC", "GJ", "V", "A", "m/s", "kg/s" );
}

#-------------------------------------------------------------------
# Process command-line arguements
#-------------------------------------------------------------------
sub ProcessCmdArgs($){

	my ($ArgRef) = @_; 
    
	my @AllArgs = @$ArgRef;
	
	my ($arg, $cmd_arguements,@binaries);

	# Compress arguements into a space-separated string
	foreach $arg (@AllArgs){
	  $cmd_arguements .= " $arg ";
	}

	# Compress white space to a single ' ', and convert to ';'
	$cmd_arguements =~ s/\s+/ /g;
	$cmd_arguements =~ s/\s+/;/g;

	# Translate shorthand arguements into longhand
  
	$cmd_arguements =~ s/-h;/--help;/g;
	$cmd_arguements =~ s/-p;/--path;/g;
	$cmd_arguements =~ s/-c;/--case;/g;
	$cmd_arguements =~ s/-a;/--historical_archive;/g;
	$cmd_arguements =~ s/-v;/--verbose;/g;
	$cmd_arguements =~ s/-vv;/--very_verbose;/g;
	$cmd_arguements =~ s/-d;/--databases;/g;

	# Aliases: 'ref_res' means the same as 'ref_loc'...
	$cmd_arguements =~ s/--ref_res;/--ref_loc;/g;
	$cmd_arguements =~ s/--test_res;/--test_loc;/g;

  $cmd_arguements =~ s/-c;/-p;/g;
  $cmd_arguements =~ s/--case;/--path;/g;
  
	# Collate options expecting arguements, and link option with 
	# arguement by turning trailing ';' to ':' 
	$cmd_arguements =~ s/--databases;/--databases:/g;
	$cmd_arguements =~ s/--adj_tol;/--adj_tol:/g;
	$cmd_arguements =~ s/--path;/--path:/g;
	$cmd_arguements =~ s/--historical_archive;/--historical_archive:/g;
	$cmd_arguements =~ s/--create_historical_archive;/--create_historical_archive:/g;
	$cmd_arguements =~ s/--ref_loc;/--ref_loc:/g;
	$cmd_arguements =~ s/--test_loc;/--test_loc:/g;
	$cmd_arguements =~ s/--diff_tool;/--diff_tool:/g;

	# If any options expecting arguements are followed by other
	# options (ie. the arguement is missing), insert empty arguement:
	$cmd_arguements =~ s/:-/:;-/;

	# remove leading and trailing ;'s (if any)
	$cmd_arguements =~ s/^;//g;
	$cmd_arguements =~ s/;$//g;

	# split processed arguements back into array that we can handle one-by-one
	@gProcessed_args = split /;/, $cmd_arguements;

	# Intrepret arguements: each element in @processed_args is now a separate 
	# arguement, including its associated parameters (if any)
	foreach $arg (@gProcessed_args){
	  SWITCH:
	  {
		if ( $arg =~/^--help/ ){
		  # Dump help messages and quit.
		  print $Help_msg;
		  die;
		  last SWITCH;
		}
	  
		if( $arg =~ /^--path:/){
		  # Path to folder containing test case(s)
		  $gTest_paths{"test_suite"} = $arg;
		  $gTest_paths{"test_suite"} =~ s/--path://g;
		  if ( ! $gTest_paths{"test_suite"} ){
			fatalerror("Path to test suite must be specified with --path (or -p) option!");
		  }
		  last SWITCH;
		}

		if( $arg =~ /^--historical_archive:/ ){
		  # results to be compared to historical archive (expect a tar.gz file)

		  $gTest_params{"compare_versions"}   = 0;

		  if ( ! $gTest_params{"compare_to_archive"} ){

			$gTest_params{"compare_to_archive"} = 1;
			$gTest_paths{"old_archive"} = $arg;
			$gTest_paths{"old_archive"} =~ s/--historical_archive://g;
			if ( ! $gTest_paths{"old_archive"} ){
			  fatalerror("Path to historical archive must be specified with ".
						 "--historical_archive (or -a) option!");
			}

		  }else{

			$gTest_params{"compare_to_archive"} = 0;
			$gTest_params{"compare_two_archives"} = 1;

			$gTest_paths{"new_archive"} = $arg;
			$gTest_paths{"new_archive"} =~ s/--historical_archive://g;
			if ( ! $gTest_paths{"new_archive"} ){
			  fatalerror("Path to historical archive must be specified with ".
						 "--historical_archive (or -a) option!");
			}
		  }

		  last SWITCH;
		}
		if( $arg =~ /^--create_historical_archive/ ){
		  # results to be saved as a historical archive for future use
		  $gTest_params{"create_archive"} = 1;
		  $gTest_paths{"new_archive"} = $arg;
		  $gTest_paths{"new_archive"} =~ s/--create_historical_archive://g;
		  if ( ! $gTest_paths{"new_archive"} ){
			fatalerror("Path to historical archive must be specified with ".
					  "--create_historical_archive option!");
		  }
		  last SWITCH;
		}

		if ( $arg =~ /--databases:/){
		  # User has provided path to non-standard databases
		  $gTest_params{"user_databases"} = 1;
		  $gTest_paths{"user_databases"} = $arg;
		  $gTest_paths{"user_databases"} =~ s/--databases://g;
		  if ( ! $gTest_paths{"user_databases"} ) {
			fatalerror("Path to databases must be specified with ".
					   "--databases option!");
		  }
		  last SWITCH;
		}

		# Switch flag on for valgrind analysis (and measure code efficiency)
		if ( $arg =~ /--run_callgrind/ ){
		  $gTest_params{'run_callgrind'} = 1;
		  $gTest_ext{'callgrind'} = 1;
		  last SWITCH;
		}
		
		if ( $arg =~ /--adj_tol:/ ){
		  # Multiply all comparison tolerances by specified value. 
		  # ( --adj_tol 0 forces exact agreement between results )
		  $arg =~ s/--adj_tol://g;
		  # Check that provided adjustment is actually numeric 
		  if ( $arg !~ /^[0-9]+\.*[0-9]*$/ && $arg !~ /^\.[0-9]+$/ ){
			fatalerror("Specified tolerance ($arg) is not understood!");
		  }
		  # Adjust tolerances.
		  while ( my ($units, $tolerance) = each %gTolerance ){
			$gTolerance{$units} = eval ("$arg\*$gTolerance{\"$units\"}");
		  }
		  last SWITCH;
		}
		
		# Disable specific file comparisons for --no_XXXX arguements
		if ( $arg =~ /--no_xml/ ){
		  $gTest_ext{"xml"} = 0;
		  last SWITCH;
		}
		if ( $arg =~ /--no_csv/ ){
		  $gTest_ext{"csv"} = 0;
		  last SWITCH;
		}
		if ( $arg =~ /--no_data/ ){
		  $gTest_ext{"data"} = 0;
		  last SWITCH;
		}
		if ( $arg =~ /--no_fcts/ ){
		  $gTest_ext{"fcts"} = 0;
		  last SWITCH;
		}
		if ( $arg =~ /--no_h3k/ ){
		  $gTest_ext{"h3k"} = 0;
		  last SWITCH;
		}
		if ( $arg =~ /--no_summary/ ){
		  $gTest_ext{"summary"} = 0;
		  last SWITCH;
		}
		
		
		# Optionally format test report in .csv form for spreadsheet analysis
		if ( $arg =~/--csv_output/ ){
		  $gTest_params{"report_format"} = "csv";
		  last SWITCH;
		}
		  
		  
		if ( $arg =~ /^--save_results/ ){
		  # Save output for comparison purposes
		  $gTest_params{"save_output"} = 1;
		
		  last SWITCH;
		}
		if ( $arg =~ /^--verbose/ ){
		  # stream out progess messages
		  $gTest_params{"verbosity"} = "verbose";
		  
		  last SWITCH;
		}
		if ( $arg =~ /^--very_verbose/ ){
		  # steam out all messages
		  $gTest_params{"verbosity"} = "very_verbose";
		  $gTest_params{"logfile"}="";

		  last SWITCH;
		}
		if ( $arg =~ /^--short/){
		  # run short simulaitons
		  $gTest_params{"abbreviated_runs"} = 1;
		  
		  last SWITCH;
		}
		if ( $arg =~ /^--echo/ ){
		  # Echo configuration
		  $gTest_params{"echo_config"} = 1;
	  
		  last SWITCH;
		}

		if ( $arg =~ /^--ref_loc:/ ){
		  # Path to res for refernece bps
		  $arg =~ s/--ref_loc://g;
		  $gTest_params{"ref_loc"} = $arg ;
		
		  last SWITCH;
		}

		if ( $arg =~ /^--test_loc:/ ){
		  # Path to res for test bps
		  $arg =~ s/--test_loc://g;
		  $gTest_params{"test_loc"} = $arg ;
		
		  last SWITCH;
		}

		if ( $arg =~ /^--diff_data/ ){
		  # invoke a thrid party diff to compare differning data files...
		  $gTest_params{"diff_data_files"} = 1;

		  last SWITCH;
		}
		
		if ( $arg =~ /^--diff_tool/){
		  # use custom thrid-party for .data file comparisons
		  $arg =~ s/--diff_tool://g;
		  $gTest_params{"third_party_diff_cmd"} = $arg;
	# Translate shorthand arguements into longhand
		  last SWITCH; 

		}
		if ( $arg =~ /^-/ ){
		  #arguement is unsupported. Quit.
		  fatalerror("Option $arg is unsupported!");
		}
		  
		# arguement must be an executable. add to exe stack
		push @binaries, $arg;
		
	  }
	}


	# convert relative paths to absolute using resolve_path function.
	$gTest_paths{"local_models"} = resolve_path ( $gTest_paths{"local_models"} );
	$gTest_paths{"test_suite"}   = resolve_path ( $gTest_paths{"test_suite"} );
	$gTest_paths{"single_case"}  = resolve_path ( $gTest_paths{"single_case"} );
	$gTest_paths{"new_archive"}  = resolve_path ( $gTest_paths{"new_archive"} );
	$gTest_paths{"old_archive"}  = resolve_path ( $gTest_paths{"old_archive"} );
	$gTest_paths{"esp-r"}        = resolve_path ( $gTest_paths{"esp-r"} );
	$gTest_paths{"master"}       = resolve_path ( $gTest_paths{"master"} );
	$gTest_paths{"results"}      = resolve_path ( $gTest_paths{"results"} );


	#-------------------------------------------------------------------
	# Check to see that the specified binary (or binaries) exists and
	# is executable.
	#-------------------------------------------------------------------
	my ($binary, $bin_count, $bin_list, $found, $bin_1, $bin_2, $path);
	$bin_count = 0;
	foreach $binary (@binaries){
	  $bin_count++;
	  $bin_list .= " $binary ";
	  # find binary
	  $found = 0;
	  foreach $path ( @Env_Paths ) {
		my $binpath = resolve_path( "$path/$binary" );
		# Check that result is valid
		if ( $binpath ){
		  if ( ! $found &&  -r $binpath ){
			if ( ! -x $binpath ){
			  fatalerror("Binary test file $binary ($binpath) is not executable.");
			}elsif ( ! $bin_1 ){
			  $bin_1 = $binpath;
			}elsif( ! $bin_2 ){
			  $bin_2 = $binpath;
			}
			$found = 1;
		  }
		}
	  }
	  if ( ! $found ) {
		fatalerror ("Test file binary $binary could not be found ".
					"in paths: \n $ENV{PATH} ");
	  }
	}


	# if a single bps is provided and "create_historical_archive" has
	# been specified, a result set will be created for future comparisions.
	# Disable incremental testing.
	if ( $bin_count == 1 && ( $gTest_params{"create_archive"} ) ){
	  $gTest_params{"compare_versions"} = 0;
	}


	if ( $gTest_params{"compare_versions"} ){
	  # 2 bps files should be specified
	  if ( $bin_count != 2 ) {
		fatalerror("Two binary test files expected but $bin_count ($bin_list) specified.");
	  }
	  $gRef_Test_params{"test_binary"} = $bin_1;
	  $gTest_params{"test_binary"} = $bin_2;
	}elsif ( $gTest_params{"compare_to_archive"} || $gTest_params{"create_archive"} ){
	  # a single bps should be specified
	  if ( $bin_count != 1 ) {
		fatalerror("A single binary test file was expected but ".
				   "$bin_count ($bin_list) specified.");
	  }
	  $gTest_params{"test_binary"} = $bin_1;
	}elsif ( $gTest_params{"compare_two_archives"} ){
	  # No bps executables should be specified.
	  if ( $bin_count != 0 ) {
		fatalerror("Two archives were expected, but ".
				   "$bin_count ($bin_list) binary test files were specified.");
	  }
	}


	#-----------------------------------------------------------------------
	# Look for custom res/ish files
	#-----------------------------------------------------------------------
	$gTest_params{"test_res_found"} = 0;
	$gTest_params{"test_ish_found"} = 0;
	if ( defined( $gTest_params{"test_loc"} ) ){
	  # Loop through all paths in environment and append specified path
	  my $pathfound = 0;
	  foreach my $path ( @Env_Paths ){
		# Only proceed if test_inst_path is not defined. 
		if ( ! $pathfound ){
		  
		  # Combine environment and specified paths
		  my $combpath = resolve_path ( "$path/$gTest_params{\"test_loc\"}" );

		  # Check if combined path contains res /ish 
		  if ( -r "$combpath/res" && -x "$combpath/res"  ){
			# Res was found!
			$gTest_params{"test_res_found"} = 1;
		  }
		  if ( -r "$combpath/ish" && -x "$combpath/ish"  ){
			# ish was found !
			$gTest_params{"test_ish_found"} = 1;
		  }

		  # Update test path if res/ish were found 
		  if ( $gTest_params{"test_res_found"} ||
			   $gTest_params{"test_ish_found"}    ){

			$gTest_paths{"test_loc"} = $combpath;
			$pathfound = 1; 
		  }
		  
		}
	  }
	}

	$gTest_params{"ref_res_found"} = 0;
	$gTest_params{"ref_ish_found"} = 0;
	if ( defined( $gTest_params{"ref_loc"} ) ){
	  # Loop through all paths in environment and append specified path
	  my $pathfound = 0;
	  foreach my $path ( @Env_Paths ){
		# Only proceed if test_inst_path is not defined. 
		if ( ! $pathfound ){
		  
		  # Combine environment and specified paths
		  my $combpath = resolve_path ( "$path/$gTest_params{\"ref_loc\"}" );

		  # Check if combined path contains res /ish 
		  if ( -r "$combpath/res" && -x "$combpath/res"  ){
			# Res was found!
			$gTest_params{"ref_res_found"} = 1;
		  }
		  if ( -r "$combpath/ish" && -x "$combpath/ish"  ){
			# ish was found !
			$gTest_params{"ref_ish_found"} = 1;
		  }

		  # Update test path if res/ish were found 
		  if ( $gTest_params{"ref_res_found"} ||
			   $gTest_params{"ref_ish_found"}    ){

			$gTest_paths{"ref_loc"} = $combpath;
			$pathfound = 1;
		  }
		  
		}
	  }
	}

	if ( ! defined($gTest_paths{"ref_loc"}) ) {
	  $gTest_paths{"ref_loc"} = $gTest_paths{"esp-r"};
	}
	if ( ! defined($gTest_paths{"test_loc"}) ) {
	  $gTest_paths{"test_loc"} = $gTest_paths{"esp-r"};
	}

	#-----------------------------------------------------------------------
	# Look for customized database directory. If it could not be found or
	# does not contain databases and climate folders, revert to standard
	# databases. 
	#-----------------------------------------------------------------------
	if ( $gTest_params{"user_databases"} ){

	  $path = resolve_path($gTest_paths{"user_databases"});

	  if ( -d $path &&
		   -r $path &&
		   -x $path ){
		# Check that directory contains climate and databases folders
		if ( ! -d "$path/climate" ||
			 ! -r "$path/climate" ||
			 ! -x "$path/climate"    ){

		  stream_out(
			" Warning: specified database folder ($gTest_paths{\"user_databases\"})\n".
			"          does not contain a 'climate' folder. Using default databases\n".
			"          instead ($gTest_paths{\"default_dbs\"}).\n"
		  );
		  $gTest_params{"user_databases"} = 0;

		}

		if ( ! -d "$path/databases" ||
			 ! -r "$path/databases" ||
			 ! -x "$path/databases"    ){

		  stream_out(
			" Warning: specified database folder ($gTest_paths{\"user_databases\"})\n".
			"          does not contain a 'databases' folder'. Using default databases\n".
			"          instead ($gTest_paths{\"default_dbs\"}).\n"
		  );
		  $gTest_params{"user_databases"} = 0;
		}

		if ( $gTest_params{"user_databases"} ) {
		  $gTest_paths{"user_databases"} = $path;
		}

	  }else{

		stream_out(
			" Warning: specified database folder ($gTest_paths{\"user_databases\"})\n".
			"          could not be found. Using default databases instead.\n".
			"          ($gTest_paths{\"default_dbs\"})\n"
		);

		$gTest_params{"user_databases"} = 0;
		
	  }
	}

}

#-------------------------------------------------------------------
# Get info about bps binaries for use in reports. 
#-------------------------------------------------------------------
sub GetBinInfo(){

	# get modification dates & md5 checksums for test file
	if ( ! $gTest_params{"compare_two_archives"} &&  `which stat ` !~ /no stat/ ){
	  $gTest_params{"test_bin_mod_date"} = `stat --format=%y $gTest_params{'test_binary'}`;
	}else{
	  $gTest_params{"test_bin_mod_date"} = "unknown";
	}
	if ( ! $gTest_params{"compare_two_archives"} && `which md5sum ` !~ /no md5sum/ ){
	  $gTest_params{"test_bin_md5sum"} = `md5sum $gTest_params{'test_binary'}`;
	  $gTest_params{"test_bin_md5sum"} =~ s/\s+.*$//g;
	}else{
	  $gTest_params{"test_bin_md5sum"} = "unknown";
	}
	if ( ! $gTest_params{"compare_two_archives"} ){
	  my $version_dump = `$gTest_params{'test_binary'} -buildinfo`;
	  my @version_info = split /\n/, $version_dump;
	  foreach my $line (@version_info){
		SWITCH:
		{
		  if ( $line =~ /SVN source -/ ){
			$line =~ s/^\s*SVN source -\s*//g;
			$gTest_params{"test_bin_svn_src"} = $line;
			last SWITCH;
		  }
		  if ( $line =~ /Compilers -/ ){
			$line =~ s/^\s*Compilers -\s*//g;
			$gTest_params{"test_bin_compilers"} = $line;
			last SWITCH;
		  }
		  if ( $line =~ /Graphics library -/ ){
			$line =~ s/^\s*Graphics library -\s*//g;
			$gTest_params{"test_bin_graphics_lib"} = $line;
			last SWITCH;
		  }
		  if ( $line =~ /XML output -/ ){
			$line =~ s/^\s*XML output -\s*//g;
			$gTest_params{"test_bin_xml_support"} = $line;
			last SWITCH;
		  }
		}
	  }
	}

	if ( $gRef_Test_params{"test_binary"} && $gTest_params{"compare_versions"} ){
	  # get modification dates & md5 checksums for reference file.
	  # Note: if we"re comparing against a historical archive, these values will
	  # be read from the configuration.txt file.
	  if ( `which stat ` !~ /no stat/ ){
		$gRef_Test_params{"test_bin_mod_date"} = `stat --format=%y $gRef_Test_params{'test_binary'}`;
	  }else{
		$gRef_Test_params{"test_bin_mod_date"} = "unknown";
	  }
	  if ( `which md5sum ` !~ /no md5sum/ ){
		$gRef_Test_params{"test_bin_md5sum"} = `md5sum $gRef_Test_params{'test_binary'}`;
		$gRef_Test_params{"test_bin_md5sum"} =~ s/\s+.*$//g;
	  }else{
		$gRef_Test_params{"test_bin_md5sum"} = "unknown";
	  }
	  
	  # make a copy of the hostname for reporting purposes
	  $gRef_Sys_params{"hostname"} = $gSys_params{"hostname"};

	  # Get version information out of each binary
	  my $version_dump = `$gRef_Test_params{'test_binary'} -buildinfo`;
	  my @version_info = split /\n/, $version_dump;
	  foreach my $line (@version_info){
		SWITCH:
		{
		  if ( $line =~ /SVN source -/ ){
			$line =~ s/^\s*SVN source -\s*//g;
			$gRef_Test_params{"test_bin_svn_src"} = $line;
			last SWITCH;
		  }
		  if ( $line =~ /Compilers -/ ){
			$line =~ s/^\s*Compilers -\s*//g;
			$gRef_Test_params{"test_bin_compilers"} = $line;
			last SWITCH;
		  }
		  if ( $line =~ /Graphics library -/ ){
			$line =~ s/^\s*Graphics library -\s*//g;
			$gRef_Test_params{"test_bin_graphics_lib"} = $line;
			last SWITCH;
		  }
		  if ( $line =~ /XML output -/ ){
			$line =~ s/^\s*XML output -\s*//g;
			$gRef_Test_params{"test_bin_xml_support"} = $line;
			last SWITCH;
		  }
		}
	  }
	}

}


#-------------------------------------------------------------------
# Open historical archives and validate their configuration 
# against current settings (or each other)
#-----------------------------------------------------------------------
sub ValidateArchives(){

  # First archive is old archive. 
  process_historical_archive($gTest_paths{"old_archive"});

  # if specified, second archive is new archive. 
  if (  $gTest_params{"compare_two_archives"} ) {
    process_historical_archive($gTest_paths{"new_archive"});
  }

  #-----------------------------------------------------------------------
  #   Resolve specified test parameters against those in the
  #   archive.
  #-----------------------------------------------------------------------
  if ( $gTest_params{"abbreviated_runs"} ne
       $gRef_Test_params{"abbreviated_runs"} ){
    stream_out( " Warning: Abbreviated simulations have been "
              .is_empty($gTest_params{"abbreviated_runs"})
              ." but the historical archive was\n");
    stream_out( "          created with abbreviated simulations "
              .is_empty($gRef_Test_params{"abbreviated_runs"})
              .". Abbreviated simulations will be "
              .is_empty($gRef_Test_params{"abbreviated_runs"}).".\n\n");
  
    $gTest_params{"abbreviated_runs"} = $gRef_Test_params{"abbreviated_runs"};
  }     
  if ( $gTest_params{"abbreviated_runs"} &&
       $gTest_params{"abbreviated_run_period"} ne
       $gRef_Test_params{"abbreviated_run_period"} ){

    stream_out( " Warning: An abbreviated simulation period of "
              ."$gTest_params{\"abbreviated_run_period\"} has been specified\n");
    stream_out( "          but the historical archive was created with a period of "
              ."$gRef_Test_params{\"abbreviated_run_period\"}\n");
    stream_out( "          A period of $gRef_Test_params{\"abbreviated_run_period\"}"
              ." will be used.\n\n");
  
    $gTest_params{"abbreviated_run_period"} = $gRef_Test_params{"abbreviated_run_period"};
  }
}

#-------------------------------------------------------------------
# Create folders to hold local copies of the models and the 
# results output 
#-------------------------------------------------------------------

sub PrepLocalFolders(){

  if ( ! $gTest_params{"compare_two_archives"} ){
  # Test that 'local_models' folder can be created.
    if ( ! -d $gTest_paths{"local_models"} ){
      execute("mkdir $gTest_paths{\"local_models\"}");
      if ( ! -d $gTest_paths{"local_models"} ){
        fatalerror("Local model folder could not be created ($gTest_paths{\"local_models\"})");
      }
    }elsif ( ! -w $gTest_paths{"local_models"} ){
      execute("chmod u+w $gTest_paths{\"local_models\"}");
      if ( ! -w $gTest_paths{"local_models"} ){
        fatalerror("Local model folder is not writable ($gTest_paths{\"local_models\"})");
      }
    }
    # Now delete it. We'll create it as we need to later on.
    unlink $gTest_paths{"local_models"};
  }


  # Scope varaibles needed for historical archive file names


  # if archive is to be created, check to see if tar file exists
  # and delete if necessary.
  if ( $gTest_params{"create_archive"} ){
    # Historical archive to be created.
    # strip.tar.gz extention, if present.
    $gTest_paths{"new_archive"} =~ s/\.tar\.gz$//g;

    # delete existing file, if it exists.
    if ( -r "$gTest_paths{\"new_archive\"}\.tar.gz" ){
      stream_out(" Warning: Archive $gTest_paths{\"new_archive\"}\.tar.gz already exists, \n");
      stream_out("          and will be deleted.\n\n");
      execute("rm -fr $gTest_paths{\"new_archive\"}\.tar.gz");
      if ( -r "$gTest_paths{\"new_archive\"}\.tar.gz" ){
        fatalerror("Archive $gTest_paths{\"new_archive\"}\.tar.gz already exists and could not be deleted!");
      }
    }

    $gTmp_Test_archive = $gTest_paths{"new_archive"};

  }else{
    # Historical archive is not to be created. Store results in same folder
    # as reference case.
    $gTmp_Test_archive = $gTest_paths{"results"};
  }


  # check to see that test archive directory doesn't exist,
  # and delete it if it does.
  if ( -r "$gTmp_Test_archive" ){
    execute("rm -fr $gTmp_Test_archive");
  }
  # Make test new archive folder
  execute("mkdir $gTmp_Test_archive");

  # Check that the folder was successfully created.
  if ( ! -r $gTmp_Test_archive ){
    fatalerror("Archive directory $gTmp_Test_archive could not be created!");
  }


  # If reference version of bps is to be tested,
  # assign reference results archive lcoation.
  if ( $gTest_params{"compare_versions"} ) {
    # Results archive for reference case
    $gTmp_Ref_archive = $gTest_paths{"results"};
    if ( $gTmp_Test_archive ne $gTmp_Ref_archive ){
      # check to see that reference archive directory doesn't exist,
      # and increment root folder if it does
      my $tmp_Ref_archive_root = $gTmp_Ref_archive;
      my $ii = 0;
      while ( -r "$gTmp_Ref_archive" ){
        $gTmp_Ref_archive = "$tmp_Ref_archive_root\_$ii";
        $ii++;
      }
    
      # Make reference archive folder
      $gTmp_Ref_archive = resolve_path($gTmp_Ref_archive);
      execute("mkdir $gTmp_Ref_archive");
    
      # Check that the folder was successfully created.
      if ( ! -r $gTmp_Ref_archive ){
        fatalerror("Archive directory $gTmp_Ref_archive could not be created!");
      }
    }
  }

  # If a historical archive is to be created, write
  # a configuration file in historical archive
  if ( $gTest_params{"create_archive"}){
    # Get archive root name
    $gTest_params{"archive_root_name"} = $gTmp_Test_archive;
    $gTest_params{"archive_root_name"} =~ s/\/$//g;              # strip any trailing slashes
    $gTest_params{"archive_root_name"} =~ s/^.*\/([^\/]+)$/$1/g; # get last folder name
  }


}



#-------------------------------------------------------------------
# Read the cfg file, build temporary files for relevant save-levels,
# and call invoke_tests() to exercise bps. This block of code 
# is the primary supervisory routine for:
#   -pre-processing test cases (including invoking shading analysis 
#    if needed)
#   -invoking reference and test versions of bps 
#   -handling output files 
#   -comparing results 
#
#-------------------------------------------------------------------
sub process_case($){

  # get starting path, and move to master path
  my $start_path = getcwd();
  chdir $gTest_paths{"master"};

  #-------------------------------------------------------------------
  # Prepare to run tests. 
  #   - Make local copy of model
  #   - impose custom database paths
  #   - identify zones requiring shading analysis 
  #-------------------------------------------------------------------
  
  
  # Make foldet to store local copy of the model
  execute("mkdir $gTest_paths{\"local_models\"}");
  
  # collect test case path. We assume that the test case lies in 
  # ESP-r's standard modelname/cfg/modelname.cfg path.
  my ($test_model) = @_;
  $test_model = resolve_path($test_model);

  # get model name
  my $model_name = get_model_name($test_model);

  # Get root model name
  my $model_root_name = get_model_root_name($test_model);

  # make local copy of model folder
  my $model_folder_path = get_model_folder($test_model);
  my $local_cfg_file = "$gTest_paths{\"local_models\"}/$model_root_name/cfg/$model_name.cfg";
  
  # Report on progress
  stream_out(" > TESTING: $model_name (in folder $model_root_name) \n");
  
  # Actually copy files. 
  execute("cp -fr $model_folder_path $gTest_paths{\"local_models\"}");

  # If user has specified local databases (e.g. /home/user/esp-r/databases...),
  # replace default database path (usr/esru) with specified path. 
  # Obviously this only works if test cases have standard /usr/esru  database 
  # paths. Anything else will break tester.
  
  if ( $gTest_params{"user_databases"} ){
    
    # Search through model directory and operate on ascii files. 
    find( sub{
        # move on to next file if (1) file is a directory,
        # (2) file is not readable, (3) or file is not ascii,

        return if -d;
        return unless -r;
        return unless -T;
        my $file = $File::Find::name;
        return if $file =~ m/CVS./;
        return if $file =~ m/\.svn/;
        # Open file and read contents 
        open (EDIT_FILE, $file) or fatalerror("Could not open $file for reading!");
        my @lines = ();

        while ( my $line = <EDIT_FILE> ) {
        
          # Replace standard dbs with user specified path
          
          $line =~ s/$gTest_paths{"default_dbs"}/$gTest_paths{"user_databases"}/g;
          push @lines, $line;

         
        }

        # Close file
        close(EDIT_FILE);

        # Reopen file with status 'new' and write out contents.
        open(WRITE_FILE, ">$file") or fatalerror("Could not open $file for writing!");
        foreach my $line ( @lines ){
          print WRITE_FILE $line;
        }
        close(WRITE_FILE);
       
      },  $gTest_paths{"local_models"} );

    # move back to master path
    chdir $gTest_paths{"master"};
      
  }
  # get model path, without .cfg extention
  my $test_root = $local_cfg_file;
  $test_root =~ s/\.cfg$//g;
  my @cfg_lines = "";

  my ($zone_number);
  my %zone_geo_files;
  my %zone_shading_status;
  my %zone_names;

  #-----------------------------------------------------------------------
  # read cfg file into @cfg_lines buffer,identify simulation presets, and rename
  # res libraries for consistency. Flag zones that require shading analysis.
  #-----------------------------------------------------------------------
  open(CFG_FILE, $local_cfg_file) or fatalerror("Could not open $local_cfg_file!");
  while ( my $line = <CFG_FILE> ) {
    # rename results libraries for consistancy
    $line =~ s/\*sblr[\s]*.*/*sblr $model_name.bres/g;
    $line =~ s/\*splr[\s]*.*/*splr $model_name.pres/g;
    $line =~ s/\*selr[\s]*.*/*selr $model_name.eres/g;
    $line =~ s/\*sipv[\s]*.*/*sipv $model_name.ires/g;
    push @cfg_lines, $line;


    #--------------------------------------------
    # Save zone geo file paths & shading tags
    # for use when regenerating shading files.
    #--------------------------------------------

    # If line describes zone #, parse number
    if ($line =~ /^\*zon\s+/ ){
      $zone_number = $line;
      $zone_number =~ s/^\s*\*zon\s+([0-9]+).*$/$1/g;
      $zone_number =~ s/\s*\n*//g;
      
      # initialize zone shading file flag to zero.
      $zone_shading_status{$zone_number} = 0;
      
    }
    
    # If line describes zone geometry record, save
    #   in zone geo buffer.

    if ( $line =~ /^\*geo\s+/ ){
      $zone_geo_files{$zone_number} = $line;
      $zone_geo_files{$zone_number} =~ s/^\*geo\s+([^\s]+).*$/$1/g;
    }

    # Check if line describes zone shading file, and set flag

    if ( $line =~ /\*isi\s+/ ){
      $zone_shading_status{$zone_number} = 1;
    }
    
  }
  close (CFG_FILE);


  #-----------------------------------------------------------------------
  # Loop through save levels, modify cfg files, and invoke test procedures
  #-----------------------------------------------------------------------
  foreach my $save_level (@gSave_levels){
    
    # Create test file
    open(TST_FILE, ">$test_root\_temp.cfg");
    foreach my $line (@cfg_lines){
       
      # Check to see if line describes save level, and adjust to reflect loop.
      my $line_copy = $line;
      if ($line =~/^\*sps/ ){
         # set save level: Substitute specified save level instead of one in file.
         $line_copy =~ s/(\*sps[\s]*[0-9]*[\s]*[0-9]*[\s]*[0-9]*[\s]*[0-9]*[\s]*)[0-9]*/$1 $save_level/g;
      }
	  # Impose abbriviated (one-day) simulaitons, if requested.
      if (  $line =~/^\s*[0-9]+\s+[0-9]+\s+[0-9]+\s+[0-9]+\s+$gTest_params{"period_name"}/ && $gTest_params{"abbreviated_runs"} ){
        $line_copy =~ s/^\s*[0-9]+\s+[0-9]+\s+[0-9]+\s+[0-9]+\s+$gTest_params{"period_name"}/$gTest_params{"abbreviated_run_period"} $gTest_params{"period_name"}/g;
      }
      print TST_FILE "$line_copy";
    }
    close(TST_FILE);

    # run bps in model"s cfg directory
    chdir "$gTest_paths{\"local_models\"}/$model_root_name/cfg/";

    # disable h3kreports for save-level-4.
    if ( $save_level =~ /4/ ){
      execute ( "mv input.xml input_bak.xml" );
    }
    # Run tests!
    invoke_tests($model_name,
                 $model_root_name,
                 "$model_name\_temp.cfg",
                 $save_level,
                 \%zone_shading_status);
    if ( $save_level =~ /4/ ){
      execute ( "mv input_bak.xml input.xml" );
    }

    chdir "$gTest_paths{\"master\"}";
    
  }
   
  # empty local folder
  unlink $gTest_paths{"local_models"};
  
  # move to master path
  chdir $gTest_paths{"master"};
  
  # compare results.
  if ( $gTest_params{"compare_versions"} || $gTest_params{"compare_to_archive"} ){
    compare_results($model_name,$model_root_name);
  }

  
  # delete archived results unless archive is requested.
  if ( ! $gTest_params{"save_output"} ){
     unlink "$gTest_paths{\"results\"}";
  }
  # Return to starting path
  chdir $start_path;
}



#-------------------------------------------------------------------
# Create a report describing the test results. (bps_test_report.txt)
#-------------------------------------------------------------------
sub create_report(){
  
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
  
  push @output, "  - Test suite path:        <>$gTest_paths{\"test_suite\"}";

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
  push @output, sprintf (" %\-".$folder_length."s<>  %-".$model_length."s<> .summary <> .xml<> .csv <> overall<> dt-CPU(%%)", "Folder", "Model");
  push @output, $current_rule;

  # Loop throug results, and report to buffer
  foreach my $test ( sort keys %gTest_Results ){
    my ($folder,$model) = split /\//, $test;
    my $xml_pass      = result_to_string($test,"xml");
	my $summary_pass  = result_to_string($test,"summary");
    my $csv_pass      = result_to_string($test,"csv");
    my $h3k_pass      = result_to_string($test,"h3k");
    my $data_pass     = result_to_string($test,"data");
    my $overall_pass  = result_to_string($test,"overall");
    my $cpu_change    = $gRun_Times{"$folder/$model"}{"chg"};
    # add extra space to align +ive and -ive CPU runtime changes
    my $spacer = "";
    if ( $gTest_params{"test_efficiency"} && $cpu_change !~ /N\/A/){
      if ( $cpu_change > 0 ){$spacer=" ";}
      $cpu_change = sprintf($spacer."%-10.2g", $cpu_change);
    }else{
      $cpu_change = "N/A";
    }
    
    push @output, sprintf (" %\-".$folder_length."s<>  %-".$model_length."s<>     %1s   <>    %1s  <>  %1s   <>   %1s    <> ".$spacer."%-10s  ", $folder, $model,  $summary_pass,$xml_pass, $csv_pass, $overall_pass, $cpu_change);
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
  push @output, " =========== Comparison of Numerical results ================= ";
  # Detailed report of XML output comparison
  if ( ( $gTest_ext{"xml"} || $gTest_ext{"summary"} ) 
       && ! $gNum_Report_needed ){
    push @output, " ";
    push @output, " No differences were found in numerical output. Detailed report unnecessary. ";
    push @output, " ";
  }elsif ( $gTest_ext{"xml"} || $gTest_ext{"summary"}  ){

    my $current_folder = "";
    my $current_model  = "";

    
    push @output, " Numerical output: Detailed report ";
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
   
    foreach my $failure ( sort keys %gNum_Failures ){
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

      $current_line  = sprintf (" %\-80s<>%\-20s[]", "$element_path ($attribute)", $gNum_Failures{$failure}{"units"});

      $current_line .= sprintf ("%15s%5s<>%15s%5s<>%15s%5s<>%15s%5s[]",
                                format_my_number(
                                        $gNum_Failures{$failure}{"difference"}{"relative"},15,"%10.5g"),
                                "",
                                format_my_number(
                                        $gNum_Failures{$failure}{"difference"}{"absolute"},15,"%10.5g"),
                                "",
                                format_my_number(
                                        $gNum_Failures{$failure}{"difference"}{"reference_value"},15,"%10.5g"),
                                "",
                                format_my_number(
                                        $gNum_Failures{$failure}{"difference"}{"test_value"},15,"%10.5g"),
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

#-------------------------------------------------------------------
# Delete local copies of models, simulation output (unless 
# --save_results is specified)
#-------------------------------------------------------------------
sub DelTempFiles(){
  # delete folders containing outputs, unless --save_results option invoked.
  if ( ! $gTest_params{"save_output"} ){
    execute("rm -fr $gTmp_Test_archive");
    if ( $gTmp_Ref_archive ){
      execute("rm -fr $gTmp_Ref_archive");
    }
  }

  # If comparison was made with old archive, delete uncompressed
  # archive 

  if ( $gTest_params{"compare_to_archive"} && $gTest_paths{"old_archive"} ) {
    my $uncompressed_archive_root = $gTest_paths{"old_archive"};
    $uncompressed_archive_root =~ s/\.tar\.gz$//g;
    execute ("rm -fr $gTest_params{\"configuration_file\"} $uncompressed_archive_root");
    
  }

  # delete local files folder
  execute ("rm -fr $gTest_paths{\"local_models\"}");
}


#-------------------------------------------------------------------
# Process an archive:
#   1. Uncompress it
#   2. Parse the test configuration file
#   3. Check that output files exist.
#   4. Store output file locations in reference hash.
#-------------------------------------------------------------------
sub process_historical_archive($){

  my ($archive_file) = @_;


  my %archive_Sys_params;
  my %archive_Test_params;


  # Check that historical archive exists.
  if ( ! -r $archive_file ) {
    fatalerror("Historical archive ($archive_file) could not be found!");
  }

  my $version = ( $gProcessed_Archive_count == 0 ) ? "reference" : "test" ;
  $gProcessed_Archive_count++;


  stream_out(" > Exploding tarball $archive_file...");


  # Check if historical archive is a tar-gz file.
  if ( $archive_file !~ /\.tar\.gz$/ ){

    # make backup copy

    $gTest_paths{"$version\_archive_backup"} = "$archive_file\_bak";
    execute("cp -fr $archive_file $gTest_paths{\"$version\_archive_backup\"}");

    if ( ! -r $gTest_paths{"$version\_archive_backup"} ){
      fatalerror("\nA backup copy of $archive_file ".
                 "could not be made at $gTest_paths{\"$version\_archive_backup\"}");
    }

    # file does not end in .tar.gz. Attempt to rename and uncompress
    stream_out("\n Warning: $archive_file does not appear ".
               "to be a valid archive.\n\n");
    stream_out("          Attempting to uncompress anyway.");
    execute("mv $archive_file $archive_file.tar.gz");

    if ( ! -r "$archive_file.tar.gz" ){
      # file could not be renamed
      fatalerror("\n$archive_file could not be moved to ".
                 "$archive_file.tar.gz!");

    }else{
      # file renamed successfully.
      $archive_file .= ".tar.gz";
    }
  }

  
  
  # Get historical archive folder and root name
  $gTest_paths{"$version\_archive_folder"} = $archive_file;
  $gTest_paths{"$version\_archive_folder"} =~ s/^(.+\/)[^\/]*$/$1/g;
  
  $gTest_paths{"$version\_archive_file"} = $archive_file;
  $gTest_paths{"$version\_archive_file"} =~ s/$gTest_paths{"$version\_archive_folder"}//g;

  # Move to achive directory
  chdir $gTest_paths{"$version\_archive_folder"};
  
  # Decompress and explode tarball.
  my $decompression_failure = `$gSys_params{"unzip_command"} $gTest_paths{"$version\_archive_file"} | $gSys_params{"untar_command"} -`;
  if ( $decompression_failure ){
    print "\n>>>>>> $decompression_failure ";
    fatalerror("\nCould not decompress $archive_file");
  }

  stream_out("done.\n");

  # Open test configuration file
  open (REF_TEST_CONFIG, "$gTest_params{\"configuration_file\"}");
  
  # Set parsing flags
  my $header_ok = 0;
  my $version_ok = 0;
  my $root_folder_ok = 0;
  my $error_msgs = "";
  my $line_number = 0;
  # parse file:
  while ( my $line = <REF_TEST_CONFIG> ){
    $line_number ++;
    # strip comments (beginning with a "#")
    $line =~ s/\#.*$//g;
 
    # parse line if there's anything left.
    if ( $line ){

      # lines are space-separated
      my @values = split / /, $line;

      # Initialize uninitialized array locations.
      for (my $ii = 0; $ii < 4; $ii++){
        if ( ! $values[$ii] ){
          $values[$ii]="";
        }
      }
       
      # Check for header
      if ( "$values[0] $values[1] $values[2]" =~ /\*tester.pl configuration file/ ) {
        # file is valid.
        $header_ok = 1;
      }

      # Check for version
      if ( $values[0] eq "*version" ){
        if ( $values[1] <= $gTest_params{"max_version_#"} ||
             $values[1] >= $gTest_params{"min_version_#"}    ){
          # version number is valid
          $version_ok = 1;

          # check if various features are supported by current version
          if ( $values[1] < $gTest_params{"test_eff_arch_version"} ){
            # version predates efficiency testing. Disable.
            $gTest_params{"test_efficiency"} = 0;
          }
          
        }else{
          $error_msgs .= "     - Configuration file version number ($values[2])".
                         " is not supported. (line: $line_number) \n";
        }
      }

      # Parse system parameters
      if ( $values[0] eq "*sys_param" ){
        if ( $values[1] && $values[2] ){
          $archive_Sys_params{$values[1]} = $values[2];
        }elsif ( $values[1] ) {
          $archive_Sys_params{$values[1]} = "unknown";
        }else{
          $error_msgs .= "      - System parameter ($values[1]=$values[2])".
                         " not understood (line: $line_number)\n";
        }
      }

      # Parse test parameters
      if ( $values[0] eq "*test_param" ){
        if ( $values[1] && $values[2] ){
          $archive_Test_params{$values[1]} = is_true_false($values[2]);
        }else{
          $error_msgs .= "      - Test parameter ($values[1]=$values[2])".
                         " not understood (line: $line_number)\n";
        }
      }

      # Parse test extentions
      if ( $values[0] eq "*test_ext" ){
        if ( $values[1] && $values[2] ){
          # If user has specified that an extention has been tested,
          # ensure that a reference file exists.
          if ( $gTest_ext{$values[1]} && ! is_true_false($values[2]) ){
            stream_out( " Warning: testing of $values[1] specified, but historical archive "
                       ."does not contain $values[1] files.  \n");
            stream_out( "          Testing of $values[1] files supressed\n\n");
            $gTest_ext{$values[1]} = is_true_false($values[2]);
          }
        }else{
          $error_msgs .= "      - Test extention ($values[1]=$values[2])".
                         " not understood (line: $line_number)\n";
        }
      }
      
      # Parse results files.
      if ( $values[0] eq "*output" ){
      
        if ( $values[1] && $values[2] && $values[3] ){

          my $file_list = "";

          if ( defined ( $gTestable_files{$values[1]}{$version} ) ){
            $file_list = ";"
          }
          $values[3] = resolve_path("$gTest_paths{\"$version\_archive_folder\"}/$values[3]");
          $file_list .= "$values[2]:$values[3]";
          $gTestable_files{$values[1]}{"$version"} .= $file_list;

        }else{
          $error_msgs .= "      - Results file model/extention/path".
                         " ($values[1]/$values[2]$values[3])".
                         " not understood (line: $line_number)\n";

        }

      }



      # Parse recorded runtimes.
      if ( $values[0] eq "*runtime" ){
        if ( $values[1] && $values[2] && $values[3] ){
          # arg 1: model path (folder/model name)
          # arg 2: save level
          # arg 3: runtime
          $gRun_Times{$values[1]}{$values[2]}{"$version"} = $values[3];

        }else{
          # Runtime 
          $error_msgs .= "      - Results file model/save-level/runtime".
                         " ($values[1]/$values[2]$values[3])".
                         " not understood (line: $line_number)\n";
        }
      }
    }
  }
  close(REF_TEST_CONFIG);

  # Clean any ~'s, which are used to denote spaces within a string
  foreach my $parameter (keys %gRef_Test_params ){
     $archive_Test_params{$parameter} =~ s/~/ /g;
  }
  foreach my $parameter (keys %gRef_Sys_params ){
     $archive_Sys_params{$parameter} =~ s/~/ /g;
  }

  # Now copy archive parameters into ref/test commons, depending on
  # version.

  if ( $version =~ /reference/ ){

    while (my ($param, $val) = each ( %archive_Test_params ) ){
      $gRef_Test_params{$param} = $val;
    }
    while (my ($param, $val) = each ( %archive_Sys_params ) ){
      $gRef_Sys_params{$param} = $val;
    }

  }else{

    while (my ($param, $val) = each ( %archive_Test_params ) ){
      $gTest_params{$param} = $val;
    }
    while (my ($param, $val) = each ( %archive_Sys_params ) ){
      $gSys_params{$param} = $val;
    }

  }

  if ( $error_msgs ){
    print $error_msgs;
    fatalerror("Could not parse $archive_file");
  }
  
}

#--------------------------------------------------------------------
# Create a new archive, tar & gzip files.
#--------------------------------------------------------------------
sub create_historical_archive(){

  # open new test configuration file
  open(TEST_CONFIG, ">$gTmp_Test_archive/../$gTest_params{\"configuration_file\"}")
    or fatalerror("Could not open ".resolve_path("$gTest_params{\"archive_root_name\"}")."!");

  # Write basic info to configuration file
  print TEST_CONFIG "#--------------------------------------------\n";
  print TEST_CONFIG "# This file describes a set of tests performed \n";
  print TEST_CONFIG "# using the test.pl script\n";
  print TEST_CONFIG "#--------------------------------------------\n";
  print TEST_CONFIG "*tester.pl configuration file\n";
  print TEST_CONFIG "*version $gTest_params{\"default_version_#\"}\n";

  # System parameters
  print TEST_CONFIG "#--------------------------------------------\n";
  print TEST_CONFIG "# System parameters: \n";
  print TEST_CONFIG "#--------------------------------------------\n";

  while ( my ( $param, $value ) = each %gSys_params ){
    $value =~ s/\s/~/g;
    $value =~ s/\n//g;
    print TEST_CONFIG "*sys_param $param $value \n";
  }

  # test parameters
  print TEST_CONFIG "#--------------------------------------------\n";
  print TEST_CONFIG "# Test parameters: \n";
  print TEST_CONFIG "#--------------------------------------------\n";
  foreach  my $param ( @gDumped_parameters ){
    my $value = is_empty($gTest_params{$param});
    $value =~ s/\n//g;
    $value =~ s/ /~/g;
    print TEST_CONFIG "*test_param $param $value \n";
  }

  print TEST_CONFIG "#--------------------------------------------\n";
  print TEST_CONFIG "# Tested extentions: \n";
  print TEST_CONFIG "#--------------------------------------------\n";
  foreach  my $ext ( keys %gTest_ext ){
    my $value = is_empty($gTest_ext{$ext});
    $value =~ s/\n//g;
    $value =~ s/ /~/g;
    print TEST_CONFIG "*test_ext $ext $value \n";
  }
    
  print TEST_CONFIG "#--------------------------------------------\n";
  print TEST_CONFIG "# Paths to results files:    \n";
  print TEST_CONFIG "#--------------------------------------------\n";
  # Append index of testable files to test configuration file
  while ( my ( $model, $outputs ) = each %gTestable_files ){
    while ( my ( $version, $filelist ) = each %$outputs ){
      if ( $version =~ /test/ ){
        my @files = split /;/, $filelist;
        foreach my $file ( @files ){
          my ( $extention, $path ) = split /:/, $file;
          $path =~ s/$gTmp_Test_archive//g;
          $path = "$gTest_params{\"archive_root_name\"}/$path";
          $path =~ s/\/\//\//g;
          print TEST_CONFIG "*output $model $extention $path \n";
        }
        # Add runtimes
        foreach my $level (@gSave_levels){
          print TEST_CONFIG "*runtime $model $level $gRun_Times{$model}{$level}{$version} \n";
        }
      }
    }
  }
  # Append measured CPU runtimes to test configuration file 
  while ( my ( $model, $outputs ) = each %gTestable_files ){


  }

  
  close (TEST_CONFIG);
  
  # Tar and zip historical archive file. First, move to parent directory,
  # and collect relative name for archive (this is necessary because
  # tar behaves differently depending if the supplied path is relative
  # or absolute).

  my $parent_dir = $gTmp_Test_archive;
  my $archive_file_name = $gTmp_Test_archive;

  $archive_file_name =~ s/^.*\///g;
  $parent_dir =~ s/$archive_file_name//g;
  if ( ! $parent_dir ) { $parent_dir = $gTest_paths{"master"}; }

  # move to parent folder and tar archive
  chdir $parent_dir;
  execute("$gSys_params{\"tar_command\"} $archive_file_name.tar $archive_file_name $gTest_params{\"configuration_file\"}");

  # delete configuration text file
  execute("rm -fr $gTest_params{\"configuration_file\"}");

  # move back to master path
  chdir $gTest_paths{"master"};

  # Compress archive
  execute("$gSys_params{\"zip_command\"} $gTmp_Test_archive\.tar");

  # move archive to desired name
  execute("mv $gTmp_Test_archive\.tar.gz $gTest_paths{\"new_archive\"}.tar.gz");

}

#-------------------------------------------------------------------
# Run bps on a specified model from the current directory, 
# move results to archive folder, and post-process as required.
#-------------------------------------------------------------------

sub invoke_tests($$$$$){

  my ($model,$folder,$test_case,$save_level,$zone_shading_status) = @_;

  my $model_path  = getcwd();

  my ( $time_start, $time_end, $cmd, $sim_msgs, $system_time, $user_time, $real_time, $run_time);

  my @msg_buffer;

  my %bps_versions  = ("test" => $gTest_params{'test_binary'} );
  my %bps_locations = ("test" => $gTest_paths{'test_loc'} );

  if ( $gTest_params{"compare_versions"} ){
    $bps_versions{"reference"}  = $gRef_Test_params{'test_binary'} ;
    $bps_locations{"reference"} = $gTest_paths{'ref_loc'} ;
  }

  foreach my $version (sort keys %bps_versions){
    # delete unnecessary files - may be lying around of previous test cycle
    # was interrupted
    delete_old_files();

    # Loop through zone shading status flag, and regenerate
    # shading for any 'shaded zones' using ish. 
    while ( my($zone,$regen) = each ( %$zone_shading_status ) ){
      if ( $regen ) {
        stream_out("   Regenerating shading files for zone $zone using $bps_locations{\"$version\"}/ish ...");
        $time_start = (times)[2];   # Sim start time 
        $cmd = "$bps_locations{\"$version\"}/ish -mode text -file $test_case -zone $zone -act update_silent";
        execute($cmd);
        $time_end = (times)[2];   # Sim end time 
        $user_time = $time_end-$time_start; # lapsed simulaiton time
        stream_out("   Done. ($user_time seconds on CPU)\n");
      }
    }

    $cmd = "$bps_versions{\"$version\"} -file $test_case -mode text -p $gTest_params{\"period_name\"} silent";

    # Now run call-grind if requested.
    if ( $gTest_params{'run_callgrind'} && $save_level =~ /5/ ){

      $time_start = (times)[2];
      stream_out("   performing callgrind analysis on $bps_versions{\"$version\"} ($version, save level $save_level) ...");
      execute("rm -fr callgind.out.*");

      # Invoke callgrind analysis on bps
      execute("valgrind --tool=callgrind $cmd ");

      # Use callgrind_annotate to turn raw callgrind output into meaningful
      # results
      system("callgrind_annotate --threshold=100 --inclusive=yes --tree=both callgrind.out.* > out.callgrind 2>&1 ");
      
      # move simulation results. 
      move_simulation_results($model_path,$model,$folder,$save_level,"$version");
      $time_end = (times)[2];

    }else{
   
    # run new bin

      stream_out("   running: $bps_versions{\"$version\"} ($version, save level $save_level) ...");

      $time_start = (times)[2];

      execute($cmd);

      # archive output
      move_simulation_results($model_path,$model,$folder,$save_level,"$version");
      
      $time_end = (times)[2];
  
    }
  
    # Save lapsed run-time data
    $user_time = $time_end-$time_start;
    $gRun_Times{"$folder/$model"}{$save_level}{"$version"} = $user_time;

    # Prep message for reporting to screen.
    $run_time = "($user_time seconds on CPU)";
    stream_out("done. $run_time\n");
  }
}

#--------------------------------------------------------------------
# Move simulation results to results-archive folder, and
# save file location in  handy hash
#--------------------------------------------------------------------
sub move_simulation_results($$$$$){

  my($path,$model,$folder,$save_level,$version) = @_;
  my $start_path  = getcwd();
  my (@copied_files,$Destination);

  # Check if this is the test or reference case, and
  # set desitnation
  if ( $version =~ /test/ ){
    $Destination = $gTmp_Test_archive;
  }else{
    $Destination = $gTmp_Ref_archive;
  }

  # If necessary, create folders for model in results
  # folder.
  add_case_folders_to_archive($Destination,$model,$folder);

  # For save-level 4, process binary results libraries, and copy
  # ascii output to destination.
  if ($save_level == 4 ){
  
    # post-process building library results file (*.bres),
    # if i) it exists and ii) ANALYSE script has been found

    if  ( -r  "$path/$model.bres"  && $gTest_params{"analyse_found"} ){

      # ANALYSE script must be run from model"s cfg directory
      chdir $path;

      # possibly use specified res binary to process esru results
      if ( $gTest_params{"ref_res_found"} && $version =~ /reference/ ){
        # use res corresponding to reference bps 
        execute("$gTest_paths{\"analyse_location\"} $model.bres $gTest_paths{\"ref_loc\"}");

      }elsif ($gTest_params{"test_res_found"} && $version =~ /test/){
        # Use res corresponding to test bps.
        execute("$gTest_paths{\"analyse_location\"} $model.bres $gTest_paths{\"test_loc\"}");

      }else{
        # use default res
        execute("$gTest_paths{\"analyse_location\"} $model.bres $gTest_paths{\"esp-r\"}");
      }
      
      # rename ANALYSE's output
      execute("mv $model.bres.data $model.data");

      # Move files to destination folder
      execute ("cp -f $path/$model.data $Destination/$folder/$model/SL4\_$version.data");
      execute ("cp -f $path/out.callgrind $Destination/$folder/$model/SL4\_$version.callgrind");
      # Get names of files that were actually copied.
      @copied_files = split (/\s/, `ls $Destination/$folder/$model/*SL4\_$version.data`);
    }

  }elsif ( $save_level == 5 ){
    # copy files to archive folder.
    execute ("cp -f $path/out.callgrind $Destination/$folder/$model/SL5\_$version.callgrind");
    execute ("cp -f $path/out.csv $Destination/$folder/$model/SL5\_$version.csv");
    execute ("cp -f $path/out.xml $Destination/$folder/$model/SL5\_$version.xml");
	execute ("cp -f $path/out.summary $Destination/$folder/$model/SL5\_$version.summary");
    execute ("cp -f $path/*.h3k   $Destination/$folder/$model/SL5\_$version.h3k");
    execute ("cp -f $path/callgrind.out* $Destination/$folder/$model/SL5\_$version.raw-callgrind-output");
    execute ("cp -f $path/*.fcts1 $Destination/$folder/$model/SL5\_$version.fcts1");
    execute ("cp -f $path/*.fcts2 $Destination/$folder/$model/SL5\_$version.fcts2");
    execute ("cp -f $path/*.fcts3 $Destination/$folder/$model/SL5\_$version.fcts3");
    execute ("cp -f $path/*.fcts4 $Destination/$folder/$model/SL5\_$version.fcts4");
    # Get names of files that were actually copied.
    @copied_files = split (/\s/, `ls $Destination/$folder/$model/*SL5\_$version.*`);
  }

  # Get filenames and extentions, and make semi-colon separated list:
  #
  #    extention_1:file_1;extention_2:file_2;extention_3:file_3...
  #
  my $file_list = "";
  foreach my $file (@copied_files){
    if ( $file ){
      my $extention = $file;
      $extention =~ s/^.*\.([^\.]*)$/$1/g;
      if ( $file_list ) { $file_list .= ";";}
      $file_list .="$extention:$file";
    }
  }
  
  # Store list in hash.
  if ( $file_list ){
    if ( defined( $gTestable_files{"$folder/$model"}{$version} ) ){
      $gTestable_files{"$folder/$model"}{$version} .= ";";
    }
    $gTestable_files{"$folder/$model"}{$version} .= $file_list;
  }
  
  # delete all remaining results files.
  execute("rm -fr $path/.callgrind $path/*.bres $path/*.pres $path/*.eres $path/*.ires $path/*.h3k $path/out.xml $path/out.summary $path/out.csv $path/*.fcts* $path/*.data");
  
  # move back to starting path.
  chdir $start_path;
  
} 


#-------------------------------------------------------------------
# Create a folder within the archive directory to hold a model"s
# results.
#-------------------------------------------------------------------
sub add_case_folders_to_archive($$$){
  my ($destination,$model,$folder) = @_;

  if ( ! -r "$destination/$folder" )       {execute("mkdir $destination/$folder");}
  if ( ! -r "$destination/$folder/$model" ){execute("mkdir $destination/$folder/$model");}
}

#-------------------------------------------------------------------
# Compare reference and test results for a model
#-------------------------------------------------------------------
sub compare_results($$){

  my ($model, $folder) = @_;
  
  # Do reference files exist?
  if ( ! defined $gTestable_files{"$folder/$model"}{"reference"} ){
    $gTest_Results{"$folder/$model"}{"overall"} = "unknown";
    $gRun_Times{"$folder/$model"}{"chg"} = "N/A";

  # Do test files exist?
  }elsif ( ! defined $gTestable_files{"$folder/$model"}{"test"} ){

    if ( $gTest_params{"compare_to_archive"} ||
         $gTest_params{"compare_two_archives"}  ){
      $gTest_Results{"$folder/$model"}{"overall"} = "unknown";
    }else{
      $gTest_Results{"$folder/$model"}{"overall"} = "fail";
    }
    $gRun_Times{"$folder/$model"}{"chg"} = "N/A";

  }else{
    # Collect list of testable files for this model
    
    my @reference_file_list = split /;/, $gTestable_files{"$folder/$model"}{"reference"};
    my @test_file_list      = split /;/, $gTestable_files{"$folder/$model"}{"test"};
  
    # Scope reference and test file path buffers
    my ( %reference_files, %test_files );
  
    # split file lists into more managable hashes
    foreach my $file (@reference_file_list){
      my ($extention, $path) = split /:/, $file;
      $reference_files{$extention} = $path;
    }
    foreach my $file (@test_file_list){
      my ($extention, $path) = split /:/, $file;
      $test_files{$extention} = $path;
    }
  
    # Set master flag for model
    $gTest_Results{"$folder/$model"}{"overall"} = "pass";
    
    # Now loop though extentions in reference file list, and compare with
    # similar files in test list.
  
    # Optionally report results to buffer.
    stream_out ("   Comparing results:\n");
  
    # Loop through extensions and perfom file-to-file comparisons
    foreach my $extention ( sort ( keys %reference_files ) ){
  
      if ( $gTest_ext{$extention} ){
    
        stream_out (sprintf ("    - %-20s ","$extention files: ") );
      
        # get reference and test files
        my $reference_file = $reference_files{$extention};
        my $test_file      = $test_files{$extention};

        if (   $reference_file && ! $test_file ||
              !$reference_file &&   $test_file  ){

           $gTest_Results{"$folder/$model"}{$extention} = "fail";
           $gTest_Results{"$folder/$model"}{"overall"} = "fail";
           $gRun_Times{"$folder/$model"}{"chg"} = "N/A";
           
        }elsif ( $extention =~ /xml/ ||  $extention =~ /summary/ ){
          
		    

          # First, check if files differ.
          if ( ! compare( $reference_file, $test_file ) ){
            # files are identical - there's no need to perform
            # xml-comparison.
            $gTest_Results{"$folder/$model"}{$extention} = "pass";
          }else{
		  
		   		  
		    # Files differ - delve into contents:
			
			my %results = (); 
			
			if ( $extention =~ /xml/ ) {
		  
				# Handle XML first
		  
				# Create new objects to hold document tree
				$gRefXML  = new XML::Simple();
				$gTestXML = new XML::Simple();
		  
				# Parse Document trees
				$gRefXML  = XML::Simple::XMLin($reference_file);
				$gTestXML = XML::Simple::XMLin($test_file);
		  
				# Compare reference and test xml: This function will parse the contents
				# of $gRefXML and $gTestXML, and return a hash containing comparable
				# values.
				%results = CollectXMLResults();
		  
			}else{
			
				# Analysis of summary.out: Provides simpler treatment?
				%results = CollectSummaryResults($reference_file, $test_file); 
			
			}
			
			# This function will compare the values in the % results hash.
			my $case_failed = CompareNumericalResults($folder,$model,\%results);
		  
            if ( $case_failed ){
              $gNum_Report_needed = 1;
              $gTest_Results{"$folder/$model"}{$extention} = "fail";
              $gTest_Results{"$folder/$model"}{"overall"} = "fail";
            }else{
              $gTest_Results{"$folder/$model"}{$extention} = "pass";
            }
            
            # Delete unnecessasry objects.
            $gRefXML = "";
            $gTestXML = "";
              
          }

        }elsif ( $extention =~/data/ ){
          
          # Diff files. compare returns 1 if files differ.
          if ( compare( $reference_file, $test_file ) ){
            # Files differ
            $gTest_Results{"$folder/$model"}{$extention} = "fail";
            $gTest_Results{"$folder/$model"}{"overall"}  = "fail";
            
            # If 'diff data' has been specified, invoke a semi-interactive
            # diff. We'll use the raw 'system' command instead of our
            # custom 'execute' wrapper, as we don't want diff's output to 
            # be filtered according to verbosity.
            
            if ( $gTest_params{"diff_data_files"} ){
              system ("$gTest_params{\"third_party_diff_cmd\"} $reference_file $test_file");
            }
            
          }else{
            # Files are the same
            $gTest_Results{"$folder/$model"}{$extention} = "pass";
          }

        }elsif ( $extention =~/callgrind/ ){

          %gCallGrindResults = CollectCallGrindResults("$folder/$model",$reference_file,$test_file);

          if ( $gCallGrindResults{"$folder/$model"}{"deltas"}{"all procedures"}{"percent"} > 0 ){
            $gTest_Results{"$folder/$model"}{$extention} = "slowdown by ";
          }else{
            $gTest_Results{"$folder/$model"}{$extention} = "speed-up by ";
          }
          $gTest_Results{"$folder/$model"}{$extention} .= sprintf("%-.1f %%",$gCallGrindResults{"$folder/$model"}{"deltas"}{"all procedures"}{"percent"});
          
        }elsif ( $extention =~/XXX/ ){
          # Add new extention-specific analsis here
          
        }else{
          # Diff files. compare returns 1 if files differ.
          if ( compare( $reference_file, $test_file ) ){
            # Files differ
            $gTest_Results{"$folder/$model"}{$extention} = "fail";
            $gTest_Results{"$folder/$model"}{"overall"}  = "fail";
          }else{
            # Files are the same
            $gTest_Results{"$folder/$model"}{$extention} = "pass";
          }
        }

        # If comparison is to historical archive, copy reference files
        if ( $gTest_params{"compare_to_archive"} ) {
          my $copied_reference_file = $reference_file;
          $copied_reference_file =~ s/test/reference/g;
          $copied_reference_file =~ s/^.*\///g;
          execute("cp $reference_file $gTmp_Test_archive/$folder/$model/$copied_reference_file");
        }
        # Report test result to buffer
        stream_out ($gTest_Results{"$folder/$model"}{$extention}."\n");
      }
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
    
  }

  # update global pass flag upon failure.
  if ( $gTest_Results{"$folder/$model"}{"overall"} =~ /fail/ ){
    $gAll_tests_pass = 0;
  }

  # Report overall test result to buffer
  stream_out (
                sprintf ("    - %-20s %s \n","Overall:",
                          $gTest_Results{"$folder/$model"}
                          {"overall"})
                             
  );
  
}

#-------------------------------------------------------------------
# Parse summary.out files and store in common hash. 
#-------------------------------------------------------------------
sub CollectSummaryResults($$){

	my %SummaryResults; 

	my ($reference_file, $test_file) = @_; 
	
	open (INPUT, $reference_file ) or fatalerror("\n\nCould not open $reference_file!\n"); 
	
	while ( my $line = <INPUT> ){
		
		my ($path,$remainder) = split /::/, $line; 
	    my ($attribute,$value,$units) = split / /, $remainder; 
		# Units will contain a trailing space and '(', ')'. delete these. 
		$units =~ s/\s//g; 
		$units =~ s/(\(|\))//g; 
		
		
		# Add values to hash. 
		
		if ( ! defined ($SummaryResults{$path}{"units"} ) )
		{
			$SummaryResults{$path}{"units"}  = $units; 
		}
		$SummaryResults{$path}{$attribute} = "$value/"; 
		
				
	}
	
	close (INPUT); 

	
	open (INPUT, $test_file ) or fatalerror("\n\nCould not open $test_file!\n"); 
		
	while ( my $line = <INPUT> ){	

		my ($path,$remainder) = split /::/, $line; 
	    my ($attribute,$value,$units) = split / /, $remainder; 
		# Units will contain a trailing space and '(', ')'. delete these. 
		$units =~ s/\s//g; 
		$units =~ s/(\(|\))//g; 
				
		# Add values to hash. 
		
		if ( ! defined ($SummaryResults{$path}{"units"} ) )
		{
			$SummaryResults{$path}{"units"}  = $units; 
		}
				
				
		my $slash_value = defined ( $SummaryResults{$path}{$attribute} ) ? $SummaryResults{$path}{$attribute}."$value" : "/$value"; 
		
		$SummaryResults{$path}{$attribute} = $slash_value; 
		
	
	}
	close (INPUT) ; 
 
	return %SummaryResults; 
 
	# Debugging code. 
	#
	# foreach my $path ( sort keys %SummaryResults ){
	# 	
	# 	stream_out (">$path\n");
	# 	
	# 	my %attributes = %{ $SummaryResults{$path} }; 
	# 	
	# 	foreach my $attribute ( sort keys  %attributes ){
	# 	
	# 		stream_out (" -> $attribute = ".$SummaryResults{$path}{$attribute}."\n") ; 
	# 	
	# 	}
	# 
	# }
	
}

#-------------------------------------------------------------------
# Traverse XML trees for reference and test document, and store
# values in handy hash.
#-------------------------------------------------------------------
sub CollectXMLResults(){

  my (%XMLresults);
  
  #Empty @gCurrent_path and %gElements stacks
  @gCurrent_path = ();
  %gElements = ();

  # call function to parse $gRefXML object.
  parse_hash_recursively();

  # Empty unneeded array:
  @gCurrent_path = ();

  # Now loop through elements, and compare values between
  # reference and test arrays

  foreach my $element_path ( keys %gElements ){
    if ( ! eval ("defined(\$gTestXML$element_path)") ){
      # Element path does not exist in test hash. There"s
      # nothing to compare to!

    }elsif ( ! eval ("defined(\$gRefXML$element_path\-\>{type})" ) ){
      # Element does not contain a type attribute. It is
      # not appropraite for comparison.

    }elsif ( eval ("\$gRefXML$element_path\-\>{type}") =~ /max/ ){
      # element is a meaningless tag appended to xml output for
      # hot3000 purposes. Ignore it.
        
    }else{
      
      # Compare values of element:
      
      # Get the "type" of the array (monthly/annual), by
      # checking lowest-level hash"s type attribute
      
      my $element_type = eval("\$gRefXML$element_path\-\>{\"type\"}");

      # Check if this is a "WattsToGJ" tag, and set units to GJ if so.
      # Otherwise, read units.

      my $element_units;
      if ( $element_path =~ /WattsToGJData/ ){
        $element_units = "GJ";
      }elsif ( ! eval ( "defined ( \$gRefXML$element_path\-\>{\"units\"} )" ) ){
        $element_units = "-";
      }else{
        $element_units = eval("\$gRefXML$element_path\-\>{\"units\"}");
        $element_units =~ s/^\(//g;
        $element_units =~ s/\)$//g;
      }
        
      # if it"s a monthly bin, determine which month (0->11)
      # by checking the "index" attribute.
      if ( $element_type =~ /monthly/ &&
           (    defined( eval ("\$gRefXML$element_path\-\>{\"index\"}") )
             || defined( eval ("\$gRefXML$element_path\-\>{\"number\"}") )
           )
         ){
         
        my $element_month_index;
        # Get index number
        if ( defined( eval ("\$gRefXML$element_path\-\>{\"index\"}") ) ){
          $element_month_index = eval("\$gRefXML$element_path\-\>{\"index\"}");
        }else{
          $element_month_index = eval("\$gRefXML$element_path\-\>{\"number\"}");
        }

        # increment monthly index 0-11 -> 1-12, and add leading 0
        # to single digit months.
        $element_month_index = eval ("$element_month_index") + 1;

        if ( $element_month_index < 10 ){
          $element_month_index = "0$element_month_index";
        }

        # rename element type from "monthly" to "month_XX"
        $element_type = "month_$element_month_index";

      }

      # create a (semi)human readable string describing element"s
      # topology.
      my $readable_element_string = $element_path;
  
      # rename array location with element type
      $readable_element_string =~ s/\[[0-9]+\]/\{$element_type\}/g;
      $readable_element_string =~ s/->//g;    # remove arrows
      $readable_element_string =~ s/^{//g;    # remove leading brace
      $readable_element_string =~ s/}$//g;    # remove trailing brace
      $readable_element_string =~ s/}{/:/g;   # replace braces with colons

      # Delete "WattsToGJ:bin" and "binned_data" tags ( We"re effectively
      # merging two different elements into a parent element that will
      # hold data for both)
      $readable_element_string =~ s/:WattsToGJData:bin:/:/g;
      $readable_element_string =~ s/:binned_data:/:/g;
      
      #scope reference and test comparison variables
      my ($RefVal, $TestVal );

      # Loop through comparable attributes, and collect from XML
      foreach my $attribute ( keys %gNum_Compare_Vars ){

        # Check that attribute is actually defined in both reference
        # and test hashes. If so, collect values
        
        if ( eval("defined(\$gRefXML$element_path\-\>{$attribute})") &&
             eval("defined(\$gTestXML$element_path\-\>{$attribute})")){
          $RefVal = eval("\$gRefXML$element_path\-\>{$attribute}");
          $TestVal = eval("\$gTestXML$element_path\-\>{$attribute}");
        }else{
          $RefVal  = "undef";
          $TestVal = "undef";
        }
        
        # Rename "content" attribute for watts->gj tag to integrated_to_GJ
        if ( $attribute =~ /content/ && $element_units =~ /GJ/ ){
          # append renamed content tag to results hash
          $XMLresults{$readable_element_string}{integrated_to_GJ} = "$RefVal/$TestVal";
        }elsif ( $RefVal !~ /undef/ && $TestVal !~ /undef/ ){
          # append attribute and content tag to results hash
          $XMLresults{$readable_element_string}{$attribute} = "$RefVal/$TestVal";
        }
      }
      
      # for all units but "GJ", add unit attribute to test results hash
      if ( $element_units !~ /GJ/ ){
        $XMLresults{$readable_element_string}{units} = $element_units;
      }
    
    }
  }

  # Empty unneeded hashes
  %gElements = ();
  
  return %XMLresults;

}

#------------------------------------------------------------------
# This function will parse a hash containing results collected from
# the XML files, and perform context-aware comparisons on them.
# Note: the results hash is passed by reference into the routine
#------------------------------------------------------------------
sub CompareNumericalResults($$$){

  my ($folder, $model, $results) = @_;
  my $global_fail = 0;
  foreach my $element_path ( sort keys %$results ){

    # Get element"s units
    my $units = $$results{$element_path}{units};
    if ( ! defined($units) ){ $units = "(-)"; }
    # Loop through attributes and perform appropriate comparisons

    foreach my $attribute ( sort keys %{$$results{$element_path}} )   {
      
      # Check the value of "attribute" and perform comparisons.
      # Skip "units" since we can"t meaningfully compare them
      if ( $attribute !~ /units/ ){

        # Get reference and test values
        my ($ref_val, $test_val ) = split /\//, $$results{$element_path}{$attribute};

        # Check that tolerance has been defined for given units
        if ( defined( $gTolerance{$units} ) ){
          # Call number_cruncher to compare values
			
          my %difference = number_cruncher ($ref_val, $test_val, $units);

          # check if file passes, and set pass/fail flag. If case fails,
          # append 'difference' hash to failures hash for later reporting.
          if ( ! $difference{"pass"} ) {

            $global_fail = 1;

            if ( ! defined ( $gMax_difference{"global"}{"$units"}{"absolute"} ) ||
                  abs($difference{"absolute"}) >= $gMax_difference{"global"}{"$units"}{"absolute"} ){
                  $gMax_difference{"global"}{"$units"}{"relative"} = abs($difference{"relative"});
                  $gMax_difference{"global"}{"$units"}{"absolute"} = abs($difference{"absolute"});
                  $gMax_difference{"global"}{"$units"}{"folder"} = "$folder";
                  $gMax_difference{"global"}{"$units"}{"model"}  = "$model";
                  $gMax_difference{"global"}{"$units"}{"element_path"} = "$element_path";
                  $gMax_difference{"global"}{"$units"}{"attribute"} = "$attribute";

            }
            
            if ( ! defined ( $gMax_difference{"$folder;$model"}{"$units"}{"absolute"} ) ||
                  abs($difference{"absolute"}) >= $gMax_difference{"$folder;$model"}{"$units"}{"absolute"} ){
                  $gMax_difference{"$folder;$model"}{"$units"}{"absolute"} = abs($difference{"absolute"});
                  $gMax_difference{"$folder;$model"}{"$units"}{"relative"} = abs($difference{"relative"});
                  $gMax_difference{"$folder;$model"}{"$units"}{"element_path"} = "$element_path";
                  $gMax_difference{"$folder;$model"}{"$units"}{"attribute"} = "$attribute";
            }                                       

            %{$gNum_Failures{"$folder;$model;$element_path;$attribute"}{"difference"}} = %difference;
            $gNum_Failures{"$folder;$model;$element_path;$attribute"}{"units"} = $units;

            

          }else{
          }
        }
      }
    }
  }

  return $global_fail;
}


#------------------------------------------------------------------
# This function compares callgrind data from two bps's, and reports
# on the changes in computational requirements.
#------------------------------------------------------------------
sub CollectCallGrindResults($$$){

  my ($key,$ref_file,$test_file) = @_;

  my (%results, %file);

  $file{"reference"} = $ref_file;
  $file{"test"} = $test_file;
  
  my $debug_out = 0;
  
  # Collect procedure-by-procedure results for reference and test versions.
  foreach my $version (sort keys %file){

    open(INPUT,$file{"$version"}) or fatalerror("Could not open $file{\"$version\"}");

    my %referenced_by = ();
    my $program_total;
    my $parent = "";

    while ( my $line = <INPUT> ){

      # Delete leading spaces
      $line =~ s/^\s*//g; 

      # Delete any line that does not begin with a number
      $line =~ s/^[^0-9].*\n//g;

      # if there's anything left, process line.
      if ( $line ){
        
        # If line contains PROGRAM TOTALS keyphrase, it's describing
        # the total number of instructions. append to all-procedures 
        if ($line =~ /PROGRAM TOTALS/ ){

          $line =~ s/PROGRAM TOTALS//g;
          $line =~ s/\s*//g;
          $line =~ s/,//g;

          $results{$key}{"$version"}{"all procedures"}{"total_instructions"} = $line;

          $program_total = $results{$key}{"$version"}{"all procedures"}{"total_instructions"};


        }else{

          # Add meaningful delimiters to call-grind output to ensure
          # it can be consistantly parsed. Delimiter: ':~:'

          # Initial format:
          # 459,620,468  < /path/to/esrubps/bmatsu.F:mzcoe3_ (720x) [/home/aferguso/esp-prp/bin/bps]
          # 459,620,468  * /path/to/casual.F:mzcasi_ [/home/aferguso/esp-test/bin/bps]
          #      50,644  > /path/to/plt_zone_gain_coupling.F:fpltzone_convective_gain__ (720x) [/home/aferguso/esp-test/bin/bps]
          #       2,880  > /path/to/shocc_dummy.c:updateequipment_ (720x) [/home/aferguso/esp-test/bin/bps]
          #     120,303  > ???:s_copy (1440x) [/usr/lib/gcc/i686-pc-linux-gnu/3.4.6/libg2c.so.0.0.0]
          #      50,441  > /path/to/plt_zone_gain_coupling.F:fpltzone_radiant_gain__ (720x) [/home/aferguso/esp-test/bin/bps]
          #      12,240  > /path/to/esru_lib.F:eclose_ (720x) [/home/aferguso/esp-test/bin/bps]
          #      20,160  > /path/to/shocc_dummy.c:fetchloads_ (5040x) [/home/aferguso/esp-test/bin/bps
          # Final format:
          # 459,620,468:~:<:~:/path/to/bmatsu.F:mzcoe3_:~:(720x):~:[/home/aferguso/esp-prp/bin/bps] 
          # 459,620,468:~:*:~:/path/to/casual.F:mzcasi_:~:[/home/aferguso/esp-prp/bin/bps] 
          # 50,644:~:>:~:/path/to/plt_zone_gain_coupling.F:fpltzone_convective_gain__:~:(720x):~:[/home/aferguso/esp-prp/bin/bps] 
          # 2,880:~:>:~:/path/to/shocc_dummy.c:updateequipment_:~:(720x):~:[/home/aferguso/esp-prp/bin/bps]
          # 120,303:~:>:~:???:s_copy:~:(1440x):~:[/usr/lib/gcc/i686-pc-linux-gnu/3.4.6/libg2c.so.0.0.0]
          


          my $line_org = $line;

          # Extract number indicating how many instructions are associated
          # with this procedure.
          $line=~ s/(^[0-9,]+) /$1:~:/g;
          # Extract special character (*,<,>) indicating if this line describes
          # a procedure (*), a calling procedure (<) or a called procedure (>)
          $line=~ s/:~:\s*([\*|<|>])+\s* /:~:$1:~:/g;

          # Extract the number of calls to this procedure [e.g. (720x)]
          $line=~ s/\s*(\([0-9]+x\))\s*/:~:$1 /g;
          # Extrac the binary this procedure is cotnained in.
          $line=~ s/\s*(\[[^\s]+\])\s*$/:~:$1/g;

          # Split line according to delimiter.
          # - $iCount    : number of instructions
          # - $class     : flag for calling-procedure, current-procedure,
          #                called-procedure
          # - $procedure : procedure name
          # - $callcount : # of times a procedure was called. 
          
          my ($iCount, $class, $procedure, $callcount, $arg5) = split /:~:/, $line;

          # Recent versions of callgrid append full path to filename in front 
          # of procedure. Strip portion of procedure before last '/' character.
          # This is important as it allows us to compare corresponding proceudures
          # from different versions (and therefore located at different paths)
          $procedure =~ s/^.*\///g;          

          # Strip ',' from procedure instruction count --- allows them to
          # be intrepreted as ints
          $iCount =~ s/,//g;

          # Screen procedure name against known list of low-level procedures
          # that aren't part of ESP-r, and which we're not interested in
          # (these are usually elements of the c/c++ libraries)
          if ( ! IsLowLevel($procedure) ){

            if( $class =~ /\</ ) {
              # This number indicates the number of times other procedures called
              # the given procedure.
            
              $referenced_by{$procedure}{"callcount"} = $callcount;

            }

            if ( $class =~ /\*/ ){

              # This code can be used to limit output to a single 
              # procedure for debugging.
              #if ( $procedure =~ /0x08e109cc/ ) { $debug_out = 1; }
              #else{ $debug_out = 0; }
              
              # This is the total number of instructions for a routine
              $results{$key}{$version}{$procedure}{"total_instructions"} = $iCount;


              # Extract the number of times this procedure was referenced, and append
              # the number of calls to the procedures that reference it.
              foreach my $reference (keys %referenced_by ){
                $callcount = $referenced_by{$reference}{"callcount"};
                  $results{$key}{$version}{$procedure}{"references"}{$reference}{"callcount"} = $callcount;
              }

            }

            # Save data on sub-calls --- that is, the procedures that
            # this procedure calls. Instructions for each procedure is stored
            # along side it's current parent for later summing.
            if ( $class =~/\>/){

              $results{$key}{$version}{$parent}{'children'}{$procedure} = $iCount;
              # This code can be used to limit output to a single 
              # procedure for debugging.
              # if ( $procedure =~ /0x08e109cc/ ) { $debug_out = 1; }
              # else{ $debug_out = 0; }
              
            }
            
            # If this is a summary variable for a procedure, mark it as a
            # parent and begin tracking its children.
            if ( $class =~ /\*/  ){
              %referenced_by = ();
              $parent = $procedure;
            }
          
          }


          if ( $debug_out ) { print "$line_org\n"; } 

        }

      }

    }

    # Done parsing callgrind results.
    
    $debug_out = 0; 
    
    # Now compute the actual number of instructions in each procedure
    foreach my $procedure ( keys %{$results{$key}{$version}} ){

      # Get the number of instructions for this procedure and its children.
      my $local = defined( $results{$key}{$version}{$procedure}{"total_instructions"} ) ?
                   $results{$key}{$version}{$procedure}{"total_instructions"} : 0;

      # Skip procedures in low-level libraries
      if ( ! IsLowLevel($procedure) ){

      # This code can be used to limit output to a single 
      # procedure for debugging.
      # if ( $procedure =~ /prodedure_name/ ) { $debug_out = 1; }
      # else{ $debug_out = 0; }
              
      
        if ( $debug_out ){ print "\n>-- $version : $procedure TOTAL -> $local \n";}

        # Loop through this procedure's children, and subract their instructions
        # from the parent's total. 
        foreach my $child ( keys %{$results{$key}{$version}{$procedure}{'children'}} ){
          my $subcalls;
          # Only subtract instructions for children that are actually part of the
          # ESP-r build; procedures that are part of low-level libraries will be
          # aggregated into their calling ESP-r procedure. 
          if ( ! IsLowLevel($child) ){
            # The number of instructions executed by the child procedure...
            $subcalls = defined( $results{$key}{$version}{$procedure}{'children'}{$child} ) ?
                                 $results{$key}{$version}{$procedure}{'children'}{$child} : 0;
            # ... is subtracted from its parent's total.
            $local = $local - $subcalls;

          }
          if ( $debug_out ) { print "         - SUBCALL ".substr($child,0,30)."($subcalls) \n";}
        }

        # Save final talley for procedure's instructions.
        $results{$key}{$version}{$procedure}{"local_instructions"} = $local;
        

        if ( $debug_out ){ print "         = LOCAL  $local \n"; }


      }

    }

  }

  # Loop through all procedures in test version, and compare
  # them to reference version instructions.
  my $procedures = $results{$key}{"test"};
  my %diffs_for_sort=();
  foreach my $procedure ( keys %$procedures ){


      # Get instruction counts for test and reference version of this procedure.
      my ($test_inst, $reference_inst);
 
      $test_inst = defined ( $results{$key}{"test"}{"$procedure"}{"local_instructions"} ) ?
                   $results{$key}{"test"}{"$procedure"}{"local_instructions"} : 0 ;
      $reference_inst = defined( $results{$key}{"reference"}{"$procedure"}{"local_instructions"} ) ?
                        $results{$key}{"reference"}{"$procedure"}{"local_instructions"} : 0 ;
                        
      # Compute difference and % change
      $results{$key}{"deltas"}{"$procedure"}{"diff"} = $test_inst - $reference_inst ;
      
      if ( $reference_inst > 0 ) {
        $results{$key}{"deltas"}{"$procedure"}{"percent"} = ($test_inst - $reference_inst) / $reference_inst * 100;
      }else{
        $results{$key}{"deltas"}{"$procedure"}{"percent"} = "NaN";
      }

      # Save procedure and intruction as unique key for sorting later.
      $diffs_for_sort{$procedure}=$results{$key}{'deltas'}{$procedure}{'diff'};


  }

  # Now loop through reference procedures and make sure none are missing
  # from test versions

  $procedures = $results{$key}{"reference"};
  foreach my $procedure ( keys %$procedures ){
    
      if ( ! defined( $results{$key}{"deltas"}{"$procedure"}{"diff"} ) ) {
        my $reference_inst = $results{$key}{"reference"}{"$procedure"}{"local_instructions"};
        $results{$key}{"deltas"}{"$procedure"}{"diff"} = 0 - $reference_inst;
        $results{$key}{"deltas"}{"$procedure"}{"percent"} = -100 ;
        
        $results{$key}{"test"}{"$procedure"}{"local_instructions"} = 0;
        # Save procedure and intruction as unique key for sorting later.
        $diffs_for_sort{$procedure}=$results{$key}{'deltas'}{$procedure}{'diff'};
      }
  }

  # now sort differences,
  my @procedures_ranked=();

  # Now explode unique keys and save sorted procedures.

  @procedures_ranked = sort { $diffs_for_sort{$b} <=> $diffs_for_sort{$a} } keys %diffs_for_sort;

  # Append list of sorted procedures to results-hash
  $results{$key}{'procedures_assorted_desc'} = [ @procedures_ranked ];

  return %results;
  
}


#------------------------------------------------------------------
# This function will parse the $gRefXML object, and return a
# stack of keys that can be used sequentially access all of its
# elements. In this context, an "element" is a member that contains
# a single child hash describing it"s attributes.
#
# Note: This function CALLS ITSELF RECURSIVELY if a subbordinate
#       hash or array is encountered.
# 
#------------------------------------------------------------------
sub parse_hash_recursively(){

  # Note: @gElements and @gCurrent_path are inhereted
  # from function compareXML. We"ll use @gCurrent_path like
  # a stack here.

  # Turn current path set into a string that can be appended to
  # $gRefXML
  
  my $current_key_set = "";

  # If we"re in the midst of $gRefXML (ie. gCurrent_path is not
  # empty, Create a string from the keys in "current_path" that
  # can be used to access $gRefXML"s members:
  #
  #  eg: ->{key1}->{key2}[array_loc1]->{key3}...
  #
  if ( $#gCurrent_path + 1 > 0 ){
    foreach my $element ( @gCurrent_path ){
      $current_key_set .="$element";
    }
  }

  # Determine "type" (hash/array/scalar) of contents
  # of $gRefXML at current key set.
  
  my $type = ref(eval("\$gRefXML$current_key_set"));
  
  # Check if type is hash/array/scalar.
  if ( $type =~ /HASH/ ){
  
    # Content is a hash. Loop through children, and determine if
    # they contain child hashes.
    foreach my $element ( eval "keys \%{\$gRefXML$current_key_set}" ){
      my $child_is_hash_or_array = 0;

      # Check if this element"s value pair is another hash or array
      if ( eval ( "defined(\%{\$gRefXML$current_key_set\-\>{$element}})" ) ){
        if ( ref( eval( "\$gRefXML$current_key_set\-\>{$element}")) =~ /HASH/ ||
             ref( eval( "\$gRefXML$current_key_set\-\>{$element}")) =~ /ARRAY/   ){
          # YES! Child element is a hash.
            $child_is_hash_or_array = 1;
        }
      }

      # If child is another hash or array, take another step into the
      # object by calling self recursively.
      
      if ( $child_is_hash_or_array ){

        # push key onto stack
        push (@gCurrent_path, "->{$element}");

        # Recursively call parse_hash_recursively to deal with
        # subbordinate hash
        parse_hash_recursively();

        # Pop key from stack
        pop (@gCurrent_path);
       
      }else{

        # No! Child element is not a hash.
        # write the current path to element stack.
        # ( We use a hash to suppress duplicate records)
        my $element_list = "";
        foreach my $element (@gCurrent_path) {
          $element_list .= "$element"
        }
        $gElements{$element_list} = 1;

        # Add units as attribute to the current element.
        # Note: We assume that units are held 2 levels up
        # in the xml tree, and need to drop the
        # ->{hash_loc}[array_loc] from the current path
        
        my $units_path = $current_key_set;
        $units_path =~ s/->\{[^\{\}]+\}\[[0-9]+\]$//g;
        if ( eval ("defined(\$gRefXML$units_path\-\>{units})")){
          eval( "\$gRefXML$current_key_set\-\>{units} = \$gRefXML$units_path\-\>{units}");
        }
      }
    }
  }elsif ( $type =~ /ARRAY/ ){
    # Ugh --- there"s an array embedded within the hash!

    # Loop through array locations, and call parse_hash_recursively
    # (note, we assume that all arrays contain child hashes!)
    for (my $array_loc = 0;
            $array_loc < eval("\$#{\$gRefXML$current_key_set} + 1");
            $array_loc++ ){

      # push array location onto stack
      push (@gCurrent_path, "[$array_loc]");

      # Recursively call parse_hash_recursively to deal with
      # subbordinate array
      parse_hash_recursively();

      # Pop location from stack
      pop (@gCurrent_path);

    }
  }elsif ( $type =~ /SCALAR/ || ! $type ){
    # This shouldn"t happen!
  }
  return();
}

#-------------------------------------------------------------------
# Echo out tester configuration and quit. Intended for debugging
# purposes.
#-------------------------------------------------------------------
sub echo_config(){
  
  my $echo_msg = "

tester.pl: Script for automated testing of ESP-r.
           Testing initiatated at $gSys_params{\"date\"} $gSys_params{\"time\"}

Configuration:

   - Arguements:
        @ARGV
   - Processed arguements:
        @gProcessed_args
   - Echo configuration:             ".is_empty($gTest_params{"echo_config"})."
   - Test a single case:             ".is_empty($gTest_params{"single_case"})."";

  if ( $gTest_params{"single_case"}  ){
    $echo_msg .= "
       -> Single test case path:    ".is_empty($gTest_paths{"single_case"})."";
  }else{
    $echo_msg .= "
   - Test suite path:               $gTest_paths{\"test_suite\"}";
  }
  $echo_msg .= "\n
   - Test system parameters
      Test date / time:             $gSys_params{\"date\"} $gSys_params{\"time\"}
      Username:                     $gSys_params{\"username\"}
      Host:                         $gSys_params{\"hostname\"}
      Platform:                     $gSys_params{\"sys_type\"}
      Operating system:             $gSys_params{\"os_type\"} ";

         
  $echo_msg .= "\n
  - Comparison to archive:          ".is_empty($gTest_params{"compare_to_archive"});
        
  if ( $gTest_params{"compare_to_archive"} ){
    $echo_msg .= "
      -> Historical archive:        $gTest_paths{\"old_archive\"}

  - Reference system parameters
      Test date / time:             $gRef_Sys_params{\"date\"} $gRef_Sys_params{\"time\"}
      Username:                     $gRef_Sys_params{\"username\"}
      Host:                         $gRef_Sys_params{\"hostname\"}
      Platform:                     $gRef_Sys_params{\"sys_type\"}
      Operating system:             $gRef_Sys_params{\"os_type\"} ";

  }

  $echo_msg .= "\n
  - Test parameters:
      Short runs:                   ".is_empty($gTest_params{"abbreviated_runs"})."
      Short run period              ".is_empty($gTest_params{"abbreviated_run_period"})."
      Test .data files              ".is_empty($gTest_ext{"data"})."
      Test .h3k files               ".is_empty($gTest_ext{"h3k"})."
      Test .xml files               ".is_empty($gTest_ext{"xml"})."
      Test .csv files               ".is_empty($gTest_ext{"csv"})."
      Test .fcts files              ".is_empty($gTest_ext{"fcts"})."
             ";

  $echo_msg .= "
  - Comparison tolerances:

      -------------------------------------------
      | Units              | Tolerance          |
      -------------------------------------------\n";

   # This code can be improved!
  foreach my $unit ( sort keys %gTolerance ){
    my $units_whitespace  = 19 - length("$unit");
    my $toler_whitespace  = 19 - length("$gTolerance{\"$unit\"}");
    $echo_msg .= "      | $unit";
    for (my $iChar = 0; $iChar < $units_whitespace; $iChar ++ ){
      $echo_msg.=" ";
    }
    $echo_msg.="| $gTolerance{\"$unit\"}";
    for (my $iChar = 0; $iChar < $toler_whitespace; $iChar ++ ){
      $echo_msg.=" ";
    }
    $echo_msg .="|\n";
  }
  $echo_msg.="      -------------------------------------------\n";
               
  
  $echo_msg .= "\n
  - Create new historical archive:  ".is_empty($gTest_params{"create_archive"});
  if ( $gTest_params{"create_archive"} ){
    $echo_msg .= "
      -> Historical archive:        $gTest_paths{\"new_archive\"}";
  }

  $echo_msg .= "\n
  - bps binaries:
      -> Reference
             path:                  $gRef_Sys_params{\"hostname\"}:$gRef_Test_params{\"test_binary\"}
             modification date:     $gRef_Test_params{\"test_bin_mod_date\"}
             md5 checksum:          $gRef_Test_params{\"test_bin_md5sum\"}
      -> Test
             path:                  $gSys_params{\"hostname\"}:$gTest_params{\"test_binary\"}
             modification date:     $gTest_params{\"test_bin_mod_date\"}
             md5 checksum:          $gTest_params{\"test_bin_md5sum\"}";

  $echo_msg .= "\n
  - List of test parameters:\n";               
  foreach my $key ( sort keys %gTest_params ){
    $echo_msg .= sprintf ("      - %-30s %-50s \n", "$key\:", $gTest_params{$key});
  }
  
  $echo_msg .= "\n\n";
  return $echo_msg;
}

####################################################################
####################################################################
##############                                     #################
##############         LOW-LEVEL FUNCTIONS         #################
##############                                     #################
####################################################################
####################################################################



#-------------------------------------------------------------------
# Simple function that strips unneeded white space from system 
# params for output,
#-----------------------------------------------------------------------
sub CleanParams(){

  # strip trailing lines from white space parameters,
  # and convert '~' to ' '. Remember, ~ is used to
  # demarcate spaces in space-separated configuration.txt
  # file in historical tester.pl archives.
  foreach my $key ( keys %gTest_params ){
    $gTest_params{$key} =~ s/\n//g;
    $gTest_params{$key} =~ s/~/ /g;
  }
  foreach my $key ( keys  %gSys_params ){
    $gSys_params{$key} =~ s/\n//g;
    $gSys_params{$key} =~ s/~/ /g;
  }
  foreach my $key ( keys %gRef_Test_params ){
    $gRef_Test_params{$key}  =~ s/\n//g;
    $gRef_Test_params{$key}  =~ s/~/ /g;
  }
  foreach my $key ( keys %gRef_Sys_params ){
    $gRef_Sys_params{$key}  =~ s/\n//g;
    $gRef_Sys_params{$key}  =~ s/~/ /g;
  }
}


#-------------------------------------------------------------------
# Compare two numerical values, and fill a hash describing their
# differences.
#-------------------------------------------------------------------
sub number_cruncher($$$){
  my ($ref, $test, $units) = @_;

  # scope results hash:
  my (%difference);

  # compute absolute difference
  my $absolute_difference =  $ref - $test;

  
  # compute relative difference
  my $relative_difference;

  if ( abs($absolute_difference) > $gSmall ){

    # Check values to avoid divide-by-zero error
    SWITCH:
    {
       if ( abs ($ref) > $gSmall )
             {
                 $relative_difference = ($test - $ref ) / $ref * 100.0 ;
                 last SWITCH;
             }
       if ( abs($ref) < $gSmall && abs($test) > $gSmall )
             {
                 $relative_difference = ( $ref - $test ) / $test * 100.0;
                 last SWITCH;
             }
       # Default (shouldn"t happen though)
       $relative_difference = "undef";
    }
  }else{
    $relative_difference = 0.0;
  }
  
  # does the difference between values exceed the tolerance?
  if ( abs ( $absolute_difference ) - $gTolerance{$units} < $gSmall ||
       abs ( $relative_difference ) - $gTolerance{"relative"} < $gSmall  ){
    
    # no - test passes
    $difference{"pass"} = 1;

  }else{
    # yes - test fails
    $difference{"pass"} = 0;

  }
  

  $difference{"absolute"} = $absolute_difference;
  $difference{"relative"} = $relative_difference;
  $difference{"diff_to_abs_tolerance"} = abs ( $absolute_difference ) - $gTolerance{$units};
  $difference{"diff_to_rel_tolerance"} = abs ( $relative_difference ) - $gTolerance{"relative"};
  $difference{"reference_value"} = $ref;
  $difference{"test_value"}      = $test;
  
  return %difference;
  
}


#-------------------------------------------------------------------
# Optionally write text to buffer
#-------------------------------------------------------------------
sub stream_out($){
  my($txt) = @_;
  if ($gTest_params{"verbosity"} ne "quiet"){
    print $txt;
  }
}

#-------------------------------------------------------------------
# Display a fatal error and quit.
#-------------------------------------------------------------------

sub fatalerror($){
  my ($err_msg) = @_;

  if ( $gTest_params{"verbosity"} eq "very_verbose" ){
    print echo_config();
  }
  print "\ntester.pl -> Fatal error: \n";
  print " >>> $err_msg \n\n";
  die;
}

#-------------------------------------------------------------------
# Check to see if arguement is 1 or 0, and return "enabled" / "disabled".
# Otherwise, return arguement.
#-------------------------------------------------------------------

sub is_empty($){
  my ($arguement) = @_;
  my ($result);
  if ( $arguement eq "1" ){
    $result = "enabled";
  }elsif ( $arguement ){
    $result = $arguement;
  }else{
    $result = "disabled";
  }
  return $result;
}

#-----------------------------------------------------------------------
# Check to see if arguement is "enabled" / "disabled", and return 1/0.
# Otherwise, return arguement.
#-----------------------------------------------------------------------

sub is_true_false($){
  my ($arguement) = @_;
  my ($result);
  if ( $arguement =~ /enabled/ ){
    $result = 1;
  }elsif ( $arguement =~ /disabled/ ){
    $result = 0;
  }else{
    $result = $arguement;
  }
  return $result;
}

#-----------------------------------------------------------------------
# Delete simulation files
#-----------------------------------------------------------------------
  
sub delete_old_files(){
  execute("rm -fr _.xml out.csv out.xml *.h3k *.fcts* *.res *.bres *.eres *.pres libb libp libe callgrind.out.* ");
  return;
}

#--------------------------------------------------------------------
# Perform system commands with optional redirection
#--------------------------------------------------------------------
sub execute($){
  my($command) =@_;
  my $result;
  if ($gTest_params{"verbosity"} eq "very_verbose"){print "\n > executing $command \n";}
  if ($gTest_params{"logfile"}){
    $result = system("$command $gTest_params{\"logfile\"}");
  }else{
    $result = system($command);
  }
  return $result;
}

#--------------------------------------------------------------------
# Log messages in log file, if specified.
#--------------------------------------------------------------------
sub log_msg($){
  my($msg) =@_;
  system( "echo -E \"$msg\" $gTest_params{\"logfile\"}");
  return 1;
}

#-----------------------------------------------------------------------
# Extract model name (zzz) from path containing cfg file
# (abc/xxx/cfg/zzz.cfg)
#-----------------------------------------------------------------------
sub get_model_name($){
  my($case_path) = @_;
  $case_path =~ s/(^|.*\/)([^\*\/]*)\.cfg$/$2/g;
  $case_path =~ s/\_temp$//g;
  return $case_path;
}

#-----------------------------------------------------------------------
# Get model folder (abc/xxx) from path containing cfg file
# (abc/xxx/cfg/zzz.cfg)
#-----------------------------------------------------------------------
sub get_model_folder($){
  my($folder_path) = @_;
  $folder_path =~ s/(.*)\/cfg\/.*/$1/g;
  return $folder_path;
}

#-----------------------------------------------------------------------
# Get model root folder (xxx) from path containing cfg file
# (abc/xxx/cfg/zzz.cfg)
#-----------------------------------------------------------------------
sub get_model_root_name($){
  my($folder_path) = @_;
  $folder_path =~ s/(.*)\/([^\/]*)\/cfg\/.*/$2/g;
  return $folder_path;
}

#----------------------------------------------------------------------
# Simple function for converting realtive paths into absolute ones.
# Added because perl"s "realpath" function has been flakey.
#----------------------------------------------------------------------
sub resolve_path ($){

  my ( $path ) = @_;
  
  if ( $path !~ /^c:/ ){
    # if path is not absolute, prepend master path.
    if ( $path !~ /^\// ){
      $path = "$gTest_paths{\"master\"}/$path";
    }
    
    # Nuke extraneous characters
    $path =~ s/\/\//\//g;             # elimates double slashes
    $path =~ s/\/\.\//\//g;           # eliminates /./ characters
    while ( $path =~ /\.\./ ){
      $path =~ s/\/[^\/]+\/\.\.\//\//;  # resolves /path/to/../file paths
      $path =~ s/^\/\.\.\//\//;         # resolves /../ error 
    }
  }
  return $path;
}

#----------------------------------------------------------------------
# Convert " " character to a "~" for use in space-separated output
#----------------------------------------------------------------------
sub convert_spaces($){
  my $string = @_;
  $string =~ s/ /~/g;
  return $string; 
}

#----------------------------------------------------------------------
# Convert test pass/fail result into flag suitable for
# summary chart
#----------------------------------------------------------------------
sub result_to_string($$){

  my ($test, $extention) = @_;
  my $string;
  
  if ( defined( $gTest_Results{$test}{$extention} ) ){
    if ( $gTest_Results{$test}{$extention} =~ /pass/ ){
      $string = '-'
    }else{
      $string = 'X'
    }
  }else{
    $string = '.';
  }
  return $string;
}

#----------------------------------------------------------------------
# Format a number using the provided string, and return string containing
# number.
#----------------------------------------------------------------------
sub format_my_number($$$){
  my ($value,$length,$format) = @_;

  $value = sprintf(
             "%".eval($length-1)."s ",
             sprintf ($format,$value)
           );
  
  return $value;
}
#-------------------------------------------------------------
# This procedure turns a long int into a readable number. EG:
#  1225333647 -> +1,225,333,647
# -12722787   -> -12,722,787
#-------------------------------------------------------------
sub format_instruction_count($){

  my $count = "@_";



  my $buffer = "";

  my $sign = $count =~ /^-/ ? "-" : "+" ;
  $count =~ s/^[-|+]//g;;

  my $start = 0;
  my $end = length ($count);

  while ( $end > $start  ){
    
    my $length = $end - 3 < $start ? $end - $start : 3;
    
    if ( $buffer ){

      $buffer = ",$buffer";
      
    }

    $buffer = substr($count,$end-$length,$length)."$buffer";
    $end = $end-$length;
    
  }
  

  return "$sign$buffer";

}

#-----------------------------------------------------------------------
# This function is used to query a procedure name against a list of
# known low-level functions that exist outside ESP-r source code. It's
# used to filter functions that have no bearing on ESP-r from call-grind
# results.
#-----------------------------------------------------------------------
sub IsLowLevel($){

  my ($procedure) = @_;
  my $result;

  # Check if procedure matches list of known low-level trouble makers
  if ( $procedure =~ /^\?\?\?/ ||
       $procedure =~ /^basic_string\.h/ ||
       $procedure =~ /^crt[i|n]\.S/ ||
       $procedure =~ /^[_a-z]+\.h/  ||
       $procedure =~ /^vector\.tcc/ ){
       
    $result = 1;

  }else{

    $result = 0;

  }

  return $result;
  
}

