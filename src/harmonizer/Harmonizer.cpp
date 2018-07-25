/*
 This file is part of the ESP-r system.
 Copyright Energy Systems Research Unit, University of
 Strathclyde, Glasgow Scotland, 2001.

 ESP-r and the harmonizer are free software.  You can redistribute
 them and/or modify them under the terms of the GNU General Public
 License as published by the Free Software Foundation
 (version 2 orlater).

 ESP-r and the harmonizer are distributed in the hope that they will
 be useful but WITHOUT ANY WARRANTY; without even the implied warranty
 of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 PURPOSE. See the GNU General Public License for more  details.

 You should have received a copy of the GNU General Public
 License along with ESP-r. If not, write to the Free
 Software Foundation, Inc., 59 Temple Place, Suite 330,
 Boston, MA 02111-1307 USA.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

 This file is Harmonizer.cpp contains the main functionality of the harmonizer dll.  For full explanations see
 1. 'THE DESIGN OF AN ESP-r AND TRNSYS CO-SIMULATOR' by Ian Beausoleil-Morrison, Francesca Macdonald, Michael Kummert
  	 Timothy McDowell and Romain Jost and Alex Ferguson (Building Simulation 2011)
 2. 'A Demonstration of the Run-Time Coupling between ESP-r and TRNSYS' by Francesca Macdonald, Romain Jost,
     Ian Beausoleil-Morrison, Michael Kummert, Timothy McDowell, and Alex Ferguson (SimBuild 2012)

The Harmonizer's primary functionality:
1) To load and Initialise both TRNSYS & ESP-r.
2) To create threads for both ESP-r and TRNSYSs simulation time loops
3) To handle the data exchange & ensure synchronisation
4) To manage overall system convergence

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

This file contains the following functions:

 * Initialization Functions:
 * -------------------------
 * SetHarmonizerInput:  Called by the launching executable to initialize the harmonizer with the harmonizer input file.
 * 						This subroutines reads the file and stores the locations of the bps dll and the TRNSYS dll
 * 						together with the command line for bps and the TRNSYS deck file.
 *
 * Main Functions:
 * --------------
 * Main:					Loads and initializes both TRNSYS and ESP-r.  Then it checks that simulation parameters are
 * 							compatible (by calling CheckCoSimValidity).  It creates threads for each program (with
 * 							CreateSimulationThread) and then launches ESP-r (LaunchESPrTimeLoop) which in turn
 * 							launches TRNSYS(LaunchTRNSYSTimeLoop).  It then waits for the threads to complete before
 * 							terminating the program.
 * CheckCoSimValidity:		Checks to insure that the simulation time parameters are the same for both programs.
 * CreateSimulationThread:	Creates the threads for both ESP-r and TRNSYSs simulation time loops.
 * LaunchESPrTimeLoop:		Launches the ESP-r simulation by calling the subroutine subbps in esrubps/bps.F.
 * LaunchTRNSYSTimeLoop:	Launches the TRNSYS simulation by repeatedly calling the TRNSYS routine.
 *
 * Functions called by TRNSYS
 * --------------------------
 * GETESPRDATA:			Called by TRNSYS to retrieve the data passed by ESP-r and stored in the harmonizer.
 * PASSDATATOESPR:		Called by TRNSYS to pass the derived data structure to the harmonizer, where it is copied to the
 * 						harmonizers global variable EsprData casting all doubles to floats.
 *
 * Functions called by ESP-r
 * -------------------------
 * SETARRAYSIZES:		Sets the sizes of each of the arrays contained in the data structure passed between ESP-r and TRNSYS.
 * 						This improves efficiency as only for the number of arrays that actually contain data are copied (as
 * 						opposed to the maximum possible).
 * GETTRNSYSDATA:		Called by bps to retrieve the data passed by TRNSYS and stored in the harmonizer.
 * PASSDATATOTRNSYS:	Called by ESP-r to pass the derived data structure to the harmonizer, where it is copied to the
 * 						harmonizers global variable EsprData casting all floats to doubles.
 *
 * Convergence functions
 * ---------------------
 * GETSYSTEMCONV:			Called by ESP-r and TRNSYS to see if both programs have converged.
 * CheckForZeroFlow:		Checks for a zero flowrate condition which results in a division by zero in evaluation flow convergence
 * 							and renders checking temperature convergence as meaningless.
 * CheckTemperatureConvergence:		Ensures temperatures stabilise within the convergence tolerance for Temperature.
 * CheckFlowrateConvergence:		Ensures flow rates stabilise within the convergence tolerance for flowrate.
 * CheckMoistureFlowConvergence:	Ensures moisture flows stabilise within the convergence tolerance for moisture flowrate.
 *
 */
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#include <pthread.h> //used in GCC for thread support
#include <stdio.h>
#include "CTrnsysDLLObject.h"
#include "HarmonizerDataTypes.h"
#include <fstream>
#include <math.h>
#include <iostream>


using namespace std;

typedef void (__stdcall *FNPTR)(const float*);
typedef void (__stdcall *STRFNPTR)(int*, char*, int*);
typedef void (__stdcall *DATAFNPTR)(int*, float*);
typedef int (__stdcall *INTFNPTR)(void);

//Number of threads
#define N_THREADS 2
#define ESPr_THREAD 0
#define TRNSYS_THREAD 1
//In the Windows API (with some exceptions discussed in the following paragraphs), the maximum length for a path is MAX_PATH,
//which is defined as 260 characters. A local path is structured in the following order: drive letter, colon, backslash,
//name components separated by backslashes, and a terminating null character. For example, the maximum path on drive D is
//"D:\some 256-character path string<NUL>" where "<NUL>" represents the invisible terminating null character for the current
//system codepage. (The characters < > are used here for visual clarity and cannot be part of a valid path string.)
#define MAX_CHAR 260
//Convergence criteria
#define TEMPERATURECONVERGENCE  0.01 //default temperature convergence deg C
#define FLOWCONVERGENCE  0.1  //default flow convergence %
#define MAX_INVOCATIONS 50	  //max number of invocations (default)
#define MIN_INVOCATIONS  2	  //min number of invocations

//array indices for bool matrix bZeroFlow to check for zero flow condition in all HCCs and ACCs
#define HCC_TO_TRNSYS_ARRAY_POS   0
#define HCC_TO_ESPR_ARRAY_POS     1
#define ACC_TO_TRNSYS_ARRAY_POS   2
#define ACC_TO_ESPR_ARRAY_POS     3
#define NUMBER_OF_ZONES_ARRAY_POS 4

#define VERSION_LINE  	 0		// Line number of harmonizer input file with version number
#define BPS_DLL_LINE  	 4		// Line number of harmonizer input file with location of bps dll
#define TRNSYS_DLL_LINE  6		// Line number of harmonizer input file with location of TRNSYS dll
#define BPS_CL_LINE  	 8		// Line number of harmonizer input file with command line for bps
#define TRNSYS_DCK_LINE  10		// Line number of harmonizer input file with location and name of TRNSYS deck file
#define CONVERGENCE_LINE 12		// Line number of harmonizer input file with convergence criteria

#define VERSION2	     2.0	// File version including convergence criteria
/////////////////////////////////////////////////////////////////////////////////////////////////
//global variables
/////////////////////////////////////////////////////////////////////////////////////////////////
float FileVersionNo;
//Path of bps dll
char bpsDll[MAX_CHAR];
//Path of TRNSYS dll
char TrnsysDll[MAX_CHAR];
//ESP-r dll
HINSTANCE ESPr_dll;
// TRNSYS dll object
CTrnsysDllObject TRNSYS_dll;
//the bps command line
char bpsCommandLine[MAX_CHAR];
//the number of arguments in the bps command line
int  bpsCommandLineArg;
//the TRNSYS deck file
char TRNSYSdeckFile[MAX_CHAR];
//threads for ESP-r & TRNSYS simulations
pthread_t ESPr_thread, TRNSYS_thread;

//Flag for when TRNSYS is initializing
bool bTrnsysInitializing;
//Flag used when ESP-r is calling the harmonizer without TRNSYS
bool bBpsStandalone;
//Flag used to tell the main thread if the number of timesteps in ESP-r and TRNSYS is the same.
//This is needed to terminate a waiting thread, so the harmonizer terminates and doesn't just hang.
bool bCosimValid;
//Flag set to true once one program has checked for convergence and it has been reached so
//the subsequent program 'knows' convergence has been achieved without having to check it numerically.
bool bSystemConverged;
//Flag used to ensure ESP-r and TRNSYS don't check for convergence at exactly the same time.
bool bCheckingConvergence;
//Flag used to ensure ESP-r and TRNSYS don't both check for convergence.  If the numbers don't converge for one program
//nothing is gained by checking them twice and it could slow things down.
bool bSkipNextCheck;
//Flags used to ensure ESP-r and TRNSYS call the harmonizers functions in the correct order to ensure synchronization
//otherwise the two threads can access the functions in the wrong order and corrupt the data
bool bPassingDataToTrnsys;
bool bPassingDataToEspr;
//bool matrix to check for zero flow condition in all HCCs and ACCs
bool bZeroFlow[4][MAX_FCCs];
//temporary variable used to fix the TRNSYS zero start issue.
int TrnsysStartTS;
//Counter of the number of times the co-simulation fails to converge.
int iConvergenceFailed;
//Array sizes used (to save computational time in copying & comparing data used)
int nHCC_to_Trnsys;
int nHCC_to_ESPr;
int nACC_to_Trnsys;
int nACC_to_ESPr;
int nZONES;

//The harmonizers' derived data structures using in copying data in and out
d_EsprTrnsysData EsprData; //all real values are double precision as required by TRNSYS
f_EsprTrnsysData TrnsysData; //all real values are single precision as required by ESP-r

float fTemperatureConvergence; 		//convergence criteria for temperature
float fFlowConvergence;		 		//convergence criteria for flow
float fMoistureFlowConvergence;		//convergence criteria for moisture flow
int iMaxInvocations;				//Maximum number of invocations

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// The following data structures are used for convergence checking (inside the harmonizer).
EsprConvergenceData* pESPrConvData;
TrnsysConvergenceData* pTrnsysConvData;


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Function called by launching executable prior to calling 'main'
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*
SetHarmonizerInput is called by the launching executable passing the harmonizer input file.  This subroutines reads the file and
stores the locations of the bps dll and the TRNSYS dll together with the command line for bps and the TRNSYS deck file.
*/
extern "C"  //ensures the function name in the dll isn't mangled
void SetHarmonizerInput(char* FileName)
{
	//Initialize global convergence variables (for harmonizer inputfiles of versions predating 1.2)
	fTemperatureConvergence = TEMPERATURECONVERGENCE;
	fFlowConvergence = FLOWCONVERGENCE;
	fMoistureFlowConvergence = FLOWCONVERGENCE;
	iMaxInvocations = MAX_INVOCATIONS;
	//set the harmonizer input filename
    char HarmonizerInputFileName[MAX_CHAR];
    strcpy(HarmonizerInputFileName, FileName);
    string line;
    ifstream HarmonizerFile(HarmonizerInputFileName);
    int linecount = bpsCommandLineArg = 0;
    //open the file
    if (HarmonizerFile.is_open()){
        cout << "\n\n ---------------Reading Hamonizer input file.\n";
        while (HarmonizerFile.good()){
        	//read it line by line
            getline(HarmonizerFile, line);
            //line containing the file's version number
            if (linecount == VERSION_LINE){
            	FileVersionNo = atof(line.c_str()); //convert string to floating point.
            }
            //path to the bps dll
            if (linecount == BPS_DLL_LINE){
                strcpy(bpsDll,line.data());
                cout << " bpsDll: " << bpsDll << "\n";
            }
            //path to TRNSYS dll
            else if (linecount == TRNSYS_DLL_LINE){
                 strcpy(TrnsysDll,line.data());
                 cout << " TrnsysDll: " << TrnsysDll << "\n";
            }
            //bps command line
            else if (linecount == BPS_CL_LINE){
                strcpy(bpsCommandLine,line.data());
                printf(" Bps command line:%s\n\n",bpsCommandLine);
                //count the command line aguments
                for (int i=0; i<MAX_CHAR; i++){
                    if (bpsCommandLine[i] == ' ')
                        bpsCommandLineArg++;
                }
            }
            //path to TRNSYS deck file
            else if (linecount == TRNSYS_DCK_LINE){
                strcpy(TRNSYSdeckFile,line.data());
                printf(" TRNSYS deck file:%s\n",TRNSYSdeckFile);
                bBpsStandalone = FALSE;
                // if the deckfile specified is 'N/A' to run Bps alone
                if (line == "N/A"){
                	bBpsStandalone = TRUE;
                }
            }
            //convergence criteria
            else if (linecount == CONVERGENCE_LINE){
            	//convergence criteria were introduced with version 1.2 of the file
            	//Thus the lines of files with smaller version numbers are ignored.
            	if (FileVersionNo >= VERSION2){
             		char * pch;
            		pch = strtok((char*) line.data(),",");
            		int loop = 0;
            		while (pch != NULL)
            		{
//            		    printf ("%s\n",pch);
            		    if (loop == 0){
            		    	//convert string to floating point convergence criteria for temperature
            		    	fTemperatureConvergence = atof(pch);
            		    }
            		    else if (loop == 1){
            		    	//convert string to floating point convergence criteria for flow
            		    	fFlowConvergence = atof(pch);
            		    }
            		    else if (loop == 2){
            		        //convert string to floating point convergence criteria for moisture flow
                    		fMoistureFlowConvergence = atof(pch);
            		    }
            		    else if (loop == 3){
            		        //convert string to integer to get max number of invocations
            		    	iMaxInvocations = atoi(pch);
            		    }
            		    pch = strtok (NULL, ",");
            		    loop++;
            		 }
            	}
            }
            linecount++;
        }
	    printf ("Convergence criteria:\n");
	    printf ("Temperature=%4.4f, Flow=%4.4f, Moisture_Flow=%4.4f, Max no of invocations=%d\n\n",fTemperatureConvergence,fFlowConvergence,fMoistureFlowConvergence, iMaxInvocations);
    	//allocate memory for convergence arrays (the extra 10 is required because for some reason smaller arrays cause the program to end abruptly terminating calling thread)
    	pESPrConvData = new EsprConvergenceData[iMaxInvocations+10];
    	pTrnsysConvData = new TrnsysConvergenceData[iMaxInvocations+10];
        //close the file.
        HarmonizerFile.close();
    }
    else cout << " Unable to open Harmonizer input file.\n";
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/*Check to insure that the simulation time parameters are the same for both programs*/
void CheckCoSimValidity()
{
	// check to see if the climate files are the same
	// Tim is not sure why we would need the harmonizer to check if the climate files are the same -
	// some onus can be put on the user to get things correct when they set-up their simulation.
	// He can also envision some rare cases where you might want to drive the TRNSYS simulation with
	// a custom weather file that differs from what is used in the ESP-r simulation.

	// check to see if the total number of timesteps are the same
    //the TRNSYS simulation timestep (in hours)
	printf("\n\n ---------------Checking co-simulation validity .....\n\n");
    double TrnsysTime = TRNSYS_dll.GetTimeStep();
	//the number of timesteps per hour in the ESP-r simulation
    INTFNPTR EsprTimestepFunc = (INTFNPTR)GetProcAddress(ESPr_dll, (LPCSTR)"gettimesteps_");
    int EsprTimestep = EsprTimestepFunc();
    if (EsprTimestep != int(TrnsysTime+0.5)){
    	printf(" ******************************************************\n");
    	printf(" *****Warning: TRNSYS & ESP-r have different time steps\n");
    	printf(" ******************************************************\n");
     	printf(" TRNSYS timesteps: %d\n", (int)(TrnsysTime+0.5));
    	printf(" ESP-r timesteps: %d\n", EsprTimestep);
    }
    else
    	printf(" TRNSYS & ESP-r have the same number of time steps per hour: %d\n", EsprTimestep);

	//the start time of the TRNSYS simulation (hour of year)
    int TrnsysStart = (int)((TRNSYS_dll.GetStartTime()+24.0)/24.0);
    TrnsysStartTS = ((TrnsysStart-1)*(int)(TrnsysTime+0.5)*24);
	//the first day of the ESP-r simulation (julian day)
    INTFNPTR EsprStartFunc = (INTFNPTR)GetProcAddress(ESPr_dll, (LPCSTR)"getsimulationstart_");
    int EsprStart = EsprStartFunc();
	//the startup period used in the ESP-r simulation
    INTFNPTR EsprStartupFunc = (INTFNPTR)GetProcAddress(ESPr_dll, (LPCSTR)"getstartup_");
    int EsprStartup = EsprStartupFunc();
    if ((EsprStart-EsprStartup) != TrnsysStart){
    	//Check to make sure TRNSYS start day is not specified as a day at the end of the previous
    	//year (which is how a start up period would be specified in TRNSYS).
    	int negStart = 365 - TrnsysStart;
        if ((EsprStart-EsprStartup) == -negStart)
        	printf(" TRNSYS & ESP-r have the same start day: %d\n", (EsprStart-EsprStartup));
        else{
			printf(" ******************************************************\n");
			printf(" ****Warning: TRNSYS & ESP-r have different start times\n");
			printf(" ******************************************************\n");
			printf(" TRNSYS start day: %d\n", TrnsysStart);
			printf(" Espr start day: %d\n", (EsprStart-EsprStartup));
			printf(" ESP-r start day (excluding start up period): %d\n", EsprStart);
			printf(" ESP-r startup: %d days\n", EsprStartup);
        }
    }
    else
    	printf(" TRNSYS & ESP-r have the same start day: %d\n", (EsprStart-EsprStartup));


    //the stop time of the TRNSYS simulation (hour of year)
    double TrnsysStop = TRNSYS_dll.GetStopTime()/24;
	//the last day of the ESP-r simulation (julian day)
    INTFNPTR EsprStopFunc = (INTFNPTR)GetProcAddress(ESPr_dll, (LPCSTR)"getsimulationend_");
    int EsprStop = EsprStopFunc();//includes that last day.
    if (EsprStop != (int)(TrnsysStop)){
    	//Check to make sure TRNSYS end day is not specified as a day in the second
    	//year (which is how a start up period would be specified in TRNSYS).
    	int negEnd = (int)TrnsysStop - 365;
        if (EsprStop == negEnd)
        	printf(" TRNSYS & ESP-r have the same stop day: %d\n", EsprStop);
        else{
			printf(" ******************************************************\n");
			printf(" ******Warning: TRNSYS & ESP-r have different stop days\n");
			printf(" ******************************************************\n");
			printf(" TRNSYS stop day: %d\n", (int)TrnsysStop);
			printf(" ESP-r stop day: %d\n", EsprStop);
        }
    }
    else
    	printf(" TRNSYS & ESP-r have the same stop day: %d\n", EsprStop);
    //calculate the total number of timesteps
    int TotalTrnsysTimesteps = TRNSYS_dll.GetnTimeSteps();
    int TotalEsprTimesteps = (EsprStop+1-(EsprStart-EsprStartup))*24*EsprTimestep;
	bCosimValid=true;
    if (TotalTrnsysTimesteps != TotalEsprTimesteps ){
    	printf(" *************************************************************************\n");
    	printf(" ******Warning: TRNSYS & ESP-r have a different number of total time steps\n");
    	printf(" *************************************************************************\n");
        printf(" Total TRNSYS timesteps: %d\n", TotalTrnsysTimesteps);
    	printf(" Total ESP-r timesteps: %d\n\n\n", TotalEsprTimesteps);
    	bCosimValid=false;
    }
    else
    	printf(" TRNSYS & ESP-r have the same total number of timesteps: %d \n\n", TotalEsprTimesteps);

 }

/*LaunchESPrTimeLoop launches the ESP-r simulation by calling the subroutine subbps in esrubps/bps.F.
  It finds that subroutine using the windows API function GetProcAddress */
void *LaunchESPrTimeLoop(void * threadid)
{
    // Get the address of the simulation time loop in ESP-r
    FARPROC  runESPrSimulation = GetProcAddress(ESPr_dll, (LPCSTR)"subbps_");
    //check for valid function address
    if (runESPrSimulation){
        //call simulation time loop
        printf(" ---------------Running ESP-r simulation\n");
        runESPrSimulation();
    }
    else{
        printf(" Could not find the procedure address to run ESP-r simulation\n");
    }
    printf(" ---------------Ended running ESP-r simulation\n");
    if (!bCosimValid){
   	 printf(" Co-simulation unsuccessful: The total number of timesteps specified in TRNSYS and ESP-r were not equal.");
     //terminate program
   	 exit(0);
    }
    //terminate the thread and makes threadid available to 'join' with the terminating thread
    pthread_exit(threadid);
}

/*LaunchTRNSYSTimeLoop launches the TRNSYS simulation by repeatedly calling the TRNSYS routine.*/
void *LaunchTRNSYSTimeLoop(void * threadid)
{
	if (!bBpsStandalone){
    	//call simulation time loop
    	TRNSYS_dll.RunTrnsysSimulation();
    	printf(" ---------------Ended running TRNSYS simulation\n");
        if (!bCosimValid){
       	 printf(" Co-simulation unsuccessful: The total number of timesteps specified in TRNSYS and ESP-r were not equal.");
         //terminate program
       	 exit(0);
        }
	}
    //terminate the thread and makes threadid available to 'join' with the terminating thread
    pthread_exit(threadid);
}

/*CreateSimulationThread creates the threads for both ESP-r and TRNSYSs simulation time loops.*/
void CreateSimulationThread(int thread_id)
{
    //Function 2: Creating threads for both ESP-r and TRNSYSs simulation time loops
    pthread_attr_t tattr;                   //thread attributes
    int returnval;                          //to check the sucess of thread functions

    // initialize the thread attributes with default attributes
    returnval = pthread_attr_init(&tattr);
    if (returnval){
        printf(" Error: initialising thread attributes: %d\n", returnval);
        exit(-1);
    }
    // Setting the  size of the stack and allocating memory for it is essential to being
    // able to run ESP-r & TRNSYS as threads (they are unusually large for threads)

    // setting the base address in the attribute
    // The stackaddr attribute defines the base of the thread's stack.
    // If this is set to non-null (NULL is the default) the system initializes the stack at that address.
    void *stackbase;
    int size = 40000000; //This is the size required to run an annual simulation.
    stackbase = (void *) malloc(size);
    returnval = pthread_attr_setstackaddr(&tattr, stackbase);
    if (returnval){
        printf(" Error: setting the base address: %d\n", returnval);
        exit(-1);
    }
    // setting the size of the stack also
    returnval = pthread_attr_setstacksize(&tattr, (size_t)size);
    if (returnval){
        printf(" Error: setting the size of the stack: %d\n", returnval);
        exit(-1);
    }

    // setting the thread to be joinable - needed to be able to wait on it
    returnval = pthread_attr_setdetachstate(&tattr, PTHREAD_CREATE_JOINABLE);
    if (returnval){
        printf(" Error: setting the thread as joinable: %d\n", returnval);
        exit(-1);
    }

    if (thread_id == ESPr_THREAD)
    {
        int espr_id = ESPr_THREAD;
        //Set synchronisation flags to false.
        TrnsysData.Ready = false;
        EsprData.Ready = false;
        //Creating ESP-r_thread with tattr attributes which calls LaunchESPrTimeLoop
        printf("\n\n ---------------Creating ESP-r_thread\n");
        returnval = pthread_create(&ESPr_thread, &tattr, LaunchESPrTimeLoop, (void *)espr_id);
        if (returnval){
            printf(" Error: return code from pthread_create() for ESP-r_thread is %d\n", returnval);
            exit(-1);
        }
    }
    else if (thread_id == TRNSYS_THREAD){
        int trnsys_id = TRNSYS_THREAD;
        //Creating TRNSYS_thread with tattr attributes which calls LaunchTRNSYSTimeLoop
        printf("\n\n ---------------Creating TRNSYS_thread\n");
        returnval = pthread_create(&TRNSYS_thread, &tattr, LaunchTRNSYSTimeLoop, (void *)trnsys_id);
        if (returnval){
            printf(" Error: return code from pthread_create() for TRNSYS_thread is %d\n", returnval);
            exit(-1);
        }
    }
    //Destroy the thread attributes object, since it is no longer needed
    returnval = pthread_attr_destroy(&tattr);
    if (returnval){
        printf(" Error: return code from pthread_attr_destroy() is %d\n", returnval);
        exit(-1);
    }
}

/*
 Main loads and initializes both TRNSYS and ESP-r and checks that simulation parameters are compatible,
 It creates threads for each program and launches ESP-r which in turn launches TRNSYS.
 It then waits for the threads to complete before terminating the program.
*/
int main (int argc, char *argv[])
{
//Initialize all global variables
    bSystemConverged = bSkipNextCheck = false;
    bCheckingConvergence = bPassingDataToTrnsys = bPassingDataToEspr = false;
    iConvergenceFailed = 0;

//Loading in & initialising ESP-r & TRNSYS
    if (!bBpsStandalone){// if not running only bps through the harmonizer (for testing purposes) load & initialize TRNSYS.
    	//Get the TRNSYS path.  Needed to load in all the dlls TRNSYS is dependent on.
    	string path;
    	path.assign(TrnsysDll);
    	//change all characters to upper case to search for TRNDll.dll in case people have
    	//used different cases in the harmonizer input file.
    	int length = path.length();
    	locale loc;
    	for(int i=0; i!=length ; ++i)
   	    {
            //Convert string to uppercase.
    	    path[i] =  toupper(path[i], loc);
    	}
    	int PathLength = path.find("TRNDLL.DLL")-1;
// Load & initialize TRNSYS DLL
        cout << "\n\n ---------------Loading TRNSYS Dll: " << TrnsysDll << "\n";
    	TRNSYS_dll.LoadDll((LPCSTR)TrnsysDll, PathLength);
    	// Initialise the TRNSYS DLL with the deck file
    	int len = strlen(TRNSYSdeckFile);
    	bTrnsysInitializing = true;
    	if (TRNSYS_dll.InitializeTrnsys(TRNSYSdeckFile, len) != 0){
    		printf("Problem with TRNSYS - terminating....");
    	    return (0);    	}

    }

    int len;
// Load & initialize ESP-r dll
    cout << "\n\n ---------------Loading ESP-r Dll: " << bpsDll << "\n";
    ESPr_dll = LoadLibraryA((LPCSTR)bpsDll);
    // Check we have a valid handle to the dll
    int err = GetLastError();
    if(!ESPr_dll){
        printf(" Could not find %s.  Error: %d:\n",bpsDll, err);
    }
    else{
    	printf(" Success loading BPS dll.\n");
    }
    // Get the address of the initialise subroutine in the ESP-r dll
    STRFNPTR InitialiseESPr =  (STRFNPTR)(GetProcAddress(ESPr_dll, (LPCSTR)"passincommandline_"));
    // Use the subroutine passincommandline_ to pass the parsed command line to ESP-r.
    if (InitialiseESPr){
        len = strlen(bpsCommandLine);
        InitialiseESPr(&bpsCommandLineArg,bpsCommandLine, &len);
        printf(" ESPr initialised.\n");
    }
    else{
        printf(" Could not initialise ESPr.\n");
    }

// Create and launch threads for both ESP-r and TRNSYSs simulation time loops
    CreateSimulationThread(ESPr_THREAD);


    pthread_t ThreadIDs[N_THREADS];
    ThreadIDs[ESPr_THREAD] = ESPr_thread;
    ThreadIDs[TRNSYS_THREAD] = TRNSYS_thread;
    void *status; //return status

// Wait for threads to terminate
    for (int i=0; i<N_THREADS; i++){
        pthread_join(ThreadIDs[i], &status);
        printf("Joined with thread %d; returned value was %d.\n",ThreadIDs[i], (int) status);
        //Free memory allocated by thread
        free(status);
     }
    //free memory from convergence arrays
    if (pESPrConvData){
    	delete [] pESPrConvData;
    }
    if (pTrnsysConvData){
    	delete [] pTrnsysConvData;
    }
    float percentagetime = (float)(100.0*iConvergenceFailed/EsprData.ESPrTimestep);
    printf("Co-simulation failed to converge at %d timesteps (%4.4f%% of the time).\n",iConvergenceFailed, percentagetime);

//terminate calling thread
    pthread_exit(NULL);
    return (0);
}




///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Functions called by TRNSYS
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*
 GETESPRDATA is called by TRNSYS to retrieve the data passed by ESP-r and stored in the harmonizer
 in EsprData.  The function will only return once the data has been retrieved which will only happen once
 'Ready' is set true (i.e. once ESP-r has made that data available)
 Once TRNSYS has received the data, the flag bPassingDataToTrnsys is set to allow PASSDATATOESPR to proceed.
*/
extern "C"  //ensures the function name in the dll isn't mangled
void GETESPRDATA(d_EsprTrnsysData* pEsprData)
{
//	printf("in GETESPRDATA\n");
    if (bTrnsysInitializing){
        return;
    }
    bool done = false;
    while (!done)
    {
        //wait for the data to be set in PASSDATATOTRNSYS
        if (EsprData.Ready){
        	//copy the ESP-r data out (into TRNSYS's data structure)
            pEsprData->CopyEsprData(nHCC_to_Trnsys, nACC_to_Trnsys, nZONES, &EsprData);
            //quit this loop once the data is copied
            done = true;
            //set the EsprData.Ready to false to prevent GETESPRDATA from copying out any future data
            //when there is no new data to copy
            EsprData.Ready = false;
        }
    }
    //set bPassingDataToTrnsys to false to allow PASSDATATOESPR to proceed
	bPassingDataToTrnsys = false;
}

/*
PASSDATATOESPR is called by TRNSYS to pass the derived data structure to the harmonizer,
where it is copied to the harmonizers global variable EsprData casting all doubles to floats,
so that ESP-r can take those values and work with them directly (as all ESP-r real variables
are single precision).  Once the data has been copied the TrnsysData.Ready variable is set to
true which allows GETTRNSYSDATA to continue.  (Thus GETTRNSYSDATA must wait for the data copying
in PASSDATATOESPR to be complete).
The order of calls is:
1.PASSDATATOTRNSYS (except in the first call, waits for GETTRNSYSDATA to complete)
2.GETESPRDATA (waits for PASSDATATOTRNSYS)
3.PASSDATATOESPR (waits for GETESPRDATA)
4.GETTRNSYSDATA (waits for PASSDATATOESPR)
PASSDATATOESPR is forced to wait (using the bPassingDataToTrnsys flag)for data to be copied from ESP-r to the harmonizr and from the harmonizer
to TRNSYS before TRNSYS can pass its data into the harmonizer (this ensures ESP-r and TRNSYS remain synchronized)
*/

extern "C"  //ensures the function name in the dll isn't mangled
void PASSDATATOESPR(const d_EsprTrnsysData* pTrnsysData)
{
//    printf("in PASSDATATOESPR\n");
	while (bPassingDataToTrnsys){
		//wait for GETESPRDATA to complete
	}
	bPassingDataToEspr = true;
    if (bTrnsysInitializing){
//        printf("Initialising in PASSDATATOESPR\n");
    	//PASSDATATOESPR is called during TRNSYS initaialization but no data is passed.
        bPassingDataToEspr = false;
        return;
    }
    //copy the derived data structure into the harmonizer casting all doubles to floats
    for (int i=0;i<nHCC_to_ESPr;i++){
    	TrnsysData.HCC_to_ESPr[i].Flowrate = (float)pTrnsysData->HCC_to_ESPr[i].Flowrate;
    	TrnsysData.HCC_to_ESPr[i].Temperature = (float)pTrnsysData->HCC_to_ESPr[i].Temperature;
//        printf("ts=%d, i=%d, Flow=%4.4f, Temp=%4.4f\n", pTrnsysData->TrnsysTimestep, i, pTrnsysData->HCC_to_ESPr[i].Flowrate, pTrnsysData->HCC_to_ESPr[i].Temperature);

       	//copy ESP-r boundary conditions for future convergence checking.
       	pTrnsysConvData[EsprData.Invocations].HCC_to_ESPr[i].Temperature = pTrnsysData->HCC_to_ESPr[i].Temperature;
       	pTrnsysConvData[EsprData.Invocations].HCC_to_ESPr[i].Flowrate = pTrnsysData->HCC_to_ESPr[i].Flowrate;
    }
    for (int i=0;i<nACC_to_ESPr;i++){
    	TrnsysData.ACC_to_ESPr[i].Flowrate = (float)pTrnsysData->ACC_to_ESPr[i].Flowrate;
    	TrnsysData.ACC_to_ESPr[i].Temperature = (float)pTrnsysData->ACC_to_ESPr[i].Temperature;
    	TrnsysData.ACC_to_ESPr[i].Moisture_flowrate = (float)pTrnsysData->ACC_to_ESPr[i].Moisture_flowrate;
       	//copy ESP-r boundary conditions for future convergence checking.
       	pTrnsysConvData[EsprData.Invocations].ACC_to_ESPr[i].Temperature = pTrnsysData->ACC_to_ESPr[i].Temperature;
       	pTrnsysConvData[EsprData.Invocations].ACC_to_ESPr[i].Flowrate = pTrnsysData->ACC_to_ESPr[i].Flowrate;
       	pTrnsysConvData[EsprData.Invocations].ACC_to_ESPr[i].Moisture_flowrate = pTrnsysData->ACC_to_ESPr[i].Moisture_flowrate;
    }
    //Only pass the data that needs to go to ESP-r
    for (int i=0;i<nZONES;i++){
    	TrnsysData.ESPrZonesData[i].AirPointCasualGains = (float)pTrnsysData->ESPrZonesData[i].AirPointCasualGains;
    }
   	TrnsysData.Invocations = pTrnsysData->Invocations;
    TrnsysData.TrnsysConverged = pTrnsysData->TrnsysConverged;
    TrnsysData.ESPrTimestep = pTrnsysData->ESPrTimestep;
    TrnsysData.TrnsysTimestep = pTrnsysData->TrnsysTimestep;
   	TrnsysData.TotalTrnsysIterations = pTrnsysData->TotalTrnsysIterations;
    TrnsysData.TrnsysCode = pTrnsysData->TrnsysCode;
    //allow GETTRNSYSDATA data to proceed
    TrnsysData.Ready = true;
}



///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Functions called by ESP-r
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/*
SETARRAYSIZES sets the sizes of each of the arrays contained in the data structure passed
between ESP-r and TRNSYS, so that when data is copied it is done so efficiently (i.e. only for the number of arrays that
actually contain data. It would have been nicer to use allocatable arrays but they didn't port well to C (I think this
may be possible with fortran 2003 - so might be something for the future.....
 */
extern "C"  //ensures the function name in the dll isn't mangled
void SETARRAYSIZES(int* pSizes)
{
	nHCC_to_Trnsys = pSizes[HCC_TO_TRNSYS_ARRAY_POS];
	nHCC_to_ESPr = pSizes[HCC_TO_ESPR_ARRAY_POS];
	nACC_to_Trnsys = pSizes[ACC_TO_TRNSYS_ARRAY_POS];
	nACC_to_ESPr = pSizes[ACC_TO_ESPR_ARRAY_POS];
	nZONES = pSizes[NUMBER_OF_ZONES_ARRAY_POS];
    cout << "\n\n ---------------Setting array sizes.\n";
	printf(" HCCs sending (%d)\n",nHCC_to_Trnsys);
	printf(" HCCs receiving (%d)\n",nHCC_to_ESPr);
	printf(" ACCs sending (%d)\n",nACC_to_Trnsys);
	printf(" ACCs receiving (%d)\n",nACC_to_ESPr);
	printf(" Zones (%d)\n",nZONES);
}
/*
 GETTRNSYSDATA is called by bps to retrieve the data passed by TRNSYS and stored in the harmonizer
 in TrnsysData.  The function will only return once the data has been retrieved which will only happen once
 'Ready' is set true (i.e. once TRNSYS has made that data available)
Once ESP-r has received the data, the flag bPassingDataToESP-r is set to allow PASSDATATOTRNSYS to proceed.
*/
extern "C"  //ensures the function name in the dll isn't mangled
void GETTRNSYSDATA(f_EsprTrnsysData* pTrnsysData)
{
//	printf("in GETTRNSYSDATA\n");
    bool done = false;
    while (!done)
    {
        //wait for the data to be set in PASSDATATOESPR
        if (TrnsysData.Ready){
        	//copy the TRNSYS data out (into ESP-r's data structure)
        	pTrnsysData->CopyTrnsysData(nHCC_to_ESPr, nACC_to_ESPr, nZONES, &TrnsysData);
            //quit this loop once the data is copied
            done = true;
            //set the TrnsysData.Ready to false to prevent GETTRNSYSDATA from copying out any future data
            //when there is no new data to copy
            TrnsysData.Ready = false;
        }
    }
    //set bPassingDataToEspr to false to allow PASSDATATOTRNSYS to proceed
	bPassingDataToEspr = false;
}

/*
PASSDATATOTRNSYS is called by ESP-r to pass the derived data structure to the harmonizer,
where it is copied to the harmonizers global variable EsprData casting all floats to doubles,
so that TRNSYS can take those values and work with them directly (as all TRNSYS real variables
are double precision).  Once the data has been copied the EsprData.Ready variable is set to
true which allows GETESPRDATA to continue.  (Thus GETESPRDATA must wait for the data copying
in PASSDATATOTRNSYS to be complete).
The order of calls is:
1.PASSDATATOTRNSYS (except in the first call, waits for GETTRNSYSDATA to complete)
2.GETESPRDATA (waits for PASSDATATOTRNSYS)
3.PASSDATATOESPR (waits for GETESPRDATA)
4.GETTRNSYSDATA (waits for PASSDATATOESPR)
PASSDATATOTRNSYS is forced to wait (using the bPassingDataToEspr flag)for data to be copied from TRNSYS to the harmonizr and from the harmonizer
to ESP-r before ESP-r can pass its data into the harmonizer (this ensures ESP-r and TRNSYS remain synchronized)
*/
extern "C"  //ensures the function name in the dll isn't mangled
void PASSDATATOTRNSYS(const f_EsprTrnsysData* pEsprData)
{
//   printf("in PASSDATATOTRNSYS\n");
	while (bPassingDataToEspr){//initialised to false so that the first time this is called it does not wait.
		//wait for GETTRNSYSDATA to complete
//		printf("waiting for bPassingDataToEspr");
	}
	//force PASSDATATOESPR to wait for data to be copied from ESP-r to the harmonizr and from the harmonizer
	//to TRNSYS before TRNSYS can pass its data into the harmonizer (this ensures ESP-r and TRNSYS remain synchronized)
	bPassingDataToTrnsys = true;
    if (bTrnsysInitializing){
        bTrnsysInitializing = false;
//        printf("TrnsysInitializing in PASSDATATOTRNSYS\n");
        CreateSimulationThread(TRNSYS_THREAD);
        CheckCoSimValidity();
    }
    //copy the derived data structure into the harmonizer casting all floats to doubles
    for (int i=0;i<nHCC_to_Trnsys;i++){
    	EsprData.HCC_to_Trnsys[i].Flowrate = (double)pEsprData->HCC_to_Trnsys[i].Flowrate;
    	EsprData.HCC_to_Trnsys[i].Temperature = (double)pEsprData->HCC_to_Trnsys[i].Temperature;
    	EsprData.HCC_to_Trnsys[i].Moisture_flowrate = (double)pEsprData->HCC_to_Trnsys[i].Moisture_flowrate;
    	//copy ESP-r boundary conditions for future convergence checking.
    	pESPrConvData[pEsprData->Invocations].HCC_to_Trnsys[i].Temperature = pEsprData->HCC_to_Trnsys[i].Temperature;
    	pESPrConvData[pEsprData->Invocations].HCC_to_Trnsys[i].Flowrate = pEsprData->HCC_to_Trnsys[i].Flowrate;
    }
    for (int i=0;i<nACC_to_Trnsys;i++){
    	EsprData.ACC_to_Trnsys[i].Flowrate = (double)pEsprData->ACC_to_Trnsys[i].Flowrate;
    	EsprData.ACC_to_Trnsys[i].Temperature = (double)pEsprData->ACC_to_Trnsys[i].Temperature;
    	EsprData.ACC_to_Trnsys[i].Moisture_flowrate = (double)pEsprData->ACC_to_Trnsys[i].Moisture_flowrate;
    	//copy ESP-r boundary conditions for future convergence checking.
    	pESPrConvData[pEsprData->Invocations].ACC_to_Trnsys[i].Temperature = pEsprData->ACC_to_Trnsys[i].Temperature;
    	pESPrConvData[pEsprData->Invocations].ACC_to_Trnsys[i].Flowrate = pEsprData->ACC_to_Trnsys[i].Flowrate;
    	pESPrConvData[pEsprData->Invocations].ACC_to_Trnsys[i].Moisture_flowrate = pEsprData->ACC_to_Trnsys[i].Moisture_flowrate;
    }
    //Only pass the data that needs to go to TRNSYS
    for (int i=0;i<nZONES;i++){
    	EsprData.ESPrZonesData[i].AirPointTemperatures = (double)pEsprData->ESPrZonesData[i].AirPointTemperatures;
    	EsprData.ESPrZonesData[i].AirPointHumidities = (double)pEsprData->ESPrZonesData[i].AirPointHumidities;
    }
   	EsprData.Invocations = pEsprData->Invocations;
    EsprData.ESPrConverged = pEsprData->ESPrConverged;
    EsprData.ESPrTimestep = pEsprData->ESPrTimestep;
   	EsprData.TotalESPrIterations = pEsprData->TotalESPrIterations;

    //allow GETESPRDATA data to proceed
    EsprData.Ready = true;
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Convergence control
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/* CheckForZeroFlow checks for a zero flowrate condition which results in a division by zero in evaluation flow convergence and
 * renders checking temperature convergence as meaningless.*/
bool CheckForZeroFlow(void)
{
	//If this is the first invocation return false as we can only check convergence with 2 or more invocations.
	if (EsprData.Invocations < MIN_INVOCATIONS)
		return false;
	for (int i=0;i<nHCC_to_Trnsys;i++){
		if ((pESPrConvData[EsprData.Invocations].HCC_to_Trnsys[i].Flowrate +
				 pESPrConvData[EsprData.Invocations-1].HCC_to_Trnsys[i].Flowrate)== 0.0)
			bZeroFlow[HCC_TO_TRNSYS_ARRAY_POS][i]=true;
		else
			bZeroFlow[HCC_TO_TRNSYS_ARRAY_POS][i]=false;
	}
	for (int i=0;i<nHCC_to_ESPr;i++){
		if ((pTrnsysConvData[EsprData.Invocations].HCC_to_ESPr[i].Flowrate +
				 pTrnsysConvData[EsprData.Invocations-1].HCC_to_ESPr[i].Flowrate)== 0.0)
			bZeroFlow[HCC_TO_ESPR_ARRAY_POS][i]=true;
		else
			bZeroFlow[HCC_TO_ESPR_ARRAY_POS][i]=false;
	}
	for (int i=0;i<nACC_to_Trnsys;i++){
		if ((pESPrConvData[EsprData.Invocations].ACC_to_Trnsys[i].Flowrate +
				 pESPrConvData[EsprData.Invocations-1].ACC_to_Trnsys[i].Flowrate)== 0.0)
			bZeroFlow[ACC_TO_TRNSYS_ARRAY_POS][i]=true;
		else
			bZeroFlow[ACC_TO_TRNSYS_ARRAY_POS][i]=false;
	}
	for (int i=0;i<nACC_to_ESPr;i++){
		if ((pTrnsysConvData[EsprData.Invocations].ACC_to_ESPr[i].Flowrate +
		    pTrnsysConvData[EsprData.Invocations-1].ACC_to_ESPr[i].Flowrate)== 0.0)
			bZeroFlow[ACC_TO_ESPR_ARRAY_POS][i]=true;
		else
			bZeroFlow[ACC_TO_ESPR_ARRAY_POS][i]=false;
	}
	return true;
}
/* CheckTemperatureConvergence ensures temperatures stabilise within the convergence tolerance fTemperatureConvergence (degrees)*/
bool CheckTemperatureConvergence(void)
{
	if (EsprData.Invocations >= iMaxInvocations)
	{
		printf("Temperatures for Timestep: %d Invocation: %d ESP-rIteration: %d TRnsysIteration: %d fTemperatureConvergence: %4.4f \n",
    	    EsprData.ESPrTimestep,
    	   	EsprData.Invocations,
    	   	EsprData.TotalESPrIterations,
    	   	EsprData.TotalTrnsysIterations,
    	   	fTemperatureConvergence);
		for (int i=1;i<=iMaxInvocations;i++){
			for (int j=0;j<nHCC_to_Trnsys;j++){
				printf("HCC_to_TRNSYS%d %4.4f %4.4f result=%4.4f \n",
					j,
					pESPrConvData[i].HCC_to_Trnsys[j].Temperature,
					pESPrConvData[i-1].HCC_to_Trnsys[j].Temperature,
					fabs(pESPrConvData[i].HCC_to_Trnsys[j].Temperature - pESPrConvData[i-1].HCC_to_Trnsys[j].Temperature));
			}
		}
		for (int i=1;i<=iMaxInvocations;i++){
			for (int j=0;j<nHCC_to_ESPr;j++){
				printf("HCC_to_ESPr%d %4.4f %4.4f result=%4.4f \n",
					j,
					pTrnsysConvData[i].HCC_to_ESPr[j].Temperature ,
					pTrnsysConvData[i-1].HCC_to_ESPr[j].Temperature,
					fabs(pTrnsysConvData[i].HCC_to_ESPr[j].Temperature - pTrnsysConvData[i-1].HCC_to_ESPr[j].Temperature));
			}
		}
		for (int i=1;i<=iMaxInvocations;i++){
			for (int j=0;j<nACC_to_Trnsys;j++){
				printf("ACC_to_TRNSYS%d %4.4f %4.4f result=%4.4f \n",
					j,
					pESPrConvData[i].ACC_to_Trnsys[j].Temperature,
					pESPrConvData[i-1].ACC_to_Trnsys[j].Temperature,
					fabs(pESPrConvData[i].ACC_to_Trnsys[j].Temperature - pESPrConvData[i-1].ACC_to_Trnsys[j].Temperature));
			}
		}
		for (int i=1;i<=iMaxInvocations;i++){
			for (int j=0;j<nACC_to_ESPr;j++){
				printf("ACC_to_ESPr%d %4.4f %4.4f result=%4.4f \n",
					j,
					pTrnsysConvData[i].ACC_to_ESPr[j].Temperature ,
					pTrnsysConvData[i-1].ACC_to_ESPr[j].Temperature,
					fabs(pTrnsysConvData[i].ACC_to_ESPr[j].Temperature - pTrnsysConvData[i-1].ACC_to_ESPr[j].Temperature));
			}
		}
		// Didn't converge but move on with Simulation
		return true;
	}
	//If this is the first invocation return false as we can only check convergence with 2 or more invocations.
	if (EsprData.Invocations < MIN_INVOCATIONS)
		return false;
	float result;
	//for all HCC-S components check convergence of ESP-r data
	for (int i=0;i<nHCC_to_Trnsys;i++){
		//check for zero flow.
		if (bZeroFlow[HCC_TO_TRNSYS_ARRAY_POS][i])
			result = fTemperatureConvergence-1;//under zero flow consider data converged
		else
		    //check to see if data in ESP-r between this invocation and the previous invocation has changed.
			result = fabs(pESPrConvData[EsprData.Invocations].HCC_to_Trnsys[i].Temperature -
				      pESPrConvData[EsprData.Invocations-1].HCC_to_Trnsys[i].Temperature);

	    if (result > fTemperatureConvergence){
//			printf("returning false for TemperatureConvergence nHCC_to_Trnsys %d result=%4.4f Timestep:%d Invocation:%d\n",i,result,EsprData.ESPrTimestep,EsprData.Invocations);
			return false;
	    }
	}
	//for all HCC-R components check convergence of TRNSYS data
	for (int i=0;i<nHCC_to_ESPr;i++){
		//check for zero flow.
		if (bZeroFlow[HCC_TO_ESPR_ARRAY_POS][i])
			result = fTemperatureConvergence-1;//under zero flow consider data converged
		else
			result = fabs(pTrnsysConvData[EsprData.Invocations].HCC_to_ESPr[i].Temperature -
					pTrnsysConvData[EsprData.Invocations-1].HCC_to_ESPr[i].Temperature);
		if (result > fTemperatureConvergence){
//			printf("returning false for TemperatureConvergence nHCC_to_ESPr %d result=%4.4f  Timestep:%d Invocation:%d\n",i,result,EsprData.ESPrTimestep,EsprData.Invocations);
			return false;
		}
	}
	//for all ACC-S components check convergence of ESP-r data
	for (int i=0;i<nACC_to_Trnsys;i++){
		//check for zero flow.
		if (bZeroFlow[ACC_TO_TRNSYS_ARRAY_POS][i])
			result = fTemperatureConvergence-1;//under zero flow consider data converged
		else
			//check to see if data in ESP-r between this invocation and the previous invocation has changed.
			result = fabs(pESPrConvData[EsprData.Invocations].ACC_to_Trnsys[i].Temperature -
					  pESPrConvData[EsprData.Invocations-1].ACC_to_Trnsys[i].Temperature);
		if (result > fTemperatureConvergence){
//			printf("returning false for TemperatureConvergence nACC_to_Trnsys %d result=%4.4f  Timestep:%d Invocation:%d\n",i,result,EsprData.ESPrTimestep,EsprData.Invocations);
			return false;
		}
	}
	//for all ACC-R components check convergence of TRNSYS data
	for (int i=0;i<nACC_to_ESPr;i++){
		//check for zero flow.
		if (bZeroFlow[ACC_TO_ESPR_ARRAY_POS][i])
			result = fTemperatureConvergence-1;//under zero flow consider data converged
		else
			result = fabs(pTrnsysConvData[EsprData.Invocations].ACC_to_ESPr[i].Temperature -
					pTrnsysConvData[EsprData.Invocations-1].ACC_to_ESPr[i].Temperature);
		if (result > fTemperatureConvergence){
//			printf("returning false for TemperatureConvergence nACC_to_ESPr %d result=%4.4f  Timestep:%d Invocation:%d\n",i,result,EsprData.ESPrTimestep,EsprData.Invocations);
			return false;
		}
	}
	return true;
}




/* CheckFlowrateConvergence ensures flow rates stabilise within the convergence tolerance for flowrate (fFlowConvergence)*/
bool CheckFlowrateConvergence(void)
{
	if (EsprData.Invocations >= iMaxInvocations)
	{
		printf("Flow-rates for Timestep: %d Invocation: %d ESP-rIteration: %d TRnsysIteration: %d fFlowrateConvergence: %4.4f \n",
    	    EsprData.ESPrTimestep,
    	   	EsprData.Invocations,
    	   	EsprData.TotalESPrIterations,
    	   	EsprData.TotalTrnsysIterations,
    	   	fFlowConvergence);
		for (int i=1;i<=iMaxInvocations;i++){
			for (int j=0;j<nHCC_to_Trnsys;j++){
				printf("HCC_to_TRNSYS%d %4.4f %4.4f result=%4.4f \n",
					j,
					pESPrConvData[i].HCC_to_Trnsys[j].Flowrate,
					pESPrConvData[i-1].HCC_to_Trnsys[j].Flowrate,
					fabs(pESPrConvData[i].HCC_to_Trnsys[j].Flowrate - pESPrConvData[i-1].HCC_to_Trnsys[j].Flowrate));
			}
		}
		for (int i=1;i<=iMaxInvocations;i++){
			for (int j=0;j<nHCC_to_ESPr;j++){
				printf("HCC_to_ESPr%d %4.4f %4.4f result=%4.4f \n",
					j,
					pTrnsysConvData[i].HCC_to_ESPr[j].Flowrate ,
					pTrnsysConvData[i-1].HCC_to_ESPr[j].Flowrate,
					fabs(pTrnsysConvData[i].HCC_to_ESPr[j].Flowrate - pTrnsysConvData[i-1].HCC_to_ESPr[j].Flowrate));
			}
		}
		for (int i=1;i<=iMaxInvocations;i++){
			for (int j=0;j<nACC_to_Trnsys;j++){
				printf("ACC_to_TRNSYS%d %4.4f %4.4f result=%4.4f \n",
					j,
					pESPrConvData[i].ACC_to_Trnsys[j].Flowrate,
					pESPrConvData[i-1].ACC_to_Trnsys[j].Flowrate,
					fabs(pESPrConvData[i].ACC_to_Trnsys[j].Flowrate - pESPrConvData[i-1].ACC_to_Trnsys[j].Flowrate));
			}
		}
		for (int i=1;i<=iMaxInvocations;i++){
			for (int j=0;j<nACC_to_ESPr;j++){
				printf("ACC_to_ESPr%d %4.4f %4.4f result=%4.4f \n",
					j,
					pTrnsysConvData[i].ACC_to_ESPr[j].Flowrate ,
					pTrnsysConvData[i-1].ACC_to_ESPr[j].Flowrate,
					fabs(pTrnsysConvData[i].ACC_to_ESPr[j].Flowrate - pTrnsysConvData[i-1].ACC_to_ESPr[j].Flowrate));
			}
		}
		// Didn't converge but move on with Simulation
		return true;
	}
	float diff;
	float ave;
	float result;
	//for all HCC-S components check convergence of ESP-r data
	for (int i=0;i<nHCC_to_Trnsys;i++){
		//check for zero flow.
		if (bZeroFlow[HCC_TO_TRNSYS_ARRAY_POS][i])
			result = 0.0;//under zero flow consider data converged
		else{
		    //check to see if data in ESP-r between this invocation and the previous invocation has changed.
			diff = fabs(pESPrConvData[EsprData.Invocations].HCC_to_Trnsys[i].Flowrate -
	                    pESPrConvData[EsprData.Invocations-1].HCC_to_Trnsys[i].Flowrate);
	        ave = (pESPrConvData[EsprData.Invocations].HCC_to_Trnsys[i].Flowrate +
			       pESPrConvData[EsprData.Invocations-1].HCC_to_Trnsys[i].Flowrate)/2;
			result = 100*(diff/ave);
		}
		if (result > fFlowConvergence){
			return false;
		}
	}
	//for all HCC-R components check convergence of TRNSYS data
	for (int i=0;i<nHCC_to_ESPr;i++){
		//check for zero flow.
		if (bZeroFlow[HCC_TO_ESPR_ARRAY_POS][i])
			result = 0.0;//under zero flow consider data converged
		else{
			//check to see if data in ESP-r between this invocation and the previous invocation has changed.
			diff = fabs(pTrnsysConvData[EsprData.Invocations].HCC_to_ESPr[i].Flowrate -
					pTrnsysConvData[EsprData.Invocations-1].HCC_to_ESPr[i].Flowrate);
	        ave = (pTrnsysConvData[EsprData.Invocations].HCC_to_ESPr[i].Flowrate +
	        		pTrnsysConvData[EsprData.Invocations-1].HCC_to_ESPr[i].Flowrate)/2;
			result = 100*(diff/ave);
		}
		if (result > fFlowConvergence){
			return false;
		}
	}
	//for all ACC-S components check convergence of ESP-r data
	for (int i=0;i<nACC_to_Trnsys;i++){
		//check for zero flow.
		if (bZeroFlow[ACC_TO_TRNSYS_ARRAY_POS][i])
			result = 0.0;//under zero flow consider data converged
		else{
			//check to see if data in ESP-r between this invocation and the previous invocation has changed.
			diff = fabs(pESPrConvData[EsprData.Invocations].ACC_to_Trnsys[i].Flowrate -
	                    pESPrConvData[EsprData.Invocations-1].ACC_to_Trnsys[i].Flowrate);
	        ave = (pESPrConvData[EsprData.Invocations].ACC_to_Trnsys[i].Flowrate +
			       pESPrConvData[EsprData.Invocations-1].ACC_to_Trnsys[i].Flowrate)/2;
			result = 100*(diff/ave);
		}
		if (result > fFlowConvergence){
			return false;
		}
	}
	//for all ACC-R components check convergence of TRNSYS data
	for (int i=0;i<nACC_to_ESPr;i++){
		//check for zero flow.
		if (bZeroFlow[ACC_TO_ESPR_ARRAY_POS][i])
			result = 0.0;//under zero flow consider data converged
		else{
			//check to see if data in ESP-r between this invocation and the previous invocation has changed.
			diff = fabs(pTrnsysConvData[EsprData.Invocations].ACC_to_ESPr[i].Flowrate -
					pTrnsysConvData[EsprData.Invocations-1].ACC_to_ESPr[i].Flowrate);
	        ave = (pTrnsysConvData[EsprData.Invocations].ACC_to_ESPr[i].Flowrate +
	        		pTrnsysConvData[EsprData.Invocations-1].ACC_to_ESPr[i].Flowrate)/2;
			result = 100*(diff/ave);
		}
		if (result > fFlowConvergence){
			return false;
		}
	}
	return true;
}


/* CheckMoistureFlowConvergence ensures moisture flows stabilise within the convergence tolerance for moisture flowrate (fMoistureFlowConvergence)*/
bool CheckMoistureFlowConvergence(void)
{
	if (EsprData.Invocations >= iMaxInvocations)
	{
		printf("Moisture Flow-rates for Timestep: %d Invocation: %d ESP-rIteration: %d TRnsysIteration: %d fMoisture_flowrateConvergence: %4.4f \n",
    	    EsprData.ESPrTimestep,
    	   	EsprData.Invocations,
    	   	EsprData.TotalESPrIterations,
    	   	EsprData.TotalTrnsysIterations,
    	   	fMoistureFlowConvergence);
		for (int i=1;i<=iMaxInvocations;i++){
			for (int j=0;j<nACC_to_Trnsys;j++){
				printf("ACC_to_TRNSYS%d %4.4f %4.4f result=%4.4f \n",
					j,
					pESPrConvData[i].ACC_to_Trnsys[j].Moisture_flowrate,
					pESPrConvData[i-1].ACC_to_Trnsys[j].Moisture_flowrate,
					fabs(pESPrConvData[i].ACC_to_Trnsys[j].Moisture_flowrate - pESPrConvData[i-1].ACC_to_Trnsys[j].Moisture_flowrate));
			}
		}
		for (int i=1;i<=iMaxInvocations;i++){
			for (int j=0;j<nACC_to_ESPr;j++){
				printf("ACC_to_ESPr%d %4.4f %4.4f result=%4.4f \n",
					j,
					pTrnsysConvData[i].ACC_to_ESPr[j].Moisture_flowrate ,
					pTrnsysConvData[i-1].ACC_to_ESPr[j].Moisture_flowrate,
					fabs(pTrnsysConvData[i].ACC_to_ESPr[j].Moisture_flowrate - pTrnsysConvData[i-1].ACC_to_ESPr[j].Moisture_flowrate));
			}
		}
		// Didn't converge but move on with Simulation
		return true;
	}
	float result;
	float denominator;
	//for all ACC-S components check convergence of ESP-r data
	for (int i=0;i<nACC_to_Trnsys;i++){
		denominator = pESPrConvData[EsprData.Invocations].ACC_to_Trnsys[i].Moisture_flowrate +
				      pESPrConvData[EsprData.Invocations-1].ACC_to_Trnsys[i].Moisture_flowrate;
		if (denominator == 0.0)
			result = 0.0;
		else
			result = fabs(200*(pESPrConvData[EsprData.Invocations].ACC_to_Trnsys[i].Flowrate -
				      pESPrConvData[EsprData.Invocations-1].ACC_to_Trnsys[i].Flowrate))/denominator;
		if (result > fMoistureFlowConvergence){
            return false;
		}
	}
	//for all ACC-R components check convergence of TRNSYS data
	for (int i=0;i<nACC_to_ESPr;i++){
		denominator = pTrnsysConvData[EsprData.Invocations].ACC_to_ESPr[i].Moisture_flowrate +
				pTrnsysConvData[EsprData.Invocations-1].ACC_to_ESPr[i].Moisture_flowrate;
		if (denominator == 0.0)
			result = 0.0;
		else
			result = fabs(200*(pTrnsysConvData[EsprData.Invocations].ACC_to_ESPr[i].Flowrate -
					pTrnsysConvData[EsprData.Invocations-1].ACC_to_ESPr[i].Flowrate))/denominator;
		if (result > fMoistureFlowConvergence){
            return false;
		}
	}
	return true;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Functions called by both TRNSYS and ESP-r
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*
GETSYSTEMCONV is called by ESP-r and TRNSYS to see if both programs have converged.
A number of flags are used to ensure synchronisation and for efficiency reasons.
bCheckingConvergence is used to ensure ESP-r and TRNSYS are not calling GETSYSTEMCONV at the same time.
bSkipNextCheck       is used to prevent the duplication of checking for convergence.
                     If one program tested for convergence and it was not attained it will not be attained
                     by checking the same numbers twice, so there is no need for the other to check.
bSystemConverged     is used to prevent the duplication of checking for convergence when convergence has been attained,
					 there is no need to check for convergence again but GETSYSTEMCONV still needs to pass out a 'true'.
CheckTemperatureConvergence() && CheckFlowrateConvergence() are used to check for convergence.
*/

extern "C"  //ensures the function name in the dll isn't mangled
void GETSYSTEMCONV(int* epc)
{
	while (bCheckingConvergence)
	{
		//wait to make sure ESP-r and TRNSYS are not calling GETSYSTEMCONV at the same time.
	}
	bCheckingConvergence=true;
	//assume it will not converge.
    int Converged = 0;
/*This check is performed to ensure ESP-r and TRNSYS are executing the same timestep.
//It is actually not required now that the calling order is stable but it is a useful check for ensuring
//the data in derived data structure isn't corrupted when the DDS changes.
//This code can be commented out between testing changes in the DDS as it will only slow the co-simulation down
//by repeatedly checking for a condition which will always be false in a stable simulation.
    if (EsprData.ESPrTimestep != (TrnsysData.TrnsysTimestep-TrnsysStartTS)){
    	printf("*******************************************\n");
		printf("ESP-r timestep: %d TRNSYS timestep: %d (%d)\n", EsprData.ESPrTimestep, TrnsysData.TrnsysTimestep-TrnsysStartTS, TrnsysData.TrnsysTimestep);
    	printf("In the past this has been due to a problem with:\n");
    	printf("1. the DDS\n");
    	printf("2. ESP-r overriding the convergence (because an electrical network was present).\n");
		bCheckingConvergence=false;
	    *epc = Converged;
	    return;
    }*/
    //Can we skip checking convergence because the numbers didn't converge at the last check?
   	if (!bSkipNextCheck){
   		//Did the system converge at the last check?
   		if (bSystemConverged){//convergence was checked by the other program and has been attained so proceed to next timestep
            Converged = 1; //return true
            bSystemConverged = false; //reset flag
        }
        //check for convergence
        else {
        	/*Check for zero flowrate which would result in a division by zero in evaluating flow convergence and
        	  renders checking temperature convergence as meaningless.  Fails for the first invocation during zero flow*/
        	if (CheckForZeroFlow()){
				/*The order these checks are called is important in terms of speed in detecting a non convergence condition.
				CheckMoistureFlowConvergence() will converge when no ACC is present but this check is redundant if CheckTemperatureConvergence fails.
				if CheckTemperatureConvergence fails the other two will not be evaluated as all conditions won't be met.*/
				if ( CheckTemperatureConvergence() && CheckFlowrateConvergence() && CheckMoistureFlowConvergence()){
					Converged = 1; //return true
					/*Set bSystemConverged to true so when the next program calls GETSYSTEMCONV it doesn't need to check for
					  convergence to see if convergence has been reached (as this would slow things down unnecessarily.)*/
					bSystemConverged = true;  //set flag to prevent duplicated checking
					if (EsprData.Invocations >= iMaxInvocations)
					{
						printf("\n\n******************Did not converge at timestep %d\n",EsprData.ESPrTimestep);
						iConvergenceFailed++;
						Converged = 1; //return true
						/*Set bSystemConverged to true so when the next program calls GETSYSTEMCONV it doesn't need to
						  check for convergence to see if convergence has been reached (as this would slow things down unnecessarily.)*/
						bSkipNextCheck = false;
						bSystemConverged = true;  //set flag to prevent duplicated checking
					}
				}
				else{
					//If one program has checked for convergence there is no need for the other to check
					bSkipNextCheck = true;
				}
        	}
        }
   	}
   	//This check could be skipped but the subsequent one can not.
    else{
       bSkipNextCheck = false; //reset flag
    }
   	*epc = Converged;
   	bCheckingConvergence = false;
}








