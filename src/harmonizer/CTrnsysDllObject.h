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

 This file is CTrynsysDllObject.h contains the class definition for the TrnsysDllObject which acts like the
 TRNSYS executable (TRNExe) in that this class is used to initialize TRNSYS with a deck file, and to repeatedly call
 TRNSYS for each timestep for the duration of the simulation.  It is also reponsible for terminating the simulation.

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
*/

#pragma once
#include "windows.h"

// typedef is required to obtain a usable function pointer out of GetProcAddress().
// typedef is used to give the name FNPTR to a function pointer of type void which takes
// parameters ((int*, double*,double*,char*,char*,char*)
// The __stdcall calling convention is used to call Win32 API functions

typedef void (__stdcall *TRNSYSFNPTR)(int*, double*,double*,char*,char*,char*);

//In the Windows API the maximum length for a path is MAX_PATH, which is defined as 260 characters.
//A local path is structured in the following order: drive letter, colon, backslash,
//name components separated by backslashes, and a terminating null character. For example, the maximum path on drive D is
//"D:\some 256-character path string<NUL>" where "<NUL>" represents the invisible terminating null character for the current
//system codepage. (The characters < > are used here for visual clarity and cannot be part of a valid path string.)
#define MAX_CHAR 260

class CTrnsysDllObject
{
private:
    //The following variables are defined in TRNSYS source code .  Array sizes are defined there.
    double m_parout[1000];		// Set array passed to TRNSYS (not used)
    double m_plotout[1000];		// Set array passed to TRNSYS (not used)
    char m_labels[4000];		// Set character array passed to TRNSYS (not used)
    char m_titles[1500];		// Set character array passed to TRNSYS (not used)
    char m_deckn[300];			// Character array containing the name of the deck file
    /////////////////////////////////////////////////////////////////////////////////////////////
    HINSTANCE m_LibHandle;  	// Handel to dll
    char m_DLLPath[MAX_CHAR];	// Character array containing the path to the TRNSYS dll
    double m_StartTime;			// Simulation start time
    double m_StopTime;			// Simulation end time
    double m_TimeStep;			// Simulation timestep
    int m_nTimeSteps;			// Number of timesteps

//  TRNSYSFNPTR m_trnsysproc;

public:
    CTrnsysDllObject(void);
    ~CTrnsysDllObject(void);
    void LoadDll(LPCSTR dll, int length);
    int InitializeTrnsys(const char* deckFile, int length);
    void RunTrnsysSimulation(void);
    void TerminateTrnsysSimulation(void);
    //access functions
    double GetStartTime(void){return m_StartTime;}
    double GetStopTime(void){return m_StopTime;}
    double GetTimeStep(void){return m_TimeStep;}
    int GetnTimeSteps(void){return m_nTimeSteps;}
};

