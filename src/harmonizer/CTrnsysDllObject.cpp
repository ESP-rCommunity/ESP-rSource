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

 This file is CTrynsysDllObject.cpp contains the class implementation for the TrnsysDllObject which acts like the
 TRNSYS executable (TRNExe) in that this class is used to initialize TRNSYS with a deck file, and to repeatedly call
 TRNSYS for each timestep for the duration of the simulation.  It is also reponsible for terminating the simulation.
 LoadDll: 					loads the TRNSYS dll
 InitializeTrnsys: 			initialized TRNSYS with the deckfile (this must follow the call to LoadDll).
 	 	 	 	 	 		First the TRNSYS routine is called with an iCall value of 9000 to inform the TRNSYS
 	 	 	 	 	 		dll where all other dlls are.
 RunTrnsysSimulation: 		calls the TRNSYS routine with an iCall=1 to run the simulation.  This function acts
 	 	 	 	 	 		as the simulation clock for TRNSYS and is therefore called at every timestep
 	 	 	 	 	 		(provided no errors occur) , until the simulation period is complete.  One extra
 	 	 	 	 	 		call is made to ensure ESP-r is not waiting for data from TRNSYS.  Once the
 	 	 	 	 	 		simulation period has been successfully simulated the simulation is terminated.
 TerminateTrnsysSimulation:	terminates the TRNSYS simulation by calling the TRNSYS routine with an iCall value of -1,
 	 	 	 	 	 	 	which allows TRNSYS to perform any end of simulation tasks it needs.

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
*/

#include "CTrnsysDllObject.h"
#include <stdio.h>





//CTrnsysDllObject constructor - in which member variables are initialized
CTrnsysDllObject::CTrnsysDllObject(void)
{
    //initialise all member variables
    for (int i=0;i<1000;i++){m_parout[i] = m_plotout[i] = 0.0;}
    memset(m_titles,' ',sizeof(m_titles));
    memset(m_labels,' ',sizeof(m_labels));
    memset(m_deckn,' ',sizeof(m_deckn));
    memset(m_DLLPath,' ',sizeof(m_DLLPath));
}

//CTrnsysDllObject destructor
CTrnsysDllObject::~CTrnsysDllObject(void)
{
}

//LoadDll loads the TRNSYS dll
void CTrnsysDllObject::LoadDll(LPCSTR dll, int length)
{
	//copy dll path into m_DLLPath char array (needed to tell TRNSYS where the dlls are).
    if (length > 0){
		for (int i=0;i<length;i++)
		{
			m_DLLPath[i]=dll[i];
		}
		//  Load in the dll
		m_LibHandle = LoadLibraryA(dll);
		// Check we have a valid handle to the dll
		if(!m_LibHandle)
		{
			int err = GetLastError();
		    printf(" Could load dll %s Error: %d\n", dll, err);
		    exit(0);
		}
    }
    else{
    	printf(" The DLL path is not valid.  Could not find TRNDLL.DLL.\n");
    }
}


// InitializeTrnsys initialized TRNSYS with the deckfile (this must follow the call to LoadDll).
// First the TRNSYS routine is called with an iCall value of 9000 to inform the TRNSYS dll where all other dlls are.
int CTrnsysDllObject::InitializeTrnsys(const char* deckFile, int length)
{
    /*  Note to self:
    When TRNSYSFNPTR m_trnsysproc is declared in the header, the string m_deckn becomes corrupt - who knows why. */
    TRNSYSFNPTR trnsysproc = (TRNSYSFNPTR)GetProcAddress(m_LibHandle, (LPCSTR)"TRNSYS");
	/*the TRNSYS routine is called with iCall=9000 and the path to TRNSYS dlls m_DLLPath (e.g. "C:\TRNSYS\exe")
	*This tells the TRNSYS dll where to find all the other dlls it is dependent on.*/
    int iCall = 9000;//iCall set to 9000 tells TRNSYS to set the trnsys directory
    if (trnsysproc){
    	printf(" Success loading TRNSYS Dll.\n");
    	printf(" Setting TRNSYS DLL Path: %s \n", m_DLLPath);
    	//Set TRNSYS Dll path
        trnsysproc(&iCall,m_parout,m_plotout,m_labels,m_titles,m_DLLPath);// returns iCall = 9000 if no error occurs
        if (iCall == 9000) {// Initialisation call to TRNSYS
            //copy deckFile into m_dckn char array
        	memset(m_deckn,' ',sizeof(m_deckn));
        	for (int i=0;i<length;i++){
        		m_deckn[i]=deckFile[i];
        	}
        	//Set deck file (with iCall of 0)
        	iCall = 0;
        	trnsysproc(&iCall,m_parout,m_plotout,m_labels,m_titles,m_deckn);// returns iCall = 0 if no error occurs
        	if (iCall==0){
        		//store Simulation Start & Stop times & number of timesteps
        		m_StartTime = m_parout[0];
        		m_StopTime = m_parout[1];
        		m_TimeStep = 1.0/m_parout[2];
        		m_nTimeSteps = int(((m_StopTime-m_StartTime)*m_TimeStep)+0.5);
        		printf("\nSuccess loading TRNSYS deck file \n" );
        	}
        	else{
        		printf("\n **************Problem reading deck file************\n");
        		printf(" deck file: %s \niCall=%d\n", m_deckn, iCall);
        		printf("\n\n");
        	}
        }
        else{
    		printf("\n **************Problem setting TRNSYS DLL path************\n");
    		printf(" DLL Path: %s %d\n", m_DLLPath, iCall);
    		printf("\n\n");
        }
    }
    else{
    	int err = GetLastError();
        printf("\n Could not load TRNSYS DLL.\n Error: %d\n", err);
        return (999); //exiting with error code.
    }
    return iCall;
}

//RunTrnsysSimulation calls the TRNSYS routine with an iCall=1 to run the simulation.  This function acts as the simulation clock for TRNSYS
//and is therefore called at every timestep (provided no errors occur) , until the simulation period is complete.  One extra call is made
//to ensure ESP-r is not waiting for data from TRNSYS.  Once the simulation period has been successfully simulated the simulation is terminated.
void CTrnsysDllObject::RunTrnsysSimulation(void)
{
    TRNSYSFNPTR trnsysproc = (TRNSYSFNPTR)GetProcAddress(m_LibHandle, (LPCSTR)"TRNSYS");
    if (trnsysproc){
        //values in m_parout are received from the initialisation call to TRNSYS
        int iCall = 1;  // iCall set to 1 tells TRNSYS to run a simulation
        int nCall = 0;
        printf(" ---------------Running TRNSYS simulation\n");
        //run the simulation until there is either an error (iCall != 0) or the number of timesteps has been executed
        do{
            nCall++;
            iCall = 1;
            //run the simulation for this timestep
            trnsysproc(&iCall,m_parout,m_plotout,m_labels,m_titles,m_deckn);// returns iCall = 0 if no error occurs
        }while ((iCall == 0)&(nCall < m_nTimeSteps+1));
        printf("nCall: %d, m_nTimeSteps: %d\n",nCall,m_nTimeSteps);
        if (iCall!=0){
            printf("Error in TRNSYS simulation %d.\nCheck TRNSYS list file for details.",iCall);
            exit(0);
        }
        //Finish the simulation
        TerminateTrnsysSimulation();
    }
    else{
    	int err = GetLastError();
        printf("\n Could not load TRNSYS DLL.\n Error: %d\n", err);
    }
}
//TerminateTrnsysSimulation terminates the TRNSYS simulation by calling the TRNSYS routine with an iCall value of -1,
//which allows TRNSYS to perform any end of simulation tasks it needs.
void CTrnsysDllObject::TerminateTrnsysSimulation(void)
{
    int iCall = -1; // iCall set to -1 tells TRNSYS there will be no further simulations
    TRNSYSFNPTR trnsysproc = (TRNSYSFNPTR)GetProcAddress(m_LibHandle, (LPCSTR)"TRNSYS");
    if (trnsysproc){
        trnsysproc(&iCall,m_parout,m_plotout,m_labels,m_titles,m_deckn);// returns iCall = 0 if no error occurs
        printf("Finished TRNSYS simulation with iCall=%d\n", iCall);
    }
    else{
    	int err = GetLastError();
        printf("\n Could not load TRNSYS DLL.\n Error: %d\n", err);
    }
}







