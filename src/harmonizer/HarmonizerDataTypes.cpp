/*
 This file is part of the ESP-r system.
 Copyright Energy Systems Research Unit, University of
 Strathclyde, Glasgow Scotland, 2001.

 ESP-r and the harmonizer are free software.  You can redistribute
 them and/or modify them under the terms of the GNU General Public
 License as published by the Free Software Foundation
 (version 2 or later).

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

 This file is HarmonizerDataTypes.cpp and contains the implementation of the interface defined in HarmonizerDataTypes.h.


 This file contains two version of the same data structure, one comprising single precision real numbers (floats)
 and the other comprising double precision real numbers (doubles).  This is because the real data is
 single precision by default in ESP-r and double in TRNSYS.  All casting is done in the harmonizer (which is why
 it requires two versions of the same DDS) - they are differentiated by the prefixes f_ and d_.

 The functions CopyTrnsysData and CopyEsprData exist to prevent all data being copied between the programs and
 allow copying of only the data that is required by the receiving program to help minimize co-simulation times.

 Two additional data structures are defined d_ConvergenceParameters and  f_ConvergenceParameters, which are used
 to check convergence of the ESP-r and TRNSYS data respectively.


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// */
#include "HarmonizerDataTypes.h"


//f_EsprTrnsysData constructor - initialises all variables of the DDS to zero.
f_EsprTrnsysData::f_EsprTrnsysData(void)
{
	for (int i=0;i<MAX_FCCs;i++)
	{
		HCC_to_Trnsys[i].Flowrate=0.0;
		HCC_to_Trnsys[i].Temperature=0.0;
		HCC_to_Trnsys[i].Moisture_flowrate=0.0;

		HCC_to_ESPr[i].Flowrate=0.0;
		HCC_to_ESPr[i].Temperature=0.0;
		HCC_to_ESPr[i].Moisture_flowrate=0.0;

		ACC_to_Trnsys[i].Flowrate=0.0;
		ACC_to_Trnsys[i].Temperature=0.0;
		ACC_to_Trnsys[i].Moisture_flowrate=0.0;

		ACC_to_ESPr[i].Flowrate=0.0;
		ACC_to_ESPr[i].Temperature=0.0;
		ACC_to_ESPr[i].Moisture_flowrate=0.0;
	}
	for (int i=0;i<MAX_ZONES;i++)
	{
		ESPrZonesData[i].AirPointTemperatures=0.0;
		ESPrZonesData[i].AirPointHumidities=0.0;
		ESPrZonesData[i].AirPointCasualGains=0.0;
	}
	Invocations=0;
    ESPrConverged=0;
    TrnsysConverged=0;
    ESPrTimestep=0;
    TrnsysTimestep=0;
    TotalESPrIterations=0;
    TotalTrnsysIterations=0;
    TrnsysCode=0;
    Ready=false;
}

// CopyTrnsysData copies only the data (in the DDS) required by ESP-r
void f_EsprTrnsysData::CopyTrnsysData(const int nHCC_to_ESPr, const int nACC_to_ESPr, const int nZONES, const f_EsprTrnsysData* pTrnsysData)
{
	for (int i=0;i<nHCC_to_ESPr;i++){
		HCC_to_ESPr[i].Flowrate = pTrnsysData->HCC_to_ESPr[i].Flowrate;
		HCC_to_ESPr[i].Temperature = pTrnsysData->HCC_to_ESPr[i].Temperature;

	}
	for (int i=0;i<nACC_to_ESPr;i++){
		ACC_to_ESPr[i].Flowrate = pTrnsysData->ACC_to_ESPr[i].Flowrate;
		ACC_to_ESPr[i].Temperature = pTrnsysData->ACC_to_ESPr[i].Temperature;
		ACC_to_ESPr[i].Moisture_flowrate = pTrnsysData->ACC_to_ESPr[i].Moisture_flowrate;
	}
    for (int i=0;i<nZONES;i++){
    	ESPrZonesData[i].AirPointCasualGains = pTrnsysData->ESPrZonesData[i].AirPointCasualGains;
    }
    TotalTrnsysIterations=pTrnsysData->TotalTrnsysIterations;
}

//f_EsprTrnsysData destructor
f_EsprTrnsysData::~f_EsprTrnsysData(void)
{
}


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//d_EsprTrnsysData constructor - initialises all variables of the DDS to zero.
d_EsprTrnsysData::d_EsprTrnsysData(void)
{
	for (int i=0;i<MAX_FCCs;i++)
	{
		HCC_to_Trnsys[i].Flowrate=0.0;
		HCC_to_Trnsys[i].Temperature=0.0;
		HCC_to_Trnsys[i].Moisture_flowrate=0.0;

		HCC_to_ESPr[i].Flowrate=0.0;
		HCC_to_ESPr[i].Temperature=0.0;
		HCC_to_ESPr[i].Moisture_flowrate=0.0;

		ACC_to_Trnsys[i].Flowrate=0.0;
		ACC_to_Trnsys[i].Temperature=0.0;
		ACC_to_Trnsys[i].Moisture_flowrate=0.0;

		ACC_to_ESPr[i].Flowrate=0.0;
		ACC_to_ESPr[i].Temperature=0.0;
		ACC_to_ESPr[i].Moisture_flowrate=0.0;
	}
	for (int i=0;i<MAX_ZONES;i++)
	{
		ESPrZonesData[i].AirPointTemperatures=0.0;
		ESPrZonesData[i].AirPointHumidities=0.0;
		ESPrZonesData[i].AirPointCasualGains=0.0;
	}
	Invocations=0;
    ESPrConverged=0;
    TrnsysConverged=0;
    ESPrTimestep=0;
    TrnsysTimestep=0;
    TotalESPrIterations=0;
    TotalTrnsysIterations=0;
    TrnsysCode=0;
    Ready=false;
}

// CopyEsprData copies only the data (in the DDS) required by TRNSYS
void d_EsprTrnsysData::CopyEsprData(const int nHCC_to_Trnsys, const int nACC_to_Trnsys, const int nZONES,const d_EsprTrnsysData* pEsprData)
{
	//copy the ESP-r data out (into Trnsys's data structure)
    for (int i=0;i<nHCC_to_Trnsys;i++){
    	HCC_to_Trnsys[i].Flowrate = pEsprData->HCC_to_Trnsys[i].Flowrate;
    	HCC_to_Trnsys[i].Temperature = pEsprData->HCC_to_Trnsys[i].Temperature;

    }
    for (int i=0;i<nACC_to_Trnsys;i++){
    	ACC_to_Trnsys[i].Flowrate = pEsprData->ACC_to_Trnsys[i].Flowrate;
    	ACC_to_Trnsys[i].Temperature = pEsprData->ACC_to_Trnsys[i].Temperature;
    	ACC_to_Trnsys[i].Moisture_flowrate = pEsprData->ACC_to_Trnsys[i].Moisture_flowrate;
    }
    for (int i=0;i<nZONES;i++){
    	ESPrZonesData[i].AirPointTemperatures = pEsprData->ESPrZonesData[i].AirPointTemperatures;
    	ESPrZonesData[i].AirPointHumidities = pEsprData->ESPrZonesData[i].AirPointHumidities;

    }
}

//f_EsprTrnsysData destructor
d_EsprTrnsysData::~d_EsprTrnsysData(void)
{
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//TrnsysConvergenceData constructor - initialises all variables to zero.
TrnsysConvergenceData::TrnsysConvergenceData(void)
{
	for (int i=0;i<MAX_FCCs;i++)
	{
		HCC_to_ESPr[i].Flowrate=0.0;
		HCC_to_ESPr[i].Temperature=0.0;
		HCC_to_ESPr[i].Moisture_flowrate=0.0;

		ACC_to_ESPr[i].Flowrate=0.0;
		ACC_to_ESPr[i].Temperature=0.0;
		ACC_to_ESPr[i].Moisture_flowrate=0.0;
	}
}

//TrnsysConvergenceData destructor
TrnsysConvergenceData::~TrnsysConvergenceData(void)
{
}



////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//EsprConvergenceData constructor - initialises all variables to zero.
EsprConvergenceData::EsprConvergenceData(void)
{
	for (int i=0;i<MAX_FCCs;i++)
	{
		HCC_to_Trnsys[i].Flowrate=0.0;
		HCC_to_Trnsys[i].Temperature=0.0;
		HCC_to_Trnsys[i].Moisture_flowrate=0.0;


		ACC_to_Trnsys[i].Flowrate=0.0;
		ACC_to_Trnsys[i].Temperature=0.0;
		ACC_to_Trnsys[i].Moisture_flowrate=0.0;
	}
}


//EsprConvergenceData destructor
EsprConvergenceData::~EsprConvergenceData(void)
{
}


