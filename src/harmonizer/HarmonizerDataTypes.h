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

 This file is HarmonizerDataTypes.h contains the class definition for the derived data structure
 (EsprTrnsysData) used in passing data between ESP-r, TRNSYS and the harmonizer in co-simulation.

**********************************************************************************************************
**********************************************************************************************************
 WARNING: ESP-r and Trnsys compile data structures almost identical to this
 ESPrTRNSYSData.F (ESP-r) and CosimDataTypes.f90 (TRNSYS).  Differences in the DDS will corrupt the data being
 passed between the program.
 **********************************************************************************************************
**********************************************************************************************************

 This file contains two version of the same data structure, one comprising single precision real numbers (floats)
 and the other comprising double precision real numbers (doubles).  This is because the real data is
 single precision by default in ESP-r and double in TRNSYS.  All casting is done in the harmonizer (which is why
 it requires two versions of the same DDS) - they are differentiated by the prefixes f_ and d_.

 The functions CopyTrnsysData and CopyEsprData exist to prevent all data being copied between the programs and
 allow copying of only the data that is required by the receiving program to help minimize co-simulation times.

 Two additional data structures are defined d_ConvergenceParameters and  f_ConvergenceParameters, which are used
 to check convergence of the ESP-r and TRNSYS data respectively.

 */

#define MAX_FCCs 20  	// Maximum number of fluid coupling components (must be the same as in esprs "include/plant.h")
#define MAX_ECCs 10  	// Maximum number of electrical coupling components (must be the same as in esprs "include/plant.h")
#define MAX_ZONES 100	// Maximum number of zones.

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// This holds the DDS as floats because that is what is used in ESP-r
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

struct f_FluidCouplingComponent{
	float Flowrate;  			//kg/s
	float Temperature; 			//deg C
	float Moisture_flowrate;	//kg/s
};

struct f_ZoneData{
	float AirPointTemperatures;		// Zone Air Point Temperature
	float AirPointHumidities; 		// Zone Humidities
	float AirPointCasualGains;      // Casual gains to zone air-point (W)
};

struct f_ElectricalCouplingComponent{
    int Source;      	//Source=true->Power source, Source=false->Power sink
    float Power;		//W
};

class f_EsprTrnsysData
{
public:
	//note the order in which this data is positioned in the structure is critical
	//and must match that of ESPrTrnsysData.F. and  CosimDataTypes.f90
	f_FluidCouplingComponent HCC_to_Trnsys[MAX_FCCs];
	f_FluidCouplingComponent HCC_to_ESPr[MAX_FCCs];
	f_FluidCouplingComponent ACC_to_Trnsys[MAX_FCCs];
	f_FluidCouplingComponent ACC_to_ESPr[MAX_FCCs];
//	f_ElectricalCouplingComponent ECC[MAX_ECCs];
	f_ZoneData ESPrZonesData[MAX_ZONES];
	int Invocations;			//Number of program invocations
    int ESPrConverged;			//Flag for ESPr convergence (not used yet)
    int TrnsysConverged;		//Flag for TRNSYS convergence (notused yet)
    int ESPrTimestep;			//Number of ESP-r  timestep
    int TotalESPrIterations;    //Total Number of ESP-r iteration
    int TrnsysTimestep;			//Number of TRNSYS  timestep
    int TotalTrnsysIterations;  //Total Number of TRNSYS iteration
    int TrnsysCode;				//Flag for TRNSYS
    bool Ready;					//Flag used in the harmonizer to indicate that data copying is complete


    f_EsprTrnsysData(void);
    void CopyTrnsysData(const int nHCC_to_ESPr, const int nACC_to_ESPr, const int nZONES, const f_EsprTrnsysData* pTrnsysData);
    ~f_EsprTrnsysData(void);
};


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// This holds the DDS as doubles because that is what is used in TRNSYS
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


struct d_FluidCouplingComponent{
	double Flowrate;  			    //kg/s
	double Temperature; 			//deg C
	double Moisture_flowrate;
};

struct d_ZoneData{
	double AirPointTemperatures;
	double AirPointHumidities;
	double AirPointCasualGains;
};

struct d_ElectricalCouplingComponent{
    int Source;      	//Source=true->Power source, Source=false->Power sink
    double Power;		//W
};
class d_EsprTrnsysData
{
public:
	//note the order in which this data is positioned in the structure is critical
	//and must match that of ESPrTrnsysData.F. and CosimDataTypes.f90
	d_FluidCouplingComponent HCC_to_Trnsys[MAX_FCCs];
	d_FluidCouplingComponent HCC_to_ESPr[MAX_FCCs];
	d_FluidCouplingComponent ACC_to_Trnsys[MAX_FCCs];
	d_FluidCouplingComponent ACC_to_ESPr[MAX_FCCs];
//	d_ElectricalCouplingComponent ECC[MAX_ECCs];
	d_ZoneData ESPrZonesData[MAX_ZONES];
	int Invocations;			//Number of program invocations
    int ESPrConverged;			//Flag for ESPr convergence (not used yet)
    int TrnsysConverged;		//Flag for TRNSYS convergence (notused yet)
    int ESPrTimestep;			//Number of ESP-r  timestep
    int TotalESPrIterations;    //Total Number of ESP-r iteration
    int TrnsysTimestep;			//Number of TRNSYS  timestep
    int TotalTrnsysIterations;  //Total Number of TRNSYS iteration
    int TrnsysCode;				//Flag for TRNSYS
    bool Ready;


    d_EsprTrnsysData(void);
    void CopyEsprData(const int nHCC_to_Trnsys, const int nACC_to_Trnsys, const int nZONES,	const d_EsprTrnsysData* pEsprData);
    ~d_EsprTrnsysData(void);
};
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// This holds a data structure used to check convergence of the TRNSYS data.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
struct d_ConvergenceParameters{
	double Flowrate;  			    //kg/s
	double Temperature; 			//deg C
	double Moisture_flowrate;
};

class TrnsysConvergenceData
{
public:
	d_ConvergenceParameters HCC_to_ESPr[MAX_FCCs];
	d_ConvergenceParameters ACC_to_ESPr[MAX_FCCs];

	TrnsysConvergenceData(void);
    ~TrnsysConvergenceData(void);
};

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// This holds a data structure used to check convergence of the TRNSYS data.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

struct f_ConvergenceParameters{
	float Flowrate;  			    //kg/s
	float Temperature; 			//deg C
	float Moisture_flowrate;
};


class EsprConvergenceData
{
public:
	f_ConvergenceParameters HCC_to_Trnsys[MAX_FCCs];
	f_ConvergenceParameters ACC_to_Trnsys[MAX_FCCs];

	EsprConvergenceData(void);
    ~EsprConvergenceData(void);
};



