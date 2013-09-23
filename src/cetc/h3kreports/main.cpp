#include <iostream>
#include <string>
#include <cstdlib>
#include "TReportsManager.h"

using namespace std;

void test1()
{
	int numRecords, numUnits;
	int i = 0;
	string names[64];
	string units[64];

	names[i++] = "building/infiltration/air_changes_per_hour";
	names[i++] = "building/infiltration/air_infiltration";
	names[i++] = "building/zone_1/air_point/temperature";
	names[i++] = "building/zone_1/casual_gains/convective";
	names[i++] = "building/zone_1/casual_gains/radiant";
	names[i++] = "building/zone_1/casual_gains/total";
	names[i++] = "building/zone_1/heat_injection";
	names[i++] = "building/zone_1/solar_gains/total_net_absorbed";
	names[i++] = "building/zone_1/solar_gains/total_net_gain";
	names[i++] = "electrical_net/generation/total_generation";
	names[i++] = "electrical_net/loads/HVAC_load";
	names[i++] = "electrical_net/loads/occupant_load";
	names[i++] = "electrical_net/loads/total_load";
	names[i++] = "plant/CETC_HVAC/HRV/elec_load";
	names[i++] = "plant/fuel_cell/Cogeneration_efficiency";
	names[i++] = "plant/fuel_cell/Electric_efficiency_BOP";
	names[i++] = "plant/fuel_cell/Electric_efficiency_FC";
	names[i++] = "plant/fuel_cell/Fuel_HHV";
	names[i++] = "plant/fuel_cell/Q_electric_net";
	names[i++] = "plant/fuel_cell/Q_electric_parasitic";
	names[i++] = "plant/fuel_cell/Q_thermal_net";
	names[i++] = "plant/water_tank_fuel/Temperature";
	names[i++] = "plant/water_tank_fuel/burner_heat_transfer";
	names[i++] = "plant/water_tank_fuel/domestic_hot_water/make_up_temperature";
	names[i++] = "plant/water_tank_fuel/domestic_hot_water/thermal_load";
	names[i++] = "plant/water_tank_fuel/domestic_hot_water/volumetric_dra";
	names[i++] = "plant/water_tank_fuel/exhaust_flow";
	names[i++] = "plant/water_tank_fuel/fuel_flow";
	
	numRecords	= i;
	
	i = 0;
	units[i++] = "l";
	units[i++] = "kg";
	units[i++] = "watts";
	units[i++] = "m";
	
	numUnits = i;
	
	srand(1); //we always seed with the same number so we can compare results across runs
				
	TReportsManager *reportsManager = TReportsManager::Instance();
	
	for(i = 0; i< numRecords; i++)
	{
		reportsManager->SetMeta(names[i], "units", units[rand() % numUnits]);
		reportsManager->SetMeta(names[i], "WattsToGJ", "");
	}
	
/*	reportsManager->SetMeta(names[0], "StepData", "");
	reportsManager->SetMeta(names[1], "StepData", "");
	reportsManager->SetMeta(names[2], "StepData", "");*/
	
	for(i = 0; i < 9000; i++) ///105120
	{
		int j;
		for(j= 0; j < numRecords/3; j++)
		{
			reportsManager->Report(names[j], 3.0);
		}
		
		for(j= numRecords/3; j < 2*numRecords/3; j++)
		{
			reportsManager->Report(names[j], (i/12) * 4.0 + (rand() % 256) / 64.0);
		}
		
		for(j= 2*numRecords/3; j < numRecords; j++)
		{
			if(rand()%4)
				reportsManager->Report(names[j], (i/12) * 4.0 + (rand() % 1024) / 64.0);
		}

		reportsManager->Update(i, (i/12) % 24, (i/(12*24))%365 );
	}
	
	reportsManager->OutputXMLSummary("test1.xml");
	
	
}
int main (int argc, char * const argv[]) {
    // insert code here...
	//TReportsManager *reportsManager = TReportsManager::Instance();

	TTimeData begin, end;
	TTimeDataRange range;
	
	begin.month = 2;
	begin.day = 10;
	begin.hour = 0;
	begin.step = 0;
	
	end.month = 2;
	end.day = 14;
	end.hour = 23;
	end.step = 0;
	
	range.begin = begin;
	range.end = end;
	
//	reportsManager->AddUserDefinedTimeRange(range);

	
	test1();
	return 0;
}
