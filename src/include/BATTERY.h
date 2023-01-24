C This file is part of the ESP-r system.
C Copyright Natural Resources Canada, Government
C of Canada 2004/2005. Please Contact Ian
C Beausoliel-Morrison for details concerning licensing.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C******************************************************************************
C******************************* BATTERY_COMMON *******************************
C Created by: Patrice Pinel
C Initial Creation Date: MARCH, 2005
C Modified by: Maria Mottillo
C Modified by: Neil Saldanha January 2010 to include Li-on battery parameters
C Modified by: Achim Geissler, October 2015 to allow for multiple batteries
C              in electrical net

C This common declares all the general variables required for the battery model
C These variables describe the state of the battery at the end of a time step
C which corresponds to the state of the battery at the begining of the next step
C 
C---------------------------------------------------------------------------------
C Declaration of variables as common
C---------------------------------------------------------------------------------
      common/BATTERY_STATE/
     & abuseFlag(MPOWCOM),
     & activeBatLifeControl(MPOWCOM),
     & ageing_cycle(MPOWCOM),
     & ageing_time_ini(MPOWCOM),
     & batCapRep(MPOWCOM),
     & batDemandP(MPOWCOM),
     & batDOD(MPOWCOM),
     & batLifeUsed(MPOWCOM),
     & batSOE(MPOWCOM),
     & batTemp(MPOWCOM),
     & cumBatLifeUsed(MPOWCOM),
     & cycles_used_Lion(MPOWCOM),
     & mandChargeCycle(MPOWCOM),
     & mandChargePhase(MPOWCOM),
     & mandChargePhaseIncrease(MPOWCOM),
     & hiTempLion(MPOWCOM),
     & hiVoltLion(MPOWCOM),
     & lifeUseFactor(MPOWCOM),
     & loTempLion(MPOWCOM),
     & loVoltLion(MPOWCOM),
     & nPreviousTS,
     & state_of_health(MPOWCOM),
     & timeSinceLastFullCharge(MPOWCOM)


C---------------------------------------------------------------------------------
C Declaration of variable type and definition
C---------------------------------------------------------------------------------
C Flag indicating whether battery is being abused
C (by not performing full charge cycles regularly)
      INTEGER abuseFlag

C Flag for active battery life control (-)
      INTEGER activeBatLifeControl 
C       activeBatLifeControl = 0 : no active battery life control
C       activeBatLifeControl = 1 : active battery life control

C Battery temperature (C)
      REAL batTemp

C Battery state of energy (Wh)
      REAL batSOE

C Battery Depth Of Discharge (%)
      REAL batDOD

C Batery life used during the last time step (years)
      REAL batLifeUsed

C Cumulative battery life used (years)
      REAL cumBatLifeUsed

C Factor describing dominant battery life use in a time step
C       lifeUseFactor = 1 : battery life is determined by float life
C       lifeUseFactor = 2 : battery life is determined by cycle life
C       lifeUseFactor = 3 : battery life is determined by bad treatment
      INTEGER lifeUseFactor

C Flag indicating that mandatory charge cycle is to be started or continued
      INTEGER mandChargeCycle

C Phase of mandatory charge cycle
      INTEGER mandChargePhase

C Flag indicating battery to go into next phase of mandatory charge cycle
      INTEGER mandChargePhaseIncrease

C Time elapsed since the last time the battery was fully charged
      REAL timeSinceLastFullCharge

C--------------------------------------------------------------------------------
C Number of the time step the last time the routine was called
C If the time step value is not equal to the previous value -> time step changed
      INTEGER nPreviousTS

C--------------------------------------------------------------------------------
C Battery demand 
C + Discharge -> energy demanded by system to the battery
C - Charge -> energy provided by system to the batter
      REAL batDemandP

C-------------------------------------------------------------------------------
C Li-on battery management flags added by Neil Saldanha, 1 = True, 0 = False
C-------------------------------------------------------------------------------
      INTEGER loVoltLion, hiVoltLion, loTempLion, hiTempLion

C-------------------------------------------------------------------------------
C Li-on degradation, number of cycles used & calendaric ageing factor
C-------------------------------------------------------------------------------      
      REAL cycles_used_Lion,ageing_cycle,batCapRep
      REAL ageing_time_ini,state_of_health
