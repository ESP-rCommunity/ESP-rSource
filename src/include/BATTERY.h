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

C This common declares all the general variables required for the battery model
C These variables describe the state of the battery at the end of a time step
C which corresponds to the state of the battery at the begining of the next step
C 
C---------------------------------------------------------------------------------
C Declaration of variables as common
C---------------------------------------------------------------------------------
      common/BATTERY_STATE/
     & abuseFlag,
     & activeBatLifeControl,
     & batTFin,
     & batDODFin,
     & batLifeUsed,
     & cumBatLifeUsed,
     & lifeUseFactor,
     & mandChargeCycle,
     & mandChargePhase,
     & mandChargePhaseIncrease,
     & timeSinceLastFullCharge,
     & nPreviousTS,
     & batDemandP,
     & loVoltLion,
     & hiVoltLion,
     & loTempLion,
     & hiTempLion,
     & cycles_used_Lion,
     & batDemandP_Lion

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
      REAL batTFin

C Battery Depth Of Discharge (%)
      REAL batDODFin

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

C   Faraday constant
      REAL Faraday_constant
      PARAMETER (Faraday_constant = 96485.)

C   Standard potential of Vanadium Redox Battery (VRB), V
      REAL VRB_E0
      PARAMETER (VRB_E0 = 1.259)

C    Gas constant
      REAL gas_constant
      PARAMETER (gas_constant = 8.314)

C-------------------------------------------------------------------------------
C Li-on battery management flags added by Neil Saldanha, 1 = True, 0 = False
C-------------------------------------------------------------------------------
      INTEGER loVoltLion, hiVoltLion, loTempLion, hiTempLion

C-------------------------------------------------------------------------------
C Li-on degradation, number of cycles used
C-------------------------------------------------------------------------------      
      REAL cycles_used_Lion
      
C---------------------------
C Battery type signatures
C---------------------------
C Lead acid
      INTEGER pba
      PARAMETER (pba = 17)
C Vanadium redox
      INTEGER vrb
      PARAMETER (vrb = 21)
C Lithium-ion
      INTEGER lion
      PARAMETER (lion = 22)

C-----------------------------------------
C Res electric control flags and parameters
C-----------------------------------------
C These variables are invoked by Res_elec_Ctl.F in the cetc
C folder to assign a battery demand if and only if its a li-on battery type

      REAL batDemandP_Lion

