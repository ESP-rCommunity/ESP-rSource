C This file is part of the ESP-r system.
C Copyright CANMET Energy Technology Centre 
C Natural Resources Canada, Government of Canada
C 2004. Please Contact Ian Beausoliel-Morrison for details 
C concerning licensing.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

c *********************************************************************
c fDHW_FlueGasLossFactor
c Returns the fDHW_FlueGasLossFactor. This routine returns the ratio of energy 
c that is lost to the flue in relation to the energy  injected into the water. This
c is dependant on the type of tank used and, the fuel source.Note these 
c values are based on values gathered for the hot2000 project by brian bradley and Debra Haltrech
c Reports on these values are contained in the Hot2000 DHW reports vol 1 and 2. 
c Specifically the faxes sent in February 1995
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group
c OUTPUTS:
c   REAL fDHW_FlueGasLossFactor (%) 
c----56--1---------2---------3---------4---------5---------6---------7---------8
 
           
      FUNCTION fDHW_FlueGasLossFactor()
!Variables Used From Module
      
      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"
C#include "ground_temp_mains_common.h" 
      
      
!Function Type Declaration
c Flue Loss Factors These factors were obtained from the H2K code H2KDHW.F Line 32 

c Local Variable
      REAL   fPilot
c This is to check if there was a pilot light present. If so the flue loss is increased by a 
c factor. This factor was found by brian bradley in his faxed report in January 1995.
      
      IF (fPilotEnergyRate.lt.0.1) then
        fPilot = 0.0
      Else
        fPilot = 0.161823
      END IF
c Ensure the variable is Zero.
      fDHW_FlueGasLossFactor = 0.0 
c Assign fAlpha values and determine the flue loss factor based on tank type.
      SELECT CASE (iTankType)
c Propane and Gas Tank Types      
      CASE (Conventional_tank)
        fAlpha = 0.314113
        fDHW_FlueGasLossFactor = fAlpha
     &                          / fDOEEF
     &                          *(1.0-fDOEEF*(1.0 + fPilot))
       
      CASE (Conventional_tank_pilot)
        fAlpha = 0.393235
        fDHW_FlueGasLossFactor = fAlpha
     &                          / fDOEEF
     &                          *(1.0-fDOEEF*(1.0 + fPilot))

      CASE (Tankless)
        fAlpha = 1.000000
        fDHW_FlueGasLossFactor = fAlpha
     &                          / fDOEEF
     &                          *(1.0-fDOEEF*(1.0 + fPilot))
      
      CASE (Instantaneous)
        fAlpha = 0.588235
        fDHW_FlueGasLossFactor = fAlpha
     &                          / fDOEEF
     &                          *(1.0-fDOEEF*(1.0 + fPilot))
      
      CASE (Instantaneous_pilot)
        fAlpha = 0.708805
        fDHW_FlueGasLossFactor = fAlpha
     &                          / fDOEEF
     &                          *(1.0-fDOEEF*(1.0 + fPilot))
             
      CASE (Induced_draft_fan)
        fAlpha = 0.489625
        fDHW_FlueGasLossFactor = fAlpha
     &                          / fDOEEF
     &                          *(1.0-fDOEEF*(1.0 + fPilot))
      
      CASE (Induced_draft_fan_pilot)
        fAlpha = 0.624103
        fDHW_FlueGasLossFactor = fAlpha
     &                          / fDOEEF
     &                          *(1.0-fDOEEF*(1.0 + fPilot))
             
      CASE (Direct_vent)
        fAlpha = 0.494467
        fDHW_FlueGasLossFactor = fAlpha
     &                          / fDOEEF
     &                          *(1.0-fDOEEF*(1.0 + fPilot))
             
      CASE (Direct_vent_pilot)
        fAlpha = 0.633287
        fDHW_FlueGasLossFactor = fAlpha
     &                          / fDOEEF
     &                          *(1.0-fDOEEF*(1.0 + fPilot))
             
      CASE (Condensing)
        fAlpha = 0.000000
        fDHW_FlueGasLossFactor = fAlpha
     &                          / fDOEEF
     &                          *(1.0-fDOEEF*(1.0 + fPilot))
c Oil tank types             
      CASE (Oil_Conventional_tank)
        fAlpha = 0.468284
        fDHW_FlueGasLossFactor = fAlpha
     &                          / fDOEEF
     &                          *(1.0-fDOEEF*(1.0 + fPilot))
             
      CASE (Oil_Tankless)
        fAlpha = 1.000000
        fDHW_FlueGasLossFactor = fAlpha
     &                          / fDOEEF
     &                          *(1.0-fDOEEF*(1.0 + fPilot))

c There are no alpha values for wood systems and no standby tank losses for wood systems thus
c the effieciency for the wood system is related directly to the flue gas losses. 
c This is an over simplification of the system which should be address if the need arises. 
c Once again this has wood model is borrowed by H2KDHW.F
       
      CASE (Wood_Fireplace)
       fDHW_FlueGasLossFactor = (1.0-fDOEEF) 
     &                           / fDOEEF
       
      CASE (Wood_stove_water_coil)
       fDHW_FlueGasLossFactor = (1.0-fDOEEF) 
     &                           / fDOEEF
       
      CASE DEFAULT
       fDHW_FlueGasLossFactor = 0.0       

c Electricity and SOLAR do not have a flue so it is set to Zero.                 
      END SELECT
      IF (iFuelType.eq.ELECTRICITY 
     &    .OR.
     &    iFuelType.eq.SOLAR) THEN
      
      fDHW_FlueGasLossFactor = 0.0
      ENDIF

      END

c *********************************************************************
c The following 4 functions are used to calculate the cold mains water 
c inlet temperature:
c   - fDHW_DepthAmplitudeMod()
c   - fDHW_LimitMeanGroundTemp()
c   - iDHW_GroundOffset()
c   - fDHW_ColdMainTemp()
c They are used in several component models and were calculated separately for each.
c These functions have been moved to a central location
c /cetc/ground_temp_mains.F

c *********************************************************************
c fDHW_DepthAmplitudeMod ****CURRENTLY UNUSED!******
c Returns the Depth Amplitude Modifier.
c This Formula was obtained originally from 
c SAR Engineerings LTD report on 'Domestic Hot Water Simulation Algorithms' by Ken Cooper for NRCan
c Section 4.2 Appendix. Where AMod = 0.2+0.04*(TGMean-5.0)
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group
c INPUTS:
c   REAL fMeanGroundTemp Mean Ground Temp from weather data
c OUTPUTS:
c   REAL fDHW_DepthAmplitudeMod  Depth Amplitude Modifier
c----56--1---------2---------3---------4---------5---------6---------7---------8
 
           
      FUNCTION fDHW_DepthAmplitudeMod()
!Variables Used From Module
      
      
      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"

!This line was obtained from B.Bradeley H2KDHW.F  line 205 and is related to the Ken Cooper model.
        fDHW_DepthAmplitudeMod = 0.2 
     &                           + 0.04  
     &                           * ( fDHW_LimitMeanGroundTemp()
     &                           - 5.0 )
      
      END


c *********************************************************************
c FUNCTION fDHW_LimitMeanGroundTemp
c Returns GroundTemp in Limit Bounds Ground Temp is rarely beyond these values. 
c This is reference in the Ken Cooper model as well as B.Bradley's code in H2KDHW.F line 205-206
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group
c INPUTS:
c   REAL fMeanGroundTemp Mean Ground Temp from weather data
C
c OUTPUTS:
c   REAL fDHW_LimitMeanGroundTemp limits Values of fMeangroundTemp to <15.0 and >5.0 C.
c----56--1---------2---------3---------4---------5---------6---------7---------8
        FUNCTION fDHW_LimitMeanGroundTemp()
        
        IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"

        fDHW_LimitMeanGroundTemp = fMeanGroundTemp
        
        IF (fMeanGroundTemp.lt.5.0) THEN
          fDHW_LimitMeanGroundTemp = 5.0
        ENDIF

        IF (fMeanGroundTemp.gt.20.0) THEN 
          fDHW_LimitMeanGroundTemp = 20.0
        ENDIF
        
      END

c *********************************************************************
c INTEGER FUNCTION iDHW_GroundOffset(fMeanGroundTemp)
c Returns the ground temperature offset value. This formula was obtained from 
c SAR Engineerings LTD report on 'Domestic Hot Water Simulation Algorithms' by Ken Cooper for NRCan
c Section 4.2 Appendix
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group
c INPUTS:
c   REAL fMeanGroundTemp Mean Ground Temp from weather data
c
c OUTPUTS:
c   INTEGER iDHW_GroundOffset   Offset to account for depth temp. variance
c----56--1---------2---------3---------4---------5---------6---------7---------8

        FUNCTION iDHW_GroundOffset()
        
        IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"        

c Select Offset Modifier based on Mean Ground Temperature
         IF (fMeanGroundTemp.lt.5.0) THEN
           iDHW_GroundOffset = 6
         
         ELSEIF (fMeanGroundTemp.gt.15.0) THEN
          iDHW_GroundOffset = 8
       
         ELSE 
          iDHW_GroundOffset = 7
        
         ENDIF 
      END

c *********************************************************************
c FUNCTION fDHW_ColdMainTemp
c Returns the cld main value in C. This formula was obtain from 
c SAR Engineerings LTD report on 'Domestic Hot Water Simulation Algorithms' by Ken Cooper for NRCan
c Section 4.2 Appendix
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group
c
c OUTPUTS:
c  fDHW_ColdMainTemp   cold water main temperature C
c----56--1---------2---------3---------4---------5---------6---------7---------8

      FUNCTION fDHW_ColdMainTemp()
      
      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"      

c Variable Definition        
c Formula  This is based on Ken Coopers Cold water model and B.Bradley's model used in H2KDHW.F 
c line 250     
        fDHW_ColdMainTemp =
     &     11.1 
     &     + 3.0 
     &     + fAmpGroundTempVar
     &     * SIN(PI/6*(REAL(iMonth) + REAL(iDHW_GroundOffset())))             

      END

c *********************************************************************
c FUNCTION fDHW_WaterDraw()
c Returns Water demand per timestep.  This formula was obtained from
c SAR Engineerings LTD report on 'Domestic Hot Water Simulation Algorithms' by Ken Cooper for NRCan
c Section 4.2 Appendix. See 'Load' equation
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group
c OUTPUTS:
c       REAL    fWaterDraw                   Litres/Timestep
c
c----56--1---------2---------3---------4---------5---------6---------7---------8
      FUNCTION fDHW_WaterDraw()
      
      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"
c#include "ground_temp_mains_common.h"

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      INTEGER   ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its           
      REAL   fNormalizedDailyWaterDraw 
      REAL   fLitresforNumOccupants          

c This calulates the amount of water  used based on the the number of occupants
        fLitresforNumOccupants = 85.0 + ( 35.0 * fNumOfOccupants ) 
c This normalises the water draw based on a standard usage temperature (currently a parameter 
c of 60C. This is from B.Bradley'd H2KDHW.F line 254.
        fNormalizedDailyWaterDraw = (0.75 
     &                   * fLitresforNumOccupants
     &                   * ( ( fUsageTemp - fDHW_ColdMainTemp() )
     &                   / ( fHotSupplyTemp - fDHW_ColdMainTemp()))
     &                   + 0.25 * fLitresforNumOccupants )
c        fNormalizedDailyWaterDraw = ( 0.75 
c     &                   * fLitresforNumOccupants
c     &                   * ( (fUsageTemp 
c     &                           - fDHW_ColdMainTemp(iMonth))
c     &                   / (fHotSupplyTemp 
c     &                          - fDHW_ColdMainTemp(iMonth)) )
c     &                   + 0.25 * fLitresforNumOccupants )
       
      IF (iH2K.eq.1) then ! treat it like H2K 
        fDHW_WaterDraw = fNormalizedDailyWaterDraw
     &                  /fHoursInDay / REAL(iNTSTEP)

  
      ELSE   !Treat it with water demand curve contained in fWaterDrawCurveHourly(24).
c This formula will first obtain the percentage of total daily water which has been drawn in that hour.
        fDHW_WaterDraw = fWaterDrawCurveHourly(ihrp)
     &                 / fWaterDrawCurveTotal
c Then this will muliply it by the actual water draw and divide it by the time step slice.
     &                   * fNormalizedDailyWaterDraw / REAL(iNTSTEP)   
      ENDIF  

c Note the draw is considered instantaneous ie flow = infinite during draw period

      END


c *********************************************************************
c FUNCTION fDHW_StartTemp
c Returns Temperature of Water in Tank at start of time step..
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group
c INPUTS:
c OUTPUTS:
c   REAL fDHW_StartTemp     Temperature at start of Current Timestep C
c----56--1---------2---------3---------4---------5---------6---------7---------8

      FUNCTION fDHW_StartTemp()
!Variables Used From Module
      
      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"      
c#include "ground_temp_mains_common.h"


!Local Variables
      REAL   fEnergyInHotWaterPortion
      REAL   fEnergyInColdWaterPortion
      REAL   fEnergyInTank
      REAL   fHotWaterVolumeRemaining
      
C Determine if the tank was depleted completely.
      fHotWaterVolumeRemaining = fTankSize-fWaterDraw

c If the draw is more than the tank then assume that the temperature of the water in the
c tank is equal to the cold water temp.
      IF (fHotWaterVolumeRemaining.le.0.0) then 
        fHotWaterVolumeRemaining = 0.0   
        fDHW_StartTemp = fDHW_ColdMainTemp()
c    fDHW_StartTemp = fDHW_ColdMainTemp(iMonth)
      ELSE    

C Energy in Tank
C Energy in Joules of Hot water in tank
        fEnergyInHotWaterPortion =
     &    fDHW_WaterEnergyFromTemp(fOldFinalWaterTemp,
     &                             fHotWaterVolumeRemaining)
c Energy in Joules of Cold water in tank
        fEnergyInColdWaterPortion =
     &     fDHW_WaterEnergyFromTemp(fDHW_ColdMainTemp(),
     &                              fWaterDraw) 

C Future form for mains water temp calculation
C        fEnergyInColdWaterPortion =
C    &     fDHW_WaterEnergyFromTemp(fDHW_ColdMainTemp(iMonth),fWaterDraw)


c This sums the energy present in tank (hot and cold water) and subtracts the tank losses 
c from the previous timestep
        fEnergyInTank = fEnergyInHotWaterPortion
     &                 +fEnergyInColdWaterPortion
     &                 -fOldStandbyTankLosses

C Takes the fEnergy in tank and converts it to a temperature using E = SPHeatWater*Temp*Volume
        fDHW_StartTemp =fDHW_WaterTempFromEnergy(fEnergyInTank,
     &                                           fTankSize)     
      END IF

      END


c *********************************************************************
c FUNCTION fDHW_DrawsForEachTank
c Returns Temperature of Water in Tank at start of time step..
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group
c OUTPUTS:
c   REAL fDHW_DrawsForEachtank     Draw for tank is Litres
c----56--1---------2---------3---------4---------5---------6---------7---------8

      SUBROUTINE fDHW_DrawsForEachtank()
!Variables Used From Module
      
      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"      
c#include "ground_temp_mains_common.h" 
!Local Variables

      
      END
     
c *********************************************************************
c FUNCTION DHW_WaterTempfromEnergy
c Returns Returns Temperature of Water given Energy(joules) and Volume(L)..
c Created by: Phylroy A. Lopez/users/phylroy/ccht_basic/cfg/ccht_basic.cfg
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group
c INPUTS:
c   REAL fWaterEnergy (C)
c   REAL fVolumeofWater (L)
c   REAL SpecHeatWater  (J/(C*L))
 
c OUTPUTS:
c   fDHW_WaterTempFromEnergy (C)
c----56--1---------2---------3---------4---------5---------6---------7---------8

      FUNCTION fDHW_WaterTempFromEnergy(fWaterEnergy,
     &                                  fVolumeofWater)
!Variables Used From Module


        IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"
c#include "ground_temp_mains_common.h" 


!Local Variables
        REAL fWaterEnergy
        REAL fVolumeofWater     

c Formula        
        fDHW_WaterTempFromEnergy = fWaterEnergy
     &                             /(fVolumeofWater * fSpecHeatWater)
      
      END


c *********************************************************************
c FUNCTION DHW_WaterVolumefromTemp
c Returns Determines Energy Stored in Water..
c This subroutine determines the energy content of water given a temperature(C) 
c and a volume (L)
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group
c INPUTS:
c   REAL fWaterTemp (C)
c   REAL fVolumeofWater (L)
c   REAL SpecHeatWater (J/(C*L))
c OUTPUTS:
c   REAL fDHW_WaterEnergyFromTemp (J)
c----56--1---------2---------3---------4---------5---------6---------7---------8

       FUNCTION fDHW_WaterVolumeFromTempEnergy(fWaterTemp,
     &                                         fEnergy)
c Variables Used From Module


       IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"       
c#include "ground_temp_mains_common.h" 

c Local Variables
       REAL   fWaterTemp
       REAL   fEnergy      

c Formula       
       fDHW_WaterVolumeFromTempEnergy = fEnergy
     &                                  /fWaterTemp
     &                                  /fSpecHeatWater

      END

c *********************************************************************
c FUNCTION DHW_WaterEnergyfromTemp
c Returns Determines Energy Stored in Water..
c This subroutine determines the energy content of water given a temperature(C) 
c and a volume (L)
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group
c INPUTS:
c   REAL fWaterTemp     (C)
c   REAL fVolumeofWater (L)
c   REAL SpecHeatWater  (J/(C*L))
c OUTPUTS:
c   REAL fDHW_WaterEnergyFromTemp (J)
c----56--1---------2---------3---------4---------5---------6---------7---------8


       FUNCTION fDHW_WaterEnergyFromTemp(fWaterTemp,
     &                                   fVolumeofWater)
c Variables Used From Module


       IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"       
c#include "ground_temp_mains_common.h" 


c Local Variables
       REAL   fWaterTemp
       REAL   fVolumeofWater      

c Formula       
       fDHW_WaterEnergyFromTemp = fVolumeofWater
     &                            *fWaterTemp
     &                            *fSpecHeatWater

      END


c *********************************************************************
c FUNCTION fDHW_PilotEnergy
c Returns Returns Pilot energy in Joules Used in Time Step
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group
c INPUTS:

c OUTPUTS:
c   fDHW_WaterTempFromEnergy
c----56--1---------2---------3---------4---------5---------6---------7---------8

      FUNCTION fDHW_PilotEnergy()
!Variables Used From Module
       
      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"            
c#include "ground_temp_mains_common.h" 
                      
c Formula simply mutliply the power by the timestep (in seconds)
        fDHW_PilotEnergy = fPilotEnergyRate
     &                      * fSecondsInHour
     &                      / REAL(iNTSTEP)
        
      END

c *********************************************************************
c FUNCTION fDHW_PilotEnergyToRoom
c Returns Returns Pilot energy in Joules Lost to Room in Time Step
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group
c INPUTS:
c       REAL fPilotEnergyToSpaceHeat (%)
c       REAL    fPilotEnergy                    (Joules)
c OUTPUTS:
c       fDHW_PilotEnergyToRoom
c----56--1---------2---------3---------4---------5---------6---------7---------8

      FUNCTION fDHW_PilotEnergyToRoom()
!Variables Used From Module

      
      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"      
c#include "ground_temp_mains_common.h"    
      
c This formua determines the energy injected to room by the pilot light by
c multplying the pilot energy to space factor by the Pilot energy expending during the timestep.         
       fDHW_PilotEnergyToRoom =  fPilotToSpaceHeat 
     &                              * fDHW_PilotEnergy()
      END

c *********************************************************************
c FUNCTION fDHW_FuelEnergyRequired
c Returns Total DHW energy in Joules in Time Step. 
c Total energy here is defined and the energy required from the fuel source during the timestep.
c Please note that fDHW_FlueLosses is a function of fDHW_EnergyInputWater 
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group
c OUTPUTS
c       fDHW_FuelEnergyRequired
c----56--1---------2---------3---------4---------5---------6---------7---------8

      FUNCTION fDHW_FuelEnergyRequired()
      
      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"
c#include "ground_temp_mains_common.h" 

      REAL   fEnergy
      
     
c Formula 
      SELECT CASE (iTankType)
      
      CASE (Elec_Tankless_Heatpump)
       fEnergy = fDHW_EnergyInputWater()/fDOEEF

      CASE (Elec_Heatpump)
       fEnergy = fDHW_EnergyInputWater()/fDOEEF
       
      CASE (Elec_Instantaneous)
       fEnergy = fDHW_EnergyInputWater()
     &            +fDHW_EnergyInputWater()
     &            *(1-FDOEEF)/fDOEEF
      CASE DEFAULT 
       fEnergy = fDHW_EnergyInputWater()

      END SELECT
            
c Energy imparted to water during timestepl(Due to Draws, Standby (Parasitic) Losses)
c Energy Lost to flue due to ineffieceiency of combustion process
c Pilot energy use during timestep this factor of 0.4 is explain in Brian Bradley's fax 
c report January 1995
      fDHW_FuelEnergyRequired =  
     &                  fEnergy 
     &                  + fDHW_FlueLosses()     
     &                  + 0.4 *fDHW_PilotEnergy () 
  
      END

c *********************************************************************
c FUNCTION fDHW_EnergyToZone
c Returns the energy in Joules in Time Step that is transferred to room 
c taking in to account the following factors. 
c Parasitic losses:Standby tank losses
c Pipelosses : Cold main pipes absorbing energy.
c Pilot Energy: The portion of pilot energy that is transmitted to the room.
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group
c INPUTS:
c   REAL fPipeLosses         (J) Energy added to water during timestep
c   REAL    fStandByTankLosses  (J) Losses from tank to Room during timestep
c   REAL    fPilotEnergyToRoom  (j) Pilot energy to Room during timestep
c OUTPUTS:
c       fDHW_EnergyToZone                 (J)
c----56--1---------2---------3---------4---------5---------6---------7---------8

      FUNCTION fDHW_EnergyToZone()
      
      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"
c#include "ground_temp_mains_common.h" 

c Formula         
c Parasitic losses to room from water in tank losing energy. 
c Pipe Losses (very small..may eliminate in future)
c Portion of Pilot energy that is transmitted to room.
        fDHW_EnergyToZone =
     &    + fDHW_StandByTankLosses()
     &    + fDHW_PipeGains()
     &    + fDHW_PipeLosses()
     &    + fDHW_PilotEnergyToRoom()  
             
      END

c *********************************************************************
c FUNCTION fDHW_PipeGains
c 
c Pipegains from room to cold water main.
c Pilot Energy: The portion of pilot energy that is transmitted to the room.
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group

c OUTPUTS:
c       fDHW_Pipegains                 (J)
c----56--1---------2---------3---------4---------5---------6---------7---------8

      FUNCTION fDHW_PipeGains()
      
      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"
c#include "ground_temp_mains_common.h"

c Formula         

c Pipe gains to Cold Water Main from space obtained from Brian bradley's H2K model 
c H2KDHW.F Line 857

        fDHW_PipeGains = ( -3.7 + 0.37 * 
     &                   ( fDHW_ColdMainTemp() - 10. ) -
     &                 0.83 * ( fNumOfOccupants - 3.5 ) )
     &                 / fSecondsINDAY
     &                 * fSecondsInHour/iNTSTEP
     &                 * 1e6  ! MJ to Joules

C Future form for calculation using monthly water mains temperature.
C        fDHW_PipeGains = ( -3.7 + 0.37 * 
C    &                   ( fDHW_ColdMainTemp(iMonth) - 10. ) -
C    &                 0.83 * ( fNumOfOccupants - 3.5 ) )
C    &                 / fSecondsINDAY
C    &                 * fSecondsInHour/iNTSTEP
C    &                 * 1e6   ! MJ to Joules
     

      END

c *********************************************************************
c FUNCTION fDHW_PipeLosses
c Returns the energy in Joules in Time Step that is transferred to room 
c taking in to account the following factors. 
c Parasitic losses:Standby tank losses
c Pipelosses : Cold main pipes absorbing energy.
c Pilot Energy: The portion of pilot energy that is transmitted to the room.
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group
c OUTPUTS:
c       fDHW_PipeLosses                (J)
c----56--1---------2---------3---------4---------5---------6---------7---------8

      FUNCTION fDHW_PipeLosses()
      
      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"
c#include "ground_temp_mains_common.h" 

c Formula         
c PipeLoss in Watts from Hot Water pipes this was obtain from Ken coopers report and in Brian Bradley's code for Hot2000
c H2kDHW.F line 755
        fDHW_PipeLosses = (4.4 
     &            + 0.1
     &            *( fDHW_AverageWaterTemp() - 60.0)
     &            + 0.5*( fNumOfOccupants / 3.5 - 1.0 )) / fSECONDSINDAY
     &            * fSecondsInHour/iNTStep 
     &            *1e6 !MJ to Joules
     
      END

c********************************************************************************
c FUNCTION fDHW_DOEEFTank
c Estimate the Tank Efficiency using DOE Standard values.
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group

c OUTPUTS:
c     REAL       fDHW_EFTank    (%)
c----56--1---------2---------3---------4---------5---------6---------7---------8

      FUNCTION fDHW_DOEEFTank()
cVariables Used From Module
      
      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"
c#include "ground_temp_mains_common.h"       

c Local variables

C      REAL   fAlpha
      REAL   fPilot
      
      IF (fPilotEnergyRate.lt.0.1) then
        fPilot = 0.0
      Else
        fPilot = 0.161823
      END IF

      SELECT CASE (iTankType)
      
      CASE (Conventional_tank)
        fAlpha = 0.314113
        fDHW_DOEEFTank = (1.-fAlpha)
     &                 *(1.-fDOEEF*(1. + fPilot))
       
      CASE (Conventional_tank_pilot)
       fAlpha = 0.393257
       fDHW_DOEEFTank = (1.-fAlpha)
     &                 *(1.-fDOEEF*(1. + fPilot))

      CASE (Tankless)
        fAlpha = 1.000000
        fDHW_DOEEFTank = (1.-fAlpha)
     &                 *(1.-fDOEEF*(1. + fPilot))
      
      CASE (Instantaneous)
        fAlpha = 0.588235
        fDHW_DOEEFTank = (1.-fAlpha)
     &                 *(1.-fDOEEF*(1. + fPilot))
      
      CASE (Instantaneous_pilot)
        fAlpha = 0.708805
        fDHW_DOEEFTank = (1.-fAlpha)
     &                 *(1.-fDOEEF*(1 + fPilot))
             
      CASE (Induced_draft_fan)
        fAlpha = 0.489625
        fDHW_DOEEFTank = (1.-fAlpha)
     &                 *(1.-fDOEEF*(1. + fPilot))
      
      CASE (Induced_draft_fan_pilot)
        fAlpha = 0.624103
        fDHW_DOEEFTank = (1.-fAlpha)
     &                 *(1.-fDOEEF*(1. + fPilot))
             
      CASE (Direct_vent)
        fAlpha = 0.494467
        fDHW_DOEEFTank = (1.-fAlpha)
     &                 *(1.-fDOEEF*(1. + fPilot))
             
      CASE (Direct_vent_pilot)
        fAlpha = 0.633287
        fDHW_DOEEFTank = (1.-fAlpha)
     &                 *(1.-fDOEEF*(1. + fPilot))
             
      CASE (Condensing)
        fAlpha = 0.000000
        fDHW_DOEEFTank = (1.-fAlpha)
     &                 *(1.-fDOEEF*(1. + fPilot))
             
      CASE (Oil_Conventional_tank)
        fAlpha = 0.468284
        fDHW_DOEEFTank = (1.-fAlpha)
     &                 *(1.-fDOEEF*(1 + fPilot))
             
      CASE (Oil_Tankless)
        fAlpha = 1.000000
        fDHW_DOEEFTank = (1.-fAlpha)
     &                 *(1.-fDOEEF*(1 + fPilot))
             
      CASE (Wood_Fireplace)
        fDHW_DOEEFTank = 1.-fDOEEF
             
      CASE (Wood_stove_water_coil)
        fDHW_DOEEFTank = 1.-fDOEEF

      CASE (Elec_Tankless_Heatpump)
        fDHW_DOEEFTank = 1.-fDOEEF

      CASE (Elec_Heatpump)
        fDHW_DOEEFTank = 1.-fDOEEF

      CASE (Elec_Instantaneous)
       fDHW_DOEEFTank = 1.-fDOEEF
      
      CASE (Elec_Conventional_tank)
        fDHW_DOEEFTank = 1.- fDOEEF
      
      CASE (Elec_Conserver_tank)
        fDHW_DOEEFTank = 1.-fDOEEF
      END SELECT
      fEFTank = fDHW_DOEEFTank
      END

      
c *********************************************************************
c REAL  FUNCTION fDHW_StandbyTankLosses
c Returns Total Tank Losses in Joules in Time Step
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group
c OUTPUTS:
c   REAL    fDHW_StandbyTankLosses (J)
c----56--1---------2---------3---------4---------5---------6---------7---------8

      FUNCTION fDHW_StandbyTankLosses()
!Variables Used From Module
      
     
      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"
c#include "ground_temp_mains_common.h" 
      

      SELECT CASE (iTankType)
!Propane and NGas non tank systems
      CASE (Tankless)
      fDHW_StandbyTankLosses = 0.0
      
      CASE (Instantaneous) 
      fDHW_StandbyTankLosses = 0.0
      
      CASE (Instantaneous_pilot)
      fDHW_StandbyTankLosses = 0.0

!Oil non tank system
      CASE (Oil_tankless)
      fDHW_StandbyTankLosses = 0.0

!Elec non tank systems
      CASE (Elec_Tankless_Heatpump)
      fDHW_StandbyTankLosses = 0.0 

      CASE (Elec_Heatpump)
      fDHW_StandbyTankLosses = 0.0

      CASE (Elec_Instantaneous)
      fDHW_StandbyTankLosses = 0.0
      
      CASE DEFAULT 
      IF (iH2K.eq.1) then
        fDHW_StandbyTankLosses = fDHW_UA()  
     &                           * (fFreductFactor*fHotSupplyTemp 
     &                              - fRoomTemp)
     &                           * fSecondsInHour/ REAL(iNTSTEP)  

      ELSE

        fDHW_StandbyTankLosses = fDHW_UA()  
     &                           * (fDHW_AverageWaterTemp() 
     &                              - fRoomTemp)
     &                           * fSecondsInHour/ REAL(iNTSTEP)

      ENDIF
      
      END SELECT

      END


c *********************************************************************
c FUNCTION fDHW_UA
c Estimate the UA  Using DOE default Vaues
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group
c     REAL      fDHW_UA             (%)
c----56--1---------2---------3---------4---------5---------6---------7---------8

      FUNCTION fDHW_UA()
!Variables Used From Module
      
      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"
c#include "ground_temp_mains_common.h"       

    
!Local Variables      
      REAL   fSurfArea ! in meters
      REAL   fRadius   ! in meters
c  Ua calcuation obtained from b.Bradley H2KDHW.F line 544
      fDHW_UA = 15.2543 
     &          * fDHW_DOEEFTank() 
     &          /fDOEEF
     
cThis boolean operation will add blaket insulation to the tank UA if any.
      If ( fBlanketRSI .gt. 0.1 ) Then
         fRadius = Sqrt ( fTankSize * 0.001 / fTankHeight ) ! Radius(m)
         fSurfArea = PI * fRadius * ( 2. * fTankHeight + fRadius ) ! Surf A (m2)
         fDHW_UA = fSurfArea / ( fBlanketRSI 
     &                           + fSurfArea / fDHW_UA )
      Endif           
      END

c *********************************************************************
c FUNCTION fDHW_EnergyRequired
c Estimate Determine the Energy Required per Timestep
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group
c INPUTS:
c OUTPUTS:
c  REAL      fDHW_EnergyRequired (J)
c----56--1---------2---------3---------4---------5---------6---------7---------8

      FUNCTION fDHW_EnergyRequired()
c Variables Use from Modules      
      
      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"
c#include "ground_temp_mains_common.h"


      REAL   fWaterDrawGreaterThanTank

c This determines if the water draw is greater than the tank.
      fWaterDrawGreaterThanTank = fWaterDraw -
     &                            fTankSize

c If the water draw is less than the tank size then set it to zero   
      IF (fWaterDrawGreaterThanTank .lt. 0.0) then
          fWaterDrawGreaterThanTank = 0
      END IF
       

c Checks to see if it is still in the dead band.
      IF (fDHW_StartTemp()
     &    .ge.
     &    fHotSupplyTemp-fTemperatureBand) THEN
        fDHW_EnergyRequired = 0.0
      ELSE

c If not, it determines how much burner energy is required.
        fDHW_EnergyRequired =
     &        fDHW_WaterEnergyFromTemp( fHotSupplyTemp , 
     &                                       fTankSize )
     &        - fDHW_WaterEnergyFromTemp(fDHW_StartTemp(), 
     &                                     fTankSize)
     &        +fDHW_WaterEnergyFromTemp(fHotSupplyTemp,
     &                              fWaterDrawGreaterThanTank)
     &        -fDHW_WaterEnergyFromTemp(fDHW_ColdMainTemp(),
     &                              fWaterDrawGreaterThanTank)

C Future form of calulation using monthly mains water temperature
C        fDHW_EnergyRequired =
C     &        fDHW_WaterEnergyFromTemp( fHotSupplyTemp , 
C     &                                       fTankSize )
C     &        - fDHW_WaterEnergyFromTemp(fDHW_StartTemp(), 
C     &                                     fTankSize)
C     &        +fDHW_WaterEnergyFromTemp(fHotSupplyTemp,
C     &                              fWaterDrawGreaterThanTank)
C     &        -fDHW_WaterEnergyFromTemp(fDHW_ColdMainTemp(iMonth),
C     &                              fWaterDrawGreaterThanTank)
         
        fEnergyRequired = fDHW_EnergyRequired 
      ENDIF
      END

c *********************************************************************
c FUNCTION fDHW_EnergyInputWater
c Estimate Determine the actual Input energy during the Timestep.
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group
c fDHW_EnergyInputWater (J)
c----56--1---------2---------3---------4---------5---------6---------7---------8

      FUNCTION fDHW_EnergyInputWater()
!Variables Used From Module
           
      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"
c#include "ground_temp_mains_common.h"

!Local Vars
      REAL   fMaxOutputOfInjectorPerTstep
c Determine max output for heat injector for timestep.
      fMaxOutputofInjectorPerTStep =fHeatinjectorPower
     &                              *fSecondsInHour
     &                              /REAL(iNTSTEP)
   
      IF (fDHW_EnergyRequired()
     &    .gt.fMaxOutputofInjectorPerTStep )THEN 
 
        fDHW_EnergyInputWater = fMaxOutputofInjectorPerTStep 
      ELSE
        fDHW_EnergyInputWater = fDHW_EnergyRequired() 
      ENDIF
      fEnergyinputWater = fDHW_EnergyInputWater
      END
      

c *********************************************************************
c FUNCTION fDHW_FinalWaterTemp
c Determine the Final water temperature at te end of the timestep..
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group
c OUTPUTS:
c     REAL       fDHW_FinalWaterTemp          (C)
c----56--1---------2---------3---------4---------5---------6---------7---------8

      FUNCTION fDHW_FinalWaterTemp()
!Variables Used From Module

      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"
c#include "ground_temp_mains_common.h"

!Local Variables 
      REAL   fTotalEnergyInTank
      REAL   fEnergyInHotWaterPortion
      REAL   fEnergyInColdWaterPortion
      REAL   fEnergyInTank
      REAL   fHotWaterVolumeRemaining
      
C Determine if volume of draw was greater than tank volume.
      fHotWaterVolumeRemaining =  fTankSize-fWaterDraw
      IF (fHotWaterVolumeRemaining.le.0.0) then

c If so set Hot water volume remaining to zero.       
        fHotWaterVolumeRemaining = 0.0
c Determine energy in volume of water draw after heat injection for burner
        fEnergyInTank = 
     &    fDHW_WaterEnergyFromTemp(fDHW_ColdMainTemp(),
     &                             fWaterDraw)
     &    + fDHW_EnergyInputWater()

C Future form of calculation with mains monthly water temperatures
C        fEnergyInTank = 
C     &    fDHW_WaterEnergyFromTemp(fDHW_ColdMainTemp(iMonth),
C     &                             fWaterDraw)

c Determine temperature in volume of water.   
        fDHW_FinalWaterTemp = 
     &    fDHW_WaterTempFromEnergy(fEnergyInTank,
     &                             fWaterDraw)
      ELSE  

c Sum Energy in the tank    
        fTotalEnergyInTank =
     &    fDHW_WaterEnergyFromTemp( 
     &                            fDHW_StartTemp(),
     &                            fTankSize
     &            )
     &    + fDHW_EnergyInputWater()

c Get final temp form energy and volume      
        fDHW_FinalWaterTemp = 
     &    fDHW_WaterTempfromEnergy(fTotalEnergyInTank,
     &                           fTankSize)
      END IF
      END

c *********************************************************************
c FUNCTION fDHW_AverageTemp
c Estimate Determine the actual Input energy.
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group
c OUTPUTS:
c     REAL      fDHW_AverageTemp           (C)
c----56--1---------2---------3---------4---------5---------6---------7---------8

      FUNCTION fDHW_AverageWaterTemp()
!Variables Used From Module
        
      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"
c#include "ground_temp_mains_common.h" 

c Local variables
      REAL   fMaxOutputOfInjectorPerTstep
      fMaxOutputOfInjectorPerTstep = fHeatInjectorPower
     &                               * fSecondsInHour
     &                               /REAL(iNTSTEP)

      fDHW_AverageWaterTemp =  (fDHW_StartTemp()
     &                          +fDHW_FinalWaterTemp())
     &                         / 2.0
     &                         * fDHW_EnergyInputWater()
     &                         / fMaxOutputOfInjectorPerTstep
     &                         + fDHW_FinalWaterTemp()
     &                         *(1.0
     &                         - fDHW_EnergyInputWater()
     &                         /fMaxOutputOfInjectorPerTstep)
      END
     
 

c *******************************************************************
c FUNCTION fDHW_FlueLosses
c Estimate Determine the actual Input energy.
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group
c OUTPUTS:
c     REAL      fDHW_FlueLosses           (J)
c----56--1---------2---------3---------4---------5---------6---------7---------8

      FUNCTION fDHW_FlueLosses()
!Variables Used From Module

      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"
c#include "ground_temp_mains_common.h" 

c Local variables
      SELECT CASE (iFuelType)
      CASE (ELECTRICITY,SOLAR)
        fDHW_FlueLosses = 0.0
      CASE (OIL,NATURALGAS,WOOD,PROPANE)
        fDHW_FlueLosses = (fDHW_EnergyInputWater()
     &                    -fDHW_standbyTankLosses())
     &                    * fDHW_FlueGasLossFactor()
      END SELECT
      END
     
c *******************************************************************
c REAL SUBROUTINE sDHW_Initialize
c Initialize Variables (Hard coded for present).
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group

c----56--1---------2---------3---------4---------5---------6---------7---------8

      SUBROUTINE DHW_Initialize()
!Variables Used From Module
      
      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"
c#include "ground_temp_mains_common.h" 

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/C21/IFCFG,cfgroot,LCFGF
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep


C Declarations for local variables and variables passed in common blocks.
      INTEGER   ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,iDAYN
      INTEGER   isd1,ism1,isd2,ism2,isds,isdf,ntstep,iTankNumber
      character cfgroot*24,LCFGF*72
      integer IFCFG

C Check to see if DHW is required.
      IF (iDHW_Flag.ne.1) then
        RETURN
      ENDIF
      
      iNTSTEP = ntstep

      fRoomTemp = 20.0
              

C Initialise to default values.
      fOldAverageWaterTemp =  55.0
      fOldFinalWaterTemp =  55.0
      fOldStandbyTankLosses  = 0.0

      fUA = fDHW_UA() 
      
      fFGas = fDHW_FlueGasLossFactor()

      fFTank =  fDHW_DOEEFTank()

       
c This water draw curve was taken from the CSA f379.1-88 Solar Domestic Hot Water Systems.
      fWaterDrawCurveHourly(1) = 9.
      fWaterDrawCurveHourly(2) = 7.
      fWaterDrawCurveHourly(3) = 0.
      fWaterDrawCurveHourly(4) = 0.
      fWaterDrawCurveHourly(5) = 0.
      fWaterDrawCurveHourly(6) = 0.
      fWaterDrawCurveHourly(7) = 0.
      fWaterDrawCurveHourly(8) = 8.
      fWaterDrawCurveHourly(9) = 18.
      fWaterDrawCurveHourly(10) = 15.
      fWaterDrawCurveHourly(11) = 16.
      fWaterDrawCurveHourly(12) = 16.
      fWaterDrawCurveHourly(13) = 14.
      fWaterDrawCurveHourly(14) = 13.
      fWaterDrawCurveHourly(15) = 12.
      fWaterDrawCurveHourly(16) = 10.
      fWaterDrawCurveHourly(17) = 9.
      fWaterDrawCurveHourly(18) = 10.
      fWaterDrawCurveHourly(19) = 12.
      fWaterDrawCurveHourly(20) = 16.
      fWaterDrawCurveHourly(21) = 17.
      fWaterDrawCurveHourly(22) = 14.
      fWaterDrawCurveHourly(23) = 12.
      fWaterDrawCurveHourly(24) = 11.
 
      fWaterDrawCurveTotal = 239.0
      RETURN
      END
      
c *******************************************************************
c REAL SUBROUTINE sDHW_CarryToNextStep()
c Carry Final water Temp in tank, and standby tank losses to the next step.
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group

c----56--1---------2---------3---------4---------5---------6---------7---------8

      SUBROUTINE DHW_CarryToNextStep()
!Variables Used From Module

      IMPLICIT NONE

#include "dhw_parameters.h"
#include "dhw_common.h"
c#include "ground_temp_mains_common.h" 

!Calculated Values

        fOldFinalWaterTemp =  fDHW_FinalWaterTemp()
        fOldStandbyTankLosses =  fDHW_StandbyTankLosses()
 
      RETURN
      END

 
 
c *******************************************************************
c REAL SUBROUTINE sDHW_Update
c Update variables that relate to ESP-r data.
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group

c----56--1---------2---------3---------4---------5---------6---------7---------8

      SUBROUTINE DHW_Update()
!Variables Used From Module

      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"
c#include "ground_temp_mains_common.h" 
 
C Note this should be changed to the zone temperature that  the tank is in. 
      fRoomTemp = 20.0
        
      RETURN
 
      END
      
c *******************************************************************
c SUBROUTINE DHW_Main
c Main DHW routine.
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group

c----56--1---------2---------3---------4---------5---------6---------7---------8

      SUBROUTINE DHW_Main
!Variables Used From Module

      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"
c#include "ground_temp_mains_common.h" 

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      common/sus/issny
      INTEGER   ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its  
      INTEGER   ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      INTEGER   ISSNY
      INTEGER   iTankNumber
      logical   bInStartUp

c If flag is set for no DHW sim then return
      IF (iDHW_Flag.ne.1) then
        RETURN
      ENDIF

c Check if in startup period via function bInStartup.
      if (bInStartup())then
        return
      endif
C        bInStartUp=.true.
C        IF((ISSNY.EQ.1.AND.IDYP.EQ.365.AND.IHRP.EQ.24.AND.ISDS.EQ.1)
C     &    .OR.(ISSNY.EQ.0.AND.(IDYP.GE.ISDS.OR.
C     &        (IDYP.EQ.ISDS-1.AND.IHRP.EQ.24))))bInStartUp=.false.

C If still in start up return.
C       IF (bInStartUp) then
C         RETURN
C       ENDIF

      fWaterDraw = fDHW_WaterDraw() 

c This function will calulate exactly how much load the current tank can support. 
      CALL fDHW_DrawsForEachTank()

c This determines the air temperature of the room in which the Hot water tank is located
      CALL DHW_Update()

c This routine calulates the energy use and sum the values for monthly analysis. 
      CALL DHW_StoreMonthlyValues()

c This will carry the standy tank losses and the tank teperatures to the next timestep.
      CALL DHW_CarryToNextStep()

      RETURN
      END
      
C ************************ DHW_READIN *******************************************
C Created by: Phylroy A. Lopez
C Initial Creation Date: July 31, 2001
C Copyright 2000: NRCan Buildings Group

C This subroutine reads the input data required by the DHW
C model (i.e. the contents of the .dhw file). It is called once per
C simulation when the other building-level input data files (e.g. .ctl file)
C are read in. All data read from the .dhw file are saved to variables declared
C in MODULE DHW_DATA.

C --------------------------------------------------------------------------------

      SUBROUTINE DHW_READIN

      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"
c#include "ground_temp_mains_common.h" 

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/C21/IFCFG,cfgroot,LCFGF

C Declarations for local variables and variables passed in common blocks.
      character cfgroot*24,LCFGF*72
      integer IFCFG
      character outs*124,OUTSTR*124 
      integer IUOUT,IUIN,IFIL,IUNIT,ISTAT,IER,ND,k,ival
      real val
      INTEGER   iTankNumber

c  REAL fDHWVersionNumber ! version number of DHW input file      

  
C Set the version number
c      fDHWVersionNumber = 0.      
C Open DHW input data file. Note that if IFIL only incremented by one, problems
C occur as this file unit is already in use.
      IUNIT=IFIL+10
      CALL ERPFREE(IUNIT,ISTAT)
      CALL EFOPSEQ(IUNIT,sDHW_InputFileName,1,IER)
      IF(ier.ne.0)goto 1000

c Addition of version number to DHW input file (October 2005). This is required
c for backward file compatability, because the ground properties are no longer 
c read in from this file. 
c They are calculated in /cetc/ground_temp_main.F from the climate data 
c (and DD calculations) based on Moore's method.

c Determine the version number of this file.
c If it is an old file (with no version number), then we will read in the 
c values of fMeanGroundTemp and fAmpGroundTempVar but they won't be used anywhere.
c For newer versions, these values won't be read in at all. 

c Get first line.
c      CALL STRIPC(IUNIT,OUTSTR,1,ND,1,' DHW data',IER)
c      IF(IER.NE.0)goto 1000
c      K=0

c-----Read the file version number, if applicable
c      if ( outstr(1:13).eq.'* FileVersion' ) then
c        k = 13
c        call egetwr(outstr,k,fDHWVersionNumber,0.01,100.,'F',
c     &              'version number',IER)
c-------if there is a version number, then we need to read in the next line of the input
c-------to read in the number of hot water tanks.
c        CALL STRIPC(IUNIT,OUTSTR,1,ND,1,' DHW data',IER)
c      ELSE   
c-------if there is no version number, continue with reading in the BASESIMP data
c        continue
c      ENDIF
        
C Read  Number of Hot Water Tanks.
      CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
      IF(IER.NE.0)goto 1000
      K=0

      CALL EGETWI(OUTSTR,K,IVAL,1,10,'F',' # of Hot Water Tanks ',IER)
      if(IER.NE.0)then
        call edisp(IUOUT,' Cannot read DHW Number of Tanks.')
        goto 1000
      endif
      iNumberOfTanks = IVAL

C Read  fNumOfOccupants of DHW.
      CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETWR(OUTSTR,K,VAL,0.,100.,'F',' fNumOfOccupants ',IER)
      if(IER.NE.0)then
        call edisp(IUOUT,' Cannot read DHW fNumOfOccupants.')
        goto 1000
      endif
      fNumOfOccupants = VAL

c ***********************************************************************
c This ground temperature information (fMeanGroundTemp and fAmpGroundTempVar) 
c is no longer read in from the .dhw input file. It is calculated in 
c /cetc/ground_temp_main.F based on Moore's method from the climate data.
c************************************************************************
c-----If there is no version number associated with the BASESIMP input file, then 
c-----the variables fMeanGroundTemp and are fAmpGroundTempVar read in, but not used
c-----in the simulation
c      IF(fDHWVersionNumber .EQ. 0.) then 
c Read  fMeanGroundTemp of DHW.
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
        IF(IER.NE.0)goto 1000
        K=0
C Update by Julia Purdy (November 5, 2003)
C Update to allow for negative mean ground temperatures. Some parts of
C the North will have negative ground temperatures (permafrost). 
        CALL EGETWR(OUTSTR,K,VAL, -10.,100.,'F',' fMeanGroundTemp ',IER)
        if(IER.NE.0)then
          call edisp(IUOUT,' Cannot read DHW fMeanGroundTemp.')
          goto 1000
        endif    
        fMeanGroundTemp = VAL   

C Read  fAmpGroundTempVar of DHW.
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
        IF(IER.NE.0)goto 1000
        K=0
        CALL EGETWR(OUTSTR,K,VAL,0.,100.,'F',' fAmpGroundTempVar ',IER)
        if(IER.NE.0)then
          call edisp(IUOUT,' Cannot read DHW fAmpGroundTempVar.')
          goto 1000
        endif
        fAmpGroundTempVar = VAL 

c-----If there is a version number associated with the BASESIMP input file, then 
c-----continue reading in the rest of the .dhw input file as normal.
c      ELSE
c        continue
c      END IF  
      
C Read  fHotSupplyTemp of DHW.
      CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETWR(OUTSTR,K,VAL,0.,99.,'F',' fHotSupplyTemp ',IER)
      if(IER.NE.0)then
        call edisp(IUOUT,' Cannot read DHW fHotSupplyTemp.')
        goto 1000
      endif
      fHotSupplyTemp = VAL 

    
      
c loop through tanks  (only one tank supported)  
      DO iTankNumber=1 , 1

C Read  DHW Zone Location.
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
        IF(IER.NE.0)goto 1000
        K=0

        CALL EGETWI(OUTSTR,K,IVAL,1,10,'F',' DHW Zone Location ',IER)
        if(IER.NE.0)then
          call edisp(IUOUT,' Cannot read DHW Zone Location.')
          goto 1000
        endif
        iDHWZoneLocation = IVAL

C Read  DHW type.
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
        IF(IER.NE.0)goto 1000
        K=0

        CALL EGETWI(OUTSTR,K,IVAL,1,6,'F',' DHW type ',IER)
        if(IER.NE.0)then
          call edisp(IUOUT,' Cannot read DHW type.')
          goto 1000
        endif
        iFuelType = IVAL

C Read  DHW Tank type.
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
        IF(IER.NE.0)goto 1000
        K=0

        CALL EGETWI(OUTSTR,K,IVAL,1,20,'F',' DHW tanktype ',IER)
        if(IER.NE.0)then
          call edisp(IUOUT,' Cannot read DHW Tank type.')
          goto 1000
        endif
        iTankType = IVAL

C Read  DOEEF of DHW.
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
        IF(IER.NE.0)goto 1000
        K=0
        CALL EGETWR(OUTSTR,K,VAL,0.,2.,'F',' DHW EF ',IER)
        if(IER.NE.0)then
          call edisp(IUOUT,' Cannot read DOEEF.')
          goto 1000
        endif
        fDOEEF = VAL

C Read  fHeatInjectorPower of DHW.
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
        IF(IER.NE.0)goto 1000
        K=0
        CALL EGETWR(OUTSTR,K,VAL,0.,1000000.,'F',
     &             ' DHW Heat Injector Power ',IER)
        if(IER.NE.0)then
          call edisp(IUOUT,' Cannot read DHW fHeatInjectorPower.')
          goto 1000
        endif
        fHeatInjectorPower = VAL      
          
C Read  fPilotEnergyRate of DHW.
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
        IF(IER.NE.0)goto 1000
        K=0
        CALL EGETWR(OUTSTR,K,VAL,0.,10000.,'F',' fPilotEnergyRate ',IER)
        if(IER.NE.0)then
          call edisp(IUOUT,' Cannot read DHW fPilotEnergyRate.')
          goto 1000
        endif
        fPilotEnergyRate = VAL      
    
C Read  fTankSize of DHW.
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
        IF(IER.NE.0)goto 1000
        K=0
        CALL EGETWR(OUTSTR,K,VAL,0.,5000.,'F',' fTankSize ',IER)
        if(IER.NE.0)then
          call edisp(IUOUT,' Cannot read DHW fTankSize.')
          goto 1000
        endif
        fTankSize = VAL       

C Read  fTemperatureBand of DHW.
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
        IF(IER.NE.0)goto 1000
        K=0
        CALL EGETWR(OUTSTR,K,VAL,0.,99.,'F',' fTemperatureBand ',IER)
        if(IER.NE.0)then
          call edisp(IUOUT,' Cannot read DHW fTemperatureBand.')
          goto 1000
        endif
        fTemperatureBand = VAL

C Read  fBlanket Insulation of DHW.
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
        IF(IER.NE.0)goto 1000
        K=0
        CALL EGETWR(OUTSTR,K,VAL,0.,99.,'F',' fBlanketRSI ',IER)
        if(IER.NE.0)then
          call edisp(IUOUT,' Cannot read DHW fBlanketRSI.')
          goto 1000
        endif
        fBlanketRSI = VAL

      END DO
      
C Close DHW input data file.
      CALL ERPFREE(IUNIT,ISTAT)
      goto 1001

C Error handling.
 1000 IER=1
      write(outs,7)
    7 format(' DHW input file error.DHW SIM CANCELLED')
      call edisp(iuout,outs)
      iDHW_Flag = 0 !sets flag not to perform DHW simulation. 

      
 1001 RETURN
      END
      
      
c *******************************************************************
c SUBROUTINE DHW_StoreMonthlyValues
c Initialize Variables (Hard coded for present).
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Copyright 2000: NRCan Buildings Group

c----56--1---------2---------3---------4---------5---------6---------7---------8

      SUBROUTINE DHW_StoreMonthlyValues()
!Variables Used From Module

      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"
c#include "ground_temp_mains_common.h"

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
 
      INTEGER   IODS,IODF,ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      INTEGER   ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
        CHARACTER*128  outs
      CALL EDAY(ISD1,ISM1,IODS)
      CALL EDAY(ISD2,ISM2,IODF)
      CALL EDAYR(idyp,iDay,iMonth)
      if (idyp.ge.IODS.and.idyp.le.IODF) then

        fSumTotalEnergy(iMonth) =
     &    fSumTotalEnergy(iMonth) + fDHW_FuelEnergyRequired()
     
        fSumTotalEnergyToSpace(iMonth) =
     &    fSumTotalEnergyToSpace(iMonth) + fDHW_EnergyToZone()
     
        fSumTotalPipeLosses(iMonth) =
     &    fSumTotalpipeLosses(iMonth) + fDHW_PipeLosses()

        fSumTotalPipeGains(iMonth) =
     &    fSumTotalPipeGains(iMonth) + fDHW_PipeGains()
     
        fSumStandbyTankLosses(iMonth) = 
     &    fSumStandbyTankLosses(iMonth) + fDHW_StandByTankLosses()
     
        fSumEnergyInputWater(iMonth) = 
     &    fSumEnergyInputWater(iMonth) + fDHW_EnergyInputWater()
    
        fSumFlueLosses(iMonth) = 
     &    fSumFlueLosses(iMonth) + fDHW_FlueLosses()  
       
        fSumPilotEnergyToRoom(iMonth) = 
     &    fSumPilotEnergyToRoom(iMonth) + fDHW_PilotEnergyToRoom()
     
        fCold_Main_Temp(iMonth) =  fDHW_ColdMainTemp()    
c        fCold_Main_Temp(iMonth) =  fDHW_ColdMainTemp(iMonth)    
    
        fSumWaterLoad(iMonth)=
     &    fSumWaterLoad(iMonth) + fWaterDraw

        fSumPilotEnergy(imonth) = 
     &    fDHW_PilotEnergy() +  fSumPilotEnergy(imonth)
 
      END IF
      RETURN
      END
      
      
c*******************************************************************
c REAL SUBROUTINE H3KOUPUT_DHW
c Initialize Variables.
c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Updated: November 5 2003 (JPurdy)
c Copyright 2000: NRCan Buildings Group

c----56--1---------2---------3---------4---------5---------6---------7---------8

      SUBROUTINE H3KOUTPUT_DHW(IWU)
      !Variables Used From Module
       
      
      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"  
c#include "ground_temp_mains_common.h"
    
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      DIMENSION MTHNAM(12)
      CHARACTER DS*7,DS1*10,DE*7,DE1*10,MTHNAM*3
      INTEGER   ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP,IWU
      INTEGER   iTankNumber
      REAL   fSumTotalEnergyYear
      character*256 outs
      DATA MTHNAM/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &     'Sep','Oct','Nov','Dec'/ 

      fSumTotalEnergyYear = 0.
      IF (iDHW_Flag.ne.1) then
         RETURN
      ENDIF
      write(outs,'(a)')' '
      call edisp(IWU,outs)   
      write(outs,'(a)')'********DHW Information*******'
      call edisp(IWU,outs)      
      write(outs,'(15a)')
     &     'Month ',
     &     'Cold_Main_Temp(C) ',
     &     'WaterLoad(L/Mo) ',
     &     'FGasFactor ',     
     &     'FTankFactor ',
     &     'UA ',    
     &     'WaterHeatEnergy(MJ) ',
     &     'TankLosses(MJ) ',
     &     'FlueLosses(MJ) ',
     &     'PilotEnerToRoom(MJ) ',
     &     'PilotEnergy(MJ) ',
     &     'TotalEnergy(MJ) ', 
     &     'EnergyToSpace(MJ) ',
     &     'PipeLosses(MJ) ',
     &     'PipeGains(MJ) '
      call edisp(IWU,outs)
      DO iTankNumber = 1, 1
        DO imonth=ISM1,ISM2
          write(outs,
     &      '(a3,1x,f10.3,1x,f10.3,1x,f10.3,1x,f10.3,1x,
     &        f10.3,1x,f10.3,1x,f10.3,1x,f10.3,1x,f10.3,1x,
     &        f10.3,1x,f10.3,1x,f10.3,1x,f10.3,1x,f10.3,1x,f10.3)')
C Future form:
c     &         MTHNAM(imonth),fDHW_ColdMainTemp(iMonth),
C Present form:
     &        MTHNAM(imonth),fDHW_ColdMainTemp(),
     &        fSumWaterLoad(iMonth),fFGas,fFTank,fUA,    
     &        fSumEnergyInputWater(iMonth)/1e6-
     &        fSumStandByTankLosses(iMonth)/1e6,
     &        fSumStandByTankLosses(iMonth)/1e6,
     &        fSumFlueLosses(iMonth)/1e6,
     &        fSumPilotEnergyToRoom(iMonth)/1e6,
     &        fSumPilotEnergy(imonth)/1e6,
     &        fSumTotalEnergy(iMonth)/1e6, 
     &        fSumTotalEnergyToSpace(imonth)/1e6,
     &        fSumTotalPipelosses(imonth)/1e6, 
     &        fSumTotalPipeGains(imonth)/1e6 
        call edisp(IWU,outs)

        fSumTotalEnergyYear = fSumTotalEnergyYear 
     &           + fSumTotalEnergy(iMonth)/1e6 

        END DO
      END DO
      write(outs,'(a30)')' '
      call edisp(IWU,outs)   
      WRITE (outs,'(a20,1x,I10)')'#Number of Tanks',iNumberOfTanks
      call edisp(IWU,outs)
      WRITE (outs,'(a20,1x,f10.2)')'#fNumOfOccupants',fNumOfOccupants
      call edisp(IWU,outs)
      WRITE (outs,'(a20,1x,f10.2)')'#Mean Ground Temp',fMeanGroundTemp
c      WRITE (outs,'(a20,1x,f10.2)')'#Mean Ground Temp',fTemp_Ground_Avg
      call edisp(IWU,outs)
      WRITE (outs,'(a20,1x,f10.2)')'#Ground Temp Amp',fAmpGroundTempVar
c      WRITE (outs,'(a20,1x,f10.2)')'#Ground Temp Amp',fTemp_Ground_Amp
      call edisp(IWU,outs)
      WRITE (outs,'(a20,1x,f10.2)')'#fHotSupplyTemp',fHotSupplyTemp
      call edisp(IWU,outs)
c     Should add iterator to go through multiple tanks when that functionality is needed (if ever).
      WRITE (outs,'(a20,1x,f10.2)')'#fDOEEF',fDOEEF
      call edisp(IWU,outs)
      WRITE (outs,'(a20,1x,f10.2)')'#fAlpha',fAlpha
      call edisp(IWU,outs)
      WRITE (outs,'(a30,1x,f10.2)')'#TOTAL DHW Energy Used(MJ)',
     &     fSumTotalEnergyYear
      call edisp(IWU,outs)
      RETURN
      END

