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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"


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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"
      
      real fDHW_WaterDraw_byVolume, fDummy
      
      fDHW_WaterDraw = fDHW_WaterDraw_byVolume(fDailyDrawVolL,
     &                                 bDefinedLoad,
     &                                 fHotSupplyTemp,
     &                                 fUsageTemp,
     &                                 fDummy )
      
      
      
      
      return 
      end function fDHW_WaterDraw
      
      real FUNCTION fDHW_WaterDraw_byVolume(DrawVolL,
     &                                 bLoadIsDefined,
     &                                 fTempHWT,
     &                                 fTempUsage,
     &                                 fTempColdSupply) 
      
      IMPLICIT NONE
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"

  

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      INTEGER   ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      REAL   fNormalizedDailyWaterDraw
      REAL   fLitresforNumOccupants
      integer h3k_month
      real DrawVolL, fTempHWT, fTempUsage, fTempColdSupply
      logical bLoadIsDefined
      
c This calulates the amount of water  used based on the the number of occupants
        if ( bLoadIsDefined ) then 
          fLitresforNumOccupants = DrawVolL
        else
          fLitresforNumOccupants = 85.0 + ( 35.0 * fNumOfOccupants )
        endif
        
        !write(*,*) "fLitresforNumOccupants ", fLitresforNumOccupants 
c This normalises the water draw based on a standard usage temperature (currently a parameter
c of 60C. This is from B.Bradley'd H2KDHW.F line 254.
        iMonth = h3k_month(real(idyp)+real(ihrp)/24)
        !write(*,*) "iMonth", iMonth
        fTempColdSupply = fDHW_ColdMainTemp(iMonth)
        
        fNormalizedDailyWaterDraw = ( 0.75
     &                   * fLitresforNumOccupants
     &                   * ( (fTempUsage
     &                           - fDHW_ColdMainTemp(iMonth))
     &                   / (fTempHWT
     &                          - fDHW_ColdMainTemp(iMonth)) )
     &                   + 0.25 * fLitresforNumOccupants )

C      write (*,*) " DHW: ih2k ", iH2k , bLoadIsDefined, DrawVolL, 
C     & fTempUsage, fTempHWT,
C     & fNormalizedDailyWaterDraw, fDHW_ColdMainTemp(iMonth), 
C     & REAL(iNTSTEP)
     
      IF (iH2K.eq.1) then ! treat it like H2K
        fDHW_WaterDraw_byVolume
     &        = fNormalizedDailyWaterDraw
     &                  /fHoursInDay / REAL(iNTSTEP)

   
      ELSE   !Treat it with water demand curve contained in fWaterDrawCurveHourly(24).
c This formula will first obtain the percentage of total daily water which has been drawn in that hour.
        fDHW_WaterDraw_byVolume
     &        = fWaterDrawCurveHourly(ihrp)
     &                 / fWaterDrawCurveTotal
c Then this will muliply it by the actual water draw and divide it by the time step slice.
     &                   * fNormalizedDailyWaterDraw / REAL(iNTSTEP)
      ENDIF

      
C      write (*,*) "fHoursInDay:", fHoursInDay
C      write (*,*) "fWaterDrawCurveTotal", fWaterDrawCurveTotal
c Note the draw is considered instantaneous ie flow = infinite during draw period

C Save water draw volume in (l/s)
C
C                l       # timesteps      1 hour
C       l/s = -------- * ----------- * ------------
C             timestep       hour      3600 seconds
C
C
        fDHW_H3K_water_draw = fDHW_WaterDraw * real (iNTSTEP)
     &                                       * ( 1.0 / 3600.0 ) ! (l/s)

C Save supply and delivery temperatures (oC)
        fDHW_H3K_supply_temp = fDHW_ColdMainTemp(iMonth)
        fDHW_H3K_delivery_temp = fTempHWT

C        write (*,*) " >>> fwd", fDHW_WaterDraw_byVolume 
C        write (*,*) " fDHW_WaterDraw h2k :", fDHW_H3K_water_draw
C        write (*,*) " fDHW 33k supp temp" , fDHW_H3K_supply_temp
C        write (*,*) " fDHW 33k delv temp" , fDHW_H3K_delivery_temp
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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"


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
        fDHW_StartTemp = fDHW_ColdMainTemp(iMonth)
      ELSE

C Energy in Tank
C Energy in Joules of Hot water in tank
        fEnergyInHotWaterPortion =
     &    fDHW_WaterEnergyFromTemp(fOldFinalWaterTemp,
     &                             fHotWaterVolumeRemaining)

c Energy in Joules of Cold water in tank
        fEnergyInColdWaterPortion =
     &   fDHW_WaterEnergyFromTemp(fDHW_ColdMainTemp(iMonth),
     &                              fWaterDraw)


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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"
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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"


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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"

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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"


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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"

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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"

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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"

      REAL   fEnergy


c Formula
      SELECT CASE (iTankType)

      Case (Gas_HP_WH)
        fEnergy = fDHW_EnergyInputWater()/fDOEEF
        fHPRoomLoad = fDHW_EnergyInputWater() - fEnergy
          
      
      CASE (Elec_Tankless_Heatpump)
       fEnergy     = fDHW_EnergyInputWater()/fDOEEF
       fHPRoomLoad = fDHW_EnergyInputWater() - fEnergy

      CASE (Elec_Heatpump)
       fEnergy = fDHW_EnergyInputWater()/fDOEEF
       fHPRoomLoad = fDHW_EnergyInputWater() - fEnergy

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

C.....Save data for H3KReports output
      fDHW_H3K_energy_requirement = fDHW_FuelEnergyRequired
     &                                       * real (iNTSTEP)
     &                                       * ( 1.0 / 3600.0 ) ! (W)

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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"

c Formula
c Parasitic losses to room from water in tank losing energy.
c Pipe Losses (very small..may eliminate in future)
c Portion of Pilot energy that is transmitted to room.

      if ( iTankType ==  Elec_Heatpump .or. 
     &     iTankType ==  Elec_Tankless_Heatpump .or.
     &     iTankType ==  Gas_HP_WH ) then 
      
        fDHW_EnergyToZone =
     &    + fDHW_StandByTankLosses()
     &    + fDHW_PipeGains()
     &    + fDHW_PipeLosses()
     &    + fDHW_PilotEnergyToRoom()
     &    - fHPRoomLoad
      else 
      
        fDHW_EnergyToZone =
     &    + fDHW_StandByTankLosses()
     &    + fDHW_PipeGains()
     &    + fDHW_PipeLosses()
     &    + fDHW_PilotEnergyToRoom()

      endif 
C.....Save energy transfer to room (W)
      fDHW_H3K_heat_xfer_to_room = fDHW_EnergyToZone
     &                                       * real (iNTSTEP)
     &                                       * ( 1.0 / 3600.0 ) ! (W)
     

      END

C *********************************************************************
c Function fIdealDHWGain
C
C Report the heat gain from a water heater so that it can be imposed
C on a zone.

      real function fIdealDHWGain(iZone) 
      implicit none 
      
      include "building.h"
      include "dhw_parameters.h"
      include "dhw_common.h"
      include "ground_temp_mains_common.h"

      integer iZone 
      
      if ( iZone == iDHWZoneLocation ) then 
      
        fIdealDHWGain = fDHW_H3K_heat_xfer_to_room
      
      else 
      
        fIdealDHWGain = 0 
      
      endif 
      return 
      
      end function fIdealDHWGain
      
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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"

c Formula

c Pipe gains to Cold Water Main from space obtained from Brian bradley's H2K model
c H2KDHW.F Line 857
        fDHW_PipeGains = ( -3.7 + 0.37 *
     &                   ( fDHW_ColdMainTemp(iMonth) - 10. ) -
     &                 0.83 * ( fNumOfOccupants - 3.5 ) )
     &                 / fSecondsINDAY
     &                 * fSecondsInHour/iNTSTEP
     &                 * 1e6   ! MJ to Joules


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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"

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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"

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

      CASE (Gas_HP_WH)
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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"


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

C.....Save standby skin losses (W)
      fDHW_H3K_skin_loss = fDHW_StandbyTankLosses
     &                                       * real (iNTSTEP)
     &                                       * ( 1.0 / 3600.0 ) ! (W)

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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"


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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"


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
     &        -fDHW_WaterEnergyFromTemp(fDHW_ColdMainTemp(iMonth),
     &                              fWaterDrawGreaterThanTank)

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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"

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

C.....Energy transfer to water
      fDHW_H3K_energy_transfer_to_water = fEnergyinputWater
     &                                       * real (iNTSTEP)
     &                                       * ( 1.0 / 3600.0 ) ! (W)

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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"

!Local Variables
      REAL   fTotalEnergyInTank
      REAL   fEnergyInTank
      REAL   fHotWaterVolumeRemaining

C Determine if volume of draw was greater than tank volume.
      fHotWaterVolumeRemaining =  fTankSize-fWaterDraw
      IF (fHotWaterVolumeRemaining.le.0.0) then

c If so set Hot water volume remaining to zero.
        fHotWaterVolumeRemaining = 0.0
c Determine energy in volume of water draw after heat injection for burner
        fEnergyInTank =
     &    fDHW_WaterEnergyFromTemp(fDHW_ColdMainTemp(iMonth),
     &                             fWaterDraw)
     &    + fDHW_EnergyInputWater()

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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"

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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"

c Local variables
      SELECT CASE (iFuelType)
      CASE (ELECTRICITY,SOLAR)
        fDHW_FlueLosses = 0.0
      CASE (OIL,NATURALGAS,WOOD,PROPANE)
        fDHW_FlueLosses = (fDHW_EnergyInputWater()
     &                    -fDHW_standbyTankLosses())
     &                    * fDHW_FlueGasLossFactor()
      END SELECT

      fDHW_H3K_flue_loss = fDHW_FlueLosses * real (iNTSTEP)
     &                                     * ( 1.0 / 3600.0 ) ! (W)

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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"

      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep


C Declarations for local variables and variables passed in common blocks.
      INTEGER   ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,iDAYN
      INTEGER   isd1,ism1,isd2,ism2,isds,isdf,ntstep
      character cfgroot*24,LCFGF*72
      integer IFCFG

C Define default draw profile 

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
          
      
C Initialise to default values.

      iNTSTEP = ntstep

      fRoomTemp = 20.0



      fOldAverageWaterTemp =  55.0
      fOldFinalWaterTemp =  55.0
      fOldStandbyTankLosses  = 0.0

C Check to see if DHW is required.
      IF (iDHW_Flag.ne.1) then
        RETURN
      ENDIF

      fUA = fDHW_UA()

      fFGas = fDHW_FlueGasLossFactor()

      fFTank =  fDHW_DOEEFTank()

C For the case of DHW input files with FileVersion numbers greater than 2, the hourly
C and daily DHW load is defined in the DHW input file, so skip the next section.
      check_DHW_draws_in_file: if  ( fDHWVersionNumber .ge. 2.0 ) then
          continue

c This water draw curve was taken from the CSA f379.1-88 Solar Domestic Hot Water Systems.
      else

      endif check_DHW_draws_in_file
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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"

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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"

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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"

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
      fDHW_H3K_water_draw = fWaterDraw  * real (iNTSTEP)
     &                                       * ( 1.0 / 3600.0 ) ! (l/s)
      
c This function will calulate exactly how much load the current tank can support.
      CALL fDHW_DrawsForEachTank()

c This determines the air temperature of the room in which the Hot water tank is located
      CALL DHW_Update()

c This routine calulates the energy use and sum the values for monthly analysis.
      CALL DHW_StoreMonthlyValues()

c This will carry the standy tank losses and the tank teperatures to the next timestep.
      CALL DHW_CarryToNextStep()

C Report data to h3kreports, site-utilities facility
      call H3KReportDHWModule()

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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"
#include "espriou.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/C21/IFCFG,cfgroot,LCFGF

C Declarations for local variables and variables passed in common blocks.
      character cfgroot*24,LCFGF*72
      integer IFCFG
      character outs*124,OUTSTR*124
      integer IUOUT,IUIN,IFIL,IUNIT,ISTAT,IER,ND,k,ival
      real val
      INTEGER  iTankNumber
      logical error_flag, close, close2, close3
      integer iFileLine           ! Line number in file
      character*248 cLnWords(124) ! words on the line
      character*248 cLnBuffer 
      integer iWordCount
      integer iEGetArrW
      real fCtoR_err             ! Convert a char to a real

C Set the version number
      fDHWVersionNumber = 0.

C Set error flag to false
      error_flag = .false.

C Open DHW input data file. Note that if IFIL only incremented by one, problems
C occur as this file unit is already in use.
      IUNIT=IFIL+10
      write (currentfile, '(A)') sDHW_InputFileName
      CALL ERPFREE(IUNIT,ISTAT)
      CALL EFOPSEQ(IUNIT,sDHW_InputFileName,1,IER)
      IF(ier.ne.0) error_flag = .true.

c Addition of version number to DHW input file (October 2005). This is required
c for backward file compatability.
c If there is a version number > 0 at the top of the dhw file, the ground temperature
c variables fDHW_ground_temp_avg and fDHW_ground_temp_amp are calculated in
c file /esrubld/ground_temp_main.F from the climate data (and DD calculations)
c based on Moore's method.

C Addition of hourly draw profile to dhw input file (JP - Feb 2008)
C If the version number is > 2, 24 hourly draw profiles are included in the .dhw file.

c Determine the version number of this file.
c If it is an old file (with no version number or version number = 0), then we
c will read in the values of fDHW_ground_temp_avg and fDHW_ground_temp_amp.
c For newer versions, these values won't be read in at all and they are
c calculated internally instead.

c Get first line.
      CALL STRIPC(IUNIT,OUTSTR,1,ND,1,' DHW data',IER)
      IF(IER.NE.0) error_flag = .true.
      K=0

c-----Read the file version number, if applicable
      if ( outstr(1:12).eq.'*FileVersion' ) then
        k = 12
        call egetwr(outstr,k,fDHWVersionNumber,0.01,100.,'F',
     &              'version number',IER)
c-------if there is a version number, then we need to read in the next line of the input
c-------to read in the number of hot water tanks.
        CALL STRIPC(IUNIT,OUTSTR,1,ND,1,' DHW data',IER)
        k=0
      ELSE
c-------if there is no version number, continue with reading dhw data
        continue

      ENDIF

      if(.not. error_flag) then
        CALL EGETWI(OUTSTR,K,IVAL,1,10,'F',' # of Hot Water Tanks ',IER)
        if(IER.NE.0)then
          call edisp(IUOUT,' Cannot read DHW Number of Tanks.')
          error_flag = .true.
        endif
        iNumberOfTanks = IVAL
      endif

C Read  Next line. Could contain # of occupants, or *LoadData. fNumOfOccupants of DHW
      if(.not. error_flag) then
      
        call LStripc_LN (iUnit, cLnBuffer, 0, ND, 0,
     &            'DHW data',
     &             IER, iFileLine)      
      
        OUTSTR = cLnBuffer(1:124)
      
        IF(IER.NE.0) error_flag = .true.
        K=0
      endif
      
      if ( OUTSTR(1:9) == '*LoadData' ) then 
      
        bDefinedLoad = .true. 
        
C.......Format is: *LoadData <#-of-occupants>  <Litres-Per-Day> 
C.......
        iWordCount = iEGetArrW ( cLnBuffer, cLnWords )
        
        fNumOfOccupants = fCtoR_err(
     &               cLnWords(2),
     &               'Intrepreting number of occupants',
     &               error_flag)
      
        fDailyDrawVolL = fCtoR_err(
     &               cLnWords(3),
     &               'Intrepreting number of occupants',
     &               error_flag)
      
      
      else 
      
          if(.not. error_flag) then
            CALL EGETWR(OUTSTR,K,VAL,0.,100.,'F',' fNumOfOccupants '
     &                 ,IER)
            if(IER.NE.0)then
              call edisp(IUOUT,' Cannot read DHW fNumOfOccupants.')
              error_flag = .true.
            endif
            fNumOfOccupants = VAL
          endif
      endif 
c ***********************************************************************
c This ground temperature information (fDHW_ground_temp_avg and
c fDHW_ground_temp_amp) is read in and used in the calculations only
c when the dhw input file format is of an older version (0. in this case)
c or the top of the dhw input file does not contains a "FileVersion" at all.
c************************************************************************
c-----If there is no version number associated with the dhw input file, then
c-----the variables fDHW_ground_temp_avg and fDHW_ground_temp_amp are read in
      CALL ECLOSE(fDHWVersionNumber,0.0,0.0001,CLOSE)
      CALL ECLOSE(fDHWVersionNumber,1.0,0.0001,CLOSE2)
      CALL ECLOSE(fDHWVersionNumber,2.0,0.0001,CLOSE3)
      IF(CLOSE) then
c Read  fDHW_ground_temp_avg of DHW.
        if(.not. error_flag) then
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
          IF(IER.NE.0) error_flag = .true.
          K=0
        endif

        if(.not. error_flag) then
          CALL EGETWR(OUTSTR,K,VAL, -10.,100.,'F','
     &                fDHW_ground_temp_avg ',IER)
          if(IER.NE.0)then
            call edisp(IUOUT,
     &      ' Cannot read DHW fDHW_ground_temp_avg')
            error_flag = .true.
          endif
          fDHW_ground_temp_avg = VAL
        endif

C Read  fDHW_ground_temp_amp of DHW.
        if(.not. error_flag) then
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
          IF(IER.NE.0) error_flag = .true.
          K=0
        endif

        if(.not. error_flag) then
          CALL EGETWR(OUTSTR,K,VAL,0.,100.,'F','
     &                fDHW_ground_temp_amp ',IER)
          if(IER.NE.0)then
            call edisp(IUOUT,
     &      ' Cannot read DHW fDHW_ground_temp_amp')
            error_flag = .true.
          endif
          fDHW_ground_temp_amp = VAL
        endif

c In this case the ground temperature evaluation method is based on user inputs
c Set flag to 0
        iDHW_ground_temp_cal_flag = 0

c-----If there is a version number associated with the dhw input file, then
c-----continue reading in the rest of the .dhw input file as normal.
c-----In this case the ground average temperature and amplitude are based on
c-----calculations using the weather file. Values for the ground temperature
c-----average and amplitude are stored in common Ground_Temp in the include
c-----file "ground_temp_mains_common"
      ELSEIF(CLOSE2 .or. CLOSE3) THEN
c In this case the ground temperature evaluation method flag is set to 1
        iDHW_ground_temp_cal_flag = 1
        continue
      END IF

C Read  fHotSupplyTemp of DHW.
      if(.not. error_flag) then
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
        IF(IER.NE.0) error_flag = .true.
        K=0
      endif

      if(.not. error_flag) then
        CALL EGETWR(OUTSTR,K,VAL,0.,99.,'F',' fHotSupplyTemp ',IER)
        if(IER.NE.0)then
          call edisp(IUOUT,' Cannot read DHW fHotSupplyTemp.')
          error_flag = .true.
        endif
        fHotSupplyTemp = VAL
      endif

c loop through tanks  (only one tank supported)
      DO iTankNumber=1 , 1

C Read  DHW Zone Location.
        if(.not. error_flag) then
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
          IF(IER.NE.0) error_flag = .true.
          K=0
        endif

        if(.not. error_flag) then
          CALL EGETWI(OUTSTR,K,IVAL,1,10,'F',' DHW Zone Location ',IER)
          if(IER.NE.0)then
            call edisp(IUOUT,' Cannot read DHW Zone Location.')
            error_flag = .true.
          endif
          iDHWZoneLocation = IVAL
        endif

C Read  DHW type.
        if(.not. error_flag) then
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
          IF(IER.NE.0) error_flag = .true.
          K=0
        endif

        if(.not. error_flag) then
          CALL EGETWI(OUTSTR,K,IVAL,1,6,'F',' DHW type ',IER)
          if(IER.NE.0)then
            call edisp(IUOUT,' Cannot read DHW type.')
            error_flag = .true.
          endif
          iFuelType = IVAL
        endif

C Read  DHW Tank type.
        if(.not. error_flag) then
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
          IF(IER.NE.0) error_flag = .true.
          K=0
        endif

        if(.not. error_flag) then
          CALL EGETWI(OUTSTR,K,IVAL,1,21,'F',' DHW tanktype ',IER)
          if(IER.NE.0)then
            call edisp(IUOUT,' Cannot read DHW Tank type.')
            error_flag = .true.
          endif
          iTankType = IVAL
        endif

C Read  DOEEF of DHW.
        if(.not. error_flag) then
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
          IF(IER.NE.0) error_flag = .true.
          K=0
        endif

        if(.not. error_flag) then
          CALL EGETWR(OUTSTR,K,VAL,0.,4.,'F',' DHW EF ',IER)
          if(IER.NE.0)then
            call edisp(IUOUT,' Cannot read DOEEF.')
            error_flag = .true.
          endif
          fDOEEF = VAL
        endif

C Read  fHeatInjectorPower of DHW.
        if(.not. error_flag) then
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
          IF(IER.NE.0) error_flag = .true.
          K=0
        endif

        if(.not. error_flag) then
          CALL EGETWR(OUTSTR,K,VAL,0.,1000000.,'F',
     &             ' DHW Heat Injector Power ',IER)
          if(IER.NE.0)then
            call edisp(IUOUT,' Cannot read DHW fHeatInjectorPower.')
            error_flag = .true.
          endif
          fHeatInjectorPower = VAL
        endif

C Read  fPilotEnergyRate of DHW.
        if(.not. error_flag) then
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
          IF(IER.NE.0) error_flag = .true.
          K=0
        endif

        if(.not. error_flag) then
          CALL EGETWR(OUTSTR,K,VAL,0.,10000.,'F','
     &                fPilotEnergyRate ',IER)
          if(IER.NE.0)then
            call edisp(IUOUT,' Cannot read DHW fPilotEnergyRate.')
            error_flag = .true.
          endif
          fPilotEnergyRate = VAL
        endif

C Read  fTankSize of DHW.
        if(.not. error_flag) then
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
          IF(IER.NE.0) error_flag = .true.
          K=0
        endif

        if(.not. error_flag) then
          CALL EGETWR(OUTSTR,K,VAL,0.,5000.,'F',' fTankSize ',IER)
          if(IER.NE.0)then
            call edisp(IUOUT,' Cannot read DHW fTankSize.')
            error_flag = .true.
          endif
          fTankSize = VAL
        endif

C Read  fTemperatureBand of DHW.
        if(.not. error_flag) then
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
          IF(IER.NE.0) error_flag = .true.
          K=0
        endif

        if(.not. error_flag) then
          CALL EGETWR(OUTSTR,K,VAL,0.,99.,'F',' fTemperatureBand ',IER)
          if(IER.NE.0)then
            call edisp(IUOUT,' Cannot read DHW fTemperatureBand.')
            error_flag = .true.
          endif
          fTemperatureBand = VAL
        endif

C Read  fBlanket Insulation of DHW.
        if(.not. error_flag) then
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' DHW data',IER)
          IF(IER.NE.0) error_flag = .true.
          K=0
        endif

        if(.not. error_flag) then
          CALL EGETWR(OUTSTR,K,VAL,0.,99.,'F',' fBlanketRSI ',IER)
          if(IER.NE.0)then
            call edisp(IUOUT,' Cannot read DHW fBlanketRSI.')
            error_flag = .true.
          endif
          fBlanketRSI = VAL
        endif

      END DO


C Close DHW input data file.
      if(.not.error_flag) then
        CALL ERPFREE(IUNIT,ISTAT)
      endif

C Error handling.
      if(error_flag) then
        IER=1
        write(outs,7)
    7   format(' DHW input file error.DHW SIM CANCELLED')
        call edisp(iuout,outs)
        iDHW_Flag = 0 !sets flag not to perform DHW simulation.
        stop ' Fatal error: Cannot read DHW simulation file.'
      endif

      RETURN
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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"

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

        fCold_Main_Temp(iMonth) =  fDHW_ColdMainTemp(iMonth)

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
#include "building.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"

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
     &         MTHNAM(imonth),fDHW_ColdMainTemp(iMonth),
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
      WRITE (outs,'(a20,1x,f10.2)')'#Mean Ground Temp',
     &                              fDHW_ground_temp_avg
      call edisp(IWU,outs)
      WRITE (outs,'(a20,1x,f10.2)')'#Ground Temp Amp',
     &                              fDHW_ground_temp_amp
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

C=H3KReportDHWModule====================================================
C
C Abstract:
C
C This procedure transports data from the DHW module to the
C H3Kreports and Site Utilities facilities.
C
C Inputs: None
C
C Outputs: None
C
C=======================================================================

      subroutine H3KReportDHWModule()
      use h3kmodule
      implicit none
#include "dhw_common.h"
#include "OffsiteUtilitiesPublic.h"

C.....Store energy requirement in site-utilities facility storage
C.....array. (W)
      fSUFuelEnergyUse( iFuelType, iUseDHW ) =
     &    fDHW_H3K_energy_requirement
C.....Pass data to storage facility.
      call StoreSiteEnergyUse(iIdealDHW, fSUFuelEnergyUse )


C.....Report interesting parameters:
      Call AddToReport(rvPltDHWWtr%Identifier,
     &  fDHW_H3K_water_draw)

      Call AddToReport(rvPltDHWEnIn%Identifier,
     &  fDHW_H3K_energy_requirement)

       Call AddToReport(rvPltDHWSupTmp%Identifier,
     &  fDHW_H3K_supply_temp)

      Call AddToReport(rvPltDHWDlvTmp%Identifier,
     &  fDHW_H3K_delivery_temp)

      Call AddToReport(rvPltDHWBrnHOut%Identifier,
     &  fDHW_H3K_energy_transfer_to_water)

      Call AddToReport(rvPltDHWSknLss%Identifier,
     &  fDHW_H3K_skin_loss)

      Call AddToReport(rvPltDHWFluLss%Identifier,
     &  fDHW_H3K_flue_loss)

      Call AddToReport(rvPltDHWHTrnBld%Identifier,
     &  fDHW_H3K_heat_xfer_to_room)

      
    !  write (*,*)"   >2> fDHW_WaterDraw h2k :", fDHW_H3K_water_draw
    !  write (*,*)"   >2> fDHW 33k supp temp" , fDHW_H3K_supply_temp
    !  write (*,*)"   >2> fDHW 33k delv temp" , fDHW_H3K_delivery_temp
    ! 
    ! 
    !  print*, ">>>", fDHW_H3K_water_draw, fDHW_H3K_delivery_temp,
    ! &   fDHW_H3K_supply_temp
     
     
      Call AddToReport(rvPltDHWWtrHLd%Identifier,
     &  fDHW_H3K_water_draw * 4106.0
     &      * ( fDHW_H3K_delivery_temp - fDHW_H3K_supply_temp))


      return
      end
