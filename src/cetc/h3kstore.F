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

C*****************************************************************
c     This file contain all of the storage functions that set up the variables
c     used for output to the h3k interface via flat file. These store values
c     are listed in h3koutput.h and are outputted via the functions in h3koutput.F
c     SUBROUTINES:
c     H3KSTORE_ZERO_DATA : Zeros Data in MZUMA
c     H3KSTORE_HVAC_MONTHLY: Collects HVAC Energy Information from HVACSIM
c     H3KSTORE_AIM2_DATA: Infiltration data from AIM2_CONTROL
c     H3KSTORE_FNDTN_LOSSES:BSIMP Foundation loses MZADJC
c     H3KSTORE_SUM_PLR_BIN_HOURS: Capacity data used in FURNACE
c     H3KSTORE_PLANT_ELEC: Calculates the plant electrical power

c     *********************************************************************
c     H3KSTORE_ZERODATA
c     Output Monthly Energy Used by HVAC systems
c     Created by: Phylroy A. Lopez
c     Initial Creation Date:  March 23th 2001
c     Copyright 2000: NRCan Buildings Group

c     INPUTS:
c     None
c     OUTPUTS:
c     None.
c     COMMENTS:
c     This routine zeros all variables used in the h3koutput.h routines.
c     This routine is currently only called at the begining of MZNUMA.
c**********************************************************************
      SUBROUTINE H3KSTORE_ZERO_DATA
      IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"



C Local Variables
        INTEGER iSystem
        INTEGER iPercent
        INTEGER iMonth
        INTEGER iMonths
        INTEGER iZone



c     This Iterates through the systems
      do 1 isystem=1,max_sys

c     Variables used 'H3KSTORE_SUM_PLR_BIN_HOURS'
         total_num_of_hours(isystem) = 0.0
         underheated_hours(isystem) = 0.0

c     This iterates trhough the PLR percent bins
         do 3 ipercent = 1,10
            sum_plr_capacity_bin(isystem,ipercent) = 0.0
 3       continue

c     This iterates though the months
         do 2 imonth=1,months

c HVAC Variables
           fheat_energy_monthly(isystem,imonth) = 0.0
           fcool_energy_monthly(isystem,imonth) = 0.0
           ffan_energy_monthly(imonth) = 0.0
           fGSHP_pump_energy_monthly(imonth) = 0.0
           fGCEP_pump_energy_monthly(imonth) = 0.0
           fpilot_energy_monthly(isystem,imonth) = 0 .0
           fheating_load_monthly(imonth) = 0.0
           fcooling_load_monthly(imonth) = 0.0
c     AIM2 Variables (infiltration data)
           fmax_air_change(imonth) = 0.0
           imax_air_change_day(imonth) = 0.0
           imax_air_change_hour(imonth) = 0.0
           fmin_air_change(imonth) = 0.0
           imin_air_change_day(imonth) = 0.0
           imin_air_change_hour(imonth) = 0.0
           fnumber_of_hours(imonth) =0.0
           fmonthly_air_infiltration(imonth) = 0.0
           iNumberOfPlantTimeSteps(imonth) = 0
           iNumberOfBuildingTimeSteps(imonth) = 0
c     FC

           fH3KSTORE_FC_q_net_elec(imonth) = 0.
           fH3KSTORE_FC_q_total(imonth)  = 0.
           iNumberTimeStepsFC_Active(imonth) = 0
           fH3KSTORE_FC_q_net_elec(imonth) = 0.
           fH3KSTORE_FC_q_total(imonth) = 0.
           fH3KSTORE_FC_Vdot_fuel_STP(imonth) = 0.
           fH3KSTORE_FC_eta_fc(imonth) = 0.
           fH3KSTORE_EL_q_element(imonth) = 0.
           fH3KSTORE_EL_T_Node1_future(imonth) = 0.
           fH3KSTORE_FF_Vdot_fuel_STP(imonth) = 0.
           fH3KSTORE_FF_T_node1_future(imonth) = 0.
           fH3KSTORE_FC_q_par_elec(iMonth) = 0.0
           fH3KSTORE_FC_q_nonHVAC_el(iMonth)= 0.0
           fH3KSTORE_FC_q_HVAC_elec(iMonth)= 0.0

c     HRV
           fHRV_Elec_Energy(imonth) = 0.
           iNumber_Of_Lines_In_FCT_File = 0

C     Explicit Plant Pump and Fan Electrical Energy
           fPlant_Fan_Elec(imonth)  = 0.
           fPlant_Pump_Elec(imonth) = 0.

 2       continue
 1    continue

c     This zeros the foundation data which is in an array of zones and
c     months.
      do 5 izone = 1,mcom
         do 4 imonths=1,months

c     BSIMP Variable (foundation losses)
            foundation_losses(izone,imonths) = 0.0

c     Solar and Casual Variables
            Solar_From_Extern_Monthly(izone,imonths) = 0.0
            Solar_From_Intern_Monthly(izone,imonths) = 0.0
            Solar_Abs_From_Trans_Monthly(izone,imonths) = 0.0
            Solar_Abs_From_Opaque_Monthly(izone,imonths) = 0.0
            Casual_Radiant_Monthly(izone,imonths) = 0.0
            Casual_Conv_Monthly(izone,imonths) = 0.0
 4       continue
 5    continue

      fPreviousTS_q_total_elec        = 0.0
      fPreviousTS_q_net_elec          = 0.0
      fPreviousTS_q_par_elec          = 0.0
      fPreviousTS_eta_fc              = 0.0
      fPreviousTS_FC_Vdot_fuel_STP   = 0.0
      fPreviousTS_Vdot_air_STP       = 0.0
      fPreviousTS_stoichs            = 0.0
      fPreviousTS_Vdot_liqwater      = 0.0
      fPreviousTS_Vdot_exh_STP       = 0.0
      fPreviousTS_T_exh              = 0.0
      fPreviousTS_T_exit             = 0.0
      fPreviousTS_Twater_enter       = 0.0
      fPreviousTS_UA_exh             = 0.0
      fPreviousTS_fHeatToWater       = 0.0
      fPreviousTimeStepNumber       = 0.0
      fPreviousTimestepHour         = 0.0
      iPreviousTimestepMonth        = 0
      iPreviousTimestepDay          = 0


      fPreviousTS_f_FF_Vdot_fuel_STP    = 0.0
      fPreviousTS_q_FF_capture_fut      = 0.0
      fPreviousTS_T_FF_node1_fut        = 0.0
      fPreviousTS_f_FF_T_1a_present     = 0.0
      fPreviousTS_f_FF_First_Conn      = 0.0
      fPreviousTS_f_FF_T_1b_present     = 0.0
      fPreviousTS_f_FF_Second_Conn      = 0.0
      fPreviousTS_T_FF_makeup_fut      = 0.0
      fPreviousTS_f_FF_DHW_draw_fut    = 0.0
      fPreviousTS_f_FF_JacketLosses    = 0.0
      fPreviousTS_f_FF_heatdump        = 0.0


      fPreviousTS_q_EL_element      = 0.0
      fPreviousTS_q_EL_capture_fut  = 0.0
      fPreviousTS_T_EL_Node1_fut    = 0.0
      fPreviousTS_T_EL_1a_present   = 0.0
      fPreviousTS_f_EL_First_Conn   = 0.0
      fPreviousTS_T_EL_1b_present   = 0.0
      fPreviousTS_f_EL_Second_Conn  = 0.0
      fPreviousTS_T_EL_makeup_fut   = 0.0
      fPreviousTS_f_EL_DHW_draw_fut = 0.0
      fPreviousTS_f_EL_JacketLosses = 0.0
      fPreviousTS_f_EL_heatdump     = 0.0

      fPreviousTS_Plant_Fan_Elec    = 0.0
      fPreviousTS_Plant_Pump_Elec   = 0.0

      iCurrentMonth = 0
      bQ1Started = .false.
      bQ2Started = .false.
      bQ3Started = .false.
      bQ4Started = .false.
      return
      end

c*****************************************************************
c     H3KSTORE_HVAC_ENERGY
c     Store Sum Energy Used by HVAC systems monthly.
c     Created by: Phylroy A. Lopez
c     Initial Creation Date:  March 23th 2001
c     Copyright 2000: NRCan Buildings Group

c     This routine gathers the energy used by the HVAC systems
c     by it's various components.
c     .
c     the HVAC system type.

c     INPUTS:
c     isystem           HVAC SYSTEM used
c     imonth            Month of Simulation
c     heating_load,   Heating Load required
c     sys_energy      System Energy used
c     backup_energy,  Backup Sys Energy Used
c     fan_energy,
c     pilot_energy

c     OUTPUTS:
c     None. Sums up Monthly versions of energy Variables above..

      SUBROUTINE H3KSTORE_HVAC_SUBSYS_ENERGY(isystem,
     &     fheat_energy,
     &     fcool_energy,
     &     fpilot_energy,
     &     PLR)
      IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      INTEGER ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      INTEGER ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      INTEGER IDAYN,iMonth
      INTEGER isystem        !System Number
      REAL    fheat_energy   !Sytstem Energy Joules
      REAL    fcool_energy   !System Cooling energy in Joules.
      REAL    fpilot_energy  !Pilot Energy Joules
      LOGICAL bInStartup     !Function to see if still in start up mode.
      REAL    PLR

c     Sum up the energy for the timestep.

c     This IF ensures start-up is over.
      IF (bInStartUp()) then
         RETURN
      ELSE
         CALL EDAYR(idyp,IDAYN,imonth)
         CALL H3KSTORE_SUM_PLR_BIN_HOURS(isystem,PLR)

c     Sum up monthly energies
         fheat_energy_monthly(isystem,imonth) =
     &        fheat_energy_monthly(isystem,imonth) + fheat_energy

         fcool_energy_monthly(isystem,imonth) =
     &        fcool_energy_monthly(isystem,imonth) + fcool_energy

         fpilot_energy_monthly(isystem,imonth) =
     &        fpilot_energy_monthly(isystem,imonth) +fpilot_energy

         return
      ENDIF
      END

c*****************************************************************
c     H3KSTORE_HVAC_ENERGY
c     Store Sum Energy Used by HVAC systems monthly.
c     Created by: Phylroy A. Lopez
c     Initial Creation Date:  March 23th 2001
c     Copyright 2000: NRCan Buildings Group

c     This routine gathers the energy used by the HVAC systems
c     by it's various components.
c     .
c     the HVAC system type.

c     INPUTS:
c     isystem           HVAC SYSTEM used
c     imonth            Month of Simulation
c     heating_load,   Heating Load required
c     sys_energy      System Energy used
c     backup_energy,  Backup Sys Energy Used
c     fan_energy,
c     pilot_energy

c     OUTPUTS:
c     None. Sums up Monthly versions of energy Variables above..

      SUBROUTINE H3KSTORE_HVAC_Packaged_DATA(
     &     fHeating_load,
     &     fCooling_load,
     &     fFan_energy,
     &     fGSHP_pump_energy,
     &     fGCEP_pump_energy)
C      USE Ventilation_Data

           IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"
#include "Ventilation_Data.h"
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      INTEGER ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      INTEGER ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      INTEGER IDAYN,iMonth
      REAL    fHeating_load  !Sytstem Energy Joules
      REAL    fCooling_load  !System Cooling energy in Joules.
      REAL    fFan_energy
      REAL    fGSHP_pump_energy
      REAL    fGCEP_pump_energy
      LOGICAL bInStartup     !Function to see if still in start up mode.

c     Sum up the energy for the timestep.

c     This IF ensures start-up is over.
      IF (bInStartUp()) then
         RETURN
      ELSE
         CALL EDAYR(idyp,IDAYN,imonth)

c     Sum up monthly energies

         fFan_energy_monthly(imonth) =
     &        ffan_energy_monthly(imonth) + fFan_energy

         fHeating_load_monthly(imonth) =
     &        fheating_load_monthly(imonth) + fHeating_load

         fCooling_load_monthly(imonth) =
     &        fcooling_load_monthly(imonth) + fCooling_load

         fGSHP_pump_energy_monthly(imonth) =
     &        fGSHP_pump_energy_monthly(imonth) + fGSHP_pump_energy

        fGCEP_pump_energy_monthly(imonth) =
     &      fGCEP_pump_energy_monthly(imonth) + fGCEP_pump_energy
         return
      ENDIF

      END

c*****************************************************************
c     H3KSTORE_AIM2_DATA
c     Store AIr change data for each month
c     Created by: Phylroy A. Lopez
c     Initial Creation Date:  March 23th 2001
c     Copyright 2000: NRCan Buildings Group

c     This routine gathers the air changes information for the building.


c     INPUTS:
c     AIM2_total        volume of air in m^3 for the timestep
c     OUTPUTS:
c     None. Updates MAX and MIN airchange volume and times
c     total number of hours and total monthly infiltration.
c


      SUBROUTINE H3KSTORE_AIM2_DATA(AIM2_total)

           IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "aim2_parameters.h"      
#include "aim2_common.h"
#include "h3kstore_common.h"
      character outs*124
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      INTEGER ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      INTEGER ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      INTEGER IDAYN,imonth
      REAL AIM2_total,fcurrent_air_change
      LOGICAL bInStartUp
c     This IF ensures that it is past start-up.
      IF (bInStartUp()) then
c     Still in startup do not execute!
         RETURN
      ELSE
c     This Gets the current month from the day of year variable (idyp)
         CALL EDAYR(idyp,IDAYN,imonth)
c     Determines the air change for the time step. Normalize to 1 hour
         fcurrent_air_change = (AIM2_total/AIM2_house_vol)*3600.
c     The two following if statements update
c     the min and max variables as neccesary
         if (fmax_air_change(imonth).lt.fcurrent_air_change) then
            fmax_air_change(imonth) = fcurrent_air_change
            imax_air_change_day(imonth) = IDAYN
            imax_air_change_hour(imonth) = ihrp
         endif
         if (fcurrent_air_change.lt.fmin_air_change(imonth)
     &        .or.fmin_air_change(imonth).EQ.0.00) then
            fmin_air_change(imonth) = fcurrent_air_change
            imin_air_change_day(imonth) = IDAYN
            imin_air_change_hour(imonth) = ihrp
         endif
c     Sums up the total hours and total volume of air per month. This
c     is used for the average airchange which is calculated at output.
         fnumber_of_hours(imonth) = fnumber_of_hours(imonth)
     &        +1.0/REAL(NTSTEP)
         fmonthly_air_infiltration(imonth) =
     &        fmonthly_air_infiltration(imonth) +  AIM2_total*3600.0
     &        /REAL(NTSTEP)
      endif
      return
      END


c     *********************************************************************
c     H3KOUT_STORE_FNDTN_LOSSES(ICOMP,idyp,QBS)
c     Store Sum Energy Used by HVAC systems monthly.
c     Created by: Phylroy A. Lopez
c     Initial Creation Date:  March 23th 2001
c     Copyright 2000: NRCan Buildings Group

c     This routine gathers the energy used by the HVAC systems
c     by it's various components.
c     .
c     the HVAC system type.

c     INPUTS:
c     icomp             Zone Number
c     idyp              current day of year
c     QBS               Heat Transfer (W) in basement model during timestep

c     OUTPUTS:
c     None. Sums up Monthly versions of energy Variables above..

      SUBROUTINE H3KSTORE_FNDTN_LOSSES(ICOMP,idyp,QBS)

        IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"
C Inputs
        INTEGER ICOMP
        INTEGER idyp
        REAL QBS


      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      INTEGER  ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      character outs*124

      INTEGER IODS
      INTEGER IODF
      INTEGER IDAYN
      INTEGER IMTHN
      INTEGER IMONTH
      INTEGER JMONTH
      REAL total_foundation_losses

c     Ensures that the start up is over.
      CALL EDAY(ISD1,ISM1,IODS)
      CALL EDAY(ISD2,ISM2,IODF)
      CALL EDAYR(idyp,IDAYN,IMTHN)
      if (idyp.ge.IODS.and.idyp.le.IODF) then
c     Sums up losses of basement for each month.
c     Converting QBS(Watts) to Joules by * by 1hour and / by number of timesteps
c     in an hour
         total_foundation_losses = total_foundation_losses+(QBS *
     &        3600.0 / NTSTEP )
         imonth = IMTHN
         DO 10 jmonth = 1,12
            if (jmonth.eq.imonth) then
               foundation_losses(ICOMP,imonth) =
     &              foundation_losses(ICOMP,imonth)+(QBS *
     &              3600.0 / NTSTEP )
            endif
 10      continue
      endif
      return
      end



c*****************************************************************
c     H3KSTORE_SUM_PLR_BIN_HOURS
c     Store Sum Energy Used by HVAC systems monthly.
c     Created by: Phylroy A. Lopez
c     Initial Creation Date:  March 23th 2001
c     Copyright 2000: NRCan Buildings Group

c     This routine gathers the energy used by the HVAC systems
c     by it's various components.
c     .
c     the HVAC system type.

c     INPUTS:
c     isystem           HVAC SYSTEM used
c     PLR               Part load ratio at Time Step

c     OUTPUTS:
c     None. Determines number of hours isystem operates at percentage range.
c     and stores it in sum_plr_capacity_bin(isystem,ipercent)

      SUBROUTINE H3KSTORE_SUM_PLR_BIN_HOURS(isystem,PLR)
      IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"
c     Common for the number of time steps per hour
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      INTEGER ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      INTEGER ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      INTEGER IODS,IODF
      REAL frac_hour,PLR,upper,lower,fhours,iPLR
      INTEGER isystem,iPercent
c     This IF ensures start-up is over.
      fhours = total_num_of_hours(isystem)
      CALL EDAY(ISD1,ISM1,IODS)
      CALL EDAY(ISD2,ISM2,IODF)
      if (idyp.ge.IODS.and.idyp.le.IODF) then

c     Determines real time slice
         frac_hour = 1.0/NTSTEP
c     Turns part load ratio into a percentage.
         iPLR =PLR*100.
c     Iterates through percentage range and sums appropriate percentage.If Furnance is on.
         if (PLR.ge.0.00000) then
            do 10 ipercent = 1,ipercent_capacity
               upper = ipercent*10
               lower = (ipercent-1.0)*10
               if(upper.lt.100.) then
                  if((iPLR.ge.lower).and.(iPLR.lt.upper)) then
                     sum_plr_capacity_bin(isystem,ipercent)
     &                     =sum_plr_capacity_bin(isystem,ipercent)
     &                     +frac_hour
                  endif
               else
                  if((iPLR.ge.lower).and.(iPLR.le.upper)) then
                     sum_plr_capacity_bin(isystem,ipercent)
     &                    =sum_plr_capacity_bin(isystem,ipercent)
     &                    +frac_hour
                  endif
               endif
 10      continue
         endif

c     Sums total hours to be used with average during output
         if (plr.ge.0.00000) then
            total_num_of_hours(isystem) =
     &           total_num_of_hours(isystem) + frac_hour
         endif
      endif
      return
      end


c*****************************************************************
c     H3KSTORE_Casual_solar
c     Stores Monthly values of Casual and Solar energy exchanges per zone).
c     Created by: Phylroy A. Lopez
c     Initial Creation Date:  March 23th 2001
c     Copyright 2000: NRCan Buildings Group


      SUBROUTINE H3KSTORE_casual_solar(izonenumber, ! Number of zone
     &     fQInternalSources,   !Internal Solar Sources
     &     fQInternalLosses,    !Internal Solar Losses
     &     fQTMCMLCAbs,         !TRn mlc Absorbed
     &     fQExternalSources,   !External sources solar
     &     fQExternalLosses,    !External Losses solar
     &     fQOpaqueMLCAbs,      !Opaque mlc Absorbed
     &     fQTotalSources,      !Total Sources
     &     fQTotalLosses,       !Total Losses
     &     fQTotalAbsorbed,     !Total Absorbed
     &     fQCASR,              !Casual Radiant
     &     fQCASC               !Casual Conv
     &     )
      Implicit None
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"

C     These commons are required for
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its

      REAL fQInternalSources, !Internal Sources
     &     fQInternalLosses,    !Internal Losses
     &     fQTMCMlcAbs,         !TRn mlc Absorbed
     &     fQExternalSources,   !External sources solar
     &     fQExternalLosses,    !External Losses solar
     &     fQOpaqueMLCAbs,      !Opaque mlc Absorbed
     &     fQTotalSources,      !Total Sourcesc  !Total Losses
     &     fQTotalAbsorbed,     !Total Absorbed
     &     fQTotalLosses,       !Total Losses
     &     fQCASR,              !Casual Radiant
     &     fQCASC               !Casual Conv


      INTEGER iZoneNumber
      INTEGER ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP,IMTHN,IDAYN
      INTEGER ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its, IODS,IODF



c     This IF ensures that it is past start-up.
      CALL EDAY(ISD1,ISM1,IODS)
      CALL EDAY(ISD2,ISM2,IODF)
      CALL EDAYR(idyp,IDAYN,IMTHN)
      if (idyp.ge.IODS.and.idyp.le.IODF) then
c     This Gets the current month from the day of year variable (idyp)
C     NOTE: these equations were changed to multiply the heat 
C     gains by 3600 seconds/hour, as opposed to 360 seconds/hour
         Solar_From_Extern_Monthly(iZoneNumber,IMTHN) =
     &        Solar_From_Extern_Monthly(iZoneNumber,IMTHN)
     &        + fQExternalSources*3600./NTSTEP

         Solar_From_Intern_Monthly(iZoneNumber,IMTHN) =
     &        Solar_From_Intern_Monthly(iZoneNumber,IMTHN)
     &        + fQInternalSources*3600./NTSTEP

         Solar_Abs_From_Trans_Monthly(iZoneNumber,IMTHN) =
     &        Solar_Abs_From_Trans_Monthly(iZoneNumber,IMTHN)
     &        + fQTMCMlcAbs*3600./NTSTEP

         Solar_Abs_From_Opaque_Monthly(iZoneNumber,IMTHN) =
     &        Solar_Abs_From_Opaque_Monthly(iZoneNumber,IMTHN)
     &        + fQOpaqueMLCAbs*3600./NTSTEP

         Casual_Conv_Monthly(iZoneNumber,IMTHN) =
     &        Casual_Conv_Monthly(iZoneNumber,IMTHN)
     &        + fQCASC*3600./NTSTEP


         Casual_Radiant_Monthly(iZoneNumber,IMTHN) =
     &        Casual_Radiant_Monthly(iZoneNumber,IMTHN)
     &        + fQCASR *3600./NTSTEP

      ENDIF
      RETURN
      END

c     ******************** H3KSTORE_SAVELEVEL5
c     H3KSTORE_SAVELEVEL5 is called NCOMP times per time-step and
C     prior to computation beginning for the next zone)
c     to transfer the mean results of the current zone
c     computation to the results library. The transferred data
c     is dependent on the save option chosen from the simulation
c     menu.

C     ITSMUS relates to the time-step control facility. It
C     is 1 when time-row corresponds to user specified
C     time-stepping scheme; 0 when it does not.
C     ndbph is number of data blocks/hr for monitor function.

      SUBROUTINE H3KSTORE_SAVELEVEL5(ICOMP,IDAY,ITSMUS,ndbph)
      IMPLICIT NONE
#include "building.h"

C     #include "cfd.h"
#include "monitor.h"


      COMMON/TC/ITC,ICNT
      INTEGER	ITC,ICNT

      COMMON/FILEP/IFIL
      INTEGER	IFIL



        COMMON/TRACE/ITCF,ITRACE,IZNTRC,ITU
        INTEGER ITCF
        INTEGER ITRACE(MTRACE)
        INTEGER IZNTRC(MCOM)
        INTEGER ITU

	COMMON/BTIME/BTIMEP,BTIMEF
	REAL	BTIMEP,BTIMEF

      COMMON/LIBREC/IRECPL
       INTEGER IRECPL

      COMMON/SAVE/ISAVE
      INTEGER ISAVE

      COMMON/PREC1/SNA,ZOA
      REAL SNA(MCOM,MS),ZOA(MCOM)


      common/prec7/itcnst
      INTEGER itcnst

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      INTEGER ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP


      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      INTEGER IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/PVALC/TPC,QPC
      REAL TPC(MCOM,MS,MN),QPC(MCOM)

      COMMON/PVALS/TPS,QPS
      REAL TPS(MCOM,MS),QPS(MCOM)

      COMMON/PVALA/TPA,QPA
      REAL TPA(MCOM),QPA(MCOM)

      COMMON/FVALC/TFC,QFC
        REAL TFC(MCOM,MS,MN),QFC(MCOM)

      COMMON/FVALS/TFS,QFS
        REAL TFS(MCOM,MS),QFS(MCOM)

      COMMON/FVALA/TFA,QFA
        REAL TFA(MCOM),QFA(MCOM)

      COMMON/SETUQ/QPLTP,QPLTF,CONV
        REAL    QPLTP(MCOM),QPLTF(MCOM),CONV(MCOM)

      COMMON/COE32/QSOLI,QSOLE
      REAL QSOLI(MS,2),QSOLE(MS,2)

      COMMON/COE34/QCASRT,QCASCT
      REAL QCASRT(MCOM),QCASCT

      COMMON/COE39/CVIP,CVVP,CVIF,CVVF,QVNP,XX3,CVVPM,CVVFM
        REAL    CVIP,CVVP,CVIF,CVVF,QVNP,XX3,CVVPM,CVVFM


C     Commons for summary of solar (for results library)
      common/solsum/q1adjz,q1outs,q2adjz,q2lost,q2tmc,
     &     q2wall,q2rem

        REAL q1adjz(2),q1outs(2),q2adjz(2),q2lost(2)
        REAL q2tmc(2),q2wall(2),q2rem(2)


      COMMON/CCTL/ICASCF
      INTEGER ICASCF(MCOM)

      COMMON/BCTL/NCF,IBSN,IBAN,NBCDT,
     &     IBCDV,NBCDP,TBCPS,IBCTYP,IBCLAW,BMISCD
       INTEGER NCF
       INTEGER IBSN(MCF,4)
       INTEGER IBAN(MCF,3)
       INTEGER NBCDT(MCF)
       INTEGER IBCDV(MCF,MBCDT,2)
       INTEGER NBCDP(MCF,MBCDT)
       REAL TBCPS(MCF,MBCDT,MBCDP)
       INTEGER IBCTYP(MCF,MBCDT,MBCDP)
       INTEGER IBCLAW(MCF,MBCDT,MBCDP)
       REAL BMISCD(MCF,MBCDT,MBCDP,MISC)



      COMMON/PREC2/VOL
      REAL VOL(MCOM)

      COMMON/precz/zname,zdesc
        CHARACTER*12  zname(MCOM)
        CHARACTER*64 zdesc(MCOM)

      COMMON/PREC9/NCONST,NELTS,NGAPS,NPGAP
        INTEGER NCONST(MCOM),NELTS(MCOM,MS)
        INTEGER NGAPS(MCOM,MS),NPGAP(MCOM,MS,MGP)



      COMMON/PRECTC/ITMCFL,TMCT,TMCA,TMCREF,TVTR
        INTEGER ITMCFL(MCOM,MS)
        REAL TMCT(MCOM,MTMC,5),TMCA(MCOM,MTMC,ME,5)
        REAL TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)


      COMMON/RHCTLDAT/ZLLDF,ZLLDP
      REAL ZLLDF(MCOM),ZLLDP(MCOM)

      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

      COMMON/SUS/ISSNY

      COMMON/SAVE0/TVMEM,ZTU,ZTL,ZPH,
     &     ZPC,TZPH,TZPC,TZPHM,TZPCM
        REAL TVMEM(MCOM,4,2)
        REAL ZTU(MCOM)
        REAL ZTL(MCOM)
        REAL ZPH(MCOM)
        REAL  ZPC(MCOM)
        REAL TZPH(MCOM)
        REAL TZPC(MCOM)
        REAL TZPHM(MCOM,12)
        REAL TZPCM(MCOM,12)

      COMMON/LS3/LS3CNT
      INTEGER LS3CNT

      COMMON/AVRAGE/IDAVER
        INTEGER IDAVER

      COMMON/ITERST/ITER
        INTEGER ITER

      COMMON/LS3A/TMIXP
	REAL TMIXP

C THE DEFINITION OF LIGHTS common block HAS CHANGED. 
      COMMON/LIGHTS/ICGCTL(MCOM)
      INTEGER ICGCTL
C      COMMON/LIGHTS/FRACP,FRACF,FRACS
C	REAL FRACP,FRACF,FRACS(MCOM)

      COMMON/ADJC/IE,ATP,ATF,ARP,ARF
        INTEGER IE(MCOM,MS)
        REAL    ATP(MCOM,MS),ATF(MCOM,MS)
        REAL    ARP(MCOM,MS),ARF(MCOM,MS)


C     Commons in support of surface energy balance trace feature.
      COMMON/COE31/HRP,ZHRP,HRF,ZHRF
        REAL    HRP(MS,MS)
        REAL    ZHRP(MS)
        REAL    HRF(MS,MS)
        REAL    ZHRF(MS)


      common/concoe/hcip,hcif,hcop,hcof
        REAL    HCIP(MCOM,MS),HCIF(MCOM,MS)
        REAL    HCOP(MCOM,MS),HCOF(MCOM,MS)


C     T2S4 added for use with save option 4 save & trace.
      COMMON/T2S4/CONI,CONE,DENI,DENE,SHTI,SHTE,THKI,THKE
        REAL    CONI(MCOM,MS),CONE(MCOM,MS)
        REAL    DENI(MCOM,MS),DENE(MCOM,MS)
        REAL    SHTI(MCOM,MS),SHTE(MCOM,MS)
        REAL    THKI(MCOM,MS),THKE(MCOM,MS)

      common/enbal/nsurf,insurf
        INTEGER nsurf,insurf(ms)


      common /bmontr/mnzn,mzone,montrb,yminb,ymaxb,
     &     ibplot,irecpb,mbstv
        INTEGER mnzn,mzone(mcom),montrb
        REAL  yminb(2),ymaxb(2)
        INTEGER ibplot,irecpb(MDPH),mbstv

C     BEMS interaction time for results dump.
      COMMON/BEMSTIM/bemstime,ibems,bresfil
        REAL bemstime
        INTEGER ibems
        CHARACTER bresfil*72


      COMMON/GR1D01/NNDS,NNDZ,NNDC,NNDL
      INTEGER NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)

      COMMON/ENETZONE/PEZON(MCOM,MGTY),QEZON(MCOM,MGTY)
      REAL PEZON,QEZON


      COMMON/ENETZONP/ZONPPP,ZONQPP
      REAL ZONPPP(MCOM),ZONQPP(MCOM)

      COMMON/ENETZONF/ZONPP,ZONQP
      REAL ZONPP(MCOM),ZONQP(MCOM)


C Common to store casual/radiative/latent gain injections
C on a zone-by-zone basis. Note: the latent gain
C common variable has been created is a place-holder
C for storing this data in the future, but is not
C presently used. All units are in watts (W)
C
C - Alex, Feb 24, 2004.

      COMMON/CasDat/Cas_Rad_dat,Cas_Con_dat,
     &     Cas_Lat_dat
        REAL Cas_Rad_dat(MCOM)
        REAL Cas_Con_dat(MCOM)
        REAL Cas_Lat_dat(MCOM)


C     CFD
      COMMON/cfdfil/LCFD,IFCFD
        CHARACTER*72 LCFD(MCOM)
        INTEGER IFCFD(MCOM)

      COMMON/CFDCNV/QCFDF,QCFDP
      REAL QCFDF(MCOM,MS)
      REAL QCFDP(MCOM,MS)

      COMMON/OUTIN/IUOUT,IUIN
        INTEGER IUOUT
        INTEGER IUIN

C     Once per hour storage of results, averaged over that hour.
      common/rdcgen/irdact
        INTEGER irdact

      common/rdcbld/irdcb,sTMA,sTMCON,sQM,
     &     sTMS,sQSI,sQSE,
     &     sQV1,sQV2,sQW1,sQW2,
     &     sZONPP,sZONQP,sQSA,sQCASR,
     &     sQCASC,sZRH,sFRAC,
     &     sTMC,sZLL,sSPM1,sSPM2,
     &     sqswrdi,sqlwrdi,sqconvi,
     &     sqcondi,sqstori,sqstore,
     &     sqconde,sqconve,sqlwrde,
     &     sqlwrbd,sqlwrsk,sqswrde,
     &     soutopq,souttrn,sopqin,strnin



        INTEGER IRDCB(MCOM)
        REAL    STMA(MCOM)
        REAL    STMCON(MCOM)
        REAL    SQM(MCOM)
        REAL    STMS(MCOM,MS)
        REAL    SQSI(MCOM,MS)
        REAL    SQSE(MCOM,MS)
        REAL    SQV1(MCOM)
        REAL    SQV2(MCOM)
        REAL    SQW1(MCOM)
        REAL    SQW2(MCOM)
        REAL    SZONPP(MCOM)
        REAL    SZONQP(MCOM)
        REAL    SQSA(MCOM)
        REAL    SQCASR(MCOM)
        REAL    SQCASC(MCOM)
        REAL    SZRH(MCOM)
        REAL    SFRAC(MCOM)
        REAL    STMC(MCOM,MS,MN)
        REAL    SZLL(MCOM)
        REAL    SQSWRDI(MCOM,MS)
        REAL    SQLWRDI(MCOM,MS)
        REAL    SQCONVI(MCOM,MS)
        REAL    SQCONDI(MCOM,MS)
        REAL    SQSTORI(MCOM,MS)
        REAL    SQSTORE(MCOM,MS)
        REAL    SQCONDE(MCOM,MS)
        REAL    SQCONVE(MCOM,MS)
        REAL    SQLWRDE(MCOM,MS)
        REAL    SQLWRBD(MCOM,MS)
        REAL    SQLWRSK(MCOM,MS)
        REAL    SQSWRDE(MCOM,MS)
        REAL    SOUTOPQ(MCOM)
        REAL    SOUTTRN(MCOM)
        REAL    SOPQIN(MCOM)
        REAL    STRNIN(MCOM)
        REAL    sSPM1(MCOM)
        REAL    sSPM2(MCOM)

      character outs*144
      CHARACTER LCFGF*72
      CHARACTER cfgroot*24
      CHARACTER ZSDES*28
      CHARACTER ZSDESC*20
      CHARACTER ZSDESS*16

      logical close



C Local Variables

        INTEGER ICOMP
        INTEGER IDAY
        INTEGER ITSMUS
        INTEGER NDBPH
        INTEGER ISSNY
        REAL  QFP
        REAL  QFF
        REAL TP
        REAL TF
        REAL QDP
        REAL QDF
        REAL VP
        REAL VF
        REAL DP
        REAL DF
        REAL HP
        REAL HF
        REAL TMS
        REAL QSI
        REAL QSE
        REAL TMC
        INTEGER IPASS
        INTEGER ICNTB
        REAL Q1TOT
        REAL Q2TOT
        REAL TMA
        REAL ZRH
        REAL CNDS
        INTEGER IUNIT
        INTEGER IREC
        INTEGER IRDNOW
        INTEGER IC
        INTEGER INODE
        REAL  TMCON
        INTEGER ISURF
        INTEGER IPOS
        REAL TMRT
        REAL TMIX
        INTEGER IZ
        REAL QM
        REAL QMA
        REAL ZLL
        INTEGER IS0D
        INTEGER IS0M
        INTEGER I
        INTEGER J
        INTEGER IM
        REAL TIME
        INTEGER NDAY


      DIMENSION TMS(MS),QSI(MS),QSE(MS),TMC(MN)
      data ipass/0/,icntb/1/



C--------------------------------------------------------------------
C     Note: this code, which is intended to to calculate the the 
C     radiant and convective casual gains based on the total casual gain 
C     reported by ESP-r and the zone floor area, has been commented out 
C     because it makes no sense.  Instead, the data contained in the
C     Cas_Rad_dat and Cas_Con_dat arrays are used, as these contain the
C     the actual casual gain heat injections used when characterizing the
C     zones. 
C
C     - Alex, Feb 24 2004. 
C
C     QCASR=QCASRT(icomp)*(ZOA(ICOMP))/2.0
C     QCASC=QCASCT/2.0
C--------------------------------------------------------------------
      Call TrackBuildingTimeSteps()
      call H3KSTORE_Progress()
      call H3KSTORE_Casual_Solar(icomp,
     &     q1adjz(2),           !Internal Sources
     &     q2adjz(2),           !Internal Losses
     &     q2tmc(2),            !TRn mlc Absorbed
     &     q1outs(2),           !External sources solar
     &     q2lost(2),           !External Losses solar
     &     (q2wall(2)+q2rem(2)), !Opaque mlc Absorbed
     &     q1tot,               !Total Sources
     &     (q2adjz(2)+q2lost(2)), !Total Losses
     &     q2tot,               !Total Absorbed
     &     Cas_Rad_dat(icomp),  !Casual Radiant (W)
     &     Cas_Con_dat(icomp)   !Casual Conv  (W)
     &     )

      IF(NSINC.EQ.1)LS3CNT=0


C     Compute mean zone air temperature across time-step.
      if (IDAVER.eq.1) then
         TMA=TFA(ICOMP)
      else
         TMA=(TPA(ICOMP)+TFA(ICOMP))*.5
      endif
      call H3KSTORE_Zone_Temp_Averages(ICOMP,TMA)

C     Call routine MZVAPC to establish the vapour content.
      CALL MZVAPC(ICOMP,ZRH,CNDS)


C     If the bems flag is active then write out the current zone temperature to the bems
C     output file (fort.60). Currently only write out the data for optimum start
C     stop. This will later be replaced by a dedicated call to res.
      if(ibems.gt.0) then
C     WRITE(iuout,'(A,I2)') 'Dumpimg BEMS data ..... Zone ',ICOMP
         WRITE(59,*) '#Zone  Day  Time   Temperature Plant'
         CALL ECLOSE(BTIMEF,1.000,0.001,CLOSE)
         IF(CLOSE)THEN
            WRITE(59,'(I6,1X,F8.4,1X,F8.4,1X,F8.4,2F8.1)')
     &           ICOMP,
     &           FLOAT(IDYF),
     &           BTIMEF,
     &           TFA(ICOMP),
     &           QFA(ICOMP),
     &           tfs(ICOMP,1)
         ELSE
            WRITE(59,'(I6,1X,F8.4,1X,F8.4,1X,F8.4,2F8.1)')
     &           ICOMP,
     &           FLOAT(IDYP),
     &           BTIMEF,
     &           TFA(ICOMP),
     &           QFA(ICOMP),
     &           tfs(ICOMP,1)
         ENDIF
      endif

C     Jump if time-step controller active and current time-row
C     does not coincide with user specified time-stepping scheme.
      IF (ITSMUS.EQ.0) return

c     Jump if still in start-up period.
      IF(ISSNY.EQ.0) goto 88
      IF(IDAY.EQ.1) goto 881
      IF(IDAY.EQ.365.AND.IHRP.EQ.24.AND.ITS.EQ.NTSTEP.AND.
     &     ISDS.EQ.1) goto  991
      return
 881  ISSNY=0
 88   IF(IDAY.EQ.ISDS-1.AND.IHRP.EQ.24) goto  991
      IF(IDAY.LT.ISDS) return
      IF(IDAY.EQ.ISDF.AND.IHRP.EQ.24) then
         ipass=0
         return
      endif
 991  IUNIT=IFIL+2
      IREC=IRECPL
      irecpb(icntb)=irecpl
      icntb=icntb+1
      if(icntb.gt.ndbph) icntb=1

C     Once per hour results storage. Determine whether storage at this
C     time step is needed; ie only during last time step within
C     current hour.
      if(irdact.gt.0) then
         if(its.eq.ntstep) then
            irdnow=1
         else
            irdnow=0
         endif
      endif

C     If zone temperature monitor flag is active then
C     go and set up x & y axes for subsequent plotting.
C     ipass ensures that this is done only once.
      if(montrb.eq.1.and.ipass.eq.0) then
         if(issny.eq.1) then
            call zngrph(1,isdf,0,1)
         else
            call zngrph(iday+1,isdf,0,1)
         endif
         call usrmsg(' ',' Monitoring now in progress...','-')
         ipass=1
         ibplot=1
      endif

C     Determine control function for current zone.
      IC=ICASCF(ICOMP)

c     Determine mean control node temperature (same as mean air
c     temperature if control point node - CPN - is located at air
c     point).

c     If no control imposed assume CPN is air point.
      IF(NCF.EQ.0.OR.IC.EQ.0)goto 2
      IF(IBSN(IC,1).EQ.0)INODE=1
      IF(IBSN(IC,2).EQ.0.AND.IBSN(IC,3).EQ.0.AND.
     &     IBSN(IC,1).GT.0)INODE=1
      IF(IBSN(IC,2).GT.0.AND.IBSN(IC,3).EQ.0)INODE=2
      IF(IBSN(IC,2).GT.0.AND.IBSN(IC,3).GT.0)INODE=3
      IF(IBSN(IC,1).EQ.-2)INODE=4
      IF(IBSN(IC,1).EQ.-3)INODE=5
      IF(IBSN(IC,1).EQ.99)INODE=6
      IF(IBSN(IC,1).EQ.-1)INODE=7
      goto (2,3,4,5,51,61,71),INODE

c     Control point is air node.
    2 TMCON=TMA
      goto 1

c     Control node is situated at a surface.
    3 ISURF=IBSN(IC,2)
      TMCON=(TPS(ICOMP,ISURF)+TFS(ICOMP,ISURF))*.5
      IF(IDAVER.EQ.1)TMCON=TFS(ICOMP,ISURF)
      goto 1

c     Control node is within a construction.
    4 IPOS=IBSN(IC,3)
      ISURF=IBSN(IC,2)
      TMCON=(TPC(ICOMP,ISURF,IPOS)+TFC(ICOMP,ISURF,IPOS))*.5
      IF(IDAVER.EQ.1)TMCON=TFC(ICOMP,ISURF,IPOS)
      goto 1

c     Control node is at the zone weighted temperature point.
    5 ITER=ITER-1
      CALL MZMIXT(ICOMP,TMRT,TMIX)
      TMCON=(TMIXP+TMIX)*.5
      IF(IDAVER.EQ.1)TMCON=TMIX
      TMIXP=TMIX
      goto 1

C     Control point is external air temperature.
 51   TMCON=(TP+TF)*0.5
      goto 1

c     Control point is the air point in another zone.
 61   IZ=-IBSN(IC,1)
      TMCON=(TFA(IZ)+TPA(IZ))*0.5
      goto 1

c     Control point is a plant component node (nb to be implemented).
 71   TMCON=TMA

c     Compute mean energy injection/extraction at plant
c     interaction node (PIN). Also remember convective plant
C     injection (qma) for trace).
    1 QM=0.
      qma=0.
      IF(NCF.EQ.0.OR.IC.EQ.0)goto 6
      INODE=1
      IF(IBAN(IC,3).GT.0)INODE=3
      IF(IBAN(IC,2).GT.0.AND.IBAN(IC,3).EQ.0)INODE=2
      IF(IBAN(IC,1).EQ.-1)INODE=4
      IF(IBAN(IC,1).EQ.-2)INODE=5
      goto (7,8,9,91,92),INODE

c     PIN is air point.
    7 QM=(QPA(ICOMP)+QFA(ICOMP))*.5
      IF(IDAVER.EQ.1)QM=QFA(ICOMP)
      qma=QM
      goto 6

c     PIN is at surface.
    8 QM=(QPS(ICOMP)+QFS(ICOMP))*.5
      IF(IDAVER.EQ.1)QM=QFS(ICOMP)
      qma=0.
      goto 6

c     PIN is within construction.
    9 QM=(QPC(ICOMP)+QFC(ICOMP))*.5
      IF(IDAVER.EQ.1)QM=QFC(ICOMP)
      qma=0.
      goto 6

c     PIN is at a plant component node (NB not yet implemented).
 91   goto 7

c     PIN is a mix of radiant/convective flux (eg radiator).
 92   QM=(QPLTP(ICOMP)+QPLTF(ICOMP))*.5
      IF(IDAVER.EQ.1)QM=QPLTF(ICOMP)
      qma=QM*CONV(ICOMP)

 6    CONTINUE

C     Compute the zone-side latent load
      if (IDAVER.eq.1) then
         ZLL=ZLLDF(ICOMP)
      else
         ZLL=(ZLLDP(ICOMP)+ZLLDF(ICOMP))*.5
      endif

C     Store the currently computed value.
      ZLLDP(ICOMP)=ZLLDF(ICOMP)

C     Write out solar zone info (to see if similar to that of solar.F)
C     write(6,*) 'solsum blibsv',BTIMEF,ICOMP,q1adjz(2),q1outs(2),
C     &   q2adjz(2),q2lost(2),q2tmc(2),q2wall(2),q2rem(2)

C     Temporary statement for dumping structured mesh temperatures.
C     call TEMPSD(icomp)

C     Save appropriate data for ISAVE=0. Initialise during first pass
C     of each zone. Find current month (IS0M) for the simulation day.
      CALL EDAYR(IDAY,IS0D,IS0M)
      IF (ICOMP.EQ.1) LS3CNT=LS3CNT+1
      IF (LS3CNT.eq.1)then
         DO 50 I=1,4
            DO 60 J=1,2
               TVMEM(ICOMP,I,J)=0
 60         CONTINUE
 50      CONTINUE
         ZTU(ICOMP)=-1000.0
         ZTL(ICOMP)=1000.0
         ZPH(ICOMP)=0.0
         ZPC(ICOMP)=0.0
         TZPH(ICOMP)=0.0
         TZPC(ICOMP)=0.0
         do 15 im=1,12
            TZPHM(ICOMP,im)=0.0
            TZPCM(ICOMP,im)=0.0
 15      continue
      endif
 16   TIME=(BTIMEP+BTIMEF)*0.5
      IF (IDAVER.EQ.1) TIME=BTIMEF
      NDAY=IDYP
      IF(TIME.LE.BTIMEF) goto 551
      TIME=TIME-12.0
      NDAY=IDYF
 551  IF (TMA.LE.ZTU(ICOMP).AND.LS3CNT.NE.1) goto 17
      ZTU(ICOMP)=TMA
      TVMEM(ICOMP,1,1)=TIME
      TVMEM(ICOMP,1,2)=FLOAT(NDAY)
 17   IF (TMA.GE.ZTL(ICOMP).AND.LS3CNT.NE.1) goto 18
      ZTL(ICOMP)=TMA
      TVMEM(ICOMP,2,1)=TIME
      TVMEM(ICOMP,2,2)=FLOAT(NDAY)
 18   call eclose(QM,0.00,0.001,close)
      IF (close.AND.LS3CNT.NE.1) return
      IF (close.AND.LS3CNT.EQ.1) goto 21
      IF (QM.LT.0.0.AND.LS3CNT.NE.1) goto 19

C     Increment zone heating demand (total and monthly).
      TZPH(ICOMP)=TZPH(ICOMP)+QM
      TZPHM(ICOMP,IS0M)=TZPHM(ICOMP,IS0M)+QM
      IF(QM.LE.ZPH(ICOMP).AND.LS3CNT.NE.1) return
      ZPH(ICOMP)=QM
 21   TVMEM(ICOMP,3,1)=TIME
      TVMEM(ICOMP,3,2)=FLOAT(NDAY)
      IF(LS3CNT.EQ.1)goto 22
      return

C     Increment zone cooling demand (total and monthly).
 19   TZPC(ICOMP)=TZPC(ICOMP)+QM
      TZPCM(ICOMP,IS0M)=TZPCM(ICOMP,IS0M)+QM
      IF(QM.GE.ZPC(ICOMP).AND.LS3CNT.NE.1) return
      ZPC(ICOMP)=QM
 22   TVMEM(ICOMP,4,1)=TIME
      TVMEM(ICOMP,4,2)=FLOAT(NDAY)
      return
 1000 write(outs,'(A,I5,A)')' MZLS3: error at record',IREC,'.'
      call edisp(iuout,outs)
      return

      END

c*****************************************************************
c     H3KSTORE_H3KSTORE_FuelCell
c     Store FC load data and optionally outputs data in Timestep format to
C     a file through a  call to 'FC_Timestep_out'.
c     Created by: Phylroy A. Lopez
c     Initial Creation Date:  October 23th 2001
c     Copyright 2000: NRCan Buildings Group
c     ********************************************************************
      SUBROUTINE H3KSTORE_FuelCell(
     &     q_total_elec,        ! total elec demand on FC controller (W)
     &     q_nonHVAC_elec,      ! total non-hvac demand (W)
     &     q_HVAC_elec,         ! total hvac demand (w)
     &     q_net_elec,          ! net electrical output (W)
     &     q_par_elec,          ! parasitic electrical losses (W)
     &     eta_fc,              ! electrical efficiency(%)
     &     FC_Vdot_fuel_STP,    ! fuel consumption (slpm)
     &     Vdot_air_STP,        ! air flow (slpm)
     &     stoichs,             ! air flow (stoichs)
     &     Vdot_liqwater,       ! water supply (L/min)
     &     Vdot_exh_STP,        ! exhaust flow (slpm)
     &     T_exh,               ! temp of exhaust gases into HX (oC)
     &     T_exit,              ! temp of gases at outlet of HX (oC)
     &     Twater_enter,        ! Temperature of water entering (oC)
     &     UA_exh               ! UA of Exhaust to Entering water temp
     &     )

           IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"
c#include "SOFC.h"
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      common/ptime/ptimep,ptimef
      COMMON/PCTSTP/NTSTPP
c     Variable to see if plant matrix values have converged and the ptimestep has incremented
      COMMON/pltcon/bPltCon, LastTSTP
      REAL      LastTSTP
      INTEGER   ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      INTEGER   ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      INTEGER   IDAYN,IMTHN
      INTEGER   NTSTPP
      REAL      ptimep,ptimef
      REAL      q_total_elec   ! total elec demand on FC controller (W)
      REAL      q_nonHVAC_elec ! total non-hvac demand (w)
      REAL      q_HVAC_elec    ! total HVAC eled demand(w)
      REAL      q_net_elec     ! net electrical output (W)
      REAL      q_par_elec     ! parasitic electrical losses (W)
      REAL      eta_fc         ! electrical efficiency
      REAL      FC_Vdot_fuel_STP ! fuel consumption (slpm
      REAL      Vdot_air_STP   ! air flow (slpm)
      REAL      stoichs        ! air flow (stoichs)
      REAL      Vdot_liqwater  ! water supply (L/min)
      REAL      Vdot_exh_STP   ! exhaust flow (slpm)
      REAL      T_exh          ! temp of exhaust gases into HX (oC)
      REAL      T_exit         ! temp of gases at outlet of HX (oC)
      REAL      Twater_enter   ! Temperature of water entering
      REAL      UA_exh         ! UA of Exhaust to Entering water temp
      logical   bInStartUp     ! Check if it is in start up mode.
      LOGICAL   bPltCon        !Variable to see if plant matrix values have converged and the ptimestep has incremented
      REAL      fafter_midnight
      INTEGER   iNext_day
      LOGICAL   bPrevTSTOL

C     Have we just crossed over midnight?
      IF( PTIMEF < 1. )THEN
c     Don't use 0:00h but rather 24:00h
         fafter_midnight = 24.0
      ELSE
c     If past 1am then use 0.0
         fafter_midnight = 0.0
      END IF
      fCurrentTimestepHour = PTIMEF + fafter_midnight
c     Store timestep values for next timestep
c     Ian Suggestion to check for decimal precision.
      
      CALL ECLOSE(fCurrentTimestepHour,1.0,1.0e-5,bPrevTSTOL)
      IF( fCurrentTimestepHour .ge. 24. .or.
     &     fCurrentTimestepHour .eq. 1..and.bPrevTSTOL )THEN
c     Make sure using correct day if at midnight to 1am ptimef
         iNext_day = 1.0
      ELSE
c     If past 1am then use 0.0
         iNext_day = 0.0
      END IF
C      iCurrentTimestepDay   = idyp  + iNext_Day
      CALL EDAYR(idyp+iNext_Day,IDAYN,IMTHN)
c     If passes new years eve.Ensure it is Jan 1st Not Dec 32nd.
      IF (IDAYN .eq.32 .and. IMTHN .eq. 12) then
         IDAYN = 1
         IMTHN = 1
      ENDIF
c     Store the elec 
c     values for the next timestep.

      fCurrentTS_q_total_elec        = q_total_elec
      fCurrentTS_q_nonHVAC_elec      = q_nonHVAC_elec
      fCurrentTS_q_HVAC_elec         = q_HVAC_elec
      fCurrentTS_q_net_elec          = q_net_elec
      fCurrentTS_q_par_elec          = q_par_elec  
      fCurrentTS_eta_fc              = eta_fc
      fCurrentTS_FC_Vdot_fuel_STP    = FC_Vdot_fuel_STP 
      fCurrentTS_Vdot_air_STP        = Vdot_air_STP  
      fCurrentTS_stoichs             = stoichs     
      fCurrentTS_Vdot_liqwater       = Vdot_liqwater 
      fCurrentTS_Vdot_exh_STP        = Vdot_exh_STP
      fCurrentTS_T_exh               = T_exh      
      fCurrentTS_T_exit              = T_exit     
      fCurrentTS_Twater_enter        = Twater_enter 
      fCurrentTS_UA_exh              = UA_exh     
      fCurrentTimeStepNumber         = ptimef
      fCurrentTimestepHour           = PTIMEF + fafter_midnight
      iCurrentTimestepMonth          = IMTHN
      iCurrentTimestepDay            = IDAYN
      RETURN
      END

      
      
      
c*****************************************************************
c     H3KSTORE_H3KSTORE_FuelCell_Elec_Tank
c     Store FC Elec Water Tank data.
c     Created by: Phylroy A. Lopez
c     Initial Creation Date:  October 23th 2001
c     Copyright 2000: NRCan Buildings Group
c     ********************************************************************     
      SUBROUTINE H3KSTORE_FuelCell_Elec_Tank(
     &     q_EL_element,        ! element power
     &     q_EL_capture_future, ! element -> water
     &     T_EL_node1_future,   ! tank temperature
     &     T_EL_1a_present,
     &     f_EL_First_Connection, ! 1st connection
     &     T_EL_1b_present,
     &     f_EL_Second_Connection, ! 2nd connection
     &     T_EL_makeup_future,  ! make-up water to tank (oC)
     &     f_EL_DHW_draw_future, ! DHW draw (kg/s)
     &     UA_EL_Tank,          !Tank UA
     &     T_EL_Room_future,    !Predicted future temp of Room.
     &     f_EL_heatdump        ! rate of energy dumped (W)
     &     )
           IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"

      REAL    q_EL_element     ! element power
      REAL    q_EL_capture_future ! element -> water
      REAL    T_EL_node1_future ! tank temperature
      REAL    T_EL_1a_present
      REAL    f_EL_First_Connection ! 1st connection
      REAL    T_EL_1b_present
      REAL    f_EL_Second_Connection ! 2nd connection
      REAL    T_EL_makeup_future
      REAL    f_EL_DHW_draw_future ! DHW
      REAL    f_EL_heatdump
      REAL    UA_EL_Tank       !Tank UA
      REAL    T_EL_Room_future !Predicted future temp of Room.

c     Mirror the values in the H3KSTORE_FC_DATA module for next timestep

      fCurrentTS_q_EL_element       = q_EL_element
      fCurrentTS_q_EL_capture_fut   = q_EL_capture_future
      fCurrentTS_T_EL_Node1_fut     = T_EL_Node1_future
      fCurrentTS_T_EL_1a_present    = T_EL_1a_present
      fCurrentTS_f_EL_First_Conn    = f_EL_First_Connection
      fCurrentTS_T_EL_1b_present    = T_EL_1b_present
      fCurrentTS_f_EL_Second_Conn   = f_EL_Second_Connection
      fCurrentTS_T_EL_makeup_fut    = T_EL_makeup_future
      fCurrentTS_f_EL_DHW_draw_fut  = f_EL_DHW_draw_future
      fCurrentTS_UA_EL_Tank         = UA_EL_Tank
      fCurrentTS_T_EL_Room_fut      = T_EL_Room_future
      fCurrentTS_f_EL_heatdump      = f_EL_heatdump

      RETURN
      END
      

      
c*****************************************************************
c     H3KSTORE_H3KSTORE_FuelCell_FOSSIL_TANK
c     Store FC Natural Gas Tank data and optionally outputs data in Timestep format to 
C     a file through a  call to 'FC_Timestep_out'.
c     Created by: Phylroy A. Lopez
c     Initial Creation Date:  October 23th 2001
c     Copyright 2000: NRCan Buildings Group
c     ********************************************************************
      SUBROUTINE H3KSTORE_FuelCell_Fossil_Tank(
     &     f_FF_Vdot_fuel_STP,  ! fuel supply
     &     q_FF_capture_future, ! burner -> water
     &     T_FF_node1_future,   ! tank temperature
     &     f_FF_T_1a_present,
     &     f_FF_First_Connection, ! 1st connection
     &     f_FF_T_1b_present,
     &     f_FF_Second_Connection, ! 2nd connection
     &     T_FF_makeup_future,
     &     f_FF_DHW_draw_future, ! DHW
     &     UA_FF_Tank,          ! Tank UA
     &     T_FF_Room_future,    ! Room temperature predicted for future.
     &     f_FF_heatdump        ! rate of energy dumped from tank
     &     )
            IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"
      REAL    f_FF_Vdot_fuel_STP
      REAL    q_FF_capture_future                                   
      REAL    T_FF_node1_future      
      REAL    f_FF_T_1a_present                                       
      REAL    f_FF_First_Connection 
      REAL    f_FF_T_1b_present
      REAL    f_FF_Second_Connection 
      REAL    T_FF_makeup_future
      REAL    f_FF_DHW_draw_future                       
      REAL    UA_FF_Tank       ! Tank UA
      REAL    T_FF_Room_future ! Room temperature predicted for future.
      REAL    f_FF_heatdump


      fCurrentTS_f_FF_Vdot_fuel_STP     = f_FF_Vdot_fuel_STP
      fCurrentTS_q_FF_capture_fut       = q_FF_capture_future
      fCurrentTS_T_FF_node1_fut         = T_FF_node1_future
      fCurrentTS_f_FF_T_1a_present      = f_FF_T_1a_present
      fCurrentTS_f_FF_First_Conn        = f_FF_First_Connection
      fCurrentTS_f_FF_T_1b_present      = f_FF_T_1b_present
      fCurrentTS_f_FF_Second_Conn       = f_FF_Second_Connection
      fCurrentTS_T_FF_makeup_fut        = T_FF_makeup_future
      fCurrentTS_f_FF_DHW_draw_fut      = f_FF_DHW_draw_future
      fCurrentTS_UA_FF_Tank             = UA_FF_Tank
      fCurrentTS_T_FF_Room_fut       = T_FF_Room_future
      fCurrentTS_f_FF_heatdump          = f_FF_heatdump



      RETURN
      END
      
      
c*****************************************************************
c     H3KSTORE_H3KSTORE_FuelCell
c     Store FC load data and optionally outputs data in Timestep format to 
C     a file through a  call to 'FC_Timestep_out'.
c     Created by: Phylroy A. Lopez
c     Initial Creation Date:  October 23th 2001
c     Copyright 2000: NRCan Buildings Group
c     ********************************************************************      
      SUBROUTINE H3KSTORE_Zone_Temp_Averages(iZone,fZone_Temp)
           IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      INTEGER   ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      INTEGER   iZONE,IDAYN,iMonth
      REAL   fZone_temp
      LOGICAL bInStartup
      Character*72 outs
      IF( .not.bInStartUp() )then
         CALL EDAYR(idyp,IDAYN,iMonth)

         fSummed_Zone_Temps(iZone,iMonth) = 
     &        fSummed_Zone_Temps(iZone,iMonth) + fZone_Temp

      ENDIF
      RETURN
      END


c*****************************************************************
c H3KSTORE_PLANT_ELEC
c Stores the Sum of the Pump and Fan Electrical Energy 
C from the Explicit Plant 
c
c Created by: Julia Purdy
c Initial Creation Date:  February 25, 2002
c Copyright 2000: NRCan Buildings Group

c This routine gathers the electrical energy used by the 
c explicit plant pump and fan components

c INPUTS:
c  imonth              Month of Simulation
C  pump_IPCOMP         component number for first pump modelled, if applicable
C  pump_IPCOMP2        component number for second pump modelled, if applicable
C  fan_IPCOMP          component number for first fan modelled, if applicable 

c SIDE-EFFECTS:
c  fPlant_Fan_Elec(imonth)  monthly fan electrical energy, stored in h3kstore_module.
c  fPlant_Pump_Elec(imonth) monthly pump electrical energy, stored in h3kstore_module.
c  fPreviousTS_Plant_Pump_Fan    real power consumption of the fan, if applicable,
c                                for the previous timestep, in W.
C    FCctl_pump_fuelcell   Power draw of pump that circulates water from
C                          tank to fuel cell, W (in MODULE FC_ctl_h3khvacloads)
C    FCctl_pump_fancoil    Power draw of pump that circulates water from
C                          tank to fan-coil, W (in MODULE FC_ctl_h3khvacloads)
C    FCctl_fan_fancoil     Power draw of fan that blows air through fan-coil, W
C                          (in MODULE FC_ctl_h3khvacloads)

c LOCAL:
c  fPreviousTS_Plant_Pump_Elec1  real power consumption of the first pump, if applicable,
c                                for the previous timestep, in W.
c  fPreviousTS_Plant_Pump_Elec2  real power consumption of the second pump, if applicable,
c                                for the previous timestep, in W
 


      SUBROUTINE H3KSTORE_PLANT_ELEC
           IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"
#include "SOFC.h"

#include "plant.h"


      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      INTEGER ISD1
      INTEGER ISM1
      INTEGER ISD2
      INTEGER ISM2
      INTEGER ISDS
      INTEGER ISDF
      INTEGER NTSTEP

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      INTEGER ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
c Variable to see if plant matrix values have converged and the ptimestep has incremented
      COMMON/pltcon/bPltCon, LastTSTP
       REAL     LastTSTP
       LOGICAL  bPltCon

C Common block used for Plant Additional Outputs (PCAOUT)
C MPCOM       maximum number of plant components in configuration (set to 50 in plant.h) 
C MPCRES      maximum number of additional plant component outputs (set to 15 in plant.h)
C IPCOMP      index number of the component under consideration
C NPCDAT(?,4) plant component model type number for component ? (equals NPCDAT(?,3)/10)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

      COMMON/C12PS/NPCDAT,IPOFS1,IPOFS2
      INTEGER   NPCDAT(MPCOM,9)
      INTEGER   IPOFS1(MCOEFG)
      INTEGER   IPOFS2(MCOEFG,MPVAR)

      COMMON/C9/NPCOMP,NCI,CDATA
      INTEGER   NPCOMP
      INTEGER   NCI(MPCOM)
      REAL      CDATA(MPCOM,MMISCD)



      COMMON/PCTSTP/NTSTPP
      INTEGER NTSTPP


      INTEGER  IDAYN,iMonth, I

C These two real variables are only used in this subroutine to sum up the plant
C pump and fan electrical loads.  
      REAL    fTS_Plant_Pump_Elec
      REAL    fTs_Plant_Fan_Elec

c      INTEGER   I

      LOGICAL  bInStartup            !Function to see if still in start up mode.
C      LOGICAL  bPltCon               !Variable to see if plant matrix values have
                                       !converged and the ptimestep has incremented


C  Now get the current variable values..
      
C  Initialize the pump electrical power variable. 
        fTS_Plant_Pump_Elec = 0.0
C  Initialize the fan electrical power variable.
        fTS_Plant_Fan_Elec = 0.0



C In the explicit plant model for devices that can be linked to fluid flow models, loads are set to
C negative values. Therefore, for the purposes of presenting an electrical energy load,
C the absolute value of the explicit plant variables will be used.

C The real power draw [PCAOUT(pump_IPCOMP(?),1)] for the current time-step is taken as the
C value for the most recent plant matrix iteration (could be the previous time-step).
C `FCctl_pumps' is the total power draw of the pumps defined in the plant network. This variable is used 
C by the fuel cell controller to determine the electrical demand of the HVAC system.

        IF (Num_pumps .ne. 0) THEN  
C Sum the power consumption of all the pumps.
           DO I=1,Num_pumps
              fTS_Plant_Pump_Elec  = fTS_Plant_Pump_Elec + 
     &                               (-1 * PCAOUT(pump_IPCOMP(I),1))
           END DO

           FCctl_pumps = fTS_Plant_Pump_Elec
        END IF
        

C Set the pump electrical draw at the current timestep
        fCurrentTS_Plant_Pump_Elec = fTS_Plant_Pump_Elec

C If a fan has been defined in the plant load network, the timestep real fan power - 
C PCAOUT(fan_IPCOMP(?),1) will be taken as the value for the previous plant iteration (see above).
C `FCctl_fans' is the total power draw of the fans defined in the plant network.
C This variable is used by the fuel cell controller to determine the electrical demand of
c the HVAC system.
        IF (Num_fans .ne. 0) THEN
           DO I=1,Num_fans
              fTS_Plant_Fan_Elec   = fTS_Plant_Fan_Elec +
     &                               (-1 * PCAOUT(fan_IPCOMP(I),1))
           END DO

           FCctl_fans = fTS_Plant_Fan_Elec

        END IF

C Set the fan electrical draw at the current timestep
        fCurrentTS_Plant_Fan_Elec = fTS_Plant_Fan_Elec


C The plant matrix converged the previous solver iteration. Therefore, save the results.
c If the last time step is not the same as the next time step and not in start up
        IF( bPltCon .and. .not.bInStartUp() )then
           CALL EDAYR(idyp,IDAYN,imonth)

c If fans have been specified in the explicit plant model then Num_fans is not zero, then
c sum up the monthly electrical energy and store in array (defined in 
c h3kstore_module.F). 
              IF (Num_fans .ne. 0) THEN
                fPlant_Fan_Elec(imonth) = fPlant_Fan_Elec(imonth) +
     &                                   (fCurrentTS_Plant_Fan_Elec
     &                                  / REAL(NTSTPP)/ REAL(NTSTEP))
               END IF
    

c If a pump has been specified in the explicit plant model, 
c sum up the monthly electrical energy and store in array (defined in 
c h3kstore_module.F):      
              IF (Num_pumps .ne. 0) THEN
                fPlant_Pump_Elec(imonth) =  fPlant_Pump_Elec(imonth) +
     &                                     (fCurrentTS_Plant_Pump_Elec           
     &                                    / REAL(NTSTPP)/ REAL(NTSTEP))
             END IF


        ENDIF

      RETURN

      END





c*****************************************************************
c     H3KSTORE_H3KSTORE_Plant_TimeSteps
c     Store FC load data and optionally outputs data in Timestep format to 
C     a file through a  call to 'FC_Timestep_out'.
c     Created by: Phylroy A. Lopez
c     Initial Creation Date:  October 23th 2001
c     Copyright 2000: NRCan Buildings Group
C NOTE:
C     The plant solver contains information that is ouputted via the FCTS 
C     files. 
C     There are some values that are solve in the current timestep when the 
c     matrix converges and some variables before convergence. This routine
C     manages the data required for the current and previous timesteps.

c     ********************************************************************
      SUBROUTINE H3KSTORE_Plant_TimeSteps() 

           IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"
#include "SOFC.h"

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP 
      common/ptime/ptimep,ptimef
      COMMON/PCTSTP/NTSTPP
c     Variable to see if plant matrix values have converged and the ptimestep has incremented
      COMMON/pltcon/bPltCon, LastTSTP
      REAL      LastTSTP

      INTEGER   ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its  
      INTEGER   ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      INTEGER   IDAYN,IMTHN
      INTEGER   NTSTPP
      REAL      ptimep,ptimef
      logical   bInStartUp     ! Check if it is in start up mode.
      LOGICAL   bPltCon        !Variable to see if plant matrix values have converged and the ptimestep has incremented
      REAL      fafter_midnight
      INTEGER   iNext_day
      LOGICAL   bPrevTSTOL     

C     If not in start up

c     Get the real previous timestep data. The Martix solutions for certain coeeficients are taken from previous and current TS data values.
c     For example the heat exchange from the exhaust to the water (Previous)  is the difference of the 
c     current Temperature of the exhaust and the Water entry temprature multiplied by the previous UA of the exhaust. 

C     Fuel_cell itself

         fPreviousTS_fHeatToWater        =  
     &        fPreviousTS_UA_exh
     &        * ( fCurrentTS_T_exh - fCurrentTS_Twater_enter ) 

C     Fossil Fuel Water Tank

         fPreviousTS_f_FF_Jacketlosses = 
     &        fPreviousTS_UA_FF_Tank
     &        * ( fCurrentTS_T_FF_node1_fut
     &        - fPreviousTS_T_FF_Room_fut )

C     Electric Water Tank

         fPreviousTS_f_EL_Jacketlosses = 
     &        fPreviousTS_UA_EL_Tank
     &        * ( fCurrentTS_T_EL_Node1_fut 
     &        - fPreviousTS_T_EL_Room_fut )


C     Handle output if not in start-up
      IF( .not. bInStartUp() )then


c     Call routine to bin monthly values
         CALL H3KSTORE_Plnt_TS_Sum_Monthly
c     Call FC timestep output routine to output timestep data. If flagged to do so in input
c     file. 
         If (FC_timeStep_output == FC_timeStep_output_yes) then 
            CALL FC_Timestep_Out()
         ENDIF
      ENDIF

C Calculations for this timerow are complete. Copy current time row data 
C to previous time-row.
      Call H3Kstore_timerow_transport()

      END



c*****************************************************************
c     H3KSTORE_H3KSTORE_FuelCell
c     Store FC load data and optionally outputs data in Timestep format to 
C     a file through a  call to 'FC_Timestep_out'.
c     Created by: Phylroy A. Lopez
c     Initial Creation Date:  October 23th 2001
c     Copyright 2000: NRCan Buildings Group
c     ********************************************************************
      SUBROUTINE H3KSTORE_Plnt_TS_Sum_Monthly

           IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"
#include "SOFC.h"
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP 
      common/ptime/ptimep,ptimef
      COMMON/PCTSTP/NTSTPP

      INTEGER   ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its  
      INTEGER   ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      INTEGER   IDAYN,IMTHN
      INTEGER   NTSTPP
      REAL      ptimep,ptimef

      CALL EDAYR(idyp,IDAYN,IMTHN)
c     Add previous timestep values to the monthly bin numbers for...
c     Energy produed by FC
      fH3KSTORE_FC_q_net_elec(IMTHN) = fH3KSTORE_FC_q_net_elec(IMTHN) 
     &     +  fPreviousTS_q_net_elec 
     &     / REAL(NTSTPP)/REAL(NTSTEP)
   
c     Energy demand on FC
      fH3KSTORE_FC_q_total(IMTHN) = 
     &     fH3KSTORE_FC_q_total(IMTHN)
     &     + fPreviousTS_q_total_elec
     &     / REAL(NTSTPP)/ REAL(NTSTEP)

c     Energy Para on FC
      fH3KSTORE_FC_q_par_elec(IMTHN) = 
     &     fH3KSTORE_FC_q_par_elec(IMTHN)
     &     +fPreviousTS_q_par_elec 
     &     / REAL(NTSTPP)/ REAL(NTSTEP)
c     Volume of fuel used
      fH3KSTORE_FC_Vdot_fuel_STP(IMTHN) 
     &     = fH3KSTORE_FC_Vdot_fuel_STP(IMTHN) 
     &     + fPreviousTS_FC_Vdot_fuel_STP !(L/min) 
     &     *60./ REAL(NTSTPP)/ REAL(NTSTEP) !Convert to litres
c     FC efficiency running average. 
      fH3KSTORE_FC_eta_fc(IMTHN) = 
     &     fH3KSTORE_FC_eta_fc(IMTHN) 
     &     + fPreviousTS_eta_fc 
      IF (fPreviousTS_eta_fc.gt.0.0) THEN
         iNumberTimeStepsFC_Active(IMTHN)=
     &        iNumberTimeStepsFC_Active(IMTHN)+1
      ENDIF
      
c     Electric Water Heater
c     Electric element energy (KW-h)
      fH3KSTORE_EL_q_element(IMTHN) = 
     &     fH3KSTORE_EL_q_element(IMTHN) 
     &     + fPreviousTS_q_EL_element
     &     / REAL(NTSTPP)/REAL(NTSTEP)
c     Electric Tank temperature (oC) (note: this uses the current TS value)
      fH3KSTORE_EL_T_Node1_future(IMTHN) = 
     &     fH3KSTORE_EL_T_Node1_future(IMTHN)
     &     + fCurrentTS_T_EL_Node1_fut
c     Natural Gas Water Heater
c     Natural Gas Fuel Litres used.(L)
      fH3KSTORE_FF_Vdot_fuel_STP(IMTHN) = 
     &     fH3KSTORE_FF_Vdot_fuel_STP(IMTHN) 
     &     + fPreviousTS_f_FF_Vdot_fuel_STP
     &     *60.0/ REAL(NTSTPP)/REAL(NTSTEP)
c     Natural Gas Tank Temperature(oC)(Note this uses the current TS value) 
      fH3KSTORE_FF_T_node1_future(IMTHN) = 
     &     fH3KSTORE_FF_T_node1_future(IMTHN)
     &     + fCurrentTS_T_FF_node1_fut

c     NON HVAC Energy demand on FC
      fH3KSTORE_FC_q_nonHVAC_el(IMTHN) = 
     &     fH3KSTORE_FC_q_nonHVAC_el(IMTHN)
     &     + fPreviousTS_q_nonHVAC_elec
     &     / REAL(NTSTPP)/ REAL(NTSTEP)

c     HVAC Energy demand on FC
      fH3KSTORE_FC_q_HVAC_elec(IMTHN) = 
     &     fH3KSTORE_FC_q_HVAC_elec(IMTHN)
     &     + fPreviousTS_q_HVAC_elec
     &     / REAL(NTSTPP)/ REAL(NTSTEP)
      
      RETURN
      END
c*****************************************************************
c     H3KSTORE_timerow_transport
C
c     Created by: Alex Ferguson
c     Initial Creation Date:  July 13, 2004
c     Copyright 2004: NRCan Buildings Group
C
C     This file contains a simple routine that transports CETC HVAC 
C     data from future time-row variables to present time-row variables
c********************************************************************

      SUBROUTINE H3Kstore_timerow_transport()

      IMPLICIT NONE 

#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"
#include "SOFC.h"
#include "cogen_tanks.h"

C.....Time 
      fPreviousTimestepHour          = fCurrentTimestepHour
      ipreviousTimestepDay           = iCurrentTimestepDay
      iPreviousTimeStepMonth         = iCurrentTimeStepMonth
     
C.....Electric hot water tank
      fPreviousTS_q_EL_element       = fCurrentTS_q_EL_element
      fPreviousTS_q_EL_capture_fut   = fCurrentTS_q_EL_capture_fut
      fPreviousTS_T_EL_Node1_fut     = fCurrentTS_T_EL_Node1_fut
      fPreviousTS_T_EL_1a_present    = fCurrentTS_T_EL_1a_present
      fPreviousTS_f_EL_First_Conn    = fCurrentTS_f_EL_First_Conn
      fPreviousTS_T_EL_1b_present    = fCurrentTS_T_EL_1b_present
      fPreviousTS_f_EL_Second_Conn   = fCurrentTS_f_EL_Second_Conn
      fPreviousTS_T_EL_makeup_fut    = fCurrentTS_T_EL_makeup_fut
      fPreviousTS_f_EL_DHW_draw_fut  = fCurrentTS_f_EL_DHW_draw_fut
      fPreviousTS_UA_EL_Tank         = fCurrentTS_UA_EL_Tank
      fPreviousTS_T_EL_Room_fut   = fCurrentTS_T_EL_Room_fut
      fPreviousTS_f_EL_heatdump      = fCurrentTS_f_EL_heatdump

C.....Fossil Fuel hot water tank
      fPreviousTS_f_FF_Vdot_fuel_STP = fCurrentTS_f_FF_Vdot_fuel_STP     
      fPreviousTS_q_FF_capture_fut   = fCurrentTS_q_FF_capture_fut      
      fPreviousTS_T_FF_node1_fut     = fCurrentTS_T_FF_node1_fut        
      fPreviousTS_f_FF_T_1a_present  = fCurrentTS_f_FF_T_1a_present
      fPreviousTS_f_FF_First_Conn    = fCurrentTS_f_FF_First_Conn
      fPreviousTS_f_FF_T_1b_present  = fCurrentTS_f_FF_T_1b_present    
      fPreviousTS_f_FF_Second_Conn   = fCurrentTS_f_FF_Second_Conn      
      fPreviousTS_T_FF_makeup_fut    = fCurrentTS_T_FF_makeup_fut     
      fPreviousTS_f_FF_DHW_draw_fut  = fCurrentTS_f_FF_DHW_draw_fut    
      fPreviousTS_UA_FF_Tank         = fCurrentTS_UA_FF_Tank
      fPreviousTS_T_FF_Room_fut      = fCurrentTS_T_FF_Room_fut
      fPreviousTS_f_FF_heatdump      = fCurrentTS_f_FF_heatdump     

C.....Water tank connection data (in cogen_tanks.h)
      mdot_conn1a_present = mdot_conn1a_future 
      mdot_conn1b_present = mdot_conn1b_future 
      q_heatdump_present = q_heatdump_future

C.....Pump and fan data
      fPreviousTS_Plant_Fan_Elec  = fCurrentTS_Plant_Fan_Elec
      fPreviousTS_Plant_Pump_Elec = fCurrentTS_Plant_Pump_Elec

C.....Fuel cell data
      fPreviousTS_q_total_elec        = fCurrentTS_q_total_elec
      fPreviousTS_q_nonHVAC_elec      = fCurrentTS_q_nonHVAC_elec
      fPreviousTS_q_HVAC_elec         = fCurrentTS_q_HVAC_elec
      fPreviousTS_q_net_elec          = fCurrentTS_q_net_elec
      fPreviousTS_q_par_elec          = fCurrentTS_q_par_elec  
      fPreviousTS_eta_fc              = fCurrentTS_eta_fc
      fPreviousTS_FC_Vdot_fuel_STP    = fCurrentTS_FC_Vdot_fuel_STP 
      fPreviousTS_Vdot_air_STP        = fCurrentTS_Vdot_air_STP  
      fPreviousTS_stoichs             = fCurrentTS_stoichs     
      fPreviousTS_Vdot_liqwater       = fCurrentTS_Vdot_liqwater 
      fPreviousTS_Vdot_exh_STP        = fCurrentTS_Vdot_exh_STP
      fPreviousTS_T_exh               = fCurrentTS_T_exh      
      fPreviousTS_T_exit              = fCurrentTS_T_exit     
      fPreviousTS_Twater_enter        = fCurrentTS_Twater_enter 
      fPreviousTS_UA_exh              = fCurrentTS_UA_exh     
      fPreviousTimeStepNumber         = fCurrentTimeStepNumber
      fPreviousTimestepHour           = fCurrentTimestepHour
      iPreviousTimestepMonth          = iCurrentTimestepMonth 
      iPreviousTimestepDay            = iCurrentTimestepDay



      RETURN
      END
