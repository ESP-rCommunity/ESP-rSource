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

C *********************************************************************
C POWOC_CETC_BATTERY
C
C Created by: Patrice Pinel
C Initial Creation Date: March 2005
C Modified by: Maria Mottillo
C  
C - This code is an internal resistance model of a battery
C - Battery internal resistance, free voltage and capacity are determined
C   from input parameters and correlations
C   so the model can be used to model different technologies by modifying these parameters
C - The model includes a crude thermal mass model of battery temperature evolution
C   as well as a thermal management system composed of electric resistances 
C   that take their energy from the battery (added to the battery load) in discharge mode
C   and from the energy available in charge mode.
C   When there is no available energy, the battery temperature is floating
C
C
C INPUTS:
C - IPWC: Power-only component index number
C
C *********************************************************************
      SUBROUTINE POWOC_CETC_BATTERY(IPWC)
      IMPLICIT NONE
#include "BATTERY.h"
#include "power.h"
#include "building.h"

C External functions.     
      integer lnblnk
      REAL PowForCur,MaxCurrent,InternalHeat

C ESP-r COMMONs
       COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
       INTEGER IHRP            !-hour of present time-step
       INTEGER IHRF            !-hour of future time-step
       INTEGER IDYP            !-year day number of present day
       INTEGER IDYF            !-year day number of future day
       INTEGER IDWP            !-day of the week of present day
       INTEGER IDWF            !-day of the week of future day
       INTEGER NSINC           !-number of building-side time increments
                               !-since start of simulation
       INTEGER ITS             !-current building time-step within
                               !-current hour

       COMMON/PCTIME/TIMSEC
       REAL    TIMSEC          !-length of time increment (second)


C Common holding the power-only component description read from the electric
C network input file.
       COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     & POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     & NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)
       integer npowcom        !- number of power-only components
       integer powcomno       !- the power-only component reference number
       integer powcomid       !- power-only component database id number
       integer powcomphtyp    !- power-only component phase type
       integer powconen       !- nodes connected to the power-only component
       integer npowcdat       !- number of data items associated with the power-only component
       integer npowcdats      !- number of string data items associated with the power-only component
       real    powcdat        !- power-only component numerical data item

       COMMON/FVALA/TFA(MCOM),QFA(MCOM)
       real    tfa            !- air node temperature, future time-row
       real    qfa            !- energy injected at air node, future time-row


C Common block storing electrical domain time-row data for additional data items.
      common / Enet_Power_Time_Row_Data /
     &    iPow_Time_Row_Count,
     &    fPow_Time_Row_Present,
     &    fPow_Time_Row_Future
      integer iPow_Time_Row_Count ( MPOWCOM ) ! # of additional data items associated with power-only component.
      real fPow_Time_Row_Present ( MPOWCOM, MEnet_Time_Row_MAX ) ! Component present time-row data
      real fPow_Time_Row_Future ( MPOWCOM, MEnet_Time_Row_MAX ) ! Component future time-row data


C Common holding power generation and loads data for power-only components.
      COMMON/ENETPOWC/PPOWOC(MPOWCOM),QPOWOC(MPOWCOM),
     &VPOWC(MPOWCOM)
      real    ppowoc         !-real power generated/load for power-only component
      real    qpowoc         !-reactive power generated/load for power-only component
      complex vpowc          !-voltage of power only component


C Node and component names
      COMMON/ECNAMES/ENODNAM(MENOD),HYCOMNAM(MHYCOM),
     &HYCOMTYPSTR(MHYCOM),POWCOMNAM(MPOWCOM),POWCDATS(MPOWCOM,MPCDS),
     &CONECOMNAM(MCONECOM)
      character ENODNAM*12        !-string holding electrical node name
      character HYCOMNAM*12       !-string holding hybrid component name
      character HYCOMTYPSTR*12    !-string containing the hybrid component type
      character POWCOMNAM*12      !-string holding the power-only component name
      character POWCDATS*72       !-string data item for a power-only component 
      character CONECOMNAM*12     !-string containing connecting component name

C------------------------------------------------------------------------
C PARAMETERS FROM SIGNATURE
C------------------------------------------------------------------------

      INTEGER IPWC

C-------------------------------------------------------------------------
C Local variables
C-------------------------------------------------------------------------

      INTEGER batMode              ! 0 for charge, 1 for discharge
      INTEGER nTimeStep            !- number of time steps since the beginning of the simulation
      INTEGER iZoneIndex           !- index number of zone containing battery

      REAL batTIni, batDODIni,batLoad, pBat, pHeaters,batVolt,
     &     maxPBat, maxCBat, maxIntHeat

      REAL dtTStep                 !- Length of the time step (hour)
      REAL zoneT                   !- Zone ambient temperature (°C)
      REAL batSOC                  !- Battery SOC (%)

      CHARACTER*128  hold_name, root_name, H3K_rep_NAME       ! strings used for XML output reporting


C---------------------------------------------------------------------------

      nTimeStep = NSINC
      dtTStep = TIMSEC / 3600.           ! convert to hour
      iZoneIndex = INT(powcdat(IPWC,47))
      zoneT = tfa(iZoneIndex)


C************** Get mode (charge/discharge)
      IF (batDemandP .EQ. 0. ) THEN
         batMode = 2
         batLoad = batDemandP
      ELSEIF (batDemandP .GT. 0.0) THEN
         batMode = 1   ! Discharge
         batLoad = batDemandP
      ELSE
         batMode = 0   ! Charge
         batLoad = -batDemandP   ! set power back to a positive value
      ENDIF

C************** Initialisation

C See if the time step has been changed and, if so, do the proper initialisation
      IF (nTimeStep .GT. nPreviousTS) THEN
         !First time step -> initialise all parameters at the beginning of the simulation
         IF(nTimeStep .EQ. 1) THEN
            Call InitSimulation(IPWC,batTIni, batDODIni)

         ! Following time steps -> Initialise parameters to their final value at the previous time step
         ELSE 
            Call InitTimeStep(batTFin,batDODFin,batTIni,batDODIni)
         ENDIF
      ENDIF

C************ Calculations

C    Get maximum curent, power and internal heat generation
      maxCBat = MaxCurrent(IPWC,batTIni,batDODIni,batMode,dtTStep)
      maxPBat=PowForCur(IPWC,batTIni,batDODIni,batMode,dtTStep,maxCBat)
      maxIntHeat = InternalHeat(IPWC,batTIni,batDODIni,batMode,dtTStep,
     &                          maxCBat)

      IF(batMode .EQ. 1 .OR. batMode .EQ. 2 ) THEN  ! Discharge or idle

      ! First, try to see if putting the load on the battery will result in a thermal emergency
         batLoad = MIN(batDemandP, maxPBat)
         CALL DoDischarge(IPWC,batTIni,zoneT,batDODIni,dtTStep,
     &                    maxCBat,maxPBat,batLoad,pBat,pHeaters,
     &                    batVolt,batTFin,batDODFin)

      ELSE ! Charge
         CALL DoCharge(IPWC,batTIni,zoneT,batDODIni,dtTStep,
     &                 maxCBat,maxPBat,batLoad,pBat,pHeaters,
     &                 batVolt,batTFin,batDODFin)

      ENDIF

      nPreviousTS = nTimeStep

C Save variables that are required by control sensors.
      iPow_Time_Row_Count(IPWC) = 2                         ! # of additional data
      fPow_Time_Row_Future(IPWC,1) = (1. - batDODFin)*100.  ! battery SOC, %
      fPow_Time_Row_Future(IPWC,2) = batTFin                ! battery temperature


C power generated/consumed by battery
      if ( batMode. eq. 1 ) then         ! discharge
          ppowoc(ipwc) = pBat            ! generation is +ve
      else                               ! charge
          ppowoc(ipwc) = -1. * pBat      ! load is negative
      endif
      qpowoc(ipwc) = 0.         ! reactive power


C-------------------------------------------------------
C XML output
C-------------------------------------------------------


C.... Get component name
      hold_name = powcomnam(IPWC)
C.....Format string as 'electrical_net/power_only_components/NAME/misc_data'

      write(root_name, '(A,A,A,A)')
     &     'electrical_net/',
     &     'power_only_components/',
     &     hold_name(1:lnblnk(hold_name)),
     &     '/misc_data'


      write(H3K_rep_NAME,'(A,A)')
     &     root_name(1:lnblnk(root_name)),
     &     '/battery_load'


      call add_to_xml_reporting(
     &                       batLoad,
     &                       H3K_rep_name,
     &                       'units',
     &                       '(W)',
     &                       'Load on battery' )


      write(H3K_rep_NAME,'(A,A)')
     &     root_name(1:lnblnk(root_name)),
     &     '/battery_mode'


      call add_to_xml_reporting(
     &                       real(batMode),
     &                       H3K_rep_name,
     &                       'units',
     &                       '(-)',
     &                       'Battery mode of operation' )


      write(H3K_rep_NAME,'(A,A)')
     &     root_name(1:lnblnk(root_name)),
     &     '/battery_voltage'


      call add_to_xml_reporting(
     &                       batVolt,
     &                       H3K_rep_name,
     &                       'units',
     &                       '(V)',
     &                       'Battery operating voltage' )


      write(H3K_rep_NAME,'(A,A)')
     &     root_name(1:lnblnk(root_name)),
     &     '/battery_DOD'


      call add_to_xml_reporting(
     &                       batDODFin,
     &                       H3K_rep_name,
     &                       'units',
     &                       '(-)',
     &                       'Battery depth of discharge' )

 
C Calculate and report SOC in %
       batSOC = (1. - batDODFin ) * 100.

      write(H3K_rep_NAME,'(A,A)')
     &     root_name(1:lnblnk(root_name)),
     &     '/battery_SOC'


      call add_to_xml_reporting(
     &                       batSOC,
     &                       H3K_rep_name,
     &                       'units',
     &                       '(%)',
     &                       'Battery state of charge' )


      write(H3K_rep_NAME,'(A,A)')
     &     root_name(1:lnblnk(root_name)),
     &     '/battery_power'


      call add_to_xml_reporting(
     &                       pBat,
     &                       H3K_rep_name,
     &                       'units',
     &                       '(W)',
     &                       'Power consumed/generated by battery' )

      RETURN
      END

C**************************************************************************
C 1 InitSimulation
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine gets all battery parameters at the begining of the simulation
C
C - This routine is called from POWOC_CETC_Battery
C
C - This subroutine is called only once at the beginning of the simulation
C 
C INPUTS: Data from the heather file containing the initial state of the batery
C - IPWC: Index of the power-only component associated with the model
C 
C OUTPUTS:
C -BatTempInit: Battery temperature at the begining of the simulation (°C)
C -BatDODInit: Battery Depth Of Discharge at the beginning of  the simulation (%)
C
C *********************************************************************
      SUBROUTINE InitSimulation(IPWC, batTempInit, batDODInit)
      IMPLICIT NONE
#include "power.h"

C Common holding the power-only component description read from the electric
C network input file.
       COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     & POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     & NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)
       integer npowcom        !- number of power-only components
       integer powcomno       !- the power-only component reference number
       integer powcomid       !- power-only component database id number
       integer powcomphtyp    !- power-only component phase type
       integer powconen       !- nodes connected to the power-only component
       integer npowcdat       !- number of data items associated with the power-only component
       integer npowcdats      !- number of string data items associated with the power-only component
       real    powcdat        !- power-only component numerical data item

C PARAMETERS FROM SIGNATURE
       REAL batTempInit, batDODInit
       INTEGER IPWC

C ************** Initialisation
       batTempInit = POWCDAT(IPWC,9)
       batDODInit = POWCDAT(IPWC,10)

      RETURN
      END

C**************************************************************************
C 2 InitTimeStep
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine initialises some battery parameters 
C   at the begining of a time step
C   to their value at the end of the preceding time step
C
C - This routine is called from POWOC_CETC_Battery
C
C - This subroutine is called only once at the begining of every time step
C 
C INPUTS: 
c -batTFinal: Battery temperature at the end of a time step (°C)
C -DODFinal: Battery Depth Of Discharge at the end of a time step (%)
C 
C OUTPUTS:
C -batTInit: Battery temperature at the begining of a time step (°C)
C -DODInit: Battery Depth Of Discharge at the begining of a time step (%)
C
C *********************************************************************
      SUBROUTINE InitTimeStep(batTFinal,DODFinal,batTInit, DODInit)
      IMPLICIT NONE

C PARAMETERS FROM SIGNATURE
      REAL batTFinal,DODFinal,batTInit,DODInit

C Initialise values at the beginning of the time step to their value at the end of the previous one      
      batTInit =  batTFinal
      DODInit = DODFinal

      RETURN
      END

C**************************************************************************
C 3 FreeVoltage
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine returns the free voltage of the battery
C   as a function of its depth of discharge
C
C This routine is called from various points of the program
C 
C INPUTS:
C -batDOD: The battery's depth of discharge
C -IPWC: Power-only component index
C 
C OUTPUTS:
C -returns the battery's free voltage
C
C *********************************************************************
      REAL FUNCTION FreeVoltage(IPWC,batDOD)
      IMPLICIT NONE
#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C Common holding the power-only component description read from the electric
C network input file.
      COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     &POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     &NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)
      integer npowcom        !- number of power-only components
      integer powcomno       !- the power-only component reference number
      integer powcomid       !- power-only component database id number
      integer powcomphtyp    !- power-only component phase type
      integer powconen       !- nodes connected to the power-only component
      integer npowcdat       !- number of data items associated with the power-only component
      integer npowcdats      !- number of string data items associated with the power-only component
      real    powcdat        !- power-only component numerical data item

C PARAMETERS FROM SIGNATURE
      REAL batDOD
      INTEGER IPWC

C Coefficients for the freeVoltage equation
      REAL E0CoefA, E0CoefB, E0CoefC, E0CoefD, E0ExpB, E0ExpC, E0ExpD
      INTEGER nSerie ! number of battery cells connected in series

C Battery Parameters (technology and configuration)
      REAL batMaxVolt      ! Maximum manufacturer voltage of a battery cell (0% DOD) (V)
      REAL batMinVolt      ! Minimum manufacturer voltage of a battery cell (100% DOD) (V)

C************** Initialisation

C These will be changed for the proper references once the "ACEP_BATTERY.h" will be replaced with the
C proper node in the electrical network
      E0CoefA = POWCDAT(IPWC,15)
      E0CoefB = POWCDAT(IPWC,16)
      E0CoefC = POWCDAT(IPWC,17)
      E0CoefD = POWCDAT(IPWC,18)
      E0ExpB = POWCDAT(IPWC,19)
      E0ExpC = POWCDAT(IPWC,20)
      E0ExpD = POWCDAT(IPWC,21)
      nSerie = POWCDAT(IPWC,2)

C     Voltages are multiplied by the number of units connected in series
      batMaxVolt = POWCDAT(IPWC,5) * nSerie
      batMinVolt = POWCDAT(IPWC,6) * nSerie

C ************** Calculations

C     Free voltage of the battery is the free voltage of a cell time the number of cells in series
      FreeVoltage = (E0CoefA + E0CoefB*batDOD**E0ExpB +
     &               E0CoefC*batDOD**E0ExpC + 
     &               E0CoefD*batDOD**E0ExpD)*nSerie

C     Make sure value is within boundaries
      IF (FreeVoltage .GT. batMaxVolt) THEN

         WRITE(IUOUT,*) 'Warning Battery Model: Free voltage above ',
     &                  ' max in function FreeVoltage'
         FreeVoltage = batMaxVolt

      ELSEIF(FreeVoltage .LT. batMinVolt) THEN

         WRITE(IUOUT,*) 'Warning Battery Model: Free voltage below ',
     &                  'min in function FreeVoltage'
         FreeVoltage = batMinVolt

      ENDIF

      END

C**************************************************************************
C 4 BatteryCap
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine returns the capacity of the battery
C   as a function of its temperature
C   and the number of battery cells connected in parallel
C
C This routine is called from various points of the program
C 
C INPUTS:
C -batteryTemp: The battery's temperature (°C)
C -IPWC: Power-only component index number
C 
C OUTPUTS:
C -returns the battery's capacity (Ah)
C
C *********************************************************************
      REAL FUNCTION BatteryCap(IPWC,batTemp)
      IMPLICIT NONE
#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C Common holding the power-only component description read from the electric
C network input file.
      COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     &POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     &NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)
      integer npowcom        !- number of power-only components
      integer powcomno       !- the power-only component reference number
      integer powcomid       !- power-only component database id number
      integer powcomphtyp    !- power-only component phase type
      integer powconen       !- nodes connected to the power-only component
      integer npowcdat       !- number of data items associated with the power-only component
      integer npowcdats      !- number of string data items associated with the power-only component
      real    powcdat        !- power-only component numerical data item

C PARAMETERS FROM SIGNATURE
      REAL batTemp
      INTEGER IPWC

C Battery parameters
      REAL cellCapRef
      INTEGER nParal
C Coefficients for the capacity equation
      REAL capCoefA, capCoefB, capCoefC

C************** Initialisation

      capCoefA = POWCDAT(IPWC,22)
      capCoefB = POWCDAT(IPWC,23)
      capCoefC = POWCDAT(IPWC,24)
      cellCapRef = POWCDAT(IPWC,3)
      nParal = POWCDAT(IPWC,1)

C************** Calculations
C     Capacity of the battery is the capacity of one cell time the number of cells in parallel      
      BatteryCap = cellCapRef*nParal* 
     &             (capCoefA+capCoefB*batTemp+capCoefC*batTemp**2)

C     Make sure no negative value is returned
      IF (BatteryCap .LE. 0.0) THEN
        WRITE(IUOUT,*) 'Warning Battery Model: Negative or ',
     &                 'Nul capacity in function BatteryCap'
        BatteryCap = 0.01
      ENDIF

      END

C**************************************************************************
C 5 BatteryRInt
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine returns the internal resistance of the battery
C   as a function of its temperature, the current, the depth of discharge
C   and the number of battery cells connected in parallel
C - The polynom will eventually be changed for a form that better suits available data
C
C This routine is called from various points of the program
C 
C INPUTS:
C -batTemp: The battery's temperature (°C)
C -batDOD: The battery's depth of discharge
C -batI: The battery's current
C -BatMode: 0 for charge, 1 for discharge
C -IPWC: Power-only component index number
C 
C OUTPUTS:
C -returns the battery's internal resistance (Ohm)
C
C *********************************************************************
      REAL FUNCTION BatteryRInt(IPWC,batTemp,batDOD,batI,batMode)
      IMPLICIT NONE
#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C Common holding the power-only component description read from the electric
C network input file.
      COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     &POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     &NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)
      integer npowcom        !- number of power-only components
      integer powcomno       !- the power-only component reference number
      integer powcomid       !- power-only component database id number
      integer powcomphtyp    !- power-only component phase type
      integer powconen       !- nodes connected to the power-only component
      integer npowcdat       !- number of data items associated with the power-only component
      integer npowcdats      !- number of string data items associated with the power-only component
      real    powcdat        !- power-only component numerical data item

C PARAMETERS FROM SIGNATURE
      REAL batTemp,batDOD,batI
      INTEGER batMode,IPWC

C Battery parameters
      INTEGER nParal,nSerie ! number of battery cells connected in series and parallel
C Coefficients for the capacity equation
      REAL cellCapRef, rIntCA,rIntCB,rIntCC,rIntCD,rIntCH,
     &     rIntEB, rIntEC, rIntED, rIntEH1, rIntEH2, rIntEH3

C************** Initialisation

      IF(batMode .EQ. 0) THEN   ! charge mode -> use charge coefficients
         rIntCA = POWCDAT(IPWC,25)
         rIntCB = POWCDAT(IPWC,26)
         rIntCC = POWCDAT(IPWC,27)
         rIntCD = POWCDAT(IPWC,28)
         rIntCH = POWCDAT(IPWC,29)
         rIntEB = POWCDAT(IPWC,30)
         rIntEC = POWCDAT(IPWC,31)
         rIntED = POWCDAT(IPWC,32)
         rIntEH1 = POWCDAT(IPWC,33)
         rIntEH2 = POWCDAT(IPWC,34)
         rIntEH3 = POWCDAT(IPWC,35)
      ELSE   ! discharge mode -> use discharge coefficient
         rIntCA = POWCDAT(IPWC,36)
         rIntCB = POWCDAT(IPWC,37)
         rIntCC = POWCDAT(IPWC,38)
         rIntCD = POWCDAT(IPWC,39)
         rIntCH = POWCDAT(IPWC,40)
         rIntEB = POWCDAT(IPWC,41)
         rIntEC = POWCDAT(IPWC,42)
         rIntED = POWCDAT(IPWC,43)
         rIntEH1 = POWCDAT(IPWC,44)
         rIntEH2 = POWCDAT(IPWC,45)
         rIntEH3 = POWCDAT(IPWC,46)
      ENDIF
      nSerie = POWCDAT(IPWC,2)
      nParal = POWCDAT(IPWC,1)
      cellCapRef = POWCDAT(IPWC,3)

C************** Calculation
      !current for only one cell -> /nParal
      IF (nParal .GT. 0.0) THEN
         BatteryRInt = nSerie/nParal*(rIntCA + rIntCB*batDOD**rIntEB +
     &    rIntCC*(batI/nParal/cellCapRef)**rIntEC + 
     &    rIntCD*batTemp**rIntED+rIntCH*batDOD**rIntEH1+
     &    rIntCD*batTemp**rIntEH2*(batI/nParal/cellCapRef)**rIntEH3)
     
      ELSE
         WRITE(IUOUT,*) 'Error Battery Model: zero Parallel ',
     &                  'connections defined '
      ENDIF

C Make sure no negative value is returned
      IF (BatteryRInt .LT. 0.0) THEN
         WRITE(IUOUT,*) 'Warning Battery Model: Negative or Nul ',
     &                  ' resistance in routine BatteryRInt'
         BatteryRInt = 0.01
      ENDIF

      END

C**************************************************************************
C 6 MaxCurrent
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
c
C - This subroutine returns the maximal current the battery can charge/discharge with
C   without crossing the voltage limits
C - The temperature at the begining of the time step is used to evaluate properties ->
C   the problem is decoupled as far as temperatures are concerned
C
C This routine is called from various points of the program
C 
C INPUTS:
c -batTemp: The battery's temperature (°C)
c -DODInit: The battery's initial depth of discharge
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -IPWC: power-only component index number
! 
! OUTPUTS:
! -returns the maximum allowable current (Amp)
!
! *********************************************************************
      REAL FUNCTION MaxCurrent(IPWC,batTemp,DODInit,batMode,dtTStep)
      IMPLICIT NONE
#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C Common holding the power-only component description read from the electric
C network input file.
      COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     &POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     &NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)
      integer npowcom        !- number of power-only components
      integer powcomno       !- the power-only component reference number
      integer powcomid       !- power-only component database id number
      integer powcomphtyp    !- power-only component phase type
      integer powconen       !- nodes connected to the power-only component
      integer npowcdat       !- number of data items associated with the power-only component
      integer npowcdats      !- number of string data items associated with the power-only component
      real    powcdat        !- power-only component numerical data item

C PARAMETERS FROM SIGNATURE
      REAL batTemp,DODInit,dtTStep
      INTEGER batMode,IPWC

C Battery Parameters (technology and configuration)
      REAL batMaxVolt      ! Maximum manufacturer voltage of a battery cell (0% DOD) (V)
      REAL batMaxVoltCh    ! Maximum manufacturer charging voltage of a battery cell (V)
      REAL batMinVolt      ! Minimum manufacturer voltage of a battery cell (100% DOD) (V)
      REAL batMaxChCur     ! Maximum allowable current during charge mode (Amp)

C Declaration of functions used
      REAL BatteryRInt,FreeVoltage,FinalDOD

C Local variables
      REAL relaxFact    ! Relaxation factor for iteration loop (to reduce oscillations)
      REAL convVal      ! Convergence is assumed when difference between 2 iterations is smaller than this factor
      REAL batCurrent   ! Battery current (Amp)
      REAL dumCurrent   ! Dummy current used to see the current evolution from time step to time step (Amp)
      REAL lastGoodCur  ! Last curent that did not exceed the DOD limits (0-100%)
      REAL DODEnd       ! Depth of discharge at the end of the time step
      REAL FreeVEnd     ! Free Voltage at the end of the time step (Volt)
      REAL FreeVAvg     ! Free Voltage at the center of the time step (Volt)
      REAL RIntEnd      ! Internal resistance at the end of the time step (Ohm)
      REAL RIntAvg      ! Internal resistance at the center of the time step (Ohm)
      INTEGER numIter   ! Number of iterrations done
      INTEGER maxIter   ! Max number of iterrations before we consider there is a convergence problem

C************** Initialisation
C     Voltages are multiplied by the number of units connected in series
      batMaxVolt = POWCDAT(IPWC,5)*POWCDAT(IPWC,2)
      batMinVolt = POWCDAT(IPWC,6)*POWCDAT(IPWC,2)
      batMaxVoltCh = POWCDAT(IPWC,8)*POWCDAT(IPWC,2)
C     Currents are multiplied by the number of units connected in parallel
      batMaxChCur = POWCDAT(IPWC,7)*POWCDAT(IPWC,1)

      ! Assign value for relaxation factor
      relaxFact = 0.1
      ! Assign convergence criteria
      convVal = 0.001
      ! initialise the number of iterations
      numIter = 0
      maxIter = 100

C     Initialise curents to zero
      batCurrent = 0
      lastGoodCur = 0

C************** Calculation
      ! initialise the dummy current to the value of the current at the last iteration
610   dumCurrent = batCurrent

      DODEnd = FinalDOD(IPWC,batTemp,DODInit,batMode,dtTStep,batCurrent)
      FreeVEnd = FreeVoltage(IPWC,DODEnd)
      FreeVAvg = FreeVoltage(IPWC,(DODInit+DODEnd)/2.0)
      RIntEnd = BatteryRInt(IPWC,batTemp,DODEnd,batCurrent,batMode)
      RIntAvg = BatteryRInt(IPWC,batTemp,(DODInit+DODEnd)/2.0,batCurrent
     &          ,batMode)

      IF ((DODEnd .LT. 0.0).OR.(DODEnd .GT. 100.0) .OR. 
     &   (batCurrent>FreeVAvg/2.0/RIntAvg)) THEN
      ! DOD exceeds limit -> current too high   or
      ! I=[E0+-sqrt(E0²-4RP)]/2R => Pmax < E0²/4R => I max disch = E0/2R

        ! Bissectrice method
        ! Average of the actual current (that exceeds DOD limit) and last current that did not exceed
        batCurrent = (batCurrent+lastGoodCur)/2.0

      ELSE
      ! This current does not exceed limits
      
         ! Reset the good current to the actual current
         lastGoodCur = batCurrent

         IF(batMode .EQ. 0) THEN    ! charge
C           Adjust current so that (V-E0) = Rint*I results in V=Vmax_allowable 
C           at the end of the time step
            batCurrent = relaxFact*(batMaxVoltCh - FreeVEnd)/RIntEnd
     &                   + (1-relaxFact) * batCurrent
         
C           Make sure the current does not exceed the maximum allowable charging current
            batCurrent = MIN(batCurrent,batMaxChCur)

         ELSE  !Discharge
         
C           Adjust current so that (E0 - V) = Rint*I results in V=Vmin at the end of the time step
            batCurrent = relaxFact*(FreeVEnd - batMinVolt)/RIntEnd
     &                      + (1-relaxFact) * batCurrent
   
         ENDIF ! Mode

      ENDIF ! DOD

C     Increment the number of iterations
      numIter = numIter + 1

      IF((ABS(batCurrent - dumCurrent).GE.convVal).AND.
     &   (numIter.LT.maxIter)) THEN
      ! Not converged and iteration limit not exceeded -> repeat loop
         GOTO 610
      ENDIF

      IF(numIter.GT.maxIter)  THEN
      !Max number of iterations exceeded -> give warning
        WRITE(IUOUT,*) 'Warning Battery Model: Troublesome ',
     &                 'convergence in function MaxCurrent '
      ENDIF

C     Assign function result
      MaxCurrent = batCurrent

      END

C**************************************************************************
C 7 PowForCur
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine returns the power the battery charges/discharges with
C   corresponding to a given current
C - The temperature at the begining of the time step is used to evaluate properties ->
C   the problem is decoupled as far as temperatures are concerned
C
C This routine is called from various points of the program
C 
C INPUTS:
C -batTemp: The battery's temperature (°C)
C -DODInit: The battery's initial depth of discharge (%)
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -IPWC: Power-only component index number
C -batCur: The battery current
C 
C OUTPUTS:
C -returns the corresponding average power (W)
C
C *********************************************************************
      REAL FUNCTION PowForCur(IPWC,batTemp,DODInit,batMode,dtTStep,
     &                        batCur)
      IMPLICIT NONE
#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C Common holding the power-only component description read from the electric
C network input file.
      COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     &POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     &NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)
      integer npowcom        !- number of power-only components
      integer powcomno       !- the power-only component reference number
      integer powcomid       !- power-only component database id number
      integer powcomphtyp    !- power-only component phase type
      integer powconen       !- nodes connected to the power-only component
      integer npowcdat       !- number of data items associated with the power-only component
      integer npowcdats      !- number of string data items associated with the power-only component
      real    powcdat        !- power-only component numerical data item

C PARAMETERS FROM SIGNATURE
      REAL batTemp, DODInit, dtTStep, batCur
      INTEGER batMode,IPWC

C Declaration of functions used
      REAL FreeVoltage,FinalDOD,BatteryRInt

C Battery Parameters (technology and configuration)
      REAL batMaxVoltCh    ! Maximum manufacturer charging voltage of a battery cell (V)
      REAL batMinVolt      ! Minimum manufacturer voltage of a battery cell (100% DOD) (V)

C Local variables
      REAL DODAvg       ! Average Depth of discharge during time step if max current is used (%)
      REAL E0Avg        ! Average free voltage during timestep at maxCur (V)
      REAL VAvg         ! Average voltage during timestep at maxCur (V)
      REAL RintAvg      ! Internal resistance at DODAvg during timestep at maxCur (Ohm)

C************** Initialisation
C     Voltages are multiplied by the number of units connected in series
      batMinVolt = POWCDAT(IPWC,6)*POWCDAT(IPWC,2)
      batMaxVoltCh = POWCDAT(IPWC,8)*POWCDAT(IPWC,2)

C************** Calculation
      DODAvg = FinalDOD(IPWC,batTemp,DODInit,batMode,dtTStep/2.0,batCur)

      E0Avg = FreeVoltage(IPWC,DODAvg)

      RintAvg = BatteryRInt(IPWC,batTemp,DODAvg,batCur,batMode)

C     V=E0-RI      
      VAvg = E0Avg-batCur*RintAvg

C     Make a check that the voltage in in the acceptable range
      IF((VAvg.LT.batMinVolt).OR.(VAvg.GT.batMaxVoltCh))  THEN
        !Abnormal Voltage -> give warning
           WRITE(IUOUT,*) 'Warning Battery Model: Abnormal voltage ',
     &                    ' in function PowForCur'
      ELSE
C          P=VI
           PowForCur = batCur*VAvg
        ENDIF

      END

C**************************************************************************
C 8 CurForPow
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine returns the current that must flow through the battery
C   in order to obtain a certain power
C
C This routine is called from various points of the program
C 
C INPUTS:
C -batTemp: The battery's temperature (°C)
C -DODInit: The battery's initial depth of discharge (%)
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -batPow: Load on the battery (W) (always positive)
C -IPWC: power-only component index number
C 
C OUTPUTS:
C -returns the current corresponding to a certain power generated by the battery (Amp)
C  (always positive)
C
C *********************************************************************
      REAL FUNCTION CurForPow(IPWC,batTemp,DODInit,batMode,dtTStep,
     &                        batPow)
      IMPLICIT NONE

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C PARAMETERS FROM SIGNATURE
      REAL batTemp, DODInit, dtTStep, batPow
      INTEGER batMode,IPWC

C Declaration of functions used
      REAL BatteryRInt, FreeVoltage, FinalDOD

C Local variables
      REAL DODAvg       ! Average DOD during time step (DODInit+DODEnd)/2
      REAL E0Avg        ! Free voltage corresponding to the average conditions
      REAL RIntAvg      ! Internal resistance corresponding to the average conditions
      REAL relaxFact    ! Relaxation factor for iteration loop (to reduce oscillations)
      REAL convVal      ! Convergence is assumed when difference between 2 iterations is smaller than this factor
      REAL batCurrent   ! Battery current (Amp)
      REAL dumCurrent   ! Dummy current used to see the current evolution from time step to time step (Amp)
      INTEGER numIter   ! Number of iterrations done
      INTEGER maxIter   ! Max number of iterrations before we consider there is a convergence problem

C************** Initialisation
      ! Assign value for relaxation factor
      relaxFact = 0.1
      ! Assign convergence criteria
      convVal = 0.001
      ! initialise the number of iterations
      numIter = 0
      maxIter = 100

      batCurrent = 0.0

C************** Calculation
C initialise the dummy current to the value of the current at the last iteration
810   dumCurrent = batCurrent

      DODAvg = FinalDOD(IPWC,batTemp,DODInit,batMode,dtTStep/2.0,
     &                  batCurrent)

      RIntAvg = BatteryRInt(IPWC,batTemp,DODAvg,batCurrent,batMode)

      E0Avg = FreeVoltage(IPWC,DODAvg)

      IF (batMode .EQ. 0) THEN   ! charge
         batCurrent=(-E0Avg+(E0Avg**2+4*batPow*RIntAvg)**0.5)/2./RIntAvg
      ELSE  ! Discharge
         batCurrent=(E0Avg-(E0Avg**2-4*batPow*RIntAvg)**0.5)/2./RIntAvg
      ENDIF

      numIter = numIter + 1

      IF((ABS(batCurrent - dumCurrent).GT.convVal).AND.
     &   (numIter.LT.maxIter)) THEN
         ! Not converged -> repeat loop
         GOTO 810
      ENDIF

      IF(numIter.GE.maxIter)  THEN
C Max number of iterations exceeded -> give warning
        WRITE(IUOUT,*) 'Warning Battery Model: Troublesome ',
     &                 'convergence in function CurForPow '
      ENDIF

C     Assign function result
      CurForPow = batCurrent

      END

C**************************************************************************
C 9 InternalHeat
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine returns the heat generated by a battery's internal resistance
C - The temperature at the begining of the time step is used to evaluate properties ->
C   the problem is decoupled as far as temperatures are concerned
C
C This routine is called from various points of the program
C 
C INPUTS:
C -batTemp: The battery's temperature (°C)
C -DODInit: The battery's initial depth of discharge
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -batCur:  Current flowing through the battery (Amp)
C -IPWC: Power-only component index number
C 
C OUTPUTS:
C -returns the heat generated by the current flowing through the 
C  battery's internal resistance (W)
C
C *********************************************************************
      REAL FUNCTION InternalHeat(IPWC,batTemp,DODInit,batMode,dtTStep,
     &                           batCur)
      IMPLICIT NONE


C PARAMETERS FROM SIGNATURE
      REAL batTemp,DODInit,batCur,dtTStep
      INTEGER batMode,IPWC

C Declaration of functions used
      REAL BatteryRInt,FinalDOD

C Local variables
      REAL DODAvg    ! Average DOD during time step (DODInit+DODEnd)/2
      REAL RIntAvg   ! Internal resistance corresponding to DODAvg (Ohm)

C************** Calculation
      DODAvg = FinalDOD(IPWC,batTemp,DODInit,batMode,dtTStep/2.0,batCur)

      RIntAvg = BatteryRInt(IPWC,batTemp,DODAvg,batCur,batMode)

      ! Heat is: RI²
      InternalHeat = RIntAvg*batCur**2
      END

C**************************************************************************
C 10 BatFinalTemp
C**************************************************************************
c
C Created by: Patrice Pinel
C Initial Creation Date: May, 2005
C
C - This subroutine returns the final temperature of a battery
C   depending ont the internal heat generation
C   and the heat generated by its thermal management system
!
C This routine is called from various points of the program
C 
C INPUTS:
C -batTempInit: The battery's initial temperature (°C)
C -dtTStep: Duration of the time step (hour)
C -pIntHeat: Heat generated internally (W)
C -pHeaters: Heat generated by the thermal management system(W)
C -zoneTemp: Zone ambient temperature (°C) 
C -IPWC: Power-only component index number
C 
C OUTPUTS:
C -returns the battery's final temperature (°C) 
C
C *********************************************************************
      REAL FUNCTION BatFinalTemp(IPWC,batTempInit,dtTStep,pIntHeat,
     &                           pHeaters,zoneTemp)
      IMPLICIT NONE
#include "power.h"

C Common holding the power-only component description read from the electric
C network input file.
      COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     &POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     &NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)
      integer npowcom        !- number of power-only components
      integer powcomno       !- the power-only component reference number
      integer powcomid       !- power-only component database id number
      integer powcomphtyp    !- power-only component phase type
      integer powconen       !- nodes connected to the power-only component
      integer npowcdat       !- number of data items associated with the power-only component
      integer npowcdats      !- number of string data items associated with the power-only component
      real    powcdat        !- power-only component numerical data item

C PARAMETERS FROM SIGNATURE
      REAL batTempInit,dtTStep,pIntHeat,pHeaters,zoneTemp
      INTEGER IPWC

C Battery Parameters (technology and configuration)
      REAL thermalMass  ! Thermal mass of the battery-mass*Cp (J/°C)
      REAL heatLossFact ! Heat loss factor h*A (W/°C)
      REAL expValue     ! Temperature exponential exp(-hA/mCp * dt)
      REAL heatGen      ! Total heat generated (internal res and heaters)

C************** Initialisation

      thermalMass = POWCDAT(IPWC,11)
      heatLossFact = POWCDAT(IPWC,12)

C************** Calculation

C Patrice's code
C      expValue = EXP(thermalMass/heatLossFact*dtTStep)

C my code
       expValue = EXP(-1.*heatLossFact/thermalMass*dtTStep)

       heatGen = pIntHeat + pHeaters
 
      BatFinalTemp = (zoneTemp + heatGen/heatLossFact)*(1.0-expValue)
     &               + batTempInit*expValue


      END

C**************************************************************************
C 11 BatFinalTempRes
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine returns the final temperature of a battery
C   with a certain load and operating resistive heaters
C
C This routine is called from various points of the program
C 
C INPUTS:
C -batTempInit: The battery's initial temperature (°C)
C -DODInit: The battery's initial depth of discharge
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -pLoad: The power to the load (W)
C -pHeaters: The power to the heaters (W)
C -zoneTemp: Zone ambient temperature (°C) 
C -IPWC: Power-only component index number
C 
C OUTPUTS:
C -returns the battery's final temperature (°C) 
C
C *********************************************************************
      REAL FUNCTION BatFinalTempRes(IPWC,batTempInit,DODInit,batMode,
     &                              dtTStep,pLoad,pHeaters,zoneTemp)
      IMPLICIT NONE


C PARAMETERS FROM SIGNATURE
      REAL batTempInit,DODInit,dtTStep,pLoad,pHeaters,zoneTemp
      INTEGER batMode,IPWC

C Declaration of functions used
      REAL InternalHeat,CurForPow,BatFinalTemp 

C Local variables
      REAL batCurrent   ! Current flowing through the battery
      REAL pIntHeat     ! Heat generated by the current flowing in the internal resistance of the battery
      REAL heatPow      ! Power coming from the battery

C************** Calculation
      IF (batMode .EQ. 0) THEN   !Charge
         ! Battery does not handle load on heaters
         heatPow = pLoad

      ELSE  !Discharge
         ! Battery does handle load on heaters
         heatPow = pLoad+pHeaters

      ENDIF

      batCurrent = CurForPow(IPWC,batTempInit,DODInit,batMode,dtTStep,
     &                         heatPow)

      pIntHeat = InternalHeat(IPWC,batTempInit,DODInit,batMode,dtTStep,
     &           batCurrent)

      BatFinalTempRes = BatFinalTemp(IPWC,batTempInit,dtTStep,pIntHeat,
     &                               pHeaters,zoneTemp)

      END

C**************************************************************************
C 12 AvgVolt
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine returns average voltage observed during a constant current time step
C 
C INPUTS:
C -batTemp: The battery's initial temperature (°C)
C -DODInit: The battery's initial depth of discharge
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -CDemand: The current demanded/offered to the battery (A)
C -IPWC: Power-only component index number
C 
C OUTPUTS:
C -returns the average voltage observed during the time step
C
C *********************************************************************
      REAL Function AvgVolt(IPWC,batTemp,DODInit,batMode,dtTStep,batCur)
      IMPLICIT NONE

C PARAMETERS FROM SIGNATURE
      REAL batTemp,DODInit,dtTStep,batCur
      INTEGER batMode,IPWC

C Declaration of functions used
      REAL FinalDOD,FreeVoltage,BatteryRInt

C Local variables
      REAL DODAvg       ! Average DOD during time step (DODInit+DODEnd)/2
      REAL E0Avg        ! Free voltage corresponding to the average conditions
      REAL RIntAvg      ! Internal resistance corresponding to the average conditions

C************** Calculation
      DODAvg=FinalDOD(IPWC,batTemp,DODInit,batMode,dtTStep/2.0,batCur)

      E0Avg = FreeVoltage(IPWC,DODAvg)

      RIntAvg = BatteryRInt(IPWC,batTemp,DODAvg,batCur,batMode)

      IF (batMode .EQ. 0) THEN   !Charge
      ! Observed voltage is higher than free voltage
         AvgVolt = E0Avg + batCur*RIntAvg
      ELSE  !discharge
      ! Observed voltage is lower than free voltage
         AvgVolt = E0Avg - batCur*RIntAvg
      ENDIF

      END

C**************************************************************************
C 13 FinalDOD
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine returns average depth of discharge during a constant current discharge step
C
C INPUTS:
C -batTemp: The battery's initial temperature (°C)
C -DODIni: The battery's initial depth of discharge
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -batCur: The power demanded/offered to the battery (W)
C -IPWC: Identity of the plant component associated with the model
C 
C OUTPUTS:
C -returns the DOD at the end of the time step
C
C *********************************************************************
      REAL Function FinalDOD(IPWC,batTemp,DODIni,batMode,dtTStep,batCur)
      IMPLICIT NONE

C PARAMETERS FROM SIGNATURE
      REAL batTemp,DODIni,dtTStep,batCur
      INTEGER batMode,IPWC

C Declaration of functions used
      REAL BatteryCap

C************** Calculation
      IF(batMode .EQ. 0) THEN   !Charge
         FinalDOD = DODIni - batCur*dtTStep/BatteryCap(IPWC,batTemp)
      ELSE  !Discharge
         FinalDOD = DODIni + batCur*dtTStep/BatteryCap(IPWC,batTemp)
      ENDIF

      END

C****************************************************************************
C 14 GetNeededHeat
C****************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: May, 2005
C
C - This subroutine returns the heat needed
C   to increase the battery temperature to a desired value
C
C This routine is called from various points of the program
C 
C INPUTS:
C -batTIni: The battery's initial temperature (°C)
C -dtTStep: Duration of the time step (hour)
C -batTFin: The desired final temperature (°C)
C -zoneT: Zone ambient temperature (°C) 
C -IPWC: Power-only component index number
C 
C OUTPUTS:
C -returns the power needed (W)
C
C *********************************************************************
      REAL FUNCTION GetNeededHeat(IPWC,batTIni,dtTStep,batTFin,zoneT)
      IMPLICIT NONE
#include "power.h"

C Common holding the power-only component description read from the electric
C network input file.
      COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     &POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     &NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)
      integer npowcom        !- number of power-only components
      integer powcomno       !- the power-only component reference number
      integer powcomid       !- power-only component database id number
      integer powcomphtyp    !- power-only component phase type
      integer powconen       !- nodes connected to the power-only component
      integer npowcdat       !- number of data items associated with the power-only component
      integer npowcdats      !- number of string data items associated with the power-only component
      real    powcdat        !- power-only component numerical data item

C PARAMETERS FROM SIGNATURE
      REAL batTIni,dtTStep,batTFin,zoneT
      INTEGER IPWC

C Battery Parameters (technology and configuration)
      REAL thermalMass  ! Thermal mass of the battery-mass*Cp (J/°C)
      REAL heatLossFact ! Heat loss factor h*A (W/°C)
      REAL expValue     ! Temperature exponential exp(-hA/mCp * dt)
      REAL neededHeat   ! Heat needed (W)

C************** Initialisation

      thermalMass = POWCDAT(IPWC,11)
      heatLossFact = POWCDAT(IPWC,12)

C************** Calculation

C Patrice's original code
C      expValue = EXP(thermalMass/heatLossFact*dtTStep)
      
C      neededHeat = ((batTFin - batTIni*expValue)/(1.0-expValue)-zoneT)
C     &              *heatLossFact


C correction
      expValue = EXP(-1. * heatlossFact*dtTStep/thermalMass)
      neededHeat = ((batTIni - zoneT)*expValue +
     &              (zoneT - batTfin)) /
     &             (1. - expValue) * (-1.) * heatLossFact


C     Make sure no values lower than 0 are returned.
      IF(neededHeat .LE. 0.0) THEN
         GetNeededHeat = 0.0
      ELSE
         GetNeededHeat = neededHeat
      ENDIF

      END

C**************************************************************************
C 15 HeaterForFinalTemp
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: May, 2005
C
C - This subroutine returns the power required in the heaters 
C   to increase the battery temp to a certain value
C - It takes into account the heat generated in the battery's internal resistance
C
C This routine is called from various points of the program
C 
C INPUTS:
C -batTIni: The battery's initial temperature (°C)
C -batTFin: The battery's targetted final temperature (°C)
C -DODIni: The battery's initial depth of discharge
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -pLoad: The power flowing into the battery (W)
C -pAvail: The power available (W) (= max discharge rate in discharge, = what is available in charge)
C -zoneT: Zone ambient temperature (°C) 
C -IPWC: Power-only component index number
C 
C OUTPUTS:
C -returns the power required for the heaters (W)
C
C *********************************************************************
      REAL FUNCTION HeaterForFinalTemp(IPWC,batTIni,batTFin,DODIni,
     &                            batMode,dtTStep,pAvail,pLoad,zoneT)
      IMPLICIT NONE

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C PARAMETERS FROM SIGNATURE
      REAL batTIni,batTFin,DODIni,dtTStep,pAvail,pLoad,zoneT
      INTEGER batMode,IPWC

C Declaration of functions used
      REAL InternalHeat,GetNeededHeat,CurForPow

C Local variables
      REAL batCur       ! Current flowing through the battery
      REAL reqHeat      ! Total required heat to get the battery to the final temperature
      REAL pIntHeat     ! Heat generated by the current flowing in the internal resistance of the battery
      REAL pHeaters     ! Power to the heaters (W)
      REAL pHDum        ! Dummy value of Power to the heaters used to verify convergence
      REAL batPow       ! Power coming from the battery
      REAL relaxFact    ! Relaxation factor for iteration loop (to reduce oscillations)
      REAL convVal      ! Convergence is assumed when difference between 2 iterations is smaller than this factor
      INTEGER numIter   ! Number of iterrations done
      INTEGER maxIter   ! Max number of iterrations before we consider there is a convergence problem

C************** Initialisation
      ! Assign value for relaxation factor
      relaxFact = 0.1
      ! Assign convergence criteria
      convVal = 0.001
      ! Initialise the number of iterations
      numIter = 0
      maxIter = 100

C************** Calculation
C     Total (heater+internal resistance) heat needed to obtain the desired final temp
      reqHeat = GetNeededHeat(IPWC,batTIni,dtTStep,batTFin,zoneT)

C Initialise heater power to a close value
C     Curent needed to take the load
      batCur=CurForPow(IPWC,batTIni,DODIni,batMode,dtTStep,pLoad)
C     Internal heat generated by that curent
      pIntHeat=InternalHeat(IPWC,batTIni,DODIni,batMode,dtTStep,batCur)
C     The power to the heaters is the difference between what is needed and what is generated internally
C     The power to the heater can not exceed what is available
      pHeaters = MIN(MAX(0.0,pAvail-pLoad),MAX(0.0,reqHeat - pIntHeat))

1510  pHDum = pHeaters  ! set dummy value to value at the end of the previous iterration

      IF (batMode .EQ. 0) THEN   !Charge
         ! Battery does not handle load on heaters
         batPow = pLoad
      ELSE  !Discharge
         ! Battery does handle load on heaters
         batPow = pLoad+pHeaters
      ENDIF

C     Curent flowing through the battery      
      batCur = CurForPow(IPWC,batTIni,DODIni,batMode,dtTStep,batPow)

C     Heat generated by that curent flowing through the internal resistance
      pIntHeat=InternalHeat(IPWC,batTIni,DODIni,batMode,dtTStep,batCur)

C     The power to the heaters is the difference between what is needed and what is generated internally
C     Relaxed in order to avoid excessive oscillations
      pHeaters = MIN(MAX(0.0,pAvail-pLoad),
     &           relaxFact*(reqHeat-pIntHeat) + (1-relaxFact)*pHeaters)

C     increment the number of iterrations
      numIter = numIter + 1

      IF((ABS(pHeaters - pHDum).GT.convVal).AND.
     &   (numIter.LT.maxIter)) THEN
      ! Not converged -> repeat loop
         GOTO 1510
      ENDIF

      IF(numIter.GT.maxIter)  THEN
      !Max number of iterations exceeded -> give warning
        WRITE(IUOUT,*) 'Warning Battery Model: Troublesome ',
     &                 'convergence in function HeaterForFinalTemp'
      ENDIF

      HeaterForFinalTemp = pHeaters

      END

C**************************************************************************
C 16 DoCharge
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine returns the power repartition between the load and the heaters
C   during a time step in charge mode and the average voltage of the battery
C - Gives priority to the heaters using only part of the load to recharge when the temp is below operating conditions
C
C INPUTS:
C -batTIni: The battery's initial temperature (°C)
C -zoneT: Zone ambient temperature (°C) 
C -DODIni: The battery's initial depth of discharge
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -maxC: The maximum curent that can flow thriugh the battery (Amp)
C -maxP: The maximum power that can flow thriugh the battery (W)
C -IPWC: Power-only component index number
C -pLoad: The offer (what can be used) (W)
C 
C OUTPUTS:
C -pHeaters: The power to the heaters (W)
C -pBat: The actual power used by the battery (W)
C -batVolt: Battery average voltage corresponding to these loads (V)
C -batTFin: Battery temperature at the end of the time step (°C)
C -batDODFin: Battery DOD at the end of the time step (%)
C
C NOTE:
C - batMode = 0 since this is in charge
c *********************************************************************
      SUBROUTINE DoCharge(IPWC,batTIni,zoneT,DODIni,dtTStep,
     &                    maxC,maxP,pLoad,pBat,pHeaters,
     &                    batVolt,batTFin,batDODFin)
      IMPLICIT NONE
#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C Common holding the power-only component description read from the electric
C network input file.
      COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     &POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     &NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)
      integer npowcom        !- number of power-only components
      integer powcomno       !- the power-only component reference number
      integer powcomid       !- power-only component database id number
      integer powcomphtyp    !- power-only component phase type
      integer powconen       !- nodes connected to the power-only component
      integer npowcdat       !- number of data items associated with the power-only component
      integer npowcdats      !- number of string data items associated with the power-only component
      real    powcdat        !- power-only component numerical data item

C PARAMETERS FROM SIGNATURE
      REAL batTIni,zoneT,DODIni,dtTStep,maxC,maxP,pLoad,
     &     pBat,pHeaters,batVolt,batTFin,batDODFin
      INTEGER IPWC

C Declaration of functions used
      REAL AvgVolt,GetNeededHeat,InternalHeat,CurForPow,BatFinalTemp,
     &     FinalDOD

C Battery Parameters (technology and configuration)
      REAL heatersRes   ! Resistance of the heaters
      REAL batTOper     ! Battery operating temperature (°C)

C Local variables
      REAL maxPheaters     ! Maximum power the heaters can provide (W)
      REAL pIntHeat        ! Heat generated by the current flowing in the internal resistance of the battery
      REAL availForCharge  ! Available heat for charging (Total available - Heaters) (W)
      REAL neededHeatTOper ! Heat needed to get to the operating temperature (W)
      REAL needHTOperLoad  ! Power to the heaters needed to reach the emergency temperature (W)
      REAL pHDum           ! Dummy heater power to verify convergence
      REAL batCur          ! Current flowing through the battery (Amp)
      REAL relaxFact       ! Relaxation factor for iteration loop (to reduce oscillations)
      REAL convVal         ! Convergence is assumed when difference between 2 iterations is smaller than this factor
      INTEGER numIter      ! Number of iterrations done
      INTEGER maxIter      ! Max number of iterrations before we consider there is a convergence problem

C************** Initialisation
      ! Assign value for relaxation factor
      relaxFact = 0.1
      ! Assign convergence criteria
      convVal = 0.001

      ! Initialise the number of iterations
      numIter = 0
      maxIter = 100

      heatersRes = POWCDAT(IPWC,13)
      batTOper = POWCDAT(IPWC,4)

      ! Initialise pBat to the highest rate possible -> maximum voltage
      pBat = maxP
      batCur = maxC

C************** Calculation

C     Heat required from the heaters to get to the operating temperature when going with no charging
      neededHeatTOper=GetNeededHeat(IPWC,batTIni,dtTStep,batTOper,zoneT)

C     Initiate the heat needed from the heater to the total heat needed
      needHTOperLoad = neededHeatTOper

1610  pHDum = pHeaters

C     Find voltage corresponding to discharge rate
      batVolt = AvgVolt(IPWC,batTIni,DODIni,0,dtTStep,batCur)
      
C     Maximum power the heaters can provide = V²/Rheaters
      maxPheaters = (batVolt**2)/heatersRes

C     Make sure the energy directed to the heaters does not exceed the maximums available
      maxPheaters = MIN(pLoad,maxPheaters)

C     Power used by the heaters
      pHeaters = MIN(maxPheaters, needHTOperLoad)

C     What is available for charging is the difference between what is available and what is used by the heaters
      availForCharge = pLoad - pHeaters

C     The power used by the battery is either what is available or the maximum
      pBat = MIN(availForCharge, maxP)

      batCur = CurForPow(IPWC,batTIni,DODIni,0,dtTStep,pBat)

C     Internal heat
      pIntHeat = InternalHeat(IPWC,batTIni,DODIni,0,dtTStep,batCur)

C     Recalculate the heater needs taking into acount the power flowin through the internal resistance of the battery
C     Used the InternalHeat function instead of the HeaterForFinalTemp one since we already know the current and 
C     there is no need to go once more through the iterration loop
      needHTOperLoad=(1-relaxFact)*needHTOperLoad +
     &               relaxFact*(neededHeatTOper - pIntHeat)

C     increment the number of iterrations
      numIter = numIter + 1

      IF((ABS(pHeaters - pHDum).GT.convVal).AND.
     &   (numIter.LT.maxIter) .AND. (numIter.GT.1)) THEN
      ! Not converged -> repeat loop
         GOTO 1610
      ENDIF

      IF(numIter.GT.maxIter)  THEN
      !Max number of iterations exceeded -> give warning
        WRITE(IUOUT,*) 'Warning Battery Model: Troublesome ',
     &                 'convergence in function DoCharge'
      ENDIF

C Get battery's final state      
      batTFin=BatFinalTemp(IPWC,batTIni,dtTStep,pIntHeat,pHeaters,zoneT)
      batDODFin = FinalDOD(IPWC,batTIni,DODIni,0,dtTStep,batCur)

      END

C**************************************************************************
C 17 DoDischarge
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine returns the power repartition between the load and the heaters
C   during a time step in discharge mode
C - Gives priority to the heaters (meeting only part of the load) when the temp is below emergency conditions
C - Meets the load and maintain the temp as close as possible to operating conditions for non emergency conditions
C
C INPUTS:
C -batTIni: The battery's initial temperature (°C)
C -zoneT: Zone ambient temperature (°C) 
C -DODIni: The battery's initial depth of discharge
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -maxC: The maximum curent available from the battery (Amp)
C -maxP: The maximum power available from the battery (W)
C -IPWC: Power-only component index number
C -pLoad: The requested (demand) load (W)
C 
C OUTPUTS:
C -pHeaters: The power to the heaters (W)
C -pBat: The actual power used by the battery (W)
C -batVolt: Battery average voltage corresponding to these loads (V)
C -batTFin: Battery temperature at the end of the time step (°C)
C -batDODFin: Battery DOD at the end of the time step (%)
C
C NOTE:
C - batMode = 1 since this is in discharge
C *********************************************************************
      SUBROUTINE DoDischarge(IPWC,batTIni,zoneT,DODIni,dtTStep,
     &                       maxC,maxP,pLoad,pBat,pHeaters,
     &                       batVolt,batTFin,batDODFin)
      IMPLICIT NONE
#include "power.h"

C Common holding the power-only component description read from the electric
C network input file.
      COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     &POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     &NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)
      integer npowcom        !- number of power-only components
      integer powcomno       !- the power-only component reference number
      integer powcomid       !- power-only component database id number
      integer powcomphtyp    !- power-only component phase type
      integer powconen       !- nodes connected to the power-only component
      integer npowcdat       !- number of data items associated with the power-only component
      integer npowcdats      !- number of string data items associated with the power-only component
      real    powcdat        !- power-only component numerical data item

C PARAMETERS FROM SIGNATURE
      REAL batTIni,zoneT,DODIni,dtTStep,maxC,maxP,pLoad,pBat,pHeaters,
     &     batVolt,batTFin,batDODFin
      INTEGER IPWC

C Declaration of functions used
      REAL AvgVolt,GetNeededHeat,HeaterForFinalTemp,InternalHeat,
     &CurForPow,BatFinalTemp,FinalDOD

C Battery Parameters (technology and configuration)
      REAL heatersRes   ! Resistance of the heaters
      REAL batTEmer     ! Emergency temperature below which the thermal management system has priority over the load (°C)
      REAL batTOper     ! Battery operating temperature (°C)

C Local variables
      REAL maxPheaters     ! Maximum power the heaters can provide (W)
      REAL availForHeaters ! Available heat for heaters (Total available - Load) (W)
      REAL pIntHeat        ! Heat generated by the current flowing in the internal resistance of the battery
      REAL neededHeatTEmer ! Heat needed to get to the emergency temperature (W)
      REAL needHTEmerMaxLoad ! Power to the heaters needed to reach the emergency temperature when using the maximum battery capacity (W)
      REAL neededHeatTOper ! Heat needed to get to the operating temperature (W)
      REAL needHTOperLoad  ! Power to the heaters needed to reach the operating temperature when meeting the load (W)
      REAL needHTOperMaxLoad ! Power to the heaters needed to reach the operating temperature when using the maximum battery capacity (W)
      REAL batCur          ! Curent flowing through the battery (Amp)

C************** Initialisation

      heatersRes = POWCDAT(IPWC,13)
      batTEmer = POWCDAT(IPWC,14)
      batTOper = POWCDAT(IPWC,4)

C************** Calculation

      ! Heat needed to get the temperature from batTempInit to  emergency temperature
      neededHeatTEmer=GetNeededHeat(IPWC,batTIni,dtTStep,batTEmer,zoneT)
      needHTEmerMaxLoad=neededHeatTEmer - 
     &                  InternalHeat(IPWC,batTIni,DODIni,1,dtTStep,maxC)
      
      ! Heat needed to get the temperature from batTempInit to  operating temperature
      neededHeatTOper=GetNeededHeat(IPWC,batTIni,dtTStep,batTOper,zoneT)
      needHTOperLoad=HeaterForFinalTemp(IPWC,batTIni,batTOper,DODIni,
     &               1,dtTStep,maxP,pLoad,zoneT)
      needHTOperMaxLoad=neededHeatTOper -
     &                  InternalHeat(IPWC,batTIni,DODIni,1,dtTStep,maxC)

C     Max available for the heaters(Assuming All the available power in the battery is used->maxPBat)
C     Maximum power the heaters can provide = V²/Rheaters
      maxPheaters = ((AvgVolt(IPWC,batTIni,DODIni,1,dtTStep,maxC))**2)
     &                /heatersRes

C     Make sure the energy directed to the heaters does not exceed the maximums available
      maxPheaters = MIN(maxP,maxPheaters)

c     Power available for the heaters if the load is met
      availForHeaters = MIN(MAX(0.0,(maxP-pLoad)),maxPheaters)

C 3 possible situations:
C 1- T below emergency
C 2- T between emergency and operation
C 3- T above operation

C     First situation
      IF (needHTEmerMaxLoad .GT. availForHeaters) THEN
      ! Emergency situation, there is not enough power available for the heaters
      ! to increase the temperature to the emergency level -> heaters have the priority

C        All the available current will definitely flow in the battery and is therefore 
C        used to evaluate the Internal heat generation
         pHeaters =  needHTEmerMaxLoad

C        Make sure that pHeater heat does not exceed the maximum available heaters' capacity
         pHeaters = MIN(maxPheaters,pHeaters)

C        Evaluate the portion of the load that can be met
         pBat = maxP - pHeaters

C     Second situation
      ELSEIF(needHTOperMaxLoad .GT. availForHeaters) THEN
C     There is enough energy for the heaters to exceed the emergency temperature
C     But not enough to be above the operating temperature

C        Throw all that is left to the heaters
         pHeaters =  availForHeaters

C        Battery takes the load
         pBat = MIN(pLoad,maxP)

C     Third situation
      ELSE
C     If all available heat is thrown to the heaters, the temperature will exceed the operating temperature

C        Limit the power to the heaters to what is needed to maintain the temp to the operating point
         pHeaters = needHTOperLoad

C        Make sure that pHeaters does not exceed the heaters' capacity
         pHeaters = MIN(maxPheaters,pHeaters)

C        Battery takes the load
         pBat = MIN(pLoad,maxP)

      ENDIF

      batCur = CurForPow(IPWC,batTIni,DODIni,1,dtTStep,pBat+pHeaters)

      batVolt = AvgVolt(IPWC,batTIni,DODIni,1,dtTStep,batCur)

      pIntHeat = InternalHeat(IPWC,batTIni,DODIni,1,dtTStep,batCur)

C     Get battery's final state      
      batTFin=BatFinalTemp(IPWC,batTIni,dtTStep,pIntHeat,pHeaters,zoneT)
      batDODFin = FinalDOD(IPWC,batTIni,DODIni,1,dtTStep,batCur)

      END

