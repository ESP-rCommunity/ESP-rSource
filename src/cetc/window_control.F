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

C*********************************************************************
c LOGICAL FUNCTION AreWindowsOpen.

c Created by: Phylroy Lopez
c Initial Creation Date: Feb 12th 2004
c Copyright 2004: NRCan Buildings Group
c
C This function will return the state of the windows when windows modelling is used.

C The logic verifies the the window position as follows:
C     1) IF Window Control is not in use then the windows are closed
C     2) ELSEIF the Central cooling system (having capacity >= 1 W) temperature sensor >= temperature setpoint windows are closed
C     3) ELSEIF there is active airpoint heating or cooling in the zone (determined by flux /= 0) the windows are closed
C     4) ELSEIF the zone temperature is <= the Low Setpoint the windows are closed
C     5) ELSEIf the zone temperature is >= the High Setpoint AND the DeltaT is >= specified AND Ambient temperature >= fMinAmbTemp THEN the windows are opened
C     6) ELSE do nothing which allows for the deadband in window operation


C The inputs required are:
C     1) Ambient temperature
C     2) Zone temperatures (central and window zone)
C     3) Central cooling system characteristics (capacity and setpoint)
C     4) Zone airpoint heat flux
C     5) Variables specified in COMMON/WindowControl/ (see descriptions below)


c ***********************************************************************
      
      
      
      LOGICAL FUNCTION AreWindowsOpen(iComp)
      IMPLICIT NONE
#include "building.h"
#include "control.h"

!     Passed variable
      Integer iComp ! Zone number

C Future values common block.

      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
C     Zone Temp.
      REAL  TFA,QFA

C Climate 
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      REAL  QFP,QFF,TP
C     Ambiant Temperature at Future Timestep.
      REAL  TF,QDP,QDF,VP,VF,DP,DF,HP,HF 

C Common block required for getting AC Setpoint
       COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
       INTEGER  ICF,IDTYP,IPER
       REAL  BB1,BB2,BB3
       INTEGER  IICOMP
       REAL  TNP,QFUT,TFUT

C Common block for the window model.           
      COMMON/WindowControl/
     &     bWindowControlFlag,
     &     iCoolingControlZone,
     &     fWindowHighSetpoint,
     &     fWindowLowSetpoint,
     &     fDeltaTInttoExt,
     &     fMinAmbTemp,
     &     bWindowsAreOpen

      LOGICAL bWindowControlFlag !Indicates that the window_control.F model should activate
      INTEGER iCoolingControlZone !The zone in which an active central cooling system sensor is place (usually a main level zone 1, should correspond to BC01 type and use of NRCan HVAC module)
      REAL fWindowHighSetpoint !Temperature above which windows open
      REAL fWindowLowSetpoint !Temperature below which windows close (allowing an operating deadband)
      REAL fDeltaTInttoExt !Temperature difference (Tinterior - Texterior) required to open windows (so they are only opened when natural cooling will occur and is of significance)
      REAL fMinAmbTemp !The minimum ambient temperature that windows are allowed to open too (otherwise it is possible to open windows when it is cold and replace the entire zone air mass in the timestep)
      LOGICAL bWindowsAreOpen(MCOM) !Array to store the window position for each zone



C DEBUG for Checking Controller values
!     write (*,*) 'ICF is ', ICF, '; IDTYP is ', IDTYP, '; IPER is ',
!    &  IPER, '; fACsetpoint is ', fACSetPoint, '; fACCapacityW is', 
!    &  fACCapacityW


C LOGIC
C     Check if WindowOpenControl model is not running 
      IF (.not. bWindowControlFlag) THEN
        bWindowsAreOpen(iComp) = .false.

C     Check if the cooling system has capacity AND will turn on
C       BMISCD(1,IDTYP,IPER,4) is hardcoded to controller 1 cooling capacity
C       BMISCD(1,IDTYP,IPER,7) is hardcoded to controller 1 cooling temperature setpoint
      ELSEIF ( BMISCD(1,IDTYP,IPER,4) .ge. 1 .and. !Check cooling capacity is greater than 1 W
     &  TFA(iCoolingControlZone) .ge. BMISCD(1,IDTYP,IPER,7)) THEN
        bWindowsAreOpen(iComp) = .false.
    
C     Check if Cooling or Heating is on in the zone based on the air point heat flux
      ELSEIF ( abs(QFA(iComp)) >= 1) THEN
        bWindowsAreOpen(iComp) = .false.

C     Check if zone temperature is below Low Setpoint
      ELSEIF ( TFA(iComp) <= fWindowLowSetpoint) THEN
        bWindowsAreOpen(iComp) = .false.

C     Check if zone temperature is above the High Setpoint AND the temperature is great enough AND it is not too cold outside
      ELSEIF ( TFA(iComp) >= fWindowHighSetpoint .and.
     &  (TFA(iComp) - TF) >= fDeltaTInttoExt .and.
     &  TF >= fMinAmbTemp) THEN
        bWindowsAreOpen(iComp) = .true.

      ELSE
C     Do nothing so as to maintain the window operating deadband

      ENDIF

C     Set AreWindowsOpen to return to the function call. 
      AreWindowsOpen = bWindowsAreOpen(iComp)

C     Debug    
C      call AreWindowsOpenDebug
      RETURN  

      END

C*********************************************************************
c     LOGICAL Subroutine  AreWindowsOpenDebug.
C     
c     Created by: Phylroy Lopez
c     Initial Creation Date: Feb 12th 2004
c     Copyright 2004: NRCan Buildings Group
c     
C     This function is used only for debugging. The Output can be modified to suit your needs.
C     It writes to window.csv in the cwd. 
C      
C     *******************************************************************
      SUBROUTINE AreWindowsOpenDebug
      IMPLICIT NONE
#include "building.h"
C     Debug Common Blocks.
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      REAL  QFP,QFF
C     Present and future timestep.
      REAL  TP,TF,QDP,QDF,VP
C Wind velocity (m/s).
      REAL  VF,DP,DF,HP,HF      

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      INTEGER ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP



      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      INTEGER  IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      INTEGER IDAYN
      INTEGER IMONTH
      INTEGER IWINDOWFLAG

C Future values common block.

      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
C     Zone Temp.
      REAL  TFA,QFA

C     Present Values.
      COMMON/PVALA/TPA(MCOM),QPA(MCOM)
C     Zone Temp
      REAL  TPA,QPA



C Common block for the window model.           
      COMMON/WindowControl/
     &     fWindowSetpoint,
     &     fACSetPoint, 
     &     bWindowControlFlag,
     &     iWindowControlZone,
     &     fDeltaTInttoExt,
     &     fWindowLowSetpoint,
     &     bWindowsAreOpen
         

C     Exterior Temperature at which and below the windows could open.  
      REAL fWindowSetpoint

C     The AC Temperature setpoint. 
      REAL fACSetPoint

C     The zone at which the temperature is controlled against.
      INTEGER iWindowControlZone

C     The flag which indicates the window feature is active.
      LOGICAL bWindowControlFlag

C     Common block variable to tell other routines that the windows are open. 
C     (This saves processor time)
      LOGICAL bWindowsAreOpen(MCOM) !Array to store the window position for each zone

C     The DeltaT that is used to detemine if cooling can be used from the outside.
C     Thus Windows could be open when DeltaT >= Zone Temp - ExteriorTemp
      REAL  fDeltaTInttoExt

C     The temperature at which and below that the windows remain closed.
C     This prevents the zone becoming too cold. 
      REAL fWindowLowSetpoint



      
C     Debug file for win.res. Note: using same unit number as hvac.res!!!
      open(unit=202,file='window.csv',status='unknown')
      
C     get current timestep information
      CALL EDAYR(idyp,IDAYN,imonth)
      
      if (bWindowsAreOpen(iWindowControlZone)) then 
         iwindowflag = 1
      else
         iwindowflag = 0  
      endif

      write(202,101) 
     &     iMonth , ',',
     &     idyp,',',
     &     ihrp,',',
     &     iwindowflag,',',
     &     iWindowControlZone,',',
     &     (TPA(iWindowControlZone)+TFA(iWindowControlZone))
     &     /2.0, ',',
     &     TF,',',
     &     fACSetpoint,',',
     &     fWindowSetPoint,',',
     &     fDeltaTInttoExt,',',
     &     fWindowLowSetpoint,',',
     &     VF
          
 101  format(i3,a3,
     &     i4,a3,
     &     i3,a3,
     &     i3,a3,
     &     i3,a1,
     &     f6.3,a3,
     &     f6.3,a3,
     &     f6.3,a3,
     &     f6.3,a3,
     &     f6.3,a3,
     &     f6.3,a3,
     &     f6.3)



      END



