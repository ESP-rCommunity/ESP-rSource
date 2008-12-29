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
C     This function will return the state of the windows when windows modelling is used.
C     This function assumes the ac unit can meet the demand. 
C     This assumes the window_open setpoint is less than the AC cooling setpoint. 

C     The requirements are.


c     The requirements of the function  is:
C     0. bWindowControlFlag = .false. return false
C     1. The Zone Temperature >= fACSetPoint return false. 
C     2. The Zone Temperature < fACSetPoint return result of requirement 3, 4, 5,6, 7.
C     2.1a. The Zone Temperature <= fWindowLowSetpoint return false. 
C     2.1b. The Zone Temperature >  fWindowLowSetpoint return true. 
C     2.2a   Ext Temp. <= fWindowSetpoint return .true. 
C     2.2b   Ext Temp. > fWindowSetpoint return .false. 
C     2.3a    (Zone_temp. - External_Temp) >=  fDeltaTInttoExt return .true.
C     2.3b    (Zone_temp. - External_Temp) <  fDeltaTInttoExt return .false.


C The inputs required are 

C From 

C     Exterior Temperature at which and below the windows could open.
C      REAL fWindowSetpoint

C     The AC Temperature setpoint. Obtained from 
C      REAL fACSetPoint

C     The zone at which the temperature is controlled against.
C      INTEGER iWindowControlZone

C     The flag which indicates the window feature is active.
C      LOGICAL bWindowControlFlag

C     Common block variable to tell other routines that the windows are open. 
C     (This saves processor time)
C      LOGICAL bWindowsAreOpen

C     The DeltaT that is used to detemine if cooling can be used from the outside.
C     Thus Windows could be open when DeltaT >= Zone Temp - ExteriorTemp
C      REAL  fDeltaTInttoExt

C     The temperature at which and below that the windows remain closed.
C     This prevents the zone becoming too cold. 
C      REAL fWindowLowSetpoint



c ***********************************************************************
      
      
      
      LOGICAL FUNCTION AreWindowsOpen()
      IMPLICIT NONE
#include "building.h"
#include "control.h"


C Future values common block.

      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
C     Zone Temp.
      REAL	TFA,QFA

C     Present Values.
      COMMON/PVALA/TPA(MCOM),QPA(MCOM)
C     Zone Temp
      REAL	TPA,QPA

C Climate 
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      REAL	QFP,QFF,TP
C     Ambiant Temperature at Future Timestep.
      REAL	TF,QDP,QDF,VP,VF,DP,DF,HP,HF 

C Common block required for getting AC Setpoint
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      INTEGER	ICF,IDTYP,IPER
      REAL	BB1,BB2,BB3
      INTEGER	IICOMP
      REAL	TNP,QFUT,TFUT

C Common block for the window model.           
      COMMON/WindowControl/
     &     fWindowSetpoint,
     &     fACSetPoint, 
     &     bWindowControlFlag,
     &     iWindowControlZone,
     &     bWindowsAreOpen,
     &     fDeltaTInttoExt,
     &     fWindowLowSetpoint


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
      LOGICAL bWindowsAreOpen

C     The DeltaT that is used to detemine if cooling can be used from the outside.
C     Thus Windows could be open when DeltaT >= Zone Temp - ExteriorTemp
      REAL  fDeltaTInttoExt

C     The temperature at which and below that the windows remain closed.
C     This prevents the zone becoming too cold. 
      REAL fWindowLowSetpoint

C****DEBUG CODE**** 


C****END DEBUG CODE**** 


C Get the cooling set point of the ac from this array.
      fACSetPoint=BMISCD(ICF,IDTYP,IPER,7) 

    

      
C     Default value initialisation. 
      AreWindowsOpen = .false.
      
C     Check if WindowOpenControl model is running. This value is from the aim-2 input file. 
      IF (bWindowControlFlag) THEN
    
C     Check if AC-Cooling is required. The air conditioner will only be used if the zone temperature exceed 
C     the AC set-point. This will force the windows to closed in this case. 
         IF ( (TPA(iWindowControlZone)+TFA(iWindowControlZone))/2.0 
     &        .ge.  (fACSetPoint+ 1.0) ) THEN
            
C     Ac -Cooling is required, windows closed.
            AreWindowsOpen = .false.
            
C     Check if window cooing is required
         ELSEIF (
C     If the exterior temperature is lower then the windows exterior setpoint.
     &           TF .le. fWindowSetpoint 
     &           .and.
C     
C     If the Zone temperature is greater then the window low setpoint. 
     &           TPA(iWindowControlZone) .gt. fWindowLowSetpoint
     &           .and.
C     
C     If there is a big enough delta T between the Zone temp and the outdoor temperature. 
     &           ( 
     &           (TPA(iWindowControlZone)+TFA(iWindowControlZone))
     &           /2.0  
     &           -TF) 
     &           .ge. fDeltaTInttoExt )THEN

C     If all above  is true open the windows.
            AreWindowsOpen = .true.

         ENDIF

      ELSE
C     Too hot to use outdoor cooling. Close all windows, will use AC if available when setpoint is tripped. 
         AreWindowsOpen = .false.
      ENDIF
C     Set common block variable to make inforamtion available globally. 
      bWindowsAreOpen = AreWindowsOpen

C     Debug    
C      call AreWindowsOpenDebug
      return  

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
      REAL	QFP,QFF
C     Present and future timestep.
      REAL	TP,TF,QDP,QDF,VP
C Wind velocity (m/s).
      REAL	VF,DP,DF,HP,HF      

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      INTEGER ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP



      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      INTEGER	IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      INTEGER IDAYN
      INTEGER IMONTH
      INTEGER IWINDOWFLAG

C Future values common block.

      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
C     Zone Temp.
      REAL	TFA,QFA

C     Present Values.
      COMMON/PVALA/TPA(MCOM),QPA(MCOM)
C     Zone Temp
      REAL	TPA,QPA



C Common block for the window model.           
      COMMON/WindowControl/
     &     fWindowSetpoint,
     &     fACSetPoint, 
     &     bWindowControlFlag,
     &     iWindowControlZone,
     &     bWindowsAreOpen,
     &     fDeltaTInttoExt,
     &     fWindowLowSetpoint
          

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
      LOGICAL bWindowsAreOpen

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
      
      if (bWindowsAreOpen) then 
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



