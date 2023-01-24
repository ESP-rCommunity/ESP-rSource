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

C This file contains the following routines related to the fuel cell
C hot-water storage tank component models.

C mains_temp_draw_profile_coeff_gen Coefficient generator for a water mains monthly temperature and
C                                   hourly daily profiles
C mains_temp_draw_profile_static    Static matrix checker for temperature and draw profile component
C
C mains_draw_daily_profile_static   Static matrix checker for component to set hourly water draw profile
C
C mains_draw_daily_profile_coeff_gen
C                                   Coefficient generator for component to set hourly water draw profile

C *********************************************************************************************
C ****************************** mains_temp_draw_profile_coeff_gen ****************************
C Created by: Kamel Haddad
C Initial Creation Date: February 17th, 2006
C Copyright CETC 2006

C This subroutine is the coefficient generator for a component used to
C set the temperature for
C the mains water. Temperature is set constant for the whole month with
C values either set by the user or based on the Moore's model.
C The component has one connection, and assumes this connection orginates
C from the last component in the DHW delivery system (that is, its temperature
C represents the DHW delivery temperature.) If configured in this way, this
C component will meaningfully compute the thermal load associated with
C the domestic hot water service, no matter how many components actually
C contribute to this service. The flow rate of the upstream connection
C determines the flow rate through this component, while the temperature
C of the connection is ignored.
C
C INPUTS:
C     IPCOMP     Component number within the user defined plant component network
C     ISTATS     Flag indicating whether energy balance or water mass balance is sought
C     ICONDX     ESP-r common variable holding number for plant network connections associated
C                with component
C     NPCDAT     ESP-r common variable holding node numbers for plant components within plant
C                network
C     ADATA(IPCOMP,1)
C                Flag indicating whether mains water temperature is based on user inputs or on
C                the Moore's model
C     ADATA(IPCOMP,i)
C                i = 1,12; Water temperature for each month of the year
C
C OUTPUTS:
C     COUT       List of matrix coefficients
C -------------------------------------------------------------------------------------------
      SUBROUTINE mains_temp_draw_profile_coeff_gen(IPCOMP,COUT,ISTATS)
      use h3kmodule
      IMPLICIT NONE
#include "plant.h"
#include "building.h"

C----------------------------------------------------------------------
C     Passed arguments
C----------------------------------------------------------------------
      INTEGER IPCOMP        ! Number for plant component
      REAL COUT(MPCOE)      ! Array for matrix coefficients for component
      INTEGER ISTATS        ! Flag indicating type of coefficients to be
                            ! generated (=1: energy balance; =2 1st phase
                            ! water mass blance; =3 2nd phase vapour mass
                            ! balance)

C----------------------------------------------------------------------
C     ESP-r commons
C----------------------------------------------------------------------
C-----Trace/error unit reporting numbers
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      INTEGER IUOUT,IUIN,IEOUT
      INTEGER ITC,ICNT
      INTEGER ITCF,ITRACE,IZNTRC,ITU

C-----Common for component data
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      REAL ADATA                ! Component data
      REAL BDATA

C-----Common for present and future simulation time steps
      COMMON/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,idynow
      INTEGER ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,idynow

C.....ESP-r Commons
      common/pcnam/pcname(mpcom)        ! Plant component names
      character*15 pcname

C-----Present and future plant side simulation time steps
      COMMON/PTIME/PTIMEP,PTIMEF
      REAL PTIMEP,PTIMEF

C-----Plant component connection data
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      REAL CONVAR    ! States variables for connections
      INTEGER ICONTP ! Type of each connection
      INTEGER ICONDX ! Pointer to connections for each
                     ! component node

C.....Common containing plant componet mass diversion ratios
      common/c10/npcon,ipc1(mpcon),ipn1(mpcon),ipct(mpcon),ipc2(mpcon),
     &            ipn2(mpcon),pcondr(mpcon),pconsd(mpcon,2)
      integer npcon            ! number of inter-component connections
      integer ipc1             ! component number defining the receiving
                               !   component
      integer ipn1             ! node number defining the receiving component
      integer ipct             ! plant component inter-connection type
      integer ipc2             ! component number defining the sending
                               !   component
      integer ipn2             ! node number defining the sending component
      real    pcondr           ! ratio of mass flow rate through the connection
                               !   and the mass flow rate entering the receiving
                               !   node for each plant component inter-connection
      real    pconsd           ! supplementary data for plant component inter-
                               !   connection

C-----Network and control data
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      INTEGER NPCDAT ! Miscelanous plant data
      INTEGER IPOFS1
      INTEGER IPOFS2

C-----Common contaning total number of plant components in user defined
C-----plant network
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      INTEGER NPCOMP,NCI
      REAL CDATA

C----------------------------------------------------------------------
C     ESP-r functions
C----------------------------------------------------------------------
      REAL fDHW_ColdMainTemp      ! Function for determining mains water
                                  ! temperature for the month

      REAL SHtFld                 ! Function returning specific heat
                                  !   of air/water/vapour

C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      INTEGER mains_temp_flag     ! Mains water temperature calculationmethod
                                  ! flag
                                  ! =1 Moore method
                                  ! =2 Use monthly temperature values entered by
                                  ! user
      INTEGER iday_present        ! Present simulation day of the month
      INTEGER imonth_present      ! Present simulation month of the year
      INTEGER mains_future_hour   ! Future hour for determining mains draw
      LOGICAL close               ! Logical variable used to check how close a number is
                                  ! to another number
      REAL mains_temp             ! Mains temperature for month
      INTEGER mains_node_conn     ! Mains component connection number in the plant network
      INTEGER mains_node_num      ! Mains component node number in the plant network
      INTEGER num_matrix_coeff    ! Total number of matrix coefficients for component
      INTEGER IX1,I

      real fConTemp               ! Upstream connection temperature (oC)
      real fConFlow               ! Upstream connection flow rate (kg/s)
      real fConCp                 ! Specific heat of water flowing
                                  !   through unit (J/kg oC)
      real fDHWLoad               ! Water heater thermal load (W)


C ----Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*)
     &   ' Entering subroutine mains_temp_draw_profile_coeff_gen'

C ----Initialize pointers to the 1 connections to the mains water
C-----and the mains water node.
      mains_node_conn = ICONDX(IPCOMP,1,1)
      mains_node_num = NPCDAT(IPCOMP,9)

C-----Determine month of the year from day of the year
      CALL EDAYR(idyp,iday_present,imonth_present)

C-----Mains water temperature is either set to user entered value or
C-----set based on Moore's model from subroutine fDHW_ColdMainTemp

C-----Flag for method used to find the mains monthly water temperature
C-----=1 Use Moore's method
C-----=2 Use monthly water temperature entered by user
      mains_temp_flag = int(ADATA(IPCOMP,1))

C-----Set mains ground temperature for month imonth_present
C-----If temperatures are based on Moore's model
      IF(mains_temp_flag.EQ.1) THEN
        mains_temp = fDHW_ColdMainTemp(imonth_present)

C-----In the case the mains temperatures are based on user-input
C-----values
      ELSEIF(mains_temp_flag.EQ.2) THEN
        mains_temp = ADATA(IPCOMP,imonth_present + 1)

      ENDIF

C-----Check whether future plant side time ptimef is 24.
      CALL ECLOSE(ptimef,24.,0.0001,CLOSE)

C-----If ptimef is not 24., then the future hour for determining
C-----Mains draw is the integer part of ptimef + 1
      IF(.NOT.CLOSE) THEN
        mains_future_hour = int(ptimef) + 1

C-----If ptimef is close to 24., then the future hour for mains
C-----draw is hour 1
      ELSEIF(CLOSE) THEN
        mains_future_hour = 1

      ENDIF

C ----Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C-------Self coupling coefficient
        COUT(1) = 1.

C-------Cross coupling coefficient
        COUT(2) = 0.

C-------Right-hand side constant term set to the mains temperature
C-------for the month
        COUT(3) = mains_temp

C-----1st phase mass (ie. "water") mass balance coefficients.
C-----What comes in must go out!
      ELSE IF(ISTATS.EQ.2) THEN

C-----Self coupling coefficient
        COUT(1) = 1.

C-----Cross coupling coefficient
        COUT(2) = -1.0 * pcondr ( mains_node_conn )

C-----Right-hand side constant term set to mains draw
        COUT(3) = 0.0

C-----2nd phase mass (ie. "vapour") mass balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1) = 1.
         COUT(2) = 0.
         COUT(3) = 0.

      ENDIF

C-----Compute thermal load associated with water service. This figure
C-----is only meaningful if the incomming connection represents the
C-----DHW delivery temperature.

      fConTemp = CONVAR ( mains_node_conn, 1 )  ! (oC)
      fConFlow = CONVAR ( mains_node_conn, 2 )  ! (kg/s)

C-----Calculate specific heat capactiy
C-----   = Shtfld ( X , Temperature ) (J/kg oC)
C-----     where X=1 -> air, X=2 -> vapor, X=3 -> liq. water

      fConCp = Shtfld ( 3, ( fConTemp + mains_temp ) /2 )

C-----Calculate thermal load (W):
C-----
C-----   = ( flow tate ) * ( heat capacity )
C-----                         * ( delivery temp - supply temp )
C-----

      fDHWLoad = fConFlow * fConCp * ( mains_temp - fConTemp )

C-----Report thermal load: Format string as: 'plant/NAME/misc_data'
      call AddToReport(rvPltDHWTermalLoad%Identifier,
     &      fDHWLoad,
     &      pcname(iPComp)(1:iPltNameLen(iPComp)))

C-----Trace.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node mains temp and draw profiles'
         WRITE(ITU,*) ' Matrix node(s) ',mains_node_num
         WRITE(ITU,*) ' Connection(s)  ',mains_node_conn
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' Mains water temp ',
     &                     mains_temp,' (Deg C)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         num_matrix_coeff = 3
         WRITE(ITU,*) (COUT(I), I = 1, num_matrix_coeff)
         IF(ITU.EQ.IUOUT) THEN   ! trace output going to screen, not file
            IX1 = (IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew   ! write 4 lines at a time
         END IF
         WRITE(ITU,*) ' Leaving sub mains_temp_draw_profile_coeff_gen'
      END IF
      RETURN

      END


C *********************************************************************************************
C ****************************** mains_temp_draw_profile_static *******************************
C Created by: Kamel Haddad
C Initial Creation Date: February 21st, 2006
C Copyright CETC 2006

C This subroutine checks the number of controlled variables, connections, and fluid
C type for the component for mains water temperature and draw

C INPUTS:
C     IPCOMP     Component number within the user defined plant component network
C     NCI        ESP-r common variable for number of controlled variable for component
C     ICONDX     ESP-r common variable holding number for plant network connections associated
C                with component
C     ICONTP     ESP-r common variable for type of fluid for connections
C     NPCDAT     ESP-r common variable holding node numbers for plant components within plant
C                network
C     ADATA(IPCOMP,1)
C                Flag indicating whether mains water temperature is based on user inputs or on
C                the Moore's model
C     ADATA(IPCOMP,i)
C                i = 1,12; Mains water temperature for each month of the year
C     ADATA(IPCOMP,i)
C                i = 13,37; Mains water draw in L/hr for each hour of the day
C -------------------------------------------------------------------------------------------
      SUBROUTINE mains_temp_draw_profile_static(IPCOMP)

      IMPLICIT NONE
#include "plant.h"
#include "building.h"

C----------------------------------------------------------------------
C     Passed arguments
C----------------------------------------------------------------------
      INTEGER IPCOMP        ! Number for plant component

C----------------------------------------------------------------------
C     ESP-r commons
C----------------------------------------------------------------------
C-----Trace/error unit reporting numbers
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      INTEGER IUOUT,IUIN,IEOUT
      INTEGER ITC,ICNT
      INTEGER ITCF,ITRACE,IZNTRC,ITU

C-----Common containing total number of plant components in user defined
C-----plant network
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      INTEGER NPCOMP,NCI
      REAL CDATA

C-----Plant component connection data
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      REAL CONVAR    ! States variables for connections
      INTEGER ICONTP ! Type of each connection
      INTEGER ICONDX ! Pointer to connections for each
                     ! component node

C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      INTEGER NCITM  ! Number of controlled variables for component
      INTEGER NCONPN ! Number of connections per node
      INTEGER NCONS  ! Number of connections
      INTEGER IPCON1 ! Incoming connecction number
      INTEGER IX1,IPCONC

C-----Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
      WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
      WRITE(ITU,*) ' 1-node mains water temperature and draw profiles'
         IF(ITU.EQ.IUOUT) THEN
            IX1 = (IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C-----Check user specified number of controlled variables
      NCITM = 0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' mains_temp_draw_profile_static warning:',
     &                ' user specified wrong number of controlled',
     &                ' variables'

C-----Check component has 1st connection to water
      NCONS = 1
      NCONPN = 1

C-----Loop through max number of connections for each node
      DO 10 IPCONC = 1,MPCONC

      IPCON1 = ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONPN) THEN

C-----In case there is no connection to component, then issue error
C-----message
        IF(IPCON1.EQ.0) THEN
C-----Error handling
      WRITE(IUOUT,*) ' mains_temp_draw_profile_static: connection error'
      WRITE(IUOUT,*) ' for component ',IPCOMP,' should be ',NCONS
      WRITE(IUOUT,*) ' connection to water'
      STOP ' mains_temp_draw_profile_static: unresolvable error'
C-----In the case that the connection fluid is not water then issue
C-----error message
        ELSE IF(ICONTP(IPCON1).NE.20) THEN
C-----Error handling
      WRITE(IUOUT,*) ' mains_temp_draw_profile_static: connection error'
      WRITE(IUOUT,*) ' for component ',IPCOMP,' should be ',NCONS
      WRITE(IUOUT,*) ' connection to water'
      STOP ' mains_temp_draw_profile_static: unresolvable error'
        END IF

C-----If there are more connections than allowed for component then
C-----issue error message
      ELSE IF(IPCON1.NE.0) THEN
C-----Error handling
      WRITE(IUOUT,*) ' mains_temp_draw_profile_static: connection error'
      WRITE(IUOUT,*) ' for component ',IPCOMP,' should be ',NCONS
      WRITE(IUOUT,*) ' connection to water'
      STOP ' mains_temp_draw_profile_static: unresolvable error'
      END IF

   10 CONTINUE

      RETURN

      END

C *********************************************************************************************
C ****************************** mains_draw_daily_profile_static ******************************
C Created by: Kamel Haddad
C Initial Creation Date: November 23rd, 2006
C Copyright CETC 2006

C This subroutine checks the number of controlled variables, connections, and fluid
C type for the component for water draw profile

C INPUTS:
C     IPCOMP     Component number within the user defined plant component network
C     NCI        ESP-r common variable for number of controlled variable for component
C     ICONDX     ESP-r common variable holding number for plant network connections associated
C                with component
C     ICONTP     ESP-r common variable for type of fluid for connections
C     NPCDAT     ESP-r common variable holding node numbers for plant components within plant
C                network
C     ADATA(IPCOMP,i)
C                i = 1,24; Mains water draw in L/hr for each hour of the day
C -------------------------------------------------------------------------------------------
      SUBROUTINE mains_draw_daily_profile_static(IPCOMP)

      IMPLICIT NONE
#include "plant.h"
#include "building.h"

C----------------------------------------------------------------------
C     Passed arguments
C----------------------------------------------------------------------
      INTEGER IPCOMP        ! Number for plant component

C----------------------------------------------------------------------
C     ESP-r commons
C----------------------------------------------------------------------
C-----Trace/error unit reporting numbers
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      INTEGER IUOUT,IUIN,IEOUT
      INTEGER ITC,ICNT
      INTEGER ITCF,ITRACE,IZNTRC,ITU

C-----Common containing total number of plant components in user defined
C-----plant network
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      INTEGER NPCOMP,NCI
      REAL CDATA

C-----Plant component connection data
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      REAL CONVAR    ! States variables for connections
      INTEGER ICONTP ! Type of each connection
      INTEGER ICONDX ! Pointer to connections for each
                     ! component node

C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      INTEGER NCITM  ! Number of controlled variables for component
      INTEGER NCONPN ! Number of connections per node
      INTEGER NCONS  ! Number of connections
      INTEGER IPCON1 ! Incoming connecction number
      INTEGER IX1,IPCONC

C-----Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
      WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
      WRITE(ITU,*) ' 1-node water hourly draw profile'
         IF(ITU.EQ.IUOUT) THEN
            IX1 = (IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C-----Check user specified number of controlled variables
      NCITM = 0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*)
     &   ' mains_draw_daily_profile_static warning: user specified',
     &   ' wrong number of controlled variables'

C-----Check component has 1st connection to water
      NCONS = 1
      NCONPN = 1

C-----Loop through max number of connections for each node
      DO 10 IPCONC = 1,MPCONC

      IPCON1 = ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONPN) THEN

C-----In case there is no connection to component, then issue error
C-----message
        IF(IPCON1.EQ.0) THEN
C-----Error handling
      WRITE(IUOUT,*) ' mains_draw_daily_profile_static: connection '
      WRITE(IUOUT,*) ' error for component ',IPCOMP,' should be ',NCONS
      WRITE(IUOUT,*) ' connection to water'
      STOP ' mains_draw_daily_profile_static: unresolvable error'
C-----In the case that the connection fluid is not water then issue
C-----error message
        ELSE IF(ICONTP(IPCON1).NE.20) THEN
C-----Error handling
      WRITE(IUOUT,*) ' mains_draw_daily_profile_static: connection '
      WRITE(IUOUT,*) ' error for component ',IPCOMP,' should be ',NCONS
      WRITE(IUOUT,*) ' connection to water'
      STOP ' mains_draw_daily_profile_static: unresolvable error'
        END IF

C-----If there are more connections than allowed for component then
C-----issue error message
      ELSE IF(IPCON1.NE.0) THEN
C-----Error handling
      WRITE(IUOUT,*) ' mains_draw_daily_profile_static: connection '
      WRITE(IUOUT,*) ' error for component ',IPCOMP,' should be ',NCONS
      WRITE(IUOUT,*) ' connection to water'
      STOP ' mains_draw_daily_profile_static: unresolvable error'
      END IF

   10 CONTINUE

      RETURN

      END

C *********************************************************************************************
C ****************************** mains_draw_daily_coeff_gen ***********************************
C Created by: Kamel Haddad
C Initial Creation Date: November 23rd, 2006
C Copyright CETC 2006

C This subroutine is the coefficient generator for a component used to set water draw mass fow rate
C based on a daily hourly profile. The component has one connection. The temperature at the exit
C of the component is unchanged equal to the temperature at the inlet.

C INPUTS:
C     IPCOMP     Component number within the user defined plant component network
C     ISTATS     Flag indicating whether energy balance or water mass balance is sought
C     ICONDX     ESP-r common variable holding number for plant network connections associated
C                with component
C     NPCDAT     ESP-r common variable holding node numbers for plant components within plant
C                network
C     ADATA(IPCOMP,i)
C                i = 1,24; Mains water draw in L/hr for each hour of the day

C OUTPUTS:
C     COUT       List of matrix coefficients
C -------------------------------------------------------------------------------------------
      SUBROUTINE mains_draw_daily_profile_coeff_gen(IPCOMP,COUT,ISTATS)
      IMPLICIT NONE
#include "plant.h"
#include "building.h"

C----------------------------------------------------------------------
C     Passed arguments
C----------------------------------------------------------------------
      INTEGER IPCOMP        ! Number for plant component
      REAL COUT(MPCOE)      ! Array for matrix coefficients for component
      INTEGER ISTATS        ! Flag indicating type of coefficients to be
                            ! generated (=1: energy balance; =2 1st phase
                            ! water mass blance; =3 2nd phase vapour mass
                            ! balance)

C----------------------------------------------------------------------
C     ESP-r commons
C----------------------------------------------------------------------
C-----Trace/error unit reporting numbers
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      INTEGER IUOUT,IUIN,IEOUT
      INTEGER ITC,ICNT
      INTEGER ITCF,ITRACE,IZNTRC,ITU

C-----Common for component data
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      REAL ADATA                ! Component data
      REAL BDATA

C-----Common for present and future simulation time steps
      COMMON/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,idynow
      INTEGER ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,idynow

C-----Present and future plant side simulation time steps
      COMMON/PTIME/PTIMEP,PTIMEF
      REAL PTIMEP,PTIMEF

C-----Plant component connection data
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      REAL CONVAR    ! States variables for connections
      INTEGER ICONTP ! Type of each connection
      INTEGER ICONDX ! Pointer to connections for each
                     ! component node

C-----Common containing ratio for flow rate diversions
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      INTEGER NPCON,IPC1,IPN1,IPCT,IPC2,IPN2
      REAL PCONDR               ! Flow rate diversions
      REAL PCONSD

C-----Network and control data
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      INTEGER NPCDAT ! Miscelanous plant data
      INTEGER IPOFS1
      INTEGER IPOFS2

C-----Common contaning total number of plant components in user defined
C-----plant network
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      INTEGER NPCOMP,NCI
      REAL CDATA

C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      INTEGER iday_present        ! Present simulation day of the month
      INTEGER imonth_present      ! Present simulation month of the year
      INTEGER future_hour         ! Future hour for determining water draw
      LOGICAL close               ! Logical variable used to check how close a number is
                                  ! to another number
      REAL water_draw             ! Waters draw for future hour
      INTEGER comp_node_conn      ! Component connection number in the plant network
      INTEGER comp_node_num       ! Component node number in the plant network
      INTEGER num_matrix_coeff    ! Total number of matrix coefficients for component
      INTEGER IX1,I

C ----Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*)
     &   ' Entering subroutine mains_draw_daily_profile_coeff_gen'

C ----Initialize pointers to the 1 connection to the component.
      comp_node_conn = ICONDX(IPCOMP,1,1)
      comp_node_num = NPCDAT(IPCOMP,9)

C-----Determine month of the year from day of the year
      CALL EDAYR(idyp,iday_present,imonth_present)

C-----Check whether future plant side time ptimef is 24.
      CALL ECLOSE(ptimef,24.,0.0001,CLOSE)

C-----If ptimef is not 24., then the future hour for determining
C-----water draw is the integer part of ptimef + 1
      IF(.NOT.CLOSE) THEN
        future_hour = int(ptimef) + 1

C-----If ptimef is close to 24., then the future hour for water
C-----draw is hour 1
      ELSEIF(CLOSE) THEN
        future_hour = 1

      ENDIF

C-----Set water draw depending on the future hour. Convert Liters/hour to
C-----kg/s.

      water_draw = ADATA(IPCOMP,future_hour) / 3600.

C ----Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C-------Self coupling coefficient
        COUT(1) = 1.

C-------Cross coupling coefficient
        COUT(2) = -1

C-------Right-hand side constant term set to the mains temperature
C-------for the month
        COUT(3) = 0.

C-----1st phase mass (ie. "water") mass balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN

C-----Self coupling coefficient
        COUT(1) = 1.

C-----Cross coupling coefficient
        COUT(2) = 0.

C-----Right-hand side constant term set to mains draw
        COUT(3) = water_draw

C-----2nd phase mass (ie. "vapour") mass balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1) = 1.
         COUT(2) = 0.
         COUT(3) = 0.

      ENDIF

C-----Trace.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node water hourly draw profile'
         WRITE(ITU,*) ' Matrix node(s) ',comp_node_num
         WRITE(ITU,*) ' Connection(s)  ',comp_node_conn
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' Water draw = ',
     &                     water_draw,' (kg/s)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         num_matrix_coeff = 3
         WRITE(ITU,*) (COUT(I), I = 1, num_matrix_coeff)
         IF(ITU.EQ.IUOUT) THEN   ! trace output going to screen, not file
            IX1 = (IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew   ! write 4 lines at a time
         END IF
         WRITE(ITU,*) ' Leaving sub mains_draw_daily_profile_coeff_gen'
      END IF
      RETURN

      END
