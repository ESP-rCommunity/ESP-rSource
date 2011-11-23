C ====================================================================
C This file is part of the ESP-r system.
C Copyright CANMET Energy Technology Centre
C Natural Resources Canada, Government of Canada
C 2007. Please Contact Ian Beausoleil-Morrison for details
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

C===================== stratified_tank_static ==========================
C     Created by: Didier Thevenard
C     Created on: April 2007
C     Copyright:  CETC 2007
C-----------------------------------------------------------------------
C     ABSTRACT:
C     Establishes for a stratified tank whether the correct 
C     number of control variables have been specified, whether the 
C     number of water connections is correct and whether the connections 
C     are to the correct fluid type.
C
C=======================================================================

      SUBROUTINE stratified_tank_static(IPCOMP)

C---- Set implicit to none to force declaration of all variables

      IMPLICIT NONE
      
C---- Include statements

      INCLUDE "plant.h"
      INCLUDE "building.h"

C---- Arguments

      INTEGER IPCOMP
      
C---- Common blocks

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN
      
      COMMON/TC/ITC,ICNT
      INTEGER ITC,ICNT
      
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      INTEGER ITCF,ITRACE,IZNTRC,ITU

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      INTEGER NPCOMP,NCI
      REAL CDATA

      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      REAL ADATA,BDATA

      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      REAL CONVAR
      INTEGER ICONTP,ICONDX

      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      REAL PCTF,PCRF,PUAF,PCQF,PCNTMF,PCTP,PCRP,PUAP,PCQP,PCNTMP

C---- Stratified tank common

      INCLUDE "stratified_tank.h"
      
C---- Local variables      

      INTEGER NITMS
C      INTEGER I
      INTEGER J
      INTEGER NCITM,NCONS
      INTEGER IPCON,IPCONC
      LOGICAL CLOSEA
      LOGICAL ERROR_FLAG         
      INTEGER NNODES            ! Number of nodes of stratified tank
                                
      REAL HTANK                ! Tank height (m)
      REAL HIN1                 ! Height of inlet 1 (m)
      REAL HOUT1                ! Height of outlet 1 (m)
      REAL HIN2                 ! Height of inlet 2 (m)
      REAL HOUT2                ! Height of outlet 2 (m)
      REAL TINIT                ! Initial temperature of stratified tank (C)
      REAL K_DESTRAT            ! Destratification conductivity [W/m/C]
      REAL VOL                  ! Tank volume [m3]
      REAL U                    ! Tank heat loss coefficient [W/m2/C]


C---- Set error flag to false
      ERROR_FLAG = .false.

C---- Trace output

      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
        WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
        WRITE(ITU,*) ' 2 node (ISV=20) stratified storage tank model'
        NITMS=13
        WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
        call epagew
      END IF ! Matches to IF(ITC.GT.0.AND.ITRACE(35).NE.0)

C---- Check value of parameters

      VOL   = ADATA(IPCOMP,1)     ! Tank volume
      HTANK = ADATA(IPCOMP,2)     ! Tank height
      HIN1  = ADATA(IPCOMP,4)     ! Height of inlet 1
      HOUT1 = ADATA(IPCOMP,5)     ! Height of outlet 1
      HIN2  = ADATA(IPCOMP,6)     ! Height of inlet 2
      HOUT2 = ADATA(IPCOMP,7)     ! Height of outlet 2
      U     = ADATA(IPCOMP,8)     ! Tank heat loss coefficient
      K_DESTRAT = ADATA(IPCOMP,9) ! Additional destratification coefficient
      NNODES = INT(ADATA(IPCOMP,10))   ! Number of nodes
C---- Check that number of nodes is within acceptable range
      IF (NNodes .GT. MAX_NNODES .OR. NNodes .LT. 1) THEN
        WRITE(IUOUT,*) ' stratified_tank_static: Number of nodes'
        WRITE(IUOUT,*) ' not within range 1 - ', MAX_NNODES
        STOP ' stratified_tank_static: unresolvable error'
      endif
C---- Check that parameters are properly defined
      IF (VOL .LE. 0.) THEN
        WRITE(IUOUT,*) ' stratified_tank_static: Tank volume'
        WRITE(IUOUT,*) ' is less than or equal to 0'
        STOP ' stratified_tank_static: unresolvable error'
      endif
      IF (HTANK .LE. 0.1) THEN
        WRITE(IUOUT,*) ' stratified_tank_static: Tank height'
        WRITE(IUOUT,*) ' is less than 0.1 m'
        STOP ' stratified_tank_static: unresolvable error'
      endif
      IF (U .LT. 0.) THEN
        WRITE(IUOUT,*) ' stratified_tank_static: Tank heat loss'
        WRITE(IUOUT,*) ' coefficient is negative'
        STOP ' stratified_tank_static: unresolvable error'
      endif
      IF (K_DESTRAT .LT. 0.) THEN
        WRITE(IUOUT,*) ' stratified_tank_static: destratification'
        WRITE(IUOUT,*) ' conductivity is negative'
        STOP ' stratified_tank_static: unresolvable error'
      endif
C---- Check that heights of flow inlets and outlets are within tank height
      IF (HIN1.GT.HTANK .OR. HOUT1.GT.HTANK .OR.
     &    HIN2.GT.HTANK .OR. HOUT2.GT.HTANK) THEN
        WRITE(IUOUT,*) ' stratified_tank_static: Height of inlet'
        WRITE(IUOUT,*) ' or outlet greater than tank height'
        STOP ' stratified_tank_static: unresolvable error'
      ENDIF

C---- Check that containment exists

      CALL ECLOSE(PCNTMF(IPCOMP),-99.00,0.001,CLOSEA)
      IF(CLOSEA) THEN
        WRITE(IUOUT,*) ' stratified_tank_static : A containment '
        WRITE(IUOUT,*) ' must be specified for component ',IPCOMP
        WRITE(IUOUT,*) ' and all components of the same type'
        STOP ' stratified_tank_static: unresolvable error'
      ENDIF ! Matches to IF(CLOSEA) THEN

C---- Check user specified number of controlled variables

      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM) THEN
        WRITE(ITU,*) ' stratified_tank_static warning: user ',
     &               ' specified wrong number of controlled ',
     &               ' variables '
        STOP ' stratified_tank_static: unresolvable error'
      ENDIF

C---- Check component has 2 connections only, to water

      NCONS=2
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,IPCONC,1)
      IF(IPCONC.LE.NCONS) THEN
        IF(IPCON.EQ.0) THEN
          ERROR_FLAG = .TRUE.
        ELSE IF(ICONTP(IPCON).NE.20) THEN
          ERROR_FLAG = .TRUE.
        END IF
      ELSE IF(IPCON.NE.0) THEN
        ERROR_FLAG = .TRUE.
      END IF
   10 CONTINUE
      IF (ERROR_FLAG) THEN
        WRITE(IUOUT,*) ' stratified_tank_static: '
        WRITE(IUOUT,*) ' connection error for component ',IPCOMP
        WRITE(IUOUT,*) '   should be ',NCONS,' water connection(s)'
        STOP ' stratified_tank_static: unresolvable error'
      ENDIF

C---- Component is well defined. Store IPCOMP in stratified tank common
C---- block, then set the node temperatures to their initial value

      N_STANK = N_STANK+1
      IF (N_STANK .GT. MAX_STANK) THEN
        WRITE(IUOUT,*) ' stratified_tank_static: '
        WRITE(IUOUT,*) ' too many stratified tanks defined'
        WRITE(IUOUT,*) ' maximum should be ',MAX_STANK
        STOP ' stratified_tank_static: unresolvable error'
      ENDIF
      IPCOMP_STANK(N_STANK) = IPCOMP
      NNODES = INT(ADATA(IPCOMP,10))
      IF (NNODES .LT. 1) NNODES=1
      TINIT = ADATA(IPCOMP,12)
      DO 20 J=1,NNODES,1
        TP_STANK(J,N_STANK) = TINIT
        TF_STANK(J,N_STANK) = TINIT
   20 CONTINUE
      TAVGP_STANK(N_STANK) = TINIT
      TAVGF_STANK(N_STANK) = TINIT

C---- Normal return
 
      RETURN    
      END

C===================== stratified_tank_coeff_gen =======================
C     Created by: Didier Thevenard
C     Created on: April 2007
C     Copyright:  CETC 2007
C-----------------------------------------------------------------------
C     ABSTRACT:
C     This is the ESP-r coefficient generator for the stratified tank.
C     The model is based on an input/output approach, i.e. it calculates
C     temperatures at the two outlet nodes given temperatures and 
C     flowrates at the two inlet nodes. 
C
C References:

C See model description in separate document, DEVELOPMENT OF A STRATIFIED
C TANK MODEL IN ESP-R: MODEL SUMMARY, by Didier Thevenard (April 2007)
C Available from CANMET Energy Technology Centre, Natural Resources 
C Canada, Government of Canada

C=======================================================================

      SUBROUTINE stratified_tank_coeff_gen(IPCOMP,COUT,ISTATS)

C-----------------------------------------------------------------------
C     Declarations
C-----------------------------------------------------------------------

C---- Set implicit to none to force declaration of all variables

      IMPLICIT NONE

C---- Include statements

      INCLUDE "building.h"
      INCLUDE "plant.h"

C---- Arguments

      INTEGER  IPCOMP,ISTATS
      REAL COUT(MPCOE)

C---- ESP-r Common blocks

      COMMON/TC/ITC,ICNT
      INTEGER ITC,ICNT

      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      INTEGER ITCF,ITRACE,IZNTRC,ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      INTEGER IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      INTEGER NPCON,IPC1,IPN1,IPCT,IPC2,IPN2
      REAL PCONDR,PCONSD

      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      INTEGER NPCDAT,IPOFS1,IPOFS2

      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      REAL CSVF,CSVP

      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      REAL CONVAR
      INTEGER ICONTP,ICONDX
      
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)
      INTEGER MAXITP,ITRCLP,ICSV
      REAL PERREL,PERTMP,PERFLX,PERMFL,CSVI


C---- Local variables

      INTEGER ICON1           ! Pointer to interconnection 1
      INTEGER INOD1           ! Pointer to node 1
      INTEGER ICON2           ! Pointer to interconnection 2
      INTEGER INOD2           ! Pointer to node 2
      REAL TOUT1              ! Outlet temperature 1 [C]
      REAL TOUT2              ! Outlet temperature 2 [C]

C---- Trace output

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine ',
     &   ' stratified_tank_coeff_gen '

C---- Initialize pointers to inter-connection(s) ICON, and node(s) INOD.

      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,2,1)
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1

C=======================================================================
C     Generate coefficients for energy balance equation
C=======================================================================

      IF(ISTATS.EQ.1) THEN

C---- Mark temperature of nodes for iteration

        ICSV(INOD1,1)=1
        CSVI(INOD1,1)=CSVF(INOD1,1)
        ICSV(INOD2,1)=1
        CSVI(INOD2,1)=CSVF(INOD2,1)

C---- Call wrapper of TRNSYS-like function to calculate temperature of tank outlets
      
        CALL stratified_tank_wrapper(IPCOMP,TOUT1,TOUT2)

C Establish matrix equation self-coupling coefficients,
         COUT(1)=1.
         COUT(2)=1.
C then matrix equation cross-coupling coefficients,
         COUT(3)=0.
         COUT(4)=0.
C and then present-time coefficients (ie. right hand sides)
         COUT(5)=TOUT1
         COUT(6)=TOUT2

C=======================================================================
C     Generate coefficients for 1st phase flow equation.
C=======================================================================

      ELSEIF(ISTATS.EQ.2) THEN

         COUT(1)=1.
         COUT(2)=1.
         COUT(3)=-PCONDR(ICON1)
         COUT(4)=-PCONDR(ICON2)
         COUT(5)=0.
         COUT(6)=0.

C=======================================================================
C     Generate coefficients for 2nd phase flow equation.
C=======================================================================

      ELSEIF(ISTATS.EQ.3) THEN

         COUT(1)=1.
         COUT(2)=1.
         COUT(3)=-PCONDR(ICON1)
         COUT(4)=-PCONDR(ICON2)
         COUT(5)=0.
         COUT(6)=0.

      ENDIF 

C-----------------------------------------------------------------------
C     End of calculation
C-----------------------------------------------------------------------

C---- Trace output

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
        WRITE(ITU,*) ' Component     ',IPCOMP,':'
        WRITE(ITU,*) ' 2 node (ISV=20) stratified tank '
        WRITE(ITU,*) ' Matrix node(s) ',INOD1,', ',INOD2
        WRITE(ITU,*) ' Connection(s)  ',ICON1,', ',ICON2
        IF(ISTATS.EQ.1) THEN
          WRITE(ITU,*) ' DAY        =', IDYF
          WRITE(ITU,*) ' HOUR       =', IHRF
          WRITE(ITU,*) ' '
        ENDIF ! Matches IF(ISTATS.EQ.1)
        WRITE(ITU,*) 'Exiting subroutine stratified_tank_coeff_gen'
      ENDIF ! Matches IF(ITC.GT.0.AND.NSINC.GE.ITC ...

C---- Return to the calling module

      RETURN

      END

C===================== stratified_tank_wrapper =========================
C     Created by: Didier Thevenard
C     Created on: April 2007
C     Copyright:  CETC 2007
C-----------------------------------------------------------------------
C     ABSTRACT:
C     This is a wrapper around the subroutine that calculates
C     the temperatures in the stratified tank. The wrapper is called 
C     by the ESP-r coefficient generator
C
C=======================================================================
      SUBROUTINE stratified_tank_wrapper(IPCOMP,TOUT1,TOUT2)

C---- Set implicit to none to force declaration of all variables

      IMPLICIT NONE

C---- Include statements

      INCLUDE "building.h"
      INCLUDE "plant.h"

C---- ESP-r Common blocks

      COMMON/PTIME/PTIMEP,PTIMEF
      REAL PTIMEP,PTIMEF

      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     & IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      INTEGER NPCON,IPC1,IPN1,IPCT,IPC2,IPN2
      REAL PCONDR,PCONSD

      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),NAPDAT(MPCOM)
      REAL QDATA,PCAOUT
      INTEGER NAPDAT

      COMMON/TC/ITC,ICNT
      INTEGER ITC,ICNT

      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      REAL CONVAR
      INTEGER ICONTP,ICONDX

      COMMON/PCTIME/TIMSEC
      REAL TIMSEC

      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),
     & PCQF(MPNODE),PCNTMF(MPCOM),PCTP(MPCON),PCRP(MPCON),
     & PUAP(MPNODE),PCQP(MPNODE),PCNTMP(MPCOM)
      REAL PCTF,PCRF,PUAF,PCQF,PCNTMF,PCTP,PCRP,PUAP
      REAL PCQP,PCNTMP

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      INTEGER IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      INTEGER ITCF,ITRACE,IZNTRC,ITU

      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      REAL ADATA,BDATA

      COMMON/PCNAM/PCNAME(MPCOM)       ! PLANT COMPONENT NAMES
      CHARACTER*15 PCNAME


      INCLUDE "stratified_tank.h"

C---- Arguments

      INTEGER  IPCOMP           ! Component number
      REAL TOUT1                ! Outlet temperature 1 [C]
      REAL TOUT2                ! Outlet temperature 2 [C]

C---- Local variables

      LOGICAL CLOSEA,CLOSEB,CLOSEC,CLOSED
                                ! Auxiliary variables 
      REAL HIN1                 ! Height of inlet 1 [m]
      REAL HOUT1                ! Height of outlet 1 [m]
      REAL HIN2                 ! Height of inlet 2 [m]
      REAL HOUT2                ! Height of outlet 2 [m]
      REAL MDOT1                ! Flow rate at inlet 1 [kg/s]
      REAL MDOT2                ! Flow rate at inlet 2 [kg/s]
      REAL TIN1                 ! Temperature at inlet 1 [C]
      REAL TIN2                 ! Temperature at inlet 2 [C]
      REAL K                    ! Thermal conductivity of fluid [W/m/C]
      REAL K_DESTRAT            ! Destratification conductivity [W/m/C]
      REAL VOL                  ! Tank volume [m3]
      REAL CP                   ! Heat capacitance of fluid [J/kg/C]
      REAL RHO                  ! Density of fluid in tank [kg/m3]
      REAL TCONT                ! Containment temperature [C]
      REAL H                    ! Tank height [m]
      REAL P                    ! Actual tank perimeter [m]
      REAL U                    ! Tank heat loss coefficient [W/m2/C]
      REAL TBOIL                ! Boiling temperature of tank fluid [C]
      INTEGER NNODES            ! Number of nodes
      INTEGER NSTEPS            ! Number of internal time steps per simulation
                                ! time step
      INTEGER I_STANK           ! Number of stratified tank in common
                                ! stratified_tank
      INTEGER ICON1             ! Pointer to inter-connection 1
      INTEGER ICON2             ! Pointer to inter-connection 2
      INTEGER I,J               ! Counters

C     Outputs of model
      REAL QLOSS                ! Heat loss from tank to environment [J]
      REAL QIMMERSED_HX         ! Heat gain from immersed heat exchangers [J]
      INTEGER STEPCOUNT         ! Number of internal time steps
      INTEGER MIXCOUNT          ! Number of times mixing algorithm called
      INTEGER NSECAVG           ! Average number of mixed sections
      INTEGER IER               ! Error code

C     Dummy variables
      LOGICAL HASHX1            ! Indicates presence of HX1 (.FALSE.)
      LOGICAL HASHX2            ! Indicates presence of HX2 (.FALSE.)
      REAL DUMMY                ! Dummy variable

C---- Functions
      REAL SHTFLD
      REAL RHOFLD
      REAL KWATER

C---- Trace output

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine ',
     &   ' stratified_tank_wrapper '

C=======================================================================
C---- Initialise component parameters from the database information

      VOL   = ADATA(IPCOMP,1)     ! Tank volume
      H     = ADATA(IPCOMP,2)     ! Tank height
      P     = ADATA(IPCOMP,3)     ! Tank perimeter as entered by user
      HIN1  = ADATA(IPCOMP,4)     ! Height of inlet 1
      HOUT1 = ADATA(IPCOMP,5)     ! Height of outlet 1
      HIN2  = ADATA(IPCOMP,6)     ! Height of inlet 2
      HOUT2 = ADATA(IPCOMP,7)     ! Height of outlet 2
      U     = ADATA(IPCOMP,8)     ! Tank heat loss coefficient
      K_DESTRAT = ADATA(IPCOMP,9) ! Additional destratification coefficient
      NNODES = INT(ADATA(IPCOMP,10))   
                                  ! Number of nodes
      NSTEPS = INT(ADATA(IPCOMP,11))   
                                  ! Number of internal time steps per sim. t.s.
      TBOIL  = ADATA(IPCOMP,13)   ! Boiling temperature of tank fluid 

C---- Initialize pointers to inter-connection(s) ICON, and node(s) INOD.

      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,2,1)

C---- Identify variables

      TIN1=CONVAR(ICON1,1)
      TIN2=CONVAR(ICON2,1)
      MDOT1=PCONDR(ICON1)*CONVAR(ICON1,2)
      MDOT2=PCONDR(ICON2)*CONVAR(ICON2,2)

C---- Check that tank fluid does not have glycol
C     Enabling the tank to be filled with glycol would not be difficult to implement, 
C     however this is not done for now as such systems are probably very infrequent
      CALL ECLOSE(CONVAR(ICON1,3),0.0,1.0e-15,CLOSEC)
      CALL ECLOSE(CONVAR(ICON2,3),0.0,1.0e-15,CLOSED)
      IF ((.NOT.CLOSEC) .OR. (.NOT.CLOSED)) THEN
        WRITE(ITU,*) 'Error in stratified tank model'
        WRITE(ITU,*) 'Tank can be filled with water only - no glycol'
        WRITE(ITU,*) 'Aborting simulation'
        STOP
      ENDIF

C---- Calculate containment temperature. If no containment, set U to zero

      TCONT = PCNTMF(IPCOMP)
      CALL ECLOSE(TCONT,-99.0,0.001,CLOSEA)
      IF(closea) U=0.

C---- Compare current simulation time to time stored in common stratified_tank.
C     If they are not equal, the simulation has advanced and future tank
C     temperatures have to be transfered to past tank temperatures

      CALL ECLOSE(PTIMEF_STANK,PTIMEF,1.0e-6,CLOSEB)
      IF (.NOT.CLOSEB) THEN
        DO 5 J=1,N_STANK
          DO 7 I=1,MAX_NNODES
            TP_STANK(I,J)=TF_STANK(I,J)
    7     CONTINUE
          TAVGP_STANK(J)=TAVGF_STANK(J)
    5   CONTINUE
        PTIMEF_STANK = PTIMEF          ! Update tank time
      ENDIF

C---- Identify number of stratified tank in common stratified_tank.h
    
      I_STANK = 0
      DO 10 I=1,N_STANK
        IF (IPCOMP .EQ. IPCOMP_STANK(I)) THEN
          I_STANK = I
          GOTO 20
        ENDIF
   10 CONTINUE
      WRITE(ITU,*) 'Stratified tank information not found'
      WRITE(ITU,*) 'for component ', IPCOMP
      WRITE(ITU,*) 'Aborting simulation'
      STOP
   20 CONTINUE

C---- Calculate heat capacitance and density of fluid
C     Use average temperature of tank at previous time step
      CP=SHTFLD(3,TAVGP_STANK(I_STANK))
      RHO=RHOFLD(3,TAVGP_STANK(I_STANK))
      K=KWATER(TAVGP_STANK(I_STANK))

C---- Call stratified tank function
      HASHX1=.FALSE.
      HASHX2=.FALSE.
      DUMMY=0.
      CALL stratified_tank_calc(VOL, H, P, 
     &  HIN1, HIN2, HOUT1, HOUT2, U, K_DESTRAT, NSTEPS, NNODES,
     &  HASHX1, 
     &  DUMMY, DUMMY, DUMMY, DUMMY, DUMMY, DUMMY, DUMMY,
     &  HASHX2, 
     &  DUMMY, DUMMY, DUMMY, DUMMY, DUMMY, DUMMY, DUMMY,
     &  TIMSEC, TIN1, TIN2, MDOT1, MDOT2, TCONT, CP, K,
     &  RHO, TBOIL, TP_STANK(1,I_STANK),
     &  DUMMY, DUMMY, DUMMY, 
     &  DUMMY, DUMMY, DUMMY, 
     &  TOUT1, TOUT2, TAVGF_STANK(I_STANK), DUMMY, DUMMY, 
     &  QLOSS, QIMMERSED_HX,
     &  STEPCOUNT, MIXCOUNT, NSECAVG, 
     &  TF_STANK(1,I_STANK),IER)

C---- Check error code

      IF (IER .NE. 0) THEN
        WRITE(ITU,*) 'Internal error in stratified tank model'
        WRITE(ITU,*) 'Error code: IER = ', IER
        WRITE(ITU,*) 'Aborting simulation'
        STOP
      ENDIF
     
C---- Additional outputs

      NAPDAT(IPCOMP) = 5
      PCAOUT(IPCOMP,1) = TAVGF_STANK(I_STANK)
      PCAOUT(IPCOMP,2) = QLOSS
      PCAOUT(IPCOMP,3) = STEPCOUNT
      PCAOUT(IPCOMP,4) = MIXCOUNT
      PCAOUT(IPCOMP,5) = NSECAVG

C---- XML output

      call add_to_xml_reporting(
     &   TAVGF_STANK(I_STANK),
     &   'plant/' // pcname(ipcomp)(1:iPltNameLen(ipcomp))
     &    //'/misc_data/average_temperature',
     &   'units','(C)',
     &   'Stratified tank : Average temperature')

C---- Trace output

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Exiting subroutine ',
     &   ' stratified_tank_wrapper '

C---- Return to the calling module

      RETURN
      END

C***********************************************************************
C***********************************************************************
C
C     FUNCTIONS COMMON TO ALL THREE STRATIFIED TANK MODELS
C
C***********************************************************************
C***********************************************************************

C===================== KWATER ==========================================
C     Created by: Didier Thevenard
C     Created on: April 2007
C     Copyright:  CETC 2007
C-----------------------------------------------------------------------
C     ABSTRACT:
C     This function calculates the conductivity of water in the range
C     0 to 140 �C
C     
C     REFERENCE
C
C     Fitted from data in:
C     Incropera & DeWitt, Fundamentals of Heat and Mass Transfer, 3rd ed.
C     p. A22.
C
C     INPUT
C     T         REAL           temperature, �C
C
C     OUTPUT
C     KWATER    REAL           thermal conductivity of water, W/m/K
C=======================================================================

      FUNCTION KWATER(T)
      
C-----------------------------------------------------------------------
C     Declarations
C-----------------------------------------------------------------------

C---- Set implicit to none to force declaration of all variables

      IMPLICIT NONE

C---- Arguments

      REAL T
      REAL KWATER
      
C---- Local variables

      REAL TK                  ! temperature in Kelvins

C-----------------------------------------------------------------------
C     Calculation of thermal conductivity
C     Fit was derived in range 273.15-430 K so temperature is limited
C     to that range
C-----------------------------------------------------------------------

      TK = T + 273.15
      IF (TK .LT. 273.15) TK = 273.15
      IF (TK .GT. 430.0)  TK = 430.0
      KWATER=(-0.0068*TK*TK + 5.51*TK - 427)/1000.
     
C-----------------------------------------------------------------------
C     Normal end of function
C-----------------------------------------------------------------------

      RETURN
      END
     
C===================== GetArrayAverage =================================
C     Created by: Didier Thevenard
C     Created on: April 2007
C     Copyright:  CETC 2007
C-----------------------------------------------------------------------
C     ABSTRACT:
C     This function calculates the average of an array between two 
C     indices
C
C     INPUT
C     x         REAL           array to average
C     imin      INTEGER        bottom index to average
C     imax      INTEGER        top index to average
C
C     OUTPUT
C     GetArrayAverage   REAL   average of array between imin and imax
C=======================================================================

      FUNCTION GetArrayAverage(x,imin,imax)
      
C-----------------------------------------------------------------------
C     Declarations
C-----------------------------------------------------------------------

C---- Set implicit to none to force declaration of all variables

      IMPLICIT NONE

C---- Arguments

      REAL x(*)
      INTEGER imin
      INTEGER imax
      REAL GetArrayAverage
      
C---- Local variables

      REAL xavg
      INTEGER i

C-----------------------------------------------------------------------
C     Calculation of average
C-----------------------------------------------------------------------

      xavg = 0.
      DO 10 i=imin,imax,1
        xavg = xavg + x(i)
   10 CONTINUE     
      IF (imax .GE. imin)
     &  xavg = xavg / (imax - imin + 1)
     
      GetArrayAverage = xavg
      
C-----------------------------------------------------------------------
C     Normal end of function
C-----------------------------------------------------------------------

      RETURN
      END
     
C===================== Bound ===========================================
C     Created by: Didier Thevenard
C     Created on: April 2007
C     Copyright:  CETC 2007
C-----------------------------------------------------------------------
C     ABSTRACT:
C     This function bounds a value by a minimum and a maximum
C
C     INPUT
C     x         INTEGER        value to bound  
C     xmin      INTEGER        lower bound
C     xmax      INTEGER        upper bound
C
C     OUTPUT
C     Bound     INTEGER        value bound by xmin and xmax
C=======================================================================

      FUNCTION Bound(x,xmin,xmax)
      
C-----------------------------------------------------------------------
C     Declarations
C-----------------------------------------------------------------------

C---- Set implicit to none to force declaration of all variables

      IMPLICIT NONE

C---- Arguments

      INTEGER x
      INTEGER xmin
      INTEGER xmax
      INTEGER Bound
      
C-----------------------------------------------------------------------
C     Body
C-----------------------------------------------------------------------

      if (xmin .GT. xmax) then
        Bound = nint((xmin+xmax)/2.)
      else if (x .LT. xmin) then
        Bound = xmin
      else if (x .GT. xmax) then
        Bound = xmax
      else
        Bound = x
      endif

C-----------------------------------------------------------------------
C     Normal end of function
C-----------------------------------------------------------------------

      RETURN
      END

C===================== SolveTridiag ====================================
C     Created by: Didier Thevenard
C     Created on: April 2007
C     Copyright:  CETC 2007
C-----------------------------------------------------------------------
C     ABSTRACT:
C     Solving a tri-diagonal system
C     The function returns 0 if successful, and 1+row number where problem occured
C     if it failed, or -1 is incorrect size
C     Notes:
C     1. All matrices are destroyed in the process
C     2. SubDiag[i] contains element[i][i-1] of the matrix
C        Diag[i]    contains element[i][i]   of the matrix
C        SupDiag[i] contains element[i][i+1] of the matrix
C
C=======================================================================
   
      SUBROUTINE SolveTridiag(Size,SubDiag,Diag,SupDiag,V,IER)
      
C-----------------------------------------------------------------------
C     Declarations
C-----------------------------------------------------------------------

C---- Set implicit to none to force declaration of all variables

      IMPLICIT NONE

C---- Arguments

      INTEGER Size                ! size of matrix 
      REAL SubDiag(*)             ! sub-diagonal of matrix 
      REAL Diag(*)                ! diagonal of matrix 
      REAL SupDiag(*)             ! super-diagonal of matrix 
      REAL V(*)                   ! input: right-hand side; output: solution 
      INTEGER IER                 ! error code (0 = all OK)

C---- Local variables

      INTEGER row
      REAL TOL
      PARAMETER (TOL = 1.e-10)

C---- Initialize error code

      IER = 0

C---- Check arguments 

      if (Size .LE. 0) then
        IER = -1
        return
      endif

      
C-----------------------------------------------------------------------
C---- Forward substitution 
C---- This eliminates the sub-diagonal
C---- During the forward substitution, diagonal elements are normalized to 1. 
C---- This prevents problems of coefficients becoming extremely large even 
C---- in well-conditioned systems 
C---- As a consequence, diagonal elements from previous rows do not appear 
C---- in formulae, since they are equal to 1 
C-----------------------------------------------------------------------

C---- Take care of first row. After this, Diag(1) is assumed to be 1 
      if (abs(Diag(1)) .LT. TOL) then
        IER = 1
        return
      endif
      V(1) = V(1) / Diag(1)
      if (Size .eq. 1) return     ! trivial case (one row)
      SupDiag(1) = SupDiag(1) / Diag(1)

C---- Loop on rows

      do 110 row=2,Size,1

C---- Eliminate. The complete formulae are:
C       Diag(row) = Diag(row)*Diag(row-1)-SubDiag(row)*SupDiag(row-1)
C       SupDiag(row) = SupDiag(row) * Diag(row-1)
C       V(row) = V(row)*Diag(row-1)-SubDiag(row)*V(row-1) 
C     The formulae get simplified because the diagonal up to the previous
C     step is assumed to be normalized to 1

        Diag(row) = Diag(row) - SubDiag(row)*SupDiag(row-1)
        V(row) = V(row) - SubDiag(row)*V(row-1)

C---- Normalize diagonal to 1 
C---- After this, Diag(row) is assumed to be 1 

        if (abs(Diag(row)) < TOL) then
          IER = row
          return
        endif
        SupDiag(row) = SupDiag(row) / Diag(row)
        V(row) = V(row) / Diag(row)

  110 continue

C-----------------------------------------------------------------------
C---- Backward substitution 
C---- The complete formulae used are:
C----   V(Size) /= Diag(Size)
C----   V(row) = (V(row)-SupDiag(row)*V(row+1))/Diag(row)
C---- but they get simplified since the diagonal is 1 
C-----------------------------------------------------------------------

      do 120 row=Size-1,1,-1
        V(row) = V(row) - SupDiag(row)*V(row+1)
  120 continue

C-----------------------------------------------------------------------
C     Normal end of function
C-----------------------------------------------------------------------

      continue

      return
      end

C===================== stratified_tank_calc ============================
C     Created by: Didier Thevenard
C     Created on: March 2007
C     Copyright:  CETC 2007
C-----------------------------------------------------------------------
C     ABSTRACT:
C     This function calculates the temperature profile and the outlet
C     temperature of a stratified tank
C
C=======================================================================
   
      SUBROUTINE stratified_tank_calc(Volume, Height, 
     &  DefPerimeter,HIn0, HIn1, HOut0, HOut1, U, k_destrat, NSteps, 
     &  NNodes,HasHX0, 
     &  HInHX0, HOutHX0, DinHX0, DoutHX0, DcoilHX0, PitchHX0, kHX0, 
     &  HasHX1, 
     &  HInHX1, HOutHX1, DinHX1, DoutHX1, DcoilHX1, PitchHX1, kHX1, 
     &  dt, TIn0, TIn1, Mdot0, Mdot1, Text, Cp, k, Rho, TBoil, TPast,
     &  TInHX0, MdotHX0, GlycolFracHX0,
     &  TInHX1, MdotHX1, GlycolFracHX1,
     &  TOut0, TOut1, TAvg, TOutHX0, TOutHX1, QLoss, QImmersedHX,
     &  StepCount, MixCount, NSecAvg, T, IER, 
     &  HPCM_MOD,TPCM_MOD,
     &  Axb,Ax,Ay,V,
     &  nX,nY,NPCMbot,NPCMtop,dr,
     &  PCMrhos,PCMrhol,cs,cl,heatLat,cndcts,cndctl,Tmi,method,e,
     &  HPCMcyl,DPCMcyl,HPCMbot,HPCMtop, NPCM, HASPCM,DPCMnode)
C-----------------------------------------------------------------------
C     Declarations
C-----------------------------------------------------------------------

C---- Set implicit to none to force declaration of all variables

      IMPLICIT NONE

C---- Parameters
      
C     MAXNNODES is the maximum number of nodes in a stratified tank
      INTEGER MAX_NNODES
      PARAMETER(MAX_NNODES=100)
C     PI is the number pi
      REAL PI
      PARAMETER (PI=3.1415926535897932385)


C=====AGGIUNTO DA ROBY=========================================================================================
      INTEGER mX,mY

C      INTEGER MCELLS
C      PARAMETER(MCELLS=2000)
C      INTEGER MAX_STANK
C      PARAMETER (MAX_STANK=5)
C      INTEGER mX
      PARAMETER (mX=100)
C      INTEGER mY
      PARAMETER (mY=100)
C=====FINE AGGIUNTO DA ROBY====================================================================================


C---- Arguments

C     Input: tank parameters

      REAL    Volume          ! tank volume (m3)
      REAL    Height          ! tank height (m)
      REAL    DefPerimeter    ! tank perimeter (m; <0 if cylindrical)
      REAL    HIn0            ! height of inlet 0 (m)
      REAL    HIn1            ! height of inlet 1 (m)
      REAL    HOut0           ! height of outlet 0 (m)
      REAL    HOut1           ! height of outlet 1 (m)
      REAL    U               ! tank heat loss coefficient (W/m2/C)
      REAL    k_destrat       ! destratification conductivity (W/m/K)
      INTEGER NSteps          ! number of internal time steps per 
                              ! simulation time step
      INTEGER NNodes          ! number of nodes

C     Input: parameters of first immersed HX (HX0)

      LOGICAL HasHX0          ! .TRUE. if first immersed heat exchanger (HX0) present
      REAL    HInHX0          ! height of HX0 inlet (m)
      REAL    HOutHX0         ! height of HX0 outlet (m)
      REAL    DinHX0          ! inside diameter of HX0 pipe (m)
      REAL    DoutHX0         ! outside diameter of HX0 pipe (m)
      REAL    DcoilHX0        ! diameter of HX0 coil (m)
      REAL    PitchHX0        ! pitch of HX0 coil (vertical distance from one loop to the next) (m)
      REAL    kHX0            ! thermal conductivity of coil material (W/m/K)
       
C     Input: parameters of second immersed HX (HX1)

      LOGICAL HasHX1          ! .TRUE. if second immersed heat exchanger (HX1) present
      REAL    HInHX1          ! height of HX1 inlet (m)
      REAL    HOutHX1         ! height of HX1 outlet (m)
      REAL    DinHX1          ! inside diameter of HX1 pipe (m)
      REAL    DoutHX1         ! outside diameter of HX1 pipe (m)
      REAL    DcoilHX1        ! diameter of HX1 coil (m)
      REAL    PitchHX1        ! pitch of HX1 coil (vertical distance from one loop to the next) (m)
      REAL    kHX1            ! thermal conductivity of coil material (W/m/K)
       
C     Input: tank variables

      REAL    dt              ! simulation time step (s)
      REAL    TIn0            ! inlet temperature 0 (C)
      REAL    TIn1            ! inlet temperature 1 (C)
      REAL    Mdot0           ! flow rate 0 (kg/s)
      REAL    Mdot1           ! flow rate 1 (kg/s)
      REAL    Text            ! external temperature (C)
      REAL    Cp              ! heat capacitance of fluid (J/kg/C)
      REAL    k               ! heat conductivity of fluid (W/m/C)
      REAL    Rho             ! fluid density (kg/m3)
      REAL    TBoil           ! boiling temperature of fluid (C)
      REAL    TPast(MAX_NNODES)! temperature of tank (C) at end of 
                              ! previous time step

C     Input: first immersed heat exchanger (HX0)

      REAL    TInHX0          ! inlet temperature of HX0 (C)
      REAL    MdotHX0         ! flow rate through HX0 (kg/s)
      REAL    GlycolFracHX0   ! percentage of glycol in HX0 (0-100%)
  
C     Input: second immersed heat exchanger (HX1)

      REAL    TInHX1          ! inlet temperature of HX1 (C)
      REAL    MdotHX1         ! flow rate through HX1 (kg/s)
      REAL    GlycolFracHX1   ! percentage of glycol in HX1 (0-100%)
  
C     Output: variables and counters

      REAL    TOut0           ! average outlet temperature 0 over time step (C)
      REAL    TOut1           ! average outlet temperature 1 over time step (C)
      REAL    TAvg            ! average tank temperature over time setp (C)
      REAL    TOutHX0         ! average temperature of HX0 outlet over time step (C)
      REAL    TOutHX1         ! average temperature of HX1 outlet over time step (C)
      REAL    QLoss           ! average heat loss to exterior over time step (W)
      REAL    QImmersedHX     ! average heat gain from immersed HXs, if any (W)
      INTEGER StepCount       ! number of internal integration steps
      INTEGER MixCount        ! number of times mixing algorithm called within 
                              ! time step
      INTEGER NSecAvg         ! average number of mixed sections
      REAL    T(MAX_NNODES)   ! temperature of tank (C) at end of time step
      INTEGER IER             ! error code (0 = everything OK)

C---- Local functions

      INTEGER Bound
      REAL GetArrayAverage

C---- Local variables

      INTEGER i,j,m,n          ! node counters
      INTEGER p                ! step counter
      INTEGER LastpHX          ! last step counter used for immersed HX calculation
      REAL Perimeter           ! actual tank perimeter (m)
      REAL CrossArea           ! cross-sectional area of tank (m2)
      INTEGER NSec             ! number of sections for current time step
      INTEGER NIn0             ! number of node corresponding to inlet 0
      INTEGER NIn1             ! number of node corresponding to inlet 1
      INTEGER NOut0            ! number of node corresponding to outlet 0
      INTEGER NOut1            ! number of node corresponding to outlet 1
      REAL NodeHeight          ! distance between nodes
      REAL Mdot(MAX_NNODES)    ! flow rate at each node (value i is from node i 
                               ! to node i+1)
      REAL SubDiag(MAX_NNODES) ! sub-diagonal terms fo balance equation
      REAL Diag(MAX_NNODES)    ! diagonal terms fo balance equation
      REAL SupDiag(MAX_NNODES) ! super-diagonal terms fo balance equation
      REAL Tstep(MAX_NNODES)   ! temperature of nodes at end of internal 
                               ! time step
      REAL Tstep_prev(MAX_NNODES) ! temperature of nodes at beginning of internal 
                                  ! time step
      REAL Tsec(MAX_NNODES)    ! temperature of sections (C) 
      REAL QHX(MAX_NNODES)     ! energy transferred from immersed HXs (J)
      INTEGER Secbot(MAX_NNODES)  ! bottom node of a mixed section of the tank
      INTEGER Sectop(MAX_NNODES)  ! top node of a mixed section of the tank
      REAL ts                  ! internal time step
      REAL Mdotmax             ! maximum flow rate between nodes
      INTEGER ibot             ! bottom node of section
      INTEGER itop             ! top node of section
      INTEGER NNodesSec        ! number of nodes in section
      REAL Ti                  ! average temperature of section
      REAL Tinv                ! average temperature of inversion
      LOGICAL Mixing           ! indicates whether mixing occurs inside section
      REAL kA_dz               ! auxiliary variable
      REAL MCp_dt              ! auxiliary variable
      REAL UAs                 ! auxiliary variable
      REAL QLoss_step          ! tank heat loss during one internal step
      REAL TOutHX0step         ! outlet temperature of HX0 at end of internal time step
      REAL TOutHX1step         ! outlet temperature of HX0 at end of internal time step
      LOGICAL CLOSEA,CLOSEB,CLOSEC
                               ! auxiliary variables

C     ALPHA is the parameter of the Crank-Nicholson integration scheme
C     0.0 = Euler explicit, order 1, conditionally stable
C     0.5 = Crank-Nicholson, order 2, unconditionally stable
C     1.0 = Euler implicit, order 1, unconditionally stable
      REAL ALPHA
     

C=====AGGIUNTO ROBY================================================================================
      REAL QPCM(MAX_NNODES)       ! exchanged heat between PCM cylinders and water [W]
      LOGICAL HasPCM


      REAL TPCM_MOD(nY,nX) 
      REAL HPCM_MOD(nY,nX) 

      INTEGER I_STANK 

      REAL V(nY,nX)           ! cells volumes [m^3]
      REAL Ax(nY,nX-1)        ! CV faces in radial direction (east-west) [m^2]
      REAL Axb(nY)             ! border face of external CV, in contact with DHW
      REAL Ay(nX)          ! CV faces in axial direction  (north-south) [m^2]

      INTEGER nX           ! number of cells in radial direction [-]
      INTEGER nY           ! number of cells in axial direction [-]
      INTEGER NPCMbot            ! tank node corresponding to the lower cylinder node
                                 ! tank node which the cylinder bottom is located in
      INTEGER NPCMtop            ! tank node corresponding to the higher cylinder node
                                 ! tank node which the cylinder top is located in
      INTEGER NPCM               ! number of PCM modules

      REAL*4 PCMrhos             ! density of solid [kg/m^3]
      REAL*4 PCMrhol             ! density of solid [kg/m^3]
      REAL*4 cs              ! specific heat of solid [J/(kg K)]
      REAL*4 cl              ! specific heat of liquid [J/(kg K)]
      REAL*4 heatLat         ! latent heat [J/kg]
      REAL*4 cndcts          ! conductivity into solid [W/(m K)]
      REAL*4 cndctl          ! conductivity into liquid [W/(m K)]
      REAL*4 Tmi             ! melting temperature  [°C]
      INTEGER method         ! time integration method [-]
      REAL*4 e               ! phase change range [°C]
      REAL*4 HPCMcyl         ! cylinder height [m]
      REAL*4 DPCMcyl         ! PCM cylinders diameter [m]
      REAL*4 HPCMbot         ! Position of the cylinder bottom [m]
      REAL*4 HPCMtop         ! Position of the cylinder top [m]
      REAL*4 dr              ! grid dimension in radial direction
      REAL*4 DPCMnode(nY-1)    ! distance among PCM nodes

! C-CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       REAL TMP_PREV(mY,mX)
! C-CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

C=====AGGIUNTO ROBY================================================================================



C-----------------------------------------------------------------------
C     Initialization
C-----------------------------------------------------------------------

C---- Set outputs and error code to zero

      TOut0 = 0.
      TOut1 = 0.
      TAvg = 0.
      QLoss = 0.
      do 10 i=1,NNodes,1
        T(i) = 0.
   10 continue
      NSecAvg = 0
      IER = 0

C---- Check values of input variables

      if (Cp .LT. 0.) then
        IER = 1
        return
      endif
      if (Rho .LT. 0.) then
        IER = 2
        return
      endif
      if (k .LT. 0.) then
        IER = 3
        return
      endif
      if (NNodes .GT. MAX_NNODES) then
        IER = 4
        return
      endif
      if (NNodes .LT. 1) then
        IER = 4
        return
      endif

C---- Calculate perimeter and cross-sectional area

      if (DefPerimeter .LT. 0)
     &  Perimeter = SQRT(4.*PI*Volume/Height)
      CrossArea = Volume/Height

C---- Calculate default node height

      NodeHeight = Height/NNodes

C---- Calculate node numbers for inlets and outlets

      NIn0  = Bound(NINT(HIn0 /NodeHeight+0.5), 1, NNodes)
      NOut0 = Bound(NINT(HOut0/NodeHeight+0.5), 1, NNodes)
      NIn1  = Bound(NINT(HIn1 /NodeHeight+0.5), 1, NNodes)
      NOut1 = Bound(NINT(HOut1/NodeHeight+0.5), 1, NNodes)

C---- Zero arrays used in calculations

      do 20 i = 1,NNodes,1
        Mdot(i) = 0.
        Tstep(i) = 0.
        Tstep_prev(i) = 0.
        QHX(i) = 0.
        QPCM(i) = 0.
   20 continue

C---- Zero losses to the environment

      QLoss = 0.

C---- Zero energy transferred from immersed HX

      QImmersedHX = 0.

C---- Calculate flow rates at all nodes
C---- Mdot(i) contains the flow from node i to i+1. It is positive 
C---- if going up and negative if going down

      if (NIn0 .LT. NOut0) then   ! inlet lower than outlet
        do 30 i=NIn0,NOut0-1,1
          Mdot(i) = Mdot(i)+Mdot0
   30   continue
      else                        ! inlet higher than outlet
        do 40 i=NOut0,NIn0-1,1
          Mdot(i) = Mdot(i)-Mdot0
   40   continue
      endif
      if (NIn1 .LT. NOut1) then   ! inlet lower than outlet
        do 50 i=NIn1,NOut1-1,1
          Mdot(i) = Mdot(i)+Mdot1
   50   continue
      else                        ! inlet higher than outlet
        do 60 i=NOut1,NIn1-1,1
          Mdot(i) = Mdot(i)-Mdot1
   60   continue
      endif

C---- Calculate internal time step. During an internal time step, the fluid does not move by more than
C---- the distance between two nodes

C----   Find maximum flow rate between nodes
      Mdotmax = 0.
      do 70 i=1,NNodes,1
        Mdotmax = max(abs(Mdot(i)), Mdotmax)
   70 continue
C----   Calculate internal time step
      CALL ECLOSE(Mdotmax,0.0,1.0E-15,CLOSEA)
      if (CLOSEA) then   ! no-flow case
        ts = dt/max(NSteps, 1)
      else
        ts = min(dt, NodeHeight*Rho*CrossArea/Mdotmax)/max(NSteps, 1)
      endif

C----   Round up internal time step so that there is an integral number of time steps
C----   within the system time step
C----   The 0.5 in the formula below is to make sure that the rounding is made upwards
C----   (equivalent to ceil function in C)

      StepCount = NINT(0.499999+dt/ts)
      ts = dt/StepCount

C---- Initialize value of node temperatures

      do 80 i=1,NNodes,1
        Tstep_prev(i) = TPast(i)
   80 continue

C---- Initialize outlet temperatures

      TOut0 = 0.
      TOut1 = 0.

C---- Initialize outlet temperature of immersed HX (if present)

      if (HasHX0) TOutHX0 = 0.
      if (HasHX1) TOutHX1 = 0.

C---- Define the tank as being totally unmixed

      NSec = NNodes
      do 90 i=1,NNodes,1
        Secbot(i) = i
        Sectop(i) = i
        Tsec(i) = Tstep_prev(i)
   90 continue

C---- Zero mixing counter

      MixCount = 0

C---- If boiling occurs, limit inlet temperatures to boiling
C     temperatures and transfer corresponding energy to losses
C---- The actual limiting of TIn0 and TIn1 to TBoil occurs in 
C     the calculation of the right-hand side

      if (TIn0 .GT. TBoil .AND. Mdot0 .GT. 0.) then
        QLoss = QLoss + Mdot0*Cp*(TIn0-TBoil)*dt
      endif
      if (TIn1 .GT. TBoil .AND. Mdot1 .GT. 0.) then
        QLoss = QLoss + Mdot1*Cp*(TIn1-TBoil)*dt
      endif
      if (QLoss .GT. 0.) then
        write(6,*) 'Warning: stratified tank model'
        write(6,*) 'Boiling temperature of fluid exceeded!'
        write(6,*) 'Please check components leading to tank'
      endif

C---- Define the parameter of the Crank-Nicholson integration scheme
C     according to the presence of immersed heat exchangers. If none
C     is present, the full CN scheme (alpha = 0.5) may be used, 
C     otherwise an explicit scheme has to be used (alpha = 0)

      if (HasHX0 .OR. HasHX1) then
        ALPHA = 0.0
      else
        ALPHA = 0.5
      endif

C---- Force alpha to 0.5 if no flow rate through immersed HXs, since
C     in that case the higher order semi-implicit scheme can be used
      CALL ECLOSE(MdotHX0,0.0,1.0E-15,CLOSEB)
      CALL ECLOSE(MdotHX1,0.0,1.0E-15,CLOSEC)
      if ( CLOSEB .AND. CLOSEC ) ALPHA = 0.5

C---- Iteration util time step is covered
C---- p is the step counter, goes from 1 to StepCount
C---- The loop starts here and ends at label 900

      LastpHX = 0

      do 900 p = 1,StepCount,1
  110   continue

C-----------------------------------------------------------------------
C---- CALCULATE CONTRIBUTION FROM IMMERSED HEAT EXCHANGERS AND PCM MODULES  <--- AGGIUNTO DA ROBY
C     The calculation is done only once per internal time step, since it 
C     depends only on the temperatures in the tank at the end of the 
C     previous internal time step
C     When the calculation is done, the tank is considered unmixed. The
C     heat transfer from the immersed HX, QHX, and from the immersed PCM   <--- AGGIUNTO DA ROBY
C     modules, QPCM,is calculated at the node level (not the section level)
C-----------------------------------------------------------------------

      if (p > LastpHX) then
C----     Zero heat transfer array
          do 115 i = 1,NSec,1
            QPCM(i) = 0.
            QHX(i) = 0.
  115     continue
! C===============================================================================CANCELLARE!!!!!!!!!!!!!!!!!
! C---- Convert section temperatures into node temperatures
!         do 117 i=NSec,1,-1
!           do 116 j = Sectop(i),Secbot(i),-1
!             Tstep_prev(j) = Tsec(i)
!   116     continue
!   117   continue
! C===============================================================================CANCELLARE!!!!!!!!!!!!!!!!!


          if (HasHX0) 
     &      call immersed_HX_calc(NodeHeight, CrossArea, 
     &        HInHX0, HOutHX0, DinHX0, DoutHX0, 
     &        DcoilHX0, PitchHX0, kHX0, 
     &        Tstep_prev, Mdot, NNodes,
     &        TInHX0, MdotHX0, GlycolFracHX0, TOutHX0step, QHX)

          if (HasHX1) 
     &      call immersed_HX_calc(NodeHeight, CrossArea, 
     &        HInHX1, HOutHX1, DinHX1, DoutHX1, 
     &        DcoilHX1, PitchHX1, kHX1, 
     &        Tstep_prev, Mdot, NNodes,
     &        TInHX1, MdotHX1, GlycolFracHX1, TOutHX1step, QHX)

! C===============================================================================CANCELLARE!!!!!!!!!!!!!!!!!
!          endif
! C===============================================================================CANCELLARE!!!!!!!!!!!!!!!!!

          if (HasPCM) 
     &      call immersed_PCM(nX,nY,NPCMbot,NPCMtop,dr, !Se cicla da "goto 110" ==> p = LastpHX e questo non viene chiamato
     &        NodeHeight, Tstep_prev(1),  
     &        Mdot(1), NNodes, QPCM(1), ts, 
     &        HPCM_MOD(1,1),TPCM_MOD(1,1),
     &        Axb(1),Ax(1,1),Ay(1),V(1,1),
     &        PCMrhos,PCMrhol,cs,cl,heatLat,cndcts,cndctl,Tmi,
     &        method,e,
     &        HPCMcyl,DPCMcyl,HPCMbot,HPCMtop,
     &        DPCMnode(1),CrossArea)

          LastpHX = p

C===============================================================================SCOMMENTARE!!!!!!!!!!!!!!!!!
      endif
C===============================================================================SCOMMENTARE!!!!!!!!!!!!!!!!!






C-CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! C-----Computes the average heat flux from PCM
!       DO i=1,nY
!           QPCM(i) = 0.
!       ENDDO
C-CANCELLARE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




C-----------------------------------------------------------------------
C---- SECTION TEMPERATURE CALCULATION ALGORITHM
C-----------------------------------------------------------------------
C     At the beginning of this section, Tsec is used for the right-hand 
C     side. After the system is solved, it becomes the temperatures of 
C     the sections
C-----------------------------------------------------------------------

C---- Zero all coefficients

        do 120 i=1,NNodes,1
          SubDiag(i) = 0.
          Diag(i) = 0.
          SupDiag(i) = 0.
          Tsec(i) = 0.
  120   continue       

C---- Loop over sections

        do 130 i=1,NSec,1

C---- Calculate bottom and top node of this section, and number of nodes

          ibot = Secbot(i)
          itop = Sectop(i)
          NNodesSec = itop-ibot+1

C---- Calculate auxiliary quantities

          kA_dz = (k+k_destrat)*CrossArea/NodeHeight 
          MCp_dt = Volume*Rho/NNodes*Cp/ts*NNodesSec
          UAs = U*NodeHeight*Perimeter*NNodesSec
          if (ibot .EQ. 1)      UAs = UAs + U*CrossArea
          if (itop .EQ. NNodes) UAs = UAs + U*CrossArea

C==== FILL LEFT-HAND SIDE

C---- Left hand-side, sub-diagonal element of matrix
C---- i.e. fill coefficient (i-1,i) for current section

          if (ibot .GT. 1) then
            SubDiag(i) = -ALPHA*kA_dz
            if (Mdot(ibot-1) .GT. 0)
     &        SubDiag(i) = SubDiag(i) - ALPHA*Mdot(ibot-1)*Cp
          endif

C---- Left hand side, diagonal element of matrix 
C---- i.e. fill coefficient (i,i) for current section
          Diag(i) = MCp_dt + ALPHA*UAs
C---- Terms corresponding to flows from this section to neighbouring sections
          if (itop .LT. NNodes .AND. Mdot(itop) .GT. 0)
     &      Diag(i) = Diag(i) +  ALPHA*Mdot(itop)*Cp

          if (ibot .GT. 1 ) then
            if ( Mdot(ibot-1) .LT. 0)
     &        Diag(i) = Diag(i) - ALPHA*Mdot(ibot-1)*Cp
          endif 
C---- Terms corresponding to flows from this node to outlets
          if (ibot .LE. NOut0 .AND. NOut0 .LE. itop)
     &      Diag(i) = Diag(i) +  ALPHA*Mdot0*Cp
          if (ibot .LE. NOut1 .AND. NOut1 .LE. itop)
     &      Diag(i) = Diag(i) +  ALPHA*Mdot1*Cp
C---- Terms corresponding to thermal conduction to neighbouring nodes
          if (ibot .GT. 1)
     &      Diag(i) = Diag(i) +  ALPHA*kA_dz
          if (itop .LT. NNodes)
     &      Diag(i) = Diag(i) +  ALPHA*kA_dz

C---- Left hand side, super-diagonal element of matrix
C---- i.e. fill coefficient (i,i+1) for current section
          if (itop .LT. NNodes) then
            SupDiag(i) = -ALPHA*kA_dz
            if (Mdot(itop) .LT. 0)
     &        SupDiag(i) = SupDiag(i)+ALPHA*Mdot(itop)*Cp
          endif

C==== FILL RIGHT-HAND SIDE

C---- Calculate temperature of section at beginning of time step
          Ti = GetArrayAverage(Tstep_prev, ibot, itop)

C------- Terms always present, whatever the integration method
C---- Heat capacitance term 
          Tsec(i) = Tsec(i) + MCp_dt*Ti

C---- Flows to this section from inlets
          if (ibot .LE. NIn0 .AND. NIn0 .LE. itop)
     &      Tsec(i) = Tsec(i) + Mdot0*Cp*AMIN1(TIn0,TBoil)
          if (ibot .LE. NIn1 .AND. NIn1 .LE. itop)
     &      Tsec(i) = Tsec(i) + Mdot1*Cp*AMIN1(TIn1,TBoil)

C------- Terms calculated at beginning of time step
C----    (these terms are eliminated if Euler implicit (ALPHA=1) 
C----    is chosen)

C---- Heat loss term, beginning of time step
          Tsec(i) = Tsec(i) 
     &      + (1.-ALPHA)*U*NodeHeight*Perimeter*NNodesSec*(Text-Ti)
          if (ibot .EQ. 1)              ! bottom of tank
     &      Tsec(i) = Tsec(i) 
     &      + (1.-ALPHA)*U*CrossArea*(Text-Tstep_prev(1))
          if (itop .EQ. NNodes)         ! top of tank
     &      Tsec(i) = Tsec(i) 
     &      + (1.-ALPHA)*U*CrossArea*(Text-Tstep_prev(NNodes))

C---- Transfer from immersed HX
          do 125 m = ibot,itop,1
            Tsec(i) = Tsec(i) + QHX(m)+NPCM*QPCM(m)
  125     continue

C---- Conduction term to lower section, beginning of time step
          if (ibot .GT. 1)
     &      Tsec(i) = Tsec(i) 
     &        + (1.-ALPHA)*kA_dz*(Tstep_prev(ibot-1)-Tstep_prev(ibot))

C---- Conduction term to upper section, beginning of time step
          if (itop .LT. NNodes)
     &      Tsec(i) = Tsec(i) 
     &        + (1.-ALPHA)*kA_dz*(Tstep_prev(itop+1)-Tstep_prev(itop))

C---- Flow from or to lower section, beginning of time step
          if (ibot .GT. 1) then
            if (Mdot(ibot-1) .LT. 0.) then
              Tsec(i) = Tsec(i) 
     &          + (1.-ALPHA)*Mdot(ibot-1)*Cp*Tstep_prev(ibot)
            else
              Tsec(i) = Tsec(i) 
     &          + (1.-ALPHA)*Mdot(ibot-1)*Cp*Tstep_prev(ibot-1)
            endif
          endif

C---- Flow from or to upper section, beginning of time step
          if (itop .LT. NNodes) then
            if (Mdot(itop) .GT. 0.) then
              Tsec(i) = Tsec(i) 
     &          - (1.-ALPHA)*Mdot(itop)*Cp*Tstep_prev(itop)
            else
              Tsec(i) = Tsec(i) 
     &          - (1.-ALPHA)*Mdot(itop)*Cp*Tstep_prev(itop+1)
            endif
          endif

C---- Flows from this section to outlets, beginning of time step
          if (ibot .LE. NOut0 .AND. NOut0 .LE. itop)
     &      Tsec(i) = Tsec(i) - (1.-ALPHA)*Mdot0*Cp*Tstep_prev(NOut0)
          if (ibot .LE. NOut1 .AND. NOut1 .LE. itop)
     &      Tsec(i) = Tsec(i) - (1.-ALPHA)*Mdot1*Cp*Tstep_prev(NOut1)

C------- Terms calculated at end of time step
C----    (these terms are eliminated if Euler explicit (ALPHA=0) 
C----    is chosen)

C---- Heat loss term, end of time step
          Tsec(i) = Tsec(i) + ALPHA*UAs*Text
          
C---- End of loop on sections          
  130   continue
    

C---- Solve system
C---- Note: if Euler explicit is chosen, solution is trivial as all non-diagonal terms
C---- are zero

        CALL SolveTridiag(NSec, SubDiag, Diag, SupDiag, Tsec, IER)
        if (IER .NE. 0) return

C--------------------------------------------------------------------------
C---- MIXING ALGORITHM
C--------------------------------------------------------------------------

C---- Mix nodes in case of inversion, that is, if temperature of section i-1
C---- is higher than temperature of section i
C---- This algorithm works in an iterative way from the bottom up. If an inversion
C---- is detected, it mixes all the sections in the inversion. This takes care of the
C---- inversion locally, but the temperature of the mixed layer may still be higher
C---- than the temperature of the section above it, or lower than the temperature of
C---- the section below it. For that reason the algorithm is iterated until no inversion
C---- is detected

        Mixing = .FALSE.

C---- Loop on sections. The loop exits at label 290
C---- i is the number of the current section
  210   continue
        do 290 i = 1,NSec-1,1
C---- Check if temperature inversion. If found, mix
          if (Tsec(i+1) .LE. Tsec(i)) then

C---- There is an inversion. Record that mixing occurs.
            Mixing = .TRUE.

C---- Progress through the sections above section i until
C---- the end of the inversion is found

            do 230 j=i+1,NSec,1
              if (Tsec(j) .GT. Tsec(j-1)) goto 240
  230       continue
  240       continue

C---- Now j is the number of the first section not part of the inversion
C---- Mix sections i to j-1
C---- Calculate average temperature of inversion
            Tinv = 0.               ! average temperature of inversion
            do 250 m=i,j-1,1
              Tinv = Tinv + Tsec(m)*(Sectop(m)-Secbot(m)+1)
  250       continue
            Tsec(i) = Tinv/(Sectop(j-1)-Secbot(i)+1)
C---- Update number of nodes in section i and remove sections i+1 to j-1
            Sectop(i) = Sectop(j-1)
            do 260 n=j,NSec,1
              m = n-j+i+1
              Tsec(m) = Tsec(n)
              Secbot(m) = Secbot(n)
              Sectop(m) = Sectop(n)
  260       continue
      
C---- Update number of sections
            NSec = NSec + i + 1 - j

C---- After mixing, there still may be inversions. Redo mixing
            goto 210
          endif
  290   continue                  ! end of loop

C---- If new mixing has occurred, increment mixing counter and request new calculation
        if (Mixing) then
          MixCount = MixCount+1
          goto 110                ! will force new calc of tank temperature for step
        endif

C--------------------------------------------------------------------------
C---- CLEANUP AND CALCULATION OF AUXILIARY QUANTITIES
C--------------------------------------------------------------------------

C---- Convert section temperatures into node temperatures

        do 510 i=NSec,1,-1
          do 520 j = Sectop(i),Secbot(i),-1
            Tstep(j) = Tsec(i)
  520     continue
  510   continue

C---- Update running total of average values of outlet temperatures over the time step
C---- (use the same averaging coefficients as the integration algorithm, so that
C---- energy balance equation is satisfied)

        TOut0 = TOut0 
     &     + ALPHA*Tstep(NOut0) + (1.-ALPHA)*Tstep_prev(NOut0)
        TOut1 = TOut1 
     &     + ALPHA*Tstep(NOut1) + (1.-ALPHA)*Tstep_prev(NOut1)

C---- Update running total of losses to the environment
C---- (use the same averaging coefficients as the integration algorithm, so that
C---- energy balance equation is satisfied)

        QLoss_step = 0.
        UAs = U*NodeHeight*Perimeter
        do 610 i=1,NNodes,1
          QLoss_step = QLoss_step 
     &      + UAs*(ALPHA*Tstep(i)+(1.-ALPHA)*Tstep_prev(i)-Text)           
  610   continue
        QLoss_step = QLoss_step + U*CrossArea*
     &    (ALPHA*Tstep(1) +(1.-ALPHA)*Tstep_prev(1)
     &    +ALPHA*Tstep(NNodes)+(1.-ALPHA)*Tstep_prev(NNodes)
     &    -2.*Text)
        QLoss = QLoss + QLoss_step*ts

C---- Update energy transferred from immersed HXs, and their outlet temperatures

        do 615 i=1,NNodes,1
          QImmersedHX = QImmersedHX + QHX(i)*ts
  615   continue
        if (HasHX0) TOutHX0 = TOutHX0 + TOutHX0step
        if (HasHX1) TOutHX1 = TOutHX1 + TOutHX1step

C---- Advance variables

        do 620 i=1,NNodes,1
          Tstep_prev(i) = Tstep(i)
  620   continue

C---- Update running total of sections
       
        NSecAvg = NSecAvg+NSec

C---- Mark tank as unmixed for next step
 
        NSec = NNodes
        do 630 i=1,NNodes,1
          Secbot(i) = i
          Sectop(i) = i
          Tsec(i) = Tstep(i)
  630   continue

  900 continue                    ! end of internal time step loop

C--------------------------------------------------------------------------
C---- SIMULATION FOR WHOLE TIME STEP IS COMPLETE
C--------------------------------------------------------------------------

C---- Copy results in T array

      do 1010 i=1,NNodes,1
        T(i) = Tstep(i)
 1010 continue

C---- Calculate outlet temperatures

      TOut0 = TOut0/StepCount
      TOut1 = TOut1/StepCount

C---- Calculate outlet temperatures of immersed HXs, if present
      
      if (HasHX0) TOutHX0 = TOutHX0/StepCount
      if (HasHX1) TOutHX1 = TOutHX1/StepCount

C---- Calculate average temperature

      TAvg = GetArrayAverage(T,1,NNodes)

C---- Calculate average heat loss and energy transferred from immersed HX
 
      QLoss = QLoss / dt
      QImmersedHX = QImmersedHX / dt

C---- Calculate average number of mixed sections

      NSecAvg = NSecAvg/StepCount

C---- normal return

      return
      end
 

C===================== immersed_HX_calc ============================
C     Created by: Didier Thevenard
C     Created on: June 2009   
C     Copyright:  CETC 2009
C-----------------------------------------------------------------------
C     ABSTRACT:
C     This function calculates the heat transfer between an immersed
C     HX and the tank fluid for all nodes in the tank
C
C     References:
C     Hemisphere (1990). Hemisphere Handbook of Heat Exchanger Design, G.F. Hewitt, 
C       coordinating Editor. Hemisphere Publishing Corp. Section 2.5.14, 'Helically
C       coiled tubes of circular cross sections'.
C     Incropera FP and DeWitt DP (1990) Fundamentals of heat and mass transfer, third edition. 
C       John Wiley & Sons.
C     Kreith F and Bohn MS (2001) Principles of heat transfer, sixth edition. Brooks/Cole.
C     Liu W, Davidson JH, Kulacki FA and Mantell SC (2003) Natural convection from a horizontal 
C       tube heat exchanger immersed in a tilted enclosure. J. Solar Energy Eng. 125, 67-75
C
C=======================================================================

      SUBROUTINE immersed_HX_calc(NodeHeight, CrossArea,
     &      HInHX, HOutHX, DinHX, DoutHX, DcoilHX, PitchHX, kHX,
     &      Tstep_prev, Mdot, NNodes,
     &      TInHX, MdotHX, GlycolFracHX, TOutHX, QHX)
C-----------------------------------------------------------------------
C     Declarations
C-----------------------------------------------------------------------

C---- Set implicit to none to force declaration of all variables

      IMPLICIT NONE

C---- Parameters
      
C     MAXNNODES is the maximum number of nodes in a stratified tank
      INTEGER MAX_NNODES
      PARAMETER(MAX_NNODES=100)
C     PI is the number pi
      REAL PI
      PARAMETER (PI=3.1415926535897932385)
C     THX_tol is the default tolerance on temperature convergence (C)
      REAL tol
      PARAMETER (tol = 0.001)
C     MAXITER is the maximum number of iterations for THX calculation
      INTEGER MAXITER
      PARAMETER(MAXITER = 30)

C---- Arguments

C     Input: tank parameters (normally don't change from one call to the next)

      REAL    NodeHeight   ! height of one node [m]
      REAL    CrossArea    ! cross area of tank [m2]

C     Input: HX parameters (normally don't change from one call to the next)

      REAL    HInHX        ! height of HX inlet (m)
      REAL    HOutHX       ! height of HX outlet (m)
      REAL    DinHX        ! inside diameter of HX pipe (m)
      REAL    DoutHX       ! outside diameter of HX pipe (m)
      REAL    DcoilHX      ! diameter of HX coil (m)
      REAL    PitchHX      ! pitch of HX coil (vertical distance from one loop to the next) (m)
      REAL    kHX          ! thermal conductivity of coil material (W/m/K)
       
C     Input: tank variables  (change from one call to the next)

      REAL Tstep_prev(MAX_NNODES) ! temperature of nodes at beginning of internal 
                                  ! time step (C)
      REAL    Mdot(MAX_NNODES)! flow rate at each node (value i is from node i 
                           ! to node i+1) (kg/s)
      INTEGER NNodes       ! number of nodes in tank

C     Input: HX variables (change from one call to the next)

      REAL    TInHX        ! inlet temperature of HX (C)
      REAL    MdotHX       ! flow rate through HX (kg/s)
      REAL    GlycolFracHX ! percentage of glycol in HX (0-100%)

C     Output

      REAL    TOutHX       ! outlet temperature of HX (C)
      REAL    QHX(MAX_NNODES)     ! energy transferred from immersed HXs to each node [J]

C---- Local variables

      INTEGER i            ! node counter
      INTEGER iter         ! iteration counter
      INTEGER NInHX        ! Node where inlet is located
      INTEGER NOutHX       ! Node where outlet is located
      REAL    THXin        ! input  temperature of a segment of HX
      REAL    THXout       ! output temperature of a segment of HX
      REAL    THXout_last  ! last calculated temperature of a segment of HX
      REAL    Cp_avg       ! average specific heat of fluid in HX [J/kgK]
      REAL    Ts           ! average temperature of node of tank (C)
      REAL    Twall_i      ! internal wall temperature
      REAL    Twall_o      ! external wall temperature
      REAL    DcurvHX      ! curvature of coil [m]
      REAL    LMTD         ! Log Mean Temperature Difference of HX
      REAL    THX          ! average temperature of segment of HX
      REAL    THX_tol      ! tolerance on calculation of temperature of segment of HX
C     Thermophysical properties fluid inside HX
      REAL    mu           ! dynamic viscosity [Pa�s or kg/m/s]
      REAL    Cp           ! specific heat [J/kg/K]
      REAL    k            ! thermal conductivity [W/m/K]
      REAL    mu_wall_i    ! dynamic viscosity at inside wall temperature [N�s/m or kg/s]
      REAL    Cp_wall_i    ! specific heat at inside wall temperature [J/kg/K]
      REAL    k_wall_i     ! thermal conductivity at inside wall temperature [W/m/K]
C     Non-dimensional numbers of fluid inside HX
      REAL    Pr           ! Prandtl number
      REAL    Pr_wall_i    ! Prandtl number at inside wall temperature
      REAL    Re           ! Reynolds number
      REAL    Re_crit      ! Reynolds number for transition to turbulence
      REAL    Re_turb      ! Reynolds number for full turbulence
      REAL    Nu           ! Nusselt number
      REAL    Nu_crit      ! Nusselt number at Re = Re_crit
      REAL    Nu_turb      ! Nusselt number at Re = Re_turb
      REAL    m            ! auxiliary exponent in calculation of Nu
      REAL    f            ! auxiliary coefficient in the calculation of Nu
      REAL    A            ! auxiliary coefficient in the calculation of Nu
C     Heat transfer coefficients between HX fluid and tank fluid
      REAL    U_int        ! heat transfer coefficient, inside of pipe [W/m/K]
      REAL    U_wall       ! heat transfer coefficient through pipe [W/m/K]
      REAL    U_ext        ! heat transfer coefficient, outside of pipe [W/m/K]
      REAL    U            ! overall heat transfer coefficient [W/m/K]
      LOGICAL CLOSEA       ! Auxiliary variable

C     Thermophysical properties of tank fluid
      REAL    mu_s         ! dynamic viscosity [Pa�s or kg/m/s]
      REAL    Cp_s         ! specific heat [J/g/K]
      REAL    beta_s       ! coefficient of volumetric thermal expansion [1/K] 
      REAL    k_s          ! thermal conductivity [W/m/K]
      REAL    rho_s        ! density [kg/m3]
      REAL    mu_wall_s    ! dynamic viscosity at outside wall temperature [Pa�s/m or kg/m/s]
      REAL    Cp_wall_s    ! specific heat at outside wall temperature [J/kg/K]
      REAL    k_wall_s     ! thermal conductivity at inside wall temperature [W/m/K]
      REAL    Pr_s         ! Prandtl number []
      REAL    Pr_wall_s    ! Prandtl number at wall temperature []
C     Heat transfer in tank outside HX
      REAL    Daux         ! half circumference of HX pipe [m]
      REAL    DeltaT       ! temperature difference between tank fluid and outside of HX wall [K]
      REAL    Ra           ! Rayleigh number []
      REAL    Nu_n         ! Nusselt number associated with natural convection []
      REAL    Nu_f         ! Nusselt number associated with forced convection []
      REAL    Nu_m         ! Nusselt number associated with mixed convection []
      REAL    V            ! velocity of fluid in tank node [m/s]
      REAL    Re_s         ! Reynolds number in tank node []
      REAL    C            ! auxiliary coefficient 
      REAL    n            ! auxiliary exponent for Nusselt number calculation
      REAL    NTU          ! NTU of segment of HX
      REAL    eps          ! effectiveness of segment of HX
C     Coil characteristics
      REAL    L            ! length of coil within current node [m] 
      REAL    Htop         ! height of top of the HX [m]
      REAL    Hbot         ! height of bottom of the HX [m]
      REAL    Hhx          ! height of the HX that is contained within current node [m]
      INTEGER NStepHX      ! step by which to explore nodes (+1 if HX inlet lower than outlet, -1 otherwise)

C---- Functions

      INTEGER Bound                ! Bound a number by two others
      REAL glycol_water_density    ! Function for density of glycol-water mixture [kg/m3]
      REAL glycol_water_viscosity  ! Function for dynamic viscosity of glycol-water mixture [Pa�s]
      REAL glycol_water_cond       ! Function for thermal conductivity of glycol [W/m/K]
      REAL glycol_water_cp         ! Function for specific heat or glycol-water [J/kg/K]
      REAL glycol_water_thermexp   ! Function for coefficient of thermal expansion of glycol-water [1/K]

C-----------------------------------------------------------------------
C     Preparatory work
C-----------------------------------------------------------------------

C---- There is no flow: nothing to do
      CALL ECLOSE(MdotHX,0.0,1.0E-15,CLOSEA) 
      if (CLOSEA) then
        TOutHX = TInHX
        RETURN
      endif

C---- Calculate the nodes where the HX is located

      NInHX   = Bound(NINT(HInHX  /NodeHeight+0.5), 1, NNodes)
      NOutHX  = Bound(NINT(HOutHX /NodeHeight+0.5), 1, NNodes)

C---- Calculate curvature of HX
      DcurvHX = DcoilHX*(1.+(PitchHX/PI/DcoilHX)**2.)

C---- Calculate specific heat at HX inlet temperature
      Cp_avg = glycol_water_cp(GlycolFracHX,TinHX)

C=======================================================================
C     Loop on segments of the HX
C=======================================================================
C
C-----------------------------------------------------------------------
C       Progress from the inlet to the outlet of the HX, dividing the HX in segments, each
C       segment corresponding to a node of the tank. 
C         THXin is the inlet temperature of fluid in the segment of the HX
C         THXout is the outlet temperature of fluid in the segment of the HX
C         THX is the 'average' temperature of fluid in the segment of the HX (based on the LMTD)
C       Fluid properties are estimated at THX. Since THX is not known until the LMTD is known,
C       an iterative solution is required. It works this way:
C         1. Start by assuming THX = THXin
C         2. Calculate the heat transfer coefficient between the HX segment and the fluid
C         3. Calculate the HX segment outlet temperature THXout
C         4. Calculate the LMTD and THX
C         5. Go to step 2 
C       Convergence is obtained when THXout does not vary more than the specified threshold.
C-----------------------------------------------------------------------

C---- Tolerance on HX temperature calculation for just one segment
C     This is the tolerance on the HX temperature calculation, divided by the number of segments
      THX_tol = tol/(abs(NOutHX-NInHX)+1)

C---- The loop proceeds up or down depending upon the relative positions of 
C     the inlet and the outlet
      if (NOutHX >= NInHX) then
        NStepHX = 1
      else
        NStepHX = -1
      endif
  
C---- Start of the loop

      THXout = TInHX

      do 900 i= NInHX,NOutHX,NStepHX

        THXin = THXout        ! Inlet temperature is outlet temperature of previous segment

C----   Get average temperature of node at end of previous internal time step

        Ts = Tstep_prev(i)

C----   Provide initial guess of Twall as average of THXin and Ts

        Twall_o = (THXin + Ts)/2.
        Twall_i = Twall_o

C----   Particular case: THXin is equal to Ts within tolerance: nothing much to do
        if (abs(THXin - Ts) < THX_tol) cycle      ! this goes to the next segment of the HX

C----   Iterate on calculation of THX
C       Initial guess is THX = THXout = THXin
        THX = THXin
        THXout_last = Ts                          ! forces first pass in loop
      
C----   Iterative loop
        iter = 0
        do while (abs(THXout_last-THXout)>THX_tol .AND. iter<MAXITER)
          iter = iter+1
          THXout_last = THXout

C================= Inside of coil ===============
        
C----     Fluid properties of HX liquid are determined at THX
          mu = glycol_water_viscosity(GlycolFracHX,THX)
          Cp = glycol_water_cp(GlycolFracHX,THX)
          k = glycol_water_cond(GlycolFracHX,THX)
          mu_wall_i = glycol_water_viscosity(GlycolFracHX,Twall_i)
          Cp_wall_i = glycol_water_cp(GlycolFracHX,Twall_i)
          k_wall_i = glycol_water_cond(GlycolFracHX,Twall_i)
        
C----     Calculate non-dimensional numbers
          Pr = mu*Cp/k
          Pr_wall_i = mu_wall_i*Cp_wall_i/k_wall_i
        
C----     Calculate Reynolds number
          Re = MdotHX*4./(PI*DinHX*mu)
C----     Check if Reynolds number is above transition to turbulence
C         Hemisphere (1990), eq. 8
          Re_crit = 2300*(1.+8.6*(DinHX/DcurvHX)**0.45)
          Re_turb = 22000
C----       Laminar flow correlation
C           Hemisphere (1990), eq. 9
          if (Re <= Re_crit) then
            m = 0.5 + 0.2903*(DinHX/DcurvHX)**0.194
            Nu = 3.65 + 0.08*(1. + 0.8*(DinHX/DcurvHX)**0.9)
     &           *(Re**m)*(Pr**(1./3.))*((Pr/Pr_wall_i)**0.14)
C----       Turbulent flow correlation
C           Gnielinski, cited in Incropera & DeWitt (1990), eq 8.63a, 8.63b
          else if (Re >= Re_turb) then
            f = 1.0 / (0.79*log(Re)-1.64)**2.
            Nu = (f/8.) * (Re-1000.)
     &         * Pr/(1 + 12.7*sqrt(f/8.)*(Pr**(2./3.)-1.))
C----       Transition area
C           Hemisphere (1990) eq. 12 and 13
          else
            m = 0.5 + 0.2903*(DinHX/DcurvHX)**0.194
            Nu_crit = 3.65 + 0.08*(1. + 0.8*(DinHX/DcurvHX)**0.9)
     &        *(Re_crit**m)*(Pr**(1./3.))*((Pr/Pr_wall_i)**0.14)
            f = 1.0 / (0.79*log(Re_turb)-1.64)**2.
            Nu_turb = (f/8.)*(Re_turb-1000.)*Pr
     &        /(1+12.7*sqrt(f/8.)*(Pr**(2./3.)-1.))
            A = (Re_turb-Re)/(Re_turb-Re_crit)
            Nu = A*Nu_crit+(1.-A)*Nu_turb
          endif
C----     Convert Nu into thermal conductance, expressed per unit length of coil
          U_int = Nu*k/DinHX
        
        
C================= Conduction of heat through wall of coil ===============
C         Kreith and Bohn, 2001, eq. 2.39
        
          U_wall = 2*PI*kHX/log(DoutHX/DinHX)  
        
        
C================= Outside of coil ===============
        
C----     Thermophysical properties of tank liquid for that node
          mu_s = glycol_water_viscosity(0.,Ts)
          beta_s = glycol_water_thermexp(0.,Ts)
          k_s = glycol_water_cond(0.,Ts)
          rho_s = glycol_water_density(0.,Ts)
          Cp_s = glycol_water_cp(0.,Ts)
          mu_wall_s = glycol_water_viscosity(0.,Twall_o)
          Cp_wall_s = glycol_water_cp(0.,Twall_o)
          k_wall_s = glycol_water_cond(0.,Twall_o)
          Pr_s = mu_s*Cp_s/k_s
          Pr_wall_s = mu_wall_s*Cp_wall_s/k_wall_s
        
C----     Natural convection
C         The length scale to use for Ra is the distance that a fluid particle travels 
C         in the boundary layer on the body, or PI�D/2. See Liu, 2003
          Daux = PI*DoutHX/2
          DeltaT = abs(Ts-Twall_o)
          Ra = abs(9.81*beta_s*Daux**3.*DeltaT
     &      /(mu_s/rho_s)/(k_s/rho_s/Cp_s))
          Nu_n = 0.52*Ra**0.25
        
C----     Forced convection
C         Zhukauskas correlation. In Incropera & DeWitt (1990) eq. 7.56
          V = Mdot(i)/rho_s/CrossArea
          Re_s = abs(V)*DoutHX/(mu_s/rho_s)
          if (Re_s < 40) then
            C = 0.75
            m = 0.4
          else if (Re_s < 1000) then
            C = 0.51
            m = 0.5
          else 
            C = 0.26
            m = 0.6
          endif
          if (Pr < 10) then
            n = 0.37
          else
            n = 0.36
          endif
          Nu_f = C * (Re_s**m) * (Pr_s**n) 
     &         * ((Pr_s/Pr_wall_s)**0.25)
        
C----     Mixed convection
C         Incropera & DeWitt (1990), eq. 9.64
          Nu_m = ((Nu_n ** 3.) + (Nu_f ** 3.)) ** (1./3.)
        
C----     Calculate thermal conductance, U_ext
          U_ext = Nu_m*k/DoutHX 
        
C======== Calculate heat transfer from HX fluid to tank ===============================
C----     Calculate total conductance, per unit length of coil
C         For safety, check U_int and U_ext. Experience shows that U_ext may be zero 
C         in very rare circumstances
          U = 1./(1./U_int + 1./U_wall + 1./U_ext)
          Htop = max(HInHX, HOutHX)
          Hbot = min(HInHX, HOutHX)
          Hhx = min(i*NodeHeight, Htop) 
     &        - max((i-1)*NodeHeight, Hbot)
          if (PitchHX > 0.) then
            L = PI*DcurvHX*Hhx/PitchHX 
          else
            L = PI*DcurvHX               ! case of flat loop
          endif
C----     For NTU, see Incropera & DeWitt (1990), ch. 11
C         eps is limited to 99.5% to avoid rare numerical problems, i.e. THXout = Ts
C         which then leads to Nu_n = 0 and U_ext = 0 at the next iteration
          NTU = U*DinHX*PI*L/MdotHX/Cp_avg
          eps = min(1.-exp(-NTU), 0.995)  
          THXout = THXin - eps*(THXin-Ts)
                   
C----     Calculate LMTD, new THX
C         Note that LMTD = (DeltaTHXin-DeltaTHXout)/log(DeltaTHXin/DeltaTHXout)
C         with DeltaTHXin = (THXin - Ts)
C         and  DeltaTHXout = (THXout - Ts)
C         which is simplified using the expression for THXout above 
          LMTD = -eps/log(1-eps)*(THXin-Ts)
          THX = Ts + LMTD             ! this is the average HX fluid temperature

C----     Update inside and outside wall temperatures, for next iteration
          Twall_i = THX - U*(THX-Ts)/U_int
          Twall_o = Ts + U*(THX-Ts)/U_ext

C----     End of iterative loop
        end do
        if (iter >= MAXITER) 
     &    write(6,*) 'Warning - no convergence in immersed_HX_calc'

C----   Convergence has been reached for this node. Update QHX(i)
       
        QHX(i) = QHX(i) + MdotHX*Cp_avg*(THXin-THXout)   

C----   Go to next segment of HX

  900 CONTINUE
     
C---- Update running total of temperature of HX outlet (will be used to calculate 
C     average of temperature over the step)

      TOutHX = THXout

C---- Normal return
     
      return
      end


