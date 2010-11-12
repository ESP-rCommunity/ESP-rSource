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

C===================== stratified_tank_2hx_static ======================
C     Created by: Didier Thevenard
C     Created on: June 2009
C     Copyright:  CETC 2009
C-----------------------------------------------------------------------
C     ABSTRACT:
C     Establishes for a stratified tank with 1 immersed HX whether the 
C     correct number of control variables have been specified, whether 
C     the number of water connections is correct and whether the 
C     connections are to the correct fluid type.
C
C=======================================================================

      SUBROUTINE stratified_tank_2hx_static(IPCOMP)

C---- Set implicit to none to force declaration of all variables

      IMPLICIT NONE
      
C---- Include statements

#include "plant.h"
#include "building.h"

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

#include "stratified_tank.h"
      
C---- Local variables      

      INTEGER NITMS
      INTEGER J
      INTEGER NCITM,NCONS
      INTEGER IPCON,IPCONC
      LOGICAL CLOSEA
      LOGICAL ERROR_FLAG         
      INTEGER NNODES            ! Number of nodes of stratified tank
                                
      REAL HTANK                ! Tank height (m)
      REAL HIN1                 ! Height of inlet 1 (m)
      REAL HOUT1                ! Height of outlet 1 (m)
      REAL TINIT                ! Initial temperature of stratified tank (C)
      REAL K_DESTRAT            ! Destratification conductivity [W/m/C]
      REAL VOL                  ! Tank volume [m3]
      REAL U                    ! Tank heat loss coefficient [W/m2/C]
      REAL HINHX1               ! Height of inlet of HX1 (m)
      REAL HOUTHX1              ! Height of inlet of HX1 (m)
      REAL DINHX1               ! Internal diameter of HX1 (m)
      REAL DOUTHX1              ! External diameter of HX1 (m)
      REAL DCOILHX1             ! Diameter of the coil of HX1 (m)
      REAL PITCHHX1             ! Pitch of the coil of HX1 (m)
      REAL KHX1                 ! Thermal conductivity of HX1 (W/m/C)
      REAL HINHX2               ! Height of inlet of HX2 (m)
      REAL HOUTHX2              ! Height of inlet of HX2 (m)
      REAL DINHX2               ! Internal diameter of HX2 (m)
      REAL DOUTHX2              ! External diameter of HX2 (m)
      REAL DCOILHX2             ! Diameter of the coil of HX2 (m)
      REAL PITCHHX2             ! Pitch of the coil of HX2 (m)
      REAL KHX2                 ! Thermal conductivity of HX2 (W/m/C)

C---- Set error flag to false
      ERROR_FLAG = .false.

C---- Trace output

      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
        WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
        WRITE(ITU,*) ' 2 node (ISV=20) stratified storage tank model'
        WRITE(ITU,*) ' with 1 immersed heat exchager'
        NITMS=19
        WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
        call epagew
      END IF ! Matches to IF(ITC.GT.0.AND.ITRACE(35).NE.0)

C---- Check value of parameters

      VOL   = ADATA(IPCOMP,1)     ! Tank volume
      HTANK = ADATA(IPCOMP,2)     ! Tank height
      HIN1  = ADATA(IPCOMP,4)     ! Height of inlet 1
      HOUT1 = ADATA(IPCOMP,5)     ! Height of outlet 1
      U     = ADATA(IPCOMP,6)     ! Tank heat loss coefficient
      K_DESTRAT = ADATA(IPCOMP,7) ! Additional destratification coefficient
      NNODES = INT(ADATA(IPCOMP,8))   ! Number of nodes
      HINHX1 = ADATA(IPCOMP,12)   ! Height of inlet of HX1 (m)
      HOUTHX1 = ADATA(IPCOMP,13)  ! Height of inlet of HX1 (m)
      DINHX1 = ADATA(IPCOMP,14)   ! Internal diameter of HX1 (m)
      DOUTHX1 = ADATA(IPCOMP,15)  ! External diameter of HX1 (m)
      DCOILHX1 = ADATA(IPCOMP,16) ! Diameter of the coil of HX1 (m)
      PITCHHX1 = ADATA(IPCOMP,17) ! Pitch of the coil of HX1 (m)
      KHX1 = ADATA(IPCOMP,18)     ! Thermal conductivity of HX1 (W/m/C)
      HINHX2 = ADATA(IPCOMP,19)   ! Height of inlet of HX2 (m)
      HOUTHX2 = ADATA(IPCOMP,20)  ! Height of inlet of HX2 (m)
      DINHX2 = ADATA(IPCOMP,21)   ! Internal diameter of HX2 (m)
      DOUTHX2 = ADATA(IPCOMP,22)  ! External diameter of HX2 (m)
      DCOILHX2 = ADATA(IPCOMP,23) ! Diameter of the coil of HX2 (m)
      PITCHHX2 = ADATA(IPCOMP,24) ! Pitch of the coil of HX2 (m)
      KHX2 = ADATA(IPCOMP,25)     ! Thermal conductivity of HX2 (W/m/C)

C---- Check that number of nodes is within acceptable range
      IF (NNodes .GT. MAX_NNODES .OR. NNodes .LT. 1) THEN
        WRITE(IUOUT,*) ' stratified_tank_2hx_static: Number of nodes'
        WRITE(IUOUT,*) ' not within range 1 - ', MAX_NNODES
        STOP ' stratified_tank_2hx_static: unresolvable error'
      endif
C---- Check that parameters are properly defined
      IF (VOL .LE. 0.) THEN
        WRITE(IUOUT,*) ' stratified_tank_2hx_static: Tank volume'
        WRITE(IUOUT,*) ' is less than or equal to 0'
        STOP ' stratified_tank_2hx_static: unresolvable error'
      endif
      IF (HTANK .LE. 0.1) THEN
        WRITE(IUOUT,*) ' stratified_tank_2hx_static: Tank height'
        WRITE(IUOUT,*) ' is less than 0.1 m'
        STOP ' stratified_tank_2hx_static: unresolvable error'
      endif
      IF (U .LT. 0.) THEN
        WRITE(IUOUT,*) ' stratified_tank_2hx_static: Tank heat loss'
        WRITE(IUOUT,*) ' coefficient is negative'
        STOP ' stratified_tank_2hx_static: unresolvable error'
      endif
      IF (K_DESTRAT .LT. 0.) THEN
        WRITE(IUOUT,*) ' stratified_tank_2hx_static: destratification'
        WRITE(IUOUT,*) ' conductivity is negative'
        STOP ' stratified_tank_2hx_static: unresolvable error'
      endif
C---- Check that heights of inlets and outlets are within tank height
      IF (HIN1.GT.HTANK .OR. HOUT1.GT.HTANK .OR.
     &    HINHX1.GT.HTANK .OR. HOUTHX1.GT.HTANK .OR.
     &    HINHX2.GT.HTANK .OR. HOUTHX2.GT.HTANK) THEN
        WRITE(IUOUT,*) ' stratified_tank_2hx_static: Height of inlet'
        WRITE(IUOUT,*) ' or outlet greater than tank height'
        STOP ' stratified_tank_2hx_static: unresolvable error'
      ENDIF
C---- Check that immersed HX parameters are properly defined
      IF (DINHX1.GE.DOUTHX1 .OR.DINHX2.GE.DOUTHX2) THEN
        WRITE(IUOUT,*) ' stratified_tank_2hx_static: Coil internal'
        WRITE(IUOUT,*) ' diameter >= external diameter.'
        STOP ' stratified_tank_2hx_static: unresolvable error'
      ENDIF
C---- Check that containment exists
      CALL ECLOSE(PCNTMF(IPCOMP),-99.00,0.001,CLOSEA)
      IF(CLOSEA) THEN
        WRITE(IUOUT,*) ' stratified_tank_2hx_static : A containment '
        WRITE(IUOUT,*) ' must be specified for component ',IPCOMP
        WRITE(IUOUT,*) ' and all components of the same type'
        STOP ' stratified_tank_2hx_static: unresolvable error'
      ENDIF ! Matches to IF(CLOSEA) THEN

C---- Check user specified number of controlled variables

      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM) THEN
        WRITE(ITU,*) ' stratified_tank_2hx_static warning: user ',
     &               ' specified wrong number of controlled ',
     &               ' variables '
        STOP ' stratified_tank_2hx_static: unresolvable error'
      ENDIF

C---- Check component has 3 connections only, to water

      NCONS=3
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
        WRITE(IUOUT,*) ' stratified_tank_2hx_static: '
        WRITE(IUOUT,*) ' connection error for component ',IPCOMP
        WRITE(IUOUT,*) '   should be ',NCONS,' water connection(s)'
        STOP ' stratified_tank_2hx_static: unresolvable error'
      ENDIF

C---- Component is well defined. Store IPCOMP in stratified tank common
C---- block, then set the node temperatures to their initial value

      N_STANK = N_STANK+1
      IF (N_STANK .GT. MAX_STANK) THEN
        WRITE(IUOUT,*) ' stratified_tank_2hx_static: '
        WRITE(IUOUT,*) ' too many stratified tanks defined'
        WRITE(IUOUT,*) ' maximum should be ',MAX_STANK
        STOP ' stratified_tank_2hx_static: unresolvable error'
      ENDIF
      IPCOMP_STANK(N_STANK) = IPCOMP
      NNODES = INT(ADATA(IPCOMP,8))
      IF (NNODES .LT. 1) NNODES=1
      TINIT = ADATA(IPCOMP,10)
      DO 20 J=1,NNODES,1
        TP_STANK(J,N_STANK) = TINIT
        TF_STANK(J,N_STANK) = TINIT
   20 CONTINUE
      TAVGP_STANK(N_STANK) = TINIT
      TAVGF_STANK(N_STANK) = TINIT

C---- Normal return
 
      RETURN    
      END

C===================== stratified_tank_2hx_coeff_gen ===================
C     Created by: Didier Thevenard
C     Created on: June 2009
C     Copyright:  CETC 2009
C-----------------------------------------------------------------------
C     ABSTRACT:
C     This is the ESP-r coefficient generator for the stratified tank 
C     with 1 immersed HX
C     The model is based on an input/output approach, i.e. it calculates
C     temperatures at the two outlet nodes given temperatures and 
C     flowrates at the two inlet nodes. 
C
C References:

C See model description in separate document, DEVELOPMENT OF A STRATIFIED
C TANK MODEL WITH IMMERSED HEAT EXCHANGERS IN ESP-R: MODEL SUMMARY, 
C by Didier Thevenard (June 2009)
C Available from CANMET Energy Technology Centre, Natural Resources 
C Canada, Government of Canada

C=======================================================================

      SUBROUTINE stratified_tank_2hx_coeff_gen(IPCOMP,COUT,ISTATS)

C-----------------------------------------------------------------------
C     Declarations
C-----------------------------------------------------------------------

C---- Set implicit to none to force declaration of all variables

      IMPLICIT NONE

C---- Include statements

#include "building.h"
#include "plant.h"

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
      INTEGER ICON3           ! Pointer to interconnection 3
      INTEGER INOD3           ! Pointer to node 3
      REAL TOUT1              ! Outlet temperature 1 [C]
      REAL TOUT2              ! Outlet temperature 2 [C]
      REAL TOUT3              ! Outlet temperature 3 [C]

C---- Trace output

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine ',
     &   ' stratified_tank_2hx_coeff_gen '

C---- Initialize pointers to inter-connection(s) ICON, and node(s) INOD.

      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,2,1)
      ICON3=ICONDX(IPCOMP,3,1)
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1
      INOD3=NPCDAT(IPCOMP,9)+2

C=======================================================================
C     Generate coefficients for energy balance equation
C=======================================================================

      IF(ISTATS.EQ.1) THEN

C---- Mark temperature of nodes for iteration

        ICSV(INOD1,1)=1
        CSVI(INOD1,1)=CSVF(INOD1,1)
        ICSV(INOD2,1)=1
        CSVI(INOD2,1)=CSVF(INOD2,1)
        ICSV(INOD3,1)=1
        CSVI(INOD3,1)=CSVF(INOD3,1)

C---- Call wrapper of TRNSYS-like function to calculate temperature of tank outlets
      
        CALL stratified_tank_2hx_wrapper(IPCOMP,TOUT1,TOUT2,TOUT3)

C Establish matrix equation self-coupling coefficients,
         COUT(1)=1.
         COUT(2)=1.
         COUT(3)=1.
C then matrix equation cross-coupling coefficients,
         COUT(4)=0.
         COUT(5)=0.
         COUT(6)=0.
C and then present-time coefficients (ie. right hand sides)
         COUT(7)=TOUT1
         COUT(8)=TOUT2
         COUT(9)=TOUT3

C=======================================================================
C     Generate coefficients for 1st phase flow equation.
C=======================================================================

      ELSEIF(ISTATS.EQ.2) THEN

         COUT(1)=1.
         COUT(2)=1.
         COUT(3)=1.
         COUT(4)=-PCONDR(ICON1)
         COUT(5)=-PCONDR(ICON2)
         COUT(6)=-PCONDR(ICON3)
         COUT(7)=0.
         COUT(8)=0.
         COUT(9)=0.

C=======================================================================
C     Generate coefficients for 2nd phase flow equation.
C=======================================================================

      ELSEIF(ISTATS.EQ.3) THEN

         COUT(1)=1.
         COUT(2)=1.
         COUT(3)=1.
         COUT(4)=-PCONDR(ICON1)
         COUT(5)=-PCONDR(ICON2)
         COUT(6)=-PCONDR(ICON3)
         COUT(7)=0.
         COUT(8)=0.
         COUT(9)=0.

      ENDIF 

C-----------------------------------------------------------------------
C     End of calculation
C-----------------------------------------------------------------------

C---- Trace output

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
        WRITE(ITU,*) ' Component     ',IPCOMP,':'
        WRITE(ITU,*) ' 2 node (ISV=20) stratified tank '
        WRITE(ITU,*) ' with 2 immersed heat exchangerS '
        WRITE(ITU,*) ' Matrix node(s) ',INOD1,', ',INOD2,', ',INOD3
        WRITE(ITU,*) ' Connection(s)  ',ICON1,', ',ICON2,', ',ICON3
        IF(ISTATS.EQ.1) THEN
          WRITE(ITU,*) ' DAY        =', IDYF
          WRITE(ITU,*) ' HOUR       =', IHRF
          WRITE(ITU,*) ' '
        ENDIF ! Matches IF(ISTATS.EQ.1)
        WRITE(ITU,*) 'Exiting subroutine stratified_tank_2hx_coeff_gen'
      ENDIF ! Matches IF(ITC.GT.0.AND.NSINC.GE.ITC ...

C---- Return to the calling module

      RETURN

      END

C===================== stratified_tank_2hx_wrapper =========================
C     Created by: Didier Thevenard
C     Created on: June 2009
C     Copyright:  CETC 2009
C-----------------------------------------------------------------------
C     ABSTRACT:
C     This is a wrapper around the subroutine that calculates
C     the temperatures in the stratified tank. The wrapper is called 
C     by the ESP-r coefficient generator
C
C=======================================================================
      SUBROUTINE stratified_tank_2hx_wrapper(IPCOMP,TOUT1,TOUTHX1,
     &  TOUTHX2)

C---- Set implicit to none to force declaration of all variables

      IMPLICIT NONE

C---- Include statements

#include "building.h"
#include "plant.h"

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


#include "stratified_tank.h"

C---- Arguments

      INTEGER  IPCOMP           ! Component number
      REAL TOUT1                ! Outlet temperature 1 [C]
      REAL TOUT2                ! Outlet temperature 2 [C] (dummy variable)
      REAL TOUTHX1              ! Outlet temperature of HX1 [C]
      REAL TOUTHX2              ! Outlet temperature of HX2 [C]

C---- Local variables

      LOGICAL CLOSEA,CLOSEB     ! Auxiliary variable 
      REAL AUX                  ! Auxiliary variable
      REAL HIN1                 ! Height of inlet 1 [m]
      REAL HOUT1                ! Height of outlet 1 [m]
      REAL HIN2                 ! Height of inlet 2 [m] (dummy variable)
      REAL HOUT2                ! Height of outlet 2 [m] (dummy variable)
      REAL MDOT1                ! Flow rate at inlet 1 [kg/s]
      REAL MDOT2                ! Flow rate at inlet 1 [kg/s] (always zero)
      REAL TIN1                 ! Temperature at inlet 1 [C]
      REAL TIN2                 ! Temperature at inlet 2 [C] (dummy variable)
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
      INTEGER ICON3             ! Pointer to inter-connection 3
      INTEGER I,J               ! Counters

C     Parameters of immersed HX 1
      REAL HINHX1               ! Height of inlet of HX1 (m)
      REAL HOUTHX1              ! Height of inlet of HX1 (m)
      REAL DINHX1               ! Internal diameter of HX1 (m)
      REAL DOUTHX1              ! External diameter of HX1 (m)
      REAL DCOILHX1             ! Diameter of the coil of HX1 (m)
      REAL PITCHHX1             ! Pitch of the coil of HX1 (m)
      REAL KHX1                 ! Thermal conductivity of HX1 (W/m/C)
      REAL GLYCOLHX1            ! Mass percentage of propylene glycol in HX (0 or 50) (%)
      REAL MDOTHX1              ! Flow rate through HX1 [kg/s]
      REAL TINHX1               ! Temperature at inlet of HX1 [C]

C     Parameters of immersed HX 2
      REAL HINHX2               ! Height of inlet of HX2 (m)
      REAL HOUTHX2              ! Height of inlet of HX2 (m)
      REAL DINHX2               ! Internal diameter of HX2 (m)
      REAL DOUTHX2              ! External diameter of HX2 (m)
      REAL DCOILHX2             ! Diameter of the coil of HX2 (m)
      REAL PITCHHX2             ! Pitch of the coil of HX2 (m)
      REAL KHX2                 ! Thermal conductivity of HX2 (W/m/C)
      REAL GLYCOLHX2            ! Mass percentage of propylene glycol in HX (0 or 50) (%)
      REAL MDOTHX2              ! Flow rate through HX2 [kg/s]
      REAL TINHX2               ! Temperature at inlet of HX2 [C]

C     Outputs of model
      REAL QLOSS                ! Heat loss from tank to environment [J]
      REAL QIMMERSED_HX         ! Heat gain from immersed heat exchangers [J]
      INTEGER STEPCOUNT         ! Number of internal time steps
      INTEGER MIXCOUNT          ! Number of times mixing algorithm called
      INTEGER NSECAVG           ! Average number of mixed sections
      INTEGER IER               ! Error code

C     Dummy variables
      LOGICAL HASHX1            ! Indicates presence of HX1 (.TRUE.)
      LOGICAL HASHX2            ! Indicates presence of HX2 (.TRUE.)
      REAL DUMMY                ! Dummy variable

C---- Functions
      REAL SHTFLD
      REAL RHOFLD
      REAL KWATER

C---- Trace output

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine ',
     &   ' stratified_tank_2hx_wrapper '

C=======================================================================
C---- Initialise component parameters from the database information

      VOL   = ADATA(IPCOMP,1)     ! Tank volume
      H     = ADATA(IPCOMP,2)     ! Tank height
      P     = ADATA(IPCOMP,3)     ! Tank perimeter as entered by user
      HIN1  = ADATA(IPCOMP,4)     ! Height of inlet 1
      HOUT1 = ADATA(IPCOMP,5)     ! Height of outlet 1
      U     = ADATA(IPCOMP,6)     ! Tank heat loss coefficient
      K_DESTRAT = ADATA(IPCOMP,7) ! Additional destratification coefficient
      NNODES = INT(ADATA(IPCOMP,8))   
                                  ! Number of nodes
      NSTEPS = INT(ADATA(IPCOMP,9))   
                                  ! Number of internal time steps per sim. t.s.
      TBOIL  = ADATA(IPCOMP,11)   ! Boiling temperature of tank fluid 

      HINHX1 = ADATA(IPCOMP,12)   ! Height of inlet of HX1 (m)
      HOUTHX1 = ADATA(IPCOMP,13)  ! Height of inlet of HX1 (m)
      DINHX1 = ADATA(IPCOMP,14)   ! Internal diameter of HX1 (m)
      DOUTHX1 = ADATA(IPCOMP,15)  ! External diameter of HX1 (m)
      DCOILHX1 = ADATA(IPCOMP,16) ! Diameter of the coil of HX1 (m)
      PITCHHX1 = ADATA(IPCOMP,17) ! Pitch of the coil of HX1 (m)
      KHX1 = ADATA(IPCOMP,18)     ! Thermal conductivity of HX1 (W/m/C)

      HINHX2 = ADATA(IPCOMP,19)   ! Height of inlet of HX2 (m)
      HOUTHX2 = ADATA(IPCOMP,20)  ! Height of inlet of HX2 (m)
      DINHX2 = ADATA(IPCOMP,21)   ! Internal diameter of HX2 (m)
      DOUTHX2 = ADATA(IPCOMP,22)  ! External diameter of HX2 (m)
      DCOILHX2 = ADATA(IPCOMP,23) ! Diameter of the coil of HX2 (m)
      PITCHHX2 = ADATA(IPCOMP,24) ! Pitch of the coil of HX2 (m)
      KHX2 = ADATA(IPCOMP,25)     ! Thermal conductivity of HX2 (W/m/C)

C---- Initialize pointers to inter-connection(s) ICON, and node(s) INOD.

      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,2,1)
      ICON3=ICONDX(IPCOMP,3,1)

C---- Identify variables

      TIN1=CONVAR(ICON1,1)
      TIN2=0.
      TINHX1=CONVAR(ICON2,1)
      TINHX2=CONVAR(ICON3,1)
      MDOT1=PCONDR(ICON1)*CONVAR(ICON1,2)
      MDOT2=0.
      MDOTHX1=PCONDR(ICON2)*(CONVAR(ICON2,2)+CONVAR(ICON2,3))
      MDOTHX2=PCONDR(ICON3)*(CONVAR(ICON3,2)+CONVAR(ICON3,3))

C---- Calculate mass percentage of glycol in HX fluids

      AUX = CONVAR(ICON2,2)+CONVAR(ICON2,3)
      if (AUX /= 0.) THEN
        GLYCOLHX1 = 100.*CONVAR(ICON2,3)/AUX
      ELSE
        GLYCOLHX1 = 0.
      ENDIF
      AUX = CONVAR(ICON3,2)+CONVAR(ICON3,3)
      if (AUX /= 0.) THEN
        GLYCOLHX2 = 100.*CONVAR(ICON3,3)/AUX
      ELSE
        GLYCOLHX2 = 0.
      ENDIF

C---- Check that tank fluid does not have glycol
C     Enabling the tank to be filled with glycol would not be difficult to implement, 
C     however this is not done for now as such systems are probably very infrequent

      IF (CONVAR(ICON1,3) /= 0.) THEN
        WRITE(ITU,*) 'Error in stratified tank model'
        WRITE(ITU,*) 'Tank can be filled with water only - no glycol'
        WRITE(ITU,*) 'Aborting simulation'
        STOP
      ENDIF

C---- Calculate containment temperature. If no containment, set U to zero

      TCONT = PCNTMF(IPCOMP)
      CALL ECLOSE(TCONT,-99.0,0.001,CLOSEA)
      IF(CLOSEA) U=0.

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
      HASHX1=.TRUE.
      HASHX2=.TRUE.
      DUMMY=0.
      CALL stratified_tank_calc(VOL, H, P,
     &  HIN1, 0., HOUT1, H, U, K_DESTRAT, NSTEPS, NNODES,
     &  HASHX1, 
     &  HINHX1, HOUTHX1, DINHX1, DOUTHX1, DCOILHX1, PITCHHX1, KHX1,
     &  HASHX2, 
     &  HINHX2, HOUTHX2, DINHX2, DOUTHX2, DCOILHX2, PITCHHX2, KHX2,
     &  TIMSEC, TIN1, TIN2, MDOT1, MDOT2, TCONT, CP, K,
     &  RHO, TBOIL, TP_STANK(1,I_STANK),
     &  TINHX1, MDOTHX1, GLYCOLHX1,
     &  TINHX2, MDOTHX2, GLYCOLHX2,
     &  TOUT1, TOUT2, TAVGF_STANK(I_STANK), TOUTHX1, TOUTHX2, 
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

      NAPDAT(IPCOMP) = 6
      PCAOUT(IPCOMP,1) = TAVGF_STANK(I_STANK)
      PCAOUT(IPCOMP,2) = QLOSS
      PCAOUT(IPCOMP,3) = STEPCOUNT
      PCAOUT(IPCOMP,4) = MIXCOUNT
      PCAOUT(IPCOMP,5) = NSECAVG
      PCAOUT(IPCOMP,6) = QIMMERSED_HX

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
     &   ' stratified_tank_2hx_wrapper '

C---- Return to the calling module

      RETURN
      END
