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

C===================== stratified_tank_1hx_static ======================
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

      SUBROUTINE stratified_tank_1hx_static(IPCOMP)

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
      INTEGER I,J,K
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


C========================================================================================================INIZIO ROBERTA
      INTEGER nX                ! Number of cells in radial direction [-]
      INTEGER nY                ! Number of cells in axial direction [-]
      INTEGER nXnY              ! Number of total cells [-]
C      INTEGER nCells
      INTEGER NPCM              ! Number of PCM modules
      REAL*4 PCMrhos            ! Density of solid [kg/m^3]
      REAL*4 PCMrhol            ! Density of liquid [kg/m^3]
      REAL*4 cs                 ! Specific heat of solid [J/(kg K)]
      REAL*4 cl                 ! Specific heat of liquid [J/(kg K)]
      REAL*4 heatLat            ! Latent heat [J/kg]
      REAL*4 cndcts             ! Conductivity into solid [W/(m K)]
      REAL*4 cndctl             ! Conductivity into liquid [W/(m K)]
      REAL*4 Tmi                ! Melting temperature  [°C]
      INTEGER method            ! time integration method [-]
      REAL*4 e                  ! Phase change range [°C]

      INTEGER NPCMsec           ! Number of layers which the PCM cyl is divided into

      REAL*4 DPCMcyl            ! PCM cylinders diameter [m]
      REAL*4 HPCMcyl            ! Cylinder height [m]
      REAL*4 HPCMbot            ! Position of the cylinder bottom [m]
      REAL*4 HPCMtop            ! Position of the cylinder top [m]
      INTEGER NPCMbot           ! Tank node corresponding to the lower cylinder node
                                ! Tank node which the cylinder bottom is located in
      INTEGER NPCMtop           ! Tank node corresponding to the higher cylinder node
                                ! Tank node which the cylinder top is located in
      REAL*4 HeightNode         ! Tank CV height

      REAL*4 rp(mX)             ! Radius (axis-node distance) [m]
C========================================================================================================FINE ROBERTA


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

C========================================================================================================INIZIO ROBERTA
C---- PCM data 
      NPCM    = INT(ADATA(IPCOMP,19)) 
                                  ! Number of PCM modules       
      HPCMcyl = ADATA(IPCOMP,20)  ! Cylinder height (m)
      DPCMcyl = ADATA(IPCOMP,21)  ! Cylinder diameter (m)    
      HPCMbot = ADATA(IPCOMP,22)  ! Position of the ciylinder bottom (m)
      nX      = INT(ADATA(IPCOMP,23)) 
                                  ! Number of cells in radial direction (-)
      PCMrhos = ADATA(IPCOMP,24)  ! Density of solid (kg/m^3)
      PCMrhol = ADATA(IPCOMP,25)  ! Density of liquid (kg/m^3)
      cs      = ADATA(IPCOMP,26)  ! Specific heat of solid (J/(kg K))
      cl      = ADATA(IPCOMP,27)  ! Specific heat of liquid (J/(kg K))
      heatLat = ADATA(IPCOMP,28)  ! Latent heat (J/kg)
      cndcts  = ADATA(IPCOMP,29)  ! Conductivity into solid (W/(m K))
      cndctl  = ADATA(IPCOMP,30)  ! Conductivity into liquid [W/(m K)]
      Tmi     = ADATA(IPCOMP,31)  ! Phase change temperature (C)
      method  = INT(ADATA(IPCOMP,32))
                                  !time integration method (-)
      e       = ADATA(IPCOMP,33)  ! Phase change range (C)
C========================================================================================================FINE ROBERTA

C---- Check that number of nodes is within acceptable range
      IF (NNodes .GT. MAX_NNODES .OR. NNodes .LT. 1) THEN
        WRITE(IUOUT,*) ' stratified_tank_1hx_static: Number of nodes'
        WRITE(IUOUT,*) ' not within range 1 - ', MAX_NNODES
        STOP ' stratified_tank_1hx_static: unresolvable error'
      endif
C---- Check that parameters are properly defined
      IF (VOL .LE. 0.) THEN
        WRITE(IUOUT,*) ' stratified_tank_1hx_static: Tank volume'
        WRITE(IUOUT,*) ' is less than or equal to 0'
        STOP ' stratified_tank_1hx_static: unresolvable error'
      endif
      IF (HTANK .LE. 0.1) THEN
        WRITE(IUOUT,*) ' stratified_tank_1hx_static: Tank height'
        WRITE(IUOUT,*) ' is less than 0.1 m'
        STOP ' stratified_tank_1hx_static: unresolvable error'
      endif
      IF (U .LT. 0.) THEN
        WRITE(IUOUT,*) ' stratified_tank_1hx_static: Tank heat loss'
        WRITE(IUOUT,*) ' coefficient is negative'
        STOP ' stratified_tank_1hx_static: unresolvable error'
      endif
      IF (K_DESTRAT .LT. 0.) THEN
        WRITE(IUOUT,*) ' stratified_tank_1hx_static: destratification'
        WRITE(IUOUT,*) ' conductivity is negative'
        STOP ' stratified_tank_1hx_static: unresolvable error'
      endif
C---- Check that heights of inlets and outlets are within tank height
      IF (HIN1.GT.HTANK .OR. HOUT1.GT.HTANK .OR.
     &    HINHX1.GT.HTANK .OR. HOUTHX1.GT.HTANK) THEN
        WRITE(IUOUT,*) ' stratified_tank_1hx_static: Height of inlet'
        WRITE(IUOUT,*) ' or outlet greater than tank height'
        STOP ' stratified_tank_1hx_static: unresolvable error'
      ENDIF
C---- Check that immersed HX parameters are properly defined
      IF (DINHX1.GE.DOUTHX1) THEN
        WRITE(IUOUT,*) ' stratified_tank_1hx_static: Coil internal'
        WRITE(IUOUT,*) ' diameter >= external diameter.'
        STOP ' stratified_tank_1hx_static: unresolvable error'
      ENDIF
C---- Check that containment exists
      CALL ECLOSE(PCNTMF(IPCOMP),-99.00,0.001,CLOSEA)
      IF(CLOSEA) THEN
        WRITE(IUOUT,*) ' stratified_tank_1hx_static : A containment '
        WRITE(IUOUT,*) ' must be specified for component ',IPCOMP
        WRITE(IUOUT,*) ' and all components of the same type'
        STOP ' stratified_tank_1hx_static: unresolvable error'
      ENDIF ! Matches to IF(CLOSEA) THEN

C---- Check user specified number of controlled variables

      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM) THEN
        WRITE(ITU,*) ' stratified_tank_1hx_static warning: user ',
     &               ' specified wrong number of controlled ',
     &               ' variables '
        STOP ' stratified_tank_1hx_static: unresolvable error'
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
        WRITE(IUOUT,*) ' stratified_tank_1hx_static: '
        WRITE(IUOUT,*) ' connection error for component ',IPCOMP
        WRITE(IUOUT,*) '   should be ',NCONS,' water connection(s)'
        STOP ' stratified_tank_1hx_static: unresolvable error'
      ENDIF

C---- Component is well defined. Store IPCOMP in stratified tank common
C---- block, then set the node temperatures to their initial value
 
      N_STANK = N_STANK+1
      IF (N_STANK .GT. MAX_STANK) THEN
        WRITE(IUOUT,*) ' stratified_tank_1hx_static: '
        WRITE(IUOUT,*) ' too many stratified tanks defined'
        WRITE(IUOUT,*) ' maximum should be ',MAX_STANK
        STOP ' stratified_tank_1hx_static: unresolvable error'
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






C======================================================================================INIZIO AGGIUNTO ROBERTA

C---- Initialize pcm_module
      HPCMtop = HPCMbot+HPCMcyl

C      Tm = 0. !perchè messo nel common 


C---- Calculate default node height

      HeightNode = HTANK/NNODES


C---- Calculate PCM position

      NPCMbot=NINT(HPCMbot/HeightNode+0.5)
      NPCMtop=NINT((HPCMbot+HPCMcyl)/HeightNode+0.5)
      IF (HPCMtop.EQ.((NPCMtop-1)*HeightNode)) THEN
        NPCMtop = NPCMtop-1
      ENDIF

      IF (NPCM.GT.0) THEN
        IF ((HPCMbot+HPCMcyl).GT.HTank) THEN
          STOP 'geometric incongruity: lower cylinder(s) position!'
        ENDIF
      ENDIF


C-----PCM section height - from bottom to top

      HPCMsec(1,N_STANK) = FLOAT(NPCMbot)*HeightNode-HPCMbot

      NPCMsec = NPCMtop-NPCMbot+1
      DO I=2,NPCMsec-1
        HPCMsec(I,N_STANK) = HeightNode
      ENDDO

C      HPCMsec(NPCMsec,N_STANK) = NPCMtop*HeightNode-HPCMtop
      HPCMsec(NPCMsec,N_STANK) = HPCMtop-FLOAT((NPCMtop-1))*HeightNode
      IF (FLOAT(NPCMtop)*HeightNode.EQ.HPCMtop) THEN
        HPCMsec(NPCMsec,N_STANK) = HeightNode
      ENDIF


C-----distance among PCM nodes - from bottom to top
C-----skip in case of 1! cell in axial direction
      IF (nPCMsec.GT.1) THEN
        IF (nPCMsec.GT.2) THEN
          DO I=1,nPCMsec-1
            IF (I.EQ.1) THEN! 1. element: distance between bottom node and node north
              IF (HPCMbot.EQ.((NPCMbot-1)*HeightNode)) THEN 
                dPCMnode(1,N_STANK) = HeightNode
              ELSE 
                dPCMnode(1,N_STANK) = HeightNode/2+
     &                               ((NPCMbot*HeightNode)-HPCMbot)/2
              ENDIF
            ELSEIF(I.EQ.nPCMsec-1) THEN ! last element: distance between top node and node south
              IF (HPCMtop.EQ.(NPCMtop*HeightNode)) THEN
                dPCMnode(I,N_STANK) = HeightNode
              ELSE
                dPCMnode(I,N_STANK) = HeightNode/2+ 
     &                              (HPCMtop-(NPCMtop-1)*HeightNode)/2
              ENDIF
            ELSE
              dPCMnode(I,N_STANK) = HeightNode !generic element, generic distance
            ENDIF
          ENDDO
        ELSE !case of 2 PCM sections
          IF ((HPCMbot.EQ.((NPCMbot-1)*HeightNode))
     &         .AND.(HPCMtop.EQ.(NPCMtop*HeightNode))) THEN                  
            dPCMnode(1,N_STANK) = HeightNode
          ELSEIF ((HPCMbot.EQ.((NPCMbot-1)*HeightNode))
     &         .AND.(HPCMtop.NE.(NPCMtop*HeightNode)))  THEN
            dPCMnode(1,N_STANK) = HeightNode/2+ 
     &                             (HPCMtop-(NPCMtop-1)*HeightNode)/2
          ELSEIF ((HPCMtop.EQ.(NPCMtop*HeightNode))
     &         .AND.(HPCMbot.NE.((NPCMbot-1)*HeightNode))) THEN
            dPCMnode(1,N_STANK) = ((NPCMbot*HeightNode)-HPCMbot)
     &                        /2+HeightNode/2
          ELSE 
            dPCMnode(1,N_STANK) =(HPCMtop-(NPCMtop-1)*HeightNode)/2+
     &                           ((NPCMbot*HeightNode)-HPCMbot)/2
          ENDIF
        ENDIF
      ENDIF


! RIPRISTINARE!!!!!!!!!!!!!!!!!
C-----GRID
      dr(N_STANK) = DPCMcyl/(2*(nX-1))
! RIPRISTINARE!!!!!!!!!!!!!!!!!

      nY = NPCMsec
      nXnY = nX*nY ! TPCM_MOD and HPCM_MOD dimension 



C-----INITIAL CONDITIONS
      DO J=1, nX       
        DO i=1, nY
          K=(J-1)*NY+I
          TPCM_MOD(K,N_STANK) = TINIT-Tmi
        ENDDO
      ENDDO


      DO J=1, nX
        DO i=1, nY
          K=(J-1)*NY+I
          IF ((TINIT-Tmi) .LT. Tm) THEN ! cells initial enthalpy [J/kg]
            HPCM_MOD(K,N_STANK) = cs*(TINIT-Tmi)
          ELSE
            HPCM_MOD(K,N_STANK) = cl*(TINIT-Tmi-Tm)+
     &                               heatLat+cs*Tm
          ENDIF
        ENDDO
      ENDDO



! RIPRISTINARE!!!!!!!!!!!!!!!!!
C-----GEOMETRY

C-----PCM cells radius 

        J = nX ! central cell
        rp(nX) = 0.
        DO WHILE (J.GT.1) 
            J=J-1
            rp(J)=rp(J+1)+dr(N_STANK)
        ENDDO ! rp(1)=ext, rp(nX)=0=int

C-----lateral surface of PCM cells 

      DO I=1,nY
        Axb(I,N_STANK) = 2*pi*rp(1)*HPCMsec(I,N_STANK) ! border cells
      ENDDO 


      DO J=1,nX-1 
        DO I=1,nY
          K=(J-1)*nY+I
          Ax(K,N_STANK)=2*pi*HPCMsec(I,N_STANK)*(rp(J)-
     &                    dr(N_STANK)/2) ! generic cells
        ENDDO
      ENDDO

C-----top/bottom surface of PCM cells 

      Ay(1,N_STANK) = rp(1)*dr(N_STANK)*pi-dr(N_STANK)**2/4*pi ! external cell 
      DO J=2,nX-1
          Ay(J,N_STANK) = 2*dr(N_STANK)*rp(J)*pi ! generic cell
      ENDDO
      Ay(nX,N_STANK) = (dr(N_STANK)/2)**2*pi ! internal cell
   

C-----cells PCM volume 
      DO J=1,nX
        DO I=1,nY
          K=(J-1)*nY+I
          IF (J.EQ.1) THEN ! external cell 
            V(K,N_STANK) = pi*HPCMsec(I,N_STANK)*((rp(J))**2-
     &                     (rp(J)-dr(N_STANK)/2)**2) 
          ELSEIF (J.EQ.nX) THEN !internal cell 
            V(K,N_STANK) = pi*HPCMsec(I,N_STANK)*
     &                      (dr(N_STANK)/2)**2 
          ELSE !generic cell
            V(K,N_STANK)=pi*HPCMsec(I,N_STANK)*((rp(J)+
     &                   dr(N_STANK)/2)**2-(rp(J)-dr(N_STANK)/2)**2) 
          ENDIF
        ENDDO              
      ENDDO
! RIPRISTINARE!!!!!!!!!!!!!!!!!



! CANCELLARE!!!!!!!!!!!!!
! C-----PCM cells radius 
! 
!       dr(N_STANK) = DPCMcyl/((2*nX)-1)
! 
!         rp(nX) = 0.
!         DO j=nX-1,1,-1
!           rp(j)=rp(j+1)+dr(N_STANK)
!         ENDDO
! 
! 
! C-----lateral surface of PCM cells 
! 
!       DO I=1,nY
!         Axb(I,N_STANK) = pi*DPCMcyl*HPCMsec(I,N_STANK) ! border cells
!       ENDDO 
! 
! 
!       DO J=1,nX-1 
!         DO I=1,nY
!           K=(J-1)*nY+I
!           Ax(K,N_STANK)=2*pi*HPCMsec(I,N_STANK)*
!      &                    (rp(J)-dr(N_STANK)/2) ! generic cells
!         ENDDO
!       ENDDO
! 
! C-----top/bottom surface of PCM cells 
! 
!       DO J=1,nX-1
!           Ay(J,N_STANK) = 2*dr(N_STANK)*rp(j)*pi ! generic cell
!       ENDDO
!       Ay(nX,N_STANK) = (dr(N_STANK)/2)**2*pi ! internal cell
!    
! 
! C-----cells PCM volume 
!       DO J=1,nX-1
!         DO I=1,nY
!           K=(J-1)*nY+I
!           V(K,N_STANK)= 2*dr(N_STANK)*rp(j)*pi*HPCMsec(I,N_STANK) ! generic cell
!         ENDDO              
!       ENDDO
!       J=nX
!       DO I=1,nY
!         K=(J-1)*nY+I
!         V(K,N_STANK)= (dr(N_STANK)/2)**2*pi*HPCMsec(I,N_STANK) ! internal cell
!       ENDDO
! !CANCELLARE!!!!!!!!!!!!!



C---- Assign PCM grid and position data
      NPCMDATA(1,N_STANK)=nX
      NPCMDATA(2,N_STANK)=nY
      NPCMDATA(3,N_STANK)=NPCMbot
      NPCMDATA(4,N_STANK)=NPCMtop 
C======================================================================================FINE AGGIUNTO ROBERTA

C---- Normal return
 
      RETURN    
      END

C===================== stratified_tank_1hx_coeff_gen ===================
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

      SUBROUTINE stratified_tank_1hx_coeff_gen(IPCOMP,COUT,ISTATS)

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
      INTEGER INOD3           ! Pointer to dummy node 3
      REAL TOUT1              ! Outlet temperature 1 [C]
      REAL TOUT2              ! Outlet temperature 2 [C]
      REAL TOUT3              ! Outlet temperature 3 [C]

C---- Trace output

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine ',
     &   ' stratified_tank_1hx_coeff_gen '

C---- Initialize pointers to inter-connection(s) ICON, and node(s) INOD.

      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,2,1)
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

!       OPEN(UNIT=93,FILE='CellsTempFromRoutine.dat', 
!      &                           FORM='FORMATTED', STATUS='UNKNOWN',
!      &                           ACCESS='APPEND')
!         WRITE(93, '(A)') "#call to wrapper"
!       CLOSE(93)
        CALL stratified_tank_1hx_wrapper(IPCOMP,TOUT1,TOUT2,TOUT3)

C Establish matrix equation self-coupling coefficients,
         COUT(1)=1.
         COUT(2)=1.
         COUT(3)=1.
C then matrix equation cross-coupling coefficients,
         COUT(4)=0.
         COUT(5)=0.
C and then present-time coefficients (ie. right hand sides)
         COUT(6)=TOUT1
         COUT(7)=TOUT2
         COUT(8)=TOUT3

C=======================================================================
C     Generate coefficients for 1st phase flow equation.
C=======================================================================

      ELSEIF(ISTATS.EQ.2) THEN

         COUT(1)=1.
         COUT(2)=1.
         COUT(3)=1.
         COUT(4)=-PCONDR(ICON1)
         COUT(5)=-PCONDR(ICON2)
         COUT(6)=0.
         COUT(7)=0.
         COUT(8)=0.

C=======================================================================
C     Generate coefficients for 2nd phase flow equation.
C=======================================================================

      ELSEIF(ISTATS.EQ.3) THEN

         COUT(1)=1.
         COUT(2)=1.
         COUT(3)=1.
         COUT(4)=-PCONDR(ICON1)
         COUT(5)=-PCONDR(ICON2)
         COUT(6)=0.
         COUT(7)=0.
         COUT(8)=0.

      ENDIF 

C-----------------------------------------------------------------------
C     End of calculation
C-----------------------------------------------------------------------

C---- Trace output

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
        WRITE(ITU,*) ' Component     ',IPCOMP,':'
        WRITE(ITU,*) ' 2 node (ISV=20) stratified tank '
        WRITE(ITU,*) ' with 1 immersed heat exchanger '
        WRITE(ITU,*) ' Matrix node(s) ',INOD1,', ',INOD2
        WRITE(ITU,*) ' Connection(s)  ',ICON1,', ',ICON2
        IF(ISTATS.EQ.1) THEN
          WRITE(ITU,*) ' DAY        =', IDYF
          WRITE(ITU,*) ' HOUR       =', IHRF
          WRITE(ITU,*) ' '
        ENDIF ! Matches IF(ISTATS.EQ.1)
        WRITE(ITU,*) 'Exiting subroutine stratified_tank_1hx_coeff_gen'
      ENDIF ! Matches IF(ITC.GT.0.AND.NSINC.GE.ITC ...

C---- Return to the calling module

      RETURN

      END

C===================== stratified_tank_1hx_wrapper =========================
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
      SUBROUTINE stratified_tank_1hx_wrapper(IPCOMP,TOUT1,TOUTHX1,TOUT2)

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
      REAL TOUTHX2              ! Outlet temperature of HX2 [C] (dummy variable)

C---- Local variables

      LOGICAL CLOSEA,CLOSEB,CLOSEC,CLOSED     
                                ! Auxiliary variable 
      REAL AUX                  ! Auxiliary variable
      REAL HIN1                 ! Height of inlet 1 [m]
      REAL HOUT1                ! Height of outlet 1 [m]
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

C     Outputs of model
      REAL QLOSS                ! Heat loss from tank to environment [J]
      REAL QIMMERSED_HX         ! Heat gain from immersed heat exchangers [J]
      INTEGER STEPCOUNT         ! Number of internal time steps
      INTEGER MIXCOUNT          ! Number of times mixing algorithm called
      INTEGER NSECAVG           ! Average number of mixed sections
      INTEGER IER               ! Error code

C     Dummy variables
      LOGICAL HASHX1            ! Indicates presence of HX1 (.TRUE.)
      LOGICAL HASHX2            ! Indicates presence of HX2 (.FALSE.)
      LOGICAL HASPCM            ! Indicates presence of PCM module (.FALSE.)
      REAL DUMMY                ! Dummy variable

C---- Functions
      REAL SHTFLD
      REAL RHOFLD
      REAL KWATER

C======================================================================================INIZIO AGGIUNTO ROBERTA
c---- PCM data
      INTEGER NPCM              ! Number of PCM modules
      REAL*4 PCMrhos            ! Density of solid [kg/m^3]
      REAL*4 PCMrhol            ! Density of liquid [kg/m^3]
      REAL*4 cs                 ! Specific heat of solid [J/(kg K)]
      REAL*4 cl                 ! Specific heat of liquid [J/(kg K)]
      REAL*4 heatLat            ! Latent heat [J/kg]
      REAL*4 cndcts             ! Conductivity into solid [W/(m K)]
      REAL*4 cndctl             ! Conductivity into liquid [W/(m K)]
      REAL*4 Tmi                ! Melting temperature  [°C]
      INTEGER method            ! time integration method [-]
      REAL*4 e                  ! Phase change range [°C]
c---- PCM geometrical data 
      REAL*4 HPCMcyl            ! Cylinder height [m]
      REAL*4 DPCMcyl            ! PCM cylinders diameter [m]
      REAL*4 HPCMbot            ! Position of the cylinder bottom [m]
      REAL*4 HPCMtop            ! Position of the cylinder top [m]

      INTEGER nX                ! Number of cells in radial direction [-]
      INTEGER nY                ! Number of cells in axial direction [-]
      INTEGER NPCMbot           ! Tank node corresponding to the lower cylinder node
                                ! Tank node which the cylinder bottom is located in
      INTEGER NPCMtop           ! Tank node corresponding to the higher cylinder node
                                ! Tank node which the cylinder top is located in
C======================================================================================FINE AGGIUNTO ROBERTA

C---- Trace output

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine ',
     &   ' stratified_tank_1hx_wrapper '

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

C======================================================================================INIZIO AGGIUNTO ROBERTA
C---- PCM data 
      NPCM    = INT(ADATA(IPCOMP,19)) 
                                  ! Number of PCM modules       
      HPCMcyl = ADATA(IPCOMP,20)  ! Cylinder height (m)
      DPCMcyl = ADATA(IPCOMP,21)  ! Cylinder diameter (m)    
      HPCMbot = ADATA(IPCOMP,22)  ! Position of the ciylinder bottom (m)
      nX      = INT(ADATA(IPCOMP,23)) 
                                  ! Number of cells in radial direction (-)
      PCMrhos = ADATA(IPCOMP,24)  ! Density of solid (kg/m^3)
      PCMrhol = ADATA(IPCOMP,25)  ! Density of liquid (kg/m^3)
      cs      = ADATA(IPCOMP,26)  ! Specific heat of solid (J/(kg K))
      cl      = ADATA(IPCOMP,27)  ! Specific heat of liquid (J/(kg K))
      heatLat = ADATA(IPCOMP,28)  ! Latent heat (J/kg)
      cndcts  = ADATA(IPCOMP,29)  ! Conductivity into solid (W/(m K))
      cndctl  = ADATA(IPCOMP,30)  ! Conductivity into liquid [W/(m K)]
      Tmi     = ADATA(IPCOMP,31)  ! Phase change temperature (C)
      method  = INT(ADATA(IPCOMP,32))
                                  !time integration method (-)
      e       = ADATA(IPCOMP,33)  ! Phase change range (C)

C---- Initialize pcm_module
      HPCMtop = HPCMbot+HPCMcyl
C======================================================================================FINE AGGIUNTO ROBERTA


C---- Initialize pointers to inter-connection(s) ICON, and node(s) INOD.

      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,2,1)

C---- Identify variables
C     HX mass flow rate is sum of water and secondary fluid mass flow rates 
C     (secondary fluid is for glycol)

      TIN1=CONVAR(ICON1,1)
      TIN2=0.
      TINHX1=CONVAR(ICON2,1)
      MDOT1=PCONDR(ICON1)*CONVAR(ICON1,2)
      MDOT2=0.
      MDOTHX1=PCONDR(ICON2)*(CONVAR(ICON2,2)+CONVAR(ICON2,3))

C---- Calculate mass percentage of glycol in HX fluid
      AUX = CONVAR(ICON2,2)+CONVAR(ICON2,3)
      CALL ECLOSE(AUX,0.0,1.0e-15,CLOSEC)
      if (.NOT.CLOSEC) THEN
        GLYCOLHX1 = 100.*CONVAR(ICON2,3)/AUX
      ELSE
        GLYCOLHX1 = 0.
      ENDIF

C---- Check that tank fluid does not have glycol
C     Enabling the tank to be filled with glycol would not be difficult to implement, 
C     however this is not done for now as such systems are probably very infrequent
      CALL ECLOSE(CONVAR(ICON1,3),0.0,1.0e-15,CLOSED)
      IF (.NOT.CLOSED) THEN
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



C=================================================================INIZIO AGGIUNTO ROBERTA
C---- Assign PCM grid and position data

      nX=NPCMDATA(1,I_STANK)
      nY=NPCMDATA(2,I_STANK)
      NPCMbot=NPCMDATA(3,I_STANK)
      NPCMtop=NPCMDATA(4,I_STANK)
C=================================================================FINE AGGIUNTO ROBERTA


C---- Calculate heat capacitance and density of fluid
C     Use average temperature of tank at previous time step

      CP=SHTFLD(3,TAVGP_STANK(I_STANK))
      RHO=RHOFLD(3,TAVGP_STANK(I_STANK))
      K=KWATER(TAVGP_STANK(I_STANK))

C---- Call stratified tank function

C=================================================================INIZIO AGGIUNTO ROBERTA
      HASPCM=.FALSE.
      IF (NPCM .GT. 0) HASPCM=.TRUE.

C---- If tank contains PCM cylinders subtract their volume
      IF (HASPCM) THEN
        VOL = VOL - NPCM*HPCMcyl*DPCMcyl**2/4*PI
      ENDIF
C=================================================================FINE AGGIUNTO ROBERTA

      HASHX1=.TRUE.
      HASHX2=.FALSE.
      DUMMY=0.
      CALL stratified_tank_calc(VOL, H, P,
     &  HIN1, 0., HOUT1, 0.5*H, U, K_DESTRAT, NSTEPS, NNODES,
     &  HASHX1, 
     &  HINHX1, HOUTHX1, DINHX1, DOUTHX1, DCOILHX1, PITCHHX1, KHX1,
     &  HASHX2, 
     &  DUMMY, DUMMY, DUMMY, DUMMY, DUMMY, DUMMY, DUMMY,
     &  TIMSEC, TIN1, TIN2, MDOT1, MDOT2, TCONT, CP, K,
     &  RHO, TBOIL, TP_STANK(1,I_STANK),
     &  TINHX1, MDOTHX1, GLYCOLHX1,
     &  DUMMY, DUMMY, DUMMY, 
     &  TOUT1, TOUT2, TAVGF_STANK(I_STANK), TOUTHX1, TOUTHX2, 
     &  QLOSS, QIMMERSED_HX,
     &  STEPCOUNT, MIXCOUNT, NSECAVG, 
     &  TF_STANK(1,I_STANK),IER,
     &  HPCM_MOD(1,I_STANK),TPCM_MOD(1,I_STANK),!====================================INIZIO AGGIUNTO ROBERTA
     &  Axb(1,I_STANK),Ax(1,I_STANK),Ay(1,I_STANK),
     &  V(1,I_STANK),
     &  nX,nY,NPCMbot,NPCMtop,dr(I_STANK),
     &  PCMrhos,PCMrhol,cs,cl,heatLat,cndcts,cndctl,Tmi,method,e,
     &  HPCMcyl,DPCMcyl,HPCMbot,HPCMtop, NPCM, HASPCM,
     &  DPCMNODE(1,I_STANK))!============================================FINE AGGIUNTO ROBERTA

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
     &   ' stratified_tank_1hx_wrapper '

C---- Return to the calling module

      RETURN
      END
