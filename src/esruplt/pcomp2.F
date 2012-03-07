C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

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

C **********************************************************************
C PCOMP2 of ESRUbps
C
C For an overview of functions and plant component sub-routines
C currently available, see "pcomp1.f"
C The following plant component sub-routines are collected here:
C  CMP20C   1 node (ISV=20) WCH boiler
C  CMP21C   2 node (ISV=20) WCH radiator
C  CMP22C   1 node (ISV=20) WCH pipe
C  CMP23C   1 node (ISV=20) WCH converging 2-leg junction
C  CMP24C   1 node (ISV=20) WCH pump
C  CMP25C   2 node (ISV=20) WCH boiler with on/off control
C  CMP26C   2 node (ISV=20) WCH boiler with aquastat control
C  CMP27C   8 node (ISV=20) WCH radiator
C  CMP28C   1 node (ISV=9)  Oil-filled electric panel radiator
C  CMP29C   1 node (ISV=20) WCH flow control valve 
C  CMP31C   2 node (ISV=20) WCH calorifier
C  CMP32C   2 node (ISV=20) WCH generic liquid/liquid heat exchanger
C  CMP33C   2 node (ISV>20) WCH generic gas/liquid heat exchanger
C  CMP34C   3 node (ISV>20) WCH storage water heater
C  CMP35C   2 node (ISV=20) WCH basic radiator
C  CMP36C   2 node (ISV=20) WCH basic chiller or heat pump
C  CMP37C   1 node (ISV=20) WCH water/air heat rejector
C  CMP38C   1 node (ISV=20) WCH converging multi-leg junction    
C                           (up to 10 connections)
C  CMP39C   1 node (ISV=20) WCH pump (mass flow control)
C  CMP42C   2 node (ISV=20) WCH modulating boiler
C  CMP44C   2 node (ISV=20) WCH exponent model radiator
C  CMP45C   1 node (ISV=20) Air source heat pump connection to WCH
C  CMP50C   1 node (ISV=29) WCH thermostatic radiator valve
C  CMP51C   1 node (ISV=29)     mechanical room thermostat
c  CMP79C   1 node (ISV=20) WCH stochastic hot water draw
C  CMP90C   2 node (ISV>19)     air & water temperature source
C  CMP91C   2 node (ISV>19)     imaginary building-like plant load
C                               acting on heat and mass balances
C  TYPE6    TRNSYS type component for WCH boiler with aquastat control
C
C
C ******************** CMP20C ********************
C
C CMP20C generates for plant component IPCOMP with plant db code 200 ie.
C 1 node (ISV=20) WCH boiler & flux control
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C            3 UA modulus (W/K)
C     BDATA: none
C     CDATA: 1 Heating duty (W)
C
      SUBROUTINE CMP20C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PTIME/PTIMEP,PTIMEF
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

      common/pcnam/pcname(mpcom)       ! Plant component names
      character*15 pcname


      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      character outs*124
      character*128  H3K_rep_NAME      ! H3Kreporting object name
      character*128  root_name         ! root name of component node
      character*128  hold_name         ! temporary variable for string manipulation

      logical closea

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP20C'

C Check control data for relevant balance type
      IF(ISTATS.EQ.1.AND.CDATA(IPCOMP,1).LT.0.) THEN
         CALL DAYCLK(IDYP,PTIMEF,IUOUT)
         WRITE(outs,*) ' CMP20C: invalid control data for component ',
     &                  IPCOMP,' : ',CDATA(IPCOMP,1)
         call edisp(iuout,outs)
         call edisp(iuout,' CMP20C: unresolvable error.')
         call epwait
         call epagend
         STOP
      END IF

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      INOD1=NPCDAT(IPCOMP,9)

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C First initialize UA modulus (for calculation of containment heat loss)
         UA=ADATA(IPCOMP,3)
         call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
         IF(closea) UA=0.

C Then initialize added heat Q
         Q=CDATA(IPCOMP,1)

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(3,CONVAR(ICON1,1))

C Calculate current component time-constant TC
         TC(IPCOMP)=CM/AMAX1(SMALL,(C1+UA))

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
         IF(IMPEXP.EQ.1) THEN
            ALPHA=1.
         ELSE IF(IMPEXP.EQ.2) THEN
            ALPHA=RATIMP
         ELSE IF(IMPEXP.EQ.3) THEN
            IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
               ALPHA=1.
            ELSE
               ALPHA=RATIMP
            END IF
         ELSE IF(IMPEXP.EQ.4) THEN
            CM=0.
            ALPHA=1.
         END IF

C Establish matrix equation self- and cross-coupling coefficients
         COUT(1)=ALPHA*(-C1-UA)-CM/TIMSEC
         COUT(2)=ALPHA*C1
C and then present-time coefficient (ie. right hand side)
         COUT(3)=((1.-ALPHA)*(PCRP(ICON1)+PUAP(INOD1))
     &              -CM/TIMSEC)*CSVP(INOD1,1)
     &             +(1.-ALPHA)*(-PCRP(ICON1))*PCTP(ICON1)
     &             -ALPHA*UA*PCNTMF(IPCOMP)
     &             -(1.-ALPHA)*PUAP(INOD1)*PCNTMP(IPCOMP)
     &             -ALPHA*Q-(1.-ALPHA)*PCQP(INOD1)

C Store "environment" variables future values
         PUAF(INOD1)=UA
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCRF(ICON1)=C1
         PCQF(INOD1)=Q

C 1st phase mass (ie. water) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=-PCONDR(ICON1)
         COUT(3)=0.

C 2nd phase mass (ie. vapour) balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=0.
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=20) WCH boiler & flux control'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' UA     = ',UA,' (W/K)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
            WRITE(ITU,*) ' CDATA  = ',CDATA(IPCOMP,1)
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=3
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP20C'

C------------------------------------------------------
C XML output
C------------------------------------------------------

C.....Get component name
      hold_name = pcname(IPCOMP) 
C.....Format string as: 'plant/NAME/misc_data'

       WRITE(root_name,'(A,A,A)') 
     &     'plant/',
     &     hold_name(1:lnblnk(hold_name)),
     &     '/misc_data'

       WRITE(H3K_rep_NAME,'(A,A)') 
     &     root_name(1:lnblnk(root_name)), '/Q_added_heat'

       call add_to_xml_reporting(
     &                   Q,
     &                   H3K_rep_name,
     &                   'units',
     &                   '(W)',
     &                   'Heat added to boiler' )


      RETURN
      END

C ******************** CMP21C ********************

C CMP21C generates for plant component IPCOMP with plant db code 210 ie.
C 2 node (ISV=20) WCH radiator
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C     BDATA: 1 Radiator exponent (-)
C            2 Nominal heat emission of radiator (W)
C            3 Nominal supply temperature (C)
C            4 Nominal exit temperature (C)
C            5 Nominal environment temperature (C)
C            6 Index of coupled building zone (-)
C            7 Number of walls used for defining Te (-)
C            8 Index of 1st wall for defining Te (-)
C            9 Weighting factor for 1st wall when defining Te (-)
C           10 Index of 2nd wall for defining Te (-)
C           11 Weighting factor for 2nd wall when defining Te (-)
C           12 etc.
C     CDATA: none

C     PCDATF/P
C            1 Inter-node fluid heat capacity rate (W/K)

      SUBROUTINE CMP21C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

      COMMON/C6/INDCFG
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      logical closea
      logical bNoFlow

      real fQ_node1_frac
      real fQ_node2_frac

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP21C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C Initialize the nominal log mean temperature difference DTLM0
         TS0=BDATA(IPCOMP,3)
         TX0=BDATA(IPCOMP,4)
         TE0=BDATA(IPCOMP,5)
         call eclose(((TS0-TE0)/(TX0-TE0)),1.00,0.001,closea)
         IF(ABS(TS0-TE0).LT.SMALL.OR.ABS(TX0-TE0).LT.SMALL
     &      .OR.(TS0-TE0)/(TX0-TE0).LT.SMALL
     &      .OR.closea) THEN
C     &      .OR.(TS0-TE0)/(TX0-TE0).EQ.1.0) THEN
           DTLM0=(TS0+TX0)/2.-TE0
         ELSE
           DTLM0=(TS0-TX0)/ALOG((TS0-TE0)/(TX0-TE0))
         END IF

C Then evaluate the current environment temperature
         IF(INDCFG.EQ.2.OR.NINT(BDATA(IPCOMP,6)).EQ.0) THEN
            call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
            IF(closea) THEN
               TE=BDATA(IPCOMP,5)
            ELSE
               TE=PCNTMF(IPCOMP)
            END IF
         ELSE
            IZ=INT(BDATA(IPCOMP,6))
            SUMT=TFA(IZ)
            NW=int(BDATA(IPCOMP,7))
            SUMW=1.
            IF(NW.GE.1.) THEN
               DO 10 IW=1,NW
                  W=BDATA(IPCOMP,7+IW*2)
                  SUMW=SUMW+W
                  SUMT=SUMT+W*TFS(IZ,NINT(BDATA(IPCOMP,6+IW*2)))
   10          CONTINUE
            END IF
            TE=SUMT/SUMW
         END IF
C And the current log mean temperature difference DTLM based on
C current node 1 temp., so mark node 1 temperature for iteration
         ICSV(INOD1,1)=1
         CSVI(INOD1,1)=CSVF(INOD1,1)
         TS=CSVF(INOD1,1)
         TX=CSVF(INOD2,1)
         call eclose(((TS-TE)/(TX-TE)),1.00,0.001,closea)
         IF(ABS(TS-TE).LT.SMALL.OR.ABS(TX-TE).LT.SMALL
     &      .OR.(TS-TE)/(TX-TE).LT.SMALL
     &      .OR.closea) THEN
C     &      .OR.(TS-TE)/(TX-TE).EQ.1.0) THEN
           DTLM=(TS+TX)/2.-TE
         ELSE
           DTLM=(TS-TX)/ALOG((TS-TE)/(TX-TE))
         END IF

C Then calculate radiator heat emission Q
         DTR=AMAX1(SMALL,(DTLM/DTLM0))
         Q=BDATA(IPCOMP,2)*DTR**BDATA(IPCOMP,1)

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)/2.
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(3,CONVAR(ICON1,1))
C Inter-node fluid heat capacity rate (W/K)
         PCDATF(IPCOMP,1)=CSVF(INOD1,2)*SHTFLD(3,CSVF(INOD1,1))

C Determine if there's fluid flow in the raditor
         call eclose  ( PCONDR(ICON1)*CONVAR(ICON1,2),
     &                  0.0, 1.0E-05, bNoFlow )

C Modification: 2007-11-19 (aferguso)
C Hensen's original model appended all of the heat emission to the
C second node's heat balance. Hensen (1991) writes:
C
C  " The heat transfer from the radiator to the environment
C    is assumed to take place between the two nodes, and will
C    therefore appear only in the energy balance of the second
C    node (hence, this model still needs refinements because
C    it suffers from the same problem as indicated before for
C    the boiler models: ie no heat loss from the first node
C    when the water flow rate approaches zero). "
C
C Since the LMTD method uses the first node temperature to compute
C compute heat transfer from the radiator, this defect is more severe
C than Hensen suggests. When there is no flow to the radiator, the
C model will:
C
C   - incorrectly compute the resulting heat transfer, and
C
C   - predict outlet temperatures well below the environment
C     temperature (and often below zero).
C
C To correct this behavior, we'll modify the node heat balance
C equations depending on whether there's flow through the
C radiator.
C
C   - If there's flow through the radiator, we'll use the
C     same energy balance equations that Hensen originally
C     proposed (that is, all of the heat transfer is appended
C     to the second node energy balance).
C
C   - Otherwise, we'll divide the computed heat transfer between
C     the first and second nodes. This will cause both nodes to
C     approach the the environmental temperaute as they cool.
C
C
C Under no-flow conditions, the heat transfer will be apportioned
C according to the temperature differences between the nodes
C and the enviroment:
C
C                           [node 1 temp ] - [env. temp]
C    [node 1 %] = ------------------------------------------------
C                 [node 1 temp ] + [node 2 temp] - 2 * [env. temp]
C
C                           [node 2 temp ] - [env. temp]
C    [node 2 %] = ------------------------------------------------
C                 [node 1 temp ] + [node 2 temp] - 2 * [env. temp]
C
C We must also ensure the resulting heat transfer does not cool
C the nodes below the enviroment temperature --- that is:
C
C
C                       [heat capacity] * ([node 1 temp] - [env. temp])
C   [Q] * [node 1 %] <= -----------------------------------------------
C                                  [time-step duration]
C
C                       [heat capacity] * ([node 2 temp] - [env. temp])
C   [Q] * [node 2 %] <= -----------------------------------------------
C                                  [time-step duration]
C
C This modification ignores the central issue --- the model's LMTD 
C basis is not valid under unsteady conditions. But the model's heat
C transfer predictions when water is flowing through the radiator are
C unaffected, and with these changes it does provide sensible predictions
C under no-flow conditions.
C
C Reference:
C
C Hensen J L M (1991) 'On the thermal interaction of building
C     structure and heating and ventilating system', PhD Thesis,
C     Eindhoven University of Technology.

        if ( bNoFlow ) then

          fQ_node1_frac = (TS-TE) / ( TS + TX - 2.0 * TE)

          fQ_node2_frac = (TX-TE) / ( TS + TX - 2.0 * TE)

C Make sure Resulting heat transfer does not cool nodes below environment
C temperature
          if ( TS - Q * fQ_node1_frac * TIMSEC / CM < TE ) then

            fQ_node1_frac = CM * (TS-TE) / TIMSEC / Q

          endif

C Make sure Resulting heat transfer does not cool nodes below environment
C temperature
          if ( TX - Q * fQ_node2_frac * TIMSEC / CM < TE ) then

            fQ_node2_frac = CM * (TX-TE) / TIMSEC / Q

          endif


        else 

          fQ_node1_frac = 0.0
          fQ_node2_frac = 1.0

        endif



C Calculate current component time-constant TC
         TC(IPCOMP)=AMAX1(
     &        CM/AMAX1(SMALL,(C1)),
     &        CM/AMAX1(SMALL,(PCDATF(IPCOMP,1)+
     &            Q*(fQ_node1_frac + fQ_node2_frac)
     &            /AMAX1(SMALL,AMAX1(SMALL,(TS-TE))))))

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
         IF(IMPEXP.EQ.1) THEN
            ALPHA=1.
         ELSE IF(IMPEXP.EQ.2) THEN
            ALPHA=RATIMP
         ELSE IF(IMPEXP.EQ.3) THEN
            IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
               ALPHA=1.
            ELSE
               ALPHA=RATIMP
            END IF
         ELSE IF(IMPEXP.EQ.4) THEN
            CM=0.
            ALPHA=1.
         END IF

C Establish matrix equation self-coupling coefficients,
C  node 1
         COUT(1)=ALPHA*(-C1)-CM/TIMSEC
C  node 2
         COUT(2)=ALPHA*PCDATF(IPCOMP,1)
         COUT(3)=ALPHA*(-PCDATF(IPCOMP,1))-CM/TIMSEC
C then matrix equation cross-coupling coefficient,
         COUT(4)=ALPHA*C1
C and then present-time coefficients (ie. right hand sides). Scale
C future-time-row Q value by respecitve node fraction.
         COUT(5)=((1.-ALPHA)*PCRP(ICON1)-CM/TIMSEC)*CSVP(INOD1,1)
     &           +(1.-ALPHA)*(-PCRP(ICON1))*PCTP(ICON1)
     &           +fQ_node1_frac* ALPHA*Q +(1.-ALPHA)*PCQP(INOD1)
         COUT(6)=(1.-ALPHA)*(-PCDATP(IPCOMP,1))*CSVP(INOD1,1)
     &           +((1.-ALPHA)*PCDATP(IPCOMP,1)-CM/TIMSEC)*CSVP(INOD2,1)
     &           +fQ_node2_frac* ALPHA*Q+(1.-ALPHA)*PCQP(INOD2)


C Store "environment" variables future values. Scale Q value by
C node fractions.
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCRF(ICON1)=C1
         PCQF(INOD1)=Q * fQ_node1_frac
         PCQF(INOD2)=Q * fQ_node2_frac
 
C Establish "containment loss" data. Scale Q value by sum
C of node fractions. 
         QDATA(IPCOMP)=ALPHA*Q* ( fQ_node1_frac + fQ_node2_frac )
     &       +(1.-ALPHA)*PCQP(INOD2)

C Establish additional output variables
         PCAOUT(IPCOMP,1)=TE
         PCAOUT(IPCOMP,2)=DTLM
         PCAOUT(IPCOMP,3)=Q

C 1st phase mass (ie. water) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=-1.
         COUT(3)=1.
         COUT(4)=-PCONDR(ICON1)
         COUT(5)=0.
         COUT(6)=0.

C 2nd phase mass (ie. "vapour") balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=1.
         COUT(4)=0.
         COUT(5)=0.
         COUT(6)=0.
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 2 node (ISV=20) WCH radiator'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' TS     = ',TS,' (C)'
            WRITE(ITU,*) ' TX     = ',TX,' (C)'
            WRITE(ITU,*) ' TE     = ',TE,' (C)'
            WRITE(ITU,*) ' DTLM   = ',DTLM,' (K)'
            WRITE(ITU,*) ' Q      = ',
     &            Q*(fQ_node1_frac + fQ_node2_frac ),' (W)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=6
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP21C'

      RETURN
      END

C ******************** CMP22C ********************

C CMP22C generates for plant component IPCOMP with plant db code 220 ie.
C 1 node (ISV=20) WCH pipe
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C            3 UA modulus from wall to environment (W/K)
C            4 Hydraulic diameter of pipe (m)
C            5 Length of pipe (m)
C     BDATA: none
C     CDATA: none

      SUBROUTINE CMP22C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      logical closea,closeb

      PI = 4.0 * ATAN(1.0)

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP22C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      INOD1=NPCDAT(IPCOMP,9)

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C First initialize UA modulus (for calculation of containment heat loss)
         UA=ADATA(IPCOMP,3)
         call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
         IF(closea) THEN
            UA=0.
         ELSE
            DH=ADATA(IPCOMP,4)
            PL=ADATA(IPCOMP,5)
            TW=CONVAR(ICON1,1)
            WMFR=PCONDR(ICON1)*CONVAR(ICON1,2)
            call eclose(WMFR,0.00,0.0000001,closeb)
            IF(.NOT.closeb) THEN
              VW=WMFR/(RHOFLD(3,TW)*ADATA(IPCOMP,6))
              HW=1400.*(1.+0.015*TW)*VW**0.8*DH**(-0.2)
              UA=1./(1./(HW*PI*DH*PL)+1./UA)
              WMCP=WMFR*SHTFLD(3,CONVAR(ICON1,1))
              UA=WMCP*(EXP(UA/WMCP)-1.)
            ENDIF
         END IF

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(3,CONVAR(ICON1,1))

C Calculate current component time-constant TC
         TC(IPCOMP)=CM/AMAX1(SMALL,(C1+UA))

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
         IF(IMPEXP.EQ.1) THEN
            ALPHA=1.
         ELSE IF(IMPEXP.EQ.2) THEN
            ALPHA=RATIMP
         ELSE IF(IMPEXP.EQ.3) THEN
            IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
               ALPHA=1.
            ELSE
               ALPHA=RATIMP
            END IF
         ELSE IF(IMPEXP.EQ.4) THEN
            CM=0.
            ALPHA=1.
         END IF

C Establish matrix equation self- and cross-coupling coefficients
         COUT(1)=ALPHA*(-C1-UA)-CM/TIMSEC
         COUT(2)=ALPHA*C1
C and then present-time coefficient (ie. right hand side)
         COUT(3)=((1.-ALPHA)*(PCRP(ICON1)+PUAP(INOD1))
     &              -CM/TIMSEC)*CSVP(INOD1,1)
     &             +(1.-ALPHA)*(-PCRP(ICON1))*PCTP(ICON1)
     &             -ALPHA*UA*PCNTMF(IPCOMP)
     &             -(1.-ALPHA)*PUAP(INOD1)*PCNTMP(IPCOMP)

C Store "environment" variables future values
         PUAF(INOD1)=UA
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCRF(ICON1)=C1

C 1st phase mass (ie. water) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=-PCONDR(ICON1)
         COUT(3)=0.

C 2nd phase mass (ie. vapour) balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=0.
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=20) WCH pipe'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' UA     = ',UA,' (W/K)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=3
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP22C'

      RETURN
      END

C ******************** CMP23C ********************

C CMP23C generates for plant component IPCOMP with plant db code 230 ie.
C 1 node (ISV=20) WCH converging 2-leg junction
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C            3 UA modulus (W/K)
C     BDATA: none
C     CDATA: none

      SUBROUTINE CMP23C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      logical closea

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP23C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,1,2)
      INOD1=NPCDAT(IPCOMP,9)

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C First initialize UA modulus (for calculation of containment heat loss)
        UA=ADATA(IPCOMP,3)
        call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
        IF(closea) UA=0.0

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
        CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)
        C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(3,CONVAR(ICON1,1))
        C2=PCONDR(ICON2)*CONVAR(ICON2,2)*SHTFLD(3,CONVAR(ICON2,1))

C Calculate current component time-constant TC
        TC(IPCOMP)=CM/AMAX1(SMALL,(C1+C2+UA))

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
        IF(IMPEXP.EQ.1) THEN
          ALPHA=1.
        ELSE IF(IMPEXP.EQ.2) THEN
          ALPHA=RATIMP
        ELSE IF(IMPEXP.EQ.3) THEN
          IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
            ALPHA=1.
          ELSE
            ALPHA=RATIMP
          END IF
        ELSE IF(IMPEXP.EQ.4) THEN
          CM=0.
          ALPHA=1.
        END IF

C Establish matrix equation self- and cross-coupling coefficients
        COUT(1)=ALPHA*(-C1-C2-UA)-CM/TIMSEC
        COUT(2)=ALPHA*C1
        COUT(3)=ALPHA*C2
C and then present-time coefficient (ie. right hand side)
        COUT(4)=((1.-ALPHA)*(PCRP(ICON1)+PCRP(ICON2)+PUAP(INOD1))
     &              -CM/TIMSEC)*CSVP(INOD1,1)
     &             +(1.-ALPHA)*(-PCRP(ICON1))*PCTP(ICON1)
     &             +(1.-ALPHA)*(-PCRP(ICON2))*PCTP(ICON2)
     &             -ALPHA*UA*PCNTMF(IPCOMP)
     &             -(1.-ALPHA)*PUAP(INOD1)*PCNTMP(IPCOMP)

C Store "environment" variables future values
        PUAF(INOD1)=UA
        PCTF(ICON1)=CONVAR(ICON1,1)
        PCTF(ICON2)=CONVAR(ICON2,1)
        PCRF(ICON1)=C1
        PCRF(ICON2)=C2

C 1st phase mass (ie. water) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
        COUT(1)=1.
        COUT(2)=-PCONDR(ICON1)
        COUT(3)=-PCONDR(ICON2)
        COUT(4)=0.

C 2nd phase mass (ie. "vapour") balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=0.
         COUT(4)=0.
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=20) WCH converging 2-leg junction'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         WRITE(ITU,*) ' Connection(s)  ',ICON1,ICON2
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' C2     = ',C2,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' UA     = ',UA,' (W/K)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=4
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP23C'

      RETURN
      END

C ******************** CMP24C ********************

C CMP24C generates for plant component IPCOMP with plant db code 240 ie.
C 1 node (ISV=20) WCH pump
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C            3 UA modulus (W/K)
C     BDATA: 1 Rated absorbed power (W)
C            2 Rated volume flow rate (m^3/s)
C            3 Overall efficiency (-)
C     CDATA: 1 Volume flow rate (m^3/s)

      SUBROUTINE CMP24C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"
#include "net_flow.h"
#include "OffsiteUtilitiesPublic.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PTIME/PTIMEP,PTIMEF
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

      COMMON/FFN/IFLWN,ICFFS(MPCON)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)

      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

      common/pcnam/pcname(mpcom)       ! Plant component names
      character*15 pcname

C Electrical details for specified plant components
      common/pcelflg/ipcelf(mpcom)
      common/elpcp/NPEL,PFP(mpcom),IPFP(mpcom),PWRP(mpcom),
     &BVOLTP(mpcom),IPHP(mpcom)

      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      character outs*124
      logical closea

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP24C'

C Check control data for relevant balance type
      IF(ISTATS.EQ.2.AND.CDATA(IPCOMP,1).LT.0.) THEN
         CALL DAYCLK(IDYP,PTIMEF,IUOUT)
         WRITE(outs,*) ' CMP24C: invalid control data for component ',
     &                  IPCOMP,' : ',CDATA(IPCOMP,1)
         call edisp(iuout,outs)
         call edisp(iuout,' CMP24C: unresolvable error.')
         call epwait
         call epagend
         STOP
      END IF

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      INOD1=NPCDAT(IPCOMP,9)

      ICSV(INOD1,1)=1
      CSVI(INOD1,1)=CSVF(INOD1,1)
      ICSV(INOD1,2)=1
      CSVI(INOD1,2)=CSVF(INOD1,2)
      ICSV(INOD1,3)=1
      CSVI(INOD1,3)=CSVF(INOD1,3)

C Total mass flow rate into pump      
      TOTAL_MASS_FR = PCONDR(ICON1) * 
     &                (CONVAR(ICON1,2) + CONVAR(ICON1,3)) 

C Mass fraction of glycol in incoming flow      
      GLYCOL_MASS_FRACTION = 
     &            100. * CONVAR(ICON1,3) / (TOTAL_MASS_FR + SMALL)

C Find volume fraction of glycol in mixture of incoming flow
      GLYCOL_VOL_FRACTION = 
     &GLYCOL_VOL_FRACTION_FIND(GLYCOL_MASS_FRACTION,CONVAR(ICON1,1))

C Boiling temperature of glycol mixture
      TBOIL = GLYCOL_WATER_TBOIL(GLYCOL_VOL_FRACTION)

C Freezing temperature of glycol-water mixture
      TFREEZE = GLYCOL_WATER_TFREEZE(GLYCOL_VOL_FRACTION)

C Set incoming flow temperature using upper and lower bounds
      TEMP_INFLOW = MAX(TFREEZE,MIN(TBOIL,CONVAR(ICON1,1)))

C Set specific heat of incoming flow
      GLY_WATER_CP = GLYCOL_WATER_CP(GLYCOL_VOL_FRACTION,TEMP_INFLOW)
      
C Set temperature of pump using lower and upper bounds
      TEMP_PUMP = MAX(TFREEZE,MIN(TBOIL,CSVF(INOD1,1)))

C Density of fluid based on exit pump temperature
      GLY_WATER_DEN = GLYCOL_WATER_DENSITY(GLYCOL_VOL_FRACTION,
     &                 TEMP_PUMP)

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C First initialize UA modulus (for calculation of containment heat loss)
         UA=ADATA(IPCOMP,3)
         call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
         IF(closea) UA=0.

C Establish absorbed power E based on current water flow rate which might
C have been calculated by mfs
C In case of mfs E is based on flow rate, pressure *rise*, and efficiency
         IF(IFLWN.NE.0) THEN
            ICNN=ICFFS(ICON1)
            E=real((FLW1(ICNN)+FLW2(ICNN))
     &        *(PRES(NODNE(ICNN))-PRES(NODPS(ICNN)))
     &        /(BDATA(IPCOMP,3)*GLY_WATER_DEN))
         ELSE
            E=((CSVF(INOD1,2) + CSVF(INOD1,3))
     &         /(GLY_WATER_DEN*BDATA(IPCOMP,2)))**3
     &         *BDATA(IPCOMP,1)
         END IF

         PWRP(IPCOMP)=-ABS(E)
         IEMODEL=1
         CALL EMACH(IPCOMP,IEMODEL,PWRP(IPCOMP),PQ,PA)
         PWRQ=PQ

C Pass power consumption to Site Utilities facility
         fSUFuelEnergyUse( iElectricity, iUseUncatagorized) = ABS(E)
         call StorePltCompEnergyUse ( IPCOMP, fSUFuelEnergyUse )

C Report power consumption
         call add_to_xml_reporting(
     &          ABS(E),
     &          'plant/' // pcname(ipcomp)(1:iPltNameLen(ipcomp))
     &              //'/misc_data/energy_input',
     &          'units','(W)',
     &          'WCH pump: Energy input')
         

C Now Q is made up of all inefficiencies
         Q=(1.-BDATA(IPCOMP,3))*E

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)
         C1=TOTAL_MASS_FR*GLY_WATER_CP

C Calculate current component time-constant TC
         TC(IPCOMP)=CM/AMAX1(SMALL,(C1+UA))

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
         IF(IMPEXP.EQ.1) THEN
            ALPHA=1.
         ELSE IF(IMPEXP.EQ.2) THEN
            ALPHA=RATIMP
         ELSE IF(IMPEXP.EQ.3) THEN
            IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
               ALPHA=1.
            ELSE
               ALPHA=RATIMP
            END IF
         ELSE IF(IMPEXP.EQ.4) THEN
            CM=0.
            ALPHA=1.
         END IF

C Establish matrix equation self- and cross-coupling coefficients
         COUT(1)=ALPHA*(-C1-UA)-CM/TIMSEC
         COUT(2)=ALPHA*C1
C and then present-time coefficient (ie. right hand side)
         COUT(3)=((1.-ALPHA)*(PCRP(ICON1)+PUAP(INOD1))
     &              -CM/TIMSEC)*CSVP(INOD1,1)
     &             +(1.-ALPHA)*(-PCRP(ICON1))*PCTP(ICON1)
     &             -ALPHA*UA*PCNTMF(IPCOMP)
     &             -(1.-ALPHA)*PUAP(INOD1)*PCNTMP(IPCOMP)
     &             -ALPHA*Q-(1.-ALPHA)*PCQP(INOD1)

C Store "environment" variables future values
         PUAF(INOD1)=UA
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCRF(ICON1)=C1
         PCQF(INOD1)=Q

C Save plant additional output data.
          napdat(ipcomp)=2
          pcaout(ipcomp,1)=pwrp(ipcomp)
          pcaout(ipcomp,2)=pwrq

C 1st phase mass (ie. water) balance coefficients
C Note that if fluid mass flow solver active, source of mass zeroised
      ELSE IF(ISTATS.EQ.2) THEN
        COUT(1)=1.
        IF(IFLWN.EQ.0) THEN
          COUT(2)=0.
          COUT(3)=CDATA(IPCOMP,1)*GLY_WATER_DEN*
     &            (1.-GLYCOL_MASS_FRACTION/100.)
        ELSE
          COUT(2)=-PCONDR(ICON1)
          COUT(3)=0.
        END IF

C 2nd phase mass (ie. "vapour") balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
        COUT(1)=1.
        IF(IFLWN.EQ.0) THEN
          COUT(2)=0.
          COUT(3)=CDATA(IPCOMP,1)*GLY_WATER_DEN*
     &    GLYCOL_MASS_FRACTION/100.
        ELSE
          COUT(2)=-PCONDR(ICON1)
          COUT(3)=0.
        END IF
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=20) WCH pump'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' UA     = ',UA,' (W/K)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
            WRITE(ITU,*) ' Q      = ',Q,' (W)'
            WRITE(ITU,*) ' E      = ',E,' (W)'
            WRITE(ITU,*) ' E,cum. = ',E*TIMSEC,' (J)'
         ELSE IF(ISTATS.EQ.2) THEN
            WRITE(ITU,*) ' CDATA  = ',CDATA(IPCOMP,1)
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=3
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP24C'

      RETURN
      END

C ******************** CMP25C ********************

C CMP25C generates for plant component IPCOMP with plant db code 250 ie.
C 2 node (ISV=20) WCH boiler with on/off control
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C     BDATA: 1 Full load gas firing rate when boiler on (m^3/s)
C            2 Stand-by mode gas consumption relative to 1 (-)
C            3 Gas heating value at STP (J/m^3)
C            4 Full load water sided efficiency at Tc (-)
C            5 Tangent of efficiency curve for Tj < Tc (1/K)
C            6 Tangent of efficiency curve for Tj > Tc (1/K)
C            7 Stand-by loss at Tj = Te relative to 1 (-)
C            8 Tangent of stand-by loss curve (1/K)
C            9 Normalized start-stop loss (s)
C           10 Upper boiler temperature limit (C)
C     CDATA: 1 ON/OFF control signal (-)

C     PCDATF/P
C            1 Holds boiler actual ON/OFF state (-)
C            2 Inter-node fluid heat capacity rate (W/K)

      SUBROUTINE CMP25C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PTIME/PTIMEP,PTIMEF
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

      PARAMETER (SMALL=1.0E-20)
      REAL      COUT(MPCOE)
      character outs*124

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP25C'

C Check control data for relevant balance type
      IF(ISTATS.EQ.1.AND.
     &   (CDATA(IPCOMP,1).LT.0..OR.CDATA(IPCOMP,1).GT.1.)) THEN
         CALL DAYCLK(IDYP,PTIMEF,IUOUT)
         WRITE(outs,*) ' CMP25C: invalid control data for component ',
     &                  IPCOMP,' : ',CDATA(IPCOMP,1)
         call edisp(iuout,outs)
         call edisp(iuout,' CMP25C: unresolvable error.')
         call epwait
         call epagend
         STOP
      END IF

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C First calculate boiler efficiency ETA based on current node 1 temp.
C so mark node 1 temperature for iteration
         ICSV(INOD1,1)=1
         CSVI(INOD1,1)=CSVF(INOD1,1)
         IF(CSVF(INOD1,1).LT.50.) THEN
            TGALPH=BDATA(IPCOMP,5)
         ELSE
            TGALPH=BDATA(IPCOMP,6)
         END IF
         ETA=BDATA(IPCOMP,4)-TGALPH*(50.-CSVF(INOD1,1))

C Establish whether boiler is on or off (ie 1 or 0)
         IONOFF=int(CDATA(IPCOMP,1))
C Reset to off in case maximum temperature exceeded
         IF(CSVF(INOD2,1).GE.BDATA(IPCOMP,10)) IONOFF=0
C Then calculate heat input into the water PHIW for ON or OFF
         IF(IONOFF.EQ.1) THEN
            PHISB=0.
            FGAS=BDATA(IPCOMP,1)
C Adjust efficiency for start-stop losses if the boiler was off
C during the previous time-step
            IF(NINT(PCDATP(IPCOMP,1)).EQ.0) THEN
               ETA=ETA*(TIMSEC-BDATA(IPCOMP,9))/TIMSEC
            END IF
            PHIW=ETA*FGAS*BDATA(IPCOMP,3)
         ELSE
            FGAS=BDATA(IPCOMP,2)*BDATA(IPCOMP,1)
            PHIW=ETA*FGAS*BDATA(IPCOMP,3)
            PHISB=(BDATA(IPCOMP,7)
     &             +BDATA(IPCOMP,8)*(CSVF(INOD1,1)-PCNTMF(IPCOMP)))
     &            *BDATA(IPCOMP,1)*BDATA(IPCOMP,3)
         END IF

C Then calculate net heat input Q
         Q=PHIW-PHISB

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)/2.
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(3,CONVAR(ICON1,1))
C Boiler actual ON/OFF state PCDATF(IPCOMP,1) (-)
         PCDATF(IPCOMP,1)=IONOFF
C Inter-node fluid heat capacity rate (W/K)
         PCDATF(IPCOMP,2)=CSVF(INOD1,2)*SHTFLD(3,CSVF(INOD1,1))

C Calculate current component time-constant TC
         TC(IPCOMP)=AMAX1(
     &        CM/AMAX1(SMALL,(C1)),
     &        CM/AMAX1(SMALL,(PCDATF(IPCOMP,2)
     &                        +PHISB/(CSVF(INOD1,1)-PCNTMF(IPCOMP)))))

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
         IF(IMPEXP.EQ.1) THEN
            ALPHA=1.
         ELSE IF(IMPEXP.EQ.2) THEN
            ALPHA=RATIMP
         ELSE IF(IMPEXP.EQ.3) THEN
            IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
               ALPHA=1.
            ELSE
               ALPHA=RATIMP
            END IF
         ELSE IF(IMPEXP.EQ.4) THEN
            CM=0.
            ALPHA=1.
         END IF

C Establish matrix equation self-coupling coefficients,
C  node 1
         COUT(1)=ALPHA*(-C1)-CM/TIMSEC
C  node 2
         COUT(2)=ALPHA*PCDATF(IPCOMP,2)
         COUT(3)=ALPHA*(-PCDATF(IPCOMP,2))-CM/TIMSEC
C then matrix equation cross-coupling coefficient,
         COUT(4)=ALPHA*C1
C and then present-time coefficients (ie. right hand sides)
         COUT(5)=((1.-ALPHA)*PCRP(ICON1)-CM/TIMSEC)*CSVP(INOD1,1)
     &           +(1.-ALPHA)*(-PCRP(ICON1))*PCTP(ICON1)
         COUT(6)=(1.-ALPHA)*(-PCDATP(IPCOMP,2))*CSVP(INOD1,1)
     &           +((1.-ALPHA)*PCDATP(IPCOMP,2)-CM/TIMSEC)*CSVP(INOD2,1)
     &           -ALPHA*Q-(1.-ALPHA)*PCQP(INOD2)

C Store "environment" variables future values
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCRF(ICON1)=C1
         PCQF(INOD2)=Q

C Establish "containment loss" data
         QDATA(IPCOMP)=0.

C Establish additional output variables
         napdat(ipcomp)=5
         PCAOUT(IPCOMP,1)=IONOFF
         PCAOUT(IPCOMP,2)=FGAS
         PCAOUT(IPCOMP,3)=ETA
         PCAOUT(IPCOMP,4)=PHIW
         PCAOUT(IPCOMP,5)=PHISB

C 1st phase mass (ie. water) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=-1.
         COUT(3)=1.
         COUT(4)=-PCONDR(ICON1)
         COUT(5)=0.
         COUT(6)=0.

C 2nd phase mass (ie. "vapour") balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=1.
         COUT(4)=0.
         COUT(5)=0.
         COUT(6)=0.
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 2 node (ISV=20) WCH boiler & on/off control'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
            WRITE(ITU,*) ' CDATA  = ',CDATA(IPCOMP,1)
            WRITE(ITU,*) ' IONOFF = ',IONOFF,' (-)'
            WRITE(ITU,*) ' FGAS   = ',FGAS,' (m^3/s)'
            WRITE(ITU,*) ' ETA    = ',ETA,' (-)'
            WRITE(ITU,*) ' PHIW   = ',PHIW,' (W)'
            WRITE(ITU,*) ' PHISB  = ',PHISB,' (W)'
            WRITE(ITU,*) ' GASCO  = ',FGAS*TIMSEC,' (m^3)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=6
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP25C'

      RETURN
      END

C ******************** CMP26C ********************

C CMP26C generates for plant component IPCOMP with plant db code 260 ie.
C 2 node (ISV=20) WCH boiler with aquastat control
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C     BDATA: 1 fuel mass flow rate (kg/s)
C            2 volumetric ratio CO2 in flue gases during operation (-)
C            3 heat exchange coefficient water/flue gases in
C              nominal conditions (W/K)
C            4 sensitivity coefficient for 3 (-)
C            5 sensitivity coefficient for 3 (-)
C            6 heat loss coefficient to the environment if OFF (W/K)
C            7 heat loss increase to the environment if ON (W/K)
C            8 weighting factor for defining mean water temperature (-)
C            9 fuel nominal mass flow rate (kg/s)
C           10 water nominal mass flow rate (kg/s)
C           11 nominal ratio of CO2 in flue gases (-)
C           12 coefficient for defining specific heat flue gases (J/kgK)
C           13 coefficient for defining specific heat flue gases (J/kgK)
C           14 fuel specific heat (J/kgK)
C           15 fuel heating value (J/kg)
C     CDATA: 1 aquastat set point (C)
C            2 ON/OFF control signal (-)

C     PCDATF/P
C            1 Inter-node fluid heat capacity rate (W/K)

      SUBROUTINE CMP26C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PTIME/PTIMEP,PTIMEF
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

      DIMENSION XIN(5),PAR(18),OUT(11),INFO(10)

      PARAMETER (SMALL=1.0E-20)
      REAL      COUT(MPCOE)
      character outs*124

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP26C'

C Check control data for relevant balance type
      IF(ISTATS.EQ.1.AND.
     &   (CDATA(IPCOMP,1).LE.0..OR.
     &    CDATA(IPCOMP,2).LT.0..OR.CDATA(IPCOMP,2).GT.1.)) THEN
         CALL DAYCLK(IDYP,PTIMEF,IUOUT)
         WRITE(outs,*) ' CMP26C: invalid control data for component ',
     &                  IPCOMP,' : ',CDATA(IPCOMP,1)
         call edisp(iuout,outs)
         call edisp(iuout,' CMP26C: unresolvable error.')
         call epwait
         call epagend
         STOP
      END IF

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C Set up interface parameters and variables for TRNSYS type
         TWSU=CSVF(INOD1,1)
         WMFR=CSVF(INOD1,2)
         CPWT=SHTFLD(3,TWSU)
         XIN(1)=TWSU
         XIN(2)=WMFR
         XIN(3)=PCNTMF(IPCOMP)
         XIN(4)=CDATA(IPCOMP,1)
         XIN(5)=CDATA(IPCOMP,2)
         DO 10 I=1,13
            PAR(I)=BDATA(IPCOMP,I)
   10    CONTINUE
         PAR(14)=CPWT
         PAR(15)=BDATA(IPCOMP,14)
         PAR(16)=BDATA(IPCOMP,15)
C Now activate TRNSYS type representing static boiler model
C which will start from the current node 1 temperature
C so mark node 1 temperature for iteration
         ICSV(INOD1,1)=1
         CSVI(INOD1,1)=CSVF(INOD1,1)
         CALL TYPE6(TIME,XIN,OUT,T,DTDT,PAR,INFO)

C Then calculate net heat input Q
         Q=WMFR*CPWT*(OUT(1)-CSVF(INOD1,1))

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)/2.
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(3,CONVAR(ICON1,1))
C Inter-node fluid heat capacity rate (W/K)
         PCDATF(IPCOMP,1)=WMFR*CPWT

C Calculate current component time-constant TC
         TC(IPCOMP)=AMAX1(
     &        CM/AMAX1(SMALL,C1),
     &        CM/AMAX1(SMALL,PCDATF(IPCOMP,1)))

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
         IF(IMPEXP.EQ.1) THEN
            ALPHA=1.
         ELSE IF(IMPEXP.EQ.2) THEN
            ALPHA=RATIMP
         ELSE IF(IMPEXP.EQ.3) THEN
            IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
               ALPHA=1.
            ELSE
               ALPHA=RATIMP
            END IF
         ELSE IF(IMPEXP.EQ.4) THEN
            CM=0.
            ALPHA=1.
         END IF

C Establish matrix equation self-coupling coefficients,
C  node 1
         COUT(1)=ALPHA*(-C1)-CM/TIMSEC
C  node 2
         COUT(2)=ALPHA*PCDATF(IPCOMP,1)
         COUT(3)=ALPHA*(-PCDATF(IPCOMP,1))-CM/TIMSEC
C then matrix equation cross-coupling coefficient,
         COUT(4)=ALPHA*C1
C and then present-time coefficients (ie. right hand sides)
         COUT(5)=((1.-ALPHA)*PCRP(ICON1)-CM/TIMSEC)*CSVP(INOD1,1)
     &           +(1.-ALPHA)*(-PCRP(ICON1))*PCTP(ICON1)
         COUT(6)=(1.-ALPHA)*(-PCDATP(IPCOMP,1))*CSVP(INOD1,1)
     &           +((1.-ALPHA)*PCDATP(IPCOMP,1)-CM/TIMSEC)*CSVP(INOD2,1)
     &           -ALPHA*Q-(1.-ALPHA)*PCQP(INOD2)

C Store "environment" variables future values
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCRF(ICON1)=C1
         PCQF(INOD2)=Q

C Establish "containment loss" data
         QDATA(IPCOMP)=0.

C Establish additional output variables
         napdat(ipcomp)=12
         DO 12 IOUT=1,11
            PCAOUT(IPCOMP,IOUT)=OUT(IOUT)
   12    CONTINUE
         PCAOUT(IPCOMP,12)=Q

C 1st phase mass (ie. water) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=-1.
         COUT(3)=1.
         COUT(4)=-PCONDR(ICON1)
         COUT(5)=0.
         COUT(6)=0.

C 2nd phase mass (ie. "vapour") balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=1.
         COUT(4)=0.
         COUT(5)=0.
         COUT(6)=0.
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 2 node (ISV=20) WCH boiler & aquastat control'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
            WRITE(ITU,*) ' CDATA1 = ',CDATA(IPCOMP,1)
            WRITE(ITU,*) ' CDATA2 = ',CDATA(IPCOMP,2)
            WRITE(ITU,*) ' OUT,1  = ',OUT(1)
            WRITE(ITU,*) ' OUT,2  = ',OUT(2)
            WRITE(ITU,*) ' OUT,3  = ',OUT(3)
            WRITE(ITU,*) ' OUT,4  = ',OUT(4)
            WRITE(ITU,*) ' OUT,5  = ',OUT(5)
            WRITE(ITU,*) ' OUT,6  = ',OUT(6)
            WRITE(ITU,*) ' OUT,7  = ',OUT(7)
            WRITE(ITU,*) ' OUT,8  = ',OUT(8)
            WRITE(ITU,*) ' OUT,9  = ',OUT(9)
            WRITE(ITU,*) ' OUT,10 = ',OUT(10)
            WRITE(ITU,*) ' OUT,11 = ',OUT(11)
            WRITE(ITU,*) ' Q      = ',Q,' (W)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=6
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP26C'

      RETURN
      END

C ******************** CMP27C ********************

C CMP27C generates for plant component IPCOMP with plant db code 270 ie.
C 8 node (ISV=20) WCH radiator
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C     BDATA: 1 Radiator exponent (-)
C            2 Nominal heat emission of radiator (W)
C            3 Nominal supply temperature (C)
C            4 Nominal exit temperature (C)
C            5 Nominal environment temperature (C)
C            6 Index of coupled building zone (-)
C            7 Number of walls used for defining Te (-)
C            8 Index of 1st wall for defining Te (-)
C            9 Weighting factor for 1st wall when defining Te (-)
C           10 Index of 2nd wall for defining Te (-)
C           11 Weighting factor for 2nd wall when defining Te (-)
C           12 etc.
C     CDATA: none

C     PCDATF/P
C            1 Inter-node fluid heat capacity rate (W/K)
C            2 Total radiator heat emission (W)

      SUBROUTINE CMP27C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/Pctime/TIMSEC
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

      COMMON/C6/INDCFG
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      logical closea,close

      DIMENSION INOD(8),Q(8)

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP27C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      INOD(1)=NPCDAT(IPCOMP,9)
      DO 10 I=2,8
         INOD(I)=INOD(I-1)+1
   10 CONTINUE

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C Initialize the nominal log mean temperature difference DTLM0
         TS0=BDATA(IPCOMP,3)
         TX0=BDATA(IPCOMP,4)
         TE0=BDATA(IPCOMP,5)
         TE0TMP=(TS0-TE0)/(TX0-TE0)
         call eclose(TE0TMP,1.00,0.0001,close)
         IF(ABS(TS0-TE0).LT.SMALL.OR.ABS(TX0-TE0).LT.SMALL
     &      .OR.(TS0-TE0)/(TX0-TE0).LT.SMALL.OR.close) then
C     &      .OR.(TS0-TE0)/(TX0-TE0).EQ.1.0) THEN
         ELSE
           DTLM0=(TS0-TX0)/ALOG((TS0-TE0)/(TX0-TE0))
         END IF

C Then evaluate the current environment temperature
         IF(INDCFG.EQ.2.OR.NINT(BDATA(IPCOMP,6)).EQ.0) THEN
            call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
            IF(closea) THEN
               TE=BDATA(IPCOMP,5)
            ELSE
               TE=PCNTMF(IPCOMP)
            END IF
         ELSE
            IZ=INT(BDATA(IPCOMP,6))
            SUMT=TFA(IZ)
            NW=INT(BDATA(IPCOMP,7))
            SUMW=1.
            IF(NW.GE.1.) THEN
               DO 12 IW=1,NW
                  W=BDATA(IPCOMP,7+IW*2)
                  SUMW=SUMW+W
                  SUMT=SUMT+W*TFS(IZ,NINT(BDATA(IPCOMP,6+IW*2)))
   12          CONTINUE
            END IF
            TE=SUMT/SUMW
         END IF
C As the current log mean temperature difference DTLM will be based
C on it, mark current node 1 temperature for iteration
         ICSV(INOD(1),1)=1
         CSVI(INOD(1),1)=CSVF(INOD(1),1)

C Now calculate the heat emission at each successive radiator part
C and the total heat emission PCDATF(IPCOMP,2)
         PCDATF(IPCOMP,2)=0.
         DO 14 IP=2,8
            TS=CSVF(INOD(IP-1),1)
            TX=CSVF(INOD(IP),1)
            TXTMP=(tx-te)
            call eclose(TXTMP,0.00,0.0001,close)
C            if((tx-te).ne.0.0) then
            if(.NOT.close) then
              TSTMP=(TS-TE)/(TX-TE)
              call eclose(TSTMP,1.00,0.0001,close)
               IF(ABS(TS-TE).LT.SMALL.OR.ABS(TX-TE).LT.SMALL
     &            .OR.(TS-TE)/(TX-TE).LT.SMALL.OR.close)then
C     &            .OR.(TS-TE)/(TX-TE).EQ.1.0) THEN
               ELSE
                  DTLM=(TS-TX)/ALOG((TS-TE)/(TX-TE))
               END IF
            endif
            DTR=AMAX1(SMALL,(DTLM/DTLM0))
            Q(IP)=BDATA(IPCOMP,2)/7.*DTR**BDATA(IPCOMP,1)
            PCDATF(IPCOMP,2)=PCDATF(IPCOMP,2)+Q(IP)
   14    CONTINUE

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)/8.
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(3,CONVAR(ICON1,1))
C Inter-node fluid heat capacity rate (W/K)
         PCDATF(IPCOMP,1)=CSVF(INOD(1),2)*SHTFLD(3,CSVF(INOD(1),1))

C Calculate current component time-constant TC, based on node 1 only
         TC(IPCOMP)=CM/AMAX1(SMALL,C1)

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
         IF(IMPEXP.EQ.1) THEN
            ALPHA=1.
         ELSE IF(IMPEXP.EQ.2) THEN
            ALPHA=RATIMP
         ELSE IF(IMPEXP.EQ.3) THEN
            IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
               ALPHA=1.
            ELSE
               ALPHA=RATIMP
            END IF
         ELSE IF(IMPEXP.EQ.4) THEN
            CM=0.
            ALPHA=1.
         END IF

C Establish matrix equation self-coupling coefficients,
C  node 1
         COUT(1)=ALPHA*(-C1)-CM/TIMSEC
C  nodes 2 to 8
         COEF1=ALPHA*PCDATF(IPCOMP,1)
         COEF2=ALPHA*(-PCDATF(IPCOMP,1))-CM/TIMSEC
         DO 20 IP=2,8
            COUT(2*IP-2)=COEF1
            COUT(2*IP-1)=COEF2
   20    CONTINUE
C then matrix equation cross-coupling coefficient,
         COUT(16)=ALPHA*C1
C and then present-time coefficients (ie. right hand sides)
C  node 1
         COUT(17)=((1.-ALPHA)*PCRP(ICON1)-CM/TIMSEC)*CSVP(INOD(1),1)
     &            +(1.-ALPHA)*(-PCRP(ICON1))*PCTP(ICON1)
C  nodes 2 to 8
         COEF1=(1.-ALPHA)*(-PCDATP(IPCOMP,1))
         COEF2=(1.-ALPHA)*PCDATP(IPCOMP,1)-CM/TIMSEC
         DO 22 IP=2,8
            COUT(16+IP)=COEF1*CSVP(INOD(IP-1),1)
     &                 +COEF2*CSVP(INOD(IP),1)
     &                 +ALPHA*Q(IP)+(1.-ALPHA)*PCQP(INOD(IP))
   22    CONTINUE

C Store "environment" variables future values
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCRF(ICON1)=C1
         DO 24 IP=2,8
            PCQF(INOD(IP))=Q(IP)
   24    CONTINUE

C Establish "containment loss" data
         QDATA(IPCOMP)=ALPHA*PCDATF(IPCOMP,2)+
     &                 (1.-ALPHA)*PCDATP(IPCOMP,2)

C Establish additional output variables
         napdat(ipcomp)=9
         PCAOUT(IPCOMP,1)=TE
         PCAOUT(IPCOMP,2)=DTLM
         DO 26 IP=2,8
            PCAOUT(IPCOMP,1+IP)=Q(IP)
   26    CONTINUE

C 1st phase mass (ie. water) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         DO 30 IP=2,8
            COUT(2*IP-2)=-1.
            COUT(2*IP-1)=1.
   30    CONTINUE
         COUT(16)=-PCONDR(ICON1)
         COUT(17)=0.
         DO 32 IP=2,8
            COUT(16+IP)=0.
   32    CONTINUE
C
C 2nd phase mass (ie. "vapour") balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         DO 40 IP=2,8
            COUT(2*IP-2)=0.
            COUT(2*IP-1)=1.
   40    CONTINUE
         COUT(16)=0.
         COUT(17)=0.
         DO 42 IP=2,8
            COUT(16+IP)=0.
   42    CONTINUE
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 8 node (ISV=20) WCH radiator'
         WRITE(ITU,*) ' Matrix node(s) ',(INOD(I),I=1,8)
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' Q(I)   = ',(Q(I),I=1,8),' (W)'
            WRITE(ITU,*) ' Qtot   = ',PCDATF(IPCOMP,2),' (W)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=24
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP27C'

      RETURN
      END

C ******************** CMP28C ********************

C CMP28C generates for plant component IPCOMP with plant db code 280 ie.
C 1 node (ISV=29) Oil-filled electric panel radiator
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C     BDATA: 1 Radiator exponent (-)
C            2 Nominal heat emission of radiator (W)
C            3 Nominal radiator temperature (C)
C            4 Nominal environment temperature (C)
C            5 Index of coupled building zone (-)
C            6 Number of walls used for defining Te (-)
C            7 Index of 1st wall for defining Te (-)
C            8 Weighting factor for 1st wall when defining Te (-)
C            9 Index of 2nd wall for defining Te (-)
C           10 Weighting factor for 2nd wall when defining Te (-)
C           11 etc.
C     CDATA: Supplied electric energy (W).
C
C     PCDATF(IPCOMP,1): Supplied electrical energy (W).

      SUBROUTINE CMP28C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

C Electrical details for specified plant components
      common/pcelflg/ipcelf(mpcom)
      common/elpcp/NPEL,PFP(mpcom),IPFP(mpcom),PWRP(mpcom),
     &BVOLTP(mpcom),IPHP(mpcom)

      COMMON/C6/INDCFG
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      logical closea

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP21C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      INOD1=NPCDAT(IPCOMP,9)

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C Initialize the nominal log mean temperature difference DTLM0
         TR0=BDATA(IPCOMP,3)
         TE0=BDATA(IPCOMP,4)
         DTLM0=TR0-TE0

C Then evaluate the current environment temperature
         IF(INDCFG.EQ.2.OR.NINT(BDATA(IPCOMP,5)).EQ.0) THEN
            call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
            IF(closea) THEN
               TE=BDATA(IPCOMP,4)
            ELSE
               TE=PCNTMF(IPCOMP)
            END IF
         ELSE
            IZ=INT(BDATA(IPCOMP,5))
            SUMT=TFA(IZ)
            NW=int(BDATA(IPCOMP,6))
            SUMW=1.
            IF(NW.GE.1.) THEN
               DO 10 IW=1,NW
                  W=BDATA(IPCOMP,6+IW*2)
                  SUMW=SUMW+W
                  SUMT=SUMT+W*TFS(IZ,NINT(BDATA(IPCOMP,5+IW*2)))
   10          CONTINUE
            END IF
            TE=SUMT/SUMW
         END IF

C And the current log mean temperature difference DTLM based on
C current node 1 temp., so mark node 1 temperature for iteration
         ICSV(INOD1,1)=1
         CSVI(INOD1,1)=CSVF(INOD1,1)
         TR=CSVF(INOD1,1)
         DTLM=TR-TE

C Then calculate radiator heat emission Q
         DTR=AMAX1(SMALL,(DTLM/DTLM0))
         QR=BDATA(IPCOMP,2)*DTR**BDATA(IPCOMP,1)

C Establish required electrical energy.
         QE=CDATA(IPCOMP,1)

C Establish heat capacity of component mass CM (J/K).
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)

C Calculate current component time-constant TC
         TC(IPCOMP)=CM/
     &         AMAX1(SMALL,(QR/AMAX1(SMALL,AMAX1(SMALL,(TR-TE)))))

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
         IF(IMPEXP.EQ.1) THEN
            ALPHA=1.
         ELSE IF(IMPEXP.EQ.2) THEN
            ALPHA=RATIMP
         ELSE IF(IMPEXP.EQ.3) THEN
            IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
               ALPHA=1.
            ELSE
               ALPHA=RATIMP
            END IF
         ELSE IF(IMPEXP.EQ.4) THEN
            CM=0.
            ALPHA=1.
         END IF

C Establish matrix equation self-coupling coefficients,
C  node 1
         COUT(1)=-CM/TIMSEC

C and then present-time coefficients (ie. right hand sides)
         COUT(2)=(-CM/TIMSEC)*CSVP(INOD1,1)
     &           +ALPHA*QR+(1.-ALPHA)*PCQP(INOD1)
     &           -ALPHA*QE-(1.-ALPHA)*PCDATF(IPCOMP,1)

C Store "environment" variables future values
         PCQF(INOD1)=QR
         PCDATF(IPCOMP,1)=QE
         
         PWRP(IPCOMP)=-ABS(QE)
         IEMODEL=1
         CALL EMACH(IPCOMP,IEMODEL,PWRP(IPCOMP),PQ,PA)
         PWRQ=PQ
C Establish "containment loss" data
         QDATA(IPCOMP)=ALPHA*QR+(1.-ALPHA)*PCQP(INOD1)

C Establish additional output variables
         napdat(ipcomp)=6
         PCAOUT(IPCOMP,1)=TE
         PCAOUT(IPCOMP,2)=DTLM
         PCAOUT(IPCOMP,3)=QR
         PCAOUT(IPCOMP,4)=TR
         pcaout(ipcomp,5)=qe
         pcaout(ipcomp,6)=pwrq

C 1st phase mass (ie. "water") balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=0.

C 2nd phase mass (ie. "vapour") balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=0.
      endif

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=9) Oil-filled electric radiator'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         WRITE(ITU,*) ' Connection(s)  (NONE)'
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' TR     = ',TR,' (C)'
            WRITE(ITU,*) ' TE     = ',TE,' (C)'
            WRITE(ITU,*) ' DTLM   = ',DTLM,' (K)'
            WRITE(ITU,*) ' QR      = ',QR,' (W)'
            WRITE(ITU,*) ' QE      = ',QE,' (W)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=2
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP28C'
      RETURN
      END

c ******************** CMP29C ********************

c 'cmp29c' generates for plant component IPCOMP with plant db code 290 ie.
c 1 node (ISV=20) WCH  valve.
c matrix equation coefficients COUT (in order: self-coupling, cross-
c coupling, and present-time coefficients) for energy balance (ISTATS=1),
c 1st phase mass balance (ISTATS=2)
c     adata: 1 Component total mass (kg)
c            2 Mass weighted average specific heat (J/kgK)
c            3 UA modulus (W/K)
c     bdata: none
c     cdata: mass flow fraction entering damper. 

      subroutine cmp29c(ipcomp,cout,istats)
#include "plant.h"
#include "building.h"

      common/outin/iuout,iuin
      common/tc/itc,icnt
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/pctime/timsec
      common/pctc/tc(mpcom)

      common/pcequ/impexp,ratimp

      common/c9/npcomp,nci(mpcom),cdata(mpcom,mmiscd)
      common/c10/npcon,ipc1(mpcon),ipn1(mpcon),ipct(mpcon),
     &           ipc2(mpcon),ipn2(mpcon),pcondr(mpcon),pconsd(mpcon,2)
      common/c12ps/npcdat(mpcom,9),ipofs1(mcoefg),ipofs2(mcoefg,mpvar)
      common/pdbdt/adata(mpcom,madata),bdata(mpcom,mbdata)
      common/pcval/csvf(mpnode,mpvar),csvp(mpnode,mpvar)
      common/pcvar/pctf(mpcon),pcrf(mpcon),puaf(mpnode),pcqf(mpnode),
     &             pcntmf(mpcom),
     &             pctp(mpcon),pcrp(mpcon),puap(mpnode),pcqp(mpnode),
     &             pcntmp(mpcom)
      common/pcond/convar(mpcon,mconvr),icontp(mpcon),
     &             icondx(mpcom,mnodec,mpconc)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

      parameter (small=1.0e-15)
      real      cout(mpcoe)
      logical closea

c Trace output
      if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   ITRACE(37).ne.0) write(itu,*) ' Entering subroutine CMP07C'

c Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      icon1=icondx(ipcomp,1,1)
      inod1=npcdat(ipcomp,9)

c Initialise mass flow fraction entering damper.
         fm=cdata(ipcomp,1)

c Check for bad data.
c Note that very small values of 'fm' sometimes causes unpredictable
c results. For now always assume a min value of 0.1 or 10%.
         if(fm.gt.1.0) fm=1.0
         if(fm.lt.1E-06) fm=1E-06

c Generate coefficients for energy balance equation
      if(istats.eq.1) then

c First initialize UA modulus (for calculation of containment heat loss)
         ua=adata(ipcomp,3)
         call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
         IF(closea) UA=0.



c Establish heat capacity of component mass CM (J/K) and
c fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         cm=adata(ipcomp,1)*adata(ipcomp,2)
         c1=pcondr(icon1)*convar(icon1,2)*shtfld(1,convar(icon1,1))


c Calculate current component time-constant TC
         tc(ipcomp)=cm/amax1(small,(c1+ua))

c Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
         if(impexp.eq.1) then
            alpha=1.
         else if(impexp.eq.2) then
            alpha=ratimp
         else if(impexp.eq.3) then
            if(timsec.gt.0.63*tc(ipcomp)) then
               alpha=1.
            else
               alpha=ratimp
            end if
         else if(impexp.eq.4) then
            cm=0.
            alpha=1.
         end if

c Establish matrix equation self- and cross-coupling coefficients
         cout(1)=alpha*(-c1-ua)-cm/timsec
         cout(2)=alpha*c1

c and then present-time coefficient (ie. right hand side)
         cout(3)=((1.-alpha)*(pcrp(icon1)+puap(inod1))
     &              -cm/timsec)*csvp(inod1,1)
     &             +(1.-alpha)*(-pcrp(icon1))*pctp(icon1)
     &             -alpha*ua*pcntmf(ipcomp)
     &             -(1.-alpha)*puap(inod1)*pcntmp(ipcomp)

c Store "environment" variables future values
         puaf(inod1)=ua
         pctf(icon1)=convar(icon1,1)
         pcrf(icon1)=c1

c 1st phase mass (ie. water) balance coefficients
      else if(istats.eq.2) then
         cout(1)=1.
         cout(2)=-pcondr(icon1)*fm
         cout(3)=0.

c 2nd phase mass (ie. none) balance coefficients
      else if(istats.eq.3) then
         cout(1)=1.
         cout(2)=0
         cout(3)=0.
      end if
      
C Save additional output.
      napdat(ipcomp)=1
      pcaout(ipcomp,1)=fm

c Trace output
      if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   itrace(37).ne.0) then
         write(itu,*) ' Component      ',ipcomp,':'
         write(itu,*) ' 1 node (ISV=20) WCH valve'
         write(itu,*) ' Matrix node(s) ',inod1
         write(itu,*) ' Connection(s)  ',icon1
         if(istats.eq.1) then
            write(itu,*) ' CM     = ',cm,' (J/K)'
            write(itu,*) ' C1     = ',c1,' (W/K)'
            write(itu,*) ' TC     = ',TC(IPCOMP),' (s)'
            write(itu,*) ' ALPHA  = ',alpha,' (-)'
            write(itu,*) ' UA     = ',ua,' (W/K)'
            write(itu,*) ' PCNTMF = ',pcntmf(ipcomp),' (C)'
            write(itu,*) ' CDATA  = ',cdata(ipcomp,1)
         end if
         write(itu,*) ' Matrix coefficients for ISTATS = ',istats
         nitms=3
         write(itu,*) (cout(i),i=1,nitms)
         if(itu.eq.iuout) then
            ix1=(ipcomp/4)*4
            if(ix1.eq.ipcomp.or.ipcomp.eq.npcomp) call epagew
         end if
      end if

      if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   ITRACE(37).ne.0) write(itu,*) ' Leaving subroutine CMP29C'
     
      return
      end

C ******************** CMP31C ********************

C CMP31C generates for plant component IPCOMP with plant db code 310 ie.
C 2 node (ISV=20) WCH calorifier with electric emersion heater
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C            3 UA modulus
C            4 Mass of water encapsulated in tubes (kg)
C     BDATA: 1 Coil internal heat transfer surface area (m^2)
C            2 Coil internal heat transfer coefficient (W/m^2K)
C            3 Coil external heat transfer surface area (m^2)
C            4 Coil external heat transfer coefficient (W/m^2k)
C            



      SUBROUTINE CMP31C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PTIME/PTIMEP,PTIMEF
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      character outs*124
      logical closea

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP31C'

C Check control data for relevant balance type
      IF(ISTATS.EQ.1.AND.
     &   (CDATA(IPCOMP,1).LT.0..OR.CDATA(IPCOMP,1).GT.1.)) THEN
         CALL DAYCLK(IDYP,PTIMEF,IUOUT)
         WRITE(outs,*) ' CMP31C: invalid control data for component ',
     &                  IPCOMP,' : ',CDATA(IPCOMP,1)
         call edisp(iuout,outs)
         call edisp(iuout,' CMP31C: unresolvable error.')
         call epwait
         call epagend
         STOP
      END IF

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,2,1)
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

         ua=adata(ipcomp,3)
         call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
         IF(closea) UA=0.

C Determine flux transfer between tubes and tank.
      RI=1./(BDATA(IPCOMP,1)*BDATA(IPCOMP,2))
      RO=1./(BDATA(IPCOMP,3)*BDATA(IPCOMP,4))
      H=1./(RI+RO)

C Heat flux based on temperatures so mark 1st and 2nd nodal temps 
C and flows for iteration.
      ICSV(INOD1,1)=1
      CSVI(INOD1,1)=CSVF(INOD1,1)
      ICSV(INOD2,1)=1
      CSVI(INOD2,1)=CSVF(INOD2,1)
      ICSV(INOD1,2)=1
      CSVI(INOD1,2)=CSVF(INOD1,2)
      ICSV(INOD2,2)=1
      CSVI(INOD2,2)=CSVF(INOD2,2)       

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)
         CW=SHTFLD(3,CONVAR(ICON2,1))*ADATA(IPCOMP,4)
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(3,CONVAR(ICON1,1))
         C2=PCONDR(ICON2)*CONVAR(ICON2,2)*SHTFLD(3,CONVAR(ICON2,1))


C Calculate current component time-constant TC
         CMAX=AMAX1(CM,CW)
         TC(IPCOMP)=CMAX/AMAX1(SMALL,(C1+C2))

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
         IF(IMPEXP.EQ.1) THEN
            ALPHA=1.
         ELSE IF(IMPEXP.EQ.2) THEN
            ALPHA=RATIMP
         ELSE IF(IMPEXP.EQ.3) THEN
            IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
               ALPHA=1.
            ELSE
               ALPHA=RATIMP
            END IF
         ELSE IF(IMPEXP.EQ.4) THEN
            CM=0.
            CW=0.
            ALPHA=1.
         END IF

C Establish matrix equation self-coupling coefficients,
         COUT(1)=ALPHA*(-C1-UA-H)-CM/TIMSEC         
         COUT(2)=ALPHA*H
         COUT(3)=ALPHA*H
         COUT(4)=ALPHA*(-C2-H)-CW/TIMSEC
C then matrix equation cross-coupling coefficients,
         COUT(5)=ALPHA*C1
         COUT(6)=ALPHA*C2
C and then present-time coefficients (ie. right hand sides)
         COUT(7)=((1.-ALPHA)*(PCRP(ICON1)+PUAP(INOD1)+H)-CM/TIMSEC)
     &           *CSVP(INOD1,1)
     &           -(1.-ALPHA)*H*CSVP(INOD2,1)
     &           +(1.-ALPHA)*(-PCRP(ICON1))*PCTP(ICON1)
     &           -(1.-ALPHA)*PUAP(INOD1)*PCNTMP(IPCOMP)
     &           -ALPHA*UA*PCNTMF(IPCOMP)-(1.-ALPHA)*UA*PCNTMP(IPCOMP)
           
         COUT(8)=((1.-ALPHA)*(PCRP(ICON2)+H)-CW/TIMSEC)*CSVP(INOD2,1)
     &           -(1.-ALPHA)*H*CSVP(INOD1,1)
     &           -(1.-ALPHA)*PCRP(ICON2)*PCTP(ICON2)

C Store "environment" variables future values
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCTF(ICON2)=CONVAR(ICON2,1)
         PCRF(ICON1)=C1
         PCRF(ICON2)=C2

C Establish "containment loss" data
         QDATA(IPCOMP)=0.

C Establish additional output variables
         QCOIL=H*(CSVF(INOD2,1)-CSVF(INOD1,1))
         napdat(ipcomp)=1
         PCAOUT(IPCOMP,1)=QCOIL

C 1st phase mass (ie. water) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=0.
         COUT(4)=1.
         COUT(5)=-PCONDR(ICON1)
         COUT(6)=-PCONDR(ICON2)
         COUT(7)=0.
         COUT(8)=0.

C 2nd phase mass (ie. "vapour") balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=0.
         COUT(4)=1.
         COUT(5)=0.
         COUT(6)=0.
         COUT(7)=0.
         COUT(8)=0.
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 2 node (ISV=20) WCH calorifier'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2
         WRITE(ITU,*) ' Connection(s)  ',ICON1,ICON2
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' C2     = ',C2,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
            WRITE(ITU,*) ' CDATA  = ',CDATA(IPCOMP,1)
            WRITE(ITU,*) ' QCOIL  = ',QCOIL,' (W)'
            WRITE(ITU,*) ' HTC  = ',H,' (W/K)'
            WRITE(ITU,*) ' TDIFF  = ',
     &(CSVF(INOD1,1)-CSVF(INOD2,1)),' (W/K)'       
          
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=6
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP31C'

      RETURN
      END

C ******************** CMP32C ********************
C CMP40C generates for plant component IPCOMP with plant db code 320 ie.
C 2 node (ISV=20) WCH generic liquid-liquid heat exchanger.
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)

C     ADATA: 1 Mass of node 1 (solids+liquid) (kg)
C            2 Mass of node 2 (solids+liquid) (kg)
C            3 Overall heat transfer coefficient (W/m^2K)
C            4 Heat transfer surface area (m^2)
C            5 UA modulus for component (W/K)

C     BDATA: 1 Flow arrangement index (1-7)

C     CDATA: NONE

C The heat exchanger model uses the NTU method to determine the exchanger 
C effectiveness. Several different configurations of heat exchanger are 
C dealt with in the one model;
C 1 - Parallel flow
C 2 - Counter flow
C 3 - Shell and tube
C 4 - Cross flow with both fluids mixed
C 5 - Cross flow with both fluids unmixed
C 6 - Cross flow with Cmin unmixed
C 7 - Cross flow with Cmax unmixed
C
C Important assumptions made in the model are that the exchanger mass is small
C compared to the mass of encapsulated fluids, it is also assumed that the 
C interface thermal resistance is negligible compared to the film resistances.
C Fouling resistances are neglected. Overall UA value is assumed constant.

C REF:OPSYS - simulation of thermal systems P169. 
C     
C   
      SUBROUTINE CMP32C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP

      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

      PARAMETER (SMALL=1.0E-20)
      REAL      COUT(MPCOE),K,NTU
      
      LOGICAL CLOSE
C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP32C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,2,1)
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1

C Heat flux based on temperatures so mark 1st and 2nd nodal temps 
C and flows for iteration.
      ICSV(INOD1,1)=1
      CSVI(INOD1,1)=CSVF(INOD1,1)
      ICSV(INOD2,1)=1
      CSVI(INOD2,1)=CSVF(INOD2,1)
      ICSV(INOD1,2)=1
      CSVI(INOD1,2)=CSVF(INOD1,2)
      ICSV(INOD2,2)=1
      CSVI(INOD2,2)=CSVF(INOD2,2)
      ICSV(INOD1,3)=1
      CSVI(INOD1,3)=CSVF(INOD1,3)
      ICSV(INOD2,3)=1
      CSVI(INOD2,3)=CSVF(INOD2,3)       

C Total mass flow rate into node 1     
      TOTAL_MASS_FR1 = PCONDR(ICON1) * 
     &                (CONVAR(ICON1,2) + CONVAR(ICON1,3))

C Total mass flow rate into node 2
      TOTAL_MASS_FR2 = PCONDR(ICON2) * 
     &                (CONVAR(ICON2,2) + CONVAR(ICON2,3)) 

C Mass fraction of glycol in incoming flow to node 1      
      GLYCOL_MASS_FRACTION1 = 
     &            100 * CONVAR(ICON1,3) / (TOTAL_MASS_FR1 + SMALL)

C Mass fraction of glycol in incoming flow to node 2      
      GLYCOL_MASS_FRACTION2 = 
     &            100 * CONVAR(ICON2,3) / (TOTAL_MASS_FR2 + SMALL)

C Find volume fraction of glycol in mixture of incoming flow for node 1
      GLYCOL_VOL_FRACTION1 = 
     &GLYCOL_VOL_FRACTION_FIND(GLYCOL_MASS_FRACTION1,CONVAR(ICON1,1))

C Find volume fraction of glycol in mixture of incoming flow for node 2
      GLYCOL_VOL_FRACTION2 = 
     &GLYCOL_VOL_FRACTION_FIND(GLYCOL_MASS_FRACTION2,CONVAR(ICON2,1))

C Boiling temperature of glycol mixture for node 1
      TBOIL1 = GLYCOL_WATER_TBOIL(GLYCOL_VOL_FRACTION1)

C Boiling temperature of glycol mixture for node 2
      TBOIL2 = GLYCOL_WATER_TBOIL(GLYCOL_VOL_FRACTION2)

C Freezing temperature of glycol-water mixture for node 1
      TFREEZE1 = GLYCOL_WATER_TFREEZE(GLYCOL_VOL_FRACTION1)

C Freezing temperature of glycol-water mixture for node 2
      TFREEZE2 = GLYCOL_WATER_TFREEZE(GLYCOL_VOL_FRACTION2)

C Set incoming flow temperature using upper and lower bounds for node 1
      TEMP_INFLOW1 = MAX(TFREEZE1,MIN(TBOIL1,CONVAR(ICON1,1)))

C Set incoming flow temperature using upper and lower bounds for node 2
      TEMP_INFLOW2 = MAX(TFREEZE2,MIN(TBOIL2,CONVAR(ICON2,1)))

C Set specific heat of incoming flow for node 1
      GLY_WATER_CP1 = GLYCOL_WATER_CP(GLYCOL_VOL_FRACTION1,TEMP_INFLOW1)

C Set specific heat of incoming flow for node 2
      GLY_WATER_CP2 = GLYCOL_WATER_CP(GLYCOL_VOL_FRACTION2,TEMP_INFLOW2)
      
C Set temperature of node 1
      TEMP_NODE1 = MAX(TFREEZE1,MIN(TBOIL1,CSVF(INOD1,1)))

C Set temperature of node 2
      TEMP_NODE2 = MAX(TFREEZE2,MIN(TBOIL2,CSVF(INOD2,1)))

C Set specific heat of node 1
      GLY_WATER_CP_NODE1 = 
     &          GLYCOL_WATER_CP(GLYCOL_VOL_FRACTION1,TEMP_NODE1)

C Set specific heat of node 2
      GLY_WATER_CP_NODE2 = 
     &          GLYCOL_WATER_CP(GLYCOL_VOL_FRACTION2,TEMP_NODE2)

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN 

C Establish fluid thermal capacity rates
        CW1=GLY_WATER_CP_NODE1*ADATA(IPCOMP,1)
        CW2=GLY_WATER_CP_NODE2*ADATA(IPCOMP,2)
        C1=TOTAL_MASS_FR1*GLY_WATER_CP1
        C2=TOTAL_MASS_FR2*GLY_WATER_CP2

C Establish the overall K value
        K=ADATA(IPCOMP,3)*ADATA(IPCOMP,4)

C Establish heat loss modulus
        UA=ADATA(IPCOMP,5)
        CALL ECLOSE(PCNTMF(IPCOMP),99.,0.0001,CLOSE)
        IF(CLOSE) UA=0.0

C Check for zero flow.
C If zero flow found then set transfered flux to zero and skip.
        IF(C1.LT.SMALL.OR.C2.LT.SMALL) THEN
          HFLX=0.0
          GOTO 777
        ENDIF

C Find Cmin and Cmax
        CMIN=AMIN1(C1,C2)
        CMAX=AMAX1(C1,C2)

C Determine the NTU's and the thermal capacities ratio TCRAT
        NTU=K/CMIN
        TCRAT=CMIN/CMAX

C Determine the flow regime operational within the heat exchanger.
        IFLWR=INT(BDATA(IPCOMP,1))

C Establish the Effectiveness of the exchanger.
         IF(IFLWR.EQ.1) THEN

C Parallel Flow.
           EFF=(1.-EXP(-NTU*(1.+TCRAT)))/(1.+TCRAT)
        ELSEIF(IFLWR.EQ.2) THEN
C Counter Flow.
         A1=(1.-TCRAT*(EXP(-NTU*(1.-TCRAT))))
         IF(A1.LT.SMALL) A1=SMALL
         EFF=(1.-EXP(-NTU*(1.-TCRAT)))/A1
     &   
        ELSEIF(IFLWR.EQ.3) THEN 

C Shell and Tube.
          A1=1.+EXP(-NTU*((1.+TCRAT**2)**0.5))
          A2=1.-EXP(-NTU*((1.+TCRAT**2)**0.5)) 
          EFF=2./(1.+TCRAT+((1.+TCRAT**2)**0.5)*(A1/A2))
        ELSEIF(IFLWR.EQ.4) THEN 

C Cross FLow with both fluids mixed.
          A1=NTU/(1.-EXP(-NTU))
          A2=TCRAT*NTU/(1.-EXP(-TCRAT*NTU))
          EFF=NTU/(A1+A2-1.)
        ELSEIF (IFLWR.EQ.5) THEN

C Cross flow with both fluids unmixed.
          A1=((1.-EXP(-TCRAT*NTU**1.22))/TCRAT*NTU**0.22)
          EFF=1.-EXP(-A1)
        ELSEIF(IFLWR.EQ.6) THEN 
 
C Cross flow with CMIN=unmixed.  
          A1=(1.-EXP(-NTU))
          EFF=(1.- EXP(-TCRAT*A1))/TCRAT 
        ELSEIF(IFLWR.EQ.7) THEN
C Cross flow with CMAX=unmixed.
  
          A1=(1.-EXP(-TCRAT*NTU))/TCRAT
          EFF=1.-EXP(-A1)
        ENDIF      
          
C Establish the heat flux transferred between the nodes.
        HFLX=EFF*CMIN*(CONVAR(ICON2,1)-CONVAR(ICON1,1))
  777   CONTINUE
        TDIF=AMAX1(CSVF(INOD1,1)-CSVF(INOD2,1),SMALL)
C Calculate current component time-constant TC
        TC(IPCOMP)=AMAX1(
     &CW1/AMAX1(SMALL,(C1+ABS(HFLX)/TDIF)),
     &CW2/AMAX1(SMALL,(C2+ABS(HFLX)/TDIF)))

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
        IF(IMPEXP.EQ.1) THEN
           ALPHA=1.
        ELSE IF(IMPEXP.EQ.2) THEN
           ALPHA=RATIMP
        ELSE IF(IMPEXP.EQ.3) THEN
          IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
             ALPHA=1.
          ELSE
             ALPHA=RATIMP
          END IF
        ELSE IF(IMPEXP.EQ.4) THEN
            CW1=0.
            CW2=0.
            ALPHA=1.
        END IF

C Establish matrix equation self- and cross-coupling coefficients.
        COUT(1)=ALPHA*(-C1-UA/2.)-CW1/TIMSEC
        COUT(2)=ALPHA*(-C2-UA/2.)-CW2/TIMSEC
C Matrix cross coupling coefficients.
        COUT(3)=ALPHA*C1
        COUT(4)=ALPHA*C2
C Establish the present and known coefficient i.e. RHS
        COUT(5)=((1.-ALPHA)*(PCRP(ICON1)+UA/2.)-CW1/TIMSEC)
     &          *CSVP(INOD1,1)
     &          -(1.-ALPHA)*PCRP(ICON1)*PCTP(ICON1)
     &          -(1.-ALPHA)*(UA/2.)*PCNTMP(IPCOMP)
     &          -ALPHA*(UA/2.)*PCNTMF(IPCOMP)
     &          -ALPHA*HFLX
     &          -(1.-ALPHA)*PCQP(INOD1)
         COUT(6)=((1.-ALPHA)*(PCRP(ICON2)+UA/2.)-CW1/TIMSEC)
     &           *CSVP(INOD2,1)
     &          -(1.-ALPHA)*PCRP(ICON2)*PCTP(ICON2)
     &          -(1.-ALPHA)*(UA/2.)*PCNTMP(IPCOMP)
     &          -ALPHA*(UA/2.)*PCNTMF(IPCOMP)
     &          -ALPHA*(-HFLX)
     &          -(1.-ALPHA)*(-PCQP(INOD2))
C Store "environment" variables future values
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCTF(ICON2)=CONVAR(ICON2,1)
         PCRF(ICON1)=C1
         PCRF(ICON2)=C2
         PCQF(INOD1)=HFLX
         PCQF(INOD2)=HFLX
C Addition output for results analysis.
         NAPDAT(IPCOMP)=3
         PCAOUT(IPCOMP,1)=HFLX
         PCAOUT(IPCOMP,2)=NTU
         PCAOUT(IPCOMP,3)=EFF
C 1st phase mass (ie. "water") balance coefficients

      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=1.
         COUT(3)=-PCONDR(ICON1)
         COUT(4)=-PCONDR(ICON2)
         COUT(5)=0.
         COUT(6)=0.

C 2nd phase mass (ie. "vapour") balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=1.
         COUT(3)=-PCONDR(ICON1)
         COUT(4)=-PCONDR(ICON2)
         COUT(5)=0.
         COUT(6)=0.
      END IF


C Trace.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 2 node (ISV=29) WCH generic fluid-fluid HE'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2
         WRITE(ITU,*) ' Connection(s)  ',ICON1,ICON2
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CW1     = ',CW1,' (J/K)'
            WRITE(ITU,*) ' CW2     = ',CW2,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/Ks)'
            WRITE(ITU,*) ' C2     = ',C2,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' EFF     = ',EFF,' (-)'
            WRITE(ITU,*) ' NTU     = ',NTU,' (-)'
            WRITE(ITU,*) ' HFLX     = ',HFLX,' (W)' 
            WRITE(ITU,*) ' IFLWR     = ',IFLWR,' (-)'
            WRITE(ITU,*) ' TCRAT     = ',TCRAT,' (-)'  
            WRITE(ITU,*) ' TDIFF  = ',
     &(CONVAR(ICON2,1)-CONVAR(ICON1,1)),' (W/K)'                          
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=6
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP32C'

      RETURN
      END

C ******************** CMP33C ********************
C CMP33C generates for plant component IPCOMP with plant db code 320 ie.
C 2 node (ISV=20) WCH generic gas-liquid heat exchanger. This heat exchanger
C is intended for use with the CMP99C CHP unit model, where the input
C for the gas side is a hight temp exhaust gas (T>400 C). The properties
C of the exhaust gas are assumed to be those of high temperature air.
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)

C Node 1 - Gas node.
C Node 2 - Fluid node.

C     ADATA: 1 Mass of node 1 (solids+liquid) (kg)
C            2 Mass of node 2 (solids+liquid) (kg)
C            3 Overall heat transfer coefficient (W/m^2K)
C            4 Heat transfer surface area (m^2)
C            5 UA modulus for component (W/K)

C     BDATA: 1 Flow arrangement index (1-7)

C     CDATA: NONE

C The heat exchanger model uses the NTU method to determine the exchanger 
C effectiveness. Several different configurations of heat exchanger are 
C dealt with in the one model;
C 1 - Parallel flow
C 2 - Counter flow
C 3 - Shell and tube
C 4 - Cross flow with both fluids mixed
C 5 - Cross flow with both fluids unmixed
C 6 - Cross flow with Cmin unmixed
C 7 - Cross flow with Cmax unmixed
C
C Important assumptions made in the model are that the exchanger mass is small
C compared to the mass of encapsulated fluids, it is also assumed that the 
C interface thermal resistance is negligible compared to the film resistances.
C Fouling resistances are neglected. Overall UA value is assumed constant.

C REF:OPSYS - simulation of thermal systems P169. 
C     
C   
      SUBROUTINE CMP33C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP

      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

      PARAMETER (SMALL=1.0E-20)
      REAL      COUT(MPCOE),K,NTU
      
      LOGICAL CLOSE
C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP33C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,2,1)
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1

C Heat flux based on temperatures so mark 1st and 2nd nodal temps 
C and flows for iteration.
      ICSV(INOD1,1)=1
      CSVI(INOD1,1)=CSVF(INOD1,1)
      ICSV(INOD2,1)=1
      CSVI(INOD2,1)=CSVF(INOD2,1)
      ICSV(INOD1,2)=1
      CSVI(INOD1,2)=CSVF(INOD1,2)
      ICSV(INOD2,2)=1
      CSVI(INOD2,2)=CSVF(INOD2,2)       

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN 

C Establish fluid thermal capacity rates. Assume the specific
C heat of the combustion products is the following value.
C Cp=1.17kJ/kgK Assuming 200% theoretical (stochiometric) air.
        CW1=1170.0*ADATA(IPCOMP,1)
        CW2=SHTFLD(3,CSVF(INOD2,1))*ADATA(IPCOMP,2)
        C1=PCONDR(ICON1)*CONVAR(ICON1,2)*1170.0
        C2=PCONDR(ICON2)*CONVAR(ICON2,2)*SHTFLD(3,CONVAR(ICON2,1))

C Establish the overall K value
        K=ADATA(IPCOMP,3)*ADATA(IPCOMP,4)

C Establish heat loss modulus
        UA=ADATA(IPCOMP,5)
        CALL ECLOSE(PCNTMF(IPCOMP),99.,0.0001,CLOSE)
        IF(CLOSE) UA=0.0

C Check for zero flow.
C If zero flow found then set transfered flux to zero and skip.
        IF(C1.LT.0.0001.OR.C2.LT.0.0001) THEN
          HFLX=0.0
          GOTO 777
        ENDIF

C Find Cmin and Cmax
        CMIN=AMIN1(C1,C2)
        CMAX=AMAX1(C1,C2)

C Determine the NTU's and the thermal capacities ratio TCRAT
        NTU=K/CMIN
        TCRAT=CMIN/CMAX

C Determine the flow regime operational within the heat exchanger.
        IFLWR=INT(BDATA(IPCOMP,1))

C Establish the Effectiveness of the exchanger.
         IF(IFLWR.EQ.1) THEN

C Parallel Flow.
           EFF=(1.-EXP(-NTU*(1.+TCRAT)))/(1.+TCRAT)
        ELSEIF(IFLWR.EQ.2) THEN
C Counter Flow.
         A1=(1.-TCRAT*(EXP(-NTU*(1.-TCRAT))))
         IF(A1.LT.SMALL) A1=SMALL
         EFF=(1.-EXP(-NTU*(1.-TCRAT)))/A1
     &   
        ELSEIF(IFLWR.EQ.3) THEN 

C Shell and Tube.
          A1=1.+EXP(-NTU*((1.+TCRAT**2)**0.5))
          A2=1.-EXP(-NTU*((1.+TCRAT**2)**0.5)) 
          EFF=2./(1.+TCRAT+((1.+TCRAT**2)**0.5)*(A1/A2))
        ELSEIF(IFLWR.EQ.4) THEN 

C Cross FLow with both fluids mixed.
          A1=NTU/(1.-EXP(-NTU))
          A2=TCRAT*NTU/(1.-EXP(-TCRAT*NTU))
          EFF=NTU/(A1+A2-1.)
        ELSEIF (IFLWR.EQ.5) THEN

C Cross flow with both fluids unmixed.
          A1=((1.-EXP(-TCRAT*NTU**1.22))/TCRAT*NTU**0.22)
          EFF=1.-EXP(-A1)
        ELSEIF(IFLWR.EQ.6) THEN 
 
C Cross flow with CMIN=unmixed.  
          A1=(1.-EXP(-NTU))
          EFF=(1.- EXP(-TCRAT*A1))/TCRAT 
        ELSEIF(IFLWR.EQ.7) THEN
C Cross flow with CMAX=unmixed.
  
          A1=(1.-EXP(-TCRAT*NTU))/TCRAT
          EFF=1.-EXP(-A1)
        ENDIF      
          
C Establish the heat flux transferred between the nodes.
        HFLX=EFF*CMIN*(CONVAR(ICON2,1)-CONVAR(ICON1,1))
  777   CONTINUE
        TDIF=AMAX1(CSVF(INOD1,1)-CSVF(INOD2,1),SMALL)
C Calculate current component time-constant TC
        TC(IPCOMP)=AMAX1(
     &CW1/AMAX1(SMALL,(C1+ABS(HFLX)/TDIF)),
     &CW2/AMAX1(SMALL,(C2+ABS(HFLX)/TDIF)))

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
        IF(IMPEXP.EQ.1) THEN
           ALPHA=1.
        ELSE IF(IMPEXP.EQ.2) THEN
           ALPHA=RATIMP
        ELSE IF(IMPEXP.EQ.3) THEN
          IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
             ALPHA=1.
          ELSE
             ALPHA=RATIMP
          END IF
        ELSE IF(IMPEXP.EQ.4) THEN
            CW1=0.
            CW2=0.
            ALPHA=1.
        END IF

C Establish matrix equation self- and cross-coupling coefficients.
        COUT(1)=ALPHA*(-C1-UA/2.)-CW1/TIMSEC
        COUT(2)=ALPHA*(-C2-UA/2.)-CW2/TIMSEC
C Matrix cross coupling coefficients.
        COUT(3)=ALPHA*C1
        COUT(4)=ALPHA*C2
C Establish the present and known coefficient i.e. RHS
        COUT(5)=((1.-ALPHA)*(PCRP(ICON1)+UA/2.)-CW1/TIMSEC)
     &          *CSVP(INOD1,1)
     &          -(1.-ALPHA)*PCRP(ICON1)*PCTP(ICON1)
     &          -(1.-ALPHA)*(UA/2.)*PCNTMP(IPCOMP)
     &          -ALPHA*(UA/2.)*PCNTMF(IPCOMP)
     &          -ALPHA*HFLX
     &          -(1.-ALPHA)*PCQP(INOD1)
         COUT(6)=((1.-ALPHA)*(PCRP(ICON2)+UA/2.)-CW1/TIMSEC)
     &           *CSVP(INOD2,1)
     &          -(1.-ALPHA)*PCRP(ICON2)*PCTP(ICON2)
     &          -(1.-ALPHA)*(UA/2.)*PCNTMP(IPCOMP)
     &          -ALPHA*(UA/2.)*PCNTMF(IPCOMP)
     &          -ALPHA*(-HFLX)
     &          -(1.-ALPHA)*(-PCQP(INOD2))
C Store "environment" variables future values
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCTF(ICON2)=CONVAR(ICON2,1)
         PCRF(ICON1)=C1
         PCRF(ICON2)=C2
         PCQF(INOD1)=HFLX
         PCQF(INOD2)=HFLX
C Addition output for results analysis.
         NAPDAT(IPCOMP)=3
         PCAOUT(IPCOMP,1)=HFLX
         PCAOUT(IPCOMP,2)=NTU
         PCAOUT(IPCOMP,3)=EFF
C 1st phase mass (ie. "water") balance coefficients

      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=1.
         COUT(3)=-PCONDR(ICON1)
         COUT(4)=-PCONDR(ICON2)
         COUT(5)=0.
         COUT(6)=0.

C 2nd phase mass (ie. "vapour") balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=1.
         COUT(3)=0.
         COUT(4)=0.
         COUT(5)=0.
         COUT(6)=0.
      END IF


C Trace.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 2 node (ISV>20) WCH generic gas-fluid HE'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2
         WRITE(ITU,*) ' Connection(s)  ',ICON1,ICON2
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CW1     = ',CW1,' (J/K)'
            WRITE(ITU,*) ' CW2     = ',CW2,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/Ks)'
            WRITE(ITU,*) ' C2     = ',C2,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' EFF     = ',EFF,' (-)'
            WRITE(ITU,*) ' NTU     = ',NTU,' (-)'
            WRITE(ITU,*) ' HFLX     = ',HFLX,' (W)' 
            WRITE(ITU,*) ' IFLWR     = ',IFLWR,' (-)'
            WRITE(ITU,*) ' TCRAT     = ',TCRAT,' (-)'  
            WRITE(ITU,*) ' TDIFF  = ',
     &(CONVAR(ICON2,1)-CONVAR(ICON1,1)),' (W/K)'                          
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=6
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP33C'

      RETURN
      END
C ******************** CMP34C ********************
C CMP34C generates for plant component IPCOMP with plant db code 340 ie.
C 3 node (ISV>20) WCH storage water heater. Developed from the model described
C in "Combo/AIMS model description" by Michael Parent;
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C            3 Component UA modulus (for heat loss to environment) (W/K)
C
C Component data.
C     BDATA: 
C            1 Burner capacity when ON (W)
C            2 Burner capacity when OFF (standby) (W)
C            3 UAx value for heat transfer to water (W/K)
C
C Fuel data.
C            4 Fuel heating value @ nominal AF ratio (J/kg fuel)
C            5 Stoichiometric air/fuel mass ratio (W/K)
C            6 Excess air (%)
C            7 Fuel specific heat capacity (J/kgK)
C            8 Efficiency at full load (ON) (K)
C
C Combustion products.
C            9 Specific heat of combustion gases with T<537.8C (J/kgK)
C           10 Mass ratio of water in combustion gas per unit mass of fuel (kg/kg)
C
C     CDATA: 1 ON/OFF signal 
      SUBROUTINE CMP34C(IPCOMP,COUT,ISTATS)
C
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PCTIME/TIMSEC
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP

      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

      COMMON/LASTCTL/CTLDATA(MPCOM,MMISCD)


      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE),KHT,MSC,MFCP,LMTD
      LOGICAL CLOSE,CLOSEA,CLOSEB,CLOSEC
      CHARACTER*124 OUTS

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP01C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD

C 2 connections 1,2 to the water storage tank (for DHW and heating system)
      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,1,2)

C 1 connection 3 to the combustion chamber node (for combustion air) 
      ICON3=ICONDX(IPCOMP,2,1)

C 3 - nodes 1) water+casing 2) combustion node 2) flue (outlet) node
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1
      INOD3=NPCDAT(IPCOMP,9)+2

C Mark the nodal temperatures (flue outlet node) for iteration. 
      ICSV(INOD3,1)=1
      CSVI(INOD3,1)=CSVF(INOD3,1)

C Generate coefficients for the energy balance equation
      IF(ISTATS.EQ.1) THEN

C First initialize UA modulus (for calculation of containment heat loss),
C if there is no containment then zero the UA value. 
         UA=ADATA(IPCOMP,3)
         CALL ECLOSE(PCNTMF(IPCOMP),-99.00,0.001,CLOSEA)
         IF(CLOSEA) UA=0.

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(3,CONVAR(ICON1,1))
         C2=PCONDR(ICON2)*CONVAR(ICON2,2)*SHTFLD(3,CONVAR(ICON2,1))

C The flow rate supplied to the component from a fan is the 
C combustion air and should = stochiometric air fuel ratio * excess air/100 * fuel flow
         CV=PCONDR(ICON3)*CONVAR(ICON3,3)*SHTFLD(2,CONVAR(ICON3,1))
         CA=PCONDR(ICON3)*CONVAR(ICON3,2)*SHTFLD(1,CONVAR(ICON3,1))

C Burner output at 'ON'
         BURNHI=BDATA(IPCOMP,1)

C Burner output at 'OFF' of 'STANDBY'
         BURNLO=BDATA(IPCOMP,2)

C Heat transfer conductance to water. 
         UAx=BDATA(IPCOMP,3)

C Nominal Fuel heating value @ EXCESAIR (J/kg)
         FUELHV=BDATA(IPCOMP,4)

C Stochiometric air fuel mass ratio (kg/kg)
         AIRFLR=BDATA(IPCOMP,5)

C Nominal excess air (%)
         EXSAIR=BDATA(IPCOMP,6)

C Fuel specific heat 
         CPFUEL=BDATA(IPCOMP,7)

C Efficiency at fully load (ON)
         EFFX=BDATA(IPCOMP,8)/100.

C Specific heat of combustion products @ TCOMB
         CPLOW=BDATA(IPCOMP,9)

C Mass ratio of H2O in exhaust gas
         EXGH2O=BDATA(IPCOMP,10)

C Calculate the fuel flow rate depending on the ON/OFF control signal. 
         CALL ECLOSE(CDATA(IPCOMP,2),0.0,0.0001,CLOSE)
         IF(FUELHV.GT.0.)THEN
           IF(CLOSE)THEN
             FFR=BURNLO/FUELHV
           ELSE 
C If fan is not on the previous time step step up burner output to trigger
C the fan. 
             IF(CTLDATA(IPCOMP,1).GT.0.01)THEN       
               FFR=BURNHI/FUELHV      
             ELSE
               FFR=4*BURNLO/FUELHV
             ENDIF
           ENDIF 
         ELSE
           WRITE(IUOUT,*)'Error in CMP34C fuel heating value is zero.'
           STOP      
         ENDIF
         CTLDATA(IPCOMP,1)=CDATA(IPCOMP,2)

C Check for zero flow and issue warrning
         IF(CONVAR(ICON3,2).LT.SMALL)THEN
           FFR=0.0
           WRITE(OUTS,*)'WARNING: CMP34C no combustion air flow.'
           CALL EDISP(IUOUT,OUTS)
           WRITE(OUTS,*)'fuel supply shut off. ' 
           CALL EDISP(IUOUT,OUTS)
         ENDIF
         
C Dry Air Cp
         CPA=SHTFLD(1,CONVAR(ICON3,2))

C Vapour Cp
         CPV=SHTFLD(2,CONVAR(ICON3,3))

C Moisture content
         IF(CONVAR(ICON3,2).GT.0.)THEN
           MSC=CONVAR(ICON3,3)/CONVAR(ICON3,2)
         ELSE

C Check for zero flow. 
           MSC=0.0
         ENDIF

C hfg of air at temperature of the inlet
         HFGIN=CNDWAT(CONVAR(ICON3,1))

C Calculate the enthalpy of the inlet fuel and air streams. 
         HAIR=CPA*CONVAR(ICON3,1)+MSC*(HFGIN+CPV*CONVAR(ICON3,1))
         HIN=FFR*(FUELHV+CPFUEL*CONVAR(ICON3,1))
     &   +PCONDR(ICON3)*CONVAR(ICON3,2)*HAIR


C Calculate the specific heat of the combustion gases
         CPG=(0.28399*(EXSAIR/100.)**(-0.08244))*4.184*1000.

C Calculate the combustion product flow rate (kg) (the flow rate between nodes 
C 2 and 3) air flow + moisture flow + fuel flow. 
         MFCP=PCONDR(ICON3)*(CONVAR(ICON3,2)+CONVAR(ICON3,3))+FFR
         FW=FFR*EXGH2O/(PCONDR(ICON3)*(CONVAR(ICON3,2)))
C Calculate the dew point temperature of the combustion gases
         TDEW=19.456*LOG(FW*100)+2.7339
                 
C Calculate the temperature differences  dT1 and dT2
         dT2=CSVF(INOD3,1)-CSVF(INOD1,1)
         IF(dT2.LT.0.0)dT2=0.0
         dT1=CSVF(INOD2,1)-CSVF(INOD1,1)
         IF(dT1.LT.0.0)dT1=0.0
         CLOSE=.FALSE.
         CALL ECLOSE(dT1,dT2,0.01,CLOSEA)
         CALL ECLOSE(dT1,0.00,0.01,CLOSEB)    
         CALL ECLOSE(dT2,0.00,0.01,CLOSEC)         

C Calculate the temperature drop along the heat exchanger
C using iteration. 
         IF(CLOSEA)THEN       
           LMTD=(dT1+dT2)*0.5
         ELSEIF(CLOSEB)THEN
           LMTD=0.0
         ELSEIF(CLOSEC)THEN
           LMTD=dT1/2.
         ELSE
           LMTD=(dT2-dT1)/(ALOG(dT2/dT1))
         ENDIF

         IF(LMTD.LT.0.)LMTD=0.0

C Check that the transferred heat flux does not exceed the full load
C efficiency.      
         HFLX=UAx*LMTD
         IF(HFLX.GT.EFFX*HIN)HFLX=EFFX*HIN

C Also assign new value to UAx
         IF(LMTD.GT.0.001)THEN
           UAx=EFFX*HIN/LMTD
         ELSE
           UAx=0.0
         ENDIF
         HFLX=UAx*LMTD

C If the calculated outlet temperature < Tdew then reduce the moisture 
C content of the outlet air and reduce the sensible heat removed from the
C gas stream
         IF(TDEW.GT.CSVF(INOD3,1))THEN

C Calculate the "equivalent" heat transfer in the condensing region
           Qx=MFCP*CPG*(TDEW-CSVF(INOD3,1))

C Calculate the inlet and outlet enthalpy streams to the condensing region
           Hi=ENTHP2(TDEW,MSC)*1000.
           Ho=Hi-(Qx/MFCP)

C Calculate the outlet temp assuming RH=100%           
           Tox=TSATH0(Ho/1000.,1013.25)  

C Calculate the sensible heat transfer in the condensing region  
           Qs=MFCP*CPG*(TDEW-Tox)
           HFLXs=HFLX-(Qx-Qs)
C Calculate the amount of condensation 
           CMx=(Qx-Qs)/CNDWAT(CSVF(INOD1,1))
         ELSE
           HFLXs=HFLX 
         ENDIF
       
C Calculate current component time-constant TC 
         TC(IPCOMP)=CM/AMAX1(SMALL,(UA+HFLX))

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
         IF(IMPEXP.EQ.1) THEN
            ALPHA=1.
         ELSE IF(IMPEXP.EQ.2) THEN
            ALPHA=RATIMP
         ELSE IF(IMPEXP.EQ.3) THEN
            IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
               ALPHA=1.
            ELSE
               ALPHA=RATIMP
            END IF
         ELSE IF(IMPEXP.EQ.4) THEN
            CM=0.
            ALPHA=1.
         END IF

C Establish matrix equation self-and cross-coupling coefficients
C matrix order as follows
C   A  B  C | i  j  k   RHS
C   1  0  0 | 5  6  0    11
C   0  2  0 | 0  0  7  = 12
C   0  3  4 | 0  0  0    13

C Solid node A
         COUT(1)=ALPHA*(-C1-C2-UA)-CM/TIMSEC

C Combustion node B
         COUT(2)=-MFCP*CPG         

C Outlet node C 
         COUT(3)=(MFCP*CPG)
         COUT(4)=-(MFCP*CPG)                   

C Cross coupling coefficients
C Node A
         COUT(5)=ALPHA*C1
         COUT(6)=ALPHA*C2

C Node B.
         COUT(7)=(MFCP*CPG)
         
C and then present-time coefficient (ie. right hand side)
         COUT(8)=((1.-ALPHA)*(PCRP(ICON1)+PCRP(ICON2)+PUAP(INOD1))
     &           -CM/TIMSEC)*CSVP(INOD1,1)
     &           -(1.-ALPHA)*(PCRP(ICON1))*PCTP(ICON1)
     &           -(1.-ALPHA)*(PCRP(ICON2))*PCTP(ICON2)
     &           -ALPHA*UA*PCNTMF(IPCOMP)
     &           -(1.-ALPHA)*PUAP(INOD1)*PCNTMP(IPCOMP)
     &           -(1.-ALPHA)*PCQP(INOD1)
     &           -(ALPHA)*HFLX
         COUT(9)=-HIN
     &            +(PCONDR(ICON3)*CONVAR(ICON3,3)+(FFR*EXGH2O))*HFGIN
         COUT(10)=HFLXs

C Store "environment" variables future values
         PUAF(INOD1)=UA
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCTF(ICON2)=CONVAR(ICON2,1)
         PCRF(ICON1)=C1
         PCRF(ICON2)=C2
         PCQF(INOD1)=HFLX

C 1st phase mass (ie. dry air) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=1.
         COUT(3)=1.
         COUT(4)=-1.
         COUT(5)=-PCONDR(ICON1)
         COUT(6)=-PCONDR(ICON2)
         COUT(7)=-PCONDR(ICON3)
         COUT(8)=0.
         COUT(9)=0.
         COUT(10)=0.

C 2nd phase mass (ie. vapour) balance coefficients.
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=1.
         COUT(3)=1.
         COUT(4)=-1.
         COUT(5)=0.
         COUT(6)=0.
         COUT(7)=0.
         COUT(8)=0.
         COUT(9)=FFR*EXGH2O-CMx
         COUT(10)=0.
      END IF

C Establish additional output variables
      NAPDAT(IPCOMP)=3
C On/off signal
      PCAOUT(IPCOMP,1)=CDATA(IPCOMP,2)
C Fuel flow rate
      PCAOUT(IPCOMP,2)=FFR
C Total Heating flux
      PCAOUT(IPCOMP,3)=HFLX
C Condensation
      PCAOUT(IPCOMP,4)=CMx

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 3 node (ISV>21) WCH Storage water heater'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         WRITE(ITU,*) ' Connection(s)  ',ICON1,ICON2
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' C2     = ',C2,' (W/K)'
            WRITE(ITU,*) ' CV     = ',CV,' (W/K)'
            WRITE(ITU,*) ' CA     = ',CA,' (W/K)'
            WRITE(ITU,*) ' UAx   =  ',UAx,' (W/K)'
            WRITE(ITU,*) ' MSC    = ',MSC,' (kg/kg) da'
            WRITE(ITU,*) ' HFG    = ',HFGIN,' (J/kg) da'
            WRITE(ITU,*) ' FFR    = ',FFR,' (kg/s)'
            WRITE(ITU,*) ' CDATA  = ',CDATA(IPCOMP,1),' (-)'
            WRITE(ITU,*) ' QH     = ',HFLX, ' (W) '
            WRITE(ITU,*) ' QF     = ',FFR*FUELHV, ' (W) '
            WRITE(ITU,*) ' CPG    = ',CPG,' (J/kgK) '
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' UA     = ',UA,' (W/K)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=10
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) then
        WRITE(ITU,*) ' Leaving subroutine CMP34C trace'
      ENDIF

      RETURN
      END
      
C ******************** CMP35C ********************
C CMP35C generates for plant component IPCOMP with plant db code 350 ie.
C 2 node (ISV=20) WCH basic radiator model
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C     BDATA: 1 Total radiator surface area (m2)
C            2 Surface heat transfer coefficient (W/m2K)
C            3 Nominal environment temperature (C)
C            4 Index of coupled building zone (0-n)
C            5 Number of walls used for defining Te (0-n)
C            Then for each wall ....
C            6 Index of 1st wall for defining Te (-)
C            7 Weighting factor for 1st wall when defining Te (-)
C            8 Index of 2nd wall for defining Te (-)
C            9 Weighting factor for 2nd wall when defining Te (-)
C           10 etc.
C
C If no zone or wall surface is specified then the nominal environment 
C temperature value is used
C
C     CDATA: none

C     PCDATF/P
C            1 Inter-node fluid heat capacity rate (W/K)

      SUBROUTINE CMP35C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

      COMMON/C6/INDCFG
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      logical closea

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP35C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C Initialize hA value for the radiator
         rad_area=BDATA(IPCOMP,1)
         ht_coeff=BDATA(IPCOMP,2)
 
         T_env_nom=BDATA(IPCOMP,3)
 
C Set default for TE
         TE=T_env_nom

C Then evaluate the current environment temperature
         
C If this is a plant only simulation or if no zone has been defined then
C set the environmental temperature to the nominal value specified in the 
C data.
         IF(INDCFG.EQ.2.OR.NINT(BDATA(IPCOMP,4)).EQ.0) THEN
            call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
            IF(closea) THEN
               TE=T_env_nom
            ELSE
               TE=PCNTMF(IPCOMP)
            ENDIF

C Otherwise get the zone/surface temperatures.
         ELSE
            IZ=INT(BDATA(IPCOMP,4))
            SUMT=TFA(IZ)
            NW=int(BDATA(IPCOMP,5))
            SUMW=1.
            IF(NW.GE.1.) THEN
               DO 10 IW=1,NW
                  W=BDATA(IPCOMP,5+IW*2)
                  SUMW=SUMW+W
                  SUMT=SUMT+W*TFS(IZ,NINT(BDATA(IPCOMP,4+IW*2)))
   10          CONTINUE
            END IF
            TE=SUMT/SUMW
         END IF
 
C Mark the nodal temperatures for iteration.
         ICSV(INOD1,1)=1
         CSVI(INOD1,1)=CSVF(INOD1,1)
         ICSV(INOD2,1)=1
         CSVI(INOD2,1)=CSVF(INOD2,1)
         TS=CSVF(INOD1,1)
         TX=CSVF(INOD2,1)


C Then calculate radiator heat emission Q 
           Q1=0.5*rad_area*ht_coeff*(TS-TE)
           Q2=0.5*rad_area*ht_coeff*(TX-TE)
           Q=Q1+Q2
  

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)/2.
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(3,CONVAR(ICON1,1))

C Inter-node fluid heat capacity rate (W/K)
         PCDATF(IPCOMP,1)=CSVF(INOD1,2)*SHTFLD(3,CSVF(INOD1,1))

C Calculate current component time-constant TC
         TC(IPCOMP)=AMAX1(
     &        CM/AMAX1(SMALL,(C1)),
     &        CM/AMAX1(SMALL,(PCDATF(IPCOMP,1)+
     &        Q/AMAX1(SMALL,AMAX1(SMALL,((0.5*(TS+TX))-TE))))))

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
         IF(IMPEXP.EQ.1) THEN
            ALPHA=1.
         ELSE IF(IMPEXP.EQ.2) THEN
            ALPHA=RATIMP
         ELSE IF(IMPEXP.EQ.3) THEN
            IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
               ALPHA=1.
            ELSE
               ALPHA=RATIMP
            END IF
         ELSE IF(IMPEXP.EQ.4) THEN
            CM=0.
            ALPHA=1.
         END IF

C Establish matrix equation self-coupling coefficients,
C  node 1
         COUT(1)=ALPHA*(-C1)-CM/TIMSEC
C  node 2
         COUT(2)=ALPHA*PCDATF(IPCOMP,1)
         COUT(3)=ALPHA*(-PCDATF(IPCOMP,1))-CM/TIMSEC
C then matrix equation cross-coupling coefficient,
         COUT(4)=ALPHA*C1
C and then present-time coefficients (ie. right hand sides)
         COUT(5)=((1.-ALPHA)*PCRP(ICON1)-CM/TIMSEC)*CSVP(INOD1,1)
     &           +(1.-ALPHA)*(-PCRP(ICON1))*PCTP(ICON1)
     &           +ALPHA*Q1+(1.-ALPHA)*PCQP(INOD1)
         COUT(6)=(1.-ALPHA)*(-PCDATP(IPCOMP,1))*CSVP(INOD1,1)
     &           +((1.-ALPHA)*PCDATP(IPCOMP,1)-CM/TIMSEC)*CSVP(INOD2,1)
     &           +ALPHA*Q2+(1.-ALPHA)*PCQP(INOD2)

C Store "environment" variables future values
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCRF(ICON1)=C1
         PCQF(INOD1)=Q1
         PCQF(INOD2)=Q2

C Establish "containment loss" data
         QDATA(IPCOMP)=ALPHA*Q+(1.-ALPHA)*(PCQP(INOD2)+PCQP(INOD1))

C Establish additional output variables
         PCAOUT(IPCOMP,1)=Q
         PCAOUT(IPCOMP,2)=0.5*(TS+TX)
         PCAOUT(IPCOMP,3)=TE

C 1st phase mass (ie. water) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=-1.
         COUT(3)=1.
         COUT(4)=-PCONDR(ICON1)
         COUT(5)=0.
         COUT(6)=0.

C 2nd phase mass (ie. "vapour") balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=1.
         COUT(4)=0.
         COUT(5)=0.
         COUT(6)=0.
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 2 node (ISV=20) WCH basic radiator'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' T_rad_mean = ',0.5*(TS+TX),' (C)'
            WRITE(ITU,*) ' TE     = ',TE,' (C)'
            WRITE(ITU,*) ' ht_coeff   = ',ht_coeff,' (W/m2K)'
            WRITE(ITU,*) ' rad_area   = ',rad_area,' (m2)'
            WRITE(ITU,*) ' Q      = ',Q,' (W)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=6
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP35C'

      RETURN
      END

C ******************** CMP36C ********************
C CMP36C generates for plant component IPCOMP with plant db code 360 ie.
C 2 node (ISV=19,21) WCH basic water loop reversible heat pump.
C
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)

C     ADATA: 1 Mass of component (solids+liquid) (kg)
C            2 Mass weighted average specific heat node (J/kgK)
C            3 UA modulus for component (W/K)

C     BDATA: 1 COP heating (if < 0 use quadratic f(Twater))
C            2 coef. h0
C            3 coef. h1
C            4 coef. h2
C            5 COP cooling (if < 0 use quadratic f(Tair))
C            6 coef. c0
C            7 coef. c1
C            8 coef. c2            

C     CDATA: 1 cooling or heating duty (W)


C Node 1 is the element of the component connected to the water loop. Node 2 is the    
C part of the conponent connected to conditioned air.
C
C   
      SUBROUTINE CMP36C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP

      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat
      
           
C Electrical details for specified plant components
      common/pcelflg/ipcelf(mpcom)
      common/elpcp/NPEL,PFP(mpcom),IPFP(mpcom),PWRP(mpcom),
     &BVOLTP(mpcom),IPHP(mpcom)
     
C Reversible heat pump common
      common/rev_HP_logic/rev_heat_pump(MPCOM)
     
      logical rev_heat_pump 


      PARAMETER (SMALL=1.0E-20)
      REAL      COUT(MPCOE)
      
      LOGICAL CLOSE
C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP36C'

C Set reversible flag to true
      rev_heat_pump(IPCOMP)= .true.
      
      write(84,*) 'rev_heat_pump ',ipcomp,rev_heat_pump(IPCOMP)

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,2,1)
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1

C COP based on inlet temperatures so mark 1st and 2nd nodal temps 
C and flows for iteration.
      ICSV(INOD1,1)=1
      CSVI(INOD1,1)=CSVF(INOD1,1)
      ICSV(INOD2,1)=1
      CSVI(INOD2,1)=CSVF(INOD2,1)

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN 

C Establish the nodal thermal capacities.
        CM1=0.5*ADATA(IPCOMP,1)*ADATA(IPCOMP,2)
        CM2=CM1

C node 1 water
        C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(3,CONVAR(ICON1,1))

C node 2 moist air
        C2=PCONDR(ICON2)*(CONVAR(ICON2,2)*SHTFLD(1,CONVAR(ICON2,1))
     &     +CONVAR(ICON2,3)*SHTFLD(2,CONVAR(ICON2,1)))

C Establish the COP for heating mode
        COPh=BDATA(IPCOMP,1)

C Establish hot and cold temperatures
        T_h=convar(icon1,1) 
        T_c=convar(icon2,1)

C Establish the COP for heating mode if calculated using a quadratic
        if(COPh.lt.0.)then
          h2=BDATA(IPCOMP,2)
          h1=BDATA(IPCOMP,3)
          h0=BDATA(IPCOMP,4)
          COPh=h2*T_h**2+h1*T_h+h0
        endif

C Establish the COP for cooling mode
        COPc=BDATA(IPCOMP,5)

C Establish the COP for heating mode if calculated using a quadratic
        if(COPc.lt.0.)then
          c2=BDATA(IPCOMP,6)
          c1=BDATA(IPCOMP,7)
          c0=BDATA(IPCOMP,8)
          T_c=CONVAR(ICON2,1)
          COPc=c2*T_c**2+c1*T_c+c0
        endif

C Establish the heat pump duty cooling or heating 
C (heat to or from the air stream)
        heat_pump_duty=CDATA(IPCOMP,1)

C Establish heat loss modulus
        UA=ADATA(IPCOMP,5)
        CALL ECLOSE(PCNTMF(IPCOMP),99.,0.0001,CLOSE)
        IF(CLOSE) UA=0.0

C Set default state (off)
        w_compressor=0.
        Q_water=0.
        Q_air=0.

C If zero flow found then set transfered flux to zero and skip.
        IF(C1.LT.SMALL.OR.C2.LT.SMALL) THEN
          heat_pump_duty=0.
          Q_water=0.
          Q_air=0.
          w_compressor=0.
        ELSE

C Establish if heating or cooling
          if(heat_pump_duty.ge.0.)then
            if(COPh.gt.small) w_compressor=heat_pump_duty/COPh

C heating mode, heat removed from water + compressor work added to 
C air node.
            Q_air=heat_pump_duty+w_compressor
            Q_water=heat_pump_duty
          else
  
C cooling mode, heat removed from air + compressor work added to 
C water node.
            if(COPc.gt.small) w_compressor=-heat_pump_duty/COPc
            Q_water=heat_pump_duty-w_compressor
            Q_air=heat_pump_duty
    
C Calculate effect of possible condensation here. 
C << to be added >>
C end condensation calc. 

          endif
        ENDIF     

C Calculate the electrical demand of the components
        PWRP(IPCOMP)=w_compressor
        IEMODEL=1
        CALL EMACH(IPCOMP,IEMODEL,PWRP(IPCOMP),PQ,PA)
        PWRQ=PQ
          
 
C Calculate current component time-constant TC
        TC(IPCOMP)=AMAX1(
     &CM1/AMAX1(SMALL,(C1+UA)),
     &CM2/AMAX1(SMALL,(C2+UA)))

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
        IF(IMPEXP.EQ.1) THEN
           ALPHA=1.
        ELSE IF(IMPEXP.EQ.2) THEN
           ALPHA=RATIMP
        ELSE IF(IMPEXP.EQ.3) THEN
          IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
             ALPHA=1.
          ELSE
             ALPHA=RATIMP
          END IF
        ELSE IF(IMPEXP.EQ.4) THEN
            CM1=0.
            CM2=0.
            ALPHA=1.
        END IF

C Establish matrix equation self- and cross-coupling coefficients.
        COUT(1)=ALPHA*(-C1)-CM1/TIMSEC
        COUT(2)=ALPHA*(-C2-UA)-CM2/TIMSEC
C Matrix cross coupling coefficients.
        COUT(3)=ALPHA*C1
        COUT(4)=ALPHA*C2
C Establish the present and known coefficient i.e. RHS
        COUT(5)=((1.-ALPHA)*(PCRP(ICON1))-CM1/TIMSEC)
     &          *CSVP(INOD1,1)
     &          -(1.-ALPHA)*PCRP(ICON1)*PCTP(ICON1)
     &          +ALPHA*Q_water
     &          +(1.-ALPHA)*PCQP(INOD1)
         COUT(6)=((1.-ALPHA)*(PCRP(ICON2)+UA)-CM1/TIMSEC)
     &           *CSVP(INOD2,1)
     &          -(1.-ALPHA)*PCRP(ICON2)*PCTP(ICON2)
     &          -(1.-ALPHA)*(UA)*PCNTMP(IPCOMP)
     &          -ALPHA*(UA)*PCNTMF(IPCOMP)
     &          -ALPHA*Q_air
     &          -(1.-ALPHA)*(PCQP(INOD2))
     
C Store "environment" variables future values
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCTF(ICON2)=CONVAR(ICON2,1)
         PCRF(ICON1)=C1
         PCRF(ICON2)=C2
         PCQF(INOD1)=Q_water
         PCQF(INOD2)=Q_air
 
C Addition output for results analysis.
         NAPDAT(IPCOMP)=6
         PCAOUT(IPCOMP,1)=heat_pump_duty
         PCAOUT(IPCOMP,2)=Q_water
         PCAOUT(IPCOMP,3)=Q_air
         PCAOUT(IPCOMP,4)=w_compressor
         PCAOUT(IPCOMP,5)=COPh
         PCAOUT(IPCOMP,6)=COPc
  
C 1st phase mass (ie. "water") balance coefficients

      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=1.
         COUT(3)=-PCONDR(ICON1)
         COUT(4)=-PCONDR(ICON2)
         COUT(5)=0.
         COUT(6)=0.

C 2nd phase mass (ie. "vapour") balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=1.
         COUT(3)=0.
         COUT(4)=-PCONDR(ICON2)
         COUT(5)=0.
         COUT(6)=0.
      END IF


C Trace.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 2 node (ISV=29) WCH reversible heat pump'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2
         WRITE(ITU,*) ' Connection(s)  ',ICON1,ICON2
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM1     = ',CM1,' (J/K)'
            WRITE(ITU,*) ' CM2     = ',CM2,' (J/K)'
            WRITE(ITU,*) ' C1      = ',C1,' (W/Ks)'
            WRITE(ITU,*) ' C2      = ',C2,' (W/K)'
            WRITE(ITU,*) ' TC      = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA   = ',ALPHA,' (-)'
            WRITE(ITU,*) ' T_h     = ',convar(icon1,1),' (C)' 
            WRITE(ITU,*) ' T_c     = ',convar(icon2,1),' (C)'    
            WRITE(ITU,*) ' COPh    = ',COPh,' (-)'
            WRITE(ITU,*) ' COPc    = ',COPc,' (-)'
            WRITE(ITU,*) ' q_air   = ',Q_air,' (W)'
            WRITE(ITU,*) ' q_water = ',Q_water,' (W)' 
            WRITE(ITU,*) ' w_comp  = ',w_compressor,' (W)'                  
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=6
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP36C'

      RETURN
      END

C ******************** CMP37C ********************
C
C CMP37C generates for plant component IPCOMP with plant db code 370 ie.
C 1 node (ISV=20) WCH air cooled or evaporative heat rejector unit
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C            3 UA modulus (W/K)
C     BDATA: 1 Pump or fan parasitic load (W/W of cooling)
C            2 Parasitic load coefficients <<to be added>>
C     CDATA: 1 Cooling duty (W)
C
      SUBROUTINE CMP37C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PTIME/PTIMEP,PTIMEF
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
     
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

     
C Electrical details for specified plant components
      common/pcelflg/ipcelf(mpcom)
      common/elpcp/NPEL,PFP(mpcom),IPFP(mpcom),PWRP(mpcom),
     &BVOLTP(mpcom),IPHP(mpcom)

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      character outs*124
      logical closea

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP37C'

C Check control data for relevant balance type
      IF(ISTATS.EQ.1.AND.CDATA(IPCOMP,1).GT.0.) THEN
         CALL DAYCLK(IDYP,PTIMEF,IUOUT)
         WRITE(outs,*) ' CMP37C: invalid control data for component ',
     &                  IPCOMP,' : ',CDATA(IPCOMP,1)
         call edisp(iuout,outs)
         call edisp(iuout,' CMP37C: unresolvable error.')
         call epwait
         call epagend
         STOP
      END IF

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      INOD1=NPCDAT(IPCOMP,9)

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C First initialize UA modulus (for calculation of containment heat loss)
         UA=ADATA(IPCOMP,3)
         call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
         IF(closea) UA=0.

C Then initialize heat rejected  Q
         Q=CDATA(IPCOMP,1)
 
C Calculate the parasitic loads here. 
         E_parasitic=Q*BDATA(IPCOMP,1)
 

C Polynomial calc of E_parasitic to go here.
C For example.
C        E_parasitic=Q^2*a2+Q*a1+a0 + TE^2*b2+TE*b2+b0
C        TE=ambient air temp. 

         PWRP(IPCOMP)=E_parasitic
         IEMODEL=1
         CALL EMACH(IPCOMP,IEMODEL,PWRP(IPCOMP),PQ,PA)
         PWRQ=PQ

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(3,CONVAR(ICON1,1))

C Calculate current component time-constant TC
         TC(IPCOMP)=CM/AMAX1(SMALL,(C1+UA))

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
         IF(IMPEXP.EQ.1) THEN
            ALPHA=1.
         ELSE IF(IMPEXP.EQ.2) THEN
            ALPHA=RATIMP
         ELSE IF(IMPEXP.EQ.3) THEN
            IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
               ALPHA=1.
            ELSE
               ALPHA=RATIMP
            END IF
         ELSE IF(IMPEXP.EQ.4) THEN
            CM=0.
            ALPHA=1.
         END IF

C Establish matrix equation self- and cross-coupling coefficients
         COUT(1)=ALPHA*(-C1-UA)-CM/TIMSEC
         COUT(2)=ALPHA*C1
C and then present-time coefficient (ie. right hand side)
         COUT(3)=((1.-ALPHA)*(PCRP(ICON1)+PUAP(INOD1))
     &              -CM/TIMSEC)*CSVP(INOD1,1)
     &             +(1.-ALPHA)*(-PCRP(ICON1))*PCTP(ICON1)
     &             -ALPHA*UA*PCNTMF(IPCOMP)
     &             -(1.-ALPHA)*PUAP(INOD1)*PCNTMP(IPCOMP)
     &             -ALPHA*Q-(1.-ALPHA)*PCQP(INOD1)

C Store "environment" variables future values
         PUAF(INOD1)=UA
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCRF(ICON1)=C1
         PCQF(INOD1)=Q
 
C Addition output for results analysis.
         NAPDAT(IPCOMP)=2
         PCAOUT(IPCOMP,1)=Q
         PCAOUT(IPCOMP,2)=E_parasitic

C 1st phase mass (ie. water) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=-PCONDR(ICON1)
         COUT(3)=0.

C 2nd phase mass (ie. vapour) balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=0.
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=20) air cooled or evaporative heat'
         WRITE(ITU,*) ' 1 node (ISV=20) rejection device'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' UA     = ',UA,' (W/K)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
            WRITE(ITU,*) ' CDATA  = ',CDATA(IPCOMP,1)
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=3
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP37C'


      RETURN
      END

C ******************** CMP38C ********************
C CMP38C generates for plant component IPCOMP with plant db code 380 ie.
C 1 node (ISV=20) WCH converging mutli-leg junction (manifold) with up 
C to 10 connections.
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C            3 UA modulus (W/K)
C     BDATA: 1 Number of connections (-)
C     CDATA: none

      SUBROUTINE CMP38C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE),Coeff(10),PCRP_tot,Coeff_tot,CoeffT_tot
      INTEGER   ICON(10)
      logical closea

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP38C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      
      INOD1=NPCDAT(IPCOMP,9)
      NCON=NINT(BDATA(IPCOMP,1))

      DO 5 I=1,10
        ICON(I)=0
        Coeff(I)=0.0
5     CONTINUE

C Set up connection data for up to 10 connections
      PCRP_tot=0.0    
      DO 10 I=1,NCON
        ICON(I)=ICONDX(IPCOMP,1,I)
        if(icon(I).ne.0)then
          PCRP_tot=PCRP_tot+PCRP(ICON(I))
        endif
10    CONTINUE  
    

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C First initialize UA modulus (for calculation of containment heat loss)
        UA=ADATA(IPCOMP,3)
        call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
        IF(closea) UA=0.0

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
        CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)
        Coeff_tot=0.0
        DO 20 J=1,NCON
          if(icon(J).ne.0)then
            Coeff(J)=PCONDR(ICON(J))*CONVAR(ICON(J),2)*
     &        SHTFLD(3,CONVAR(ICON(J),1))
            Coeff_tot=Coeff_tot+Coeff(J)
          endif
20      CONTINUE

C Calculate current component time-constant TC        
        TC(IPCOMP)=CM/AMAX1(SMALL,(Coeff_tot+UA))

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
        IF(IMPEXP.EQ.1) THEN
          ALPHA=1.
        ELSE IF(IMPEXP.EQ.2) THEN
          ALPHA=RATIMP
        ELSE IF(IMPEXP.EQ.3) THEN
          IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
            ALPHA=1.
          ELSE
            ALPHA=RATIMP
          END IF
        ELSE IF(IMPEXP.EQ.4) THEN
          CM=0.
          ALPHA=1.
        END IF

C Establish matrix equation self- and cross-coupling coefficients
        COUT(1)=ALPHA*(-Coeff_tot-UA)-CM/TIMSEC
        DO 30 K=1,10
          COUT(K+1)=ALPHA*Coeff(K)
30      CONTINUE

C and then present-time coefficient (ie. right hand side)
        CoeffT_tot=0.0
        PCRP_tot=0.0
        DO 35 L=1,10
          if(icon(L).ne.0)then
            CoeffT_tot=CoeffT_tot+(-PCRP(ICON(L)))*PCTP(ICON(L))
            PCRP_tot=PCRP_tot+PCRP(ICON(L))
          endif 
35      CONTINUE
        COUT(12)=((1.-ALPHA)*(PCRP_tot+PUAP(INOD1)) 
     &              -CM/TIMSEC)*CSVP(INOD1,1)
     &             +(1.-ALPHA)*CoeffT_tot 
     &             -ALPHA*UA*PCNTMF(IPCOMP)
     &             -(1.-ALPHA)*PUAP(INOD1)*PCNTMP(IPCOMP)

C Store "environment" variables future values
        PUAF(INOD1)=UA

        DO 40 I=1,10
          if(icon(I).ne.0)then
            PCTF(ICON(I))=CONVAR(ICON(I),1)
            PCRF(ICON(I))=Coeff(I)
          else
            continue
          endif
40      CONTINUE

C 1st phase mass (ie. water) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
        COUT(1)=1.
        DO 50 J=1,10
          if(icon(j).ne.0)then
            COUT(J+1)=-PCONDR(ICON(J))
          else
            COUT(J+1)=0.0
          endif
50      CONTINUE
        COUT(12)=0.

C 2nd phase mass (ie. "vapour") balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         DO 60 K=1,10
           COUT(K+1)=0.
60       CONTINUE
         COUT(12)=0.
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=20) WCH multi-leg junction'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         WRITE(ITU,*) ' Connection(s)  ',(ICON(J),J=1,NCON)
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1-10     = ',(Coeff(K),K=1,10),' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' UA     = ',UA,' (W/K)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=12
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP38C'

      RETURN
      END


C ******************** CMP39C ********************

C CMP39C generates for plant component IPCOMP with plant db code 240 ie.
C 1 node (ISV=20) WCH pump with mass flow control
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C            3 UA modulus (W/K)
C     BDATA: 1 Rated absorbed power (W)
C            2 Rated mass flow rate (kg/s)
C            3 Overall efficiency (-)
C     CDATA: 1 Mass flow rate (kg/s)

      SUBROUTINE CMP39C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"
#include "net_flow.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PTIME/PTIMEP,PTIMEF
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

      COMMON/FFN/IFLWN,ICFFS(MPCON)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)

      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

C Electrical details for specified plant components
      common/pcelflg/ipcelf(mpcom)
      common/elpcp/NPEL,PFP(mpcom),IPFP(mpcom),PWRP(mpcom),
     &BVOLTP(mpcom),IPHP(mpcom)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      character outs*124
      logical closea

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP39C'

C Check control data for relevant balance type
      IF(ISTATS.EQ.2.AND.CDATA(IPCOMP,1).LT.0.) THEN
         CALL DAYCLK(IDYP,PTIMEF,IUOUT)
         WRITE(outs,*) ' CMP39C: invalid control data for component ',
     &                  IPCOMP,' : ',CDATA(IPCOMP,1)
         call edisp(iuout,outs)
         call edisp(iuout,' CMP39C: unresolvable error.')
         call epwait
         call epagend
         STOP
      END IF

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      INOD1=NPCDAT(IPCOMP,9)

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C First initialize UA modulus (for calculation of containment heat loss)
         UA=ADATA(IPCOMP,3)
         call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
         IF(closea) UA=0.

C Establish absorbed power E based on current water flow rate which might
C have been calculated by mfs
C In case of mfs E is based on flow rate, pressure *rise*, and efficiency
         IF(IFLWN.NE.0) THEN
            ICNN=ICFFS(ICON1)
            E=real((FLW1(ICNN)+FLW2(ICNN))
     &        *(PRES(NODNE(ICNN))-PRES(NODPS(ICNN)))
     &        /(BDATA(IPCOMP,3)*RHOFLD(3,CSVF(INOD1,1))))
         ELSE
            E=((CSVF(INOD1,2)
     &         /BDATA(IPCOMP,2))**3)*BDATA(IPCOMP,1)
         END IF

         PWRP(IPCOMP)=-ABS(E)
         IEMODEL=1
         CALL EMACH(IPCOMP,IEMODEL,PWRP(IPCOMP),PQ,PA)
         PWRQ=PQ

C Now Q is made up of all inefficiencies
         Q=E

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(3,CONVAR(ICON1,1))

C Calculate current component time-constant TC
         TC(IPCOMP)=CM/AMAX1(SMALL,(C1+UA))

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
         IF(IMPEXP.EQ.1) THEN
            ALPHA=1.
         ELSE IF(IMPEXP.EQ.2) THEN
            ALPHA=RATIMP
         ELSE IF(IMPEXP.EQ.3) THEN
            IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
               ALPHA=1.
            ELSE
               ALPHA=RATIMP
            END IF
         ELSE IF(IMPEXP.EQ.4) THEN
            CM=0.
            ALPHA=1.
         END IF

C Establish matrix equation self- and cross-coupling coefficients
         COUT(1)=ALPHA*(-C1-UA)-CM/TIMSEC
         COUT(2)=ALPHA*C1
C and then present-time coefficient (ie. right hand side)
         COUT(3)=((1.-ALPHA)*(PCRP(ICON1)+PUAP(INOD1))
     &              -CM/TIMSEC)*CSVP(INOD1,1)
     &             +(1.-ALPHA)*(-PCRP(ICON1))*PCTP(ICON1)
     &             -ALPHA*UA*PCNTMF(IPCOMP)
     &             -(1.-ALPHA)*PUAP(INOD1)*PCNTMP(IPCOMP)
     &             -ALPHA*Q-(1.-ALPHA)*PCQP(INOD1)

C Store "environment" variables future values
         PUAF(INOD1)=UA
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCRF(ICON1)=C1
         PCQF(INOD1)=Q

C Save plant additional output data.
          napdat(ipcomp)=2
          pcaout(ipcomp,1)=pwrp(ipcomp)
          pcaout(ipcomp,2)=pwrq

C 1st phase mass (ie. water) balance coefficients
C Note that if fluid mass flow solver active, source of mass zeroised
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         IF(IFLWN.EQ.0) THEN
            COUT(2)=0.
            COUT(3)=CDATA(IPCOMP,1)
         ELSE
            COUT(2)=-PCONDR(ICON1)
            COUT(3)=0.
         END IF

C 2nd phase mass (ie. "vapour") balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=0.
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=20) WCH pump (mass flow control)'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' UA     = ',UA,' (W/K)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
            WRITE(ITU,*) ' Q      = ',Q,' (W)'
            WRITE(ITU,*) ' E      = ',E,' (W)'
            WRITE(ITU,*) ' E,cum. = ',E*TIMSEC,' (J)'
         ELSE IF(ISTATS.EQ.2) THEN
            WRITE(ITU,*) ' CDATA  = ',CDATA(IPCOMP,1)
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=3
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP39C'

      RETURN
      END

C ******************** CMP42C ********************

C CMP42C generates for plant component IPCOMP with plant db code 420 ie.
C 2 node (ISV=20) WCH boiler with modulation
C This boiler is exactly similar to the WCH 2 node boiler with ON/OFF
C control except that full load gas firing rate BDATA(?,1) is modified
C at run time depending upon return water temperature.
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C     BDATA: 1 Full load gas firing rate when boiler on (m^3/s)
C            2 Dump load fraction
C            3 Gas heating value at STP (J/m^3)
C            4 Boiler lock out time (min)
C            5 Boiler heat loss to surroundings UA (W/K)
C            6 DHW draw schedule control loop no. if combiboiler (0 otherwise)
C            7 DHW calorifier component number (0 otherwise)
C            8 Constant cold water feed temperature for DHW for combiboiler (degC)
C            9 Constant supply temperature for DHW for combiboiler(degC)
C           10 Upper boiler temperature limit (C)
C           11 Lower limit of modulating range (%), 0 for nonmodulating
C           12 Lower limit total differential (%), 0 for nonmodulating
C        13-21 Coefficients a1 to a9 for efficiency equation
C     CDATA: 1 ON/OFF control signal (-)

C     PCDATF/P
C            1 Holds boiler actual ON/OFF state (-)
C            2 Inter-node fluid heat capacity rate (W/K)

      SUBROUTINE CMP42C(IPCOMP,COUT,ISTATS)
      IMPLICIT NONE
#include "plant.h"
#include "building.h"
#include "control.h"
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      INTEGER ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      INTEGER ITCF,ITRACE,IZNTRC,ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      INTEGER IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      REAL TIMSEC
      COMMON/PTIME/PTIMEP,PTIMEF
      REAL PTIMEP,PTIMEF
      COMMON/PCTC/TC(MPCOM)
      REAL TC
      COMMON/PCEQU/IMPEXP,RATIMP
      INTEGER IMPEXP
      REAL RATIMP
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)
      INTEGER MAXITP,ITRCLP,ICSV
      REAL PERREL,PERTMP,PERFLX,PERMFL,CSVI
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      INTEGER NPCOMP,NCI
      REAL CDATA
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      INTEGER NPCON,IPC1,IPN1,IPCT,IPC2,IPN2
      REAL PCONDR,PCONSD
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      INTEGER NPCDAT,IPOFS1,IPOFS2
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      REAL ADATA,BDATA
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      REAL CSVF,CSVP
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      REAL PCTF,PCRF,PUAF,PCQF,PCNTMF,PCTP,PCRP,PUAP,PCQP,PCNTMP
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      REAL CONVAR
      INTEGER ICONTP,ICONDX
      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
      REAL PCDATF,PCDATP
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER NAPDAT
      COMMON/LOCAL1/ISET,PHIMAX
      INTEGER ISET
      REAL PHIMAX
      COMMON/LOKOUT/INSO,ITSBO
      INTEGER INSO,ITSBO
      COMMON/PCLOP8/LASTOUT(MCF)
      INTEGER LASTOUT
      REAL SMALL
      PARAMETER (SMALL=1.0E-20)
      REAL      COUT(MPCOE),TOTMAS,FLGFR,UA,TF,GFR,EFF_ON,GMAX,
     & RUBTL,A1,A2,A3,A4,A5,A6,A7,A8,A9,CUTIN,CUTOUT,FEEDTM,SUPPTM,RI,
     & RO,HK,PHIW,A,TR,ZMFR,QL,TRD,G,Q,PHI,QLD,BLKOUT,FGAS,PHISB,CM,
     & ALPHA,C1,SHTFLD
      character outs*124
      LOGICAL CLOSEA
      INTEGER ICON1,INOD1,INOD2,IONOFF,ICOMBI,ICCOMP,ICOMBIMOD,ICCN1,
     & IPC,NTSBO,IX1,I,NITMS,IPCON,ISTATS,IPCOMP
 
C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP42C'

      ICON1=ICONDX(IPCOMP,1,1)
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1

C Check control data for relevant balance type
      IF(ISTATS.EQ.1.AND.
     &   (CDATA(IPCOMP,1).LT.0..OR.CDATA(IPCOMP,1).GT.1.)) THEN
         CALL DAYCLK(IDYP,PTIMEF,IUOUT)
         WRITE(outs,*) ' CMP42C: invalid control data for component ',
     &                  IPCOMP,' : ',CDATA(IPCOMP,1)
         call edisp(iuout,outs)
         call edisp(iuout,' CMP42C: unresolvable error.')
         call epwait
         call epagend
         STOP
      END IF

C Scale down parameters if dump load is to be supplied.
      TOTMAS=ADATA(IPCOMP,1)*(1.-BDATA(IPCOMP,2))
      FLGFR=BDATA(IPCOMP,1)*(1.-BDATA(IPCOMP,2))
      UA=BDATA(IPCOMP,5)*(1.-BDATA(IPCOMP,2))

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      IF(ISTATS.EQ.1) THEN
      TF=BDATA(IPCOMP,10)
      IF(ISET.EQ.0)THEN
        GFR=1.0
        EFF_ON=1.
        PHIMAX=FLGFR*BDATA(IPCOMP,3)*EFF_ON
        GMAX=FLGFR*BDATA(IPCOMP,3)
        ISET=1
        RUBTL=BDATA(IPCOMP,10)
      ENDIF

C Establish whether boiler is on or off (ie 1 or 0)
      IONOFF=int(CDATA(IPCOMP,1))

C Set up coefficients for efficiency equation
      A1=BDATA(IPCOMP,13)
      A2=BDATA(IPCOMP,14)
      A3=BDATA(IPCOMP,15)
      A4=BDATA(IPCOMP,16)
      A5=BDATA(IPCOMP,17)
      A6=BDATA(IPCOMP,18)
      A7=BDATA(IPCOMP,19)
      A8=BDATA(IPCOMP,20)
      A9=BDATA(IPCOMP,21)

C Get gas firing modulation cut-in and cut-out
      CUTIN=(BDATA(IPCOMP,11)+BDATA(IPCOMP,12)*0.5)*0.01
      CUTOUT=(BDATA(IPCOMP,11)-BDATA(IPCOMP,12)*0.5)*0.01

C Determine if combi-boiler or not
      ICOMBI=INT(BDATA(IPCOMP,6))
      ICCOMP=INT(BDATA(IPCOMP,7))
      FEEDTM=BDATA(IPCOMP,8)
      SUPPTM=BDATA(IPCOMP,9)
      ICOMBIMOD=0

C If boiler is combi and is supplying DHW determine firing rate 
      IF(ICOMBI.NE.0)THEN
        IF(LASTOUT(ICOMBI).NE.0)THEN
          ICOMBIMOD=1
          ICCN1=ICONDX(ICCOMP,1,1)
          RI=1./(BDATA(ICCOMP,1)*BDATA(ICCOMP,2))
          RO=1./(BDATA(ICCOMP,3)*BDATA(ICCOMP,4))
          HK=1./(RI+RO)
          PHIW=(PCONDR(ICCN1)*CONVAR(ICCN1,2)*SHTFLD(3,CONVAR(ICCN1,1)))
     &         *(SUPPTM-FEEDTM)
          A=0.5*HK/
     &      (PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(3,CONVAR(ICON1,1)))
          TR=((1.-A)*CSVF(INOD2,1)+A*(SUPPTM+FEEDTM))/(1.+A)
          IF(TR.LT.FEEDTM)TR=FEEDTM
          IF(TR.GT.CSVF(INOD2,1))TR=CSVF(INOD2,1)
          ZMFR=PCONDR(ICON1)*CONVAR(ICON1,2)
          QL=1.
          TRD=TR
          IF(TR.LT.30.)TRD=30.
          IF(TR.GT.70.)TRD=70.
          EFF_ON=(A1*TRD+A2*(TRD**2.)+A3*(TRD**3.)+A4*(TRD**4.)
     &         +A5*QL+A6*TRD*QL+A7*(TRD**2.)*QL+A8+A9*(QL**2.))/100.
          PHIMAX=FLGFR*BDATA(IPCOMP,3)*EFF_ON
          QL=PHIW/PHIMAX
          IF(QL.GT.1.)THEN
            QL=1.
            PHIW=PHIMAX
          ENDIF
          EFF_ON=(A1*TRD+A2*(TRD**2.)+A3*(TRD**3.)+A4*(TRD**4.)
     &         +A5*QL+A6*TRD*QL+A7*(TRD**2.)*QL+A8+A9*(QL**2.))/100.
          PHIMAX=FLGFR*BDATA(IPCOMP,3)*EFF_ON
          G=PHIW/EFF_ON
          GFR=G/(FLGFR*BDATA(IPCOMP,3))

C Consider decreasing modulation lower limit to below 30% otherwise 
C low volumes of water draw do not switch boiler on
          IF(GFR.LT.CUTIN)GFR=0.
          IF(GFR.GT.1.)GFR=1.
          Q=PHIW
        ENDIF
      ENDIF

C Set boiler on if there is demand for hot water from a combi boiler
      IF(ICOMBIMOD.EQ.1)IONOFF=1
      IF(ICOMBIMOD.EQ.1)GOTO 33

C TR=Return water temperature, QL=Load
C TF=Flow water (temperature design value of boiler) to be controlled in
C ZMFR=Water mass flow rate though boiler
C G=heat supplied by gas (gas firing rate*calorifc value)
C GFR=gas firing rate
C Get connection which sends mass to boiler, assuming only one 
C component sends mass to the boiler
      DO 1515 IPC=1,NPCON
        IF(IPC1(IPC).EQ.IPCOMP)IPCON=IPC
 1515 CONTINUE
      TR=CONVAR(IPCON,1)
      IF(TR.GT.TF)TR=TF
      ZMFR=PCONDR(ICON1)*CONVAR(ICON1,2)
      QL=1.
      TRD=TR
      IF(TR.LT.30.)TRD=30.
      IF(TR.GT.70.)TRD=70.
      EFF_ON=(A1*TRD+A2*(TRD**2.)+A3*(TRD**3.)+A4*(TRD**4.)
     &         +A5*QL+A6*TRD*QL+A7*(TRD**2.)*QL+A8+A9*(QL**2.))/100.
      PHI=ZMFR*SHTFLD(3,CONVAR(ICON1,1))*(TF-TR)
      IF(PHI.GT.PHIMAX)PHI=PHIMAX
      QL=PHI/PHIMAX
      IF (BDATA(IPCOMP,12).NE.0) THEN

C Condensing boiler
        EFF_ON=(A1*TRD+A2*(TRD**2.)+A3*(TRD**3.)+A4*(TRD**4.)
     &         +A5*QL+A6*TRD*QL+A7*(TRD**2.)*QL+A8+A9*(QL**2.))/100.
      ELSE

C non condensing - non modulating (QLD=1)
        QLD=1
        EFF_ON=(A1*TRD+A2*(TRD**2.)+A3*(TRD**3.)+A4*(TRD**4.)
     &         +A5*QLD+A6*TRD*QLD+A7*(TRD**2.)*QLD+A8+A9*(QL**2.))/100.
      ENDIF      
      G=PHI/EFF_ON
      GFR=G/GMAX

C Reset to off in case maximum temperature exceeded
      IF(IONOFF.EQ.1)THEN
        IF(GFR.GT.1.)THEN
          GFR=1.
        ELSEIF(GFR.LT.CUTOUT)THEN
          GFR=0.
          IONOFF=0
        ENDIF
      ELSE
        IF(GFR.GT.1.)THEN
          GFR=1.
        ELSEIF(GFR.LT.CUTIN)THEN
          GFR=0.
        ENDIF
      ENDIF

C Lock out time (in seconds), If boiler temperature reaches upper
C boiler temperature limit it is shut down for this amount of time
C INSO = boiler on/off (1/0) flag for lockout
C ITSBO = present no. of timesteps boiler is off due to lockout
C NTSBO = total no. of timesteps boiler is off due to lockout
 33     BLKOUT=BDATA(IPCOMP,4)*60.       
        NTSBO=INT(BLKOUT/TIMSEC)
        IF(CSVF(INOD2,1).GT.RUBTL)THEN
          INSO=1
          ITSBO=0
        ENDIF
        IF(ITSBO.LE.0)ITSBO=0
        IF(INSO.EQ.1)THEN
          ITSBO=ITSBO+1
          IF(ITSBO.GE.NTSBO)INSO=0
          Q=0.0
          IONOFF=0
          FGAS=0.
        ENDIF
        IF(INSO.EQ.1)GOTO 333

C First calculate boiler efficiency ETA based on current node 1 temp.
C so mark node 1 temperature for iteration
        ICSV(INOD1,1)=1
        CSVI(INOD1,1)=CSVF(INOD1,1)
 
C Then calculate heat input into the water PHIW if boiler is on (assume
C that boiler comes on regardless of control loop if there is demand for
C hot water if it is a combiboiler)
        PHIW=0.0
        FGAS=0.0
        IF(IONOFF.EQ.1.OR.ICOMBIMOD.EQ.1) THEN
          PHISB=0.
          FGAS=FLGFR
          IF (NINT(BDATA(IPCOMP,12)).NE.0) THEN 
            FGAS=GMAX*GFR/BDATA(IPCOMP,3)
          ENDIF   
          PHIW=EFF_ON*FGAS*BDATA(IPCOMP,3)
        ENDIF

C Then calculate net heat input Q
        Q=PHIW

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
 333    CM=TOTMAS*ADATA(IPCOMP,2)/2.
        C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(3,CONVAR(ICON1,1))

C Boiler actual ON/OFF state PCDATF(IPCOMP,1) (-)
        PCDATF(IPCOMP,1)=IONOFF

C Inter-node fluid heat capacity rate (W/K)
        PCDATF(IPCOMP,2)=CSVF(INOD1,2)*SHTFLD(3,CSVF(INOD1,1))

C Calculate current component time-constant TC
        TC(IPCOMP)=AMAX1(
     &        CM/AMAX1(SMALL,(C1)),CM/AMAX1(SMALL,(PCDATF(IPCOMP,2))))

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
        IF(IMPEXP.EQ.1) THEN
          ALPHA=1.
        ELSE IF(IMPEXP.EQ.2) THEN
          ALPHA=RATIMP
        ELSE IF(IMPEXP.EQ.3) THEN
          IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
            ALPHA=1.
          ELSE
            ALPHA=RATIMP
          END IF
        ELSE IF(IMPEXP.EQ.4) THEN
          CM=0.
          ALPHA=1.
        ENDIF

C Establish matrix equation self-coupling coefficients,
C  node 1
        COUT(1)=ALPHA*(-C1-UA)-CM/TIMSEC

C  node 2
        COUT(2)=ALPHA*(PCDATF(IPCOMP,2)+UA)
        COUT(3)=ALPHA*(-PCDATF(IPCOMP,2)-UA)-CM/TIMSEC

C then matrix equation cross-coupling coefficient,
        COUT(4)=ALPHA*C1

C and then present-time coefficients (ie. right hand sides)
        COUT(5)=((1.-ALPHA)*(PCRP(ICON1)+UA)-CM/TIMSEC)*CSVP(INOD1,1)
     &           +(1.-ALPHA)*(-PCRP(ICON1))*PCTP(ICON1)
     &           -(1.-ALPHA)*PUAP(INOD1)*PCNTMP(IPCOMP)
     &           -ALPHA*UA*PCNTMF(IPCOMP)-(1.-ALPHA)*UA*PCNTMP(IPCOMP)

        COUT(6)=(1.-ALPHA)*(-PCDATP(IPCOMP,2)-UA)*CSVP(INOD1,1)
     &           +((1.-ALPHA)*(UA+PCDATP(IPCOMP,2))
     &           -CM/TIMSEC)*CSVP(INOD2,1)-ALPHA*Q-
     &           (1.-ALPHA)*PCQP(INOD2)

C Store "environment" variables future values
        PCTF(ICON1)=CONVAR(ICON1,1)
        PCRF(ICON1)=C1
        PCQF(INOD2)=Q

C Establish "containment loss" data
        QDATA(IPCOMP)=0.

C If boiler is off set its efficiency to zero
        IF(IONOFF.EQ.0)EFF_ON=0.

C Establish additional output variables
C FGAS*1000 is to give resolution on output
c this will translate from m3/s to l/s??
        napdat(ipcomp)=5
        PCAOUT(IPCOMP,1)=IONOFF
        PCAOUT(IPCOMP,2)=FGAS*1000.
        PCAOUT(IPCOMP,3)=TF
        PCAOUT(IPCOMP,4)=PHIW
        PCAOUT(IPCOMP,5)=EFF_ON

C 1st phase mass (ie. water) balance coefficients
      ENDIF
      IF(ISTATS.EQ.2) THEN
        COUT(1)=1.
        COUT(2)=-1.
        COUT(3)=1.
        COUT(4)=-PCONDR(ICON1)
        COUT(5)=0.
        COUT(6)=0.

C 2nd phase mass (ie. "vapour") balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
        COUT(1)=1.
        COUT(2)=0.
        COUT(3)=1.
        COUT(4)=0.
        COUT(5)=0.
        COUT(6)=0.
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 2 node (ISV=20) WCH boiler & modulation'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
            WRITE(ITU,*) ' CDATA  = ',CDATA(IPCOMP,1)
            WRITE(ITU,*) ' IONOFF = ',IONOFF,' (-)'
            WRITE(ITU,*) ' FGAS   = ',FGAS,' (m^3/s)'
            WRITE(ITU,*) ' PHIW   = ',PHIW,' (W)'
            WRITE(ITU,*) ' PHISB  = ',PHISB,' (W)'
            WRITE(ITU,*) ' GASCO  = ',FGAS*TIMSEC,' (m^3)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=6
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP42C'

      RETURN
      END

C ******************** CMP44C ********************
C CMP44C generates for plant component IPCOMP with plant db code 440 ie.
C 2 node (ISV=20) WCH exponent radiator model
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C     BDATA: 1 Nominal heat supply rate (W)
C            2 Nominal supply water temperature (C)
C            3 Nominal environment temperature (C)
C            4 Nominal exit water temperature (C)
C            5 Radiator Exponent (-)
C            6 Index of 1st wall for defining Te (-)
C            7 Weighting factor for 1st wall when defining Te (-)
C            8 Index of 2nd wall for defining Te (-)
C            9 Weighting factor for 2nd wall when defining Te (-)
C           10 Index of coupled building zone (0-n)
C           11 Number of walls used for defining Te (0-n)
C
C If no zone or wall surface is specified then the nominal environment 
C temperature value is used
C
C     CDATA: none

C     PCDATF/P
C            1 Inter-node fluid heat capacity rate (W/K)

      SUBROUTINE CMP44C(IPCOMP,COUT,ISTATS)
      IMPLICIT NONE
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      INTEGER ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      INTEGER ITCF,ITRACE,IZNTRC,ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      INTEGER IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      REAL TIMSEC
      COMMON/PCTC/TC(MPCOM)
      REAL TC
      COMMON/PCEQU/IMPEXP,RATIMP
      INTEGER IMPEXP
      REAL RATIMP
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)
      INTEGER MAXITP,ITRCLP,ICSV
      REAL PERREL,PERTMP,PERFLX,PERMFL,CSVI
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      INTEGER NPCOMP,NCI
      REAL CDATA
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      INTEGER NPCON,IPC1,IPN1,IPCT,IPC2,IPN2
      REAL PCONDR,PCONSD
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      INTEGER NPCDAT,IPOFS1,IPOFS2
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      REAL ADATA,BDATA
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      REAL CSVF,CSVP
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      REAL PCTF,PCRF,PUAF,PCQF,PCNTMF,PCTP,PCRP,PUAP,PCQP,PCNTMP
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      REAL CONVAR
      INTEGER ICONTP,ICONDX
      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
      REAL PCDATF,PCDATP
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER NAPDAT
      COMMON/C6/INDCFG
      INTEGER INDCFG
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      REAL TFA,QFA
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      REAL TFS,QFS
      REAL SMALL
      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE),PHI_N,TS_N,TE_N,TX_N,RADEX,XKVALUE,TE,SUMT,
     & SUMW,W,TS,TX,T_rad_mean,Q1,Q2,Q,CM,C1,ALPHA,SHTFLD
      logical closea
      INTEGER ICON1,INOD1,INOD2,IZ,NW,IW,IX1,I,NITMS,ISTATS,IPCOMP

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP44C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C Initialize parameters value for the radiator
         PHI_N=BDATA(IPCOMP,1)
         TS_N=BDATA(IPCOMP,2)
         TE_N=BDATA(IPCOMP,3)
         TX_N=BDATA(IPCOMP,4)
         RADEX=BDATA(IPCOMP,5)
         XKVALUE=PHI_N*((0.5*(TS_N+TX_N)-TE_N)**(-1.*RADEX))

C Then evaluate the current environment temperature
C If this is a plant only simulation or if no zone has been defined then
C set the environmental temperature to the nominal value specified in the 
C data.
         IF(INDCFG.EQ.2.OR.NINT(BDATA(IPCOMP,10)).EQ.0) THEN
            call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
            IF(closea) THEN
               TE=TE_N
            ELSE
               TE=PCNTMF(IPCOMP)
            ENDIF

C Otherwise get the zone/surface temperatures.
         ELSE
            IZ=INT(BDATA(IPCOMP,10))
            SUMT=TFA(IZ)

C            NW=int(BDATA(IPCOMP,11))
C Just consider two maximum of two walls currently
            NW=2

            SUMW=1.
            IF(NW.GE.1.) THEN
               DO 10 IW=1,NW
                  W=BDATA(IPCOMP,5+IW*2)
                  SUMW=SUMW+W
                  SUMT=SUMT+W*TFS(IZ,NINT(BDATA(IPCOMP,4+IW*2)))
   10          CONTINUE
            END IF
            TE=SUMT/SUMW
         END IF
 
C Mark the nodal temperatures for iteration.
         ICSV(INOD1,1)=1
         CSVI(INOD1,1)=CSVF(INOD1,1)
         ICSV(INOD2,1)=1
         CSVI(INOD2,1)=CSVF(INOD2,1)
         TS=CSVF(INOD1,1)
         TX=CSVF(INOD2,1)
         T_rad_mean=0.5*(TS+TX)

C Then calculate radiator heat emission Q 
           IF(TS.GT.TE)THEN
             Q1=0.5*XKVALUE*(TS-TE)**RADEX
           ELSE
             Q1=0.0
           ENDIF
           IF(TX.GT.TE)THEN
             Q2=0.5*XKVALUE*(TX-TE)**RADEX
           ELSE
             Q2=0.
           ENDIF
           Q=Q1+Q2

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)/2.
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(3,CONVAR(ICON1,1))

C Inter-node fluid heat capacity rate (W/K)
         PCDATF(IPCOMP,1)=CSVF(INOD1,2)*SHTFLD(3,CSVF(INOD1,1))

C Calculate current component time-constant TC
         TC(IPCOMP)=AMAX1(
     &        CM/AMAX1(SMALL,(C1)),
     &        CM/AMAX1(SMALL,(PCDATF(IPCOMP,1)+
     &        Q/AMAX1(SMALL,AMAX1(SMALL,(T_rad_mean-TE))))))

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
         IF(IMPEXP.EQ.1) THEN
            ALPHA=1.
         ELSE IF(IMPEXP.EQ.2) THEN
            ALPHA=RATIMP
         ELSE IF(IMPEXP.EQ.3) THEN
            IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
               ALPHA=1.
            ELSE
               ALPHA=RATIMP
            END IF
         ELSE IF(IMPEXP.EQ.4) THEN
            CM=0.
            ALPHA=1.
         END IF

C Establish matrix equation self-coupling coefficients,
C  node 1
         COUT(1)=ALPHA*(-C1)-CM/TIMSEC
C  node 2
         COUT(2)=ALPHA*PCDATF(IPCOMP,1)
         COUT(3)=ALPHA*(-PCDATF(IPCOMP,1))-CM/TIMSEC
C then matrix equation cross-coupling coefficient,
         COUT(4)=ALPHA*C1
C and then present-time coefficients (ie. right hand sides)
         COUT(5)=((1.-ALPHA)*PCRP(ICON1)-CM/TIMSEC)*CSVP(INOD1,1)
     &           +(1.-ALPHA)*(-PCRP(ICON1))*PCTP(ICON1)
     &           +ALPHA*Q1+(1.-ALPHA)*PCQP(INOD1)
         COUT(6)=(1.-ALPHA)*(-PCDATP(IPCOMP,1))*CSVP(INOD1,1)
     &           +((1.-ALPHA)*PCDATP(IPCOMP,1)-CM/TIMSEC)*CSVP(INOD2,1)
     &           +ALPHA*Q2+(1.-ALPHA)*PCQP(INOD2)

C Store "environment" variables future values
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCRF(ICON1)=C1
         PCQF(INOD1)=Q1
         PCQF(INOD2)=Q2

C Establish "containment loss" data
         QDATA(IPCOMP)=ALPHA*Q+(1.-ALPHA)*(PCQP(INOD2)+PCQP(INOD1))

C Establish additional output variables
         NAPDAT(IPCOMP)=3
         PCAOUT(IPCOMP,1)=Q
         PCAOUT(IPCOMP,2)=0.5*(TS+TX)
         PCAOUT(IPCOMP,3)=TE

C 1st phase mass (ie. water) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=-1.
         COUT(3)=1.
         COUT(4)=-PCONDR(ICON1)
         COUT(5)=0.
         COUT(6)=0.

C 2nd phase mass (ie. "vapour") balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=1.
         COUT(4)=0.
         COUT(5)=0.
         COUT(6)=0.
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 2 node (ISV=20) WCH basic radiator'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' T_rad_mean = ',T_rad_mean,' (C)'
            WRITE(ITU,*) ' TE     = ',TE,' (C)'
            WRITE(ITU,*) ' Q      = ',Q,' (W)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=6
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP44C'

      RETURN
      END

C ******************** CMP45C ********************
C CMP45C generates for plant component IPCOMP with plant db code 450 ie.
C 1 node (ISV=20) WCH air-source heat pump feeding a hydronic heating system.
C the matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)

C     ADATA: 1 Mass of component (solids+liquid) (kg)
C            2 Mass weighted average specific heat node (J/kgK)
C            3 UA modulus for component (W/K)

C     BDATA: 1 COP model [1 - fixed COP 2 - modified Carnot efficiency; 3- f(dT) 4 - polynomial]
C            2 Model coef a0
C            3 Model coef a1
C            4 Model coef a2
C            5 Model coef a3
C            6 Device power draw (kW) model [1 - fixed 2- polynomial]
C            7 Model coef b0
C            8 Model coef b1 
C            9 Model coef b2
C           10 Model coef b3
C           11 Compressor pf (-)
C           12 Pump rating (W)
C           13 Pump pf (-)
C           14 Flowrate at rated pump power (l/s)
C           15 Fan power (W)
C           16 Fan pf (-)
C           17 Controller power (W)
C           18 Controller pf (-)
C           19 Tout max (degC)
C           20 Tin max (degC)
C           21 Defrost cycle trigger ambient temp (degC)
C           22 Defrost cycle time calc (1-user def 2-f(RH))
C           23 Defrost cycle  calc coefficient b1/fixed defrost cycle  time  (-)
C           24 Defrost cycle  calc coefficient b2 (-)
C           25 Defrost cycle  lockout time (mins)
C           26 Min defrost time (mins)
C           26 Max defrost time (mins)
C           27 Temp compensation on/off (-)
C           28 Nominal water return temperature (Deg C)
C           29 Nominal water return deadband (Deg C)
C           30 Ambient temperature for temp compensation start [Deg C]
C           31 Ambient temperature for temp compensation end [Deg C]
C           32 Temp compensation gradient [deg C return/deg C ambient] (degCr/degCa)


C     CDATA: 1 Call for heat [0 or 1] (-)

C Node 1 represents the condenser heat exchanger and couples to the hydronic heating circuit. 
C The device has an internal pump and so an explicit pump model is not needed in system 
C models containing this component. This particular device model is suitable for ON/OFF type control
C and will require modification if PID-type control is to be applied. The model internally controls the 
C water outlet temperature and ambient temperature compensation can be applied [linear model]

      SUBROUTINE CMP45C(IPCOMP,COUT,ISTATS)

      implicit none
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/ITERINDEX/ITERNU !plant iteration number

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)

      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      common/pcnam/pcname(mpcom)

      COMMON/CLIMIP/QFPP,QFFP,TPP,TFP,QDPP,QDFP,VPP,VFP,DPP,DFP,HPP,HFP

      COMMON/ASHPvar1/CompMass,AveSpHt,UAMod,CarEffMod,COPa0,COPa1,
     &COPa2,COPa3,Compa0,Compa1,Compa2,Compa3,
     &CompressPf,PumpRating,PumpPf,RatedFlow,
     &FanRating,FanPf,CtlRating,CtlPf,ToutMax,TinMax,DefrostT,
     &DefrostTime,Defrostb0,Defrostb1,DefrostLockout,DefrostMinTime,
     &DefrostMaxTime,NomRetT,NomRetTDeadB,TempCompS,TempCompE,
     &TempCompc0,DefrostDur,DefrostLockDur

      COMMON/ASHPvar2/COPModel,CompModel,DefrostCalc,AmbientTempComp,
     &DeviceONOFFp,DeviceONOFF

      COMMON/ASHPvar3/CallforHeat,InDeadB,DefrostLock,InDefrost

      REAL SMALL
      PARAMETER (SMALL=1.0E-20)
      
      LOGICAL CallforHeat,InDeadB,DefrostLock,InDefrost

      INTEGER COPModel,CompModel,DefrostCalc,AmbientTempComp,
     &DeviceONOFFp,DeviceONOFF

      REAL CompMass,AveSpHt,UAMod,CarEffMod,COPa0,COPa1,
     &COPa2,COPa3,Compa0,Compa1,Compa2,Compa3,CompressPf,
     &PumpRating,PumpPf,RatedFlow,FanRating,FanPf,CtlRating,CtlPf,
     &ToutMax,TinMax,DefrostTime,DefrostT,Defrostb0,Defrostb1,
     &DefrostLockout,DefrostMinTime,DefrostMaxTime,NomRetT,NomRetTDeadB,
     &TempCompS,TempCompE,TempCompc0,DefrostDur,DefrostLockDur

      REAL DefrostStat,CompPower,CompressPower,COP,CtlPower,DeviceFlow,
     &FanPower,HeatOutput,PumpPower,ReturnSP,ReturnSPH,ReturnSPL,RHamb,
     &TotApparentPower,TotReacPower,TotRealPower

      INTEGER lnblnk
      REAL SHTFLD
 
      INTEGER IUOUT,IUIN,ITC,ICNT,ITCF,ITRACE,IZNTRC,ITU,ITERNU,
     &IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS,IMPEXP
      INTEGER NPCDAT,IPOFS1,IPOFS2,ICON1,INOD1,napdat,
     &IPCOMP,ISTATS,I,ICONDX,ICONTP,IX1,NCI,NITMS,NPCOMP

      REAL TIMSEC,TC,RATIMP,CSVF,CSVP,PCTF,PCRF,PUAF,PCQF,PCNTMF,
     &PCTP,PCRP,PUAP,PCQP,PCNTMP,CONVAR,QFPP,QFFP,TPP,TFP,QDPP,QDFP,VPP
      REAL VFP,DPP,DFP,HPP,HFP,Tamb,ReturnT,Td,Ta,TL,TH,ALPHA,CM,C1,
     &TmpDev

      REAL COUT(MPCOE),QDATA,PCAOUT,CDATA
      
      LOGICAL CLOSE,CLOSEA

      CHARACTER OUTS*248,PCNAME*15,CHAR_TEMP*128
      

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP45C'

C **NB** Variable Assignment and initilalisation in CMP40S

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      INOD1=NPCDAT(IPCOMP,9)         

      call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
      IF(closea) UAMod=0.

C Read ambient temperature
      Tamb=TFP

C Set initial operation mode based on CDATA. 
      IF(CDATA(IPCOMP,1).GT.0.0) THEN 
        CallForHeat=.true.

C Pump circulates while there is a call for heat.
        PumpPower=PumpRating
        DeviceFlow=RatedFlow
      ELSE
        CallforHeat=.false.
        DeviceFlow=0. 
        PumpPower=0. 
      ENDIF


C Calculate the return water temperature set point if temperature compensation is active.
      ReturnSP=NomRetT !default return temperature
      ReturnSPL=ReturnSP-0.5*(NomRetTDeadB)
      ReturnSPH=ReturnSP+0.5*(NomRetTDeadB)
      IF(ReturnSP.GT.TinMax) ReturnSP=TinMax

      IF(AmbientTempComp.GT.0)THEN

        IF(Tamb.GE.TempCompS.AND.Tamb.LT.TempCompE)THEN
          ReturnSP=((Tamb-TempCompS)*TempCompc0)+NomRetT
          ReturnSPL=ReturnSP-0.5*(NomRetTDeadB)
          ReturnSPH=ReturnSP+0.5*(NomRetTDeadB)

C Case where Tamb > Temp compensation saturation point
        ELSEIF(Tamb.GE.TempCompE)THEN
          ReturnSP=((TempCompE-TempCompS)*TempCompc0)+NomRetT
          ReturnSPL=ReturnSP-0.5*(NomRetTDeadB)
          ReturnSPH=ReturnSP+0.5*(NomRetTDeadB)
        ENDIF
        IF(ReturnSP.GT.TinMax)THEN 
          ReturnSP=TinMax
          ReturnSPH=ReturnSP
        ENDIF
      ENDIF

C Determine whether machine should be on or off based on return water temperature. 
      ReturnT=CONVAR(ICON1,1)
      DeviceONOFFp=DeviceONOFF
      IF(CallforHeat)THEN

C Check to see if the device is in the dead band.
        IF(ReturnT.GE.ReturnSPL.AND.ReturnT.LE.ReturnSPH)THEN
          InDeadB=.true.
        ELSE
          InDeadB=.false.
        ENDIF

C Evaluate the device status.
        IF(ReturnT.GT.ReturnSPH)THEN 
          DeviceONOFF=0
        ELSEIF(ReturnT.LT.ReturnSPL)THEN
          DeviceONOFF=1
        ELSEIF(DeviceONOFFp.EQ.1.AND.InDeadB)THEN
          DeviceONOFF=1
        ELSEIF(DeviceONOFFp.EQ.0.AND.InDeadB)THEN
          DeviceONOFF=0
        ENDIF
      ELSE
        DeviceONOFF=0
      ENDIF

C Establish the device COP 
      IF(DeviceONOFF.GT.0)THEN

C Determine the defrost status of the device (do only once per timestep so check
C iteration number.
        IF(DefrostCalc.GT.0)THEN

          IF(.NOT.InDefrost)THEN
C Update the defrost time.
            IF(DefrostCalc.eq.1)THEN
              DefrostTime=Defrostb0 
            ELSE
              RHamb=HFP
C Calculate defrost time to the nearest minute.
              DefrostTime=float(nint(Defrostb0+Defrostb1*RHamb))
            ENDIF

C Impose limits on defrost time
            IF(DefrostTime.GT.DefrostMaxTime)
     &DefrostTime=DefrostMaxTime
            IF(DefrostTime.LT.DefrostMinTime)
     &DefrostTime=DefrostMinTime

C Issue warning about timestep length if it is longer than the Defrost time. 
            IF(DefrostTime.LT.TIMSEC/60.)THEN
              WRITE(OUTS,'(a,f7.4,a,f7.4,a)')
     &'ASHP Warning: time step ',TIMSEC/60.,' > defrost cycle time',
     &DefrostTime,' reduce the time step to avoid problems.' 
              CALL EDISP(IUOUT,OUTS)
            ENDIF
          ENDIF

C Set defrost state.
          IF(Tamb.LE.DefrostT)THEN
            IF(.NOT.InDefrost)THEN
              IF(.NOT.DefrostLock)THEN
                InDefrost=.TRUE.
                DefrostDur=0.0
              ENDIF
            ENDIF
          ELSE
            InDefrost=.FALSE.
            DefrostLock=.FALSE.
          ENDIF

C Increment timers
          IF(InDefrost.and.ITERNU.eq.1) 
     &DefrostDur=DefrostDur+0.5*timsec/60.

          IF(DefrostLock.and.ITERNU.eq.1) 
     &DefrostLockDur=DefrostLockDur+0.5*timsec/60.

C Update state for next ts
          IF(DefrostDur.GT.DefrostTime)THEN
            InDefrost=.false.
            DefrostDur=0.0
            DefrostLock=.true.
            DefrostLockDur=0.0
          ENDIF

          IF(DefrostLockDur.GT.DefrostLockout)THEN
            DefrostLock=.false.
            DefrostLockDur=0.0            
          ENDIF

        ENDIF !end of defrost code.
C --------------------------

C Calculate the TOTAL device power draw (W)
        IF(CompModel.eq.1)THEN
          CompPower=1000.*(Compa0) !fixed power consumption
        ELSEIF(CompModel.eq.2)THEN
          Td=ReturnT
          Ta=Tamb
          CompPower=1000.*(Compa0+(Compa1*Ta)+(Compa2*Td))
        ENDIF
        if(CompPower.LT.0.0)CompPower=0.0

        FanPower=FanRating

C Calculate the COP based on the user-specified method.
        IF(COPModel.EQ.1)THEN
          COP=COPa0 !fixed COP
        ELSEIF(COPModel.EQ.2)THEN
          TL=Tamb+273.15
          TH=ReturnT+273.15
          COP=CarEffMod*((1-(TL/TH))**(-1)) !modified carnot COP
        ELSEIF(COPModel.EQ.3)THEN !quadratic based on Td-Ta
          TL=Tamb
          TH=ReturnT
          COP=COPa0+(COPa1*(TH-TL))+(COPa2*(TH-TL)**2.)
        ELSEIF(COPModel.EQ.4)THEN
          COP=COPa0+COPa1*Tamb+COPa2*Tamb**2.+COPa3*Tamb**3. !cubic polymonial COP based on ambient T
        ELSE
          WRITE(OUTS,'(a)')'Error in ASHP, COP model #'
          CALL EDISP(IUOUT,OUTS)
        ENDIF

C Set limits on calculated values of COP
        IF(COP.LT.0.0) COP=0.0
        IF(COP.GT.12.0) COP=12.0

        IF(InDefrost)THEN
          HeatOutput=0.0
        ELSE
          HeatOutput=COP*CompPower
          IF(HeatOutput.LT.0) THEN
            write(*,*) "Error: heat output < 0! ",HeatOutput, COP, 
     &CompPower
            STOP
          ENDIF
        ENDIF
      ELSE

C Device is 'off' set parameters accordingly (assume controller is still operating).
        
        CompPower=0.
        HeatOutput=0.
        FanPower=0.
        COP=0.
        InDefrost=.false.
        DefrostLock=.false.


      ENDIF

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN 

C Establish the nodal thermal capacities.
        CM=CompMass*AveSpHt

C node 1 water
        C1=DeviceFlow*SHTFLD(3,CONVAR(ICON1,1))

C Establish heat loss modulus
        CALL ECLOSE(PCNTMF(IPCOMP),99.,0.0001,CLOSE)
        IF(CLOSE) UAMod=0.0

C Calculate the electrical demand of the device assumung the controller is ON.
        CtlPower=CtlRating
        TotRealPower=CompPower+CtlPower

c Calculate the compressor power draw.
        CompressPower=CompPower-(PumpPower+FanPower+CtlPower)
        IF(CompressPower.LT.0.0)CompressPower=0.0
C       TotRealPower=CompPower+PumpPower+CtlPower+FanPower

        TotReacPower=(((CompressPower/CompressPf)**2)
     &-(CompressPower**2))**0.5
     &+(((PumpPower/PumpPf)**2)-(PumpPower**2))**0.5
     &+(((CtlPower/CtlPf)**2)-(CtlPower**2))**0.5
     &+(((FanPower/FanPf)**2)-(FanPower**2))**0.5

        TotApparentPower=((TotRealPower**2)+(TotReacPower*2))**0.5
 
C Calculate current component time-constant TC
        TC(IPCOMP)=CM/AMAX1(SMALL,(C1+UAMod))

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
        IF(IMPEXP.EQ.1) THEN
           ALPHA=1.
        ELSE IF(IMPEXP.EQ.2) THEN
           ALPHA=RATIMP
        ELSE IF(IMPEXP.EQ.3) THEN
          IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
             ALPHA=1.
          ELSE
             ALPHA=RATIMP
          END IF
        ELSE IF(IMPEXP.EQ.4) THEN
            CM=0.
            ALPHA=1.
        END IF

C Establish matrix equation self- and cross-coupling coefficients.
        COUT(1)=ALPHA*(-C1-UAMod)-CM/TIMSEC
C Matrix cross coupling coefficients.
        COUT(2)=ALPHA*C1
C Establish the present and known coefficient i.e. RHS
        COUT(3)=((1.-ALPHA)*(PCRP(ICON1)+PUAP(INOD1))-CM/TIMSEC)
     &          *CSVP(INOD1,1)
     &          +(1.-ALPHA)*(-PCRP(ICON1))*PCTP(ICON1)
     &          -ALPHA*UAMod*PCNTMF(IPCOMP)
     &          -ALPHA*HeatOutput
     &          -(1.-ALPHA)*PCQP(INOD1)
     &          -(1.-ALPHA)*PUAP(INOD1)*PCNTMP(IPCOMP)
     
C Store "environment" variables future values
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCRF(ICON1)=C1
         PCQF(INOD1)=HeatOutput
         PUAF(INOD1)=UAMod
 
C Addition output for results analysis.
         NAPDAT(IPCOMP)=9
         PCAOUT(IPCOMP,1)=HeatOutput
         PCAOUT(IPCOMP,2)=COP
         PCAOUT(IPCOMP,3)=Tamb
         PCAOUT(IPCOMP,4)=DeviceONOFF
         PCAOUT(IPCOMP,5)=ReturnSP
         PCAOUT(IPCOMP,6)=TotRealPower
         PCAOUT(IPCOMP,7)=TotReacPower
         PCAOUT(IPCOMP,8)=TotApparentPower
         if(Indefrost)then
           PCAOUT(IPCOMP,9)=1.0
         elseif(DefrostLock)then
           PCAOUT(IPCOMP,9)=-1.0
         else
           PCAOUT(IPCOMP,9)=0.0
         endif
C---------------------------------------------------------------------------------
C Make select results available in XML and CVS output.
C---------------------------------------------------------------------------------

        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/Heat_Out'
        call add_to_xml_reporting(HeatOutput, char_temp, 'units', '(W)',
     &        'ASHP: Heat Output' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/COP'
        call add_to_xml_reporting( COP, char_temp, 'units', '(-)',
     &        'ASHP: COP' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/Tambient'
        call add_to_xml_reporting(Tamb, char_temp, 'units',
     &        '(degC)',
     &        'ASHP: Ambient Temp' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/DeviceONOFF'
              tmpdev=float(DeviceONOFF)
        call add_to_xml_reporting(tmpdev, char_temp, 'units',
     &        '(-)','ASHP: ON-OFF status' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/Return T SP'
        call add_to_xml_reporting(ReturnSP, char_temp, 'units',
     &        '(degC)',
     &        'ASHP: Return Temp Set Pt.' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/Real Power'
        call add_to_xml_reporting(TotRealPower, char_temp, 'units',
     &        '(W)',
     &        'ASHP: Real Power Demand' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/Reac. Power'
        call add_to_xml_reporting(TotReacPower, char_temp, 'units',
     &        '(VAr)',
     &        'ASHP: Reactive Power Demand' )
        write(char_temp,'(A,A,A,A)') 'plant/',
     &        pcname(IPCOMP)(1:lnblnk(pcname(IPCOMP))),'/misc_data',
     &        '/Appar. Power'
        call add_to_xml_reporting(TotApparentPower, char_temp, 'units',
     &        '(VA)',
     &        'ASHP: Apparent Power Demand' )
        DefrostStat=PCAOUT(IPCOMP,9)
        call add_to_xml_reporting(DefrostStat, char_temp, 'units',
     &        '(-)',
     &        'ASHP: Defrost Status 0-off, 1-on, -1-lockout' )

  
C 1st phase mass (ie. "water") balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=DeviceFlow

C 2nd phase mass (ie. "vapour") balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=0.
      END IF


C Trace.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=20) Air Source Heat Pump'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM      = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1      = ',C1,' (W/Ks)'
            WRITE(ITU,*) ' Flow      = ',DeviceFlow,' (l/s)'
            WRITE(ITU,*) ' TC      = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA   = ',ALPHA,' (-)'
            WRITE(ITU,*) ' UAMod   = ',UAMod,' (W/K)' 
            WRITE(ITU,*) ' Tamb    = ',Tamb,' (C)'
            WRITE(ITU,*) ' ReturnT = ',ReturnT,' (C)'
            WRITE(ITU,*) ' ReturnSP        = ',ReturnSP,' (C)'
            WRITE(ITU,*) ' ReturnSPH       = ',ReturnSPH,' (C)'
            WRITE(ITU,*) ' ReturnSPL       = ',ReturnSPL,' (C)'
            WRITE(ITU,*) ' InDeadB         = ',InDeadB,' (-)'
            WRITE(ITU,*) ' CallforHeat     = ',CallforHeat,' (-)'
            WRITE(ITU,*) ' DeviceONOFF     = ',DeviceONOFF,' (-)'
            WRITE(ITU,*) ' DeviceONOFFp    = ',DeviceONOFFp,' (-)'
            WRITE(ITU,*) ' COP             = ',COP,' (-)'
            WRITE(ITU,*) ' AmbientTempComp = ',AmbientTempComp,' (C)'
            WRITE(ITU,*) ' TempCompS       = ',TempCompS,' (C)'
            WRITE(ITU,*) ' TempCompE       = ',TempCompE,' (C)'
            WRITE(ITU,*) ' DefrostCalc     = ',DefrostCalc,' (-)'
            WRITE(ITU,*) ' AmbientRH       = ',RHamb,' (%)'
            WRITE(ITU,*) ' DefrostTime     = ',DefrostTime,' (mins)'
            WRITE(ITU,*) ' DefrostDur      = ',DefrostDur,' (-)'
            WRITE(ITU,*) ' InDefrost       = ',Indefrost,' (-)'
            WRITE(ITU,*) ' DefrostLock     = ',DefrostLock,' (-)'
            WRITE(ITU,*) ' DefrostLockout  = ',DefrostLockout,' (mins)'
            WRITE(ITU,*) ' DefrostLockDur  = ',DefrostLockDur,' (mins)'
            WRITE(ITU,*) ' HeatOutput   = ',HeatOutput,' (W)'
            WRITE(ITU,*) ' CompPower    = ',CompPower,' (W)' 
            WRITE(ITU,*) ' FanPower     = ',FanPower,' (W)'
            WRITE(ITU,*) ' PumpPower    = ',PumpPower,' (W)'
            WRITE(ITU,*) ' CtlPower     = ',CtlPower,' (W)'                 
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=6
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP45C'

      RETURN
      END

C ******************** CMP50C ********************

C CMP50C generates for plant component IPCOMP with plant db code 500 ie.
C 1 node (ISV=29) WCH thermostatic radiator valve
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C     BDATA: 1 Index of coupled building zone (-)
C            2 Index of coupled wall in that zone (-)
C            3 Thermal conductance between water and sensor (W/K)
C            4 Equiv. convective conductance to air (W/K)
C            5 Equiv. radiative conductance to wall (W/K)
C            6 Equiv. radiative conductance to radiator (W/K)
C     CDATA: none

C     PCDATF/P
C            1 Air temperature of coupled building zone (C)
C            2 Surface temperature of coupled wall in 1 (C)

      SUBROUTINE CMP50C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)

      COMMON/C6/INDCFG
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      logical closea

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP50C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,1,2)
      INOD1=NPCDAT(IPCOMP,9)

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C First set-up "surroundings"
         IF(INDCFG.EQ.2.OR.NINT(BDATA(IPCOMP,6)).EQ.0) THEN
           call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
           IF(closea) THEN
               TAIR=20.
            ELSE
               TAIR=PCNTMF(IPCOMP)
            END IF
            TWAL=TAIR
         ELSE
            IZ=INT(BDATA(IPCOMP,1))
            TAIR=TFA(IZ)
            TWAL=TFS(IZ,NINT(BDATA(IPCOMP,2)))
         END IF
         HWAT=BDATA(IPCOMP,3)
         HAIR=BDATA(IPCOMP,4)
         HWAL=BDATA(IPCOMP,5)
         HRAD=BDATA(IPCOMP,6)

C Establish heat capacity of component mass CM (J/K)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)

C Calculate current component time-constant TC
         TC(IPCOMP)=CM/AMAX1(SMALL,(HWAT+HAIR+HWAL+HRAD))

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
         IF(IMPEXP.EQ.1) THEN
            ALPHA=1.
         ELSE IF(IMPEXP.EQ.2) THEN
            ALPHA=RATIMP
         ELSE IF(IMPEXP.EQ.3) THEN
            IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
               ALPHA=1.
            ELSE
               ALPHA=RATIMP
            END IF
         ELSE IF(IMPEXP.EQ.4) THEN
            CM=0.
            ALPHA=1.
         END IF

C Establish matrix equation self- and cross-coupling coefficients
         COUT(1)=ALPHA*(-HWAT-HAIR-HWAL-HRAD)-CM/TIMSEC
         COUT(2)=ALPHA*HWAT
         COUT(3)=ALPHA*HRAD
C and then present-time coefficient (ie. right hand side)
         COUT(4)=((1.-ALPHA)*(HWAT+HAIR+HWAL+HRAD)
     &              -CM/TIMSEC)*CSVP(INOD1,1)
     &             +(1.-ALPHA)*(-HWAT)*PCTP(ICON1)
     &             +(1.-ALPHA)*(-HRAD)*PCTP(ICON2)
     &             -ALPHA*HAIR*TAIR
     &             -(1.-ALPHA)*HAIR*PCDATP(IPCOMP,1)
     &             -ALPHA*HWAL*TWAL
     &             -(1.-ALPHA)*HWAL*PCDATP(IPCOMP,2)

C Store "environment" variables future values
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCTF(ICON2)=CONVAR(ICON2,1)
         PCDATF(IPCOMP,1)=TAIR
         PCDATF(IPCOMP,2)=TWAL

C 1st phase mass (ie. "water") balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=0.
         COUT(4)=0.

C 2nd phase mass (ie. "vapour") balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=0.
         COUT(4)=0.
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=29) WCH thermostatic valve'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         WRITE(ITU,*) ' Connection(s)  ',ICON1,ICON2
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' Tair   = ',TAIR,' (C)'
            WRITE(ITU,*) ' Twall  = ',TWAL,' (C)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=3
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP50C'

      RETURN
      END

C ******************** CMP51C ********************

C CMP51C generates for plant component IPCOMP with plant db code 510 ie.
C 1 node (ISV=29)     mechanical room thermostat
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C     BDATA: 1 Index of coupled building zone (-)
C            2 Index of "viewed" wall in that zone (-)
C            3 Index of wall on which device is mounted (-)
C            4 Equiv. convective conductance to air (W/K)
C            5 Equiv. radiative conductance to wall 2 (W/K)
C            6 Equiv. thermal conductance to wall 3 (W/K)
C     CDATA: 1 acceleration heating (W)

C     PCDATF/P
C            1 Air temperature of coupled building zone (C)
C            2 Surface temperature of "viewed" wall in 1 (C)
C            3 Surface temperature of mount wall in 1 (C)

      SUBROUTINE CMP51C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)

      COMMON/C6/INDCFG
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      logical closea

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP51C'

C Initialize pointers to node(s) INOD
      INOD1=NPCDAT(IPCOMP,9)

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C First set-up "surroundings"
         IF(INDCFG.EQ.2.OR.NINT(BDATA(IPCOMP,1)).EQ.0) THEN
            call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
            IF(closea) THEN
               TAIR=20.
            ELSE
               TAIR=PCNTMF(IPCOMP)
            END IF
            TWL1=TAIR
            TWL2=TAIR
         ELSE
            IZ=INT(BDATA(IPCOMP,1))
            TAIR=TFA(IZ)
            TWL1=TFS(IZ,NINT(BDATA(IPCOMP,2)))
            TWL2=TFS(IZ,NINT(BDATA(IPCOMP,3)))
         END IF
         HAIR=BDATA(IPCOMP,4)
         HWL1=BDATA(IPCOMP,5)
         HWL2=BDATA(IPCOMP,6)

C Then initialize acceleration heat Q
         Q=CDATA(IPCOMP,1)

C Establish heat capacity of component mass CM (J/K)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)

C Calculate current component time-constant TC
         TC(IPCOMP)=CM/AMAX1(SMALL,(HAIR+HWL1+HWL2))

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
         IF(IMPEXP.EQ.1) THEN
            ALPHA=1.
         ELSE IF(IMPEXP.EQ.2) THEN
            ALPHA=RATIMP
         ELSE IF(IMPEXP.EQ.3) THEN
            IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
               ALPHA=1.
            ELSE
               ALPHA=RATIMP
            END IF
         ELSE IF(IMPEXP.EQ.4) THEN
            CM=0.
            ALPHA=1.
         END IF

C Establish matrix equation self-coupling coefficient
         COUT(1)=ALPHA*(-HAIR-HWL1-HWL2)-CM/TIMSEC
C and then present-time coefficient (ie. right hand side)
         COUT(2)=((1.-ALPHA)*(HAIR+HWL1+HWL2)
     &              -CM/TIMSEC)*CSVP(INOD1,1)
     &             -ALPHA*HAIR*TAIR
     &             -(1.-ALPHA)*HAIR*PCDATP(IPCOMP,1)
     &             -ALPHA*HWL1*TWL1
     &             -(1.-ALPHA)*HWL1*PCDATP(IPCOMP,2)
     &             -ALPHA*HWL2*TWL2
     &             -(1.-ALPHA)*HWL2*PCDATP(IPCOMP,3)
     &             -ALPHA*Q-(1.-ALPHA)*PCQP(INOD1)

C Store "environment" variables future values
         PCQF(INOD1)=Q
         PCDATF(IPCOMP,1)=TAIR
         PCDATF(IPCOMP,2)=TWL1
         PCDATF(IPCOMP,3)=TWL2

C 1st phase mass (ie. "water") balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=0.

C 2nd phase mass (ie. "vapour") balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=0.
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=29) mechanical room thermostat'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' Tair   = ',TAIR,' (C)'
            WRITE(ITU,*) ' Twall,1= ',TWL1,' (C)'
            WRITE(ITU,*) ' Twall,2= ',TWL2,' (C)'
            WRITE(ITU,*) ' CDATA  = ',CDATA(IPCOMP,1)
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=2
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP51C'

      RETURN
      END
C --------------------------------- CMP79C -----------------------------------
C CMP79C (DHWdraw) calculates an instantaneous hot water draw based on a 
C probabilistic model. For each simulation time step the model calculates 
C a hot water draw V in l/s. This flow is then passed into the model coefficient 
C generator. The models is (loosely) based on the work of Jordan and Vajen in IEA SHC Annex26. 
C
C The draw calculation is based on the user supplying a nominal daily draw (litres).
C The user also needs to define the typical % of the total draw consumed in up to
C 6 concurent user-defined periods adding up to 24h.
C 
C The user can define up to 6 distinct draw types; these can be individual
C loads such as a shower or grouped loads of simular types. These are characterised by 
C 1) a flowrate when in use (l/min) 
C 2) a nominal flow duration (min) 
C 3) a flow duration standard dev. 
C
C For each draw types the user needs to define the typical % of the total daily 
C draw attributable to the draw type along with information on any change in draw
C use between weekdays and weekends (e.g. applies to use of baths)
C
C Data on holidays, seasonal variation of the total daily draw also need to be defined.
C During holiday periods the total draw is set to zero.
C
C
C Data input and main variable descriptions (up to 60 items): 
C NomDraw               Total Average Nominal HW Draw (l per day)
C SeasDrawFluc          Seasonal draw fluctuation (%)
C SeasPhas              Seasonal phase shift (days)
C
C NoHolPer              # of Holiday Periods (up to 3)
C For Each Holiday Period:
C HolPerS(MHolPer)      Start DOY
C HolPerE(MHolPer)      End DOY
C
C NoDrawPer             # of Concurrent Draw Periods in Day (up to 6)
C For each draw period i
C DrawPerS(MDrawPer)    Start of draw period (hours)
C NomDrawFrac(MDrawPer) Nominal. % of total Nominal. draw taken in period i
C 
C
C NDrawType             Number of distinct draw types (up to 6)
C NomTypeFrac[i]        % of total Nominal draw attributable to type i
C DrawV[i]              Draw i flow rate (l/min)
C DrawVstd[i]           Draw i flow rate std dev (l/min)
C NomDrawDur[i]         Draw i nominal flow duration (min)
C DrawWdvar[i]          Draw i weekday probability modifier (-)
C DrawWevar[i]          Draw i weekend probability modifier (-)

C ----------------------------------------------------------------------
      SUBROUTINE CMP79C(IPCOMP,COUT,ISTATS)

      implicit none
#include "plant.h"
#include "building.h"


      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PTIME/PTIMEP,PTIMEF

      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)

      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

      common/pcnam/pcname(mpcom)       ! Plant component names
      COMMON/ITERINDEX/ITERNU !plant iteration number

C Ground temperature
      common/C25/igttype,GTMP(12)

C Commons for DWHStochastic calculation.
      COMMON/DHWStoch1/DHWsubmin,CalCount,DHWCalcInc
      COMMON/DHWStoch2/DHWdrawV,DHWDrawVp

      Common/DHWStoch3R/NomDraw,SeasDrawFluc,DrawPerS(6),NomDrawFrac(6),
     &NomTypeFrac(6),DrawV(6),DrawVstd(6),DrawWdvar(6),DrawWevar(6)
      Common/DHWStoch3I/SeasPhas,NoHolPer,HolPerS(3),HolPerE(3),
     &NoDrawPer,NDrawType,NomDrawDur(6)

      Common/DHWStoch4/CurDrawDur(6),ElapseDrawDur(6),DHWDraw(6)

      Common/DHWStochL/inDraw(6)

      Common/DHWTrakType/DHWDrawTypeTot(6),DHWDrawTypeTotV(6),
     &DHWDrawTypeTotVp(6)


      DIMENSION NDMonth(12)
      DATA NDMonth/31,28,31,30,31,30,31,31,30,31,30,31/


      REAL COUT(MPCOE)
      character outs*124
      character*128  H3K_rep_NAME      ! H3Kreporting object name
      character*128  root_name         ! root name of component node
      character*128  hold_name         ! temporary variable for string manipulation
      character*15 pcname
      character*16 char_tmp
      logical DHWsubmin,DHWCalc,Weekend,indraw,DisPos


      integer iuout,iuin,itc,icnt,ITCF,ITRACE,IZNTRC,ITU,IHRP,IHRF,IDYP,
     &IDYF,IDWP,IDWF,NSINC,ITS,NPCOMP,NCI,I,IHol,ITERNU,
     &NPCDAT,IPOFS1,IPOFS2,ICONTP,ICONDX,NDMonth,igttype,ICON1

      integer IPCOMP,IX1,NAPDAT

      integer INOD1,IMonth,IM,IDoM,ID,ISTATS,iMinS,CalCount,iPer,inPer,
     &NITMS

      integer DHWCalcInc,iDHWinc

      real DHWdrawV,DHWDrawVp,FracMin,TSperDHWCalc,DHWDrawTot,DrawV,
     &DHWDraw,DrawPVal,DrawProb,DrawWevar,DrawWdvar,NomDrawFrac,
     &NomDrawDur,Tincr,NomTypeFrac,ActV

      integer IDHWType,HolPerS,HolPerE,NDrawType,NoDrawPer,NoHolPer,
     &SeasPhas

      REAL DHWDrawTypeTot,DHWDrawTypeTotV,DHWDrawTypeTotVp

      real TIMSEC,PTIMEP,PTIMEF,CDATA,PCAOUT,QDATA,
     &CONVAR,GTMP,TG2,TG1,CurrGTMP,PI,SeasDrawFluc,RHOFLD


      real DOY,NomDrawDay,NomDraw,xHol,xWE,DrawPerS,PerDur,
     &ElapseDrawDur,DrawVstd,CurDrawDur,DenS,Rand_No


      PI=22./7.

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP79C'

C Check control data for relevant balance type
      IF(ISTATS.EQ.1.AND.CDATA(IPCOMP,1).LT.0.) THEN
         CALL DAYCLK(IDYP,PTIMEF,IUOUT)
         WRITE(outs,*) ' CMP79C: invalid control data for component ',
     &                  IPCOMP,' : ',CDATA(IPCOMP,1)
         call edisp(iuout,outs)
         call edisp(iuout,' CMP79C: unresolvable error.')
         call epwait
         call epagend
         STOP
      END IF

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      INOD1=NPCDAT(IPCOMP,9)

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C Calculate the current ground temperatures by interpolating the monthly ground
C temperatures.
        CALL EDAYR(IDYP,ID,IM)
        IMonth=IM
        IDoM=ID

C Get current month temperature
        TG1=GTMP(IMonth)
        IF(IMonth.EQ.12)THEN

C Get next month's temperature (assume loop round to January).
           TG2=GTMP(1)
        ELSE

C Get next month's temperature
           TG2=GTMP(IMonth+1)
        ENDIF

C Get today's ground temp.
        currGTMP=((float(IDoM)/float(NDMonth(IMonth)))*(TG2-TG1))+TG1


C Establish matrix equation self- and cross-coupling coefficients

C The return flow from this component (if needed) is mains cold water and so the
C 'exit' temperature is set to the interpolated ground temperature. 
         COUT(1)=1.
         COUT(2)=0.
C and then present-time coefficient (ie. right hand side)
         COUT(3)=currGTMP


C 1st phase mass (ie. water) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN

C Calculate the DHWdraw (l) at this simulation timestep.

C Determine the calculation frequency if timestep < 1 min or the number of draw time increments
C to be calculated if timestep > 1 min.
        IF(TIMSEC.LT.60.)THEN
          DHWSubmin=.true.
          iMinS=NINT(60./TIMSEC)
          FracMin=abs(TIMSEC*float(iMinS)-60.)
          DHWCalcInc=1
          TSperDHWCalc=iMinS
          if(FracMin.gt.1E-2)then
            WRITE(IUOUT,*)' WARNING:CMP79C [DHW] requires the plant'
            WRITE(IUOUT,*)' time step (s) to be an integer multiple or' 
            WRITE(IUOUT,*)' divisor of 60. Adjust the plant time   '
            WRITE(IUOUT,*)' step accordingly to avoid errors!'
          endif
        ELSE
          DHWSubmin=.false.
          iMinS=NINT(TIMSEC/60.)
          FracMin=abs(TIMSEC-(60.*float(iMinS)))
          DHWCalcInc=iMinS
          if(FracMin.gt.1E-2)then
            WRITE(IUOUT,*)' WARNING:CMP79C [DHW] requires the plant'
            WRITE(IUOUT,*)' time step (s) to be an integer multiple or' 
            WRITE(IUOUT,*)' divisor of 60. Adjust the plant time      '
            WRITE(IUOUT,*)' step accordingly to avoid errors!'
          endif
        ENDIF


C Calculate the draw ONLY at the first calc increment if sim timestep < 1 min , 
C and in the first iteration of a plant time step.
        IF(ITERNU.EQ.1)THEN
          DHWCalc=.true.
          if(DHWsubmin.AND.CalCount.EQ.1)then
            DHWCalc=.true.
            CalCount=CalCount+1
          elseif(DHWsubmin.AND.CalCount.GT.1)then
            DHWCalc=.false.
            CalCount=CalCount+1
            if(CalCount.GT.TSperDHWCalc)then
              CalCount=1
            endif
          endif
        ELSE
          DHWCalc=.FALSE.
        ENDIF

C Calculate the current DHWDraw over the current time step. If time step > 1 min then
C the routine will loop N times and will calculate the equivalent average draw during the time
C step. 
        if(DHWcalc)then

C Calculate general info required for calculation
C Day of year [needed for average flow modifier]
          DOY=float(iDYP)

C Calculate nominal draw for today, accounting for seasonal variablity
          NomDrawDay=NomDraw*
     &(1.+(SeasDrawFluc/100.)*(cos(2*PI*(1/365.)
     &*DOY-(SeasPhas*(2*PI/365.)))))

          if(NomDrawDay.LT.0.0)then
             NomDrawDay=0.0
             WRITE(IUOUT,*)' WARNING:CMP79C [DHW] a nominal draw < 0.0'
             WRITE(IUOUT,*)' has been calculated. Resetting to zero' 
          endif


C Check for weekend [flow modifier]
          if(IDWP.GT.5)then
            Weekend=.true.
          else
            Weekend=.false.
          endif

C Check for holiday [flow modifier] 
          xHol=1.0
          do 5 iHol=1,noHolPer
            if(iDYP.GE.HolPerS(iHol).AND.iDYP.LT.HolPerE(iHol))then
              xHol=0.0
            endif
   5      continue


C Work out the current draw period
          do 7 iPer=1,NoDrawPer-1
            if(PTIMEP.GE.DrawPerS(iPer).AND.
     &      PTIMEP.LT.DrawPerS(iPer+1))then
              inPer=iPer
              PerDur=(DrawPerS(inPer+1)-DrawPerS(inPer))*60.
            endif
  7       continue

C Check if in final draw period
          if(PTIMEP.GE.DrawPerS(NoDrawPer))then
            inPer=NoDrawPer
            PerDur=(24.0-DrawPerS(NoDrawPer))*60.
          endif


          DHWDrawTot=0.0
          do 10 iDHWinc=1,DHWCalcInc !increment loop if simulation TS > 1 min
            do 101 iDHWType=1,NDrawType !Types loop 

C Check to see if the component is in the process of a multi-timestep draw
              if(indraw(iDHWType))then
                ElapseDrawDur(iDHWType)=ElapseDrawDur(iDHWType)+1.0
                if(ElapseDrawDur(iDHWType).GT.CurDrawDur(iDHWType))then
                  DHWDraw(iDHWType)=0.0
                  indraw(iDHWType)=.false.
                  ElapseDrawDur(iDHWType)=0.0
                  CurDrawDur(iDHWType)=0.0
                else
                  indraw(iDHWType)=.true.
                endif 
              else
C Not currently in a draw, check to see if a draw is made and calculate the draw characteristics
C if required.
                DHWDraw(iDHWType)=0.0

                if(weekend)then
                  xWE=DrawWevar(iDHWType)
                else
                  xWE=DrawWdvar(iDHWType)
                endif

C Calculate the draw probability (all times in minutes)
                DrawProb=((NomDrawFrac(inPer)/100.)*
     &          ((NomTypeFrac(iDHWType)/100.)*NomDrawDay*xHol*xWE))/
     &          (DrawV(iDHWType)*PerDur*NomDrawDur(iDHWType))

                 DrawPVal=Rand_No()

                if(DrawPVal.LE.DrawProb)then

C Set the draw to the nominal value.
                  DHWDraw(iDHWType)=DrawV(iDHWType)
                  Tincr=0.1

C Calculate the actual draw flow rate based on a normal distribution about the mean
C As flow cannot be zero force a lognormal distribution if the range goes below zero
                  DisPos=.true.
                  call StdDisVal(DrawV(iDHWType),
     &DrawVstd(iDHWType),Tincr,ActV,DisPos)
                  DHWDraw(iDHWType)=ActV

c Calculate the time duration of the current draw and set the inDraw varaible if the duration > 1min.
C This allows draw to persist over multiple time steps.
                  ElapseDrawDur(iDHWType)=0.0
                  CurDrawDur(iDHWType)=NomDrawDur(iDHWType)
                  if(CurDrawDur(iDHWType).gt.1.0)then 
                    ElapseDrawDur(iDHWType)=ElapseDrawDur(iDHWType)+1.0
                    inDraw(iDHWType)=.true.
                  else
                    inDraw(iDHWType)=.false.
                  endif    
                else
                  DHWDraw(iDHWType)=0.0
                endif

              endif

C Add to the calculated total draw to the totals for tracking draw from each type.
              DHWDrawTypeTot(iDHWType)=DHWDrawTypeTot(iDHWType)
     &+DHWDraw(iDHWType)

              DHWDrawTot=DHWDrawTot+DHWDraw(iDHWType)

  101       continue
  10      continue

C Overwrite the previous time step DHW draw with the average (convert l/min -> l/s) of the calculated
C draw over the plant simulation time step
          DHWDrawV=DHWDrawTot/(60.*float(DHWCalcInc))
          DHWDrawVp=DHWDrawV   
          DHWDrawTot=0.0
          DO 105 iDHWType=1,NDrawType
            DHWDrawTypeTotV(iDHWType)=
     &DHWDrawTypeTot(iDHWType)/(60.*float(DHWCalcInc))
            DHWDrawTypeTotVp(iDHWType)=DHWDrawTypeTotV(iDHWType)
            DHWDrawTypeTot(iDHWType)=0.0
  105     Continue

        else

C Don't need to calculate draw as plant simulation TS is < 1 min, 
C use previously calculated value instead.
          DHWDrawV=DHWDrawVp

          DO 107 iDHWType=1,NDrawType
            DHWDrawTypeTotV(iDHWType)=DHWDrawTypeTotVp(iDHWType)
  107     Continue

        endif !end on DHW calc check



C Fluid density 
       DenS=RHOFLD(3,CONVAR(ICON1,1))

       COUT(1)=1.
       COUT(2)=0.
       COUT(3)=DHWDrawV*(DenS/1000.)

C Store individual type flows as addtitional output.
        NAPDAT(IPCOMP)=6
        DO 109 iDHWType=1,NDrawType
          PCAOUT(IPCOMP,iDHWType)=
     &DHWDrawTypeTotV(iDHWType)*(DenS/1000.)
  109   CONTINUE

C------------------------------------------------------
C XML output
C------------------------------------------------------

C.....Get component name
        hold_name = pcname(IPCOMP) 
C.....Format string as: 'plant/NAME/misc_data'

         WRITE(root_name,'(A,A,A)') 
     &     'plant/',
     &     hold_name(1:lnblnk(hold_name)),
     &     '/misc_data'

         WRITE(H3K_rep_NAME,'(A,A)') 
     &     root_name(1:lnblnk(root_name)), '/DHW_draw_stoch'

         call add_to_xml_reporting(
     &                   DHWDrawV*(DenS/1000.),
     &                   H3K_rep_name,
     &                   'units',
     &                   '(kg/s)',
     &                   'Hot water draw rate' )

          DO 111 iDHWType=1,NDrawType
            write(char_tmp,'(A,I2)')'Draw for type:',iDHWType
            call add_to_xml_reporting(
     &                   DHWDrawTypeTotV(iDHWType)*(DenS/1000.),
     &                   H3K_rep_name,
     &                   'units',
     &                   '(kg/s)',
     &                   char_tmp)
  111     Continue

c      write(87,*)'DHW: mass flow rate '
c      write(87,*)'kg/s | rho ', DHWDrawV*(DenS/1000.), DenS

C 2nd phase mass (ie. vapour) balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=0.
      END IF



C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=20) WCH Stochastic DHW draw'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' GTMP       = ',currGTMP,' (C)'
            WRITE(ITU,*) ' DHWCalc    = ',DHWCalc,' (-)'
            WRITE(ITU,*) ' DHWSubmin  = ',DHWSubmin,' (-)'
            WRITE(ITU,*) ' DHWCalcInc = ',DHWCalcInc,' (-)'
            WRITE(ITU,*) ' DOY        = ',DOY,' (-)'
            WRITE(ITU,*) ' NomDraw    = ',NomDraw,' (l)'
            WRITE(ITU,*) ' NomDrawDay = ',NomDrawDay,' (l)'
            WRITE(ITU,*) ' DoW        = ',IDWP,' (-)'
            WRITE(ITU,*) ' Weekend    = ',Weekend,' (-)'
            WRITE(ITU,*) ' Holiday    = ',xHol,' (-)'
            WRITE(ITU,*) ' # Periods  = ',NoDrawPer,'(-)'
            WRITE(ITU,*) ' # Types    = ',NDrawType,'(-)'
            WRITE(ITU,*) ' Period     = ',inPer,'(-)'
            WRITE(ITU,*) ' DHWDraw    = ',inPer,'(l)'
            WRITE(ITU,*) ' DHWDrawV   = ',DHWDrawV,'(l/s)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=3
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP79C'

      RETURN
      END



C ******************** CMP90C ********************

C CMP90C generates for plant component IPCOMP with plant db code 900 ie.
C 2 node (ISV>19)     air & water temperature source
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: none
C     BDATA: none
C     CDATA: 1 air temperature (C)
C            2 water temperature (C)

      SUBROUTINE CMP90C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

      REAL      COUT(MPCOE)

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP90C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,2,1)
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C Establish matrix equation self-coupling coefficients,
         COUT(1)=1.
         COUT(2)=1.
C then matrix equation cross-coupling coefficients,
         COUT(3)=0.
         COUT(4)=0.
C and then present-time coefficients (ie. right hand sides)
         COUT(5)=CDATA(IPCOMP,1)
         COUT(6)=CDATA(IPCOMP,2)

C 1st phase mass (ie. dry air) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=1.
         COUT(3)=-PCONDR(ICON1)
         COUT(4)=-PCONDR(ICON2)
         COUT(5)=0.
         COUT(6)=0.

C 2nd phase mass (ie. vapour) balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=1.
         COUT(3)=-PCONDR(ICON1)
         COUT(4)=-PCONDR(ICON2)
         COUT(5)=0.
         COUT(6)=0.
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 2 node (ISV>19) air & water temperature source'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2
         WRITE(ITU,*) ' Connection(s)  ',ICON1,ICON2
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CDATA1 = ',CDATA(IPCOMP,1)
            WRITE(ITU,*) ' CDATA2 = ',CDATA(IPCOMP,2)
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=6
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP90C'

      RETURN
      END

c ******************** CMP91C ********************

C CMP91C generates for plant component IPCOMP with plant db code 910 ie.
c 2 node (ISV>19) imaginary building-like plant load
c matrix equation coefficients COUT (in order: self-coupling, cross-
c coupling, and present-time coefficients) for energy balance (ISTATS=1),
c 1th phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
c     adata: 1 Total mass (kg)
c            2 Mass weighted average specific heat (J/kgK)
c            3 Wall U value (W/m^2K)
c            4 Total surface area of walls (m^2)
c            5 zone space volume (m^3)
c     bdata: 1 Inside heat transfer coefficient (W/m^2K)
c            2 Outside heat transfer coefficient (W/m^2K)
c            3 Air changes per hour
c     cdata: 1 Heat gain load (W)

      subroutine cmp91c(ipcomp,cout,istats)
#include "plant.h"
#include "building.h"

      common/outin/iuout,iuin
      common/tc/itc,icnt
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/pctime/timsec
      common/pctc/tc(mpcom)

      common/pcequ/impexp,ratimp

      common/c9/npcomp,nci(mpcom),cdata(mpcom,mmiscd)
      common/c10/npcon,ipc1(mpcon),ipn1(mpcon),ipct(mpcon),
     &           ipc2(mpcon),ipn2(mpcon),pcondr(mpcon),PCONSD(MPCON,2)
      common/c12ps/npcdat(mpcom,9),ipofs1(mcoefg),ipofs2(mcoefg,mpvar)
      common/pdbdt/adata(mpcom,madata),bdata(mpcom,mbdata)
      common/pcval/csvf(mpnode,mpvar),csvp(mpnode,mpvar)
      common/pcvar/pctf(mpcon),pcrf(mpcon),puaf(mpnode),pcqf(mpnode),
     &             pcntmf(mpcom),
     &             pctp(mpcon),pcrp(mpcon),puap(mpnode),pcqp(mpnode),
     &             pcntmp(mpcom)
      common/pcond/convar(mpcon,mconvr),icontp(mpcon),
     &             icondx(mpcom,mnodec,mpconc)

      COMMON/CLIMIP/QFPP,QFFP,TPP,TFP,QDPP,QDFP,VPP,VFP,DPP,DFP,HPP,HFP
      COMMON/CLMPHG/HEXTPP,HEXTFP,GEXTPP,GEXTFP,TWBPP,TWBFP

      parameter (small=1.0e-15)
      real      cout(mpcoe)
      logical closea

c Trace output
      if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   ITRACE(37).ne.0) write(itu,*) ' Entering subroutine CMP91C'

c Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      icon1=icondx(ipcomp,2,1)
      inod1=npcdat(ipcomp,9)
      inod2=npcdat(ipcomp,9)+1

c Establish air changes per hour (acph) in zone.
      acph=bdata(ipcomp,3)

c Evaluate air leakage flow rate and air specific heat capcity
c based on whether air going in (acph +ve) or out (acph -ve).
      if (acph.gt.0.0) then
         amdotl=rhofld(1,tfp)*acph*adata(ipcomp,5)/3600.
         cpa=shtfld(1,tfp)
         hmdr=gextfp
      else if(acph.lt.0.0) then
         amdotl=rhofld(1,csvf(inod2,1))*acph*adata(ipcomp,5)/3600.
         cpa=shtfld(1,csvf(inod2,1))
         call eclose(csvf(inod2,2),0.00,0.0001,closea)
         if(closea) then
            hmdr=gextfp
         else
            hmdr=csvf(inod2,3)/csvf(inod2,2)
         endif
      endif

c Generate coefficients for energy balance equation
      if(istats.eq.1) then

c Establish heat capacity of component mass CM (J/K) and
c fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         cm=adata(ipcomp,1)*adata(ipcomp,2)
         c1=pcondr(icon1)*convar(icon1,2)*shtfld(1,convar(icon1,1))+
     &      pcondr(icon1)*convar(icon1,3)*shtfld(2,convar(icon1,1))

c Establish thermal resistances for inside and outside.
         U=adata(ipcomp,3)
         hi=bdata(ipcomp,1)
         ho=bdata(ipcomp,2)
         ri=adata(ipcomp,4)/(1./hi+1./(2.*U))
         ru=adata(ipcomp,4)/(1./ho+1./(2.*U))
         call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
         IF(closea) ru=0.0

c Calculate heat gain in zone.
         qg=cdata(ipcomp,1)


c Calculate current component time-constant TC
         tc(ipcomp)=cm/amax1(small,(ri+ru))

c Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
         if(impexp.eq.1) then
            alpha=1.
         else if(impexp.eq.2) then
            alpha=ratimp
         else if(impexp.eq.3) then
            if(timsec.gt.0.63*tc(ipcomp)) then
               alpha=1.
            else
               alpha=ratimp
            end if
         else if(impexp.eq.4) then
            cm=0.
            alpha=1.
         end if

c Establish matrix equation self- and cross-coupling coefficients
c Do wall first.
         cout(1)=alpha*(-ri-ru)-cm/timsec
         cout(2)=alpha*ri

c then air.
         cout(3)=ri
         cout(4)=-c1-ri-amdotl*cpa

c Cross coupling.
         cout(5)=c1

c and then present-time coefficient (ie. right hand side)
         cout(6)=((alpha-1.)*(-ri-ru)
     &              -cm/timsec)*csvp(inod1,1)
     &             +((alpha-1.)*ri*csvp(inod2,1))
     &             -(alpha*ru*pcntmf(ipcomp))
     &             +((alpha-1.)*ru*pcntmp(ipcomp))
         cout(7)=-qg-amdotl*cpa*pcntmf(ipcomp)

c 1th phase mass (ie. dry air) balance coefficients
      else if(istats.eq.2) then
         cout(1)=1.
         cout(2)=0.
         cout(3)=0.
         cout(4)=1.
         cout(5)=-pcondr(icon1)
         cout(6)=0.
         cout(7)=amdotl

c 2nd phase mass (ie. vapour) balance coefficients
      else if(istats.eq.3) then
         cout(1)=1.
         cout(2)=0.
         cout(3)=0.
         cout(4)=1.
         cout(5)=-pcondr(icon1)
         cout(6)=0.
         cout(7)=amdotl*hmdr
      end if

c Trace output
      if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   itrace(37).ne.0) then
         write(itu,*) ' Component      ',ipcomp,':'
         write(itu,*) ' 2 node (ISV>19) Building zone'
         write(itu,*) ' Matrix node(s) ',inod1,inod2
         write(itu,*) ' Connection(s)  ',icon1
         if(istats.eq.1) then
            write(itu,*) ' CM     = ',cm,' (J/K)'
            write(itu,*) ' C1     = ',c1,' (W/K)'
            write(itu,*) ' TC     = ',TC(IPCOMP),' (s)'
            write(itu,*) ' ALPHA  = ',alpha,' (-)'
            write(itu,*) ' U      = ',u,' (W/K)'
            write(itu,*) ' qg     = ',qg,' (W)'
            write(itu,*) ' PCNTMF = ',pcntmf(ipcomp),' (C)'
            write(itu,*) ' ri     = ',ri,' (W/m^2K)'
            write(itu,*) ' ru     = ',ru,' (W/m^2K)'
         end if
         write(itu,*) ' Matrix coefficients for ISTATS = ',istats
         nitms=7
         write(itu,*) (cout(i),i=1,nitms)
         if(itu.eq.iuout) then
            ix1=(ipcomp/4)*4
            if(ix1.eq.ipcomp.or.ipcomp.eq.npcomp) call epagew
         end if
      end if

      if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   ITRACE(37).ne.0) write(itu,*) ' Leaving subroutine CMP91C'

      return
      end
