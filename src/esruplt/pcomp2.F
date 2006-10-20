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
C  CMP50C   1 node (ISV=29) WCH thermostatic radiator valve
C  CMP51C   1 node (ISV=29)     mechanical room thermostat
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

C Calculate current component time-constant TC
         TC(IPCOMP)=AMAX1(
     &        CM/AMAX1(SMALL,(C1)),
     &        CM/AMAX1(SMALL,(PCDATF(IPCOMP,1)+
     &                        Q/AMAX1(SMALL,AMAX1(SMALL,(TS-TE))))))

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
     &           +ALPHA*Q+(1.-ALPHA)*PCQP(INOD2)

C Store "environment" variables future values
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCRF(ICON1)=C1
         PCQF(INOD2)=Q

C Establish "containment loss" data
         QDATA(IPCOMP)=ALPHA*Q+(1.-ALPHA)*PCQP(INOD2)

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
      logical closea

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
            VW=WMFR/(RHOFLD(3,TW)*ADATA(IPCOMP,6))
            HW=1400.*(1.+0.015*TW)*VW**0.8*DH**(-0.2)
            UA=1./(1./(HW*PI*DH*PL)+1./UA)
            WMCP=WMFR*SHTFLD(3,CONVAR(ICON1,1))
            UA=WMCP*(EXP(UA/WMCP)-1.)
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
            E=(CSVF(INOD1,2)
     &         /(RHOFLD(3,CSVF(INOD1,1))*BDATA(IPCOMP,2)))**3
     &         *BDATA(IPCOMP,1)
         END IF

         PWRP(IPCOMP)=-ABS(E)
         IEMODEL=1
         CALL EMACH(IPCOMP,IEMODEL,PWRP(IPCOMP),PQ,PA)
         PWRQ=PQ

C Now Q is made up of all inefficiencies
         Q=(1.-BDATA(IPCOMP,3))*E

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
            COUT(3)=CDATA(IPCOMP,1)*RHOFLD(3,CSVF(INOD1,1))
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
         if(fm.lt.0.1) fm=0.1

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

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN 

C Establish fluid thermal capacity rates
        CW1=SHTFLD(3,CSVF(INOD1,1))*ADATA(IPCOMP,1)
        CW2=SHTFLD(3,CSVF(INOD2,1))*ADATA(IPCOMP,2)
        C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(3,CONVAR(ICON1,1))
        C2=PCONDR(ICON2)*CONVAR(ICON2,2)*SHTFLD(3,CONVAR(ICON2,1))

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
         COUT(3)=0.
         COUT(4)=0.
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
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP01C'

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
