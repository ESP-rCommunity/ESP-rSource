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

C This file contains the following routines:
C  SHTFLD returns specific heat of air, water vapour, or water
C  RHOFLD returns density of air, water vapour, or water
C  CNDWAT returns heat condensation for water vapour

C  EMACH  calculate power characteristics of an electrical machine.
C  EMACH001 calculate reactive power and voltage vector.

C Plant component model coefficient generators:

C  CMP01C 1 node (ISV=21) AC converging 2-leg junction
C  CMP02C 1 node (ISV=21) AC  steam/spray humidifier 
C  CMP03C 1 node (ISV=21) AC fan 
C  CMP04C 1 node (ISV=21) AC cooling coil & flux control
C  CMP05C 1 node (ISV=21) AC heating coil & flux control
C  CMP06C 1 node (ISV=21) AC duct & condensation ?
C  CMP07C 1 node (ISV=21) AC damper & flow ratio control
C  CMP08C 1 node (ISV=21) AC flow diversion damper
C  CMP09C 1 node (ISV=21) AC counterflow cooling coil & water
C                        mass flow rate control (TRNSYS)
C  CMP10C 1 node (ISV=21) AC counterflow cooling coil & water
C                        mass flow rate control
C  CMP11C 1 node (ISV=21) AC counterflow heating coil & water
C                        mass flow rate control
C  CMP12C 2 node (ISV=21) AC plate heat exchanger
C  CMP13C 2 node (ISV=21) AC generic heat exchanger model
C  CMP14C 2 node (ISV>20) Cooling tower
C  CMP15C 1 node (ISV=21) AC fan mass flow rate control
C  CMP40C 3 node (ISV>19) AC counterflow cooling coil fed
C                        by WCH system
C  CMP41C 3 node (ISV>19) AC counterflow heating coil fed
C                        by WCH system

C ******************** SHTFLD ********************

C SHTFLD is a real function which returns the specific heat (J/kgK)
C of dry air (IFLD=1), water vapour (IFLD=2), or water (IFLD=3)
C as a function of (TEMP) the fluid temperature (C)
C Source for the expressions: Ham, Ph.J. 1984. "Mollier-h/x-dia-
C grammen voor vochtige lucht, geconstrueerd door middel van de
C computer", Klimaatbeheersing, vol. 13, no. 8, pp. 269-274

      FUNCTION SHTFLD(IFLD,TEMP)

      COMMON/SHOUT/ICOUT
      COMMON/WRNOFF/IWMOFF

      VTEMP=TEMP

      IF(((IFLD.LE.2.AND.(TEMP.LT.-40..OR.TEMP.GT.100.))).OR.
     &    (IFLD.EQ.3.AND.(TEMP.LT. 0.0.OR.TEMP.GT.100.))) THEN

         IF(IFLD.LE.2.AND.TEMP.LT.-40.) THEN
            VTEMP=-40.
         ELSE IF(IFLD.EQ.3.AND.TEMP.LT.0.0) THEN
            VTEMP=0.0
         ELSE
            VTEMP=100.
         ENDIF

         IF(IWMOFF.EQ.0) THEN
            WRITE(ICOUT,*) ' SHTFLD: fluid temp. [',TEMP,
     &                     '] outside valid range: -40 < T < 100'
            WRITE(ICOUT,*) ' SHTFLD: specific heat evaluated',
     &                     ' for T = ',VTEMP
         ENDIF

      ENDIF

      IF(IFLD.EQ.1) THEN
        SHTFLD=1006.+(VTEMP/200.)+(VTEMP*VTEMP/7500.)
      ELSE IF(IFLD.EQ.2) THEN
        SHTFLD=1858.4+(.10875*VTEMP)+(3.083E-4*VTEMP*VTEMP)
      ELSE IF(IFLD.EQ.3) THEN
        SHTFLD=4244.-(22.65*SQRT(VTEMP))+(1.95*VTEMP)
      ELSE
         call edisp(icout,' SHTFLD: unsupported fluid type.')
         call epwait
         call epagend
         STOP
      END IF

      RETURN
      END

C ******************** RHOFLD ********************

C RHOFLD is a real function which returns the density (kg/m^3)
C of dry air (IFLD=1), water vapour (IFLD=2), or water (IFLD=3)
C as a function of (TEMP) the fluid temperature (C)
C For the time being this function relies on DENSIT in MFUTIL of ESRUmfs

      FUNCTION RHOFLD(IFLD,TEMP)

      COMMON/SHOUT/ICOUT
      COMMON/WRNOFF/IWMOFF

      IF(IFLD.EQ.1) THEN
         RHOFLD=DENSIT(1,TEMP)
      ELSE IF(IFLD.EQ.3) THEN
         RHOFLD=DENSIT(2,TEMP)
      ELSE
         call edisp(icout,' RHOFLD: unsupported fluid type.')
         call epwait
         call epagend
         STOP
      END IF

      RETURN
      END

C ******************** CNDWAT ********************

C CNDWAT is a real function which returns the energy (J/kg)
C dissipated/necessary in case of condensation/evaporation
C of a unit mass of water as a function of (TEMP) the water
C temperature (C)
C Source for the expression: Recknagel-Sprenger 1979. "Taschenbuch fur
C heizung und klimatechnik", R.Oldenbourg Verlag, Munich

      FUNCTION CNDWAT(TEMP)

      COMMON/SHOUT/ICOUT
      COMMON/WRNOFF/IWMOFF

      VTEMP=TEMP

      IF(TEMP.LT.0..OR.TEMP.GT.100.) THEN

         IF(TEMP.LT.0.0) THEN
            VTEMP=0.0
         ELSE
            VTEMP=100.
         ENDIF

         IF(IWMOFF.EQ.0) THEN
            WRITE(ICOUT,*) ' CNDWAT: water temperature [',TEMP,
     &                     '] outside valid range: 0 < T < 100'
            WRITE(ICOUT,*) ' CNDWAT: heat of condensation',
     &                     ' evaluated for T = ',VTEMP
         ENDIF
      ENDIF

      CNDWAT=1000.*(3158.-2.43*(VTEMP+273.))

      RETURN
      END

********** EMACH *****************
C Subroutine emach is used to calculate the power characteristics of an electrical 
C machine associated a plant component. Currently only rudumentary calculations are 
C performed.
      subroutine EMACH(IPCOMP,IEMODEL,PR,PQ,PA)

# include "plant.h"
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)

C Loop through each component to see if calculation is necessary.

      IF(IEMODEL.EQ.1)THEN
        CALL EMACH001(IPCOMP,PR,PQ,PA)
      elseif ( iemodel.eq.2 ) then
        call emach002(ipcomp,PR,PQ)
      ENDIF
      
      RETURN
      END

****** MACH1 ***********
C Subroutine mach1 calcuates the reactive power component and
C voltage vector of a plant component from power factor information.  
      SUBROUTINE EMACH001(IPCOMP,PR,PQ,PA)
#include "building.h"
#include "plant.h"
#include "power.h"

C Common block for load flow simulation
C Plant
      common/elpcp/NPEL,PFP(mpcom),IPFP(mpcom),PWRP(mpcom),
     &BVOLTP(mpcom),IPHP(mpcom)

      LOGICAL CLOSE

C Calculate the reactive power component. And save to load flow
C simulation common.

      CALL ECLOSE(PFP(IPCOMP),0.000,0.00001,CLOSE)
      IF(CLOSE)THEN     
        PPCMP(IPCOMP)=PR  
        PA=PR
        QPCMP(IPCOMP)=0.0
      ELSE
        PANG=ACOS(PFP(IPCOMP))
        PPCMP(IPCOMP)=PR     
        QPCMP(IPCOMP)=PWRP(IPCOMP)*TAN(PANG)*(IPFP(IPCOMP))
        PQ=QPCMP(IPCOMP)
        PA=(QPCMP(IPCOMP)**2.+PPCMP(IPCOMP)**2.)**(0.5)
      ENDIF

      RETURN
      END

****** MACH2 ***********
C Subroutine mach2 saves the dc and ac power load/generation
C to a common block and sets the other common block variables
C holding the load flow simulation data to zero.


      SUBROUTINE EMACH002(ipcomp,plt_dc_pwr,plt_ac_pwr)
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"

C ESP-r COMMON blocks
C Passed variables
      integer ipcomp      !- plant component index
      real    plt_dc_pwr  !- dc power/load generation associated with plant component
      real    plt_ac_pwr  !- ac power/load generation associated with plant component

      pdcpower(ipcomp) = plt_dc_pwr
      pacpower(ipcomp) = plt_ac_pwr

      ppcmp(ipcomp) = 0.
      qpcmp(ipcomp) = 0.

      RETURN
      END


C ******************** CMP01C ********************

C CMP01C generates for plant component IPCOMP with plant db code 10 ie.
C 1 node (ISV=21) AC  converging 2-leg junction
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C            3 UA modulus (W/K)
C     BDATA: none
C     CDATA: none

      SUBROUTINE CMP01C(IPCOMP,COUT,ISTATS)
#include "building.h"
#include "plant.h"

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
      logical closea

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP01C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,1,2)
      INOD1=NPCDAT(IPCOMP,9)

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C First initialize UA modulus (for calculation of containment heat loss)
         UA=ADATA(IPCOMP,3)
         call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
         IF(closea) UA=0.

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(1,CONVAR(ICON1,1))+
     &      PCONDR(ICON1)*CONVAR(ICON1,3)*SHTFLD(2,CONVAR(ICON1,1))
         C2=PCONDR(ICON2)*CONVAR(ICON2,2)*SHTFLD(1,CONVAR(ICON2,1))+
     &      PCONDR(ICON2)*CONVAR(ICON2,3)*SHTFLD(2,CONVAR(ICON2,1))

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

C 1st phase mass (ie. dry air) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=-PCONDR(ICON1)
         COUT(3)=-PCONDR(ICON2)
         COUT(4)=0.

C 2nd phase mass (ie. vapour) balance coefficients.
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=-PCONDR(ICON1)
         COUT(3)=-PCONDR(ICON2)
         COUT(4)=0.
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=21) AC  converging 2-leg junction'
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
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP01C'

      RETURN
      END
C ******************** CMP02C ********************

C CMP02C generates for plant component IPCOMP with plant db code 20 ie.
C 1 node (ISV=21) AC  steam/spray humidifier 
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C            3 UA modulus (W/K)
C     BDATA: 1 Rated effectiveness (-)
C            2 Rated face velocity (m/s)
C            3 Face area (m^2)
C            4 Mode of operation 1-spray 2-steam
C     CDATA: 1 Water/Steam supply rate (kg/s)
C
C     PCDATF/P
C            1 Water vapour rate actually added to moist air (kg/s)

C Calculation of vapour addition to moist air follows IEA X approach
C involving humidifier effectiveness adjusted for actual face velocity.
C For the steam humidifier a conmstant effectiveness of 99% is assumed.


      SUBROUTINE CMP02C(IPCOMP,COUT,ISTATS)
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

      COMMON/ATPRES/PATMOS
      COMMON/WBULBO/IOPT

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      character outs*124
      logical closea

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP02C'

C Check control data for relevant balance type
      IF(ISTATS.EQ.3.AND.CDATA(IPCOMP,1).LT.0.) THEN
         CALL DAYCLK(IDYP,PTIMEF,IUOUT)
         WRITE(outs,*) ' CMP02C: invalid control data for component ',
     &                  IPCOMP,' : ',CDATA(IPCOMP,1)
         call edisp(iuout,outs)
         call edisp(iuout,' CMP02C: unresolvable error.')
         call epwait
         call epagend
         STOP
      END IF

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      INOD1=NPCDAT(IPCOMP,9)

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C Determine which mode the humidifier is working in (first set default).
         IHMOD=INT(BDATA(IPCOMP,4))
         IF(IHMOD.EQ.0) IHMOD=1

C First initialize UA modulus (for calculation of containment heat loss)
         UA=ADATA(IPCOMP,3)
         call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
         IF(closea) UA=0.

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(1,CONVAR(ICON1,1))+
     &      PCONDR(ICON1)*CONVAR(ICON1,3)*SHTFLD(2,CONVAR(ICON1,1))

         IF(IHMOD.EQ.1) then

C Establish condensation heat of water; assume present component temp.
            RW=CNDWAT(CSVP(INOD1,1))

         ELSEIF(IHMOD.EQ.2) then

C Calculate the enthalpy at the current temp and moisture content. 
c           g1=CONVAR(ICON1,3)/CONVAR(ICON1,2)
c           g2=(CONVAR(ICON1,3)+PCDATF(IPCOMP,1))/CONVAR(ICON1,2)
C Assume steam is added at 100C and cools sensibly to the temperature of
C the humidifier component.
           Q=PCDATF(IPCOMP,1)*2010.*(100.-CSVF(IPCOMP,1))
           
C           hv=PCDATF(IPCOMP,1)*2454.
C           H1=ENTHP2(CONVAR(ICON1,1),G1)
C           H2=H1+HV
C           H12=ENTHP2(CONVAR(ICON1,1),G2)
         ENDIF

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
         IF(IHMOD.EQ.1) then
           COUT(3)=((1.-ALPHA)*(PCRP(ICON1)+PUAP(INOD1))
     &              -CM/TIMSEC)*CSVP(INOD1,1)
     &             +(1.-ALPHA)*(-PCRP(ICON1))*PCTP(ICON1)
     &             -ALPHA*UA*PCNTMF(IPCOMP)
     &             -(1.-ALPHA)*PUAP(INOD1)*PCNTMP(IPCOMP)
     &             +ALPHA*PCDATF(IPCOMP,1)*RW
     &             +(1.-ALPHA)*PCDATP(IPCOMP,1)*RW

         ELSEIF(IHMOD.EQ.2) THEN
           COUT(3)=((1.-ALPHA)*(PCRP(ICON1)+PUAP(INOD1))
     &              -CM/TIMSEC)*CSVP(INOD1,1)
     &             +(1.-ALPHA)*(-PCRP(ICON1))*PCTP(ICON1)
     &             -ALPHA*UA*PCNTMF(IPCOMP)
     &             -(1.-ALPHA)*PUAP(INOD1)*PCNTMP(IPCOMP)
     &             -ALPHA*Q
     &             -(1.-ALPHA)*PCQP(INOD1)
         ENDIF

C Store "environment" variables future values
         PUAF(INOD1)=UA
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCRF(ICON1)=C1

C 1st phase mass (ie. dry air) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=-PCONDR(ICON1)
         COUT(3)=0.

C 2nd phase mass (ie. vapour) balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         AMFR=PCONDR(ICON1)*CONVAR(ICON1,2)

C Establish the humidifier rated (internal) UA value HUAR; assume
C rated values valid for Tair=20 C (ie. Rho=1.2 kg/m^3, Cp=1006 J/kgK)
         IF(IHMOD.EQ.1) then
           HUAR=-ALOG(1.-BDATA(IPCOMP,1))*
     &          (BDATA(IPCOMP,2)*BDATA(IPCOMP,3)*1207.)

C Establish ratio between current and rated face velocity VRAT
           VRAT=AMFR/(RHOFLD(1,CONVAR(ICON1,1))
     &          *BDATA(IPCOMP,3)*BDATA(IPCOMP,2))

C Calculate effectiveness for current face velocity HEFF assuming n=0.6
           IF(AMFR.GE.SMALL) THEN
              HEFF=1.-EXP(-HUAR*VRAT**.6/(AMFR*SHTFLD(1,CSVF(INOD1,1))))
           ELSE
              HEFF=0.
           END IF
         ELSEIF(IHMOD.EQ.2) then
           IF(AMFR.GE.SMALL) THEN
              HEFF=0.99
           ELSE
              HEFF=0.
           END IF          
         ENDIF

C Max moisture content XMAX (kg/kg) at current air temperature (RH=100%)
         XMAX=HUMRT1(CSVF(INOD1,1),100.,PATMOS,IOPT)

C Water vapour rate to add DXR = HEFF * (XMAX - Xin)*(air mass flow)
         DXR=HEFF*(XMAX-CONVAR(ICON1,3)/AMAX1(SMALL,CONVAR(ICON1,2)))
     &       *AMFR

C This is based on current estimate for entering humidity ratio
C so mark vapour mass flow rate for iteration
         ICSV(INOD1,3)=1
         CSVI(INOD1,3)=CSVF(INOD1,3)

         COUT(1)=1.
         COUT(2)=-PCONDR(ICON1)

C Check that DXR does not exceed availabe vapour rate
         IF(CDATA(IPCOMP,1).LE.DXR) THEN
            COUT(3)=CDATA(IPCOMP,1)
         ELSE
            COUT(3)=DXR
         END IF

C Store added water vapour rate future value for energy balance usage
         PCDATF(IPCOMP,1)=COUT(3)
         PCQF(INOD1)=Q
 
C Establish the energy needed to produce the steam assuming and inlet
C temperature of 10C (m*Hv)
         IF(IHMOD.EQ.1)THEN
           QH=0.0
         ELSEIF(IHMOD.EQ.2)THEN
           QH=CDATA(IPCOMP,1)*((2256.7*1000.)+(4200.*90.))
         ENDIF
      END IF
C Save plant additional output.      
      napdat(IPCOMP)=3
      PCAOUT(IPCOMP,1)=PCDATF(IPCOMP,1)
      PCAOUT(IPCOMP,2)=QH
      PCAOUT(IPCOMP,3)=Q

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=21) AC  humidifier adiabatic'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' UA     = ',UA,' (W/K)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
         ELSE IF(ISTATS.EQ.3) THEN
            WRITE(ITU,*) ' HUAR   = ',HUAR,' (W/K)'
            WRITE(ITU,*) ' VRAT   = ',VRAT,' (-)'
            WRITE(ITU,*) ' HEFF   = ',HEFF,' (-)'
            WRITE(ITU,*) ' XMAX   = ',XMAX,' (kg,v/kg,a)'
            WRITE(ITU,*) ' DXR    = ',DXR,' (kg,v/s)'
            WRITE(ITU,*) ' Q   = ',Q,' (W)'  
            WRITE(ITU,*) ' QH   = ',QH,' (W)'           
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
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP02C'

      RETURN
      END

C ******************** CMP03C ********************

C CMP03C generates for plant component IPCOMP with plant db code 30 ie.
C 1 node (ISV=21) AC  fan
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

      SUBROUTINE CMP03C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"
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

C Electrical details for specified plant components
      common/pcelflg/ipcelf(mpcom)
      common/elpcp/NPEL,PFP(mpcom),IPFP(mpcom),PWRP(mpcom),
     &BVOLTP(mpcom),IPHP(mpcom)

      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)

      COMMON/FFN/IFLWN,ICFFS(MPCON)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      character outs*124
      logical closea,closeb

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP03C'

C Check control data for relevant balance type
      IF(ISTATS.EQ.2.AND.CDATA(IPCOMP,1).LT.0.) THEN
         CALL DAYCLK(IDYP,PTIMEF,IUOUT)
         WRITE(outs,*) ' CMP03C: invalid control data for component ',
     &                  IPCOMP,' : ',CDATA(IPCOMP,1)
         call edisp(iuout,outs)
         call edisp(iuout,' CMP03C: unresolvable error.')
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

C Establish absorbed power E based on current air flow rate which might
C have been calculated by mfs
C In case of mfs E is based on flow rate, pressure *rise*, and efficiency
         IF(IFLWN.NE.0.AND.ICFFS(ICON1).NE.0) THEN
            ICNN=ICFFS(ICON1)
            E=real((FLW1(ICNN)+FLW2(ICNN))*
     &             (PRES(NODNE(ICNN))-PRES(NODPS(ICNN))))/
     &                  (BDATA(IPCOMP,3)*RHOFLD(1,CSVF(INOD1,1)))
         ELSE

C Trap possible division by zero.
            call eclose(BDATA(IPCOMP,2),0.00,0.001,closea)
            call eclose(RHOFLD(1,CSVF(INOD1,1)),0.00,0.001,closeb)
            if(closea.or.closeb)then

C << what should E be if demoninator was going to be zero? >>
              E=0.0
            else
              E=(CSVF(INOD1,2)/
     &          (RHOFLD(1,CSVF(INOD1,1))*BDATA(IPCOMP,2)))**3*
     &           BDATA(IPCOMP,1)
            endif
         END IF

C Write the power consumption to the common block
         PWRP(IPCOMP)=-ABS(E)
         IEMODEL=1
         CALL EMACH(IPCOMP,IEMODEL,PWRP(IPCOMP),PQ,PA)
         PWRQ=PQ

C Pass power consumption to Site Utilities facility
         fSUFuelEnergyUse( iElectricity, iUseUncatagorized) = ABS(E)
         call StorePltCompEnergyUse ( IPCOMP, fSUFuelEnergyUse )

C Now Q is made up of all inefficiencies
         Q=E

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(1,CONVAR(ICON1,1))+
     &      PCONDR(ICON1)*CONVAR(ICON1,3)*SHTFLD(2,CONVAR(ICON1,1))

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

C 1st phase mass (ie. dry air) balance coefficients
C Note that if fluid mass flow solver active, source of mass zeroised
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         IF(IFLWN.EQ.0.or.ICFFS(ICON1).eq.0) THEN
            COUT(2)=0.
            COUT(3)=CDATA(IPCOMP,1)*RHOFLD(1,CSVF(INOD1,1))
         ELSE
            COUT(2)=-PCONDR(ICON1)
            COUT(3)=0.
         END IF

C 2nd phase mass (ie. vapour) balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=-PCONDR(ICON1)
         COUT(3)=0.
      END IF

C  Store additional output for the results
      napdat(ipcomp)=3
      pcaout(ipcomp,1)=pwrp(ipcomp)
      pcaout(ipcomp,2)=pwrq
      pcaout(ipcomp,3)=Q

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=21) AC  fan'
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
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP03C'

      RETURN
      END

C ******************** CMP04C ********************

C CMP04C generates for plant component IPCOMP with plant db code 40 ie.
C 1 node (ISV=21) AC  cooling coil & flux control
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C            3 UA modulus (W/K)
C     BDATA: none
C     CDATA: 1 Cooling duty (W)

C     PCDATF/P
C            1 Water vapour rate extracted from moist air (kg/s)

      SUBROUTINE CMP04C(IPCOMP,COUT,ISTATS)
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

      COMMON/ATPRES/PATMOS
      COMMON/WBULBO/IOPT

      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)

C Electrical details for specified plant components
      common/elpcp/NPEL,PFP(mpcom),IPFP(mpcom),PWRP(mpcom),
     &BVOLTP(mpcom),IPHP(mpcom)

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      character outs*124
      logical closea

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP04C'

C Check control data for relevant balance type
      IF(ISTATS.EQ.1.AND.CDATA(IPCOMP,1).GT.0.) THEN
         CALL DAYCLK(IDYP,PTIMEF,IUOUT)
         WRITE(outs,*) ' CMP04C: invalid control data for component ',
     &                  IPCOMP,' : ',CDATA(IPCOMP,1)
         call edisp(iuout,outs)
         call edisp(iuout,' CMP04C: unresolvable error.')
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

C Then initialize extracted heat Q
         IF(CONVAR(ICON1,2).GT.SMALL) THEN
           Q=CDATA(IPCOMP,1)
         ELSE
           Q=0.0
         ENDIF

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(1,CONVAR(ICON1,1))+
     &      PCONDR(ICON1)*CONVAR(ICON1,3)*SHTFLD(2,CONVAR(ICON1,1))

C Calculate future time-row value of rate of condensation based
C on most recent vapour mass balance information
         PCDATF(IPCOMP,1)=PCONDR(ICON1)*CONVAR(ICON1,3)-CSVF(INOD1,3)

C Mark 2nd phase mass flow rate for iteration
         ICSV(INOD1,3)=1
         CSVI(INOD1,3)=CSVF(INOD1,3)

C Establish condensation heat of water; assume most recent node temp.
         RW=CNDWAT(CSVF(INOD1,1))

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
     &             -ALPHA*PCDATF(IPCOMP,1)*RW
     &             -(1.-ALPHA)*PCDATP(IPCOMP,1)*RW

C Store "environment" variables future values
         PUAF(INOD1)=UA
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCRF(ICON1)=C1
         PCQF(INOD1)=Q
         PWRP(IPCOMP)=-ABS(Q)
         IEMODEL=1
         CALL EMACH(IPCOMP,IEMODEL,PWRP(IPCOMP),PQ,PA)
         PWRQ=PQ

C 1st phase mass (ie. dry air) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=-PCONDR(ICON1)
         COUT(3)=0.

C 2nd phase mass (ie. vapour) balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.

C Check whether air temperature is below dew point TDEW, if so
C this component extracts water vapour from moist air stream
C First establish TDEW; assume present moisture content

C If the air flow is very small then this calculation is problematic. So
C Don't bother with dew point calculation.
         IF(CONVAR(ICON1,2).LT.CONVAR(ICON1,3).or.
     &      CONVAR(ICON1,2).LT.SMALL) THEN
           TDEW=CSVF(INOD1,1)-1.
         ELSE 
           TDEW=DEWPT(CONVAR(ICON1,3)/AMAX1(SMALL,CONVAR(ICON1,2)),
     &PATMOS)
         ENDIF
         

C Now establish coefficients, first for Tair >= Tdew; as this
C is based on future temperature, mark temperature for iteration
         ICSV(INOD1,1)=1
         CSVI(INOD1,1)=CSVF(INOD1,1)
         IF(CSVF(INOD1,1).GE.TDEW) THEN
            COUT(2)=-PCONDR(ICON1)
            COUT(3)=0.
         ELSE

C Then for Tair < Tdew in which case we have to know the maximum
C vapour mass flow rate VMAX = (Xmax * air mass flow) at Tair
            VMAX=HUMRT1(CSVF(INOD1,1),100.,PATMOS,IOPT)*
     &           PCONDR(ICON1)*CONVAR(ICON1,2)
            COUT(2)=0.
            COUT(3)=VMAX
         END IF
      END IF

C Store additional output variables for the results file.
      napdat(ipcomp)=3
      pcaout(ipcomp,1)=pwrp(ipcomp)
      pcaout(ipcomp,2)=pwrq
      pcaout(ipcomp,3)=q

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=21) AC  cooling coil & flux control'
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
         ELSE IF(ISTATS.EQ.3) THEN
            WRITE(ITU,*) ' Tcur   = ',CSVF(INOD1,1),' (C)'
            WRITE(ITU,*) ' Tdew   = ',TDEW,' (C)'
            IF(CSVF(INOD1,1).LT.TDEW)
     &         WRITE(ITU,*) ' VPmax  = ',VMAX,' (kg,v/s)'
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
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP04C'

      RETURN
      END

C ******************** CMP05C ********************

C CMP05C generates for plant component IPCOMP with plant db code 50 ie.
C 1 node (ISV=21) AC  heating coil & flux control
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C            3 UA modulus (W/K)
C     BDATA: none
C     CDATA: 1 Heating duty (W)

      SUBROUTINE CMP05C(IPCOMP,COUT,ISTATS)
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

C Electrical details for specified plant components
      common/elpcp/NPEL,PFP(mpcom),IPFP(mpcom),PWRP(mpcom),
     &BVOLTP(mpcom),IPHP(mpcom)

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      character outs*124
      logical closea

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP05C'

C Check control data for relevant balance type
      IF(ISTATS.EQ.1.AND.CDATA(IPCOMP,1).LT.0.) THEN
         CALL DAYCLK(IDYP,PTIMEF,IUOUT)
         WRITE(outs,*) ' CMP05C: invalid control data for component ',
     &                  IPCOMP,' : ',CDATA(IPCOMP,1)
         call edisp(iuout,outs)
         call edisp(iuout,' CMP05C: unresolvable error.')
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

C Then initialize added heat Q. However check that fan is
C ON! 
         IF(CONVAR(ICON1,2).GT.SMALL) THEN
           Q=CDATA(IPCOMP,1)
         ELSE
           Q=0.0
         ENDIF

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(1,CONVAR(ICON1,1))+
     &      PCONDR(ICON1)*CONVAR(ICON1,3)*SHTFLD(2,CONVAR(ICON1,1))

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
         PWRP(IPCOMP)=-ABS(Q)
         IEMODEL=1
         CALL EMACH(IPCOMP,IEMODEL,PWRP(IPCOMP),PQ,PA)
         PWRQ=PQ 
C Save additional output (Heating duty and electrical details).
         napdat(ipcomp)=3
         pcaout(ipcomp,1)=q
         pcaout(ipcomp,2)=pwrp(ipcomp)
         pcaout(ipcomp,3)=pwrq

C 1st phase mass (ie. dry air) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=-PCONDR(ICON1)
         COUT(3)=0.

C 2nd phase mass (ie. vapour) balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=-PCONDR(ICON1)
         COUT(3)=0.
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=21) AC  heating coil & flux control'
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
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP05C'

      RETURN
      END

C ******************** CMP06C ********************

C CMP06C generates for plant component IPCOMP with plant db code 60 ie.
C 1 node (ISV=21) AC  duct & condensation ?
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C            3 UA modulus from wall to environment (W/K)
C            4 Hydraulic diameter of duct (m)
C            5 Length of duct (m)
C     BDATA: none
C     CDATA: none

      SUBROUTINE CMP06C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SHOUT/ICOUT
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

      COMMON/ATPRES/PATMOS

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      logical closea

      PI = 4.0 * ATAN(1.0)

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP06C'

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
            DL=ADATA(IPCOMP,5)
            TA=CONVAR(ICON1,1)
            AMFR=PCONDR(ICON1)*CONVAR(ICON1,2)
            if(amfr.ge.small) then
               VA=AMFR/(RHOFLD(1,TA)*ADATA(IPCOMP,6))
               HI=0.02*(1.+(DH/DL)**(2./3.))
     &            *(153.-0.25*TA)*VA**0.78/DH**0.22
               UA=1./(1./(HI*PI*DH*DL)+1./UA)
               AMCP=AMFR*SHTFLD(1,CONVAR(ICON1,1))
               UA=AMCP*(EXP(UA/AMCP)-1.)
            endif
         END IF

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(1,CONVAR(ICON1,1))+
     &      PCONDR(ICON1)*CONVAR(ICON1,3)*SHTFLD(2,CONVAR(ICON1,1))

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

C 1st phase mass (ie. dry air) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=-PCONDR(ICON1)
         COUT(3)=0.

C 2nd phase mass (ie. vapour) balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=-PCONDR(ICON1)
         COUT(3)=0.
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN

C Check whether air temperature is below dew point TDEW, if so
C issue warning that condensation is likely to occur
C First establish TDEW; assume present moisture content
         IF(CONVAR(ICON1,2).LT.CONVAR(ICON1,3).or.
     &      CONVAR(ICON1,2).LT.1E-6) THEN
           TDEW=CSVF(INOD1,1)-1.
         ELSE
           TDEW=DEWPT(CONVAR(ICON1,3)/AMAX1(SMALL,CONVAR(ICON1,2)),
     &PATMOS)
         ENDIF
         IF(CSVF(INOD1,1).LT.TDEW) THEN
            CALL DAYCLK(IDYP,PTIMEF,ICOUT)
            WRITE(ITU,*) ' CMP06C: duct component ',IPCOMP,' is',
     &                     ' likely to experience internal condensation'
            WRITE(ITU,*) '         Tcur = ',CSVF(INOD1,1),' C',
     &                     '         Tdew = ',TDEW,' C'
         END IF
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=21) AC  duct & condensation ?'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' UA     = ',UA,' (W/K)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
         ELSE IF(ISTATS.EQ.3) THEN
            WRITE(ITU,*) ' Tcur   = ',CSVF(INOD1,1),' (C)'
            WRITE(ITU,*) ' Tdew   = ',TDEW,' (C)'
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
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP06C'

      RETURN
      END

c ******************** CMP07C ********************

c 'cmp07c' generates for plant component IPCOMP with plant db code 70 ie.
c 1 node (ISV=21) AC  damper.
c matrix equation coefficients COUT (in order: self-coupling, cross-
c coupling, and present-time coefficients) for energy balance (ISTATS=1),
c 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
c     adata: 1 Component total mass (kg)
c            2 Mass weighted average specific heat (J/kgK)
c            3 UA modulus (W/K)
c     bdata: none
c     cdata: mass flow fraction entering damper.

      subroutine cmp07c(ipcomp,cout,istats)
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


      COMMON/FFN/IFLWN,ICFFS(MPCON)

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

c In case a mass flow network is defined, 
c reset 'fm' to 1.
      if(iflwn.ne.0.and.icffs(icon1).ne.0) fm=1.0

c Generate coefficients for energy balance equation
      if(istats.eq.1) then

c First initialize UA modulus (for calculation of containment heat loss)
         ua=adata(ipcomp,3)
         call eclose(pcntmf(ipcomp),-99.00,0.001,closea)
         if(closea) ua=0.

c Establish heat capacity of component mass CM (J/K) and
c fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         cm=adata(ipcomp,1)*adata(ipcomp,2)
         asht=shtfld(1,convar(icon1,1))
         vsht=shtfld(2,convar(icon1,1))
         if(abs(fm).lt.0.01) fm=0.01
         c1=pcondr(icon1)*convar(icon1,2)*shtfld(1,convar(icon1,1))+
     &      pcondr(icon1)*convar(icon1,3)*shtfld(2,convar(icon1,1))

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

c 1st phase mass (ie. dry air) balance coefficients
      else if(istats.eq.2) then
         cout(1)=1.
         cout(2)=-pcondr(icon1)*fm
         cout(3)=0.

c 2nd phase mass (ie. vapour) balance coefficients
      else if(istats.eq.3) then
         cout(1)=1.
         cout(2)=-pcondr(icon1)*fm
         cout(3)=0.
      end if
C Save additional output.
      napdat(ipcomp)=1
      pcaout(ipcomp,1)=fm

c Trace output
      if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   itrace(37).ne.0) then
         write(itu,*) ' Component      ',ipcomp,':'
         write(itu,*) ' 1 node (ISV=21) A/C damper'
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
     &   ITRACE(37).ne.0) write(itu,*) ' Leaving subroutine CMP07C'

      return
      end

c ******************** CMP08C ********************
c
c 'cmp08c' generates for plant component IPCOMP with plant db code 80 ie.
c 1 node (ISV=21) AC  flow diversion damper.
c The component alters the flow through the damper and diverts the 
c balance of the flow into a divert connection. The component can operate
c with any plant control loop which generates a value as an output and 
c supercedes plant component CMP07C, which is retained to support legacy
c applications.
c
c matrix equation coefficients COUT (in order: self-coupling, cross-
c coupling, and present-time coefficients) for energy balance (ISTATS=1),
c 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
c     adata: 1 Component total mass (kg)
c            2 Mass weighted average specific heat (J/kgK)
c            3 UA modulus (W/K)
c            4 divert connection # (-) 
c
c     bdata: none
c     cdata: mass flow fraction entering damper.

      subroutine cmp08c(ipcomp,cout,istats)
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

      COMMON/FFN/IFLWN,ICFFS(MPCON)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat
      
      dimension tmdr(mpcom,2)

      parameter (small=1.0e-15)
      real      cout(mpcoe)
      logical closea
      character outs*124
      
      save tmdr

c Trace output
      if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   ITRACE(37).ne.0) write(itu,*) ' Entering subroutine CMP08C'

c Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      icon1=icondx(ipcomp,1,1)
      inod1=npcdat(ipcomp,9)
      icon_aux=int(adata(ipcomp,4))
      
c Initialise the total mass diversion ratios associated with the primary and
C secondary connections. 
      if(int(tmdr(ipcomp,2)).eq.0)then
        tmdr(ipcomp,1)=pcondr(icon1)+pcondr(icon_aux) 
        tmdr(ipcomp,2)=1.
      endif

c Initialise mass flow fraction entering damper.
      fm=cdata(ipcomp,1)

      
c Alter the corresponding flow through the second connection
      if(icon_aux.gt.0.and.icon_aux.lt.npcon)then
        pcondr(icon_aux)=tmdr(ipcomp,1)-pcondr(icon1)*fm
      else
        write(outs,*)'Error damper component ',ipcomp,
     &  'auxilliary connection number ',icon_aux,' is invalid'
        call edisp(iuout,outs)    
      endif


c In case a mass flow network is defined, 
c reset 'fm' to 1.
      if(iflwn.ne.0.and.icffs(icon1).ne.0) fm=1.0

c Generate coefficients for energy balance equation
      if(istats.eq.1) then

c First initialize UA modulus (for calculation of containment heat loss)
         ua=adata(ipcomp,3)
         call eclose(pcntmf(ipcomp),-99.00,0.001,closea)
         if(closea) ua=0.

c Establish heat capacity of component mass CM (J/K) and
c fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         cm=adata(ipcomp,1)*adata(ipcomp,2)
         c1=pcondr(icon1)*convar(icon1,2)*shtfld(1,convar(icon1,1))+
     &      pcondr(icon1)*convar(icon1,3)*shtfld(2,convar(icon1,1))

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

c 1st phase mass (ie. dry air) balance coefficients
      else if(istats.eq.2) then
         cout(1)=1.
         cout(2)=-pcondr(icon1)*fm
         cout(3)=0.

c 2nd phase mass (ie. vapour) balance coefficients
      else if(istats.eq.3) then
         cout(1)=1.
         cout(2)=-pcondr(icon1)*fm
         cout(3)=0.
      end if
      
      napdat(IPCOMP)=1
      pcaout(IPCOMP,1)=FM

c Trace output
      if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   itrace(37).ne.0) then
         write(itu,*) ' Component      ',ipcomp,':'
         write(itu,*) ' 1 node (ISV=21) A/C damper'
         write(itu,*) ' Matrix node(s) ',inod1
         write(itu,*) ' Connection(s)  ',icon1
         write(itu,*) ' Auxilliary     ',icon_aux
         if(istats.eq.1) then
            write(itu,*) ' CM     = ',cm,' (J/K)'
            write(itu,*) ' C1     = ',c1,' (W/K)'
            write(itu,*) ' TC     = ',TC(IPCOMP),' (s)'
            write(itu,*) ' ALPHA  = ',alpha,' (-)'
            write(itu,*) ' UA     = ',ua,' (W/K)'
            write(itu,*) ' PCNTMF = ',pcntmf(ipcomp),' (C)'
            write(itu,*) ' CDATA  = ',cdata(ipcomp,1)
            write(itu,*) ' TMDR   = ',tmdr(ipcomp,1)
            write(itu,*) ' pcondr = ',pcondr(icon_aux)
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
     &   ITRACE(37).ne.0) write(itu,*) ' Leaving subroutine CMP08C'

      return
      end        

C ******************** CMP09C ********************

C CMP09C generates for plant component IPCOMP with plant db code 90 ie.
C 1 node (ISV=21) AC  counterflow cooling coil &
C                     water mass flow rate control
C Rejected heat is calculated using TRNSYS type32 component.
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C            3 UA modulus (W/K)
C     BDATA: 1 Coil face area (m^2)
C            2 Internal tube diameter (m)
C            3 Inlet water temperature (C)
C            4 Number of rows deep (-)
C            5 Number of parallel coil circuits (-)
C     CDATA: 1 Water flow rate (kg/s)

C     PCDATF/P
C            1 Water vapour rate extracted from moist air (kg/s)

      SUBROUTINE CMP09C(IPCOMP,COUT,ISTATS)
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

      COMMON/ATPRES/PATMOS
      COMMON/WBULBO/IOPT

      DIMENSION XIN(6),PAR(6),OUT(10),INFO(10)

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      character outs*124
      logical closea,closeb,closec

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP09C'

C Check control data for relevant balance type
      IF(ISTATS.EQ.1.AND.CDATA(IPCOMP,1).LT.0.) THEN
         CALL DAYCLK(IDYP,PTIMEF,IUOUT)
         WRITE(outs,*) ' CMP09C: invalid control data for component ',
     &                  IPCOMP,' : ',CDATA(IPCOMP,1)
         call edisp(iuout,outs)
         call edisp(iuout,' CMP09C: unresolvable error.')
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

C Now calculate extracted heat Q using single node coil model and
C TRNSYS type32 component, unless water or air mass flow rate is zero
C First mark air 1st phase mass flow rate for iteration
         ICSV(INOD1,2)=1
         CSVI(INOD1,2)=CSVF(INOD1,2)
         call eclose(CDATA(IPCOMP,1),0.00,0.0001,closeb)
         call eclose(CONVAR(ICON1,2),0.00,0.0001,closec)
         IF(closeb.or.closec) THEN
            Q=0.
         ELSE

C For TRNSYS type32 component, setup interface parameters.
            par(1)=1.0
            par(2)=bdata(ipcomp,4)
            par(3)=bdata(ipcomp,5)
            par(4)=bdata(ipcomp,1)
            par(5)=bdata(ipcomp,2)

C Set up input parameters.
            xin(1)=convar(icon1,1)

C Screen wet bulb temperature rerquired (iopt=2).
            patmos=1013.25
            iopt=2
            gh=convar(icon1,3)/convar(icon1,2)
            if(gh.gt.0.2) gh=0.01
            xin(2)=twb(xin(1),gh,patmos,iopt)
            xin(3)=convar(icon1,2)*3600.0
            xin(4)=bdata(ipcomp,3)
            xin(5)=cdata(ipcomp,1)*3600.0
            call type32(time,xin,out,t,dtdt,par,info)
            q=convar(icon1,2)*shtfld(1,convar(icon1,1))*
     &        (out(1)-convar(icon1,1))
         endif

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(1,CONVAR(ICON1,1))+
     &      PCONDR(ICON1)*CONVAR(ICON1,3)*SHTFLD(2,CONVAR(ICON1,1))

C Calculate future time-row value of rate of condensation based
C on most recent vapour mass balance information
         PCDATF(IPCOMP,1)=PCONDR(ICON1)*CONVAR(ICON1,3)-CSVF(INOD1,3)

C Mark 2nd phase mass flow rate for iteration
         ICSV(INOD1,3)=1
         CSVI(INOD1,3)=CSVF(INOD1,3)

C Establish inlet water temperature
         tw=bdata(ipcomp,3)

C Establish specific heat capacity
         CPW=SHTFLD(3,TW)

C Establish condensation heat of water; assume most recent node temp.
         RW=CNDWAT(CSVF(INOD1,1))

C Calculate current component time-constant TC
         TC(IPCOMP)=CM/AMAX1(SMALL,(C1+UA+CDATA(IPCOMP,1)*CPW))

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
     &             -ALPHA*PCDATF(IPCOMP,1)*RW
     &             -(1.-ALPHA)*PCDATP(IPCOMP,1)*RW

C Store "environment" variables future values
         PUAF(INOD1)=UA
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCRF(ICON1)=C1
         PCQF(INOD1)=Q

C Save additional data output.
         napdat(ipcomp)=3
         pcaout(ipcomp,1)=convar(icon1,1)

C Screen wet bulb temperature rerquired (iopt=2).
         gh=convar(icon1,3)/convar(icon1,2)
         if(gh.gt.0.2) gh=0.03
         pcaout(ipcomp,2)=twb(convar(icon1,1),gh,patmos,iopt)
         pcaout(ipcomp,3)=q

C 1st phase mass (ie. dry air) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=-PCONDR(ICON1)
         COUT(3)=0.

C 2nd phase mass (ie. vapour) balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.

C Check whether air temperature is below dew point TDEW, if so
C this component extracts water vapour from moist air stream
C First establish TDEW; assume present moisture content
         TDEW=DEWPT(CONVAR(ICON1,3)/AMAX1(SMALL,CONVAR(ICON1,2)),PATMOS)

C Now establish coefficients, first for Tair >= Tdew; as this
C is based on future temperature, mark temperature for iteration
         ICSV(INOD1,1)=1
         CSVI(INOD1,1)=CSVF(INOD1,1)
         IF(CSVF(INOD1,1).GE.TDEW) THEN
            COUT(2)=-PCONDR(ICON1)
            COUT(3)=0.
         ELSE

C Then for Tair < Tdew in which case we have to know the maximum
C vapour mass flow rate VMAX = (Xmax * air mass flow) at Tair
            VMAX=HUMRT1(CSVF(INOD1,1),100.,PATMOS,IOPT)*
     &           PCONDR(ICON1)*CONVAR(ICON1,2)
            COUT(2)=0.
            COUT(3)=VMAX
         END IF
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=21) AC  cooling coil &',
     &           ' water mass flow rate control (TRNSYS type32)'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' UA     = ',UA,' (W/K)'
            WRITE(ITU,*) ' Q      = ',Q,' (W)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
            WRITE(ITU,*) ' CDATA  = ',CDATA(IPCOMP,1)
         ELSE IF(ISTATS.EQ.3) THEN
            WRITE(ITU,*) ' Tcur   = ',CSVF(INOD1,1),' (C)'
            WRITE(ITU,*) ' Tdew   = ',TDEW,' (C)'
            IF(CSVF(INOD1,1).LT.TDEW)
     &         WRITE(ITU,*) ' VPmax  = ',VMAX,' (kg,v/s)'
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
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP09C'

      RETURN
      END

C ******************** CMP10C ********************

C CMP10C generates for plant component IPCOMP with plant db code 100 ie.
C 1 node (ISV=21) AC  counterflow cooling coil &
C                     water mass flow rate control
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C            3 UA modulus (W/K)
C     BDATA: 1 Coil air side heat transfer area (m^2)
C            2 Coil water side heat transfer area (m^2)
C            3 Coil face area (m^2)
C            4 Metal thermal resistance (m^2K/W)
C            5 Internal tube diameter (m)
C            6 Inlet water temperature (C)
C     CDATA: 1 Water flow rate (kg/s)

C     PCDATF/P
C            1 Water vapour rate extracted from moist air (kg/s)

      SUBROUTINE CMP10C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SHOUT/ICOUT
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

      COMMON/ATPRES/PATMOS
      COMMON/WBULBO/IOPT

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      character outs*124
      logical closea,closeb,closec

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP10C'

C Check control data for relevant balance type
      IF(ISTATS.EQ.1.AND.CDATA(IPCOMP,1).LT.0.) THEN
         CALL DAYCLK(IDYP,PTIMEF,IUOUT)
         WRITE(outs,*) ' CMP10C: invalid control data for component ',
     &                  IPCOMP,' : ',CDATA(IPCOMP,1)
         call edisp(iuout,outs)
         call edisp(iuout,' CMP10C: unresolvable error.')
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

C Now calculate extracted heat Q using single zone coil model and
C the bypass factor method, unless water or air mass flow rate is zero
C First mark air 1st phase mass flow rate for iteration
         ICSV(INOD1,2)=1
         CSVI(INOD1,2)=CSVF(INOD1,2)
         call eclose(CDATA(IPCOMP,1),0.00,0.0001,closeb)
         call eclose(CONVAR(ICON1,2),0.00,0.0001,closec)
         IF(closeb.or.closec) THEN
            Q=0.
         ELSE

C Start with establishing air VA and water VW velocity, and air TA and
C water temperature TW. Then determine air RA, metal RM, and
C water RF thermal resistance
            TA=CONVAR(ICON1,1)
            TW=BDATA(IPCOMP,6)

C Issue non-fatal warning if cooling coil in heating mode
            IF(TW.GT.TA) THEN
               CALL DAYCLK(IDYP,PTIMEF,ICOUT)
               WRITE(outs,*) ' CMP10C: component ',IPCOMP,
     &            ' will heat',' air, based on cooling coil model !'
               call edisp(iuout,outs)
            END IF
            AMFR=PCONDR(ICON1)*CONVAR(ICON1,2)
            wmfr=CDATA(IPCOMP,1)
            VA=AMFR/(BDATA(IPCOMP,3)*RHOFLD(1,TA))
            VW=wmfr/
     &         (RHOFLD(3,TW)*BDATA(IPCOMP,5)**2*3.1416/4.)
            RA=1./(38.*VA)
            RM=BDATA(IPCOMP,4)
            RF=BDATA(IPCOMP,1)/
     &         (BDATA(IPCOMP,2)
     &         *1400.*(1.+0.015*TW)*VW**0.8/BDATA(IPCOMP,5)**0.2)

C Calculate inlet moisture content.
            gs=convar(icon1,3)/convar(icon1,2)

C Call cooling coil subroutine to calculate 
C heat transfer, note 'qa' already negated.
            call ccoilq(bdata(ipcomp,1),bdata(ipcomp,3),
     &                     ra,rm,rf,ta,tw,amfr,wmfr,gs,q,cmfr)
         endif

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(1,CONVAR(ICON1,1))+
     &      PCONDR(ICON1)*CONVAR(ICON1,3)*SHTFLD(2,CONVAR(ICON1,1))

C Calculate future time-row value of rate of condensation based
C on most recent vapour mass balance information
         PCDATF(IPCOMP,1)=PCONDR(ICON1)*CONVAR(ICON1,3)-CSVF(INOD1,3)

C Mark 2nd phase mass flow rate for iteration
         ICSV(INOD1,3)=1
         CSVI(INOD1,3)=CSVF(INOD1,3)

C Establish condensation heat of water; assume most recent node temp.
         RW=CNDWAT(CSVF(INOD1,1))

C Calculate current component time-constant TC
         TC(IPCOMP)=CM/AMAX1(SMALL,(C1+UA+CDATA(IPCOMP,1)*
     &              SHTFLD(3,BDATA(IPCOMP,6))))

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
     &             -ALPHA*PCDATF(IPCOMP,1)*RW
     &             -(1.-ALPHA)*PCDATP(IPCOMP,1)*RW

C Store "environment" variables future values
         PUAF(INOD1)=UA
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCRF(ICON1)=C1
         PCQF(INOD1)=Q

C 1st phase mass (ie. dry air) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=-PCONDR(ICON1)
         COUT(3)=0.

C 2nd phase mass (ie. vapour) balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.

C Check whether air temperature is below dew point TDEW, if so
C this component extracts water vapour from moist air stream
C First establish TDEW; assume present moisture content
         TDEW=DEWPT(CONVAR(ICON1,3)/AMAX1(SMALL,CONVAR(ICON1,2)),PATMOS)

C Now establish coefficients, first for Tair >= Tdew; as this
C is based on future temperature, mark temperature for iteration
         ICSV(INOD1,1)=1
         CSVI(INOD1,1)=CSVF(INOD1,1)
         IF(CSVF(INOD1,1).GE.TDEW) THEN
            COUT(2)=-PCONDR(ICON1)
            COUT(3)=0.
         ELSE

C Then for Tair < Tdew in which case we have to know the maximum
C vapour mass flow rate VMAX = (Xmax * air mass flow) at Tair
            VMAX=HUMRT1(CSVF(INOD1,1),100.,PATMOS,IOPT)*
     &           PCONDR(ICON1)*CONVAR(ICON1,2)
            COUT(2)=0.
            COUT(3)=VMAX
         END IF
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=21) AC  cooling coil &',
     &                ' water mass flow rate control'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' UA     = ',UA,' (W/K)'
            WRITE(ITU,*) ' VA     = ',VA,' (m/s)'
            WRITE(ITU,*) ' VW     = ',VW,' (m/s)'
            WRITE(ITU,*) ' RA     = ',RA,' (m^2.K/W)'
            WRITE(ITU,*) ' RW     = ',RW,' (m^2.K/W)'
            WRITE(ITU,*) ' RM     = ',RM,' (m^2.K/W)'
            WRITE(ITU,*) ' AT     = ',bdata(ipcomp,1),' (m^2)'
            WRITE(ITU,*) ' AF     = ',bdata(ipcomp,3),' (m^2)'
            WRITE(ITU,*) ' TA     = ',TA,' (C)'
            WRITE(ITU,*) ' TW     = ',TW,' (C)'
            WRITE(ITU,*) ' AMFR   = ',AMFR,' (kg/s)'
            WRITE(ITU,*) ' WMFR   = ',WMFR,' (kg/s)'
            WRITE(ITU,*) ' GS     = ',GS,' (kg/kg.da)'
            WRITE(ITU,*) ' Q      = ',Q,' (W)'
            WRITE(ITU,*) ' CMFR   = ',CMFR,' (kg/s)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
            WRITE(ITU,*) ' CDATA  = ',CDATA(IPCOMP,1)
         ELSE IF(ISTATS.EQ.3) THEN
            WRITE(ITU,*) ' Tcur   = ',CSVF(INOD1,1),' (C)'
            WRITE(ITU,*) ' Tdew   = ',TDEW,' (C)'
            IF(CSVF(INOD1,1).LT.TDEW)
     &         WRITE(ITU,*) ' VPmax  = ',VMAX,' (kg,v/s)'
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
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP10C'

      RETURN
      END

C ******************** CMP11C ********************

C CMP11C generates for plant component IPCOMP with plant db code 110 ie.
C 1 node (ISV=21) AC  counterflow heating coil &
C                     water mass flow rate control
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C            3 UA modulus (W/K)
C     BDATA: 1 Coil air side heat transfer area (m^2)
C            2 Coil water side heat transfer area (m^2)
C            3 Coil face area (m^2)
C            4 Metal thermal resistance (m^2K/W)
C            5 Internal tube diameter (m)
C            6 Inlet water temperature (C)
C     CDATA: 1 Water flow rate (kg/s)

      SUBROUTINE CMP11C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SHOUT/ICOUT
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

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      character outs*124
      logical closea,closeb,closec

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP11C'

C Check control data for relevant balance type
      IF(ISTATS.EQ.1.AND.CDATA(IPCOMP,1).LT.0.) THEN
         CALL DAYCLK(IDYP,PTIMEF,IUOUT)
         WRITE(outs,*) ' CMP11C: invalid control data for component ',
     &                  IPCOMP,' : ',CDATA(IPCOMP,1)
         call edisp(iuout,outs)
         call edisp(iuout,' CMP11C: unresolvable error.')
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

C Now calculate transfered heat Q,
C unless water or air mass flow rate is zero
C First mark air 1st phase mass flow rate for iteration
         ICSV(INOD1,2)=1
         CSVI(INOD1,2)=CSVF(INOD1,2)
         call eclose(CDATA(IPCOMP,1),0.00,0.0001,closeb)
         call eclose(CONVAR(ICON1,2),0.00,0.0001,closec)
         IF(closeb.or.closec) THEN
            Q=0.
         ELSE

C Start with establishing air VA and water VW velocity, and air TA and
C water temperature TW. Then determine air RA, metal RM, and
C water RF thermal resistance
            TA=CONVAR(ICON1,1)
            TW=BDATA(IPCOMP,6)

C Mark current air temperature for iteration
            ICSV(INOD1,1)=1
            CSVI(INOD1,1)=CSVF(INOD1,1)

C Issue non-fatal warning if coil in cooling mode
            IF(TW.LT.TA) THEN
               CALL DAYCLK(IDYP,PTIMEF,ICOUT)
               WRITE(outs,*) ' CMP11C: component ',IPCOMP,' will cool',
     &                        ' air, based on heating coil model !!'
               call edisp(iuout,outs)
            END IF
            AMFR=PCONDR(ICON1)*CONVAR(ICON1,2)
            VA=AMFR/(BDATA(IPCOMP,3)*RHOFLD(1,TA))
            VW=CDATA(IPCOMP,1)/
     &         (RHOFLD(3,TW)*BDATA(IPCOMP,5)**2*3.1416/4.)
            RA=1./(38.*VA)
            RM=BDATA(IPCOMP,4)
            RF=BDATA(IPCOMP,1)/
     &         (BDATA(IPCOMP,2)
     &          *1400.*(1.+0.015*TW)*VW**0.8/BDATA(IPCOMP,5)**0.2)

C Calculate heat flow capacities CWAT and CAIR (W/K)
            CWAT=CDATA(IPCOMP,1)*SHTFLD(3,TW)
            CAIR=AMFR*SHTFLD(1,TA)+
     &           PCONDR(ICON1)*CONVAR(ICON1,3)*SHTFLD(2,TA)

C Establish CMIN and CMAX (W/K)
            CMIN=AMIN1(CAIR,CWAT)
            CMAX=AMAX1(CAIR,CWAT)

C Establish capacity ratio CRAT, RNTU, and EFF-ectiveness
            CRAT=CMIN/CMAX
            RNTU=BDATA(IPCOMP,1)/(CMIN*(RA+RM+RF))
            EFF=(1.-EXP(-RNTU*(1.-CRAT)))/(1.-CRAT*EXP(-RNTU*(1.-CRAT)))

C And finally calcultate heat transferred
            Q=CMIN*EFF*(TW-TA)
         END IF

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(1,CONVAR(ICON1,1))+
     &      PCONDR(ICON1)*CONVAR(ICON1,3)*SHTFLD(2,CONVAR(ICON1,1))

C Calculate current component time-constant TC
         TC(IPCOMP)=CM/AMAX1(SMALL,(C1+UA+CWAT))

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

C 1st phase mass (ie. dry air) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=-PCONDR(ICON1)
         COUT(3)=0.

C 2nd phase mass (ie. vapour) balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=-PCONDR(ICON1)
         COUT(3)=0.
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=21) AC  heating coil &',
     &                ' water mass flow rate control'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' UA     = ',UA,' (W/K)'
            WRITE(ITU,*) ' VA     = ',VA,' (m/s)'
            WRITE(ITU,*) ' VW     = ',VW,' (m/s)'
            WRITE(ITU,*) ' RA     = ',RA,' (m^2.K/W)'
            WRITE(ITU,*) ' RF     = ',RF,' (m^2.K/W)'
            WRITE(ITU,*) ' EFF    = ',EFF,' (-)'
            WRITE(ITU,*) ' Q      = ',Q,' (W)'
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
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP11C'

      RETURN
      END

C ******************** CMP12C ********************

C CMP12C generates for plant component IPCOMP with plant db code 120 ie.
C 2 node (ISV=21) AC  plate heat exchanger
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C            3 UA modulus (W/K)
C     BDATA: 1 Total plate heat transfer area (m^2)
C            2 Heat exchanger net face area (m^2)
C     CDATA: none

      SUBROUTINE CMP12C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SHOUT/ICOUT
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

      COMMON/ATPRES/PATMOS
      COMMON/WRNOFF/IWMOFF

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      logical closea,closeb,closec

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP12C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,2,1)
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C First initialize UA modulus (for calculation of containment heat loss)
         UA=ADATA(IPCOMP,3)
         call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
         IF(closea) UA=0.

C Calculate transfered heat Q using single zone coil model,
C unless water or air mass flow rate is zero.
C First mark air 1st phase mass flow rates for iteration
         ICSV(INOD1,2)=1
         CSVI(INOD1,2)=CSVF(INOD1,2)
         ICSV(INOD2,2)=1
         CSVI(INOD2,2)=CSVF(INOD2,2)
         call eclose(CONVAR(ICON1,2),0.00,0.0001,closeb)
         call eclose(CONVAR(ICON2,2),0.00,0.0001,closec)
         IF(closeb.or.closec) THEN
            Q=0.
            RA1=0.10
            RA1=0.10
         ELSE

C Start with establishing air velocities VA1 and VA2, and air
C temperatures TA1 and TA2.
C Then determine air thermal resistances RA1 and RA2
            TA1=CONVAR(ICON1,1)
            TA2=CONVAR(ICON2,1)

C Mark current air temperatures for iteration
            ICSV(INOD1,1)=1
            CSVI(INOD1,1)=CSVF(INOD1,1)
            ICSV(INOD2,1)=1
            CSVI(INOD2,1)=CSVF(INOD2,1)
            AMFR1=PCONDR(ICON1)*CONVAR(ICON1,2)
            AMFR2=PCONDR(ICON2)*CONVAR(ICON2,2)
            VA1=AMFR1/(BDATA(IPCOMP,2)*RHOFLD(1,TA1))
            VA2=AMFR2/(BDATA(IPCOMP,2)*RHOFLD(1,TA2))
            RA1=1./(10.*VA1)
            RA2=1./(10.*VA2)

C Calculate heat flow capacities CAIR1 and CAIR2 (W/K)
            CAIR1=AMFR1*SHTFLD(1,TA1)+
     &            PCONDR(ICON1)*CONVAR(ICON1,3)*SHTFLD(2,TA1)
            CAIR2=AMFR2*SHTFLD(1,TA2)+
     &            PCONDR(ICON2)*CONVAR(ICON2,3)*SHTFLD(2,TA2)

C Establish CMIN and CMAX (W/K)
            CMIN=AMIN1(CAIR1,CAIR2)
            CMAX=AMAX1(CAIR1,CAIR2)

C Establish capacity ratio CRAT, ETA, RNTU, and EFF-ectiveness
            CRAT=CMIN/CMAX
            RNTU=BDATA(IPCOMP,1)/(CMIN*(RA1+RA2))
            ETA=RNTU**(-0.22)
            EFF=1.-EXP((EXP(-RNTU*ETA*CRAT)-1.)/(ETA*CRAT))

C And finally calcultate heat transferred between air streams Q
            Q=CMIN*EFF*(TA2-TA1)
         END IF

C Establish heat capacity of component mass CM (J/K), and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)/2
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(1,CONVAR(ICON1,1))+
     &      PCONDR(ICON1)*CONVAR(ICON1,3)*SHTFLD(2,CONVAR(ICON1,1))
         C2=PCONDR(ICON2)*CONVAR(ICON2,2)*SHTFLD(1,CONVAR(ICON2,1))+
     &      PCONDR(ICON2)*CONVAR(ICON2,3)*SHTFLD(2,CONVAR(ICON2,1))

C Calculate current component time-constant TC (max. of solid and water)
         TC(IPCOMP)=AMAX1(
     &        CM/AMAX1(SMALL,(C1+Q/(TA2-TA1))),
     &        CM/AMAX1(SMALL,(C2+Q/(TA2-TA1))))

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
         COUT(1)=ALPHA*(-C1-UA/2)-CM/TIMSEC
         COUT(2)=ALPHA*(-C2-UA/2)-CM/TIMSEC
C then matrix equation cross-coupling coefficients,
         COUT(3)=ALPHA*C1
         COUT(4)=ALPHA*C2
C and then present-time coefficients (ie. right hand sides)
         COUT(5)=((1.-ALPHA)*(C1+UA/2)-CM/TIMSEC)*CSVP(INOD1,1)
     &           +(1.-ALPHA)*(-PCRP(ICON1))*PCTP(ICON1)
     &           -ALPHA*UA/2*PCNTMF(IPCOMP)
     &           -(1.-ALPHA)*PUAP(INOD1)/2*PCNTMP(IPCOMP)
     &           -ALPHA*Q-(1.-ALPHA)*PCQP(INOD1)
         COUT(6)=((1.-ALPHA)*(C2+UA/2)-CM/TIMSEC)*CSVP(INOD2,1)
     &           +(1.-ALPHA)*(-PCRP(ICON2))*PCTP(ICON2)
     &           -ALPHA*UA/2*PCNTMF(IPCOMP)
     &           -(1.-ALPHA)*PUAP(INOD2)/2*PCNTMP(IPCOMP)
     &           +ALPHA*Q+(1.-ALPHA)*PCQP(INOD1)

C Store "environment" variables future values
         PUAF(INOD1)=UA
         PUAF(INOD2)=UA
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCTF(ICON2)=CONVAR(ICON2,1)
         PCRF(ICON1)=C1
         PCRF(ICON2)=C2
         PCQF(INOD1)=Q
         PCQF(INOD2)=Q

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

C Check whether air temperature is below dew point TDEW, if so
C issue warning that condensation is likely to occur
C First establish TDW1 and TDW2; assume present moisture contents
         IF(CONVAR(ICON1,2).LT.CONVAR(ICON1,3)) THEN
           TDW1=CSVF(INOD1,1)-1.
         ELSE
           TDW1=DEWPT(CONVAR(ICON1,3)/AMAX1(SMALL,CONVAR(ICON1,2)),
     &PATMOS)
         ENDIF
         IF(CONVAR(ICON2,2).LT.CONVAR(ICON2,3)) THEN
           TDW1=CSVF(INOD2,1)-1.
         ELSE
           TDW2=DEWPT(CONVAR(ICON2,3)/AMAX1(SMALL,CONVAR(ICON2,2)),
     &PATMOS)
         ENDIF

         IF(CSVF(INOD1,1).LT.TDW1.OR.CSVF(INOD2,1).LT.TDW2.and.
     &      iwmoff.eq.0) THEN
            CALL DAYCLK(IDYP,PTIMEF,ICOUT)
            WRITE(ICOUT,*) ' CMP12C: plate exchanger ',IPCOMP,' is',
     &                     ' likely to experience internal condensation'
            WRITE(ICOUT,*) '         Tcur1 = ',CSVF(INOD1,1),' C',
     &                     '         Tcur2 = ',CSVF(INOD2,1),' C'
            WRITE(ICOUT,*) '         Tdew1 = ',TDW1,' C',
     &                     '         Tdew2 = ',TDW2,' C'
         END IF
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 2 node (ISV=21) AC  plate heat exchanger'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2
         WRITE(ITU,*) ' Connection(s)  ',ICON1,ICON2
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' C2     = ',C2,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' UA     = ',UA,' (W/K)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
            WRITE(ITU,*) ' TA1    = ',TA1,' (C)'
            WRITE(ITU,*) ' TA2    = ',TA2,' (C)'
            WRITE(ITU,*) ' VA1    = ',VA1,' (m/s)'
            WRITE(ITU,*) ' VA2    = ',VA2,' (m/s)'
            WRITE(ITU,*) ' HA1    = ',1/RA1,' (W/m^2K)'
            WRITE(ITU,*) ' HA2    = ',1/RA2,' (W/m^2K)'
            WRITE(ITU,*) ' Cmin   = ',CMIN,' (W/K) '
            WRITE(ITU,*) ' Cmax   = ',CMAX,' (W/K) '
            WRITE(ITU,*) ' Cratio = ',CRAT,' (-) '
            WRITE(ITU,*) ' NTU    = ',RNTU,' (-) '
            WRITE(ITU,*) ' ETA    = ',ETA,' (-) '
            WRITE(ITU,*) ' EFF    = ',EFF,' (-) '
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
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP12C'

      RETURN
      END

C ******************** CMP13C ********************

C CMP13C generates for plant component IPCOMP with plant db code 130 ie.
C 2 node (ISV=21) basic AC heat exchanger model. The model can be used to
C represent any air-air heat exchanger and also has a moisture recovery 
C capability (i.e. used when modelling thermal wheels).

C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C            3 Heat loss modulus (W/K)
C     BDATA: 1 Heat exchanger effectiveness (%)
C            2 Moisture recovery (%)
C     CDATA: none

      SUBROUTINE CMP13C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SHOUT/ICOUT
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

      COMMON/ATPRES/PATMOS
      COMMON/WRNOFF/IWMOFF

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE),w_vap_recov
      logical closea,closeb,closec

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP13C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,2,1)
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1
      w_vap_recov=0.0

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C First initialize UA modulus (for calculation of containment heat loss)
         UA=ADATA(IPCOMP,3)
         call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
         IF(closea) UA=0.


C First mark air 1st phase mass flow rates for iteration
         ICSV(INOD1,2)=1
         CSVI(INOD1,2)=CSVF(INOD1,2)
         ICSV(INOD2,2)=1
         CSVI(INOD2,2)=CSVF(INOD2,2)
         call eclose(CONVAR(ICON1,2),0.00,0.0001,closeb)
         call eclose(CONVAR(ICON2,2),0.00,0.0001,closec)
         IF(closeb.or.closec) THEN
            Q=0.
         ELSE

C Start with establishing air velocities VA1 and VA2, and air
C temperatures TA1 and TA2.
C Then determine air thermal resistances RA1 and RA2
            TA1=CONVAR(ICON1,1)
            TA2=CONVAR(ICON2,1)

C Mark current air temperatures and second phase flow of node 2 for iteration
            ICSV(INOD1,1)=1
            CSVI(INOD1,1)=CSVF(INOD1,1)
            ICSV(INOD2,1)=1
            CSVI(INOD2,1)=CSVF(INOD2,1)
            ICSV(INOD2,3)=1
            CSVI(INOD2,3)=CSVF(INOD2,3)
            AMFR1=PCONDR(ICON1)*CONVAR(ICON1,2)
            AMFR2=PCONDR(ICON2)*CONVAR(ICON2,2)

C Calculate heat flow capacities CAIR1 and CAIR2 (W/K)
            CAIR1=AMFR1*SHTFLD(1,TA1)+
     &            PCONDR(ICON1)*CONVAR(ICON1,3)*SHTFLD(2,TA1)
            CAIR2=AMFR2*SHTFLD(1,TA2)+
     &            PCONDR(ICON2)*CONVAR(ICON2,3)*SHTFLD(2,TA2)

C Establish CMIN and CMAX (W/K)
            CMIN=AMIN1(CAIR1,CAIR2)
            CMAX=AMAX1(CAIR1,CAIR2)

C Establish  EFF-ectiveness
            CRAT=CMIN/CMAX
            EFF=BDATA(IPCOMP,1)/100.

C And finally calcultate heat transferred between air streams Q
            Q=CMIN*EFF*(TA2-TA1)
         ENDIF

C Set the moisture recovery
         w_vap_recov=BDATA(IPCOMP,2)/100.0
  
C Establish heat capacity of component mass CM (J/K), and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)/2
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(1,CONVAR(ICON1,1))+
     &      PCONDR(ICON1)*CONVAR(ICON1,3)*SHTFLD(2,CONVAR(ICON1,1))
         C2=PCONDR(ICON2)*CONVAR(ICON2,2)*SHTFLD(1,CONVAR(ICON2,1))+
     &      PCONDR(ICON2)*CONVAR(ICON2,3)*SHTFLD(2,CONVAR(ICON2,1))
     
C Calculate current component time-constant TC (max. of solid and water)
         TC(IPCOMP)=AMAX1(
     &        CM/AMAX1(SMALL,(C1+Q/(TA2-TA1))),
     &        CM/AMAX1(SMALL,(C2+Q/(TA2-TA1))))

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
         COUT(1)=ALPHA*(-C1-UA/2)-CM/TIMSEC
         COUT(2)=ALPHA*(-C2-UA/2)-CM/TIMSEC
C then matrix equation cross-coupling coefficients,
         COUT(3)=ALPHA*C1
         COUT(4)=ALPHA*C2
C and then present-time coefficients (ie. right hand sides)
         COUT(5)=((1.-ALPHA)*(C1+UA/2)-CM/TIMSEC)*CSVP(INOD1,1)
     &           +(1.-ALPHA)*(-PCRP(ICON1))*PCTP(ICON1)
     &           -ALPHA*UA/2*PCNTMF(IPCOMP)
     &           -(1.-ALPHA)*PUAP(INOD1)/2*PCNTMP(IPCOMP)
     &           -ALPHA*Q-(1.-ALPHA)*PCQP(INOD1)
         COUT(6)=((1.-ALPHA)*(C2+UA/2)-CM/TIMSEC)*CSVP(INOD2,1)
     &           +(1.-ALPHA)*(-PCRP(ICON2))*PCTP(ICON2)
     &           -ALPHA*UA/2*PCNTMF(IPCOMP)
     &           -(1.-ALPHA)*PUAP(INOD2)/2*PCNTMP(IPCOMP)
     &           +ALPHA*Q+(1.-ALPHA)*PCQP(INOD1)

C Store "environment" variables future values
         PUAF(INOD1)=UA
         PUAF(INOD2)=UA
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCTF(ICON2)=CONVAR(ICON2,1)
         PCRF(ICON1)=C1
         PCRF(ICON2)=C2
         PCQF(INOD1)=Q
         PCQF(INOD2)=Q

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
         COUT(5)=w_vap_recov*(PCONDR(ICON2)*CONVAR(ICON2,3)-
     &PCONDR(ICON1)*CONVAR(ICON1,3))
         COUT(6)=-w_vap_recov*(PCONDR(ICON2)*CONVAR(ICON2,3)-
     &PCONDR(ICON1)*CONVAR(ICON1,3))

 
C Check whether air temperature is below dew point TDEW, if so
C issue warning that condensation is likely to occur
C First establish TDW1 and TDW2; assume present moisture contents
         IF(CONVAR(ICON1,2).LT.CONVAR(ICON1,3)) THEN
           TDW1=CSVF(INOD1,1)-1.
         ELSE
           TDW1=DEWPT(CONVAR(ICON1,3)/AMAX1(SMALL,CONVAR(ICON1,2)),
     &PATMOS)
         ENDIF            
         IF(CONVAR(ICON2,2).LT.CONVAR(ICON2,3)) THEN
           TDW1=CSVF(INOD2,1)-1.
         ELSE
           TDW2=DEWPT(CONVAR(ICON2,3)/AMAX1(SMALL,CONVAR(ICON2,2)),
     &PATMOS)
         ENDIF

         IF(CSVF(INOD1,1).LT.TDW1.OR.CSVF(INOD2,1).LT.TDW2.and.
     &      iwmoff.eq.0) THEN
            CALL DAYCLK(IDYP,PTIMEF,ICOUT)
            WRITE(ICOUT,*) ' CMP13C: heat exchanger ',IPCOMP,' is',
     &                     ' likely to experience internal condensation'
            WRITE(ICOUT,*) '         Tcur1 = ',CSVF(INOD1,1),' C',
     &                     '         Tcur2 = ',CSVF(INOD2,1),' C'
            WRITE(ICOUT,*) '         Tdew1 = ',TDW1,' C',
     &                     '         Tdew2 = ',TDW2,' C'
         END IF
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 2 node (ISV=21) Basic heat exchanger'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2
         WRITE(ITU,*) ' Connection(s)  ',ICON1,ICON2
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' C2     = ',C2,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' UA     = ',UA,' (W/K)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
            WRITE(ITU,*) ' TA1    = ',TA1,' (C)'
            WRITE(ITU,*) ' TA2    = ',TA2,' (C)'
            WRITE(ITU,*) ' Cmin   = ',CMIN,' (W/K) '
            WRITE(ITU,*) ' Cmax   = ',CMAX,' (W/K) '
            WRITE(ITU,*) ' Cratio = ',CRAT,' (-) '
            WRITE(ITU,*) ' EFF    = ',EFF,' (-) '
            WRITE(ITU,*) ' Q      = ',Q,' (W)'
            WRITE(ITU,*) ' Vapour recovered    = ',
     &       w_vap_recov*(PCONDR(ICON2)*CONVAR(ICON2,3)-
     &PCONDR(ICON1)*CONVAR(ICON1,3)),' (-) '
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
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP13C'

      RETURN
      END

C ******************** CMP14C ********************
C CMP14C generates for plant component IPCOMP with plant db code 120 ie.
C 2 node (ISV>21) 2-node  Cooling tower model.
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Mass of tower (kg)
C            2 Packing volume (m^3)
C            3 Contact area per m^3 of packing (m^-1)
C            4 Vapour transfer coefficient (kg/s m^2)
C            5 UA modulus for the component (W/K)
C            6 Mass weighted average specific heat (kJ/kgK)
C     BDATA: NONE
C     CDATA: NONE
C The component has two nodes 1. - Moist air node
C                             2. - Liquid mode
C
C The evaluation of the exit liquid temperature uses an iterative proceedure
C based on MERKEL's cooling tower equations. The theory behind the model is
C detailed in IEA annex X s8.
C
C
      SUBROUTINE CMP14C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PTIME/PTIMEP,PTIMEF
      COMMON/Pctime/TIMSEC
      COMMON/PCTC/TC(MPCOM)
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
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
      COMMON/CTWRIT/TWEXF
      COMMON/ATPRES/PATMOS

      PARAMETER (SMALL=1.0E-20)
      REAL      COUT(MPCOE),NTUr,NTUa,KG 
      LOGICAL CLOSE      
      character outs*124

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP14C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,2,1)
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1
      icall=0

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C Establish heat loss modulus
        UA=ADATA(IPCOMP,5)
        CALL ECLOSE(PCNTMF(IPCOMP),99.,0.0001,CLOSE)
        IF(CLOSE) UA=0.0

C Calculate extracted heat Q using  and Merkels theory, mark the 
C air and water mass flows for iteration as well as the water inlet
C temp.
        ICSV(INOD1,2)=1
        CSVI(INOD1,2)=CSVF(INOD1,2)
        ICSV(INOD2,2)=1
        CSVI(INOD2,2)=CSVF(INOD2,2)
        ICSV(INOD2,1)=1
        CSVI(INOD2,1)=CSVF(INOD2,1)

C Establish the nodal thermal capacities.
        CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,6)
        C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(1,CONVAR(ICON1,1))+
     &      PCONDR(ICON1)*CONVAR(ICON1,3)*SHTFLD(2,CONVAR(ICON1,1))
        C2=PCONDR(ICON2)*CONVAR(ICON2,2)*SHTFLD(3,CONVAR(ICON2,1))

        AMFR=PCONDR(ICON2)*CONVAR(ICON2,2)
        WMFR=PCONDR(ICON2)*CONVAR(ICON2,2)

C Set the internal iteration flag and counter.
        NCTITER=1

C If any of th mass flows are zero then set transfered flux to zero.
        IF(CONVAR(ICON1,2).LT.SMALL.OR.CONVAR(ICON2,2).LT.SMALL) THEN
          QSW=0.0
        ELSEIF(CONVAR(ICON1,1).GT.CONVAR(ICON2,1)) THEN
          QSW=0.0
        ELSE

C Calculate the NTUa from the component data.
          VOL=ADATA(IPCOMP,2)
          AMT=ADATA(IPCOMP,3)
          KG=ADATA(IPCOMP,4)
          
          NTUa=(KG*VOL*AMT)/(PCONDR(ICON1)*CONVAR(ICON1,2))

C Note that all enthalpies are in kJ/kg.
C Calculate enthalpies around the  inlet conditions.
C Assume that the saturated vapour and water are at the same temp.
C First the saturated water temp.
          TSSU=CONVAR(ICON2,1)
          VCNT=HUMR(CONVAR(ICON2,1),100.,PATMOS)

C Saturated water enthalpy.
          HSSU=ENTHP2(TSSU,VCNT)

C Inlet water enthalpy
          TWSU=CONVAR(ICON2,1)
          HWSU=SHTFLD(3,TWSU)*TWSU/1000.

C Calculate the inlet air enthalpy
          TSU=CONVAR(ICON1,1)

C Calculate the downstream air enthalpy
          GSU=CONVAR(ICON1,3)/CONVAR(ICON1,2)
          IF(GSU.LT.SMALL) GSU=SMALL
          HSU=ENTHP2(CONVAR(ICON1,1),GSU)
          IF(HSU.LT.0.0) HSU=0.01

C Use old value of exit temperature.
C If at start of simulation, initialise array info.
         if(icall.eq.0) then
          TWEXI=TWSU-((TWSU-TSU)/2.)
          icall=1
         else
          TWEXI=TWEXF
         endif
         IF(TWEXI.LT.0.0) TWEXI=0.001
         IF(TWEXI.GT.100.0) TWEXI=99.99

C Iterate to find the exit temperature
  5       HWEX=SHTFLD(3,TWEXI)*TWEXI/1000.
          HEX=HSU + (WMFR/AMFR)*(HWSU-HWEX)

C Calculate the NTUr
          CALL NTUCT(TWSU,TWEXI,HEX,HSU,RNTU)
          NTUR=RNTU
          IF(NCTITER.LT.1000) THEN
            IF(ABS(NTUA-NTUR).GT.0.001) THEN
              IF(NTUR.LT.NTUA) TWEXI=TWEXI-0.001               
              IF(NTUR.GT.NTUA) TWEXI=TWEXI+0.001
              IF(TWEXI.LT.0.0) TWEXI=0.001
              IF(TWEXI.GT.100.0) TWEXI=99.99
              NCTITER=NCTITER+1
              GOTO 5
            ENDIF
          ELSE
            WRITE(outs,*) 'NTUr ',NTUR, ' NTUa ',NTUA, ' T ',TWEXI
            call edisp(iuout,outs)
            call edisp(iuout,'maximum number of iterations exceeded.')
          ENDIF

C Store the value of TWEX            
          TWEXF=TWEXI
              
C Calculate the air exit enthalpy
          HEX=HSU + (WMFR/AMFR)*(HWSU-HWEX)         

C Once iterations have converged calculate the transfered fluxes;
C QSW.
          QSW=(TWEXI-TWSU)*C2

C Calculate the overall effectiveness
          GSS=HUMR(TWEXI,100.,PATMOS)
          HSEX=ENTHP2(TWEXI,GSU)
          IF(GSU.LT.SMALL) GSU=SMALL
          EFF=(HSSU - HSEX)/(HSSU - HSU)
        ENDIF

C Ignore the effects on the air stream as this exits to the environment.
C We only need to know the water exit temperature...
         TC(IPCOMP)=CM/AMAX1(SMALL,(C1+C2))
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

C Establish matrix equation self-coupling coefficients.
          COUT(1)=ALPHA*(-C1-UA/2)
          COUT(2)=ALPHA*(-C2-UA/2)-CM/TIMSEC

C Then matrix equation cross-coupling coefficients.
          COUT(3)=ALPHA*C1
          COUT(4)=ALPHA*C2

C Present-time coefficients (ie. right hand sides) - ignoring
C thermal mass and transport lags for air node.
          COUT(5)=-(UA/2.)*PCNTMF(IPCOMP)
          COUT(6)= ((1.-ALPHA)*(PCRP(ICON2)+UA/2)-CM/TIMSEC)
     &             *CSVP(INOD2,1)
     &             -(1.-ALPHA)*PCRP(ICON2)*PCTP(ICON2)
     &             -ALPHA*(UA/2)*PCNTMF(IPCOMP)
     &             -(1.-ALPHA)*UA/2*PCNTMP(IPCOMP)
     &             -ALPHA*QSW
     &             -(1.-ALPHA)*PCQP(INOD2)

C Store "environment" variables future values
          PCTF(ICON1)=CONVAR(ICON1,1)
          PCTF(ICON2)=CONVAR(ICON2,1)
          PCRF(ICON1)=C1
          PCRF(ICON2)=C2
          PCQF(INOD2)=QSW
          TWEXF=TWEXI

      ELSE IF(ISTATS.EQ.2) THEN
C 1st phase mass (ie. "water") balance coefficients
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
      ENDIF
C Write out the addtional output
      NAPDAT(IPCOMP)=3
      PCAOUT(IPCOMP,1)=EFF
      PCAOUT(IPCOMP,2)=-QSW/1000.0
      PCAOUT(IPCOMP,3)=NTUr
        

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 2 node (ISV=21) AC  cooling tower'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2
         WRITE(ITU,*) ' Connection(s)  ',ICON1,ICON2
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' C2     = ',C2,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' UA     = ',UA,' (W/K)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
            WRITE(ITU,*) ' NTUa    = ',NTUa,' (-)'
            WRITE(ITU,*) ' NTUr    = ',NTUr,' (-)'
            WRITE(ITU,*) ' Temp inlet = ',TWSU,' (DegC)'
            WRITE(ITU,*) ' Temp exit = ',TWEXI,' (DegC)'
            WRITE(ITU,*) ' HSU    = ',HSU,' (kJ/kg)'
            WRITE(ITU,*) ' HWSU    = ',HWSU,' (kJ/kg) '
            WRITE(ITU,*) ' HSSU    = ',HSSU,' (kJ/kg) '
            WRITE(ITU,*) ' HEX    = ',HEX,' (kJ/kg) '
            WRITE(ITU,*) ' HWEX    = ',HWEX,' (kJ/kg) '
            WRITE(ITU,*) ' EFF    = ',EFF,' (-) '
            WRITE(ITU,*) ' QSW      = ',-QSW/1000.,' (kW)'

         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=6
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/6)*6
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP14C'

      RETURN
      END

C ******************** CMP15C ********************

C CMP03C generates for plant component IPCOMP with plant db code 30 ie.
C 1 node (ISV=21) AC  fan with mass flow control.
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

      SUBROUTINE CMP15C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"

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

C Electrical details for specified plant components
      common/elpcp/NPEL,PFP(mpcom),IPFP(mpcom),PWRP(mpcom),
     &BVOLTP(mpcom),IPHP(mpcom)

      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

      COMMON/FFN/IFLWN,ICFFS(MPCON)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      character outs*124
      logical closea,closeb

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP15C'

C Check control data for relevant balance type
      IF(ISTATS.EQ.2.AND.CDATA(IPCOMP,1).LT.0.) THEN
         CALL DAYCLK(IDYP,PTIMEF,IUOUT)
         WRITE(outs,*) ' CMP15C: invalid control data for component ',
     &                  IPCOMP,' : ',CDATA(IPCOMP,1)
         call edisp(iuout,outs)
         call edisp(iuout,' CMP15C: unresolvable error.')
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

C Establish absorbed power E based on current air flow rate which might
C have been calculated by mfs
C In case of mfs E is based on flow rate, pressure *rise*, and efficiency
         IF(IFLWN.NE.0.AND.ICFFS(ICON1).NE.0) THEN
            ICNN=ICFFS(ICON1)
            E=real((FLW1(ICNN)+FLW2(ICNN))*
     &             (PRES(NODNE(ICNN))-PRES(NODPS(ICNN))))/
     &                  (BDATA(IPCOMP,3)*RHOFLD(1,CSVF(INOD1,1)))
         ELSE

C Trap possible division by zero.
            call eclose(BDATA(IPCOMP,2),0.00,0.001,closea)
            call eclose(RHOFLD(1,CSVF(INOD1,1)),0.00,0.001,closeb)
            if(closea.or.closeb)then

C << what should E be if demoninator was going to be zero? >>
              E=0.0
            else
              E=(CSVF(INOD1,2)/BDATA(IPCOMP,2))**3*BDATA(IPCOMP,1)
            endif
         END IF

C Write the power consumption to the common block
         PWRP(IPCOMP)=-ABS(E)
         IEMODEL=1
         CALL EMACH(IPCOMP,IEMODEL,PWRP(IPCOMP),PQ,PA)
         PWRQ=PQ
C Now Q is made up of all inefficiencies
         Q=E

C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(1,CONVAR(ICON1,1))+
     &      PCONDR(ICON1)*CONVAR(ICON1,3)*SHTFLD(2,CONVAR(ICON1,1))

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

C 1st phase mass (ie. dry air) balance coefficients
C Note that if fluid mass flow solver active, source of mass zeroised
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         IF(IFLWN.EQ.0.or.ICFFS(ICON1).eq.0) THEN
            COUT(2)=0.
            COUT(3)=CDATA(IPCOMP,1)
         ELSE
            COUT(2)=-PCONDR(ICON1)
            COUT(3)=0.
         END IF

C 2nd phase mass (ie. vapour) balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=-PCONDR(ICON1)
         COUT(3)=0.
      END IF

C  Store additional output for the results
      napdat(ipcomp)=3
      pcaout(ipcomp,1)=pwrp(ipcomp)
      pcaout(ipcomp,2)=pwrq
      pcaout(ipcomp,3)=Q

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=21) AC  fan mass flow control'
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
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP15C'

      RETURN
      END

C ******************** CMP40C ********************

C CMP40C generates for plant component IPCOMP with plant db code 400 ie.
C 3 node (ISV>19) AC  counterflow cooling coil fed by WCH system
C   node 1 represents solid materials
C   node 2 represents air
C   node 3 represents water
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C            3 UA modulus (W/K)
C            4 Mass of encapsulated water (kg)
C     BDATA: 1 Coil outside (air side) heat transfer area (m^2)
C            2 Coil inside (water side) heat transfer area (m^2)
C            3 Coil face area (m^2)
C            4 Metal thermal resistance (m^2K/W)
C            5 Internal tube diameter (m)
C     CDATA: none

C     PCDATF/P
C            1 Water vapour rate extracted from moist air (kg/s)
C            2 Air/solid   inter-node thermal conductance (W/K)
C            3 Water/solid inter-node thermal conductance (W/K)

      SUBROUTINE CMP40C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SHOUT/ICOUT
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

      COMMON/ATPRES/PATMOS
      COMMON/WBULBO/IOPT

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      character outs*124
      logical closea,closeb,closec, CloseD

      PI = 4.0 * ATAN(1.0)

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP40C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,2,1)
      ICON2=ICONDX(IPCOMP,3,1)
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1
      INOD3=NPCDAT(IPCOMP,9)+2

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C First initialize UA modulus (for calculation of containment heat loss)
         UA=ADATA(IPCOMP,3)
         call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
         IF(closea) UA=0.

C Calculate extracted heat Q using single zone coil model and the
C bypass factor method, unless water or air mass flow rate is zero.
C First mark air and water 1st phase mass flow rate for iteration
         ICSV(INOD2,2)=1
         CSVI(INOD2,2)=CSVF(INOD2,2)
         ICSV(INOD3,2)=1
         CSVI(INOD3,2)=CSVF(INOD3,2)
         call eclose(CONVAR(ICON1,2),0.00,0.0001,closeb)
         call eclose(CONVAR(ICON2,2),0.00,0.0001,closec)
         IF(closeb.or.closec) THEN
            QA=0.
            RA=0.10
            RM=BDATA(IPCOMP,4)
            RF=BDATA(IPCOMP,5)/(2.*0.58)
         ELSE

C Start with establishing air VA and water VW velocity, and air TA and
C water temperature TW. Then determine air RA, metal RM, and
C water RF thermal resistance
            TA=CONVAR(ICON1,1)
            TW=CONVAR(ICON2,1)

C Mark current air and water temperature for iteration
            ICSV(INOD2,1)=1
            CSVI(INOD2,1)=CSVF(INOD2,1)
            ICSV(INOD3,1)=1
            CSVI(INOD3,1)=CSVF(INOD3,1)

C Issue non-fatal warning if cooling coil in heating mode
            IF(TW.GT.TA) THEN
               CALL DAYCLK(IDYP,PTIMEF,ICOUT)
               WRITE(outs,*) ' CMP40C: component ',IPCOMP,' will heat',
     &                        ' air, based on cooling coil model !'
               call edisp(iuout,outs)
            END IF
            AMFR=PCONDR(ICON1)*CONVAR(ICON1,2)
            WMFR=PCONDR(ICON2)*CONVAR(ICON2,2)
            VA=AMFR/(BDATA(IPCOMP,3)*RHOFLD(1,TA))
            VW=WMFR/(RHOFLD(3,TW)*BDATA(IPCOMP,5)**2*PI/4.)
            RA=1./(38.*VA)
            RM=BDATA(IPCOMP,4)
            RF=BDATA(IPCOMP,1)/
     &         (BDATA(IPCOMP,2)
     &          *1400.*(1.+0.015*TW)*VW**0.8/BDATA(IPCOMP,5)**0.2)

C Calculate inlet moisture content.
            gs=convar(icon1,3)/convar(icon1,2)

C Call cooling coil subroutine to calculate 
C heat transfer, note 'qa' already negated.
            call ccoilq(bdata(ipcomp,1),bdata(ipcomp,3),
     &                     ra,rm,rf,ta,tw,amfr,wmfr,gs,qa,cmfr)
         endif

C Establish heat capacity of component mass CM (J/K), and
C heat capacity of encapsulated water CW (J/K), and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)
         CW=ADATA(IPCOMP,4)*SHTFLD(3,CONVAR(ICON2,1))
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(1,CONVAR(ICON1,1))+
     &      PCONDR(ICON1)*CONVAR(ICON1,3)*SHTFLD(2,CONVAR(ICON1,1))
         C2=PCONDR(ICON2)*CONVAR(ICON2,2)*SHTFLD(3,CONVAR(ICON2,1))

C Inter-node conductances PCDATF(IPCOMP,2) and PCDATF(IPCOMP,3) (W/K)
         PCDATF(IPCOMP,2)=BDATA(IPCOMP,1)/(RA+RM/2)
         PCDATF(IPCOMP,3)=BDATA(IPCOMP,1)/(RF+RM/2)

C Calculate future time-row value of rate of condensation based
C on most recent vapour mass balance information
         PCDATF(IPCOMP,1)=PCONDR(ICON1)*CONVAR(ICON1,3)-CSVF(INOD2,3)

C Mark 2nd phase mass flow rate for iteration
         ICSV(INOD2,3)=1
         CSVI(INOD2,3)=CSVF(INOD2,3)

C Establish condensation heat of water; assume most recent node temp.
         RW=CNDWAT(CSVF(INOD2,1))

C Calculate 'future time-row' heat transfer to water QW
         QW=-QA-PCDATF(IPCOMP,1)*RW

C Calculate current component time-constant TC (max. of solid and water)
         TC(IPCOMP)=AMAX1(
     &        CM/AMAX1(SMALL,(PCDATF(IPCOMP,2)+PCDATF(IPCOMP,3)+UA)),
     &        CW/AMAX1(SMALL,(PCDATF(IPCOMP,3)+C2)))

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
C  node 1 (solids)
         COUT(1)=ALPHA*(-PCDATF(IPCOMP,2)-PCDATF(IPCOMP,3)-UA)-CM/TIMSEC
         COUT(2)=ALPHA*PCDATF(IPCOMP,2)
         COUT(3)=ALPHA*PCDATF(IPCOMP,3)

C  node 2 (air)
         COUT(4)=PCDATF(IPCOMP,2)
         COUT(5)=-C1-PCDATF(IPCOMP,2)

C  node 3 (water)
         COUT(6)=ALPHA*PCDATF(IPCOMP,3)
         COUT(7)=ALPHA*(-PCDATF(IPCOMP,3)-C2)-CW/TIMSEC

C then matrix equation cross-coupling coefficients,
         COUT(8)=C1
         COUT(9)=ALPHA*C2

C and then present-time coefficients (ie. right hand sides)
         COUT(10)=((1.-ALPHA)
     &             *(PCDATP(IPCOMP,2)+PCDATP(IPCOMP,3)+PUAP(INOD1))
     &              -CM/TIMSEC)*CSVP(INOD1,1)
     &             +(1.-ALPHA)*(-PCDATP(IPCOMP,2))*CSVP(INOD2,1)
     &             +(1.-ALPHA)*(-PCDATP(IPCOMP,3))*CSVP(INOD3,1)
     &             -ALPHA*UA*PCNTMF(IPCOMP)
     &             -(1.-ALPHA)*PUAP(INOD1)*PCNTMP(IPCOMP)
         COUT(11)=-QA-PCDATF(IPCOMP,1)*RW
         COUT(12)=(1.-ALPHA)*(-PCDATP(IPCOMP,3))*CSVP(INOD1,1)
     &            +((1.-ALPHA)*(PCRP(ICON2)+PCDATP(IPCOMP,3))
     &              -CW/TIMSEC)*CSVP(INOD3,1)
     &            +(1.-ALPHA)*(-PCRP(ICON2))*PCTP(ICON2)
     &            -ALPHA*QW-(1.-ALPHA)*PCQP(INOD3)

C Store "environment" variables future values
         PUAF(INOD1)=UA
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCTF(ICON2)=CONVAR(ICON2,1)
         PCRF(ICON1)=C1
         PCRF(ICON2)=C2
         PCQF(INOD2)=QA
         PCQF(INOD3)=QW

C 1st phase mass (ie. dry air) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=0.
         COUT(4)=0.
         COUT(5)=1.
         COUT(6)=0.
         COUT(7)=1.
         COUT(8)=-PCONDR(ICON1)
         COUT(9)=-PCONDR(ICON2)
         COUT(10)=0.
         COUT(11)=0.
         COUT(12)=0.

C 2nd phase mass (ie. vapour) balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=0.
         COUT(4)=0.
         COUT(5)=1.
         COUT(6)=0.
         COUT(7)=1.
         COUT(8)=-PCONDR(ICON1)
         COUT(9)=0.
         COUT(10)=0.
C Set COUT(11) to zero for the case where airflow = 0.
         COUT(11)=0.
         COUT(12)=0.



C Changes made to ensure that DEWPT is only called when the air
C flow rate is not equal to zero. The case of a water flow rate 
C (CONVAR(ICON1,3) not equal to zero and an air flow rate
C (CONVAR(ICON1,2) equal to zero resulted in a large value
C being passed to DEWPT. This leads to an endless loop in DEWPT
C as convergence is attempted but never reached.

C Check if airflow rate is zero
         CALL ECLOSE(CONVAR(ICON1,2),0.0,0.001,CloseD)

C If airflow rate is not zero, perform dewpoint check otherwise,
C do not.

         IF  (.NOT.CloseD) THEN       

C Check whether air temperature is below dew point TDEW, if so
C this component extracts water vapour from moist air stream
C First establish TDEW; assume present moisture content
            TDEW=DEWPT(CONVAR(ICON1,3)/AMAX1(SMALL,CONVAR(ICON1,2)),
     &           PATMOS)

C Now establish air node coefficients, first for Tair >= Tdew; as this
C is based on future temperature, mark air temperature for iteration
            ICSV(INOD2,1)=1
            CSVI(INOD2,1)=CSVF(INOD2,1)
            IF(CSVF(INOD2,1).GE.TDEW) THEN
               COUT(8)=-PCONDR(ICON1)
               COUT(11)=0.
            ELSE

C Then for Tair < Tdew in which case we have to know the maximum
C vapour mass flow rate VMAX = (Xmax * air mass flow) at Tair
               VMAX=HUMRT1(CSVF(INOD2,1),100.,PATMOS,IOPT)*
     &           PCONDR(ICON1)*CONVAR(ICON1,2)
               COUT(8)=0.
               COUT(11)=VMAX
            END IF

         ENDIF

      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 3 node (ISV>19) AC  counterflow cooling coil',
     &                ' fed by WCH system'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2,INOD3
         WRITE(ITU,*) ' Connection(s)  ',ICON1,ICON2
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' CW     = ',CW,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' C2     = ',C2,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' UA     = ',UA,' (W/K)'

            WRITE(ITU,*) ' RA     = ',RA,' (m^2.K/W)'
            WRITE(ITU,*) ' RM     = ',RM,' (m^2.K/W)'
            WRITE(ITU,*) ' RF     = ',RF,' (m^2.K/W)'
            WRITE(ITU,*) ' AT     = ',bdata(ipcomp,1),' (m^2)'
            WRITE(ITU,*) ' AF     = ',bdata(ipcomp,3),' (m^2)'
            WRITE(ITU,*) ' AMFR   = ',AMFR,' (kg/s)'
            WRITE(ITU,*) ' WMFR   = ',WMFR,' (kg/s)'
            WRITE(ITU,*) ' VA     = ',VA,' (m/s)'
            WRITE(ITU,*) ' VW     = ',VW,' (m/s)'
            WRITE(ITU,*) ' TA     = ',TA,' (C)'
            WRITE(ITU,*) ' TW     = ',TW,' (C)'
            WRITE(ITU,*) ' GS     = ',GS,' (kg/kg.da)'
            WRITE(ITU,*) ' QA     = ',QA,' (W)'
            WRITE(ITU,*) ' QW     = ',QW,' (W)'
            WRITE(ITU,*) ' CMFR   = ',CMFR,' (kg/s)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
         ELSE IF(ISTATS.EQ.3) THEN
            WRITE(ITU,*) ' Tcur   = ',CSVF(INOD1,1),' (C)'
            WRITE(ITU,*) ' Tdew   = ',TDEW,' (C)'
            IF(CSVF(INOD2,1).LT.TDEW)
     &         WRITE(ITU,*) ' VPmax  = ',VMAX,' (kg,v/s)'
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
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP40C'


      RETURN
      END

C ******************** CMP41C ********************

C CMP41C generates for plant component IPCOMP with plant db code 410 ie.
C 3 node (ISV>19) AC  counterflow heating coil fed by WCH system
C   node 1 represents solid materials
C   node 2 represents air
C   node 3 represents water
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C            3 UA modulus (W/K)
C            4 Mass of encapsulated water (kg)
C     BDATA: 1 Coil outside (air side) heat transfer area (m^2)
C            2 Coil inside (water side) heat transfer area (m^2)
C            3 Coil face area (m^2)
C            4 Metal thermal resistance (m^2K/W)
C            5 Internal tube diameter (m)
C     CDATA: none

C     PCDATF/P
C            1 Air/solid   inter-node thermal conductance (W/K)
C            2 Water/solid inter-node thermal conductance (W/K)

      SUBROUTINE CMP41C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SHOUT/ICOUT
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

      COMMON/WRNOFF/IWMOFF

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      character outs*124
      logical closea,closeb,closec

      PI = 4.0 * ATAN(1.0)

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP41C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,2,1)
      ICON2=ICONDX(IPCOMP,3,1)
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1
      INOD3=NPCDAT(IPCOMP,9)+2

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C First initialize UA modulus (for calculation of containment heat loss)
         UA=ADATA(IPCOMP,3)
         call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
         IF(closea) UA=0.

C Calculate transfered heat Q using single zone coil model,
C unless water or air mass flow rate is zero.
C First mark air and water 1st phase mass flow rate for iteration
         ICSV(INOD2,2)=1
         CSVI(INOD2,2)=CSVF(INOD2,2)
         ICSV(INOD3,2)=1
         CSVI(INOD3,2)=CSVF(INOD3,2)
         call eclose(CONVAR(ICON1,2),0.00,0.0001,closeb)
         call eclose(CONVAR(ICON2,2),0.00,0.0001,closec)
         IF(closeb.or.closec) THEN
            QA=0.
            RA=0.10
            RM=BDATA(IPCOMP,4)
            RF=BDATA(IPCOMP,5)/(2.*0.58)
         ELSE

C Start with establishing air VA and water VW velocity, and air TA and
C water temperature TW. Then determine air RA, metal RM, and
C water RF thermal resistance
            TA=CONVAR(ICON1,1)
            TW=CONVAR(ICON2,1)

C Mark current air and water temperature for iteration
            ICSV(INOD2,1)=1
            CSVI(INOD2,1)=CSVF(INOD2,1)
            ICSV(INOD3,1)=1
            CSVI(INOD3,1)=CSVF(INOD3,1)

C Issue non-fatal warning if coil in cooling mode
            IF(TW.LT.TA.and.iwmoff.eq.0) THEN
               CALL DAYCLK(IDYP,PTIMEF,ICOUT)
               WRITE(outs,*) ' CMP41C: component ',IPCOMP,' will cool',
     &                        ' air, based on heating coil model !!'
               call edisp(iuout,outs)
            END IF
            AMFR=PCONDR(ICON1)*CONVAR(ICON1,2)
            WMFR=PCONDR(ICON2)*CONVAR(ICON2,2)
            VA=AMFR/(BDATA(IPCOMP,3)*RHOFLD(1,TA))
            VW=WMFR/(RHOFLD(3,TW)*BDATA(IPCOMP,5)**2*PI/4.)
            RA=1./(38.*VA)
            RM=BDATA(IPCOMP,4)
            RF=BDATA(IPCOMP,1)/
     &         (BDATA(IPCOMP,2)
     &          *1400.*(1.+0.015*TW)*VW**0.8/BDATA(IPCOMP,5)**0.2)

C Calculate heat flow capacities CAIR and CWAT (W/K)
            CAIR=AMFR*SHTFLD(1,TA)+
     &           PCONDR(ICON1)*CONVAR(ICON1,3)*SHTFLD(2,TA)
            CWAT=WMFR*SHTFLD(3,TW)

C Establish CMIN and CMAX (W/K)
            CMIN=AMIN1(CAIR,CWAT)
            CMAX=AMAX1(CAIR,CWAT)

C Establish capacity ratio CRAT, RNTU, and EFF-ectiveness
            CRAT=CMIN/CMAX
            RNTU=BDATA(IPCOMP,1)/(CMIN*(RA+RM+RF))
            EFF=(1.-EXP(-RNTU*(1.-CRAT)))/(1.-CRAT*EXP(-RNTU*(1.-CRAT)))

C And finally calcultate heat transferred to air stream QA
            QA=CMIN*EFF*(TW-TA)
         END IF

C Establish heat capacity of component mass CM (J/K), and
C heat capacity of encapsulated water CW (J/K), and
C fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)
         CW=ADATA(IPCOMP,4)*SHTFLD(3,CONVAR(ICON2,1))
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(1,CONVAR(ICON1,1))+
     &      PCONDR(ICON1)*CONVAR(ICON1,3)*SHTFLD(2,CONVAR(ICON1,1))
         C2=PCONDR(ICON2)*CONVAR(ICON2,2)*SHTFLD(3,CONVAR(ICON2,1))

C Inter-node conductances PCDATF(IPCOMP,1) and PCDATF(IPCOMP,2) (W/K)
         PCDATF(IPCOMP,1)=BDATA(IPCOMP,1)/(RA+RM/2)
         PCDATF(IPCOMP,2)=BDATA(IPCOMP,1)/(RF+RM/2)

C Calculate 'future time-row' heat extracted from water QW
         QW=-QA

C Calculate current component time-constant TC (max. of solid and water)
         TC(IPCOMP)=AMAX1(
     &        CM/AMAX1(SMALL,(PCDATF(IPCOMP,1)+PCDATF(IPCOMP,2)+UA)),
     &        CW/AMAX1(SMALL,(PCDATF(IPCOMP,2)+C2)))

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
C  node 1 (solids)
         COUT(1)=ALPHA*(-PCDATF(IPCOMP,1)-PCDATF(IPCOMP,2)-UA)-CM/TIMSEC
         COUT(2)=ALPHA*PCDATF(IPCOMP,1)
         COUT(3)=ALPHA*PCDATF(IPCOMP,2)

C  node 2 (air)
         COUT(4)=PCDATF(IPCOMP,1)
         COUT(5)=-C1-PCDATF(IPCOMP,1)

C  node 3 (water)
         COUT(6)=ALPHA*PCDATF(IPCOMP,2)
         COUT(7)=ALPHA*(-PCDATF(IPCOMP,2)-C2)-CW/TIMSEC

C then matrix equation cross-coupling coefficients,
         COUT(8)=C1
         COUT(9)=ALPHA*C2

C and then present-time coefficients (ie. right hand sides)
         COUT(10)=((1.-ALPHA)
     &             *(PCDATP(IPCOMP,1)+PCDATP(IPCOMP,2)+PUAP(INOD1))
     &              -CM/TIMSEC)*CSVP(INOD1,1)
     &             +(1.-ALPHA)*(-PCDATP(IPCOMP,1))*CSVP(INOD2,1)
     &             +(1.-ALPHA)*(-PCDATP(IPCOMP,2))*CSVP(INOD3,1)
     &             -ALPHA*UA*PCNTMF(IPCOMP)
     &             -(1.-ALPHA)*PUAP(INOD1)*PCNTMP(IPCOMP)
         COUT(11)=-QA
         COUT(12)=(1.-ALPHA)*(-PCDATP(IPCOMP,2))*CSVP(INOD1,1)
     &            +((1.-ALPHA)*(PCRP(ICON2)+PCDATP(IPCOMP,2))
     &              -CW/TIMSEC)*CSVP(INOD3,1)
     &            +(1.-ALPHA)*(-PCRP(ICON2))*PCTP(ICON2)
     &            -ALPHA*QW-(1.-ALPHA)*PCQP(INOD3)

C Store "environment" variables future values
         PUAF(INOD1)=UA
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCTF(ICON2)=CONVAR(ICON2,1)
         PCRF(ICON1)=C1
         PCRF(ICON2)=C2
         PCQF(INOD2)=QA
         PCQF(INOD3)=QW

C Store the heat addition to the air stream for use with building control
C law 6 (applied by subroutine BCL06) and connection type (variable ISIND)
C 2.  With this the plant-component calculated heat exchange is injected
C to the building zone. 
         QDATA(IPCOMP) = QA

C 1st phase mass (ie. dry air) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=0.
         COUT(4)=0.
         COUT(5)=1.
         COUT(6)=0.
         COUT(7)=1.
         COUT(8)=-PCONDR(ICON1)
         COUT(9)=-PCONDR(ICON2)
         COUT(10)=0.
         COUT(11)=0.
         COUT(12)=0.

C 2nd phase mass (ie. vapour) balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=0.
         COUT(4)=0.
         COUT(5)=1.
         COUT(6)=0.
         COUT(7)=1.
         COUT(8)=-PCONDR(ICON1)
         COUT(9)=0.
         COUT(10)=0.
         COUT(11)=0.
         COUT(12)=0.
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 3 node (ISV>19) AC  counterflow heating coil',
     &                ' fed by WCH system'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2,INOD3
         WRITE(ITU,*) ' Connection(s)  ',ICON1,ICON2
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' C2     = ',C2,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' UA     = ',UA,' (W/K)'
            WRITE(ITU,*) ' TA     = ',TA,' (C)'
            WRITE(ITU,*) ' TW     = ',TW,' (C)'
            WRITE(ITU,*) ' VA     = ',VA,' (m/s)'
            WRITE(ITU,*) ' VW     = ',VW,' (m/s)'
            WRITE(ITU,*) ' EFF    = ',EFF,' (-)'
            WRITE(ITU,*) ' QA     = ',QA,' (W)'
            WRITE(ITU,*) ' QW     = ',QW,' (W)'
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
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP41C'

      RETURN
      END

C ******************** CCOILQ ********************
C This subroutine finds the amount of heat extracted 
C by a cooling coil using the bypass factor method.
C The parameters passed have the following meaning:
C   at    : Total coil area (m^2)
C   af    : Coil face area  (m^2)
C   ra    : Air thermal resistance (m^2 K/W)
C   rm    : Metal thermal resistance (m^2 K/W)
C   rw    : Water therma resistance (m^2 K/W)
C   tai   : Air inlet temperature (C)
C   wit   : Water inlet temperature (C)
C   amfr  : Air mass flow rate (kg/s)
C   wmfr  : water mass flow rate (kg/s)
C   gi    : moisture content at inlet (kg(water)/kg(air))
C The returned parameters have the following meaning:
C   q     : Heat extracted from air (W)
C   cmfr  : Condensate mass flow rate (kg/s)

      subroutine ccoilq(at,af,ra,rm,rw,tai,wit,amfr,wmfr,gi,q,cmfr)

C Remember effectivness and specific heat ratio.
      logical closea
      data effp/0.0/,shrp/0.0/


C Initialise q
      Q=0.

C Establish inlet enthalpy HAI, air specific heat CPA 
C and water specific heat ratio 'shtw'.
      IF(AMFR.LE.0.0)GOTO 4
      HAI=enthp2(tai,gi)*1000.
      CPA=SHTFLD(1,tai)
      shtw=SHTFLD(3,wit)

C Solve for coil heat extraction.
    3 IF(WMFR.LE.0.0.OR.AMFR.LE.0.0)GOTO 4

C Coil by-pass factor.
      BPF=EXP((-AT)/(CPA*AMFR*RA))

C Assume effectiveness and SHR values then set up iteration loop.
      ITER1=0
      EFF=EFFP
      SHR=SHRP
      IF(EFF.LE.0.0)EFF=0.70
      IF(SHR.LE.0.0)SHR=0.8
      EFFA=0.0
      EFFC=0.0

C Establish Cmax, Cmin and Cratio.
   14 ITER2=0
    9 C1=AMFR*CPA/SHR
      C2=WMFR*SHTW
      CMIN=AMIN1(C1,C2)
      CMAX=AMAX1(C1,C2)

C Determine Cratio.
      CRATIO=CMIN/CMAX

C Determine coil 'U' and NTU values.
      U=1.0/((RA*SHR)+RW+RM)
      HTU=AT*U/CMIN

C Calculate effectiveness for this value of SHR then test
C if SHR requires recalculation.
      EFFX=HTU/(1.0+HTU)
      IF(EFFX.GT.0.9975)GOTO 5
      IF(CRATIO.LT.0.999)GOTO 6
    5 EFF1=EFFX
      GOTO 7
    6 E=(-HTU)/(1.0-CRATIO)
      IF(E.LT.-8.0)E=-8.0
      E=EXP(E)
      EFFX=(1.0-E)/(1.0-(CRATIO*E))
      IF(EFF1.LT.EFFX)EFF1=EFFX
    7 IF(ITER1.GT.0)GOTO 8
      DIFF=ABS(EFF-EFF1)
      IF(DIFF.LT.0.0001)GOTO 8
      IF(ITER2.GT.100)GOTO 8
      ITER2=ITER2+1
      SHR=SHR-(1.0-(EFF1/EFF))*3.0
      IF(SHR.GT.0.0.AND.SHR.LT.1.0)GOTO 9
      IF(SHR.GE.1.0)SHR=1.0
      IF(SHR.LE.0.0)SHR=0.0

C Effectiveness is satisfactory and SHR established.
    8 EFF=EFF1

C Determine coil heat transfer Q (value is positive but negated late).
      Q=EFF*CMIN*(TAI-WIT)

C Calculate HAO from Q and BPF.
      HAO=(HAI-(Q/AMFR))

C Determine Hsat of air.
      HSAT=(HAO-BPF*HAI)/(1.0-BPF)

C Determine GSAT and TSAT.
      patmos=1013.25
      TSAT=TSATH0(HSAT/1000.0,PATMOS)
      GSAT=HUMRAT(TSAT,TSAT,PATMOS,1)
      IF(GSAT.GT.GI)GSAT=GI

C Establish Gout and Tao.
      GOUT=BPF*(GI-GSAT)+GSAT
      IF(GOUT.GT.GI)GOUT=GI
      TAO=TDB(HAO/1000.0,GOUT)
      PRH=PCRH2(TAO,GOUT,PATMOS)
      IF(PRH.LT.100.)GOTO 11
      RH=99.9
      GOUT=HUMRT1(TAO,RH,PATMOS,IOPT)
   11 CONTINUE

C SHR is as follows.
      IF(ABS(HAI-HAO).GT.0.001)GOTO 111
      SHR2=1.0
      GOTO 112
  111 SHR2=((TAI-TAO)*CPA)/(HAI-HAO)
  112 call eclose(GOUT,GI,0.00001,closea)
      IF(closea)SHR2=1.0
      IF(SHR2.GT.1.0)SHR2=1.0
      IF(SHR2.LT.-1.0)SHR2=-1.0

C Test SHR.
      DIFF=ABS(SHR-SHR2)
      IF(ABS(DIFF).LT.0.0001)GOTO 12
      IF(ITER1.GT.100)GOTO 12

C Prepare iteration of effectiveness.
      ITER1=ITER1+1
      EFF=EFF-(1.0-SHR/SHR2)/10.0
      IF(EFF.GT.EFFC.AND.EFF.LT.EFFA)GOTO 13
      EFF=EFFC
      IF(CRATIO.LT.0.2)EFF=EFFA
   13 SHR=SHR2
      GOTO 14

C Results.
   12 SHRP=SHR
      EFFP=EFF

C Coil condensate.
      cmfr=(GI-GOUT)*AMFR

C Water outlet temperature.
      CCTWO=WIT+(Q/(WMFR*SHTW))

    4 Q=-Q
      RETURN
      END

C ********* NTUCT **************
C Subroutine NTUCT Calculates the available transfer units for a 
C cooling tower.
C Simpson's rule calculation starts here to find NTUr at current
C exit temp.
C Calculate the temperature step and enthalpy step for Simpson's
C calculation.
      SUBROUTINE NTUCT(TWSU,TWEXI,HEX,HSU,NTUR)
          REAL NTUr 
          DIMENSION COEFF(8)          
          PATMOS=1013.25
          SMALL=1.0E-20

          TSTEP=(TWSU-TWEXI)/8.0
          ESTEP=(HEX-HSU)/8.0

C Calculate the 0th coefficients.
          H=HSU
          GH0=HUMR(TWEXI,100.,PATMOS)
          HSAT0=ENTHP2(TWEXI,GH0)
          IF((HSAT0-H).LE.0.0) THEN 
             COEFF0=0.0
          ELSE          
             COEFF0=1./(HSAT0-H)
          ENDIF

C Calculate the NTUr using numerical integration in 8 steps. 9 co-ordinates
         DO 10 i=1,8

C Calculate the coefficients for the equation HO & H. H - air enthalpy
C HO - saturated air enthalpy at temp T.
           T=TWEXI+i*TSTEP
           H=HSU+i*ESTEP

           GHSAT=HUMR(T,100.,PATMOS) 
           HSAT=ENTHP2(T,GHSAT)

          IF((HSAT-H).LE.0.0) THEN 
             COEFF(I)=0.0
          ELSE          
             COEFF(I)=1./(HSAT-H)
          ENDIF

 10       CONTINUE 


C Evaluate NTUr using simpson's rule.
         NTUr=((HEX-HSU)/(3.*8.))*
     &   (COEFF0+4*COEFF(1)+2*COEFF(2)+4*COEFF(3)+2*COEFF(4)
     &    +4*COEFF(5)+2*COEFF(6)+4*COEFF(7)+COEFF(8))
 
      return
      end
