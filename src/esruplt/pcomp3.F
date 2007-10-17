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
C PCOMP3 of ESPbps

c This file contains the following component subroutines:
c  cmp30c   1 node (isv=20) Water cooler.
c  cmp43c   2 node (isv>20) cooling coil.
c  cmp67c   3 node heat transfer tube.
c  cmp70c   1 node (isv=20) Solar flat plate collector.
c  cmp71c   1 node (isv=20) Solar collector segment.
c  cmp72c   2 node (isv=20) Water storage tank layer.
c  cmp92c   1 node (isv=21) fictitious boundary component.
c  cmp93c  10 nodes (isv=21) Heat exchanger.
c  cmp94c   3 node (isv=20) Heat exchanger segment.
c  cmp98c   1 node (isv=20) CHP engine component.
c  cmp99c   3 node (isv=20) CHP engine component.
c ******************** CMP30C ********************

      subroutine cmp30c(ipcomp,cout,istats)
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
     &   ITRACE(37).ne.0) write(itu,*) ' Entering subroutine CMP30C'

c Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      icon1=icondx(ipcomp,1,1)
      inod1=npcdat(ipcomp,9)

c Generate coefficients for energy balance equation
      if(istats.eq.1) then

c First initialize UA modulus (for calculation of containment heat loss)
         ua=adata(ipcomp,3)
         call eclose(pcntmf(ipcomp),-99.00,0.001,closea)
         if(closea) ua=0.

c Then initialize extracted heat q
         q=cdata(ipcomp,1)

c Establish heat capacity of component mass CM (J/K) and
c fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         cm=adata(ipcomp,1)*adata(ipcomp,2)
         c1=pcondr(icon1)*convar(icon1,2)*shtfld(3,convar(icon1,1))

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
     &             -alpha*q-(1.-alpha)*pcqp(inod1)

c Store "environment" variables future values
         puaf(inod1)=ua
         pctf(icon1)=convar(icon1,1)
         pcrf(icon1)=c1
         pcqf(inod1)=q

c 1th phase mass (ie. water) balance coefficients
      else if(istats.eq.2) then
         cout(1)=1.
         cout(2)=-pcondr(icon1)
         cout(3)=0.

c 2nd phase mass balance coefficients
      else if(istats.eq.3) then
         cout(1)=1.
         cout(2)=0.
         cout(3)=0.
      end if

c Trace output
      if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   itrace(37).ne.0) then
         write(itu,*) ' Component      ',ipcomp,':'
         write(itu,*) ' 1 node (ISV=20) Water cooler'
         write(itu,*) ' Matrix node(s) ',inod1
         write(itu,*) ' Connection(s)  ',icon1
         if(istats.eq.1) then
            write(itu,*) ' CM     = ',cm,' (J/K)'
            write(itu,*) ' C1     = ',c1,' (W/K)'
            write(itu,*) ' TC     = ',tc,' (s)'
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
     &   ITRACE(37).ne.0) write(itu,*) ' Leaving subroutine CMP30C'

C Return to calling module
      return
      end

c ******************** CMP43C ********************
c CMP43C generates for plant component IPCOMP with plant db code
c 430 ie. 2 node (ISV>20) cooling coil model,
c matrix equation coefficients COUT (in order: self-coupling, cross-
c coupling, and present-time coefficients) for energy balance (ISTATS=1),
c 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3).
c    ADATA : 1-Total mass of solid material (kg)
c    ADATA : 2-Mass weighted average specific heat (J/kgK)
c    ADATA : 3-UA modulus (W/K)
c    BDATA : 1-Number of rows (-)
c    BDATA : 2-Number of fins per metre (-)
c    BDATA : 3-Fin thickness (m)
c    BDATA : 4-Fin efficiency (-)
c    BDATA : 5-Thermal conductivity of tube material (W/mK)
c    BDATA : 6-Tube spacing (m)
c    BDATA : 7-Tube inside diameter (m)
c    BDATA : 8-Tube outside diameter (m)
c    BDATA : 9-Coil face width (m)
c    BDATA :10-Coil face height (m)
c    CDATA : None.
c
C    PCDATF/P
C            1 Inter-node fluid heat capacity rate (W/K)
C            2 condensate flow rate (kg/s)

      subroutine cmp43c(ipcomp,cout,istats)
#include "plant.h"
#include "building.h"

      common/outin/iuout,iuin
      common/tc/itc,icnt
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/pctime/timsec
      common/pctc/tc(mpcom)

      common/pcequ/impexp,ratimp
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

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
      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

      parameter (small=1.0e-20)
      real      cout(mpcoe)
      logical closea

      PI = 4.0 * ATAN(1.0)

c Trace output
      if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   ITRACE(37).ne.0) write(itu,*) ' Entering subroutine CMP43C'

c Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      icon1=icondx(ipcomp,1,1)
      icon2=icondx(ipcomp,2,1)
      inod1=npcdat(ipcomp,9)
      inod2=npcdat(ipcomp,9)+1

c Generate coefficients for energy balance equation
      if(istats.eq.1) then

C Evaluate the current environment temperature
         ua=adata(ipcomp,4)
         call eclose(pcntmf(ipcomp),-99.00,0.001,closea)
         if(closea) ua=0.

C Mark air and water temperature for iteration
         icsv(inod1,1)=1
         csvi(inod1,1)=csvf(inod1,1)
         icsv(inod2,1)=1
         csvi(inod2,1)=csvf(inod2,1)
         icsv(inod1,3)=1
         csvi(inod1,3)=csvf(inod1,3)

c Establish air and water mass flow rates
         amfr=pcondr(icon1)*convar(icon1,2)
         wmfr=pcondr(icon2)*convar(icon2,2)

C Establish air and water specific heats.
         cpa=shtfld(1,convar(icon1,1))
         cpw=shtfld(3,convar(icon2,1))

C Calculate number of fins.
         nfins=int(bdata(ipcomp,2)*bdata(ipcomp,9))

C Calculate fin length in direction of airflow
         flngth=bdata(ipcomp,1)*bdata(ipcomp,6)

C Calculate gross surface area for one fin
         gfsa=(bdata(ipcomp,10)*bdata(ipcomp,3)+
     &         bdata(ipcomp,3)*flngth+bdata(ipcomp,10)*flngth)*2.

C Calculate number of tubes in one row
         ntubes=int(bdata(ipcomp,10)/bdata(ipcomp,6))

C Calculate total number of tubes
         tntubs=float(ntubes)*bdata(ipcomp,1)

C Calculate external cross sectional area of one tube
         a1tube=pi*bdata(ipcomp,8)**2/4.

C Calculate net surface area of one fin
         sa1fin=gfsa-a1tube*tntubs*2.

C and for all fins.
         safins=float(nfins)*sa1fin

C Calculate gross external surface area of all tubes
         gtesa=tntubs*pi*bdata(ipcomp,8)*bdata(ipcomp,9)

C Calculate net surface area of all tubes.
         tanet=gtesa-tntubs*bdata(ipcomp,3)*float(nfins)*
     &               pi*bdata(ipcomp,8)

C and total surface area of coil.
         tota=safins+tanet

C calculate internal tube surface area
         saitub=pi*bdata(ipcomp,7)*tntubs*bdata(ipcomp,9)

C ratio of tota/saitub 
         atai=tota/saitub

C Calculate coil face flow area
         ffa=bdata(ipcomp,10)*(bdata(ipcomp,9)-
     &       float(nfins)*bdata(ipcomp,3))-
     &       tanet/(2.*bdata(ipcomp,1))

C Calculate face velocity entering coil for a staggered
C tube arrangement.
         vfa=amfr/(ffa*rhofld(1,convar(icon1,1)))
         raa=1./(27.42*vfa**0.8)

C Calculate specific heat ratio based on most recent
C values of state variables.
         gi=convar(icon1,3)/convar(icon1,2)
         go=csvf(inod1,3)/csvf(inod1,2)
         hih=enthp2(convar(icon1,1),gi)*1000.
         hoc=enthp2(csvf(inod1,1),go)*1000.
         hoh=enthp2(convar(icon1,1),go)*1000.
         call eclose((hih-hoc),0.00,0.0001,closea)
         if(.NOT.closea) then
            shr=(hoh-hoc)/(hih-hoc)
         else
            shr=1.0
         endif

C modify air thermal resistance based on shr
         raa=raa*shr

C Calculate air thermal resistance in (K/W)
         ra=raa/tota

C Calculate effectiveness of fin surface
         seff=(bdata(ipcomp,4)*safins+tanet)/tota

C Calculate fin thermal resistance
         fres=(1.-seff)/seff*raa/safins

C Calculate thermal resistance of tube metal
         tres=atai*bdata(ipcomp,8)/(2*bdata(ipcomp,5))*
     &        log(bdata(ipcomp,8)/bdata(ipcomp,7))

C Now find metal thermal resistance in (K/W)
         rm=fres+tres/tanet

C Calculate water velocity (m/s)
         vfw=wmfr/(float(ntubes)*pi*bdata(ipcomp,7)**2/4.*
     &       rhofld(3,convar(icon2,1)))

C Calculate water thermal resistance referred to the internal 
C tube surface (m^2K/W)
         rw=bdata(ipcomp,7)**0.2/((1429.+20.9*convar(icon2,1))*
     &            vfw**0.8)

C and convert to (K/W)
         rw=rw/saitub

C Calculate water mass in coil.
         wmass=wmfr*bdata(ipcomp,1)*bdata(ipcomp,9)/vfw

C Calculate thermal resistance of metal and water.
          rmw=rm+rw

C Establish fluid heat capacity rates (W/K)
         c1=amfr*cpa+pcondr(icon1)*convar(icon1,3)*
     &      shtfld(2,convar(icon1,1))
         c2=wmfr*cpw

C Calculate inter-node conductance (W/K)
         pcdatf(ipcomp,1)=1./rmw

C Calculate rate of condensation (kg/s) based on
C most recent value of second phase flow rate.
         pcdatf(ipcomp,2)=pcondr(icon1)*convar(icon1,3)-csvf(inod1,3)

C calculate thermal resistances ratio
         r4=1.0/c1
         thr=(r4+ra)/r4

C Calculate metal and water thermal mass.
         cm=adata(ipcomp,1)*adata(ipcomp,2)
         cw=wmass*shtfld(3,convar(icon2,1))

C Establish condensation heat of water; assume most recent node temp.
         hw=cndwat(csvf(inod1,1))

c Calculate current component time-constant TC
         tc(ipcomp)=amax1(cm/amax1(small,(c1+ua)),
     &                    cw/amax1(small,c2))

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
            cw=0.
            alpha=1.
         end if

C Evaluate coefficients: self coupling, cross coupling,
C inter-component coupling and right hand side.
C Do air equation first
         cout(1)=alpha*(-thr*pcdatf(ipcomp,1)-c1-ua)-cm/timsec*thr
         cout(2)=alpha*pcdatf(ipcomp,1)

C then water equation
         cout(3)=alpha*thr*pcdatf(ipcomp,1)
         cout(4)=alpha*(-c2-pcdatf(ipcomp,1))-cw/timsec

C inter-component coupling coefficients
         cout(5)=alpha*(thr*pcdatf(ipcomp,1)+c1-pcdatf(ipcomp,1))+
     &           cm/timsec*thr
         cout(6)=alpha*c2

C equation right hand side.
         cout(7)=((1.-alpha)*(thr*pcdatp(ipcomp,1)+pcrp(icon1)-ua)-
     &           cm/timsec*thr)*csvp(inod1,1)
     &          -(1.-alpha)*pcdatp(ipcomp,1)*csvp(inod2,1)
     &          +((1.-alpha)*(-thr*pcdatp(ipcomp,1)-pcrp(icon1)+
     &           pcdatp(ipcomp,1))+cm/timsec*thr)*pctp(icon1)
     &          -alpha*ua*pcntmf(ipcomp)
     &          -(1.-alpha)*puap(inod1)*pcntmp(ipcomp)
     &          -alpha*pcdatf(ipcomp,2)*hw
     &          -(1.-alpha)*pcdatp(ipcomp,2)*hw
         cout(8)=-(1.-alpha)*pcdatp(ipcomp,1)*thr*csvp(inod1,1)
     &          +((1.-alpha)*(pcrp(icon2)+pcdatp(ipcomp,1))-
     &            cw/timsec)*csvp(inod2,1)
     &          -(1.-alpha)*pctp(icon2)*pcrp(icon2)
     &          +((1.-alpha)*pcdatp(ipcomp,1)*(-1.+thr)+cw/timsec)*
     &           pctp(icon1)
     &          +(alpha*pcdatf(ipcomp,1)*(-1.+thr)-cw/timsec)*
     &           pctf(icon1)

c Store "environment" variables future values
         pctf(icon1)=convar(icon1,1)
         pctf(icon2)=convar(icon2,1)
         pcrf(icon1)=c1
         pcrf(icon2)=c2
         puaf(inod1)=ua

C Establish additional output variables
C calculate total cooling load (W)
         napdat(ipcomp)=3
         pcaout(ipcomp,1)=csvf(inod1,2)*(hih-hoc)-pcdatf(ipcomp,2)*hw
         pcaout(ipcomp,1)=-pcaout(ipcomp,1)

C Calculate sensible cooling load (W)
         pcaout(ipcomp,2)=pcaout(ipcomp,1)*shr

C establish condensate flow rate (kg/s)
         pcaout(ipcomp,3)=pcdatf(ipcomp,2)

c 1st phase mass (ie. air) balance coefficients
      else if(istats.eq.2) then
         cout(1)=1.
         cout(2)=0.
         cout(3)=0.
         cout(4)=1.
         cout(5)=-pcondr(icon1)
         cout(6)=-pcondr(icon2)
         cout(7)=0.
         cout(8)=0.

c 2nd phase mass balance coefficients
      else if(istats.eq.3) then
         cout(1)=1.
         cout(2)=0.
         cout(3)=0.
         cout(4)=1.
         cout(5)=-pcondr(icon1)
         cout(6)=0.
         cout(7)=0.
         cout(8)=0.

C Check whether air temperature is below dew point TDEW, if so
C this component extracts water vapour from moist air stream
C First establish TDEW; assume present moisture content
         patmos=1013.25
         TDEW=DEWPT(CONVAR(ICON1,3)/AMAX1(SMALL,CONVAR(ICON1,2)),PATMOS)

C Now establish coefficients, first for Tair >= Tdew; as this
C is based on future temperature, mark temperature for iteration
         ICSV(INOD1,1)=1
         CSVI(INOD1,1)=CSVF(INOD1,1)
         IF(CSVF(INOD1,1).lt.TDEW) THEN

C Then for Tair < Tdew in which case we have to know the maximum
C vapour mass flow rate VMAX = (Xmax * air mass flow) at Tair
            VMAX=HUMRT1(CSVF(INOD1,1),100.,PATMOS,IOPT)*
     &           PCONDR(ICON1)*CONVAR(ICON1,2)
            COUT(5)=0.
            COUT(7)=VMAX
         END IF
      end if

c Trace output
      if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   itrace(37).ne.0) then
         write(itu,*) ' Component      ',ipcomp,':'
         write(itu,*) ' 2 node (ISV>20) colling coil'
         write(itu,*) ' Matrix node(s) ',inod1,inod2
         write(itu,*) ' Connection(s)  ',icon1, icon2
         write(itu,*) ' Matrix coefficients for ISTATS = ',istats
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',cm,' (J/K)'
            WRITE(ITU,*) ' CW     = ',cw,' (W/K)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            write(itu,*) ' TC     = ',tc(ipcomp),' (sec)'
         END IF
         nitms=8
         write(itu,*) (cout(i),i=1,nitms)
         if(itu.eq.iuout) then
            ix1=(ipcomp/4)*4
            if(ix1.eq.ipcomp.or.ipcomp.eq.npcomp) call epagew
         end if
      end if
      if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   ITRACE(37).ne.0) write(itu,*) ' Leaving subroutine CMP43C'

C Return to calling module
      return
      end
        
C ******************** CMP67C ********************

C CMP67C generates for plant component IPCOMP with plant db code 670 ie.
C 3 node (ISV>19) AC heat transfer tube fed by WCH system
C   node 1 represents solid materials
C   node 2 represents air
C   node 3 represents water
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C   ADATA: 1 Metallic tube total mass (kg)
C          2 Specific heat of tube (J/kgK)
C          3 UA modulus of casing(W/K)
C          4 Mass of encapsulated water (kg)
C   BDATA: 1 Coil outside (air side) heat transfer area (m^2)
C          2 Coil inside (water side) heat transfer area (m^2)
C          3 Coil face area (m^2)
C          4 Internal tube diameter (m)
C   CDATA: none

C NOTE: This component was added by Tin Tie Chow.
C       It should not be used yet because it is still
C       being tested.

      SUBROUTINE CMP67C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PTIME/PTIMEP,PTIMEF
      common/pctime/timsec
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

      REAL      COUT(MPCOE)
      logical closea,closeb,closec

      PI = 4.0 * ATAN(1.0)

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP67C'

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
C First mark air and water 1st phase mass flow rates for iteration
         ICSV(INOD2,2)=1
         CSVI(INOD2,2)=CSVF(INOD2,2)
         ICSV(INOD3,2)=1
         CSVI(INOD3,2)=CSVF(INOD3,2)

C Start with establishing air VA and water VW velocity, and incomimng air and
C water temperatures Ta0 & Tw0.
            Ta0=CONVAR(ICON1,1)
            Tw0=CONVAR(ICON2,1)

C Mark current air and water temperature for iteration
            ICSV(INOD2,1)=1
            CSVI(INOD2,1)=CSVF(INOD2,1)
            ICSV(INOD3,1)=1
            CSVI(INOD3,1)=CSVF(INOD3,1)
            Ta1=CSVI(INOD2,1)
            Tw1=CSVI(INOD3,1)

C Determine values of valuables used in this component e.g. encapsulated
C masses, fluid mass flow rates, specific heats, flow velocities, heat
C transfer coefficients, and latent heat.
            RMs=ADATA(IPCOMP,1)
            RMw=ADATA(IPCOMP,4)
            RMa=0.0
            Rma0=PCONDR(ICON1)*CONVAR(ICON1,2)
            Rmv0=PCONDR(ICON1)*CONVAR(ICON1,3)
            Rmw0=PCONDR(ICON2)*CONVAR(ICON2,2)
            Rma1=PCONDR(ICON1)*CSVP(INOD2,2)
            Rmv1=PCONDR(ICON1)*CSVP(INOD2,3)
            Rmw1=PCONDR(ICON2)*CSVP(INOD3,2)
            Cs=ADATA(IPCOMP,2)
            Cpa0=SHTFLD(1,CONVAR(ICON1,1))
            Cpv0=SHTFLD(2,CONVAR(ICON1,1))
            Cpw0=SHTFLD(3,CONVAR(ICON2,1))
            Cpa1=SHTFLD(1,Ta1)
            Cpv1=SHTFLD(2,Ta1)
            Cpw1=SHTFLD(3,Tw1)
            VA=Rma1/(BDATA(IPCOMP,3)*RHOFLD(1,Ta1))
            VW=Rmw1/(RHOFLD(3,Tw1)*BDATA(IPCOMP,4)**2*PI/4.)
            ho=38.*VA
            Ao=BDATA(IPCOMP,1)
            Ai=BDATA(IPCOMP,2)
            hi=1400.*(1.+0.015*Tw1)*VW**0.8/BDATA(IPCOMP,4)**0.2
            call eclose(convar(icon1,2),0.00,0.0001,closeb)
            call eclose(convar(icon2,2),0.00,0.0001,closec)
            if(closeb.or.closec) then
               ho=5.0
               hi=15.0
               rma0=0.0
               rma1=0.00
               rmv0=0.0
               rmv1=0.0
            endif
            Hfg=CNDWAT(Ta1)
            HfgP=CNDWAT(CSVP(INOD2,1))

C Establish fluid heat capacity rates (W/K), ie. SUM(mass flow * specific heat)
         Ca1=Rma1*Cpa1+Rmv1*Cpv1
         Cw1=Rmw1*Cpw1
         Ca0=Rma0*Cpa0+Rmv0*Cpv0
         Cw0=Rmw0*Cpw0
         Csw=0.5*hi*Ai
         Cas=0.5*ho*Ao
         Cae=UA
         Cav=UA*PCNTMF(IPCOMP)-Hfg*(Rmv0-Rmv1)
         CavP=UA*PCNTMP(IPCOMP)-HfgP*(CSVP(ICON1,3)-CSVP(INOD2,3))

C Calculate current component time-constant TC (max. of solid and water)
         TC(IPCOMP)=1.0

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
         COUT(1)=2*ALPHA*(Cas-Csw)-RMs*Cs/TIMSEC
         COUT(2)=ALPHA*Cas
         COUT(3)=ALPHA*Csw

C  node 2 (air)
         COUT(4)=2*ALPHA*Cas
         COUT(5)=ALPHA*(Cae-Ca1-Cas)-RMa*Cpa1/TIMSEC

C  node 3 (water)
         COUT(6)=2*ALPHA*Csw
         COUT(7)=-ALPHA*(Cw1+Csw)-RMw*Cpw1/TIMSEC

C then matrix equation cross-coupling coefficients,
         COUT(8)=ALPHA*Cas
         COUT(9)=ALPHA*Csw
         COUT(10)=ALPHA*(Ca0-Cas)
         COUT(11)=ALPHA*(Cw0-Csw)

C and then present-time coefficients (ie. right hand sides)
      COUT(12)=-(1.-ALPHA)*Cas*PCTP(ICON1)-(1.-Alpha)*Cas*CSVP(INOD2,1)
     &         -(1.-ALPHA)*Csw*PCTP(ICON2)-(1.-Alpha)*Csw*CSVP(INOD3,1)
     &            +(-2*(1.-Alpha)*(Cas-Csw)-RMs*Cs/Timsec)*CSVP(INOD1,1)
      COUT(13)=(-(1.-Alpha)*(Cae-Ca1-Cas)-RMa*Cpa1/Timsec)*CSVP(INOD2,1)
     &            -(1.-ALPHA)*(Ca0-Cas)*PCTP(ICON1)
     &            -2*(1.-Alpha)*Cas*CSVP(INOD1,1)
     &            +Alpha*Cav-(1.-ALPHA)*CavP
      COUT(14)=((1.-Alpha)*(Cw1+Csw)-RMw*Cpw1/Timsec)*CSVP(INOD3,1)
     &            +(1.-ALPHA)*(Cw0-Csw)*PCTP(ICON2)
     &            -2*(1.-Alpha)*Csw*CSVP(INOD1,1)

C Store "environment" variables future values
         PUAF(INOD1)=UA
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCTF(ICON2)=CONVAR(ICON2,1)
         PCRF(ICON1)=Ca1
         PCRF(ICON2)=Cw1

C 1st phase mass (ie. dry air) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=0.
         COUT(4)=0.
         COUT(5)=1.
         COUT(6)=0.
         COUT(7)=1.
         COUT(8)=0.
         COUT(9)=0.
         COUT(10)=-PCONDR(ICON1)
         COUT(11)=-PCONDR(ICON2)
         COUT(12)=0.
         COUT(13)=0.
         COUT(14)=0.

C 2nd phase mass (ie. vapour) balance coefficients
C Assume no vapour condensation for the time being
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=0.
         COUT(4)=0.
         COUT(5)=1.
         COUT(6)=0.
         COUT(7)=1.
         COUT(8)=0.
         COUT(9)=0.
         COUT(10)=-PCONDR(ICON1)
         COUT(11)=0.
         COUT(12)=0.
         COUT(13)=0.
         COUT(14)=0.
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 3 node (ISV>19) AC heat transfer tube',
     &                ' fed by WCH system'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2,INOD3
         WRITE(ITU,*) ' Connection(s)  ',ICON1,ICON2
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' Ca1    = ',Ca1,' (W/K)'
            WRITE(ITU,*) ' Cw1    = ',Cw1,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' UA     = ',UA,' (W/K)'
            WRITE(ITU,*) ' Ta0    = ',Ta0,' (C)'
            WRITE(ITU,*) ' Tw0    = ',Tw0,' (C)'
            WRITE(ITU,*) ' VA     = ',VA,' (m/s)'
            WRITE(ITU,*) ' VW     = ',VW,' (m/s)'
            WRITE(ITU,*) ' Ca0    = ',Ca0,' (W/K)'
            WRITE(ITU,*) ' Cw0    = ',Cw0,' (W/k)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=14
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP67C'
      RETURN
      END

C ******************** CMP70C ********************

C CMP70C generates for plant component IPCOMP with plant db code 700 ie.
C 1 node (ISV=20) Theoratical flat-plate solar collector &
C                     water mass flow rate control.
C Rate of heat qain is calculated using TRNSYS type1 solar collector
C with collector mode set to 3. See TRNSYS manual for type1 and type16.
C
C Matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Component total mass (kg)
C            2 Mass weighted average specific heat (J/kgK)
C            3 UA modulus (W/K)
C     BDATA: 1 Radiation mode (-)
C            2 Tracking mode (-)
C            3 Latitude (degrees)
C            4 Solar Constant (W/m^2)
C            5 Shift in solar time hour angle (degrees)
C            6 Number of collectors in series (-)
C            7 Total collector area (m^2)
C            8 Specific heat of collector fluid (J/kg K)
C            9 Collector fin efficiency factor (-)
C           10 Loss coefficient for bottom and edge of collector
C              per unit aperture area (W/m^2 K)
C           11 Absorber plate emittance (-)
C           12 Absorbtance of absorber plate (-)
C           13 Number of glass covers (-)
C           14 Index of refraction of cover material (-)
C           15 Product of extinction coefficient and thickness
C              of each cover plate.
C           16 Ground reflectance (-)
C           17 Slope of surface or tracking axis (degrees)
C           18 Azimuth of surface or tracking axis (degrees)

      SUBROUTINE CMP70C(IPCOMP,COUT,ISTATS)
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

      common/simsdy/iss,isf
      COMMON/CLIM/IDIF(MT),ITMP(MT),IDNR(MT),IVEL(MT),IDIR(MT),
     &IHUM(MT),IDIFF,ITMPF,IDNRF,IVELF,IDIRF,IHUMF
      COMMON/CLIMIP/QFPP,QFFP,TPP,TFP,QDPP,QDFP,VPP,VFP,DPP,DFP,HPP,HFP

      DIMENSION XIN(15),PAR(15),OUT(20),INFO(10),sav1(20)

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      character outs*124
      logical closea,closeb
      data time/0.0/,info1/0/,info16/0/

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP10C'

C Check control data for relevant balance type
      IF(ISTATS.EQ.1.AND.CDATA(IPCOMP,1).LT.0.) THEN
         CALL DAYCLK(IDYP,PTIMEF,IUOUT)
         WRITE(outs,*) ' CMP70C: invalid control data for component ',
     &                  IPCOMP,' : ',CDATA(IPCOMP,1)
         call edisp(iuout,outs)
         call edisp(iuout,' CMP70C: unresolvable error.')
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

C Now calculate heat gain Q using single node model and
C TRNSYS type1 component, unless water mass flow rate is zero.
C First mark water temperature for iteration.
         ICSV(INOD1,1)=1
         CSVI(INOD1,1)=CSVF(INOD1,1)
         call eclose(CONVAR(ICON1,2),0.00,0.0001,closeb)
         IF(closeb) THEN
            Q=0.
         ELSE

C If at start of simulation, initialise array info.
         call eclose(ptimep,1.00,0.0001,closea)
         if(closea.and.idyp.eq.iss) then

C Set arbitrary unit number
            info(1)=1

C Set initialisation flag for types 1 & 16.
            info16=-1
            info1 =-1
            ptim=ptimep
            time=ptimep
            qt=0.0
         endif

C Calculate absolute simulation time.
C and time-step, hour fraction format
            delt=timsec/3600.
            call eclose(ptim,ptimep,0.0001,closeb)
            if(.NOT.closeb) time=time+delt

C Now call type16 solar radiation processor to calculate solar
C radiation components for solar collector.
C So setup interface parameters for type16 first..
            par(1)=bdata(ipcomp,1)
            par(2)=bdata(ipcomp,2)
            par(3)=iss
            par(4)=bdata(ipcomp,3)
            par(5)=bdata(ipcomp,4)*3600./1000.
            par(6)=bdata(ipcomp,5)
            par(7)=0.0

C Assume calculation start time =0.0
            time0=1.0

C Calculate final time
            tfinal=(float(isf)-float(iss)+1.0)*24.0


C Set up input parameters. The first input is the
C radiation on horizontal surface (KJ/hr-m^2). Not sure
C if this is how it is calculated.!! (essam).
            xin(1)=float(IDIF(ihrp)+IDNR(ihrp))*3600.0/1000.0

C Time of last radiation data reading.
            xin(2)=float(ihrp)

C Time of next radiation data reading.
            xin(3)=float(ihrf)
            xin(4)=bdata(ipcomp,16)
            xin(5)=bdata(ipcomp,17)
            xin(6)=bdata(ipcomp,18)

C Set value of INFO(7) accoringly to indicate 
C whether this is first call of TRNSYS type.
            info(7)=info16

C This statement means that radiation components
C are required for one surface. (see type16).
            info(3)=8

C Call type16.
            call type16(time,xin,out,time0,tfinal,delt,par,info,iuout)

C Now setup interface parameters for type1.
C For now this is hard-wired to collector mode 3 
C (i.e theoretical flat plate collector).
            par(1)=3.

C SI units.
            par(2)=1.
            par(3)=bdata(ipcomp,6)
            par(4)=bdata(ipcomp,7)
            par(5)=SHTFLD(3,csvf(inod1,1))/1000.
            par(6)=bdata(ipcomp,9)
            par(7)=bdata(ipcomp,10)*3600./1000.
            par(8)=bdata(ipcomp,11)
            par(9)=bdata(ipcomp,12)
            par(10)=bdata(ipcomp,13)
            par(11)=bdata(ipcomp,14)
            par(12)=bdata(ipcomp,15)

C Set input parameters
            xin(1)=csvf(inod1,1)

C Convert to kg/hr.
            xin(2)=convar(icon1,2)*3600.
            xin(3)=tfp
            xin(4)=out(6)
            xin(5)=vfp
            xin(6)=out(4)
            xin(7)=out(5)
            xin(8)=bdata(ipcomp,16)
            xin(9)=out(9)
            xin(10)=bdata(ipcomp,17)

            do 707 ii=1,20
  707          out(ii)=sav1(ii)
            info(7)=info1

C Call type1 to calculate heat gain.
            call type1(ptimep,xin,out,t,dt,par,info)
            do 708 ii=1,20
  708          sav1(ii)=out(ii)

C Convert Q to Watts.
            Q=out(3)*1000./3600.
            ptim=ptimep
            info1=0
            info16=0
         endif

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

C 1st phase mass (ie. dry air) balance coefficients
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
         WRITE(ITU,*) ' 1 node solar collector (TRNSYS type1)'
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
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP70C'

      RETURN
      END

C ******************** CMP71C ********************
C CMP71C generates for plant component IPCOMP with plant db code
C 720 i.e. one node (ISV=20) solar collector "segment".
C Matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance 
C (ISTATS=1), 1st phase mass balance (ISTATS=2), or 2nd phase mass 
C (ISTATS=3).
C ******************* NOTES **********************
C CMP71C was developed from the model described in "Simulation of 
C Thermal Systems", a publication of the project OPSYS of the Solar 
C Energy Applications to Buildings and Solar Radiation Data R&D 
C programmes of the CEC (1991). The equations for the solar model
C are derived from those found in Duffie and Beckman "Solar Thermal
C Engineering Processes".
C ************************************************
C 
C
C      ADATA : 1. Collector mass (kg) 
C              2. Collector mass weighted thermal capacity (W/kgK)
C              3. Back and sides heat loss coefficient (W/m^2K)
C              4. Collector surface area (front) (m^2)
C              5. Collector surface area (back & sides) (m^2)
C              6. The number of transparent cover plates (-)
C              7. Collector efficiency factor (-)
C              8. Absorber plate emittance (-)
C              9. Absorber plate absorptance (-) 
C             10. Cover plate emittance (-)
C             11. Extinction coeffient of the glazing (-)
C             12. Average cover plate thickness (m)
C             13. Refraction index of cover plate (-)
C             14. Collector tilt (0 - 90 degrees)
C             15. Collector azimuth (0 - 360 degrees)
C             16. Ground reflectance (0 - 1.0)
C             17. Collector latitude (degrees)
C             18. Collector longtitude (degrees)
C             19. Collector tube spacing (m)
C             20. Tube internal diameter (m)
C             21. Conductivity of sheet material (W/mK)
C             22. Sheet thickness (m)
C      
C     BDATA : None.
C
C     CDATA : None.
C ************************************************
C
C Descriptions of the variables used in this subroutine:
C
C  AC    - Collector surface area (front) (m^2)
C  AB    - Collector surface area (back & sides) (m^2)
C  AI    - Angle of incidence for the direct beam (Deg)
C  AP    - Cover plate emittance (-)
C  AZI   - Absolute azimuth angle for collector (Deg)
C  BETA  - Collector tilt (0 - 90 degrees)
C  C     - Collector mass weighted thermal capacity (W/kgK)
C  D     - The thickness of the (individual) cover plates (m)
C  DIA   - Collector tube internal diameter (m)
C  EG    - Absorber plate absorptance (-)
C  EP    - Absorber plate emittance (-)
C  HW    - Collector convective heat transfer coefficient (W/m^2)
C  IRAD  - Total radiation*TA product of the collector
C  K     - Extinction coeffient of the glazing (-)
C  KR    - Ground reflectance (0 - 1.0)
C  KSHT  - Collector back sheet conductivity (W/mK)
C  LAT   - Collector latitude (degrees)
C  LON   - Collector longtitude (degrees)
C  M     - Collector mass (kg)
C  N     - The number of collector cover plates (-)
C  RI    - Refraction index of cover plate (-)
C  SRADDO- Direct solar radiation in collector plane (W/m^2K)
C  GRDIF - Ground diffuse componennt in collector plane (W/m^2K) 
C  SKYDIF- Skydiffuse component in collector plane (W/m^2K) 
C  TE    - Containment temp (future time step) (C)
C  TADIF - Transmission absorptance product for the direct beam
C  TADIF -      "         "            "      "     diffuse beam
C  TAGR  -      "         "            "    ground reflected beam
C  TEP   - Containment temp (present time step) (C)
C  TP    - Mean collector back plate temp (C)
C  TPP   - Mean collector back plate temp (C)
C  TSHT  - Collector back sheet thickness (m)
C  UA    - Back and sides heat loss coefficient (W/m^2K)
C  UL    - Front heat loss coefficient (W/m^2K)
C  ULP   - Front heat loss coefficient present time step (W/m^2K)
C  W     - Tube spacing
C  WVEL  - Wind velocity (10*m/s)
C ************************************************
      SUBROUTINE CMP71C(IPCOMP,COUT,ISTATS)

#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/CLIM/IDIF(MT),ITMP(MT),IDNR(MT),IVEL(MT),IDIR(MT),
     &IHUM(MT),IDIFF,ITMPF,IDNRF,IVELF,IDIRF,IHUMF

      COMMON/SOLP/ULP(MPCOM),TPP(MPCOM),IRADP(MPCOM)

      COMMON/Pctime/TIMSEC
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP

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
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)
      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE),K,KR,LAT,LON,IRAD,IRADP,N,WVEL,KSHT,MU1
     &          ,MFIN
      CHARACTER OUTS*72
      logical closea,closeb,close

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP71C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD.
      ICON1=ICONDX(IPCOMP,1,1)
      INOD1=NPCDAT(IPCOMP,9)

C Generate coefficients for energy balance equation.
      IF(ISTATS.EQ.1) THEN

C Initialise component parameters from the database information.

C Collector 'back and sides' heat loss coefficient.
        UA=ADATA(IPCOMP,3) 
        call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
        IF(closea) THEN
          WRITE(IUOUT,*) 'CMP71C : A containment must be specified '
          WRITE(IUOUT,*) 'for component',IPCOMP,'and all components'
          WRITE(IUOUT,*) 'of the same type'
          GOTO 999
        ENDIF

C Mark water temp and flow rate for iteration.
         ICSV(INOD1,1)=1
         CSVI(INOD1,1)=CSVF(INOD1,1)
         ICSV(INOD1,2)=1
         CSVI(INOD1,2)=CSVF(INOD1,2)
C Establish heat capacity of component mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K), ie. 
C SUM(mass flow * specific heat)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(3,CONVAR(ICON1,1))

C Collector coverplate, 'back and sides' areas (m^2)
C and the number of cover plates.
         AC=ADATA(IPCOMP,4)
         AB=ADATA(IPCOMP,5)
         N=ADATA(IPCOMP,6)


C Obtain the absorber and cover plate data
         EP=ADATA(IPCOMP,8)
         EG=ADATA(IPCOMP,9)
         AP=ADATA(IPCOMP,10)
         K=ADATA(IPCOMP,11)
         D=ADATA(IPCOMP,12)
         RI=ADATA(IPCOMP,13)

C Establish the component site data - tilt, azimuth, ground reflectance
C latitude,longtitude and containment temp.
         BETA=ADATA(IPCOMP,14)
         THETA=ADATA(IPCOMP,15)
         KR=ADATA(IPCOMP,16)
         LAT=ADATA(IPCOMP,17)
         LON=ADATA(IPCOMP,18)
         TE=PCNTMF(IPCOMP)
         TEP=PCNTMP(IPCOMP)

C Get data to establish the effectiveness of the collector
         W=ADATA(IPCOMP,19)
         DIA=ADATA(IPCOMP,20)
         KSHT=ADATA(IPCOMP,21)
         TSHT=ADATA(IPCOMP,22)      
C Determine the values of solar altitude and azimuth for the future 
C time step. Also check to see that the future time step is not the
C next day.
           TIMEH=IHRF
           
           CALL EAZALTS(TIMEH,IDYF,LAT,LON,ISUNUP,SAZI1,SALT1)
         
         IF(ISUNUP.GT.0) THEN
C Determine absolute azimuth angle for collector.
           AZI=ABS(THETA-SAZI1)

C Calculate the incident angle for the direct beam.
C multiply by R to get radians , divide to get degrees
           R=ATAN(1.0)/45.0
           X1=COS(SALT1*R)
           X2=SIN((90.-BETA)*R)
           X3=COS(AZI*R)
           X4=SIN(SALT1*R)
           X5=COS((90.-BETA)*R)
           CAI=X1*X2*X3+X4*X5
           AI=ACOS(CAI)/R
C Calculate the TRAP for this angle (transmission absorptance poduct)
           CALL TRAP(AI,N,K,D,AP,RI,TA)
           
           TADIR=TA
           
C Calculate the effective incident angle for the diffuse beam,
C note that the equation is in radians so we require to convert beta to
C radians.
           AIDIF=(1.0416-(0.1388-0.0858*BETA*R)*(BETA*R))/R
C Calculate the TRAP for this angle
           CALL TRAP(AIDIF,N,K,D,AP,RI,TA)
           TADIF=TA
C Calculate the effective incident angle for the ground reflected 
C radiation.
           AIGR=(1.5707-(0.5788-1.5430*BETA*R)*(BETA*R))/R
C Calculate the TRAP for this angle
           CALL TRAP(AIGR,N,K,D,AP,RI,TA)
           TAGR=TA
C Calculate the values of direct, diffuse and ground reflected
C rediation in the plane of the solar collector. Values
C are calculated using the Klucher model.

C *** START Klucher.
C At this stage QD and QF are the unmodified direct normal and
C diffuse horizontal values. We now modify for circumsolar activity.
C Note that the evaluation of CLEAR is approximate since the
C circumsolar has not yet been determined.
C Set values of QF and QD !! Check that climate file is not global 
C hoizontal  !!
           QF=IDIF(IHRF)
           QD=IDNR(IHRF)
           SRADT=QF+(QD*SIN(SALT1*R))
           IF(SRADT.LT.0.001)goto 1
           CLEAR=1.-((QF**2)/(SRADT**2))
           goto 2
    1      CLEAR=0.

    2      QFB=QF/(1.0+(CLEAR*(SIN(SALT1*R)**2)*((SIN((90.0-SALT1)
     &     *R))**3)))

C Use temporary variables (ADJD & ADJF) to avoid adjusting
C QD & QF each time this routine is called per surface.
           ADJD=QD+((QF-QFB)/SIN(SALT1*R))
           ADJF=QFB

C Direct solar radiation incident normally on
C external construction prior to shading adjustment
C and absorption.
           SRADDO=ADJD*CAI
           IF( SRADDO .LT. 0.0 ) SRADDO = 0.0

C  The ground reflected component: assume isotropic
C  ground conditions.
           SRADT=ADJF+(ADJD*SIN(SALT1*R))
           GRDDIF=0.5*KR*(1.0-COS(BETA*R))*SRADT

C  The sky diffuse component.
            SKYDIF=ADJF*0.5*(1.0+COS(BETA*R))
         
C *** END Klucher.


C Calculate total future radiation incident on collector.        
           IRAD=SRADDO*TADIR + SKYDIF*TADIF + GRDDIF*TAGR 
         ELSE
           IRAD=0
         ENDIF


C Calculate component current performance data.
C Calculate HW the collector front heat transfer coefficient.
C Ivel is divided by ten as it has a value if 1/10ths of m/s.
         WVEL=IVEL(IHRF)
         HW=5.7+(3.8*(0.1*WVEL))

C Calculate F
         F=(1.0+0.089*HW - 0.1166*HW*EP)*
     &   (1.0+0.07866*N)

C Calculate C
         IF (BETA.LT.70.0) THEN
           C=520*(1.0-0.000051*(BETA**2))
         ELSE
           C=390.052
         END IF

C Iterate values of TP and UL until within user defined limits.
C Set value of ULP for iteration.
         ULI=ULP(IPCOMP)
         ITER=1

C Firsltly calculate the effectiveness of the collector
C based on the previous value of UL, any major difference 
C between ULI and UL will require iteration.

C Calculate the fin efficiency for the collector, we need
C to calculate the value m for the sheet material m^2=Ul/kd
  20     MFIN=(ULI/(KSHT*TSHT))**0.5

C Calculate the fin efficiency
         call eclose(MFIN,0.00,0.001,close)
         IF (close) THEN
C         IF (MFIN.EQ.0.) THEN
           GOTO 30
         ELSE
           FEFF=(TANH(MFIN*(W-DIA)/2))/(MFIN*(W-DIA)/2)
         ENDIF

C Calculate the tube internal heat transfer coefficent HIN
C assume turbulent conditions within the tubes, if laminar 
C print a warning.
C Definition of PI
         PI=3.1415927

C Fluid density 
         RHO1=RHOFLD(3,CSVF(INOD1,1))

C Fluid Velocity
         VEL1=CONVAR(ICON1,2)/((PI*(DIA**2)/4)*RHO1)

C Stagnation situation i.e. flow=0.0
         call eclose(CONVAR(ICON1,2),0.00,0.0001,closeb)
         IF(closeb) THEN
          GOTO 29
         ENDIF
C Fluid viscosity
         MU1=DYVISC(2,CSVF(INOD1,1))
         
C Reynolds number 
         REYNO1=RHO1*VEL1*DIA/MU1

         

C Calculate the Prandtl number for current conditions.
C Pr=mu*cp/k
C Evaluate the fluid conductivity - this is based on a linear 
C relationship between k and Temp, however this will be replaced 
C with an empirical derivation from Reid and Sherwood.
      COND1=(1.12*CONVAR(ICON1,1)+569.)/1000.

C Dynamic viscosity
      VISC1=DYVISC(2,CONVAR(INOD1,1))

C Specific heat
      SHT1=SHTFLD(3,CONVAR(INOD1,1))

C Prandtl number
      PRANDT1=MU1*SHT1/COND1

C Calculate the internal tube htc
         FFR=(0.79*ALOG(REYNO1)-1.64)**(-2.)
         FR=FFR/8
         HIN=(COND1/DIA)*(FR*REYNO1*PRANDT1)/
     & (1.07 + 12.7*(FR**0.5)*((PRANDT1**0.667)-1.0))

C         HIN=(COND1/DIA)*0.023*(REYNO1**0.8)*(PRANDT1**0.4)
C No flow condition.

C And finally calculate EFF, assuming bond resistance is negligible.
         EFF1=1/(ULI*(DIA+(W-DIA)*FEFF))
         EFF2=1/(PI*DIA*HIN)
         EFF=(1/ULI)/(W*(EFF1+EFF2))
         
C Calculate TP the mean collector surface temp.
      
   30    TP1=ULI+UA
         TP2=ULI*(CSVF(INOD1,1)-PCNTMF(IPCOMP))
         TP3=UA*(CSVF(INOD1,1)-PCNTMF(IPCOMP))

         TP=CSVF(INOD1,1)+273.0+(1.0-EFF)*((IRAD-TP2-TP3)/TP1)

C In the stagantion case there is no flow to or from the collector
C in this case assume the default effectiveness and that Tfluid=Tp
   29    call eclose(CONVAR(ICON1,2),0.00,0.0001,closeb)
         IF(closeb) THEN
           TP=CSVF(INOD1,1)
           EFF=ADATA(IPCOMP,7)
         ENDIF
C An error trap is needed to ensure that TP-TE is never 0 or -ve!       
         IF(TP.LE.(TE+273.0))  THEN
         TP=TE+274.0
         ENDIF
C Calculate E using calculated value of TP
         E=0.43*(1.0-(100.0/TP))

C Calculate UL the collector front loss coefficient
      

          UL1=((TP-TE-273.)/(N + F))**E
          UL2=(1/HW + (N*TP)/(UL1*C))
          UL3=5.667E-8*(TP+(TE+273.))*(TP**2.0+(TE+273.0)**2.0)
          UL4=(EP+0.00591*N*HW)**(-1.0)
          UL5=(1.0/EG)*(2*N+F-1.+0.133*EP)
          UL=UL2**(-1.0)+(UL3/(UL4+UL5-N))

C Compare old and new values of UL, if the difference is greater than
C 1 degC recalculate TP and UL.

         IF(ABS(UL-ULI).GT.0.5.AND.ITER.LT.50) THEN
           ULI=UL
           ITER=ITER+1
           GOTO 20 
         ELSE IF(ITER.GT.50) THEN
           WRITE(OUTS,*)'CMP71C : Max number of iterations
     &  exceeded'
           CALL EDISP(IUOUT,OUTS)
         ELSE
           CONTINUE
         ENDIF
   
         
C Calculate the current component time constant TC
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

C Calculate the component coefficients,
C Establish matrix equation self and cross-coupling coeffiients.
         COUT(1)=ALPHA*(-AC*EFF*UL-AB*EFF*UA-C1)-CM/TIMSEC
         COUT(2)=ALPHA*C1

C Establish the present time coefficient (right hand side).
         COUT(3)=((1.-ALPHA)*(AC*EFF*ULP(IPCOMP) + AB*EFF*PUAP(INOD1) + 
     &            PCRP(ICON1))
     &           -CM/TIMSEC)*CSVP(INOD1,1)
     &           -(1.0-ALPHA)*AC*EFF*ULP(IPCOMP)*TEP
     &           -ALPHA*AC*EFF*UL*TE
     &           -(1.-ALPHA)*AB*EFF*PUAP(INOD1)*TEP
     &           -ALPHA*AB*F*UA*TE
     &           -(1.-ALPHA)*PCRP(ICON1)*PCTP(ICON1)
     &           -ALPHA*AC*EFF*IRAD
     &           -(1.-ALPHA)*AC*EFF*IRADP(IPCOMP)  

C Store "environment" variables future values
         PUAF(INOD1)=UA
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCRF(ICON1)=C1
C Store present calculated in the components common block
       
         IRADP(IPCOMP)=IRAD
         TPP(IPCOMP)=TP
         ULP(IPCOMP)=UL

C 1st phase mass (i.e water) balance coefficients.
      ELSEIF(ISTATS.EQ.2) THEN
         COUT(1)=1.0
         COUT(2)=-PCONDR(ICON1)
         COUT(3)=0.0

C 2nd phase mass flow (i.e air) balance coefficients.
      ELSEIF(ISTATS.EQ.3) THEN
         COUT(1)=1.0
         COUT(2)=0.0
          COUT(3)=0.0
      ENDIF

C Establish additional output variables for th results file.
      napdat(ipcomp)=3
      PCAOUT(IPCOMP,1)=UL
      PCAOUT(IPCOMP,2)=SRADDO+GRDDIF+SKYDIF
      PCAOUT(IPCOMP,3)=TP

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
       WRITE(ITU,*) ' Component     ',IPCOMP,':'
       WRITE(ITU,*) ' 1 node (ISV=20) solar collector "segment"'
       WRITE(ITU,*) ' Matrix node(s) ',INOD1
       WRITE(ITU,*) ' Connection(s)  ',ICON1
        IF(ISTATS.EQ.1) THEN
          WRITE(ITU,*) ' DAY    =', IDYF
          WRITE(ITU,*) ' HOUR    =', IHRF
          WRITE(ITU,*) ' ISUNUP =', ISUNUP
          WRITE(ITU,*) ' EFF    =',EFF,'(-)'
          WRITE(ITU,*) ' CM     =',CM, '(W/kgK)'
          WRITE(ITU,*) ' UA     =',UA,  '(W/m^2K)'
          WRITE(ITU,*) ' UL     =',UL,UL1,UL2,UL3,UL4,UL5,'(W/m^2K)'
          WRITE(ITU,*) ' TP     =',TP,  '(C)'
          WRITE(ITU,*) ' IRAD   =',IRAD,'(W/m^2)'
          WRITE(ITU,*) ' CAI    =',CAI,'(Deg)'
          WRITE(ITU,*) ' T      =',CSVP(INOD1,1),'(C)'
          WRITE(ITU,*) ' TF     =',CSVF(INOD1,1),'(C)'
          WRITE(ITU,*) ' TE     =',PCNTMF(IPCOMP)+273.0,'(C)'
          WRITE(ITU,*) ' C      =',C,'(-)'
          WRITE(ITU,*) ' F      =',F,'(-)'
          WRITE(ITU,*) ' E      =',E,'(-)'
          WRITE(ITU,*) ' HW     =',HW,'(W/m^2K)'
          WRITE(ITU,*) ' SKYDIF     =',SKYDIF,'(W/m^2)'
          WRITE(ITU,*) ' GRDDIF     =',GRDDIF,'(W/m^2)'
          WRITE(ITU,*) ' SRADDO     =',SRADDO,'(W/m^2)'
          WRITE(ITU,*) ' TADIR TADIF TAGR =',TADIR,TADIF,TAGR, '(Deg)'
        ENDIF
      ENDIF

C Return to the calling module
      RETURN

C Error handling
  999 STOP ' Fatal error for CMP71C - specify a containment !'
C
      END

C ******************** CMP72C ********************
C CMP72C generates for plant component IPCOMP with plant db code
C 720 i.e. one node (ISV=20) water storage tank layer, with one or 
C two connections.
C Matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance 
C (ISTATS=1), 1st phase mass balance (ISTATS=2), or 2nd phase mass 
C (ISTATS=3).
C ******************* NOTES **********************
C CMP73C was developed from the model described in "Simulation of 
C Thermal Systems", a publication of the project OPSYS of the Solar 
C Energy Applications to Buildings and Solar Radiation Data R&D 
C programmes of the CEC (1991). The model described here is a 
C modified version of that model. The tank layer is designed for
C use in an active solar heating circuit but can also be used in
C a normal DHW system model.
C ************************************************
C
C
C ADATA : 1. Layer UA modulus (W/m^2K))
C         2. Layer mass weighted specific heat (W/kgK)
C         3. Layer mass (kg)
C         4. Layer cross sectional area (m^2)
C         5. Layer depth (m)
C         6. The position of the layer in the tank (-)
C     
C
C ************************************************
C Variables used in this calculation 
C VEFF     - Volumetric expansion coefficient for water
C QGAINUP  - Conduction gains from neighbouring 
C QGAINDN  - tank layers (inc buoyancy effect)      
C GRAV     - Gravitational acceleration
C DPTH     - The depth of the fluid layer
C RK       - Fluid heat conduction coefficient
C KEQ      - The equivalent heat conduction coeffient describing the effect 
C          of buoyancy
C LTYPE

C RHOT     - The density  of the fluid layer
C SHLYR    - The specific heat of the fluid layer
C TML      - The turbulent mixing length
C VKK      - The Von Karman constant

C ************************************************ 
      SUBROUTINE CMP72C(IPCOMP,COUT,ISTATS)

#include "plant.h"
#include "building.h"

      
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
       
      COMMON/Pctime/TIMSEC
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP

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
      
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)
      PARAMETER (SMALL=1.0E-15)
      INTEGER UPNOD,DWNOD
      REAL      COUT(MPCOE),RK,KEQUP,KEQDN,KEQUP1,KEQUP2,KEQUP3,
     & KEQUP4,KEQDN1,KEQDN2,KEQDN3,KEQDN4
      logical closea
      

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP72C'

C Establish the layer type within the tank 0-single, 
C 1-intermediate, 2-bottom, 3-top
       LTYPE=int(ADATA(IPCOMP,6))

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD.
C Where the upnod node corresponds to the i-1 th layer and the dwnod 
C corresponds to the i+1 th layer. 
      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,1,2)
C Determine whether the layer has one or two connections.
      IF(ICON1.GT.0.AND.ICON2.GT.0) THEN 
         ICONLYR=1
      ELSE
         ICONLYR=0
      ENDIF
      INOD1=NPCDAT(IPCOMP,9)

C Identify the adjoining nodes to be used when calculating 
C conduction and buoyancy gain/loss from adjoining layers.
C Note that the storage tank layers must have sequential 
C component numbers i.e ipcomp 1,2,3 or 4,5,6 non-sequential
C numbers will mean the gain from adjoining layers will be calculated 
C incorrectly.
      IF (LTYPE.EQ.0) THEN
        DWNOD=0
        UPNOD=0
      ELSEIF (LTYPE.EQ.1) THEN
        DWNOD=NPCDAT(IPCOMP-1,9)
        UPNOD=NPCDAT(IPCOMP+1,9)
      ELSEIF (LTYPE.EQ.3) THEN
        DWNOD=NPCDAT(IPCOMP-1,9)
        UPNOD=0
      ELSEIF (LTYPE.EQ.2) THEN
        UPNOD=NPCDAT(IPCOMP+1,9)
        DWNOD=0
      ENDIF
C Generate coefficients for energy balance equation.
      IF(ISTATS.EQ.1) THEN

C Initialise component parameters from the database information.
            
C Layer heat loss coefficient. 
      UA=ADATA(IPCOMP,1)
      call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
      IF(closea) UA=0.
C Layer thermal capacity
      CM=ADATA(IPCOMP,2)*ADATA(IPCOMP,3)

C Layer cross sectional area and depth.
      ASLYR=ADATA(IPCOMP,4)
      DPTH=ADATA(IPCOMP,5)



C Mark  nodal temps for iteration
       ICSV(INOD1,1)=1
       CSVI(INOD1,1)=CSVF(INOD1,1)


C Estblish fluid capacity rates
      C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(3,CONVAR(ICON1,1))
      IF (ICONLYR.EQ.1) THEN
         C2=PCONDR(ICON2)*CONVAR(ICON2,2)*SHTFLD(3,CONVAR(ICON2,1))
      ENDIF
C Calculate the gains to the layer from conduction and buoyancy 
C induced flow. This gain takes the form Qgain=A(K + Keq)dT/dx
C where k is the conduction between layers and keq is a term which
C represents the equivalent conduction between layers caused by 
C buoyancy.

C When dT/dx > 0. turbulent mixing develops an for most liquids Keq >> K,
C the effective conductivity therefore varies strongly with tank temperature 
C gradients.
C Define physical constants used in the buoyancy equation

C Density of tank fluid
      RHOT = RHOFLD(3,CSVF(INOD1,1))

C Specific heat of tank fluid
      SHLYR = SHTFLD(3,CSVF(INOD1,1))

C Ze Von Karman universal constant
      VKK = 0.4

C Gravitational constant 
      GRAV=9.81

C Turbulent mixing length
      TML=0.07

C Volumetric expansion coefficient for water
      VEFF=0.00034

C Fluid thermal conductivity (assume this is constant)
      RK=0.02624

C For Keq an equation derived by Viskanta is used.

C Initialise the values of qgain and keq
      QGAINUP=0.0
      QGAINDN=0.0
      KEQUP=0.0
      KEQDN=0.0
      Q=0.0
C If the tank layer position is 0 - single then

      IF (LTYPE.EQ.0) THEN 
        QGAINUP=0.0
        QGAINDN=0.0
      ENDIF
      IF (LTYPE.EQ.1.OR.LTYPE.EQ.2) THEN
C Calculate the effective conduction from upper i-1 th "upnod" layer, 
C this is 0. is the layer type is "top".
        KEQUP1=(CSVF(INOD1,1)-CSVF(UPNOD,1))
        KEQUP2=(2./3.)*RHOT*SHLYR*VKK*(TML**2)
        KEQUP3=(2.*GRAV*VEFF)**0.5
        KEQUP4=(2.*DPTH)**0.5
        IF (KEQUP1.LE.0.0) THEN 
          KEQUP=0.
        ELSE
          KEQUP=((KEQUP2*KEQUP3)/KEQUP4)*KEQUP1
        ENDIF

C Now calculate the total conduction between the i and i-1 th layer.
C This value is given the value qgainup, and can be either negative or 
C positive.
        QGAINUP1=2.*(ASLYR**2)*(RK**2)
        QGAINUP2=2.*(RK*ASLYR*DPTH)
        QGAINUP3=2.*(ASLYR**2)*KEQUP
        QGAINUP4=2.*(ASLYR*DPTH)
        QGAINUP5=(CSVF(UPNOD,1)-CSVF(INOD1,1))
        QGAINUP=((QGAINUP1/QGAINUP2)+(QGAINUP3/QGAINUP4))*QGAINUP5
      ENDIF
      IF (LTYPE.EQ.1.OR.LTYPE.EQ.3) THEN
C Calculate the effective conduction from lower i+1 th layer, this
C is 0. is the layer type is "bottom".
        KEQDN1=(CSVF(DWNOD,1)-CSVF(INOD1,1))
        KEQDN2=(2./3.)*RHOT*SHLYR*VKK*(TML**2)
        KEQDN3=(2.*GRAV*VEFF)**0.5
        KEQDN4=(2.*DPTH)**0.5
        IF (KEQDN1.LE.0.0) THEN 
          KEQDN=0.
        ELSE
          KEQDN=((KEQDN2*KEQDN3)/KEQDN4)*KEQDN1
        ENDIF
C Now calculate the total conduction between the i and i+1 th layer.
C This value is given the value qgaindn, and can be either negative or 
C positive.
        QGAINDN1=2.*(ASLYR**2)*(RK**2)
        QGAINDN2=2.*(RK*ASLYR*DPTH)
        QGAINDN3=2.*(ASLYR**2)*KEQDN
        QGAINDN4=2.*(ASLYR*DPTH)
        QGAINDN5=(CSVF(DWNOD,1)-CSVF(INOD1,1))
        QGAINDN=((QGAINDN1/QGAINDN2)+(QGAINDN3/QGAINDN4))*QGAINDN5

      ENDIF
        
C Establish the final value of the heat transfer to/from the layer
       Q=QGAINUP+QGAINDN

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
      ENDIF

C Calculate the component coefficients,
C Establish matrix equation self and cross-coupling coeffiients.

C Self-coupling coefficient
       IF(ICONLYR.EQ.1) THEN
          COUT(1)=ALPHA*(-C1-C2-UA)-CM/TIMSEC
       ELSE
          COUT(1)=ALPHA*(-C1-UA)-CM/TIMSEC
       ENDIF

C Cross coupling coeffients
       COUT(2)=ALPHA*C1

       IF (ICONLYR.EQ.1) THEN
         COUT(3)=ALPHA*C2
       ELSE
C Present time and known coefficient
       COUT(3)=((1.-ALPHA)*(PCRP(ICON1)+PUAP(INOD1))
     &        -CM/TIMSEC)*CSVP(INOD1,1)
     &        -(1.-ALPHA)*PCRP(ICON1)*PCTP(ICON1)
     &        -ALPHA*UA*PCNTMF(IPCOMP) 
     &        -(1.-ALPHA)*PUAP(INOD1)*PCNTMP(IPCOMP)
     &        -ALPHA*Q
     &        -(1.-ALPHA)*PCQP(INOD1)
       ENDIF

       IF (ICONLYR.EQ.1) THEN
C Present time and known coefficient
       COUT(4)=((1.-ALPHA)*(PCRP(ICON1)+PCRP(ICON2)
     &        +PUAP(INOD1))-CM/TIMSEC)*CSVP(INOD1,1)
     &        +(1.-ALPHA)*(-PCRP(ICON1))*PCTP(ICON1)
     &        +(1.-ALPHA)*(-PCRP(ICON2))*PCTP(ICON2)
     &        -ALPHA*UA*PCNTMF(IPCOMP) 
     &        -(1.-ALPHA)*PUAP(INOD1)*PCNTMP(IPCOMP)
     &        -ALPHA*Q
     &        -(1.-ALPHA)*PCQP(INOD1)
        ENDIF

C Store "environment" variables future values
         PUAF(INOD1)=UA
         PCTF(ICON1)=CONVAR(ICON1,1)
         IF (ICONLYR.EQ.1) THEN
           PCTF(ICON2)=CONVAR(ICON2,1)
           PCRF(ICON2)=C2
         ENDIF
         PCRF(ICON1)=C1
         PCQF(INOD1)=Q


C Mass balance coefficients
C 1st phase mass balance i.e. water.

      ELSEIF(ISTATS.EQ.2) THEN
      COUT(1)=1.
      COUT(2)=-PCONDR(ICON1)
      COUT(3)=-PCONDR(ICON2)
      COUT(4)=0.

C Mass balance coefficients
C 2nd  phase mass balance i.e. vapour
      ELSEIF(ISTATS.EQ.3) THEN
      COUT(1)=1.
      COUT(2)=0.
      COUT(3)=0.
      COUT(3)=0.
      ENDIF

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
       WRITE(ITU,*) ' Component     ',IPCOMP,':'
       WRITE(ITU,*) ' 1 node (ISV=20) stratified storage tank'
       WRITE(ITU,*) ' Matrix node(s) ',INOD1
       WRITE(ITU,*) ' Connection(s)  ',ICON1,ICON2
        IF(ISTATS.EQ.1) THEN
          WRITE(ITU,*) ' TC         =',TC(IPCOMP),'(s)'
          WRITE(ITU,*) ' CM         =',CM, '(W/kgK)'
          WRITE(ITU,*) ' UA         =',UA,  '(W/m^2K)'
          WRITE(ITU,*) ' TEMP       =',CSVF(INOD1,1),'(C)'
          WRITE(ITU,*) ' QGAINUP    =',QGAINUP,'(W)'
          WRITE(ITU,*) ' QGAINDN    =',QGAINDN,'(W)'
          WRITE(ITU,*) ' Qcomps UP  =',QGAINUP1,QGAINUP2,QGAINUP3
     & ,QGAINUP4,QGAINUP5
          WRITE(ITU,*) ' Qcomps DN  =',QGAINDN1,QGAINDN2,QGAINDN3
     & ,QGAINDN4,QGAINDN5
          WRITE(ITU,*) ' Keq        =',KEQUP, KEQDN
          WRITE(ITU,*) ' Kcomps UP  =',KEQUP1, KEQUP2,KEQUP3, KEQUP4
          WRITE(ITU,*) ' Kcomps DN  =',KEQDN1, KEQDN2,KEQDN3, KEQDN4
          WRITE(ITU,*) ' Q          =',Q,'(W)'
          WRITE(ITU,*) ' FLOW1      =',CONVAR(ICON1,2),'(kg/s)'
     & ,CONVAR(ICON1,1)
          WRITE(ITU,*) ' FLOW2      =',CONVAR(ICON2,2),'(kg/s)'
     & ,CONVAR(ICON2,1)
          WRITE(ITU,*) ' ADJACENT   =','UP ',UPNOD,'DOWN ',DWNOD
          WRITE(ITU,*) ' ADJ TEMPS  =',CSVF(UPNOD,1),CSVF(DWNOD,1)
        ENDIF
      ENDIF

C Return to calling module
      RETURN
  
      END

c ******************** CMP92C ********************
c CMP92C generates for plant component IPCOMP with plant db code
c 920 ie. 1 node (ISV=21) fictitious boundary component,
c matrix equation coefficients COUT (in order: self-coupling, cross-
c coupling, and present-time coefficients) for energy balance (ISTATS=1),
c 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3).
c    ADATA : None.
c    BDATA : None.
c    CDATA : None.

      subroutine cmp92c(ipcomp,cout,istats)
#include "plant.h"
#include "building.h"

      common/outin/iuout,iuin
      common/tc/itc,icnt
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/pctc/tc(mpcom)

      common/pcequ/impexp,ratimp

      common/c9/npcomp,nci(mpcom),cdata(mpcom,mmiscd)
      common/c10/npcon,ipc1(mpcon),ipn1(mpcon),ipct(mpcon),
     &           ipc2(mpcon),ipn2(mpcon),pcondr(mpcon),pconsd(mpcon,2)
      common/c12ps/npcdat(mpcom,9),ipofs1(mcoefg),ipofs2(mcoefg,mpvar)
      common/pcond/convar(mpcon,mconvr),icontp(mpcon),
     &             icondx(mpcom,mnodec,mpconc)

      parameter (small=1.0e-20)
      real      cout(mpcoe)

c Trace output
      if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   ITRACE(37).ne.0) write(itu,*) ' Entering subroutine CMP92C'  

c Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      icon1=icondx(ipcomp,1,1)
      inod1=npcdat(ipcomp,9)

c Generate coefficients for energy balance equation
      if(istats.eq.1) then

c Establish matrix equation self- and cross-coupling coefficients
         cout(1)=1.0
         cout(2)=0.0

c and then present-time coefficient (ie. right hand side)
         cout(3)=convar(icon1,1)

c 1th phase mass (ie. water) balance coefficients
      else if(istats.eq.2) then
         cout(1)=1.
         cout(2)=-pcondr(icon1)
         cout(3)=0.

c 2nd phase mass balance coefficients
      else if(istats.eq.3) then
         cout(1)=1.
         cout(2)=0.0
         cout(3)=convar(icon1,3)
      end if

c Trace output
      if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   itrace(37).ne.0) then
         write(itu,*) ' Component      ',ipcomp,':'
         write(itu,*) ' 1 node (ISV=21) fictitious air boundary node'
         write(itu,*) ' Matrix node(s) ',inod1
         write(itu,*) ' Connection(s)  ',icon1
         write(itu,*) ' Matrix coefficients for ISTATS = ',istats
         nitms=3
         write(itu,*) (cout(i),i=1,nitms)
         if(itu.eq.iuout) then
            ix1=(ipcomp/4)*4
            if(ix1.eq.ipcomp.or.ipcomp.eq.npcomp) call epagew
         end if
      end if
      if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   ITRACE(37).ne.0) write(itu,*) ' Leaving subroutine CMP92C'

C Return to calling module
      return
      end


C ******************** CMP93C ********************

C CMP93C generates for plant component IPCOMP with plant db code 930 ie.
C 10 node (ISV>20) Heat exchanger model.

C Model theory based on paper titled " The Transient Response of Heat
C Exchangers Having an Infinte Capacitance Rate Fluid." By Myers G.E.
C Journal of heat transfer, Vol 92.

C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3).
C Total number of coefficents generated is 35.

C     ADATA: 1 Total mass of cold fluid (kg)
C            2 Cold fluid mass weighted average specific heat (J/kgK)
C            3 Total mass of solids (kg)
C            4 Solid mass wieghted average specific heat (J/kgK)
C            5 UA modulus (W/K)
C     BDATA: 1 Cold fluid flow area (m^2)
C            2 Heat exchanger flow length (m)
C            3 Cold fluid heat transfer resistance (K/W)
C            4 Hot fluid heat transfer resistance (K/W)
C     CDATA: 1 Hot fluid temperature (C)

C     PCDATF/P
C            1 Inter-node fluid heat capacity rate (W/K)
C            2 Hot fluid temperature (C)

      SUBROUTINE CMP93C(IPCOMP,COUT,ISTATS)
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

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)

      DIMENSION INOD(10)
      logical closea


C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP27C'

C Initialise number of nodes for this component.
      inodes=npcdat(ipcomp,8)

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      INOD(1)=NPCDAT(IPCOMP,9)
      DO 10 I=2,inodes
         INOD(I)=INOD(I-1)+1
   10 CONTINUE

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C Evaluate the current environment temperature
C There are 5 segments.
         ua=adata(ipcomp,3)/5.
         call eclose(pcntmf(ipcomp),-99.00,0.001,closea)
         if(closea) ua=0.

C Since fluid nodes calculations are based on first node
C temperature, mark first node for iteration.
         ICSV(INOD(1),1)=1
         CSVI(INOD(1),1)=CSVF(INOD(1),1)

C Initilise DELTA x for each segment.
         deltax=bdata(ipcomp,2)/5.

C For convenience, establish resistance
C to heat transfer for cold and hot fluid respectively.
C These variables are assumed to be fixed.
         rc=bdata(ipcomp,3)
         rh=bdata(ipcomp,4)

C Establish heat capacity of cold fluid mass CM (J/K) and
C fluid heat capacity rate(s) C (W/K).
         cmc=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)/5.
         c1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(1,CONVAR(ICON1,1))+
     &      PCONDR(ICON1)*CONVAR(ICON1,3)*SHTFLD(2,CONVAR(ICON1,1))

C Establish heat capacity of solid mass CM (J/K).
         cms=ADATA(IPCOMP,3)*ADATA(IPCOMP,4)/5.

C Inter-node fluid heat capacity rate (W/K)
         PCDATF(IPCOMP,1)=CSVF(INOD(1),2)*rhofld(1,CSVF(INOD(1),1))*cmc/
     &                    (bdata(ipcomp,1)*deltax)

C Establish future hot fluid temperature.
         pcdatf(ipcomp,2)=cdata(ipcomp,1)

C Calculate current component time-constant TC, based on node 1 only.
C ** Note that this is incorrect **. To be fixed later.
         TC(IPCOMP)=cms/AMAX1(SMALL,C1)

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
C  nodes 1 to 5 (fluid nodes).
         do 20 ip=1,(inodes-5)
            if(ip.eq.1) then
               fc=c1
            else
               fc=pcdatf(ipcomp,1)
            endif
            cout(ip*3-2)=ALPHA*(-fc-1./rc)-cmc/timsec
            cout(ip*3-1)=alpha/rc
            if(ip.lt.5) cout(ip*3)=pcdatf(ipcomp,1)
   20    continue

C then nodes 6 to 10 (solid nodes).
C 'ioffse' ensures that we are at the right coefficient.
         ioffst=5
         do 30 ip=6,inodes
            cout(ip*2-2+ioffst)=alpha/rc
            cout(ip*2-1+ioffst)=alpha*(-1./rc-1./rh-ua)-cms/timsec
   30    continue

C now the first 24 coefficients have been calculated.
C Matrix equation cross-coupling coefficient (25) follows:
         cout(25)=alpha*c1

C and then present-time coefficients (26 to 35) (ie. right hand sides)
C node 1
         cout(26)=((1.-ALPHA)*(1./rc+PCRP(ICON1)-cmc/timsec))
     &            *CSVP(INOD(1),1)
     &            +(1.-alpha)*(-1./rc)*csvp(inod(6),1)
     &            +(1.-ALPHA)*(-PCRP(ICON1))*PCTP(ICON1)

C nodes 2 to 5
         coef1=(1.-alpha)*(-pcdatp(ipcomp,1))
         coef2=(1.-alpha)*(pcdatp(ipcomp,1)+1./rc)-cmc/timsec
         coef3=(1.-alpha)*(-1./rc)
         do 22 IP=2,(inodes-5)
            cout(25+ip)= coef1*csvp(inod(ip-1),1)
     &                 + coef2*csvp(inod(ip),1)
     &                 + coef3*csvp(inod(ip+5),1)
   22    continue

C nodes 6 to 10
         coef1=(1.-alpha)*(1./rc+1./rh+ua)-cms/timsec
         coef2=(1.-alpha)*(-1./rc)
         coef3=(1.-alpha)*(-1./rh)
         do 23 ip=6,inodes
            cout(25+ip)= coef1*csvp(inod(ip),1)
     &              + coef2*csvp(inod(ip-5),1)
     &              + coef3*pcdatp(ipcomp,2)-(alpha/rh*pcdatf(ipcomp,2))
     &              - alpha*ua*PCNTMF(IPCOMP)
     &              - (1.-alpha)*puap(inod(ip))*PCNTMP(IPCOMP)
   23    continue

C Store variables future values
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCRF(ICON1)=C1
         do 24 ip=6,inodes
   24      PUAF(INOD(ip))=UA

C 1st phase mass (ie. water) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN

C Calculate self coupling coefficients for nodes 1 to 5.
         COUT(1)=1.
         COUT(2)=0.
         DO 31 IP=2,(inodes-5)
            cout(3*ip-3)=1.
            COUT(3*IP-2)=-1.
            COUT(3*IP-1)=0.
   31    CONTINUE

C Calculate self coupling coefficients for nodes 6 to 10.
C 'ioffse' ensures that we are at the right coefficient.
         ioffst=5
         DO 32 IP=6,inodes
            COUT(2*IP-2+ioffst)=0.
            COUT(2*IP-1+ioffst)=1.
   32    CONTINUE

C calculate cross coupling coefficient.
         cout(25)=-pcondr(icon1)

C Now establish RHS vector coefficients
         do 35 ip=1,inodes
   35       cout(25+ip)=0.0

C 2nd phase mass (ie. "vapour") balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN

C Calculate self coupling coefficients for nodes 1 to 5.
         cout(1)=1.
         cout(2)=0.
         DO 40 IP=2,(inodes-5)
            cout(3*ip-3)=1.
            cout(3*ip-2)=-1.
            cout(3*ip-1)=0.
   40    CONTINUE

C Calculate self coupling coefficients for nodes 6 to 10.
C 'ioffse' ensures that we are at the right coefficient.
         ioffst=5
         DO 42 IP=6,inodes
            COUT(2*IP-2+ioffst)=0.
            COUT(2*IP-1+ioffst)=1.
   42    CONTINUE

C calculate cross coupling coefficient.
         cout(25)=-pcondr(icon1)

C Now establish RHS vector coefficients
         do 45 ip=1,inodes
   45       cout(25+ip)=0.0
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 10 node (ISV>20) Heat exchanger'
         WRITE(ITU,*) ' Matrix node(s) ',(INOD(I),I=1,10)
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CMC     = ',CMC,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' CMS     = ',CMS,' (J/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' CDATA   = ',cdata(IPCOMP,1),' (C)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=35
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP93C'

      RETURN
      END

C ******************** CMP94C ********************
C
C CMP94C generates for plant component IPCOMP with plant db code 940 ie.
C 3 node counterflow "shell & tube heat exchanger segment.
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C
C ************************************************
C
C The heat exchanger segment is developed from an idea  in "Simulation of 
C Thermal Systems", a publication of the project OPSYS of the Solar 
C Energy Applications to Buildings and Solar Radiation Data R&D 
C programmes of the CEC (1991). The equations governing the thermal
C properties  of the heat exchanger are taken from "Fundamentals of
C Momentum, Heat and Mass Transfer 3rd Ed." by Wilson, Welty and Wicks
C pp 363-377 and pp 414-425.
C
C  ADATA:  1. Total mass of the segment solids (kg)
C          2. Mass of encapsulated water in tubes (kg)
C          3. Mass of "    "    "        in shell (kg)
C          4. UA modulus of casing (W/K)
C          5. Conduction coefft from shell fluid - casing (W/K)
C          6. Conduction coefft from tube fluid - casing  (W/K)
C          7. Number of tubes
C          8. Internal tube radius (m)
C               9. External tube radius (m)
C              10. Length of segment (m)
C              11. Spacing between tubes (m)
C              12. Conductivity of tubing material (W/mK)
C              13. Mass weighted average specific heat
C       BDATA: none
  
C       CDATA: none
C ************************************************

      SUBROUTINE CMP94C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PCTC/TC(MPCOM)
      COMMON/PCEQU/IMPEXP,RATIMP

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
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)
C Heat exchanger common block variables 
      COMMON/HEXCH/HINP(MPCOM),HOUTP(MPCOM)

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE),K1,K2,HIN,HOUT,HINP,HOUTP
      logical closea

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP94C'



C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,2,1)
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1
      INOD3=NPCDAT(IPCOMP,9)+2

C First mark air and water 1st phase mass flow rates and temps for iteration
         ICSV(INOD1,1)=1
         CSVI(INOD1,1)=CSVF(INOD1,1)
         ICSV(INOD1,2)=1
         CSVI(INOD1,2)=CSVF(INOD1,2)
         ICSV(INOD2,1)=1
         CSVI(INOD2,1)=CSVF(INOD2,1)
         ICSV(INOD2,2)=1
         CSVI(INOD2,2)=CSVF(INOD2,2)

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN
C Establish the heat capacities of the solid node and
C the enapsulated fluid masses.
      CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,13)
      CW1=ADATA(IPCOMP,2)*SHTFLD(3,CONVAR(ICON1,1))
      CW2=ADATA(IPCOMP,3)*SHTFLD(3,CONVAR(ICON2,1))

C Establish the UA modulus and fluid - casing heat loss coefficients (W/K)
      UA=ADATA(IPCOMP,4)
      call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
      IF (closea)  UA=0.0

      K1=ADATA(IPCOMP,6)
      K2=ADATA(IPCOMP,5)

C Calculate the fluid thermal capacities
      C1=PCONDR(ICON1)*CONVAR(ICON1,2)*
     &   SHTFLD(3,CONVAR(ICON1,1))
      C2=PCONDR(ICON2)*CONVAR(ICON2,2)*
     &   SHTFLD(3,CONVAR(ICON2,1))

C Definition of PI
      PI=3.1415927

C Calculate the thermal parameters necessary to evaluate the
C heat exchange within the component. NB that connection 1 should
C flow through the tubes and connection 2 the flow accross the 
C tubes.

C Calculate the total flow area in the tube bundle.
      FLWAREA1=ADATA(IPCOMP,7)*PI*(ADATA(IPCOMP,8)**2)

C Calculate the density of the fluid entering.
      RHO1=RHOFLD(3,CONVAR(ICON1,1))

C Calculate the flow velocity in the tube bundle.
      FLWVEL1=CONVAR(ICON1,2)/(RHO1*FLWAREA1)

C Calculate the flow area accoss the tube bundle
      FLWAREA2=ADATA(IPCOMP,7)*ADATA(IPCOMP,11)*ADATA(IPCOMP,10)

C Calculate the Reynold's number for tube cross flow.
      RHO2=RHOFLD(3,CONVAR(ICON2,1))

C Calculate the flow velocity across the tubes
      FLWVEL2=CONVAR(ICON2,2)/(RHO2*FLWAREA2)

C Calculate the Reynold's number for internal tube flow.
C Re=rho*V*D/mu

C Calculate the fluid viscosity
      VISC1=DYVISC(2,CONVAR(ICON1,1))

C Finally calculate the reynold's number for the tube 
C flow.
      REYNO1=(RHO1*FLWVEL1*2*ADATA(IPCOMP,8))/VISC1

C Calculate the fluid viscosity.
      VISC2=DYVISC(2,CONVAR(ICON2,1))

C Reynolds number for cross tube flow. Here the charactersitic 
C length is the tube outer diameter.
      REYNO2=(RHO2*FLWVEL2*2*ADATA(IPCOMP,9))/VISC2


C Calculate the heat transfer coefficients for both the inside and 
C outside tube surfaces. These equations are taken from HEAT EXCHANGERS -
C selection design and construction by E.A.D Saunders Longman Scientific,
C New York, 1988
C
C Inside tube.
C First calculate the Prandtl number for current conditions.
C Pr=mu*cp/k
C Evaluate the fluid conductivity - this is based on a linear 
C relationship between k and Temp, however this will be replaced 
C with an empirical derivation from Reid and Sherwood.
      COND1=(1.12*CONVAR(ICON1,1)+569.)/1000.
C
      PRANDT1=(VISC1*SHTFLD(3,CONVAR(ICON1,1)))/COND1

C Calculate the Graetz number for internal tube flow RePr(d/L)
      GZ1=REYNO1*PRANDT1*(2*(ADATA(IPCOMP,9)/ADATA(IPCOMP,10)))

C Evaluate the wall temperature and from this deduce the wall dynamic 
C viscosity.
         IF (HINP(IPCOMP).LT.SMALL) THEN
         TWALL=CONVAR(ICON1,1)
         ELSE
         TWALL=CONVAR(ICON1,1)+(PCQP(INOD1)/HINP(IPCOMP))
         ENDIF
         VISCW=DYVISC(2,TWALL)

C For the inside surface the value of HIN is dependant upon
C the type of flow; Re<2000 laminar 2000<Re<10000 transition,
C Re>10000 fully turbulent.
C Turbulent case.
      IF(REYNO1.GE.10000.) THEN
         HIN=0.0204*(COND1/(2.*ADATA(IPCOMP,8)))*PRANDT1**0.415*
     &REYNO1**0.805*(VISC1/VISCW)**0.3

C Transition case
      ELSEIF(REYNO1.LT.10000..AND.REYNO1.GT.2000.) THEN
         HIN=0.1*(COND1/(2.*ADATA(IPCOMP,8)))*((REYNO1**0.667)-125.)*
     &   (PRANDT1**0.495)*(EXP(-0.0225*(ALOG(PRANDT1))**2))*
     &   ((1.+(2.*ADATA(IPCOMP,8)/ADATA(IPCOMP,10)))**0.667)
     &   *((VISC1/VISCW)**0.14)

C Laminar case1
      ELSEIF(REYNO1.LE.2000.0.AND.GZ1.GT.9.) THEN 
C Calculate the Grasshof number for internal tube.
         GR=(1./CSVF(IPCOMP,1))*9.81*((2*(ADATA(IPCOMP,8)))**3)*RHO1*
     &   (CSVF(IPCOMP,1)-TWALL)/(VISC1**2)

         HIN=1.75*(COND1/(2.*ADATA(IPCOMP,8)))*
     &   (GZ1+0.00083*(GR*PRANDT1)**0.75)**0.33*((VISC1/VISCW)**0.25)

C Laminar case2
      ELSEIF(REYNO1.LE.2000.0.AND.GZ1.LE.9.) THEN 
         HIN=3.66*(COND1/(2.*ADATA(IPCOMP,8)))
      ENDIF
             
C Store the value of HIN for the next timestep
        HINP(IPCOMP)=HIN
C
C Evaluate the outside heat transfer coefficient HOUT. For this value
C assume that the tube pitch is 45 degrees i.e a staggered tube 
C arrangement.
C 
C First evlauate the fluid conductivity.
C Again this value is based on a linear relationship.
        COND2=(1.12*CONVAR(ICON2,1)+569.)/1000.   
C 
C Evaluate the Prandtl number for the exterior flow.
        PRANDT2=(VISC2*SHTFLD(3,CONVAR(ICON2,1)))/COND2
C Evaluate the Prandtl number for the tube wall temperature.

         IF (HOUTP(IPCOMP).LT.SMALL) THEN
         TWALL2=CONVAR(ICON2,1)
         ELSE
         TWALL2=CONVAR(ICON2,1)+(PCQP(INOD2)/HOUTP(IPCOMP))
         ENDIF

        PRANDTW=(VISCW*SHTFLD(3,TWALL2))/COND2
C Evaluate dimensionless parameter hj, based on previous timestep 
C values
        IF(REYNO2.LT.300.0) THEN
          HJ=1.33*REYNO2**0.353
        ELSE
          HJ=0.274*REYNO2**0.635
        ENDIF

C Evaluate the heat transfer coefficient using HJ.
        HOUT=HJ*(COND2/(2*ADATA(IPCOMP,9)))*PRANDT2**0.33*
     &((PRANDT2/PRANDTW)**0.26)

C Calculate the external htc for a tube in cross flow - later use 
C an equation for tube banks in cross flow.

        
C Store value of HOUT for next timestep.
        HOUTP(IPCOMP)=HOUT
C Now that HIN and HOUT have been calculated we can evaluate the overall
C heat transfer coefficient for the tubes based the external area. UoAo.

C Evaluate the various quantities required in the equation.

C Inner area of tubing.
      AIN=2*PI*ADATA(IPCOMP,8)*ADATA(IPCOMP,10)
     &*ADATA(IPCOMP,7)
C Outer tube area.
      AOUT=2*PI*ADATA(IPCOMP,9)*ADATA(IPCOMP,10)
     &*ADATA(IPCOMP,7)
C Calculate UA overall
      UAO1=(AIN*HIN)**(-1)
      UAO2=ALOG(ADATA(IPCOMP,9)/ADATA(IPCOMP,8))
     &     /(2*PI*ADATA(IPCOMP,12)*ADATA(IPCOMP,10))
      UAO3=(AOUT*HOUT)**(-1)

      UAO=(UAO1+UAO2+UAO3)**(-1)

C Evaluate the mean temperatures in the heat exchanger nodes upon
C which the value of  transfered flux will be based.

C Give initial values to T1out and T2out.
      T1out=CSVF(INOD1,1)
      T2out=CSVF(INOD2,1)

  5   T1AVE=(CONVAR(ICON1,1)+T1out)/2
      T2AVE=(CONVAR(ICON2,1)+T2out)/2

C Calculate the value of Q using the average values.
      Q=UAO*(T1AVE-T2AVE)

C Establish the flux between the fluids
      Q1=-Q
      Q2=-Q1

C Now evaluate the outlet temperatures for the heat exchanger and if
C necessary iterate.

      T1=Q1/C1 + CONVAR(ICON1,1)

      T2=Q2/C2 + CONVAR(ICON2,1)

C Establish whether iteration is necessary on temperatures.
      IF (ABS(T2-T2out).GT.0.1.OR.ABS(T1-T1out).GT.0.1) THEN 
        T1out=T1
        T2out=T2
        GOTO 5
      ENDIF

C ****In here put efficienct calculation for non-cross flow.
C i.e. F-factors


C Calculate current component time-constant TC
         TC(IPCOMP)=CM/AMAX1(SMALL,(CW1+CW2+CM+UA))

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

C Generate coefficients for the energy balance equation in the order 
C Self-coupling, cross-coupling, present time.
C Matrix layout.
C            "self"  "cross"          "future"
C  node1    x  o  x | x  o | 1 |         |x|
C  node2    o  x  x | o  x | 2 |     =   |x|
C  node3    x  x  x | o  o | 3 |         |x|
C                           -  -  
C                          | 4 |
C                          | 5 |
C
C "Self-coupling" coefficients.
C Node 1 (Tube flow).
      COUT(1)=ALPHA*(-C1-K1)-CW1/TIMSEC
      COUT(2)=K1

C Node 2 (Cross flow).
      COUT(3)=ALPHA*(-C2-K2)-CW2/TIMSEC
      COUT(4)=K2
      
C Node 3 (Solid node).
      COUT(5)=K1
      COUT(6)=K2
      COUT(7)=ALPHA*(-K1-K2-UA)-CM/TIMSEC

C Cross coupling coefficients.
      COUT(8)=C1
      COUT(9)=C2

C Pesent time and "known" coefficients.
      COUT(10)=((1.-ALPHA)*(PCRP(ICON1)+K1)-CW1/TIMSEC)*CSVP(INOD1,1)
     &          -(1.-ALPHA)*PCRP(ICON1)*PCTP(ICON1)   
     &          -(1.-ALPHA)*K1*CSVP(INOD3,1)
     &          -ALPHA*Q1
     &          -(1.-ALPHA)*PCQP(INOD1)
C
      COUT(11)=((1.-ALPHA)*(PCRP(ICON2)+K2)-CW2/TIMSEC)*CSVP(INOD2,1)
     &          -(1.-ALPHA)*PCRP(ICON2)*PCTP(ICON2)
     &          -(1.-ALPHA)*K2*CSVP(INOD3,1)
     &          -ALPHA*Q2
     &          -(1.-ALPHA)*PCQP(INOD2)
C
      COUT(12)=((1.-ALPHA)*(K1+K2+PUAP(INOD3))-CM/TIMSEC)*CSVP(INOD3,1)
     &          -(1.-ALPHA)*K1*CSVP(INOD1,1)
     &          -(1.-ALPHA)*K2*CSVP(INOD2,1)
     &          -ALPHA*UA*PCNTMF(IPCOMP)
     &          -(1.-ALPHA)*PUAP(INOD3)*PCNTMP(IPCOMP)

C Store "environment" varaibles future values
       PUAF(INOD3)=UA
       PCTF(ICON1)=CONVAR(ICON1,1)
       PCTF(ICON2)=CONVAR(ICON2,1)
       PCRF(ICON1)=C1
       PCRF(ICON2)=C2
       PCQF(INOD1)=Q1
       PCQF(INOD2)=Q2

C 1st phase (i.e. water) mass balance coefficients
       ELSEIF(ISTATS.EQ.2) THEN
C Coefficients in order self-coupling, cross coupling and known.
C Note that the matrix coefficients correspond to the same matrix
C posotions as are specified in the energy balance matrix.    

       COUT(1)=1.
       COUT(2)=0.
       COUT(3)=1.
       COUT(4)=0.
       COUT(5)=0.
       COUT(6)=0.
       COUT(7)=1.
       COUT(8)=-PCONDR(ICON1)      
       COUT(9)=-PCONDR(ICON2)    
       COUT(10)=0. 
       COUT(11)=0. 
       COUT(12)=0.

C 2nd phase mass balance coefficients (i.e. dry air)
       ELSEIF(ISTATS.EQ.3) THEN
       COUT(1)=1.
       COUT(2)=0.
       COUT(3)=1.
       COUT(4)=0.
       COUT(5)=0.   
       COUT(6)=0
       COUT(7)=1. 
       COUT(8)=0.
       COUT(9)=0.
       COUT(10)=0.
       COUT(11)=0.
       COUT(12)=0.

       ENDIF

C Trace faciltites
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) 'Component       ',IPCOMP,':'
         WRITE(ITU,*) '3 Node (ISV=20) Heat Exchanger segment'
         WRITE(ITU,*) 'Matrix nodes ',INOD1,INOD2,INOD3 
         WRITE(ITU,*) 'Connections ',ICON1, ICON2
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) 'TEMPS   = ',CSVF(INOD1,1), CSVF(INOD2,1)
     & , CSVF(INOD3,1)
            WRITE(ITU,*) 'CM      = ',CM
            WRITE(ITU,*) 'CW1     = ',CW1
            WRITE(ITU,*) 'CW2     = ',CW2
            WRITE(ITU,*) 'UA      = ',UA
            WRITE(ITU,*) 'Q       = ',Q1, Q2
            WRITE(ITU,*) 'REYNO   = ',REYNO1, REYNO2
            WRITE(ITU,*) 'hi, ho  = ',HIN,HOUT
            WRITE(ITU,*) 'ln(ro/ri)/2pikL= ',UAO2
            WRITE(ITU,*) 'CONDW   = ',COND1, COND2
            WRITE(ITU,*) 'PRANDT  = ',PRANDT1, PRANDT2
            WRITE(ITU,*) 'UoAo    = ',UAO
          ENDIF
       ENDIF

C Return to the calling module.
       RETURN
       
       END

C ******************** CMP98C ********************
C
C CMP98C generates for plant component IPCOMP with plant database
C code 980 i.e. a 1-node CHP engine component model capable of 
C connection to the WCH system.
C Matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2).  
C 
C Data requirements for the model.
C   ADATA:  1 Engine block mass (kg)
C           2 Average specific heat (J/kgK)
C           3 UA modulus for engine block (W/K)
C
C 
C   BDATA:  1 Fuel calorific value (J/kg)
C           2 Idling fuel consumption (kg/s)
C           3 Incremental fuel consumption (kg/s/Nm)
C           4 Combustion efficiency (-)
C           5 Generator efficiency (-)
C           6 Shaft velocity (rpm)
C           7 Generator power factor (-)
C 
C   CDATA:  1 Generator output in (kW) (on/off)
C               

      SUBROUTINE CMP98C(IPCOMP,COUT,ISTATS)

#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PCTC/TC(MPCOM)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat
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

C Electrical details for specified plant components.
      common/elpcp/NPEL,PFP(mpcom),IPFP(mpcom),PWRP(mpcom),
     &BVOLTP(mpcom),IPHP(mpcom)

C CHP unit previous work output.
      common/chpwo/genwp(mpcom) 

      parameter (small=1E-15)
      real cout(mpcoe)
      logical closea

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP98C'

C Debug ... 
C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      INOD1=NPCDAT(IPCOMP,9)

      if(istats.eq.1) then
C Debug ... 

C Initialise the data from the databases         
         ua=adata(IPCOMP,3)
         call eclose(pcntmf(ipcomp),-99.00,0.001,closea)
         if(closea) ua=0.
         genout=cdata(ipcomp,1)

         fcv=bdata(ipcomp,1)*1000.

         fcidl=bdata(ipcomp,2)
         fcinc=bdata(ipcomp,3)
         cef=bdata(ipcomp,4)
         genef=bdata(ipcomp,5)
         svel=bdata(ipcomp,6)

C Establish nodal thermal capacities.
      cm=adata(ipcomp,1)*adata(ipcomp,2)
      c1=pcondr(icon1)*convar(icon1,2)*shtfld(3,convar(icon1,1))
      
C
C Mechanical data will be from a 1st order generator model which will give such 
C data as fuel consumption, heat generation, torque output etc ....

C Calculate torque required from the engine
         gent=(genout/genef)/(svel*0.10471)
         genw=gent*svel*0.10471

C Calculate the heat release from the fuel
C Firstly fuel mass flow.
         fmf=fcidl+fcinc*gent
    

C Heat release.
         qe=fmf*fcv*cef
         if(gent.lt.1E-03)  then
           qe=0.0
           fmf=0.0
         endif

C Temp sensor trip.
c         if(csvf(inod1,1).ge.99.) then
c           qe=0.0
c           fmf=0.0
c           write(outs,*) 'Time: ',ptimef
c           call edisp(iuout,outs)  
c           call edisp(iuout,'CHP unit temp trip Tnod=100.0 ')  
c         endif
       
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

C Establish matrix equation self-coupling coefficients,
         cout(1)=alpha*(-c1-ua)-cm/timsec
         cout(2)=alpha*c1
         cout(3)=((1.-alpha)*(c1+ua)-cm/timsec)*csvp(inod1,1)
     &-(1.-alpha)*pcrp(icon1)*pctp(icon1)
     &+alpha*genw + (1.-alpha)*genwp(ipcomp)
     &-alpha*(0.85)*qe - (1.-alpha)*(0.85)*pcqp(inod1)
     &-alpha*ua*pcntmf(ipcomp) - (1.-alpha)*puap(inod1)*pcntmp(ipcomp)


C Total heat recovery in coolant
         reht=c1*(csvf(inod1,1)-convar(icon1,1)) 
C Save plant additional output.
         napdat(ipcomp)=4
         pcaout(ipcomp,1)=fmf
         pcaout(ipcomp,2)=qe
         pcaout(ipcomp,3)=genout
C Overall electrical efficiency of unit
         if(fmf.gt.1E-20) then
           pcaout(ipcomp,4)=(genout+reht)/(fmf*fcv)
         else
           pcaout(ipcomp,4)=0.0
         endif

C Save electrical data for the component.
         pwrp(ipcomp)=genout
         IEMODEL=1
         CALL EMACH(IPCOMP,IEMODEL,pwrp(ipcomp),PQ,PA)
         PWRQ=PQ

C Store "future" values of variables 
         genwp(ipcomp)=genw
         pcrf(icon1)=c1
         pctf(icon1)=convar(icon1,1)
         pcqf(inod1)=qe 

C 1st Phase mass balance equation. 
      elseif(istats.eq.2) then
         cout(1)=1.
         cout(2)=-pcondr(icon1)
         cout(3)=0.

C 2nd phase mass balance.
      elseif(istats.eq.3) then
         cout(1)=1.
         cout(2)=0.
         cout(3)=0.
      endif


C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV>19) CHP engine/generator unit'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' UA     = ',UA,' (W/K)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
            WRITE(ITU,*) ' NODE TEMP = ',CSVF(INOD1,1),' (C)'
            WRITE(ITU,*) ' TORQUE = ',gent,' (Nm)'
            WRITE(ITU,*) ' HEAT RELEASE = ',qe,' (W)'
            WRITE(ITU,*) ' FUEL CONSUMP = ',fmf, '(kg/s)'
            WRITE(ITU,*) ' OUTPUT PWR   = ',genw,'(W)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=5
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP98C'

       
      return
      end

C ******************** CMP99C ********************
C
C CMP99C generates for plant component IPCOMP with plant db code 990 ie.
C 2 node small scale a CHP unit. Node 1 - cooling water node. Node 2 - 
C exhaust gas node. Node 3 - engine block (mass) node.
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)

C This data for this CHP model is derived from engine test data. Key parameters
C are the cylinder and cooling water heat transfer moduli. These values are 
C expressed as a function of loading. The engine parameters in the ESP-r 
C plant component database is derived from a 36kWe gas engine. However the 
C model allows other data to be used as input, though it must be re-calibrated.
C
C From test data almost all engine parameters vary linearly with the loading. 
C 
C Cubic polynominals have been used to express these parameters as functions 
C of the loading on the engine unit. The engine and generator unit are 
C regarded as one entity within this model, hence all shaft and generator 
C losses are included in the values used here.

C Note that specific heats are not calculated explicitly to prevent the generation
C of error messages when high gas temps are used in equations. 

C  ADATA: 1 Solid node (engine block) total mass (kg)
C         2 Mass weighted average specific heat of block material (J/kgK)
C         3 UA modulus for environmental heat loss (W/K)

C         4 Mass of coolant encapsulated in cooling circuit (kg)
C         5 Fuel calorific value (J/kg)

C     Generator.
C     BDATA: 1 Maximum power output (W)

C     Fuel combustion parameters.       
C            2 Fuel consumption at idling speed (kg/s)
C            3 Incremental fuel consumption (kg/s/%)
C            4 Combustion air intake at idling speed (kg/s)
C            5 Incremental air intake (kg/s/%)

C            6 Combustion efficiency curve fit parameter (a0)           
C            7 Combustion efficiency curve fit parameter (a1)
C            8 Combustion efficiency curve fit parameter (a2)
C            9 Combustion efficiency curve fit parameter (a3)
C
C           10 Cylinder wall Kc curve fit parameter (a0)
C           11 Cylinder wall Kc curve fit parameter (a1)
C           12 Cylinder wall Kc curve fit parameter (a2)
C           13 Cylinder wall Kc curve fit parameter (a3)

C           14 Primary cooling circuit Kw curve fit parameter (a0)
C           15 Primary cooling circuit Kw curve fit parameter (a1)
C           16 Primary cooling circuit Kw curve fit parameter (a2)
C           17 Primary cooling circuit Kw curve fit parameter (a3)

C           18 Incremental UA    
C     CDATA: 1  Loading for CHP unit (%)

      SUBROUTINE CMP99C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PCTIME/TIMSEC
      COMMON/PTIME/PTIMEP,PTIMEF
      COMMON/PCTC/TC(MPCOM)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat
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

C Electrical details for specified plant components
      common/elpcp/NPEL,PFP(mpcom),IPFP(mpcom),PWRP(mpcom),
     &BVOLTP(mpcom),IPHP(mpcom)


      PARAMETER (SMALL=1.0E-20)
      REAL      COUT(MPCOE),LOAD
      character outs*124
      logical closea

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP99C'

C Check control data

      IF(CDATA(IPCOMP,1).LT.0..OR.CDATA(IPCOMP,1).GT.100.) THEN
         CALL DAYCLK(IDYP,PTIMEF,IUOUT)
         WRITE(outs,*) ' CMP99C: invalid control data for component ',
     &                  IPCOMP,' : ',CDATA(IPCOMP,1)
         call edisp(iuout,outs)
         call edisp(iuout,' CMP99C: unresolvable error.')
         call epwait
         call epagend
         STOP
      END IF

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,2,1)

      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1
      INOD3=NPCDAT(IPCOMP,9)+2

C Mark temperatures  for iteration.
      ICSV(INOD1,1)=1
      CSVI(INOD1,1)=CSVF(INOD1,1)
      ICSV(INOD2,1)=1
      CSVI(INOD2,1)=CSVF(INOD2,1)
      ICSV(INOD3,1)=1
      CSVI(INOD3,1)=CSVF(INOD3,1)

C Initialise the required base load (power) from the CHP unit (%).
      LOAD=CDATA(IPCOMP,1)

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN


C Exhaust gas mass flow.
         GMF=BDATA(IPCOMP,4) + BDATA(IPCOMP,5)*LOAD +
     &   BDATA(IPCOMP,2) + BDATA(IPCOMP,3)*LOAD

C Air intake into engine
         AMF=BDATA(IPCOMP,4) + BDATA(IPCOMP,5)*LOAD

C Fuel consumption.
         FC=BDATA(IPCOMP,2) + BDATA(IPCOMP,3)*LOAD

C First initialize UA modulus (for calculation of containment heat loss)
         UA=ADATA(IPCOMP,3) + BDATA(IPCOMP,18)*LOAD
         call eclose(PCNTMF(IPCOMP),-99.00,0.001,closea)
         IF(closea) UA=0.
C Establish the various parameters for the engine unit.

C Combustion efficiency
         COE=BDATA(IPCOMP,6)+ BDATA(IPCOMP,7)*LOAD+
     &     BDATA(IPCOMP,8)*LOAD**2  + BDATA(IPCOMP,9)*LOAD**3 

C Heat release due to combustion.
         QF=COE*FC*ADATA(IPCOMP,5)

C Cylinder wall heat transfer moduli.
         HAC=BDATA(IPCOMP,10)+ BDATA(IPCOMP,11)*LOAD+
     &     BDATA(IPCOMP,12)*LOAD**2  + BDATA(IPCOMP,13)*LOAD**3

C Cooling circuit heat transfer moduli.
         HAW=BDATA(IPCOMP,14)+ BDATA(IPCOMP,15)*LOAD+
     &     BDATA(IPCOMP,16)*LOAD**2  + BDATA(IPCOMP,17)*LOAD**3


C Power output.       
         PWR=LOAD*BDATA(IPCOMP,1)/100. 

C If load is zero then assume the unit is OFF
         IF(LOAD.LT.SMALL) THEN
           PWR=0.0
           TEXH=CONVAR(ICON2,1)
           FC=0.0
           AMF=SMALL
           QCW=0.0
         ENDIF

C Establish heat capacity of component CM (J/K) and 
C the heat capacity of the cooling circuit CW (J/K)
         CM=ADATA(IPCOMP,1)*ADATA(IPCOMP,2)
         CW=ADATA(IPCOMP,4)*SHTFLD(3,CSVP(INOD1,1))

C coolant heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*4180.


C Exhaust heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
C Current Cp value from literature though may be calculated.
         C2=PCONDR(ICON2)*CONVAR(ICON2,2)*1001.00


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

C Generate coefficients for the energy balance equation in the order 
C Self-coupling, cross-coupling, present time.
C Matrix layout.
C            "self"    "cross"        "present time and known"
C  node1    C1  0  C2 | C8 | T1 |         |C10|
C  node2     0  C3 C4 | C9 | T2 |     =   |C11|
C  node3    C5 C6  C7 | 0  | T3 |         |C12|
C                           -  -  
C                          | T4 |
C                          | T5 |
C  Note the connection to T5 sohuld be a connection to ambient.
C
C "Self-coupling" coefficients.
C 
C Node 1 the cooling water node
         COUT(1)=ALPHA*(-C1-HAW)-CW/TIMSEC
         COUT(2)=ALPHA*HAW
C Node 2 the exhaust gas node (no thermal inertia)
         COUT(3)=-C2-HAC
         COUT(4)=HAC
C Node 3 the engine block
         COUT(5)=ALPHA*HAW
         COUT(6)=ALPHA*HAC
         COUT(7)=ALPHA*(-HAC-HAW-UA)-CM/TIMSEC

C Cross coupling coefficients
         COUT(8)=ALPHA*C1
         COUT(9)=ALPHA*C2

C and then present-time coefficient (ie. right hand side)
         COUT(10)=((1.-ALPHA)*(PCRP(ICON1)+PUAP(INOD1))
     &              -CW/TIMSEC)*CSVP(INOD1,1)
     &             -(1.-ALPHA)*(PCRP(ICON1)*PCTP(ICON1))
     &             -(1.-ALPHA)*(PUAP(INOD1)*CSVP(INOD3,1))

         COUT(11)=-QF+PWR

         COUT(12)=((1.-ALPHA)*(PUAP(INOD1)+PUAP(INOD2)+PUAP(INOD3))
     &             -CM/TIMSEC)*CSVP(INOD3,1)
     &             -(1.-ALPHA)*(PUAP(INOD1))*CSVP(INOD1,1)
     &             -(1.-ALPHA)*(PUAP(INOD2))*CSVP(INOD2,1)
     &             -ALPHA*UA*PCNTMF(IPCOMP)
     &             -(1.-ALPHA)*PUAP(INOD3)*PCNTMP(IPCOMP)
C Store "environment" variables future values
         PUAF(INOD1)=HAW
         PUAF(INOD2)=HAC
         PUAF(INOD3)=UA
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCTF(ICON2)=CONVAR(ICON2,1)
         PCRF(ICON1)=C1
         PCRF(ICON2)=C2
         PCQF(INOD2)=QF

C Store the additional variables for results output.
        napdat(ipcomp)=3
        PCAOUT(IPCOMP,1)=LOAD
        PCAOUT(IPCOMP,2)=PWR
        PCAOUT(IPCOMP,3)=FC

C Write the electrical power output to the common blocks.
        PWRP(IPCOMP)=PWR
        IEMODEL=1
        CALL EMACH(IPCOMP,IEMODEL,PWRP(IPCOMP),PQ,PA)
        PWRQ=PQ

C 1st phase mass (ie. water) balance coefficients
        ELSE IF(ISTATS.EQ.2) THEN
          COUT(1)=1.
          COUT(2)=0.
          COUT(3)=1.
          COUT(4)=0.
          COUT(5)=0.
          COUT(6)=0.
          COUT(7)=1.
          COUT(8)=-PCONDR(ICON1)      
          COUT(9)=0.   
          COUT(10)=0. 
          COUT(11)=AMF 
          COUT(12)=0.

C 2nd phase mass (ie. vapour) balance coefficients
       ELSE IF(ISTATS.EQ.3) THEN
          COUT(1)=1.
          COUT(2)=0.
          COUT(3)=1.
          COUT(4)=0.
          COUT(5)=0.
          COUT(6)=0.
          COUT(7)=1.
          COUT(8)=0.      
          COUT(9)=0.   
          COUT(10)=0. 
          COUT(11)=0. 
          COUT(12)=0.        
        END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 3 Node CHP unit'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         WRITE(ITU,*) ' Matrix node(s) ',INOD2
         WRITE(ITU,*) ' Matrix node(s) ',INOD3
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         WRITE(ITU,*) ' Connection(s)  ',ICON2
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' CW     = ',CW,' (J/K)'
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' C2     = ',C2,' (W/K)'
            WRITE(ITU,*) ' QF     = ',QF,' (W)'
            WRITE(ITU,*) ' COE    = ',COE,' (W)'
            WRITE(ITU,*) ' HAW    = ',HAW,' (W/K)'
            WRITE(ITU,*) ' HAC    = ',HAC,' (W/K)'
            WRITE(ITU,*) ' AMF    = ',AMF,' (kg/s)'
            WRITE(ITU,*) ' GMF    = ',GMF,' (kg/s)'
            WRITE(ITU,*) ' FC     = ',FC ,' (kg/s)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' UA     = ',UA,' (W/K)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
            WRITE(ITU,*) ' LOAD   = ',CDATA(IPCOMP,1)
            WRITE(ITU,*) ' POWER  = ', PWR 
            WRITE(ITU,*) ' ENERGY BALANCES '
            data=HAC*(CSVP(INOD3,1)-CSVP(INOD1,1))-C1*(CONVAR(ICON1,1)
     &-CSVP(INOD1,1))
            WRITE(ITU,*) ' COOLING WATER ',data 
            data=QF-PWR+HAW*(CSVP(INOD3,1)-CSVP(INOD2,1))+
     &           C2*(PCNTMF(IPCOMP)-CSVP(INOD2,1))
            WRITE(ITU,*) ' EXHAUST GAS ',data
            data=HAC*(CSVP(INOD1,1)-CSVP(INOD3,1))+HAW*(CSVP(INOD2,1)
     &-CSVP(INOD3,1))-UA*(PCNTMF(IPCOMP)-CSVP(INOD3,1))
            WRITE(ITU,*) ' ENGINE BLOCK ',data
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
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP99C'


      RETURN
      END
 


C ******************** TRAP **********************
C Subroutine TRAP calculates the transmission absorptance product for
C a solar collector glazing system with N transparent covers.
C definition of variables used in subroutine TRAP.
C AI - angle of incidence
C N - the number of covers in the glazing system
C K - extinction coeffient of the glazing system
C d - thickness of the  covers
C T - transmission coefficient
C A - absorption coefficient AP-absorption of backplate at 90 deg
C TE - effective transmission 
C TA - effective absorption
C PP - perpendicular reflectance coefficient
C R - surface reflection coefficient
C PAP - parallel reflectance coefficient
C PD - reflectance at 60 deg of cover system 
C RA - refraction angle
C RI - refraction index
      SUBROUTINE TRAP(AI,N,K,D,AP,RI,TA)

#include "plant.h"
#include "building.h"
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      REAL K,N  
C The formulas in this subroutine are based on those found in
C 'Solar Engineering of thermal processes - Duffy'
C and 'Simulation of thermal systems - CEC'
C Calculate refraction angle RA, PP and PAP
C Check the CAI is not negative otherwise the code will crash!
      R=ATAN(1.0)/45.0
      IF(COS(AI*R).LT.0) THEN
      TA=0.0
      RETURN
      ENDIF
      

      RA=ASIN((SIN(AI*R)/RI))

      PP=(((SIN(RA - AI*R))**2)/((SIN(RA + AI*R))**2))
      PAP=(((TAN(RA - AI*R))**2)/((TAN(RA + AI*R))**2))
      P=0.5*(PP + PAP)

C Calculate the transmission & effective transmission of the cover 
C material.
      T=0.5*((1.0-PP)/(1.+(2.0*N-1.0)*PP)+
     &      (1.0-PAP)/(1.0+(2.0*N-1.0)*PAP))
      TE=T*EXP((-K*N*D)/COS(RA))
      
C If we have a negative value of TE then set TE to zero.
      IF(TE.LT.0) TE=0.0
C Calculate the absorption of the cover material at 60 deg.
      AD=AP*((COS(60*R))**0.25)
      RAD=ASIN((SIN(60*R)/RI))
      PPD=(((SIN(RAD - 60*R))**2)/((SIN(RAD + 60*R))**2))
      PAPD=(((TAN(RAD - 60*R))**2)/((TAN(RAD + 60*R))**2))
      PD=0.5*(PPD + PAPD)

C Calculate the absorption at current angle of incidence.
      A=AP*((COS(AI*R))**0.25)

C Finally calculate the transmission absorptance product
      TA=TE*A + TE*(1.-A)*((AD*PD)/(1.-(1.-AD)*PD))

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
      WRITE(ITU,*) 'Trace output for subroutine TRAP'
      WRITE(ITU,*) 'Calculating the trans-absorp product'
      WRITE(ITU,*) 'For CMP72C'
      WRITE(ITU,*) 'AoI = ',AI
      WRITE(ITU,*) 'RA  = ',RA
      WRITE(ITU,*) 'PP & PAP  = ',PP,PAP
      WRITE(ITU,*) 'T & TE = ',T,TE
      WRITE(ITU,*) 'Props at 60 Deg'
      WRITE(ITU,*) 'AD, RAD,PPD,PAPD,PD',AD,RAD,PPD,PAPD,PD
      WRITE(ITU,*) 'Apsorption at AI = ',A
      WRITE(ITU,*) 'TA prod = ',TA
      ENDIF                

      RETURN
      END
