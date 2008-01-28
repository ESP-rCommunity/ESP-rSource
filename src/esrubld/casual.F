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

C   MZCASI: Returns total convective and radiant casual gains.
C   INTLUM: Timestep determination of controled casual gains.
C   DWINVT: Returns the visible transmission of a TMC window.
C   HUNTA:  See if current hour has scheduled casual gains to control.
C   VRTILM: Calculate the illum (Ev) on a unobstructed vert surface.

C ******************** MZCASI ********************************
C MZCASI sets the present and future time-row total convective and
C radiant casual gains.  Any control action (e.g. occupant behaviour, 
C lighting)is applied here.

C Returned variables are:
C QCASRT - summation of the present and future time-row total radiant
C          injections.
C QCASCT - summation of the present and future time-row total convective
C          injections.
C QCASR  - radiant gain for present (1) and future (2) time step.
C QCASC  - convective gain for present (1) and future (2) time step.
C ZPWRR  - zone real power consumption total.
C ZPWRQ  - zone reactive power total
C QR     - real power for present (1) and future (2) time step.
C QP     - reactive power for present (1) and future (2) time step.
C QLAT   - latent load for present (1) and future (2) time step.

      subroutine mzcasi(icomp)
#include "building.h"
#include "tdf2.h"
#include "sbem.h"
#include "power.h"

      common/tc/itc,icnt
      common/trc/itrc
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      common/outin/iuout,iuin

      common/btime/btimep,btimef

      COMMON/PREC1/SNA(MCOM,MS),ZOA(MCOM)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)

      common/PREC17/ZBASEA(MCOM),IBASES(MCOM,12),IUZBASEA(MCOM),
     &  IZBASELIST(MCOM)

C Zone lighting and power commons.
      COMMON/ELECFLG/IELF(MCOM)
      COMMON/ENETZONE/PEZON(MCOM,MGTY),QEZON(MCOM,MGTY)

C Zone casual gain control data.
      COMMON/CGCIN2/NCGTC(MCOM,3),NLITZ(MCOM),IDFST(MCOM,MLCOM),
     & CGX(MCOM,MLCOM,MDF),CGY(MCOM,MLCOM,MDF),CGH(MCOM,MLCOM,MDF),
     & UX(MCOM,MLCOM,MDF),UY(MCOM,MLCOM,MDF),UH(MCOM,MLCOM,MDF),
     & SETPT(MCOM,MLCOM),SYSPER(MCOM,MLCOM),SOFFLL(MCOM,MLCOM),
     & IOFFDT(MCOM,MLCOM),SMLOUT(MCOM,MLCOM),SMEOUT(MCOM,MLCOM)
      COMMON/CGCIN3/ICGCS(MCOM),ICGCF(MCOM),
     & ICGCFL(MCOM,MLCOM),SPELEC(MCOM,MLCOM),SLOPEM(MCOM,MLCOM)

C Switch off delay time memory common block
      COMMON/SOFDT/IDTM(MCOM,MLCOM)

C Temporal db commons.
      COMMON/TDFFLG2/IALLCLM,ICASUAL(MCOM),IZIVENT(MCOM),IRAIRVL(MCOM),
     &       ISETPTT(MCOM),ICTLSTA(MCOM),ISKYLUX,IDBTEXT,IWINDVL,
     &       IWINDDR,IRELHUM,IDIFHSL,IDIRSOL,IGLOHSL,IOBJVEL,IOBJDIR,
     &       ISKYTMP,IGLOVRT,ICASUAL3(MCOM),IACTIV(MCOM)

C Variables for weekdays, and weekends.
C Assume: Mon=1, Tue=2, Wed=3, Thu=4, Fri=5, Sat=6, Sun=7
      common/wkdtyp/idwe1,idwe2,wkd1,wkd2

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/COE34/QCASRT(MCOM),QCASCT
      COMMON/COE34D/QCASR(2),QCASC(2)
      COMMON/COE34S/QCCS(MCOM),QCRS(MCOM)
      COMMON/COE34L/QLAT(MCOM)
      COMMON/GRSD130/QCASRS(MCOM)

C Common for occupant/lighting/equipment sensible convective, sensible
C radiant and latent W for future and present in each zone.
      real ctlperocupc,ctlperocupr,ctlperocupl ! occupant after control applied
      real ctlperlightc,ctlperlightr,ctlperlightl ! lighting after control applied
      real ctlequipc,ctlequipr,ctlequipl ! equipment after control applied
      common/applyctl/ctlperocupc(MCOM,2),ctlperocupr(MCOM,2),
     &  ctlperocupl(MCOM,2),ctlperlightc(MCOM,2),ctlperlightr(MCOM,2),
     &  ctlperlightl(MCOM,2),ctlequipc(MCOM,2),ctlequipr(MCOM,2),
     &  ctlequipl(MCOM,2) 

C Common for HOT3000 electrical-network -> casual-gain coupling
C Symopsys: E_to_G_coupling: boolian indicating coupling exists
C           Coupling_gain_info(i,j,k,l): array containing data
C           for gain k on daytype j for zone i. 
C           l = 1 -> start hour (0->24)
C           l = 2 -> end hour (0->24)
C           l = 3 -> sensible load (fraction of non-hvac elec load
C           l = 4 -> latent load (fraction of non-hvac elec load
C           l = 5 -> radiative feaction (0->1)
C           l = 6 -> convective fraction (0->1)
C
C - Alex Feb. 20 2004
      COMMON/CasCou/iNumb_Gain_coupling(MCOM,3),
     &     Coupling_gain_info(MCOM,3,MC,6)

C Common to store casual/radiative gain injections 
C on a zone-by-zone basis. Note: the latent gain 
C common variable has been created is a place-holder
C for storing this data in the future, but is not
C presently used
      COMMON/CasDat/Cas_Rad_dat(MCOM),Cas_Con_dat(MCOM),
     &     Cas_Lat_dat(MCOM)

C Casual gain data.
C NCGPER - number of periods for each gain (zone, day type, gain type)
C TCGS - start time of gain (zone, day type, gain type, period)
C CGSENR - sensible gain radiant portion (zone, day type, gain type, period)
C CGSENC - sensible gain convective portion (zone, day type, gain type, period)
C CGLAT - latent gain (zone, day type, gain type, period)
C CGCTL - gain control fraction (zone, present/future, gain type)
C ICGCTL - pointer to controlled gain type.
      common/CASGNS/NCGPER(MCOM,MDTY,MGTY),TCGS(MCOM,MDTY,MGTY,MGPER),
     &        CGSENC(MCOM,MDTY,MGTY,MGPER),CGSENR(MCOM,MDTY,MGTY,MGPER),
     &        CGLAT(MCOM,MDTY,MGTY,MGPER),CGCTL(MCOM,2,MGTY)
      COMMON/LIGHTS/ICGCTL(MCOM)
      common/CASGNSE/ELQ(MCOM,MDTY,MGTY,MGPER,MPHAS),
     &               ELR(MCOM,MDTY,MGTY,MGPER,MPHAS)

C Variables used in trace.
      common/CASGNTRC/QP(MCOM,2,MC,MPHAS),RP(MCOM,2,MC,MPHAS)

C Descriptive label for a zone load or casual gain types.
      common/loadlabel/lodlabel(mcom,7)

C Adaptive comfort commons
      COMMON/ADPCOM/PREVSTATWIN,PREVSTATFAN,PREVSTATLIGHT,TOTPWR(MCOM)

C.........START: USED FOR ACTIVITY TDF DATA.....................
C TDFINT are ranges allowed for integer fields in the instance.
      COMMON/TDFINT/ITMAR(MIT,MGAI),ITMIN(MIT,MGAI),ITMAX(MIT,MGAI)

      COMMON/TDFREL/TMAR(MIT,MGAR),TMIN(MIT,MGAR),TMAX(MIT,MGAR)
C Using the TOTOCCUP as a shortcut for the total occupant gains
C that are taken from the activities temporal data
      REAL QTOTOCCUP,QOCCCASC,QOCCCASR,QOCCLATN,QTOTEQUIP,QEQUIPCASC
      REAL QEQUIPCASR,QEQUIPLATN,QTOTLIGHT,QLIGHTCASC,QLIGHTCASR
      REAL QLIGHTLATN
      INTEGER ILIGHT_IN_SBEMDB
C.........END: USED FOR ACTIVITY TDF DATA.....................

      REAL QCASLT  ! to hold the summation of pres + future latent

      DIMENSION QLATN(2),VAL(MBITS+2)

      CHARACTER outs*124, lodlabel*6
      character*10 wkd1, wkd2, DSTR
      logical traceok,closea
      REAL PEZON,QEZON

C Declation of heat gain from h3k models
c      REAL H3K_HeatGainsFromModels(ICOMP)
      REAL H3KGains
C     Number of electric-load->gain couplings per zone,
      INTEGER iNumb_Gain_coupling
C     Data fro each coupling
      REAL Coupling_gain_info
C     Flag indicating type of present day (weekday/saturday/sunday)
      INTEGER iDay_type
C     HOT3000 storage arrays
      REAL Cas_Rad_dat, Cas_Con_dat, Cas_Lat_dat

C     Flag indicating that function Elec_Net_Load_Calc(?) should 
C     return the occupant-driven electrical load (excluding 
C     HVAC equipment)
      INTEGER occupant_load 
      PARAMETER (occupant_load = 3)
C.....Named constnats for timerows
      integer iPresent, iFuture
      parameter (iPresent=1, iFuture=2) 

C Trace output ?
      traceok = .true.
      IF(ITC.LE.0.OR.NSINC.LT.ITC)traceok = .false.
      IF(ITRACE(12).EQ.0.OR.NSINC.GT.ITCF.OR.
     &   IZNTRC(ICOMP).NE.1)traceok = .false.
      if(traceok)then
        call edisp(itu,' ')
        write(outs,9995)ICNT,ICOMP
 9995   format(' Subroutine MZCASI Trace output',I4,5X,'Zone',I3)
        call edisp(itu,outs)
        ICNT=ICNT+1
        CALL DAYCLK(IDYP,BTIMEF,ITU)
      endif

C Update present and zero the future zone power values. 
      DO 4 IPHS=1,MPHAS
        DO 5 ICAS=1,MC
          RP(ICOMP,1,ICAS,IPHS)=RP(ICOMP,2,ICAS,IPHS)
          QP(ICOMP,1,ICAS,IPHS)=QP(ICOMP,2,ICAS,IPHS)
          RP(ICOMP,2,ICAS,IPHS)=0.
          QP(ICOMP,2,ICAS,IPHS)=0.
  5     CONTINUE
  4   CONTINUE

C Zero zone power loads and update present control fractions.
      DO 9 ITY=1,MGTY
        PEZON(ICOMP,ITY)=0.
        QEZON(ICOMP,ITY)=0.
        CGCTL(ICOMP,1,ITY)=CGCTL(ICOMP,2,ITY)
        CGCTL(ICOMP,2,ITY)=1.0
  9   CONTINUE

C Update present values.
      QCASC(1)=QCCS(ICOMP)
      QCASR(1)=QCRS(ICOMP)
      QLATN(1)=QLAT(ICOMP)
      QCASC(2)=0.
      QCASR(2)=0.
      QLATN(2)=0.

C Recover definitions for current day type and timeF.
      if (IHRF.eq.1) then 
        ID=IDWF
      else
        ID=IDWP
      endif
      if (btimef.gt.24.) then
        write (6,*) 'btimef.gt.24'
        btimef=btimef-24.
        ID=IDWF
      endif
      DSTR='       '
      if(ID.EQ.IDWE1)then
        DSTR=wkd1
        IDAY=2
      elseif(ID.EQ.IDWE2)then
        DSTR=wkd2
        IDAY=3
      else
        DSTR='weekdays'
        IDAY=1
      endif
 
C Set controlled gain type.
      icgctl(icomp)=ncgtc(icomp,iday)
 
C Determine future timerow casual gain multiplier CGCTL(2,ICGCTL(ICOMP)).
      if (ICGC(ICOMP).EQ.1) then
        if (NCGTC(ICOMP,IDAY).NE.0) then

C Set switch-off delay time memory to "zero" before start of the
C new day control period:
          IT=int(btimef)
          if (IT.EQ.ICGCS(ICOMP)) then
            do 20 N=1,NLITZ(ICOMP)
              IDTM(ICOMP,N)=0
20          continue
          endif
          IPOINT=0
          if (IT.GT.ICGCS(ICOMP).AND.IT.LE.ICGCF(ICOMP)) IPOINT=1
          if (IPOINT.NE.0) then
            call INTLUM(ICOMP,FRAC)
            CGCTL(ICOMP,2,ICGCTL(ICOMP))=FRAC
          endif
        endif
      endif

C Update SHOCC-calculated equipment loads.
      call updateequipment(icomp)

C Use data from one of three sources.
C NB tdf and time step file should have electrical data added to the 
C   definition.
      if(ICASUAL(icomp).ne.0)then

C Data in temporal database (1 column of gains). Also
C instanciate the future value of equipment casual gain so that
C it can be used within MZLS3. Equipment is an arbitrary choice.
        IFOC=ICASUAL(icomp)
        CALL RCTDFB(itrc,btimef,VAL,ISD,IFOC,IER)
        QCASC(2)=VAL(ISD)
        ctlperocupc(icomp,2)=0.0
        ctlperlightc(icomp,2)=0.0
        ctlequipc(icomp,2)=VAL(ISD)

        QCASR(2)=VAL(ISD+1)
        ctlperocupr(icomp,2)=0.0
        ctlperlightr(icomp,2)=0.0
        ctlequipr(icomp,2)=VAL(ISD+1)

        QLATN(2)=VAL(ISD+2)
        ctlperocupl(icomp,2)=0.0
        ctlperlightl(icomp,2)=0.0
        ctlequipl(icomp,2)=VAL(ISD+2)
        if(traceok)then
          write(outs,*)'casual gains from temporal db=',
     &          VAL(ISD),VAL(ISD+1),VAL(ISD+2)
          call edisp(itu,outs)
        endif
      elseif(ICASUAL3(icomp).ne.0)then

C Data in temporal database (3 columns of gains). Also
C instanciate the future value of each casual gain so that
C it can be used within MZLS3.
        IFOC=ICASUAL3(icomp)
        CALL RCTDFB(itrc,btimef,VAL,ISD,IFOC,IER)
        QCASC(2)=VAL(ISD)+VAL(ISD+3)+VAL(ISD+6)
        ctlperocupc(icomp,2)=VAL(ISD)
        ctlperlightc(icomp,2)=VAL(ISD+3)
        ctlequipc(icomp,2)=VAL(ISD+6)

        QCASR(2)=VAL(ISD+1)+VAL(ISD+4)+VAL(ISD+7)
        ctlperocupr(icomp,2)=VAL(ISD+1)
        ctlperlightr(icomp,2)=VAL(ISD+4)
        ctlequipr(icomp,2)=VAL(ISD+7)

        QLATN(2)=VAL(ISD+2)+VAL(ISD+5)+VAL(ISD+8)
        ctlperocupl(icomp,2)=VAL(ISD+2)
        ctlperlightl(icomp,2)=VAL(ISD+5)
        ctlequipl(icomp,2)=VAL(ISD+8)
        if(traceok)then
          write(outs,*)'separate casual gains from temporal db=',
     &        VAL(ISD),VAL(ISD+1),VAL(ISD+2),VAL(ISD+3),VAL(ISD+4),
     &        VAL(ISD+5),VAL(ISD+6),VAL(ISD+7),VAL(ISD+8)
          call edisp(itu,outs)
        endif
      elseif(IACTIV(icomp).ne.0)then

C Data in temporal database.
C The calculations need to change in case the order of the 
C activity data type changes (in the tdf file).
        IFOC=IACTIV(icomp)
        CALL RCTDFB(itrc,btimef,VAL,ISD,IFOC,IER)

C Total occupancy gains are equal to: occupancy density*metabolic rate*fraction*
C Zone floor area
        QTOTOCCUP=TMAR(1,1)*ITMAR(1,2)*VAL(ISD)*ZBASEA(ICOMP)

C Get the casual sensible gains from occupants (using the percentage of 
C latent occupant gains here)
        QOCCCASC=(QTOTOCCUP-((QTOTOCCUP*ITMAR(1,3))/100))/2

C Convective and radiative occupant gains are the same at the moment (50%)
        QOCCCASR=QOCCCASC

C Using again the percentage of latent gains for the occupants' latent heat gains
        QOCCLATN=((QTOTOCCUP*ITMAR(1,3))/100)

C Total EQUIPMENT gains are equal to: equipment gains in w/m2*fraction*
C Zone floor area
        QTOTEQUIP=TMAR(1,4)*VAL(ISD+2)*ZBASEA(ICOMP)

C C Get the casual sensible gains from equipment (using the percentage of 
C latent equipment gains)
        QEQUIPCASC=(QTOTEQUIP-((QTOTEQUIP*ITMAR(1,5))/100))/2

C Convective and radiative equipment gains are the same at the moment (50%)
        QEQUIPCASR=QEQUIPCASC
        QEQUIPLATN=((QTOTEQUIP*ITMAR(1,5))/100)        
        
C Read information from ncm file. Need type of light and then a value
C taken from sbem.db1 will be combined with a TDF static data        
        CALL SBEMPR(IER)
        CALL RSBEM
        ILIGHT_IN_SBEMDB=ILITYP(ICOMP)

C Multiply lux levels (static tdf) with fraction schedule and 
C a value (in w/m2/100lux) taken from ncm file for the specific type of lights
        QTOTLIGHT=
     &  PDRL(ILIGHT_IN_SBEMDB,2)*(ITMAR(1,6)/100)*VAL(ISD+1)*
     &  ZBASEA(ICOMP)

C Convective and radiative equipment gains are the same at the moment (50%)
        QLIGHTCASC=QTOTLIGHT/2
        QLIGHTCASR=QLIGHTCASC

C For lighting heat gains assume latent gains are zero
        QLIGHTLATN=0
C       QTOTGAIN=QTOTOCCUP+QTOTEQUIP+QTOTLIGHT

<<<<<<< .working
	QCASC(2)=QOCCCASC+QEQUIPCASC+QLIGHTCASC
        QCASR(2)=QOCCCASR+QEQUIPCASR+QLIGHTCASR
=======
        QCASC(2)=QOCCCASC+QEQUIPCASC+QLIGHTCASC
        QCASR(2)=QOCCCASR+QEQUIPCASR+QLIGHTCASR
>>>>>>> .merge-right.r2400
        QLATN(2)=QOCCLATN+QEQUIPLATN+QLIGHTLATN

C Instanciate the future value of each casual gain so that
C it can be used within MZLS3.
        ctlperocupc(icomp,2)=QOCCCASC
        ctlperlightc(icomp,2)=QLIGHTCASC
        ctlequipc(icomp,2)=QEQUIPCASC
        ctlperocupr(icomp,2)=QOCCCASR
        ctlperlightr(icomp,2)=QLIGHTCASR
        ctlequipr(icomp,2)=QEQUIPCASR
        ctlperocupl(icomp,2)=QOCCLATN
        ctlperlightl(icomp,2)=QLIGHTLATN
        ctlequipl(icomp,2)=QEQUIPLATN
        if(traceok)then
          write(outs,*)'TOTAL OCCUPANT gains from temporal db=',
     &      QTOTOCCUP,QOCCCASC,QOCCCASR,QOCCLATN
          call edisp(itu,outs)
          write(outs,*)'TOTAL LIGHTING gains from temporal db=',
     &      QTOTLIGHT,QLIGHTCASC,QLIGHTCASR,QLIGHTLATN
          call edisp(itu,outs)
          write(outs,*)'TOTAL EQUIPMENT gains from temporal db=',
     &      QTOTEQUIP,QEQUIPCASC,QEQUIPCASR,QEQUIPLATN
          call edisp(itu,outs)
        endif
      else

C Data in operations file.
C For each casual gain type set gain for current period.
        do 10 IGN=1,MGTY

C First initialize SHOCC commons to -1, i.e. assume no SHOCC-calulated loads
C the following 'fetchloads' subroutine will override '-1' if SHOCC
C does indeed calculate loads.
          shcon=-1.
          shrad=-1.
          shlat=-1.
                    
          call fetchloads(icomp,lodlabel(icomp,ign),
     &      len(lodlabel(icomp,ign)),shcon,shrad,shlat)

C Calculate the current period id.
          IPER=NCGPER(ICOMP,IDAY,IGN)
          do 30 I=1,NCGPER(ICOMP,IDAY,IGN)
            if (BTIMEF.le.TCGS(ICOMP,IDAY,IGN,I+1).and.
     &          BTIMEF.gt.TCGS(ICOMP,IDAY,IGN,I)) then
              IPER=I
            endif
 30       continue

C Set future casual gain values if there is periods defined.
          if(IPER.gt.0)then
            if(shcon.lt.0.) then
              QCASC(2)=QCASC(2)+CGSENC(ICOMP,IDAY,IGN,IPER)*
     &                 CGCTL(ICOMP,2,IGN)
              QCASR(2)=QCASR(2)+CGSENR(ICOMP,IDAY,IGN,IPER)*
     &                 CGCTL(ICOMP,2,IGN)
              QLATN(2)=QLATN(2)+CGLAT(ICOMP,IDAY,IGN,IPER)*
     &                 CGCTL(ICOMP,2,IGN)
            else

C Instead of using fracf, add-on shcon,shrad,shlat for SHOCC.
              QCASC(2)=QCASC(2)+shcon
              QCASR(2)=QCASR(2)+shrad
              QLATN(2)=QLATN(2)+shlat

C Quick fix: cgctl variable is used within eletrical network
C so if SHOCC load, then cgctl equals shcon/CGCSENC ratio.
              CGCTL(ICOMP,2,IGN)=shcon/CGSENC(ICOMP,IDAY,IGN,IPER)
            endif

C Debug...
            if(traceok)then
C              write(itu,*) 'match ',DSTR,BTIMEF,IPER,IGN,IDAY,ICOMP
C              write(itu,*) 'Time    Zone   Period   GainSen'
C              write(itu,*) BTIMEF,ICOMP,IPER,CGSENC(ICOMP,IDAY,IGN,IPER)
C              write(itu,*) QCASC,QCASR,QLATN,' ctl:',CGCTL(ICOMP,1,1),
C     &          CGCTL(ICOMP,2,1),CGCTL(ICOMP,1,2),CGCTL(ICOMP,2,2)  
            endif

C Set future electrical loads.
            if (BTIMEF.le.TCGS(ICOMP,IDAY,IGN,IPER+1).and.
     &          BTIMEF.gt.TCGS(ICOMP,IDAY,IGN,IPER)) then
              IF(IELF(ICOMP).GT.0) THEN

C loop through power consumption for each phase.
                DO 75 IPH=1,MPHAS
                  PEZON(ICOMP,IGN)=PEZON(ICOMP,IGN)-
     &              ELQ(ICOMP,IDAY,IGN,IPER,IPH)*CGCTL(ICOMP,2,IGN)
                  QEZON(ICOMP,IGN)=QEZON(ICOMP,IGN)-
     &              ELR(ICOMP,IDAY,IGN,IPER,IPH)*CGCTL(ICOMP,2,IGN)

C Keep track of electrical loads for trace.
                  RP(ICOMP,2,IGN,IPH)=RP(ICOMP,2,IGN,IPH)+
     &              ELQ(ICOMP,IDAY,IGN,IPER,IPH)*CGCTL(ICOMP,2,IGN)
                  QP(ICOMP,2,IGN,IPH)=QP(ICOMP,2,IGN,IPH)+
     &              ELR(ICOMP,IDAY,IGN,IPER,IPH)*CGCTL(ICOMP,2,IGN)
  75            CONTINUE
              ENDIF 
            endif
          endif
 10     continue
      endif

C If adaptive comfort algorithm is active add sensible heat gains when on
      call eclose(PREVSTATFAN,1.00,0.01,closea)
      IF(CLOSEA)THEN

C Debug...
C        write(6,*)"before addition",QCASC(2)
        QCASC(2)=QCASC(2)+TOTPWR(ICOMP)

C Debug...
C        write(6,*)"after addition",QCASC(2)
      ENDIF

C BEGIN NRCan casual-gain -> electrical coupling code.
C
C ** NOTE: electric-load -> latent couplings 
C are presently not supported. However, if they were to be implemented, 
C code similar to below would have to be added to moistr.F, subsys.F and 
C drylink.F in the esrubld directory, AND NOT IN THIS FILE!!!

C For the present zone, loop through all gains associated
C with the casual-gain->electrical load coupling

C Common for HOT3000 electrical-network -> casual-gain coupling
C Symopsys: E_to_G_coupling: boolean indicating coupling exists
C           Coupling_gain_info(i,j,k,l): array containing data
C           for gain k on daytype j for zone i. 
C           l = 1 -> start hour (0->24)
C           l = 2 -> end hour (0->24)
C           l = 3 -> sensible load (fraction of non-hvac elec load)
C           l = 4 -> latent load (fraction of non-hvac elec load)
C           l = 5 -> radiative feaction (0->1)
C           l = 6 -> convective fraction (0->1)
C
C Initialize total electric load - related gains
      Total_elec_CON_gain = 0. ! (W) 
      Total_elec_RAD_gain = 0. ! (W)
      DO ii = 1, iNumb_Gain_coupling(icomp,iDay)
C Check to see if the future  building time is between the start hour and            
C the end hour of the gain
        start_hour = Coupling_gain_info(icomp,iDay,ii,1) ! start hour 0-24)
        stop_hour  = Coupling_gain_info(icomp,iDay,ii,2) ! end hour (0-24)
        if ( ( btimef .gt. start_hour ) 
     &          .and. ( btimef .le. stop_hour ) )then
C gain applies to future time. Get total non-hvac electric load (W_elec_nonHVAC, W)
          W_electric_nonHVAC =  Elec_Net_Load_Calc(occupant_load) ! W
C get data for gain coupling:
          Sens_load_fraction = 
     &             Coupling_gain_info(icomp,iDay,ii,3) ! (0->1)
          radiant_load_fraction =  
     &             Coupling_gain_info(icomp,iDay,ii,5) ! (0->1)
          convective_load_fraction =  
     &             Coupling_gain_info(icomp,iDay,ii,6) ! (0->1)        
C Convective casual gains (W)
          Total_elec_CON_gain = Total_elec_CON_gain 
     &             + sens_load_fraction 
     &             * convective_load_fraction 
     &             * W_electric_nonHVAC
C Radiative casual gains (W)
          Total_elec_RAD_gain = Total_elec_RAD_gain 
     &             + sens_load_fraction 
     &             * radiant_load_fraction 
     &             * W_electric_nonHVAC

        endif          
      ENDDO


C NOTE: add radiant & convective gains from occupant-driven
C electric loads to ESP-r common blocks QCASR and QCASC.
C These data are actually used to calculate the heat injection
C into the building.
C
C Also append convective and radiant gains associated with
C heat losses from plant components.
C 
C 
      QCASC(2)= QCASC(2) + Total_elec_CON_gain
     &                   + fPltZone_Convective_Gain(iComp,iFuture) !(W)
      QCASR(2)= QCASR(2) + Total_elec_RAD_gain
     &                   + fPltZone_Radiant_Gain(iComp,iFuture)    !(W)

C STORE future row casual and radiant gains
      Cas_Con_dat(icomp) = QCASC(2)  ! W
      Cas_Rad_dat(icomp) = QCASR(2)  ! W

C Remember future values for next timestep.
      QCCS(ICOMP)=QCASC(2)
      QCRS(ICOMP)=QCASR(2)
      QLAT(ICOMP)=QLATN(2)
    
C If first timestep set previous values to current.
      if(NSINC.eq.1)then
        QCASC(1)=QCASC(2)
        QCASR(1)=QCASR(2)
        QLATN(1)=QLATN(2)
      endif

C Sum present and future time rows and convert radiant casual 
C variables to W/m^2.
      QCASCT=QCASC(1)+QCASC(2)
      QCASRT(icomp)=(QCASR(1)+QCASR(2))/ZOA(ICOMP)
      QCASR(1)=QCASR(1)/ZOA(ICOMP)
      QCASR(2)=QCASR(2)/ZOA(ICOMP)
      QCASRS(ICOMP)=QCASR(2)
      QCASLT=QLATN(1)+QLATN(2)

C Trace output ?
      if(.NOT.traceok)goto 999

C Output computed results.
      call edisp(itu,' ')
      write(outs,'(A,A)')' Casual gain summary for ',DSTR
      call edisp(itu,outs)
      call edisp(itu,' ')
      call edisp(itu,' Control fraction for each type:')
      write(outs,'(a,10f7.4)') 'Present: ',(CGCTL(icomp,1,I),I=1,MGTY)
      call edisp(itu,outs)
      write(outs,'(a,10f7.4)') 'Future : ',(CGCTL(icomp,2,I),I=1,MGTY)
      call edisp(itu,outs)
      call edisp(itu,' ')
      write(outs,'(15X,A)')
     & '|      Radiant       |    Convection      |    Latent         '
      call edisp(itu,outs)
      write(outs,'(15X,A)')
     & '|   Present   Future |  Present   Future  |  Present    Future'
      call edisp(itu,outs)

C Display radiant contribution as Watts.
      RWP=QCASR(1)*ZOA(ICOMP)
      RWF=QCASR(2)*ZOA(ICOMP)
      RW=QCASRT(icomp)*ZOA(ICOMP)
      write (outs,9994) RWP,RWF,QCASC(1),QCASC(2),QLATN(1),QLATN(2)
 9994 format(' Total gain    |',2F10.2,'|',2F10.2,'|',2F10.2)
      call edisp(itu,outs)
      write (outs,9996) RW,QCASCT,QCASLT
 9996 format(' Summation     |   ',F10.2,'W      |   ',F10.2,
     &                                   'W      |   ',F10.2)
      call edisp(itu,outs)

C Output the electrical results for the zone
      if(ielf(icomp).gt.0) then
        call edisp(itu,' ')
        write(outs,'(a,1x,i3,1x,i2)') ' Zone: ',icomp,ielf(icomp)
        call edisp(itu,outs)
        write(outs,'(A,A)')' Electrical load summary for ',DSTR
        call edisp(itu,outs)
        call edisp(itu,'Each Phase  Real Power         Reactive Power')
        call edisp(itu,'          |Present|Future|    |Present|Future|')
        DO 777 K=1,MGTY
          write(outs,'(6(2F7.0,1x))') 
     &              (RP(ICOMP,1,K,J),RP(ICOMP,2,K,J),
     &               QP(ICOMP,1,K,J),QP(ICOMP,2,K,J),J=1,MPHAS)
          call edisp(itu,outs) 
  777   CONTINUE    
      endif

  999 RETURN

 1000 call edisp(iuout,' MZCASI: casual gains file read error.')
      goto 999
      END

C ******************** INTLUM ********************
C INTLUM Calculate the switched level of casual gains (normally associated
C with artificial lighting) when controlled on the basis of available
C natural light.

C Variables:

C  SAZI : solar azimuth
C  SALT : solar altitude
C  QDIR : direct normal irradiance
C  QDIF : diffuse horizontal irradiance
C  SETPT: set point illumination
C  ICGCFL: control law flag (-1,0,1,2,3,4 and 31, 32 as below)
C  NEWT : total number of external default windows in the zone
C  NDF  : number of light sensors (daylight factors) for each external zone window
C  DFDAT: the daylight factors (CIE standard overcast sky)
C  GAZI &
C  GELV: window edge azimuthal and elevational angles.
C  FRAC : fraction of casual gain (returned value)

      SUBROUTINE INTLUM(ICOMP,FRAC)
#include "building.h"

      COMMON/BTIME/BTIMEP,BTIMEF

      common/uhome/upath
      common/filep/ifil
      COMMON/TC/ITC,ICNT
      common/trc/itrc
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SHOUT/ICOUT
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/SUNPOS/SAZI,SALT,ISUNUP
C      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

      character CTYPE*4
      real gversion
      integer igupgrade
      COMMON/G0/CTYPE(MCOM),gversion(MCOM),igupgrade
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      common/c2/lsnam,nccode(mcom),lproj(mcom),lgeom(mcom),
     &          lshad(mcom),lthrm(mcom),indutl(mcom),lutil(mcom)
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/precz/zname(MCOM),zdesc(MCOM)

C Zone casual gain control data.
      COMMON/CGCIN2/NCGTC(MCOM,3),NLITZ(MCOM),IDFST(MCOM,MLCOM),
     & CGX(MCOM,MLCOM,MDF),CGY(MCOM,MLCOM,MDF),CGH(MCOM,MLCOM,MDF),
     & UX(MCOM,MLCOM,MDF),UY(MCOM,MLCOM,MDF),UH(MCOM,MLCOM,MDF),
     & SETPT(MCOM,MLCOM),SYSPER(MCOM,MLCOM),SOFFLL(MCOM,MLCOM),
     & IOFFDT(MCOM,MLCOM),SMLOUT(MCOM,MLCOM),SMEOUT(MCOM,MLCOM)
      COMMON/CGCIN3/ICGCS(MCOM),ICGCF(MCOM),
     & ICGCFL(MCOM,MLCOM),SPELEC(MCOM,MLCOM),SLOPEM(MCOM,MLCOM)

C Daylight coefficient common block data definition:
      COMMON/DCOEF/NDCFP,NDCFS(MDCFP),DCOEF(MDCFP,MDCFS,MDCFV),
     & IDCFID(MCOM,MLCOM,MDF)

C Daylight factor data (see dayfac.f).
      COMMON/DAYF/NDF(MCOM,MLCOM),DFDAT(MCOM,MLCOM,MGT,MDF),
     & AZIJL(MCOM,MGT),SUMDF(MCOM,MLCOM,MDF),NUDWIN(MCOM)

      COMMON/ADJC2/DIRT(MCOM,MS),DIFT(MCOM,MS),AIRT(MCOM,MS)
      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)
      COMMON/D7/T1,T2,P1,P2
C TMC data
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)

C TMC blind control flag
      COMMON/TMCB4/IBOFOT(MCOM,MS),NBPONT(MCOM,MS),ECRAT(MCOM,MS)

C Construction information
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      
C Hunt's information.
      COMMON/HUNT/FIRSTL(MCOM),RANDN(MCOM)

C Switch off delay time memory common block
      COMMON/SOFDT/IDTM(MCOM,MLCOM)
      common/sus/issny

C Illuminance database.
      common/ILDB/NDBENT,DBILLA(200,5),DBILLS(200,MLCOM,MDF)

      DIMENSION selum(MLCOM,MDF),alt(8),SENTMP(MLCOM,MDF)
      DIMENSION QDFTMP(MLCOM,MDF),QDRTMP(MLCOM,MDF),Yb(MLCOM,MDF)
      DIMENSION Aa(MLCOM,MDF),Ab(MLCOM,MDF),Ac(MLCOM,MDF),Ya(MLCOM,MDF)
      DIMENSION zelum(MLCOM)
      DIMENSION ZFRAC(MLCOM)
      DIMENSION XPL(MV),YPL(MV),ZPL(MV)
      CHARACTER*25 CSTR
      character outs*124,doit*124,outstr*124,cfgroot*24
      character*72 lsnam,lproj,lgeom,lshad,lutil,lthrm

      character*72 LCFGF,LTMPFL,LTMPFLA,upath,OUTCMD
      character zname*12,zdesc*64

      logical nog,close,closea,closeb,closec,closed,closee,closef
      logical closeg,closeh,closei,instart,neglum,XST
      logical bInStartup
      logical newgeo  ! to use for testing if new/old geometry file.

C Definition of sun altitude for daylight coefficient method:
      data alt/6.,18.,30.,42.,54.,66.,78.,90./

      iunit=ifil+1
      newgeo=.false.  ! assume older format geometry.

C Trace output ?
      IFT=1
      IF(ITC.LE.0.OR.NSINC.LT.ITC)IFT=0
      IF(ITRACE(12).EQ.0.OR.NSINC.GT.ITCF.OR.IZNTRC(ICOMP).NE.1)IFT=0
      IF(IFT.EQ.1)THEN
        write(outs,9998)ICNT,ICOMP,zname(icomp)
 9998   format(' Subroutine INTLUM  Trace output',I4,' Zone',I3,1x,a)
        call edisp(itu,outs)
        ICNT=ICNT+1
        CALL DAYCLK(IDYP,BTIMEF,ITU)
        write(outs,'(A,A)')' Light|Sens|sun |via   |daylight',
     &        '|visib|sun/dir|sky/dif|sensed|set  |ext vert'
        call edisp(itu,outs)
        write(outs,'(A,A)')'  zone|num.|seen|window|factor% ',
     &        '|trnsm|contrib|contrib|lux   |point|ilum(Ev)'
        call edisp(itu,outs)
      ENDIF

C If esp pre-processor or user defined df then read in geometry data.
      nog=.false.
      do  1 iu=1,NLITZ(icomp)
        IF((IDFST(ICOMP,iu).EQ.1).or.(IDFST(ICOMP,iu).eq.2))nog=.true.
  1   continue
      if(nog)then
        call eclose(gversion(icomp),1.1,0.01,newgeo)
        if(newgeo)then
          call georead(iunit,lgeom(icomp),icomp,0,itrc,itu,ier)
        else
          call egomin(iunit,lgeom(icomp),icomp,0,itrc,itu,ier)
        endif
      endif

C Assume casual gains untouched and establish solar data for correct
C time-row.  For conversion from radians to degrees.
      CALL MZSINT(ICOMP,QDIR,QDIF)

      PI = 4.0 * ATAN(1.0)
      TODEG=180/PI
      RAD=PI/180.

C Transform direct normal to horizontal irradiance as only horizontal
C sensor aloved in methods 1 and 2:
      dir=QDIR*sin(SALT*rad)

C Calculate external horizontal unobstructed diffuse and direct
C illuminance:
        call LUMEFF(QDIF,QDIR,SALT,IDYP,skyeff,suneff)
        esun=dir*suneff
        esky=QDIF*skyeff

C Loop through all lighting zones
      FRAC=0.
      DO 9 N=1,NLITZ(ICOMP)
      ZFRAC(N)=0.

C If external sensor & surface (overload CGX) is exterior
C calculate lux.  If CGY (overloaded)=1. then calculate
C vertical unobstructed illum otherwise use horizontal illum.
      IF(IDFST(ICOMP,N).EQ.3)THEN
        J=INT(CGX(ICOMP,N,1))
        IF(J.EQ.0)J=1
        CALL SURADJ(ICOMP,J,IES,T,IZC,ISC,ICN,CSTR)
        IF(IES.EQ.0)THEN
          call eclose(CGY(ICOMP,N,1),1.00,0.01,close)
          IF(close)THEN
            CALL VRTILM(PAZI(ICOMP,J),QDIR,QDIF,SRADDO,SKYDIF)
            zelum(N)=SRADDO*suneff+SKYDIF*skyeff
            if(IFT.EQ.1)then
              write(outs,105)N,CGY(ICOMP,N,1),J,esun,esky,zelum(N),
     &              SETPT(ICOMP,N)
  105         format(I6,F5.1,' N/A ','  N/A  ',I5,'   N/A   ','  N/A ',
     &                       F8.1,F8.1,F7.1,F6.0,'   N/A   ')
              call edisp(itu,outs)
            endif
          ELSE
            zelum(N)=esun+esky
            if(IFT.EQ.1)then
              write(outs,106)N,CGY(ICOMP,N,1),J,esun,esky,zelum(N),
     &              SETPT(ICOMP,N)
  106         format(I6,F5.1,' N/A ','  N/A  ',I5,'   N/A   ','  N/A ',
     &                       F8.1,F8.1,F7.1,F6.0,'   N/A   ')
              call edisp(itu,outs)
            endif
          ENDIF
        ELSE
          Ev=0.
          IF(IFT.EQ.1)call edisp(itu,
     &      '  not external surface, sensor not allowed!')
        ENDIF

C Daylight coefficient method. Test if there is any diffuse 
C irradiation and if solar altitude is positive
C number. If not set photo-cell values to zero:
      ELSEIF(IDFST(ICOMP,N).EQ.5)THEN
        call eclose(QDIF,0.00,0.01,closea)
        if(SALT.LT.0.) closea=.true.
        if(closea) then
          suma=0.
          DO 41 I=1,NDF(ICOMP,N)
            selum(N,I)=0.

C Trace output:
            if(IFT.EQ.1)then
              write(outs,111)N,I,selum(N,I),SETPT(ICOMP,N)
              call edisp(itu,outs)
            endif
   41     CONTINUE
        else

C Calculate reference horizontal illuminance:
          call REFILL(QDIF,QDIR,SALT,SAZI,IDYP,refell)

C Loop through all TMC in the zone and determine blind and
C shuter control status. 
C If IBOFOT()=1, then blind/shutter is ON and used second stage
C daylight coefficients or if IBOFOT()=2 then linear interpolation
C on optical properties i.e. electrochromic glazing:
          NC=NCONST(ICOMP)
          DO 51 I=1,NC
            IF(ITMCFL(ICOMP,I).GT.0)THEN
              IF(IBOFOT(ICOMP,I).EQ.0)RATIO=1.0
              IF(IBOFOT(ICOMP,I).EQ.1)RATIO=0.0
              IF(IBOFOT(ICOMP,I).EQ.2)RATIO=ECRAT(ICOMP,I)
            ENDIF
51        CONTINUE

C Loop via all sensors for this lighting zone:
          suma=0.
          DO 40 J=1,NDF(ICOMP,N)

C Find out daylight coefficient set associated with current sensor: 
            IDCFP=IDCFID(ICOMP,N,J)

C Define starting angle for test of sky element comprising sun:
            ang0=4.0*PI

C Loop through all 145 sky patchs and calculate delta illuminance:
            sill = 0.0
            do 30 npatch = 1, 145
              dill=0.0

C Calculate sky patch reference luminance:
              call PATLUM(QDIF,QDIR,SALT,SAZI,IDYP,npatch,plv,ang)

C Find sky element with minimum angle between sun position and element.
C This identifies number of sky element for calculation of direct
C illuminance contribution:
              if(ang.lt.ang0)then
                nelem = npatch
                ang0 = ang
              endif

C Calculate luminance (cd/m^2) for given sky patch:
              plum=plv*esky/refell

C If inconsistency (i.e. error) in irradiance data - plum < 0 set it to 0:
              if(plum.le.0.0)then
                if((npatch.ge.1).and.(npatch.le.30))then
                  nzone = 1
                elseif((npatch.ge.31).and.(npatch.le.60))then
                  nzone = 2
                elseif((npatch.ge.61).and.(npatch.le.84))then
                  nzone = 3
                elseif((npatch.ge.85).and.(npatch.le.108))then
                  nzone = 4
                elseif((npatch.ge.109).and.(npatch.le.126))then
                  nzone = 5
                elseif((npatch.ge.127).and.(npatch.le.138))then
                  nzone = 6
                elseif((npatch.ge.139).and.(npatch.le.144))then
                  nzone = 7
                elseif(npatch.eq.145)then
                  nzone = 8
                endif
                neglum = .true.

C Use aproximate solution assuming uniform sky luminance distribution
C to estimate sky patch luminance:
                plum = esky/(2.0*PI*sin(alt(nzone)*rad))
              else
                neglum = .false.
              endif

C Calculate delta illuminance at sensor from given sky patch:
C Test for blind or linear interpolation and use appropriate
C daylight coefficient:
              CALL ECLOSE(RATIO,1.00,0.0001,closea)
              IF(CLOSEA)THEN
                DMIN=0.0
                DMAX=DCOEF(IDCFP,1,npatch)
              ELSE
                DMIN=DCOEF(IDCFP,2,npatch)
                DMAX=DCOEF(IDCFP,1,npatch)
              ENDIF

              DD1=DMIN + (DMAX - DMIN)*RATIO 
              dill=DD1*plum*(2.0*PI/145.0)
              sill = sill + dill

30          continue

C Give warnning about calculated negative luminance:
            if(neglum)then
              write (outs,'(a)') ' Sky patch luminance < = 0 occured '
              call edisp(iuout,outs)
C              write (outs,'(a)') ' Using uniform sky aproximation '
C              call edisp(iuout,outs)
              write (outs,'(a,I3,a,f6.1,a,f6.1,a,f4.1)') ' Day ',IDYP,
     &        ' Direct ',QDIR,' Diffuse ',QDIF,' Sun altitude ',SALT
              call edisp(iuout,outs)
            endif

C Check if any direct sun - if yes then calculate its contribution
C into sensor illuminance:
            call eclose(QDIR,0.00,0.01,closea)
            if(closea)then
              dircon=0.0
            else
              CALL ECLOSE(RATIO,1.00,0.0001,closea)
              IF(CLOSEA)THEN
                DMIN=0.0
                DMAX=DCOEF(IDCFP,1,nelem)
              ELSE
                DMIN=DCOEF(IDCFP,2,nelem)
                DMAX=DCOEF(IDCFP,1,nelem)
              ENDIF
              DD2=DMIN + (DMAX - DMIN)*RATIO
              dircon=DD2*QDIR*suneff
            endif 

C Define sensed illumination at given sensor:
            subT = sill + dircon
            suma=suma+subT

C Define "selum" only for Trace output - not used in calculation:
            selum(N,J)=subT

C Trace output:
            if(IFT.EQ.1)then
              write(outs,113)N,J,dircon,sill,selum(N,J),SETPT(ICOMP,N)
  113         format(I6,I5,' N/A ','  N/A  ','   N/A   ','  N/A ',
     &               F8.1,F8.1,F7.1,F6.0,'   N/A   ')
              call edisp(itu,outs)
            endif
40        CONTINUE
        endif

C Average illumination for this light zone (in case of more than one
C sensor in lighting zone):
        zelum(N)=suma/float(NDF(ICOMP,N))

C Time varying illuminances calculated by Radiance. Test if there
C is any diffuse irradiation and if solar altitude is positive
C number. Do not carry out lighting simulation if not, and if 
C in pre-calculation period (via call to bInStartup):
      ELSEIF(IDFST(ICOMP,N).EQ.4)THEN
        call eclose(QDIF,0.00,0.01,closea)
        instart=.false.
        if (bInStartup())then
          instart=.true.
        endif
C        IF((ISSNY.EQ.1.AND.IDYP.EQ.365.AND.IHRP.EQ.24.AND.ISDS.EQ.1)
C     &    .OR.(ISSNY.EQ.0.AND.(IDYP.GE.ISDS.OR.
C     &        (IDYP.EQ.ISDS-1.AND.IHRP.EQ.24))))instart=.false.
        if(closea.OR.(SALT.LT.0.).OR.(IDYP.LT.ISDS).OR.instart)then
          suma=0.
          DO 21 I=1,NDF(ICOMP,N)
            selum(N,I)=0.

C Trace output:
            if(IFT.EQ.1)then
              write(outs,111)N,I,selum(N,I),SETPT(ICOMP,N)
              call edisp(itu,outs)
            endif
   21     CONTINUE
        else

C Call RADIANCE only once for first lighting zone
C as all internal illuminances are determined now:
          IF (N.EQ.1) then

C Transfer solar azimuth into Radiance coordinate system
C where South is 0 degree and azimuth is measured + towards west:
            RSAZI=SAZI-180.
            
C Loop through all TMC in the zone and determine blind and
C shuter control status. 
C If IBOFOT()=1, then blind/shutter is ON.
            NC=NCONST(ICOMP)
            IBLIND = 0
            RATIO = 0.0
            DO 31 I=1,NC
              IF(ITMCFL(ICOMP,I).GT.0)THEN
                IF(IBOFOT(ICOMP,I).GT.0)THEN
                  IBLIND = 1
                  RATIO = 1.0
                ENDIF
              ENDIF
31          CONTINUE

C Check if sensor values already in illuminance database and set accuracies.
            ILLDBM=0
            AZIACC=5.
            ALTACC=5.
            QDRACC=5.

            write (66,*) ' ***  Solar data for this timestep *** '
            write (66,'(4f9.1,i3)') SALT,RSAZI,QDIR,QDIF,IBLIND

C Calculate differences.  Blind state MUST match.
C Check for no direct solar (effectively use dayfactor).
            if (QDIR.lt.QDRACC) then
              NMATCH=0
              do 311 ILZ=1,MLCOM
                do 3111 IDF=1,MDF
                  SENTMP(ILZ,IDF)=0.0
                  QDFTMP(ILZ,IDF)=0.0
 3111           continue
 311          continue
              do 310 IDB=1,NDBENT
                if ((DBILLA(IDB,3).lt.QDRACC).and.
     &              (IBLIND.eq.nint(DBILLA(IDB,5)))) then

                  write (66,*) ' Found a diffuse match: '
            write (66,*) 'Dir: ',DBILLA(IDB,3),' Blind: ',DBILLA(IDB,5)

                  NMATCH=NMATCH+1
                  do 321 ILZ=1,MLCOM
                    do 3211 IDF=1,MDF
                      SENTMP(ILZ,IDF)=SENTMP(ILZ,IDF)+
     &                                DBILLS(IDB,ILZ,IDF)
                      QDFTMP(ILZ,IDF)=QDFTMP(ILZ,IDF)+DBILLA(IDB,4)
 3211               continue
 321              continue
                endif
 310          continue
              if (NMATCH.gt.1) then
                ILLDBM=1
                do 322 ILZ=1,MLCOM
                  do 3221 IDF=1,MDF
                    if (QDFTMP(ILZ,IDF).gt.0.0) then

C Assume that the diffuse regresses to a straight line: y=mx
C The SENTMP and QDFTMP both need divided by NMATCH hence NMATCH
C cancels iff y=mx assumption holds.
                      SENTMP(ILZ,IDF)=SENTMP(ILZ,IDF)*QDIF
                      selum(ILZ,IDF)=SENTMP(ILZ,IDF)/QDFTMP(ILZ,IDF)
                    else
                      selum(ILZ,IDF)=0.
                    endif
 3221             continue
 322            continue

                write (66,*) 'Estimated illuminances:'
                write (66,'(9(f7.1,1x))') (selum(1,IDFT),IDFT=1,MDF)
                write (66,'(9(f7.1,1x))') (selum(2,IDFT),IDFT=1,MDF)
                write (66,'(9(f7.1,1x))') (selum(3,IDFT),IDFT=1,MDF)
                write (66,'(9(f7.1,1x))') (selum(4,IDFT),IDFT=1,MDF)
              endif
            else

C Check for similar sun position.
              NMATCH=0
              do 411 ILZ=1,MLCOM
                do 4111 IDF=1,MDF
                  SENTMP(ILZ,IDF)=0.0
                  QDFTMP(ILZ,IDF)=0.0
 4111           continue
 411          continue
              do 320 IDB=1,NDBENT
                SOLDIF=esind(90.0-DBILLA(IDB,1))*esind(90.0-SALT)*
     &                 ecosd(DBILLA(IDB,2)-RSAZI)
                SOLDIF=SOLDIF+ecosd(90.0-DBILLA(IDB,1))*ecosd(90.0-SALT)
                SOLDIF=acos(SOLDIF)*180.0/PI
C                AZIDIF=abs(DBILLA(IDB,1)-SALT)
C                ALTDIF=abs(DBILLA(IDB,2)-RSAZI)
                if ((SOLDIF.lt.AZIACC).and.
     &              (IBLIND.eq.nint(DBILLA(IDB,5)))) then
                  NMATCH=NMATCH+1

            write (66,*) ' Found a direct/ diffuse match: '
            write (66,*) 'Dir: ',DBILLA(IDB,3),' Dif: ',DBILLA(IDB,4),
     &                    ' Blind: ',DBILLA(IDB,5)

                  do 421 ILZ=1,MLCOM
                    do 4211 IDF=1,MDF

C Generate coefficients of least squares arrays.
                      if (ILZ.eq.1.and.IDF.eq.1) then
                        Aa(ILZ,IDF)=DBILLA(IDB,3)**2
                        Ab(ILZ,IDF)=DBILLA(IDB,3)*DBILLA(IDB,4)
                        Ac(ILZ,IDF)=DBILLA(IDB,4)**2
                        Ya(ILZ,IDF)=DBILLA(IDB,3)*DBILLS(IDB,ILZ,IDF)
                        Yb(ILZ,IDF)=DBILLA(IDB,4)*DBILLS(IDB,ILZ,IDF)
                      else
                        Aa(ILZ,IDF)=Aa(ILZ,IDF)+DBILLA(IDB,3)**2
                        Ab(ILZ,IDF)=Ab(ILZ,IDF)+DBILLA(IDB,3)*
     &                                                    DBILLA(IDB,4)
                        Ac(ILZ,IDF)=Ac(ILZ,IDF)+DBILLA(IDB,4)**2
                        Ya(ILZ,IDF)=Ya(ILZ,IDF)+DBILLA(IDB,3)*
     &                                              DBILLS(IDB,ILZ,IDF)
                        Yb(ILZ,IDF)=Yb(ILZ,IDF)+DBILLA(IDB,4)*
     &                                              DBILLS(IDB,ILZ,IDF)
                      endif
 4211               continue
 421              continue
                endif
 320          continue
              if (NMATCH.gt.2) then
                ILLDBM=1
                do 422 ILZ=1,MLCOM
                  do 4221 IDF=1,MDF

C Calculate regression coefficients GRADQR and GRADQF.
                    GRADQF= Ya(ILZ,IDF) * (Ab(ILZ,IDF) / Aa(ILZ,IDF))
                    GRADQF= GRADQF - Yb(ILZ,IDF)
                    GRADQF= GRADQF / 
     &                 ( ( (Ab(ILZ,IDF)**2) /Aa(ILZ,IDF)) - Ac(ILZ,IDF))
                    GRADQR= Ya(ILZ,IDF) - (Ab(ILZ,IDF) * GRADQF)
                    GRADQR= GRADQR / Aa(ILZ,IDF)
                    selum(ILZ,IDF)=(GRADQR*QDIR) + (GRADQF*QDIF)
 4221             continue
 422            continue

                write (66,*) 'Estimated illuminances:'
                write (66,'(9(f7.1,1x))') (selum(1,IDFT),IDFT=1,MDF)
                write (66,'(9(f7.1,1x))') (selum(2,IDFT),IDFT=1,MDF)
                write (66,'(9(f7.1,1x))') (selum(3,IDFT),IDFT=1,MDF)
                write (66,'(9(f7.1,1x))') (selum(4,IDFT),IDFT=1,MDF)
              endif
            endif

C Call e2r to calculate illuminances.
C ** Set this as the default treatment **
            ILLDBM=0
            if (ILLDBM.eq.0) then

C Create transfer file (format: .PID.tmp) LTMPFL is the file name (passed
C to e2r) and  LTMPFLA is the absolute path/ file name.
              call esppid(ipid)
              write(LTMPFLA,'(a,i7,a)') '.',ipid,'.dat'
              call st2file(LTMPFLA,LTMPFL)
              write (LTMPFLA,'(a,a,a)')upath(1:lnblnk(upath)),'/',
     &          LTMPFL(1:lnblnk(LTMPFL))
              INQUIRE (FILE=LTMPFLA(1:lnblnk(LTMPFLA)),EXIST=XST)
              if(XST)then
                call edisp(iuout,'Removing existing transfer file. ')
                call FPOPEN(IUNIT,ISTAT,1,1,LTMPFLA)
                call EFDELET(IUNIT,ISTAT)
              endif
              call FPOPEN(IUNIT,ISTAT,1,2,LTMPFLA)
              write (OUTCMD,'(4f9.1,i3)') SALT,RSAZI,QDIR,QDIF,IBLIND
              write (IUNIT,'(a)') OUTCMD
              call ERPFREE(IUNIT,ISTAT)
              write(doit,'(3a,i3,3a)')'e2r -file ',
     &          LCFGF(:lnblnk(LCFGF)),' -purpose Coupling -zone ',
     &          ICOMP,' -act ',LTMPFL(:lnblnk(LTMPFL)),' -mode text'
              write(6,'(3a,i3,3a)')'e2r -file ',
     &          LCFGF(:lnblnk(LCFGF)),' -purpose Coupling -zone ',
     &          ICOMP,' -act ',LTMPFL(:lnblnk(LTMPFL)),' -mode text'
              call edisp(icout,'Lighting simulation has now commenced.')
              call runit(doit,'-')

C Create new entry in illuminance database.
              NDBENT=NDBENT+1
              DBILLA(NDBENT,1)=SALT
              DBILLA(NDBENT,2)=RSAZI
              DBILLA(NDBENT,3)=QDIR
              DBILLA(NDBENT,4)=QDIF
              DBILLA(NDBENT,5)=float(IBLIND)

C Recover new illuminance data.
C Open transfer file.
              suma=0.
              CALL FPOPEN(IUNIT,ISTAT,1,1,LTMPFLA)
              if(ISTAT.lt.0)then
                IER=1
                write(outs,7) zname(icomp),N
    7           format(' Zone ',A,'light zone',I1,' time var illum err')
                call edisp(iuout,outs)
                CALL EPWAIT
                CALL EPAGEND
                stop
              endif

C Loop through till 'OUTCMD' string found.
 777          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'trn file line',IER)
              if (IER.ne.0) then
                write(outs,17) zname(icomp),N
   17           format(' Zone ',A,'light zone',I1,'trns file read err')
                call edisp(iuout,outs)
                CALL EPWAIT
                CALL EPAGEND
                stop
              endif
              if (OUTSTR(1:60).ne.OUTCMD(1:60)) goto 777

C Now read sensor data.
              DO 19 I=1,NDF(ICOMP,N)
                sumJ=0.
                CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'t var illum',IER)
                if (IER.ne.0) then
                 write(outs,'(2a)')' Fatal error: Radiance simulation',
     &              ' produced no data.'
                  call edisp(iuout,outs)
                  CALL EPWAIT
                  CALL EPAGEND
                  stop
                endif
                K=0
                si=0.0
                CALL EGETWR(OUTSTR,K,si,0.,0.,'-','sen illum',IER)
                subT=si
                suma=suma+subT
                selum(N,I)=subT

C Copy new information to illuminance DB.
                DBILLS(NDBENT,N,I)=int(subT)

C Trace output:
                if(IFT.EQ.1)then
                  write(outs,111)N,I,selum(N,I),SETPT(ICOMP,N)
  111             format(I6,I5,' N/A ','  N/A  ','   N/A   ','  N/A ',
     &               '   N/A  ','   N/A  ',F7.1,F6.0,'   N/A   ')
                  call edisp(itu,outs)
                endif
   19         CONTINUE

C Close transfer file.
              call ERPFREE(IUNIT,ISTAT)

              write (66,*) 'Calculated illuminances:'
              write (66,'(9(f7.1,1x))') (selum(1,IDFT),IDFT=1,MDF)
              write (66,'(9(f7.1,1x))') (selum(2,IDFT),IDFT=1,MDF)
              write (66,'(9(f7.1,1x))') (selum(3,IDFT),IDFT=1,MDF)
              write (66,'(9(f7.1,1x))') (selum(4,IDFT),IDFT=1,MDF)
            else
              call edisp(iuout,'Using information from illum database.')
            endif
          ENDIF
        endif

C Average illumination for this light zone:
        zelum(N)=suma/float(NDF(ICOMP,N))
      ELSE

C Loop through each sensor point, calculate the illumination due
C to each external window and summate.  Each window will
C have a different solar contribution depending on whether or
C not it can see the sun.  To regain information on the associated
C surfaces, loop as in dayfac.f.  Note suma is total sensed lux
C in zone for use in averaging, sumJ is the total sensed lux at
C each sensor.
        suma=0.
        DO 10 I=1,NDF(ICOMP,N)
          sumJ=0.
          XP=CGX(ICOMP,N,I)
          YP=CGY(ICOMP,N,I)
          ZP=CGH(ICOMP,N,I)

C Internal sensor, determine the 'other' zone and connection
C information.   
          DO 20 J=1,NSUR
            CALL SURADJ(ICOMP,J,IES,T,IZC,ISC,ICN,CSTR)
            NA=NVER(J)
            DO 201 K=1,NA
              XPL(K)=X(JVN(J,K))
              YPL(K)=Y(JVN(J,K))
              ZPL(K)=Z(JVN(J,K))
  201       CONTINUE

C Continue only if this surface is vertical, is TMC, has 4 vertices
C and is connected to either the exterior or another real zone.  
C Keep track of total windows in zone with nt. Exclude skylights.
            IF(ITMCFL(ICOMP,J).GT.0)THEN
              call eclose(PELV(ICOMP,J),0.00,0.70,close)
              IF(close)THEN
                IF((NVER(J).NE.4).and.(IDFST(ICOMP,N).ne.2))then
                  WRITE(ITU,843) zname(icomp),J
  843             format('Zone ',A,'TMC surf ',I2,' not 4 vertices!')

C This would be a good place to calculate an alternative bounding box
C for use in width/height calculations.
C << to be done >>
                ENDIF

C Test if the TMC is rectangular and calculate window width
C and high WIDTH, HIGH:
                call eclose(XPL(1),XPL(2),0.01,closea)
                call eclose(XPL(1),XPL(3),0.01,closeb)
                call eclose(XPL(1),XPL(4),0.01,closec)
                call eclose(YPL(1),YPL(2),0.01,closed)
                call eclose(YPL(1),YPL(3),0.01,closee)
                call eclose(YPL(1),YPL(4),0.01,closef)
                call eclose(ZPL(1),ZPL(2),0.01,closeg)
                call eclose(ZPL(1),ZPL(3),0.01,closeh)
                call eclose(ZPL(1),ZPL(4),0.01,closei)
                IF(closea.OR.closeb.OR.closec)THEN
                  IF(closed.OR.closee.OR.closef)THEN
                    IF(closeg.OR.closeh.OR.closei)THEN
                      CONTINUE
                    ELSE
                      WRITE(ITU,842)ICOMP,J
                    ENDIF      
                  ELSE
                    WRITE(ITU,842)ICOMP,J
                  ENDIF
                ELSE
                  if(IDFST(ICOMP,N).ne.2)then
                    WRITE(ITU,842) zname(icomp),J
  842             format('Zone ',A,'TMC',I2,' may not be rectangular!')
                  endif
                ENDIF 

                IF((IES.EQ.0).OR.(IES.EQ.3))THEN
                  IF((IES.EQ.3).AND.(IDFST(ICOMP,N).NE.2))THEN

C Calculate equivalent source illuminance via interior window.
C DIFT in in Watts/m^2 (similar to the outside radiation). DIRT is
C disregarded because we do not trace it direction in this zone.
C Note that the attenuation of visible transmittance going through
C the glass is roughly accounted for in solar algorithm and DFDAT
C includes the maintinence factor.
                    diftzn=DIFT(IZC,ISC)*skyeff
                    subT=diftzn*DFDAT(ICOMP,N,J,I)
                    if(IFT.EQ.1)then
                      write(outs,103)I,J,
     &                      DFDAT(ICOMP,N,J,I)*100.,diftzn,
     &                      subT,SETPT(ICOMP,N)
  103                 format(I5,' via internal ',I5,F9.2,'      ',
     &                     F8.1,F7.1,F6.0)
                      call edisp(itu,outs)
                    endif

C If user specified DF for internal window then calculate the
C same way as for external, but do not check for sun dir illuminance:
                  ELSEIF((IES.EQ.3).AND.(IDFST(ICOMP,N).EQ.2 ))THEN
                    suncon=0.
                    CALL DWINVT(ICOMP,J,V,RATIO)
                    skycon=esky*DFDAT(ICOMP,N,J,I)*V
                    subT=skycon
                    if(IFT.EQ.1)then
                      write(outs,107)N,I,J,
     &                 DFDAT(ICOMP,N,J,I)*V*100.,V,
     &                 suncon,skycon,subT,SETPT(ICOMP,N),Ev
  107                 format(I6,I5,'  no ',I7,F9.2,F6.2,F8.1,F8.1,
     &                       F7.1,F6.0,F9.0)
                      call edisp(itu,outs)
                    endif
                  ELSE

C Determine the angles of view to window from the sensor point,
C convert to degrees between 0 & 360, and compare with the
C solar angle. Calculate WIDTH and HIGH of window to be used in
C subroutine ANGS:
                    XX1=XPL(1)
                    DO 555 M=1,4
                      XPLTEM=XPL(M)
                      call eclose(XPL(1),XPLTEM,0.01,close)
                      IF(.NOT.close)XX2=XPL(M)
  555               CONTINUE
                    YY1=YPL(1)
                    DO 556 M=1,4
                      YPLTEM=YPL(M)
                      call eclose(YPL(1),YPLTEM,0.01,close)
                      IF(.NOT.close)YY2=YPL(M)
  556               CONTINUE
                    ZZ1=ZPL(1)              
                    DO 557 M=1,4
                      ZPLTEM=ZPL(M)
                      call eclose(ZPL(1),ZPLTEM,0.01,close)
                      IF(.NOT.close)ZZ2=ZPL(M)
  557               CONTINUE

C If the surface is 4-sided the width calculation works.
C However, if the surface has different edges, then the
C calculation of width and height could be incorrect.
                    WIDTH=SQRT((XX1-XX2)**2+(YY1-YY2)**2)
                    HIGH=ABS(ZZ1-ZZ2)

C P1,P2,T1 & T2  as returned from ANGS are the radians off of the normal to
C the surface with the window (from the inside). Convert azimuth to
C degrees (clockwise from north) equivalent to SAZI by subtracting
C from the azimuth angle of the surface in question and keeping in the
C range of 0 to 360 degrees.
                    CALL ANGS(NA,XPL,YPL,ZPL,WIDTH,HIGH,XP,YP,ZP)
                    PP1=PAZI(ICOMP,J)-(P1*TODEG)
                    IF(PP1.LT.0.)PP1=360.+PP1
                    IF(PP1.GE.360.)PP1=PP1-360.
                    PP2=PAZI(ICOMP,J)-(P2*TODEG)
                    IF(PP2.LT.0.)PP2=360.+PP2
                    IF(PP2.GE.360.)PP2=PP2-360.
                    TT1=T1*TODEG
                    TT2=T2*TODEG

C Calculate Ev vertical unobstructed illumination @ plane of
C glazing and get current visible transmission of glazing.
                    CALL VRTILM(PAZI(ICOMP,J),QDIR,QDIF,SRADDO,SKYDIF)
                    Ev=SRADDO*suneff+SKYDIF*skyeff
                    IF((PP1.LT.SAZI).AND.(PP2.GT.SAZI).AND.
     &                (TT1.LT.SALT).AND.(TT2.GT.SALT))THEN
                      CALL DWINVT(ICOMP,J,V,RATIO)
                      suncon=esun*V
                      skycon=esky*DFDAT(ICOMP,N,J,I)*V
                      subT=suncon+skycon
                      if(IFT.EQ.1)then
                        write(outs,101)N,I,J,
     &                   DFDAT(ICOMP,N,J,I)*V*100.,V,
     &                    suncon,skycon,subT,SETPT(ICOMP,N),Ev
  101                   format(I6,I5,' yes ',I7,F9.2,F6.2,F8.1,F8.1,
     &                       F7.1,F6.0,F9.0)
                        call edisp(itu,outs)
                      endif
                    ELSE
                      suncon=0.
                      CALL DWINVT(ICOMP,J,V,RATIO)
                      skycon=esky*DFDAT(ICOMP,N,J,I)*V
                      subT=skycon
                      if(IFT.EQ.1)then
                        write(outs,102)N,I,J,
     &                   DFDAT(ICOMP,N,J,I)*V*100.,V,
     &                   suncon,skycon,subT,SETPT(ICOMP,N),Ev
  102                   format(I6,I5,'  no ',I7,F9.2,F6.2,F8.1,F8.1,
     &                       F7.1,F6.0,F9.0)
                        call edisp(itu,outs)
                      endif
                    ENDIF
                  ENDIF
                  suma=suma+subT
                  sumJ=sumJ+subT
                ENDIF
              ELSE
                if(IDFST(ICOMP,N).ne.2)then
                  WRITE(ITU,844) zname(icomp),J
  844         format('Zone ',A,'TMC',I2,'is not vertical-no dir illum!')
                endif
                suncon=0.
                CALL DWINVT(ICOMP,J,V,RATIO)
                skycon=esky*DFDAT(ICOMP,N,J,I)*V
                subT=skycon
                if(IFT.EQ.1)then
                  write(outs,104)N,I,J,
     &              DFDAT(ICOMP,N,J,I)*V*100.,V,
     &              suncon,skycon,subT,SETPT(ICOMP,N),Ev
  104             format(I6,I5,'  no ',I7,F9.2,F6.2,F8.1,F8.1,
     &                  F7.1,F6.0,F9.0)
                  call edisp(itu,outs)
                endif
                suma=suma+subT
                sumJ=sumJ+subT
              ENDIF
            ENDIF
   20     CONTINUE

C Sensed illumination at given sensor:
          selum(N,I)=sumJ
   10   CONTINUE

C Average illumination at lighting zone:
        zelum(N)=suma/float(NDF(ICOMP,N)) 
      ENDIF

C Find lighting output fraction (FRAC) under current control.
C ICGCFL=-1: casual gain ON
C       = 0: casual gain OFF
C       = 1: ON/OFF control.
C       = 2: step down control.
C       = 3: proportional control.
C       = 4: Hunt probability function.

C Special lighting control parameters, which are currently set
C to default values (later they will be user specified) :
C SOFFLL  Switching-off-light reference level fraction of lux set point
C         (usual range 1.2 - 2.5 i.e. 120% - 250%)
C SMEOUT  Constant control gear + min. lamp's dimming or
C         steping wattage fraction of total lights casual load
C SMLOUT  Minimum dimming light output fraction (usually 0.1 i.e. 10%)
C SOFFRL  Switching-off-light reference level in Lux
C IOFFDT  Switch-of-delay time (usual range 5 - 30 minutes), here number
C         of the simulation time steps
C DFRAC   Fraction of luminous output of the artificial lighting
C ZFRAC   Fraction of electric power consumption of the artificial lighting

C Definition of switching-off-light reference level in Lux:
      SOFFRL=SOFFLL(ICOMP,N)*SETPT(ICOMP,N)

C Generally one may choose to define the setpoints at the same
C lux to follow Lynes' method or different setpoints can be used
C for different tasks.  The grid option (1) uses individually
C placed sensors so that one could average over the multiple
C sensors or deal with individual sensor/systems.
      IF(ICGCFL(ICOMP,N) .eq. -1)THEN
        ZFRAC(N)=SYSPER(ICOMP,N)
        IF(IFT.EQ.1)then
          WRITE(outs,801)N,ZFRAC(N)
  801     FORMAT(' Light zone ',I1,' always on!    ZFRAC=',F5.2)
          call edisp(itu,outs)
        endif
        
C Update corresponding SHOCC lighting (if existing) ...
        call updatelighting(icomp,n,zelum(n),btimep,btimef)

      ELSEIF(ICGCFL(ICOMP,N) .eq. 0)THEN
        ZFRAC(N)=0.
        IF(IFT.EQ.1)then
          WRITE(outs,802)N,ZFRAC(N)
  802     FORMAT(' Light zone ',I1,' always off!   ZFRAC=',F5.2)
          call edisp(itu,outs)
        endif
        
C Update corresponding SHOCC lighting (if existing).
        call updatelighting(icomp,n,zelum(n),btimep,btimef)

C Case of On/Off with chosen lighting systems and sensors.
      ELSEIF(ICGCFL(ICOMP,N).EQ.1)THEN
          ZFRAC(N)=0.0
          zell=zelum(N)+SETPT(ICOMP,N)
        IF(zell.GE.SOFFRL.AND.IDTM(ICOMP,N).GE.IOFFDT(ICOMP,N))THEN
          ZFRAC(N)=0.0
          IDTM(ICOMP,N)=IDTM(ICOMP,N)+1
        ELSEIF(zell.GE.SOFFRL.AND.
     &         IDTM(ICOMP,N).LT.IOFFDT(ICOMP,N))THEN
          ZFRAC(N)=SYSPER(ICOMP,N)
          IDTM(ICOMP,N)=IDTM(ICOMP,N)+1
        ELSE
          ZFRAC(N)=SYSPER(ICOMP,N)
          IDTM(ICOMP,N)=0
        ENDIF
        IF(IFT.EQ.1)then
          IF(IDFST(ICOMP,N).EQ.1)WRITE(outs,803)N,ZFRAC(N)
          IF(IDFST(ICOMP,N).EQ.2)WRITE(outs,804)N,ZFRAC(N)
          IF(IDFST(ICOMP,N).EQ.3)WRITE(outs,805)N,ZFRAC(N)
          IF(IDFST(ICOMP,N).EQ.4)WRITE(outs,806)N,ZFRAC(N)
          IF(IDFST(ICOMP,N).EQ.5)WRITE(outs,807)N,ZFRAC(N)
  803     FORMAT(' Light zone ',I1,' ON/OFF ESP-r calc df, ZFRAC=',F5.2)
  804     FORMAT(' Light zone ',I1,' ON/OFF User def df, ZFRAC=',F5.2)
  805     FORMAT(' Light zone ',I1,' ON/OFF Ext sensor, ZFRAC=',F5.2)
  806     FORMAT(' Light zone ',I1,' ON/OFF Coupling, ZFRAC=',F5.2)
  807     FORMAT(' Light zone ',I1,' ON/OFF Daylight coef, ZFRAC=',F5.2)
          call edisp(itu,outs)
        endif

C Update corresponding SHOCC lighting (if existing).
        call updatelighting(icomp,n,zelum(n),btimep,btimef)

C Case of Step Up/Down with chosen sensors/lighting systems:
      ELSEIF(ICGCFL(ICOMP,N) .EQ. 2)THEN
        ZFRAC(N)=0.
        zell=zelum(N)+0.5*SETPT(ICOMP,N)
        IF(zell.GE.SOFFRL.AND.IDTM(ICOMP,N).GE.IOFFDT(ICOMP,N)) THEN
          ZFRAC(N)=0.0
          IDTM(ICOMP,N)=IDTM(ICOMP,N)+1
        ELSEIF(zell.GE.SOFFRL.AND.IDTM(ICOMP,N).LT.IOFFDT(ICOMP,N)) THEN
          ZFRAC(N)=SMEOUT(ICOMP,N)*SYSPER(ICOMP,N)
          IDTM(ICOMP,N)=IDTM(ICOMP,N)+1
        ELSEIF(zell.LT.SOFFRL.AND.zell.GE.SETPT(ICOMP,N))THEN
          ZFRAC(N)=SMEOUT(ICOMP,N)*SYSPER(ICOMP,N)
          IDTM(ICOMP,N)=0
        ELSEIF(zell.LT.SETPT(ICOMP,N))THEN
          ZFRAC(N)=SYSPER(ICOMP,N)
          IDTM(ICOMP,N)=0
        ENDIF
        IF(IFT.EQ.1)then
          IF(IDFST(ICOMP,N).EQ.1)WRITE(outs,808)N,ZFRAC(N)
          IF(IDFST(ICOMP,N).EQ.2)WRITE(outs,809)N,ZFRAC(N)
          IF(IDFST(ICOMP,N).EQ.3)WRITE(outs,810)N,ZFRAC(N)
          IF(IDFST(ICOMP,N).EQ.4)WRITE(outs,811)N,ZFRAC(N)
          IF(IDFST(ICOMP,N).EQ.5)WRITE(outs,812)N,ZFRAC(N)        
  808   FORMAT(' Light zone ',I1,' STEP u/d ESP-r calc df, ZFRAC=',F5.2)
  809   FORMAT(' Light zone ',I1,' STEP u/d User def df, ZFRAC=',F5.2)
  810   FORMAT(' Light zone ',I1,' STEP u/d Ext sensor, ZFRAC=',F5.2)
  811   FORMAT(' Light zone ',I1,' STEP u/d Coupling, ZFRAC=',F5.2)
  812   FORMAT(' Light zone ',I1,' STEP u/d Daylight coef, ZFRAC=',F5.2)
          call edisp(itu,outs)
        endif

C Update corresponding SHOCC lighting (if existing).
        call updatelighting(icomp,n,zelum(n),btimep,btimef)

C Case of proproportional dimming control with chosen sensors/lighting
C systems:
      ELSEIF(ICGCFL(ICOMP,N).EQ.3)THEN
          ZFRAC(N)=0.0
          DFRAC = 0.0
          zell=zelum(N)+SMLOUT(ICOMP,N)*SETPT(ICOMP,N)
        IF(zell.GE.SOFFRL.AND.IDTM(ICOMP,N).GE.IOFFDT(ICOMP,N))THEN
          ZFRAC(N)=0.0
          DFRAC = 0.0
          IDTM(ICOMP,N)=IDTM(ICOMP,N)+1
        ELSEIF(zell.GE.SOFFRL.AND.IDTM(ICOMP,N).LT.IOFFDT(ICOMP,N))THEN
          ZFRAC(N)=SMEOUT(ICOMP,N)*SYSPER(ICOMP,N)
          DFRAC = SMLOUT(ICOMP,N)
          IDTM(ICOMP,N)=IDTM(ICOMP,N)+1
        ELSEIF(zell.LT.SOFFRL.AND.zell.GE.SETPT(ICOMP,N))THEN
          ZFRAC(N)=SMEOUT(ICOMP,N)*SYSPER(ICOMP,N)
          DFRAC = SMLOUT(ICOMP,N)
          IDTM(ICOMP,N)=0
        ELSEIF(zell.LT.SETPT(ICOMP,N).AND.zell.GE.
     &        (1.-SMLOUT(ICOMP,N))*SETPT(ICOMP,N))THEN
          ZFRAC(N)=SMEOUT(ICOMP,N)*SYSPER(ICOMP,N)
          DFRAC = SMLOUT(ICOMP,N)
          IDTM(ICOMP,N)=0
        ELSE
         XXX=((1.-SMEOUT(ICOMP,N))*SMLOUT(ICOMP,N))/(1.-SMLOUT(ICOMP,N))
         YYY=SMEOUT(ICOMP,N)-XXX
         ZFRAC(N)=(((1.-SMEOUT(ICOMP,N))/(1.-SMLOUT(ICOMP,N)))*
     &   ((SETPT(ICOMP,N)-zelum(N))/SETPT(ICOMP,N))+YYY)*SYSPER(ICOMP,N)
         DFRAC = (SETPT(ICOMP,N)-zelum(N))/SETPT(ICOMP,N)
         IDTM(ICOMP,N)=0
        ENDIF
        IF(IFT.EQ.1)then
          IF(IDFST(ICOMP,N).EQ.1)WRITE(outs,813)N,ZFRAC(N),DFRAC
          IF(IDFST(ICOMP,N).EQ.2)WRITE(outs,814)N,ZFRAC(N),DFRAC
          IF(IDFST(ICOMP,N).EQ.3)WRITE(outs,815)N,ZFRAC(N),DFRAC
          IF(IDFST(ICOMP,N).EQ.4)WRITE(outs,816)N,ZFRAC(N),DFRAC
          IF(IDFST(ICOMP,N).EQ.5)WRITE(outs,817)N,ZFRAC(N),DFRAC  
  813     FORMAT(' Light zone ',I1,' Prop ESP-r calc df, ZFRAC=',F5.2,
     &    ' DFRAC=',F5.2)
  814     FORMAT(' Light zone ',I1,' Prop User def df, ZFRAC=',F5.2,
     &    ' DFRAC=',F5.2)
  815     FORMAT(' Light zone ',I1,' Prop Ext sensor, ZFRAC=',F5.2,
     &    ' DFRAC=',F5.2)
  816     FORMAT(' Light zone ',I1,' Prop Coupling, ZFRAC=',F5.2,
     &    ' DFRAC=',F5.2)
  817     FORMAT(' Light zone ',I1,' Prop Daylight coef, ZFRAC=',F5.2,
     &    ' DFRAC=',F5.2)
          call edisp(itu,outs)
        ENDIF

C Update corresponding SHOCC lighting (if existing).
        call updatelighting(icomp,n,zelum(n),btimep,btimef)

C Case of explicit proproportional dimming control with integral
C reset control algorithm:
C
C New sensor and control algorithm settings or definition:
C SPELEC  sensor signal (lux) under artificial lighting
C         - night time calibration;
C zelum   sensor signal (lux) under daylight (time dependend).
C For detailed explanation of control algorithms see: Rubinstein, F.,
C Ward, G., Verderberg, R., Improving the Performance of Photo-
C Electrically Controled Lighting Systems. Journal of the IES Winter 1989.

      ELSEIF(ICGCFL(ICOMP,N).EQ.31)THEN
C Redefine SOFFRL for explicit control:
          SOFFRL=SOFFLL(ICOMP,N)*SPELEC(ICOMP,N)

          ZZZ1 = (1.-SMLOUT(ICOMP,N))*SOFFRL
          YYY1 = (1.-SMLOUT(ICOMP,N))*SPELEC(ICOMP,N)

          ZFRAC(N)=0.0
          DFRAC = 0.0
          zell=zelum(N)
        IF(zell.GE.ZZZ1.AND.IDTM(ICOMP,N).GE.IOFFDT(ICOMP,N))THEN
          ZFRAC(N)=0.0
          DFRAC = 0.0
          IDTM(ICOMP,N)=IDTM(ICOMP,N)+1
        ELSEIF(zell.GE.ZZZ1.AND.IDTM(ICOMP,N).LT.IOFFDT(ICOMP,N))THEN
          ZFRAC(N)=SMEOUT(ICOMP,N)*SYSPER(ICOMP,N)
          DFRAC = SMLOUT(ICOMP,N)
          IDTM(ICOMP,N)=IDTM(ICOMP,N)+1
        ELSEIF(zell.LT.ZZZ1.AND.zell.GT.YYY1)THEN
          ZFRAC(N)=SMEOUT(ICOMP,N)*SYSPER(ICOMP,N)
          DFRAC = SMLOUT(ICOMP,N)
          IDTM(ICOMP,N)=0
        ELSE
          DFRAC=(1.- zelum(N)/SPELEC(ICOMP,N))
          if(DFRAC.lt.SMLOUT(ICOMP,N))DFRAC = SMLOUT(ICOMP,N)
         XXX=((1.-SMEOUT(ICOMP,N))*SMLOUT(ICOMP,N))/(1.-SMLOUT(ICOMP,N))
         YYY=SMEOUT(ICOMP,N)-XXX
         ZFRAC(N)=(((1.-SMEOUT(ICOMP,N))/(1.-SMLOUT(ICOMP,N)))*DFRAC
     &   + YYY)*SYSPER(ICOMP,N)
          IDTM(ICOMP,N)=0
        ENDIF
        IF(IFT.EQ.1)then
          IF(IDFST(ICOMP,N).EQ.1)WRITE(outs,913)N,ZFRAC(N),DFRAC
          IF(IDFST(ICOMP,N).EQ.2)WRITE(outs,914)N,ZFRAC(N),DFRAC
          IF(IDFST(ICOMP,N).EQ.3)WRITE(outs,915)N,ZFRAC(N),DFRAC
          IF(IDFST(ICOMP,N).EQ.4)WRITE(outs,916)N,ZFRAC(N),DFRAC
          IF(IDFST(ICOMP,N).EQ.5)WRITE(outs,917)N,ZFRAC(N),DFRAC  
  913     FORMAT(' Light zone ',I1,' Prop ESP-r calc df, ZFRAC=',F5.2,
     &    ' DFRAC=',F5.2)
  914     FORMAT(' Light zone ',I1,' Prop User def df, ZFRAC=',F5.2,
     &    ' DFRAC=',F5.2)
  915     FORMAT(' Light zone ',I1,' Prop Ext sensor, ZFRAC=',F5.2,
     &    ' DFRAC=',F5.2)
  916     FORMAT(' Light zone ',I1,' Prop Coupling, ZFRAC=',F5.2,
     &    ' DFRAC=',F5.2)
  917     FORMAT(' Light zone ',I1,' Prop Daylight coef, ZFRAC=',F5.2,
     &    ' DFRAC=',F5.2)
          call edisp(itu,outs)
        ENDIF

C Update corresponding SHOCC lighting (if existing).
        call updatelighting(icomp,n,zelum(n),btimep,btimef)

C Case of explicit proproportional dimming control with closed-loop
C algorithm:
C
C New sensor and control algorithm settings or definition:
C SPELEC  sensor signal (lux) under artificial lighting
C         - night time calibration;
C zelum   sensor signal (lux) under daylight (time dependend);
C SLOPEM  linear control response slope (-).

      ELSEIF(ICGCFL(ICOMP,N).EQ.32)THEN
C Redefine SOFFRL for explicit control:
          SOFFRL=SOFFLL(ICOMP,N)*SPELEC(ICOMP,N)

          ZZ1 = SMLOUT(ICOMP,N)*(1. - SLOPEM(ICOMP,N)*SOFFRL)
          ZZ2 = (ZZ1 - 1.)/SLOPEM(ICOMP,N) + SOFFRL

          YY1 = SMLOUT(ICOMP,N)*(1. - SLOPEM(ICOMP,N)*SPELEC(ICOMP,N))
          YY2 = (ZZ1 - 1.)/SLOPEM(ICOMP,N) + SPELEC(ICOMP,N)

          ZFRAC(N)=0.0
          DFRAC = 0.0
          zell=zelum(N)
        IF(zell.GT.ZZ2.AND.IDTM(ICOMP,N).GE.IOFFDT(ICOMP,N))THEN
          ZFRAC(N)=0.0
          DFRAC = 0.0
          IDTM(ICOMP,N)=IDTM(ICOMP,N)+1
        ELSEIF(zell.GT.ZZ2.AND.IDTM(ICOMP,N).LT.IOFFDT(ICOMP,N))THEN
          ZFRAC(N)=SMEOUT(ICOMP,N)*SYSPER(ICOMP,N)
          DFRAC = SMLOUT(ICOMP,N)
          IDTM(ICOMP,N)=IDTM(ICOMP,N)+1
        ELSEIF(zell.LT.ZZ2.AND.zell.GT.YY2)THEN
          ZFRAC(N)=SMEOUT(ICOMP,N)*SYSPER(ICOMP,N)
          DFRAC = SMLOUT(ICOMP,N)
          IDTM(ICOMP,N)=0
        ELSE
          XX1 =  1.+ SLOPEM(ICOMP,N)*(zelum(N)-SPELEC(ICOMP,N))
          XX2 =  1. - SLOPEM(ICOMP,N)*SPELEC(ICOMP,N)
          DFRAC =  XX1/XX2
          if(DFRAC.lt.SMLOUT(ICOMP,N))DFRAC=SMLOUT(ICOMP,N)
         XXX=((1.-SMEOUT(ICOMP,N))*SMLOUT(ICOMP,N))/(1.-SMLOUT(ICOMP,N))
         YYY=SMEOUT(ICOMP,N)-XXX
         ZFRAC(N)=(((1.-SMEOUT(ICOMP,N))/(1.-SMLOUT(ICOMP,N)))*DFRAC
     &   + YYY)*SYSPER(ICOMP,N)
          IDTM(ICOMP,N)=0
        ENDIF
        IF(IFT.EQ.1)then
          IF(IDFST(ICOMP,N).EQ.1)WRITE(outs,713)N,ZFRAC(N),DFRAC
          IF(IDFST(ICOMP,N).EQ.2)WRITE(outs,714)N,ZFRAC(N),DFRAC
          IF(IDFST(ICOMP,N).EQ.3)WRITE(outs,715)N,ZFRAC(N),DFRAC
          IF(IDFST(ICOMP,N).EQ.4)WRITE(outs,708)N,ZFRAC(N),DFRAC
          IF(IDFST(ICOMP,N).EQ.5)WRITE(outs,709)N,ZFRAC(N),DFRAC  
  713     FORMAT(' Light zone ',I1,' Prop ESP-r calc df, ZFRAC=',F5.2,
     &    ' DFRAC=',F5.2)
  714     FORMAT(' Light zone ',I1,' Prop User def df, ZFRAC=',F5.2,
     &    ' DFRAC=',F5.2)
  715     FORMAT(' Light zone ',I1,' Prop Ext sensor, ZFRAC=',F5.2,
     &    ' DFRAC=',F5.2)
  708     FORMAT(' Light zone ',I1,' Prop Coupling, ZFRAC=',F5.2,
     &    ' DFRAC=',F5.2)
  709     FORMAT(' Light zone ',I1,' Prop Daylight coef, ZFRAC=',F5.2,
     &    ' DFRAC=',F5.2)
          call edisp(itu,outs)
        ENDIF

C Update corresponding SHOCC lighting (if existing).
        call updatelighting(icomp,n,zelum(n),btimep,btimef)

      ELSEIF(ICGCFL(ICOMP,N).EQ.4)THEN
C Hunt's manual switching.
C Assume that the on/off periods set in the operation file for the
C matching casual gain represent occupancy times. If there are two
C periods ie. 8h00 to 12h00 and 14h00 to 18h00 then the probability
C will be first calculated at 8h00 and then recalculated at 14h00.
C FIRSTL() is the elum when the probability was last calculated.

C With multiple sensors use the ratio of the daylight factor
C for the brightest and dimmest sensors to judge uniformity.
C If the ratio is outside the range of 0.33<r<3.0 then occupants
C will tend to judge the spatial character of the lighting as
C contrasty and will keep the lights on. If one sensor assume
C ok, as not enough info.
        iuni=0

        IMAXX=1
        IMINN=1
        SMAXX=0.0
        SMINN=1000.0
        DO 886 I=1,NDF(ICOMP,N)
          IF(SUMDF(ICOMP,N,I).GT.SMAXX)THEN
            SMAXX=SUMDF(ICOMP,N,I)
            IMAXX=I
          ENDIF
          IF(SUMDF(ICOMP,N,I).LE.SMINN)THEN
            SMINN=SUMDF(ICOMP,N,I)
            IMINN=I
          ENDIF
  886   CONTINUE
        ratdf=1.0
        IF(SUMDF(ICOMP,N,IMINN).GT.1E-10)THEN
          ratdf=SUMDF(ICOMP,N,IMAXX)/SUMDF(ICOMP,N,IMINN)
        ENDIF
        IF((ratdf.LE.0.33).OR.(ratdf.GT.3.))iuni=1
        elum1=selum(N,IMINN)


C Read in the current zone operation file so that common/P3 will
C be correct for access by HUNTA. HUNTA returns ACTIVE=1. if there
C is a matching casual gain to control at the current timestep.
        IUNITA=IFIL+22
        CALL EROPER(IFT,ITU,IUNITA,ICOMP,IER)
        CALL HUNTA(ICOMP,IHRF,ACTIVE)

C If first hour of the day then calculate a new random number for
C the current zone.
C Following code works on a Sun workstation:
        IF(IHRF.EQ.1)THEN
          seed=(idyp*24+ihrp)+icomp
C For NT.
C          call set_seed@(seed)
C          randn(icomp)=real(random())
C For Unix and Absoft.
          RANDN(ICOMP)=RAND(INT(seed))
        ENDIF

C Following code works on a Hitech workstation or VAX:
C        IF(IHRF.EQ.1)THEN
C          Iseed=(idyp*24+ihrp)+icomp
C          RANDN(ICOMP)=RAN(Iseed)
C          WRITE(IUOUT,887)Iseed,RANDN(ICOMP)
C  887     FORMAT(' iseed=',I12,' RANDN=',F5.3)
C        ENDIF

C If ACTIVE = 0. reset the probability, FRAC and FIRSTL to 0. to
C force later calculation of probability.
        call eclose(ACTIVE,0.00,0.01,close)
        IF(close)THEN
          prob=0.
          ZFRAC(N)=0.
          FIRSTL(ICOMP)=0.

C Generate description for use in the trace.
          IF(IFT.EQ.1)then
            WRITE(outs,901)N,ZFRAC(N)
  901      FORMAT(' Light zone ',I1,' Hunt: no occup! ZFRAC=(',F5.2,')')
            call edisp(itu,outs)
          endif
        ELSE

C Check limits, if elum1 low lighting must be turned on
C (reset Firstl), if high lighting must be turned off (do
C not reset FIRSTL).
          IF(elum1.LT.16.0)THEN
            ZFRAC(N)=SYSPER(ICOMP,N)
            prob=1.
            FIRSTL(ICOMP)=elum1
            IF(IFT.EQ.1)then
               WRITE(outs,902)N,ZFRAC(N),elum1
  902          FORMAT(' Light zone ',I1,' Hunt: ZFRAC=',F4.2,
     &             ' elum low(',F6.0,'): force lighting on')
               call edisp(itu,outs)
            endif
          ELSEIF(elum1.GT.1510.0)THEN
            ZFRAC(N)=0.0
            prob=0.0
            IF(IFT.EQ.1)then
                WRITE(outs,903)N,ZFRAC(N),elum1
  903          FORMAT(' Light zone ',I1,' Hunt: ZFRAC=',F4.2,
     &         ' elum high(',F6.0,'): force lighting off')
               call edisp(itu,outs)
            endif
          ELSEIF(iuni.EQ.1)THEN
            ZFRAC(N)=SYSPER(ICOMP,N)
            prob=1.0
            IF(IFT.EQ.1)then
               WRITE(outs,908)N,ZFRAC(N),elum1
  908          FORMAT(' Light zone ',I1,' Hunt: Z FRAC=',F4.2,
     &         ' elum(',F6.0,'): uniformity fails, lighting on')
               call edisp(itu,outs)
            endif
          ELSE

C If FIRSTL <> 0. then prob previously calculated. If elum > FIRSTL
C keep switched on, if less then test again.
            call eclose(FIRSTL(ICOMP),0.00,0.001,close)
            IF(FIRSTL(ICOMP).GT.0.)THEN
              IF(elum1.GT.FIRSTL(ICOMP))THEN
                ZFRAC(N)=SYSPER(ICOMP,N)
                prob=1.
                IF(IFT.EQ.1)then
                  WRITE(outs,904)N,ZFRAC(N),elum1,FIRSTL(ICOMP)
  904             FORMAT(' Light zone ',I1,' Hunt: ZFRAC=',F4.2,
     &            ' elum(',F6.0,')>inital(',F6.0,
     &            '): keep lighting switched on')
                  call edisp(itu,outs)
                endif
              ELSEIF(elum1.LE.FIRSTL(ICOMP))THEN

C Test again using previous random seed.
                Xel=log10(elum1)
                prob=-0.0175+(1.0361/(1.+exp(4.0835*(Xel-1.8223))))
                if(prob.lt.0.)prob=0.
                if(prob.gt.1.)prob=1.
                IF(prob.gt.RANDN(ICOMP))THEN
                  ZFRAC(N)=SYSPER(ICOMP,N)
                ELSE
                  ZFRAC(N)=0.
                ENDIF
                IF(IFT.EQ.1)then
                  WRITE(outs,905)N,ZFRAC(N),elum1,FIRSTL(ICOMP),prob,
     &                          RANDN(ICOMP)
  905             FORMAT(' Light zone ',I1,' Hunt: ZFRAC=',F4.2,
     &            ' elum(',F6.0,')<inital(',F6.0,
     &            ') test again: prob(',F4.2,') r(',F4.2,')')
                  call edisp(itu,outs)
                endif
              ENDIF
            ELSEIF(close)THEN

C Occupants arriving, calculate probability and save FIRSTL.
              Xel=log10(elum1)
              prob=-0.0175+(1.0361/(1.+exp(4.0835*(Xel-1.8223))))
              if(prob.lt.0.)prob=0.
              if(prob.gt.1.)prob=1.
              IF(prob.gt.RANDN(ICOMP))THEN
                ZFRAC(N)=SYSPER(ICOMP,N)
                FIRSTL(ICOMP)=elum1
              ELSE
                ZFRAC(N)=0.
              ENDIF
                IF(IFT.EQ.1)then
                  WRITE(outs,906)N,ZFRAC(N),elum1,prob,RANDN(ICOMP)
  906             FORMAT(' Light zone ',I1,' Hunt: ZFRAC=',F4.2,
     &            ' Occup arrival first ','elum(',F6.0,
     &            ') prob(',F4.2,') r(',F4.2,')')
                  call edisp(itu,outs)
                endif
            ENDIF
          ENDIF
        ENDIF

C Update corresponding SHOCC lighting (if existing).
        call updatelighting(icomp,n,zelum(n),btimep,btimef)

      ENDIF
      FRAC=FRAC+ZFRAC(N)
    9 CONTINUE

C Remove transfer file.
      call EFDELET(IUNIT,ISTAT)

C Trace output for FRAC:
      IF(IFT.EQ.1)then
        write(outs,910)FRAC
  910   format('Fraction of lighting "ON" FRAC=',F4.2)
        call edisp(itu,outs)
      endif

C Trace output ?
      IF(ITC.LE.0.OR.NSINC.LT.ITC)goto 999
      IF(ITRACE(12).EQ.0.OR.NSINC.GT.ITCF.OR.
     &   IZNTRC(ICOMP).NE.1)goto 999

C Trace solar incident on interior transparent surfaces if DIFT
C is non-zero.
      NC=NCONST(ICOMP)
      DO 52 I=1,NC
        IF(ITMCFL(ICOMP,I).GT.0.and.DIFT(ICOMP,I).gt.0.0)THEN        
          write(outs,'(a,i3,a,f5.1)') 'equiv source illum on surface ',
     &      I,'=',DIFT(ICOMP,I)
          call edisp(itu,outs)
          write(outs,911)I,ICOMP,IBOFOT(ICOMP,I)
 911      format('the TMC surface ',I3,' on zone',I3,' has IBOFOT=',I1)  
          call edisp(itu,outs)
          write(outs,'(A,I3,A,F5.2,A)')'The VT of surface',I,' is ',
     &      V,'**'
          call edisp(itu,outs)
        ENDIF 
 52   CONTINUE  

C Description for irradiation/illumination data:
      write(outs,99971)QDIR,QDIF
99971 format(' Ext: Dir.nor.rad. W/m^2(',F7.1,') Dif.hor.rad.(',
     &       F7.1,')')
      call edisp(itu,outs)
      write(outs,99972)QDIR*suneff,esky
99972 format('    : sun light lux(',F7.1,') sky light lux(',F7.1,')')
      call edisp(itu,outs)
      write(outs,99973)suneff,skyeff
99973 format('    : sun/dir efficacy (',F7.2,') sky/dif efficacy (',
     &       F7.2,')')
      call edisp(itu,outs)
      write(outs,99974)SAZI,SALT
99974 format('    : solar azi deg(',F6.1,') solar alt deg(',F6.1,')')
      call edisp(itu,outs)
      write(outs,99975)RATIO,nelem
99975 format('    : TMC blind/shuter(',F4.2,') Sun patch no.(',I3,')')
      call edisp(itu,outs)

  999 return
      end

C ******************** DWINVT ****************************

C Returns the visible transmittance of glazing at the
C future timestep. IZONE is the zone, ISURF the surface,
C VT is the returned visible transmittance.
C Check to see if blind/shutter control is enabled for this zone,
C if so see if it is active for the current glazing and substitute
C the visible transmittance.

      SUBROUTINE DWINVT(IZONE,ISURF,VT,BLIND)
#include "building.h"

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/TMCB1/IBCMT(MCOM,MTMC)
      COMMON/TMCB2/NBCTMC(MCOM,MTMC),IBCST(MCOM,MBP,MTMC),
     &IBCFT(MCOM,MBP,MTMC),IBCSUR(MCOM,MTMC)
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/PRECT2/TMCT2(MCOM,MTMC,5,MBP),TMCA2(MCOM,MTMC,ME,5,MBP),
     &              TVTR2(MCOM,MTMC,MBP)
C TMC blind control flag
      COMMON/TMCB4/IBOFOT(MCOM,MS),NBPONT(MCOM,MS),ECRAT(MCOM,MS)

C Test here if blind/shutter control is on for this
C TMC surface.
      BLIND = 0.0
      ITVTR=ITMCFL(IZONE,ISURF)
      IF(ITVTR.GT.0.AND.IBCMT(IZONE,ITVTR).EQ.1)THEN
C TMC blind/shutter is in place. Test if it is active.
C NP is no. of control periods for this TMC type.
        NP=NBCTMC(IZONE,ITVTR)
        DO 43 KK=1,NP
          IT1=IBCST(IZONE,KK,ITVTR)
          IT2=IBCFT(IZONE,KK,ITVTR)
          IF(IHRF.GT.IT1.AND.IHRF.LE.IT2.AND.
     &      IBOFOT(IZONE,ISURF).EQ.1)THEN
            BLIND = 1.0
            VT=TVTR2(IZONE,ITVTR,KK)
          ELSE
            BLIND = 0.0
            VT=TVTR(IZONE,ITVTR)
          ENDIF
   43   CONTINUE
      ELSE
        BLIND = 0.0
        VT=TVTR(IZONE,ITVTR)
      ENDIF

      RETURN
      END


C ******************** HUNTA ********************

C Calculate if current hour has scheduled casual gains which are
C controlled via a casual gain control file.
C If ACTIVE=1.0 then there is a potential casual gain/occupants,
C if ACTIVE=0. then no matching casual gain/occupants.

      SUBROUTINE HUNTA(IZONE,IH,ACTIVE)
#include "building.h"

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/P3/NCAS1,ICGS1(MC),ICGF1(MC),CMGS1(MC),CMGL1(MC),
     & RADC1(MC),CONC1(MC),NCAS2,ICGS2(MC),ICGF2(MC),CMGS2(MC),
     & CMGL2(MC),RADC2(MC),CONC2(MC),NCAS3,ICGS3(MC),ICGF3(MC),
     & CMGS3(MC),CMGL3(MC),RADC3(MC),CONC3(MC)
      COMMON/P3TYPE/ICGT1(MC),ICGT2(MC),ICGT3(MC)
      COMMON/CGCIN2/NCGTC(MCOM,3),NLITZ(MCOM),IDFST(MCOM,MLCOM),
     & CGX(MCOM,MLCOM,MDF),CGY(MCOM,MLCOM,MDF),CGH(MCOM,MLCOM,MDF),
     & UX(MCOM,MLCOM,MDF),UY(MCOM,MLCOM,MDF),UH(MCOM,MLCOM,MDF),
     & SETPT(MCOM,MLCOM),SYSPER(MCOM,MLCOM),SOFFLL(MCOM,MLCOM),
     & IOFFDT(MCOM,MLCOM),SMLOUT(MCOM,MLCOM),SMEOUT(MCOM,MLCOM)


      ACTIVE=0.

C Determine which day of the week (6=Sat, 7=Sun ,other weekday)
C and set IGC (gain to control) to that reqested in the casual
C gain control file.  If matching gain(s) set ACTIVE=1.0.
      IF(IDWF.GE.1.OR.IDWF.LE.6)THEN
        IGC=NCGTC(IZONE,1)
        IF(NCAS1.GE.1)THEN
          DO 30 ICNN=1,NCAS1
            IF(ICGT1(ICNN).EQ.IGC)THEN
              IF(IH.GT.ICGS1(ICNN).AND.IH.LE.ICGF1(ICNN))ACTIVE=1.0
            ENDIF
   30     CONTINUE
        ENDIF
      ELSEIF(IDWF.EQ.6)THEN
        IGC=NCGTC(IZONE,2)
        IF(NCAS2.GE.1)THEN
          DO 32 ICNN=1,NCAS2
            IF(ICGT2(ICNN).EQ.IGC)THEN
              IF(IH.GT.ICGS2(ICNN).AND.IH.LE.ICGF2(ICNN))ACTIVE=1.0
            ENDIF
   32     CONTINUE
        ENDIF
      ELSEIF(IDWF.EQ.7)THEN
        IGC=NCGTC(IZONE,3)
        IF(NCAS3.GE.1)THEN
          DO 33 ICNN=1,NCAS3
            IF(ICGT3(ICNN).EQ.IGC)THEN
              IF(IH.GT.ICGS3(ICNN).AND.IH.LE.ICGF3(ICNN))ACTIVE=1.0
            ENDIF
   33     CONTINUE
        ENDIF
      ENDIF

      RETURN
      END

C ******************** VRTILM ********************

C Calculate the illumination (Ev) on a unobstructed vertical
C surface with an azimuth (AZMANG) in degrees clockwise from
C the north for use with Lynes' daylighting methods.  Use
C method in shadow.f to determine if surface is self shaded. 
C Note not yet integrated into lighting calculations but available.
      SUBROUTINE VRTILM(AZMANG,QDIR,QDIF,SRADDO,SKYDIF)
      COMMON/SUNPOS/SAZI,SALT,ISUNUP

      PI = 4.0 * ATAN(1.0)
      R=PI/180.

C Determine surface-solar azimuth angle & Cosine of angle of
C incidence.  Surface is self-shaded if CAI is less than or
C equal to zero.  SRADDO is the direct radiation falling on
C the surface.
      PSAZI=ABS(AZMANG-SAZI)
      CAI=(COS(SALT*R))*(SIN(90.*R))*(COS(PSAZI*R))+
     &    (SIN(SALT*R))*(COS(90.*R))
      IF(CAI.LE.0.)THEN
        SRADDO=0.
      ELSE
        SRADDO=QDIR*CAI
      ENDIF

C Sky diffuse falling on the surface.
      SKYDIF=QDIF*0.5*(1.0+COS(90.*R))

      return
      end

C ******************** PATLUM ********************
C PATLUM - calculate reference luminance of a sky patch

C QDIF    diffuse irradiance
C QDIR    direct normal irradiance
C SALT    solar altitude
C SAZI    solar azimith
C IDYP    julian day number
C npatch  number of the sky patch
C plv     reference sky luminance - returned value

      SUBROUTINE PATLUM(QDIF,QDIR,SALT,SAZI,IDYP,npatch,plv,ang)
      COMMON/SHOUT/ICOUT

      dimension npz(8),alt(8),azim(8)
      DIMENSION  A1(8),A2(8),A3(8),A4(8)
      DIMENSION  B1(8),B2(8),B3(8),B4(8)
      DIMENSION  C1(8),C2(8),C3(8),C4(8)
      DIMENSION  D1(8),D2(8),D3(8),D4(8)
      DIMENSION  E1(8),E2(8),E3(8),E4(8)

C Definitions for daylight coefficient patch calculation:
      data npz/30,60,84,108,126,138,144,145/
      data alt/6.,18.,30.,42.,54.,66.,78.,90./
      data azim/12.,12.,15.,15.,20.,30.,60.,0./

C Start definition of Perez All-Weather Sky Luminance model coefficients
C (Solar Energy Vol. 50. No. 3. pp. 235-245, 1993):
C   Coefficients:
      DATA A1/1.3525,-1.2219,-1.1,-.5484,-.6,-1.0156,-1.0,-1.05/
      DATA A2/-.2567,-.773,-.2515,-.6654,-.3566,-.367,.0211,.0289/
      DATA A3/-.269,1.4148,.8952,-.2672,-2.5,1.0078,.5025,.426/
      DATA A4/-1.4366,1.1016,.0156,.7117,2.325,1.4051,-.5119,.359/

      DATA B1/-.767,-.2054,.2782,.7234,.2937,.2875,-.3,-.325/
      DATA B2/.0007,.0367,-.1812,-.6219,.0496,-.5328,.1922,.1156/
      DATA B3/1.2734,-3.9128,-4.5,-5.6812,-5.6812,-3.85,.7023,.7781/
      DATA B4/-.1233,.9156,1.1766,2.6297,1.8415,3.375,-1.6317,.0025/

      DATA C1/2.8,6.975,24.7219,33.3389,21.,14.,19.,31.0625/
      DATA C2/.6004,.1774,-13.0812,-18.3,-4.7656,-.9999,-5.,-14.5/
      DATA C3/1.2375,6.4477,-37.7,-62.25,-21.5906,-7.1406,1.2438,
     &       -46.1148/
      DATA C4/1.,-.1239,34.8438,52.0781,7.2492,7.5469,-1.9094,55.375/

      DATA D1/1.8734,-1.5798,-5.,-3.5,-3.5,-3.4,-4.,-7.2312/
      DATA D2/.6297,-.5081,1.5218,.0016,-.1554,-.1078,.025,.405/
      DATA D3/.9738,-1.7812,3.9229,1.1477,1.4062,-1.075,.3844,13.35/
      DATA D4/.2809,.108,-2.6204,.1062,.3988,1.5702,.2656,.6234/

      DATA E1/.0356,.2624,-.0156,.4659,.0032,-.0672,1.0468,1.5/
      DATA E2/-.1246,.0672,.1597,-.3296,.0766,.4016,-.3788,-.6426/
      DATA E3/-.5718,-.219,.4199,-.0876,-.0656,.3017,-2.4517,1.8564/
      DATA E4/.9938,-.4285,-.5562,-.0329,-.1294,-.4844,1.4656,.5636/
C END Perez definitions.

      PI = 4.0 * ATAN(1.0)
      RAD=PI/180.
      ZENN=(90.-SALT)*rad
      GONN=1367.*(1.+0.033*COS(0.017214*IDYP))
      CONN1=1./0.001572
      CONN2=(COS(ZENN))**2+0.0031465
      OPTM=CONN1*(SQRT(CONN2)-COS(ZENN))
C Define sky brightness:
      DELTT=QDIF*OPTM/GONN
C Define sky clearness:
      EPSS=((QDIF+QDIR)/QDIF+1.041*ZENN**3)/(1.+1.041*ZENN**3)
C Find sky clearness bin (1-8):
      IF(EPSS.LT.1.)THEN
        IE=1
        call edisp(icout,'Sky clearness < 1.0 setting to 1.0')
        ELSEIF((EPSS.GE.1.).AND.(EPSS.LT.1.065))THEN
        IE=1
        ELSEIF((EPSS.GE.1.065).AND.(EPSS.LT.1.23))THEN
        IE=2
        ELSEIF((EPSS.GE.1.23).AND.(EPSS.LT.1.50))THEN
        IE=3
        ELSEIF((EPSS.GE.1.50).AND.(EPSS.LT.1.95))THEN
        IE=4
        ELSEIF((EPSS.GE.1.95).AND.(EPSS.LT.2.80))THEN
        IE=5
        ELSEIF((EPSS.GE.2.80).AND.(EPSS.LT.4.50))THEN
        IE=6
        ELSEIF((EPSS.GE.4.50).AND.(EPSS.LT.6.20))THEN
        IE=7
        ELSEIF(EPSS.GE.6.20)THEN 
        IE=8
      ENDIF

C Calculate sky luminanace coefficients:
      a=a1(ie)+a2(ie)*zenn+deltt*(a3(ie)+a4(ie)*zenn)
      b=b1(ie)+b2(ie)*zenn+deltt*(b3(ie)+b4(ie)*zenn)
      if(ie.eq.1)then
        c=exp((deltt*(c1(ie)+c2(ie)*zenn))**c3(ie))-c4(ie)
        d=-exp(deltt*(d1(ie)+d2(ie)*zenn))+d3(ie)+deltt*d4(ie)
      else
        c=c1(ie)+c2(ie)*zenn+deltt*(c3(ie)+c4(ie)*zenn)
        d=d1(ie)+d2(ie)*zenn+deltt*(d3(ie)+d4(ie)*zenn)
      endif
      e=e1(ie)+e2(ie)*zenn+deltt*(e3(ie)+e4(ie)*zenn)

C Calculate sun position unit vector x, y, z coordinates:
C Transfer sun azimuth angle (clockwise from North) into sky
C patch azimuth angle (Anticlockwise from East):
      if((sazi.gt.0.0).and.(sazi.lt.90.0))then
        aazim = 90.0 - sazi
      else
        aazim = 450.0 - sazi
      endif

      x3 = ecosd(aazim)*cos(salt*RAD)
      y3 = esind(aazim)*cos(salt*RAD)
      z3 = sin(salt*RAD)

C Origin coordinates for calculation of angle between sun
C and given sky patch:
      x2=0.0
      y2=0.0
      z2=0.0

C Find which sky patch level (there are 8 levels):
      if((npatch.ge.1).and.(npatch.le.30))then
        nzone = 1
      elseif((npatch.ge.31).and.(npatch.le.60))then
        nzone = 2
      elseif((npatch.ge.61).and.(npatch.le.84))then
        nzone = 3
      elseif((npatch.ge.85).and.(npatch.le.108))then
        nzone = 4
      elseif((npatch.ge.109).and.(npatch.le.126))then
        nzone = 5
      elseif((npatch.ge.127).and.(npatch.le.138))then
        nzone = 6
      elseif((npatch.ge.139).and.(npatch.le.144))then
        nzone = 7
      elseif(npatch.eq.145)then
        nzone = 8
      endif

C Calculate x, y, z coordinates for unit vector position of given
C sky patch:
      if(nzone.eq.8) then
         x1 = 0.0
         y1 = 0.0
         z1 = 1.0
      elseif(nzone.eq.1) then
         aazim = azim(nzone)*(npatch - 1)
         x1 = ecosd(aazim)*cos(alt(nzone)*RAD)
         y1 = esind(aazim)*cos(alt(nzone)*RAD)
         z1 = sin(alt(nzone)*RAD)
      else
         aazim = azim(nzone)*((npatch - npz(nzone-1)) - 1)
         x1 = ecosd(aazim)*cos(alt(nzone)*RAD)
         y1 = esind(aazim)*cos(alt(nzone)*RAD)
         z1 = sin(alt(nzone)*RAD)
      endif

C Debug
C      write(6,*) 'x3: ',x3,' y3: ',y3,'z3: ',z3
C      write(6,*) 'x2: ',x2,' y2: ',y2,'z2: ',z2
C      write(6,*) 'x1: ',x1,' y1: ',y1,'z1: ',z1
C End debug

C Find angle between given sky patch and sun position
C and convert it to radiance:
      call ang3vtx(x1,y1,z1,x2,y2,z2,x3,y3,z3,ang)
      ang = ang*rad

C Calculate reference luminance for given sky patch:
C Define zenith angle of given sky patch:
      zelm = (90.0 - alt(nzone))*rad
      plv=(1.+a*exp(b/cos(zelm)))*(1.+c*exp(d*ang)+e*(cos(ang))**2)
      return
      end

C ******************** REFILL ********************
C REFILL - calculate reference illuminance for model normalisation

C QDIF   diffuse irradiance
C QDIR   direct normal irradiance
C SALT   solar altitude
C SAZI   solar azimith
C IDYP   julian day number
C refell reference sky illuminance

      SUBROUTINE REFILL(QDIF,QDIR,SALT,SAZI,IDYP,refell)
      COMMON/SHOUT/ICOUT

      dimension npz(8),alt(8),azim(8)
      DIMENSION  A1(8),A2(8),A3(8),A4(8)
      DIMENSION  B1(8),B2(8),B3(8),B4(8)
      DIMENSION  C1(8),C2(8),C3(8),C4(8)
      DIMENSION  D1(8),D2(8),D3(8),D4(8)
      DIMENSION  E1(8),E2(8),E3(8),E4(8)

C Definitions for daylight coefficient patch calculation:
      data npz/30,60,84,108,126,138,144,145/
      data alt/6.,18.,30.,42.,54.,66.,78.,90./
      data azim/12.,12.,15.,15.,20.,30.,60.,0./

C Start definition of Perez All-Weather Sky Luminance model coefficients
C (Solar Energy Vol. 50. No. 3. pp. 235-245, 1993):
C   Coefficients:
      DATA A1/1.3525,-1.2219,-1.1,-.5484,-.6,-1.0156,-1.0,-1.05/
      DATA A2/-.2567,-.773,-.2515,-.6654,-.3566,-.367,.0211,.0289/
      DATA A3/-.269,1.4148,.8952,-.2672,-2.5,1.0078,.5025,.426/
      DATA A4/-1.4366,1.1016,.0156,.7117,2.325,1.4051,-.5119,.359/

      DATA B1/-.767,-.2054,.2782,.7234,.2937,.2875,-.3,-.325/
      DATA B2/.0007,.0367,-.1812,-.6219,.0496,-.5328,.1922,.1156/
      DATA B3/1.2734,-3.9128,-4.5,-5.6812,-5.6812,-3.85,.7023,.7781/
      DATA B4/-.1233,.9156,1.1766,2.6297,1.8415,3.375,-1.6317,.0025/

      DATA C1/2.8,6.975,24.7219,33.3389,21.,14.,19.,31.0625/
      DATA C2/.6004,.1774,-13.0812,-18.3,-4.7656,-.9999,-5.,-14.5/
      DATA C3/1.2375,6.4477,-37.7,-62.25,-21.5906,-7.1406,1.2438,
     &       -46.1148/
      DATA C4/1.,-.1239,34.8438,52.0781,7.2492,7.5469,-1.9094,55.375/

      DATA D1/1.8734,-1.5798,-5.,-3.5,-3.5,-3.4,-4.,-7.2312/
      DATA D2/.6297,-.5081,1.5218,.0016,-.1554,-.1078,.025,.405/
      DATA D3/.9738,-1.7812,3.9229,1.1477,1.4062,-1.075,.3844,13.35/
      DATA D4/.2809,.108,-2.6204,.1062,.3988,1.5702,.2656,.6234/

      DATA E1/.0356,.2624,-.0156,.4659,.0032,-.0672,1.0468,1.5/
      DATA E2/-.1246,.0672,.1597,-.3296,.0766,.4016,-.3788,-.6426/
      DATA E3/-.5718,-.219,.4199,-.0876,-.0656,.3017,-2.4517,1.8564/
      DATA E4/.9938,-.4285,-.5562,-.0329,-.1294,-.4844,1.4656,.5636/
C END Perez definitions.

      PI = 4.0 * ATAN(1.0)
      RAD=PI/180.
      ZENN=(90.-SALT)*rad
      GONN=1367.*(1.+0.033*COS(0.017214*IDYP))
      CONN1=1./0.001572
      CONN2=(COS(ZENN))**2+0.0031465
      OPTM=CONN1*(SQRT(CONN2)-COS(ZENN))
C Define sky brightness:
      DELTT=QDIF*OPTM/GONN
C Define sky clearness:
      EPSS=((QDIF+QDIR)/QDIF+1.041*ZENN**3)/(1.+1.041*ZENN**3)
C Find sky clearness bin (1-8):
      IF(EPSS.LT.1.)THEN
        IE=1
        call edisp(icout,'Sky clearness < 1.0 setting to 1.0')
        ELSEIF((EPSS.GE.1.).AND.(EPSS.LT.1.065))THEN
        IE=1
        ELSEIF((EPSS.GE.1.065).AND.(EPSS.LT.1.23))THEN
        IE=2
        ELSEIF((EPSS.GE.1.23).AND.(EPSS.LT.1.50))THEN
        IE=3
        ELSEIF((EPSS.GE.1.50).AND.(EPSS.LT.1.95))THEN
        IE=4
        ELSEIF((EPSS.GE.1.95).AND.(EPSS.LT.2.80))THEN
        IE=5
        ELSEIF((EPSS.GE.2.80).AND.(EPSS.LT.4.50))THEN
        IE=6
        ELSEIF((EPSS.GE.4.50).AND.(EPSS.LT.6.20))THEN
        IE=7
        ELSEIF(EPSS.GE.6.20)THEN 
        IE=8
      ENDIF

C Calculate sky luminanace coefficients:
      a=a1(ie)+a2(ie)*zenn+deltt*(a3(ie)+a4(ie)*zenn)
      b=b1(ie)+b2(ie)*zenn+deltt*(b3(ie)+b4(ie)*zenn)
      if(ie.eq.1)then
        c=exp((deltt*(c1(ie)+c2(ie)*zenn))**c3(ie))-c4(ie)
        d=-exp(deltt*(d1(ie)+d2(ie)*zenn))+d3(ie)+deltt*d4(ie)
      else
        c=c1(ie)+c2(ie)*zenn+deltt*(c3(ie)+c4(ie)*zenn)
        d=d1(ie)+d2(ie)*zenn+deltt*(d3(ie)+d4(ie)*zenn)
      endif
      e=e1(ie)+e2(ie)*zenn+deltt*(e3(ie)+e4(ie)*zenn)

C Calculate sun position unit vector x, y, z coordinates:
C Transfer sun azimuth angle (clockwise from North) into sky
C patch azimuth angle (Anticlockwise from East):
      if((sazi.gt.0.0).and.(sazi.lt.90.0))then
        aazim = 90.0 - sazi
      else
        aazim = 450.0 - sazi
      endif

      x3 = ecosd(aazim)
      y3 = esind(aazim)
      z3 = tan(salt*RAD)

C Origin coordinates for calculation of angle between sun
C and given sky patch:
      x2=0.0
      y2=0.0
      z2=0.0
      
      refell = 0.0
      do 20 npatch = 1,145
C Find which sky patch level (there are 8 levels):
        if((npatch.ge.1).and.(npatch.le.30))then
          nzone = 1
        elseif((npatch.ge.31).and.(npatch.le.60))then
          nzone = 2
        elseif((npatch.ge.61).and.(npatch.le.84))then
          nzone = 3
        elseif((npatch.ge.85).and.(npatch.le.108))then
          nzone = 4
        elseif((npatch.ge.109).and.(npatch.le.126))then
          nzone = 5
        elseif((npatch.ge.127).and.(npatch.le.138))then
          nzone = 6
        elseif((npatch.ge.139).and.(npatch.le.144))then
          nzone = 7
        elseif(npatch.eq.145)then
          nzone = 8
        endif

C Calculate x, y, z coordinates for unit vector position of given
C sky patch:
        if(nzone.eq.8) then
           x1 = 0.0
           y1 = 0.0
           z1 = 1.0
        elseif(nzone.eq.1) then
           aazim = azim(nzone)*(npatch - 1)
           x1 = ecosd(aazim)
           y1 = esind(aazim)
           z1 = tan(alt(nzone)*RAD)
        else
           aazim = azim(nzone)*((npatch - npz(nzone-1)) - 1)
           x1 = ecosd(aazim)
           y1 = esind(aazim)
           z1 = tan(alt(nzone)*RAD)
        endif

C Find angle between given sky patch and sun position
C and convert it to radiance:
        call ang3vtx(x1,y1,z1,x2,y2,z2,x3,y3,z3,ang)
        ang = ang*rad

C Calculate reference luminance for given sky patch:
C Define zenith angle of given sky patch:
        zelm = (90.0 - alt(nzone))*rad
        plv=(1.+a*exp(b/cos(zelm)))*(1.+c*exp(d*ang)+e*(cos(ang))**2)
      
Calculate delta reference illuminance from npatch:
        refplv = plv*(2.*PI/145.0)*cos(zelm)
        refell = refell + refplv

20    continue

      return
      end

C ******************** LUMEFF ********************
C LUMEFF - calculate luminous efficacy of solar irradiance.

C QDIF   diffuse irradiance
C QDIR   direct normal irradiance
C SALT   solar altitude
C IDYP   julian day number
C skyeff luminous efficacy of diffuse irradiance
C suneff luminous efficacy of direct normal irradiance

      SUBROUTINE LUMEFF(QDIF,QDIR,SALT,IDYP,skyeff,suneff)
      COMMON/SHOUT/ICOUT

C Start definition of Perez model coefficients for luminous efficacy
C of solar irradiance and zenith luminance;
C (Solar Energy Vol. 44. No. 5. pp. 271-289, 1990):
      DIMENSION  ASUN(8),ASKY(8),CSUN(8),CSKY(8),DSUN(8),DSKY(8)

      logical closea

C   Coefficients:
      DATA ASUN/57.20,98.99,109.83,110.34,106.36,107.19,105.75,101.18/
      DATA ASKY/97.24,107.22,104.97,102.39,100.71,106.42,141.88,152.23/
      DATA CSUN/-2.98,-1.21,-1.71,-1.99,-1.75,-1.51,-1.26,-1.10/
      DATA CSKY/12.00,0.59,-5.53,-13.95,-22.75,-36.15,-53.24,-45.27/
      DATA DSUN/117.12,12.38,-8.81,-4.56,-6.16,-26.73,-34.44,-8.29/
      DATA DSKY/-8.91,-3.95,-8.77,-13.90,-23.74,-28.83,-14.03,-7.98/
C END Perez definitions.

      PI = 4.0 * ATAN(1.0)
      RAD=PI/180.

C Check if any diffuse irradiance and if sun is up - if not do not 
C calculate and set efficacy and zenith luminance to zero:

      call eclose(QDIF,0.00,0.01,closea)
      if(SALT.LT.0.)closea=.true.
      if(closea) then
        skyeff=0.
        suneff=0.     
      else

C Calculate efficacy of direct and diffuse irradiance 
C and zenith luminance:

        ZENN=(90.-SALT)*rad
        GONN=1367.*(1.+0.033*COS(0.017214*IDYP))
        CONN1=1./0.001572
        CONN2=(COS(ZENN))**2+0.0031465
        OPTM=CONN1*(SQRT(CONN2)-COS(ZENN))
C Define sky brightness:
        DELTT=QDIF*OPTM/GONN
C Define sky clearness:
        EPSS=((QDIF+QDIR)/QDIF+1.041*ZENN**3)/(1.+1.041*ZENN**3)
C Find sky clearness bin (1-8):
        IF(EPSS.LT.1.)THEN
          IE=1
          call edisp(icout,'Sky clearness < 1.0 setting to 1.0')
          ELSEIF((EPSS.GE.1.).AND.(EPSS.LT.1.065))THEN
          IE=1
          ELSEIF((EPSS.GE.1.065).AND.(EPSS.LT.1.23))THEN
          IE=2
          ELSEIF((EPSS.GE.1.23).AND.(EPSS.LT.1.50))THEN
          IE=3
          ELSEIF((EPSS.GE.1.50).AND.(EPSS.LT.1.95))THEN
          IE=4
          ELSEIF((EPSS.GE.1.95).AND.(EPSS.LT.2.80))THEN
          IE=5
          ELSEIF((EPSS.GE.2.80).AND.(EPSS.LT.4.50))THEN
          IE=6
          ELSEIF((EPSS.GE.4.50).AND.(EPSS.LT.6.20))THEN
          IE=7
          ELSEIF(EPSS.GE.6.20)THEN 
          IE=8
        ENDIF

        call eclose(QDIR,0.00,0.01,closea)
        IF(closea)THEN
          suneff=0.
          skyeff=(ASKY(IE)+CSKY(IE)*COS(ZENN)+DSKY(IE)*LOG(DELTT))
        ELSE
          suneff=(ASUN(IE)+CSUN(IE)*EXP(5.73*ZENN-5.)+DSUN(IE)*DELTT)
          skyeff=(ASKY(IE)+CSKY(IE)*COS(ZENN)+DSKY(IE)*LOG(DELTT))
        ENDIF
      endif 
    
      return
      end
