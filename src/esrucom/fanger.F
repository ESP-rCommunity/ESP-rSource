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
C   letdrivr
C   LETINIT
C   LETLOOP
C   PPD
C   VDD
C   LETINDEX
C   FANGER

c ******************** letdrivr ********************

c John B. Pierce Foundation Copyright reference:

c             Gagge, Fobelets & Berglund
c             Standard Predictive Indices ...
c             Paper PO-86-14-1
c             Appendix 2, ASHRAE Transactions
c             Vol. 92, Pt 2, 1986.

c Program evaluates the physiological and sensory responses of
c human subjects to their thermal environment.
c The following members of the Pierce Lab. have made major
c contributions to the present model over the past 20 years:
c Berglund, Casby, Fobelets, Gagge, Gonzales, Hardy, Nishi, Oohori,
c Stolwijk, and others unnamed as visiting fellows.

c PART I:  Main Program 'LETDRIVR'

c Read input data file, compute appropriate variables when two
c alternatives exist for one input, call the initial values
c subroutine, call the minute by minute simulation of physiological
c responses, call the indices subroutine when specified by user,
c write output data file.
c Variables in common block LET1 are input variables.

      subroutine letdrivr(ta,erftr,vel,rhpa,act,clo,set,icmfrt,
     &                    pme,pmv,xppd)

      real lr,icl,im,kclo
c     character*10 word(52)

#include "gagge.h"

c Data entry: variables in common block LET1.
      ttsk   = 33.7
      ttcr   = 36.8
      csw    = 170.0
      cstr   = 0.5
      cdil   = 200.0
      bz     = 0.1
      skbfl  = 90.0
      rgswl  = 500.0
      tski   = 33.7
      tcri   = 36.8
      alphai = 0.1
      skbfi  = 6.3
      eski   = 7.3
      we     = 0.0
      kclo   = 0.15
      evef   = 1.0

C Patch from Cor 11/93.
      icl    = 0.45
      ata    = 1.0
      chcc   = 0.0
      itime  = 60
      isteps = 60
      ida    = 2
      idb    = 1
      idc    = 1
      idd    = 1

c Select ERF or TR (IDA=1 for ERF input, 2 for TR).
      if(ida.eq.1) then
         erf=erftr
         tr=ta+erf/4.33
 20      trold=tr
         tr=ta+erf/(.72*4.*5.67e-8*((tr+ta)/2.+273.15)**3)
         if(abs(tr-trold).gt.0.01) goto 20
      else
         tr=erftr
         erf=.72*4.*5.67e-8*((tr+ta)/2.+273.15)**3*(tr-ta)
      endif

c Select RH or PA (IDB=1 for RH input, 2 for PA).
      if(idb.eq.1) then
         rh=rhpa
         pa=rh*vdd(ta)
      else
         pa=rhpa
         rh=pa/vdd(ta)
      endif

c Select CLO or CLOE (IDC=1 for CLO input, 2 for CLOE).
c CLO = intrinsic clothing insulation, CLOE = effective cloth. insul.
c CLO mode : ref. point for heat tr. calc. is at clothing surface.
c Skin + clothing area is greater than skin area alone by factor FACL.
c CLOE mode : reference point is at skin surface; FACL=1.
c Note : KCLO=0 --> CLO=CLOE,  KCLO>0 --> CLO>CLOE.
c Need CLOE or CLO and FACL for first evaluation of CHR and TSK
c in LETLOOP.  After first iteration, all calculations use CLO.
      cloe=clo
      facl=1.
      if(idc.eq.1) facl=1.+kclo*clo
      chclo=1.e5
      if(clo.gt.0.01) chclo=1./(.155*clo)

c Select ICL or IM (IDD=1 for ICL input, 2 for IM input).
      if(idd.eq.2) im=icl

c Call initial values subroutine.
      call letinit(ta,vel,act,clo)

c Simulation of body temperature regulation.  Loop (1 min. time increments).
c itime,itim = total exposure time, integrated time since start.
      itim=0
  100 itim=itim+1
      call letloop(ta,act,clo)

c Compute indices only every isteps minutes.
      if(mod(itim,isteps).ne.0) goto 30
      call letindex(ta,vel,rhpa,act,clo,set,pme,pmv)

      xppd = ppd(pmv)   !get value from function ppd

      x=0.
      time=float(itim)

 30   continue
      if(itim.lt.itime) goto 100

c Determination of the verbal category.
        if (pmv.ge.-0.5.and.pmv.le.0.5) icmfrt = 5
        if (pmv.gt.0.5.and.pmv.lt.0.8) icmfrt = 6
        if (pmv.ge.0.8.and.pmv.lt.2.0) icmfrt = 7
        if (pmv.ge.2.0.and.pmv.lt.3.5) icmfrt = 8
        if (pmv.ge.3.5.and.pmv.lt.4.5) icmfrt = 9
        if (pmv.ge.-0.8.and.pmv.lt.-0.5) icmfrt = 4
        if (pmv.ge.-2.0.and.pmv.lt.-0.8) icmfrt = 3
        if (pmv.ge.-3.5.and.pmv.lt.-2.0) icmfrt = 2
        if (pmv.ge.-4.5.and.pmv.lt.-3.5) icmfrt = 1
        if (pmv.lt.-4.5.or.pmv.gt.4.5) icmfrt = 12

      return
      end

c ******************** LETINIT ********************

c PART II: Initial Values Subroutine 'LETINIT.f'

c Compute initial physiological and heat transfer variables.
c Variables in common block LET2 are computed in this routine.

      subroutine letinit(ta,vel,act,clo)

      real lr,icl,im,kclo
#include "gagge.h"

c Set initial values for physiological variables:
c initial conditions at physiological thermal neutrality and
c steady state when default values are used,
c or final values of previous run when environments are chained.
      alpha=alphai
      tsk=tski
      tcr=tcri
      tbm=alpha*tsk+(1.-alpha)*tcr
      skbf=skbfi
      esk=eski

      if(clo.le.0.)then
         eveff=.38*vel**(-.29)
         icl=1.
      else
         eveff=.59*vel**(-.08)
      endif

c When input evef is positive, eveff=input value
      if(evef.gt.0.) eveff=evef
      if(ata.le.0.) ata=1.

c Activity level:
c activity may be entered in MET units or in W/m^2.
      if(act.lt.15.) act=act*58.2
      rm=act
      wk=we
      if(abs(we).lt.1.5) wk=wk*act

c All heat and mass transf. coef. here are expressed in W/K per sq.
c meter of skin surface.  Sk. surf. is evaluated by DuBois height-
c weight relation.
c First approximation for rad. heat tr. coeff. (fct of cloth. temp.)
c cloth.+ heat tr. coeff. at sea level.
      chr=4.7

c Calculation of conv. heat tr. coeff. (constant during exposure)
c chca corresponds to walking in still air
      chca=0.
      if((act/58.2).gt.0.85) chca=5.66*((act/58.2-.85)*ata)**.39

c chcv=room air movement.
      chcv=8.6*(vel*ata)**.53
      chc=amax1(chca,chcv)

c chc=3.0 still air sea level (corresponds to air velocity of .137 m/s).
      if(chc.lt.(3.*ata**.53))
     1   chc=3.*ata**.53
      if(chcc.gt.0.) chc=chcc

c Lewis relation at sea level and 25 C : lr=2.2 K/mmHg.
c 2.2 *760/101.325 = 16.50 K/kPa
      lr=16.50/ata

c ff=1 for air.  ff=.7 for helium at ATA>10.
      ff=1.

c If ata>10 correct chc, lr and ff for helium env.
      if(ata.ge.10.) then
         chc=2.*chc
         lr=.7*lr
         ff=.7
      endif

c Initial estimate of clothing temperature.
      tcl=(chclo*tsk+facl*(chc*ta+chr*tr))/(chclo+facl*(chc+chr))

      return
      end

c ******************** LETLOOP ********************

c PART III: Simulation of Body Temperature Regulation 'LETLOOP.f'

c Perform minute by minute integration of temperature regulation using
c physiological data from previous time step and current environmental
c conditions.
c Variables in common block LET3 are computed in this routine.

      subroutine letloop(ta,act,clo)
      real lr,icl,im,kclo
#include "gagge.h"

c Dry heat balance : solve for tcl and chr.
 21   tclold=tcl
      chr=4.*.72*5.67e-8*((tcl+tr)/2+273.15)**3
      tcl=(chclo*tsk+facl*(chc*ta+chr*tr))/(chclo+facl*(chc+chr))
      if(abs(tcl-tclold).gt.0.01) goto 21

c Heat flow from clothing surface to environment (facl=1. if cloe used).
      dry=facl*(chc*(tcl-ta)+chr*(tcl-tr))

c Compute transfer coefficients in terms of CLO (IDC=1 if CLO input).
c Dry heat exchange can be computed either in terms of CLO or CLOE
c in first iteration. CLO is used in all calculations afterwards.
      ctc=chr+chc
      if(idc.eq.1) then
         cloe=clo-(facl-1.)/(.155*facl*ctc)
      else
         clo=1.-kclo*cloe-kclo/(.155*ctc)
         clo=(-clo+sqrt(clo*clo+4.*kclo*cloe))/(2.*kclo)
         facl=1.+kclo*clo
      endif
      chclo=1.e5
      if(clo.gt.0.01) chclo=1./(.155*clo)

c Dry and latent respiratory heat losses.
c 5.8662 kPa=44 mmHg; .017251=.0023*760 mmHg/101.325 kPa
      eres=.017251*rm*(5.8662-pa)
      cres=.0014*rm*(34.-ta)*ata*ff

c Heat flows to skin and core (W/m^2).
      hfsk=(tcr-tsk)*(5.28+1.163*skbf)-dry-esk
      hfcr=rm-(tcr-tsk)*(5.28+1.163*skbf)-cres-eres-wk

c Thermal capacities (average man : 70 kg, 1.8 square meter).
c 58.2 W.min/(kg.K). Therm. capacities (TCCR, TCSK) in W.min/K.
      tccr=58.2*(1.-alpha)*70.
      tcsk=58.2*alpha*70.

c Temperature changes in 1 minute (DTSK, DTCR) in K/min.
      dtsk=(hfsk*1.8)/tcsk
      dtcr=(hfcr*1.8)/tccr
c      dtbm=alpha*dtsk+(1.-alpha)*dtcr
      tsk=tsk+dtsk
      pssk=vdd(tsk)
      tcr=tcr+dtcr

c Definition of vascular control signals and physiological
c temperature regulation. Physiological constants
c have been measured for average (unacclimatized) population
c (John B. Pierce Foundation, 1980-82).

c TTCR, TTSK, and TTBM and standard set points for core, skin and
c average body temperatures corresponding to physiol. neutrality
c Typical values for TTCR, and TTSK are 36.8, and 33.7.
c BZ is the stand. ratio of skin mass to total body mass (skin+core):
c BZ = 0.1.
c ALPHA is the actual ratio of skin mass to total body mass.
c Constants for vasodilation :     cdil = 200 liters/(m2.hr.K)
c               vasoconstriction : cstr = 0.1 dimensionless
c               reg. sweating  :   csw  = 170 g/(m2.hr).
c 6.3 liter/(m2.hr) is normal skin blood flow in the absence
c of any thermoregulatory vascular control.
c max. skin blood flow : skbfl = 90 liter/(m2.hr).
c maximum sweating rate : regswl = 500 g/m2.hr.

c Definition of vascular control signals.
      if(tsk.gt.ttsk) then
         warms=tsk-ttsk
         colds=0.
      else
         colds=ttsk-tsk
         warms=0.
      endif
      if(tcr.gt.ttcr) then
         warmc=tcr-ttcr
         coldc=0.
      else
         coldc=ttcr-tcr
         warmc=0.
      endif
      ttbm=bz*ttsk+(1.-bz)*ttcr
      tbm=alpha*tsk+(1.-alpha)*tcr
      if(tbm.gt.ttbm) then
         warmb=tbm-ttbm
         coldb=0.
      else
         coldb=ttbm-tbm
         warmb=0.
      endif

c Physiological temperature regulation.

c Control skin blood flow.
      dilat=cdil*warmc
      stric=cstr*colds
      skbf=(6.3+dilat)/(1.+stric)

c skbf is never below 0.5 liter/(m2.hr) nor above skbfl.
      if(skbf.lt.0.5) skbf=0.5
      if(skbf.gt.skbfl) skbf=skbfl

c Ratio of skin-core masses changes with skbf.
c (alpha,skbf)=(.15,6.3),(.45,1.24),(.05,90)
      alpha=.0417737+.7451832/(skbf+.585417)

c Control of regulatory sweating. REGSW in gr./m2.hr.
c 0.68 Wh/gr. is latent heat of evaporation of sweat.
      regsw=csw*warmb*exp(warms/10.7)
      if(regsw.gt.rgswl) regsw=rgswl
      ersw=.68*regsw

c Adjustment of metabolic heat due to shivering (Stolwijk, Hardy).
      rm=act+19.4*colds*coldc

c Evaluation of heat transfer by evaporation at skin surface.
c Mass transfer equation between skin and environment.
c RT is total vapor resistance of clothing + air layer.
c IM is efficency of mass transfer for (clothing + air layer)
c defined with respect to total (rad. and conv.) heat transfer.
c ICL is efficiency of mass transfer for clothing alone with respect
c to convective heat transfer.
c Reference : Woodcock, Breckenridge and Goldman.
      if (idd.eq.1) then
         rt=(1./lr)*(1./(facl*chc)+1./(chclo*icl))
         im=(1./(rt*lr))*(1./(facl*ctc)+1./chclo)
      else
         rt=(1./(im*lr))*(1./(facl*(chc+chr))+1./chclo)
         icl=facl*chc/(chclo*(rt*lr*facl*chc-1.))
      endif
      emax=(1./rt)*(pssk-pa)

c Air and hyperbaric environments.
      prsw=ersw/emax

c .06 is pdif for nonsweating skin --- Kerslake.
      pdif=(1.-prsw)*.06
      edif=pdif*emax
      esk=ersw+edif
      pwet=esk/emax

c Beginning of dripping (Sweat not evaporated on skin surface).
      if((pwet.ge.eveff).and.(emax.ge.0.)) then
         pwet=eveff
         prsw=(eveff-.06)/.94
         ersw=prsw*emax
         pdif=(1.-prsw)*.06
         edif=pdif*emax
         esk=ersw+edif
      endif

c When emax<0. condensation on skin occurs. Esk is latent heat
c exchange at skin surface.
      if(emax.lt.0.) then
         pdif=0.
         edif=0.
         esk=emax
         pwet=eveff
         prsw=eveff
         ersw=0.
      endif

c edrip = unevaporated sweat in air.
c edrip grams/sq.m/hr (swdrip on listing).
      edrip=(regsw*.68-prsw*emax)/0.68
      if(edrip.lt.0.) edrip=0.

c Vapor pressure at skin (as measured by dewpoint sensors).
      vpsk=pwet*pssk+(1.-pwet)*pa

c rhsk is skin relative humidity.
      rhsk=vpsk/pssk

      return
      end

c ******************** PPD ********************

c Funktie PPD berekent de PPD van Fanger aan
c de hand van de PMV-index.

        function ppd(pmv)

        pmv2 = pmv * pmv
        pmv4 = pmv2 * pmv2
        ppd = 100. - 95. * exp(-0.2179 * pmv2 - 0.03353 * pmv4)

        return
        end

c ******************** VDD ********************

c SVP subroutine: computes saturation water vapor
c pressure at saturation for given air temperature.
c input=degrees cel, output=kPa.

        function vdd(temp)
        vdd=exp(16.6536-4030.183/(temp+235.))
        return
        end

C ******************** LETINDEX ********************

C PART IV: Comfort and Sensory Indices 'LETINDEX.f'

C Compute environmental and comfort indices.
C Inputs to this routine are the physiological data from
C the temperature regulation simulation part.
C Variables in common block LET3 are computed in this routine.

      subroutine letindex(ta,vel,rhpa,act,clo,set,pme,pmv)

      real lr,icl,ias,ims,icls,im,kclo,kclos
#include "gagge.h"

C Statement Functions: calculation of ET* and SET* by Newton's
C iterative technique requires successive evaluations of the same
C function (ERR or ERRS).
      err(et1)=hsk-ctc*fcle*(tsk-et1)-pwet*lr*chc*facl*fpcl*
     &         (pssk-vdd(et1)/2.)
      errs(set1)=hsk-ctcs*fcles*(tsk-set1)-pwet*lr*chcs*facls*fpcls*
     &          (pssk-vdd(set1)/2.)

C DELTA is the infinitesimal increment for evaluation of derivatives
C in Newton's method.
      delta=1.e-4

C Section I : Real environment heat and mass transfer indices.

C In previous sections all heat and mass fluxes were calculated
C using the standard heat and mass transfer equations that can be
C found in any textbook (ASHRAE Handbook of Fundamentals, chapters 2
C and 3 for example).
C The following equations are specifiC to the field of thermal
C comfort (ASHRAE Handbook of Fundamentals, Chapter 8). Both
C methods lead to the same results. Some of the calculations are
C redundant but are kept for debugging purposes.

c Section I-a : Heat Transfer
c        temperature gradient (driving force) = tsk-to
c           to=ta+erf/ctc, ctc=chc+chr     effective radiant field mode
c           to=(chr*tr+chc*ta)/(chc+chr)   radiant temperature mode
c        dry heat transfer from skin to environment
c           dry=fcle*ctc*(tsk-to)    or   dry=facl*fcl*ctc*(tsk-to)
c           fcle=1./(1.+.155*ctc*cloe)    fcl=1./(1.+.155*facl*ctc*clo)
c           fcle=facl*fcl
c        effective sensible heat transfer coefficient
c           efctc=ctc*fcle                efctc=facl*fcl*ctc
c           also efctc=6.45/(clo+cloa),  cloa=1./(facl*ctc)
c Section I-b : Mass Transfer
c        pressure gradiant (driving force) = (pssk-pa)
c        latent heat transfer from skin to environment
c           esk=lr*facl*fpcl*chc*pwet*(pssk-pa)
c           fpcl=1./(1.+(.155/icl)*facl*chc*clo)
c        effective latent heat transfer coefficient
c           efche=lr*facl*fpcl*chc.  Also efche=lr*im*6.45/(clo+cloa)
c        icl and im are related:
c           im=fpcl*ia+(1.-fpcl)*icl,     ia=chc/ctc
c           1./im=fcl/ia+(1.-fcl)/icl
c        note: one can also define an effective mass transfer coeff.
c        however, the equation fpcle=facl*fpcl (analog of heat transfer)
c        is not valid: clo-cloe and fcl-fcle equations depend on the
c        total (radiative + convective) heat transfer whereas the
c        fpcl-fpcle relationship should depend on convection only.
c           fpcle=1./(1.+(.155/icl)*chc*cloe)
c           esk<>lr*fpcle*chc*pwet*(pssk-pa)

      to=ta+erf/ctc
      fcle=1./(1.+.155*ctc*cloe)
      fpcl=1./(1.+(.155/icl)*facl*chc*clo)
      efctc=ctc*fcle
      efche=lr*chc*facl*fpcl

c Section II : ET* (standardized humidity/actual CLO ,ATA and CHC).

c Calculation of heat storage: heat balance equation.
c Net metabolic rate - (respiratory heat loss) - (skin heat loss)
      store=(rm-wk)-(cres+eres)-(dry+esk)

c calculation of skin heat loss (HSK=DRY+ESK)
      hsk=ctc*fcle*(tsk-to)+pwet*lr*chc*facl*fpcl*(pssk-pa)

c HSK in ET* environment (50% rh) equals HSK in real environment.
c Get a low approximation for ET* and find ET* which
c satisfies the skin heat transfer equation by iteration.
c Newton's method : x new = x old - F(x)/F'(x) (from Taylor's series)
c ETOLD = old approx. for ET*, ERR (at the beginning of this routine)
c is the function of which the root (ET*) must be found.
c ERR is HSK in real environment minus HSK in ET* environment.
c (ERR2-ERR1)/DELTA is the numerical approximation to the derivative.
      etold=tsk-hsk/(ctc*fcle)
   90 continue
         err1=err(etold)
         err2=err(etold+delta)
         etnew=etold-delta*err1/(err2-err1)
         if(abs(etnew-etold).lt.0.01) goto 91
         etold=etnew
         goto 90
   91 et=etnew

c Section III : Standard effective temperature SET*
c Standardized humidity. CHC, CLO, ATA normalized for given activity.
c Same as ET* but normalize heat transfer coefficients.

c Standard environment.
      chrs=chr

c CHCS = standard conv. heat tr. coeff. (level walking/still air).
      chcs=0.
      if((act/58.2).gt.0.85) chcs=5.66*(act/58.2-.85)**.39

c Minimum value of chc at sea level = 3.0 (vel=.137 m/s).
      if(chcs.lt.3.) chcs=3.

c Standard MET-CLOS relation gives SET*=23.5 C when PMV=PMV*=0
c by using initial values, part II above.
      rn=rm-wk
      clos=1.5200/(rn/58.15+.6944)-.1835
      kclos=.25
      facls=1.+kclos*clos
      ctcs=chrs+chcs
      cloes=clos-(facls-1.)/(.155*facls*ctcs)
      fcles=1./(1.+.155*ctcs*cloes)
      fcls=1./(1.+.155*facls*ctcs*clos)

c IM or ICL can be selected as reference for standard environment.
c Here, IM is selected and ICL is derived from the Oohori relation.
c IMS = .45 for typical environment (per Breckenridge, 9/15/86)
c 1./IMS = FCLS/IAS + (1.-FCLS)/ICLS  (Oohori relation, 1985).
      ims=.45
      ias=chcs/ctcs
      icls=ims*ias*(1.-fcls)/(ias-fcls*ims)
      fpcls=1./(1.+(.155/icls)*chcs*clos*facls)

c HSK in SET* environment equals HSK in real environment.
c SET* using Newton's method (as in Section II, calculation of ET*).
      etold=tsk-hsk/(ctcs*fcles)
   92 continue
         err1=errs(etold)
         err2=errs(etold+delta)
         etnew=etold-delta*err1/(err2-err1)
         if(abs(etnew-etold).lt.0.01) goto 93
         etold=etnew
         goto 92
   93 set=etnew

c STO and SVPO are coordinates of psychrometric chart for
c Standard Environment.(standardized heat tr. coeff.)
c STO = standard operative temperature.
c DRY in STO-SVPO environment equals DRY in real environment.
      sto=ctc*fcle/(ctcs*fcles)*to+(1.-ctc*fcle/(ctcs*fcles))*tsk

c SVPO = standard operative vapor pressure.
c ESK in STO-SVPO environment equals ESK in real environment.
      svpo=(chc*facl*fpcl/(chcs*facls*fpcls))*pa+
     1 (1.-(chc*facl*fpcl)/(chcs*facls*fpcls))*pssk

c For standard environmental conditions with MET-CLO relation,
c TO is equal to STO.

c Section IV a : Fanger's comfort eqn. Predicted mean vote (PMV).

c ESW is evaporative heat loss by sweat only and is a measure of
c the heat load on the human body in zone of evaporative
c thermoregulation.
c Note: following eqn is the heat balance eqn with 0 storage.
      esw=rn-cres-eres-ctc*fcle*(tsk-to)-edif

c In cold climates, ESW is 0. ESW in eqn is replaced by negative
c STORE, i.e. rate of body cooling.
c In extremely warm climates, ESW=EMAX+STORE (rate of body heating).

c Fanger's reg. sweating at comfort threshold (PMV=0) is:
      ecomf=(rn-58.2)*.42
      if(ecomf.lt.0.) ecomf=0.

c PMV, the classic Fanger's index, is positive in warm climates.
c     pmv=(.303*exp(-.036*rm)+.028)*(esw-ecomf)
      closi = clo * 0.155
      call fanger(act,wk,closi,ta,tr,vel,pa,pmv)

c Section IV b : PMV* : rational alternative to Fanger's PMV in terms
c of ET* or SET instead of TO or STO to account for humidity.

c PMVET (PME in prgm) uses ET instead of TO.
c --- Gagge's new Tsk relation (21 june 1988, private communication)
      gtsk=33.103 + (0.4667 / 58.15) * rm * (1.-we)
      eswe=rn-cres-eres-ctc*fcle*(gtsk-et)-edif
      pme=(.303*exp(-.036*rm)+.028)*(eswe-ecomf)

c PMVSET (PMS in prgm) uses SET instead of STO.
      esws=rn-cres-eres-ctcs*fcles*(tsk-set)-edif
      pms=(.303*exp(-.036*rm)+.028)*(esws-ecomf)
c
c For standard environmental conditions with MET-CLO relation,
c PMVET is equal to PMVSET or PMV* as used in text.

c Section V : Warm indices derived from ESK, EMAX, W (skin wettedness)
c and WCRIT.

c EMAX is readjusted for EVEFF or PCRIT or ECRIT/EMAX.
      emax=emax*eveff

c DISC (discomfort) varies with relative thermoregulatory heat strain.
c Only valid for positive values of DISC (warm climates),
c does not apply to cold environments.
      disc=4.7*(ersw-ecomf)/(emax-ecomf-edif)

c Belding's classic heat stress index (HSI).
      ereq=rn-cres-eres-ctc*fcle*(tsk-to)
      hsi=100.*ereq/emax

c Belding's HSI is also an index of thermoregulatory strain.
c Heart rate (HR) is a fct of HSI and RM.
c HR data base (Gonzales, 1968) as follows:
c (RM,HSI,HR)=(1,10,75),(1,100,110),(3,18,92),(3,100,170).
      if(hsi.le.0.) then
         hr=1.885*rm/58.2+69.225
      else
         hr=(.281*rm/58.2+.108)*hsi+1.885*rm/58.2+69.225
      endif
      if(hr.gt.220.) hr=220.

c Section VI : Thermal sensation TSENS as funct. of mean body temp.
c TSENS is an alternate to PMV and DISC.
c This section not used in ASHRAE reference above.

c TBML is TBM when DISC and PMV are 0. (near lower limit of zone
c of evap.  regul.).
      tbml=(.194/58.15)*rn+36.301

c TBMH is TBM when HSI=100 (upper limit of zone of evap.regul.).
      tbmh=(.347/58.15)*rn+36.669

c TSENS=DISC=4.7*WCRIT when WCRIT (i.e. EVEFF) is less than unity.
c In cold, TSENS is a negative function of TBM.
c In extreme heat, when HSI>100, TSENS is a positive function of TBM.
      if(tbm.lt.tbml) then
         tsens=.4685*(tbm-tbml)
      elseif(tbm.lt.tbmh) then
         tsens=4.7*eveff*(tbm-tbml)/(tbmh-tbml)
      else
         tsens=4.7*eveff+.4685*(tbm-tbmh)
      endif

c For standard environment, EVEFF (WCRIT) is set at .85.
c For any standard environment at TA and 50% RH, PMVSET=TSENS.
c TSENS in the cold and the extreme heat as defined above is
c tentative and subject to modifications.

c Section VII : Other indices

c On Mollier chart, SLOPE is the slope of a locus of constant ET*
c or SET*. SLOPE in K/kPa.(ta=ordinate,pa=abscissa).
      slope=pwet*efche/efctc

c The line through ET* with slope equal to SLOPE on the Mollier
c chart can be viewed as a line of constant enthalpy of the total
c environment (taking into account clothing heat and mass transfer
c characteristics, skin wettedness and total dry heat transfer)
c for the characterization of the heat and mass transfer from a
c human subject.

c For the humid environment alone (no correction for skin wetted-
c ness, clothing, and radiative heat transfer) the psychrometric
c ratio is the Lewis Relation LR: 16.5 K/kPa (Combined heat and mass
c transfer from a flat wet plate due to convection).

c For the total environment (including clothing), the analogous
c psychrometric ratio is the slope corresponding to PWET = WCRIT,
c i.e.,the maximum wettedness.
c WCRIT*EFCHE/EFCTC or WCRIT*LR*IM

      return
      end

C ******************** FANGER ********************

C Subroutine to calculate Fanger's PMV index according
C to ISO 7730 - 1984.
      subroutine fanger(m,w,iclsi,ta,tr,var,pa,pmv)
      real m, iclsi   ! real parameter passed to fanger
      real mw  ! local variable
      eps = 0.00015
      mw = m - w

C Compute the corresponding fcl value.
      fclf=1.05+0.645*iclsi
      if (iclsi.lt.0.078) fclf=fclf-0.05+0.645*iclsi
      fcic=iclsi*fclf
      p2=fcic*3.96
      p3=fcic*100
      tra=tr+273.
      taa=ta+273.
      p1=fcic*taa
      p4=308.7-0.028*mw+p2*(tra/100)**4

C First guess for surface temperature.
      tcla=taa+(35.5-ta)/(3.5*(6.45*iclsi+0.1))
      xn=tcla/100
      xf=xn

      hcf=12.1*sqrt(var)
      noi=0

C Compute surface temperature of clothing by iterations.
 100  xf=(xf+xn)/2
      hcn=2.38*abs(100*xf-taa)**0.25
      hc=amax1(hcf,hcn)
      xn=(p4+p1*hc-p2*xf**4)/(100.+p3*hc)
      noi=noi+1
      if (noi.gt.150) goto 200
      if (abs(xn-xf).gt.eps) goto 100
      tclf=100*xn-273.

C Compute predicted mean vote.
      pm1=3.96*fclf*(xn**4-(tra/100)**4)
      pm2=fclf*hc*(tclf-ta)
      pm3=0.303*exp(-0.036*m)+0.028
      pm4=0.0
      if (mw.gt.58.15) pm4=0.42*(mw-58.15)
      pmv=pm3*(mw-3.05*0.001*(5733.-6.99*mw-pa*1000.)
     &-pm4-1.7*0.00001*m*(5867.-pa*1000.)-0.0014*m*(34.-ta)-pm1-pm2)

C Allow more extreme pmv to be reported.
C      if (abs(pmv).gt.3.0) goto 200
      goto 300
 200  pmv=999999.999

 300  continue
      return
      end

