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

C      mznuma - effectively a simulation clock.

C ******************** mznuma

C The main controller for the numerical simulation.

      subroutine mznuma
#include "building.h"
#include "tdf2.h"
#include "monitor.h"
#include "uncertainty.h"

      common/outin/iuout,iuin
      common/filep/ifil
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      common/tc/itc,icnt
      common/trc/itrc

      common/c1/ncomp,ncon
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/c6/indcfg

      COMMON/PREC7/ITCNST

      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      common/shad1/iml

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/clim/idif(mt),itmp(mt),idnr(mt),ivel(mt),idir(mt),
     &            ihum(mt),idiff,itmpf,idnrf,ivelf,idirf,ihumf
      common/sus/issny

      common/btime/btimep,btimef

      common/pvala/tpa(mcom),qpa(mcom)
      common/fvala/tfa(mcom),qfa(mcom)
C Time-step controller data.
      common/ts/itsflg,ntscpd,itssch(msch,5),itsmus
      common/ts1/nf(msch),ifl(msch,mcvt1),fd(msch,mcvt1),itscf1,
     &           nmax1(msch)
      common/ts2/nf2(msch),ifl2(msch,mcvt2),fd2(msch,mcvt2),
     &           nmax2(msch),ipass,ipassc,tasav(mcom),qasav(mcom)
      common/ts3/itsv(msch)
      common/ts4/itsitr(msch),itrpas
      COMMON/TS6/idysv,ihrsv,irwcou,tsclaw,rewnd,nsncv1,nsncv2,pasone

C Monitor commons.
      common /bmontr/mnzn,mzone(mcom),montrb,yminb(2),ymaxb(2),
     &               ibplot,irecpb(MDPH),mbstv

C TDF commons. zrotp is the previous rotation angle for each zone.
      COMMON/TDFFLG/ITEMSTD
      COMMON/TDFFLG2/IALLCLM,ICASUAL(MCOM),IZIVENT(MCOM),IRAIRVL(MCOM),
     &       ISETPTT(MCOM),ICTLSTA(MCOM),ISKYLUX,IDBTEXT,IWINDVL,
     &       IWINDDR,IRELHUM,IDIFHSL,IDIRSOL,IGLOHSL,IOBJVEL,IOBJDIR,
     &       ISKYTMP,IGLOVRT,ICASUAL3(MCOM)
      common/zrotat/zrotp(mcom)

C Common for thermophysical property replacement.
      common/tpmod/itpmod(mcom)

C Power simulation flag.
      COMMON/PWRFLG/IPWRSIM

C Common for 3D building model.
      COMMON/GR1D04/GAM,RGAM

      COMMON/GR3D100/BLDG3D,ZONE3D(MCOM)
      COMMON/GR3D108/L3DCVS(MCOM),L3DCNC(MCOM),L3DNDC(MCOM),L3DTAQ(MCOM)
      COMMON/GR3D109/L3DNDD(MCOM),L3DTLW(MCOM),L3DLWV(MCOM)
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3
      COMMON/GR3D130/IFST(MNOD3),W3(MCFF3),NCLMN(MCFF3),NXT(MCFF3),SML3
      COMMON/GR3D131/IAIR,IPLT,IPRT,IAVL,NZMCF
      COMMON/GR3D150/INDX3,DTIME

      COMMON/GRND100/GRND3D
      COMMON/GRND101/NNODTG,ITCNSTG,TEMP3G(MCOM),AREAGD(MCOM)
      COMMON/GRND108/LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD
      COMMON/GRND110/GRNDSP

      COMMON/GRSD100/IndxSt
      COMMON/GRSD120/RLnrSD,RRLnrSD
      COMMON/GRSD122/InSvST,ISTemp

      COMMON/MOIST01/MSTROK,MSTRZN(MCOM)
      COMMON/MOIST12/GAMm,RGAMm
      COMMON/MOIST13/RLXln,RRLXln,RLXlnF,RRLXlnF,RLXlnT,RRLXlnT
      COMMON/MOIST14/DTfalse,TRMfls,TRMflsp1

      common/bctl/ncf,ibsn(mcf,4),iban(mcf,3),nbcdt(mcf),
     &       ibcdv(mcf,mbcdt,2),nbcdp(mcf,mbcdt),tbcps(mcf,mbcdt,mbcdp),
     &       ibctyp(mcf,mbcdt,mbcdp),ibclaw(mcf,mbcdt,mbcdp),
     &       bmiscd(mcf,mbcdt,mbcdp,misc)
      common/glbctl/global,glbrwd
      common/sysctl/ipassr
      common/glbsav/ksave
      common/globct/qfutgb(mcf),tfutgb(mcf),tnpgb(mcf)

C Uncertainty common.
      COMMON/UATD/UACLIM(6,3),UACTL(7,4),UAMCSA(MNACT)

C Common for electrical file 
      COMMON/ENTFILE/ENTFLNAM,IENTXIST

C Additional commons for SHOCC.
      common/c20/nzsur(mcom),nztv(mcom)
C sname - surface name
C sotf - db surface type: 'OPAQ' or 'TRAN' or ...
C smlcn - db multilayered construction name
C svfc - db multilayered construction facing?: 'VERT' or 'SLOP' or 'FLOR' or 'CEIL' or ...
C sother - db surface attributes for 'other' side: 'SIMILAR' or 'EXTERIOR' or ...
      common/g5/sname(mcom,ms),sotf(ms),smlcn(ms),svfc(ms),sother(ms)

C SHOCC: iblnd - SHOCC blinds corresponding to ESP-r TMCs:
C   0, no; else, a f77 index [1,~] to access SHOCC zone blind data structure
      common/shocc/iblnd(mcom,ms)

C Path to SHOCC input files
C bSHOCCed     - logical flag whether project has SHOCC input
C bZoneSHOCCed - logical array whether zones have SHOCC input
C SHOCCshlFile - path to SHOCC .shp file for SHOCC'd project library
C SHOCCshzFile - path to SHOCC .shz file for each SHOCC'd zone
      common/SHOCCcfg/bSHOCCed,SHOCCshlFile,bZoneSHOCCed(mcom),
     &                SHOCCshzFile(mcom)
      logical bSHOCCed,bZoneSHOCCed
      character SHOCCshlFile*72,SHOCCshzFile*72

      character outs*124
      character*72 LSNAM,LPROJ,LGEOM,LSHAD,LUTIL,LTHRM
      CHARACTER*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ,L3DNDD,L3DTLW,L3DLWV
      CHARACTER*72 LGDCVS,LGDCNC,LGDNDC,LGDNDD,LGDTAQ,ENTFLNAM
      
C SHOCC: complement to 'g5' common
      character sname*12,sotf*4,smlcn*12,svfc*4,sother*15

C CWEC Solar radiation interpolation if solar radiation data is half-hour centered.
      COMMON/CWEC_SOL2/idif_next,idnr_next
      integer  idif_next         !- diffuse radiation data for 2nd hour of next day.
      integer  idnr_next         !- direct normal radiation data for 2nd hour of next day.

      dimension VAL(MBITS+2)
      logical itpmod,WDARDO,MSTROK,MSTRZN
      logical rewnd,irew,tsclaw,pasone
      LOGICAL BLDG3D,ZONE3D,GRND3D,GRNDSP
      logical global,glbrwd,close
      logical context

C Expected finish time variables.
      character*24 ectime

C Trace output.
      if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   itrace(40).ne.0) call edisp(itu,' entering subroutine mznuma')
C Determine the value for RGAM and RGAMm.
      RGAM=1.-GAM
      RGAMm=1.-GAMm
      RRLXln=1.-RLXln
      RRLXlnF=1.-RLXlnF
      RRLXlnT=1.-RLXlnT
      DTIME=3600./FLOAT(NTSTEP)
      TRMfls=DTIME/DTfalse
      TRMflsp1=TRMfls+1.0
      RRLnrSD=1.-RLnrSD

C Set initial temporal rotation for each zone before looping begins.
      do 222 i=1,ncomp
        zrotp(i)=0.
  222 continue

C Set flag for modification of thermophysical properties.
C itpmod is false: the original thermophysical properties are used.
C itpmod is true:  the replacement properties are used.
      do 223 i=1,ncomp
        itpmod(i)=.false.
  223 continue

C `issny' determines whether or not a simulation spans
C the new year period.
      issny=0

C THE FOLLOWING 25 LINES OF CODE SHOULD BE MOVED TO APPROPRIATE
C PRE-SIMULATION SUBROUTINES FOR SCANNING AND LOADING SHOCC
C DATA STRUCTURES (i.e. NOT HERE IN MAIN TIME CONTROLLER)

C Initialize SHOCC project tree.
      call initprj(ncomp)
      
      if(bSHOCCed) then

C Load SHOCC project library (database) and zone data and
C check if any zone TMCs are linked to SHOCC 'blinds'. If so
C iblnd(i,j) contains the index to the SHOCC zone blind array [1,~]
        call loadlib(SHOCCshlFile,len(SHOCCshlFile))
        do 5 i=1,ncomp
          if(bZoneSHOCCed(i)) then
            call loadzone(SHOCCshzFile(i),len(SHOCCshzFile(i)),i)
            do 6 j=1,nzsur(i)
              call blindcheck(i,sname(i,j),len(sname(i,j)),iblnd(i,j))
    6       continue
          endif
    5   continue
      endif

C Initialize HVAC variables
      call ZERO_HVAC_VAR

C Write simulation control information to the solutions
C library.
      if(indcfg.ne.2)call mzls2

C Invoke initialisation of climate data and
C zone future air temperatures.
      call mzini1

C Initialise control parameters as required by building pid controller,
      call mzibct

C Initialise  Global Control parameters.
      ipassr=1
      glbrwd=.false.

C Set simulation start and finish days: actual start
C day is prior to the requested start day (as determined
C in mztcon).  This is necessary to eliminate the effects
C of the assumed starting conditions.
      iss=isds-itcnst
      isf=isdf
      nhs=(isdf-isds+1+itcnst)*24
      inhs=0
      inhsi=0

C Has the start day been converted from a post-new year
C day to a pre-new year day.
      IF(ISS.LT.1)THEN
        iss=365-itcnst+isds
        isf=isf+365
        issny=1
      ENDIF

      iunit=ifil
C Simulation year from configuration, if old way uncomment next 3 lines.
C Determine simulation year from climate file.
C      irec=366
C      read(iunit,rec=irec,iostat=istat,err=1000)iyear

C Commence simulation: output header information and
C set `iml' to zero to ensure shading file is always
C read at start of simulation.
      call mzsimh(icon)

C Perhaps terminate simulation.
      if(icon.eq.0)goto 16
      iml=0
      nsinc=0

C Assign time-step control variables.
      mtsc=1
      iagain=0
      ntsctr=0
      itsok=0
      itsct=0
      itrflg=0
      itrpas=0

C For type 6 time-step controller, assign variables,
      irew=.false.
      call mzintc

C Save user specified time-step value for
C later use if time-step controller active.
      itstp1=ntstep

C Initialize the variable thermophysical properties flags.
      CALL MZITHP

C Initialise moisture modelling variables.
      DO 551 ICOMP=1,NCOMP
        IF(MSTRZN(ICOMP))CALL INITMZ(ICOMP)
  551 CONTINUE

C Initialize the sparse storage variables.
      IF(BLDG3D.OR.GRND3D)THEN
        CALL INIT3D
        DO 552 ICFF=1,MCFF3
          NXT(ICFF)=0
  552   CONTINUE
        NZMCF=1
      ENDIF

C WDARDO defines if calling mzwdar is required at building side.
      WDARDO=.TRUE.

C Perform the start up period for 3D ground model if invoked.
      IF(GRND3D.AND.ITCNSTG.GT.0)THEN
        WDARDO=.FALSE.
        GRNDSP=.TRUE.
        CALL EFOPRAN(ICVS,LGDCVS,3,1,IER)
        CALL EFOPRAN(ICNC,LGDCNC,7,1,IER)
        CALL EFOPRAN(INDD,LGDNDD,3,1,IER)
        CALL EFOPRAN(ITAQ,LGDTAQ,2,1,IER)

C Ground start up period.
        IF(NTSTEP.GT.1)THEN
          TSTEP=FLOAT(NTSTEP)
          CALL MZCOE2(TSTEP)
        ENDIF

C Building side time step.
        DTIME=3600.0
        CALL MZGSUP(ISS)
        IF(NTSTEP.GT.1)THEN
          TSTEP=1./FLOAT(NTSTEP)
          CALL MZCOE2(TSTEP)
        ENDIF
        CALL ERPFREE(ICVS,ISTAT)
        CALL ERPFREE(ICNC,ISTAT)
        CALL ERPFREE(INDD,ISTAT)
        CALL ERPFREE(ITAQ,ISTAT)
        NSINC=0
      ENDIF

C Flag indicating within ground start up period.
      GRNDSP=.FALSE.

C Building side time step.
      DTIME=3600./FLOAT(NTSTEP)

C If monitoring is on then while in start-up period, display message.
      if(montrb.eq.1.and.itcnst.gt.0) 
     & call usrmsg(' ',
     &     ' Monitoring will commence shortly, please wait...','-')

C Remember the 'start value' of the unix system clock.
      call getsecs(istime)
      iltime=istime

123   continue

C At startup, SHOCC is initially out of sync with ESP-r time
C (i.e. ESP-r new day starts at 1., while SHOCC starts at 0.)

C Setting startup day of week ...
      call edayr(iss,id,im)
      call eweekd(id,im,iyear,idwp)

C Setting SHOCC shbtimep & shbtimef
C (i.e. SHOCC versions of btimep & btimef ... only used at startup)
      shbtimep=0.
      shbtimef=1./float(ntstep)

C Setting SHOCC new day events ...
      call UpdateDaily(idwp,shbtimef)
      do 8 i=1,ntstep
        do 9 j=1,ncomp
          call UpdateOccupancy(j,shbtimep,shbtimef)
    9   continue
        shbtimep=shbtimef
        shbtimef=shbtimef+1./float(ntstep)
    8 continue

C Open the file for  structured mesh temperatures.
      IF(InSvST.EQ.1)open(unit=ISTemp,file='dump3d',status='unknown')

C Start simulation day-loop.
C (i =curr day, iss = sim start day, isf = finish day)
      do 10 i=iss,isf

C For type 6 time-step controller, loop until saved day, 
        if(irew.and.i.lt.idysv)goto 10

C `ntsctr' is used to count the number of TSC operations
C per day.  This is used only if TSC is active.
        if(itsflg.ne.0)ntsctr=1

C Set day `i' (the present day) and day `i+1' (the
C future day) actual day numbers.
        idyp=i
        idyf=i+1
        if(idyp.gt.365)THEN
          idyp=idyp-365
          idyf=idyp+1
        ENDIF
        if(idyf.eq.366)idyf=1

C What day of week is present and future day: Monday 1
C through Sunday 7.
        call edayr(idyp,id,im)
        call eweekd(id,im,iyear,idwp)
        call edayr(idyf,id,im)
        call eweekd(id,im,iyear,idwf)

C Establish climatic data for present day and first
C hour of future day: future day first.
        irec=idyf
        read(iunit,rec=irec,iostat=istat,err=1000)
     &    (idif(j),itmp(j),idnr(j),ivel(j),idir(j),ihum(j),j=1,24)
        idiff=idif(1)
        itmpf=itmp(1)
        idnrf=idnr(1)
        ivelf=ivel(1)
        idirf=idir(1)
        ihumf=ihum(1)

C Get values for next day to determine solar radiation data
C at hour 1 of next day (needed if solar radiation data is half-hour centered).
        idif_next = idif(2)
        idnr_next = idnr(2)


C Present day.
        irec=idyp
        read(iunit,rec=irec,iostat=istat,err=1000)
     &    (idif(j),itmp(j),idnr(j),ivel(j),idir(j),ihum(j),j=1,24)

C Check for uncertainty associated with climate data.
        UAFLG=0.
        do 66 IUA=1,6
          UAFLG=UAFLG+UACLIM(IUA,1)
 66     continue
        if (UAFLG.gt.0.01) call UAT03a(idyp)

C Establish shading/ insolation information for present day
C only: hour 1 of future day is assumed equal to hour 1 of
C present day even if present and future days are within
C different months.
        call mzshdo(idyp)
      
C Set SHOCC ishday to zero to signal a new day.
        ishday=0

C Continue simulation on an hour-by-hour basis.
        do 20 j=1,24

C For type 6 time-step controller, loop until saved hour.
          if(irew.and.j.lt.ihrsv)goto 20

C Set present and future hours.
          ihrp=j
          ihrf=j+1
          if(ihrf.eq.25)ihrf=1

C Check for uncertainty associated with control data.
          UAFLG=0.
          do 67 IUA=1,7
            UAFLG=UAFLG+UACTL(IUA,1)
 67       continue
          if (UAFLG.gt.0.01) call UAT01a(idyp,ihrp)

C Increment number of TSC operations counter if present hour
C is greater than finish hour for present TSC operation.
          if(itsflg.ne.0)then
            if(ihrp.gt.itssch(ntsctr,2).and.ntsctr.lt.ntscpd)
     &      ntsctr=ntsctr+1

c For type 6 time-step controller, reset ntsctr,
            if(tsclaw.and.ihrp.eq.24.and.i.eq.itssch(1,4))ntsctr=1

C Check if present hour lies within TSC duration for this
C operation.  If so then set `itsok' flag else reset it to
C 0. Also get timestep controller type (`itsct').
            itsok=0

C Set iteration flag for TSC type 4.
            itrflg=0
            if(ihrp.gt.itssch(ntsctr,1).and.ihrp.le.itssch(ntsctr,2)
     &.or.(tsclaw.and.itssch(ntsctr,1).eq.24))then
              itsok=1
              itsct=itssch(ntsctr,3)
            endif
          endif

C `itscf1' controls the climate look ahead as required by
C a type 1 time-step controller. If 1 it will ensure that
C the controller receives climate data for a time-row one
C time-step in advance of the current simulation time-row.
          itscf1=0

C Call type 1 time-step controller. Change `ntstep' if
C control variable gradient > user-specified value. If
C `ntstep' was modified then modify `c' & `qc' coefficients.
          if(itsct.eq.1.and.itsok.eq.1)then
            call tscon1(ntsctr)
            mtsc=ntstep/itstp1
            if(mtsc.ne.1)call coeff1(itstp1,ntstep)
          endif

C Call type 2 time-step controller.
   27     if(itsct.eq.2.and.itsok.eq.1)then

C Restore `c' & `qc' to their original values.
            if(mtsc.ne.1)call coeff1(ntstep,itstp1)
            call tscon2(iagain,itstp1,ntsctr)
            do 929 iii=1,ncomp
              tasav(iii)=0.0
              qasav(iii)=0.0
  929       continue
            mtsc=ntstep/itstp1

C If `ntstep' was modified then modify `c' & `qc' coefficients.
            if(mtsc.ne.1)call coeff1(itstp1,ntstep)
            if(iagain.eq.0)goto 28
          endif

C For type 3 timestep controller, get user specified time-step
C value for this hour.
          if(itsct.eq.3.and.itsok.eq.1)then
            ntstep=itsv(ntsctr)
            mtsc=ntstep/itstp1
            if(mtsc.ne.1)call coeff1(itstp1,ntstep)
          endif

C Process building zones first; if no building, n=1.
C Is building computational time-step less than one hour ?
          n=ntstep
          do 30 kk=1,n
            k=kk

  87        continue      
            if(glbrwd)then
              k=ksave
            endif

C `its' is the current time-step within an hour.
            its=k

c Global control: initialise commons, save/retrieve data.
            if(global.and.ipassr.eq.1)then
              do 107 ilaw=1,ncf
                qfutgb(ilaw)=0.
                tfutgb(ilaw)=0.
                tnpgb(ilaw)=0.
 107          continue
              glbrwd=.false.
              call SAVEPV
            elseif(global.and.ipassr.gt.1)then
              call RTRVPV
            endif

C For type 6 time-step controller, retrieve and save data,
            if(itsct.eq.6.and.itsok.eq.1)then
              if(i.ge.itssch(ntsctr,4).and.i.le.itssch(ntsctr,5))then
                call tscon6(i,j,ntsctr,irew)
              endif
            endif

C Increment the simulation counter for every user-specifed
C time-step.
            rem=amod(float(k),float(mtsc))
            call eclose(rem,0.00,0.0001,close)
            if(close)nsinc=nsinc+1

C Set the future time-row hour value.
            btimep=btimef
            if(nsinc.eq.1)btimep=1.0
            btimef=float(ihrp)+float(its)/float(ntstep)
      
C New day events in SHOCC occur when btimef first exceeds 24.0 ...
            if(btimef.gt.24.0)then
              if(ishday.eq.0)then
                call updatedaily(idwp,btimef)
                ishday=1
              endif
            endif

            if(btimef.gt.24.0)btimef=btimef-24.

C Determine climatic parameters at the present and
C future time row. If any of the temporal definition flags for
C boundary conditions have been set then use them in place of
C the normal climatic interpolation.
            itdfex=IALLCLM+IDBTEXT+IWINDVL+IWINDDR+IRELHUM+IDIFHSL+
     &         IDIRSOL+IGLOHSL+IOBJVEL
            if(itdfex.eq.0)then
              call mzclmp(1)
            elseif(itdfex.ge.1)then
              call mzclmpt(1)
            endif

C Determine for each component and surface, the defining
C index and the adjacent space temperature and radiation
C at the present and future time-rows.  Also set up plant
C input flux to zones due to plant component containment
C losses already established by the coefficient generators.
            call mzadjc

C Compute total external and internal window and door areas
C and transfer to results library; then initiate start-up
C nodal temperature assignments (at first time-step only).
 81         if(itsct.eq.4.and.itsok.eq.1)then
              call tscon4(itrflg,ntsctr)
            endif
      
            if(i.eq.iss.and.j.eq.1.and.k.eq.1.AND.WDARDO)then
              do 7 l=1,ncomp
                icomp=l
                call mzwdar(icomp)
                call mznasg(icomp)
    7         continue
            endif

C And compute all inside and 'outside' convection coefficients.
            call mzconv

C Update ground reflectivity depending on the model used (constant
C reflectivity; simple model; or advanced model)
            CALL MZGREF

C Consider each zone in turn.
            do 40 l=1,ncomp
              icomp=l

C Update SHOCC occupancy status.
              call updateoccupancy(icomp,btimep,btimef)

C Define the degree of zone model (1D or 3D).
              IF(ZONE3D(ICOMP))THEN
                INDX3=3

C For 3D zone model, open the associated files.
                CALL EFOPRAN(ICVS,L3DCVS(ICOMP),5,1,IER)
                CALL EFOPRAN(ICNC,L3DCNC(ICOMP),7,1,IER)
                CALL EFOPRAN(INDD,L3DNDD(ICOMP),4,1,IER)
                CALL EFOPRAN(ITAQ,L3DTAQ(ICOMP),2,1,IER)
                CALL EFOPRAN(ITLW,L3DTLW(ICOMP),5,1,IER)
                CALL EFOPRAN(ILWV,L3DLWV(ICOMP),4,1,IER)
              ELSE
                INDX3=1
              ENDIF

C Set `itsmus' if output is required at this time-row.
              itsmus=0
              rem=amod(float(k),float(mtsc))
              call eclose(rem,0.00,0.0001,close)
              if(close.and.itrflg.eq.0)itsmus=1

C If there is a temporal definition to rotate the problem description
C then read in the zone geometry, rotate the zone, update the solar
C angles and miscel zone data.
              if(IOBJDIR.ne.0)then
                IOB=IOBJDIR

C << temporal rotation of model disabled pending re-expression in tabular form >>
C DBTAG was read in via system configuration file and input.
                CALL RCTDFB(itrc,btimef,VAL,ISD,IOB,IER)
                OBROT=VAL(ISD)

C The current rotation is different from the last, force each zone to
C be rotated (about the site origin) and then recompose the matrix.
                call eclose(OBROT,zrotp(icomp),0.0001,close)
                IF(.NOT.close)THEN

C Debug...
C                 if(itrc.gt.1)then
                    write(outs,'(A,F6.2)')'proceeding with rotation..',
     &                OBROT
                    CALL EDISP(IUOUT,outs)
C                 endif
                  igu=ifil+1
                  call egomin(igu,LGEOM(icomp),icomp,1,itrc,iuout,IER)
                  call escrot(OBROT,0.,0.)

C Recalculate time invariant zone data.
                  CALL MZAREA(icomp)
                  call mzpazi(icomp)
                  call mzmisc(icomp)

C Display surface information after rotation.
C                 if(itrc.gt.1) then
                    context=.true.
                    call surinfo(icomp,iuout,context)
C                 endif

C Remember the rotation.
                  zrotp(icomp)=OBROT
                endif
              endif

C For 1D zone model.
              IF(INDX3.EQ.1)THEN

C Variable thermophysical properties simulation.
                CALL MZVTHP(ICOMP)

C Set the heat variables at the current time step "N" equal to 
C the future values "F".
                CALL SHTNEF(ICOMP)

C Compute all time-dependent heat injections and complete
C coefficient set-up for current zone.
                CALL MZCOE3(ICOMP)

C Structured mesh gridding.
                IF(IndxSt.EQ.ICOMP)THEN

C Set the present heat variables equal to the future values.
                  if (ITRPAS.eq.0) CALL MZLS5(ICOMP)
                  CALL MAINST(ICOMP)
                ELSE

C Modify the zone heat matrix to account for the heat of transformation
C if moisture transfere modelling is invoked.
                  IF(MSTRZN(ICOMP))CALL MZCOE5(ICOMP)

C Set the present heat variables equal to the future values.
                  if (ITRPAS.eq.0) CALL MZLS5(ICOMP)

C Call matrix handling routines depending on the location
C of the control point node.
                  CALL MTXCTL(ICOMP)

C Call the zone moisture calculations controller.
                  IF(MSTRZN(ICOMP))CALL MZMSTR(ICOMP)
                ENDIF

C For 3D zone model.
              ELSEIF(INDX3.EQ.3)THEN

C Update the temperature array.
                CALL UD1TMP(ICOMP)

C Set up the zone matrix.
                CALL MZCOE3(ICOMP)

C Set the present heat variables equal to the future values.
                if (ITRPAS.eq.0) CALL MZLS5(ICOMP)

C Update the temperature file.
                CALL UD2TMP

C Forward reduce the zone 3D matrix.
                CALL FORW3D(ICOMP,ISNSR,B1,B2,B3)

C Apply the control law.
                CALL CONT3D(ICOMP,ISNSR,B1,B2,B3)

C Back substitute the zone 3D matrix.
                CALL BACK3D

C Fill the associated 1D temperature and heat arrays.
                CALL FILL1D(ICOMP)
              ENDIF

C Transfer results to solution library: no transfer if first day
C of simulation not yet reached.
              iday=idyp

C Find number of data blocks per hour to be saved.
              ndbph=ncomp*itstp1
              call mzls3(icomp,iday,itsmus,ndbph)

C If TSC type 2 is active then find sum of averages for zone air
C temperature and zone air energy injection.
              if(itsct.eq.2.and.itsok.eq.1)then
                qma=(qpa(icomp)+qfa(icomp))*0.5
                qasav(icomp)=qasav(icomp)+qma
                tma=(tpa(icomp)+tfa(icomp))*0.5
                tasav(icomp)=tasav(icomp)+tma
              endif

C For 3D zone model, close the associated files.
              IF(INDX3.EQ.3)THEN
                CALL ERPFREE(ICVS,ISTAT)
                CALL ERPFREE(ICNC,ISTAT)
                CALL ERPFREE(INDD,ISTAT)
                CALL ERPFREE(ITAQ,ISTAT)
                CALL ERPFREE(ITLW,ISTAT)
                CALL ERPFREE(ILWV,ISTAT)
              ENDIF
   40       continue

C Perform ground simulation for the current time step
            IF(GRND3D)THEN
              CALL EFOPRAN(ICVS,LGDCVS,3,1,IER)
              CALL EFOPRAN(ICNC,LGDCNC,7,1,IER)
              CALL EFOPRAN(INDD,LGDNDD,3,1,IER)
              CALL EFOPRAN(ITAQ,LGDTAQ,2,1,IER)
              CALL UD1TMP(-1)
              CALL MZ3SU1(-1)
              CALL UD2TMP
              CALL FORW3D(-1,ISNSR,B1,B2,B3)
              TNP=B3/B1
              CALL SVQTMP(-1,ISNSR,TNP,0.)
              CALL BACK3D
              CALL FILL1G
              CALL ERPFREE(ICVS,ISTAT)
              CALL ERPFREE(ICNC,ISTAT)
              CALL ERPFREE(INDD,ISTAT)
              CALL ERPFREE(ITAQ,ISTAT)
            ENDIF

C Call the electrical network simulation controlling routine here.
            IF(IENTXIST.GT.0) THEN 
              CALL ENETSOLV
            ENDIF

C For type 6 time-step controller, possibly 'rewind' simulation,
            if(itsct.eq.6.and.itsok.eq.1)then
              if(i.ge.itssch(ntsctr,4).and.i.le.itssch(ntsctr,5))then
                call tscon6(i,j,ntsctr,irew)
                if(irew)goto 123
              endif
            endif

C For global control, increment pass counter.
            if(glbrwd)then
              ipassr=ipassr+1
              ksave=k
              goto 87
            else
              ipassr=1
            endif

C Building side now complete.
            if(indcfg.eq.1)goto 30

C Plant code in this position in bps version.

C For type 4 timestep controller, iterate until itrflg=0.
   77       if (itrflg.gt.0) goto 81
            ITRPAS=0
   30     CONTINUE

C Hour step now complete.
          if(itsct.eq.2.and.iagain.eq.1)goto 27
   28     continue

C Plot zone temperature values if monitor is active.
          if(montrb.eq.1.and.ibplot.eq.1)
     &             call zngrph(0,0,itstp1,2)

C Restore original values of `c' and `qc' array coefficients
C if they were modified. Also restore original time-step.
          if(mtsc.ne.1)then
            call coeff1(ntstep,itstp1)
            ntstep=itstp1
            mtsc=1
          endif

C Establish percentage completion (of simulation), adjust expected
C finish time and output appropriate message, but only if previous
C report was issued more than 5 seconds ago.
          inhs=inhs+1
          simcom=float(inhs)/float(nhs)
          call getsecs(ictime)
          istamp=0

          if(simcom.gt.0.95.and.inhsi.lt.6) then
            inhsi=inhsi+1
            iperc=95
            istamp=1
          else if(simcom.gt.0.75.and.inhsi.lt.5) then
            inhsi=inhsi+1
            iperc=75
            istamp=1
          else if(simcom.gt.0.50.and.inhsi.lt.4) then
            inhsi=inhsi+1
            iperc=50
            istamp=1
          else if(simcom.gt.0.25.and.inhsi.lt.3) then
            inhsi=inhsi+1
            iperc=25
            istamp=1
          else if(simcom.gt.0.05.and.inhsi.lt.2) then
            inhsi=inhsi+1
            iperc=5
            istamp=1
          else if(simcom.gt.0.01.and.inhsi.lt.1) then
            inhsi=inhsi+1
            iperc=1
            istamp=1
          end if

          if(istamp.ne.0.and.(ictime-iltime).ge.5) then
            iltime=ictime
            ietim=istime+(ictime-istime)*100/iperc

C Unix variant.
            write(outs,'(I4,A,A)') iperc,
     &      ' %   complete; expected finish time : ',
     &      ectime(ietim)
C NT variant.
C           write(outs,'(I4,A)') iperc,' %   complete; '
            call edisp(iuout,outs)
          end if

C Hour loop now complete.
   20   continue

C Day loop now complete.
   10 continue
   
C Close the file opened for dumping the structured mesh temperatures.
      IF(InSvST.EQ.1)close(ISTemp)

C Set start address of next solution-set in library.
      call mzls4

C Disable TSC and set `ntscpd' to zero. This is necessary since
C on return to the `SIMUL' menu, the TSC by default should be
C inactive.
      itsflg=0
      ntscpd=0

C Switch off building plot.
      if(montrb.eq.1) ibplot=0

C Trace output.
   16 if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   itrace(40).ne.0) call edisp(itu,' Leaving subroutine mznuma.')
      return

 1000 write(outs,'(A,I3)') ' mznuma: climate file error @ rec ',irec
      call edisp(iuout,outs)
      call epwait
      goto 16

      end

