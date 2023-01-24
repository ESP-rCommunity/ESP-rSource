C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C This file contains the following subroutine.
C mznuma - main controller for the numerical simulation.


C ******************** mznuma ********************
C Main controller for the numerical simulation.

      subroutine mznuma
      use h3kmodule
      USE TCC, ONLY:TCC_communication, COSIM
#include "H2_wind.h"
#include "SOFC.h"
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "plant.h"
#include "net_flow.h"
#include "tdf2.h"
#include "monitor.h"
#include "uncertainty.h"
#include "dhw_common.h"
#include "CETC_definitions.h"
#include "MultiYear_simulations.h"
#include "bc_data.h"
#include "lookup_data.h"
#include "control.h"
#include "roam.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"
#include "qrun.h"
#include "h3k_report_data.h"
#include "hvac_parameters.h"
#include "FMI.h"
#include "climate.h"

      integer lnblnk  ! function definition

      common/outin/iuout,iuin,ieout
      common/filep/ifil
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      common/tc/itc,icnt
      common/trc/itrc
      common/SAVE/ISAVE
      common/c1/ncomp,ncon
      common/c6/indcfg
      common/PREC7/ITCNST
      common/pstctr/nsincp,nsncpr
      common/pctstp/ntstpp
      common/pctinc/ipts
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      common/shad1/iml
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,idynow
      common/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/clim/idif(mt),itmp(mt),idnr(mt),ivel(mt),idir(mt),
     &            ihum(mt),idiff,itmpf,idnrf,ivelf,idirf,ihumf
      common/sus/issny
      common/btime/btimep,btimef
      common/ptime/ptimep,ptimef
      common/pctime/timsec
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
      common/TS6/idysv,ihrsv,irwcou,tsclaw,rewnd,nsncv1,nsncv2,pasone

C itsplt if set then transfer of mass flow results is required.
      common/outflg/itsplt

C Monitor directives.
      common /bmontr/mnzn,mzone(mcom),montrb,yminb(2),ymaxb(2),
     &               ibplot,irecpb(MDPH),mbstv
      common /montrv/mnpc,mcomp(mpcom),mcnod(mpcom),mstate,monitr,
     &               ymin,ymax

      common/zrotat/zrotp(mcom)

C Thermophysical property replacement.
      common/tpmod/itpmod(mcom)

C Air flow network.
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      CHARACTER LAPROB*72

C CFD domains.
      character*72 LCFD
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      real CFTIMS,CFTIMF               ! start and finish times for CFD assessment
      integer ICFDYS,ICFDYF            ! day-of-year for start and end of CFD
      integer ICFAUX                   ! helps to keep track of IFCFD()
      integer ICFVIEW                  ! toggle for preview via dfv during assessment
      integer ICFVIEWMINT,ICFVIEWMAXT  ! range of temperatures for dfv
      logical DFVinvoked
      COMMON/CFSEUP/CFTIMS,CFTIMF,ICFDYS,ICFDYF,ICFAUX(MCOM),ICFVIEW,
     &  ICFVIEWMINT,ICFVIEWMAXT,DFVinvoked
      COMMON/calcdCFD/mICORcfd(MCOM),mHCcfd(MCOM)

C Simulation start day required by cmp70c.
      common/simsdy/iss,isf

C Common for 3D building model.
      COMMON/GR1D04/GAM,RGAM

      COMMON/GR3D100/BLDG3D,ZONE3D(MCOM)
      LOGICAL BLDG3D,ZONE3D
      COMMON/GR3D108/L3DCVS(MCOM),L3DCNC(MCOM),L3DNDC(MCOM),L3DTAQ(MCOM)
      COMMON/GR3D109/L3DNDD(MCOM),L3DTLW(MCOM),L3DLWV(MCOM)
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3
      COMMON/GR3D130/IFST(MNOD3),W3(MCFF3),NCLMN(MCFF3),NXT(MCFF3),SML3
      COMMON/GR3D131/IAIR,IPLT,IPRT,IAVL,NZMCF
      COMMON/GR3D150/INDX3,DTIME

      COMMON/GRND100/GRND3D
      LOGICAL GRND3D
      COMMON/GRND101/NNODTG,ITCNSTG,TEMP3G(MCOM),AREAGD(MCOM)
      COMMON/GRND108/LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD
      CHARACTER*72 LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD
      COMMON/GRND110/GRNDSP

      COMMON/GRSD100/IndxSt
      COMMON/GRSD120/RLnrSD,RRLnrSD
      COMMON/GRSD122/InSvST,ISTemp

      COMMON/MOIST01/MSTROK,MSTRZN(MCOM)
      LOGICAL MSTROK,MSTRZN
      COMMON/MOIST12/GAMm,RGAMm
      COMMON/MOIST13/RLXln,RRLXln,RLXlnF,RRLXlnF,RLXlnT,RRLXlnT
      COMMON/MOIST14/DTfalse,TRMfls,TRMflsp1

      common/glbctl/global,glbrwd
      common/sysctl/ipassr
      common/glbsav/ksave
      common/globct/qfutgb(mcf),tfutgb(mcf),tnpgb(mcf)


C Specification of timestep averaging required
C so configuration can be passed to h3kreports.
      common/avrage/idaver

C Uncertainty common.
      COMMON/UATD/UACLIM(6,4),UACTL(MNACT,7,6),UAMCSA(MNACT)

C Common holding flag indicating presence of hvac data input file:
C ihvacflag=1 there is hvac file; ihvacflag=0 no hvac file.
      common/hvacinfo/ihvacflag,hvacfile

C Common holding flag indicating presence of gshp data input file:
C igshp = 1 there is a gshp file; igshp = 0 no gshp file.
      common/gshpinfo/igshp

C GCEP: Common holding flag indicating presence of gcep data input file:
C igcep = 1 there is a gcep file; igcep = 0 no gcep file.
      common/gcepinfo/igcep

C NumUNIT is the unit number of the binary files to be erased.
      INTEGER NumUNIT

C Counter to go through the unit numbers
      INTEGER CounterUNIT

C Logical function that returns true if the
C simulation is still in the startup period.
      LOGICAL bInStartup

C Active domain common.
      common/ACTDOM/CFDOK

C CFD coupling toggles.
      common/CFDTOGS/ACCON,CFD2BLD,CFD2MFS
      integer ACCON,CFD2BLD,CFD2MFS

C Common for results file names.
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),scfdres(MSPS),sipvres
      character sblres*72,sflres*72,splres*72,smstres*72,
     &  selres*72,scfdres*72,sipvres*72

C Additional commons for SHOCC.
C ZBASEA - area of base (m^2)
C IBASES - surfaces (up to MBL) associated with base area. If all 0 then
C          base area has been supplied by the user.
C Common block G5 documented in geometry.h
      COMMON/TRANSO/LTROR,PAZIO(MCOM,MS)

C SHOCC: iblnd - SHOCC blinds corresponding to TMCs:
C   0, no; else, a f77 index [1,~] to access SHOCC zone blind data structure
      common/shocc/iblnd(mcom,ms)

C Path to SHOCC input files.
C bSHOCCed     - logical flag whether project has SHOCC input
C bZoneSHOCCed - logical array whether zones have SHOCC input
C SHOCCshlFile - path to SHOCC .shp file for SHOCC'd project library
C SHOCCshzFile - path to SHOCC .shz file for each SHOCC'd zone
      common/SHOCCcfg/bSHOCCed,SHOCCshlFile,bZoneSHOCCed(mcom),
     &                SHOCCshzFile(mcom)
      logical bSHOCCed,bZoneSHOCCed
      character SHOCCshlFile*72,SHOCCshzFile*72

      character outs*124
      CHARACTER*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ,L3DNDD,L3DTLW,L3DLWV

      CHARACTER*72 hvacfile

      dimension VAL(MBITS+2)
      logical itpmod,WDARDO
      logical rewnd,irew,tsclaw,pasone
      LOGICAL GRNDSP
      logical global,glbrwd,close
      logical CFDOK
      logical context

C H3Kreporting object.
      CHARACTER*128 H3K_rep_NAME, H3K_rep_value

C Array of strings, string index and root name length for split of
C h3k (.csv) results file path.
      CHARACTER*32 LocalWords(12)
      integer*8 iRootLen
      integer IW

C Root file name for h3k (.csv) output
      character*72 cOutputRoot

C Expected finish time variables.
      character*24 ectime

C Flag for iteration between plant and electric power flow domains.
      LOGICAL plant_elec_converged

C Flag to enable inter-domain iteration.
      common / Plt_Elec_Net / bInter_domain_iteration
      logical bInter_domain_iteration

C Loop counters for cross domain iterations.
      common / Cross_Domain_Loop / plt_elec_loop_counter
      integer plt_elec_loop_counter

C Indicies of start, end, current & future indicies.
      integer iMY_start_yr_index, iMY_end_yr_index,
     &        iMY_current_yr_index, iMY_future_yr_index

C BASESIMP.
      common/bsmtdef/iBSIMP(MCOM),LBSIMP(MCOM)
      character LBSIMP*72

C Toggle for extreme silent running (endless) mode.
      logical endless,issilient
      common/endlessmode/endless,issilient

C Quick-run info.
      integer iOldSeasonIndex     !- Flags for tracking changes between seasons.
      save iOldSeasonIndex

C Flag for whether there is basesimp file.
      logical bsm_flag

C Plant network data
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      common/c9/npcomp,nci(mpcom),cdata(mpcom,mmiscd)

C Flag for whether plant network has storage tank that uses mains make-up water.
      logical itank_flag

      logical autook,ok
      integer ier,IClmErr
      logical unixok,concat
      character fs*1
      character llclmdb*144  ! to expand weather file
      integer lndbp          ! length of weather folder path

C Path to model.
      common/rpath/path
      character path*72

      integer agt_problem
      
C SUN F90 has spotty support for cpu_time intrinsic.
#ifdef F90
#else

C Store start time for reporting simulation duration.
      call cpu_time(sec_a)
#endif

      ier=0       ! set prior to use
      IClmErr=0

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Set the H3k reporting on/off
      IF(isReportingInstalled() .eqv. .false.) THEN
         call SetReportEnable(.false.)
      ENDIF

C During a co-simulation with TRNSYS, the path changes to where the TRNSYS deck file is
C located. This needs to be reset to the directory containing the cfg file to ensure
C the input.xml file can be read before generating output.
      CALL chdir(path)

C Update path to take account of this.
      write(path,'(a)')'./'

      IF(isH3KEnabled()) THEN

C Send the simulation information to the h3k report.
         Call UpdateH3kSimInfo(isd1,ism2,isd2,ism2,isds,isdf,ntstep)

C Initialise the h3k report variables.
         Call UpdateH3kReport

C Initialise fortran data variables for h3koutput.
         Call H3KSTORE_ZERO_DATA

C Set h3k output file name.
        call GETTOKENS(sblres(1),IW,LocalWords)
        iRootLen=lnblnk(LocalWords(IW))

C -4 implies a "." + 3-letter ending.
        iRootLen=iRootLen-4

        write(cOutputRoot,'(A)')LocalWords(IW)(1:iRootLen)
        CALL SetReportFilename(cOutputRoot(1:iRootLen))
      ENDIF

C Open file to store HVAC simulation results.
      call HVAC_OUTPUT_FILE

C Trace output.
      if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   itrace(40).ne.0) call edisp(itu,'Entering subroutine mznuma.')

C If computational time-step less than one hour
C modify coefficients computed in MZCOE1.
      IF(NTSTEP.GT.1)THEN
        TSTEP=1./FLOAT(NTSTEP)
      ELSE
        TSTEP = 1
      ENDIF

C Scale MZCOE1 coefficients as necessary.
      CALL MZCOE2(TSTEP)
      DTfalse=TSTEP*3600.0

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
C itpmod false: the original thermophysical properties are used;
C itpmod true: the replacement properties are used.
      do 223 i=1,ncomp
        itpmod(i)=.false.
  223 continue

C issny determines whether or not a simulation spans
C the new year period.
      issny=0

C << THE FOLLOWING 25 LINES OF CODE SHOULD BE MOVED TO APPROPRIATE
C PRE-SIMULATION SUBROUTINES FOR SCANNING AND LOADING SHOCC
C DATA STRUCTURES (i.e. NOT HERE IN MAIN TIME CONTROLLER) >>

C SHOCC project tree, if any zones have SHOCC check for
C multiyear and warn user.
      call initprj(ncomp)

      if(bSHOCCed) then
        if ( bMY_sim_enabled ) then
          call edisp(iuout,
     &   ' SHOCC does not currently support multi-year simulation!')
          STOP 'bmatsv.F: Fatal Error!'
        endif

C Load SHOCC project library and zone data and check if
C any zone TMCs are linked to SHOCC 'blinds'. If so
C iblnd(i,j) contains the index to the SHOCC zone blind array [1,~].
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

C Functional mock-up interface.
      if (is_FMU) then

C Initialise FMI and FMUs.
        call FMI_INIT(IER)

C Tell FMUs to start simulation.
        call FMI_SIMSTART(IER)
      else

C Ensure all control commons are initialised to "off".
        call FMI_CLEARCTL()
      endif

C Initialise agents.
      CALL AGT_INITSIM

C Initialise gremlins.
      CALL GRM_INITSIM

C Initialise HVAC variables.
      call ZERO_HVAC_VAR

C Initialise zone-to-plant casual gain coupling.
      call plt_zone_gain_zero()

C Write simulation control information to the results libraries.
      if(indcfg.ne.2)call mzls2
      if(indcfg.ne.1.and.(isave.ne.0.and.isave.ne.5))call mzpls2

C Write the CFD result library header.
      if (CFDOK) call cfdlibhead

C Invoke initialisation of weather data and zone future air temperatures.
      call mzini1

C Initialise control parameters as required by building PID controller.
      call mzibct

C Initialise global control parameters.
      ipassr=1
      glbrwd=.false.

C Set simulation start and finish days. Actual start
C day is prior to the requested start day (as determined
C in mztcon). This is necessary to eliminate the effects
C of the assumed starting conditions.
      iss=isds-itcnst
      isf=isdf
      nhs=(isdf-isds+1+itcnst)*24
      inhs=0
      inhsi=0

C If quick run method is requested, set the start date and end date of the
C simulation to span all periods of interest (all of the best weeks).
        if ( iQuick_run .eq. 1 ) then
          iOldSeasonIndex = 0
          iss = iFirst_day_num - itcnst
          isf = iLast_day_num
          nhs = ( iLast_day_num - iFirst_day_num + 1 + itcnst )*24
        endif

C Has the start day been converted from a post-new year
C day to a pre-new year day?
        IF(ISS.LT.1)THEN
          iss=365-itcnst+isds
        if ( iQuick_run .eq. 1 ) then
          iss = 365 - itcnst + iFirst_day_num
        endif
        isf=isf+365
        issny=1

C Adjust multi-year simulation start year to account for pre-new-year
C start-up period.
        if ( bMY_sim_enabled ) then
          iMY_Start_year = iMY_Start_year - 1
        endif

      ENDIF

C Determine indicies of start and end years, with respect to
C start and end of defined multi-year climate database.
      if ( bMY_sim_enabled ) then

C Start year (result can be less than 1).
        iMY_start_yr_index =
     &        iMY_start_year - iMY_clm_file_years ( 1 ) + 1

C End year (not used at present).
        iMY_end_yr_index = iMY_end_year - iMY_start_year

      endif

C Set the variable bSimThisDay (introduced for quick run method)
C if a quick run is not requested.
      if ( iQuick_run .ne. 1 ) then
        quickrun_markdays_loop: do iCount = iss, isf
             bSimThisDay(iCount) = .true.
        enddo quickrun_markdays_loop
      endif

C H3Kreporting object - save the frequency of building timesteps (simulations with different
C timesteps for multiple domains not presently supported).
      if ( isH3KEnabled() )  then

C Set flags indicating if h3kreports should (re)initialise.
        bFirst_call_bld = .true.
        do 88 i = 1, max_sys
          bFirst_call_hvac(i) = .true.
88       continue
        bFirst_call_fan = .true.
        bFirst_call_hrv = .true.
        bFirst_call_erv = .true.
        bFirst_call_dhw = .true.
        WRITE(H3K_rep_NAME,'(A)')'minutes_per_step'
        WRITE(H3K_rep_VALUE,'(f7.3)') 60./real(ntstep)

C Update meta-data.
        call SetReportParameter(
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &     H3K_rep_value(1:LNBLNK(H3K_rep_VALUE)))
        WRITE(H3K_rep_NAME,'(A)')'default_time_step_averaging'

C Set iadver = 1 to manually disable timestep averaging
C (or include '<time_step_averaging>false</time_step_averaging>'
C in input.xml )
        WRITE(H3K_rep_VALUE,'(I3)')  idaver

C Update meta-data.
        call SetReportParameter(
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &     H3K_rep_value(1:LNBLNK(H3K_rep_VALUE)))

        WRITE(H3K_rep_NAME,'(A)')'number_of_timesteps'
        WRITE(H3K_rep_VALUE,'(I9)') ((isf-iss+1))*NTSTEP*24

C Update meta-data.
        call SetReportParameter(
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &     H3K_rep_value(1:LNBLNK(H3K_rep_VALUE)))

      endif ! <- matches 'if ( isH3KEnabled() )...'

C Check to see if flexible boundary condition file has been defined,
C and process file if necessary.
      if ( bBC_data_defined ) then
        call process_BC_data_file(ifil,autook,bBC_data_err)
        if ( bBC_data_err ) bBC_data_defined = .false.
      endif


C Initialise DHW (this has been moved down below BCD initialisation
C to support a column check in the DHW_Initialise function
      call DHW_Initialize

C Check to see if lookup table data file has been defined and
C process file if necessary.
      if ( bLookup_data_defined ) then
        call process_Lookup_data_file(ifil,autook,
     &                            bLookup_data_err)
        if ( bLookup_data_err ) bLookup_data_defined = .false.
      endif

C Zero energy usage storage arrays.
      call ZeroEnergyStorageArrays()

C Commence simulation: output header information and set iml to zero
C to ensure shading file is always read at start of simulation if
C this calculation method is activated.
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

C If imonitor = 1 then plant state variables may be plotted.
      imonitor=0

C Save user specified time-step value for
C later use if time-step controller is active.
      itstp1=ntstep
      itstpp=ntstpp

C Initialise the variable thermophysical properties flags.
      CALL MZITHP

C Initialise moisture modelling variables.
      DO 551 ICOMP=1,NCOMP
        IF(MSTRZN(ICOMP))CALL INITMZ(ICOMP)
  551 CONTINUE

C Initialise the sparse storage variables.
      IF(BLDG3D.OR.GRND3D)THEN
        CALL INIT3D
        DO 552 ICFF=1,MCFF3
          NXT(ICFF)=0
  552   CONTINUE
        NZMCF=1
      ENDIF

C WDARDO defines if calling mzwdar is required at building side.
      WDARDO=.TRUE.

C Perform the start up period for 3D ground model if active.
      IF(GRND3D.AND.ITCNSTG.GT.0)THEN
        WDARDO=.FALSE.
        GRNDSP=.TRUE.
        ier=0
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

C Adjust matrix equations coefficients as partly set-up in MZCOE1.
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

C If monitoring is on then in start-up period display message.
      if((montrb.eq.1.or.monitr.eq.1).and.itcnst.gt.0)
     & call edisp(iuout,'Progress:')

C Remember the 'start value' of the unix system clock.
      call getsecs(istime)
      iltime=istime

123   continue

C At startup, SHOCC is initially out of sync with ESP-r time
C (ESP-r new day starts at 1, while SHOCC starts at 0.)

C Set startup day of week.
      call edayr(iss,id,im)
      call eweekd(id,im,iyear,idwp)

C Set SHOCC shbtimep & shbtimef
C (i.e. SHOCC versions of btimep & btimef ... only used at startup).
      shbtimep=0.
      shbtimef=1./float(ntstep)

C Setting SHOCC new day events.
      call UpdateDaily(idwp,shbtimef)
      do 8 i=1,ntstep
        do 9 j=1,ncomp
          call UpdateOccupancy(j,shbtimep,shbtimef)
    9   continue
        shbtimep=shbtimef
        shbtimef=shbtimef+1./float(ntstep)
    8 continue

C Open the file for structured mesh temperatures.
      IF(InSvST.EQ.1)open(unit=ISTemp,file='dump3d',status='unknown')

C Initilise basesimp file flag.
      bsm_flag = .false.
      do 11 ibsm_counter = 1,mcom
        if(iBSIMP(ibsm_counter).eq.1) then
          bsm_flag = .true.
        endif
 11   continue

C Check whether plant network has a storage tank that uses mains make-up
C water
      itank_flag = .false.
      do 12 itank_counter = 1,npcomp
        iplant_comp_type = NPCDAT(itank_counter,4)
        if((iplant_comp_type.eq.81).or.
     &     (iplant_comp_type.eq.82).or.
     &      (iplant_comp_type.eq.83).or.
     &     (iplant_comp_type.eq.113).or.
     &      (iplant_comp_type.eq.114)) then
          itank_flag = .true.
        endif
 12   continue

C If this is a single year simulation, open the weather file. During
C multi-year simulations, the weather files will be opened inside
C the simulation loop.
      if ( .not. bMY_sim_enabled ) then
        CALL CLMOPB(0,IClmErr)
      endif

C In the case that we have a GSHP input file, DHW input file, and/or basesimp
C input file then call routines to determine degree-days and average monthly
C ground temperature.
      if((igshp.eq.1).or.(iDHW_Flag.eq.1).or.
     &    bsm_flag.or.itank_flag) then
         call CLM_DEG_DAY

C Initialise ground temperature profile parameters.
         call Ground_Temp_Profile
      endif

C Start simulation day-loop.
C (i = curr day, iss = simulation start day, isf = simulation finish day)
      do 10 i=iss,isf

C Get seasonal information.
        seasonal_run_test: if ( bSimThisDay(i) ) then
        quick_run_init: if ( iQuick_run == 1 ) then
          iThisSeason = iSeasonIndex(i)
          if ( iOldSeasonIndex /= iThisSeason .and.
     &         iThisSeason /= 0 ) then
            call ReportNewSeason( iThisSeason,
     &                           fHtgMultiplier(iThisSeason),
     &                           fClgMultiplier(iThisSeason),
     &                           fGenMultiplier(iThisSeason)  )
            iOldSeasonIndex = iThisSeason
          endif
        endif quick_run_init

C For multi-year simulations, determine the current
C and future time row years, and the indices of these
C years with respect to the multi-year weather files.
        MY_sim_initialize: if ( bMY_sim_enabled ) then
          iMY_current_year = iMY_Start_year + int(float(i-1)/365.0)
          iMY_future_year = iMY_Start_year + int(float(i)/365.0)

C Assign global year (iYear), present year (iYear_p) and future year (iYear_f).
          iYear   = iMY_current_year
          iYear_p = iMY_current_year
          iYear_f = iMY_future_year
          iMY_current_yr_index = iMY_start_yr_index
     &                + ( iMY_current_year - iMY_Start_year )
          iMY_future_yr_index = iMY_start_yr_index
     &                + ( iMY_future_year - iMY_Start_year )

C Are indicies within the defined weather file coverage? If not, adjust
C indicies to the first/last records in the file.
          if ( iMY_current_yr_index .lt. 1 ) iMY_current_yr_index = 1
          if ( iMY_current_yr_index .gt. iMY_clm_file_count )
     &           iMY_current_yr_index = iMY_clm_file_count
          if ( iMY_future_yr_index  .lt. 1 ) iMY_future_yr_index = 1
          if ( iMY_future_yr_index  .gt. iMY_clm_file_count )
     &           iMY_future_yr_index  = iMY_clm_file_count

        else
          iYear_p = iYear
          iYear_f = iYear
        endif MY_sim_initialize

C For type 6 time-step controller, loop until saved day,
        if(irew.and.i.lt.idysv)goto 10

C ntsctr is used to count the number of time step control (TSC) operations
C per day (used only where TSC is active).
        if(itsflg.ne.0)ntsctr=1

C Set day `i' (the present day, idyp) and day `i+1' (the
C future day, idyf) actual day numbers.
C Ensure present & future day numbers are between 1 & 365
        if ( bMY_sim_enabled ) THEN
          idyp = i - ( iMY_current_year - iMY_start_year ) * 365
          idyf = i + 1 - ( iMY_future_year - iMY_start_year ) * 365
        else
          idyp=i
          idyf=i+1

          if ( i.gt.1 ) then
            iYrShift_P = int( float(i-1)/365.0  )
          else
            iYrShift_P = 0
          endif

          iYrShift_F = int( float(i)/365.0 )

          idyp = i     - 365 * iYrShift_P
          idyf = i + 1 - 365 * iYrShift_F
        endif

C Save a copy of i for routines that need the 'real' day.
        iMY_real_day = i

C What day of week are the present and future days: Monday 1
C through Sunday 7.
        call edayr(idyp,id,im)
        call eweekd(id,im,iyear_p,idwp)
        call edayr(idyf,id,im)
        call eweekd(id,im,iyear_f,idwf)

C Establish weather data for present day and first hour of future day: future
C day first. The weather file contains records in the order specified by CMCOL 
C in climate.h (all data stored as integers). If this is a multi-year simulation,
C open the approprate weather file.
        if ( bMY_sim_enabled )then
          if (.not. bMY_clm_file_open ( iMY_current_yr_index ) ) then

C Close previous weather file if necessary.
            if ( iMY_current_yr_index .gt. 1 ) then
              if ( bMY_clm_file_open ( iMY_current_yr_index - 1) )
     &         call ERpFree( iClim , istat )
            endif

C Open new weather file.
            ier=0
            LCLIM=cMY_clm_file_names(iMY_current_yr_index)
            CALL CLMOPB(1,IER)

C Set flag.
            bMY_clm_file_open ( iMY_current_yr_index ) = .true.

C If there is a GSHP input file, DHW input file and/or basesimp
C input file, call routines to determine degree-days and monthly average
C ground temperatures.
            if((igshp.eq.1).or.(iDHW_Flag.eq.1).or.
     &        bsm_flag.or.itank_flag) then
              call CLM_DEG_DAY
              call Ground_Temp_Profile
            endif
          endif
        endif

C Present day.
        CALL CLMGET(idyp,IER)
        CALL CLMDN2O

C Note that the 'future' year may correspond to another weather file.
C If this is a multi-year simulation, we may need to open a new file.
        if ( bMY_sim_enabled ) then
          if ( .not. bMY_clm_file_open ( iMY_future_yr_index ) ) then
            if ( iMY_future_yr_index .gt. 1 ) then
              if ( bMY_clm_file_open ( iMY_future_yr_index - 1) )
     &        call ERpFree( iClim , istat )
            endif
            ier=0
            CALL CLMOPB(0,IER)
            bMY_clm_file_open ( iMY_future_yr_index ) = .true.
          endif
        endif

C Check for uncertainty associated with weather data.
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

C Flag signaling new SHOCC day.
        ishday=0

C *** Continue simulation on an hour-by-hour basis.
        do 20 j=1,24 

C For type 6 time-step controller, loop until saved hour.
          if(irew.and.j.lt.ihrsv)goto 20

C Set present and future hours. If incrementing at midnight set
C idynow to present day plus one so that the 0h00-1h00 period gets
C the correct day.
          ihrp=j
          ihrf=j+1
          idynow=idyp         ! idynow is normally the same as idyp
          if(ihrf.eq.25)then
            ihrf=1
            idynow=idyp+1     ! crossed midnight so increment
            if(idynow.eq.366) idynow=1
          endif

C Check for uncertainty associated with control data. The
C UACTL array will be zero during the base case run but will
C be populated in subsequent runs. Loop through available
C uncertainty actions to identify which are related to control.
          do loop=1,NIACT
           ichgtyp=0
           if(IACTD(loop,1).gt.0)then
             ichgtyp=IDTYPU(IACTD(loop,1))
           endif
           if (ichgtyp.eq.1001) then
             iact=loop 
             UAFLG=0.
             do 67 IUA=1,7
               UAFLG=UAFLG+UACTL(iact,IUA,1)
 67          continue
             if (UAFLG.gt.0.01)then
               call UAT01a(iact,idyp,ihrp)
             endif
           endif
         enddo

C Increment number of TSC operations counter if present hour
C is greater than finish hour for present TSC operation.
          if(itsflg.ne.0)then
            if(ihrp.gt.itssch(ntsctr,2).and.ntsctr.lt.ntscpd)
     &        ntsctr=ntsctr+1

c For type 6 time-step controller, reset ntsctr.
            if(tsclaw.and.ihrp.eq.24.and.i.eq.itssch(1,4))ntsctr=1

C Check if present hour lies within TSC duration for this
C operation. If so then set itsok flag else reset it to 0
C Also get timestep controller type (itsct).
            itsok=0

C Set iteration flag for TSC type 4.
            itrflg=0
            if(ihrp.gt.itssch(ntsctr,1).and.ihrp.le.itssch(ntsctr,2)
     &.or.(tsclaw.and.itssch(ntsctr,1).eq.24))then
              itsok=1
              itsct=itssch(ntsctr,3)
            endif
          endif

C TSC7 - Time-step iteration between plant and electrical domains.
C << Will be generalised later to be consistent with the treatment of other time-step
C iteration approaches. Specifically, it will be supported as a simulation toggle
C and will allow time-step iteration between plant and electrical as well as
C between these domains and the building thermal domain. For now, force TSC7
C if both the DG controller and the electrical domains are active.
          IF( bInter_domain_iteration .and. IENTXIST==1 ) THEN
            itsct = 7
          END IF

C itscf1 controls the weather look-ahead as required by
C a Type 1 time-step controller. If 1, it will ensure that
C the controller receives weather data for a time-row one
C time-step in advance of the current simulation time-row.
          itscf1=0

C Call Type 1 time-step controller. Change ntstep if control
C variable gradient > user-specified value. If ntstep was
C was modified then modify the c & qc matrix coefficients.
          if(itsct.eq.1.and.itsok.eq.1)then
            call tscon1(ntsctr)
            mtsc=ntstep/itstp1
            if(mtsc.ne.1)call coeff1(itstp1,ntstep)
          endif

C Call Type 2 time-step controller.
   27     if(itsct.eq.2.and.itsok.eq.1)then

C Restore c & qc coefficients to their original values.
            if(mtsc.ne.1)call coeff1(ntstep,itstp1)
            call tscon2(iagain,itstp1,ntsctr)
            do 929 iii=1,ncomp
              tasav(iii)=0.0
              qasav(iii)=0.0
  929       continue
            mtsc=ntstep/itstp1

C If ntstep was modified then modify c & qc coefficients.
            if(mtsc.ne.1)call coeff1(itstp1,ntstep)
            if(iagain.eq.0)goto 28
          endif

C For Type 3 timestep controller, get user-specified time-step
C value for this hour.
          if(itsct.eq.3.and.itsok.eq.1)then
            ntstep=itsv(ntsctr)
            mtsc=ntstep/itstp1
            if(mtsc.ne.1)call coeff1(itstp1,ntstep)
          endif

C *** Case for sub-hour building time steps. 
          n=ntstep     ! if no building, n=1
          do 30 kk=1,n
            k=kk
  87        continue
            if(glbrwd)then
              k=ksave
            endif

C its is the current time-step within an hour.
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

C For Type 6 time-step controller, retrieve and save data.
            if(itsct.eq.6.and.itsok.eq.1)then
              if(i.ge.itssch(ntsctr,4).and.i.le.itssch(ntsctr,5))then
                call tscon6(i,j,ntsctr,irew)
              endif
            endif

C Increment the simulation counter for every user-specifed time-step.
            rem=amod(float(k),float(mtsc))
            call eclose(rem,0.00,0.0001,close)
            if(close)nsinc=nsinc+1

C Set the future time-row hour value.
            btimep=btimef
            if(nsinc.eq.1)btimep=1.0
            btimef=float(ihrp)+float(its)/float(ntstep)

C Activate gremlins.
            if (.not.bInStartup()) CALL GRM_CTL(ier)

C New day events in SHOCC occur when btimef first exceeds midnight.
            if(btimef.gt.24.0)then
              if(ishday.eq.0)then
                call updatedaily(idwp,btimef)
                ishday=1
              endif
            endif

            if(btimef.gt.24.0)btimef=btimef-24.

C H3Kreports: send simulation time to reporting object.
            if ( isH3KEnabled() ) Then
              call H3K_Report_time(
     &          btimep, btimef, ihrp, ihrf, idyp, idyf, its, ntstep,
     &          nsinc )
            endif

C Fuel costs: reset monthly fuel consumption.            
            if ( bIncFuelCostCalcs ) then
                  call ResetSumFuelUseArray()
            end if

C Output current simulation increment if trace is on.
            if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &         itrace(33).ne.0)then
              write(outs,'(A,I3)')' Simulation increment ',nsinc
              call edisp(itu,outs)
            endif

C Determine weather parameters at the present and future time row.
            call mzclmpt(1)

            if(indcfg.eq.2)goto 401

C Calculate the heat injections associated with plant component heat losses to
C associated zone locations.
            call plt_zone_gain_calc()

C Call DHW simulation controller (placed here prior to casual gains
C so that any additional heat transfer to a zone can be added.
            if(iDHW_Flag.eq.1) then
              call DHW_Main
            endif

C Determine for each component and surface, the defining
C index and the adjacent space temperature and radiation
C at the present and future time-rows. Also set up plant
C input flux to zones due to plant component containment
C losses as determined from the plant-side solution.
            call mzadjc

C Start of iterative loop for stationary time.
   81       if(itsct.eq.4.and.itsok.eq.1)then
              call tscon4(itrflg,ntsctr)
            endif

C Actions at first simulation time-step only.
            if(i.eq.iss.and.j.eq.1.and.k.eq.1.AND.WDARDO)then
              do 7 l=1,ncomp
                icomp=l

C Compute total external and internal window and door areas
C and transfer to building results file. 
                call mzwdar(icomp)

C Initiate start-up nodal temperature assignments.
                call mznasg(icomp)
    7         continue
            endif

C Set itsmus if output is required at this time-row.
            itsmus=0
            rem=amod(float(k),float(mtsc))
            call eclose(rem,0.00,0.0001,close)
            if (close.and.itrflg.eq.0) itsmus=1

C Access functional mock-up interface, e.g. for occupant behaviour modelling.
            if (is_FMU) then
              if (.not.bInStartup()) then
                call FMI_SIMSTEP(IER)
              else
                call FMI_CLEARCTL()
              endif
            endif

C Skip CFD if it has been disabled. Otherwise check CFD coupling toggles
C to determine if adaptive conflation control (ACC) or 2-way CFD-building
C conflation is active (the latter is checked in CFMNGE). 
            if (CFDOK) then
              outs='not yet'
              DO 15 ICP=1,NCOMP
                IF(ICFAUX(ICP).NE.0)THEN
                  if(ACCON.eq.0)then     ! ACC disabled 

C Set conflation type to 1.
                    if(abs(ICFAUX(ICP)).gt.1)
     &                ICFAUX(ICP)=sign(1,ICFAUX(ICP))
                  endif
                  if(CFD2BLD.eq.2)then   ! 2-way CFD-building conflation activated

C Set conflation type to 5.
                    if(abs(ICFAUX(ICP)).eq.4)
     &                ICFAUX(ICP)=sign(5,ICFAUX(ICP))
                  endif

C Activate CFD model during a required period by resetting IFCFD.
                  if(icfaux(icp).gt.0) then
                    if(idyp.lt.isds) then
                      IFCFD(ICP)=ICFAUX(ICP)
                    else
                      IFCFD(ICP)=0
                    endif
                  endif

C Need CFD results for this time step?
C 1st test clause    - present day is within defined range.
C OR test 2nd clause - future time is the first time step of CFD start day,
C                      and present time is still in the previous day.
                  IF((IDYP.GE.ICFDYS.AND.IDYP.LE.ICFDYF).or.
     &    (IDYP.eq.(ICFDYS-1).and.CFTIMS.eq.1.0.and.btimef.eq.1.0))THEN
                    if (btimef.le.1.0) then
                      rtmp=btimef+24.
                    else
                      rtmp=btimef
                    endif

C Ensure IFCFD is negative so that SURFCONF will write out results.
                    IF(rtmp.GE.CFTIMS.and.abs(IFCFD(ICP)).eq.0)
     &                IFCFD(ICP)=-abs(ICFAUX(ICP))

C CFD solution has finished.
                    IF(IDYP.eq.ICFDYF.and.rtmp.GT.CFTIMF)THEN
                      IFCFD(ICP)=0
                      ICFAUX(ICP)=0
                      if(outs.eq.'not yet')then
                        write(outs,'(2a)')
     &                    'CFD solver has now completed for the ',
     &                    'selected period.'
                        call edisp(iuout,' ')
                        call edisp(itu,outs)
                        write(outs,'(2a)')'Disabling CFD for the ',
     &                    'remainder of the simulation.'
                        call edisp(itu,outs)
                      endif
                    ENDIF
                  ENDIF

                ENDIF

C Set flag indicating that surface-convection correlation indices have
C not yet been calculated.
                mICORcfd(ICP) = 0

C Set flag indicating HCs have not been calculated by CFD for the current
C zone for the current time step: this will be reset if CFD is invoked.
                mHCcfd(ICP) = 0

C Invoke CFD if there is `surface conflation' between the thermal and CFD domains. 
                IF( ABS(IFCFD(ICP)).eq.1 .or. 
     &             (ABS(IFCFD(ICP)).eq.4 .or. 
     &              ABS(IFCFD(ICP)).eq.5) )THEN
                  CALL SURFCONF(ICP)
                ELSEIF(IFCFD(ICP).ne.0)then
                  write(outs,'(a,i2)')'Unrecognised conflation mode: ',
     &              ABS(IFCFD(ICP))
                  call edisp(itu,outs)
                  write(outs,'(a,I3,a)')'CFD for building zone ',ICP,
     &              ' disabled.'
                  call edisp(itu,outs)
                  IFCFD(ICP)=0
                  ICFAUX(ICP)=0
                  outs='not yet'
                ENDIF
  15          CONTINUE

            endif

C Compute all inside and 'outside' surface convection coefficients.
            call MZCONV

C Update ground reflectivity depending on active model.
            CALL MZGREF

C Execute complex fenestration control to determine state of shading
C layers for the current time step.
            call CFC_control()

C *** Consider each building zone.
            do 40 l=1,ncomp
              icomp=l

C Update SHOCC occupancy status.
              call updateoccupancy(icomp,btimep,btimef)

C Define the zone constructions model degree (1D or 3D).
              IF(ZONE3D(ICOMP))THEN
                INDX3=3

C For 3D model, open the associated files.
                ier=0
                CALL EFOPRAN(ICVS,L3DCVS(ICOMP),5,1,IER)
                CALL EFOPRAN(ICNC,L3DCNC(ICOMP),7,1,IER)
                CALL EFOPRAN(INDD,L3DNDD(ICOMP),4,1,IER)
                CALL EFOPRAN(ITAQ,L3DTAQ(ICOMP),2,1,IER)
                CALL EFOPRAN(ITLW,L3DTLW(ICOMP),5,1,IER)
                CALL EFOPRAN(ILWV,L3DLWV(ICOMP),4,1,IER)
              ELSE
                INDX3=1
              ENDIF

C If there is a temporal definition to rotate the problem description
C then read in the zone geometry, rotate the zone, and update the solar
C angles and miscel zone data.
              if(IOBJDIR.ne.0)then
                IOB=IOBJDIR

C DBTAG was read in via system configuration file and input.
                CALL RCTDFB(itrc,btimef,VAL,ISD,IOB,IER)
                OBROT=VAL(ISD)

C Current rotation is different from the last, force each zone to
C be rotated (about the site origin) and then recompose the matrix
C coefficients.
                call eclose(OBROT,zrotp(icomp),0.0001,close)
                IF(.NOT.close)THEN
C                 if(itrc.gt.1)then
                    write(outs,'(A,F6.2)')'Proceeding with rotation.',
     &                OBROT
                    CALL EDISP(IUOUT,outs)
C                 endif
                  igu=ifil+1
                  call georead(igu,LGEOM(icomp),icomp,1,iuout,IER)
                  call escrot(OBROT,0.,0.)

C Recalculate time invariant zone data.
                  call mzarea(icomp)
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

C If roaming is activated then rotate the zone if applicable. Rotation
C using roaming is considered to be better than using temporal
C definitions because only azimuth angles for surfaces will change and
C file read/write and various calls to subroutines will not be performed.
              IF(IROAM.EQ.1)THEN
                HOUR=BTIMEF
                S_TIME=REAL(IDYP)+HOUR/24.0        ! Calculate simulation time
                DO 130 IPDR=1,NPDR

C Calculate roaming day of year.
                  IRoamD=LDYS(IPDR)
                  IRoamM=LMTS(IPDR)
                  CALL EDAY(IRoamD,IRoamM,IYDNRoam)
                  RTIME=REAL(IYDNRoam)+XLHRS(IPDR)/24.0 ! Calculate roaming time
                  IF(S_TIME.LT.RTIME)THEN               ! Roaming not started yet
                    CONTINUE
                  ELSEIF(S_TIME.GE.RTIME)THEN
                    DO 131 ISurf=1,MS
                      SPAZI(ICOMP,ISurf)=PAZIO(ICOMP,ISurf)+LORNT(IPDR)
                      IF(SPAZI(ICOMP,ISurf).GT.360.)
     &                SPAZI(ICOMP,ISurf)=SPAZI(ICOMP,ISurf)-360.
                      LTROR=LORNT(IPDR)
 131                CONTINUE
                  ENDIF
 130            CONTINUE
              ENDIF

C For 1D zone constructions model.
              IF(INDX3.EQ.1)THEN

C Variable thermophysical properties.
                CALL MZVTHP(ICOMP)

C Set the temperature and plant injection variables at the present
C time row equal to the future values prior to replacing the latter.
                CALL SHTNEF(ICOMP)

C Compute all future time-row heat injections and complete
C coefficient set-up for current zone.
                CALL MZCOE3(ICOMP)

C Structured mesh gridding.
                IF(IndxSt.EQ.ICOMP)THEN

C Set the present time-row variables equal to the future values.
                  if (ITRPAS.eq.0) CALL MZLS5(ICOMP)
                  CALL MAINST(ICOMP)
                ELSE

C Modify the zone heat balance matrix to account for the heat of transformation
C if moisture transfer modelling is active.
                  IF(MSTRZN(ICOMP))CALL MZCOE5(ICOMP)

C Set the present time-row variables equal to the future values.
                  if (ITRPAS.eq.0) CALL MZLS5(ICOMP)

C Call matrix handling routines depending on the location
C of the control point node(s).
                  CALL MTXCTL(ICOMP)

C Call optical control.
                  call mzoctl(icomp)

C Call the zone moisture calculations controller.
                  IF(MSTRZN(ICOMP))CALL MZMSTR(ICOMP)
                ENDIF

C For 3D zone constructions model.
              ELSEIF(INDX3.EQ.3)THEN
                CALL UD1TMP(ICOMP)  ! update the temperature array
                CALL MZCOE3(ICOMP)  ! set up the zone matrix
                if (ITRPAS.eq.0) CALL MZLS5(ICOMP) ! set present variables equal
                                                   ! to future values
                CALL UD2TMP         ! update the temperature file
                CALL FORW3D(ICOMP,ISNSR,B1,B2,B3)  ! forward reduce the zone 3D matrix
                CALL CONT3D(ICOMP,ISNSR,B1,B2,B3)  ! apply the control law
                CALL BACK3D         ! back substitute the zone 3D matrix
                CALL FILL1D(ICOMP)  ! fill the 1D temperature and heat arrays
              ENDIF

C Transfer data to building results file if after the start-up period.
              iday=idyp

C Find number of data blocks per hour to be saved.
              ndbph=ncomp*itstp1

              IF (ISAVE.eq.5)then
                call surbal(icomp)
                call H3KSTORE_SAVELEVEL5(icomp,iday,itsmus,ndbph)
              ELSE
                call mzls3(icomp,iday,itsmus,ndbph)
              ENDIF

C Transfer miscellaneous simulation data to H3K reporting object.
              if ( isH3KEnabled() ) then
                 call H3K_transport_build_data(icomp)
                 if (IAIRN .ne. 0) THEn
                    call H3K_transport_mfn_data()
                 endif
                 if ( MSTROK ) then
                    call H3K_transport_mould_data(icomp)
                 endif

C << insert (conditional) call to explicit CFC h3k output function here >>
              endif

C If TSC Type 2 is active then find sum of averages for zone air
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

C CFC extensive reporting output (for debugging)
C              call CFCoutput(icomp)

C CFC designated h3k output function (U-value ...)
              call CFCoutputH3K(icomp)

   40       continue   ! end of building zones loop

C If H3K plant system model defined, commence simulation.
            if(ihvacflag.eq.1) then
              call START_HVAC_SIM(i)
            endif

C Transfer weather data to reporting object.
            if (isH3KEnabled()) call H3K_transport_climi_data()

C Perform ground simulation for the current time step.
            IF(GRND3D)THEN
              ier=0
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

C For Type 6 time-step controller, possibly 'rewind' the simulation.
            if(itsct.eq.6.and.itsok.eq.1)then
              if(i.ge.itssch(ntsctr,4).and.i.le.itssch(ntsctr,5))then
                call tscon6(i,j,ntsctr,irew)
                if(irew)goto 123
              endif
            endif

C Reset h3k zone control action flags.
            call h3kReports_reset_zone_flags()


C Beginning of ESP-r plant and electrical domain simulation.
            if(indcfg.eq.1)goto 77   ! no plant domain

C First time step only: assign starting values of plant component
C nodal variables, assign plant time-step counter and plant control
C loop PID control common digpid.
  401       if(i.eq.iss.and.j.eq.1.and.k.eq.1)then
              CALL MZNASP

C Initialise plant time-step counters.
              nsincp=0
              nsncpr=0

C Initialise control parameters as required by plant PID controllers.
              call mzipct
            endif

C Number of seconds per plant time step.
            timsec=3600.0/(float(ntstep)*float(ntstpp))

C Zeroise zone-to-plant casual gain coupling arrays.
            call plt_zone_gain_zero()

C ** Commence plant network simulation (there may be one or more
C plant time-steps ithin each building simulation time-step).
            do 100 L=1,ntstpp

C Adjust timer.
              ipts=L
              ptimep=ptimef
              if(nsinc.eq.1.and.L.eq.1)ptimep=1.0
              its0=its-1
              ptimef=float(ihrp)+float(its0)/float(ntstep)
     &            +float(ipts)/float(ntstpp*ntstep)
              if(ptimef.gt.24.0)ptimef=ptimef-24.0
              nsincp=nsincp+1   ! increment plant time-step counter.

C Assign weather variables.
              call mzclmpt(2)

C Reset flag to indicate whether iteration is required between the plant and
C electrical domains in order to converge the solution (plant_elec_converged
C is true if no further iteration is required).
              plant_elec_converged = .FALSE.

C Call subroutine XXX_TimeRow_Manipulate to store present time-row
C data. This permits the plant and electrical domains to be rewound
C in time, which is necessary where inter-domain iteration is active
C within the time-step. Rewinding is also necessary in the case of
C co-simulation with TRNSYS when ESP-r's plant (and eventually electrical)
C domain is invoked multiple times per time-step by the harmonizer.
C iSaving tells XXX_TimeRow_Manipulate to store data for the present time row.
              if ( itsct.eq.7 ) then
                call Plt_TimeRow_Manipulate(iSaving)
                call Pfs_TimeRow_Manipulate(iSaving)
              endif
              IF ( COSIM ) THEN
                call Plt_TimeRow_Manipulate(iSaving)
                call Pfs_TimeRow_Manipulate(iSaving)
              END IF

C Initalise the loop counter at the beginning of each plant time step.
              plt_elec_loop_counter = 1

C Calls to Plt_TimeRow_Manipulate() and Pfs_TimeRow_Manipulate()
C to invoke the 'iWinding' operation are embedded inside the solution of
C the plant and electrical domains. Thus, if either domain is called
C outside the context of bps (and specifically, this iteration loop) then
C the state-variables will be handled normally.
              plant_elec_loop: DO WHILE( .not. ( plant_elec_converged))

C Set up and solve the plant matrix.
                call mzpmrx(itera)

C If co-simulation with TRNSYS is active then pass data to and receive data from
C TRNSYS via the Harmonizer. This is accomplished via the call to function
C TCC_communication. The returned value from TCC_communication determines whether
C the Harmonizer is satisfied with the convergence between ESP-r and TRNSYS. If
C the Harmonizer imposes another invocation of ESP-r's plant domain then: increment
C the counter tracking the number of invocations per time-step; rewind the plant
C domain's present time-row variables to the state they were in at the first
C invocation of this time-step; and then invoke the plant domain again.
                IF (COSIM) THEN
                  IF (TCC_communication()== 0) THEN
                    plt_elec_loop_counter = plt_elec_loop_counter + 1
                    call Plt_TimeRow_Manipulate(iRewinding)
                    CYCLE plant_elec_loop
                  END IF
                END IF

C If necessary, iterate. *** NOT YET ACTIVE ***
C The variable `ITERA' is currently hard-code to zero. Its intention is to force
C iteration between the building and plant domains, but the necessary logic required
C to evaluate whether there is a need for iteration has not been implemented.
C If this block of code is activated in the future it will have to be moved
C since it resides within a do loop: there cannot be a goto jump within a do loop.
c              if(itera.ne.0)then
c                goto 81
c              endif

C Call the electrical network simulation controlling routine for a
C building + plant solution.
                IF(IENTXIST.GT.0) THEN
                  CALL ENETSOLV
                ENDIF

C If the time-step iteration between the plant and electrical domains is active
C then check whether a converged solution has been achieved. If further iteration
C is required then `plant_elec_converged' will be set to FALSE. If the time-step
C iteration between the plant and electrical domains is not active (e.g. if the
C electrical domain is not in use) then set `plant_elec_converged' to TRUE to
C prevent repeating the plant solution this time-step.
                IF ( itsct .eq. 7 ) THEN
                  CALL TSCON7(plant_elec_converged)
                ELSE
                  plant_elec_converged = .TRUE.
                END IF

C Check to see if plant-electrical domain solutions have converged. If not,
C use subtroutine XXX_TimeRow_Manipulate(iOperation) to 'rewind' plant
C and electrical domain historical data to the point where iteration was
C ajourned. Use flag 'iRewinding' to indicate that historical data should
C be restored.
                if (.not. plant_elec_converged ) then
                  call Plt_TimeRow_Manipulate(iRewinding)
                  call Pfs_TimeRow_Manipulate(iRewinding)
                  plt_elec_loop_counter = plt_elec_loop_counter + 1
                endif
              ENDDO plant_elec_loop

C Store plant results.
              iday=idyp

C Imonitor should be set to 1 then plant state variables will be
C displayed.
              itsplt=0

C Do not write plant libraries if save-levels 0 or 5 are active.
              if ( ISAVE .ne. 5 .and. ISAVE .ne. 0) then
                if(NINT(amod(float(L),float(mtsc))).eq.0.and.
     &            (itrflg.eq.0.or.itrflg.eq.2)) then
                  call mzpls3(iday,imonitor)
                  itsplt=1
                  if(monitr.eq.1.and.imonitor.eq.1)
     &              call mgraph(0,0,itstpp*itstp1,2)
                endif
              endif

C Write out plant results.
              if ( isH3KEnabled() ) call H3KSTORE_Plant_TimeSteps()

C Post-process and save electrical network results.
              IF(IENTXIST.GT.0) then

C Database is not written if save-levels 0 or 5 are active.
                if ( ISAVE .ne. 0 .and. isave .ne.5 ) then
                  CALL ENETLIBSV
                endif

C Call h3k electrical data transport routines.
                if ( isH3KEnabled() )
     &                call H3K_transport_electric_data()
              endif

C End of solution of plant and electrical domains for the current plant time-step.
C Return to `do 100' to solve the plant and electrical domains for the next
C plant time-step.
  100       continue

C For Type 4 timestep controller, iterate until itrflg=0.
   77       if (itrflg.gt.0) goto 81
            ITRPAS=0

C Plant side now complete.
C Call the electrical network simulation controlling routine here for a
C building-side only electrical network solution (i.e. no plant).
            IF(IENTXIST.GT.0.AND.INDCFG.EQ.1) THEN
              CALL ENETSOLV

C Database is not written if save-levels 0 or 5 are active.
              if ( ISAVE .ne. 0 .and. isave .ne.5 ) then
                CALL ENETLIBSV
              endif
              if ( isH3KEnabled() ) THEN
                call H3K_transport_electric_data()
              ENDIF
            ENDIF

C For global control, increment pass counter.
            if(glbrwd)then
              ipassr=ipassr+1
              ksave=k
              goto 87
            else
              ipassr=1
            endif

C Save boundary condition data, if it exists.
            if ( isH3KEnabled() ) call report_BC_data()

C Call SiteUtilities procedure to characterise purchased energy usage.
            call SiteUtilities()

C H3Kreports.(UPDATE).start
C Call reporting object update function to process and
C store logged data.

C Check to see present time is close to midnight
            if (ihrp.eq.24) then
              myday = iday + 1
              if (myday .gt. 365) myday = 1
            else
              myday = iday
            endif

C Update reports.
            if ( bInStartup() .or. ( iStart_flag(i) .eq. 1 ) ) then
              iStartupFlag = 1
            else
              iStartupFlag = 0
            endif

C << NOTE: dummy call REP_UPDATE only has 3 parameters >>
            call ReportNextTimeStep(nsinc,btimep,myday,iStartupFlag,
     &                              iQuick_run)    ! <- store logged data

C Activate agents.
            if (.not.bInStartup()) CALL AGT_CTL(agt_problem,ier)
   30     continue

          if(itsct.eq.2.and.iagain.eq.1)goto 27
   28   continue

C Plot zone temperature values if monitor is active.
        if(montrb.eq.1.and.ibplot.eq.1)
     &             call zngrph(0,0,itstp1,2)

C Restore original values of c and qc matrix coefficients
C if they were modified. Also restore original time-step.
        if(mtsc.ne.1)then
          call coeff1(ntstep,itstp1)
          ntstep=itstp1
          mtsc=1
        endif

C Establish percentage completion of simulation, adjust expected
C finish time and output appropriate message (but only if previous
C report was issued more than 5 seconds ago).
        inhs=inhs+1
        simcom=float(inhs)/float(nhs)
        call getsecs(ictime)
        istamp=0

        if(simcom.gt.0.95.and.inhsi.lt.12) then
          inhsi=inhsi+1
          iperc=95
          istamp=1
        else if(simcom.gt.0.90.and.inhsi.lt.11) then
          inhsi=inhsi+1
          iperc=90
          istamp=1
        else if(simcom.gt.0.80.and.inhsi.lt.10) then
          inhsi=inhsi+1
          iperc=80
          istamp=1
        else if(simcom.gt.0.70.and.inhsi.lt.9) then
          inhsi=inhsi+1
          iperc=70
          istamp=1
        else if(simcom.gt.0.60.and.inhsi.lt.8) then
          inhsi=inhsi+1
          iperc=60
          istamp=1
        else if(simcom.gt.0.50.and.inhsi.lt.7) then
          inhsi=inhsi+1
          iperc=50
          istamp=1
        else if(simcom.gt.0.40.and.inhsi.lt.6) then
          inhsi=inhsi+1
          iperc=40
          istamp=1
        else if(simcom.gt.0.30.and.inhsi.lt.5) then
          inhsi=inhsi+1
          iperc=30
          istamp=1
        else if(simcom.gt.0.20.and.inhsi.lt.4) then
          inhsi=inhsi+1
          iperc=20
          istamp=1
        else if(simcom.gt.0.10.and.inhsi.lt.3) then
          inhsi=inhsi+1
          iperc=10
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

        if(istamp.ne.0.and.(ictime-iltime).ge.1) then
          iltime=ictime
          ietim=istime+(ictime-istime)*100/iperc

C Progress report.
          write(outs,'(2x,I4,2A)') iperc,
     &      '% complete; expected finish time ',ectime(ietim)

          if(endless)then
            continue
          else
            call edisp(iuout,outs)
          endif
        end if

C If a GCEP ground coupling model is active, call the simulation
C routine only once per hour.
        if(igcep.eq.1) then
          call GCEP_SIM
        endif

C Hour loop now complete.
   20 continue

C If a ground source heat pump model is active, call the simulation routine
C only once per day.
       if(igshp.eq.1) then
         call GSHPSIM
       endif

       endif seasonal_run_test

C Agent problem check. If a problem is detected stop the simulation here.
        if (agt_problem.eq.1) call AGT_STOP(iday)

C Day loop now complete.
   10 continue

C Functional mock-up interface.
      if (is_FMU) then
        CALL FMI_DONE(IER)
      endif

C Close the file opened for dumping the structured mesh temperatures.
      IF(InSvST.EQ.1)close(ISTemp)

C Set start address of next solution-set in resuts file.
      if(indcfg.ne.2)call mzls4
      if(indcfg.ne.1  .and. (isave .ne. 0 .and. isave .ne. 5 ) )
     &     call mzpls4

C Disable TSC and set ntscpd to zero. This is necessary since
C on return to the `SIMUL' menu, the TSC by default should be inactive.
      itsflg=0
      ntscpd=0

C Switch off building progress monitor.
      if(montrb.eq.1) ibplot=0

c Switch off plant progress monitor.
      if(monitr.eq.1) imonitor=0

C Delete binary file containing fuel cell electrical loads.
      if(IFCloads.eq.0)then
        continue
      else
        CALL EFDELET(IFCloads,ISTAT)
      endif
      if(IWinsupp.eq.0)then
        continue
      else
        CALL EFDELET(IWinsupp,ISTAT)
      endif

C Delete binary files used for GCEP numerical calculations.
      DO CounterUNIT=50, 63
        NumUNIT=IFIL+CounterUNIT
        CALL EFDELET(NumUNIT,ISTAT)
      END DO

C SUN F90 has spotty support for cpu_time intrinsic.
#ifdef F90
#else
C Report simulation runtime.
      CALL cpu_time(sec_b)
      write(outs,'(A,F9.1,A)')
     &     'CPU time:',sec_b-sec_a,'s.'
      CALL EDISP(iuout,outs)
#endif

C H3Kreports.(SUMMARY).start
C Process and export the data collected by the reporting object.
C Set the .csv output root file name to the root file name of the 
C building results file.
      if ( isH3Kenabled() ) then
        CALL GenerateOutput(cOutputRoot(1:iRootLen))
      endif

C SUN F90 has spotty support for cpu_time intrinsic.
#ifdef F90
#else
C Use function isReportingInstalled() to report if h3k reports is available.
C If so, write out xml processing runtime - useful for investigating
C the impact of modifications on simulation times.
C      if ( isReportingInstalled() ) then
C         CALL cpu_time(sec_c)
C         write(outs,'(A,F7.2,A)')
C     &        'XML postprocessor cpu runtime ',sec_c-sec_b,' s.'
C         if(endless.or.issilient)then
C           continue
C         else
C           CALL EDISP(iuout,' ')           
C           CALL EDISP(iuout,outs)
C         endif
C      endif
#endif

C Delete temporary file created for boundary conditions if one exists.
      if ( bBC_data_defined ) call cleanup_BC_data()

C Delete temporary file created for lookup table facility if applicable.
      if ( bLookup_data_defined ) call cleanup_Lookup_data()

C Trace output.
   16 if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   itrace(40).ne.0) call edisp(itu,'Leaving subroutine mznuma.')
      return
      end

