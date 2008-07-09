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

C Include files req'd for deleting temporary files after the simulation is complete.
#include "H2_wind.h"
C HOT3000: Fuel cell.
#include "SOFC.h"
#include "building.h"
#include "esprdbfile.h"
#include "plant.h"
#include "tdf2.h"
#include "monitor.h"
#include "cfd.h"
#include "uncertainty.h"
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "CETC_definitions.h"
#include "MultiYear_simulations.h"
#include "bc_data.h"
#include "lookup_data.h"

C CWEC Degree Day and ground temperature parameters
#include "ground_temp_mains_common.h"

      common/outin/iuout,iuin
      common/filep/ifil
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      common/tc/itc,icnt
      common/trc/itrc
      COMMON/SAVE/ISAVE
      common/c1/ncomp,ncon
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/c6/indcfg

      COMMON/PREC7/ITCNST

      common/pstctr/nsincp,nsncpr
      common/pctstp/ntstpp
      common/pctinc/ipts
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      common/shad1/iml

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
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
      COMMON/TS6/idysv,ihrsv,irwcou,tsclaw,rewnd,nsncv1,nsncv2,pasone

C itsplt if set then transfer of mass flow
C results is required.
      common/outflg/itsplt

C Monitor commons.
      common /bmontr/mnzn,mzone(mcom),montrb,yminb(2),ymaxb(2),
     &               ibplot,irecpb(MDPH),mbstv
      common /montrv/mnpc,mcomp(mpcom),mcnod(mpcom),mstate,monitr,
     &               ymin,ymax

C TDF commons. zrotp is the previous rotation angle for each zone.
      COMMON/TDFFLG/ITEMSTD
      COMMON/TDFFLG2/IALLCLM,ICASUAL(MCOM),IZIVENT(MCOM),IRAIRVL(MCOM),
     &       ISETPTT(MCOM),ICTLSTA(MCOM),ISKYLUX,IDBTEXT,IWINDVL,
     &       IWINDDR,IRELHUM,IDIFHSL,IDIRSOL,IGLOHSL,IOBJVEL,IOBJDIR,
     &       ISKYTMP,IGLOVRT,ICASUAL3(MCOM),IACTIV(MCOM)
      common/zrotat/zrotp(mcom)

C Common for thermophysical property replacement.
      common/tpmod/itpmod(mcom)

C Air flow network
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      CHARACTER LAPROB*72

C CFD
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      COMMON/CFSEUP/CFTIMS,CFTIMF,ICFDYS,ICFDYF,ICFAUX(MCOM)

C Power simulation flag.
      COMMON/PWRFLG/IPWRSIM
      common/pnsolm/ipsolm

C Simulation start day required by cmp70c.
      common/simsdy/iss,isf

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


C Specification of timestep averaging is required
C so configuration can be passed to h3kreports
      common/avrage/idaver

C Uncertainty common.
      COMMON/UATD/UACLIM(6,3),UACTL(7,4),UAMCSA(MNACT)

C Common holding flag indicating presence of hvac data input file:
C ihvacflag=1 there is hvac file; ihvacflag=0 no hvac file.
      common/hvacinfo/ihvacflag,hvacfile

C Common holding flag indicating presence of gshp data input file:
C igshp = 1 there is a gshp file; igshp = 0 no gshp file.
      common/gshpinfo/igshp
      common/gshpfile/gshpfile
      character gshpfile*72

C GCEP (begin)
C Common holding flag indicating presence of gcep data input file:
C igcep = 1 there is a gcep file; igcep = 0 no gcep file.
      common/gcepinfo/igcep
      common/gcepfile/gcepfile
      character gcepfile*72
C NumUNIT is the unit number of the binary files to be erased
      INTEGER NumUNIT
C counter to go through the unit numbers
      INTEGER CounterUNIT

C Logical function that returns true if the
C simulation is still in the startup period
      LOGICAL bInStartup

C Active domain common.
      common/ACTDOM/CFDOK

C Common for electrical file.
      COMMON/ENTFILE/ENTFLNAM,IENTXIST
      COMMON/rcmd/LCMDFL
      CHARACTER LCMDFL*144

C Additional commons for SHOCC.
      common/c20/nzsur(mcom),nztv(mcom)

      character CTYPE*4
      real gversion
      integer igupgrade
      COMMON/G0/CTYPE(MCOM),gversion(MCOM),igupgrade

C SNAME (12 char) - surface name attribute
C ZBASEA    - area of base (m^2)
C IBASES    - surfaces (up to 12) associated with base area. If all 0 then
C             base area has been supplied by the user.
C SOTF (32 char) - surface attribute indicating OPAQUE or an optical property.
C SVFC (4 char) - string indicating whether the surface is to be
C     counted as a UP/FLOR (face up), VERT (wall), DOWN/CEIL (face down),
C     SLOP (not vertical) for purposes of floor area and daylighting.
C 
C SMLCN (32 char) surface construction attribute.
C SOTHER (24 char * 3) surface attributes for `other` side. 
C   SOTHER(i,1) = UNKNOWN indicatesthat no attribute has been set and
C               for this case SOTHER(2) and SOTHER(3) are '0'
C   SOTHER(i,1) = EXTERIOR means SOTHER(2) and SOTHER(3) are '0'
C   SOTHER(i,1) = ADIABATIC means SOTHER(2) and SOTHER(3) are '0'
C   SOTHER(i,1) = SIMILAR means that SOTHER(2) and SOTHER(3) are as IC2 & IE2
C   SOTHER(i,1) = BASESIMP means that SOTHER(2) and SOTHER(3) are as IC2 & IE2
C   SOTHER(i,1) = GROUND means that SOTHER(2) and SOTHER(3) are as IC2 & IE2
C   SOTHER(i,1) = if a zone name then its index will be moved to SOTHER(2) and
C               the surface index in SOTHER(3) and the key word ANOTHER is
C               put in SOTHER(1).
C   SOTHER(i,1) = ANOTHER then SOTHER(2) is zone index and SOTHER(3) is
C               the surface index in that zone (e.g. as IC2 and IE2).
C SUSE (8 char * 2) is the usage of the surface as follows (note this is
C    likely to be a temporary variable and will be depreciated in favour
C    of SSUSE:
C    DOOR,CLOSED  DOOR,UNDERCUT  DOOR,OPEN  DOOR,BIDIR
C    FRAME,CLOSED FRAME,CRACK    FRAME,OPEN
C    WINDOW,CLOSED WINDOW,CRACK  WINDOW,OPEN  WINDOW,SASH  WINDOW,BIDIR
C    GRILL,SOURCE  GRILL,EXTRACT
C    FICT,CLOSED  FICT,CRACK  FICT,OPEN  FICT,BIDIREC
C    BLIND,FIXED  BLIND,MOVE (??explicit blinds)
C    otherwise  -,- )
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)

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

      character outs*124,STYPE*8
      CHARACTER DBTAG*12,DBTASK*12,DBZN*15
      character*72 LSNAM,LPROJ,LGEOM,LSHAD,LUTIL,LTHRM,LCFD,ENTFLNAM
      CHARACTER*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ,L3DNDD,L3DTLW,L3DLWV
      CHARACTER*72 LGDCVS,LGDCNC,LGDNDC,LGDNDD,LGDTAQ

      CHARACTER*72 hvacfile

      CHARACTER SOTHER*24,SNAME*12,SMLCN*32,SVFC*4,SOTF*32,SUSE*8
      CHARACTER SPARENT*12

      dimension VAL(MBITS+2)
      logical ok,itpmod,WDARDO,MSTROK,MSTRZN
      logical rewnd,irew,tsclaw,pasone
      LOGICAL BLDG3D,ZONE3D,GRND3D,GRNDSP
      logical global,glbrwd,close
      logical CFDOK

C foundshoc will become true if any zones have SHOCC descriptions.
      logical context,foundshoc

C H3Kreporting object
      CHARACTER*128 H3K_rep_NAME, H3K_rep_value
      logical rep_xmlstatus ! function indicating if h3kreports is available.
      logical bH3K_rep_enabled ! function indicating if h3kreports is enabled.

C Expected finish time variables.
      character*24 ectime

C Flag for iteration between plant and electric power flow domains.
      LOGICAL plant_elec_converged

C Flag to enable inter-domain iteration
      common / Plt_Elec_Net / bInter_domain_iteration
      logical bInter_domain_iteration

C Loop counters for cross domain iterations
      common / Cross_Domain_Loop / plt_elec_loop_counter
      integer plt_elec_loop_counter

C Indicies of start, end, current & future indicies
      integer iMY_start_yr_index, iMY_end_yr_index,
     &        iMY_current_yr_index, iMY_future_yr_index

C BASESIMP.
      common/bsmtdef/iBSIMP(MCOM),LBSIMP(MCOM)
      character LBSIMP*72

C Flag for whether there is basesimp file
      logical bsm_flag

C Plant network data
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      common/c9/npcomp,nci(mpcom),cdata(mpcom,mmiscd)
C Flag for whether plant network has storage tank that uses mains make up 
C water
      logical itank_flag

      logical autook
      logical newgeo  ! to use for testing if new/old geometry file.


C SUN F90 has spotty support for cpu_time intrinsic
#ifdef F90
#else
C Store start time for reporting simulationd duration
      call cpu_time(sec_a)
#endif

C*h3koutput* Initialize data variables
      call H3KSTORE_ZERO_DATA

C Open file to store hvac simulation results. This is only a temporary
C measure and will be removed once development of the HVAC routines
C is complete.
      call HVAC_OUTPUT_FILE

C Trace output.
      if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   itrace(40).ne.0) call edisp(itu,' entering subroutine mznuma')

      newgeo=.false.  ! assume older format geometry.


C If computational time-step less than one hour
C modify coefficients computed in MZCOE1.
      IF(NTSTEP.GT.1)THEN
        TSTEP=1./FLOAT(NTSTEP)
      ELSE
        TSTEP = 1
      ENDIF 

C Scale MZCOE1 coefficients as necessary      
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

C SHOCC project tree, if any zones have SHOCC check for
C multiyear and warn the user.
      call initprj(ncomp)

      if(bSHOCCed) then
        
        if ( bMY_sim_enabled ) then

C Running SHOCC in multi-year mode might(?) cause
C bps to fail with spectacular results! Perhaps Denis can
C test shocc & add multi-year support in the future.
          call edisp(iuout,
     &   ' SHOCC does not currently support multi-year simulation!')
          STOP 'bmatsv.F: Fatal Error!'
        endif

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

C Initialize DHW
      call DHW_Initialize

C Initialize zone<->plant casual gain coupling
      call plt_zone_gain_zero()

C Write simulation control information to the solutions
C library.
      if(indcfg.ne.2)call mzls2
      if(indcfg.ne.1 .and. (isave .ne. 0 .and. isave .ne. 5 ) )
     &     call mzpls2

C Write the CFD result library header.
      if (CFDOK) call cfdlibhead

C Invoke initialisation of climate data and
C zone future air temperatures.
      call mzini1

C Initialise control parameters as required by building pid controller,
      call mzibct

C Initialise  Global Control parameters,
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

C Adjust multi-year simulation start year to account for pre-new-year
C start-up period.
        if ( bMY_sim_enabled ) then
          iMY_Start_year = iMY_Start_year - 1
        endif 

      ENDIF

C Determine indicies of start and end years, with respect to
C start and end of defined multi-year climate database.
      if ( bMY_sim_enabled ) then

C Start year: Note: result can be less than one      
        iMY_start_yr_index =
     &        iMY_start_year - iMY_clm_file_years ( 1 ) + 1

C End year: (this data is not used at present)
        iMY_end_yr_index = iMY_end_year - iMY_start_year

      endif

C H3Kreporting object - save the frequency of
C building timesteps. Note: simulations with
C different timesteps for multiple domains are
C not presently supported.
C.....H3Kreports.(timesteps).start.................
C.....Minutes per timetep (building domain)
C.....-> set name
      if ( bH3K_rep_enabled() )  then
        WRITE(H3K_rep_NAME,'(A)')'minutes_per_step'
        WRITE(H3K_rep_VALUE,'(f7.3)') 60./real(ntstep)
C.....-> update meta-data
        call REP_SET_PARAMETER(
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &     H3K_rep_value(1:LNBLNK(H3K_rep_VALUE)))
C.....Timestep averaging parameter
C.....-> set name
        WRITE(H3K_rep_NAME,'(A)')'default_time_step_averaging'

C Set iadver = 1 to manually disable timestep averaging
C (or include '<time_step_averaging>false</time_step_averaging>'
C  in input.xml )
        WRITE(H3K_rep_VALUE,'(I3)')  idaver

C.....-> update meta-data
        call REP_SET_PARAMETER(
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &     H3K_rep_value(1:LNBLNK(H3K_rep_VALUE)))

C.....Number of timesteps
C.....-> set name
        WRITE(H3K_rep_NAME,'(A)')'number_of_timesteps'
        WRITE(H3K_rep_VALUE,'(I9)') ((isf-iss+1))*NTSTEP*24

C.....-> update meta-data
        call REP_SET_PARAMETER(
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &     H3K_rep_value(1:LNBLNK(H3K_rep_VALUE)))


C.....H3Kreports.(timesteps).end.................

      endif ! <- matches 'if ( bH3K_rep_enabled() )...'


C Check to see if flexible boundary condition file has been defined,
C and process file if necessary.
      if ( bBC_data_defined ) then
        call process_BC_data_file(ifil,autook,bBC_data_err)
        if ( bBC_data_err ) bBC_data_defined = .false.
      endif

C Check to see if lookup table data file has been defined and
C process file if necessary

      if ( bLookup_data_defined ) then

        call process_Lookup_data_file(ifil,autook,
     &                            bLookup_data_err)
        if ( bLookup_data_err ) bLookup_data_defined = .false.
      endif

C Zero Site energy usage storage array
      call ZeroFuelEnergyStorageArray()

C Initialize file unit number. This number should correspond
C to the climate file number. However, the climate file name
C and number saved in common C22 is used to handle the
C climate file.
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

C Imonitor if set (i.e = 1) then plant state variables
C may be plotted.
      imonitor=0

C Save user specified time-step value for
C later use if time-step controller active.
      itstp1=ntstep
      itstpp=ntstpp

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
      if((montrb.eq.1.or.monitr.eq.1).and.itcnst.gt.0)
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

C Flag for whether there is a basesimp file set to false
      bsm_flag = .false.
C Check and see whether there is actually bsm file and set flag to true      
      do 11 ibsm_counter = 1,mcom
        if(iBSIMP(ibsm_counter).eq.1) then
          bsm_flag = .true.
        endif
 11   continue

C Check whether plant network has a storage tank that uses mains make up
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

C If this is a single year simulation, open the climate file. During
C multi-year simulations, the climate files will be opened inside
C the simulation loop
      if ( .not. bMY_sim_enabled ) 
     &   call EFOPRAN(ICLIM,LCLIM,144,0,IClmErr)

C In the case that we have a gshp input file, dhw input file, and/or basesimp 
C input file, then call routines to determine degree days and ground temperature
C parameters
      if((igshp.eq.1).or.(iDHW_Flag.eq.1).or.
     &    bsm_flag.or.itank_flag) then

C Degree Days and Monthly Average Ambient Temperature
c Call to subroutine that calculates the number of annual heating degree days
c and the average monthly temperatures based on the climate file chosen. 
c These values are required to calculate the mains water temperature.
c This calculation is performed once per simulation or once per climate file - in the 
c case of multi-year simulations.
         call CLM_DEG_DAY

C Initialize ground temperature profile parameters
         call Ground_Temp_Profile
      endif

C Start simulation day-loop.
C (i =curr day, iss = sim start day, isf = finish day)
      do 10 i=iss,isf 

C For multi-year simulaitons, determine the current
C and future time row years, and the incidies of these
C years with respect to the multi-year climate databases
        if ( bMY_sim_enabled ) then
C Years
          iMY_current_year = iMY_Start_year + int(float(i-1)/365.0)
          iMY_future_year = iMY_Start_year + int(float(i)/365.0)

C Assign global year (iYear), present year (iYear_p) & future year
C (iYear_f)
          iYear   = iMY_current_year
          iYear_p = iMY_current_year
          iYear_f = iMY_future_year

C Year indicies
          iMY_current_yr_index = iMY_start_yr_index
     &                + ( iMY_current_year - iMY_Start_year )
          iMY_future_yr_index = iMY_start_yr_index
     &                + ( iMY_future_year - iMY_Start_year )

C Are indicies within the defined climate database? If not, adjust
C indicies to the first/last records in the database.
          if ( iMY_current_yr_index .lt. 1 ) iMY_current_yr_index = 1
          if ( iMY_current_yr_index .gt. iMY_clm_file_count )
     &           iMY_current_yr_index = iMY_clm_file_count
          if ( iMY_future_yr_index  .lt. 1 ) iMY_future_yr_index = 1
          if ( iMY_future_yr_index  .gt. iMY_clm_file_count )
     &           iMY_future_yr_index  = iMY_clm_file_count

        else
          iYear_p = iYear
          iYear_f = iYear
        endif

C For type 6 time-step controller, loop until saved day,
        if(irew.and.i.lt.idysv)goto 10

C `ntsctr' is used to count the number of TSC operations
C per day.  This is used only if TSC is active.
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

C What day of week is present and future day: Monday 1
C through Sunday 7.
        call edayr(idyp,id,im)
        call eweekd(id,im,iyear_p,idwp)
        call edayr(idyf,id,im)
        call eweekd(id,im,iyear_f,idwf)

C Establish climatic data for present day and first
C hour of future day: future day first.

C The climate database contains records int he following
C order. Note: the database stores all records in
C integer format.
C col 1: Diffuse solar on the horizontal (W/m**2)
C col 2: External dry bulb temperature   (Tenths DEG.C)
C col 3: Direct normal solar intensity   (W/m**2)
C col 4: Prevailing wind speed           (Tenths m/s)
C col 5: Wind direction     (clockwise deg from north)
C col 6: Relative humidity               (Percent)

C If this is a multi-year simulation, we may need to
C open the approprate climate file.
        if ( bMY_sim_enabled )then
          if (.not. bMY_clm_file_open ( iMY_current_yr_index ) ) then

C Close old file, if necessary.
            if ( iMY_current_yr_index .gt. 1 ) then
              if ( bMY_clm_file_open ( iMY_current_yr_index - 1) ) 
     &         call ERpFree( iClim , istat )
            endif 

C Open new file.   
            call EfOpRan(iClim,cMY_clm_file_names(iMY_current_yr_index),
     &                144,0,IER)

C set flag      
            bMY_clm_file_open ( iMY_current_yr_index ) = .true.

C In the case that we have a gshp input file, dhw input file, and/or basesimp 
C input file, then call routines to determine degree days and ground temperature
C parameters
            if((igshp.eq.1).or.(iDHW_Flag.eq.1).or.
     &        bsm_flag.or.itank_flag) then
c If a new climate file is opened, then need to recalculate the number 
c of annual heating degree days and the average monthly temperatures based on 
c this new climate information.
              call CLM_DEG_DAY

C Initialize ground temperature profile parameters
              call Ground_Temp_Profile
            endif

          endif 

        endif

C Present day.
        irec=idyp
        read(iClim,rec=irec,iostat=istat,err=1000)
     &    (idif(j),itmp(j),idnr(j),ivel(j),idir(j),ihum(j),j=1,24)

C Note: future year may correspond to another climate file
C If this is a multi-year simulation, we may need to  open the
C approprate climate file.

        if ( bMY_sim_enabled ) then
           if ( .not. bMY_clm_file_open ( iMY_future_yr_index ) ) then
C Close old file, if necessary.
            if ( iMY_future_yr_index .gt. 1 ) then
              if ( bMY_clm_file_open ( iMY_future_yr_index - 1) )
     &        call ERpFree( iClim , istat )
            endif

C Open new file.
           call EfOpRan(iClim,cMY_clm_file_names(iMY_future_yr_index),
     &                144,0,IER)

C Set flag 
            bMY_clm_file_open ( iMY_future_yr_index ) = .true.
          endif
        endif 
C Future day
        irec=idyf

C Get values for first hour of the future day.
       read(iClim,rec=irec,iostat=istat,err=1000)
     &   idiff,itmpf,idnrf,ivelf,idirf,ihumf

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

C flag signaling new SHOCC day.
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
     &        ntsctr=ntsctr+1

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

C TSC7 Time-step iteration between plant and electrical domains. This will be
C generalized later to be consistent with the treatment of other time-step
C iteration approaches.  Specifically, it will be supported as a simulation toggle
C and it will allow both the time-step iteration between plant and electrical,
C as well as the iteration between these domains and the building thermal domain.
C For now, force TSC7 if both the DG controller and the electric solution
C domains are active.
          IF( bInter_domain_iteration .and. IENTXIST==1 ) THEN
            itsct = 7
          END IF

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

C H3Kreports: send simulation time to reporting object

            if ( bH3K_rep_enabled() ) then
              call H3K_Report_time(
     &          btimep, btimef, ihrp, ihrf, idyp, idyf, its, ntstep, 
     &          nsinc )
            endif

C Output current simulation increment if trace on.
            if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &         itrace(33).ne.0)then
              write(outs,'(A,I3)')' Simulation increment ',nsinc
              call edisp(itu,outs)
            endif

C ** DEBUG ** Write nsinc to screen
C If uncommented, this line will write the current timestep #,
C hour and day to the screen.
C           write(itu,*) 'TS:',nsinc,' hr:',ihrp,' dy:',idyp,
C     &                ' yr:', iMY_current_year
C           call edisp(itu,outs)


C Determine climatic parameters at the present and future time row. 
            call mzclmpt(1)

            if(indcfg.eq.2)goto 401

C Calculate the heat injections associated with plant
C component heat losses to associated zone/surface/
C containment
            call plt_zone_gain_calc()

C Determine for each component and surface, the defining
C index and the adjacent space temperature and radiation
C at the present and future time-rows.  Also set up plant
C input flux to zones due to plant component containment
C losses already established by the coefficient generators.
            call mzadjc

C Compute total external and internal window and door areas
C and transfer to results library; then initiate start-up
C nodal temperature assignments (at first time-step only).
C Start of iterative loop for stationary time.
   81       if(itsct.eq.4.and.itsok.eq.1)then
              call tscon4(itrflg,ntsctr)
            endif

            if(i.eq.iss.and.j.eq.1.and.k.eq.1.AND.WDARDO)then
              do 7 l=1,ncomp
                icomp=l
                call mzwdar(icomp)
                call mznasg(icomp)
    7         continue
            endif

C Set `itsmus' if output is required at this time-row.
            itsmus=0
            rem=amod(float(k),float(mtsc))
            call eclose(rem,0.00,0.0001,close)
            if (close.and.itrflg.eq.0) itsmus=1

C Activate CFD model during an intended period.
C Do this by resetting IFCFD variable this is then checked in MZCONV.
            DO 15 ICP=1,NCOMP
              IF(ICFAUX(ICP).NE.0)THEN
                if(icfaux(icp).gt.0) then
                  if(idyp.lt.isds) then
                    IFCFD(ICP)=ICFAUX(ICP)
                  else
                    IFCFD(ICP)=0
                  endif
                endif
                IF(IDYP.GE.ICFDYS.AND.IDYP.LE.ICFDYF)THEN
                  IF(btimef.GT.CFTIMS.AND.btimef.LE.CFTIMF)THEN
                    IFCFD(ICP)=ICFAUX(ICP)
                  ELSEIF(btimef.GT.CFTIMF)THEN
                    IFCFD(ICP)=0
                    ICFAUX(ICP)=0
                  ENDIF
                ENDIF
              ENDIF
  15        CONTINUE

C And compute all inside and 'outside' convection coefficients.
            call mzconv

C Update ground reflectivity depending on the model used (constant
C reflectivity; simple model; or advanced model)
            CALL MZGREF

C Consider each zone in turn.
            do 40 l=1,ncomp
              icomp=l

C Update SHOCC occupancy status ...
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
                  call eclose(gversion(icomp),1.1,0.01,newgeo)
                  if(newgeo)then
                    call georead(igu,LGEOM(icomp),icomp,1,itrc,iuout,
     &                IER)
                  else
                    call egomin(igu,LGEOM(icomp),icomp,1,itrc,iuout,
     &                IER)
                  endif
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
C if moisture transfer modelling is invoked.
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

              IF (ISAVE.eq.5)then
                call surbal(icomp)
                call H3KSTORE_SAVELEVEL5(icomp,iday,itsmus,ndbph)
              ELSE
                call mzls3(icomp,iday,itsmus,ndbph)
              ENDIF

C transport miscellaneous simulation data to reporting object
              if ( bH3K_rep_enabled() ) then
                 call H3K_transport_build_data(icomp)
                 if (IAIRN .ne. 0) THEn
                    call H3K_transport_mfn_data()
                 endif
              endif
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

C Zone loop complete. Call HVAC simulation controller if HVAC models are active.
            if(ihvacflag.eq.1) then
              call START_HVAC_SIM(i)
            endif

C Call DHW simulation controller if DHW models are active.
            if(iDHW_Flag.eq.1) then
              call DHW_Main
            endif

C Transport climate data to reporting object
            if ( bH3K_rep_enabled() )
     &          call H3K_transport_climi_data()

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

C For type 6 time-step controller, possibly 'rewind' simulation,
            if(itsct.eq.6.and.itsok.eq.1)then
              if(i.ge.itssch(ntsctr,4).and.i.le.itssch(ntsctr,5))then
                call tscon6(i,j,ntsctr,irew)
                if(irew)goto 123
              endif
            endif

C Reset h3k zone control action flags

            call h3kReports_reset_zone_flags()

C Building side now complete.
            if(indcfg.eq.1)goto 77

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C Beginning of plant and electrical domains.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C First time step only: assign starting values of plant
C nodal variables, assign plant time-step counter
C and plant control loop PID control common digpid.
  401       if(i.eq.iss.and.j.eq.1.and.k.eq.1)then
              CALL MZNASP
C Initialise plant time-step counters.
              nsincp=0
              nsncpr=0

C Initialise control parameters as required by plant pid controllers.
              call mzipct
            endif

C Establish number of seconds per plant time step.
            timsec=3600.0/(float(ntstep)*float(ntstpp))

C Plant multiple time-steps within each building-side time-step.
C Solve multiple time-steps in plant and electrical domains to catch up to building domain.
            do 100 L=1,ntstpp

C Adjust timer.
              ipts=L
              ptimep=ptimef
              if(nsinc.eq.1.and.L.eq.1)ptimep=1.0
              its0=its-1
              ptimef=float(ihrp)+float(its0)/float(ntstep)
     &            +float(ipts)/float(ntstpp*ntstep)
              if(ptimef.gt.24.0)ptimef=ptimef-24.0

C Increment plant time-step counter.
              nsincp=nsincp+1

C Assign climate variable values.
              call mzclmpt(2)

C Re-zero zone<->plant casual gain coupling arrays
              call plt_zone_gain_zero()

C Reset flag to indicate whether iteration is required between the plant and
C electrical domains in order to converge the solution. `plant_elec_converged'
C is true if no further iteration is required in order to converge the solution.
C Iteration between the plant and electrical domains will continue as long as
C `plant_elec_converged' remains false.
              plant_elec_converged = .FALSE.

C Call subroutine XXX_TimeRow_Manipulate to store present time-row
C data, permitting the simulation to be re-wound to to the state at
C which plant iteration is ajourned, if inter-domain iteration is active.

C Use flag 'iSaving' to tell XXX_TimeRow_Manipulate to store data
              if ( itsct.eq.7 ) then
                call Plt_TimeRow_Manipulate(iSaving)
                call Pfs_TimeRow_Manipulate(iSaving)
              endif

C Initalize the loop conter at the beginning of each plant time step
              plt_elec_loop_counter = 1

C Note: Calls to Plt_TimeRow_Manipulate() and Pfs_TimeRow_Manipulate()
C to invoke the 'iWinding' operation are embedded inside the solution of
C the plant and electrical domains. Thus, if either domain is called
C outside the context of bps ( and specifically, this iteration loop )
C the state-variables will be handled normally.

              DO WHILE( .not. ( plant_elec_converged ) )

C Set up and solve the plant matrix.
C Subroutine MZPMRX (esruplt/pmatrx.F) controls iteration to converge the plant domain.
C It also calls the controlling routine that executes each coefficient generator
C within each iteration through the plant domain.
                call mzpmrx(itera)

C If necessary, iterate. *** NOT YET ACTIVE.
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
C then check whether a converged solution has been achieved.  If further iteration
C is required, then `plant_elec_converged' will be set to FALSE. If the time-step
C iteration between the plant and electrical domains is not active (e.g. if the
C electrical domain is not in use), then set `plant_elec_converged' to TRUE to
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
              ENDDO

C Store plant results.
              iday=idyp

C Imonitor should be set to 1 then plant state variables will be
C displayed....
              itsplt=0

C  don't write plant libraries if save-levels 0 or 5 are active
              if ( ISAVE .ne. 5 .and. ISAVE .ne. 0) then
                if(amod(float(L),float(mtsc)).eq.0.0.and.
     &            (itrflg.eq.0.or.itrflg.eq.2)) then
                  call mzpls3(iday,imonitor)
                  itsplt=1
                  if(monitr.eq.1.and.imonitor.eq.1)
     &              call mgraph(0,0,itstpp*itstp1,2)
                endif
              endif

C write out plant results
              if ( bH3K_rep_enabled() ) call H3KSTORE_Plant_TimeSteps()

C Post-process and save electrical network results:
              IF(IENTXIST.GT.0) then
C Database is not written if save-levels 0 or 5 are active.
                if ( ISAVE .ne. 0 .and. isave .ne.5 ) then
                  CALL ENETLIBSV
                endif

C Call h3k reports electrical data transport routines 
                if ( bH3K_rep_enabled() )
     &                call H3K_transport_electric_data()
              endif

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C End of solution of plant and electrical domains for the current plant time-step.
C Return to `do 100' to solve the plant and electrical domains for the next
C plant time-step.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  100       continue

C For type 4 timestep controller, iterate until itrflg=0.
   77       if (itrflg.gt.0) goto 81
            ITRPAS=0

C Plant side now complete.

C Call the electrical network simulation controlling routine here for a
C building-side only electrical network solution (no plant).
            IF(IENTXIST.GT.0.AND.INDCFG.EQ.1) THEN
              CALL ENETSOLV

C Database is not written if save-levels 0 or 5 are active. 
              if ( ISAVE .ne. 0 .and. isave .ne.5 ) then
                CALL ENETLIBSV
              endif
              if ( bH3K_rep_enabled() ) THEN
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

C Save boundary condition data, if it exists
            if ( bH3K_rep_enabled() ) call report_BC_data()

C Call SiteUtilities procedure to characterize purchased energy
C usage
            call SiteUtilities()

C.....H3Kreports.(UPDATE).start.................
C.....Timestep is now complete; call reporting
C.....object update function to process and
C.....store logged data
C.....
C.....Any changes to this code should be made within
C.....the H3Kreports demarcaration comments

C.....Check to see present time is close to midnight
            if (ihrp.eq.24) then
              myday = iday + 1
              if (myday .gt. 365) myday = 1
            else
              myday = iday
            endif

C.....Update reports.
            if ( bInStartup() ) then
              iStartupFlag = 1
            else
              iStartupFlag = 0
            endif

C << NOTE: dummy call REP_UPDATE only has 3 parameters >>
            call REP_UPDATE(nsinc,btimep,myday, iStartupFlag) ! <- store logged data

   30     continue

C Hour step now complete.
          if(itsct.eq.2.and.iagain.eq.1)goto 27
   28   continue

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
C         write(outs,'(I4,A)') iperc,' %   complete; '
          call edisp(iuout,outs)
        end if

C The GCEP simulation routine is called only once per hour, if a
C GCEP system is defined.
        if(igcep.eq.1) then
          call GCEP_SIM
        endif

C Hour loop now complete.
   20 continue

C The GSHP simulation routine is called only once per day, if a
C GSHP system is defined.
       if(igshp.eq.1) then
         call GSHPSIM
       endif

C Day loop now complete.
   10 continue

C Close the file opened for dumping the structured mesh temperatures.
      IF(InSvST.EQ.1)close(ISTemp)

C Set start address of next solution-set in library.
      if(indcfg.ne.2)call mzls4
      if(indcfg.ne.1  .and. (isave .ne. 0 .and. isave .ne. 5 ) )
     &     call mzpls4

C Disable TSC and set `ntscpd' to zero. This is necessary since
C on return to the `SIMUL' menu, the TSC by default should be
C inactive.
      itsflg=0
      ntscpd=0

C Switch off building plot.
      if(montrb.eq.1) ibplot=0

c Switch off plant plot..
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

C GCEP (begin).
C Delete binary files used for numerical calculations
      DO CounterUNIT=50, 63
        NumUNIT=IFIL+CounterUNIT
        CALL EFDELET(NumUNIT,ISTAT)
      END DO


C SUN F90 has spotty support for cpu_time intrinsic
#ifdef F90
#else
C write out simulation runtime - useful for investating the impact
C of modifications on simulation times.
      CALL cpu_time(sec_b)
      write(outs,'(A,F7.2,A)')
     &     'Simulation cpu runtime: ',sec_b-sec_a,' seconds'
      CALL EDISP(IOUTS,' ')
      CALL EDISP(IUOUT,outs)
#endif

C H3Kreports.(SUMMARY).start......................
C The following code is used to process and export
C the data collected by the reporting object.
C
C Any changes to this code should be made within
C the H3Kreports demarcaration comments

      CALL REP_XML_SUMMARY
      CALL REP_CLEANUP_FILES
      
C........H3Kreports.(SUMMARY).end........................



C SUN F90 has spotty support for cpu_time intrinsic
#ifdef F90
#else
C Use function rep_xmlstatus() to report if h3k reports is available.
C If so, write out xml processing runtime - useful for investating
C the impact of modifications on simulation times.
      if ( rep_xmlstatus() ) then
         CALL cpu_time(sec_c)
         write(outs,'(A,F7.2,A)')
     &        'XML postprocessor cpu runtime: ',sec_c-sec_b,' seconds'
         CALL EDISP(IUOUT,outs)
         CALL EDISP(IOUTS,' ')
      endif
#endif 


C Delete temporary file created for boundary conditions, if one exists.        
      if ( bBC_data_defined ) call cleanup_BC_data()

C Delete temporary file created for lookup table facility if applicable
      if ( bLookup_data_defined ) call cleanup_Lookup_data()


C Trace output.
   16 if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   itrace(40).ne.0) call edisp(itu,' Leaving subroutine mznuma.')
      return

 1000 write(outs,'(A,I3)') ' mznuma: climate file error @ rec ',irec
      call edisp(iuout,outs)
      call epwait
      goto 16

      end

