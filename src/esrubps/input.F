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

C BPS version: This file contains the following routines:
C      mzinpt
C      mzprec
C      mzutil
C      usesps
 
C ******************** mzinpt
C Mzinpt controls input of a data set which defines a building
C and/or plant configuration for simulation.
C IREP sets the level of reporting/ checking - designed for use in 
C uncertainty analyses where data has to be re-read silently. 
C IREP=1 implies silent and no checking.

C Common block variables are:
 
C indcfg  - is the configuration file index where: 1= building only,
C           2= plant only, 3= building and plant

C xlat,   - Site latitude and the longitudinal difference between the 
C xlon      site and the reference time zone (east +ve).
 
C siteexposureindex - Site exposure index (from site.h):
C           1= city centre, normal case; 2= urban site, normal case
C           3= rural site, normal case;
C           4= city centre, equal sky, ground & building view factors
C           5= city centre, building below mean height of surrounding
C              buildings
C           6= rural site, isolated; 7= totally enclosed building
C           8= user defined
 
C gref    - external ground reflectivity
 
C modeltitle - the name of the entire system
 
C ncomp   - number of zones comprising the system to be simulated
C nccode  - zone code (for eventual database reference)
 
C lproj   - zone scheduling (operation file)
C lgeom   - zone geometry file      
C lthrm   - zone construction file
 
C ncon    - total number of intra-zone connections
 
C ic1 &   - zone and multilayered construction numbers defining the start
C ie1       point of each connection

C ict     - connection type where:
C           0 ; connection to external conditions
C           1 ; connection to identical environmental conditions
C               as the source zone (OR, relative temp and absolute 
C               radiation can be specified, see below)
C           2 ; connection to constant and known environmental conditions
C           3 ; connection to another zone (as defined by IC2 & IE2 
C           4 ; connected to ground
C           5 ; surface is adiabatic
C           7 ; surface according to the CEN 13791 partition

C ic2 &   - connection description where ic2 & ie2:
C ie2       - are unused if corresponding ict=0
C           - contain relative temp and absolute incident radiation 
C             intensity values respectively if corresponding ict=1
C             (If ic2=ie2=0, then identical to source zone)
C           - contain constant temperature and incident radiation 
C             intensity values respectively if corresponding ict=2
C           - contain the zone and multilayered construction numbers 
C             at which each connectionterminates (if corresponding ict=3)
C           - contain pointers to ground temps (if corresponding ict=4)

C iairn   - building fluid (1=text. 2=graphic) flow simulation index (0 = off)
C icaas   - fluid (= air) flow network node associated with each
C           building zone. NB fluid flow network and building zones
C           need not be matched
C iflwn   - plant fluid flow simulation index (1 = on)
C icffs   - fluid flow network node associated with each plant component. 
C           NB fluid flow network and plant energy network need not be matched

C laprob  - fluid flow description file

C lpnam   - the name of the plant system
C npcomp  - number of plant components

C npcdat(*,1) - component code number. Presently this code is only
C               used for reference purpose

C npmtyp  - type of matrix required for configuration
C             1 = energy balance only
C             2 = mass balance, single phase
C             3 = mass balance, two phase

C npref   - plant components database pointer (for user selection)
C npsta   - start address for each component in the database

C nci     - number of possible control variables
C           Note that if any of these variables are actually
C           to be controlled, then a corresponding control
C           loop must be activated

C cdata   - design (or upper) values for each control variable

C npcon   - number of inter-component connections

C ipc1 &  - component and node numbers defining receiving component
C ipn1      (ie defining inlet node)

C ipct    - plant component inter-connection type where:
C            1 = connected to identical environmental conditions
C                as the source component;
C            2 = connected to constant and known environmental
C                conditions (defined by ipc2 and ipn2);
C            3 = connected to another component (as defined
C                by ipc2 and ipn2);
C            4 or 5= connected to a building zone (as defined
C                by ipc2) or ambient conditions (ipc2=0)

C ipc2 &  - component and node numbers defining the connected comp.
C ipn2

C ncont   - total number of plant component containment conditions
C           NB it is permissible to contain only some subset of the
C           total number of components. Unreferenced components
C           would then have no exchange with their environment

C ipcc    - defines a plant component for containment

C indcp   - index which dictates the type of containment (as ict
C           above but read 'component' for 'zone')

C cntdat  - environment (or surroundings) data for a plant component
C            if indcp=0 ; outside air used and cntdat(1) used to
C                         indicated the variation from this value
C            if indcp=1 ; temperature of a specified plant node.
C                         cntdat(1) identifies the component, and
C                         cntdat(2) the node therein, and cntdat(3)
C                         some constant temperature shift. cntdat(1)
C                         and cntdat(2) both zero is the self con-
C                         tainment case
C            if indcp=2 ; constant temperature the value of which
C                         is in cntdat(1)
C            if indcp=3 ; temperature of a building zone, where
C                         cntdat(1) identifies the zone number,
C                         cntdat(2) the surface number ; and
C                         cntdat(3) the node number from 'outside'
C                         If cntdat(2) and cntdat(3) both zero then
C                         zone air temp. used and if cntdat(3) zero
C                         use the inside surface node temperature

      subroutine mzinpt(IREP,IER)

#include "building.h"
#include "site.h"

C espriou.h provides current file.
#include "espriou.h"

#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LPCDB/IPCDB (for plant template database)
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
C ICLIM,LCLIM (for climate file)
C default file names for databases

#include "plant.h"
#include "tdf2.h"
#include "dhw_common.h"
#include "MultiYear_simulations.h"
      PARAMETER (MSTMC=20)
      
      integer lnblnk  ! function definition
      integer igraphiclib  ! external definition

      common/er1/ier1
      common/trc/itrc
      common/filep/ifil
      common/pophelp/h(60)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG

      common/outin/iuout,iuin

C Path to model and command line file name.
      common/rpath/path
      common/rcmd/LCMDFL
      common/C21/IFCFG,cfgroot,LCFGF
      common/c6/indcfg
      common/CFGV/icfgv

      common/afn/iairn,laprob,icaas(mcom)
      common/ffn/iflwn,icffs(mpcon)

      common/prec7/itcnst
      COMMON/DNORGH/IRTYPE
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

C Temporal definition commons.
      COMMON/TDFI/IUTDF,ITDFLG,IUTDFA
      COMMON/TDFFT/LTDF,LTDFA

C Common for electrical file 
      COMMON/ENTFILE/ENTFLNAM,IENTXIST

C Defaults.
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF
      COMMON/GR1D04/GAM,RGAM
      COMMON/GR3D100/BLDG3D,ZONE3D(MCOM)
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3
      COMMON/GRND100/GRND3D
      COMMON/GRND101/NNODTG,ITCNSTG,TEMP3G(MCOM),AREAGD(MCOM)
      COMMON/GRND108/LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD
      COMMON/MOIST01/MSTROK,MSTRZN(MCOM)
      COMMON/GRSD100/IndxSt
      COMMON/GRSD101/LGrdSt

C Uncertainty common.
      COMMON/UA5/IMET,ISIM,NRSIM

C Simulator parameter.
      COMMON/SPFL/spfileok,perok,tstepok,saveok,autook,exitok,startupok
      common/SPFLN/spsname
      COMMON/Vld30/Tinitial
      common/dllerr/dllsubr,dllmesg

C HVAC
C Flag indicating presence of HVAC models:
C ihvacflag=1 indicates HVAC models are active; 0 indicates no HVAC models.
      common/hvacinfo/ihvacflag,hvacfile

C AIM-2.
      common/aim2def/iAIM2,LAIM2

C Mechanical Ventilation systems.
      Integer iMech
      Character LMech*72
      common/mechvdef/iMech,LMech

C For reading in of bi-directional data.
      COMMON/BIDIRFL/bidirfile,bidirname(MSTMC)

C Material data arrays.
      real matver
      integer matcats,matcatitems,matdbitems
      character matdbdate*24,matdbdoc*248,matcatname*32,matcatdoc*248
      common/matheader/matver,matcats,matdbitems,matcatitems(MGCL),
     &  matdbdate,matdbdoc,matcatname(MGCL),matcatdoc(MGCL)

      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      CHARACTER*72 LGDCVS,LGDCNC,LGDNDC,LGDNDD,LGDTAQ,LGrdSt

      CHARACTER OUTSTR*124,LCFGF*72,LTMP*72,MODE*4,cfgroot*24
      CHARACTER outs*124,louts*248,h*72,spsname*8,L144*144,fs*1
      CHARACTER LTDF*72,LTDFA*72,LCMDFL*144
      character*72 laprob,path,ENTFLNAM
      character dllsubr*12,dllmesg*124,bidirfile*72,bidirname*12
      DIMENSION ICLN(30)  ! for checking material db categories

C HVAC AIM-2
      character hvacfile*72,LAIM2*72

      LOGICAL spfileok,perok,tstepok,saveok,autook,exitok,startupok
      LOGICAL OK,DOK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,XST
      LOGICAL BLDG3D,ZONE3D,GRND3D,MSTROK,MSTRZN
      logical dll,close,unixok

C GSHP
C Common holding flag indicating presence of gshp data input file:
C igshp = 1 there is a gshp file; igshp = 0 no gshp file.
      common/gshpinfo/igshp
      common/gshpfile/gshpfile
      character gshpfile*72

C GCEP
C Common holding flag indicating presence of gcep data input file:
C igcep = 1 there is a gcep file; igcep = 0 no gcep file.
      common/gcepinfo/igcep
      common/gcepfile/gcepfile
      character gcepfile*72

C Flag to enable inter-domain iteration 
      common / Plt_Elec_Net / bInter_domain_iteration
      logical bInter_domain_iteration

C Status of multi-year climate database
      logical bMY_clm_err 
      integer lndbp   ! for length of standard database path
      character lltmp*144  ! to re-create plant database file name with path

C Reset error indicator and check if running as a dll.
      ier1=0
      call isadll(dll)

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Initialize the system variables.
      GAM=0.5
      BLDG3D=.FALSE.
      GRND3D=.FALSE.
      MSTROK=.FALSE.
      IndxSt=0
      LGrdSt=' '
      Tinitial=15.0

C If an input file has been specified then load it after confirmation
C and return to the main menu.
      H(1)='A model configuration file defines the model for'
      H(2)='simulation. This can be a building only (in whole or '
      H(3)='part), a plant network, or a combined building and '
      H(4)='plant system. It is also possible to specify building'
      H(5)='leakage and pressure distribution to enable air flow '
      H(6)='simulation. The file is created using ESRU: imp. '
      H(7)='Examples of system configuration files are given in '
      H(8)='sections 3.8 and 7 of the user manual.'
 42   if (.NOT.(ISIM.gt.1.or.autook)) then
        write(L144,'(a)') LCMDFL(1:lnblnk(LCMDFL))
        llt=lnblnk(LCMDFL)

C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(L144,' Model configuration file?',' ',96,DFCFG,
     &        'config file name',IER,8)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(L144,' Model configuration file?',' ',124,DFCFG,
     &        'config file name',IER,8)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(L144,' Model configuration file?',' ',144,DFCFG,
     &        'config file name',IER,8)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(L144,' Model configuration file?',' ',144,DFCFG,
     &      'config file name',IER,8)
        else
          CALL EASKF(L144,' Model configuration file?',' ',96,DFCFG,
     &      'config file name',IER,8)
        endif

        if(L144(1:2).ne.'  '.and.L144(1:4).ne.'UNKN')then
          write(LCMDFL,'(a)') L144(1:lnblnk(L144))
        else
          goto 42
        endif
      endif
      INQUIRE (FILE=LCMDFL,EXIST=XST)
      if(XST)then

C Find the path and local file name.
        call fdroot(LCMDFL,path,LCFGF)

C Use supplementary temporary unit number for IAPROB
        IAPROB=IFIL+22
        MODE='ALL '
        call usrmsg('Scanning the model description...',' ','-')
        CALL ERSYS_mmode(LCFGF,ifcfg,IAPROB,MODE,ITRC,IER,autook)
        call usrmsg('Scanning the model description...done ',' ','-')

C Building and plant description now complete.
C Unless error occured, initiate computation/setting-up of:
        if(ier.ne.0)then
          if(dll)then
            if(ier.eq.2)then
              return
            else
              dllsubr='mzinput'
              dllmesg='The read of configuration file was not clean.'
              ier=2
              return
            endif
          else
            dok=.false.
            h(1)='Usually problems with a model are detected prior to'
            h(2)='starting the simulator. It is unclear what the nature'
            h(3)='of the problem is - so be careful if you say no to '
            h(4)='continue the assessment with the current model.'
            CALL ASKOK(' The model scan was not clean. Do you want',
     &        'to rescan the model? (see help)',OK,dok,4)
            if(OK)goto 42
          endif
        endif
      else
        if(dll)then
          dllsubr='mzinput'
          dllmesg='The configuration file does not exist.'
          ier=2
          return
        else
          call usrmsg('The configuration file does not exist.',
     &     'Please retry. ','W')
          goto 42
        endif
      endif
      CFGOK=.TRUE.

C Use any simulation parameters in the configuration file.
      if(spsname(1:2).ne.'  '.and.spsname(1:4).ne.'UNKN')then
        call usesps(spsname,ier)
        if(ier.eq.2)then
          return
        endif
      endif

C Time-independent zone data (unless plant only).
      if(indcfg.ne.2) then
        call mzprec(IREP,IER)
        if(ier.eq.2)then
          return
        endif
        if(iflwn.ge.1.and.iairn.eq.0) call mfcdat
      else
        if(iflwn.eq.1) call mfcdat
      endif

C HVAC.
C If there is an hvac file scan it here.
      if(ihvacflag.eq.1) then
        call HVACINPUT(hvacfile)
      endif

C AIM-2.
      if(iAIM2.eq.1)then
        call AIM2_READIN
      endif

c DHW.
       if( iDHW_Flag.eq.1)then
        call DHW_READIN
       endif

C Mechanicanical ventilation.
      if(iMech.eq.1)then
        call MECH_VENT_READIN ( )
      endif

C GSHP.
      if(igshp .eq. 1)then
        call GSHPINPUT
      endif

C GCEP.
      if(igcep .eq. 1)then
        call GCEPINPUT
      endif

C Fluid flow simulation parameters if fluid flow network defined.
      if(iairn.ge.1.or.iflwn.eq.1) call mflwsu

C Unless building only, plant matrix template setup & checks.
C If plant component db file unit not set use iaprob. Work
C with lltmp because the standard database path might be pre-pended.
      if(indcfg.ne.1) then
        if(ipcdb.eq.0)ipcdb=IAPROB
        lndbp=lnblnk(standarddbpath)
        if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
          lltmp=LPCDB
        elseif(ipathpcdb.eq.2)then
          write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &      lpcdb(1:lnblnk(lpcdb))  ! prepend db folder path
        endif
        write(currentfile,'(a)') lltmp(1:lnblnk(lltmp))
        call EFOPRAN(ipcdb,lltmp,20,1,IER)
        call mzpmxt(ipcdb)

C Default flag for plant<->electrical network interdomain iteration
C to false. Plant coponents requiring this facility will set flag
C to true. Note: this code is not required in the stand-alone 
C plt simulator since, by definition, it doesn't support the
C electrical network.

        bInter_domain_iteration = .false.

C Plant manufacturer's and/or static data & plant template checks.
        call mzsdat
      endif

C If the power flow simulation flag is set then set up the electrical network.
      IF(IENTXIST.GT.0) CALL ENETSETU

C Advanced albedo model: pre-process snow depth file. The ascii file is
C read and converted to a binary file
      IF (groundreflmodel.EQ.3) THEN
        CALL PPGREF(IER)
        IF (IER.NE.0) RETURN
      ENDIF

C Report specified start-up period.
      write(outs,'(a,I3,a)')
     &     'A systems and building simulation start-up period of ',
     &     ITCNST,' days has been specified.'
      call edisp(iuout,outs)

      IF(GRND3D)THEN

C Specify ground simulation start-up period.
        CALL EFOPRAN(ICNC,LGDCNC,7,1,IER)
        READ(ICNC,REC=1,IOSTAT=ISTAT,ERR=7)ITCNSTG
        CALL ERPFREE(ICNC,ISTAT)
        H(1)='The start-up period is the number of days preceding'
        H(2)='the user defined simulation period. The results for'
        H(3)='this start-up period are not transferred to the'
        H(4)='results library and so cannot be analysed. '
        H(5)='  '
        H(6)='Note that a start-up period prior to January 1 '
        H(7)='will utilise climatic data from the end of the'
        H(8)='same year. '
        H(9)='  '
        H(10)='Note also that the start up period for the ground '
        H(11)='is dependent on its boundaries which are not '
        H(12)='considared for the default value provided which '
        H(13)='is estimated for the worse case.'
        if (IREP.eq.1) then
          write(OUTS,'(a,I4,a)')
     &        ' Ground simulation start-up period is',ITCNSTG,' days.'
          call EDISP(IUOUT,OUTS)
        else
          WRITE(OUTS,'(2A)')' Number of days for ground',
     &                                  ' simulation start-up period?'
          CALL EASKI(ITCNSTG,' ',OUTS,0,'F',99999,'W',1,
     &                                               'startup',IER,13)
        endif
      ENDIF

C Free plant components database if opened.
      if(ipcdb.ne.0) call erpfree(ipcdb,istat)

C If materials db not yet defined has not yet been opened do so.
C Use the unit IFMAT=IFIL+5 for this via common condb.
      IF(.NOT.MATDBOK)THEN
  55    write(currentfile,'(a)') LFMAT(1:lnblnk(LFMAT))
        call EFOPRAN(IFMAT,LFMAT,40,1,IER)
        if(ier.ne.0)then
C Database was not found, or file could not be read.
          if(dll)then
            dllsubr='mzinpt'
            dllmesg='Problem opening materials database.'
            ier=2
            return
C In interactive mode, prompt user for new database.
          elseif ( .not. autook ) then
            IER=0
            H(1)='The material db holds the thermophysical'
            H(2)='properties for aluminium, brick, earth, tile, etc.'
            ltmp=LFMAT
  56        CALL EASKS(ltmp,' Material database? ',
     &        ' ',72,DFCON,'materials db file name',IER,2)
            if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
              LFMAT=ltmp
              goto 55
            else
              goto 56
            endif
          else
C In silent running mode, notify user and terminate simulation.
            call UsrMsg('Materials database could not be found',
     &                  ' ', 'F')
          endif ! <- matches 'if(ier.ne.0)then'
        else

C Also check to see if the file has correct info (using pattern of
C code from esruprj/eddb.F subroutine opendb.
          IREC=601
          READ(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1006)NCLASS,
     &      (ICLN(I),I=1,30)
          if(NCLASS.LE.0)then
            goto 1006   ! no classifications so try alternatives
          else
            matver = 0.0
            MATDBOK=.TRUE.
            goto 139  !  a binary file found so carry on.
          endif
 1006     CALL ERPFREE(IFMAT,ISTAT)  ! if a read error look at alternatives.
          write(currentfile,'(a)') LFMAT(1:lnblnk(LFMAT))
          call rascimat(IFMAT,LFMAT,IER)  ! try current ascii format
          if(ier.eq.-2)then
            ier=0
            call rlegacymat(IFMAT,LFMAT,ier)  ! try older ascii format
            if(ier.ne.0)then
              if(dll)then
                dllsubr='mzinpt'
                dllmesg='Problem opening materials database.'
                ier=2
                return
              else
                call usrmsg('No readable materials database was not',
     &            'found or file was corrupt','W')
              endif
            else
              call eclose(matver,1.1,0.001,close)
              if(close) MATDBOK=.TRUE.
            endif
          elseif(ier.eq.0)then
            call eclose(matver,1.1,0.001,close)
            if(close) MATDBOK=.TRUE.
          endif
        endif
      ENDIF

C Read in the multilayer construction database so that it's
C data may be accessed by control and reporting facilities.
C Use file unit IAPROB to read db.
 139  IFMUL=IAPROB
      CALL ERPFREE(IFMUL,ISTAT)
      call FINDFIL(LFMUL,XST)
      if(.NOT.XST)then
        write(outs,'(A,A)') ' Could not find construction database ',
     &    LFMUL(1:lnblnk(LFMUL))
        call isadll(dll)
        if(dll)then
          dllsubr='mzinpt'
          dllmesg=outs
          ier=2
          return
        else
          call usrmsg(outs,' ','W')
        endif
      else
        write(currentfile,'(a)') lfmul(1:lnblnk(lfmul))
        CALL ERMLDB(0,iuout,IER)
        if(IER.NE.0)then
          outs='Problem during reading of construction database.'
          if(dll)then
            dllsubr='mzinpt'
            dllmesg=outs
            ier=2
            return
          else
C Note: the databases are not actually referenced by bps. This is a non-
C       critical error --- warn user and clear error flag
            call usrmsg(outs,' ','W')
            ier = 0 
          endif
        else
          MLDBOK=.TRUE.
        endif
      endif

C If a temporal file has been referenced in the configuration file
C then process it, creating a scratch file for use during the
C simulation and filling the TDFFLG2 commons.
      IF(ITDFLG.ne.0)THEN
        call supplyandcheck(ltdfa,'S',ier)
        if(IER.NE.0)then
          outs='Problem opening temporal file...aborting.'
          if(dll)then
            dllsubr='mzinpt'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
            ier=2
            return
          endif
        endif
      ENDIF
  

 35   if ( bSY_climate_defined ) then
 
C Now check if the climate data file in the configuration file
C is correct. If not, warn user that climate file has not
C been properly entered.

C Collect & process single-year climate file
C Note: the climate file uses a different error flag than
C the global model definition, as it can be specified separately.
         write(currentfile,'(a)') lclim(1:lnblnk(lclim))
         call EFOPRAN(ICLIM,LCLIM,144,0,IClmErr)
         IF(iClmErr.eq.0)THEN
            IREC=366
            READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=103)ICYEAR,ICDNGH
            IREC=367
            READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=103)CLMLOC
            WRITE(OUTSTR,1000)CLMLOC,ICYEAR
 1000       FORMAT(' Climate data: ',A30,': ',I5)
            CALL EDISP(IUOUT,' ')
            CALL EDISP(IUOUT,OUTSTR)
            IRTYPE=0
            IF(ICDNGH.EQ.123)IRTYPE=1
   
C If older configuration, confirm the year.
            if(icfgv.lt.3)then
               h(1)='Older config files do not have a year record. '
               h(2)='If you want the year to be taken from the climate'
               h(3)='database use the default.'
               IYEAR=ICYEAR
               CALL EASKI(IYEAR,
     &         'Year for the simulation? (The configuration does not',
     &         'specify one and the climate db year is shown) ',
     &         1900,'W',2005,'@',ICYEAR,'year from clim',IClmErr,3)
            endif
            
         ELSE

C Set climate definition flag to false 
            bSY_climate_defined = .false.
            if(dll)then
C Kill simulation if in DLL mode            
               dllsubr='mzinpt'
               dllmesg='Problem opening climate database.'
               ier=2
               return
            else
C Warn the user that the climate file definition has failed. 
               write(louts,'(A,A,A)')
     &           'Warning: specified climate file (',
     &           LCLIM(1:LNBLNK(LCLIM)), ') could not be found.'
              
               call edisp248 (iuout,louts,100)
               if ( autook ) then

C If running silently, stop
                  call edisp248(iuout,louts,100)               
                  call pauses(3)   ! pause for 3 seconds then abort program.
                  call epagend
                  stop
               else
C If running interactively, warn user and free file channel
                  call usrmsg(outs,' ','W')
               endif
            endif
         ENDIF

C Close & free file (it will be opened again before the start
C of the simulation         
         CALL ERPFREE(ICLIM,ISTAT)

         
      endif 

      if ( bMY_climates_defined ) then

C Check status of multi-year climate file: parse file

         call parse_MY_clm_db ( ICLIM, autook, bMY_clm_err)
C Check validity of records
         if ( .not. bMY_clm_err )
     &           call check_MY_clm_db ( autook, 'fix', bMY_clm_err )
         
         if ( bMY_clm_err ) then
            bMY_climates_defined = .false.
         else
            bMY_climates_defined = .true.
         endif 
      
      endif      ! <- matches " if ( bSY_climate_defined ) ..."

C Open and scan the bidirectional data file if it exists.
      if(lnblnk(bidirfile).eq.0)then
        continue
      elseif(bidirfile(1:7).eq.'UNKNOWN')then
        continue
      else
        iua=IAPROB
        call erbiwin(iuout,iua,bidirfile,ier)
        if(ier.eq.0)then
          call edisp(itru,'The bi-directional data was sucessfully')
          call edisp(itru,'scanned.')
        else
          call edisp(itru,'There was a problem reading the bi-')
          call edisp(itru,'directional data.')
        endif
      ENDIF

      return

C Error.
  103 if(dll)then
        dllsubr='mzinpt'
        dllmesg='Problem opening climate database.'
        ier=2
        return
      else
        dok=.false.
        h(1)='At simulation time there should not be any problems'
        h(2)='with the climate data. Check your model. '
        CALL ASKOK(' Problem opening climate file, try again? ',
     &             ' (no exits program)',OK,dok,2)
        IF(OK)THEN
          goto 35
        ELSE
          CALL EPAGEND
          CALL ERPFREE(ICLIM,ISTAT)
          call pauses(2)   ! pause for 2 seconds then abort program.
          call epagend
          STOP
        ENDIF
      endif

    7 if(dll)then
        dllsubr='mzinpt'
        dllmesg='Problem at first record of ground connections file.'
        ier=2
        return
      else
        CALL USRMSG(' Error occured at the first record ',
     &              ' in the ground connections file.','W')
        call pauses(2)   ! pause for 2 seconds then abort program.
        CALL EPAGEND
        STOP
      endif

 1001 write(outs,'(A,I3)') ' mzinpt: climate file error @ rec ',irec
      call edisp(iuout,outs)
      call epwait
      return
      end

C ******************** mzprec
C MZPREC initiates computation of all time-independent zone
C properties, checks the legality of all type 3 connections, and
C checks if user wants to re-specify simulation start-up period.
C IREP=1 implies no reporting or data checking.

      SUBROUTINE MZPREC(IREP,IER)
#include "building.h"
#include "model.h"

C geometry.h provides commons G0/G2/c20/precz.
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/WARN/IWARN
      COMMON/WRNOFF/IWMOFF
      COMMON/FILEP/IFIL

      common/trc/itrc
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu

      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

C Version of operations file. ip3ver=0 standard, =1 sorted with header
C =2 with more than 3 day types
      common/p3ver/ip3ver(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C14/UGRDTP(12,MGRDP),NGRDP
      COMMON/C24/IZSTOCN(MCOM,MS)

C During setup scan for inbuilt and user defined ground temperatures.
C igttype: if > 1 then index points to most often used standard profile
C         if < 1 then index points to most often used user profile
C         if = 0 then use compiled pattern.
      common/C25/igttype,GTMP(12)
      common/ground/grdtmp(12,mgrdp)

      COMMON/PREC1/SNA(MCOM,MS)
      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)
      COMMON/PREC7/ITCNST
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),AE(MCOM,MS)
      COMMON/PREC13/C(MCOM,MS,MN,2),QC(MCOM,MS,MN)
      COMMON/PREC14/emarea(MCOM)

C izsfloor is the index (within the zone) of a floor surface in each zone
C  (zero is no surface is close to horizontal facing up).
C izsceil is the index (within the zone) of a ceiling surface in each zone 
C  (zero is no surface is close to horizontal facing down).
      COMMON/PREC16/izsfloor(MCOM),izsceil(MCOM)

      COMMON/VTHP30/ILTHPS,ILTHPZ(MCOM)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR3D100/BLDG3D,ZONE3D(MCOM)
      COMMON/MOIST01/MSTROK,MSTRZN(MCOM)
      COMMON/MOIST02/LMOIST(MCOM)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)

C Uncertainty common.
      COMMON/UA5/IMET,ISIM,NRSIM
      common/spmfxst/ispmxist,spflnam 

C Thermal bridge common block is described in esruprj/edgeo.F.
      integer nbrdg, ibrdg
      real psi,lngth,losspercent,totheatloss,thbrpercent
      common/THRBRDG/nbrdg(MCOM),psi(MCOM,16),lngth(MCOM,16),
     &               ibrdg(MCOM,16),losspercent(MCOM),totheatloss(MCOM),
     &               thbrpercent

C Dll mode error subroutine and error details.
      common/dllerr/dllsubr,dllmesg

C Simulator parameters.
      COMMON/SPFL/spfileok,perok,tstepok,saveok,autook,exitok,startupok
      common/SPFLN/spsname
      common/spfldes/spfdescr(MSPS)
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres
      LOGICAL     spfileok,perok,tstepok,saveok,autook,exitok,startupok
      logical newgeo  ! to use for testing if new/old geometry file.

      CHARACTER*72 LMOIST,LMSTR
      character outs*124,CXSTR*78,spflnam*72,H*72
      CHARACTER SOTHER*24,SNAME*12,SMLCN*32,SVFC*4,SOTF*32,SUSE*8
      CHARACTER SPARENT*12
      character sblres*72,sflres*72,splres*72,smstres*72,sipvres*72
      character selres*72,spsname*8,spfdescr*8
      character dllsubr*12,dllmesg*124
      LOGICAL BLDG3D,ZONE3D,MSTROK,MSTRZN
      LOGICAL ILTHPS,ILTHPZ,CLOSE
      logical dll,prob
      dimension igarray(12),iguarray(12)

      if (.not.startupok) ITCNST=0
      IWARN=0
      call isadll(dll)

C Zone-by-zone. Use IUNIT for geometry file.
      newgeo=.false.  ! assume older format geometry.
      IUNIT=IFIL+1
      NNDS=0
      MNDS=0

C Clear igarray() & iguarray()
       igttype=0
       do 9 ij=1,12
         igarray(ij)=0
         iguarray(ij)=0
  9    continue
      call usrmsg(' Checking zone descriptions... ',' ','-')
      ILTHPS=.FALSE.
      BLDG3D=.FALSE.
      MSTROK=.FALSE.
      DO 10 I=1,NCOMP
        IZ=I
        MNDZ(IZ)=0

C Read contents of the I'th zone data files.
        CALL EROPER(ITRC,ITU,IUNIT,IZ,IER)
        if(ier.eq.1)then
          write(outs,'(a,i2,a)') 
     &        'Error reading operation file for zone ',IZ,'.'
          if(dll)then
            dllsubr='MZPREC'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
            call pauses(3)   ! pause for 3 seconds then abort program.
            CALL EPAGEND
            STOP
          endif
        endif

C If a non-sorted operations file has been read warn user and
C translate the casual gains.

C Debug.
C        write(6,*) IZ,IP3VER(IZ)

        if(ip3ver(iz).eq.0)then
          write(outs,'(a,i2,a)') 
     &      'Operation file for zone ',IZ,' periods being sorted!'
          call edisp(iuout,outs)
          call PROCESSOLDCAS(IZ,itrc,itu,IER)
        endif

C Zero the thermal bridge heat loss before (perhaps) reading in from geometry file.
        totheatloss(IZ)=0.0

        call eclose(gversion(IZ),1.1,0.01,newgeo)
        if(newgeo)then
          call georead(IUNIT,LGEOM(IZ),IZ,1,ITU,IER)
        else
          call egomin(IUNIT,LGEOM(IZ),IZ,1,ITRC,ITU,IER)
        endif
        if(ier.eq.1)then
          write(outs,'(a,i2,a)') 
     &        'Error reading geometry file for zone ',IZ,'.'
          if(dll)then
            dllsubr='MZPREC'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
            call pauses(3)   ! pause for 3 seconds then abort program.
            CALL EPAGEND
            STOP
          endif
        endif

        CALL ECONST(LTHRM(IZ),IUNIT,IZ,ITRC,ITU,IER)
        if(ier.eq.1)then
          write(outs,'(a,i2,a)') 
     &        'Error reading construction file for zone ',IZ,'.'
          if(dll)then
            dllsubr='MZPREC'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
            call pauses(3)   ! pause for 3 seconds then abort program.
            CALL EPAGEND
            STOP
          endif
        endif

C Open any timestep based utility files.
        CALL MZUTIL(IZ,IER)
        if(ier.eq.2.and.dll)then
          return
        endif
        IF(ZONE3D(IZ))BLDG3D=.TRUE.

C Now alter properties for uncertainty analysis. 
        if (ISIM.gt.1) then
          if (IMET.eq.1) then
            call DSA(IZ)
          elseif (IMET.eq.2) then
            call FCTA(IZ)
          elseif (IMET.ge.3) then
            call MCSA(IZ)
          endif
        endif

C Now initiate computation of all time-independent zone properties.
        CALL MZAREA(IZ)

        CALL MZVOLM(IZ)
        CALL MZPAZI(IZ)
        IF(IVF(IZ).EQ.0)CALL MZVFCL(IZ)
        CALL MZVENC(IZ)
        CALL MZCASG(IZ)

C If the start-up period has been specified in a simulator parameter 
C file then do not recalculate it.
        if(.not.startupok) CALL MZTCON(IZ)
        CALL MZMISC(IZ)
        IF(MSTRZN(IZ))THEN
          MSTROK=.TRUE.
          LMSTR=LMOIST(IZ)

C Read moisture file.
          CALL EMSTRR(LMSTR,IUNIT,IZ,ITRC,ITU,IER)
        ENDIF

C Keep track of the total surface area/emisivity for use
C in apportioning casual and plant radiation to surfaces.
C Also instanciate izsfloor and izsceil
        emarea(iz)=0.0
        izsfloor(iz)=0
        izsceil(iz)=0
        do 80 kk=1,nconst(iz)
          emarea(iz)=emarea(iz)+sna(iz,kk)*ei(iz,kk)
          if(PELV(iz,kk).GE.89.98.AND.PELV(iz,kk).LE.90.02)then
            izsceil(iz)=kk
          endif
          if(PELV(iz,kk).GE.-90.02.AND.PELV(iz,kk).LE.-89.98)then
            izsfloor(iz)=kk
          endif
  80    continue

C Assign two nodes per layer.
        CALL NODTWO(IZ)
        NNDS=NNDS+NNDZ(IZ)
        CALL MZCOE1(IZ)
        IF(ICGC(IZ).EQ.1)CALL DAYFAC(IZ)
   10 CONTINUE
      call usrmsg(' Checking zone descriptions...done ',' ','-')
      if (IREP.eq.1) return

C Check connections between type 3 surfaces to ensure
C no mismatch.
      IW=IWARN
      IWARN=0
      call usrmsg(' Checking model topology...',' ','-')
      call ckcurmatch(prob,iprob)
      if(prob.and.iprob.gt.1)then
        outs='More than one complaint about model contiguity'
        if(dll)then
          dllsubr='MZPREC'
          dllmesg=outs
          ier=2
          return
        else
          call edisp(iuout,outs)
          IWARN=IWARN+iprob
        endif
      endif
      DO 20 I=1,NCON
        NZ1=IC1(I)
        NS1=IE1(I)
        NZ2=IC2(I)
        NS2=IE2(I)

C Check for ground temperature profiles and keep track of
C which ones are most often used.
        if(ICT(I).eq.4)then
          if(NZ2.gt.0)then
            igarray(NZ2)=igarray(NZ2)+1
          elseif(NZ2.eq.-3)then
            continue
          else
            iguarray(NS2)=iguarray(NS2)+1
          endif
        endif
        IF(ICT(I).NE.3)goto 20

C Make sure the other side is an allowable surface.
        if(NZ2.gt.NCOMP.or.NS2.gt.NZSUR(NZ2))then
          write(outs,'(3a,I3,A,i3,A,I3,a,i3,a)')' Zone ',zname(NZ1),
     &      ' (',NZ1,') surface',NS1,' connected to nonexistant zone (',
     &      NZ2,') or surface (',NS2,').'
          if(dll)then
            dllsubr='MZPREC'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
            call pauses(3)   ! pause for 3 seconds then abort program.
            CALL EPAGEND
            STOP
          endif
        endif

C Label for the connection being checked.
        CALL CONXINFO(1,I,CXSTR)

C For the `i` connection find its matching connection and see that it
C has reciprocal information.
        ioc=IZSTOCN(NZ2,NS2)
        if(ioc.eq.0)then
          write(outs,'(3a,I3,A,i3,A)')' Zone ',zname(NZ2),
     &      ' (',NZ2,') surface',NS2,' connection cannot be resolved.'
          if(dll)then
            dllsubr='MZPREC'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
            call pauses(3)   ! pause for 3 seconds then abort program.
            CALL EPAGEND
            STOP
          endif
        endif

C Debug...
C        write(6,*) 'i,ioc,nz2,ns2,IC1(ioc),IE1(ioc)',i,ioc,nz2,ns2,
C     &    IC1(ioc),IE1(ioc)

C Check back to see if the other side is also a type 3 connection.
        if(ICT(ioc).ne.3)then
          IF(IWMOFF.ne.1)then
            write(outs,'(A,A12,A2,I3,A,I3,A)') ' Zone ',zname(NZ2),
     &        ' (',NZ2,') Surface',NS2,' is not'
            call edisp(iuout,outs)
            call edisp(iuout,' connected to a Type 3 surface.')
          endif
          IWARN=IWARN+1
        else
          if(NZ2.EQ.IC1(ioc).AND.NS2.EQ.IE1(ioc))then

C We have a zone connection that matches so carry out other tests.
C to ensure proper matching. The first test involves surface azimuth
C and elevation angles to ensure surfaces face each other.
            SELV=PELV(NZ1,NS1)+PELV(NZ2,NS2)
            if(SELV.LT.-0.9.OR.SELV.GT.0.9)then
              write(outs,'(A,A)')' For: ',CXSTR
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
              write(outs,'(A,2f8.3,A)')' the surface elevations (',
     &          PELV(NZ1,NS1),PELV(NZ2,NS2),') beyond tolerance.'
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
              IF(IWMOFF.ne.1)call edisp(iuout,' (should be + & - 1deg')
              IWARN=IWARN+1
            endif
            SELV=ABS(PELV(NZ1,NS1))
            IF(SELV.GT.89.1.AND.SELV.LT.90.9)goto 71
            DAZI=PAZI(NZ1,NS1)-PAZI(NZ2,NS2)
            DAZI=ABS(DAZI)
            if(DAZI.LT.179.1.OR.DAZI.GT.180.9)then
              write(outs,'(A,A)')' For: ',CXSTR
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
              write(outs,'(A,2f8.3,A)')' the surface azimuths (',
     &          PAZI(NZ1,NS1),PAZI(NZ2,NS2),') beyond tolerance.'
              IF(IWMOFF.ne.1) call edisp(iuout,outs)
              IF(IWMOFF.ne.1) call edisp(iuout,
     &            '(should be ~180 deg dif')
              IWARN=IWARN+1
            endif

C Check physical properties where possible.
   71       if(NELTS(NZ1,NS1).NE.NELTS(NZ2,NS2))then
              write(outs,'(A,A)')' For: ',CXSTR
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
              write(outs,'(A,A,i3,a,i3,A)')' the mlc layers (',
     &          SMLCN(NS1),NELTS(NZ1,NS1),' & ',NELTS(NZ2,NS2),
     &          ') don`t match.'
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
              IWARN=IWARN+1
              goto 20
            endif
            if(NGAPS(NZ1,NS1).NE.NGAPS(NZ2,NS2))then
              write(outs,'(A,A)')' For: ',CXSTR
              if(IWMOFF.ne.1)call edisp(iuout,outs)
              write(outs,'(A,A,2i3,A)')' the no. of mlc air gaps (',
     &         SMLCN(NS1),NGAPS(NZ1,NS1),NGAPS(NZ2,NS2),') don`t match.'
              if(IWMOFF.ne.1)call edisp(iuout,outs)
              IWARN=IWARN+1
              goto 20
            endif

C Check surface radiative properties.
            CALL ECLOSE(EE(NZ1,NS1),EI(NZ2,NS2),0.02,CLOSE)
            IF(.NOT.CLOSE)then
              write(outs,'(A,A)')' For: ',CXSTR
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
              write(outs,'(A,2f5.2,A)')' the outside face emissivity (',
     &          EE(NZ1,NS1),EI(NZ2,NS2),') don`t match.'
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
              IWARN=IWARN+1
              goto 20
            endif
            CALL ECLOSE(EI(NZ1,NS1),EE(NZ2,NS2),0.02,CLOSE)
            IF(.NOT.CLOSE)then
              write(outs,'(A,A)')' For: ',CXSTR
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
              write(outs,'(A,2f5.2,A)')' the inside face emissivity (',
     &          EI(NZ1,NS1),EE(NZ2,NS2),') don`t match.'
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
              IWARN=IWARN+1
              goto 20
            endif
            CALL ECLOSE(AE(NZ1,NS1),AI(NZ2,NS2),0.02,CLOSE)
            IF(.NOT.CLOSE)then
              write(outs,'(A,A)')' For: ',CXSTR
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
            write(outs,'(A,2f5.2,A)')' the outside face absorptivity (',
     &          AE(NZ1,NS1),AI(NZ2,NS2),') don`t match.'
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
              IWARN=IWARN+1
              goto 20
            endif
            CALL ECLOSE(AI(NZ1,NS1),AE(NZ2,NS2),0.02,CLOSE)
            IF(.NOT.CLOSE)then
              write(outs,'(A,A)')' For: ',CXSTR
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
             write(outs,'(A,2f5.2,A)')' the inside face absorptivity (',
     &          AI(NZ1,NS1),AE(NZ2,NS2),') don`t match.'
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
              IWARN=IWARN+1
              goto 20
            endif

C Check thermophysical properties of construction.
            NN=NNDC(NZ1,NS1)-1
            KK=NN
            DO 40 K=2,NN
              D1=C(NZ1,NS1,K,1)-C(NZ2,NS2,KK,2)
              DD1=ABS(C(NZ1,NS1,K,1)/100000.0)
              IF(D1.LT.-DD1.OR.D1.GT.DD1) then
                write(outs,'(A,A)')' For: ',CXSTR
                IF(IWMOFF.ne.1)call edisp(iuout,outs)
               write(outs,'(A,i3,A)')' the thermophysical prp of layer',
     &            K,' doesn`t match.'
                IF(IWMOFF.ne.1)call edisp(iuout,outs)
                IWARN=IWARN+1
                goto 20
              endif
              D2=C(NZ1,NS1,K,2)-C(NZ2,NS2,KK,1)
              DD2=ABS(C(NZ1,NS1,K,2)/100000.0)
              IF(D2.LT.-DD2.OR.D2.GT.DD2) then
                write(outs,'(A,A)')' For: ',CXSTR
                IF(IWMOFF.ne.1)call edisp(iuout,outs)
              write(outs,'(A,i3,A)')' the thermophysical prop of layer',
     &             K,' doesn`t match.'
                IF(IWMOFF.ne.1)call edisp(iuout,outs)
                IWARN=IWARN+1
                goto 20
              endif
              KK=KK-1
   40       CONTINUE
          else

C We have a mismatch in zone connections (connection which should have
C matched does not).
            if(IWMOFF.ne.1)then
              write(outs,'(A,I4,A,I4,A)')
     &          ' Mismatch between connections',I,' and',ioc,'.'
              call edisp(iuout,outs)
              IWARN=IWARN+1
              goto 20
            endif
          endif
        endif
   20 continue
      IWARN=IWARN+IW
      call usrmsg(' Checking model topology...done',' ','-')

C Find out which was most often used in terms of ground temperatures.
      igmax=0
      igmaxindex=0
      igumax=0
      igumaxindex=0
      do 8 ij=1,12
        if(igarray(ij).gt.igmax)then
          igmax=igarray(ij)
          igmaxindex=ij
        endif
        if(iguarray(ij).gt.igumax)then
          igumax=iguarray(ij)
          igumaxindex=ij
        endif
  8   continue

C Debug.
C      write(6,*) igmax,igmaxindex,igarray
C      write(6,*) igumax,igumaxindex,iguarray

      if(igmaxindex.eq.0.and.igumaxindex.eq.0)then

C Nothing specified as a connection so use UK monthly mean
C ground temperatures at 1.2m depth as given by Lacy.
        igtype=0
        GTMP(1)=7.28
        GTMP(2)=6.5
        GTMP(3)=6.56
        GTMP(4)=7.33
        GTMP(5)=9.06
        GTMP(6)=11.39
        GTMP(7)=13.5
        GTMP(8)=14.33
        GTMP(9)=14.11
        GTMP(10)=11.72
        GTMP(11)=10.72
        GTMP(12)=8.67
      elseif(igmax.gt.igumax)then

C More standard profiles used so get from grdtmp
        igttype=igmaxindex 
        do 6 ij = 1,12
          GTMP(ij)=grdtmp(ij,igmaxindex)
  6     continue
      elseif(igmax.le.igumax)then

C More user defined profiles used so get from UGRDTP
        igttype= -1 * igumaxindex
        do 5 ij = 1,12
          GTMP(ij)=UGRDTP(ij,igumaxindex)
  5     continue
      endif
C Debug...
C      write(6,*) 'lw gtmp is ',GTMP

C Identify special materials file & call routine to read it.
      if(ispmxist.gt.0)then
        call spminit
      endif

      RETURN
      END

C ******************** MZUTIL ********************

C MZUTIL reads the zone utilities file if one is active. This
C file contains information on the source of any data which should
C be used in preference to the zone file defaults.
C Timestep zone airflow is handled by tdf.

      SUBROUTINE MZUTIL(ICOMP,IER)
#include "building.h"
#include "model.h"
#include "cfd.h"
#include "espriou.h"
#include "CFC_common.h"
C espriou.h provides currentfile.

      common/trc/itrc
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/ER1/IER1

      COMMON/ICFNOD/ICFD,ICP
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      common/ndcfd/NCFDND,ICFDND(MNZ),NCONF
      COMMON/CHANNL/NOCHAN
      common/flvpar/Lflpar(MCOM)
      common/CFDres/photon,flwvis
      common/bsmtdef/iBSIMP(MCOM),LBSIMP(MCOM)
      COMMON/cfcall/icfcal(MNZ)
      COMMON/ACCrec/IACC

C Active domain common.
      common/ACTDOM/CFDOK
      common/dllerr/dllsubr,dllmesg

C Experimental Hydrogen controller. use with caution
C Residential hydrogen cogen: control file
      common/resh2ctl/iresh2ctl,resh2flnam
      integer iresh2ctl         !- flag indicating that file required by residential hydrogen
                                !- cogen controller is specified
      character*72 resh2flnam   !- name of file containing residential hydrogen cogen
                                !- controller inputs

      CHARACTER NAME*5,OUTSTR*124
      CHARACTER*72 LCFD,Lflpar
      CHARACTER*72 lfil
      CHARACTER*72 LBSIMP
      character dllsubr*12,dllmesg*124,outs*124

      logical CFDOK,photon,flwvis,dll

C Reset warning flag and check and see if running in dll mode.
      IER1=0
      IUNIT=IFIL+1
      call isadll(dll)
      
C Open zone viewfactors, pre-calculated shading, convection regimes.
      IF(IVF(ICOMP).EQ.1)CALL MZVFIN(ICOMP)
      IF(ISI(ICOMP).EQ.1)CALL MZSHIN(ICOMP)
      IF(IHC(ICOMP).EQ.1)CALL MZHCFV2(ICOMP)

C If BASESIMP foundation defined, read it in.
      if(iBSIMP(icomp).eq.1)then
        call BASEREAD(icomp,ier)
        if(dll.and.ier.eq.2)then
          return
        endif
      endif

C H2-dep_controller: start
C Experimental Hydrogen controller. Use with caution
C Residential hydrogen cogen : control file
      if ( iresh2ctl .eq. 1 ) then
         call RESH2_Ctl_Read
      endif
C H2-dep_controller: end

C If zone TMC file read it in.
      IF(ITW(ICOMP).EQ.1)then
        CALL ERTWIN(itrc,itu,iunit,LTWIN(ICOMP),icomp,ier)
        if(dll.and.ier.eq.2)then
          return
        endif
      endif

C If zone CFC file read it in.
      if (icfc(icomp).eq.1)then
        call read_in_cfc_file(itrc,itu,iunit,lcfcin(icomp),icomp,ier)
        if(dll.and.ier.eq.2)then
          return
        end if
      end if

C If zone casual gain control file, read it in.
      IF(ICGC(ICOMP).EQ.1)then
        CALL ercgcf(itrc,itu,LCGCIN(ICOMP),ICOMP,ier)
        if(dll.and.ier.eq.2)then
          return
        endif
        if (ier.eq.-101) call usrmsg(
     &            'Daylight coefficients not yet calculated!','  ','F')
      endif

C CFD domanin.
      IF(ABS(IFCFD(ICOMP)).GT.0)THEN
        ICP=ICOMP

C If adaptive conflation control is active, then open a file to record
C the actions of the controller.
        if (ABS(IFCFD(ICOMP)).eq.4.or.ABS(IFCFD(ICOMP)).eq.5) then
          lfil='ACC-actions.rec'
          IACC = IFIL + 29
          call efopseq(IACC,lfil,4,ier)
        endif

C Set the flags that indicate whether there is thermal conflation
C and whether there is air-flow conflation.  These flags affect how
C the .dfd file is read in.  Five conflation mechanisms
C are currently supported (1-5).  If a new mechanism is,
C code must be added below to properly set IBLD and IMFN for the new mechanism.
C If IBLD=1 there is thermal conflation and mapping between
C           dfs and bps surfaces must be provided.
C If IMFN=1 there is air-flow conflation and mapping between
C           dfs and bps connections must be provided.
        IBLD=0
        IMFN=0
        if(IFCFD(ICOMP).eq.1) IBLD=1
        if(IFCFD(ICOMP).eq.2) IBLD=1
        if(IFCFD(ICOMP).eq.3) IMFN=1
        if(IFCFD(ICOMP).eq.4) IBLD=1
        if(IFCFD(ICOMP).eq.5) IBLD=1
        ICFD=0
 98     ICFD=ICFD+1

C Read CFD configuration file and set length scales basd on ESP-r standard
C geometry model.
        if(ICFDND(ICFD).EQ.ICOMP) then
          IFL=IFIL+1
          CALL EFOPSEQ(IFL,LCFD(ICOMP),1,IER)
          CALL STRIPC(IFL,OUTSTR,0,ND,1,'dfd line 1',IER)
          if(OUTSTR(1:15).eq.'DFS DESCRIPTION')then
            rewind(IFL)
            CALL CFDDTA(ITRC,ITU,ICOMP,IBLD,IMFN,IER)
          else
            CALL DFDREAD(ICOMP,itrc,itu,IER)
          endif
          CALL CFDLENS
          CFDOK=.true.

C Initialize counter to indicate that CFD has not yet been invoked. This
C will be incremented each time CFD is called for this zone.
          icfcal(ICFD) = 0
        endif
        if(ICFD.LT.NCONF)goto 98
      ENDIF

      RETURN

 1002 outs='Problem reading *air or *cas!'
      CALL USRMSG(outs,' ','W')
      goto 1000
 1004 outs='Could not open timestep air file!'
      CALL USRMSG(outs,' ','W')
      goto 1000
 1005 outs='Not a zone timestep air flow file!'
      CALL USRMSG(outs,' ','W')
      goto 1000
 1007 outs='Could not open timestep casual file!'
      CALL USRMSG(outs,' ','W')
      goto 1000
 1008 outs='Not a zone timestep casual gain file!'
      CALL USRMSG(outs,' ','W')

C Record errors if running in dll mode or simply return.
 1000 if(dll)then
        dllsubr='MZUTIL'
        dllmesg=outs
        ier=2
        return
      else
        IER1=1
        RETURN
      endif
      END

C Routines for passing presimulation data in configuration file to
C simulation control.

C Parameters which can be set in the control file are as follows:
C *St_up - startup period
C *Period - the simulation start day and month, end day and month
C *Time step - the number of time steps per hour
C *Hourly - whether each timestep is written to the file.
C *Save option - 1,2,3,4
C *Results - building results file
C *Flow_results - air flow results file
C *Plant_results - plant results file

C Climate and control are as specified in the configuration file.

C ********************** USESPS
C USESPS uses configuration presimuilation data.
      SUBROUTINE USESPS(name,IER)
#include "building.h"
#include "MultiYear_simulations.h"
      
      integer lnblnk  ! function definition

      common/outin/iuout,iuin
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      COMMON/PCTIME/TIMSEC
      COMMON/PCTSTP/NTSTPP
      common/save/isave
      common/prec7/itcnst
      common/spfldes/spfdescr(MSPS)
      common/spflper/isstday(MSPS),isstmon(MSPS),isfnday(MSPS),
     &               isfnmon(MSPS)
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh

C Once per hour storage of results, averaged over that hour.
      common/rdcgen/irdact
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres

C lafres  - fluid flow network results file
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/ENETRES/ERESLIB
      COMMON/ENTFILE/ENTFLNAM,IENTXIST
      common/dllerr/dllsubr,dllmesg

      COMMON/SPFL/spfileok,perok,tstepok,saveok,autook,exitok,startupok
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      LOGICAL     spfileok,perok,tstepok,saveok,autook,exitok,startupok
      logical dll,match

      character RFILE*72,PFILE*72,MSTRFILE*72,LAFRES*72
      character*72 LAPROB,ENTFLNAM,ERESLIB
      character outs*124, outs2*124
      character sblres*72,sflres*72,splres*72,smstres*72,sipvres*72
      character selres*72,spfdescr*8,name*8
      character dllsubr*12,dllmesg*124

C Check if running in dll mode.
      call isadll(dll)
      IER=0

C Assign data.
      IPARS=0
      if(isstup.ne.0)then
        ITCNST=isstup
        startupok=.true.
      endif
      if(isbnstep.ne.0)then
        ntstep=isbnstep

C Also check against user preference in simulation parameters for 
C whether to save every timestep to the file.
        if(isavgh.eq.1)then
          irdact=1
        else
          irdact=0
        endif
        tstepok=.true.

C Debug.
C       write(6,*) 'in input irdact & isavgh are ',irdact,isavgh
 
      endif

C Plant timesteps and plant time increment and
C calculate the length of the plant time increment in seconds.
      if(ispnstep.ne.0)then
        NTSTPP=ispnstep
        TIMSEC=3600.0/(FLOAT(NTSTEP)*FLOAT(NTSTPP))
        tstepok=.true.
      else

C Prevent ntstpp from being zero.
        NTSTPP=1
      endif
      if(issave.ge.0)then
        isave=issave
        saveok=.true.
      endif

C Match period name in command line.
      ln=lnblnk(name)
      match=.false.

      do 42 i=1, nsset
        if(name(1:ln).eq.spfdescr(i)(1:lnblnk(spfdescr(i))))then
          match=.true.

C Determine if period supports multi-year simulations
          bMY_sim_enabled = bMY_period_support (i)

          isd1=isstday(i)
          ism1=isstmon(i)
          CALL EDAY(isd1,ism1,isds)
          isd2=isfnday(i)
          ism2=isfnmon(i)
          CALL EDAY(isd2,ism2,isdf)
          
          if ( bMY_sim_enabled ) then
C Handle start/end year
            iMY_start_year = iMY_period_start_years(i)
            iMY_end_year = iMY_period_end_years(i)

            isdf = isdf + 365 * ( iMY_end_year - iMY_start_year )

          else
C Assign start/end year to simulation year. These data won't be
C used in single simulaiton mode, but assigning them now enables
C the interface to initialize the start/end year to relevant values
C (ie. not zero!) when the user interactively switches to multi-year
C mode.
            
            iMY_start_year = iYear
            iMY_end_year   = iYear   

          endif

          perok = .true.
          
          if ( isdf .lt. isds ) then

            perok = .false. 

            if ( bMY_sim_enabled ) then
            
              write(outs,'(A,I2,A,I2,A,I4,A)')
     &          ' Error: Period start day (',
     &          isd1,'/',ism1,'/',iMY_start_year,')'

              write(outs2,'(A,I2,A,I2,A,I4,A)')
     &          ' is after period finish day (',
     &          isd2,'/',ism2,'/',iMY_end_year,')'

            else

              write(outs,'(A,I2,A,I2,A)')
     &          ' Error: Period start day (',
     &          isd1,'/',ism1,')'

              write(outs2,'(A,I2,A,I2,A)')
     &          ' is after period finish day (',
     &          isd2,'/',ism2,')'

            endif

            call edisp(iuout, outs )
            call edisp(iuout, outs2)

            if ( autook ) then

              call pauses(3)   ! pause for 3 seconds then abort program.
              stop

            else

              call UsrMsg(outs,outs2,'W')

            endif

          endif 

C Assign results file names based on names recorded in the simulation
C parameter sets.  If the user is browsing then code in reslib.F will ensure 
C that it is placed in the users home folder (if there is a relative path
C included such as ../res then this will be ignored). 
C If the file user is not browsing and the simulation was started 
C in the model cfg folder then the supplied name will be used as is (e.g.
C if it has a relative path then it will be written using that relative
C path.
C If the file name includes an absolute path e.g. /tmp then this is respected
C for interactive or silent runs and for invocations from within the cfg folder
C or from another location.
          if(sblres(i)(1:2).ne.'  ')then
            RFILE=sblres(i)
          endif
          if(sflres(i)(1:2).ne.'  ')then
            LAFRES=sflres(i)
          endif
          if(splres(i)(1:2).ne.'  ')then
            pfile=splres(i)
          endif
          if(smstres(i)(1:2).ne.'  ')then
            MSTRFILE=smstres(i)
          endif
          if(ientxist.gt.0.and.(selres(i)(1:2).ne.'  '))then
            ERESLIB=selres(i)
          endif

C Debug.
C          write(6,*) ' input.F usesps rfile ',i,rfile
C          write(6,*) ' input.F usesps lafres ',i,LAFRES
C          write(6,*) ' input.F usesps pfile ',i,pfile
C          write(6,*) ' input.F usesps splres ',i,splres

          if(match)goto 43
        endif
   42 continue

C Check that we have enough info to proceede if autook
   43 if(autook) then
        if(perok.AND.tstepok.AND.saveok)then
          spfileok=.true.
        else
          write(outs,'(2a)') 
     &      'Not enough parameter information for automatic use of ',
     &      'simulator (missing periods or timesteps or save-level).'
          if(dll)then
            dllsubr='mzinpt'
            dllmesg=outs
            ier=2
            return
          else
            call usrmsg(outs,' ','W')
            call pauses(3)   ! pause for 3 seconds then abort program.
            CALL EPAGEND
            STOP
          endif
        endif
      else
        spfileok=.true.
      endif

C Debug.
C      write(6,*)'spfileok,perok,tstepok,saveok,autook,exitok,startupok'
C      write(6,*)spfileok,perok,tstepok,saveok,autook,exitok,startupok

      return
      end

C ****** Scan and report current mis-matches (similar to code in emkcfg.F).
      subroutine ckcurmatch(prob,iprob)
#include "building.h"
#include "model.h"
#include "geometry.h"

C Parameters
      logical prob  ! true if there is a problem
      integer iprob ! number of mis-matches detected

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

      logical dup,first
      logical newgeo  ! to use for testing if new/old geometry file.

      CHARACTER CXSTR*78

      newgeo=.false.  ! assume older format geometry.
      IPROB=0
      ICC=0
      prob=.false.
      first=.false.
      do 287 IZ=1,NCOMP
        call eclose(gversion(IZ),1.1,0.01,newgeo)
        if(newgeo)then
          call georead(IFIL+1,LGEOM(IZ),IZ,0,itru,IER)
        else
          call egomin(IFIL+1,LGEOM(IZ),IZ,0,0,itru,IER)
        endif
        nzsur(iz)=nsur
        do 288 IS=1,NSUR
          ICC=ICC+1
          if(IC1(ICC).ne.IZ)then

C If this is the first warning print out heading.
            if(.NOT.first)then
              CALL CONXINFO(1,0,CXSTR)
              CALL EDISP(iuout,CXSTR)
              first=.true.
            endif
            call edisp(iuout,'Current zone does not match contig.')
            CALL CONXINFO(1,ICC,CXSTR)
            CALL EDISP(iuout,CXSTR)
            prob=.true.
            IPROB=IPROB+1
          endif
          if(IE1(ICC).ne.IS)then
            if(.NOT.first)then
              CALL CONXINFO(1,0,CXSTR)
              CALL EDISP(iuout,CXSTR)
              first=.true.
            endif
            call edisp(iuout,'Current surface does not match contig.')
            CALL CONXINFO(1,ICC,CXSTR)
            CALL EDISP(iuout,CXSTR)
            prob=.true.
            IPROB=IPROB+1
          endif
  288   continue
  287 continue

C Check to see if partitions match. See if other zone/surface
C exists and what it points to.
      do 290 i=1,NCON
        if(ICT(i).eq.3)then
          if(IC2(i).gt.0.and.IC2(i).le.NCOMP.and.
     &      IE2(i).gt.0.and.IE2(i).le.NZSUR(IC2(i)))then
            ioc=IZSTOCN(IC2(i),IE2(i))
            if(ioc.ne.0)then
              if(ICT(ioc).ne.3)then
                if(.NOT.first)then
                  CALL CONXINFO(1,0,CXSTR)
                  CALL EDISP(iuout,CXSTR)
                  first=.true.
                endif
                call edisp(iuout,'One side is not linked...')
                CALL CONXINFO(1,i,CXSTR)
                CALL EDISP(iuout,CXSTR)
                CALL CONXINFO(1,ioc,CXSTR)
                CALL EDISP(iuout,CXSTR)
                prob=.true.
                IPROB=IPROB+1
               elseif(IC2(ioc).ne.IC1(i))then
                if(.NOT.first)then
                  CALL CONXINFO(1,0,CXSTR)
                  CALL EDISP(iuout,CXSTR)
                  first=.true.
                endif
                call edisp(iuout,'Contig. zone link >|< incorrect...')
                prob=.true.
                IPROB=IPROB+1
                CALL CONXINFO(1,i,CXSTR)
                CALL EDISP(iuout,CXSTR)
                CALL CONXINFO(1,ioc,CXSTR)
                CALL EDISP(iuout,CXSTR)
              elseif(IE2(ioc).ne.IE1(i))then
                if(.NOT.first)then
                  CALL CONXINFO(1,0,CXSTR)
                  CALL EDISP(iuout,CXSTR)
                  first=.true.
                endif
               call edisp(iuout,'Contig. surface link >|< incorrect...')
                prob=.true.
                IPROB=IPROB+1
                CALL CONXINFO(1,i,CXSTR)
                CALL EDISP(iuout,CXSTR)
                CALL CONXINFO(1,ioc,CXSTR)
                CALL EDISP(iuout,CXSTR)
              endif

C Check to see if this connection is only pointed to once by an
C "another zone" connection in the whole list.
              dup=.false.
              do 291 i2=1,NCON
                if(IC2(i2).eq.IC1(i).and.IE2(i2).eq.IE1(i))then
                  if(.NOT.dup)then
                    if(ICT(i2).eq.3)dup=.true.
                  else
                    if(.NOT.first)then
                      CALL CONXINFO(1,0,CXSTR)
                      CALL EDISP(iuout,CXSTR)
                      first=.true.
                    endif
                    call edisp(iuout,'Contig. 2nd reference found...')
                    CALL CONXINFO(1,i,CXSTR)
                    CALL EDISP(iuout,CXSTR)
                    CALL CONXINFO(1,ioc,CXSTR)
                    CALL EDISP(iuout,CXSTR)
                    CALL CONXINFO(1,i2,CXSTR)
                    CALL EDISP(iuout,CXSTR)
                    prob=.true.
C (not necessarily an error) IPROB=IPROB+1
                  endif
                endif
  291         continue
            endif
          else
            if(.NOT.first)then
              CALL CONXINFO(1,0,CXSTR)
              CALL EDISP(iuout,CXSTR)
              first=.true.
            endif
            call edisp(iuout,'Contig. other side links out of range...')
            CALL CONXINFO(1,i,CXSTR)
            CALL EDISP(iuout,CXSTR)
            prob=.true.
            IPROB=IPROB+1
          endif
        endif
  290 continue
      return
      end
