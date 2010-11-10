C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C This file includes:
C High level controller subroutine
C  MMGFUN   Multiple Model Generating Facility for the UK NCM. This
C           subroutine generates the stripped, notional, typical and
C           reference models. Calls lower level subroutines to run
C           simulations and extract results.

C Front end -- editing and display subroutines
C  SBMEDIT  Define high level data associated with UK SBEM methodology
C           i.e. all information required to generate *.ncm file. This
C           subroutine generates the stripped model
C  LSTNCM   Lists the current contents of the NCM description.
C  EDSBMSYS Edits the system definition within the UK NCM file.
C  EDSBMBLD Edits the building contact definition within the UK NCM file.
C  EDSBMASSOR Edits the assessor contact definition within the UK NCM file.
C  EDSBMREGS Edits the building regs definition within the UK NCM file.

C Model generation subroutines
C  EMKSTRIPPED first pass for notional model creation
C  EMKSBM   generates notional model
C  EMKREF   Generates reference model
C  EMKTYP   Generates typical model

C Simulation and results analysis subroutine
C  SIMRUN   Performs simulations of stripped, notional, typical and
C           reference buildings. results are written out as save level 6
C           text files.
C  RSL6TF   Reads save level 6 results text file to generate information
C           relevant to BRUKL and EPCGen

C Post processing and input file generation for BRE software subroutines
C  BERTER   Post processes results to generate BER, TER etc.
C  NCMCAL   generates BRUKL input file
C  EPCCAL   Generates the Energy Performance Certificate file for UK

C Miscellaneous subroutines
C  defLeakage allows the definition of the AHU and ductwork
C           leakage details.
C  ENLARGE  performs geometric enlargement (contraction) of a surface.

C ********************* MMGFUN
C MMGFUN controls all UK NCM related tasks
      SUBROUTINE MMGFUN
      
      USE AIM2_InputData, ONLY: iAIM2
      
      IMPLICIT NONE
#include "building.h"
#include "sbem.h"
#include "MultiYear_simulations.h"
#include "bc_data.h"
#include "lookup_data.h"
#include "roam.h"
#include "net_flow.h"
#include "uncertainty.h"
#include "model.h"
#include "site.h"
#include "ipvdata.h"
#include "schedule.h"

      integer lnblnk  ! function definition

      common/pophelp/h(60)
      character*72 h
      common/FILEP/IFIL
      integer ifil
      common/OUTIN/IUOUT,IUIN
      integer iuout,iuin

      COMMON/HVACINFO/IHVACFLAG,HVACFILE
      INTEGER :: ihvacflag
      CHARACTER*72 HVACFILE

      COMMON/C21/IFCFG,CFGROOT,LCFGF
      INTEGER IFCFG
      CHARACTER CFGROOT*24,LCFGF*72

      COMMON/SPFLDAT/NSSET,ISSET,ISSTUP,ISBNSTEP,ISPNSTEP,ISSAVE,ISAVGH
      COMMON/SPFLPER/ISSTDAY(MSPS),ISSTMON(MSPS),ISFNDAY(MSPS),
     &               ISFNMON(MSPS)
      COMMON/SPFLDES/SPFDESCR(MSPS)
      INTEGER nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh,ISSTDAY
      INTEGER ISSTMON,ISFNDAY,ISFNMON
      CHARACTER SPFDESCR*8

      COMMON/CFDFIL/LCFD(MCOM),IFCFD(MCOM)
      INTEGER IFCFD
      CHARACTER*72 LCFD

      COMMON/GR3D100/BLDG3D,ZONE3D(MCOM)
      LOGICAL :: BLDG3D,ZONE3D

      COMMON/MOIST01/MSTROK,MSTRZN(MCOM)
      LOGICAL MSTROK,MSTRZN

      COMMON/GRSD100/INDXST
      INTEGER INDXST

      COMMON/SHOCCcfg/bSHOCCed,SHOCCshlFile,bZoneSHOCCed(mcom),
     &             SHOCCshzFile(mcom)
      LOGICAL bSHOCCed,bZoneSHOCCed
      CHARACTER SHOCCshlFile*72,SHOCCshzFile*72

      COMMON/dhw/sDHW_InputFileName,iDHW_FLAG
      INTEGER   iDHW_FLAG
      CHARACTER sDHW_InputFileName*72


      COMMON/Fcell/iFClds,LFClds
      INTEGER iFClds
      CHARACTER LFClds*72

      COMMON/gshpinfo/igshp
      INTEGER IGSHP

      COMMON/gcepinfo/igcep
      INTEGER IGCEP

      COMMON/H2wind/LWndSupp,iWndSupp
      INTEGER iWndSupp
      CHARACTER LWndSupp*72

      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      INTEGER NCONTM,NOCNTM
      CHARACTER CONTMNAM*12

      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER ICOMP,IAIRN,ICAAS
      CHARACTER*72 LAPROB

      COMMON/C1/NCOMP,NCON
      INTEGER NCOMP,NCON

      COMMON/UA1/LUALF,LCNG(MNCNG),LLOC(MNIL)
      CHARACTER LUALF*72,LCNG*15,LLOC*15

      COMMON/PCONV/ipconv,pcnvht,pcnvcl,pcnvlt,pcnvfn,pcnvsp,pcnvhw
      INTEGER IPCONV
      REAL pcnvht,pcnvcl,pcnvlt,pcnvfn,pcnvsp,pcnvhw

      COMMON/CPCALC/icpcon,ble,bwi,bhi,blox,bloy,bloz,orient,irt,ra,
     &              sbh,pad,wvpe
      INTEGER ICPCON,IRT
      REAL ble,bwi,bhi,blox,bloy,bloz,orient,ra,sbh,pad,wvpe

      COMMON/VTHP31/INTHPS,INTHPZ(MCOM)
      LOGICAL INTHPS,INTHPZ

      COMMON/GR1D06/IGR1D
      LOGICAL IGR1D

      INTEGER MSTMC
      PARAMETER (MSTMC=20)
      COMMON/BIDIRFL/bidirfile,bidirname(MSTMC)
      character bidirfile*72,bidirname*12

      common/spmfxst/ispmxist,spflnam
      INTEGER ISPMXIST
      CHARACTER spflnam*72

      common/entfile/entflnam,ientxist
      INTEGER :: ientxist
      CHARACTER entflnam*72

      common/IPVF/lipvdatf
      CHARACTER*72 LIPVDATF

      COMMON/GRND100/GRND3D
      LOGICAL GRND3D

      COMMON/GTFIL/GTGEOM
      CHARACTER *72 GTGEOM

      COMMON/LongRad/iExtLgRadFlag,eGrdTp(12)
      INTEGER iExtLgRadFlag
      REAL eGrdTp

      common/anchora/NALOC,ALOC(20),ALOCLBL(20),ALOCTYP(20)
      INTEGER NALOC
      CHARACTER ALOC*12,ALOCLBL*12,ALOCTYP*4

      COMMON/C6/INDCFG
      INTEGER INDCFG

      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      INTEGER IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      LOGICAL OK,DOK
      integer ier,iier,itrc,IAPROB,iecmoption,lns,iw,IYEAR_O
      integer iuact  ! return state from sbmedit call
      integer IROOTLEN ! length of cfgroot
      INTEGER NGRDP_O
      character EXT*4,APE*6,MODE*4
      character LASBEM_O*72  ! to re-establish the ncm file name
      CHARACTER*72 LCNN_O


C Initialise
      OK=.TRUE.
      DOK=.TRUE.

C Change cfgroot to have only 20 characters instead of the usual 24 (to
C assist in addition of standard appendages _str, _not, _ref and _typ)
      IF(LNBLNK(CFGROOT).GT.20)WRITE(CFGROOT,'(2a)')CFGROOT(1:20),
     &'    '

C Remember original model name, configuration and connection files
C and model title.
      LCFGF_O   = LCFGF
      CFGROOT_O = CFGROOT
      LCNN_O    = LCNN
      LSNAM_O   = modeltitle
      IYEAR_O   = IYEAR
      LASBEM_O  = LASBEM   ! remember ncm file name

C Generate stripped, notional, reference and typical model
C configuration file names.
      EXT='.cfg'
      APE='_str  '
      CALL FNCNGR(LCFGF,APE,EXT,LCFGF_S)
      IROOTLEN=LNBLNK(CFGROOT)
      if(irootlen.le.19)then
        write(cfgroot_s,'(2a)')cfgroot(1:irootlen),ape(1:4)
      else
        write(cfgroot_s,'(2a)')cfgroot(1:19),ape(1:4)
      endif
      APE='_not  '
      CALL FNCNGR(LCFGF,APE,EXT,LCFGF_N)
      if(irootlen.le.19)then
        write(cfgroot_n,'(2a)')cfgroot(1:irootlen),ape(1:4)
      else
        write(cfgroot_n,'(2a)')cfgroot(1:19),ape(1:4)
      endif
      APE='_ref  '
      CALL FNCNGR(LCFGF,APE,EXT,LCFGF_R)
      if(irootlen.le.19)then
        write(cfgroot_r,'(2a)')cfgroot(1:irootlen),ape(1:4)
      else
        write(cfgroot_r,'(2a)')cfgroot(1:19),ape(1:4)
      endif
      APE='_typ  '
      CALL FNCNGR(LCFGF,APE,EXT,LCFGF_T)
      if(irootlen.le.19)then
        write(cfgroot_t,'(2a)')cfgroot(1:irootlen),ape(1:4)
      else
        write(cfgroot_t,'(2a)')cfgroot(1:19),ape(1:4)
      endif

C Remember zone specific files
      DO 1 ICOMP=1,NCOMP
        LGEOM_O(ICOMP)=LGEOM(ICOMP)

C Strip model of optional higher resolution
        IVF(ICOMP)=0                 ! viewfactors
        IHC(ICOMP)=0                 ! convective heat transfer coefficients
        LCFD(ICOMP)='UNKNOWN'        ! CFD domain flow definition
        ZONE3D(ICOMP)=.FALSE.        ! 3D heat transfer
        MSTRZN(ICOMP)=.FALSE.        ! moisture
        IndxSt=0                     ! structured mesh
        bZoneSHOCCed(ICOMP)=.FALSE.  ! SHOCC
 1    CONTINUE

C Dereference all files that are to be dereferenced and generate
C stripped model. Define the simulation parameters for the run.
      ISBEM=2    ! have description as well as SBEM db
      INOTI=4  ! mark explicitly as stripped model
      IYEAR=2008 ! match SBEM activities database version year
      H(1)='Assessment period choices allow you to do quick'
      H(2)='checks of your model for the month of January or'
      H(3)='April or July. And eventually you MUST do the'
      H(4)='annual assessment REQUIRED by the code.'
      CALL EASKABCD(' ','Initial assessment period (see help)',
     &      'January','April','July','Annual (official)',IW,4)
      if(isstup.le.0)isstup=20
      if(isbnstep.le.2)isbnstep=6   ! 1 tsph for testing 4 std 12 for 5m
      if(ispnstep.le.2)ispnstep=10
      NSSET=1
      issave=6
      isavgh=1
      if(iw.eq.1)then
        isstday(1)=1
        isstmon(1)=1
        isfnday(1)=31   ! currently set to January
        isfnmon(1)=1
      elseif(iw.eq.2)then
        isstday(1)=1
        isstmon(1)=4
        isfnday(1)=30   ! currently set to April
        isfnmon(1)=4
      elseif(iw.eq.3)then
        isstday(1)=1
        isstmon(1)=7
        isfnday(1)=31   ! currently set to July
        isfnmon(1)=7
      elseif(iw.eq.4)then
        isstday(1)=1
        isstmon(1)=1
        isfnday(1)=31   ! currently set to year
        isfnmon(1)=12    ! 1 for testing 12 std
      endif
      spfdescr(1)='stripped'

C Adapt title of model from original.
      lns=lnblnk(LSNAM_O)
      if(lns.lt.61)then
        write(modeltitle,'(2a)') LSNAM_O(1:lns),' (stripped)'
      else
        write(modeltitle,'(2a)') LSNAM_O(1:61),' (stripped)'
      endif

C Dereference optional higher resolution information
      groundreflmodel=1                          ! ground reflectivity
      bMY_climates_defined=.FALSE.      ! multi year climates
      ihvacflag=0                       ! HVAC flag
      IDHW_FLAG=0                       ! DHW flag
      iAIM2=0                           ! AIM2 flag
      iFClds=0                          ! Fuel cell
      igshp=0                           ! GSHP flag
      igcep=0                           ! GCEP flag
      iWndSupp=0                        ! wind generated electricity
      bSHOCCed=.FALSE.                  ! SHOCC
      bBC_data_defined=.FALSE.          ! boundary condition definiton
      bLookup_data_defined=.FALSE.      ! lookup data
      NOCNTM=0                          ! contaminants
      IROAM=0                           ! roaming
      LUALF='UNKNOWN'                   ! uncertainity
      ipconv=0                          ! primary energy conversions (IPV)
      bdmds='UNKNOWN'                   ! building demands
      icpcon=0                          ! CPCALC data
      lradcf='UNKNOWN'                  ! Radiance configuration *rif file
      INTHPS=.FALSE.                    ! nonlinear thermophysical properties configuration file
      IGR1D=.FALSE.                     ! building 1D node distribution
      bidirfile='UNKNOWN'               ! bidirectional optical data
      ispmxist=0                        ! special materials file
      ientxist=0                        ! electrical bus description file
      ITDFLG=0                          ! temporal data file
      LIPVDATF='UNKNOWN'                ! IPV data
      nipvassmt=0                       ! IPV data
      GRND3D=.FALSE.                    ! 3D ground information
      GTGEOM='UNKNOWN'                  ! ground topology
      NALOC=0                           ! anchor
      INDCFG=1                          ! Building only model
      IAIRN=0                           ! disconnect any air flow network
      iExtLgRadFlag=0                   ! external longwave radiation flag

C Define ground temperature flag to be 1 in order to include ground
C temperatures defined from climate file.
C Uncomment following line when ground temperature averaging is complete
      NGRDP_O=NGRDP
      NGRDP=0                           ! ground information

C Check if the model is sufficiently attributed for NCM purposes.
      call okforncm('w',ier)
      if(ier.eq.0)then
        continue
      elseif(ier.eq.1)then
        call edisp(iuout,'Model still lacks some attributions.')
        call edisp(iuout,'It will probably not run NCM cleanly.')
      elseif(ier.eq.2)then
        call edisp(iuout,'Model is old format. Correct this and')
        call edisp(iuout,'try again.')
        return
      elseif(ier.eq.3)then
        call edisp(iuout,'Zone surface attribution incomplete. ')
        call edisp(iuout,'Use geometry facilities to complete.')
      elseif(ier.eq.4)then
        call edisp(iuout,'Model geometry is overly complex. Correct')
        call edisp(iuout,'this and try again.')
        return
      elseif(ier.eq.5)then
        call edisp(iuout,'Some UK NCM descriptions missing. Go to')
        call edisp(iuout,'model context menu and complete.')
        return
      endif

C Offer choice to generate stripped model and running assessments.
  42  H(1) ='This facility takes information provided in the model'
      H(2) ='context menu NCM descriptions and will use it to'
      H(3) ='create several alternative models and optionally run'
      H(4) ='a series of assessments to confirm the performance of the'
      H(5) ='design.'
      H(6) =' '
      H(7) ='If the model is incomplete you may need to supply'
      H(8) ='additional information or return to the model context'
      H(9) ='or zone geometry menus. '
      H(10)=' '
      H(11)='1. Generate models (with standard constructions'
      H(12)='   and glazing areas) and usage patterns, run '
      H(13)='   assessments and generate EPC.'
      H(14)=' '
      H(15)='2. If you did not have time to run the assessments in'
      H(16)='   step one you can use the model variants and run the'
      H(17)='   assessments for the energy performance certificate'
      H(18)='   EPC and/or generate BRUKL specific information.'
      H(19)=' '
      H(20)='3. Scan existing results from assessments and re-generate'
      H(21)='   the NCM file (for testing purposes).'
      H(22)=' '
      H(23)='If you plan on using existing models and or existing'
      H(24)='assessments check that you have loaded your ORIGINAL'
      H(25)='model prior to asking for assessments or reports.'
      H(26)=' '
      H(27)='If you are running your assessments on a Linux or OSX or'
      H(28)='under the Cygwin emulation environment you will have to'
      H(29)='transfer the BRUKL.inp and EPCGen.inp files to a Windows'
      H(30)='computer to complete the final step. '
      H(31)=' '
      H(32)='Remember to make backup copies of your models!'
      CALL EASKABCD('Options (see help): ',' ',
     &  'generate models & make EPC','run assessments & make EPC',
     &  'use existing runs & make EPC','exit NCM',iecmoption,32)
      if(iecmoption.eq.1)then

C Generate all the model variants and run assessments and generate EPC.
        iuact = 1 ! signal generation of notional requested
        call EMKSTRIPPED(iuact,ier)
        call edisp(iuout,'Initial pass completed...')

        iuact = 2
        CFGROOT = CFGROOT_O ! re-establish the initial root name
        CALL EMKSBM(iier)   ! generate Notional model
        if(iier.eq.2.or.iier.eq.3.or.iier.eq.4) goto 42  ! Ask user what to do
        call edisp(iuout,'Notional model completed...')
        CALL EMKREF  ! Reference model
        call edisp(iuout,'Reference model completed...')
        CALL EMKTYP  ! Typical model
        call edisp(iuout,'Typical model completed...')

C Ask user if they want to run the assessments.
        H(1)='You are using new UK NCM stripped, notional,'
          H(2)='reference and typical models. In order to generate'
          H(3)='EPC and BRUKL (UK building regulations) specific'
          H(4)='information annual simulations for all these models'
          H(5)='need to run and may take some time. You may choose not'
          H(6)='to run simulations at this time and run these at a'
        H(7)='later time (via the 2nd option).'
        CALL ASKOK(
     &    'Run NCM simulations based on these models (see help)',
     &    ' ',OK,dok,7)

C Run simulation and recover information from save level 6 results file
        if(ok)then
        CALL SIMRUN(2) ! Notional model
        CALL SIMRUN(1) ! Stripped model

C To save time in formal NCM testing comment out the next two lines.
        CALL SIMRUN(3) ! Reference model
        CALL SIMRUN(4) ! Typical model

C Generate BER and TER - if doing DSM testing the call is made from
C RSL6TF
          IF(iDsmTestingFlag.LE.0.OR.iDsmTestingFlag.GE.3)then
            CALL BERTER(IER)
          ENDIF

C Generate BRUKL information
          CALL NCMCAL(IER)

C Generate EPC information
          CALL EPCCAL(IER)
        endif
        goto 42

      elseif(iecmoption.eq.2)then

C Use existing models and run assessments and generate EPC.
        iuact = 2
C        CFGROOT = CFGROOT_O ! re-establish the initial root name
C        CALL EMKSBM(iier)   ! generate Notional model
C        if(iier.eq.2.or.iier.eq.3.or.iier.eq.4) goto 42  ! Ask user what to do
C        CALL EMKREF  ! Reference model
C        CALL EMKTYP  ! Typical model

        H(1)='You are using existing UK NCM stripped, notional,'
        H(2)='reference and typical models. In order to generate'
        H(3)='EPC and BRUKL (UK building regulations) specific'
        H(4)='information annual simulations for all these models'
        H(5)='need to run and may take some time. If you picked this'
        H(6)='choice in error just say no.'
        CALL ASKOK(
     &    'Run NCM simulations based on existing models (see help)',
     &    ' ',OK,dok,6)

C Run simulation and recover information from save level 6 results file
        if(ok)then
          CALL SIMRUN(2) ! Notional model
          CALL SIMRUN(1) ! Stripped model

C To save time in formal NCM testing comment out the next two lines.
          CALL SIMRUN(3) ! Reference model
          CALL SIMRUN(4) ! Typical model

C Generate BER and TER - if doing DSM testing the call is made from 
C RSL6TF
          IF(iDsmTestingFlag.LE.0.OR.iDsmTestingFlag.GE.3)then
            CALL BERTER(IER)
          ENDIF

C Generate BRUKL information
          CALL NCMCAL(IER)

C Generate EPC information
          CALL EPCCAL(IER)
        endif
        goto 42

      elseif(iecmoption.eq.2)then

C Use existing models and run assessments and generate EPC.
        iuact = 2
C        CFGROOT = CFGROOT_O ! re-establish the initial root name
C        CALL EMKSBM(iier)   ! generate Notional model
C        if(iier.eq.2.or.iier.eq.3.or.iier.eq.4) goto 42  ! Ask user what to do
C        CALL EMKREF  ! Reference model
C        CALL EMKTYP  ! Typical model

        H(1)='You are using existing UK NCM stripped, notional,'
        H(2)='reference and typical models. In order to generate'
        H(3)='EPC and BRUKL (UK building regulations) specific'
        H(4)='information annual simulations for all these models'
        H(5)='need to run and may take some time. If you picked this'
        H(6)='choice in error just say no.'
        CALL ASKOK(
     &    'Run NCM simulations based on existing models (see help)',
     &    ' ',OK,dok,6)

C Run simulation and recover information from save level 6 results file
        if(ok)then
          CALL SIMRUN(2) ! Notional model
          CALL SIMRUN(1) ! Stripped model

C To save time in formal NCM testing comment out the next two lines.
          CALL SIMRUN(3) ! Reference model
          CALL SIMRUN(4) ! Typical model

C Generate BER and TER - if doing DSM testing the call is made from 
C RSL6TF
          IF(iDsmTestingFlag.LE.0.OR.iDsmTestingFlag.GE.3)then
            CALL BERTER(IER)
          ENDIF

C Generate BRUKL information
          CALL NCMCAL(IER)

C Generate EPC information
          CALL EPCCAL(IER)
        endif
        goto 42

      elseif(iecmoption.eq.3)then

C Recover information from save level 6 results file
C Reset cfg files and call rsl6tf.  Do the notional
C set first to make sure AER is filled.
        dok=.false.
        H(1)='You are using existing save level 6 files from'
        H(2)='stripped, notional, reference and typical model runs.'
        H(3)='In order to generate EPC and BRUKL (UK building'
        H(4)='regulations) data will be re-scanned. FOR TESTING'
        H(5)='PURPOSES.'
        CALL ASKOK(
     &    'Re-scan prior assessments to generate EPC (see help)',' ',
     &    ok,dok,5)
        if(ok)then
        LCFGF=LCFGF_N
        CALL RSL6TF(2)
        LCFGF=LCFGF_S
        CALL RSL6TF(1)
        LCFGF=LCFGF_R
        CALL RSL6TF(3)
        LCFGF=LCFGF_T
        CALL RSL6TF(4)
        LCFGF=LCFGF_O

C Generate BER and TER - if doing DSM testing the call is made from
C RSL6TF
          IF(iDsmTestingFlag.LE.0.OR.iDsmTestingFlag.GE.3)then
            CALL BERTER(IER)
          ENDIF

C Generate BRUKL information
      CALL NCMCAL(IER)

C Generate EPC information
      CALL EPCCAL(IER)
        endif
        goto 42

      elseif(iecmoption.eq.4)then

C Clear the commons, restore file names and reload the original model
C before returning.
        call clrprb
        NGRDP=NGRDP_O
        LCFGF   = LCFGF_O
        CFGROOT = CFGROOT_O
        LCNN    = LCNN_O
        IFCFG=IFIL+1
        IAPROB=IFIL+2
        MODE='ALL '
        ITRC=0
        CALL ERSYS(LCFGF,IFCFG,IAPROB,MODE,itrc,IER)
        IYEAR=IYEAR_O 	          ! re-establish simulation year
        modeltitle  = LSNAM_O     ! re-establish model title
        LASBEM = LASBEM_O    ! re-establish ncm name in original model
        INOTI=0              ! reset to original model, but
        CALL EMKCFG('-',IER) ! include ncm file in original model
      endif

      RETURN
      END

C ********************* sbmedit
C sbmedit define high level data associated with UK NCM methodology and
C write stripped model.
C iuact is returned based on the users action:
C   iuact = 0  user selected the - exit option
C   iuact = 1  user asked to generate notional
C   iuact = 2  user asked to procced to calculations
C ier is returned as non-zero if there was a problem.
      subroutine sbmedit(iuact,ier)
#include "building.h"
#include "geometry.h"
#include "sbem.h"
#include "model.h"
C #include "tdf2.h"
C #include "espriou.h"

      integer lnblnk  ! function definition

      COMMON/C24/IZSTOCN(MCOM,MS)
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      common/C21/IFCFG,cfgroot,LCFGF

      character D24*24

      DIMENSION ITEMS(33),ITEMX(15),KSYS(MHT),ITEMH(MCOM+4)
      DIMENSION KSTR(10),HSYSNM(MCOM+1),JSTR(8),LSYS(10)
      DIMENSION ITEMZ(MREN+5),LSTR(10)
      character cfgroot*24,LCFGF*72,NNAME*72
      character outs*124,h*72
      character ITEMS*50,D64*64,ITEMX*50,ITEMH*70
      CHARACTER HSYSNM*70,TMPSTR*64
      character EXT*4,APE*6

      real light_input_watt !user input for heat gains from lights
                            !instead of using the values from the sbem database
      LOGICAL OK,dok
      logical unixok,XST
      character TRYMATCH*40,ITEMZ*50
      integer mainactivity       ! the index 1-25? of the general categories of activity

C For tdf to call arlist:
      dimension itemfromactivity(MAC) ! To remember which major activity was the source
      integer itemfromactivity        ! of the item in the selection list
      dimension iselected(5) !NCM HVAC system control adjustment index
      integer iselected
      dimension Opt_NCM_CTL(5) !string describing the selected control adjustment
      character Opt_NCM_CTL*40
      integer iSelFlag
      integer IANSWER !Used for a call to EASKAB (1:yes, 2: no)
      integer iavailsyst !Temporary integer used for looping through the defined HVAC systems
      integer IPASS !to ask for a password
      integer ier,itrc,IAPROB,ISTAT,iw
      integer iuact  ! return state from sbmedit call
      integer NITEMH,IDO,nitms,INN,INND ! max items and current menu item
      integer ivalue ! for editing integers
      CHARACTER*32 SURFOTF
      LOGICAL CLOSER
      CLOSER=.TRUE.

C Change cfgroot to have only 20 characters instead of the usual 24 (to
C assist in addition of standard appendages _str, _not, _ref and _typ)
      IF(LNBLNK(CFGROOT).GT.20)WRITE(CFGROOT,'(2a)')CFGROOT(1:20),
     &'    '

C Remember original model name, configuration and model title.
      LCFGF_O   = LCFGF
      CFGROOT_O = CFGROOT
      LSNAM_O   = modeltitle

C Generate stripped, notional, reference and typical model
C configuration file names.
      EXT='.cfg'
      APE='_str  '
      CALL FNCNGR(LCFGF,APE,EXT,LCFGF_S)
      APE='_not  '
      CALL FNCNGR(LCFGF,APE,EXT,LCFGF_N)
      APE='_ref  '
      CALL FNCNGR(LCFGF,APE,EXT,LCFGF_R)
      APE='_typ  '
      CALL FNCNGR(LCFGF,APE,EXT,LCFGF_T)

C Remember zone specific files
      DO 1 ICOMP=1,NCOMP
        LGEOM_O(ICOMP)=LGEOM(ICOMP)
 1    CONTINUE

      ISBEM=1

      call isunix(unixok)

      H(1) = 'You have chose to configure you model for UK NCM based'
      H(2) = 'calculations to generate Energy Performance Certificate'
      H(3) = '(EPC_ Make sure that the original model follows the'
      H(4) = 'following assumptions: '
      H(5) = '1. Exterior surfaces that have other surfaces within them'
      H(6) = '   are called parents. Parents may have more surfaces '
      H(7) = '   within them. Only a three tier hierarchy is allowed '
      H(8) = '   i.e. only parent, child and grandchild are allowed '
      H(9) = '   e.g. a wall with a frame and a window). '
      H(10)= '  '
      H(11)= '2. Each exterior surface needs to have a usage. You will'
      H(12)= '   be asked to provide it if not already defined in the'
      H(13)= '   geometry and attributions menu.'
      H(14)= '  '
      H(15)= '   Only the following usages can be associated with '
      H(16)= '   exterior surfaces that are parents: '
      H(17)= '   - if exterior & vertical & opaque = WALL '
      H(18)= '   - if exterior & horiz UP & opaque = ROOF FLAT'
      H(19)= '   - if exterior & horiz DOWN & opaque = not allowed'
      H(20)= '   - if exterior & pitched & opaque = ROOF PITCHED'
      H(21)= '   - if facing ground & horizontal = GROUND'
      H(22)= '   - if facing another room & any orientation = PARTN'
      H(23)= '  '
      H(24)= '   Usages associated with child and grandchild surfaces'
      H(25)= '   (e.g. not walls and roofs) :'
      H(26)= '   - if exterior & horiz UP & transp = WINDOW SKYLIGHT'
      H(27)= '   - if exterior & pitched & transp = WINDOW SKYLIGHT'
      H(28)= '   - if exterior & vertical & transp = WINDOW'
      H(29)= '   - if exterior & vertical & opaque = FRAME or DOOR'
      H(30)= '   - if facing another room = FRAME or DOOR'
      H(31) = '  '
      H(32)= '3. Exterior surfaces should   | |                      '
      H(33)= '   avoid extreme L shapes.    | |  obtuse interior     '
      H(34)= '   Surfaces with lots of      | | /   angle here       '
      H(35)= '   edges can cause problems.  | |/_____                '
      H(36)= '   (e.g. ceiling of passage)  |________                '
      H(37) = '  '
      H(38)= '4. Only one child surface is allowed per parent, if more '
      H(39)= '   child surfaces are required divide a parent into two '
      H(40)= '   surfaces and make one child in each.'
      H(41)= ' '
      H(42)= 'When generating the notional model all areas for parents'
      H(43)= '& children that are not glazed are conserved. Additional'
      H(44)= 'glazing is added to all exterior surfaces as per UK NCM. '
      H(45)= '* if parent surface is not wall/roof no glazing is added'
      H(46)= '(This is exceptional and is discouraged)'
      H(47)= 'In order to calculate the required glazed area, gross  '
      H(48)= 'parent area is used (e.g. if a wall has a door in it then'
      H(49)= 'the glazed area is some % of (wall area + door area))'
      H(50)= ' '
      H(51)= 'If glazing area in original is more than required for  '
      H(52)= 'notional then the same area is retained in notional model'
      H(53)= ' '
      H(54)= '5. If you have not attributed some use (eg door) to zone'
      H(55)= '   surfaces then do so before defining a notional model.'
      H(56)= ' '
      H(57)= 'For interior surfaces it is assumed that all vertical '
      H(58)= 'surfaces are walls. Horizontal interior surfaces are '
      H(59)= 'internal roofs/ceilings. '

      CALL PHELPD('SBEM Assumptions',59,'-',0,0,IER)
      dok=.false.
      CALL ASKOK('Continue with UK NCM setup/calculations?',
     &  ' ',OK,dok,58)
      if(.not.OK)return

C Read sbem db.
      call sbempr(ier)

      ISBEM=2   ! signal that an NCM description will be available

C Check if *.ncm file exists if it does then read.
      APE='_str  '
      EXT='.ncm'

C Check length of cfgroot string. If less than 20 char then
C add on the _str, otherwise write initial 19 char of cfgroof
C and then add in the _str.
      IROOTLEN=LNBLNK(CFGROOT)
      if(irootlen.le.19)then
      write(cfgroot,'(2a)')cfgroot(1:irootlen),ape(1:4)
      else
        write(cfgroot,'(2a)')cfgroot(1:19),ape(1:4)
      endif

      CALL FNCNGR(LCFGF,APE,EXT,NNAME)
      LTF=LNBLNK(NNAME)
      write(nname,'(2a)')nname(1:ltf),ext
      LTF=LNBLNK(NNAME)
      XST=.false.
      call FINDFIL(NNAME,XST)  ! using path to cfg folder
      IF(XST)THEN

C There is an existing ncm definition file, scan it.
        WRITE(LASBEM,'(a)')nname(1:ltf)
        CALL RSBEM
        CALL EDISP(IUOUT,
     &    'An existing UK NCM file was found and scanned')

C Check if the model is sufficiently attributed for NCM purposes.
        call okforncm('w',ier)
        if(ier.eq.0)then
          continue
        elseif(ier.eq.1)then
          call edisp(iuout,'Model still lacks some attributions.')
          call edisp(iuout,'It will probably not run NCM cleanly.')
        elseif(ier.eq.2)then
          call edisp(iuout,'Model is old format. Correct this and')
          call edisp(iuout,'try again.')
	  return
        elseif(ier.eq.3)then
          call edisp(iuout,'Zone surface attribution incomplete. ')
          call edisp(iuout,'Use geometry facilities to complete.')
        elseif(ier.eq.4)then
          call edisp(iuout,'Model geometry is overly complex. Correct')
          call edisp(iuout,'this and try again.')
	  return
        elseif(ier.eq.5)then
          call edisp(iuout,'Some UK NCM items need to be updated in')
          call edisp(iuout,'this menu.')
        endif
      ELSE

C Check if the model is sufficiently attributed for NCM purposes.
        call okforncm('w',ier)
        if(ier.eq.0)then
          continue
        elseif(ier.eq.1)then
          call edisp(iuout,'Model still lacks some attributions.')
          call edisp(iuout,'It will probably not run NCM cleanly.')
        elseif(ier.eq.2)then
          call edisp(iuout,'Model is old format. Correct this and')
          call edisp(iuout,'try again.')
          WRITE(LASBEM,'(a)') '  '   ! clear ncm file name
	  return
        elseif(ier.eq.3)then
          call edisp(iuout,'Zone surface attribution incomplete. ')
          call edisp(iuout,'It will probably not run NCM cleanly.')
        elseif(ier.eq.4)then
          call edisp(iuout,'Model geometry is overly complex. Correct')
          call edisp(iuout,'this and try again.')
          WRITE(LASBEM,'(a)') '  '   ! clear ncm file name
	  return
        elseif(ier.eq.5)then
          call edisp(iuout,'Some UK NCM items need to be updated in')
          call edisp(iuout,'this menu.')
        endif

C Ask about building, owner and certifier detail. Note that
C the initial call to clrprb will have cleared pjname, baddress,
C bcity, buildingPC, ownername, ownertel, owneraddr, ownercity
C and ownerPC. It will also clear certifname, certTel, certaddress
C and certPC.
        IBSS=1
        IRGG=1
        ISTG=1
        BINF50=10.
      ENDIF

C Generate help text
 222  H(1) ='UK NCM calculations require a number of inputs from'
      H(2) ='the user. You are presented with several sections'
      H(3) ='to document the project and specify the type of building'
      H(4) ='as well as standard occupancy profiles within the'
      H(5) ='building. If you have all of the relevant information'
      H(6) ='you can complete these discriptions in the current'
      H(7) ='session. If not you can continue in later sessions.'
      H(8) =' '
      H(9) ='The building, owner and certifier information forms '
      H(10)='part of the energy certificate and is required. '
      H(11)=' '
      H(12)='You must select a buidling type from the list provided'
      H(13)='as well as select the region of the UK (there are'
      H(14)='some differences in the calculations between regions).'
      H(15)=' '
      H(16)='One or more HVAC system types need to be defined and'
      H(17)='these need to be linked to the zones in the model.'
      H(18)=' '
      H(19)='DHW use is associated with the activity assoicated with'
      H(20)='each zone. If DHW is produced separately from the HVAC'
      H(21)='system then define them. The link DHW systems option'
      H(22)='allows you to define additional DHW use beyond what is'
      H(23)='assumed in the zone activity schedules.'
      H(24)=' '
      H(25)='Each zone in the model must be assigned with one of the'
      H(26)='standard room activity profiles. Regardless of what'
      H(27)='was defined in the original model, standard profiles of'
      H(28)='occupancy, lighting and small power will be imposed along'
      H(29)='with assumptions about infiltration.'
      H(30)=' '
      H(31)='It is a GOOD IDEA to use the display current information'
      H(32)='choice to review the current contents before proceeding'
      H(33)='to the `setting up required models` stage. '
      H(34)=' '
      H(35)='Facade surfaces in the model must be attributed for their'
      H(36)='USE e.g. WINDOW DISPLAY prior to setting up the models.'
      H(37)='It is recommended that you FIRST define the information'
      H(38)='in the first three menu items (to register the model as'
      H(39)='being used for code assessments and THEN return to the'
      H(40)='zone attribution menu and define the surface USE'
      H(41)='attributes (only certain USE types are allowed). '
      H(42)=' '
      H(43)='It is a good idea to back up your model after you have'
      H(44)='setup the required models. '

C Make high level menu asking for different parts of UK NCM
      ITEMX(1) = 'Building, owner and certifier information'
      ITEMX(2) = 'Energy assessor and additional building details'
      ITEMX(3) = 'Building type / regulations information '
      ITEMX(4) = 'HVAC and systems information'
      ITEMX(5) = 'Domestic Hot Water systems (if separate)'
      ITEMX(6) = 'Link HVAC systems and zones '
      ITEMX(7) = 'Link separate DHW systems and zones '
      ITEMX(8) = 'Link standard activities and zones '
      ITEMX(9) = 'Define lighting systems'
      ITEMX(10)= 'Renewables information (optional)'
      ITEMX(11)= 'HVAC controls for EPC recommendations (optional)'
      ITEMX(12)= 'Advanced (only for DSM testing)'
      ITEMX(13)= 'Display current NCM information '
C      ITEMX(14)= 'Proceed with setting up required models '
C      ITEMX(15)= 'Run required assessments '
      IX=1
      CALL EPICKS(IX,KSTR,' ','Define UK NCM data',
     &  50,13,ITEMX,'Define UK NCM data',IER,44)
      IF(KSTR(1).EQ.0)THEN

C User did not pick anything. Save any changes in the NCM
C file and return with iuact set to zero.
        call mksbem
C        call mksbem2
        iuact = 0
        CFGROOT = CFGROOT_O  ! re-establish model root name
        call emkcfg('-',IER)
        WRITE (OUTS,'(2A)')'Writing configuration file ',LCFGF
        CALL EDISP(IUOUT,OUTS)
        return

      ELSEIF(KSTR(1).EQ.1)THEN

C Enter building, owner and verifier information
        call edsbmbld()  ! edit the contact information
        h(1)='You have been editing NCM data. If you want to save'
        h(2)='recent changes in the model configuration say yes.'
        dok=.true.
        CALL ASKOK('Update NCM descriptions (see help)',' ',OK,dok,2)
        if(ok)then
          call mksbem
          call emkcfg('-',IER) ! the cfg file holds this info
        endif
        goto 222  ! see what the user wants to do next

      ELSEIF(KSTR(1).EQ.2)THEN

C Enter energy assessor and additional building information
        call edsbmassor()
        h(1)='You have been editing NCM data. If you want to save'
        h(2)='recent changes say yes. '
        dok=.true.
        CALL ASKOK('Update NCM descriptions (see help)',' ',OK,dok,2)
        if(ok) call mksbem
        goto 222  ! see what the user wants to do next

      ELSEIF(KSTR(1).EQ.3)THEN

C Enter building regulations to be used
        call edsbmregs()
        h(1)='You have been editing NCM data. If you want to save'
        h(2)='recent changes say yes. '
        dok=.true.
        CALL ASKOK('Update NCM descriptions (see help)',' ',OK,dok,2)
        if(ok) call mksbem
        goto 222  ! see what the user wants to do next

      ELSEIF(KSTR(1).EQ.4)THEN

C Edit HVAC and system information
        call edsbmsys()
        h(1)='You have been editing NCM data. If you want to save'
        h(2)='recent changes say yes. '
        dok=.true.
        CALL ASKOK('Update NCM descriptions (see help)',' ',OK,dok,2)
        if(ok) call mksbem
        goto 222  ! see what the user wants to do next

      ELSEIF(KSTR(1).EQ.5)THEN

C Define DHW systems
 207    IDO=-1
        DO 206 IDHWSYS=1,NDHWSYS
          WRITE(ITEMH(IDHWSYS),'(A,1X,A)')CHAR(96+IDHWSYS),
     &          DHWNAME(IDHWSYS)
 206    CONTINUE
        ITEMH(NDHWSYS+1)=' ----------------------------------------'
        ITEMH(NDHWSYS+2)='+ add/delete'
        ITEMH(NDHWSYS+3)='? help'
        ITEMH(NDHWSYS+4)='- exit'
        NITEMH=NDHWSYS+4
        CALL EMENU('DHW systems defined',ITEMH,NITEMH,IDO)
        IF(IDO.EQ.NITEMH-1)THEN
          CALL PHELPD('DHW help not defined (yet)',2,'-',0,0,IER)
        ELSEIF(IDO.EQ.NITEMH-2)THEN
          JC=0
          CALL EASKABC('Choose option',' ',
     &         'add','delete','cancel',JC,2)

C Add new DHW system
          IF(JC.EQ.1)THEN
            NDHWSYS=NDHWSYS+1
            DHWNAME(NDHWSYS)=' new DHW generator '
            CALL EASKS(DHWNAME(NDHWSYS),'DHW system name',
     &        '(<30 characters)',30,D64,'DHW sys name',IER,1)
            IY=1
            KSYS(1)=0
 909        CALL EPICKS(IY,KSYS,' ','Select DHW system type',
     &        25,MDW,DHWGEN,'system types',IER,2)
            IF(KSYS(1).EQ.0)GOTO 909
            IDHWS(NDHWSYS)=KSYS(1)
            CALL EASKABC('Was the DHW generator built before 1998',
     &        ' ','yes','no','Do not know',IW,2)
            IF(IW.EQ.1.or.iw.eq.3)THEN
              HWEF(NDHWSYS)=DHWEFF(MDW,1)
            ELSE
              HWEF(NDHWSYS)=DHWEFF(MDW,2)
            ENDIF
            CALL EASKR(HWEF(NDHWSYS),
     &        'Enter system efficiency or accept default',' ',
     &        0.,'F',0.,'-',10.,'DHW gen eff ',IER,2)
            ITEMS(1)='natural gas                      '
            ITEMS(2)='LPG                              '
            ITEMS(3)='Biogas                           '
            ITEMS(4)='Oil                              '
            ITEMS(5)='Coal                             '
            ITEMS(6)='Biomass                          '
            ITEMS(7)='Waste heat                       '
            ITEMS(8)='Grid supplied electricity        '
            ITEMS(9)='                                 '
            IDA=1
            JSTR(1)=0
 910        CALL EPICKS(IDA,JSTR,
     &      ' ','Choose fuel type for DHW generator',
     &      30,8,ITEMS,'Choose fuel type for DHW generator',IER,2)
            IF(JSTR(1).EQ.0)GOTO 910

C Hold same fuel type indices as found for heating/cooling HVAC systems
            IF(JSTR(1).LE.5)THEN
              CONTINUE
            ELSEIF(JSTR(1).EQ.6)THEN
              JSTR(1)=9
            ELSEIF(JSTR(1).EQ.7)THEN
              JSTR(1)=12
            ELSEIF(JSTR(1).EQ.8)THEN
              JSTR(1)=10
            ENDIF
            IDHFL(NDHWSYS)=JSTR(1)

C Add storage and loop information (only if not instantaneous heater)
            IF(IDHWS(NDHWSYS).EQ.3.OR.IDHWS(NDHWSYS).EQ.4)THEN
              IDHWSS(NDHWSYS)=0
              DHWSPD(NDHWSYS,1)=0.0
              DHWSPD(NDHWSYS,2)=0.0
              DHWSPD(NDHWSYS,3)=0.0
              DHWSPD(NDHWSYS,4)=0.0
              DHWSPD(NDHWSYS,5)=0.0
            ELSE
              IDHWSS(NDHWSYS)=0
            CALL EASKABC('Is there a water storage system',
     &      ' ','yes','no','Do not know',IDL,2)
            IF(IDL.EQ.1)THEN
                IDHWSS(NDHWSYS)=1
              CALL EASKABC('Which of the following do you know',
     &        '(Choose volume if you know both) ',
     &        'Storage volume (lit)','Storage losses (MJ/month)',
     &        'Do not know either',IDM,2)
                DHWSPD(NDHWSYS,1)=0.0
                DHWSPD(NDHWSYS,2)=0.0
                DHWSPD(NDHWSYS,3)=0.0
                DHWSPD(NDHWSYS,4)=0.0
                DHWSPD(NDHWSYS,5)=0.0
              IF(IDM.EQ.1)THEN
                  CALL EASKR(DHWSPD(NDHWSYS,1),
     &          'Enter system volume (litres)',' ',
     &          0.,'F',1000.,'-',100.,'DHW capacity ',IER,2)
              ELSEIF(IDM.EQ.2)THEN
                  CALL EASKR(DHWSPD(NDHWSYS,2),
     &          'Enter system losses (MJ/month)',' ',
     &          0.,'F',1000000.,'-',10000.,'DHW capacity ',IER,2)
              ENDIF
              CALL EASKABC('Is there a water circulation loop',
     &        ' ','yes','no','Do not know',IDN,2)
              IF(IDN.EQ.1)THEN
                  IDHWSS(NDHWSYS)=2
                CALL EASKAB('Do you know circulation losses (W/m)',
     &          ' ','yes','no',IDO,2)
                IF(IDO.EQ.1)THEN
                    CALL EASKR(DHWSPD(NDHWSYS,3),
     &            'Enter circulation losses (W/m)',' ',
     &            0.,'F',10.,'-',50.,'DHW circ loss',IER,2)
                ENDIF
                CALL EASKAB('Do you know pump power (kW)',
     &          ' ','yes','no',IDP,2)
                IF(IDP.EQ.1)THEN
                    CALL EASKR(DHWSPD(NDHWSYS,4),
     &            'Enter pump power (kW)',' ',
     &            0.,'F',10.,'-',10.,'DHW pump power ',IER,2)
                ENDIF
                CALL EASKAB('Do you know loop length (m)',
     &          ' ','yes','no',IDQ,2)
                IF(IDQ.EQ.1)THEN
                    CALL EASKR(DHWSPD(NDHWSYS,5),
     &            'Enter loop length (m)',' ',
     &            0.,'F',10000.,'-',1000.,'DHW loop length ',IER,2)
                ENDIF
              ENDIF
            ENDIF
            ENDIF

C Delete existing system
          ELSEIF(JC.EQ.2)THEN
            IDA=1
            JSTR(1)=0
 911        CALL EPICKS(IDA,JSTR,
     &        ' ','Choose DHW generator to delete',
     &        30,8,DHWNAME,'Choose DHW generator to delete',IER,2)
            IF(JSTR(1).EQ.0)GOTO 911
            IF(JSTR(1).NE.0)THEN
              NDHWSYS=NDHWSYS-1
              DO 211 IDHWSYS=JSTR(1),NDHWSYS
                DHWNAME(IDHWSYS)=DHWNAME(IDHWSYS+1)
                HWEF(IDHWSYS)=HWEF(IDHWSYS+1)
                IDHWS(IDHWSYS)=IDHWS(IDHWSYS+1)
                IDHFL(IDHWSYS)=IDHFL(IDHWSYS+1)
 211          CONTINUE
            ENDIF
          ENDIF

C Edit existing DHW system
        ELSEIF(IDO.GE.1.AND.IDO.LE.NDHWSYS)THEN
          IDHWSYS=IDO
          CALL EASKS(DHWNAME(IDHWSYS),'DHW system name',
     &      '(<30 characters)',30,D64,'DHW sys name',IER,1)
          IY=1
          KSYS(1)=IDHWS(IDHWSYS)
 912      CALL EPICKS(IY,KSYS,' ','Select DHW system type',
     &    25,MDW,DHWGEN,'system types',IER,2)
          IF(KSYS(1).EQ.0)GOTO 912
          IDHWS(IDHWSYS)=KSYS(1)
          CALL EASKABC('Was the DHW generator built before 1998',
     &      ' ','yes','no','Do not know',IW,2)
          IF(IW.EQ.1)THEN
            HWEF(IDHWSYS)=DHWEFF(MDW,1)
          ELSE
            HWEF(IDHWSYS)=DHWEFF(MDW,2)
          ENDIF
          CALL EASKR(HWEF(IDHWSYS),
     &    'Enter DHW system efficiency or accept previous'
     &    ,' ',0.,'F',0.,'-',10.,'DHW gen eff ',IER,2)
          ITEMS(1)='natural gas                   '
          ITEMS(2)='LPG                           '
          ITEMS(3)='Biogas                        '
          ITEMS(4)='Oil                           '
          ITEMS(5)='Coal                          '
          ITEMS(6)='Biomass                       '
          ITEMS(7)='Waste heat                    '
          ITEMS(8)='Grid supplied electricity     '
          ITEMS(9)='                              '
          IDA=1
          JSTR(1)=IDHFL(IDHWSYS)
 913      CALL EPICKS(IDA,JSTR,
     &    ' ','Choose fuel type for DHW generator',
     &    30,8,ITEMS,'Choose fuel type for DHW generator',IER,2)
          IF(JSTR(1).EQ.0)GOTO 913

C Hold same fuel type indices as found for heating/cooling HVAC systems
          IF(JSTR(1).LE.5)THEN
            CONTINUE
          ELSEIF(JSTR(1).EQ.6)THEN
            JSTR(1)=9
          ELSEIF(JSTR(1).EQ.7)THEN
            JSTR(1)=12
          ELSEIF(JSTR(1).EQ.8)THEN
            JSTR(1)=10
          ENDIF
          IDHFL(IDHWSYS)=JSTR(1)

C Add storage and loop information
          IDHWSS(IDHWSYS)=0
          CALL EASKABC('Is there a water storage system',
     &    ' ','yes','no','Do not know',IDL,2)
          IF(IDL.EQ.1)THEN
            IDHWSS(IDHWSYS)=1
            CALL EASKABC('Which of the following do you know',
     &      '(Choose volume if you know both) ',
     &      'Storage volume (lit)','Storage losses (MJ/month)',
     &      'Do not know either',IDM,2)
            DHWSPD(IDHWSYS,1)=0.0
            DHWSPD(IDHWSYS,2)=0.0
            DHWSPD(IDHWSYS,3)=0.0
            DHWSPD(IDHWSYS,4)=0.0
            DHWSPD(IDHWSYS,5)=0.0
            IF(IDM.EQ.1)THEN
              CALL EASKR(DHWSPD(IDHWSYS,1),
     &        'Enter system volume (litres)'
     &        ,' ',0.,'F',1000.,'-',100.,'DHW capacity ',IER,2)
            ELSEIF(IDM.EQ.2)THEN
              CALL EASKR(DHWSPD(IDHWSYS,2),
     &        'Enter system losses (MJ/month)'
     &        ,' ',0.,'F',1000000.,'-',10000.,'DHW capacity ',IER,2)
            ENDIF
            CALL EASKABC('Is there a water circulation loop',
     &      ' ','yes','no','Do not know',IDN,2)
            IF(IDN.EQ.1)THEN
              IDHWSS(IDHWSYS)=2
              CALL EASKAB('Do you know circulation losses (W/m)',
     &        ' ','yes','no',IDO,2)
              IF(IDO.EQ.1)THEN
                CALL EASKR(DHWSPD(IDHWSYS,3),
     &          'Enter circulation losses (W/m)'
     &          ,' ',0.,'F',10.,'-',50.,'DHW circ loss',IER,2)
              ENDIF
              CALL EASKAB('Do you know pump power (kW)',
     &        ' ','yes','no',IDP,2)
              IF(IDP.EQ.1)THEN
                CALL EASKR(DHWSPD(IDHWSYS,4),
     &          'Enter pump power (kW)'
     &          ,' ',0.,'F',10.,'-',10.,'DHW pump power ',IER,2)
              ENDIF
              CALL EASKAB('Do you know loop length (m)',
     &        ' ','yes','no',IDQ,2)
              IF(IDQ.EQ.1)THEN
                CALL EASKR(DHWSPD(IDHWSYS,5),
     &          'Enter loop length (m)'
     &          ,' ',0.,'F',10000.,'-',1000.,'DHW loop length ',IER,2)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        IF(IDO.NE.NITEMH)GOTO 207

        h(1)='You have been editing NCM data. If you want to save'
        h(2)='recent changes say yes. '
        dok=.true.
        CALL ASKOK('Update NCM descriptions (see help)',' ',OK,dok,2)
        if(ok) call mksbem
        goto 222  ! see what the user wants to do next

      ELSEIF(KSTR(1).EQ.6)THEN

C Link HVAC and zones
C If no zones specified in model advise user and go back to menu
        IF(NCOMP.EQ.0)THEN
          CALL EDISP(IUOUT,'No zones have been defined in the model')
          CALL EDISP(IUOUT,'Define zones before linking with systems')
        ELSE
          call lstncm('s',iuout)
          HSYSNM(1)='Zone not serviced by any HVAC system'
          DO 401 ISNM=2,NCMSYS+1
            HSYSNM(ISNM)=HVACNAME(ISNM-1)
 401      CONTINUE
          DO 102 IZ=1,NCOMP
            IF(IHLZ(IZ).EQ.0)THEN
              TMPSTR=' No HVAC system for this zone!'
            ELSE
              WRITE(TMPSTR,'(2A)')' ',HVACNAME(IZ)
            ENDIF
            WRITE(OUTS,'(3A,I3,A)')
     &       'Zone ',zname(IZ)(1:lnzname(iz)),
     &       ' is linked to system number ',IHLZ(IZ),TMPSTR
            CALL EDISP(IUOUT,OUTS)
 102      CONTINUE
          DO 402 IZ=1,NCOMP
            IA=1
            WRITE(OUTS,'(2A)')'Choose HVAC system for zone ',
     &      zname(IZ)(1:lnzname(IZ))
            JSTR(1)=0
 914        CALL EPICKS(IA,JSTR,' ','Link zones to HVAC systems',
     &      70,NCMSYS+1,HSYSNM,OUTS(1:40),IER,2)
            IF(JSTR(1).EQ.0)GOTO 914
            IHLZ(IZ)=JSTR(1)-1
 402      CONTINUE
        ENDIF
        goto 222  ! see what the user wants to do next

      ELSEIF(KSTR(1).EQ.7)THEN

C Link DHW systems and zones
        IF(NCOMP.EQ.0)THEN
          CALL EDISP(IUOUT,'No zones have been defined in the model')
          CALL EDISP(IUOUT,'Define zones before linking with systems')
        ELSE
          call lstncm('d',iuout)
          HSYSNM(1)='Default DHW generator'
          IF(NCMSYS.GE.1)THEN
            HSYSNM(NDHWSYS+2)=
     &      'Zone serviced by HVAC system not DHW system'
          ELSE
            HSYSNM(NDHWSYS+2)=' '
          ENDIF
          DO 403 ISNM=2,NDHWSYS+1
            HSYSNM(ISNM)=DHWNAME(ISNM-1)
 403      CONTINUE
          DO 109 IZ=1,NCOMP
            IF(IDHWLZ(IZ).EQ.0)THEN
              TMPSTR=' (Default DHW generator)'
            ELSEIF(IDHWLZ(IZ).GT.0)THEN
              WRITE(TMPSTR,'(2A)')' ',DHWNAME(IDHWLZ(IZ))
            ELSEIF(IDHWLZ(IZ).LT.0)THEN
              WRITE(TMPSTR,'(2A)')' (HVAC system) ',
     &        HVACNAME(-IDHWLZ(IZ))
            ENDIF
            WRITE(OUTS,'(3A,I3,A)')
     &       'Zone ',zname(IZ),' is linked to system number ',
     &       ABS(IDHWLZ(IZ)),TMPSTR
            CALL EDISP(IUOUT,OUTS)
 109      CONTINUE
          DO 404 IZ=1,NCOMP
            IA=1
            WRITE(OUTS,'(2A)')'Choose DHW system for zone ',
     &      zname(IZ)(1:lnzname(IZ))
            JSTR(1)=0
            CALL EPICKS(IA,JSTR,' ','Link zones to DHW systems',
     &      70,NDHWSYS+2,HSYSNM,OUTS(1:44),IER,2)
            IF(JSTR(1).EQ.0)GOTO 404
            IF(JSTR(1).EQ.1)THEN
              IDHWLZ(IZ)=0
            ELSEIF(JSTR(1).EQ.NDHWSYS+2)THEN
              IF(NCMSYS.NE.0)THEN
                IA=1
                KSYS(1)=0
 916            CALL EPICKS(IA,KSYS,' ',
     &          'Link zones-HVAC systems for DHW',30,NCMSYS,HVACNAME,
     &          'Link zones-HVAC systems for DHW',IER,2)
                IF(KSYS(1).EQ.0)GOTO 916
                IDHWLZ(IZ)=-KSYS(1)
              ELSE
                IDHWLZ(IZ)=0
              ENDIF
            ELSEIF(JSTR(1).GE.2.AND.JSTR(1).LE.1+NDHWSYS)THEN
              IDHWLZ(IZ)=JSTR(1)-1
            ENDIF
          CALL EASKR(DEADLEG(IZ),
     &    'Enter dead leg length of water draw off point'
     &    ,'(average if more than one draw off points) ',0.,'F',10.,
     &    '-',10.,'DHW gen eff ',IER,2)
 404      CONTINUE
        ENDIF
        goto 222  ! see what the user wants to do next

      ELSEIF(KSTR(1).EQ.8)THEN

C Link activity types and zones. Loop through all of the activities
C which are associated with this building type and make up a list
C to present to the user. Remember the IAT index for each of the
C selections via the array
        IF(IBUSERTYP.NE.0)THEN
          call lstncm('o',iuout)  ! first list current activities
          DO 140 IZ=1,NCOMP
            ICAC=0
            DO 130 IAT=1,MAC
              IF(IATYPNDX(IAT,ibusertyp).EQ.1)THEN
                ICAC=ICAC+1
                ITEMS(ICAC)=ATYPNAME(IAT)
                itemfromactivity(ICAC)=IAT  ! remember which was the source
              ENDIF
 130        CONTINUE
            WRITE(OUTS,'(2A)')'Choose activity for zone ',
     &      zname(IZ)(1:lnzname(IZ))
            IX=1
            CALL EPICKS(IX,KSYS,' ',OUTS(1:LNBLNK(OUTS)),
     &      40,ICAC,ITEMS,OUTS(1:LNBLNK(OUTS)),IER,2)

C Save the string describing the activity into trymatch.
            mainactivity=KSYS(1)
            write(TRYMATCH,'(a)') ITEMS(mainactivity)(1:40)
            DO 150 IAT=1,MACL

C Match the selected name with the name within the selected category
              if(TRYMATCH(1:32).eq.roomactname(iat)(1:32))then

C When you find the name try to match the selected building index
C with the index of the activity inside this main category (i.e.
C the number after the matched string in the SBEM.db1 database
                if(ibusertyp.eq.bld_order_index(IAT))then

C This finds the selcted general index of the activity (i.e. 1-512)
C and can now be written in other places (e.g. ncm file for using it
C with tdf). Also set iactytyp to itemfromactivity (i.e.1-65)
                  theactivityindex(IZ)=IAT
                  IACTYTYP(IZ)=itemfromactivity(mainactivity)
                  write(outs,*)'for zone ',
     &              zname(iz)(1:lnzname(iz)),
     &              ' main activity index',IACTYTYP(IZ),
     &              ' the room activity name is:',
     &              roomactname(iat)(1:lnblnk(roomactname(iat)))
                  CALL EDISP(IUOUT,OUTS)
                  goto 151
              endif
              endif
 150        CONTINUE
 151        continue
 140      CONTINUE

          h(1)='You have been editing NCM data. If you want to save'
          h(2)='recent changes say yes. '
          dok=.true.
          CALL ASKOK('Update NCM descriptions (see help)',' ',OK,dok,2)
          if(ok) call mksbem
        ELSE
          CALL EDISP(IUOUT,'Define building type first')
        ENDIF
        goto 222  ! see what the user wants to do next

      ELSEIF(KSTR(1).EQ.9)THEN
        ISaveChanges=0

C Link lighting systems with zones
        DO 160 IZ=1,NCOMP
          CALL EDISP(IUOUT,' ')
          IF(ILIGHTUSER(IZ).EQ.1)THEN
            CALL EDISP(IUOUT,'Currently user defined W/m2 per 100 lux')
            WRITE(OUTS,'(A,F7.3)')' with a value of ',LIGHTWATTAGE(IZ)
            CALL EDISP(IUOUT,OUTS)
          ELSEIF(ILITYP(IZ).GT.0)THEN
            CALL EDISP(IUOUT,'Currently set as (pick from list):')
            CALL EDISP(IUOUT,TLIGHT(ILITYP(iz)))
          ENDIF
          ILIGHTUSER(IZ)=0
          CALL EASKABC(
     &      'Options for defining lighting heat gains for zone:',
     &      zname(IZ)(1:lnzname(IZ)),
     &      'user defined W/m2 per 100 lux','pick from list',
     &      'do not modify at this time',
     &      Iwaylight,2)
          IF(Iwaylight.EQ.1)THEN

C flag needed for the casual.F to account for lighting gains
C depending on the way they are specified by the user.
            ILIGHTUSER(IZ)=1

            ISaveChanges=1
            
C If users pick this then they have to demonstrate that this
C value is a realistic one
            light_input_watt=LIGHTWATTAGE(IZ)
            CALL EASKR(light_input_watt,
     &   'Lighting gains W/m^2 per 100 lux (assume 100% on) for zone:',
     &      zname(IZ)(1:lnzname(IZ)),0.,'F',3.75,'-',
     &      LIGHTWATTAGE(IZ),'light heat gains ',IER,2)
            LIGHTWATTAGE(IZ)=light_input_watt
          ELSEIF(Iwaylight.EQ.2)THEN
            WRITE(OUTS,'(2A)')'Choose lighting for zone ',
     &      zname(IZ)(1:lnzname(IZ))
            CALL EPICKS(IY,LSYS,' ',OUTS(1:LNBLNK(OUTS)),
     &        70,MILS,TLIGHT,OUTS(1:LNBLNK(OUTS)),IER,2)
            ILITYP(IZ)=LSYS(1)
            ISaveChanges=1
          ELSEIF(Iwaylight.EQ.3)THEN
            continue
          ENDIF

C Now choose lighting control -- Note this is different from standard
C lighting control and is much more simplistic in that it just
C multiplies annual lighting power with coefficients representing
C control types
          if(Iwaylight.ne.3)then
            IF(Ilightcontrol(iz).EQ.1)THEN
              OUTS='Currently control is local manual switching'
            ELSEIF(Ilightcontrol(iz).EQ.2)THEN
              OUTS='Currently control is photo-electric'
            ELSEIF(Ilightcontrol(iz).EQ.3)THEN
              OUTS='Currently control is both'
            ELSE
              OUTS='Currently there is no control'
            ENDIF
            CALL EDISP(IUOUT,OUTS)
            CALL EASKABCD('Choose lighting control type for',
     &      zname(IZ)(1:lnzname(IZ)),
     &      'Local manual switching ','Photoelectric control',
     &      'Both','No control',Ilightcontrol(iz),2)

C If photoelectric control then ask whether switching or dimming
            if(Ilightcontrol(iz).eq.2.or.Ilightcontrol(iz).eq.3)then
              IF(Ipe_control(iz).EQ.1)THEN
                OUTS='Currently there is photo-electric switching'
              elseif(Ipe_control(iz).EQ.2)THEN
                OUTS='Currently there is photo-electric dimming'
              else
                OUTS='Currently there is neither'
              endif
              call edisp(iuout,outs)

              Ipe_control(iz)=0
              CALL EASKAB('Type of photoelectric control in zone',
     &        zname(IZ)(1:lnzname(IZ)),
     &        'Switching','Dimming',Ipe_control(iz),2)
              Ipe_sensor=0
              CALL EASKAB('Type of photoelectric control sensor',
     &        zname(IZ)(1:lnzname(IZ)),
     &        'Standalone','Addresable',Ipe_sensor,2)

C Confirm front sensor or front and back sensor.
              h(1)='Photoelectric controls are based on one sensor at'
              h(2)='the front of the room or front and back sensors '
              h(3)='in the room. '
              IF(Idaylightzoning(iz).EQ.1)THEN
                outs='Currently there is a front of room sensor'
              ELSEIF(Idaylightzoning(iz).EQ.2)THEN
                outs='Currently there are front and back sensors'
              ELSE
                outs='Currently there is no sensor location defined'
              ENDIF
              CALL EDISP(IUOUT,OUTS)
              Idaylightzoning(iz)=0   ! reset prior to edit
              ivalue=0
              CALL EASKABC('Choose lighting sensor location for',
     &          zname(IZ)(1:lnzname(IZ)),
     &          'Front of room','Front and back of room',
     &          'continue',ivalue,3)
              if(ivalue.eq.1.or.ivalue.eq.2)then
                Idaylightzoning(iz)=ivalue
              endif

C Confirm photoelectric sensor parasitic power W/m2
              write(outs,'(A,F5.3,A)')'Current parasitic power is ',
     &        PE_sensor_PP(iz),'W/m2.'
              call edisp(iuout,outs)
              IF(Ipe_sensor.EQ.1)THEN
                PE_sensor_PP(iz)=0.3
              ELSEIF(Ipe_sensor.EQ.2)THEN
                PE_sensor_PP(iz)=0.57
              ENDIF
              h(1)='If you selected a standalone sensor then parasitic'
              h(2)='power is assumed to be 0.3W/m2. If the sensor is '
              h(3)='addressable (advanced) the PP is assumed to be 0.57'
              h(4)='W/m2. If you have other data please supply it now. '
              CALL EASKR(PE_sensor_PP(iz),
     &        'Confirm photo-electric sensor parasitic power',
     &        ' ',0.,'-',0.,'-',10.,'sensor parasitic power',IER,4)
            endif

C Ask for occupancy sensing
            CALL EDISP(IUOUT,
     &      'MAN-ON-OFF+EXT = Manual ON/OFF switching with additional')
            CALL EDISP(IUOUT,
     &      'automatic sweeping extinction signal. The other options')
            CALL EDISP(IUOUT,
     &      'are explained in the context help message.')
            IF(IOcc_sensing(iz).EQ.1)THEN
              OUTS='Currently MAN-ON-OFF+EXTinction signal'
            ELSEIF(IOcc_sensing(iz).EQ.2)THEN
              OUTS='Currently AUTO-ON-DIMMED'
            ELSEIF(IOcc_sensing(iz).EQ.3)THEN
              OUTS='Currently AUTO-ON-OFF'
            ELSEIF(IOcc_sensing(iz).EQ.4)THEN
              OUTS='Currently MAN-ON-DIMMED'
            ELSEIF(IOcc_sensing(iz).EQ.5)THEN
              OUTS='Currently MAN-ON-AUTO-OFF'
            ELSE
              OUTS='Currently NONE'
            ENDIF
            CALL EDISP(IUOUT,OUTS)
            h(1)='This is equivalent to the choices offered by the UK'
            h(2)='iSBEM application which is minimally documented in'
            h(3)='the UK SBEM technical manual table 20. Your choice'
            h(4)='will set an occupancy modifier which is used in '
            h(5)='the control.  '
            h(6)=' '
            h(7)='For systems without automatic presence or absence '
            h(8)='detection the following options are allowed: '
            h(9) =' Manual on/off switching  Foc=1.0 '
            h(10)=' Manual on/off switching with additional automatic'
            h(11)='   sweeping extinction signal  Foc=0.95'
            h(12)=' '
            h(13)='For systems with automatic presence and/or abscence'
            h(14)='detection the following options are allowed: '
            h(15)=' '
            h(16)=' Auto On with dimming on absence   Foc=0.95'
            h(17)=' Auto On with off on absence       Foc=0.90'
            h(18)=' Manual On with dimming on absence Foc=0.90'
            h(19)=' Manual On with off on absence     Foc=0.82'
            CALL EASKATOG(
     &      'Is there occupancy sensing in zone',
     &      zname(iz)(1:lnzname(IZ)),'MAN-ON-OFF+EXT','AUTO-ON-DIMMED',
     &      'AUTO-ON-OFF','MAN-ON-DIMMED','MAN-ON-AUTO-OFF','NONE',
     &      ' ',IOcc_sensing(iz),19)

C Confirm occupancy sensor parasitic power W/m2
            if(IOcc_sensing(iz).ne.6)then
              write(outs,'(A,F5.3)')'Current parasitic power is ',
     &        Occ_sensor_PP(iz)
              call edisp(iuout,outs)
              Occ_sensor_PP(iz)=0.3
              CALL EASKR(Occ_sensor_PP(iz),
     &        'Confirm occupancy sensor parasitic power',
     &        ' ',0.,'-',0.,'-',10.,'sensor parasitic power',IER,2)
            endif
          endif

C Generate daylight factors (approximate value)
C Figure out vertical and horizontal glazed surface area and also total
C surface area
          VERT_GLZ=0.0
          HORZ_GLZ=0.0
          DO 20 ISURF=1,NZSUR(IZ)
            ICC=IZSTOCN(IZ,ISURF)       
            SURFOTF=SSOTF(ICC)
            IF(SURFOTF(1:4).NE.'OPAQ')THEN
              IF(SSVFC(icc)(1:4).eq.'CEIL')THEN
                HORZ_GLZ=HORZ_GLZ+SSNA(ICC)
              ELSE
                VERT_GLZ=VERT_GLZ+SSNA(ICC)
              ENDIF
            ENDIF
 20       CONTINUE
          CALL ECLOSE(0.,zonetotsurfacearea(IZ),0.000001,CLOSER)
          IF(.NOT.CLOSER)
     &    DF=(45.*VERT_GLZ+90.*HORZ_GLZ)/zonetotsurfacearea(IZ)
C Debug.
C          write(6,*)vert_glz,horz_glz,zonetotsurfacearea(IZ)
          DFFront(IZ)=1.75*DF
          DFRear(IZ) =0.25*DF
 160    CONTINUE
        if(ISaveChanges.ne.0)then
          h(1)='You have been editing NCM data. If you want to save'
          h(2)='recent changes say yes. '
          dok=.true.
          CALL ASKOK('Update NCM descriptions (see help)',' ',OK,dok,2)
          if(ok) call mksbem
        endif
        goto 222  ! see what the user wants to do next

      ELSEIF(KSTR(1).EQ.10)THEN

C Renewables
C RENDATA holds supplementary information about renewable systems
C where IREN is index of renewable energy system
C NRENTYPE(IREN) = system type which is
C                1 is solar thermal collector (only used to feed DHW)
C                2 is solar photovoltaic collector
C                3 is wind turbine
C                4 is CHP
C RENDATA for solar thermal collectors is
C                1 = Index no of DHW system this is linked to
C                2 = area m2
C                3 = orientation (deg from North)
C                4 = inclination (90deg=vertical)
C RENDATA for solar photovoltaic collector is
C                1 = conversion efficiency (depending on PV type)
C                2 = area m2
C                3 = orientation (deg from North)
C                4 = inclination (90deg=vertical)
C RENDATA for wind turbine is
C                1 = terrain type index
C                2 = diameter of turbine
C                3 = hub height
C                4 = rated power
C RENDATA for CHP is
C                1 = thermal seasonal efficiency
C                2 = % of building space heating demand supplied
C                3 = % of DHW supplied
C                4 = CHP heat to power ratio
C                5 = fuel type index
        H(1) ='You can add renewable energy in this menu'
        H(2) =' '
        H(3) =' '
        H(4) =' '
        H(5) =' '
        H(6) =' '
 1584   INN=-1
        IADD=0
        IDEL=0
        IED=0
        DO 768 IREN=1,NREN
          ITEMZ(IREN)=RENNAME(IREN)
 768    CONTINUE
        ITEMZ(NREN+1) = '  -----------------------'
        ITEMZ(NREN+2) = '@ display information'
        ITEMZ(NREN+3) = '+ add/delete'
        ITEMZ(NREN+4) = '? help'
        ITEMZ(NREN+5) = '- exit'
        nitms=NREN+5
        CALL EMENU('Renewables',ITEMZ,nitms,INN)
        IF(INN.LE.NREN.AND.INN.GE.1)THEN
          dok=.false.
          CALL ASKOK('Edit this component?',' ',OK,dok,6)
          IF(OK)THEN
            IED=INN
          ENDIF
        ELSEIF(INN.EQ.NREN+1)THEN
          CONTINUE
        ELSEIF(INN.EQ.NREN+2)THEN
          CALL EDISP(IUOUT,'Information display not available yet')
        ELSEIF(INN.EQ.NREN+3)THEN
          CALL EASKABC('Choose action',' ','add','delete','cancel',IA,6)
          IF(IA.EQ.1)THEN
            IADD=1
          ELSEIF(IA.EQ.2)THEN
            IDEL=1
          ENDIF
        ELSEIF(INN.EQ.NREN+4)THEN
          CALL PHELPD('Renewables help',6,'-',0,0,IER)
        ENDIF

C Delete renewable systems
        IF(IDEL.EQ.1)THEN
 7645     INND=-2
          NITMS=NREN+1
          ITEMZ(NREN+1) = '- cancel and exit'
          CALL EMENU('Which to delete',ITEMZ,NITMS,INND)
          IF(INND.GE.1.AND.INND.LE.NREN)THEN
            NREN=NREN-1
            DO 769 IREN=INND,NREN
              RENNAME(IREN)=RENNAME(IREN+1)
              DO 771 IDN=1,6
                RENDATA(IREN,IDN)=RENDATA(IREN+1,IDN)
 771          CONTINUE
 769        CONTINUE
          ELSEIF(INND.NE.NITMS)THEN
            GOTO 7645
          ENDIF

C Add/edit renewable systems
        ELSEIF(IADD.EQ.1.OR.IED.GT.0)THEN
          IF(IADD.EQ.1)THEN
            INDEX=NREN+1
            NREN=NREN+1
            RENNAME(INDEX)=' '
            D24=' '
            CALL EASKS(RENNAME(INDEX),'Name of renewable energy system',
     &        '(<24 characters)',24,D24,'RENNAME',IER,6)
          ELSEIF(IED.GT.0)THEN
            INDEX=IED
            D24=RENNAME(INDEX)
            CALL EASKS(RENNAME(INDEX),'Name of renewable energy system',
     &        '(<24 characters)',24,D24,'RENNAME',IER,6)
          ENDIF
          IDNO=0 ; IB=0
          call MENUATOL('Choose renewable energy system',
     &    'Choose renewable energy system',
     &    'a solar thermal (for DHW)','b solar PV',
     &    'c wind turbine','d combined heat and power',' ',' ',
     &    ' ',' ',' ',' ',' ',' ',IB,idno,6)
          IF(IB.EQ.1)THEN

C Solar thermal collectors
            NRENTYPE(INDEX)=1
            IF(NDHWSYS.LE.0)THEN
              CALL EDISP(IUOUT,' ')
              CALL EDISP(IUOUT,
     &        'You do not have a DHW system in the model')
              CALL EDISP(IUOUT,
     &        'Define one before defining solar thermal collection')
              CALL EDISP(IUOUT,
     &        'otherwise DHW energy savings will be added to space')
              CALL EDISP(IUOUT,
     &        'heating system number 1')
            ELSEIF(NDHWSYS.EQ.1)THEN ! No need to ask which one the collector is linked to
              RENDATA(INDEX,1)=1.0
            ELSE
              IX=1
              CALL EPICKS(IX,LSTR,'Choose which DHW system ',
     &          'to link with this thermal collector',
     &          30,NDHWSYS,DHWNAME,'Choose DHW system',IER,6)
              RENDATA(INDEX,1)=REAL(LSTR(1))
            ENDIF
            CALL EASKR(RENDATA(INDEX,2),
     &        'Enter area in m2 of collector',
     &        ' ',0.,'-',0.,'-',10.,'collector area ',IER,6)
            CALL EASKR(RENDATA(INDEX,3),
     &        'Enter orientation of collector (degrees from North)',
     &        ' ',0.,'-',0.,'-',360.,'collector orientation ',IER,6)
            CALL EASKR(RENDATA(INDEX,4),
     &        'Enter inclination of collector (90deg = vertical)',
     &        ' ',0.,'-',0.,'-',90.,'collector inclination ',IER,6)
          ELSEIF(IB.EQ.2)THEN

C Solar PV collectors
            NRENTYPE(INDEX)=2
            IE=0 ; IENO=0
            call MENUATOL('Choose PV type','Choose PV type',
     &      'a mono-crystalline silicon','b poly-crystalline silicon',
     &      'c amorphous silicon','d other thin films',' ',' ',
     &      ' ',' ',' ',' ',' ',' ',IE,ieno,6)

C Hold PV efficiency from table 26, page 80, SBEM technical manual
C version 1 of 10-dec-07
            IF(IE.EQ.1)THEN
              RENDATA(INDEX,1)=0.15
            ELSEIF(IE.EQ.2)THEN
              RENDATA(INDEX,1)=0.12
            ELSEIF(IE.EQ.3)THEN
              RENDATA(INDEX,1)=0.06
            ELSEIF(IE.EQ.4)THEN
              RENDATA(INDEX,1)=0.08
            ENDIF
            CALL EASKR(RENDATA(INDEX,2),
     &        'Enter area in m2 of collector',
     &        ' ',0.,'-',0.,'-',10.,'collector area ',IER,6)
            CALL EASKR(RENDATA(INDEX,3),
     &        'Enter orientation of collector (degrees from North)',
     &        ' ',0.,'-',0.,'-',360.,'collector orientation ',IER,6)
            CALL EASKR(RENDATA(INDEX,4),
     &        'Enter inclination of collector (90deg = vertical)',
     &        ' ',0.,'-',0.,'-',90.,'collector inclination ',IER,6)
          ELSEIF(IB.EQ.3)THEN

C Wind turbines
            NRENTYPE(INDEX)=3
            CALL EDISP(IUOUT,'SBEM terrain types for wind turbines:')
            CALL EDISP(IUOUT,'Type 1 is open flat country')
            CALL EDISP(IUOUT,'Type 2 is farm land with boundary hedges')
            CALL EDISP(IUOUT,' occasional small farm structures, ')
            CALL EDISP(IUOUT,' houses or trees')
            CALL EDISP(IUOUT,'Type 3 is suburban, industrial areas and')
            CALL EDISP(IUOUT,' permanent forests')
            CALL EDISP(IUOUT,'Type 4 is urban areas in which at least')
            CALL EDISP(IUOUT,' 15% of surface is covered by buildings')
            CALL EDISP(IUOUT,' of average height exceeding 15m')
            CALL EDISP(IUOUT,' (CIBSE,2002)')
            CALL EDISP(IUOUT,' ')
            IE=0 ; IENO=0
            call MENUATOL('Choose terrain type',
     &      'Choose terrain (see text feed back area) ',
     &      'a type 1 (open)','b type 2 (farm land)',
     &      'c type 3 (suburban etc)','d type 4 (urban)',' ',' ',
     &      ' ',' ',' ',' ',' ',' ',IE,ieno,6)
            RENDATA(INDEX,1)=REAL(IE)
            CALL EASKR(RENDATA(INDEX,2),
     &        'Enter diameter of turbine (m)',' ',
     &        0.,'-',0.,'-',10.,'turbine diameter',IER,6)
            CALL EASKR(RENDATA(INDEX,3),
     &        'Enter hub height (m)',' ',
     &        0.,'-',0.,'-',20.,'hub height',IER,6)
            CALL EASKR(RENDATA(INDEX,4),
     &        'Enter rated power (kW)',' ',
     &        0.,'-',0.,'-',500.,'rated power',IER,6)
          ELSEIF(IB.EQ.4)THEN
            NRENTYPE(INDEX)=4
            CALL EASKR(RENDATA(INDEX,1),
     &        'Enter thermal seasonal efficiency of CHP unit (-)',
     &        ' ',0.,'-',0.,'-',1.,'efficiency',IER,6)
            CALL EASKR(RENDATA(INDEX,2),
     &        'Enter % of building space heating demand ',
     &        'supplied by CHP generator ',0.,'-',0.,'-',100.,
     &        '% bldg demand ',IER,6)
            CALL EASKR(RENDATA(INDEX,3),
     &        'Enter % of building DHW demand ',
     &        'supplied by CHP generator ',0.,'-',0.,'-',100.,
     &        '% DHW demand ',IER,6)
            CALL EASKR(RENDATA(INDEX,4),
     &        'Enter CHP heat to power ratio (-) ',
     &        ' ',0.,'-',0.,'-',1.,'heat to power ratio ',IER,6)
            ITEMS(1)='natural gas                   '
            ITEMS(2)='LPG                           '
            ITEMS(3)='Biogas                        '
            ITEMS(4)='Oil                           '
            ITEMS(5)='Coal                          '
            ITEMS(6)='Anthracite                    '
            ITEMS(7)='Smokeless Fuel                '
            ITEMS(8)='Dual Fuel Appliances          '
            ITEMS(9)='Biomass                       '
            ITEMS(10)='Waste heat                    '
            ITEMS(11)='                              '
            IDA=1
            JSTR(1)=0
 993        CALL EPICKS(IDA,JSTR,
     &      ' ','Choose fuel type for CHP generator',
     &      30,11,ITEMS,'Choose fuel type for CHP generator',IER,2)
            IF(JSTR(1).EQ.0)GOTO 993

C Synchronise with fuel types as defined in SBEM.db1
            IF(JSTR(1).EQ.10)JSTR(1)=12
            RENDATA(INDEX,5)=real(JSTR(1))
          ENDIF
        ENDIF
        IF(INN.NE.nitms)GOTO 1584
        goto 222  ! see what the user wants to do next

      ELSEIF(KSTR(1).EQ.11)THEN

C Loop for the different available HVAC systems
        do 352 iavailsyst=1,NCMSYS
          H(1)='Are there any controls for the specific HVAC system?'
          H(2)='This is only used in the recommendations report for'
          H(3)='EPC.'
          CALL EASKAB('Define controls for system: ',
     &      HVACNAME(iavailsyst),'yes','no',IANSWER,3)
          if(IANSWER.eq.1)then

C Ask for systems controls
            Opt_NCM_CTL(1)='Central time control                    '
            Opt_NCM_CTL(2)='Optimum start/stop control              '
            Opt_NCM_CTL(3)='Local Time Control (i.e. room by room)  '
            Opt_NCM_CTL(4)='Local Temperature Control (room by room)'
            Opt_NCM_CTL(5)='Weather Compensation Control            '
            H(1)='Select systems controls that apply to the specific'
            H(2)='HVAC system. These are based on the HVAC Non-Domestic'
            H(3)='guide. They are only used for the Energy Performance'
            H(4)='Recommendations report and they are not currently '
            H(5)='affecting the calculated values for the energy loads.'
            H(6)='This is in accordance with iSBEM vers. 3.2b '
            IPICKS=5
            CALL EPICKS(IPICKS,iselected,' ',
     &       ' Controls (used only for EPC recommendations)?',40,5,
     &       Opt_NCM_CTL,'controls list (see help)',IER,6)
            do 351 i=1,IPICKS
              iSelFlag = iselected(i)

C << Need to initialise iRecControls
              if(iSelFlag.gt.0)then
                if(iSelFlag.eq.1)then
                  iRecControls(1,iavailsyst)=1
                elseif(iSelFlag.eq.2)then
                  iRecControls(2,iavailsyst)=1
                elseif(iSelFlag.eq.3)then
                  iRecControls(3,iavailsyst)=1
                elseif(iSelFlag.eq.4)then
                  iRecControls(4,iavailsyst)=1
                elseif(iSelFlag.eq.5)then
                  iRecControls(5,iavailsyst)=1
              endif
              endif
 351        continue
          endif
 352    continue
        goto 222  ! see what the user wants to do next

      ELSEIF(KSTR(1).EQ.12)THEN

C Configure stripped model to use the same assumptions for overheating
C and for calculating Auxiliary energy as the assumptions for the notional building.
        H(1)='This option is password protected because it is'
        H(2)='only meant to be used for testing DSM against'
        H(3)='requirements set accrediting the software. They'
        H(4)='do not have any practical use. The differences'
        H(5)='when this flag is set are related with overheating'
        H(6)='and the calculation of the auxiliary energy.'
        call easki(IPASS,' ','Password?',
     &                            0,'-',0,'-',0,'password',ieri,6)
        if(IPASS.eq.101) then
          call tstamp('>','PRJ: set advanced flag for DSM testing')

          H(1)='default 0: stripped does NOT follow the rules of '
          H(2)='the notional/reference building.'
          H(3)='if equal to 1: stripped follows the rules of the '
          H(4)='notional building. '
          H(5)='if equal to 2: stripped follows the rules of the '
          H(6)='reference building. '
          call easki(iDsmTestingFlag,' ','DSM tests flag (see help)?',
     &      0,'-',0,'-',iDsmTestingFlag,'flag',ieri,6)
        endif
        goto 222  ! see what the user wants to do next

      ELSEIF(KSTR(1).EQ.13)THEN

C Display information
        call lstncm('p',iuout)
        call edisp(iuout,'  ')
        call lstncm('e',iuout)
        call edisp(iuout,'  ')
        call lstncm('b',iuout)
        call edisp(iuout,'  ')
        call lstncm('s',iuout)
        call edisp(iuout,'  ')
        call lstncm('d',iuout)
        call edisp(iuout,'  ')
        call lstncm('o',iuout)
        call edisp(iuout,'  ')
        call lstncm('l',iuout)
        call edisp(iuout,'  ')
        goto 222  ! see what the user wants to do next

      ENDIF
          return

      end

C ********************* LSTNCM
C LSTNCM lists the current contents of the NCM description.
C Passed act (1 char) to define what to list:
C  act = 'a' lists all
C  act = 'b' building type
C  act = 'd' DHW information
C  act = 'e' energy assessor information
C  act = 'p' project level information
C  act = 's' system information
C  act = 'o' occupancy information
C  act = 'l' lighting information
      SUBROUTINE LSTNCM(act,itu)
#include "building.h"
#include "geometry.h"
#include "sbem.h"

      integer lnblnk  ! function definition

      integer ncomp,ncon
      common/c1/ncomp,ncon

      character act*1,outs*124,outs248*248
      character TMPSTR*64
      integer lnb,lnbb  ! actual lengths of strings
      integer itu       ! unit to write to

C Re-establish glazing fractions based on UK NCM guidelines
      if(ibusertyp.ne.0)then
        ibtyp = IBTYPNDX(ibusertyp)
        if(IBTYP.EQ.1)then
          ROOFFR=0.2
          WALLFR=0.3
        elseif(IBTYP.EQ.2)then
          ROOFFR=0.2
          WALLFR=0.4
        elseif(IBTYP.EQ.3)then
          ROOFFR=0.2
          WALLFR=0.15
        endif
        rooffrfr=0.3
        wallfrfr=0.1
      endif

C Depending on the topic generate a string and display.
      if(act.eq.'a'.or.act.eq.'p')then
        write(outs,'(2a)')'Project name: ',pjname(1:lnblnk(pjname))
        call edisp(itu,outs)
        write(outs,'(2a)')'Building address: ',
     &    baddress(1:lnblnk(baddress))
        call edisp(itu,outs)
        write(outs,'(2a)')'Building city: ',bcity(1:lnblnk(bcity))
        call edisp(itu,outs)
        write(outs,'(2a)')'Building Postcode: ',
     &    buildingPC(1:lnblnk(buildingPC))
        call edisp(itu,outs)
        write(outs,'(2a)')'Building owner name: ',
     &    ownername(1:lnblnk(ownername))
        call edisp(itu,outs)
        write(outs,'(2a)')'Building owner telephone: ',
     &    ownertel(1:lnblnk(ownertel))
        call edisp(itu,outs)
        write(outs,'(2a)')'Building owner address: ',
     &    owneraddr(1:lnblnk(owneraddr))
        call edisp(itu,outs)
        write(outs,'(2a)')'Building owner city: ',
     &    ownercity(1:lnblnk(ownercity))
        call edisp(itu,outs)
        write(outs,'(2a)')'Building owner Postcode: ',
     &    ownerPC(1:lnblnk(ownerPC))
        call edisp(itu,outs)
        write(outs,'(2a)')'Certifier name: ',
     &    certifname(1:lnblnk(certifname))
        call edisp(itu,outs)
        write(outs,'(2a)')'Certifier telephone: ',
     &    certTel(1:lnblnk(certTel))
        call edisp(itu,outs)
        write(outs,'(2a)')'Certifier address: ',
     &    certaddress(1:lnblnk(certaddress))
        call edisp(itu,outs)
        write(outs,'(2a)')'Certifier city: ',
     &    certcity(1:lnblnk(certcity))
        call edisp(itu,outs)
        write(outs,'(2a)')'Certifier postcode: ',
     &    certPC(1:lnblnk(certPC))
        call edisp(itu,outs)
      elseif(act.eq.'a'.or.act.eq.'e')then
        call edisp(itu,
     &  'Energy Assessor and additional building details')
        write(outs,'(2a)') 'Unique Property Reference Number (UPRN): ',
     &    UPRN(1:lnblnk(UPRN))
        call edisp(itu,outs)
        write(outs,'(A,I2,A,I2,A,I4)') 'Inspection date: ',
     &    D_inspect,'  ',M_inspect,'  ',Y_inspect
        call edisp(itu,outs)
        write(outs,'(2a)') 'Accreditation Scheme (E,W and NI only): ',
     &    Accr_Scheme(1:lnblnk(Accr_Scheme))
        call edisp(itu,outs)
        write(outs,'(2a)')
     &    'Assessor registration number (E,W and NI only):',
     &     assessRegNumber(1:lnblnk(assessRegNumber))
        call edisp(itu,outs)
        write(outs,'(2a)')
     &    'Employer/trading name of energy assessor: ',
     &    empl_Trading_name(1:lnblnk(empl_Trading_name))
        call edisp(itu,outs)
        write(outs,'(2a)')
     &    'Employer/trading address of energy assessor: ',
     &    addr_empl_Trading(1:lnblnk(addr_empl_Trading))
        call edisp(itu,outs)
        write(outs,'(2a)') 'Related party disclosure: ',
     &    party_disclosure(1:lnblnk(party_disclosure))
        call edisp(itu,outs)
        write(outs,'(2a)') 'Qualifications of energy assessor:',
     &    qualifications_assessor(1:lnblnk(qualifications_assessor))
        call edisp(itu,outs)
        write(outs,'(2a)') 'Insurance company: ',
     &    insurer_Company(1:lnblnk(insurer_Company))
        call edisp(itu,outs)
        write(outs,'(2a)') 'Insurance policy number: ',
     &    policyNumberInsurance(1:lnblnk(policyNumberInsurance))
        call edisp(itu,outs)
        write(outs,'(A,I2,A,I2,A,I4)') 'Insurance policy start date: ',
     &    S_Dinsur,'  ',S_Minsur,'  ',S_Yinsur
        call edisp(itu,outs)
        write(outs,'(A,I2,A,I2,A,I4)')'Insurance policy expiry date: ',
     &    E_Dinsur,'  ',E_Minsur,'  ',E_Yinsur
        call edisp(itu,outs)
        write(outs,'(A,I9)') 'Insurance policy cover limit: ',
     &    pi_limit
        call edisp(itu,outs)
        write(outs,'(2a)') 'Complexity level of the project: ',
     &    pjLevComplexity(1:lnblnk(pjLevComplexity))
        call edisp(itu,outs)
      elseif(act.eq.'a'.or.act.eq.'b')then
        if(ibusertyp.ne.0)then
          write(outs,'(a,i2,1x,2a)') 'The current building type is ',
     &      ibusertyp,BTYPNAME(ibusertyp),'.'
          call edisp(itu,outs)
          ibtyp = IBTYPNDX(ibusertyp)
          if(ibtyp.eq.1)then
            write(outs248,'(2a,f6.2,a,f6.2,a)')
     &      'The NCM guide reference is a residenatial ',
     &      'building with roof glazing fraction ',ROOFFR,
     &      ' and a wall glazing fraction ',WALLFR,'.'
            call edisp248(itu,outs248,90)
          elseif(ibtyp.eq.2)then
            write(outs248,'(2a,f6.2,a,f6.2,a)')
     &      'The NCM guide table 4 reference is an office, shop or ',
     &      'place of assembly with roof glazing fraction ',ROOFFR,
     &      ' and a wall glazing fraction ',WALLFR,'.'
            call edisp248(itu,outs248,90)
          elseif(ibtyp.eq.3)then
            write(outs248,'(2a,f6.2,a,f6.2,a)')
     &      'The NCM guide table 4 reference is an industrial or ',
     &      'storage building with roof glazing fraction ',ROOFFR,
     &      ' and a wall glazing fraction ',WALLFR,'.'
            call edisp248(itu,outs248,90)
          endif
          if(IBSS.NE.0)then
            write(outs,'(A,I3,1X,A)')
     &        'Building services strategy: ',IBSS,BLDSS(IBSS)
            call edisp(itu,outs)
          else
            write(outs,'(A)')
     &        'Building services strategy: 0 not defined yet'
            call edisp(itu,outs)
          endif
          write(outs,'(A,I3,1X,A)')
     &      'Building Regulations: ',IRGG,BLDREG(IRGG)
          call edisp(itu,outs)
          write(outs,'(A,I3,1X,A)')
     &      'Building Design Stage: ',ISTG,BLDSTG(ISTG)
          call edisp(itu,outs)
          if(ISBT.NE.0)then
            write(outs,'(A,I3,1X,A)')
     &        'Scottish building type: ',ISBT,SBTYP(ISBT)
            call edisp(itu,outs)
            write(outs,'(2A)')
     &        'Accredited construction details (Scotland) followed ',
     &        SBREF
            call edisp(itu,outs)
          else
            write(outs,'(A,I3,A)')
     &        'Scottish building type: ',ISBT,
     &        ' Scottish regulations not being followed! '
            call edisp(itu,outs)
            write(outs,'(A)')
     &      'Accredited construction details (Scotland) followed?  N/A'
            call edisp(itu,outs)
          endif
        else
          write(outs,'(2a)') 'The current building use is undefined'
          call edisp(itu,outs)
        endif
      elseif(act.eq.'a'.or.act.eq.'s')then

C System information.
        write(outs,'(A,I4)')'Number of HVAC Systems:',NCMSYS
        call edisp(itu,outs)
        DO 101 ICMSYS=1,NCMSYS
          write(outs,'(A,I3)')'For HVAC system number:',ICMSYS
          call edisp(itu,outs)
          lnb=lnblnk(HVACNAME(ICMSYS))
          lnbb=lnblnk(HSYSNAME(INCMSYS(ICMSYS)))
          write(outs,'(3A,I3,2A)')
     &      'Name: ',HVACNAME(ICMSYS)(1:lnb),' index:',
     &      INCMSYS(ICMSYS),' type: ',HSYSNAME(INCMSYS(ICMSYS))(1:lnbb)
          call edisp(itu,outs)

          lnb=lnblnk(SYSNAME(IHGEF(ICMSYS)))
          write(outs,'(A,F6.3,a,I4,2A)')
     &      'Heating gen efficiency:',HGEF(ICMSYS),' index: ',
     &      IHGEF(ICMSYS),' name: ',SYSNAME(IHGEF(ICMSYS))(1:lnb)
          call edisp(itu,outs)
          IBRUK=IBRUKLH(IFTYP(ICMSYS),IHGEF(ICMSYS),INCMSYS(ICMSYS))
          if(IBRUK.LE.0)then
            call edisp(itu,'BRUKL heat generator not defined.')
            call edisp(itu,'Using default assumptions')
          else
            write(outs,'(A,I4)')
     &        'BRUKL heat generator index number:',IBRUK
            call edisp(itu,outs)

            lnb=lnblnk(FUELNAME(IFTYP(ICMSYS)))
            write(outs,'(A,I4,2A)')'fuel type index: ',IFTYP(ICMSYS),
     &        ' name: ',FUELNAME(IFTYP(ICMSYS))(1:lnb)
            call edisp(itu,outs)
          endif

          if(ICGEF(ICMSYS).NE.0)then
            lnb=lnblnk(SYSNAME(ICGEF(ICMSYS)+29))
            write(outs,'(A,F6.3,A,I4,2A)')'Cool gen efficiency: ',
     &        CGEF(ICMSYS),' index: ',ICGEF(ICMSYS),' name: ',
     &        SYSNAME(ICGEF(ICMSYS)+29)(1:lnb)
            call edisp(itu,outs)
            write(outs,'(A,I5)')
     &        'BRUKL cool generator index number:',IBRUK
            call edisp(itu,outs)
            write(outs,'(A,F6.3)')
     &       'Specific fan power W/l/s (0 if fans N/A)',SFPHS(ICMSYS)
            call edisp(itu,outs)
            write(outs,'(A,I5)')
     &        'BRUKL fan index number',IBRUKLF(INCMSYS(ICMSYS))
            call edisp(itu,outs)
            write(outs,'(4a)')'LeakageClass ductwork: ',
     &        ductwork(ICMSYS)(1:LNBLNK(ductwork(ICMSYS))),' AHU: ',
     &        AHUleakage(ICMSYS)(1:LNBLNK(AHUleakage(ICMSYS)))
            call edisp(itu,outs)
            write(outs,'(a,f6.4,a,f6.4)')
     &        'Auxil Energy Calculation duct constant: ',
     &        duct_tDLd(ICMSYS),' AHU constant: ',AHU_tDLd(ICMSYS)
            call edisp(itu,outs)
          else
            call edisp(itu,
     &      'No cool generator specified. Using default assumptions.')
          endif
 101    continue
        call edisp(itu,'  ')
        write(outs,'(A,I4,A)')'Zone to system linkage for',
     &     NCOMP,' zones'
        call edisp(itu,outs)
        DO 102 IZ=1,NCOMP
          IF(IHLZ(IZ).EQ.0)THEN
            TMPSTR=' No HVAC system for this zone!'
          ELSE
            lnb=lnblnk(HVACNAME(IHLZ(IZ)))
            WRITE(TMPSTR,'(2A)')' ',HVACNAME(IHLZ(IZ))(1:lnb)
          ENDIF
          lnb=lnzname(IZ)
          write(outs,'(3A,I3,A)')
     &      'Zone ',ZNAME(IZ)(1:lnb),' is linked to system number ',
     &      IHLZ(IZ),TMPSTR(1:lnblnk(TMPSTR))
          call edisp(itu,outs)
 102    CONTINUE
      elseif(act.eq.'a'.or.act.eq.'d')then

C DHW reporting.
        write(outs,'(A,I4)')'DHW generators included:',NDHWSYS
        call edisp(itu,outs)
        DO 103 IDHWSYS=1,NDHWSYS
          write(outs,'(A,I4)')'DHW generator number:',IDHWSYS
          call edisp(itu,outs)
          write(outs,'(3A,F6.3,A,I4)') 'Name: ',DHWNAME(IDHWSYS),
     &      ' efficiency: ',HWEF(IDHWSYS),' type: ',IDHWS(IDHWSYS)
          call edisp(itu,outs)
          if(IDHWSS(IDHWSYS).eq.0)then
            write(outs,'(A,I4,A,I4,A)')'fuel type: ',IDHFL(IDHWSYS),
     &       ' BRUKL index:',IBRUKLW(IDHWS(IDHWSYS),IDHFL(IDHWSYS)),
     &       ' simple generator'
          elseif(IDHWSS(IDHWSYS).eq.1)then
            write(outs,'(A,I4,A,I4,A)')'fuel type: ',IDHFL(IDHWSYS),
     &       ' BRUKL index:',IBRUKLW(IDHWS(IDHWSYS),IDHFL(IDHWSYS)),
     &       ' storage generator'
          elseif(IDHWSS(IDHWSYS).eq.2)then
            write(outs,'(A,I4,A,I4,A)')'fuel type: ',IDHFL(IDHWSYS),
     &       ' BRUKL index:',IBRUKLW(IDHWS(IDHWSYS),IDHFL(IDHWSYS)),
     &       ' storage with circulation loop'
          endif
          call edisp(itu,outs)
          write(outs,'(A,F6.0,A,F7.2,A,F6.3,A,F6.3,A,F6.0)')
     &      'storage tank m3:',DHWSPD(IDHWSYS,1),
     &      ' system loss MJ/mnth:',DHWSPD(IDHWSYS,2),
     &      ' circ loss W/m:',DHWSPD(IDHWSYS,3),
     &      ' pump power kW:',DHWSPD(IDHWSYS,4),
     &      ' loop length m:',DHWSPD(IDHWSYS,5)
          call edisp(itu,outs)
 103    continue
        write(outs,'(A,I4,A)')
     &    'Zone to DHW linkage for',NCOMP,' zones'
        call edisp(itu,outs)
        call edisp(itu,
     &   'Zone      dead-leg (m)    linked DHW system no. & name')
        DO 104 IZ=1,NCOMP
          IF(IDHWLZ(IZ).EQ.0)THEN
            TMPSTR=' Default DHW system'
          ELSEIF(IDHWLZ(IZ).GT.0)THEN
            WRITE(TMPSTR,'(2A)')' ',DHWNAME(IDHWLZ(IZ))
          ELSEIF(IDHWLZ(IZ).LT.0)THEN
            WRITE(TMPSTR,'(2A)')' (HVAC system) ',HVACNAME(-IDHWLZ(IZ))
          ENDIF
          write(outs,'(A,1X,F5.2,6X,I3,1X,A)')
     &      ZNAME(IZ),DEADLEG(IZ),IDHWLZ(IZ),
     &      TMPSTR(1:lnblnk(TMPSTR))
          call edisp(itu,outs)
 104    CONTINUE
      elseif(act.eq.'a'.or.act.eq.'o')then

C Occupancy links.
        write(outs,'(A)')'Zone to activity linkages'
        call edisp(itu,outs)
        DO 105 IZ=1,NCOMP
          lnb=lnzname(IZ)
          ireadactivityindex=theactivityindex(IZ)
          if(ireadactivityindex.gt.0)then
            write(outs,'(3A,I4,I6,1X,A)') 'Zone ',zname(iz)(1:lnb),
     &        ' is linked to ',IACTYTYP(IZ),ireadactivityindex,
     &        roomactname(ireadactivityindex)
          else
            write(outs,'(6A)') 'Zone ',zname(iz)(1:lnb),
     &        ' is linked to ',' 0',' 0',
     &        ' activity number 0 (undefined activity)'
          endif
          call edisp(itu,outs)
 105    continue
      elseif(act.eq.'a'.or.act.eq.'l')then

C Lighting.
        write(outs,'(A)')'Zone to lighting type linkage'
        call edisp(itu,outs)
        DO 106 IZ=1,NCOMP

C Check if there is a user defined entry for lighting heat gains
          lnb=lnzname(IZ)
          if(ILIGHTUSER(IZ).eq.1)then
            write(outs,'(4A,F7.2)') 'User_defined ',
     &        'Zone ',zname(iz)(1:lnb),
     &        ' uses heat gains from lights of (W/m2 per 100 lux):',
     &        LIGHTWATTAGE(IZ)
            call edisp(itu,outs)

C Otherwise take the suggested values (gains) for the specified
C lighting type
          elseif(ILIGHTUSER(IZ).eq.0)then
            if(ILITYP(IZ).gt.0)then
              write(outs,'(4A)') 'default_type ',
     &          'Zone ',zname(iz),' is linked to lighting type '
              call edisp(itu,outs)
              write(outs,'(I4,A)')ILITYP(IZ),TLIGHT(ILITYP(IZ))
            else
              write(outs,'(3A)') 'Zone ',zname(iz)(1:lnb),
     &         ' is linked to lighting type  0 (undefined type)'
            endif
            call edisp(itu,outs)
          endif
 106    continue
      endif
      return
      end

C ********************* EDSBMSYS
C EDSBMSYS edits the system definition within the UK NCM file.

      subroutine edsbmsys()
#include "building.h"
#include "sbem.h"

      common/OUTIN/IUOUT,IUIN
      integer iuout,iuin
      common/pophelp/h(60)
      character h*72

      integer JC,IY,IIN  ! positions in menu
      integer ICMSYS,KSYS,ITHND,IHSYS,IHCG,ICS,ICNDX
      dimension ITEMH(MCOM+4),KSYS(MHT),ITHND(MPT),PRSSN(MPT)
      character ITEMH*70,D64*64,PRSSN*60
      character outs*124
      integer INatMech !flag for easkab to choose between natural or
                       !mechanical extract/supply ventilation system
      integer nitEmH,IHO ! max items and current menu item

C Edit HVAC and system information after a display of current contents.
      call lstncm('s',iuout)
 205  IHO=-2
      H(1)='Help text for adding UK NCM HVAC systems'
      H(2)='This message still needs to be generated'
      DO 201 ICMSYS=1,NCMSYS
        WRITE(ITEMH(ICMSYS),'(A,1X,A)')CHAR(96+ICMSYS),
     &    HVACNAME(ICMSYS)
 201  CONTINUE
      ITEMH(NCMSYS+1)=' ----------------------------------------'
      ITEMH(NCMSYS+2)='+ add/delete'
      ITEMH(NCMSYS+3)='? help'
      ITEMH(NCMSYS+4)='- exit'
      NITEMH=NCMSYS+4
      CALL EMENU('HVAC systems defined',ITEMH,nitEmH,IHO)
      IF(IHO.EQ.NCMSYS+4)THEN
        return
      ELSEIF(IHO.EQ.NCMSYS+3)THEN
        CALL PHELPD('NCM HVAC help',2,'-',0,0,IER)
      ELSEIF(IHO.EQ.NCMSYS+2)THEN
        JC=0
        CALL EASKABC('Choose option',' ','add','delete','cancel',JC,2)
        IF(JC.EQ.1)THEN

C Add HVAC and system information
          NCMSYS=NCMSYS+1
          H(1) ='Choose the type of HVAC environment control system'
          H(2) ='that satisfies / comes closest to your requirements'
          H(3) ='(see section 7.6.2 of the iSBEM users guide). The'
          H(4) ='available selections are summarized below: '
          H(5) =' '
          H(6) ='Central heating using water: radiators '
          H(7) ='  Centrally generated heat with water distribution'
          H(8) ='  using radiators in rooms. Natural vent assumed.'
          H(9) =' '
          H(10)='Central heating using water: convectors '
          H(11) =' '
          H(12)='  As above but using convectors in rooms.'
          H(13)=' '
          H(14)='Central heating using water: floor heating '
          H(15)='  As above but with under floor heating.'
          H(16)=' '
          H(17)='Central heating with air distribution '
          H(18)='  As above but with air distribution.'
          H(19)=' '
          H(20)='Other local room heater - fanned. Includes gas'
          H(21)='  fires, gas convectors, direct electric heaters,'
          H(22)='  electric storage (with fans). Large units should'
          H(23)='  be treated as forced convection air heaters.'
          H(24)=' '
          H(25)='Other local room heater - unfanned. Includes gas'
          H(26)='  fires, gas convectors, direct electric heaters,'
          H(27)='  electric storage (w/o fans).'
          H(28)=' '
          H(29)='Unflued radiant heater - high temperature overhead'
          H(30)='  radiant (electric or gas) without flue.'
          H(31)=' '
          H(32)='Flued radiant heater - high temperature overhead'
          H(33)='  radiant (electric or gas) with flue.'
          H(34)=' '
          H(35)='Multi-burner radiant heaters - high temperature'
          H(36)='  overhead gas units'
          H(37)=' '
          H(38)='Flued forced-convective air heaters - optional fan'
          H(39)='  to assist combustion.'
          H(40)=' '
          H(41)='Unflued forced-convective air heaters - typically'
          H(42)='  called direct gas heaters. Adequate ventilation'
          H(43)='  must be provided.'
          H(44)=' '
          H(45)='continued... '
          CALL PHELPD('SBEM HVAC choices',45,'-',0,0,IER)
          H(1) ='Air based HVAC systems follow... '
          H(2) =' '
          H(3) ='Single-duct VAV - all air system where volume is'
          H(4) ='  adjusted to match cooling load. Reheat optional.'
          H(5) ='  Assumed to provide heating. Optional perimeter'
          H(6) ='  heating.'
          H(7) =' '
          H(8) ='Dual-duct VAV - as above except separate hot and'
          H(9) ='  cold air ducts.'
          H(10)=' '
          H(11)='Indoor packaged cabinet (VAV) - local cooling &'
          H(12)='  heating units supplying to room or floor or'
          H(13)='  ceiling voids. Optional locally varied recirc'
          H(14)='  rate and electric top-up heating.'
          H(15)=' '
          H(16)='Fan coil system - local fanned terminal unit (in'
          H(17)='  ceiling/on wall/on floor) with central chilled'
          H(18)='  water. 2/3/4-pipe. Assumed to provide heating.'
          H(19)=' '
          H(20)='Induction system - high pressure central supply'
          H(21)='  air with heating and cooling within terminal'
          H(22)='  unit. 2/3/4-pipe. Assumed to provide heating.'
          H(23)=' '
          H(24)='Constant volume system with fixed fresh air - '
          H(25)='  Fixed air volume units w/o terminal reheat.'
          H(26)='  Assumed to provide heating.'
          H(27)=' '
          H(28)='Constant volume system with vairable fresh air -'
          H(29)='  As above but with free cooling economiser.'
          H(30)=' '
          H(31)='Multizone hot deck/cold deck - central AHU with a'
          H(32)='  single duct to each room. Provides heating.'
          H(33)=' '
          H(34)='Terminal reheat - Central chilled air supply with'
          H(35)='  reheat at terminal. Assumed to provide heating'
          H(36)='  and optional perimeter heating.'
          H(37)=' '
          H(38)='Dual duct (constant volume) - separate hot and '
          H(39)='  cold air supplies locally mixed at each zone.'
          H(40)='  Assumed to provide heat, optional perimeter.'
          H(41)=' '
          H(42)='Chilled ceilings or passive chilled beams - A '
          H(43)='  combination of chilled ceiling or passive'
          H(44)='  chilled beam with separate tempered ventilation'
          H(45)='  air. Heating via LTHW. Assumes displacement vent.'
          H(46)=' '
          H(47)='Active chilled beams - typically induction units'
          H(48)='  via central high pressure air supply. Heating'
          H(49)='  via LTHW. May include local fans.'
          H(50)=' '
          H(51)='Water loop heat pump - local heat pumps served by'
          H(52)='  common water loop. Central heating or cooling '
          H(53)='  plant used. Assumed to provide heating.'
          H(54)=' '
          H(55)='Split or multi-split system - outdoor and indoor'
          H(56)='  units connected via refrigerant. No mechanical'
          H(57)='  vent. Also for ducted split systems.'
          H(58)=' '
          H(59)='Single room cooling - integral units w/o ducts. '
          CALL PHELPD('SBEM HVAC choices',59,'-',0,0,IER)

          HVACNAME(NCMSYS)=' default HVAC system '
          CALL EASKS(HVACNAME(NCMSYS),'HVAC system name',
     &      '(<30 characters)',30,D64,'HVAC sys name',IER,59)
          IY=1
          KSYS(1)=0
 906      CALL EPICKS(IY,KSYS,' ','Select system type',
     &      70,MHT,HSYSNAME,'system types',IER,59)
          IF(KSYS(1).EQ.0)GOTO 906
          IHSYS=KSYS(1)
          IF(IHSYS.GE.1)INCMSYS(NCMSYS)=IHSYS

C Add corresponding heat generator, efficiency and fuel type
          IHCG=1
          DO 220 IPT=1,MPT
            IF(ISYSAPP(IPT,IHSYS).EQ.1)THEN
              PRSSN(IHCG)=SYSNAME(IPT)
              HGEF(IHCG)=SYSEFF(IPT)
              ITHND(IHCG)=IPT
              IHCG=IHCG+1
            ENDIF
 220      CONTINUE
 330      CALL EDISP(IUOUT,'Select heat generator')
          IY=1
          KSYS(1)=0
          CALL EPICKS(IY,KSYS,' ','Select heat generator',
     &      60,IHCG-1,PRSSN,'system types',IER,2)
          IF(KSYS(1).EQ.0)GOTO 330
          IHGEF(NCMSYS)=ITHND(KSYS(1))

C Display HVAC heat generator exceptions if applicable
          IF(IHGEF(NCMSYS).EQ.21)THEN
            H(1)='You will be asked for system efficiency/COP next'
            H(2)='For the district heating option note that default'
            H(3)='emission factor specified in iSBEM v3.4.a is 0.293'
            H(4)='kgCO2/kWh. Default efficiency of the system is '
            H(5)='set to 0.65, this corresponds to natural gas as fuel'
            H(6)='for aforementioned emission factor.'
            CALL PHELPD('SBEM HVAC exceptions',6,'-',0,0,IER)
            HGEF(NCMSYS)=0.65
          ELSEIF(IHGEF(NCMSYS).LE.20.AND.IHGEF(NCMSYS).GE.9)THEN
            H(1)='You will be asked for system efficiency/COP next'
            H(2)='For heat pumps values for natural gas are quoted as'
            H(3)='default values. The same values are applicable to '
            H(4)='oil as well. If electricity is being used as fuel '
            H(5)='then it is appropriate to multiply the COP with 2.5'
            H(6)='(following iSBEM ver 3.4.a)'
            CALL PHELPD('SBEM HVAC exceptions',6,'-',0,0,IER)
          ENDIF

C Ask for system efficiency
          CALL EASKR(HGEF(NCMSYS),
     &      'Enter heat generator efficiency/COP or accept default',
     &      ' ',0.,'F',0.,'-',10.,'heat gen eff ',IER,2)
 331      IY=1
          KSYS(1)=0
          CALL EDISP(IUOUT,'Select fuel type')
          CALL EPICKS(IY,KSYS,' ','Select fuel type',
     &      42,MFT,FUELNAME,'Fuel Types',IER,2)
          IF(KSYS(1).EQ.0)GOTO 331
          IFTYP(NCMSYS)=KSYS(1)

C Add corresponding cool generator if applicable
          IF(IBRUKLC(1,IHSYS).NE.-1111)THEN
 456        CALL EASKATOG(' Choose cooling system',' ',SYSNAME(MPT-4),
     &      SYSNAME(MPT-3),SYSNAME(MPT-2),SYSNAME(MPT-1),SYSNAME(MPT),
     &      ' ',' ',ICGEF(NCMSYS),2)
            ICS=ICGEF(NCMSYS)
            IF(ICS.GE.6)GOTO 456
            IF(ICS.LE.3)THEN
              CALL EASKABCD('Enter power rating of system (kW)',' ',
     &        '0-100','101-500','501-750','751-3500',IADC,1)
              CALL EASKAB('Is this system in the ECA list',
     &        ' ','yes','no',IECA,2)
              ICNDX=8*(ICS-1)+2*(IADC-1)+IECA
            ELSE

C Heat pumps do not have range of power ratings (use maximum value of
C counters above)
              ICNDX=8*(3-1)+2*(4-1)+2+ICS
            ENDIF

C Values in database are nominal energy efficiency ratio (EER) values
C which are 125% of Seasonal EER value so multiply by 0.8
            CGEF(NCMSYS)=SYSEFFC(ICNDX,IHSYS)*0.8
            CALL EASKR(CGEF(NCMSYS),
     &        'Enter cool generator efficiency/COP or accept default',
     &        ' ',0.,'F',0.,'-',10.,'cool gen eff ',IER,2)
          ELSE
            CGEF(NCMSYS)=0.0
            ICGEF(NCMSYS)=0
          ENDIF

C Ask for ventilation specific fan power SFP if applicable
          IF(IBRUKLF(IHSYS).NE.-1111)THEN
            SFPHS(NCMSYS)=SFPDEF
            CALL EASKR(SFPHS(NCMSYS),
     &        'Enter ventilation system specifc fan power SFP or',
     &        'accept default (Watts/liter/sec)',
     &        0.,'F',0.,'-',50.,'heat gen eff ',IER,2)

C Define system adjustment inputs for ductwork and AHU
            call defLeakage(NCMSYS,IER)
          ELSE
            duct_tDLd(NCMSYS)=0.0 !assume no ducts for these systems
            AHU_tDLd(NCMSYS)=0.0
          ENDIF

C Ask if there is a supply/extract fun (for nat. ventil. cases only)
            H(1) ='If supply/extract then additional ventilation '
          H(2) ='losses will be taken for the heating loads plus'
          H(3) ='additional auxiliary loads.'
          H(4) =' '
          H(5) ='Choose centralised system for centralised balanced'
          H(6) ='mechanical ventilation system including air supplies'
          H(7) ='to centralised air conditioning system'
          H(8) =''
          H(9) ='Choose zonal supply for where the fan in remote from'
          H(10)='the zone such as ceiling void or roof mounted units'
          H(11) =''
          H(12)='Choose zonal extract where the fan is remote from the'
          H(13)='zone'
          CALL EASKABCD('Zone ventilation type (see help)',
     &    ' ','Natural','Centralised balanced',
     &    'Zonal supply','Zonal extract',Ivent(ncmsys),13)
          CALL EASKAB('Does system have local ventilation only units',
     &    'e.g. toilet exhausts ','Yes','No',IIN,1)
          INatMech=IVENT(NCMSYS)+1
          IF(IIN.EQ.1)IVENT(NCMSYS)=10+IVENT(NCMSYS)
            if(INatMech.eq.1)then
            SFPHS(NCMSYS)=0.0
          else
              SFPHS(NCMSYS)=1.5 !1.5 W/l/s is the default in SBEM
              CALL EASKR(SFPHS(NCMSYS),
     &        'Enter supply/extract specifc fan power SFP or',
     &        'accept default (Watts/liter/sec)',
     &      0.,'F',0.,'-',50.,'sfp mech ',IER,2)
          endif
        ELSEIF(JC.EQ.2)THEN

C Delete HVAC and system information
          H(1) = 'Choose HVAC system to delete or simply exit without'
          H(2) = 'making any choice to retain all defined systems'
 907      IZ=1
          KSYS(1)=0
          CALL EPICKS(IZ,KSYS,' ','HVAC system to delete',
     &      70,NCMSYS,ITEMH,'HVAC system to delete',IER,2)
          IF(KSYS(1).EQ.0)GOTO 907
          IF(KSYS(1).GE.1)THEN
            DO 203 ICMSYS=KSYS(1),NCMSYS
              INCMSYS(ICMSYS)  = INCMSYS(ICMSYS+1)
              IHGEF(ICMSYS)    = IHGEF(ICMSYS+1)
              HGEF(ICMSYS)     = HGEF(ICMSYS+1)
              IFTYP(ICMSYS)    = IFTYP(ICMSYS+1)
              ICGEF(ICMSYS)    = ICGEF(ICMSYS+1)
              CGEF(ICMSYS)     = CGEF(ICMSYS+1)
              SFPHS(ICMSYS)    = SFPHS(ICMSYS+1)
              HVACNAME(ICMSYS) = HVACNAME(ICMSYS+1)
 203        CONTINUE
            NCMSYS=NCMSYS-1
          ENDIF
        ELSEIF(JC.EQ.3)THEN
          CONTINUE
        ENDIF
      ELSEIF(IHO.LE.NCMSYS+3.AND.IHO.GE.1)THEN

C Edit HVAC and system information
        H(1) = 'Choose HVAC system to replace current system'
        H(2) = '(see text feed back area) or exit to retain current'
        CALL EASKS(HVACNAME(IHO),'HVAC system name',
     &    '(<30 characters)',30,D64,'HVAC sys name',IER,1)
        IY=1
        KSYS(1)=0
 908    WRITE(OUTS,'(2A)')'Current HVAC system is ',
     &    HSYSNAME(INCMSYS(IHO))
        call edisp(iuout,OUTS)
        call edisp(iuout,
     &    'Choose HVAC system to replace it or exit to retain current')
        CALL EPICKS(IY,KSYS,' ','Select system type',
     &    70,MHT,HSYSNAME,'system types',IER,2)
        IF(KSYS(1).EQ.0)GOTO 908
        IHSYS=KSYS(1)
        IF(IHSYS.GE.1)THEN
          INCMSYS(IHO)=IHSYS
        ENDIF

C Edit corresponding heat generator, efficiency and fuel type
        IHCG=1
        DO 229 IPT=1,MPT
          IF(ISYSAPP(IPT,IHSYS).EQ.1)THEN
            PRSSN(IHCG)=SYSNAME(IPT)
            ITHND(IHCG)=IPT
            IHCG=IHCG+1
          ENDIF
 229    CONTINUE
 339    CALL EDISP(IUOUT,'Select heat generator')
        WRITE(OUTS,'(2A)')'Current heat generator is',
     &    SYSNAME(IHGEF(IHO))
        CALL EDISP(IUOUT,OUTS)
        IY=1
        KSYS(1)=0
        CALL EPICKS(IY,KSYS,' ','Select heat generator',
*     &    60,IHCG,PRSSN,'system types',IER,2)
     &    60,IHCG-1,PRSSN,'system types',IER,2)
        IF(KSYS(1).EQ.0)GOTO 339
        IHGEF(IHO)=ITHND(KSYS(1))

C Display HVAC heat generator exceptions if applicable
 	IF(IHGEF(IHO).EQ.21)THEN
 	  H(1)='You will be asked for system efficiency/COP next'
 	  H(2)='For the district heating option note that default'
 	  H(3)='emission factor specified in iSBEM v3.4.a is 0.293'
 	  H(4)='kgCO2/kWh. Default efficiency of the system is '
 	  H(5)='set to 0.65, this corresponds to natural gas as fuel'
 	  H(6)='for aforementioned emission factor.'
 	  CALL PHELPD('SBEM HVAC exceptions',6,'-',0,0,IER)
 	  HGEF(IHO)=0.65
 	ELSEIF(IHGEF(IHO).LE.20.AND.IHGEF(IHO).GE.9)THEN
 	  H(1)='You will be asked for system efficiency/COP next'
 	  H(2)='For heat pumps values for natural gas are quoted as'
 	  H(3)='default values. The same values are applicable to '
 	  H(4)='oil as well. If electricity is being used as fuel '
 	  H(5)='then it is appropriate to multiply the COP with 2.5'
 	  H(6)='(following iSBEM ver 3.4.a)'
 	  CALL PHELPD('SBEM HVAC exceptions',6,'-',0,0,IER)
 	ENDIF

        CALL EASKR(HGEF(IHO),
     &    'Edit heat/cool generator efficiency/COP',' ',
     &    0.,'F',0.,'-',10.,'heat gen eff ',IER,2)
 341    IY=1
        KSYS(1)=0
        CALL EDISP(IUOUT,'Select fuel type')
        CALL EPICKS(IY,KSYS,' ','Select fuel type',
     &    42,MFT,FUELNAME,'Fuel Types',IER,2)
        IF(KSYS(1).EQ.0)GOTO 341
        IFTYP(IHO)=KSYS(1)

C Edit corresponding cool generator if applicable
        IF(IBRUKLC(1,IHSYS).NE.-1111)THEN
 457        CALL EASKATOG(' Choose cooling system',' ',SYSNAME(MPT-4),
     &      SYSNAME(MPT-3),SYSNAME(MPT-2),SYSNAME(MPT-1),SYSNAME(MPT),
     &      ' ',' ',ICGEF(IHO),2)
            ICS=ICGEF(IHO)
            IF(ICS.GE.6)GOTO 457
            IF(ICS.LE.3)THEN
              CALL EASKABCD('Enter power rating of system (kW)',' ',
     &        '0-100','101-500','501-750','751-3500',IADC,1)
              CALL EASKAB('Is this system in the ECA list',
     &        ' ','yes','no',IECA,2)
              ICNDX=8*(ICS-1)+2*(IADC-1)+IECA
            ELSE

C Heat pumps do not have range of power ratings (use maximum value of
C counters above)
              ICNDX=8*(3-1)+2*(4-1)+2+ICS
            ENDIF

C Values in database are nominal energy efficiency ratio (EER) values
C which are 125% of Seasonal EER value so multiply by 0.8
            CGEF(IHO)=SYSEFFC(ICNDX,IHSYS)*0.8
            CALL EASKR(CGEF(IHO),
     &        'Enter cool generator efficiency/COP or accept default',
     &        ' ',0.,'F',0.,'-',10.,'cool gen eff ',IER,2)
        ELSE
          CGEF(IHO)=0.0
          ICGEF(IHO)=0
        ENDIF

C Edit ventilation specific fan power SFP if applicable
        IF(IBRUKLF(IHSYS).NE.-1111)THEN
          CALL EASKR(SFPHS(IHO),
     &      'Enter ventilation system specifc fan power SFP or',
     &      'accept default (Watts/liter/sec)',
     &      0.,'F',0.,'-',50.,'heat gen eff ',IER,2)

C Define system adjustment inputs for ductwork and AHU
          call defLeakage(IHO,IER)
        ELSE

C Ask if there is a supply/extract fun (for nat. ventil. cases only)
          H(1) ='If supply/extract then additional ventilation '
          H(2) =' losses will be taken for the heating loads plus'
          H(3) =' additional auxiliary loads.'
          CALL EASKAB('Zone ventilation type',
     &    ' ','Natural','Machanical supply or extract',INatMech,3)
          if(INatMech.eq.1)then
            SFPHS(IHO)=0.0
          elseif(INatMech.eq.2)then
            SFPHS(IHO)=1.5 !1.5 W/l/s is the default in SBEM
            CALL EASKR(SFPHS(IHO),
     &      'Enter supply/extract specifc fan power SFP or',
     &      'accept default (Watts/liter/sec)',
     &      0.,'F',0.,'-',50.,'sfp mech ',IER,2)
          endif
          duct_tDLd(IHO)=0.0
          AHU_tDLd(IHO)=0.0
        ENDIF
      ENDIF
      IF(IHO.NE.NITEMH)GOTO 205

      return
      end

C ********************* EDSBMBLD
C EDSBMBLD edits the building contact definition within the UK NCM file.

      subroutine edsbmbld()
#include "building.h"
#include "sbem.h"

      common/OUTIN/IUOUT,IUIN
      integer iuout,iuin
      common/pophelp/h(60)
      character h*72
      DIMENSION ITEMS(33)
      character HEAD*30,ITEMS*50,D64*64,D20*20,outs*124
      integer nitms,INO ! max items and current menu item

C Enter building, owner and verifier information
      call edisp(iuout,' ')          
      call edisp(iuout,'Details for code complience...')
      write(outs,'(2a)')'Project name: ',pjname(1:lnblnk(pjname))
      call edisp(iuout,outs)
      write(outs,'(2a)')'Building address: ',
     &  baddress(1:lnblnk(baddress))
      call edisp(iuout,outs)
      write(outs,'(2a)')'Building city: ',bcity(1:lnblnk(bcity)) 
      call edisp(iuout,outs)
      write(outs,'(2a)')'Building Postcode: ',
     &  buildingPC(1:lnblnk(buildingPC))   
      call edisp(iuout,outs)
      call edisp(iuout,' ')          
      call edisp(iuout,'Details of client/owner...')
      write(outs,'(2a)')'Building owner name: ',
     &  ownername(1:lnblnk(ownername))
      call edisp(iuout,outs)   
      write(outs,'(2a)')'Building owner telephone: ',
     &  ownertel(1:lnblnk(ownertel))
      call edisp(iuout,outs)   
      write(outs,'(2a)')'Building owner address: ',
     &  owneraddr(1:lnblnk(owneraddr)) 
      call edisp(iuout,outs) 
      write(outs,'(2a)')'Building owner city: ',
     &  ownercity(1:lnblnk(ownercity)) 
      call edisp(iuout,outs) 
      write(outs,'(2a)')'Building owner Postcode: ',
     &  ownerPC(1:lnblnk(ownerPC))
      call edisp(iuout,outs)     
      call edisp(iuout,' ')          
      call edisp(iuout,'Details of simulationist...')
      write(outs,'(2a)')'Simulationist name: ',
     &  certifname(1:lnblnk(certifname)) 
      call edisp(iuout,outs)     
      write(outs,'(2a)')'Simulationist telephone: ',
     &  certTel(1:lnblnk(certTel))  
      call edisp(iuout,outs)     
      write(outs,'(2a)')'Simulationist address: ',
     &  certaddress(1:lnblnk(certaddress))    
      call edisp(iuout,outs)   
      write(outs,'(2a)')'Simulationist city: ',
     &  certcity(1:lnblnk(certcity))   
      call edisp(iuout,outs)    
      write(outs,'(2a)')'Simulationist postcode: ',
     &  certPC(1:lnblnk(certPC))   
      call edisp(iuout,outs)          
    
  15  CALL EDISP(IUOUT,
     &  'Enter/Confirm building, owner and certifier details')
      INO=-1
      ITEMS(1)  = 'a Project name: '
      write(ITEMS(2),'(2A)') '  ',pjname(1:20)
      ITEMS(3)  = 'b Building address: '
      write(ITEMS(4),'(2A)') '  ',baddress(1:48)
      ITEMS(5)  = 'c Building city: '
      write(ITEMS(6),'(2A)') '  ',bcity(1:48)
      ITEMS(7)  = 'd Building Postcode: '
      write(ITEMS(8),'(2A)') '  ',buildingPC(1:48)
      ITEMS(9)  = 'e Building owner name: '
      write(ITEMS(10),'(2A)') '  ',ownername(1:48)
      ITEMS(11) = 'f Building owner telephone: '
      write(ITEMS(12),'(2A)') '  ',ownertel(1:48)
      ITEMS(13) = 'g Building owner address: '
      write(ITEMS(14),'(2A)') '  ',owneraddr(1:48)
      ITEMS(15) = 'h Building owner city: '
      write(ITEMS(16),'(2A)') '  ',ownercity(1:48)
      ITEMS(17) = 'i Building owner Postcode: '
      write(ITEMS(18),'(2A)') '  ',ownerPC(1:48)
      ITEMS(19) = 'j Certifier name: '
      write(ITEMS(20),'(2A)') '  ',certifname(1:48)
      ITEMS(21) = 'k Certifier telephone: '
      write(ITEMS(22),'(2A)') '  ',certTel(1:48)
      ITEMS(23) = 'l Certifier address: '
      write(ITEMS(24),'(2A)') '  ',certaddress(1:48)
      ITEMS(25) = 'm Certifier city: '
      write(ITEMS(26),'(2A)') '  ',certcity(1:48)
      ITEMS(27) = 'n Certifier postcode: '
      write(ITEMS(28),'(2A)') '  ',certPC(1:48)
      ITEMS(29) = '# Enter all details            '
      ITEMS(30) = '  _____________________________'
      ITEMS(31) = '* list project details         '
      ITEMS(32) = '? help                         '
      ITEMS(33) = '- exit this menu               '
      nitms=33
      WRITE(HEAD,'(2A)')'Project details for BRUKL/EPC'

C Help text for this menu.
      H(1)='This menu allows the definition of project'
      H(2)='details. These will be used by BRUKL/EPC for'
      H(3)='reporting purposes and are required data.'
      H(4)=' '
      H(5)='The enter all details will ask for for all the details'
      H(6)='shown in the menu (in sequence).'

C Menu for building details.
      CALL EMENU(HEAD,ITEMS,nitms,INO)
      D64='<this item has not been described>'
      IF(INO.EQ.nitms-1)THEN

C Produce help text for the menu.
        CALL PHELPD(HEAD,6,'-',0,0,IER)
      ELSEIF(INO.EQ.1)THEN

C Building project description. Echo after edit.
        D20='Project un-named'
        H(1)='Input a succinct description of the project name here.'
        CALL EASKS(pjname,'project name?','(20 characters maximum)',
     &    20,D20,'project name',IER,1)
        call tstamp('>','PRJ: edited project name')
        write(outs,'(2a)')'Project name: ',pjname(1:lnblnk(pjname))
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.3)THEN

C Building address. Echo after edit.
        D64='No site selected.'
        H(1)='Input the building street address.'
        CALL EASKS(baddress,'building address?','(<64 characters)',
     &    64,D64,'build address',IER,1)
        call tstamp('>','PRJ: edited project address')
        write(outs,'(2a)')'Building address: ',
     &    baddress(1:lnblnk(baddress))
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.5)THEN

C Building city name. Echo after edit.
        D64='No site selected.'
        H(1)='Input the names of the town/city of the building.'
        CALL EASKS(bcity,'building city?','(<64 characters)',
     &    64,D64,'city building',IER,1)
        call tstamp('>','PRJ: edited project city')
        write(outs,'(2a)')'Building city: ',bcity(1:lnblnk(bcity))
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.7)THEN

C Building post code. Echo after edit.
        D64='No site selected.'
        H(1)='Input for: building post code'
        CALL EASKS(buildingPC,'building post code?',
     &    '(<64 characters)',64,D64,'building PC',IER,1)
        call tstamp('>','PRJ: edited project post code')
        write(outs,'(2a)')'Building Postcode: ',
     &    buildingPC(1:lnblnk(buildingPC))
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.9)THEN

C Building owner name. Echo after edit.
        D64='Owner name withheld'
        H(1)='Input a succinct description of the owner name here'
        CALL EASKS(ownername,'Name of owner?','(<64 characters)',
     &    64,D64,'owner name',IER,1)
        call tstamp('>','PRJ: edited project owner name')
        write(outs,'(2a)')'Building owner name: ',
     &    ownername(1:lnblnk(ownername))
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.11)THEN

C Building owner phone number.
        D64='Owner phone withheld'
        H(1)='telephone number of the owner'
        CALL EASKS(ownertel,'Phone of owner?','(<64 characters)',
     &    64,D64,'owner tel',IER,1)
        call tstamp('>','PRJ: edited project owner phone')
        write(outs,'(2a)')'Building owner telephone: ',
     &    ownertel(1:lnblnk(ownertel))
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.13)THEN

C Building owner address.
        D64='Owner address withheld.'
        H(1)='Street address of the owner'
        CALL EASKS(owneraddr,'Address of owner?','(<64 characters)',
     &    64,D64,'owner address',IER,1)
        call tstamp('>','PRJ: edited project owner address')
        write(outs,'(2a)')'Building owner address: ',
     &    owneraddr(1:lnblnk(owneraddr))
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.15)THEN

C Building owner city.
        D64='Owner city withheld.'
        H(1)='City (where the owner is located)'
        CALL EASKS(ownercity,'City of owner?','(<64 characters)',
     &    64,D64,'owner city',IER,1)
        call tstamp('>','PRJ: edited project owner city')
        write(outs,'(2a)')'Building owner city: ',
     &    ownercity(1:lnblnk(ownercity))
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.17)THEN

C Building owner postcode.
        D64='Owner postcode withheld.'
        H(1)='Post code (where the owner is located)'
        CALL EASKS(ownerPC,'Post code for owner?','(<64 characters)',
     &    64,D64,'owner post code',IER,1)
        call tstamp('>','PRJ: edited project owner postcode')
        write(outs,'(2a)')'Building owner Postcode: ',
     &    ownerPC(1:lnblnk(ownerPC))
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.19)THEN

C Building certifier name.
        D64='Certifier name withheld.'
        H(1)='certifier name'
        CALL EASKS(certifname,'Name of certifier?','(<64 characters)',
     &    64,D64,'certifier name',IER,1)
        call tstamp('>','PRJ: edited project certifier')
        write(outs,'(2a)')'Certifier name: ',
     &    certifname(1:lnblnk(certifname))
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.21)THEN

C Building certifier phone number.
        D64='Certifier phone number withheld.'
        H(1)='certifier telephone number'
        CALL EASKS(certTel,'Phone of certifier?','(<64 characters)',
     &    64,D64,'certifier tel',IER,1)
        call tstamp('>','PRJ: edited project certifier phone')
        write(outs,'(2a)')'Certifier telephone: ',
     &    certTel(1:lnblnk(certTel))
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.23)THEN

C Building certifier address.
        D64='Certifier address withheld.'
        H(1)='certifier street address'
        CALL EASKS(certaddress,'Address of certifier?',
     &    '(<64 characters)',64,D64,'certifier address',IER,1)
        call tstamp('>','PRJ: edited project cert address')
        write(outs,'(2a)')'Certifier address: ',
     &    certaddress(1:lnblnk(certaddress))
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.25)THEN

C Building certifier city.
        D64='Certifier city withheld.'
        H(1)='Name of city of the certifier.'
        CALL EASKS(certcity,'City of certifier?','(<64 characters)',
     &    64,D64,'certifier city',IER,1)
        call tstamp('>','PRJ: edited project certifier city')
        write(outs,'(2a)')'Certifier city: ',
     &    certcity(1:lnblnk(certcity))
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.27)THEN

C Building certifier postcode.
        D64='Certifier postcode withheld.'
        H(1)='certifier post code'
        CALL EASKS(certPC,'Post code of certifier?',
     &    '(<64 characters)',64,D64,'certifier Post code',IER,1)
        call tstamp('>','PRJ: edited project cert post code')
        write(outs,'(2a)')'Certifier postcode: ',
     &    certPC(1:lnblnk(certPC))
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.29)THEN

C Enter all information
        D20='Project un-named'
        H(1)='Input a succinct description of the project name here.'
        CALL EASKS(pjname,'project name?','(20 characters maximum)',
     &    20,D20,'project name',IER,1)
        D64='No site selected.'
        H(1)='Input the building street address.'
        CALL EASKS(baddress,'building address?','(<64 characters)',
     &    64,D64,'build address',IER,1)
        D64='No site selected.'
        H(1)='Input the names of the town/city of the building.'
        CALL EASKS(bcity,'building city?','(<64 characters)',
     &    64,D64,'city building',IER,1)
        D64='No site selected.'
        H(1)='Input for: building post code'
        CALL EASKS(buildingPC,'building post code?',
     &    '(<64 characters)',64,D64,'building PC',IER,1)
        D64='Owner name withheld'
        H(1)='Input a succinct description of the owner name here'
        CALL EASKS(ownername,'Name of owner?','(<64 characters)',
     &    64,D64,'owner name',IER,1)
        D64='Owner phone withheld'
        H(1)='telephone number of the owner'
        CALL EASKS(ownertel,'Telephone of owner?','(<64 characters)',
     &    64,D64,'owner tel',IER,1)
        D64='Owner address withheld.'
        H(1)='Street address of the owner'
        CALL EASKS(owneraddr,'Address of owner?','(<64 characters)',
     &    64,D64,'owner address',IER,1)
        D64='Owner city withheld.'
        H(1)='City (where the owner is located)'
        CALL EASKS(ownercity,'City of owner?','(<64 characters)',
     &    64,D64,'owner city',IER,1)
        D64='Owner postcode withheld.'
        H(1)='Post code (where the owner is located)'
        CALL EASKS(ownerPC,'Post code for owner?','(<64 characters)',
     &    64,D64,'owner post code',IER,1)
        D64='Certifier name withheld.'
        H(1)='certifier name'
        CALL EASKS(certifname,'Name of certifier?',
     &    '(<64 characters)',64,D64,'certifier name',IER,1)
        D64='Certifier phone number withheld.'
        H(1)='certifier telephone number'
        CALL EASKS(certTel,'Phone of certifier?','(<64 characters)',
     &    64,D64,'certifier tel',IER,1)
        D64='Certifier address withheld.'
        H(1)='Certifier street address'
        CALL EASKS(certaddress,'Address of certifier?',
     &    '(<64 characters)',64,D64,'certifier address',IER,1)
        D64='Certifier city withheld.'
        H(1)='Name of city of the certifier.'
        CALL EASKS(certcity,'City of certifier ?','(<64 characters)',
     &    64,D64,'certifier city',IER,1)
        D64='Certifier postcode withheld.'
        H(1)='certifier post code'
        CALL EASKS(certPC,'Post code of certifier?',
     &    '(<64 characters)',64,D64,'certifier Post code',IER,1)
        call lstncm('p',iuout)
      ELSEIF(INO.EQ.31)THEN

C Display information
        call lstncm('p',iuout)
      ENDIF
      IF(INO.NE.nitms)GOTO 15

      return
      end


C ********************* EDSBASSOR
C EDSBMASSOR edits the assessor contact definition within the UK NCM file.

      subroutine edsbmassor()
#include "building.h"
#include "sbem.h"

      common/OUTIN/IUOUT,IUIN
      integer iuout,iuin
      common/pophelp/h(60)
      character h*72
      DIMENSION ITEMS(33)
      character HEAD*30,ITEMS*50,D64*64,outs*124
      integer nitms,INO ! max items and current menu item

C List current state of the energy assessor data.
      call lstncm('e',iuout)

C Enter energy assessor and additional building information
      CALL EDISP(IUOUT,
     &  'Enter Energy Assessor and additional building details')

  55  INO=-1
      ITEMS(1)  = 'a Unique Property Reference Number (UPRN): '
      write(ITEMS(2),'(2A)') '  ',UPRN(1:48)
      ITEMS(3)  = 'b Inspection date: '
      write(ITEMS(4),'(A,I2,A,I2,A,I4)') '  ',D_inspect,'  ',
     &  M_inspect,'  ',Y_inspect
      ITEMS(5)  = 'd Accreditation Scheme (E,W and NI only): '
      write(ITEMS(6),'(2A)') '  ',Accr_Scheme(1:48)
      ITEMS(7)  = 'e Assessor registration number (E,W and NI only):'
      write(ITEMS(8),'(2A)') '  ',assessRegNumber(1:48)
      ITEMS(9) =  'f Employer/trading name of energy assessor: '
      write(ITEMS(10),'(2A)') '  ',empl_Trading_name(1:48)
      ITEMS(11) = 'g Employer/trading address of energy assessor: '
      write(ITEMS(12),'(2A)') '  ',addr_empl_Trading(1:48)
      ITEMS(13) = 'h Related party disclosure: '
      write(ITEMS(14),'(2A)') '  ',party_disclosure(1:48)
      ITEMS(15) = 'i Qualifications of energy assessor (3 choices):'
      write(ITEMS(16),'(2A)') '  ',qualifications_assessor(1:48)
      ITEMS(17) = 'j Insurer company: '
      write(ITEMS(18),'(2A)') '  ',insurer_Company(1:48)
      ITEMS(19) = 'k Insurance policy number: '
      write(ITEMS(20),'(2A)') '  ',policyNumberInsurance(1:48)
      ITEMS(21) = 'l Insurance policy start date: '
      write(ITEMS(22),'(A,I2,A,I2,A,I4)') '  ',S_Dinsur,'  ',
     &  S_Minsur,'  ',S_Yinsur
      ITEMS(23) = 'o Insurance policy expiry date: '
      write(ITEMS(24),'(A,I2,A,I2,A,I4)') '  ',E_Dinsur,'  ',
     &  E_Minsur,'  ',E_Yinsur
      ITEMS(25) = 'r Insurance policy cover limit: '
      write(ITEMS(26),'(A,I9)') '  ',pi_limit
      ITEMS(27) = 's Complexity level of the project: '
      write(ITEMS(28),'(2A)') '  ',pjLevComplexity(1:48)
      ITEMS(29) = '  _____________________________'
      ITEMS(30) = '? help                         '
      ITEMS(31) = '- exit this menu               '
      nitms=31
      WRITE(HEAD,'(A)')'Project details for BRUKL/EPC'

C Help text for this menu.
      H(1) ='This menu allows the definition of'
      H(2) ='project details. These will be used by'
      H(3) ='BRUKL/EPC for reporting purposes'
      H(4) =' '
      H(5) ='The Unisque Property Reference Number is applicable'
      H(6) ='in England, Wales & Northern Ireland.'
      H(7) ='<< help message is work-in-progress >> '

C Menu for energy assessor details.
      CALL EMENU(HEAD,ITEMS,nitms,INO)
      D64='<this item has not been described>'
      IF(INO.EQ.nitms-1)THEN

C Produce help text for the menu.
        CALL PHELPD(HEAD,6,'-',0,0,IER)
      ELSEIF(INO.EQ.1)THEN

C Unique Property Reference Number (UPRN)
C Leave it as a string because it is not certain if this is always an integer
C (it may be a mix of letters and numbers).
        D64='000000000000'
        D64='000000000000'
        H(1)='Input the Unique Property Reference Number (UPRN) here.'
        H(2)='Applicable only for England, Wales & Northern Ireland.'
        CALL EASKS(UPRN,'Unique Property Reference Number (UPRN)?',
     &    '(<64 characters)',64,D64,'UPR Number',IER,2)
        call tstamp('>','PRJ: edited UPRN')
        write(outs,'(2a)') 'Unique Property Reference Number (UPRN): ',
     &    UPRN(1:lnblnk(UPRN))
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.3)THEN

C Month and day of month of inspection.
        H(1)='Input the month and the day of the month of the '
        H(2)='inspection. Applicable only for England, '
        H(3)='Wales & Northern Ireland.'
        CALL EASKI(D_inspect,' ','Day of inspection?',
     &    1,'W',31,'W',1,'day',IERI,3)
        CALL EASKI(M_inspect,' ','Month of inspection?',
     &    1,'W',12,'W',1,'month',IERI,3)

C Year for which the energy assessor inspected the building.
        H(1)='Input the year of the inspection. Applicable '
        H(2)='only for England, Wales & Northern Ireland.'
        CALL EASKI(Y_inspect,' ','Year of inspection?',
     &    1900,'W',2080,'W',2009,'year',IERI,2)
        call tstamp('>','PRJ: edited date of inspection')
        write(outs,'(A,I2,A,I2,A,I4)') 'Inspection date: ',
     &    D_inspect,'  ',M_inspect,'  ',Y_inspect
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.5)THEN

C Accreditation scheme
        H(1)='Input certifier/energy assessor accreditation scheme.'
        H(2)='Applicable only for England, Wales & Northern Ireland.'
        H(3)='This is the scheme name from CIBSE or BRE. '
        CALL EASKS(Accr_Scheme,'Accreditation scheme ?',
     &    '(<64 characters)',64,D64,'accreditation scheme',IER,3)
        call tstamp('>','PRJ: edited accreditation scheme')
        write(outs,'(2a)') 'Accreditation Scheme (E,W and NI only): ',
     &    Accr_Scheme(1:lnblnk(Accr_Scheme))
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.7)THEN

C Assessor registration number
        H(1)='The assessor registration number should be in the'
        H(2)='form of CCCCNNNNNN where C is character and N is a'
        H(3)='number such as ABCD123456.'
        D64='ABCD123456'
        CALL EASKS(assessRegNumber,
     &    'Assessor regisration number (see help)?',
     &    '(e.g. ABCD123456)',64,D64,'assessor number',IER,3)
        call tstamp('>','PRJ: edited assessor registration number')
        D64='<this item has not been described>'
        write(outs,'(2a)')
     &    'Assessor registration number (E,W and NI only):',
     &     assessRegNumber(1:lnblnk(assessRegNumber))
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.9)THEN

C Employer or trading name of energy assessor
        D64='Not a registered company.'
        H(1)='The employer or the trading name of the energy'
        H(2)='assessor. Applicable only for England, '
        H(3)='Wales & Northern Ireland.'
        CALL EASKS(empl_Trading_name,'Employer/trading name ?',
     &    '(<64 characters)',64,D64,'trading name',IER,3)
        call tstamp('>','PRJ: edited employer/traing name')
        write(outs,'(2a)')
     &    'Employer/trading name of energy assessor: ',
     &    empl_Trading_name(1:lnblnk(empl_Trading_name))
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.11)THEN

C Employer/trading address of energy assessor
        D64='PO box in Basingstoke.'
        H(1)='The employer or the trading address of the energy'
        H(2)='assessor. Applicable only for England, '
        H(3)='Wales & Northern Ireland.'
        CALL EASKS(addr_empl_Trading,'Employer/trading address ?',
     &    '(<64 characters)',64,D64,'owner address',IER,3)
        call tstamp('>','PRJ: edited employer/trading address')
        write(outs,'(2a)')
     &    'Employer/trading name of energy assessor: ',
     &    empl_Trading_name(1:lnblnk(empl_Trading_name))
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.13)THEN

C Related party disclosure
        D64='No related party.'
        H(1)='Any related party disclosure by the energy'
        H(2)='assessor. Applicable only for England, '
        H(3)='Wales & Northern Ireland.'
        CALL EASKS(party_disclosure,'Party disclosure ?',
     &    '(<64 characters)',64,D64,'owner city',IER,3)
        call tstamp('>','PRJ: edited party disclosure')
        write(outs,'(2a)') 'Related party disclosure: ',
     &    party_disclosure(1:lnblnk(party_disclosure))
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.15)THEN

C Qualifications of the energy assessor (3 choices)
        H(1)='Qualifications of the energy assessor.'
        H(2)='Applicable only for England, Wales & '
        H(3)='Northern Ireland. There are 3 choices: '
        H(4)='NOS3, NOS4 and NOS5'
        CALL EASKABC(' ','Energy assessor qualifications:',
     &    'NOS3','NOS4','NOS5',IRT,4)
        if(IRT.eq.1)then
          write(qualifications_assessor,'(A)') 'NOS3'
        elseif(IRT.eq.2)then
          write(qualifications_assessor,'(A)') 'NOS4'
        elseif(IRT.eq.3)then
          write(qualifications_assessor,'(A)') 'NOS5'
        endif
        call tstamp('>','PRJ: edited qualifications')
        write(outs,'(2a)') 'Qualifications of energy assessor:',
     &    qualifications_assessor(1:lnblnk(qualifications_assessor))
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.17)THEN

C Name of the energy assessor's insurance company
        D64='Not insured.'
        H(1)='Name of the insurance company. Applicable '          
        H(2)='only for England, Wales & Northern Ireland.'
        CALL EASKS(insurer_Company,'Name of insurance company ?',
     &    '(<64 characters)',64,D64,'insurance company',IER,2)
        call tstamp('>','PRJ: edited insurer company name')
        write(outs,'(2a)') 'Insurance company: ',
     &    insurer_Company(1:lnblnk(insurer_Company))
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.19)THEN

C The energy assessor's insurance policy number
        D64='Not insured.'
        H(1)='Enter insurance policy number. Applicable '          
        H(2)='only for England, Wales & Northern Ireland.'
        CALL EASKS(policyNumberInsurance,'Policy number ?',
     &    '(<64 characters)',64,D64,'policy number',IER,2)
        call tstamp('>','PRJ: edited insurance policy number')
        write(outs,'(2a)') 'Insurance policy number: ',
     &    policyNumberInsurance(1:lnblnk(policyNumberInsurance))
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.21)THEN

C Insurance policy start date
        H(1)='Input the date for start of the insurance. '
        H(2)='Applicable only for England, Wales & Northern'
        H(3)='Ireland.'
        CALL EASKI(S_Dinsur,' ','Insurance start (day)?',
     &    1,'W',31,'W',1,'day',IERI,3)
        CALL EASKI(S_Minsur,' ','Insurance start (month)?',
     &    1,'W',12,'W',1,'month',IERI,3)
        CALL EASKI(S_Yinsur,' ','Insurance start (year)?',
     &    1900,'W',2080,'W',2009,'year',IERI,3)
        call tstamp('>','PRJ: edited start date of insurance')
        write(outs,'(A,I2,A,I2,A,I4)') 'Insurance policy start date: ',
     &    S_Dinsur,'  ',S_Minsur,'  ',S_Yinsur
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.23)THEN

C Insurance policy expiry date - year
        H(1)='Input the date for the expiry of the insurance. '
        H(2)='Applicable only for England, Wales & Northern'
        H(3)='Ireland.'
        CALL EASKI(E_Dinsur,' ','Insurance expiry (day)?',
     &    1,'W',31,'W',1,'day',IERI,3)
        CALL EASKI(E_Minsur,' ','Insurance expiry (month)?',
     &    1,'W',12,'W',1,'month',IERI,3)
        CALL EASKI(E_Yinsur,' ','Insurance expiry (year)?',
     &    1900,'W',2080,'W',2010,'year',IERI,3)
        call tstamp('>','PRJ: edited expiry date of insurance')
        write(outs,'(A,I2,A,I2,A,I4)')'Insurance policy expiry date: ',
     &    E_Dinsur,'  ',E_Minsur,'  ',E_Yinsur
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.25)THEN

C Insurance policy cover limit
        H(1)='Input the insurance policy cover limit. '
        H(2)='Applicable only for England, Wales & Northern'
        H(3)='Ireland.'
        CALL EASKI(pi_limit,' ','Insurance cover limit?',
     &    1,'W',999999999,'W',500000,'cover limit',IERI,3)
        call tstamp('>','PRJ: edited insurance cover limit')
        write(outs,'(A,I9)') 'Insurance policy cover limit: ',
     &    pi_limit
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.27)THEN

C Define the complexity level
        H(1)='Complexity level of the project. Applicable only for'
        H(2)='England, Wales & Northern Ireland.'
        H(3)='There are 4 choices: '
        H(4)=' Undefined - no opinion (yet)'
        H(5)=' Level 3 less than 100kW heating & less than 12kW cooling'
        H(6)=' Level 4 more than 100kW heating & 12kW cooling'
        H(7)=' Level 5 building cannot be done via sBEM.'
        CALL EASKABCD(' ','Complexity level ?',
     &    'Undefined','Level 3','Level 4','Level 5',IW,7)
        if(IW.eq.1)then
          write(pjLevComplexity,'(A)') 'Undefined'
        elseif(IW.eq.2)then
          write(pjLevComplexity,'(A)') 'Level 3'
        elseif(IW.eq.3)then
          write(pjLevComplexity,'(A)') 'Level 4'
        elseif(IW.eq.4)then
          write(pjLevComplexity,'(A)') 'Level 5'
        endif
        call tstamp('>','PRJ: edited complexity level')
        write(outs,'(2a)') 'Complexity level of the project: ',
     &    pjLevComplexity(1:lnblnk(pjLevComplexity))
        call edisp(iuout,outs)
      ENDIF
      IF(INO.NE.nitms)GOTO 55
      return
      end

C ********************* EDSBMREGS
C EDSBMREGS edits the building regs definition within the UK NCM file.

      subroutine edsbmregs()
#include "building.h"
#include "geometry.h"
#include "sbem.h"
#include "epara.h"

      common/OUTIN/IUOUT,IUIN
      integer iuout,iuin
      common/pophelp/h(60)
      character h*72
      integer ncomp,ncon
      common/C1/NCOMP,NCON

      character KEY*1,outs*124
      character*35 ZITEMS(MCOM+4)
      CHARACTER*46 CIBSESTR2(4) ! concatenation of no. floors and area
      DIMENSION JSTR(8)
      dimension IVALSS(29)       ! array to get epicks answer from
      dimension iedit(MCOM) !to use for menu and saving of air leakage (ach per zone)
      integer iedit
      integer nitms,INO ! max items and current menu item
      logical NEAR
      real Air_infilt

C Echo the current state of regulations.
      call lstncm('b',iuout)

C Enter building regulations to be used. Clear array signaling
c individual editing of infiltration.
      do 230 IZ=1,NCOMP
        iedit(iz)=0
 230  continue

      IX=1
      JSTR(1)=IRGG
 902  CALL EPICKS(IX,JSTR,' ','Define building regulations to use',
     &  50,MREG,BLDREG,'Define building services strategy',IER,2)
      IF(JSTR(1).EQ.0)GOTO 902
      if(JSTR(1).LE.4.AND.JSTR(1).GE.1)then
        IRGG=JSTR(1)
        write(outs,'(2a)') 'Selected ',
     &    BLDREG(IRGG)(1:LNBLNK(BLDREG(IRGG)))
        call edisp(iuout,outs)
      endif

C Enter bulding design stage
      IX=1
      JSTR(1)=ISTG
 903  CALL EPICKS(IX,JSTR,' ','Define building regulations to use',
     &  12,MSTG,BLDSTG,'Define building services strategy',IER,2)
      IF(JSTR(1).EQ.0)GOTO 903
      if(JSTR(1).LE.2.AND.JSTR(1).GE.1)then
        ISTG=JSTR(1)
        write(outs,'(2a)') 'Selected ',
     &    BLDSTG(ISTG)(1:LNBLNK(BLDSTG(ISTG)))
        call edisp(iuout,outs)
      endif

C Enter Scottish building types if Scottish regs to be used
      IF(IRGG.EQ.2)THEN
        IX=1
        JSTR(1)=ISBT
 904    CALL EPICKS(IX,JSTR,' ','Define building type',
     &    40,MSBT,SBTYP,'Define building type',IER,2)
        IF(JSTR(1).EQ.0)GOTO 904
        IF(JSTR(1).LE.2.AND.JSTR(1).GE.1)ISBT=JSTR(1)
        CALL EASKABC('Is the building built/designed following',
     &    'guidance in Accredited Construction Details (Scotland)',
     &    'yes','no','Do not know',IW,2)
        IF(IW.EQ.1)THEN
          SBREF='YES'
        ELSE
          SBREF='NO '
        ENDIF
      ELSE
        ISBT=0
      ENDIF

C Enter building permeability
      CALL EASKR(BINF50,
     &  'Enter building permeability at 50Pa in m3/m2.hour',
     &  ' ',0.,'F',0.,'-',10.,'heat gen eff ',IER,2)
      CALL EASKABC(
     &  'Is a compliance check to be performed on the building',
     &  'regarding air permeability value?',
     &  'yes','no','Do not know',IW,2)
      IF(IW.EQ.2)THEN
        APCHK='NO '
      ELSE
        APCHK='YES'
      ENDIF

C Use CIBSE data until SBEM approach to prediction of infiltration is
C communicated by AECOM
c      CALL EASKABC(
c     &  'Infer air changes per hour from permeability',
c     &  'value using CIBSE data',
c     &  'yes','no','Do not know',IW,2)
      IW=1
 4252 IF(IW.EQ.1)THEN
        ICIBSE1=1 ; ICIBSE2=1 ; ICIBSE3=1 ! initialise
        CALL EPICKS(IX,JSTR,' ','CIBSE building selection',
     &  54,8,CIBSESTR1,'Pick building that best matches model',IER,2)
        ICIBSE1=JSTR(1)
        DO 134 IBT=1,4
          IF(ICIBSESTOR(ICIBSE1,IBT).EQ.0)THEN
            WRITE(CIBSESTR2(IBT),'(A,1I6)')
     &      'Area per floor(m2) of ',ICIBSEAREA(ICIBSE1,IBT)
          ELSEIF(ICIBSEAREA(ICIBSE1,IBT).EQ.0)THEN
            WRITE(CIBSESTR2(IBT),'(1I4,A)')
     &      ICIBSESTOR(ICIBSE1,IBT),' storeys '
          ELSE
            WRITE(CIBSESTR2(IBT),'(1I4,A,1I6)')
     &      ICIBSESTOR(ICIBSE1,IBT),' storeys & area per floor(m2) of ',
     &      ICIBSEAREA(ICIBSE1,IBT)
          ENDIF
 134    CONTINUE
        CALL EPICKS(IX,JSTR,'Pick storeys and floor area ',
     &  'per floor that best matches model',
     &  46,4,CIBSESTR2,'Pick closest storeys and/or floor area',IER,2)
        ICIBSE2=JSTR(1)

C Select CIBSE table value that best fits user supplied permeability value
C Do not worry too much about comparison between reals because table
C values are approximations
        IF(BINF50.LT.4.0)THEN      ! nominal permeability value of 3
          ICIBSE3=5
        ELSEIF(BINF50.LT.6.5)THEN  ! nominal permeability value of 5
          ICIBSE3=4
        ELSEIF(BINF50.LT.8.5)THEN  ! nominal permeability value of 7
          ICIBSE3=3
        ELSEIF(BINF50.LT.15.0)THEN ! nominal permeability value of 10
          ICIBSE3=2
        ELSE                       ! nominal permeability value of 20
          ICIBSE3=1
        ENDIF
        do 2331 icomp=1,ncomp
          ach(icomp)=CIBSEACH(ICIBSE1,ICIBSE2,ICIBSE3)
          write(outs,'(A,F6.4,2A)')'Air change rate of ',ACH(1),
     &    ' ac/h selected from CIBSE Guide A tables for zone ',
     &    zname(icomp)
          call edisp(iuout,outs)
 2331   continue
      ELSE

C Approximate zone infiltration based on volume unless
C the user already edited the value.
      do 231 IZ=1,NCOMP
        CALL ECLOSE(ACH(IZ),0.,0.001,NEAR)
        if(iedit(iz).ne.1.OR.NEAR)then
          if(VOL(IZ).gt.0.)then
            ACH(IZ)=(10.0*ZBASEA(IZ)/VOL(IZ))/20.0
          else
            ACH(IZ)=0.0
            write(outs,'(2A)')
     &     'Volume of 0 or negative found in zone: ',zname(IZ)
              call edisp(itu,outs)
          endif
        endif
 231  continue
      ENDIF

C Echo back the current state of building regs.
      call lstncm('b',iuout)

C Enter air changes per hour for every zone as air leakage
C This will be added to the ventilation rates taken from the tdf (as
C minimum fresh air requirements from the activities database)
C Begin with high level menu.
      MHEAD=0
      MCTL=3
      ILEN=NCOMP
      IPACT=CREATE
      call EKPAGE(IPACT)
    3 INO=-4
      IER=0
      M=MHEAD
      do 10 L=1,ILEN
        if(L.GE.IST.AND.(L.LT.(IST+MIFULL)))then
          M=M+1
          call EMKEY(L,KEY,IER)
          write(ZITEMS(M),'(A1,1X,A,1X,F6.4)')KEY,
     &       zname(L)(1:lnzname(L)),ACH(L)
        endif
   10 continue

C If a long list include page facility text and info on portion seen.
      IF(IPFLG.EQ.0)THEN
        ZITEMS(M+1)=  '  ______________________'
      ELSE
        WRITE(ZITEMS(M+1),155)IPM,MPM
  155   FORMAT ('0 page: ',I1,' -- of:',I1)
      ENDIF
      ZITEMS(M+2)= '? help                           '
      ZITEMS(M+3)= '- exit this menu                 '
      nitms=M+3

C Help text for this menu.
      H(1)='Air leakage rates can be specified for'
      H(2)='for every zone in terms of air changes per hour. '

      CALL EMENU('zone air changes per hour',ZITEMS,nitms,INO)
      IF(INO.EQ.nitms)THEN
        continue
      ELSEIF(INO.EQ.nitms-1)THEN

C Produce help text for the menu.
        CALL PHELPD('AIR LEAKAGE',2,'-',0,0,IER)
        goto 3
      ELSEIF(INO.EQ.nitms-2)THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
          goto 3
        ENDIF

      ELSEIF(INO.GE.1.AND.INO.LE.nitms-3)THEN
        Air_infilt=ACH(INO)
        CALL EASKR(Air_infilt,' ',
     &   'Air changes per hour for this zone?',0.0,'W',30.0,'W',0.0,
     &   'air leakage for the zone',IER,0)
        ACH(INO)=Air_infilt
        iedit(ino)=1
        goto 3
      ELSE
        goto 3
      ENDIF

C Use existing building type if non-zero.
      call lstncm('b',iuout)
      h(1)='For the UK National Calculation Method you are required'
      h(2)='to select one of these building types. '
      h(3)=' '
      h(4)='Depending on the type of building the notional model will'
      h(5)='have different assumptions about glazing areas. The '
      h(6)='building type will also help in the selection of room '
      h(7)='use patterns. '
      IX=1
 905  CALL EPICKS(IX,IVALSS,' ','Building type:',
     &  42,29,BTYPNAME,'building types',IER,7)
      IF(IVALSS(1).EQ.0)GOTO 905
      if(IVALSS(1).gt.0)then
        ibusertyp=IVALSS(1)
        ibtyp = IBTYPNDX(ibusertyp)

C Establish glazing fractions based on type of building
        IF(IBTYP.EQ.1)THEN
          ROOFFR=0.2
          WALLFR=0.3
        ELSEIF(IBTYP.EQ.2)THEN
          ROOFFR=0.2
          WALLFR=0.4
        ELSEIF(IBTYP.EQ.3)THEN
          ROOFFR=0.2
          WALLFR=0.15
        ENDIF
        ROOFFRFR=0.30
        WALLFRFR=0.10

C After setting up echo back what was selected.
        call lstncm('b',iuout)
      endif
      return
      end

C ********************* EMKSTRIPPED
C EMKSTRIPPED is the first pass for creating stripped model 
C after all of the NCM details have been defined. Initially part of the
C fall-though from choice 14 in sbmedit ('Proceed with setting up
C required models ') which returned iuact=1.
      subroutine EMKSTRIPPED(iuact,ier)
#include "building.h"
#include "geometry.h"
#include "sbem.h"
#include "tdf2.h"
#include "control.h"
#include "model.h"
#include "espriou.h"

      integer lnblnk  ! function definition

      common/FILEP/IFIL
      INTEGER :: ifil
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: iuout,iuin
      common/pophelp/h(60)
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      common/C21/IFCFG,cfgroot,LCFGF
      INTEGER :: IFCFG
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY6/LINSTY(MCON)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

C TDF related.
      COMMON/TDFIO/LIMP,IUIMP,ITIMP,LEXP,IUEXP,ITEXP
      COMMON/TDAT/TABU(MTABC),TABT(MTABC),TABUMN(MTABC),TABUMX(MTABC)

C Need to write TDF info in the cfg file. Use next common(s) for this.
      COMMON/TDFFLG0/DBTAG(MIT),DBTASK(MIT),DBZN(MIT),DBSN(MIT)
      COMMON/HDAT/IHREC(MIT)

      common/sctl/tcps,ictyp,iclaw,cm(misc)
      common/cctl/icascf(mcom)
      common/cctlnm/ctldoc,lctlf
      character ctldoc*248,LCTLF*72

      integer nbrdg, ibrdg
      real psi,lngth,losspercent,totheatloss,thbrpercent
      real uavtotal
      common/THRBRDG/nbrdg(MCOM),psi(MCOM,16),lngth(MCOM,16),
     &               ibrdg(MCOM,16),losspercent(MCOM),totheatloss(MCOM),
     &               thbrpercent,uavtotal(MCOM)

      CHARACTER DFILE*72,LIMP*72,LEXP*72
      character DBTAG*12,DBTASK*12,DBZN*15,DBSN*15

C Where ESP-r was installed (as recorded when it was compiled).
      common/deflt4/dinstpath
      character dinstpath*48
      character subpath*72
      CHARACTER OUTSTR*124

      CHARACTER ACT*1,ADF*124
      character cfgroot*24,LCFGF*72,NNAME*72
      character fs*1,msgl2*48,tab*1
      character outs*124,h*72,GUESS*24

      LOGICAL NEWGEO,MODIFY,MODLEN,MODBND
      logical unixok,XST
      logical ctl_Zonelink_match !This si to allow the sorted activity indices to be matched with
                                 !the original ones. The zone numbers can then be used to link the
                                 !controls to zones.
      character APE*6,EXT*4
      character delim*1
      character louts*400        ! buffer of 400 char to support larger models
      real CDAY,RDOTY            ! reals for timestamp calculations
      integer itime,ip           ! integers for timestamp calculations
      integer iafil              ! file unit for writing temporal header
      integer icolumns           ! number of colums for tdf file depending on the unique non-zero activities

      integer inxt_column !the next available column in the tdf file
      DIMENSION iactiv_tdf_item(MCOM)
      DIMENSION iforuseintdfentry(MCOM)
      DIMENSION iactiv_tdf_items(MCOM)
      integer iactiv_tdf_item
      integer iforuseintdfentry
      integer icountnotequal  ! the final unique and non-zero number of activities that represent the number of entries
                              ! in the tdf file.
      integer iactiv_tdf_items
      integer iarraycount !used to fill in the arrays and call arlist
      integer iskipandmatch ! counter to skip common unsorted activities. See comments in the code for additional info.
      integer i2skipandmatch !second counter in case iactiv_tdf_item(IZ).eq.theactivityindex(iCTLlink)
      logical isnotequal     ! to avoid range error on logic test

C For tdf to call arlist:
      DIMENSION COLUMNTDF(MTABC,8760)
      REAL COLUMNTDF
      DIMENSION COLUMNWRITE(MTABC)
      REAL COLUMNWRITE
      integer iloc1,iloc2,iloc3,iloc4,iloc5  !for writing tdf location data in the file
      REAL val_converted
      real zd  ! depth

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      tab=','
      cfgroot=cfgroot_s  ! set to stripped

C Proceed with setting up the stripped model. 
C First get ground temperatures at depth Z
      ZD=2.0
      CALL GTCALC(ZD,'s',IER)

C Assume that geometry files are version 1.1. If not exit with ier set to 2.
      APE='_str  '
      call edisp(iuout,
     &  'Processing zones for stripped building variant.')
      GUESS='notional'
      DO 551 ICOMP=1,NCOMP
        IUF=IFIL+2
        CALL ECLOSE(GVERSION(ICOMP),1.1,0.01,NEWGEO)
        IF(NEWGEO)THEN
          CALL GEOREAD(IUF,LGEOM(ICOMP),ICOMP,1,IUOUT,IER)
        ELSE
          call usrmsg('Older geometry file cannot be used.',
     &      LGEOM(ICOMP),'W')
          ier=2
          return
        ENDIF
        ACT='s'

C Code to loop through each surface to build a hash table so that each
C surface also knowns its children
        CALL SURREL(ACT,ICOMP,IER)

C For each surface in this zone
        DO 552 ISUR=1,NZSUR(ICOMP)
          ICN=IZSTOCN(ICOMP,ISUR)

C If surface is ground then specify ground temperature profile as
C calculated in GTCALC
          IF(SSOTHER(ICN,1)(1:8).EQ.'GROUND')THEN
            SSOTHER(ICN,2)='00'
            SSOTHER(ICN,3)='01'
            IC2(ICN)=0
            IE2(ICN)=1
          ENDIF

C If surface is exterior then check if use is specified if not then ask
          IF(SSOTHER(ICN,1)(1:8).EQ.'EXTERIOR')THEN
            ADF=SSUSE(ICN,1)
            IF(ADF(1:1).EQ.'-')THEN
              IZGFOC=ICOMP
              MODIFY=.TRUE.
              CALL INLNST(1)
              LINSTY(ICN)=2
              CALL ADJVIEW(IER)
              WRITE(OUTS,'(4A)')'Enter usage for surface ',SSNAME(ICN),
     &          ' in zone ',zname(icomp)(1:lnzname(icomp))
              CALL EDISP(IUOUT,' ')
              CALL EDISP(IUOUT,OUTS)
              CALL EDISP(IUOUT,' ')
              CALL EDITUSE(ICOMP,ISUR,ICN,GUESS)
            ENDIF
          ENDIF
 552    CONTINUE
        EXT='.geo'
        CALL FNCNGR(LGEOM(ICOMP),APE,EXT,NNAME)
        LGEOM(ICOMP)=NNAME

C Add 10% thermal bridge by silently calling appropriate subroutine if
c none have been defined
        if(losspercent(icomp).le.0.0)CALL LINTHBRDG(ICOMP,'s',0.1)

        CALL GEOWRITE(IUF,LGEOM(ICOMP),ICOMP,ITRU,3,IER)
 551  CONTINUE

C Save information to NCM project specific (*.ncm) file
C Also write out version 1.1 of ncm file for testing.
      call mksbem
C      call mksbem2

C If there is no temporal file generate initial file contents over
C the whole year at one tsph and with one activity item.
      call edisp(iuout,'Generating occupancy patterns.')
      if (LTDFA(1:4).eq.'UNKN'.or.LTDFA(1:2).eq.'  ') then
        XST=.false.

C Default name for temporal file.
        if(netpth(1:2).eq.'  '.or.netpth(1:2).eq.'./')then
          WRITE(LTDFA,'(2a)')cfgroot(1:lnblnk(cfgroot)),
     &      '_temporal.tdfa'
        else
          WRITE(LTDFA,'(4a)') netpth(1:lnblnk(netpth)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'_temporal.tdfa'
        endif
      else
        call FINDFIL(LTDFA,XST)  ! using model path
      endif
      if(XST)then

C There is an existing temporal file, scan it. NOTE: no checks
C yet made that it is hourly and full year.
        call isunix(unixok)
        IER=0
        call supplyandcheck(LTDFA,'T',ier)
        if(ier.ne.0.and.unixok)then
          write(6,*) 'error during supplyandcheck'
        endif
      endif
      if(ISBEM.EQ.2)THEN
        icountnotequal=0
        DO 623 IZ=1,NCOMP
          iactiv_tdf_item(IZ)=0
          iactiv_tdf_items(IZ)=0
          iforuseintdfentry(IZ)=0
          iactiv_tdf_item(IZ)=theactivityindex(IZ)
  623   CONTINUE

C Set entries in ascending order.
C There is more than one zone so sort the activities so that
C controls can be created.
        KFLAG=2
        call SORTI(iactiv_tdf_item,iactiv_tdf_items,NCOMP,KFLAG)
        iskipandmatch=1
        DO 622 IZ=1,NCOMP
          isnotequal=.false.
          ctl_Zonelink_match=.false.
          if(IZ.eq.1)then
            isnotequal=.true.
          elseif(IZ.gt.1)then
            if(iactiv_tdf_item(IZ).ne.iactiv_tdf_item(IZ-1))then
              isnotequal=.true.

C Debug.
C              if(unixok)then
C                write(6,*)'iz: ',iactiv_tdf_item(IZ),'iz-1: ',
C     &                 iactiv_tdf_item(IZ-1)
C              endif
            endif
          endif
          if(isnotequal)then

C Reset the counter iskipandmatch to use for the opposite case (i.e.
C where iactiv_tdf_item(IZ).eq.iactiv_tdf_item(IZ-1)
C See comments at the place where iskipandmatch is used for more details
C about this counter
            iskipandmatch=1
            icountnotequal=icountnotequal+1
            iforuseintdfentry(icountnotequal)=iactiv_tdf_item(IZ)
C Debug.
C            write(6,*)'a:', icountnotequal

C Control items
C Need a counter here to skip common non-sorted activities and match the right one
C Example: A building with sorted activity indices of: 92,92,92,155,196
C in needed to identify the zone number where this activities belong to in order
C to connect the control loops. Let's say: 2,4,5,3,1 respectively for this example.
C Unsorted this activities would have been: 155,92,196,92,92 and the zone numbers
C would obviously be 1,2,3,4,5 respectively.
            iCTLlink=1
            do while(.not.ctl_Zonelink_match.and.iCTLlink.le.ncomp)
              if(iactiv_tdf_item(IZ).eq.
     &            theactivityindex(iCTLlink))then
                ctl_Zonelink_match=.true.
              else
                iCTLlink=iCTLlink+1
              endif
            end do

C For an ACTIVITY type of item the heating setpoint will
C always be the 5th column and the cooling setpoint will
C always be the 6th colum so set cm(2) and cm(3) with
C fixed values. Later when the temporal data is scanned
C the returned array of values for the current ACTIVITY item
C will have the heating and cooling at this offset.
            ibsn(icountnotequal,1)=-2 !0
            ibsn(icountnotequal,2)=0
            ibsn(icountnotequal,3)=50 !0
            ibsn(icountnotequal,4)=0
            val_converted=REAL(icountnotequal)
            cm(2)=5.0*val_converted
            cm(3)=5.0*val_converted+1.
            iban(icountnotequal,1)=0
            iban(icountnotequal,2)=0
            iban(icountnotequal,3)=0
            nbcdt(icountnotequal)=1
            ibcdv(icountnotequal,1,1)=1
            ibcdv(icountnotequal,1,2)=365
            nbcdp(icountnotequal,1)=1
            tbcps(icountnotequal,1,1)=0.0
            ibctyp(icountnotequal,1,1)=0
            ibclaw(icountnotequal,1,1)=23
            cm(1)=2.0

C Update the control period data.
            bmiscd(icountnotequal,1,1,1)=cm(1)
            bmiscd(icountnotequal,1,1,2)=cm(2)
            bmiscd(icountnotequal,1,1,3)=cm(3)

C Update the control file and link the control loop to the correct zone.
            icascf(iCTLlink)=icountnotequal
          elseif(iactiv_tdf_item(IZ).eq.iactiv_tdf_item(IZ-1))then
            iskipandmatch=iskipandmatch+1

C Debug.
C            write(6,*)'iz_b: ',iactiv_tdf_item(IZ),'iz_b-1: ',
C     &                 iactiv_tdf_item(IZ-1)
C            write(6,*)'b:', icountnotequal
            i2skipandmatch=0
            iCTLlink=1
            do while(.not.ctl_Zonelink_match.and.iCTLlink.le.ncomp)
              if(iactiv_tdf_item(IZ).eq.
     &           theactivityindex(iCTLlink))then

C Need a counter here to skip common non-sorted activities and match the right one
C Example: A building with sorted activity indices of: 92,92,92,155,196
C in needed to identify the zone number where this activities belong to in order
C to connect the control loops. Let's say: 2,4,5,3,1 respectively for this example.
C Unsorted this activities would have been: 155,92,196,92,92 and the zone numbers
C would obviously be 1,2,3,4,5 respectively. The first loop (IZ here) goes through the
C sorted indices and then the second loop (iCTLlink here) goes through the unsorted.
C In this second case here where iactiv_tdf_item(IZ).eq.iactiv_tdf_item(IZ-1)
C for example 92,92... the logic for the loop through the unsorted activities needs to
C know if the activity has already been scanned and skip it in order to take the right
C zone number and use for the control loops. This is why the counter iskipandmatch is
C used here.
C i2skipandmatch will be used as a second counter in case
C iactiv_tdf_item(IZ).eq.theactivityindex(iCTLlink)
C This whole thing is done to determine which control function links to which zone
                i2skipandmatch=i2skipandmatch+1
                if(iskipandmatch.eq.i2skipandmatch)then
                  ctl_Zonelink_match=.true.
                else
                  ctl_Zonelink_match=.false.
                endif
                iCTLlink=iCTLlink+1
              else
                iCTLlink=iCTLlink+1
              endif
            end do

C Update the control file and link the control loop to the correct zone.
C Using iCTLlink-1 because 1 has been added to iCTLlink when
C the do while loop finished.
            icascf(iCTLlink-1)=icountnotequal
          endif
  622   CONTINUE
        call edisp(iuout,'Generating zone controls.')
        iloc1=0
        iloc2=0
        iloc3=0
        iloc4=0
        iloc5=0
        znctldoc='Specific controls for UK NCM activities '
        ncf=icountnotequal

C Save control file.
        if(LCTLF(1:2).eq.'  '.or.LCTLF(1:4).eq.'UNKN')then
          LN=max(1,LNBLNK(cfgroot))
          if(ctlpth(1:2).eq.'  '.or.ctlpth(1:2).eq.'./')then
            WRITE(LCTLF,'(2a)')cfgroot(1:ln),'.ctl'
          else
            WRITE(LCTLF,'(4a)') ctlpth(1:lnblnk(ctlpth)),fs,
     &           cfgroot(1:ln),'.ctl'
          endif
        endif
        ICTLF=IFIL+1
        call usrmsg('Updating model control for temporal link...',
     &    ' ','-')

C Write control file (filename.ctl is saved to filename_str.ctl)
        EXT='.ctl'
        CALL FNCNGR(LCTLF,APE,EXT,NNAME)
        LCTLF=NNAME
        XST=.FALSE.
        call FINDFIL(LCTLF,XST)  ! using model path
        IF(XST)THEN
          H(1) = ' Control file exists. Choose to overwrite in '
          H(2) = ' question that will follow '
          CALL PHELPD('File exists',2,'-',0,0,IER)
        ENDIF
        CALL CTLWRT(ICTLF,IER)

C Write configuration file for stripped file (filename.ctl is
C saved to filename_str.ctl)
        call edisp(iuout,'Generating stripped model configuration.')
        LCFGF=LCFGF_S

C Variant connections file also required set up its name and
C then use emkcfg to create it.
        APE='_str'
        EXT='.cnn'
        CALL FNCNGR(LCNN,APE,EXT,NNAME)
        LCNN=NNAME
        CALL EMKCFG('-',IER)

C Write out the temporal header as well as hourly data for the
C first temporal item.
        XST=.FALSE.
        call findfil(LTDFA,XST)
        IF(XST)THEN
          H(1)='Temporal data file exists. Choose to overwrite in '
          H(2)='question that will follow '
          CALL PHELPD('File exists',2,'-',0,0,IER)
        ENDIF
        iafil=IUTDFA
        ITDFLG=3
        itdyear=iyear
        CALL ERPFREE(iafil,ISTAT)
        CALL EFOPSEQ(iafil,LTDFA,4,IER)
        if(ier.eq.0)THEN
          write(currentfile,'(a)') LTDFA(1:lnblnk(LTDFA))
        ELSE ! WARNING MESSAGE HERE
        ENDIF
        call edisp(iuout,'Generating hourly room use data.')
        WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) 'ASCIITDF3'
        WRITE(iafil,'(a)',IOSTAT=ios,ERR=101)
     &    '# NWPR NITDF NTSPH itdyear,itdbdoy,itdedoy,columns'
        if(icountnotequal.gt.1)then

C write 5 columns per unique activity entry
          icolumns=5*icountnotequal
          WRITE(iafil,'(a,i2,a,i5,a,i3)',IOSTAT=ios,ERR=101)
     &    '   20   ',icountnotequal,'    1',itdyear,'    1  365',
     &    icolumns
          WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) '# NEXTRC,NEXTCL,NDBSTP'
          inxt_column=icolumns+1
          WRITE(iafil,'(a,i3,a)',IOSTAT=ios,ERR=101) '      1   ',
     &         inxt_column,' 8760'
        else
          icolumns=5
          WRITE(iafil,'(a,i5,a)',IOSTAT=ios,ERR=101)
     &    '   20    1    1',itdyear,'    1  365    5'
          WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) '# NEXTRC,NEXTCL,NDBSTP'
          WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) '      1      1   8760'
        endif
        WRITE(iafil,'(a)',IOSTAT=ios,ERR=101)
     &    '*tdaid1,Set set of UK NCM occupancy patterns'
        WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) '*tdaid2,-'
        DO 1 i=1,icountnotequal

C Get first the location of the columns to use the right data during simulation.
C Instanciate ITCOL for use later on in the subroutine.
          iloc1=5*(i-1)+1
          ITCOL(i,12)=iloc1
          iloc2=5*(i-1)+2
          ITCOL(i,13)=iloc2
          iloc3=5*(i-1)+3
          ITCOL(i,14)=iloc3
          iloc4=5*(i-1)+4
          ITCOL(i,15)=iloc4
          iloc5=5*(i-1)+5
          ITCOL(i,16)=iloc5

C Write the header of the ASCII tdf file.
          WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) '*items'
          WRITE(iafil,'(2a,i4)',IOSTAT=ios,ERR=101) '*tag',tab,
     &          iforuseintdfentry(i)
          WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) '*type,ACTIVITY'
          WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) '*menu,Activity Type:'
          WRITE(iafil,'(a)',IOSTAT=ios,ERR=101)
     &      '*aide,Activity db (DOE/SBEM)'

C NTBITS and NTSTAT are hardwired here to 5 and 11 respectively
          WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) '*other,  11  5'
          WRITE(iafil,'(2a)',IOSTAT=ios,ERR=101) '*fields,16'
          WRITE(iafil,'(a,F4.3,2a)',IOSTAT=ios,ERR=101)
     &      'REAL  1  0   ',occupant_dens(iforuseintdfentry(i)),
     &      ' 0.000    100.000  ','Occupant density (pers/m2):'
          WRITE(iafil,'(a,I3,2a)',IOSTAT=ios,ERR=101)
     &      'INTG  2  0   ',metabolic_rate(iforuseintdfentry(i)),
     &      ' 0       999  ','Metabolic rate (W/pers):'
          WRITE(iafil,'(a,I3,2a)',IOSTAT=ios,ERR=101)
     &      'INTG  3  0   ',latent_ocup_percent(iforuseintdfentry(i)),
     &      ' 0       100  ','Occup. Latent gain % (0-100):'
          WRITE(iafil,'(a,F6.3,2a)',IOSTAT=ios,ERR=101)
     &      'REAL  4  0    ',equip_gain(iforuseintdfentry(i)),
     &      ' 0.000    400.000  ','Equipment (W/m2):'
          WRITE(iafil,'(1a,I3,2a)',IOSTAT=ios,ERR=101)
     &      'INTG  5  0    ',latent_equip_percent(iforuseintdfentry(i)),
     &      ' 0       100  ','Equip. Latent gain % (0-100):'
          WRITE(iafil,'(a,I3,2a)',IOSTAT=ios,ERR=101)
     &      'INTG  6  0    ',lighting_lux(iforuseintdfentry(i)),
     &      ' 0       999  ','Number of luxes (lux):'
          WRITE(iafil,'(a,F6.3,2a)',IOSTAT=ios,ERR=101)
     &      'REAL  7  0    ',display_lighting(iforuseintdfentry(i)),
     &      ' 0.000    400.000  ','Display Lighting (W/m2):'
          WRITE(iafil,'(a,F7.2,2a)',IOSTAT=ios,ERR=101)
     &      'REAL  8  0   ',dhw_litres(iforuseintdfentry(i)),
     &      ' 0.000    400.000  ','Dom. Hot Water (l/d/m2):'
          WRITE(iafil,'(a,F6.3,2a)',IOSTAT=ios,ERR=101)
     &      'REAL  9  0    ',fresh_air(iforuseintdfentry(i)),
     &      ' 0.000    400.000  ','Outdoor air (l/s/pers.):'
          WRITE(iafil,'(2a)',IOSTAT=ios,ERR=101)
     &      'INTG 10  0         0         0       100  ',
     &      'Min Humidity levels (%): '
          WRITE(iafil,'(2a)',IOSTAT=ios,ERR=101)
     &      'INTG 11  0         0         0       100  ',
     &      'Max Humidity levels (%): '
          WRITE(iafil,'(a,i3,2a)',IOSTAT=ios,ERR=101)
     &      'REAL 12 ',iloc1,'   0.000      0.000      1.000  ',
     &      'Occupant fraction:'
          WRITE(iafil,'(a,i3,2a)',IOSTAT=ios,ERR=101)
     &      'REAL 13 ',iloc2,'   0.000      0.000      1.000  ',
     &      'Lights fraction:'
          WRITE(iafil,'(a,i3,2a)',IOSTAT=ios,ERR=101)
     &      'REAL 14 ',iloc3,'   0.000      0.000      1.000  ',
     &      'Equipment fraction:'
          WRITE(iafil,'(a,i3,F7.2,2a)',IOSTAT=ios,ERR=101)
     &      'REAL 15 ',iloc4,Hmainsetpoint(iforuseintdfentry(i)),
     &      '   -101.000    101.000  ','Heating setpoint (C):'
          WRITE(iafil,'(a,i3,F7.2,2a)',IOSTAT=ios,ERR=101)
     &      'REAL 16 ',iloc5,Cmainsetpoint(iforuseintdfentry(i)),
     &      '   -101.000    101.000  ','Cooling setpoint (C):'
          WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) '*end_item'
 1      CONTINUE
        WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) '*pointers'

C Need to predict the next value of ihrec. Do this by setting irec
C to the next position after the last item header. If the first
C item then the position will be 5.
        if(icountnotequal.gt.1)then

C It will always be NTBITS=5 here and NTSTAT=11 for NCM activities
          do 852 ibits_and_stats=1,icountnotequal
            NTBITS(ibits_and_stats)=5
            NTSTAT(ibits_and_stats)=11
  852     continue
          ihrec(1)=5
          do 963 iwrpoint=2,icountnotequal
            ihrec(iwrpoint)=ihrec(iwrpoint-1)+
     &            NTBITS(iwrpoint-1)+NTSTAT(iwrpoint-1)+1
  963     continue
          itrunc=1
          ipos=1
          delim='C'
          do while (itrunc.ne.0)
            call ailist(ipos,icountnotequal,IHREC,MIT,delim,louts,
     &           loutln,itrunc)
            WRITE(iafil,'(1x,a)',IOSTAT=ios,ERR=101) louts(1:loutln)
            ipos=itrunc+1
          end do
        else
          ihrec(1)=5
          WRITE(iafil,'(1x,i3)',IOSTAT=ios,ERR=101) ihrec(1)
        endif
        WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) '*tabular_data'
        WRITE(iafil,'(2a)',IOSTAT=ios,ERR=101)
     &    '# Time Col 1 Col 2 Col 3 Col 4 Col 5 Col 6 Col 7 Col 8...'
        iarraycount=0
        DO 2 iimport=1,icountnotequal
          if(iforuseintdfentry(iimport).ge.1.and.
     &       iforuseintdfentry(iimport).lt.10)then
            write(subpath,'(6a,I1,a)',iostat=ios,err=212)
     &         dinstpath(1:lnblnk(dinstpath)),fs,'databases',fs,
     &        'UK_NCM',fs,iforuseintdfentry(iimport),'.csv'
          elseif(iforuseintdfentry(iimport).ge.10.and.
     &           iforuseintdfentry(iimport).lt.100)then
            write(subpath,'(6a,I2,a)',iostat=ios,err=212)
     &         dinstpath(1:lnblnk(dinstpath)),fs,'databases',fs,
     &        'UK_NCM',fs,iforuseintdfentry(iimport),'.csv'
          elseif(iforuseintdfentry(iimport).ge.100.and.
     &           iforuseintdfentry(iimport).lt.1000)then
            write(subpath,'(6a,I3,a)',iostat=ios,err=212)
     &         dinstpath(1:lnblnk(dinstpath)),fs,'databases',fs,
     &        'UK_NCM',fs,iforuseintdfentry(iimport),'.csv'
          endif
          write(DFILE,'(a)')subpath(1:lnblnk(subpath))

C << Improve this part in the future >>
          LIMP=DFILE
          IF(ITIMP.EQ.1)THEN
            CALL ERPFREE(IUIMP,ISTAT)
            call findfil(LIMP,XST)
            IF(.NOT.XST)THEN
              msgl2='  '
              CALL USRMSG(
     &          'The csv activity file is not in databases/UK_NCM.',
     &          msgl2,'W')
            ENDIF
          ENDIF

C Import data - open the import file and read them.
          IUNIT=IFIL+4
          CALL EFOPSEQ(IUNIT,LIMP,1,IER)
          if(ier.eq.0)THEN
            write(currentfile,'(a)') LIMP(1:lnblnk(LIMP))
          ELSE
            msgl2='Is the name correct?'
            CALL USRMSG('Could not open import file.',msgl2,'W')

C << Return may not needed.. test >>
            RETURN
          endif

C STRIP the specific line of the specific timestep
          DO 4 istrip=1,8760
            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'import raw data',IER)
            IF(IER.NE.0)GOTO 103

C Read value, associate with proper column and check if it changes
C the minimum/maximum of that column.
            K=0
            VAL=0.
            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','imp data',IER)
            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','imp data',IER)
            TABU(ITCOL(iimport,12))=VAL
            if(TABU(ITCOL(iimport,12)).lt.TABUMN(ITCOL(iimport,12)))
     &        TABUMN(ITCOL(iimport,12))=TABU(ITCOL(iimport,12))
            if(TABU(ITCOL(iimport,12)).gt.TABUMX(ITCOL(iimport,12)))
     &        TABUMX(ITCOL(iimport,12))=TABU(ITCOL(iimport,12))
            iarraycount=1+((iimport-1)*5)
            COLUMNTDF(iarraycount,istrip)=TABU(ITCOL(iimport,12))

            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','imp data',IER)
            TABU(ITCOL(iimport,13))=VAL
            if(TABU(ITCOL(iimport,13)).lt.TABUMN(ITCOL(iimport,13)))
     &        TABUMN(ITCOL(iimport,13))=TABU(ITCOL(iimport,13))
            if(TABU(ITCOL(iimport,13)).gt.TABUMX(ITCOL(iimport,13)))
     &        TABUMX(ITCOL(iimport,13))=TABU(ITCOL(iimport,13))
            iarraycount=2+((iimport-1)*5)
            COLUMNTDF(iarraycount,istrip)=TABU(ITCOL(iimport,13))

            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','imp data',IER)
            TABU(ITCOL(iimport,14))=VAL
            if(TABU(ITCOL(iimport,14)).lt.TABUMN(ITCOL(iimport,14)))
     &        TABUMN(ITCOL(iimport,14))=TABU(ITCOL(iimport,14))
            if(TABU(ITCOL(iimport,14)).gt.TABUMX(ITCOL(iimport,14)))
     &        TABUMX(ITCOL(iimport,14))=TABU(ITCOL(iimport,14))
            iarraycount=3+((iimport-1)*5)
            COLUMNTDF(iarraycount,istrip)=TABU(ITCOL(iimport,14))

            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','imp data',IER)
            TABU(ITCOL(iimport,15))=INT(VAL)
            if(TABU(ITCOL(iimport,15)).lt.TABUMN(ITCOL(iimport,15)))
     &        TABUMN(ITCOL(iimport,15))=TABU(ITCOL(iimport,15))
            if(TABU(ITCOL(iimport,15)).gt.TABUMX(ITCOL(iimport,15)))
     &        TABUMX(ITCOL(iimport,15))=TABU(ITCOL(iimport,15))
            iarraycount=4+((iimport-1)*5)
            COLUMNTDF(iarraycount,istrip)=TABU(ITCOL(iimport,15))

            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','imp data',IER)
            TABU(ITCOL(iimport,16))=INT(VAL)
            if(TABU(ITCOL(iimport,16)).lt.TABUMN(ITCOL(iimport,16)))
     &        TABUMN(ITCOL(iimport,16))=TABU(ITCOL(iimport,16))
            if(TABU(ITCOL(iimport,16)).gt.TABUMX(ITCOL(iimport,16)))
     &        TABUMX(ITCOL(iimport,16))=TABU(ITCOL(iimport,16))
            iarraycount=5+((iimport-1)*5)
            COLUMNTDF(iarraycount,istrip)=TABU(ITCOL(iimport,16))
 4        CONTINUE

C End of file encountered, close the file and return to menu.
  103     msgl2=' End of import file reached'
          CALL USRMSG(' ',msgl2,'-')
          CALL ERPFREE(IUNIT,ISTAT)
          continue

C << Change this in the future >>
C << Need to add "errors when creating string buffers." ?? >>
  212     continue
  2     continue
        NTSPH=1
        itdbdoy=1
        DO 46 IP=1,8760
C          ITREC=IP+49
          CDAY=AINT(FLOAT(IP)/FLOAT(NTSPH*24))
          ITIME=IP-(INT(CDAY)*NTSPH*24)
          RDOTY=REAL(itdbdoy)+CDAY+(REAL(ITIME)/(REAL(NTSPH)*24.))
          itrunc=1
          ipos=1
          delim='C'
          do while (itrunc.ne.0)
            do 5 iwrite=1,iarraycount
              COLUMNWRITE(iwrite)=COLUMNTDF(iwrite,IP)
  5         continue
            call aelist(ipos,icolumns,COLUMNWRITE,
     &        iarraycount,delim,louts,loutln,itrunc)
            if(ipos.eq.1) then
              WRITE(iafil,'(F8.4,2a)',IOSTAT=ios,ERR=101) RDOTY,',',
     &          louts(1:loutln)
            else
              WRITE(iafil,'(1x,a)',
     &          IOSTAT=ios,ERR=101) louts(1:loutln)
            endif
            ipos=itrunc+1
          end do
   46   CONTINUE
        WRITE(iafil,'(2a)',IOSTAT=ios,ERR=101) '*end_tabular_data'
        CALL ERPFREE(iafil,ISTAT)
      endif

      ITEMSTD=0
      DO 555 IZ=1,NCOMP

C If there is an activity associated to the zone then assign
C DBZN to use later on in the cfg file
        if(theactivityindex(IZ).gt.0)then
          ITEMSTD=ITEMSTD+1

C Keep writing now the items needed for cfg file
          write(DBTAG(ITEMSTD),'(i4)')theactivityindex(IZ)
          write(DBZN(ITEMSTD),'(a)') zname(IZ)(1:12)
          DBSN(ITEMSTD)='ALL'
          DBTASK(ITEMSTD)='ACTIVITY'
        endif
 555  CONTINUE
      call emkcfg('-',IER)
      WRITE (OUTS,'(2A)')'Writing stripped configuration file ',LCFGF
      CALL EDISP(IUOUT,OUTS)
      iuact = 1 ! signal generation of notional completed
      return

 101  msgl2='  '
      CALL USRMSG('Could not write TDF header',msgl2,'W')
      IER=1
      return
      end


C ********************* EMKSBM
C EMKSBM creates a model variant based on the SBEM methodology. Details
C of this can be found in relevant EU and UK building regulations.
C This subroutine copies the cfg, cnn and mandatory zones files and
C makes a new model representative of the notional building.
C UK standard construction and materials databases are also copied.
C Changes to the model are then made, geometry and construction files
C are changed and so are the constructions and materials databases.
C If ier=0 no problems, ier=2 insufficient data.
C This routine assumes newer geometry files are being used.
      SUBROUTINE EMKSBM(ier)
#include "building.h"
#include "geometry.h"
#include "sbem.h"
#include "esprdbfile.h"
#include "model.h"

      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/C1/NCOMP,NCON
      integer ncomp,ncon
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      INTEGER :: ic1,ie1,ict,ic2,ie2
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/C24/IZSTOCN(MCOM,MS)
      INTEGER :: IZSTOCN

C G9 holds information on children of a surface.
      integer nbchild   ! how many children (up to 4) for each connection
      integer nbgchild  ! how many grand children (up to 4) for each connection
      integer ichild    ! list of children for each connection
      integer igchild   ! list of grand children for each connection
      common/G9/nbchild(MCON),nbgchild(MCON),ichild(MCON,4),
     &igchild(MCON,4)

      common/FILEP/IFIL

      CHARACTER*1 ACT
      character EXT*4    ! up to 4 characters in file extension
      character APE*6    ! up to 6 characters in variant name
      CHARACTER CFGROOT*24
      CHARACTER LCFGF*72,NNAME*72,H*72
      CHARACTER OUTS*124,ADF*124,SNM*12,USE*8,USE2*8
      character SN*12
      CHARACTER*12 XJCCU,XKCCU

C ivalsg keeps track of whether a geometry file has alread been copied.
C irmw keeps track of up to 90 surfaces in each zone which can be deleted
      dimension ivalsg(MCOM),IRMV(MCOM,90),nrmv(mcom)

      DIMENSION COE(3),POINT(3),TRANS(3)
      DIMENSION SNA(MS),PAZI(MS),PELV(MS),TARSURF(MCON)
      DIMENSION XPDAREA(MCON) ! Pedestrian door area in surface
      DIMENSION XREAREA(MCON) ! If requisite area is more than required
                              ! do not put any glazing in this surface
      DIMENSION XVDAREA(MCON) ! Vehicle door area in surface
      DIMENSION XDWAREA(MCON) ! Display window area in surface
      DIMENSION XNWAREA(MCON) ! Normal window area in surface
      logical newgeo  ! to use for testing if new/old geometry file.
C      logical closeelv ! true if surface is close to vertical
      LOGICAL MODIFY,MODLEN,MODBND,QUIET,XST,NEAR
      integer LOOPMAX  ! loop iterator for shifting surface list
      integer ISCURRENT ! the current connection to be deleted

C Initialise variables
      SN=' '

C Check if Notional Model Flag has been set (NCM data has been defined)
C If not then advise user and send him/her back
      IF(ISBEM.NE.2)THEN
        H(1) = 'You have not defined NCM details about the building. '
        H(2) = 'These can be defined by choosing model context in the '
        H(3) = 'browse / edit / simulate menu and then choosing  '
        H(4) = 'option UK NCM specifications. '
        H(5) = 'You are being returned to the NCM editing facility. '
        CALL PHELPD('NCM details undefined!',5,'-',0,0,IER)
        ier=2  ! signal insufficient details
        RETURN
      ENDIF
      APE='_not  '
      newgeo=.true.  ! assume newer format geometry.

C The inital task is to make copies of the relevant zone files
C which is functionally equivalent to the code blocks in subroutine
C verman for the case of geometry and attribution (ILM=2).

C Change name of configuration file to filename_not.cfg
      LCFGF=LCFGF_N
      cfgroot=cfgroot_n  ! set to notional

C Adapt original title of model.
      lns=lnblnk(LSNAM_O)
      if(lns.lt.61)then
        write(modeltitle,'(2a)') LSNAM_O(1:lns),' (notional)'
      else
        write(modeltitle,'(2a)') LSNAM_O(1:61),' (notional)'
      endif

C Set flag for cfg file that this is a notional model
      INOTI=1

C Assign standard databases containing UK NCM materials and constructions
C These files are UK_notional.constrdb, material.db3.a and optics.db2
C As they will be in the standard database folder use the *std...
      ipathmat=2
      WRITE(LFMAT,'(A)') 'material.db3.a'
      ipathmul=2
      WRITE(LFMUL,'(A)') 'UK_notional.constrdb'
      ipathoptdb=2
      WRITE(LOPTDB,'(A)') 'optics.db2'
      ipathpcdb=2
      WRITE(LPCDB,'(A)') 'plantc.db1'
      ipathsbem=2
      WRITE(LSBEM,'(a)') 'SBEM.db1'
      call opendb(ier)

C Loop through each zone in the model and first scan in the zone
C geometry, next use surrel to update parent/child information and
C write out the zone.
      DO 550 ICOMP=1,NCOMP
        IUF=IFIL+2
        EXT='.geo'
        call eclose(gversion(icomp),1.1,0.01,newgeo)
        if(newgeo)then
          call georead(IUF,LGEOM(ICOMP),ICOMP,1,iuout,IER)
        else
          call usrmsg('Older geometry file cannot be used.',
     &      LGEOM(ICOMP),'W')
          ier = 2
        endif
        if(ier.ne.0)then
          H(1)='A problem was found when scanning the origial'
          H(2)='model geometry. You may need to alter the original'
          H(3)='model in order to proceed.'
          H(4)='You are being returned to the NCM editing facility. '
          CALL PHELPD('NCM details undefined!',4,'-',0,0,IER)
          ier=3  ! signal geometry issue
          return
        endif

        CALL FNCNGR(LGEOM_O(ICOMP),APE,EXT,NNAME)
        LGEOM(ICOMP)=NNAME
        ACT='s'
        CALL SURREL(ACT,ICOMP,IER)
        call geowrite(IUF,LGEOM(ICOMP),ICOMP,ITRU,3,IER)

C Make a copy of constructions file and tmc file if applicable.
        IUNIT=12
        QUIET=.FALSE.
        call FINDFIL(LTHRM(ICOMP),XST)
        ITRC=0
        ITRU=6
        CALL ECONST(LTHRM(ICOMP),IUNIT,ICOMP,ITRC,ITRU,IER)
        EXT='.con'
        CALL FNCNGR(LTHRM(ICOMP),APE,EXT,NNAME)
        LTHRM(ICOMP)=NNAME
        CALL EMKCON(LTHRM(ICOMP),IUNIT,ICOMP,QUIET,IER)
        call FINDFIL(LTWIN(ICOMP),XST)
        IF(.NOT.XST)GOTO 500
        IFU=13
        CALL ERTWIN(ITRC,ITRU,IFU,LTWIN(ICOMP),ICOMP,IER)
        EXT='.tmc'
        CALL FNCNGR(LTWIN(ICOMP),APE,EXT,NNAME)
        LTWIN(ICOMP)=NNAME
        CALL MKTWIN(IFU,ICOMP,QUIET,IER)
        if(ier.ne.0)then
          H(1)='A problem was found when copying origial'
          H(2)='model files. Check if you have write permission.'
          H(3)='You are being returned to the NCM editing facility. '
          CALL PHELPD('NCM details undefined!',3,'-',0,0,IER)
          ier=4  ! signal geometry issue
          return
        endif

C Remember this geometry file has been done so that a later selection
C of constructions does not re-do it.
 500    ivalsg(icomp)=icomp

C Initialise indices of surfaces to be deleted
        NRMV(icomp)=0
        do 40 i=1,90
          irmv(ICOMP,i)=0
 40     continue
 550  CONTINUE

C Check length of cfgroot string and add _not to it.
      IROOTLEN=LNBLNK(CFGROOT_O)
      write(cfgroot,'(2a)')cfgroot_O(1:irootlen),ape(1:4)

C Variant connections file also required set up its name and
C then use emkcfg to create it.
      EXT='.cnn'
      CALL FNCNGR(LCNN,APE,EXT,NNAME)
      LCNN=NNAME
      CALL EMKCFG('-',IER)

C Inform the user that we are setting up the notional model.
      WRITE (OUTS,'(2A)')'Writing notional configuration file ',LCFGF
      CALL EDISP(IUOUT,OUTS)

C Re-establish glazing fractions based on UK NCM guidelines
      if(ibusertyp.ne.0)then
        ibtyp = IBTYPNDX(ibusertyp)
        IF(IBTYP.EQ.1)THEN
          ROOFFR=0.2
          WALLFR=0.3
        ELSEIF(IBTYP.EQ.2)THEN
          ROOFFR=0.2
          WALLFR=0.4
        ELSEIF(IBTYP.EQ.3)THEN
          ROOFFR=0.2
          WALLFR=0.15
        ENDIF
        ROOFFRFR=0.30
        WALLFRFR=0.10
        GLZFR=0.
      else
        H(1)='A problem was found - the NCM building type'
        H(2)='was not defined. Please do this and try again.'
        CALL PHELPD('NCM building type undefined!',2,'-',0,0,IER)
        ier=5  ! signal ncm issue
        return
      endif
      itrc=0

C Loop through each zone in the model and if ivalsg was set earlier
C then read in the zone geometry file to re-gather information on
C parent child relationships
      DO 559 ICOMP=1,NCOMP
        IELF=0
        if(ivalsg(icomp).eq.icomp)then
          IUF=IFIL+2
          call eclose(gversion(icomp),1.1,0.01,newgeo)
          call georead(IUF,LGEOM(ICOMP),ICOMP,1,iuout,IER)

C Get surface areas via call to zinfo.
          call zinfo(sna,zoa,pazi,pelv,zvol)

C Get gross area of this surface (add child areas to it)
C Areas of parents are hence held as parent + child in TARSURF and
C areas of children are held as 0.0
C For each surface in this zone
          do 555 isur=1,nzsur(icomp)
            ICC=IZSTOCN(icomp,isur)
            write(SNM,'(a)') SSUSE(ICC,1)

C For all exterior surfaces that are wall or roof
            if(ssother(icc,1)(1:8).eq.'EXTERIOR'.AND.(
     &      SNM(1:4).EQ.'WALL'.or.SNM(1:4).EQ.'ROOF'))then

C Only the following types of areas will be retained in this exterior
C type surface with use roof or wall
              XPDAREA(ICC)=0. ! pedestrian door area in this parent
              XVDAREA(ICC)=0. ! vehicular door area in this parent
              XDWAREA(ICC)=0. ! display window area in this parent
              XNWAREA(ICC)=0. ! normal window area in this parent
              XREAREA(ICC)=0. ! If requisite area is more than required
                              ! do not put any glazing in this surface
              TARSURF(ICC)=SSNA(ICC)
              DO 5500 IGCD=1,NBGCHILD(ICC)
                KCC=IGCHILD(ICC,IGCD)
                NRMV(icomp)=NRMV(icomp)+1
                IRMV(ICOMP,NRMV(icomp))=KCC
                XKCCU=SSUSE(KCC,1)
                TARSURF(ICC)=TARSURF(ICC)+SSNA(KCC)
                IF(XKCCU(1:6).EQ.'P-DOOR')THEN
                  XPDAREA(ICC)=XPDAREA(ICC)+SSNA(KCC)
                  XREAREA(ICC)=XREAREA(ICC)+SSNA(KCC)
                ELSEIF(XKCCU(1:6).EQ.'V-DOOR')THEN
                  XVDAREA(ICC)=XVDAREA(ICC)+SSNA(KCC)
                  XREAREA(ICC)=XREAREA(ICC)+SSNA(KCC)
                ELSEIF(XKCCU(1:8).EQ.'D-WINDOW')THEN
                  XDWAREA(ICC)=XDWAREA(ICC)+SSNA(KCC)
                  XREAREA(ICC)=XREAREA(ICC)+SSNA(KCC)
                ELSEIF(XKCCU(1:8).EQ.'S-WINDOW'.OR.
     &                 XKCCU(1:8).EQ.'C-WINDOW')THEN
                  XNWAREA(ICC)=XNWAREA(ICC)+SSNA(KCC)
                  XREAREA(ICC)=XREAREA(ICC)+SSNA(KCC)
                ENDIF
 5500         CONTINUE
              DO 553 ICHL=1,NBCHILD(ICC)
                JCC=ICHILD(ICC,ICHL)
                NRMV(icomp)=NRMV(icomp)+1
                IRMV(ICOMP,NRMV(icomp))=JCC
                XJCCU=SSUSE(JCC,1)
                TARSURF(ICC)=TARSURF(ICC)+SSNA(JCC)
                IF(XJCCU(1:6).EQ.'P-DOOR')THEN
                  XPDAREA(ICC)=XPDAREA(ICC)+SSNA(JCC)
                  XREAREA(ICC)=XREAREA(ICC)+SSNA(JCC)
                ELSEIF(XJCCU(1:6).EQ.'V-DOOR')THEN
                  XVDAREA(ICC)=XVDAREA(ICC)+SSNA(JCC)
                  XREAREA(ICC)=XREAREA(ICC)+SSNA(JCC)
                ELSEIF(XJCCU(1:8).EQ.'D-WINDOW')THEN
                  XDWAREA(ICC)=XDWAREA(ICC)+SSNA(JCC)
                  XREAREA(ICC)=XREAREA(ICC)+SSNA(JCC)
                ELSEIF(XJCCU(1:8).EQ.'S-WINDOW'.OR.
     &                 XJCCU(1:8).EQ.'C-WINDOW')THEN
                  XNWAREA(ICC)=XNWAREA(ICC)+SSNA(JCC)
                  XREAREA(ICC)=XREAREA(ICC)+SSNA(JCC)
                ENDIF
 553          CONTINUE
            ENDIF
 555      CONTINUE
        ENDIF
 559  CONTINUE

C Loop through all of the zones again and remove surfaces marked
C for deletion in IRMV
      DO 6100 ICOMP=1,NCOMP

C Debug the initial list of surfaces to remove.
C        write(6,*) 'about to remove ',NRMV(icomp),' surfs in z ',
C     &    icomp
C        write(6,*) 'list ',IRMV(ICOMP,1),IRMV(ICOMP,2),IRMV(ICOMP,3),
C     &    IRMV(ICOMP,4),IRMV(ICOMP,5),IRMV(ICOMP,6),IRMV(ICOMP,7),
C     &    IRMV(ICOMP,8),IRMV(ICOMP,9),IRMV(ICOMP,10),IRMV(ICOMP,11)

C In the current zone we need to delete NRMV surfaces.
        DO 6001 IR=1,NRMV(icomp)

C Debug.
C          write(6,*) ' doing conn surf',IRMV(ICOMP,IR),
C     &      ie1(IRMV(ICOMP,IR)),' for ir ',ir

C Pack the list of TARSURF etc from the current connection data
C to one less than the end of the number of connections in the
C model. Set ISCURRENT to the current connection to delete (because
C IRMV is going to be altered).
          LOOPMAX=NCON-1
          ISCURRENT=IRMV(ICOMP,IR)
          DO 6004 IS2=ISCURRENT,LOOPMAX
            TARSURF(IS2)=TARSURF(IS2+1)
            XPDAREA(IS2)=XPDAREA(IS2+1)
            XVDAREA(IS2)=XVDAREA(IS2+1)
            XDWAREA(IS2)=XDWAREA(IS2+1)
            XNWAREA(IS2)=XNWAREA(IS2+1)
            XREAREA(IS2)=XREAREA(IS2+1)
 6004     CONTINUE

C And for subsequent surfaces to delete in this zone, decrement the
C connection index held in IRMV.
          DO 6005 IRC=IR+1,NRMV(ICOMP)
            IF(IRMV(ICOMP,IR).LT.IRMV(ICOMP,IRC))
     &        IRMV(ICOMP,IRC)=IRMV(ICOMP,IRC)-1
 6005     CONTINUE

C And for subsequent surfaces in subsequent zones, decrement their
C connection index held in IRMV.
          DO 6006 IC=ICOMP+1,NCOMP
            DO 6007 IRC=1,NRMV(IC)
              IRMV(IC,IRC)=IRMV(IC,IRC)-1
 6007       CONTINUE
 6006     CONTINUE

C Debug the packed irmv array.
C        write(6,*) 'listx ',IRMV(ICOMP,1),IRMV(ICOMP,2),IRMV(ICOMP,3),
C     &    IRMV(ICOMP,4),IRMV(ICOMP,5),IRMV(ICOMP,6),IRMV(ICOMP,7),
C     &    IRMV(ICOMP,8),IRMV(ICOMP,9),IRMV(ICOMP,10),IRMV(ICOMP,11)

C Remove the surface. Get is from the irmv list (which is
C connection based).
          is=ie1(IRMV(ICOMP,IR))

C Debug.
C          write(6,*) 'ir is ',ir,' removing surf ',is,' in zone ',icomp

          iopt=0
          call georead(IUF,LGEOM(ICOMP),ICOMP,1,iuout,IER)
          CALL ADDSUR(ITRC,ICOMP,IS,'D','A',iopt,IER)
          CALL EMKCFG('-',IER)
          call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,ITRU,3,IER)

C Update the image.
          if(MMOD.eq.8)then
            MODBND=.TRUE.
            MODIFY=.TRUE.
            CALL INLNST(1)
            nzg=1
            nznog(1)=ICOMP
            izgfoc=ICOMP
            CALL ADJVIEW(IER)
            call pausems(500)
          endif
 6001   CONTINUE
 6100 CONTINUE

C Work out area of glazings etc. into all exterior surfaces
C that remain in the model. Do this by looping through
C each zone in the model and in each of the surfaces
C in the current zone check its attributes.
      DO 557 ICOMP=1,NCOMP
        do 200 isur=1,nzsur(icomp)
          ICC=IZSTOCN(icomp,isur)
          SNM=SSUSE(ICC,1)

C For all exterior surfaces that are wall or roof
          if(ssother(icC,1)(1:8).eq.'EXTERIOR'.AND.(
     &    SNM(1:4).EQ.'WALL'.or.SNM(1:4).EQ.'ROOF'))then
            IF(SNM(1:4).EQ.'WALL')THEN
              GLZFR=WALLFR
              FRMFR=WALLFRFR
            ELSEIF(SNM(1:4).EQ.'ROOF')THEN
              GLZFR=ROOFFR
              FRMFR=ROOFFRFR
            ENDIF
            IF(XREAREA(ICC).GT.TARSURF(ICC)*GLZFR)THEN

C Area of doors and windows is greater than glazings to be put in so do
C not put in additional windows but retain if any are present
              CONTINUE
            ELSE

C Area of doors and windows is less than glazings to be put in so put in
C additional windows
              XNWAREA(ICC)=
     &        TARSURF(ICC)*GLZFR-XREAREA(ICC)+XNWAREA(ICC)
            ENDIF
          endif
 200    continue
 557  continue

C Now loop through the model in reverse zone order and
C add required surfaces.
C << need to trap error state in georead >>
      DO 551 ICOMP=NCOMP,1,-1
        NUMBS=NZSUR(ICOMP)
        do 580 isur=NUMBS,1,-1
          call georead(IUF,LGEOM(ICOMP),ICOMP,1,iuout,IER)
          is=isur
          ICON=IZSTOCN(icomp,nsur)  ! the last surface in the zone
          IELF=0
          ICC=IZSTOCN(icomp,isur)   ! connection for current surface
          COE(1)=SSURCOG(ICC,1)
          COE(2)=SSURCOG(ICC,2)
          COE(3)=SSURCOG(ICC,3)
          SNM=SSUSE(ICC,1)

C For all exterior surfaces that are wall or roof
          if(ssother(icC,1)(1:8).eq.'EXTERIOR'.AND.
     &      (SNM(1:4).EQ.'WALL'.or.SNM(1:4).EQ.'ROOF'))then
            IF(SNM(1:4).EQ.'WALL')THEN
              GLZFR=WALLFR
              FRMFR=WALLFRFR
            ELSEIF(SNM(1:4).EQ.'ROOF')THEN
              GLZFR=ROOFFR
              FRMFR=ROOFFRFR
            ENDIF

C Get scale factors
            TOTAREA=TARSURF(ICC)
            REQDAREA=XPDAREA(ICC)+XVDAREA(ICC)+
     &      XDWAREA(ICC)+XNWAREA(ICC)
            SCALEFACTORPD=SQRT(REQDAREA/TOTAREA)
            SCALEFACTORFR=SQRT(FRMFR)

C First put pedestrian door in
            CALL ECLOSE(XPDAREA(ICC),0.0,0.01,NEAR)
            IF(.NOT.NEAR)THEN
              IELF=IELF+1
              ICON=ICON+1
              NSUR=NSUR+1
              NZTV(ICOMP)=NTV
              WRITE(SNAME(ICOMP,NSUR),'(i2,2A)')
     &          is,CHAR(96+IELF),'_Pdoor'  ! Name
              WRITE(SN,'(i2,2A)')
     &          is,CHAR(96+IELF),'_Pdoor'  ! Name
              write(outs,'(2a)') 'Adding new surf ',SN(1:lnblnk(SN))
              call edisp(iuout,outs)
              ISUR2=NSUR
              DO 7212 NV=1,NVER(IS)
                POINT(1)=X(JVN(IS,NV))
                POINT(2)=Y(JVN(IS,NV))
                POINT(3)=Z(JVN(IS,NV))
                CALL ENLARGE(POINT,TRANS,COE,SCALEFACTORPD)
                X(NTV+NV)=TRANS(1)
                Y(NTV+NV)=TRANS(2)
                Z(NTV+NV)=TRANS(3)
                JVN(ISUR2,NV)=NTV+NV
                JVN(IS,2*NVER(IS)-NV+2)=NTV+NV
 7212         CONTINUE
              JVN(IS,NVER(IS)+1)=JVN(IS,1)
              JVN(IS,2*NVER(IS)+2)=JVN(IS,NVER(IS)+2)
              NTV=NTV+NVER(IS) ! increment nb verts
              NZTV(ICOMP)=NTV  ! update the zone array
              NZSUR(ICOMP)=NSUR
              NVER(ISUR2)=NVER(IS)
              NVER(IS)=2*NVER(IS)+2

C Update the connection list. Move all others up and then insert with
C default values.
              call addedsurf(icomp,icon,itrc,ier)

C Add surfaces attribution parameters
              ICT(ICON)=0
              IC2(ICON)=0
              IE2(ICON)=0
              SSOTHER(ICON,1)='EXTERIOR'
              SSOTHER(ICON,2)='0'
              SSOTHER(ICON,3)='0'
              SSMLCN(ICON)='opDoorWind_n' ! MLC type
              SSPARENT(ICON)=SSNAME(ICC)
              SSOTF(ICON)='OPAQUE'  ! OTF type
              SSVFC(ICON)=SSVFC(ICC)
              SSUSE(ICON,1)='P-DOOR'   ! USE
              SSUSE(ICON,2)='-'

C Update connections list before displaying notional model zone
              call emkcfg('-',IER)
              MODIFY=.TRUE.
              NZSUR(ICOMP)=NSUR
              call zgupdate(itrc,icomp,ier)
              call geowrite(IUF,LGEOM(ICOMP),ICOMP,ITRU,3,IER)
              call emkcfg('-',IER)

C Precondition variables IS and TOTAREA to set them up for other child
C surfaces that may need to be put into this original surface. E.g. if a
C door and a window are to be added to original surface, the window is
C added as child of door and not child of original surface.
              is=nsur
              TOTAREA=REQDAREA

C Update the image.
              if(MMOD.eq.8)then
                MODBND=.TRUE.
                MODIFY=.TRUE.
                CALL INLNST(1)
                nzg=1
                nznog(1)=ICOMP
                izgfoc=ICOMP
                CALL ADJVIEW(IER)
                call pausems(500)
              endif
            ENDIF

C Now put vehicle door in
            REQDAREA=XVDAREA(ICC)+
     &      XDWAREA(ICC)+XNWAREA(ICC)
            SCALEFACTORVD=SQRT(REQDAREA/TOTAREA)
            CALL ECLOSE(XVDAREA(ICC),0.0,0.01,NEAR)
            IF(.NOT.NEAR)THEN
              IELF=IELF+1
              ICON=ICON+1
              NSUR=NSUR+1
              NZTV(ICOMP)=NTV
              WRITE(SNAME(ICOMP,NSUR),'(i2,2A)')
     &          is,CHAR(96+IELF),'_Vdoor'  ! Name
              WRITE(SN,'(i2,2A)')
     &          is,CHAR(96+IELF),'_Vdoor'  ! Name
              write(outs,'(2a)') 'Adding new surf ',SN(1:lnblnk(SN))
              call edisp(iuout,outs)
              ISUR2=NSUR
              DO 7312 NV=1,NVER(IS)
                POINT(1)=X(JVN(IS,NV))
                POINT(2)=Y(JVN(IS,NV))
                POINT(3)=Z(JVN(IS,NV))
                CALL ENLARGE(POINT,TRANS,COE,SCALEFACTORVD)
                X(NTV+NV)=TRANS(1)
                Y(NTV+NV)=TRANS(2)
                Z(NTV+NV)=TRANS(3)
                JVN(ISUR2,NV)=NTV+NV
                JVN(IS,2*NVER(IS)-NV+2)=NTV+NV
 7312         CONTINUE
              JVN(IS,NVER(IS)+1)=JVN(IS,1)
              JVN(IS,2*NVER(IS)+2)=JVN(IS,NVER(IS)+2)
              NTV=NTV+NVER(IS)
              NZTV(ICOMP)=NTV
              NZSUR(ICOMP)=NSUR
              NVER(ISUR2)=NVER(IS)
              NVER(IS)=2*NVER(IS)+2

C Update the connection list. Move all others up and then insert with
C default values.
              call addedsurf(icomp,icon,itrc,ier)

C Add surfaces attribution parameters
              ICT(ICON)=0
              IC2(ICON)=0
              IE2(ICON)=0
              SSOTHER(ICON,1)='EXTERIOR'
              SSOTHER(ICON,2)='0'
              SSOTHER(ICON,3)='0'
              SSMLCN(ICON)='Vehicle_door' ! MLC type
              SSPARENT(ICON)=SSNAME(ICC)
              SSOTF(ICON)='OPAQUE'  ! OTF type
              SSVFC(ICON)=SSVFC(ICC)
              SSUSE(ICON,1)='V-DOOR'  ! USE
              SSUSE(ICON,2)='-'

C Update connections list before displaying notional model zone
              call emkcfg('-',IER)
              MODIFY=.TRUE.
              NZSUR(ICOMP)=NSUR
              call zgupdate(itrc,icomp,ier)
              call geowrite(IUF,LGEOM(ICOMP),ICOMP,ITRU,3,IER)
              call emkcfg('-',IER)

C Precondition variables IS and TOTAREA to set them up for other child
C surfaces that may need to be put into this original surface. E.g. if a
C door and a window are to be added to original surface, the window is
C added as child of door and not child of original surface.
              is=nsur
              TOTAREA=REQDAREA

C Update the image.
              if(MMOD.eq.8)then
                MODBND=.TRUE.
                MODIFY=.TRUE.
                CALL INLNST(1)
                nzg=1
                nznog(1)=ICOMP
                izgfoc=ICOMP
                CALL ADJVIEW(IER)
                call pausems(500)
              endif
            ENDIF

C Now put display glazing in
            REQDAREA=XDWAREA(ICC)+XNWAREA(ICC)
            SCALEFACTORDG=SQRT(REQDAREA/TOTAREA)
            CALL ECLOSE(XDWAREA(ICC),0.0,0.01,NEAR)
            IF(.NOT.NEAR)THEN
              IELF=IELF+1
              ICON=ICON+1
              NSUR=NSUR+1
              NZTV(ICOMP)=NTV
              WRITE(SNAME(ICOMP,NSUR),'(i2,2A)')
     &          is,CHAR(96+IELF),'_dispglz'  ! Name
              WRITE(SN,'(i2,2A)')
     &          is,CHAR(96+IELF),'_dispglz'  ! Name
              write(outs,'(2a)') 'Adding new surf ',SN(1:lnblnk(SN))
              call edisp(iuout,outs)
              ISUR2=NSUR
              DO 7412 NV=1,NVER(IS)
                POINT(1)=X(JVN(IS,NV))
                POINT(2)=Y(JVN(IS,NV))
                POINT(3)=Z(JVN(IS,NV))
                CALL ENLARGE(POINT,TRANS,COE,SCALEFACTORDG)
                X(NTV+NV)=TRANS(1)
                Y(NTV+NV)=TRANS(2)
                Z(NTV+NV)=TRANS(3)
                JVN(ISUR2,NV)=NTV+NV
                JVN(IS,2*NVER(IS)-NV+2)=NTV+NV
 7412         CONTINUE
              JVN(IS,NVER(IS)+1)=JVN(IS,1)
              JVN(IS,2*NVER(IS)+2)=JVN(IS,NVER(IS)+2)
              NTV=NTV+NVER(IS)  ! increment nb of verts
              NZTV(ICOMP)=NTV   ! update zone array
              NZSUR(ICOMP)=NSUR
              NVER(ISUR2)=NVER(IS)
              NVER(IS)=2*NVER(IS)+2

C Update the connection list. Move all others up and then insert with
C default values.
              call addedsurf(icomp,icon,itrc,ier)

C Add surfaces attribution parameters
              ICT(ICON)=0
              IC2(ICON)=0
              IE2(ICON)=0
              SSOTHER(ICON,1)='EXTERIOR'
              SSOTHER(ICON,2)='0'
              SSOTHER(ICON,3)='0'
              SSMLCN(ICON)='Not_DisplayG'  ! MLC type
              SSPARENT(ICON)=SSNAME(ICC)
              SSOTF(ICON)='dispGlz_not' ! OTF type
              SSVFC(ICON)=SSVFC(ICC)
              SSUSE(ICON,1)='D-WINDOW'    ! USE
              SSUSE(ICON,2)='-'

C Update connections list before displaying notional model zone
              call emkcfg('-',IER)
              MODIFY=.TRUE.
              NZSUR(ICOMP)=NSUR
              call zgupdate(itrc,icomp,ier)
              call geowrite(IUF,LGEOM(ICOMP),ICOMP,ITRU,3,IER)
              call emkcfg('-',IER)

C Precondition variables IS and TOTAREA to set them up for other child
C surfaces that may need to be put into this original surface. E.g. if a
C door and a window are to be added to original surface, the window is
C added as child of door and not child of original surface.
              is=nsur
              TOTAREA=REQDAREA

C Update the image.
              if(MMOD.eq.8)then
                MODBND=.TRUE.
                MODIFY=.TRUE.
                CALL INLNST(1)
                nzg=1
                nznog(1)=ICOMP
                izgfoc=ICOMP
                CALL ADJVIEW(IER)
                call pausems(500)
              endif
            ENDIF

C Now put glazing and frame
            REQDAREA=XNWAREA(ICC)
            SCALEFACTORNG=SQRT(REQDAREA/TOTAREA)
            CALL ECLOSE(XNWAREA(ICC),0.0,0.01,NEAR)
            IF(.NOT.NEAR)THEN
              IELF=IELF+1
              ICON=ICON+1
              NSUR=NSUR+1
              NZTV(ICOMP)=NTV
              WRITE(SNAME(ICOMP,NSUR),'(i2,2A)')
     &          is,CHAR(96+IELF),'_glz'  ! Name
              WRITE(SN,'(i2,2A)')
     &          is,CHAR(96+IELF),'_glz'  ! Name
              write(outs,'(2a)') 'Adding new surf ',SN(1:lnblnk(SN))
              call edisp(iuout,outs)
              ISUR2=NSUR
              DO 7512 NV=1,NVER(IS)
                POINT(1)=X(JVN(IS,NV))
                POINT(2)=Y(JVN(IS,NV))
                POINT(3)=Z(JVN(IS,NV))
                CALL ENLARGE(POINT,TRANS,COE,SCALEFACTORNG)
                X(NTV+NV)=TRANS(1)
                Y(NTV+NV)=TRANS(2)
                Z(NTV+NV)=TRANS(3)
                JVN(ISUR2,NV)=NTV+NV
                JVN(IS,2*NVER(IS)-NV+2)=NTV+NV
 7512         CONTINUE
              JVN(IS,NVER(IS)+1)=JVN(IS,1)
              JVN(IS,2*NVER(IS)+2)=JVN(IS,NVER(IS)+2)
              NTV=NTV+NVER(IS)  ! increment nb of verts
              NZTV(ICOMP)=NTV   ! update zone array
              NZSUR(ICOMP)=NSUR
              NVER(ISUR2)=NVER(IS)
              NVER(IS)=2*NVER(IS)+2

C Update the connection list. Move all others up and then insert with
C default values.
              call addedsurf(icomp,icon,itrc,ier)

C Add surfaces attribution parameters
              ICT(ICON)=0
              IC2(ICON)=0
              IE2(ICON)=0
              SSOTHER(ICON,1)='EXTERIOR'
              SSOTHER(ICON,2)='0'
              SSOTHER(ICON,3)='0'
              SSMLCN(ICON)='window_Not_G'  ! MLC type
              SSPARENT(ICON)=SSNAME(ICC)
              SSOTF(ICON)='Glaz_notiona' ! OTF type
              SSVFC(ICON)=SSVFC(ICC)
              SSUSE(ICON,1)='C-WINDOW'    ! USE
              IF(SNM(1:4).EQ.'ROOF')SSUSE(ICON,1)='S-WINDOW'
              SSUSE(ICON,2)='-'

C Update connections list before displaying notional model zone
              call emkcfg('-',IER)
              MODIFY=.TRUE.
              NZSUR(ICOMP)=NSUR
              call zgupdate(itrc,icomp,ier)
              call geowrite(IUF,LGEOM(ICOMP),ICOMP,ITRU,3,IER)
              call emkcfg('-',IER)
              is=nsur

C Update the image.
              if(MMOD.eq.8)then
                MODBND=.TRUE.
                MODIFY=.TRUE.
                CALL INLNST(1)
                nzg=1
                nznog(1)=ICOMP
                izgfoc=ICOMP
                CALL ADJVIEW(IER)
                call pausems(500)
              endif

C Now put frame
              IELF=IELF+1
              ICON=ICON+1
              NSUR=NSUR+1
              NZTV(ICOMP)=NTV
              WRITE(SNAME(ICOMP,NSUR),'(i2,2A)')
     &          is,CHAR(96+IELF),'_frm'  ! Name
              WRITE(SN,'(i2,2A)')
     &          is,CHAR(96+IELF),'_frm'  ! Name
              write(outs,'(2a)') 'Adding new surf ',SN(1:lnblnk(SN))
              call edisp(iuout,outs)
              ISUR2=NSUR
              DO 7612 NV=1,NVER(IS)
                POINT(1)=X(JVN(IS,NV))
                POINT(2)=Y(JVN(IS,NV))
                POINT(3)=Z(JVN(IS,NV))
                CALL ENLARGE(POINT,TRANS,COE,SCALEFACTORFR)
                X(NTV+NV)=TRANS(1)
                Y(NTV+NV)=TRANS(2)
                Z(NTV+NV)=TRANS(3)
                JVN(ISUR2,NV)=NTV+NV
                JVN(IS,2*NVER(IS)-NV+2)=NTV+NV
 7612         CONTINUE
              JVN(IS,NVER(IS)+1)=JVN(IS,1)
              JVN(IS,2*NVER(IS)+2)=JVN(IS,NVER(IS)+2)
              NTV=NTV+NVER(IS)  ! increment nb verts
              NZTV(ICOMP)=NTV   ! update zone array
              NZSUR(ICOMP)=NSUR
              NVER(ISUR2)=NVER(IS)
              NVER(IS)=2*NVER(IS)+2

C Update the connection list. Move all others up and then insert with
C default values.
              call addedsurf(icomp,icon,itrc,ier)

C Add surfaces attribution parameters
              ICT(ICON)=0
              IC2(ICON)=0
              IE2(ICON)=0
              SSOTHER(ICON,1)='EXTERIOR'
              SSOTHER(ICON,2)='0'
              SSOTHER(ICON,3)='0'
              SSMLCN(ICON)='frame_notion'  ! MLC type
              SSPARENT(ICON)=SSNAME(ICC)
              SSOTF(ICON)='OPAQUE' ! OTF type
              SSVFC(ICON)=SSVFC(ICC)
              SSUSE(ICON,1)='F-FRAME'    ! USE
              SSUSE(ICON,2)='-'

C Update connections list before displaying notional model zone
              call emkcfg('-',IER)
              MODIFY=.TRUE.
              NZSUR(ICOMP)=NSUR
              call zgupdate(itrc,icomp,ier)
              call geowrite(IUF,LGEOM(ICOMP),ICOMP,ITRU,3,IER)
              call emkcfg('-',IER)

C Update the image.
              if(MMOD.eq.8)then
                MODBND=.TRUE.
                MODIFY=.TRUE.
                CALL INLNST(1)
                nzg=1
                nznog(1)=ICOMP
                izgfoc=ICOMP
                CALL ADJVIEW(IER)
                call pausems(500)
              endif
            ENDIF
          ENDIF
 580    CONTINUE
 551  CONTINUE

C Change constructions for different surfaces now based on use
      DO 600 ICOMP=1,NCOMP
        call georead(IUF,LGEOM(ICOMP),ICOMP,1,iuout,IER)
        DO 601 I=1,NZSUR(icomp)
          icc=izstocn(icomp,i)
          USE=SSUSE(ICC,1)
          USE2=SSUSE(ICC,2)
          ADF=SSVFC(icc)
          IF(ssother(icc,1)(1:8).eq.'EXTERIOR')THEN
            IF(USE(1:4).EQ.'WALL')THEN
              IF(IRGG.EQ.2)THEN ! Scottish regulations
                SSMLCN(ICC)='Wall_Scot_no'
              ELSEIF(IRGG.EQ.3)THEN ! Northern Ireland regulations
                SSMLCN(icc)='Wall_NI_noti'
              ELSE ! England and Wales regulations (and omissions)
                SSMLCN(ICC)='Wall_EW_noti'
              ENDIF
            ELSEIF(USE(1:4).EQ.'ROOF')THEN
              IF(USE2(1:4).EQ.'FLAT')THEN
                SSMLCN(ICC)='Fl_roof_noti'
              ELSE
                IF(IRGG.EQ.2)THEN ! Scottish regulations
                  SSMLCN(ICC)='SC_Pitch_roo'
                ELSE ! England, Wales and N. Ireland regulations
                  SSMLCN(ICC)='EW_notPitch_'
                ENDIF
              ENDIF
            ELSEIF(USE(3:6).EQ.'DOOR')THEN
              IF(USE(1:1).EQ.'V')THEN
                SSMLCN(ICC)='Vehicle_door'
              ELSE
                SSMLCN(ICC)='opDoorWind_n'
              ENDIF
            ELSEIF(USE(3:8).EQ.'WINDOW')THEN
              IF(USE(1:1).EQ.'D')THEN
                SSMLCN(ICC)='Not_DisplayG'
                SSOTF(ICC)='dispGlz_not'
              ELSE
                SSMLCN(ICC)='window_Not_G'
                SSOTF(ICC)='Glaz_notiona'
              ENDIF
            ELSEIF(USE(1:5).EQ.'FLOOR')THEN
              SSMLCN(ICC)='Sol_grnd_not'
            ELSEIF(USE(1:7).EQ.'F-FRAME')THEN
              SSMLCN(ICC)='frame_notion'
            ELSE
              SNM=SSNAME(ICC)
              WRITE(OUTS,'(5A)')'No relevant use defined for ',
     &        SNM(1:LNBLNK(SNM)),' in zone ',zname(icomp),
     &        ' setting to use: external wall'
              CALL EDISP(IUOUT,OUTS)
              IF(IRGG.EQ.2)THEN ! Scottish regulations
                SSMLCN(ICC)='Wall_Scot_no'
              ELSEIF(IRGG.EQ.3)THEN ! Northern Ireland regulations
                SSMLCN(icc)='Wall_NI_noti'
              ELSE ! England and Wales regulations (and omissions)
                SSMLCN(ICC)='Wall_EW_noti'
              ENDIF
            ENDIF
          ELSEIF(ssother(icc,1)(1:6).eq.'GROUND')THEN
              SSMLCN(ICC)='Sol_grnd_not'
          ELSE
            IF(USE(1:5).EQ.'FLOOR')THEN
              SSMLCN(ICC)='pa_fl_notion'
            ELSEIF(USE(1:4).EQ.'WALL')THEN
              SSMLCN(ICC)='party_wall_n'
            ELSEIF(USE(3:8).EQ.'WINDOW')THEN
              SSMLCN(ICC)='internal_glz'
              SSOTF(ICC)='Glaz_notiona'
            ELSEIF(ADF(1:4).EQ.'FLOR')THEN
              SSMLCN(ICC)='pa_fl_notion'
            ELSEIF(ADF(1:4).EQ.'CEIL')THEN
              SSMLCN(ICC)='inv_pa_fl_no'
            ELSEIF(ADF(1:4).EQ.'VERT')THEN
              SSMLCN(ICC)='party_wall_n'
            ELSE
              SNM=SSNAME(ICC)
              WRITE(OUTS,'(5A)')'No relevant use defined for ',
     &        SNM(1:LNBLNK(SNM)),' in zone ',zname(icomp),
     &        ' - Setting to internal wall'
              SSMLCN(ICC)='party_wall_n'
              CALL EDISP(IUOUT,OUTS)
            ENDIF
          ENDIF
 601    CONTINUE

C Add 10% thermal bridge by silently calling appropriate subroutine
        CALL LINTHBRDG(ICOMP,'s',0.1)

C Write geometry file version 1.1 (1.0 does not support UK NCM)
        call geowrite(IUF,LGEOM(ICOMP),ICOMP,ITRU,3,IER)
        QUIET=.TRUE.

C Update constructions and tmc files if applicable
        CALL EDCON(ITRC,ITRU,ICOMP,QUIET,IER)
 600  CONTINUE
      RETURN
      END

C ********************* EMKREF
C EMKREF creates a model variant based on the SBEM methodology. Details
C of this can be found in relevant EU and UK building regulations.
C This subroutine copies and modifies the cfg file and
C makes a new model representative of the reference building.
      SUBROUTINE EMKREF
      IMPLICIT NONE
#include "building.h"
#include "sbem.h"
#include "model.h"

      common/C21/IFCFG,cfgroot,LCFGF
      CHARACTER CFGROOT*24,LCFGF*72
      INTEGER IFCFG,IER,LNBLNK,lns

C Initialise
      IER=0
      cfgroot=cfgroot_r  ! set to reference

C Change name of configuration file to filename_ref.cfg
      LCFGF=LCFGF_R

C Adapt original title of model.
      lns=lnblnk(LSNAM_O)
      if(lns.lt.60)then
        write(modeltitle,'(2a)') LSNAM_O(1:lns),' (reference)'
      else
        write(modeltitle,'(2a)') LSNAM_O(1:60),' (reference)'
      endif
      INOTI=2
      CALL EMKCFG('-',IER)
      RETURN
      END

C ********************* EMKTYP
C EMKSBM creates a model variant based on the SBEM methodology. Details
C of this can be found in relevant EU and UK building regulations.
C This subroutine copies and modifies the cfg file and
C makes a new model representative of the typical building.
C Changes to the model are then made.
      SUBROUTINE EMKTYP
      IMPLICIT NONE
#include "sbem.h"
#include "building.h"
#include "geometry.h"
#include "model.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

      common/FILEP/IFIL
      integer ifil

      common/C21/IFCFG,cfgroot,LCFGF
      CHARACTER CFGROOT*24,LCFGF*72
      INTEGER IFCFG

      common/C1/NCOMP,NCON
      integer ncomp,ncon

      COMMON/C24/IZSTOCN(MCOM,MS)
      INTEGER IZSTOCN

      INTEGER IER,LNBLNK,ICOMP,IUNIT,ITRU,ITRC,ILEN,IFU,IUF,I,ICC,lns
      LOGICAL QUIET,XST
      CHARACTER EXT*4,APE*6,TNAME*72,NNAME*72
      character USE*8,OUTS*124,SNM*12

C Initialise
      IER=0
      APE='_typ  '
      cfgroot=cfgroot_t ! set to typical
      LCFGF=LCFGF_T

C Adapt title of model from the original.
      lns=lnblnk(LSNAM_O)
      if(lns.lt.61)then
        write(modeltitle,'(2a)') LSNAM_O(1:lns),' (typical)'
      else
        write(modeltitle,'(2a)') LSNAM_O(1:61),' (typical)'
      endif

      ITRC=0
      INOTI=3
      CALL EMKCFG('-',IER)

C Make a copy of constructions file and tmc file if applicable
      DO 100 ICOMP=1,NCOMP
        IUNIT=12
        QUIET=.FALSE.
        call FINDFIL(LTHRM(ICOMP),XST)
        ITRU=6
        CALL ECONST(LTHRM(ICOMP),IUNIT,ICOMP,ITRC,ITRU,IER)

C File name will always be zonename_not.con so remove _not before
C passing on to FNCNGR
        TNAME=LTHRM(ICOMP)
        ILEN=LNBLNK(TNAME)
        WRITE(LTHRM(ICOMP),'(2A)')TNAME(1:ILEN-8),TNAME(ILEN-3:ILEN)
        EXT='.con'
        CALL FNCNGR(LTHRM(ICOMP),APE,EXT,NNAME)
        LTHRM(ICOMP)=NNAME
        CALL EMKCON(LTHRM(ICOMP),IUNIT,ICOMP,QUIET,IER)
        call FINDFIL(LTWIN(ICOMP),XST)
        IF(XST)THEN
          IFU=13
          CALL ERTWIN(ITRC,ITRU,IFU,LTWIN(ICOMP),ICOMP,IER)
          TNAME=LTWIN(ICOMP)
          ILEN=LNBLNK(TNAME)
          WRITE(LTWIN(ICOMP),'(2A)')TNAME(1:ILEN-8),TNAME(ILEN-3:ILEN)
          EXT='.tmc'
          CALL FNCNGR(LTWIN(ICOMP),APE,EXT,NNAME)
          LTWIN(ICOMP)=NNAME
          CALL MKTWIN(IFU,ICOMP,QUIET,IER)
        ENDIF

C Read zone geometry file and copy this as well
        IUF=IFIL+2
        call georead(IUF,LGEOM(ICOMP),ICOMP,1,iuout,IER)
        EXT='.geo'
        CALL FNCNGR(LGEOM_O(ICOMP),APE,EXT,NNAME)
        LGEOM(ICOMP)=NNAME

C Now change constructions.
        DO 601 I=1,NZSUR(icomp)
          icc=izstocn(icomp,i)
          USE=SSUSE(ICC,1)
          IF(ssother(icc,1)(1:8).eq.'EXTERIOR')THEN
            IF(USE(1:4).EQ.'WALL')THEN
              SSMLCN(ICC)='exWall_typic'
            ELSEIF(USE(1:4).EQ.'ROOF')THEN
              SSMLCN(ICC)='Fl_roof_typi'
            ELSEIF(USE(3:6).EQ.'DOOR')THEN
              IF(USE(1:1).EQ.'V')THEN
                SSMLCN(ICC)='Vehicle_door'
              ELSE
                SSMLCN(ICC)='opDoorWind_n'
              ENDIF
            ELSEIF(USE(3:8).EQ.'WINDOW')THEN
              SSMLCN(ICC)='window_Typ_G'
              SSOTF(ICC)='Glaz_typical'
            ELSEIF(USE(1:5).EQ.'FLOOR')THEN
              SSMLCN(ICC)='Sol_grnd_typ'
            ELSEIF(USE(1:7).EQ.'F-FRAME')THEN
              SSMLCN(ICC)='frame_notion'
            ELSE
              SNM=SSNAME(ICC)
              WRITE(OUTS,'(5A)')'No relevant use defined for ',
     &        SNM(1:lnblnk(snm)),' in zone ',zname(icomp),
     &        ' setting to use: external wall in typical model'
              CALL EDISP(IUOUT,OUTS)
              SSMLCN(ICC)='exWall_typic'
            ENDIF
          ELSEIF(ssother(icc,1)(1:6).eq.'GROUND')THEN
            SSMLCN(ICC)='Sol_grnd_typ'
          ENDIF
 601    CONTINUE

C Write construction and transparent constructions files
C Write geometry file version 1.1 (1.0 does not support UK NCM)
        call geowrite(IUF,LGEOM(ICOMP),ICOMP,ITRU,3,IER)

C Update constructions and tmc file if applicable
        QUIET=.TRUE.
        CALL EDCON(ITRC,ITRU,ICOMP,QUIET,IER)
 100  CONTINUE

C Change name of configuration file to filename_typ.cfg
      CALL EMKCFG('-',IER)
      RETURN
      END

************************* SIMRUN *****************************
C Subroutine SIMRUN runs silent simulations of the actual and notional
C building as described in SBEM - EPBD documents

      SUBROUTINE SIMRUN(IMODE)
      IMPLICIT NONE
#include "sbem.h"
#include "building.h"
      COMMON/C21/IFCFG,CFGROOT,LCFGF
      INTEGER IFCFG
      CHARACTER CFGROOT*24,LCFGF*72

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

      COMMON/APPW/IAPPW,IAPPX,IAPPY
      INTEGER IAPPW,IAPPX,IAPPY

      INTEGER IER,LNBLNK,IMODE
      CHARACTER AUT*12,DOIT*248,LONGTFILE*144,LONGTFILEDOS*144
      character OUTS*248,TMODE*8
      LOGICAL CONCAT,UNIXOK
#ifdef OSI
      integer ICPMOD     ! passed from tchild
#else
      integer*8 ICPMOD     ! passed from tchild
#endif

C First run simulation
      IF(IMODE.EQ.1)THEN      ! simulating stripped building
        LCFGF=LCFGF_S
      ELSEIF(IMODE.EQ.2)THEN  ! simulating notional building
        LCFGF=LCFGF_N
      ELSEIF(IMODE.EQ.3)THEN  ! simulating reference building
        LCFGF=LCFGF_R
      ELSEIF(IMODE.EQ.4)THEN  ! simulating typical building
        LCFGF=LCFGF_T
      ENDIF
      aut=' silent'
      call tstamp('>','PRJ: beginning simulation with param set.')
      doit = ' '
      call isunix(unixok)
      if(unixok)then
        call addpath(LCFGF,longtfile,concat)
      else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
        call addpath(LCFGF,longtfile,concat)
        call cmdfiledos(longtfile,longtfiledos,ier)

C Debug the patched file name.
        write(outs,'(2a)') '* Corrected file ',
     &  longtfiledos(1:lnblnk(longtfiledos))
        call edisp248(iuout,outs,100)
        longtfile=' '
        longtfile=longtfiledos
      endif

C If on a Unix derivative computer run the assessments in text
C mode. Otherwise prj initial size is a % of default pass this
C on to child with an offset from prj start position.
      if(unixok)then
        write(doit,'(5a)') 'bps -mode text -file ',
     &    longtfile(1:lnblnk(longtfile)),' -p ',
     &    'stripped ',aut
        tmode = 'text '   ! force system call in text mode
        call edisp248(iuout,doit,100)
      else
        write(doit,'(5a)') 'bps -file ',
     &      longtfile(1:lnblnk(longtfile)),' -p ',
     &      'stripped ',aut
        tmode='graph'
        call edisp248(iuout,doit,100)
        call tchild(ICPMOD)   ! find out if in text or graphic mode
        call termode(ICPMOD,tmode)
      endif

C Run simulation with a message to the user before and after.
      IF(IMODE.EQ.1)THEN      ! simulating stripped building
        call edisp(iuout,'Simulating stripped model...')
      ELSEIF(IMODE.EQ.2)THEN  ! simulating notional building
        call edisp(iuout,'Simulating notional model...')
      ELSEIF(IMODE.EQ.3)THEN  ! simulating reference building
        call edisp(iuout,'Simulating reference model...')
      ELSEIF(IMODE.EQ.4)THEN  ! simulating typical building
        call edisp(iuout,'Simulating typical model...')
      ENDIF
      call runit(doit,tmode)

      IF(IMODE.EQ.1)THEN      ! simulating stripped building
        call edisp(iuout,'Simulating stripped model...done')
      ELSEIF(IMODE.EQ.2)THEN  ! simulating notional building
        call edisp(iuout,'Simulating notional model...done')
      ELSEIF(IMODE.EQ.3)THEN  ! simulating reference building
        call edisp(iuout,'Simulating reference model...done')
      ELSEIF(IMODE.EQ.4)THEN  ! simulating typical building
        call edisp(iuout,'Simulating typical model...done')
      ENDIF

C Now fill arrays for heating, cooling, DHW, auxiliary energy and
C lighting for this UK NCM (stripped/notional/reference/typical) model
      CALL RSL6TF(IMODE)

      RETURN
      END

************************* RSL6TF *****************************
C Subroutine RSL6TF reads an ESP-r generated save level 6 results text
C file. Any changes made to the save level 6 file writing in mzout6.F
C will affect this subroutine so should be taken into account.
C This subroutine fills requisite common blocks for UK NCM calculations
C commons for the following models are filled depending upon IMODE
C IMODE = 1 -> Stripped model
C IMODE = 2 -> Notional model
C IMODE = 3 -> Reference model
C IMODE = 4 -> Typical model
      SUBROUTINE RSL6TF(IMODE)
      IMPLICIT NONE
#include "sbem.h"
#include "building.h"
#include "geometry.h"
#include "espriou.h"
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      INTEGER IFIL

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

      common/C1/NCOMP,NCON
      INTEGER NCOMP,NCON

      COMMON/C21/IFCFG,CFGROOT,LCFGF
      INTEGER IFCFG
      CHARACTER CFGROOT*24,LCFGF*72

      common/pophelp/h(60)
      character h*72

      INTEGER IUR,ND,ISTAT,K,LTW2,ICDHW,IER,IMODE,
     & ICHT,ICCL,ICLT,ICAUX,LZNL,IM,Istart,Ifinish,INDX,icomp
      CHARACTER LTXTF*72,OUTSTR*248,WORD1*72,WORD2*24,WORD3*24,
     & LASTZONE*12,TWOL*2,outs*124
      LOGICAL EOF,Getmonth,cl
      REAL VALZ,tbasea,Heating_efficiency,Cooling_efficiency,
     & DHW_efficiency,total_Heat_S,total_Cool_S,total_DHW_S,total_Light,
     & total_Aux,total_Heat_D,total_Cool_D,total_DHW_D
      integer IZNOVERHEAT !counter for zones to report overheating
      REAL A,B,C,D,E,F,G ! Annual parameters
      REAL T(12),U(12),V(12),W(12),XX(12),YY(12),ZZ(12) ! Monthly parameters

C Monthly averaging parameters
      REAL DHW_Supply(12), DHW_Demand(12)
      REAL Heat_Supply(12), Heat_Demand(12)
      REAL Cool_Supply(12), Cool_Demand(12)
      REAL Aux_Supply(12)
      REAL Light_Supply(12)
      DO 34 IM=1,12
        DHW_Supply(IM)  =0. ; DHW_Demand(IM)  =0.
        Heat_Supply(IM) =0. ; Heat_Demand(IM) =0.
        Cool_Supply(IM) =0. ; Cool_Demand(IM) =0.
        Aux_Supply(IM)  =0.
        Light_Supply(IM)=0.
 34   CONTINUE
      total_Heat_S=0. ; total_Cool_S=0. ; total_DHW_S=0.
      total_Heat_D=0. ; total_Cool_D=0. ; total_DHW_D=0.
      total_Light=0. ; total_Aux=0.

C Get text file name and initialise variables
      IUR=IFIL+1
      WRITE(LTXTF,'(2A)')LCFGF(1:LNBLNK(LCFGF)-4),'.txt'
      CALL EFOPSEQ(IUR,LTXTF,1,IER)
      if(ier.eq.0)THEN
        write(currentfile,'(a)') LTXTF(1:lnblnk(LTXTF))
      ELSE ! warn if file could not be opened
        write(outs,'(2a)') 'could not open ',ltxtf(1:72)
        call edisp(iuout,outs)
        H(1)='name of the text result file'
        CALL EASKS(LTXTF,'Name of text results file?',
     &    '(<72 characters)',72,'result.txt','text res file',IER,1)
        CALL EFOPSEQ(IUR,LTXTF,1,IER)
        if(ier.eq.0)THEN
        write(currentfile,'(a)') LTXTF(1:lnblnk(LTXTF))
        else
          call usrmsg('Unable to file text result file named',
     &      ltxtf,'W')
          return
        endif
      ENDIF

      EOF=.FALSE.
      LASTZONE=ZNAME(NCOMP)
      LZNL=LNBLNK(LASTZONE)

C Individual indices for DHW, heating etc. required because of the way
C the save level 6 results file is formatted
      ICDHW=1 ; ICHT=1 ; ICCL=1 ; ICLT=1 ; ICAUX=1
      IZNOVERHEAT=1

C Read the first line of the file (which is only text).
      K=0
      CALL STRIPC(IUR,OUTSTR,99,ND,1,'res 6 line 1',IER)

C Loop until the end of the file detected.
      DO WHILE(.NOT.EOF)

C Set up efficiencies according to Modelling Guide for the various
C models
        if(IHLZ(ICHT).eq.0)then
          Heating_efficiency=0.0
        else

C Heated and Mechanically ventilated
        IF(INCMSYS(IHLZ(ICHT)).EQ.4)THEN
          IF(IMODE.EQ.2)THEN
            Heating_efficiency=0.78
          ELSEIF(IMODE.EQ.3)THEN
            Heating_efficiency=0.73
          ELSEIF(IMODE.EQ.4)THEN
            Heating_efficiency=0.55
          ENDIF

C Heated and naturally ventilated (or heated only)
        ELSEIF(INCMSYS(IHLZ(ICHT)).LE.11)THEN
          IF(IMODE.EQ.2)THEN
            Heating_efficiency=0.73
          ELSEIF(IMODE.EQ.3)THEN
            Heating_efficiency=0.73
          ELSEIF(IMODE.EQ.4)THEN
            Heating_efficiency=0.55
          ENDIF

C Air conditioned
        ELSE
          IF(IMODE.EQ.2)THEN
            Heating_efficiency=0.83
          ELSEIF(IMODE.EQ.3)THEN
            Heating_efficiency=0.73
          ELSEIF(IMODE.EQ.4)THEN
            Heating_efficiency=0.55
          ENDIF
        ENDIF
        endif

C Heated and Mechanically ventilated.
        if(IHLZ(ICCL).eq.0)then
          Cooling_efficiency=0.  ! if no system set eff to zero
        else
        IF(INCMSYS(IHLZ(ICCL)).EQ.4)THEN
          IF(IMODE.EQ.2)THEN
            Cooling_efficiency=0.
          ELSEIF(IMODE.EQ.3)THEN
            Cooling_efficiency=2.25
          ELSEIF(IMODE.EQ.4)THEN
            Cooling_efficiency=0.
          ENDIF

C Heated and naturally ventilated (or heated only)
        ELSEIF(INCMSYS(IHLZ(ICCL)).LE.11)THEN
          IF(IMODE.EQ.2)THEN
            Cooling_efficiency=0.
          ELSEIF(IMODE.EQ.3)THEN
            Cooling_efficiency=2.25
          ELSEIF(IMODE.EQ.4)THEN
            Cooling_efficiency=0.
          ENDIF

C Air conditioned
        ELSE
          IF(IMODE.EQ.2)THEN
            Cooling_efficiency=1.67
          ELSEIF(IMODE.EQ.3)THEN
            Cooling_efficiency=2.25
          ELSEIF(IMODE.EQ.4)THEN
            Cooling_efficiency=1.17
          ENDIF
        ENDIF
        endif

        IF(IMODE.EQ.1)THEN
          IF(IDHWLZ(ICDHW).EQ.0)IDHWLZ(ICDHW)=-1*IHLZ(ICDHW)
          IF(IDHWLZ(ICDHW).GT.0)THEN
            DHW_efficiency=HWEF(IDHWLZ(ICDHW))
          ELSEIF(IDHWLZ(ICDHW).LT.0)THEN
            INDX=-1*IDHWLZ(ICDHW)
            DHW_efficiency=HGEF(INDX)
          ENDIF
          if(iDsmTestingFlag.eq.2)then !reference building (need something somthing similar for
                                       !notional?)
            Heating_efficiency=0.73
            Cooling_efficiency=2.25
          else
            if(IHLZ(ICHT).eq.0)then
              Heating_efficiency=0.0
            else
            Heating_efficiency=HGEF(IHLZ(ICHT))
            endif
            if(IHLZ(ICCL).eq.0)then
              Cooling_efficiency=0.0
            else
            Cooling_efficiency=CGEF(IHLZ(ICCL))
          endif
          endif
        ELSE
          IF(IDHWLZ(ICDHW).EQ.0)IDHWLZ(ICDHW)=-1*IHLZ(ICDHW)
          IF(IDHWLZ(ICDHW).GT.0)THEN
            DHW_efficiency=0.45
          ELSEIF(IDHWLZ(ICDHW).LT.0)THEN
            INDX=-1*IDHWLZ(ICDHW)
            DHW_efficiency=Heating_efficiency
            DHW_efficiency=HeatSCoP_N(IHLZ(ICDHW))
          ENDIF
        ENDIF

C If DSM testing then DHW efficiency is always 45%
        if(iDsmTestingFlag.gt.0)DHW_efficiency=0.45

C Expect to find three strings and one real on each line. The first
C line of the file is only for documentation.
        K=0
        CALL STRIPC(IUR,OUTSTR,99,ND,1,'res 6 line 2+',IER)
        CALL EGETW(OUTSTR,K,WORD1,'W','res 6 first word',IER)
        CALL EGETW(OUTSTR,K,WORD2,'W','res 6 second word',IER)
        CALL EGETW(OUTSTR,K,WORD3,'W','res 6 third word',IER)
        LTW2=LNBLNK(WORD2)
        IF(WORD3(1:3).NE.'key')
     &  CALL EGETWR(OUTSTR,K,VALZ,1.,-1.,'-','res 6 fourth real',IER)

C Get month no. for DHW, aux and lights
        Getmonth=.False.
        IF(WORD3(1:9).EQ.'z_Aux_Mon')THEN
          Istart=13 ; Ifinish=14
          Getmonth=.true.
        ELSEIF(WORD3(1:12).EQ.'z_DHW_Month_')then

C z_DHW_Month_ can be followed by kWh or MJ.
          if(WORD3(15:17).EQ.'kWh'.OR.WORD3(16:18).EQ.'kWh'.or.
     &       WORD3(15:16).EQ.'MJ'.OR.WORD3(16:17).EQ.'MJ')THEN
          Istart=13 ; Ifinish=14
          Getmonth=.true.
          endif
        ELSEIF(WORD3(1:9).EQ.'z_Lights')THEN
          Istart=16 ; Ifinish=17
          Getmonth=.true.
        ENDIF
        TWOL=WORD3(Istart:Ifinish)  ! copy out the two characters for month
        IF(Getmonth)THEN
          IF(TWOL.EQ.'1_')THEN
            IM=1
          ELSEIF(TWOL.EQ.'2_')THEN
            IM=2
          ELSEIF(TWOL.EQ.'3_')THEN
            IM=3
          ELSEIF(TWOL.EQ.'4_')THEN
            IM=4
          ELSEIF(TWOL.EQ.'5_')THEN
            IM=5
          ELSEIF(TWOL.EQ.'6_')THEN
            IM=6
          ELSEIF(TWOL.EQ.'7_')THEN
            IM=7
          ELSEIF(TWOL.EQ.'8_')THEN
            IM=8
          ELSEIF(TWOL.EQ.'9_')THEN
            IM=9
          ELSEIF(TWOL.EQ.'10')THEN
            IM=10
          ELSEIF(TWOL.EQ.'11')THEN
            IM=11
          ELSEIF(TWOL.EQ.'12')THEN
            IM=12
          ELSE
            write(98,*)'Month not found'
          ENDIF
        ENDIF

C Debug.
C        write(6,*) IM,'word3 is ',WORD3

C Get monthly lighting loads
        IF(WORD3(1:14).EQ.'z_Lights_Month')THEN
          Light_Supply(IM)=Light_Supply(IM)+VALZ
          total_Light=total_Light+VALZ

C Get monthly auxiliary loads
        ELSEIF(WORD3(1:11).EQ.'z_Aux_Month')THEN
          Aux_Supply(IM)=Aux_Supply(IM)+VALZ
          total_Aux=total_Aux+VALZ

C Get monthly DHW loads (if expressed in kWh).
        ELSEIF(WORD3(1:12).EQ.'z_DHW_Month_'.AND.
     &         (WORD3(15:17).EQ.'kWh'.OR.WORD3(16:18).EQ.'kWh'))THEN
          DHW_Demand(IM)=DHW_Demand(IM)+VALZ
          DHW_Supply(IM)=DHW_Supply(IM)+VALZ/DHW_efficiency
          total_DHW_S=total_DHW_S+VALZ/DHW_efficiency
          total_DHW_D=total_DHW_D+VALZ

C Get DHW energy
        ELSEIF(WORD3(1:9).EQ.'z_DHW_kWh')THEN
          IF(IMODE.EQ.1)THEN
            BERDHW(ICDHW)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERDHW(ICDHW)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERDHW(ICDHW)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERDHW(ICDHW)=VALZ
          ENDIF
          ICDHW=ICDHW+1

C Get Heating energy
        ELSEIF(WORD3(1:4).EQ.'MH12')THEN
          IF(IMODE.EQ.1)THEN
            BERHEATM(ICHT,12)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERHEATM(ICHT,12)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERHEATM(ICHT,12)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERHEATM(ICHT,12)=VALZ
          ENDIF
          Heat_Demand(12)=Heat_Demand(12)+VALZ*zbasea(icht)
          call eclose(0.,Heating_efficiency,0.001,cl)
          if(.not.cl)then
            Heat_Supply(12)=Heat_Supply(12)+VALZ*zbasea(icht)/
     &       Heating_efficiency
            total_Heat_S=total_Heat_S+VALZ*zbasea(icht)/
     &       Heating_efficiency
          else
            Heat_Supply(12)=Heat_Supply(12)+0.
            total_Heat_S=total_Heat_S+0.
          endif
          total_Heat_D=total_Heat_D+VALZ*zbasea(icht)
        ELSEIF(WORD3(1:4).EQ.'MH11')THEN
          IF(IMODE.EQ.1)THEN
            BERHEATM(ICHT,11)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERHEATM(ICHT,11)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERHEATM(ICHT,11)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERHEATM(ICHT,11)=VALZ
          ENDIF
          Heat_Demand(11)=Heat_Demand(11)+VALZ*zbasea(icht)
          call eclose(0.,Heating_efficiency,0.001,cl)
          if(.not.cl)then
            Heat_Supply(11)=Heat_Supply(11)+VALZ*zbasea(icht)/
     &       Heating_efficiency
            total_Heat_S=total_Heat_S+VALZ*zbasea(icht)/
     &       Heating_efficiency
          else
            Heat_Supply(11)=Heat_Supply(11)+0.
            total_Heat_S=total_Heat_S+0.
          endif
          total_Heat_D=total_Heat_D+VALZ*zbasea(icht)
        ELSEIF(WORD3(1:4).EQ.'MH10')THEN
          IF(IMODE.EQ.1)THEN
            BERHEATM(ICHT,10)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERHEATM(ICHT,10)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERHEATM(ICHT,10)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERHEATM(ICHT,10)=VALZ
          ENDIF
          Heat_Demand(10)=Heat_Demand(10)+VALZ*zbasea(icht)
          call eclose(0.,Heating_efficiency,0.001,cl)
          if(.not.cl)then
            Heat_Supply(10)=Heat_Supply(10)+VALZ*zbasea(icht)/
     &       Heating_efficiency
            total_Heat_S=total_Heat_S+VALZ*zbasea(icht)/
     &       Heating_efficiency
          else
            Heat_Supply(10)=Heat_Supply(10)+0.
            total_Heat_S=total_Heat_S+0.
          endif
          total_Heat_D=total_Heat_D+VALZ*zbasea(icht)
        ELSEIF(WORD3(1:3).EQ.'MH9')THEN
          IF(IMODE.EQ.1)THEN
            BERHEATM(ICHT,9)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERHEATM(ICHT,9)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERHEATM(ICHT,9)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERHEATM(ICHT,9)=VALZ
          ENDIF
          Heat_Demand(9)=Heat_Demand(9)+VALZ*zbasea(icht)
          call eclose(0.,Heating_efficiency,0.001,cl)
          if(.not.cl)then
            Heat_Supply(9)=Heat_Supply(9)+VALZ*zbasea(icht)/
     &       Heating_efficiency
            total_Heat_S=total_Heat_S+VALZ*zbasea(icht)/
     &       Heating_efficiency
          else
            Heat_Supply(9)=Heat_Supply(9)+0.
            total_Heat_S=total_Heat_S+0.
          endif
          total_Heat_D=total_Heat_D+VALZ*zbasea(icht)
        ELSEIF(WORD3(1:3).EQ.'MH8')THEN
          IF(IMODE.EQ.1)THEN
            BERHEATM(ICHT,8)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERHEATM(ICHT,8)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERHEATM(ICHT,8)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERHEATM(ICHT,8)=VALZ
          ENDIF
          Heat_Demand(8)=Heat_Demand(8)+VALZ*zbasea(icht)
          call eclose(0.,Heating_efficiency,0.001,cl)
          if(.not.cl)then
            Heat_Supply(8)=Heat_Supply(8)+VALZ*zbasea(icht)/
     &        Heating_efficiency
            total_Heat_S=total_Heat_S+VALZ*zbasea(icht)/
     &        Heating_efficiency
          else
            Heat_Supply(8)=Heat_Supply(8)+0.
            total_Heat_S=total_Heat_S+0.
          endif
          total_Heat_D=total_Heat_D+VALZ*zbasea(icht)
        ELSEIF(WORD3(1:3).EQ.'MH7')THEN
          IF(IMODE.EQ.1)THEN
            BERHEATM(ICHT,7)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERHEATM(ICHT,7)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERHEATM(ICHT,7)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERHEATM(ICHT,7)=VALZ
          ENDIF
          Heat_Demand(7)=Heat_Demand(7)+VALZ*zbasea(icht)
          call eclose(0.,Heating_efficiency,0.001,cl)
          if(.not.cl)then
            Heat_Supply(7)=Heat_Supply(7)+VALZ*zbasea(icht)/
     &       Heating_efficiency
            total_Heat_S=total_Heat_S+VALZ*zbasea(icht)/
     &       Heating_efficiency
          else
            Heat_Supply(7)=Heat_Supply(7)+0.
            total_Heat_S=total_Heat_S+0.
          endif
          total_Heat_D=total_Heat_D+VALZ*zbasea(icht)
        ELSEIF(WORD3(1:3).EQ.'MH6')THEN
          IF(IMODE.EQ.1)THEN
            BERHEATM(ICHT,6)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERHEATM(ICHT,6)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERHEATM(ICHT,6)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERHEATM(ICHT,6)=VALZ
          ENDIF
          Heat_Demand(6)=Heat_Demand(6)+VALZ*zbasea(icht)
          call eclose(0.,Heating_efficiency,0.001,cl)
          if(.not.cl)then
            Heat_Supply(6)=Heat_Supply(6)+VALZ*zbasea(icht)/
     &       Heating_efficiency
            total_Heat_S=total_Heat_S+VALZ*zbasea(icht)/
     &       Heating_efficiency
          else
            Heat_Supply(6)=Heat_Supply(6)+0.
            total_Heat_S=total_Heat_S+0.
          endif
          total_Heat_D=total_Heat_D+VALZ*zbasea(icht)
        ELSEIF(WORD3(1:3).EQ.'MH5')THEN
          IF(IMODE.EQ.1)THEN
            BERHEATM(ICHT,5)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERHEATM(ICHT,5)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERHEATM(ICHT,5)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERHEATM(ICHT,5)=VALZ
          ENDIF
          Heat_Demand(5)=Heat_Demand(5)+VALZ*zbasea(icht)
          call eclose(0.,Heating_efficiency,0.001,cl)
          if(.not.cl)then
            Heat_Supply(5)=Heat_Supply(5)+VALZ*zbasea(icht)/
     &       Heating_efficiency
            total_Heat_S=total_Heat_S+VALZ*zbasea(icht)/
     &       Heating_efficiency
          else
            Heat_Supply(5)=Heat_Supply(5)+0.
            total_Heat_S=total_Heat_S+0.
          endif
          total_Heat_D=total_Heat_D+VALZ*zbasea(icht)
        ELSEIF(WORD3(1:3).EQ.'MH4')THEN
          IF(IMODE.EQ.1)THEN
            BERHEATM(ICHT,4)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERHEATM(ICHT,4)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERHEATM(ICHT,4)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERHEATM(ICHT,4)=VALZ
          ENDIF
          Heat_Demand(4)=Heat_Demand(4)+VALZ*zbasea(icht)
          call eclose(0.,Heating_efficiency,0.001,cl)
          if(.not.cl)then
            Heat_Supply(4)=Heat_Supply(4)+VALZ*zbasea(icht)/
     &       Heating_efficiency
            total_Heat_S=total_Heat_S+VALZ*zbasea(icht)/
     &       Heating_efficiency
          else
            Heat_Supply(4)=Heat_Supply(4)+0.
            total_Heat_S=total_Heat_S+0.
          endif
          total_Heat_D=total_Heat_D+VALZ*zbasea(icht)
        ELSEIF(WORD3(1:3).EQ.'MH3')THEN
          IF(IMODE.EQ.1)THEN
            BERHEATM(ICHT,3)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERHEATM(ICHT,3)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERHEATM(ICHT,3)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERHEATM(ICHT,3)=VALZ
          ENDIF
          Heat_Demand(3)=Heat_Demand(3)+VALZ*zbasea(icht)
          call eclose(0.,Heating_efficiency,0.001,cl)
          if(.not.cl)then
            Heat_Supply(3)=Heat_Supply(3)+VALZ*zbasea(icht)/
     &       Heating_efficiency
            total_Heat_S=total_Heat_S+VALZ*zbasea(icht)/
     &       Heating_efficiency
          else
            Heat_Supply(3)=Heat_Supply(3)+0.
            total_Heat_S=total_Heat_S+0.
          endif
          total_Heat_D=total_Heat_D+VALZ*zbasea(icht)
        ELSEIF(WORD3(1:3).EQ.'MH2')THEN
          IF(IMODE.EQ.1)THEN
            BERHEATM(ICHT,2)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERHEATM(ICHT,2)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERHEATM(ICHT,2)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERHEATM(ICHT,2)=VALZ
          ENDIF
          Heat_Demand(2)=Heat_Demand(2)+VALZ*zbasea(icht)
          call eclose(0.,Heating_efficiency,0.001,cl)
          if(.not.cl)then
            Heat_Supply(2)=Heat_Supply(2)+VALZ*zbasea(icht)/
     &       Heating_efficiency
            total_Heat_S=total_Heat_S+VALZ*zbasea(icht)/
     &       Heating_efficiency
          else
            Heat_Supply(2)=Heat_Supply(2)+0.
            total_Heat_S=total_Heat_S+0.
          endif
          total_Heat_D=total_Heat_D+VALZ*zbasea(icht)
        ELSEIF(WORD3(1:3).EQ.'MH1')THEN
          IF(IMODE.EQ.1)THEN
            BERHEATM(ICHT,1)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERHEATM(ICHT,1)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERHEATM(ICHT,1)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERHEATM(ICHT,1)=VALZ
          ENDIF
          Heat_Demand(1)=Heat_Demand(1)+VALZ*zbasea(icht)
          call eclose(0.,Heating_efficiency,0.001,cl)
          if(.not.cl)then
            Heat_Supply(1)=Heat_Supply(1)+VALZ*zbasea(icht)/
     &       Heating_efficiency
            total_Heat_S=total_Heat_S+VALZ*zbasea(icht)/
     &       Heating_efficiency
          else
            Heat_Supply(1)=Heat_Supply(1)+0.
            total_Heat_S=total_Heat_S+0.
          endif
          total_Heat_D=total_Heat_D+VALZ*zbasea(icht)
        ELSEIF(WORD3(1:21).EQ.'integrZAHforFloorArea')THEN
          IF(IMODE.EQ.1)THEN
            BERHEAT(ICHT)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERHEAT(ICHT)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERHEAT(ICHT)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERHEAT(ICHT)=VALZ
          ENDIF
          ICHT=ICHT+1

C Get Cooling energy
        ELSEIF(WORD3(1:4).EQ.'MC12')THEN
          IF(IMODE.EQ.1)THEN
            BERCOOLM(ICCL,12)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERCOOLM(ICCL,12)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERCOOLM(ICCL,12)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERCOOLM(ICCL,12)=VALZ
          ENDIF
          Cool_Demand(12)=Cool_Demand(12)+VALZ*zbasea(icht)
          call eclose(0.,Cooling_efficiency,0.001,cl)
          if(.not.cl)then
          Cool_Supply(12)=Cool_Supply(12)+VALZ*zbasea(icht)
     &    /Cooling_efficiency
          total_Cool_S=total_Cool_S+VALZ*zbasea(icht)/Cooling_efficiency
          total_Cool_D=total_Cool_D+VALZ*zbasea(icht)
          endif
        ELSEIF(WORD3(1:4).EQ.'MC11')THEN
          IF(IMODE.EQ.1)THEN
            BERCOOLM(ICCL,11)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERCOOLM(ICCL,11)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERCOOLM(ICCL,11)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERCOOLM(ICCL,11)=VALZ
          ENDIF
          Cool_Demand(11)=Cool_Demand(11)+VALZ*zbasea(icht)
          call eclose(0.,Cooling_efficiency,0.001,cl)
          if(.not.cl)then
          Cool_Supply(11)=Cool_Supply(11)+VALZ*zbasea(icht)
     &    /Cooling_efficiency
          total_Cool_S=total_Cool_S+VALZ*zbasea(icht)/Cooling_efficiency
          total_Cool_D=total_Cool_D+VALZ*zbasea(icht)
          endif
        ELSEIF(WORD3(1:4).EQ.'MC10')THEN
          IF(IMODE.EQ.1)THEN
            BERCOOLM(ICCL,10)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERCOOLM(ICCL,10)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERCOOLM(ICCL,10)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERCOOLM(ICCL,10)=VALZ
          ENDIF
          Cool_Demand(10)=Cool_Demand(10)+VALZ*zbasea(icht)
          call eclose(0.,Cooling_efficiency,0.001,cl)
          if(.not.cl)then
          Cool_Supply(10)=Cool_Supply(10)+VALZ*zbasea(icht)
     &    /Cooling_efficiency
          total_Cool_S=total_Cool_S+VALZ*zbasea(icht)/Cooling_efficiency
          total_Cool_D=total_Cool_D+VALZ*zbasea(icht)
          endif
        ELSEIF(WORD3(1:3).EQ.'MC9')THEN
          IF(IMODE.EQ.1)THEN
            BERCOOLM(ICCL,9)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERCOOLM(ICCL,9)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERCOOLM(ICCL,9)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERCOOLM(ICCL,9)=VALZ
          ENDIF
          Cool_Demand(9)=Cool_Demand(9)+VALZ*zbasea(icht)
          call eclose(0.,Cooling_efficiency,0.001,cl)
          if(.not.cl)then
          Cool_Supply(9)=Cool_Supply(9)+VALZ*zbasea(icht)
     &    /Cooling_efficiency
          total_Cool_S=total_Cool_S+VALZ*zbasea(icht)/Cooling_efficiency
          total_Cool_D=total_Cool_D+VALZ*zbasea(icht)
          endif
        ELSEIF(WORD3(1:3).EQ.'MC8')THEN
          IF(IMODE.EQ.1)THEN
            BERCOOLM(ICCL,8)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERCOOLM(ICCL,8)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERCOOLM(ICCL,8)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERCOOLM(ICCL,8)=VALZ
          ENDIF
          Cool_Demand(8)=Cool_Demand(8)+VALZ*zbasea(icht)
          call eclose(0.,Cooling_efficiency,0.001,cl)
          if(.not.cl)then
          Cool_Supply(8)=Cool_Supply(8)+VALZ*zbasea(icht)
     &    /Cooling_efficiency
          total_Cool_S=total_Cool_S+VALZ*zbasea(icht)/Cooling_efficiency
          total_Cool_D=total_Cool_D+VALZ*zbasea(icht)
          endif
        ELSEIF(WORD3(1:3).EQ.'MC7')THEN
          IF(IMODE.EQ.1)THEN
            BERCOOLM(ICCL,7)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERCOOLM(ICCL,7)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERCOOLM(ICCL,7)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERCOOLM(ICCL,7)=VALZ
          ENDIF
          Cool_Demand(7)=Cool_Demand(7)+VALZ*zbasea(icht)
          call eclose(0.,Cooling_efficiency,0.001,cl)
          if(.not.cl)then
          Cool_Supply(7)=Cool_Supply(7)+VALZ*zbasea(icht)
     &    /Cooling_efficiency
          total_Cool_S=total_Cool_S+VALZ*zbasea(icht)/Cooling_efficiency
          total_Cool_D=total_Cool_D+VALZ*zbasea(icht)
          endif
        ELSEIF(WORD3(1:3).EQ.'MC6')THEN
          IF(IMODE.EQ.1)THEN
            BERCOOLM(ICCL,6)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERCOOLM(ICCL,6)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERCOOLM(ICCL,6)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERCOOLM(ICCL,6)=VALZ
          ENDIF
          Cool_Demand(6)=Cool_Demand(6)+VALZ*zbasea(icht)
          call eclose(0.,Cooling_efficiency,0.001,cl)
          if(.not.cl)then
          Cool_Supply(6)=Cool_Supply(6)+VALZ*zbasea(icht)
     &    /Cooling_efficiency
          total_Cool_S=total_Cool_S+VALZ*zbasea(icht)/Cooling_efficiency
          total_Cool_D=total_Cool_D+VALZ*zbasea(icht)
          endif
        ELSEIF(WORD3(1:3).EQ.'MC5')THEN
          IF(IMODE.EQ.1)THEN
            BERCOOLM(ICCL,5)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERCOOLM(ICCL,5)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERCOOLM(ICCL,5)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERCOOLM(ICCL,5)=VALZ
          ENDIF
          Cool_Demand(5)=Cool_Demand(5)+VALZ*zbasea(icht)
          call eclose(0.,Cooling_efficiency,0.001,cl)
          if(.not.cl)then
          Cool_Supply(5)=Cool_Supply(5)+VALZ*zbasea(icht)
     &    /Cooling_efficiency
          total_Cool_S=total_Cool_S+VALZ*zbasea(icht)/Cooling_efficiency
          total_Cool_D=total_Cool_D+VALZ*zbasea(icht)
          endif
        ELSEIF(WORD3(1:3).EQ.'MC4')THEN
          IF(IMODE.EQ.1)THEN
            BERCOOLM(ICCL,4)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERCOOLM(ICCL,4)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERCOOLM(ICCL,4)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERCOOLM(ICCL,4)=VALZ
          ENDIF
          Cool_Demand(4)=Cool_Demand(4)+VALZ*zbasea(icht)
          call eclose(0.,Cooling_efficiency,0.001,cl)
          if(.not.cl)then
          Cool_Supply(4)=Cool_Supply(4)+VALZ*zbasea(icht)
     &    /Cooling_efficiency
          total_Cool_S=total_Cool_S+VALZ*zbasea(icht)/Cooling_efficiency
          total_Cool_D=total_Cool_D+VALZ*zbasea(icht)
          endif
        ELSEIF(WORD3(1:3).EQ.'MC3')THEN
          IF(IMODE.EQ.1)THEN
            BERCOOLM(ICCL,3)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERCOOLM(ICCL,3)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERCOOLM(ICCL,3)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERCOOLM(ICCL,3)=VALZ
          ENDIF
          Cool_Demand(3)=Cool_Demand(3)+VALZ*zbasea(icht)
          call eclose(0.,Cooling_efficiency,0.001,cl)
          if(.not.cl)then
          Cool_Supply(3)=Cool_Supply(3)+VALZ*zbasea(icht)
     &    /Cooling_efficiency
          total_Cool_S=total_Cool_S+VALZ*zbasea(icht)/Cooling_efficiency
          total_Cool_D=total_Cool_D+VALZ*zbasea(icht)
          endif
        ELSEIF(WORD3(1:3).EQ.'MC2')THEN
          IF(IMODE.EQ.1)THEN
            BERCOOLM(ICCL,2)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERCOOLM(ICCL,2)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERCOOLM(ICCL,2)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERCOOLM(ICCL,2)=VALZ
          ENDIF
          Cool_Demand(2)=Cool_Demand(2)+VALZ*zbasea(icht)
          call eclose(0.,Cooling_efficiency,0.001,cl)
          if(.not.cl)then
          Cool_Supply(2)=Cool_Supply(2)+VALZ*zbasea(icht)
     &    /Cooling_efficiency
          total_Cool_S=total_Cool_S+VALZ*zbasea(icht)/Cooling_efficiency
          total_Cool_D=total_Cool_D+VALZ*zbasea(icht)
          endif
        ELSEIF(WORD3(1:3).EQ.'MC1')THEN
          IF(IMODE.EQ.1)THEN
            BERCOOLM(ICCL,1)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERCOOLM(ICCL,1)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERCOOLM(ICCL,1)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERCOOLM(ICCL,1)=VALZ
          ENDIF
          Cool_Demand(1)=Cool_Demand(1)+VALZ*zbasea(icht)
          call eclose(0.,Cooling_efficiency,0.001,cl)
          if(.not.cl)then
          Cool_Supply(1)=Cool_Supply(1)+VALZ*zbasea(icht)
     &    /Cooling_efficiency
          total_Cool_S=total_Cool_S+VALZ*zbasea(icht)/Cooling_efficiency
          total_Cool_D=total_Cool_D+VALZ*zbasea(icht)
          endif
        ELSEIF(WORD3(1:21).EQ.'integrZACforFloorArea')THEN
          IF(IMODE.EQ.1)THEN
            BERCOOL(ICCL)=VALZ
          ELSEIF(IMODE.EQ.2)THEN
            AERCOOL(ICCL)=VALZ
          ELSEIF(IMODE.EQ.3)THEN
            RERCOOL(ICCL)=VALZ
          ELSEIF(IMODE.EQ.4)THEN
            TyERCOOL(ICCL)=VALZ
          ENDIF
          ICCL=ICCL+1

C Get Auxiliary energy
        ELSEIF(WORD3(1:17).EQ.'z_Auxiliary_kWhm2')THEN
          IF(IMODE.EQ.1)THEN
            BERAUX(ICAUX)=VALZ*zbasea(icaux)
          ELSEIF(IMODE.EQ.2)THEN
            AERAUX(ICAUX)=VALZ*zbasea(icaux)
          ELSEIF(IMODE.EQ.3)THEN
            RERAUX(ICAUX)=VALZ*zbasea(icaux)
          ELSEIF(IMODE.EQ.4)THEN
            TyERAUX(ICAUX)=VALZ*zbasea(icaux)
          ENDIF
          ICAUX=ICAUX+1

C Get lighting energy
        ELSEIF(WORD3(1:16).EQ.'z_ReqLight_kWhm2')THEN
          IF(IMODE.EQ.1)THEN
            BERLIGHT(ICLT)=VALZ*zbasea(iclt)
          ELSEIF(IMODE.EQ.2)THEN
            AERLIGHT(ICLT)=VALZ*zbasea(iclt)
          ELSEIF(IMODE.EQ.4)THEN
            TyERLIGHT(ICLT)=VALZ*zbasea(iclt)
          ENDIF
          ICLT=ICLT+1
        ELSEIF(WORD3(1:24).EQ.'Overh_PercentOcc_Above27')THEN
          IF(IMODE.EQ.1)THEN
            prcnt_Overheat(IZNOVERHEAT)=VALZ
          ENDIF
          IZNOVERHEAT=IZNOVERHEAT+1
        ENDIF

C Check to see if end of file reached
        IF(WORD2(1:LTW2).EQ.LASTZONE(1:LZNL).AND.WORD3(7:9).EQ.'ZAC')
     &  EOF=.TRUE.
      ENDDO
      CALL ERPFREE(IUR,ISTAT)

c Integrate for total energy
      tbasea=0.
      DO 60 ICOMP=1,NCOMP
        TBASEA=TBASEA+ZBASEA(ICOMP)
 60   CONTINUE

C Debug.
C      write(6,*) 'prior to call to berter '
C      write(6,*) 'monthly heating demand ',Heat_Demand
C      write(6,*) 'monthly heating supply ',Heat_Supply
C      write(6,*) 'total heat s',total_Heat_S
C      write(6,*) 'monthly cooling demand ',Cool_Demand
C      write(6,*) 'monthly cooling supply ',Cool_Supply
C      write(6,*) 'total cool s',total_Cool_S
C      write(6,*) 'bercool ',bercool
C      write(6,*) 'berheat ',berheat
C      write(6,*) 'aerheat ',aerheat
C      write(6,*) 'AER is ',AER

      IF(iDsmTestingFlag.GT.0)THEN

c Call BERTER for calculating BER if in iDSM mode
        CALL BERTER(IER)

C Debug.
C      write(6,*) 'after call to berter '
C      write(6,*) 'monthly heating demand ',Heat_Demand
C      write(6,*) 'monthly heating supply ',Heat_Supply
C      write(6,*) 'total heat s',total_Heat_S
C      write(6,*) 'monthly cooling demand ',Cool_Demand
C      write(6,*) 'monthly cooling supply ',Cool_Supply
C      write(6,*) 'total cool s',total_Cool_S
C      write(6,*) 'bercool ',bercool
C      write(6,*) 'berheat ',berheat
C      write(6,*) 'aerheat ',aerheat
C      write(6,*) 'AER is ',AER

        IF(IMODE.EQ.1)THEN
          write(98,*)'writing,stripped,model,from,file ',LTXTF,',,,,'
        ELSEIF(IMODE.EQ.2)THEN
          write(98,*)',,,,'
          write(98,*)',,,,'
          write(98,*)'writing,notional,model,from,file ',LTXTF,',,,,'
        ELSEIF(IMODE.EQ.3)THEN
          write(98,*)',,,,'
          write(98,*)',,,,'
          write(98,*)'writing,reference,model,from,file,',LTXTF,',,,,'
        ELSEIF(IMODE.EQ.4)THEN
          write(98,*)',,,,'
          write(98,*)',,,,'
          write(98,*)'writing,typical,model,from,file,',LTXTF,',,,,'
        ENDIF
        WRITE(98,*)'Supply,tbasea=,',tbasea,',,'
        WRITE(98,*)'Heat,Cool,DHW,Aux,Lights'
        DO 70 IM=1,12
          WRITE(98,*)Heat_Supply(IM)/tbasea,',',Cool_Supply(IM)/tbasea,
     &    ',',DHW_Supply(IM)/tbasea,',',Aux_Supply(IM)/tbasea,',',
     &    Light_Supply(IM)/tbasea
          T(IM)=Heat_Supply(IM)/tbasea
          U(IM)=-1.*Cool_Supply(IM)/tbasea
          V(IM)=DHW_Supply(IM)/tbasea
          W(IM)=Aux_Supply(IM)/tbasea
          XX(IM)=Light_Supply(IM)/tbasea
          YY(IM)=T(IM)+V(IM) ! gas load 
          ZZ(IM)=W(IM)+XX(IM)+U(IM) ! electricity load
 70     CONTINUE
        A=total_Heat_S/tbasea
        B=-1.*total_Cool_S/tbasea
        C=total_DHW_S/tbasea
        D=total_AUX/tbasea
        E=total_Light/tbasea
        F=A+C ! gas load
        G=D+E+B ! electricity load
        write(98,*)total_Heat_S/tbasea,',',total_Cool_S/tbasea,',',
     &  total_DHW_S/tbasea,',',total_AUX/tbasea,',',
     &  total_Light/tbasea
        write(98,*)',,,,'
        WRITE(98,*)'Demand,,,,'
        WRITE(98,*)'Heat,Cool,DHW,,'
        DO 80 IM=1,12
          WRITE(98,*)Heat_Demand(IM)/tbasea,',',Cool_Demand(IM)/tbasea,
     &  ',',DHW_Demand(IM)/tbasea,',,'
 80     CONTINUE
        write(98,*)total_Heat_D/tbasea,',',total_Cool_D/tbasea,',',
     &  total_DHW_D/tbasea

C Write summary Latex input text
        IF(IMODE.EQ.1)THEN
          write(91,*)"\\multicolumn{8}{|c|}{Actual model (annual results
     &)} \\\\"
          write(91,*)"\\hline"
	  write(91,*)"Total & Heating & Cooling & DHW & Auxiliary & Ligh
     &ts & Gas & Electricity \\\\"
	  write(91,*)"\\hline"
	  write(91,'(f6.2,7(a,f6.2),a)')BER,
     &" & ",A," & ",B," & ",C," & ",D," & ",E," & ",F," & ",G," \\\\"
	  write(91,*)"\\hline"
	  write(91,*)"\\multicolumn{8}{|c|}{ }\\\\"
	  write(91,*)"\\hline"
	ELSEIF(IMODE.EQ.2)THEN
	  write(91,*)""
          write(91,*)"\\hline"
          write(91,*)"CO2 $kg/m^{2}$ & \\multicolumn{7}{|c|}{Energy $kWh
     &/m^{2}$} \\\\"
          write(91,*)"\\hline"
	  write(91,*)"\\multicolumn{8}{|c|}{Notional model (annual resul
     &ts)} \\\\"
	  write(91,*)"\\hline"
	  write(91,*)"Total & Heating & Cooling & DHW & Auxiliary & Ligh
     &ts & Gas & Electricity \\\\"
	  write(91,*)"\\hline"
	  write(91,'(f6.2,7(a,f6.2),a)')AER,
     &" & ",A," & ",B," & ",C," & ",D," & ",E," & ",F," & ",G," \\\\"
	  write(91,*)"\\hline"
	  write(91,*)"\\multicolumn{8}{|c|}{ }\\\\"
	  write(91,*)"\\hline"
	  write(91,*)""
	ELSEIF(IMODE.EQ.3)THEN
	  write(91,*)"\\multicolumn{8}{|c|}{Reference model (annual resu
     &lts)} \\\\"
	  write(91,*)"\\hline"
	  write(91,*)"Total & Heating & Cooling & DHW & Auxiliary & Ligh
     &ts & Gas & Electricity \\\\"
	  write(91,*)"\\hline"
	  write(91,'(f6.2,7(a,f6.2),a)')RER,
     &" & ",A," & ",B," & ",C," & ",D," & ",E," & ",F," & ",G," \\\\"
	  write(91,*)"\\hline"
        ENDIF

C Write detailed Latex input file
        IF(IMODE.EQ.1)THEN
	  write(92,*)"\\hline"
	  write(92,*)"\\multicolumn{8}{|c|}{Actual model Energy $kWh/m^{
     &2}$} \\\\"
	  write(92,*)"\\hline"
	  write(92,*)"Month & Heating & Cooling & DHW & Auxiliary & Ligh
     &ts & Gas & Electricity \\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Jan & ",T(1)," & ",U(1)," & ",V(1)," & ",W(1),
     &" & ",XX(1)," & ",YY(1)," & ",ZZ(1),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Feb & ",T(2)," & ",U(2)," & ",V(2)," & ",W(2),
     &" & ",XX(2)," & ",YY(2)," & ",ZZ(2),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Mar & ",T(3)," & ",U(3)," & ",V(3)," & ",W(3),
     &" & ",XX(3)," & ",YY(3)," & ",ZZ(3),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Apr & ",T(4)," & ",U(4)," & ",V(4)," & ",W(4),
     &" & ",XX(4)," & ",YY(4)," & ",ZZ(4),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"May & ",T(5)," & ",U(5)," & ",V(5)," & ",W(5),
     &" & ",XX(5)," & ",YY(5)," & ",ZZ(5),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Jun & ",T(6)," & ",U(6)," & ",V(6)," & ",W(6),
     &" & ",XX(6)," & ",YY(6)," & ",ZZ(6),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Jul & ",T(7)," & ",U(7)," & ",V(7)," & ",W(7),
     &" & ",XX(7)," & ",YY(7)," & ",ZZ(7),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Aug & ",T(8)," & ",U(8)," & ",V(8)," & ",W(8),
     &" & ",XX(8)," & ",YY(8)," & ",ZZ(8),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Sep & ",T(9)," & ",U(9)," & ",V(9)," & ",W(9),
     &" & ",XX(9)," & ",YY(9)," & ",ZZ(9),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Oct & ",T(10)," & ",U(10)," & ",V(10)," & ",W(10),
     &" & ",XX(10)," & ",YY(10)," & ",ZZ(10),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Nov & ",T(11)," & ",U(11)," & ",V(11)," & ",W(11),
     &" & ",XX(11)," & ",YY(11)," & ",ZZ(11),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Dec & ",T(12)," & ",U(12)," & ",V(12)," & ",W(12),
     &" & ",XX(12)," & ",YY(12)," & ",ZZ(12),"\\\\"
	  write(92,*)"\\hline"
	ELSEIF(IMODE.EQ.2)THEN
	  write(92,*)"\\multicolumn{8}{|c|}{Notional model Energy $kWh/m
     &^{2}$} \\\\"
	  write(92,*)"\\hline"
	  write(92,*)"Total & Heating & Cooling & DHW & Auxiliary & Ligh
     &ts & Gas & Electricity \\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Jan & ",T(1)," & ",U(1)," & ",V(1)," & ",W(1),
     &" & ",XX(1)," & ",YY(1)," & ",ZZ(1),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Feb & ",T(2)," & ",U(2)," & ",V(2)," & ",W(2),
     &" & ",XX(2)," & ",YY(2)," & ",ZZ(2),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Mar & ",T(3)," & ",U(3)," & ",V(3)," & ",W(3),
     &" & ",XX(3)," & ",YY(3)," & ",ZZ(3),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Apr & ",T(4)," & ",U(4)," & ",V(4)," & ",W(4),
     &" & ",XX(4)," & ",YY(4)," & ",ZZ(4),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"May & ",T(5)," & ",U(5)," & ",V(5)," & ",W(5),
     &" & ",XX(5)," & ",YY(5)," & ",ZZ(5),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Jun & ",T(6)," & ",U(6)," & ",V(6)," & ",W(6),
     &" & ",XX(6)," & ",YY(6)," & ",ZZ(6),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Jul & ",T(7)," & ",U(7)," & ",V(7)," & ",W(7),
     &" & ",XX(7)," & ",YY(7)," & ",ZZ(7),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Aug & ",T(8)," & ",U(8)," & ",V(8)," & ",W(8),
     &" & ",XX(8)," & ",YY(8)," & ",ZZ(8),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Sep & ",T(9)," & ",U(9)," & ",V(9)," & ",W(9),
     &" & ",XX(9)," & ",YY(9)," & ",ZZ(9),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Oct & ",T(10)," & ",U(10)," & ",V(10)," & ",W(10),
     &" & ",XX(10)," & ",YY(10)," & ",ZZ(10),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Nov & ",T(11)," & ",U(11)," & ",V(11)," & ",W(11),
     &" & ",XX(11)," & ",YY(11)," & ",ZZ(11),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Dec & ",T(12)," & ",U(12)," & ",V(12)," & ",W(12),
     &" & ",XX(12)," & ",YY(12)," & ",ZZ(12),"\\\\"
	  write(92,*)"\\hline"
	ELSEIF(IMODE.EQ.3)THEN
	  write(92,*)"\\multicolumn{8}{|c|}{Reference model Energy $kWh/
     &m^{2}$} \\\\"
	  write(92,*)"\\hline"
	  write(92,*)"Total & Heating & Cooling & DHW & Auxiliary & Ligh
     &ts & Gas & Electricity \\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Jan & ",T(1)," & ",U(1)," & ",V(1)," & ",W(1),
     &" & ",XX(1)," & ",YY(1)," & ",ZZ(1),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Feb & ",T(2)," & ",U(2)," & ",V(2)," & ",W(2),
     &" & ",XX(2)," & ",YY(2)," & ",ZZ(2),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Mar & ",T(3)," & ",U(3)," & ",V(3)," & ",W(3),
     &" & ",XX(3)," & ",YY(3)," & ",ZZ(3),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Apr & ",T(4)," & ",U(4)," & ",V(4)," & ",W(4),
     &" & ",XX(4)," & ",YY(4)," & ",ZZ(4),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"May & ",T(5)," & ",U(5)," & ",V(5)," & ",W(5),
     &" & ",XX(5)," & ",YY(5)," & ",ZZ(5),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Jun & ",T(6)," & ",U(6)," & ",V(6)," & ",W(6),
     &" & ",XX(6)," & ",YY(6)," & ",ZZ(6),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Jul & ",T(7)," & ",U(7)," & ",V(7)," & ",W(7),
     &" & ",XX(7)," & ",YY(7)," & ",ZZ(7),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Aug & ",T(8)," & ",U(8)," & ",V(8)," & ",W(8),
     &" & ",XX(8)," & ",YY(8)," & ",ZZ(8),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Sep & ",T(9)," & ",U(9)," & ",V(9)," & ",W(9),
     &" & ",XX(9)," & ",YY(9)," & ",ZZ(9),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Oct & ",T(10)," & ",U(10)," & ",V(10)," & ",W(10),
     &" & ",XX(10)," & ",YY(10)," & ",ZZ(10),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Nov & ",T(11)," & ",U(11)," & ",V(11)," & ",W(11),
     &" & ",XX(11)," & ",YY(11)," & ",ZZ(11),"\\\\"
	  write(92,*)"\\hline"
	  write(92,'(7(a,f6.2),a)')
     &"Dec & ",T(12)," & ",U(12)," & ",V(12)," & ",W(12),
     &" & ",XX(12)," & ",YY(12)," & ",ZZ(12),"\\\\"
	  write(92,*)"\\hline"
	  write(92,*)""
        ENDIF
      ENDIF
      RETURN
      END

************************** BERTER ********************************
c Subroutine BERTER generates
C BER = stripped building CO2 emissions rate
C TER = target CO2 emissions rate calculated by applying improvement
C       factors onto notional building CO2 emissions rate
C RER = reference building CO2 emissions rate
C SER = standard CO2 emissions rate calculated by applying fixed
C       improvement factor of 0.235 onto RER (SER=0.765*RER)
C TyER= typical building CO2 emissions rate
C in order to provide information about UK NCM code (non)compliance.
C Relevant sections of legislation are,
C Building Code Section 6 for Scotland,
C Part L for England and Wales
C Part F for Northern Ireland.

      SUBROUTINE BERTER(IER)
      IMPLICIT NONE
#include "sbem.h"
#include "building.h"
#include "geometry.h"

      common/C1/NCOMP,NCON
      INTEGER NCOMP,NCON

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

      INTEGER ICOMP,IER,INDX,IFT,IREN,IORIENT,INCLIN,ICLMT,ICMSYS,IHF
      integer ihlzp  ! pointer from zone to sytem
      INTEGER ITDH   ! pointer set to allow solar thermal contributions
                     ! to be made to HVAC system supplying DHW
      CHARACTER OUTS*124
      LOGICAL NEAR,CLOSER,CLOSE
      DIMENSION FHM(MFT) ! Array holding heating supplied by fuel type
      REAL FHM           ! Main Heating Fuel type is held in MHF
      DIMENSION ZCO2N(MCOM) ! Notional zonal CO2 emissions
      REAL ZCO2N,XIF,TBASEA,rer_dhwco2

C Solar insolation interpolation variables
      REAL C_ORIENTATION,C_INCLINATION,A,B,C,D,AC,BD,P,
     &Q,X1,X2,XX,ANG_O,ANG_I

C Renewable energy sources variables
      REAL Q_ses   ! kWh from solar thermal
      REAL TLOAD   ! total DHW load supplied from solar thermal
      REAL Q_pv    ! kWh from solar PV
      REAL PVDCO2  ! CO2 displaced by PV
      REAL KR      ! wind turbine terrain factor
      REAL Z0      ! wind turbine surface roughness factor
      REAL K_wt    ! wind turbine efficiency
      REAL Q_wt    ! kWh from wind turbine
      REAL WTDCO2  ! CO2 displaced by wind turbine
      REAL THEAT   ! Total space heating demand for building
      REAL TDHW    ! Total DHW heating demand
      REAL FR      ! Fuel requirements of CHP system
      REAL CHPGCO2 ! CO2 generated by CHP system
      REAL CHPDCO2 ! CO2 displaced by CHP system
      REAL E       ! kWh electricity generated by CHP unit
      REAL Ren_energy ! Total renewable energy available in system

C RER_* is energy (after including efficiency of HVAC systems) and
C CO2 emissions associated with reference building.
C       REAL RER_HEAT,RER_COOL,RER_AUX,RER_HEATCO2,RER_COOLCO2
C       REAL RER_AUXCO2

C Initialise
      AER=0. ; AERE=0. ; RER=0.
      BER=0. ; BERE=0. ; TyER=0.
      BER_HEAT=0.0 ; BER_HEATCO2=0.0 ; BER_COOL=0.0 ; BER_COOLCO2=0.0
      BER_AUX=0.0 ; BER_AUXCO2=0.0 ; BER_LIGHT=0.0 ; BER_LIGHTCO2=0.0
      BER_DHW=0.0 ; BER_DHWCO2=0.0
      AER_HEAT=0.0 ; AER_HEATCO2=0.0 ; AER_COOL=0.0 ; AER_COOLCO2=0.0
      AER_AUX=0.0 ; AER_AUXCO2=0.0 ; AER_LIGHT=0.0 ; AER_LIGHTCO2=0.0
      AER_DHW=0.0 ; AER_DHWCO2=0.0
      RER_HEAT=0.0 ; RER_COOL=0.0 ; RER_AUX=0.0 ; RER_DHW=0.0
      RER_HEATCO2=0.0 ; RER_COOLCO2=0.0 ; RER_DHWCO2=0.0 ;
      RER_AUXCO2=0.0
      TyER_HEAT=0.0; TyER_HEATCO2=0.0; TyER_COOL=0.0 ; TyER_COOLCO2=0.0
      TyER_AUX=0.0; TyER_AUXCO2=0.0; TyER_LIGHT=0.0 ; TyER_LIGHTCO2=0.0
      TyER_DHW=0.0 ; TyER_DHWCO2=0.0
      TBASEA=0.0
      CLOSER=.FALSE.
      ITDH=0
      Ren_energy=0.0
      DO 10 IFT=1,MFT
        FHM(IFT)=0.0 ; BERF(IFT)=0.0 ; AERF(IFT)=0.0
        RERF(IFT)=0.0 ; TyERF(IFT)=0.0
 10   CONTINUE
      DO 11 IFT=1,MNS
        BERSH(IFT)=0.0 ; BERSC(IFT)=0.0 ; BERSA(IFT)=0.0
        AERSH(IFT)=0.0 ; AERSC(IFT)=0.0 ; AERSA(IFT)=0.0
        RERSH(IFT)=0.0 ; RERSC(IFT)=0.0 ; RERSA(IFT)=0.0
        TYERSH(IFT)=0.0 ; TyERSC(IFT)=0.0 ; TyERSA(IFT)=0.0
 11   CONTINUE
      Q_ses=0.0 ; TLOAD=0.0 ; Q_pv=0.0 ; PVDCO2=0.0 ; KR=0.0 ; Z0=0.0
      K_wt=0.0 ; Q_wt=0.0 ; WTDCO2=0.0 ; THEAT=0.0 ; TDHW=0.0 ; FR=0.0
      CHPGCO2=0.0 ; E=0.0 ; CHPDCO2=0.0
      ICLMT=1 ! To be read from file when more climates have been added

C If there are renewables then calculate output
        DO 768 IREN=1,NREN

C If solar system then calculate insolation
          IF(NRENTYPE(IREN).EQ.1.OR.NRENTYPE(IREN).EQ.2)THEN
            ANG_O=RENDATA(IREN,3)
            ANG_I=RENDATA(IREN,4)

C calculate insolation at collector surface (kWh/m2)
            C_ORIENTATION=ANG_O/45.    ! Values held at 45deg intervals
            IORIENT=INT(C_ORIENTATION)
            C_INCLINATION=ANG_I/15.    ! Values held at 15deg intervals
            INCLIN=INT(C_INCLINATION)

C Interpolation needs to be done for four values i.e. two values for
C inclination and two for orientation
C Work out indices for values to look up
            A=SLRINSO(ICLMT,IORIENT*7+INCLIN)
            B=SLRINSO(ICLMT,(IORIENT+1)*7+INCLIN)
            C=SLRINSO(ICLMT,IORIENT*7+INCLIN+1)
            D=SLRINSO(ICLMT,(IORIENT+1)*7+INCLIN+1)

C Work out corresponding angles
            AC=REAL(IORIENT)*45.
            BD=REAL(IORIENT+1)*45.
            P=REAL(INCLIN)*15.
            Q=REAL(INCLIN+1)*15.

C Interpolate across orientation
            X1=(A*(BD-ANG_O)+B*(ANG_O-AC))/(BD-AC)
            X2=(C*(BD-ANG_O)+D*(ANG_O-AC))/(BD-AC)

C Interpolate across inclination
            XX=(X1*(Q-ANG_I)+X2*(ANG_I-P))/(Q-P)
          ENDIF
          IF(NRENTYPE(IREN).EQ.1)THEN

C Solar thermal
C Calculate energy according to SBEM technical manual
            Q_ses=XX*0.38*RENDATA(IREN,2)
            INDX=nint(RENDATA(IREN,1))
            ITDH=INDX

C If DHW system not specified add solar gains to first HVAC system
            IF(ITDH.LE.0)ITDH=-1

C Calculate total DHW load on this system
            TLOAD=0.0
            DO 99 ICOMP=1,NCOMP
              IF(IDHWLZ(ICOMP).EQ.INDX)THEN
                TLOAD=BERDHW(ICOMP)+TLOAD
              ENDIF
 99         CONTINUE
            IF(TLOAD.LT.Q_ses)TLOAD=Q_ses

C Reduce zone demand by fraction of load supplied by thermal collector
            DO 98 ICOMP=1,NCOMP
              IF(IDHWLZ(ICOMP).EQ.INDX)THEN
                CALL ECLOSE(Q_ses,0.0,0.01,near)
                IF(.NOT.NEAR)
     &          BERDHW(ICOMP)=BERDHW(ICOMP)*(TLOAD-Q_ses)/TLOAD
              ENDIF
 98         CONTINUE
          ELSEIF(NRENTYPE(IREN).EQ.2)THEN

C Solar PV
C Calculate annual kWh produced by PV (system efficiency is 75%) and CO2
C displaced by it
            Q_pv=Q_pv+XX*RENDATA(IREN,1)*0.75*RENDATA(IREN,2) ! kWh
            PVDCO2=Q_pv*0.568 ! kgCO2
          ELSEIF(NRENTYPE(IREN).EQ.3)THEN

C Wind Turbine
C Determine terrain factor KR and roughness length Z0
            IF(nint(RENDATA(IREN,1)).EQ.1)THEN
              KR=0.17
              Z0=0.01
            ELSEIF(nint(RENDATA(IREN,1)).EQ.2)THEN
              KR=0.19
              Z0=0.05
            ELSEIF(nint(RENDATA(IREN,1)).EQ.3)THEN
              KR=0.22
              Z0=0.30
            ELSEIF(nint(RENDATA(IREN,1)).EQ.4)THEN
              KR=0.24
              Z0=1.00
            ENDIF

C Determine wind turbine efficiency K_wt
            IF(AAWS(ICLMT).LT.3.0)THEN
              K_WT=0.0
              IF(RENDATA(IREN,4).GT.80.)K_wt=0.0
            ELSEIF(AAWS(ICLMT).LT.4.0)THEN
              K_WT=0.20
              IF(RENDATA(IREN,4).GT.80.)K_wt=0.36
            ELSEIF(AAWS(ICLMT).LT.5.0)THEN
              K_WT=0.20
              IF(RENDATA(IREN,4).GT.80.)K_wt=0.35
            ELSEIF(AAWS(ICLMT).LT.6.0)THEN
              K_WT=0.19
              IF(RENDATA(IREN,4).GT.80.)K_wt=0.33
            ELSEIF(AAWS(ICLMT).LT.7.0)THEN
              K_WT=0.16
              IF(RENDATA(IREN,4).GT.80.)K_wt=0.29
            ELSEIF(AAWS(ICLMT).LT.8.0)THEN
              K_WT=0.15
              IF(RENDATA(IREN,4).GT.80.)K_wt=0.26
            ELSEIF(AAWS(ICLMT).LT.9.0)THEN
              K_WT=0.14
              IF(RENDATA(IREN,4).GT.80.)K_wt=0.23
            ELSE
              K_WT=0.14
              IF(RENDATA(IREN,4).GT.80.)K_wt=0.23
            ENDIF

C Work out Q_wt (Given in SBEM technical manual of 10 Dec 2007 section
C 4.10.2) and CO2 displaced by it
            Q_wt=Q_wt+0.5*1.225*KR*log(RENDATA(IREN,3)/Z0)*
     &      (3.14159*(RENDATA(IREN,2)**2.)/4.)*SAWS(ICLMT)*K_wt/1000.
c     &      (3.14159*(RENDATA(IREN,2)**2.)/4.)*(SAWS(ICLMT)/8760.)*K_wt
            WTDCO2=Q_wt*0.568 ! kgCO2
          ELSEIF(NRENTYPE(IREN).EQ.4)THEN ! CHP

C Combined Heat and Power
C Calculate annual space heating and DHW demand
            THEAT=0.0
            TDHW=0.0
            DO 97 ICOMP=1,NCOMP
              THEAT=BERHEAT(ICOMP)+THEAT
              TDHW=BERDHW(ICOMP)+TDHW

C Reduce building space heating and DHW demand
              BERHEAT(ICOMP)=(100.-RENDATA(IREN,2))*BERHEAT(ICOMP)
              BERDHW(ICOMP)=(100.-RENDATA(IREN,3))*BERDHW(ICOMP)
 97         CONTINUE

C Calculate annual fuel requirement
            FR=(THEAT*RENDATA(IREN,2)+TDHW*RENDATA(IREN,3))/
     &         (100.0*RENDATA(IREN,1))

C Calculate CO2 emission from consuming this fuel
            ift=nint(RENDATA(IREN,5))
            CHPGCO2=FR*FUELCO2(ift)

C Calculate electricity generated by CHP unit
C << Check for divided by zero exception
            E=E+FR*RENDATA(IREN,1)/RENDATA(IREN,4)

C Calculate CO2 displaced by this electricity
            CHPDCO2=E*0.568
          ENDIF
 768    CONTINUE

C Now calculate total energy (generated energy demand i.e. taking in
C account heat or coolth generator efficiency) and
C carbon dioxide emissions
C Work out zone wise energy and CO2 emissions for stripped model
      DO 100 ICOMP=1,NCOMP
        IHLZP=IHLZ(ICOMP)  ! set up pointer

C Integrate zonal base area
        TBASEA=TBASEA+ZBASEA(ICOMP)

C Heating energy for stripped model. Calculate energy generated
        IF(IHLZP.NE.0)THEN ! Only if there is plant linked to this zone
        call eclose(HGEF(IHLZP),0.0,0.001,near)
        if(near)then
          BERHEAT_afterefficiency(ICOMP)=0.
        else  
          if(iDsmTestingFlag.eq.2)then
            BERHEAT_afterefficiency(ICOMP)=BERHEAT(ICOMP)/0.73
          else
            BERHEAT_afterefficiency(ICOMP)=BERHEAT(ICOMP)/HGEF(IHLZP)
          endif
        endif

C Integrating for total energy
        BER_HEAT=BER_HEAT+BERHEAT_afterefficiency(ICOMP)

C Integrating for total CO2 emissions
        BER_HEATCO2=BER_HEATCO2+
     &  BERHEAT_afterefficiency(ICOMP)*FUELCO2(IFTYP(IHLZP))

C Integrating for fuel type
        BERF(IFTYP(IHLZP))=
     &    BERF(IFTYP(IHLZP))+BERHEAT_afterefficiency(ICOMP)

C Integrating energy generated for each HVAC type
        BERSH(IHLZP)=BERSH(IHLZP)+BERHEAT_afterefficiency(ICOMP)

C Hold heating energy based on fuel type (to work out main heating fuel)
        FHM(IFTYP(IHLZP))=
     &    FHM(IFTYP(IHLZP))+BERHEAT_afterefficiency(ICOMP)

C Cooling energy is always supplied by electricity type (10).
C Only if a coolth generator is associated with this zone
        BERCOOL(ICOMP)=ABS(BERCOOL(ICOMP))
        NEAR=.FALSE.
        CALL ECLOSE(CGEF(IHLZP),0.0,0.001,NEAR)
        IF(.NOT.NEAR)THEN
          BERCOOL_afterefficiency(ICOMP)=BERCOOL(ICOMP)/CGEF(IHLZP)
        ELSE ! no cooling generator hence no cooling load 
          BERCOOL_afterefficiency(ICOMP)=0.0
        ENDIF
        if(iDsmTestingFlag.eq.2)THEN
        BERCOOL_afterefficiency(ICOMP)=BERCOOL(ICOMP)/2.25
        endif

C Integrating for total energy
        BER_COOL=BER_COOL+BERCOOL_afterefficiency(ICOMP)

C Integrating for total CO2 emissions
        BER_COOLCO2=BER_COOLCO2+
     &    BERCOOL_afterefficiency(ICOMP)*FUELCO2(10)

C Integrating for electricity fuel type.
        BERF(10)=BERF(10)+BERCOOL_afterefficiency(ICOMP)

C Hold cooling energy based on fuel type (to work out main heating fuel)
        BERSC(IHLZP)=BERSC(IHLZP)+BERCOOL_afterefficiency(ICOMP)

C DHW energy ...
C (depending upon whether there is a stand alone DHW
C generator for this zone or DHW is additionally generated from the
C space heating HVAC system. Each zone has to have a DHW source, if the
C user had not defined it warn the user and assume DHW is supplied from
C HVAC system.
        IF(IDHWLZ(ICOMP).EQ.0)THEN
          WRITE(OUTS,'(2A)')' No DHW system defined for zone ',
     &    ZNAME(ICOMP)
          CALL EDISP(IUOUT,OUTS)
          OUTS=' '
          write(outs,'(2A)')' Assuming DHW supplied from ',
     &    HVACNAME(IHLZP)
          CALL EDISP(IUOUT,OUTS)
          IDHWLZ(ICOMP)=-1*IHLZP
        ENDIF
        IF(IDHWLZ(ICOMP).GT.0)THEN

C We have a dedicated DHW system.
          BERDHW_afterefficiency(ICOMP)=
     &      BERDHW(ICOMP)/HWEF(IDHWLZ(ICOMP))
          DHWCO2RATE=FUELCO2(IDHFL(IDHWLZ(ICOMP)))
          BERF(IDHFL(IDHWLZ(ICOMP)))=BERF(IDHFL(IDHWLZ(ICOMP)))+
     &      BERDHW_afterefficiency(ICOMP)
        ELSEIF(IDHWLZ(ICOMP).LT.0)THEN

C We are taking DHW from the heating system.
          INDX=-1*IDHWLZ(ICOMP) ! INDX is actually set equal to IHLZP
          CALL ECLOSE(HGEF(INDX),0.0,0.001,CLOSER)
          IF(.NOT.CLOSER)THEN
            if(iDsmTestingFlag.eq.2)then
              BERDHW_afterefficiency(ICOMP)=BERDHW(ICOMP)/0.45
            else
              BERDHW_afterefficiency(ICOMP)=BERDHW(ICOMP)/HGEF(INDX)
            endif
            DHWCO2RATE=FUELCO2(IFTYP(INDX))
            BERF(IFTYP(IHLZP))=
     &        BERF(IFTYP(IHLZP))+BERDHW_afterefficiency(ICOMP)
          ELSE
            BERDHW_afterefficiency(ICOMP)=0.0
          ENDIF
        ENDIF
        BER_DHW=BER_DHW+BERDHW_afterefficiency(ICOMP)
        BER_DHWCO2=BER_DHWCO2+BERDHW_afterefficiency(ICOMP)*DHWCO2RATE

C For both lighting and auxiliary energy assume these are supplied by
C the electrical grid (Fuel type 10 from SBEM.db1)
C Auxiliary energy.
        BER_AUX=BER_AUX+BERAUX(ICOMP)
        BER_AUXCO2=BER_AUXCO2+BERAUX(ICOMP)*FUELCO2(10)
        BERF(10)=BERF(10)+BERAUX(ICOMP)
        BERSA(IHLZP)=BERSA(IHLZP)+BERAUX(ICOMP)
        ENDIF

C Lighting energy
        BER_LIGHT=BER_LIGHT+BERLIGHT(ICOMP)
        BER_LIGHTCO2=BER_LIGHTCO2+BERLIGHT(ICOMP)*FUELCO2(10)
        BERF(10)=BERF(10)+BERLIGHT(ICOMP)
 100  CONTINUE

C Calculation of emissions and total energy for stripped model
      BERE=BER_HEAT+BER_COOL+BER_DHW+BER_LIGHT+BER_AUX-
     &     Q_pv-Q_wt-E

C Work out total energy supplied by renewables.
      Ren_energy=Q_pv+Q_wt+E
      IF(ITDH.EQ.-1)Ren_energy=Ren_energy+Q_ses

      BER=BER_HEATCO2+BER_COOLCO2+BER_DHWCO2+BER_LIGHTCO2+BER_AUXCO2-
     &    PVDCO2-WTDCO2-CHPDCO2+CHPGCO2

C Normalise by floor area
      BERE=BERE/TBASEA
      BER=BER/TBASEA
      DO 1000 ICMSYS=1,NCMSYS
        BERSH(ICMSYS)=BERSH(ICMSYS)/TBASEA
        BERSC(ICMSYS)=BERSC(ICMSYS)/TBASEA
        BERSA(ICMSYS)=BERSA(ICMSYS)/TBASEA
 1000 CONTINUE

C Assuming that there are MFT fuel types.
      DO 1001 IHF=1,MFT
        call eclose(BERF(IHF),0.00,0.01,close)
	if(close)then
          continue
        else
          BERF(IHF)=BERF(IHF)/TBASEA
        endif
 1001 CONTINUE
      BER_HEAT=BER_HEAT/TBASEA
      BER_COOL=BER_COOL/TBASEA
      BER_AUX=BER_AUX/TBASEA
      BER_LIGHT=BER_LIGHT/TBASEA
      BER_DHW=BER_DHW/TBASEA

C First work out fuel type for notional model based on main fuel type in
C the stripped model
      MHF=1
      DO 201 IFT=2,MFT
        IF(FHM(IFT).GT.FHM(MHF))MHF=IFT
 201  CONTINUE

C Now work out notional heating fuel NHF based on paragraph 38 NCM
C Modelling Guide ver4d of March 2008
      IF(MHF.EQ.1.OR.MHF.EQ.3.OR.MHF.EQ.9.OR.MHF.EQ.11.OR.MHF.EQ.12)THEN
        NHF=1 ! Natural gas
      ELSE
        NHF=4 ! Oil
      ENDIF

C Calculate all energy and CO2 emissions information for notional model
      DO 200 ICOMP=1,NCOMP

C Work out efficiency of heating, cooling and DHW systems associated
C with each zone paragraphs 44 and table 6 NCM Modelling Guide
        DHWNGE=0.45
        IHLZP=IHLZ(ICOMP)  ! set up pointer
        IF(IHLZP.NE.0)THEN ! Only if there is plant linked to this zone

C Heated and Mechanically ventilated
        IF(INCMSYS(IHLZP).EQ.4)THEN
          CoolSSEER_N(IHLZP)=0.
          HeatSCoP_N(IHLZP)=0.78

C Heated and naturally ventilated (or heated only)
        ELSEIF(INCMSYS(IHLZP).LE.11)THEN
          CoolSSEER_N(IHLZP)=0.
          HeatSCoP_N(IHLZP)=0.73

C Air conditioned
        ELSE
          CoolSSEER_N(IHLZP)=1.67
          HeatSCoP_N(IHLZP)=0.83
        ENDIF

C Heating energy
          AERHEAT_afterefficiency(ICOMP)=
     &    AERHEAT(ICOMP)/HeatSCoP_N(IHLZP)
        AER_HEAT=AER_HEAT+AERHEAT_afterefficiency(ICOMP)
        AER_HEATCO2=AER_HEATCO2+
     &  AERHEAT_afterefficiency(ICOMP)*FUELCO2(NHF)
        AERF(NHF)=AERF(NHF)+AERHEAT_afterefficiency(ICOMP)
        AERSH(IHLZP)=AERSH(IHLZP)+AERHEAT_afterefficiency(ICOMP)

C Cooling energy only if a coolth generator is associated with this zone
C Cooling energy is always supplied by electricity type (10).
        AERCOOL(ICOMP)=ABS(AERCOOL(ICOMP))
        NEAR=.FALSE.
        CALL ECLOSE(CoolSSEER_N(IHLZP),0.0,0.001,NEAR)
        IF(.NOT.NEAR)THEN
          AERCOOL_afterefficiency(ICOMP)=
     &      AERCOOL(ICOMP)/CoolSSEER_N(IHLZP)
        ELSE
          AERCOOL_afterefficiency(ICOMP)=0.0
        ENDIF
        AER_COOL=AER_COOL+AERCOOL_afterefficiency(ICOMP)
        AER_COOLCO2=AER_COOLCO2+
     &    AERCOOL_afterefficiency(ICOMP)*FUELCO2(10)  ! electricity
        AERF(10)=AERF(10)+AERCOOL_afterefficiency(ICOMP)
        AERSC(IHLZP)=AERSC(IHLZP)+AERCOOL_afterefficiency(ICOMP)

C DHW energy (depending upon whether there is a stand alone DHW
C generator for this zone or DHW is additionally generated from the
C space heating HVAC system. Each zone has to have a DHW source, if the
C user had not defined it warn the user and assume DHW is supplied from
C HVAC system
        IF(IDHWLZ(ICOMP).EQ.0)THEN
          WRITE(OUTS,'(2A)')' No DHW system defined for zone ',
     &      ZNAME(ICOMP)
          CALL EDISP(IUOUT,OUTS)
          write(outs,'(2A)')' Assuming DHW supplied from ',
     &      HVACNAME(IHLZP)
          CALL EDISP(IUOUT,OUTS)
        ENDIF
        IF(IDHWLZ(ICOMP).GT.0)THEN
          AERDHW_afterefficiency(ICOMP)=AERDHW(ICOMP)/DHWNGE
          DHWCO2RATE=FUELCO2(NHF)
          AERF(NHF)=AERF(NHF)+AERDHW_afterefficiency(ICOMP)
        ELSEIF(IDHWLZ(ICOMP).LE.0)THEN
            AERDHW_afterefficiency(ICOMP)=
     &      AERDHW(ICOMP)/HeatSCoP_N(IHLZP)
          DHWCO2RATE=FUELCO2(NHF)
          AERF(NHF)=AERF(NHF)+AERDHW_afterefficiency(ICOMP)
        ENDIF
        AER_DHW=AER_DHW+AERDHW_afterefficiency(ICOMP)
        AER_DHWCO2=AER_DHWCO2+AERDHW_afterefficiency(ICOMP)*DHWCO2RATE

C For both lighting and auxiliary energy assume these are supplied by
C the electrical grid (Fuel type 10 from SBEM.db1)
C Auxiliary energy
        AER_AUX=AER_AUX+AERAUX(ICOMP)
        AER_AUXCO2=AER_AUXCO2+AERAUX(ICOMP)*FUELCO2(10)
        AERF(10)=AERF(10)+AERAUX(ICOMP)
        AERSA(IHLZP)=AERSA(IHLZP)+AERAUX(ICOMP)
        ENDIF

C Lighting energy
        AER_LIGHT=AER_LIGHT+AERLIGHT(ICOMP)
        AER_LIGHTCO2=AER_LIGHTCO2+AERLIGHT(ICOMP)*FUELCO2(10)
        AERF(10)=AERF(10)+AERLIGHT(ICOMP)

C Get zonal CO2 emissions for calculation of Improvement Factor
        ZCO2N(ICOMP)=AERHEAT_afterefficiency(ICOMP)*FUELCO2(NHF)+
     &  AERCOOL_afterefficiency(ICOMP)*FUELCO2(10)+
     &  AERDHW_afterefficiency(ICOMP)*DHWCO2RATE+
     &  AERLIGHT(ICOMP)*FUELCO2(10)+AERAUX(ICOMP)*FUELCO2(10)
 200  CONTINUE

C Calculation of emissions and AER (total energy for notional model)
      AERE=AER_HEAT+AER_COOL+AER_DHW+AER_LIGHT+AER_AUX
      AER=AER_HEATCO2+AER_COOLCO2+AER_DHWCO2+AER_LIGHTCO2+AER_AUXCO2

C Normalise by floor area
      AERE=AERE/TBASEA
      AER=AER/TBASEA
      DO 2000 ICMSYS=1,NCMSYS
        AERSH(ICMSYS)=AERSH(ICMSYS)/TBASEA
        AERSC(ICMSYS)=AERSC(ICMSYS)/TBASEA
        AERSA(ICMSYS)=AERSA(ICMSYS)/TBASEA
 2000 CONTINUE

C This should loop thru all of the MFT fuels.
      DO 2001 IHF=1,MFT
        AERF(IHF)=AERF(IHF)/TBASEA
 2001 CONTINUE
      AER_HEAT=AER_HEAT/TBASEA
      AER_COOL=AER_COOL/TBASEA
      AER_AUX=AER_AUX/TBASEA
      AER_LIGHT=AER_LIGHT/TBASEA
      AER_DHW=AER_DHW/TBASEA

C Calculation of improvement factor IF
      FXIF=0.
      XIF=0.
      DO 202 ICOMP=1,NCOMP

C Heated and Mechanically ventilated
        IF(INCMSYS(IHLZ(ICOMP)).EQ.4)THEN
          XIF=0.20

C Heated and naturally ventilated (or heated only)
        ELSEIF(INCMSYS(IHLZ(ICOMP)).LE.11)THEN
          XIF=0.15

C Air conditioned
        ELSE
          XIF=0.20
        ENDIF
        FXIF=FXIF+XIF*ZCO2N(ICOMP)/(AER*TBASEA)
 202  CONTINUE

C Calculate TER based on calc on Part L 2A/Section 6 page 5.
      XLZC=0.1
      TER=ABS(AER*(1.-FXIF)*(1.-XLZC))

C Generate reference model results
      RER=0.

C Calculate all energy and CO2 emissions information for reference model
      DO 300 ICOMP=1,NCOMP
        IHLZP=IHLZ(ICOMP)  ! set up pointer

C Work out efficiency of heating, cooling and DHW systems associated
C with each zone paragraphs 44 and table 6 NCM Modelling Guide
C Heating energy
        IF(IHLZP.NE.0)THEN ! Only if there is plant linked to this zone
        HeatSCoP_R(IHLZP)=0.73     ! Fix heating efficiency to 0.73 
        CoolSSEER_R(IHLZP)=2.25    ! Fix cooling efficiency to 2.25

          RERHEAT_afterefficiency(ICOMP)=
     &    RERHEAT(ICOMP)/HeatSCoP_R(IHLZP)
        RER_HEAT=RER_HEAT+RERHEAT_afterefficiency(ICOMP)
        RER_HEATCO2=RER_HEATCO2+
     &  RERHEAT_afterefficiency(ICOMP)*FUELCO2(1)

C Heating and hot water are always Natural Gas for reference building (NHF=1)
        RERF(1)=RERF(1)+RERHEAT_afterefficiency(ICOMP)
        RERSH(IHLZP)=RERSH(IHLZP)+RERHEAT_afterefficiency(ICOMP)

C DHW for reference building - need to use 0.73 efficiency if there is no
C stand-alone generator
        IF(IDHWLZ(ICOMP).EQ.0)THEN
          WRITE(OUTS,'(2A)')' No DHW system defined for zone ',
     &      ZNAME(ICOMP)
          CALL EDISP(IUOUT,OUTS)
          write(outs,'(2A)')' Assuming DHW supplied from ',
     &      HVACNAME(IHLZP)
          CALL EDISP(IUOUT,OUTS)
        ENDIF
        IF(IDHWLZ(ICOMP).GT.0)THEN
          RERDHW_afterefficiency(ICOMP)=RERDHW(ICOMP)/DHWNGE
          DHWCO2RATE=FUELCO2(1)

C Heating and hot water are always supplied by Natural Gas for reference building (NHF=1)
          RERF(1)=RERF(1)+RERDHW_afterefficiency(ICOMP)
        ELSEIF(IDHWLZ(ICOMP).LE.0)THEN
            RERDHW_afterefficiency(ICOMP)=
     &      RERDHW(ICOMP)/HeatSCoP_R(IHLZP)
          DHWCO2RATE=FUELCO2(1)

C Heating and hot water are always supplied by Natural Gas for reference building (NHF=1)
          RERF(1)=RERF(1)+RERDHW_afterefficiency(ICOMP)
        ENDIF
        RER_DHW=RER_DHW+RERDHW_afterefficiency(ICOMP)
        RER_DHWCO2=RER_DHWCO2+RERDHW_afterefficiency(ICOMP)*DHWCO2RATE

C Cooling energy is always supplied by electricity type (10).
        RERCOOL(ICOMP)=ABS(RERCOOL(ICOMP))
          RERCOOL_afterefficiency(ICOMP)=
     &    RERCOOL(ICOMP)/CoolSSEER_R(IHLZP)
        RER_COOL=RER_COOL+RERCOOL_afterefficiency(ICOMP)
        RER_COOLCO2=RER_COOLCO2+
     &    RERCOOL_afterefficiency(ICOMP)*FUELCO2(10)
        RERF(10)=RERF(10)+RERCOOL_afterefficiency(ICOMP)
        RERSC(IHLZP)=RERSC(IHLZP)+RERCOOL_afterefficiency(ICOMP)

C Auxiliary energy is always supplied by electricity type (10).
        RER_AUX=RER_AUX+RERAUX(ICOMP)
        RER_AUXCO2=RER_AUXCO2+RERAUX(ICOMP)*FUELCO2(10)
        RERF(10)=RERF(10)+RERAUX(ICOMP)
        RERSA(IHLZP)=RERSA(IHLZP)+RERAUX(ICOMP)
        ENDIF

C Add the fuel needed for lights - same as notional building (electricity)
C This is why AERLIGHT(ICOMP) is used here
        RERF(10)=RERF(10)+AERLIGHT(ICOMP)
 300  CONTINUE

C Calculation of emissions and total energy for reference model (use
C DHW and lighting energy and emissions of notional model and only
C modify heating, cooling and auxiliary energy)
C RERE is not yet used anywhere
C      RERE=RER_HEAT+RER_COOL+RER_DHW+AER_LIGHT+RER_AUX
      RER=RER_HEATCO2+RER_COOLCO2+RER_DHWCO2+AER_LIGHTCO2+RER_AUXCO2

C Normalise by floor area
C      RERE=RERE/TBASEA
      RER=RER/TBASEA
      DO 3000 ICMSYS=1,NCMSYS
        RERSH(ICMSYS)=RERSH(ICMSYS)/TBASEA
        RERSC(ICMSYS)=RERSC(ICMSYS)/TBASEA
        RERSA(ICMSYS)=RERSA(ICMSYS)/TBASEA
 3000 CONTINUE
      DO 3001 IHF=1,MFT
        RERF(IHF)=RERF(IHF)/TBASEA
 3001 CONTINUE
      RER_HEAT=RER_HEAT/TBASEA
      RER_COOL=RER_COOL/TBASEA
      RER_AUX=RER_AUX/TBASEA
      RER_DHW=RER_DHW/TBASEA
C      RER_LIGHT=RER_LIGHT/TBASEA

C Get standard Emissions SER (always 23.5% improvement factor on RER)
      SER=0.765*RER

C Get asset rating NAR
      NAR=NINT(BER/SER)*100

C Now generate typical building results
C Calculate all energy and CO2 emissions information for notional model
      TyER=0.0
      DO 400 ICOMP=1,NCOMP

C Work out efficiency of heating, cooling and DHW systems associated
C with each zone paragraphs 44 and table 6 NCM Modelling Guide
        DHWNGE=0.45
        IHLZP=IHLZ(ICOMP)  ! set up pointer
        IF(IHLZP.NE.0)THEN ! Only if there is plant linked to this zone

C Heated and Mechanically ventilated
        IF(INCMSYS(IHLZP).EQ.4)THEN
          CoolSSEER_T(IHLZP)=0.
          HeatSCoP_T(IHLZP)=0.55

C Heated and naturally ventilated (or heated only)
        ELSEIF(INCMSYS(IHLZP).LE.11)THEN
          CoolSSEER_T(IHLZP)=0.
          HeatSCoP_T(IHLZP)=0.55

C Air conditioned
        ELSE
          CoolSSEER_T(IHLZP)=1.17
          HeatSCoP_T(IHLZP)=0.55
        ENDIF

C Heating energy
        TyERHEAT_afterefficiency(ICOMP)=
     &    TyERHEAT(ICOMP)/HeatSCoP_T(IHLZP)
        TyER_HEAT=TyER_HEAT+TyERHEAT_afterefficiency(ICOMP)
        TyER_HEATCO2=TyER_HEATCO2+
     &  TyERHEAT_afterefficiency(ICOMP)*FUELCO2(1)

C Heating and hot water are always supplied by Natural Gas for typical building (NHF=1)
        TyERF(1)=TyERF(1)+TyERHEAT_afterefficiency(ICOMP)  
        TyERSH(IHLZP)=TyERSH(IHLZP)+TyERHEAT_afterefficiency(ICOMP)

C Cooling energy only if a coolth generator is associated with this zone
        TyERCOOL(ICOMP)=ABS(TyERCOOL(ICOMP))
        NEAR=.FALSE.
        CALL ECLOSE(CoolSSEER_T(IHLZP),0.0,0.001,NEAR)
        IF(.NOT.NEAR)THEN
          TyERCOOL_afterefficiency(ICOMP)=
     &      TyERCOOL(ICOMP)/CoolSSEER_T(IHLZP)
        ELSE
          TyERCOOL_afterefficiency(ICOMP)=0.0
        ENDIF
        TyER_COOL=TyER_COOL+TyERCOOL_afterefficiency(ICOMP)
        TyER_COOLCO2=TyER_COOLCO2+
     &    TyERCOOL_afterefficiency(ICOMP)*FUELCO2(10)
        TyERF(10)=TyERF(10)+TyERCOOL_afterefficiency(ICOMP)
        TyERSC(IHLZP)=TyERSC(IHLZP)+TyERCOOL_afterefficiency(ICOMP)

C DHW energy (depending upon whether there is a stand alone DHW
C generator for this zone or DHW is additionally generated from the
C space heating HVAC system. Each zone has to have a DHW source, if the
C user had not defined it assume DHW is supplied from HVAC system
        IF(IDHWLZ(ICOMP).GT.0)THEN
          TyERDHW_afterefficiency(ICOMP)=TyERDHW(ICOMP)/DHWNGE
          DHWCO2RATE=FUELCO2(1)

C Heating and hot water are always supplied by Natural Gas for typical building (NHF=1)
          TyERF(1)=TyERF(1)+TyERDHW_afterefficiency(ICOMP)
        ELSEIF(IDHWLZ(ICOMP).LE.0)THEN
          TyERDHW_afterefficiency(ICOMP)=
     &      TyERDHW(ICOMP)/HeatSCoP_T(IHLZP)
          DHWCO2RATE=FUELCO2(NHF)

C Heating and hot water are always supplied by Natural Gas for typical building (NHF=1)
          TyERF(1)=TyERF(1)+TyERDHW_afterefficiency(ICOMP)
        ENDIF
        TyER_DHW=TyER_DHW+TyERDHW_afterefficiency(ICOMP)
        TyER_DHWCO2=
     &    TyER_DHWCO2+TyERDHW_afterefficiency(ICOMP)*DHWCO2RATE

C For both lighting and auxiliary energy assume these are supplied by
C the electrical grid (Fuel type 10 from SBEM.db1)
C Auxiliary energy
        TyER_AUX=TyER_AUX+TyERAUX(ICOMP)
        TyER_AUXCO2=TyER_AUXCO2+TyERAUX(ICOMP)*FUELCO2(10)
        TyERF(10)=TyERF(10)+TyERAUX(ICOMP)
        TyERSA(IHLZP)=TyERSA(IHLZP)+TyERAUX(ICOMP)
        ENDIF

C Lighting energy
        TyER_LIGHT=TyER_LIGHT+TyERLIGHT(ICOMP)
        TyER_LIGHTCO2=TyER_LIGHTCO2+TyERLIGHT(ICOMP)*FUELCO2(10)
        TyERF(10)=TyERF(10)+TyERLIGHT(ICOMP)
 400  CONTINUE

C Calculation of emissions and total energy for notional model
C TyERE is not yet used anywhere but could be used in the future
C      TyERE=TyER_HEAT+TyER_COOL+TyER_DHW+TyER_LIGHT+TyER_AUX
      TyER=TyER_HEATCO2+TyER_COOLCO2+TyER_DHWCO2+TyER_LIGHTCO2+
     &     TyER_AUXCO2

C Normalise by floor area
C      TyERE=TyERE/TBASEA
      TyER=TyER/TBASEA
      DO 4000 ICMSYS=1,NCMSYS
        TyERSH(ICMSYS)=TyERSH(ICMSYS)/TBASEA
        TyERSC(ICMSYS)=TyERSC(ICMSYS)/TBASEA
        TyERSA(ICMSYS)=TyERSA(ICMSYS)/TBASEA
 4000 CONTINUE
      DO 4001 IHF=1,MFT
        TyERF(IHF)=TyERF(IHF)/TBASEA
 4001 CONTINUE
      TyER_HEAT=TyER_HEAT/TBASEA
      TyER_COOL=TyER_COOL/TBASEA
      TyER_AUX=TyER_AUX/TBASEA
      TyER_LIGHT=TyER_LIGHT/TBASEA
      TyER_DHW=TyER_DHW/TBASEA
      IF(IER.EQ.0)RETURN  ! SOMETHING WRONG HAS OCCURED
      RETURN
      END

************************* NCMCAL *****************************
C Subroutine NCMCAL checks if sufficient information is present in the
C model in order to generate the BRUKL input file. Sufficient
C information being:
C 1. Information about project (building), owner and certifier.
C    This information can be added from the context menu.
C    If this information is not present then it asks the user for it.
C 2. Makes sure building type information is also present in the model.
C    If not then asks user for it.
C 3. There must be a working notional model in the cfg folder. If
C    not then automatically makes one. There is facility in the variants
C    option in the main project manager menu to manually make a notional
C    model.
C 4. Makes sure that SBEM plant and systems have been defined and that
C    there is a linkage between zones and plant types. If SBEM plant and
C    systems are not defined it provides user with the opportunity to do
C    so.
C    NOTE: SBEM plant and systems are not part of dynamic thermal
C    simulation but make use of seasonal efficiency values to generate
C    carbon dioxide emissions.
C If sufficient information is not present it generates
C this information silently as far as possible.
C It then calls simulation for the actual building and notional model,
C Then maps energy use to zone linkage of SBEM plant and systems and
C generates Building Emission Rating and Target Emission Rating, BER
C and TER.
C It finally generate BRUKL input document and other SBEM reports.
      SUBROUTINE NCMCAL(IER)
#include "sbem.h"
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
#include "geometry.h"
#include "material.h"
#include "espriou.h"
      common/FILEP/IFIL

      integer ncomp,ncon
      common/C1/NCOMP,NCON
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/OUTIN/IUOUT,IUIN
      integer nbrdg, ibrdg
      real psi,lngth,losspercent,totheatloss,thbrpercent
      real uavtotal
      common/THRBRDG/nbrdg(MCOM),psi(MCOM,16),lngth(MCOM,16),
     &               ibrdg(MCOM,16),losspercent(MCOM),totheatloss(MCOM),
     &               thbrpercent,uavtotal(MCOM)
      common/rpath/path
      character DOIT*248
      character limgpth*24,ldocpth*24,ltmppth*24,lcfgroot*24,lpath*72
      character path*72,lupath*72
      character fs*1, col*1
      character BATCHFL*72

C Build variable (These should eventually be placed in build_info.h)
      logical bBuild_Info            ! Flag indicating if build info should
                                     !   be dumped.
C      character cURL*256             ! Repository URL commented out in build_info.h

      CHARACTER BRUKFL*72
      CHARACTER CFGROOT*24,LCFGF*72,MODE*4
      CHARACTER TMPSTR*24,TMPSTR2*70
      character delim*1,louts*248,outs*124,TEMP1*42
      LOGICAL CONTEXT,BNDRY,ISNEAR,ISNEAR2,ISNEAR3
      LOGICAL UNIXOK
      REAL TMPREAL
      integer iactFlarea
      real act_area !for the ACT-AREA entry of BRUKL file::lists sequentialy
                    !activity numbers and floor areas
      dimension act_area(ITWOMNS)
      dimension hvacarea(MNS) !area serviced by the same hvac system - MHT is from sbem.h
      REAL hvacarea
      integer Act_HVAC !temporary passing activity index to hvac
      dimension Act_HVAC(MHT,MCOM),TotHVACactArea(ITWOMNS)
      real TotHVACactArea,TMPVAL
      integer IhvAct
      real zhvacarea !temporary passing activity index to hvac
      dimension zhvacarea(MNS,MCOM)
      real b_alpha

C Names of surfaces associated with highest U value.
      character WALL_MAX_N*12,FLOR_MAX_N*12,ROOF_MAX_N*12,WNRF_MAX_N*12
      character P_DR_MAX_N*12,V_DR_MAX_N*12,H_DR_MAX_N*12
      logical flagoverheating

C Counters for components of buildings.
      integer N_WALL,N_FLOR,N_ROOF,N_WNRF,N_P_DR,N_V_DR
      integer N_H_DR,I_WALL,I_FLOR,I_ROOF,I_WNRF,I_P_DR,I_V_DR,I_H_DR

C build_info is generated by the Install script.
#include "build_info.h"

C Read  notional model configuration file (and hence geometry files)
C The value of uavtotal will have been updated as the geometry files
C were scanned.
      IFCFG=IFIL+1
      IAPROB=IFIL+2
      MODE='ALL '
      ITRC=4 ! To skip warning message about notional model
      CALL ERSYS(LCFGF_N,IFCFG,IAPROB,MODE,itrc,IER)
      build_UAN=0.0    ! for notional
      do 131 iz=1, ncomp
        build_UAN=uavtotal(iz)+build_UAN
 131  continue

C Read cfg file for the stripped model
      IFCFG=IFIL+3
      IAPROB=IFIL+4
      MODE='ALL '
      ITRC=4 ! To skip warning message about stripped model
      CALL ERSYS(LCFGF_S,IFCFG,IAPROB,MODE,itrc,IER)

C build_UA and b_totheatloss are building's UA values and heat loss coefficient
C values due to thermal bridges in W/K
C If users do not enter the thermal bridges menu the uavtotal will be 0 and
C the overall build_UA will be less than usual
      build_UA=0.0    ! for other than notional
      b_totheatloss=0.0
      tot_floor_area=0.0
      do 31 iz=1, ncomp
        tot_floor_area=zbasea(iz)+tot_floor_area
        build_UA=uavtotal(iz)+build_UA
        b_totheatloss=totheatloss(iz)+b_totheatloss
 31   continue

C Read *.ncm
      CALL RSBEM

C Start writing BRUKL file
C Building, owner, certifier detail...
      ILEN=LNBLNK(LASBEM)
      WRITE(BRUKFL,'(2A)')LASBEM(1:ILEN-4),'_brukl.inp'
      IUF=IFIL+1
      CALL EFOPSEQ(IUF,BRUKFL,3,IER)
      if(ier.eq.0)THEN
        write(currentfile,'(a)') BRUKFL(1:lnblnk(BRUKFL))
      ELSE ! WARNING MESSAGE HERE
      ENDIF
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3) '$'
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &  '$ Compliance input file for BR PART L 2006'
      WRITE(IUF,'(a)',iostat=ios)  '$ ESP-r -> BRUKL 3.2.b'
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3) '$'
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3) '"BRUKL-PROJECT" = GENERAL'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)'  P-NAME             = ',
     &  '"',pjname(1:lnblnk(pjname)),'"'
      IF(IBUSERTYP.NE.0)THEN
        TMPSTR2=BTYPNAME(IBUSERTYP)
      ELSE
        TMPSTR2='Building type not defined (yet)'
      ENDIF
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'  B-TYPE             = ',
     &  TMPSTR2(1:LNBLNK(TMPSTR2))
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)'  B-ADDRESS          = ',
     &  '"',BADDRESS(1:lnblnk(BADDRESS)),'"'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)'  B-CITY             = ',
     &  '"',BCITY(1:lnblnk(BCITY)),'"'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)'  B-POSTCODE         = ',
     &  '"',BUILDINGPC(1:lnblnk(BUILDINGPC)),'"'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)'  O-NAME             = ',
     &  '"',ownername(1:lnblnk(ownername)),'"'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)'  O-TELEPHONE        = ',
     &  '"',ownertel(1:lnblnk(ownertel)),'"'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)'  O-ADDRESS          = ',
     &  '"',owneraddr(1:lnblnk(owneraddr)),'"'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)'  O-CITY             = ',
     &  '"',ownercity(1:lnblnk(ownercity)),'"'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)'  O-POSTCODE         = ',
     &  '"',ownerPC(1:lnblnk(ownerPC)),'"'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)'  C-NAME             = ',
     &  '"',certifname(1:lnblnk(certifname)),'"'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)'  C-TELEPHONE        = ',
     &  '"',certTel(1:lnblnk(certTel)),'"'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)'  C-ADDRESS          = ',
     &  '"',certaddress(1:lnblnk(certaddress)),'"'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)'  C-CITY             = ',
     &  '"',certcity(1:lnblnk(certcity)),'"'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)'  C-POSTCODE         = ',
     &  '"',certPC(1:lnblnk(certPC)),'"'

C Calculation engine and interface detail...
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'  CENGINE            = ',
     &  '"ESP-r"'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)'  CENGINE-VERSION    = ',
     &  cBranch(1:lnblnk(cBranch)),'@',
     &  cRelease_num(1:lnblnk(cRelease_num))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'  INTERFACE          = ',
     &  '"ESP-r Project Manager"'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)'  INTERFACE-VERSION  = ',
     &  '"',cRelease_num(1:lnblnk(cRelease_num)),'"'
      WRITE(IUF,'(a)',iostat=ios)       '  ..'
      WRITE(IUF,'(2a)',iostat=ios) '"ESP-r" = ','COMPLIANCE'
      WRITE(IUF,'(2a)',iostat=ios)      '  TYPE               = ',
     &  BLDREG(IRGG)(1:LNBLNK(BLDREG(IRGG)))
      WRITE(IUF,'(2a)',iostat=ios)      '  BR-STAGE           = ',
     &  BLDSTG(ISTG)(1:LNBLNK(BLDSTG(ISTG)))

C Enter Scottish building information if applicable
      IF(ISBT.NE.0)THEN
        WRITE(IUF,'(2a)',iostat=ios)    '  BR-S6-BTYPE        = ',
     &  SBTYP(ISBT)(1:LNBLNK(SBTYP(ISBT)))
        WRITE(IUF,'(2a)',iostat=ios)    '  BR-S6-ACCRED-CONST = ',SBREF
      ENDIF

C If at least one zone does not have "low risk" for overheating
C then OVERHEAT-RISK will be switched to yes
      flagoverheating=.false.
      do 550 iz=1, ncomp
        if(prcnt_Overheat(iz).gt.0.5)then
          flagoverheating=.true.
        endif
 550  continue
      if(flagoverheating)then
        WRITE(IUF,'(2a)',iostat=ios)    '  OVERHEAT-RISK      = ','YES'
      else
        WRITE(IUF,'(2a)',iostat=ios)    '  OVERHEAT-RISK      = ','NO'
      endif
      WRITE(IUF,'(a)',iostat=ios)       '  ..'

C CHECK1 detail...
      WRITE(IUF,'(a)',iostat=ios) '"CHECK1" = CHECK1'
      WRITE(IUF,'(a,F7.1)',iostat=ios)  '  KG-CO2-NOT/M2      = ',AER
      WRITE(IUF,'(a,F7.1)',iostat=ios)  '  BER                = ',BER
      WRITE(IUF,'(a,F7.1)',iostat=ios)  '  TER                = ',TER
      WRITE(IUF,'(a,F4.2)',iostat=ios)  '  IF                 = ',FXIF
      WRITE(IUF,'(a,F4.2)',iostat=ios)  '  RB                 = ',XLZC
      WRITE(IUF,'(a)',iostat=ios)       '  ..'

C CHECK2 detail...
      WALL_MAX_U=0. ; WALL_AVG_U=0. ; FLOR_MAX_U=0. ; FLOR_AVG_U=0.
      ROOF_MAX_U=0. ; ROOF_AVG_U=0. ; WNRF_MAX_U=0. ; WNRF_AVG_U=0.
      P_DR_MAX_U=0. ; P_DR_AVG_U=0. ; V_DR_MAX_U=0. ; V_DR_AVG_U=0.
      H_DR_MAX_U=0. ; H_DR_AVG_U=0.
      flat_ROOF_MAX_U=0. ; pitched_ROOF_MAX_U=0.

      N_WALL=0 ; N_FLOR=0 ; N_ROOF=0 ; N_WNRF=0 ; N_P_DR=0 ; N_V_DR=0
      N_H_DR=0 ; I_WALL=0 ; I_FLOR=0 ; I_ROOF=0 ; I_WNRF=0 ; I_P_DR=0
      I_V_DR=0 ; I_H_DR=0
      EXP_AREA=0. ! External envelope area
c      EXP_COND=0.   ! External envelope conductance W/K
      DO 114 ICOMP=1,NCOMP
        DO 214 IS=1,NZSUR(ICOMP)
          ICC=IZSTOCN(ICOMP,IS)
          TMPSTR=SSOTHER(ICC,1)
          if(TMPSTR(1:8).eq.'EXTERIOR'.OR.TMPSTR(1:6).EQ.'GROUND')then

C Recover the ISO 6946 U values as in prjqa.F.
            do 7511 ii=1,nmlc
              if(SSMLCN(icc)(1:12).eq.DESC(ii)(1:12))then
                call etmldbu(0,itu,ii,UVH,UVU,UVD,UVI,UVG)
              endif
 7511       continue
            TMPSTR2=SSUSE(ICC,1)

C Floors may not have usage defined (because it is mandatorily required
C only for exterior surfaces)
            IF(SSVFC(ICC)(1:4).EQ.'FLOR')THEN
              IF(FLOR_MAX_U.LT.UVD)THEN
                FLOR_MAX_U=UVD
                FLOR_MAX_N=SSNAME(ICC)
                I_FLOR=1
              ENDIF
              FLOR_AVG_U=FLOR_AVG_U+UVD
              N_FLOR=N_FLOR+1
              EXP_AREA=EXP_AREA+SSNA(ICC)
            ELSEIF(TMPSTR2(1:6).EQ.'P-DOOR')THEN
              IF(P_DR_MAX_U.LT.UVH)THEN
                P_DR_MAX_U=UVH
                P_DR_MAX_N=SSNAME(ICC)
                I_P_DR=1
              ENDIF
              P_DR_AVG_U=P_DR_AVG_U+UVH
              N_P_DR=N_P_DR+1
              EXP_AREA=EXP_AREA+SSNA(ICC)
c              EXP_COND=EXP_COND+SSNA(ICC)*UVH
            ELSEIF(TMPSTR2(1:6).EQ.'H-DOOR')THEN
              IF(H_DR_MAX_U.LT.UVH)THEN
                H_DR_MAX_U=UVH
                H_DR_MAX_N=SSNAME(ICC)
                I_H_DR=1
              ENDIF
              H_DR_AVG_U=H_DR_AVG_U+UVH
              N_H_DR=N_H_DR+1
              EXP_AREA=EXP_AREA+SSNA(ICC)
c              EXP_COND=EXP_COND+SSNA(ICC)*UVH
            ELSEIF(TMPSTR2(1:6).EQ.'V-DOOR')THEN
              IF(V_DR_MAX_U.LT.UVH)THEN
                V_DR_MAX_U=UVH
                V_DR_MAX_N=SSNAME(ICC)
                I_V_DR=1
              ENDIF
              V_DR_AVG_U=V_DR_AVG_U+UVH
              N_V_DR=N_V_DR+1
              EXP_AREA=EXP_AREA+SSNA(ICC)
c              EXP_COND=EXP_COND+SSNA(ICC)*UVH
            ELSEIF(TMPSTR2(1:7).EQ.'F-FRAME'.OR.TMPSTR2(1:5).EQ.
     &      'FRAME')THEN
              ! No checks on window frames are made
              EXP_AREA=EXP_AREA+SSNA(ICC)
            ELSEIF(TMPSTR2(1:4).EQ.'ROOF')THEN
              IF(ROOF_MAX_U.LT.UVU)THEN
                ROOF_MAX_U=UVU
                ROOF_MAX_N=SSNAME(ICC)
                I_ROOF=1
                if(ssuse(icc,2)(1:4).eq.'FLAT')then
                  flat_ROOF_MAX_U=ROOF_MAX_U
                elseif(ssuse(icc,2)(1:7).eq.'PITCHED')then
                  pitched_ROOF_MAX_U=ROOF_MAX_U
                endif
              ENDIF
              ROOF_AVG_U=ROOF_AVG_U+UVU ! Assume upward heat transfer
              N_ROOF=N_ROOF+1
              EXP_AREA=EXP_AREA+SSNA(ICC)
c              EXP_COND=EXP_COND+SSNA(ICC)*UVU
            ELSEIF(TMPSTR2(1:4).EQ.'WALL')THEN
              IF(WALL_MAX_U.LT.UVH)THEN
                WALL_MAX_U=UVH
                WALL_MAX_N=SSNAME(ICC)
                I_WALL=1
              ENDIF
              WALL_AVG_U=WALL_AVG_U+UVH
              N_WALL=N_WALL+1
              EXP_AREA=EXP_AREA+SSNA(ICC)
c              EXP_COND=EXP_COND+SSNA(ICC)*UVH
            ELSEIF(TMPSTR2(1:8).EQ.'C-WINDOW')THEN
              IF(WNRF_MAX_U.LT.UVH)THEN
                WNRF_MAX_U=UVH
                WNRF_MAX_N=SSNAME(ICC)
                I_WNRF=1
              ENDIF
              WNRF_AVG_U=WNRF_AVG_U+UVH
              N_WNRF=N_WNRF+1
              EXP_AREA=EXP_AREA+SSNA(ICC)
            ELSEIF(TMPSTR2(1:8).EQ.'D-WINDOW')THEN
              ! No checks on display window are made
              EXP_AREA=EXP_AREA+SSNA(ICC)
            ELSEIF(TMPSTR2(1:8).EQ.'S-WINDOW')THEN
              IF(WNRF_MAX_U.LT.UVH)THEN
                WNRF_MAX_U=UVH
                WNRF_MAX_N=SSNAME(ICC)
                I_WNRF=1
              ENDIF
              WNRF_AVG_U=WNRF_AVG_U+UVH
              N_WNRF=N_WNRF+1
              EXP_AREA=EXP_AREA+SSNA(ICC)
c              EXP_COND=EXP_COND+SSNA(ICC)*UVH
            ELSE
              write(outs,'(5a)')'Exterior surface ',ssname(icc),
     &        ' in zone ',zname(icomp)
              call edisp(iuout,outs)
              call edisp(iuout,
     &        'does not have usage defined and has been excluded')
              call edisp(iuout,'from BRUKL (UK regulation) checks')
            ENDIF
          ENDIF
 214    CONTINUE
 114  CONTINUE
      IF(N_WALL.GT.0)THEN
        WALL_AVG_U=WALL_AVG_U/N_WALL
      ELSE
        WALL_AVG_U=0.0
      ENDIF
      IF(N_FLOR.GT.0)THEN
        FLOR_AVG_U=FLOR_AVG_U/N_FLOR
      ELSE
        FLOR_AVG_U=0.0
      ENDIF
      IF(N_ROOF.GT.0)THEN
        ROOF_AVG_U=ROOF_AVG_U/N_ROOF
      ELSE
        ROOF_AVG_U=0.0
      ENDIF
      IF(N_WNRF.GT.0)THEN
        WNRF_AVG_U=WNRF_AVG_U/N_WNRF
      ELSE
        WNRF_AVG_U=0.0
      ENDIF
      IF(N_P_DR.GT.0)THEN
        P_DR_AVG_U=P_DR_AVG_U/N_P_DR
      ELSE
        P_DR_AVG_U=0.0
      ENDIF
      IF(N_V_DR.GT.0)THEN
        V_DR_AVG_U=V_DR_AVG_U/N_V_DR
      ELSE
        V_DR_AVG_U=0.0
      ENDIF
      IF(N_H_DR.GT.0)THEN
        H_DR_AVG_U=H_DR_AVG_U/N_H_DR
      ELSE
        H_DR_AVG_U=0.0
      ENDIF

C Write information for walls
      WRITE(IUF,'(a)',iostat=ios) '"CHECK2" = CHECK2'
      WRITE(IUF,'(a,f6.3)',iostat=ios)  '  WALL-U-AVE         = ',
     &  WALL_AVG_U
      WRITE(IUF,'(a,f6.3)',iostat=ios)  '  WALL-U-MAX         = ',
     &  WALL_MAX_U
      if(I_WALL.gt.0)then
        WRITE(IUF,'(2a)',iostat=ios)    '  WALL-MAX           = ',
     &    WALL_MAX_N
      else
        WRITE(IUF,'(2a)',iostat=ios)    '  WALL-MAX           = ',
     &  '"No external walls in project"'
      endif

C write out information for floors
      WRITE(IUF,'(a,f6.3)',iostat=ios)  '  FLOOR-U-AVE        = ',
     &  FLOR_AVG_U
      WRITE(IUF,'(a,f6.3)',iostat=ios)  '  FLOOR-U-MAX        = ',
     &  FLOR_MAX_U
      if(I_FLOR.gt.0)then
        WRITE(IUF,'(2a)',iostat=ios)    '  FLOOR-MAX          = ',
     &    FLOR_MAX_N
      else
        WRITE(IUF,'(2a)',iostat=ios)    '  FLOOR-MAX          = ',
     &    '"No floors in project"'
      endif

C write out information for roofs
      WRITE(IUF,'(a,f6.3)',iostat=ios)  '  ROOF-U-AVE         = ',
     &  ROOF_AVG_U
      WRITE(IUF,'(a,f6.3)',iostat=ios)  '  ROOF-U-MAX         = ',
     &  ROOF_MAX_U
      if(I_ROOF.gt.0)then
        WRITE(IUF,'(2a)',iostat=ios)    '  ROOF-MAX           = ',
     &    ROOF_MAX_N
      else
        WRITE(IUF,'(2a)',iostat=ios)    '  ROOF-MAX           = ',
     &    '"No external roofs in project"'
      endif

C write out information for windows and rooflights
      WRITE(IUF,'(a,f6.3)',iostat=ios)  '  WINROOF-U-AVE      = ',
     &  WNRF_AVG_U
      WRITE(IUF,'(a,f6.3)',iostat=ios)  '  WINROOF-U-MAX      = ',
     &  WNRF_MAX_U
      if(I_WNRF.gt.0)then
        WRITE(IUF,'(2a)',iostat=ios)    '  WINROOF-MAX        = ',
     &    WNRF_MAX_N
      else
        WRITE(IUF,'(2a)',iostat=ios)    '  WINROOF-MAX        = ',
     &    '"No external windows or rooflights in project"'
      endif

C write out information for personnel doors
      WRITE(IUF,'(a,f6.3)',iostat=ios)  '  P-DOOR-U-AVE       = ',
     &  P_DR_AVG_U
      WRITE(IUF,'(a,f6.3)',iostat=ios)  '  P-DOOR-U-MAX       = ',
     &  P_DR_MAX_U
      if(I_P_DR.gt.0)then
        WRITE(IUF,'(2a)',iostat=ios)    '  P-DOOR-MAX         = ',
     &    P_DR_MAX_N
      else
        WRITE(IUF,'(2a)',iostat=ios)    '  P-DOOR-MAX         = ',
     &    '"No Personnel doors in project"'
      endif

C write out information for vehicle doors
      WRITE(IUF,'(a,f6.3)',iostat=ios)  '  V-DOOR-U-AVE       = ',
     &  V_DR_AVG_U
      WRITE(IUF,'(a,f6.3)',iostat=ios)  '  V-DOOR-U-MAX       = ',
     &  V_DR_MAX_U
      if(I_V_DR.gt.0.0)then
        WRITE(IUF,'(2a)',iostat=ios)    '  V-DOOR-MAX         = ',
     &    V_DR_MAX_N
      else
        WRITE(IUF,'(2a)',iostat=ios)    '  V-DOOR-MAX         = ',
     &    '"No vehicle doors in project"'
      endif

C write out information for high usage doors
      WRITE(IUF,'(a,f6.3)',iostat=ios)  '  H-DOOR-U-AVE       = ',
     &  H_DR_AVG_U
      WRITE(IUF,'(a,f6.3)',iostat=ios)  '  H-DOOR-U-MAX       = ',
     &  H_DR_MAX_U
      if(I_H_DR.gt.0.0)then
        WRITE(IUF,'(2a)',iostat=ios)    '  H-DOOR-MAX         = ',
     &  H_DR_MAX_N
      else
        WRITE(IUF,'(2a)',iostat=ios)    '  H-DOOR-MAX         = ',
     &    '"No High usage entrance doors in project"'
      endif

C Following is no longer in use
      WRITE(IUF,'(a)',iostat=ios)       '  S-VENT-U-AVE       =  0'
      WRITE(IUF,'(a)',iostat=ios)       '  S-VENT-U-MAX       =  0'
      WRITE(IUF,'(2a)',iostat=ios)      '  S-VENT-MAX         = ',
     &  '"No Roof ventilators in project"'

C Write infiltration and permeability information
      WRITE(IUF,'(a,F5.2)',iostat=ios)  '  Q50-INF            = ',BINF50
      WRITE(IUF,'(2a)',iostat=ios)      '  Q50-INF-CHECK      = ',APCHK
      WRITE(IUF,'(a)',iostat=ios)       '  ..'

C DHW checks
      DO 40 IDHWSYS=1,NDHWSYS
        TMPSTR2=DHWNAME(IDHWSYS)
        write(iuf,'(3a)',iostat=ios)'"',TMPSTR2(1:LNBLNK(TMPSTR2)),
     &  '" = CHECK2-DHW'
        write(iuf,'(a,I5)',iostat=ios)  '  HVACGUIDE-DHW      = ',
     &   IBRUKW(IDHWSYS)
        write(iuf,'(a,F6.3)',iostat=ios)'  DHW-ACT-EFF        = ',
     &   HWEF(IDHWSYS)
        WRITE(IUF,'(a)',iostat=ios)     '  ..'
 40   CONTINUE

C Write information about systems
      DO 20  ICMSYS=1,NCMSYS
        TMPSTR2=HVACNAME(ICMSYS)
        WRITE(IUF,'(3a)',iostat=ios)'"',TMPSTR2(1:LNBLNK(TMPSTR2)),
     &    '" = CHECK2-11'

C << we need a value for $HG-TYPE >>
        WRITE(IUF,'(2a)',iostat=ios)    '  $HG-TYPE           = ',
     &    '46'
        WRITE(IUF,'(a,F6.3)',iostat=ios)'  HEAT-GEN-SEFF      = ',
     &    HGEF(ICMSYS)
        TMPREAL=CGEF(ICMSYS)
        CALL ECLOSE(TMPREAL,0.0,0.01,ISNEAR)

C If cooling efficiency is near zero set its value to default of 0
C (ref: BRUKL document of 9 July 2008)
        IF(ISNEAR)TMPREAL=0.
        WRITE(IUF,'(a,F6.3)',iostat=ios)'  COOL-GEN-EER       = ',
     &    TMPREAL

C Check if we have data for this combination of IBRUKLH.
        if(IFTYP(ICMSYS).ne.0.and.IHGEF(ICMSYS).ne.0.and.
     &     INCMSYS(ICMSYS).ne.0)then
          IBRUK=IBRUKLH(IFTYP(ICMSYS),IHGEF(ICMSYS),INCMSYS(ICMSYS))
        else
          IBRUK=0
        endif
        IF(IBRUK.LE.0)IBRUK=-5555
        WRITE(IUF,'(a,I5)',iostat=ios)  '  HVACGUIDE-HEAT     = ',IBRUK

C Check if we have data for this combination of IBRUKLC.
        if(ICGEF(ICMSYS).ne.0.and.INCMSYS(ICMSYS).ne.0)then
          IBRUK=IBRUKLC(ICGEF(ICMSYS),INCMSYS(ICMSYS))
        else
          IBRUK=0
        endif

        IF(IBRUK.LE.0)IBRUK=-5555
        IF(INCMSYS(ICMSYS).LE.11)IBRUK=-1111
        WRITE(IUF,'(a,I5)',iostat=ios)  '  HVACGUIDE-COOL     = ',IBRUK
        WRITE(IUF,'(A,I5)',iostat=ios)  '  HVACGUIDE-SFP      = ',
     &    IBRUKLF(INCMSYS(ICMSYS))
        TMPREAL=SFPHS(ICMSYS)
        CALL ECLOSE(TMPREAL,0.0,0.01,ISNEAR)
        IF(ISNEAR)THEN
          WRITE(IUF,'(a)',iostat=ios)   '  SFP-ACT            = -6666'
        ELSE
          WRITE(IUF,'(a,F6.3)',iostat=ios)'  SFP-ACT            = ',
     &    TMPREAL
        ENDIF
        WRITE(IUF,'(a)',iostat=ios)     '  ..'
 20   CONTINUE

C If Scottish building regulations, skip checks 3 to 6 inclusive
      IF(ISBT.EQ.0)THEN

C Write remainder of the check information
        do 30 iz=1, ncomp
          WRITE(IUF,'(3a)',iostat=ios) '"',
     &    zname(iz)(1:lnzname(IZ)),'" = CHECK3'

C Rules for overheating are based on NCM modelling Guide v4d.
          call eclose(prcnt_Overheat(iz),0.5,0.0001,ISNEAR)
          call eclose(prcnt_Overheat(iz),1.0,0.0001,ISNEAR2)
          call eclose(prcnt_Overheat(iz),1.5,0.0001,ISNEAR3)
          if(prcnt_Overheat(iz).lt.0.5)then
            WRITE(IUF,'(2a)',iostat=ios)
     &        '  OVERHEATING-RISK   = ','Low risk'
          elseif(ISNEAR.or.(prcnt_Overheat(iz).gt.0.5.and.
     &           prcnt_Overheat(iz).lt.1.0))then
            WRITE(IUF,'(2a)',iostat=ios)
     &        '  OVERHEATING-RISK   = ','Moderate risk'
          elseif(ISNEAR2.or.(prcnt_Overheat(iz).gt.1.0.and.
     &           prcnt_Overheat(iz).lt.1.5))then
            WRITE(IUF,'(2a)',iostat=ios)
     &        '  OVERHEATING-RISK   = ','Significant risk'
          elseif(ISNEAR3.or.prcnt_Overheat(iz).gt.1.5)then
            WRITE(IUF,'(2a)',iostat=ios)
     &        '  OVERHEATING-RISK   = ','High risk'
          endif
          WRITE(IUF,'(a)',iostat=ios)   '  ..'
 30     continue

        WRITE(IUF,'(a)',iostat=ios) '"CHECK4" = CHECK4'
        WRITE(IUF,'(a,F5.2)',iostat=ios)'  Q50-INF            = ',
     &    BINF50
        WRITE(IUF,'(a)',iostat=ios)     '  ..'

        WRITE(IUF,'(a)',iostat=ios)     '"CHECK5" = CHECK5'
        WRITE(IUF,'(a)',iostat=ios)     '  ..'

        WRITE(IUF,'(a)',iostat=ios)     '"CHECK6" = CHECK6'
        WRITE(IUF,'(a)',iostat=ios)     '  ..'
      ENDIF

C Write building data for Stripped building
      WRITE(IUF,'(2a)',iostat=ios)      '"BUILDING_DATA" = ',
     &  'BUILDING-DATA'
      WRITE(IUF,'(a)',iostat=ios)
     &  '  ANALYSIS                  = ACTUAL'
      WRITE(IUF,'(a,f8.2)',iostat=ios)
     &  '  AREA                      = ',tot_floor_area

      WRITE(IUF,'(a,f8.2)',iostat=ios)
     &  '  AREA-EXT                  = ',EXP_AREA

C This will have to be based on the location of the study
C?? To hold weather information in *ncm file
      WRITE(IUF,'(a)',iostat=ios)'  WEATHER                   = LON'

C Confirm that BINF50 instead of some sort of "translated" air changes
      WRITE(IUF,'(a,F5.2)',iostat=ios)
     &  '  Q50-INF                   = ',BINF50

C BRUKL Manual defines this as the "building's average thermal conductance
C through all external/exposed envelopes"
C If users do not enter the thermal bridges menu the uavtotal will be 0 and
C the overall build_UA will be less than usual
      WRITE(IUF,'(a,f9.2)',iostat=ios)
     &  '  BUILDING-W/K              = ',build_UA

C BRUKL Manual defines this as the "building's average heat transfer
C coefficient through all external/exposed envelopes"
      call eclose(EXP_AREA,0.0,0.0001,ISNEAR)
      if(.not.ISNEAR)then
        WRITE(IUF,'(a,f6.2)',iostat=ios)
     &    '  BUILDING-W/M2K            = ',build_UA/EXP_AREA
      else
        WRITE(IUF,'(a)',iostat=ios)
     &    '  BUILDING-W/M2K            =  0.0'
      endif

C Using info from common of thermal bridges to find the percentage of heat loss
C due to thermal bridges out of the overall heat loss from the envelope. build_UA
C was made up from sum of uavtotal and b_totheatloss is the sum of thermal bridge 
C psi and lengths.
      call eclose(build_UA,0.0,0.0001,ISNEAR)
      if(.not.ISNEAR)then
        b_alpha=(b_totheatloss/build_UA)*100.0
      else
        b_alpha=0.0
      endif
      WRITE(IUF,'(a,f6.2)',iostat=ios)
     &  '  BUILDING-ALPHA            = ',b_alpha

C Calculate loads for heating, cooling, DHW etc.
      WRITE(IUF,'(a,f9.2)',iostat=ios)
     &  '  KWH/M2-HEAT               = ',ABS(BER_HEAT)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-COOL               = ',ABS(BER_COOL)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-AUX                = ',BER_AUX
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-LIGHT              = ',BER_LIGHT
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-DHW                = ',BER_DHW

C?? To enter total casual gains/m2 here
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-EQUP               = ',0.0
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-NATGAS             = ',BERF(1)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-LPG                = ',BERF(2)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-BIOGAS             = ',BERF(3)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-OIL                = ',BERF(4)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-COAL               = ',BERF(5)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-ANTHRACITE         = ',BERF(6)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-SMOKELESS          = ',BERF(7)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-DUELFUEL           = ',BERF(8)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-BIOMASS            = ',BERF(9)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-SUPELEC            = ',ABS(BERF(10))
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-WASTEHEAT          = ',BERF(12)

C?? To enter the following bits of information
      WRITE(IUF,'(a,F9.2)',iostat=ios) '  KWH/M2-DISTRICT-HEATING   = ',
     &  0.0
      WRITE(IUF,'(a,F9.2)',iostat=ios) '  KWH/M2-DISP               = ',
     &  0.0
      WRITE(IUF,'(a,F9.2)',iostat=ios) '  KWH/M2-PVS                = ',
     &  0.0
      WRITE(IUF,'(a,F9.2)',iostat=ios) '  KWH/M2-WIND               = ',
     &  0.0
      WRITE(IUF,'(a,F9.2)',iostat=ios) '  KWH/M2-CHP                = ',
     &  0.0
      WRITE(IUF,'(a,F9.2)',iostat=ios) '  KWH/M2-SES                = ',
     &  0.0

C Activity number and area per zone
C iactFlarea is a counter to prepare the act_area for the arlist call
C This act_area has to have maximum size 2 times the zone number, using
C ITWOMNS here because it is defined in sbem.h as two times the maximum
C number of activities
      iactFlarea=0
      do 18 iz=1,ncomp
        iactFlarea=iactFlarea+1
        act_area(iactFlarea)=real(theactivityindex(iz))
        iactFlarea=iactFlarea+1
        act_area(iactFlarea)= zbasea(iz)
 18   continue
      itrunc=1
      ipos=1
      delim='C'
      do while (itrunc.ne.0)
        call arlist(ipos,iactFlarea,act_area,ITWOMNS,delim,louts,
     &      loutln,itrunc)

C here we write each activity and the related floor area. SBEM seems to sum up the
C floor areas and writes the activity once if it covers many zones.
C BRUKL v3.2 seems to accept either ways.
        WRITE(IUF,'(4a)',iostat=ios)   '  ACT-AREA                  = ',
     &   '{ ',louts(1:loutln),' }'
        ipos=itrunc+1
      end do
      WRITE(IUF,'(a)',iostat=ios) '  ..'

C Section for each HVAC system
      do 101 ICMSYS=1,NCMSYS
        hvacarea(ICMSYS)=0.
        IhvAct=0
        imerg=0
        do 15 iz=1,ncomp
          if(ICMSYS.eq.IHLZ(IZ))then
            IhvAct=IhvAct+1
            zhvacarea(ICMSYS,IhvAct)=0.
            hvacarea(ICMSYS)=zbasea(iz)+hvacarea(ICMSYS)
            zhvacarea(ICMSYS,IhvAct)= zbasea(iz)
            Act_HVAC(ICMSYS,IhvAct)=theactivityindex(iz)
          endif
 15     continue
        if((hvacarea(ICMSYS)).gt.0.)then
          TMPSTR2=HVACNAME(ICMSYS)
          LNB=lnblnk(TMPSTR2)
          WRITE(IUF,'(4a)',iostat=ios) '"',TMPSTR2(1:LNB),
     &     '"',' = HVAC-SYSTEM-DATA'
          WRITE(IUF,'(a,f8.2)',iostat=ios)
     &      '  AREA                      = ',hvacarea(ICMSYS)
          LNB=lnblnk(HSYSNAME(INCMSYS(ICMSYS)))
          WRITE(IUF,'(2a)',iostat=ios)
     &      '  TYPE                      = ',
     &      HSYSNAME(INCMSYS(ICMSYS))(1:LNB)

C Select keywords for BRUKL based on heat generator (keyword HEAT-SOURCE
C in BRUKL), Need ammendments for LTHW boiler and heat pumps
          OUTS=SYSNAME(IHGEF(ICMSYS))
          TEMP1=FUELNAME(IFTYP(ICMSYS))
          TEMPZS(ICMSYS)=' '
          IF(OUTS(1:4).EQ.'LTHW')THEN
            TEMPZS(ICMSYS)='LTHW boiler'
          ELSEIF(OUTS(1:13).EQ.'Heat pump air')THEN
            IF(TEMP1(1:4).EQ.'Gas'.OR.TEMP1(1:4).EQ.'Oil')THEN
              TEMPZS(ICMSYS)='Heat pump (gas/oil): air source'
            ELSE
              TEMPZS(ICMSYS)='Heat pump (electric): air source'
            ENDIF
          ELSEIF(OUTS(1:13).EQ.'Heat pump gro')THEN
            IF(TEMP1(1:3).EQ.'Gas'.OR.TEMP1(1:3).EQ.'Oil')THEN
              TEMPZS(ICMSYS)=
     &        'Heat pump (gas/oil): ground or water source'
            ELSE
              TEMPZS(ICMSYS)=
     &        'Heat pump (electric): ground or water source'
            ENDIF
          ELSE
            TEMPZS(ICMSYS)=SYSNAME(IHGEF(ICMSYS))
          ENDIF
          LNB=lnblnk(TEMPZS(ICMSYS))
          WRITE(IUF,'(2a)',iostat=ios)
     &      '  HEAT-SOURCE               = ',TEMPZS(ICMSYS)(1:LNB)
          LNB=lnblnk(FUELNAME(IFTYP(ICMSYS)))
          WRITE(IUF,'(2a)',iostat=ios)
     &      '  FUEL-TYPE                 = ',
     &      FUELNAME(IFTYP(ICMSYS))(1:LNB)

C Next line is required even if there is no cooling in the system.
          WRITE(IUF,'(a)',iostat=ios)
     &      '  FUEL-TYPE-COOL            = Grid Supplied Electricity'
          BERSH_DEMAND(ICMSYS)=BERSH(ICMSYS)*HGEF(ICMSYS)
          WRITE(IUF,'(a,F9.2)',iostat=ios)
     &      '  MJ/M2-HEAT-DEM            = ',ABS(BERSH_DEMAND(ICMSYS)*
     &      3.6)
          BERSC_DEMAND(ICMSYS)=BERSC(ICMSYS)*CGEF(ICMSYS)
          WRITE(IUF,'(a,F9.2)',iostat=ios)
     &      '  MJ/M2-COOL-DEM            = ',
     &      ABS(BERSC_DEMAND(ICMSYS)*3.6)
          WRITE(IUF,'(a,F9.2)',iostat=ios)
     &      '  KWH/M2-HEAT               = ',ABS(BERSH(ICMSYS))
          WRITE(IUF,'(a,F9.2)',iostat=ios)
     &      '  KWH/M2-COOL               = ',ABS(BERSC(ICMSYS))
          WRITE(IUF,'(a,F9.2)',iostat=ios)
     &      '  KWH/M2-AUX                = ',BERSA(ICMSYS)

C?? To add system efficienies for heating and cooling which depend upon
C heat/cool generator efficiencies and system adjustments (SFP, M&T,
C ductwork leakage and AHU leakage). Exact relationship for all these
C parameters is unknown but should be available from BRE.
C If there is a heating system use the value, else the default is 0
          WRITE(IUF,'(a,F7.4)',iostat=ios)
     &      '  HEAT-SSEFF                = ',HGEF(ICMSYS) !?? Needs to be updated

C If there is a cooling system then:
          if(IBRUKLC(1,INCMSYS(ICMSYS)).ne.-1111)then
            WRITE(IUF,'(a,F7.4)',iostat=ios)
     &        '  COOL-SSEER                = ',CGEF(ICMSYS) !?? Needs to be updated
          endif
          WRITE(IUF,'(a,F7.4)',iostat=ios)
     &        '  HEAT-GEN-SEFF             = ',HGEF(ICMSYS)

C If there is a cooling system then:
          if(IBRUKLC(1,INCMSYS(ICMSYS)).ne.-1111)then
            WRITE(IUF,'(a,f7.4)',iostat=ios)
     &        '  COOL-GEN-SEER             = ',CGEF(ICMSYS)
          endif

C Now start the logic needed for ACT-AREA entry
          itrunc=1
          ipos=1
          delim='C'
          if(IhvAct.gt.0)then

C Loop through the number of activities that this hvac covers (for their spaces)
C This will prepare TotHVACactArea for the arlist routine
C imerg is used for merging activities and floor areas in order to prepare the
C the array for the call to arlist
            do 17 iloop_IhvAct=1,IhvAct
              imerg=imerg+1
              TotHVACactArea(imerg)=
     &          real(Act_HVAC(ICMSYS,iloop_IhvAct))
              imerg=imerg+1
              TotHVACactArea(imerg)=
     &          zhvacarea(ICMSYS,iloop_IhvAct)
 17         continue
          endif
          do while (itrunc.ne.0)
            call arlist(ipos,imerg,TotHVACactArea,ITWOMNS,delim,louts,
     &        loutln,itrunc)
            WRITE(IUF,'(4a)',iostat=ios)
     &       '  ACT-AREA                  = ',
     &       '{ ',louts(1:loutln),' }'
            ipos=itrunc+1
          end do
          WRITE(IUF,'(a)',iostat=ios) '  ..'
        endif
 101  continue

C Notional building related data
      WRITE(IUF,'(2a)',iostat=ios)'"BUILDING_DATA" = ',
     &  'BUILDING-DATA'
      WRITE(IUF,'(a)',iostat=ios)
     &  '  ANALYSIS                   =  NOTIONAL'
      WRITE(IUF,'(a,f8.2)',iostat=ios)
     &  '  AREA                       = ',tot_floor_area

C Calculate external area as (ext wall exposed + floor exposed
C + roof exposed). This has been concluded looking SBEM 3.1
C and BRUKL output
      WRITE(IUF,'(a,f8.2)',iostat=ios)
     &  '  AREA-EXT                   = ',EXP_AREA

C This will have to be based on the location of the study
      WRITE(IUF,'(a)',iostat=ios)
     &  '  WEATHER                    =  LON'

C Notional building has infiltration of 10m3/m2h at 50Pa (Paragraph 30
C NCM Modelling Guide v4d of March 2008)
      WRITE(IUF,'(a)',iostat=ios)
     &  '  Q50-INF                    = 10'

C Hardwiring values here as they are not clear what they mean from the
C BRUKL manual
      WRITE(IUF,'(a,f9.2)',iostat=ios)
     &  '  BUILDING-W/K               = ',build_UAN
      call eclose(EXP_AREA,0.0,0.0001,ISNEAR)
      if(.not.ISNEAR)then
        WRITE(IUF,'(a,f10.2)',iostat=ios)
     &  '  BUILDING-W/M2K             = ',build_UAN/EXP_AREA
      else
        WRITE(IUF,'(a)',iostat=ios)
     &  '  BUILDING-W/M2K             =  0.0'
      endif

C Using info from common of thermal bridges to find the percentage of heat loss
C due to thermal bridges out of the overall heat loss from the envelope.
C Thermal bridges are always 10% for notional building so b_alpha is
C always 10%.
      b_alpha=10.0
      WRITE(IUF,'(a,f6.2)',iostat=ios)   
     &  '  BUILDING-ALPHA             = ',b_alpha

C Calculate loads for heating, cooling, DHW etc.
      WRITE(IUF,'(a,f9.2)',iostat=ios)
     &  '  KWH/M2-HEAT                = ',ABS(AER_HEAT)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-COOL                = ',ABS(AER_COOL)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-AUX                 = ',AER_AUX
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-LIGHT               = ',AER_LIGHT
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-DHW                 = ',AER_DHW

C?? To enter total casual gains/m2 here
      TMPVAL=0.0  ! until there is something to write
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-EQUP                = ',TMPVAL
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-NATGAS              = ',AERF(1)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-LPG                 = ',AERF(2)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-BIOGAS              = ',AERF(3)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-OIL                 = ',AERF(4)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-COAL                = ',AERF(5)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-ANTHRACITE          = ',AERF(6)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-SMOKELESS           = ',AERF(7)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-DUELFUEL            = ',AERF(8)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-BIOMASS             = ',AERF(9)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-SUPELEC             = ',ABS(AERF(10))
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-WASTEHEAT           = ',AERF(12)

C?? To enter the following bits of information
      TMPVAL=0.0  ! until there is something to write
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-DISTRICT-HEATING    = ',TMPVAL
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-DISP                = ',TMPVAL
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-PVS                 = ',TMPVAL
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-WIND                = ',TMPVAL
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-CHP                 = ',TMPVAL
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-SES                 = ',TMPVAL

C Activity number and area per zone
C iactFlarea is a counter to prepare the act_area for the arlist call
C This act_area has to have maximum size 2 times the zone number, using
C ITWOMNS here because it is defined in sbem.h as two times the maximum
C number of activities
      iactFlarea=0
      do 19 iz=1,ncomp
        iactFlarea=iactFlarea+1
        act_area(iactFlarea)=real(theactivityindex(iz))
        iactFlarea=iactFlarea+1
        act_area(iactFlarea)= zbasea(iz)
 19   continue
      itrunc=1
      ipos=1
      delim='C'
      do while (itrunc.ne.0)
        call arlist(ipos,iactFlarea,act_area,ITWOMNS,delim,louts,
     &      loutln,itrunc)

C here we write each activity and the related floor area. SBEM seems to sum up the
C floor areas and writes the activity once if it covers many zones.
C BRUKL v3.2 seems to accept either ways.
        WRITE(IUF,'(4a)',iostat=ios) '  ACT-AREA                = ',
     &   '{ ',louts(1:loutln),' }'
        ipos=itrunc+1
      end do
      WRITE(IUF,'(a)',iostat=ios) '  ..'

C Section for each HVAC system for the notional building
      do 201 ICMSYS=1,NCMSYS
        hvacarea(ICMSYS)=0.
        IhvAct=0
        imerg=0
        do 225 iz=1,ncomp
          if(ICMSYS.eq.IHLZ(IZ))then
            IhvAct=IhvAct+1
            zhvacarea(ICMSYS,IhvAct)=0.
            hvacarea(ICMSYS)=zbasea(iz)+hvacarea(ICMSYS)
            zhvacarea(ICMSYS,IhvAct)= zbasea(iz)
            Act_HVAC(ICMSYS,IhvAct)=theactivityindex(iz)
          endif
 225    continue
        if((hvacarea(ICMSYS)).gt.0.)then
          TMPSTR2=HVACNAME(ICMSYS)

C Same as actual building
          WRITE(IUF,'(4a)',iostat=ios) '"',TMPSTR2(1:LNBLNK(TMPSTR2)),
     &     '"',' = HVAC-SYSTEM-DATA'

C Same as actual building
          WRITE(IUF,'(a,f8.2)',iostat=ios)
     &      '  AREA                      = ',hvacarea(ICMSYS)

C Same as actual building
          LNB=lnblnk(HSYSNAME(INCMSYS(ICMSYS)))
          WRITE(IUF,'(2a)',iostat=ios)
     &      '  TYPE                      = ',
     &      HSYSNAME(INCMSYS(ICMSYS))(1:LNB)

C Same as actual building
          LNB=lnblnk(TEMPZS(ICMSYS))
          WRITE(IUF,'(2a)',iostat=ios)
     &      '  HEAT-SOURCE               = ',
     &      TEMPZS(ICMSYS)(1:LNB)

C Notional building uses either gas or oil as worked out in BERTER and
C holds index number as NHF
          LNB=lnblnk(FUELNAME(NHF))
          WRITE(IUF,'(2a)',iostat=ios)
     &      '  FUEL-TYPE                 = ',FUELNAME(NHF)(1:LNB)
          WRITE(IUF,'(a)',iostat=ios)
     &      '  FUEL-TYPE-COOL            = Grid Supplied Electricity'
          AERSH_DEMAND(ICMSYS)=AERSH(ICMSYS)*HeatSCoP_N(ICMSYS)
          WRITE(IUF,'(a,F9.2)',iostat=ios)
     &      '  MJ/M2-HEAT-DEM            = ',ABS(AERSH_DEMAND(ICMSYS)*
     &      3.6)
          AERSC_DEMAND(ICMSYS)=AERSC(ICMSYS)*CoolSSEER_N(ICMSYS)
          WRITE(IUF,'(a,F9.2)',iostat=ios)
     &      '  MJ/M2-COOL-DEM            = ',
     &      ABS(AERSC_DEMAND(ICMSYS)*3.6)
          WRITE(IUF,'(a,F9.2)',iostat=ios)
     &      '  KWH/M2-HEAT               = ',ABS(AERSH(ICMSYS))
          WRITE(IUF,'(a,F9.2)',iostat=ios)
     &      '  KWH/M2-COOL               = ',ABS(AERSC(ICMSYS))
          WRITE(IUF,'(a,F9.2)',iostat=ios)
     &      '  KWH/M2-AUX                = ',AERSA(ICMSYS)

C This is based on what SBEM is using. Additional details
C can be found at NCM modelling guide vers. 2e Table6)
C However, SBEM v3.2b does not implement these values as this table
C in the modelling guide. It does not have for example a case where
C there is air-conditioning.
C If there is no ooling system:
          if(IBRUKLC(1,INCMSYS(ICMSYS)).eq.-1111)then

C then only heated space.
            WRITE(IUF,'(a,f6.2)',iostat=ios)
     &      '  HEAT-SSEFF                = ',HeatSCoP_N(ICMSYS)

C Treat all the other cases as heated and mechanically ventilated
          else
            WRITE(IUF,'(a,f6.2)',iostat=ios)
     &      '  HEAT-SSEFF                = ',HeatSCoP_N(ICMSYS)
            WRITE(IUF,'(a,f6.2)',iostat=ios)
     &      '  COOL-SSEER                = ',CoolSSEER_N(ICMSYS)
          endif

C Now start the logic needed for ACT-AREA entry
          itrunc=1
          ipos=1
          delim='C'
          if(IhvAct.gt.0)then

C Loop through the number of activities that this hvac covers (for their spaces)
C This will prepare TotHVACactArea for the arlist routine
C imerg is used for merging activities and floor areas in order to prepare the
C the array for the call to arlist
            do 197 iloop_IhvAct=1,IhvAct
              imerg=imerg+1
              TotHVACactArea(imerg)=
     &          real(Act_HVAC(ICMSYS,iloop_IhvAct))
              imerg=imerg+1
              TotHVACactArea(imerg)=
     &          zhvacarea(ICMSYS,iloop_IhvAct)
 197        continue
          endif
          do while (itrunc.ne.0)
            call arlist(ipos,imerg,TotHVACactArea,ITWOMNS,delim,louts,
     &          loutln,itrunc)
            WRITE(IUF,'(4a)',iostat=ios) ' ACT-AREA            = ',
     &       '{ ',louts(1:loutln),' }'
            ipos=itrunc+1
          end do
          WRITE(IUF,'(a)',iostat=ios) ' ..'
        endif
 201  continue

      call edisp(iuout,'Completed export of data to BRUKL inp')
      CALL ERPFREE(IUF,ISTAT)

C Create a batch file to run the BRUKL input file for Windows only
C Check if the machine is UNIX, If UNIX, do nothing
      call isunix(unixok)
      if(unixok)then
        continue

C If not UNIX, first determine the path of the current model folder
      else
        lcfgroot=cfgroot
        lpath=path
        lupath=upath
        limgpth=imgpth
        ldocpth=docpth
        ltmppth=tmppth
        ibrowse = 0

C Determine the path of current model
        call curproject(lcfgroot,lpath,lupath,limgpth,ldocpth,ltmppth,
     &    ibrowse)

        fs = char(92)
        col = char(58)

C Write batch file (also add command to copy BRKL.log from
C C:\Esru\esp-r\bin to model cfg folder)
        ILEN=LNBLNK(LASBEM)
        WRITE(BATCHFL,'(2A)')LASBEM(1:ILEN-4),'.bat'
        IUF=IFIL+1
        CALL EFOPSEQ(IUF,BATCHFL,3,IER)
        if(ier.eq.0)THEN
          write(currentfile,'(a)') BATCHFL(1:lnblnk(BATCHFL))
        ELSE ! WARNING MESSAGE HERE
	ENDIF
        WRITE(IUF,'(8A)',IOSTAT=IOS,ERR=3) 'cd C',col,fs,'Esru',fs,
     &    'esp-r',fs,'bin'
        WRITE(IUF,'(3A)',IOSTAT=IOS,ERR=3) 'BRUKL.exe /F ',
     &    lpath(1:lnblnk(lpath)),BRUKFL(1:lnblnk(BRUKFL))
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3) 'copy BRUKL.log ',
     &    lpath(1:lnblnk(lpath))

        CALL ERPFREE(IUF,ISTAT)

C Run batch file to call BRUKL.exe
        write (doit,'(a)') BATCHFL
        call runit(doit,'-')
      endif
      RETURN
 3    CONTINUE ! error message to go here
      END

************************* EPCCAL *****************************
C Subroutine EPCCAL generates the UK Energy Performance input file.
      SUBROUTINE EPCCAL(IER)
      IMPLICIT NONE

#include "sbem.h"
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "espriou.h"
      integer lnblnk  ! function definition
      integer ier

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

      integer IFIL
      common/FILEP/IFIL

      integer ncomp,ncon
      common/C1/NCOMP,NCON

      CHARACTER CFGROOT*24,LCFGF*72,MODE*4
      integer ifcfg
      common/C21/IFCFG,cfgroot,LCFGF

      common/rpath/path
      character DOIT*248
      character limgpth*24,ldocpth*24,ltmppth*24,lcfgroot*24,lpath*72
      character path*72,lupath*72
      character fs*1, col*1, quote*1
      CHARACTER EPCBATFL*72,EPCINIFL*72
      integer mpof, ibrowse
      LOGICAL UNIXOK

      CHARACTER EPCFL*72
      CHARACTER TMPSTR2*70
      character delim*1,louts*248
      LOGICAL ISNEAR,ISNEAR2,ISNEAR3
      integer iactFlarea
      real act_area !for the ACT-AREA entry of EPC file: lists sequentialy
                    !activity numbers and floor areas
      dimension act_area(ITWOMNS)
      dimension hvacarea(MHT) !area serviced by the same hvac system - MHT is from sbem.h
      REAL hvacarea
      integer Act_HVAC !temporary passing activity index to hvac
      dimension Act_HVAC(MHT,MCOM),TotHVACactArea(ITWOMNS)
      real TotHVACactArea
      integer IhvAct,imerg,iloop_IhvAct
      real zhvacarea !temporary passing activity index to hvac)
      dimension zhvacarea(MHT,MCOM)
      real b_alpha
      integer IcurrentDHW
      integer ILEN,LNB
      integer iuf
      integer icomp,iz       ! for zone loops
      integer ICMSYS

C Build variable (These should eventually be placed in build_info.h)
C      character cBuilder*32          ! User who compiled system
C      character cArchitecture*32     ! Machine archictecture
C      character cOperSystem*32       ! Operating system
C      character cOSversion*32        ! OS flavour/version
C      character cHost*256            ! Host name
C      logical   bVersioned           ! Flag indicating if repository
C                                     !   is versioned
C      logical bRelease               ! Logical indicating if this is
C                                     !   a release version.
      logical bBuild_Info            ! Flag indicating if build info should
                                     !   be dumped.
C      logical bModified              ! Flag indicating local copy contains
C                                     !   modifications
C      character cRelease_Num*8       ! Release version
C      character cBase*8              ! Base version
C      character cURL*256             ! Repository URL commented out in build_info.h
C      character cSource*32           ! Repository version
C      character cBranch*48           ! Branch name
C      character cRevision*32         ! Revision number
C      character cState*32            ! Flag indicating repository state.
C      character cCC*32               ! Flags for compilers
C      character cFC*32               !
C      character cCPL*32              !
C      character cXML_support*32      ! Flags for compile-time options
C      character cXlibrary*32         !

      real H3_impact !impact of the application of recommendation H3
      real H1_impact !impact of the application of recommendation H1
      real H5_impact !impact of the application of recommendation H5
      real H2_impact !impact of the application of recommendation H2
      real H6_impact !impact of the application of recommendation H6
      real H7_impact !impact of the application of recommendation H7
      real H8_impact !impact of the application of recommendation H8
      real C1_impact !impact of the application of recommendation C1
      real C3_impact !impact of the application of recommendation C3
      real F1_impact !impact of the application of recommendation F1
      real F2_impact !impact of the application of recommendation F2
      real F3_impact !impact of the application of recommendation F3
      real F4_impact !impact of the application of recommendation F4
      real F5_impact !impact of the application of recommendation F5
      real F6_impact !impact of the application of recommendation F6
      real W1_impact !impact of the application of recommendation W1
      real W2_impact !impact of the application of recommendation W2
      real L1_impact !impact of the application of recommendation L1
      real L2_impact !impact of the application of recommendation L2
      real L3_impact !impact of the application of recommendation L3
      real L4_impact !impact of the application of recommendation L4
      real L5_impact !impact of the application of recommendation L5
      real L7_impact !impact of the application of recommendation L7

      REAL TyERSH_DEMAND ! As TyERSH * heat generator efficiency
      REAL TyERSC_DEMAND ! As TyERSC * cool generator efficiency
      REAL RERSH_DEMAND ! As RERSH * heat generator efficiency
      REAL RERSC_DEMAND ! As RERSC * cool generator efficiency
      REAL TMPVAL
      dimension TyERSH_DEMAND(MCOM),TyERSC_DEMAND(MCOM)
      dimension RERSH_DEMAND(MCOM),RERSC_DEMAND(MCOM)
      integer itrunc,ipos,loutln
      integer istat
      integer ios
      real vexposed,fexposed,rexposed
      integer IAPROB
      integer itrc
      logical ExistLightType
      real fPercentHeating,fPercentCooling,fPercentLighting
      real fPercentAuxiliary,fPercentDhw
      logical coolxst !check if cooling generator exists
      logical dhwxst !check if stand alone dhw system exists
      logical heatingxst !check if a heating system exists
      integer idhw !to loop for dhw systems
      real fSumDhwEfficiencies,fAverageDhwEfficiency !sum dhw efficiencies and find the average
      real fSumHeatingGenEfficiencies,fAverageHeatingGenEfficiency !sum heating generator
                                                                  !efficiencies and find the average
      real fSumCoolingGenEfficiencies,fAverageCoolingGenEfficiency !sum cooling generator
                                                                  !efficiencies and find the average
      real fMaxZoneOverheating !the maximum percentage overheating (between zones)
      real fBerMainFuel !temporary variable to identify the main fuel based on the maximum BER
      character cNameMainFuel*42 !string to define the name of the main fuel type
      integer IHF !index for fuel number (as used in previous routines.
      integer ifueloriginal !to hold temporarily the fuel type index number
      INTEGER IMODE

#include "build_info.h"

C Initialise IMODE
      IMODE=2

C Read cfg file for the stripped model
      ifcfg=ifil+3
      iaprob=ifil+4
      mode='ALL '
      itrc=4 ! To skip warning message about stripped model
      call ersys(lcfgf_s,ifcfg,iaprob,mode,itrc,ier)

C Read *.ncm file and SBEM database
      CALL RSBEM

C Start writing EPC file
C Building, owner, certifier detail...
      ILEN=LNBLNK(LASBEM)
      IF(LASBEM(ILEN-8:ILEN).EQ.'_str.ncm')THEN
        WRITE(EPCFL,'(2A)')LASBEM(1:ILEN-8),'_epc.inp'
      ELSE
        WRITE(EPCFL,'(2a)')CFGROOT(1:LNBLNK(CFGROOT)),'_epc.inp'
      ENDIF
      IUF=IFIL+1
      CALL EFOPSEQ(IUF,EPCFL,3,IER)
      if(ier.eq.0)THEN
        write(currentfile,'(a)') EPCFL(1:lnblnk(EPCFL))
      ELSE ! WARNING MESSAGE HERE
      ENDIF
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3) '$'
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &  '$ Compliance input file for BR PART L 2006'
      WRITE(IUF,'(a)',iostat=ios)  '$ ESP-r -> EPCgen '
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3) '$'
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3) '"EPC-PROJECT" = GENERAL'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' P-NAME              = ',
     &  '"',pjname(1:lnblnk(pjname)),' "'

C UPRN stands for the Unique Property Reference Number (UPRN) of the building
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' UPRN            = ','"',
     &  UPRN(1:lnblnk(UPRN)),'"'

C B-INSP-DATE specifies the inspection date of the building by the Energy Assessor
      WRITE(IUF,'(2A,I4,A,I2,A,I2,A)',IOSTAT=IOS,ERR=3)
     &  ' B-INSP-DATE       = ',' {',Y_inspect,',',M_inspect,',',
     &  D_inspect,'}'

      IF(IBUSERTYP.NE.0)THEN
        TMPSTR2=BTYPNAME(IBUSERTYP)
      ELSE
        TMPSTR2='Building type not defined (yet)'
      ENDIF
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)' B-TYPE              = ',
     &  TMPSTR2(1:LNBLNK(TMPSTR2))

C Specifies one line of the building address
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' B-ADDRESS-0         = ',
     &  '"',BADDRESS(1:lnblnk(BADDRESS)),' "'

C Specifies a fourth additional line of the building address
C We will skip ADDRESS-1,2,3 here and we will assume that City is
C is described in this line (this is fully acceptable by EPCgen program)
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' B-ADDRESS-4         = ',
     &  '"',BCITY(1:lnblnk(BCITY)),' "'

C Specifies a fifth additional line of the building address
C We will assume here that post-code is described in this line
C (this is fully acceptable by EPCgen program)
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' B-ADDRESS-5         = ',
     &  '"',BUILDINGPC(1:lnblnk(BUILDINGPC)),' "'

C << B-LOC-DESCRIPTION may be an additional output in the future >>

C Specifies the total floor area of the building in m^2
      WRITE(IUF,'(a,f8.2)',IOSTAT=IOS,ERR=3)' BUILDING-AREA       = ',
     &   tot_floor_area

      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' O-NAME              = ',
     &  '"',ownername(1:lnblnk(ownername)),' "'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' O-TELEPHONE         = ',
     &  '"',ownertel(1:lnblnk(ownertel)),' "'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' O-ADDRESS           = ',
     &  '"',owneraddr(1:lnblnk(owneraddr)),' "'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' O-CITY              = ',
     &  '"',ownercity(1:lnblnk(ownercity)),' "'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' O-POSTCODE          = ',
     &  '"',ownerPC(1:lnblnk(ownerPC)),' "'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' C-NAME              = ',
     &  '"',certifname(1:lnblnk(certifname)),' "'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' C-TELEPHONE         = ',
     &  '"',certTel(1:lnblnk(certTel)),' "'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' C-ADDRESS           = ',
     &  '"',certaddress(1:lnblnk(certaddress)),' "'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' C-CITY              = ',
     &  '"',certcity(1:lnblnk(certcity)),' "'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' C-POSTCODE          = ',
     &  '"',certPC(1:lnblnk(certPC)),' "'

C Accreditation Scheme (E,W and NI only)
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' C-ACCRED-SCHEME     = ',
     &  '"',Accr_Scheme(1:lnblnk(Accr_Scheme)),' "'

C Assessor registration number (E,W and NI only)
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' C-REG-NUMBER        = ',
     &  '"',assessRegNumber(1:lnblnk(assessRegNumber)),'"'

C Employer/trading name of energy assessor
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' C-EMP-TRAD-NAME     = ',
     &  '"',empl_Trading_name(1:lnblnk(empl_Trading_name)),'"'

C Employer/trading address of energy assessor
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' C-EMP-TRAD-ADDRESS  = ',
     &  '"',addr_empl_Trading(1:lnblnk(addr_empl_Trading)),'"'

C Related party disclosure
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' C-REL-PART-DISC    = ',
     &  '"',party_disclosure(1:lnblnk(party_disclosure)),'"'

C Qualifications of energy assessor
C Available choices are 3: NOS3 , NOS4 , NOS5
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)' C-QUALIFICATIONS   = ',
     &  qualifications_assessor(1:lnblnk(qualifications_assessor))

C Insurer company
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' C-INSURER       = ',
     &  '"',insurer_Company(1:lnblnk(insurer_Company)),'"'

C Insurance policy number
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' C-INS-POL-NUMBER = ',
     &  '"',policyNumberInsurance(1:lnblnk(policyNumberInsurance)),'"'

C Insurance policy start date
      WRITE(IUF,'(2A,I4,A,I2,A,I2,A)',IOSTAT=IOS,ERR=3)
     & ' C-INS-EFF-DATE  = ','"',S_Yinsur,'-',S_Minsur,'-',S_Dinsur,'"'

C Insurance policy expiry date
      WRITE(IUF,'(2A,I4,A,I2,A,I2,A)',IOSTAT=IOS,ERR=3)
     & ' C-INS-EXP-DATE  = ','"',E_Yinsur,'-',E_Minsur,'-',E_Dinsur,'"'

C Insurance policy cover limit
      WRITE(IUF,'(2A,I9,A)',IOSTAT=IOS,ERR=3)' C-INS-PI-LIMIT  = ',
     &  '"',pi_limit,'"'

C Calculation engine and interface detail...
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' CENGINE             = ',
     &  '"','Not Approved','"'

C For trying the export, change ESP-r to "Not Approved"
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)' CENGINE-VAL         = ',
     &  'Not Approved'
      WRITE(IUF,'(6A)',IOSTAT=IOS,ERR=3)' CENGINE-VERSION     = ',
     &  cBranch(1:lnblnk(cBranch)),'@',
     &  cRelease_num(1:lnblnk(cRelease_num)),' ',
     &  cState(1:lnblnk(cState))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)' INTERFACE           = ',
     &  '"ESP-r Project Manager"'
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)' INTERFACE-VERSION   = ',
     &  cRelease_num(1:lnblnk(cRelease_num))

C This can be left as it is for now. EPC manual says for this entry:
C "Specifies the path location of the input file to the interface software"
C Examples give the nct file path in SBEM (i.e. input for interface isbem)
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' PATH-FILE-INTERFACE = ',
     &  '"',' hardwired again ',' "'

C This can be left as it is for now. EPC manual says for this entry:
C "Specifies the path location of the input file to the calculation engine"
C Examples give the inp file path in SBEM (i.e. input for engine sbem)
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' PATH-FILE-ENGINE  = ',
     &  '"',' hardwired again ',' "'

C There is an option for "NO" here but there is no point incuding it
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)' PAUSE-KNOWN-ERRORS = ',
     &  ' YES'
      WRITE(IUF,'(a)',iostat=ios) ' ..'
      WRITE(IUF,'(2a)',iostat=ios) '"ESP-r" = ',' EPC'
      if(BLDREG(IRGG)(1:7).eq.'England')then
        WRITE(IUF,'(2a)',iostat=ios) ' TYPE = ',
     &  'England and Wales'
      elseif(BLDREG(IRGG)(1:8).eq.'Scottish')then
        WRITE(IUF,'(2a)',iostat=ios) ' TYPE = ',
     &    'Scottish'
      elseif(BLDREG(IRGG)(1:8).eq.'Northern')then
        WRITE(IUF,'(2a)',iostat=ios) ' TYPE = ',
     &    'Northern Ireland'
      endif

C SER is defined as the Standard CO2 emission rate in kg/m2year.
C This is calculated according to the NCM modelling guide
C paragraph 52 vers. 4d: "The Standard Emission Rate is determined by
C applying a fixed improvement factor to the emissions from a reference building"
      WRITE(IUF,'(a,f7.1)',iostat=ios) ' SER            = ',SER

C CO2 emission rate in kg/m2year for the "typical" building
      WRITE(IUF,'(a,f7.1)',iostat=ios) ' TYR            = ',TyER

C << Need interface to give the option here between English and Welsh >>
C << Default is english >>
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)' EPC-LANGUAGE       = ',
     &  ' ENGLISH'
      WRITE(IUF,'(a,1X,F7.1)',iostat=ios)' BER                 = ',
     &  ABS(BER)
      WRITE(IUF,'(a,1X,F7.1)',iostat=ios)' TER                 = ',
     &  ABS(TER)

C NOS-LEVEL depends on the complexity of the project
C Options: Undefined, Level 3, Level 4 and Level 5.
C Default is Undefined
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)' NOS-LEVEL       = ',
     &  pjLevComplexity(1:lnblnk(pjLevComplexity))

C << Some input files seem to also have an "EA-KEY" entry >>
C << but there is nowhere info about this >>
C << This is left commented for future use >>
C      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)' EA-KEY           = ',
C     &  ' ""'

C Loop for the systems and write the
C fuel for the system that covers the largest area.
      fBerMainFuel=0.
      do 161 IHF=1,MFT
        if(BERF(IHF).gt.fBerMainFuel)then
          fBerMainFuel=BERF(IHF)
          cNameMainFuel=FUELNAME(IHF)(1:lnblnk(FUELNAME(IHF)))
        endif
 161  continue
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
     &   ' MAIN-FUEL-TYPE    = ',cNameMainFuel(1:lnblnk(cNameMainFuel))

C Building services for the actual building.
C This seems to be a global value but there is no explanation
C on how to determine it (currently depends on the type of
C system per zone. Probably, it should be based on zone areas
C << TO BE DONE -hardwired to Heating and Natural Ventilation >>
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)' BUILDING-ENVIRONMENT = ',
     &  ' Heating and Natural Ventilation'

      WRITE(IUF,'(a)',iostat=ios) ' ..'

C The object "EPC-REC-PROJECT" does not seem to be compulsory
C for detailed simulation programs but without it there is no
C way to generate the recommendations report
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)'"EPC-REC-PROJECT" = REC-PROJECT'
      dhwxst=.false.
      heatingxst=.false.
      do 99 ICOMP=1,NCOMP
        if(IDHWLZ(ICOMP).gt.0)then
          dhwxst=.true.
        endif
        if(IHLZ(ICOMP).gt.0)then
        if(INCMSYS(IHLZ(ICOMP)).gt.0)then !check also if there is a heating system
          heatingxst=.true.
        endif
        endif
 99   continue

C If DHW is provided by the heating system then PERF-DHW-ENERGY is
C NOT APPLICABLE
      if(.not.dhwxst)then
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)' PERF-DHW-ENERGY       = ',
     &    'NOT APPLICABLE'
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)' PERF-DHW-CO2       = ',
     &    'NOT APPLICABLE'
      elseif(dhwxst)then
        fSumDhwEfficiencies=0.
        do 88 idhw=1,NDHWSYS
          fSumDhwEfficiencies=HWEF(idhw)+fSumDhwEfficiencies
 88     continue
        fAverageDhwEfficiency=fSumDhwEfficiencies/real(NDHWSYS)
        call eclose(fAverageDhwEfficiency,0.70,0.0001,ISNEAR)
        if(ISNEAR.or.fAverageDhwEfficiency.lt.0.70)then
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-DHW-ENERGY       = POOR'
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-DHW-CO2          = POOR'
        elseif(fAverageDhwEfficiency.gt.0.79)then
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-DHW-ENERGY       = GOOD'
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-DHW-CO2          = GOOD'
        else
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-DHW-ENERGY       = FAIR'
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-DHW-CO2          = FAIR'
        endif
      endif

C The logic should be applied for CO2 too but the documentation refers
C only to energy. Also, CO2 detailed outputs are not available for the
C moment. For these reasons the same logic will be applied for CO2 as the
C logic for energy (see EPC dictionary and filtering recommendations
C documents). iSBEM seems to be following the same logic.
      call eclose(BER_DHW,AER_DHW,0.001,ISNEAR)
      if(BER_DHW.lt.AER_DHW)then
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &    ' PERF-DHW-SYSTEM-ENERGY       = GOOD'
      elseif((isnear.or.BER_DHW.gt.AER_DHW).and.BER_DHW.lt.TyER_DHW)
     &  then
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &    ' PERF-DHW-SYSTEM-ENERGY       = FAIR'
      else
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &    ' PERF-DHW-SYSTEM-ENERGY       = POOR'
      endif
      call eclose(BER_DHWCO2,AER_DHWCO2,0.001,ISNEAR)
      if(BER_DHWCO2.lt.AER_DHWCO2)then
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &    ' PERF-DHW-SYSTEM-CO2         = GOOD'
      elseif((isnear.or.BER_DHWCO2.gt.AER_DHWCO2).and.
     &        BER_DHWCO2.lt.TyER_DHWCO2)then
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &    ' PERF-DHW-SYSTEM-CO2         = FAIR'
      else
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &    ' PERF-DHW-SYSTEM-CO2         = POOR'
      endif
      if(.not.dhwxst)then
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)' DHW-PERCENT-CO2      = ',
     &    '0'
      elseif(dhwxst)then

C iSBEM does not yet consider renewables in the percentage calculations
        fPercentDhw=
     &   (BER_DHWCO2/(BER_HEATCO2+BER_COOLCO2+BER_DHWCO2+
     &    BER_LIGHTCO2+BER_AUXCO2))*100
        WRITE(IUF,'(a,f7.2)',IOSTAT=IOS,ERR=3)
     &    ' DHW-PERCENT-CO2      = ',fPercentDhw
      endif

C Section for heating
      if(.not.heatingxst)then
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)' PERF-HEAT-ENERGY       = ',
     &  'NOT APPLICABLE'
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)' PERF-HEAT-CO2       = ',
     &    'NOT APPLICABLE'
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &    ' PERF-HEAT-SYSTEM-ENERGY       = NOT APPLICABLE'
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &    ' PERF-HEAT-SYSTEM-CO2       = NOT APPLICABLE'
      else
        fSumHeatingGenEfficiencies=0
        do 91 icmsys=1,ncmsys
          fSumHeatingGenEfficiencies=
     &      HGEF(ICMSYS)+fSumHeatingGenEfficiencies
 91     continue
        fAverageHeatingGenEfficiency=
     &    fSumHeatingGenEfficiencies/real(ncmsys)
        call eclose(fAverageHeatingGenEfficiency,0.70,0.0001,ISNEAR)
        if(ISNEAR.or.fAverageHeatingGenEfficiency.lt.0.70)then
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-HEAT-ENERGY       = POOR'
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-HEAT-CO2          = POOR'
         elseif(fAverageHeatingGenEfficiency.gt.0.88)then
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-HEAT-ENERGY       = GOOD'
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-HEAT-CO2          = GOOD'
         else
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-HEAT-ENERGY       = FAIR'
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-HEAT-CO2          = FAIR'
        endif
        call eclose(BER_HEAT,AER_HEAT,0.001,ISNEAR)
        if(BER_HEAT.lt.AER_HEAT)then
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-HEAT-SYSTEM-ENERGY       = GOOD'
        elseif((isnear.or.BER_HEAT.gt.AER_HEAT).and.
     &          BER_HEAT.lt.TyER_HEAT)then
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-HEAT-SYSTEM-ENERGY       = FAIR'
        else
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-HEAT-SYSTEM-ENERGY       = POOR'
        endif
        call eclose(BER_HEATCO2,AER_HEATCO2,0.001,ISNEAR)
        if(BER_HEATCO2.lt.AER_HEATCO2)then
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-HEAT-SYSTEM-CO2         = GOOD'
        elseif((isnear.or.BER_HEATCO2.gt.AER_HEATCO2).and.
     &          BER_HEATCO2.lt.TyER_HEATCO2)then
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-HEAT-SYSTEM-CO2         = FAIR'
        else
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-HEAT-SYSTEM-CO2         = POOR'
        endif
      endif
      fPercentHeating=
     &   (BER_HEATCO2/(BER_HEATCO2+BER_COOLCO2+BER_DHWCO2+
     &    BER_LIGHTCO2+BER_AUXCO2))*100
      WRITE(IUF,'(a,f7.2)',IOSTAT=IOS,ERR=3)
     &  ' HEAT-PERCENT-CO2      = ',fPercentHeating
      coolxst =.false.
      do 565 ICMSYS=1,NCMSYS
          if(IBRUKLC(1,INCMSYS(ICMSYS)).ne.-1111)then
            coolxst =.true.
          endif
 565  continue
      if(.not.coolxst)then
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)' PERF-COOL-ENERGY       = ',
     &    'NOT APPLICABLE'
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)' PERF-COOL-CO2       = ',
     &    'NOT APPLICABLE'
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
     &    ' PERF-COOL-SYSTEM-ENERGY   = ','NOT APPLICABLE'
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
     &    ' PERF-COOL-SYSTEM-CO2      = ','NOT APPLICABLE'
        WRITE(IUF,'(a)',IOSTAT=IOS,ERR=3)
     &   ' COOL-PERCENT-CO2      = 0'
      else
        fSumCoolingGenEfficiencies=0
        do 691 icmsys=1,ncmsys
          fSumCoolingGenEfficiencies=
     &      CGEF(ICMSYS)+fSumCoolingGenEfficiencies
 691     continue
        fAverageCoolingGenEfficiency=
     &    fSumCoolingGenEfficiencies/real(ncmsys)
        call eclose(fAverageCoolingGenEfficiency,2.0,0.0001,ISNEAR)

C The guide does not cover the case where fAverageCoolingGenEfficiency=2.0
C Assuming poor in this case
        if(fAverageCoolingGenEfficiency.lt.2.0.or.isnear)then
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-COOL-ENERGY       = POOR'
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-COOL-CO2          = POOR'
        elseif(fAverageCoolingGenEfficiency.gt.2.4)then
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-COOL-ENERGY       = GOOD'
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-COOL-CO2          = GOOD'
        else
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-COOL-ENERGY       = FAIR'
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-COOL-CO2          = FAIR'
        endif
        call eclose(BER_COOL,AER_COOL,0.001,ISNEAR)
        if(BER_COOL.lt.AER_COOL)then
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-COOL-SYSTEM-ENERGY       = GOOD'
        elseif((isnear.or.BER_COOL.gt.AER_COOL).and.
     &    BER_COOL.lt.TyER_COOL)then
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-COOL-SYSTEM-ENERGY       = FAIR'
        else
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-COOL-SYSTEM-ENERGY       = POOR'
        endif
        call eclose(BER_COOLCO2,AER_COOLCO2,0.001,ISNEAR)
        if(BER_COOLCO2.lt.AER_COOLCO2)then
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-COOL-SYSTEM-CO2          = GOOD'
        elseif((isnear.or.BER_COOLCO2.gt.AER_COOLCO2).and.
     &    BER_COOLCO2.lt.TyER_COOLCO2)then
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-COOL-SYSTEM-CO2          = FAIR'
        else
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' PERF-COOL-SYSTEM-CO2          = POOR'
        endif
        fPercentCooling=
     &    (BER_COOLCO2/(BER_HEATCO2+BER_COOLCO2+BER_DHWCO2+
     &     BER_LIGHTCO2+BER_AUXCO2))*100
        WRITE(IUF,'(a,f7.2)',IOSTAT=IOS,ERR=3)
     &    ' COOL-PERCENT-CO2      = ',fPercentCooling
      endif

C Always applicable for lights
      call eclose(BER_LIGHT,AER_LIGHT,0.001,ISNEAR)
      if(BER_LIGHT.lt.AER_LIGHT)then
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &    ' PERF-LIGHT-SYSTEM-ENERGY       = GOOD'
      elseif((isnear.or.BER_LIGHT.gt.AER_LIGHT).and.
     &       BER_LIGHT.lt.TyER_LIGHT)then
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &    ' PERF-LIGHT-SYSTEM-ENERGY       = FAIR'
      else
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &    ' PERF-LIGHT-SYSTEM-ENERGY       = POOR'
      endif
      call eclose(BER_LIGHTCO2,AER_LIGHTCO2,0.001,ISNEAR)
      if(BER_LIGHTCO2.lt.AER_LIGHTCO2)then
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &    ' PERF-LIGHT-SYSTEM-CO2          = GOOD'
      elseif((isnear.or.BER_LIGHTCO2.gt.AER_LIGHTCO2).and.
     &       BER_LIGHTCO2.lt.TyER_LIGHTCO2)then
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &    ' PERF-LIGHT-SYSTEM-CO2          = FAIR'
      else
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &    ' PERF-LIGHT-SYSTEM-CO2          = POOR'
      endif
      fPercentLighting=
     &   (BER_LIGHTCO2/(BER_HEATCO2+BER_COOLCO2+BER_DHWCO2+
     &    BER_LIGHTCO2+BER_AUXCO2))*100
      WRITE(IUF,'(a,f7.2)',IOSTAT=IOS,ERR=3)
     &  ' LIGHT-PERCENT-CO2      = ',fPercentLighting

C Always applicable for Auxiliary output
      call eclose(BER_AUX,AER_AUX,0.001,ISNEAR)
      if(BER_AUX.lt.AER_AUX)then
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &    ' PERF-AUX-SYSTEM-ENERGY       = GOOD'
      elseif((isnear.or.BER_AUX.gt.AER_AUX).and.
     &       BER_AUX.lt.TyER_AUX)then
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &    ' PERF-AUX-SYSTEM-ENERGY       = FAIR'
      else
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &    ' PERF-AUX-SYSTEM-ENERGY       = POOR'
      endif
      call eclose(BER_AUXCO2,AER_AUXCO2,0.001,ISNEAR)
      if(BER_AUXCO2.lt.AER_AUXCO2)then
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &    ' PERF-AUX-SYSTEM-CO2          = GOOD'
      elseif((isnear.or.BER_AUXCO2.gt.AER_AUXCO2).and.
     &       BER_AUXCO2.lt.TyER_AUXCO2)then
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &    ' PERF-AUX-SYSTEM-CO2          = FAIR'
      else
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &    ' PERF-AUX-SYSTEM-CO2          = POOR'
      endif
      fPercentAuxiliary=
     &   (BER_AUXCO2/(BER_HEATCO2+BER_COOLCO2+BER_DHWCO2+
     &    BER_LIGHTCO2+BER_AUXCO2))*100
      WRITE(IUF,'(a,f7.2)',IOSTAT=IOS,ERR=3)
     &  ' AUX-PERCENT-CO2      = ',fPercentAuxiliary

C Rules for overheating are based on NCM modelling Guide v4d.
C EPCgen seems to report one output for this and not for each zone
C The current implementation in ESP-r will report the worse case
C option for the zone that is more likely to overheat (this seems
C to be different from what iSBEM is doing)
      fMaxZoneOverheating=0.
      do 30 iz=1, ncomp
        if(prcnt_Overheat(iz).gt.fMaxZoneOverheating)then
          fMaxZoneOverheating=prcnt_Overheat(iz)
        endif
 30   continue
      call eclose(fMaxZoneOverheating,0.5,0.0001,ISNEAR)
      call eclose(fMaxZoneOverheating,1.0,0.0001,ISNEAR2)
      call eclose(fMaxZoneOverheating,1.5,0.0001,ISNEAR3)
      if(fMaxZoneOverheating.lt.0.5)then
        WRITE(IUF,'(2a)',iostat=ios)
     &    ' OVERHEATING-RISK   = ','Low risk'
      elseif(ISNEAR.or.(fMaxZoneOverheating.gt.0.5.and.
     &       fMaxZoneOverheating.lt.1.0))then
        WRITE(IUF,'(2a)',iostat=ios)
     &    ' OVERHEATING-RISK   = ','Moderate risk'
      elseif(ISNEAR2.or.(fMaxZoneOverheating.gt.1.0.and.
     &       fMaxZoneOverheating.lt.1.5))then
        WRITE(IUF,'(2a)',iostat=ios)
     &    ' OVERHEATING-RISK   = ','Significant risk'
      elseif(ISNEAR3.or.fMaxZoneOverheating.gt.1.5)then
        WRITE(IUF,'(2a)',iostat=ios)
     &    ' OVERHEATING-RISK   = ','High risk'
      endif

C This keyword specifies whether the building has been identified as being:
C one of special architectural or historical interest, in a conservation area,
C in a designated area of special character or appearance, or of traditional
C construction. The value can be YES or NO. Hardwired to NO
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)' ENG-HERITAGE     = ',
     &  'NO'
      WRITE(IUF,'(a)',iostat=ios) ' ..'

C << CHECK IF LOOP IS CORRECT
      do 607 ICMSYS=1,NCMSYS

C REC-EPC-H2: Triggered when the heating system does not have centralised time control
C This is true when iRecControls(1,ICMSYS).ne.1
        if(iRecControls(1,ICMSYS).ne.1)then
          WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-H2" = RECOMMENDATION'
          WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = HEATING'
          WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-H2'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
          WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Calculation for ENERGY-IMPACT is optional for detailed simulation programs
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT  = UNKNOWN'

C Need a new BER calculation here for an improvement of 1% on the heating
C efficiency and calculate total BER CO2 emissions after this change.
C Temporary change the efficiency to HGEF(ICMSYS)+1.%
          EFFORIGINAL(ICMSYS)=HGEF(ICMSYS)
          BERORIGINAL=BER
          HGEF(ICMSYS)= HGEF(ICMSYS)+0.01

C Call BERTER to recalculate BER using the new efficiency
          CALL BERTER(IER)

C Then determine the % change from this in TOTAL building emissions
C BER is now a new recalculated BER after changing HGEF(ICMSYS)
          H2_impact=((BERORIGINAL-BER)/BERORIGINAL)*100

C Return the efficiency and BER back to their original values
          HGEF(ICMSYS)=EFFORIGINAL(ICMSYS)
          BER=BERORIGINAL
          CALL BERTER(IER)

C If %change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low".
          CALL ECLOSE(H2_impact,4.0,0.001,ISNEAR)
          IF(H2_impact.GT.4.0)THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
          ELSEIF((H2_impact.LT.4.0.OR.ISNEAR).AND.H2_impact.GT.0.5)
     &       THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
          ELSE
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
          ENDIF

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
          WRITE(IUF,'(a)',iostat=ios) ' ..'

C REC-HVAC-EPC-H2: Triggered when the heating system does not have
C centralised time control.
          WRITE(IUF,'(a)',iostat=ios)
     &                  '"REC-HVAC-EPC-H2" = RECOMMENDATION'
          WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = HEATING'
          WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-H2'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
          WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Calculation for ENERGY-IMPACT is optional for detailed simulation programs
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT  = UNKNOWN'

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low".
          CALL ECLOSE(H2_impact,4.0,0.001,ISNEAR)
          IF(H2_impact.GT.4.0)THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
          ELSEIF((H2_impact.LT.4.0.OR.ISNEAR).AND.H2_impact.GT.0.5)
     &       THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
          ELSE
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
          ENDIF

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

          WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT-SC = ','CALC'

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'

C Next line is only for specific recommendations.
          WRITE(IUF,'(a)',iostat=ios) 'REF-OBJECT-TYPE = HVAC-SYSTEM'

C Next line is only for specific recommendations (no where documented).
C Need the name of the HVAC system here, which uses the specific fuel
          WRITE(IUF,'(2a)',iostat=ios) 'REF-OBJECT     = ',
     &          HVACNAME(ICMSYS)
          WRITE(IUF,'(a)',iostat=ios) ' ..'
        endif

C REC-EPC-H1: Consider replacing heating boiler plant with high efficiency type
C If heat generator efficiency <=0.70 then trigger this recommendation
        call eclose(HGEF(ICMSYS),0.70,0.001,ISNEAR)
        if(ISNEAR.or.HGEF(ICMSYS).lt.0.70)then
          WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-H1" = RECOMMENDATION'
          WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = HEATING'
          WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-H1'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
          WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C << Next line could be included with additions on the interface
C << It is optional for SBEM
C         WRITE(IUF,'(a)',iostat=ios)
C      &    'REC-COMMENT  = "Assesor may provide additional comments"'

          WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ',
     &      'UNKNOWN'

C Temporary change the efficiency to 0.81
          EFFORIGINAL(ICMSYS)=HGEF(ICMSYS)
          BERORIGINAL=BER
          HGEF(ICMSYS)=0.81

C Call BERTER to recalculate BER using the new efficiency
          CALL BERTER(IER)

C Then determine the % change from this in TOTAL building emissions
          H1_impact=((BERORIGINAL-BER)/BERORIGINAL)*100

C Return the efficiency back to it's original value
          HGEF(ICMSYS)=EFFORIGINAL(ICMSYS)
          BER=BERORIGINAL
          CALL BERTER(IER)

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low"
          CALL ECLOSE(H1_impact,4.0,0.001,ISNEAR)
          IF(H1_impact.GT.4.0)THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
          ELSEIF((H1_impact.LT.4.0.OR.ISNEAR).AND.H1_impact.GT.0.5)
     &       THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
          ELSE
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
          ENDIF

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
          WRITE(IUF,'(a)',iostat=ios) ' ..'
        endif

C H1 needs to be written twice - the second time it should include the REF-OBJECT
C and be REC-HVAC- EPC-H1: Consider replacing heating boiler plant with high
C efficiency type. If heat generator efficiency <=0.70 then trigger this
C recommendation
        call eclose(HGEF(ICMSYS),0.70,0.001,ISNEAR)
        if(ISNEAR.or.HGEF(ICMSYS).lt.0.70)then
          WRITE(IUF,'(a)',iostat=ios)
     &                    '"REC-HVAC-EPC-H1" = RECOMMENDATION'
          WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = HEATING'
          WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-H1'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
          WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C << Next line could be included with additions on the interface
C << It is optional for SBEM
C         WRITE(IUF,'(a)',iostat=ios)
C      &    'REC-COMMENT  = "Assesor may provide additional comments"'

          WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ',
     &    'UNKNOWN'

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low"
          CALL ECLOSE(H1_impact,4.0,0.001,ISNEAR)
          IF(H1_impact.GT.4.0)THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
          ELSEIF((H1_impact.LT.4.0.OR.ISNEAR).AND.H1_impact.GT.0.5)
     &       THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
          ELSE
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
          ENDIF

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'

C Next line is only for specific recommendations.
          WRITE(IUF,'(a)',iostat=ios) 'REF-OBJECT-TYPE = HVAC-SYSTEM'

C Next line is only for specific recommendations (no where documented).
C Need the name of the HVAC system here, which uses the specific fuel
          WRITE(IUF,'(2a)',iostat=ios) 'REF-OBJECT     = ',
     &          HVACNAME(ICMSYS)
          WRITE(IUF,'(a)',iostat=ios) ' ..'
        endif

C REC-EPC-H3: Consider replacing heating boiler plant with a condensing type
C << Condition need to be added on the interface
C << MAKE SURE IT IS NOT WRITTEN SEVERAL TIMES - CHECK THE DO LOOPS >>
        if(HGEF(ICMSYS).lt.0.89)then
          if(FUELNAME(IFTYP(ICMSYS))(1:11).eq.'Natural gas'.or.
     &       FUELNAME(IFTYP(ICMSYS))(1:3).eq.'Oil'.or.
     &       FUELNAME(IFTYP(ICMSYS))(1:3).eq.'LPG')then

            WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-H3" = RECOMMENDATION'
            WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = HEATING'
            WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-H3'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
            WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'
            WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ',
     &      'UNKNOWN'

C Temporary change the efficiency to 0.89
            EFFORIGINAL(ICMSYS)=HGEF(ICMSYS)
            BERORIGINAL=BER
            HGEF(ICMSYS)=0.89

C Call BERTER to recalculate BER using the new efficiency
            CALL BERTER(IER)

C Then determine the % change from this in TOTAL building emissions
            H3_impact=((BERORIGINAL-BER)/BERORIGINAL)*100

C Return the efficiency back to it's original value
            HGEF(ICMSYS)=EFFORIGINAL(ICMSYS)
            BER=BERORIGINAL
            CALL BERTER(IER)

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low"
C << Implementation is similar as for EPC-H1
            CALL ECLOSE(H3_impact,4.0,0.001,ISNEAR)
            IF(H3_impact.GT.4.0)THEN
              WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
            ELSEIF((H3_impact.LT.4.0.OR.ISNEAR).AND.H3_impact.GT.0.5)
     &         THEN
              WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
            ELSE
              WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
            ENDIF

C For the next line: Instead of CALC, it could also be USER
            WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
            WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
            WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
            WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
            WRITE(IUF,'(a)',iostat=ios) ' ..'

C H3 needs to be written twice (for REC-HVAC-EPC-H3 item)
            WRITE(IUF,'(a)',iostat=ios)
     &                      '"REC-HVAC-EPC-H3" = RECOMMENDATION'
            WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = HEATING'
            WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-H3'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
            WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'
            WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ',
     &        'UNKNOWN'

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low"
C << Implementation is similar as for EPC-H1
            CALL ECLOSE(H3_impact,4.0,0.001,ISNEAR)
            IF(H3_impact.GT.4.0)THEN
              WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
            ELSEIF((H3_impact.LT.4.0.OR.ISNEAR).AND.H3_impact.GT.0.5)
     &         THEN
              WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
            ELSE
              WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
            ENDIF

C For the next line: Instead of CALC, it could also be USER
            WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
            WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
            WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
            WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'

C Next line is only for specific recommendations.
            WRITE(IUF,'(a)',iostat=ios) 'REF-OBJECT-TYPE = HVAC-SYSTEM'

C Next line is only for specific recommendations (no where documented).
C Need the name of the HVAC system here, which uses the specific fuel
            WRITE(IUF,'(2a)',iostat=ios) 'REF-OBJECT     = ',
     &          HVACNAME(ICMSYS)
            WRITE(IUF,'(a)',iostat=ios) ' ..'
          endif
        endif

C REC-EPC-H4: Check if using default heat generator efficiency
C Use 0.65 as default heat generator efficiency
C This is the same as for EPC-H1
        call eclose(HGEF(ICMSYS),0.65,0.001,ISNEAR)
        if(ISNEAR)then
          WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-H4" = RECOMMENDATION'
          WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = HEATING'
          WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-H4'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
          WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'
          WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ',
     &      'UNKNOWN'

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low"
C Using H1_impact as it is the same for H4
          CALL ECLOSE(H1_impact,4.0,0.001,ISNEAR)
          IF(H1_impact.GT.4.0)THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
          ELSEIF((H1_impact.LT.4.0.OR.ISNEAR).AND.H1_impact.GT.0.5)
     &       THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
          ELSE
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
          ENDIF

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
          WRITE(IUF,'(a)',iostat=ios) ' ..'

C REC-HVAC-EPC-H4:
          WRITE(IUF,'(a)',iostat=ios)
     &                    '"REC-HVAC-EPC-H4" = RECOMMENDATION'
          WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = HEATING'
          WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-H4'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
          WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low"
          CALL ECLOSE(H1_impact,4.0,0.001,ISNEAR)
          IF(H1_impact.GT.4.0)THEN
            WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ','HIGH'
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
          ELSEIF((H1_impact.LT.4.0.OR.ISNEAR).AND.H1_impact.GT.0.5)
     &       THEN
            WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ','MEDIUM'
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
          ELSE
            WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ','LOW'
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
          ENDIF

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'

C Next line is only for specific recommendations.
          WRITE(IUF,'(a)',iostat=ios) 'REF-OBJECT-TYPE = HVAC-SYSTEM'

C Next line is only for specific recommendations (no where documented).
C Need the name of the HVAC system here, which uses the specific fuel
          WRITE(IUF,'(2a)',iostat=ios) 'REF-OBJECT     = ',
     &          HVACNAME(ICMSYS)
          WRITE(IUF,'(a)',iostat=ios) ' ..'
        endif

C REC-EPC-H5: Check if heating systems have room by room time control
        if(iRecControls(3,ICMSYS).ne.1)then
          WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-H5" = RECOMMENDATION'
          WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = HEATING'
          WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-H5'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
          WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'
          WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ',
     &     'UNKNOWN'

C Need a new BER calculation here for an improvement of 1% on the heating
C efficiency and calculate total BER CO2 emissions after this change.
C Temporary change the efficiency to HGEF(ICMSYS)+0.01
          EFFORIGINAL(ICMSYS)=HGEF(ICMSYS)
          BERORIGINAL=BER
          HGEF(ICMSYS)= HGEF(ICMSYS)+0.01

C Call BERTER to recalculate BER using the new efficiency
          CALL BERTER(IER)

C Then determine the % change from this in TOTAL building emissions
          H5_impact=((BERORIGINAL-BER)/BERORIGINAL)*100

C Return the efficiency back to it's original value
          HGEF(ICMSYS)=EFFORIGINAL(ICMSYS)
          BER=BERORIGINAL
          CALL BERTER(IER)

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low".
          CALL ECLOSE(H5_impact,4.0,0.001,ISNEAR)
          IF(H5_impact.GT.4.0)THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
          ELSEIF((H5_impact.LT.4.0.OR.ISNEAR).AND.H5_impact.GT.0.5)
     &       THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
          ELSE
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
          ENDIF

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
          WRITE(IUF,'(a)',iostat=ios) ' ..'

C REC-HVAC-EPC-H5: Check if heating systems have room by room time control
          WRITE(IUF,'(a)',iostat=ios)
     &                    '"REC-HVAC-EPC-H5" = RECOMMENDATION'
          WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = HEATING'
          WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-H5'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
          WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

          WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ',
     &     'UNKNOWN'

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low".
          CALL ECLOSE(H5_impact,4.0,0.001,ISNEAR)
          IF(H5_impact.GT.4.0)THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
          ELSEIF((H5_impact.LT.4.0.OR.ISNEAR).AND.H5_impact.GT.0.5)
     &       THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
          ELSE
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
          ENDIF

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'

C Next line is only for specific recommendations.
          WRITE(IUF,'(a)',iostat=ios) 'REF-OBJECT-TYPE = HVAC-SYSTEM'

C Next line is only for specific recommendations (no where documented).
C Need the name of the HVAC system here, which uses the specific fuel
          WRITE(IUF,'(2a)',iostat=ios) 'REF-OBJECT     = ',
     &          HVACNAME(ICMSYS)
          WRITE(IUF,'(a)',iostat=ios) ' ..'
        endif

C REC-EPC-H6: Check if heating systems have room by room temperature control
        if(iRecControls(4,ICMSYS).ne.1)then
          WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-H6" = RECOMMENDATION'
          WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = HEATING'
          WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-H6'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
          WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Calculation for ENERGY-IMPACT is optional for detailed simulation programs
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT  = UNKNOWN'

C Need a new BER calculation here for an improvement of 2% on the heating
C efficiency and calculate total BER CO2 emissions after this change.
C Temporary change the efficiency to HGEF(ICMSYS)+0.02
          EFFORIGINAL(ICMSYS)=HGEF(ICMSYS)
          BERORIGINAL=BER
          HGEF(ICMSYS)= HGEF(ICMSYS)+0.02

C Call BERTER to recalculate BER using the new efficiency
          CALL BERTER(IER)

C Then determine the % change from this in TOTAL building emissions
          H6_impact=((BERORIGINAL-BER)/BERORIGINAL)*100

C Return the efficiency and BER back to their original value
          HGEF(ICMSYS)=EFFORIGINAL(ICMSYS)
          BER=BERORIGINAL
          CALL BERTER(IER)

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low".
          CALL ECLOSE(H6_impact,4.0,0.001,ISNEAR)
          IF(H6_impact.GT.4.0)THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
          ELSEIF((H6_impact.LT.4.0.OR.ISNEAR).AND.H6_impact.GT.0.5)
     &       THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
          ELSE
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
          ENDIF

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
          WRITE(IUF,'(a)',iostat=ios) ' ..'

C REC-HVAC-EPC-H6: Check if heating systems have room by room
C temperature control
          WRITE(IUF,'(a)',iostat=ios)
     &                    '"REC-HVAC-EPC-H6" = RECOMMENDATION'
          WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = HEATING'
          WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-H6'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
          WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Calculation for ENERGY-IMPACT is optional for detailed simulation programs
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT  = UNKNOWN'

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low".
          CALL ECLOSE(H6_impact,4.0,0.001,ISNEAR)
          IF(H6_impact.GT.4.0)THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
          ELSEIF((H6_impact.LT.4.0.OR.ISNEAR).AND.H6_impact.GT.0.5)
     &       THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
          ELSE
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
          ENDIF

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'

C Next line is only for specific recommendations.
          WRITE(IUF,'(a)',iostat=ios) 'REF-OBJECT-TYPE = HVAC-SYSTEM'

C Next line is only for specific recommendations (no where documented).
C Need the name of the HVAC system here, which uses the specific fuel
          WRITE(IUF,'(2a)',iostat=ios) 'REF-OBJECT     = ',
     &          HVACNAME(ICMSYS)
          WRITE(IUF,'(a)',iostat=ios) ' ..'
        endif

C REC-EPC-H7: Check if heating systems have optimum start/stop control
        if(iRecControls(2,ICMSYS).ne.1)then
          WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-H7" = RECOMMENDATION'
          WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = HEATING'
          WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-H7'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
          WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'
          WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ',
     &     'UNKNOWN'

C Need a new BER calculation here for an improvement of 2% on the heating
C efficiency and calculate total BER CO2 emissions after this change.
C Temporary change the efficiency to HGEF(ICMSYS)+2.%
          EFFORIGINAL(ICMSYS)=HGEF(ICMSYS)
          BERORIGINAL=BER
          HGEF(ICMSYS)= HGEF(ICMSYS)+0.02

C Call BERTER to recalculate BER using the new efficiency
          CALL BERTER(IER)

C Then determine the % change from this in TOTAL building emissions
          H7_impact=((BERORIGINAL-BER)/BERORIGINAL)*100

C Return the efficiency back to it's original value
          HGEF(ICMSYS)=EFFORIGINAL(ICMSYS)
          BER=BERORIGINAL
          CALL BERTER(IER)

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low".
          CALL ECLOSE(H7_impact,4.0,0.001,ISNEAR)
          IF(H7_impact.GT.4.0)THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
          ELSEIF((H7_impact.LT.4.0.OR.ISNEAR).AND.H7_impact.GT.0.5)
     &       THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
          ELSE
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
          ENDIF

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
          WRITE(IUF,'(a)',iostat=ios) ' ..'

C REC-HVAC-EPC-H7: Check if heating systems have optimum
C start/stop control
          WRITE(IUF,'(a)',iostat=ios)
     &                    '"REC-HVAC-EPC-H7" = RECOMMENDATION'
          WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = HEATING'
          WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-H7'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
          WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'
          WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ',
     &     'UNKNOWN'

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low".
          CALL ECLOSE(H7_impact,4.0,0.001,ISNEAR)
          IF(H7_impact.GT.4.0)THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
          ELSEIF((H7_impact.LT.4.0.OR.ISNEAR).AND.H7_impact.GT.0.5)
     &       THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
          ELSE
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
          ENDIF
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'

C Next line is only for specific recommendations.
          WRITE(IUF,'(a)',iostat=ios) 'REF-OBJECT-TYPE = HVAC-SYSTEM'

C Next line is only for specific recommendations (no where documented).
C Need the name of the HVAC system here, which uses the specific fuel
          WRITE(IUF,'(2a)',iostat=ios) 'REF-OBJECT     = ',
     &          HVACNAME(ICMSYS)
          WRITE(IUF,'(a)',iostat=ios) ' ..'
        endif

C REC-EPC-H8: Check if heating systems have weather compensation controls
        if(iRecControls(5,ICMSYS).ne.1)then
          WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-H8" = RECOMMENDATION'
          WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = HEATING'
          WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-H8'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
          WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'
          WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ',
     &     'UNKNOWN'

C Need a new BER calculation here for an improvement of 1.5% on the heating
C efficiency and calculate total BER CO2 emissions after this change.
C Temporary change the efficiency to HGEF(ICMSYS)+1.5%
          EFFORIGINAL(ICMSYS)=HGEF(ICMSYS)
          BERORIGINAL=BER
          HGEF(ICMSYS)= HGEF(ICMSYS)+0.015

C Call BERTER to recalculate BER using the new efficiency
          CALL BERTER(IER)

C Then determine the % change from this in TOTAL building emissions
          H8_impact=((BERORIGINAL-BER)/BERORIGINAL)*100

C Return the efficiency and BER back to their original values
          HGEF(ICMSYS)=EFFORIGINAL(ICMSYS)
          BER=BERORIGINAL
          CALL BERTER(IER)

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low".
          CALL ECLOSE(H8_impact,4.0,0.001,ISNEAR)
          IF(H8_impact.GT.4.0)THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
          ELSEIF((H8_impact.LT.4.0.OR.ISNEAR).AND.H8_impact.GT.0.5)
     &       THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
          ELSE
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
          ENDIF

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
          WRITE(IUF,'(a)',iostat=ios) ' ..'

C REC-HVAC-EPC-H8: Check if heating systems have weather compensation controls
          WRITE(IUF,'(a)',iostat=ios)
     &                    '"REC-HVAC-EPC-H8" = RECOMMENDATION'
          WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = HEATING'
          WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-H8'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
          WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'
          WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ',
     &     'UNKNOWN'

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low".
          CALL ECLOSE(H8_impact,4.0,0.001,ISNEAR)
          IF(H8_impact.GT.4.0)THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
          ELSEIF((H8_impact.LT.4.0.OR.ISNEAR).AND.H8_impact.GT.0.5)
     &       THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
          ELSE
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
          ENDIF

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'

C Next line is only for specific recommendations.
          WRITE(IUF,'(a)',iostat=ios) 'REF-OBJECT-TYPE = HVAC-SYSTEM'

C Next line is only for specific recommendations (no where documented).
C Need the name of the HVAC system here, which uses the specific fuel
          WRITE(IUF,'(2a)',iostat=ios) 'REF-OBJECT     = ',
     &          HVACNAME(ICMSYS)
          WRITE(IUF,'(a)',iostat=ios) ' ..'
        endif

C COOLING
C If there is a cooling system
C << Using 2.2 as default cooling efficiency for now
C << The default values in ESP-r seem too high - need to check this
C REC-EPC-C1: Check if using default cooling efficiency
        call eclose(CGEF(ICMSYS),2.2,0.001,ISNEAR)
	if(INCMSYS(ICMSYS).eq.0)then
	  continue
        elseif(IBRUKLC(1,INCMSYS(ICMSYS)).ne.-1111.and.ISNEAR)then
          WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-C1" = RECOMMENDATION'
          WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = COOLING'
          WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-C1'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
          WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Temporary change the efficiency to 2.5
          EFFORIGINAL(ICMSYS)=CGEF(ICMSYS)
          BERORIGINAL=BER
          CGEF(ICMSYS)=2.5

C Call BERTER to recalculate BER using the new efficiency
          CALL BERTER(IER)

C Then determine the % change from this in TOTAL building emissions
          C1_impact=((BERORIGINAL-BER)/BERORIGINAL)*100

C Return the efficiency back to it's original value
          CGEF(ICMSYS)=EFFORIGINAL(ICMSYS)
          BER=BERORIGINAL
          CALL BERTER(IER)

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low"
          CALL ECLOSE(C1_impact,4.0,0.001,ISNEAR)
          IF(C1_impact.GT.4.0)THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
          ELSEIF((C1_impact.LT.4.0.OR.ISNEAR).AND.C1_impact.GT.0.5)
     &       THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
          ELSE
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
          ENDIF

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
          WRITE(IUF,'(a)',iostat=ios) ' ..'

C REC-HVAC-EPC-C1: Check if using default cooling efficiency
          WRITE(IUF,'(a)',iostat=ios)
     &                    '"REC-HVAC-EPC-C1" = RECOMMENDATION'
          WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = COOLING'
          WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-C1'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
          WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'
          WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ',
     &     'UNKNOWN'

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low".
          CALL ECLOSE(C1_impact,4.0,0.001,ISNEAR)
          IF(C1_impact.GT.4.0)THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
          ELSEIF((C1_impact.LT.4.0.OR.ISNEAR).AND.C1_impact.GT.0.5)
     &       THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
          ELSE
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
          ENDIF

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'

C Next line is only for specific recommendations.
          WRITE(IUF,'(a)',iostat=ios) 'REF-OBJECT-TYPE = HVAC-SYSTEM'

C Next line is only for specific recommendations (no where documented).
C Need the name of the HVAC system here, which uses the specific fuel
          WRITE(IUF,'(2a)',iostat=ios) 'REF-OBJECT     = ',
     &          HVACNAME(ICMSYS)
          WRITE(IUF,'(a)',iostat=ios) ' ..'
        endif

C REC-EPC-C2: Check if cooling efficiency is less or equal to 2.4 and
C greater than 2.0
	if(INCMSYS(ICMSYS).eq.0)then
	  continue
        elseif(IBRUKLC(1,INCMSYS(ICMSYS)).ne.-1111)then
          call eclose(CGEF(ICMSYS),2.4,0.001,ISNEAR)
          if(ISNEAR.or.(CGEF(ICMSYS).lt.2.4.and.CGEF(ICMSYS).gt.2.0))
     &     then
            WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-C2" = RECOMMENDATION'
            WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = COOLING'
            WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-C2'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
            WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'
            WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ',
     &       'UNKNOWN'

C this is the same as C1 - so use C1_impact
            CALL ECLOSE(C1_impact,4.0,0.001,ISNEAR)

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low"
            IF(C1_impact.GT.4.0)THEN
              WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
            ELSEIF((C1_impact.LT.4.0.OR.ISNEAR).AND.C1_impact.GT.0.5)
     &         THEN
              WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
            ELSE
              WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
            ENDIF

C For the next line: Instead of CALC, it could also be USER
            WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
            WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
            WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
            WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
            WRITE(IUF,'(a)',iostat=ios) ' ..'

C REC-HVAC-EPC-C2: Check if cooling efficiency is less or equal to 2.4 and
C greater than 2.0
            WRITE(IUF,'(a)',iostat=ios)
     &                      '"REC-HVAC-EPC-C2" = RECOMMENDATION'
            WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = COOLING'
            WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-C2'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
            WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'
            WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ',
     &       'UNKNOWN'

C this is the same as C1 - so use C1_impact
            CALL ECLOSE(C1_impact,4.0,0.001,ISNEAR)

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low"
            IF(C1_impact.GT.4.0)THEN
              WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
            ELSEIF((C1_impact.LT.4.0.OR.ISNEAR).AND.C1_impact.GT.0.5)
     &         THEN
              WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
            ELSE
              WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
            ENDIF

C For the next line: Instead of CALC, it could also be USER
            WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
            WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
            WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
            WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'

C Next line is only for specific recommendations.
            WRITE(IUF,'(a)',iostat=ios) 'REF-OBJECT-TYPE = HVAC-SYSTEM'

C Next line is only for specific recommendations (no where documented).
C Need the name of the HVAC system here, which uses the specific fuel
            WRITE(IUF,'(2a)',iostat=ios) 'REF-OBJECT     = ',
     &          HVACNAME(ICMSYS)
            WRITE(IUF,'(a)',iostat=ios) ' ..'
          endif
        endif

C REC-EPC-C3: Duct and AHU leakage
C << ADD logic to write it only once for the building and not when triggered
C << for every system >>
C << Check for -1111 is not probably necessary since the systems used are known>>
	if(incmsys(icmsys).eq.0)then
	  continue
        elseif(ibruklc(1,incmsys(icmsys)).ne.-1111)then

C Check if HVAC system is VAV, fan coil, induction, constant volume,
C multizone, terminal reheat, dual duct chilled ceiling or chilled
C beam (with displacement ventilation)
          if(incmsys(icmsys).ge.12.and.incmsys(icmsys).le.23)
     &    then
            totLeakage(icmsys)=duct_tDLd(icmsys)+AHU_tDLd(icmsys)
            call eclose(totLeakage(icmsys),0.05,0.0001,ISNEAR)
            if((isnear.or.totLeakage(icmsys).gt.0.05).and.
     &        totLeakage(icmsys).lt.0.1)then
              write(iuf,'(a)',iostat=ios)
     &          '"REC-EPC-C3" = RECOMMENDATION'
              write(iuf,'(a)',iostat=ios) 'CATEGORY     = COOLING'
              write(iuf,'(a)',iostat=ios) 'CODE         = EPC-C3'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
              write(iuf,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Calculation for ENERGY-IMPACT is optional for detailed simulation programs
              write(iuf,'(a)',iostat=ios) 'ENERGY-IMPACT  = UNKNOWN'
              beroriginal=ber
              coolOriginal(icmsys)=bersc(icmsys)

C If fan coil reduce cooling energy by 2%
              if(incmsys(icmsys).eq.15)then
                bersc(icmsys)=0.98*bersc(icmsys)

C If chilled ceiling, chilled beam reduce cooling energy by 0.5%
              elseif(incmsys(icmsys).eq.22.or.incmsys(icmsys).eq.23)then
                bersc(icmsys)=0.995*bersc(icmsys)

C If VAV, CAV, multizone, terminal reheat, dual duct reduce cooling
C energy by 5%
              else
                bersc(icmsys)=0.95*bersc(icmsys)
              endif

C Call BERTER to recalculate BER using the new efficiency
              call BERTER(ier)

C Then determine the % change from this in TOTAL building emissions
C BER is now a new recalculated BER after changing the cooling energy
              C3_impact=((beroriginal-ber)/beroriginal)*100

C Return the cooling energy and BER back to their original values
              bersc(icmsys)=coolOriginal(icmsys)
              ber=beroriginal
              call BERTER(ier)
              call eclose(c3_impact,4.0,0.001,isnear)

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low"
              if(c3_impact.gt.4.0)then
                write(iuf,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
              elseif((c3_impact.lt.4.0.or.isnear).and.c3_impact.gt.0.5)
     &          then
                write(iuf,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
              else
                write(iuf,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
              endif

C For the next line: Instead of CALC, it could also be USER
              write(iuf,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
              write(iuf,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
              write(iuf,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
              write(iuf,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
              write(iuf,'(a)',iostat=ios) ' ..'

C REC-HVAC-EPC-C3: Duct and AHU leakage
              write(iuf,'(a)',iostat=ios)
     &                      '"REC-HVAC-EPC-C3" = RECOMMENDATION'
              write(iuf,'(a)',iostat=ios) 'CATEGORY     = COOLING'
              write(iuf,'(a)',iostat=ios) 'CODE         = EPC-C3'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
              write(iuf,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Calculation for ENERGY-IMPACT is optional for detailed simulation programs
              write(iuf,'(a)',iostat=ios) 'ENERGY-IMPACT  = UNKNOWN'
              call eclose(c3_impact,4.0,0.001,isnear)
              if(c3_impact.gt.4.0)then
                write(iuf,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
              elseif((c3_impact.lt.4.0.or.isnear).and.c3_impact.gt.0.5)
     &          then
                write(iuf,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
              else
                write(iuf,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
              endif

C For the next line: Instead of CALC, it could also be USER
              write(iuf,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
              write(iuf,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: instead of USER, it could also be CALC
              write(iuf,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: instead of USER, it could also be CALC
              write(iuf,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'

C Next line is only for specific recommendations.
              write(iuf,'(a)',iostat=ios)
     &           'REF-OBJECT-TYPE = HVAC-SYSTEM'

C Next line is only for specific recommendations (no where documented).
C Need the name of the HVAC system here, which uses the specific fuel
              write(iuf,'(2a)',iostat=ios) 'REF-OBJECT     = ',
     &          hvacname(icmsys)
              write(iuf,'(a)',iostat=ios) ' ..'
            else
              call eclose(totLeakage(icmsys),0.1,0.0001,isnear)
              if((isnear.or.totLeakage(icmsys).gt.0.1))then
                write(iuf,'(a)',iostat=ios)
     &            '"REC-EPC-C3" = RECOMMENDATION'
                write(iuf,'(a)',iostat=ios) 'CATEGORY     = COOLING'
                write(iuf,'(a)',iostat=ios) 'CODE         = EPC-C3'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
                write(iuf,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Calculation for ENERGY-IMPACT is optional for detailed simulation programs
                write(iuf,'(a)',iostat=ios) 'ENERGY-IMPACT  = UNKNOWN'
                beroriginal=ber
                coolOriginal(icmsys)=bersc(icmsys)

C If fan coil reduce cooling energy by 4%
                if(incmsys(icmsys).eq.15)then
                  bersc(icmsys)=0.96*bersc(icmsys)

C If chilled ceiling, chilled beam reduce cooling energy by 1%
                elseif(incmsys(icmsys).eq.22.or.incmsys(icmsys).eq.23)
     &            then
                  bersc(icmsys)=0.99*bersc(icmsys)

C If VAV, CAV, multizone, terminal reheat, dual duct reduce cooling
C energy by 10%
                else
                  bersc(icmsys)=0.9*bersc(icmsys)
                endif

C Call BERTER to recalculate BER using the new efficiency
                call BERTER(ier)

C Then determine the % change from this in TOTAL building emissions
C BER is now a new recalculated BER after changing the cooling energy
                C3_impact=((beroriginal-ber)/beroriginal)*100

C Return the cooling energy and BER back to their original values
                bersc(icmsys)=coolOriginal(icmsys)
                ber=beroriginal
                call BERTER(ier)
                call eclose(c3_impact,4.0,0.001,isnear)

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low"
                if(c3_impact.gt.4.0)then
                  write(iuf,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
                elseif((c3_impact.lt.4.0.or.isnear)
     &            .and.c3_impact.gt.0.5)then
                  write(iuf,'(2a)',iostat=ios) 'CO2-IMPACT = ',
     &            'MEDIUM'
                else
                  write(iuf,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
                endif

C For the next line: Instead of CALC, it could also be USER
                write(iuf,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: instead of CALC, it could also be USER
                write(iuf,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: instead of USER, it could also be CALC
                write(iuf,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: instead of USER, it could also be CALC
                write(iuf,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
                write(iuf,'(a)',iostat=ios) ' ..'

C REC-HVAC-EPC-C3: Duct and AHU leakage
                write(iuf,'(a)',iostat=ios)
     &                      '"REC-HVAC-EPC-C3" = RECOMMENDATION'
                write(iuf,'(a)',iostat=ios) 'CATEGORY     = COOLING'
                write(iuf,'(a)',iostat=ios) 'CODE         = EPC-C3'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
                write(iuf,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Calculation for ENERGY-IMPACT is optional for detailed simulation programs
                write(iuf,'(a)',iostat=ios) 'ENERGY-IMPACT  = UNKNOWN'
                call eclose(c3_impact,4.0,0.001,isnear)
                if(c3_impact.GT.4.0)THEN
                  write(iuf,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
                elseif((c3_impact.lt.4.0.or.isnear)
     &            .and.c3_impact.gt.0.5)then
                  write(iuf,'(2a)',iostat=ios) 'CO2-IMPACT = ',
     &            'MEDIUM'
                else
                  write(iuf,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
                endif

C For the next line: Instead of CALC, it could also be USER
                write(iuf,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
                write(iuf,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: instead of USER, it could also be CALC
                write(iuf,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: instead of USER, it could also be CALC
                write(iuf,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'

C Next line is only for specific recommendations.
                write(iuf,'(a)',iostat=ios)
     &           'REF-OBJECT-TYPE = HVAC-SYSTEM'

C Next line is only for specific recommendations (no where documented).
C Need the name of the HVAC system here, which uses the specific fuel
                write(iuf,'(2a)',iostat=ios) 'REF-OBJECT     = ',
     &            hvacname(icmsys)
                write(iuf,'(a)',iostat=ios) ' ..'
            endif
          endif
        endif
        endif

C Check if heating fuel is Oil or LPG and if it is trigger EPC-F1
        if(IFTYP(ICMSYS).eq.4.or.IFTYP(ICMSYS).eq.2)then
           IFUELORIGINAL=IFTYP(ICMSYS)

C REC-HVAC-EPC-F1: Consider switching from oil or LPG to natural gas
          WRITE(IUF,'(a)',iostat=ios)
     &                           '"REC-HVAC-EPC-F1" = RECOMMENDATION'
          WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = FUEL-SWITCHING'
          WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-F1'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
          WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Calculation for ENERGY-IMPACT is optional for detailed simulation programs
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT  = UNKNOWN'

C Recalculate carbon emissions by using instead Natural Gas
          IFTYP(ICMSYS)=1
          BERORIGINAL=BER

C Call BERTER to recalculate BER using the new Fuel type
          CALL BERTER(IER)

C Then determine the % change from this in TOTAL building emissions
          F1_impact=((BERORIGINAL-BER)/BERORIGINAL)*100

C Return the fuel back to the original value
          IFTYP(ICMSYS)=IFUELORIGINAL
          BER=BERORIGINAL
          CALL BERTER(IER)

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low"
          CALL ECLOSE(F1_impact,4.0,0.001,ISNEAR)
          IF(F1_impact.GT.4.0)THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
          ELSEIF((F1_impact.LT.4.0.OR.ISNEAR).AND.F1_impact.GT.0.5)
     &      THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
          ELSE
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
          ENDIF

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'

C Next line is only for specific recommendations.
          WRITE(IUF,'(a)',iostat=ios) 'REF-OBJECT-TYPE = HVAC-SYSTEM'

C Next line is only for specific recommendations (no where documented).
C Need the name of the HVAC system here, which uses the specific fuel
          WRITE(IUF,'(2a)',iostat=ios) 'REF-OBJECT     = ',
     &          HVACNAME(ICMSYS)
          WRITE(IUF,'(a)',iostat=ios) ' ..'
        endif

C Check if heating fuel is coal and if it is trigger EPC-F2
        if(IFTYP(ICMSYS).eq.5)then

C REC-HVAC-EPC-F2: Consider converting the boiler from coal to natural gas
          WRITE(IUF,'(a)',iostat=ios)
     &                           '"REC-HVAC-EPC-F2" = RECOMMENDATION'
          WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = FUEL-SWITCHING'
          WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-F2'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
          WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Calculation for ENERGY-IMPACT is optional for detailed simulation programs
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT  = UNKNOWN'

C Recalculate carbon emissions by using instead Natural Gas
          IFTYP(ICMSYS)=1
          BERORIGINAL=BER

C Call BERTER to recalculate BER using the new Fuel type
          CALL BERTER(IER)

C Then determine the % change from this in TOTAL building emissions
          F2_impact=((BERORIGINAL-BER)/BERORIGINAL)*100

C Return the fuel index back to the original value
          IFTYP(ICMSYS)=5
          BER=BERORIGINAL
          CALL BERTER(IER)

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low"
          CALL ECLOSE(F2_impact,4.0,0.001,ISNEAR)
          IF(F2_impact.GT.4.0)THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
          ELSEIF((F2_impact.LT.4.0.OR.ISNEAR).AND.F2_impact.GT.0.5)
     &      THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
          ELSE
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
          ENDIF

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'

C Next line is only for specific recommendations.
          WRITE(IUF,'(a)',iostat=ios) 'REF-OBJECT-TYPE = HVAC-SYSTEM'

C Next line is only for specific recommendations (no where documented).
C Need the name of the HVAC system here, which uses the specific fuel
          WRITE(IUF,'(2a)',iostat=ios) 'REF-OBJECT     = ',
     &          HVACNAME(ICMSYS)
          WRITE(IUF,'(a)',iostat=ios) ' ..'
        endif

C Check if heating fuel is coal and if it is trigger EPC-F3
        if(IFTYP(ICMSYS).eq.5)then

C REC-HVAC-EPC-F3: Consider switching from coal to biomass
          WRITE(IUF,'(a)',iostat=ios)
     &                           '"REC-HVAC-EPC-F3" = RECOMMENDATION'
          WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = FUEL-SWITCHING'
          WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-F3'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
          WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Calculation for ENERGY-IMPACT is optional for detailed simulation programs
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT  = UNKNOWN'

C Recalculate carbon emissions by using instead biomass
          IFTYP(ICMSYS)=9
          BERORIGINAL=BER

C Call BERTER to recalculate BER using the new Fuel type
          CALL BERTER(IER)

C Then determine the % change from this in TOTAL building emissions
          F3_impact=((BERORIGINAL-BER)/BERORIGINAL)*100

C Return the fuel index back to the original value
          IFTYP(ICMSYS)=5
          BER=BERORIGINAL
          CALL BERTER(IER)

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low"
          CALL ECLOSE(F3_impact,4.0,0.001,ISNEAR)
          IF(F3_impact.GT.4.0)THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
          ELSEIF((F3_impact.LT.4.0.OR.ISNEAR).AND.F3_impact.GT.0.5)
     &      THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
          ELSE
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
          ENDIF

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'

C Next line is only for specific recommendations.
          WRITE(IUF,'(a)',iostat=ios) 'REF-OBJECT-TYPE = HVAC-SYSTEM'

C Next line is only for specific recommendations (no where documented).
C Need the name of the HVAC system here, which uses the specific fuel
          WRITE(IUF,'(2a)',iostat=ios) 'REF-OBJECT     = ',
     &          HVACNAME(ICMSYS)
          WRITE(IUF,'(a)',iostat=ios) ' ..'
        endif

C Check if heating fuel is Oil or LPG and if it is trigger EPC-F4
        if(IFTYP(ICMSYS).eq.4.or.IFTYP(ICMSYS).eq.2)then
           IFUELORIGINAL=IFTYP(ICMSYS)

C REC-HVAC-EPC-F4: Consider switching from oil or LPG to biomass
          WRITE(IUF,'(a)',iostat=ios)
     &                           '"REC-HVAC-EPC-F4" = RECOMMENDATION'
          WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = FUEL-SWITCHING'
          WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-F4'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
          WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Calculation for ENERGY-IMPACT is optional for detailed simulation programs
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT  = UNKNOWN'

C Recalculate carbon emissions by using instead Biomass
          IFTYP(ICMSYS)=9
          BERORIGINAL=BER

C Call BERTER to recalculate BER using the new Fuel type
          CALL BERTER(IER)

C Then determine the % change from this in TOTAL building emissions
          F4_impact=((BERORIGINAL-BER)/BERORIGINAL)*100

C Return the fuel CO2 emissions back to the original value
          IFTYP(ICMSYS)=IFUELORIGINAL
          BER=BERORIGINAL
          CALL BERTER(IER)

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low"
          CALL ECLOSE(F4_impact,4.0,0.001,ISNEAR)
          IF(F4_impact.GT.4.0)THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
          ELSEIF((F4_impact.LT.4.0.OR.ISNEAR).AND.F4_impact.GT.0.5)
     &      THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
          ELSE
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
          ENDIF

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'

C Next line is only for specific recommendations.
          WRITE(IUF,'(a)',iostat=ios) 'REF-OBJECT-TYPE = HVAC-SYSTEM'

C Next line is only for specific recommendations (no where documented).
C Need the name of the HVAC system here, which uses the specific fuel
          WRITE(IUF,'(2a)',iostat=ios) 'REF-OBJECT     = ',
     &      HVACNAME(ICMSYS)
          WRITE(IUF,'(a)',iostat=ios) ' ..'
        endif

C Check if heating fuel is gas and if it is trigger EPC-F5
        if(IFTYP(ICMSYS).eq.1)then

C REC-HVAC-EPC-F5: Consider switching from (natural) gas to biomass
          WRITE(IUF,'(a)',iostat=ios)
     &                           '"REC-HVAC-EPC-F5" = RECOMMENDATION'
          WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = FUEL-SWITCHING'
          WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-F5'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
          WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Calculation for ENERGY-IMPACT is optional for detailed simulation programs
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT  = UNKNOWN'

C Recalculate carbon emissions by using instead Biomass
          IFTYP(ICMSYS)=9
          BERORIGINAL=BER

C Call BERTER to recalculate BER using the new Fuel type
          CALL BERTER(IER)

C Then determine the % change from this in TOTAL building emissions
          F5_impact=((BERORIGINAL-BER)/BERORIGINAL)*100

C Return the fuel back to the original value
          IFTYP(ICMSYS)=1
          BER=BERORIGINAL
          CALL BERTER(IER)

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low"
          CALL ECLOSE(F5_impact,4.0,0.001,ISNEAR)
          IF(F5_impact.GT.4.0)THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
          ELSEIF((F5_impact.LT.4.0.OR.ISNEAR).AND.F5_impact.GT.0.5)
     &      THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
          ELSE
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
          ENDIF

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'

C Next line is only for specific recommendations.
          WRITE(IUF,'(a)',iostat=ios) 'REF-OBJECT-TYPE = HVAC-SYSTEM'

C Next line is only for specific recommendations (no where documented).
C Need the name of the HVAC system here, which uses the specific fuel
          WRITE(IUF,'(2a)',iostat=ios) 'REF-OBJECT     = ',
     &          HVACNAME(ICMSYS)
          WRITE(IUF,'(a)',iostat=ios) ' ..'
        endif

C Check if heating fuel is coal and if it is trigger EPC-F6
        if(IFTYP(ICMSYS).eq.5)then

C REC-HVAC-EPC-F6: Consider switching from coal to oil
          WRITE(IUF,'(a)',iostat=ios)
     &                           '"REC-HVAC-EPC-F6" = RECOMMENDATION'
          WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = FUEL-SWITCHING'
          WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-F6'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
          WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Calculation for ENERGY-IMPACT is optional for detailed simulation programs
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT  = UNKNOWN'

C Recalculate carbon emissions by using instead Oil
          IFTYP(ICMSYS)=4
          BERORIGINAL=BER

C Call BERTER to recalculate BER using the new Fuel type
          CALL BERTER(IER)

C Then determine the % change from this in TOTAL building emissions
          F6_impact=((BERORIGINAL-BER)/BERORIGINAL)*100

C Return the fuel index back to the original value
          IFTYP(ICMSYS)=5
          BER=BERORIGINAL
          CALL BERTER(IER)

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low"
          CALL ECLOSE(F6_impact,4.0,0.001,ISNEAR)
          IF(F6_impact.GT.4.0)THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
          ELSEIF((F6_impact.LT.4.0.OR.ISNEAR).AND.F6_impact.GT.0.5)
     &      THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
          ELSE
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
          ENDIF

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'

C Next line is only for specific recommendations.
          WRITE(IUF,'(a)',iostat=ios) 'REF-OBJECT-TYPE = HVAC-SYSTEM'

C Next line is only for specific recommendations (no where documented).
C Need the name of the HVAC system here, which uses the specific fuel
          WRITE(IUF,'(2a)',iostat=ios) 'REF-OBJECT     = ',
     &          HVACNAME(ICMSYS)
          WRITE(IUF,'(a)',iostat=ios) ' ..'
        endif
 607  continue

C ***Hot water recommendations
C Check if hot water is provided by the heating generator
C << NEED THE CHECK HERE: if DHW not from heating generator >>

C Loop for all DHW systems
      DO 608 IcurrentDHW=1,NDHWSYS

C REC-EPC-W1: Install more efficient water heater
        call eclose(HWEF(IcurrentDHW),0.79,0.0001,ISNEAR)

C << Confirm that variable HWEF(IcurrentDHW) is correct >>
        if(ISNEAR.or.(HWEF(IcurrentDHW).lt.0.79))then
          WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-W1" = RECOMMENDATION'
          WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = HOT-WATER'
          WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-W1'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
          WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Calculation for ENERGY-IMPACT is optional for detailed simulation programs
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT  = UNKNOWN'

C Need a new BER calculation here for a DWH generator efficiency of 0.8
C Temporary change the efficiency to HWEF(IcurrentDHW)=0.8
          EFFDHWORIGINAL(IcurrentDHW)=HWEF(IcurrentDHW)
          BERORIGINAL=BER
          HWEF(IcurrentDHW)=0.8

C Call BERTER to recalculate BER using the new efficiency
          CALL BERTER(IER)

C Then determine the % change from this in TOTAL building emissions
C BER is now a new recalculated BER after changing HWEF(IcurrentDHW)= 0.8
          W1_impact=((BERORIGINAL-BER)/BERORIGINAL)*100

C Return the efficiency and BER back to their original values
          HWEF(IcurrentDHW)=EFFDHWORIGINAL(IcurrentDHW)
          BER=BERORIGINAL
          CALL BERTER(IER)

C If %change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low".
          CALL ECLOSE(W1_impact,4.0,0.001,ISNEAR)
          IF(W1_impact.GT.4.0)THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
          ELSEIF((W1_impact.LT.4.0.OR.ISNEAR).AND.W1_impact.GT.0.5)
     &       THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
          ELSE
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
          ENDIF

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'

C Next line is only for specific recommendations.
          WRITE(IUF,'(a)',iostat=ios)'REF-OBJECT-TYPE = DHW-GENERATOR'

C Next line is only for specific recommendations (no where documented).
C Need the name of the HVAC system here, which uses the specific fuel
          WRITE(IUF,'(2a)',iostat=ios) 'REF-OBJECT     = ',
     &       DHWNAME(IcurrentDHW)
          WRITE(IUF,'(a)',iostat=ios) ' ..'
        endif

C REC-EPC-W2: Consider replacing (poor) DHW system with point of use system
        call eclose(HWEF(IcurrentDHW),0.70,0.0001,ISNEAR)

C << Confirm that variable HWEF(IcurrentDHW) is correct >>
        if(ISNEAR.or.(HWEF(IcurrentDHW).lt.0.70))then
          WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-W2" = RECOMMENDATION'
          WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = HOT-WATER'
          WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-W2'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
          WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Calculation for ENERGY-IMPACT is optional for detailed simulation programs
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT  = UNKNOWN'

C Need a new BER calculation here for a DWH generator efficiency of 0.8
C Temporary change the efficiency to HWEF(IcurrentDHW)=0.8
          EFFDHWORIGINAL(IcurrentDHW)=HWEF(IcurrentDHW)
          BERORIGINAL=BER
          HWEF(IcurrentDHW)=0.75

C Call BERTER to recalculate BER using the new efficiency
          CALL BERTER(IER)

C Then determine the % change from this in TOTAL building emissions
C BER is now a new recalculated BER after changing HWEF(IcurrentDHW)= 0.8
          W2_impact=((BERORIGINAL-BER)/BERORIGINAL)*100

C Return the efficiency and BER back to their original values
          HWEF(IcurrentDHW)=EFFDHWORIGINAL(IcurrentDHW)
          BER=BERORIGINAL
          CALL BERTER(IER)

C If %change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low".
          CALL ECLOSE(W2_impact,4.0,0.001,ISNEAR)
          IF(W2_impact.GT.4.0)THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
          ELSEIF((W2_impact.LT.4.0.OR.ISNEAR).AND.W2_impact.GT.0.5)
     &       THEN
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
          ELSE
            WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
          ENDIF

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
          WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'

C Next line is only for specific recommendations.
          WRITE(IUF,'(a)',iostat=ios) 'REF-OBJECT-TYPE = DHW-GENERATOR'

C Next line is only for specific recommendations (no where documented).
C Need the name of the HVAC system here, which uses the specific fuel
          WRITE(IUF,'(2a)',iostat=ios) 'REF-OBJECT     = ',
     &       DHWNAME(IcurrentDHW)
          WRITE(IUF,'(a)',iostat=ios) ' ..'
        endif

C REC-EPC-W3: Improve insulation on DHW storage
C << CHECK if this has to be within the loop 608 of the hot water systems
C << DHW storage still needs to be implemented >>

C << Logic is: if storage heat loss > default value*0.9 then trigger EPC-W3
C << Default value needs to be decided >>
        WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-W3" = RECOMMENDATION'
        WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = HOT-WATER'
        WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-W3'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
        WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'


C << Need a new BER calculation here for a DHW system that consumes
C energy based on 50% less storage losses.
C << This needs then to be used and calculate total BER CO2 emissions
C << Then determine the % change from this in TOTAL building emissions
C << If change in total carbon emission is >4% then potential impact is "high"
C << If change in total carbon emission is greater than 0.5 and
C << less or equal than 4% then potential impact is "medium"
C << Otherwise impact is "low"
        WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ','LOW'
        WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
        WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
        WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
        WRITE(IUF,'(a)',iostat=ios) ' ..'

C REC-EPC-W3: needs to be written twice
C REC-EPC-W3: Improve insulation on DHW storage
C << CHECK if this has to be within the loop 608 of the hot water systems
C << DHW storage still needs to be implemented >>

C << Logic is: if storage heat loss > default value*0.9 then trigger EPC-W3
C << Default value needs to be decided >>>
        WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-W3" = RECOMMENDATION'
        WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = HOT-WATER'
        WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-W3'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
        WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C << Need a new BER calculation here for a DHW system that consumes
C energy based on 50% less storage losses.
C << This needs then to be used and calculate total BER CO2 emissions
C << Then determine the % change from this in TOTAL building emissions
C << If change in total carbon emission is >4% then potential impact is "high"
C << If change in total carbon emission is greater than 0.5 and
C << less or equal than 4% then potential impact is "medium"
C << Otherwise impact is "low"
        WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ','LOW'
        WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
        WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
        WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'

C Next line is only for specific recommendations.
        WRITE(IUF,'(a)',iostat=ios) 'REF-OBJECT-TYPE = DHW-GENERATOR'

C Next line is only for specific recommendations (no where documented).
C Need the name of the HVAC system here, which uses the specific fuel
        WRITE(IUF,'(2a)',iostat=ios) 'REF-OBJECT     = ',
     &     DHWNAME(IcurrentDHW)
        WRITE(IUF,'(a)',iostat=ios) ' ..'


C REC-EPC-W4: Add time control to DHW secondary circulation
C << CHECK if this has to be within the loop 608 of the hot water systems
C << Still needs to be implemented
C << Logic is: if there is secondary DHW circulation and there is no time
C << control then trigger EPC-W4

        WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-W4" = RECOMMENDATION'
        WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = HOT-WATER'
        WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-W4'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
        WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C << Need a new BER calculation here for a DHW system that consumes
C energy 30% less of total DHW energy.
C << This needs then to be used and calculate total BER CO2 emissions
C << Then determine the % change from this in TOTAL building emissions
C << If change in total carbon emission is >4% then potential impact is "high"
C << If change in total carbon emission is greater than 0.5 and
C << less or equal than 4% then potential impact is "medium"
C << Otherwise impact is "low"
        WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ','LOW'
        WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
        WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
        WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
        WRITE(IUF,'(a)',iostat=ios) ' ..'

C REC-EPC-W4: needs to be written twice
C REC-EPC-W4: Add time control to DHW secondary circulation
        WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-W4" = RECOMMENDATION'
        WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = HOT-WATER'
        WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-W4'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
        WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C << Need a new BER calculation here for a DHW system that consumes
C energy 30% less of total DHW energy.
C << This needs then to be used and calculate total BER CO2 emissions
C << Then determine the % change from this in TOTAL building emissions
C << If change in total carbon emission is >4% then potential impact is "high"
C << If change in total carbon emission is greater than 0.5 and
C << less or equal than 4% then potential impact is "medium"
C << Otherwise impact is "low"
        WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ','LOW'
        WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
        WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
        WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'

C Next line is only for specific recommendations.
        WRITE(IUF,'(a)',iostat=ios) 'REF-OBJECT-TYPE = DHW-GENERATOR'

C Next line is only for specific recommendations (no where documented).
C Need the name of the HVAC system here, which uses the specific fuel
        WRITE(IUF,'(2a)',iostat=ios) 'REF-OBJECT     = ',
     &     DHWNAME(IcurrentDHW)
        WRITE(IUF,'(a)',iostat=ios) ' ..'
 608  continue

C Lighting
      ExistLightType=.false.
      do 609 icomp=1,ncomp
        if(ILITYP(ICOMP).eq.1)then
          ExistLightType=.true.

C << Need a new calculation for a new  BER by using instead T8 Fluorescent
C - triphosphor - high frequency ballast lamps
C Recalculate carbon emissions
          LIGHTORIGINAL(ICOMP)=BERLIGHT(ICOMP)

C << This is hardwired here and it is taken from the SBEM database for
C T8 high frequency: 3.4 W/m2 per 100 lux divided by T12: 5.0 W/m2 per 100 lux
          BERLIGHT(ICOMP)=(3.4/5.0)*BERLIGHT(ICOMP)
        endif
 609  continue

C REC-EPC-L1: Replace 38mm duameter (T12) fluorescent tubes on failure
C with 26mm (T8) tubes
      if(ExistLightType)then
        WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-L1" = RECOMMENDATION'
        WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = LIGHTING'
        WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-L1'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
        WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Calculation for ENERGY-IMPACT is optional for detailed simulation programs
        WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT  = UNKNOWN'

C Recalculate carbon emissions
        BERORIGINAL=BER

C Call BERTER to recalculate BER using the new T8 lamps
        CALL BERTER(IER)

C Then determine the % change from this in TOTAL building emissions
        L1_impact=((BERORIGINAL-BER)/BERORIGINAL)*100

C set BER as for initial calculations
        BER=BERORIGINAL
        CALL BERTER(IER)

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low"
        CALL ECLOSE(L1_impact,4.0,0.001,ISNEAR)
        IF(L1_impact.GT.4.0)THEN
          WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
        ELSEIF((L1_impact.LT.4.0.OR.ISNEAR).AND.L1_impact.GT.0.5)
     &    THEN
          WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
        ELSE
          WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
        ENDIF

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
        WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
        WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
        WRITE(IUF,'(a)',iostat=ios) ' ..'
      endif
      do 709 icomp=1,ncomp

C Return the BERLIGHT back to the original values
        BERLIGHT(ICOMP)=LIGHTORIGINAL(ICOMP)
 709  continue
      ExistLightType=.false.
      do 710 icomp=1,ncomp
        if(ILITYP(ICOMP).eq.9)then
          ExistLightType=.true.

C REC-EPC-L2: Replace tungsten GLS lamps with CFLs
C Recalculate carbon emissions
          LIGHTORIGINAL(ICOMP)=BERLIGHT(ICOMP)

C << This is hardwired here and it is taken from the SBEM database for
C Compact fluorescent: 4.6 W/m2 per 100 lux divided by
C Tungsten lamp (GLS): 28.0 W/m2 per 100 lux
          BERLIGHT(ICOMP)=(4.6/28.0)*BERLIGHT(ICOMP)
        endif
 710  continue

C REC-EPC-L2: Replace tungsten GLS lamps with CFLs
C Recalculate carbon emissions
      if(ExistLightType)then
        WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-L2" = RECOMMENDATION'
        WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = LIGHTING'
        WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-L2'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
        WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Calculation for ENERGY-IMPACT is optional for detailed simulation programs
        WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT  = UNKNOWN'

C Recalculate carbon emissions
        BERORIGINAL=BER

C Call BERTER to recalculate BER using the Compact fluorescent lamps
        CALL BERTER(IER)

C Then determine the % change from this in TOTAL building emissions
        L2_impact=((BERORIGINAL-BER)/BERORIGINAL)*100

C set BER as for initial calculations
        BER=BERORIGINAL
        CALL BERTER(IER)

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low"
        CALL ECLOSE(L2_impact,4.0,0.001,ISNEAR)
        IF(L2_impact.GT.4.0)THEN
          WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
        ELSEIF((L2_impact.LT.4.0.OR.ISNEAR).AND.L2_impact.GT.0.5)
     &    THEN
          WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
        ELSE
          WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
        ENDIF

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
        WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
        WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
        WRITE(IUF,'(a)',iostat=ios) ' ..'
      endif
      do 711 icomp=1,ncomp

C Return the BERLIGHT back to the original values
        BERLIGHT(ICOMP)=LIGHTORIGINAL(ICOMP)
 711  continue
      ExistLightType=.false.
      do 712 icomp=1,ncomp
        if(ILITYP(ICOMP).eq.7)then
          ExistLightType=.true.

C REC-EPC-L3: Replace high-pressure mercury discharge lamps with
C plug-in SON replacements (HP sodium)
C EPC-L6 is the same but the payback period is supposed to differ
          LIGHTORIGINAL(ICOMP)=BERLIGHT(ICOMP)

C << This is hardwired here and it is taken from the SBEM database for
C high-pressure sodium: 4.5 W/m2 per 100 lux divided by
C high-pressure mercury discharge lamps: 7.6 W/m2 per 100 lux
          BERLIGHT(ICOMP)=(4.5/7.6)*BERLIGHT(ICOMP)
        endif
 712  continue

C REC-EPC-L3: Replace high-pressure mercury discharge lamps with
C plug-in SON replacements (HP sodium)
C EPC-L6 is the same but the payback period is supposed to differ
      if(ExistLightType)then
        WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-L3" = RECOMMENDATION'
        WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = LIGHTING'
        WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-L3'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
        WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Calculation for ENERGY-IMPACT is optional for detailed simulation programs
        WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT  = UNKNOWN'

C Recalculate carbon emissions
        BERORIGINAL=BER

C Call BERTER to recalculate BER using the Compact fluorescent lamps
        CALL BERTER(IER)

C Then determine the % change from this in TOTAL building emissions
        L3_impact=((BERORIGINAL-BER)/BERORIGINAL)*100

C set BER as for initial calculations
        BER=BERORIGINAL
        CALL BERTER(IER)

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low"
        CALL ECLOSE(L3_impact,4.0,0.001,ISNEAR)
        IF(L3_impact.GT.4.0)THEN
          WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
        ELSEIF((L3_impact.LT.4.0.OR.ISNEAR).AND.L3_impact.GT.0.5)
     &    THEN
          WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
        ELSE
          WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
        ENDIF

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
        WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
        WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
        WRITE(IUF,'(a)',iostat=ios) ' ..'

C Because EPC-L6 is the same as EPC-L3
C REC-EPC-L6: Replace high-pressure mercury discharge lamps with
C complete new lamp/gear SON DL (HP sodium)
C EPC-L3 is the same but the payback period is supposed to differ
        WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-L6" = RECOMMENDATION'
        WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = LIGHTING'
        WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-L6'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
        WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Calculation for ENERGY-IMPACT is optional for detailed simulation programs
        WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT  = UNKNOWN'

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low"
        CALL ECLOSE(L3_impact,4.0,0.001,ISNEAR)
        IF(L3_impact.GT.4.0)THEN
          WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
        ELSEIF((L3_impact.LT.4.0.OR.ISNEAR).AND.L3_impact.GT.0.5)
     &    THEN
          WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
        ELSE
          WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
        ENDIF

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
        WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
        WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
        WRITE(IUF,'(a)',iostat=ios) ' ..'
      endif
      do 713 icomp=1,ncomp

C Return the BERLIGHT back to the original values
        BERLIGHT(ICOMP)=LIGHTORIGINAL(ICOMP)
 713  continue
      ExistLightType=.false.
      do 714 icomp=1,ncomp
        if(ILITYP(ICOMP).eq.9)then
          ExistLightType=.true.

C REC-EPC-L4: Replace tungsten GLS spotlights with low voltage tungsten halogen
C iSBEM uses Metal halide for low voltage tungsten halogen. This is used here
C too. Recalculate carbon emissions
          LIGHTORIGINAL(ICOMP)=BERLIGHT(ICOMP)

C This is hardwired here and it is taken from the SBEM database for
C Metal halide: 5.5 W/m2 per 100 lux divided by
c Tungsten lamp: 28.0 W/m2 per 100 lux
          BERLIGHT(ICOMP)=(5.5/28.0)*BERLIGHT(ICOMP)
        endif
 714  continue

C REC-EPC-L4: Replace tungsten GLS spotlights with low voltage tungsten halogen
C Recalculate carbon emissions
      if(ExistLightType)then
        WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-L4" = RECOMMENDATION'
        WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = LIGHTING'
        WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-L4'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
        WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Calculation for ENERGY-IMPACT is optional for detailed simulation programs
        WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT  = UNKNOWN'

C Recalculate carbon emissions
        BERORIGINAL=BER

C Call BERTER to recalculate BER using the new T8 lamps
        CALL BERTER(IER)

C Then determine the % change from this in TOTAL building emissions
        L4_impact=((BERORIGINAL-BER)/BERORIGINAL)*100

C set BER as for initial calculations
        BER=BERORIGINAL
        CALL BERTER(IER)

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low"
        CALL ECLOSE(L4_impact,4.0,0.001,ISNEAR)
        IF(L4_impact.GT.4.0)THEN
          WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
        ELSEIF((L4_impact.LT.4.0.OR.ISNEAR).AND.L4_impact.GT.0.5)
     &    THEN
          WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
        ELSE
          WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
        ENDIF

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
        WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
        WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
        WRITE(IUF,'(a)',iostat=ios) ' ..'
      endif
      do 715 icomp=1,ncomp

C Return the BERLIGHT back to the original values
        BERLIGHT(ICOMP)=LIGHTORIGINAL(ICOMP)
 715  continue
      ExistLightType=.false.
      do 716 icomp=1,ncomp

C Check if T8 lamps are used
C for type 4 there will be no improvement according to SBEM.db1 database
C Type 4: "T8 (25mm dia) triphosphor coated fluorescent tube high freq ballast"
C Type 4 will be ignored here as it is the same in SBEM.db1 with T5 in terms of
C Watts/m2 per 100 lux
        if(ILITYP(ICOMP).eq.2.or.ILITYP(ICOMP).eq.3.or.
     &     ILITYP(ICOMP).eq.4)then
          ExistLightType=.true.

C REC-EPC-L5: Replace T8 lamps with retrofit T5 conversion kit
          LIGHTORIGINAL(ICOMP)=BERLIGHT(ICOMP)
          if(ILITYP(ICOMP).eq.2)then

C << This is hardwired here and it is taken from the SBEM database for
C T5: 3.4 W/m2 per 100 lux divided by
C T8 standard ballast: 4.4 W/m2 per 100 lux
            BERLIGHT(ICOMP)=(3.4/4.4)*BERLIGHT(ICOMP)
          elseif(ILITYP(ICOMP).eq.3)then

C << This is hardwired here and it is taken from the SBEM database for
C T5: 3.4 W/m2 per 100 lux divided by
C T8 halophosphate coated fluorescent tube high freq ballast: 3.8 W/m2 per 100 lux
            BERLIGHT(ICOMP)=(3.4/3.8)*BERLIGHT(ICOMP)
          endif
        endif
 716  continue

C REC-EPC-L5: Replace T8 lamps with retrofit T5 conversion kit
      if(ExistLightType)then
        WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-L5" = RECOMMENDATION'
        WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = LIGHTING'
        WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-L5'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
        WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Calculation for ENERGY-IMPACT is optional for detailed simulation programs
        WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT  = UNKNOWN'

C << Need a new calculation for a new CO2 by using instead T5 lamps
C Recalculate carbon emissions
        BERORIGINAL=BER

C Call BERTER to recalculate BER using T5 lamps
        CALL BERTER(IER)

C Then determine the % change from this in TOTAL building emissions
        L5_impact=((BERORIGINAL-BER)/BERORIGINAL)*100

C set BER as for initial calculations
        BER=BERORIGINAL
        CALL BERTER(IER)

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low"
        CALL ECLOSE(L5_impact,4.0,0.001,ISNEAR)
        IF(L5_impact.GT.4.0)THEN
          WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
        ELSEIF((L5_impact.LT.4.0.OR.ISNEAR).AND.L5_impact.GT.0.5)
     &    THEN
          WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
        ELSE
          WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
        ENDIF

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
        WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
        WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
        WRITE(IUF,'(a)',iostat=ios) ' ..'
      endif
      do 717 icomp=1,ncomp

C Return the BERLIGHT back to the original values
        BERLIGHT(ICOMP)=LIGHTORIGINAL(ICOMP)
 717  continue
      ExistLightType=.false.
      do 718 icomp=1,ncomp

C Check if T8 standard ballast and replace with high frequency
        if(ILITYP(ICOMP).eq.2)then
          ExistLightType=.true.

C REC-EPC-L7: Introduce High Frequency ballasts for fluorescent tubes:
C Reduce number of fittings required
          LIGHTORIGINAL(ICOMP)=BERLIGHT(ICOMP)

C << This is hardwired here and it is taken from the SBEM database for
C T8 (25mm dia) triphosphor coated fluorescent tube high
C freq ballast: 3.4 W/m2 per 100 lux divided by T8 standard
C ballast: 4.4 W/m2 per 100 lux
          BERLIGHT(ICOMP)=(3.4/4.4)*BERLIGHT(ICOMP)
        endif
 718  continue

C REC-EPC-L7: Introduce High Frequency ballasts for fluorescent tubes:
C Reduce number of fittings required
      if(ExistLightType)then
        WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-L7" = RECOMMENDATION'
        WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = LIGHTING'
        WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-L7'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
        WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Calculation for ENERGY-IMPACT is optional for detailed simulation programs
        WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT  = UNKNOWN'

C Need a new calculation for BER by using instead T8 high frequency lamps
C Recalculate carbon emissions
        BERORIGINAL=BER

C Call BERTER to recalculate BER using T8 high frequency lamps
        CALL BERTER(IER)

C Then determine the % change from this in TOTAL building emissions
        L7_impact=((BERORIGINAL-BER)/BERORIGINAL)*100

C set BER as for initial calculations
        BER=BERORIGINAL
        CALL BERTER(IER)

C If change in total carbon emission is >4% then potential impact is "high"
C If change in total carbon emission is greater than 0.5 and
C less or equal than 4% then potential impact is "medium"
C Otherwise impact is "low"
        CALL ECLOSE(L7_impact,4.0,0.001,ISNEAR)
        IF(L7_impact.GT.4.0)THEN
          WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','HIGH'
        ELSEIF((L7_impact.LT.4.0.OR.ISNEAR).AND.L7_impact.GT.0.5)
     &    THEN
          WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','MEDIUM'
        ELSE
          WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'
        ENDIF

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
        WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
        WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
        WRITE(IUF,'(a)',iostat=ios) ' ..'
      endif
      do 719 icomp=1,ncomp

C Return the BERLIGHT back to the original values
        BERLIGHT(ICOMP)=LIGHTORIGINAL(ICOMP)
 719  continue

C At the end: reset the ExistLightType logical to false
      ExistLightType=.false.

C ....ADD RENEWABLES.........
C Loop through renewable systems
C << need the DO LOOP for this
C << NEED a check to see if heating fuel is electricity
C << IF heating fuel is electricity then check heat generator
C << efficiency. If efficiency is less than 2 then EPC-R1

C REC-EPC-R1: Consider Ground Source Heat Pump
      WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-R1" = RECOMMENDATION'
      WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = RENEWABLES'
      WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-R1'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
      WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C << Need a new heating calculation here (and if appropriate DHW) as
C << a ratio between actual efficiency and 3.1
C << This needs then to be used and calculate total BER CO2 emissions
C << Then determine the % change from this in TOTAL building emissions
C << If change in total carbon emission is >4% then potential impact is "high"
C << If change in total carbon emission is greater than 0.5 and
C << less or equal than 4% then potential impact is "medium"
C << Otherwise impact is "low"
      WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ','LOW'
      WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'

C For the next line: Instead of CALC, it could also be USER
      WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
      WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
      WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
      WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
      WRITE(IUF,'(a)',iostat=ios) ' ..'

C << NEED A CHECK TO SEE IF THERE IS WIND TURBINE
C REC-EPC-R2: Consider installing building mounted wind turbine
      WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-R2" = RECOMMENDATION'
      WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = RENEWABLES'
      WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-R2'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
      WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Energy impact is always "low"
      WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ','LOW'
C Carbon impact is always "low"
      WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'

C For the next line: Instead of CALC, it could also be USER
      WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
      WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
      WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
      WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
      WRITE(IUF,'(a)',iostat=ios) ' ..'

C << NEED A CHECK TO SEE IF THERE IS SOLAR THERMAL WATER HEATING
C REC-EPC-R3: Consider installing solar water heating
      WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-R3" = RECOMMENDATION'
      WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = RENEWABLES'
      WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-R3'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
      WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Energy impact is always "low"
      WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ','LOW'

C Carbon impact is always "low"
      WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'

C For the next line: Instead of CALC, it could also be USER
      WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
      WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
      WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
      WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
      WRITE(IUF,'(a)',iostat=ios) ' ..'

C << NEED A CHECK TO SEE IF THERE IS PV
C REC-EPC-R4: Consider installing PV
      WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-R4" = RECOMMENDATION'
      WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = RENEWABLES'
      WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-R4'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
      WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C Energy impact is always "low"
      WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ','LOW'

C Carbon impact is always "low"
      WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'

C For the next line: Instead of CALC, it could also be USER
      WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
      WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
      WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
      WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
      WRITE(IUF,'(a)',iostat=ios) ' ..'

C << NEED a check to see if heating fuel is electricity
C << IF heating fuel is electricity then check heat generator
C << efficiency. If efficiency is less than 2 then EPC-R1

C REC-EPC-R5: Consider Air Source Heat Pump
      WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-R5" = RECOMMENDATION'
      WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = RENEWABLES'
      WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-R5'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
      WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'

C << Need a new heating calculation here (and if appropriate DHW) as
C << a ratio between actual efficiency and 2.2
C << This needs then to be used and calculate total BER CO2 emissions
C << Then determine the % change from this in TOTAL building emissions
C << If change in total carbon emission is >4% then potential impact is "high"
C << If change in total carbon emission is greater than 0.5 and
C << less or equal than 4% then potential impact is "medium"
C << Otherwise impact is "low"
      WRITE(IUF,'(2a)',iostat=ios) 'ENERGY-IMPACT = ','LOW'
      WRITE(IUF,'(2a)',iostat=ios) 'CO2-IMPACT = ','LOW'

C For the next line: Instead of CALC, it could also be USER
      WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
      WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
      WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
      WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
      WRITE(IUF,'(a)',iostat=ios) ' ..'

C...Overheating
C << NEED A LOOP THROUGH THE ZONES TO CHECK THEIR OVERHEATING
C << IF OVERHEATS, trigger EPC-V1
C REC-EPC-V1: Significant risk of overheating. Consider solar control
C measures such as the application of reflective coating or shading
C devices to windows
      WRITE(IUF,'(a)',iostat=ios) '"REC-EPC-V1" = RECOMMENDATION'
      WRITE(IUF,'(a)',iostat=ios) 'CATEGORY     = OVERHEATING'
      WRITE(IUF,'(a)',iostat=ios) 'CODE         = EPC-V1'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
      WRITE(IUF,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'


C << Need a new heating calculation here (and if appropriate DHW) as
C << a ratio between actual efficiency and 2.2
C << This needs then to be used and calculate total BER CO2 emissions
C << Then determine the % change from this in TOTAL building emissions
C << If change in total carbon emission is >4% then potential impact is "high"
C << If change in total carbon emission is greater than 0.5 and
C << less or equal than 4% then potential impact is "medium"
C << Otherwise impact is "low"

C For the next line: Instead of CALC, it could also be USER
      WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
      WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
      WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
      WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
      WRITE(IUF,'(a)',iostat=ios) ' ..'

C...Envelope
C REC-EPC-E1: Floors are poorly insulated
C if U value of floor > 1.0 then EPC-E1

      if(flor_max_u.gt.1.0)then
        write(iuf,'(a)',iostat=ios) '"REC-EPC-E1" = RECOMMENDATION'
        write(iuf,'(a)',iostat=ios) 'CATEGORY     = ENVELOPE'
        write(iuf,'(a)',iostat=ios) 'CODE         = EPC-E1'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
        write(iuf,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'
        write(iuf,'(a)',iostat=ios) 'ENERGY-IMPACT = UNKNOWN'

C << The calculation for this is basic and it is based on a very confusing table
C inside the recommendations document: Depends on what proportion of the
C overall energy consumption heating is accounted for.
C If U greater than 2.5 then high. All other cases (1.0<U<1.5) is low
        if(flor_max_u.gt.1.5.and.flor_max_u.lt.2.5)then
          write(iuf,'(a)',iostat=ios) 'CO2-IMPACT = MEDIUM'
        elseif(flor_max_u.gt.2.5)then
          write(iuf,'(a)',iostat=ios) 'CO2-IMPACT = HIGH'
        else
          write(iuf,'(a)',iostat=ios) 'CO2-IMPACT = LOW'
        endif

C For the next line: Instead of CALC, it could also be USER
        write(iuf,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
        write(iuf,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
        write(iuf,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
        write(iuf,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
        write(iuf,'(a)',iostat=ios) ' ..'
      endif

C REC-EPC-E2: Roof(S) are poorly insulated
C if U value for flat roof > 1.0 then EPC-E2
      if(flat_ROOF_MAX_U.gt.1.0)then
        write(iuf,'(a)',iostat=ios) '"REC-EPC-E2" = RECOMMENDATION'
        write(iuf,'(a)',iostat=ios) 'CATEGORY     = ENVELOPE'
        write(iuf,'(a)',iostat=ios) 'CODE         = EPC-E2'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
        write(iuf,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'
        write(iuf,'(a)',iostat=ios) 'ENERGY-IMPACT = UNKNOWN'

C << The calculation for this is basic and it is based on a very confusing table
C inside the recommendations document: Depends on what proportion of the
C overall energy consumption heating is accounted for.
C For simplicity reasons: if U>1.5 and less than 2.5 then medium
C If U greater than 2.5 then high. All other cases (1.0<U<1.5) is low
        if(flat_ROOF_MAX_U.gt.1.5.and.flat_ROOF_MAX_U.lt.2.5)then
          write(iuf,'(a)',iostat=ios) 'CO2-IMPACT = MEDIUM'
        elseif(flat_ROOF_MAX_U.gt.2.5)then
          write(iuf,'(a)',iostat=ios) 'CO2-IMPACT = HIGH'
        else
          write(iuf,'(a)',iostat=ios) 'CO2-IMPACT = LOW'
        endif

C For the next line: Instead of CALC, it could also be USER
        write(iuf,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
        write(iuf,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
        write(iuf,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
        write(iuf,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
        write(iuf,'(a)',iostat=ios) ' ..'
      endif

C REC-EPC-E3: Solid walls are poorly insulated
C << EPC RECOMMENDATIONS DOCUMENT talks about "internal walls"
C << but it is probably a typo error
C if U value for solid walls > 1.0 then EPC-E3
      if(wall_max_u.gt.1.0)then
        write(iuf,'(a)',iostat=ios) '"REC-EPC-E3" = RECOMMENDATION'
        write(iuf,'(a)',iostat=ios) 'CATEGORY     = ENVELOPE'
        write(iuf,'(a)',iostat=ios) 'CODE         = EPC-E3'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
        write(iuf,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'
        write(iuf,'(a)',iostat=ios) 'ENERGY-IMPACT = UNKNOWN'

C << The calculation for this is basic and it is based on a very confusing table
C inside the recommendations document: Depends on what proportion of the
C << overall energy consumption heating is accounted for.
C For simplicity reasons: if U>1.5 and less than 2.5 then medium
C If U greater than 2.5 then high. All other cases (1.0<U<1.5) is low
        if(wall_max_u.gt.1.5.and.wall_max_u.lt.2.5)then
          write(iuf,'(a)',iostat=ios) 'CO2-IMPACT = MEDIUM'
        elseif(wall_max_u.gt.2.5)then
          write(iuf,'(a)',iostat=ios) 'CO2-IMPACT = HIGH'
        else
          write(iuf,'(a)',iostat=ios) 'CO2-IMPACT = LOW'
        endif

C For the next line: Instead of CALC, it could also be USER
        write(iuf,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
        write(iuf,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
        write(iuf,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
        write(iuf,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
        write(iuf,'(a)',iostat=ios) ' ..'

C REC-EPC-E4: Walls have uninsulated cavities - introduce cavity wall insulation
C if U value for cavity walls > 1.0 then EPC-E4
C There is no way to separate this and EPC-E3 - so trigger them together
C << SBEM seems to always trigger EPC-E4
        write(iuf,'(a)',iostat=ios) '"REC-EPC-E4" = RECOMMENDATION'
        write(iuf,'(a)',iostat=ios) 'CATEGORY     = ENVELOPE'
        write(iuf,'(a)',iostat=ios) 'CODE         = EPC-E4'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
        write(iuf,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'
        write(iuf,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'
        write(iuf,'(a)',iostat=ios) 'ENERGY-IMPACT = UNKNOWN'

C << The calculation for this is basic and it is based on a very confusing table
C inside the recommendations document: Depends on what proportion of the
C << overall energy consumption heating is accounted for.
C For simplicity reasons: if U>1.5 and less than 2.5 then medium
C If U greater than 2.5 then high. All other cases (1.0<U<1.5) is low
        if(wall_max_u.gt.1.5.and.wall_max_u.lt.2.5)then
          write(iuf,'(a)',iostat=ios) 'CO2-IMPACT = MEDIUM'
        elseif(wall_max_u.gt.2.5)then
          write(iuf,'(a)',iostat=ios) 'CO2-IMPACT = HIGH'
        else
          write(iuf,'(a)',iostat=ios) 'CO2-IMPACT = LOW'
        endif

C For the next line: Instead of CALC, it could also be USER
        write(iuf,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
        write(iuf,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
        write(iuf,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
        write(iuf,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
        write(iuf,'(a)',iostat=ios) ' ..'
      endif

C REC-EPC-E5: Some windows have high U values, consider installing secondary
C glazing
C if U value for windows > 3.5 then EPC-E5
C EPC-E5 AND EPC-E8 are triggered together
      if(wnrf_max_u.gt.3.5)then
        write(iuf,'(a)',iostat=ios) '"REC-EPC-E5" = RECOMMENDATION'
        write(iuf,'(a)',iostat=ios) 'CATEGORY     = ENVELOPE'
        write(iuf,'(a)',iostat=ios) 'CODE         = EPC-E5'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
        write(iuf,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'
        write(iuf,'(a)',iostat=ios) 'ENERGY-IMPACT = UNKNOWN'

C << The calculation for this is basic and it is based on a very confusing table
C inside the recommendations document: Depends on what proportion of the
C overall energy consumption heating is accounted for.
C For simplicity reasons: if U>3.5 and less than 4.0 then medium
C If U greater than 4.5 then high. All other cases (4.0<U<4.5) is low
        if(wnrf_max_u.gt.4.0.and.wnrf_max_u.lt.4.5)then
          write(iUF,'(a)',iostat=ios) 'CO2-IMPACT = MEDIUM'
        elseif(wnrf_max_u.gt.4.5)then
          write(iUF,'(a)',iostat=ios) 'CO2-IMPACT = HIGH'
        else
          write(iUF,'(a)',iostat=ios) 'CO2-IMPACT = LOW'
        endif

C For the next line: Instead of CALC, it could also be USER
        write(iuf,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
        write(iuf,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
        write(iuf,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
        write(iuf,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
        write(iuf,'(a)',iostat=ios) ' ..'

C REC-EPC-E8: Some windows have high U values, improve glazing/frames
C if U value for windows > 3.5 then EPC-E8
C EPC-E5 AND EPC-E8 are triggered together
        write(iuf,'(a)',iostat=ios) '"REC-EPC-E8" = RECOMMENDATION'
        write(iuf,'(a)',iostat=ios) 'CATEGORY     = ENVELOPE'
        write(iuf,'(a)',iostat=ios) 'CODE         = EPC-E8'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
        write(iuf,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'
        write(iuf,'(a)',iostat=ios) 'ENERGY-IMPACT = UNKNOWN'

C << The calculation for this is basic and it is based on a very confusing table
C inside the recommendations document: Depends on what proportion of the
C overall energy consumption heating is accounted for.
C For simplicity reasons: if U>3.5 and less than 4.0 then medium
C If U greater than 4.5 then high. All other cases (4.0<U<4.5) is low
        if(wnrf_max_u.gt.4.0.and.wnrf_max_u.lt.4.5)then
          write(iUF,'(a)',iostat=ios) 'CO2-IMPACT = MEDIUM'
        elseif(wnrf_max_u.gt.4.5)then
          write(iUF,'(a)',iostat=ios) 'CO2-IMPACT = HIGH'
        else
          write(iUF,'(a)',iostat=ios) 'CO2-IMPACT = LOW'
        endif

C For the next line: Instead of CALC, it could also be USER
        write(iuf,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
        write(iuf,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
        write(iuf,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
        write(iuf,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
        write(iuf,'(a)',iostat=ios) ' ..'
      endif

C REC-EPC-E6: Poorly insulated pitched roofs with lofts
C If U value of pitched roof > 1.0 then EPC-E6
      if(pitched_ROOF_MAX_U.gt.1.0)then
        write(iuf,'(a)',iostat=ios) '"REC-EPC-E6" = RECOMMENDATION'
        write(iuf,'(a)',iostat=ios) 'CATEGORY     = ENVELOPE'
        write(iuf,'(a)',iostat=ios) 'CODE         = EPC-E6'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
        write(iuf,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'
        write(iuf,'(a)',iostat=ios) 'ENERGY-IMPACT = UNKNOWN'

C << The calculation for this is basic and it is based on a very confusing table
C inside the recommendations document: it depends on what proportion of the
C overall energy consumption heating is accounted for.
C For simplicity reasons: if U>1.5 and less than 2.5 then medium
C If U greater than 2.5 then high. All other cases (1.0<U<1.5) is low
        if(pitched_ROOF_MAX_U.gt.1.5.and.pitched_ROOF_MAX_U.lt.2.5)then
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT = MEDIUM'
        elseif(pitched_ROOF_MAX_U.gt.2.5)then
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT = HIGH'
        else
          WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT = LOW'
        endif

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
        WRITE(IUF,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
        WRITE(IUF,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
        WRITE(IUF,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
        WRITE(IUF,'(a)',iostat=ios) ' ..'
      endif

C REC-EPC-E7: Improve air leakage
C if air permeability > 14 m^3/h (at 50Pa) then EPC-E7
      if(binf50.gt.14.0)then
        write(iuf,'(a)',iostat=ios) '"REC-EPC-E7" = RECOMMENDATION'
        write(iuf,'(a)',iostat=ios) 'CATEGORY     = ENVELOPE'
        write(iuf,'(a)',iostat=ios) 'CODE         = EPC-E7'

C << There are also options for the next line to be a user input recommendation
C << or that the user has exluded it. keywords for this: USER-IN and USER-OUT
        write(iuf,'(a)',iostat=ios) 'REC-SOURCE   = CALC-IN'
        write(iuf,'(a)',iostat=ios) 'ENERGY-IMPACT = UNKNOWN'

C Documentation is confusing: it points to a table that was used for
C the U values without any further explanation
C For simplicity: if binf50 greater than 15.0 and less than 16 then medium
C If binf50 greater than 16 then high. All other cases (14<U<15) is low
        if(binf50.gt.15.0.and.binf50.lt.16.0)then
          write(IUF,'(a)',iostat=ios) 'CO2-IMPACT = MEDIUM'
        elseif(binf50.gt.16.0)then
          write(IUF,'(a)',iostat=ios) 'CO2-IMPACT = HIGH'
        else
          write(IUF,'(a)',iostat=ios) 'CO2-IMPACT = LOW'
        endif

C For the next line: Instead of CALC, it could also be USER
        write(iuf,'(a)',iostat=ios) 'ENERGY-IMPACT-SC = CALC'

C For the next line: Instead of CALC, it could also be USER
        write(iuf,'(a)',iostat=ios) 'CO2-IMPACT-SC = CALC'

C For the next line: Instead of USER, it could also be CALC
        write(iuf,'(a)',iostat=ios) 'PAY-BACK-SC = USER'

C For the next line: Instead of USER, it could also be CALC
        write(iuf,'(a)',iostat=ios) 'CO2-SAVE-POUND-SC = USER'
        write(iuf,'(a)',iostat=ios) ' ..'
      endif

C ***ACTUAL BUILDING (should be as in BRUKL)
      WRITE(IUF,'(a)',iostat=ios) '"BUILDING_DATA" = BUILDING-DATA'
      WRITE(IUF,'(a)',iostat=ios)
     &  '  ANALYSIS                  =  ACTUAL'

      WRITE(IUF,'(a,f8.2)',iostat=ios)
     &  '  AREA                      = ',tot_floor_area

      WRITE(IUF,'(a,f8.2)',iostat=ios)
     &  '  AREA-EXT                  = ',EXP_AREA

C This will have to be based on the location of the study
      WRITE(IUF,'(a)',iostat=ios) '  WEATHER                   =  LON'

C Confirm that BINF50 instead of some sort of "translated" air changes
      WRITE(IUF,'(a,F5.2)',iostat=ios)
     &  '  Q50-INF                   = ',BINF50

C BRUKL Manual defines this as the "building's average thermal conductance
C through all external/exposed envelopes"
C If users do not enter the thermal bridges menu the uavtotal will be 0 and
C the overall build_UA will be less than usual
      WRITE(IUF,'(a,f9.2)',iostat=ios)
     &  '  BUILDING-W/K              = ',build_UA

C BRUKL Manual defines this as the "building's average heat transfer
C coefficient through all external/exposed envelopes"
      call eclose(EXP_AREA,0.0,0.0001,ISNEAR)
      if(.not.ISNEAR)then
        WRITE(IUF,'(a,f9.2)',iostat=ios)
     &  '  BUILDING-W/M2K            = ',build_UA/EXP_AREA
      else
        WRITE(IUF,'(a)',iostat=ios)
     &  '  BUILDING-W/M2K            =  0.0'
      endif

C Using info from common of thermal bridges to find the percentage of heat loss
C due to thermal bridges out of the overall heat loss from the envelope.
      call eclose(build_UA,0.0,0.001,ISNEAR)
      if(ISNEAR)then      
        b_alpha=0.0
      else
        b_alpha=(b_totheatloss/build_UA)*100.0
      endif      
      WRITE(IUF,'(a,f6.2)',iostat=ios)
     &  '  BUILDING-ALPHA            = ',b_alpha

C Calculate loads for heating, cooling, DHW etc.
      WRITE(IUF,'(a,f9.2)',iostat=ios)
     &  '  KWH/M2-HEAT               = ',ABS(BER_HEAT)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-COOL               = ',ABS(BER_COOL)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-AUX                = ',BER_AUX
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-LIGHT              = ',BER_LIGHT
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-DHW                = ',BER_DHW

C?? To enter total casual gains/m2 here
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  ' KWH/M2-EQUP             = ',0.0

      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  ' KWH/M2-NATGAS           = ',BERF(1)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  ' KWH/M2-LPG              = ',BERF(2)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  ' KWH/M2-BIOGAS           = ',BERF(3)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  ' KWH/M2-OIL              = ',BERF(4)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  ' KWH/M2-COAL             = ',BERF(5)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  ' KWH/M2-ANTHRACITE       = ',BERF(6)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  ' KWH/M2-SMOKELESS        = ',BERF(7)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  ' KWH/M2-DUELFUEL         = ',BERF(8)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  ' KWH/M2-BIOMASS          = ',BERF(9)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  ' KWH/M2-SUPELEC          = ',ABS(BERF(10))
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  ' KWH/M2-WASTEHEAT        = ',BERF(12)

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-DISTRICT-HEATING = ',' 0.0'

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-DISP             = ',' 0.0'

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-PVS              = ',' 0.0'

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-WIND             = ',' 0.0'

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-CHP              = ',' 0.0'

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-SES              = ',' 0.0'

C Activity number and area per zone
C iactFlarea is a counter to prepare the act_area for the arlist call
C This act_area has to have maximum size 2 times the zone number, using
C ITWOMNS here because it is defined in sbem.h as two times the maximum
C number of activities
      iactFlarea=0
      do 18 iz=1,ncomp
        iactFlarea=iactFlarea+1
        act_area(iactFlarea)=real(theactivityindex(iz))
        iactFlarea=iactFlarea+1
        act_area(iactFlarea)= zbasea(iz)
 18   continue
      itrunc=1
      ipos=1
      delim='C'
      do while (itrunc.ne.0)
        call arlist(ipos,iactFlarea,act_area,ITWOMNS,delim,louts,
     &      loutln,itrunc)

C here we write each activity and the related floor area. SBEM seems to sum up the
C floor areas and writes the activity once if it covers many zones.
C BRUKL v3.2 seems to accept either ways.
        WRITE(IUF,'(4a)',iostat=ios) ' ACT-AREA                = ',
     &   '{ ',louts(1:loutln),' }'
        ipos=itrunc+1
      end do
      WRITE(IUF,'(a)',iostat=ios) ' ..'

C Section for each HVAC system
      do 101 ICMSYS=1,NCMSYS
        hvacarea(ICMSYS)=0.
        IhvAct=0
        imerg=0
        do 15 iz=1,ncomp
          if(ICMSYS.eq.IHLZ(iz))then
            IhvAct=IhvAct+1
            zhvacarea(ICMSYS,IhvAct)=0.
            hvacarea(ICMSYS)=zbasea(iz)+hvacarea(ICMSYS)
            zhvacarea(ICMSYS,IhvAct)= zbasea(iz)
            Act_HVAC(ICMSYS,IhvAct)=theactivityindex(iz)
          endif
 15     continue
        if((hvacarea(ICMSYS)).gt.0.)then
          TMPSTR2=HVACNAME(ICMSYS)
          LNB=lnblnk(TMPSTR2)
          WRITE(IUF,'(4a)',iostat=ios) '"',TMPSTR2(1:LNB),
     &     '"',' = HVAC-SYSTEM-DATA'
          WRITE(IUF,'(a,f8.2)',iostat=ios)
     &      '  AREA                      = ',hvacarea(ICMSYS)
          LNB=lnblnk(HSYSNAME(INCMSYS(ICMSYS)))
          WRITE(IUF,'(2a)',iostat=ios)
     &      '  TYPE                      = ',
     &      HSYSNAME(INCMSYS(ICMSYS))(1:LNB)
          LNB=lnblnk(TEMPZS(ICMSYS))
          WRITE(IUF,'(2a)',iostat=ios)
     &      '  HEAT-SOURCE               = ',
     &      TEMPZS(ICMSYS)(1:LNB)
          LNB=lnblnk(FUELNAME(IFTYP(ICMSYS)))
          WRITE(IUF,'(2a)',iostat=ios)
     &      '  FUEL-TYPE                 = ',
     &      FUELNAME(IFTYP(ICMSYS))(1:LNB)

C Next line is required even if there is no cooling in the system.
          WRITE(IUF,'(a)',iostat=ios)
     &      '  FUEL-TYPE-COOL            = Grid Supplied Electricity'
          BERSH_DEMAND(ICMSYS)=BERSH(ICMSYS)*HGEF(ICMSYS)
          WRITE(IUF,'(a,F10.2)',iostat=ios)
     &      '  MJ/M2-HEAT-DEM            = ',ABS(BERSH_DEMAND(ICMSYS)*
     &      3.6)
          BERSC_DEMAND(ICMSYS)=BERSC(ICMSYS)*CGEF(ICMSYS)
          WRITE(IUF,'(a,F10.2)',iostat=ios)
     &      '  MJ/M2-COOL-DEM            = ',
     &      ABS(BERSC_DEMAND(ICMSYS)*3.6)
          WRITE(IUF,'(a,F10.2)',iostat=ios)
     &      '  KWH/M2-HEAT               = ',ABS(BERSH(ICMSYS))
          WRITE(IUF,'(a,F10.2)',iostat=ios)
     &      '  KWH/M2-COOL               = ',ABS(BERSC(ICMSYS))
          WRITE(IUF,'(a,F10.2)',iostat=ios)
     &      '  KWH/M2-AUX                = ',BERSA(ICMSYS)

C?? To add system efficienies for heating and cooling which depend upon
C heat/cool generator efficiencies and system adjustments (SFP, M&T,
C ductwork leakage and AHU leakage). Exact relationship for all these
C parameters is unknown but should be available from BRE.
C If there is a heating system use the value, else the default is 0
          WRITE(IUF,'(a,F7.4)',iostat=ios)
     &      '  HEAT-SSEFF                = ',HGEF(ICMSYS) !?? Needs to be updated

C If there is a cooling system then:
          if(IBRUKLC(1,INCMSYS(ICMSYS)).ne.-1111)then
            WRITE(IUF,'(a,F7.4)',iostat=ios)
     &      '  COOL-SSEER                = ',CGEF(ICMSYS) !?? Needs to be updated
          endif
          WRITE(IUF,'(a,F7.4)',iostat=ios)
     &      '  HEAT-GEN-SEFF             = ',HGEF(ICMSYS)

C If there is a cooling system then:
          if(IBRUKLC(1,INCMSYS(ICMSYS)).ne.-1111)then
            WRITE(IUF,'(a,f7.4)',iostat=ios)
     &      '  COOL-GEN-SEER             = ',CGEF(ICMSYS)
          endif

C Now start the logic needed for ACT-AREA entry
          itrunc=1
          ipos=1
          delim='C'
          if(IhvAct.gt.0)then

C Loop through the number of activities that this hvac covers (for their spaces)
C This will prepare TotHVACactArea for the arlist routine
C imerg is used for merging activities and floor areas in order to prepare the
C the array for the call to arlist
            do 17 iloop_IhvAct=1,IhvAct
              imerg=imerg+1
              TotHVACactArea(imerg)=
     &          real(Act_HVAC(ICMSYS,iloop_IhvAct))
              imerg=imerg+1
              TotHVACactArea(imerg)=
     &          zhvacarea(ICMSYS,iloop_IhvAct)
 17         continue
          endif
          do while (itrunc.ne.0)
            call arlist(ipos,imerg,TotHVACactArea,ITWOMNS,delim,louts,
     &          loutln,itrunc)
            WRITE(IUF,'(4a)',iostat=ios) ' ACT-AREA                = ',
     &       '{ ',louts(1:loutln),' }'
            ipos=itrunc+1
          end do
          WRITE(IUF,'(a)',iostat=ios) ' ..'
        endif
 101  continue

C ***NOTIONAL BUILDING (should be as in BRUKL)
C Notional building related data
      WRITE(IUF,'(a)',iostat=ios) '"BUILDING_DATA" = BUILDING-DATA'
      WRITE(IUF,'(a)',iostat=ios)
     &  '  ANALYSIS                   =  NOTIONAL'
      tot_floor_area=0.0
      do 131 icomp=1, ncomp
        tot_floor_area=zbasea(icomp)+tot_floor_area
 131  continue
      WRITE(IUF,'(a,f8.2)',iostat=ios)
     &  '  AREA                       = ',tot_floor_area

      WRITE(IUF,'(a,f8.2)',iostat=ios)
     &  '  AREA-EXT                   = ',exp_area

C This will have to be based on the location of the study
      WRITE(IUF,'(a)',iostat=ios)
     &  '  WEATHER                    =  LON'

C Confirm that BINF50 instead of some sort of "translated" air changes
      WRITE(IUF,'(a)',iostat=ios)
     &  '  Q50-INF                    = 10'

C Hardwiring values here as they are not clear what they mean from the
C BRUKL manual
      WRITE(IUF,'(a,f9.2)',iostat=ios)
     &  '  BUILDING-W/K               = ',build_UAN
      call eclose(EXP_AREA,0.0,0.0001,ISNEAR)
      if(.not.ISNEAR)then
        WRITE(IUF,'(a,f10.2)',iostat=ios)
     &  '  BUILDING-W/M2K             = ',build_UAN/EXP_AREA
      else
        WRITE(IUF,'(a)',iostat=ios)
     &  '  BUILDING-W/M2K             =  0.0'
      endif


C Using info from common of thermal bridges to find the percentage of heat loss
C due to thermal bridges out of the overall heat loss from the envelope.
C Thermal bridges are always 10% for notional building
      b_alpha=10.0
      WRITE(IUF,'(a,f6.2)',iostat=ios)
     &  '  BUILDING-ALPHA             = ',b_alpha

C Calculate loads for heating, cooling, DHW etc.
      WRITE(IUF,'(a,f10.2)',iostat=ios)
     &  '  KWH/M2-HEAT                = ',ABS(AER_HEAT)
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  '  KWH/M2-COOL                = ',ABS(AER_COOL)
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  '  KWH/M2-AUX                 = ',AER_AUX
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  '  KWH/M2-LIGHT               = ',AER_LIGHT
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  '  KWH/M2-DHW                 = ',AER_DHW

C?? To enter total casual gains/m2 here
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  '  KWH/M2-EQUP                = ',0.0

      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  '  KWH/M2-NATGAS              = ',AERF(1)
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  '  KWH/M2-LPG                 = ',AERF(2)
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  '  KWH/M2-BIOGAS              = ',AERF(3)
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  '  KWH/M2-OIL                 = ',AERF(4)
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  '  KWH/M2-COAL                = ',AERF(5)
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  '  KWH/M2-ANTHRACITE          = ',AERF(6)
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  '  KWH/M2-SMOKELESS           = ',AERF(7)
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  '  KWH/M2-DUELFUEL            = ',AERF(8)
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  '  KWH/M2-BIOMASS             = ',AERF(9)
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  '  KWH/M2-SUPELEC             = ',ABS(AERF(10))
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  '  KWH/M2-WASTEHEAT           = ',AERF(12)

C Hardwiring values here as they have not yet been calculated
      TMPVAL=0.0  ! until there is something to write
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-DISTRICT-HEATING    = ',TMPVAL
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-DISP                = ',TMPVAL
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-PVS                 = ',TMPVAL
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-WIND                = ',TMPVAL
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-CHP                 = ',TMPVAL
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-SES                 = ',TMPVAL

C Activity number and area per zone
C iactFlarea is a counter to prepare the act_area for the arlist call
C This act_area has to have maximum size 2 times the zone number, using
C ITWOMNS here because it is defined in sbem.h as two times the maximum
C number of activities
      iactFlarea=0
      do 19 iz=1,ncomp
        iactFlarea=iactFlarea+1
        act_area(iactFlarea)=real(theactivityindex(iz))
        iactFlarea=iactFlarea+1
        act_area(iactFlarea)= zbasea(iz)
 19   continue
      itrunc=1
      ipos=1
      delim='C'
      do while (itrunc.ne.0)
        call arlist(ipos,iactFlarea,act_area,ITWOMNS,delim,louts,
     &      loutln,itrunc)

C here we write each activity and the related floor area. SBEM seems to sum up the
C floor areas and writes the activity once if it covers many zones.
C BRUKL v3.2 seems to accept either ways.
        WRITE(IUF,'(4a)',iostat=ios) ' ACT-AREA                = ',
     &   '{ ',louts(1:loutln),' }'
        ipos=itrunc+1
      end do
      WRITE(IUF,'(a)',iostat=ios) ' ..'

C Section for each HVAC system for the notional building
      do 201 ICMSYS=1,NCMSYS
        hvacarea(ICMSYS)=0.
        IhvAct=0
        imerg=0
        do 225 iz=1,ncomp
          if(ICMSYS.eq.IHLZ(iz))then
            IhvAct=IhvAct+1
            zhvacarea(ICMSYS,IhvAct)=0.
            hvacarea(ICMSYS)=zbasea(iz)+hvacarea(ICMSYS)
            zhvacarea(ICMSYS,IhvAct)= zbasea(iz)
            Act_HVAC(ICMSYS,IhvAct)=theactivityindex(iz)
          endif
 225    continue
        if((hvacarea(ICMSYS)).gt.0.)then
          TMPSTR2=HVACNAME(ICMSYS)

C Same as actual building
          WRITE(IUF,'(4a)',iostat=ios) '"',TMPSTR2(1:LNBLNK(TMPSTR2)),
     &     '"',' = HVAC-SYSTEM-DATA'

C Same as actual building
          WRITE(IUF,'(a,f8.2)',iostat=ios)
     &      '  AREA                      = ',hvacarea(ICMSYS)

C Same as actual building
          LNB=lnblnk(HSYSNAME(INCMSYS(ICMSYS)))
          WRITE(IUF,'(2a)',iostat=ios)
     &      '  TYPE                      = ',
     &      HSYSNAME(INCMSYS(ICMSYS))(1:lnb)

C Same as actual building
          LNB=lnblnk(TEMPZS(ICMSYS))
          WRITE(IUF,'(2a)',iostat=ios)
     &      '  HEAT-SOURCE               = ',
     &      TEMPZS(ICMSYS)(1:lnb)

C Notional building uses either gas or oil as worked out in BERTER and
C holds index number as NHF
          LNB=lnblnk(FUELNAME(NHF))
          WRITE(IUF,'(2a)',iostat=ios)
     &      '  FUEL-TYPE                 = ',FUELNAME(NHF)(1:lnb)
          WRITE(IUF,'(a)',iostat=ios)
     &      '  FUEL-TYPE-COOL            = Grid Supplied Electricity'
          AERSH_DEMAND(ICMSYS)=AERSH(ICMSYS)*HGEF(ICMSYS)
          WRITE(IUF,'(a,F10.2)',iostat=ios)
     &      '  MJ/M2-HEAT-DEM            = ',ABS(AERSH_DEMAND(ICMSYS)*
     &      3.6)
          AERSC_DEMAND(ICMSYS)=AERSC(ICMSYS)*CGEF(ICMSYS)
          WRITE(IUF,'(a,F10.2)',iostat=ios)
     &      '  MJ/M2-COOL-DEM            = ',
     &      ABS(AERSC_DEMAND(ICMSYS)*3.6)
          WRITE(IUF,'(a,F10.2)',iostat=ios)
     &      '  KWH/M2-HEAT               = ',ABS(AERSH(ICMSYS))
          WRITE(IUF,'(a,F10.2)',iostat=ios)
     &      '  KWH/M2-COOL               = ',ABS(AERSC(ICMSYS))
          WRITE(IUF,'(a,F10.2)',iostat=ios)
     &      '  KWH/M2-AUX                = ',AERSA(ICMSYS)

C This is based on what SBEM is using. Additional details
C can be found at NCM modelling guide vers. 2e Table6)
C However, SBEM v3.2b does not implement these values as this table
C in the modelling guide. It does not have for example a case where
C there is air-conditioning.
C If there is no ooling system:
          if(IBRUKLC(1,INCMSYS(ICMSYS)).eq.-1111)then

C then only heated space.
            WRITE(IUF,'(a,f6.2)',iostat=ios)
     &      '  HEAT-SSEFF               = ',HeatSCoP_N(ICMSYS)

C Treat all the other cases as heated and mechanically ventilated
          else
            WRITE(IUF,'(a,f6.2)',iostat=ios)
     &      '  HEAT-SSEFF               = ',HeatSCoP_N(ICMSYS)
            WRITE(IUF,'(a,f6.2)',iostat=ios)
     &      '  COOL-SSEER               = ',CoolSSEER_N(ICMSYS)
          endif

C Now start the logic needed for ACT-AREA entry
          itrunc=1
          ipos=1
          delim='C'
          if(IhvAct.gt.0)then

C Loop through the number of activities that this hvac covers (for their spaces)
C This will prepare TotHVACactArea for the arlist routine
C imerg is used for merging activities and floor areas in order to prepare the
C the array for the call to arlist
            do 197 iloop_IhvAct=1,IhvAct
              imerg=imerg+1
              TotHVACactArea(imerg)=
     &          real(Act_HVAC(ICMSYS,iloop_IhvAct))
              imerg=imerg+1
              TotHVACactArea(imerg)=
     &          zhvacarea(ICMSYS,iloop_IhvAct)
 197        continue
          endif
          do while (itrunc.ne.0)
            call arlist(ipos,imerg,TotHVACactArea,ITWOMNS,delim,louts,
     &          loutln,itrunc)
            WRITE(IUF,'(4a)',iostat=ios) ' ACT-AREA                = ',
     &       '{ ',louts(1:loutln),' }'
            ipos=itrunc+1
          end do
          WRITE(IUF,'(a)',iostat=ios) ' ..'
        endif
 201  continue

C ***REFERENCE BUILDING
C Reference building related data
      WRITE(IUF,'(a)',iostat=ios) '"BUILDING_DATA" = BUILDING-DATA'
      WRITE(IUF,'(a)',iostat=ios)
     &  '  ANALYSIS                  =  REFERENCE'
      WRITE(IUF,'(a,f8.2)',iostat=ios)
     &  '  AREA                      = ',tot_floor_area

      WRITE(IUF,'(a,f8.2)',iostat=ios)
     &  '  AREA-EXT                  = ',exp_area

C This will have to be based on the location of the study
      WRITE(IUF,'(a)',iostat=ios) ' WEATHER                  =  LON'

C Confirm that BINF50 instead of some sort of "translated" air changes
      WRITE(IUF,'(a)',iostat=ios)
     &  '  Q50-INF                   = 10'
      WRITE(IUF,'(a,f9.2)',iostat=ios)
     &  '  BUILDING-W/K              = ',build_UAN
      call eclose(EXP_AREA,0.0,0.0001,ISNEAR)
      if(.not.ISNEAR)then
        WRITE(IUF,'(a,f9.2)',iostat=ios)
     &  '  BUILDING-W/M2K            = ',build_UAN/EXP_AREA
      else
        WRITE(IUF,'(a)',iostat=ios)
     &  '  BUILDING-W/M2K            =  0.0'
      endif

C Using info from common of thermal bridges to find the percentage of heat loss
C due to thermal bridges out of the overall heat loss from the envelope
C If users do not enter the thermal bridges menu the uavtotal will be 0 and
C the overall build_UAN will be less than usual and therefore b_alpha will be
C greater than usual.
C Thermal bridges are always 10% for notional building
      call eclose(build_UAN,0.0,0.0001,ISNEAR)
      if(.not.ISNEAR)then
        b_alpha=(b_totheatloss/build_UAN)*100.0
      else
        b_alpha=0.0
      endif
      WRITE(IUF,'(a,f6.2)',iostat=ios)
     &  '  BUILDING-ALPHA            = ',b_alpha

C Calculate loads for heating, cooling, DHW etc.
      WRITE(IUF,'(a,f9.2)',iostat=ios)
     &  '  KWH/M2-HEAT               = ',ABS(RER_HEAT)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-COOL               = ',ABS(RER_COOL)
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-AUX                = ',RER_AUX
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-LIGHT              = ',AER_LIGHT
      WRITE(IUF,'(a,F9.2)',iostat=ios)
     &  '  KWH/M2-DHW                = ',RER_DHW

C << need equipment inform????? >>
C?? To enter total casual gains/m2 here
      WRITE(IUF,'(2a)',iostat=ios)
     &  '  KWH/M2-EQUP               = ',' 0.'

C Use the total of all fuels from notional (excluding electricity):
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  ' KWH/M2-NATGAS           = ',RERF(1)
      WRITE(IUF,'(a)',iostat=ios) ' KWH/M2-LPG              = 0.0'
      WRITE(IUF,'(a)',iostat=ios) ' KWH/M2-BIOGAS           = 0.0'
      WRITE(IUF,'(a)',iostat=ios) ' KWH/M2-OIL              = 0.0'
      WRITE(IUF,'(a)',iostat=ios) ' KWH/M2-COAL             = 0.0'
      WRITE(IUF,'(a)',iostat=ios) ' KWH/M2-ANTHRACITE       = 0.0'
      WRITE(IUF,'(a)',iostat=ios) ' KWH/M2-SMOKELESS        = 0.0'
      WRITE(IUF,'(a)',iostat=ios) ' KWH/M2-DUELFUEL         = 0.0'
      WRITE(IUF,'(a)',iostat=ios) ' KWH/M2-BIOMASS          = 0.0'
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  ' KWH/M2-SUPELEC           = ',ABS(RERF(10))

      WRITE(IUF,'(a)',iostat=ios) ' KWH/M2-WASTEHEAT        = 0.0'

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-DISTRICT-HEATING = ',' 0.0'

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-DISP             = ',' 0.0'

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-PVS              = ',' 0.0'

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-WIND             = ',' 0.0'

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-CHP              = ',' 0.0'

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-SES              = ',' 0.0'

C Activity number and area per zone
C iactFlarea is a counter to prepare the act_area for the arlist call
C This act_area has to have maximum size 2 times the zone number, using
C ITWOMNS here because it is defined in sbem.h as two times the maximum
C number of activities
      iactFlarea=0
      do 49 iz=1,ncomp
        iactFlarea=iactFlarea+1
        act_area(iactFlarea)=real(theactivityindex(iz))
        iactFlarea=iactFlarea+1
        act_area(iactFlarea)= zbasea(iz)
 49   continue
      itrunc=1
      ipos=1
      delim='C'
      do while (itrunc.ne.0)
        call arlist(ipos,iactFlarea,act_area,ITWOMNS,delim,louts,
     &      loutln,itrunc)

C here we write each activity and the related floor area. SBEM seems to sum up the
C floor areas and writes the activity once if it covers many zones.
C BRUKL v3.2 seems to accept either ways.
        WRITE(IUF,'(4a)',iostat=ios) ' ACT-AREA                = ',
     &   '{ ',louts(1:loutln),' }'
        ipos=itrunc+1
      end do
      WRITE(IUF,'(a)',iostat=ios) ' ..'

C Section for each HVAC system for the reference building
      do 401 ICMSYS=1,NCMSYS
        hvacarea(ICMSYS)=0.
        IhvAct=0
        imerg=0
        do 425 iz=1,ncomp
          if(ICMSYS.eq.IHLZ(iz))then
            IhvAct=IhvAct+1
            zhvacarea(ICMSYS,IhvAct)=0.
            hvacarea(ICMSYS)=zbasea(iz)+hvacarea(ICMSYS)
            zhvacarea(ICMSYS,IhvAct)= zbasea(iz)
            Act_HVAC(ICMSYS,IhvAct)=theactivityindex(iz)
          endif
 425    continue
        if((hvacarea(ICMSYS)).gt.0.)then
          TMPSTR2=HVACNAME(ICMSYS)

C Same as actual building
          WRITE(IUF,'(4a)',iostat=ios) '"',TMPSTR2(1:LNBLNK(TMPSTR2)),
     &     '"',' = HVAC-SYSTEM-DATA'

C Same as actual building
          WRITE(IUF,'(a,f8.2)',iostat=ios)
     &      '  AREA                      = ',hvacarea(ICMSYS)

C Same as actual building
          LNB=lnblnk(HSYSNAME(INCMSYS(ICMSYS)))
          WRITE(IUF,'(2a)',iostat=ios)
     &      '  TYPE                      = ',
     &      HSYSNAME(INCMSYS(ICMSYS))(1:lnb)

C Same as actual building
          LNB=lnblnk(TEMPZS(ICMSYS))
          WRITE(IUF,'(2a)',iostat=ios)
     &      '  HEAT-SOURCE               = ',
     &      TEMPZS(ICMSYS)(1:lnb)

C Reference building is always natural gas.
C << should HeatSCoP_N be used for BRUKL? >>
          WRITE(IUF,'(2a)',iostat=ios)
     &      '  FUEL-TYPE                 = ','Natural Gas'
          RERSH_DEMAND(ICMSYS)=RERSH(ICMSYS)*HeatSCoP_R(ICMSYS)
          WRITE(IUF,'(a,F10.2)',iostat=ios)
     &      '  MJ/M2-HEAT-DEM            = ',ABS(RERSH_DEMAND(ICMSYS)*
     &      3.6)
          RERSC_DEMAND(ICMSYS)=RERSC(ICMSYS)*CoolSSEER_R(ICMSYS)
          WRITE(IUF,'(a,F10.2)',iostat=ios)
     &      '  MJ/M2-COOL-DEM            = ',
     &      ABS(RERSC_DEMAND(ICMSYS)*3.6)
          WRITE(IUF,'(a,F10.2)',iostat=ios)
     &      '  KWH/M2-HEAT               = ',ABS(RERSH(ICMSYS))
          WRITE(IUF,'(a,F10.2)',iostat=ios)
     &      '  KWH/M2-COOL               = ',ABS(RERSC(ICMSYS))
          WRITE(IUF,'(a,F10.2)',iostat=ios)
     &      '  KWH/M2-AUX                = ',RERSA(ICMSYS)

C This is based on what SBEM is using. Additional details
C can be found at NCM modelling guide vers. 2e Table6)
C However, SBEM v3.2b does not implement these values as this table
C in the modelling guide. It does not have for example a case where
C there is air-conditioning.
C If there is no ooling system:
          if(IBRUKLC(1,INCMSYS(ICMSYS)).eq.-1111)then

C then only heated space.
            WRITE(IUF,'(a,f6.2)',iostat=ios)
     &      '  HEAT-SSEFF                = ',HeatSCoP_R(ICMSYS)

C Treat all the other cases as heated and mechanically ventilated
          else
            WRITE(IUF,'(a,f6.2)',iostat=ios)
     &      '  HEAT-SSEFF                = ',HeatSCoP_R(ICMSYS)
            WRITE(IUF,'(a,f6.2)',iostat=ios)
     &      '  COOL-SSEER                = ',CoolSSEER_R(ICMSYS)
          endif

C Now start the logic needed for ACT-AREA entry
          itrunc=1
          ipos=1
          delim='C'
          if(IhvAct.gt.0)then

C Loop through the number of activities that this hvac covers (for their spaces)
C This will prepare TotHVACactArea for the arlist routine
C imerg is used for merging activities and floor areas in order to prepare the
C the array for the call to arlist
            do 497 iloop_IhvAct=1,IhvAct
              imerg=imerg+1
              TotHVACactArea(imerg)=
     &         real(Act_HVAC(ICMSYS,iloop_IhvAct))
              imerg=imerg+1
              TotHVACactArea(imerg)=
     &         zhvacarea(ICMSYS,iloop_IhvAct)
 497        continue
          endif
          do while (itrunc.ne.0)
            call arlist(ipos,imerg,TotHVACactArea,ITWOMNS,delim,louts,
     &          loutln,itrunc)
            WRITE(IUF,'(4a)',iostat=ios) ' ACT-AREA                = ',
     &       '{ ',louts(1:loutln),' }'
            ipos=itrunc+1
          end do
          WRITE(IUF,'(a)',iostat=ios) ' ..'
        endif
 401  continue

C ***TYPICAL BUILDING
C Typical building related data
      WRITE(IUF,'(a)',iostat=ios) '"BUILDING_DATA" = BUILDING-DATA'
      WRITE(IUF,'(a)',iostat=ios) ' ANALYSIS       =  TYPICAL'
      WRITE(IUF,'(a,f8.2)',iostat=ios)
     &  '  AREA                       = ',tot_floor_area

      WRITE(IUF,'(a,f8.2)',iostat=ios)
     &  '  AREA-EXT                   = ',exp_area

C This will have to be based on the location of the study
      WRITE(IUF,'(a)',iostat=ios)
     &  '  WEATHER                    =  LON'

C Confirm that BINF50 instead of some sort of "translated" air changes
      WRITE(IUF,'(a)',iostat=ios)
     &  '  Q50-INF                    = 15'

C << Next build_UAN is actually different for notional and typical buildings
C << i.e since they use different constructions
C << but EPC up to version 3.2 of sbem has them the same!!!!!
C << Keeping temporarily the same value as for the notional
      WRITE(IUF,'(a,f9.2)',iostat=ios)
     &  '  BUILDING-W/K               = ',build_UAN
      call eclose(EXP_AREA,0.0,0.0001,ISNEAR)
      if(.not.ISNEAR)then
        WRITE(IUF,'(a,f9.2)',iostat=ios)
     &  '  BUILDING-W/M2K             = ',build_UAN/EXP_AREA
      else
        WRITE(IUF,'(a)',iostat=ios)
     &  '  BUILDING-W/M2K             =  0.0'
      endif

C Using info from common of thermal bridges to find the percentage of heat loss
C due to thermal bridges out of the overall heat loss from the envelope
C If users do not enter the thermal bridges menu the uavtotal will be 0 and
C the overall build_UAN will be less than usual and therefore b_alpha will be
C greater than usual.
C Thermal bridges are always 10% for notional building
      call eclose(build_UAN,0.0,0.0001,ISNEAR)
      if(.not.ISNEAR)then
        b_alpha=(b_totheatloss/build_UAN)*100.0
      else
        b_alpha=0.0
      endif
      WRITE(IUF,'(a,f6.2)',iostat=ios)
     &  '  BUILDING-ALPHA             = ',b_alpha

C Calculate loads for heating, cooling, DHW etc.
      WRITE(IUF,'(a,f10.2)',iostat=ios)
     &  '  KWH/M2-HEAT                = ',ABS(TyER_HEAT)
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  '  KWH/M2-COOL                = ',ABS(TyER_COOL)
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  '  KWH/M2-AUX                 = ',TyER_AUX
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  '  KWH/M2-LIGHT               = ',TyER_LIGHT
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  '  KWH/M2-DHW                 = ',TyER_DHW

C << STOPPED HERE need equipm. info????? >>
C?? To enter total casual gains/m2 here
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  '  KWH/M2-EQUP                = ',0.0

C This seems to be always the same as for 'KWH/M2-HEAT'. Keeping the
C same variables and calculation as Typical building will always be
C Natural Gas
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  '  KWH/M2-NATGAS              = ',TyERF(1)

      WRITE(IUF,'(a)',iostat=ios) ' KWH/M2-LPG              = 0.0'
      WRITE(IUF,'(a)',iostat=ios) ' KWH/M2-BIOGAS           = 0.0'
      WRITE(IUF,'(a)',iostat=ios) ' KWH/M2-OIL              = 0.0'
      WRITE(IUF,'(a)',iostat=ios) ' KWH/M2-COAL             = 0.0'
      WRITE(IUF,'(a)',iostat=ios) ' KWH/M2-ANTHRACITE       = 0.0'
      WRITE(IUF,'(a)',iostat=ios) ' KWH/M2-SMOKELESS        = 0.0'
      WRITE(IUF,'(a)',iostat=ios) ' KWH/M2-DUELFUEL         = 0.0'
      WRITE(IUF,'(a)',iostat=ios) ' KWH/M2-BIOMASS          = 0.0'
      WRITE(IUF,'(a,F10.2)',iostat=ios)
     &  ' KWH/M2-SUPELEC           = ',ABS(TyERF(10))
      WRITE(IUF,'(a)',iostat=ios) ' KWH/M2-WASTEHEAT        = 0.0'

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-DISTRICT-HEATING = ',' 0.0'

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-DISP             = ',' 0.0'

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-PVS              = ',' 0.0'

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-WIND             = ',' 0.0'

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-CHP              = ',' 0.0'

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-SES              = ',' 0.0'

C Activity number and area per zone
C iactFlarea is a counter to prepare the act_area for the arlist call
C This act_area has to have maximum size 2 times the zone number, using
C ITWOMNS here because it is defined in sbem.h as two times the maximum
C number of activities
      iactFlarea=0
      do 749 iz=1,ncomp
        iactFlarea=iactFlarea+1
        act_area(iactFlarea)=real(theactivityindex(iz))
        iactFlarea=iactFlarea+1
        act_area(iactFlarea)= zbasea(iz)
 749  continue
      itrunc=1
      ipos=1
      delim='C'
      do while (itrunc.ne.0)
        call arlist(ipos,iactFlarea,act_area,ITWOMNS,delim,louts,
     &      loutln,itrunc)

C here we write each activity and the related floor area. SBEM seems to sum up the
C floor areas and writes the activity once if it covers many zones.
C BRUKL v3.2 seems to accept either ways.
        WRITE(IUF,'(4a)',iostat=ios) ' ACT-AREA                = ',
     &   '{ ',louts(1:loutln),' }'
        ipos=itrunc+1
      end do
      WRITE(IUF,'(a)',iostat=ios) ' ..'

C Section for each HVAC system for the notional building
      do 801 ICMSYS=1,NCMSYS
        hvacarea(ICMSYS)=0.
        IhvAct=0
        imerg=0
        do 725 iz=1,ncomp
          if(ICMSYS.eq.IHLZ(iz))then
            IhvAct=IhvAct+1
            zhvacarea(ICMSYS,IhvAct)=0.
            hvacarea(ICMSYS)=zbasea(iz)+hvacarea(ICMSYS)
            zhvacarea(ICMSYS,IhvAct)= zbasea(iz)
            Act_HVAC(ICMSYS,IhvAct)=theactivityindex(iz)
          endif
 725    continue
        if((hvacarea(ICMSYS)).gt.0.)then
          TMPSTR2=HVACNAME(ICMSYS)
          LNB=lnblnk(TMPSTR2)

C Same as actual building
          WRITE(IUF,'(4a)',iostat=ios) '"',TMPSTR2(1:LNB),
     &     '"',' = HVAC-SYSTEM-DATA'

C Same as actual building
          WRITE(IUF,'(a,f8.2)',iostat=ios)
     &      '  AREA                      = ',hvacarea(ICMSYS)

C Same as actual building
          LNB=lnblnk(HSYSNAME(INCMSYS(ICMSYS)))
          WRITE(IUF,'(2a)',iostat=ios)
     &      '  TYPE                      = ',
     &      HSYSNAME(INCMSYS(ICMSYS))(1:LNB)

C Same as actual building
          LNB=lnblnk(TEMPZS(ICMSYS))
          WRITE(IUF,'(2a)',iostat=ios)
     &      '  HEAT-SOURCE               = ',
     &      TEMPZS(ICMSYS)(1:LNB)

C Typical building is always natural gas
          WRITE(IUF,'(2a)',iostat=ios)
     &      '  FUEL-TYPE                 = ','Natural Gas'
          WRITE(IUF,'(a)',iostat=ios)
     &      '  FUEL-TYPE-COOL            = Grid Supplied Electricity'
          TyERSH_DEMAND(ICMSYS)=TyERSH(ICMSYS)*HeatSCoP_T(ICMSYS)
          WRITE(IUF,'(a,F10.2)',iostat=ios)
     &      '  MJ/M2-HEAT-DEM            = ',ABS(TyERSH_DEMAND(ICMSYS)*
     &      3.6)
          TyERSC_DEMAND(ICMSYS)=TyERSC(ICMSYS)*CoolSSEER_T(ICMSYS)
          WRITE(IUF,'(a,F10.2)',iostat=ios)
     &      '  MJ/M2-COOL-DEM            = ',
     &      ABS(TyERSC_DEMAND(ICMSYS)*3.6)
          WRITE(IUF,'(a,F10.2)',iostat=ios)
     &      '  KWH/M2-HEAT               = ',ABS(TyERSH(ICMSYS))
          WRITE(IUF,'(a,F10.2)',iostat=ios)
     &      '  KWH/M2-COOL               = ',ABS(TyERSC(ICMSYS))
          WRITE(IUF,'(a,F10.2)',iostat=ios)
     &      '  KWH/M2-AUX                = ',TyERSA(ICMSYS)

C This is based on what SBEM is using. Additional details
C can be found at NCM modelling guide vers. 2e Table6)
C However, SBEM v3.2b does not implement these values as this table
C in the modelling guide. It does not have for example a case where
C there is air-conditioning.
C If there is no ooling system:
          if(IBRUKLC(1,INCMSYS(ICMSYS)).eq.-1111)then

C then only heated space.
            WRITE(IUF,'(a,f6.2)',iostat=ios)
     &      '  HEAT-SSEFF                = ',HeatSCoP_T(ICMSYS)

C Treat all the other cases as heated and mechanically ventilated
          else
            WRITE(IUF,'(a,f6.2)',iostat=ios)
     &      '  HEAT-SSEFF                = ',HeatSCoP_T(ICMSYS)
            WRITE(IUF,'(a,f6.2)',iostat=ios)
     &      '  COOL-SSEER                = ',CoolSSEER_T(ICMSYS)
          endif

C Now start the logic needed for ACT-AREA entry
          itrunc=1
          ipos=1
          delim='C'
          if(IhvAct.gt.0)then

C Loop through the number of activities that this hvac covers (for their spaces)
C This will prepare TotHVACactArea for the arlist routine
C imerg is used for merging activities and floor areas in order to prepare the
C the array for the call to arlist
            do 997 iloop_IhvAct=1,IhvAct
              imerg=imerg+1
              TotHVACactArea(imerg)=
     &         real(Act_HVAC(ICMSYS,iloop_IhvAct))
              imerg=imerg+1
              TotHVACactArea(imerg)=
     &         zhvacarea(ICMSYS,iloop_IhvAct)
 997        continue
          endif
          do while (itrunc.ne.0)
            call arlist(ipos,imerg,TotHVACactArea,ITWOMNS,delim,louts,
     &          loutln,itrunc)
            WRITE(IUF,'(4a)',iostat=ios) ' ACT-AREA                = ',
     &       '{ ',louts(1:loutln),' }'
            ipos=itrunc+1
          end do
          WRITE(IUF,'(a)',iostat=ios) ' ..'
        endif
 801  continue

      call edisp(iuout,'Completed export of data to EPC inp')
      CALL ERPFREE(IUF,ISTAT)

C Create a batch file to run the EPCgen input file for Windows only
C Check if the machine is UNIX, If UNIX, do nothing
      call isunix(unixok)
      if(unixok)then
        mpof=1
        call edisp(iuout,'Copy UK NCM input files (BRUKL.inp and')
        call edisp(iuout,'EPCGen.inp) to a windows platform and ')
        call edisp(iuout,'run BRUKL and EPCGen in order to generate')
        call edisp(iuout,'UK NCM compliance report and ')
        call edisp(iuout,'Energy Performance Certificate EPC')

C If not UNIX, first determine the path of the current model folder
      else
        lcfgroot=cfgroot
        lpath=path
        lupath=upath
        limgpth=imgpth
        ldocpth=docpth
        ltmppth=tmppth
        ibrowse = 0

C Determine the path of current model
        call curproject(lcfgroot,lpath,lupath,limgpth,ldocpth,ltmppth,
     &    ibrowse)
        fs = char(92)
        col = char(58)
        quote = char(34)

C Generate an ini file for use with EPCgen.exe (v3.4b)
C Three lines in the file - 1st line includes the path
C to the location of EPCgen.exe, 2nd line is the path
C to the epc inp file, 3rd line is the path for the
C EPCgen log file (assumed to be the model cfg folder).
        ILEN=LNBLNK(LASBEM)
        WRITE(EPCINIFL,'(2A)')LASBEM(1:ILEN-4),'_epc.ini'
        IUF=IFIL+1
        CALL EFOPSEQ(IUF,EPCINIFL,3,IER)
        if(ier.eq.0)THEN
          write(currentfile,'(a)') EPCINIFL(1:lnblnk(EPCINIFL))
        ELSE ! WARNING MESSAGE HERE
        ENDIF
        WRITE(IUF,'(7A)',IOSTAT=IOS,ERR=3) 'C',col,fs,'Esru',fs,
     &    'UKNCMbin',fs
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
     &    lpath(1:lnblnk(lpath)),EPCFL(1:lnblnk(EPCFL))
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3) lpath(1:lnblnk(lpath))
        CALL ERPFREE(IUF,ISTAT)

C Write batch file that drives EPCgen.exe via the /I command
C option. Also copy EPCgen.log from  C:\Esru\esp-r\bin to model cfg folder)
        ILEN=LNBLNK(LASBEM)
        WRITE(EPCBATFL,'(2A)')LASBEM(1:ILEN-4),'_epc.bat'
        IUF=IFIL+1
        CALL EFOPSEQ(IUF,EPCBATFL,3,IER)
        if(ier.eq.0)THEN
          write(currentfile,'(a)') EPCBATFL(1:lnblnk(EPCBATFL))
        ELSE ! WARNING MESSAGE HERE
	ENDIF
        WRITE(IUF,'(6A)',IOSTAT=IOS,ERR=3) 'cd C',col,fs,'Esru',fs,
     &    'UKNCMbin'
        WRITE(IUF,'(5A)',IOSTAT=IOS,ERR=3) 'EPCgen.exe /I ',
     &    quote,lpath(1:lnblnk(lpath)),
     &    EPCINIFL(1:lnblnk(EPCINIFL)),quote
        WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3) 'copy EPCgen.log ',
     &    quote,lpath(1:lnblnk(lpath)),quote
        CALL ERPFREE(IUF,ISTAT)

C Run batch file to call EPCgen.exe
        write (doit,'(a)') EPCBATFL
        call runit(doit,'-')
      endif

      RETURN
 3    CONTINUE ! error message to go here
      END

************************* defLeakage *****************************
C Subroutine defLeakage allows the definition of the AHU and ductwork
C leakage details. Applicable only to specific systems (those with
C Specific Fan Power defined).
C isystCurrent: is the current system for which leakage is calculated
C (it will be selected/defined by the user when entering NCM HVAC
C information)
      SUBROUTINE defLeakage(isystCurrent,IER)
      IMPLICIT NONE
#include "sbem.h"

      common/outin/iuout,iuin
      integer iuout,iuin
      common/pophelp/h(60)
      character h*72
      integer isystCurrent
      logical menulogical !Logical to use for a menu in order to avoid the GOTO statements
      logical pickedsomething ! set to true if one of the options selected
      logical close,closea  ! to test for close to zero
      integer i_default,i_define,IER
      dimension items(5)
      character head*30,items*50
      integer IansLeakage
      integer nitms,ino ! max items and current menu item

      pickedsomething=.false.
      menulogical=.false.
      do while (.not.menulogical)
        INO=-1
        call edisp(iuout,'enter ductwork and ahu details')
        write(ITEMS(1),'(2A)') 'a Ductwork class: ',
     &       ductwork(isystCurrent)(1:24)
        write(ITEMS(2),'(2A)') 'b AHU class: ',
     &       AHUleakage(isystCurrent)(1:24)
        ITEMS(3) = '  _____________________________'
        ITEMS(4) = '? help                         '
        ITEMS(5) = '- exit this menu               '
        nitms=5
        write(head,'(a)')'Ductwork/AHU leakage details'

C Help text for this menu.
        H(1) ='This menu allows the definition of ductwork'
        H(2) ='and Air Handling Unit details. These will '
        H(3) ='be used for the Auxiliary Energy calculation'
        H(4) ='for the stripped building. The defaults will'
        H(5) ='use the worse case options: this for the '
        H(6) ='ductwork question is "Class Worse than A"'
        H(7) ='and for AHU is "Class worse than L3".'

C Menu for ductwork and AHU details.
        call emenu(head,items,nitms,ino)
        if(ino.eq.nitms)then

C If the user selected one of the options then we can exit.
C If nothing was selected and the data is still zero then set
C a standard default value. Thus, if there was a prior non-zero
C value it should be preserved.
          if(pickedsomething)then
            continue
          else
            call eclose(duct_tDLd(isystCurrent),0.0,0.0001,close)
            if(close)then
              duct_tDLd(isystCurrent)=0.15
            endif
            call eclose(AHU_tDLd(isystCurrent),0.0,0.0001,closea)
            if(closea)then
              AHU_tDLd(isystCurrent)=0.06
            endif
          endif
          menulogical=.true.
        elseif(ino.eq.nitms-1)then

C Produce help text for the menu.
          call phelpd(head,7,'-',0,0,IER)
          menulogical=.false.
        elseif(INO.EQ.1)THEN
          H(1)='The default is the same as Class Worse than A.'
          H(2)='Class B is the best option here. See '
          H(3)='EN 13779 Standard for more details.'
          call easkabcd(' ',
     &   'Has the ductwork been leakage tested ?',
     &   'No use default leakage','Class B','Class A',
     &   'Class Worse than A',IansLeakage,3)
          if(IansLeakage.eq.1)then
            write(ductwork(isystCurrent)(1:24),'(a)')
     &           'No, use default leakage'

C The figures for duct_tDLd and AHU_tDLd are needed to calculate the Auxiliary
C the Auxiliary energy of the actual/stripped building. The calculation is
C based on an old on an old "HVAC document" that BRE distributed to describe
C the auxiliary energy calculation in iSBEM.
            duct_tDLd(isystCurrent)=0.15
            menulogical=.false.
          elseif(IansLeakage.eq.2)then
            write(ductwork(isystCurrent)(1:24),'(a)') 'Class B'
            duct_tDLd(isystCurrent)=0.02
            menulogical=.false.
          elseif(IansLeakage.eq.3)then
            write(ductwork(isystCurrent)(1:24),'(a)') 'Class A'
            duct_tDLd(isystCurrent)=0.06
            menulogical=.false.
          elseif(IansLeakage.eq.4)then
            write(ductwork(isystCurrent)(1:24),'(a)')
     &           'Class Worse than A'
            duct_tDLd(isystCurrent)=0.15
            menulogical=.false.
          else
            write(ductwork(isystCurrent)(1:24),'(a)')
     &           'No, use default leakage'
            duct_tDLd(isystCurrent)=0.15
            menulogical=.false.
          endif
          pickedsomething=.true.
        elseif(ino.eq.2)then
          H(1)='The default is the same as Class Worse than L3.'
          H(2)='Class L1 is the best option here. See EN 13779'
          H(3)='Standard for more details.'
          i_default=1
          i_define=0
          call menuatol('Define AHU leakage',
     &    'Does the AHU meet CEN leakage Standards?',
     &    'a No, use default leakage','b Class L1','c Class L2',
     &    'd Class L3','e Class Worse than L3',' ',' ',' ',' ',
     &    ' ',' ',' ',i_define,i_default,3)
          if(i_define.eq.1)then
            write(AHUleakage(isystCurrent)(1:24),'(a)')
     &          'No, use default leakage'

C The figures for duct_tDLd and AHU_tDLd are needed to calculate the Auxiliary
C the Auxiliary energy of the actual/stripped building. The calculation is
C based on an old on an old "HVAC document" that BRE distributed to describe
C the auxiliary energy calculation in iSBEM.
            AHU_tDLd(isystCurrent)=0.06
            menulogical=.false.
          elseif(i_define.eq.2)then
            write(AHUleakage(isystCurrent)(1:24),'(a)') 'Class L1'
            AHU_tDLd(isystCurrent)=0.001
            menulogical=.false.
          elseif(i_define.eq.3)then
            write(AHUleakage(isystCurrent)(1:24),'(a)') 'Class L2'
            AHU_tDLd(isystCurrent)=0.007
            menulogical=.false.
          elseif(i_define.eq.4)then
            write(AHUleakage(isystCurrent)(1:24),'(a)') 'Class L3'
            AHU_tDLd(isystCurrent)=0.02
            menulogical=.false.
          elseif(i_define.eq.5)then
            write(AHUleakage(isystCurrent)(1:24),'(a)')
     &           'Class Worse than L3'
            AHU_tDLd(isystCurrent)=0.06
            menulogical=.false.

C << ELSE may not needed for this >>
          else
            write(AHUleakage(isystCurrent)(1:24),'(a)')
     &          'No, use default leakage'
            AHU_tDLd(isystCurrent)=0.06
            menulogical=.false.
          endif
          pickedsomething=.true.
        else
          menulogical=.false.
        endif
      end do
      END

************************* ENLARGE *****************************
C Subroutine ENLARGE performs geometric enlargement (contraction) of a
C surface and for a given vertex (POINT) it returns a transformed
C vertex (TRANS) based on a scale factor (factor) and a centre of
C enlargement (coe). TRANS will be somewhere along the line between
C COE and POINT. It is expected to be called once for each vertex
C of the surface who's size is to be adjusted.
C Factor can be a fraction (in which case it is a contraction of a
C geometric surface or may be greater than one in which case it is an
C enlargement.
      SUBROUTINE ENLARGE(POINT,TRANS,COE,FACTOR)
      DIMENSION POINT(3),COE(3),TRANS(3)
      DO 1 I=1,3

C << Find out when absolute value is required
        TRANS(I)=((1.-FACTOR))*COE(I)+FACTOR*POINT(I)
c        TRANS(I)=(ABS(1.-FACTOR))*COE(I)+FACTOR*POINT(I)
 1    CONTINUE
      RETURN
      END

C ************** okforncm ********************
C Subroutine okforncm scans the current model to find out if it is
C sufficiently attributed for use with the UK NCM method.
C Parameters:
C   ier = 0 if ok, ier = 1 if problem during scan, ier = 2 model
C      has older cfg version, ier=3 zone geometry is older version,
C      ier=4 geometry overly complex, ier=5 missing NCM description.
C   act = '-' work silently, 'w' print warnings.
      subroutine okforncm(act,ier)
#include "building.h"
#include "geometry.h"
#include "model.h"
#include "sbem.h"
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      COMMON/C24/IZSTOCN(MCOM,MS)

C G9 holds information on children of a surface.
      integer nbchild   ! how many children (up to 4)
      integer nbgchild  ! how many grand children (up to 4)
      integer ichild    ! list of children
      integer igchild   ! list of grand children
      common/G9/nbchild(MCON),nbgchild(MCON),ichild(MCON,4),
     &igchild(MCON,4)

      character acts*1   ! for call to surrel
      character outs*124 ! for feedback
      logical unixok
      logical newgeo

C For parameters:
      integer ier
      character act*1

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)

C The version of the cfg file must be 4 or greater.
      if(icfgv.lt.4)then
        ier=2
	call usrmsg(
     &    'Model configuration file is an older format. Go to pre-',
     &    'ferences menu & update it and attribute surfaces.','W')
        return
      endif

C If UK NCM model complexity is undefined.
      if(pjLevComplexity(1:9).eq.'Undefined')then
        ier=5
        call usrmsg(
     &    'Project complexity has not been defined. Go back to',
     &    'model context and add this definition.','W')
        return
      endif

C Find out if the version of geometry file is new enough.
      DO 551 ICOMP=1,NCOMP
        IUF=IFIL+2
        call eclose(gversion(icomp),1.1,0.01,newgeo)
        if(NEWGEO)then

C Scan geometry file, look for surface attributes.
          CALL GEOREAD(IUF,LGEOM(ICOMP),ICOMP,1,IUOUT,IER)
          acts='s'

C Code to loop through each surface to build a hash table so that each
C surface also knowns its children
          CALL SURREL(acts,ICOMP,IER)

C For each surface in this zone
          DO 552 ISUR=1,NZSUR(ICOMP)
            ICN=IZSTOCN(ICOMP,ISUR)

C If surface is exterior then check if use is specified.
            if(SSOTHER(ICN,1)(1:8).EQ.'EXTERIOR')then
              if(SSUSE(ICN,1)(1:1).EQ.'-')then
                write(outs,'(5A)')'Usage for surface ',SSNAME(ICN),
     &          ' in zone ',zname(icomp)(1:lnzname(icomp)),
     &          ' is unknown and must be defined.'
                call edisp(iuout,outs)
	        ier=3
              endif
            endif

C If surface has more than one child surface then the process
C of creating the notional model will fail.
            if(nbgchild(ICN).gt.1)then
              write(outs,'(5A)')'Surface ',SSNAME(ICN),
     &          ' in zone ',zname(icomp)(1:lnzname(icomp)),
     &          ' has multiple child surfaces.'
              call edisp(iuout,outs)
	      ier=4
            endif
 552      continue

C If a fatal errors exit now.
	  if(ier.eq.3.or.ier.eq.4)then
            call edisp(iuout,
     &        'Correct issues via the geometry menu.')
            return
          endif

C Further checks here...
C << to be done >>

        else
          ier=2
	  write(outs,'(3a)') 'Zone file ',
     &      LGEOM(ICOMP)(1:lnblnk(LGEOM(ICOMP))),
     &      ' is an older format.'
	  call usrmsg(outs,
     &    'Go to the preferences menu, update it & add surface use.',
     &    'W')
          return
        endif
 551  continue

      return
      end


C ************** GTCALC ********************
C Subroutine GTCALC scans the climate file associated with the model and
C writes out a ground temperature profile based on dry bulb temperature.
C After calling this subroutine the cfg file should be saved in order to
C make sure this information is written to file.

C Algorithm follows that in EPW calculation with references given as:
C 1. T. Kusuda, "Earth Temperatures Beneath Five Different Surfaces"
C    Institute for Applied Technology, NBS Report 10-373, 1971, NBS, Washington DC 20234.
C 2. T. Kusuda, Least Squares Technique for the Analysis of Periodic Temperature of the Earth's Surface Region, 
C    NBS Journal of Research, Vol. 71C, Jan-Mar. 1967, pp 43-50.

      subroutine GTCALC(ZD,act,IER)
      implicit none
#include "building.h"
#include "esprdbfile.h"
#include "site.h"

C Passed parameters.
      real ZD           ! Depth at which temperature required
      character act*1  ! '-' to report, 's' sbem mode
      integer ier      ! non-zero is an error

      common/FILEP/IFIL
      integer ifil   

      COMMON/LongRad/iExtLgRadFlag,eGrdTp(12)
      INTEGER iExtLgRadFlag
      REAL eGrdTp

      integer ICLM(24,6)
      real T_max,T_min,T_avg ! Max, min and average annual air temperature
      real tTEMP(12) ! monthly average air temperature from climate file
      real TEMP
      real T_amp       ! annual amplitude of air temperature
      real T_mnt(12)   ! average monthly ground temperature
      real pi
      real D_therm     ! Thermal diffusivity of soil calculated as
                       ! conductivity/(density*specific heat) with time
                       ! converted according to frequency of calculation
      real YearHours   ! Hours in year (assuming not a leap year)
      real PhaseLag, X, Beta, GM, Phi 
      real Theta       ! Hour number of the 15th day of the month
      integer II,I,irec,istat,j,k
      integer I_coldmonth  !Month of the year with coldest average temperature

      REAL MidMonthDay(12)
      DATA MidMonthDay/
     &   15.,46.,74.,95.,135.,166.,196.,227.,258.,288.,319.,349./

      iclim=IFIL
      call EFOPRAN(ICLIM,LCLIM,144,0,IER)
      IF(ier.ne.0)return

C Initialise
      DO 50 I=1,12
        TTEMP(I)=0.0
        T_mnt(I)=0.0
 50   CONTINUE
      T_max=-100.
      T_min=100.
      T_avg=0.0
      T_amp=0.0
      temp=0.0
      pi=4.0*ATAN(1.0)

C Set ground diffusivity - could be made user modifiable in future.
C As assumed in EPW (m2/day) - seems very low.
      D_therm=2.3225760E-03   
      YearHours=8760.0

      if(act(1:1).eq.'-')then
        continue
      else
        NGRDP=1  ! NCM methods need one NGRDP
      endif

C Calculate average monthly air temperature from climate file.
      DO 55 I=1,365
        II=I
        IREC=II
        READ(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=103)((ICLM(J,K),K=1,6),
     &         J=1,24)
        DO 656 J=1,24
          TEMP=REAL(ICLM(J,2))/10.
          IF(I.LE.31)THEN
            tTEMP(1)=tTEMP(1)+TEMP/(31.*24.)
          ELSEIF(I.LE.59)THEN
            tTEMP(2)=tTEMP(2)+TEMP/(28.*24.)
          ELSEIF(I.LE.90)THEN
            tTEMP(3)=tTEMP(3)+TEMP/(31.*24.)
          ELSEIF(I.LE.120)THEN
            tTEMP(4)=tTEMP(4)+TEMP/(30.*24.)
          ELSEIF(I.LE.151)THEN
            tTEMP(5)=tTEMP(5)+TEMP/(31.*24.)
          ELSEIF(I.LE.181)THEN
            tTEMP(6)=tTEMP(6)+TEMP/(30.*24.)
          ELSEIF(I.LE.212)THEN
            tTEMP(7)=tTEMP(7)+TEMP/(31.*24.)
          ELSEIF(I.LE.243)THEN
            tTEMP(8)=tTEMP(8)+TEMP/(31.*24.)
          ELSEIF(I.LE.273)THEN
            tTEMP(9)=tTEMP(9)+TEMP/(30.*24.)
          ELSEIF(I.LE.304)THEN
            tTEMP(10)=tTEMP(10)+TEMP/(31.*24.)
          ELSEIF(I.LE.334)THEN
            tTEMP(11)=tTEMP(11)+TEMP/(30.*24.)
          ELSEIF(I.LE.365)THEN
            tTEMP(12)=tTEMP(12)+TEMP/(31.*24.)
          ENDIF
          T_avg=T_avg+TEMP/(24.*365.)
  656   CONTINUE
   55 CONTINUE

C Calculate maximum and minimum monthly temperatures.
      T_max=tTEMP(1)
      T_min=tTEMP(1)
      I_coldmonth=1
      do 52 k=1,12
        T_max=max(T_max,tTEMP(k))
        if(tTEMP(k).lt.T_min) I_coldmonth=k
        T_min=min(T_min,tTEMP(k))
 52   continue
      CALL ERPFREE(ICLIM,ISTAT)

C Calculate monthly ground temperature.

C First get the monthly amplitude swing.
      T_amp=(T_max-T_min)/2.

C Calculate phase lag from start of year to month with lowest air temperature.
C Offset is 0.60 for locations where this month is January.
      PhaseLag=MidMonthDay(I_coldmonth)*0.017214 + 0.341787

      Beta=sqrt(pi/(D_therm*YearHours))*ZD
      X=exp(-Beta)
      GM=sqrt((X**2 -2.*X*cos(Beta) + 1.0) / (2.*Beta**2))
      Phi=ATAN((1.0-X*(cos(Beta)+sin(Beta))) 
     &        / (1.0-X*(cos(Beta)-sin(Beta))))

      DO 200 k=1,12
        Theta=MidMonthDay(k)*24.0
        T_mnt(k)=T_avg - T_amp
     &           *cos(Theta*2.0*pi/YearHours - PhaseLag - Phi)*GM
 200  CONTINUE
      
C Write to common depending on which mode it was called with.
      DO 27 I=1,12
        if(act(1:1).eq.'-')then
          eGrdTp(I)=T_mnt(I)
        else
          UGRDTP(I,1)=T_mnt(I)
        endif
  27  CONTINUE

 103  return
      end
