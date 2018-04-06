C This header relates to high level model entities in ESP-r. It is
C dependant on building.h and should follow building.h so that
C parameters will have been defined.

C Current working directory (where prj was started) and from pwd
C into the cfg folder
      character pwdinitial*84,pwdtocfg*72
      common/pwdinit/pwdinitial,pwdtocfg
      integer lnpwdi,lnpwdc  ! lengths of these paths
      common/pwdlnint/lnpwdi,lnpwdc

C Configuration file information.
      integer icfgv  ! version of configuration file which determines
                     ! the format and contents of the file:
                     ! version 1 did not have *database section
                     ! version 2 did not use tag data format (before 1996)
                     ! version 3 used zone obstruction files and IPV files
                     ! version 4 current version from May 2008
      common/CFGV/icfgv

      integer ifcfg         ! file unit for model configuration file
      character cfgroot*32  ! root name of the model used to generate file names
      character LCFGF*72    ! model configuration file
      COMMON/C21/IFCFG,cfgroot,LCFGF

C The title and summary of the model
      character modeltitle*72  ! title of the model (to replace LSNAM)
      character modeldocblock*248   ! text block of documentation for model
      common/modeltd/modeltitle,modeldocblock

C Model log file and contents file
      character lmodellog*144   ! ASCII file for project documentation
      character lmodelqa*144    ! model contents report
      common/LLOG/lmodellog,lmodelqa

C Paths from model cfg file folder to other model file types. As the
C folder names are in the form of ../nets the string length is short.
      character zonepth*24,netpth*24  ! to zone files and flow network files
      character ctlpth*24,imgpth*24   ! to control files and model images
      character radpth*24,docpth*24    ! to radiance model files and documents
      character dbspth*24   ! to local common data
      character aimpth*24,bsmpth*24   ! to AIM2 files and BASIMP files
      character hvacpth*24            ! to ideal HVAC files
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,
     &             dbspth,aimpth,bsmpth,hvacpth

      character upath*72  ! Path to the users home folder (this is often
                          ! combined with local path names to make up a
                          ! longer (144 character) string.
      common/uhome/upath

      character LPROJ*72 ! zone scheduling (operation files)
      character LGEOM*72 ! zone geometry file
      character LSHAD*72 ! zone temporal shading and insolation file
      character LTHRM*72 ! zone construction file
      COMMON/C2/LPROJ(MCOM),LGEOM(MCOM),LSHAD(MCOM),LTHRM(MCOM)

C Files associated with the model configuration file.
      character LCNN*72  ! model connections file name.
      common/C3F/LCNN

C Indicators for locations or existance of zone supplimental files.
      integer IVF  ! zone view factor file zero is not used, one exists 
      integer ISI  ! zone shading/insolation file zero is not used, one exists
      integer IHC  ! zone hc coef file zero is not used, one exists
      integer ITW  ! zone optical properties file zero is not used, one exists
      integer ICGC ! zone casual gain control file zero is not used, one exists
      integer IOBS ! zone obstructions zero is not used, one exists in a
                   ! separate file, two included in zone geometry file
      integer nccode ! zone code (not used).
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),ITW(MCOM),
     &              ICGC(MCOM),IOBS(MCOM),NCCODE(MCOM)

C File names for zone supplimental files.
      character LVIEW*72  ! zone view factor file
      character LHCCO*72  ! zone hc coef file
      character LTWIN*72  ! zone optical properties file
      character LCGCIN*72 ! zone casual gain control file
      character ZOBS*72   ! zone obstructions file (if separate from geo file)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),LTWIN(MCOM),
     &             LCGCIN(MCOM),ZOBS(MCOM)

C Temporal definition file names.
      character LTDF*72   ! binary scratch version of temporal file
      character LTDFA*72  ! ascii version of temporal file (referenced in the
                          ! model configuration file)
      COMMON/TDFFT/LTDF,LTDFA

C Temporal file units and file types
      integer iutdf   ! file unit for binary scratch version of temporal file
      integer itdflg  ! 0 none, 1 older file, 2 asci v2, 3 asci v3, -2 bin v2, -3 bin v3
      integer iutdfa  ! file unit for ascii version of temporal file
      COMMON/TDFI/IUTDF,ITDFLG,IUTDFA

C Radiance rcf scene file associated with the model.
      character LRADCF*72
      common/radcfg/LRADCF

C Anchor - named and typed concept with associated list of surfaces.
      integer naloc        ! number of anchors
      character ALOCLBL*12 ! user name for the anchor
      character ALOCTYP*4  ! type of anchor SURF (general surface)
                           ! CLDR OPDR CLWN OPWN (open or closed windows doors
                           ! CLGR OPGR (open or closed grills)
                           ! DUCT FANP (duct or pipe, fan or pump)
      common/anchora/NALOC,ALOCLBL(20),ALOCTYP(20)
      integer ialoc     ! number of links for each anchor
      integer lstanchr  ! connection index for each anchor link
      common/anchorb/IALOC(20),lstanchr(20,99)

C IES files to associate with entities passed to Radiance. Up to
C 10 can be associated with a model. The iesname is the root on to which
C variants for control will be created e.g. root arran_led -> arran_led100
C arran_led075 arran_led050 etc. are managed.
      integer nbofies       ! number of IES files
      character iesname*12  ! short root name to associate with IES data set 
      character iesmenu*32  ! menu entry for IES
      character iesfile*72  ! IES file
      character iesalong*1  ! length along axis X or Y or Z
      common/ies/nbofies,iesname(10),iesmenu(10),iesfile(10),
     &  iesalong(10)
      real ieslen,ieswid,iesht ! length width height of IES entity
      common/iesdim/ieslen(10),ieswid(10),iesht(10)
      integer iespercents   ! number steps of on percentages
      real iessteps         ! percentage for each step
      common/ieson/iespercents(10),iessteps(10,5)
