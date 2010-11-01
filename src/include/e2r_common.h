C This header relates to Radiance model entities and attributes in ESP-r. It is
C dependant on building.h and should follow building.h so that
C parameters will have been defined. Based on an initial idea by Achim Geissler.

C Radiance based file for picture processing:
      character rambfil*72  ! Radiance abient file name used if bounces > 0
      character optnfil*72  ! Radiance options file name for use with rpict and rview
                            ! to shorten command lines - root.opt
      common/e2rf/rambfil,optnfil

C Radiance model descriptive files:
      character rofil*72    ! outside facing objects file - root-out.rad
      character rzfil*72    ! zone (inside) composition - root-in.rad
      character rskyfil*72  ! the sky definition file - root.sky
      character octfil*72   ! radiance octree file - root.oct
      character picfil*72   ! picture file to be produced
      character rmfil*72    ! miscel geometries - root-misc.rad
      character glzfil*72   ! transparent materials definitions - root-glz.rad
      common/rad1/rofil,rzfil,rskyfil,octfil,picfil,rmfil,glzfil

      character aglzfil*72  ! alternative transparent definitions - root-glz_a.rad
      common/rad1a/aglzfil

      character matfil*72   ! opaque materials definitions
      character rmmfil*72   ! miscel materials descriptions
      common/rad1m/matfil,rmmfil

C File unit numbers for radiance model files
      integer irofil,irzfil,imatfil,iglzfil
      common/radif/irofil,irzfil,imatfil,iglzfil

C Logical states for model task completion. Set to fales if the
C task is not complete.
      logical skydone,outdone,indone,misdone,vewdone,glzdone
      common/raddn/skydone,outdone,indone,misdone,vewdone,glzdone

C Directives for images and image processing
      integer ipicx  ! resolution or image width [X pixels] default is 512
      common/radrp/ipicx

      character crenrp*160  ! extra rendering commands for rpict
      character coconv*40   ! extra rendering commands for oconv
      common/e2rc/crenrp,coconv

      integer indrcb  ! indirect calculation importance: 0-2
      integer itrack  ! progress reporting in whole minutes
      character imgqua*6  ! image quality directive Low|Medium|High
      character detlvl*6  ! detail level  Low|Medium|High
      character llvar*6   ! light level variability  Low|Medium|High
      character penumb*5  ! penumbras False|True  Default is False
      common/e2rq/indrcb,itrack,imgqua,detlvl,llvar,penumb

      integer mono   ! zero is mono display one is few colours two is full
      integer intext ! zero is outside view one is inside a room
      integer ifocz  ! -1 is outside view or if positive in a room
      common/rad2/mono,intext,ifocz


C Radiance views
      character vewcmds*124 ! first token of view commands is short name for
                            ! the view followed by -vp and -vd data
      character rvewsh*10   ! array of short view names for selection lists
      integer indxvew       ! number of views (up to 20)
      integer ipckvew       ! index of current view
      common/e2rv/vewcmds(20),rvewsh(20),indxvew,ipckvew

      real rvpx,rvpy,rvpz ! radiance eyepoint X Y Z (m)
      real vdx,vdy,vdz    ! radiance view vector X Y Z (-)
      real angh,angv      ! horizontal and vertical angle of view (deg)
      real azimuth,elevtn ! view direc azimuth & elevation to match  vdx vdy vdz
      common/radv/rvpx,rvpy,rvpz,vdx,vdy,vdz,angh,angv,azimuth,elevtn

C Radiance scenes
      integer indxscn  ! number of scene descriptors zero is the initial state, two
                       ! indicates that sky and glazing files have been defined. Incremented
                       ! as contents of rif file are scanned 
      character rscedes*72  ! scene descriptor -  sky then glazing the the rest
      character rzoncmd*60  ! used with the ZONE= radiance command
      character rupaxis*1   ! up axis: single character Z (the default)
      common/e2rs/indxscn,rscedes(10),rzoncmd,rupaxis

      character SCENE*28    ! scene name (unsure how this differes from SCENERT)
      character RIFNAME*72  ! radiance RIF file name (array of file names)
      character SCENERT*28  ! scene root name (used to create other named entities)
      character SCENEDESC*72  ! user documentation of the scene
      character SCENEPURP*12  ! scene purpose UNKNOWN|External|Internal|Day_fact|Coupling
      common/raddata/SCENE(MCOM+1),RIFNAME(MCOM+1),SCENERT(MCOM+1),
     &               SCENEDESC(MCOM+1),SCENEPURP(MCOM+1)

      integer NBSRIF      ! for each scene -1 if not defined, 1 if alternative rif defined
      character LBSRIF*72 ! alternate rif file name
      common/raddata3/NBSRIF(MCOM+1),LBSRIF(MCOM+1)

      integer NABS    ! number of alternative blind states
      common/radabs/NABS

      integer NSCENE  ! number of scenes and rif files
      integer ISCENE  ! current scene index (in the array of scenes)
      common/raddata2/NSCENE,ISCENE

      integer iglzty   ! glazing type 1 sets alt glazing scene= (default), type 2 sets the
                       ! alt glazing illum= precalculated scene, type 3 (?)
      common/radgt/iglzty

C Command line directives
      character zone*16 ! command line Def_ or an ESP-r thermal zone name
      character aim*12  ! command line External|Coupling|Day_coef|Create
      character cmdact*72  ! command line None|Create 
      common/cmddata/zone,aim,cmdact

      character runpath*72  ! path for working radiance model 
      character pathtype*24 ! is either radincfg or radinrad
      common/expath/runpath,pathtype

      integer lnrp  ! length of runpath
      common/expathl/lnrp

C Ground disk information
      real grdd      ! ground disk diameter (m)
      real grcx,grcy ! ground disk centre X and Y (m)
      common/grdisk/grdd,grcx,grcy

C Sky definitions
      real rgrfl     ! ground reflection (equiv to groundrefl)
      integer isky   ! 1 is -c 2 is -s 3 is +s 4 is +c
      integer irdoy  ! julian day of year for computing sky values
      real rtime     ! time (0-24) for use in computing sky values
      integer iryear ! year for use in computing sky values
      common/sky1/rgrfl,isky,irdoy,rtime,iryear

C Daylight factors
      character LDFGRID*72  ! file containing df grid points
      common/radgrid/LDFGRID

      integer NDFP  ! number of locations for daylight factors
      real DFPTS    ! location X Y Z to calculate DF
      real DFDIR    ! direction sensor is looking
      real DFVALS   ! df values
      common/radgrpts/NDFP,DFPTS(500,3),DFDIR(3),DFVALS(500)
