C This header relates to coupling of ESP-r with Radiance. It is
C dependant on building.h and include statements to e2r_common.h should
C follow building.h so that parameters will have been defined.

C -- 1st step: keep old version until all definitions in source files 
C    are replaced by this header file and the program runs as before

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Radiance based commons.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C rofil = `outside` objects, rzfil = is the zone (inside) composition.
C rskyfil is the sky definition file,  matfil,rmmfil contain
C materials and miscel geometries.
C rmfil is for miscel descriptions.

      common/e2rf/rambfil,optnfil
      character rambfil*72,optnfil*72
C     rambfil 
C     optnfil  Radiance options file name (?)


      common/rad1/rofil,rzfil,rskyfil,octfil,picfil,rmfil,glzfil
      character*72 rofil,rzfil,rskyfil,octfil,picfil,rmfil,glzfil
C     rofil
C     rzfil
C     rskyfil  Radiance sky description file
C     octfil   Radiance octree file
C     picfil   Radiance picture file
C     rmfil
C     glzfil   Radiance glazing file


      common/rad1a/aglzfil
      character*72 aglzfil
C     aglzfil  Radiance alternative glazing data set file (?)


      common/e2rq/indrcb,itrack,imgqua,detlvl,llvar,penumb
      integer indrcb,itrack
      character imgqua*6,detlvl*6,llvar*6,penumb*5
C     indrcb
C     itrack
C     imgqua
C     detlvl
C     llvar
C     penumb


      common/rad1m/matfil,rmmfil
      character*72 matfil,rmmfil
C     matfil  Radiance materials definitions file
C     rmmfil  


      common/raddn/skydone,outdone,indone,misdone,vewdone,glzdone
      logical skydone,outdone,indone,misdone,vewdone,glzdone
C     *done  Radiance logical variables


      common/rad2/mono,intext,ifocz
      integer mono,intext,ifocz
C     mono
C     intext
C     ifocz   index of current zone ("focus")

      common/radrp/ipicx
      integer ipicx
C     ipicx


      common/radv/rvpx,rvpy,rvpz,vdx,vdy,vdz,angh,angv,azimuth,elevtn
      real rvpx,rvpy,rvpz,vdx,vdy,vdz,angh,angv,azimuth,elevtn
C     rvpx, rvpx, rvpz  Radiance view point coordinates (?)
C     vdx, vdy, vdz     Radiance view direction vector (?)
C     angh, angv        Radiance view angle horizontal and vertical
C     azim, elev        Radiance ...


      common/grdisk/grdd,grcx,grcy
      real grdd,grcx,grcy
C     grdd  ground disk diameter (?)
C     grcx  ground disk center x coordinate (?)
C     grcy  ground disk center y coordinate (?)

      common/radgt/iglzty
      integer iglzty
C     iglzty


      common/sky1/rgrfl,isky,irdoy,rtime,iryear
      integer isky,irdoy,iryear
      real rgrfl,rtime
C     rgrfl
C     isky
C     irdoy
C     rtime
C     iryear


      common/radif/irofil,irzfil,imatfil,iglzfil
      integer irofil,irzfil,imatfil,iglzfil
C     irofil
C     irzfil
C     imatfil
C     iglzfil


      common/e2rc/crenrp,coconv
      character crenrp*160,coconv*40
C     crenrp  Radiance render special options
C     coconv  Radiance octree special options


      common/e2rs/indxscn,rscedes(10),rzoncmd,rupaxis
      integer indxscn
      character rscedes*72,rzoncmd*60,rupaxis*1
C     indxscn  Radiance scene index (?)
C     rscedes  Radiance scene description (??)
C     rzoncmd
C     rupaxis


      COMMON/RAY5OB/OXMN,OYMN,OZMN,OXMX,OYMX,OZMX


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Scene configuration commons.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      common/radcfg/LRADCF
C      common/radcfg/LRADCF(MCOM)
      character LRADCF*72
C     LRADCF is the ESP-r radiance configuration file .rcf

      common/raddata/SCENE(MCOM+1),RIFNAME(MCOM+1),SCENERT(MCOM+1),
     &               SCENEDESC(MCOM+1),SCENEPURP(MCOM+1)
C      common/raddata/SCENE(MCOM,MNSCN),SCENERT(MCOM,MNSCN),
C     &               SCENEDESC(MCOM,MNSCN),SCENEPURP(MCOM,MNSCN),
C     &               RIFNAME(MCOM,MNSCN,MBP)
      character SCENE*28,SCENERT*28,SCENEDESC*72
      character SCENEPURP*12,RIFNAME*72
C     SCENE     is the scene name
C     SCENERT   is the scene name root
C     SCENEDESC is a descriptive string for the scene
C     SCENEPURP is the scene purpose
C     RIFNAME   is the name of the rif (max 'MBP' per scene possible)


      common/raddata2/NSCENE,ISCENE
      integer NSCENE,ISCENE
C     NSCENE is the number of scenes defined in LRADCF
C     ISCENE is the current scene in use


      common/raddata3/NBSRIF(MCOM+1),LBSRIF(MCOM+1)
      integer NBSRIF
      character LBSRIF*72
C      character*
C      common/raddata3/NBSRIF(MCOM,MNSCN)
C     NBSRIF is the number of RIF files defined for the corresponding scene
C     LBSRIF is the name of the alternative .rif file


      common/radabs/NABS
      integer NABS
C     NABS is the number of alternative blind states (use IBCMT instead?)


      common/cmddata/zone,aim,act
C      common/cmddata/zone,aim,act
      character zone*16,aim*12,act*72
C     zone is the zone number as string
C     aim is the Radiance model result type "Day_coeff", "Coupling", "Create", "External"
C     act is the transfer file, typically ".<ESP-r PID>.dat"


      common/radgrid/LDFGRID
      character LDFGRID*72
C     LDFGRID is ...

C     radgrpts: Daylight factor sensor common
      common/radgrpts/NDFP,DFPTS(500,3),DFDIR(3),DFVALS(500)
      integer ndfp
      real dfpts,dfdir,dfvals
C   NDFP: Number of locations at which to get daylight factor.
C   DFPTS: location to calculate DF at.
C   DFDIR: direction that sensor is looking.
C   DFVALS: Daylight factors.


      common/e2rv/vewcmds(20),rvewsh(20),indxvew,ipckvew
      character vewcmds*124,rvewsh*10
      integer indxvew,ipckvew
C     vewcmds
C     rvewsh
C     indxvew
C     ipckvew
