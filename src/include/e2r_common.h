C This header relates to coupling of ESP-r with Radiance. It is
C dependant on building.h and include statements to e2r_common.h should
C follow building.h so that parameters will have been defined.

C -- 1st step: keep old version until all definitions in source files 
C    are replaced by this header file and the program runs as before

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Scene configuration commons.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      character LRADCF*72
      common/radcfg/LRADCF
C      common/radcfg/LRADCF(MCOM)
C     LRADCF is the ESP-r radiance configuration file .rcf

      character SCENE*28,SCENERT*28,SCENEDESC*72,SCENEPURP*12,RIFNAME*72
      common/raddata/SCENE(MCOM+1),RIFNAME(MCOM+1),SCENERT(MCOM+1),
     &               SCENEDESC(MCOM+1),SCENEPURP(MCOM+1)
C      common/raddata/SCENE(MCOM,MNSCN),SCENERT(MCOM,MNSCN),
C     &               SCENEDESC(MCOM,MNSCN),SCENEPURP(MCOM,MNSCN),
C     &               RIFNAME(MCOM,MNSCN,MBP)
C     SCENE     is the scene name
C     SCENERT   is the scene name root
C     SCENEDESC is a descriptive string for the scene
C     SCENEPURP is the scene purpose
C     RIFNAME   is the name of the rif (max 'MBP' per scene possible)


      integer NSCENE,ISCENE
      common/raddata2/NSCENE,ISCENE
C     NSCENE is the number of scenes defined in LRADCF
C     ISCENE is the current scene in use


      integer NBSRIF
      character LBSRIF*72
      common/raddata3/NBSRIF(MCOM+1),LBSRIF(MCOM+1)
C      character*
C      common/raddata3/NBSRIF(MCOM,MNSCN)
C     NBSRIF is the number of RIF files defined for the corresponding scene
C     LBSRIF is the name of the alternative .rif file


      integer NABS
      common/radabs/NABS
C     NABS is the number of alternative blind states (use IBCMT instead?)


      integer mono,intext,ifocz
      common/rad2/mono,intext,ifocz
C     mono
C     intext
C     ifocz   index of current zone ("focus")


      character zone*16,aim*12,act*72
      common/cmddata/zone,aim,act
C      common/cmddata/zone,aim,act
C     zone is the zone number as string
C     aim is the Radiance model result type "Day_coeff", "Coupling", "Create", "External"
C     act is the transfer file, typically ".<ESP-r PID>.dat"


      character LDFGRID*72
      common/radgrid/LDFGRID
C     LDFGRID is ...


      common/radgrpts/NDFP,DFPTS(500,3),DFDIR(3),DFVALS(500)
C     NDFP
C     DFPTS
C     DFDIR
C     DFVALS


      character vewcmds*124,rvewsh*10
      integer indxvew,ipckvew
      common/e2rv/vewcmds(20),rvewsh(20),indxvew,ipckvew
C     vewcmds
C     rvewsh
C     indxvew
C     ipckvew
