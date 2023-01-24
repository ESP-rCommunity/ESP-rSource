C This header relates to integrated performance view entities in ESP-r.
C It is dependant on building.h and include statements to
C seasons.h should be placed after building.h.


C Format and version of IPV.
      integer ipvform     ! 1 human readable text, 2 tab separated, 3 java
      integer ipvversion  ! 3 is older format, 4 is current format
      common/IPVFORMAT/ipvform,ipvversion

      character ipvtitl*40   ! title that shows up in reports
      character ipvsynop*248 ! documents the integrated performance view
      character ipvsimu*6    ! tag describing the assessments to be run
      common/IPVA/ipvtitl,ipvsynop,ipvsimu

C IPV metric sets:
      integer nms     ! number of sets,
      integer imetget ! eres iget index for the metric selected,
      integer imetmsc ! two miscelaneous indices for use by the metric,
      integer nzmg    ! number of associated zones,
      real emgflr     ! total floor area in each set,
      real emgsca     ! scaling factor (e.g. to scale these zones towards a whole building),
      real emgwtg     ! weighting factor for the metric,
      integer izmg    ! list of associated zones.
      common/IPVMS/nms,imetget(MIPVM),imetmsc(MIPVM,2),nzmg(MIPVM),
     &  emgflr(MIPVM),emgsca(MIPVM),emgwtg(MIPVM),izmg(MIPVM,MCOM)

C Performance metric text variables.
      character msdoc*12    ! label for each metric set.
      character metrglbl*20 ! label equivalent to GLABEL in esrures
      character metgroup*12 ! label for the group of associated zones
      common/IPVMDS/metrglbl(MIPVM),msdoc(MIPVM),metgroup(MIPVM)

C List of energy demand sets:
      integer neds     ! number of sets,
      integer idgmsc   ! miscel data describing set,
      integer iaggr    ! 0 = no 1 = yes timestep aggregate reporting
      integer nzedg    ! number of associated zones, 
      real edgflr      ! associated floor area in
      real edgsca      ! scaling factor 
      integer izedg    ! list of associated zones.
      common/IPVEDS/neds,idgmsc(MIPVM,2),iaggr,nzedg(MIPVM),
     &  edgflr(MIPVM),edgsca(MIPVM),izedg(MIPVM,MCOM)

C zedsdoc is 12 char identifier for each demand set.
      character zedsdoc*12
      common/IPVDEDS/zedsdoc(MIPVM)

      integer nipvassmt  ! number of assessments (runs) in an IPV (1=annual,
                         ! 3=win/trn/sum, 5=win/spr/sum/autumn/win. 
      integer ipvastjd   ! assessment start julian dates,
      integer ipvafnjd   ! assessment finish julian dates
      integer nipvdispjd ! number of days to display in detail,
      integer ipvdispjd  ! list of julian days to display for each assessment.
      common/IPVSEA/nipvassmt,nipvdispjd,ipvastjd(MIPVA),
     &              ipvafnjd(MIPVA),ipvdispjd(10)

C ipvadesc is a 40 char description associated with each assessment.
      character ipvadesc*40
      common/IPVSEALB/ipvadesc(MIPVA)

C dm* are the degree-day and day-ratio multipliers between the
C typical periods and the whole season. For nipvassmt=1 these
C are initially set to 1.0.
      real dmheat  ! degree day multiplier for heating demands
      real dmcool  ! degree day multiplier for cooling demands
      real dmlight ! degree day multiplier for lighting demands
      real dmsmlpw ! degree day multiplier for small power demands
      real dmfan   ! degree day multiplier for fan demands
      real dmdhw   ! degree day multiplier for domestic hot water.
      common/CLMDM/dmheat(MSPS),dmcool(MSPS),dmlight(MSPS),
     &              dmsmlpw(MSPS),dmfan(MSPS),dmdhw(MSPS)

C ddm* are the degree-day multipliers (or user defined multipliers) between
C the ACTUAL periods simulated and the whole season for heating, cooling, lights,
C small power, fans, domestic hot water. For nipvassmt=1 these are 
C initially set to 1.0.
      real ddmheat  ! degree day multiplier for heating demands
      real ddmcool  ! degree day multiplier for cooling demands
      real ddmlight ! degree day multiplier for lighting demands
      real ddmsmlpw ! degree day multiplier for small power demands
      real ddmfan   ! degree day multiplier for fan demands
      real ddmdhw   ! degree day multiplier for domestic hot water.
      common/IPVDDM/ddmheat(MIPVA),ddmcool(MIPVA),ddmlight(MIPVA),
     &              ddmsmlpw(MIPVA),ddmfan(MIPVA),ddmdhw(MIPVA)

C Dispersed demands (lifts/pumps/etc.) can be included from a project
C demands file (which defines weekday/saturday/sunday profiles).  These
C demands are appended to any zone based integrations and typical day
C profiles are included in a third 24 hour set of profiles.  No scaling
C is done with data, assumes that the associated areas translate directly
C to the fully scaled project.
      integer  idmdinc !  0 (exclude) or 1 (include) toggle for each type
      common/IPVDMD/idmdinc(MGTY)

C Image files associate with IPV.
      integer nipvimg       ! number of image files associate with IPV
      character lipvimg*72  ! image files associate with IPV    
      common/IPVI/nipvimg,lipvimg(4)
