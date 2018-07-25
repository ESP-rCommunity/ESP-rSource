C This header relates to integrated performance view entities in ESP-r.
C It is dependant on building.h and include statements to
C seasons.h should be placed after building.h.


C Format and version of IPV.
      integer ipvform     ! 1 human readable text, 2 tab separated, 3 java
      integer ipvversion  ! 3 is older format, 4 is current format
      common/IPVFORMAT/ipvform,ipvversion

C ipvtitl (char 40) is the title that shows up in reports
C ipvvers (char 40) is the version of the IPV e.g. 'case with mass walls'
C ipvsynop (char 248) documents the integrated performance view
C ipvsimu (char 6) is a short tag describing the assessments to be run
C   for example 'icautf' is an autumn fortnight and 'i3s' is a three
C   season assessment with all days included.
      character ipvtitl*40,ipvvers*40,ipvsynop*248,ipvsimu*6
      common/IPVA/ipvtitl,ipvvers,ipvsynop,ipvsimu

C List of metric sets: nms is number of sets, imetget is the
C esrures iget index for the metric selected, imetmsc holds two
C miscelaneous indices for use by the metric, nzmg is the
C number of zones in each set, emgflr is the total floor area in
C each set, emgsca is a scaling factor (e.g. to scale these zones
C towards a whole building), emgwtg is a weighting factor for the
C metric, izmg is the list of zones in each set.
      integer nms,imetget,imetmsc,nzmg,izmg
      real emgflr,emgsca,emgwtg
      common/IPVMS/nms,imetget(MIPVM),imetmsc(MIPVM,2),nzmg(MIPVM),
     &  emgflr(MIPVM),emgsca(MIPVM),emgwtg(MIPVM),izmg(MIPVM,MCOM)

C Performance metric text variables.
      character msdoc*12    ! label for each metric set.
      character metrglbl*20 ! label equivalent to GLABEL in esrures
      character metgroup*12 ! label for the group of associated zones
      common/IPVMDS/metrglbl(MIPVM),msdoc(MIPVM),metgroup(MIPVM)

C List of energy demand sets: neds is number of sets, idgmsc() are
C miscel data describing set, iaggr=0 no timestep aggregate reporting
C iaggr=1 timestep aggregate data included, nzedg is the
C number of zones in each set, edgflr is the total floor area in
C each set, edgsca is a scaling factor (e.g. to scale these zones
C towards a whole building), izedg is the list of zones in each set.
      integer neds,idgmsc,iaggr,nzedg,izedg
      real edgflr,edgsca
      common/IPVEDS/neds,idgmsc(MIPVM,2),iaggr,nzedg(MIPVM),
     &  edgflr(MIPVM),edgsca(MIPVM),izedg(MIPVM,MCOM)

C zedsdoc is 12 char identifier for each demand set.
      character zedsdoc*12
      common/IPVDEDS/zedsdoc(MIPVM)

C nipvassmt is the number of assessments (runs) in an IPV (1=annual,
C 3=win/trn/sum, 5=win/spr/sum/autumn/win. 
C ipvastjd() are the assessment start julian dates,
C ipvafnjd are the assessment finish julian dates and
C nipvdispjd number of days to display in detail,
C ipvdispjd is list of julian days to display (must be within one
C   of the assessment periods).
      integer nipvassmt,nipvdispjd,ipvastjd,ipvafnjd,ipvdispjd
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
