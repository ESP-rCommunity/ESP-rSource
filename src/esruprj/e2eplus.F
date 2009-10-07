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

C ESRU EnergyPlus translator to version 1.2.2 to version 2.0 (for testing).
C Current version March 2007.

C E2eplus: Inside and outside materials and composition and schedules generation.

C ******* Translate esp data to EnergyPlus file geom and materials for version 1.4
      SUBROUTINE e2eplus(itrc,ioout,ver,IER)

C itrc (integer) =0 turns off reporting, =1 brief
C ioout (integer) is the file unit to write to
C ver (real) should be 1.2, 1.3, 1.4 or 2.0
#include "building.h"
#include "model.h"
#include "site.h"

C geometry.h provides commons G0/G2/G4/G7/prec2/prec17/precz/GS5/GS6.
#include "geometry.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure database)
C LOPTDB,IOPTDB (for optical database)
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
C ICLIM,LCLIM (for climate file)
      
      integer lnblnk  ! function definition

      common/pophelp/h(60)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)

      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/C24/IZSTOCN(MCOM,MS)

      character DESC*48
      integer NMLC,IPR,LAYERS
      real DTHK,DRAIR
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)
      COMMON/GOPT/DG(5),HG(5),UVAL,VTRN,NTL,AB(ME,5),RF(ME),SRF,SAB

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)
      common/cctlnm/ctldoc,lctlf

C Version of operations file. ip3ver=0 standard, =1 sorted with header
C =2 with more than 3 day types
      common/p3ver/ip3ver(MCOM)

      CHARACTER oprdesc*248,ventdesc*248,ctlstr*24
      COMMON/P1/oprdesc(MCOM),ventdesc(MCOM),ctlstr(MCOM,MDTY)

C Descriptive label for a zone load or casual gain types.
      common/loadlabel/lodlabel(mcom,7)

C Peaks (loadcasp(mcom,7,1) sens)(loadcasp(mcom,7,2) latent) for each
C possible gain type in each zone.
      common/loadcas/loadcasp(mcom,7,2)

C loadonday(zone,daytype,gaintype) is true if there is a gain
C of a particular type on a particular day. Needed to ensure that
C compact schedule is correctly written out.
      common/loadday/loadonday(mcom,7,7)

      COMMON/P3/NCAS1,ICGS1(MC),ICGF1(MC),CMGS1(MC),CMGL1(MC),
     &          RADC1(MC),CONC1(MC),NCAS2,ICGS2(MC),ICGF2(MC),CMGS2(MC),
     &          CMGL2(MC),RADC2(MC),CONC2(MC),NCAS3,ICGS3(MC),ICGF3(MC),
     &          CMGS3(MC),CMGL3(MC),RADC3(MC),CONC3(MC)
      COMMON/P3TYPE/ICGT1(MC),ICGT2(MC),ICGT3(MC)

C Simulation parameter presets.
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/spflper/isstday(MSPS),isstmon(MSPS),isfnday(MSPS),
     &               isfnmon(MSPS)
      COMMON/CLMDT1/CLMLOC
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)

C Arrays of material properties.
      real matver
      integer matcats,matcatitems,matdbitems
      character matdbdate*24,matdbdoc*248,matcatname*32,matcatdoc*248
      common/matheader/matver,matcats,matdbitems,matcatitems(MGCL),
     &  matdbdate,matdbdoc,matcatname(MGCL),matcatdoc(MGCL)

      real matdbcon,matdbden,matdbsht,matdboute,matdbine
      real matdbouta,matdbina,matdbdrv,matdbthick
      common/matdatarray/matlegindex(MGIT),matcatindex(MGIT),
     &  matdbcon(MGIT),matdbden(MGIT),matdbsht(MGIT),matdboute(MGIT),
     &  matdbine(MGIT),matdbouta(MGIT),matdbina(MGIT),matdbdrv(MGIT),
     &  matdbthick(MGIT)

      integer mathash
      common/mathasharray/mathash(0:600)

      character matname*32,matdoc*248,matopaq*1
      common/matnamearray/matname(MGIT),matdoc(MGIT),matopaq(MGIT)

      integer legmat  ! local variable for legacy index.
      integer matarrayindex ! the indes within matdatarray
      logical closemat

      character GDESCR*36
      CHARACTER NAM*72,TITL*72,outs*124,h*72
      CHARACTER SOTHER*24,SNAME*12,SMLCN*32,SVFC*4,SOTF*32,SUSE*8
      CHARACTER SPARENT*12
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12,CLMLOC*30,LCFGF*72
      character cfgroot*24
      character ctldoc*248,LCTLF*72
      character ZSDES*28,ZSDESC*20,ZSDESS*16,mlcname*12
      character dstmp*24
      CHARACTER LAPROB*72
      CHARACTER*6 lodlabel

C iprndx saves unique materials references,
C thkiprndx holds the thickness of each unique materials reference,
C nameiprndx (char 25) is the compound name of each unique material.
C iglassndx = 0 if REGULAR and 1 if WindowGlass
C riair for potentially unique air gap resistences.
C Note: EnergyPlus holds each glazing layer properties independently
C       rather than for the whole construction.
C soltrn is singular layer solar transmittance (normal)
C solrefl is singular layer solar reflectance (normal)
C vistrn is singular layer visible trans (normal)
C visrefl is singular layer visible reflectance (normal)
      dimension iprndx(100),thkiprndx(100),optndx(100),iglassndx(100)
      dimension riair(100),soltrn(100),solrefl(100),vistrn(100)
      dimension visrefl(100),nameiprndx(100)

C Each construction needs to remember, for each layer the index
C of the material that was created or found for it.
      dimension iconmatpointer(100,8)
      dimension areamlc(MCOM)

C To hold coordinates at the corners of surfaces (in case a complex
C surface can be resolved into a quad).
      DIMENSION COORD(MV,3)
      real loadcasp,ver  ! file version passed to subroutine
      integer iver    ! file version as integer
      character optndx*12,nameiprndx*25,OPT*12,com*1,sim*1
      character act*1,layname*25
      character T25*25,T25a*25,t12a*12
      character*10 wkday(7)
      character*9 RAMONTH(12)

      logical found,close,close1,foundmlc,closea,closeb,closec,closed
      logical haveobs,haveish,havehc,haveusrgrnd,havestdgrnd,XST
      logical loadonday,ok,dok
      logical newgeo  ! to use for testing if new/old geometry file.

C Viewfactor to ground.
      dimension PROPG(8)
      DATA PROPG/0.36,0.41,0.45,0.33,0.33,0.5,0.0,0.0/
      data wkday/'Monday', 'Tuesday','Wednesday', 'Thursday',
     &           'Friday','Saturday', 'Sunday'/
      DATA RAMONTH/'January  ','February ','March    ','April    ',
     &             'May      ','June     ','July     ','August   ',
     &             'September','October  ','November ','December '/

C Assume configuration file is from IFIL+5, any leakage description
C is fom IFIL+6, revised config file on IFIL+3, geometry on
C IFILE+2 and ASCII viewing and geometry file reading on IFILE+1. 
      ITA1 = IFIL+8

C Set character variable com to be a single comma and sim to be a
C simicolon. 
      com=','
      sim=';'
      newgeo=.false.  ! assume older format geometry.

C Check the version number and set iver.
      iver = 00
      call eclose(ver,1.2,0.001,closea)
      call eclose(ver,1.3,0.001,closeb)
      call eclose(ver,1.4,0.001,closec)
      call eclose(ver,2.0,0.001,closed)
      if(closea) iver = 12
      if(closeb) iver = 13
      if(closec) iver = 14
      if(closed) iver = 20

C Open the primitive and mlc db.
      call opendb(ier)

C First task is to loop through each of the mlc and get a
C list of primitives used in this problem. Clear the local
C arrays. Assumes that there are no more than 100 materials
C and constructions used in the model.
      do 17 ii=1,100
        iprndx(ii)=-1
        thkiprndx(ii)=0.0
        iglassndx(ii)=0
        riair(ii)=0.0
        soltrn(ii)=0.0
        solrefl(ii)=0.0
        vistrn(ii)=0.0
        visrefl(ii)=0.0
        nameiprndx(ii)=' '
        optndx(ii)=' '
        iconmatpointer(ii,1)=0
        iconmatpointer(ii,2)=0
        iconmatpointer(ii,3)=0
        iconmatpointer(ii,4)=0
        iconmatpointer(ii,5)=0
        iconmatpointer(ii,6)=0
        iconmatpointer(ii,7)=0
        iconmatpointer(ii,8)=0
  17  continue
      iij=0

C Write header of file.
      call dstamp(dstmp)
      WRITE(ioout,'(a)',iostat=ios,err=2)  '!- ESP-r -> EnergyPlus'
      WRITE(ioout,'(2a)',iostat=ios,err=2) '!- MODEL based on ',
     &  LCFGF(1:lnblnk(LCFGF))
      WRITE(ioout,'(2a)',iostat=ios,err=2) '!- ',
     &  modeltitle(1:lnblnk(modeltitle))
      WRITE(ioout,'(2a)',iostat=ios,err=2) '!- DATE: ',dstmp
      WRITE(ioout,'(a)',iostat=ios,err=2)  ' '
      if(iver.eq.12)then
        WRITE(ioout,'(5a)',iostat=ios,err=2) 'VERSION',com,'1.2',sim,
     &  '   !- Version Identifier'
      elseif(iver.eq.13)then
        WRITE(ioout,'(5a)',iostat=ios,err=2) 'VERSION',com,'1.3',sim,
     &  '   !- Version Identifier'
      elseif(iver.eq.14)then
        WRITE(ioout,'(5a)',iostat=ios,err=2) 'VERSION',com,'1.4',sim,
     &  '   !- Version Identifier'
      elseif(iver.eq.20)then
        WRITE(ioout,'(5a)',iostat=ios,err=2) 'VERSION',com,'2.0',sim,
     &  '   !- Version Identifier (for testing only)'
      endif
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      WRITE(ioout,'(2a)',iostat=ios,err=2) 'BUILDING',com
      WRITE(ioout,'(4a)',iostat=ios,err=2) '  ',
     &  cfgroot(1:lnblnk(cfgroot)),com,'   !- Building (root) name.'
      WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','0.000',com,
     &  '   !- North Axis {deg}'

C Interpret site position.
      if(siteexposureindex.eq.1)then
        WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','City',com,
     &    '   !- Terrain'
      elseif(siteexposureindex.eq.2)then
        WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','Suburbs',com,
     &    '   !- Terrain'
      elseif(siteexposureindex.eq.3)then
        WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','Country',com,
     &    '   !- Terrain'
      elseif(siteexposureindex.eq.4)then
        WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','City',com,
     &    '   !- Terrain'
      elseif(siteexposureindex.eq.5)then
        WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','City',com,
     &    '   !- Terrain'
      elseif(siteexposureindex.eq.6)then
        WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','Country',com,
     &    '   !- Terrain'
      elseif(siteexposureindex.eq.7)then
        WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','City',com,
     &    '   !- Terrain'
      elseif(siteexposureindex.eq.8)then
        WRITE(ioout,'(3a)',iostat=ios,err=2) '  ',com,
     &    '   !- Terrain (user specified in ESP-r)'
      endif
      if(iver.lt.14)then
        WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','0.04',com,
     &    '   !- Loads Convergence Tolerance Value {W}'
      else
        WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','0.04',com,
     &    '   !- Loads Convergence Tolerance Value (frac of load)'
      endif
      WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','0.4',com,
     &  '   !- Temperature Convergence Tolerance Value {deltaC}'

C Interpret solar distribution field. If any of the ESP-r zones
C has had shading calculated set to `FullInteriorAndExterior`
C and write a comment to the user to check if zone is convex.
C Otherwise set to `FullExterior` is any zones have obstruction
C blocks, otherwise set to `MinimalShadowing`.
      haveobs=.false.
      haveish=.false.
      havehc=.false.
      do 19 iz=1,ncomp
        if(IOBS(iz).eq.1) haveobs=.true.
        if(IOBS(iz).eq.2) haveobs=.true.
        if(ISI(iz).eq.1) haveish=.true.
        if(IHC(iz).eq.1)havehc=.true.
  19  continue
      if(haveish)then
        WRITE(ioout,'(4a)',iostat=ios,err=2) '  ',
     &    'FullInteriorAndExterior',com,'   !- Solar Distribution'
      else
        if(haveobs)then
          WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','FullExterior',com,
     &      '   !- Solar Distribution'
        else
          WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','MinimalShadowing',
     &      com,'   !- Solar Distribution'
        endif
      endif

C Interpret warmup days. Use simulation parameter set value or calculate
C via call to scntcnst.
      if(nsset.gt.0)then
        WRITE(ioout,'(a,i2,2a)',iostat=ios,err=2) '  ',isstup,sim,
     &    '   !- Maximum Number of Warmup Days (from sim param set)'
      else
         call scntcnst(TDM,istd,TCM,ISTC)
        WRITE(ioout,'(a,i2,2a)',iostat=ios,err=2) '  ',ISTC,sim,
     &      '   !- Maximum Number of Warmup Days (from MLC TCM)'
      endif

C Interpret timesteps. Use simulation parameter set value if <= 6 or set to one.
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      WRITE(ioout,'(2a)',iostat=ios,err=2) 'TIMESTEP IN HOUR',com
      if(nsset.gt.0)then
        if(isbnstep.gt.6)then
          WRITE(ioout,'(3a,i2,a)',iostat=ios,err=2) '   6',sim,
     &      '   !- Time Step in Hour (sim param set was ',isbnstep,')'
        else
          WRITE(ioout,'(a,i2,2a)',iostat=ios,err=2) '  ',isbnstep,sim,
     &      '   !- Time Step in Hour (from sim param set)'
        endif
      else
        WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','4',sim,
     &    '   !- Time Step in Hour (default)'
      endif

C Inside convection. If there are convection regime files
C give note to user to check. Set `Detailed`.
C << note there is also a zone field to over-ride >>
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      WRITE(ioout,'(2a)',iostat=ios,err=2) 
     &  'INSIDE CONVECTION ALGORITHM',com
      if(havehc)then
        WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','Detailed',sim,
     &    '   !- [please check equivalent ESP-r HC regime]'
      else
        WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','Detailed',sim,
     &    '   !- Algorithm (default)'
      endif

      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      WRITE(ioout,'(2a)',iostat=ios,err=2) 
     &  'OUTSIDE CONVECTION ALGORITHM',com
        WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','Detailed',sim,
     &    '   !- Algorithm (default)'

C Interpret air flow model. If ESP-r has a flow network set to
C `COMIS` otherwise set to `Simple`. This is only an applicable
C option for versions before 1.3.
      if(iver.lt.13)then
        WRITE(ioout,'(a)',iostat=ios,err=2) ' '
        WRITE(ioout,'(2a)',iostat=ios,err=2) 'AIRFLOW MODEL',com
        if(IAIRN.ge.1)then
          WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','COMIS',sim,
     &      '   !- (ESP-r includes flow network)'
        else
          WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','Simple',sim,
     &      '   !- (default)'
        endif
      endif

C Solution - assume no moisture solution.
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      WRITE(ioout,'(2a)',iostat=ios,err=2) 'SOLUTION ALGORITHM',com
      WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','CTF',sim,
     &  '   !- [CTF|MTF|EMPD])'

C Interpret shading calculations. If shading used in ESP-r then
C set to 28 days, otherwise set zero=default.
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      WRITE(ioout,'(2a)',iostat=ios,err=2) 'SHADOWING CALCULATIONS',com
      if(haveish)then
        WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','28',sim,
     &    '   !- (to match ESP-r ish period)'
      else
        WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','20',sim,
     &    '   !- (default frequency of calculation)'
      endif

C Run control - set all fields to no so only run period is assessed.
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      WRITE(ioout,'(2a)',iostat=ios,err=2) 'RUN CONTROL',com
      WRITE(ioout,'(8a)',iostat=ios,err=2) '  ','No',com,'No',com,'No',
     &  com,'   !- no zone sizing or system sizing or plant sizing'
      WRITE(ioout,'(6a)',iostat=ios,err=2) '  ','No',com,'Yes',sim,
     &  '   !- no design day - use weather file'

C Interpret run period. Use simulation parameter set values or set default.
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      WRITE(ioout,'(2a)',iostat=ios,err=2) 'RunPeriod',com
      if(nsset.gt.0)then
        WRITE(ioout,'(a,i2,a,i2,2a)',iostat=ios,err=2) '  ',isstmon(1),
     &    com,isstday(1),com,
     &    '   !- Begin Month & Day Of Month (from sim param set)'
        WRITE(ioout,'(a,i2,a,i2,2a)',iostat=ios,err=2) '  ',isfnmon(1),
     &    com,isfnday(1),com,
     &    '   !- End Month & Day Of Month (from sim param set)'
        CALL EWEEKD(isstday(1),isstmon(1),IYEAR,IDWK)
      else
        WRITE(ioout,'(5a)',iostat=ios,err=2) '  ','1',com,'1',com,
     &    '   !- Begin Month & Day Of Month (default)'
        WRITE(ioout,'(5a)',iostat=ios,err=2) '  ','12',com,
     &    '30',com,'   !- End Month & Day Of Month (default)'
        CALL EWEEKD(1,1,IYEAR,IDWK)
      endif

C Interpret day of week for start day from project year.
      WRITE(ioout,'(4a)',iostat=ios,err=2) '  ',
     &  wkday(idwk)(1:lnblnk(wkday(idwk))),com,
     &  '   !- Day Of Week For Start Day'
      WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','No',com,
     &  '   !- Use WeatherFile Holidays/Special Days'
      WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','No',com,
     &  '   !- Use WeatherFile DaylightSavingPeriod'
      WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','No',com,
     &  '   !- Apply Weekend Holiday Rule'
      WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','No',com,
     &  '   !- Use WeatherFile Rain Indicators'
      WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','No',sim,
     &  '   !- Use WeatherFile Snow Indicators'

C Interpret location from climate file location name, but use
C the configuration file data for lat and long.
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      WRITE(ioout,'(2a)',iostat=ios,err=2) 'Location',com
      call EFOPRAN(ICLIM,LCLIM,144,0,IER)
      if(ier.eq.0)then
        IREC=367
        READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=103)CLMLOC
        CALL ERPFREE(ICLIM,ISTAT)
        WRITE(ioout,'(4a)',iostat=ios,err=2) '  ',
     &    CLMLOC(1:lnblnk(CLMLOC)),com,'   !- LocationName'
      else
        WRITE(ioout,'(4a)',iostat=ios,err=2) '  ','not_yet_set',com,
     &    '   !- LocationName'
      endif
      WRITE(ioout,'(a,f7.3,2a)',iostat=ios,err=2) '  ',sitelat,com,
     &  '   !- Latitude {deg}'

      WRITE(ioout,'(a,f7.3,2a)',iostat=ios,err=2) '  ',sitelongdif,com,
     &  '   !- Longitude {deg} [check this value]'

C Ask user where the current time meridian is.
      IIC2=0
      h(1)='EnergyPlus needs to know the offset in hours from GTM.'
      h(2)='West is negative, East is positive. '
      h(3)=' '
      h(4)='The cancel option will skip the creation of the '
      h(5)='EnergyPlus descriptive file time zone section.'
      IIC2T=IIC2
      CALL EASKI(IIC2T,' Time zone (hours +- GTM) ',
     &  ' ',-11,'F',11,'F',1,'time zone difference',IERI,5)
      if(ieri.eq.-3)then
        WRITE(ioout,'(3a)',iostat=ios,err=2) '  0',com,
     &    '   !- TimeZone {hr} [not yet supplied]'
      else
        iic2=iic2t
        WRITE(ioout,'(a,i3,2a)',iostat=ios,err=2) '  ',iic2,com,
     &    '   !- TimeZone {hr} [supplied by user]'
      endif

      h(1)='EnergyPlus needs to know the site elevation in metres'
      h(2)='above sea level. '
      SLM=10.0
      CALL EASKR(SLM,' Site elevation above sea level (m) ',
     &  ' ',-100.0,'F',10000.0,'F',10.0,'site elevation',IER,2)
      WRITE(ioout,'(a,f7.1,2a)',iostat=ios,err=2) '  ',SLM,sim,
     &  '   !- Elevation {m} [supplied by user]'

C Interpret ground temperatures. Loop through each connection
C and check which standard or user defined ground temperature
C profile is being used. EnergyPlus can only take one so if
C there is more than one referenced warn the user. There is
C no equivalent to NRCan BASESIMP.
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      WRITE(ioout,'(2a)',iostat=ios,err=2) 'GROUNDTEMPERATURES',com
      haveusrgrnd=.false.
      havestdgrnd=.false.
      do 18 icc=1,NCON
        if(ICT(icc).eq.4.and.IC2(icc).gt.0)then
          havestdgrnd=.true.
          igrp=IC2(icc)
        endif
        if(ICT(icc).eq.4.and.IE2(icc).gt.0)then
          haveusrgrnd=.true.
          igrp= IE2(icc)
        endif
 18   continue

C Check if there are user defined ground temps.
      IF(NGRDP.gt.0)haveusrgrnd=.true.
      if(haveusrgrnd.and.havestdgrnd)then
        WRITE(ioout,'(a)',iostat=ios,err=2)
     &    '!- multiple ground temps referenced in ESP-r'
      elseif(.NOT.havestdgrnd.and.haveusrgrnd)then
        WRITE(ioout,'(a)',iostat=ios,err=2)
     &    '!- user supplied ground temps ESP-r'
      elseif(.NOT.haveusrgrnd.and.havestdgrnd)then
        WRITE(ioout,'(a,i2)',iostat=ios,err=2)
     &    '!- standard ground profile ',igrp
      else
        WRITE(ioout,'(a)',iostat=ios,err=2) 
     &    '!- no ground connections in ESP-r'
      endif

      if((.NOT.haveusrgrnd).and.(.NOT.havestdgrnd))then
        continue
      else
        do 16 im=1,11
          if(haveusrgrnd)then
            WRITE(ioout,'(a,f7.4,4a)',iostat=ios,err=2) '  ',
     &        UGRDTP(im,igrp),com,'   !- ',RAMONTH(im),
     &        ' Ground Temperature {C}'
          else
            WRITE(ioout,'(a,f7.4,4a)',iostat=ios,err=2) '  ',
     &        GRDTMP(im,igrp),com,'   !- ',RAMONTH(im),
     &        ' Ground Temperature {C}'
          endif
  16    continue
        if(haveusrgrnd)then
          WRITE(ioout,'(a,f7.4,4a)',iostat=ios,err=2) '  ',
     &      UGRDTP(12,igrp),sim,'   !- ',RAMONTH(12),
     &      ' Ground Temperature {C}'
        else
          WRITE(ioout,'(a,f7.4,4a)',iostat=ios,err=2) '  ',
     &      GRDTMP(12,igrp),sim,'   !- ',RAMONTH(12),
     &      ' Ground Temperature {C}'
        endif
      endif


C Scan for materials.
      DO 20 INO=1,NMLC

C Loop through each construction in the database, check if it
C is used in the model and if so proceed to check.
        mlcname=DESC(ino)(1:12)
        call mlcrefs(mlcname,areamlc,tareamlc,foundmlc)
        if(.NOT.foundmlc) goto 20

C If the construction is transparent then the materials need
C to be marked as WindowGlass. Curently ESP-r holds optics for
C the overall construction and for single glazing this will the
C the same for EnergyPlus. For multiple layers the data is not
C quite correct. Assume that visible reflection is based on
C visible trans and solar absorb.
        WRITE(OPT,'(A)') DESC(INO)(21:32)
        if(opt(1:6).eq.'OPAQUE')then
          continue
        else
          itrco=0
          CALL EROPTDB(ITRCO,ITRU,OPT,GDESCR,IER)
        endif
        DO 23 IL=1,LAYERS(INO)

C For each layer get primitive db ref and then check either thickness
C or air gap resistance before adding it to the iprndx array.
          if(iij.gt.0)then
            close1 = .false.
            do 24 ij = 1,iij
              found = .false.
              if(IPR(INO,IL).eq.iprndx(ij))found = .true.
              if(IPR(INO,IL).eq.0)then

C If an opaque air gap, we have a unique one if the air gap resistence
C is different (thickness does not matter for an opaque construction
C but it does for a window). If a match found remember
C the name for this layer of this construction
                if(opt(1:6).eq.'OPAQUE')then
                  call eclose(DRAIR(ino,IL,1),riair(ij),0.001,close)
                else
                  call eclose(DTHK(ino,IL),thkiprndx(ij),0.001,close)
                endif
                if(found.and.close)then
                  close1 = .true.
                endif
              else

C If solid then check if thickness is unique.
                close=.false.
                call eclose(DTHK(ino,IL),thkiprndx(ij),0.001,close)
                if(found.and.close)then
                  close1 = .true.
                endif
              endif
  24        continue
            if(.NOT.close1)then
              iij=iij+1
              iprndx(iij)=IPR(INO,IL)
              thkiprndx(iij)=DTHK(ino,IL)
              if(opt(1:6).eq.'OPAQUE')then
                iglassndx(iij)=0
                optndx(iij)=OPT
              else
                iglassndx(iij)=1
                optndx(iij)=OPT
                soltrn(iij)=DG(1)
                solrefl(iij)= (1.0-(DG(1)+AB(IL,1)))
                vistrn(iij)=VTRN
                visrefl(iij)= (1.0-(VTRN+AB(IL,1)))

C If this approximation yields a negative reflectance try 1-vtrn.
                if(visrefl(iij).lt.0.0)then
                  visrefl(iij)= 1.0-VTRN
                endif
              endif

C If an opaque air gap remember air gap resistence and clear thickness.
C If a window air gap remember thickness and resetn airgap res to 0.0.
              if(IPR(INO,IL).eq.0)then
                if(opt(1:6).eq.'OPAQUE')then
                  riair(iij)=DRAIR(ino,IL,1)
                  thkiprndx(iij)=0.0
                else
                  riair(iij)=0.0
                  thkiprndx(iij)=DTHK(ino,IL)
                endif
              else
                continue
              endif
            endif
          else

C First material (which will never be air).
            iij=iij+1
            iprndx(iij)=IPR(INO,IL)
            thkiprndx(iij)=DTHK(ino,IL)
            if(opt(1:6).eq.'OPAQUE')then
              iglassndx(iij)=0
              optndx(iij)=OPT
            else
              iglassndx(iij)=1
              optndx(iij)=OPT
              soltrn(iij)=DG(1)
              solrefl(iij)= (1.0-(DG(1)+AB(IL,1)))
              vistrn(iij)=VTRN
              visrefl(iij)= (1.0-(VTRN+AB(IL,1)))
            endif
          endif
  23    continue
  20  continue
      nmat=iij

C Now, for each unique material write description.
      WRITE(ioout,'(a)') ' '
      WRITE(ioout,'(a)') '!- ======= MATERIAL ======'

      call eclose(matver,1.1,0.01,closemat)
      do 25 iy = 1,nmat
        if(iprndx(iy).gt.0)then
          WRITE(ioout,'(a)',iostat=ios,err=3) ' '
          if(iglassndx(iy).eq.0)then
            WRITE(ioout,'(2a)',iostat=ios,err=3) 'MATERIAL:REGULAR',com
          else
            WRITE(ioout,'(2a)',iostat=ios,err=3) 
     &        'MATERIAL:WINDOWGLASS',com
          endif
          if(closemat)then
            legmat=iprndx(iy)   ! which legacy index
            if(legmat.ge.0)then
              matarrayindex=mathash(legmat)  ! find matching array index
              DBCON=matdbcon(matarrayindex)
              DBDEN=matdbden(matarrayindex)
              DBSHT=matdbsht(matarrayindex)
              E=matdbine(matarrayindex)
              A=matdbina(matarrayindex)
              DRV=matdbdrv(matarrayindex)
              write(TITL,'(a)') 'placeholder'
              write(NAM,'(a)') matname(matarrayindex)(1:32)
            endif
          else
            CALL ERPCDB(IFMAT,iprndx(iy),ITRU,IER,DBCON,DBDEN,
     &        DBSHT,E,A,DRV,TITL,NAM)
          endif

C Make up name. t12a holds the thickness as a string w/o leading blanks
C (but only use the initial 7 characters of t12a.
C Take up to 12 char from NAM and the remove any blanks via st2name.
          ln = MIN0(lnblnk(NAM),12)
          call relstr(thkiprndx(iy),t12a,lna,ier)
          write(t25,'(3a)') NAM(1:ln),':',t12a(1:7)
          call st2name(T25,T25a)
          write(nameiprndx(iy),'(a)') T25a
          WRITE(ioout,'(4a)',iostat=ios,err=3) '  ',
     &      T25a(1:lnblnk(T25a)),com,'   !- Name'
          if(iglassndx(iy).eq.0)then
            WRITE(ioout,'(4a)',iostat=ios,err=3) '  ','Rough',com,
     &        '   !- Roughness'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) '  ',
     &        thkiprndx(iy),com,'   !- Thickness {m}'
            if(dbcon.gt.99.0)then
              WRITE(ioout,'(a,f9.2,2a)',iostat=ios,err=3) '  ',DBCON,
     &          com,'   !- Conductivity {W/m-K}'
            else
              WRITE(ioout,'(a,f7.4,2a)',iostat=ios,err=3) '  ',DBCON,
     &          com,'   !- Conductivity {W/m-K}'
            endif
            WRITE(ioout,'(a,f9.4,2a)',iostat=ios,err=3) '  ',DBDEN,com,
     &        '   !- Density {kg/m3}'
            WRITE(ioout,'(a,f7.2,2a)',iostat=ios,err=3) '  ',DBSHT,com,
     &        '   !- Specific Heat {J/kg-K}'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) '  ',E,com,
     &        '   !- Absorptance:Thermal'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) '  ',A,com,
     &        '   !- Absorptance:Solar'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) '  ',A,sim,
     &        '   !- Absorptance:Visible'
          else
            WRITE(ioout,'(4a)',iostat=ios,err=3) '  ',
     &        'SpectralAverage',com,'   !- Optical Data TypeRoughness'
            WRITE(ioout,'(3a)',iostat=ios,err=3) '  ',com,
     &        '   !- Name of Window Glass Spectral Data Set'
            WRITE(ioout,'(a,f7.4,2a)',iostat=ios,err=3) '  ',
     &        thkiprndx(iy),com,'   !- Thickness {m}'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) '  ',
     &        soltrn(iy),com,
     &        '   !- Solar Transmittance at Normal Incidence'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) '  ',
     &        solrefl(iy),com,
     &        '   !- Solar Reflectance at Normal Incidence: Front'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) '  ',
     &        solrefl(iy),com,
     &        '   !- Solar Reflectance at Normal Incidence: Back'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) '  ',
     &        vistrn(iy),com,
     &        '   !- Visible Transmittance at Normal Incidence'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) '  ',
     &        visrefl(iy),com,
     &        '   !- Visible Reflectance at Normal Incidence: Front'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) '  ',
     &        visrefl(iy),com,
     &        '   !- Visible Reflectance at Normal Incidence: Back'
            WRITE(ioout,'(4a)',iostat=ios,err=3) '  ','0.00',com,
     &        '   !- IR Transmittance at Normal Incidence'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) '  ',E,com,
     &        '   !- IR Hemispherical Emissivity: Front'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) '  ',E,com,
     &        '   !- IR Hemispherical Emissivity: Back'
            WRITE(ioout,'(a,f7.4,2a)',iostat=ios,err=3) '  ',DBCON,sim,
     &        '   !- Conductivity {W/m-K}'
          endif
        else

C Make up air name for opaque construction. t12a holds air gap resistance
C as a string w/o leading blanks (use only first 7 char of t12a).
          if(iglassndx(iy).eq.0)then
            WRITE(ioout,'(a)',iostat=ios,err=3) ' '
            WRITE(ioout,'(2a)',iostat=ios,err=3) 'MATERIAL:AIR',com
            call relstr(riair(iy),t12a,lna,ier)
            write(t25,'(3a)') 'Air-gap',':',t12a(1:7)
            call st2name(T25,T25a)
            write(nameiprndx(iy),'(a)') T25a
            WRITE(ioout,'(4a)',iostat=ios,err=3) '  ',
     &        T25a(1:lnblnk(T25a)),com,'   !- Name'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) '  ',riair(iy),
     &        sim,'   !- Thermal Resistance {m2-K/W}'
          else

C For the case of an air gap in a window, assume air is the gas and
C write out the thickness (rather than the air gap resistence, but only
C the first 7 characters of t12a).
            WRITE(ioout,'(a)',iostat=ios,err=3) ' '
            WRITE(ioout,'(2a)',iostat=ios,err=3) 'MATERIAL:WINDOWGAS',
     &        com
            call relstr(thkiprndx(iy),t12a,lna,ier)
            write(t25,'(3a)') 'Air-win',':',t12a(1:7)
            call st2name(T25,T25a)
            write(nameiprndx(iy),'(a)') T25a
            WRITE(ioout,'(4a)',iostat=ios,err=3) '  ',
     &        T25a(1:lnblnk(T25a)),com,'   !- Name'
            WRITE(ioout,'(4a)',iostat=ios,err=3) '  ','Air',com,
     &        '   !- Gas Type'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) '  ',
     &        thkiprndx(iy),sim,'   !- Thickness {m}'
          endif
        endif
  25  continue

C Now that materials are known, look back over constructions
C and assign iconmatpointer for each layer.
      DO 200 INO=1,NMLC

C Loop through each construction in the database, check if it
C is used in the model and if so proceed to check.
        mlcname=DESC(ino)(1:12)
        call mlcrefs(mlcname,areamlc,tareamlc,foundmlc)
        if(.NOT.foundmlc) goto 200

        WRITE(OPT,'(A)') DESC(INO)(21:32)
        DO 230 IL=1,LAYERS(INO)

C For each layer get primitive db ref and then check either thickness
C or air gap resistance before adding it to the iprndx array.
          close1 = .false.
          do 240 ij = 1,nmat
            found = .false.
            if(IPR(INO,IL).eq.iprndx(ij))found = .true.
            if(IPR(INO,IL).eq.0)then

C If air gap, we have a unique one if the air gap resistence
C is different (thickness does not matter for an opaque construction
C but it does for a window). If a match found remember
C the name for this layer of this construction
              if(opt(1:6).eq.'OPAQUE')then
                call eclose(DRAIR(ino,IL,1),riair(ij),0.001,close)
              else
                call eclose(DTHK(ino,IL),thkiprndx(ij),0.001,close)
              endif
              if(found.and.close)then
                close1 = .true.
                iconmatpointer(ino,IL)=ij

C Debug.
C                write(6,*) 'air mat->layer',ino,il,ij,iprndx(ij),
C     &            thkiprndx(ij),iglassndx(ij),riair(ij)

              endif
            else

C If solid then check if thickness is unique.
              close=.false.
              call eclose(DTHK(ino,IL),thkiprndx(ij),0.001,close)
              if(found.and.close)then
                close1 = .true.
                iconmatpointer(ino,IL)=ij

C Debug.
C                write(6,*) 'mat->layer',ino,il,ij,iprndx(ij),
C     &            thkiprndx(ij),iglassndx(ij),nameiprndx(ij)

              endif
            endif
 240      continue
 230    continue
 200  continue

C Now for each construction.
      WRITE(ioout,'(a)',iostat=ios,err=4) ' '
      WRITE(ioout,'(a)',iostat=ios,err=4) 
     &  '!- ======= CONSTRUCTION ======'

      do 36 ino = 1,NMLC
        mlcname=DESC(ino)(1:12)
        call mlcrefs(mlcname,areamlc,tareamlc,foundmlc)
        if(.NOT.foundmlc) goto 36

        WRITE(ioout,'(a)',iostat=ios,err=4) ' '
        WRITE(ioout,'(2a)',iostat=ios,err=4) 'CONSTRUCTION',com
        WRITE(ioout,'(4a)',iostat=ios,err=4) '  ',
     &    mlcname(1:lnblnk(mlcname)),com,'   !- Name'    
        do 37 il = 1,LAYERS(INO)
          ilamat=iconmatpointer(ino,IL)
          if(ilamat.eq.0)then
            layname='UNKNOWN'
          else
            layname=nameiprndx(iconmatpointer(ino,IL))
          endif 
          layl=lnblnk(layname)   
          if(il.eq.1)then
            if(il.eq.LAYERS(INO))then
              WRITE(ioout,'(4a)',iostat=ios,err=4) '  ',
     &          layname(1:layl),sim,'   !- Outside Layer' 
            else                          
              WRITE(ioout,'(4a)',iostat=ios,err=4) '  ',
     &          layname(1:layl),com,'   !- Outside Layer'
            endif
          elseif(il.eq.LAYERS(INO))then
            WRITE(ioout,'(4a,i1)',iostat=ios,err=4) '  ',
     &        layname(1:layl),sim,'   !- Layer #',IL
          else
            WRITE(ioout,'(4a,i1)',iostat=ios,err=4) '  ',
     &        layname(1:layl),com,'   !- Layer #',IL
          endif
  37    continue
  36  continue

C Typical schedule types.
      WRITE(ioout,'(a)',iostat=ios,err=7) ' '
      WRITE(ioout,'(a)',iostat=ios,err=7) 
     &  '!- ============= typical schedule types'
      WRITE(ioout,'(5a)',iostat=ios,err=7) 'Scheduletype',com,
     &  'Any Number',sim,'   !- Not limited.'
      WRITE(ioout,'(9a)',iostat=ios,err=7) 'Scheduletype',com,
     &  'Fraction',com,
     &  '0.0:1.0',com,'CONTINUOUS',sim,'   !- between 0.0 and 1.0.'
      WRITE(ioout,'(9a)',iostat=ios,err=7) 'Scheduletype',com,
     &  'Temperature type',com,
     &  '-60:20.0',com,'CONTINUOUS',sim,'   !- temperature range.'
      WRITE(ioout,'(9a)',iostat=ios,err=7) 'Scheduletype',com,
     &  'On/Off type',com,
     &  '0.0:40.0',com,'DISCRETE',sim,'   !- zero or one.'

C Write out a generic nothing-is-happening schedule for zones which
C have a null or all zero operation file.
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        WRITE(ioout,'(a)',iostat=ios,err=7) 
     &    '! Schedules for zones where nothing happens'
        WRITE(ioout,'(2a)',iostat=ios,err=7) 'SCHEDULE:COMPACT',com
        WRITE(ioout,'(3a)',iostat=ios,err=5) '  NOTHING',com,
     &    '   !- Name'
        WRITE(ioout,'(3a)',iostat=ios,err=7) '  Fraction',com,
     &    '   !- ScheduleType' 
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  Through: 12/31',com
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: Weekdays',com
        WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &    '24:00',com,'0.0',com
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: Saturday',com
        WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &    '24:00',com,'0.0',com
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: Sunday',com
        WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &    '24:00',com,'0.0',com
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: AllOtherDays',com
        WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &    '24:00',com,'0.0',sim

C Write out a generic it always-happens schedule.
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        WRITE(ioout,'(a)',iostat=ios,err=7) 
     &    '! Schedules for zones where something always happens'
        WRITE(ioout,'(2a)',iostat=ios,err=7) 'SCHEDULE:COMPACT',com
        WRITE(ioout,'(3a)',iostat=ios,err=5) '  CONSTANT',com,
     &    '   !- Name'
        WRITE(ioout,'(3a)',iostat=ios,err=7) '  Fraction',com,
     &    '   !- ScheduleType' 
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  Through: 12/31',com
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: Weekdays',com
        WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &    '24:00',com,'1.0',com
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: Saturday',com
        WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &    '24:00',com,'1.0',com
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: Sunday',com
        WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &    '24:00',com,'1.0',com
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: AllOtherDays',com
        WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &    '24:00',com,'1.0',sim

C Compact schedules ( lighting and small power) for zones.
C Logic is as follows:
C  a) scan each zone operations file in turn
C  b) determin maximum gains (absolute W) for occupancy, lighting & small power
C  c) write out a compact schedule using the zone name and casual gain type label.
C  d) write LIGHTING and EQUIPMENT entries for the current zone
C     using the maximum sensible+latent for each day type and gain
C     type.
C  e) if there are no gains for a particular type or day type write
C     out an until: 24.00 placeholder
      WRITE(ioout,'(a)',iostat=ios,err=7) ' '
      INPIC=NCOMP
      do 13 mz=1,inpic
        IUO=IFIL+1
        call FINDFIL(LPROJ(mz),XST)
        IF(XST)THEN

C << might not need to call eroper when ip3ver(mz) is working >>

          CALL ERPFREE(IUO,ISTAT)
          CALL EROPER(0,iuout,IUO,mz,IER)
          if(ip3ver(mz).lt.1)then
            WRITE(ioout,'(3a)',iostat=ios,err=7) 
     &      '!- ============= zone ',zname(mz)(1:lnzname(mz)),
     &      ' has old schedules (periods need to be sorted) ======= '
            goto 13
          endif
        ELSE

C No schedules for this zone, write out place-holders and continue.
          WRITE(ioout,'(3a)',iostat=ios,err=7) 
     &      '!- ============= zone ',zname(mz)(1:lnzname(mz)),
     &      ' has no schedules (place holder NOTHING used) ======= '
          WRITE(ioout,'(a)',iostat=ios,err=7) ' '
          WRITE(ioout,'(2a)',iostat=ios,err=7) 'LIGHTS',com

          if(iver.gt.12)then

C The "Name" field was added after the 1.23 version of EnergyPlus
C For previous versions comment this field
            WRITE(ioout,'(5a)',iostat=ios,err=7) '  ',
     &        zname(mz)(1:lnzname(mz)),'Lights',com,'   !- Name'
          endif  
          WRITE(ioout,'(4a)',iostat=ios,err=5) '  ',
     &      zname(mz)(1:lnzname(mz)),com,'   !- Zone Name'
          WRITE(ioout,'(6a)',iostat=ios,err=5) '  NOTHING',com,
     &      '   !- SCHEDULE Name'
          totcgnsl=0.0
          WRITE(ioout,'(a,F12.4,2a)',iostat=ios,err=7) '  ',
     &      totcgnsl,com,'   !- Design level (W)'
          WRITE(ioout,'(4a)',iostat=ios,err=5) '  ',
     &      '0.0',com,'   !- Return Air Fraction'
          radsplit=0.5
          WRITE(ioout,'(a,F6.4,2a)',iostat=ios,err=5) '  ',
     &      radsplit,com,'   !- Fraction Radiant'
          WRITE(ioout,'(4a)',iostat=ios,err=5) '  ',
     &      '0.0',com,'   !- Fraction Visible'
          WRITE(ioout,'(4a)',iostat=ios,err=5) '  ',
     &      '0.0',com,'   !- Fraction Replaceable'

C << V2.0 has an end-use subcategory here >>
          WRITE(ioout,'(3a)',iostat=ios,err=5) '  ',
     &      sim,'   !- LightsEndUseKey'

C Write out a place-holder small power for the zone.
          WRITE(ioout,'(a)',iostat=ios,err=7) ' '
          WRITE(ioout,'(2a)',iostat=ios,err=7) 'ELECTRIC EQUIPMENT',com
          WRITE(ioout,'(4a)',iostat=ios,err=5) '  ',
     &      zname(mz)(1:lnzname(mz)),com,'   !- Zone Name'
          WRITE(ioout,'(6a)',iostat=ios,err=5) '  NOTHING',com,
     &      '   !- Schedule Name'
          totcgnsl=0.0
          WRITE(ioout,'(a,F12.4,a)',iostat=ios,err=7) '  ',
     &      totcgnsl,com
          fraclatent = 0.0
          WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=5) '  ',
     &      fraclatent,com,'   !- Fraction latent'
          radsplit=0.5
          WRITE(ioout,'(a,F6.4,2a)',iostat=ios,err=5) '  ',
     &      radsplit,com,'   !- Radiant fraction'
          WRITE(ioout,'(4a)',iostat=ios,err=5) '  ',
     &      '0.0',com,'   !- Fraction lost'

C << V2.0 includes General as the End-Use Subcategory
          WRITE(ioout,'(3a)',iostat=ios,err=5) '  ',
     &      sim,'   !- EndUse Subcategory'
          goto 13
        ENDIF

C Start with a comment about zone schedule documentation.
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        WRITE(ioout,'(3a)',iostat=ios,err=7) 
     &    '!- ============= zone ',zname(mz)(1:lnzname(mz)),
     &    ' schedules follow ======='
        WRITE(ioout,'(2a)',iostat=ios,err=7) 
     &    '! ',oprdesc(mz)(1:lnblnk(oprdesc(mz)))

C Scan each days gains and calculate maximum gain.
C First clear loadcasp, looping for each gain type.
        do 9 I=1,7
          loadcasp(mz,i,1)=0.0
          loadcasp(mz,i,2)=0.0
          do 8 JD=1,7
            loadonday(mz,jd,i)=.false.
  8       continue
  9     continue

C Looping through weekdays. 
        if(ncas1.eq.0)then
          CGNS=0.
          CGNL=0.
        else
          do 87 J=1,NCAS1
            CGNS=0.
            CGNL=0.
            ity=iabs(ICGT1(J))
            if (ICGT1(J).eq.-1) then

C For case of m2/person CMGL1()J remains zero so use cmgs1(J) instead.
              if(CMGS1(J).lt.0.001)then
                continue   ! to avoid division by zero.
              else
                CGNS=real(int((ZBASEA(mz)/CMGS1(J))))*95.
                CGNL=real(int((ZBASEA(mz)/CMGS1(J))))*45.
              endif
            elseif (ICGT1(J).lt.-1) then
              CGNS=CMGS1(J)*ZBASEA(mz)
              CGNL=CMGL1(J)*ZBASEA(mz)
            else
              CGNS=CMGS1(J)
              CGNL=CMGL1(J)
            endif

C If the current periods gain is a new max for the type then update loadcasp.
C If nonzero for the day for this type set loadonday() true.
            if(CGNS.gt.loadcasp(mz,ity,1)) loadcasp(mz,ity,1)=CGNS
            if(CGNL.gt.loadcasp(mz,ity,2)) loadcasp(mz,ity,2)=CGNL
            if(CGNS.gt.0.0.or.CGNL.gt.0.0)then
              loadonday(mz,1,ity)=.true.
            endif
 87       continue
        endif
        if(ncas2.eq.0)then
          CGNS=0.
          CGNL=0.
        else
          do 86 J=1,NCAS2
            CGNS=0.
            CGNL=0.
            ity=iabs(ICGT2(J))
            if (ICGT2(J).eq.-1) then

C For case of m2/person CMGL2()J remains zero so use cmgs2(J) instead.
              if(CMGS2(J).lt.0.001)then
                continue   ! to avoid division by zero.
              else
                CGNS=real(int((ZBASEA(mz)/CMGS2(J))))*95.
                CGNL=real(int((ZBASEA(mz)/CMGS2(J))))*45.
              endif
            elseif (ICGT2(J).lt.-1) then
              CGNS=CMGS2(J)*ZBASEA(mz)
              CGNL=CMGL2(J)*ZBASEA(mz)
            else
              CGNS=CMGS2(J)
              CGNL=CMGL2(J)
            endif

C If the current periods gain is a new max for the type then update loadcasp.
            if(CGNS.gt.loadcasp(mz,ity,1)) loadcasp(mz,ity,1)=CGNS
            if(CGNL.gt.loadcasp(mz,ity,2)) loadcasp(mz,ity,2)=CGNL
            if(CGNS.gt.0.0.or.CGNL.gt.0.0)then
              loadonday(mz,2,ity)=.true.
            endif
 86       continue
        endif
        if(ncas3.eq.0)then
          CGNS=0.
          CGNL=0.
        else
          do 85 J=1,NCAS3
            CGNS=0.
            CGNL=0.
            ity=iabs(ICGT3(J))
            if (ICGT3(J).eq.-1) then
              if(CMGS3(J).lt.0.001)then
                continue   ! to avoid division by zero.
              else
                CGNS=real(int((ZBASEA(mz)/CMGS3(J))))*95.
                CGNL=real(int((ZBASEA(mz)/CMGS3(J))))*45.
              endif
            elseif (ICGT3(J).lt.-1) then
              CGNS=CMGS3(J)*ZBASEA(mz)
              CGNL=CMGL3(J)*ZBASEA(mz)
            else
              CGNS=CMGS3(J)
              CGNL=CMGL3(J)
            endif

C If the current periods gain is a new max for the type then update loadcasp.
            if(CGNS.gt.loadcasp(mz,ity,1)) loadcasp(mz,ity,1)=CGNS
            if(CGNL.gt.loadcasp(mz,ity,2)) loadcasp(mz,ity,2)=CGNL
            if(CGNS.gt.0.0.or.CGNL.gt.0.0)then
              loadonday(mz,3,ity)=.true.
            endif
 85       continue
        endif

C Debug.
C        write(6,*)'loadcasp sens ',loadcasp(mz,1,1),loadcasp(mz,2,1),
C     &    loadcasp(mz,3,1)
C        write(6,*)'loadcasp latn ',loadcasp(mz,1,2),loadcasp(mz,2,2),
C     &    loadcasp(mz,3,2)
C        write(6,*)'loadonday wss ocup',loadonday(mz,1,1),
C     &    loadonday(mz,2,1),loadonday(mz,3,1)
C        write(6,*)'loadonday wss lts',loadonday(mz,1,2),
C     &    loadonday(mz,2,2),loadonday(mz,3,2)
C        write(6,*)'loadonday wss eqp',loadonday(mz,1,3),
C     &    loadonday(mz,2,3),loadonday(mz,3,3)

C Write out the initial portion of lighting compact schedule.
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        WRITE(ioout,'(2a)',iostat=ios,err=7) 'SCHEDULE:COMPACT',com
        WRITE(ioout,'(6a)',iostat=ios,err=5) '  ',
     &    zname(mz)(1:lnzname(mz)),' ',lodlabel(mz,2),com,
     &    '   !- Name'
        WRITE(ioout,'(3a)',iostat=ios,err=7) '  Fraction',com,
     &    '   !- ScheduleType' 
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  Through: 12/31',com
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: Weekdays',com

C Go through loops for weekdays, saturdays and sundays for lighting gains.
C If there are no weekday periods or no lights write standard placeholder.
C Also do placeholder of there are no gains of this type on this day.
        totcgnsl=loadcasp(mz,2,1)+loadcasp(mz,2,2)
        call eclose(totcgnsl,0.0,0.001,close)
        if(ncas1.eq.0.or.close.or.(.NOT.loadonday(mz,1,2)))then
          WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &        '24:00',com,'0.0',com
        else
          do 77 J=1,NCAS1
            CGNS=0.
            CGNL=0.
            ity=iabs(ICGT1(J))
            totcgnsl=loadcasp(mz,ity,1)+loadcasp(mz,ity,2)
            if(ity.eq.2)then
              if (ICGT1(J).lt.-1) then
                CGNS=CMGS1(J)*ZBASEA(mz)
                CGNL=CMGL1(J)*ZBASEA(mz)
              else
                CGNS=CMGS1(J)
                CGNL=CMGL1(J)
              endif
              call eclose(totcgnsl,0.0,0.001,close)
              if(close)then
                totfrac=0.0
              else
                totfrac=(CGNS+CGNL)/totcgnsl
              endif
C Debug
C              write(6,*) 'Lights wk CGNS CGNL totcgnsl totfrac',
C     &          CGNS,CGNL,totcgnsl,totfrac

              WRITE(ioout,'(a,i2.2,2a,F7.4,a)',iostat=ios,err=7) 
     &          '  Until: ',ICGF1(J),':00',com,totfrac,com
            endif
 77       continue
        endif

C If there are no saturday periods or no lights write standard placeholder.
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: Saturday',com
        totcgnsl=loadcasp(mz,2,1)+loadcasp(mz,2,2)
        call eclose(totcgnsl,0.0,0.001,close)
        if(ncas2.eq.0.or.close.or.(.NOT.loadonday(mz,2,2)))then
          WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &        '24:00',com,'0.0',com
        else
          do 76 J=1,NCAS2
            CGNS=0.
            CGNL=0.
            ity=iabs(ICGT2(J))
            totcgnsl=loadcasp(mz,ity,1)+loadcasp(mz,ity,2)
            if(ity.eq.2)then
              if (ICGT2(J).lt.-1) then
                CGNS=CMGS2(J)*ZBASEA(mz)
                CGNL=CMGL2(J)*ZBASEA(mz)
              else
                CGNS=CMGS2(J)
                CGNL=CMGL2(J)
              endif
              call eclose(totcgnsl,0.0,0.001,close)
              if(close)then
                totfrac=0.0
              else
                totfrac=(CGNS+CGNL)/totcgnsl
              endif

C Debug.
C              write(6,*) 'Lights sat CGNS CGNL totcgnsl totfrac',
C     &          CGNS,CGNL,totcgnsl,totfrac

              WRITE(ioout,'(a,i2.2,2a,F7.4,a)',iostat=ios,err=7) 
     &          '  Until: ',ICGF2(J),':00',com,totfrac,com
            endif
 76       continue
        endif

C Again for sunday.
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: Sunday',com
        totcgnsl=loadcasp(mz,2,1)+loadcasp(mz,2,2)
        call eclose(totcgnsl,0.0,0.001,close)
        if(ncas3.eq.0.or.close.or.(.NOT.loadonday(mz,3,2)))then
          WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &        '24:00',com,'0.0',com
          WRITE(ioout,'(2a)',iostat=ios,err=7)'  For: AllOtherDays',com
          WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &        '24:00',com,'0.0',sim
        else
          do 75 J=1,NCAS3
            CGNS=0.
            CGNL=0.
            ity=iabs(ICGT3(J))
            totcgnsl=loadcasp(mz,ity,1)+loadcasp(mz,ity,2)
            if(ity.eq.2)then
              if (ICGT3(J).lt.-1) then
                CGNS=CMGS3(J)*ZBASEA(mz)
                CGNL=CMGL3(J)*ZBASEA(mz)
              else
                CGNS=CMGS3(J)
                CGNL=CMGL3(J)
              endif
              call eclose(totcgnsl,0.0,0.001,close)
              if(close)then
                totfrac=0.0
              else
                totfrac=(CGNS+CGNL)/totcgnsl
              endif

C Debug.
C              write(6,*) 'Lights sun CGNS CGNL totcgnsl totfrac',
C     &          CGNS,CGNL,totcgnsl,totfrac

              if(j.lt.NCAS3)then

C If the next period is of the same casual gain type then write
C just this period. If the next period is a different gain type
C then write the AllOtherDays line as well.
                ityn=iabs(ICGT3(J+1))
                if(ity.eq.ityn)then
                  WRITE(ioout,'(a,i2.2,2a,F7.4,a)',iostat=ios,err=7) 
     &              '  Until: ',ICGF3(J),':00',com,totfrac,com
                else
                  WRITE(ioout,'(a,i2.2,2a,F7.4,a)',iostat=ios,err=7) 
     &              '  Until: ',ICGF3(J),':00',com,totfrac,com
                  WRITE(ioout,'(2a)',iostat=ios,err=7)
     &              '  For: AllOtherDays',com
                  WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &              '24:00',com,'0.0',sim
                 endif
              else
                WRITE(ioout,'(a,i2.2,2a,F7.4,a)',iostat=ios,err=7) 
     &            '  Until: ',ICGF3(J),':00',com,totfrac,com
                WRITE(ioout,'(2a)',iostat=ios,err=7)
     &            '  For: AllOtherDays',com
                WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &            '24:00',com,'0.0',sim
              endif
            endif
 75       continue
        endif

C Write the light objects for this zone.
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        WRITE(ioout,'(2a)',iostat=ios,err=7) 'LIGHTS',com

        if(iver.gt.12)then

C The "Name" field was added after the 1.23 version of EnergyPlus
C For previous versions comment this field
          WRITE(ioout,'(5a)',iostat=ios,err=7) '  ',
     &      zname(mz)(1:lnzname(mz)),'Lights',com,'   !- Name'
        endif  
        WRITE(ioout,'(4a)',iostat=ios,err=5) '  ',
     &    zname(mz)(1:lnzname(mz)),com,'   !- Zone Name'
        WRITE(ioout,'(6a)',iostat=ios,err=5) '  ',
     &    zname(mz)(1:lnzname(mz)),' ',lodlabel(mz,2),com,
     &    '   !- Schedule Name'
        totcgnsl=loadcasp(mz,2,1)+loadcasp(mz,2,2)
        WRITE(ioout,'(a,F12.4,2a)',iostat=ios,err=7) '  ',
     &    totcgnsl,com,'   !- Design level (W)'
        WRITE(ioout,'(4a)',iostat=ios,err=5) '  ',
     &    '0.0',com,'   !- Return air fraction'

C Further logic required if there is more than one radiant/convective split.
        radsplit=amax1(RADC1(1),RADC1(2),RADC1(3),RADC1(4),RADC1(5))
        WRITE(ioout,'(a,F6.4,2a)',iostat=ios,err=5) '  ',
     &    radsplit,com,'   !- Radiant fraction'
        WRITE(ioout,'(4a)',iostat=ios,err=5) '  ',
     &    '0.0',com,'   !- Fraction visible'
        WRITE(ioout,'(4a)',iostat=ios,err=5) '  ',
     &    '0.0',com,'   !- Fraction replacable'
        WRITE(ioout,'(3a)',iostat=ios,err=5) '  ',
     &    sim,'   !- LightsEndUseKey'


C Write out the initial portion of small power compact schedule.
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        WRITE(ioout,'(2a)',iostat=ios,err=7) 'SCHEDULE:COMPACT',com
        WRITE(ioout,'(6a)',iostat=ios,err=5) '  ',
     &    zname(mz)(1:lnzname(mz)),' ',lodlabel(mz,3),com,'   !- Name'
        WRITE(ioout,'(3a)',iostat=ios,err=7) '  Fraction',com,
     &    '   !- ScheduleType' 
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  Through: 12/31',com

C Go through loops for weekdays, saturdays and sundays for small power gains.
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: Weekdays',com
        totcgnsl=loadcasp(mz,3,1)+loadcasp(mz,3,2)
        call eclose(totcgnsl,0.0,0.001,close)
        if(ncas1.eq.0.or.close.or.(.NOT.loadonday(mz,1,3)))then
          WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &        '24:00',com,'0.0',com
        else
          do 67 J=1,NCAS1
            CGNS=0.
            CGNL=0.
            ity=iabs(ICGT1(J))
            totcgnsl=loadcasp(mz,ity,1)+loadcasp(mz,ity,2)
            if(ity.eq.3)then
              if (ICGT1(J).lt.-1) then
                CGNS=CMGS1(J)*ZBASEA(mz)
                CGNL=CMGL1(J)*ZBASEA(mz)
              else
                CGNS=CMGS1(J)
                CGNL=CMGL1(J)
              endif
              call eclose(totcgnsl,0.0,0.001,close)
              if(close)then
                totfrac=0.0
              else
                totfrac=(CGNS+CGNL)/totcgnsl
              endif

C Debug.
C              write(6,*) 'SmP wkd CGNS CGNL totcgnsl totfrac',
C     &          CGNS,CGNL,totcgnsl,totfrac

              WRITE(ioout,'(a,i2.2,2a,F7.4,a)',iostat=ios,err=7) 
     &          '  Until: ',ICGF1(J),':00',com,totfrac,com
            endif
 67       continue
        endif

        WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: Saturday',com
        totcgnsl=loadcasp(mz,3,1)+loadcasp(mz,3,2)
        call eclose(totcgnsl,0.0,0.001,close)
        if(ncas2.eq.0.or.close.or.(.NOT.loadonday(mz,2,3)))then
          WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &        '24:00',com,'0.0',com
        else
          do 66 J=1,NCAS2
            CGNS=0.
            CGNL=0.
            ity=iabs(ICGT2(J))
            totcgnsl=loadcasp(mz,ity,1)+loadcasp(mz,ity,2)
            if(ity.eq.3)then
              if (ICGT2(J).lt.-1) then
                CGNS=CMGS2(J)*ZBASEA(mz)
                CGNL=CMGL2(J)*ZBASEA(mz)
              else
                CGNS=CMGS2(J)
                CGNL=CMGL2(J)
              endif
              call eclose(totcgnsl,0.0,0.001,close)
              if(close)then
                totfrac=0.0
              else
                totfrac=(CGNS+CGNL)/totcgnsl
              endif

C Debug.
C              write(6,*) 'SmP sat CGNS CGNL totcgnsl totfrac',
C     &          CGNS,CGNL,totcgnsl,totfrac

              WRITE(ioout,'(a,i2.2,2a,F7.4,a)',iostat=ios,err=7) 
     &          '  Until: ',ICGF2(J),':00',com,totfrac,com
            endif
 66       continue
        endif

C And small power for Sunday.
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: Sunday',com
        totcgnsl=loadcasp(mz,3,1)+loadcasp(mz,3,2)
        call eclose(totcgnsl,0.0,0.001,close)
        if(ncas3.eq.0.or.close.or.(.NOT.loadonday(mz,3,3)))then
          WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &        '24:00',com,'0.0',com
          WRITE(ioout,'(2a)',iostat=ios,err=7)'  For: AllOtherDays',com
          WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &        '24:00',com,'0.0',sim
        else
          do 65 J=1,NCAS3
            CGNS=0.
            CGNL=0.
            ity=iabs(ICGT3(J))
            totcgnsl=loadcasp(mz,ity,1)+loadcasp(mz,ity,2)
            if(ity.eq.3)then
              if (ICGT3(J).lt.-1) then
                CGNS=CMGS3(J)*ZBASEA(mz)
                CGNL=CMGL3(J)*ZBASEA(mz)
              else
                CGNS=CMGS3(J)
                CGNL=CMGL3(J)
              endif
              call eclose(totcgnsl,0.0,0.001,close)
              if(close)then
                totfrac=0.0
              else
                totfrac=(CGNS+CGNL)/totcgnsl
              endif

C Debug
C              write(6,*) 'SmP sun CGNS CGNL totcgnsl totfrac',
C     &          CGNS,CGNL,totcgnsl,totfrac

              if(j.lt.NCAS3)then
                WRITE(ioout,'(a,i2.2,2a,F7.4,a)',iostat=ios,err=7) 
     &            '  Until: ',ICGF3(J),':00',com,totfrac,com
              else
                WRITE(ioout,'(a,i2.2,2a,F7.4,a)',iostat=ios,err=7) 
     &            '  Until: ',ICGF3(J),':00',com,totfrac,com
                WRITE(ioout,'(2a)',iostat=ios,err=7)
     &            '  For: AllOtherDays',com
                WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &            '24:00',com,'0.0',sim
              endif
            endif
 65       continue
        endif

C Write the electrical equipment objects for this zone, if none
C the only write a brief note and jump to people.
        totcgnsl=loadcasp(mz,3,1)+loadcasp(mz,3,2)
        if(totcgnsl.lt.1.0)then
          WRITE(ioout,'(a)',iostat=ios,err=7) 
     &     '!- no small power in zone - skipping ELECTRIC EQUIPMENT'
          goto 12
        endif
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        WRITE(ioout,'(2a)',iostat=ios,err=7) 'ELECTRIC EQUIPMENT',com

        if(iver.gt.12)then

C The "Name" field was added after the 1.23 version of EnergyPlus
C For previous versions comment this field
          WRITE(ioout,'(5a)',iostat=ios,err=7) '  ',
     &      zname(mz)(1:lnzname(mz)),'EQUIPMENT',com,'   !- Name'
        endif
        WRITE(ioout,'(4a)',iostat=ios,err=5) '  ',
     &    zname(mz)(1:lnzname(mz)),com,'   !- Zone Name'
        WRITE(ioout,'(6a)',iostat=ios,err=5) '  ',
     &    zname(mz)(1:lnzname(mz)),' ',lodlabel(mz,3),com,
     &    '   !- Schedule Name'
        totcgnsl=loadcasp(mz,3,1)+loadcasp(mz,3,2)
        WRITE(ioout,'(a,F12.4,a)',iostat=ios,err=7) '  ',
     &    totcgnsl,com
        call eclose(loadcasp(mz,3,2),0.0,0.001,close)
        if(close)then
          fraclatent = 0.0
        else

C << check this out, it might be inverted >>
          fraclatent = (totcgnsl/loadcasp(mz,3,2))*0.01
        endif
        WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=5) '  ',
     &    fraclatent,com,'   !- Fraction latent'

C Further logic required if there is more than one radiant/convective split.
        radsplit=amax1(RADC1(1),RADC1(2),RADC1(3),RADC1(4),RADC1(5))
        WRITE(ioout,'(a,F6.4,2a)',iostat=ios,err=5) '  ',
     &    radsplit,com,'   !- Radiant fraction'
        WRITE(ioout,'(4a)',iostat=ios,err=5) '  ',
     &    '0.0',com,'   !- Fraction lost'
        WRITE(ioout,'(3a)',iostat=ios,err=5) '  ',
     &    sim,'   !- EndUse Category'

C --------------------------------------
C Write out the initial portion of people compact schedule.
  12    continue
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        WRITE(ioout,'(a)',iostat=ios,err=7) 
     &    '!- occupants schedule/activity level/PEOPLE definition... '
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        WRITE(ioout,'(2a)',iostat=ios,err=7) 'SCHEDULE:COMPACT',com
        WRITE(ioout,'(6a)',iostat=ios,err=5) '  ',
     &    zname(mz)(1:lnzname(mz)),' ',lodlabel(mz,1),com,'   !- Name'
        WRITE(ioout,'(3a)',iostat=ios,err=7) '  Fraction',com,
     &    '   !- ScheduleType' 
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  Through: 12/31',com

C Go through loops for weekdays, saturdays and sundays for occupants.
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: Weekdays',com
        totcgnsl=loadcasp(mz,1,1)+loadcasp(mz,1,2)
        call eclose(totcgnsl,0.0,0.001,close)
        if(ncas1.eq.0.or.close.or.(.NOT.loadonday(mz,1,1)))then
          WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &        '24:00',com,'0.0',com
        else
          do 57 J=1,NCAS1
            CGNS=0.
            CGNL=0.
            ity=iabs(ICGT1(J))
            totcgnsl=loadcasp(mz,ity,1)+loadcasp(mz,ity,2)
            if(ity.eq.1)then
              if (ICGT1(J).lt.-1) then
                CGNS=CMGS1(J)*ZBASEA(mz)
                CGNL=CMGL1(J)*ZBASEA(mz)
              else
                CGNS=CMGS1(J)
                CGNL=CMGL1(J)
              endif
              call eclose(totcgnsl,0.0,0.001,close)
              if(close)then
                totfrac=0.0
              else
                totfrac=(CGNS+CGNL)/totcgnsl
              endif

C Debug.
C              write(6,*) 'People wkd CGNS CGNL totcgnsl totfrac',
C     &          CGNS,CGNL,totcgnsl,totfrac

              WRITE(ioout,'(a,i2.2,2a,F7.4,a)',iostat=ios,err=7) 
     &          '  Until: ',ICGF1(J),':00',com,totfrac,com
            endif
 57       continue
        endif

        WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: Saturday',com
        totcgnsl=loadcasp(mz,1,1)+loadcasp(mz,1,2)
        call eclose(totcgnsl,0.0,0.001,close)
        if(ncas2.eq.0.or.close.or.(.NOT.loadonday(mz,2,1)))then
          WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &        '24:00',com,'0.0',com
        else
          do 56 J=1,NCAS2
            CGNS=0.
            CGNL=0.
            ity=iabs(ICGT2(J))
            totcgnsl=loadcasp(mz,ity,1)+loadcasp(mz,ity,2)
            if(ity.eq.1)then
              if (ICGT2(J).lt.-1) then
                CGNS=CMGS2(J)*ZBASEA(mz)
                CGNL=CMGL2(J)*ZBASEA(mz)
              else
                CGNS=CMGS2(J)
                CGNL=CMGL2(J)
              endif
              call eclose(totcgnsl,0.0,0.001,close)
              if(close)then
                totfrac=0.0
              else
                totfrac=(CGNS+CGNL)/totcgnsl
              endif

C Debug
C              write(6,*) 'People sat CGNS CGNL totcgnsl totfrac',
C     &          CGNS,CGNL,totcgnsl,totfrac

              WRITE(ioout,'(a,i2.2,2a,F7.4,a)',iostat=ios,err=7) 
     &          '  Until: ',ICGF2(J),':00',com,totfrac,com
            endif
 56       continue
        endif

C And for Sunday.
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: Sunday',com
        totcgnsl=loadcasp(mz,1,1)+loadcasp(mz,1,2)
        call eclose(totcgnsl,0.0,0.001,close)
        if(ncas3.eq.0.or.close.or.(.NOT.loadonday(mz,3,1)))then
          WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &        '24:00',com,'0.0',com
          WRITE(ioout,'(2a)',iostat=ios,err=7)'  For: AllOtherDays',com
          WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &        '24:00',com,'0.0',sim
        else
          do 55 J=1,NCAS3
            CGNS=0.
            CGNL=0.
            ity=iabs(ICGT3(J))
            totcgnsl=loadcasp(mz,ity,1)+loadcasp(mz,ity,2)
            if(ity.eq.1)then
              if (ICGT3(J).lt.-1) then
                CGNS=CMGS3(J)*ZBASEA(mz)
                CGNL=CMGL3(J)*ZBASEA(mz)
              else
                CGNS=CMGS3(J)
                CGNL=CMGL3(J)
              endif
              call eclose(totcgnsl,0.0,0.001,close)
              if(close)then
                totfrac=0.0
              else
                totfrac=(CGNS+CGNL)/totcgnsl
              endif

C Debug.
C              write(6,*) 'People sun CGNS CGNL totcgnsl totfrac',
C     &          CGNS,CGNL,totcgnsl,totfrac

              if(j.lt.NCAS3)then

C If the next period is of the same casual gain type then write
C just this period. If the next period is a different gain type
C then write the AllOtherDays line as well.
                ityn=iabs(ICGT3(J+1))
                if(ity.eq.ityn)then
                  WRITE(ioout,'(a,i2.2,2a,F7.4,a)',iostat=ios,err=7) 
     &              '  Until: ',ICGF3(J),':00',com,totfrac,com
                else
                  WRITE(ioout,'(a,i2.2,2a,F7.4,a)',iostat=ios,err=7) 
     &              '  Until: ',ICGF3(J),':00',com,totfrac,com
                  WRITE(ioout,'(2a)',iostat=ios,err=7)
     &              '  For: AllOtherDays',com
                  WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &              '24:00',com,'0.0',sim
                 endif
              else
                WRITE(ioout,'(a,i2.2,2a,F7.4,a)',iostat=ios,err=7) 
     &            '  Until: ',ICGF3(J),':00',com,totfrac,com
                WRITE(ioout,'(2a)',iostat=ios,err=7)
     &            '  For: AllOtherDays',com
                WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &            '24:00',com,'0.0',sim
              endif
            endif
 55       continue
        endif

C Write out matching activity level schedule for people, but first
C ask the user how many people are equivaent to the peak. If there
C are no occupants then don't bother.
        H(1)='EnergyPlus needs to know how many people are represented'
        H(2)='by the peak occupant gain in this room. The numbers '
        H(3)='presented are the sensible and latent gains and the '
        H(4)='tototal of the sensible and latent. '
        totcgnsl=loadcasp(mz,1,1)+loadcasp(mz,1,2)

        if(totcgnsl.lt.1.0)then
          WRITE(ioout,'(a)',iostat=ios,err=7) 
     &     '!- no occupants in zone - skipping Activity level & PEOPLE'
          goto 13
        endif

C There are occupant casual gains.
        write(outs,'(3a,f7.1,a,f7.1,a)') 'Zone ',
     &    zname(mz)(1:lnzname(mz)),
     &    ' has a peak sensible occupant load of ',loadcasp(mz,1,1),
     &    ' W and peak latent ',loadcasp(mz,1,2),'W.'
        call edisp(iuout,outs)

C Initial guess of 140W sensible+latent per person. Remember the gains
C for one person as oneperson.
        vpeople=totcgnsl/140.0
        CALL EASKR(vpeople,' Number of people in room (see report): ',
     &    ' ',0.0,'F',99.999,'W',1.0,'people in room',IER,4)
        oneperson = totcgnsl/vpeople
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        WRITE(ioout,'(2a)',iostat=ios,err=7) 'SCHEDULE:COMPACT',com
        WRITE(ioout,'(5a)',iostat=ios,err=5) '  ',
     &    'Activity level ',zname(mz)(1:lnzname(mz)),com,
     &    '   !- Name'
        WRITE(ioout,'(3a)',iostat=ios,err=7) '  Any Number',com,
     &    '   !- ScheduleType' 
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  Through: 12/31',com
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: Weekdays',com
        WRITE(ioout,'(3a,F7.3,a)',iostat=ios,err=7) '  Until: ',
     &    '24:00',com,oneperson,com
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: Saturday',com
        WRITE(ioout,'(3a,F7.3,a)',iostat=ios,err=7) '  Until: ',
     &    '24:00',com,oneperson,com
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: Sunday',com
        WRITE(ioout,'(3a,F7.3,a)',iostat=ios,err=7) '  Until: ',
     &    '24:00',com,oneperson,com
        WRITE(ioout,'(2a)',iostat=ios,err=7)'  For: AllOtherDays',com
        WRITE(ioout,'(3a,F7.3,a)',iostat=ios,err=7) '  Until: ',
     &    '24:00',com,oneperson,sim

C Now write out the people.
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        WRITE(ioout,'(2a)',iostat=ios,err=7) 'PEOPLE',com

        if(iver.gt.12)then

C The "Name" field was added after the 1.23 version of EnergyPlus
C For previous versions comment this field
          WRITE(ioout,'(5a)',iostat=ios,err=7) '  ',
     &      zname(mz)(1:lnzname(mz)),'People',com,'   !- Name'
        endif
        WRITE(ioout,'(4a)',iostat=ios,err=5) '  ',
     &    zname(mz)(1:lnzname(mz)),com,'   !- Zone Name'
        WRITE(ioout,'(a,F7.3,2a)',iostat=ios,err=7) '  ',vpeople,com,
     &    '   !- Number of people'
        WRITE(ioout,'(6a)',iostat=ios,err=5) '  ',
     &    zname(mz)(1:lnzname(mz)),' ',lodlabel(mz,1),com,
     &    '   !- Number of people schedule'

C Further logic required if there is more than one radiant/convective split.
        radsplit=amax1(RADC1(1),RADC1(2),RADC1(3),RADC1(4),RADC1(5))
        WRITE(ioout,'(a,F6.4,2a)',iostat=ios,err=5) '  ',
     &    radsplit,com,'   !- Radiant fraction'
        WRITE(ioout,'(5a)',iostat=ios,err=5) '  ',
     &    'Activity level ',zname(mz)(1:lnzname(mz)),com,
     &    '   !- Activity level schedule name (W/person)'
          
        if(iver.le.12)then

C For old EPlus versions: include line for the Group name
C (also no name at the start of this category is needed)
          WRITE(ioout,'(3a)',iostat=ios,err=5) '  ',com,
     &      '  !- Group name'
        endif
        if(iver.lt.20)then
          WRITE(ioout,'(3a)',iostat=ios,err=5) '  ',com,
     &      '  !- MRT Calculation Type'
        else
          WRITE(ioout,'(3a)',iostat=ios,err=5) '  ZoneAveraged',com,
     &      '  !- MRT Calculation Type'
        endif
        WRITE(ioout,'(3a)',iostat=ios,err=5) '  ',com,
     &    '  !- Surface Name/Angle Factor List Name'
        WRITE(ioout,'(3a)',iostat=ios,err=5) '  ',com,
     &    '  !- Work Efficiency SCHEDULE Name (0.0-1.0,real)'
        WRITE(ioout,'(3a)',iostat=ios,err=5) '  ',com,
     &    '  !- Clothing Insulation SCHEDULE Name (real)'
        WRITE(ioout,'(3a)',iostat=ios,err=5) '  ',com,
     &    '  !- Air Velocity SCHEDULE Name (units m/s, real)'
        WRITE(ioout,'(3a)',iostat=ios,err=5) '  ',com,
     &    '  !- Thermal Comfort Report Type R1'
        WRITE(ioout,'(3a)',iostat=ios,err=5) '  ',com,
     &    '  !- Thermal Comfort Report Type R2'
        WRITE(ioout,'(3a)',iostat=ios,err=5) '  ',com,
     &    '  !- Thermal Comfort Report Type R3'

C Include the sensible fraction of the total.
        totcgnsl=loadcasp(mz,1,1)+loadcasp(mz,1,2)
        sensfrac=1.0
        if(totcgnsl.gt.0.1)sensfrac=loadcasp(mz,1,1)/totcgnsl
        WRITE(ioout,'(a,F6.4,2a)',iostat=ios,err=5) '  ',
     &    sensfrac,sim,'   !- user specified sensible fraction'

 13   continue

C << More schedules here >>

C Now for each zone, write out the surfaces.
C Setup standard assumptions.
      WRITE(ioout,'(a)',iostat=ios,err=6) ' '
      WRITE(ioout,'(a)',iostat=ios,err=6) '!- ============='
      WRITE(ioout,'(8a)',iostat=ios,err=6) 'SurfaceGeometry',com,
     &  'LowerLeftCorner',com,
     &  'CounterClockWise',com,'WorldCoordinateSystem',sim
      WRITE(ioout,'(a)',iostat=ios,err=6) '!- ============='
      IFIL=11

C Assume configuration file is from IFIL+5, any leakage description
C is fom IFIL+6, revised config file on IFIL+3, geometry on
C IFILE+2 and ASCII viewing and geometry file reading on IFILE+1. 
      ITA1 = IFIL+8
      INPIC=NCOMP
      do 14 mz=1,inpic

C Read in the zone geometry.
        WRITE(ioout,'(a)',iostat=ios,err=5) ' '
        WRITE(ioout,'(2a)',iostat=ios,err=5) '! ',
     &    zdesc(mz)(1:lnzdesc(mz))
        WRITE(ioout,'(a)',iostat=ios,err=5) ' '
        WRITE(ioout,'(2a)',iostat=ios,err=5) 'ZONE',com
        WRITE(ioout,'(4a)',iostat=ios,err=5) '  ',
     &    zname(mz)(1:lnzname(mz)),com,'   !- Zone Name'
        WRITE(ioout,'(4a)',iostat=ios,err=5) '  ','0.0',com,
     &    '   !- Relative North (to building) {deg}'
        WRITE(ioout,'(8a)',iostat=ios,err=5) '  ','0.0',com,'0.0',com,
     &    '0.0',com,'   !- Origin {m}'
        WRITE(ioout,'(6a)',iostat=ios,err=5) '  ','1',com,'1',com,
     &    '   !- Type and Muitiplier'
        WRITE(outs,'(a,a)',iostat=ios,err=5)' Scanning : ',LGEOM(mz)
        CALL edisp(iuout,outs)
        call eclose(gversion(mz),1.1,0.01,newgeo)
        if(newgeo)then
          call georead(ITA1,LGEOM(mz),mz,1,IUOUT,IER)
        else
          call egomin(ITA1,LGEOM(mz),mz,1,ITRC,IUOUT,IER)
        endif

C Loop through all vertices and find the top and bottom of the zone.
        ZMX=-1.E+7
        ZMN=1.E+7
        do 40 i = 1,NTV
          ZMN=AMIN1(ZMN,Z(I))
          ZMX=AMAX1(ZMX,Z(I))
  40    continue
        WRITE(ioout,'(a,f6.3,2a)',iostat=ios,err=5) '  ',ZMX-ZMN,com,
     &    '   !- Ceiling Height {m}'
        WRITE(ioout,'(a,f9.3,2a)',iostat=ios,err=5) '  ',VOL(mz),sim,
     &    '   !- Volume {m3}'

C Test edge adjacencies and subsurfaces.
        act = 'c'
        call suredgeadj(0,act,mz,ier)

C << this process failed for a surface with 11 edges, see if there
C << is some logic that will get around this. What we really want
C << to know is if this surface has one or more holes in it.

C For each surface write out data. We know that most
C surfaces with a hole in them will have 10 edges and the first 4
C will describe a quad that can be used with EnergyPlus. We know
C that most surfaces with 8 edges will be a wall with a door in it.
        DO 60 IS=1,NSUR
          icc=izstocn(mz,is)
          if(NVER(IS).gt.4.and.NVER(IS).ne.10)then

C Patch for 10/11/12 edges might represent wall with window or an
C extra vertex or wall with joined windows in one hole.
C          if((NVER(IS).gt.4.and.NVER(IS).lt.10).or.NVER(IS).gt.12)then

C Check how many corners there are in the surface by looping through
C all the edges and checking for angles greater than 5 degrees. Do the
C corner at the 1st vertex first.

C << so why are we trying to reduce the number of edges in a surface? >>
            iedgcnt=0
            iwhich1=JVN(IS,NVER(IS))
            iwhich2=JVN(IS,1)
            iwhich3=JVN(IS,2)
            iedglim=NVER(IS)-1
            call ang3vtx(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),X(IWHICH2),
     &        Y(IWHICH2),Z(IWHICH2),X(IWHICH3),Y(IWHICH3),Z(IWHICH3),
     &        ang)
            call eclose(ang,0.00,5.00,closea)
            call eclose(ang,180.00,5.00,closeb)
            if(closea.or.closeb)then
              continue
            else
              iedgcnt=iedgcnt+1
              COORD(iedgcnt,1)=X(IWHICH2)
              COORD(iedgcnt,2)=Y(IWHICH2)
              COORD(iedgcnt,3)=Z(IWHICH2)
            endif
            do 61 iedg=1,iedglim
              if(iedg.lt.iedglim)then
                iwhich1=JVN(IS,iedg)
                iwhich2=JVN(IS,iedg+1)
                iwhich3=JVN(IS,iedg+2)
              elseif(iedg.eq.iedglim)then
                iwhich1=JVN(IS,iedg)
                iwhich2=JVN(IS,iedg+1)
                iwhich3=JVN(IS,1)
              endif
              call ang3vtx(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),X(IWHICH2),
     &          Y(IWHICH2),Z(IWHICH2),X(IWHICH3),Y(IWHICH3),Z(IWHICH3),
     &          ang)
              call eclose(ang,0.00,5.00,closea)
              call eclose(ang,180.00,5.00,closeb)
              if(closea.or.closeb)then
                continue
              else
                iedgcnt=iedgcnt+1
                COORD(iedgcnt,1)=X(IWHICH2)
                COORD(iedgcnt,2)=Y(IWHICH2)
                COORD(iedgcnt,3)=Z(IWHICH2)
              endif
  61        continue

            if(iedgcnt.eq.8)then

C << but this fails if there are any extra vertices in the parent
C << surface - one more argument for recording the topology when
C << surface was first added.

C 8 corners is typcial of a wall with a door in it. Double check
C if the angle between the first two edges is ~90 and between the
C 2nd and 3rd (going around the top of the door) is ~90
              iwhich1=JVN(IS,1)
              iwhich2=JVN(IS,2)
              iwhich3=JVN(IS,3)
              call ang3vtx(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),X(IWHICH2),
     &          Y(IWHICH2),Z(IWHICH2),X(IWHICH3),Y(IWHICH3),Z(IWHICH3),
     &          ang1)
              call eclose(ang1,90.00,5.00,closea)
              iwhich1=JVN(IS,2)
              iwhich2=JVN(IS,3)
              iwhich3=JVN(IS,4)
              call ang3vtx(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),X(IWHICH2),
     &          Y(IWHICH2),Z(IWHICH2),X(IWHICH3),Y(IWHICH3),Z(IWHICH3),
     &          ang2)
              call eclose(ang2,90.00,5.00,closeb)

C Debug.
C              write(6,*) '2nd & 3rd angles are ',ang1,ang2

C Because 8 edged surfaces can also exist in other contexts the logic
C for surfaces surrounding doors should probably not be applied to
C floor and ceiling locations.
              closec=.false.
              if(SSVFC(icc)(1:4).eq.'VERT') closec = .true.
              if(SSVFC(icc)(1:4).eq.'UNKN') closec = .true.
              if(closea.and.closeb.and.closec)then

C If first edge and 5th edge are at same height then a door.
                call eclose(Z(JVN(IS,1)),Z(JVN(IS,2)),0.01,closea)
                call eclose(Z(JVN(IS,1)),Z(JVN(IS,5)),0.01,closeb)
                call eclose(Z(JVN(IS,5)),Z(JVN(IS,6)),0.01,closec)
                if(closea.and.closeb.and.closec)then
                  call zsid(mz,is,ZSDES,ZSDESC,ZSDESS)
                  write(outs,'(3a,a)') 'Surface ',
     &              ZSDES(1:lnblnk(ZSDES)),
     &              ' probably wraps around a door'
                  call edisp(iuout,outs)
                  COORD(1,1)=X(JVN(IS,1))
                  COORD(1,2)=Y(JVN(IS,1))
                  COORD(1,3)=Z(JVN(IS,1))
                  COORD(2,1)=X(JVN(IS,6))
                  COORD(2,2)=Y(JVN(IS,6))
                  COORD(2,3)=Z(JVN(IS,6))
                  COORD(3,1)=X(JVN(IS,7))
                  COORD(3,2)=Y(JVN(IS,7))
                  COORD(3,3)=Z(JVN(IS,7))
                  COORD(4,1)=X(JVN(IS,8))
                  COORD(4,2)=Y(JVN(IS,8))
                  COORD(4,3)=Z(JVN(IS,8))
                  iedgcnt=4
                endif
              endif
            endif

C Debug.
C            write(6,*) 'for surface ',is,' corners resolve to...'
C            write(6,*) COORD(1,1),COORD(1,2),COORD(1,3)
C            write(6,*) COORD(2,1),COORD(2,2),COORD(2,3)
C            write(6,*) COORD(3,1),COORD(3,2),COORD(3,3)
C            write(6,*) COORD(4,1),COORD(4,2),COORD(4,3)

          endif

C Have not yet worked out equivalent to a constant boundary condition.
          if(ICT(icc).eq.2)then
            call zsid(mz,is,ZSDES,ZSDESC,ZSDESS)
            write(outs,'(3a,i2,a)') 'Surface ',ZSDES(1:lnblnk(ZSDES)),
     &        ' faces a constant boundary so skipping.'
            call edisp(iuout,outs)
            WRITE(ioout,'(a)') ' '
            WRITE(ioout,'(2a)',iostat=ios,err=6) 
     &          '!- ',outs(1:lnblnk(outs))
            goto 60
          endif

C If a surface is transparent, it must have a non-blank
C parent, if not ask the user for a suggestion.
          if(SSOTF(icc)(1:4).NE.'OPAQ'.and.SSOTF(icc)(1:4).NE.'CFC '
     &      .and.(SSPARENT(icc)(1:2).eq.'  '.or.
     &        SSPARENT(icc)(1:2).eq.'- '))then
            call zsid(mz,is,ZSDES,ZSDESC,ZSDESS)
            write(outs,'(3a,i2,a)') 'Surface ',ZSDES(1:lnblnk(ZSDES)),
     &        ' is transparent without parent.'
            call edisp(iuout,outs)
            dok=.false.
            h(1)='Is its parent surface in the current'
            h(2)='zone. If not say no. '
            izz=ic1(icc)
            write(outs,'(5a)') 'Does ',ssname(icc),
     &        ' have a parent surface in ',zname(izz),'?'
            call askok(outs,
     &        ' (saying no skips this subsurface)',ok,dok,2)
            if(ok)then
              write(outs,'(5a)') 'Specify parent surface for ',
     &          ssname(icc),' in ',zname(izz),'.'
              call easksur(izz,iss,'-',outs,' ',ier)
              ibinval=izstocn(izz,iss)
              if(ibinval.ne.0)then
                SSPARENT(icc)=ssname(ibinval)
              else
                SSPARENT(icc)=' '
                write(outs,'(3a,i2,a)') 'Surface ',
     &            ZSDES(1:lnblnk(ZSDES)),
     &            ' is transparent without parent.'
                WRITE(ioout,'(a)') ' '
                WRITE(ioout,'(2a)',iostat=ios,err=6) 
     &            '!- ',outs(1:lnblnk(outs))
                goto 60
              endif
            else
              SSPARENT(icc)=' '
              write(outs,'(3a,i2,a)') 'Surface ',
     &          ZSDES(1:lnblnk(ZSDES)),
     &          ' is transparent without parent.'
              WRITE(ioout,'(a)') ' '
              WRITE(ioout,'(2a)',iostat=ios,err=6) 
     &          '!- ',outs(1:lnblnk(outs))
              goto 60
            endif
          endif

C The initial tag is dependant on whether the surface is
C opaque or transparent and whether it is adiabetic.
C Treat surfaces to an esp-r similar boundary condition as
C internal mass.
C Generate combined zone & generate surface names as identifiers.
          WRITE(ioout,'(a)') ' '

          if(SSOTF(icc)(1:4).NE.'OPAQ'.AND.
     &      SSOTF(icc)(1:4).NE.'CFC ')then
            WRITE(ioout,'(2a)',iostat=ios,err=6) 
     &        'Surface:HeatTransfer:sub',com
          else
            if(ICT(icc).eq.5.or.ICT(icc).eq.1)then
              WRITE(ioout,'(2a)',iostat=ios,err=6) 
     &          'Surface:HeatTransfer:InternalMass',com
            else
              if(SSPARENT(icc)(1:2).eq.'  '.or.
     &           SSPARENT(icc)(1:2).eq.'- ')then
                WRITE(ioout,'(2a)',iostat=ios,err=6) 
     &            'Surface:HeatTransfer',com
              else
                WRITE(ioout,'(3a)',iostat=ios,err=6) 
     &            'Surface:HeatTransfer:sub',com,'   !- (a door)'
              endif
            endif
          endif
          call zsid(mz,is,ZSDES,ZSDESC,ZSDESS)
          WRITE(ioout,'(4a)',iostat=ios,err=6) '  ',
     &      ZSDES(1:lnblnk(ZSDES)),com,
     &      '   !- User Supplied Surface Name'
          if(SSOTF(icc)(1:4).NE.'OPAQ'.AND.
     &      SSOTF(icc)(1:4).NE.'CFC ')then
            WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','WINDOW',com,
     &        '   !- Surface Type!'
            WRITE(ioout,'(4a)',iostat=ios,err=6) '  ',
     &        SSMLCN(icc)(1:lnblnk(SSMLCN(icc))),com,
     &        '   !- Construction Name of the Surface'
            lsn0=lnblnk(SSPARENT(icc))
            lzn0=lnzname(mz)
            WRITE(ZSDES,'(a,a1,a)',iostat=ios,err=6) 
     &        SSPARENT(icc)(1:lsn0),':',zname(mz)(1:lzn0)
            WRITE(ioout,'(4a)',iostat=ios,err=6) '  ',
     &        ZSDES(1:lnblnk(ZSDES)),com,'   !- Base Surface Name'
            WRITE(ioout,'(3a)',iostat=ios,err=6) '  ',com,
     &        '   !- OutsideFaceEnvironment Object'

C Also include viewfactor to ground value depending on orientation
C and site exposure.
            if(siteexposureindex.ne.8)then
              if(SSVFC(icc)(1:4).eq.'VERT')then
                WRITE(ioout,'(a,f5.3,2a)',iostat=ios,err=6) '  ',
     &            PROPG(siteexposureindex),com,
     &            '   !- View Factor to Ground'
              elseif(SSVFC(icc)(1:4).eq.'CEIL')then
                WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','0.0',
     &            com,'   !- View Factor to Ground'
              elseif(SSVFC(icc)(1:4).eq.'SLOP'.or.
     &               SSVFC(icc)(1:4).eq.'UNKN')then
                WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','0.0',
     &            com,'   !- View Factor to Ground'
              elseif(SSVFC(icc)(1:4).eq.'FLOR')then
                WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','1.0',
     &            com,'   !- View Factor to Ground'
              endif
            else
              if(SSVFC(icc)(1:4).eq.'VERT')then
                WRITE(ioout,'(a,f5.3,2a)',iostat=ios,err=6) '  ',
     &           groundview,com,'   !- View Factor to Ground'
              elseif(SSVFC(icc)(1:4).eq.'CEIL')then
                WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','0.0',
     &            com,'   !- View Factor to Ground'
              elseif(SSVFC(icc)(1:4).eq.'SLOP'.or.
     &               SSVFC(icc)(1:4).eq.'UNKN')then
                WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','0.0',
     &            com,'   !- View Factor to Ground'
              elseif(SSVFC(icc)(1:4).eq.'FLOR')then
                WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','1.0',
     &            com,'   !- View Factor to Ground'
              endif
            endif      
            WRITE(ioout,'(3a)',iostat=ios,err=6) '  ',com,
     &        '   !-   Name of shading control'
            WRITE(ioout,'(3a)',iostat=ios,err=6) '  ',com,
     &        '   !-   WindowFrameAndDivider NameName'
            WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','1.0',com,
     &        '   !-   Multiplier'
          else
            if(ICT(icc).eq.5.or.ICT(icc).eq.1)then
              WRITE(ioout,'(4a)',iostat=ios,err=6) '  ',
     &          SSMLCN(icc)(1:lnblnk(SSMLCN(icc))),com,
     &          '   !- Construction Name of the Surface'
              WRITE(ioout,'(4a)',iostat=ios,err=6) '  ',
     &          zname(mz)(1:lnzname(mz)),
     &          com,'   !- InsideFaceEnvironment'
              WRITE(ioout,'(a,f8.4,2a)',iostat=ios,err=6) '  ',
     &          SSNA(icc),sim,'   !-  Area {m}'
              goto 60
            else

C Check if parent surface name is a blank or just a dash.
              if(SSPARENT(icc)(1:2).eq.'  '.or.
     &           SSPARENT(icc)(1:2).eq.'- ')then
                if(SSVFC(icc)(1:4).eq.'VERT')then
                  WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','WALL',com,
     &              '   !- Surface Type'
                elseif(SSVFC(icc)(1:4).eq.'SLOP'.or.
     &                 SSVFC(icc)(1:4).eq.'UNKN')then
                  if(ICT(icc).eq.0.or.ICT(icc).eq.4)then
                    WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','ROOF',
     &                com,'   !- Surface Type'
                  elseif(ICT(icc).eq.3)then
                    WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','WALL',
     &                com,'   !- Surface Type'
                  endif
                elseif(SSVFC(icc)(1:4).eq.'CEIL')then
                  if(ICT(icc).eq.0.or.ICT(icc).eq.4)then
                    WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','ROOF',
     &                com,'   !- Surface Type'
                  elseif(ICT(icc).eq.3)then
                    WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','CEILING',
     &                com,'   !- Surface Type'
                  endif
                elseif(SSVFC(icc)(1:4).eq.'FLOR')then
                  WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','FLOOR',com,
     &              '   !- Surface Type'
                endif

                WRITE(ioout,'(4a)',iostat=ios,err=6) '  ',
     &            SSMLCN(icc)(1:lnblnk(SSMLCN(icc))),com,
     &            '   !- Construction Name of the Surface'
                WRITE(ioout,'(4a)',iostat=ios,err=6) '  ',
     &            zname(mz)(1:lnzname(mz)),
     &            com,'  !- InsideFaceEnvironment'

C Other face environment.
                if(ICT(icc).eq.0)then
                  WRITE(ioout,'(4a)',iostat=ios,err=6) '  ',
     &              'ExteriorEnvironment',
     &              com,'   !-  OutsideFaceEnvironment'
                  WRITE(ioout,'(3a)',iostat=ios,err=6) '  ',com,
     &              '   !- OutsideFaceEnvironment Object'
                  WRITE(ioout,'(4a)',iostat=ios,err=6) '  ',
     &              'SunExposed',com,'   !- Sun Exposure'
                  WRITE(ioout,'(4a)',iostat=ios,err=6) '  ',
     &              'WindExposed',com,'   !- Wind Exposure'
                  if(siteexposureindex.ne.8)then
                    if(SSVFC(icc)(1:4).eq.'VERT')then
                      WRITE(ioout,'(a,f5.3,2a)',iostat=ios,err=6) '  ',
     &                  PROPG(siteexposureindex),com,
     &                  '   !- View Factor to Ground'
                    elseif(SSVFC(icc)(1:4).eq.'CEIL')then
                      WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','0.0',
     &                  com,'   !- View Factor to Ground'
                    elseif(SSVFC(icc)(1:4).eq.'SLOP'.or.
     &                     SSVFC(icc)(1:4).eq.'UNKN')then
                      WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','0.0',
     &                  com,'   !- View Factor to Ground'
                    elseif(SSVFC(icc)(1:4).eq.'FLOR')then
                      WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','1.0',
     &                  com,'   !- View Factor to Ground'
                    endif
                  else
                    if(SSVFC(icc)(1:4).eq.'VERT')then
                      WRITE(ioout,'(a,f5.3,2a)',iostat=ios,err=6) '  ',
     &                  groundview,com,'   !- View Factor to Ground'
                    elseif(SSVFC(icc)(1:4).eq.'CEIL')then
                      WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','0.0',
     &                  com,'   !- View Factor to Ground'
                    elseif(SSVFC(icc)(1:4).eq.'SLOP'.or.
     &                     SSVFC(icc)(1:4).eq.'UNKN')then
                      WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','0.0',
     &                  com,'   !- View Factor to Ground'
                    elseif(SSVFC(icc)(1:4).eq.'FLOR')then
                      WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','1.0',
     &                  com,'   !- View Factor to Ground'
                    endif
                  endif
                elseif(ICT(icc).eq.1)then

C Continue because this has been considered internal mass.
                  continue
                elseif(ICT(icc).eq.2)then

C A constant connection still has to be resolved.
                  continue
                elseif(ICT(icc).eq.3)then
                  WRITE(ioout,'(4a)',iostat=ios,err=6) '  ',
     &              'OtherZoneSurface',com,
     &              '   !-  OutsideFaceEnvironment'
                  call zsid(ic2(icc),ie2(icc),ZSDES,ZSDESC,ZSDESS)
                  WRITE(ioout,'(4a)',iostat=ios,err=6) '  ',
     &              ZSDES(1:lnblnk(ZSDES)),com,
     &              '   !- OutsideFaceEnvironment Object'
                  WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','NoSun',
     &              com,'   !- Sun Exposure'
                  WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','NoWind',
     &              com,'   !- Wind Exposure'
                  WRITE(ioout,'(3a)',iostat=ios,err=6) '  ',com,
     &              '   !- View Factor to Ground'
                elseif(ICT(icc).eq.4.or.ICT(icc).eq.6)then
                  WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','Ground',
     &              com,'   !-  OutsideFaceEnvironment'
                  WRITE(ioout,'(3a)',iostat=ios,err=6) '  ',com,
     &              '   !- OutsideFaceEnvironment Object'
                  WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','NoSun',
     &              com,'   !- Sun Exposure'
                  WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','NoWind',
     &              com,'   !- Wind Exposure'
                  WRITE(ioout,'(3a)',iostat=ios,err=6) '  ',com,
     &              '   !- View Factor to Ground'
                endif
              else

C There is a parent for this surface.
                WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','DOOR',com,
     &            '   !- Surface Type!'
                WRITE(ioout,'(4a)',iostat=ios,err=6) '  ',
     &            SSMLCN(icc)(1:lnblnk(SSMLCN(icc))),com,
     &            '   !- Construction Name of the Surface'
                lsn0=lnblnk(SSPARENT(icc))
                lzn0=lnzname(mz)
                WRITE(ZSDES,'(a,a1,a)',iostat=ios,err=6) 
     &            SSPARENT(icc)(1:lsn0),':',zname(mz)(1:lzn0)
                WRITE(ioout,'(4a)',iostat=ios,err=6) '  ',
     &            ZSDES(1:lnblnk(ZSDES)),com,
     &            '   !- Base Surface Name'

C If external environment (external door) then OutsideFaceEnvironment Object must remain blank
C Otherwise give name of the other side surface. (suggested by Georgios)
                if(ICT(icc).eq.0)then
                  WRITE(ioout,'(3a)',iostat=ios,err=6) '  ',com,
     &              '   !- OutsideFaceEnvironment Object'
                else
                  if(ic2(icc).eq.0.or.ie2(icc).eq.0)then  ! if unknown zone & surface
                    WRITE(ioout,'(3a)',iostat=ios,err=6) '  ',com,
     &                '   !- OutsideFaceEnvironment Object'
                  else  ! if known zone & surface
                    call zsid(ic2(icc),ie2(icc),ZSDES,ZSDESC,ZSDESS)
                    WRITE(ioout,'(4a)',iostat=ios,err=6) '  ',
     &                ZSDES(1:lnblnk(ZSDES)),com,
     &                '   !- OutsideFaceEnvironment Object'
                  endif
                endif 
                WRITE(ioout,'(3a)',iostat=ios,err=6) '  ',com,
     &            '   !- View Factor to Ground'
                WRITE(ioout,'(3a)',iostat=ios,err=6) '  ',com,
     &            '   !-   Name of shading control'
                WRITE(ioout,'(3a)',iostat=ios,err=6) '  ',com,
     &            '   !-   WindowFrameAndDivider NameName'
                WRITE(ioout,'(4a)',iostat=ios,err=6) '  ','1.0',com,
     &            '   !-   Multiplier'
              endif
            endif
          endif

C Vertex information. If there were more than 4 vertices, but
C not a wall with a hole in it then write them out.

C << move towards no space between coordinates >>

          if(NVER(IS).gt.4.and.NVER(IS).ne.10)then
            nvertex=iedgcnt
            WRITE(ioout,'(a,i2,2a)',iostat=ios,err=6) '  ',nvertex,
     &        com,'   !- Number of Surface Vertex Groups'
            do 42 iss=1,nvertex
              if(iss.lt.nvertex)then
                WRITE(ioout,'(a,f10.4,a,f10.4,a,f10.4,2a)',
     &            iostat=ios,err=6) '  ',COORD(iss,1),com,
     &            COORD(iss,2),com,COORD(iss,3),
     &            com,'   !- X Y Z [m]'
              else
                WRITE(ioout,'(a,f10.4,a,f10.4,a,f10.4,2a)',
     &            iostat=ios,err=6) '  ',COORD(iss,1),com,
     &            COORD(iss,2),com,COORD(iss,3),
     &            sim,'   !- X Y Z [m]'
              endif
  42        continue
          else

C For the case of a surface with a window in it.

C << there should be a data structure that remembers that when
C << the surface was created it was as a hole in a wall or as
C << a door in a wall or as a surface with two windows in it
            if(NVER(IS).eq.10)then
              nvertex=4
            else
              nvertex=NVER(IS)
            endif
            WRITE(ioout,'(a,i1,2a)',iostat=ios,err=6) '  ',nvertex,com,
     &        '   !- Number of Surface Vertex Groups'
            do 43 iss=1,nvertex
              if(iss.lt.nvertex)then
                WRITE(ioout,'(a,f10.4,a,f10.4,a,f10.4,2a)',
     &            iostat=ios,err=6) '  ',X(JVN(is,iss)),com,
     &            Y(JVN(is,iss)),com,Z(JVN(is,iss)),
     &            com,'   !- X Y Z [m]'
              else
                WRITE(ioout,'(a,f10.4,a,f10.4,a,f10.4,2a)',
     &            iostat=ios,err=6) '  ',X(JVN(is,iss)),com,
     &            Y(JVN(is,iss)),com,Z(JVN(is,iss)),
     &            sim,'   !- X Y Z [m]'
              endif
  43        continue
          endif
  60    continue

C Check where obstructions are located. Details are in common.
        IF(IOBS(mz).EQ.2)THEN
          if(.NOT.newgeo)then
            call edisp(iuout,'Confused where obstructions are.')
          endif
        ENDIF
        if(nbobs(mz).gt.0)then
          DO 301 IB=1,nbobs(mz)

C For each obstruction convert and write out equivalent syntax.
C depending on whether 'obs' or 'obs3'.
            if(BLOCKTYP(mz,IB)(1:4).eq.'obs ')then
              CALL CNVBLK(XOB(mz,IB),YOB(mz,IB),ZOB(mz,IB),
     &          DXOB(mz,IB),DYOB(mz,IB),DZOB(mz,IB),BANGOB(mz,IB,1))
            elseif(BLOCKTYP(mz,IB)(1:4).eq.'obs3')then
              CALL CNVBLK3A(XOB(mz,IB),YOB(mz,IB),ZOB(mz,IB),
     &          DXOB(mz,IB),DYOB(mz,IB),DZOB(mz,IB),BANGOB(mz,IB,1),
     &          BANGOB(mz,IB,2),BANGOB(mz,IB,3))
            elseif(BLOCKTYP(mz,IB)(1:4).eq.'obsp')then
              CALL CNVBLKP(mz,IB) ! convert obsp type.
            endif

            do 302 ibs=1,6   ! for each of the 6 surfaces of block
              WRITE(ioout,'(a)',iostat=ios,err=6) ' '
              WRITE(ioout,'(2a)',iostat=ios,err=6) 
     &          'Surface:Shading:Detached:Building',com
              WRITE(ioout,'(2a,i1,2a)',iostat=ios,err=6) '  ',
     &          BLOCKNAME(mz,ib)(1:LNBLOCKNAME(mz,ib)),ibs,com,
     &          '   !- User Supplied Surface Name'
              WRITE(ioout,'(5a)',iostat=ios,err=6) '  ',com,
     &          '   !- TransSchedShadowSurf'
              nvertex=4
              WRITE(ioout,'(a,i1,2a)',iostat=ios,err=6) '  ',nvertex,
     &          com,'   !- Number of Surface Vertex Groups'
              do 44 iss=1,nvertex
                if(iss.lt.nvertex)then
                  WRITE(ioout,'(a,f10.4,a,f10.4,a,f10.4,2a)',
     &              iostat=ios,err=6) '  ',XB(JVNB(ibs,iss)),com,
     &              YB(JVNB(ibs,iss)),com,ZB(JVNB(ibs,iss)),com,
     &              '   !- X Y Z [m]'
                else
                  WRITE(ioout,'(a,f10.4,a,f10.4,a,f10.4,2a)',
     &              iostat=ios,err=6) '  ',XB(JVNB(ibs,iss)),com,
     &              YB(JVNB(ibs,iss)),com,ZB(JVNB(ibs,iss)),sim,
     &            '   !- X Y Z [m]'
                endif
  44          continue
 302        continue
 301      continue
        endif
  14  continue
      call edisp(iuout,'Completed export of data to EnergyPlus.')

C Reports - report details and dxf for surfaces
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      WRITE(ioout,'(2a)',iostat=ios,err=2) 'Report',com
      WRITE(ioout,'(8a)',iostat=ios,err=2) '  ','Surfaces',com,
     &  'DETAILS',com,'Thick Polyline',sim,
     &  '   !- Type, Name and Specifications for reports'
     
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      WRITE(ioout,'(2a)',iostat=ios,err=2) 'Report',com
      WRITE(ioout,'(8a)',iostat=ios,err=2) '  ','Surfaces',com,
     &  'DXF',com,'Thick Polyline',sim,
     &  '   !- Type, Name and Specifications for reports'

C Include directive for EnergyPlus to dump out additional diagnotics
C for geometry. 
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      WRITE(ioout,'(5a)',iostat=ios,err=2) 'DIAGNOSTICS',com,
     &  'DisplayExtraWarnings',sim,'   !- Extra diagnostics'
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
     
      return

C Error.
  103 call edisp(iuout,'Error opening climate file.')
      return

C Error messages from header.
    2 if(IOS.eq.2)then
        CALL USRMSG('Permissions issue writing EnergyPlus header',
     &    ' ','W')
      else
        CALL USRMSG('Problem writing EnergyPlus header',' ','W')
      endif
      return

C Error messages from materials.
    3 if(IOS.eq.2)then
        CALL USRMSG('Permissions issue writing EnergyPlus materials',
     &    ' ','W')
      else
        CALL USRMSG('Problem writing EnergyPlus materials',' ','W')
      endif
      return

C Error messages from constructions.
    4 if(IOS.eq.2)then
        CALL USRMSG('Permission issue writing EnergyPlus construction',
     &    ' ','W')
      else
        CALL USRMSG('Problem writing EnergyPlus construction',' ','W')
      endif
      return

C Error messages from zones.
    5 if(IOS.eq.2)then
        CALL USRMSG('Permission issue writing EnergyPlus zones',
     &    ' ','W')
      else
        CALL USRMSG('Problem writing EnergyPlus zones',' ','W')
      endif
      return

C Error messages from zones.
    6 if(IOS.eq.2)then
        CALL USRMSG('Permission issue writing EnergyPlus surfaces',
     &    ' ','W')
      else
        CALL USRMSG('Problem writing EnergyPlus surfaces',' ','W')
      endif
      return

C Error messages from zones.
    7 if(IOS.eq.2)then
        CALL USRMSG('Permission issue writing EnergyPlus schedules',
     &    ' ','W')
      else
        CALL USRMSG('Problem writing EnergyPlus schedules',' ','W')
      endif
      return

      end

