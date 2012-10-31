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

C ESRU EnergyPlus translator to version 1.4 to version 3.0.
C Current source version Dec 2010.

C E2eplus: Inside and outside materials and composition and schedules generation.

C ******* Translate esp data to EnergyPlus file geom and materials.
      SUBROUTINE e2eplus(itrc,ioout,ver,IER)

C itrc (integer) =0 turns off reporting, =1 brief
C ioout (integer) is the file unit to write to
C ver (real) should be 1.4 or 2.0 or 3.0
#include "building.h"
#include "model.h"
#include "site.h"

C geometry.h provides commons G0/G1/G2/G4/G5/G7/G9/prec2/prec17/precz/GS5/GS6.
#include "geometry.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure database)
C LOPTDB,IOPTDB (for optical database)
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
C ICLIM,LCLIM (for climate file)
#include "material.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)

      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/GOPT/DG(5),HG(5),UVAL,VTRN,NTL,AB(ME,5),RF(ME),SRF,SAB

      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)
      common/cctlnm/ctldoc,lctlf

C Version of operations file. ip3ver=0 standard, =1 sorted with header
C =2 with more than 3 day types
      common/p3ver/ip3ver(MCOM)

      CHARACTER oprdesc*248,ventdesc*248,ctlstr*24
      COMMON/P1/oprdesc(MCOM),ventdesc(MCOM),ctlstr(MCOM,MDTY)

C Descriptive label for a zone load or casual gain types.
      character lodlabel*6
      common/loadlabel/lodlabel(mcom,MGTY)

C Peaks (loadcasp(z,ty,1) sens)(loadcasp(z,ty,2) latent) for each
C possible gain type in each zone.
C Radiant fraction loadradfrac(z,ty) for each possible gain type
C in each zone
      real loadcasp,loadradfrac
      common/loadcas/loadcasp(mcom,7,2),loadradfrac(mcom,7)

C loadonday(zone,daytype,gaintype) is true if there is a gain
C of a particular type on a particular day. Needed to ensure that
C compact schedule is correctly written out.
      common/loadday/loadonday(mcom,MDTY,7)

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32

      COMMON/P3N/NCAS(MDTY),ICGS(MDTY,MC),ICGF(MDTY,MC),
     &CMGS(MDTY,MC),CMGL(MDTY,MC),RADC(MDTY,MC),CONC(MDTY,MC)
      COMMON/P3TYPEN/ICGT(MDTY,MC)
      INTEGER NCAS,ICGS,ICGF,ICGT
      REAL CMGS,CMGL,RADC,CONC

C Simulation parameter presets.
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/spflper/isstday(MSPS),isstmon(MSPS),isfnday(MSPS),
     &               isfnmon(MSPS)
      COMMON/CLMDT1/CLMLOC
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)

      integer matarrayindex ! the indes within matdatarray
      logical closemat1,closemat2

      character GDESCR*36
      CHARACTER NAM*72,outs*124
      character CLMLOC*30
      character ctldoc*248,LCTLF*72
      character ZSDES*28,ZSDESC*20,ZSDESS*16,mlc_name*12
      character dstmp*24
      CHARACTER LAPROB*72

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
      real ver  ! file version passed to subroutine
      real rfr   ! local var for radiant fraction
      integer iver     ! file version as integer
      integer ier
      integer ich,iedg ! loop variable
      integer icc,iccc ! array index
      character optndx*12,nameiprndx*25,OPT*12,com*1,sim*1
      character act*1,layname*25
      character T25*25,T25a*25,t12a*12
      character*10 wkday(7)
      character*9 RAMONTH(12)
      character*4 indent   ! for indentation
      character sitestring*48  ! for view factor to ground
      character conststring*48 ! for use with constant boundary
      logical usethisone   ! array if true then surface can be used
      logical truechild    ! array if true then E+ child
      dimension usethisone(MCON),truechild(MCON)
      integer jvn1,iduploop,foundone ! for use in avoiding child duplicate vertices
      integer iccsimilar   ! connection to use in case of similar
      dimension jvn1(MV)

      logical found,close,close1,foundmlc
      logical closea
      logical haveobs,haveish,havehc,haveusrgrnd,havestdgrnd,XST
      logical loadonday,ok,dok
      logical newgeo  ! to use for testing if new/old geometry file
      logical findchildvert  ! to remember child vertices
      logical treatasmass    ! if true treat surface as internal mass
      logical processasfen   ! if true treat surface as fenistration

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

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
      helpinsub='e2eplus'  ! set for subroutine
      ITA1 = IFIL+6

C Set character variable com to be a single comma and sim to be a
C simicolon. 
      com=','
      sim=';'
      newgeo=.false.  ! assume older format geometry.

C Check the version number and set iver as well as a character variable
C to represent a 2 char or 4 char indentation.
      iver = 00
      call eclose(ver,3.0,0.001,closea)
      if(closea)then
        iver = 30; indent = '    '
      else
        call usrmsg('Only V3. IDF files recognized',' ','W')
        return
      endif

C Open the material and mlc db. Check that the materials arrays
C have been filled (warn user if not).
      call opendb(ier)
      call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)
      if(closemat1.or.closemat2)then
        continue
      else
        WRITE(ioout,'(a)')' '
        WRITE(ioout,'(a)')'!- == WARNING MATERIAL ARRAYS INCOMPLETE =='
      endif

C First task is to loop through each of the mlc and get a
C list of materials used in this problem. Clear the local
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
      if(iver.eq.30)then
        WRITE(ioout,'(5a)',iostat=ios,err=2) '  VERSION',com,'3.0',sim,
     &  '   !- Version Identifier (for testing)'
      endif
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      WRITE(ioout,'(2a)',iostat=ios,err=2) '  Building',com
      WRITE(ioout,'(4a)',iostat=ios,err=2) indent,
     &  cfgroot(1:lnblnk(cfgroot)),com,'   !- Name'
      WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'0.000',com,
     &  '   !- North Axis {deg}'

C Interpret site position.
      if(siteexposureindex.eq.1)then
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'City',com,
     &    '   !- Terrain'
      elseif(siteexposureindex.eq.2)then
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'Suburbs',com,
     &    '   !- Terrain'
      elseif(siteexposureindex.eq.3)then
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'Country',com,
     &    '   !- Terrain'
      elseif(siteexposureindex.eq.4)then
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'City',com,
     &    '   !- Terrain'
      elseif(siteexposureindex.eq.5)then
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'City',com,
     &    '   !- Terrain'
      elseif(siteexposureindex.eq.6)then
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'Country',com,
     &    '   !- Terrain'
      elseif(siteexposureindex.eq.7)then
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'City',com,
     &    '   !- Terrain'
      elseif(siteexposureindex.eq.8)then
        WRITE(ioout,'(3a)',iostat=ios,err=2) indent,com,
     &    '   !- Terrain (user specified in ESP-r)'
      endif
      WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'0.04',com,
     &  '   !- Loads Convergence Tolerance Value'
      WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'0.4',com,
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
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,
     &    'FullInteriorAndExterior',com,'   !- Solar Distribution'
      else
        if(haveobs)then
          WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'FullExterior',
     &      com,'   !- Solar Distribution'
        else
          WRITE(ioout,'(4a)',iostat=ios,err=2) indent,
     &      'MinimalShadowing',com,'   !- Solar Distribution'
        endif
      endif

C Interpret warmup days. Use simulation parameter set value or calculate
C via call to scntcnst.
      if(nsset.gt.0)then
        WRITE(ioout,'(a,i2,2a)',iostat=ios,err=2) indent,isstup,sim,
     &    '   !- Maximum Number of Warmup Days (from sim param set)'
      else
         call scntcnst(TDM,istd,TCM,ISTC)
        WRITE(ioout,'(a,i2,2a)',iostat=ios,err=2) indent,ISTC,sim,
     &      '   !- Maximum Number of Warmup Days (from MLC TCM)'
      endif

C Interpret timesteps. Use simulation parameter set value if <= 6 or set to one.
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      WRITE(ioout,'(2a)',iostat=ios,err=2) '  Timestep',com
      if(nsset.gt.0)then
        if(isbnstep.gt.8)then
          WRITE(ioout,'(4a,i2,a)',iostat=ios,err=2) indent,'8',sim,
     &      '   !- Time Step in Hour (sim param set was ',isbnstep,')'
        elseif(isbnstep.lt.4)then
          WRITE(ioout,'(4a,i2,a)',iostat=ios,err=2) indent,'4',sim,
     &      '   !- Time Step in Hour (sim param set was ',isbnstep,')'
        else
          WRITE(ioout,'(a,i2,2a)',iostat=ios,err=2) indent,isbnstep,
     &      sim,'   !- Time Step in Hour (from sim param set)'
        endif
      else
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'4',sim,
     &    '   !- Time Step in Hour (default)'
      endif

C Inside convection. If there are convection regime files
C give note to user to check. Set `Detailed`.
C << note there is also a zone field to over-ride >>
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      WRITE(ioout,'(2a)',iostat=ios,err=2) 
     &  '  SurfaceConvectionAlgorithm:Inside',com
      if(havehc)then
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'Detailed',sim,
     &    '   !- [please check equivalent ESP-r HC regime]'
      else
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'Detailed',sim,
     &    '   !- Algorithm (default)'
      endif

      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      WRITE(ioout,'(2a)',iostat=ios,err=2) 
     &  'SurfaceConvectionAlgorithm:Outside',com
      WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'Detailed',sim,
     &  '   !- Algorithm (default)'

C Solution - assume no moisture solution.
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      if(iver.eq.30)then
        WRITE(ioout,'(2a)',iostat=ios,err=2) 
     &    '  HeatBalanceAlgorithm,ConductionTransferFunction',sim
      else

      endif

C Interpret shading calculations. If shading used in ESP-r then
C set to 28 days, otherwise set zero=default.
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      WRITE(ioout,'(2a)',iostat=ios,err=2) '  ShadowCalculation',com
      if(haveish)then
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'28',sim,
     &    '   !- (to match ESP-r ish period)'
      else
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'20',sim,
     &    '   !- (default frequency of calculation)'
      endif

C Run control - set all fields to no so only run period is assessed.
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      if(haveish)then
        WRITE(ioout,'(2a)',iostat=ios,err=2) 'RUN CONTROL',com
      else
        WRITE(ioout,'(2a)',iostat=ios,err=2) '  SimulationControl',com
      endif
      WRITE(ioout,'(8a)',iostat=ios,err=2) indent,'No',com,'No',com,
     & 'No',com,'   !- no zone sizing or system sizing or plant sizing'
      WRITE(ioout,'(6a)',iostat=ios,err=2) indent,'No',com,'Yes',sim,
     &  '   !- no design day - use weather file'

C Interpret run period. Use simulation parameter set values or set default.
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      if(iver.eq.30)then
        WRITE(ioout,'(2a)',iostat=ios,err=2) '  RunPeriod',com
      else

      endif
      if(nsset.gt.0)then
        WRITE(ioout,'(a,i2,a,i2,2a)',iostat=ios,err=2) indent,
     &    isstmon(1),com,isstday(1),com,
     &    '   !- Begin Month & Day Of Month (from sim param set)'
        WRITE(ioout,'(a,i2,a,i2,2a)',iostat=ios,err=2) indent,
     &    isfnmon(1),com,isfnday(1),com,
     &    '   !- End Month & Day Of Month (from sim param set)'
        CALL EWEEKD(isstday(1),isstmon(1),IYEAR,IDWK)

C Interpret day of week for start day from project year.
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,
     &    wkday(idwk)(1:lnblnk(wkday(idwk))),com,
     &    '   !- Day Of Week For Start Day'
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'No',com,
     &    '   !- Use WeatherFile Holidays/Special Days'
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'No',com,
     &    '   !- Use WeatherFile DaylightSavingPeriod'
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'No',com,
     &    '   !- Apply Weekend Holiday Rule'
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'No',com,
     &    '   !- Use WeatherFile Rain Indicators'
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'No',sim,
     &    '   !- Use WeatherFile Snow Indicators'
      else
        WRITE(ioout,'(5a)',iostat=ios,err=2) indent,'1',com,'1',com,
     &    '   !- Begin Month & Day Of Month (default)'
        WRITE(ioout,'(5a)',iostat=ios,err=2) indent,'12',com,
     &    '30',com,'   !- End Month & Day Of Month (default)'
        CALL EWEEKD(1,1,IYEAR,IDWK)

C Interpret day of week for start day from project year.
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,
     &    wkday(idwk)(1:lnblnk(wkday(idwk))),com,
     &    '   !- Day Of Week For Start Day'
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'No',com,
     &    '   !- Use WeatherFile Holidays/Special Days'
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'No',com,
     &    '   !- Use WeatherFile DaylightSavingPeriod'
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'No',com,
     &    '   !- Apply Weekend Holiday Rule'
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'No',com,
     &    '   !- Use WeatherFile Rain Indicators'
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'No',sim,
     &    '   !- Use WeatherFile Snow Indicators'
      endif

C Interpret location from climate file location name, but use
C the configuration file data for lat and long.
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      WRITE(ioout,'(2a)',iostat=ios,err=2) '  Site:Location',com
      ier=0
      call EFOPRAN(ICLIM,LCLIM,144,0,IER)
      if(ier.eq.0)then
        IREC=367
        READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=103)CLMLOC
        CALL ERPFREE(ICLIM,ISTAT)
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,
     &    CLMLOC(1:lnblnk(CLMLOC)),com,'   !- LocationName'
      else
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'not_yet_set',com,
     &    '   !- LocationName'
      endif
      WRITE(ioout,'(a,f7.3,2a)',iostat=ios,err=2) indent,sitelat,com,
     &  '   !- Latitude {deg}'

      WRITE(ioout,'(a,f7.3,2a)',iostat=ios,err=2) indent,sitelongdif,
     &  com,'   !- Longitude {deg} [check this value]'

C Ask user where the current time meridian is.
      IIC2=0
      helptopic='eplus_needs_to_know'
      call gethelptext(helpinsub,helptopic,nbhelp)
      IIC2T=IIC2
      CALL EASKI(IIC2T,' Time zone (hours +- GTM) ',
     &  ' ',-11,'F',11,'F',1,'time zone difference',IERI,nbhelp)
      if(ieri.eq.-3)then
        WRITE(ioout,'(3a)',iostat=ios,err=2) '  0',com,
     &    '   !- TimeZone {hr} [not yet supplied]'
      else
        iic2=iic2t
        WRITE(ioout,'(a,i3,2a)',iostat=ios,err=2) indent,iic2,com,
     &    '   !- TimeZone {hr} [supplied by user]'
      endif

      SLM=10.0
      CALL EASKR(SLM,' Site elevation above sea level (m) ',
     &  ' ',-100.0,'F',10000.0,'F',10.0,'site elevation',
     &  IER,nbhelp)
      WRITE(ioout,'(a,f7.1,2a)',iostat=ios,err=2) indent,SLM,sim,
     &  '   !- Elevation {m} [supplied by user]'

C Interpret ground temperatures. Loop through each connection
C and check which standard or user defined ground temperature
C profile is being used. EnergyPlus can only take one so if
C there is more than one referenced warn the user. There is
C no equivalent to NRCan BASESIMP but there is a ground temperature
C calculation module in EnergyPlus.
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      WRITE(ioout,'(2a)',iostat=ios,err=2) 
     &  '  Site:GroundTemperature:BuildingSurface',com
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
            WRITE(ioout,'(a,f7.4,4a)',iostat=ios,err=2) indent,
     &        UGRDTP(im,igrp),com,'   !- ',RAMONTH(im),
     &        ' Ground Temperature {C}'
          else
            WRITE(ioout,'(a,f7.4,4a)',iostat=ios,err=2) indent,
     &        GRDTMP(im,igrp),com,'   !- ',RAMONTH(im),
     &        ' Ground Temperature {C}'
          endif
  16    continue
        if(haveusrgrnd)then
          WRITE(ioout,'(a,f7.4,4a)',iostat=ios,err=2) indent,
     &      UGRDTP(12,igrp),sim,'   !- ',RAMONTH(12),
     &      ' Ground Temperature {C}'
        else
          WRITE(ioout,'(a,f7.4,4a)',iostat=ios,err=2) indent,
     &      GRDTMP(12,igrp),sim,'   !- ',RAMONTH(12),
     &      ' Ground Temperature {C}'
        endif
      endif


C Scan for materials.
      DO 20 INO=1,NMLC

C Loop through each construction in the database, check if it
C is used in the model and if so proceed to check.
        mlc_name=DESC(ino)(1:12)
        call mlcrefs(mlc_name,areamlc,tareamlc,foundmlc)
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

C For each layer get material arraay index and then check either thickness
C or air gap resistance before adding it to the iprndx array.

          if(iij.gt.0)then
            close1 = .false.
            do 24 ij = 1,iij
              found = .false.
              matarrayindex=IPRMAT(INO,IL)   ! which material index
              if(matarrayindex.eq.iprndx(ij))found = .true.
 
C And if a gap layer reset dbcon.
              if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &           matopaq(matarrayindex)(1:1).eq.'h'.or.
     &           matarrayindex.eq.0)then

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

C If not already known add it to the list.
            if(.NOT.close1)then
              iij=iij+1
              iprndx(iij)=IPRMAT(INO,IL)
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
              matarrayindex=IPRMAT(INO,IL)   ! which material index
              if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &           matopaq(matarrayindex)(1:1).eq.'h'.or.
     &           matarrayindex.eq.0)then
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
            iprndx(iij)=IPRMAT(INO,IL)
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

      do 25 iy = 1,nmat
        matarrayindex=iprndx(iy)   ! which material array index

C A gap layer.
        if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &     matopaq(matarrayindex)(1:1).eq.'h'.or.
     &     matarrayindex.eq.0)then
          DBCON=0.0; DBDEN=0.0; DBSHT=0.0 
          E=0.99; A=0.99
          NAM='AIR'

C Make up air name for opaque construction. t12a holds air gap resistance
C as a string w/o leading blanks (use only first 7 char of t12a).
          if(iglassndx(iy).eq.0)then
            WRITE(ioout,'(a)',iostat=ios,err=3) ' '
            WRITE(ioout,'(2a)',iostat=ios,err=3) '  Material:AirGap',
     &        com
            call relstr(riair(iy),t12a,lna,ier)
            write(t25,'(3a)') 'Air-gap',':',t12a(1:7)
            call st2name(T25,T25a)
            write(nameiprndx(iy),'(a)') T25a
            WRITE(ioout,'(4a)',iostat=ios,err=3) indent,
     &        T25a(1:lnblnk(T25a)),com,'   !- Name'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,
     &        riair(iy),sim,'   !- Thermal Resistance {m2-K/W}'
          else

C For the case of an air gap in a window, assume air is the gas and
C write out the thickness (rather than the air gap resistence, but only
C the first 7 characters of t12a).
            WRITE(ioout,'(a)',iostat=ios,err=3) ' '
            WRITE(ioout,'(2a)',iostat=ios,err=3) 
     &        '  WindowMaterial:Gas',com
            call relstr(thkiprndx(iy),t12a,lna,ier)
            write(t25,'(3a)') 'Air-win',':',t12a(1:7)
            call st2name(T25,T25a)
            write(nameiprndx(iy),'(a)') T25a
            WRITE(ioout,'(4a)',iostat=ios,err=3) indent,
     &        T25a(1:lnblnk(T25a)),com,'   !- Name'
            WRITE(ioout,'(4a)',iostat=ios,err=3) indent,'Air',com,
     &        '   !- Gas Type'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,
     &        thkiprndx(iy),sim,'   !- Thickness {m}'
          endif
        else
          WRITE(ioout,'(a)',iostat=ios,err=3) ' '
          if(iglassndx(iy).eq.0)then
            WRITE(ioout,'(2a)',iostat=ios,err=3) '  Material',com
          else
            WRITE(ioout,'(2a)',iostat=ios,err=3) 
     &        '  WindowMaterial:Glazing',com
          endif
          matarrayindex=iprndx(iy)   ! which material array index
 
          DBCON=matdbcon(matarrayindex)
          DBDEN=matdbden(matarrayindex)
          DBSHT=matdbsht(matarrayindex)
          E=matdbine(matarrayindex)
          A=matdbina(matarrayindex)
          write(NAM,'(a)') matname(matarrayindex)(1:32)

C Make up name. t12a holds the thickness as a string w/o leading blanks
C (but only use the initial 7 characters of t12a.
C Take up to 12 char from NAM and the remove any blanks via st2name.
          ln = MIN0(lnblnk(NAM),12)
          call relstr(thkiprndx(iy),t12a,lna,ier)
          write(t25,'(3a)') NAM(1:ln),':',t12a(1:7)
          call st2name(T25,T25a)
          write(nameiprndx(iy),'(a)') T25a
          WRITE(ioout,'(4a)',iostat=ios,err=3) indent,
     &      T25a(1:lnblnk(T25a)),com,'   !- Name'
          if(iglassndx(iy).eq.0)then
            WRITE(ioout,'(4a)',iostat=ios,err=3) indent,'Rough',com,
     &        '   !- Roughness'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,
     &        thkiprndx(iy),com,'   !- Thickness {m}'
            if(dbcon.gt.99.0)then
              WRITE(ioout,'(a,f9.2,2a)',iostat=ios,err=3) indent,DBCON,
     &          com,'   !- Conductivity {W/m-K}'
            else
              WRITE(ioout,'(a,f7.4,2a)',iostat=ios,err=3) indent,DBCON,
     &          com,'   !- Conductivity {W/m-K}'
            endif
            WRITE(ioout,'(a,f9.4,2a)',iostat=ios,err=3) indent,DBDEN,
     &        com,'   !- Density {kg/m3}'
            WRITE(ioout,'(a,f7.2,2a)',iostat=ios,err=3) indent,DBSHT,
     &        com,'   !- Specific Heat {J/kg-K}'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,E,com,
     &        '   !- Thermal Absorptance'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,A,com,
     &        '   !- Solar Absorptance'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,A,sim,
     &        '   !-  Visible Absorptance'
          else
            WRITE(ioout,'(4a)',iostat=ios,err=3) indent,
     &        'SpectralAverage',com,'   !- Optical Data Type'
            WRITE(ioout,'(3a)',iostat=ios,err=3) indent,com,
     &        '   !- Window Glass Spectral Data Set Name'
            WRITE(ioout,'(a,f7.4,2a)',iostat=ios,err=3) indent,
     &        thkiprndx(iy),com,'   !- Thickness {m}'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,
     &        soltrn(iy),com,
     &        '   !- Solar Transmittance at Normal Incidence'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,
     &        solrefl(iy),com,
     &        '   !- Front Side Solar Reflectance at Normal Incidence'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,
     &        solrefl(iy),com,
     &        '   !- Back Side Solar Reflectance at Normal Incidence'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,
     &        vistrn(iy),com,
     &        '   !- Visible Transmittance at Normal Incidence'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,
     &        visrefl(iy),com,
     &       '   !- Front Side Visible Reflectance at Normal Incidence'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,
     &        visrefl(iy),com,
     &       '   !- Back Side Visible Reflectance at Normal Incidence'
            WRITE(ioout,'(4a)',iostat=ios,err=3) indent,'0.00',com,
     &       '   !- Infrared Transmittance at Normal Incidence'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,E,
     &        com,'   !- Front Side Infrared Hemispherical Emissivity'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,E,
     &        com,'   !- Back Side Infrared Hemispherical Emissivity'
            WRITE(ioout,'(a,f7.4,2a)',iostat=ios,err=3)   indent,DBCON,
     &        sim,'   !- Conductivity {W/m-K}'
          endif
        endif
  25  continue

C Now that materials are known, look back over constructions
C and assign iconmatpointer for each layer.
      DO 200 INO=1,NMLC

C Loop through each construction in the database, check if it
C is used in the model and if so proceed to check.
        mlc_name=DESC(ino)(1:12)
        call mlcrefs(mlc_name,areamlc,tareamlc,foundmlc)
        if(.NOT.foundmlc) goto 200

        WRITE(OPT,'(A)') DESC(INO)(21:32)
        DO 230 IL=1,LAYERS(INO)

C For each layer get material array index and then check either thickness
C or air gap resistance before adding it to the iprndx array.
          close1 = .false.
          do 240 ij = 1,nmat
            found = .false.
            matarrayindex=IPRMAT(INO,IL)   ! which material index
            if(matarrayindex.eq.iprndx(ij))found = .true.
            if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &         matopaq(matarrayindex)(1:1).eq.'h'.or.
     &         matarrayindex.eq.0)then

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
        mlc_name=DESC(ino)(1:12)
        call mlcrefs(mlc_name,areamlc,tareamlc,foundmlc)
        if(.NOT.foundmlc) goto 36

        WRITE(ioout,'(a)',iostat=ios,err=4) ' '
        WRITE(ioout,'(2a)',iostat=ios,err=4) '  Construction',com
        WRITE(ioout,'(4a)',iostat=ios,err=4) indent,
     &    mlc_name(1:lnblnk(mlc_name)),com,'   !- Name'    
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
              WRITE(ioout,'(4a)',iostat=ios,err=4) indent,
     &          layname(1:layl),sim,'   !- Outside Layer' 
            else                          
              WRITE(ioout,'(4a)',iostat=ios,err=4) indent,
     &          layname(1:layl),com,'   !- Outside Layer'
            endif
          elseif(il.eq.LAYERS(INO))then
            WRITE(ioout,'(4a,i2)',iostat=ios,err=4) indent,
     &        layname(1:layl),sim,'   !- Layer ',IL
          else
            WRITE(ioout,'(4a,i2)',iostat=ios,err=4) indent,
     &        layname(1:layl),com,'   !- Layer ',IL
          endif
  37    continue
  36  continue

C Typical schedule types.
      WRITE(ioout,'(a)',iostat=ios,err=7) ' '
      WRITE(ioout,'(a)',iostat=ios,err=7) 
     &  '!- ============= typical schedule types'
      if(iver.eq.30)then
        WRITE(ioout,'(5a)',iostat=ios,err=7) '  ScheduleTypeLimits',
     &    com,'Any Number',sim,'   !- Name'
      else

      endif
      if(iver.eq.30)then
        WRITE(ioout,'(9a)',iostat=ios,err=7) '  ScheduleTypeLimits',
     &    com,'Fraction',com,
     &    '0.0:1.0',com,'Continuous',sim,'  !- Name Range Numeric Type'
      else

      endif
      if(iver.eq.30)then
        WRITE(ioout,'(9a)',iostat=ios,err=7) '  ScheduleTypeLimits',
     &    com,'Temperature type',com,
     &    '-60:20.0',com,'Continuous',sim,' !- Name Range Numeric Type'
      else

      endif
      if(iver.eq.30)then
        WRITE(ioout,'(9a)',iostat=ios,err=7) '  ScheduleTypeLimits',
     &    com,'On/Off type',com,
     &    '0.0:40.0',com,'Discrete',sim,'   !- Name Range Numeric Type'
      else

      endif

C Write out a generic nothing-is-happening schedule for zones which
C have a null or all zero operation file.
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        WRITE(ioout,'(a)',iostat=ios,err=7) 
     &    '! Schedules for zones where nothing happens'
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  Schedule:Compact',com
        WRITE(ioout,'(3a)',iostat=ios,err=5) '    NOTHING',com,
     &    '   !- Name'
        WRITE(ioout,'(3a)',iostat=ios,err=7) '    Fraction',com,
     &    '   !- Schedule Type Limits Name' 
        WRITE(ioout,'(2a)',iostat=ios,err=7) '    Through: 12/31',com
        WRITE(ioout,'(2a)',iostat=ios,err=7) '    For: Weekdays',com
        WRITE(ioout,'(5a)',iostat=ios,err=7) '    Until: ',
     &    '24:00',com,'0.0',com
        WRITE(ioout,'(2a)',iostat=ios,err=7) '    For: Saturday',com
        WRITE(ioout,'(5a)',iostat=ios,err=7) '    Until: ',
     &    '24:00',com,'0.0',com
        WRITE(ioout,'(2a)',iostat=ios,err=7) '    For: Sunday',com
        WRITE(ioout,'(5a)',iostat=ios,err=7) '    Until: ',
     &    '24:00',com,'0.0',com
        WRITE(ioout,'(2a)',iostat=ios,err=7) '    For: AllOtherDays',com
        WRITE(ioout,'(5a)',iostat=ios,err=7) '    Until: ',
     &    '24:00',com,'0.0',sim

C Write out a generic it always-happens schedule.
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        WRITE(ioout,'(a)',iostat=ios,err=7) 
     &    '! Schedules for zones where something always happens'
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  Schedule:Compact',com
        WRITE(ioout,'(3a)',iostat=ios,err=5) '    CONSTANT',com,
     &    '   !- Name'
        WRITE(ioout,'(3a)',iostat=ios,err=7) '    Fraction',com,
     &    '   !- Schedule Type Limits Name' 
        WRITE(ioout,'(2a)',iostat=ios,err=7) '    Through: 12/31',com
        WRITE(ioout,'(2a)',iostat=ios,err=7) '    For: Weekdays',com
        WRITE(ioout,'(5a)',iostat=ios,err=7) '    Until: ',
     &    '24:00',com,'1.0',com
        WRITE(ioout,'(2a)',iostat=ios,err=7) '    For: Saturday',com
        WRITE(ioout,'(5a)',iostat=ios,err=7) '    Until: ',
     &    '24:00',com,'1.0',com
        WRITE(ioout,'(2a)',iostat=ios,err=7) '    For: Sunday',com
        WRITE(ioout,'(5a)',iostat=ios,err=7) '    Until: ',
     &    '24:00',com,'1.0',com
        WRITE(ioout,'(2a)',iostat=ios,err=7) '    For: AllOtherDays',
     &    com
        WRITE(ioout,'(5a)',iostat=ios,err=7) '    Until: ',
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
          WRITE(ioout,'(2a)',iostat=ios,err=7) '  Lights',com
          WRITE(ioout,'(5a)',iostat=ios,err=7) indent,
     &      zname(mz)(1:lnzname(mz)),'Lights',com,'   !- Name'
          WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &      zname(mz)(1:lnzname(mz)),com,'   !- Zone Name'
          WRITE(ioout,'(6a)',iostat=ios,err=5) '    NOTHING',com,
     &      '   !- SCHEDULE Name'
          totcgnsl=0.0
          WRITE(ioout,'(3a)',iostat=ios,err=7) '    LightingLevel',
     &      com,'   !- Design Level Calculation Method'
          WRITE(ioout,'(a,F12.4,2a)',iostat=ios,err=7) indent,
     &      totcgnsl,com,'   !- Lighting Level {W}'
          WRITE(ioout,'(3a)',iostat=ios,err=7) indent,com,
     &      '   !- Watts per Zone Floor Area {W/m2}'
          WRITE(ioout,'(3a)',iostat=ios,err=7) indent,com,
     &      '   !- Watts per Person {W/person}'
          WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &      '0.0',com,'   !- Return Air Fraction'

          radsplit=0.5
          WRITE(ioout,'(a,F6.4,2a)',iostat=ios,err=5) indent,
     &      radsplit,com,'   !- Fraction Radiant'
          WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &      '0.0',com,'   !- Fraction Visible'

C V3.0 ends with
          WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &      '0.0',sim,'   !- Fraction Replaceable'

C Write out a place-holder small power for the zone.
          WRITE(ioout,'(a)',iostat=ios,err=7) ' '
          WRITE(ioout,'(2a)',iostat=ios,err=7) 'ELECTRIC EQUIPMENT',com
          WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &      zname(mz)(1:lnzname(mz)),com,'   !- Zone Name'
          WRITE(ioout,'(6a)',iostat=ios,err=5) '  NOTHING',com,
     &      '   !- Schedule Name'
          totcgnsl=0.0
          WRITE(ioout,'(a,F12.4,a)',iostat=ios,err=7) indent,
     &      totcgnsl,com
          fraclatent = 0.0
          WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=5) indent,
     &      fraclatent,com,'   !- Fraction latent'
          radsplit=0.5
          WRITE(ioout,'(a,F6.4,2a)',iostat=ios,err=5) indent,
     &      radsplit,com,'   !- Radiant fraction'
          WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &      '0.0',com,'   !- Fraction lost'

C << V2.0 includes General as the End-Use Subcategory
          WRITE(ioout,'(3a)',iostat=ios,err=5) indent,
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

C Scan each days gains and calculate maximum gain for each
C calendar day type.
C First clear loadcasp, looping for each gain type (7 or less)
C and loadonday for day type and gain type.
        do 9 I=1,NBDAYTYPE
          if(I.le.7)loadcasp(mz,i,1)=0.0
          if(I.le.7)loadcasp(mz,i,2)=0.0
          if(I.le.7)loadradfrac(mz,i)=0.0
          do 8 JD=1,7
            loadonday(mz,i,jd)=.false.
  8       continue
  9     continue

C Looping through day types figure out the sensible and latent peak
C values in the current zone.
        do 10 ID=1,NBDAYTYPE
          if(ncas(ID).eq.0)then
            CGNS=0.   ! nothing on this day
            CGNL=0.
          else
            do 87 J=1,NCAS(ID)
              CGNS=0.
              CGNL=0.
              ity=iabs(ICGT(ID,J))  ! remember this gains type

C Detect radiant fraction.
              rfr=RADC(ID,J)
              if(loadradfrac(mz,ity).lt.rfr) loadradfrac(mz,ity)=rfr

              if (ICGT(ID,J).eq.-1) then

C For case of m2/person CMGL(ID,J) remains zero so use cmgs(ID,J) instead.
                if(CMGS(ID,J).lt.0.001)then
                  continue   ! to avoid division by zero.
                else
                  CGNS=real(int((ZBASEA(mz)/CMGS(ID,J))))*95.
                  CGNL=real(int((ZBASEA(mz)/CMGS(ID,J))))*45.
                endif
              elseif (ICGT(ID,J).lt.-1) then
                CGNS=CMGS(ID,J)*ZBASEA(mz)
                CGNL=CMGL(ID,J)*ZBASEA(mz)
              else
                CGNS=CMGS(ID,J)
                CGNL=CMGL(ID,J)
              endif

C If the current periods gain is a new max for the type then update loadcasp.
C If nonzero for the day for this type set loadonday() true.
              if(CGNS.gt.loadcasp(mz,ity,1)) loadcasp(mz,ity,1)=CGNS
              if(CGNL.gt.loadcasp(mz,ity,2)) loadcasp(mz,ity,2)=CGNL
              if(CGNS.gt.0.0.or.CGNL.gt.0.0)then
                loadonday(mz,1,ity)=.true.
              endif
 87         continue
          endif
 10     continue

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
C        write(6,*)' loadfrac 1 2 3 ',loadradfrac(mz,1),
C     &    loadradfrac(mz,2),loadradfrac(mz,3)

C Write out the initial portion of lighting compact schedule.
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  Schedule:Compact',
     &    com
        WRITE(ioout,'(6a)',iostat=ios,err=5) indent,
     &    zname(mz)(1:lnzname(mz)),' ',lodlabel(mz,2),com,
     &    '   !- Name'
        WRITE(ioout,'(3a)',iostat=ios,err=7) '    Fraction',com,
     &    '   !- Schedule Type Limits Name' 
        WRITE(ioout,'(2a)',iostat=ios,err=7) '    Through: 12/31',com

C Go through loops for each day type for lighting gains.
C If there are no day periods or no lights write standard placeholder.
C Also do placeholder of there are no gains of this type on this day.
        do 11 ID=1,NBDAYTYPE
          totcgnsl=loadcasp(mz,2,1)+loadcasp(mz,2,2)
          call eclose(totcgnsl,0.0,0.001,close)
          if(ncas(ID).eq.0.or.close.or.(.NOT.loadonday(mz,ID,2)))then
            WRITE(ioout,'(3a)',iostat=ios,err=7) 
     &        '    For: ',calentag(ID)(1:lnblnk(calentag(ID))),com
            WRITE(ioout,'(5a)',iostat=ios,err=7) '    Until: ',
     &        '24:00',com,'0.0',com
          else

C Write day type name if start of the day type.
            WRITE(ioout,'(3a)',iostat=ios,err=7) 
     &        '    For: ',calentag(ID)(1:lnblnk(calentag(ID))),com
            do 77 J=1,NCAS(ID)
              CGNS=0.
              CGNL=0.
              ity=iabs(ICGT(ID,J))
              totcgnsl=loadcasp(mz,ity,1)+loadcasp(mz,ity,2)
              if(ity.eq.2)then
                if (ICGT(ID,J).lt.-1) then
                  CGNS=CMGS(ID,J)*ZBASEA(mz)
                  CGNL=CMGL(ID,J)*ZBASEA(mz)
                else
                  CGNS=CMGS(ID,J)
                  CGNL=CMGL(ID,J)
                endif
                call eclose(totcgnsl,0.0,0.001,close)
                if(close)then
                  totfrac=0.0
                else
                  totfrac=(CGNS+CGNL)/totcgnsl
                endif
C Debug
C                write(6,*) 'Lights wk CGNS CGNL totcgnsl totfrac',
C     &            CGNS,CGNL,totcgnsl,totfrac

C Write day type name if start of the day type.
                WRITE(ioout,'(a,i2.2,2a,F7.4,a)',iostat=ios,err=7) 
     &            '    Until: ',ICGF(ID,J),':00',com,totfrac,com
              endif
 77         continue
          endif
 11     continue

C Write the AllOtherDays line as well.
        WRITE(ioout,'(2a)',iostat=ios,err=7)
     &    '    For: AllOtherDays',com
        WRITE(ioout,'(5a)',iostat=ios,err=7) '    Until: ',
     &    '24:00',com,'0.0',sim

C Write the light objects for this zone.
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  Lights',com
        WRITE(ioout,'(5a)',iostat=ios,err=7) indent,
     &    zname(mz)(1:lnzname(mz)),'  Lights',com,'   !- Name'
        WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &    zname(mz)(1:lnzname(mz)),com,'   !- Zone Name'
        WRITE(ioout,'(6a)',iostat=ios,err=5) indent,
     &    zname(mz)(1:lnzname(mz)),' ',lodlabel(mz,2),com,
     &    '   !- Schedule Name'
        totcgnsl=loadcasp(mz,2,1)+loadcasp(mz,2,2)
        WRITE(ioout,'(3a)',iostat=ios,err=7) '    LightingLevel',
     &    com,'   !- Design Level Calculation Method'
        WRITE(ioout,'(a,F12.4,2a)',iostat=ios,err=7) '    ',
     &    totcgnsl,com,'   !- Lighting Level {W}'
        WRITE(ioout,'(3a)',iostat=ios,err=7) '    ',com,
     &    '   !- Watts per Zone Floor Area {W/m2}'
        WRITE(ioout,'(3a)',iostat=ios,err=7) '    ',com,
     &    '   !- Watts per Person {W/person}'
        WRITE(ioout,'(4a)',iostat=ios,err=5) '    ',
     &    '0.0',com,'   !- Return Air Fraction'
 
C Set radiant for this gain type based on the max value from earlier
C scan of the operation file data.
C Further logic required if there is more than one radiant/convective split.

C V3.0 ends with
        WRITE(ioout,'(a,F6.4,2a)',iostat=ios,err=5) indent,
     &    loadradfrac(mz,2),com,'   !- Fraction Radiant'
        WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &    '0.0',com,'   !- Fraction visible'
        WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &    '0.0',sim,'   !- Fraction Replaceable'


C Write out the initial portion of small power compact schedule.
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  Schedule:Compact',com
        WRITE(ioout,'(6a)',iostat=ios,err=5) indent,
     &    zname(mz)(1:lnzname(mz)),' ',lodlabel(mz,3),com,'   !- Name'
        WRITE(ioout,'(3a)',iostat=ios,err=7) '    Fraction',com,
     &    '   !- Schedule Type Limits Name' 
        WRITE(ioout,'(2a)',iostat=ios,err=7) '    Through: 12/31',com

C Go through loops for each day type for small power gains.
        do 133 ID=1,NBDAYTYPE
          totcgnsl=loadcasp(mz,3,1)+loadcasp(mz,3,2)
          call eclose(totcgnsl,0.0,0.001,close)
          if(ncas(ID).eq.0.or.close.or.(.NOT.loadonday(mz,1,3)))then
            WRITE(ioout,'(3a)',iostat=ios,err=7) 
     &        '    For: ',calentag(ID)(1:lnblnk(calentag(ID))),com
            WRITE(ioout,'(5a)',iostat=ios,err=7) '    Until: ',
     &        '24:00',com,'0.0',com
          else

C Write day type name if start of the day type.
            WRITE(ioout,'(3a)',iostat=ios,err=7) 
     &        '    For: ',calentag(ID)(1:lnblnk(calentag(ID))),com
            do 67 J=1,NCAS(ID)
              CGNS=0.
              CGNL=0.
              ity=iabs(ICGT(ID,J))
              totcgnsl=loadcasp(mz,ity,1)+loadcasp(mz,ity,2)
              if(ity.eq.3)then
                if (ICGT(ID,J).lt.-1) then
                  CGNS=CMGS(ID,J)*ZBASEA(mz)
                  CGNL=CMGL(ID,J)*ZBASEA(mz)
                else
                  CGNS=CMGS(ID,J)
                  CGNL=CMGL(ID,J)
                endif
                call eclose(totcgnsl,0.0,0.001,close)
                if(close)then
                  totfrac=0.0
                else
                  totfrac=(CGNS+CGNL)/totcgnsl
                endif

C Debug.
C                write(6,*) 'SmP wkd CGNS CGNL totcgnsl totfrac',
C     &            CGNS,CGNL,totcgnsl,totfrac

                WRITE(ioout,'(a,i2.2,2a,F7.4,a)',iostat=ios,err=7) 
     &            '    Until: ',ICGF(ID,J),':00',com,totfrac,com
              endif
 67         continue
          endif
 133    continue

C Write the AllOtherDays line as well.
        WRITE(ioout,'(2a)',iostat=ios,err=7) 
     &    '    For: AllOtherDays',com
        WRITE(ioout,'(5a)',iostat=ios,err=7) '    Until: ',
     &    '24:00',com,'0.0',sim

C Write the electrical equipment objects for this zone, if none
C the only write a brief note and jump to people.
        totcgnsl=loadcasp(mz,3,1)+loadcasp(mz,3,2)
        if(totcgnsl.lt.1.0)then
          WRITE(ioout,'(a)',iostat=ios,err=7) 
     &     '!- no small power in zone - skipping ELECTRIC EQUIPMENT'
          goto 12
        endif
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        WRITE(ioout,'(2a)',iostat=ios,err=7) 'ElectricEquipment',com

        WRITE(ioout,'(5a)',iostat=ios,err=7) indent,
     &    zname(mz)(1:lnzname(mz)),'Equipment',com,'   !- Name'

        WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &    zname(mz)(1:lnzname(mz)),com,'   !- Zone Name'
        WRITE(ioout,'(6a)',iostat=ios,err=5) indent,
     &    zname(mz)(1:lnzname(mz)),' ',lodlabel(mz,3),com,
     &    '   !- Schedule Name'
        WRITE(ioout,'(3a)',iostat=ios,err=7) '    EquipmentLevel',
     &    com,'   !- Design Level Calculation Method'
        totcgnsl=loadcasp(mz,3,1)+loadcasp(mz,3,2)
        WRITE(ioout,'(a,F12.4,a)',iostat=ios,err=7) indent,
     &    totcgnsl,com
        WRITE(ioout,'(3a)',iostat=ios,err=7) '    ',com,
     &    '   !- Watts per Zone Floor Area {W/m2}'
        WRITE(ioout,'(3a)',iostat=ios,err=7) '    ',com,
     &    '   !- Watts per Person {W/person}'

        call eclose(loadcasp(mz,3,2),0.0,0.001,close)
        if(close)then
          fraclatent = 0.0
        else

C << check this out, it might be inverted >>
          fraclatent = (totcgnsl/loadcasp(mz,3,2))*0.01
        endif
        WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=5) indent,
     &    fraclatent,com,'   !- Fraction latent'

C Set radiant from initial scan.
C Further logic required if there is more than one radiant/convective split.
        WRITE(ioout,'(a,F6.4,2a)',iostat=ios,err=5) indent,
     &    loadradfrac(mz,3),com,'   !- Radiant fraction'
        WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &    '0.0',com,'   !- Fraction lost'
        WRITE(ioout,'(3a)',iostat=ios,err=5) indent,
     &    sim,'   !- EndUse Category'


C --------------------------------------

C Write out the initial portion of people compact schedule.
  12    continue
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        WRITE(ioout,'(a)',iostat=ios,err=7) 
     &    '!- occupants schedule/activity level/PEOPLE definition... '
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  Schedule:Compact',com
        WRITE(ioout,'(6a)',iostat=ios,err=5) indent,
     &    zname(mz)(1:lnzname(mz)),' ',lodlabel(mz,1),com,'   !- Name'
        WRITE(ioout,'(3a)',iostat=ios,err=7) '    Fraction',com,
     &    '   !- Schedule Type Limits Name' 
        WRITE(ioout,'(2a)',iostat=ios,err=7) '    Through: 12/31',com

C Go through loops for each day type for occupants.
C If there are no day periods or no people write standard placeholder.
C Also do placeholder of there are no gains of this type on this day.
        do 144 ID=1,NBDAYTYPE
          totcgnsl=loadcasp(mz,1,1)+loadcasp(mz,1,2)
          call eclose(totcgnsl,0.0,0.001,close)
          if(ncas(ID).eq.0.or.close.or.(.NOT.loadonday(mz,1,1)))then
            WRITE(ioout,'(3a)',iostat=ios,err=7) 
     &        '    For: ',calentag(ID)(1:lnblnk(calentag(ID))),com
            WRITE(ioout,'(5a)',iostat=ios,err=7) '    Until: ',
     &        '24:00',com,'0.0',com
          else

C Write day type name if start of the day type.
            WRITE(ioout,'(3a)',iostat=ios,err=7) 
     &        '    For: ',calentag(ID)(1:lnblnk(calentag(ID))),com
            do 57 J=1,NCAS(ID)
              CGNS=0.
              CGNL=0.
              ity=iabs(ICGT(ID,J))
              totcgnsl=loadcasp(mz,ity,1)+loadcasp(mz,ity,2)
              if(ity.eq.1)then
                if (ICGT(ID,J).lt.-1) then
                  CGNS=CMGS(ID,J)*ZBASEA(mz)
                  CGNL=CMGL(ID,J)*ZBASEA(mz)
                else
                  CGNS=CMGS(ID,J)
                  CGNL=CMGL(ID,J)
                endif
                call eclose(totcgnsl,0.0,0.001,close)
                if(close)then
                  totfrac=0.0
                else
                  totfrac=(CGNS+CGNL)/totcgnsl
                endif

C Debug.
C                write(6,*) 'People wkd CGNS CGNL totcgnsl totfrac',
C     &            CGNS,CGNL,totcgnsl,totfrac

                WRITE(ioout,'(a,i2.2,2a,F7.4,a)',iostat=ios,err=7) 
     &            '    Until: ',ICGF(ID,J),':00',com,totfrac,com
              endif
 57         continue
          endif
 144    continue

C Write the AllOtherDays line as well.
        WRITE(ioout,'(2a)',iostat=ios,err=7) 
     &    '    For: AllOtherDays',com
        WRITE(ioout,'(5a)',iostat=ios,err=7) '    Until: ',
     &    '24:00',com,'0.0',sim

C Write out matching activity level schedule for people, but first
C ask the user how many people are equivaent to the peak. If there
C are no occupants then don't bother.
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

C << Currently write the activity schedule as if one period in the day
C << and with the value for oneperson.

        vpeople=totcgnsl/140.0
        CALL EASKR(vpeople,' Number of people in room (see report): ',
     &    ' ',0.0,'F',99.999,'W',1.0,'people in room',IER,nbhelp)
        oneperson = totcgnsl/vpeople
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  Schedule:Compact',com
        WRITE(ioout,'(5a)',iostat=ios,err=5) '    ',
     &    'Activity level ',zname(mz)(1:lnzname(mz)),com,
     &    '   !- Name'
        WRITE(ioout,'(3a)',iostat=ios,err=7) '    Any Number',com,
     &    '   !- Schedule Type Limits Name' 
        WRITE(ioout,'(2a)',iostat=ios,err=7) '    Through: 12/31',com
        WRITE(ioout,'(2a)',iostat=ios,err=7) '    For: Weekdays',com
        WRITE(ioout,'(3a,F7.3,a)',iostat=ios,err=7) '    Until: ',
     &    '24:00',com,oneperson,com
        WRITE(ioout,'(2a)',iostat=ios,err=7) '    For: Saturday',com
        WRITE(ioout,'(3a,F7.3,a)',iostat=ios,err=7) '    Until: ',
     &    '24:00',com,oneperson,com
        WRITE(ioout,'(2a)',iostat=ios,err=7) '    For: Sunday',com
        WRITE(ioout,'(3a,F7.3,a)',iostat=ios,err=7) '    Until: ',
     &    '24:00',com,oneperson,com
        WRITE(ioout,'(2a)',iostat=ios,err=7)'    For: AllOtherDays',
     &    com
        WRITE(ioout,'(3a,F7.3,a)',iostat=ios,err=7) '    Until: ',
     &    '24:00',com,oneperson,sim

C Now write out the people.
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        WRITE(ioout,'(2a)',iostat=ios,err=7) '  People',com

        WRITE(ioout,'(5a)',iostat=ios,err=7) '    ',
     &    zname(mz)(1:lnzname(mz)),'People',com,'   !- Name'

        WRITE(ioout,'(4a)',iostat=ios,err=5) '    ',
     &    zname(mz)(1:lnzname(mz)),com,'   !- Zone Name'
        WRITE(ioout,'(6a)',iostat=ios,err=5) '    ',
     &    zname(mz)(1:lnzname(mz)),' ',lodlabel(mz,1),com,
     &    '   !- Number of people schedule'
        WRITE(ioout,'(3a)',iostat=ios,err=7) '    people',com,
     &    '   !- Number of people calculation method'
        WRITE(ioout,'(a,F7.3,2a)',iostat=ios,err=7) '    ',vpeople,
     &    com,'   !- Number of people'
        WRITE(ioout,'(3a)',iostat=ios,err=7) '    ',com,
     &    '   !- People per Zone Floor Area {person/m2}'
        WRITE(ioout,'(3a)',iostat=ios,err=7) '    ',com,
     &    '   !- Zone Floor Area per person {m2/person}'

C Set radiant as earlier scan
C Further logic required if there is more than one radiant/convective split.
        WRITE(ioout,'(a,F6.4,2a)',iostat=ios,err=5) '    ',
     &    loadradfrac(mz,1),com,'   !- Fraction Radiant'
        WRITE(ioout,'(3a)',iostat=ios,err=5) '    Autocalculate',com,
     &    '  !- Sensible Heat Fraction'
        WRITE(ioout,'(5a)',iostat=ios,err=5) '    ',
     &    'Activity level ',zname(mz)(1:lnzname(mz)),com,
     &    '   !- Activity level schedule name (W/person)'
        WRITE(ioout,'(3a)',iostat=ios,err=7) '    ',com,
     &    '   !- Enable ASHRAE 55 Comfort Warnings'
        WRITE(ioout,'(3a)',iostat=ios,err=5) '    ZoneAveraged',com,
     &    '  !- MRT Calculation Type'
        WRITE(ioout,'(3a)',iostat=ios,err=5) indent,com,
     &    '  !- Surface Name/Angle Factor List Name'
        WRITE(ioout,'(3a)',iostat=ios,err=5) indent,com,
     &    '  !- Work Efficiency SCHEDULE Name (0.0-1.0,real)'
        WRITE(ioout,'(3a)',iostat=ios,err=5) indent,com,
     &    '  !- Clothing Insulation SCHEDULE Name (real)'
        WRITE(ioout,'(3a)',iostat=ios,err=5) indent,com,
     &    '  !- Air Velocity SCHEDULE Name (units m/s, real)'
        WRITE(ioout,'(3a)',iostat=ios,err=5) indent,sim,
     &    '  !- Thermal Comfort Report Type R1'

C Include the sensible fraction of the total.  If using this put it just
C after the Fraction Radiant line in place of auto.
C        totcgnsl=loadcasp(mz,1,1)+loadcasp(mz,1,2)
C        sensfrac=1.0
C        if(totcgnsl.gt.0.1)sensfrac=loadcasp(mz,1,1)/totcgnsl
C        WRITE(ioout,'(a,F6.4,2a)',iostat=ios,err=5) '    ',
C     &    sensfrac,sim,'   !- user specified sensible fraction'

 13   continue

C << More schedules here >>

C Now for each zone, write out the surfaces.
C Setup standard assumptions.
      WRITE(ioout,'(a)',iostat=ios,err=6) ' '
      WRITE(ioout,'(a)',iostat=ios,err=6) '!- ============='
      WRITE(ioout,'(8a)',iostat=ios,err=6) '  GlobalGeometryRules',
     &  com,'LowerLeftCorner',com,
     &  'Counterclockwise',com,'WorldCoordinateSystem',sim
      WRITE(ioout,'(a)',iostat=ios,err=6) '!- ============='
      IFIL=11

C Assume configuration file is from IFIL+5, any leakage description
C is fom IFIL+6, revised config file on IFIL+3, geometry on
C IFILE+2 and ASCII viewing and geometry file reading on IFILE+1. 
      ITA1 = IFIL+6
      INPIC=NCOMP
      do 14 mz=1,inpic

C Loop through the zones, read in each zone geometry.
        WRITE(ioout,'(a)',iostat=ios,err=5) ' '
        WRITE(ioout,'(2a)',iostat=ios,err=5) '! ',
     &    zdesc(mz)(1:lnzdesc(mz))
        WRITE(ioout,'(a)',iostat=ios,err=5) ' '
        WRITE(ioout,'(2a)',iostat=ios,err=5) '  Zone',com
        WRITE(ioout,'(4a)',iostat=ios,err=5) '    ',
     &    zname(mz)(1:lnzname(mz)),com,'   !- Name'
        WRITE(ioout,'(4a)',iostat=ios,err=5) '    ','0.0',com,
     &    '   !- Direction of Relative North {deg}'
        WRITE(ioout,'(8a)',iostat=ios,err=5) '    ','0.0',com,'0.0',
     &    com,'0.0',com,'   !- Origin {m}'
        WRITE(ioout,'(6a)',iostat=ios,err=5) '    ','1',com,'1',com,
     &    '   !- Type and Muitiplier'
        WRITE(outs,'(a,a)',iostat=ios,err=5)' Scanning : ',LGEOM(mz)
        CALL edisp(iuout,outs)
        call eclose(gversion(mz),1.1,0.01,newgeo)
        call georead(ITA1,LGEOM(mz),mz,1,IUOUT,IER)

C Loop through all vertices and find the top and bottom of the zone.
        ZMX=-1.E+7
        ZMN=1.E+7
        do 40 i = 1,NTV
          ZMN=AMIN1(ZMN,Z(I))
          ZMX=AMAX1(ZMX,Z(I))
  40    continue
        WRITE(ioout,'(a,f6.3,2a)',iostat=ios,err=5) '    ',ZMX-ZMN,com,
     &    '   !- Ceiling Height {m}'
        WRITE(ioout,'(a,f9.3,2a)',iostat=ios,err=5) '    ',VOL(mz),sim,
     &    '   !- Volume {m3}'

C Test edge adjacencies and subsurfaces.
        act = 'c'
        call suredgeadj(0,act,mz,ier)

C Loop through the surfaces to see which can be reliably passed to E+
C e.g. identify which surfaces are recognized as child surfaces by E+
C and exclude child surfaces which themselves have children (but include
C the grandchild surface).

C Begin by clearing the assumptions.
        DO 164 IS=1,NSUR
          icc=izstocn(mz,is)
          usethisone(icc)=.true.  ! initial assumption to use
          truechild(icc)=.true.
 164    continue

C Next pass is to identify surfaces to ignore (e.g. if grandchildren exist).
        DO 165 IS=1,NSUR
          icc=izstocn(mz,is)
          if(nbgchild(icc).gt.0.and.igchild(icc,1).gt.0)then
            usethisone(ichild(icc,1))=.false.  ! use only granchchild
          endif
 165    continue

C Next pass to identify whether a surface is a true child in E+ context.
        DO 166 IS=1,NSUR
          icc=izstocn(mz,is)

C See if this surface happens to be a child in the strict sense.
          mentioned=0
          do 778 iprr=1,NZNVER(icc)   ! loop to see only parent mentioned
            if(iedgshr(icc,iprr).eq.0)then
              truechild(icc)=.false.  ! an edge to non-parallel surface
            endif
            if(iedgshr(icc,iprr).ne.0.and.mentioned.eq.0)then
              mentioned=iedgshr(icc,iprr)
            elseif(iedgshr(icc,iprr).ne.0.and.mentioned.ne.0)then
              if(iedgshr(icc,iprr).ne.mentioned)then
                truechild(icc)=.false.  ! edge to more than one
              endif
            endif
 778      continue

C Debug.
C          write(6,*) 'self truechild is ',truechild(icc)
 166    continue

        DO 64 IS=1,NSUR
          icc=izstocn(mz,is)

C Info about children of this surface.
C Debug.
C          write(6,'(3a,i2,a,i3,a,i2,a,i3)')  ' for surface named ',
C     &      SSNAME(icc),' nb children ',nbchild(icc),
C     &      ' cnn is ',ichild(icc,1),' nb gc ',nbgchild(icc),
C     &      ' cnn is ',igchild(icc,1)
          if(ichild(icc,1).gt.0) then
C            write(6,*) ' child named ',SSNAME(ichild(icc,1))
          else
            usethisone(icc)=.true.  ! no children so use
          endif

C Debug.
C          write(6,*) ' edge dup ',nbedgdup(icc)
C          write(6,*) ' edge share ',nbedgshr(icc)
C          write(6,'(a,10i4)') 'p share ',iedgshr(icc,1),iedgshr(icc,2),
C     &    iedgshr(icc,3),iedgshr(icc,4),iedgshr(icc,5),iedgshr(icc,6),
C     &    iedgshr(icc,7),iedgshr(icc,8),iedgshr(icc,9),iedgshr(icc,10)

  64    continue

C For each surface determine appropriate coordinates prior to
C writing out any data.
        DO 60 IS=1,NSUR
          icc=izstocn(mz,is)
          if(.NOT.usethisone(icc))goto 60  ! jump past this

          findchildvert=.false.  ! do not bother remembering
          if(nbchild(icc).eq.0)then

C If there are no children remember all coordinates.
            do 62 iedg=1,NVER(IS)
              COORD(iedg,1)=X(JVN(IS,iedg))
              COORD(iedg,2)=Y(JVN(IS,iedg))
              COORD(iedg,3)=Z(JVN(IS,iedg))
  62        continue
            iedgcnt=NVER(IS)
            findchildvert=.false.  ! child verts not relevant

          elseif(nbgchild(icc).gt.0)then

C There one or more grand children of this surface. Warn
C the user.
            if(ichild(icc,1).gt.0.and.igchild(icc,1).gt.0)then
              write(outs,*) 'Surface ',SSNAME(icc),
     &          ' has a child named ',SSNAME(ichild(icc,1)),
     &          ' and a gchild named ',SSNAME(igchild(icc,1)),
     &          ' E+ does not do gc.'
              call edisp(iuout,outs)

C ignore child vert + dup vert
              findchildvert=.true.  ! do remember child verts
            endif

          elseif(nbchild(icc).gt.0)then
          
C There one or more children of this surface. If the child surface
C is a true child then ignore child vert + dup vert.
            do 63 ich=1,nbchild(icc)
              if(truechild(ichild(icc,1)))then
                findchildvert=.true.  ! do remember child verts
              else
                do 69 iedg=1,NVER(IS)
                  COORD(iedg,1)=X(JVN(IS,iedg))
                  COORD(iedg,2)=Y(JVN(IS,iedg))
                  COORD(iedg,3)=Z(JVN(IS,iedg))
  69            continue
                iedgcnt=NVER(IS)
                findchildvert=.false. ! use all verts
              endif
   63       continue
          endif

C If we have been asked to exclude the child vertices and dup vertices
C then make up the list now.
          if(findchildvert)then
            iccc=ichild(icc,1) ! connection for child
            do 774 iprr=1,MV   ! loop to identify child verts
              if(iprr.le.NZNVER(iccc))then
                jvn1(iprr)=NZJVN(iccc,iprr)
              else
                jvn1(iprr)=0
              endif
  774       continue
            iedgcnt=0
            foundone=0                         ! where the logic beings
            do 748 iedg=1,NZNVER(icc)          ! loop this surface edges
              do 749 ichildloop=1,NZNVER(iccc) ! loop child surface edges
                if(jvn1(ichildloop).eq.jvn(is,iedg))then
                  foundone=jvn1(ichildloop)    ! found shared vertex
                endif
  749         continue
              if(iedg.gt.1)then               ! also check for duplicate vertex
                do 750 iduploop=1,iedg-1
                  if(JVN(IS,iedg).eq.JVN(IS,iduploop))then
                    foundone=JVN(IS,iduploop)   ! found duplicate vertex
                  endif
  750           continue
              endif
              if(foundone.eq.0)then  ! no child match or duplicate so include
                COORD(iedg,1)=X(JVN(IS,iedg))
                COORD(iedg,2)=Y(JVN(IS,iedg))
                COORD(iedg,3)=Z(JVN(IS,iedg))
                iedgcnt=iedgcnt+1
              endif
  748       continue
          endif

C Debug.
C          do 747 iedg=1,iedgcnt
C            write(6,*) COORD(iedg,1),COORD(iedg,2),COORD(iedg,3)
C  747     continue

C A constant boundary condition is defined with an OtherSideCoefficients
C entity. This should be uniquely named e.g. for the zone and surface.
C Generate this entity.
          conststring=' '   ! initial state
          if(ICT(icc).eq.2)then
            WRITE(ioout,'(a)',iostat=ios,err=5) ' '
            call zsid(mz,is,ZSDES,ZSDESC,ZSDESS)
            WRITE(ioout,'(2a)',iostat=ios,err=6) 
     &        '  SurfaceProperty:OtherSideCoefficients',com
            WRITE(ioout,'(5a)',iostat=ios,err=6) indent,'Const', 
     &        ZSDES(1:lnblnk(ZSDES)),com,'   !- OtherSideCoeff Name'
            WRITE(conststring,'(2a)') 'Const',ZSDES(1:lnblnk(ZSDES))
            WRITE(ioout,'(4a)') indent,'0.0000',com,'   !- trigger'
            WRITE(ioout,'(4a)') indent,'0.0000',com,'   !- OSC temp'
            WRITE(ioout,'(4a)') indent,'0.0000',com,
     &        '   !- OSC Temp Coef'
            WRITE(ioout,'(a,f6.2,2a)') indent,real(IC2(icc)),com,
     &        '   !- OSC outside Dry-Bulb'
            WRITE(ioout,'(4a)') indent,'0.0000',com,
     &        '   !- OSC GrndTemp'
            WRITE(ioout,'(4a)') indent,'0.0000',com,
     &        '   !- OSC WindSpdCoef'
            WRITE(ioout,'(4a)') indent,'0.0000',sim,
     &        '   !- OSC ZoneAirTemp'
          endif

C If a surface is transparent, it must have a non-blank parent
C and it must be a true child surface, if not ask the user.
          if(SSOTF(icc)(1:4).NE.'OPAQ'.and.
     &       SSOTF(icc)(1:4).NE.'CFC ')then
            if(truechild(icc))then
              continue    ! surface is a true child so process
            else
              SSPARENT(icc)=' '
              write(outs,'(3a,i2,a)') 'Skipping surface ',
     &          ZSDES(1:lnblnk(ZSDES)),
     &          ' is transparent and is NOT in a child location.'
              WRITE(ioout,'(a)') ' '
              WRITE(ioout,'(2a)',iostat=ios,err=6) 
     &          '!- ',outs(1:lnblnk(outs))
              goto 60   ! jump to next surface
            endif

C If ESP-r thinks there is no parent confirm with user.
            if(SSPARENT(icc)(1:2).eq.'  '.or.
     &         SSPARENT(icc)(1:2).eq.'- ')then
              call zsid(mz,is,ZSDES,ZSDESC,ZSDESS)
              write(outs,'(3a,i2,a)')'Surface ',ZSDES(1:lnblnk(ZSDES)),
     &          ' is transparent without parent surface.'
              call edisp(iuout,outs)
              dok=.false.
              izz=ic1(icc)
              write(outs,'(5a)') 'Does ',ssname(icc),
     &          ' have a parent surface in ',zname(izz),'?'
              call askok(outs,
     &          ' (saying no skips this subsurface)',ok,dok,nbhelp)
              if(ok)then
                write(outs,'(5a)') 'Specify parent surface for ',
     &            ssname(icc),' in ',zname(izz),'.'
                call easksur(izz,iss,'-',outs,' ',ier)
                ibinval=izstocn(izz,iss)
                if(ibinval.ne.0)then
                  SSPARENT(icc)=ssname(ibinval)
                else
                  SSPARENT(icc)=' '
                  write(outs,'(3a,i2,a)') 'Skipping surface ',
     &              ZSDES(1:lnblnk(ZSDES)),
     &              ' which is transparent WITHOUT a parent.'
                  WRITE(ioout,'(a)') ' '
                  WRITE(ioout,'(2a)',iostat=ios,err=6) 
     &              '!- ',outs(1:lnblnk(outs))
                  goto 60  ! jump to next surface
                endif
              else
                SSPARENT(icc)=' '
                write(outs,'(3a,i2,a)') 'Skipping surface ',
     &            ZSDES(1:lnblnk(ZSDES)),
     &            ' which is transparent WITHOUT a parent.'
                WRITE(ioout,'(a)') ' '
                WRITE(ioout,'(2a)',iostat=ios,err=6) 
     &            '!- ',outs(1:lnblnk(outs))
                goto 60   ! jump to next surface
              endif
            endif
          endif

C The initial tag is dependant on whether the surface is
C opaque or transparent and whether it is adiabetic or similar.
          treatasmass=.false.  ! initial assumption
          iccsimilar=0         ! connection in case of similar

C  << this logic to be updated >>
          WRITE(ioout,'(a)') ' '

          if(SSOTF(icc)(1:4).NE.'OPAQ'.AND.
     &      SSOTF(icc)(1:4).NE.'CFC ')then

C A transparent surface.
            WRITE(ioout,'(2a)',iostat=ios,err=6) 
     &        '  FenestrationSurface:Detailed',com
          else

C Opaque surface.
            if(ICT(icc).eq.7)then
              treatasmass=.true.  ! CEN similar is internal mass

            elseif(ICT(icc).eq.1)then

C If similar check if there is an opposing surface.
              if(SSVFC(icc)(1:4).eq.'CEIL')then
                if(izsfloor(mz).ne.0)then
                  iccsimilar=izstocn(mz,izsfloor(mz)) ! get connection for floor
                  treatasmass=.false.  ! we have an opposing surface to use
                else
                  treatasmass=.true.  ! treat as internal mass
                endif
              elseif(SSVFC(icc)(1:4).eq.'FLOR')then
                if(izsceil(mz).ne.0)then
                  iccsimilar=izstocn(mz,izsceil(mz)) ! get connection for ceiling
                  treatasmass=.false.  ! we have an opposing surface to use
                else
                  treatasmass=.true.  ! treat as internal mass
                endif
              else

C Logic not yet in place to find an opossing partition so point it to the ceiling.
                if(izsceil(mz).ne.0)then
                  iccsimilar=izstocn(mz,izsceil(mz)) ! get connection for ceiling
                  treatasmass=.false.  ! we have an opposing surface to use
                else
                  treatasmass=.true.  ! treat as internal mass
                endif
              endif
            endif

            if(treatasmass)then
              WRITE(ioout,'(2a)',iostat=ios,err=6) 
     &          '  InternalMass',com
            else

C Opaque surface without or with a parent and might not be a true child.
              if(SSPARENT(icc)(1:2).eq.'  '.or.
     &           SSPARENT(icc)(1:2).eq.'- ')then
                WRITE(ioout,'(2a)',iostat=ios,err=6) 
     &            '  BuildingSurface:Detailed',com
              else
                if(truechild(icc))then
                  WRITE(ioout,'(3a)',iostat=ios,err=6) 
     &              '  FenestrationSurface:Detailed',com,
     &              '   !- (a door)'
                else
                  WRITE(ioout,'(2a)',iostat=ios,err=6) 
     &              '  BuildingSurface:Detailed',com
                endif
              endif
            endif
          endif

C Generate a string for use with view factor to the ground.
C Also include viewfactor to ground value depending on orientation
C and site exposure. An index of 8 signals user defined values.
          if(siteexposureindex.ne.8)then
            if(SSVFC(icc)(1:4).eq.'VERT')then
              WRITE(sitestring,'(a,f5.3,2a)',iostat=ios,err=6) '    ',
     &          PROPG(siteexposureindex),com,
     &          '   !- View Factor to Ground'
            elseif(SSVFC(icc)(1:4).eq.'CEIL')then
              WRITE(sitestring,'(4a)',iostat=ios,err=6) indent,'0.0',
     &          com,'   !- View Factor to Ground'
            elseif(SSVFC(icc)(1:4).eq.'SLOP'.or.
     &             SSVFC(icc)(1:4).eq.'UNKN')then
              WRITE(sitestring,'(4a)',iostat=ios,err=6) indent,'0.0',
     &          com,'   !- View Factor to Ground'
            elseif(SSVFC(icc)(1:4).eq.'FLOR')then
              WRITE(sitestring,'(4a)',iostat=ios,err=6) indent,'1.0',
     &          com,'   !- View Factor to Ground'
            endif
          else
            if(SSVFC(icc)(1:4).eq.'VERT')then
              WRITE(sitestring,'(a,f5.3,2a)',iostat=ios,err=6) indent,
     &         groundview,com,'   !- View Factor to Ground'
            elseif(SSVFC(icc)(1:4).eq.'CEIL')then
              WRITE(sitestring,'(4a)',iostat=ios,err=6) indent,'0.0',
     &          com,'   !- View Factor to Ground'
            elseif(SSVFC(icc)(1:4).eq.'SLOP'.or.
     &             SSVFC(icc)(1:4).eq.'UNKN')then
              WRITE(sitestring,'(4a)',iostat=ios,err=6) indent,'0.0',
     &          com,'   !- View Factor to Ground'
            elseif(SSVFC(icc)(1:4).eq.'FLOR')then
              WRITE(sitestring,'(4a)',iostat=ios,err=6) indent,'1.0',
     &          com,'   !- View Factor to Ground'
            endif
          endif      

C Generate a unique name (zone + surface).
          call zsid(mz,is,ZSDES,ZSDESC,ZSDESS)
          WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &      ZSDES(1:lnblnk(ZSDES)),com,'   !- Name'

          if(SSOTF(icc)(1:4).NE.'OPAQ'.AND.
     &      SSOTF(icc)(1:4).NE.'CFC ')then

C We have a transparent surface.
            WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'Window',com,
     &        '   !- Surface Type!'
            WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &        SSMLCN(icc)(1:lnblnk(SSMLCN(icc))),com,
     &        '   !- Construction Name'

C If this surface has a grandparent then its parent (the frame) will
C have been skipped so use the name of the grand parent rather than
C the name of the ESP-r parent surface.
            if(igparent(icc).gt.0)then
              lsn0=lnblnk(SSNAME(igparent(icc)))
              lzn0=lnzname(mz)
              WRITE(ZSDES,'(a,a1,a)',iostat=ios,err=6) 
     &          SSPARENT(icc)(1:lsn0),':',zname(mz)(1:lzn0)
            else
              lsn0=lnblnk(SSPARENT(icc))
              lzn0=lnzname(mz)
              WRITE(ZSDES,'(a,a1,a)',iostat=ios,err=6) 
     &          SSPARENT(icc)(1:lsn0),':',zname(mz)(1:lzn0)
            endif
            WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &        ZSDES(1:lnblnk(ZSDES)),com,'   !- Building Surface Name'
            WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &        '   !- Outside Boundary Condition Object'

C Also include viewfactor to ground value depending on orientation.
            WRITE(ioout,'(a)',iostat=ios,err=6) 
     &        sitestring(1:lnblnk(sitestring))
            WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &        '   !-   Shading Control Name'
            WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &        '   !-   Frame and Divider Name'
            WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'1.0',com,
     &        '   !-   Multiplier'
          else

C We have an opaque surface. Depending on connection type generate an
C abstract or an explicit surface.
            if(treatasmass)then

C CEN or a Dynamic Similar connection that does not have an opposite.
              WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &          SSMLCN(icc)(1:lnblnk(SSMLCN(icc))),com,
     &          '   !- Construction Name'
              WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &          zname(mz)(1:lnzname(mz)),
     &          com,'   !- Zone Name'
              WRITE(ioout,'(a,f8.4,2a)',iostat=ios,err=6) indent,
     &          SSNA(icc),sim,'   !-  Surface Area {m2}'
              goto 60   ! jump to next surface
            else

C Check if parent surface name is a blank or just a dash. Which says
C that this surface has no parent (ESP-r geometric rules as well
C as E+ rules).
              processasfen=.false.
              if(SSPARENT(icc)(1:2).eq.'  '.or.
     &           SSPARENT(icc)(1:2).eq.'- ')then
                processasfen=.false.
              else

C ESP-r thinks there is a parent so double check whether E+ rules
C say this surface is a true child.
                if(truechild(icc))then
                  processasfen=.true.
                else
                  processasfen=.false.
                endif
              endif
                
              if(.NOT.processasfen)then

C Depending on the surface orientation write out Wall Roof Floor tag.
                if(SSVFC(icc)(1:4).eq.'VERT')then
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'Wall',
     &              com,'   !- Surface Type'
                elseif(SSVFC(icc)(1:4).eq.'SLOP'.or.
     &                 SSVFC(icc)(1:4).eq.'UNKN')then
                  if(ICT(icc).eq.0.or.ICT(icc).eq.4.or.
     &               ICT(icc).eq.6)then
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                'Roof',com,'   !- Surface Type'
                  else
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                'Wall',com,'   !- Surface Type'
                  endif
                elseif(SSVFC(icc)(1:4).eq.'CEIL')then
                  if(ICT(icc).eq.0.or.ICT(icc).eq.4.or.
     &               ICT(icc).eq.6)then
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                'Roof',com,'   !- Surface Type'
                  else
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                'Ceiling',com,'   !- Surface Type'
                  endif
                elseif(SSVFC(icc)(1:4).eq.'FLOR')then
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &              'Floor',com,'   !- Surface Type'
                endif

                WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &            SSMLCN(icc)(1:lnblnk(SSMLCN(icc))),com,
     &            '   !- Construction Name'
                WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &            zname(mz)(1:lnzname(mz)),
     &            com,'   !- Zone Name'

C Other face environment.
                if(ICT(icc).eq.0)then
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &              'Outdoors',
     &              com,'   !-  Outside Boundary Condition'
                  WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &              '   !- Outside Boundary Condition Object'
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &              'SunExposed',com,'   !- Sun Exposure'
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &              'WindExposed',com,'   !- Wind Exposure'

C Also include viewfactor to ground value depending on orientation.
                  WRITE(ioout,'(a)',iostat=ios,err=6) 
     &              sitestring(1:lnblnk(sitestring))

                elseif(ICT(icc).eq.1)then

                  if(treatasmass)then
                    continue  ! this has been considered internal mass
                  else

C Another surface in this room (to represent similar). If the current
C construction is the same as that used by the opposite surface then
C this will work, otherwise we are going to have to point back to the
C original surface.
 
C << for later actions, keep a list of the symmetric tag for
C << each MLC. This might save some searching...

                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                'Surface',com,'   !-  Outside Boundary (similar)'
                    if(iccsimilar.ne.0)then
                      if(SSMLCN(icc)(1:8).eq.
     &                   SSMLCN(iccsimilar)(1:8))then
                        call zsid(ic1(iccsimilar),ie1(iccsimilar),ZSDES,
     &                    ZSDESC,ZSDESS)
                        WRITE(ioout,'(4a)',iostat=ios,err=6) '    ',
     &                    ZSDES(1:lnblnk(ZSDES)),com,
     &                    '   !- Outside Boundary Condition other surf'
                      else  ! pont back at itself
                        call zsid(ic1(icc),ie1(icc),ZSDES,ZSDESC,ZSDESS)
                        WRITE(ioout,'(4a)',iostat=ios,err=6) '    ',
     &                    ZSDES(1:lnblnk(ZSDES)),com,
     &                    '   !- Outside Boundary Condition to self'
                      endif
                    else
                      ZSDES='UNKNOWN:UNKNOWN'
                      WRITE(ioout,'(4a)',iostat=ios,err=6) '    ',
     &                  ZSDES(1:lnblnk(ZSDES)),com,
     &                  '   !- Outside Boundary Condition unresolved'
                    endif
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                'NoSun',com,'   !- Sun Exposure'
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                'NoWind',com,'   !- Wind Exposure'
                    WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &                '   !- View Factor to Ground'
                  endif
                elseif(ICT(icc).eq.2)then

C A constant connection makes reference to conststring.
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &              'OtherSideCoefficients',com,
     &              '   !-  OutsideFaceEnvironment (constant)'
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent, 
     &              conststring(1:lnblnk(conststring)),com,
     &              '   !-  OutsideFaceEnvironment Object'
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &              'NoSun',com,'   !- Sun Exposure'
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &              'NoWind',com,'   !- Wind Exposure'
                  WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &              '   !- View Factor to Ground'
                elseif(ICT(icc).eq.3)then

C A partition to another room.
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &              'Surface',com,'   !-  Outside Boundary Condition'
                  call zsid(ic2(icc),ie2(icc),ZSDES,ZSDESC,ZSDESS)
                  WRITE(ioout,'(4a)',iostat=ios,err=6) '    ',
     &              ZSDES(1:lnblnk(ZSDES)),com,
     &              '   !- Outside Boundary Condition Object'
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &              'NoSun',com,'   !- Sun Exposure'
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &              'NoWind',com,'   !- Wind Exposure'
                  WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &              '   !- View Factor to Ground'
                elseif(ICT(icc).eq.4.or.ICT(icc).eq.6)then

C A ground connection.
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &              'Ground',com,'   !-  Outside Boundary Condition'
                  WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &              '   !- Outside Boundary Condition Object'
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'NoSun',
     &              com,'   !- Sun Exposure'
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &              'NoWind',com,'   !- Wind Exposure'
                  WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &              '   !- View Factor to Ground'
                elseif(ICT(icc).eq.5)then

C Adiabatic in E+ points back to itself.
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &              'Surface',com,'   !-  Outside Boundary (Adiabatic)'
                  WRITE(ioout,'(4a)',iostat=ios,err=6) '    ',
     &              ZSDES(1:lnblnk(ZSDES)),com,
     &              '   !- Outside Boundary Condition to self'
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &              'NoSun',com,'   !- Sun Exposure'
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &              'NoWind',com,'   !- Wind Exposure'
                  WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &              '   !- View Factor to Ground'
                elseif(ICT(icc).eq.7)then
                  continue  ! a CEN connection is internal mass
                endif
              else

C This surface should be processed as fenistration.
                WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'Door',
     &            com,'   !- Surface Type!'
                WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &            SSMLCN(icc)(1:lnblnk(SSMLCN(icc))),com,
     &            '   !- Construction Name'
                lsn0=lnblnk(SSPARENT(icc))
                lzn0=lnzname(mz)
                WRITE(ZSDES,'(a,a1,a)',iostat=ios,err=6) 
     &            SSPARENT(icc)(1:lsn0),':',zname(mz)(1:lzn0)
                WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &            ZSDES(1:lnblnk(ZSDES)),com,
     &            '   !- Building Surface Name'

C If external environment (external door) then OutsideFaceEnvironment Object must remain blank
C Otherwise give name of the other side surface. (suggested by Georgios)
                if(ICT(icc).eq.0)then
                  WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &              '   !- Outside Boundary Condition Object'
                else
                  if(ic2(icc).eq.0.or.ie2(icc).eq.0)then  ! if unknown zone & surface
                    WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &                '   !- Outside Boundary Condition Object'
                  else  ! if known zone & surface
                    call zsid(ic2(icc),ie2(icc),ZSDES,ZSDESC,ZSDESS)
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                ZSDES(1:lnblnk(ZSDES)),com,
     &                '   !- Outside Boundary Condition Object'
                  endif
                endif 
                WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &            '   !- View Factor to Ground'
                WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &            '   !-   Shading Control Name'
                WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &            '   !-   Frame and Divider Name'
                WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'1.0',
     &            com,'   !-   Multiplier'
              endif ! of test for fenistration or not
            endif   ! of test for CEN Dynamic or other type of connection
          endif     ! of test for opaque or transparent

C Vertex information. The logic above will have sorted out the number
C of edges to write.
          nvertex=iedgcnt
          WRITE(ioout,'(a,i2,2a)',iostat=ios,err=6) indent,nvertex,
     &      com,'   !- Number of Vertices'
          do 42 iss=1,nvertex
            if(iss.lt.nvertex)then
              WRITE(ioout,'(f10.4,a,f10.4,a,f10.4,2a)',
     &          iostat=ios,err=6) COORD(iss,1),com,
     &          COORD(iss,2),com,COORD(iss,3),
     &          com,'   !- X Y Z [m]'
            else
              WRITE(ioout,'(f10.4,a,f10.4,a,f10.4,2a)',
     &          iostat=ios,err=6) COORD(iss,1),com,
     &          COORD(iss,2),com,COORD(iss,3),
     &          sim,'   !- X Y Z [m]'
            endif
  42      continue
  60    continue   ! end of surface loop

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
              WRITE(ioout,'(2a,i1,2a)',iostat=ios,err=6) indent,
     &          BLOCKNAME(mz,ib)(1:LNBLOCKNAME(mz,ib)),ibs,com,
     &          '   !- User Supplied Surface Name'
              WRITE(ioout,'(5a)',iostat=ios,err=6) indent,com,
     &          '   !- TransSchedShadowSurf'
              nvertex=4
              WRITE(ioout,'(a,i1,2a)',iostat=ios,err=6) indent,
     &          nvertex,com,'   !- Number of Vertices'
              do 44 iss=1,nvertex
                if(iss.lt.nvertex)then
                  WRITE(ioout,'(a,f10.4,a,f10.4,a,f10.4,2a)',
     &              iostat=ios,err=6) indent,XB(JVNB(ibs,iss)),com,
     &              YB(JVNB(ibs,iss)),com,ZB(JVNB(ibs,iss)),com,
     &              '   !- X Y Z [m]'
                else
                  WRITE(ioout,'(a,f10.4,a,f10.4,a,f10.4,2a)',
     &              iostat=ios,err=6) indent,XB(JVNB(ibs,iss)),com,
     &              YB(JVNB(ibs,iss)),com,ZB(JVNB(ibs,iss)),sim,
     &            '   !- X Y Z [m]'
                endif
  44          continue
 302        continue
 301      continue
        endif
  14  continue  ! end of zone loop
      call edisp(iuout,'Completed export of data to EnergyPlus.')

C Reports - report details and dxf for surfaces
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      WRITE(ioout,'(2a)',iostat=ios,err=2) '  Output:Reports',com
      WRITE(ioout,'(8a)',iostat=ios,err=2) indent,'Surfaces',com,
     &  'Details',com,'ThickPolyline',sim,
     &  '   !- Type, Name and Specifications for reports'
     
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      WRITE(ioout,'(2a)',iostat=ios,err=2) '  Output:Reports',com
      WRITE(ioout,'(8a)',iostat=ios,err=2) indent,'Surfaces',com,
     &  'DXF',com,'ThickPolyline',sim,
     &  '   !- Type, Name and Specifications for reports'

C Include directive for EnergyPlus to dump out additional diagnotics
C for geometry. 
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      WRITE(ioout,'(5a)',iostat=ios,err=2) '  Output:Diagnostics',
     &  com,'DisplayExtraWarnings',sim,'   !- Extra diagnostics'

C Include minimal meters: ambient db and zone air temperatures.
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      WRITE(ioout,'(8a)',iostat=ios,err=2) '  Output:Variable',com,
     &  '*',com,'outdoor dry bulb',com,'hourly',sim
      WRITE(ioout,'(8a)',iostat=ios,err=2) '  Output:Variable',com,
     &  '*',com,'Zone Mean Air Temperature',com,'hourly',sim

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

