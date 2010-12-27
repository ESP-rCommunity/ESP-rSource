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

C geometry.h provides commons G0/G1/G2/G4/G5/G7/prec2/prec17/precz/GS5/GS6.
#include "geometry.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure database)
C LOPTDB,IOPTDB (for optical database)
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
C ICLIM,LCLIM (for climate file)
#include "material.h"
      
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

      integer legmat  ! local variable for legacy index.
      integer matarrayindex ! the indes within matdatarray
      logical closemat1,closemat2

      character GDESCR*36
      CHARACTER NAM*72,outs*124,h*72
      character CLMLOC*30,LCFGF*72
      character cfgroot*24
      character ctldoc*248,LCTLF*72
      character ZSDES*28,ZSDESC*20,ZSDESS*16,mlcname*12
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
      integer iver    ! file version as integer
      integer ier
      character optndx*12,nameiprndx*25,OPT*12,com*1,sim*1
      character act*1,layname*25
      character T25*25,T25a*25,t12a*12
      character*10 wkday(7)
      character*9 RAMONTH(12)
      character*4 indent   ! for indentation

      logical found,close,close1,foundmlc
      logical closea,closeb,closec,closed,closee
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

C Check the version number and set iver as well as a character variable
C to represent a 2 char or 4 char indentation.
      iver = 00
      call eclose(ver,1.2,0.001,closea)
      call eclose(ver,1.3,0.001,closeb)
      call eclose(ver,1.4,0.001,closec)
      call eclose(ver,2.0,0.001,closed)
      call eclose(ver,3.0,0.001,closee)
      if(closea)then
        iver = 12; indent = '  '
      elseif(closeb)then
        iver = 13; indent = '  '
      elseif(closec)then
        iver = 14; indent = '  '
      elseif(closed)then
        iver = 20; indent = '  '
      elseif(closee)then
        iver = 30; indent = '    '
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
      if(iver.eq.14)then
        WRITE(ioout,'(5a)',iostat=ios,err=2) 'VERSION',com,'1.4',sim,
     &  '   !- Version Identifier'
      elseif(iver.eq.20)then
        WRITE(ioout,'(5a)',iostat=ios,err=2) 'VERSION',com,'2.0',sim,
     &  '   !- Version Identifier (for testing)'
      elseif(iver.eq.30)then
        WRITE(ioout,'(5a)',iostat=ios,err=2) '  VERSION',com,'3.0',sim,
     &  '   !- Version Identifier (for testing)'
      endif
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      if(iver.le.20)then
        WRITE(ioout,'(2a)',iostat=ios,err=2) 'BUILDING',com
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,
     &    cfgroot(1:lnblnk(cfgroot)),com,'   !- Building (root) name.'
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'0.000',com,
     &    '   !- North Axis {deg}'
      else
        WRITE(ioout,'(2a)',iostat=ios,err=2) '  Building',com
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,
     &    cfgroot(1:lnblnk(cfgroot)),com,'   !- Name'
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'0.000',com,
     &    '   !- North Axis {deg}'
      endif

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
      if(iver.le.20)then
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'0.04',com,
     &    '   !- Loads Convergence Tolerance Value (frac of load)'
      else
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'0.04',com,
     &    '   !- Loads Convergence Tolerance Value'
      endif
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
      if(iver.le.20)then
        WRITE(ioout,'(2a)',iostat=ios,err=2) 'TIMESTEP IN HOUR',com
      else
        WRITE(ioout,'(2a)',iostat=ios,err=2) '  Timestep',com
      endif
      if(nsset.gt.0)then
        if(isbnstep.gt.6)then
          WRITE(ioout,'(4a,i2,a)',iostat=ios,err=2) indent,'6',sim,
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
      if(iver.le.20)then
        WRITE(ioout,'(2a)',iostat=ios,err=2) 
     &    'INSIDE CONVECTION ALGORITHM',com
      else
        WRITE(ioout,'(2a)',iostat=ios,err=2) 
     &    '  SurfaceConvectionAlgorithm:Inside',com
      endif
      if(havehc)then
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'Detailed',sim,
     &    '   !- [please check equivalent ESP-r HC regime]'
      else
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'Detailed',sim,
     &    '   !- Algorithm (default)'
      endif

      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      if(iver.le.20)then
        WRITE(ioout,'(2a)',iostat=ios,err=2) 
     &    'OUTSIDE CONVECTION ALGORITHM',com
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'Detailed',sim,
     &    '   !- Algorithm (default)'
      else
        WRITE(ioout,'(2a)',iostat=ios,err=2) 
     &    'SurfaceConvectionAlgorithm:Outside',com
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'Detailed',sim,
     &    '   !- Algorithm (default)'
      endif

C Solution - assume no moisture solution.
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      if(iver.le.20)then
        WRITE(ioout,'(2a)',iostat=ios,err=2) 'SOLUTION ALGORITHM',com
        WRITE(ioout,'(4a)',iostat=ios,err=2) indent,'CTF',sim,
     &    '   !- [CTF|MTF|EMPD])'
      elseif(iver.eq.30)then
        WRITE(ioout,'(2a)',iostat=ios,err=2) 
     &    '  HeatBalanceAlgorithm,ConductionTransferFunction',sim
      endif

C Interpret shading calculations. If shading used in ESP-r then
C set to 28 days, otherwise set zero=default.
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      if(iver.le.20)then
        WRITE(ioout,'(2a)',iostat=ios,err=2) 
     &    'SHADOWING CALCULATIONS',com
      else
        WRITE(ioout,'(2a)',iostat=ios,err=2) '  ShadowCalculation',com
      endif
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
      if(iver.le.20)then
        WRITE(ioout,'(2a)',iostat=ios,err=2) 'RunPeriod',com
      else
        WRITE(ioout,'(2a)',iostat=ios,err=2) '  RunPeriod',com
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
      if(iver.le.20)then
        WRITE(ioout,'(2a)',iostat=ios,err=2) 'Location',com
      else
        WRITE(ioout,'(2a)',iostat=ios,err=2) '  Site:Location',com
      endif
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
        WRITE(ioout,'(a,i3,2a)',iostat=ios,err=2) indent,iic2,com,
     &    '   !- TimeZone {hr} [supplied by user]'
      endif

      h(1)='EnergyPlus needs to know the site elevation in metres'
      h(2)='above sea level. '
      SLM=10.0
      CALL EASKR(SLM,' Site elevation above sea level (m) ',
     &  ' ',-100.0,'F',10000.0,'F',10.0,'site elevation',IER,2)
      WRITE(ioout,'(a,f7.1,2a)',iostat=ios,err=2) indent,SLM,sim,
     &  '   !- Elevation {m} [supplied by user]'

C Interpret ground temperatures. Loop through each connection
C and check which standard or user defined ground temperature
C profile is being used. EnergyPlus can only take one so if
C there is more than one referenced warn the user. There is
C no equivalent to NRCan BASESIMP but there is a ground temperature
C calculation module in EnergyPlus.
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      if(iver.le.20)then
        WRITE(ioout,'(2a)',iostat=ios,err=2) 'GROUNDTEMPERATURES',com
      else
        WRITE(ioout,'(2a)',iostat=ios,err=2) 
     &    '  Site:GroundTemperature:BuildingSurface',com
      endif
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
        mlcname=DESC(ino)(1:12)
        call mlcrefs(mlcname,areamlc,tareamlc,foundmlc)
        if(.NOT.foundmlc) goto 20

C If the construction is transparent then the materials need
C to be marked as WindowGlass. Curently ESP-r holds optics for
C the overall construction and for single glazing this will the
C the same for EnergyPlus. For multiple layers the data is not
C quite correct. Assume that visible reflection is based on
C visible trans and solar absorb.

C << to be done - use if IPRMAT >>

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

      do 25 iy = 1,nmat
        if(iprndx(iy).gt.0)then
          WRITE(ioout,'(a)',iostat=ios,err=3) ' '
          if(iglassndx(iy).eq.0)then
            if(iver.le.20)then
              WRITE(ioout,'(2a)',iostat=ios,err=3) 'MATERIAL:REGULAR',
     &          com
            else
              WRITE(ioout,'(2a)',iostat=ios,err=3) '  Material',com
            endif
          else
            if(iver.le.20)then
              WRITE(ioout,'(2a)',iostat=ios,err=3) 
     &          'MATERIAL:WINDOWGLASS',com
            else
              WRITE(ioout,'(2a)',iostat=ios,err=3) 
     &          '  WindowMaterial:Glazing',com
            endif
          endif
          legmat=iprndx(iy)   ! which legacy index
          if(legmat.gt.0)then
            matarrayindex=mathash(legmat)  ! find matching array index
 
C And if matarrayindex is zero then resetn dbcon dbden dbsht.
            if(matarrayindex.eq.0)then
              DBCON=0.0; DBDEN=0.0; DBSHT=0.0 
              E=0.99; A=0.99
              NAM='AIR'
            else
              DBCON=matdbcon(matarrayindex)
              DBDEN=matdbden(matarrayindex)
              DBSHT=matdbsht(matarrayindex)
              E=matdbine(matarrayindex)
              A=matdbina(matarrayindex)
              write(NAM,'(a)') matname(matarrayindex)(1:32)
            endif
          endif

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
            if(iver.le.20)then
              WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,E,com,
     &          '   !- Absorptance:Thermal'
              WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,A,com,
     &          '   !- Absorptance:Solar'
              WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,A,sim,
     &          '   !- Absorptance:Visible'
            else
              WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,E,com,
     &          '   !- Thermal Absorptance'
              WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,A,com,
     &          '   !- Solar Absorptance'
              WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,A,sim,
     &          '   !-  Visible Absorptance'
            endif
          else
            WRITE(ioout,'(4a)',iostat=ios,err=3) indent,
     &        'SpectralAverage',com,'   !- Optical Data Type'
            if(iver.le.20)then
              WRITE(ioout,'(3a)',iostat=ios,err=3) indent,com,
     &          '   !- Name of Window Glass Spectral Data Set'
            else
              WRITE(ioout,'(3a)',iostat=ios,err=3) indent,com,
     &          '   !- Window Glass Spectral Data Set Name'
            endif
            WRITE(ioout,'(a,f7.4,2a)',iostat=ios,err=3) indent,
     &        thkiprndx(iy),com,'   !- Thickness {m}'
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,
     &        soltrn(iy),com,
     &        '   !- Solar Transmittance at Normal Incidence'
            if(iver.le.20)then
              WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,
     &         solrefl(iy),com,
     &         '   !- Solar Reflectance at Normal Incidence: Front'
            else
              WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,
     &         solrefl(iy),com,
     &         '   !- Front Side Solar Reflectance at Normal Incidence'
            endif
            if(iver.le.20)then
              WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,
     &         solrefl(iy),com,
     &         '   !- Solar Reflectance at Normal Incidence: Back'
            else
              WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,
     &         solrefl(iy),com,
     &         '   !- Back Side Solar Reflectance at Normal Incidence'
            endif
            WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,
     &        vistrn(iy),com,
     &        '   !- Visible Transmittance at Normal Incidence'
            if(iver.le.20)then
              WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,
     &         visrefl(iy),com,
     &         '   !- Visible Reflectance at Normal Incidence: Front'
            else
              WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,
     &         visrefl(iy),com,
     &       '   !- Front Side Visible Reflectance at Normal Incidence'
            endif
            if(iver.le.20)then
              WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,
     &         visrefl(iy),com,
     &         '   !- Visible Reflectance at Normal Incidence: Back'
            else
              WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,
     &         visrefl(iy),com,
     &       '   !- Back Side Visible Reflectance at Normal Incidence'
            endif
            if(iver.le.20)then
              WRITE(ioout,'(4a)',iostat=ios,err=3) indent,'0.00',com,
     &          '   !- IR Transmittance at Normal Incidence'
            else
              WRITE(ioout,'(4a)',iostat=ios,err=3) indent,'0.00',com,
     &          '   !- Infrared Transmittance at Normal Incidence'
            endif
            if(iver.le.20)then
              WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,E,
     &          com,'   !- IR Hemispherical Emissivity: Front'
            else
              WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,E,
     &         com,'   !- Front Side Infrared Hemispherical Emissivity'
            endif
            if(iver.le.20)then
              WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,E,
     &          com,'   !- IR Hemispherical Emissivity: Back'
            else
              WRITE(ioout,'(a,f6.4,2a)',iostat=ios,err=3) indent,E,
     &         com,'   !- Back Side Infrared Hemispherical Emissivity'
            endif
            WRITE(ioout,'(a,f7.4,2a)',iostat=ios,err=3)   indent,DBCON,
     &        sim,'   !- Conductivity {W/m-K}'
          endif
        else

C Make up air name for opaque construction. t12a holds air gap resistance
C as a string w/o leading blanks (use only first 7 char of t12a).
          if(iglassndx(iy).eq.0)then
            WRITE(ioout,'(a)',iostat=ios,err=3) ' '
            if(iver.le.20)then
              WRITE(ioout,'(2a)',iostat=ios,err=3) 'MATERIAL:AIR',com
            else
              WRITE(ioout,'(2a)',iostat=ios,err=3) '  Material:AirGap',
     &          com
            endif
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
            if(iver.le.20)then
              WRITE(ioout,'(2a)',iostat=ios,err=3) 
     &          'MATERIAL:WINDOWGAS',com
            else
              WRITE(ioout,'(2a)',iostat=ios,err=3) 
     &          '  WindowMaterial:Gas',com
            endif
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
        if(iver.le.20)then
          WRITE(ioout,'(2a)',iostat=ios,err=4) 'CONSTRUCTION',com
        else
          WRITE(ioout,'(2a)',iostat=ios,err=4) '  Construction',com
        endif
        WRITE(ioout,'(4a)',iostat=ios,err=4) indent,
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
              WRITE(ioout,'(4a)',iostat=ios,err=4) indent,
     &          layname(1:layl),sim,'   !- Outside Layer' 
            else                          
              WRITE(ioout,'(4a)',iostat=ios,err=4) indent,
     &          layname(1:layl),com,'   !- Outside Layer'
            endif
          elseif(il.eq.LAYERS(INO))then
            if(iver.le.20)then
              WRITE(ioout,'(4a,i2)',iostat=ios,err=4) indent,
     &          layname(1:layl),sim,'   !- Layer #',IL
            else
              WRITE(ioout,'(4a,i2)',iostat=ios,err=4) indent,
     &          layname(1:layl),sim,'   !- Layer ',IL
            endif
          else
            if(iver.le.20)then
              WRITE(ioout,'(4a,i2)',iostat=ios,err=4) indent,
     &          layname(1:layl),com,'   !- Layer #',IL
            else
              WRITE(ioout,'(4a,i2)',iostat=ios,err=4) indent,
     &          layname(1:layl),com,'   !- Layer ',IL
            endif
          endif
  37    continue
  36  continue

C Typical schedule types.
      WRITE(ioout,'(a)',iostat=ios,err=7) ' '
      WRITE(ioout,'(a)',iostat=ios,err=7) 
     &  '!- ============= typical schedule types'
      if(iver.le.20)then
        WRITE(ioout,'(5a)',iostat=ios,err=7) 'Scheduletype',com,
     &    'Any Number',sim,'   !- Not limited.'
      else
        WRITE(ioout,'(5a)',iostat=ios,err=7) '  ScheduleTypeLimits',
     &    com,'Any Number',sim,'   !- Name'
      endif
      if(iver.le.20)then
        WRITE(ioout,'(9a)',iostat=ios,err=7) 'Scheduletype',com,
     &    'Fraction',com,
     &    '0.0:1.0',com,'CONTINUOUS',sim,'   !- between 0.0 and 1.0.'
      else
        WRITE(ioout,'(9a)',iostat=ios,err=7) '  ScheduleTypeLimits',
     &    com,'Fraction',com,
     &    '0.0:1.0',com,'Continuous',sim,'  !- Name Range Numeric Type'
      endif
      if(iver.le.20)then
        WRITE(ioout,'(9a)',iostat=ios,err=7) 'Scheduletype',com,
     &    'Temperature type',com,
     &    '-60:20.0',com,'CONTINUOUS',sim,'   !- temperature range.'
      else
        WRITE(ioout,'(9a)',iostat=ios,err=7) '  ScheduleTypeLimits',
     &    com,'Temperature type',com,
     &    '-60:20.0',com,'Continuous',sim,' !- Name Range Numeric Type'
      endif
      if(iver.le.20)then
        WRITE(ioout,'(9a)',iostat=ios,err=7) 'Scheduletype',com,
     &    'On/Off type',com,
     &    '0.0:40.0',com,'DISCRETE',sim,'   !- zero or one.'
      else
        WRITE(ioout,'(9a)',iostat=ios,err=7) '  ScheduleTypeLimits',
     &    com,'On/Off type',com,
     &    '0.0:40.0',com,'Discrete',sim,'   !- Name Range Numeric Type'
      endif

C Write out a generic nothing-is-happening schedule for zones which
C have a null or all zero operation file.
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        WRITE(ioout,'(a)',iostat=ios,err=7) 
     &    '! Schedules for zones where nothing happens'
        if(iver.le.20)then
          WRITE(ioout,'(2a)',iostat=ios,err=7) 'SCHEDULE:COMPACT',com
          WRITE(ioout,'(3a)',iostat=ios,err=5) '  NOTHING',com,
     &      '   !- Name'
          WRITE(ioout,'(3a)',iostat=ios,err=7) '  Fraction',com,
     &      '   !- ScheduleType' 
        else
          WRITE(ioout,'(2a)',iostat=ios,err=7) '  Schedule:Compact',com
          WRITE(ioout,'(3a)',iostat=ios,err=5) '    NOTHING',com,
     &      '   !- Name'
          WRITE(ioout,'(3a)',iostat=ios,err=7) '    Fraction',com,
     &      '   !- Schedule Type Limits Name' 
        endif
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
        if(iver.le.20)then
          WRITE(ioout,'(2a)',iostat=ios,err=7) 'SCHEDULE:COMPACT',com
          WRITE(ioout,'(3a)',iostat=ios,err=5) '  CONSTANT',com,
     &      '   !- Name'
          WRITE(ioout,'(3a)',iostat=ios,err=7) '  Fraction',com,
     &      '   !- ScheduleType' 
          WRITE(ioout,'(2a)',iostat=ios,err=7) '  Through: 12/31',com
          WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: Weekdays',com
          WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &      '24:00',com,'1.0',com
          WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: Saturday',com
          WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &      '24:00',com,'1.0',com
          WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: Sunday',com
          WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &      '24:00',com,'1.0',com
          WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: AllOtherDays',com
          WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &      '24:00',com,'1.0',sim
        else
          WRITE(ioout,'(2a)',iostat=ios,err=7) '  Schedule:Compact',com
          WRITE(ioout,'(3a)',iostat=ios,err=5) '    CONSTANT',com,
     &      '   !- Name'
          WRITE(ioout,'(3a)',iostat=ios,err=7) '    Fraction',com,
     &      '   !- Schedule Type Limits Name' 
          WRITE(ioout,'(2a)',iostat=ios,err=7) '    Through: 12/31',com
          WRITE(ioout,'(2a)',iostat=ios,err=7) '    For: Weekdays',com
          WRITE(ioout,'(5a)',iostat=ios,err=7) '    Until: ',
     &      '24:00',com,'1.0',com
          WRITE(ioout,'(2a)',iostat=ios,err=7) '    For: Saturday',com
          WRITE(ioout,'(5a)',iostat=ios,err=7) '    Until: ',
     &      '24:00',com,'1.0',com
          WRITE(ioout,'(2a)',iostat=ios,err=7) '    For: Sunday',com
          WRITE(ioout,'(5a)',iostat=ios,err=7) '    Until: ',
     &      '24:00',com,'1.0',com
          WRITE(ioout,'(2a)',iostat=ios,err=7) '    For: AllOtherDays',
     &      com
          WRITE(ioout,'(5a)',iostat=ios,err=7) '    Until: ',
     &      '24:00',com,'1.0',sim
        endif

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
          if(iver.le.20)then
            WRITE(ioout,'(2a)',iostat=ios,err=7) 'LIGHTS',com
            WRITE(ioout,'(5a)',iostat=ios,err=7) indent,
     &        zname(mz)(1:lnzname(mz)),'Lights',com,'   !- Name'
            WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &        zname(mz)(1:lnzname(mz)),com,'   !- Zone Name'
            WRITE(ioout,'(6a)',iostat=ios,err=5) '  NOTHING',com,
     &        '   !- SCHEDULE Name'
            totcgnsl=0.0
            WRITE(ioout,'(a,F12.4,2a)',iostat=ios,err=7) indent,
     &        totcgnsl,com,'   !- Design level (W)'
            WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &        '0.0',com,'   !- Return air fraction'
          else
            WRITE(ioout,'(2a)',iostat=ios,err=7) '  Lights',com
            WRITE(ioout,'(5a)',iostat=ios,err=7) indent,
     &        zname(mz)(1:lnzname(mz)),'Lights',com,'   !- Name'
            WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &        zname(mz)(1:lnzname(mz)),com,'   !- Zone Name'
            WRITE(ioout,'(6a)',iostat=ios,err=5) '    NOTHING',com,
     &        '   !- SCHEDULE Name'
            totcgnsl=0.0
            WRITE(ioout,'(3a)',iostat=ios,err=7) '    LightingLevel',
     &        com,'   !- Design Level Calculation Method'
            WRITE(ioout,'(a,F12.4,2a)',iostat=ios,err=7) indent,
     &        totcgnsl,com,'   !- Lighting Level {W}'
            WRITE(ioout,'(3a)',iostat=ios,err=7) indent,com,
     &        '   !- Watts per Zone Floor Area {W/m2}'
            WRITE(ioout,'(3a)',iostat=ios,err=7) indent,com,
     &        '   !- Watts per Person {W/person}'
            WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &        '0.0',com,'   !- Return Air Fraction'
          endif

          radsplit=0.5
          WRITE(ioout,'(a,F6.4,2a)',iostat=ios,err=5) indent,
     &      radsplit,com,'   !- Fraction Radiant'
          WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &      '0.0',com,'   !- Fraction Visible'
          if(iver.le.20)then
            WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &      '0.0',com,'   !- Fraction Replaceable'

C V1.4 and 2.0 have an end-use subcategory here
            WRITE(ioout,'(3a)',iostat=ios,err=5) indent,
     &        sim,'   !- LightsEndUseKey'
          else

C V3.0 ends with
            WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &      '0.0',sim,'   !- Fraction Replaceable'
          endif

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

C Scan each days gains and calculate maximum gain.
C First clear loadcasp, looping for each gain type.
        do 9 I=1,NBDAYTYPE
          if(I.le.7)loadcasp(mz,i,1)=0.0
          if(I.le.7)loadradfrac(mz,i)=0.0
          loadcasp(mz,i,2)=0.0
          do 8 JD=1,7
            loadonday(mz,jd,i)=.false.
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
        write(6,*)'loadcasp sens ',loadcasp(mz,1,1),loadcasp(mz,2,1),
     &    loadcasp(mz,3,1)
        write(6,*)'loadcasp latn ',loadcasp(mz,1,2),loadcasp(mz,2,2),
     &    loadcasp(mz,3,2)
        write(6,*)'loadonday wss ocup',loadonday(mz,1,1),
     &    loadonday(mz,2,1),loadonday(mz,3,1)
        write(6,*)'loadonday wss lts',loadonday(mz,1,2),
     &    loadonday(mz,2,2),loadonday(mz,3,2)
        write(6,*)'loadonday wss eqp',loadonday(mz,1,3),
     &    loadonday(mz,2,3),loadonday(mz,3,3)
        write(6,*)' loadfrac 1 2 3 ',loadradfrac(mz,1),
     &    loadradfrac(mz,2),loadradfrac(mz,3)

C Write out the initial portion of lighting compact schedule.
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        if(iver.le.20)then
          WRITE(ioout,'(2a)',iostat=ios,err=7) 'SCHEDULE:COMPACT',com
          WRITE(ioout,'(6a)',iostat=ios,err=5) indent,
     &      zname(mz)(1:lnzname(mz)),' ',lodlabel(mz,2),com,
     &      '   !- Name'
          WRITE(ioout,'(3a)',iostat=ios,err=7) '  Fraction',com,
     &      '   !- ScheduleType' 
          WRITE(ioout,'(2a)',iostat=ios,err=7) '  Through: 12/31',com
        else
          WRITE(ioout,'(2a)',iostat=ios,err=7) '  Schedule:Compact',
     &      com
          WRITE(ioout,'(6a)',iostat=ios,err=5) indent,
     &      zname(mz)(1:lnzname(mz)),' ',lodlabel(mz,2),com,
     &      '   !- Name'
          WRITE(ioout,'(3a)',iostat=ios,err=7) '    Fraction',com,
     &      '   !- Schedule Type Limits Name' 
          WRITE(ioout,'(2a)',iostat=ios,err=7) '    Through: 12/31',com
        endif

C Go through loops for each day type for lighting gains.
C If there are no day periods or no lights write standard placeholder.
C Also do placeholder of there are no gains of this type on this day.
        do 11 ID=1,NBDAYTYPE
          totcgnsl=loadcasp(mz,2,1)+loadcasp(mz,2,2)
          call eclose(totcgnsl,0.0,0.001,close)
          if(ncas(ID).eq.0.or.close.or.(.NOT.loadonday(mz,ID,2)))then
            if(iver.le.20)then
              if(ID.eq.1)then
                WRITE(ioout,'(2a)',iostat=ios,err=7) 
     &            '  For: Weekdays',com
              elseif(ID.eq.2)then
                WRITE(ioout,'(2a)',iostat=ios,err=7)
     &            '  For: Saturday',com
              elseif(ID.eq.3)then
                WRITE(ioout,'(2a)',iostat=ios,err=7)
     &            '  For: Sunday',com
              endif
              WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &          '24:00',com,'0.0',com
            else
              if(ID.eq.1)then
                WRITE(ioout,'(2a)',iostat=ios,err=7) 
     &            '    For: Weekdays',com
              elseif(ID.eq.2)then
                WRITE(ioout,'(2a)',iostat=ios,err=7) 
     &            '    For: Saturday',com
              elseif(ID.eq.3)then
                WRITE(ioout,'(2a)',iostat=ios,err=7)
     &            '    For: Sunday',com
              endif
              WRITE(ioout,'(5a)',iostat=ios,err=7) '    Until: ',
     &          '24:00',com,'0.0',com
            endif
          else
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
                write(6,*) 'Lights wk CGNS CGNL totcgnsl totfrac',
     &            CGNS,CGNL,totcgnsl,totfrac

C Need to update to substitute day type name for hard-coded
                if(iver.le.20)then
                  if(ID.eq.1)then
                    WRITE(ioout,'(2a)',iostat=ios,err=7) 
     &                '  For: Weekdays',com
                  elseif(ID.eq.2)then
                    WRITE(ioout,'(2a)',iostat=ios,err=7)
     &                '  For: Saturday',com
                  elseif(ID.eq.3)then
                    WRITE(ioout,'(2a)',iostat=ios,err=7)
     &                '  For: Sunday',com
                  endif
                  WRITE(ioout,'(a,i2.2,2a,F7.4,a)',iostat=ios,err=7) 
     &              '  Until: ',ICGF(ID,J),':00',com,totfrac,com
                else
                  if(ID.eq.1)then
                    WRITE(ioout,'(2a)',iostat=ios,err=7) 
     &                '    For: Weekdays',com
                  elseif(ID.eq.2)then
                    WRITE(ioout,'(2a)',iostat=ios,err=7) 
     &                '    For: Saturday',com
                  elseif(ID.eq.3)then
                    WRITE(ioout,'(2a)',iostat=ios,err=7)
     &                '    For: Sunday',com
                  endif
                  WRITE(ioout,'(a,i2.2,2a,F7.4,a)',iostat=ios,err=7) 
     &              '    Until: ',ICGF(ID,J),':00',com,totfrac,com
                endif
              endif
 77         continue
          endif
 11     continue

C Write the AllOtherDays line as well.
        if(iver.le.20)then
          WRITE(ioout,'(2a)',iostat=ios,err=7)
     &      '  For: AllOtherDays',com
          WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &      '24:00',com,'0.0',sim
        else
          WRITE(ioout,'(2a)',iostat=ios,err=7)
     &      '    For: AllOtherDays',com
          WRITE(ioout,'(5a)',iostat=ios,err=7) '    Until: ',
     &      '24:00',com,'0.0',sim
        endif

C Write the light objects for this zone.
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        if(iver.le.20)then
          WRITE(ioout,'(2a)',iostat=ios,err=7) 'LIGHTS',com
          WRITE(ioout,'(5a)',iostat=ios,err=7) indent,
     &      zname(mz)(1:lnzname(mz)),'Lights',com,'   !- Name'
          WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &      zname(mz)(1:lnzname(mz)),com,'   !- Zone Name'
          WRITE(ioout,'(6a)',iostat=ios,err=5) indent,
     &      zname(mz)(1:lnzname(mz)),' ',lodlabel(mz,2),com,
     &      '   !- Schedule Name'
          totcgnsl=loadcasp(mz,2,1)+loadcasp(mz,2,2)
          WRITE(ioout,'(a,F12.4,2a)',iostat=ios,err=7) indent,
     &      totcgnsl,com,'   !- Design level (W)'
          WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &      '0.0',com,'   !- Return air fraction'
        else
          WRITE(ioout,'(2a)',iostat=ios,err=7) '  Lights',com
          WRITE(ioout,'(5a)',iostat=ios,err=7) indent,
     &      zname(mz)(1:lnzname(mz)),'  Lights',com,'   !- Name'
          WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &      zname(mz)(1:lnzname(mz)),com,'   !- Zone Name'
          WRITE(ioout,'(6a)',iostat=ios,err=5) indent,
     &      zname(mz)(1:lnzname(mz)),' ',lodlabel(mz,2),com,
     &      '   !- Schedule Name'
          totcgnsl=loadcasp(mz,2,1)+loadcasp(mz,2,2)
          WRITE(ioout,'(3a)',iostat=ios,err=7) '    LightingLevel',
     &      com,'   !- Design Level Calculation Method'
          WRITE(ioout,'(a,F12.4,2a)',iostat=ios,err=7) '    ',
     &      totcgnsl,com,'   !- Lighting Level {W}'
          WRITE(ioout,'(3a)',iostat=ios,err=7) '    ',com,
     &      '   !- Watts per Zone Floor Area {W/m2}'
          WRITE(ioout,'(3a)',iostat=ios,err=7) '    ',com,
     &      '   !- Watts per Person {W/person}'
          WRITE(ioout,'(4a)',iostat=ios,err=5) '    ',
     &      '0.0',com,'   !- Return Air Fraction'
        endif
 
C Set radiant for this gain type based on the max value from earlier
C scan of the operation file data.
C Further logic required if there is more than one radiant/convective split.
        if(iver.le.20)then
          WRITE(ioout,'(a,F6.4,2a)',iostat=ios,err=5) indent,
     &      loadradfrac(mz,2),com,'   !- Radiant fraction'
          WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &      '0.0',com,'   !- Fraction visible'
          WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &      '0.0',com,'   !- Fraction Replaceable'

C V1.4 and 2.0 have an end-use subcategory here
          WRITE(ioout,'(3a)',iostat=ios,err=5) indent,
     &      sim,'   !- LightsEndUseKey'
        else

C V3.0 ends with
          WRITE(ioout,'(a,F6.4,2a)',iostat=ios,err=5) indent,
     &      loadradfrac(mz,2),com,'   !- Fraction Radiant'
          WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &      '0.0',com,'   !- Fraction visible'
          WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &      '0.0',sim,'   !- Fraction Replaceable'
        endif


C Write out the initial portion of small power compact schedule.
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        if(iver.le.20)then
          WRITE(ioout,'(2a)',iostat=ios,err=7) 'SCHEDULE:COMPACT',com
          WRITE(ioout,'(6a)',iostat=ios,err=5) indent,
     &     zname(mz)(1:lnzname(mz)),' ',lodlabel(mz,3),com,'   !- Name'
          WRITE(ioout,'(3a)',iostat=ios,err=7) '  Fraction',com,
     &     '   !- ScheduleType' 
          WRITE(ioout,'(2a)',iostat=ios,err=7) '  Through: 12/31',com
        else
          WRITE(ioout,'(2a)',iostat=ios,err=7) '  Schedule:Compact',com
          WRITE(ioout,'(6a)',iostat=ios,err=5) indent,
     &    zname(mz)(1:lnzname(mz)),' ',lodlabel(mz,3),com,'   !- Name'
          WRITE(ioout,'(3a)',iostat=ios,err=7) '    Fraction',com,
     &     '   !- Schedule Type Limits Name' 
          WRITE(ioout,'(2a)',iostat=ios,err=7) '    Through: 12/31',com
        endif

C Go through loops for each day type for small power gains.
        do 133 ID=1,NBDAYTYPE
          totcgnsl=loadcasp(mz,3,1)+loadcasp(mz,3,2)
          call eclose(totcgnsl,0.0,0.001,close)
          if(ncas(ID).eq.0.or.close.or.(.NOT.loadonday(mz,1,3)))then
            if(iver.le.20)then
              if(ID.eq.1)then
                WRITE(ioout,'(2a)',iostat=ios,err=7) 
     &            '  For: Weekdays',com
              elseif(ID.eq.2)then
                WRITE(ioout,'(2a)',iostat=ios,err=7)
     &            '  For: Saturday',com
              elseif(ID.eq.3)then
                WRITE(ioout,'(2a)',iostat=ios,err=7)
     &            '  For: Sunday',com
              endif
              WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &          '24:00',com,'0.0',com
            else
              if(ID.eq.1)then
                WRITE(ioout,'(2a)',iostat=ios,err=7) 
     &            '    For: Weekdays',com
              elseif(ID.eq.2)then
                WRITE(ioout,'(2a)',iostat=ios,err=7) 
     &            '    For: Saturday',com
              elseif(ID.eq.3)then
                WRITE(ioout,'(2a)',iostat=ios,err=7)
     &            '    For: Sunday',com
              endif
              WRITE(ioout,'(5a)',iostat=ios,err=7) '    Until: ',
     &          '24:00',com,'0.0',com
            endif
          else
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
                write(6,*) 'SmP wkd CGNS CGNL totcgnsl totfrac',
     &            CGNS,CGNL,totcgnsl,totfrac

                if(iver.le.20)then
                  if(ID.eq.1)then
                    WRITE(ioout,'(2a)',iostat=ios,err=7) 
     &                '  For: Weekdays',com
                  elseif(ID.eq.2)then
                    WRITE(ioout,'(2a)',iostat=ios,err=7)
     &                '  For: Saturday',com
                  elseif(ID.eq.3)then
                    WRITE(ioout,'(2a)',iostat=ios,err=7)
     &                '  For: Sunday',com
                  endif
                  WRITE(ioout,'(a,i2.2,2a,F7.4,a)',iostat=ios,err=7) 
     &              '  Until: ',ICGF(ID,J),':00',com,totfrac,com
                else
                  if(ID.eq.1)then
                    WRITE(ioout,'(2a)',iostat=ios,err=7) 
     &                '    For: Weekdays',com
                  elseif(ID.eq.2)then
                    WRITE(ioout,'(2a)',iostat=ios,err=7) 
     &                '    For: Saturday',com
                  elseif(ID.eq.3)then
                    WRITE(ioout,'(2a)',iostat=ios,err=7)
     &                '    For: Sunday',com
                  endif
                  WRITE(ioout,'(a,i2.2,2a,F7.4,a)',iostat=ios,err=7) 
     &              '    Until: ',ICGF(ID,J),':00',com,totfrac,com
                endif
              endif
 67         continue
          endif
 133    continue

C Write the AllOtherDays line as well.
        if(iver.le.20)then
          WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: AllOtherDays',
     &      com
          WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &      '24:00',com,'0.0',sim
        else
          WRITE(ioout,'(2a)',iostat=ios,err=7) 
     &      '    For: AllOtherDays',com
          WRITE(ioout,'(5a)',iostat=ios,err=7) '    Until: ',
     &      '24:00',com,'0.0',sim
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
        if(iver.le.20)then
          WRITE(ioout,'(2a)',iostat=ios,err=7) 'ELECTRIC EQUIPMENT',com

          WRITE(ioout,'(5a)',iostat=ios,err=7) indent,
     &      zname(mz)(1:lnzname(mz)),'EQUIPMENT',com,'   !- Name'

          WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &      zname(mz)(1:lnzname(mz)),com,'   !- Zone Name'
          WRITE(ioout,'(6a)',iostat=ios,err=5) indent,
     &      zname(mz)(1:lnzname(mz)),' ',lodlabel(mz,3),com,
     &      '   !- Schedule Name'
          totcgnsl=loadcasp(mz,3,1)+loadcasp(mz,3,2)
          WRITE(ioout,'(a,F12.4,a)',iostat=ios,err=7) indent,
     &      totcgnsl,com
        else
          WRITE(ioout,'(2a)',iostat=ios,err=7) 'ElectricEquipment',com

          WRITE(ioout,'(5a)',iostat=ios,err=7) indent,
     &      zname(mz)(1:lnzname(mz)),'Equipment',com,'   !- Name'

          WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &      zname(mz)(1:lnzname(mz)),com,'   !- Zone Name'
          WRITE(ioout,'(6a)',iostat=ios,err=5) indent,
     &      zname(mz)(1:lnzname(mz)),' ',lodlabel(mz,3),com,
     &      '   !- Schedule Name'
          WRITE(ioout,'(3a)',iostat=ios,err=7) '    EquipmentLevel',
     &      com,'   !- Design Level Calculation Method'
          totcgnsl=loadcasp(mz,3,1)+loadcasp(mz,3,2)
          WRITE(ioout,'(a,F12.4,a)',iostat=ios,err=7) indent,
     &      totcgnsl,com
          WRITE(ioout,'(3a)',iostat=ios,err=7) '    ',com,
     &      '   !- Watts per Zone Floor Area {W/m2}'
          WRITE(ioout,'(3a)',iostat=ios,err=7) '    ',com,
     &      '   !- Watts per Person {W/person}'
        endif

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
C << what this is really writing out is ??
C << need a variant for number of people

C Write out the initial portion of people compact schedule.
  12    continue
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        WRITE(ioout,'(a)',iostat=ios,err=7) 
     &    '!- occupants schedule/activity level/PEOPLE definition... '
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        if(iver.le.20)then
          WRITE(ioout,'(2a)',iostat=ios,err=7) 'SCHEDULE:COMPACT',com
          WRITE(ioout,'(6a)',iostat=ios,err=5) indent,
     &     zname(mz)(1:lnzname(mz)),' ',lodlabel(mz,1),com,'   !- Name'
          WRITE(ioout,'(3a)',iostat=ios,err=7) '  Fraction',com,
     &      '   !- ScheduleType' 
          WRITE(ioout,'(2a)',iostat=ios,err=7) '  Through: 12/31',com
        else
          WRITE(ioout,'(2a)',iostat=ios,err=7) '  Schedule:Compact',com
          WRITE(ioout,'(6a)',iostat=ios,err=5) indent,
     &    zname(mz)(1:lnzname(mz)),' ',lodlabel(mz,1),com,'   !- Name'
          WRITE(ioout,'(3a)',iostat=ios,err=7) '    Fraction',com,
     &      '   !- Schedule Type Limits Name' 
          WRITE(ioout,'(2a)',iostat=ios,err=7) '    Through: 12/31',com
        endif

C Go through loops for each day type for occupants.
C If there are no day periods or no people write standard placeholder.
C Also do placeholder of there are no gains of this type on this day.
        do 144 ID=1,NBDAYTYPE
          totcgnsl=loadcasp(mz,1,1)+loadcasp(mz,1,2)
          call eclose(totcgnsl,0.0,0.001,close)
          if(ncas(ID).eq.0.or.close.or.(.NOT.loadonday(mz,1,1)))then
            if(iver.le.20)then
              if(ID.eq.1)then
                WRITE(ioout,'(2a)',iostat=ios,err=7) 
     &            '  For: Weekdays',com
              elseif(ID.eq.2)then
                WRITE(ioout,'(2a)',iostat=ios,err=7)
     &            '  For: Saturday',com
              elseif(ID.eq.3)then
                WRITE(ioout,'(2a)',iostat=ios,err=7)
     &            '  For: Sunday',com
              endif
              WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &          '24:00',com,'0.0',com
            else
              if(ID.eq.1)then
                WRITE(ioout,'(2a)',iostat=ios,err=7) 
     &            '    For: Weekdays',com
              elseif(ID.eq.2)then
                WRITE(ioout,'(2a)',iostat=ios,err=7) 
     &            '    For: Saturday',com
              elseif(ID.eq.3)then
                WRITE(ioout,'(2a)',iostat=ios,err=7)
     &            '    For: Sunday',com
              endif
              WRITE(ioout,'(5a)',iostat=ios,err=7) '    Until: ',
     &          '24:00',com,'0.0',com
            endif
          else
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
                write(6,*) 'People wkd CGNS CGNL totcgnsl totfrac',
     &            CGNS,CGNL,totcgnsl,totfrac

                if(iver.le.20)then
                  if(ID.eq.1)then
                    WRITE(ioout,'(2a)',iostat=ios,err=7) 
     &                '  For: Weekdays',com
                  elseif(ID.eq.2)then
                    WRITE(ioout,'(2a)',iostat=ios,err=7)
     &                '  For: Saturday',com
                  elseif(ID.eq.3)then
                    WRITE(ioout,'(2a)',iostat=ios,err=7)
     &                '  For: Sunday',com
                  endif
                  WRITE(ioout,'(a,i2.2,2a,F7.4,a)',iostat=ios,err=7) 
     &              '  Until: ',ICGF(ID,J),':00',com,totfrac,com
                else
                  if(ID.eq.1)then
                    WRITE(ioout,'(2a)',iostat=ios,err=7) 
     &                '    For: Weekdays',com
                  elseif(ID.eq.2)then
                    WRITE(ioout,'(2a)',iostat=ios,err=7) 
     &                '    For: Saturday',com
                  elseif(ID.eq.3)then
                    WRITE(ioout,'(2a)',iostat=ios,err=7)
     &                '    For: Sunday',com
                  endif
                  WRITE(ioout,'(a,i2.2,2a,F7.4,a)',iostat=ios,err=7) 
     &              '    Until: ',ICGF(ID,J),':00',com,totfrac,com
                endif
              endif
 57         continue
          endif
 144    continue

C Write the AllOtherDays line as well.

        if(iver.le.20)then
          WRITE(ioout,'(2a)',iostat=ios,err=7) 
     &      '  For: AllOtherDays',com
          WRITE(ioout,'(5a)',iostat=ios,err=7) '  Until: ',
     &      '24:00',com,'0.0',sim
        else
          WRITE(ioout,'(2a)',iostat=ios,err=7) 
     &      '    For: AllOtherDays',com
          WRITE(ioout,'(5a)',iostat=ios,err=7) '    Until: ',
     &      '24:00',com,'0.0',sim
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

C << Currently write the activity schedule as if one period in the day
C << and with the value for oneperson.

        vpeople=totcgnsl/140.0
        CALL EASKR(vpeople,' Number of people in room (see report): ',
     &    ' ',0.0,'F',99.999,'W',1.0,'people in room',IER,4)
        oneperson = totcgnsl/vpeople
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        if(iver.le.20)then
          WRITE(ioout,'(2a)',iostat=ios,err=7) 'SCHEDULE:COMPACT',com
          WRITE(ioout,'(5a)',iostat=ios,err=5) indent,
     &      'Activity level ',zname(mz)(1:lnzname(mz)),com,
     &      '   !- Name'
          WRITE(ioout,'(3a)',iostat=ios,err=7) '  Any Number',com,
     &      '   !- ScheduleType' 
          WRITE(ioout,'(2a)',iostat=ios,err=7) '  Through: 12/31',com
          WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: Weekdays',com
          WRITE(ioout,'(3a,F7.3,a)',iostat=ios,err=7) '  Until: ',
     &      '24:00',com,oneperson,com
          WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: Saturday',com
          WRITE(ioout,'(3a,F7.3,a)',iostat=ios,err=7) '  Until: ',
     &      '24:00',com,oneperson,com
          WRITE(ioout,'(2a)',iostat=ios,err=7) '  For: Sunday',com
          WRITE(ioout,'(3a,F7.3,a)',iostat=ios,err=7) '  Until: ',
     &      '24:00',com,oneperson,com
          WRITE(ioout,'(2a)',iostat=ios,err=7)'  For: AllOtherDays',com
          WRITE(ioout,'(3a,F7.3,a)',iostat=ios,err=7) '  Until: ',
     &      '24:00',com,oneperson,sim
        else
          WRITE(ioout,'(2a)',iostat=ios,err=7) '  Schedule:Compact',com
          WRITE(ioout,'(5a)',iostat=ios,err=5) '    ',
     &      'Activity level ',zname(mz)(1:lnzname(mz)),com,
     &      '   !- Name'
          WRITE(ioout,'(3a)',iostat=ios,err=7) '    Any Number',com,
     &      '   !- Schedule Type Limits Name' 
          WRITE(ioout,'(2a)',iostat=ios,err=7) '    Through: 12/31',com
          WRITE(ioout,'(2a)',iostat=ios,err=7) '    For: Weekdays',com
          WRITE(ioout,'(3a,F7.3,a)',iostat=ios,err=7) '    Until: ',
     &      '24:00',com,oneperson,com
          WRITE(ioout,'(2a)',iostat=ios,err=7) '    For: Saturday',com
          WRITE(ioout,'(3a,F7.3,a)',iostat=ios,err=7) '    Until: ',
     &      '24:00',com,oneperson,com
          WRITE(ioout,'(2a)',iostat=ios,err=7) '    For: Sunday',com
          WRITE(ioout,'(3a,F7.3,a)',iostat=ios,err=7) '    Until: ',
     &      '24:00',com,oneperson,com
          WRITE(ioout,'(2a)',iostat=ios,err=7)'    For: AllOtherDays',
     &      com
          WRITE(ioout,'(3a,F7.3,a)',iostat=ios,err=7) '    Until: ',
     &      '24:00',com,oneperson,sim
        endif

C Now write out the people.
        WRITE(ioout,'(a)',iostat=ios,err=7) ' '
        if(iver.le.20)then
          WRITE(ioout,'(2a)',iostat=ios,err=7) 'PEOPLE',com

          WRITE(ioout,'(5a)',iostat=ios,err=7) indent,
     &      zname(mz)(1:lnzname(mz)),'People',com,'   !- Name'

          WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &      zname(mz)(1:lnzname(mz)),com,'   !- Zone Name'
          WRITE(ioout,'(a,F7.3,2a)',iostat=ios,err=7) indent,vpeople,
     &      com,'   !- Number of people'
          WRITE(ioout,'(6a)',iostat=ios,err=5) indent,
     &      zname(mz)(1:lnzname(mz)),' ',lodlabel(mz,1),com,
     &      '   !- Number of people schedule'
        else
          WRITE(ioout,'(2a)',iostat=ios,err=7) '  People',com

          WRITE(ioout,'(5a)',iostat=ios,err=7) '    ',
     &      zname(mz)(1:lnzname(mz)),'People',com,'   !- Name'

          WRITE(ioout,'(4a)',iostat=ios,err=5) '    ',
     &      zname(mz)(1:lnzname(mz)),com,'   !- Zone Name'
          WRITE(ioout,'(6a)',iostat=ios,err=5) '    ',
     &      zname(mz)(1:lnzname(mz)),' ',lodlabel(mz,1),com,
     &      '   !- Number of people schedule'
          WRITE(ioout,'(3a)',iostat=ios,err=7) '    people',com,
     &      '   !- Number of people calculation method'
          WRITE(ioout,'(a,F7.3,2a)',iostat=ios,err=7) '    ',vpeople,
     &      com,'   !- Number of people'
          WRITE(ioout,'(3a)',iostat=ios,err=7) '    ',com,
     &      '   !- People per Zone Floor Area {person/m2}'
          WRITE(ioout,'(3a)',iostat=ios,err=7) '    ',com,
     &      '   !- Zone Floor Area per person {m2/person}'
        endif

C Set radiant as earlier scan
C Further logic required if there is more than one radiant/convective split.
        if(iver.le.20)then
          WRITE(ioout,'(a,F6.4,2a)',iostat=ios,err=5) indent,
     &      loadradfrac(mz,1),com,'   !- Radiant fraction'
          WRITE(ioout,'(5a)',iostat=ios,err=5) indent,
     &      'Activity level ',zname(mz)(1:lnzname(mz)),com,
     &      '   !- Activity level schedule name (W/person)'
          WRITE(ioout,'(3a)',iostat=ios,err=5) indent,com,
     &      '  !- MRT Calculation Type'
        else
          WRITE(ioout,'(a,F6.4,2a)',iostat=ios,err=5) '    ',
     &      loadradfrac(mz,1),com,'   !- Fraction Radiant'
          WRITE(ioout,'(3a)',iostat=ios,err=5) '    Autocalculate',com,
     &      '  !- Sensible Heat Fraction'
          WRITE(ioout,'(5a)',iostat=ios,err=5) '    ',
     &      'Activity level ',zname(mz)(1:lnzname(mz)),com,
     &      '   !- Activity level schedule name (W/person)'
          WRITE(ioout,'(3a)',iostat=ios,err=5) '    ZoneAveraged',com,
     &      '  !- MRT Calculation Type'
        endif
        WRITE(ioout,'(3a)',iostat=ios,err=5) indent,com,
     &    '  !- Surface Name/Angle Factor List Name'
        WRITE(ioout,'(3a)',iostat=ios,err=5) indent,com,
     &    '  !- Work Efficiency SCHEDULE Name (0.0-1.0,real)'
        WRITE(ioout,'(3a)',iostat=ios,err=5) indent,com,
     &    '  !- Clothing Insulation SCHEDULE Name (real)'
        WRITE(ioout,'(3a)',iostat=ios,err=5) indent,com,
     &    '  !- Air Velocity SCHEDULE Name (units m/s, real)'
        WRITE(ioout,'(3a)',iostat=ios,err=5) indent,com,
     &    '  !- Thermal Comfort Report Type R1'
        WRITE(ioout,'(3a)',iostat=ios,err=5) indent,com,
     &    '  !- Thermal Comfort Report Type R2'
        WRITE(ioout,'(3a)',iostat=ios,err=5) indent,com,
     &    '  !- Thermal Comfort Report Type R3'

C Include the sensible fraction of the total.
        totcgnsl=loadcasp(mz,1,1)+loadcasp(mz,1,2)
        sensfrac=1.0
        if(totcgnsl.gt.0.1)sensfrac=loadcasp(mz,1,1)/totcgnsl
        if(iver.le.20)then
          WRITE(ioout,'(a,F6.4,2a)',iostat=ios,err=5) indent,
     &      sensfrac,sim,'   !- user specified sensible fraction'
        else
          WRITE(ioout,'(a,F6.4,2a)',iostat=ios,err=5) '    ',
     &      sensfrac,sim,'   !- user specified sensible fraction'
        endif

 13   continue

C << More schedules here >>

C Now for each zone, write out the surfaces.
C Setup standard assumptions.
      WRITE(ioout,'(a)',iostat=ios,err=6) ' '
      WRITE(ioout,'(a)',iostat=ios,err=6) '!- ============='
      if(iver.le.20)then
        WRITE(ioout,'(8a)',iostat=ios,err=6) 'SurfaceGeometry',com,
     &    'LowerLeftCorner',com,
     &    'CounterClockWise',com,'WorldCoordinateSystem',sim
      else
        WRITE(ioout,'(8a)',iostat=ios,err=6) '  GlobalGeometryRules',
     &    com,'LowerLeftCorner',com,
     &    'Counterclockwise',com,'WorldCoordinateSystem',sim
      endif
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
        if(iver.le.20)then
          WRITE(ioout,'(2a)',iostat=ios,err=5) 'ZONE',com
          WRITE(ioout,'(4a)',iostat=ios,err=5) indent,
     &      zname(mz)(1:lnzname(mz)),com,'   !- Zone Name'
          WRITE(ioout,'(4a)',iostat=ios,err=5) indent,'0.0',com,
     &      '0.0',com,'   !- Origin {m}'
          WRITE(ioout,'(8a)',iostat=ios,err=5) indent,'0.0',com,'0.0',
     &      com,'   !- Relative North (to building) {deg}'
          WRITE(ioout,'(6a)',iostat=ios,err=5) indent,'1',com,'1',com,
     &      '   !- Type and Muitiplier'
        else
          WRITE(ioout,'(2a)',iostat=ios,err=5) '  Zone',com
          WRITE(ioout,'(4a)',iostat=ios,err=5) '    ',
     &      zname(mz)(1:lnzname(mz)),com,'   !- Name'
          WRITE(ioout,'(4a)',iostat=ios,err=5) '    ','0.0',com,
     &      '   !- Direction of Relative North {deg}'
          WRITE(ioout,'(8a)',iostat=ios,err=5) '    ','0.0',com,'0.0',
     &      com,'0.0',com,'   !- Origin {m}'
          WRITE(ioout,'(6a)',iostat=ios,err=5) '    ','1',com,'1',com,
     &      '   !- Type and Muitiplier'
        endif
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

C  << this logic to be updated >>
          WRITE(ioout,'(a)') ' '

          if(SSOTF(icc)(1:4).NE.'OPAQ'.AND.
     &      SSOTF(icc)(1:4).NE.'CFC ')then
            if(iver.le.20)then
              WRITE(ioout,'(2a)',iostat=ios,err=6) 
     &          'Surface:HeatTransfer:sub',com
            else
              WRITE(ioout,'(2a)',iostat=ios,err=6) 
     &          '  FenestrationSurface:Detailed',com
            endif
          else
            if(ICT(icc).eq.5.or.ICT(icc).eq.1)then
              if(iver.le.20)then
                WRITE(ioout,'(2a)',iostat=ios,err=6) 
     &            'Surface:HeatTransfer:InternalMass',com
              else
                WRITE(ioout,'(2a)',iostat=ios,err=6) 
     &            '  InternalMass',com
              endif
            else
              if(SSPARENT(icc)(1:2).eq.'  '.or.
     &           SSPARENT(icc)(1:2).eq.'- ')then
                if(iver.le.20)then
                  WRITE(ioout,'(2a)',iostat=ios,err=6) 
     &              'Surface:HeatTransfer',com
                else
                  WRITE(ioout,'(2a)',iostat=ios,err=6) 
     &              '  BuildingSurface:Detailed',com
                endif
              else
                if(iver.le.20)then
                  WRITE(ioout,'(3a)',iostat=ios,err=6) 
     &              'Surface:HeatTransfer:sub',com,'   !- (a door)'
                else
                  WRITE(ioout,'(3a)',iostat=ios,err=6) 
     &              '  FenestrationSurface:Detailed',com,
     &              '   !- (a door)'
                endif
              endif
            endif
          endif
          call zsid(mz,is,ZSDES,ZSDESC,ZSDESS)
          if(iver.le.20)then
            WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &        ZSDES(1:lnblnk(ZSDES)),com,
     &        '   !- User Supplied Surface Name'
          else
            WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &        ZSDES(1:lnblnk(ZSDES)),com,'   !- Name'
          endif
          if(SSOTF(icc)(1:4).NE.'OPAQ'.AND.
     &      SSOTF(icc)(1:4).NE.'CFC ')then
            if(iver.le.20)then
              WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'WINDOW',com,
     &          '   !- Surface Type!'
            else
              WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'Window',com,
     &          '   !- Surface Type!'
            endif
            if(iver.le.20)then
              WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &          SSMLCN(icc)(1:lnblnk(SSMLCN(icc))),com,
     &          '   !- Construction Name of the Surface'
            else
              WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &          SSMLCN(icc)(1:lnblnk(SSMLCN(icc))),com,
     &          '   !- Construction Name'
            endif
            lsn0=lnblnk(SSPARENT(icc))
            lzn0=lnzname(mz)
            WRITE(ZSDES,'(a,a1,a)',iostat=ios,err=6) 
     &        SSPARENT(icc)(1:lsn0),':',zname(mz)(1:lzn0)
            if(iver.le.20)then
              WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &          ZSDES(1:lnblnk(ZSDES)),com,'   !- Base Surface Name'
            else
              WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &          ZSDES(1:lnblnk(ZSDES)),com,'   !- Building Surface Name'
            endif
            if(iver.le.20)then
              WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &          '   !- OutsideFaceEnvironment Object'
            else
              WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &          '   !- Outside Boundary Condition Object'
            endif

C Also include viewfactor to ground value depending on orientation
C and site exposure.
            if(siteexposureindex.ne.8)then
              if(SSVFC(icc)(1:4).eq.'VERT')then
                if(iver.le.20)then
                  WRITE(ioout,'(a,f5.3,2a)',iostat=ios,err=6) indent,
     &              PROPG(siteexposureindex),com,
     &              '   !- View Factor to Ground'
                else
                  WRITE(ioout,'(a,f5.3,2a)',iostat=ios,err=6) '    ',
     &              PROPG(siteexposureindex),com,
     &              '   !- View Factor to Ground'
                endif
              elseif(SSVFC(icc)(1:4).eq.'CEIL')then
                WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'0.0',
     &            com,'   !- View Factor to Ground'
              elseif(SSVFC(icc)(1:4).eq.'SLOP'.or.
     &               SSVFC(icc)(1:4).eq.'UNKN')then
                WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'0.0',
     &            com,'   !- View Factor to Ground'
              elseif(SSVFC(icc)(1:4).eq.'FLOR')then
                WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'1.0',
     &            com,'   !- View Factor to Ground'
              endif
            else
              if(SSVFC(icc)(1:4).eq.'VERT')then
                WRITE(ioout,'(a,f5.3,2a)',iostat=ios,err=6) indent,
     &           groundview,com,'   !- View Factor to Ground'
              elseif(SSVFC(icc)(1:4).eq.'CEIL')then
                WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'0.0',
     &            com,'   !- View Factor to Ground'
              elseif(SSVFC(icc)(1:4).eq.'SLOP'.or.
     &               SSVFC(icc)(1:4).eq.'UNKN')then
                WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'0.0',
     &            com,'   !- View Factor to Ground'
              elseif(SSVFC(icc)(1:4).eq.'FLOR')then
                WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'1.0',
     &            com,'   !- View Factor to Ground'
              endif
            endif      
            if(iver.le.20)then
              WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &          '   !-   Name of shading control'
            else
              WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &          '   !-   Shading Control Name'
            endif
            if(iver.le.20)then
              WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &          '   !-   WindowFrameAndDivider Name'
              WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'1.0',com,
     &          '   !-   Multiplier'
            else
              WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &          '   !-   Frame and Divider Name'
              WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'1.0',com,
     &          '   !-   Multiplier'
           endif
          else
            if(ICT(icc).eq.5.or.ICT(icc).eq.1)then
              if(iver.le.20)then
                WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &            SSMLCN(icc)(1:lnblnk(SSMLCN(icc))),com,
     &            '   !- Construction Name of the Surface'
              else
                WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &            SSMLCN(icc)(1:lnblnk(SSMLCN(icc))),com,
     &            '   !- Construction Name'
              endif
              if(iver.eq.14)then
                WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &            zname(mz)(1:lnzname(mz)),
     &            com,'   !- InsideFaceEnvironment'
                WRITE(ioout,'(a,f8.4,2a)',iostat=ios,err=6) indent,
     &            SSNA(icc),sim,'   !-  Area {m2}'
              elseif(iver.ge.20)then
                WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &            zname(mz)(1:lnzname(mz)),
     &            com,'   !- Zone Name'
                WRITE(ioout,'(a,f8.4,2a)',iostat=ios,err=6) indent,
     &            SSNA(icc),sim,'   !-  Surface Area {m2}'
              endif
              goto 60
            else

C Check if parent surface name is a blank or just a dash.
              if(SSPARENT(icc)(1:2).eq.'  '.or.
     &           SSPARENT(icc)(1:2).eq.'- ')then
                if(SSVFC(icc)(1:4).eq.'VERT')then
                  if(iver.le.20)then
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'WALL',
     &                com,'   !- Surface Type'
                  else
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'Wall',
     &                com,'   !- Surface Type'
                  endif
                elseif(SSVFC(icc)(1:4).eq.'SLOP'.or.
     &                 SSVFC(icc)(1:4).eq.'UNKN')then
                  if(ICT(icc).eq.0.or.ICT(icc).eq.4)then
                    if(iver.le.20)then
                      WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                  'ROOF',com,'   !- Surface Type'
                    else
                      WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                  'Roof',com,'   !- Surface Type'
                    endif
                  elseif(ICT(icc).eq.3)then
                    if(iver.le.20)then
                      WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                  'WALL',com,'   !- Surface Type'
                    else
                      WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                  'Wall',com,'   !- Surface Type'
                    endif
                  endif
                elseif(SSVFC(icc)(1:4).eq.'CEIL')then
                  if(ICT(icc).eq.0.or.ICT(icc).eq.4)then
                    if(iver.le.20)then
                      WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                  'ROOF',com,'   !- Surface Type'
                    else
                      WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                  'Roof',com,'   !- Surface Type'
                    endif
                  elseif(ICT(icc).eq.3)then
                    if(iver.le.20)then
                      WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                  'CEILING',com,'   !- Surface Type'
                    else
                      WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                  'Ceiling',com,'   !- Surface Type'
                    endif
                  endif
                elseif(SSVFC(icc)(1:4).eq.'FLOR')then
                  if(iver.le.20)then
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'FLOOR',
     &                com,'   !- Surface Type'
                  else
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                'Floor',com,'   !- Surface Type'
                  endif
                endif

                if(iver.le.20)then
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &              SSMLCN(icc)(1:lnblnk(SSMLCN(icc))),com,
     &              '   !- Construction Name of the Surface'
                else
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &              SSMLCN(icc)(1:lnblnk(SSMLCN(icc))),com,
     &              '   !- Construction Name'
                endif
                if(iver.eq.14)then
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &              zname(mz)(1:lnzname(mz)),
     &              com,'  !- InsideFaceEnvironment'
                elseif(iver.ge.20)then
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &              zname(mz)(1:lnzname(mz)),
     &              com,'   !- Zone Name'
                endif

C Other face environment.
                if(ICT(icc).eq.0)then
                  if(iver.le.20)then
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                'ExteriorEnvironment',
     &                com,'   !-  OutsideFaceEnvironment'
                  else
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                'Outdoors',
     &                com,'   !-  Outside Boundary Condition'
                  endif
                  if(iver.le.20)then
                    WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &                '   !- OutsideFaceEnvironment Object'
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                'SunExposed',com,'   !- Sun Exposure'
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                'WindExposed',com,'   !- Wind Exposure'
                  else
                    WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &                '   !- Outside Boundary Condition Object'
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                'SunExposed',com,'   !- Sun Exposure'
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                'WindExposed',com,'   !- Wind Exposure'
                  endif
                  if(siteexposureindex.ne.8)then
                    if(SSVFC(icc)(1:4).eq.'VERT')then
                      WRITE(ioout,'(a,f5.3,2a)',iostat=ios,err=6) 
     &                  indent,PROPG(siteexposureindex),com,
     &                  '   !- View Factor to Ground'
                    elseif(SSVFC(icc)(1:4).eq.'CEIL')then
                      WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                  '0.0',com,'   !- View Factor to Ground'
                    elseif(SSVFC(icc)(1:4).eq.'SLOP'.or.
     &                     SSVFC(icc)(1:4).eq.'UNKN')then
                      WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                  '0.0',com,'   !- View Factor to Ground'
                    elseif(SSVFC(icc)(1:4).eq.'FLOR')then
                      WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                  '1.0',com,'   !- View Factor to Ground'
                    endif
                  else
                    if(SSVFC(icc)(1:4).eq.'VERT')then
                      WRITE(ioout,'(a,f5.3,2a)',iostat=ios,err=6) 
     &                  indent,groundview,com,
     &                  '   !- View Factor to Ground'
                    elseif(SSVFC(icc)(1:4).eq.'CEIL')then
                      WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                  '0.0',com,'   !- View Factor to Ground'
                    elseif(SSVFC(icc)(1:4).eq.'SLOP'.or.
     &                     SSVFC(icc)(1:4).eq.'UNKN')then
                      WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                  '0.0',com,'   !- View Factor to Ground'
                    elseif(SSVFC(icc)(1:4).eq.'FLOR')then
                      WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                  '1.0',com,'   !- View Factor to Ground'
                    endif
                  endif
                elseif(ICT(icc).eq.1)then

C Continue because this has been considered internal mass.
                  continue
                elseif(ICT(icc).eq.2)then

C A constant connection still has to be resolved.
                  continue
                elseif(ICT(icc).eq.3)then
                  if(iver.le.20)then
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                'OtherZoneSurface',com,
     &                '   !-  OutsideFaceEnvironment'
                  else
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                'Surface',com,'   !-  Outside Boundary Condition'
                  endif
                  call zsid(ic2(icc),ie2(icc),ZSDES,ZSDESC,ZSDESS)
                  if(iver.le.20)then
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                ZSDES(1:lnblnk(ZSDES)),com,
     &                '   !- OutsideFaceEnvironment Object'
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                'NoSun',com,'   !- Sun Exposure'
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                'NoWind',com,'   !- Wind Exposure'
                    WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &                '   !- View Factor to Ground'
                  else
                    WRITE(ioout,'(4a)',iostat=ios,err=6) '    ',
     &                ZSDES(1:lnblnk(ZSDES)),com,
     &                '   !- Outside Boundary Condition Object'
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                'NoSun',com,'   !- Sun Exposure'
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                'NoWind',com,'   !- Wind Exposure'
                    WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &                '   !- View Factor to Ground'
                 endif
                elseif(ICT(icc).eq.4.or.ICT(icc).eq.6)then
                  if(iver.le.20)then
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                'Ground',com,'   !-  OutsideFaceEnvironment'
                    WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &                '   !- OutsideFaceEnvironment Object'
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'NoSun',
     &                com,'   !- Sun Exposure'
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                'NoWind',com,'   !- Wind Exposure'
                    WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &                '   !- View Factor to Ground'
                  else
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                'Ground',com,'   !-  Outside Boundary Condition'
                    WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &                '   !- Outside Boundary Condition Object'
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'NoSun',
     &                com,'   !- Sun Exposure'
                    WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                'NoWind',com,'   !- Wind Exposure'
                    WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &                '   !- View Factor to Ground'
                  endif
                endif
              else

C There is a parent for this surface.
                if(iver.le.20)then
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'DOOR',
     &              com,'   !- Surface Type!'
                else
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'Door',
     &              com,'   !- Surface Type!'
                endif
                if(iver.le.20)then
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &              SSMLCN(icc)(1:lnblnk(SSMLCN(icc))),com,
     &              '   !- Construction Name of the Surface'
                else
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &              SSMLCN(icc)(1:lnblnk(SSMLCN(icc))),com,
     &              '   !- Construction Name'
                endif
                lsn0=lnblnk(SSPARENT(icc))
                lzn0=lnzname(mz)
                WRITE(ZSDES,'(a,a1,a)',iostat=ios,err=6) 
     &            SSPARENT(icc)(1:lsn0),':',zname(mz)(1:lzn0)
                if(iver.le.20)then
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &              ZSDES(1:lnblnk(ZSDES)),com,
     &              '   !- Base Surface Name'
                else
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &              ZSDES(1:lnblnk(ZSDES)),com,
     &              '   !- Building Surface Name'
                endif

C If external environment (external door) then OutsideFaceEnvironment Object must remain blank
C Otherwise give name of the other side surface. (suggested by Georgios)
                if(ICT(icc).eq.0)then
                  if(iver.le.20)then
                    WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &                '   !- OutsideFaceEnvironment Object'
                  else
                    WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &                '   !- Outside Boundary Condition Object'
                  endif
                else
                  if(ic2(icc).eq.0.or.ie2(icc).eq.0)then  ! if unknown zone & surface
                    if(iver.le.20)then
                      WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &                  '   !- OutsideFaceEnvironment Object'
                    else
                      WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &                  '   !- Outside Boundary Condition Object'
                    endif
                  else  ! if known zone & surface
                    call zsid(ic2(icc),ie2(icc),ZSDES,ZSDESC,ZSDESS)
                    if(iver.le.20)then
                      WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                  ZSDES(1:lnblnk(ZSDES)),com,
     &                  '   !- OutsideFaceEnvironment Object'
                    else
                      WRITE(ioout,'(4a)',iostat=ios,err=6) indent,
     &                  ZSDES(1:lnblnk(ZSDES)),com,
     &                  '   !- Outside Boundary Condition Object'
                    endif
                  endif
                endif 
                if(iver.le.20)then
                  WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &              '   !- View Factor to Ground'
                  WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &              '   !-   Name of shading control'
                  WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &              '   !-   WindowFrameAndDivider Name'
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'1.0',com,
     &              '   !-   Multiplier'
                else
                  WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &              '   !- View Factor to Ground'
                  WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &              '   !-   Shading Control Name'
                  WRITE(ioout,'(3a)',iostat=ios,err=6) indent,com,
     &              '   !-   Frame and Divider Name'
                  WRITE(ioout,'(4a)',iostat=ios,err=6) indent,'1.0',
     &              com,'   !-   Multiplier'
                endif
              endif
            endif
          endif

C Vertex information. If there were more than 4 vertices, but
C not a wall with a hole in it then write them out.

C << move towards no space between coordinates >>

          if(NVER(IS).gt.4.and.NVER(IS).ne.10)then
            nvertex=iedgcnt
            if(iver.le.20)then
              WRITE(ioout,'(a,i2,2a)',iostat=ios,err=6) indent,nvertex,
     &          com,'   !- Number of Surface Vertex Groups'
            else
              WRITE(ioout,'(a,i2,2a)',iostat=ios,err=6) indent,nvertex,
     &          com,'   !- Number of Vertices'
            endif
            do 42 iss=1,nvertex
              if(iss.lt.nvertex)then
                WRITE(ioout,'(a,f10.4,a,f10.4,a,f10.4,2a)',
     &            iostat=ios,err=6) indent,COORD(iss,1),com,
     &            COORD(iss,2),com,COORD(iss,3),
     &            com,'   !- X Y Z [m]'
              else
                WRITE(ioout,'(a,f10.4,a,f10.4,a,f10.4,2a)',
     &            iostat=ios,err=6) indent,COORD(iss,1),com,
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
            if(iver.le.20)then
              WRITE(ioout,'(a,i1,2a)',iostat=ios,err=6) indent,nvertex,
     &          com,'   !- Number of Surface Vertex Groups'
            else
              WRITE(ioout,'(a,i1,2a)',iostat=ios,err=6) indent,nvertex,
     &          com,'   !- Number of Vertices'
            endif
            do 43 iss=1,nvertex
              if(iss.lt.nvertex)then
                WRITE(ioout,'(a,f10.4,a,f10.4,a,f10.4,2a)',
     &            iostat=ios,err=6) indent,X(JVN(is,iss)),com,
     &            Y(JVN(is,iss)),com,Z(JVN(is,iss)),
     &            com,'   !- X Y Z [m]'
              else
                WRITE(ioout,'(a,f10.4,a,f10.4,a,f10.4,2a)',
     &            iostat=ios,err=6) indent,X(JVN(is,iss)),com,
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
              WRITE(ioout,'(2a,i1,2a)',iostat=ios,err=6) indent,
     &          BLOCKNAME(mz,ib)(1:LNBLOCKNAME(mz,ib)),ibs,com,
     &          '   !- User Supplied Surface Name'
              WRITE(ioout,'(5a)',iostat=ios,err=6) indent,com,
     &          '   !- TransSchedShadowSurf'
              nvertex=4
              if(iver.le.20)then
                WRITE(ioout,'(a,i1,2a)',iostat=ios,err=6) indent,
     &            nvertex,com,'   !- Number of Surface Vertex Groups'
              else
                WRITE(ioout,'(a,i1,2a)',iostat=ios,err=6) indent,
     &            nvertex,com,'   !- Number of Vertices'
              endif
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
  14  continue
      call edisp(iuout,'Completed export of data to EnergyPlus.')

C Reports - report details and dxf for surfaces
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      if(iver.le.20)then
        WRITE(ioout,'(2a)',iostat=ios,err=2) 'Report',com
        WRITE(ioout,'(8a)',iostat=ios,err=2) indent,'Surfaces',com,
     &    'DETAILS',com,'Thick Polyline',sim,
     &    '   !- Type, Name and Specifications for reports'
      else
        WRITE(ioout,'(2a)',iostat=ios,err=2) '  Output:Reports',com
        WRITE(ioout,'(8a)',iostat=ios,err=2) indent,'Surfaces',com,
     &    'Details',com,'ThickPolyline',sim,
     &    '   !- Type, Name and Specifications for reports'
      endif
     
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      if(iver.le.20)then
        WRITE(ioout,'(2a)',iostat=ios,err=2) 'Report',com
        WRITE(ioout,'(8a)',iostat=ios,err=2) indent,'Surfaces',com,
     &    'DXF',com,'Thick Polyline',sim,
     &    '   !- Type, Name and Specifications for reports'
      else
        WRITE(ioout,'(2a)',iostat=ios,err=2) '  Output:Reports',com
        WRITE(ioout,'(8a)',iostat=ios,err=2) indent,'Surfaces',com,
     &    'DXF',com,'ThickPolyline',sim,
     &    '   !- Type, Name and Specifications for reports'
      endif

C Include directive for EnergyPlus to dump out additional diagnotics
C for geometry. 
      WRITE(ioout,'(a)',iostat=ios,err=2) ' '
      if(iver.le.20)then
        WRITE(ioout,'(5a)',iostat=ios,err=2) 'DIAGNOSTICS',com,
     &    'DisplayExtraWarnings',sim,'   !- Extra diagnostics'
      else
        WRITE(ioout,'(5a)',iostat=ios,err=2) '  Output:Diagnostics',
     &    com,'DisplayExtraWarnings',sim,'   !- Extra diagnostics'
      endif
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

