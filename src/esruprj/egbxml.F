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

C The file egbxml.F is a collection of support facilities for
C reading gbXML files to create esp-r model meta files:
C  silentxmlmodel: initial setup of an ESP-r model after
C    gbxml file has been scanned.
C  silentxmlread: parses a gbXML file and then invokes
C    .silentxmlmodel
C silentxmlzone creates a zone based on gbxml common blocks. It is
C   called within silentxmlread after silentxmlmodel called.
C Matchzone: given zonestring returns the index matching from array zn.
C Matchcoord: given X Y Z returns the index of matching szcoords.
C cyclejwn: takes a jvn array cycles to start at a vertex index.
C bkcyclejwn: takes a jvn array cycles to end at a vertex index.
C gbxPOINTTOLINE: determines distance from a 3D point to a 3D line.
C gbxSURLEHI: Determines the overall length and height of a gbXML surface.
C gbxdoor: wraps parent surface around a child surface which
C   has an edge along the parent boundary.
C gbxchild wraps parent surface around a child surface within it.

C ************* silentxmlmodel
C silentxmlmodel initial setup of an ESP-r model after
C    gbxml file has been scanned. 
C The parameters are:
C action (8 char) as follows:
C    'new' creates model folders based on information
C       in root and mpath parameters.
C    'within' assumes that the current folder will be used.
C << within not yet tested >>
C root   (32 char) the root name of the model
C mpath  (72 char) the path to the folder with cfg file.
C weather (32 char) the weather file name (no path included)
C simact (6 char) assessments to be carried out '------' if
C   none requested, otherwise follow pattern in edipv.F.

C Current functionality approximates that in emeta.F silentmodel.
C It does not generate an xml metrics file.

C << decide what else might be needed >>

      subroutine silentxmlmodel(action,root,mpath,weather,simact,ier)
#include "building.h"
#include "model.h"
#include "site.h"
#include "espriou.h"
#include "esprdbfile.h"
C esprdbfile.h supplies lclim.
#include "seasons.h"
C seasons.h provides typper and typsea
#include "ipvdata.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL

C Climate data.
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG

      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

C Calendar commons.
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      character calename*32,calentag*12,calendayname*32

C Simulation parameter sets.
      common/spfldes/spfdescr(MSPS)
      common/spflper/isstday(MSPS),isstmon(MSPS),isfnday(MSPS),
     &               isfnmon(MSPS)
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      INTEGER :: nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),scfdres(MSPS),sipvres
      character sblres*72,sflres*72,splres*72,smstres*72,
     &  selres*72,scfdres*72,sipvres*72

C Extended simulation parameters for each set.
      common/spfldats/isstupex(MSPS),isbnstepex(MSPS),ispnstepex(MSPS),
     &  issaveex(MSPS),isavghex(MSPS),iscfdactivate(MSPS),
     &  isicfdys(MSPS),isicfdyf(MSPS),
     &  scftims(MSPS),scftimf(MSPS)
      INTEGER :: isstupex,isbnstepex,ispnstepex,issaveex,isavghex
      INTEGER :: iscfdactivate      ! zero ignore domains
      INTEGER :: isicfdys,isicfdyf  ! CFD simulation start & finish days
      REAL :: scftims,scftimf       ! CFD simulation start & finish time

      character act*6,lipvdatf*72,cr*32,tag*3
      character spfdescr*30

C Passed in parameters.
      character action*8,root*32,mpath*72,menu*72
      character fs*1           ! file separator
      character weather*32     ! file name for the location (no path)
      character subpath*72
      character simact*6 ! action for creating assessments.

      character outs*124,OUTSTR*124
      logical unixok,XST,ok
      logical haveground ! set true if ground temp file has been found
      character inpxmlfl*144 !to write out the input.xml file
      character llclmdb*144
      integer llt,lndbp

      IER=0

C Determine operating system
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      subpath=' '
      haveground=.false.

C If the action is `new` then setup model folder(s) and the registration
C level configuration file via call to pregist in `sn` mode.
C If the action is `within` do not create new folders for the model.

C Debug
      if(unixok)then
        write(6,*)  'action ',action,' ',root
        write(6,*)  'menu ',modeltitle
        write(6,*)  'mpath ',mpath
        write(6,*)  'weather ',weather
        write(6,*)  'clm ',lclim
        write(6,*)  'assessments ',simact
      endif

      call edisp(iuout,'  ')
      if(action(1:3).eq.'new')then
        write(menu,'(a)') modeltitle(1:lnblnk(modeltitle))
        call pregist('sn',root,mpath,menu,ier)
      elseif(action(1:6).eq.'within')then

C Assume the model folders exist when calling pregist.
        write(menu,'(a)') modeltitle(1:lnblnk(modeltitle))
        call pregist('sw',root,mpath,menu,ier)
      endif

C If there is a line with the root name of a climate file look
C for a match in the current climate location and if there is
C one set the name of that file, scan it and find its site. If
C there is no weather file then set the default seasons.
      if(weather(1:4).eq.'none')then

C Set default early winter, spring, summer, autumn, late winter periods.
C The model will be getting the default climate file.
        CALL EDAY(9,1,ia1wins) 
        CALL EDAY(15,1,ia1winf)
        CALL EDAY(6,3,ia1sprs)
        CALL EDAY(12,3,ia1sprf)
        CALL EDAY(11,7,iasums)
        CALL EDAY(17,7,iasumf)
        CALL EDAY(2,10,ia2sprs)
        CALL EDAY(8,10,ia2sprf)
        CALL EDAY(20,11,ia2wins)
        CALL EDAY(26,11,ia2winf)

C Default season definitions.
        CALL EDAY(1,1,is1wins)
        CALL EDAY(28,2,is1winf)
        CALL EDAY(1,11,is2wins)
        CALL EDAY(31,12,is2winf)
        CALL EDAY(1,3,is1sprs)
        CALL EDAY(30,4,is1sprf)
        CALL EDAY(1,9,is2sprs)
        CALL EDAY(31,10,is2sprf)
        CALL EDAY(1,5,is1sums)
        CALL EDAY(31,8,is1sumf)
      endif

C Impose climate site data on the model.
C << this needs to be passed into >>
      sitelat=CLAT; sitelongdif=CLONG; IYEAR=ICYEAR

C Create a single spring simulation parameter set.
      nsset=1; isstup=5; isstupex(1)=5
      isbnstep=4; isbnstepex(1)=4
      ispnstep=1; ispnstepex(1)=1
      issave=4; issaveex(1)=4
      isavgh=0; isavghex(1)=0
      isstday(1)=6; isstmon(1)=3
      isfnday(1)=12; isfnmon(1)=3         
      write(spfdescr(1),'(a)') 'spr'
      LN=max(1,LNBLNK(cfgroot))
      WRITE(sblres(1),'(2A)')cfgroot(1:ln),'_spr.res'

C Write calendar information (just the default one for the moment)
      calename='standard weekday Sat Sun hol'
      nbdaytype=4
      nbcaldays(1)=0
      calentag(1)='weekdays'
      calendayname(1)='weekdays (all year)'
      nbcaldays(2)=0
      calentag(2)='saturday'
      calendayname(2)='Saturdays (all year)'
      nbcaldays(3)=0
      calentag(3)='sunday'
      calendayname(3)='Sundays (all year)'
      calentag(4)='holiday'
      calendayname(4)='holiday'; nbcaldays(4)=0
      calentag(5)='-'; calendayname(5)='-'; nbcaldays(5)=0
      calentag(6)='-'; calendayname(6)='-'; nbcaldays(6)=0
      calentag(7)='-'; calendayname(7)='-'; nbcaldays(7)=0
      calendayname(8)='-'; calentag(8)='-'; nbcaldays(8)=0
      calentag(9)='-'; calendayname(9)='-'; nbcaldays(9)=0
      calentag(10)='-'; calendayname(10)='-'; nbcaldays(10)=0
     
      do 542 ijd=1,365

C Assume 1 Jan is a holiday (users can change this later).
        if(ijd.eq.1)then
          icalender(ijd)=4
          nbcaldays(4)=nbcaldays(4)+1
        else

C For julian day ijd find month and day of month and day of week.
          call edayr(ijd,idayn,imthn)
          call eweekd(idayn,imthn,iyear,idwk)
          if(idwk.ge.1.and.idwk.le.5)then
            icalender(ijd)=1
            nbcaldays(1)=nbcaldays(1)+1
          elseif(idwk.eq.6)then
            icalender(ijd)=2
            nbcaldays(2)=nbcaldays(2)+1
          elseif(idwk.eq.7)then
            icalender(ijd)=3
            nbcaldays(3)=nbcaldays(3)+1
          endif
        endif
  542 continue
  
      CALL EMKCFG('-',IER)

C Other logic here....??

      return
      end


C ************* silentxmlzone
C silentxmlzone creates a zone based on gbxml common blocks. It is
C called within silentxmlread after silentxmlmodel has been called.
C In this version ask the user which room use to apply.
C IER=0 OK.
      subroutine silentxmlzone(ICOMP,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
#include "schedule.h"
      integer MSZ   ! number of zones array sizes, edit to
                    ! match MCOM in building.h
      PARAMETER (MSZ=82)

      integer lnblnk  ! function definition

C Parameters
      integer icomp    ! focus zone index

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/user/browse
      common/cctlnm/ctldoc,lctlf
      integer icascf
      common/cctl/icascf(mcom)

      common/SLNTFULL/ifullysilent
      integer ifullysilent !flag to drive the silent model creation without
                           !questions about the operation files data

C Tollerance for surface matching.
      real ANGCC  ! angle between surfaces tolerance
      real CACC   ! tolerance between vertices
      real DACC   ! tolerance along line
      real COGCC  ! tolerance between surface COG
      real SNACC  ! tolerance between surface areas
      integer IACC ! number of matching corners outside dist tolerance
      common/matching/ANGCC,CACC,DACC,COGCC,SNACC,IACC

C Global or bespoke room use.
      integer IGU
      common/roomuse/IGU

C Surface attributes (zone & surface based).
      character iszlname*16  ! surface long name attribute
      character iszname*12   ! surface short name attribute
      character iszotf*32    ! see SSOTF below
      character iszvfc*4     ! see SSVFC below
      character iszmlcn*32   ! see SSMLCN below
      character iszother*24  ! see SSOTHER below
      character iszuse*12    ! see SSUSE below
      character iszparent*12 ! see SSPARENT below
      character isztype*18   ! gbxml surface type
      COMMON/metasurf/iszlname(MCOM,MS),iszname(MCOM,MS),
     &  iszotf(MCOM,MS),iszmlcn(MCOM,MS),iszvfc(MCOM,MS),
     &  iszother(MCOM,MS,3),iszuse(MCOM,MS,2),iszparent(MCOM,MS),
     &  isztype(MCOM,MS)

C hasname (string array) the surface name for each surface (so fillsur does not wipe)
      character hasname*12
      dimension hasname(MS)  ! name to use for each surface
      DIMENSION SNA(MS),PAZI(MS),PELV(MS)
      integer jvn1(MV) ! to hold list of edges for inverting
      character fs*1
      character ctldoc*248,LCTLF*72,zd*64
      character DFILE*72,CFILE*72,OFILE*72,TFILE*72
      character sn*12,opt*12,outs*124,louts*496
      character act*1  ! q is quiet  - includes warnings
      LOGICAL browse,OKC,XST,unixok
      logical willneedtmc  !  if a tmc file will be required.
      logical bound        ! to test if polygons are bounded
      LOGICAL QUIET,closez
      logical anunknownmlc ! if true then there was an unknown MLC

      character choices(7)*32
      character defmlc*32  ! names of default MLC for Office
      dimension defmlc(13)

      integer ibcs      ! connection associated with base surface

C If browsing then user cannot create a new zone.
      if(browse)then
        call usrmsg('Cannot update model while in browse',
     &    'mode, you must `own` the model!','W')
        return
      endif

C Initialise flag
      ifullysilent=0
      anunknownmlc=.false.
      IGU=1  ! assume all rooms empty

C Fill array (should be in common).
      defmlc(1)='brk_blk_2012'  ! 1st is for exterior wall
      defmlc(2)='partition'     ! 2nd is for interior partition
      defmlc(3)='door_u1.5'     ! 3rd is for ext doors
      defmlc(4)='door'          ! 4th is for int doors
      defmlc(5)='dbl_glz'       ! 5th is for ext glazing
      defmlc(6)='single_glaz'   ! 6th is for int glazing
      defmlc(7)='Pitch_rf2013'  ! 7th is for sloped roof
      defmlc(8)='susp_floor'    ! 8th is for intermediate floor (from room below)
      defmlc(9)='grnd_floor'    ! 9th is for ground floor
      defmlc(10)='susp_ceil'    ! 10th is for internal dropped ceiling
      defmlc(11)='fictitious'   ! 11th is fictitious Air
      defmlc(12)='susp_flr_re'  ! 12th is for intermediate floor (from room above)
      defmlc(13)='earth_side'   ! 13th is concrete basement with adj earth

C Set folder separator (fs) to \ or / as required.
      willneedtmc=.false.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Creation of a new zone, begin with length of zone name.
      call tstamp('>','PRJ: new zone')
      lnzname(ICOMP)=lnblnk(zname(ICOMP))

C << Unix vs Windows needs to be updated >>
      if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
        WRITE(DFILE,'(2a)') zname(ICOMP)(1:lnzname(ICOMP)),'.geo'
        WRITE(CFILE,'(2a)') zname(ICOMP)(1:lnzname(ICOMP)),'.con'
        WRITE(TFILE,'(2a)') zname(ICOMP)(1:lnzname(ICOMP)),'.tmc'
        WRITE(OFILE,'(2a)') zname(ICOMP)(1:lnzname(ICOMP)),'.opr'
      else
        WRITE(DFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &    zname(ICOMP)(1:lnzname(ICOMP)),'.geo'
        WRITE(CFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &    zname(ICOMP)(1:lnzname(ICOMP)),'.con'
        WRITE(TFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &    zname(ICOMP)(1:lnzname(ICOMP)),'.tmc'
        WRITE(OFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &    zname(ICOMP)(1:lnzname(ICOMP)),'.opr'
      endif
      LGEOM(ICOMP)=DFILE

      write(zd,'(4a)')
     &  zname(ICOMP)(1:lnzname(ICOMP)),' auto-generated for shape ',
     &  shape(ICOMP)(1:lnblnk(shape(ICOMP))),' & attributes.'
      zdesc(ICOMP)=zd
      lnzdesc(ICOMP)=lnblnk(zd)  ! update the length of this string.

C Assume we are working with a poly shaped room.
      IER=0

C For a poly shaped zone use similar logic to reading geometry file.
C Instanciate nsur and ntv for use in escrot and filsur.
      NSUR=nbwalls(ICOMP)
      NZSUR(ICOMP)=nbwalls(ICOMP)
      NTV=nztv(icomp)
      DO 62 I=1,nztv(icomp)
        X(I)=szcoords(ICOMP,I,1)
        Y(I)=szcoords(ICOMP,I,2)
        Z(I)=szcoords(ICOMP,I,3)
   62 CONTINUE

C Remember first zone point as rotation point.
      rotateit(ICOMP,2)=X(1); rotateit(ICOMP,3)=Y(1)
      DO 10 I=1,NZSUR(ICOMP)
        NVER(I)=isznver(ICOMP,I)
        DO 12 KV=1,NVER(I)
          JVN(I,KV)=iszjvn(ICOMP,I,KV)
   12   CONTINUE
   10 CONTINUE
      IUZBASEA(icomp)=0
      IBASES(ICOMP,1)=NZSUR(ICOMP)  ! initial guess, need to check
      IZBASELIST(ICOMP)=1

C Begin with default assumptions for each surface then overwrite. The
C subroutine filsur updates variables in common block G5.
C Because filsur clears the surface name rememeber in hasname first.
      do 322 ICC=1,NZSUR(ICOMP)
        write(hasname(icc),'(a)') sname(icomp,icc)  ! remember surface name
 322  continue
      CALL FILSUR(ICOMP,0)

C Update the connection list.
      ICCC=NCON
      DO 32 ICC=1,NZSUR(ICOMP)
        ICCC=ICCC+1
        IC1(ICCC)=ICOMP
        IE1(ICCC)=ICC
        ICT(ICCC)=zboundarytype(icomp,icc,1)  ! use boundarytype
        IC2(ICCC)=zboundarytype(icomp,icc,2)
        IE2(ICCC)=zboundarytype(icomp,icc,3)

C Instanciate the surface constructions & VERT CEIL etc.
        write(sname(icomp,icc),'(a)') hasname(icc)
        write(smlcn(icc),'(a)') iszmlcn(icomp,icc)
        write(svfc(icc),'(a)') iszvfc(icomp,icc)

C Take derived surface use. If blank reset to a '-'
        write(suse(icc,1),'(a)') iszuse(icomp,icc,1)
        write(suse(icc,2),'(a)') iszuse(icomp,icc,2)
        if(suse(icc,1)(1:2).eq.'  ') suse(icc,1)='- '
        if(suse(icc,2)(1:2).eq.'  ') suse(icc,2)='- '

C Loop thru all of the known constructions to find the matching construction.
C Also set ssmlcindex for this surface.
        imatch=0
        ssmlcindex(iccc)=0  ! assume no matching MLC          
        do 5 ii=1,nmlc
          lnssmlc=lnblnk(SMLCN(icc))
          if(SMLCN(icc)(1:lnssmlc).eq.
     &       mlcname(ii)(1:lnmlcname(ii)))then
            imatch=ii
            ssmlcindex(iccc)=ii   ! remember MLC index     
          endif
  5     continue

C Take the optics from the ?? file. If there are unknown constructions
C mark an unknownmlc so construction files are not created.
        write(SOTF(icc),'(a)') iszotf(icomp,icc)
        if(iszotf(icomp,icc)(1:4).EQ.'OPAQ')then
          continue
        else
          willneedtmc=.true.
        endif
        if(imatch.eq.0) then
          anunknownmlc=.true.   ! mark that one MLC is unknown
        endif
        SPARENT(icc)='-'

C Use zboundarytype to instanciate sother & ssother << to be done >>
        IF(zboundarytype(icomp,icc,1).EQ.0)THEN
          SOTHER(icc,1)='EXTERIOR'
        ELSEIF(zboundarytype(icomp,icc,1).EQ.1)THEN
          SOTHER(icc,1)='SIMILAR'
        ELSEIF(zboundarytype(icomp,icc,1).EQ.2)THEN
          SOTHER(icc,1)='CONSTANT'
        ELSEIF(zboundarytype(icomp,icc,1).EQ.3)THEN
          SOTHER(icc,1)='ANOTHER'
        ELSEIF(zboundarytype(icomp,icc,1).EQ.4)THEN
          SOTHER(icc,1)='GROUND'
        ELSEIF(zboundarytype(icomp,icc,1).EQ.5)THEN
          SOTHER(icc,1)='ADIABATIC'
        ELSEIF(zboundarytype(icomp,icc,1).EQ.6)THEN
          SOTHER(icc,1)='BASESIMP'
        ELSEIF(zboundarytype(icomp,icc,1).EQ.7)THEN
          SOTHER(icc,1)='IDENT_CEN'
        ELSE

C Reset to represent UNKNOWN
          SOTHER(icc,1)='UNKNOWN'
        ENDIF
        write(SOTHER(icc,2),'(i2.2)') zboundarytype(icomp,icc,2)
        write(SOTHER(icc,3),'(i3.3)') zboundarytype(icomp,icc,3)

C Set connection based variables.
        SSNAME(iccc)=SNAME(icomp,icc)
        SSOTF(iccc)=SOTF(icc)
        SSMLCN(iccc)=SMLCN(icc)
        SSVFC(iccc)=SVFC(icc)
        SSOTHER(iccc,1)=SOTHER(icc,1)
        SSOTHER(iccc,2)=SOTHER(icc,2)
        SSOTHER(iccc,3)=SOTHER(icc,3)
        SSUSE(iccc,1)=SUSE(icc,1)
        SSUSE(iccc,2)=SUSE(icc,2)
        SSPARENT(iccc)=SPARENT(icc)
        IZSTOCN(icomp,icc)=iccc
   32 CONTINUE
      NCON=ICCC

      CTYPE(icomp)='GEN '
      NDP(ICOMP)=3
      IDPN(ICOMP,1)=0; IDPN(ICOMP,2)=0; IDPN(ICOMP,3)=0
      NZSUR(ICOMP)=NSUR  ! update nzsur() it is needed by zgupdate.
      NZTV(ICOMP)=NTV

C Update the G7 common blocks and then if assign ZBASEA.
      call zgupdate(0,ICOMP,ier)

C Locate the upper and lower Z in the zone.
      XMX=-1.E+7; YMX=-1.E+7
      XMN=1.E+7; YMN=1.E+7
      ZMX=-1.E+7; ZMN=1.E+7
      do iv = 1,nztv(icomp)
        XMN=AMIN1(XMN,szcoords(ICOMP,iv,1))
        YMN=AMIN1(YMN,szcoords(ICOMP,iv,2))
        ZMN=AMIN1(ZMN,szcoords(ICOMP,iv,3))
        XMX=AMAX1(XMX,szcoords(ICOMP,iv,1))
        YMX=AMAX1(YMX,szcoords(ICOMP,iv,2))
        ZMX=AMAX1(ZMX,szcoords(ICOMP,iv,3))
      enddo  ! of iv

C FLOR and CEIL check against geometric position within the zone.
C Now use information saved in zgupdate to make sure SVFC
C attribute are correct (e.g. surfaceType InteriorFloor might
C be a ceiling.
      CALL ZINFO(SNA,ZOA,PAZI,PELV,ZVOL,'q')
      do iSurface=1,NSUR
        ioc=IZSTOCN(icomp,iSurface)  ! recover connection
        if(PELV(iSurface).GE.-1.5.AND.PELV(iSurface).LE.1.5)then
          if(SVFC(iSurface).NE.'VERT')then
            SVFC(iSurface)='VERT'; SSVFC(ioc)='VERT'
          endif
        elseif(PELV(iSurface).GE.88.5.AND.PELV(iSurface).LE.91.5)then
          if(SVFC(iSurface).NE.'CEIL')then
            SVFC(iSurface)='CEIL'; SSVFC(ioc)='CEIL'
            write(iszmlcn(ICOMP,iSurface),'(a)')
     &        defmlc(8)(1:lnblnk(defmlc(8)))
            write(smlcn(iSurface),'(a)')
     &        defmlc(8)(1:lnblnk(defmlc(8)))
            ioc=IZSTOCN(ICOMP,iSurface)
            write(ssmlcn(ioc),'(a)') 
     &        defmlc(8)(1:lnblnk(defmlc(8)))
          endif
        elseif(PELV(iSurface).GE.-91.5.AND.
     &         PELV(iSurface).LE.-88.5)then
          if(SVFC(iSurface).NE.'FLOR')then
            SVFC(iSurface)='FLOR'; SSVFC(ioc)='FLOR'
            write(iszmlcn(ICOMP,iSurface),'(a)')
     &        defmlc(12)(1:lnblnk(defmlc(12)))
            write(smlcn(iSurface),'(a)')
     &        defmlc(12)(1:lnblnk(defmlc(12)))
            ioc=IZSTOCN(ICOMP,iSurface)
            write(ssmlcn(ioc),'(a)') 
     &        defmlc(12)(1:lnblnk(defmlc(12)))
          endif
        else
          if(SVFC(iSurface).NE.'SLOP')then
            SVFC(iSurface)='SLOP'; SSVFC(ioc)='SLOP'
          endif
        endif

C In some gbXML files surfaces of type InteriorFloor the orientation
C implied in the initial edge ordering may be inappropriate. For 
C ZINFO 'FLOR' check if most coordinates are new the min or max
C points in the zone.
        if(SVFC(iSurface)(1:4).EQ.'FLOR')then
          icount=0
          do iyy = 1,isznver(ICOMP,iSurface)   ! check distance
            item=iszjvn(ICOMP,iSurface,iyy)    ! UR of the child
            zitem=szcoords(ICOMP,item,3)
            call eclose(zitem,ZMN,0.02,closez)
            if(closez) icount=icount+1
          enddo
          if(icount.eq.0.and.(isznver(ICOMP,iSurface).gt.0))then

C With no matches we probably can invert the edge list.
            write(6,'(a,2i4,a,i4,a)') 'Floor surface ',ICOMP,iSurface,
     &        sname(ICOMP,iSurface)(1:lnblnk(sname(ICOMP,iSurface))),
     &        icount,' is not near the floor (so inverting it).'
            do iyy = 1,NVER(iSurface)
              jvn1(iyy)=JVN(iSurface,iyy)
            enddo
            JVN(iSurface,1)=jvn1(2)
            JVN(iSurface,2)=jvn1(1)
            do iyy = 3,NVER(iSurface)
              izz=NVER(iSurface)+3-iyy
              JVN(iSurface,iyy)=jvn1(izz)
            enddo
            write(louts,'(a,124i4)') 'inverted list is ',
     &        (JVN(iSurface,ii),ii=1,NVER(iSurface))
C            call edisp248(iuout,louts,100)
            SVFC(iSurface)='CEIL'; SSVFC(ioc)='CEIL'
            write(iszmlcn(ICOMP,iSurface),'(a)') 
     &        defmlc(8)(1:lnblnk(defmlc(8)))
            write(smlcn(iSurface),'(a)') 
     &        defmlc(8)(1:lnblnk(defmlc(8)))
            ioc=IZSTOCN(ICOMP,iSurface)
            write(ssmlcn(ioc),'(a)') 
     &        defmlc(8)(1:lnblnk(defmlc(8)))
          else
C            write(6,*) 'Floor surface ',ICOMP,iSurface,
C     &        sname(ICOMP,iSurface),icount,' is near the floor.'
          endif
        elseif(SVFC(iSurface)(1:4).EQ.'CEIL')then
          icount=0
          do iyy = 1,isznver(ICOMP,iSurface)   ! check distance
            item=iszjvn(ICOMP,iSurface,iyy)    ! UR of the child
            zitem=szcoords(ICOMP,item,3)
            call eclose(zitem,ZMX,0.02,closez)
            if(closez) icount=icount+1
          enddo
          if(icount.eq.0.and.(isznver(ICOMP,iSurface).gt.0))then

C With no matches we probably can invert the edge list.
            write(6,'(a,2i4,a,i4,a)') 'Ceil surface ',ICOMP,iSurface,
     &        sname(ICOMP,iSurface)(1:lnblnk(sname(ICOMP,iSurface))),
     &        icount,' is not near the ceiling (inverting it).'
            do iyy = 1,NVER(iSurface)
              jvn1(iyy)=JVN(iSurface,iyy)
            enddo
            JVN(iSurface,1)=jvn1(2)
            JVN(iSurface,2)=jvn1(1)
            do iyy = 3,NVER(iSurface)
              izz=NVER(iSurface)+3-iyy
              JVN(iSurface,iyy)=jvn1(izz)
            enddo
            write(louts,'(a,124i4)') 'inverted list is ',
     &        (JVN(iSurface,ii),ii=1,NVER(iSurface))
C            call edisp248(iuout,louts,100)
            SVFC(iSurface)='FLOR'; SSVFC(ioc)='FLOR'
            write(iszmlcn(ICOMP,iSurface),'(a)') 
     &        defmlc(12)(1:lnblnk(defmlc(12)))
            write(smlcn(iSurface),'(a)') 
     &        defmlc(12)(1:lnblnk(defmlc(12)))
            ioc=IZSTOCN(ICOMP,iSurface)
            write(ssmlcn(ioc),'(a)') 
     &        defmlc(12)(1:lnblnk(defmlc(12)))
          else
            continue
          endif
        endif
      enddo  ! of iSurface

C For poly shape loop to find the floor in order to set up the floor area
      do iSurface=1,NSUR
        if(SVFC(iSurface).eq.'FLOR')then
          IBASES(ICOMP,1)=iSurface
          ibcs=IZSTOCN(icomp,IBASES(ICOMP,1))
          ZBASEA(icomp)= SSNA(ibcs)
        endif
      enddo

C Save this to file before passing into the geometry editing facility.
C Use gversion 1.1.
      if(igupgrade.lt.2)then
        igupgrade=2
        gversion(icomp) =1.1
      endif
      call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
      IF(IER.EQ.1)THEN
        call usrmsg('Problem creating geometry file.','giving up','W')
        return
      ENDIF

      NCCODE(ICOMP)=ICOMP

C Update the G7 common blocks and then save cfg as well.
C Use the 's' parameter to emkcfg to avoid asking for cnn file name.
      call zgupdate(0,ICOMP,ier)
      CALL EMKCFG('s',IER)

C Make an initial call to ckvert to sort out any polygon edge rule clashes.
C Given that gbxml coordinates have lots of decimal places lets reset 
C the CACC values for fine tollerances. Do ckvert quietly while attempting
C repair of edges.
      CACC=0.002; ANGCC=1.0; DACC=1.0; COGCC=0.1; SNACC=0.1
      call ckvert(0,icomp,bound,iub,inv,'r',ier)
C Debug.
C      write(outs,*) 'icomp,bound,iub,inv,ier',
C     &  icomp,bound,iub,inv,ier
C      call edisp(iuout,outs)
C      write(6,*) outs(1:lnblnk(outs))

C If inv is > zero then call it a second time to deal with reversed surfaces.
      if(inv.gt.0)then
        call ckvert(0,icomp,bound,iub,inv,'r',ier)
        write(outs,*) 'icomp,bound,iub,inv,ier',
     &    icomp,bound,iub,inv,ier
        call edisp(iuout,outs)
        write(6,*) outs(1:lnblnk(outs))

C And if there are still unbound edges report.
        if(iub.gt.0)then
          write(outs,*) 'afer repair in z icomp,bound,iub,inv,ier',
     &      icomp,bound,iub,inv,ier
          call edisp(iuout,outs)
          write(6,*) outs(1:lnblnk(outs))
        endif
      endif

C Use edge checking logic to figure out parent - child. Update
C zone geometry as well as model cfg file.
      call surrel2('s',icomp,ier)
      call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
      CALL EMKCFG('-',IER)

C If no errors then proceed to create constructions files. The
C file name is already known, assume that attribution is complete
C so write the header of construction file so something exists and then
C call edcon to complete the process. If the file already exists
C it should be overwritten.
      if(anunknownmlc)then   ! if a MLC is unknown do not bother with edcon
        continue
      else
        IUF=IFIL+2
        LTHRM(ICOMP)=CFILE
        CALL EFOPSEQ(IUF,LTHRM(ICOMP),3,IER)
        WRITE(IUF,31,IOSTAT=IOS,ERR=146)
     &    zname(ICOMP)(1:lnzname(ICOMP)),CFILE(1:lnblnk(CFILE))
  31    FORMAT('# thermophysical properties of ',a,' defined in ',a,/,
     &  '# no of |air |surface(from geo)| multilayer construction',/,
     &  '# layers|gaps|  no.  name      | database name ')
        CALL ERPFREE(IUF,ISTAT)
      endif

      if(willneedtmc)then
        ITW(icomp)=1
        LTWIN(ICOMP)=TFILE
      endif
      if(anunknownmlc)then   ! if a MLC is unknown do not bother with edcon
        call usrmsg('Problem autogenerating zone construction',
     &    'files. A surface construction was unknown.','W')
      else
        QUIET=.true.
        CALL EDCON(0,iuout,ICOMP,QUIET,IER)  ! should auto create files.
        if(ier.ne.0)then
          call usrmsg('Problem autogenerating zone construction',
     &      'files. Please check','W')
        endif
      endif

C Ask if user wants to specify globally or on a zone-by-zone basis.
      if(IGU.eq.0)then

C Operational details for this zone need to be handled. Initial
C approach is to ask the user to select an existing pattern created
C via calls to initcaspattern.
        write(outs,'(3a)') 'In space ',zname(ICOMP)(1:lnzname(ICOMP)),
     &    ' please specify its use.'
        call edisp(iuout,outs)
        choices(1)='a nothing happens in the zone'
        choices(2)='b cellular office (1 occ)    '
        choices(3)='c open plan office (9 m2/occ)'
        choices(4)='d office corridor or stair   '
        choices(5)='e meeting room (3-6 occ) '
        choices(6)='f dining room (house 2-4 occ)'
        choices(7)='g lounge (house 2-3 occ)     '
        NIGU=7
        IIGU=-2
        CALL EMENU('Usage pattern for room',choices,NIGU,IIGU)
      endif

C Clear commons.
      DO 401 IDTY=1,NBDAYTYPE
        NAC(IDTY)=0
        NCAS(IDTY)=0
 401  CONTINUE

      if(IGU.eq.0)then
        iguu=iigu+7  ! add offset so call is the same as in prjfmk.
        call initcaspattern(icomp,iguu,ier)
      else
        iguu=igu+7  ! add offset so call is the same as in prjfmk.
        call initcaspattern(icomp,iguu,ier)
      endif

C Set the default labels, slot, type labels for this zone. Only set
C the lodslot to non-zero for the initial 3 casual gain slots.
      lodlabel(icomp,1)='Occupants'
      caskeytype(icomp,1)='people      '
      lodslot(icomp,1)=1; lodatr1(icomp,1)=0;  lodatr2(icomp,1)=0
      lodlabel(icomp,2)='Lights   '
      caskeytype(icomp,2)='lighting    '
      lodslot(icomp,2)=2; lodatr1(icomp,2)=0;  lodatr2(icomp,2)=0
      lodlabel(icomp,3)='SmallPower'
      caskeytype(icomp,3)='equipment   '  
      lodslot(icomp,3)=3; lodatr1(icomp,3)=0;  lodatr2(icomp,3)=0
      lodlabel(icomp,4)='Otherstuff'  
      caskeytype(icomp,4)='other       '  
      lodslot(icomp,4)=0; lodatr1(icomp,4)=0;  lodatr2(icomp,4)=0
      lodlabel(icomp,5)='Ann.El    '  
      caskeytype(icomp,5)='net_utilities'  
      lodslot(icomp,5)=0; lodatr1(icomp,5)=0;  lodatr2(icomp,5)=0
      lodlabel(icomp,6)='Metabolic ' 
      caskeytype(icomp,6)='dynamicpeople'  
      lodslot(icomp,6)=0; lodatr1(icomp,6)=0;  lodatr2(icomp,6)=0
      lodlabel(icomp,7)='N/A       '
      caskeytype(icomp,7)='-           '  
      lodslot(icomp,7)=0; lodatr1(icomp,7)=0;  lodatr2(icomp,7)=0
      ip3ver(icomp)=21   ! set version for testing 2.1

C Set file name and write it out.
      write(LPROJ(ICOMP),'(a)') OFILE(1:lnblnk(OFILE))
      IUO=IFIL+2
      CALL EMKOPER(IUO,LPROJ(ICOMP),ICOMP,IER)

C Update configuration file to know about the construction 
C and operation files.
      CALL EMKCFG('-',IER)

      RETURN

  146 if(IOS.eq.2)then
        CALL USRMSG(' No permission to write constructions',' ','W')
      else
        CALL USRMSG(' File write error in constructions',' ','W')
      endif
      return

      END


C ******************** SILENTXMLREAD
C SILENTXMLREAD reads a gbXML file. If act is 'v' then use
C verbose chatter, if 'vv' then very verbose otherwise
C minimal chatter. mpath directive says where to create
C the model.

       SUBROUTINE SILENTXMLREAD(IUNIT,LFILE,act,mpath,IER)
       integer MSZ   ! number of zones array sizes, edit to
                     ! match MCOM in building.h
       PARAMETER (MSZ=82)
#include "building.h"
#include "model.h"
#include "site.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "espriou.h"
C espriou.h provides currentfile.
#include "control.h"
#include "seasons.h"
C seasons.h required prior to ipvdata.h
C #include "ipvdata.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/FILEP/IFIL

C Data from site.h:
C groundrefl - External ground reflectivity.
C groundreflmonth  - (12) Monthly values of ground reflectivity
C groundreflmodel  - Type of ground reflectivity model
C           1= constant albedo
C           2= simple model (monthly albedo, number of days with snow on ground
C           3= advanced model (monthly albedo, snow depth read from file)
C snowgroundrefl - Snow reflectivity
C dayswithsnow - (12) Monthly values of number of days with snow on ground
C SNFNAM - *72 Name of the file containing hourly snow depth information
      COMMON/PREC8/SLAT,SLON
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG

C Obstruction blocks via geometry.h.

      common/dllerr/dllsubr,dllmesg
      character dllsubr*12,dllmesg*124  ! messages if errors in dll

C Simulation parameter sets.
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      INTEGER :: nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh

C IPV description via ipvdata.h.
      common/cctlnm/ctldoc,lctlf
      character LCTLF*72,CTLDOC*248

      integer icascf
      common/cctl/icascf(mcom)

C iaplic(1) toggle for shading; iaplic(2) toggle for insolation.
C   toggle for assessment where:
C   1 is 'all_applicable', 0 is manual selection of surfaces.
C nsurfcalc nb of shaded surfaces, lstsfcalc() list of applicable surfaces.
C nsurfinso nb of insolation sources, isurfinso() list of insolation sources.
      common/ishdirec/iaplic(MCOM,2),nsurfcalc(MCOM),lstsfcalc(MCOM,MS),
     &       nsurfinso(MCOM),isurfinso(MCOM,MS)

C Surface attributes (zone & surface based).
      character iszlname*16  ! surface long name attribute
      character iszname*12   ! surface short name attribute
      character iszotf*32    ! see SSOTF below
      character iszvfc*4     ! see SSVFC below
      character iszmlcn*32   ! see SSMLCN below
      character iszother*24  ! see SSOTHER below
      character iszuse*12    ! see SSUSE below
      character iszparent*12 ! see SSPARENT below
      character isztype*18   ! gbxml surface type
      COMMON/metasurf/iszlname(MCOM,MS),iszname(MCOM,MS),
     &  iszotf(MCOM,MS),iszmlcn(MCOM,MS),iszvfc(MCOM,MS),
     &  iszother(MCOM,MS,3),iszuse(MCOM,MS,2),iszparent(MCOM,MS),
     &  isztype(MCOM,MS)

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

C Tollerance for surface matching.
      real ANGCC  ! angle between surfaces tolerance
      real CACC   ! tolerance between vertices
      real DACC   ! tolerance along line
      real COGCC  ! tolerance between surface COG
      real SNACC  ! tolerance between surface areas
      integer IACC ! number of matching corners outside dist tolerance
      common/matching/ANGCC,CACC,DACC,COGCC,SNACC,IACC

      integer nbxmlchild ! number of child surfaces
      common/openings/nbxmlchild
      
      integer i_ctl_link  !flag to indicate the zone has a basic control
      common/gbxctl/i_ctl_link(MCOM),ht_Setpoint(MCOM),cl_Setpoint(MCOM)

C Global or bespoke room use.
      integer IGU
      common/roomuse/IGU

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      integer inumXMLmetrics !counts for the number of xml metrics defined

      CHARACTER OUTSTR*248,outs*124
      
      character*12 curzonenme,othzonename
      character*12 spname(MCOM) ! gbxml space name e.g. 1 Room
      character*12 cursurfname,nextsurfname,SN2 ! test zone name surf name
      character*18 curstype,nextsurftype,tmptype ! test surface type
      character*12 zn(MCOM)     ! gbxml zone name e.g. aim0024

      character act*2  ! verbosity requested.
      character simact*6 ! action for creating assessments.
      character*84 odir    ! where are folders
      integer nbz      ! number of zones detected
      integer icursurf,inextsurf ! surface focus in the current zone
      integer iothsurf ! for the other zone
      integer iothopsurf(20) ! array of children openings other surface
      
      integer icurzone ! zone focus e.g. AdjacentSpaceId spaceIdRef="aim0024"
      integer iothzone  ! for use with real partitions

C << Some of the above might be held in an array for multiple openings? >>

C Logicals for testing XML
      logical inlocation,inbuilding,inspace,insurface,inplanar
      logical inopening,iswindow,isdoor
      logical havepartition ! common surface between two zones
      logical havesimilar   ! similar boundary
      logical havenearside  ! do we know which zone we are in
      logical found  ! equivalent of similar on other side
      logical ptnflip,ptnopeningflip

      real RVX,RVY,RVZ
      integer match

C At the head of the file is a title and a description
C modeltitle (char 72) and modeldocblock (char 248) from model.h which can be
C used by scanning code to identify the contents of a silent file.
      character weather*32     ! file name for the location (no path)
      character*(*) lfile      ! name of the file
      character fs*1           ! file separator

C For passing to silentxmlmodel.
      character actions*8,root*32,mpath*72

C Same as local variable actions which is passed back to calling code.
      character silentreturndirec*8

      logical dll,unixok

      character szhasconstr*32
      dimension szhasconstr(MSZ,MS)  ! construction to use for each surface
      character szhasoptic*32
      dimension szhasoptic(MSZ,MS)  ! optics to use for each surface
      character defmlc*32  ! names of default MLC for Office
      dimension defmlc(13)
      character loutstr*248,PHRASE*360,WORD*48,STUFF*96,TAG*48,RSTR*124
      character MODE*4,errmsg*24
      
C Site related local variables.
      logical havesite       ! true if tokens included
      character hourlysnowfile*72  ! same as SNFNAM
      character lworking*144  ! for processing database file names
      logical havehourlysnowfile   ! true if set
      real hoursGTM   ! hours before or after GTM
      logical havesimparameters,havename
C     logical lmatch
      integer simstartup   ! use for isstup
      integer simzonetimestep ! use for isbnstep
      integer simplanttimestep ! use for ispnstep
      integer simsavelevel  ! use for issave
      real ht_Setpoint,cl_Setpoint !heating and cooling set-points
      integer isilentncf !silent decide how many control functions needed
      integer lsn  ! length of currentfile
      logical simxml,ok,dok,decr
      integer jvn1(MV) ! to hold list of opening edges
      integer idebg    ! debug level
      logical isSI     ! false if Units are Imperial
      logical isInch   ! false if Units are Feet
      logical wecanprocess    ! can we take surface complexity
      logical isadoor  ! if topology is like a door
      integer nbvertincurrent ! how many vertices are in current surface

      IER=0; idebg=1
      IFCFG=IFIL+5

C Determine operating system
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Set icomp to zero if there are no zones in the model.
      if(ncomp.eq.0)then
        icomp=0
      endif

C Get the current folder and display options to the user.
C first list any files with .cfg in name.
      odir=' '
      call usrdir(odir)

C Clear the sz array structures. The logic is to scan all of the
C zone details and then process the zones after *end is detected
C in the file.
      if(act(1:2).eq.'v ')then
        idebg=1
      elseif(act(1:2).eq.'vv')then
        idebg=2
      endif
      actions=' '
      root=' '
C      mpath=' '  ! passed now
      modeltitle='Imported from gbXML'
      modeldocblock='Imported from gbXML'
      weather=' '
      hourlysnowfile=' '
      simact='------'   ! assume no simulation directives

      groundreflmodel=0
      skyview=0.0; groundview=0.0; buildingview=0.0
      havesite=.false.
      havehourlysnowfile=.false.
      havesimparameters=.false.
      simstartup=0; simzonetimestep=0; simplanttimestep=0
      simsavelevel=0
      isilentncf=0
      ICYEAR=2016; CLAT=52.; CLONG=0.  ! initial guess
      simxml = .false.
      isSI=.true.  ! assume SI units
      isInch=.true.  ! but if Imperial assume Inches

C Test read of XML files and tokens. Clear local variables.
      inlocation=.false.; inbuilding=.false.; insurface=.false.
      inplanar=.false.; inspace=.false.; inopening=.false.
      havepartition=.false.; havesimilar=.false.
      iswindow=.false.; isdoor=.false.
      ptnflip=.false.; ptnopeningflip=.false.
      PHRASE='  '; WORD='  '; TAG='  '
      curzonenme='  '; othzonename='  '
      nbz=0; icursurf=0; iopensurf=0; iothopensurf=0; 
      icurzone=0; iothzone=0; inextsurf=0; iothsurf=0
      do loop=1,20
        iothopsurf(loop)=0
      enddo
      nbxmlchild=0
      match=0; n=0
      RVX=0.0; RVY=0.0; RVZ=0.0
      do loop=1,12
        defmlc(loop)='UNKNOWN'
      enddo

C For each of the possible ESP-r zones, clear values.
      do 41 iz=1,MCOM
        nzsur(loop)=0; nztv(iz) = 0
        zname(iz) = ' '; shape(iz) = ' '
        zn(loop)=' '; spname(loop)=' '
        nbwalls(iz) = 0
        rotateit(iz,1) = 0.0; rotateit(iz,2) = 0.0
        rotateit(iz,3) = 0.0; rotateit(iz,4) = 0.0
        rotateit(iz,5) = 0.0; rotateit(iz,6) = 0.0
        zorigin(iz,1) = 0.0; zorigin(iz,2) = 0.0
        zorigin(iz,3) = 0.0
        zsize(iz,1) = 0.0; zsize(iz,2) = 0.0
        zsize(iz,3) = 0.0
        znbglz(iz) = 0; znbdoor(iz) = 0; 
        znbmass(iz) = 0
        do 39 isu=1,MS
          iszlname(iz,isu)=' '; iszname(iz,isu)=' '
          isztype(iz,isu)=' '; iszotf(iz,isu)=' '
          iszuse(iz,isu,1)='- '; iszuse(iz,isu,2)='- '
          iszvfc(iz,isu)='UNKN'  ! will be sorted geometrically
          zboundarytype(iz,isu,1)=0
          zboundarytype(iz,isu,2)=0
          zboundarytype(iz,isu,3)=0
          sname(iz,isu)=' '
          zhasglaze(iz,isu)=0.0
          zhasdoor(iz,isu)=0.0
          isznver(iz,isu)=0
          do 54 ivu=1,MV
            iszjvn(iz,isu,ivu)=0
  54      continue
          iszother(iz,isu,1)=' '; iszother(iz,isu,2)=' ';
          iszother(iz,isu,3)=' '
  39    continue
  41  continue

C For each of the gbxml specific arrays, clear values.
      do 42 iz=1,MSZ
        nbobs(iz) = 0
        i_ctl_link(iz)=0
        ht_Setpoint(iz)= 0.0; cl_Setpoint(iz)= 0.0
        do 55 ibu=1,MB
          XOB(iz,ibu) = 0.0; YOB(iz,ibu) = 0.0; ZOB(iz,ibu) = 0.0
          DXOB(iz,ibu) = 0.0; DYOB(iz,ibu) = 0.0; DZOB(iz,ibu) = 0.0
          BANGOB(iz,ibu,1) = 0.0
          BANGOB(iz,ibu,2) = 0.0
          BANGOB(iz,ibu,3) = 0.0
          OPOB(iz,ibu) = 1.0
          BLOCKNAME(iz,ibu) = ' '
          LNBLOCKNAME(iz,ibu) = 0
          BLOCKMAT(iz,ibu) = ' '
          LNBLOCKMAT(iz,ibu) = 0
          BLOCKTYP(iz,ibu) = 'obs '
          do 56 ibe=1,8
            XBP(iz,ibu,ibe)=0.0
            YBP(iz,ibu,ibe)=0.0
            ZBP(iz,ibu,ibe)=0.0
  56      continue
  55    continue
        do 53 isu=1,MTV   ! clear the coordinates
          szcoords(iz,isu,1)=0.0
          szcoords(iz,isu,2)=0.0
          szcoords(iz,isu,3)=0.0
  53    continue
        do 52 isu=1,MS
          szhasconstr(iz,isu)=' '  ! clear MLC name and optics name
          szhasoptic(iz,isu)=' '
  52    continue
  42  continue

      NS=0     ! temporary array for counting surfaces.
      nsz=0
      weather='none'
      OUTSTR=' '

C Check if running in dll mode.
      call isadll(dll)

C Initialise gbXML file. and set currentfile.
      CALL EFOPSEQ(IUNIT,LFILE,1,IER)
      IF(IER.LT.0)THEN
        write(outs,'(3a)') 'gbXML file ',LFILE(1:lnblnk(LFILE)),
     &      ' could not be opened.'
        if(dll)then
          dllsubr='SILENTREAD'
          dllmesg=outs
          ier=2
          return
        else
          call edisp(iuout,outs)
          IER=1
          RETURN
        endif
      ENDIF
      write(currentfile,'(a)') LFILE(1:lnblnk(LFILE))

 442  call lstripc(IUNIT,loutstr,99,ND,1,'geo line 1',IER)
C Debug.
C      if(idebg.eq.2)write(6,'(a)') loutstr(1:lnblnk(loutstr))
      k=0
      call EGETXMLTAG(loutstr,K,PHRASE,'-','xml tag',ier)
C Debug.
C      write(6,'(3a,i3)') 'tag: ',PHRASE(1:lnblnk(PHRASE)),' k=',k

C Need to parse the head of the file beginning with <gbXML
      if(PHRASE(1:5).eq.'gbXML')then
        ihead=1
        KK=6   ! start just after 'gbXML'
        do while (ihead.ne.0)

C Read the next tag and quoted string.
          call EGETEQDQXML(PHRASE,KK,TAG,STUFF,'-','space tag',ier)
          write(6,*) 'tag is ',tag(1:lnblnk(tag)),' id= ',
     &      STUFF(1:lnblnk(STUFF))
          if(TAG(1:2).eq.'  ')then
            ihead=0
          elseif(TAG(1:18).eq.'xsi:schemaLocation')then
            continue
          elseif(TAG(1:9).eq.'xmlns:xsi')then
            continue
          elseif(TAG(1:11).eq.'xmlns:xhtml')then
            continue
          elseif(TAG(1:5).eq.'xmlns')then
            write(6,*) 'xmlns is ',
     &        STUFF(1:lnblnk(STUFF))
          elseif(TAG(1:15).eq.'temperatureUnit')then
            write(6,*) 'temperatureUnit is ',
     &        STUFF(1:lnblnk(STUFF))
            if(STUFF(1:1).eq.'F') isSI=.false.  ! Imperial units
          elseif(TAG(1:10).eq.'lengthUnit')then
            write(6,*) 'lengthUnit is ',
     &        STUFF(1:lnblnk(STUFF))
            if(STUFF(1:6).eq.'Inches')then
              isSI=.false.  ! Imperial units
              isInch=.true.
            endif
            if(STUFF(1:4).eq.'Feet')then
              isSI=.false.  ! Imperial units
              isInch=.false.
            endif
          elseif(TAG(1:8).eq.'areaUnit')then
            write(6,*) 'areaUnit is ',
     &        STUFF(1:lnblnk(STUFF))
            if(STUFF(1:10).eq.'SquareFeet') isSI=.false.  ! Imperial units
          elseif(TAG(1:10).eq.'volumeUnit')then
            write(6,*) 'volumeUnit is ',
     &        STUFF(1:lnblnk(STUFF))
            if(STUFF(1:9).eq.'CubicFeet') isSI=.false.  ! Imperial units
          elseif(TAG(1:20).eq.'useSIUnitsForResults')then
            write(6,*) 'useSIUnitsForResults is ',
     &        STUFF(1:lnblnk(STUFF))
          else
            write(outs,'(3a)') 'The xml header tag ',TAG,
     &        ' not recognized.'
            call edisp(iuout,outs)
            call edisp(iuout,STUFF)
            ihead=0
          endif
        end do
      endif
      if(PHRASE(1:8).eq.'Location')then

C Read lines from the xml file for data associated with the
C Location topic. Jump out when '/Location' found.
        inlocation=.true.  ! mark this is the current topic
        ilocation=1
        do while (ilocation.ne.0)
          call lstripc(IUNIT,loutstr,99,ND,1,'xml line',IER)
          k=0
          call EGETXMLTAG(loutstr,K,PHRASE,'-','xml tag',ier)
          if(PHRASE(1:9).eq.'StationId')then

C Decode IDType="WMO" within the StationId phrase.
            kk=10 
            call EGETEQDQXML(PHRASE,KK,TAG,STUFF,'-','idtyp',ier)
            if(idebg.ge.1)then
              write(6,*) 'tag is ',tag(1:lnblnk(tag)),' IDType= ',
     &        STUFF(1:lnblnk(STUFF))
            endif

C And then carry on from K in loutstr to get the value.
            CALL EGETWXML(loutstr,K,WORD,'W','StationId',IFLAG)
            write(6,'(2a)') 'stationid: ',WORD(1:lnblnk(WORD))
          elseif(PHRASE(1:19).eq.'ZipcodeOrPostalCode')then
            continue
          elseif(PHRASE(1:9).eq.'Elevation')then
            continue
          elseif(PHRASE(1:9).eq.'Longitude')then

C Decode Longitude
            call EGETWRXML(loutstr,K,RV,-180.,180.,'-','Longitude',IER)
            sitelongdif=RV; CLONG=RV
            write(6,'(a,f7.2)') 'Longitude: ',sitelongdif
          elseif(PHRASE(1:10).eq.'/Longitude')then
            continue
          elseif(PHRASE(1:8).eq.'Latitude')then

C Decode Latitude
            call EGETWRXML(loutstr,K,RV,-90.,90.,'-','Latitude',IER)
            sitelat=RV; CLAT=RV
            write(6,'(a,f7.2)') 'Latitude: ',sitelat
          elseif(PHRASE(1:9).eq.'/Latitude')then
            continue
          elseif(PHRASE(1:4).eq.'Name')then
            call EGETRMXML(loutstr,K,RSTR,'-','campus name',IER)
            write(6,*) 'the campus id is ',RSTR(1:lnblnk(RSTR))
          elseif(PHRASE(1:9).eq.'/Location')then
            inlocation=.false.   ! done with this set of topics
            ilocation=0
          else
            continue  ! for not-yet-dealt-with-tag 
          endif
        end do

      elseif(PHRASE(1:9).eq.'/Location')then
        inlocation=.false.
      elseif(PHRASE(1:14).eq.'BuildingStorey')then

C Everything in BuildingStorey can be skipped. Keep looping
C until the PHRASE is '/BuildingStorey'.
        istory=1
        do while (istory.ne.0)
          call lstripc(IUNIT,loutstr,99,ND,1,'xml story',IER)
          k=0
          call EGETXMLTAG(loutstr,K,PHRASE,'-','xml tag',ier)
          if(PHRASE(1:15).eq.'/BuildingStorey') istory=0
        end do

C      elseif(PHRASE(1:15).eq.'/BuildingStorey')then
C        continue
      elseif(PHRASE(1:8).eq.'Building')then
        inbuilding=.true.; WORD='  '; TAG='  '

C Decode buildingType="Office". Setup default constructions
C for this building type (to be generalized). And we might
C get id= as the first token.
        KK=9
        call EGETEQDQXML(PHRASE,KK,TAG,STUFF,'-','idtyp',ier)
        if(idebg.ge.1)then
          write(6,'(4a)') 'tag is ',tag(1:lnblnk(tag)),' Type= ',
     &      STUFF(1:lnblnk(STUFF))
        endif
        if(TAG(1:2).eq.'id')then
          call st2name(STUFF,SN2)   ! remove any spaces
          lsn=MIN0(lnblnk(SN2),32)  ! root name 32 char or less
          write(root,'(a)') SN2(1:lsn)
          write(6,*) 'the building id is ',root(1:lnblnk(root))
          call EGETEQDQXML(PHRASE,KK,TAG,STUFF,'-','idtyp',ier)
          if(idebg.ge.1)then
            write(6,'(4a)') 'tag is ',tag(1:lnblnk(tag)),' Type= ',
     &        STUFF(1:lnblnk(STUFF))
          endif
        endif
        if(STUFF(1:6).eq.'Office')then
          defmlc(1)='brk_blk_2012'  ! 1st is for exterior wall
          defmlc(2)='partition'     ! 2nd is for interior partition
          defmlc(3)='door_u1.5'     ! 3rd is for ext doors
          defmlc(4)='door'          ! 4th is for int doors
          defmlc(5)='dbl_glz'       ! 5th is for ext glazing
          defmlc(6)='single_glaz'   ! 6th is for int glazing
          defmlc(7)='Pitch_rf2013'  ! 7th is for sloped roof
          defmlc(8)='susp_floor'    ! 8th is for intermediate floor (from room below)
          defmlc(9)='grnd_floor'    ! 9th is for ground floor
          defmlc(10)='susp_ceil'    ! 10th is for internal dropped ceiling
          defmlc(11)='fictitious'   ! 11th is fictitious Air
          defmlc(12)='susp_flr_re'  ! 12th is for intermediate floor (from room above)
          defmlc(13)='earth_side'   ! 13th is concrete basement with adj earth
        endif

C Determine which PHRASE we are dealing with.
      elseif(PHRASE(1:9).eq.'/Building')then
        inbuilding=.false.   ! reset
      elseif(PHRASE(1:7).eq.'/Campus')then
        close(IUNIT)  ! got what we wanted echo debug.
        ncomp=nbz

C Occassionally for complex surfaces the last edge vertex index
C can be zero. Check if this is the case and decrement isznver.
        do loop=1,nbz
          do loop2=1,nzsur(loop)
            if(isznver(loop,loop2).gt.40)then
              loop3=isznver(loop,loop2)
              if(iszjvn(loop,loop3,loop3).eq.0)then
                isznver(loop,loop2)=isznver(loop,loop2)-1
                if(idebg.ge.1)then
                  write(6,*) '1254 decrementing zsv',loop,loop2,loop3
                endif
              endif
            endif
          enddo ! of loop2
        enddo   ! of loop

C Occassionally a vertex index may be zero. Loop through the
C data structure and if one is located remove it from the list
C and pack the list and decrement isznver.
        decr=.false.
        do loop=1,nbz
          do loop2=1,nzsur(loop)
            nvx=isznver(loop,loop2)
            do loop3=1,nvx
              if(iszjvn(loop,loop2,loop3).eq.0)then

C copy index of subsequent edges and then decrement isznver
C and then jump back and try again.
                do loop4=loop3,nvx-1
                  iszjvn(loop,loop2,loop4)=iszjvn(loop,loop2,loop4+1)
                enddo  ! of loop4
                isznver(loop,loop2)=isznver(loop,loop2)-1
                decr=.true.
              endif
            enddo  ! of loop3
            if(decr)then
              if(idebg.ge.1)then
                write(6,'(a,3i4,2x,124i4)') 'xml decr surf jvn ',
     &            loop,loop2,isznver(loop,loop2),
     &            (iszjvn(loop,loop2,j),J=1,isznver(loop,loop2))
              endif
              decr=.false.
            endif
          enddo    ! of loop2
        enddo      ! of loop
C Debug.
        write(6,'(a)') 'Summary....'
        write(6,'(a,i2,a,99a)') 'nb zones ',nbz,' their names ',
     &    (zn(J),J=1,nbz)
        write(6,'(a,i2,a,99a)') 'nb spaces ',nbz,' their names ',
     &    (spname(J),J=1,nbz)
        write(6,'(a,124i4)') ' surfs in each ',(nzsur(J),J=1,nbz)
        write(6,'(a,124i4)') ' verts in each ',(nztv(J),J=1,nbz)
        do loop=1,nbz

C Strip any blanks from zone name.
          call st2name(spname(loop),SN2)
          write(zname(loop),'(a)') SN2(1:12)  ! remember
          nbwalls(loop)=nzsur(loop)  ! for use in silentxmlzone
C         write(6,*) 'Vertices'
C          do loop4=1,nztv(loop)
C            write(6,*) szcoords(loop,loop4,1),szcoords(loop,loop4,2),
C     &        szcoords(loop,loop4,3),loop4
C          enddo
          if(idebg.ge.1)then
            write(6,'(a,124i4)') 'xml surf edges ary',
     &        (isznver(loop,J),J=1,nzsur(loop))
          endif
          do loop3=1,nzsur(loop)
            call st2name(iszlname(loop,loop3),SN2)   ! remove any spaces
            write(sname(loop,loop3),'(a)') SN2(1:12) ! truncate at 12 char
            if(idebg.ge.1)then
              write(6,'(a,i4,2x,124i4)') 'xml surf jvn ',
     &          isznver(loop,loop3),
     &          (iszjvn(loop,loop3,j),J=1,isznver(loop,loop3))
            endif
            write(6,'(20a)') 'surf ',iszlname(loop,loop3),' ',
     &        iszname(loop,loop3),' ',
     &        isztype(loop,loop3),' ',iszotf(loop,loop3)(1:12),' ',
     &        iszmlcn(loop,loop3)(1:12),' ',iszuse(loop,loop3,1),' ',
     &        iszuse(loop,loop3,2),' ',iszother(loop,loop3,1)(1:12),' ',
     &        iszother(loop,loop3,2)(1:4),' ',
     &        iszother(loop,loop3,3)(1:4)
          enddo
        enddo
        goto 1000  ! process what has been collected

      elseif(PHRASE(1:4).eq.'Name')then

C Based on the current topic decode XML name token. Note: there is an
C overlap between inbuilding and inspace.
        if(inlocation)then
          call EGETRMXML(loutstr,K,RSTR,'-','campus name',IER)
C          write(6,*) 'the campus id is ',RSTR(1:lnblnk(RSTR))
        elseif(inbuilding.AND.inspace)then
          call EGETRMXML(loutstr,K,RSTR,'-','space name',IER)
          if(.NOT.simxml)then  ! do not use with if simulationXML
            write(6,*) 'the space id is ',RSTR(1:lnblnk(RSTR))
            call st2name(RSTR,SN2)   ! remove any spaces
            lsn=MIN0(lnblnk(SN2),12)  ! zone names 12 char or less
            write(spname(nbz),'(a)') SN2(1:lsn)
          endif
        endif
        if(inbuilding.AND.(.NOT.inspace))then
          call EGETRMXML(loutstr,K,RSTR,'-','space name',IER)
          write(root,'(a)') RSTR(1:lnblnk(RSTR))
          write(6,*) 'the building id is ',root(1:lnblnk(root))
        endif
        if(insurface.AND.(.NOT.inopening))then

C Because there might have been an opening look through the existing
C short surface names in this zone for a match before assigning long name.
C Also grab the parent surface type to give to a possible partition.
          call EGETRMXML(loutstr,K,RSTR,'-','surf name',IER)
          lnsn=lnblnk(cursurfname)
          if(.NOT.havenearside)then

C If Surface <Name> before <AdjacentSpaceId then we will not yet
C know which zone to associate the string with. << ?? what to do >>
            goto 442  ! parse another line
          endif
          do loop2=1,nzsur(icurzone)
            if(iszname(icurzone,loop2)(1:lnsn).eq.
     &         cursurfname(1:lnsn))then
              call st2name(RSTR,SN2)   ! remove any spaces
              lsn=MIN0(lnblnk(SN2),16)  ! longer names 16 char or less
              write(iszlname(icurzone,loop2),'(a)')SN2(1:lsn)
              write(tmptype,'(a)') isztype(icurzone,loop2) ! remember

C Debug.
              if(idebg.ge.1)then
                write(6,'(a,2i3,5a)') 'surface',icurzone,loop2,
     &           ' long name: ',iszlname(icurzone,loop2),
     &           ' short name: ',iszname(icurzone,loop2),tmptype
                write(6,*) '  '
              endif
            endif
          enddo
          if(havepartition)then

C The long name of the surface in the other zone also needs to be
C established. Loop looking for currentsurfname. Also set partition
C other face surface type same as near surface.
            do loop2=1,nzsur(iothzone)
              if(iszname(iothzone,loop2)(1:lnsn).eq.
     &           cursurfname(1:lnsn))then
                call st2name(RSTR,SN2)   ! remove any spaces
                lsn=MIN0(lnblnk(SN2),16)  ! longer names 16 char or less
                write(iszlname(iothzone,loop2),'(a)')SN2(1:lsn)
                write(isztype(iothzone,loop2),'(a)') tmptype
C Debug
C               write(6,'(a,2i3,6a)') 'surface',iothzone,loop2,
C     &           ' long name: ',iszlname(iothzone,loop2),
C     &           ' short name: ',iszname(iothzone,loop2),
C     &           ' type: ',iszlname(iothzone,loop2)
               write(6,*) '  '
             endif
           enddo
          endif  
        endif
        if(inopening)then
          call EGETRMXML(loutstr,K,RSTR,'-','opening name',IER)
          lnsn=lnblnk(nextsurfname)
          do loop2=1,nzsur(icurzone)
            if(iszname(icurzone,loop2)(1:lnsn).eq.
     &         nextsurfname(1:lnsn))then
              call st2name(RSTR,SN2)   ! remove any spaces
              lsn=MIN0(lnblnk(SN2),16)  ! longer names 16 char or less
              write(iszlname(icurzone,loop2),'(a)')SN2(1:lsn)

C Debug.
              if(idebg.ge.1)then
                write(6,'(a,2i3,4a)') 'opening ',icurzone,loop2,
     &          ' long name: ',iszlname(icurzone,loop2),
     &          ' short name: ',iszname(icurzone,loop2)
                write(6,*) '  '
              endif
            endif
          enddo

C Inherit the parent surface iszother and increment to account for opening.
          if(havepartition)then
            lnsn=lnblnk(nextsurfname)
            do loop2=1,nzsur(iothzone)
              if(iszname(iothzone,loop2)(1:lnsn).eq.
     &           nextsurfname(1:lnsn))then
                call st2name(RSTR,SN2)   ! remove any spaces
                lsn=MIN0(lnblnk(SN2),16)  ! longer names 16 char or less
                write(iszlname(iothzone,loop2),'(a)')SN2(1:lsn)
                if(idebg.ge.1)then
                  write(6,'(a,2i3,4a)') 'ptn open ',iothzone,loop2,
     &             ' long name: ',iszlname(iothzone,loop2),
     &             ' short name: ',iszname(iothzone,loop2)
                  write(6,*) '  '
                 endif
              endif
            enddo  
          endif  ! havepartition
        endif    ! inopening

      elseif(PHRASE(1:11).eq.'Description')then

C Might be associated with a surface. Do nothing yet.
        call EGETRMXML(loutstr,K,RSTR,'-','address',IER)
        write(6,*) 'description is ',RSTR(1:lnblnk(RSTR))

      elseif(PHRASE(1:13).eq.'StreetAddress')then

        call EGETRMXML(loutstr,K,RSTR,'-','address',IER)
C        write(6,*) 'the street is ',RSTR(1:lnblnk(RSTR))

      elseif(PHRASE(1:13).eq.'SpaceBoundary')then

C Everything in SpaceBoundary can be skipped. Keep looping
C until the PHRASE is '/SpaceBoundary'.
        ispb=1
        do while (ispb.ne.0)
          call lstripc(IUNIT,loutstr,99,ND,1,'xml sp bndry',IER)
          k=0
          call EGETXMLTAG(loutstr,K,PHRASE,'-','xml tag',ier)
          if(PHRASE(1:14).eq.'/SpaceBoundary') ispb=0
        end do
C      elseif(PHRASE(1:14).eq.'/SpaceBoundary')then
C        if(inbuilding)then
C          continue
C        endif

      elseif(PHRASE(1:5).eq.'Space')then

C For a simulation export type of gbXML the phrase Space might
C be followed by:
C  zoneIdRef="w"  or lightScheduleIdRef="x" 
C or equipmentScheduleIdRef="y" or peopleScheduleIdRef="z"
C or conditionType="x" or buildingStoreyIdRef="x" or
C spaceType="xx" or id="q">

C Keep scanning until TAG is a blank string. For each of the known
C TAGs process.
        inspace=.true.; nbz=nbz+1
        havename=.false.
        simxml = .false.
        KK=6   ! start just after 'Space'
        ispace=1
        do while (ispace.ne.0)

C Read the next tag and quoted string.
          call EGETEQDQXML(PHRASE,KK,TAG,STUFF,'-','space tag',ier)
          if(idebg.ge.1)then
            write(6,*) 'tag is ',tag(1:lnblnk(tag)),' id= ',
     &        STUFF(1:lnblnk(STUFF))
          endif
          if(TAG(1:2).eq.'  ')then
            ispace=0
          elseif(TAG(1:19).eq.'buildingStoreyIdRef')then
C            write(6,*) 'buildingStoreyIdRef is ',
C     &        STUFF(1:lnblnk(STUFF))
          elseif(TAG(1:18).eq.'lightScheduleIdRef')then
            write(6,*) 'lightScheduleIdRef is ',
     &        STUFF(1:lnblnk(STUFF))
            simxml = .true.
          elseif(TAG(1:22).eq.'equipmentScheduleIdRef')then
            call EGETEQDQXML(PHRASE,KK,TAG,STUFF,'-','equip sch',ier)
            write(6,*) 'equipmentScheduleIdRef is ',
     &        STUFF(1:lnblnk(STUFF))
            simxml = .true.
          elseif(TAG(1:19).eq.'peopleScheduleIdRef')then
            write(6,*) 'peopleScheduleIdRef is ',
     &        STUFF(1:lnblnk(STUFF))
            simxml = .true.
          elseif(TAG(1:13).eq.'conditionType')then

C If a conditionType="HeatedAndCooled" use an ideal
C controller, if conditionType="Unconditioned" set to freefloat.
            write(6,*) 'conditionType is ',
     &        STUFF(1:lnblnk(STUFF))
            if(STUFF(1:15).eq.'HeatedAndCooled')then
              i_ctl_link(nbz)=1
              ht_Setpoint(nbz)= 18.0; cl_Setpoint(nbz)= 24.0
            elseif(STUFF(1:13).eq.'Unconditioned')then
              i_ctl_link(nbz)=0
              ht_Setpoint(nbz)= 0.0; cl_Setpoint(nbz)= 0.0
            endif
          elseif(TAG(1:9).eq.'spaceType')then
            write(6,*) 'spaceType is ',STUFF(1:lnblnk(STUFF))
C Notes: examples are "OfficeOpenPlan", "LaboratoryOffice",
C "FineMaterialWarehouse", "ActiveStorage"
          elseif(TAG(1:9).eq.'zoneIdRef')then
            write(6,*) 'zoneIdRef is ',STUFF(1:lnblnk(STUFF))
          elseif(TAG(1:2).eq.'id')then
            simxml = .false.
            call st2name(STUFF,SN2)   ! remove any spaces
            lsn=MIN0(lnblnk(SN2),12)  ! zone names 12 char or less
            write(zn(nbz),'(a)') SN2(1:lsn)
            lsn=MIN0(lnblnk(SN2),9)  ! 
            write(spname(nbz),'(2a,i2.2)') SN2(1:lsn),'_',nbz
            write(6,*) 'noticed zone ',nbz,' ',zn(nbz),' ',spname(nbz)
            havename=.true.
          else
            write(outs,'(3a)') 'The space type tag ',TAG,
     &        ' not recognized.'
            call edisp(iuout,outs)
            call edisp(iuout,STUFF)
            ispace=0
          endif
        end do

      elseif(PHRASE(1:6).eq.'/Space')then
        inspace=.false.   ! reset
      elseif(PHRASE(1:6).eq.'Volume')then

        call EGETWRXML(loutstr,K,RV,0.,10000.,'-','Volume',IER)
        if(.NOT.isSI) RV=RV*0.0283168
        write(6,'(a,f8.2)') 'Volume: ',RV

      elseif(PHRASE(1:14).eq.'PlanarGeometry')then
        if(insurface)then
          inplanar=.true.  ! starts edge list for a surface

C Reset a counter for how many coordinates we read for
C this surface so that if there is a problem we decrement
C some counters.
          nbvertincurrent=0
        endif
      elseif(PHRASE(1:15).eq.'/PlanarGeometry')then
        inplanar=.false.  ! stop paying attention to Coordinate
      elseif(PHRASE(1:8).eq.'PolyLoop')then
        continue
      elseif(PHRASE(1:9).eq.'/PolyLoop')then

C If we are working on a partition then the
C ordering of the other zone polygon should be inverted
C prior to doing doors and/or windows in the other side.

C << How to ensure inversion is only done once? >>

        if(havepartition)then
          if(idebg.eq.2)then
            write(6,*)'ptnflip ',ptnflip,' ptnopeningflip ',
     &        ptnopeningflip
          endif
          if(ptnflip)then
            continue   ! only do it once
          else
            n=isznver(iothzone,iothsurf)
            if(idebg.ge.1)then
              write(6,'(a,i3,124i4)') 'other ptn edges&list ',
     &          n,(iszjvn(iothzone,iothsurf,loop3),loop3=1,n)
            endif
            do iyy = 1,n   ! invert this list
              jvn1(iyy)=iszjvn(iothzone,iothsurf,iyy)
            enddo
            iszjvn(iothzone,iothsurf,1)=jvn1(2)
            iszjvn(iothzone,iothsurf,2)=jvn1(1)
            do iyy = 3,n
              izz=n+3-iyy
              iszjvn(iothzone,iothsurf,iyy)=jvn1(izz)
            enddo
            if(idebg.ge.1)then
              write(6,'(a,i3,124i4)') 'reversed other ptn edges&list ',
     &          n,(iszjvn(iothzone,iothsurf,loop3),loop3=1,n)
            endif
            ptnflip=.true.  ! set so no repeat
          endif

C And if there are openings in that other zone they should
C also be inverted.
          if(inopening)then
            if(ptnopeningflip)then
              continue   ! only do it once
            else
              if(nbxmlchild.ge.1.and.nbxmlchild.lt.21)then
                n=isznver(iothzone,iothopsurf(nbxmlchild))
                write(6,'(a,i3,124i4)') 'other ptn open edges&list',
     &            n,(iszjvn(iothzone,iothopsurf(nbxmlchild),loop3),
     &            loop3=1,n)
                do iyy = 1,n   ! invert this list
                  jvn1(iyy)=iszjvn(iothzone,iothopsurf(nbxmlchild),iyy)
                enddo
                iszjvn(iothzone,iothopsurf(nbxmlchild),1)=jvn1(2)
                iszjvn(iothzone,iothopsurf(nbxmlchild),2)=jvn1(1)
                do iyy = 3,n
                  izz=n+3-iyy
                  iszjvn(iothzone,iothopsurf(nbxmlchild),iyy)=jvn1(izz)
                enddo
                write(6,'(a,i3,124i4)') 
     &            'reversed other ptn open edges&list ',
     &            n,(iszjvn(iothzone,iothopsurf(nbxmlchild),loop3),
     &            loop3=1,n)
                ptnopeningflip=.true.  ! set so no repeat
              endif
            endif
          endif
        endif

C If we are dealing with a child process via gbxchild
C or gbxdoor
        if(inopening)then
          if(idebg.ge.1)then
            write(6,*) 'insurface ',insurface,' inplanar ',inplanar,
     &      ' havepartition ',havepartition,' inopening ',
     &      inopening,' iswindow ',iswindow,' isdoor ',isdoor,
     &      ' ptnflip ',ptnflip,' ptnopeningflip ',ptnopeningflip,
     &      nbxmlchild
          endif
          if(iswindow)then

C Logic for a window assumed to be within the parent surface.
            call gbxchild(icurzone,icursurf,iopensurf,'i')

C If this is a partition we would need to update the parent
C in the other zone.
           if(havepartition)then
             if(nbxmlchild.ge.1.and.nbxmlchild.lt.21)then
               write(6,*) 
     &         'There is also window in a partition to sort.',iothzone,
     &         iothsurf,iothopsurf(nbxmlchild),
     &         isznver(iothzone,iothopsurf(nbxmlchild))
C Needs testing.
               call gbxchild(iothzone,iothsurf,iothopsurf(nbxmlchild),
     &           'o')
             endif
           endif
          endif
          if(isdoor)then
  
C Process a child surface which has a shared edge with the parent.
C Need to signal working in other zone!
            call gbxisadoor(icurzone,icursurf,iopensurf,'i',isadoor)
            if(isadoor)then
              call gbxdoor(icurzone,icursurf,iopensurf,'i')
            else
              call gbxchild(icurzone,icursurf,iopensurf,'i')
            endif

            if(havepartition)then
              if(nbxmlchild.eq.1)then
                write(6,*) 
     &          'There is also door in a partition to sort.',
     &          iothzone,iothsurf,iothopsurf(nbxmlchild),
     &          isznver(iothzone,iothopsurf(nbxmlchild))
                call gbxisadoor(iothzone,iothsurf,
     &            iothopsurf(nbxmlchild),'o',isadoor)
                if(isadoor)then
            call gbxdoor(iothzone,iothsurf,iothopsurf(nbxmlchild),'o')
                else
            call gbxchild(iothzone,iothsurf,iothopsurf(nbxmlchild),'o')
                endif
              endif
            endif

C Useful block of code....
C For each of the coordinates associated with the door loop through
C those associated with the parent and find the two which touch a
C parent edge.
C            do loop1= 1,nopenjvn
C              ipoint=iopenjvn(loop1)
C              do loop2=1,isznver(icurzone,icursurf)-1
C                iwhich1=iszjvn(icurzone,icursurf,loop2)
C                iwhich2=iszjvn(icurzone,icursurf,loop2+1)
C                call gbxpointtoline(ipoint,icurzone,iwhich1,
C     &            iwhich2,offset,lmatch)
C                if(lmatch.and.offset.lt.0.01)then
C                  write(6,*) 'parent-door ',ipoint,icurzone,
C     &              iwhich1,iwhich2,offset,lmatch
C                endif
C              enddo
C            enddo
          endif  ! of isdoor
        endif

      elseif(PHRASE(1:14).eq.'CartesianPoint')then

C Pay attention if we are insurface & inplanar. Read 
C lines of <Coordinate> and jump when /CartesianPoint.
        if(insurface.and.inplanar)then

C Echo the current context toggles.
          nbvertincurrent=nbvertincurrent+1
          if(idebg.ge.1)then
            write(6,*) 'insurface ',insurface,' inplanar ',inplanar,
     &       ' havepartition ',havepartition,' inopening ',
     &       inopening,' iswindow ',iswindow,' isdoor ',isdoor,
     &       nbxmlchild,nbvertincurrent
          endif

C At this point the following might apply:
C a) we are dealing with facade so coords get registered once
C b) facade with an Opening so coords get registered twice and
C    the opening edges order reversed when included in parent. AND
C    if a door then need to exclude its base edge from the parent
C c) we are dealing with a real partition so coords registered twice
C    but only a single polygon is defined.  Two polygons will need to
C    be created one of which will need to have reversed edge ordering.
C d) real partition with Opening(s) so coords registered with a total
C    of 4 surfaces!

C Update the count of edges for this surface. If there is also
C an opening in it then increment for iopensurf. First check
C that the zone can take this additional complexity.

C << we should only do decr_b or decr_c decrementing nzsur()
C << what kind of logic would do this?
C << How about a logical wecandothis ?? >> 
          wecanprocess=.true.
          if(nztv(icurzone)+1.lt.MTV.and.
     &       isznver(icurzone,icursurf)+1.lt.MV)then
            continue  ! we can process this
          else
            wecanprocess=.false.
            write(6,'(a,2i4,a,2i4,3a,i3)')
     &        ' BEYOND ZONE vertex edge LIMITS in z ',
     &        icurzone,icursurf,' z verts & surf edges ',
     &        nztv(icurzone),isznver(icurzone,icursurf),' ',
     &        cursurfname,' ',nbvertincurrent
          endif
          if(inopening)then
            if(isznver(icurzone,iopensurf)+1.lt.MV)then
              continue
            else
              wecanprocess=.false.
              write(6,'(a,2i4,a,3i4)')
     &          ' BEYOND edge LIMITS in z ',
     &          icurzone,icursurf,' opening ',nztv(icurzone),
     &          isznver(icurzone,iopensurf),nbvertincurrent
            endif
          endif
          if(havepartition)then
            if(isznver(iothzone,iothsurf)+1.lt.MV)then
              continue
            else
              wecanprocess=.false.
              write(6,'(a,2i4,a,3i4)')
     &          ' BEYOND vertex or edge LIMITS in OTHER z s ',
     &          iothzone,iothsurf,' other & opening ',nztv(iothzone),
     &          isznver(iothzone,iothsurf),nbvertincurrent
            endif
          endif

          if(wecanprocess)then
            continue
          else

C Beyond complexity limits. Create a message name and jump
C to block of code that scans file until /Opening or /Surface
            errmsg='decr_bcd '
            goto 69
          endif

C          write(6,'(a,i3,a,2i4)')' incrementing edges in z ',
C     &      icurzone,' this & opening ',icursurf,iopensurf
          isznver(icurzone,icursurf)=isznver(icurzone,icursurf)+1
          if(inopening)then
            isznver(icurzone,iopensurf)=isznver(icurzone,iopensurf)+1
          endif

C Logic for havepartition may apply. So the isznver arrays for
C a parent surface in the other zone and a possible opening it it.
          if(havepartition)then
            if(idebg.ge.1)then
              write(6,'(a,i3,3a,2i3)')' have other partition ',
     &          iothzone,' ',othzonename,' surfs in other ',
     &          nzsur(iothzone),iothsurf
            endif
            isznver(iothzone,iothsurf)=isznver(iothzone,iothsurf)+1
            if(inopening)then
              if(nbxmlchild.ge.1.and.nbxmlchild.lt.21)then
                isznver(iothzone,iothopsurf(nbxmlchild))=
     &            isznver(iothzone,iothopsurf(nbxmlchild))+1
              endif
            endif
          endif

C Get a new set of coordinates.
          loop=0
          RVX=0.0; RVY=0.0; RVZ=0.0
 443      call lstripc(IUNIT,loutstr,99,ND,1,'X coord',IER)
          k=0
          call EGETXMLTAG(loutstr,K,PHRASE,'-','Coordinate',ier)
          if(PHRASE(1:10).eq.'Coordinate')then
            loop=loop+1
            if(loop.eq.1)then
              call EGETWRXML(loutstr,K,RVX,-180.,180.,'-','X',IER)
              if(.NOT.isSI)then
                if(isInch) RVX=RVX*0.0254
                if(.NOT.isInch) RVX=RVX*0.3048
              endif
            elseif(loop.eq.2)then
              call EGETWRXML(loutstr,K,RVY,-180.,180.,'-','X',IER)
              if(.NOT.isSI)then
                if(isInch) RVY=RVY*0.0254
                if(.NOT.isInch) RVY=RVY*0.3048
              endif
            elseif(loop.eq.3)then

C Got X Y & Z if this is the initial coord in the zone just add it
C otherwise test to see if it is unique.
              call EGETWRXML(loutstr,K,RVZ,-180.,180.,'-','X',IER)
              if(.NOT.isSI)then
                if(isInch) RVZ=RVZ*0.0254
                if(.NOT.isInch) RVZ=RVZ*0.3048
              endif
C Debug
              if(idebg.eq.2)then
                write(6,'(a,3f8.4)') 'X Y Z: ',RVX,RVY,RVZ
              endif
              if(nztv(icurzone).eq.0)then
                szcoords(icurzone,1,1)=RVX
                szcoords(icurzone,1,2)=RVY
                szcoords(icurzone,1,3)=RVZ
                nztv(icurzone)=nztv(icurzone)+1  ! increment
                iszjvn(icurzone,icursurf,1)=1
C Debug
                if(idebg.eq.2)then
                  write(6,'(a,2i4,3f8.3,a,2i3)') 
     &            'adding parent coords ',icurzone,
     &            icursurf,RVX,RVY,RVZ,' nztv ',
     &            NZTV(icurzone),nzsur(icurzone)
                endif
              else

C Logic similar to insert.F line 849. Check that we are
C within zone and surface limits.
                if(nztv(icurzone)+1.lt.MTV.and.
     &             isznver(icurzone,icursurf)+1.lt.MV)then
                  continue  ! we can process this
                else

C Beyond complexity limits. Create a message name and jump
C to block of code that scans file until /Opening or /Surface
                  write(6,'(a,i3,a,3i4)')
     &              ' BEYOND vertex or edge LIMITS in zb ',
     &              icurzone,' this & opening ',nztv(icurzone),
     &              isznver(icurzone,icursurf),nbvertincurrent
                  errmsg='decr_c '
                  goto 69
                endif
                found=.false.
                call matchcoord(RVX,RVY,RVZ,icurzone,found,match)

C If an existing vertex is close then reference it otherwise
C add a new vertex to the end of the zone list. The n is the
C current edge count i.e. the position where match is to be
C inserted.
                if(found)then
                  n=isznver(icurzone,icursurf)
                  iszjvn(icurzone,icursurf,n)=match
C Debug
                  if(idebg.eq.2)then
                    write(6,'(a,2i3,3f8.3,a,2i3)')'reuse coords ',
     &              icurzone,
     &              icursurf,RVX,RVY,RVZ,' at ',match,n
                  endif
                else
                  n=isznver(icurzone,icursurf)
                  NZTV(icurzone)=NZTV(icurzone)+1  ! increment
                  iszjvn(icurzone,icursurf,n)=NZTV(icurzone)
                  szcoords(icurzone,NZTV(icurzone),1)=RVX
                  szcoords(icurzone,NZTV(icurzone),2)=RVY
                  szcoords(icurzone,NZTV(icurzone),3)=RVZ
C Debug
                  if(idebg.eq.2)then
                    write(6,'(a,2i4,3f8.3,a,2i3)') 
     &              'adding parent coords ',icurzone,
     &              icursurf,RVX,RVY,RVZ,' nztv ',
     &              NZTV(icurzone),nzsur(icurzone)
                  endif
                endif
              endif

C Debug - write out the current edge list.
              if(idebg.eq.2)then
                write(6,'(a,3i4,a,124i4)') 'surf jvn a ',
     &           icurzone,icursurf,isznver(icurzone,icursurf),' ',
     &           (iszjvn(icurzone,icursurf,j),J=1,
     &           isznver(icurzone,icursurf))
              endif
              if(inopening)then

C If there is an opening repeat the logic but with iopensurf and icursurf.
                if(nztv(icurzone)+1.lt.MTV.and.
     &             isznver(icurzone,icursurf)+1.lt.MV)then
                  continue  ! we can process this
                else

C Beyond complexity limits. Create a message name and jump
C to block of code that scans file until /Opening or /Surface
                  write(6,'(a,i3,a,3i4)')
     &              ' BEYOND vertex or edge LIMITS in zc ',
     &              icurzone,' this & opening ',nztv(icurzone),
     &              isznver(icurzone,icursurf),nbvertincurrent
                  errmsg='decr_f '
                  goto 69
                endif
                found=.false.
                call matchcoord(RVX,RVY,RVZ,icurzone,found,match)

C If an existing vertex is close then reference it otherwise
C add a new vertex to the end of the zone list. Add its index to
C both the parent and opening surface if a window.
                if(found)then
                  n=isznver(icurzone,iopensurf)
                  iszjvn(icurzone,iopensurf,n)=match
                  if(iswindow.or.isdoor)then
                    n=isznver(icurzone,iopensurf)
                    iszjvn(icurzone,iopensurf,n)=match  ! ? shift to /PolyLoop ?
C Debug.
                    if(idebg.eq.2)then
                      write(6,'(a,2i3,3f8.3,a,2i3)') 
     &                'reuse coords for opening ',
     &                icurzone,iopensurf,RVX,RVY,RVZ,' at ',
     &                match,n
                    endif
                  endif
                else
                  n=isznver(icurzone,iopensurf)
                  NZTV(icurzone)=NZTV(icurzone)+1  ! increment
                  iszjvn(icurzone,iopensurf,n)=NZTV(icurzone)
C Debug.
                  if(idebg.eq.2)then
                    write(6,'(a,2i4,3f8.3,a,2i3)')
     &              'adding coords opening ',
     &              icurzone,iopensurf,RVX,RVY,RVZ,
     &              ' nztv ',NZTV(icurzone),n
                  endif
                  n=isznver(icurzone,iopensurf)
                  NZTV(icurzone)=NZTV(icurzone)+1  ! increment
                  iszjvn(icurzone,iopensurf,n)=NZTV(icurzone)
                  szcoords(icurzone,NZTV(icurzone),1)=RVX
                  szcoords(icurzone,NZTV(icurzone),2)=RVY
                  szcoords(icurzone,NZTV(icurzone),3)=RVZ
                endif

C Debug - write out the edge list for iopensurf.
C               write(6,'(a,2i4,124i4)') 'opening jvn b ',
C     &           icurzone,iopensurf,
C     &           (iszjvn(icurzone,iopensurf,j),J=1,
C     &            isznver(icurzone,iopensurf))

              endif

C Logic for havepartition may also apply here. We have already
C have iothsurf defined when we read AdjacentSpaceId (but we might
C not have read that token yet).
              if(havepartition)then
                if(idebg.eq.2)then
                  write(6,'(a,i2,1x,a,2i3)')' have other partition a',
     &             iothzone,othzonename,nzsur(iothzone),iothsurf
                endif

C The surface count in iothzone will already have a slot for this so use it.
                if(nztv(iothzone)+1.lt.MTV.and.
     &            isznver(iothzone,iothsurf)+1.lt.MV)then
                  continue  ! we can process this
                else

C Beyond complexity limits. Create a message name and jump
C to block of code that scans file until /Opening or /Surface
                  write(6,'(a,i3,a,3i4)')
     &              ' BEYOND vertex or edge LIMITS in zd ',
     &              iothzone,' this & opening ',nztv(iothzone),
     &              isznver(iothzone,iothsurf),nbvertincurrent
                  errmsg='decr_h '
                  goto 69
                endif

                found=.false.
                call matchcoord(RVX,RVY,RVZ,iothzone,found,match)
                if(found)then
                  n=isznver(iothzone,iothsurf)
                  iszjvn(iothzone,iothsurf,n)=match
C Debug.
                  if(idebg.eq.2)then
                    write(6,'(a,2i4,3f8.3,a,2i3)')
     &              'reuse coords a ',iothzone,
     &              iothsurf,RVX,RVY,RVZ,' at ',match,n
                  endif
                else
                  n=isznver(iothzone,iothsurf)
                  NZTV(iothzone)=NZTV(iothzone)+1  ! increment
                  iszjvn(iothzone,iothsurf,n)=NZTV(iothzone)
                  szcoords(iothzone,NZTV(iothzone),1)=RVX
                  szcoords(iothzone,NZTV(iothzone),2)=RVY
                  szcoords(iothzone,NZTV(iothzone),3)=RVZ
C Debug.
                  if(idebg.eq.2)then
                    write(6,'(a,2i4,3f8.3,a,2i3)') 
     &                'adding coords a ',iothzone,iothsurf,
     &                RVX,RVY,RVZ,' other nztv ',NZTV(iothzone),n
                  endif
                endif

C Debug - write out the other zone partition edge list.
                if(idebg.ge.1)then
                  write(6,'(a,2i4,124i4)') 'other ptn jvn ',
     &             iothzone,iothsurf,
     &             (iszjvn(iothzone,iothsurf,j),J=1,
     &             isznver(iothzone,iothsurf))
                endif

                if(inopening)then

C If the partition had an opening then check again.

C << Place to decrement surface counter >>

                  if(nztv(iothzone)+1.lt.MTV)then
                    if(nbxmlchild.ge.1.and.
     &           isznver(iothzone,iothopsurf(nbxmlchild))+1.ge.MV)then

C Beyond complexity limits. Create a message name and jump
C to block of code that scans file until /Opening or /Surface
                      write(6,'(a,i3,a,4i4)')
     &                  ' BEYOND vertex or edge LIMITS in ze ',
     &                  iothzone,' this & open ',nztv(iothzone),
     &                  isznver(iothzone,iothopsurf(nbxmlchild)),
     &                  nbxmlchild,nbvertincurrent
                      errmsg='decr_i '
                      goto 69
                    endif
                  else

C Beyond complexity limits. Create a message name and jump
C to block of code that scans file until /Opening or /Surface
                    write(6,'(a,i3,a,4i4)')
     &                ' BEYOND vertex or edge LIMITS in zf ',
     &                iothzone,' this & opening ',nztv(iothzone),
     &                isznver(iothzone,iothopsurf(nbxmlchild)),
     &                nbxmlchild,nbvertincurrent
                    errmsg='decr_k '
                    goto 69
                  endif
                  found=.false.
                  call matchcoord(RVX,RVY,RVZ,iothzone,found,match)
                  if(found)then
                    if(nbxmlchild.ge.1.and.nbxmlchild.lt.21)then
                      n=isznver(iothzone,iothopsurf(nbxmlchild))
                      iszjvn(iothzone,iothopsurf(nbxmlchild),n)=match
C Debug.
                      if(idebg.eq.2)then
                        write(6,'(a,2i4,3f8.3,a,3i3)') 
     &                  'reuse coords for opening b ',
     &                  iothzone,iothopsurf(nbxmlchild),
     &                  RVX,RVY,RVZ,' at ',match,n,nbxmlchild
                      endif
                    endif
                  else  ! focus on other zone opening in partition
                    if(nbxmlchild.ge.1.and.nbxmlchild.lt.21)then
                      n=isznver(iothzone,iothopsurf(nbxmlchild))
                      NZTV(iothzone)=NZTV(iothzone)+1   ! increment
                      iszjvn(iothzone,iothopsurf(nbxmlchild),n)=
     &                  NZTV(iothzone)
                    endif
                    szcoords(iothzone,NZTV(iothzone),1)=RVX
                    szcoords(iothzone,NZTV(iothzone),2)=RVY
                    szcoords(iothzone,NZTV(iothzone),3)=RVZ
C Debug.
                    if(nbxmlchild.ge.1)then
                      write(6,'(a,2i4,3f8.3,a,5i3)') 
     &                  'adding coords for opening b',iothzone,
     &                  iothopsurf(nbxmlchild),RVX,RVY,RVZ,' nztv ',
     &                  NZTV(iothzone),n,nzsur(iothzone),n,nbxmlchild
                    endif
                  endif

C Debug - write out the edge list for iopensurf.
                  if(nbxmlchild.ge.1.and.nbxmlchild.lt.21)then
                    write(6,'(a,2i4,124i4)') 
     &                'other opening jvn equiv',
     &                iothzone,iothopsurf(nbxmlchild),
     &                (iszjvn(iothzone,iothopsurf(nbxmlchild),j),J=1,
     &                isznver(iothzone,iothopsurf(nbxmlchild)))
                  endif
                endif  ! of opening
              endif    ! of havepartition
            endif
            goto 443
          elseif(PHRASE(1:15).eq.'/CartesianPoint')then
            goto 442  ! get another line
          endif
        endif

      elseif(PHRASE(1:15).eq.'/CartesianPoint')then
        if(inbuilding)then
          continue
        endif
      elseif(PHRASE(1:10).eq.'Coordinate')then
        if(inbuilding)then
          continue
        endif
      elseif(PHRASE(1:11).eq.'/Coordinate')then
        if(inbuilding)then
          continue
        endif
      elseif(PHRASE(1:13).eq.'ShellGeometry')then

C Everything in ShellGeometry can be skipped. Keep looping
C until the PHRASE is '/ShellGeometry'.
        ishell=1
        do while (ishell.ne.0)
          call lstripc(IUNIT,loutstr,99,ND,1,'xml shell',IER)
          k=0
          call EGETXMLTAG(loutstr,K,PHRASE,'-','xml tag',ier)
          if(PHRASE(1:14).eq.'/ShellGeometry') ishell=0
        end do
  
      elseif(PHRASE(1:7).eq.'Surface')then

C Depending on the source a Surface tag can be followed by:
C surfaceType="x", constructionIdRef="y",exposedToSun="z",
C id="zz". Somewhere in the definition will be a AdjacentSpaceId
C tag (untill we have that we cannot assign zone or surface
C attributes so set haveadjacentspaceid=.FALSE.
        havename=.false.
        insurface=.true.
        havenearside=.false.
        KK=8     ! start just after 'Surface'
        isurf=1  ! for while loop
        do while (isurf.ne.0)

C Read the next tag and quoted string.
          call EGETEQDQXML(PHRASE,KK,TAG,STUFF,'-','surf tag',ier)
          if(idebg.eq.2)then
            write(6,*) 'tag is ',tag(1:lnblnk(tag)),' id= ',
     &        STUFF(1:lnblnk(STUFF))
          endif
          if(TAG(1:2).eq.'  ')then
            isurf=0
          elseif(TAG(1:17).eq.'constructionIdRef')then
            write(6,*) 'constructionIdRef is ',STUFF(1:lnblnk(STUFF))
          elseif(TAG(1:12).eq.'exposedToSun')then
C            write(6,*) 'exposedToSun is ',STUFF(1:lnblnk(STUFF))
          elseif(TAG(1:11).eq.'surfaceType')then
C            write(6,*) 'surfaceTyp is ',STUFF(1:lnblnk(STUFF))
            write(curstype,'(a)')STUFF(1:lnblnk(STUFF))

C We do not yet have a context so curstype will be used later
C to set assumptions about composition and boundary conditions.
            if(curstype(1:4).eq.'Roof')then
              continue
            elseif(curstype(1:12).eq.'ExteriorWall')then
              continue
            elseif(curstype(1:15).eq.'UndergroundWall')then
              continue
            elseif(curstype(1:15).eq.'UndergroundSlab')then
              continue
            elseif(curstype(1:18).eq.'UndergroundCeiling')then
              continue
            elseif(curstype(1:12).eq.'ExposedFloor')then
              continue
            elseif(curstype(1:11).eq.'RaisedFloor')then
              continue
            elseif(curstype(1:3).eq.'Air')then
              continue
            elseif(curstype(1:5).eq.'Shade')then

C Surfaces of type Shade are not currently used so we can loop
C till we get to /Surface and then jump back to read another XML.
C              write(6,*) 'surf is Shade (not yet used)'
              insurface=.false. ! lets try to ignore Shade for now
              ishade=1  ! for while loop
              do while (ishade.ne.0)
                call lstripc(IUNIT,loutstr,99,ND,1,'xml shade',IER)
                k=0
                call EGETXMLTAG(loutstr,K,PHRASE,'-','xml tag',ier)
                if(PHRASE(1:8).eq.'/Surface') ishade=0
              end do
              goto 442  ! loop to read another line from the XML file.
            elseif(curstype(1:12).eq.'InteriorWall')then
              continue
            elseif(curstype(1:13).eq.'InteriorFloor')then
              continue
            elseif(curstype(1:11).eq.'SlabOnGrade')then
              continue
            elseif(curstype(1:7).eq.'Ceiling')then
              continue
            else

C Fall through if the surface type is not currently dealt with.
              write(outs,'(3a)') 'The surface type tag ',curstype,
     &          ' NOT RECOGNIZED.'
              call edisp(iuout,outs)
              call edisp(iuout,PHRASE)
            endif
          elseif(TAG(1:2).eq.'id')then
            insurface=.true.  ! mark we are dealing with an actual surface
            call st2name(STUFF,SN2)   ! remove any spaces
            lsn=MIN0(lnblnk(SN2),12)  ! surface names 12 char or less
            write(cursurfname,'(a)') SN2(1:lsn)  ! remember
            write(6,*) 'current surfacename is ',cursurfname
            havename=.true.

C Debug. 
            if(cursurfname(1:10).eq."S0_1-SF_29")then
              write(6,*) 'pay attention'
            endif
          else

C Fall through if the surface type is not currently dealt with.
            write(outs,'(3a)') 'The tag ',TAG(1:lnblnk(TAG)),
     &        ' NOT RECOGNIZED.'
            call edisp(iuout,outs)
            call edisp(iuout,PHRASE)
          endif
        end do  ! of isurf

      elseif(PHRASE(1:8).eq.'/Surface')then

C Toggle off surface indicators.
        insurface=.false.;isdoor=.false.; iswindow=.false.
        if(havepartition)then
          havepartition=.false.  ! unset any active partition marker
          ptnflip=.false.; ptnopeningflip=.false.; nbxmlchild=0
          do loop=1,20
            iothopsurf(loop)=0
          enddo
        endif
      elseif(PHRASE(1:7).eq.'Opening')then

C Opening might be followed by openingType="OperableWindow", id="q"
C interiorShadeType="Operable", exteriorShadeType="Fixed"
C constructionIdRef="construction-30" openingType="NonSlidingDoor"
C windowTypeIdRef="xx"
        havename=.false.
        KK=8
        iopen=1
        do while (iopen.ne.0)
          call EGETEQDQXML(PHRASE,KK,TAG,STUFF,'-','opening',ier)
          if(idebg.ge.1)then
            write(6,*) 'tag is ',tag(1:lnblnk(tag)),' ',
     &        STUFF(1:lnblnk(STUFF))
          endif
          if(TAG(1:2).eq.'  ')then
            iopen=0
          elseif(TAG(1:17).eq.'interiorShadeType')then
            write(6,*) 'interiorShadeType is ',STUFF(1:lnblnk(STUFF))
          elseif(TAG(1:17).eq.'exteriorShadeType')then
            write(6,*) 'exteriorShadeType is ',STUFF(1:lnblnk(STUFF))
          elseif(TAG(1:17).eq.'constructionIdRef')then
            write(6,*) 'constructionIdRef is ',STUFF(1:lnblnk(STUFF))
          elseif(TAG(1:15).eq.'windowTypeIdRef')then
            write(6,*) 'windowTypeIdRef is ',STUFF(1:lnblnk(STUFF))
          elseif(TAG(1:11).eq.'openingType')then
C Decode openingType="x". We will use attribute later.
            write(6,*) 'openingType is ',STUFF(1:lnblnk(STUFF))
            inopening=.true.
            if(havepartition) nbxmlchild=nbxmlchild+1
            do loop3=1,MV  ! clear jvn1 arrays.
              jvn1(loop3)=0
            enddo
            write(nextsurftype,'(a)') STUFF(1:lnblnk(STUFF))
            if(nextsurftype(1:14).eq.'OperableWindow')then
              iswindow=.true.
            elseif(nextsurftype(1:11).eq.'FixedWindow')then
              iswindow=.true.
            elseif(nextsurftype(1:16).eq.'OperableSkylight')then
              iswindow=.true.
            elseif(nextsurftype(1:13).eq.'FixedSkylight')then
              iswindow=.true.
            elseif(nextsurftype(1:14).eq.'NonSlidingDoor')then
              isdoor=.true.
            elseif(nextsurftype(1:11).eq.'SlidingDoor')then
              isdoor=.true.
            elseif(nextsurftype(1:3).eq.'Air')then
              isdoor=.true.
            else
              write(6,*) 'Opening type: ',STUFF,' NOT RECOGNIZED.'
            endif
          elseif(TAG(1:2).eq.'id')then

C Parse name and then type.
            call st2name(STUFF,SN2)   ! remove any spaces
            lsn=MIN0(lnblnk(SN2),12)  ! surf name 12 char or less
            write(nextsurfname,'(a)') SN2(1:lsn)
            havename=.true.
            write(6,*) 'Opening name: ',nextsurfname,' via id=.'
          else
            write(outs,'(3a)') 'The opening tag ',TAG,
     &        ' not recognized.'
            call edisp(iuout,outs)
            call edisp(iuout,STUFF)
          endif
        end do  ! of iopen

C Process the attributes of this opening.
        if(nzsur(icurzone)+1.lt.MS)then
          continue
        else

C If beyond limits read lines until the tag is </Opening>
C and then jump back to 442.
          write(6,'(a,2i4)')' BEYOND surface LIMITS in z ',
     &      icurzone,nzsur(icurzone)+1
          errmsg='decr_a '
          goto 69
        endif

C Increment nzsur, nbwalls.
        nzsur(icurzone)=nzsur(icurzone)+1 ! increment
        nbwalls(icurzone)=nzsur(icurzone) ! for use in silentxmlzone
        iopensurf=nzsur(icurzone)         ! remember
        write(6,'(3a,3i4)') 'setting opening next type & name ',
     &    nextsurftype,nextsurfname,icurzone,icursurf,iopensurf
        write(iszname(icurzone,iopensurf),'(a)')
     &    nextsurfname(1:lnblnk(nextsurfname))
        write(isztype(icurzone,iopensurf),'(a)')
     &    nextsurftype(1:lnblnk(nextsurftype))

C For inclusion in the near side parent surface take on its boundary.
        if(nextsurftype(1:14).eq.'OperableWindow'.or.
     &     nextsurftype(1:11).eq.'FixedWindow'.or.
     &     nextsurftype(1:16).eq.'OperableSkylight'.or.
     &     nextsurftype(1:13).eq.'FixedSkylight')then

C Need to take on the parent surface boundary attributes.  
          if(idebg.ge.1)then
            write(6,*) 'Found window match ',cursurfname,
     &      ' in ',zn(icurzone),' z ',icurzone,
     &      ' s-in-z count ',nzsur(icurzone)
          endif
          write(iszotf(icurzone,iopensurf),'(a)') 'TRAN'
          write(iszmlcn(icurzone,iopensurf),'(a)')
     &      defmlc(5)(1:lnblnk(defmlc(5)))
          write(iszuse(icurzone,iopensurf,1),'(a)') 'WINDOW'
          write(iszuse(icurzone,iopensurf,2),'(a)') 'CLOSED'
          write(iszother(icurzone,iopensurf,1),'(a)')
     &      'EXTERIOR'
          write(iszother(icurzone,iopensurf,2),'(a)')'000'
          write(iszother(icurzone,iopensurf,3),'(a)')'000'
          zboundarytype(icurzone,iopensurf,1)=0
          zboundarytype(icurzone,iopensurf,2)=0
          zboundarytype(icurzone,iopensurf,3)=0
        elseif(nextsurftype(1:14).eq.'NonSlidingDoor'.or.
     &         nextsurftype(1:11).eq.'SlidingDoor'.or.
     &         nextsurftype(1:3).eq.'Air')then     
          if(idebg.ge.1)then
            write(6,*) 'Found door match ',cursurfname,
     &      ' in ',zn(icurzone),' z ',icurzone,
     &      ' s-in-z count ',nzsur(icurzone)
          endif
          write(iszotf(icurzone,iopensurf),'(a)') 'OPAQUE'
          write(iszmlcn(icurzone,iopensurf),'(a)') 
     &      defmlc(4)(1:lnblnk(defmlc(4)))
          write(iszuse(icurzone,iopensurf,1),'(a)') 'DOOR'
          write(iszuse(icurzone,iopensurf,2),'(a)') 'CLOSED'
          write(iszother(icurzone,iopensurf,1),'(a)')
     &      'EXTERIOR'
          write(iszother(icurzone,iopensurf,2),'(a)')'000'
          write(iszother(icurzone,iopensurf,3),'(a)')'000'
          zboundarytype(icurzone,iopensurf,1)=0
          zboundarytype(icurzone,iopensurf,2)=0
          zboundarytype(icurzone,iopensurf,3)=0
        else
          write(outs,'(3a)') 'The nextsurftype type ',
     &      nextsurftype,' NOT RECOGNIZED.'
          call edisp(iuout,outs)
        endif

C Check to see if opening is within a real partition, if so
C we need to add to near side zone and other side zone and
C increment the surface count in each.
        if(havepartition)then
          if(nzsur(iothzone)+1.lt.MS)then
            continue
          else

C << Place to decrement surface or opening list >>

            write(6,'(a,2i4)')
     &        ' BEYOND surface LIMITS in OTHER z ',
     &        iothzone,nzsur(iothzone)+1
            iopen=1
            do while (iopen.ne.0)
              call lstripc(IUNIT,loutstr,99,ND,1,'xml opening',IER)
              k=0
              call EGETXMLTAG(loutstr,K,PHRASE,'-','xml tag',ier)
              if(PHRASE(1:8).eq.'/Opening')then
                iopen=0; iopensurf=0
                inopening=.false.; iswindow=.false.
                isdoor=.false.
              endif
              if(PHRASE(1:8).eq.'/Surface')then
                iopen=0; insurface=.false.
                isdoor=.false.; iswindow=.false.
              endif
            end do
            goto 442  ! get another line
          endif

C Bookkeepping for opening in other zone.
          nzsur(iothzone)=nzsur(iothzone)+1 ! increment
          nbwalls(iothzone)=nzsur(iothzone)  ! for use in silentxmlzone
          iothopensurf=nzsur(iothzone)      ! remember
          write(6,'(a,5i4)') 'opening in THIS & OTHER is',
     &      icurzone,icursurf,iopensurf,iothzone,iothopensurf

C Since in a partition, overwrite the earlier assumption for icurzone,iopensurf
C (same logic applies for any opening type).
          write(iszother(icurzone,iopensurf,1),'(a)')zn(iothzone)
          write(iszother(icurzone,iopensurf,2),'(i3.3)')iothzone
          write(iszother(icurzone,iopensurf,3),'(i3.3)')iothopensurf
          zboundarytype(icurzone,iopensurf,1)=3
          zboundarytype(icurzone,iopensurf,2)=iothzone
          zboundarytype(icurzone,iopensurf,3)=iothopensurf
          if(idebg.ge.1)then
            write(6,'(5a,i3,a,i3,a,2i4)') 'Reset ptn opening ',
     &        nextsurfname,' in ',zn(icurzone),' z ',icurzone,
     &        ' s-in-z count ',nzsur(icurzone),' pt-to',iothzone,
     &        iothopensurf
          endif

C Now turn attention to the other zone and opening.
          write(iszname(iothzone,iothopensurf),'(a)')
     &      nextsurfname(1:lnblnk(nextsurfname))
          write(isztype(iothzone,iothopensurf),'(a)')
     &      nextsurftype(1:lnblnk(nextsurftype))
          if(nextsurftype(1:14).eq.'OperableWindow'.or.
     &       nextsurftype(1:11).eq.'FixedWindow'.or.
     &       nextsurftype(1:16).eq.'OperableSkylight'.or.
     &       nextsurftype(1:13).eq.'FixedSkylight')then

C Take on the parent surface boundary attributes   
            if(idebg.ge.1)then
              write(6,'(5a,i3,a,i3,a,2i4)') 'Found ptn window match ',
     &          nextsurfname,' in ',zn(iothzone),' z ',iothzone,
     &          ' s-in-z count ',nzsur(iothzone),' pt-bk-to',icurzone,
     &          iopensurf
            endif
            write(iszotf(iothzone,iothopensurf),'(a)') 'TRAN'
            write(iszmlcn(iothzone,iothopensurf),'(a)') 
     &        defmlc(5)(1:lnblnk(defmlc(5)))
            write(iszuse(iothzone,iothopensurf,1),'(a)') 'WINDOW'
            write(iszuse(iothzone,iothopensurf,2),'(a)') 'CLOSED'
            write(iszother(iothzone,iothopensurf,1),'(a)')zn(icurzone)
            write(iszother(iothzone,iothopensurf,2),'(i3.3)')icurzone
            write(iszother(iothzone,iothopensurf,3),'(i3.3)')iopensurf
            zboundarytype(iothzone,iothopensurf,1)=3
            zboundarytype(iothzone,iothopensurf,2)=icurzone
            zboundarytype(iothzone,iothopensurf,3)=iopensurf
          elseif(nextsurftype(1:14).eq.'NonSlidingDoor'.or.
     &           nextsurftype(1:11).eq.'SlidingDoor'.or.
     &           nextsurftype(1:3).eq.'Air')then     
            if(idebg.ge.1)then
              write(6,'(5a,i3,a,i3,a,2i4)') 'Found ptn door match ',
     &        nextsurfname,' in ',zn(iothzone),' z ',iothzone,
     &        ' s-in-z count ',nzsur(iothzone),' pt-bk-to',icurzone,
     &        iopensurf
            endif
            write(iszotf(iothzone,iothopensurf),'(a)') 'OPAQUE'
            write(iszmlcn(iothzone,iothopensurf),'(a)') 
     &        defmlc(4)(1:lnblnk(defmlc(4)))
            write(iszuse(iothzone,iothopensurf,1),'(a)') 'DOOR'
            write(iszuse(iothzone,iothopensurf,2),'(a)') 'CLOSED'
            write(iszother(iothzone,iothopensurf,1),'(a)')zn(icurzone)
            write(iszother(iothzone,iothopensurf,2),'(i3.3)')icurzone
            write(iszother(iothzone,iothopensurf,3),'(i3.3)')iopensurf
            zboundarytype(iothzone,iothopensurf,1)=3
            zboundarytype(iothzone,iothopensurf,2)=icurzone
            zboundarytype(iothzone,iothopensurf,3)=iopensurf
          endif
        endif
      elseif(PHRASE(1:8).eq.'/Opening')then

C Toggle off indicators.
        iopensurf=0
        inopening=.false.; iswindow=.false.; isdoor=.false.
C ?? should nbxmlchild be reset here?

      elseif(PHRASE(1:15).eq.'AdjacentSpaceId')then

C Test the context of the surface. If we are InteriorFloor or InteriorWall
C or Air then record curzonenme & then scan next input line to
C get the othzonename. Set indicators as needed.
        if(insurface)then

C If the current type is Shade skip it.
          if(curstype(1:5).eq.'Shade')then
            goto 442 ! read another line
          endif

C Decode spaceIdRef="aim0024" The 1st reference is for the inside face.
          KK=16
          call EGETEQDQXML(PHRASE,KK,TAG,STUFF,'-','spaceIdRef',ier)
          call st2name(STUFF,SN2)   ! remove any spaces
          lsn=MIN0(lnblnk(SN2),12)  ! zone names 12 char or less
          write(curzonenme,'(a)') SN2(1:lsn)
C Debug.
          if(idebg.ge.1)then
            write(6,*) 'tag is ',tag(1:lnblnk(tag)),' adj space ',
     &        curzonenme(1:lnblnk(curzonenme))
          endif

C Get the index of curzonenme (near side).
          call matchzone(curzonenme,ZN,nbz,icurzone,IER)
          write(6,*) 'near side zone index ',curzonenme,' ',nbz,
     &      icurzone,IER
          if(icurzone.lt.0) icurzone=icurzone*(-1)

C Can this zone take another surface?
          if(nzsur(icurzone)+1.lt.MS)then
            continue
          else

C If beyond limits read lines until the tag is </Surface>
C and then jump back to 442.
            write(6,'(a,2i4)')' BEYOND surface LIMITS in za ',
     &        icurzone,nzsur(icurzone)+1
            iopen=1
            do while (iopen.ne.0)
              call lstripc(IUNIT,loutstr,99,ND,1,'xml surf',IER)
              k=0
              call EGETXMLTAG(loutstr,K,PHRASE,'-','xml tag',ier)
              if(PHRASE(1:8).eq.'/Surface')then
                iopen=0; insurface=.false.
                isdoor=.false.; iswindow=.false.
                if(havepartition)then
                  havepartition=.false.  ! unset any active partition marker
                  ptnflip=.false.; ptnopeningflip=.false.; nbxmlchild=0
                  do loop=1,20
                    iothopsurf(loop)=0
                  enddo
                endif
              endif
            end do

C << Place to decrement surface list >>

            goto 442  ! get another line
          endif
C Debug.
C          write(6,'(a,124i4)') 'a surfs in each ',(nzsur(J),J=1,nbz)

          havenearside=.true.

C If we are dealing with an InteriorFloor or InteriorWall or Ceiling
C or Air there could be duplicate AdjacentSpaceId lines. Read next line and
C confirm that it is AdjacentSpaceId. Save token to othzonename
          if(curstype(1:13).eq.'InteriorFloor'.or.
     &       curstype(1:12).eq.'InteriorWall'.or.
     &       curstype(1:3).eq.'Air'.or.
     &       curstype(1:7).eq.'Ceiling')then
            call lstripc(IUNIT,loutstr,99,ND,1,'AdjacentSpaceId',IER)
            k=0
            call EGETXMLTAG(loutstr,K,PHRASE,'-','AdjacentSpace',ier)
            if(PHRASE(1:15).ne.'AdjacentSpaceId')then
              goto 442
            endif
            KK=16
            call EGETEQDQXML(PHRASE,KK,TAG,STUFF,'-',
     &        'other spaceIdRef',ier)
            call st2name(STUFF,SN2)   ! remove any spaces
            lsn=MIN0(lnblnk(SN2),12)  ! zone names 12 char or less
            write(othzonename,'(a)') SN2(1:lnblnk(SN2))
            if(idebg.ge.1)then
              write(6,*) 'tag is ',tag(1:lnblnk(tag)),' other space ',
     &          othzonename(1:lnblnk(othzonename))
            endif

C Get the index of othzonename.
            call matchzone(othzonename,ZN,nbz,iothzone,IER)
            write(6,*) 'other side zone index ',othzonename,' ',
     &        nbz,iothzone
            if(iothzone.lt.0) iothzone=iothzone*(-1)
            if(icurzone.eq.iothzone)then

C A similar-on-other-side case. Set indices and the surface attributes.
              if(nzsur(icurzone)+1.lt.MS)then
                continue
              else
                write(6,'(a,2i4)')' BEYOND surface LIMITS in z ',
     &            icurzone,nzsur(icurzone)+1
                iopen=1
                do while (iopen.ne.0)
                  call lstripc(IUNIT,loutstr,99,ND,1,'xml surf',IER)
                  k=0
                  call EGETXMLTAG(loutstr,K,PHRASE,'-','xml tag',ier)
                  if(PHRASE(1:8).eq.'/Surface')then
                    iopen=0; insurface=.false.
                    isdoor=.false.; iswindow=.false.
                    if(havepartition)then
                      havepartition=.false.  ! unset any active partition marker
                      ptnflip=.false.; ptnopeningflip=.false. 
                      nbxmlchild=0
                      do loop=1,20
                        iothopsurf(loop)=0
                      enddo
                    endif
                  endif
                end do
                goto 442  ! get another line
              endif

C << perhaps this will need to be rolled back if there is
C << an issue with zone or surface complexity

              icursurf=nzsur(icurzone)+1
              inextsurf=icursurf+1  ! if there is an opening
              write(iszname(icurzone,icursurf),'(a)')
     &          cursurfname(1:lnblnk(cursurfname))

C And assign the surface type and set zboundarytype indicies.
              zboundarytype(icurzone,icursurf,1)=1
              zboundarytype(icurzone,icursurf,2)=0
              zboundarytype(icurzone,icursurf,3)=0
              write(isztype(icurzone,icursurf),'(a)')
     &          curstype(1:lnblnk(curstype))
              if(curstype(1:11).eq.'SlabOnGrade')then
                zboundarytype(icurzone,icursurf,1)=4
                zboundarytype(icurzone,icursurf,2)=1
                zboundarytype(icurzone,icursurf,3)=0
              endif

C A similar-on-other-side case. Add the parent surface.

C << Perhaps this will need to be rolled back if complexity limits exceeded >>

              nzsur(icurzone)=nzsur(icurzone)+1 ! increment
              nbwalls(icurzone)=nzsur(icurzone)  ! for use in silentxmlzone
              write(6,'(5a,i3,a,i3)') 
     &         'Found similar-on-other-side match ',
     &         cursurfname,' in ',zn(icurzone),' z ',
     &         icurzone,' s-in-z count ',nzsur(icurzone)
              write(iszotf(icurzone,icursurf),'(a)') 'OPAQUE'
              if(curstype(1:13).eq.'InteriorFloor')then
                write(iszmlcn(icurzone,icursurf),'(a)') 
     &            defmlc(8)(1:lnblnk(defmlc(8)))
                write(iszuse(icurzone,iopensurf,1),'(a)') 'PARTN'
                write(iszuse(icurzone,iopensurf,2),'(a)') '- '
              endif
              if(curstype(1:12).eq.'InteriorWall')then
                write(iszmlcn(icurzone,icursurf),'(a)') 
     &            defmlc(2)(1:lnblnk(defmlc(2)))
                write(iszuse(icurzone,iopensurf,1),'(a)') 'PARTN'
                write(iszuse(icurzone,iopensurf,2),'(a)') '- '
              endif
              if(curstype(1:3).eq.'Air')then

C << why are we setting air to similar? It might be a partition >>

                write(iszmlcn(icurzone,icursurf),'(a)')
     &            defmlc(11)(1:lnblnk(defmlc(11)))
                write(iszuse(icurzone,iopensurf,1),'(a)') 'FICT'
                write(iszuse(icurzone,iopensurf,2),'(a)') 'OPEN'
                write(iszother(icurzone,icursurf,1),'(a)')
     &            'SIMILAR'
              endif
              write(iszother(icurzone,icursurf,2),'(a)')'000'
              write(iszother(icurzone,icursurf,3),'(a)')'000'
              havesimilar=.true.   ! similar-on-other-side.
               ! clear a name here??
              goto 442 ! read another line
            else

C We will also need a new surface in iothzone.       
              havepartition=.true. ! a true partition.
              write(6,'(2a,i2,2a,i2)')'true partition between ', 
     &          curzonenme,icurzone,' & ',othzonename,
     &          iothzone
              if(nzsur(icurzone)+1.lt.MS.and.
     &           nzsur(iothzone)+1.lt.MS)then
                continue
              else
                write(6,'(a,2i4)')' BEYOND surface LIMITS in z ',
     &            icurzone,nzsur(icurzone)+1,iothzone,nzsur(iothzone)+1

C << Place to decrement surface list >>

C << update to use while logic >>
                goto 442  ! get another line
              endif

C Now that we know which zone we can assign cursurfname and attributes.
              nzsur(icurzone)=nzsur(icurzone)+1 ! increment
              nbwalls(icurzone)=nzsur(icurzone)  ! for use in silentxmlzone
              icursurf=nzsur(icurzone)
              inextsurf=icursurf+1  ! if there is an opening
              iothsurf=nzsur(iothzone)+1   ! remember
              iothopsurf(1)=iothsurf+1  ! if there is an opening
              iothopsurf(2)=iothsurf+2  ! if there is an 2nd opening
              iothopsurf(3)=iothsurf+3  ! if there is an 3rd opening
              iothopsurf(4)=iothsurf+4  ! if there is an 4th opening
              iothopsurf(5)=iothsurf+5  ! if there is an 5th opening
              iothopsurf(6)=iothsurf+6  ! if there is an 6th opening
              iothopsurf(7)=iothsurf+7  ! if there is an 7th opening
              iothopsurf(8)=iothsurf+8  ! if there is an 8th opening
              iothopsurf(9)=iothsurf+9  ! if there is an 9th opening
              iothopsurf(10)=iothsurf+10  ! if there is an 10th opening
              iothopsurf(11)=iothsurf+11  ! if there is an 11th opening
              iothopsurf(12)=iothsurf+12  ! if there is an 12th opening
              iothopsurf(13)=iothsurf+13  ! if there is an 13th opening
              iothopsurf(14)=iothsurf+14  ! if there is an 14th opening
              iothopsurf(15)=iothsurf+15  ! if there is an 15th opening
              iothopsurf(16)=iothsurf+16  ! if there is an 16th opening
              iothopsurf(17)=iothsurf+17  ! if there is an 17th opening
              iothopsurf(18)=iothsurf+18  ! if there is an 18th opening
              iothopsurf(19)=iothsurf+19  ! if there is an 19th opening
              iothopsurf(20)=iothsurf+20  ! if there is an 20th opening
              if(idebg.ge.1)then
                write(6,*) 'Adj setting up ',icurzone,icursurf,
     &            inextsurf,iothsurf,iothopsurf(1),iothopsurf(2),
     &            iothopsurf(3),iothopsurf(4)
              endif
              write(iszname(icurzone,icursurf),'(a)')
     &          cursurfname(1:lnblnk(cursurfname))
              if(curstype(1:12).eq.'InteriorWall')then
                if(idebg.ge.1)then
                  write(6,'(5a,i3,a,i3)') 'Found partition match ',
     &            cursurfname,' in ',zn(icurzone),' z ',
     &            icurzone,' s-in-z count ',nzsur(icurzone)
                endif
                write(iszotf(icurzone,icursurf),'(a)') 'OPAQUE'
                write(iszmlcn(icurzone,icursurf),'(a)') 
     &            defmlc(2)(1:lnblnk(defmlc(2)))
                write(iszuse(icurzone,icursurf,1),'(a)') 'PARTN'
                write(iszuse(icurzone,icursurf,2),'(a)') '- '
                write(iszother(icurzone,icursurf,1),'(a)')
     &            othzonename
                write(iszother(icurzone,icursurf,2),'(i3.3)')
     &            iothzone
                write(iszother(icurzone,icursurf,3),'(i3.3)')
     &            iothsurf
                zboundarytype(icurzone,icursurf,1)=3
                zboundarytype(icurzone,icursurf,2)=iothzone
                zboundarytype(icurzone,icursurf,3)=iothsurf
              elseif(curstype(1:7).eq.'Ceiling')then
                if(idebg.ge.1)then
                  write(6,'(5a,i3,a,i3)') 'Found ceil partition match ',
     &            cursurfname,' in ',zn(icurzone),' z ',
     &            icurzone,' s-in-z count ',nzsur(icurzone)
                endif
                write(iszotf(icurzone,icursurf),'(a)') 'OPAQUE'
                write(iszmlcn(icurzone,icursurf),'(a)') 
     &            defmlc(8)(1:lnblnk(defmlc(8)))
                write(iszuse(icurzone,icursurf,1),'(a)') 'PARTN'
                write(iszuse(icurzone,icursurf,2),'(a)') '- '
                write(iszother(icurzone,icursurf,1),'(a)')
     &            othzonename
                write(iszother(icurzone,icursurf,2),'(i3.3)')
     &            iothzone
                write(iszother(icurzone,icursurf,3),'(i3.3)')
     &            iothsurf
                zboundarytype(icurzone,icursurf,1)=3
                zboundarytype(icurzone,icursurf,2)=iothzone
                zboundarytype(icurzone,icursurf,3)=iothsurf
              elseif(curstype(1:13).eq.'InteriorFloor')then

C For InteriorFloor.
                if(idebg.ge.1)then
                  write(6,'(5a,i3,a,i3)') 'Found int floor match ',
     &            cursurfname,' in ',zn(icurzone),' z ',
     &            icurzone,' s-in-z count ',nzsur(icurzone)
                endif
                write(iszotf(icurzone,icursurf),'(a)') 'OPAQUE'
                write(iszmlcn(icurzone,icursurf),'(a)') 
     &            defmlc(12)(1:lnblnk(defmlc(12)))
                write(iszuse(icurzone,icursurf,1),'(a)') 'PARTN'
                write(iszuse(icurzone,icursurf,2),'(a)') '- '
                write(iszother(icurzone,icursurf,1),'(a)')
     &            othzonename
                write(iszother(icurzone,icursurf,2),'(i3.3)')
     &            iothzone
                write(iszother(icurzone,icursurf,3),'(i3.3)')
     &            iothsurf
                zboundarytype(icurzone,icursurf,1)=3
                zboundarytype(icurzone,icursurf,2)=iothzone
                zboundarytype(icurzone,icursurf,3)=iothsurf

              elseif(curstype(1:3).eq.'Air')then
                if(idebg.ge.1)then
                  write(6,'(5a,i3,a,i3)') 'Found int air match ',
     &            cursurfname,' in ',zn(icurzone),' z ',
     &            icurzone,' s-in-z count ',nzsur(icurzone)
                endif
                write(iszotf(icurzone,icursurf),'(a)') 'OPAQUE'
                write(iszmlcn(icurzone,icursurf),'(a)') 
     &            defmlc(12)(1:lnblnk(defmlc(12)))
                write(iszuse(icurzone,icursurf,1),'(a)') 'PARTN'
                write(iszuse(icurzone,icursurf,2),'(a)') '- '
                write(iszother(icurzone,icursurf,1),'(a)')
     &            othzonename
                write(iszother(icurzone,icursurf,2),'(i3.3)')
     &            iothzone
                write(iszother(icurzone,icursurf,3),'(i3.3)')
     &            iothsurf
                zboundarytype(icurzone,icursurf,1)=3
                zboundarytype(icurzone,icursurf,2)=iothzone
                zboundarytype(icurzone,icursurf,3)=iothsurf
              endif

C And for the other zone InteriorWall.

C << Perhaps this will need to be undone if complexity limits exceeded. >>

              write(iszname(iothzone,iothsurf),'(a)')
     &          cursurfname(1:lnblnk(cursurfname))
              nzsur(iothzone)=nzsur(iothzone)+1 ! increment
              nbwalls(iothzone)=nzsur(iothzone)  ! for use in silentxmlzone
              if(curstype(1:12).eq.'InteriorWall')then
                if(idebg.ge.1)then
                  write(6,'(5a,i3,a,i3)') 'Found partition match ',
     &            cursurfname,' in ',zn(iothzone),' z ',
     &            iothzone,' s-in-z count ',nzsur(iothzone)
                endif
                write(iszotf(iothzone,iothsurf),'(a)') 'OPAQUE'
                write(iszmlcn(iothzone,iothsurf),'(a)')
     &            defmlc(2)(1:lnblnk(defmlc(2)))
                write(iszuse(iothzone,iothsurf,1),'(a)') 'PARTN'
                write(iszuse(iothzone,iothsurf,2),'(a)') '- '
                write(iszother(iothzone,iothsurf,1),'(a)')
     &            curzonenme
                write(iszother(iothzone,iothsurf,2),'(i3.3)')
     &            icurzone
                write(iszother(iothzone,iothsurf,3),'(i3.3)')
     &            icursurf
                zboundarytype(iothzone,iothsurf,1)=3
                zboundarytype(iothzone,iothsurf,2)=icurzone
                zboundarytype(iothzone,iothsurf,3)=icursurf
              elseif(curstype(1:7).eq.'Ceiling')then
                if(idebg.ge.1)then
                  write(6,'(5a,i3,a,i3)') 'Found ceil partition match ',
     &            cursurfname,' in ',zn(iothzone),' z ',
     &            iothzone,' s-in-z count ',nzsur(iothzone)
                endif
                write(iszotf(iothzone,iothsurf),'(a)') 'OPAQUE'
                write(iszmlcn(iothzone,iothsurf),'(a)')
     &            defmlc(8)(1:lnblnk(defmlc(8)))
                write(iszuse(iothzone,iothsurf,1),'(a)') 'PARTN'
                write(iszuse(iothzone,iothsurf,2),'(a)') '- '
                write(iszother(iothzone,iothsurf,1),'(a)')
     &            curzonenme
                write(iszother(iothzone,iothsurf,2),'(i3.3)')
     &            icurzone
                write(iszother(iothzone,iothsurf,3),'(i3.3)')
     &            icursurf
                zboundarytype(iothzone,iothsurf,1)=3
                zboundarytype(iothzone,iothsurf,2)=icurzone
                zboundarytype(iothzone,iothsurf,3)=icursurf
              elseif(curstype(1:13).eq.'InteriorFloor')then

C For InteriorFloor.
                if(idebg.ge.1)then
                  write(6,'(5a,i3,a,i3)') 'Found int floor match ',
     &            cursurfname,' in ',zn(iothzone),' z ',
     &            iothzone,' s-in-z count ',nzsur(iothzone)
                endif
                write(iszotf(iothzone,iothsurf),'(a)') 'OPAQUE'
                write(iszmlcn(iothzone,iothsurf),'(a)')
     &            defmlc(12)(1:lnblnk(defmlc(12)))
                write(iszuse(iothzone,iothsurf,1),'(a)') 'PARTN'
                write(iszuse(iothzone,iothsurf,2),'(a)') '- '
                write(iszother(iothzone,iothsurf,1),'(a)')
     &            curzonenme
                write(iszother(iothzone,iothsurf,2),'(i3.3)')
     &            icurzone
                write(iszother(iothzone,iothsurf,3),'(i3.3)')
     &            icursurf
                zboundarytype(iothzone,iothsurf,1)=3
                zboundarytype(iothzone,iothsurf,2)=icurzone
                zboundarytype(iothzone,iothsurf,3)=icursurf
              endif
            endif
          else

C If we are dealing with a facade then process that.
            if(nzsur(icurzone)+1.lt.MS)then
              continue
            else
              write(6,'(a,2i4)')' BEYOND surface LIMITS in z ',
     &          icurzone,nzsur(icurzone)+1

C << Place to decrement surface list >>

C << update to while logic >>
              goto 442  ! get another line
            endif
            nzsur(icurzone)=nzsur(icurzone)+1 ! increment
            nbwalls(icurzone)=nzsur(icurzone)  ! for use in silentxmlzone
            icursurf=nzsur(icurzone)
            inextsurf=icursurf+1  ! if there is an opening
            write(iszname(icurzone,icursurf),'(a)')
     &        cursurfname(1:lnblnk(cursurfname))

C And assign the surface type.
            write(isztype(icurzone,icursurf),'(a)')
     &        curstype(1:lnblnk(curstype))

C Facade add parent surface. Also list how many edges (should
C be zero at this point).
            if(idebg.ge.1)then
              write(6,'(5a,i3,a,2i4)') 'Checking match for ',
     &          cursurfname,' in ',zn(icurzone),' z ',
     &          icurzone,' s-in-z count ',nzsur(icurzone),
     &          isznver(icurzone,nzsur(icurzone))
            endif

C Initial assumption. Orientations will be double checked
C later after the polygons have been scanned.
            write(iszother(icurzone,icursurf,2),'(a)')'000'
            write(iszother(icurzone,icursurf,3),'(a)')'000'
            zboundarytype(icurzone,icursurf,1)=0
            zboundarytype(icurzone,icursurf,2)=0
            zboundarytype(icurzone,icursurf,3)=0
            write(iszotf(icurzone,icursurf),'(a)') 'OPAQUE'
            if(curstype(1:4).eq.'Roof')then
              write(iszmlcn(icurzone,icursurf),'(a)')
     &          defmlc(7)(1:lnblnk(defmlc(7)))
              write(iszuse(icurzone,icursurf,1),'(a)') 'ROOF'
              write(iszuse(icurzone,icursurf,2),'(a)') '- '
              write(iszother(icurzone,icursurf,1),'(a)')
     &          'EXTERIOR'
            endif
            if(curstype(1:7).eq.'Ceiling')then
              write(iszmlcn(icurzone,icursurf),'(a)')
     &          defmlc(8)(1:lnblnk(defmlc(8)))
              write(iszuse(icurzone,icursurf,1),'(a)') 'PARTN'
              write(iszuse(icurzone,icursurf,2),'(a)') '- '
              write(iszother(icurzone,icursurf,1),'(a)')
     &          'ANOTHER'
            endif
            if(curstype(1:12).eq.'ExteriorWall')then
              write(iszmlcn(icurzone,icursurf),'(a)')
     &          defmlc(1)(1:lnblnk(defmlc(1)))
              write(iszuse(icurzone,icursurf,1),'(a)') 'WALL'
              write(iszuse(icurzone,icursurf,2),'(a)') '- '
              write(iszother(icurzone,icursurf,1),'(a)')
     &          'EXTERIOR'
            endif
            if(curstype(1:15).eq.'UndergroundWall')then
              write(iszmlcn(icurzone,icursurf),'(a)')
     &          defmlc(13)(1:lnblnk(defmlc(13)))
              write(iszuse(icurzone,icursurf,1),'(a)') 'WALL'
              write(iszuse(icurzone,icursurf,2),'(a)') '- '
              write(iszother(icurzone,icursurf,1),'(a)')
     &          'GROUND'
              write(iszother(icurzone,icursurf,2),'(a)')'001'
              write(iszother(icurzone,icursurf,3),'(a)')'000'
              zboundarytype(icurzone,icursurf,1)=4
              zboundarytype(icurzone,icursurf,2)=1
              zboundarytype(icurzone,icursurf,3)=0
            endif
            if(curstype(1:15).eq.'UndergroundSlab')then
              write(iszmlcn(icurzone,icursurf),'(a)')
     &          defmlc(13)(1:lnblnk(defmlc(13)))
              write(iszuse(icurzone,icursurf,1),'(a)') 'FLOOR'
              write(iszuse(icurzone,icursurf,2),'(a)') 'EXTGRND'
              write(iszother(icurzone,icursurf,1),'(a)')
     &          'GROUND'
              write(iszother(icurzone,icursurf,2),'(a)')'001'
              write(iszother(icurzone,icursurf,3),'(a)')'000'
              zboundarytype(icurzone,icursurf,1)=4
              zboundarytype(icurzone,icursurf,2)=1
              zboundarytype(icurzone,icursurf,3)=0
            endif
            if(curstype(1:18).eq.'UndergroundCeiling')then
              write(iszmlcn(icurzone,icursurf),'(a)')
     &          defmlc(13)(1:lnblnk(defmlc(13)))
              write(iszother(icurzone,icursurf,1),'(a)')
     &          'GROUND'
              write(iszother(icurzone,icursurf,2),'(a)')'001'
              write(iszother(icurzone,icursurf,3),'(a)')'000'
              zboundarytype(icurzone,icursurf,1)=4
              zboundarytype(icurzone,icursurf,2)=1
              zboundarytype(icurzone,icursurf,3)=0
            endif
            if(curstype(1:12).eq.'ExposedFloor')then
              write(iszmlcn(icurzone,icursurf),'(a)')
     &          defmlc(1)(1:lnblnk(defmlc(1)))
              write(iszuse(icurzone,icursurf,1),'(a)') 'FLOOR'
              write(iszuse(icurzone,icursurf,2),'(a)') 'EXTGRND'
              write(iszother(icurzone,icursurf,1),'(a)')
     &          'EXTERIOR'
            endif
            if(curstype(1:12).eq.'RaisedFloor')then
              write(iszmlcn(icurzone,icursurf),'(a)')
     &          defmlc(1)(1:lnblnk(defmlc(1)))
              write(iszuse(icurzone,icursurf,1),'(a)') 'FLOOR'
              write(iszuse(icurzone,icursurf,2),'(a)') 'EXTGRND'
              write(iszother(icurzone,icursurf,1),'(a)')
     &          'EXTERIOR'
            endif
            if(curstype(1:11).eq.'SlabOnGrade')then
              write(iszmlcn(icurzone,icursurf),'(a)')
     &          defmlc(9)(1:lnblnk(defmlc(9)))
              write(iszuse(icurzone,icursurf,1),'(a)') 'FLOOR'
              write(iszuse(icurzone,icursurf,2),'(a)') 'EXTGRND'
              write(iszother(icurzone,icursurf,1),'(a)')
     &          'GROUND'
              write(iszother(icurzone,icursurf,2),'(a)')'001'
              write(iszother(icurzone,icursurf,3),'(a)')'000'
              zboundarytype(icurzone,icursurf,1)=4
              zboundarytype(icurzone,icursurf,2)=1
              zboundarytype(icurzone,icursurf,3)=0
            endif
            if(curstype(1:3).eq.'Air')then
              write(iszmlcn(icurzone,icursurf),'(a)')
     &          defmlc(11)(1:lnblnk(defmlc(11)))
              write(iszother(icurzone,icursurf,1),'(a)')
     &          'SIMILAR'
            endif
          endif
        endif
      elseif(PHRASE(1:19).eq.'RectangularGeometry')then

C Read lines from the xml file for data associated with the
C RectangularGeometry topic. Jump out when '/RectangularGeometry' found.
        irectangular=1
        do while (irectangular.ne.0)
          call lstripc(IUNIT,loutstr,99,ND,1,'xml rectangular',IER)
          k=0
          call EGETXMLTAG(loutstr,K,PHRASE,'-','xml tag',ier)
          if(PHRASE(1:20).eq.'/RectangularGeometry') irectangular=0
        end do

      elseif(PHRASE(1:19).eq.'CADObjectId')then

C This often is a source of documentation. Not currently used.
        if(insurface)then
          call EGETRMXML(loutstr,K,RSTR,'-','CAD surface',IER)
          write(6,*) 'the CAD obj is ',RSTR(1:lnblnk(RSTR))
        endif
      endif
      goto 442  ! loop to read another line from the XML file.


C ***********************
C Block of code that loops through the file looking for the
C next /Opening or /Surface when complexity limits exceeded.
  69  continue
      iopen=1
      do while (iopen.ne.0)
        call lstripc(IUNIT,loutstr,99,ND,1,'xml surf',IER)
        k=0
        call EGETXMLTAG(loutstr,K,PHRASE,'-','xml tag',ier)

C If we are currently working on an Opening then look for /Opening
        if(inopening.and.PHRASE(1:8).eq.'/Opening')then
          iopen=0; iopensurf=0
          inopening=.false.; iswindow=.false.
          isdoor=.false.
          nzsur(icurzone)=nzsur(icurzone)-1 ! decrement
          nbwalls(icurzone)=nzsur(icurzone)
          if(havepartition)then
            havepartition=.false.  ! unset any active partition marker
            ptnflip=.false.; ptnopeningflip=.false.
            nbxmlchild=0
          endif
        endif
        if(PHRASE(1:8).eq.'/Surface')then
          iopen=0; insurface=.false.
          isdoor=.false.; iswindow=.false.
          nzsur(icurzone)=nzsur(icurzone)-1 ! decrement
          nbwalls(icurzone)=nzsur(icurzone)
          if(havepartition)then
            havepartition=.false.  ! unset any active partition marker
            ptnflip=.false.; ptnopeningflip=.false.
            nbxmlchild=0
            do loop=1,20
              iothopsurf(loop)=0
            enddo
          endif
        endif
      end do

C Debug.
      write(6,*) errmsg(1:lnblnk(errmsg)),' ',nzsur(icurzone),
     &  nbwalls(icurzone),nextsurftype,nextsurfname,icurzone,icursurf,
     &  iopensurf
      iopensurf=0
      goto 442  ! get another line


C ****************
C Process the data if the end of the file reached.
 1000 continue

C Create new model. Note the onfiguration file will be in the
C folder /tmp/box/cfg if mpath is /tmp/box. 
      silentreturndirec='new'  ! fix this for now
      actions='new'
C      mpath='/tmp/test'  ! hardcode for testing
      write(6,*) 'ncomp before silentxmlmodel is ',ncomp
      call silentxmlmodel(actions,root,mpath,weather,simact,ier)

C Debug.
      write(6,*) 'ncomp after silentxmlmodel is ',ncomp

C Add in site information if tokens were included.
      if(havesite)then
        SLAT=sitelat
        SLON=sitelongdif
      endif
      if(havehourlysnowfile)then
        SNFNAM=hourlysnowfile
      endif

C Just to be sure re-scan the MLC database so that the construction
C attributes array are filled.
      call opendb(ier)

C << location for further system level data >>

C If there are no zones and no *start_zone found return.
      if(ncomp.eq.0.and.icomp.eq.0)then
        return
      else
        icomp=0      ! reset because it will be incrmented later

C Ask if user wants to specify globally or on a zone-by-zone basis.
        call EASKATOG('Define zone use:',' ',
     &    'spec zone-by-zone','assume all empty',
     &    'assume cellular offices','assume open plan offices',
     &    'assume coridors',' ',' ',IW,nbhelp)
        if(IW.eq.2)then
          IGU=1  ! assume all empty
        elseif(IW.eq.3)then
          IGU=2  ! assume cellular offices
        elseif(IW.eq.4)then
          IGU=3  ! assume open plan offices
        elseif(IW.eq.5)then
          IGU=4  ! assume corridors
        elseif(IW.eq.1)then
          IGU=0  ! specify zone by zone
        endif

C << Rework for metasurf common blocks.
        nsz=ncomp  ! loop through each of the zones found in gbXML file.
        do 63 isz=1,nsz

C Call silentxmlzone and then loop back for more possible definiitons.
          do 49 isu=1,MS
C Overwrite sname() from iszname() for use in silentxmlzone.
            sname(isz,isu)=iszname(isz,isu)
  49      continue
          icomp=icomp+1

C We do not yet use an gbXML shade descriptions (these seem to be
C polygons rather than block definitions). << consider approach >>.
C If there are obstructions fill in relevant common blocks. Set iobs()=2
C to signal that the common blocks have been instanciated and data should
C be included in the version 1.1 geometry file.
          if(nbobs(icomp).gt.0)then
            iobs(icomp)=2
          else
            iobs(icomp)=0
            nbobs(icomp)=0
          endif

C Logic for creating ideal controls. 
C If gbxml parse found a conditionType="HeatedAndCooled" use a
C default ideal controller, if conditionType="Unconditioned"
C the set to freefloat.
          if(i_ctl_link(isz).gt.0)then
            isilentncf=1+isilentncf
            ncf=isilentncf
            IBSN(isilentncf,1)=0; IBSN(isilentncf,2)=0
            IBSN(isilentncf,3)=0; IBSN(isilentncf,4)=0
            IBAN(isilentncf,1)=0; IBAN(isilentncf,2)=0
            IBAN(isilentncf,3)=0
            NBCDT(isilentncf)=1
            IBCDV(isilentncf,1,1)=1; IBCDV(isilentncf,1,2)=365
            NBCDP(isilentncf,1)=1
            IBCTYP(isilentncf,1,1)=0
            IBCLAW(isilentncf,1,1)=1
            TBCPS(isilentncf,1,1)=0
            BMISCD(isilentncf,1,1,1)=7
            BMISCD(isilentncf,1,1,2)=999000
            BMISCD(isilentncf,1,1,3)=0.0
            BMISCD(isilentncf,1,1,4)=999000
            BMISCD(isilentncf,1,1,5)=0.0
            BMISCD(isilentncf,1,1,6)= ht_Setpoint(isz)
            BMISCD(isilentncf,1,1,7)=cl_Setpoint(isz)
            BMISCD(isilentncf,1,1,8)=0.0
            znctldoc='basic ideal controls'
            ICASCF(isz)=ncf
          endif

          write(outs,'(2a)') 'Processing ',zname(isz)
          call edisp(iuout,outs)

C Create zone based on a set of parameters (created above).
          call silentxmlzone(ICOMP,IER)
  63    continue

C Generate a zone control file with one control law for
C each of the zones in the model (assuming conditionType
C directives were found.
        call isunix(unixok)
        if(unixok)then
          fs = char(47)
        else
          fs = char(92)
        endif
        LN=max(1,LNBLNK(cfgroot))
        if(ctlpth(1:2).eq.'  '.or.ctlpth(1:2).eq.'./')then
          WRITE(LCTLF,'(2a)')cfgroot(1:ln),'.ctl'
        else
          WRITE(LCTLF,'(4a)') ctlpth(1:lnblnk(ctlpth)),fs,
     &       cfgroot(1:ln),'.ctl'
        endif
        ICTLF=IFIL+1
        call CTLWRT(ICTLF,IER)

C Update cfg file to know about the control file
        CALL EMKCFG('-',IER)

C Pause briefly and then rescan the model cfg file and
C write it out again to sort the paths to the databases.
        call pausems(500)
        CALL ERPFREE(IFCFG,ISTAT)
        MODE='ALL '
        IUF=IPRODB   ! assign second file unit to the events db unit
        CALL ERSYS(LCFGF,IFCFG,IUF,MODE,itrc,IER)
        call pausems(500)
        CALL EMKCFG('-',IER)
      endif
      RETURN

      END

C Supporting subroutines for scanning XML data files are
C now located in lib/esru_lib.F

C ***************** Matchzone
C Matchzone given zonestring returns the index of zone matching in array zn.
C IER=0 if ok, IER >0 indicates new zone.
C index > zero is existing otherwise new.
      SUBROUTINE Matchzone(TSTR,ZN,nbz,index,IER)
#include "building.h"
      character*12 TSTR,zn(MCOM)     ! gbxml zone name
      integer nbz,index,IER
      integer loop,lntzn,lnzn
      
C Identify which zone index this surface relates to.
      lntzn=lnblnk(TSTR)
      do loop=1,nbz
        lnzn=lnblnk(zn(loop))
        if(TSTR(1:lntzn).eq.zn(loop)(1:lnzn))then
          index=loop  ! pass back index
          return
        endif
      enddo
      index=(-1)*loop
      return
      end
      
C ***************** Matchcoord
C Matchcoord given X Y Z returns the index of matching szcoords.
C Test if coordinates within 1mm consider it a match.
      SUBROUTINE Matchcoord(RVX,RVY,RVZ,iz,found,match)
#include "building.h"
#include "geometry.h"

C Parameters
      real RVX,RVY,RVZ
      logical found
      integer match,iz
C      real dist
      logical closex
     
      found=.false.; match=0
      do ixx = 1,nztv(iz)
        call eclose3(RVX,RVY,RVZ,szcoords(iz,ixx,1),
     &    szcoords(iz,ixx,2),szcoords(iz,ixx,3),0.001,closex) ! assume the same
        if(closex)then
          found=.true.  ! close enough to auto match
          match=ixx
          return
        endif
      enddo
      match=0
      return
      end

C ********* cyclejvn
C cyclejwn takes a jvn array and a vertex index and
C cycles the list so that it starts at that index and
C returns it in jvn2.
      subroutine cyclejvn(jvn1,n,ll,jvn2)
      integer n,jvn1(n),jvn2(n),ll
      integer isave,loop,istart,inarray

C Check that ll is within the jwn1 array. If not make
C jvn2 the same and return.
      istart=1
      inarray=0
      do loop=istart,n
        if(jvn1(loop).eq.ll) inarray=1
      enddo
      if(inarray.eq.0)then
        do loop=istart,n
          jvn2(loop)=jvn1(loop)
        enddo
        return
      endif

C If we already start at the desired position make jvn2
C the same and return.
      istart=1
      if(jvn1(istart).eq.ll)then
        do loop=istart,n
          jvn2(loop)=jvn1(loop)
        enddo
        return
      else
 42     isave=jvn1(istart)  ! remember
        do loop=istart,n-1
          jvn2(loop)=jvn1(loop+1) ! cycle down
        enddo
        jvn2(n)=isave    ! place saved at end
        if(jvn2(istart).eq.ll)then
          return  ! the list starts at the correct point
        else
          do loop=1,n  ! overwrite jvn1 with jvn2
            jvn1(loop)=jvn2(loop)
          enddo
          goto 42  ! do the tests and cycling again
        endif
      endif
      end  ! of cyclejvn

C ********* bkcyclejvn
C bkcyclejwn takes a jvn array and a vertex index and
C cycles the list so that it ends at that index and
C returns it in jvn2.
      subroutine bkcyclejvn(jvn1,n,ll,jvn2)
      integer n,jvn1(n),jvn2(n),ll
      integer isave,loop,iend,inarray

C Check that ll is within the jwn1 array. If not make
C jvn2 the same and return.
      iend=n
      inarray=0
      do loop=1,n
        if(jvn1(loop).eq.ll) inarray=1
      enddo
      if(inarray.eq.0)then
        do loop=1,n
          jvn2(loop)=jvn1(loop)
        enddo
        return
      endif

C If we already end at the desired position make jvn2
C the same and return.
      iend=n
      if(jvn1(iend).eq.ll)then
        do loop=1,n
          jvn2(loop)=jvn1(loop)
        enddo
        return
      else
 42     isave=jvn1(1)  ! remember 1st
        do loop=1,n-1
          jvn2(loop)=jvn1(loop+1) ! cycle down
        enddo
        jvn2(n)=isave    ! place saved at end
        if(jvn2(iend).eq.ll)then
          return  ! the list starts at the correct point
        else
          do loop=1,n  ! overwrite jvn1 with jvn2
            jvn1(loop)=jvn2(loop)
          enddo
          goto 42  ! do the tests and cycling again
        endif
      endif
      end   ! of bkcyclejvn

C gbxPOINTTOLINE: determines distance from a 3D point to a 3D line.
C where ipoint is the index of the test vertex, iwhich1 is the index
C of the vertex at the start of the line, iwhich2 is the index of the
C index at the end of the line, offset is the distance (m), match is
C a logical set to true if close enough.
C Only returns match=true if point was found between the two
C vertices (i.e. it discards matches beyond the end points.
C It assumes that calling code will decide whether the offset distance
C can be used. 
      subroutine gbxpointtoline(ipoint,iz,iwhich1,iwhich2,offset,match)
#include "building.h"
#include "geometry.h"
      dimension vd(3),vd1(3),vd2(3)
      logical match

C If any of the indices is zero then return with match=false.
      match=.false.
      iwhich3=ipoint
      if(iwhich1.eq.0.or.iwhich2.eq.0.or.iwhich3.eq.0)then
        match=.false.
        return
      endif

C Report length of line. Use method of Ward/Radiance in fvect.c
      vd(1)= szcoords(iz,IWHICH2,1)-szcoords(iz,IWHICH1,1)
      vd(2)= szcoords(iz,IWHICH2,2)-szcoords(iz,IWHICH1,2)
      vd(3)= szcoords(iz,IWHICH2,3)-szcoords(iz,IWHICH1,3)
      call dot3(vd,vd,vdis)
      vd1(1)= szcoords(iz,IWHICH3,1)-szcoords(iz,IWHICH1,1)
      vd1(2)= szcoords(iz,IWHICH3,2)-szcoords(iz,IWHICH1,2)
      vd1(3)= szcoords(iz,IWHICH3,3)-szcoords(iz,IWHICH1,3)
      call dot3(vd1,vd1,vdis1)
      vd2(1)= szcoords(iz,IWHICH3,1)-szcoords(iz,IWHICH2,1)
      vd2(2)= szcoords(iz,IWHICH3,2)-szcoords(iz,IWHICH2,2)
      vd2(3)= szcoords(iz,IWHICH3,3)-szcoords(iz,IWHICH2,3)
      call dot3(vd2,vd2,vdis2)
      if(vdis2.gt.vdis1)then
        if((vdis2 - vdis1).gt.vdis)then
          match=.false.
          offset=0.0
          return
        endif
      else
        if((vdis1 - vdis2).gt.vdis)then
          match=.false.
          offset=0.0
          return
        endif
      endif
      if(vdis.ne.0.0)then

C The original C code returned the square of distance
C so unpack via sqrt call.
        d2l=(vdis1-(vdis+vdis1-vdis2)*
     &      (vdis+vdis1-vdis2)/vdis/4.0)
        if(abs(d2l).lt.0.0003)then
          offset=d2l   ! if really small d2l
        else
          offset=SQRT(d2l)
        endif
        match=.true.
      else
        offset=0.0   ! vdis was zero so assume a match
        match=.true.
      endif
      return
      end


C*******************************************************************
C gbxSURLEHI Determines the overall length and height of a gbXML
C surface (bounding box) and passes the vaules back as XYMAX and ZMAX.
C Uses a temporary transform into 2D to get this data so
C it work on surface of most orientations. Return the position in
C the surface list of the vertex closest to the lower left, lower
C right, upper left and upper right of the bounding box.
      SUBROUTINE gbxSURLEHI(IZ,IS,XYMAX,ZMAX,llpos,lrpos,ulpos,urpos)
#include "building.h"
#include "geometry.h"

      DIMENSION  XX(MV),YY(MV),ZZ(MV)
      DIMENSION  VP(3),EP(3),EQN(4)
      DIMENSION  TMAT(4,4),RMAT(4,4)

      real XYMAX,ZMAX
      real XMIN,YMIN,XMAX,YMAX
      real shortest,shortestlr,shortestul,shortestur
      integer llpos,lrpos,ulpos,urpos     ! closest to BB corners for parent

C Set maximum values to zero.
      XYMAX = 0.0; ZMAX = 0.0

C Transform surface into into site coordinates in the
C surface of the plane. Make up XX,YY,ZZ to pass across to the
C transform routine.
      N = isznver(IZ,IS)
      DO 150 J = 1,N
        if(iszjvn(IZ,IS,J).gt.0)then
          XX(J) = szcoords(iz,iszjvn(IZ,IS,J),1)
          YY(J) = szcoords(iz,iszjvn(IZ,IS,J),2)
          ZZ(J) = szcoords(iz,iszjvn(IZ,IS,J),3)
        else
          XX(J) = 0.0; YY(J) = 0.0; ZZ(J) = 0.0
        endif
  150 CONTINUE

C Find transformation matrices that normalise face.
      call PLEQN(XX,YY,ZZ,N,VP,EQN,IERR)
      IF (IERR .LT. 0) return
      DO 250 J = 1,3
        EP(J) = VP(J) + EQN(J)
  250 CONTINUE
      CALL  EYEMAT(EP,VP,1.0,TMAT,RMAT)

C Transform all points in surface and find lower left corner
C and the upper right corner.
      XMIN=100.0; YMIN=100.0
      XMAX=0.0; YMAX=0.0
      DO I=1,N
        CALL ORTTRN(XX(I),YY(I),ZZ(I),TMAT,X1,Y1,ZZZ,IERR)
        IF(X1.LT.XMIN)XMIN=X1
        IF(Y1.LT.YMIN)YMIN=Y1
        IF(X1.GT.XMAX)XMAX=X1
        IF(Y1.GT.YMAX)YMAX=Y1
      ENDDO

C Determine the height difference between the vertices. If the height
C difference is greater than the previous values then define as new
C maximum.
      ZVAL = ABS(YMAX - YMIN)
      If (ZVAL.gt.ZMAX) ZMAX = ZVAL

C Determine the distance between the vertices on the x plane. If the
C distance is greater than the previous values then define as new
C maximum.
      XYVAL= ABS(XMAX - XMIN)
      if (XYVAL.gt.XYMAX) XYMAX = XYVAL

C Loop through the points again and figure out which one is closest
C to XMIN and YMIN (lower left).
      shortest=100.0; llpos=0
      DO I=1,N
        CALL ORTTRN(XX(I),YY(I),ZZ(I),TMAT,X1,Y1,ZZZ,IERR)
        vdis= crowxyz(xmin,ymin,0.0,x1,y1,0.0)
        if(vdis.lt.shortest)then
          shortest=vdis
          llpos=i
        endif
      ENDDO

C Loop through the points again and figure out which one is closest
C to XMAX and YMIN (lower right).
      shortestlr=100.0; lrpos=0
      DO I=1,N
        CALL ORTTRN(XX(I),YY(I),ZZ(I),TMAT,X1,Y1,ZZZ,IERR)
        vdis= crowxyz(xmax,ymin,0.0,x1,y1,0.0)
        if(vdis.lt.shortestlr)then
          shortestlr=vdis
          lrpos=i
        endif
      ENDDO

C Loop through the points again and figure out which one is closest
C to XMIN and YMAX (upper left).
      shortestul=100.0; ulpos=0
      DO I=1,N
        CALL ORTTRN(XX(I),YY(I),ZZ(I),TMAT,X1,Y1,ZZZ,IERR)
        vdis= crowxyz(xmin,ymax,0.0,x1,y1,0.0)
        if(vdis.lt.shortestul)then
          shortestul=vdis
          ulpos=i
        endif
      ENDDO

C Loop through the points again and figure out which one is closest
C to XMAX and YMAX (upper right).
      shortestur=100.0; urpos=0
      DO I=1,N
        CALL ORTTRN(XX(I),YY(I),ZZ(I),TMAT,X1,Y1,ZZZ,IERR)
        vdis= crowxyz(xmax,ymax,0.0,x1,y1,0.0)
        if(vdis.lt.shortestur)then
          shortestur=vdis
          urpos=i
        endif
      ENDDO

C Debug.
C      write(6,*) 'x and y ',XMIN,XMAX,YMIN,YMAX
C      write(6,*) ZMAX,XYMAX
C      write(6,*) 'llpos lrpos urpos ulpos',llpos,lrpos,urpos,ulpos
      if(llpos.gt.0.and.lrpos.gt.0.and.urpos.gt.0.and.ulpos.gt.0)then
C Debug.
C        write(6,'(a,i3,4f5.1,a,i3,4f5.1,a,4i4)')
C     &   'lower left at ',llpos,shortest,
C     &    XX(llpos),YY(llpos),ZZ(llpos),
C     &    ' lr',lrpos,shortestlr,XX(lrpos),
C     &    YY(lrpos),ZZ(lrpos),
C     &    ' verts are ',iszjvn(IZ,IS,llpos),
C     &    iszjvn(IZ,IS,lrpos),iszjvn(IZ,IS,urpos),
C     &    iszjvn(IZ,IS,ulpos)
C        write(6,'(a,i3,4f5.1,a,i3,4f5.1)')
C     &   'upper left at ',ulpos,shortestul,
C     &    XX(ulpos),YY(ulpos),ZZ(ulpos),
C     &    ' ur',urpos,shortestur,XX(urpos),
C     &    YY(urpos),ZZ(urpos)
      endif
      return
      END


C*******************************************************************
C gbxisadoor check is a child has a door topology. Where IZ is the
C current zone, IPS is index of parent, IDS is index of child.
C In the case of an other zone then use alt variables.
C act 'i' inside 'o' other side.
      subroutine gbxisadoor(IZ,IPS,ICS,act,isadoor)
#include "building.h"
#include "geometry.h"
      integer IZ,IPS,ICS  ! zone parent child index
      character act*1
      logical isadoor

      integer iyy,n,lldooris
      integer lldpos,lrdpos,uldpos,urdpos ! closest to BB corners for child
      integer icwhich1,icwhich2,icvwhich1,icvwhich2
      integer ipwhich1a,ipwhich2a,ipwhich1b,ipwhich2b
      integer ipvwhich1a,ipvwhich2a,ipvwhich1b,ipvwhich2b
      integer ivc,item
      logical matchver
      
      if(isznver(IZ,ICS).eq.0) return  ! cannot deal with a zero edge 
      isadoor=.false.

C First decrement isznver of the parent by isznver of 
C the door (which got into the list via earlier steps).
C Use n in subsequent logic.
      n=isznver(IZ,IPS)-isznver(IZ,ICS)  ! subtract opening
      if(n.le.0) return  ! cannot deal with zero edges

C A door topology might also apply to surfaces which share
C one or more edges with the parent. First step is to loop
C through all of the child vertices and see if they are
C on one of the parent edges.
C Debug.
      write(6,'(a,3i4)') 'gbxisadoor focused on iz ips ics ',IZ,IPS,ICS
      write(6,'(a,124i4)') 'gbxisadoor parent surf jvn ',
     &  (iszjvn(IZ,IPS,j),J=1,n)
      do loop=1,n
        loop2=iszjvn(IZ,IPS,loop)
        write(6,'(a,i3,3f7.3)')'p coords ',loop2,szcoords(iz,loop2,1),
     &    szcoords(iz,loop2,2),szcoords(iz,loop2,3)
      end do  ! of loop

      if(act.eq.'i')then
C Debug.
        write(6,'(a,i3,124i4)') 'gbxisadoor door iszjvn edges & list ',
     &    isznver(IZ,ICS),
     &    (iszjvn(iz,ics,loop3),loop3=1,isznver(IZ,ICS))
        do loop=1,isznver(IZ,ICS)
          loop2=iszjvn(IZ,ICS,loop)
          write(6,'(a,i3,3f7.3)')'c coords ',loop2,
     &     szcoords(iz,loop2,1),szcoords(iz,loop2,2),
     &     szcoords(iz,loop2,3)
        end do  ! of loop
        call gbxSURLEHI(IZ,ICS,XYMAX,ZMAX,lldpos,lrdpos,uldpos,urdpos)
        lldooris=iszjvn(IZ,ICS,lldpos)  ! door lower left
        write(6,'(a,2i4)') 'lldooris v & pos',lldooris,lldpos
      elseif(act.eq.'o')then
C Debug.
        write(6,'(a,i3,124i4)') 'gbxisadoor other open isz equiv jvn ',
     &    isznver(IZ,ICS),(iszjvn(IZ,ICS,j),J=1,isznver(IZ,ICS))
        call gbxSURLEHI(IZ,ICS,XYMAX,ZMAX,lldpos,lrdpos,uldpos,urdpos)
        lldooris=iszjvn(IZ,ICS,lldpos)  ! door lower left
        write(6,'(a,2i4)') 'o mode lldooris v & pos',lldooris,lldpos
      endif
      icwhich1=0; icwhich2=0   ! vertex index
      icvwhich1=0; icvwhich2=0 ! vertex position
      ipwhich1a=0; ipwhich2a=0   ! parent edge vertex index
      ipwhich1b=0; ipwhich2b=0   ! parent edge vertex index
      ipvwhich1a=0; ipvwhich2a=0   ! parent vertex position
      ipvwhich1b=0; ipvwhich2b=0   ! parent vertex position
      do ivc=1,isznver(IZ,ICS)  ! for each child vertex
        item=iszjvn(iz,ics,ivc)   ! vert of the child
        do iyy = 1,n   ! for each parent edge
          if(iyy.lt.isznver(IZ,IPS))then
            iptem1=iszjvn(IZ,IPS,iyy); iptem2=iszjvn(IZ,IPS,iyy+1)
          else
            iptem1=iszjvn(IZ,IPS,iyy); iptem2=iszjvn(IZ,IPS,1)
          endif
          call gbxpointtoline(item,iz,iptem1,iptem2,offset,matchver)
          if(offset.lt.0.002.and.matchver)then  ! if within 2mm
            if(icwhich1.eq.0)then
              icwhich1=item  ! remember first match
              icvwhich1=ivc  ! and its position
              ipwhich1a=iptem1
              ipwhich1b=iptem2
              ipvwhich1a=iyy
              ipvwhich1b=iyy+1
            else
              icwhich2=item  ! remember 2nd match
              icvwhich2=ivc  ! and its position
              ipwhich2a=iptem1
              ipwhich2b=iptem2
              ipvwhich2a=iyy
              ipvwhich2b=iyy+1
              goto 43  ! no need to look further
            endif
          endif
        enddo
      enddo
 43   continue

C In the parent surface we loop backwards until we are just
C after ipvwhich1a we then insert door vertex icwhich1 
C then loop backwards (3 times) within the door vertex list.
C Debug.
      write(6,'(a,13i4)') 'gbxisadoor door verts @ edge ',lldooris,
     &  icwhich1,icvwhich1,icwhich2,icvwhich2,ipwhich1a,ipvwhich1a,
     &  ipwhich1b,ipvwhich1b,ipwhich2a,ipvwhich2a,ipwhich2b,ipvwhich2b

C If some of these are still zero then what was marked as a door
C probably should be treated as an insert-within child.
      if(icwhich1.eq.0.or.icwhich2.eq.0.or.
     &   icwhich2.eq.0.or.icvwhich2.eq.0)then
        write(6,*) '***Child not really a door topology***'
        isadoor=.false.
        return
      else
        write(6,*) '***Child is a door topology***'
        isadoor=.true.
        return
      endif

      return
      END


C*******************************************************************
C gbxdoor wraps parent surface around a child surface which
C has an edge along the parent boundary. Where IZ is the
C current zone, IPS is index of parent, IDS is index of child.
C In the case of an other zone then use alt variables.
C act 'i' inside 'o' other side.  If it turns out not to
C be a door topology then pass it to gbxchild and return.
      subroutine gbxdoor(IZ,IPS,ICS,act)
#include "building.h"
#include "geometry.h"
      integer IZ,IPS,ICS  ! zone parent child index
      character act*1

      integer iyy,n,lldooris,ivll
      integer jvn1(MV),jvn2(MV) ! to hold list of opening edges
      integer lldpos,lrdpos,uldpos,urdpos ! closest to BB corners for child
      integer icwhich1,icwhich2,icvwhich1,icvwhich2
      integer ipwhich1a,ipwhich2a,ipwhich1b,ipwhich2b
      integer ipvwhich1a,ipvwhich2a,ipvwhich1b,ipvwhich2b
      integer ivc,item
      logical matchver
      
      if(isznver(IZ,ICS).eq.0) return  ! cannot deal with a zero edge list

C First decrement isznver of the parent by isznver of 
C the door (which got into the list via earlier steps). 
      n=isznver(IZ,IPS)-isznver(IZ,ICS)  ! subtract opening
      if(n.le.0) return  ! cannot deal with zero edges
      isznver(IZ,IPS)=n

C A door topology might also apply to surfaces which share
C one or more edges with the parent. First step is to loop
C through all of the child vertices and see if they are
C on one of the parent edges.
C Debug.
      write(6,'(a,3i4)') 'gbxdoor focused on iz ips ics ',IZ,IPS,ICS
      write(6,'(a,124i4)') 'gbxdoor parent surf jvn ',
     &  (iszjvn(IZ,IPS,j),J=1,isznver(IZ,IPS))
C      do loop=1,isznver(IZ,IPS)
C        loop2=iszjvn(IZ,IPS,loop)
C        write(6,'(a,i3,3f7.3)')'p coords ',loop2,szcoords(iz,loop2,1),
C     &    szcoords(iz,loop2,2),szcoords(iz,loop2,3)
C      end do  ! of loop

      if(act.eq.'i')then
C Debug.
        write(6,'(a,i3,124i4)') 'gbxdoor door iszjvn edges & list ',
     &    isznver(IZ,ICS),
     &    (iszjvn(iz,ics,loop3),loop3=1,isznver(IZ,ICS))
C        do loop=1,isznver(IZ,ICS)
C          loop2=iszjvn(IZ,ICS,loop)
C          write(6,'(a,i3,3f7.3)')'c coords ',loop2,
C     &     szcoords(iz,loop2,1),szcoords(iz,loop2,2),
C     &     szcoords(iz,loop2,3)
C        end do  ! of loop
        call gbxSURLEHI(IZ,ICS,XYMAX,ZMAX,lldpos,lrdpos,uldpos,urdpos)
        lldooris=iszjvn(IZ,ICS,lldpos)  ! door lower left
C        write(6,'(a,2i4)') 'lldooris v & pos',lldooris,lldpos
      elseif(act.eq.'o')then
C Debug.
        write(6,'(a,i3,124i4)') 'gbxdoor other open isz equiv jvn ',
     &    isznver(IZ,ICS),(iszjvn(IZ,ICS,j),J=1,isznver(IZ,ICS))
        call gbxSURLEHI(IZ,ICS,XYMAX,ZMAX,lldpos,lrdpos,uldpos,urdpos)
        lldooris=iszjvn(IZ,ICS,lldpos)  ! door lower left
        write(6,'(a,2i4)') 'o mode lldooris v & pos',lldooris,lldpos
      endif
      icwhich1=0; icwhich2=0   ! vertex index
      icvwhich1=0; icvwhich2=0 ! vertex position
      ipwhich1a=0; ipwhich2a=0   ! parent edge vertex index
      ipwhich1b=0; ipwhich2b=0   ! parent edge vertex index
      ipvwhich1a=0; ipvwhich2a=0   ! parent vertex position
      ipvwhich1b=0; ipvwhich2b=0   ! parent vertex position
      do ivc=1,isznver(IZ,ICS)  ! for each child vertex
        item=iszjvn(iz,ics,ivc)   ! vert of the child
        do iyy = 1,isznver(IZ,IPS)   ! for each parent edge
          if(iyy.lt.isznver(IZ,IPS))then
            iptem1=iszjvn(IZ,IPS,iyy); iptem2=iszjvn(IZ,IPS,iyy+1)
          else
            iptem1=iszjvn(IZ,IPS,iyy); iptem2=iszjvn(IZ,IPS,1)
          endif
          call gbxpointtoline(item,iz,iptem1,iptem2,offset,matchver)
          if(offset.lt.0.002.and.matchver)then  ! if within 2mm
            if(icwhich1.eq.0)then
              icwhich1=item  ! remember first match
              icvwhich1=ivc  ! and its position
              ipwhich1a=iptem1
              ipwhich1b=iptem2
              ipvwhich1a=iyy
              ipvwhich1b=iyy+1
            else
              icwhich2=item  ! remember 2nd match
              icvwhich2=ivc  ! and its position
              ipwhich2a=iptem1
              ipwhich2b=iptem2
              ipvwhich2a=iyy
              ipvwhich2b=iyy+1
              goto 43  ! no need to look further
            endif
          endif
        enddo
      enddo
 43   continue

C In the parent surface we loop backwards until we are just
C after ipvwhich1a we then insert door vertex icwhich1 
C then loop backwards (3 times) within the door vertex list.
C Debug.
      write(6,'(a,13i4)') 'gbxdoor door verts @ edge ',lldooris,
     &  icwhich1,icvwhich1,icwhich2,icvwhich2,ipwhich1a,ipvwhich1a,
     &  ipwhich1b,ipvwhich1b,ipwhich2a,ipvwhich2a,ipwhich2b,ipvwhich2b

C If most of these are still zero then what was marked as a door
C probably should be treated as an insert-within.
      if(icwhich1.eq.0.and.icwhich2.eq.0)then
        write(6,*) '***Child not really a door trying as child***'
        call gbxchild(IZ,IPS,ICS,act)
        return
      endif

C Check if icwhich1 or icwhich2 are the lower left corner of the door.
C And then cycle the list so the last item in the door matches this.
      if(lldooris.eq.icwhich1)then
        do iyy = 1,isznver(IZ,ICS)   ! make arrays to pass
          jvn1(iyy)=iszjvn(iz,ics,iyy)
          jvn2(iyy)=0
        enddo
        call bkcyclejvn(jvn1,isznver(IZ,ICS),icwhich1,jvn2)
C Debug.
        write(6,*) 'case lldooris.eq.icwhich1'
        write(6,*) 'jvn1 ',(jvn1(j),j=1,isznver(IZ,ICS))
        write(6,*) 'jvn2 ',(jvn2(j),j=1,isznver(IZ,ICS))
      elseif(lldooris.eq.icwhich2)then
        do iyy = 1,isznver(IZ,ICS)   ! make arrays to pass
          jvn1(iyy)=iszjvn(iz,ics,iyy)
          jvn2(iyy)=0
        enddo
        call bkcyclejvn(jvn1,isznver(IZ,ICS),icwhich2,jvn2)
C Debug.
        write(6,*) 'case lldooris.eq.icwhich2'
        write(6,*) 'jvn1 ',(jvn1(j),j=1,isznver(IZ,ICS))
        write(6,*) 'jvn2 ',(jvn2(j),j=1,isznver(IZ,ICS))
      else
C Debug.
        write(6,*) 'case lldooris not icwhich1 or icwhich2'
      endif
      ivll=ipvwhich1a  ! remember parent insertion point
C Debug.
      write(6,'(a,i3,124i4)') 'ivll parent surf jvn ',
     &  ivll,(iszjvn(IZ,IPS,j),J=1,isznver(IZ,IPS))

      n=isznver(IZ,IPS)

C Start at end of list and work backwards. Shift existing
C members of the list and then at the insertion point (ivll)
C place the child vert and shift the insertion point to 
C the right. NC begins one past NVER(ICS) because we first
C need to insert icwhich1. Use icstart logic for this.
C Use different variables if 'i' or 'o'.
      icstart=1  ! loop from the end to first
      NC=isznver(IZ,ICS); LC=1
      do iyy = NC,LC,-1  ! loop for edges of door
        isznver(IZ,IPS)=isznver(IZ,IPS)+1  ! increment parent
        iii=isznver(IZ,IPS)+1
  448   continue
        iii=iii-1
        iszjvn(IZ,IPS,iii)=iszjvn(IZ,IPS,iii-1)  ! copy lower existing up
        IF(iii.GT.ivll+1)GOTO 448
        if(icstart.eq.0)then
          iszjvn(IZ,IPS,iii)=lldooris ! assign lldooris
          ivll=ivll+1  ! shift insertion point to the right
          icstart=1
        elseif(icstart.eq.1)then
          iszjvn(IZ,IPS,iii)=jvn2(iyy) ! assign child vertex
          ivll=ivll+1  ! shift insertion point to the right
        endif
      enddo

C Debug.
      write(6,'(a,i3,124i4)') 'parent surf after door added',iyy,
     &  (iszjvn(IZ,IPS,j),J=1,isznver(IZ,IPS))

      return
      END

C*******************************************************************
C gbxchild wraps parent surface around a child surface within it.
C Where IZ is the current zone, IPS is index of parent, IDS is
C index of child. act 'i' inside 'o' other side. It checks if
C the parent surface can accept the additional vertices.

      subroutine gbxchild(IZ,IPS,ICS,act)
#include "building.h"
#include "geometry.h"
      integer IZ,IPS,ICS  ! zone parent child index
      character act*1

      integer nbxmlchild ! list of opening edges in 1st zone
      common/openings/nbxmlchild

      integer iyy,n,lldooris
      integer jvn1(MV),jvn2(MV) ! to hold list of opening edges

C parent jvn list position at the shortest distance from child corners
      integer llparpos,lrparpos,urparpos,ulparpos
      integer lldpos,lrdpos,uldpos,urdpos
      integer ipwhich,icwhich,ivll,item
      real shortestll,shortestlr,shortestul,shortestur
      real overall

C Reality checks.
      if(IZ.eq.0.or.IPS.eq.0.or.ICS.eq.0)then
        write(6,*) 'passed a ZERO to gbxchild',IZ,IPS,ICS
        return
      endif
      if(isznver(IZ,ICS).eq.0) return 

C First decrement isznver of the parent by isznver of 
C the door (which got into the list via earlier steps). 
      n=isznver(IZ,IPS)-isznver(IZ,ICS)  ! subtract opening
      if(n.le.0) return  ! cannot deal with zero edges
      isznver(IZ,IPS)=n

      write(6,'(a,3i4)') 'gbxchild focused on iz ips ics ',IZ,IPS,ICS
        write(6,'(a,124i4)') 'gbxchild parent surf jvn ',
     &    (iszjvn(IZ,IPS,j),J=1,n)
C      do loop=1,isznver(IZ,IPS)
C        loop2=iszjvn(IZ,IPS,loop)
C        write(6,'(a,i3,3f7.3)')'p coords ',loop2,szcoords(iz,loop2,1),
C     &    szcoords(iz,loop2,2),szcoords(iz,loop2,3)
C      end do  ! of loop
      call gbxSURLEHI(IZ,IPS,XYMAX,ZMAX,llparpos,lrparpos,urparpos,
     &  ulparpos)
C      write(6,*) 'LL LR UL UR position for parent...',
C     &  llparpos,lrparpos,urparpos,ulparpos

C Logic for a window assumed to be within the parent surface.
      if(act.eq.'i')then

C The following section variant of logic in insert.F
C        write(6,'(a,i3,124i4)') 'gbxchild open window edges jvn ',
C     &    (iszjvn(IZ,ICS,j),J=1,isznver(IZ,ICS))
        call gbxSURLEHI(IZ,ICS,XYMAX,ZMAX,lldpos,lrdpos,uldpos,urdpos)
        lldooris=iszjvn(IZ,ICS,lldpos)
C        write(6,*) 'LL LR UL UR position for child...',lldooris,
C     &    lldpos,lrdpos,uldpos,urdpos
      elseif(act.eq.'o')then
C        write(6,'(a,i3,124i4)') 'gbxchild other open window edges jvn ',
C     &    (iszjvn(IZ,ICS,j),J=1,isznver(IZ,ICS))
        call gbxSURLEHI(IZ,ICS,XYMAX,ZMAX,lldpos,lrdpos,uldpos,urdpos)
        lldooris=iszjvn(IZ,ICS,lldpos)
C        write(6,*) 'LL LR UL UR position for other child...',lldooris,
C     &    lldpos,lrdpos,uldpos,urdpos
      endif

C For each of the child vertex closest to LL LR UL UR find the
C distance to parent vertices and report on the shortest.
      ipwhich=0; icwhich=0; overall=100.0
      item=iszjvn(IZ,ICS,lldpos)   ! LL of the child
      xitem=szcoords(iz,item,1); yitem=szcoords(iz,item,2)
      zitem=szcoords(iz,item,3)
      shortestll=100.0; llparpos=0
      do iyy = 1,isznver(IZ,IPS)   ! determine distance
        iptem=iszjvn(IZ,IPS,iyy)
        xpitem=szcoords(iz,iptem,1); ypitem=szcoords(iz,iptem,2)
        zpitem=szcoords(iz,iptem,3)
        vdis= crowxyz(xitem,yitem,zitem,xpitem,ypitem,zpitem)
        if(vdis.lt.shortestll)then
          shortestll=vdis; llparpos=iyy
        endif
      enddo
      if(shortestll.lt.overall)then
        overall=shortestll; ipwhich=llparpos; icwhich=lldpos
      endif
      item=iszjvn(IZ,ICS,lrdpos)   ! LR of the child
      xitem=szcoords(iz,item,1); yitem=szcoords(iz,item,2)
      zitem=szcoords(iz,item,3)
      shortestlr=100.0; lrparpos=0
      do iyy = 1,isznver(IZ,IPS)   ! determine distance
        iptem=iszjvn(IZ,IPS,iyy)
        xpitem=szcoords(iz,iptem,1); ypitem=szcoords(iz,iptem,2)
        zpitem=szcoords(iz,iptem,3)
        vdis= crowxyz(xitem,yitem,zitem,xpitem,ypitem,zpitem)
        if(vdis.lt.shortestlr)then
          shortestlr=vdis; lrparpos=iyy
        endif
      enddo
      if(shortestlr.lt.overall)then
        overall=shortestlr; ipwhich=lrparpos; icwhich=lrdpos
      endif
      item=iszjvn(IZ,ICS,urdpos)   ! UR of the child
      xitem=szcoords(iz,item,1); yitem=szcoords(iz,item,2)
      zitem=szcoords(iz,item,3)
      shortestur=100.0; urparpos=0
      do iyy = 1,isznver(IZ,IPS)   ! check distance
        iptem=iszjvn(iz,IPS,iyy)
        xpitem=szcoords(iz,iptem,1); ypitem=szcoords(iz,iptem,2)
        zpitem=szcoords(iz,iptem,3)
        vdis= crowxyz(xitem,yitem,zitem,xpitem,ypitem,zpitem)
        if(vdis.lt.shortestur)then
          shortestur=vdis; urparpos=iyy
        endif
      enddo
      if(shortestur.lt.overall)then
        overall=shortestur; ipwhich=urparpos; icwhich=urdpos
      endif
      item=iszjvn(IZ,ICS,uldpos)    ! UR of the child
      xitem=szcoords(iz,item,1); yitem=szcoords(iz,item,2)
      zitem=szcoords(iz,item,3)
      shortestul=100.0; ulparpos=0
      do iyy = 1,isznver(IZ,IPS)   ! check distance
        iptem=iszjvn(IZ,IPS,iyy)
        zpitem=szcoords(iz,iptem,3)
        vdis= crowxyz(xitem,yitem,zitem,xpitem,ypitem,zpitem)
        if(vdis.lt.shortestul)then
          shortestul=vdis; ulparpos=iyy
        endif
      enddo
      if(shortestul.lt.overall)then
        overall=shortestul; ipwhich=ulparpos; icwhich=uldpos
      endif
C      write(6,*) 'parent jvn position closest to child LL LR UR UL',
C     &  llparpos,lrparpos,urparpos,ulparpos,' dist ',
C     &  shortestll,shortestlr,shortestur,shortestul
      iptem=iszjvn(IZ,IPS,ipwhich)
      xpitem=szcoords(iz,iptem,1); ypitem=szcoords(iz,iptem,2)
      zpitem=szcoords(iz,iptem,3)
C      write(6,*) 'shortest dist ',overall,' parent pos ',ipwhich,
C     &  ' parent vert is ',iptem,' @',xpitem,ypitem,zpitem
      icvert=iszjvn(IZ,ICS,icwhich)
      xitem=szcoords(iz,icvert,1); yitem=szcoords(iz,icvert,2)
      zitem=szcoords(iz,icvert,3)
C      write(6,*) 'child vertex pos is ',icwhich,
C     &  ' child vert is ',icvert,' @',xitem,yitem,zitem

C For the child use bkcyclejvn to get an ordering that ends at
C the childs closest vertex and then we decrement along that list.
      do iyy = 1,isznver(IZ,ICS)   ! make arrays to pass
        jvn1(iyy)=iszjvn(IZ,ICS,iyy)
        jvn2(iyy)=0
      enddo
      call bkcyclejvn(jvn1,isznver(IZ,ICS),icvert,jvn2)
C Debug.
C      write(6,*) 'child jvn1 ',(jvn1(j),j=1,isznver(IZ,ICS))
C      write(6,*) 'bkcycle jvn2 ',(jvn2(j),j=1,isznver(IZ,ICS))

C The process for a surface-within is to:
C locate position ipwhich in the parent and then insert
C icvert and then looping down each of the vertices in
C the child followed by jvn(IPS,ipwhich).
      ivll=ipwhich  ! remember it
C Debug.
C      write(6,'(a,i3,124i4)') 'ivll parent surf jvn ',
C     &  ivll,(iszjvn(IZ,IPS,j),J=1,isznver(IZ,IPS))

C Expand the list by looping down (from one more than the current
C number of vertices associated with parent surface) shifting
C JVN indices up one until at the lower left corner and then
C inserting the new vertes for the door.

C If ipwhich location in the list is actually the last in the list?
      n=isznver(IZ,IPS)
      if(ivll.eq.n)then

C Add vertices of the child to the parent.
        icstart=0
        nc=isznver(IZ,ICS)
        do iyy = NC,1,-1     ! for each of the child vertices
          isznver(IZ,IPS)=isznver(IZ,IPS)+1  ! increment parent
          n=isznver(IZ,IPS)       
          iszjvn(IZ,IPS,n)=jvn2(iyy) ! assign
        enddo

C And need to repeat jvn2(NC).
        isznver(IZ,IPS)=isznver(IZ,IPS)+1  ! increment parent
        n=isznver(IZ,IPS)        
        iszjvn(IZ,IPS,n)=jvn2(NC) ! assign closest again
  
C Add iszjvn(IZ,IPS,ipwhich) so as to rejoin the parent. << might not be necessary
        isznver(IZ,IPS)=isznver(IZ,IPS)+1  ! increment parent
        n=isznver(IZ,IPS)       
        iszjvn(IZ,IPS,n)=iszjvn(IZ,IPS,ipwhich) ! assign
        write(6,'(a,124i4)') 'parent surf after ',
     &    (iszjvn(IZ,IPS,j),J=1,isznver(IZ,IPS))
      else

C Start at end of list and work backwards. Shift existing
C members of the list and then at the insertion point (ivll)
C place the child vert and shift the insertion point to 
C the right. NC begins at NVER(ICS) at zero we close the
C child and at -1 we reconnect to the parent.
        nc=isznver(IZ,ICS)
        do iyy = NC,-1,-1  ! add vertex to parent
          isznver(IZ,IPS)=isznver(IZ,IPS)+1  ! increment parent
          iii=isznver(IZ,IPS)+1
  348     continue
          iii=iii-1
          iszjvn(IZ,IPS,iii)=iszjvn(IZ,IPS,iii-1)  ! copy lower existing up
          IF(iii.GT.ivll+1)GOTO 348
          if(iyy.gt.0)then
            iszjvn(IZ,IPS,iii)=jvn2(iyy) ! assign
            ivll=ivll+1  ! shift insertion point to the right
          elseif(iyy.eq.0)then
            iszjvn(IZ,IPS,iii)=jvn2(NC) ! close the child
            ivll=ivll+1  ! shift insertion point to the right
          elseif(iyy.eq.-1)then
            iszjvn(IZ,IPS,iii)=iszjvn(IZ,IPS,ipwhich) ! re-connect
            ivll=ivll+1  ! shift insertion point to the right
          endif
        enddo

C Debug.
        write(6,'(a,i3,124i4)') 'parent surf after ',iyy,
     &    (iszjvn(IZ,IPS,j),J=1,isznver(IZ,IPS))
      endif

      return
      END


