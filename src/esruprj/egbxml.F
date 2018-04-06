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
C  silentxmlmodel: creates a model from passed parameters.
C  silentxmlread: reads a gbXML file.

C ************* silentxmlmodel
C silentxmlmodel creates a model from passed parameters.
C The parameters are:
C action (8 char) as follows:
C    'new' creates model folders based on information
C       in root and mpath parameters.
C    'within' assumes that the current folder will be used.
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

C Where ESP-r was installed (as recorded when it was compiled).
      common/deflt4/dinstpath
      character dinstpath*60

C Climate data.
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG

      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      common/XMLDATA/XMLMETRICS(200),inumXMLmetrics
      character XMLMETRICS*248

C Calendar commons.
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      character calename*32,calentag*12,calendayname*32

C Simulation parameter sets.
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh

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
      sitelat=CLAT
      sitelongdif=CLONG
      IYEAR=ICYEAR

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

C Error trap. Set ier as two.
 1000 WRITE(outs,774)ISTAT
  774 FORMAT(' Error ',I7,' reading Climate db information.')
      call edisp(iuout,outs)
      call edisp(iuout,' ')
      ier=2
      RETURN

      end


C ************* silentxmlzone
C silentxmlzone creates a zone based on parameters passed to it.
C hasconstr (string array) the name of the MLC to use for each surface
C hasoptic (string array) the name of the optics to use for each surface
C use integer*3 directives for zone use pattern.,
c usefile char*32 file name (w/o path) of pattern operation file.
C IER=0 OK.
      subroutine silentxmlzone(ICOMP,hasconstr,hasoptic,hasvfc,use,
     &  usefile,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
#include "schedule.h"

      integer lnblnk  ! function definition

C Parameters
      integer icomp    ! focus zone index
      character hasconstr*32
      dimension hasconstr(MS)  ! construction to use for each surface
      character hasoptic*32
      dimension hasoptic(MS)   ! optics to use for each surface
      character hasvfc*4       ! VERT FLOR CEIL
      dimension hasvfc(MS)
      character use*8,usefile*32  ! usage pattern directives
      dimension use(3)

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

C Where ESP-r was installed (as recorded when it was compiled).
      common/deflt4/dinstpath
      character dinstpath*60

      COMMON/Vld20/Vldtng
      logical Vldtng
      common/SLNTFULL/ifullysilent
      integer ifullysilent !flag to drive the silent model creation without
                           !questions about the operation files data

C hasname (string array) the surface name for each surface (so fillsur does not wipe)
      character hasname*12
      dimension hasname(MS)  ! name to use for each surface
      DIMENSION XX(MS),YY(MS)
      character fs*1
      character ctldoc*248,LCTLF*72,zd*64
      character DFILE*72,CFILE*72,OFILE*72,TFILE*72
      character act*2
      character subpath*84,sfile*144
      character icact*8,iwatt*8,afact*8,iflow*8,vflow*8
      character sn*12,constr*32,opt*12,outs*124
      LOGICAL browse,OKC,XST,unixok
      logical willneedtmc  !  if a tmc file will be required.
      logical closev       !  mass is close to vertical
      LOGICAL QUIET
      logical anunknownmlc ! if true then there was an unknown MLC

C Strings for surface attributes to pass to insrec.
      character rsname*12,rsotf*32,rsmlcn*32,rsuse1*12,rsuse2*12
      character rsparent*12
      integer newsurf   ! index of surface when adding door/glazing
      integer ibcs      ! connection associated with base surface
      real angr         ! for rotation

C If browsing then user cannot create a new zone.
      if(browse)then
        call usrmsg('Cannot update model while in browse',
     &    'mode, you must `own` the model!','W')
        return
      endif

C Initialise flag
      ifullysilent=0
      anunknownmlc=.false.

C Set folder separator (fs) to \ or / as required.
      willneedtmc=.false.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Clear local coordinate array.
      DO 345 IW=1,MS
        XX(iw)=0.0
        YY(iw)=0.0
 345  continue

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
      ishape=3
      IF(ishape.eq.3)THEN

C For a poly shaped zone use similar logic to reading geometry file.
C Instanciate nsur and ntv for use in escrot and filsur.
        NSUR=nbwalls(ICOMP)
        NZSUR(ICOMP)=nbwalls(ICOMP)
        NTV=nztv(icomp)
        AR=rotateit(ICOMP,1)
        DO 62 I=1,nztv(icomp)
          X(I)=szcoords(ICOMP,I,1)
          Y(I)=szcoords(ICOMP,I,2)
          Z(I)=szcoords(ICOMP,I,3)
   62   CONTINUE

C Remember first zone point as rotation point.
        rotateit(ICOMP,2)=X(1)
        rotateit(ICOMP,3)=Y(1)
        DO 10 I=1,NZSUR(ICOMP)
          NVER(I)=isznbedges(ICOMP,I)
          DO 12 KV=1,NVER(I)
            JVN(I,KV)=iszlist(ICOMP,I,KV)
   12     CONTINUE
   10   CONTINUE
        IUZBASEA(icomp)=0
        IBASES(ICOMP,1)=NZSUR(ICOMP)  ! initial guess, need to check
        IZBASELIST(ICOMP)=1

C Rotate if pending. Pass in critical dimensions to escrot
C and expect the data back via common G1.
        if(AR.LT.-.01.OR.AR.GT..01)then
          x1=X(1)
          y1=Y(1)
          CALL ESCROT(AR,x1,y1)
        endif

      ENDIF

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
        write(smlcn(icc),'(a)') hasconstr(icc)
        write(svfc(icc),'(a)') hasvfc(icc)

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
        write(SOTF(icc),'(a)') hasoptic(icc)
        if(hasoptic(icc)(1:4).EQ.'OPAQ')then
          continue
        else
          willneedtmc=.true.
        endif
        if(imatch.eq.0) then
          anunknownmlc=.true.   ! mark that one MLC is unknown
        endif
        SPARENT(icc)='-'
        SUSE(icc,1)='-'
        SUSE(icc,2)='-'

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
      IDPN(ICOMP,1)=0
      IDPN(ICOMP,2)=0
      IDPN(ICOMP,3)=0
      NZSUR(ICOMP)=NSUR  ! update nzsur() it is needed by zgupdate.
      NZTV(ICOMP)=NTV

C Update the G7 common blocks and then if assign ZBASEA.
      call zgupdate(0,ICOMP,ier)
      if(ishape.eq.3)then

C For poly shape loop to find the floor in order to set up the floor area
        do 222 iSurface=1,NSUR
          if(SVFC(iSurface).eq.'FLOR')then
            IBASES(ICOMP,1)=iSurface
            ibcs=IZSTOCN(icomp,IBASES(ICOMP,1))
            ZBASEA(icomp)= SSNA(ibcs)
          endif
 222    continue
      endif

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

C Read in control file if defined. Note: the control file will not yet
C know about this new zone so decrement NCOMP before read and re-establish
C after reading control file.
      OKC=.false.
      if(LCTLF(1:1).ne.' '.and.LCTLF(1:4).ne.'UNKN')OKC=.true.
      if(OKC)then
        ICTLF=IFIL+1
        CALL ERPFREE(ICTLF,ISTAT)
        call FINDFIL(LCTLF,XST)
        IF(XST) CALL EZCTLR(ICTLF,0,IUOUT,IER)
      endif

      NCCODE(ICOMP)=NCOMP
      if(OKC)then
        icascf(NCOMP)=0
        call usrmsg(' updating control for additional zone...',' ','-')
        call CTLWRT(ICTLF,IER)
        call usrmsg(' ',' ','-')
      endif

C Update the G7 common blocks and then the configuration file.
C Use the 's' parameter to emkcfg to avoid asking for cnn file name.
      call zgupdate(0,ICOMP,ier)
      CALL EMKCFG('s',IER)

C Predefined objects - task to decide how these might be represented
C in meta file (if at all).
C << work to be done >>

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

C Operational details for this zone need to be handled. One
C option is to rely on a file within the pattern folder for
C the initial description (usefile) and use(3) parameters.
      if(use(1)(1:7).eq.'pattern')then
        LPROJ(ICOMP)=OFILE
        call isunix(unixok)
        if(unixok)then
          fs = char(47)
        else
          fs = char(92)
        endif
        write(subpath,'(6a)',iostat=ios,err=1)
     &    dinstpath(1:lnblnk(dinstpath)),fs,'training',fs,'pattern',fs
        write(sfile,'(2a)')subpath(1:lnblnk(subpath)),
     &    usefile(1:lnblnk(usefile))

C Debug.
C        write(outs,*) 'pattern file is ',sfile(1:lnblnk(sfile))
C        call edisp(iuout,outs)

C Clear commons.
        DO 401 IDTY=1,NBDAYTYPE
          NAC(IDTY)=0
          NCAS(IDTY)=0
 401    CONTINUE
        ctlstr(icomp,1)='no control of air flow  '
        ctlstr(icomp,2)='no control of air flow  '
        ctlstr(icomp,3)='no control of air flow  '
        oprdesc(icomp)='no operations notes (yet)'
        lodlabel(icomp,1)='Occupt'
        lodlabel(icomp,2)='Lights'
        lodlabel(icomp,3)='Equipt'
        lodlabel(icomp,4)='Other '
        lodlabel(icomp,5)='Ann.El'
        lodlabel(icomp,6)='N/A   '
        lodlabel(icomp,7)='N/A   '

C Fill common blocks for new zone based on information in the
C pattern file.

C Set to current zone operation file format.
        ip3ver(icomp)=2
        afact = use(2)
        iflow='ach-ach '
        vflow='ach-ach '
        isource=0
        ifullysilent=1
        idest=icomp
        Vldtng=.true.
        call COPYAIRFL(isource,idest,afact,iflow,vflow,sfile,
     &    itrc,'s',ier)

        isource=0
        idest=icomp
        icact = use(3)
        iwatt='W-W     '
        itrc=1
        call IMPORTZCASG(isource,idest,icact,iwatt,sfile,itrc,'s',ier)

C Just in case sort the data.
        call PROCESSOLDCAS(ICOMP,0,iuout)
        IUO=IFIL+2
        CALL EMKOPER(IUO,LPROJ(ICOMP),ICOMP,IER)
      endif

C Update configuration file to know about the construction files.
      CALL EMKCFG('-',IER)

      RETURN

  146 if(IOS.eq.2)then
        CALL USRMSG(' No permission to write constructions',' ','W')
      else
        CALL USRMSG(' File write error in constructions',' ','W')
      endif
      return

C Errors when creating string buffers.
   1  write(outs,*) 'error creating subpath: ',subpath
      call edisp(iuout,outs)
      ier=2
      return

      END


C ******************** SILENTXMLREAD
C SILENTXMLREAD reads a gbXML file. If act is '?' then return a
C menu and descriptive text block for the file. Otherwise scan
C and act on the information within the file.

       SUBROUTINE SILENTXMLREAD(IUNIT,LFILE,act,silentreturndirec,IER)
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

C IPV description via ipvdata.h.
      common/cctlnm/ctldoc,lctlf
      character LCTLF*72,CTLDOC*248

      integer icascf
      common/cctl/icascf(mcom)

C XMLMETRICS are the metrics that could be included in the input.xml
C in case the flag: <dump_all_data> is false
      common/XMLDATA/XMLMETRICS(200),inumXMLmetrics
      character XMLMETRICS*248

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
      character isztype*16   ! gbxml surface type
      COMMON/metasurf/iszlname(MCOM,MS),iszname(MCOM,MS),
     &  iszotf(MCOM,MS),iszmlcn(MCOM,MS),iszvfc(MCOM,MS),
     &  iszother(MCOM,MS,3),iszuse(MCOM,MS,2),iszparent(MCOM,MS),
     &  isztype(MCOM,MS)

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      integer inumXMLmetrics !counts for the number of xml metrics defined

      CHARACTER OUTSTR*248,outs*124,outs2*124
      character dstmp*24
      
      character*12 currentzonename,otherzonename
      character*12 spname(MCOM) ! gbxml space name e.g. 1 Room
      character*12 curentsurfname,nextsurfname,SN2 ! test zone name surf name
      character*16 currentstype,nextsurftype,tmptype ! test surface type
      character*12 zn(MCOM)     ! gbxml zone name e.g. aim0024

      character act*1  ! action requested.
      character simact*6 ! action for creating assessments.
      character*84 odir    ! where are folders
      integer nbz      ! number of zones detected
      integer icursurf,inextsurf ! surface focus in the current zone
      integer iothersurf,iothernextsurf ! for the other zone
      integer icurrentzone ! zone focus e.g. AdjacentSpaceId spaceIdRef="aim0024"
      integer iotherzone  ! for use with real partitions
      integer numiopenjvn,iopenjvn(MV),jvn1(MV) ! to hold list of opening edges

C Logicals for testing XML
      logical inlocation,inbuilding,inspace,insurface,inplanar
      logical inopening
      logical havepartition ! common surface between two zones
      logical havesimilar
      logical found,closex  ! equivalent of similar on other side

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
      real valx,valy,valz      ! local variable for extrude co-ords.

C Paremeters passed to:silentxmlzone
      character use*8,szuse*8,usefile*32  ! usage pattern directives
      dimension use(3),szuse(MSZ,3),usefile(MSZ)

      character hasconstr*32,szhasconstr*32
      dimension hasconstr(MS),szhasconstr(MSZ,MS)  ! construction to use for each surface
      character hasoptic*32,szhasoptic*32
      dimension hasoptic(MS),szhasoptic(MSZ,MS)  ! optics to use for each surface
      character hasvfc*4
      dimension hasvfc(MS)
      character defmlc*32  ! names of default MLC for Office
      dimension defmlc(8)
      character loutstr*248,PHRASE*124,WORD*48,STUFF*96,TAG*48,RSTR*124

C Site related local variables.
      logical havesite       ! true if tokens included
      character hourlysnowfile*72  ! same as SNFNAM
      character lworking*144  ! for processing database file names
      logical havehourlysnowfile   ! true if set
      real hoursGTM   ! hours before or after GTM
      real groundtemps(12,MGRDP)
      real GVA(12)  ! temporary array to hold a dozen values.
      integer groundtempsets
      logical havegroundtempsets
      logical havesimparameters
      integer simstartup   ! use for isstup
      integer simzonetimestep ! use for isbnstep
      integer simplanttimestep ! use for ispnstep
      integer simsavelevel  ! use for issave
      integer i_ctl_link  !flag to indicate the zone has a basic control
      real ht_Setpoint,cl_Setpoint !heating and cooling set-points
      dimension i_ctl_link(MCOM),ht_Setpoint(MCOM),cl_Setpoint(MCOM)
      integer isilentncf !silent decide how many control functions needed
      integer lsn  ! length of currentfile
      integer iva
      dimension iva(MS)  ! for use with writing arrays of numbers

      IER=0

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
      if(act(1:1).ne.'?')then
        actions=' '
        root=' '
        mpath=' '
        modeltitle='Imported from gbXML'
        modeldocblock='Imported from gbXML'
        weather=' '
        hourlysnowfile=' '
        simact='------'   ! assume no simulation directives

        groundreflmodel=0
        skyview=0.0; groundview=0.0; buildingview=0.0
        havesite=.false.
        havehourlysnowfile=.false.
        havegroundtempsets=.false.
        groundtempsets=0
        havesimparameters=.false.
        simstartup=0; simzonetimestep=0; simplanttimestep=0
        simsavelevel=0
        isilentncf=0
        ICYEAR=2016; CLAT=52.; CLONG=0.  ! initial guess

C Test read of XML files and tokens. Clear local variables.
        inlocation=.false.; inbuilding=.false.; insurface=.false.
        inplanar=.false.; inspace=.false.; inopening=.false.
        havepartition=.false.; havesimilar=.false.
        PHRASE='  '; WORD='  '; TAG='  '
	currentzonename='  '
	otherzonename='  '
        nbz=0; icursurf=0; iopensurf=0; iotheropensurf=0; 
	icurrentzone=0; iotherzone=0; inextsurf=0; match=0; n=0
        numiopenjvn=0
	RVX=0.0; RVY=0.0; RVZ=0.0
	do loop=1,8
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
            isztype(iz,isu)=' '; iszotf(iz,isu)=' '; iszvfc(iz,isu)=' '
            zboundarytype(iz,isu,1)=0
            zboundarytype(iz,isu,2)=0
            zboundarytype(iz,isu,3)=0
            sname(iz,isu)=' '
            zhasglaze(iz,isu)=0.0
            zhasdoor(iz,isu)=0.0
            isznbedges(iz,isu)=0
            do 54 ivu=1,MV
              iszlist(iz,isu,ivu)=0
  54        continue
            iszother(iz,isu,1)=' '; iszother(iz,isu,2)=' ';
            iszother(iz,isu,3)=' '
  39      continue

C Clear (but not yet used within gbXML).
          do 59 imu=1,4
            zdatamass(iz,imu,1)= 0.0
            zdatamass(iz,imu,2)= 0.0
            zdatamass(iz,imu,3)= 0.0
            zdatamass(iz,imu,4)= 0.0
            zdatamass(iz,imu,5)= 0.0
            zdatamass(iz,imu,6)= 0.0
            zdatamass(iz,imu,7)= 0.0
            ztextmass(iz,imu,1)= ' '
            ztextmass(iz,imu,2)= ' '
            ztextmass(iz,imu,3)= ' '
  59      continue
  41    continue

C For each of the META specific arrays, clear values.
        do 42 iz=1,MSZ
          szuse(iz,1) = ' '; szuse(iz,2) = ' '; szuse(iz,3) = ' '
          usefile(iz) = ' '
          nbobs(iz) = 0
          ht_Setpoint(iz)= 0.0; cl_Setpoint(iz)= 0.0
          i_ctl_link(iz)=0
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
  56        continue
  55      continue
          do 53 isu=1,MTV   ! clear the coordinates
            szcoords(iz,isu,1)=0.0
            szcoords(iz,isu,2)=0.0
            szcoords(iz,isu,3)=0.0
  53      continue
          do 52 isu=1,MS
            szhasconstr(iz,isu)=' '  ! clear MLC name and optics name
            szhasoptic(iz,isu)=' '
  52      continue
  42    continue
      endif

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

C << TODO:
C << instanciate zboundarytype during read of gbXML

 442  call lstripc(IUNIT,loutstr,99,ND,1,'geo line 1',IER)
C Debug.
C      write(6,'(a)') loutstr(1:lnblnk(loutstr))
      k=0
      call EGETXMLTAG(loutstr,K,PHRASE,'-','xml tag',ier)
C Debug.
C      write(6,'(3a,i3)') 'tag: ',PHRASE(1:lnblnk(PHRASE)),' k=',k
      if(PHRASE(1:8).eq.'Location')then
        inlocation=.true.  ! mark this is the current topic
      elseif(PHRASE(1:9).eq.'/Location')then
        inlocation=.false.
      elseif(PHRASE(1:9).eq.'StationId')then

C Decode IDType="WMO" within the StationId phrase.
        kk=10 
        call EGETEQDQXML(PHRASE,KK,TAG,STUFF,'-','idtyp',ier)
        write(6,*) 'tag is ',tag(1:lnblnk(tag)),' IDType= ',
     &    STUFF(1:lnblnk(STUFF))

C And then carry on from K in loutstr to get the value.
        CALL EGETWXML(loutstr,K,WORD,'W','StationId',IFLAG)
        write(6,'(a)') 'stationid: ',WORD(1:lnblnk(WORD))

      elseif(PHRASE(1:9).eq.'Longitude')then

C Decode Longitude
        call EGETWRXML(loutstr,K,RV,-180.,180.,'-','Longitude',IER)
        sitelongdif=RV; CLONG=RV
        write(6,'(a,f7.2)') 'Longitude: ',sitelongdif

      elseif(PHRASE(1:10).eq.'/Longitude')then
        continue
      elseif(PHRASE(1:8).eq.'Latitude')then

C Decode Longitude
        call EGETWRXML(loutstr,K,RV,-90.,90.,'-','Latitude',IER)
        sitelat=RV; CLAT=RV
        write(6,'(a,f7.2)') 'Latitude: ',sitelat
      elseif(PHRASE(1:9).eq.'/Latitude')then
        continue
      elseif(PHRASE(1:14).eq.'BuildingStorey')then
        continue
      elseif(PHRASE(1:15).eq.'/BuildingStorey')then
        continue
      elseif(PHRASE(1:8).eq.'Building')then
        inbuilding=.true.; WORD='  '; TAG='  '

C Decode buildingType="Office"
        KK=9
        call EGETEQDQXML(PHRASE,KK,TAG,STUFF,'-','idtyp',ier)
        write(6,'(4a)') 'tag is ',tag(1:lnblnk(tag)),' Type= ',
     &    STUFF(1:lnblnk(STUFF))
        if(STUFF(1:6).eq.'Office')then
          defmlc(1)='brk_blk_2012'  ! 1st is for exterior wall
          defmlc(2)='partition'     ! 2nd is for interior partition
          defmlc(3)='door_u1.5'     ! 3rd is for ext doors
          defmlc(4)='door'          ! 4th is for int doors
          defmlc(5)='dbl_glz'       ! 5th is for ext glazing
          defmlc(6)='single_glaz'   ! 6th is for int glazing
          defmlc(7)='Pitch_rf2013'  ! 7th is for sloped roof
          defmlc(8)='susp_floor'    ! 8th is for intermediate floor
        endif

C This could be a place to set default constructions for different
C surface types in the building based on the buildingType.
C << TODO >>
      elseif(PHRASE(1:9).eq.'/Building')then
        inbuilding=.false.   ! reset
      elseif(PHRASE(1:7).eq.'/Campus')then
        close(IUNIT)  ! got what we wanted echo debug.
        ncomp=nbz
C Debug.
        write(6,'(a)') 'Summary....'
        write(6,'(a,i2,a,30a)') 'nb zones ',nbz,' their names ',
     &    (zn(J),J=1,nbz)
        write(6,'(a,i2,a,30a)') 'nb spaces ',nbz,' their names ',
     &    (spname(J),J=1,nbz)
        write(6,'(a,30i3)') ' surfs in each ',(nzsur(J),J=1,nbz)
        write(6,'(a,30i3)') ' verts in each ',(nztv(J),J=1,nbz)
        do loop=1,nbz

C Strip any blanks from zone name.
          call st2name(spname(loop),SN2)
          write(zname(loop),'(a)') SN2(1:12)  ! remember
          nbwalls(loop)=nzsur(loop)  ! for use in silentxmlzone
	  write(6,*) 'Vertices'
	  do loop4=1,nztv(loop)
            write(6,*) szcoords(loop,loop4,1),szcoords(loop,loop4,2),
     &        szcoords(loop,loop4,3),loop4
          enddo
          write(6,'(a,32i4)') 'xml surf edges ary',
     &      (isznbedges(loop,J),J=1,nzsur(loop))
          do loop3=1,nzsur(loop)
            call st2name(iszlname(loop,loop3),SN2)   ! remove any spaces
            write(sname(loop,loop3),'(a)') SN2(1:12) ! truncate at 12 char
            write(6,'(a,32i4)') 'xml surf jvn ',
     &        (iszlist(loop,loop3,j),J=1,isznbedges(loop,loop3))
            write(6,'(9a)') 'surf ',iszlname(loop,loop3),
     &        iszname(loop,loop3),iszvfc(loop,loop3),
     &        isztype(loop,loop3),iszotf(loop,loop3)(1:12),
     &        iszmlcn(loop,loop3)(1:12),
     &        iszother(loop,loop3,1)(1:12),
     &        iszother(loop,loop3,2)(1:4),
     &        iszother(loop,loop3,3)(1:4)
          enddo
        enddo
        goto 1000  ! process what has been collected

      elseif(PHRASE(1:4).eq.'Name')then

C Based on the current topic decode XML name token. Note: there is an
C overlap between inbuilding and inspace.
        if(inlocation)then
          call EGETRMXML(loutstr,K,RSTR,'-','campus name',IER)
          write(6,*) 'the campus id is ',RSTR(1:lnblnk(RSTR))
        elseif(inbuilding.AND.inspace)then
          call EGETRMXML(loutstr,K,RSTR,'-','space name',IER)
          write(6,*) 'the space id is ',RSTR(1:lnblnk(RSTR))
          write(spname(nbz),'(a)') RSTR(1:lnblnk(RSTR))
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
          lnsn=lnblnk(curentsurfname)
	  do loop2=1,nzsur(icurrentzone)
            if(iszname(icurrentzone,loop2)(1:lnsn).eq.
     &         curentsurfname(1:lnsn))then
              write(iszlname(icurrentzone,loop2),'(a)')
     &          RSTR(1:lnblnk(RSTR))
              write(tmptype,'(a)') isztype(icurrentzone,loop2) ! remember

C Debug.
              write(6,'(a,2i3,5a)') 'surface',icurrentzone,loop2,
     &          ' long name: ',iszlname(icurrentzone,loop2),
     &          ' short name: ',iszname(icurrentzone,loop2),tmptype
              write(6,*) '  '
            endif
          enddo
          if(havepartition)then

C The long name of the surface in the other zone also needs to be
C established. Loop looking for currentsurfname. Also set partition
C other face surface type same as near surface.
	    do loop2=1,nzsur(iotherzone)
              if(iszname(iotherzone,loop2)(1:lnsn).eq.
     &           curentsurfname(1:lnsn))then
                write(iszlname(iotherzone,loop2),'(a)')
     &            RSTR(1:lnblnk(RSTR))
                write(isztype(iotherzone,loop2),'(a)') tmptype
                write(6,'(a,2i3,6a)') 'surface',iotherzone,loop2,
     &            ' long name: ',iszlname(iotherzone,loop2),
     &            ' short name: ',iszname(iotherzone,loop2),
     &            ' type: ',iszlname(iotherzone,loop2)
                write(6,*) '  '
              endif
           enddo
          endif  
        endif
        if(inopening)then

C Logic only tested for one opening in a surface.
          call EGETRMXML(loutstr,K,RSTR,'-','opening name',IER)
          lnsn=lnblnk(nextsurfname)
	  do loop2=1,nzsur(icurrentzone)
            if(iszname(icurrentzone,loop2)(1:lnsn).eq.
     &         nextsurfname(1:lnsn))then
              write(iszlname(icurrentzone,loop2),'(a)')
     &          RSTR(1:lnblnk(RSTR))

C Debug.
              write(6,'(a,2i3,4a)') 'opening ',icurrentzone,loop2,
     &          ' long name: ',iszlname(icurrentzone,loop2),
     &          ' short name: ',iszname(icurrentzone,loop2)
              write(6,*) '  '
            endif
          enddo  

C Inherit the parent surface iszother and increment to account for opening.
          if(havepartition)then
            lnsn=lnblnk(nextsurfname)
	    do loop2=1,nzsur(iotherzone)
              if(iszname(iotherzone,loop2)(1:lnsn).eq.
     &           nextsurfname(1:lnsn))then
                write(iszlname(iotherzone,loop2),'(a)')
     &            RSTR(1:lnblnk(RSTR))
                write(6,'(a,2i3,4a)') 'ptn open ',iotherzone,loop2,
     &           ' long name: ',iszlname(iotherzone,loop2),
     &           ' short name: ',iszname(iotherzone,loop2)
                write(6,*) '  '
              endif
            enddo  
          endif  ! havepartition
        endif    ! inopening

      elseif(PHRASE(1:13).eq.'StreetAddress')then

        call EGETRMXML(loutstr,K,RSTR,'-','address',IER)
        write(6,*) 'the street is ',RSTR(1:lnblnk(RSTR))

      elseif(PHRASE(1:13).eq.'SpaceBoundary')then

C Ignore SpaceBoundary entry.
        continue
      elseif(PHRASE(1:14).eq.'/SpaceBoundary')then
        if(inbuilding)then
          continue
        endif

      elseif(PHRASE(1:5).eq.'Space')then

C Decode buildingStoreyIdRef="aim0015" from Space phrase. This
C gives us the zone names prior to scanning zone details.
        inspace=.true.; nbz=nbz+1
        KK=6
        call EGETEQDQXML(PHRASE,KK,TAG,STUFF,'-','idtyp',ier)
        write(6,*) 'tag is ',tag(1:lnblnk(tag)),' storeyid= ',
     &    STUFF(1:lnblnk(STUFF))
        call EGETEQDQXML(PHRASE,KK,TAG,STUFF,'-','idtyp',ier)
        write(6,*) 'tag is ',tag(1:lnblnk(tag)),' id= ',
     &    STUFF(1:lnblnk(STUFF))
        write(zn(nbz),'(a)') STUFF(1:lnblnk(STUFF))
        write(6,*) 'noticed a zone ',nbz,zn(nbz)
      elseif(PHRASE(1:6).eq.'/Space')then
        inspace=.false.   ! reset
      elseif(PHRASE(1:6).eq.'Volume')then

        call EGETWRXML(loutstr,K,RV,0.,10000.,'-','Volume',IER)
        write(6,'(a,f7.2)') 'Volume: ',RV

      elseif(PHRASE(1:14).eq.'PlanarGeometry')then
        if(insurface)then
          inplanar=.true.  ! starts edge list for a surface
        endif
      elseif(PHRASE(1:15).eq.'/PlanarGeometry')then
        inplanar=.false.  ! stop paying attention to Coordinate
      elseif(PHRASE(1:8).eq.'PolyLoop')then

C Because PolyLoop happens once prior to the many coords in an
C opening if we have a true partition with an opening we need
C to increment the number of surfaces in the zone.
        if(havepartition.and.inopening)then
          write(6,'(a,5i3)') 'polyloop: ',icurrentzone,icursurf,
     &      iopensurf,iotherzone,iotheropensurf  
        endif

      elseif(PHRASE(1:9).eq.'/PolyLoop')then

C If we are dealing with an opening then we need to reverse the
C order of the vertices in the opening for use in the parent
C surface. The /PolyLoop signals that there are no more coordinates
C in the opening.
        if(inopening)then

C << Logic for a window assumed to be within the parent surface.
C << TODO - logic for a door - perhaps use a variant of that used
C << in insert.F near line 2751. Will need to determine:
C a) which of the opening edges is along the opening bottom,
C b) which coordinate of the parent is the lower left
          write(6,'(a,i3,20i3)') 'open edges & list ',numiopenjvn,
     &      (iopenjvn(loop3),loop3=1,numiopenjvn)
          do iyy = 1,numiopenjvn   ! invert this list
            jvn1(iyy)=iopenjvn(iyy)
          enddo
          iopenjvn(1)=jvn1(2)
          iopenjvn(2)=jvn1(1)
          do iyy = 3,numiopenjvn
            izz=numiopenjvn+3-iyy
            iopenjvn(iyy)=jvn1(izz)
          enddo
          write(6,'(a,i3,20i3)') 'reversed edges&list ',numiopenjvn,
     &      (iopenjvn(loop3),loop3=1,numiopenjvn)

C Apply this reversed list to the parent surface. First decrement
C isznbedges of the parent by isznbedges of the opening. Then increment
C parent with its 1st index and then add in numiopenjvn of iopenjvn.
          n=isznbedges(icurrentzone,icursurf)-  ! subtract opening
     &        isznbedges(icurrentzone,iopensurf)
          isznbedges(icurrentzone,icursurf)=n+1 ! add one
          n=isznbedges(icurrentzone,icursurf)            
          iszlist(icurrentzone,icursurf,n)=  ! assign 1st coord in parent here
     &      iszlist(icurrentzone,icursurf,1)  
          do iyy = 1,numiopenjvn   ! apply iopenjvn to iszlist
            isznbedges(icurrentzone,icursurf)=
     &        isznbedges(icurrentzone,icursurf)+1 ! add one
            n=isznbedges(icurrentzone,icursurf)            
            iszlist(icurrentzone,icursurf,n)=iopenjvn(iyy) ! assign
          enddo

C To close the parent polygon add in the 1st point of the opening.
          isznbedges(icurrentzone,icursurf)=
     &      isznbedges(icurrentzone,icursurf)+1 ! add 1st of opening
          n=isznbedges(icurrentzone,icursurf)         
          iszlist(icurrentzone,icursurf,n)=iopenjvn(1) ! assign
          write(6,'(a,i3,20i3)') 'reversed parent edges & list ',n,
     &      (iszlist(icurrentzone,icursurf,loop3),loop3=1,n)

C Now clean up the temporary arrays.
          numiopenjvn=0  ! reset number of opening edges
	  do loop3=1,MV  ! clear iopenjvn and jvn1 arrays.
            iopenjvn(loop3)=0
            jvn1(loop3)=0
          enddo
        endif

      elseif(PHRASE(1:14).eq.'CartesianPoint')then

C Pay attention if we are insurface & inplanar. Read 
C lines of <Coordinate> and jump when /CartesianPoint.
        if(insurface)then
          if(inplanar)then

C At this point the following might apply:
C a) we are dealing with facade so coords get registered once
C b) facade with an Opening so coords get registered twice and
C    the opening edges order reversed when included in parent. AND
C    if a door then need to exclude its base edge from the parent
C c) we are dealing with a real partition so coords registered twice
C    but only a single polygon is defined.  Two polygons will need to
C    be created one of which will need to have reversed edge ordering.
C    << HOW to figure out which one to reverse? >>
C d) real partition with Opening so coords registered with a total
C    of 4 surfaces!

C Update the count of edges for this surface. If there is also
C an opening in it then increment for iopensurf.
            write(6,'(a,i3,a,2i3)')' incrementing edges in ',
     &        icurrentzone,' this & opening ',icursurf,iopensurf
            isznbedges(icurrentzone,icursurf)=
     &        isznbedges(icurrentzone,icursurf)+1
            if(inopening)then
              isznbedges(icurrentzone,iopensurf)=
     &          isznbedges(icurrentzone,iopensurf)+1
              numiopenjvn=numiopenjvn+1  ! increment
            endif

C Logic for havepartition may apply. So the isznbedges arrays for
C a parent surface in the other zone and a possible opening it it.
            if(havepartition)then
              write(6,'(a,i3,2a,i3)')' havepartition ',iotherzone,
     &          otherzonename,' surfs in other ',nzsur(iotherzone)
              isurf=nzsur(iotherzone)  ! focus on slot in iotherzone ???

              isznbedges(iotherzone,isurf)=
     &          isznbedges(iotherzone,isurf)+1
              if(inopening)then
                isznbedges(iotherzone,isurf+1)=
     &            isznbedges(iotherzone,isurf+1)+1
              endif
            endif

C Get a new set of coordinates.
            loop=0
	    RVX=0.0; RVY=0.0; RVZ=0.0
 443        call lstripc(IUNIT,loutstr,99,ND,1,'X coord',IER)
            k=0
            call EGETXMLTAG(loutstr,K,PHRASE,'-','Coordinate',ier)
            if(PHRASE(1:10).eq.'Coordinate')then
              loop=loop+1
              if(loop.eq.1)then
                call EGETWRXML(loutstr,K,RVX,-180.,180.,'-','X',IER)
              elseif(loop.eq.2)then
                call EGETWRXML(loutstr,K,RVY,-180.,180.,'-','X',IER)
              elseif(loop.eq.3)then

C Got X Y & Z if this is the initial coord in the zone just add it
C otherwise test to see if it is unique.
                call EGETWRXML(loutstr,K,RVZ,-180.,180.,'-','X',IER)
                write(6,'(a,3f8.4)') 'X Y Z: ',RVX,RVY,RVZ

                if(nztv(icurrentzone).eq.0)then
                  szcoords(icurrentzone,1,1)=RVX
                  szcoords(icurrentzone,1,2)=RVY
                  szcoords(icurrentzone,1,3)=RVZ
                  nztv(icurrentzone)=nztv(icurrentzone)+1  ! increment
                  iszlist(icurrentzone,icursurf,1)=1
                else

C Logic similar to insert.F line 849.
                  if(nztv(icurrentzone)+1.le.MTV)then
                    found=.false.
                    call matchcoord(RVX,RVY,RVZ,icurrentzone,found,
     &                match)

C If an existing vertex is close then reference it otherwise
C add a new vertex to the end of the zone list. The n is the
C current edge count i.e. the position where match is to be
C inserted.
                    if(found)then
                      n=isznbedges(icurrentzone,icursurf)
                      iszlist(icurrentzone,icursurf,n)=match
C Debug
C                      write(6,'(a,2i3,3f8.3,a,2i3)')'reuse coords ',
C     &                  icurrentzone,
C     &                  icursurf,RVX,RVY,RVZ,' at ',match,n
                    else
                      n=isznbedges(icurrentzone,icursurf)
                      NZTV(icurrentzone)=NZTV(icurrentzone)+1  ! increment
                      iszlist(icurrentzone,icursurf,n)=
     &                  NZTV(icurrentzone)
                      szcoords(icurrentzone,NZTV(icurrentzone),1)=RVX
                      szcoords(icurrentzone,NZTV(icurrentzone),2)=RVY
                      szcoords(icurrentzone,NZTV(icurrentzone),3)=RVZ
                      write(6,'(a,2i3,3f8.3,a,i3)') 
     &                  'adding parent coords ',icurrentzone,
     &                  icursurf,RVX,RVY,RVZ,' nztv ',
     &                  NZTV(icurrentzone)
                    endif
                  endif
                endif
                if(inopening)then

C If there is an opening repeat the logic but with iopensurf and icursurf.
                  if(nztv(icurrentzone)+1.le.MTV)then
                    found=.false.
                    call matchcoord(RVX,RVY,RVZ,icurrentzone,found,
     &                match)

C If an existing vertex is close then reference it otherwise
C add a new vertex to the end of the zone list. Add its index to
C both the parent and opening surface.
                    if(found)then
                      n=isznbedges(icurrentzone,iopensurf)
                      iszlist(icurrentzone,iopensurf,n)=match
                      iopenjvn(n)=match ! remember so can reverse

C Debug.
C                      write(6,'(a,2i3,3f8.3,a,2i3)') 
C     &                  'reuse coords for opening ',
C     &                  icurrentzone,iopensurf,RVX,RVY,RVZ,' at ',
C     &                  match,n
                      n=isznbedges(icurrentzone,icursurf)
                      iszlist(icurrentzone,icursurf,n)=match  ! ? shift to /PolyLoop ?
C Debug.
C                      write(6,'(a,2i3,3f8.3,a,2i3)') 
C     &                  'reuse coords for parent ',
C     &                  icurrentzone,icursurf,RVX,RVY,RVZ,' at ',
C     &                  match,n
                    else
                      n=isznbedges(icurrentzone,iopensurf)
                      NZTV(icurrentzone)=NZTV(icurrentzone)+1  ! increment
                      iszlist(icurrentzone,iopensurf,n)=
     &                  NZTV(icurrentzone)
                      iopenjvn(n)=NZTV(icurrentzone) ! remember so can reverse
C Debug.
C                      write(6,'(a,2i3,3f8.3,a,2i3)')
C     &                  'adding coords opening ',
C     &                  icurrentzone,iopensurf,RVX,RVY,RVZ,
C     &                  ' nztv ',NZTV(icurrentzone),n
                      n=isznbedges(icurrentzone,icursurf)
                      NZTV(icurrentzone)=NZTV(icurrentzone)+1  ! increment
                      iszlist(icurrentzone,icursurf,n)=
     &                  NZTV(icurrentzone)
                      szcoords(icurrentzone,NZTV(icurrentzone),1)=RVX
                      szcoords(icurrentzone,NZTV(icurrentzone),2)=RVY
                      szcoords(icurrentzone,NZTV(icurrentzone),3)=RVZ
C Debug.
C                      write(6,'(a,2i3,3f8.3,a,2i3)')
C     &                  'adding coords parent ',
C     &                  icurrentzone,icursurf,RVX,RVY,RVZ,
C     &                  ' nztv ',NZTV(icurrentzone),n
                    endif
                  endif
                endif

C Logic for havepartition may also apply here.
                if(havepartition)then
                  write(6,'(a,i2,a,2i3)')' havepartition a',
     &              iotherzone,
     &              otherzonename,nzsur(iotherzone),iothersurf

C The surface count in iotherzone will already have a slot for this so use it.
                  isurf=nzsur(iotherzone)
                  if(nztv(iotherzone)+1.le.MTV)then
                    found=.false.
                    call matchcoord(RVX,RVY,RVZ,iotherzone,found,
     &                match)
                    if(found)then
                      n=isznbedges(iotherzone,isurf)
                      iszlist(iotherzone,isurf,n)=match
C Debug.
C                      write(6,'(a,2i3,3f8.3,a,2i3)')
C     &                  'reuse coords a ',iotherzone,
C     &                  isurf,RVX,RVY,RVZ,' at ',match,n
                    else
                      n=isznbedges(iotherzone,isurf)
                      NZTV(iotherzone)=NZTV(iotherzone)+1  ! increment
                      iszlist(iotherzone,isurf,n)=NZTV(iotherzone)
                      szcoords(iotherzone,NZTV(iotherzone),1)=RVX
                      szcoords(iotherzone,NZTV(iotherzone),2)=RVY
                      szcoords(iotherzone,NZTV(iotherzone),3)=RVZ
C Debug.
C                      write(6,'(a,2i3,3f8.3,a,2i3)') 
C     &                  'adding coords a ',iotherzone,
C     &                  isurf,RVX,RVY,RVZ,' nztv ',NZTV(iotherzone),n
                    endif
                  endif
                  if(inopening)then

C If the partition had an opening then check again.
                    if(nztv(iotherzone)+1.le.MTV)then
                      found=.false.
                      call matchcoord(RVX,RVY,RVZ,iotherzone,found,
     &                  match)
                      if(found)then
                        inextsurf=nzsur(iotherzone)+1  ! focus on next surf slot
                        n=isznbedges(iotherzone,inextsurf)
                        iszlist(iotherzone,inextsurf,n)=match
C Debug.
C                        write(6,'(a,2i3,3f8.3,a,2i3)') 
C     &                    'reuse coords for opening b ',
C     &                    iotherzone,inextsurf, RVX,RVY,RVZ,' at ',
C     &                    match,n
                      else
                        inextsurf=nzsur(iotherzone)+1  ! focus on next surf slot
                        n=isznbedges(iotherzone,inextsurf)
                        NZTV(iotherzone)=NZTV(iotherzone)+1   ! increment
                        iszlist(iotherzone,inextsurf,n)=
     &                    NZTV(iotherzone)
                        szcoords(iotherzone,NZTV(iotherzone),1)=RVX
                        szcoords(iotherzone,NZTV(iotherzone),2)=RVY
                        szcoords(iotherzone,NZTV(iotherzone),3)=RVZ
C Debug.
C                        write(6,'(a,2i3,3f8.3,a,4i3)') 
C     &                    'adding coords for opening b',
C     &                    iotherzone,inextsurf,RVX,RVY,RVZ,' nztv ',
C     &                    NZTV(iotherzone),n,nzsur(iotherzone),n
                      endif
                    endif
                  endif  ! of opening
                endif    ! of havepartition
              endif
              goto 443
            elseif(PHRASE(1:15).eq.'/CartesianPoint')then
              goto 442  ! get another line
            endif
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
        if(inbuilding)then
          continue
        endif
      elseif(PHRASE(1:14).eq.'/ShellGeometry')then
        if(inbuilding)then
          continue
        endif
      elseif(PHRASE(1:11).eq.'ClosedShell')then
        if(inbuilding)then
          continue
        endif
      elseif(PHRASE(1:12).eq.'/ClosedShell')then
        if(inbuilding)then
          continue
        endif
      elseif(PHRASE(1:7).eq.'Surface')then
        insurface=.true.
	
C Decode surfaceType="ExteriorWall" and then id="aim0108"
C from the Surface PHRASE. SurfaceType is used to infer
C a number of attributes,
        KK=8
        call EGETEQDQXML(PHRASE,KK,TAG,STUFF,'-','surfacetype',ier)
	write(currentstype,'(a)')STUFF(1:lnblnk(STUFF))
        write(6,*) 'tag is ',tag(1:lnblnk(tag)),' ',
     &    STUFF(1:lnblnk(STUFF))

C For some kinds of surface types there is an exposed= tag.
        if(currentstype(1:4).eq.'Roof'.or.
     &     currentstype(1:12).eq.'ExteriorWall'.or.
     &     currentstype(1:5).eq.'Shade')then
          call EGETEQDQXML(PHRASE,KK,TAG,STUFF,'-','exposed',ier)
C          write(6,*) 'tag is ',tag(1:lnblnk(tag)),' exposed= ',
C     &      STUFF(1:lnblnk(STUFF))
        endif
        call EGETEQDQXML(PHRASE,KK,TAG,STUFF,'-','id',ier)
        if(currentstype(1:5).eq.'Shade')then
          insurface=.false. ! lets try to ignore Shade for now
        else
          insurface=.true.  ! mark we are dealing with an actual surface
          write(curentsurfname,'(a)') STUFF(1:lnblnk(STUFF))  ! remember
          write(6,*) 'current surfacename is ',curentsurfname
        endif
      elseif(PHRASE(1:8).eq.'/Surface')then

C Toggle off surface indicators.
        insurface=.false.
        if(havepartition)then
          havepartition=.false.  ! unset any active partition marker
        endif
      elseif(PHRASE(1:7).eq.'Opening')then

C Check to see if opening is within a real partition, if so
C we need to add to near side zone and other side zone and
C increment the surface count in each.
C Decode openingType="OperableWindow" id="aim0119". We will 
C use this attribute later.
        inopening=.true.
        numiopenjvn=0  ! reset number of opening edges
        do loop3=1,MV  ! clear iopenjvn and jvn1 arrays.
          iopenjvn(loop3)=0
          jvn1(loop3)=0
        enddo
        KK=8
        call EGETEQDQXML(PHRASE,KK,TAG,STUFF,'-','openingType',ier)
        write(6,*) 'tag is ',tag(1:lnblnk(tag)),' ',
     &    STUFF(1:lnblnk(STUFF))
        write(nextsurftype,'(a)') STUFF(1:lnblnk(STUFF))
        call EGETEQDQXML(PHRASE,KK,TAG,STUFF,'-','id ref',ier)
        write(6,*) 'tag is ',tag(1:lnblnk(tag)),' id= ',
     &    STUFF(1:lnblnk(STUFF))
        write(nextsurfname,'(a)') STUFF(1:lnblnk(STUFF))
        nzsur(icurrentzone)=nzsur(icurrentzone)+1 ! increment
        iopensurf=nzsur(icurrentzone)             ! remember
        write(6,*) 'setting opening next type & name ',nextsurftype,
     &    nextsurfname,icurrentzone,icursurf,iopensurf
        write(iszname(icurrentzone,iopensurf),'(a)')
     &    nextsurfname(1:lnblnk(nextsurfname))
        write(isztype(icurrentzone,iopensurf),'(a)')
     &    nextsurftype(1:lnblnk(nextsurftype))

C For inclusion in the near side parent surface take on its boundary.
        if(nextsurftype(1:14).eq.'OperableWindow')then

C Need to take on the parent surface boundary attributes << TODO >>   
          write(6,*) 'Found window match ',curentsurfname,
     &     ' in ',zn(icurrentzone),' z ',icurrentzone,
     &     ' s-in-z count ',nzsur(icurrentzone)
          write(iszvfc(icurrentzone,iopensurf),'(a)') 'VERT'
          write(iszotf(icurrentzone,iopensurf),'(a)') 'TRAN'
          write(iszmlcn(icurrentzone,iopensurf),'(a)')
     &      defmlc(5)(1:lnblnk(defmlc(5)))
          write(iszother(icurrentzone,iopensurf,1),'(a)')
     &      'EXTERIOR'
          write(iszother(icurrentzone,iopensurf,2),'(a)')'000'
          write(iszother(icurrentzone,iopensurf,3),'(a)')'000'
        elseif(nextsurftype(1:14).eq.'NonSlidingDoor')then     
          write(6,*) 'Found door match ',curentsurfname,
     &      ' in ',zn(icurrentzone),' z ',icurrentzone,
     &      ' s-in-z count ',nzsur(icurrentzone)
          write(iszvfc(icurrentzone,iopensurf),'(a)') 'VERT'
          write(iszotf(icurrentzone,iopensurf),'(a)') 'OPAQUE'
          write(iszmlcn(icurrentzone,iopensurf),'(a)') 
     &      defmlc(4)(1:lnblnk(defmlc(4)))
          write(iszother(icurrentzone,iopensurf,1),'(a)')
     &      'EXTERIOR'
          write(iszother(icurrentzone,iopensurf,2),'(a)')'000'
          write(iszother(icurrentzone,iopensurf,3),'(a)')'000'
        endif

C If we have a true partition then do the same for the other zone.
        if(havepartition)then
          nzsur(iotherzone)=nzsur(iotherzone)+1 ! increment
          iotheropensurf=nzsur(iotherzone)      ! remember
          write(6,*) 'opening in other is',iotherzone,iotheropensurf
          write(iszname(iotherzone,iotheropensurf),'(a)')
     &      nextsurfname(1:lnblnk(nextsurfname))
          write(isztype(iotherzone,iotheropensurf),'(a)')
     &      nextsurftype(1:lnblnk(nextsurftype))
          if(nextsurftype(1:14).eq.'OperableWindow')then

C Need to take on the parent surface boundary attributes << TODO >>   
            write(6,*) 'Found ptn window match ',nextsurfname,
     &       ' in ',zn(iotherzone),' z ',iotherzone,
     &       ' s-in-z count ',nzsur(iotherzone)
            write(iszvfc(iotherzone,iotheropensurf),'(a)') 'VERT'
            write(iszotf(iotherzone,iotheropensurf),'(a)') 'TRAN'
            write(iszmlcn(iotherzone,iotheropensurf),'(a)') 
     &        defmlc(5)(1:lnblnk(defmlc(5)))
            write(iszother(iotherzone,iotheropensurf,1),'(a)')
     &        'EXTERIOR'
            write(iszother(iotherzone,iotheropensurf,2),'(a)')'000'
            write(iszother(iotherzone,iotheropensurf,3),'(a)')'000'
          elseif(nextsurftype(1:14).eq.'NonSlidingDoor')then     
            write(6,*) 'Found ptn door match ',nextsurfname,
     &        ' in ',zn(iotherzone),' z ',iotherzone,
     &        ' s-in-z count ',nzsur(iotherzone)
            write(iszvfc(iotherzone,iotheropensurf),'(a)') 'VERT'
            write(iszotf(iotherzone,iotheropensurf),'(a)') 'OPAQUE'
            write(iszmlcn(iotherzone,iotheropensurf),'(a)') 
     &        defmlc(4)(1:lnblnk(defmlc(4)))
            write(iszother(iotherzone,iotheropensurf,1),'(a)')
     &        'UNKNOWN'
            write(iszother(iotherzone,iotheropensurf,2),'(a)')'000'
            write(iszother(iotherzone,iotheropensurf,3),'(a)')'000'
          endif
        endif
      elseif(PHRASE(1:8).eq.'/Opening')then

C Toggle off indicators.
        iopensurf=0
        inopening=.false.

      elseif(PHRASE(1:15).eq.'AdjacentSpaceId')then

C Test the context of the surface. If we are InteriorFloor or InteriorWall
C then record currentzonename & then scan next input line to
C get the otherzonename. Set indicators as needed.
        if(insurface)then

C If the current type is Shade skip it.
          if(currentstype(1:5).eq.'Shade')then
            goto 442 ! read another line
          endif

C Decode spaceIdRef="aim0024" The 1st reference is for the inside face.
          KK=16
          call EGETEQDQXML(PHRASE,KK,TAG,STUFF,'-','spaceIdRef',ier)
          write(currentzonename,'(a)') STUFF(1:lnblnk(STUFF))
          write(6,*) 'tag is ',tag(1:lnblnk(tag)),' adj space ',
     &      currentzonename(1:lnblnk(currentzonename))

C Get the index of currentzonename (near side).
          call matchzone(currentzonename,ZN,nbz,icurrentzone,IER)
          write(6,*) 'near side zone index ',icurrentzone

C If we are dealing with an InteriorFloor or InteriorWallthere could
C be duplicate AdjacentSpaceId lines. Read next line and confirm that
C it is AdjacentSpaceId. Save token to otherzonename
          if(currentstype(1:13).eq.'InteriorFloor'.or.
     &       currentstype(1:12).eq.'InteriorWall')then
            call lstripc(IUNIT,loutstr,99,ND,1,'AdjacentSpaceId',IER)
            k=0
            call EGETXMLTAG(loutstr,K,PHRASE,'-','Coordinate',ier)
            if(PHRASE(1:15).ne.'AdjacentSpaceId')then
              goto 442
            endif
            KK=16
            call EGETEQDQXML(PHRASE,KK,TAG,STUFF,'-',
     &        'other spaceIdRef',ier)
            write(otherzonename,'(a)') STUFF(1:lnblnk(STUFF))
            write(6,*) 'tag is ',tag(1:lnblnk(tag)),' other space ',
     &        otherzonename(1:lnblnk(otherzonename))

C Get the index of otherzonename.
            call matchzone(otherzonename,ZN,nbz,iotherzone,IER)
            write(6,*) 'other side zone index ',iotherzone
            if(icurrentzone.eq.iotherzone)then

C A similar-on-other-side case. Set indices and the surface attributes.
              icursurf=nzsur(icurrentzone)+1
              inextsurf=icursurf+1  ! if there is an opening
              write(iszname(icurrentzone,icursurf),'(a)')
     &          curentsurfname(1:lnblnk(curentsurfname))

C And assign the surface type and set zboundarytype indicies.
              write(isztype(icurrentzone,icursurf),'(a)')
     &          currentstype(1:lnblnk(currentstype))
              if(currentstype(1:13).eq.'InteriorFloor')then
                write(iszvfc(icurrentzone,icursurf),'(a)') 'FLOR'
              endif
              if(currentstype(1:12).eq.'InteriorWall')then
                write(iszvfc(icurrentzone,icursurf),'(a)') 'VERT'
              endif
              zboundarytype(icurrentzone,icursurf,1)=1
              zboundarytype(icurrentzone,icursurf,2)=0
              zboundarytype(icurrentzone,icursurf,3)=0

C A similar-on-other-side case. Add the parent surface.
              nzsur(icurrentzone)=nzsur(icurrentzone)+1 ! increment
              write(6,'(5a,i3,a,i3)') 
     &         'Found similar-on-other-side match ',
     &         curentsurfname,' in ',zn(icurrentzone),' z ',
     &         icurrentzone,' s-in-z count ',nzsur(icurrentzone)
              write(iszotf(icurrentzone,icursurf),'(a)') 'OPAQUE'
              if(currentstype(1:13).eq.'InteriorFloor')then
                write(iszmlcn(icurrentzone,icursurf),'(a)') 
     &            defmlc(8)(1:lnblnk(defmlc(8)))
              endif
              if(currentstype(1:12).eq.'InteriorWall')then
                write(iszmlcn(icurrentzone,icursurf),'(a)') 
     &            defmlc(2)(1:lnblnk(defmlc(2)))
              endif
              write(iszother(icurrentzone,icursurf,1),'(a)')
     &          'SIMILAR'
              write(iszother(icurrentzone,icursurf,2),'(a)')'000'
              write(iszother(icurrentzone,icursurf,3),'(a)')'000'
              havesimilar=.true.   ! similar-on-other-side.
               ! clear a name here??
              goto 442 ! read another line
            else

C We will also need a new surface in iotherzone.	       
              havepartition=.true. ! a true partition.
              write(6,'(2a,i2,2a,i2)')'true partition between ', 
     &          currentzonename,icurrentzone,' & ',otherzonename,
     &          iotherzone

C Now that we know which zone we can assign curentsurfname and attributes.
              nzsur(icurrentzone)=nzsur(icurrentzone)+1 ! increment
              icursurf=nzsur(icurrentzone)
              inextsurf=icursurf+1  ! if there is an opening
              iothersurf=nzsur(iotherzone)+1   ! remember
              iothernextsurf=iothersurf+1  ! if there is an opening
              write(6,*) 'Adj setting up ',icurrentzone,icursurf,
     &          inextsurf,iothersurf,iothernextsurf

              write(iszname(icurrentzone,icursurf),'(a)')
     &          curentsurfname(1:lnblnk(curentsurfname))
              if(currentstype(1:12).eq.'InteriorWall')then
                write(6,'(5a,i3,a,i3)') 'Found partition match ',
     &            curentsurfname,' in ',zn(icurrentzone),' z ',
     &            icurrentzone,' s-in-z count ',nzsur(icurrentzone)
                write(iszvfc(icurrentzone,icursurf),'(a)') 'VERT'
                write(iszotf(icurrentzone,icursurf),'(a)') 'OPAQUE'
                write(iszmlcn(icurrentzone,icursurf),'(a)') 
     &            defmlc(2)(1:lnblnk(defmlc(2)))
                write(iszother(icurrentzone,icursurf,1),'(a)')
     &            otherzonename
                write(iszother(icurrentzone,icursurf,2),'(i3.3)')
     &            iotherzone
                write(iszother(icurrentzone,icursurf,3),'(i3.3)')
     &            iothersurf
                zboundarytype(icurrentzone,icursurf,1)=3
                zboundarytype(icurrentzone,icursurf,2)=iotherzone
                zboundarytype(icurrentzone,icursurf,3)=iothersurf
              elseif(currentstype(1:13).eq.'InteriorFloor')then

C For InteriorFloor.
                write(6,'(5a,i3,a,i3)') 'Found int floor match ',
     &            curentsurfname,' in ',zn(icurrentzone),' z ',
     &            icurrentzone,' s-in-z count ',nzsur(icurrentzone)
                write(iszvfc(icurrentzone,icursurf),'(a)') 'FLOR'
                write(iszotf(icurrentzone,icursurf),'(a)') 'OPAQUE'
                write(iszmlcn(icurrentzone,icursurf),'(a)') 
     &            defmlc(8)(1:lnblnk(defmlc(8)))
                write(iszother(icurrentzone,icursurf,1),'(a)')
     &            otherzonename
                write(iszother(icurrentzone,icursurf,2),'(i3.3)')
     &            iotherzone
                write(iszother(icurrentzone,icursurf,3),'(i3.3)')
     &            iothersurf
              endif

C And for the other zone InteriorWall.
              write(iszname(iotherzone,iothersurf),'(a)')
     &          curentsurfname(1:lnblnk(curentsurfname))
              nzsur(iotherzone)=nzsur(iotherzone)+1 ! increment
              if(currentstype(1:12).eq.'InteriorWall')then
                write(6,'(5a,i3,a,i3)') 'Found partition match ',
     &            curentsurfname,' in ',zn(iotherzone),' z ',
     &            iotherzone,' s-in-z count ',nzsur(iotherzone)
                write(iszvfc(iotherzone,iothersurf),'(a)') 'VERT'
                write(iszotf(iotherzone,iothersurf),'(a)') 'OPAQUE'
                write(iszmlcn(iotherzone,iothersurf),'(a)')
     &            defmlc(2)(1:lnblnk(defmlc(2)))
                write(iszother(iotherzone,iothersurf,1),'(a)')
     &            currentzonename
                write(iszother(iotherzone,iothersurf,2),'(i3.3)')
     &            icurrentzone
                write(iszother(iotherzone,iothersurf,3),'(i3.3)')
     &            icursurf
                zboundarytype(iotherzone,iothersurf,1)=3
                zboundarytype(iotherzone,iothersurf,2)=icurrentzone
                zboundarytype(iotherzone,iothersurf,3)=icursurf
              elseif(currentstype(1:13).eq.'InteriorFloor')then

C For InteriorFloor.
                write(6,'(5a,i3,a,i3)') 'Found int floor match ',
     &            curentsurfname,' in ',zn(iotherzone),' z ',
     &            iotherzone,' s-in-z count ',nzsur(iotherzone)
                write(iszvfc(iotherzone,iothersurf),'(a)') 'FLOR'
                write(iszotf(iotherzone,iothersurf),'(a)') 'OPAQUE'
                write(iszmlcn(iotherzone,iothersurf),'(a)')
     &            defmlc(8)(1:lnblnk(defmlc(8)))
                write(iszother(iotherzone,iothersurf,1),'(a)')
     &            currentzonename
                write(iszother(iotherzone,iothersurf,2),'(i3.3)')
     &            icurrentzone
                write(iszother(iotherzone,iothersurf,3),'(i3.3)')
     &            icursurf
                zboundarytype(iotherzone,iothersurf,1)=3
                zboundarytype(iotherzone,iothersurf,2)=icurrentzone
                zboundarytype(iotherzone,iothersurf,3)=icursurf
              endif
            endif
          else

C If we are dealing with a facade then process that.
            nzsur(icurrentzone)=nzsur(icurrentzone)+1 ! increment
            icursurf=nzsur(icurrentzone)
            inextsurf=icursurf+1  ! if there is an opening
            write(iszname(icurrentzone,icursurf),'(a)')
     &        curentsurfname(1:lnblnk(curentsurfname))

C And assign the surface type.
            write(isztype(icurrentzone,icursurf),'(a)')
     &        currentstype(1:lnblnk(currentstype))
            if(currentstype(1:4).eq.'Roof'.or.
     &         currentstype(1:12).eq.'ExteriorWall')then

C Facade add parent surface.
              write(6,'(5a,i3,a,i3)') 'Found ext or roof match ',
     &          curentsurfname,' in ',zn(icurrentzone),' z ',
     &          icurrentzone,' s-in-z count ',nzsur(icurrentzone)
              if(currentstype(1:4).eq.'Roof')then
                write(iszvfc(icurrentzone,icursurf),'(a)') 'CEIL'
                write(iszmlcn(icurrentzone,icursurf),'(a)')
     &            defmlc(7)(1:lnblnk(defmlc(7)))
              endif
              if(currentstype(1:12).eq.'ExteriorWall')then
                write(iszvfc(icurrentzone,icursurf),'(a)') 'VERT'
                write(iszmlcn(icurrentzone,icursurf),'(a)')
     &            defmlc(1)(1:lnblnk(defmlc(1)))
              endif
              write(iszotf(icurrentzone,icursurf),'(a)') 'OPAQUE'
              write(iszother(icurrentzone,icursurf,1),'(a)')
     &          'EXTERIOR'
              write(iszother(icurrentzone,icursurf,2),'(a)')'000'
              write(iszother(icurrentzone,icursurf,3),'(a)')'000'
              zboundarytype(icurrentzone,icursurf,1)=0
              zboundarytype(icurrentzone,icursurf,2)=0
              zboundarytype(icurrentzone,icursurf,3)=0
            endif
          endif
        endif
      elseif(PHRASE(1:19).eq.'RectangularGeometry')then
        continue
      elseif(PHRASE(1:20).eq.'/RectangularGeometry')then
        continue
      elseif(PHRASE(1:7).eq.'Azimuth')then
        continue
      elseif(PHRASE(1:8).eq.'/Azimuth')then
        continue
      elseif(PHRASE(1:4).eq.'Tilt')then
        continue
      elseif(PHRASE(1:5).eq.'/Tilt')then
        continue
      elseif(PHRASE(1:5).eq.'Width')then
        continue
      elseif(PHRASE(1:6).eq.'/Width')then
        continue
      elseif(PHRASE(1:6).eq.'Height')then
        continue
      elseif(PHRASE(1:7).eq.'/Height')then
        continue
      elseif(PHRASE(1:19).eq.'CADObjectId')then

C This often is a source of documentation. Not currently used.
        if(insurface)then
          call EGETRMXML(loutstr,K,RSTR,'-','CAD surface',IER)
          write(6,*) 'the CAD obj is ',RSTR(1:lnblnk(RSTR))
        endif
      endif
      goto 442  ! loop to read another line from the XML file.

C Process the data if the end of the file reached.
 1000 continue

C Create new model. Note the onfiguration file will be in the
C folder /tmp/box/cfg if mpath is /tmp/box. 
      silentreturndirec='new'  ! fix this for now
      actions='new'
      mpath='/tmp/test'  ! hardcode for testing
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
      if(havegroundtempsets)then
        NGRDP=groundtempsets
        do 128 IGRDP=1,NGRDP
          DO 129 J=1,12
            UGRDTP(J,IGRDP)=groundtemps(J,IGRDP)
  129     CONTINUE
  128   continue
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

C << Rework for metasurf common blocks.
        nsz=ncomp  ! loop through each of the zones found in gbXML file.
        do 63 isz=1,nsz

C Call silentxmlzone and then loop back for more possible definiitons.
C Copy from sz* arrays into parameter arrays prior to silentzone call.
C << TODO Infer szuse from isztype >>
          use(1)= szuse(isz,1)
          use(2)= szuse(isz,2)
          use(3)= szuse(isz,3)
C        usefile(iz) = ' '

C << TODO switch from or deal with szhasoptic >>
          do 49 isu=1,MS
            hasconstr(isu)=iszmlcn(isz,isu)
            hasoptic(isu)=iszotf(isz,isu)
            hasvfc(isu)=iszvfc(isz,isu)
  49      continue
          icomp=icomp+1

C If there are obstructions fill in relevant common blocks. Set iobs()=2
C to signal that the common blocks have been instanciated and data should
C be included in the version 1.1 geometry file.
          if(nbobs(icomp).gt.0)then
            iobs(icomp)=2
          else
            iobs(icomp)=0
            nbobs(icomp)=0
          endif

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
          call silentxmlzone(ICOMP,hasconstr,hasoptic,hasvfc,use,
     &      usefile(isz),IER)
  63    continue

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
      endif
      RETURN

 1001 write(outs,'(3a)') 'SILENTREAD: conversion error in...',
     &  OUTSTR(1:50),'...'
      lsn=MIN0(lnblnk(currentfile),110)
      write(outs2,'(2a)') 'in: ',currentfile(1:lsn)
      if(dll)then
        dllsubr='SILENTREAD'
        dllmesg=outs
        ier=2
        CALL ERPFREE(IUNIT,ios)
        return
      else
        call edisp(iuout,outs)
        call edisp(iuout,outs2)
        IER=1
        CALL ERPFREE(IUNIT,ios)
        RETURN
      endif

      END

C Supporting subroutines for scanning XML data files.
C  EGETXMLTAG:  Finds first XML tag after pos k in a string within an XML file.
C  EGETWXML:    Finds first word after pos k in a string within an XML file.
C  EGETWIXML:   As EGETWI for an integer within an XML file.
C  EGETWRXML:   As EGETWR for a real within an XML file.
C  EGETEQDQXML: Recovers tag="x" returning both tag and x.
C  EGETRMXML:   The remainder of a text an XML tag within an XML file.

C ***************** EGETXMLTAG
C EGETXMLTAGE gets first XML tag after position K from the STRING of
C characters. Strips the leading < and the trailing >. Returns k at
C the trailing > Spaces within tag are ok. Provides a warning
C message if ACT='W', a failure message if ACT='F' and does
C no message if ACT='-'.
      SUBROUTINE EGETXMLTAG(STRING,K,PHRASE,ACT,MSG,ier)
#include "espriou.h"
      common/OUTIN/IUOUT,IUIN

      CHARACTER*(*) PHRASE, STRING, MSG
      CHARACTER ACT*1,A*1,loutstr*248,outs*124
      logical unixok

C LS is th maximum length of STRING, L the current position,
C LW the maximum length of PHRASE.
      ier=0
      PHRASE=' '
      LS=LEN(STRING)
      LW=LEN(PHRASE)
      L=0

C Start by skipping blanks tabs before the PHRASE.
   10 K=K+1
      IF(K.GT.LS) GOTO 999
      A=STRING(K:K)
      IF(A.EQ.' '.OR.A.EQ.','.OR.(ICHAR(A).EQ.9)) GOTO 10

      if(A.EQ.'<') then
        K=K+1            ! increment to next character
        A=STRING(K:K)    ! instanciate A
        goto 20          ! proceed to copy
      elseif(A.EQ.'>') then
        continue
      endif

C Copy PHRASE from STRING, character by character until tab or a
C comma or > is found.
   20 L=L+1
      IF(L.GT.LW) return
      if(A.EQ.'>') then
        return          ! do not save > in phrase
      endif
      PHRASE(L:L)=A     ! copy character into the phrase
      K=K+1
      IF(K.GT.LS) GOTO 999
      A=STRING(K:K)
      IF(ICHAR(A).EQ.9.or.A.eq.',') GO TO 100
      if(A.EQ.'>') then
        continue
      endif
      GOTO 20

  100 RETURN

  999 if(ACT.EQ.'-')then
        RETURN
      elseif(ACT.EQ.'W')then
        ier=1
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(loutstr,'(5a)',IOSTAT=IOS,ERR=1)
     &      'WARNING: in ',currentfile(1:LN),
     &      ' past end of line for ',MSG(1:LNM),' (phrase) in...'
        else
          WRITE(loutstr,'(3a)',IOSTAT=IOS,ERR=1)
     &      'WARNING: Past end of line for ',
     &      MSG(1:LNM),' (phrase) in...'
        endif
      elseif(ACT.EQ.'F')then
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(loutstr,'(5a)',IOSTAT=IOS,ERR=1)
     &      'FAILURE: in ',currentfile(1:LN),
     &      ' past end of line for ',MSG(1:LNM),' (phrase) in...'
        else
          WRITE(loutstr,'(3a)',IOSTAT=IOS,ERR=1)
     &      'FAILURE: Past end of line for ',
     &      MSG(1:LNM),' (phrase) in...'
        endif
      endif
      CALL EDISP248(iuout,loutstr,100)
      if(lnblnk(STRING).gt.123)then
        CALL EDISP248(iuout,STRING,100)
      else
        CALL EDISP(iuout,STRING)
      endif
      GOTO 100

   1  call isunix(unixok)
      if(.NOT.unixok) return  ! if DOS return because of lack of I/O channel
      LNM=max(1,lnblnk(STRING))
      if(IOS.eq.2)then
        write(outs,*) 'EGETXMLTAG: permission error writing warning: '
        call edisp(iuout,outs)
        call edisp(iuout,STRING(1:LNM))
      else
        write(outs,*) 'EGETXMLTAG: error writing warning: '
        call edisp(iuout,outs)
        call edisp(iuout,STRING(1:LNM))
      endif
      return

      END


C ***************** EGETWXML
C EGETWXML gets first WORD after position K from the STRING of
C characters. Words are separated by blanks, commas, |, <, or tab: WORD,WORD,WORD
C or WORD WORD WORD or WORD, WORD, WORD  WORD> are all valid.  Provides a warning
C message if ACT='W', a failure message if ACT='F' and does
C no message if ACT='-'.  Modified after:
C G.N. Walton, US Nat. Institute of Standards and Technology
C     LS     - maximum length of STRING
C     L      - current position in WORD
C     LW     - maximum length of WORD

      SUBROUTINE EGETWXML(STRING,K,WORD,ACT,MSG,IER)
#include "espriou.h"
C espriou.h provides currentfile.
      common/OUTIN/IUOUT,IUIN

      CHARACTER*(*) WORD, STRING, MSG
      CHARACTER ACT*1,A*1,loutstr*248,outs*124
      logical unixok

      WORD=' '
      LS=LEN(STRING)
      LW=LEN(WORD)
      L=0

C Start by skipping blanks and tabs before the word.
   10 K=K+1
      IF(K.GT.LS) GOTO 999
      A=STRING(K:K)
      IF(A.EQ.' '.OR.A.EQ.','.OR.(ICHAR(A).EQ.9).OR.
     &   (ICHAR(A).eq.124)) GOTO 10

C Copy WORD from STRING, character by character until separator found.
C A < is considered a separator as it represents start of a subsequent
C XML tag.
   20 L=L+1
      IF(L.GT.LW) GOTO 100
      WORD(L:L)=A
      K=K+1
      IF(K.GT.LS) GOTO 999
      A=STRING(K:K)
      IF(A.EQ.' '.OR.A.EQ.','.OR.(ICHAR(A).EQ.9).OR.
     &   (ICHAR(A).eq.124).OR.A.EQ.'<') GO TO 100
      GOTO 20

  100 RETURN

  999 ier=1
      if(ACT.EQ.'-')then
        RETURN
      elseif(ACT.EQ.'W')then
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(loutstr,'(5a)',IOSTAT=IOS,ERR=1)
     &      'WARNING: in ',currentfile(1:LN),
     &      ' past end of line for ',MSG(1:LNM),' in...'
        else
          WRITE(loutstr,'(3a)',IOSTAT=IOS,ERR=1)
     &      'WARNING: Past end of line for ',
     &      MSG(1:LNM),' (word) in...'
        endif
      elseif(ACT.EQ.'F')then
        ier=1
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(loutstr,'(5a)',IOSTAT=IOS,ERR=1)
     &      'FAILURE: in ',currentfile(1:LN),
     &      ' past end of line for ',MSG(1:LNM),' in...'
        else
          WRITE(loutstr,'(3a)',IOSTAT=IOS,ERR=1)
     &      'FAILURE: Past end of line for ',
     &      MSG(1:LNM),' (word) in...'
        endif
      endif
      CALL EDISP248(iuout,loutstr,100)
      if(lnblnk(STRING).gt.123)then
        CALL EDISP248(iuout,STRING,100)
      else
        CALL EDISP(iuout,STRING)
      endif
      GOTO 100

   1  call isunix(unixok)
      if(.NOT.unixok) return  ! if DOS return because of lack of I/O channel
      LNM=max(1,lnblnk(STRING))
      if(IOS.eq.2)then
        write(outs,*) 'EGETWXML: permission error writing warning re: '
        call edisp(iuout,outs)
        call edisp(iuout,STRING(1:LNM))
      else
        write(outs,*) 'EGETWXML: error writing warning re: '
        call edisp(iuout,outs)
        call edisp(iuout,STRING(1:LNM))
      endif
      return

      END


C ***************** EGETWIXML
C EGETWIXML gets first word after position K from the STRING of
C characters and converts it into an integer IV, tests it against
C the minimum MN and the maximum MX and provides a warning
C message if ACT='W', a failure message if ACT='F' and does
C no range checking if ACT='-'. Words may be separated by blanks,
C commas, <, or tab: WORD,WORD,WORD or WORD WORD WORD or WORD, WORD, WORD
C are all valid.
      SUBROUTINE EGETWIXML(STRING,K,IV,MN,MX,ACT,MSG,IER)
#include "espriou.h"
      common/OUTIN/IUOUT,IUIN

      CHARACTER*(*) STRING, MSG
      CHARACTER ACT*1,STR1*10,STR2*10,WORD*20,loutstr*248,outs*124
      logical unixok

C Pick up line and lenght for error messages.
      ils=max(1,lnblnk(STRING))
      if(ils.gt.105)ils=105

      IER=0
      WORD=' '

C Use alternative call that also checks for <.
      CALL EGETWXML(STRING,K,WORD,'-','integer',IER)
      IF(IER.NE.0)RETURN
      read(WORD,*,IOSTAT=IOS,ERR=1002)iv

C If range checking disabled, jump out of routine.
      IF(ACT.EQ.'-')RETURN

C Make up reporting string.
      CALL INTSTR(IV,STR1,IW1,IER)

C Check IV against minimum and respond based on ACT.
      IF(IV.LT.MN)THEN
        CALL INTSTR(MN,STR2,IW2,IER)
        IF(ACT.EQ.'W')THEN
          call edisp(iuout,' ')
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(loutstr,'(4a)',IOSTAT=IOS,ERR=1)
     &        'WARNING: in ',currentfile(1:LN),': ',STRING(1:ils)
          else
            write(loutstr,'(a,a)',IOSTAT=IOS,ERR=1)' WARNING in: ',
     &      STRING(1:ils)
          endif
          CALL EDISP248(iuout,loutstr,100)
          write(loutstr,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &      ' value (',STR1(1:IW1),') < normal minimum ',STR2(1:IW2),'!'
          CALL EDISP248(iuout,loutstr,100)
        ELSEIF(ACT.EQ.'F')THEN
          call edisp(iuout,' ')
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(loutstr,'(4a)',IOSTAT=IOS,ERR=1)
     &        'FAILURE: in ',currentfile(1:LN),': ',STRING(1:ils)
          else
            write(loutstr,'(a,a)',IOSTAT=IOS,ERR=1)' FAILURE in: ',
     &        STRING(1:ils)
          endif
          CALL EDISP248(iuout,loutstr,100)
          write(loutstr,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &       ' value (',STR1(1:IW1),') < allowable minimum ',
     &       STR2(1:IW2),'!'
          CALL EDISP248(iuout,loutstr,100)
          IER=1
          RETURN
        ENDIF
      ELSEIF(IV.GT.MX)THEN
        CALL INTSTR(MX,STR2,IW2,IER)
        IF(ACT.EQ.'W')THEN
          call edisp(iuout,' ')
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(loutstr,'(4a)',IOSTAT=IOS,ERR=1)
     &        'WARNING: in ',currentfile(1:LN),': ',STRING(1:ils)
          else
            write(loutstr,'(a,a)',IOSTAT=IOS,ERR=1)' WARNING in: ',
     &        STRING(1:ils)
          endif
          CALL EDISP248(iuout,loutstr,100)
          write(loutstr,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &      ' value (',STR1(1:IW1),') > normal maximum ',STR2(1:IW2),'!'
          CALL EDISP248(iuout,loutstr,100)
        ELSEIF(ACT.EQ.'F')THEN
          call edisp(iuout,' ')
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(loutstr,'(4a)',IOSTAT=IOS,ERR=1)
     &        'FAILURE: in ',currentfile(1:LN),': ',STRING(1:ils)
          else
            write(loutstr,'(a,a)',IOSTAT=IOS,ERR=1)' FAILURE in: ',
     &        STRING(1:ils)
          endif
          CALL EDISP248(iuout,loutstr,100)
          write(loutstr,'(7a)',IOSTAT=IOS,ERR=1) ' the ',MSG(1:LNM),
     &       ' value (',STR1(1:IW1),') > allowable maximum ',
     &       STR2(1:IW2),'!'
          CALL EDISP248(iuout,loutstr,100)
          IER=1
          RETURN
        ENDIF
      ENDIF

  100 RETURN

 1002 if(currentfile(1:2).ne.'  ')then
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        WRITE(loutstr,'(6a)',IOSTAT=IOS,ERR=1) 'Failed in ',
     &    currentfile(1:LN),': conversion of ',
     &    MSG(1:LNM),' value in ',WORD
      else
        LNM=max(1,lnblnk(MSG))
        WRITE(loutstr,1003,IOSTAT=IOS,ERR=1)MSG(1:LNM),WORD
 1003   FORMAT(' Failed: conversion of ',A,' value in ',A)
      endif
      CALL EDISP248(iuout,loutstr,100)
      IER=2
      GOTO 100

   1  call isunix(unixok)
      if(.NOT.unixok) return  ! if DOS return because of lack of I/O channel
      LNM=max(1,lnblnk(STRING))
      if(IOS.eq.2)then
        write(outs,*) 'EGETWIXML: permission error writing warning re: '
        call edisp(iuout,outs)
        call edisp(iuout,STRING(1:LNM))
      else
        write(outs,*) 'EGETWIXML: error writing warning re: '
        call edisp(iuout,outs)
        call edisp(iuout,STRING(1:LNM))
      endif
      return

      END


C **************** EGETWRXML
C EGETWRXML gets first word after position K from the STRING of
C characters and converts it into a real number RV, tests it against
C the minimum RMN and the maximum RMX and provides a warning
C message if RACT='W', a failure message if RACT='F' and does
C no range checking if RACT='-'. Words may be separated by blanks,
C commas, <, or tab: WORD,WORD,WORD or WORD WORD WORD or WORD, WORD, WORD
C are all valid.
      SUBROUTINE EGETWRXML(STRING,K,RV,RMN,RMX,RACT,MSG,IER)
#include "espriou.h"
      common/OUTIN/IUOUT,IUIN

C LOUTSTR is for messages to the user and should be long enough
c to prevent truncation of messge contents.
      CHARACTER*(*) STRING, MSG
      CHARACTER RACT*1,STR1*16,STR2*16,WORD*20,LOUTSTR*248,outs*124
      logical unixok

C Pick up line and length for error messages.
      ils=max(1,lnblnk(STRING))
      if(ils.gt.230)ils=230

      IER=0
      WORD=' '

C Use alternative call that also checks for <.
      CALL EGETWXML(STRING,K,WORD,'-','real',IER)
      IF(IER.NE.0) goto 1002
      read(WORD,*,ERR=1002)rv

C If range checking disabled, jump out of routine.
      IF(RACT.EQ.'-')RETURN

C Make up reporting string.
      CALL REL16STR(RV,STR1,IW1,IER)

C Check RV against minimum and respond based on RACT.
      IF(RV.LT.RMN)THEN
        CALL REL16STR(RMN,STR2,IW2,IER)
        IF(RACT.EQ.'W')THEN
          call edisp(iuout,' ')
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(LOUTSTR,'(4a)',IOSTAT=IOS,ERR=1)
     &        'WARNING: in ',currentfile(1:LN),': ',STRING(1:ils)
          else
            write(LOUTSTR,'(a,a)',IOSTAT=IOS,ERR=1)' WARNING in: ',
     &        STRING(1:ils)
          endif
          call edisp248(iuout,LOUTSTR,100)
          write(LOUTSTR,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &      ' value (',STR1(1:IW1),') < normal minimum ',STR2(1:IW2),'!'
          call edisp248(iuout,LOUTSTR,100)
        ELSEIF(RACT.EQ.'F')THEN
          call edisp(iuout,' ')
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(LOUTSTR,'(4a)',IOSTAT=IOS,ERR=1)
     &        'FAILURE: in ',currentfile(1:LN),': ',STRING(1:ils)
          else
            write(LOUTSTR,'(a,a)',IOSTAT=IOS,ERR=1)' FAILURE in: ',
     &      STRING(1:ils)
          endif
          call edisp248(iuout,LOUTSTR,100)
          write(LOUTSTR,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &       ' value (',STR1(1:IW1),') < allowable minumum ',
     &       STR2(1:IW2),'!'
          call edisp248(iuout,loutstr,100)
          IER=1
          RETURN
        ENDIF
      ELSEIF(RV.GT.RMX)THEN
        CALL REL16STR(RMX,STR2,IW2,IER)
        IF(RACT.EQ.'W')THEN
          call edisp(iuout,' ')
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(LOUTSTR,'(4a)',IOSTAT=IOS,ERR=1)
     &        'WARNING: in ',currentfile(1:LN),': ',STRING(1:ils)
          else
            write(LOUTSTR,'(a,a)',IOSTAT=IOS,ERR=1)' WARNING in: ',
     &      STRING(1:ils)
          endif
          call edisp248(iuout,LOUTSTR,100)
          write(LOUTSTR,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &       ' value (',STR1(1:IW1),') > normal  maximum ',
     &       STR2(1:IW2),'!'
          call edisp248(iuout,LOUTSTR,100)
        ELSEIF(RACT.EQ.'F')THEN
          call edisp(iuout,' ')
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(LOUTSTR,'(4a)',IOSTAT=IOS,ERR=1)
     &        'FAILURE: in ',currentfile(1:LN),': ',STRING(1:ils)
          else
            write(LOUTSTR,'(a,a)',IOSTAT=IOS,ERR=1)' FAILURE in: ',
     &      STRING(1:ils)
          endif
          call edisp248(iuout,LOUTSTR,100)
          write(LOUTSTR,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &       ' value (',STR1(1:IW1),') > allowable maximum ',
     &       STR2(1:IW2),'!'
          call edisp248(iuout,loutstr,100)
          IER=1
          RETURN
        ENDIF
      ENDIF

  100 RETURN

 1002 if(currentfile(1:2).ne.'  ')then
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        WRITE(LOUTSTR,'(6a)',IOSTAT=IOS,ERR=1) 'Failed in ',
     &    currentfile(1:LN),': conversion of ',
     &    MSG(1:LNM),' value in ',WORD
      else
        LNM=max(1,lnblnk(MSG))
        WRITE(LOUTSTR,1003,IOSTAT=IOS,ERR=1)MSG(1:LNM),WORD
 1003   FORMAT(' Failed: conversion of ',A,' value in ',A)
      endif
      CALL EDISP248(iuout,LOUTSTR,100)
      IER=2
      GOTO 100

   1  call isunix(unixok)
      if(.NOT.unixok) return  ! if DOS return because of lack of I/O channel
      LNM=max(1,lnblnk(STRING))
      if(IOS.eq.2)then
        write(outs,*) 'EGETWRXML: permission error writing warning re: '
        call edisp(iuout,outs)
        call edisp(iuout,STRING(1:LNM))
      else
        write(outs,*) 'EGETWRXML: error writing warning re: '
        call edisp(iuout,outs)
        call edisp(iuout,STRING(1:LNM))
      endif
      return

      END

C ***************** EGETEQDQXML
C EGETEQDQXML gets tag & first quoted PHRASE after position K 
C from the STRING of characters. Phrases can contain spaces
C and commas but not tabs.
C Provides a warning message if ACT='W', a failure message if ACT='F' and
C no message if ACT='-'.  The string returned has the quotes removed.
C Note it uses an internal string buffer which assumes that the
C phrase is less than 248 characters long.
C It is dealing with this pattern IDType="WMO"
      SUBROUTINE EGETEQDQXML(STRING,K,TAG,PHRASE,ACT,MSG,ier)
#include "espriou.h"
      common/OUTIN/IUOUT,IUIN

      CHARACTER*(*) TAG, PHRASE, STRING, MSG
      CHARACTER ACT*1,A*1,loutstr*124,outs*124
      character dq*1,sqleft*1,sqright*1
      logical unixok,isphrase

      dq = char(34)  ! double quote
      sqleft = char(96)  ! single quote left
      sqright = char(39)  ! single quote right

C LS is th maximum length of STRING, L the current position,
C LW the maximum length of PHRASE.
      ier=0
      TAG=' '; PHRASE=' '
      LS=LEN(STRING)
      LW=LEN(PHRASE)
      LT=LEN(TAG)
      L=0; isphrase=.false.

C Start by skipping blanks, commas and tabs before the PHRASE.
   10 K=K+1
      IF(K.GT.LS) GOTO 999
      A=STRING(K:K)
      IF(A.EQ.' '.OR.A.EQ.','.OR.(ICHAR(A).EQ.9)) GOTO 10

C Copy TAG or PHRASE from STRING, character by character until 
C a double quote or single quote is found (a quoted phrase 
C could include a comma, but not a tab)
   20 L=L+1
      IF(L.GE.LW) GOTO 100   ! at end of available phrase chars
      if(isphrase)then
        if(A.eq.dq.or.A.eq.sqleft.or.A.eq.sqright)then
          goto 100  ! we have terminating quote
        else
          PHRASE(L:L)=A
        endif
      else
        TAG(L:L)=A
      endif
      K=K+1
      IF(K.GT.LS) GOTO 999
      A=STRING(K:K)
      IF(ICHAR(A).EQ.9) GOTO 100
      if(A.eq."=")then
        isphrase=.true.  ! mark end of tag and advance
        K=K+1            ! increment K counter
        A=STRING(K:K)    ! instanciate A should be a quote
        L=0              ! reset L counter
        if(A.eq.dq.or.A.eq.sqleft.or.A.eq.sqright)then
          K=K+1          ! increment K counter
          A=STRING(K:K)  ! instanciate A again
          goto 20        ! loop back and deal with it
        endif
      endif
      if(A.eq.dq.or.A.eq.sqleft.or.A.eq.sqright)then

C Found terminating quote so process.
        GOTO 100
      endif
      GOTO 20

  100 return

  999 if(ACT.EQ.'-')then
        RETURN
      elseif(ACT.EQ.'W')then
        ier=1
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(loutstr,'(5a)',IOSTAT=IOS,ERR=1)
     &      'WARNING: in ',currentfile(1:LN),
     &      ' past end of line for ',MSG(1:LNM),' (phrase) in...'
        else
          WRITE(loutstr,'(3a)',IOSTAT=IOS,ERR=1)
     &      'WARNING: Past end of line for ',
     &      MSG(1:LNM),' (phrase) in...'
        endif
      elseif(ACT.EQ.'F')then
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(loutstr,'(5a)',IOSTAT=IOS,ERR=1)
     &      'FAILURE: in ',currentfile(1:LN),
     &      ' past end of line for ',MSG(1:LNM),' (phrase) in...'
        else
          WRITE(loutstr,'(3a)',IOSTAT=IOS,ERR=1)
     &      'FAILURE: Past end of line for ',
     &      MSG(1:LNM),' (phrase) in...'
        endif
      endif
      CALL EDISP248(iuout,loutstr,100)
      if(lnblnk(STRING).gt.123)then
        CALL EDISP248(iuout,STRING,100)
      else
        CALL EDISP(iuout,STRING)
      endif
      GOTO 100

   1  call isunix(unixok)
      if(.NOT.unixok) return  ! if DOS return because of lack of I/O channel
      LNM=max(1,lnblnk(STRING))
      if(IOS.eq.2)then
        write(outs,*) 
     &    'EGETEQDQXML: permission error writing warning re: '
        call edisp(iuout,outs)
        call edisp(iuout,STRING(1:LNM))
      else
        write(outs,*) 'EGETEQDQXML: error writing warning re: '
        call edisp(iuout,outs)
        call edisp(iuout,STRING(1:LNM))
      endif
      return

      END


C ***************** EGETRMXML
C EGETRMXML returns the remainder of a text string TSTR after position k
C in RSTR until a '<' is noticed.
      SUBROUTINE EGETRMXML(TSTR,K,RSTR,ACT,MSG,IER)
#include "espriou.h"
      common/OUTIN/IUOUT,IUIN

      CHARACTER*(*) TSTR,RSTR,MSG
      character ACT*1,A*1,loutstr*248
      logical unixok

C LS is th maximum length of STRING, L the current position,
C LR the maximum length of RSTR.
      RSTR=' '
      LS=LEN(TSTR)
      LR=LEN(RSTR)
      L=0

C Start by skipping blanks and tabs before TSTR.
   10 K=K+1
      IF(K.GT.LS) GOTO 999
      A=TSTR(K:K)
      IF(A.EQ.' '.OR.A.EQ.','.OR.(ICHAR(A).EQ.9)) GOTO 10

C Copy RSTR from TSTR character by character until either end
C of RSTR or TSTR or a '<' found..
   20 L=L+1
      IF(L.GT.LR)goto 100
      IF(A.EQ.'<') GOTO 100  ! start of next XML tag
      RSTR(L:L)=A
      K=K+1
      IF(K.GT.LS) goto 100
      A=TSTR(K:K)
      GOTO 20

  100 RETURN

  999 if(ACT.EQ.'-')then
        RETURN
      elseif(ACT.EQ.'W')then
        ier=1
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(loutstr,'(5a)',IOSTAT=IOS,ERR=1) 'WARNING in ',
     &      currentfile(1:LN),': no chars found for ',
     &      MSG(1:LNM),' string in:'
        else
          WRITE(loutstr,'(3a)',IOSTAT=IOS,ERR=1)
     &     'WARNING: no characters found for ',MSG(1:LNM),' string in:'
        endif
      elseif(ACT.EQ.'F')then
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(loutstr,'(5a)',IOSTAT=IOS,ERR=1) 'FAILURE in ',
     &      currentfile(1:LN),': no chars found for ',
     &      MSG(1:LNM),' string in:'
        else
          WRITE(loutstr,'(3a)',IOSTAT=IOS,ERR=1)
     &      'FAILURE: no characters found for ',MSG(1:LNM),' string in:'
        endif
      endif
      CALL EDISP248(iuout,loutstr,100)
      if(lnblnk(TSTR).gt.123)then
        CALL EDISP248(iuout,TSTR,100)
      else
        CALL EDISP(iuout,TSTR)
      endif
      GOTO 100

   1  call isunix(unixok)
      if(.NOT.unixok) return  ! if DOS return because of lack of I/O channel
      if(IOS.eq.2)then
        write(loutstr,*) 'EGETRM: permission error writing warning ',
     &    RSTR
      else
        write(loutstr,*) 'EGETRM: error writing warning re: ',RSTR
      endif
      call edisp248(iuout,loutstr,100)
      return

      END
      
C ***************** Matchzone
C Matchzone given zonestring returns the index of zone matching in array zn.
C IER=0 if ok, IER >0 indicates new zone.
      SUBROUTINE Matchzone(TSTR,ZN,nbz,index,IER)
      character*12 TSTR,zn(20)     ! gbxml zone name
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
C        dist=crowxyz(RVX,RVY,RVZ,szcoords(iz,ixx,1),
C     &     szcoords(iz,ixx,2),szcoords(iz,ixx,3))
C       write(6,*) 'dist is ',ixx,dist
        if(closex)then
          found=.true.  ! close enough to auto match
          match=ixx
          return
        endif
      enddo
      match=0
      return
      end

