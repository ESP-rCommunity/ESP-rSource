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

C The file emeta.f is a collection of support facilities for 
C reading and creating esp-r model meta files:
C  silentmodel creates a model from passed parameters.
C  silentzone: creates a zone (or limited complexity) based on parameters
C              passed to it.
C  silentread: Reads esp-r meta file zones geometry data.
C  metawrite:  Creates an esp-r meta file. Data input is from
C              the current zones common blocks.

C ************* silentmodel
C silentmodel creates a model from passed parameters.
C The parameters are:
C action (8 char) as follows: 
C    'new' creates model folders based on information
C       in root, mpath folder parameters. 
C    'within' assumes that the current folder will be used.
C root   (24 char) the root name of the model
C mpath  (72 char) the path to the folder with cfg file.
C folder (12 char) model folder layout: 'single' or 'distributed'
C weather (32 char) the weather file name (no path included)

C Current functionality is to update the climate & site data.

      subroutine silentmodel(action,root,mpath,folder,weather,ier)
#include "building.h"
#include "model.h"
#include "site.h"
#include "espriou.h"
#include "esprdbfile.h"
C esprdbfile.h supplies lclim.
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL

C Where ESP-r was installed (as recorded when it was compiled).
      common/deflt4/dinstpath
      character dinstpath*48

C Climate data.
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG

      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      common/XMLDATA/XMLMETRICS(200),inumXMLmetrics
      character XMLMETRICS*248

      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24

C Calendar commons.
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      character calename*32,calentag*12,calendayname*32 

C Passed in parameters.
      character action*8,root*24,mpath*72,folder*12,menu*72
      character fs*1           ! file separator
      character weather*32     ! file name for the location (no path)
      character weathergr*36   ! file name for weather related ground temps (no path)
      character groundfile*72  ! file name for weather related gorund temps (with path)
      character subpath*72

      character outs*124,OUTSTR*124
      logical unixok
      logical haveground ! set true if ground temp file has been found
      integer lnw   ! length of weather string.
      integer lnwg   ! length of weather ground string.
      integer igrfile ! unit number of ground file
      DIMENSION GVA(12)
      character inpxmlfl*248 !to write out the input.xml file
      
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
      write(6,*)  'action ',action,' ',root,' ',folder
      write(6,*)  'menu ',modeltitle
      write(6,*)  'mpath ',mpath
      write(6,*)  'weather ',weather

      if(action(1:3).eq.'new')then
        write(menu,'(a)') modeltitle(1:lnblnk(modeltitle))
        call pregist('sn',root,mpath,folder,menu,ier)
      elseif(action(1:6).eq.'within')then

C Assume the model folders exist when calling pregist.
        write(menu,'(a)') modeltitle(1:lnblnk(modeltitle))
        call pregist('sw',root,mpath,folder,menu,ier)
      endif

C If there is a line with the root name of a climate file look
C for a match in the current climate location and if there is
C one set the name of that file, scan it and find its site.
      if(weather(1:4).eq.'none')then
        continue
      else

C First check if the climate file exists in the subpath. Also
C make up a variant of the weather file name in case there
C might be a small 12 item file for ground temperatures which
C has a matching name.gnd.
        if(ICLIM.eq.0)then
          ICLIM=IFIL
        endif
        igrfile=IFIL+2  ! hopefully not used
        lnw=lnblnk(weather)
	write(weathergr,'(2a)') weather(1:lnw),'.gnd'
        lnwg=lnblnk(weathergr)

        write(subpath,'(4a)')
     &    dinstpath(1:lnblnk(dinstpath)),fs,'climate',fs
        write(LCLIM,'(2a)') subpath(1:lnblnk(subpath)),
     &    weather(1:lnw)
        write(groundfile,'(2a)') subpath(1:lnblnk(subpath)),
     &    weathergr(1:lnwg)
        write(currentfile,'(a)') LCLIM(1:lnblnk(LCLIM))
        CALL ERPFREE(ICLIM,ISTAT)
        CALL FPRAND(ICLIM,ISTAT,144,0,LCLIM)
        if(ISTAT.ge.0)then
          CALL ERPFREE(ICLIM,ISTAT)
	  
C Also check to see if a ground temperature file exists
          write(currentfile,'(a)') groundfile(1:lnblnk(groundfile))
          call FPOPEN(igrfile,ISTAT,1,0,groundfile)
          if(ISTAT.ge.0)then
            haveground=.true.
            CALL ERPFREE(igrfile,ISTAT)
          endif
          goto 75
        else

C Also look in the folder where the model is being created.
          write(LCLIM,'(3a)') mpath(1:lnblnk(mpath)),fs,
     &      weather(1:lnw)
          write(currentfile,'(a)') LCLIM(1:lnblnk(LCLIM))
          write(groundfile,'(3a)') mpath(1:lnblnk(mpath)),fs,
     &      weathergr(1:lnwg)

          CALL ERPFREE(ICLIM,ISTAT)
          CALL FPRAND(ICLIM,ISTAT,144,0,LCLIM)
          if(ISTAT.ge.0)then
            CALL ERPFREE(ICLIM,ISTAT)
	  
C Also check to see if a ground temperature file exists
            write(currentfile,'(a)') groundfile(1:lnblnk(groundfile))
            call FPOPEN(igrfile,ISTAT,1,0,groundfile)
            if(ISTAT.ge.0)then
              haveground=.true.
              CALL ERPFREE(igrfile,ISTAT)
            endif
            goto 75
          else

C Also look in the model dbs folder.
            write(LCLIM,'(5a)') mpath(1:lnblnk(mpath)),fs,
     &        dbspth(1:lnblnk(dbspth)),fs,weather(1:lnw)
            write(currentfile,'(a)') LCLIM(1:lnblnk(LCLIM))
            write(groundfile,'(5a)') mpath(1:lnblnk(mpath)),fs,
     &        dbspth(1:lnblnk(dbspth)),fs,weathergr(1:lnwg)
     
            CALL ERPFREE(ICLIM,ISTAT)
            CALL FPRAND(ICLIM,ISTAT,144,0,LCLIM)
            if(ISTAT.ge.0)then
              CALL ERPFREE(ICLIM,ISTAT)
	  
C Also check to see if a ground temperature file exists
              write(currentfile,'(a)') groundfile(1:lnblnk(groundfile))
              call FPOPEN(igrfile,ISTAT,1,0,groundfile)
              if(ISTAT.ge.0)then
                haveground=.true.
                CALL ERPFREE(igrfile,ISTAT)
              endif
              goto 75
            else
              write(currentfile,'(a)') DCLIM(1:lnblnk(DCLIM))
              CALL ERPFREE(ICLIM,ISTAT)
              CALL FPRAND(ICLIM,ISTAT,144,0,DCLIM)
              if(ISTAT.ge.0)then
                CALL ERPFREE(ICLIM,ISTAT)
                LCLIM=DCLIM
                goto 75
              else

C Fell through, no change in climate and location.
                call edisp(iuout,'No climate data located.')
                goto 74
              endif
            endif
          endif
        endif

C Impose climate site data on the model.
  75    continue
        CALL ERPFREE(ICLIM,ISTAT)
        CALL FPRAND(ICLIM,ISTAT,144,1,LCLIM)
        IREC=366
        READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)ICYEAR,ICDNGH
        IREC=368
        READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CLAT,CLONG
        CALL ERPFREE(ICLIM,ISTAT)
        sitelat=CLAT
        sitelongdif=CLONG
        IYEAR=ICYEAR
        write(outs,'(A,F7.3,A,F7.3,a,i4,i5,a,2F7.3,a)')
     &    'The climate lat is ',clat,' & long diff is ',
     &    clong,' & Year DN flag ',IYEAR,ICDNGH,
     &    '. Previously used ',sitelat,sitelongdif,'.'
        call edisp(iuout,outs)

C Write calendar information (just the default one for the moment)       
        calename='standard weekday Sat & Sun'
        nbdaytype=3
        nbcaldays(1)=0
        calentag(1)='weekdays'
        calendayname(1)='weekdays (all year)'
        nbcaldays(2)=0
        calentag(2)='saturday'
        calendayname(2)='Saturdays (all year)'
        nbcaldays(3)=0
        calentag(3)='sunday'
        calendayname(3)='Sundays (all year)'
        calentag(4)='-'
        calentag(5)='-'
        calentag(6)='-'
        calentag(7)='-'
        calentag(8)='-'
        calentag(9)='-'
        calendayname(4)='-'
        calendayname(5)='-'
        calendayname(6)='-'
        calendayname(7)='-'
        calendayname(8)='-'
        calendayname(9)='-'
        do 542 ijd=1,365

C for julian day ijd find month and day of month and day of week.
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
  542   continue                    	
  
C If there is ground temperature information scan that file and
C put the data in UGRDTP.
        if(haveground)then
          write(currentfile,'(a)') groundfile(1:lnblnk(groundfile))
          call FPOPEN(igrfile,ISTAT,1,0,groundfile)
          NGRDP=1
          CALL STRIPC(igrfile,OUTSTR,0,ND,1,'ground data',IER)
	  
          DO 29 J=1,12
            CALL EGETWR(OUTSTR,K,valy,0.,0.,'-','ground temp',IER)
            UGRDTP(J,NGRDP)=valy
   29     CONTINUE

C Debug.
          CALL EDISP(iuout,' From file Ground temp profile Jan-Dec:')
          WRITE(OUTS,'(12F6.1)')(UGRDTP(J,1),J=1,12)
          CALL EDISP(iuout,OUTS)

          CALL ERPFREE(igrfile,ISTAT)
        else
          NGRDP=0    ! no user defined ground temperature profiles.
        endif
        CALL EMKCFG('-',IER)

C Also write the xml input file if there is a metric defined in the flat file
        if(inumXMLmetrics.gt.0)then
          if(unixok)then
            fs = char(47)
          else
            fs = char(92)
          endif

C Write the XML input file
          write(inpxmlfl,'(3a)') mpath(1:lnblnk(mpath)),fs,
     &    'cfg/input.xml'
          iuf=ifil+2
          call efopseq(iuf,inpxmlfl,3,ier)
          write(iuf,'(a)',iostat=ios) 
     &      '<?xml version="1.0" encoding="UTF-8"?>'
          write(iuf,'(a)',iostat=ios)'<configuration>'
          write(iuf,'(a)',iostat=ios) 
     &      '  <apply_style_sheet>false</apply_style_sheet>'
          write(iuf,'(a)',iostat=ios) 
     &      '  <dump_all_data>false</dump_all_data>'  
          write(iuf,'(a)',iostat=ios) 
     &      '  <hierarchy>flat</hierarchy>'  
          write(iuf,'(a)',iostat=ios) 
     &      '  <link_style_sheet>false</link_style_sheet>'  
          write(iuf,'(a)',iostat=ios) 
     &      '  <output_dictionary>true</output_dictionary>'  
          write(iuf,'(a)',iostat=ios) 
     &'  <report_startup_period_data>false</report_startup_period_data>'
          write(iuf,'(a)',iostat=ios) 
     &      '  <save_to_disk>false</save_to_disk>'  
          write(iuf,'(a)',iostat=ios) 
     &      '  <time_step_averaging>true</time_step_averaging>'  
          write(iuf,'(a)',iostat=ios) 
     &      '  <enable_xml_wildcards>true</enable_xml_wildcards>'

C Loop around the possible defined xml outputs
          do 200 iwrxmlout=1,inumXMLmetrics
          write(iuf,'(3a)',iostat=ios)'  <step_variable>', 
     &      XMLMETRICS(iwrxmlout)(1:lnblnk(XMLMETRICS(iwrxmlout))),
     &     '</step_variable>'
 200      continue       
          write(iuf,'(a)',iostat=ios)'</configuration>'
          call erpfree(iuf,istat)
        endif
      
C Fall through point if no climates located.
  74    continue
      endif

C Other logic here....

      return

C Error trap. Set ier as two.
 1000 WRITE(outs,774)ISTAT
  774 FORMAT(' Error ',I7,' reading Climate db information.')
      call edisp(iuout,outs)
      call edisp(iuout,' ')
      ier=2
      RETURN

      end

C ************* silentzone 
C silentzone creates a zone based on meta parameters passed
C to it.
C shape (8 char) box/extrude
C nbwalls (integer) how many walls (e.g. 4 for a box, 3-20 for extrude). Note
C   the top surface is assumed to be index nbwalls+1 and the base surface is
C   assumed to be index nbwalls+2
C nbmass (integer) how many pairs of mass walls to append to zone. Note these
C   surfaces are not included in the *surface list
C origin (real * 3) XYZ of the origin in the case of shape=box. For shape=extrude
C   the 1st value is Z point of base, 2nd is the ceiling Z value, 3rd not used.
C size (real *3) for shape=box this is the length/width/height (m) and for
C   shape=extrude this should be zero
C nbpolyc (integer) number of initial coordinates
C coords (real array (nbwalls,3)) for shape=box ignore, for shape=extrude the
C   1st value is X and 2nd is Y (in world coordinates) following the normal
C   floor plan extrusion conventions. For shape=poly the Z is used.
C list (integer array (nbwalls,24) equivalent to jvn() for shape=poly
C nbedges (integer array (nbwalls) equivalent to nver() for shape=poly
C rotateit (real) angle to rotate the zone using the normal conventions and assuming
C   rotation is about the site origin
C hasglaze (real array) for each surface in the initial shape a zero has no glazing
C   and non-zero is the percentage of the parent surface for the glazing. It is
C   assumed that a default shape for the glazing is ok.
C hasdoor (real array) for each surface in the initial shape a zero indicates
C   no door and a non-zero indicates the width of the door (m).
C hasname (string array) the surface name for each surface
C hasconstr (string array) the name of the MLC to use for each surface
C boundarytype (integer (MS,3) the connection data for each surface.
C datamass (real 4*7) values to create mass
C textmass (char *32 4*3) text associated with mass
C use integer*3 directives for zone use pattern.,
c usefile char*32 file name (w/o path) of pattern operation file.
C IER=0 OK. 
      subroutine silentzone(ICOMP,name,shape,nbwalls,nbmass,nbedges,
     &  nbpolyc,list,origin,size,coords,rotateit,
     &  nbglz,hasglaze,nbdoor,hasdoor,hasname,hasconstr,
     &  boundarytype,datamass,textmass,use,usefile,IER)
#include "building.h"
#include "model.h"
#include "site.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/user/browse
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      common/cctlnm/ctldoc,lctlf
      common/cctl/icascf(mcom)

C Where ESP-r was installed (as recorded when it was compiled).
      common/deflt4/dinstpath
      character dinstpath*48

C Version of operations file. ip3ver=0 standard, =1 sorted with header
C =2 with more than 3 day types
      common/p3ver/ip3ver(MCOM)
      COMMON/P2N/NAC(MDTY),IACS(MDTY,MA),IACF(MDTY,MA),ACI(MDTY,MA),
     &           ACV(MDTY,MA),IPT(MDTY,MA),TA(MDTY,MA)
      INTEGER NAC,IACS,IACF,IPT
      REAL ACI,ACV,TA
      COMMON/P3N/NCAS(MDTY),ICGS(MDTY,MC),ICGF(MDTY,MC),
     &           CMGS(MDTY,MC),CMGL(MDTY,MC),RADC(MDTY,MC),CONC(MDTY,MC)
      INTEGER NCAS,ICGS,ICGF
      REAL CMGS,CMGL,RADC,CONC

      CHARACTER oprdesc*248,ventdesc*248,ctlstr*24
      COMMON/P1/oprdesc(MCOM),ventdesc(MCOM),ctlstr(MCOM,MDTY)

      common/loadlabel/lodlabel(mcom,7)
      CHARACTER*6 lodlabel

      COMMON/Vld20/Vldtng
      logical Vldtng      
      common/SLNTFULL/ifullysilent
      integer ifullysilent !flag to drive the silent model creation without
                           !questions about the operation files data

C Paremeters passed in:
      character name*12   ! name given to the zone
      character shape*8   ! box or extrude
      integer nbwalls     ! nb of vertical walls
      integer nbedges(MS) ! nb of edges per surface in poly shaped zone
      integer list(MS,24) ! indices of coords making up edges of each surface
      real origin(3)      ! values dependant on shape
      real datamass(4,7)  ! origin and size and orientations of mass (4 per zone)
      real size(3)        ! values dependant on shape
      real coords(MTV,3)  ! all of the coordinates in the zone X/Y/Z
      real rotateit       ! angle of rotation
      real hasglaze(MS)   ! percentage of glazing in each surface of intial shape
      real hasdoor(MS)    ! width of door in each surface of the initial shape
      character hasconstr*32
      dimension hasconstr(MS)  ! construction to use for each surface
      character hasname*12
      dimension hasname(MS)  ! name to use for each surface
      integer boundarytype(MS,3)
      character use*8,usefile*32  ! usage pattern directives
      dimension use(3)

C Stings associated with internal mass. 1=surface name, 2=construction, 3=optics
      character textmass*32 
      dimension textmass(4,3)
     
      DIMENSION XX(MS),YY(MS)
      CHARACTER SOTHER*24,SNAME*12,SMLCN*32,SVFC*4,SOTF*32,SUSE*8
      CHARACTER SPARENT*12
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      character fs*1
      character ctldoc*248,LCTLF*72,zd*64
      character DFILE*72,CFILE*72,OFILE*72,TFILE*72
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24
      character act*2
      character subpath*72,sfile*144
      character icact*8,iwatt*8,afact*8,iflow*8,vflow*8
      character sn*12,constr*12,opt*12,outs*124
      LOGICAL browse,OKC,XST,unixok
      logical willneedtmc  !  if a tmc file will be required.
      logical closev       !  mass is close to vertical
      LOGICAL QUIET

C Strings for surface attributes to pass to insrec.
      character rsname*12,rsotf*32,rsmlcn*32,rsuse1*8,rsuse2*8
      character rsparent*12
      integer newsurf   ! index of surface when adding door/glazing
      integer ibcs      ! connection associated with base surface

C If browsing then user cannot create a new zone.
      if(browse)then
        call usrmsg('Cannot update model while in browse',
     &    'mode, you must `own` the model!','W')
        return
      endif

C Initialise flag
      ifullysilent=0
      
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

C Creation of a new zone, begin with name, trapping out
C illegal characters.
      call tstamp('>','PRJ: new zone')
      call st2name(name,zname(ICOMP))
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
     &  shape(1:lnblnk(shape)),' & attributes.'
      zdesc(ICOMP)=zd
      lnzdesc(ICOMP)=lnblnk(zd)  ! update the length of this string.

C Choices based on the value of shape. 
      IER=0
      if(shape(1:3).eq.'box')then
        ishape=1
      elseif(shape(1:7).eq.'extrude')then
        ishape=2
      elseif(shape(1:4).eq.'poly')then
        ishape=3
      else
        ier=1
        return
      endif
      IF(ishape.eq.1)THEN
        XOC=origin(1)
        YOC=origin(2)
        ZOC=origin(3)
        DXC=size(1)
        DYC=size(2)
        DZC=size(3)

        ANG=rotateit

C Convert box into a gen description.
        CALL ERECC(XOC,YOC,ZOC,DXC,DYC,DZC,ANG)
        NSUR=6
        IUZBASEA(icomp)=0
        IBASES(ICOMP,1)=6
        IZBASELIST(ICOMP)=1
      ELSEIF(ishape.eq.2)THEN

C User begins with an extruded shape.
        Z1=origin(1)
        Z2=origin(2)
        NW=nbwalls

        DO 45 IW=1,NW
          XX(IW)=coords(IW,1)
          YY(IW)=coords(IW,2)
   45   CONTINUE

        AR=rotateit

C Convert into a gen body and rotate if required.
        CALL EREGC(NW,Z1,Z2,XX,YY)
        IF(AR.LT.-.01.OR.AR.GT..01)then
          x1=XX(1)
          y1=YY(1)
          CALL ESCROT(AR,x1,y1)
        endif
        NSUR=NW+2
        IUZBASEA(icomp)=0
        IBASES(ICOMP,1)=NSUR
        IZBASELIST(ICOMP)=1
      ELSEIF(ishape.eq.3)THEN

C For a poly shaped zone use similar logic to reading geometry file.
        NSUR=nbwalls
        NZSUR(ICOMP)=NSUR
        NTV=nbpolyc
        NZTV(ICOMP)=NTV
        AR=rotateit
        DO 62 I=1,NTV
          X(I)=coords(I,1)
          Y(I)=coords(I,2)
          Z(I)=coords(I,3)
   62   CONTINUE
        DO 10 I=1,NSUR
          NVER(I)=nbedges(I)
          DO 12 KV=1,NVER(I)
            JVN(I,KV)=list(I,KV)
   12     CONTINUE
   10   CONTINUE
        IUZBASEA(icomp)=0
        IBASES(ICOMP,1)=NSUR  ! initial guess, need to check
        IZBASELIST(ICOMP)=1

C Rotate if required.
        if(AR.LT.-.01.OR.AR.GT..01)then
          x1=X(1)
          y1=Y(1)
          CALL ESCROT(AR,x1,y1)
        endif

      ENDIF

C Begin with default assumptions for each surface then overwrite.
      CALL FILSUR(ICOMP,0)

C Update the connection list.
      ICCC=NCON
      DO 32 ICC=1,NSUR
        ICCC=ICCC+1
        IC1(ICCC)=ICOMP
        IE1(ICCC)=ICC
        ICT(ICCC)=boundarytype(icc,1)  ! used passed in boundarytype
        IC2(ICCC)=boundarytype(icc,2)
        IE2(ICCC)=boundarytype(icc,3)

C Instanciate the surface names and the constructions.
        write(sname(icomp,icc),'(a)') hasname(icc)
        write(smlcn(icc),'(a)') hasconstr(icc)

C Loop thru all of the constructions to find the matching construction
C and then see if it is opaque or transparent and copy out the relevant
C section of the string DESC.
        imatch=0
        do 5 ii=1,nmlc
          if(SMLCN(icc)(1:12).eq.DESC(ii)(1:12)) then
            imatch=ii          
          endif
  5     continue
        if(imatch.eq.0) then
          call edisp(iuout,'Warning: no matching MLC defined!')
          SOTF(icc)='UNKNOWN'
        else
          if(DESC(imatch)(15:18).EQ.'OPAQ')then
            SOTF(icc)='OPAQUE'
          else
            WRITE(SOTF(icc),'(A)') DESC(imatch)(21:32)
            willneedtmc=.true.
          endif
        endif
        SPARENT(icc)='-'
        SUSE(icc,1)='-'
        SUSE(icc,2)='-'

C Use boundarytype to instanciate sother & ssother << to be done >>
        IF(boundarytype(icc,1).EQ.0)THEN
          SOTHER(icc,1)='EXTERIOR'
        ELSEIF(boundarytype(icc,1).EQ.1)THEN
          SOTHER(icc,1)='SIMILAR'
        ELSEIF(boundarytype(icc,1).EQ.2)THEN
          SOTHER(icc,1)='CONSTANT'
        ELSEIF(boundarytype(icc,1).EQ.3)THEN
          SOTHER(icc,1)='ANOTHER'
        ELSEIF(boundarytype(icc,1).EQ.4)THEN
          SOTHER(icc,1)='GROUND'
        ELSEIF(boundarytype(icc,1).EQ.5)THEN
          SOTHER(icc,1)='ADIABATIC'
        ELSEIF(boundarytype(icc,1).EQ.6)THEN
          SOTHER(icc,1)='BASESIMP'
        ELSEIF(boundarytype(icc,1).EQ.7)THEN
          SOTHER(icc,1)='IDENT_CEN'
        ELSE

C Reset to represent UNKNOWN
          SOTHER(icc,1)='UNKNOWN'
        ENDIF
        write(SOTHER(icc,2),'(i2.2)') boundarytype(icc,2)
        write(SOTHER(icc,3),'(i2.2)') boundarytype(icc,3)

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
      if(ishape.eq.1)then
        ibcs=IZSTOCN(icomp,IBASES(ICOMP,1))
        ZBASEA(icomp)= SSNA(ibcs)
      elseif(ishape.eq.2)then
        ibcs=IZSTOCN(icomp,IBASES(ICOMP,1))
        ZBASEA(icomp)= SSNA(ibcs)
      elseif(ishape.eq.3)then

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
      NCOMP=NCOMP+1  ! Temporarily update NCOMP for geowrite use.
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
        NCOMP=NCOMP-1  ! Temporarily decrement NCOMP.
        ICTLF=IFIL+1
        CALL ERPFREE(ICTLF,ISTAT)
        call FINDFIL(LCTLF,XST)
        IF(XST) CALL EZCTLR(ICTLF,0,IUOUT,IER)
        NCOMP=NCOMP+1  ! Re-establish NCOMP.
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

C Next, if there are doors make standard calls to insert them into
C the parent surface. To avoid clashes with any subsequent windows
C place the door offset to the right (get bounding box via a call
C to SURLEHI with some space between it and the right edge of the
C parent surface.  Later add logic to better place the door..
      if(nbdoor.gt.0)then
        do 78 ids=1,NZSUR(ICOMP)
          if(hasdoor(ids).gt.0.0)then
            call georead(IFIL+2,LGEOM(ICOMP),ICOMP,1,iuout,IER)
            newsurf=nsur+1
            isurf=ids
            CALL SURLEHI(isurf,XYMAX,ZMAX)  ! get width of parent
            XOC=XYMAX-(hasdoor(ids)+0.3)
            ZOC=0.0
            XW=hasdoor(ids)
            if(ZMAX.gt.2.3)then
              ZH=2.1
            else
              ZH=ZMAX-0.3
            endif
            act='sd'
            write(rsname,'(a)') hasname(newsurf)
            write(rsmlcn,'(a)') hasconstr(newsurf)

C Loop thru all of the constructions to find the matching construction
C and then see if it is opaque or transparent and copy out the relevant
C section of the string DESC.
            imatch=0
            do 56 ii=1,nmlc
              if(rsmlcn(1:12).eq.DESC(ii)(1:12)) then
                imatch=ii          
              endif
 56         continue
            if(imatch.eq.0) then
              rsotf='UNKNOWN'
            else
              if(DESC(imatch)(15:18).EQ.'OPAQ')then
                rsotf='OPAQUE'
              else
                write(rsotf,'(A)') DESC(imatch)(21:32)
                willneedtmc=.true.
              endif
            endif
            rsuse1='DOOR'
            rsuse2='CLOSED'
            write(rsparent,'(a)') sname(icomp,isurf)
            call INSREC(0,iuout,ICOMP,isurf,act,XOC,ZOC,XW,ZH,
     &        rsname,rsotf,rsmlcn,rsuse1,rsuse2,rsparent,IER)

C Connection information needs to be updated.
            ibcs=IZSTOCN(icomp,newsurf)
            ICT(ibcs)=boundarytype(newsurf,1)  ! used passed in boundarytype
            IC2(ibcs)=boundarytype(newsurf,2)
            IE2(ibcs)=boundarytype(newsurf,3)

C Use boundarytype to instanciate sother & ssother << to be done >>
            IF(boundarytype(newsurf,1).EQ.0)THEN
              SOTHER(newsurf,1)='EXTERIOR'
            ELSEIF(boundarytype(newsurf,1).EQ.1)THEN
              SOTHER(newsurf,1)='SIMILAR'
            ELSEIF(boundarytype(newsurf,1).EQ.2)THEN
              SOTHER(newsurf,1)='CONSTANT'
            ELSEIF(boundarytype(newsurf,1).EQ.3)THEN
              SOTHER(newsurf,1)='ANOTHER'
            ELSEIF(boundarytype(newsurf,1).EQ.4)THEN
              SOTHER(newsurf,1)='GROUND'
            ELSEIF(boundarytype(newsurf,1).EQ.5)THEN
              SOTHER(newsurf,1)='ADIABATIC'
            ELSEIF(boundarytype(newsurf,1).EQ.6)THEN
              SOTHER(newsurf,1)='BASESIMP'
            ELSEIF(boundarytype(newsurf,1).EQ.7)THEN
              SOTHER(newsurf,1)='IDENT_CEN'
            ELSE

C Reset to represent UNKNOWN
              SOTHER(newsurf,1)='UNKNOWN'
            ENDIF
            write(SOTHER(newsurf,2),'(i2.2)') boundarytype(newsurf,2)
            write(SOTHER(newsurf,3),'(i2.2)') boundarytype(newsurf,3)
            SSOTHER(ibcs,1)=SOTHER(newsurf,1)
            SSOTHER(ibcs,2)=SOTHER(newsurf,2)
            SSOTHER(ibcs,3)=SOTHER(newsurf,3)

            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          endif
  78    continue
      endif

C Next, if there are windows make standard calls to insert them into
C the parent surface as a percentage of area.
      if(nbglz.gt.0)then
        do 77 ids=1,NZSUR(ICOMP)
          if(hasglaze(ids).gt.0.0)then
            call georead(IFIL+2,LGEOM(ICOMP),ICOMP,1,iuout,IER)
            newsurf=nsur+1
            isurf=ids
            act='sp'
            XOC=hasglaze(ids)
            ZOC=0.0
            XW=0.0
            ZH=0.0
            write(rsname,'(a)') hasname(newsurf)
            write(rsmlcn,'(a)') hasconstr(newsurf)

C Loop thru all of the constructions to find the matching construction
C and then see if it is opaque or transparent and copy out the relevant
C section of the string DESC.
            imatch=0
            do 55 ii=1,nmlc
              if(rsmlcn(1:12).eq.DESC(ii)(1:12)) then
                imatch=ii          
              endif
 55         continue
            if(imatch.eq.0) then
              rsotf='UNKNOWN'
            else
              if(DESC(imatch)(15:18).EQ.'OPAQ')then
                rsotf='OPAQUE'
              else
                write(rsotf,'(A)') DESC(imatch)(21:32)
                willneedtmc=.true.
              endif
            endif
            rsuse1='C-WINDOW'
            rsuse2='CLOSED'
            write(rsparent,'(a)') sname(icomp,isurf)
            call INSREC(0,iuout,ICOMP,isurf,act,XOC,ZOC,XW,ZH,
     &        rsname,rsotf,rsmlcn,rsuse1,rsuse2,rsparent,IER)

C Connection information needs to be updated.
            ibcs=IZSTOCN(icomp,newsurf)
            ICT(ibcs)=boundarytype(newsurf,1)  ! used passed in boundarytype
            IC2(ibcs)=boundarytype(newsurf,2)
            IE2(ibcs)=boundarytype(newsurf,3)

C Use boundarytype to instanciate sother & ssother << to be done >>
            IF(boundarytype(newsurf,1).EQ.0)THEN
              SOTHER(newsurf,1)='EXTERIOR'
            ELSEIF(boundarytype(newsurf,1).EQ.1)THEN
              SOTHER(newsurf,1)='SIMILAR'
            ELSEIF(boundarytype(newsurf,1).EQ.2)THEN
              SOTHER(newsurf,1)='CONSTANT'
            ELSEIF(boundarytype(newsurf,1).EQ.3)THEN
              SOTHER(newsurf,1)='ANOTHER'
            ELSEIF(boundarytype(newsurf,1).EQ.4)THEN
              SOTHER(newsurf,1)='GROUND'
            ELSEIF(boundarytype(newsurf,1).EQ.5)THEN
              SOTHER(newsurf,1)='ADIABATIC'
            ELSEIF(boundarytype(newsurf,1).EQ.6)THEN
              SOTHER(newsurf,1)='BASESIMP'
            ELSEIF(boundarytype(newsurf,1).EQ.7)THEN
              SOTHER(newsurf,1)='IDENT_CEN'
            ELSE

C Reset to represent UNKNOWN
              SOTHER(newsurf,1)='UNKNOWN'
            ENDIF
            write(SOTHER(newsurf,2),'(i2.2)') boundarytype(newsurf,2)
            write(SOTHER(newsurf,3),'(i2.2)') boundarytype(newsurf,3)
            SSOTHER(ibcs,1)=SOTHER(newsurf,1)
            SSOTHER(ibcs,2)=SOTHER(newsurf,2)
            SSOTHER(ibcs,3)=SOTHER(newsurf,3)

            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          endif
  77    continue
      endif

C Next, if there are mass pairs to add make standard calls to insert them.
      if(nbmass.gt.0)then
        do 79 imu=1,nbmass
          call eclose(datamass(imu,1),90.0,0.01,closev)
          write(SN,'(a)') textmass(imu,1)(1:lnblnk(textmass(imu,1)))
          write(constr,'(a)') textmass(imu,2)(1:lnblnk(textmass(imu,2)))
          write(opt,'(a)') textmass(imu,3)(1:lnblnk(textmass(imu,3)))
          if(closev)then
            call addmass(ICOMP,'VM','s',datamass(imu,2),
     &        datamass(imu,3),datamass(imu,4),datamass(imu,7),
     &        datamass(imu,5),datamass(imu,6),SN,constr,OPT,INVT)
          else
            call addmass(ICOMP,'HM','s',datamass(imu,2),
     &        datamass(imu,3),datamass(imu,4),datamass(imu,7),
     &        datamass(imu,5),datamass(imu,6),SN,constr,OPT,INVT)
          endif

          call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
  79    continue
      endif
      CALL EMKCFG('-',IER)

C If no errors then proceed to create constructions files. The
C file name is already known, assume that attribution is complete
C so write the header of construction file so something exists and then
C call edcon to complete the process. If the file already exists
C it should be overwritten.
      IUF=IFIL+2
      LTHRM(ICOMP)=CFILE
      CALL EFOPSEQ(IUF,LTHRM(ICOMP),3,IER)
      WRITE(IUF,31,IOSTAT=IOS,ERR=146)
     &  zname(ICOMP)(1:lnzname(ICOMP)),CFILE(1:lnblnk(CFILE))
  31  FORMAT('# thermophysical properties of ',a,' defined in ',a,/,
     &'# no of |air |surface(from geo)| multilayer construction',/,
     &'# layers|gaps|  no.  name      | database name ')
      CALL ERPFREE(IUF,ISTAT)

      if(willneedtmc)then
        ITW(icomp)=1
        LTWIN(ICOMP)=TFILE
      endif
      QUIET=.true.
      CALL EDCON(0,iuout,ICOMP,QUIET,IER)  ! should auto create files.
      if(ier.ne.0)then 
        call usrmsg('Problem autogenerating zone construction',
     &    'files. Please check','W')
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
        lodlabel(icomp,4)='N/A   '  
        lodlabel(icomp,5)='Ann.El'  
        lodlabel(icomp,6)='N/A   ' 
        lodlabel(icomp,7)='N/A   '      

C Fill common blocks for new zone based on information in the
C pattern file.

C Set to current format
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
        call PROCESSOLDCAS(ICOMP,0,iuout,IER)
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
 
C ******************** SILENTREAD 
C SILENTREAD reads an esp-r meta file zones data. Data input is
C based on 3rd party generators. If act is '?' then return a
C menu and descriptive text block for the file. Otherwise scan
C and act on the information within the file.
 
       SUBROUTINE SILENTREAD(IUNIT,LFILE,act,silentreturndirec,IER)
       integer MSZ   ! number of silent zones array sizes
       PARAMETER (MSZ=10) 
#include "building.h"
#include "model.h"
#include "site.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "espriou.h"
C espriou.h provides currentfile.
#include "control.h"
#include "seasons.h"
C seasons.h provides typper and typsea
#include "ipvdata.h"
      
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

C Obstruction blocks via geometry.h.

      common/rpath/path
      character*72 path,odir    ! where are folders

      common/dllerr/dllsubr,dllmesg
      character dllsubr*12,dllmesg*124  ! messages if errors in dll

C Simulation parameter sets.
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh

C IPV description via ipvdata.h.

C Control documentation common
      common/ctlnm/znctldoc,plctldoc,flctldoc,elctldoc,glctldoc,
     &             opticdoc
      character znctldoc*248,plctldoc*248,flctldoc*248,elctldoc*248
      character glctldoc*248,opticdoc*248     
  
      common/cctlnm/ctldoc,lctlf
      character LCTLF*72,CTLDOC*248  
      
      common/cctl/icascf(mcom)
      integer icascf

      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      common/C21/IFCFG,cfgroot,LCFGF

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
     
      integer inumXMLmetrics !counts for the number of xml metrics defined
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,cfgroot*24,LCFGF*72
      integer IFCFG
               
      CHARACTER OUTSTR*248,WORD*32,outs*124
      character dstmp*24
      character word248*248

      character act*1  ! action requested.
      character ipvact*1 ! action for clearipvdat
      character simact*6 ! action for creating assessments.
      character subpath*72

C At the head of the file is a title and a description
C modeltitle (char 72) and modeldocblock (char 248) from model.h which can be
C used by scanning code to identify the contents of a silent file.
      character weather*32     ! file name for the location (no path)
      character*(*) lfile      ! name of the file
      character fs*1           ! file separator

C For passing to silentmodel.
      character actions*8,root*24,mpath*72,folder*12

C Same as local variable actions which is passed back to calling code.
      character silentreturndirec*8

      logical dll,unixok,XST,ok
      real valx,valy,valz      ! local variable for extrude co-ords.

C Paremeters passed to:silentzone
      character name*12   ! name given to the zone
      dimension name(MSZ)
      character shape*8   ! box or extrude or poly
      dimension shape(MSZ)
      character use*8,szuse*8,usefile*32  ! usage pattern directives
      dimension use(3),szuse(MSZ,3),usefile(MSZ)
      integer nbwalls(MSZ)     ! nb of vertical walls or polygons in poly zone
      integer nbmass(MSZ)      ! nb of paired mass rectangles
      integer nbedges(MS) ! nb of edges per surface in poly shaped zone
      integer isznbedges(MSZ,MS) ! nb of edges per surface in poly shaped zone
      integer list(MS,24) ! indices of coords making up edges of each surface
      integer iszlist(MSZ,MS,24) ! indices of coords making up edges of each surface
      integer nbglz(MSZ)       ! nb of windows (META directive)
      integer nbdoor(MSZ)      ! nb of doors (META directive)
      integer nbcord(MSZ)      ! nb of coordinates (for extruded or poly shape)
      real rotateit(MSZ)        ! angle of rotation
      real origin(3),szorigin(MSZ,3)      ! values dependant on shape
      real datamass(4,7),sdatamass(MSZ,4,7) ! origin and size and orientations of mass (4 per zone)
      real size(3),szsize(MSZ,3)        ! values dependant on shape
      real coords(MTV,3),szcoords(MSZ,MTV,3) 
      real hasglaze(MS),szhasglaze(MSZ,MS)   ! percentage of glazing in each surface of intial shape
      real hasdoor(MS),szhasdoor(MSZ,MS)    ! width of door in each surface of the initial shape

C Stings associated with internal mass. 1=surface name, 2=construction, 3=optics
      character textmass*32,stextmass*32
      dimension textmass(4,3),stextmass(MSZ,4,3)

      character hasconstr*32,szhasconstr*32
      dimension hasconstr(MS),szhasconstr(MSZ,MS)  ! construction to use for each surface
      character hasname*12,szhasname*12
      dimension hasname(MS),szhasname(MSZ,MS)  ! name to use for each surface
      integer boundarytype(MS,3),szboundarytype(MSZ,MS,3)

C Site related local variables.
      logical havesite       ! true if tokens included
      logical havesnowdays     ! true if days with snow defined
      character hourlysnowfile*72  ! same as SNFNAM
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

C If subpath is less than 72 char with odir + path use that
C otherwise just use path.
C Debug...
      subpath=' '
      lnodir=lnblnk(odir)
      lnpath=lnblnk(path)
      if(lnodir+lnpath+1.le.72)then
        write(subpath,'(3a)') odir(1:lnodir),fs,
     &  path(1:lnpath)
      else
        write(subpath,'(a)') path(1:lnpath)
      endif

C Clear the sz array structures. The logic is to scan all of the
C zone details and then process the zones after *end is detected
C in the file.
      if(act(1:1).ne.'?')then
        actions=' '
        root=' '
        folder=' '
        mpath=' '
        modeltitle=' '
        modeldocblock=' '
        weather=' '
        hourlysnowfile=' '
        groundreflmodel=0
        skyview=0.0
        groundview=0.0
        buildingview=0.0
        havesite=.false.
        havesnowdays=.false.
        havehourlysnowfile=.false.
        havegroundtempsets=.false.
        groundtempsets=0
        havesimparameters=.false.
        simstartup=0
        simzonetimestep=0
        simplanttimestep=0
        simsavelevel=0
        isilentncf=0
        do 42 iz=1,MSZ
          name(iz) = ' '
          shape(iz) = ' '
          szuse(iz,1) = ' '
          szuse(iz,2) = ' '
          szuse(iz,3) = ' '
          usefile(iz) = ' '
          nbwalls(iz) = 0
          nbmass(iz) = 0
          nbglz(iz) = 0
          nbdoor(iz) = 0
          nbcord(iz) = 0
          rotateit(iz) = 0
          szorigin(iz,1) = 0.0
          szorigin(iz,2) = 0.0
          szorigin(iz,3) = 0.0
          szsize(iz,1) = 0.0
          szsize(iz,2) = 0.0
          szsize(iz,3) = 0.0
          nbobs(iz) = 0
          ht_Setpoint(iz)= 0.0
          cl_Setpoint(iz)= 0.0
          i_ctl_link(iz)=0
          do 55 ibu=1,MB
            XOB(iz,ibu) = 0.0
            YOB(iz,ibu) = 0.0
            ZOB(iz,ibu) = 0.0
            DXOB(iz,ibu) = 0.0
            DYOB(iz,ibu) = 0.0
            DZOB(iz,ibu) = 0.0
            BANGOB(iz,ibu,1) = 0.0
            BANGOB(iz,ibu,2) = 0.0
            BANGOB(iz,ibu,3) = 0.0
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
            isznbedges(iz,isu)=0
            do 54 ivu=1,MV
              iszlist(iz,isu,ivu)=0
  54        continue
            szhasglaze(iz,isu)=0.0
            szhasdoor(iz,isu)=0.0
            szhasconstr(iz,isu)=' '
            szhasname(iz,isu)=' '
            szboundarytype(iz,isu,1)=0
            szboundarytype(iz,isu,2)=0
            szboundarytype(iz,isu,3)=0
  52      continue
          do 59 imu=1,4
            sdatamass(iz,imu,1)= 0.0
            sdatamass(iz,imu,2)= 0.0
            sdatamass(iz,imu,3)= 0.0
            sdatamass(iz,imu,4)= 0.0
            sdatamass(iz,imu,5)= 0.0
            sdatamass(iz,imu,6)= 0.0
            sdatamass(iz,imu,7)= 0.0
            stextmass(iz,imu,1)= ' '
            stextmass(iz,imu,2)= ' '
            stextmass(iz,imu,3)= ' '
  59      continue
  42    continue
      endif

      NS=0     ! temporary array for counting surfaces.
      nsz=0
      weather='none'

C Check if running in dll mode.
      call isadll(dll)

C Initialise geometry data file. and set currentfile.
      CALL EFOPSEQ(IUNIT,LFILE,1,IER)
      IF(IER.LT.0)THEN
        write(outs,'(3a)') 'Silent file ',LFILE(1:lnblnk(LFILE)),
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

C Read header lines from file, the 1.1 version looks like: 
C *silent_input
      CALL LSTRIPC(IUNIT,OUTSTR,99,ND,1,'silent line 1',IER)
      IF(IER.NE.0)goto 1001
      if(OUTSTR(1:13).eq.'*silent_input')then
        continue
      else

C If we have reached this position then it is an older geometry
C file so close the file and set error message.
        CALL ERPFREE(IUNIT,ios)
        ier=2
        call usrmsg('Not a silent instruction file.',LFILE,'W')
        return
      endif

C Read data line. If end of file found then jump
C to 1000 to see if there are zones to process.
  43  CALL LSTRIPC(IUNIT,OUTSTR,99,ND,1,'silent tags',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','*vertex or *surface tags',IER)
      if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then
        CALL EGETRM(OUTSTR,K,dstmp,'W','date stamp',IER)
        goto 43
      elseif(WORD(1:5).eq.'*menu')then  ! depreciated
        CALL EGETRM(OUTSTR,K,modeltitle,'W','meta title',IER)
        goto 43
      elseif(WORD(1:6).eq.'*title')then
        CALL EGETRM(OUTSTR,K,modeltitle,'W','meta title',IER)
        goto 43
      elseif(WORD(1:4).eq.'*doc')then
        CALL EGETRM(OUTSTR,K,modeldocblock,'W','meta documentation',
     &    IER)
        if(act(1:1).eq.'?')then
          return
        else
          goto 43
        endif
      elseif(WORD(1:7).eq.'*action')then

C An action line is like this:
C *action,new,boxer,single,/tmp/boxer
        CALL EGETW(OUTSTR,K,actions,'W','action string',IER)
        silentreturndirec=actions
        if(actions(1:3).eq.'new'.or.actions(1:6).eq.'within')then
          CALL EGETW(OUTSTR,K,root,'W','root name',IER)
          CALL EGETW(OUTSTR,K,folder,'W','folder directive',IER)
          CALL EGETRM(OUTSTR,K,mpath,'W','model folder',IER)

C Debug.
          write(6,*)  's action ',actions,' root ',root
          write(6,*)  's folder ',folder
          write(6,*)  's mpath ',mpath
        endif
        goto 43
      elseif(WORD(1:12).eq.'*assessments')then

C Get tags related to assessments: after the tag *assessments the
C following choices are possible:
C   `annual` (no further tokens)
C   `single` followed typically by `user` followed by pairs of julian dates
C   `winter` followed by `typical_week` or `season` or 'fortnight'
C   `spring` followed by `typical_week` or `season` or 'fortnight'
C   `summer` followed by `typical_week` or `season` or 'fortnight'
C   `autumn` followed by `typical_week` or `season` or 'fortnight'
C   `three_season` followed by `typical_week` `forthnight` `user` or `season` 
C   `five_season` followed by `typical_week` `forthnight` `user` or `season` 
        CALL EGETW(OUTSTR,K,WORD,'W','*assessment tags',IER)
        if(WORD(1:7).eq.'Default'.or.WORD(1:7).eq.'default')then
          nipvassmt=1
          simsavelevel=-1   ! signal to ipv2simpar to use defaults
          simact='i1d   '
        elseif(WORD(1:6).eq.'Annual'.or.WORD(1:6).eq.'annual')then
          nipvassmt=1
          simact='ias   '
        elseif(WORD(1:6).eq.'Single'.or.WORD(1:6).eq.'single')then
          CALL EGETW(OUTSTR,K,WORD,'W','Single tags',IER)
          if(WORD(1:4).eq.'User'.or.
     &       WORD(1:4).eq.'user')then
            CALL EGETWI(OUTSTR,K,iday1,1,365,'W','start julian',IER)
            CALL EGETWI(OUTSTR,K,iday2,1,365,'W','end julian',IER)
          endif
          nipvassmt=1
          simact='i1d   '
        elseif(WORD(1:6).eq.'Winter'.or.WORD(1:6).eq.'winter')then
          CALL EGETW(OUTSTR,K,WORD,'W','Winter tags',IER)
          if(WORD(1:12).eq.'Typical_week'.or.
     &       WORD(1:12).eq.'typical_week')then
            simact='icwint'
          elseif(WORD(1:9).eq.'Fortnight'.or.
     &           WORD(1:9).eq.'fortnight')then
            simact='icwinf'
          elseif(WORD(1:9).eq.'Season'.or.
     &           WORD(1:9).eq.'season')then
            simact='icwins'
          endif
          nipvassmt=1
        elseif(WORD(1:6).eq.'Spring'.or.WORD(1:6).eq.'spring')then
          CALL EGETW(OUTSTR,K,WORD,'W','Spring tags',IER)
          if(WORD(1:12).eq.'Typical_week'.or.
     &       WORD(1:12).eq.'typical_week')then
            simact='icsprt'
          elseif(WORD(1:9).eq.'Fortnight'.or.
     &           WORD(1:9).eq.'fortnight')then
            simact='icsprf'
          elseif(WORD(1:9).eq.'Season'.or.
     &           WORD(1:9).eq.'season')then
            simact='icsprs'
          endif
          nipvassmt=1
        elseif(WORD(1:6).eq.'Summer'.or.WORD(1:6).eq.'summer')then
          CALL EGETW(OUTSTR,K,WORD,'W','Summer tags',IER)
          if(WORD(1:12).eq.'Typical_week'.or.
     &       WORD(1:12).eq.'typical_week')then
            simact='icsumt'
          elseif(WORD(1:9).eq.'Fortnight'.or.
     &           WORD(1:9).eq.'fortnight')then
            simact='icsumf'
          elseif(WORD(1:9).eq.'Season'.or.
     &           WORD(1:9).eq.'season')then
            simact='icsums'
          endif
          nipvassmt=1
        elseif(WORD(1:6).eq.'Autumn'.or.WORD(1:6).eq.'autumn')then
          CALL EGETW(OUTSTR,K,WORD,'W','Autumn tags',IER)
          if(WORD(1:12).eq.'Typical_week'.or.
     &       WORD(1:12).eq.'typical_week')then
            simact='icautt'
          elseif(WORD(1:9).eq.'Fortnight'.or.
     &           WORD(1:9).eq.'fortnight')then
            simact='icautf'
          elseif(WORD(1:9).eq.'Season'.or.
     &           WORD(1:9).eq.'season')then
            simact='icauts'
          endif
          nipvassmt=1
        elseif(WORD(1:12).eq.'Three_season'.or.
     &         WORD(1:12).eq.'three_season')then
          CALL EGETW(OUTSTR,K,WORD,'W','three_season tags',IER)
          if(WORD(1:12).eq.'Typical_week'.or.
     &       WORD(1:12).eq.'typical_week')then
            simact='i3t   '
          elseif(WORD(1:4).eq.'User'.or.WORD(1:4).eq.'user')then
            CALL EGETWI(OUTSTR,K,iday1,1,365,'W','start julian',IER)
            CALL EGETWI(OUTSTR,K,iday2,1,365,'W','end julian',IER)
            CALL EGETWI(OUTSTR,K,iday3,1,365,'W','start julian',IER)
            CALL EGETWI(OUTSTR,K,iday4,1,365,'W','end julian',IER)
            CALL EGETWI(OUTSTR,K,iday5,1,365,'W','start julian',IER)
            CALL EGETWI(OUTSTR,K,iday6,1,365,'W','end julian',IER)
            simact='i3u   '
          elseif(WORD(1:9).eq.'season'.or.
     &           WORD(1:9).eq.'season')then
            simact='i3s   '
          endif
          nipvassmt=3
        elseif(WORD(1:11).eq.'Five_season'.or.
     &         WORD(1:11).eq.'five_season')then
          CALL EGETW(OUTSTR,K,WORD,'W','five_season tags',IER)
          if(WORD(1:12).eq.'Typical_week'.or.
     &       WORD(1:12).eq.'typical_week')then
            simact='i5t   '
          elseif(WORD(1:4).eq.'User'.or.WORD(1:4).eq.'user')then
            CALL EGETWI(OUTSTR,K,iday1,1,365,'W','start julian',IER)
            CALL EGETWI(OUTSTR,K,iday2,1,365,'W','end julian',IER)
            CALL EGETWI(OUTSTR,K,iday3,1,365,'W','start julian',IER)
            CALL EGETWI(OUTSTR,K,iday4,1,365,'W','end julian',IER)
            CALL EGETWI(OUTSTR,K,iday5,1,365,'W','start julian',IER)
            CALL EGETWI(OUTSTR,K,iday6,1,365,'W','end julian',IER)
            CALL EGETWI(OUTSTR,K,iday7,1,365,'W','start julian',IER)
            CALL EGETWI(OUTSTR,K,iday8,1,365,'W','end julian',IER)
            CALL EGETWI(OUTSTR,K,iday9,1,365,'W','start julian',IER)
            CALL EGETWI(OUTSTR,K,iday10,1,365,'W','end julian',IER)
            simact='i5u   '
          elseif(WORD(1:9).eq.'season'.or.
     &           WORD(1:9).eq.'season')then
            simact='i5s   '
          endif
          nipvassmt=5
        else
          nipvassmt=0
          simact='i1d   '
        endif

C Feedback. << but simact is not passed to anything >>
        write(outs,*) 'the value of simact is ',simact
	call edisp(iuout,outs)

        goto 43
      elseif(WORD(1:8).eq.'*cooling')then
        CALL EGETWR(OUTSTR,K,cl_Setpoint(nsz),-102.,102.,'W',
     &       'cool Setp',IER)
        goto 43
      elseif(WORD(1:5).eq.'*cord')then
        nbcord(nsz)=nbcord(nsz)+1   ! increment number of coordinates in zone
        nbc=nbcord(nsz)
        CALL EGETWR(OUTSTR,K,valx,0.,0.,'-','cord-x',IER)
        CALL EGETWR(OUTSTR,K,valy,0.,0.,'-','cord-y',IER)
        szcoords(nsz,nbc,1)=valx
        szcoords(nsz,nbc,2)=valy
        if(shape(nsz)(1:4).eq.'poly')then
          CALL EGETWR(OUTSTR,K,valz,0.,0.,'-','cord-z',IER)
          szcoords(nsz,nbc,3)=valz
        else
          szcoords(nsz,nbc,3)=0.0
        endif
        goto 43
      elseif(WORD(1:5).eq.'*door')then
        CALL EGETWI(OUTSTR,K,nbd,0,MS,'W','no of doors',IER)

C Scan for pairs of parent surface index and the door width.
        nbdoor(nsz)=nbd
        if(nbdoor(nsz).gt.0)then
          do 47 igl=1,nbdoor(nsz)
            CALL EGETWI(OUTSTR,K,iparen,1,MS,'W','parent surf',IER)
            CALL EGETWR(OUTSTR,K,val,0.,0.,'-','width',IER)
            szhasdoor(nsz,iparen)=val
  47      continue
        endif
        goto 43
      elseif(WORD(1:9).eq.'*end_zone')then
        goto 43
      elseif(WORD(1:5).eq.'*end ')then

C Jump to process the collected data.
        goto 1000
      elseif(WORD(1:6).eq.'*glaze')then
        CALL EGETWI(OUTSTR,K,nbg,0,MS,'W','no of glzaings',IER)

C Scan for pairs of parent surface index and the percentage of the parent
C surface area to have as glazing.
        nbglz(nsz)=nbg
        if(nbglz(nsz).gt.0)then
          do 144 igl=1,nbglz(nsz)
            CALL EGETWI(OUTSTR,K,iparen,1,MS,'W','parent surf',IER)
            CALL EGETWR(OUTSTR,K,val,0.,0.,'-','percent',IER)
            szhasglaze(nsz,iparen)=val
 144      continue
        endif
        goto 43
      elseif(WORD(1:19).eq.'*ground_refl_annual')then
        CALL EGETWR(OUTSTR,K,groundrefl,0.,1.,'W',
     &    'Ground reflectance annual value',IER)
        goto 43
      elseif(WORD(1:20).eq.'*ground_refl_monthly')then

C Ground reflectivity model, no-snow monthly albedos, snow albedo
        CALL EGETWI(OUTSTR,K,groundreflmodel,1,3,'F',
     &    'ground refl. model',IER)
        DO 441 I=1,12
          CALL EGETWR(OUTSTR,K,groundreflmonth(I),0.,1.,'W',
     &      'monthly gr. refl.',IER)
  441   CONTINUE
        CALL EGETWR(OUTSTR,K,snowgroundrefl,0.,1.,'W','snow refl.',IER)  
        goto 43
      elseif(WORD(1:8).eq.'*heating')then
        CALL EGETWR(OUTSTR,K,ht_Setpoint(nsz),-102.,102.,'W',
     &       'heat setp',IER)
        goto 43
      elseif(WORD(1:18).eq.'*hourly_snow_depth')then

C If ground refl model is 3 then this file has hourly data.
        CALL EGETRM(OUTSTR,K,hourlysnowfile,'W','snow file',IER)
        havehourlysnowfile=.true.
        goto 43
      elseif(WORD(1:14).eq.'*ideal_control')then
        i_ctl_link(nsz)=1
        goto 43
      elseif(WORD(1:4).eq.'*ipv')then
        goto 43
      elseif(WORD(1:5).eq.'*list')then

C The current value of NS should still point to the surface data
C that was just scanned. The *list line holds the number of
C edges and then the index of each coordinate (as in the normal
C geometry file).
        CALL EGETWI(OUTSTR,K,ival1,3,MV,'W','nb of edges',IER)
        isznbedges(nsz,NS)=ival1

C Now proceed to read vertices on one or more lines.
        DO 12 KV=1,ival1
          CALL EGETWI(OUTSTR,K,ival2,0,MTV,'F','vertex',IERV)
          IF(IERV.NE.0) THEN
            call edisp(ITRU,' reading continuation line...')
            CALL STRIPC(IUNIT,OUTSTR,0,ND,0,'vertex XYZ',IER)
            K=0
            CALL EGETWI(OUTSTR,K,ival2,0,MTV,'F','vertex',IERV)
          ENDIF
          iszlist(nsz,NS,KV)=ival2
   12   CONTINUE
        goto 43
      elseif(WORD(1:5).eq.'*mass')then

C The *mass keyword is followed by 9 tokens as follows:
        nbmass(nsz)=nbmass(nsz)+1
        nma=nbmass(nsz)
        CALL EGETW(OUTSTR,K,WORD,'W','mass vertical or horiz',IER)
        if(WORD(1:2).eq.'VM'.or.WORD(1:2).eq.'vm')then
          sdatamass(nsz,nma,1)= 90.0  ! signal vertical
        elseif(WORD(1:2).eq.'HM'.or.WORD(1:2).eq.'hm')then
          sdatamass(nsz,nma,1)= 0.0   ! signal horizontal
        endif
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','mass X org',IER)
        sdatamass(nsz,nma,2)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','mass Y org',IER)
        sdatamass(nsz,nma,3)=val1
        CALL EGETWR(OUTSTR,K,val1, -99., 99.,'W','mass Z org',IER)
        sdatamass(nsz,nma,4)=val1
        CALL EGETWR(OUTSTR,K,val1,   0., 99.,'W','mass X dis',IER)
        sdatamass(nsz,nma,5)=val1
        CALL EGETWR(OUTSTR,K,val1,   0., 99.,'W','mass Z dis',IER)
        sdatamass(nsz,nma,6)=val1
        CALL EGETWR(OUTSTR,K,val1,-359.,359.,'W','mass rotate',IER)
        sdatamass(nsz,nma,7)=val1
        CALL EGETW(OUTSTR,K,WORD,'W','mass name',IFLAG)
        write(stextmass(nsz,nma,1),'(a)') WORD(1:lnblnk(WORD))
        CALL EGETW(OUTSTR,K,WORD,'W','mass constr name',IFLAG)
        write(stextmass(nsz,nma,2),'(a)') WORD(1:lnblnk(WORD))
        write(stextmass(nsz,nma,3),'(a)') 'OPAQUE'   ! set as opaque
        goto 43
      elseif(WORD(1:16).eq.'*monthly_profile')then

C Ground monthly temperature profiles. 2nd item is the number of profiles.
C And this is followed by one line each for each profile.
        CALL EGETWI(OUTSTR,K,groundtempsets,1,MGRDP,'F','grn prfls',IER)
        IF(groundtempsets.eq.0) goto 43
        DO 28 IGRDP=1,groundtempsets
          CALL EGETWRA(IUNIT,GVA,12,-9.,99.,'W','Gr tmp',IER)
          DO 29 J=1,12
            groundtemps(J,IGRDP)=GVA(J)
   29     CONTINUE
  28    CONTINUE
        havegroundtempsets=.true.
        goto 43
      elseif(WORD(1:18).eq.'*monthly_snow_days')then

C Number of days with snow on the ground (if ground refl model 2)
        DO 442 I=1,12
          CALL EGETWI(OUTSTR,K,dayswithsnow(I),0,31,'W',
     &      'days with snow in each month',IER)
  442   CONTINUE
        havesnowdays=.true.
        goto 43
      elseif(WORD(1:8).eq.'*nbwalls')then

C Number of parent surfaces has a meaning for extruded shapes and set to 6 for
C box shapes.
        if(shape(nsz)(1:7).eq.'extrude')then
          CALL EGETWI(OUTSTR,K,nbw,1,MS,'W','nb parent surfs',IER)
          nbwalls(nsz)=nbw
        elseif(shape(nsz)(1:4).eq.'poly')then
          CALL EGETWI(OUTSTR,K,nbw,1,MS,'W','nb parent surfs',IER)
          nbwalls(nsz)=nbw
        else
          nbwalls(nsz)=4
        endif
        goto 43
      elseif(WORD(1:10).eq.'*shad_calc')then

C Shading calculation instructions.
        CALL EGETW(OUTSTR,K,WORD,'W','shad directive',IFLAG)
        if(WORD(1:4).eq.'none')then
          iaplic(nsz,1)=0
          nsurfcalc(nsz)=0
        else
          if(WORD(1:14).eq.'all_applicable')then
            iaplic(nsz,1)=1
          elseif(WORD(1:4).eq.'list')then
            iaplic(nsz,1)=0
          endif

C Read number of surfaces and then the list
          CALL EGETWI(OUTSTR,K,iv,0,MS,'-','nsurfcalc',IER)
          nsurfcalc(nsz)=iv
          IRVA=nsurfcalc(nsz)
          CALL EGETWIA(IUNIT,IVA,IRVA,0,MS,'F','lstsfcalc',IER)
          DO 342 ks=1,nsurfcalc(nsz)
            lstsfcalc(nsz,ks)=IVA(ks)
 342      CONTINUE
        endif
        goto 43
      elseif(WORD(1:11).eq.'*insol_calc')then

C Insolation calculation instructions.
        CALL EGETW(OUTSTR,K,WORD,'W','insolation directive',IFLAG)
        if(WORD(1:4).eq.'none')then
          iaplic(nsz,2)=0
          nsurfinso(nsz)=0
        else
          if(WORD(1:14).eq.'all_applicable')then
            iaplic(nsz,2)=1
          elseif(WORD(1:4).eq.'list')then
            iaplic(nsz,2)=0
          endif

C Applicable surfaces are the current exterior facing surfaces.
          CALL EGETWI(OUTSTR,K,iv,0,MS,'-','nsurfinso',IER)
          nsurfinso(nsz)=iv
          IRVA=nsurfinso(nsz)
          CALL EGETWIA(IUNIT,IVA,IRVA,0,MS,'F','isurfinso',IER)
          DO 344 ks=1,nsurfinso(nsz)
            isurfinso(nsz,ks)=IVA(ks)
 344      CONTINUE
        endif
        goto 43
      elseif(WORD(1:11).eq.'*solar_grid')then

C If there will be shading or insulation then the density of the
C grid is found after the token *solar_grid.
        CALL EGETWI(OUTSTR,K,NOX(nsz),4,20,'F','opq grid X',IER)
        CALL EGETWI(OUTSTR,K,NOZ(nsz),4,20,'F','opq grid Z',IER)
        goto 43
      elseif(WORD(1:5).eq.'*obs3')then

C A 3 rotation obstruction block to be associated with the current zone.
        nbobs(nsz)=nbobs(nsz)+1
        nob=nbobs(nsz)
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','X org',IER)
        XOB(nsz,nob)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','Y org',IER)
        YOB(nsz,nob)=val1
        CALL EGETWR(OUTSTR,K,val1, -99., 99.,'W','Z org',IER)
        ZOB(nsz,nob)=val1
        CALL EGETWR(OUTSTR,K,val1,   0., 99.,'W','X dis',IER)
        DXOB(nsz,nob)=val1
        CALL EGETWR(OUTSTR,K,val1,   0., 99.,'W','Y dis',IER)
        DYOB(nsz,nob)=val1
        CALL EGETWR(OUTSTR,K,val1,   0., 99.,'W','Z dis',IER)
        DZOB(nsz,nob)=val1
        CALL EGETWR(OUTSTR,K,val1,-359.,359.,'W','rot a',IER)
        BANGOB(nsz,nob,1)=val1
        CALL EGETWR(OUTSTR,K,val1,-180.,180.,'W','rot b',IER)
        BANGOB(nsz,nob,2)=val1
        CALL EGETWR(OUTSTR,K,val1,-180.,180.,'W','rot c',IER)
        BANGOB(nsz,nob,3)=val1
        CALL EGETW(OUTSTR,K,WORD,'W','obs blk name',IFLAG)
        write(BLOCKNAME(nsz,nob),'(a)') WORD(1:lnblnk(WORD))
        LNBLOCKNAME(nsz,nob)=lnblnk(BLOCKNAME(nsz,nob))
        CALL EGETW(OUTSTR,K,WORD,'W','obs mat name',IFLAG)
        write(BLOCKMAT(nsz,nob),'(a)') WORD(1:lnblnk(WORD))
        LNBLOCKMAT(nsz,nob)=lnblnk(BLOCKMAT(nsz,nob))
        BLOCKTYP(nsz,nob)='obs3'
        if(iobs(nsz).eq.0) iobs(nsz)=2  ! mark where obstructions held
        goto 43
      elseif(WORD(1:5).eq.'*obsp')then

C A general polygon obstruction to be associated with the current zone.
C The first line includes (current fixed) integer number of vertices
C followed by number of faces and the name and material.
C The 2nd line has the first 4 coordinates and the 3rd line has the 
C next 4 coordinates. The surface and edge ordering is as in a 
C standard obstruction block when converted into a GB1 common block.
        nbobs(nsz)=nbobs(nsz)+1
        nob=nbobs(nsz)
        CALL EGETWI(OUTSTR,K,ival,8,8,'F','obs nb vertices',IER)
        CALL EGETWI(OUTSTR,K,ival,6,6,'F','pbs nb faces',IER)
        CALL EGETW(OUTSTR,K,WORD,'W','obs blk name',IFLAG)
        write(BLOCKNAME(nsz,nob),'(a)') WORD(1:lnblnk(WORD))
        LNBLOCKNAME(nsz,nob)=lnblnk(BLOCKNAME(nsz,nob))
        CALL EGETW(OUTSTR,K,WORD,'W','obs mat name',IFLAG)
        write(BLOCKMAT(nsz,nob),'(a)') WORD(1:lnblnk(WORD))
        LNBLOCKMAT(nsz,nob)=lnblnk(BLOCKMAT(nsz,nob))
        BLOCKTYP(nsz,nob)='obsp'
        if(iobs(nsz).eq.0) iobs(nsz)=2  ! mark where obstructions held

        CALL LSTRIPC(IUNIT,OUTSTR,99,ND,1,'first 4 coordinates',IER)
        IF(IER.NE.0)goto 1000
        K=0
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','XBP 1',IER)
        XBP(nsz,nob,1)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','YBP 1',IER)
        YBP(nsz,nob,1)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','ZBP 1',IER)
        ZBP(nsz,nob,1)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','XBP 2',IER)
        XBP(nsz,nob,2)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','YBP 2',IER)
        YBP(nsz,nob,2)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','ZBP 2',IER)
        ZBP(nsz,nob,2)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','XBP 3',IER)
        XBP(nsz,nob,3)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','YBP 3',IER)
        YBP(nsz,nob,3)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','ZBP 3',IER)
        ZBP(nsz,nob,3)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','XBP 4',IER)
        XBP(nsz,nob,4)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','YBP 4',IER)
        YBP(nsz,nob,4)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','ZBP 4',IER)
        ZBP(nsz,nob,4)=val1

        CALL LSTRIPC(IUNIT,OUTSTR,99,ND,1,'2nd 4 coordinates',IER)
        IF(IER.NE.0)goto 1000
        K=0
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','XBP 5',IER)
        XBP(nsz,nob,5)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','YBP 5',IER)
        YBP(nsz,nob,5)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','ZBP 5',IER)
        ZBP(nsz,nob,5)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','XBP 6',IER)
        XBP(nsz,nob,6)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','YBP 6',IER)
        YBP(nsz,nob,6)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','ZBP 6',IER)
        ZBP(nsz,nob,6)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','XBP 7',IER)
        XBP(nsz,nob,7)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','YBP 7',IER)
        YBP(nsz,nob,7)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','ZBP 7',IER)
        ZBP(nsz,nob,7)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','XBP 8',IER)
        XBP(nsz,nob,8)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','YBP 8',IER)
        YBP(nsz,nob,8)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','ZBP 8',IER)
        ZBP(nsz,nob,8)=val1
        goto 43

      elseif(WORD(1:5).eq.'*obs ')then

C An obstruction block to be associated with the current zone.
        nbobs(nsz)=nbobs(nsz)+1
        nob=nbobs(nsz)
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','X org',IER)
        XOB(nsz,nob)=val1
        CALL EGETWR(OUTSTR,K,val1,-999.,998.,'W','Y org',IER)
        YOB(nsz,nob)=val1
        CALL EGETWR(OUTSTR,K,val1, -99., 99.,'W','Z org',IER)
        ZOB(nsz,nob)=val1
        CALL EGETWR(OUTSTR,K,val1,   0., 99.,'W','X dis',IER)
        DXOB(nsz,nob)=val1
        CALL EGETWR(OUTSTR,K,val1,   0., 99.,'W','Y dis',IER)
        DYOB(nsz,nob)=val1
        CALL EGETWR(OUTSTR,K,val1,   0., 99.,'W','Z dis',IER)
        DZOB(nsz,nob)=val1
        CALL EGETWR(OUTSTR,K,val1,-359.,359.,'W','rot a',IER)
        BANGOB(nsz,nob,1)=val1
        CALL EGETW(OUTSTR,K,WORD,'W','obs blk name',IFLAG)
        write(BLOCKNAME(nsz,nob),'(a)') WORD(1:lnblnk(WORD))
        LNBLOCKNAME(nsz,nob)=lnblnk(BLOCKNAME(nsz,nob))
        CALL EGETW(OUTSTR,K,WORD,'W','obs mat name',IFLAG)
        write(BLOCKMAT(nsz,nob),'(a)') WORD(1:lnblnk(WORD))
        LNBLOCKMAT(nsz,nob)=lnblnk(BLOCKMAT(nsz,nob))
        BLOCKTYP(nsz,nob)='obs '
        if(iobs(nsz).eq.0) iobs(nsz)=2  ! mark where obstructions held
        goto 43
      elseif(WORD(1:7).eq.'*origin')then
        if(shape(nsz)(1:3).eq.'box')then
          CALL EGETWR(OUTSTR,K,origin(1),0.,0.,'-','box X origin',IER)
          CALL EGETWR(OUTSTR,K,origin(2),0.,0.,'-','box Y origin',IER)
          CALL EGETWR(OUTSTR,K,origin(3),0.,0.,'-','box Z origin',IER)
          szorigin(nsz,1)=origin(1)
          szorigin(nsz,2)=origin(2)
          szorigin(nsz,3)=origin(3)
        elseif(shape(nsz)(1:7).eq.'extrude')then
          CALL EGETWR(OUTSTR,K,origin(1),0.,0.,'-','extrude Z base',IER)
          CALL EGETWR(OUTSTR,K,origin(2),0.,0.,'-','extrude Z top',IER)
          CALL EGETWR(OUTSTR,K,origin(3),0.,0.,'-','extrude ignore',IER)
          szorigin(nsz,1)=origin(1)
          szorigin(nsz,2)=origin(2)
          szorigin(nsz,3)=origin(3)
        elseif(shape(nsz)(1:4).eq.'poly')then
          szorigin(nsz,1)=0.0
          szorigin(nsz,2)=0.0
          szorigin(nsz,3)=0.0
        endif
        goto 43
      elseif(WORD(1:7).eq.'*rotate')then
        CALL EGETWR(OUTSTR,K,rot,0.,0.,'-','rotation angle',IER)
        rotateit(nsz)=rot
        goto 43
      elseif(WORD(1:15).eq.'*sim_parameters')then

C Get the assessment independant simulation parameters.
        CALL EGETWI(OUTSTR,K,simstartup,0,300,'W','startup',IER)
        CALL EGETWI(OUTSTR,K,simzonetimestep,1,60,'W','zone_ts',IER)
        CALL EGETWI(OUTSTR,K,simplanttimestep,0,100,'W','plant_ts',IER)
        CALL EGETWI(OUTSTR,K,simsavelevel,0,6,'W','save_lv',IER)
        havesimparameters=.true.
        goto 43
      elseif(WORD(1:9).eq.'*site_loc')then

C The *site_loc line includes latitute, longitude difference from time
C meridian, time zone (hours before or after GMT). 
        CALL EGETWR(OUTSTR,K,sitelat,-90.,90.,'W','Latitude',IER)
        CALL EGETWR(OUTSTR,K,sitelongdif,-15.,15.,'W',
     &    'Longitude dif from time meridian',IER)
        CALL EGETWR(OUTSTR,K,hoursGTM,-12.,12.,'W','Hours +-GMT',IER)
        havesite=.true.
        goto 43
      elseif(WORD(1:9).eq.'*site_exp')then

C The *site_exp line includes site exposure and if
C siteexposureindex is 8 then read three more numbers
C for the general exposure to sky, ground and other buildings. 
        CALL EGETWI(OUTSTR,K,siteexposureindex,1,8,'W',
     &    'Site exposure',IER)
        if(siteexposureindex.eq.8)then
          CALL EGETWR(OUTSTR,K,skyview,0.,1.,'W','Sky viewfactor',IER)
          CALL EGETWR(OUTSTR,K,groundview,0.,1.,'W',
     &      'Ground viewfactor',IER)
          CALL EGETWR(OUTSTR,K,buildingview,0.,1.,'W',
     &      'Building viewfactor',IER)
        endif
        havesite=.true.
        goto 43
      elseif(WORD(1:5).eq.'*site')then

C The *site line includes latitute, longitude difference from time
C meridian, time zone (hours before or after GMT), site exposure
C and if siteexposureindex is 8 then read three more numbers
C for the general exposure to sky, ground and other buildings.
C This format is included for older META files. 
        CALL EGETWR(OUTSTR,K,sitelat,-90.,90.,'W','Latitude',IER)
        CALL EGETWR(OUTSTR,K,sitelongdif,-15.,15.,'W',
     &    'Longitude dif from time meridian',IER)
        CALL EGETWR(OUTSTR,K,hoursGTM,-12.,12.,'W','Hours +-GMT',IER)
        CALL EGETWI(OUTSTR,K,siteexposureindex,1,8,'W',
     &    'Site exposure',IER)
        if(siteexposureindex.eq.8)then
          CALL EGETWR(OUTSTR,K,skyview,0.,1.,'W','Sky viewfactor',IER)
          CALL EGETWR(OUTSTR,K,groundview,0.,1.,'W',
     &      'Ground viewfactor',IER)
          CALL EGETWR(OUTSTR,K,buildingview,0.,1.,'W',
     &      'Building viewfactor',IER)
        endif
        havesite=.true.
        goto 43
      elseif(WORD(1:5).eq.'*size')then
        if(shape(nsz)(1:3).eq.'box')then
          CALL EGETWR(OUTSTR,K,size(1),0.,0.,'-','box length',IER)
          CALL EGETWR(OUTSTR,K,size(2),0.,0.,'-','box width',IER)
          CALL EGETWR(OUTSTR,K,size(3),0.,0.,'-','box height',IER)
          szsize(nsz,1)=size(1)
          szsize(nsz,2)=size(2)
          szsize(nsz,3)=size(3)
        elseif(shape(nsz)(1:7).eq.'extrude')then
          szsize(nsz,1)=0.0   ! this is ignored for extruded shape
          szsize(nsz,2)=0.0
          szsize(nsz,3)=0.0
        elseif(shape(nsz)(1:4).eq.'poly')then
          szsize(nsz,1)=0.0   ! this is ignored for extruded shape
          szsize(nsz,2)=0.0
          szsize(nsz,3)=0.0
        endif
        goto 43
      elseif(WORD(1:10).eq.'*start_xml')then  
        inumXMLmetrics=0
        CALL LSTRIPC(IUNIT,OUTSTR,99,ND,1,'read metrics',IER)  
        K=0
        CALL EGETW(OUTSTR,K,WORD248,'W','xml data',IER)
        do while(WORD248(1:8).ne.'*end_xml')                           
          inumXMLmetrics=1+inumXMLmetrics        
          XMLMETRICS(inumXMLmetrics)=WORD248
          CALL LSTRIPC(IUNIT,OUTSTR,99,ND,1,'read metrics',IER)  
          K=0
          CALL EGETW(OUTSTR,K,WORD248,'W','xml data',IER)            
        end do
        if(WORD(1:8).eq.'*end_xml'.or.WORD248(1:8).eq.'*end_xml')then
          goto 43
        endif       
        goto 43
      elseif(WORD(1:11).eq.'*start_zone')then

C If this is a new model increment icomp. If the model already has
C zones then set icomp to the next available zone.
        if(ncomp.eq.0)then
          icomp=icomp+1      ! new model, ncomp not yet set
        else
          if(icomp.eq.0)then
            icomp=ncomp+1    ! increment existing zone index
          elseif(icomp.gt.0)then
            icomp=icomp+1    ! increment index for subsequent zones
          endif
        endif
        nsz=nsz+1
        CALL EGETW(OUTSTR,K,WORD,'W','Z name',IFLAG)
        write(name(nsz),'(a)') WORD(1:12)
        goto 43
      elseif(WORD(1:8).eq.'*surface')then

C Expected order is the walls first then the top then the base and
C then any windows and then any doors.
        NS=NS+1

C Surface name, allow for future spaces in name.
        CALL EGETP(OUTSTR,K,WORD,'W','surface name',IER)
        write(szhasname(nsz,NS),'(a)') WORD(1:lnblnk(WORD))

C Surface construction name, allow for spaces.
        CALL EGETP(OUTSTR,K,WORD,'W','surface construction',IER)
        write(szhasconstr(nsz,NS),'(a)') WORD(1:lnblnk(WORD))

C Read three indices representing boundary condition.
        CALL EGETWI(OUTSTR,K,ival1,-1,4,'W','connection type',IER)
        CALL EGETWI(OUTSTR,K,ival2,0,MCOM,'W','connection ic2',IER)
        CALL EGETWI(OUTSTR,K,ival3,0,MS,'W','connection ie2',IER)
        szboundarytype(nsz,NS,1)=ival1
        szboundarytype(nsz,NS,2)=ival2
        szboundarytype(nsz,NS,3)=ival3

C Note: if shape=poly for the each of the declaired walls the
C following line should start with the token *list.
        goto 43
      elseif(WORD(1:6).eq.'*shape')then
        CALL EGETW(OUTSTR,K,WORD,'W','shape type',IFLAG)
        if(WORD(1:3).eq.'box')then
          write(shape(nsz),'(a)') 'box '
          nbwalls(nsz)=6
        elseif(WORD(1:7).eq.'extrude')then
          write(shape(nsz),'(a)') 'extrude'
        elseif(WORD(1:4).eq.'poly')then
          write(shape(nsz),'(a)') 'poly'
        endif
        NS=0     ! reset number of surfaces and clear arrays.
        goto 43

      elseif(WORD(1:6).eq.'*usage')then

C After *usage the next token is 'pattern' (tag indicating that
C an operations file in the training/pattern folder to use to
C get infiltration and casual gains). Note usefile is only
C 32 char buffer because other source code knowns the path
C to the training/pattern folder.
        CALL EGETW(OUTSTR,K,use(1),'W','pattern',IFLAG)
        if(use(1)(1:7).eq.'pattern')then
          szuse(nsz,1) = use(1)
          CALL EGETW(OUTSTR,K,use(2),'W','pattern infil',IFLAG)
          szuse(nsz,2) = use(2)
          CALL EGETW(OUTSTR,K,use(3),'W','pattern casual',IFLAG)
          szuse(nsz,3) = use(3)
          CALL EGETRM(OUTSTR,K,WORD,'W','pattern file',IER)
          write(usefile(nsz),'(a)') WORD(1:lnblnk(WORD))
        else
          call usrmsg('Usage 2nd parameter not recognised',' ','W')
        endif
        goto 43
      elseif(WORD(1:16).eq.'*weather_station')then

C If there is a line with the root name of a climate file look
C for a match in the current climate location and if there is
C one set the name of that file, scan it and find its site.
        CALL EGETRM(OUTSTR,K,weather,'W','weather name',IER)
        goto 43
      elseif(WORD(1:9).eq.'*occupant')then
        goto 43   ! not yet implemented
      elseif(WORD(1:9).eq.'*lighting')then
        goto 43   ! not yet implemented
      elseif(WORD(1:10).eq.'*equipment')then
        goto 43   ! not yet implemented
      elseif(WORD(1:12).eq.'*environment')then
        goto 43   ! not yet implemented
      else
        goto 43
      endif

C Now close silent data file.
      CALL ERPFREE(IUNIT,ios)
      RETURN

C Process the data if the end of the file reached or if the
C *end mark was found.
 1000 CALL ERPFREE(IUNIT,ios)

C Create new model. Note for a single folder option the
C configuration will be in /tmp/box if mpath is /tmp/box. For the
C distributed folder option the onfiguration file will be in the
C folder /tmp/box/cfg if mpath is /tmp/box.
      call silentmodel(actions,root,mpath,folder,weather,ier)

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

C << location for further system level data >>                       

C If there are no zones and no *start_zone found return.
      if(ncomp.eq.0.and.icomp.eq.0)then
        return
      else
        if(ncomp.eq.0)then
          icomp=0      ! reset because it will be incrmented later
        elseif(ncomp.gt.0)then
          icomp=ncomp  ! for existing model set icomp to next avail zone
        endif
        do 63 isz=1,nsz

C Call silent zone and then loop back for more possible definiitons.
C Copy from sz* arrays into parameter arrays prior to silentzone call.
          origin(1)=szorigin(isz,1)
          origin(2)=szorigin(isz,2)
          origin(3)=szorigin(isz,3)
          size(1)=szsize(isz,1)
          size(2)=szsize(isz,2)
          size(3)=szsize(isz,3)
          use(1)= szuse(isz,1)
          use(2)= szuse(isz,2)
          use(3)= szuse(isz,3)
C        usefile(iz) = ' '
          do 48 isu=1,nbcord(isz)   ! for each coordinate in the zone
            coords(isu,1)=szcoords(isz,isu,1)
            coords(isu,2)=szcoords(isz,isu,2)
            coords(isu,3)=szcoords(isz,isu,3)
  48      continue
          do 49 isu=1,MS
            nbedges(isu)=isznbedges(isz,isu)
            do 57 ivu=1,MV
              list(isu,ivu)=iszlist(isz,isu,ivu)
  57        continue
            hasglaze(isu)=szhasglaze(isz,isu)
            hasdoor(isu)=szhasdoor(isz,isu)
            hasconstr(isu)=szhasconstr(isz,isu)
            hasname(isu)=szhasname(isz,isu)
            boundarytype(isu,1)=szboundarytype(isz,isu,1)
            boundarytype(isu,2)=szboundarytype(isz,isu,2)
            boundarytype(isu,3)=szboundarytype(isz,isu,3)
  49      continue
          do 158 imu=1,4
            datamass(imu,1)=sdatamass(isz,imu,1)
            datamass(imu,2)=sdatamass(isz,imu,2)
            datamass(imu,3)=sdatamass(isz,imu,3)
            datamass(imu,4)=sdatamass(isz,imu,4)
            datamass(imu,5)=sdatamass(isz,imu,5)
            datamass(imu,6)=sdatamass(isz,imu,6)
            datamass(imu,7)=sdatamass(isz,imu,7)
            textmass(imu,1)=stextmass(isz,imu,1)
            textmass(imu,2)=stextmass(isz,imu,2)
            textmass(imu,3)=stextmass(isz,imu,3)
 158      continue
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
            IBSN(isilentncf,1)=0
            IBSN(isilentncf,2)=0     
            IBSN(isilentncf,3)=0
            IBSN(isilentncf,4)=0 
            IBAN(isilentncf,1)=0
            IBAN(isilentncf,2)=0   
            IBAN(isilentncf,3)=0
            NBCDT(isilentncf)=1
            IBCDV(isilentncf,1,1)=1
            IBCDV(isilentncf,1,2)=365
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
          
          write(outs,'(2a)') 'Processing ',name(isz)
          call edisp(iuout,outs)

C Debug.
C          write(6,*) 'name',name
C          write(6,*) 'shape',shape
C          write(6,*) 'nbwalls',nbwalls
C          write(6,*) 'nbmass',nbmass
C          write(6,*) 'nbedges',nbedges
C          write(6,*) 'nbcord',nbcord
C          write(6,*) 'list',list

          call silentzone(ICOMP,name(isz),shape(isz),nbwalls(isz),
     &      nbmass(isz),nbedges,nbcord(isz),list,origin,size,
     &      coords,rotateit(isz),nbglz(isz),hasglaze,
     &      nbdoor(isz),hasdoor,hasname,hasconstr,
     &      boundarytype,datamass,textmass,use,usefile(isz),IER)
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

 1001 write(outs,'(3a)') 'Conversion error in...',OUTSTR(1:50),'...'
      if(dll)then
        dllsubr='SILENTREAD'
        dllmesg=outs
        ier=2
        CALL ERPFREE(IUNIT,ios)
        return
      else
        call edisp(iuout,outs)
        IER=1
        CALL ERPFREE(IUNIT,ios)
        RETURN
      endif

      END

 
C ******************** METAWRITE 
C METAWRITE creates an esp-r meta file. Data input is from
C the current zones common blocks. If act is .... Otherwise create
C the file with minimum interaction.
 
       SUBROUTINE METAWRITE(ifilg,LFILE,act,IER)
       integer MSZ   ! number of meta zones array sizes
       PARAMETER (MSZ=10) 
#include "building.h"
#include "model.h"
#include "site.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "espriou.h"
C espriou.h provides currentfile.
#include "seasons.h"
C seasons.h provides typper and typsea
#include "ipvdata.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)

      COMMON/PREC8/SLAT,SLON

      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)

C Obstruction blocks via geometry.h.
C common zndata (VCOORD,NZNVER,NZJVN) via geometry.h  

      common/rpath/path
      character*72 path,odir    ! where path and odir are folders

      common/dllerr/dllsubr,dllmesg
      character dllsubr*12,dllmesg*124  ! messages if errors in dll

C Simulation parameter sets.
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh

C IPV description via ipvdata.h.

C Control documentation common
      common/ctlnm/znctldoc,plctldoc,flctldoc,elctldoc,glctldoc,
     &             opticdoc
      character znctldoc*248,plctldoc*248,flctldoc*248,elctldoc*248
      character glctldoc*248,opticdoc*248     
  
      common/cctlnm/ctldoc,lctlf
      character LCTLF*72,CTLDOC*248  
      
      common/cctl/icascf(mcom)
      integer icascf

      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/C24/IZSTOCN(MCOM,MS)

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
     
      integer inumXMLmetrics !counts for the number of xml metrics defined
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,cfgroot*24,LCFGF*72
      integer IFCFG
               
      CHARACTER OUTSTR*248,outs*124,outsd*124
      character dstmp*24

      character act*1      ! action requested.
      character ipvact*1   ! action for clearipvdat
C      character simact*6  ! action for creating assessments.
      character subpath*72 ! not yet used

C At the head of the file is a menu string and a description
C modeltitle (char 72) and modeldocblock (char 248) which identifies
C the contents of the file via model.h.
      character weather*32     ! file name for the location (no path)
      character*(*) lfile      ! name of the file
      character fs*1           ! file separator

C For passing to silentmodel.
      character actions*8,root*24,mpath*72,folder*12

      logical dll,unixok,XST

C Paremeters passed to:silentzone
      character shape*8   ! box or extrude or poly
      dimension shape(MSZ)
      character szuse*8,usefile*32  ! usage pattern directives
      dimension szuse(MSZ,3),usefile(MSZ)
      integer nbwalls(MSZ)     ! nb of vertical walls
      real rotateit(MSZ)        ! angle of rotation

C Stings associated with internal mass. 1=surface name, 2=construction, 3=optics
      character stextmass*32
      dimension stextmass(MSZ,4,3)

C Site related local variables.
      logical havesnowdays     ! true if days with snow defined
      character hourlysnowfile*72  ! same as SNFNAM
      logical havehourlysnowfile   ! true if set
      real hoursGTM   ! hours before or after GTM 
      real groundtemps(12,MGRDP)
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
      character tab*1
      integer lnzp,lnnp,lncp,lndcp,lndbp ! lengths of paths to model folders

C  The following data statements carry the proportions of
C  surrounding buildings, sky vault and ground assumed
C  to be seen by a vertical surface for a particular site
C  defined by the Index of Exposure.
      DIMENSION PROPB(8),PROPS(8),PROPG(8)
      DATA PROPB/0.28,0.18,0.1,0.34,0.52,0.0,1.0,0.0/
      DATA PROPS/0.36,0.41,0.45,0.33,0.15,0.5,0.0,0.0/
      DATA PROPG/0.36,0.41,0.45,0.33,0.33,0.5,0.0,0.0/
        
      IER=0

C Determine operating system
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      call dstamp(dstmp)

C Get the current folder and display options to the user.
C first list any files with .cfg in name.
      odir=' '
      call usrdir(odir)

C If subpath is less than 72 char with odir + path use that
C otherwise just use path.

C Debug...
C      write(6,*) 'usrdir is ',odir
C      write(6,*) 'path is ',path

      subpath=' '
      lnodir=lnblnk(odir)
      lnpath=lnblnk(path)
      if(lnodir+lnpath+1.le.72)then
        write(subpath,'(3a)') odir(1:lnodir),fs,
     &  path(1:lnpath)
      else
        write(subpath,'(a)') path(1:lnpath)
      endif

C Debug.
C      write(outs,'(2a)') 'configuration files in ',
C     &  subpath(1:lnblnk(subpath))
C      call edisp(iuout,outs)

C Instanciate the sz array structures. The logic is to scan all of the
C zone details and then write into the file.
      write(actions,'(a)') 'new'
      write(root,'(a)') cfgroot

C If all of the associated folder paths are ./ then we have a
C single folder model, otherwise use distributed folders.
      lnzp=lnblnk(zonepth)
      lnnp=lnblnk(netpth)
      lncp=lnblnk(ctlpth)
      lndcp=lnblnk(docpth)
      lndbp=lnblnk(dbspth)
      if((lnzp+lnnp+lncp+lndcp+lndbp).lt.12)then
        write(folder,'(a)') 'single'  ! multiple folders
      else
        write(folder,'(a)') 'distributed'  ! multiple folders
      endif
      write(mpath,'(a)') cfgroot  ! model folder will take root name

C Currently set the block of text to the model title.
      write(modeldocblock,'(a)')  modeltitle(1:lnblnk(modeltitle))

C Site data.
C << for weather we need to use logic sort of like finddbpath >>
      weather='XXX'
      hoursGTM=0.0

C The hourlysnowfile is SNFNAM.
      write(hourlysnowfile,'(a)') SNFNAM

C Depending on the Index of Exposure the proportions of the
C surroundings are assigned new variable names.
      if(siteexposureindex.eq.8)then
        continue
      else
        buildingview=PROPB(siteexposureindex)
        skyview=PROPS(siteexposureindex)
        groundview=PROPG(siteexposureindex)
      endif

C Number of days with snow on the ground (if ground refl model 2)
      if(groundreflmodel.EQ.2)then
        havesnowdays=.true.
      endif

C Snow depth file (if ground reflectivity model 3)
      if(groundreflmodel.EQ.3)then
        write(hourlysnowfile,'(a)') SNFNAM(1:lnblnk(SNFNAM))
        havehourlysnowfile=.true.
      endif
      
      if(NGRDP.gt.0)then
        havegroundtempsets=.true.
        groundtempsets=NGRDP
        do 128 IGRDP=1,NGRDP
          DO 129 J=1,12
            groundtemps(J,IGRDP)=UGRDTP(J,IGRDP)
  129     CONTINUE
  128   continue
      endif

C Simulation parameter sets.
      if(nsset.gt.0)then
        havesimparameters=.true.
        simstartup=isstup
        simzonetimestep=isbnstep
        simplanttimestep=ispnstep
        simsavelevel=issave
        isilentncf=0   ! still to do
      endif

C For each of the zones instanciate values.
C << add in zone description >>
      do 42 iz=1,ncomp
        shape(iz) = 'poly'    ! zone shape is poly
        szuse(iz,1) = ' '     ! szuse (skip for now)
        szuse(iz,2) = ' '
        szuse(iz,3) = ' '
        usefile(iz) = ' '       ! usefile (skip for now)
        nbwalls(iz) = NZSUR(IZ) ! surfaces in the zone
        rotateit(iz) = 0.0      ! rotation is always zero

C Control data - ignore for now.
        ht_Setpoint(iz)= 0.0
        cl_Setpoint(iz)= 0.0
        i_ctl_link(iz)=0

C Obstructions - if the original model has obstructions then
C we will use them.

  42  continue

C Check if running in dll mode.
      call isadll(dll)

C Initialise geometry data file. and set currentfile.
      CALL EFOPSEQ(ifilg,LFILE,3,IER)
      IF(IER.LT.0)THEN
        write(outs,'(3a)') 'Meta file ',LFILE(1:lnblnk(LFILE)),
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

C Write header lines from file, the 1.1 version looks like: 
C *silent_input
      tab=','    ! make the separator a comma.
      write(ifilg,'(a)',IOSTAT=ios,ERR=13) '*silent_input'
      write(ifilg,'(3a)',IOSTAT=ios,ERR=13)'*title',tab,
     &  modeltitle(1:lnblnk(modeltitle))
      write(ifilg,'(3a)',IOSTAT=ios,ERR=13)'*doc',tab,
     &  modeldocblock(1:lnblnk(modeldocblock))
      write(ifilg,'(4a)',IOSTAT=IOS,ERR=13)'*date',tab,dstmp,
     &  '  # latest file modification '
      write(ifilg,'(9a)',IOSTAT=IOS,ERR=13)'*action',tab,
     &  actions(1:lnblnk(actions)),tab,
     &  root(1:lnblnk(root)),tab,
     &  folder(1:lnblnk(folder)),tab,mpath(1:lnblnk(mpath))
      write(ifilg,'(4a)',IOSTAT=IOS,ERR=13)'*weather_station',tab,
     &  weather(1:lnblnk(weather)),'  # climate file'

C Depending on site exposure write two lines.
      if(siteexposureindex.eq.8)then
        write(ifilg,'(2a,F6.3,a,F6.3,a,F5.2,a)',IOSTAT=IOS,ERR=13)
     &    '*site_loc',tab,sitelat,tab,sitelongdif,tab,
     &    hoursGTM,' # latitude deg long-diff deg time-zone GMT'
        write(ifilg,'(2a,i2,a,F5.2,a,F5.2,a,F5.2,a)',IOSTAT=IOS,ERR=13)
     &    '*site_exp',tab,siteexposureindex,tab,skyview,tab,
     &    groundview,tab,buildingview,
     &    ' # exposure-index (user) sky ground building views'
      else
        write(ifilg,'(2a,F6.3,a,F6.3,a,F5.2,a)',IOSTAT=IOS,ERR=13)
     &    '*site_loc',tab,sitelat,tab,sitelongdif,tab,hoursGTM,
     &    ' # latitude deg long-diff deg time-zone GMT'
        write(ifilg,'(2a,i2,a,F5.2,a,F5.2,a,F5.2,a)',IOSTAT=IOS,ERR=13)
     &    '*site_exp',tab,siteexposureindex,tab,skyview,tab,
     &    groundview,tab,buildingview,
     &    ' # exposure-index (std) sky ground building views'
      endif

C Write the static ground reflction value from the model.
      write(ifilg,'(2a,F6.3,a)',IOSTAT=IOS,ERR=13)
     &  '*ground_refl_annual',tab,groundrefl,
     &  ' # ground reflection (avg for year)'

C Ground reflectivity model, no-snow monthly albedos, snow albedo
C (if ground reflectivity model 2 or 3)
      if(groundreflmodel.gt.1)then
        WRITE(ifilg,803,IOSTAT=IOS,ERR=14)
     &    '*ground_refl_monthly',tab,groundreflmodel,
     &    tab,(groundreflmonth(I),I=1,12),snowgroundrefl
  803   FORMAT(2A,I1,A,13(1X,F5.3))
      endif

C Do something about monthly snow data etc.
      if(havesnowdays)then
        WRITE(ifilg,804,IOSTAT=IOS,ERR=14)
     &    '*monthly_snow_days',tab,
     &    (dayswithsnow(I),I=1,12)
  804   FORMAT(2A,13(1X,I2))
      endif
      if(havehourlysnowfile)then
        write(ifilg,'(4a)',IOSTAT=IOS,ERR=13)'*hourly_snow_depth',tab,
     &    hourlysnowfile(1:lnblnk(hourlysnowfile)),' # snow depth file'
      endif

C Ground temperatures (monthly profiles). Header followed by one
C line of 12 values for each included profile.
      if(havegroundtempsets)then
        write(ifilg,'(2a,I3,a)',IOSTAT=IOS,ERR=13)
     &    '*monthly_profile',tab,groundtempsets,
     &    '  # nb of ground temperature profiles'
        DO 28 IGRDP=1,groundtempsets
          WRITE(ifilg,933,IOSTAT=IOS,ERR=14)
     &      (groundtemps(J,IGRDP),J=1,12)
  933     FORMAT(12F6.2)
  28    CONTINUE
      endif

C Simulation parameters.
      if(havesimparameters)then
        write(ifilg,'(2a,I2,a,I2,a,I2,a,I1,a)',IOSTAT=IOS,ERR=13)
     &    '*sim_parameters',tab,simstartup,tab,simzonetimestep,tab,
     &    simplanttimestep,tab,simsavelevel,'  # simulation parameters'

C << *assessments still to do >>
      endif

C Write out data for each zone in the model. The nsz loop
C is per zone, the nsc loop is for each coordinate.
      if(ncomp.gt.0)then
        do 142 nsz=1,ncomp
          write(ifilg,'(4a)',IOSTAT=IOS,ERR=13)
     &      '*start_zone',tab,zname(nsz),'  # zone name'
          write(ifilg,'(4a)',IOSTAT=IOS,ERR=13)
     &      '*shape',tab,shape(nsz),'  # polygon enclosure'

          do 143 nsc=1,NZTV(nsz)
            write(outs,'(a,3F8.4)',IOSTAT=IOS,ERR=13)
     &        '*cord ',VCOORD(nsz,nsc,1),VCOORD(nsz,nsc,2),
     &        VCOORD(nsz,nsc,3)
            call SDELIM(outs,outsd,'C',IW)
            write(ifilg,'(2a,i3)',IOSTAT=IOS,ERR=13) 
     &        outsd(1:lnblnk(outsd)),'  # X Y Z for ',nsc
 143      continue

C The origin is ignored in poly type enclosure so do not bother with *origin.

C The size is ignored in poly type enclosure so do not bother with *size.

C Rotation is always zero (in this version of poly).
          write(ifilg,'(2a,F8.4,a)',IOSTAT=IOS,ERR=13)
     &      '*rotate',tab,rotateit(nsz),
     &      '  # rotation around zone origin'

C Number of surfaces.

C << consider altering the tag to *nbsurfs >>

          write(ifilg,'(2a,i3,a)',IOSTAT=IOS,ERR=13)
     &      '*nbwalls',tab,nbwalls(nsz),'  # number of surfaces'

C << consider adding more attributes and putting each attribute
C << on a separate line with a tag.
          do 144 isu=1,NZSUR(nsz)
            icc=IZSTOCN(nsz,isu)    ! get the connection
            lnsn=lnblnk(SSNAME(icc))
            lnsc=lnblnk(SSMLCN(icc))
            write(outs,'(5a,3I4)',IOSTAT=IOS,ERR=13)
     &        '*surface ',SSNAME(icc)(1:lnsn),' ',
     &        SSMLCN(icc)(1:lnsc),' ',ICT(icc),IC2(icc),IE2(icc)
            call SDELIM(outs,outsd,'C',IW)
            write(ifilg,'(2a)',IOSTAT=IOS,ERR=13) 
     &        outsd(1:lnblnk(outsd)),'  # surface attributes '

            write(outs,'(a,25I4)',IOSTAT=ios,ERR=13) '*list ',
     &        NZNVER(icc),(NZJVN(icc,J),J=1,NZNVER(icc))
            call SDELIM(outs,outsd,'C',IW)
            write(ifilg,'(2a,i2)',IOSTAT=IOS,ERR=13) 
     &        outsd(1:lnblnk(outsd)),'  # ',isu
 144      continue

C Shading calculation directives in the form of:
C *shad_calc,all_applic,20 # list of surfs
          if(nsurfcalc(nsz).eq.0)then
            write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '# shading directives'
            write(ifilg,'(a)',IOSTAT=ios,ERR=13)
     &        '*shad_calc,none  # no temporal shading requested'
          elseif(nsurfcalc(nsz).gt.0)then
            write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '# shading directives'
            if(iaplic(nsz,1).eq.1)then
              write(ifilg,'(a,i3,a)',IOSTAT=ios,ERR=13)
     &        '*shad_calc,all_applicable ',nsurfcalc(nsz),
     &        ' # list of surfs'
              write(ifilg,'(42i3)',IOSTAT=ios,ERR=14)
     &        (lstsfcalc(nsz,J),J=1,nsurfcalc(nsz))
            elseif(iaplic(nsz,1).eq.0)then
              write(ifilg,'(a,i3,a)',IOSTAT=ios,ERR=13)
     &        '*shad_directive,list',nsurfcalc(nsz),
     &        ' # list of surfs'
              write(ifilg,'(42i3)',IOSTAT=ios,ERR=14)
     &        (lstsfcalc(nsz,J),J=1,nsurfcalc(nsz))
            endif
          endif

C Insolation calculation directives.
C *insol_calc,all_applic,2 # insolation sources
          write(ifilg,'(a)',IOSTAT=IOS,ERR=13) 
     &      '# insolation directives'
          if(nsurfinso(nsz).eq.0)then
            write(ifilg,'(a)',IOSTAT=ios,ERR=13)
     &        '*insol_calc,none  # no insolation requested'
          elseif(nsurfinso(nsz).gt.0)then
            if(iaplic(nsz,2).eq.1)then
              write(ifilg,'(a,i3,a)',IOSTAT=ios,ERR=13)
     &        '*insol_calc,all_applicable ',nsurfinso(nsz),
     &        ' # insolation sources'
              write(ifilg,'(42i3)',IOSTAT=ios,ERR=14)
     &        (isurfinso(nsz,J),J=1,nsurfinso(nsz))
            elseif(iaplic(nsz,2).eq.0)then
              write(ifilg,'(a,i3,a)',IOSTAT=ios,ERR=13)
     &        '*insol_calc,list',nsurfinso(nsz),
     &        ' # insolation sources'
              write(ifilg,'(42i3)',IOSTAT=ios,ERR=14)
     &        (isurfinso(nsz,J),J=1,nsurfinso(nsz))
            endif
          endif

C Obstruction information lines.  Write out slightly different
C format if type 'obs' and 'obs3'
          if(nbobs(nsz).gt.0)then
            write(ifilg,'(a)',IOSTAT=IOS,ERR=13)
     &        '#  *obs = solar obstructions'
            write(ifilg,'(2a,i2,i3,a)',IOSTAT=IOS,ERR=13) 
     &        '*solar_grid',tab,NOX(nsz),NOZ(nsz),
     &        ' # solar gridding density'
            do 55 ib=1,nbobs(nsz)
              if(BLOCKTYP(nsz,ib)(1:4).eq.'obs ')then
                write(outs,'(2a,7F9.3,1X,A,1X,A)',IOSTAT=ios,ERR=13) 
     &            '*obs',tab,XOB(nsz,ib),YOB(nsz,ib),ZOB(nsz,ib),
     &            DXOB(nsz,ib),DYOB(nsz,ib),DZOB(nsz,ib),
     &            BANGOB(nsz,ib,1),BLOCKNAME(nsz,ib),
     &            BLOCKMAT(nsz,ib)
                call SDELIM(outs,outsd,'C',IW)
                write(ifilg,'(2a,i2)',IOSTAT=IOS,ERR=13) 
     &            outsd(1:lnblnk(outsd)),'  # block ',ib
              elseif(BLOCKTYP(nsz,ib)(1:4).eq.'obs3')then
                write(outs,'(2a,9F9.3,1X,A,1X,A)',IOSTAT=ios,ERR=13) 
     &            '*obs3',tab,XOB(nsz,ib),YOB(nsz,ib),ZOB(nsz,ib),
     &            DXOB(nsz,ib),DYOB(nsz,ib),DZOB(nsz,ib),
     &            BANGOB(nsz,ib,1),BANGOB(nsz,ib,2),BANGOB(nsz,ib,3),
     &            BLOCKNAME(nsz,ib),BLOCKMAT(nsz,ib)
                call SDELIM(outs,outsd,'C',IW)
                write(ifilg,'(2a,i2)',IOSTAT=IOS,ERR=13) 
     &            outsd(1:lnblnk(outsd)),'  # block ',ib
              elseif(BLOCKTYP(nsz,ib)(1:4).eq.'obsp')then
                WRITE(outs,'(2a,A,1X,A)',IOSTAT=ios,ERR=13) 
     &          '*obsp',' 8 6 ',BLOCKNAME(nsz,ib),BLOCKMAT(nsz,ib)
                call SDELIM(outs,outsd,'C',IW)
                write(ifilg,'(2a,i2,a)',IOSTAT=IOS,ERR=13) 
     &            outsd(1:lnblnk(outsd)),'  # block ',ib,
     &            ' coords follow:'

                WRITE(outs,'(12F9.3)',IOSTAT=ios,ERR=13) 
     &            XBP(nsz,ib,1),YBP(nsz,ib,1),ZBP(nsz,ib,1),
     &            XBP(nsz,ib,2),YBP(nsz,ib,2),ZBP(nsz,ib,2),
     &            XBP(nsz,ib,3),YBP(nsz,ib,3),ZBP(nsz,ib,3),
     &            XBP(nsz,ib,4),YBP(nsz,ib,4),ZBP(nsz,ib,4)
                call SDELIM(outs,outsd,'C',IW)
                write(ifilg,'(A)',IOSTAT=IOS,ERR=13) 
     &            outsd(1:lnblnk(outsd))

                WRITE(outs,'(12F9.3)',IOSTAT=ios,ERR=13) 
     &            XBP(nsz,ib,5),YBP(nsz,ib,5),ZBP(nsz,ib,5),
     &            XBP(nsz,ib,6),YBP(nsz,ib,6),ZBP(nsz,ib,6),
     &            XBP(nsz,ib,7),YBP(nsz,ib,7),ZBP(nsz,ib,7),
     &            XBP(nsz,ib,8),YBP(nsz,ib,8),ZBP(nsz,ib,8)
                call SDELIM(outs,outsd,'C',IW)
                write(ifilg,'(A)',IOSTAT=IOS,ERR=13) 
     &            outsd(1:lnblnk(outsd))
              endif
  55        continue
          endif
	  
C << usage information goes here >>

C Idea control information is work in progress.
          write(ifilg,'(2a,F6.2,a)',IOSTAT=ios,ERR=13)
     &      '*heating',tab,ht_Setpoint(nsz),
     &      '  # ideal heating stepoint'
          write(ifilg,'(2a,F6.2,a)',IOSTAT=ios,ERR=13)
     &      '*cooling',tab,cl_Setpoint(nsz),
     &      '  # ideal heating stepoint'
          write(ifilg,'(2a,i3,a)',IOSTAT=ios,ERR=13)
     &      '*ideal_control',tab,i_ctl_link(nsz),
     &      '  # ideal control linkage'

C Mark the end of zone information.
          write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '*end_zone'
 142    continue
      endif

C Mark the end of model information.
      write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '*end'

C Now close meta file.
  99  CALL ERPFREE(ifilg,ios)
      RETURN


C Data structures which have not yet been sorted.
C      elseif(WORD(1:6).eq.'*usage')then

C After *usage the next token is 'pattern' (tag indicating that
C an operations file in the training/pattern folder to use to
C get infiltration and casual gains). Note usefile is only
C 32 char buffer because other source code knowns the path
C to the training/pattern folder.
C        CALL EGETW(OUTSTR,K,use(1),'W','pattern',IFLAG)
C        if(use(1)(1:7).eq.'pattern')then
C          szuse(nsz,1) = use(1)
C          CALL EGETW(OUTSTR,K,use(2),'W','pattern infil',IFLAG)
C          szuse(nsz,2) = use(2)
C          CALL EGETW(OUTSTR,K,use(3),'W','pattern casual',IFLAG)
C          szuse(nsz,3) = use(3)
C          CALL EGETRM(OUTSTR,K,WORD,'W','pattern file',IER)
C          write(usefile(nsz),'(a)') WORD(1:lnblnk(WORD))
C        else
C          call usrmsg('Usage 2nd parameter not recognised',' ','W')
C        endif
C      elseif(WORD(1:12).eq.'*assessments')then

C Get tags related to assessments: after the tag *assessments the
C following choices are possible:
C   `annual` (no further tokens)
C   `single` followed typically by `user` followed by pairs of julian dates
C   `winter` followed by `typical_week` or `season` or 'fortnight'
C   `spring` followed by `typical_week` or `season` or 'fortnight'
C   `summer` followed by `typical_week` or `season` or 'fortnight'
C   `autumn` followed by `typical_week` or `season` or 'fortnight'
C   `three_season` followed by `typical_week` `forthnight` `user` or `season` 
C   `five_season` followed by `typical_week` `forthnight` `user` or `season` 
C        CALL EGETW(OUTSTR,K,WORD,'W','*assessment tags',IER)
C        if(WORD(1:7).eq.'Default'.or.WORD(1:7).eq.'default')then
C          nipvassmt=1
C          simsavelevel=-1   ! signal to ipv2simpar to use defaults
C          simact='i1d   '
C        elseif(WORD(1:6).eq.'Annual'.or.WORD(1:6).eq.'annual')then
C          nipvassmt=1
C          simact='ias   '
C        elseif(WORD(1:6).eq.'Single'.or.WORD(1:6).eq.'single')then
C          CALL EGETW(OUTSTR,K,WORD,'W','Single tags',IER)
C          if(WORD(1:4).eq.'User'.or.
C     &       WORD(1:4).eq.'user')then
C            CALL EGETWI(OUTSTR,K,iday1,1,365,'W','start julian',IER)
C            CALL EGETWI(OUTSTR,K,iday2,1,365,'W','end julian',IER)
C          endif
C          nipvassmt=1
C          simact='i1d   '
C        elseif(WORD(1:6).eq.'Winter'.or.WORD(1:6).eq.'winter')then
C          CALL EGETW(OUTSTR,K,WORD,'W','Winter tags',IER)
C          if(WORD(1:12).eq.'Typical_week'.or.
C     &       WORD(1:12).eq.'typical_week')then
C            simact='icwint'
C          elseif(WORD(1:9).eq.'Fortnight'.or.
C     &           WORD(1:9).eq.'fortnight')then
C            simact='icwinf'
C          elseif(WORD(1:9).eq.'Season'.or.
C     &           WORD(1:9).eq.'season')then
C            simact='icwins'
C          endif
C          nipvassmt=1
C        elseif(WORD(1:6).eq.'Spring'.or.WORD(1:6).eq.'spring')then
C          CALL EGETW(OUTSTR,K,WORD,'W','Spring tags',IER)
C          if(WORD(1:12).eq.'Typical_week'.or.
C     &       WORD(1:12).eq.'typical_week')then
C            simact='icsprt'
C          elseif(WORD(1:9).eq.'Fortnight'.or.
C     &           WORD(1:9).eq.'fortnight')then
C            simact='icsprf'
C          elseif(WORD(1:9).eq.'Season'.or.
C     &           WORD(1:9).eq.'season')then
C            simact='icsprs'
C          endif
C          nipvassmt=1
C        elseif(WORD(1:6).eq.'Summer'.or.WORD(1:6).eq.'summer')then
C          CALL EGETW(OUTSTR,K,WORD,'W','Summer tags',IER)
C          if(WORD(1:12).eq.'Typical_week'.or.
C     &       WORD(1:12).eq.'typical_week')then
C            simact='icsumt'
C          elseif(WORD(1:9).eq.'Fortnight'.or.
C     &           WORD(1:9).eq.'fortnight')then
C            simact='icsumf'
C          elseif(WORD(1:9).eq.'Season'.or.
C     &           WORD(1:9).eq.'season')then
C            simact='icsums'
C          endif
C          nipvassmt=1
C        elseif(WORD(1:6).eq.'Autumn'.or.WORD(1:6).eq.'autumn')then
C          CALL EGETW(OUTSTR,K,WORD,'W','Autumn tags',IER)
C          if(WORD(1:12).eq.'Typical_week'.or.
C     &       WORD(1:12).eq.'typical_week')then
C            simact='icautt'
C          elseif(WORD(1:9).eq.'Fortnight'.or.
C     &           WORD(1:9).eq.'fortnight')then
C            simact='icautf'
C          elseif(WORD(1:9).eq.'Season'.or.
C     &           WORD(1:9).eq.'season')then
C            simact='icauts'
C          endif
C          nipvassmt=1
C        elseif(WORD(1:12).eq.'Three_season'.or.
C     &         WORD(1:12).eq.'three_season')then
C          CALL EGETW(OUTSTR,K,WORD,'W','three_season tags',IER)
C          if(WORD(1:12).eq.'Typical_week'.or.
C     &       WORD(1:12).eq.'typical_week')then
C            simact='i3t   '
C          elseif(WORD(1:4).eq.'User'.or.WORD(1:4).eq.'user')then
C            CALL EGETWI(OUTSTR,K,iday1,1,365,'W','start julian',IER)
C            CALL EGETWI(OUTSTR,K,iday2,1,365,'W','end julian',IER)
C            CALL EGETWI(OUTSTR,K,iday3,1,365,'W','start julian',IER)
C            CALL EGETWI(OUTSTR,K,iday4,1,365,'W','end julian',IER)
C            CALL EGETWI(OUTSTR,K,iday5,1,365,'W','start julian',IER)
C            CALL EGETWI(OUTSTR,K,iday6,1,365,'W','end julian',IER)
C            simact='i3u   '
C          elseif(WORD(1:9).eq.'season'.or.
C     &           WORD(1:9).eq.'season')then
C            simact='i3s   '
C          endif
C          nipvassmt=3
C        elseif(WORD(1:11).eq.'Five_season'.or.
C     &         WORD(1:11).eq.'five_season')then
C          CALL EGETW(OUTSTR,K,WORD,'W','five_season tags',IER)
C          if(WORD(1:12).eq.'Typical_week'.or.
C     &       WORD(1:12).eq.'typical_week')then
C            simact='i5t   '
C          elseif(WORD(1:4).eq.'User'.or.WORD(1:4).eq.'user')then
C            CALL EGETWI(OUTSTR,K,iday1,1,365,'W','start julian',IER)
C            CALL EGETWI(OUTSTR,K,iday2,1,365,'W','end julian',IER)
C            CALL EGETWI(OUTSTR,K,iday3,1,365,'W','start julian',IER)
C            CALL EGETWI(OUTSTR,K,iday4,1,365,'W','end julian',IER)
C            CALL EGETWI(OUTSTR,K,iday5,1,365,'W','start julian',IER)
C            CALL EGETWI(OUTSTR,K,iday6,1,365,'W','end julian',IER)
C            CALL EGETWI(OUTSTR,K,iday7,1,365,'W','start julian',IER)
C            CALL EGETWI(OUTSTR,K,iday8,1,365,'W','end julian',IER)
C            CALL EGETWI(OUTSTR,K,iday9,1,365,'W','start julian',IER)
C            CALL EGETWI(OUTSTR,K,iday10,1,365,'W','end julian',IER)
C            simact='i5u   '
C          elseif(WORD(1:9).eq.'season'.or.
C     &           WORD(1:9).eq.'season')then
C            simact='i5s   '
C          endif
C          nipvassmt=5
C        else
C          nipvassmt=0
C          simact='i1d   '
C        endif
C        write(6,*) 'the value of simact is ',simact
C        goto 43
C      elseif(WORD(1:4).eq.'*ipv')then
C        goto 43
C      elseif(WORD(1:10).eq.'*start_xml')then  
C        inumXMLmetrics=0
C        CALL LSTRIPC(IUNIT,OUTSTR,99,ND,1,'read metrics',IER)  
C        K=0
C        CALL EGETW(OUTSTR,K,WORD248,'W','xml data',IER)
C        do while(WORD248(1:8).ne.'*end_xml')                           
C          inumXMLmetrics=1+inumXMLmetrics        
C          XMLMETRICS(inumXMLmetrics)=WORD248
C          CALL LSTRIPC(IUNIT,OUTSTR,99,ND,1,'read metrics',IER)  
C          K=0
C          CALL EGETW(OUTSTR,K,WORD248,'W','xml data',IER)            
C        end do
C        if(WORD(1:8).eq.'*end_xml'.or.WORD248(1:8).eq.'*end_xml')then
C          goto 43
C        endif       
C        goto 43
C      endif

C Error messages.
   13 if(IOS.eq.2)then
        CALL USRMSG('No permission to write ',LFILE,'W')
      else
        CALL USRMSG('File write error in ',LFILE,'W')
      endif
      IER=1
      GOTO 99
   14 if(IOS.eq.2)then
        CALL USRMSG('No prmission to write array in ',LFILE,'W')
      else
        CALL USRMSG('Long arrary write error in ',LFILE,'W')
      endif
      IER=1
      GOTO 99

      END

