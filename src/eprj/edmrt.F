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

C ************* EDMRT 
C Edit MRT attributes via a paging menu.
C ITRU = unit number for user output, IER=0 OK, IER=1 problem.
C If silent is true then called from either a global facility
C or from version facility. A file with area weighted view factors
C will be created if act = 'a' and if act = 'v' then ray tracing
C calculations will be commissioned. If silent the viewfactor file name
C will have been defined prior to calling edmrt.
      SUBROUTINE EDMRT(ITRC,ITRU,IUF,ICOMP,silent,act,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "epara.h"
#include "prj3dv.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/user/browse
      integer childterminal  ! picks up mmod from starting of prj
      common/childt/childterminal

      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

      COMMON/MRTF/FFL(MCUB,6,MSM),VEWF(MS,MS)
      common/appw/iappw,iappx,iappy
      integer iappw,iappx,iappy
      integer iappwpc ! application %

      DIMENSION VERT(35)
      DIMENSION SNA(MS),PAZI(MS),PELV(MS)
      LOGICAL concat,cfgmod,browse,clkok,XST
      logical silent,unixok
      CHARACTER VERT*46,LTMP*72
      character VFILE*72,head*32,outs*124,KEY*1,ETEXT*82
      character doit*248,tmode*8,tfile*72,ZN*12
      character act*1
      character longtfile*144,longtfiledos*144
      integer icni,icnj  ! connection numbers for pair of surfaces.
      integer MVERT,IVERT ! max items and current menu item
      logical newgeo

C Passed parameters for cfgtogg
#ifdef OSI
      integer icfg_type  ! model cfg type
      integer icfgz      ! if non-zero then there are zones
      integer icfgn      ! if non-zero then there are networks
      integer icfgc      ! if non-zero then cfg file known
      integer icfgdfn    ! if non-zero then cdf domain exists
      integer iicfgz     ! there are zone related images
      integer iicfgn     ! there are network related images
      integer iicfgc     ! there are control related images
      integer iicfgdfn   ! there are cfd related images
#else
      integer*8 icfg_type,icfgz,icfgn,icfgc,icfgdfn,iicfgz
      integer*8 iicfgn,iicfgc,iicfgdfn
#endif

#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
#else
      integer*8 iside,isize,ifont   ! passed to viewtext
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDMRT'  ! set for subroutine

      cfgmod=.false.

C Check if Unix-based or DOS based.
      call isunix(unixok)

C If user is browsing model then give warning and exit.
      if(browse)then
        call usrmsg(
     &  'You do not have permission to increase the resolution of this',
     &  'model. To calculate viewfactors you must own the model.','W')
        return
      endif

C Initialize the default viewfactors and MRT sensor file name and the 
C matching geometry file.

C << find a place in the initial scan of model to comment on the path
C << to the zone files if blank or ./

C << Take into account where prj was started and the
C << path to the model cfg folder. Also check if zonepth is non-blank
C << follows the standard folder naming scheme.
C << Let VFILE include the extended path information and LVIEW the
C << local reference information. We want to support creating the
C << boilerplate file if working remotely.
 
      if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
        WRITE(VFILE,'(2a)')zname(ICOMP)(1:lnzname(ICOMP)),'.vwf'
      else
        WRITE(VFILE,'(4a)') zonepth(1:lnblnk(zonepth)),'/',
     &        zname(ICOMP)(1:lnzname(ICOMP)),'.vwf'
      endif
 
      write(head,'(2A)')'  Viewfactors in ',zname(ICOMP)
      write(ZN,'(A)') zname(ICOMP)

C Set VEWF array in preparation for editing or silent creation.
      call georead(IUF,LGEOM(ICOMP),ICOMP,1,IUOUT,IER)

C << update logic to use alternative source for ZOA rather than call zinfo >>
      CALL ZINFO(SNA,ZOA,PAZI,PELV,ZVOL,'q')
      NS=NZSUR(ICOMP)
      DO 111 I=1,NS
        II=I
        DO 201 J=1,NS
          JJ=J
          icc=izstocn(icomp,ii)
          jcc=izstocn(icomp,jj)
          VEWF(II,JJ)=0.0
          IF(II.EQ.JJ)goto 201
          VEWF(II,JJ)=(SSNA(jcc)/(ZOA-SSNA(icc)))
  201   CONTINUE
  111 CONTINUE
      if(silent)then
        if(IVF(ICOMP).eq.1)then

C There should be an existing viewfactor file.
C << ?? test the error return from ermrt call.
          call FINDFIL(LVIEW(ICOMP),XST)
          if(XST)then
            call edisp(iuout,' Opened existing viewfactor file.')
            CALL ERMRT(ITRC,ITRU,IUF,LVIEW(ICOMP),ICOMP,IER)
            if(ier.eq.3)then
              write(6,*) 'zone surface mrt surfs mismatch'
            endif
          else
            VFILE=LVIEW(ICOMP)
            CALL usrmsg(
     &        'Creating initial viewfactor file...',vfile,'-')
            CALL EMKMRT(LVIEW(ICOMP),LGEOM(ICOMP),NZSUR(ICOMP),IUF,
     &        ICOMP,'a',IER)
            if(ier.ne.0)then
              call usrmsg(
     &  'Problem detected while creating viewfactor data file.',
     &  'Check that you have adequate permissions and disk space.','W')
              IVF(ICOMP)=0
              return
            else
              IVF(ICOMP)=1
            endif
          endif
        elseif(IVF(ICOMP).eq.0)then

C Need to create a boilerplate file.
          VFILE=LVIEW(ICOMP)
          CALL usrmsg('Creating initial viewfactor file...',vfile,'-')
          CALL EMKMRT(LVIEW(ICOMP),LGEOM(ICOMP),NZSUR(ICOMP),IUF,
     &      ICOMP,'a',IER)
          if(ier.ne.0)then
            call usrmsg(
     &  'Problem detected while creating viewfactor data file.',
     &  'Check that you have adequate permissions and disk space.','W')
            IVF(ICOMP)=0
            return
          else
            CALL usrmsg(
     &        'Creating initial viewfactor file...done.',' ','-')
            IVF(ICOMP)=1
          endif
        endif

C If act = 'a' then return.
        if(act.eq.'a')then
          return
        else

C Otherwise commission an explicit calculation.

C Get logical name of terminal type, expand model name
C to include the path and create a string to drive espvwf module.
          doit = ' '
          call terminalmode(childterminal,tmode)
          if(unixok)then
            call addpath(LCFGF,longtfile,concat)
          else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
            call addpath(LCFGF,longtfile,concat)
            call cmdfiledos(longtfile,longtfiledos,ier)
            longtfile=' '
            longtfile=longtfiledos
          endif

C If prj initial size is a % of default pass this on to child with
C an offset from prj start position.
          if(iappw.eq.630)then
            iappwpc=100
          else
            iappwpc=nint(100.0*(real(iappw)/630.0))  ! reconstitute %
          endif
          if(iappwpc.gt.0.and.iappwpc.le.200)then
            write(doit,'(3a,3i4,4a)') 'espvwf -mode ',tmode,
     &        ' -s ',iappwpc,iappx+20,iappy+30,' -file ',
     &        longtfile(:lnblnk(longtfile)),' -zone ',ZN(1:lnblnk(ZN))
          else
            write(doit,'(6a)') 'espvwf -mode ',tmode,
     &        ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),
     &        ' -zone ',ZN(1:lnblnk(ZN))
          endif
          call usrmsg('starting espvwf module via',doit,'-')
          call runit(doit,tmode)
          call usrmsg(' Reading viewfactor file....',' ','-')
          call pauses(1)

C Test the error return from ermrt call.
          CALL ERMRT(ITRC,ITRU,IUF,LVIEW(ICOMP),ICOMP,IER)
          if(ier.eq.3)then
            write(6,*) 
     &      'zone surface mrt surfs mismatch on return from mrt'
          endif
          return
        endif
      endif

C Check and see if a viewfactor file exists.
C Viewfactor file exists, offer option to dereference it. Otherwise
C read it or create one with default (area weighted) viewfactors
C and no MRT sensors.
      helptopic='mrt_menu_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)
      if(IVF(ICOMP).eq.1)then
        LTMP=LVIEW(ICOMP)
        clkok=.false.
   46   CALL EASKSCMD(LTMP,'Zone viewfactor file? ',' ',
     &    'dereference',clkok,72,VFILE,'viewfactor file',IER,nbhelp)
        if(clkok)then
          LVIEW(ICOMP)='UNKNOWN'
          IVF(ICOMP)=0
          call usrmsg(' updating model to remove mrt...',' ','-')
          CALL EMKCFG('-',IER)
          cfgmod=.false.
          call usrmsg(' updating model to remove mrt...done',' ','-')
          return
        else
          IF(IER.NE.0)GOTO 46
          IF(LTMP(1:2).NE.'  ')THEN
            LVIEW(ICOMP)=LTMP
            IVF(ICOMP)=1
          ELSE
            goto 46
          ENDIF
        endif
        call edisp(iuout,' Opened existing viewfactor file.')

C Test the error return from ermrt call. If 3 then info in vwf
C file is out of date.
        CALL ERMRT(ITRC,ITRU,IUF,LVIEW(ICOMP),ICOMP,IER)
        if(ier.eq.3)then
          call easkabc(
     &      'Found difference in number of zone surfaces and MRT',
     &      'surfaces. Options:','Re-make area weighted',
     &      'Re-make via ray-tracing calcs','dereferemce',
     &      IW,nbhelp)
          if(IW.eq.1.or.IW.eq.2)then
            CALL ZINFO(SNA,ZOA,PAZI,PELV,ZVOL,'q')
            NS=NZSUR(icomp)
            DO I=1,NS
              II=I
              DO 21 J=1,NS
                JJ=J
                icc=izstocn(icomp,ii)
                jcc=izstocn(icomp,jj)
                VEWF(II,JJ)=0.0
                IF(II.EQ.JJ)goto 21
                VEWF(II,JJ)=(SSNA(jcc)/(ZOA-SSNA(icc)))
   21         CONTINUE
            ENDDO
            if(IW.eq.1)then
              CALL EMKMRT(LVIEW(ICOMP),LGEOM(ICOMP),NZSUR(ICOMP),IUF,
     &          ICOMP,'a',IER)
              call edisp(iuout,'Area weighted viewfactors saved.')
            elseif(IW.eq.2)then
              CALL EMKMRT(LVIEW(ICOMP),LGEOM(ICOMP),NZSUR(ICOMP),IUF,
     &          ICOMP,'a',IER)   ! should this be 'r' ?
              call edisp(iuout,'Now ask for a ray-trace calculation.')
            endif
          elseif(IW.eq.3)then
            LVIEW(ICOMP)='UNKNOWN'
            IVF(ICOMP)=0
            call usrmsg(' updating model to remove mrt...',' ','-')
            CALL EMKCFG('-',IER)
            cfgmod=.false.
            call usrmsg(' updating model to remove mrt...done',' ','-')
          endif
        endif
      else

C No mrt file referenced so offer default name as well as an option
C to back out of the facility.
        LTMP=VFILE
        clkok=.false.
   47   CALL EASKSCMD(LTMP,'Zone viewfactor file? ',' ',
     &    'dereference',clkok,72,VFILE,'viewfactor file',IER,nbhelp)
        if(clkok)then
          return
        else
          IF(LTMP(1:2).NE.'  ')THEN
            LVIEW(ICOMP)=LTMP
            IVF(ICOMP)=1
          ELSE
            goto 47
          ENDIF

C Check just in case the file exists (or was previously dereferenced).
          call FINDFIL(LVIEW(ICOMP),XST)
          IF(XST)THEN
            call edisp(iuout,' Opened existing viewfactor file.')

C Test the error return from ermrt call.
            CALL ERMRT(ITRC,ITRU,IUF,LVIEW(ICOMP),ICOMP,IER)
            if(ier.eq.3)then
              write(6,*) 
     &        'zone surface mrt surfs mismatch after read of existing'
            endif
            cfgmod=.true.
          else

C LVIEW does not exist create initial file with area weighted
C viewfactors. If NCUB(ICOMP) was defined via the geometry file
C then use that information.
C            write(6,*) ' EDMRT b',icomp,NCUB(ICOMP)
C            NCUB(ICOMP)=0
            CALL usrmsg('Creating area weighted viewfactor file...',
     &        ' ','-')
            CALL EMKMRT(LVIEW(ICOMP),LGEOM(ICOMP),NZSUR(ICOMP),IUF,
     &        ICOMP,'a',IER)
            write(6,*) ' EDMRT c',icomp,NCUB(ICOMP)
            if(ier.ne.0)then
              call usrmsg(
     &  'Problem detected while creating viewfactor data file.',
     &  'Check that you have adequate permissions and disk space.','W')
              IVF(ICOMP)=0
              return
            endif
            CALL usrmsg(
     &        'Creating area weighted viewfactor file...done.',
     &        ' ','-')
            cfgmod=.true.
          endif
        endif
      endif
      if(cfgmod)then
        call usrmsg(' updating model to include mrt...',' ','-')
        CALL EMKCFG('-',IER)
        cfgmod=.false.
        call usrmsg(' updating model to include mrt...done',' ','-')
      endif

C On entry refresh geometry, determine its bounds, force it
C drawn and remember number of surfaces to pass to sensor file.
      NZS=NZSUR(icomp)
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      MODIFYVIEW=.TRUE.
      MODLEN=.TRUE.
      MODBND=.TRUE.
      CALL ESCZONE(ICOMP)
      CALL BNDOBJ(0,IER)
      CALL ERCZONE(ICOMP)

C Initialise menu size variables based on window size. 
C IVERT is the menu position, MVERT the current no of lines.
      MHEAD=9
      MCTL=6
      ILEN=NCUB(ICOMP)
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      ILEN=NCUB(ICOMP)
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
    3 M=MHEAD
      WRITE(VERT(1),'(A,A)')  '  Viewfactor file: ',LVIEW(ICOMP)(1:26)
      VERT(2)='  ___________________________________________'
      VERT(3)='a Edit zone viewfactors                      '
      VERT(4)='b Calculate zone or MRT sensor viewfactors   '
      VERT(5)='  ___________________________________________'
      WRITE(VERT(6),'(A,I2)') '  Number of MRT sensors: ',NCUB(ICOMP)
      VERT(7)='  ___________________________________________'
      VERT(8)='  Sensor|   Origin (m)    |    Size (m)  |Rot'
      VERT(9)='  name  |Xcord|Ycord|Zcord|len |wid |ht  |deg'

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(M,KEY,IER)
          WRITE(VERT(M),303)KEY,CUBN(L),XOC(L),YOC(L),ZOC(L),
     &                      DXC(L),DYC(L),DZC(L),CANG(L)
  303     FORMAT(A1,1X,A6,3F6.1,3F5.2,F5.0)
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  ___________________________________________'
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 Page --- Part: ',I2,' of ',I2,' ---')
      ENDIF
      VERT(M+2)  ='+ Add a MRT sensor                 '
      VERT(M+3)  ='> Save MRT sensors & viewfactors '
      VERT(M+4)  ='! List zone viewfactors            '
      VERT(M+5)  ='? Help                             '
      VERT(M+6)  ='- Exit to model definition         '

C If editing the MRT sensors and user has asked for an 
C update of the image then use common block info for 
C the zone, obstructions and sensors.
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      CALL ADJVIEW(IER)
      call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
      if(MMOD.EQ.8)then
        call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
        WRITE(etext,'(2A)')'Model: ',modeltitle(1:lnblnk(modeltitle))
        iside=1
        isize=1
        ifont=1
        call viewtext(etext,iside,isize,ifont)
      endif
      CALL ESCZONE(ICOMP)
      CALL DRWSEN(ICOMP,ier)
      CALL ERCZONE(ICOMP)

C Help text for this menu.
      helptopic='mrt_menu_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      CALL EMENU(head,VERT,MVERT,IVERT)
      IF(IVERT.EQ.MVERT)THEN

C Exit to main routine.
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C List help text for the  menu.
        helptopic='mrt_menu_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('mrt sensor',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C List viewfactor contents.
        call edisp(itru,' ')
        WRITE(outs,'(A,I3)')' Number of surfaces = ',NZS
        call edisp(itru,outs)
        call edisp(itru,' ')
        call edisp(itru,' View Factors from Surface to Surface')
        DO 860 I=1,NZS
          DO 850 J=1,NZS
            ICNI=IZSTOCN(ICOMP,I)
            ICNJ=IZSTOCN(ICOMP,J)
            WRITE(outs,5203)SSNAME(icni),I,SSNAME(icnj),J,VEWF(I,J)
 5203       FORMAT(10X,A12,I3,'  to ',A12,I3,' = ',F6.4)
            call edisp(itru,outs)
  850     CONTINUE
  860   CONTINUE
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C Save viewfactor & sensor file...
        LTMP=LVIEW(ICOMP)
   48   CALL EASKS(LTMP,' Zone viewfactor file? ',
     &    ' ',72,VFILE,'viewfactor file',IER,nbhelp)
        IF(IER.NE.0)GOTO 48
        IF(LTMP(1:2).NE.'  ')THEN
          LVIEW(ICOMP)=LTMP
          IVF(ICOMP)=1
        ELSE
          IVF(ICOMP)=0
          goto 48
        ENDIF
        CALL usrmsg(' Updating the viewfactor file...',' ','-')
        CALL EMKMRT(LVIEW(ICOMP),LGEOM(ICOMP),NZS,IUF,ICOMP,'v',IER)
        CALL usrmsg(' Updating the viewfactor file...done.',' ','-')
        if(ncub(ICOMP).gt.0)then

C Also save any mrt sensors to geometry file.
          call eclose(gversion(ICOMP),1.1,0.01,newgeo)
          if(newgeo)then
            call geowrite(IUF,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          endif
        endif

      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C Add a mrt sensor.
        IF(NCUB(ICOMP)+1.LE.MCUB)THEN
          NCUB(ICOMP)=NCUB(ICOMP)+1
          NCU=NCUB(ICOMP)
          XOC(NCU)=ZCOG(ICOMP,1)
          YOC(NCU)=ZCOG(ICOMP,2)
          ZOC(NCU)=ZCOG(ICOMP,3)
          DXC(NCU)=0.1
          DYC(NCU)=0.1
          DZC(NCU)=0.1
          CANG(NCU)=0.
          write(CUBN(NCU),'(A,I2.2)')'sen_',NCU
          MODIFYVIEW=.TRUE.
        write(6,*) ' EDMRT g',icomp,NCUB(ICOMP)

C Also save mrt sensors to geometry file.
          call eclose(gversion(ICOMP),1.1,0.01,newgeo)
          if(newgeo)then
            call geowrite(IUF,LGEOM(ICOMP),ICOMP,iuout,3,IER)
        write(6,*) ' EDMRT h',icomp,NCUB(ICOMP)
          endif
        ELSE
          CALL USRMSG(' ',' Too many sensors defined!','W')
        ENDIF
      ELSEIF(IVERT.EQ.(MVERT-5))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.EQ.3)THEN

C Manual editing of viewfactors. Keep track of the sum and
C offer the user a chance to re-edit if not close to one.
        A=0.0
        IS=1
        CALL EPMENSV
        CALL EASKSUR(ICOMP,IS,'-',
     &    'Select surface for viewfactor editing.',' ',IER)
  31    do 33 MM=1,NZS
          if(MM.NE.IS)then
            ICNI=IZSTOCN(ICOMP,IS)
            ICNJ=IZSTOCN(ICOMP,MM)
            write(outs,'(4A)') ' For ',SSNAME(ICNI),' to ',
     &        SSNAME(ICNJ)
            V=VEWF(IS,MM)
            CALL EASKR(V,outs,' view factor: ',
     &        0.,'F',0.999,'F',0.,'viewfactors',IER,nbhelp)
            VEWF(IS,MM)=V
          else
            VEWF(IS,IS)=0.
          endif
          A=A+VEWF(IS,MM)
          write(outs,'(A,F8.5)') ' Current total is ',A
          call edisp(iuout,outs)
  33    continue
        IF(ABS(A).LT.0.9.OR.ABS(A).GT.1.002)then
          write(outs,'(A,F8.5,A)')' currently the total is ',A,
     &      ' and should be close to zero...please re-edit...'
          call easkab(outs,' ','re-edit','continue',IW,nbhelp)
          if(iw.eq.1)then
            A=0.0  ! clear the accumulator
            goto 31
          endif
        endif
        CALL EPMENRC
      ELSEIF(IVERT.EQ.4)THEN

C View factor calculations - offer choice of default viewfactors or
C call to ESPVWF. Recover information on areas in the current zone for
C use in area weighted computation.
        call easkabc(' Zone viewfactor calculations can be either',
     &    ':','Area weighted','Based on ray-tracing calcs','continue',
     &    IW,nbhelp)
        if(IW.eq.1)then

C << Rewrite logic to use G7 common blocks rather than call zinfo. >>

          CALL ZINFO(SNA,ZOA,PAZI,PELV,ZVOL,'q')
          NS=NZSUR(icomp)
          DO 11 I=1,NS
            II=I
            DO 20 J=1,NS
              JJ=J
              icc=izstocn(icomp,ii)
              jcc=izstocn(icomp,jj)
              VEWF(II,JJ)=0.0
              IF(II.EQ.JJ)goto 20
              VEWF(II,JJ)=(SSNA(jcc)/(ZOA-SSNA(icc)))
   20       CONTINUE
   11     CONTINUE
        elseif(IW.eq.2)then

C Get logical name of terminal type, expand model name
C to include the path and create a string to drive espvwf.
          doit = ' '
          call terminalmode(childterminal,tmode)
          if(unixok)then
            call addpath(LCFGF,longtfile,concat)
          else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
            call addpath(LCFGF,longtfile,concat)
            call cmdfiledos(longtfile,longtfiledos,ier)
            longtfile=' '
            longtfile=longtfiledos
          endif

C If prj initial size is a % of default pass this on to child with
C an offset from prj start position.
          if(iappw.eq.630)then
            iappwpc=100
          else
            iappwpc=nint(100.0*(real(iappw)/630.0))  ! reconstitute %
          endif
          if(iappwpc.gt.0.and.iappwpc.le.200)then
            write(doit,'(3a,3i4,5a)') 'espvwf -mode ',tmode,
     &        ' -s ',iappwpc,iappx+20,iappy+30,' -file ',
     &        longtfile(:lnblnk(longtfile)),' -zone ',ZN(1:lnblnk(ZN)),
     &        ' &'
          else
            write(doit,'(7a)') 'espvwf -mode ',tmode,
     &        ' -s 0 0 0 -file ',longtfile(:lnblnk(longtfile)),
     &        ' -zone ',ZN(1:lnblnk(ZN)),' &'
          endif
          call usrmsg('starting espvwf module via',doit,'-')
          call runit(doit,tmode)

          helptopic='pause_for_calcs'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('espvwf running',nbhelp,'-',0,0,IER)
          CALL EASKABC(' Post calculation choices :',' ',
     &      'use new data','revert (no viewfactors)','continue',
     &      irt,nbhelp)
          if(irt.eq.1)then

C Test the error return from ermrt call.
            call edisp(iuout,' Reading viewfactor file....')
            CALL ERMRT(ITRC,ITRU,IUF,LVIEW(ICOMP),ICOMP,IER)
            if(ier.eq.3)then
              write(6,*) 'post calc zone surface mrt surfs mismatch'
            endif
            goto 92
          elseif(irt.eq.2)then
            IVF(ICOMP)=0
            CALL ERPFREE(IUF,ISTAT)
            CALL EFOPSEQ(IUF,LVIEW(ICOMP),1,IER)
            if(ier.eq.0)then
              write(outs,'(A,A)') ' removing ',LVIEW(ICOMP)
              call edisp(itru,outs)
              CALL EFDELET(IUF,ISTAT)
            endif
            CALL EMKCFG('-',IER)
          endif
        endif
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Edit item identified by KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        ICUB=IFOC
        CALL EDMRTS(ICOMP,ICUB,IER)
      ELSE

C Not one of the legal menu choices.
        IVERT=-1
        GOTO 3
      ENDIF
      GOTO 92

      END 


C ************* EDMRTS
C EDMRTS: Edit details of a MRT sensor block.
      SUBROUTINE EDMRTS(ICOMP,ICUB,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "prj3dv.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      DIMENSION VERT(15)
      CHARACTER VERT*28,MSG*72,S6*6,head*30,outs*124,hold*36
      character ETEXT*82
      integer NITMS,IVERT ! max items and current menu item
      logical newgeo
      DIMENSION IVLST(MTV)
      integer INPICK  ! to use with epkvert

C Passed parameters for cfgtogg
#ifdef OSI
      integer icfg_type  ! model cfg type
      integer icfgz      ! if non-zero then there are zones
      integer icfgn      ! if non-zero then there are networks
      integer icfgc      ! if non-zero then cfg file known
      integer icfgdfn    ! if non-zero then cdf domain exists
      integer iicfgz     ! there are zone related images
      integer iicfgn     ! there are network related images
      integer iicfgc     ! there are control related images
      integer iicfgdfn   ! there are cfd related images
#else
      integer*8 icfg_type,icfgz,icfgn,icfgc,icfgdfn,iicfgz
      integer*8 iicfgn,iicfgc,iicfgdfn
#endif

#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDMRTS'  ! set for subroutine

C Echo current information and then offer choice of manual
C edit or select zone vertex.

C Initial menu entry setup.
      write(head,'(A,A)')'  MRT details: ',zname(ICOMP)
      MODIFYVIEW=.FALSE.
   92 IER=0
      IVERT=-3

C Set menu header text.
      WRITE(VERT(1),'(A,F7.3)') 'a Sensor origin X : ',XOC(ICUB)
      WRITE(VERT(2),'(A,F7.3)') 'b Sensor origin Y : ',YOC(ICUB)
      WRITE(VERT(3),'(A,F7.3)') 'c Sensor origin Z : ',ZOC(ICUB)
      WRITE(VERT(4),'(A,F7.3)') 'd Sensor width (X): ',DXC(ICUB)
      WRITE(VERT(5),'(A,F7.3)') 'e Sensor depth (Y): ',DYC(ICUB)
      WRITE(VERT(6),'(A,F7.3)') 'f Sensor height(Z): ',DZC(ICUB)
      WRITE(VERT(7),'(A,F5.1)') 'g Sensor rotation : ',CANG(ICUB)
      WRITE(VERT(8),'(A,A6)')   'h Sensor name : ',CUBN(ICUB)
      VERT(9)    ='  _______________________ '
      VERT(10)   ='? Help                    '
      VERT(11)   ='- Exit to MRT menu        '

C If editing the sensors and user has asked for an update of the
C image then use common block info for the zone as well as for the
C sensors. Draw the zone and obstructions as standard and then
C draw the sensor cubes.  Set all surface lines to standard width.
      CALL INLNST(1)
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      CALL ADJVIEW(IER)
      call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
      if(MMOD.EQ.8)then
        call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
        WRITE(etext,'(2A)')'Model: ',modeltitle(1:lnblnk(modeltitle))
        iside=1; isize=1; ifont=1
        call viewtext(etext,iside,isize,ifont)
      endif
      CALL ESCZONE(ICOMP)
      CALL DRWSEN(ICOMP,ier)
      CALL ERCZONE(ICOMP)

C Help text for this menu.
      helptopic='mrt_sensor_dimensions'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      NITMS=11
      CALL EMENU(head,VERT,NITMS,IVERT)
      IF(IVERT.EQ.1.or.IVERT.eq.2.or.IVERT.eq.3)THEN
        write(outs,'(a,3F8.3)')'Origin is: ',XOC(ICUB),YOC(ICUB),
     &    ZOC(ICUB)
        call edisp(iuout,outs)
        write(outs,'(a,3F8.3)')'Dimensions:',DXC(ICUB),DYC(ICUB),
     &    DZC(ICUB)
        call edisp(iuout,outs)
        call easkabc('Origin choices:',' ','edit sensor origin',
     &    'use zone vertex','abort',ibopt,nbhelp)
        if(ibopt.eq.1)then
          IF(IVERT.EQ.1)THEN
            WRITE(MSG,'(a,I3)')'For sensor ',ICUB
            CALL EASKR(XOC(ICUB),MSG,'Origin X coordinate?',
     &       -999.9,'W',999.9,'W',1.0,'origin X coord',IER,nbhelp)
            IVFOK(icub)=0
            MODIFYVIEW=.TRUE.
            MODLEN=.TRUE.
          ELSEIF(IVERT.EQ.2)THEN
            CALL EASKR(YOC(ICUB),MSG,'Origin Y coordinate?',
     &       -999.9,'W',999.9,'W',1.0,'origin X coord',IER,nbhelp)
            IVFOK(icub)=0
            MODIFYVIEW=.TRUE.
            MODLEN=.TRUE.
          ELSEIF(IVERT.EQ.3)THEN
            CALL EASKR(ZOC(ICUB),MSG,'Origin Z coordinate?',
     &       -999.9,'W',999.9,'W',1.0,'origin X coord',IER,nbhelp)
            MODIFYVIEW=.TRUE.
            MODLEN=.TRUE.
          ENDIF
        elseif(ibopt.eq.2)then

C User current zone G1 common block to present list of points.
          inpick=1
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Vertices for origin',
     &      'Select a vertex to define the origin...',' ',nbhelp,ier)
          CALL EPMENRC
          if(inpick.eq.1)then
            iwhich1=IVLST(1)
            HOLD=' '
            WRITE(hold,'(1x,3f8.4)')X(iwhich1),Y(iwhich1),Z(iwhich1)
            CALL EASKS(HOLD,'Confirm object origin @ (X Y Z metres)',
     &      'i.e. within the room: ',
     &      36,' 0. 0. 0.','origin XYZ',IER,nbhelp)
            K=0
            CALL EGETWR(HOLD,K,DX,-99.,99.,'W','X tr',IER)
            CALL EGETWR(HOLD,K,DY,-99.,99.,'W','Y tr',IER)
            CALL EGETWR(HOLD,K,DZ,-99.,99.,'W','Z tr',IER)
            XOC(ICUB)=DX; YOC(ICUB)=DY; ZOC(ICUB)=DZ
            MODIFYVIEW=.TRUE.
            MODLEN=.TRUE.
          else
            goto 92
          endif
        elseif(ibopt.eq.3)then
          goto 92
        endif
      ELSEIF(IVERT.EQ.4)THEN
        CALL EASKR(DXC(ICUB),MSG,' Sensor width? ',
     &       0.001,'W',99.9,'W',1.0,'sensor X dimension',IER,nbhelp)
        IVFOK(icub)=0
        MODIFYVIEW=.TRUE.
        MODLEN=.TRUE.
      ELSEIF(IVERT.EQ.5)THEN
        CALL EASKR(DYC(ICUB),MSG,' Sensor depth? ',
     &       0.001,'W',99.9,'W',1.0,'sensor Y dimension',IER,nbhelp)
        IVFOK(icub)=0
        MODIFYvIEW=.TRUE.
        MODLEN=.TRUE.
      ELSEIF(IVERT.EQ.6)THEN
        CALL EASKR(DZC(ICUB),MSG,' Sensor height? ',
     &       0.001,'W',99.9,'W',1.0,'sensor Z dimension',IER,nbhelp)
        IVFOK(icub)=0
        MODIFYVIEW=.TRUE.
        MODLEN=.TRUE.
      ELSEIF(IVERT.EQ.7)THEN
        CALL EASKR(CANG(ICUB),MSG,' Sensor rotation? ',
     &       -359.0,'W',359.0,'W',0.0,'sensor rotation',IER,nbhelp)
        IVFOK(icub)=0
        MODIFYVIEW=.TRUE.
        MODLEN=.TRUE.
      ELSEIF(IVERT.EQ.8)THEN
        S6=' '
        CALL EASKS(S6,' ',' Name of MRT sensor ? ',
     &    6,'sensor','sensor name',IER,nbhelp)
        IF(S6.NE.' ')CUBN(ICUB)=S6
        MODIFYVIEW=.TRUE.
      ELSEIF(IVERT.EQ.10)THEN
        helptopic='mrt_sensor_dimensions'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('sensor menu',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.11)THEN
        return
      ENDIF

C If we changed something and we have a newer geometry file
C update the file.
C Also save mrt sensors to geometry file.
      if(MODIFYVIEW)then
        IUF=IFIL+2
        call eclose(gversion(ICOMP),1.1,0.01,newgeo)
        if(newgeo)then
          call geowrite(IUF,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          write(outs,*) ' EDMRTS a update geometry'
          call edisp(iuout,outs)
        endif
      endif
      GOTO 92

      END
