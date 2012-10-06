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

C The file edzone.f is a collection of support facilities for 
C creating zones and adding/ subtracting vertices and surfaces:
C  SCRZONE: creates initial form of an enclosure as prism or extrusion.
C  NEWZONE: controls creation of a new zone via prj or cad or file.
C  DELZONE: systematically removes a thermal zone.
C  WARNMOD: provide warnings when model evolves.
C  SUMRCHG: provides a summary of updates needed after model changes.
C comissionish: invokes the shading module in a number of modes
C  POINTTOLINE: determines distance from a 3D point to a 3D line.


C ************* SCRZONE 
C SCRZONE provides initial form of an enclosure as either a prism or
C an extrusion. IER=0 OK. 
      SUBROUTINE SCRZONE(ICOMP,IER)
#include "building.h"
#include "model.h"

C Use geometry for common g0/g1/g2/g4/g5/g6/g7/prec16/precz/c20.
#include "geometry.h"
#include "esprdbfile.h"
#include "prj3dv.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN

      Logical MODIFY,MODLEN,MODBND
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/user/browse
      common/cctlnm/ctldoc,lctlf
      common/cctl/icascf(mcom)

      DIMENSION XX(MS),YY(MS)
      CHARACTER HOLD*32
      character MODE*4,fs*1
      CHARACTER OUTSTR*124,ZN*12,outs*124
      character ctldoc*248,LCTLF*72,zd*64,DFILE*72,CFILE*72,OFILE*72
      LOGICAL OK,dok,browse,OKC,XST,unixok
      logical newgeo  ! to use for testing if new/old geometry file.
      real DX1,DY1,DZ1  ! to avoid name clash with geometry.h
      real XO1,YO1,ZO1
      real ANGR,ANGE    ! rotation angles

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='SCRZONE'  ! set for subroutine

C << Consider how a more robust cancel facility can
C << be established. Possibly one that deletes the new zone
C << from the model or that finds ways to uninstantiate the
C << common block data that was set prior to the cancel instruction.

C If browsing then user cannot create a new zone.
      if(browse)then
        call usrmsg('Cannot update model while in browse',
     &    'mode, you must `own` the model!','W')
        return
      endif
 
C If newer model cfg file ensure version 1.1 geometry file created. 
      if(icfgv.lt.4)then
      newgeo=.false.  ! assume older format geometry.
      else
        newgeo=.true.   ! set to newer format
        gversion(icomp)=1.1
      endif

C Set folder separator (fs) to \ or / as required.
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

C Creation of a new zone, first ask its name, trapping out
C illegal characters.
      call tstamp('>','PRJ: new zone')
      ZN=' '
 42   helptopic='zone_name'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKS(ZN,'New zone description?','(<12 chars, no spaces)',
     &  12,'New zone','zone name',IER,nbhelp)
      IF(ZN.eq.' '.or.ier.ne.0)goto 42
      call st2name(ZN,zname(ICOMP))
      lnzname(icomp)=lnblnk(zname(ICOMP))  ! update this string length.

C << Unix vs NT needs to be updated >>
      if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
        WRITE(DFILE,'(2a)') zname(ICOMP)(1:lnzname(ICOMP)),'.geo'
        WRITE(CFILE,'(2a)') zname(ICOMP)(1:lnzname(ICOMP)),'.con'
        WRITE(OFILE,'(2a)') zname(ICOMP)(1:lnzname(ICOMP)),'.opr'
      else
        WRITE(DFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &     zname(ICOMP)(1:lnzname(ICOMP)),'.geo'
        WRITE(CFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &     zname(ICOMP)(1:lnzname(ICOMP)),'.con'
        WRITE(OFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &     zname(ICOMP)(1:lnzname(ICOMP)),'.opr'
      endif
      LGEOM(ICOMP)=DFILE
      LTHRM(ICOMP)=CFILE
      LPROJ(ICOMP)=OFILE

      write(zd,'(2a)') zname(ICOMP)(1:lnzname(ICOMP)),' describes a'
 43   CALL EASKS(zd,'What does it represent?','(<64 characters)',
     &  64,'no description entered','zone descr',IER,nbhelp)
      IF(zd.eq.' '.or.ier.ne.0)goto 43
      zdesc(ICOMP)=zd
      lnzdesc(ICOMP)=lnblnk(zd)  ! update the length of this string.

C Allow user choices of beginning as a simple shape (with image feedback). 
      IER=0
      helptopic='zone_initial_shapes'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKABCD(' ','Base zone geometry on:',
     &    'rectangular plan','polygon plan','general 3D',
     &  'bitmap',ishape,nbhelp)
 144  IF(ishape.eq.1)THEN
        XO1=0.
        YO1=0.
        ZO1=0.
        DX1=5.0
        DY1=3.0
        DZ1=2.7
        helptopic='zone_initial_rect'
        call gethelptext(helpinsub,helptopic,nbhelp)
        HOLD=' '
        WRITE(HOLD,'(1x,3f8.3)')XO1,YO1,ZO1
        CALL EASKS(HOLD,' ','Origin X,Y,Z?',
     &    32,' 0. 0. 0. ','origin coord',IER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,XO1,-999.9,999.9,'W','X origin',IER)
        CALL EGETWR(HOLD,K,YO1,-999.9,999.9,'W','Y origin',IER)
        CALL EGETWR(HOLD,K,ZO1,-9.9,999.9,'W','Z origin',IER)
	write(outs,'(a,3f9.4)') 'Origin is ',XO1,YO1,ZO1
	call edisp(iuout,outs)

 242    HOLD=' '
        WRITE(HOLD,'(1x,3f8.3)')DX1,DY1,DZ1
        CALL EASKS(HOLD,' ','Rectangle length, width and height?',
     &    32,' 2.5 3.5 2.7 ','rectangle size',IER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,DX1,0.001,99.9,'W','box width',IER)
        CALL EGETWR(HOLD,K,DY1,0.001,99.9,'W','box depth',IER)
        CALL EGETWR(HOLD,K,DZ1,0.001,99.9,'W','box height',IER)
        if(DX1.lt.0.001.or.DY1.lt.0.001)goto 242
	write(outs,'(a,3f9.4)') 'Width depth height is ',DX1,DY1,DZ1
	call edisp(iuout,outs)

        ANGR=0.
        CALL EASKR(ANGR,' ','Rectangle orientation?',
     &    -359.0,'W',359.0,'W',0.0,'box orientation',IER,nbhelp)

        ANGE=0.
        CALL EASKR(ANGE,' ','Rectangle elevation (see help)?',
     &    -359.0,'W',359.0,'W',0.0,'box elevation',IER,nbhelp)

C Convert box into a gen description using two rotation angles.
C Pass in critical dimensions and expect the data back via common G1.
        CALL ERECC3A(XO1,YO1,ZO1,DX1,DY1,DZ1,ANGR,ANGE,0.0)
        NSUR=6
        IUZBASEA(icomp)=0
        IBASES(ICOMP,1)=6
        MODBND=.TRUE.
        MODIFY=.TRUE.
        MODLEN=.TRUE.
        iZBFLG(ICOMP)=0
      ELSEIF(ishape.eq.2)THEN

C User begins with an extruded shape.
        Z1=0.
        Z2=2.4
        NW=4
        helptopic='zone_initial_extrude'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKR(Z1,' ','Z value of the base surface?',
     &    0.000,'W',99.9,'W',0.0,'elevation of base',IER,nbhelp)
        CALL EASKR(Z2,' ','Z value of the top surface?',
     &    0.000,'W',99.9,'W',2.7,'elevation of top',IER,nbhelp)
        CALL EASKI(NW,' ','Number of walls?',
     &    3,'F',MS-2,'F',4,'no of walls',IERI,nbhelp)
        if(ieri.eq.-3) goto 42

        DO 45 IW=1,NW
          WRITE(OUTSTR,'(A,I2,A)')' For base vertex',IW,'  X & Y:'
          HOLD=' '
          WRITE(HOLD,'(1x,2f8.3)')XX(IW),YY(IW)
          CALL EASKS(HOLD,OUTSTR,'  ',32,' 0.  0.  ','floor xy',
     &      IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,XX(IW),-999.,999.,'W','flr X',IER)
          CALL EGETWR(HOLD,K,YY(IW),-999.,999.,'W','flr Y',IER)
	  write(outs,'(a,i3,a,2f9.4)') 'X&Y for v ',IW,' is ',
     &      XX(IW),YY(IW)
	  call edisp(iuout,outs)
   45   CONTINUE

C Allow user to have a 2nd chance to input the vertices.
        dok=.true.
        CALL ASKOK(' ','Accept base vertices?',OK,dok,nbhelp)
        if(.NOT.ok)then
          ishape=2
          goto 144
        endif

        AR=0.
        CALL EASKR(AR,' ','Rotation angle?',
     &    -359.0,'W',359.0,'W',0.0,'extrusion rotation',IER,nbhelp)

C Convert into a gen body and rotate if required. Pass in critical dimensions
C and expect the data back via common G1. And ESCROT also acts on G1.
        CALL EREGC(NW,Z1,Z2,XX,YY)
        IF(AR.LT.-.01.OR.AR.GT..01)then
          x1=XX(1)
          y1=YY(1)
          CALL ESCROT(AR,x1,y1)
        endif
        NSUR=NW+2
        IUZBASEA(icomp)=0
        IBASES(ICOMP,1)=NSUR
        MODBND=.TRUE.
        MODIFY=.TRUE.
        MODLEN=.TRUE.
        iZBFLG(ICOMP)=0
      ENDIF

C Begin with default assumptions for each surface then overwrite
C this if user supplied information exists.
      IF(ishape.ne.3.and.ishape.ne.4)THEN

C Use subroutine filsur to instanciate G5 common block.
        CALL FILSUR(ICOMP,0)

C Update the connection list and hash.
        ICCC=NCON
        DO 32 ICC=1,NSUR
          ICCC=ICCC+1
          IC1(ICCC)=ICOMP
          IE1(ICCC)=ICC
          ICT(ICCC)=-1   ! mark as unknown at this time.
          IC2(ICCC)=0
          IE2(ICCC)=0
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
   32   CONTINUE
        NCON=ICCC
      endif
      if(ishape.eq.3)then

C User will begin with a minimal GEN body.
        helptopic='zone_initial_polyhedra'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('gen start popup',nbhelp,'-',0,0,IER)
        NTV=4
        NSUR=1
        AR=0.0
        HOLD=' '
        WRITE(HOLD,'(1x,3f8.3)')X(1),Y(1),Z(1)
        CALL EASKS(HOLD,' ','Origin X Y Z?',
     &    32,' 0. 0. 0. ','origin coord',IER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,X0,0.,0.,'-','X origin',IER)
        CALL EGETWR(HOLD,K,Y0,0.,0.,'-','Y origin',IER)
        CALL EGETWR(HOLD,K,Z0,0.,0.,'-','Z origin',IER)
        X(1)=X0+2.0
        Y(1)=Y0
        Z(1)=Z0
        X(2)=X0
        Y(2)=Y0
        Z(2)=Z0
        X(3)=X0
        Y(3)=Y0+2.0
        Z(3)=Z0
        X(4)=X0+2.0
        Y(4)=Y0+2.0
        Z(4)=Z0
        NVER(1)=4
        JVN(1,1)=1
        JVN(1,2)=2
        JVN(1,3)=3
        JVN(1,4)=4
        NZTV(icomp)=NTV
        SNAME(ICOMP,1)='first'
        SOTF(1)='OPAQUE'
        SMLCN(1)='UNKN'
        SVFC(1)='FLOR'
        SOTHER(1,1)='UNKNOWN'
        SOTHER(1,2)='0'
        SOTHER(1,3)='0'
        SUSE(1,1)='-'
        SUSE(1,2)='-'
        SPARENT(1)='-'
        IUZBASEA(icomp)=0
        IBASES(ICOMP,1)=1

C Update the connection list and hash for initial surface.
        ICCC=NCON
        ICCC=ICCC+1
        IC1(ICCC)=ICOMP
        IE1(ICCC)=1
        ICT(ICCC)=-1  ! mark as unknown
        IC2(ICCC)=0
        IE2(ICCC)=0
        SSNAME(iccc)=SNAME(ICOMP,1)
        SSOTF(iccc)=SOTF(1)
        SSMLCN(iccc)=SMLCN(1)
        SSVFC(iccc)=SVFC(1)
        SSOTHER(iccc,1)=SOTHER(1,1)
        SSOTHER(iccc,2)=SOTHER(1,2)
        SSOTHER(iccc,3)=SOTHER(1,3)
        SSUSE(iccc,1)=SUSE(1,1)
        SSUSE(iccc,2)=SUSE(1,2)
        SSPARENT(iccc)=SPARENT(1)
        IZSTOCN(icomp,1)=iccc
        NCON=ICCC
        MODBND=.TRUE.
        MODIFY=.TRUE.
        MODLEN=.TRUE.
        iZBFLG(icomp)=0
      elseif(ishape.eq.4)then

C Point and click on a bitmap. After return, re-scan the system
C configuration file to ensure that all the derived information
C in the new geometry files is established.
        if(MMOD.eq.8)then
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          if(iglib.eq.1)then
            call clickonbitmap(icomp,ier) 
            if(ier.ne.0)then
              itrc=0
              MODE='ALL '
              IAPROB=IPRODB
              call edisp(iuout,'  ')  ! echo blank line in case of warnings
              CALL ERSYS(LCFGF,IFCFG,IAPROB,MODE,itrc,IER)
            endif
            return
          elseif(iglib.eq.2)then
            call usrmsg('The clickonbitmap facility is not yet working',
     &      'with new graphic library.','W')
          endif
        else
          call usrmsg('Must be in graphic mode to use click on bitmap',
     &      'facility to define vertices or floor plan corners. ','W')
          return
        endif
      endif

C High level info for all cases except clickonbitmap.
      IF(zname(ICOMP)(1:2).EQ.'  ')THEN
        IF(ICOMP.LE.9)WRITE(zname(ICOMP),'(A5,I1)')'Zone-',ICOMP
        IF(ICOMP.GT.9)WRITE(zname(ICOMP),'(A5,I2)')'Zone-',ICOMP
        lnzname(ICOMP)=lnblnk(zname(ICOMP))  ! update length for this string.
      ENDIF
      if(zdesc(ICOMP)(1:2).EQ.'  ')then
        write(zdesc(ICOMP),'(2a)') zname(ICOMP)(1:lnzname(ICOMP)),
     &    ' describes a '
        lnzdesc(ICOMP)=lnblnk(zdesc(ICOMP))  ! update the length of this string.
      endif
      CTYPE(icomp)='GEN '
      NDP(ICOMP)=3
      IDPN(ICOMP,1)=0
      IDPN(ICOMP,2)=0
      IDPN(ICOMP,3)=0
      NZSUR(ICOMP)=NSUR  ! update nzsur() it is needed by zgupdate.
      NZTV(ICOMP)=NTV

C Update the G7 common blocks and then assign ZBASEA.
      call zgupdate(1,ICOMP,ier)

C Find co-planer surfaces and edges of similar materials.
      call suredgeadj(itrc,'-',icomp,ier)
      if(ishape.eq.1)then
        ibcs=IZSTOCN(icomp,IBASES(ICOMP,1))
        ZBASEA(icomp)= SSNA(ibcs)
      elseif(ishape.eq.2)then
        ibcs=IZSTOCN(icomp,IBASES(ICOMP,1))
        ZBASEA(icomp)= SSNA(ibcs)
      elseif(ishape.eq.3)then
        ibcs=IZSTOCN(icomp,IBASES(ICOMP,1))
        ZBASEA(icomp)= SSNA(ibcs)
      endif

C Update the global coordinates for this zones surfaces so that
C the subsequent wireframe image can be drawn and the bounds of
C the zone can be calculated. Also instanciate nznver and nzjvn
C for the new zone (as is done in subroutine zdata).
      DO 40 J=1,NZTV(ICOMP)
        VCOORD(ICOMP,J,1)=X(J)
        VCOORD(ICOMP,J,2)=Y(J)
        VCOORD(ICOMP,J,3)=Z(J)
   40 CONTINUE
      DO 50 J=1,nzsur(ICOMP)
        icc=IZSTOCN(icomp,j)
        if(icc.ne.0)then
          NZNVER(icc)=NVER(J)
          N = NVER(J)
          DO 60 K=1,N
            NZJVN(icc,K)=JVN(J,K)
   60     CONTINUE
        endif
   50 continue
C Save this to file before passing into the geometry editing facility.
      call eclose(gversion(icomp),1.1,0.01,newgeo)
      if(igupgrade.eq.2.and.(.NOT.newgeo))then
        gversion(icomp) =1.1
        newgeo = .true.
      endif
      if(newgeo)then
        call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
      else
        call emkgeo(IFIL+2,LGEOM(ICOMP),ICOMP,3,IER)
      endif
      IF(IER.EQ.1)THEN
        dok=.true.
        helptopic='zone_initial_write'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL ASKOK('Problem creating geometry file.','Try again?',
     &              OK,dok,nbhelp)
        IF(OK)GOTO 42
      ENDIF

C Read in control file if defined.
      OKC=.false.
      if(LCTLF(1:1).ne.' '.and.LCTLF(1:4).ne.'UNKN')OKC=.true.
      if(OKC)then
        ICTLF=IFIL+1
        CALL ERPFREE(ICTLF,ISTAT)
        call FINDFIL(LCTLF,XST)
        IF(XST) CALL EZCTLR(ICTLF,ITRC,IUOUT,IER)
      endif

      NCOMP=NCOMP+1
      NCCODE(ICOMP)=NCOMP
      if(OKC)then
        icascf(NCOMP)=0
        call usrmsg(' updating control for additional zone...',' ','-')
        call CTLWRT(ICTLF,IER)
        call usrmsg(' ',' ','-')
      endif

      CALL EMKCFG('-',IER)

      RETURN
      END

C ************* NEWZONE 
C NEWZONE controls the creation of a new zone. IER=0 OK. 
      SUBROUTINE NEWZONE(ITRC,IC,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "prj3dv.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/cctlnm/ctldoc,lctlf
      common/cctl/icascf(mcom)

      CHARACTER*72 LTMP,DFILE
      CHARACTER OUTSTR*124,ctldoc*248,LCTLF*72
      character sfile*72,snpfile*72,fs*1,outs*124

      LOGICAL OK,DOK,XST,OKC,unixok
      logical newgeo  ! to use for testing if new/old geometry file.
      logical altered ! to signal change in boundary conditions of added existing zone.

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found
      integer ianother   ! flag for jumping to a different zone

      helpinsub='NEWZONE'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      newgeo=.false.  ! assume older format geometry.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

 293  helptopic='zone_source_files'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKABCD(' ','New zone options:','input dimensions',
     &  'load existing (ESP-r)','load existing (cflow3 zip)',
     &  'cancel',INZOPT,nbhelp)

C Input dimensions.
      if(INZOPT.eq.1)then

C Provide scratch description and the default assumptions for
C other zone files. If the cfg file is version 4 or newer then
C set to create version 1.1 zone geometry files. Do not bother
C to react to user request for jump to prior or next zone.
        CALL SCRZONE(IC,IER)
        CALL EDZONE(ITRC,IC,ianother,IER)
        if(ianother.ne.0)then
          call usrmsg('Ignoring request to jump to different zone',
     &      '  ','-')
        endif
        iZBFLG(IC)=0
        return

C Load existing ESP-r.
      elseif(INZOPT.eq.2)then

C Find out if there are geometry files in the ../zones folder.
        sfile=' '
        snpfile=' '
        call edisp(iuout,' ')
        call browsefilelist('?','zon','geo',sfile,snpfile,nfile,iier)
        if(nfile.gt.0)then
          sfile=' '
          snpfile=' '
          call browsefilelist('b','zon','geo',sfile,snpfile,nfile,iier)

          if(snpfile(1:2).ne.'  ')then
            write(ltmp,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &        snpfile(1:lnblnk(snpfile))

C Debug.
C            write(6,*) ltmp

          else

C User did not select a file. Take them back to the question.
            call usrmsg('You did not select any existing file.',
     &         'Please select or cancel.','W')
            goto 293 ! ask for choice again
          endif
        else
          call usrmsg('You did not select any existing file.',
     &       'Please select or cancel.','W')
          goto 293 ! ask for choice again
        endif

C Existing ESP-r geom file.
        DFILE=' '
        WRITE(OUTSTR,'(a,i2,a)')' For zone ',IC,
     &   ' the geometry is defined in:'
        CALL EASKS(LTMP,OUTSTR,' ',72,DFILE,'geom file',IER,nbhelp)
        IF(LTMP(1:2).EQ.'  ')GOTO 293
        call FINDFIL(LTMP,XST)
        IF(.NOT.XST)THEN
          dok=.true.
          CALL ASKOK(' ','Geometry file not found, try again?',OK,
     &      dok,nbhelp)
          if(OK)then
            GOTO 293 ! as for choice again
          else
            ier = -3 ! signal user cancel
            return   ! stop this before the model gets messed up.
          endif
        ENDIF
        LGEOM(IC)=LTMP

C Scan geometry file and update control and connections.
C Try reading in the geometry to fill in the data below.
          call georead(IFIL+1,LGEOM(IC),IC,1,iuout,IER)
        IF(IER.NE.0)THEN
          dok=.true.
          CALL ASKOK(OUTSTR,
     &      'Problem scanning geometry file...try again?',
     &      OK,dok,nbhelp)
          if(OK)then
            GOTO 293
          else
            ier = -3 ! signal user cancel
            return   ! stop this before the model gets messed up.
          endif
        ENDIF
        call eclose(gversion(ic),1.1,0.01,newgeo) ! find out what version

C Read in control file if defined.
        OKC=.false.
        if(LCTLF(1:1).ne.' '.and.LCTLF(1:4).ne.'UNKN')OKC=.true.
        if(OKC)then
          ICTLF=IFIL+1
          CALL ERPFREE(ICTLF,ISTAT)
          call FINDFIL(LCTLF,XST)
          IF(XST) CALL EZCTLR(ICTLF,ITRC,IUOUT,IER)
        endif

C Update the connection list and then the model configuration.
        NCOMP=NCOMP+1
        NZSUR(NCOMP)=NSUR
        NZTV(NCOMP)=NTV
        NCCODE(NCOMP)=NCOMP
        iZBFLG(NCOMP)=0
        if(OKC)then
          icascf(NCOMP)=0
          call CTLWRT(ICTLF,IER)
        endif
        ICCC=NCON   ! connection number of the newest surface

C If surface attributes are adaibetic or similar then use these in
C the connections list, then remind user to double check topology.
        altered = .false.
        DO 132 ICC=1,NSUR
          ICCC=ICCC+1
          IC1(ICCC)=IC
          IE1(ICCC)=ICC
          if(SOTHER(icc,1)(1:8).eq.'EXTERIOR')then
            write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &        ' is a EXTERIOR connection which will be used.'
            call edisp(iuout,outs)
            ICT(ICCC)=0
            IC2(ICCC)=0
            IE2(ICCC)=0
          elseif(SOTHER(icc,1)(1:7).eq.'SIMILAR')then
            write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &        ' is a SIMILAR connection which will be used.'
            call edisp(iuout,outs)
            ICT(ICCC)=1
            IC2(ICCC)=0
            IE2(ICCC)=0
          elseif(SOTHER(icc,1)(1:9).eq.'ADIABATIC')then
            ICT(ICCC)=5
            IC2(ICCC)=0
            IE2(ICCC)=0
          elseif(SOTHER(icc,1)(1:8).eq.'CONSTANT')then
            write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &    ' was a CONSTANT connection. Confirm its surface attributes.'
            call edisp(iuout,outs)
            SOTHER(icc,1)='UNKNOWN'
            ICT(ICCC)=-1
            IC2(ICCC)=0
            IE2(ICCC)=0
          elseif(SOTHER(icc,1)(1:6).eq.'GROUND')then
            write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &      ' was a GROUND connection. Confirm its surface attributes.'
            call edisp(iuout,outs)
            SOTHER(icc,1)='UNKNOWN'
            ICT(ICCC)=-1
            IC2(ICCC)=0
            IE2(ICCC)=0
          elseif(SOTHER(icc,1)(1:7).eq.'UNKNOWN')then
            write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &    ' has an undefined boundary. Confirm its surface attributes.'
            call edisp(iuout,outs)
            ICT(ICCC)=-1
            IC2(ICCC)=0
            IE2(ICCC)=0
          elseif(SOTHER(icc,1)(1:7).eq.'ANOTHER')then

C Existing partitions in an imported zone are reset to UNKNOWN.
            write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &        ' was a partition. Confirm its surface attributes.'
            call edisp(iuout,outs)
            SOTHER(icc,1)='UNKNOWN'
            ICT(ICCC)=-1
            IC2(ICCC)=0
            IE2(ICCC)=0
            altered = .true.   ! will need to write out geometry
          else

C Existing partitions in an imported zone are reset to UNKNOWN.
            write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &        ' might be a partition. Confirm its surface attributes.'
            call edisp(iuout,outs)
            SOTHER(icc,1)='UNKNOWN'
            ICT(ICCC)=-1
            IC2(ICCC)=0
            IE2(ICCC)=0
            altered = .true.   ! will need to write out geometry
          endif
	  
C Update the connection based data structures.
          SSNAME(iccc)=SNAME(NCOMP,icc)
          SSOTF(iccc)=SOTF(icc)
          SSMLCN(iccc)=SMLCN(icc)
          SSVFC(iccc)=SVFC(icc)
          SSOTHER(iccc,1)=SOTHER(icc,1)
          SSOTHER(iccc,2)=SOTHER(icc,2)
          SSOTHER(iccc,3)=SOTHER(icc,3)
          SSUSE(iccc,1)=SUSE(icc,1)
          SSUSE(iccc,2)=SUSE(icc,2)
          SSPARENT(iccc)=SPARENT(icc)
          IZSTOCN(ncomp,icc)=iccc

C Find the index of the MLC which matches this surface.
          ssmlcindex(iccc)=0  ! assume no matching MLC          
          do 5 ii=1,nmlc
            if(SSMLCN(iccc)(1:12).eq.DESC(ii)(1:12)) then
              ssmlcindex(iccc)=ii   ! remember MLC index     
            endif
  5       continue
  132   CONTINUE
        NCON=ICCC

C Update the global coordinates for this new zones surfaces so that
C the subsequent wireframe image can be drawn and the bounds of
C the zone can be calculated.
        DO 40 J=1,NZTV(NCOMP)
          VCOORD(NCOMP,J,1)=X(J)
          VCOORD(NCOMP,J,2)=Y(J)
          VCOORD(NCOMP,J,3)=Z(J)
   40   CONTINUE

C If any of the boundary conditions were altered then write out
C the geometry file.
        if(altered)then
          if(newgeo)then
            call geowrite(IFIL+1,LGEOM(ic),ic,iuout,3,IER)
          else
            call emkgeo(IFIL+1,LGEOM(ic),ic,3,IER)
          endif
        endif

C Update the G7 common blocks and then write out configuration file.
        call zgupdate(0,NCOMP,ier)
        CALL EMKCFG('-',IER)
        call usrmsg(
     &  'NOTE: ensure that the model topology is correct by using ',
     &  'the topology facility under [zones definition].','W')

C Check to see if there are other zone files which can be associated
C with this new zone.
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(DFILE,'(A,A4)')zname(IC)(1:lnzname(IC)),'.opr'
        else
          WRITE(DFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      zname(IC)(1:lnzname(IC)),'.opr'
        endif
        dok=.false.
        helptopic='zone_also_schedules'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL ASKOK(' ','Also use an existing operation file?',
     &    OK,dok,nbhelp)
        if(OK)then
          if(LPROJ(IC)(1:4).eq.'UNKN'.or.LPROJ(IC)(1:2).eq.'  ')then

C Find out if there are operation files in the model ../zones folder.
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('?','zon','opr',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','opr',sfile,snpfile,nfile,
     &          iier)
              if(snpfile(1:2).ne.'  ')then
                write(ltmp,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &            snpfile(1:lnblnk(snpfile))

C Debug.
C                write(6,*) ltmp

              else
                LTMP=DFILE
              endif
            else
              LTMP=DFILE
            endif
          else
            LTMP=LPROJ(IC)
          endif
          WRITE(OUTSTR,'(a,i2,a)')' For zone ',IC,
     &      ' the operations are defined in:'
 295      CALL EASKS(LTMP,OUTSTR,' ',72,DFILE,'opr file',IER,nbhelp)
          IF(LTMP(1:2).EQ.'  ')GOTO 295
          call FINDFIL(LTMP,XST)
          IF(.NOT.XST)THEN
            dok=.true.
            CALL ASKOK(' ','Operations file not found, try again?',
     &        OK,dok,nbhelp)
            IF(OK)GOTO 295
          endif
          LPROJ(IC)=LTMP
        else
          LPROJ(IC)='UNKNOWN'
        endif

        dok=.false.
        helptopic='zone_also_constructions'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL ASKOK(' ','Also use an existing constructions file? ',
     &    OK,dok,nbhelp)
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(DFILE,'(A,A4)')zname(IC)(1:lnzname(IC)),'.con'
        else
          WRITE(DFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      zname(IC)(1:lnzname(IC)),'.con'
        endif
        if(OK)then
          if(LTHRM(IC)(1:4).eq.'UNKN'.or.LTHRM(IC)(1:2).eq.'  ')then

C Find out if there are construction files in the model ../zones folder.
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('?','zon','con',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','con',sfile,snpfile,nfile,
     &          iier)
              if(snpfile(1:2).ne.'  ')then
                write(ltmp,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &            snpfile(1:lnblnk(snpfile))

C Debug.
C                write(6,*) ltmp

              else
                LTMP=DFILE
              endif
            else
              LTMP=DFILE
            endif
          else
            LTMP=LTHRM(IC)
          endif
          WRITE(OUTSTR,'(a,i2,a)')' For zone ',IC,
     &      ' the constructions are defined in:'
 297      CALL EASKS(LTMP,OUTSTR,' ',72,DFILE,'constr file',IER,nbhelp)
          IF(LTMP(1:2).EQ.'  ')GOTO 297
          call FINDFIL(LTMP,XST)
          IF(.NOT.XST)THEN
            dok=.true.
            CALL ASKOK(' ','Constructions file not found, try again?',
     &        OK,dok,nbhelp)
            IF(OK)GOTO 297
          endif
          LTHRM(IC)=LTMP
        else
          LTHRM(IC)=DFILE
        endif

C << still to do are asking for any other optional files for this zone.>>
        return

C Load existing CAD.
      elseif(INZOPT.eq.3)then

C Find out if there are CAD (zip) files in ../zones folder.
        sfile=' '
        snpfile=' '
        call edisp(iuout,' ')
        call browsefilelist('?','zon','zip',sfile,snpfile,nfile,iier)
        if(nfile.gt.0)then
          sfile=' '
          snpfile=' '
          call browsefilelist('b','zon','zip',sfile,snpfile,nfile,iier)

          if(snpfile(1:2).ne.'  ')then
            write(ltmp,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &        snpfile(1:lnblnk(snpfile))

C Debug.
C            write(6,*) ltmp

          else
            write(ltmp,'(a)')zonepth(1:lnblnk(zonepth))
          endif
        else
          write(ltmp,'(a)')zonepth(1:lnblnk(zonepth))
        endif

C Existing zip geom file.
        helptopic='zone_also_zip_source'
        call gethelptext(helpinsub,helptopic,nbhelp)
        DFILE=' '
        WRITE(OUTSTR,'(a,i2,a)')'For zone ',IC,
     &    ' the geometry is to be scanned from:'
 396    CALL EASKS(LTMP,' ',OUTSTR,72,DFILE,'zip file',IER,nbhelp)
        IF(LTMP(1:2).EQ.'  ')GOTO 293
        call FINDFIL(LTMP,XST)
        IF(.NOT.XST)THEN
          dok=.true.
          CALL ASKOK('Zip file not found!','Try again?',OK,
     &      dok,nbhelp)
          IF(OK)GOTO 396
        ENDIF

C Scan zip file and update control and connections.
        CALL EZIPIN(IFIL+1,ltmp,IC,1,ITRC,iuout,IER)
        IF(IER.NE.0)THEN
          dok=.true.
          CALL ASKOK('Problem scanning CAD file!',
     &               'Try again?',OK,dok,nbhelp)
          IF(OK)GOTO 396
        ENDIF

      elseif(INZOPT.EQ.4)then
        ier = -3  ! signal user cancel
        return
      endif
      end

C ************* DELZONE
C DELZONE systematicly removes a thermal zone.
      SUBROUTINE DELZONE(ITRC,IWHICH,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "net_flow.h"
#include "esprdbfile.h"
#include "espriou.h"
#include "CFC_common.h"
C espriou.h provides currentfile.
#include "control.h"
#include "prj3dv.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/P2N/NAC(MDTY),IACS(MDTY,MA),IACF(MDTY,MA),ACI(MDTY,MA),
     &ACV(MDTY,MA),IPT(MDTY,MA),TA(MDTY,MA)
      INTEGER NAC,IACS,IACF,IPT
      REAL ACI,ACV,TA
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      COMMON/P2CTL/ITCTL(MCOM),TLO(MCOM),TUP(MCOM),THI(MCOM),ACIL(MCOM),
     &             ACVL(MCOM),IVL(MCOM),TAL(MCOM),ACIU(MCOM),ACVU(MCOM),
     &             IVU(MCOM),TAU(MCOM),ACIH(MCOM),ACVH(MCOM),
     &             IVH(MCOM),TAH(MCOM)

      common/cctlnm/ctldoc,lctlf
      common/cctl/icascf(mcom)
      common/user/browse
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX

      LOGICAL DOK,OK,XST,changed,OKC,OKM,modopr,browse
      logical newgeo  ! to use for testing if new/old geometry file.

      CHARACTER LAPROB*72
      CHARACTER ctldoc*248,LCTLF*72,outs*124
      character ltmp*72,NDNAM*12
      character OUTSTR*124

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='DELZONE'  ! set for subroutine

      if(browse)then
        call usrmsg('Cannot remove a zone while in browse ',
     &              'mode, you must `own` the model! ','W')
        return
      endif
      IW=IWHICH
      IW2=IWHICH
      newgeo=.false.  ! assume older format geometry.

C General warning.
      helptopic='zone_delete_warning'
      call gethelptext(helpinsub,helptopic,nbhelp)
      write(outs,'(3a)')' Continue with deletion of ',zname(IW),'?'
      dok=.false.
      CALL ASKOK(outs,'(see help)',OK,dok,nbhelp)
      if(.NOT.OK)return

C Read in mass flow network if defined and see if there is a node which
C is linked to the zone being deleted.
      call tstamp('>','PRJ: deleting a zone')
      if(IAIRN.ge.1)then

C Temporarily use same file unit as profiles db.
        if(IPRODB.eq.IFIL+6)then
          IUM=IPRODB
        else
          IUM=IFIL+6
        endif
        CALL ERPFREE(IUM,ISTAT)
        CALL MFCDAT

C Read the file header and check for first-line tag. If 4 items
C then an older file so rewind the file and call emfread.
C Otherwise check and see if it is a graphic network file which
C should be scanned and converted into flow common blocks.
        CALL EFOPSEQ(IUM,LAPROB,1,IER)
        CALL STRIPC(IUM,OUTSTR,99,ND,0,'1st line of file',IER)
        if(ier.eq.0)then
          write(currentfile,'(a)') LAPROB(1:lnblnk(LAPROB))
          IF(ND.GE.4)THEN
            REWIND(IUM,ERR=999)
            CALL EMFREAD(IUM,IER)
            CALL ERPFREE(IUM,ISTAT)
          elseif((ND.eq.1.or.ND.eq.2).and.
     &            OUTSTR(1:18).EQ.'* Graphical_network')then

C Found a graphic network file, scan it and then convert it
C into network flow common blocks.
            CALL ERPFREE(IUM,ISTAT)
            call NETREAD(IUM,'R',IER)
            CALL NETTOFLW(ier)
          endif
C          CALL EMFREAD(IUM,IER)
C          CALL ERPFREE(IUM,ISTAT)
          if(ier.ne.0)then
            CALL USRMSG(' ','Problem in flow network!','F')
            OKM=.false.
          else

C Reset strings which hold mass flow links so that subsequent
C write of configuration file will generate proper text.
C If a node is linked to zone to be deleted then check with user.
            if(ICAAS(IW).gt.0)then
              write(outs,'(3A)')zname(IW)(1:lnzname(IW)),
     &          ' is linked to ventilation network node: ',
     &          NDNAM(ICAAS(IW))
              DOK=.true.
              helptopic='zone_delete_net_node'
              call gethelptext(helpinsub,helptopic,nbhelp)
              CALL ASKOK(outs,'Remove this link? (see help)',
     &          OK,DOK,nbhelp)
              if(OK)then
                ICAAS(IW)=0
              endif
            endif

C Any node-zone links to subsequent zones need to be decremented.
            do 246 IX=IW,NCOMP
              if(ICAAS(IX).gt.iw)then
                ICAAS(IX)=ICAAS(IX)-1
              endif
  246       continue
          endif
        endif
      endif

C Read in control file if defined. Use IW2 index from this point.
      helptopic='zone_delete_control'
      call gethelptext(helpinsub,helptopic,nbhelp)
      if(LCTLF(1:2).eq.'  '.or.LCTLF(1:4).eq.'UNKN')then
        DOK=.false.
        CALL ASKOK(' System zone-based controls should also be',
     &   ' updated. Is one associated with the model ?',
     &   OKC,DOK,nbhelp)
      else
        DOK=.true.
        CALL ASKOK(' System zone-based controls should also be',
     &    ' updated. Do this ?',OKC,DOK,nbhelp)
      endif
      if(OKC)then
        ltmp=LCTLF
        CALL EASKS(ltmp,' Control file ? ',
     &   ' ',72,'std.ctl','Control file',IER,nbhelp)
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          lctlf=ltmp
          ICTLF=IFIL+1
          CALL ERPFREE(ICTLF,ISTAT)
          call FINDFIL(LCTLF,XST)
          if(XST)then
            CALL EZCTLR(ICTLF,ITRC,IUOUT,IER)
            if(ncf.gt.0)then

C If one of the control loops references the zone to be deleted warn
C the user and ask if it should be deleted.
              do 77 ik=1,ncf
                if(ibsn(ik,1).eq.IW2.or.iban(ik,1).eq.IW2)then
                  call LSTCNTL(iuout,0,ik)
                  write(outs,'(a,i2,a,a)') 'Control loop ',ik,
     &              ' is specific to ',zname(IW2)
                  DOK=.true.
                  CALL ASKOK(outs,' Remove this control loop?',
     &              OK,DOK,nbhelp)
                  if(ok)then

C User requests deletion of control loop. Do this and then decrement all
C icascf links to subsequent controls.
                    icfoc=0
                    call ADDCNTL(icfoc,'D')
                    do 245 IXI=IW2,NCOMP
                      if(icascf(IXI).gt.ik)then
                        write(outs,'(3A)') ' shifting zone-ctl link ',
     &                    zname(IXI),'...'
                        icascf(IXI)=icascf(IXI)-1
                      endif
  245               continue
                  endif
                endif
  77          continue

C Shift sensor/actuator zone references > IW2 down one.
              do 78 ik=1,ncf
                if(ibsn(ik,1).gt.IW2)then
                  ibsn(ik,1)=ibsn(ik,1)-1
                  write(outs,'(a,i2)') 
     &              'Shifting sensor position in control loop',ik
                  call edisp(iuout,outs)
                endif
                if(iban(ik,1).gt.IW2)then
                  iban(ik,1)=iban(ik,1)-1
                  write(outs,'(a,i2)') 
     &              'Shifting actuator position in control loop',ik
                  call edisp(iuout,outs)
                endif
  78          continue
            endif
            if(ncc.gt.0)then

C If one of the ventilation control loops references the zone to be deleted warn
C the user and ask if it should be deleted.
              do 79 ik=1,ncc
                helptopic='zone_delete_net_control'
                call gethelptext(helpinsub,helptopic,nbhelp)
                if(ifsn(ik,1).eq.IW2.or.ifan(ik,1).eq.IW2)then
                  call LSTCNTL(itru,2,ik)
                  write(outs,'(a,i2,a,a)') 'Ventilation control loop ',
     &              ik,' is specific to ',zname(IW2)
                  DOK=.true.
                  CALL ASKOK(outs,' Remove this control loop?',
     &              OK,DOK,nbhelp)
                  if(ok)then
                    icfoc=2
                    call ADDCNTL(icfoc,'D')
                  endif
                endif
  79          continue

C Shift sensor/actuator zone references > IW2 down one.
              do 80 ik=1,ncc
                if(ifsn(ik,1).gt.IW2)then
                  ifsn(ik,1)=ifsn(ik,1)-1
                  write(outs,'(a,i2)') 
     &              'Shifting sensor position in control loop',ik
                  call edisp(iuout,outs)
                endif
                if(ifan(ik,1).gt.IW2)then
                  ifan(ik,1)=ifan(ik,1)-1
                  write(outs,'(a,i2)') 
     &              'Shifting actuator position in control loop',ik
                  call edisp(iuout,outs)
                endif
  80          continue
            endif
            
            if(nCFCctlloops.gt.0)then

C If one of the control loops references the zone to be deleted warn
C the user and ask if it should be deleted.
              ik = 1
              do while(ik.le.nCFCctlloops)
                helptopic='zone_delete_cfc_control'
                call gethelptext(helpinsub,helptopic,nbhelp)
                if(iCFCsensor(ik,1).eq.IW2.or.                          ! senses zone temp. 
     &          (iCFCsensor(ik,1).eq.-2.and.iCFCsensor(ik,2).eq.IW2)    ! senses mix dbT and MRT
     &          .or.
     &          (iCFCsensor(ik,1).eq.-4.and.iCFCsensor(ik,2).eq.IW2)    ! senses incidient solar rad. on surface
     &          .or.iCFCactuator(ik,2).eq.IW2)then
                  call LSTCNTL(iuout,6,ik)
                  write(outs,'(a,i2,a,a)') 'Control loop ',ik,
     &              ' is specific to ',zname(IW2)
                  DOK=.true.
                  CALL ASKOK(outs,' Remove this control loop?',
     &              OK,DOK,nbhelp)
                  if(ok)then

C User requests deletion of control loop. Do this and then decrement all
C icascf links to subsequent controls.
                    icfoc=6
                    call ADDCNTL(icfoc,'D')
                    do 247 IXI=IW2,NCOMP
                      if(icascf(IXI).gt.ik)then
                        write(outs,'(3A)') ' shifting zone-ctl link ',
     &                    zname(IXI),'...'
                        icascf(IXI)=icascf(IXI)-1
                      endif
  247               continue
                    ik=ik-1 !decrement the control loop counter because control loops have shifted up by 1
                  endif
                endif
                ik = ik +1
              end do

C Shift sensor/actuator zone references > IW2 down one.
              do 82 ik=1,nCFCctlloops
                if(iCFCsensor(ik,1).gt.IW2)then       !senses zone temp.
                  iCFCsensor(ik,1)=iCFCsensor(ik,1)-1
                  write(outs,'(a,i2)') 
     &              'Shifting sensor position in control loop',ik
                  call edisp(iuout,outs)
                elseif(iCFCsensor(ik,1).eq.-2.and.    !senses mix dbT and MRT
     &                 iCFCsensor(ik,2).gt.IW2)then
                  iCFCsensor(ik,2)=iCFCsensor(ik,2)-1
                  write(outs,'(a,i2)') 
     &              'Shifting sensor position in control loop',ik
                  call edisp(iuout,outs)
                elseif(iCFCsensor(ik,1).eq.-4.and.    !senses solar incident rad. on surface
     &                 iCFCsensor(ik,2).gt.IW2)then
                  iCFCsensor(ik,2)=iCFCsensor(ik,2)-1
                  write(outs,'(a,i2)') 
     &              'Shifting sensor position in control loop',ik
                  call edisp(iuout,outs)
                endif
                if(iCFCactuator(ik,2).gt.IW2)then
                  iCFCactuator(ik,2)=iCFCactuator(ik,2)-1
                  write(outs,'(a,i2)') 
     &              'Shifting actuator position in control loop',ik
                  call edisp(iuout,outs)
                endif
  82          continue
            endif

            endif
        endif
      endif
      
C Loop through all other zones and if a surface attribute
C faces the current zone then reset it to UNKNOWN. Also update
C the values of ICT, IE2 and IC2.
      call usrmsg('Unlinking surfaces referencing this zone...',' ','-')
      do 25 IX=1,NCOMP
        if(IX.ne.IW2)then
          changed=.FALSE.
            call georead(IFIL+1,LGEOM(IX),IX,1,iuout,IER)
          do 28 ISX=1,NSUR
            ioc=IZSTOCN(ix,isx)

C If the surface connection type is 3 and it (ic2) points back to IW2
C then reset sother and ict and ie2 and ic2.
            if(ICT(ioc).EQ.3)then
              if(IC2(ioc).eq.IW2)then
                SOTHER(ISX,1)='UNKNOWN'
                SOTHER(ISX,2)='0'
                SOTHER(ISX,3)='0'
                SSOTHER(ioc,1)='UNKNOWN'
                SSOTHER(ioc,2)='0'
                SSOTHER(ioc,3)='0'
                ICT(ioc)=-1
                IE2(ioc)=0
                IC2(ioc)=0
                changed = .true.
              endif
            endif
   28     continue
          if(changed)then
            write(outs,'(A,A)') ' updating topology in ',zname(ix)
            call usrmsg('  ',outs,'-')
            call eclose(gversion(ix),1.1,0.01,newgeo)
            if(igupgrade.eq.2.and.(.NOT.newgeo))then
              gversion(ix) =1.1
              newgeo = .true.
            endif
            if(newgeo)then
              call geowrite(IFIL+2,LGEOM(IX),IX,iuout,3,IER)
            else
              call emkgeo(IFIL+2,LGEOM(IX),IX,3,IER)
            endif
          endif
          call usrmsg('  ','  ','-')
        endif

C Check in operations file that ventilation connections are not
C to a zone that is to be removed.
        IUO=IFIL+1
        call FINDFIL(LPROJ(IX),XST)
        IF(XST)THEN
          CALL ERPFREE(IUO,ISTAT)
          CALL EROPER(0,iuout,IUO,IX,IER)
        ENDIF

C Disconnect lower range alternative ventilation if pointing to focus.
        modopr=.false.
        if(IVL(IX).eq.IW2)then
          modopr=.true.
          IVL(IX)=0
          ACVL(IX)=0.0
        endif

C Disconnect middle range alternative ventilation if pointing to focus.
        if(IVU(IX).eq.IW2)then
          modopr=.true.
          IVU(IX)=0
          ACVU(IX)=0.0
        endif

C Disconnect high range alternative ventilation if pointing to focus.
        if(IVH(IX).eq.IW2)then
          modopr=.true.
          IVH(IX)=0
          ACVH(IX)=0.0
        endif

C Air flows for each day type.
        DO 400 IDTY=1,NBDAYTYPE
          IF(NAC(IDTY).GT.0)THEN
            DO 10 I=1,NAC(IDTY)
              IF(IPT(IDTY,I).EQ.IW2)THEN
                MODOPR=.TRUE.
                IPT(IDTY,I)=0
                ACV(IDTY,I)=0.0
              ENDIF
 10         CONTINUE
          ENDIF
 400    CONTINUE

C Update operations file if any changes.
        if(modopr)then
          call edisp(iuout,' ')
          write(outs,'(1X,A,A)') zname(ix)(1:lnzname(ix)),
     &      ' has inter-zone ventilation with the'
          call edisp(iuout,outs)
          call edisp(iuout,' zone to be deleted.  Links have been')
          call edisp(iuout,' removed and the operation file is now')
          call edisp(iuout,' being updated...')
          CALL EMKOPER(IUO,LPROJ(IX),IX,IER)
        endif
  25  continue

C Reread selected zone data then remove each of the surfaces
C in reverse order (adjusting the connections as required).
      write(outs,'(2A)') ' removing surfaces in ',zname(IW2)
      call usrmsg('  ',outs,'-')
        call georead(IFIL+1,LGEOM(IW2),IW2,1,iuout,IER)
      IX=NSUR+1
  35  CONTINUE
      IX=IX-1
      icb=IZSTOCN(IW2,ix)
      iopt=0
      CALL ADDSUR(ITRC,IW2,IX,'D','A',iopt,IER)

C If there are any anchor points which point to this surface then the list
C must be compacted.
      if(NALOC.gt.0)then
        do 135 ia=1,NALOC
          IRVA=IALOC(ia)
          ifound=0
          do 136 ib=1,IRVA
            if(lstanchr(ia,ib).eq.icb)ifound=ib
 136      continue
          if(ifound.gt.0)then
            IALOC(ia)=IALOC(ia)-1
            do 137 ib=ifound,IALOC(ia)
              lstanchr(ia,ib)=lstanchr(ia,ib+1)
 137        continue
          endif

C Decrement any anchors to subsequent surfaces.
          do 138 ib=1,IALOC(ia)
            if(lstanchr(ia,ib).gt.icb)lstanchr(ia,ib)=lstanchr(ia,ib)-1
 138      continue
 135    continue
      endif
      IF(IX.GT.1)GOTO 35

C Perform connection shift (zone reference) for later zones.
C Also update the other side IC2.
      write(outs,'(A)') ' shifting model topology... '
      call usrmsg('  ',outs,'-')
      do 38 IX=1,NCON
        if(IC1(IX).gt.IW2)IC1(IX)=IC1(IX)-1
        if(ICT(IX).eq.3.and.IC2(IX).gt.IW2)IC2(IX)=IC2(IX)-1
   38 continue

C Re-establish link between zone/surfaces and connections.
      do 335 iccc = 1, NCON
        IZSTOCN(IC1(iccc),IE1(iccc))=iccc
 335  continue

C Ask if user wishes to remove all files related to the zone.
      dok=.false.
      helptopic='zone_delete_files'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL ASKOK(' In addition to de-referencing this zone do you',
     &  ' also wish to delete the descriptive files ?',
     &  OK,dok,nbhelp)
      if(OK)then
        IUO=IFIL+1
        CALL ERPFREE(IUO,ISTAT)
        call FINDFIL(LPROJ(IW2),XST)
        if(XST)then
          CALL EFOPSEQ(IUO,LPROJ(IW2),0,IER)
          CALL EFDELET(IUO,ISTAT)
        endif

        CALL ERPFREE(IUO,ISTAT)
        call FINDFIL(LGEOM(IW2),XST)
        if(XST)then
          CALL EFOPSEQ(IUO,LGEOM(IW2),1,IER)
          if(ier.eq.0)CALL EFDELET(IUO,ISTAT)
        endif

        CALL ERPFREE(IUO,ISTAT)
        call FINDFIL(LTHRM(IW2),XST)
        if(XST)then
          CALL EFOPSEQ(IUO,LTHRM(IW2),1,IER)
          if(ier.eq.0)CALL EFDELET(IUO,ISTAT)
        endif
        if(IVF(IW2).eq.1)then
          CALL ERPFREE(IUO,ISTAT)
          CALL EFOPSEQ(IUO,LVIEW(IW2),1,IER)
          if(ier.eq.0)CALL EFDELET(IUO,ISTAT)
        endif
        if(IHC(IW2).eq.1)then
          CALL ERPFREE(IUO,ISTAT)
          CALL EFOPSEQ(IUO,LHCCO(IW2),1,IER)
          if(ier.eq.0)CALL EFDELET(IUO,ISTAT)
        endif
        if(ITW(IW2).eq.1)then
          CALL ERPFREE(IUO,ISTAT)
          CALL EFOPSEQ(IUO,LTWIN(IW2),1,IER)
          if(ier.eq.0)CALL EFDELET(IUO,ISTAT)
        endif
        if(icfc(IW2).eq.1)then
          CALL ERPFREE(IUO,ISTAT)
          CALL EFOPSEQ(IUO,lcfcin(IW2),1,IER)
          if(ier.eq.0)CALL EFDELET(IUO,ISTAT)
        endif
        if(ICGC(IW2).eq.1)then
          CALL ERPFREE(IUO,ISTAT)
          CALL EFOPSEQ(IUO,LCGCIN(IW2),1,IER)
          if(ier.eq.0)CALL EFDELET(IUO,ISTAT)
        endif
      endif

C For next zones shift surface names.
      if(IW2.eq.NCOMP)goto 46
      do 45 IX=IW2,NCOMP-1
        write(outs,'(3A)') ' shifting zone ',zname(IX),'....'
        call usrmsg('  ',outs,'-')
        DO 66 IS=1,NZSUR(IX+1)
          SNAME(IX,IS)=SNAME(IX+1,IS)
          ITMCFL(IX,IS)=ITMCFL(IX+1,IS)
          icfcfl(IX,IS)=icfcfl(IX+1,IS)
   66   continue

C For each zone `above' the selected one reset the various file names
C and zone based data.
        NZSUR(IX)=NZSUR(IX+1)
        NZTV(IX)=NZTV(IX+1)
        NCCODE(IX)=NCCODE(IX+1)-1
        zname(IX)=zname(IX+1)
        lnzname(IX)=lnzname(IX+1)
        zdesc(IX)=zdesc(IX+1)
        lnzdesc(IX)=lnzdesc(IX+1)
        LPROJ(IX)=LPROJ(IX+1)
        LGEOM(IX)=LGEOM(IX+1)
        LTHRM(IX)=LTHRM(IX+1)
        LSHAD(IX)=LSHAD(IX+1)
        LVIEW(IX)=LVIEW(IX+1)
        LHCCO(IX)=LHCCO(IX+1)
        LTWIN(IX)=LTWIN(IX+1)
        lcfcin(IX)=lcfcin(IX+1)
        LCGCIN(IX)=LCGCIN(IX+1)
        ZOBS(IX)=ZOBS(IX+1)
        IVF(IX)=IVF(IX+1)
        ISI(IX)=ISI(IX+1)
        IHC(IX)=IHC(IX+1)
        ITW(IX)=ITW(IX+1)
        icfc(IX)=icfc(IX+1)
        ICGC(IX)=ICGC(IX+1)
        IOBS(IX)=IOBS(IX+1)
        icascf(IX)=icascf(IX+1)
        ICAAS(IX)=ICAAS(IX+1)

C Shift viewing bounds.
        ZXMN(IX)=ZXMN(IX+1)
        ZYMN(IX)=ZYMN(IX+1)
        ZZMN(IX)=ZZMN(IX+1)
        ZXMX(IX)=ZXMX(IX+1)
        ZYMX(IX)=ZYMX(IX+1)
        ZZMX(IX)=ZZMX(IX+1)
        iZBFLG(IX)=iZBFLG(IX+1)
        ZCOG(IX,1)=ZCOG(IX+1,1)
        ZCOG(IX,2)=ZCOG(IX+1,2)
        ZCOG(IX,3)=ZCOG(IX+1,3)

C Shift zone base surfaces and base areas.
        ZBASEA(IX)=ZBASEA(IX+1)
        IBASES(IX,1)=IBASES(IX+1,1)
        IBASES(IX,2)=IBASES(IX+1,2)
        IBASES(IX,3)=IBASES(IX+1,3)
        IBASES(IX,4)=IBASES(IX+1,4)
        IBASES(IX,5)=IBASES(IX+1,5)
        IBASES(IX,6)=IBASES(IX+1,6)

C Shift zone default solar distributions and volumes.
        NDP(IX)=NDP(IX+1)
        IDPN(IX,1)=IDPN(IX+1,1)
        IDPN(IX,2)=IDPN(IX+1,2)
        IDPN(IX,3)=IDPN(IX+1,3)
        VOL(IX)=VOL(IX+1)

C Shift the TMC information in subsequent zones??
C << should code just deal with common block >>

   45 continue
   46 NCOMP=NCOMP-1

C Update common blocks for Geometry, operation & CFC
C commons via reading in related files.
      write(outs,'(A)') ' rebuilding model... '
      call usrmsg('  ',outs,'-')
      do 48 ir=1,NCOMP
          call georead(IFIL+1,LGEOM(ir),ir,1,IUOUT,IER)
        XST=.false.
        call FINDFIL(LPROJ(ir),XST)
        IF(XST)THEN
          CALL ERPFREE(IFIL+1,ISTAT)
          CALL EROPER(0,iuout,IFIL+1,ir,IER)
        ENDIF
        XST=.false.
        call FINDFIL(LTHRM(ir),XST)
        IF(XST)THEN
          CALL ERPFREE(IFIL+1,ISTAT)
          CALL ECONST(LTHRM(ir),IFIL+1,ir,0,IUOUT,IER)
        ENDIF
        XST=.false.
        call FINDFIL(LTWIN(ir),XST)
        IF(XST)THEN
          CALL ERPFREE(IFIL+1,ISTAT)
          CALL ERTWIN(0,IUOUT,IFIL+1,LTWIN(ir),ir,IER)
        ENDIF
        XST=.false.
        call FINDFIL(lcfcin(ir),XST)
        IF(XST)THEN
          CALL ERPFREE(IFIL+1,ISTAT)
          CALL read_in_cfc_file(0,IUOUT,IFIL+1,lcfcin(ir),ir,IER)
        ENDIF        
  48  continue
      call usrmsg('  ','  ','-')

C Warn user about updating the mass flow.
      if(IAIRN.ge.1.and.OKM)then
        helptopic='zone_net_delete_warning'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('mfs update popup',nbhelp,'-',0,0,IER)
      endif

C Write out control file to update control/zone list.
      if(OKC)then
        helptopic='zone_ctl_delete_warning'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('control update popup',nbhelp,'-',0,0,IER)
        ICTLF=IFIL+1
        CALL CTLWRT(ICTLF,IER)
      endif

      helptopic='zone_delete_update_cfg'
      call gethelptext(helpinsub,helptopic,nbhelp)
      ltmp=LCFGF
 49   CALL EASKS(ltmp,' Updated configuration file? ',
     &   ' ',72,'revised.cfg','updated config file',IER,nbhelp)
      if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
        LCFGF=ltmp
      else
        goto 49
      endif
      CALL EMKCFG('-',IER)
      return

C File rewind errors.
  999 CALL USRMSG('Error rewinding flow network file:',
     &  LAPROB,'W')
      IER=1
      return 
      end

C ************* WARNMOD 
C WARNMOD provides warning of updates needed when model is changed.
C Initially deals with change in number of surfaces. 
C Act is action to be tested. act = 'sf+' or 'sf-' surface added:deleted.
C act = 'sat' surface attribute. act = 'str' surf transform.
C act = 'ob+' or 'ob-' obstruction changes.
C IER=0 OK. 
      SUBROUTINE WARNMOD(ICOMP,act)
#include "building.h"
#include "model.h"
#include "CFC_common.h"
      common/pmchange/comold,tmcold,vwfold,ishold,cfcold

      CHARACTER act*3

      LOGICAL XST,comold,tmcold,vwfold,ishold,cfcold

      if(act(1:3).eq.'sf-'.or.act(1:3).eq.'sf+'.or.
     &   act(1:3).eq.'sat')then
        call FINDFIL(LTHRM(ICOMP),XST)
        if(XST)then
          comold = .true.
        else
          comold = .false.
        endif
      endif

C Check for implications of surface addition or removal or attribution change.
C << refine testing for setting tmcold >>
      if(act(1:3).eq.'sf-'.or.act(1:3).eq.'sf+'.or.
     &   act(1:3).eq.'sat')then
        if(ITW(ICOMP).eq.1)then
          call FINDFIL(LTWIN(ICOMP),XST)
          if(XST)then
            tmcold = .true.
          else
            tmcold = .false.
          endif
        endif
      endif
      if(act(1:3).eq.'sf-'.or.act(1:3).eq.'sf+'.or.
     &   act(1:3).eq.'sat')then
        if(icfc(ICOMP).eq.1)then
          call FINDFIL(lcfcin(ICOMP),XST)
          if(XST)cfcold = .true.
        endif
      endif
      if(act(1:3).eq.'sf-'.or.act(1:3).eq.'sf+'.or.
     &   act(1:3).eq.'str')then
        if(IVF(ICOMP).eq.1)then
          call FINDFIL(LVIEW(ICOMP),XST)
          if(XST)then
            vwfold = .true.
          else
            vwfold = .false.
          endif
        endif
      endif
      if(act(1:3).eq.'sf-'.or.act(1:3).eq.'sf+'.or.
     &   act(1:3).eq.'str')then
        if(ISI(ICOMP).eq.1)then
          call FINDFIL(LSHAD(ICOMP),XST)
          if(XST)then
            ishold = .true.
          else
            ishold = .false.
          endif
        endif
      endif
      if(act(1:3).eq.'ob-'.or.act(1:3).eq.'ob+')then
        if(ISI(ICOMP).eq.1)then
          call FINDFIL(LSHAD(ICOMP),XST)
          if(XST)then
            ishold = .true.
          else
            ishold = .false.
          endif
        endif
      endif

      RETURN
      END

C ************* SUMRCHG 
C SUMRCHG provides a summary of updates needed after model changes.
C Indications are collected from previous calls to WARNMOD. SUMRCHG
C should be called with act = 'i' (initialise) when entering a zone and
C with act = 'r' (report) when leaving the zone.
C IER=0 OK. 

C Note: this subroutine uses explicit pophelp common block because
C the help message will be built up dynamically based on the current
C contents of the model.
      SUBROUTINE SUMRCHG(ICOMP,act,silent)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "CFC_common.h"
      
      integer lnblnk  ! function definition

C Passed parameters
      integer icomp   ! index of the current zone
      character act*1 ! i is initialise r is report on leaving
      logical silent  ! if true then use silent recalc

      COMMON/OUTIN/IUOUT,IUIN
      character h*72
      common/pophelp/h(60)
      common/pmchange/comold,tmcold,vwfold,ishold,cfcold

      logical comold,tmcold,vwfold,ishold,ok,dok,QUIET
      logical unixok,cfcold

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='SUMRCHG'  ! set for subroutine

C Check if Unix-based or DOS based.
      call isunix(unixok)

      nh = 0
      if(act(1:1).eq.'i'.or.act(1:1).eq.'I')then
        comold = .false.
        tmcold = .false.
        vwfold = .false.
        ishold = .false.
        cfcold = .false.
        return
      elseif(act(1:1).eq.'r'.or.act(1:1).eq.'R')then
        if(comold.or.tmcold.or.vwfold.or.ishold.or.cfcold)then
          nh=nh+1
          write(h(nh),'(3a)') 'For ',
     &    zname(icomp)(1:lnzname(icomp)),' the last action makes:'
        endif
        if(comold)then
          nh=nh+1
          write(h(nh),'(3a)')' the construction file ',
     &      LTHRM(ICOMP)(1:lnblnk(LTHRM(ICOMP))),' out of date.'
        endif
        if(tmcold)then
          nh=nh+1
          write(h(nh),'(3a)')' the TMC file ',
     &          LTWIN(ICOMP)(1:lnblnk(LTWIN(ICOMP))),' out of date.'
        endif
        if(cfcold)then
          nh=nh+1
          write(h(nh),'(3a)')' the CFC file ',
     &          lcfcin(ICOMP)(1:lnblnk(lcfcin(ICOMP))),' out of date.'
        endif
        if(vwfold)then
          nh=nh+1
          write(h(nh),'(3a)')' the view factor file ',
     &          LVIEW(ICOMP)(1:lnblnk(LVIEW(ICOMP))),' out of date.'
          nh=nh+1
          h(nh)=' (recalculate via `Options viewfactors`)'
        endif
        if(ishold)then
          nh=nh+1
          write(h(nh),'(3a)')' the shading db ',
     &        LSHAD(ICOMP)(1:lnblnk(LSHAD(ICOMP))),' out of date.'
        endif
        if(nh.gt.1)then
	  if(.NOT.silent)then
            CALL PHELPD('files out of date',nh,'-',0,0,IER)
	  endif  
        endif
      endif
      if(ishold)then
        if(silent)then
	  IRT=1
	else
          DOK=.true.
          helptopic='change_for_shading'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKABC(' Reflect zone change in shading options:',' ',
     &   'recalculate (silent)','recalculate (interactive)','continue',
     &    IRT,nbhelp)
        endif
        if(IRT.eq.1.or.IRT.eq.2)then
          if(IRT.eq.1) call edisp(iuout,
     & 'When finished control will be returned to the project manager.')

C If existing db out of date the perform recalculation.
          call tstamp('>','PRJ: start shading recalculation')
          if(IRT.eq.1)then
            call comissionish(icomp,'sra',ier)
            ishold=.false.
          else
            call comissionish(icomp,'ira',ier)
            ishold=.false.
          endif
        endif
      endif
      if(comold.or.tmcold.or.cfcold)then
        if(silent)then
	  ok= .true.
	else
          DOK=.true.
          helptopic='change_for_constructions'
          call gethelptext(helpinsub,helptopic,nbhelp)
        CALL ASKOK(
     &    'Update the zone construction and/ or TMC and/ or ',
     &    'CFC files to reflect the changes that you have made?',
     &     OK,DOK,nbhelp)
        endif
        if(OK)then
          QUIET=.true.
          itru=iuout
          CALL EDCON(0,itru,ICOMP,QUIET,IER)
          QUIET=.false.
        endif
      endif

      RETURN
      END

C******* comissionish
C comissionish invokes the shading module in a number of modes
C act = 'sr '  silent recalculation (existing files)
C     = 'sra'  silent recalculation (of all files)
C     = 'ir '  interactive recalculation (existing files)
C     = 'ira'  interactive recalculation (of all files)
C     = 'in '  interactive new
      subroutine comissionish(iz,act,ier)
#include "building.h"
#include "model.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/appw/iappw,iappx,iappy

      CHARACTER ZN*12,longtfile*144,longtfiledos*144
      character TMODE*8,outs*248,DOIT*248
      character act*3  ! the parameter passed
      logical XST,concat,unixok
#ifdef OSI
      integer ICPMOD     ! passed from tchild
#else
      integer*8 ICPMOD     ! passed from tchild
#endif

C Check if Unix-based or DOS based.
      call isunix(unixok)

      if(iz.eq.0.or.iz.gt.ncomp)then
        ier=1
        return  ! do not do if iz is zero
      endif

      if(ISI(iz).eq.0)then
        ier=2
        return  ! do not do if ISI has not been setup yet
      endif

C Check if file exists and get local zone name for feedback.
      call FINDFIL(LSHAD(iz),XST)
      write(ZN,'(A)') zname(iz)
      LNZ=max(1,lnzname(iz))  ! find current lenght of at least on character

C Clear command and setup terminal type.
      doit = ' '
      call tchild(ICPMOD)
      call termode(ICPMOD,tmode)

C Setup file name depending on the machine type.
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

      if(act(1:3).eq.'sr ')then

C Silent recalculate (try not to ask the user anything if there
C is an existing shading file but if there is no exisitng file
C set ier=3 and return to calling code.
        if(XST)then
          write(doit,'(5a)') 'ish -mode text -file ',
     &      longtfile(1:lnblnk(longtfile)),' -zone ',
     &      ZN(1:lnz),' -act update_silent'
          call usrmsg('starting shading analysis via',doit,'-')

C Since silent, tell runit to use the current command window
C rather than starting up a new xterm.
          call runit(doit,'-')
          write(outs,'(3a)') 'Shading for ',ZN(1:lnz),
     &        ' has been recalculated.'
          call edisp(iuout,outs)
        else
          ier=3
          return  ! if shading file does exist
        endif

      elseif(act(1:3).eq.'sra')then

C Silent recalculate whether or not there is an existing shading
C file.
        write(doit,'(5a)') 'ish -mode text -file ',
     &    longtfile(1:lnblnk(longtfile)),' -zone ',
     &    ZN(1:lnz),' -act update_silent'
        call usrmsg('starting shading analysis via',doit,'-')

C Since silent, tell runit to use the current command window
C rather than starting up a new xterm.
        call runit(doit,'-')
        write(outs,'(3a)') 'Shading for ',ZN(1:lnz),
     &      ' has been recalculated.'
        call edisp(iuout,outs)

      elseif(act(1:3).eq.'ir ')then

C Interactive recalculate of zone shading if there is an existing
C shading file. If prj initial size is a % of default pass this\
C on to child with an offset from prj start position.
        if(XST)then
          if(iappw.gt.0.and.iappw.le.200)then
            write(doit,'(3a,3i4,5a)') 'ish -mode ',
     &          tmode,' -s ',iappw,iappx+15,iappy+50,' -file ',
     &          longtfile(1:lnblnk(longtfile)),' -zone ',
     &          ZN(1:lnz),' -act recalculate'
          else
            write(doit,'(7a)') 'ish -mode ',tmode,
     &         ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),
     &         ' -zone ',ZN(1:lnz),' -act recalculate'
          endif
          call usrmsg('starting shading analysis via',doit,'-')
          call runit(doit,tmode)
          write(outs,'(3a)') 'Shading for ',ZN(1:lnz),
     &      ' has been recalculated.'
          call edisp(iuout,outs)
        else
          ier=3
          return  ! if shading file does exist
        endif
      elseif(act(1:3).eq.'ira')then

C Interactive recalculate of zone shading without checking if
C a shading file exists. If prj initial size is a % of default
C pass this on to child with an offset from prj start position.
        if(iappw.gt.0.and.iappw.le.200)then
          write(doit,'(3a,3i4,5a)') 'ish -mode ',
     &        tmode,' -s ',iappw,iappx+15,iappy+50,' -file ',
     &        longtfile(1:lnblnk(longtfile)),' -zone ',
     &        ZN(1:lnz),' -act recalculate'
        else
          write(doit,'(7a)') 'ish -mode ',tmode,
     &       ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),
     &       ' -zone ',ZN(1:lnz),' -act recalculate'
        endif
        call usrmsg('starting shading analysis via',doit,'-')
        call runit(doit,tmode)
        write(outs,'(3a)') 'Shading for ',ZN(1:lnz),
     &    ' has been recalculated.'
        call edisp(iuout,outs)

      elseif(act(1:3).eq.'in ')then

C Interactive shading calculation for a new shading database.
        if(iappw.gt.0.and.iappw.le.200)then
          write(doit,'(3a,3i4,4a)') 'ish -mode ',tmode,
     &      ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &      longtfile(:lnblnk(longtfile)),' -zone ',
     &      ZN(1:lnblnk(ZN))
        else
          write(doit,'(6a)') 'ish -mode ',tmode,
     &      ' -s 0 0 0 -file ',longtfile(:lnblnk(longtfile)),
     &      ' -zone ',ZN(1:lnblnk(ZN))
        endif
        call usrmsg('starting shading analysis via',doit,'-')
        call runit(doit,tmode)
        write(outs,'(3a)') 'Shading for ',ZN(1:lnz),
     &    ' has been recalculated.'
        call edisp(iuout,outs)
       
      endif

      return
      end

C POINTTOLINE: determines distance from a 3D point to a 3D line.
C where ipoint is the index of the test vertex, iwhich1 is the index
C of the vertex at the start of the line, iwhich2 is the index of the
C index at the end of the line, offset is the distance (m), match is
C a logical set to true if close enough.
C Only returns match=true if point was found along the line between
C the two vertices (i.e. it discards matches beyond the end points.
C It assumes that calling code will decide whether the distance
C can be used. 
      subroutine pointtoline(ipoint,iwhich1,iwhich2,offset,match)
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
      vd(1)= X(IWHICH2)-X(IWHICH1)
      vd(2)= Y(IWHICH2)-Y(IWHICH1)
      vd(3)= Z(IWHICH2)-Z(IWHICH1)
      call dot3(vd,vd,vdis)
      vd1(1)= X(IWHICH3)-X(IWHICH1)
      vd1(2)= Y(IWHICH3)-Y(IWHICH1)
      vd1(3)= Z(IWHICH3)-Z(IWHICH1)
      call dot3(vd1,vd1,vdis1)
      vd2(1)= X(IWHICH3)-X(IWHICH2)
      vd2(2)= Y(IWHICH3)-Y(IWHICH2)
      vd2(3)= Z(IWHICH3)-Z(IWHICH2)
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
        d2l=(vdis1-(vdis+vdis1-vdis2)*
     &      (vdis+vdis1-vdis2)/vdis/4.0)
        if(abs(d2l).lt.0.003)then
          offset=d2l
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


C TWODPOINTTOLINE: determines distance from a 2D point to the 2D lines
C associated with the current zone wireframe. Where:
C itx & ity are the test pixel location,
C icomp is the zone to check within and ioffset is the number of pixels
C  of tolerance allowed.
C match is a logical signaling that something was found
C ifsurf1 & ifsurf2 are surfaces associated with the line (both zero 
C   if nothing found) and
C ifvrt1 and ifvert2 are nearest two vertices on the edge (zero if
C   nothing found).
C      subroutine twodpointtoline(itx,ity,icomp,ioffset,ifsurf1,ifsurf2,
C     &  ifvrt1,ifvrt2,match)
C#include "building.h"
C#include "geometry.h"

C << logic needs to be written >>

C      dimension vd(3),vd1(3),vd2(3)
C      logical match

C If any of the indices is zero then return with match=false.
C      match=.false.
C      iwhich3=ipoint
C      if(iwhich1.eq.0.or.iwhich2.eq.0.or.iwhich3.eq.0)then
C        match=.false.
C        return
C      endif

C Report length of line. Use method of Ward/Radiance in fvect.c
C      vd(1)= X(IWHICH2)-X(IWHICH1)
C      vd(2)= Y(IWHICH2)-Y(IWHICH1)
C      vd(3)= Z(IWHICH2)-Z(IWHICH1)
C      call dot3(vd,vd,vdis)
C      vd1(1)= X(IWHICH3)-X(IWHICH1)
C      vd1(2)= Y(IWHICH3)-Y(IWHICH1)
C      vd1(3)= Z(IWHICH3)-Z(IWHICH1)
C      call dot3(vd1,vd1,vdis1)
C      vd2(1)= X(IWHICH3)-X(IWHICH2)
C      vd2(2)= Y(IWHICH3)-Y(IWHICH2)
C      vd2(3)= Z(IWHICH3)-Z(IWHICH2)
C      call dot3(vd2,vd2,vdis2)
C      if(vdis2.gt.vdis1)then
C        if((vdis2 - vdis1).gt.vdis)then
C          match=.false.
C          offset=0.0
C          return
C        endif
C      else
C        if((vdis1 - vdis2).gt.vdis)then
C          match=.false.
C          offset=0.0
C          return
C        endif
C      endif
C      d2l=(vdis1-(vdis+vdis1-vdis2)*
C     &    (vdis+vdis1-vdis2)/vdis/4.0)
C      if(abs(d2l).lt.0.003)then
C        offset=d2l
C      else
C        offset=SQRT(d2l)
C      endif
C      match=.true.
C      return
C      end
