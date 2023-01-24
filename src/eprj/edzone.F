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


C The file edzone.f is a collection of support facilities for 
C creating zones and adding/ subtracting vertices and surfaces:
C  SCRZONE: creates initial form of an enclosure as prism or extrusion.
C  NEWZONE: controls creation of a new zone via prj or cad or file.
C  DELZONE: systematically removes a thermal zone.
C  COPYZONE: systematically copies one or more thermal zones.
C  WARNMOD: provide warnings when model evolves.
C  SUMRCHG: provides a summary of updates needed after model changes.
C comissionish: invokes the shading module in a number of modes
C  POINTTOLINE: determines distance from a 3D point to a 3D line.


C ******************** SCRZONE ********************
C Provides initial form of an enclosure as either a prism or
C an extrusion.

      SUBROUTINE SCRZONE(ICOMP,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "prj3dv.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/user/browse
      common/cctlnm/ctldoc,lctlf
      integer icascf
      common/cctl/icascf(mcom)

      DIMENSION XX(MS),YY(MS)
      CHARACTER HOLD*32
      character MODE*4,fs*1
      character msg*72
      CHARACTER OUTSTR*124,ZN*12,outs*124
      character ctldoc*248,LCTLF*72,zd*64,DFILE*72,CFILE*72,OFILE*72
      LOGICAL OK,browse,OKC,XST,unixok
      logical newgeo  ! to use for testing if new/old geometry file.
      real DX1,DY1,DZ1  ! to avoid name clash with geometry.h
      real XO1,YO1,ZO1
      real ANGR,ANGE    ! rotation angles

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.

      helpinsub='edzone'  ! set for subroutine

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
 43   CALL EASKS(zd,' ','What does it represent?',
     &     64,'no description entered','zone descr',IER,nbhelp)
      IF(zd.eq.' '.or.ier.ne.0)goto 43
      zdesc(ICOMP)=zd
      lnzdesc(ICOMP)=lnblnk(zd)  ! update the length of this string.

C Allow user choices of beginning as a simple shape (with image feedback). 
      IER=0
      helptopic='zone_initial_shapes'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKABCD(' ','Zone shape options:',
     &  'rectangular plan','polygon plan','general 3D',
     &  'bitmap',ishape,nbhelp)
 144  IF(ishape.eq.1)THEN
        XO1=0.0; YO1=0.0; ZO1=0.0
        DX1=5.0; DY1=3.0; DZ1=2.7
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
        CALL EASKS(HOLD,' ','Length, width and height?',
     &    32,' 2.5 3.5 2.7 ','rectangle size',IER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,DX1,0.001,99.9,'W','box width',IER)
        CALL EGETWR(HOLD,K,DY1,0.001,99.9,'W','box depth',IER)
        CALL EGETWR(HOLD,K,DZ1,0.001,99.9,'W','box height',IER)
        if(DX1.lt.0.001.or.DY1.lt.0.001)goto 242
        write(outs,'(a,3f9.4)') 'Width depth height is ',DX1,DY1,DZ1
        call edisp(iuout,outs)

        ANGR=0.
        CALL EASKR(ANGR,' ','Orientation?',
     &    -359.0,'W',359.0,'W',0.0,'box orientation',IER,nbhelp)

        ANGE=0.
        CALL EASKR(ANGE,' ','Elevation?',
     &    -359.0,'W',359.0,'W',0.0,'box elevation',IER,nbhelp)

C Convert box into a gen description using two rotation angles.
C Pass in critical dimensions and expect the data back via common G1.
        CALL ERECC3A(XO1,YO1,ZO1,DX1,DY1,DZ1,ANGR,ANGE,0.0)
        NSUR=6
        IUZBASEA(icomp)=0
        IBASES(ICOMP,1)=6
        MODBND=.TRUE.; MODIFYVIEW=.TRUE.; MODLEN=.TRUE.
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
          WRITE(OUTSTR,'(A,I2,A)')'For base vertex',IW,' X & Y:'
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
        CALL EASKOK(' ','Accept base vertices?',OK,nbhelp)
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
        MODBND=.TRUE.; MODIFYVIEW=.TRUE.; MODLEN=.TRUE.
        iZBFLG(ICOMP)=0
      ENDIF

C Begin with default assumptions for each surface then overwrite
C this if user supplied information exists.
      IF(ishape.ne.3.and.ishape.ne.4)THEN

C Use subroutine filsur to instantiate G5 common block.
        CALL FILSUR(ICOMP,0)

C Update the connection list and hash.
        ICCC=NCON
        DO 32 ICC=1,NSUR
          ICCC=ICCC+1
          IC1(ICCC)=ICOMP; IE1(ICCC)=ICC; ICT(ICCC)=-1   ! mark as unknown at this time.
          IC2(ICCC)=0; IE2(ICCC)=0
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
        X(1)=X0+2.0; Y(1)=Y0; Z(1)=Z0
        X(2)=X0; Y(2)=Y0; Z(2)=Z0
        X(3)=X0; Y(3)=Y0+2.0; Z(3)=Z0
        X(4)=X0+2.0; Y(4)=Y0+2.0; Z(4)=Z0
        NVER(1)=4
        JVN(1,1)=1; JVN(1,2)=2; JVN(1,3)=3; JVN(1,4)=4
        NZTV(icomp)=NTV
        SNAME(ICOMP,1)='first'
        SOTF(ICOMP,1)='OPAQUE'; SMLCN(ICOMP,1)='UNKN' 
        SVFC(ICOMP,1)='FLOR'
        SOTHER(ICOMP,1,1)='UNKNOWN'; SOTHER(ICOMP,1,2)='0'
        SOTHER(ICOMP,1,3)='0'
        SUSE(ICOMP,1,1)='-'; SUSE(ICOMP,1,2)='-' 
        SPARENT(ICOMP,1)='-'
        IUZBASEA(icomp)=0; IBASES(ICOMP,1)=1

C Update the connection list and hash for initial surface.
        ICCC=NCON
        ICCC=ICCC+1
        IC1(ICCC)=ICOMP; IE1(ICCC)=1; ICT(ICCC)=-1  ! mark as unknown
        IC2(ICCC)=0; IE2(ICCC)=0
        IZSTOCN(icomp,1)=iccc
        NCON=ICCC
        MODBND=.TRUE.; MODIFYVIEW=.TRUE.; MODLEN=.TRUE.
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
            call usrmsg('The clickonbitmap facility is not active',
     &      'with new graphic library.','W')
          endif
        else
          call usrmsg('Must be in graphic mode to use click on bitmap',
     &      'facility to define vertices or floor plan corners.','W')
          return
        endif
      endif

C High level info for all cases except clickonbitmap.
      IF(zname(ICOMP)(1:2).EQ.'  ')THEN
        IF(ICOMP.LE.9)WRITE(zname(ICOMP),'(A5,I1)')'Zone-',ICOMP
        IF(ICOMP.GT.9)WRITE(zname(ICOMP),'(A5,I2)')'Zone-',ICOMP
        IF(ICOMP.GT.99)WRITE(zname(ICOMP),'(A5,I3)')'Zone-',ICOMP
        lnzname(ICOMP)=lnblnk(zname(ICOMP))  ! update length for this string.
      ENDIF
      if(zdesc(ICOMP)(1:2).EQ.'  ')then
        write(zdesc(ICOMP),'(2a)') zname(ICOMP)(1:lnzname(ICOMP)),
     &    ' describes a '
        lnzdesc(ICOMP)=lnblnk(zdesc(ICOMP))  ! update the length of this string.
      endif
      CTYPE(icomp)='GEN '
      NDP(ICOMP)=3
      IDPN(ICOMP,1)=0; IDPN(ICOMP,2)=0; IDPN(ICOMP,3)=0
      NZSUR(ICOMP)=NSUR  ! update nzsur() it is needed by zgupdate.
      NZTV(ICOMP)=NTV

C Update the G7 common blocks and then assign ZBASEA.
      call zgupdate(1,ICOMP,ier)

C Find co-planer surfaces and edges of similar materials.
      call suredgeadj(itrc,'-',icomp,ier)
      if(ishape.eq.1)then
        ZBASEA(icomp)= SNA(icomp,IBASES(ICOMP,1))
      elseif(ishape.eq.2)then
        ZBASEA(icomp)= SNA(icomp,IBASES(ICOMP,1))
      elseif(ishape.eq.3)then
        ZBASEA(icomp)= SNA(icomp,IBASES(ICOMP,1))
      endif

C Update the global coordinates for this zones surfaces so that
C the subsequent wireframe image can be drawn and the bounds of
C the zone can be calculated. Also instantiate iszjvn
C for the new zone (as is done in subroutine zdata).
      DO 40 J=1,NZTV(ICOMP)
        szcoords(ICOMP,J,1)=X(J)
        szcoords(ICOMP,J,2)=Y(J)
        szcoords(ICOMP,J,3)=Z(J)
   40 CONTINUE
      DO 50 J=1,nzsur(ICOMP)
        icc=IZSTOCN(icomp,j)
        if(icc.ne.0)then
          isznver(icomp,j)=NVER(J)
          N = isznver(icomp,j)
          DO 60 K=1,N
            iszjvn(icomp,j,K)=JVN(J,K)
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

        helptopic='zone_initial_write'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKOK('Problem creating geometry file!','Retry?',
     &              OK,nbhelp)
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
        call usrmsg(' ','Updating control for additional zone.','-')
        call CTLWRT(ICTLF,IER)
        call usrmsg(' ',' ','-')
      endif

      CALL EMKCFG('-',IER)

      RETURN
      END

C ******************* NEWZONE *******************
C Controls the creation of a new zone. 

      SUBROUTINE NEWZONE(ITRC,IC,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "predefined.h"
#include "prj3dv.h"
#include "esprdbfile.h"
#include "material.h"
#include "help.h"
      
      integer lnblnk  ! function definition
      integer itrc    ! trace level
      integer ic      ! index of zone to create
      integer ier     ! non-zero return is error

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/cctlnm/ctldoc,lctlf
      integer icascf
      common/cctl/icascf(mcom)

      CHARACTER LTMP*72
      CHARACTER OUTSTR*124,ctldoc*248,LCTLF*72
      CHARACTER DFILE*72,CFILE*72,OFILE*72
      character sfile*72,snpfile*72,fs*1,outs*124

      LOGICAL OK,XST,OKC,unixok
      logical newgeo    ! Test for new/old geometry file.
      logical altered   ! Signal change in boundary conditions of altered zone.

      real CX,CY,CZ     ! XYZ of selected verticies from another zone
      dimension CX(10),CY(10),CZ(10)
      dimension IVALS(MCOM)
      integer  iclkok   ! for import vertex from another zone dialog

C Predefined entity variables.
      character name*12   ! the object name to pass to PREDEFEMBED
      character prec*1    ! character to pre-pend to names
      character hold*36   ! for dialog
      character objmenu*32 !menu for selected object
      character ZN*12,zd*64,msg*72
      real objbb(3)
      character predef*144,temp*12
      integer ianother   ! flag for jumping to a different zone

      helpinsub='edzone'  ! set for subroutine

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
      CALL EASKATOG(' ','New zone options:','input dimensions',
     &  'load existing (ESP-r)','load existing (cflow3 zip)',
     &  'pre-defined entity','cancel',' ',' ',INZOPT,nbhelp)

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

C Existing ESP-r geometry file.
        DFILE=' '
        WRITE(OUTSTR,'(a,i3,a)')'For zone ',IC,
     &   ' the geometry is defined in: '
        CALL EASKS(LTMP,OUTSTR,' ',72,DFILE,'geom file',IER,nbhelp)
        IF(LTMP(1:2).EQ.'  ')GOTO 293
        call FINDFIL(LTMP,XST)
        IF(.NOT.XST)THEN
          CALL EASKOK('Geometry file not found!','Retry?',
     &         OK,nbhelp)
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
          CALL EASKOK(OUTSTR,'Read problem, retry?',OK,nbhelp)
          if(OK)then
            GOTO 293
          else
            ier = -3 ! signal user cancel
            return
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

C Update the zone and surface arrays as well as the
C connection list and then the model configuration.
        NCOMP=NCOMP+1
        NZSUR(NCOMP)=NSUR
        NZTV(NCOMP)=NTV
        NCCODE(NCOMP)=NCOMP
        iZBFLG(NCOMP)=0
        do J=1,NZSUR(NCOMP)
          isznver(ncomp,J)=NVER(J)
          N = isznver(ncomp,J)
          do K=1,N
            iszjvn(ncomp,j,K)=JVN(J,K)
          enddo
        enddo

C Update the global coordinates for this new zones surfaces so that
C the subsequent wireframe image can be drawn and the bounds of
C the zone can be calculated.
        do J=1,NZTV(NCOMP)
          szcoords(NCOMP,J,1)=X(J)
          szcoords(NCOMP,J,2)=Y(J)
          szcoords(NCOMP,J,3)=Z(J)
        enddo
        if(OKC)then
          icascf(NCOMP)=0
          call CTLWRT(ICTLF,IER)  ! update the control file
        endif
        ICCC=NCON   ! connection number of the newest surface

C If surface attributes are adaibetic or similar then use these in
C the connections list, then remind user to double check topology.
        altered = .false.
        DO 132 ICC=1,NSUR
          ICCC=ICCC+1
          IC1(ICCC)=IC
          IE1(ICCC)=ICC
          if(SOTHER(NCOMP,icc,1)(1:8).eq.'EXTERIOR')then
            write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &        ' is an EXTERIOR connection which will be used.'
            call edisp(iuout,outs)
            ICT(ICCC)=0; IC2(ICCC)=0; IE2(ICCC)=0
          elseif(SOTHER(NCOMP,icc,1)(1:7).eq.'SIMILAR')then
            write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &        ' is a SIMILAR connection which will be used.'
            call edisp(iuout,outs)
            ICT(ICCC)=1; IC2(ICCC)=0; IE2(ICCC)=0
          elseif(SOTHER(NCOMP,icc,1)(1:9).eq.'ADIABATIC')then
            ICT(ICCC)=5; IC2(ICCC)=0; IE2(ICCC)=0
          elseif(SOTHER(NCOMP,icc,1)(1:8).eq.'CONSTANT')then
            write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &    ' was a CONSTANT connection. Confirm its surface attributes.'
            call edisp(iuout,outs)
            SOTHER(NCOMP,icc,1)='UNKNOWN'
            ICT(ICCC)=-1; IC2(ICCC)=0; IE2(ICCC)=0
          elseif(SOTHER(NCOMP,icc,1)(1:6).eq.'GROUND')then
            write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &      ' was a GROUND connection. Confirm its surface attributes.'
            call edisp(iuout,outs)
            SOTHER(NCOMP,icc,1)='UNKNOWN'
            ICT(ICCC)=-1; IC2(ICCC)=0; IE2(ICCC)=0
          elseif(SOTHER(NCOMP,icc,1)(1:7).eq.'UNKNOWN')then
            write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &    ' has an undefined boundary. Confirm its surface attributes.'
            call edisp(iuout,outs)
            ICT(ICCC)=-1; IC2(ICCC)=0; IE2(ICCC)=0
          elseif(SOTHER(NCOMP,icc,1)(1:7).eq.'ANOTHER')then

C Existing partitions in an imported zone are reset to UNKNOWN.
            write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &        ' was a partition. Confirm its surface attributes.'
            call edisp(iuout,outs)
            SOTHER(NCOMP,icc,1)='UNKNOWN'
            ICT(ICCC)=-1; IC2(ICCC)=0; IE2(ICCC)=0
            altered = .true.   ! will need to write out geometry
          else

C Existing partitions in an imported zone are reset to UNKNOWN.
            write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &        ' might be a partition. Confirm its surface attributes.'
            call edisp(iuout,outs)
            SOTHER(NCOMP,icc,1)='UNKNOWN'
            ICT(ICCC)=-1
            IC2(ICCC)=0
            IE2(ICCC)=0
            altered = .true.   ! will need to write out geometry
          endif
  
C Update the connection based data structures.
          IZSTOCN(ncomp,icc)=iccc

C Find the index of the MLC which matches this surface.
          smlcindex(ncomp,icc)=0  ! assume no matching MLC          
          do 5 ii=1,nmlc
            lnssmlc=lnblnk(SMLCN(NCOMP,icc))
            if(SMLCN(NCOMP,icc)(1:lnssmlc).eq.
     &         mlcname(ii)(1:lnmlcname(ii)))then
              smlcindex(ncomp,icc)=ii   ! remember MLC index     
            endif
  5       continue
  132   CONTINUE
        NCON=ICCC
        call zgupdate(0,ncomp,ier)     ! update data structures
        call zinfo(ncomp,zoa,zvol,'q')

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
        helptopic='zone_also_schedules'
        call gethelptext(helpinsub,helptopic,nbhelp)
        write(msg,'(3a)') 'Use an existing operation file for ',
     &    zname(IC)(1:lnzname(IC)),'?'
        CALL EASKOK(' ',msg,OK,nbhelp)
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
          WRITE(OUTSTR,'(a,i3,3a)')' For zone ',IC,' ',
     &      zname(IC)(1:lnzname(IC)),' the operations are defined in:'
 295      CALL EASKS(LTMP,OUTSTR,' ',72,DFILE,'opr file',IER,nbhelp)
          IF(LTMP(1:2).EQ.'  ')GOTO 295
          call FINDFIL(LTMP,XST)
          IF(.NOT.XST)THEN
            CALL EASKOK('Operations file not found!','Retry?',OK,nbhelp)
            IF(OK)GOTO 295
          endif
          LPROJ(IC)=LTMP
        else
          LPROJ(IC)='UNKNOWN'
        endif

        helptopic='zone_also_constructions'
        call gethelptext(helpinsub,helptopic,nbhelp)
        write(msg,'(3a)') 'Use an existing constructions file for ',
     &    zname(IC)(1:lnzname(IC)),'?'
        CALL EASKOK(' ',msg,OK,nbhelp)
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
          WRITE(OUTSTR,'(a,i3,3a)')' For zone ',IC,' ',
     &      zname(IC)(1:lnzname(IC)),
     &      ' the constructions are defined in:'
 297      CALL EASKS(LTMP,OUTSTR,' ',72,DFILE,'constr file',IER,nbhelp)
          IF(LTMP(1:2).EQ.'  ')GOTO 297
          call FINDFIL(LTMP,XST)
          IF(.NOT.XST)THEN
            CALL EASKOK('Constructions file not found!','Retry?',
     &        OK,nbhelp)
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
        WRITE(OUTSTR,'(a,i3,a)')'For zone ',IC,
     &    ' the geometry is to be scanned from:'
 396    CALL EASKS(LTMP,' ',OUTSTR,72,DFILE,'zip file',IER,nbhelp)
        IF(LTMP(1:2).EQ.'  ')GOTO 293
        call FINDFIL(LTMP,XST)
        IF(.NOT.XST)THEN
          CALL EASKOK('Zip file not found!','Retry?',OK,nbhelp)
          IF(OK)GOTO 396
        ENDIF

C Scan zip file and update control and connections.
        CALL EZIPIN(IFIL+1,ltmp,IC,1,ITRC,iuout,IER)
        IF(IER.NE.0)THEN
          CALL EASKOK('Problem reading CAD file!',
     &               'retry?',OK,nbhelp)
          IF(OK)GOTO 396
        ENDIF

      elseif(INZOPT.eq.4)then

C Predefined entities. We are looking for zone type entries such
C as plant components or portions of buildings.
C NOTE: If creating a new zone then we do not have to
C invert any of the surfaces. If the entity bounding surfaces
C are to be shared with another zone then they need to be
C inverted. Currently, assume that the user will manually import.
        call choosepredef(name,objmenu,objbb,ier)
        if(ier.eq.2)then
          goto 293    ! nothing selected
        elseif(ier.eq.1)then
          call usrmsg('Predefined objects not found or the file',
     &                'was corrupt','W')
          goto 293
        endif

C Take into account of the path to the database.
        lndbp=lnblnk(standarddbpath)
        if(ipathpredef.eq.0.or.ipathpredef.eq.1)then
          predef=LPREDEF  ! use as is
        elseif(ipathpredef.eq.2)then
          write(predef,'(3a)') standarddbpath(1:lndbp),fs,
     &      LPREDEF(1:lnblnk(LPREDEF))  ! prepend db folder path
        endif
        call RPREDEFCOM(IFA,predef,name,IER)

C Code bits from SCRZONE.
C If newer model cfg file ensure version 1.1 geometry file created. 
        if(icfgv.lt.4)then
          newgeo=.false.  ! assume older format geometry.
        else
          newgeo=.true.   ! set to newer format
          gversion(ic)=1.1
        endif

C Set folder separator (fs) to \ or / as required.
        call isunix(unixok)
        if(unixok)then
          fs = char(47)
        else
          fs = char(92)
        endif

C Establish name of the zone. Base this on the predefined entity.
        write(ZN,'(a)') name
 42     helptopic='zone_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKS(ZN,'Zone description?','(<12 chars, no spaces)',
     &    12,'New zone','zone name',IER,nbhelp)
        IF(ZN.eq.' '.or.ier.ne.0)goto 42
        call st2name(ZN,zname(IC))
        lnzname(ic)=lnblnk(zname(IC))  ! update this string length.

        write(zd,'(3a)') zname(IC)(1:lnzname(IC)),' ',objmenu
 43     CALL EASKS(zd,' ','What does it represent?',
     &       64,'no description entered','zone descr',IER,nbhelp)
        IF(zd.eq.' '.or.ier.ne.0)goto 43
        zdesc(IC)=zd
        lnzdesc(IC)=lnblnk(zd)  ! update the length of this string.

C Initial guesses for related zone files.
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(DFILE,'(2a)') zname(IC)(1:lnzname(IC)),'.geo'
          WRITE(CFILE,'(2a)') zname(IC)(1:lnzname(IC)),'.con'
          WRITE(OFILE,'(2a)') zname(IC)(1:lnzname(IC)),'.opr'
        else
          WRITE(DFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &      zname(IC)(1:lnzname(IC)),'.geo'
          WRITE(CFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &      zname(IC)(1:lnzname(IC)),'.con'
          WRITE(OFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &      zname(IC)(1:lnzname(IC)),'.opr'
        endif
        LGEOM(IC)=DFILE
        LTHRM(IC)=CFILE
        LPROJ(IC)=OFILE
        NTV=0            ! clear geometry prior to import
        NZTV(ic)=NTV
        NSUR=0
        NZSUR(IC)=NSUR
        IUZBASEA(ic)=0
        IBASES(IC,1)=0
        iZBFLG(IC)=0
        nbvis(ic)=0      ! what else needs to be cleared?

C As a new zone establish what the next available connection is.
        ICCC=NCON   ! connection number of the first object surface
        ICCC=ICCC+1
        IC1(ICCC)=IC
        IE1(ICCC)=1
        IZSTOCN(ic,1)=ICCC

C Ask where to place it (get new origin) and then use GB1
C to temporarily draw it.
 422    call edisp(iuout,'  ')
        write(outs,'(3a)')'bounding box of ',objmenu,' is.'
        call edisp(iuout,outs)
        write(outs,'(3f7.3,a)') objbb(1),objbb(2),objbb(3),' XYZm'
        call edisp(iuout,outs)
        call edisp(iuout,objnotes(1))
        if(nbobjnotes.gt.1) call edisp(iuout,objnotes(2))
        if(nbobjnotes.gt.2) call edisp(iuout,objnotes(3))
        VALR=0.
        CALL EASKR(VALR,'Object rotation (+ = anticlockwise)',
     &    'around its lower left corner:',
     &    -359.0,'W',359.0,'W',0.0,'rotation',IER,nbhelp)

C Offer direct edit or pick existing vertex.
  92    hold = ' 0.000  0.000  0.000    '
        call EASKS2CMD(HOLD,' ','Place object origin @ (X Y Z metres)',
     &    'vertex in another zone',' ',
     &    iclkok,36,' 0. 0. 0. ','vertex coord',IER,nbhelp)
        if(iclkok.eq.1) then
          INPIC=1
          CALL EPICKS(INPIC,IVALS,' ','Source zone:',
     &      12,NCOMP,zname,'zone list',IER,nbhelp)
          IF(INPIC.EQ.0) goto 92
          IZ=IVALS(1)  ! assign source zone index
          if(IZ.EQ.0)goto 92
          call edisp(iuout,' ')
          call edisp(iuout,'Select ONE vertex from the source zone.')
          call CPVERT(IZ,NVC,CX,CY,CZ,IER)
          DX=CX(1); DY=CY(1); DZ=CZ(1)
        else
          K=0
          CALL EGETWR(HOLD,K,DX,-99.,99.,'W','X tr',IER)
          CALL EGETWR(HOLD,K,DY,-99.,99.,'W','Y tr',IER)
          CALL EGETWR(HOLD,K,DZ,-99.,99.,'W','Z tr',IER)
        endif
        CALL CNVBLK(DX,DY,DZ,objbb(1),objbb(2),objbb(3),VALR)

C Peview the bounding box of the object via DRWBB.
        write(temp,'(A)')name
        call drwbb(temp,ier)

        CALL EASKABC(' ','Options for predefined object: ',
     &    'apply transform and merge','revise transform','cancel',
     &    IW,nbhelp)
        if (IW.eq.1) then

C Ask for single character to prepend to copied associated entities.
C Then used predefembed to read in the predefined into the model.
          CALL EASKABCD(' ','Character to pre-pend to copied entities:',
     &      ' a ',' b ',' c ',' d ',IBOPT,nbhelp)
          if(ibopt.eq.1)prec='a'
          if(ibopt.eq.2)prec='b'
          if(ibopt.eq.3)prec='c'
          if(ibopt.eq.4)prec='d'
          call PREDEFEMBED(IFIL+2,predef,IC,DX,DY,DZ,VALR,name,
     &      prec,IER)
        elseif (IW.eq.2) then
          goto 422   ! test another location
        elseif (IW.eq.3) then

C User declined. Free the data structures.
          LGEOM(IC)="UNKNOWN"
          LTHRM(IC)="UNKNOWN"
          LPROJ(IC)="UNKNOWN"
          NZTV(ic)=0
          NZSUR(IC)=0
          NCON=NCON-1
C          NCOMP=NCOMP-1
          ier = -3  ! signal user cancel
          return
        endif

C Signal that the wireframe image needs to be redrawn.
        MODIFYVIEW=.TRUE.; MODLEN=.TRUE.; MODBND=.TRUE.

C As surfaces added save the model cfd and cnn files.
        CALL EMKCFG('-',IER)

C Present message about what to do next.
        helptopic='after_predef_warning'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('predef popup',nbhelp,'-',0,0,IER)

      elseif(INZOPT.EQ.5)then
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
#include "net_flow_data.h"
#include "esprdbfile.h"
#include "espriou.h"
#include "CFC_common.h"
#include "control.h"
#include "schedule.h"
#include "prj3dv.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

      common/cctlnm/ctldoc,lctlf
      integer icascf
      common/cctl/icascf(mcom)
      common/user/browse
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas

      LOGICAL OK,XST,changed,OKC,OKM,modopr,browse
      logical newgeo  ! to use for testing if new/old geometry file.

      CHARACTER LAPROB*72
      CHARACTER ctldoc*248,LCTLF*72,outs*124
      character ltmp*72
      character OUTSTR*124

      helpinsub='edzone'  ! set for subroutine

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
      CALL EASKOK(' ',outs,OK,nbhelp)
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
          if(OUTSTR(1:18).EQ.'*Graphical_network')then

C Found a graphic network file, scan it (silently) and then convert it
C into network flow common blocks.
            IAIRN = 2
            CALL ERPFREE(IUM,ISTAT)
            call NETREAD(IUM,'R',IER)
            CALL NETTOFLW(ier)
          elseif(OUTSTR(1:13).EQ.'*Flow_network')then

C Found 3D flow network file.
            IAIRN = 3
            CALL ERPFREE(IUM,ISTAT)
            CALL MFCDAT
            CALL EMF3DREAD(IUM,'R',IER)
          else

C Found legacy text mass flow file.
            REWIND(IUM,ERR=999)
            CALL EMFREAD(IUM,IER)
            CALL ERPFREE(IUM,ISTAT)
            IAIRN = 1
          endif
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
              helptopic='zone_delete_net_node'
              call gethelptext(helpinsub,helptopic,nbhelp)
              CALL EASKOK(outs,'Remove ?',OK,nbhelp)
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
        CALL EASKOK('Zone controls should be updated.',
     &   'Is one associated with the model?',OKC,nbhelp)
      else
        CALL EASKOK('Zone control should be updated.',
     &    'Do this?',OKC,nbhelp)
      endif
      if(OKC)then
        ltmp=LCTLF
        CALL EASKS(ltmp,'Control file?',
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
                  CALL EASKOK(outs,'Remove?',OK,nbhelp)
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
                  CALL EASKOK(outs,' Remove?',OK,nbhelp)
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

                  CALL EASKOK(outs,'Remove?',OK,nbhelp)
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
                SOTHER(IX,ISX,1)='UNKNOWN'
                SOTHER(IX,ISX,2)='0'
                SOTHER(IX,ISX,3)='0'
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
C << place to take into account where prj was invoked and the
C << path to the model cfg folder.

      helptopic='zone_delete_files'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKOK(' ','Delete corresponding tdescriptive files?',
     &        OK,nbhelp)
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
        IBASES(IX,7)=IBASES(IX+1,7)
        IBASES(IX,8)=IBASES(IX+1,8)
        IBASES(IX,9)=IBASES(IX+1,9)
        IBASES(IX,10)=IBASES(IX+1,10)
        IBASES(IX,11)=IBASES(IX+1,11)
        IBASES(IX,12)=IBASES(IX+1,12)
        IBASES(IX,13)=IBASES(IX+1,13)
        IBASES(IX,14)=IBASES(IX+1,14)
        IBASES(IX,15)=IBASES(IX+1,15)
        IBASES(IX,16)=IBASES(IX+1,16)
        IBASES(IX,17)=IBASES(IX+1,17)
        IBASES(IX,18)=IBASES(IX+1,18)
        IBASES(IX,19)=IBASES(IX+1,19)

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

C ************* COPYZONE
C COPYZONE systematicly replicates a list of thermal zones.

      SUBROUTINE COPYZONE(ITRC,iznumber,izclist,DX,DY,DZ,DANG,IER)

#include "building.h"
#include "model.h"
#include "geometry.h"
#include "net_flow.h"
#include "schedule.h"
#include "prj3dv.h"
#include "help.h"
      
      integer lnblnk  ! function definition

C Parameters passed:
      integer itrc            ! level of chatter
      integer iznumber        ! how many zones in the list
      dimension izclist(MCOM) ! list of zones
      real DX,DY,DZ           ! offsets to apply
      real DANG               ! Z rotation to apply
      integer ier             ! error state on return

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      common/gzonpik/izgfoc,nzg,nznog(mcom)
      INTEGER :: izgfoc,nzg,nznog

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

      common/cctlnm/ctldoc,lctlf
      integer icascf
      common/cctl/icascf(mcom)
      common/user/browse
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/PRECT2/TMCT2(MCOM,MTMC,5),TMCA2(MCOM,MTMC,ME,5),
     &              TVTR2(MCOM,MTMC)
      COMMON/PRECT3/NTMC,NGLAZ(MTMC)
      character TOPTIC*12
      common/PRECT4/TOPTIC(MCOM,MTMC)
      COMMON/TMCB1/IBCMT(MCOM,MTMC)
      COMMON/TMCB2/NBCTMC(MCOM,MTMC),IBCST(MCOM,MTMC),
     &             IBCFT(MCOM,MTMC),IBCSUR(MCOM,MTMC)
      COMMON/TMCB3/NBCTT(MCOM,MTMC),BACTPT(MCOM,MTMC)

      common/BSCTL/ITPREP(MCOM,MTMC),ICZN,IMDB(3),IGLZ(3)
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas

C Zone casual gain control data.
      character cgcdesc*64
      common/CGCIND/cgcdesc(MCOM)
      COMMON/CGCIN2N/NCGTC(MCOM,MDTY),NLITZ(MCOM),IDFST(MCOM,MLCOM),
     & CGX(MCOM,MLCOM,MDF),CGY(MCOM,MLCOM,MDF),CGH(MCOM,MLCOM,MDF),
     & UX(MCOM,MLCOM,MDF),UY(MCOM,MLCOM,MDF),UH(MCOM,MLCOM,MDF),
     & SETPT(MCOM,MLCOM),SYSPER(MCOM,MLCOM),SOFFLL(MCOM,MLCOM),
     & IOFFDT(MCOM,MLCOM),SMLOUT(MCOM,MLCOM),SMEOUT(MCOM,MLCOM)
      COMMON/CGCIN3/ICGCS(MCOM,MDTY),ICGCF(MCOM,MDTY),
     & ICGCFL(MCOM,MLCOM),SPELEC(MCOM,MLCOM),SLOPEM(MCOM,MLCOM)

      COMMON/DAYF/NDF(MCOM,MLCOM),DFDAT(MCOM,MLCOM,MGT,MDF),
     & AZIJL(MCOM,MGT),SUMDF(MCOM,MLCOM,MDF),NUDWIN(MCOM)

      COMMON/DCOEF/NDCFP,NDCFS(MDCFP),DCOEF(MDCFP,MDCFS,MDCFV),
     & IDCFID(MCOM,MLCOM,MDF)

      logical OK,XST,OKC,browse,unixok
      logical newgeo  ! to use for testing if new/old geometry file.
      logical closex,closey,closez  ! to check transforms
      logical okzt,okzr,closer,okob,quiet
      integer loop
      dimension jict(MS),jic2(MS),jie2(MS),jmlcindex(MS)
      dimension jsotf(MS),jsmlcn(MS),jsvfc(ms),jsother(ms,3)
      dimension jsname(MS),jsparent(MS),jsuse(ms,3)
      character jsmlcn*32,jsvfc*4,jsotf*32,jsother*24,jsname*12
      character jsparent*12,jsuse*12

      CHARACTER LAPROB*72
      CHARACTER ctldoc*248,LCTLF*72,outs*124,ZN*12
      character ltmp*72,DFILE*72,CFILE*72
      character msg*96,zd*64,fs*1
      character lctmp*72
      character sfile*72,snpfile*72

      helpinsub='edzone'  ! set for subroutine

      if(browse)then
        call usrmsg('Cannot copy zone(s) while in browse ',
     &              'mode, you must `own` the model! ','W')
        return
      endif
      newgeo=.false.          ! assume older format geometry.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      VALX=DX; VALY=DY; VALZ=DZ; VAL=DANG; IER=0   ! Cast parameters to local values.
    
      do loop = 1,iznumber    ! Deal with each of the zones in the list
        IC=izclist(loop)
        LTMP=LGEOM(IC)

C Recover connections associated with that zone.
        jixu=0
        do 35 IXU = 1,NCON
          if(IC1(IXU).eq.IC)then
            icz=IC1(IXU); ics=IE1(IXU)   ! recover associated zone and surface
            jixu=jixu+1
            jict(jixu)=ICT(IXU)
            jic2(jixu)=IC2(IXU)
            jie2(jixu)=IE2(IXU)
            jsotf(jixu)=SOTF(icz,ics)
            jsmlcn(jixu)=SMLCN(icz,ics)
            jmlcindex(jixu)=smlcindex(icz,ics)
            jsvfc(jixu)=SVFC(icz,ics)
            jsother(jixu,1)=SOTHER(icz,ics,1)
            jsother(jixu,2)=SOTHER(icz,ics,2)
            jsother(jixu,3)=SOTHER(icz,ics,3)
            jsuse(jixu,1)=SUSE(icz,ics,1)
            jsuse(jixu,2)=SUSE(icz,ics,2)
            jsname(jixu)=SNAME(icz,ics)
            jsparent(jixu)=SPARENT(icz,ics)
          endif
   35   continue

C If there is control and one zone confirm. If multiple zones set OKC=.true.
        if(iznumber.eq.1)then
          OKC=.false.
          helptopic='zone_copy_and_ctl'
          call gethelptext(helpinsub,helptopic,nbhelp)
          if(LCTLF(1:2).eq.'  '.or.LCTLF(1:4).eq.'UNKN')then
            continue
          else
            CALL EASKOK(' ','Update zone control assignment?',
     &        OKC,nbhelp)
          endif
          if(OKC)then
            lctmp=LCTLF
            CALL EASKS(lctmp,'Control file?',
     &        ' ',72,'std.ctl','Control file',IER,nbhelp)
            if(lctmp(1:2).ne.'  '.and.lctmp(1:4).ne.'UNKN')then
              lctlf=lctmp
              ICTLF=IFIL+2
              CALL ERPFREE(ICTLF,ISTAT)
              call FINDFIL(LCTLF,XST)
              if(XST)then
                CALL EZCTLR(ICTLF,ITRC,IUOUT,IER)
              else
                OKC=.false.
              endif
            endif
          endif
        else
          if(LCTLF(1:2).eq.'  '.or.LCTLF(1:4).eq.'UNKN')then
            OKC=.false.
          else
            OKC=.true.
          endif
          if(OKC)then
            ICTLF=IFIL+2
            CALL ERPFREE(ICTLF,ISTAT)
            call FINDFIL(LCTLF,XST)
            if(XST)then
              CALL EZCTLR(ICTLF,ITRC,IUOUT,IER)
            else
              OKC=.false.
            endif
          endif
        endif

C Read the geometry of the source zone to fill in the data below.
C Make the source zone and the destination zone the same version
C number. When reading in the source zone use the updated ncomp
C zone index so that the related commons are filled.
        NCOMP=NCOMP+1
        call eclose(gversion(IC),1.1,0.01,newgeo)
        gversion(ncomp)=gversion(ic)  ! set to same version
        call georead(IFIL+1,LTMP,ncomp,1,iuout,IER)
        IF(IER.NE.0)THEN
          ier=-2
          call usrmsg('Problem scanning geometry file.',LTMP,'W')
          return
        ENDIF

        helpinsub='edcfg'   ! multiple dialogs use this
        helptopic='zone_name_description'
        call gethelptext(helpinsub,helptopic,nbhelp)
        helpinsub='edzone'  ! reset for subroutine
        write(msg,'(2a)') 'The original zone name: ',zname(NCOMP)
        call edisp(iuout,' ')
        call edisp(iuout,msg)
 42     ZN=' '
        CALL EASKS(ZN,'Name of this copied zone?',
     &    ' ( <12 char, no blanks) ?',12,'new_zone','zone name',
     &    IER,nbhelp)
        IF(ZN.eq.' '.or.ier.ne.0)goto 42
        call st2name(ZN,zname(NCOMP))
        lnzname(NCOMP)=lnblnk(zname(NCOMP))
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(DFILE,'(A,A4)') zname(NCOMP)(1:lnzname(NCOMP)),'.geo'
        else
          WRITE(DFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &      zname(NCOMP)(1:lnzname(NCOMP)),'.geo'
        endif
        LGEOM(NCOMP)=DFILE

        write(outs,'(2a)') 'The original description: ',zdesc(NCOMP)
        call edisp(iuout,' ')
        call edisp(iuout,outs)
        write(zd,'(2a)') 
     &    zname(NCOMP)(1:lnzname(NCOMP)),' describes a '
  40    CALL EASKS(zd,' ','What does it represent?',
     &       64,'no description entered','zone descr',IER,nbhelp)
        IF(zd.eq.' '.or.ier.ne.0)goto 40
        zdesc(NCOMP)=zd
        lnzdesc(NCOMP)=lnblnk(zd)  ! update the length of this string.

C If an obstruction file exists check if this should be copied as well.
C Save zone geometry, read in obstructions, write out to another
C file and then recover the zone commons.
        if(IOBS(ic).eq.1)then
          OKOB=.true.
          helpinsub='edcfg'   ! multiple dialogs use this
          helptopic='options_for_copy_obs'
          call gethelptext(helpinsub,helptopic,nbhelp)
          helpinsub='edzone'  ! reset for subroutine
          CALL EASKABC('The source zone had an obstructions file.',
     &      'Options:','copy obstructions file','use source file',
     &      'ignore it',IRT,nbhelp)
          if(IRT.eq.1)then
            CALL ESCZONE(NCOMP)
            call FINDFIL(ZOBS(IC),XST)
            IF(XST)THEN
              CALL EGOMST(IUF,IC,ZOBS(IC),0,ITRC,ITRU,IER)
              IOBS(NCOMP)=1 
              if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
                WRITE(DFILE,'(2a)') 
     &            zname(NCOMP)(1:lnzname(NCOMP)),'.obs'
              else
                WRITE(DFILE,'(4a)') 
     &            zonepth(1:lnblnk(zonepth)),fs,
     &            zname(NCOMP)(1:lnzname(NCOMP)),'.obs'
              endif
              ZOBS(NCOMP)=DFILE
              CALL MKGOMST(IUF,ZOBS(NCOMP),NCOMP,IER)
            endif
            CALL ERCZONE(NCOMP)
          elseif(IRT.eq.2)then
            ZOBS(NCOMP)=ZOBS(IC)  ! point to the file
            IOBS(NCOMP)=1 
            OKOB=.false.          ! but do not transform it
          elseif(IRT.eq.2)then
            ZOBS(NCOMP)='UNKNOWN' ! clear name
            IOBS(NCOMP)=0         ! dereference
            OKOB=.false.          ! and do not attempt to transform
          endif
        elseif(IOBS(ic).eq.2)then

C Obstructions in geo file so just set OKOB to true.
          OKOB=.true.
        endif

C Transforms and rotations.
        okzt=.false.
        closex=.false.; closey=.false.; closez=.false.
        VALX=DX; VALY=DY; VALZ=DZ
        call eclose(VALX,0.0,0.01,closex)
        call eclose(VALY,0.0,0.01,closey)
        call eclose(VALZ,0.0,0.01,closez)
        if(closex.or.closey.or.closez)then

C Start with the zone vertices.
          DO 162 I=1,NTV
            X(I)=X(I)+VALX
            Y(I)=Y(I)+VALY
            Z(I)=Z(I)+VALZ
  162     continue
          iZBFLG(NCOMP)=0

C If there are visual entities.
          if(nbvis(ncomp).gt.0)then
            do loop2=1,nbvis(ncomp)
              XOV(ncomp,loop2)=XOV(ncomp,loop2)+VALX
              YOV(ncomp,loop2)=YOV(ncomp,loop2)+VALY
              ZOV(ncomp,loop2)=ZOV(ncomp,loop2)+VALZ
              do ibe=1,8
                XVP(ncomp,loop2,ibe)=XVP(ncomp,loop2,ibe)+VALX
                YVP(ncomp,loop2,ibe)=YVP(ncomp,loop2,ibe)+VALY
                ZVP(ncomp,loop2,ibe)=ZVP(ncomp,loop2,ibe)+VALZ
              enddo  ! of ibe
            enddo    ! of loop2
          endif
          okzt=.true.
        endif

        VAL=DANG
        okzr=.false.
        call eclose(VAL,0.0,0.01,closer)
        if(.NOT.closer)then
          okzr=.true.
          if(VAL.LT.-.01.OR.VAL.GT..01)then

C Rotation choices.
            rotateit(ncomp,1)=VAL
            CALL EASKABCD(' ','Rotation choices:',
     &        'vertex 1 of zone','site origin',
     &        'specified X & Y','skip rotation',IRT,nbhelp)
            call usrmsg(' ',' ','-')
            if(IRT.eq.1)then
              x1=X(1)
              y1=Y(1)
              rotateit(ncomp,2)=x1
              rotateit(ncomp,3)=y1
              CALL ESCROT(VAL,x1,y1)
              rotateit(ncomp,1)=0.0
              rotateit(ncomp,2)=0.0
              rotateit(ncomp,3)=0.0
            elseif(IRT.EQ.2)THEN
              x1=0.
              y1=0.
              rotateit(ncomp,2)=x1
              rotateit(ncomp,3)=y1
              CALL ESCROT(VAL,x1,y1)
              rotateit(ncomp,1)=0.0
              rotateit(ncomp,2)=0.0
              rotateit(ncomp,3)=0.0
            elseif(IRT.EQ.3)THEN
              x1=0.
              CALL EASKR(x1,' ',' X point (metres) ? ',
     &          0.0,'-',0.0,'-',0.0,'x point',IER,nbhelp)
              y1=0.
              CALL EASKR(y1,' ',' Y point (metres) ? ',
     &          0.0,'-',0.0,'-',0.0,'y point',IER,nbhelp)
              rotateit(ncomp,2)=x1
              rotateit(ncomp,3)=y1
              CALL ESCROT(VAL,x1,y1)
              rotateit(ncomp,1)=0.0
              rotateit(ncomp,2)=0.0
              rotateit(ncomp,3)=0.0
              call usrmsg(' ',' ','-')
            elseif(IRT.EQ.4)THEN
              rotateit(ncomp,1)=0.0
              rotateit(ncomp,2)=0.0
              rotateit(ncomp,3)=0.0
            endif
            iZBFLG(NCOMP)=0
          endif

C If there are visual entities.
          if(nbvis(ncomp).gt.0)then
            if(IRT.eq.1)then
              x1=X(1)
              y1=Y(1)
            elseif(IRT.EQ.2)THEN
              x1=0.
              y1=0.
            elseif(IRT.EQ.3)THEN
              CALL EASKR(x1,' ',' X point (metres) ? ',
     &          0.0,'-',0.0,'-',0.0,'x point',IER,nbhelp)
              CALL EASKR(y1,' ',' Y point (metres) ? ',
     &          0.0,'-',0.0,'-',0.0,'y point',IER,nbhelp)
            elseif(IRT.EQ.4)THEN
              continue
            endif
            if(IRT.ge.1.and.IRT.lt.4)then
              PI = 4.0 * ATAN(1.0)
              A=-VAL*PI/180.0; CA=COS(A); SA=SIN(A)
              do loop3=1,nbvis(ncomp)
                XXX=XOV(ncomp,loop3)-X1; YYY=YOV(ncomp,loop3)-Y1
                XR=XXX*CA+YYY*SA; YR=YYY*CA-XXX*SA
                XOV(ncomp,loop3)=XR+X1; YOV(ncomp,loop3)=YR+Y1
                BANGOV(ncomp,loop3,1)=BANGOV(ncomp,loop3,1)+VAL
                do ibe=1,8
                  XXX=XVP(ncomp,loop3,ibe)-X1
                  YYY=YVP(ncomp,loop3,ibe)-Y1
                  XR=XXX*CA+YYY*SA; YR=YYY*CA-XXX*SA
                  XVP(ncomp,loop3,ibe)=XR+X1
                  YVP(ncomp,loop3,ibe)=YR+Y1
                enddo  ! of ibe
              enddo
            endif
          endif
        endif

C Because the writing of geometry files is based on G6 common block data 
C update connections based info prior to writing the new geometry file. Retain 
C 'similar' and 'adiabatic' connections if found. Check if any of the SUSE
C tag imply flow components.
        NZSUR(NCOMP)=NSUR
        NZTV(NCOMP)=NTV
        NCCODE(NCOMP)=NCOMP
        nzg=NCOMP
        ICCC=NCON
        DO 132 ICC=1,NSUR
          ICCC=ICCC+1
          IC1(ICCC)=NCOMP
          IE1(ICCC)=ICC 
          IZSTOCN(ncomp,icc)=iccc
          SOTF(ncomp,icc)=jsotf(icc)
          SMLCN(ncomp,icc)=jsmlcn(icc)
          smlcindex(ncomp,icc)=jmlcindex(icc)
          SVFC(ncomp,icc)=jsvfc(icc)
          SOTHER(ncomp,icc,1)=jsother(icc,1)
          SOTHER(ncomp,icc,2)=jsother(icc,2)
          SOTHER(ncomp,icc,3)=jsother(icc,3)
          SUSE(ncomp,icc,1)=jsuse(icc,1)
          SUSE(ncomp,icc,2)=jsuse(icc,2)
          SPARENT(ncomp,icc)=jsparent(icc)
          if(jict(ICC).eq.0)then
            ICT(ICCC)=0; IC2(ICCC)=0; IE2(ICCC)=0
          elseif(jict(ICC).eq.1.or.jict(ICC).eq.2)then
            ICT(ICCC)=jict(ICC)
            IC2(ICCC)=jic2(ICC)
            IE2(ICCC)=jie2(ICC)
          elseif(jict(ICC).eq.3)then
            SOTHER(ncomp,icc,1)='UNKNOWN'
            SOTHER(ncomp,icc,2)='0'
            SOTHER(ncomp,icc,3)='0'
            ICT(ICCC)=0; IC2(ICCC)=0; IE2(ICCC)=0
          elseif(jict(ICC).eq.4.or.jict(ICC).eq.5)then
            ICT(ICCC)=jict(ICC)
            IC2(ICCC)=jic2(ICC)
            IE2(ICCC)=jie2(ICC)
          endif
  132   CONTINUE
        NCON=ICCC
        call zgupdate(0,ncomp,ier)
        call zinfo(ncomp,zoa,zvol,'q')

C Find co-planer surfaces and edges of similar materials.
        call suredgeadj(itrc,'-',ncomp,ier)

C Update the global coordinates for this zone's surfaces so that
C the subsequent wireframe image can be drawn and the bounds of
C the zone can be calculated.
        DO 41 J=1,NZTV(ncomp)
          szcoords(ncomp,J,1)=X(J)
          szcoords(ncomp,J,2)=Y(J)
          szcoords(ncomp,J,3)=Z(J)
   41   CONTINUE

C Now it is ok to write out the copied zone.
        call eclose(gversion(NCOMP),1.1,0.01,newgeo)
        if(igupgrade.eq.2.and.(.NOT.newgeo))then
          gversion(ncomp) =1.1
          newgeo = .true.
        endif
        if(newgeo)then
          iuf=ifil+2
          call geowrite(iuf,LGEOM(NCOMP),NCOMP,ITRU,3,IER)
        else
          iuf=ifil+2
          call emkgeo(iuf,LGEOM(NCOMP),NCOMP,3,IER)
        endif
        IF(IER.EQ.1)THEN
          helptopic='problem_writing_file'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKOK('Problem creating geometry file!','Retry?',
     &      OK,nbhelp)
          IF(OK)GOTO 42
        ENDIF

C Apply same transformation to obstructions? If yes, take into account
C location of obstruction data.
        if(OKOB)then
          CALL ESCZONE(NCOMP)
          if(OKZT)then
            do 85 ij=1,nbobs(ncomp)
              XOB(ncomp,ij)=XOB(ncomp,ij)+VALX
              YOB(ncomp,ij)=YOB(ncomp,ij)+VALY
              ZOB(ncomp,ij)=ZOB(ncomp,ij)+VALZ
              do 95 ibe=1,8
                XBP(ncomp,ij,ibe)=XBP(ncomp,ij,ibe)+VALX
                YBP(ncomp,ij,ibe)=YBP(ncomp,ij,ibe)+VALY
                ZBP(ncomp,ij,ibe)=ZBP(ncomp,ij,ibe)+VALZ
   95         continue
   85       continue
          endif
          if(OKZR)then
            PI = 4.0 * ATAN(1.0)
            A=-VAL*PI/180.
            CA=COS(A)
            SA=SIN(A)
            do 88 ij=1,nbobs(ncomp)
              XXX=XOB(ncomp,ij)-X1
              YYY=YOB(ncomp,ij)-Y1
              XR=XXX*CA+YYY*SA
              YR=YYY*CA-XXX*SA
              XOB(ncomp,ij)=XR+X1
              YOB(ncomp,ij)=YR+Y1
              BANGOB(ncomp,ij,1)=BANGOB(ncomp,ij,1)+VAL
              do 89 ibe=1,8
                XXX=XBP(ncomp,ij,ibe)-X1
                YYY=YBP(ncomp,ij,ibe)-Y1
                XR=XXX*CA+YYY*SA
                YR=YYY*CA-XXX*SA
                XBP(ncomp,ij,ibe)=XR+X1
                YBP(ncomp,ij,ibe)=YR+Y1
  89          continue
  88        continue
          endif

C If geometry file can hold obstructions save them there,
C otherwise write out zone obstruction file.
          call eclose(gversion(ncomp),1.1,0.01,newgeo)
          if(newgeo)then
            iuf=ifil+2
            call geowrite(iuf,LGEOM(NCOMP),NCOMP,ITRU,3,IER)
            IOBS(NCOMP)=2    ! reset to signal obs in geo file
            ZOBS(NCOMP)='  '
          else
            CALL MKGOMST(IUF,ZOBS(NCOMP),NCOMP,IER)
          endif
          CALL ERCZONE(NCOMP)
        endif

C If control exists update zone links (assume no control
C in the copied zone).
        if(OKC)then
          icascf(NCOMP)=0
          CALL CTLWRT(ICTLF,IER)
        endif

C Ask if user wants to copy or point to other zone files.
C Construction and zone TMC file.
        helpinsub='edcfg'   ! multiple dialogs use this
        helptopic='copy_constr_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
        helpinsub='edzone'  ! reset for subroutine
        call FINDFIL(LTHRM(IC),XST)
        irop=1
        if(XST)then
          write(outs,'(2a)')'Found: ',LTHRM(IC)(1:lnblnk(LTHRM(IC)))
          call edisp(iuout,' ')
          call edisp(iuout,outs)
          CALL EASKATOG(' ','Source zone has a constructions file:',
     &      'copy source file','use source file',
     &      'browse','cancel',' ',' ',' ',irop,nbhelp)
        else
          CALL EASKATOG(' ','Source zone has no constructions file:',
     &      '-','-','browse','cancel',' ',' ',' ',irop,nbhelp)
          if(irop.eq.1.or.irop.eq.2) irop=4
        endif

C DFILE is the default file name for any TMC file to be created,
C CFILE is default for constr file.
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(DFILE,'(A,A4)')zname(NCOMP)(1:lnzname(NCOMP)),'.tmc'
          WRITE(CFILE,'(A,A4)')zname(NCOMP)(1:lnzname(NCOMP)),'.con'
        else
          WRITE(DFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),fs,
     &      zname(NCOMP)(1:lnzname(NCOMP)),'.tmc'
          WRITE(CFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),fs,
     &      zname(NCOMP)(1:lnzname(NCOMP)),'.con'
        endif
        if(CFILE(1:2).EQ.'  ')then   ! If blank ask user.
          CALL EASKS(CFILE,' ',' New construction file name:',
     &      72,'new.con','construction file',IER,nbhelp)
        endif
        if(DFILE(1:2).EQ.'  ')then
          CALL EASKS(DFILE,' ','New zone tmc file name:',
     &       72,'new.tmc','tmc file',IER,nbhelp)
        endif
        if(irop.eq.1)then

C Read in source constructions and write out to file for new zone.
C Also update lodlabel commons before writing out.
          CALL ERPFREE(IUF,ISTAT)
          CALL ECONST(LTHRM(IC),IFIL+1,IC,0,IUOUT,IER)
          if(ITW(IC).eq.1)then
            CALL ERTWIN(ITRC,IUOUT,IUF,LTWIN(IC),IC,IER)
          endif
          QUIET=.false.
          write(LTHRM(NCOMP),'(a)') CFILE(1:lnblnk(CFILE))
          CALL EMKCON(LTHRM(NCOMP),IUF,NCOMP,QUIET,IER)
          if(ITW(IC).eq.1)then

C Update itmcfl for the new zone.
            NTMC=0
            do 61 ISS=1,NZSUR(NCOMP)
              ITMCFL(NCOMP,ISS)=ITMCFL(IC,ISS)
              IF(ITMCFL(IC,ISS).GT.NTMC)NTMC=ITMCFL(IC,ISS)
   61       continue
            DO ISST=1,NTMC
              TOPTIC(NCOMP,ISST)=TOPTIC(IC,ISST)
              TMCT(NCOMP,ISST,1)=TMCT(IC,ISST,1)
              TMCT(NCOMP,ISST,2)=TMCT(IC,ISST,2)
              TMCT(NCOMP,ISST,3)=TMCT(IC,ISST,3)
              TMCT(NCOMP,ISST,4)=TMCT(IC,ISST,4)
              TMCT(NCOMP,ISST,5)=TMCT(IC,ISST,5)
              TVTR(NCOMP,ISST)=TVTR(IC,ISST)
              IBCMT(NCOMP,ISST)=IBCMT(IC,ISST)
              NBCTMC(NCOMP,ISST)=NBCTMC(IC,ISST)
              IBCSUR(NCOMP,ISST)=IBCSUR(IC,ISST)
              ITPREP(NCOMP,ISST)=ITPREP(IC,ISST)
              DO ISSTGL=1,NGLAZ(ISST)
                DO J5=1,5
                  TMCA(NCOMP,ISST,ISSTGL,J5)=TMCA(IC,ISST,ISSTGL,J5)
                ENDDO ! of J5
              ENDDO  ! of ISSTGL
              DO IBC=1,NBCTMC(IC,ISST)
                IBCST(NCOMP,ISST)=IBCST(IC,ISST)
                IBCFT(NCOMP,ISST)=IBCFT(IC,ISST)
                NBCTT(NCOMP,ISST)=NBCTT(IC,ISST)
                BACTPT(NCOMP,ISST)=BACTPT(IC,ISST)
                TMCT2(NCOMP,ISST,1)=TMCT2(IC,ISST,1)
                TMCT2(NCOMP,ISST,2)=TMCT2(IC,ISST,2)
                TMCT2(NCOMP,ISST,3)=TMCT2(IC,ISST,3)
                TMCT2(NCOMP,ISST,4)=TMCT2(IC,ISST,4)
                TMCT2(NCOMP,ISST,5)=TMCT2(IC,ISST,5)
                TVTR2(NCOMP,ISST)=TVTR2(IC,ISST)
                DO ISSTGL=1,NGLAZ(ISST)
                  DO J5=1,5
                    TMCA2(NCOMP,ISST,ISSTGL,J5)=
     &                TMCA2(IC,ISST,ISSTGL,J5)
                  ENDDO ! of J5
                ENDDO  ! of ISSTGL
              ENDDO  ! of IBC
            ENDDO    ! of ISST
            write(LTWIN(NCOMP),'(a)') DFILE(1:lnblnk(DFILE))
            ITW(NCOMP)=1
            QUIET=.false.
            CALL MKTWIN(IUF,NCOMP,QUIET,IER)
          endif
        elseif(irop.eq.2)then

C Point to source zones file.
          LTHRM(NCOMP)=LTHRM(IC)
          LTWIN(NCOMP)=LTWIN(IC)
          ITW(NCOMP)=1
        elseif(irop.eq.3)then

C Find out if there are construction files in the model ../zones folder.
          sfile=' '
          snpfile=' '
          call browsefilelist('?','zon','con',sfile,snpfile,nfile,
     &        iier)
          if(nfile.gt.0)then
            sfile=' '
            snpfile=' '
            call browsefilelist('b','zon','con',sfile,snpfile,
     &        nfile,iier)
            if(snpfile(1:2).ne.'  ')then
              write(CFILE,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &          snpfile(1:lnblnk(snpfile))

              LTHRM(NCOMP)=CFILE
            endif
          endif

C Find out if there are tmc files in the model ../zones folder.
          if(ITW(IC).eq.1)then
            sfile=' '
            snpfile=' '
            call browsefilelist('?','zon','tmc',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','tmc',sfile,snpfile,
     &          nfile,iier)
              if(snpfile(1:2).ne.'  ')then
                write(DFILE,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &            snpfile(1:lnblnk(snpfile))

                LTWIN(NCOMP)=DFILE
                ITW(NCOMP)=1
              endif
            endif
          endif
        elseif(irop.eq.4)then
          continue
        endif

C Operations file. If source has a file, offer options to copy or
C point to it, otherwise offer option to browse for an existing file.
        helpinsub='edcfg'   ! multiple dialogs use this
        helptopic='copy_operation_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
        helpinsub='edzone'  ! reset for subroutine
        call FINDFIL(LPROJ(IC),XST)
        irop=1
        if(XST)then
          write(outs,'(2a)')'Found: ',LPROJ(IC)(1:lnblnk(LPROJ(IC)))
          call edisp(iuout,' ')
          call edisp(iuout,outs)
          CALL EASKATOG(
     &      'Source zone has an operations file.',
     &      'Options:','copy file','point to file',
     &      'browse file','cancel',' ',' ',' ',irop,nbhelp)
        else
          CALL EASKATOG(
     &      'Source zone has no operations file.',
     &      'Options:','-','-','browse','cancel',' ',' ',' ',
     &      irop,nbhelp)
          if(irop.eq.1.or.irop.eq.2) irop=4
        endif
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(DFILE,'(A,A4)')zname(NCOMP)(1:lnzname(NCOMP)),'.opr'
        else
          WRITE(DFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),fs,
     &      zname(NCOMP)(1:lnzname(NCOMP)),'.opr'
        endif
        if(irop.eq.1)then

C Read in source operations and write out to file for new zone.
C Also update lodlabel commons before writing out. If the version
C is greater or equal 21 then additional data structures should be copied.
          CALL ERPFREE(IUF,ISTAT)
          CALL EROPER(0,iuout,IUF,IC,IER)
   91     CALL EASKS(DFILE,' New zone operations file name:',
     &       ' ',72,'new.opr','operations file',IER,nbhelp)
          IF(DFILE.NE.' ')THEN
            LPROJ(NCOMP)=DFILE
            ip3ver(ncomp)=ip3ver(ic)      ! set copied to same version
            oprdesc(ncomp)=oprdesc(ic)    ! use same description
            ventdesc(ncomp)=ventdesc(ic)  ! use same vent description
            ctlstr(ncomp,1)=ctlstr(ic,1)  ! use same control descriptions
            ctlstr(ncomp,2)=ctlstr(ic,2)
            ctlstr(ncomp,3)=ctlstr(ic,3)
            ITCTL(ncomp)=ITCTL(ic)        ! copy infil and vent controls
            TLO(ncomp)=TLO(ic); TUP(ncomp)=TUP(ic); THI(ncomp)=THI(ic)
            ACIL(ncomp)=ACIL(ic); ACVL(ncomp)=ACVL(ic)
            IVL(ncomp)=IVL(ic); TAL(ncomp)=TAL(ic)
            ACIU(ncomp)=ACIU(ic); ACVU(ncomp)=ACVU(ic)
            IVU(ncomp)=IVU(ic); TAU(ncomp)=TAU(ic)
            ACIH(ncomp)=ACIH(ic); ACVH(ncomp)=ACVH(ic)
            IVH(ncomp)=IVH(ic); TAH(ncomp)=TAH(ic)

C Copy labels for the gains.
            lodlabel(ncomp,1)=lodlabel(ic,1)  ! revise to be in a loop
            lodlabel(ncomp,2)=lodlabel(ic,2)
            lodlabel(ncomp,3)=lodlabel(ic,3)
            lodlabel(ncomp,4)=lodlabel(ic,4)

C If ip3ver >= 21 copy caskeytype and associated information.
            if(ip3ver(ic).ge.21)then
              caskeytype(ncomp,1)=caskeytype(ic,1)
              lodslot(ncomp,1)=lodslot(ic,1)
              lodatr1(ncomp,1)=lodatr1(ic,1)
              lodatr2(ncomp,1)=lodatr2(ic,1)
              caskeytype(ncomp,2)=caskeytype(ic,2)
              lodslot(ncomp,2)=lodslot(ic,2)
              lodatr1(ncomp,2)=lodatr1(ic,2)
              lodatr2(ncomp,2)=lodatr2(ic,2)
              caskeytype(ncomp,3)=caskeytype(ic,3)
              lodslot(ncomp,3)=lodslot(ic,3)
              lodatr1(ncomp,3)=lodatr1(ic,3)
              lodatr2(ncomp,3)=lodatr2(ic,3)
              caskeytype(ncomp,4)=caskeytype(ic,4)
              lodslot(ncomp,4)=lodslot(ic,4)
              lodatr1(ncomp,4)=lodatr1(ic,4)
              lodatr2(ncomp,4)=lodatr2(ic,4)
              caskeytype(ncomp,5)=caskeytype(ic,5)
              lodslot(ncomp,5)=lodslot(ic,5)
              lodatr1(ncomp,5)=lodatr1(ic,5)
              lodatr2(ncomp,5)=lodatr2(ic,5)
            endif
            CALL ERPFREE(IUF,ISTAT)
            CALL EMKOPER(IUF,LPROJ(NCOMP),NCOMP,IER)
          else
            goto 91
          endif
        elseif(irop.eq.2)then
          LPROJ(NCOMP)=LPROJ(IC)
        elseif(irop.eq.3)then

C Find out if there are operation files in the model ../zones folder.
          sfile=' '
          snpfile=' '
          call browsefilelist('?','zon','opr',sfile,snpfile,nfile,
     &      iier)
          if(nfile.gt.0)then
            sfile=' '
            snpfile=' '
            call browsefilelist('b','zon','opr',sfile,snpfile,
     &        nfile,iier)
            if(snpfile(1:2).ne.'  ')then
              write(DFILE,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &          snpfile(1:lnblnk(snpfile))

              LPROJ(NCOMP)=DFILE
            endif
          endif
        elseif(irop.eq.4)then
          continue
        endif

C See if the source zone has a viewfactor file.
C NOTE: any viewfactor block attributes will not yet be know by the
C associated zone geometry file.
        if(IVF(ic).eq.1)then
          write(outs,'(2a)')'Found: ',LVIEW(IC)(1:lnblnk(LVIEW(IC)))
          call edisp(iuout,' ')
          call edisp(iuout,outs)
          helpinsub='edcfg'   ! multiple dialogs use this
          helptopic='copy_vwf_options'
          call gethelptext(helpinsub,helptopic,nbhelp)
          helpinsub='edzone'  ! reset for subroutine
          irop=1
          CALL EASKATOG(
     &      'Source zone has a view factor file.',
     &      'Options:','copy file','point to file',
     &      'browse file','cancel',' ',' ',' ',irop,nbhelp)
          if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
            WRITE(DFILE,'(A,A4)')zname(NCOMP)(1:lnzname(NCOMP)),
     &        '.vwf'
          else
            WRITE(DFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),fs,
     &        zname(NCOMP)(1:lnzname(NCOMP)),'.vwf'
          endif
          if(irop.eq.1)then
            CALL ERPFREE(IUF,ISTAT)
            CALL ERMRT(0,itru,IUF,LVIEW(IC),IC,IER)
   92       CALL EASKS(DFILE,' ','New view factor file name?',
     &         72,'new.vwf','viewfactor file',IER,nbhelp)
            IF(DFILE.NE.' ')THEN
              LVIEW(NCOMP)=DFILE
              IVF(NCOMP)=1
              CALL ERPFREE(IUF,ISTAT)
              NZS=NZSUR(NCOMP)
              CALL EMKMRT(LVIEW(NCOMP),LGEOM(NCOMP),NZS,IUF,NCOMP,
     &          'v',IER)
            else
              goto 92
            endif
          elseif(irop.eq.2)then  ! point to another zone's file.
            LVIEW(NCOMP)=LVIEW(IC)
            IVF(NCOMP)=1
          elseif(irop.eq.3)then

C Find out if there are viewfactor files in the model ../zones folder.
            sfile=' '
            snpfile=' '
            call browsefilelist('?','zon','vwf',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','vwf',sfile,snpfile,
     &          nfile,iier)
              if(snpfile(1:2).ne.'  ')then
                write(DFILE,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &            snpfile(1:lnblnk(snpfile))

                LVIEW(NCOMP)=DFILE
                IVF(NCOMP)=1
              endif
            endif
          elseif(irop.eq.4)then
            continue
          endif
        endif

C See if the source zone has a casual gain control file.
        if(ICGC(ic).eq.1)then
          helpinsub='edcfg'   ! multiple dialogs use this
          helptopic='copy_cgc_options'
          call gethelptext(helpinsub,helptopic,nbhelp)
          helpinsub='edzone'  ! reset for subroutine
          irop=1
          write(outs,'(2a)')'Found: ',LCGCIN(IC)(1:lnblnk(LCGCIN(IC)))
          call edisp(iuout,' ')
          call edisp(iuout,outs)
          CALL EASKATOG(
     &      'Source zone has a CG ctl file.',
     &      'Options:','copy file','point to file',
     &      'browse file','cancel',' ',' ',' ',irop,nbhelp)
          if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
            WRITE(DFILE,'(A,A4)')zname(NCOMP)(1:lnzname(NCOMP)),
     &        '.cgc'
          else
            WRITE(DFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),fs,
     &        zname(NCOMP)(1:lnzname(NCOMP)),'.cgc'
          endif
          if(irop.eq.1)then
            IUNIT=IFIL+1
            CALL ERPFREE(IUNIT,ISTAT)
            call ercgcf(0,iuout,LCGCIN(IC),IC,ier)

   93       CALL EASKS(DFILE,' ','Zone CG file name?',
     &        72,'new.cgc','casual gain control file',IER,nbhelp)
            IF(DFILE.NE.' ')THEN

C Copy across the common block variables to the new zone.
              LCGCIN(NCOMP)=DFILE
              ICGC(NCOMP)=1
              cgcdesc(NCOMP)=cgcdesc(IC)
              DO IDTY=1,nbdaytype
                NCGTC(NCOMP,IDTY)=NCGTC(IC,IDTY)
                ICGCS(NCOMP,IDTY)=ICGCS(IC,IDTY)
                ICGCF(NCOMP,IDTY)=ICGCF(IC,IDTY)
              ENDDO  ! of IDTY
              NLITZ(NCOMP)=NLITZ(IC)
              NUDWIN(NCOMP)=NUDWIN(IC)
              DO NLZ=1,NLITZ(IC)
                SETPT(NCOMP,NLZ)=SETPT(IC,NLZ)
                SOFFLL(NCOMP,NLZ)=SOFFLL(IC,NLZ)
                SMLOUT(NCOMP,NLZ)=SMLOUT(IC,NLZ)
                SMEOUT(NCOMP,NLZ)=SMEOUT(IC,NLZ)
                IOFFDT(NCOMP,NLZ)=IOFFDT(IC,NLZ)
                SYSPER(NCOMP,NLZ)=SYSPER(IC,NLZ)
                IDFST(NCOMP,NLZ)=IDFST(IC,NLZ)
                ICGCFL(NCOMP,NLZ)=ICGCFL(IC,NLZ)
                SPELEC(NCOMP,NLZ)=SPELEC(IC,NLZ)
                SLOPEM(NCOMP,NLZ)=SLOPEM(IC,NLZ)
                SPELEC(NCOMP,NLZ)=SPELEC(IC,NLZ)
                SLOPEM(NCOMP,NLZ)=SLOPEM(IC,NLZ)
                NDF(NCOMP,NLZ)=NDF(IC,NLZ)
                DO JLZ=1,NDF(IC,NLZ)
                  CGX(NCOMP,NLZ,JLZ)=CGX(IC,NLZ,JLZ)
                  CGY(NCOMP,NLZ,JLZ)=CGY(IC,NLZ,JLZ)
                  CGH(NCOMP,NLZ,JLZ)=CGH(IC,NLZ,JLZ)
                  UX(NCOMP,NLZ,JLZ)=UX(IC,NLZ,JLZ)
                  UY(NCOMP,NLZ,JLZ)=UY(IC,NLZ,JLZ)
                  UH(NCOMP,NLZ,JLZ)=UH(IC,NLZ,JLZ)
                  IDCFID(NCOMP,NLZ,JLZ)=IDCFID(IC,NLZ,JLZ)
                ENDDO  ! of JLZ
                do IL=1,NUDWIN(IC)
                  do IW = 1, NZSUR(ic)
                    do L=1,NDF(IC,NLZ)
                      DFDAT(NCOMP,NLZ,IW,L)=DFDAT(IC,NLZ,IW,L)
                    enddo  ! of L
                  enddo    ! of IW
                enddo      ! of IL
              ENDDO        ! of NLZ
                 
              CALL ERPFREE(IUF,ISTAT)
              call CASCTMK(LCGCIN(NCOMP),NCOMP,'-',IER)
            else
              goto 93
            endif
          elseif(irop.eq.2)then
            LCGCIN(NCOMP)=LCGCIN(IC)
            ICGC(NCOMP)=1
          elseif(irop.eq.3)then

C Find out if there are casual gain control files in the model ../zones folder.
            sfile=' '
            snpfile=' '
            call browsefilelist('?','zon','cgc',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','cgc',sfile,snpfile,
     &          nfile,iier)
              if(snpfile(1:2).ne.'  ')then
                write(DFILE,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &            snpfile(1:lnblnk(snpfile))

                LCGCIN(NCOMP)=DFILE
                ICGC(NCOMP)=1
              endif
            endif
          elseif(irop.eq.4)then
            continue
          endif
        endif

C See if the source zone has a convection regime file.
        if(IHC(ic).eq.1)then
          helpinsub='edcfg'   ! multiple dialogs use this
          helptopic='copy_hc_options'
          call gethelptext(helpinsub,helptopic,nbhelp)
          helpinsub='edzone'  ! reset for subroutine
          irop=1
          write(outs,'(2a)')'Found: ',LHCCO(IC)(1:lnblnk(LHCCO(IC)))
          call edisp(iuout,' ')
          call edisp(iuout,outs)
          CALL EASKATOG('Source zone has a CC file.',
     &      'Options:','copy file','point file',
     &      'browse file','cancel',' ',' ',' ',irop,nbhelp)
          if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
            WRITE(DFILE,'(A,A4)')zname(NCOMP)(1:lnzname(NCOMP)),
     &        '.htc'
          else
            WRITE(DFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),fs,
     &        zname(NCOMP)(1:lnzname(NCOMP)),'.htc'
          endif
          if(irop.eq.1)then
            CALL ERPFREE(IUF,ISTAT)
            call ehtcff(LHCCO(IC),IUF,IER)
   94       CALL EASKS(DFILE,' ','Zone CC file name?',
     &        72,'new.vwf','convection regime file',IER,nbhelp)
            IF(DFILE.NE.' ')THEN
              LHCCO(NCOMP)=DFILE
              IHC(NCOMP)=1
              CALL ERPFREE(IUF,ISTAT)
              CALL EMKHTC(LHCCO(NCOMP),NCOMP,IUF,ITRU,IER)
            else
              goto 94
            endif
          elseif(irop.eq.2)then
            LHCCO(NCOMP)=LHCCO(IC)
            IHC(NCOMP)=1
          elseif(irop.eq.3)then

C Find out if there are casual gain control files in the model ../zones folder.
            sfile=' '
            snpfile=' '
            call browsefilelist('?','zon','htc',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','htc',sfile,snpfile,
     &          nfile,iier)
              if(snpfile(1:2).ne.'  ')then
                write(DFILE,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &            snpfile(1:lnblnk(snpfile))

                LHCCO(NCOMP)=DFILE
                IHC(NCOMP)=1
              endif
            endif
          elseif(irop.eq.4)then
            continue
          endif
        endif

C If there is a flow network, update the configuration file before
C writing out. This requires that we rebuild some zone data structures
C to include the newly copied zone.
        NZONES=NCOMP
        ISFSUM=0
        CALL ZDATA (ITRC,IER,NZONES,ISFSUM)
        nzg=1
        nznog(1)=NCOMP
        izgfoc=NCOMP
        CALL ESCZONE(NCOMP)
        CALL BNDOBJ(0,IER)   ! find the zone COG used in create_zone_node
        CALL ERCZONE(NCOMP)

        if(IAIRN.ge.1)then
          ICAAS(ncomp)=0
          call usrmsg(
     &      'Updating the current air flow network to include a',
     &      'node and connections for new/copied zone.','-') 
          call pauses(1)
          call create_zone_node(ncomp)  ! create new node
          DO isur=1,NSUR

C Act on surface attributes if there are any.
            icc=IZSTOCN(ncomp,isur)
            if(ICT(icc).eq.3)then
              CYCLE  ! skip internals
            endif
            if(ICT(icc).eq.-1)then
              CYCLE  ! skip UNKNOWN
            endif

C Cases to ignore.
            if(SUSE(ncomp,isur,1)(1:1).eq.'-'.or.
     &         SUSE(ncomp,isur,1)(1:4).eq.'WALL'.or.
     &         SUSE(ncomp,isur,1)(1:5).eq.'FLOOR'.or.
     &         SUSE(ncomp,isur,1)(1:5).eq.'FURNI'.or.
     &         SUSE(ncomp,isur,1)(1:7).eq.'ITEQUIP'.or.
     &         SUSE(ncomp,isur,1)(1:5).eq.'PARTN'.or.
     &         SUSE(ncomp,isur,1)(1:4).eq.'ROOF'.or.
     &         SUSE(ncomp,isur,1)(1:5).eq.'STRUC'.or.
     &         SUSE(ncomp,isur,1)(1:7).eq.'FIXTURE'.or.
     &         SUSE(ncomp,isur,1)(1:6).eq.'PLANTS')then
              CYCLE
            endif

C Create a default boundary flow node or component based on surface attributes.
C Note: linkatpartition() used by create_surface_node_cmp will not have been
C instantiated.
            call create_surface_node_cmp(ncomp,isur)
          enddo
          call updatebothflownetworks(ier) ! save changes 
        endif

C Update the configuration file.
        CALL EMKCFG('-',IER)
        MODIFYVIEW=.TRUE.
        MODBND=.TRUE.
      enddo  ! of loop
      return
      end


C ************* WARNMOD 
C Provides warning of updates needed when model is changed.
C Initially deals with change in number of surfaces. 
C Act is action to be tested. act = 'sf+' or 'sf-' surface added:deleted.
C act = 'sat' surface attribute. act = 'str' surf transform.
C act = 'ob+' or 'ob-' obstruction changes.
 
      SUBROUTINE WARNMOD(ICOMP,act)
#include "building.h"
#include "model.h"
#include "CFC_common.h"

      common/shad0/ISIcalc,icalcD,icalcM
      integer ISIcalc,icalcD,icalcM

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
        if(ISIcalc.ne.1)then            ! if not embedded calculation
          if(ISIcalc.eq.0) ISIcalc = 2  ! reset if zero
          if(ISI(ICOMP).eq.1)then
            call FINDFIL(LSHAD(ICOMP),XST)
            if(XST)then
              ishold = .true.
            else
              ishold = .false.
            endif
          endif
        else
          ishold = .false.  ! embedded
        endif
      endif
      if(act(1:3).eq.'ob-'.or.act(1:3).eq.'ob+')then
        if(ISIcalc.ne.1)then  ! if not embedded calculation
          if(ISI(ICOMP).eq.1)then
            call FINDFIL(LSHAD(ICOMP),XST)
            if(XST)then
              ishold = .true.
            else
              ishold = .false.
            endif
          endif
        else
          ishold = .false.  ! embedded
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
#include "help.h"
      
      integer lnblnk  ! function definition

C Passed parameters
      integer icomp   ! index of the current zone
      character act*1 ! i is initialise r is report on leaving
      logical silent  ! if true then use silent recalc

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/pmchange/comold,tmcold,vwfold,ishold,cfcold
      logical comold,tmcold,vwfold,ishold,cfcold
      integer izconstv,iztmcv
      common/znconstrv/izconstv(MCOM),iztmcv(MCOM)

      logical ok,QUIET,unixok

      helpinsub='edzone'  ! set for subroutine

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
          if(lnblnk(LTHRM(ICOMP))+32.lt.72)then
            write(h(nh),'(3a)')' construction file ',
     &      LTHRM(ICOMP)(1:lnblnk(LTHRM(ICOMP))),' out of date.'
          else
            write(h(nh),'(3a)')' constructions ',
     &      LTHRM(ICOMP)(1:lnblnk(LTHRM(ICOMP))),' out of date.'
          endif 
        endif
        if(tmcold)then
          nh=nh+1
          if(lnblnk(LTWIN(ICOMP))+23.lt.72)then
            write(h(nh),'(3a)')' TMC file ',
     &        LTWIN(ICOMP)(1:lnblnk(LTWIN(ICOMP))),' out of date.'
          else
            write(h(nh),'(3a)')' TMC ',
     &        LTWIN(ICOMP)(1:lnblnk(LTWIN(ICOMP))),' out of date.'
          endif
        endif
        if(cfcold)then
          nh=nh+1
          if(lnblnk(lcfcin(ICOMP))+23.lt.72)then
            write(h(nh),'(3a)')' CFC file ',
     &        lcfcin(ICOMP)(1:lnblnk(lcfcin(ICOMP))),' out of date.'
          else
            write(h(nh),'(3a)')' CFC ',
     &        lcfcin(ICOMP)(1:lnblnk(lcfcin(ICOMP))),' out of date.'
          endif
        endif
        if(vwfold)then
          nh=nh+1
          if(lnblnk(LVIEW(ICOMP))+31.lt.72)then
            write(h(nh),'(3a)')' view factor file ',
     &        LVIEW(ICOMP)(1:lnblnk(LVIEW(ICOMP))),' out of date.'
          else
            write(h(nh),'(3a)')' view factor ',
     &        LVIEW(ICOMP)(1:lnblnk(LVIEW(ICOMP))),' out of date.'
          endif
          nh=nh+1
          h(nh)=' (recalculate via `Options viewfactors`)'
        endif
        if(ishold)then
          nh=nh+1
          write(h(nh),'(3a)')' shading db ',
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
          helptopic='change_for_shading'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKABC('Reflect zone change in shading?','Options:',
     &   'recalculate (silent)','recalculate (interactive)','cancel',
     &    IRT,nbhelp)
        endif
        if(IRT.eq.1.or.IRT.eq.2)then
          if(IRT.eq.1) call edisp(iuout,
     & 'When finished control will be returned to the project manager.')

C If existing db out of date the perform recalculation.
          call tstamp('>','PRJ: start shading recalculation')
          if(IRT.eq.1)then
            call comissionish(icomp,'sra',ier)
            if(ier.eq.0)ishold=.false.
          else
            call comissionish(icomp,'ira',ier)
            if(ier.eq.0)ishold=.false.
          endif
        endif
      endif
      if(comold.or.tmcold.or.cfcold)then
        if(silent)then
          ok= .true.
        else
          helptopic='change_for_constructions'
          call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKOK(' ',
     &    'Update zone construction, TMC and/or CFC files?',
     &    OK,nbhelp)
        endif
        if(OK)then

C If current version of construction file continue with the
C rebuild otherwise confirm with user whether to use legacy
C format with seaparte tmc file.
          QUIET=.true.
          itru=iuout
          if(izconstv(ICOMP).eq.21)then
            continue
          else
            CALL EASKAB(' ','Save zone constructions:',
     &        'via legacy format with separate tmc file',
     &        'current format with optical data',IWB,nbhelp)
            if(IWB.eq.1)then
              izconstv(ICOMP)=0
              iztmcv(ICOMP)=0
            else
              izconstv(ICOMP)=21
              iztmcv(ICOMP)=21
            endif
          endif
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
C     = 'sab'  silent conversion ASCII to binary
C     = 'sua'  silent useupdate_silent mode
      subroutine comissionish(iz,act,ier)
#include "building.h"
#include "model.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN,IEOUT
      integer childterminal  ! picks up mmod from starting of prj
      common/childt/childterminal
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/appw/iappw,iappx,iappy
      integer iappw,iappx,iappy
      integer iappwpc ! application %

C Directives for shading and/or insolation.
      common/ishdirec/iaplic(MCOM,2),nsurfcalc(MCOM),lstsfcalc(MCOM,MS),
     &     nsurfinso(MCOM),isurfinso(MCOM,MS)

      CHARACTER ZN*12,longtfile*144,longtfiledos*144
      character TMODE*8,outs*248,DOIT*248
      character shdafile*72
      character act*3  ! the parameter passed
      logical XST,XSTA,concat,unixok
      logical shdasked,ishasked

C Check if Unix-based or DOS based.
      call isunix(unixok)
      shdasked=.true.; ishasked=.true.

      if(iz.eq.0.or.iz.gt.ncomp)then
        ier=1
        return  ! do not do if iz is zero
      endif

      if(ISI(iz).eq.0)then
        ier=2
        return  ! do not do if ISI has not been setup yet
      endif

C Confirm that zone shading directives have been defined. Use
C similar logic to that used in edgeo.F for the interface.
      if(nsurfcalc(iz).gt.0)then
        if(iaplic(iz,1).eq.0)then
          shdasked=.true.
        else
          shdasked=.true.
        endif
      else
        if(IOBS(iz).eq.0)then
          shdasked=.false.
        elseif(IOBS(iz).eq.2)then
          shdasked=.false.
        else
          shdasked=.false.
        endif
      endif
      if(nsurfinso(iz).gt.0)then
        if(iaplic(iz,2).eq.0)then
          ishasked=.true.
        else
          ishasked=.true.
        endif
      else
        ishasked=.false.
      endif
      if((.NOT.shdasked).and.(.NOT.ishasked))then
        ier=3
        call edisp(iuout,
     &  'No shading or insolation directives set for this zone.')
        call edisp(iuout,
     &  'Return to the zone geometry facility and set these.')
        return  ! do not do if ISI has not been setup yet
      endif

C Check if file exists and get local zone name for feedback.
      call FINDFIL(LSHAD(iz),XST)
      write(shdafile,'(2a)') lshad(iz)(1:lnblnk(lshad(iz))),'a'
      call FINDFIL(shdafile,XSTA)
      write(ZN,'(A)') zname(iz)
      LNZ=max(1,lnzname(iz))  ! find current lenght of at least on character

C Clear command and setup terminal type.
      doit = ' '
      call terminalmode(childterminal,tmode)

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

C Get application % nopminal size.
      if(iappw.eq.690)then
        iappwpc=100
      else
        iappwpc=nint(100.0*(real(iappw)/690.0))  ! reconstitute %
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
          ier=0
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
        ier=0

      elseif(act(1:3).eq.'ir ')then

C Interactive recalculate of zone shading if there is an existing
C shading file. If prj initial size is a % of default pass this\
C on to child with an offset from prj start position.
        if(XST)then
          if(iappwpc.gt.0.and.iappwpc.le.200)then
            write(doit,'(3a,3i4,5a)') 'ish -mode ',
     &          tmode,' -s ',iappwpc,iappx+15,iappy+50,' -file ',
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
          ier=0
        else
          ier=3
          return  ! if shading file does exist
        endif
      elseif(act(1:3).eq.'ira')then

C Interactive recalculate of zone shading without checking if
C a shading file exists. If prj initial size is a % of default
C pass this on to child with an offset from prj start position.
        if(iappwpc.gt.0.and.iappwpc.le.200)then
          write(doit,'(3a,3i4,5a)') 'ish -mode ',
     &        tmode,' -s ',iappwpc,iappx+15,iappy+50,' -file ',
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
        ier=0

      elseif(act(1:3).eq.'in ')then

C Interactive shading calculation for a new shading database.
        if(iappwpc.gt.0.and.iappwpc.le.200)then
          write(doit,'(3a,3i4,4a)') 'ish -mode ',tmode,
     &      ' -s ',iappwpc,iappx+15,iappy+50,' -file ',
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
        ier=0

      elseif(act(1:3).eq.'sab')then

C Convert ASCII shading to binary but only if an ASCII file is
C found.
        if(XSTA)then
          write(doit,'(6a)') 'ish -mode text -file ',
     &      longtfile(1:lnblnk(longtfile)),' -zone ',
     &      ZN(1:lnz),' -act asci2bin ',shdafile(1:lnblnk(shdafile))
          call usrmsg('starting shading conversion via',doit,'-')

C Since silent, tell runit to use the current command window
C rather than starting up a new xterm.
          call runit(doit,'-')
          write(outs,'(3a)') 'Shading for ',ZN(1:lnz),
     &      ' has been imported from ASCII.'
          call edisp(iuout,outs)
        endif
        ier=0

      elseif(act(1:3).eq.'sua')then

C Silent useupdate_silent mode to convert ASCII if available or
C to recalculate whether or not there is an existing shading
C file.
        write(doit,'(5a)') 'ish -mode text -file ',
     &    longtfile(1:lnblnk(longtfile)),' -zone ',
     &    ZN(1:lnz),' -act useupdate_silent'
        call usrmsg('starting shading analysis via',doit,'-')

C Since silent, tell runit to use the current command window
C rather than starting up a new xterm.
        call runit(doit,'-')
        write(outs,'(3a)') 'Shading for ',ZN(1:lnz),
     &      ' has been converted or recalculated.'
        call edisp(iuout,outs)
        ier=0

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

C The original C code returned the square of distance
C so unpack via sqrt call.  The 0.00001 below should protect
C from stressing SQRT in cases of actual distances of a few mm.
        d2l=(vdis1-(vdis+vdis1-vdis2)*
     &      (vdis+vdis1-vdis2)/vdis/4.0)
        if(abs(d2l).lt.0.00001)then
          offset=d2l   ! if small d2l 
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
