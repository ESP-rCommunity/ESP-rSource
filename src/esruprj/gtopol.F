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

C Contents:
C  GTOPOL - interface to ground topology 
C  CROWDD - shortest distance between two points
C  TRIANG - creates surface descriptions from a list of vertices
C  CIRCUM - calculates the circumcenter and radius of a triangle
C  INPOLY - Polygon point containment test.

C ****** GTOPOL
      SUBROUTINE GTOPOL(itrc,ier)
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)
      COMMON/GT/GTNAME
      COMMON/GTFIL/GTGEOM
      COMMON/GT5/GSNAME(MGRT),GMLCN(MGRT)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/C6/INDCFG
      
      CHARACTER*33 ITEMS(12)
      character*32 VERTS(MGRT)
      dimension ivals(MGRT)
      CHARACTER GMLCN*12,GSNAME*6,GTNAME*15,GTGEOM*72
      CHARACTER outs*124,ltmp*72,t15*15,ETEXT*82
      logical XST,MODIFY,MODLEN,MODBND,grmod,ok,dok
      integer nitms,INO ! max items and current menu item

#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='GTOPOL'  ! set for subroutine

      grmod=.true.

C Read ground file, if not found use data.
      IFILG=IFIL+1
      helptopic='ground_form_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)
      ltmp=gtgeom
 46   CALL EASKS(ltmp,' Ground topology/geometry file name ? ',
     &      ' ',72,'grnd.geo','ground geom file',IER,nbhelp)
      if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
        gtgeom=ltmp
      else
        goto 46
      endif
      INQUIRE (FILE=gtgeom,EXIST=XST)
      if(XST)then
        call EGRNIN(IFILG,gtgeom,ITRC,iuout,IER)
        if(NGT.eq.0.and.NGTV.gt.0)then
          call triang(ier)
        endif
      else

C Clear the commons.
        NGT=0
        NGTV=0
        do 38 ijj=1,MGTV
          XGT(ijj)=0.0
          YGT(ijj)=0.0
          ZGT(ijj)=0.0
  38    continue
        do 37 ijj=1,MGRT
          NGVER(ijj)=0
          IVEDGE(ijj)=0
          JGVN(ijj,1)=0
          JGVN(ijj,2)=0
          JGVN(ijj,3)=0
          JGVN(ijj,4)=0
          IF(ijj.LE.9)WRITE(GSNAME(ijj),'(a,i1)')'gt-',ijj
          IF(ijj.GT.9.and.I.LE.99)WRITE(GSNAME(ijj),'(a,i2)')'gt-',ijj
          IF(ijj.GT.99)WRITE(GSNAME(ijj),'(a,i3)')'gt-',ijj
          GMLCN(ijj)='UNKNOWN'
  37    continue
  39    t15=GTNAME
        CALL EASKS(t15,' Description of the ground?',
     &        ' ',15,'flat','not flat',IER,nbhelp)
        if(t15(1:2).ne.'  '.and.t15(1:4).ne.'UNKN')then
          GTNAME=t15
        else
          goto 39
        endif
        call usrmsg('....... ','........ ','W')
      endif

C Set to display vertex numbers and only the ground surfaces as
C well as the surface names.
      ITVNO=0
      ITDSP=6
      ITSNM=0

C Begin with menu.
    3 INO=-4
      WRITE(ITEMS(1),'(A,A)')  'a description: ',GTNAME(1:15)
      WRITE(ITEMS(2),'(A,2i4)')'  verts & surfs: ',NGTV,NGT
      ITEMS(3)=                'b vertex coordinates             '
      ITEMS(4)=                'c surface topology               '
      ITEMS(5)=                'd surface attributes             '
      ITEMS(6)=                '  ___________________________    '
      ITEMS(7)=                '* auto triangulate               '
      ITEMS(8)=                '# rotate/ transform ground       '
      ITEMS(9)=                '> save                           '
      ITEMS(10)=               '! list data                      '
      ITEMS(11)=               '? help                           '
      ITEMS(12)=               '- exit this menu                 '
      nitms = 12

C If ground needs drawing or updating do it here.
      if(MMOD.ge.8.and.grmod)then
        MODIFY=.TRUE.
        MODBND=.TRUE.
        MODLEN=.TRUE.
        if(ITDSP.le.5)then
          nzg=NCOMP
          DO 44 I=1,nzg
            nznog(I)=I
  44      CONTINUE
          CALL INLNST(1)
        else
          nzg=0
        endif
        izgfoc=0
        CALL ADJVIEW(IER)
        call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
        call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
        WRITE(ETEXT,'(2A)')'Model: ',modeltitle(1:lnblnk(modeltitle))
        iside=1
        isize=1
        ifont=1
        call viewtext(etext,iside,isize,ifont)
        CALL INLNSTG(3)
        call EGRNDR(IER)
        grmod=.false.
      endif

C Help text for this menu.
      helptopic='ground_form_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('Ground Topology',ITEMS,nitms,INO)
      IF(INO.EQ.nitms)THEN
        RETURN
      ELSEIF(INO.EQ.nitms-1)THEN

C List help.
        helptopic='ground_form_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('grnd topo section',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.nitms-2)THEN
        call edisp(iuout,'Current list of vertices.... ')
        DO 335, I=1, NGTV
          write(outs,'(a,i3,3F8.3)')' verts ',I ,XGT(I),YGT(I),ZGT(I)
          call edisp(iuout,outs)
 335    CONTINUE
        call gtinfo(iuout)
      ELSEIF(INO.EQ.1)THEN
        t15=GTNAME
        CALL EASKS(t15,' Description of the ground?',
     &        ' ',15,'flat','not flat',IER,nbhelp)
        if(t15(1:2).ne.'  '.and.t15(1:4).ne.'UNKN')then
          GTNAME=t15
        endif
      ELSEIF(INO.EQ.3)THEN

C Edit, and manipulate vertices and support transforms.
        call EDGVERT(grmod,IER)
        if(grmod)MODIFY=.TRUE.
      ELSEIF(INO.EQ.4)THEN

C Edit surface list.
        call edgvlist(itrc,ier)
        MODIFY=.TRUE.
        grmod=.true.
      ELSEIF(INO.EQ.5)THEN

C Edit surface attributes.
        CALL EPMENSV
        CALL EASKGSUR(IS,'M','Select surface(s) to attribute.',' ',IER)
        CALL EPMENRC
        if(is.ge.1)then

C Now edit the composition.
          write(outs,'(3a)') 'Select construction for ',
     &    GSNAME(IS),' from the list (or exit to leave unknown).'
          CALL EPMENSV
          CALL EPKMLC(ISEL,outs,'(or exit to leave unknown)',IERR)
          CALL EPMENRC
          IF(ISEL.GT.0.and.ierr.eq.0)then
            WRITE(GMLCN(IS),'(A12)')DESC(ISEL)(1:12)
          else
            if(GMLCN(IS)(1:1).eq.' ')GMLCN(IS)='UNKNOWN'
          endif
          MODIFY=.TRUE.
          grmod=.true.
        elseif(is.lt.0)then
          write(outs,'(2a)') 'Select construction attribute ',
     &      ' from the list (or exit to leave unknown).'
          CALL EPMENSV
          CALL EPKMLC(ISEL,outs,'(or exit to leave unknown)',IERR)
          CALL EPMENRC
          IF(ISEL.GT.0.and.ierr.eq.0)then
            DO 10 L=1,NGT
              WRITE(VERTS(L),'(a,1x,a)')GSNAME(L),GMLCN(L)
  10        continue
            INPICK=NGT
            CALL EPICKS(INPICK,IVALS,' ',' ',32,NGT,VERTS,
     &        'Surface  composition',IER,nbhelp)
            do 11 i=1,inpick
              iss = ivals(i)
              WRITE(GMLCN(ISS),'(A12)')DESC(ISEL)(1:12)
  11        continue
            MODIFY=.TRUE.
            grmod=.true.
          endif
        endif
      ELSEIF(INO.EQ.7)THEN

C Check and see if a few vertices have had edge definitions.
        ienough=0
        DO 61 I=1,NGTV
          if(IVEDGE(I).eq.1)ienough=ienough+1
  61    continue
        if(ienough.lt.5)then
          call usrmsg(
     &      'Less than 5 of the vertices are marked as being on a',
     &      'boundary/edge. Triangulation will probably fail.','W')
        else

C Set to draw only ground and then after figuring out triangles 
C swop back to previous.
          if(NGT.gt.1)then
            dok=.true.
            call askok('Existing surfaces will be removed prior to',
     &        'auto triangulate. Is this ok?',ok,dok,nbhelp)
          else
            ok=.true.
          endif
          if(ok)then
            LITDSP=ITDSP
            ITDSP=6
            LITVNO=ITVNO
            ITVNO=0
            LITORG=ITORG
            ITORG=1
            LITGRD=ITGRD
            ITGRD=1
            CALL TRIANG(ier)
            ITDSP=LITDSP
            ITVNO=LITVNO
            ITORG=LITORG
            ITGRD=LITGRD
            MODIFY=.TRUE.
            grmod=.true.
          endif
        endif
      ELSEIF(INO.EQ.8)THEN

C Rotate ground.
        CALL EASKAB(' Adjust location of ground:',' ',
     &    'rotate ground','transform X,Y or Z',IW,nbhelp)
        if(IW.eq.1)then
          VAL=0.
          CALL EASKR(VAL,' ',' Rotation (deg + = anticlockwise) ? ',
     &      -359.0,'W',359.0,'W',0.0,'rotation',IER,nbhelp)
          if(VAL.LT.-.01.OR.VAL.GT..01)then

C Rotation choices.
            CALL EASKABC(' Rotation choices :',' ','Site origin',
     &        'User specified X & Y','cancel  ?',IRT,nbhelp)
            if(IRT.eq.1)then
              CALL EGTROT(VAL,0.,0.)
            elseif(IRT.EQ.2)THEN
              x1=0.
              CALL EASKR(x1,' ',' X point (metres) ? ',
     &          0.0,'-',0.0,'-',0.0,'x point',IER,nbhelp)
              y1=0.
              CALL EASKR(y1,' ',' Y point (metres) ? ',
     &          0.0,'-',0.0,'-',0.0,'y point',IER,nbhelp)
              CALL EGTROT(VAL,x1,y1)
            endif
          endif
        elseif(IW.eq.2)then
          VALX=0.
          CALL EASKR(VALX,' ',' X shift (metres) ? ',
     &           -50.0,'W',50.0,'W',0.0,'x shift',IER,nbhelp)
          VALY=0.
          CALL EASKR(VALY,' ',' Y shift (metres) ? ',
     &           -50.0,'W',50.0,'W',0.0,'y shift',IER,nbhelp)
          VALZ=0.
          CALL EASKR(VALZ,' ',' Z shift (metres) ? ',
     &           -50.0,'W',50.0,'W',0.0,'y shift',IER,nbhelp)
          DO 62 I=1,NGTV
            XGT(I)=XGT(I)+VALX
            YGT(I)=YGT(I)+VALY
            ZGT(I)=ZGT(I)+VALZ
   62     continue
        endif
        MODIFY=.TRUE.
        grmod=.true.
      ELSEIF(INO.EQ.9)THEN

C Save ground file.
        helptopic='ground_form_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        ltmp=gtgeom
        CALL EASKS(ltmp,' Ground topology/geometry file name ? ',
     &        ' ',72,'grnd.geo','ground geom file',IER,nbhelp)
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          gtgeom=ltmp
        endif
        IFILG=IFIL+1
        call EGROUT(IFILG,gtgeom,iuout,IER)
        CALL EMKCFG('-',IER)
      ELSE
        GOTO 3
      ENDIF
      GOTO 3

      end

C ****** EDGVERT
C Edit vertex attributes in common block G1 via a paging menu.
C IER=0 OK, IER=1 problem.
      SUBROUTINE EDGVERT(MODGEO,IER)
#include "epara.h"
#include "building.h"
#include "prj3dv.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      COMMON/GTFIL/GTGEOM
      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)
      COMMON/GT5/GSNAME(MGRT),GMLCN(MGRT)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY6G/LINSTYG(MGRT)
      LOGICAL MODIFY,MODLEN,MODBND,MODGEO,ok,dok,match,matchver

      DIMENSION VERT(35),IVLST(MGTV),ITEMP(MGTV),COG1(3),COG2(3),COG3(3)
      CHARACTER VERT*35,KEY*1,HOLD*32,GTGEOM*72
      character outs*124,headv*48,GMLCN*12,GSNAME*6
      integer IRT  ! for radio button
      integer MVERT,IVERT ! max items and current menu item

#ifdef OSI
      integer iix,iiy
#else
      integer*8 iix,iiy
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDGVERT'  ! set for subroutine

C Initial values for vertex at angle facility.
      V1=1.0
      AZ=0.0
      EL=0.0

C Initialise zone vertex menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
      MHEAD=1
      MCTL=6
      ILEN=NGTV
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      ILEN=NGTV
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
    3 M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          if(ivedge(L).eq.1)then
            WRITE(VERT(M),'(a1,i3,3F9.3,a)')KEY,L,XGT(L),YGT(L),ZGT(L),
     &        ' *'
          else
            WRITE(VERT(M),'(a1,i3,3F9.3)')KEY,L,XGT(L),YGT(L),ZGT(L)
          endif
        ENDIF
   10 CONTINUE

      VERT(1)    =' Index|X coord |Y coord |Z coord E'

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        VERT(M+1)='  ______________________________ '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' --------')
      ENDIF
      VERT(M+2)  ='+ add/delete/copy/replicate/edit '
      VERT(M+3)  ='^ transforms                     '
      VERT(M+4)  ='! mark edge vertices             '
      VERT(M+5)  ='? help                           '
      VERT(M+6)  ='- exit                           '

C Update display.
      if(MMOD.ge.8.and.MODIFY)then
        CALL INLNST(1)
        ITVNO=0
        izgfoc=0
        CALL ADJVIEW(IER)
        CALL INLNSTG(3)
        call EGRNDR(IER)
        MODIFY=.false.
      endif

C Help text for this menu.
      helptopic='ground_form_verticies'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      CALL EMENU(' Ground Vertices',VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN
        if(MODGEO)then
          IFILG=IFIL+1
          call EGROUT(IFILG,gtgeom,iuout,IER)
          CALL EMKCFG('-',IER)
        endif
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C List help.
        helptopic='ground_form_verticies'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('configuration section',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C Mark vertices which act as edges.
        inpick=NGTV
        CALL EPMENSV
        call EPKGVERT(INPICK,IVLST,'Vertices at edge',
     &    'Select vertices at edge of ground...',' ',nbhelp,ier)
        CALL EPMENRC
        if(inpick.gt.0)then
          do 44 ij=1,inpick
            iwhich1=IVLST(ij)
            ivedge(iwhich1)=1
  44      continue
        endif
        goto 3
      ELSEIF(IVERT.EQ.(MVERT-3))THEN
        helptopic='ground_form_verticies'
        call gethelptext(helpinsub,helptopic,nbhelp)
        idno=2
        call MENUATOL('available vertex transforms (see help):',
     &   ' Vertex transform options ','a vertices along a line',
     &   'b distance between two vertices',
     &   'c vertex @ angle & distance',
     &   'd angle between two lines','e -',
     &   'f distance from vertex to a line',
     &   'g align vertex with a line','h find close vertices',
     &   'i move vetices along a line',' ',' ',' ',ino,idno,nbhelp)
        if(ino.eq.0.or.ino.eq.10)then
          continue
        elseif(ino.eq.1)then

C Vertices along a line between two verts.
          inpick=2
          CALL EPMENSV
          call EPKGVERT(INPICK,IVLST,'Vertices for *---x--*',
     &      'Select two vertices to define a line...',' ',nbhelp,ier)
          CALL EPMENRC
  72      if(inpick.eq.2)then
            iwhich1=IVLST(1)
            iwhich2=IVLST(2)
            tdis= crowxyz(XGT(IWHICH1),YGT(IWHICH1),ZGT(IWHICH1),
     &        XGT(IWHICH2),YGT(IWHICH2),ZGT(IWHICH2))
            write(outs,'(a,i3,a,i3,a,f9.4)') ' Distance between v ',
     &        IWHICH1,' & v ',IWHICH2,' =',tdis
            call edisp(iuout,outs)

            CALL EASKR(vdis,' Distance along the line (metres): ',
     &        ' : ',0.001,'F',99.999,'W',0.1,'dist along line',
     &        IER,nbhelp)

C Use ratio calculation.
            r2 = tdis - vdis
            r1 = vdis
            x3 = ((r2 * XGT(IWHICH1)) + (r1 * XGT(IWHICH2)))/tdis
            y3 = ((r2 * YGT(IWHICH1)) + (r1 * YGT(IWHICH2)))/tdis
            z3 = ((r2 * ZGT(IWHICH1)) + (r1 * ZGT(IWHICH2)))/tdis
  
            write(outs,'(a,3f10.4)') ' Vertex is at X,Y,Z:',x3,y3,z3
            call edisp(iuout,outs)

C Show the new point on the current wireframe.
            if(MMOD.ge.8)then
              COG1(1)=x3
              COG1(2)=y3
              COG1(3)=z3
              CALL VECTRN(COG1,TEMAT,COG2,IER)
              CALL VECTRN(COG2,ETSMAT,COG3,IER)
              call u2pixel(COG3(1),COG3(2),iix,iiy)
              call ecirc(iix,iiy,3,1)
              call forceflush()
            endif
            if(NGTV.lt.MGTV)then
              write(outs,'(a,i3,a)') 'Make this (',NGTV+1,') a:'
              CALL EASKABC(outs,' ','new vertex',
     &          'new vertex after editing','cancel',INVT,nbhelp)
              if(INVT.eq.1)then
                NGTV=NGTV+1
                xgt(ngtv)=x3
                ygt(ngtv)=y3
                zgt(ngtv)=z3
              elseif(INVT.eq.2)then
                NGTV=NGTV+1
                WRITE(HOLD,'(1x,3f10.5)')x3,y3,z3
                write(outs,'(a,i3,a)')' Vertex (',ngtv,') X  Y  Z (m):'
 443            CALL EASKS(HOLD,outs,' ',32,' 0. 0. 0. ','vtx co',
     &            IER,nbhelp)
                K=0
                CALL EGETWR(HOLD,K,XGT(ngtv),-999.,999.,'W','X cd',IER)
                CALL EGETWR(HOLD,K,YGT(ngtv),-999.,999.,'W','Y cd',IER)
                CALL EGETWR(HOLD,K,ZGT(ngtv),-9.9,999.9,'W','Z cd',IER)
                if(ier.ne.0)goto 443
              elseif(INVT.eq.3)then
                continue
              endif
              ILEN=NGTV
              IPACT=CREATE
              CALL EKPAGE(IPACT)
              MODGEO=.TRUE.
              MODIFY=.TRUE.
              MODBND=.TRUE.
              call usrmsg(' ',' ','-')
              if(MMOD.ge.8)then
                CALL INLNST(1)
                izgfoc=0
                CALL ADJVIEW(IER)
                call EGRNDR(IER)
              endif

C Check to see if this new vertex should be included in any existing edges.
              if(invt.eq.1.or.invt.eq.2)then
                iwhich3=NGTV
                do 246 ivj=1,NGT
                  ivjlimit=NGVER(ivj)
                  do 247 ivjj=1,ivjlimit
                    if(ivjj.eq.ivjlimit)then
                      iwhich1=JGVN(ivj,ivjj)
                      iwhich2=JGVN(ivj,1)
                    else
                      iwhich1=JGVN(ivj,ivjj)
                      iwhich2=JGVN(ivj,ivjj+1)
                    endif

C Report length of line. Use method of Ward/Radiance in fvect.c
                    call pointtogline(iwhich3,iwhich1,iwhich2,offset,
     &                matchver)
                    if(.NOT.matchver) goto 247
                    if(offset.lt.0.003)then
                      write(outs,'(a,i3,a,3f8.3,a,f6.4,a,i3,a,i3,2a)')
     &                  'New vertex ',iwhich3,' @',XGT(NGTV),YGT(NGTV),
     &                  ZGT(NGTV),' is close (',offset,') to edge ',
     &                  IWHICH1,' &',IWHICH2,' of surface ',
     &                  GSNAME(ivj)
                      call edisp(iuout,outs)

C If current surface (ivj) can take another vertex expand the
C list. Logic works by looping down (from one more than the current
C number of vertices associated with this surface) shifting
C JGVN indices up one until at the current edge (ivjj) and then
C inserting the new vertex index.
                      if(NGVER(ivj)+1.le.8)then
                        dok=.true.
                        helptopic='ground_coord_very_close'
                        call gethelptext(helpinsub,helptopic,nbhelp)
                        call askok('Insert this vertex?','(see help)',
     &                    ok,dok,nbhelp)
                      else
                        ok=.false.
                      endif
                      if(ok)then
                        NGVER(ivj)=NGVER(ivj)+1
                        IXV=NGVER(ivj)+1
  148                   continue
                        IXV=IXV-1
                        JGVN(ivj,IXV)=JGVN(ivj,IXV-1)
                        IF(IXV.GT.ivjj+1)GOTO 148
                        JGVN(ivj,ivjj+1)=iwhich3

C Debug.
C                       write(6,'(a,30i4)') 'now jgvn is ',
C     &                  (JGVN(ivj,ii),ii=1,NGVER(ivj))

C Surface (ivj) vertex list has been updated. Go on to next surface.
                        goto 246 
                      endif
                    endif
  247             continue
  246           continue
              endif
            endif
            dok=.false.
            call forceflush()
            call askok('Another vertex along the line? ',' ',
     &        ok,dok,nbhelp)
            if(ok)goto 72
          else
            goto 3
          endif
        elseif(ino.eq.2)then

C Distance between two vertices.
          inpick=2
          CALL EPMENSV
          call EPKGVERT(INPICK,IVLST,'Vertices for *<--?-->*',
     &      'Select two vertices...',' ',nbhelp,ier)
          CALL EPMENRC
          if(inpick.eq.2)then
            iwhich1=IVLST(1)
            iwhich2=IVLST(2)
            vdis= crowxyz(XGT(IWHICH1),YGT(IWHICH1),ZGT(IWHICH1),
     &        XGT(IWHICH2),YGT(IWHICH2),ZGT(IWHICH2))
            call ln2az(XGT(IWHICH1),YGT(IWHICH1),ZGT(IWHICH1),
     &        XGT(IWHICH2),YGT(IWHICH2),ZGT(IWHICH2),az,el)
            write(outs,'(a,i3,a,i3,a,f9.4,a,f8.3,a,f7.3)')
     &        ' Distance between v ',IWHICH1,' & v ',IWHICH2,' =',vdis,
     &        ' @ aimuth ',az,' & elev ',el
            call edisp(iuout,outs)
          endif
        elseif(ino.eq.3)then
          inpick=1
          CALL EPMENSV
          call EPKGVERT(INPICK,IVLST,'Vertex for origin',
     &      'Select vertex to act as origin...',' ',nbhelp,ier)
          CALL EPMENRC
  73      if(inpick.eq.1)then
            iwhich=IVLST(1)

C Present distance, azimuth and elevation then parse data from HOLD.
            write(hold,'(f10.4,f9.3,f8.3)') V1,AZ,EL
 342        CALL EASKS(HOLD,
     &      'Distance (m), azimuth (north=0, east=90), elev (vert=90):',
     &      ' ',32,' 1. 0. 0. ','dist azim elev',IER,nbhelp)
            K=0
            CALL EGETWR(HOLD,K,V1,-999.9,999.9,'W','dist',IER)
            CALL EGETWR(HOLD,K,AZ,-359.9,359.9,'W','azim',IER)
            CALL EGETWR(HOLD,K,EL,-90.0,90.0,'W','elev',IER)
            if(ier.ne.0)goto 342
            PI = 4.0 * ATAN(1.0)
            RAD = PI/180.
            RYAZI = AZ*RAD
            RSALT = EL*RAD
            z3 = V1*SIN(RSALT)
            XYDIS = V1*COS(RSALT)
            IF (XYDIS .LT. 1E-6)THEN
              x3 = 0.
              y3 = 0.
            ELSE
              x3 = XYDIS*SIN(RYAZI)
              y3 = XYDIS*COS(RYAZI)
            ENDIF
            write(outs,'(a,3f10.4)') ' Vertex @ X,Y,Z:',x3+XGT(IWHICH),
     &        y3+YGT(IWHICH),z3+ZGT(IWHICH)
            call edisp(iuout,outs)
            x3=x3+XGT(IWHICH)
            y3=y3+YGT(IWHICH)
            z3=z3+ZGT(IWHICH)

C Show the new point on the current wireframe.
            if(MMOD.ge.8)then
              COG1(1)=x3
              COG1(2)=y3
              COG1(3)=z3
              CALL VECTRN(COG1,TEMAT,COG2,IER)
              CALL VECTRN(COG2,ETSMAT,COG3,IER)
              call u2pixel(COG3(1),COG3(2),iix,iiy)
              call ecirc(iix,iiy,3,1)
              call forceflush()
            endif
            if(NGTV.lt.MGTV)then
              write(outs,'(a,i3,a)') 'Make this (',NGTV+1,') a:'
              CALL EASKABC(outs,' ','new vertex',
     &          'new vertex after editing','cancel',INVT,nbhelp)
              if(INVT.eq.1)then
                NGTV=NGTV+1
                xgt(ngtv)=x3
                ygt(ngtv)=y3
                zgt(ngtv)=z3
              elseif(INVT.eq.2)then
                NGTV=NGTV+1
                WRITE(HOLD,'(1x,3f10.5)')x3,y3,z3
                write(outs,'(a,i3,a)')' Vertex (',ngtv,') X  Y  Z (m):'
 543            CALL EASKS(HOLD,outs,' ',32,' 0. 0. 0. ','vtx co',
     &            IER,nbhelp)
                K=0
                CALL EGETWR(HOLD,K,XGT(ngtv),-999.,999.,'W','X cd',IER)
                CALL EGETWR(HOLD,K,YGT(ngtv),-999.,999.,'W','Y cd',IER)
                CALL EGETWR(HOLD,K,ZGT(ngtv),-9.9,999.9,'W','Z cd',IER)
                if(ier.ne.0)goto 543
              elseif(INVT.eq.3)then
                continue
              endif
              ILEN=NGTV
              IPACT=CREATE
              CALL EKPAGE(IPACT)
              MODGEO=.TRUE.
              MODIFY=.TRUE.
              MODBND=.TRUE.
              call usrmsg(' ',' ','-')
              if(MMOD.ge.8)then
                CALL ADJVIEW(IER)
                CALL INLNSTG(3)
                call EGRNDR(IER)
              endif
            endif
            dok=.false.
            call askok('Another vertex from this origin?',' ',
     &        ok,dok,nbhelp)
            if(ok)goto 73
          endif
          MODIFY=.true.
          if(MMOD.ge.8)then
            CALL ADJVIEW(IER)
            CALL INLNSTG(3)
            call EGRNDR(IER)
          endif
        elseif(ino.eq.4)then

C Angle between two lines.
          inpick=3
          CALL EPMENSV
          call EPKGVERT(INPICK,IVLST,'Angle between 3 vertices',
     &      'Select three vertices (see help for rules).',' ',
     &      nbhelp,ier)
          CALL EPMENRC
          if(inpick.eq.3)then
            iwhich1=IVLST(1)
            iwhich2=IVLST(2)
            iwhich3=IVLST(3)
            call ang3vtx(XGT(IWHICH1),YGT(IWHICH1),ZGT(IWHICH1),
     &        XGT(IWHICH2),YGT(IWHICH2),ZGT(IWHICH2),XGT(IWHICH3),
     &        YGT(IWHICH3),ZGT(IWHICH3),ang3)
            write(outs,'(a,3i3,a,f9.4)') ' Angle between verts ',
     &        iwhich1,iwhich2,iwhich3,' is ',ang3
            call edisp(iuout,outs)
          endif
          MODIFY=.true.
        elseif(ino.eq.5)then
          continue
        elseif(ino.eq.6)then

C Distance between point and a line.
          inpick=2
          CALL EPMENSV
          call EPKGVERT(INPICK,IVLST,'Vertices defining line',
     &      'Select two vertices...',' ',nbhelp,ier)
          CALL EPMENRC
          if(inpick.eq.2)then
            iwhich1=IVLST(1)
            iwhich2=IVLST(2)
          endif
          inpick=1
          CALL EPMENSV
          call EPKGVERT(INPICK,IVLST,'Vertex to test',
     &      'Select a vertex...',' ',nbhelp,ier)
          CALL EPMENRC
          if(inpick.eq.1)iwhich3=IVLST(1)

C Report length of line. Use method of Ward/Radiance in fvect.c
          call pointtogline(iwhich3,iwhich1,iwhich2,offset,match)
          if(.NOT.match) goto 3
          write(outs,'(a,i3,a,3f8.3,a,f6.4,a,i3,a,i3)')
     &     'Vertex ',iwhich3,' @',XGT(iwhich3),YGT(iwhich3),
     &     ZGT(iwhich3),' is ',offset,'(m) to edge ',IWHICH1,' &',
     &     IWHICH2
           call edisp(iuout,outs)
          MODIFY=.TRUE.
          MODBND=.TRUE.
          call usrmsg(' ',' ','-')
        elseif(ino.eq.7)then

C Bring a point off a line into alignment. Begin by checking the
C distance between point and the line.
          inpick=2
          CALL EPMENSV
          call EPKGVERT(INPICK,IVLST,'Vertices defining line',
     &      'Select two vertices...',' ',nbhelp,ier)
          CALL EPMENRC
          if(inpick.eq.2)then
            iwhich1=IVLST(1)
            iwhich2=IVLST(2)
          endif
          inpick=1
          CALL EPMENSV
          call EPKGVERT(INPICK,IVLST,'Vertex to align',
     &      'Select a vertex...',' ',nbhelp,ier)
          CALL EPMENRC
          if(inpick.eq.1)iwhich3=IVLST(1)

C Report length of line. Use method of Ward/Radiance in fvect.c
          call pointtogline(iwhich3,iwhich1,iwhich2,offset,match)
          if(.NOT.match) goto 3
          write(outs,'(a,i3,a,3f8.3,a,f6.4,a,i3,a,i3)')
     &     'Vertex ',iwhich3,' @',XGT(iwhich3),YGT(iwhich3),
     &     ZGT(iwhich3),' is ',offset,'(m) to edge ',IWHICH1,' &',
     &     IWHICH2
           call edisp(iuout,outs)

C If the distance is greater than 1mm get the distance between the
C start of the line (iwhich1) and the test point (iwhich3) and do this
C for the end point on the line (iwhich2) and the test point.
          if(offset.gt.0.002)then
            tdis= crowxyz(XGT(IWHICH1),YGT(IWHICH1),ZGT(IWHICH1),
     &        XGT(IWHICH2),YGT(IWHICH2),ZGT(IWHICH2))
            vdislsp=crowxyz(XGT(IWHICH1),YGT(IWHICH1),ZGT(IWHICH1),
     &        XGT(IWHICH3),YGT(IWHICH3),ZGT(IWHICH3))
            write(outs,'(a,F8.4)')' Line start -> unaligned vertex is ',
     &        vdislsp
            call edisp(iuout,outs)

C Use square root of (vdislsp^2 - offset^2) 
            aligndis = SQRT((vdislsp * vdislsp) - (offset * offset))

C Use ratio calculation to make an aligned point vdislsp along the line.
            r2 = tdis - aligndis 
            r1 = aligndis
            x3 = ((r2 * XGT(IWHICH1)) + (r1 * XGT(IWHICH2)))/tdis
            y3 = ((r2 * YGT(IWHICH1)) + (r1 * YGT(IWHICH2)))/tdis
            z3 = ((r2 * ZGT(IWHICH1)) + (r1 * ZGT(IWHICH2)))/tdis
            write(outs,'(a,3f10.4)')' Aligned vertex @ X,Y,Z:',x3,y3,z3
            call edisp(iuout,outs)
            if(MMOD.ge.8)then
              COG1(1)=x3
              COG1(2)=y3
              COG1(3)=z3
              CALL VECTRN(COG1,TEMAT,COG2,IER)
              CALL VECTRN(COG2,ETSMAT,COG3,IER)
              call u2pixel(COG3(1),COG3(2),iix,iiy)
              call ecirc(iix,iiy,3,1)
              call forceflush()
            endif
            dok=.true.
            call askok('Is this aligned vertex ok?',' ',ok,dok,nbhelp)
            if(ok)then
              XGT(IWHICH3)=x3
              YGT(IWHICH3)=y3
              ZGT(IWHICH3)=z3
              MODGEO=.TRUE.
              iwhich3=NGTV
              do 346 ivj=1,NGT
                ivjlimit=NGVER(ivj)
                do 347 ivjj=1,ivjlimit
                  if(ivjj.eq.ivjlimit)then
                    iwhich1=JGVN(ivj,ivjj)
                    iwhich2=JGVN(ivj,1)
                  else
                    iwhich1=JGVN(ivj,ivjj)
                    iwhich2=JGVN(ivj,ivjj+1)
                  endif

C Report length of line. Use method of Ward/Radiance in fvect.c
                  call pointtogline(iwhich3,iwhich1,iwhich2,offset,
     &              matchver)
                  if(.NOT.matchver) goto 347
                  if(offset.lt.0.003)then
                    write(outs,'(a,i3,a,3f8.3,a,f6.4,a,i3,a,i3,2a)')
     &                'New vertex ',iwhich3,' @',XGT(NGTV),
     &                YGT(NGTV),ZGT(NGTV),' is close (',offset,
     &                ') to edge ',IWHICH1,' &',IWHICH2,' of surface ',
     &                GSNAME(ivj)
                    call edisp(iuout,outs)

C If current surface (ivj) can take another vertex expand the
C list. Logic works by looping down (from one more than the current
C number of vertices associated with this surface) shifting
C JGVN indices up one until at the current edge (ivjj) and then
C inserting the new vertex index.
                    if(NGVER(ivj)+1.le.8)then
                      dok=.true.
                      helptopic='ground_coord_very_close'
                      call gethelptext(helpinsub,helptopic,nbhelp)
                      call askok('Insert this vertex?','(see help)',
     &                  ok,dok,nbhelp)
                    else
                      ok=.false.
                    endif
                    if(ok)then
                      NGVER(ivj)=NGVER(ivj)+1
                      IXV=NGVER(ivj)+1
  248                 continue
                      IXV=IXV-1
                      JGVN(ivj,IXV)=JGVN(ivj,IXV-1)
                      IF(IXV.GT.ivjj+1)GOTO 248
                      JGVN(ivj,ivjj+1)=iwhich3

C Debug.
C                     write(6,'(a,30i4)') 'now jgvn is ',
C     &                (JGVN(ivj,ii),ii=1,NGVER(ivj))

C Surface (ivj) vertex list has been updated. Go on to next surface.
                      goto 346 
                    endif
                  endif
  347           continue
  346         continue
            endif
          endif
        elseif(ino.eq.8)then
        
C Find close vertices.
          CALL EASKABC('Options:',' ','find any close vertices',
     &          'find closest to one vertex','cancel',INVT,nbhelp)
          if(INVT.eq.1)then
            do 102 iwhich1=1,NGTV
              write(headv,'(a,i3,a,3f10.4)') ' Vert',iwhich1,
     &          ' @ XYZ:',XGT(IWHICH1),YGT(IWHICH1),ZGT(IWHICH1)
              do 103 iwhich2=1,NGTV
                if(iwhich1.ne.iwhich2)then
                  tdis= crowxyz(XGT(IWHICH1),YGT(IWHICH1),ZGT(IWHICH1),
     &                  XGT(IWHICH2),YGT(IWHICH2),ZGT(IWHICH2))
                  if(tdis.lt.0.05)then
                  write(outs,'(2a,i3,a,f9.4,a)') headv(1:lnblnk(headv)),
     &              ' is close to vert ',IWHICH2,' (',tdis,'m).'
                  call edisp(iuout,outs)
                  endif
                endif
  103         continue
  102       continue
          elseif(INVT.eq.2)then
            inpick=1
            CALL EPMENSV
            call EPKGVERT(INPICK,IVLST,'Focus vertex',
     &        '(close vertices will be listed)',' ',nbhelp,ier)
            CALL EPMENRC
            if(inpick.eq.1)then
              iwhich1=IVLST(1)
              write(headv,'(a,i3,a,3f10.4)') ' Vert',iwhich1,
     &          ' @ XYZ:',XGT(IWHICH1),YGT(IWHICH1),ZGT(IWHICH1)
              do 101 iwhich2=1,NGTV
                if(iwhich1.ne.iwhich2)then
                  tdis= crowxyz(XGT(IWHICH1),YGT(IWHICH1),ZGT(IWHICH1),
     &                  XGT(IWHICH2),YGT(IWHICH2),ZGT(IWHICH2))
                  if(tdis.lt.0.05)then
           write(outs,'(2a,i3,a,f9.4,a,3F10.4)') headv(1:lnblnk(headv)),
     &       ' close to v ',IWHICH2,' (',tdis,'m) @ XYZ:',XGT(IWHICH2),
     &        YGT(IWHICH2),ZGT(IWHICH2)
                    call edisp(iuout,outs)
                  endif
                endif
  101         continue
            endif
          else
            continue
          endif
        elseif(ino.eq.9)then

C Move an existing vertex along an existing line.
          inpick=2
          CALL EPMENSV
          call EPKGVERT(INPICK,IVLST,'Vertices for <-x->---*',
     &      'Select two vertices along a line...',
     &      '(first one is the vertex to move)',nbhelp,ier)
          CALL EPMENRC
          if(inpick.eq.2)then
            iwhich1=IVLST(1)
            iwhich2=IVLST(2)
            tdis= crowxyz(XGT(IWHICH1),YGT(IWHICH1),ZGT(IWHICH1),
     &        XGT(IWHICH2),YGT(IWHICH2),ZGT(IWHICH2))
            write(outs,'(a,i3,a,i3,a,f9.4)') ' Distance between v ',
     &        IWHICH1,' & v ',IWHICH2,' =',tdis
            call edisp(iuout,outs)

            CALL EASKR(vdis,' Move along the line (metres): ',
     &        '(see help)',-1.0,'W',99.999,'W',0.1,'dist along line',
     &        IER,nbhelp)

C Use ratio calculation.
            r2 = tdis - vdis
            r1 = vdis
            x3 = ((r2 * XGT(IWHICH1)) + (r1 * XGT(IWHICH2)))/tdis
            y3 = ((r2 * YGT(IWHICH1)) + (r1 * YGT(IWHICH2)))/tdis
            z3 = ((r2 * ZGT(IWHICH1)) + (r1 * ZGT(IWHICH2)))/tdis
  
            write(outs,'(a,3f10.4)') ' Moved vertex @ XYZ:',x3,y3,z3
            call edisp(iuout,outs)

C Show the new point on the current wireframe.
            if(MMOD.ge.8)then
              COG1(1)=x3
              COG1(2)=y3
              COG1(3)=z3
              CALL VECTRN(COG1,TEMAT,COG2,IER)
              CALL VECTRN(COG2,ETSMAT,COG3,IER)
              call u2pixel(COG3(1),COG3(2),iix,iiy)
              call ecirc(iix,iiy,3,1)
              call forceflush()
            endif
            dok=.true.
            call askok('Apply this move?',' ',OK,dok,nbhelp)
            if(OK)then
              xgt(IWHICH1)=x3
              ygt(IWHICH1)=y3
              zgt(IWHICH1)=z3
            endif
            MODGEO=.TRUE.
            MODIFY=.TRUE.
            MODBND=.TRUE.
            if(MMOD.ge.8)then
              CALL ADJVIEW(IER)
              CALL INLNSTG(3)
              call EGRNDR(IER)
            endif
          else
            goto 3
          endif
        endif
        call usrmsg(' ',' ','-')

C If possible to add another vertex ask if calculated point should
C be a new vertex and update the wireframe.
        if(ino.eq.5)then
          if(NGTV.lt.MGTV)then
            write(outs,'(a,i3,a)') 'Make this (',NGTV+1,') a:'
            CALL EASKABC(outs,' ','new vertex',
     &        'new vertex after editing','cancel',INVT,nbhelp)
            if(INVT.eq.1)then
              NGTV=NGTV+1
              xgt(ngtv)=x3
              ygt(ngtv)=y3
              zgt(ngtv)=z3
            elseif(INVT.eq.2)then
              NGTV=NGTV+1
              WRITE(HOLD,'(1x,3f10.5)')x3,y3,z3
              write(outs,'(a,i3,a)')' Vertex (',ngtv,') X  Y  Z (m):'
  43          CALL EASKS(HOLD,outs,' ',32,' 0. 0. 0. ','vtx co',
     &          IER,nbhelp)
              K=0
              CALL EGETWR(HOLD,K,XGT(ngtv),-999.,999.,'W','X cd',IER)
              CALL EGETWR(HOLD,K,YGT(ngtv),-999.,999.,'W','Y cd',IER)
              CALL EGETWR(HOLD,K,ZGT(ngtv),-9.9,999.9,'W','Z cd',IER)
              if(ier.ne.0)goto 43
            elseif(INVT.eq.3)then
              goto 3
            endif
            ILEN=NGTV
            IPACT=CREATE
            CALL EKPAGE(IPACT)
            MODGEO=.TRUE.
            MODIFY=.TRUE.
            MODBND=.TRUE.
            call usrmsg(' ',' ','-')
C            if(MMOD.ge.8)then
C              CALL INLNST(1)
C              izgfoc=0
C              CALL ADJVIEW(IER)
C              call EGRNDR(IER)
C            endif
            goto 92
          endif
        endif
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C Alter vertex list by deleting/ copy / replicate / editing.
        helptopic='ground_form_vertex_manage'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IRT=1
        CALL EASKATOG('Vertex operations:',' ','add','delete','copy',
     &    'replicate','edit (several)','do nothing',' ',IRT,nbhelp)
        if(IRT.eq.1)then
          IADD=1
          CALL EASKI(IADD,' ',' How many vertices to add ? ',
     &     1,'F',MGTV-NGTV,'F',1,'+ vertex',IERI,nbhelp)
          if(iadd.eq.0)then
            goto 92
          else
            if(ieri.eq.-3)then
              goto 92
            else
              CALL ADDGVERT(IADD,'A',IER)
              ILEN=NGTV
              IPACT=CREATE
              CALL EKPAGE(IPACT)
              MODGEO=.TRUE.
            endif
          endif
        elseif(IRT.eq.2)then

C Delete one or more vertices, get list and then sort in decending
C order so that compating of list works correctly.
          jipm=IPM
          inpick=MIN0(12,NGTV-1)
          CALL EPMENSV
          call EPKGVERT(INPICK,IVLST,'Vertices to delete',
     &      'Select vertices to delete from list...',' ',nbhelp,ier)
          CALL EPMENRC
          if(inpick.gt.0)then
            KFLAG = -1
            call SORTI(IVLST,ITEMP,MTV,KFLAG)
            do 142 ij=1,inpick
              iwhich=IVLST(ij)
              CALL ADDGVERT(IWHICH,'D',IER)
  142       continue
            ILEN=NGTV
            IPACT=CREATE
            CALL EKPAGE(IPACT)
            IPACT= -1*jipm
            CALL EKPAGE(IPACT)
            MODGEO=.TRUE.
            MODIFY=.TRUE.
            MODBND=.TRUE.
          endif
        elseif(IRT.eq.3)then

C In the case of copying existing vertices, process one or more,
C adding to zone data structure and updating the interface. Allow
C user to copy only as many vertices as there are in the zone but
C no more than will overrange MTV.
          inpick=MIN0(NGTV,MGTV-NGTV)
          CALL EPMENSV
          call EPKGVERT(INPICK,IVLST,'Vertices to copy',
     &      'Select vertices to copy from list...',' ',nbhelp,ier)
          CALL EPMENRC
          if(inpick.ge.1)then
            do 143 ij=1,inpick
              iwhich=IVLST(ij)
              CALL ADDGVERT(IWHICH,'C',IER)
  143       continue
            ILEN=NGTV
            IPACT=CREATE
            CALL EKPAGE(IPACT)
            MODGEO=.TRUE.
            MODIFY=.TRUE.
            MODBND=.TRUE.
          endif
        elseif(IRT.eq.4)then

C In the case of replicating an existing vertex, add it to zone
C data structure and updating the interface. Allow user to replicate
C only up to MTV.
          inpick=1
          CALL EPMENSV
          call EPKGVERT(INPICK,IVLST,'Vertex to replicate',
     &      'Select vertex to replicate from list...',' ',nbhelp,ier)
          CALL EPMENRC
          if(inpick.eq.1)then
            iwhich=IVLST(1)
            CALL EASKI(IADD,' ',' Replicate how many times ? ',
     &        1,'F',MGTV-NGTV,'F',1,'+ replicate vertex',IERI,nbhelp)
            if(ieri.eq.-3)then
              goto 92
            elseif(ieri.eq.0)then
              if(IADD.ge.1)then
                do 144 ij=1,IADD
                  CALL ADDGVERT(IWHICH,'C',IER)
 144            continue
                ILEN=NGTV
                IPACT=CREATE
                CALL EKPAGE(IPACT)
                MODGEO=.TRUE.
                MODIFY=.TRUE.
                MODBND=.TRUE.
              endif
            endif
          endif
        elseif(IRT.eq.5)then

C Select (possibly via mouse) and then edit.
          inpick=MIN0(NGTV,MGTV-NGTV)
          CALL EPMENSV
          call EPKGVERT(INPICK,IVLST,'Vertices to edit',
     &      'Select vertex to edit from list...',' ',nbhelp,ier)
          CALL EPMENRC
          if(inpick.ge.1)then
            do 145 ij=1,inpick
              ifoc=IVLST(ij)

C Present vertex coords for editing then parse data from HOLD.
              WRITE(HOLD,'(1x,3f10.5)')XGT(IFOC),YGT(IFOC),ZGT(IFOC)
              write(outs,'(a,i3,a)')
     &          ' Vertex (',ifoc,') X  Y  Z (in metres):'
 146          CALL EASKS(HOLD,outs,' ',32,' 0. 0. 0. ','vertex coord',
     &          IER,nbhelp)
              K=0
              CALL EGETWR(HOLD,K,XGT(IFOC),-999.9,999.9,'W','Xcord',IER)
              CALL EGETWR(HOLD,K,YGT(IFOC),-999.9,999.9,'W','Ycord',IER)
              CALL EGETWR(HOLD,K,ZGT(IFOC),-9.9,999.9,'W','Zcord',IER)
              if(ier.ne.0)goto 146
              MODIFY=.TRUE.
              MODBND=.TRUE.
              MODGEO=.TRUE.
              MODLEN=.TRUE.
 145        continue
          endif
        endif
      ELSEIF(IVERT.EQ.(MVERT-5))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Edit vertex identified by KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)

C Present vertex coords for editing then parse data from HOLD.
        WRITE(HOLD,'(1x,3f10.5)')XGT(IFOC),YGT(IFOC),ZGT(IFOC)
        write(outs,'(a,i3,a)')' Vertex (',ifoc,') X  Y  Z (in metres):'
  42    CALL EASKS(HOLD,outs,' ',32,' 0. 0. 0. ','vertex coord',
     &    IER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,XGT(IFOC),-999.9,999.9,'W','X cord',IER)
        CALL EGETWR(HOLD,K,YGT(IFOC),-999.9,999.9,'W','Y cord',IER)
        CALL EGETWR(HOLD,K,ZGT(IFOC),-9.9,999.9,'W','Z cord',IER)
        if(ier.ne.0)goto 42
        MODIFY=.TRUE.
        MODBND=.TRUE.
        MODGEO=.TRUE.
        MODLEN=.TRUE.
      ELSE
C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-2
      goto 3

      END 

C ************* EPKGVERT 
C EPKGVERT Select one or more vertices from information currently in
C common block G1.
C IER=0 OK, IER=1 problem.
      SUBROUTINE EPKGVERT(INPICK,IVLST,TITLE,PROMPT1,PROMPT2,NHELP,IER)
#include "building.h"
#include "epara.h"
#include "prj3dv.h"
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)
      DIMENSION  item(36)
      DIMENSION  COG1(3),COG2(3),COG3(3),IVLST(MGTV)
      CHARACTER*(*) TITLE,PROMPT1,PROMPT2
      CHARACTER item*33,outs*124,KEY*1
      logical found
      integer MVERT,IVERT ! max items and current menu item

C It is assumed that contextual help will have been setup before
C calling this subroutine.

#ifdef OSI
      integer iix,iiy,ixd,iyd
#else
      integer*8 iix,iiy,ixd,iyd
#endif

C Initialise vertex list menu size variables based on window size.
C IVERT is the menu position, MVERT the current number of menu lines.
C Also clear tagged items list (IVLST).
      IER=0
      MHEAD=1
      MCTL=4
      ILEN=NGTV
      IPACT=CREATE
      CALL EKPAGE(IPACT)
      IALLOW=INPICK
      INPICK=0
      DO 40 I=1,NGTV
        IVLST(I)=0
   40 CONTINUE

C Initial menu entry setup.
      CALL USRMSG(PROMPT1,PROMPT2,'-')
   92 IER=0
      ILEN=NGTV
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
    3 M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          item(m)=' '
          WRITE(item(M),'(A1,I3,3F9.3)')KEY,L,XGT(L),YGT(L),ZGT(L)
          do 20 K=1,INPICK
            if(IVLST(K).eq.L) then
              WRITE(item(M),'(A1,I3,3F9.3,A)')KEY,L,XGT(L),YGT(L),
     &          ZGT(L),' *'
            endif
 20       continue
        ENDIF
   10 CONTINUE

      item(1)    =' Vertex | X Y Z coordinates      '

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        item(M+1)='  ______________________________ '
      ELSE
        WRITE(item(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' --------')
      ENDIF
      if(MMOD.lt.8)then
        write(item(M+2),'(2x,a,i2,a)') '(',IALLOW,' items)'
      else
        write(item(M+2),'(a,i2,a)')'* select via mouse (',IALLOW,
     &    ' items)'
      endif
      item(M+3)  ='? help                           '
      item(M+4)  ='- exit                           '

C Help text for this menu is passed in from calling routine.
      CALL EMENU(TITLE,item,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN

C Return with updated IVLST().
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Display help strings setup in the calling function.
        CALL PHELPD('ground vertex pick',NHELP,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C Use mouse to select existing vertices.
        if(MMOD.eq.8)then
          ijvn=0
          if(inpick.ne.2)then
            call edisp(iuout,
     &        'Select points via cursor...type `e` to finish.')
          else
            call edisp(iuout,
     &        'Select points or edge via cursor...type `e` to finish.')
          endif

C Return pixel position of mouse click, check if key `e` or `E` was
C hit and then loop through each of the vertices for something close.
  46      CALL trackview(ii,ixx,iyy)
          if(ii.eq.69.or.ii.eq.101)goto 47
          found=.false.
          do 45 i=1,NGTV
            COG1(1)=XGT(I)
            COG1(2)=YGT(I)
            COG1(3)=ZGT(I)
            CALL VECTRN(COG1,TEMAT,COG2,IER)
            CALL VECTRN(COG2,ETSMAT,COG3,IER)
            call u2pixel(COG3(1),COG3(2),iix,iiy)
            ixd=iix-ixx
            iyd=iiy-iyy
            if(abs(ixd).lt.5.and.abs(iyd).lt.5)then
              if(found)then
                call edisp(iuout,'Close points...try again.')
                goto 46
              endif
              WRITE(outs,'(a,i3,a,3F9.3)')' The point matches vertex',
     &          i,' @ XYZ ',XGT(I),YGT(I),ZGT(I)
              call edisp(iuout,outs)
              found=.true.
              INPICK=INPICK+1
              ijvn=ijvn+1
              IVLST(ijvn)=i
              CALL ecirc(iix,iiy,3,0)
              call forceflush()
              goto 46
            endif
  45      continue
          if(.NOT.found)then

C If there were only two points to find, check if user clicked on edge.
            if(inpick.eq.2)then
              call edisp(iuout,'code for edge check in progress...')
            endif
            goto 46
          endif
  47      continue
        endif
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Look through previous selections and see if IFOC is unique, if
C so update IVLST and loop back for another.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        FOUND=.FALSE.
        IF(INPICK.GT.0)THEN
          DO 44 J=1,INPICK
            IF(IVLST(J).EQ.IFOC.or.FOUND) then
              FOUND=.TRUE.
              if (J+1.gt.NGTV) then
                IVLST(J)=0
              else
                IVLST(J)=IVLST(J+1)
              endif
            endif
  44      CONTINUE
          IF(.NOT.FOUND)THEN
            if (INPICK.lt.IALLOW) then
              INPICK=INPICK+1
              IVLST(INPICK)=IFOC
            endif
          ELSE
            INPICK=INPICK-1
          ENDIF
        ELSEIF(INPICK.EQ.0)THEN
          INPICK=1
          IVLST(INPICK)=IFOC
        ENDIF
      ELSE

C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-2
      goto 3

      END

C ************* ADDGVERT 
C ADDGVERT: Add,  delete, copy a vertex within ground topology.
C Passed character ACTION to signal deletion or addition. NUM will
C be either the vertex to delete or the number of vertices to
C added at the end of the list.
      SUBROUTINE ADDGVERT(NUM,ACTION,IER)
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)
      COMMON/GT5/GSNAME(MGRT),GMLCN(MGRT)
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      LOGICAL MODIFY,MODLEN,MODBND,check,found,closex,closey,closez
      logical ok,dok,matchver

      DIMENSION ISASSO(MS),X1(MV),Y1(MV),Z1(MV)
      CHARACTER OUTSTR*124,ACTION*1,HOLD*32,outs*124
      character GMLCN*12,GSNAME*6

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='ADDGVERT'  ! set for subroutine

      check = .false.
      if(NUM.eq.0)then
        call usrmsg('Sorry, there is no vertex 0 in the list...',
     &    ' ','W')
        return
      endif
      if(ACTION.eq.'D'.or.ACTION.eq.'d')then

C Loop through the surfaces and see if this vertex is mentioned.
        IWHICH=NUM
        IF(NGT.GT.0)THEN
          IHIT=0
          DO 8792 IS=1,NGT
            DO 8794 IV=1,NGVER(IS)
              IF(IWHICH.EQ.JGVN(IS,IV))THEN
                IHIT=IHIT+1
                ISASSO(IHIT)=IS
              ENDIF
 8794       CONTINUE
 8792     CONTINUE
          IF(IHIT.GT.0)THEN
            WRITE(OUTSTR,9993)IWHICH,XGT(IWHICH),YGT(IWHICH),ZGT(IWHICH)
 9993       FORMAT(' Vertex ',I3,' located at ',3F10.3)
            CALL EDISP(iuout,OUTSTR)
            CALL EDISP(iuout,' is associated with surfaces: ')
            WRITE(OUTSTR,9983)(ISASSO(IH),IH=1,IHIT)
 9983       FORMAT(' ',10(I2,', '))
            CALL EDISP(iuout,OUTSTR)
          ELSE

C Vertex is free of connections so loop through the vertices in each
C surface and any JNV larger than IWHICH should be decremented by one
            DO 792 IS=1,NGT
              DO 794 IV=1,NGVER(IS)
                IF(JGVN(IS,IV).GT.IWHICH)THEN
                  JGVN(IS,IV)=JGVN(IS,IV)-1
                ENDIF
  794         CONTINUE
  792       CONTINUE

C Now loop through the vertices and copy the contents of vertices >
C IWHICH into the next lower one.
            DO 791 IVV=IWHICH,NGTV-1
              XGT(IVV)=XGT(IVV+1)
              YGT(IVV)=YGT(IVV+1)
              ZGT(IVV)=ZGT(IVV+1)
  791       CONTINUE

C Finally decrement NGTV.
            NGTV=NGTV-1
          ENDIF
        ENDIF
        MODIFY=.TRUE.

      elseif(ACTION.eq.'A'.or.ACTION.eq.'a')then

C Increment NGTV and present vertex coords (use previous
C in case of repitition), then parse. Check to see if the
C new vertex is unique (i.e. not within 1mm of an existing
C vertex.
        helptopic='ground_add_vertex'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IADD=NUM
        DO 18 I=1,IADD
          if(I.eq.1)then
            HOLD = ' 0.000  0.000  0.000 '
          else
            WRITE(HOLD,'(1x,3f8.3)')X1(I-1),Y1(I-1),Z1(I-1)
          endif
          CALL EASKS(HOLD,' Vertex  X  Y  Z (in metres): ',
     &     '  ',32,' 0. 0. 0. ','vertex coord',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,X1(I),-999.9,999.9,'W','Xcoord',IER)
          CALL EGETWR(HOLD,K,Y1(I),-999.9,999.9,'W','Ycoord',IER)
          CALL EGETWR(HOLD,K,Z1(I),-9.9,999.9,'W','Zcoord',IER)
  18    CONTINUE

C Append new vertices to zone list if they are unique,
C otherwise continue.
        do 143 ix = 1,IADD
          if(NGTV+1.le.MGTV)then
            found=.false.
            do 144 ixx = 1,NGTV
              CALL ECLOSE(X1(ix),XGT(ixx),0.001,closex)
              CALL ECLOSE(Y1(ix),YGT(ixx),0.001,closey)
              CALL ECLOSE(Z1(ix),ZGT(ixx),0.001,closez)
              if(closex.and.closey.and.closez)then
                found=.true.
                goto 145
              endif
  144       continue

C If an existing vertex is close then skip to next, otherwise
C add a new vertex to the end of the zone list.
  145       if(found)then
              continue
            else
              NGTV=NGTV+1
              XGT(NGTV)=X1(ix)
              YGT(NGTV)=Y1(ix)
              ZGT(NGTV)=Z1(ix)

C For a unique vertex also test to see if it is close (+-0.003m) to
C one of the existing lines. Also check the last edge in the surface.
              iwhich3=NGTV
              do 246 ivj=1,NGT
                ivjlimit=NGVER(ivj)
                do 247 ivjj=1,ivjlimit
                  if(ivjj.eq.ivjlimit)then
                    iwhich1=JGVN(ivj,ivjj)
                    iwhich2=JGVN(ivj,1)
                  else
                    iwhich1=JGVN(ivj,ivjj)
                    iwhich2=JGVN(ivj,ivjj+1)
                  endif

C Report length of line. Use method of Ward/Radiance in fvect.c
                  call pointtogline(iwhich3,iwhich1,iwhich2,offset,
     &              matchver)
                  if(.NOT.matchver) goto 247
                  if(offset.lt.0.003)then
                    write(outs,'(a,i3,a,3f8.3,a,f6.4,a,i3,a,i3,2a)')
     &               'New vertex ',iwhich3,' @',XGT(NGTV),
     &                YGT(NGTV),ZGT(NGTV),' is close (',offset,
     &                ') to edge ',IWHICH1,' &',IWHICH2,' of ',
     &                gsname(ivj)
                    call edisp(iuout,outs)

C If current surface (ivj) can take another vertex expand the
C list. Logic works by looping down (from one more than the current
C number of vertices associated with this surface) shifting
C JGVN indices up one until at the current edge (ivjj) and then
C inserting the new vertex index.
                    if(NGVER(ivj)+1.le.8)then
                      dok=.true.
                      call askok('Insert this vertex?','(see help)',
     &                  ok,dok,nbhelp)
                    else
                      ok=.false.
                    endif
                    if(ok)then
                      NGVER(ivj)=NGVER(ivj)+1
                      IXV=NGVER(ivj)+1
  148                 continue
                      IXV=IXV-1
                      JGVN(ivj,IXV)=JGVN(ivj,IXV-1)
                      IF(IXV.GT.ivjj+1)GOTO 148
                      JGVN(ivj,ivjj+1)=iwhich3

C Debug.
C                      write(6,'(a,30i4)') 'now jgvn is ',
C     &                  (JGVN(ivj,ii),ii=1,NGVER(ivj))

C Surface (ivj) vertex list has been updated. Go on to next surface.
                      goto 246 
                    endif
                  endif
  247           continue
  246         continue
            endif
          endif
  143   continue
        MODIFY=.TRUE.
        check = .true.
      elseif(ACTION.eq.'C'.or.ACTION.eq.'c')then

C Copy vertex index and then increment NGTV and present vertex coords 
C then parse into new vertex.
        helptopic='ground_add_vertex'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call tstamp('>','PRJ: copy vertex')
        WRITE(HOLD,'(1x,3f8.3)')XGT(NUM),YGT(NUM),ZGT(NUM)
        write(outs,'(a,i3,a,i3,a)')'Copied Vertex (',NUM,
     &    ' old; current index ',NGTV+1,')'
        CALL EASKS(HOLD,outs,' X  Y  Z (in metres): ',
     &    32,' 0. 0. 0. ','vertex coord',IER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,X1(1),-999.9,999.9,'W','X coord',IER)
        CALL EGETWR(HOLD,K,Y1(1),-999.9,999.9,'W','Y coord',IER)
        CALL EGETWR(HOLD,K,Z1(1),-9.9,999.9,'W','Z coord',IER)

C Append new vertice to zone list if it is unique, otherwise continue.
        if(NGTV+1.le.MGTV)then
          found=.false.
          do 244 ixx = 1,NGTV
            CALL ECLOSE(X1(1),XGT(ixx),0.001,closex)
            CALL ECLOSE(Y1(1),YGT(ixx),0.001,closey)
            CALL ECLOSE(Z1(1),ZGT(ixx),0.001,closez)
            if(closex.and.closey.and.closez)then
              found=.true.
              goto 245
            endif
  244     continue

C If an existing vertex is close then skip to next, otherwise
C add a new vertex to the end of the zone list.
  245     if(found)then
            continue
          else
            NGTV=NGTV+1
            XGT(NGTV)=X1(1)
            YGT(NGTV)=Y1(1)
            ZGT(NGTV)=Z1(1)

C For a unique vertex also test to see if it is close (+-0.003m) to
C one of the existing lines. Also check the last edge in the surface.
            iwhich3=NGTV
            do 346 ivj=1,NGT
              ivjlimit=NGVER(ivj)
              do 347 ivjj=1,ivjlimit
                if(ivjj.eq.ivjlimit)then
                  iwhich1=JGVN(ivj,ivjj)
                  iwhich2=JGVN(ivj,1)
                else
                  iwhich1=JGVN(ivj,ivjj)
                  iwhich2=JGVN(ivj,ivjj+1)
                endif

C Report length of line. Use method of Ward/Radiance in fvect.c
                call pointtogline(iwhich3,iwhich1,iwhich2,offset,
     &            matchver)
                if(.NOT.matchver) goto 347
                if(offset.lt.0.003)then
                  write(outs,'(a,i3,a,3f8.3,a,f6.4,a,i3,a,i3,2a)')
     &             'Copied vertex ',iwhich3,' @',
     &              XGT(NGTV),YGT(NGTV),ZGT(NGTV),' is close (',offset,
     &              ') to edge ',IWHICH1,' &',IWHICH2,' of ',
     &                gsname(ivj)
                  call edisp(iuout,outs)

C If current surface (ivj) can take another vertex expand the
C list. Logic works by looping down (from one more than the current
C number of vertices associated with this surface) shifting
C JGVN indices up one until at the current edge (ivjj) and then
C inserting the new vertex index.
                  if(NGVER(ivj)+1.le.8)then
                    dok=.true.
                    call askok('Insert this vertex?','(see help)',
     &                ok,dok,nbhelp)
                  else
                    ok=.false.
                  endif
                  if(ok)then
                    NGVER(ivj)=NGVER(ivj)+1
                    IXV=NGVER(ivj)+1
  248               continue
                    IXV=IXV-1
                    JGVN(ivj,IXV)=JGVN(ivj,IXV-1)
                    IF(IXV.GT.ivjj+1)GOTO 248
                    JGVN(ivj,ivjj+1)=iwhich3

C Surface (ivj) vertex list has been updated. Go on to next surface.
                    goto 346 
                  endif
                endif
  347         continue
  346       continue
          endif
        endif
        MODIFY=.TRUE.
        check = .true.
      endif
      if(check)then

C Check to see if new vertex is beyond current bounds of zone.
        MODBND=.FALSE.
        MODLEN=.FALSE.
        IF(XGT(NGTV).GT.XMX.OR.XGT(NGTV).LT.XMN) MODBND=.TRUE.
        IF(YGT(NGTV).GT.YMX.OR.YGT(NGTV).LT.YMN) MODBND=.TRUE.
        IF(ZGT(NGTV).GT.ZMX.OR.ZGT(NGTV).LT.ZMN) MODBND=.TRUE.
        IF(MODBND)MODLEN=.TRUE.
      endif

      RETURN
      END

C ******************* EASKGSUR 
C EASKSUR presents a list of ground surfaces returning the
C index IS. It assumes that a ground topology file has been read in.
C If MOD = '-' name list only, MOD = 'M' attribute Many, MOD = 'A'
C show attributes in list.
      SUBROUTINE EASKGSUR(IS,MOD,PROMPT1,PROMPT2,IER)
#include "building.h"
#include "epara.h"
      
      integer lnblnk  ! function definition

      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)
      COMMON/GT/GTNAME
      COMMON/GT5/GSNAME(MGRT),GMLCN(MGRT)
      DIMENSION VERT(35),VERTS(35)

      CHARACTER*(*) PROMPT1,PROMPT2
      CHARACTER VERT*20,VERTS*33,KEY*1,MOD*1,title*32
      CHARACTER GMLCN*12,GSNAME*6,GTNAME*15
      LOGICAL SELECT
      integer MVERT,IVERT ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EASKGSUR'  ! set for subroutine

      IER=0
      write(title,'(2a)') 'Surface attribs: ',
     &                    GTNAME(1:lnblnk(GTNAME))

C Initialise surface menu variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
      SELECT=.FALSE.
      if(MOD.eq.'-')then
        MHEAD=0
      elseif(MOD.eq.'M')then
        MHEAD=1
        VERTS(1)='  Name   Composition'
      elseif(MOD.eq.'A')then
        MHEAD=1
        VERTS(1)='  Name   Composition'
      endif
      MCTL=4
      ILEN=NGT
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
    3 M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          if(MOD.eq.'-')then
            WRITE(VERT(M),14)KEY,GSNAME(L)
   14       FORMAT(A1,1X,A)
          else
            WRITE(VERTS(M),13)KEY,GSNAME(L),GMLCN(L)
   13       FORMAT(A1,1X,A,1X,A)
          endif
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1) ='  ________________  '
        VERTS(M+1)='  _________________________  '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT ('0 page part:',I2,' of ',I2)
        WRITE(VERTS(M+1),16)IPM,MPM 
   16   FORMAT ('0 page -- part: ',I2,' of ',I2)
      ENDIF

C If MOD has been passed as a M then return -2 to indicate further processing
      IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
        VERTS(M+2)='* attribute many    '
      ELSE
        VERT(M+2) ='                    '
        VERTS(M+2)='                        '
      ENDIF
      VERT(M+3)   ='? help              '
      VERTS(M+3)  ='? help              '
      VERT(M+4)   ='- exit this menu    '
      VERTS(M+4)  ='- exit this menu    '

C Display the menu.
      helptopic='ground_surf_selection'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call usrmsg(PROMPT1,PROMPT2,'-')
      if(MOD.eq.'-')then
        CALL EMENU('Surfaces',VERT,MVERT,IVERT)
      else
        CALL EMENU(title,VERTS,MVERT,IVERT)
      endif
      IF(IVERT.LE.MHEAD)THEN
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN

C If no selection has been made before exit then return with 0.
        IF(.NOT.SELECT)IS=0
        call usrmsg(' ',' ','-')
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN
        helptopic='ground_surf_selection'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('ground surface file section',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C Allow user to say several to be attributed.
        IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
          IS= -2
          call usrmsg(' ',' ','-')
          RETURN
        ELSE
          IVERT=-1
          goto 3
        ENDIF
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Decode from the potential long list to the zone number via KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        SELECT=.TRUE.
        IS=IFOC
        call usrmsg(' ',' ','-')
        RETURN
      ELSE
C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-2
      goto 3

      END

C ************* ADDGSUR 
C ADDGSUR: Add / delete / copy a ground surface.
C Passed character ACTION to signal deletion 'D', addition 'A', copy 'C'.
C It is easy to add a surface at the end of the list. 
C Method checks for minimally linked vertices.
      SUBROUTINE ADDGSUR(ITRC,IWHICH,ACTION,IER)
#include "building.h"
#include "prj3dv.h"
      
      integer lnblnk  ! function definition
      integer iCountWords

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)
      COMMON/GT5/GSNAME(MGRT),GMLCN(MGRT)
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      LOGICAL MODIFY,MODLEN,MODBND,closex,closey,closez,OK,DOK
      logical clkok,found,matchver

      dimension X1(8),Y1(8),Z1(8),jvn1(8),XT(8),YT(8),ZT(8)
      DIMENSION COG1(3),COG2(3),COG3(3),JJVN(8)

      CHARACTER GMLCN*12,GSNAME*6
      CHARACTER ACTION*1,HOLD*72,outs*124,SN*6,SN2*6
      character holds*36,SNAME1*6,SNAMED*6,SMLCN1*12
      integer IRT  ! for radio button
#ifdef OSI
      integer iix,iiy,ixd,iyd
#else
      integer*8 iix,iiy,ixd,iyd
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='ADDGSUR'  ! set for subroutine
    
      IER=0
      DOK=.false.
      valx=0.
      valy=0.
      valz=0.
      IOK=0

      IF(ACTION.EQ.'D')THEN

C  Since this routine may be used to completly remove ground, trap
C if small number of surfaces.
        if(NGT.eq.1)goto 65

C Loop through each of the surfaces greater than IWHICH and copy their
C contents into the surface below. 
        if(IWHICH.eq.NGT)then
          GSNAME(IWHICH)=' '
          GMLCN(IWHICH)=' '
          goto 65
        endif
        DO 66 IS=IWHICH,NGT-1
          GSNAME(IS)=GSNAME(IS+1)
          GMLCN(IS)=GMLCN(IS+1)
          NGVER(IS)=NGVER(IS+1)
          DO 67 IV=1,NGVER(IS)
            JGVN(IS,IV)=JGVN(IS+1,IV)
   67     CONTINUE
   66   CONTINUE
   65   NGT=NGT-1
      ELSEIF(ACTION.EQ.'C')THEN

C Copy an existing surface, remember how many can be copied
C from another zone.
        IF(NGT+1.GT.MGRT)THEN
          CALL USRMSG('No more surfaces are allowed.',' ','W')
          RETURN
        ENDIF
        call tstamp('>','PRJ: copy surface')
        CALL EDISP(iuout,'Please specify a surface to copy.')
        IS=1

        CALL EPMENSV
        CALL EASKGSUR(IS,'-','Select surface to copy.',' ',IER)
        CALL EPMENRC
        if(is.eq.0)then
          CALL usrmsg('No surface selected.',
     &      'aborting surface copy.','P')
          return
        endif

        write(outs,'(a,a)') 'Copying attributes of ',GSNAME(IS)
        call usrmsg(outs,' ','-')
        NVER1=NGVER(IS)
        DO 142 IV=1,NVER1
          X1(iv)=XGT(JGVN(IS,IV))
          Y1(iv)=YGT(JGVN(IS,IV))
          Z1(iv)=ZGT(JGVN(IS,IV))
  142   continue

C Generate a new surface name and default, then check to see if
C if is a duplicate.
        write(SNAMED,'(a)') GSNAME(IS)(1:6)
        write(SNAME1,'(a)') GSNAME(IS)(1:6)
          CALL EASKS(SNAME1,'Revised surface name','Please edit.',
     &    6,SNAMED,'ground surface name',IER,2)
        SMLCN1=GMLCN(IS)

C Up the surface count.
        NGT=NGT+1
        NGVER(NGT)=NVER1
        GSNAME(NGT)=SNAME1
        GMLCN(NGT)=SMLCN1

        helptopic='ground_surf_transforms'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(.NOT.DOK)then
          IRT=1
          CALL EASKATOG(' ','Surface action options:',
     &      'shift along normal','transform X Y Z','rotate','invert',
     &      'combination','cancel',' ',IRT,nbhelp)
        endif
        if(IRT.eq.1.or.IRT.eq.5)then

C Shift along normal and copy transformed points back to X1,Y1,Z1 array.
          if(.NOT.DOK)then
            vdis=0.0
            CALL EASKR(vdis,' ','Distance along normal?',
     &        -9.999,'F',9.999,'W',0.0,'d along normal',IER,nbhelp)
          endif
          CALL TRANGSUR(ITRC,iuout,NVER1,X1,Y1,Z1,vdis,XT,YT,ZT,
     &      gsname(ngt))
          do 151 nt1=1,NVER1
            X1(nt1)=XT(nt1)
            Y1(nt1)=YT(nt1)
            Z1(nt1)=ZT(nt1)
 151      continue
        endif
        if(IRT.eq.2.or.IRT.eq.5)then

C Transform all surface vertices.
          if(.NOT.DOK)then
            HOLDS= ' 0.  0.  0. '
 152        CALL EASKS(HOLDS,' X Y & Z offsets: ',' ',
     &        36,' 0. 0. 0. ','offsets',IER,nbhelp)
            K=0
            CALL EGETWR(HOLDS,K,VALX,-50.0,50.0,'W','X off',IER)
            CALL EGETWR(HOLDS,K,VALY,-50.0,50.0,'W','Y off',IER)
            CALL EGETWR(HOLDS,K,VALZ,-50.0,50.0,'W','Z off',IER)
            if(ier.ne.0)goto 152
          endif
          DO 153 I=1,NVER1
            X1(I)=X1(I)+VALX
            Y1(I)=Y1(I)+VALY
            Z1(I)=Z1(I)+VALZ
 153      continue
        endif
        if(IRT.eq.3.or.IRT.eq.5)then

C Rotate around a point in the surface.
          if(.NOT.DOK)then
            CALL EASKI(IRV,' ','Rotate about which vertex?',
     &        1,'F',NVER1,'F',1,'rotation anchor',IERI,nbhelp)
            if(ieri.eq.-3)then

C User asked to cancel so retract new information and return.
              MODIFY=.FALSE.
              NGVER(NGT)=0
              GSNAME(NGT)=' '
              GMLCN(NGT)=' '
              NGT=NGT-1
              return
            endif
            ANGR=0.
            CALL EASKR(ANGR,' ','Rotation degrees?',
     &       -359.0,'W',359.0,'W',0.0,'rotation',IER,nbhelp)
          endif
          if(ANGR.LT.-.01.OR.ANGR.GT..01)then
            PI = 4.0 * ATAN(1.0)
            A=-ANGR*PI/180.
            CA=COS(A)
            SA=SIN(A)
            XX1=X1(IRV)
            YY1=Y1(IRV)
            DO 10 I=1,NVER1
              XXX=X1(I)-XX1
              YYY=Y1(I)-YY1
              XR=XXX*CA+YYY*SA
              YR=YYY*CA-XXX*SA
              X1(I)=XR+XX1
              Y1(I)=YR+YY1
   10       continue
          endif
        endif

C Append new surface vertices to ground list if they are unique,
C otherwise link the new surface to an existing vetex.
        do 143 ix = 1,NVER1
          if(NGTV+1.le.MGTV)then
            found=.false.
            do 144 ixx = 1,NGTV
              CALL ECLOSE(X1(ix),XGT(ixx),0.001,closex)
              CALL ECLOSE(Y1(ix),YGT(ixx),0.001,closey)
              CALL ECLOSE(Z1(ix),ZGT(ixx),0.001,closez)
              if(closex.and.closey.and.closez)then
                found=.true.
                match=ixx
                goto 145
              endif
  144       continue

C If an existing vertex is close then reference it otherwise
C add a new vertex to the end of the zone list.
  145       if(found)then
              JGVN(NGT,ix)=match
            else
              NGTV=NGTV+1
              JGVN(NGT,ix)=NGTV
              XGT(NGTV)=X1(ix)
              YGT(NGTV)=Y1(ix)
              ZGT(NGTV)=Z1(ix)
              XMN=AMIN1(XMN,XGT(NGTV))
              YMN=AMIN1(YMN,YGT(NGTV))
              ZMN=AMIN1(ZMN,ZGT(NGTV))
              XMX=AMAX1(XMX,XGT(NGTV))
              YMX=AMAX1(YMX,YGT(NGTV))
              ZMX=AMAX1(ZMX,ZGT(NGTV))

C For a unique vertex also test to see if it is close (+-0.002m) to
C one of the existing lines. ngt-1 because the new surface does
C not need to be checked. Also check the last edge in the surface.
              iwhich3=NGTV
              do 246 ivj=1,NGT-1
                ivjlimit=NGVER(ivj)
                do 247 ivjj=1,ivjlimit
                  if(ivjj.eq.ivjlimit)then
                    iwhich1=JGVN(ivj,ivjj)
                    iwhich2=JGVN(ivj,1)
                  else
                    iwhich1=JGVN(ivj,ivjj)
                    iwhich2=JGVN(ivj,ivjj+1)
                  endif

C Report length of line. Use method of Ward/Radiance in fvect.c
                  call pointtogline(iwhich3,iwhich1,iwhich2,offset,
     &              matchver)
                  if(.NOT.matchver) goto 247
                  if(offset.lt.0.003)then
                    write(outs,'(a,i3,a,3f8.3,a,f6.4,a,i3,a,i3,2a)')
     &               'New vertex ',iwhich3,' @',XGT(NGTV),
     &                YGT(NGTV),ZGT(NGTV),' is close (',offset,
     &                ') to edge ',IWHICH1,' &',IWHICH2,' of surface ',
     &                GSNAME(ivj)
                    call edisp(iuout,outs)

C If current surface (ivj) can take another vertex expand the
C list. Logic works by looping down (from one more than the current
C number of vertices associated with this surface) shifting
C JGVN indices up one until at the current edge (ivjj) and then
C inserting the new vertex index.
                    if(NGVER(ivj)+1.le.8)then
                      dok=.true.
                      call askok(' ','Insert this vertex?',
     &                            ok,dok,nbhelp)
                    else
                      ok=.false.
                    endif
                    if(ok)then
                      NGVER(ivj)=NGVER(ivj)+1
                      IXV=NGVER(ivj)+1
  148                 continue
                      IXV=IXV-1
                      JGVN(ivj,IXV)=JGVN(ivj,IXV-1)
                      IF(IXV.GT.ivjj+1)GOTO 148
                      JGVN(ivj,ivjj+1)=iwhich3

C Debug.
C                      write(6,'(a,30i4)') 'now jgvn is ',
C     &                  (JGVN(ivj,ii),ii=1,NGVER(ivj))

C Surface (ivj) vertex list has been updated. Go on to next surface.
                      goto 246 
                    endif
                  endif
  247           continue
  246         continue
            endif
          endif
  143   continue

C Reverse the ordering.
        if(IRT.eq.4.or.IRT.eq.5.or.IRT.eq.6)then
          write(outs,'(3a)') ' surface ',SNAME1(1:lnblnk(SNAME1)),'?'
          if(.NOT.DOK)then
            if(IRT.eq.4)then
              IOK=1
            else
              CALL EASKAB('Reverse the edge ordering of',
     &                  outs,'yes','no',IOK,nbhelp)
            endif
          endif
          if(IOK.eq.1)then
            do 146, iyy = 1,NGVER(NGT)
              jvn1(iyy)=JGVN(NGT,iyy)
  146       CONTINUE
            JGVN(NGT,1)=jvn1(2)
            JGVN(NGT,2)=jvn1(1)
            do 147, iyy = 3,NGVER(NGT)
              izz=NGVER(NGT)+3-iyy
              JGVN(NGT,iyy)=jvn1(izz)
  147       CONTINUE
          endif
        endif

        MODBND=.TRUE.
        MODIFY=.TRUE.
      ELSEIF(ACTION.EQ.'A')THEN

C Create a new surface, set as many defaults as possible.
        IF(NGT+1.GT.MGRT)THEN
          CALL USRMSG('No more surfaces are allowed.',' ','W')
          RETURN
        ENDIF
        call tstamp('>','PRJ: add surface')
        NGT=NGT+1

C Allow user to type in the list of vertices as a string.  The
C number of items is the number of vertices and then parse out
C the individual vertex numbers from the list.
   41   HOLD=' '
        helptopic='ground_surf_create'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(MMOD.ne.8)then
          CALL EASKS(HOLD,' ','Vertex list?',
     &      72,' ','associated vertices',IER,nbhelp)
        else
          CALL EASKSCMD(HOLD,' ','Vertex list?','via mouse',
     &      clkok,72,' 1 2 3','assoc vert+mouse',IER,nbhelp)
          if(clkok)then
            ijvn=0
            call edisp(iuout,
     &        'Select points via cursor...type `e` to finish.')
  46        CALL trackview(ii,ixx,iyy)
            if(ii.eq.69.or.ii.eq.101)goto 47
            found=.false.
            do 45 i=1,NGTV
              COG1(1)=XGT(I)
              COG1(2)=YGT(I)
              COG1(3)=ZGT(I)
              CALL VECTRN(COG1,TEMAT,COG2,IER)
              CALL VECTRN(COG2,ETSMAT,COG3,IER)
              call u2pixel(COG3(1),COG3(2),iix,iiy)
              ixd=iix-ixx
              iyd=iiy-iyy
              if(abs(ixd).lt.5.and.abs(iyd).lt.5)then
                if(found)then
                  call edisp(iuout,'Close points...try again.')
                  goto 46
                endif
                WRITE(outs,'(a,i5,a,i5,a,i3)')' The point @ x=',ixx,
     &                                ' & y=',iyy,' matches vertex ',i
                call edisp(iuout,outs)
                found=.true.
                ijvn=ijvn+1
                jjvn(ijvn)=i
                call forceflush()
                goto 46
              endif
  45        continue
            if(.NOT.found)goto 46
  47        if(ijvn.lt.3)then
              call edisp(iuout,'Not enough points to make a surface.')
               goto 41
            endif
            HOLD=' '
            WRITE(HOLD,'(9I4)')(JJVN(J),J=1,ijvn)
            CALL EASKS(HOLD,' Associated vertices (confirm):',
     &        '  ',72,' ','associated vertices',IIER,nbhelp)
          endif
        endif
        NV = iCountWords(HOLD)
        K=0
        DO 94 J=1,NV
          CALL EGETWI(HOLD,K,JV,1,NGTV,'W','vertex list',IER)
          JGVN(NGT,J)=JV
   94   CONTINUE
        NGVER(NGT)=NV

C Fill default attributes.
        IF(ngt.LE.9)WRITE(SN,'(a,i1)')'gt-',ngt
        IF(ngt.GT.9.and.I.LE.99)WRITE(SN,'(a,i2)')'gt-',ngt
        IF(ngt.GT.99)WRITE(SN,'(a,i3)')'gt-',ngt
        CALL EASKS(SN,'Surface name','(unique word <=6char):',
     &      6,'new_surf','surface name',IER,nbhelp)
        call st2name(SN,SN2)
        GSNAME(NGT)=SN2
        GMLCN(NGT)='UNKNOWN'
        MODIFY=.TRUE.
      endif

      RETURN
      END

C ************* EDGVLIST 
C Edit ground surface-vertex list attributes in common block grndpl
C via a paging menu. ITRU = unit number for user output, IER=0 OK,
C IER=1 problem.
      SUBROUTINE EDGVLIST(ITRC,IER)
#include "building.h"
#include "epara.h"
#include "prj3dv.h"
#include "esprdbfile.h"
#include "material.h"
      integer iCountWords

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)
      COMMON/GTFIL/GTGEOM
      COMMON/GT/GTNAME
      COMMON/GT5/GSNAME(MGRT),GMLCN(MGRT)

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      COMMON/RAY6G/LINSTYG(MGRT)
      DIMENSION  COG1(3),COG2(3),COG3(3)
      DIMENSION  XXW(4),YYW(4),ZZW(4)
      DIMENSION  AX(MPL),AY(MPL),AZ(MPL),IANXT(MPL)
      DIMENSION  BX(MPL),BY(MPL),BZ(MPL),IBNXT(MPL)
      DIMENSION  SBBOX(3,2)

      LOGICAL MODIFY,MODLEN,MODBND,OK,DOK

      DIMENSION VERT(36)
      DIMENSION  XX(8),YY(8),ZZ(8),XT(8),YT(8),ZT(8),jvn1(8)
      CHARACTER VERT*35,KEY*1,HOLD*72,SN*6
      character head*32,HOLDS*36,outs*124
      CHARACTER GMLCN*12,GSNAME*6,GTNAME*15,GTGEOM*72
      integer IRT   ! for radio button
      integer MVERT,IVERT ! max items and current menu item

#ifdef OSI
      integer iupdown,isym,iix,iiy    ! passed to etplot
#else
      integer*8 iupdown,isym,iix,iiy    ! passed to etplot
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDGVLIST'  ! set for subroutine

C Initialise zone surface-vertex list menu size variables based on
C window size. IVERT is the menu position, MVERT the current
C number of menu lines.
      MHEAD=2
      MCTL=7
      ILEN=NGT
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      ILEN=NGT
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
    3 M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          NV=NGVER(L)
          if(NV.gt.6)NV=6
          WRITE(VERT(M),9990)KEY,GSNAME(L),NV,(JGVN(L,J),J=1,NV)
 9990     FORMAT(A1,1X,A6,1x,I1,6(I4))
        ENDIF
   10 CONTINUE

C Set menu header text.
      VERT(1)=    ' Surface|No. |Verts (anti-clk '
      VERT(2)=    ' name   |vert|from top)   '
C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  _______________________________ '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 Page --- Part: ',I2,' of ',I2,' --')
      ENDIF
      VERT(M+2)  ='+ add / insert / copy a surface   '
      VERT(M+3)  ='* delete a surface                '
      VERT(M+4)  ='> transforms                      '
      VERT(M+5)  ='! browse surface-vertex topology  '
      VERT(M+6)  ='? help                            '
      VERT(M+7)  ='- exit to zone definition menu    '

C Update display.
      if(MMOD.ge.8.and.MODIFY)then
        CALL INLNST(1)
        ITVNO=0
        izgfoc=0
        CALL ADJVIEW(IER)
        CALL INLNSTG(3)
        call EGRNDR(IER)
        MODIFY=.false.
      endif

C Help text for this menu.
      helptopic='ground_vertex_edits'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      write(head,'(A,A)')'Topology of ',GTNAME(1:15)
      CALL EMENU(head,VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Produce help text for the vertex menu.
        helptopic='ground_vertex_edits'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('ground vertex section',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C List info in vertices.
        call gtinfo(iuout)
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C Miscel. geometric transforms. Ask which surface and setup temp array.
        CALL EPMENSV
        CALL EASKGSUR(IS,'-','Select surface to transform.',' ',IER)
        CALL EPMENRC
        if(is.eq.0)goto 92

        N = NGVER(IS)
        DO 150 J = 1,N
          XX(J) = XGT(JGVN(IS,J))
          YY(J) = YGT(JGVN(IS,J))
          ZZ(J) = ZGT(JGVN(IS,J))
  150   CONTINUE

        write(outs,'(a,a)') ' Actions to take on ',GSNAME(is)
        IRT=1
        CALL EASKATOG(outs,' :','shift along normal','transform xyz',
     &    'rotate','invert ','combination','continue ?',' ',IRT,nbhelp)
        if(IRT.eq.1.or.IRT.eq.5)then

C Shift along normal and copy transformed points back to XX,YY,ZZ array.
          call tstamp('>','PRJ: do surface shift along normal')
          vdis=0.0
          CALL EASKR(vdis,' Distance along the normal (metres): ',
     &      '(0.0 = none) :',-9.999,'F',9.999,'W',0.0,'d along normal',
     &      IER,nbhelp)
          CALL TRANGSUR(ITRC,ITRU,N,XX,YY,ZZ,vdis,XT,YT,ZT,gsname(is))
          do 151 nt1=1,N
            XX(nt1)=XT(nt1)
            YY(nt1)=YT(nt1)
            ZZ(nt1)=ZT(nt1)
 151      continue
        endif
        if(IRT.eq.2.or.IRT.eq.5)then

C Transform all surface vertices.
          call tstamp('>','PRJ: do surface vertex tranform')
          HOLDS= ' 0.  0.  0. '
 152      CALL EASKS(HOLDS,' X Y & Z offsets: ',' ',
     &      36,' 0. 0. 0.  ','offsets',IER,nbhelp)
          K=0
          CALL EGETWR(HOLDS,K,VALX,-50.0,50.0,'W','X off',IER)
          CALL EGETWR(HOLDS,K,VALY,-50.0,50.0,'W','Y off',IER)
          CALL EGETWR(HOLDS,K,VALZ,-50.0,50.0,'W','Z off',IER)
          if(ier.ne.0)goto 152
          DO 153 I=1,N
            XX(I)=XX(I)+VALX
            YY(I)=YY(I)+VALY
            ZZ(I)=ZZ(I)+VALZ
 153      continue
        endif
        if(IRT.eq.3.or.IRT.eq.5)then

C Rotate around a point in the surface.
          call tstamp('>','PRJ: do surface rotation')
          CALL EASKI(IRV,'Rotate about which vertex index: ',
     &       '(ie. 1st, 2nd or 3rd in the surface) ',
     &       1,'F',N,'F',1,'rotation anchor',IERI,nbhelp)
          if(ieri.eq.-3)then
            MODIFY=.FALSE.
            goto 92
          endif
          ANGR=0.
          CALL EASKR(ANGR,' ',' Rotation (deg + = anticlockwise) ?',
     &       -359.0,'W',359.0,'W',0.0,'rotation',IER,nbhelp)
          if(ANGR.LT.-.01.OR.ANGR.GT..01)then
            PI = 4.0 * ATAN(1.0)
            A=-ANGR*PI/180.
            CA=COS(A)
            SA=SIN(A)
            XX1=XX(IRV)
            YY1=YY(IRV)
            DO 100 I=1,N
              XXX=XX(I)-XX1
              YYY=YY(I)-YY1
              XR=XXX*CA+YYY*SA
              YR=YYY*CA-XXX*SA
              XX(I)=XR+XX1
              YY(I)=YR+YY1
  100       continue
          endif
        endif
        if(IRT.eq.4)then

C Reverse the ordering, redraw image and return.
          call tstamp('>','PRJ: do surface invert vertex order')
          do 146, iyy = 1,NGVER(IS)
            jvn1(iyy)=JGVN(IS,iyy)
  146     CONTINUE
          JGVN(IS,1)=jvn1(2)
          JGVN(IS,2)=jvn1(1)
          do 147, iyy = 3,NGVER(IS)
            izz=NGVER(IS)+3-iyy
            JGVN(IS,iyy)=jvn1(izz)
  147     CONTINUE
          MODIFY=.TRUE.
          CALL INLNST(1)
          izgfoc=0
          CALL ADJVIEW(IER)
          call EGRNDR(IER)
          goto 92
        endif
        if(IRT.eq.6)then
          MODIFY=.FALSE.
          goto 92
        endif

C Check bounds.
        do 143 ix = 1,N
          XMN=AMIN1(XMN,XX(ix))
          YMN=AMIN1(YMN,YY(ix))
          ZMN=AMIN1(ZMN,ZZ(ix))
          XMX=AMAX1(XMX,XX(ix))
          YMX=AMAX1(YMX,YY(ix))
          ZMX=AMAX1(ZMX,ZZ(ix))
  143   continue

C Update the image.
        MODBND=.TRUE.
        MODIFY=.TRUE.
        if(MMOD.ge.8)then
          CALL INLNST(1)
          izgfoc=0
          CALL ADJVIEW(IER)
          call EGRNDR(IER)

C Show these points as small circles...
          call edisp(iuout,' proposed points are shown as dots....')
          DO 154 J = 1,N
            COG1(1)=XX(J)
            COG1(2)=YY(J)
            COG1(3)=ZZ(J)
            CALL VECTRN(COG1,TEMAT,COG2,IER)
            CALL VECTRN(COG2,ETSMAT,COG3,IER)
            call u2pixel(COG3(1),COG3(2),iix,iiy)
            CALL ecirc(iix,iiy,3,1)
            call forceflush()
  154     CONTINUE
        endif

C Then draw the revised and if acceptable then used transformed
C points within the zone in place of the originals....
        DOK=.true.
        CALL ASKOK('Is it ok to apply the transformed points  ',
     &             'to the surface (and attached surfaces) ?',
     &             OK,DOK,nbhelp)
        if(OK)then
          MODIFY=.TRUE.
          DO 155 J = 1,N
            XGT(JGVN(IS,J)) = XX(J) 
            YGT(JGVN(IS,J)) = YY(J)
            ZGT(JGVN(IS,J)) = ZZ(J) 
  155     CONTINUE
        else
          MODIFY=.FALSE.
        endif
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C Delete a surface. If number of surfaces changed then update cfg.
        CALL EPMENSV
        CALL EASKGSUR(IS,'-','Select surface to delete.',' ',IER)
        CALL EPMENRC
        IF(IS.EQ.0)goto 92
        LASTS=NGT

        CALL ADDGSUR(ITRC,IS,'D',IER)
        if(LASTS.ne.NGT)then
          IFILG=IFIL+1
          call EGROUT(IFILG,gtgeom,iuout,IER)
          CALL EMKCFG('-',IER)
        endif
        ILEN=NGT
        IPACT=CREATE
        CALL EKPAGE(IPACT)

C Update the image.
        MODBND=.TRUE.
        MODIFY=.TRUE.
        if(MMOD.ge.8)then
          CALL INLNST(1)
          izgfoc=0
          CALL ADJVIEW(IER)
          CALL INLNSTG(3)
          call EGRNDR(IER)
        endif
      ELSEIF(IVERT.EQ.(MVERT-5))THEN

C Add/insert a surface.
   30   helptopic='ground_insert_surf'
        call gethelptext(helpinsub,helptopic,nbhelp)
        irt=0
        ilrt=irt
        idrt=2
        call MENUATOL(' ','new surface options',
     &   'a made from existing vertices',
     &   'b -',
     &   'c copy another ground surface',
     &   'd - ',
     &   'e vertical rectangle (origin&azim)',
     &   'f horizontal rectangle (origin&rot)',' ',
     &   ' ',' ',' ',' ',' ',irt,idrt,nbhelp)
        call usrmsg(' ',' ','-')
        IF(ilrt.eq.irt)GOTO 92
        IF(irt.eq.0.or.irt.eq.7)GOTO 92
        LASTS=NGT
        if(IRT.eq.1)then
          call tstamp('>','PRJ: insert surface via vertex')
          CALL ADDGSUR(ITRC,0,'A',IER)
        elseif(IRT.eq.2)then
          call edisp(iuout,'Not an available selection.')
        elseif(IRT.eq.3)then
          call tstamp('>','PRJ: copy ground surface')
          CALL ADDGSUR(ITRC,0,'C',IER)
        elseif(IRT.eq.4)then
          call edisp(iuout,'Not an available selection.')
        elseif(IRT.eq.5)then

C Get vertical rectangular surface orgin.
          IF((NGT+1.GT.MGRT).OR.(NGTV+4.GT.MGTV))THEN
            CALL USRMSG(' A new surface could not be added as',
     &        ' it will make ground too complex!','W')
            RETURN
          ENDIF
          call tstamp('>','PRJ: add vertical surface')
          HOLDS= ' 0.  0.  0. '
          CALL EASKS(HOLDS,' Surface origin X Y & Z: ',' ',
     &      36,' 0. 0. 0.  ','origin XYZ',IER,2)
          K=0
          CALL EGETWR(HOLDS,K,VALOX,-99.0,99.0,'W','X org',IER)
          CALL EGETWR(HOLDS,K,VALOY,-99.0,99.0,'W','Y org',IER)
          CALL EGETWR(HOLDS,K,VALOZ,-99.0,99.0,'W','Z org',IER)

C Get azimuth and length and height.
          HOLDS= ' 180.0   1.0  1.0 '
          CALL EASKS(HOLDS,' Surface azimuth (deg) length & height: ',
     &      ' ',36,' 180.0 1.0 1.0  ','azim length height',IER,nbhelp)
          K=0
          CALL EGETWR(HOLDS,K,VALA,0.0,360.0,'W','Azim',IER)
          CALL EGETWR(HOLDS,K,DDX,0.0,50.0,'W','length',IER)
          CALL EGETWR(HOLDS,K,DDZ,0.0,50.0,'W','width',IER)

C Set first coord at origin (lower left).
          XXW(1) = VALOX
          YYW(1) = VALOY
          ZZW(1) = VALOZ

C Set second coord along DDX metres at VALA-90.
          PI = 4.0 * ATAN(1.0)
          RAD = PI/180.
          RYAZI = (VALA-90.0)*RAD
          RSALT = 0.00
          z3 = DDX*SIN(RSALT)
          XYDIS = DDX*COS(RSALT)
          IF (ABS(XYDIS) .LT. 1E-6)THEN
            x3 = 0.
            y3 = 0.
          ELSE
            x3 = XYDIS*SIN(RYAZI)
            y3 = XYDIS*COS(RYAZI)
          ENDIF
          XXW(2)=x3+XXW(1)
          YYW(2)=y3+YYW(1)
          ZZW(2)=z3+ZZW(1)

C Third point is as second with Z adjusted. Fourth point is as first with
C Z adjusted.
          XXW(3)=XXW(2)
          YYW(3)=YYW(2)
          ZZW(3)=ZZW(2)+DDZ
          XXW(4)=XXW(1)
          YYW(4)=YYW(1)
          ZZW(4)=ZZW(1)+DDZ
        elseif(IRT.eq.6)then

C Get horizontal rectangular surface orgin.
          IF((NGT+1.GT.MGRT).OR.(NGTV+4.GT.MGTV))THEN
            CALL USRMSG(' A new surface could not be added as',
     &        ' it will make the ground or surface too complex!','W')
            RETURN
          ENDIF
          call tstamp('>','PRJ: add horizontal surface')
          HOLDS= ' 0.  0.  0. '
          CALL EASKS(HOLDS,' Surface origin X Y & Z: ',' ',
     &      36,' 0. 0. 0.  ','origin XYZ',IER,nbhelp)
          K=0
          CALL EGETWR(HOLDS,K,VALOX,-99.0,99.0,'W','X org',IER)
          CALL EGETWR(HOLDS,K,VALOY,-99.0,99.0,'W','Y org',IER)
          CALL EGETWR(HOLDS,K,VALOZ,-99.0,99.0,'W','Z org',IER)

C Get rotation and length and height.
          HOLDS= ' 0.0    1.0  1.0 '
          CALL EASKS(HOLDS,' Surface rotation (deg) length & height: ',
     &      ' ',36,' 180.0 1.0 1.0  ','rotation length height',
     &      IER,nbhelp)
          K=0
          CALL EGETWR(HOLDS,K,ANGR,0.0,360.0,'W','Rot',IER)
          CALL EGETWR(HOLDS,K,DDX,0.0,50.0,'W','length',IER)
          CALL EGETWR(HOLDS,K,DDZ,0.0,50.0,'W','width',IER)

C Set first coord at origin (lower left) and the unrotated other corners.
          XXW(1) = VALOX
          YYW(1) = VALOY
          ZZW(1) = VALOZ
          XXW(2) = VALOX+DDX
          YYW(2) = VALOY
          ZZW(2) = VALOZ
          XXW(3) = VALOX+DDX
          YYW(3) = VALOY+DDZ
          ZZW(3) = VALOZ
          XXW(4) = VALOX
          YYW(4) = VALOY+DDZ
          ZZW(4) = VALOZ

C Now rotate around the first coordinate.
          PI = 4.0 * ATAN(1.0)
          if(ANGR.LT.-.01.OR.ANGR.GT..01)then
            A=-ANGR*PI/180.
            CA=COS(A)
            SA=SIN(A)
            XX1=XXW(1)
            YY1=YYW(1)
            DO 200 I=1,4
              XXX=XXW(I)-XX1
              YYY=YYW(I)-YY1
              XR=XXX*CA+YYY*SA
              YR=YYY*CA-XXX*SA
              XXW(I)=XR+XX1
              YYW(I)=YR+YY1
  200       continue
          endif
        endif
        if(IRT.eq.5.or.IRT.eq.6)then

C Display the new rectangle.
          IAPNT = 1
          DO 350 J = 1,4
            AX(J) = XXW(J)
            AY(J) = YYW(J)
            AZ(J) = ZZW(J)
            IANXT(J) = J + 1
  350     CONTINUE
          IANXT(4) = IAPNT
          CALL MATPOL(4,IAPNT,AX,AY,AZ,IANXT,TEMAT,
     &                SBBOX,NBP,IBPNT,BX,BY,BZ,IBNXT,IERR)
          CALL CLIPFL(NBP,BX,BY,BZ,ISTAT)
          IF(ISTAT .EQ. 1)goto 32
          IF(ISTAT .EQ.-1)CALL CUTPOL(NB,NBP,IBPNT,BX,BY,BZ,IBNXT,ISTAT)

C Draw the (perhaps clipped) rectangle.
          DO 302 JB = 1,NB
            IP = IABS(IBPNT)
            CALL ORTTRN(BX(IP),BY(IP),BZ(IP),ETSMAT,AAX,AAY,AAZ,IERR)
            iupdown=0
            isym=0
            call etplot(AAX,AAY,iupdown,isym)

C Draw succeeding points until last vertex.
            IP1 = IBNXT(IP)
 451        CONTINUE
            CALL ORTTRN(BX(IP1),BY(IP1),BZ(IP1),ETSMAT,AAX,AAY,AAZ,IERR)
            iupdown=1
            isym=0
            call etplot(AAX,AAY,iupdown,isym)

            IP1 = IBNXT(IP1)
            IF(IP1 .NE. IP) GOTO 451

C Complete rec.
            CALL  ORTTRN(BX(IP),BY(IP),BZ(IP),ETSMAT,AAX,AAY,AAZ,IERR)
            iupdown=1
            call etplot(AAX,AAY,iupdown,isym)
  302     CONTINUE
   32     CONTINUE
          call forceflush()
          CALL EDISP(ITRU,
     &      ' New coords: (lower left, lower right, up right, up left')
          WRITE(OUTS,'(a,4F8.3)') ' X coords:',(XXW(I),I=1,4)
          CALL EDISP(ITRU,OUTS)
          WRITE(OUTS,'(a,4F8.3)') ' Y coords:',(YYW(I),I=1,4)
          CALL EDISP(ITRU,OUTS)
          WRITE(OUTS,'(a,4F8.3)') ' Z coords:',(ZZW(I),I=1,4)
          CALL EDISP(ITRU,OUTS)

C Confirm opening.
          CALL EASKABC('Options for this rectangle:',' ',
     &       'accept position','revise position','abort',INVT,nbhelp)
          if(INVT.eq.2)then
            ITVNO=0
            ITSNM=0
            MODIFY=.TRUE.
            if(MMOD.ge.8)then
              CALL INLNSTG(3)
              call EGRNDR(IER)
            endif
            GOTO 30
          elseif(INVT.eq.3)then
            return
          endif

C Now update the appropriate common data for the new surface. The
C following code is similar to that in addsur in edzone.f. 
          NGT=NGT+1
          is=ngt
          NGVER(NGT)=4
          DO 251 J = 1,4
            XGT(NGTV+J)=XXW(J)
            YGT(NGTV+J)=YYW(J)
            ZGT(NGTV+J)=ZZW(J)
            JGVN(NGT,J)=NGTV+J
  251     CONTINUE
          NGTV=NGTV+4
          SN=' '
          CALL EASKS(SN,' ',' Inserted surface name: ',
     &      6,'new_door','surface name',IER,nbhelp)
          GSNAME(NGT)=SN
          NV=NGVER(NGT)
          if(MMOD.ge.8)then
            CALL INLNSTG(3)
            LINSTYG(ngt)=2
            call EGRNDR(IER)
            call EGRNDR(IER)
            CALL INLNSTG(3)
          endif

          CALL EPMENSV
          CALL EPKMLC(ISEL,'Select one of the constructions for the',
     &      'inserted surface. ',IER)
          CALL EPMENRC
          IF(ISEL.GT.0)then
            WRITE(GMLCN(NGT),'(A12)')DESC(ISEL)(1:12)
          endif
        endif
        if(LASTS.ne.NGT)then
          IFILG=IFIL+1
          call EGROUT(IFILG,gtgeom,itru,IER)
          CALL EMKCFG('-',IER)
        endif
        ILEN=NGT
        IPACT=CREATE
        CALL EKPAGE(IPACT)

C Update the image.
        MODBND=.TRUE.
        MODIFY=.TRUE.
        call usrmsg(' ',' ','-')
        if(MMOD.ge.8)then
          CALL INLNST(1)
          izgfoc=0
          CALL ADJVIEW(IER)
          CALL INLNSTG(3)
          call EGRNDR(IER)
        endif
      ELSEIF(IVERT.EQ.(MVERT-6))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Edit item identified by KEYIND. Treat the list of vertices as a
C long string and then parse the data.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)

C Set all surfaces to standard line width and surface being edited to 
C a thick line. Calling egrndr twice seems to force hilight of surface.
        MODIFY=.TRUE.
        if(MMOD.ge.8)then
          LINSTYG(ifoc)=2
          call EGRNDR(IER)
          call EGRNDR(IER)
          CALL INLNSTG(3)
        endif

        helptopic='ground_surf_edge_list'
        call gethelptext(helpinsub,helptopic,nbhelp)
   93   HOLD=' '

C Incrementally write out vertex information (within 72 chars).
        NV=NGVER(IFOC)
        ix=1
        lc=3
        do 42 j=1,nv
          ixl=ix+lc
          if(j.lt.NV.and.ixl.le.72)then
            write(hold(ix:ixl),'(i4)')JGVN(IFOC,j)
            ix=ix+lc+1
          elseif(j.eq.NV.and.ixl.le.72)then
            write(hold(ix:ixl),'(i4)')JGVN(IFOC,j)
          else
            continue
          endif
  42    continue
        write(outs,'(2a)') ' Associated vertices for ',gsname(ifoc)
  43    CALL EASKS(HOLD,outs,'  ',72,' ','associated vertices',
     &    IIER,nbhelp)
        NV = iCountWords(HOLD)

C Debug.
C        write(6,*) 'found nv ',NV

        K=0
        DO 94 J=1,NV

C Read an index, check if within range and if so add to JGVN().
          CALL EGETWI(hold,K,JV,1,NGTV,'W','vertex list',IIER)
          IF(IIER.NE.0)GOTO 93
          if(JV.eq.0.or.JV.gt.NGTV)then
            call edisp(iuout,
     &        'At least one of the vertex indices was out of range!')
            goto 43
          endif
          JGVN(IFOC,J)=JV
   94   CONTINUE
        NGVER(IFOC)=NV
        MODIFY=.TRUE.
      ELSE

C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-4
      goto  92

      END 

C POINTTOGLINE: finds distance from a ground 3D point to a 3D line.
C where ipoint is the index of the test vertex, iwhich1 is the index
C of the vertex at the start of the line, iwhich2 is the index of the
C index at the end of the line, offset is the distance (m), match is
C a logical set to true if close enough.
C Only returns match=true if point was found along the line between
C the two vertices (i.e. it discards matches beyond the end points.
C It assumes that calling code will decide whether the distance
C can be used. 
      subroutine pointtogline(ipoint,iwhich1,iwhich2,offset,match)
#include "building.h"
      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)
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
      vd(1)= XGT(IWHICH2)-XGT(IWHICH1)
      vd(2)= YGT(IWHICH2)-YGT(IWHICH1)
      vd(3)= ZGT(IWHICH2)-ZGT(IWHICH1)
      call dot3(vd,vd,vdis)
      vd1(1)= XGT(IWHICH3)-XGT(IWHICH1)
      vd1(2)= YGT(IWHICH3)-YGT(IWHICH1)
      vd1(3)= ZGT(IWHICH3)-ZGT(IWHICH1)
      call dot3(vd1,vd1,vdis1)
      vd2(1)= XGT(IWHICH3)-XGT(IWHICH2)
      vd2(2)= YGT(IWHICH3)-YGT(IWHICH2)
      vd2(3)= ZGT(IWHICH3)-ZGT(IWHICH2)
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
      d2l=(vdis1-(vdis+vdis1-vdis2)*
     &    (vdis+vdis1-vdis2)/vdis/4.0)
      if(abs(d2l).lt.0.003)then
        offset=d2l
      else
        offset=SQRT(d2l)
      endif
      match=.true.
      return
      end


C ****** Routine to update-draw ground during triangulation.
      SUBROUTINE UGRNDR(IVV,NGTG,MSG,IER)
#include "building.h"
#include "prj3dv.h"
      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      DIMENSION  COG1(3),COG2(3),COG3(3)

      character*(*) msg
      character outs*72
#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
      integer iupdown,isym,iix,iiy    ! passed to etplot
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iupdown,isym,iix,iiy    ! passed to etplot
#endif

      CALL startbuffer()
      izgfoc=0
      CALL ADJVIEW(IER)
      WRITE(outs,'(A,i3,A,i3)')' Verts: ',IVV,' surfs ',NGTG
      iside=1
      isize=0
      ifont=1
      call viewtext(outs,iside,isize,ifont)
      iside=2
      call viewtext(msg,iside,isize,ifont)

      if(IVV.eq.0)return
      do 42 iv = 1,IVV
        COG1(1)=XGT(IV)
        COG1(2)=YGT(IV)
        COG1(3)=ZGT(IV)
        CALL VECTRN(COG1,TEMAT,COG2,IER)
        CALL VECTRN(COG2,ETSMAT,COG3,IER)
        call u2pixel(COG3(1),COG3(2),iix,iiy)
        CALL ecirc(iix,iiy,2,1)
        CALL VERTLBL(iix,iiy,IV,ier)
        call forceflush()
  42  continue
      if(NGTG.eq.0)return
      do 43 igs = 1, NGTG
        DO 1100 J = 1,3
          COG1(1)=XGT(JGVN(igs,J))
          COG1(2)=YGT(JGVN(igs,J))
          COG1(3)=ZGT(JGVN(igs,J))
          CALL VECTRN(COG1,TEMAT,COG2,IER)
          CALL VECTRN(COG2,ETSMAT,COG3,IER)
          if(J.eq.1)then
            iupdown=0
            isym=0
            call etplot(COG3(1),COG3(2),iupdown,isym)
          else
            iupdown=1
            isym=0
            call etplot(COG3(1),COG3(2),iupdown,isym)
          endif
 1100   CONTINUE

C Finish off the surface.
        COG1(1)=XGT(JGVN(igs,1))
        COG1(2)=YGT(JGVN(igs,1))
        COG1(3)=ZGT(JGVN(igs,1))
        CALL VECTRN(COG1,TEMAT,COG2,IER)
        CALL VECTRN(COG2,ETSMAT,COG3,IER)
        iupdown=1
        isym=0
        call etplot(COG3(1),COG3(2),iupdown,isym)
  43  continue
      call forceflush()
      call pauses(1)

      return
      end

C ******************** TRIANG
C TRIANG takes a list of vertices defined in V(x,y,z) and creates
C a RADIANCE description of the plane described by these points by 
C forming triangles through the vertices.
C THE VERTEX LIST CONTAINS I VERTICES, THE TRIANGLE LIST WILL CONTAIN
C LESS THAN 2*I TRIANGLES the triangle list is stored in T(IT,3)
C  SCR    : super circle radius
C  IV     : number of sample vertices
C  NT     : number of triangles defined
C  XGT,YGT,ZGT  : arrays of sample vertex coords
C  JGVN   : array of vertices used in each triangle
C  CENT   : array of triangle circumcenter coords
C  CCR    : array of circumcenters radius
C  CG     : center of super sphere
C  LVERT  : logical if vertex has been used or not
C  IVSEL  : array of triangle vertices for new triangles
C  ITSEL  : array of triangles to be removed
C  NVSEL   : number of sellected vertices for array IVSEL
C  NTSEL   : number of sellected triangles in array ITSEL
C  PT*    : a single vertex - for compatability
C  newvert: chosen vertex as vert 3 for new triangle

C input: vertex list and number of vertices
C output: triangle list
      SUBROUTINE TRIANG(ier)
#include "building.h"
#include "prj3dv.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)
      COMMON/GT5/GSNAME(MGRT),GMLCN(MGRT)
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX

      INTEGER NVSEL, NTSEL, VLIST(90), CLOSE, CENTER, TRIANGLE
      REAL DIST, TRI(12), DX

      DIMENSION CG(2), CCR(MGRT), CENT(MGRT,2), IVSEL(90)
      DIMENSION PTA(2), ITSEL(90), ITCHK(90)
      CHARACTER GMLCN*12,GSNAME*6,msg*32,outs*124
      LOGICAL LVERT(MGTV), INPOLY
      logical MODIFY,MODLEN,MODBND,OK,dok

#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
#else
      integer*8 igwid,igheight  ! for use with axiscale
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='TRIANG'  ! set for subroutine

C If there is a ground geometry then check its bounds.
      do 741 iv = 1,NGTV
        XMN=AMIN1(XMN,XGT(iv))
        YMN=AMIN1(YMN,YGT(iv))
        ZMN=AMIN1(ZMN,ZGT(iv))
        XMX=AMAX1(XMX,XGT(iv))
        YMX=AMAX1(YMX,YGT(iv))
        ZMX=AMAX1(ZMX,ZGT(iv))
 741  continue

      ier=0

C Clear the logical array LVERT.
      do 2, I=1,MGTV
        LVERT(I)=.FALSE.
   2  continue

C Set the sellected vertices all to vertex 0 - ie none selected
      DO 3, I=1,90
        VLIST(I)=0
        IVSEL(I)=0
   3  CONTINUE

C Set the centers and radii of the circumcenters to zero and clear 
C any existing triangles from the common blocks.
      NGT=0
      DO 5, I=1,MGRT
        CCR(I)=0.0
        CENT(I,1)=0.0
        CENT(I,2)=0.0
        JGVN(I,1)=0
        JGVN(I,2)=0
        JGVN(I,3)=0
        JGVN(I,4)=0
        JGVN(I,5)=0
        JGVN(I,6)=0
        JGVN(I,7)=0
        JGVN(I,8)=0
   5  CONTINUE

C find the center of gravity of the vertices CG and then find the 
C most distant point from CG - this will be the radius of the 
C supercircle.
      CG(1)=0.0
      CG(2)=0.0
      DO 55 I=1,NGTV
        CG(1)=CG(1)+XGT(I)
        CG(2)=CG(2)+YGT(I)
   55 CONTINUE

C Divide by the number of vertices to get C of G, set view point.
      CG(1)=CG(1)/(NGTV)
      CG(2)=CG(2)/(NGTV)
      if(MMOD.ge.8)then
        oldv1=VIEWM(1)
        oldv2=VIEWM(2)
        oldv3=VIEWM(3)
        oldang=ANG
        VIEWM(1)= CG(1)
        VIEWM(2)= CG(2)
        VIEWM(3)= 1.
        MODLEN=.TRUE.
        MODIFY=.TRUE.
        MODBND=.TRUE.

C If viewpoint or bounds different then initialise viewing paramters.
        IF(MODLEN)THEN
          HANG=ANG/2.0
          CALL LENS(IER)
        ENDIF

C Clear current viewing box.
        CALL startbuffer()
        CALL SITE2D(SXMX,SXMN,SYMX,SYMN,ier)
        igwid=igw
        igheight=igwh
        call axiscale(igwid,igheight,SXMN,SXMX,SYMN,SYMX,xsc,ysc,sca,
     &                Xadd,Yadd)
        call linescale(igl,Xadd,sca,igb,Yadd,sca)

        OK=.FALSE.
        dok=.false.
        helptopic='ground_include_indices'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL ASKOK(' ','Draw vertices?',OK,dok,nbhelp)
        IF(OK)then
          ix=NGTV
          call UGRNDR(ix,0,'initial',IER)
        endif
      endif

C Find the closest vertex to the center of gravity, that is not on 
C the boundary of the ground plane.
      DIST=10000.0
      DO 60 I=1,NGTV
        IF (IVEDGE(I).NE.1) THEN
          PTA(1)=XGT(I)
          PTA(2)=YGT(I)
          CROWDD = (CG(1)-PTA(1))**2 + (CG(2)-PTA(2))**2 
          DX=SQRT(CROWDD)
          IF (DX.LT.DIST) THEN
            DIST=DX
            CLOSE=I
          ENDIF
        ENDIF
  60  CONTINUE
      
C Make initial triangles using this point as the center and the 
C boundary verts as those selected.
      DO 70 I=1,NGTV
        IF (IVEDGE(I).EQ.1) THEN
          NVERT=NVERT+1
          VLIST(NVERT)=I
        ENDIF
  70  CONTINUE
      CALL MKTRI(CLOSE,NVERT,VLIST,CCR,CENT)

      DO 72 I=1,NVERT
        LVERT(VLIST(I))=.TRUE.
  72  CONTINUE
      LVERT(CLOSE)=.TRUE.


C Start looping to add all the remaining vertices to the surface 
C discription.
 150  DO 18, CENTER=1,(NGTV)

C First clear out the array of sellected vertices IVSEL() and 
C set number of sellected verts to zero.
        DO 180, I=1,90
          IVSEL(I)=0
 180    CONTINUE
        NVSEL=0

C and clear array of selected triangles and triangles for checking.
        DO 181, I=1,90
          ITSEL(I)=0
          ITCHK(I)=0
 181    CONTINUE
        NTCHK=0
        NTSEL=0

C If vertex X,Y,ZGT(CENTER) is unused use it.
        IF (.NOT.LVERT(CENTER)) THEN

C Find triangles associated with VERTEX COUNTER = CENTER.
C We will use the intersection logical function INPOLY.
        DO 182, TRIANGLE=1,NGT
          DO 183, J=1,3
            TRI(J*2-1)=XGT(JGVN(TRIANGLE,J))
            TRI(J*2)=YGT(JGVN(TRIANGLE,J))
 183      CONTINUE
          IF (INPOLY(XGT(CENTER),YGT(CENTER),3,TRI)) THEN
            LVERT(CENTER)=.TRUE.
            NTSEL=NTSEL+1
            ITSEL(NTSEL)=TRIANGLE
            NTCHK=NTCHK+1
            ITCHK(NTCHK)=TRIANGLE

C A triangle has been sellected, check if already sellected
            IF (NTSEL.EQ.1) THEN
              NVSEL=3
              IVSEL(1)=JGVN(TRIANGLE,1)
              IVSEL(2)=JGVN(TRIANGLE,2)
              IVSEL(3)=JGVN(TRIANGLE,3)
            ELSE

C Check to see if vertex is chosen already.
              DO 1834, K=1,3
                DO 1831, L=1,NVSEL
                  IF (JGVN(TRIANGLE,K).EQ.IVSEL(L)) GOTO 1836
1831            CONTINUE

C Add the vertex to the list of chosen ones IVSEL. The selection 
C triangle list should not be incremented without checking to see if 
C triangle is chosen already.
                NVSEL = NVSEL + 1
                IVSEL(NVSEL)=JGVN(TRIANGLE,K)
1836            CONTINUE
1834          CONTINUE
              ENDIF
            ENDIF
 182      CONTINUE

C Form new triangles by joining CENTER to all vertices listed
C in IVSEL().
          CALL MKTRI(CENTER,NVSEL,IVSEL,CCR,CENT)

C ****************************************************************
C get a list of the new triangles made in MKTRI and check them in CHKTRI
C if triangles are changed then add them to the list for checking...

C Check the new triangles are OK.
          CALL CHKTRI(NTCHK,ITCHK,CENTER,CCR,CENT,NTSEL,ITSEL,ier)

C remove the triangles listed in ITSEL
          CALL RMTRI(NTSEL,ITSEL,CCR,CENT)
        ENDIF 

C Debug.
C        DO 334, IPL=1, NGT
C          write(6,*)'W: triangle no. and verts ',IPL ,JGVN(IPL,1) ,
C     &    JGVN(IPL,2) ,JGVN(IPL,3)

C 334    CONTINUE
        ivx=NGTV
        isurx=NGT
        write(msg,'(a,3i4)') 'updating for ',ivx,isurx,center
        call UGRNDR(ivx,isurx,msg,IER)
  18  CONTINUE

C Debug.
C      write(6,*)'D: total triangles = ',NGT,' vertices=',NGTV

C Finally pass the number of triangles back to the common block NGT
C and give each a name and composition.
      if(MMOD.ge.8)then
        CALL startbuffer()
        call UGRNDR(NGTV,NGT,'parsed all triangles',IER)
      endif

C Finally check to see if all the vertices have been used.
      DO 111, I=1,NGTV
        IF (.NOT.LVERT(I)) THEN
          write(outs,*)' Vertex number is unused ',I
          call edisp(iuout,outs)
          dok=.true.
          helptopic='ground_alt_logic_triang'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call askok('Try again to find this vertex?',' ',
     &      ok,dok,nbhelp)
          if(ok)goto 150
        ELSE
          write(outs,*)' Vertices number has been used ',I
          call edisp(iuout,outs)
        ENDIF
 111  CONTINUE

      do 642 inn = 1, NGT
       IF(inn.LE.9)WRITE(GSNAME(inn),'(A3,I1)') 'gs-',inn
       IF(inn.GT.9.and.inn.LE.99)WRITE(GSNAME(inn),'(A3,I2)') 'gs-',inn
       IF(inn.GT.99)WRITE(GSNAME(inn),'(A3,I3)') 'gs-',inn
       GMLCN(inn) = 'UNKNOWN'
       NGVER(inn) = 3
 642  continue

C Restore view point.
      VIEWM(1)=oldv1
      VIEWM(2)=oldv2
      VIEWM(3)=oldv3
      ANG=oldang
      MODIFY=.TRUE.
      MODLEN=.TRUE.
      MODBND=.TRUE.

      RETURN
      END

C ******************** MKTRI
C MKTRI creates N triangles from the list VLIST and puts the data 
C into the common block /grndpln/.

C Form new triangles by joining CENTER to all vertices listed
C in IVSEL().

      SUBROUTINE MKTRI(CENTER,NVERT,VLIST,CCR,CENT)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)

      REAL CCR(MGRT), CENT(MGRT,2), LARGEANG, ANGT
      REAL ELEV, ANGLE(90)

      INTEGER VLIST(90), NVERT, NMK, VMK(90), CENTER, LARGEVERT

      DIMENSION PTA(2), PTB(2), PTC(2), PTD(2)
      LOGICAL LVLIST(90)
      character outs*124

      DO 10 I=1,90
        LVLIST(I)=.FALSE.
        VMK(I)=0
        ANGLE(I)=0.0
  10  CONTINUE

      NMK = 0
      DO 15 K=1,NVERT
      LARGEVERT=0
      LARGEANG=-10.0
      DO 20 I=1,NVERT
        IF (.NOT.LVLIST(I)) THEN
          vdx=XGT(VLIST(I))-XGT(CENTER)
          vdy=YGT(VLIST(I))-YGT(CENTER)
          vdz=0.0
          ANGT=0.0
          elev=0.0
          CALL UV2AZ(vdx,vdy,vdz,ANGT,elev)

          IF (ANGT.GT.LARGEANG) THEN
            LARGEANG = ANGT
            LARGEVERT = I
            write(outs,*)'Found large angle ',LARGEANG,VLIST(LARGEVERT)
            call edisp(iuout,outs)
          ENDIF
        ENDIF
  20  CONTINUE
      LVLIST(LARGEVERT)=.TRUE.
      NMK=NMK+1
      VMK(NMK)=VLIST(LARGEVERT)
      ANGLE(NMK)=LARGEANG
      write(outs,*)'Ordered selected verts ',VLIST(LARGEVERT)
      call edisp(iuout,outs)
  15  CONTINUE

C Check to see if the triangle has no internal angles greater than
C 180deg before forming.
      DO 35, K=1,NMK

C The triangle has an angle greater than 180deg therefore do not 
C make it, or less than 0deg
        IF ((K+1).GT.NMK) THEN
          ANGT=ANGLE(K)-ANGLE(1) 
          ANGT=ANGT+360.
        ELSE
          ANGT=ANGLE(K)-ANGLE(K+1) 
        ENDIF

        IF (ANGT.GE.180.0) THEN
          write(outs,*)'triangle has angle greater than 180degs '
          call edisp(iuout,outs)
        ELSEIF (angt.le.0.0) THEN
          write(outs,*)'triangle has angle less than 0degs '
          call edisp(iuout,outs)
        ELSE

C Forming new triangle => increase total number of triangles
C The vertices to use are in order in the array VMK and there 
C are VSEL of them.
         NGT = NGT + 1
         JGVN(NGT,1)=CENTER
         JGVN(NGT,2)=VMK(K)
         IF ((K + 1).GT.NMK) THEN
           JGVN(NGT,3)=VMK(1)
         ELSE
           JGVN(NGT,3)=VMK(K + 1)
         ENDIF

C calculate the new circumcenter and radius of JGVN(NGT)
C Convert vertex array first
         PTA(1)=XGT(JGVN(NGT,1))
         PTA(2)=YGT(JGVN(NGT,1))
         PTB(1)=XGT(JGVN(NGT,2))
         PTB(2)=YGT(JGVN(NGT,2))
         PTC(1)=XGT(JGVN(NGT,3))
         PTC(2)=YGT(JGVN(NGT,3))
         CALL CIRCUM(PTA,PTB,PTC,PTD,RAD)
         CENT(NGT,1)=PTD(1)
         CENT(NGT,2)=PTD(2)
         CCR(NGT)=RAD

C Debug.
C          write(6,*)'++ formed new triangle ',NGT,JGVN(NGT,1),
C     & JGVN(NGT,2),JGVN(NGT,3),' @ ',CENT(NGT,1),CENT(NGT,2),CCR(NGT)
     
        ENDIF

  35  CONTINUE
      
      RETURN
      END

C ******************** CHKTRI
C CHKTRI checks that the new triangle NTRI does not break the Delauny 
C stipulation of an empty circumcenter.

      SUBROUTINE CHKTRI(NTCHK,ITCHK,POINT,CCR,CENT,NTSEL,ITSEL,ier)
#include "building.h"

      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)

      REAL CCR(MGRT), CENT(MGRT,2), PTA(2), PTB(2), PTC(2), PTD(2)

      INTEGER SELTRI(MGRT,3), ADJTRI, ITCHK(90), OLDTRI, POINT
      INTEGER ITSEL(90)

      ier=0
      JCHK=1
C Start checking the triangles in ITCHK
   8  IF (JCHK.LE.NTCHK) THEN
        OLDTRI=ITCHK(JCHK)
      
C Find a triangle adjacent to triangle OLDTRI. This is 
C done by searching for shared vertices.
C Zero adjacency counter SELTRI
        DO 10, I=1,NGT
          DO 12, J=1,3
            SELTRI(I,J)=0
  12      CONTINUE
  10    CONTINUE
        DO 20, I=1,NGT
          DO 30, J=1,3
            IF (JGVN(I,1).EQ.JGVN(OLDTRI,J))  SELTRI(I,1)=1
            IF (JGVN(I,2).EQ.JGVN(OLDTRI,J))  SELTRI(I,2)=1
            IF (JGVN(I,3).EQ.JGVN(OLDTRI,J))  SELTRI(I,3)=1
            IF (JGVN(I,1).EQ.POINT)  SELTRI(I,1)=1
            IF (JGVN(I,2).EQ.POINT)  SELTRI(I,2)=1
            IF (JGVN(I,3).EQ.POINT)  SELTRI(I,3)=1
  30      CONTINUE
  20    CONTINUE

C Find adjacent triangle.
        DO 40, ADJTRI=1,NGT
          ITRI=0
          DO 42, J=1,3
            ITRI=ITRI+SELTRI(ADJTRI,J)
  42      CONTINUE
          IF (ITRI.EQ.2) THEN

C Found adjacent triangle => test to see if this triangle is in list ITSEL 
C if it is ignore it else test circumcircle.
            DO 43, IA=1,90
              IF (ITSEL(IA).EQ.ADJTRI) GOTO 40
  43        CONTINUE

C Find distance from this point to the circumcenter of the new triangle.
            PTA(1)=XGT(POINT)
            PTA(2)=YGT(POINT)
            PTB(1)=CENT(ADJTRI,1)
            PTB(2)=CENT(ADJTRI,2)

            CROWDD = (PTA(1)-PTB(1))**2 + (PTA(2)-PTB(2))**2 
            X=SQRT(CROWDD)
            IF (X.LT.CCR(ADJTRI)) THEN

C Not Delauny triangles => make new triangles.
C Forming new triangle => increase total number of triangles
              DO 52, I=1,3
                IF (SELTRI(ADJTRI,I).EQ.1) THEN
                  NGT = NGT + 1
                  JGVN(NGT,1)=JGVN(ADJTRI,I)
                  IF ((I+1).GT.3) THEN
                    IT=1
                  ELSE 
                    IT=I+1
                  ENDIF
                  IF (SELTRI(ADJTRI,IT).EQ.0) THEN
                    JGVN(NGT,2)=JGVN(ADJTRI,IT)
                    JGVN(NGT,3)=POINT
                  ELSE
                    JGVN(NGT,2)=POINT
                    IF ((IT+1).GT.3) THEN
                      ITT=1
                    ELSE 
                      ITT=IT+1
                    ENDIF
                    JGVN(NGT,3)=JGVN(ADJTRI,ITT)
                  ENDIF

C Calculate the new circumcenter and radius of JGVN(NGT)
C Convert vertex array first
                  PTA(1)=XGT(JGVN(NGT,1))
                  PTA(2)=YGT(JGVN(NGT,1))
                  PTB(1)=XGT(JGVN(NGT,2))
                  PTB(2)=YGT(JGVN(NGT,2))
                  PTC(1)=XGT(JGVN(NGT,3))
                  PTC(2)=YGT(JGVN(NGT,3))
                  CALL CIRCUM(PTA,PTB,PTC,PTD,RAD)
                  CENT(NGT,1)=PTD(1)
                  CENT(NGT,2)=PTD(2)
                  CCR(NGT)=RAD
                ENDIF
  52          CONTINUE

C Remove old triangles from the master list - the adjacent triangle should 
C be removed and the triangle formed by the adjacent vertices and the 
C vertex POINT.
              NTSEL=NTSEL+1
              ITSEL(NTSEL)=ADJTRI

C Add adjacent triangle to list of old triangles - ie check it's adjacent 
C triangles for Delauney-ness.
              NTCHK=NTCHK+1
              ITCHK(NTCHK)=ADJTRI

C Remove the old adjacent triangle from the list ITCHK and 
C decrease NTCHK by one. 
              DO 520, IZ=JCHK,NTCHK
                IF (IZ+1.GT.NTCHK) THEN
                  ITCHK(IZ)=0
                ELSE
                  ITCHK(IZ)=ITCHK(IZ+1)
                ENDIF
 520          CONTINUE
              NTCHK=NTCHK-1

C find new triangle then compare with ADJTRI
              IVA=0
              IVB=0
              IF (SELTRI(ADJTRI,1).EQ.1) IVA=JGVN(ADJTRI,1)
              IF (SELTRI(ADJTRI,2).EQ.1) THEN
                IF (IVA.EQ.0) THEN 
                  IVA=JGVN(ADJTRI,2)
                ELSE
                  IVB=JGVN(ADJTRI,2)
                ENDIF
              ENDIF
              IF (SELTRI(ADJTRI,3).EQ.1) IVB=JGVN(ADJTRI,3)

C Debug.
C              write(6,*) 'vertices A,B,P: ',IVA,IVB,POINT

C Look for triangle containing vertices IVA, IVB and POINT
              DO 400, IA=1,NGT
                DO 405, IB=1,3
                  IF(JGVN(IA,IB).EQ.POINT) THEN
                    DO 407, IC=1,3
                      IF(JGVN(IA,IC).EQ.IVA) THEN
                        DO 409, ID=1,3
                          IF(JGVN(IA,ID).EQ.IVB) THEN
                            NTSEL=NTSEL+1
                            ITSEL(NTSEL)=IA
                          ENDIF
 409                    CONTINUE
                      ENDIF
 407                CONTINUE
                  ENDIF
 405            CONTINUE
 400          CONTINUE
              GOTO 40
            ENDIF
          ENDIF
  40    CONTINUE
        JCHK=JCHK+1
        GOTO 8
      ENDIF

C Clear out triangle's to check array as all triangles have now been checked.
      NTCHK=0
      DO 55, IA=1,90
        ITCHK(IA)=0
  55  CONTINUE

      RETURN
      END

C ******************** RMTRI
C RMTRI removes N triangles in the list TLIST from the triangles 
C kept in the common block /grndpln/.
C The list of triangles must be sorted first into a list of decreasing 
C values.

      SUBROUTINE RMTRI(NDEL,TDEL,CCR,CENT)
#include "building.h"

      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)

      REAL CCR(MGRT), CENT(MGRT,2)
      INTEGER TDEL(90), NDEL, NSWFLG, ITEMP, RPTFLG

C Sort triangle list into decreasing order.
  10  NSWFLG=0
      DO 15, I=1,NDEL
        IF (TDEL(I).LT.TDEL(I+1)) THEN
          ITEMP = TDEL(I)
          TDEL(I) = TDEL(I+1)
          TDEL(I+1) = ITEMP
          NSWFLG = 1
        ENDIF
  15  CONTINUE
      IF (NSWFLG.EQ.1) GOTO 10

C Check that no number is repeated in the triangle list.
  20  RPTFLG = 0
      DO 25, I=1,NDEL
        IF (TDEL(I).EQ.TDEL(I+1)) THEN
          DO 27, J=I,NDEL
            IF (J+1.GT.NDEL) THEN
              TDEL(J)=0
            ELSE
              TDEL(J)=TDEL(J+1)
            ENDIF
  27      CONTINUE
          NDEL = NDEL - 1
          RPTFLG = 1
        ENDIF
  25  CONTINUE
      IF (RPTFLG.EQ.1) GOTO 20

C Remove the triangles...
      DO 30 L=1,NDEL
        DO 40, K=TDEL(L),NGT
          IF ((K+1).GT.NGT) THEN
            JGVN(K,1)=0
            JGVN(K,2)=0
            JGVN(K,3)=0
            CCR(K)=0.0
            CENT(K,1)=0.0
            CENT(K,2)=0.0
          ELSE
            JGVN(K,1)=JGVN(K+1,1)
            JGVN(K,2)=JGVN(K+1,2)
            JGVN(K,3)=JGVN(K+1,3)
            CCR(K)=CCR(K+1)
            CENT(K,1)=CENT(K+1,1)
            CENT(K,2)=CENT(K+1,2)
          ENDIF
  40    CONTINUE

C Reduce the triangle identifier by one to account for the removed 
C triangle.
      NGT = NGT - 1

C Debug.
C      write(6,*)'-- triangle ',TDEL(L),' removed '

  30  CONTINUE
      RETURN
      END

C ******************** CIRCUM
C CIRCUM calculates the circumcircle center and radius for a triangle 
C whos vertices are A(x,y) B(x,y) and C(x,y) 
C the result is returned in D(x,y) and RAD
C the array V contains vector information
C the array M contains the mid-point coords of two sides of the 
C triangle
      SUBROUTINE CIRCUM(A,B,C,D,RAD)

      REAL A, B, C, D, MAB, MAC
      REAL X, Y, RAD, GPAB, GPAC
      DIMENSION A(2), B(2), C(2), D(2), MAB(2), MAC(2)
      
C Find the mid-points of two sides of the triangle.
        MAB(1)=(A(1) + B(1))/2.
        MAB(2)=(A(2) + B(2))/2.

        MAC(1)=(A(1) + C(1))/2.
        MAC(2)=(A(2) + C(2))/2.

C line equations...
C      Y(1)=GPAB*X - GPAB * MAB(1) + MAB(2)
C      Y(2)=GPAC*X - GPAC * MAC(1) + MAC(2)
C Gradients 1 and 2 are perp to sides of the triangle,
C gradient could be infinity => special cases.

C Is the line between A and B vertical?
      IF (ABS(B(1)-A(1)).LT.0.001) THEN
        GPAC=(C(2)-A(2))/(C(1)-A(1))
        IF (ABS(GPAC).LT.0.001) THEN
          Y = (A(2)+B(2))/2.
          X = (A(1)+C(1))/2.
        ELSE  
          GPAC=-1/GPAC
          Y = (A(2)+B(2))/2.
          X = ((Y - MAC(2))/GPAC) + MAC(1)
        ENDIF

C Is the line between A and C vertical?
      ELSEIF (ABS(C(1)-A(1)).LT.0.001) THEN
        GPAB=(B(2)-A(2))/(B(1)-A(1))
        IF (ABS(GPAB).LT.0.001) THEN
          Y = (A(2)+C(2))/2.
          X = (A(1)+B(1))/2.
        ELSE  
          GPAB=-1/GPAB
          Y = (A(2)+C(2))/2.
          X = ((Y - MAB(2))/GPAB) + MAB(1)
        ENDIF

C Is the line between A and B horizontal?
      ELSEIF (ABS(B(2)-A(2)).LT.0.001) THEN
        GPAC=(C(2)-A(2))/(C(1)-A(1))
        GPAC=-1/GPAC
        X = (A(1)+B(1))/2.
        Y = GPAC * ( X - MAC(1)) + MAC(2)

C Is the line between A and C horizontal?
      ELSEIF (ABS(C(2)-A(2)).LT.0.001) THEN
        GPAB=(B(2)-A(2))/(B(1)-A(1))
        GPAB=-1/GPAB
        X = (A(1)+C(1))/2.
        Y = GPAB * ( X - MAB(1)) + MAB(2)

      ELSE
        GPAB=(B(2)-A(2))/(B(1)-A(1))
        GPAB=-1/GPAB
        GPAC=(C(2)-A(2))/(C(1)-A(1))
        GPAC=-1/GPAC

C Define the two lines thru the midpoints perp to the sides of 
C the triangle.
        X=((GPAB*MAB(1))- MAB(2) -(GPAC*MAC(1))+ MAC(2))/
     &        (GPAB - GPAC)
        Y=GPAB*X -(GPAB*MAB(1))+ MAB(2)
      ENDIF

      D(1) = X
      D(2) = Y


C Calculate the radius of the circumcenter
      RAD= SQRT((D(1)-A(1))**2+(D(2)-A(2))**2)

C Debug.
C      write(6,*)'J: vertex A ',A(1), A(2)
C      write(6,*)'J: vertex B ',B(1), B(2)
C      write(6,*)'J: vertex C ',C(1), C(2)
C      write(6,*)'J: circumcenter ',D(1), D(2)
C      write(6,*)'J: radius ',RAD

      RETURN
      END

C ******************** INPOLY
C INPOLY Polygon point containment test. THIS WILL ONLY WORK FOR A 
C ** CONVEX ** POLYGON.
C Returns .TRUE. or .FALSE. depending on whether (X,Y) is inside
C the polygon or not.
C NV is the number of vertices in the polygon, and VLIST is a 
C list of the x,y coords of the verts - i.e.
C VLIST(1)=X1
C VLIST(2)=Y1
C VLIST(3)=X2 ...etc

      LOGICAL FUNCTION  INPOLY(X,Y,NV,VLIST)

      REAL  VLIST(12)

      INPOLY = .FALSE.
      NVM=NV*2

C Test that the vector to the vertex X,Y is between the two sides 
C of the polygon from each vertex on the polygon.

      DO 10, I=1,NV
        IF (I*2+1.GT.NVM) THEN
          VAX=VLIST(I*2-1)
          VAY=VLIST(I*2)
          VBX=VLIST(1)
          VBY=VLIST(2)
          VCX=VLIST(3)
          VCY=VLIST(4)
        ELSEIF (I*2+3.GT.NVM) THEN
          VAX=VLIST(I*2-1)
          VAY=VLIST(I*2)
          VBX=VLIST(I*2+1)
          VBY=VLIST(I*2+2)
          VCX=VLIST(1)
          VCY=VLIST(2)
        ELSE
          VAX=VLIST(I*2-1)
          VAY=VLIST(I*2)
          VBX=VLIST(I*2+1)
          VBY=VLIST(I*2+2)
          VCX=VLIST(I*2+3)
          VCY=VLIST(I*2+4)
        ENDIF
C Calculate angles (rotation from +ive x=0 clockwise).

        vdx=X-VAX
        vdy=Y-VAY
        vdz=0.0
        ANGA=0.0
        elev=0.0
        CALL UV2AZ(vdx,vdy,vdz,ANGA,elev)
        vdx=VBX-VAX
        vdy=VBY-VAY
        vdz=0.0
        ANGB=0.0
        elev=0.0
        CALL UV2AZ(vdx,vdy,vdz,ANGB,elev)
        vdx=VCX-VAX
        vdy=VCY-VAY
        vdz=0.0
        ANGC=0.0
        elev=0.0
        CALL UV2AZ(vdx,vdy,vdz,ANGC,elev)
C Check that the angles are in the correct order. ANGB will 
C always be larger than ANCC.

C Debug.
C        write(6,*) 'angles >A, >B, >C: ', ANGA, ANGB, ANGC

        IF (ANGB.LT.ANGC) THEN
          ANGB=ANGB+360.0
          IF (ANGA.LT.ANGC) ANGA=ANGA+360.0
        ENDIF
        IF (ANGA.LT.ANGC.OR.ANGA.GT.ANGB) THEN
          INPOLY = .FALSE.
          RETURN
        ENDIF
 10   CONTINUE

      INPOLY = .TRUE.

      RETURN
      END

C ********************* TRANGSUR 
C TRANGSUR transforms a ground surface along its normal.
C OFFSET is the offset from the base polygon (0.02 typically).
C NB       = no. base surf vertices,
C XB,YB,ZB = base surf vertex arrays,
C XT,YT,ZT =  transformed surf vertex arrays,
      SUBROUTINE TRANGSUR(ITRC,ITRU,NB,XB,YB,ZB,OFFSET,XT,YT,ZT,gsname)
#include "building.h"
      DIMENSION  XB(8),YB(8),ZB(8),XT(8),YT(8),ZT(8)
      DIMENSION  VP(3),EQN(4),TRNS(3)
      CHARACTER gsname*6,outs*144,out96*96

C Find transformation matrices that normalise ground surface face.
      call PLEQNG(XB,YB,ZB,NB,VP,EQN,IERR)

C If error, return transformed points same as initial.
      IF (IERR .NE. 0)then
        write(outs,'(a,a)') ' PLEQN problem with ',gsname
        call edisp(itru,outs)
        do 354 k=1,NB
          XT(k)=XB(k)
          YT(k)=YB(k)
          ZT(k)=ZB(k)
  354   continue
        return
      endif
      DO 352 K = 1,3
        TRNS(k)=EQN(k)*OFFSET
  352 continue
      do 353 k=1,NB
        XT(k)=XB(k)+TRNS(1)
        YT(k)=YB(k)+TRNS(2)
        ZT(k)=ZB(k)+TRNS(3)
  353 continue

      IF(ITRC.GT.1)THEN
        CALL EDISP(ITRU,' Plane equation data: ')
        WRITE(outs,'(a,3F8.3)')' Center of grav:',(VP(I),I=1,3)
        CALL EDISP(ITRU,outs)
        WRITE(outs,'(a,4F8.3)')' Equation:',(EQN(I),I=1,4)
        CALL EDISP(ITRU,outs)
        CALL EDISP(ITRU,' Transforming coords: ')
        ipos=1
        call arlist(ipos,NB,XT,MV,'S',out96,loutlen,itrunc)
        WRITE(outs,'(a,20F7.3)') ' X coords:',out96(1:loutlen)
        CALL EDISP(ITRU,outs)
        call arlist(ipos,NB,YT,MV,'S',out96,loutlen,itrunc)
        WRITE(outs,'(a,20F7.3)') ' Y coords:',out96(1:loutlen)
        CALL EDISP(ITRU,outs)
        call arlist(ipos,NB,ZT,MV,'S',out96,loutlen,itrunc)
        WRITE(outs,'(a,20F7.3)') ' Z coords:',out96(1:loutlen)
        CALL EDISP(ITRU,outs)
      ENDIF

      RETURN
      END


C ********************  PLEQNG
C PLEQNG finds the equation EQN to a ground surface polygon which
C is defined as a set of X() Y() Z() verticies tracing in order the 
C edges of the polygon.  The plane is the best fit from points and its 
C equation is in the form:
C       A*X + B*Y + C*Z = D
C where the vector (A B C) is the unit normal vector to the plane.
C The normal will point out from the surface if the vertices are 
C passed in anti-clockwise.

      SUBROUTINE PLEQNG(X,Y,Z,NP,CG,EQN,IERPLN)

      DIMENSION X(8), Y(8), Z(8), CG(3), EQN(4)
      DIMENSION VA(3), VB(3), VC(3), VN(3)
      logical close

C Initialise.
      IERPLN=0
      CG(1)=0.
      CG(2)=0.
      CG(3)=0.
      EQN(1)=0.
      EQN(2)=0.
      EQN(3)=0.
      EQN(4)=0.
      VN(1)=0.
      VN(2)=0.
      VN(3)=0.

C Check that we have some points.
      if (NP.eq.0) then
        IERPLN=1
        RETURN
      endif

C Find the center of gravity.
      do 10 I=1,NP
        CG(1)=CG(1)+X(I)
        CG(2)=CG(2)+Y(I)
        CG(3)=CG(3)+Z(I)
 10   continue
      do 20 I=1,3
        CG(I)=CG(I)/NP
 20   continue

C Loop through vertices calculating cross product.
      do 30 I=1,NP
        if ((I+1).gt.NP) then
          VA(1)=X(1)-X(I)
          VA(2)=Y(1)-Y(I)
          VA(3)=Z(1)-Z(I)
          VB(1)=X(2)-X(1)
          VB(2)=Y(2)-Y(1)
          VB(3)=Z(2)-Z(1)
        elseif ((I+1).eq.NP) then
          VA(1)=X(I+1)-X(I)
          VA(2)=Y(I+1)-Y(I)
          VA(3)=Z(I+1)-Z(I)
          VB(1)=X(1)-X(I+1)
          VB(2)=Y(1)-Y(I+1)
          VB(3)=Z(1)-Z(I+1)
        else
          VA(1)=X(I+1)-X(I)
          VA(2)=Y(I+1)-Y(I)
          VA(3)=Z(I+1)-Z(I)
          VB(1)=X(I+2)-X(I+1)
          VB(2)=Y(I+2)-Y(I+1)
          VB(3)=Z(I+2)-Z(I+1)
        endif

C Do cross product and if VC is not close to zero then
C normalise it (so that a polygon with long and short 
C edges is treated correctly.
        call CROSS(VA,VB,VC)
        SNORM=0.
        SNORM=sqrt(VC(1)**2+VC(2)**2+VC(3)**2)
        close=.false.
        call eclose(SNORM,0.0,0.0001,close)
        if(.not.close)then
          do 50 J=1,3
            VC(J)=VC(J)/SNORM
 50       continue
        endif

C Add to VN the normalised VC.
        VN(1)=VN(1)+VC(1)
        VN(2)=VN(2)+VC(2)
        VN(3)=VN(3)+VC(3)
 30   continue

C Make the surface normal of unit length and build EQN from VN.
      SNORM=0.
      SNORM=sqrt(VN(1)**2+VN(2)**2+VN(3)**2)
      close=.false.
      call eclose(SNORM,0.0,0.0001,close)
      if(close)then
        ierpln=-1
        return
      else
        do 40 I=1,3
          EQN(I)=VN(I)/SNORM
 40     continue
      endif

C Calculate constant term. 
      EQN(4)=(EQN(1)*CG(1)) + (EQN(2)*CG(2)) + (EQN(3)*CG(3))

      RETURN
      END

