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

C Patched version of subroutines in read3dv.f (selective display of
C surface names).

C PADJVIEW: Adjusts the viewing transforms & boundaries if necessary
C           for a predefined object preview.
C CDRAWESP: Loads the geometric description(s) from an ESP file,
C          converts into viewing coords, clips the image and draws it.

C There is a lot of commonality here when drawing graphic entities
C once the IANXT IBNXT AX AY AZ data structures have been filled.
C Variants - name @ 1st vertex, name @ surf COG, name @ COG of body
C Note check array sizes if different from MV.

C ************* PADJVIEW 
C PADJVIEW takes the current status of view and boundary mods
C and adjusts the viewing transforms & object boundaries if 
C necessary before displaying a predefined object. It is assumed
C that predefined commons have been filled prior to calling this.
C IER=0 OK, IER=1 problem. 
C izgfoc greater than zero represents the zone which is currently being edited.

C << TODO call padjview from the wire-frame control menu.
      SUBROUTINE PADJVIEW(IER)
#include "building.h"
#include "predefined.h"
#include "prj3dv.h"

C Passed parameter.
      integer ier

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/HOLDVW/iglhld,igbhld,Xaddhld,Yaddhld,scahld

#ifdef OSI
      integer igwid,igheight          ! for use with axiscale
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 igwid,igheight          ! for use with axiscale
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

C      real CG(3)
      logical CLOSE

      SMALL=0.01

C If not in graphic mode return.
      if(MMOD.lt.8)return

C Do a bound check on the current object and update display.
C      write(6,'(a)') 'BNDOBJ XMN YMN ZMN XMX YMX ZMX'

C Find the centroid of object and bounding box of the object. objbnds(3)
      XMX=objbnds(1)+0.1; YMX=objbnds(2)+0.1; ZMX=objbnds(3)+0.1

C Set a bit of buffer on the left and bottom. If small object set at zero.
      XMN=(-0.1); YMN=(-0.1); ZMN=(-0.1)
      if(objbnds(1).lt.0.4) XMN=0.0
      if(objbnds(2).lt.0.4) YMN=0.0
      if(objbnds(3).lt.0.4) ZMN=0.0

      if(objbnds(1).gt.0.4) XMX=objbnds(1)+0.2  ! and some buffer to right and top
      if(objbnds(2).gt.0.4) YMX=objbnds(2)+0.2
      if(objbnds(3).gt.0.4) ZMX=objbnds(3)+0.2
      if(objbnds(1).gt.1.) XMX=objbnds(1)+0.3  ! and some buffer to right and top
      if(objbnds(2).gt.1.) YMX=objbnds(2)+0.3
      if(objbnds(3).gt.1.) ZMX=objbnds(3)+0.3

C Calculate object centre within bounding box.
      call eclose(XMN,0.0,0.001,CLOSE)
      if(XMN.LT.0..AND.XMX.GE.0.0)then
        VIEWM(1)=XMN+((XMX+ABS(XMN))/2.0)
      elseif(XMN.LT.0.0.AND.XMX.LE.0.)then
        VIEWM(1)=XMN+((ABS(XMN)-ABS(XMX))/2.0)
      elseif(XMN.GT.0.0.AND.XMX.GT.0.0)then
        VIEWM(1)=XMX-((XMX-XMN)/2.0)
      elseif(CLOSE.AND.XMX.GT.0.0)then
        VIEWM(1)=XMX/2.0
      endif
      call eclose(YMN,0.0,0.001,CLOSE)
      if(YMN.LT.0..AND.YMX.GE.0.0)then
        VIEWM(2)=YMN+((YMX+ABS(YMN))/2.0)
      elseif(YMN.LT.0.0.AND.YMX.LE.0.)then
        VIEWM(2)=YMN+((ABS(YMN)-ABS(YMX))/2.0)
      elseif(YMN.GT.0.0.AND.YMX.GT.0.0)then
        VIEWM(2)=YMX-((YMX-YMN)/2.0)
      elseif(CLOSE.AND.YMX.GT.0.0)then
        VIEWM(2)=YMX/2.0
      endif
      call eclose(ZMN,0.0,0.001,CLOSE)
      if(ZMN.LT.0.0.AND.ZMX.GE.0.0)then
        VIEWM(3)=ZMN+((ZMX+ABS(ZMN))/2.0)
      elseif(ZMN.LT.0.0.AND.ZMX.LE.0.0)then
        VIEWM(3)=ZMN+((ABS(ZMN)-ABS(ZMX))/2.0)
      elseif(ZMN.GT.0.0.AND.ZMX.GT.0.0)then
        VIEWM(3)=ZMX-((ZMX-ZMN)/2.0)
      elseif(CLOSE.AND.ZMX.GT.0.0)then
        VIEWM(3)=ZMX/2.0
      endif

C Initialise viewing parameters.
      ANGobj=40.0
      HANG=ANGobj/2.0
      DIS = (VIEWM(1)-EYEM(1))**2 + (VIEWM(2)-EYEM(2))**2 +
     &      (VIEWM(3)-EYEM(3))**2
      IF(DIS.GE.SMALL)THEN
        HITH=1.0
        YON=1300.0
      ELSE
        CALL USRMSG(' ',
     &    ' The eye position and viewed position are too close!','W')
        RETURN
      ENDIF
      CALL LENS(IER)

C Clear current viewing box.
      CALL startbuffer()

C Setup and pass in parameters to win3d. Leave space for title lines.
      iiw1=4; iiw2=4; iiw3=4; iiw4=1; iimenu=menuchw
      if(nbobjnotes.gt.1) iiw3=5  ! if a 2nd text line
      iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
      CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &  iigl,iigr,iigt,iigb,iigw,iigwh)
      igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
      igw=int(iigw); igwh=int(iigwh)
      igwid=igw
      igheight=igwh

C If optimal view bounds, reset scaling ratios.
      IF(ITBND.EQ.1)THEN
        CALL SITE2D(SXMX,SXMN,SYMX,SYMN,ier)
        CALL axiscale(igwid,igheight,SXMN,SXMX,SYMN,SYMX,xsc,ysc,sca,
     &    Xadd,Yadd)
        call linescale(iigl,Xadd,sca,iigb,Yadd,sca)
        iglhld=igl; Xaddhld=Xadd; igbhld=igb; Yaddhld=Yadd; scahld=sca ! remember values

C If static bounds, reset linescale in case we have returned from
C something else that changed it.
      ELSE
        iigl=igl; iigb=igb
        call linescale(iigl,Xaddhld,scahld,iigb,Yaddhld,scahld)
      ENDIF

C If view or scaling has changed, reset clipping planes.
      if (MODLEN .or. ITBND.EQ.1) call INICLP(ier)

C Draw the selected bodies.
      CALL CDRAWOBJ(IER)

      MODIFYVIEW=.FALSE.
      MODLEN=.FALSE.
      MODBND=.FALSE.

      RETURN
      END

C ******************** CDRAWOBJ 
C CDRAWOBJ loads the object description(s) from common blocks,
C converts into viewing coords, clips the image and draws it.
C ier is returned as non-zero if there has been a problem.
C This code is similar to that used in cread3dv.F subroutine drawesp.
      SUBROUTINE CDRAWOBJ(ier)
#include "building.h"
#include "geometry.h"
#include "predefined.h"
#include "prj3dv.h"
#include "esprdbfile.h"
#include "material.h"

C Passed parameter.
      integer ier

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS

      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec

C For the pre-defined solar obstructions that were converted
C into GB1 common block.
      real XB,YB,ZB
      integer JVNB
      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)

C szcoords : X,Y & Z coordinates in model (via geometry.h).
C iszjvn : topology of vertices (via geometry.h).

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL  ISTOK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical greyok

C Local arrays.
      real AX,AY,AZ,BX,BY,BZ,SBBOX,COG1,COG2
      integer IANXT,IBNXT,edges
      DIMENSION  AX(MV),AY(MV),AZ(MV),IANXT(MV)
      DIMENSION  BX(MV),BY(MV),BZ(MV),IBNXT(MV)
      DIMENSION  SBBOX(3,2),COG1(3),COG2(3)
      DIMENSION  XX(MV),YY(MV),ZZ(MV),CG(3),EQN(4)

      CHARACTER temp*16,ETEXT*72
      integer loop,loop1,loop2,loop3

C Local variables to pass to edline and axiscale.
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iix,iiy,iixlast,iiylast,iid2
      integer iicol
      integer iside,isize,ifont     ! passed to viewtext
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iix,iiy,iixlast,iiylast,iid2
      integer*8 iicol
      integer*8 iside,isize,ifont     ! passed to viewtext
#endif

#ifdef OSI
      integer numberofzones ! to pass to updwire rather than ncomp
#else
      integer*8 numberofzones
#endif

C The following logic embeds the sequence of steps used when setting
C up and drawing standard zones but for pre-defined objects.

C If not in graphic mode return.
      if(mmod.lt.8)return
      SMALL=0.01

C Tell wireframe control the current number of selected zones.
      numberofzones=nzg
      call updwire(numberofzones)  ! pass local integer

C If libraries and monitor support greyscale remember this.
      greyok=.false.
      if(nifgrey.gt.4)then
        greyok=.true.
      endif

C The right offset for the graphic box is the character width of the
C menu. Also allow 2 text lines at the bottom for future dialogue.
      IER=0

C Refresh the dialogue box if terminal type 8.
      IF(MMOD.EQ.8)CALL USRMSG(' ',' ','-')

C Display object name category description the top of the display.
      WRITE(ETEXT,'(5A,3F7.3)')'Object: ',objname(1:lnblnk(objname)),
     &  ' Category: ',objectcat(1:lnblnk(objectcat)),' Size:',
     &  objbnds(1),objbnds(2),objbnds(3)
      iside=1; isize=1; ifont=1
      call viewtext(etext,iside,isize,ifont)
      WRITE(ETEXT,'(2A)') ' Description: ',objdesc(1:lnblnk(objdesc))
      iside=2; isize=1; ifont=1
      call viewtext(etext,iside,isize,ifont)
      WRITE(ETEXT,'(2A)') objnotes(1)(1:lnblnk(objnotes(1)))
      iside=3; isize=0; ifont=1
      call viewtext(etext,iside,isize,ifont)
      if(nbobjnotes.gt.1)then
        WRITE(ETEXT,'(2A)') objnotes(2)(1:lnblnk(objnotes(2)))
        iside=4; isize=0; ifont=1
        call viewtext(etext,iside,isize,ifont)
      endif
      if(nbobjnotes.gt.2)then
        WRITE(ETEXT,'(2A)') objnotes(3)(1:lnblnk(objnotes(3)))
        iside=5; isize=0; ifont=1
        call viewtext(etext,iside,isize,ifont)
      endif
      call forceflush()

C Also show documentation in text feedback.
      do loop=1,nbobjnotes
        call edisp(iuout,objnotes(loop))
      enddo

      call wiresymbol(0.0,0.0,0.0,2,33)  ! indicate origin

C Draw any mass surfaces as double width lines with dot at vertices.
C Setup ZX AY AZ arrays for transforms and XX YY ZZ for COG
C      write(6,*) 'nbmass is ',nbmass
      if(nbmass.gt.0)then
        do loop=1,nbmass

C Draw any mass surfaces as double width lines with dot at vertices.
C Setup ZX AY AZ arrays for transforms and XX YY ZZ for COG
          NAP = nbmassv(loop)
          do loop2=1,NAP
            k=masjvn(loop,loop2)
            XX(loop2) = vertmass(k,1)
            YY(loop2) = vertmass(k,2)
            ZZ(loop2) = vertmass(k,3)
            call wiresymbol(xx(loop2),yy(loop2),zz(loop2),1,32)

C If vertex is on the bounding edge draw it in color as well.
            if(nbbedge.gt.0)then
              do loop3=1,nbbedge
                if(edgejvn(loop3).eq.k)then
                  call wiresymbol(xx(loop2),yy(loop2),zz(loop2),2,24)
                endif
              enddo  ! of nbbedge
            endif
          enddo ! of loop2
        enddo
      endif

      if(nbmass.gt.0)then
        do 1000 loop=1,nbmass
          do J = 1,MV  ! clear arrays
            IANXT(J) = 0; IBNXT(J) = 0
          enddo
          IAPNT = 1
          NAP = nbmassv(loop)
          do loop2=1,NAP
            k=masjvn(loop,loop2)
            AX(loop2) = vertmass(k,1); AY(loop2) = vertmass(k,2)
            AZ(loop2) = vertmass(k,3)
            IANXT(loop2) = loop2 + 1
            XX(loop2) = vertmass(k,1)  ! for COG calc
            YY(loop2) = vertmass(k,2)
            ZZ(loop2) = vertmass(k,3)
            call wiresymbol(xx(loop2),yy(loop2),zz(loop2),1,32)
          enddo ! of loop2

          N = nbmassv(loop)  ! for placing name
          ierr=0
          call PLEQN(XX,YY,ZZ,N,CG,EQN,IERR)

          IANXT(NAP) = IAPNT
          CALL MATPOLS(NAP,AX,AY,AZ,IANXT,TSMAT,SBBOX,
     &               NBP,BX,BY,BZ,IBNXT,IERR)

C Generate clipping flags and clip geometry in eye coords.
          IBPNT=1
          call CLIPSUR(NBP,BX,BY,BZ,ISTAT)
          IF (ISTAT .EQ. 1) THEN
            goto 1000
          ELSEIF (ISTAT .EQ. -1) THEN
            CALL CUTSUR(NB1,NBP,IBPNT,BX,BY,BZ,IBNXT,ISTAT)
          else
            NB1=1
          ENDIF

C If there are no edges to draw then jump.
          if(NB1.eq.0)goto 1000

C Multiply each vertex by ETSMAT (Eye to Screen Matrix).
C First move to the beginning (as passed back from CUTSUR).
          IP = IABS(IBPNT)
          call u2pixel(BX(IP),BY(IP),iix,iiy)
          iixlast=iix   ! cast for edline call
          iiylast=iiy

C If IP1 is ever negative then jump to a later point.
          IP1 = IBNXT(IP)
          if(IP1.lt.1)IP1 = IBNXT(IP+1)
          if(IP1.lt.1)IP1 = IBNXT(IP+2)

 351      CONTINUE
          call u2pixel(BX(IP1),BY(IP1),iix,iiy)
          call edwline(iixlast,iiylast,iix,iiy)
          call pausems(20)
          call forceflush()
  
C Repeat until next point is the first, remember position and skip
C past any -1 values of IBNXT, save IP1 as IPT for testing.
          IPT = IP1
          IP1 = IBNXT(IP1)
          if(IP1.lt.1)IP1 = IBNXT(IPT+1)
          if(IP1.lt.1)IP1 = IBNXT(IPT+2)
          iixlast=iix   ! cast for edline call
          iiylast=iiy
          IF(IP1 .NE. IP) GOTO 351

C Complete the polygon.
          call u2pixel(BX(IP),BY(IP),iix,iiy)
          call edwline(iixlast,iiylast,iix,iiy)
          call pausems(20)
          call forceflush()

C Use COG of the mass surface in order to draw its name.
          CALL VECTRN(CG,TSMAT,COG2,IER)
          call u2pixel(COG2(1),COG2(2),iix,iiy)
          temp='        '
          write(temp,'(a)') msurname(loop)
          call CLIPST(temp,iix,iiy,COG2(3),iclp)
          if (iclp.eq.0) then
            iicol=0
            call textatxy(iix,iiy,temp,'-',iicol)
            call forceflush()
          endif
 1000   continue  ! of loop nbmass

      endif

C Now draw the bounding surfaces. Follow the usual pattern.
      if(nbbound.gt.0)then
        do 1001 loop=1,nbbound
          do J = 1,MV
            IANXT(J) = 0; IBNXT(J) = 0
          enddo
          IAPNT = 1
          NAP = nbboundv(loop)
          do loop2=1,NAP
            k=boundjvn(loop,loop2)
            AX(loop2) = vertmass(k,1); AY(loop2) = vertmass(k,2)
            AZ(loop2) = vertmass(k,3)
            XX(loop2) = vertmass(k,1); YY(loop2) = vertmass(k,2)
            ZZ(loop2) = vertmass(k,3)
            call wiresymbol(xx(loop2),yy(loop2),zz(loop2),2,32)  ! indicate vertex
            IANXT(loop2) = loop2 + 1
          enddo ! of loop2
          N = nbboundv(loop)
          ierr=0
          call PLEQN(XX,YY,ZZ,N,CG,EQN,IERR)

          IANXT(NAP) = IAPNT
          CALL MATPOLS(NAP,AX,AY,AZ,IANXT,TSMAT,SBBOX,
     &               NBP,BX,BY,BZ,IBNXT,IERR)

C Generate clipping flags and clip geometry in eye coords.
          call CLIPSUR(NBP,BX,BY,BZ,ISTAT)
          IBPNT=1
          IF (ISTAT .EQ. 1) THEN
            goto 1001
          ELSEIF (ISTAT .EQ. -1) THEN
            CALL CUTSUR(NB1,NBP,IBPNT,BX,BY,BZ,IBNXT,ISTAT)
          else
            NB1=1
          ENDIF

C If there are no edges to draw then jump.
          if(NB1.eq.0)goto 1001
          edges=0    ! keep track so complex surfaces are completed
          IP = IABS(IBPNT)
          call u2pixel(BX(IP),BY(IP),iix,iiy)
          iixlast=iix   ! cast for edline call
          iiylast=iiy

C If IP1 is ever negative then jump to a later point.
          IP1 = IBNXT(IP)
          if(IP1.lt.1)IP1 = IBNXT(IP+1)
          if(IP1.lt.1)IP1 = IBNXT(IP+2)

 352      CONTINUE
          call u2pixel(BX(IP1),BY(IP1),iix,iiy)
          call eswline(iixlast,iiylast,iix,iiy)
          edges=edges+1
          call forceflush()
          call pausems(20)
  
C Repeat until next point is the first, remember position and skip
C past any -1 values of IBNXT, save IP1 as IPT for testing.
C If surface has a hole (another polygon) in there will be
C a duplicate index so test that we have completed the full circuit.
          IPT = IP1
          IP1 = IBNXT(IP1)
          if(IP1.lt.1)IP1 = IBNXT(IPT+1)
          if(IP1.lt.1)IP1 = IBNXT(IPT+2)
          iixlast=iix   ! cast for edline call
          iiylast=iiy
          IF(IP1 .NE. IP)then
            if(edges.lt.nbp) GOTO 352
          endif

C Complete the polygon.
          call u2pixel(BX(IP),BY(IP),iix,iiy)
          call eswline(iixlast,iiylast,iix,iiy)
          call forceflush()
          call pausems(20)

C Use COG of the bounding surface in order to draw its name.
          CALL VECTRN(CG,TSMAT,COG2,IER)
          call u2pixel(COG2(1),COG2(2),iix,iiy)
          temp='        '
          write(temp,'(a)') bsurname(loop)
          call CLIPST(temp,iix,iiy,COG2(3),iclp)
          if (iclp.eq.0) then
            iicol=0
            call textatxy(iix,iiy,temp,'-',iicol)
            call forceflush()
          endif
 1001   continue  ! of loop nbmass

      endif

C Now draw the visual entities.  << As grey lines. >>
      if(nbobjvis.gt.0)then
        DO 301 IB=1,nbobjvis
          if(objvistyp(IB)(1:4).eq.'vis ')then
            CALL CNVBLK(OBJXOV(IB),OBJYOV(IB),OBJZOV(IB),
     &        OBJDXOV(IB),OBJDYOV(IB),OBJDZOV(IB),
     &        objbangov(IB,1))
          elseif(objvistyp(IB)(1:4).eq.'vis3')then
            CALL CNVBLK3A(OBJXOV(IB),OBJYOV(IB),OBJZOV(IB),
     &        OBJDXOV(IB),OBJDYOV(IB),OBJDZOV(IB),
     &        objbangov(IB,1),objbangov(IB,2),objbangov(IB,3))
          elseif(objvistyp(IB)(1:4).eq.'visp')then
            call CNVOBJVISP(IB) ! convert predefined visp type.
          endif

C Place its name near first vertex.
          COG1(1)=XB(1); COG1(2)=YB(1); COG1(3)=ZB(1)
          CALL VECTRN(COG1,TSMAT,COG2,IER)
          call u2pixel(COG2(1),COG2(2),iix,iiy)
          write(temp,'(A)')objvisname(IB)
          iid2=iiy+8
          call CLIPST(temp,iix,iid2,COG2(3),iclp)
          if (iclp.eq.0) then
            iicol=0
            call textatxy(iix,iid2,temp,'-',iicol)
          endif
          do J = 1,MV
            IANXT(J) = 0; IBNXT(J) = 0
          enddo

C There are 4 initial vertices per block side and 6 sides.
          NAP=4
          DO 1002 I=1,6
            DO J = 1,NAP
              K = JVNB(I,J)
              AX(J) = XB(K); AY(J) = YB(K); AZ(J) = ZB(K)
              IANXT(J) = J + 1
            ENDDO
            IANXT(4) = 1
            CALL MATPOLS(NAP,AX,AY,AZ,IANXT,TSMAT,SBBOX,
     &                 NBP,BX,BY,BZ,IBNXT,IERR)
            call CLIPSUR(NBP,BX,BY,BZ,ISTAT)
            IBPNT=1
            IF (ISTAT .EQ. 1) THEN
              GOTO  1002
            ELSEIF (ISTAT .EQ. -1) THEN
              CALL CUTSUR(NB1,NBP,IBPNT,BX,BY,BZ,IBNXT,ISTAT)
            else
              NB1=1
            ENDIF

C If there are no edges to draw then jump.
            if(NB1.eq.0)goto 1002

            IP = IABS(IBPNT)
            call u2pixel(BX(IP),BY(IP),iix,iiy)
            iixlast=iix   ! cast for edline call
            iiylast=iiy
            IP1 = IBNXT(IP)
            if(IP1.lt.1)IP1 = IBNXT(IP+1)
            if(IP1.lt.1)IP1 = IBNXT(IP+2)

 353        CONTINUE
            call u2pixel(BX(IP1),BY(IP1),iix,iiy)
            if(greyok)then
              iicol=nifgrey-3
              call winscl('i',iicol)
            endif
            call eswline(iixlast,iiylast,iix,iiy)
            if(greyok)then
              iicol=0
              call winscl('-',iicol)
            endif
            call forceflush()
            call pausems(20)

C Repeat until....
            IPT = IP1
            IP1 = IBNXT(IP1)
            if(IP1.lt.1)IP1 = IBNXT(IPT+1)
            if(IP1.lt.1)IP1 = IBNXT(IPT+2)
            iixlast=iix   ! cast for edline call
            iiylast=iiy
            IF(IP1 .NE. IP) GOTO 353

            call u2pixel(BX(IP),BY(IP),iix,iiy)
            if(greyok)then
              iicol=nifgrey-3
              call winscl('i',iicol)
            endif
            call eswline(iixlast,iiylast,iix,iiy)
            if(greyok)then
              iicol=0
              call winscl('-',iicol)
            endif
            call forceflush()
 1002     CONTINUE
  301   CONTINUE

      endif

C Now draw any predefined solar obstructions. << NOT YET TESTED >>
      if(objnbobs.gt.0)then
        DO 302 IB=1,objnbobs
          if(objblocktyp(IB)(1:4).eq.'vis ')then
            CALL CNVBLK(OBJXOB(IB),OBJYOB(IB),OBJZOB(IB),
     &        OBJDXOB(IB),OBJDYOB(IB),OBJDZOB(IB),
     &        objbangob(IB,1))
          elseif(objblocktyp(IB)(1:4).eq.'vis3')then
            CALL CNVBLK3A(OBJXOV(IB),OBJYOV(IB),OBJZOV(IB),
     &        OBJDXOV(IB),OBJDYOV(IB),OBJDZOV(IB),
     &        objbangob(IB,1),objbangob(IB,2),objbangob(IB,3))
          elseif(objblocktyp(IB)(1:4).eq.'visp')then
            call CNVPREOBS(IB) ! convert predef obstruction visp type.
          endif

C Place its name near first vertex.
          COG1(1)=XB(1); COG1(2)=YB(1); COG1(3)=ZB(1)
          CALL VECTRN(COG1,TSMAT,COG2,IER)
          call u2pixel(COG2(1),COG2(2),iix,iiy)
          write(temp,'(A)')objblockname(IB)
          iid2=iiy+8
          call CLIPST(temp,iix,iid2,COG2(3),iclp)
          if (iclp.eq.0) then
            iicol=0
            call textatxy(iix,iid2,temp,'-',iicol)
          endif

C Follow the usual pattern.
          do J = 1,MV
            IANXT(J) = 0; IBNXT(J) = 0
          enddo
          NAP=4
          DO 1003 I=1,6
            DO J = 1,NAP
              K = JVNB(I,J)
              AX(J) = XB(K); AY(J) = YB(K); AZ(J) = ZB(K)
              IANXT(J) = J + 1
            ENDDO
            IANXT(4) = 1
            CALL MATPOLS(NAP,AX,AY,AZ,IANXT,TSMAT,SBBOX,
     &                 NBP,BX,BY,BZ,IBNXT,IERR)
            call CLIPSUR(NBP,BX,BY,BZ,ISTAT)
            IBPNT=1
            IF (ISTAT .EQ. 1) THEN
              GOTO  1003
            ELSEIF (ISTAT .EQ. -1) THEN
              CALL CUTSUR(NB1,NBP,IBPNT,BX,BY,BZ,IBNXT,ISTAT)
            else
              NB1=1
            ENDIF

C If there are no edges to draw then jump.
            if(NB1.eq.0)goto 1003

            IP = IABS(IBPNT)
            call u2pixel(BX(IP),BY(IP),iix,iiy)
            iixlast=iix   ! cast for edline call
            iiylast=iiy

            IP1 = IBNXT(IP)
            if(IP1.lt.1)IP1 = IBNXT(IP+1)
            if(IP1.lt.1)IP1 = IBNXT(IP+2)

 354        CONTINUE
            call u2pixel(BX(IP1),BY(IP1),iix,iiy)
            call eswline(iixlast,iiylast,iix,iiy)
            call forceflush()

C Repeat until....
            IPT = IP1
            IP1 = IBNXT(IP1)
            if(IP1.lt.1)IP1 = IBNXT(IPT+1)
            if(IP1.lt.1)IP1 = IBNXT(IPT+2)
            iixlast=iix   ! cast for edline call
            iiylast=iiy
            IF(IP1 .NE. IP) GOTO 354

C Complete the polygon.
            call u2pixel(BX(IP),BY(IP),iix,iiy)
            call eswline(iixlast,iiylast,iix,iiy)
            call forceflush()
 1003     CONTINUE
  302   CONTINUE

      endif

      END
