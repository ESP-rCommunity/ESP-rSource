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

C PLELEV: Scales and draws a site plan/elev showing zone & obstructions.

C ************* PLELEV
C PLELEV Scales and draws a plan or elevation of a model
C and the surrounding obstruction blocks. direc is
C a character S, E, P,  ie. S gives a south elevation and
C P gives a plan.  Inpic is the number of zones, ivals is a
C array of selected zones to include.

C This version does not scan geometry or obstruction files and
C instead uses the VCOORD coordinates.

      SUBROUTINE PLELEV(direc)
#include "building.h"
#include "model.h"

C geometry.h provides commons G0/G2/precz/GS5/GS6.
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)
      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)

      integer izstocn
      common/c24/izstocn(mcom,ms)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS

      CHARACTER sstr*90,blabel*4,direc*1
      character tz*13
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iupdown,isym    ! passed to etplot
      integer iid1,iid2,iix,iiy
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iupdown,isym    ! passed to etplot
      integer*8 iid1,iid2,iix,iiy
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

C The right offset for the graphic box is the character width
C of the menu. Also allow 2 text lines at the bottom for future
C dialogue.
      if(mmod.lt.8)then
        call usrmsg('Not in graphic mode!',' ','W')
        return
      endif

C Define a viewing box, draw its border after clearing it and
C return the offsets which define it. Recalculate igw and igwh
C on fortran to address fault in 64 bit win3d code.
      call startbuffer()
      if(menuchw.ne.0)then

C Setup and pass in parameters to win3d.
        iiw1=7; iiw2=4; iiw3=3; iiw4=3; iimenu=menuchw
        iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
        CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &    iigl,iigr,iigt,iigb,iigw,iigwh)
      else
        iiw1=7; iiw2=4; iiw3=3; iiw4=3; iimenu=32
        iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
        CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &    iigl,iigr,iigt,iigb,iigw,iigwh)
      endif
      igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
      igw=int(iigw); igwh=int(iigwh)
      igwid=igw
      igheight=igwh
      
C Reopen the dialogue box.
      call usrmsg(' ',' ','-')

C Establish the maximum and minimum vertix values for the model.
      xmx=-1.E+8
      ymx=-1.E+8
      zmx=-1.E+8
      xmn=1.E+8
      ymn=1.E+8
      zmn=1.E+8
      newpic=nzg
      do 11 mz=1,newpic
        newfoc=nznog(mz)
        do 10 i=1,nztv(newfoc)
          xmn=amin1(xmn,VCOORD(newfoc,i,1))
          ymn=amin1(ymn,VCOORD(newfoc,i,2))
          zmn=amin1(zmn,VCOORD(newfoc,i,3))
          xmx=amax1(xmx,VCOORD(newfoc,i,1))
          ymx=amax1(ymx,VCOORD(newfoc,i,2))
          zmx=amax1(zmx,VCOORD(newfoc,i,3))
   10   continue

C Convert obstruction block geometry to (/gb1/) and
C establish maximum and minimum site coordinates.
        if(nbobs(newfoc).gt.0)then
          do 20 i=1,nbobs(newfoc)
            if(BLOCKTYP(newfoc,i)(1:4).eq.'obs ')then
              call cnvblk(xob(newfoc,i),yob(newfoc,i),zob(newfoc,i),
     &          xob(newfoc,i),dyob(newfoc,i),dzob(newfoc,i),
     &          bangob(newfoc,i,1))
            elseif(BLOCKTYP(newfoc,i)(1:4).eq.'obs3')then
              call cnvblk3a(xob(newfoc,i),yob(newfoc,i),zob(newfoc,i),
     &          xob(newfoc,i),dyob(newfoc,i),dzob(newfoc,i),
     &          bangob(newfoc,i,1),bangob(newfoc,i,2),
     &          bangob(newfoc,i,3))
            elseif(BLOCKTYP(newfoc,i)(1:4).eq.'obsp')then
              call cnvblkp(newfoc,i) ! convert obsp type.
            endif

            do 30 j=1,8
              xmn=amin1(xmn,xb(j))
              ymn=amin1(ymn,yb(j))
              zmn=amin1(zmn,zb(j))
              xmx=amax1(xmx,xb(j))
              ymx=amax1(ymx,yb(j))
              zmx=amax1(zmx,zb(j))
   30       continue
   20     continue
        endif
   11 continue

C Depending on plan or elevation views:
C Determine scaling ratios for the drawing and axis.
C Place scaling in static C variables for use by line drawing routines.
C Draw a scale horizontal axis, move to beginning point.
C Draw a scale vertical axis.
      if(direc.eq.'P')then
        CALL axiscale(igwid,igheight,XMN,XMX,YMN,YMX,xsc,ysc,sca,
     &    Xadd,Yadd)
        call linescale(iigl,Xadd,sca,iigb,Yadd,sca)
        CALL horaxis(XMN,XMX,iigl,iigr,iigb,Xadd,sca,1,'East (m) ->')
        CALL vrtaxis(YMN,YMX,iigl,iigb,iigt,Yadd,sca,1,0,'^ North')
      elseif(direc.eq.'S')then
        CALL axiscale(igwid,igheight,XMN,XMX,ZMN,ZMX,xsc,ysc,sca,
     &    Xadd,Yadd)
        call linescale(iigl,Xadd,sca,iigb,Yadd,sca)
        call horaxis(XMN,XMX,iigl,iigr,iigb,Xadd,sca,1,'East (m) ->')
        call vrtaxis(ZMN,ZMX,iigl,iigb,iigt,Yadd,sca,1,0,'^ Up')
      elseif(direc.eq.'E')then
        CALL axiscale(igwid,igheight,YMN,YMX,ZMN,ZMX,xsc,ysc,sca,
     &    Xadd,Yadd)
        call linescale(iigl,Xadd,sca,iigb,Yadd,sca)
        call horaxis(YMN,YMX,iigl,iigr,iigb,Xadd,sca,1,'North (m) ->')
        call vrtaxis(ZMN,ZMX,iigl,iigb,iigt,Yadd,sca,1,0,'^ Up')
      endif

C Loop through each of the selected zones and plot. Move to first 
C vertex and give the zone name.
      newpic=nzg
      do 54 mz=1,newpic
        newfoc=nznog(mz)

        if(direc.eq.'P')then
          call u2pixel(ZCOG(newfoc,1),ZCOG(newfoc,2),iix,iiy)
          iupdown=0
          isym=0
          call etplot(ZCOG(newfoc,1),ZCOG(newfoc,2),iupdown,isym)
        elseif(direc.eq.'S')then
          call u2pixel(ZCOG(newfoc,1),ZCOG(newfoc,3),iix,iiy)
          iupdown=0
          isym=0
          call etplot(ZCOG(newfoc,1),ZCOG(newfoc,3),iupdown,isym)
        elseif(direc.eq.'E')then
          call u2pixel(ZCOG(newfoc,2),ZCOG(newfoc,3),iix,iiy)
          iupdown=0
          isym=0
          call etplot(ZCOG(newfoc,2),ZCOG(newfoc,3),iupdown,isym)
        endif
        IF(lnzname(newfoc).GT.1)THEN
          write(tz,'(A)')zname(newfoc)
          call winfnt(ITFS)
          iid1=iix+2; iid2=iiy-2;
          call wstxpt(iid1,iid2,tz)
        ELSE
          call winfnt(ITFS)
          iid1=iix+2; iid2=iiy-2;
          call wstxpt(iid1,iid2,'Target')
        endif
        call forceflush()

C Move to first vertex of zone.
        iupdown=0
        isym=0
        if(direc.eq.'P')then
          call etplot(VCOORD(newfoc,1,1),VCOORD(newfoc,1,2),
     &      iupdown,isym)
        elseif(direc.eq.'S')then
          call etplot(VCOORD(newfoc,1,1),VCOORD(newfoc,1,3),
     &      iupdown,isym)
        elseif(direc.eq.'E')then
          call etplot(VCOORD(newfoc,1,2),VCOORD(newfoc,1,3),
     &      iupdown,isym)
        endif
        call forceflush()

C For each surface in the zone plot its initial vertex followed
C by each of the edge points.
        do 40 i=1,nzsur(newfoc)
          iupdown=0
          isym=0
          icc=IZSTOCN(newfoc,i)
          if(icc.gt.0)then
            j=NZJVN(icc,1)
            if(j.gt.0)then
              if(direc.eq.'P')then
                call etplot(VCOORD(newfoc,j,1),VCOORD(newfoc,j,2),
     &            iupdown,isym)
              elseif(direc.eq.'S')then
                call etplot(VCOORD(newfoc,j,1),VCOORD(newfoc,j,3),
     &            iupdown,isym)
              elseif(direc.eq.'E')then
                call etplot(VCOORD(newfoc,j,2),VCOORD(newfoc,j,3),
     &            iupdown,isym)
              endif
              call forceflush()
            else
              call usrmsg('Model may be unstable. Tried to draw an',
     &          'unknown surface edge.','P')
            endif
          else
            call usrmsg('Model may be unstable. Tried to draw an',
     &        'unknown surface.','P')
          endif

          k=NZNVER(icc)
          do 50 l=2,k
            iupdown=-302
            isym=0
            m=NZJVN(icc,l)
            if(m.gt.0)then
              if(direc.eq.'P')then
                call etplot(VCOORD(newfoc,m,1),VCOORD(newfoc,m,2),
     &            iupdown,isym)
              elseif(direc.eq.'S')then
                call etplot(VCOORD(newfoc,m,1),VCOORD(newfoc,m,3),
     &            iupdown,isym)
              elseif(direc.eq.'E')then
                call etplot(VCOORD(newfoc,m,2),VCOORD(newfoc,m,3),
     &            iupdown,isym)
              endif
              call forceflush()
            endif
   50     continue

C Back to the initial point of the surface.
          iupdown=-302
          isym=0
          if(j.gt.0)then
            if(direc.eq.'P')then
              call etplot(VCOORD(newfoc,j,1),VCOORD(newfoc,j,2),
     &          iupdown,isym)
            elseif(direc.eq.'S')then
              call etplot(VCOORD(newfoc,j,1),VCOORD(newfoc,j,3),
     &          iupdown,isym)
            elseif(direc.eq.'E')then
              call etplot(VCOORD(newfoc,j,2),VCOORD(newfoc,j,3),
     &          iupdown,isym)
            endif
            call forceflush()
          endif
   40   continue
        call forceflush()

C Now draw each obstruction block by first converting to GB1
C common block via CNVBLK.
        if(nbobs(newfoc).eq.0)goto 54
        DO 420 IB= 1,nbobs(newfoc)
          if(BLOCKTYP(newfoc,IB)(1:4).eq.'obs ')then
            CALL CNVBLK(XOB(newfoc,IB),YOB(newfoc,IB),ZOB(newfoc,IB),
     &        DXOB(newfoc,IB),DYOB(newfoc,IB),DZOB(newfoc,IB),
     &        BANGOB(newfoc,IB,1))
          elseif(BLOCKTYP(newfoc,IB)(1:4).eq.'obs3')then
            CALL CNVBLK3A(XOB(newfoc,IB),YOB(newfoc,IB),ZOB(newfoc,IB),
     &        DXOB(newfoc,IB),DYOB(newfoc,IB),DZOB(newfoc,IB),
     &        BANGOB(newfoc,IB,1),BANGOB(newfoc,IB,2),
     &        BANGOB(newfoc,IB,3))
          elseif(BLOCKTYP(newfoc,IB)(1:4).eq.'obsp')then
            CALL CNVBLKP(newfoc,IB) ! convert obsp type.
          endif

          DO 422 I=1,6   ! for each surface in the block
            J=JVNB(I,1)
            iupdown=0
            isym=0
            if(direc.eq.'P')call etplot(XB(J),YB(J),iupdown,isym)
            if(direc.eq.'S')call etplot(XB(J),ZB(J),iupdown,isym)
            if(direc.eq.'E')call etplot(YB(J),ZB(J),iupdown,isym)
            K=4          ! for each edge of block surface
            DO 424 L=2,K
              M=JVNB(I,L)
              iupdown=1
              isym=0
              if(direc.eq.'P')call etplot(XB(M),YB(M),iupdown,isym)
              if(direc.eq.'S')call etplot(XB(M),ZB(M),iupdown,isym)
              if(direc.eq.'E')call etplot(YB(M),ZB(M),iupdown,isym)
  424       CONTINUE
            iupdown=1
            isym=0
            if(direc.eq.'P')call etplot(XB(J),YB(J),iupdown,isym)
            if(direc.eq.'S')call etplot(XB(J),ZB(J),iupdown,isym)
            if(direc.eq.'E')call etplot(YB(J),ZB(J),iupdown,isym)
  422     CONTINUE
          if(direc.eq.'P')call u2pixel(XB(J),YB(J),iix,iiy)
          if(direc.eq.'S')call u2pixel(XB(J),ZB(J),iix,iiy)
          if(direc.eq.'E')call u2pixel(YB(J),ZB(J),iix,iiy)
          WRITE(blabel,423)IB
  423     FORMAT('B',I2)
          iid1=iix+4; iid2=iiy-4;
          call wstxpt(iid1,iid2,blabel)
          call forceflush()
  420   CONTINUE
  54  continue
      call forceflush()

C If there is a ground, ITDSP is flagged and plan view draw it.
      IF(ITDSP.EQ.5.and.NGT.gt.0)then
        if(direc.eq.'P')then
          DO 401 I=1,NGT
            J=JGVN(I,1)
            iupdown=0
            isym=0
            call etplot(XGT(J),YGT(J),iupdown,isym)
            K=NGVER(I)
            iupdown=-203
            isym=0
            DO 501 L=2,K
              M=JGVN(I,L)
              call etplot(XGT(M),YGT(M),iupdown,isym)
  501       CONTINUE
            call etplot(XGT(J),YGT(J),iupdown,isym)
  401     CONTINUE
        endif
      endif

C Print labels and return to default font.
      call winfnt(IFS)
      if(direc.eq.'P')then
        WRITE(sstr,'(a,a)')'Site Plan of: ',
     &    modeltitle(1:lnblnk(modeltitle))
      elseif(direc.eq.'S')then
        WRITE(sstr,'(a,a)')'South elev of: ',
     &    modeltitle(1:lnblnk(modeltitle))
      elseif(direc.eq.'E')then
        WRITE(sstr,'(a,a)')'East elev of: ',
     &    modeltitle(1:lnblnk(modeltitle))
      endif
      iiy= igt-10
      iix = igl
      call wstxpt(iix,iiy,sstr)
      call forceflush()

      RETURN
      END
