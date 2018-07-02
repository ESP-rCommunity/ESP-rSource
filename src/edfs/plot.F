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

C Contains:
C  PLOT - monitors itteration process.

C ********************* PLOT *********************
C PLOT - This routine traces the iteration processes by
C  ploting residuals of the equation and monitoring values.
C Up to 4 variables can be displayed MFRP iteration values are plotted.
C IPLT   - Index for the current value to be plotted.
C NPLT   - Number of iterations between plotting.
C ITM    - Time-step index.
C NITER  - Iteration index.
C RMX    - Max residual for convergence.
C NCRV   - number of curves to draw.
C CRES   - current residuals.
C CMON   - current monitoring values.
C LRES   - labels for residuals.
C LMON   - labels for monitoring values.
      SUBROUTINE PLOT(iplt,NPLT,ITM,NITER,RMX,NCRV,CRES,CMON,LRES,LMON)
#include "cfd.h"

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

C Stores all MFRP residuals, maximum and minimum residuals and max X value.
      COMMON/RES/RESID(4,MFRP),RESMX,RESMN,XMAX

C Stores all MFRP monitoring values, maximum and minimum values.
      COMMON/MON/XMONIT(4,MFRP),XMONMX(4),XMONMN(4)

      COMMON/CONST/GREAT,small,GRAV

      CHARACTER title*124,LRES(4)*24,LMON(4)*24,LINE(4)*5
      DIMENSION ILINE(4),CRES(4),CMON(4)
#ifdef OSI
      integer ipltwd,igheight  ! for use with axiscale
      integer iside,isize,ifont     ! passed to viewtext
      integer iupdown,isym    ! passed to etplot
      integer iigl,iigr,iigt,iigb,iigw,iigwh,iglmon
#else
      integer*8 ipltwd,igheight  ! for use with axiscale
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iupdown,isym    ! passed to etplot
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh,iglmon
#endif

      LOGICAL RDR

      DATA LINE/'-----','- - -','.....','-.-.-'/
      DATA ILINE/100,-3,-204,-105/

      RDR=.FALSE.
      RDR=.TRUE.

C Cast values for linescale.
      iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh

C Increment plot counter.  Check if negative (indicates max iterations
C have been increased, therefore force redraw).
      if (iplt.lt.0) then
        iplt=iplt*(-1)
        RDR=.TRUE.
      endif
      iplt=iplt+1

C Create local max residual
      XRMX=LOG(RMX)

C Initialized values when subroutine is first called.
      IF(IPLT.EQ.1)THEN
        RDR=.TRUE.
        XMAX=10.0
        XMIN=0.0
        YMAX=1.0
        YMIN=0.0
        RESMX = -GREAT
        RESMN = XRMX
        do 5 I=1,4
          RESMX = AMAX1(LOG(CRES(I)+0.001),RESMX)
          if (CMON(I).gt.0.0) then
            XMONMX(I)= CMON(I)*1.2
            XMONMN(I)= CMON(I)*0.8
          else
            XMONMX(I)= CMON(I)*0.8
            XMONMN(I)= CMON(I)*1.2
          endif
 5      continue
      ENDIF

C Copy current values into common.
      do 10 I=1,NCRV
        CRES(I)=AMAX1(CRES(I),small)
        RESID(I,IPLT)=LOG(CRES(I))
        XMONIT(I,IPLT)=CMON(I)
 10   continue

C Re-set maximum number of iteration.
 11   IF(IPLT.GT.INT(XMAX))THEN
        XMAX=XMAX+10.0
        RDR=.TRUE.
        GOTO 11
      ENDIF

C Re-set Maximum and Minimum residuals.
      do 20 I=1,NCRV
        if (RESID(I,IPLT).gt.RESMX) then
          RESMX=RESID(I,IPLT)
          RDR=.TRUE.
        elseif (RESID(I,IPLT).lt.RESMN) then
          RESMN=RESID(I,IPLT)
          RDR=.TRUE.
        endif
        if (XMONIT(I,IPLT).gt.XMONMX(I)) then
          if (CMON(I).gt.0.0) then
            XMONMX(I)=XMONIT(I,IPLT)*1.2
          else
            XMONMX(I)=XMONIT(I,IPLT)*0.8
          endif
          RDR=.TRUE.
        elseif (XMONIT(I,IPLT).lt.XMONMN(I)) then
          if (CMON(I).gt.0.0) then
            XMONMN(I)=XMONIT(I,IPLT)*0.8
          else
            XMONMN(I)=XMONIT(I,IPLT)*1.2
          endif
          RDR=.TRUE.
        endif
 20   continue

C Re-scale the screen and re-draw the coordinate axes.
      ipltwd=int(igw/2)-15
      iglmon=igl + int(ipltwd) + 30
      IF(RDR)THEN

C Calculate plotting box locations.
        plxmin=Xmin*real(NPLT)
        plxmax=Xmax*real(NPLT)
        YMAX=1.
        YMIN=0.
        CALL startbuffer()
        igheight=igwh
        CALL axiscale(ipltwd,igheight,plxmin,plxmax,Ymin,Ymax,
     &                xsc,ysc,sca,xadd,yadd)

C Scale and draw residuals plot.
        call dintervalf(plxmin,plxmax,DY,NDEC,0)
        CALL horaxisdd(plxmin,plxmax,iigl,iigr,iigb,xadd,XSC,0,
     &    DY,NDEC,'Number of Iterations')
        CALL linescale(iigl,xadd,xsc,iigb,yadd,ysc)
        iupdown=0
        isym=0
        CALL etplot(plxmin,YMIN,iupdown,isym)
        iupdown=1
        CALL etplot(plxmin,YMAX,iupdown,isym)
        CALL etplot(plxmax,YMAX,iupdown,isym)
        CALL etplot(plxmax,YMIN,iupdown,isym)

C Scale and draw monitoring plot.
        call dintervalf(plxmin,plxmax,DY,NDEC,0)
        CALL horaxisdd(plxmin,plxmax,iglmon,iigr,iigb,xadd,XSC,0,
     &    DY,NDEC,' ')
        CALL linescale(iglmon,xadd,xsc,iigb,yadd,ysc)
        iupdown=0
        isym=0
        CALL etplot(plxmin,YMIN,iupdown,isym)
        iupdown=1
        CALL etplot(plxmin,YMAX,iupdown,isym)
        CALL etplot(plxmax,YMAX,iupdown,isym)
        CALL etplot(plxmax,YMIN,iupdown,isym)
        call forceflush()
      ENDIF

C Print title. IPS is used to keep track of number of title lines.
      IPS=1
      write(title,'(12x,a,36x,a)') 'RESIDUALS','MONITORING VALUES'
      iside=IPS
      isize=1
      ifont=0
      call viewtext(title,iside,isize,ifont)

C Plot residuals curves.
      do 30 I=1,NCRV

C Rescale axis for first curve.
        if (I.eq.1) then
          YMAX=RESMX
          YMIN=RESMN
          igheight=igwh
          CALL axiscale(ipltwd,igheight,Xmin,Xmax,Ymin,Ymax,
     &                  xsc,ysc,sca,xadd,yadd)
          CALL linescale(iigl,xadd,xsc,iigb,yadd,ysc)
        endif

C If graph was re-scaled re-draw all residuals.
        IF(RDR)THEN
          iupdown=0
          isym=0
          CALL etplot(1.0,RESID(I,1),iupdown,isym)
          DO 100 J=2,IPLT
            iupdown=ILINE(I)
            XPLT=real(J)
            CALL etplot(XPLT,RESID(I,J),iupdown,isym)
  100     CONTINUE
        elseif(IPLT.gt.1) then

C Else just plot current iteration value.
          XPLT=real(IPLT-1)
          iupdown=0
          isym=0
          CALL etplot(XPLT,RESID(I,IPLT-1),iupdown,isym)
          iupdown=ILINE(I)
          XPLT=real(IPLT)
          CALL etplot(XPLT,RESID(I,IPLT),iupdown,isym)
        ENDIF
 30   continue

C Plot a star where the max residual value is on the curve.
      XMARK=XMAX*1.01
      iupdown=0
      isym=13
      CALL etplot(XMARK,XRMX,iupdown,isym)

C Plot monitoring curves.
      do 40 I=1,NCRV

C Rescale axis for each curve.
        YMAX=XMONMX(I)
        YMIN=XMONMN(I)
        igheight=igwh
        CALL axiscale(ipltwd,igheight,Xmin,Xmax,Ymin,Ymax,
     &                  xsc,ysc,sca,xadd,yadd)
        CALL linescale(iglmon,xadd,xsc,iigb,yadd,ysc)

C If graph was re-scaled re-draw all residuals.
        IF(RDR)THEN
          iupdown=0
          isym=0
          CALL etplot(1.0,XMONIT(I,1),iupdown,isym)
          DO 110 J=2,IPLT
            XPLT=real(J)
            iupdown=ILINE(I)
            CALL etplot(XPLT,XMONIT(I,J),iupdown,isym)
  110     CONTINUE
        elseif(IPLT.gt.1) then

C Else just plot current iteration value.
          XPLT=real(IPLT-1)
          iupdown=0
          isym=0
          CALL etplot(XPLT,XMONIT(I,IPLT-1),iupdown,isym)
          XPLT=real(IPLT)
          iupdown=ILINE(I)
          CALL etplot(XPLT,XMONIT(I,IPLT),iupdown,isym)
        ENDIF
 40   continue
      call forceflush()

C Label curves.
      do 50 I=1,NCRV
        IPS=IPS+1
        write(title,'(2(2(a,1x),a,1PE11.3,1x))')
     &          LINE(I),LRES(I),'=',CRES(I),LINE(I),LMON(I),'=',CMON(I)
        iside=IPS
        isize=1
        ifont=0
        call viewtext(title,iside,isize,ifont)
 50   continue

C Display time-step index and iteration index.
      IPS=IPS+2
      write(title,'(a,i5)')'ITST=',ITM
      iside=IPS
      isize=0
      ifont=0
      call viewtext(title,iside,isize,ifont)
      IPS=IPS+1
      write(title,'(a,i5)')'ITER=',NITER
      iside=IPS
      isize=0
      ifont=0
      call viewtext(title,iside,isize,ifont)
      call forceflush()

      RETURN
      END

C ********************* REPLOT *********************
C REPLOT - scale plots for new number of maximum iterations.
      SUBROUTINE REPLOT(MOLD,MNEW,IPLOT)
#include "cfd.h"

C Stores all MFRP residuals, maximum and minimum residuals and max X value.
      COMMON/RES/RESID(4,MFRP),RESMX,RESMN,XMAX

C Stores all MFRP monitoring values, maximum and minimum values.
      COMMON/MON/XMONIT(4,MFRP),XMONMX(4),XMONMN(4)

C Calculate index of old data and copy to new data slot.
      STEP=float(MNEW)/float(MOLD)
C      JEND=int(50.0/STEP)
      JEND=int(MFRP/STEP)
      IPLOT=0
      do 10 J=1,JEND
        JOLD=int(real(J)*STEP)
        IPLOT=IPLOT+1
        do 20 I=1,4
          RESID(I,IPLOT)=RESID(I,JOLD)
          XMONIT(I,IPLOT)=XMONIT(I,JOLD)
 20     continue
 10   continue

C Reduce IPLOT and negate to force redraw of graphs.
      iplot=(iplot-1)*(-1)

      return
      end
