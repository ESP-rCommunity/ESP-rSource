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

C MISCEL.F contains the following routines used by ish.

C AREA:    Returns area for any polygon.
C MESH:    Computes centre point X&Z coord of grid squares.
C PICKSSUR selects surfaces for shading & insolation analysis.
C POINT1   determine if a point is within the target surface
C POINT2   determine if a point is within a shadow polygon.

C ************* AREA 
C Calculates the area of a surface comprising 'n' vertices
C and coordinates 'x & y'; the calculated area is 'a'.

      subroutine area(n,x,y,a)
#include "building.h"

      dimension x(mv),y(mv)

      a=0
      da=(x(2)-x(1))**2+(y(2)-y(1))**2
      do 10 i=3,n
         db=(x(i)-x(1))**2+(y(i)-y(1))**2
         dc=(x(i)-x(i-1))**2+(y(i)-y(i-1))**2
         si=(y(1)-y(i-1))*(x(i)-x(i-1))-(y(i)-y(i-1))*(x(1)-x(i-1))
         if(si.gt.0.)si=1.
         if(si.lt.0.)si=-1.
         if(abs(si).lt.0.0001)goto 1

C Check square root is not negative.
         temp=2.*(da*db+db*dc+dc*da)-(da*da+db*db+dc*dc)
         if(temp.gt.0.00001)then
            arex=.25*sqrt(temp)
         else
            arex=0.0
         endif
         a=a+si*arex
    1    da=db
   10 continue
      a=abs(a)
      return
      end

C *************** MESH ***********************
C Computes the centre point 'x & y' coordinates for each rectangular
C grid cell as superimposed on the target surface. Note that some of
C these points will lie outside a non-rectangular target surface; such
C points are later excluded.

      subroutine mesh(izone,its)
#include "building.h"
#include "geometry.h"
      common/g1t/xft(mcom,mv),yft(mcom,mv),zft(mcom,mv)
      common/grid3/ogrida(ms)
      common/grid11/xop(mox,moz),zop(mox,moz)
      common/c24/izstocn(mcom,ms)

C Get the current connection and number of surfaces in zone.
      icc=izstocn(izone,its)

C Derive bounding box (in 2d) for surface 'its'. First find max and min
C x & y values for this surface by scanning each vertex.
      xmax=-1.E+10
      zmax=-1.E+10
      xmin=1.E+10
      zmin=1.E+10
      nv=nznver(icc)
      do 10 i=1,nv
         xmax=amax1(xmax,xft(icc,i))
         zmax=amax1(zmax,zft(icc,i))
         xmin=amin1(xmin,xft(icc,i))
         zmin=amin1(zmin,zft(icc,i))
   10 continue

C Mesh length and height.
      dxmesh=xmax-xmin
      dzmesh=zmax-zmin

C Establish grid interval for this surface. Ogrida is the area of
C one grid cell.
      divx=dxmesh/nox(izone)
      divz=dzmesh/noz(izone)
      ogrida(its)=divx*divz

C Compute x & y values for centre point of each grid cell.
      do 20 i=1,nox(izone)
         do 30 j=1,noz(izone)
            xop(i,j)=divx/2.+(i-1)*divx
            zop(i,j)=divz/2.+(j-1)*divz
   30    continue
   20 continue
      return
      end

C ******************** PICKSSUR 
C Selects surfaces for shading & insolation analysis.
C 'izone' is the target zone, 'np' the number of surfaces selected,
C 'act' = 's or S' for shading, 'act' = 'i or I' for insolation,
C 'act' = 'l or L' for view shading surfaces and 'act' = '-' for
C no selection.  'iva' is the array of selections.

      subroutine pickssur(izone,np,act,iva,ier)
#include "building.h"
#include "geometry.h"

      common/pophelp/h(60)
      
      integer ncomp,ncon
      common/c1/ncomp,ncon
      common/c24/izstocn(mcom,ms)
      common/g6/ssname(mcon),ssotf(mcon),ssmlcn(mcon),ssvfc(mcon),
     &          ssother(mcon,3),ssparent(mcon),ssuse(mcon,2)
      common/contr/mon,isc(ms),iyd

C iaplic(1) toggle for shading; iaplic(2) toggle for insolation;
C iaplic(3) toggle for assessment where:
C                      1 is 'all_applicable' and
C                      0 is manual selection of surfaces.
C nsurfcalc nb of shaded surfaces, lstsfcalc() list of applicable surfaces.
C nsurfinso nb of insolation sources, isurfinso() list of insolation sources.
      common/ishdirec/iaplic(mcom,2),nsurfcalc(mcom),lstsfcalc(mcom,ms),
     &       nsurfinso(mcom),isurfinso(mcom,ms)

      dimension stmp(ms),iva(ms),ivals(ms)
      character ssname*12,ssotf*32,ssmlcn*32,ssvfc*4,ssother*24,
     &          ssparent*12,ssuse*8
      character h*72,stmp*33,act*1

      ier=0
      if(izone.gt.ncomp.or.izone.eq.0)then
         call usrmsg(' ',' Zone number out of range! ','W')
         ier=1
         return
      endif

 43   h(1) ='A shading analysis computes the temporal patterns of'
      h(2) ='shading on exterior surfaces, while an insolation'
      h(3) ='analysis tracks the direct solar radiation entering'
      h(4) ='a zone to determine the distribution between internal'
      h(5) ='surfaces. When selecting the surfaces to include, note'
      h(6) ='that:'
      h(7) ='  + for the case of shading, the surface must be external'
      h(8) ='    and belong to a zone that has an associated Zone'
      h(9) ='    Obstructions file; and'
      h(10)='  + for the case of insolation, the surface must be'
      h(11)='    transparent. If it is an internal surface it will'
      h(12)='    be processed as if external.'
      if(act.eq.'s'.or.act.eq.'S')then
         call easkabc(' ','Shading options:',
     &          'all applicable surfaces','select surface(s)',
     &          'cancel',iic,12)
         if(iic.eq.1)iaplic(izone,1)=1
         if(iic.eq.2)iaplic(izone,1)=0
      elseif(act.eq.'i'.or.act.eq.'I')then
         call easkabc(' ','Insolation options:',
     &          'all applicable sources','select source(s)',
     &          'cancel',iic,12)
         if(iic.eq.1)iaplic(izone,2)=1
         if(iic.eq.2)iaplic(izone,2)=0
      elseif(act.eq.'l'.or.act.eq.'L')then
         iic=4
      else
         iic=2
      endif

C Clear return array.
      do 14 i=1,nzsur(izone)
         iva(i)=0
  14  continue

C Process initial selection.
      if(iic.eq.3)then
         return
      elseif(iic.eq.1)then
         if(act.eq.'s'.or.act.eq.'S')then
            np=0
            do 13 i=1,nzsur(izone)
               icn=izstocn(izone,i)
               if(ssother(icn,1)(1:5).eq.'EXTER')then
                  np=np+1
                 iva(np)=i
               endif
  13        continue
            if(np.eq.0)then
               call usrmsg('No applicable surfaces found!',' ','W')
               goto 43
            endif
            return
         elseif(act.eq.'i'.or.act.eq.'I')then
            np=0
            do 15 i=1,nzsur(izone)
               icn=izstocn(izone,i)
               if(ssother(icn,1)(1:5).eq.'EXTER'.and.
     &                    ssotf(icn)(1:4).ne.'OPAQ')then
                  np=np+1
                  iva(np)=i
               endif
   15       continue
            if(np.eq.0)then
               call usrmsg('No applicable surfaces found!',' ','W')
               goto 43
            endif
            return
         endif
      elseif(iic.eq.2)then
         inpic=nzsur(izone)
         do 11 i=1,inpic
            icn=izstocn(izone,i)
            if(act.eq.'s'.or.act.eq.'S')then
               if(ssother(ICN,1)(1:5).eq.'EXTER')then
                  write(stmp(I),'(5a)')ssname(icn),'|',ssotf(icn)(1:4),
     &                             '|',ssother(icn,1)(1:15)
               else
                  write(stmp(I),'(2a)')ssname(icn),' not applicable'
               endif
            elseif(act.eq.'-')then
               write(stmp(i),'(5a)')ssname(icn),'|',ssotf(icn)(1:4),
     &                             '|',ssother(icn,1)(1:15)
            elseif(act.eq.'i'.or.act.eq.'I')then
               if(ssother(icn,1)(1:5).eq.'EXTER'.and.
     &                           ssotf(icn)(1:4).ne.'OPAQ')then
                  write(stmp(i),'(5a)')ssname(icn),'|',ssotf(icn)(1:4),
     &                             '|',ssother(icn,1)(1:15)
               else
                  write(stmp(i),'(2a)')ssname(icn),' not applicable'
               endif
            endif
            iva(i)=0
   11    continue

         h(1)='Pick one, several or all surfaces for inclusion'
         h(2)='in analysis.'
         call epicks(inpic,ivals,' ','Surface(s) to include',
     &        33,nzsur(izone),stmp,'Surface(s) to include?',ier,2)
         if(inpic.eq.0)then
            return
         else
            np=inpic
            do 12 i=1,np
               iva(i)=ivals(i)
   12       continue
         endif

      elseif(iic.eq.4)then
         inpic=nzsur(izone)
         do 21 i=1,inpic
            icn=izstocn(izone,i)
            if(isc(i).eq.1)then
               write(stmp(i),'(5a)')ssname(icn),'|',ssotf(icn)(1:4),
     &                         '|',ssother(icn,1)(1:15)
            else
               write(stmp(i),'(2a)')ssname(icn),' not assessed'
            endif
            iva(i)=0
   21    continue
         inpic=1
         h(1)='Only surfaces that have been assessed are listed.'
         call epicks(inpic,ivals,' ','Surface to View',
     &           33,nzsur(izone),stmp,'Surface(s) to view?',ier,1)
         if(inpic.eq.0)then
            return
         else
            np=inpic
            iva(1)=ivals(1)
         endif
      endif
      return
      end

C ******************** ASKDAY 
C Returns 'imo' (month), 'ido' (day of month) and 'ijday' (day of year).
C 'ifday' is a toggle controlling the display and input of periods:
C 0 = Julian day, 1 or 2 = day of month.

      subroutine askday(ifday,imo,ido,ijday,ier)

      common/outin/iuout,iuin
      common/pophelp/h(60)

      character h*72
      dimension id(12)
      data id/31,28,31,30,31,30,31,31,30,31,30,31/

      ier=0
      h(1)='The date is either specified as a day and month number or'
      h(2)='as a year day in Julian notation (Jan 1 = 1, Dec 31 = 365).'
    1 if(ifday.eq.0)then
         call easki(ijday,' ','Year day?',
     &              1,'F',365,'F',1,'start day',ier,2)
         call edayr(ijday,ido,imo)
         if(ier.ne.0)goto 1
      else
         call easki(imo,' ', 'Month number?',
     &              1,'F',12,'F',1,'end day',ier,2)
         call easki(ido,' ', 'Day of the month?',
     &              1,'F',31,'F',1,'start day',ier,2)
         call eday(ido,imo,ijday)
      endif

C Check range.
      if(ido.gt.id(imo))then
         call edisp(iuout,'Day past end of month! Respecify.')
         goto 1
      endif
      call eday(ido,imo,ijday)
      return
      end

C ************* POINT1 **********************
C Determines if a specified point is within or outwith the
C target surface.

      subroutine point1(icomp,is,xx,zz,ians)
#include "building.h"
#include "geometry.h"

      common/g1t/xft(mcom,mv),yft(mcom,mv),zft(mcom,mv)
      common/c24/izstocn(mcom,ms)

      dimension c(2*mv+2)

      ians=0

C Get the current connection and number of surfaces in zone.
      icc=izstocn(icomp,is)

C Determine if point xx,zz is within surface 'is'.
      np=nznver(icc)
      do 10 i=1,np
         ii=2*i-1
         c(ii)=xft(icc,i)
         ii=ii+1
         c(ii)=zft(icc,i)
   10 continue
      last1=2*np+1
      last2=2*np+2
      c(last1)=xft(icc,1)
      c(last2)=zft(icc,1)
      ln=0
      ip=np+1
      do 20 i=2,ip
         n=2*i
         if((c(n-2)-zz)*(zz-c(n)))20,1,2
    2    if((zz-c(n-2))*(c(n-1)-c(n-3))/(c(n)-c(n-2))+c(n-3)-xx)20,3,4
    1    if(c(n-2)-c(n))5,6,7
    6    if((c(n-3)-xx)*(xx-c(n-1)))20,3,3
    5    if((zz-c(n-2))*(c(n-1)-c(n-3))/(c(n)-c(n-2))+c(n-3)-xx)20,3,8
    7    if((zz-c(n-2))*(c(n-1)-c(n-3))/(c(n)-c(n-2))+c(n-3)-xx)20,3,9
    9    ln=ln-2
    8    ln=ln-1
    4    ln=ln+2
   20 continue
      if((ln/4)*4.ne.ln)goto 3
      goto 11
    3 ians=1
   11 return
      end

C ************* POINT2 **********************
C Determines if point xx,zz is within or outwith the shadow polygon
C xs,zs.

      subroutine point2(xx,zz,ians)

      common/shad2/ishd,npoint,xs(5),zs(5)

      dimension c(12)

      ians=0
      np=npoint
      do 10 i=1,np
         ii=2*i-1
         c(ii)=xs(i)
         ii=ii+1
         c(ii)=zs(i)
   10 continue
      last1=2*np+1
      last2=2*np+2
      c(last1)=xs(1)
      c(last2)=zs(1)
      ln=0
      ip=np+1
      do 20 i=2,ip
         n=2*i
         if((c(n-2)-zz)*(zz-c(n)))20,1,2
    2    if((zz-c(n-2))*(c(n-1)-c(n-3))/(c(n)-c(n-2))+c(n-3)-xx)20,3,4
    1    if(c(n-2)-c(n))5,6,7
    6    if((c(n-3)-xx)*(xx-c(n-1)))20,3,3
    5    if((zz-c(n-2))*(c(n-1)-c(n-3))/(c(n)-c(n-2))+c(n-3)-xx)20,3,8
    7    if((zz-c(n-2))*(c(n-1)-c(n-3))/(c(n)-c(n-2))+c(n-3)-xx)20,3,9
    9    ln=ln-2
    8    ln=ln-1
    4    ln=ln+2
   20 continue
      if((ln/4)*4.ne.ln)goto 3
      goto 11
    3 ians=1
   11 return
      end
