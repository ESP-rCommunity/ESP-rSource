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

C TRNFILE.F contains the following routines relating to the
C management of a zone transitional shading file.
C TFILE1: Writes model geometry to the zone transitional shading
C         file.
C TFILE2: writes the transformed coordinates of surfaces to the
C         zone transitional shading file.
C TFILE3: Saves the hourly grid shading index values and overall
C         surface shading factors to the zone transitional shading
C         file.
C RETRV1: Retrieves information from the header block of the zone
C         transitional shading file.

C ********** TFILE1 **********
C Writes the model geometry to the zone transitional shading file.

      subroutine tfile1(icomp)
#include "building.h"

C geometry.h provides commons G0, G2, GS5, GS6.
#include "geometry.h"

      common/outin/iuout,iuin
      common/filep/ifil
      common/contr/mon,isc(ms),iyd
      common/find/irecl
      character outs*124

      iunit=ifil

      irec=2
      write(iunit,rec=irec,iostat=istat,err=1000)mon,
     &  (isc(i),i=1,nzsur(icomp)),iyd
      irec=irec+1

      irecl=irec+1
      return

 1000 write(outs,1001)irec
 1001 format('TFILE1: Zone Transitional Shading file error, record',i5)
      call edisp(iuout,outs)
      return
      end

C ********** TFILE2 **********
C Transfers the transformed coordinates of the target surface
C to the zone transitional shading file.

      subroutine tfile2(icomp,is)
#include "building.h"
#include "geometry.h"

      common/outin/iuout,iuin
      common/filep/ifil
      common/g1t/xft(mv),zft(mv)
      common/find/irecl
      common/c24/izstocn(mcom,ms)

      dimension irecs(ms)
      character outs*124
      integer nv,icc,nzs   ! local for vertex, connection and surface

      iunit=ifil

C Get the current connection and number of surfaces in zone.
      icc=izstocn(icomp,is)
      nzs=nzsur(icomp)

C Recover and update irecs for surface `is`.
      irec=1
      read(iunit,rec=irec,iostat=istat,err=1000)(irecs(i),i=1,nzs)
      irecs(is)=irecl
      write(iunit,rec=irec,iostat=istat,err=1000)(irecs(i),i=1,nzs)
      irec=irecs(is)
      nv=nznver(icc)
      write(iunit,rec=irec,iostat=istat,err=1000)(xft(i),i=1,nv)
      irec=irec+1
      write(iunit,rec=irec,iostat=istat,err=1000)(zft(i),i=1,nv)
      irecl=irec+1
      return

 1000 write(outs,1)irec
    1 format('TFILE2: Zone Transitional Shading file error, record',i5)
      call edisp(iuout,outs)
      return
      end

C ********** TFILE3 **********
C Saves the hourly direct shading index values, corresponding to
C the target surface grid, to the zone transitional shading file
C and computes the overall direct and diffuse shading factors.

      subroutine tfile3(icomp,is,mode)
#include "building.h"
#include "geometry.h"

      common/outin/iuout,iuin
      common/tracech/icout
      common/filep/ifil
      common/g6/ssname(mcon),ssotf(mcon),ssmlcn(mcon),ssvfc(mcon),
     &          ssother(mcon,3),ssparent(mcon),ssuse(mcon,2)
      common/g1t/xft(mv),zft(mv)
      common/grid3/ogrida(ms)
      common/shad1/isunup
      common/shad3/ioshd(mox,moz),foshd(mox,moz),gssa,ipexcl
      common/find/irecl
      common/ihtime/ihour
      common/data2/pso(ms,mt),psof(ms,mt)
      common/c24/izstocn(mcom,ms)
      common/data1/ical,idifc,init

      character ssname*12,ssotf*32,ssmlcn*32,ssvfc*4,ssother*24,
     &          ssparent*12,ssuse*8
      character outs*248,mode*3
      logical tok

C If trace requested enable writing.
      tok=.false.
      if(icout.eq.33)tok=.true.

      iunit=ifil

C Get the current connection and number of surfaces in zone.
      icc=IZSTOCN(icomp,is)

C mode = dup; therefore use diffuse shading factors as calculated
C for last hour.
      if(mode.eq.'dup')then
         psof(is,ihour)=psof(is,ihour-1)
         irec=irecl
         write(iunit,rec=irec,iostat=istat,err=1000)pso(is,ihour),
     &                                              psof(is,ihour)
         irecl=irec+1
         goto 9999
      endif

C mode = dif; therefore this routine is only called after all sky
C patches have been processed.
      if(mode.eq.'dif')goto 1

C mode = dir or abn (i.e. sun not up in latter case).
      irec=irecl
      write(iunit,rec=irec,iostat=istat,err=1000)isunup
      irec=irec+1
      if(isunup.eq.1)goto 2

C Sun not up.
      xval=1.0
    7 pso(is,ihour)=xval
      psof(is,ihour)=xval
      goto 3

    2 is1=-1
      is2=-2
      do 10 i=1,nox(icomp)
         do 20 j=1,noz(icomp)
            if(ioshd(i,j).eq.-1)goto 20
            if(ioshd(i,j).eq.0)is1=0
            if(ioshd(i,j).eq.1)is2=1
   20    continue
   10 continue
      if(is1.eq.0.and.is2.eq.1) goto 4
      if(is1.eq.0.and.is2.eq.-2)goto 5
      if(is1.eq.-1.and.is2.eq.1)goto 6

C << the value of is1 or is2 is for the current surface and the
C << current hour. To store this will require a 2D array
C << this information would need to be written out to the
C << ascii file as 24 columns (hours) * nzsur rows.

C No direct shading, is2 = -2; write to zone transitional shading
C file.
    5 write(iunit,rec=irec,iostat=istat,err=1000)is2
      irec=irec+1
      xval=0.0
      goto 7

C Total direct shading, is1= -1; write to zone transitional shading
C file.
    6 write(iunit,rec=irec,iostat=istat,err=1000)is1
      irec=irec+1
      xval=1.0
      goto 7

C Partial direct shading, is1= 0; write ioshd to zone transitional
C shading file.
    4 write(iunit,rec=irec,iostat=istat,err=1000)is1
      irec=irec+1
      do 30 i=1,nox(icomp)
         write(iunit,rec=irec,iostat=istat,err=1000)
     &     (ioshd(i,j),j=1,noz(icomp))
         irec=irec+1
   30 continue

C Calculate overall shading on target surface, gssa (gssa is
C initialised in shadc before direct and diffuse processing).
    1 do 40 k=1,nox(icomp)
         do 50 l=1,noz(icomp)
            if(ioshd(k,l).eq.-1)goto 50
            if(mode.eq.'dir')gssa=gssa+ioshd(k,l)*ogrida(is)

C Next line only reached after all sky patches have been processed.
            if(mode.eq.'dif')gssa=gssa+foshd(k,l)*ogrida(is)
  50     continue
  40  continue

C Calculate gross surface area, gsa.
C << why calculate? this is already known >>>
      nv=nznver(icc)
      call area(nv,xft,zft,gsa)

      if(mode.eq.'dir')pso(is,ihour)=gssa/gsa
      if(mode.eq.'dif')then
C Adjust diffuse shading to relate only to the sky portion that is
C visible to the target surface. This is required because the calculation
C of the diffuse irradiance in bps does not include the obscured
C sky portion.
         adj=145.0/(145.0-float(ipexcl))
         psof(is,ihour)=gssa*adj/gsa
      endif
      icn=izstocn(icomp,is)

    3 irecl=irec

C Check for inappropriate shading values.
      if(mode.eq.'dir'.and.pso(is,ihour).gt.1.0)then
         write(outs,'(3a,f5.3,a,i2,a)') 'Direct shading factor for ',
     &        ssname(icn),' is ',pso(is,ihour),' @ ',ihour,
     &        'hrs - saved as 1.0.'
         call edisp248(iuout,outs,124)
         pso(is,ihour)=1.0
      elseif(mode.eq.'dir'.and.pso(is,ihour).lt.0.0)then
         write(outs,'(3a,f5.3,a,i2,a)') 'Direct shading factor for ',
     &        ssname(icn),' is ',pso(is,ihour),' @ ',ihour,
     &        'hrs - saved as 0.0.'
         call edisp248(iuout,outs,124)
         pso(is,ihour)=0.0
      elseif(mode.eq.'dif'.and.psof(is,ihour).gt.1.0)then
        write(outs,'(3a,f5.3,a,i2,a)') 'Diffuse shading factor for ',
     &        ssname(icn),' is ',psof(is,ihour),' @ ',ihour,
     &        'hrs - saved as 1.0.'
        call edisp248(iuout,outs,124)
        psof(is,ihour)=1.0
      elseif(mode.eq.'dif'.and.psof(is,ihour).lt.0.0)then
        write(outs,'(3a,f5.3,a,i2,a)') 'Diffuse shading factor for ',
     &        ssname(icn),' is ',psof(is,ihour),' @ ',ihour,
     &        'hrs - saved as 0.0.'
        call edisp248(iuout,outs,124)
        psof(is,ihour)=0.0
      endif

      if(mode.eq.'dif')then
         irec=irecl
         write(iunit,rec=irec,iostat=istat,err=1000)pso(is,ihour),
     &                                              psof(is,ihour)
         irecl=irec+1
      endif

C Trace output.
 9999 if(.not.tok)return
      call edisp(icout,' ')
      write(outs,9998)mode
 9998 format('*** TFILE3: mode ',a,'.')
      call edisp(icout,outs)
      write(outs,9997)is,ihour
 9997 format('Surface = ',i2,' Hour = ',i2)
      call edisp(icout,outs)
      if(isunup.eq.1)then
         write(outs,9996)gssa,gsa,ogrida(is)
 9996    format('Shadow area = ',f8.4,' Surface area = ',f8.4,
     &          ' Grid area =',f8.4)
         call edisp(icout,outs)
      endif
      if(mode.eq.'dir')then
         write(outs,9995)pso(is,ihour)
 9995    format('Direct shading = ',f8.4)
         call edisp(icout,outs)
         call edisp(icout,'IOSHD:')
         do 9994 j=noz(icomp),1,-1
            write(outs, 9993) (ioshd(i,j),i=1,nox(icomp))
 9993       format(50i3)
            call edisp(icout,outs)
 9994    continue
      elseif(mode.eq.'dif')then
         write(outs,9992)psof(is,ihour)
 9992    format('Diffuse shading = ',f8.4)
         call edisp(icout,outs)
         call edisp(icout,'FOSHD:')
         do 9991 j=noz(icomp),1,-1
            write(outs, 9990) (foshd(i,j),i=1,nox(icomp))
 9990       format(50f7.3)
            call edisp(icout,outs)
 9991    continue
      endif
      return

 1000 write(outs,1001)irec
 1001 format('TFILE3: Zone Transitional Shading file error, record',i5)
      call edisp(iuout,outs)
      return
      end

C ********** RETRV1 **********
C Retrives information from the header block of the zone transitional
C shading file as previously written by tfile1 and tfile12.

      subroutine retrv1(icomp)
#include "building.h"

C geometry.h provides commons G0,G2,GS5,GS6,GS7,GS8.
#include "geometry.h"

      common/outin/iuout,iuin
      common/filep/ifil
      common/contr/mon,isc(ms),iyd
      common/headl/irecs(ms)

      character outs*124

      iunit=ifil
      
      irec=1
      read(iunit,rec=irec,iostat=istat,err=1000)
     &  (irecs(i),i=1,nzsur(icomp))
      irec=2
      read(iunit,rec=irec,iostat=istat,err=1000)mon,
     &  (isc(i),i=1,nzsur(icomp)),iyd
      
      return

 1000 write(outs,1001)irec
 1001 format('RETRV1: Zone Transitional Shading file error, record',I6)
      call edisp(iuout,outs)
      return
      end
