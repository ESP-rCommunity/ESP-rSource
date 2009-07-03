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

C SYNOP.F of ish contains the following routines:

C SHSYNP:   Display table of direct and diffuse shading factors for analysed surfaces
C           based on scan of transitional file.
C SHADSYNP: Display table of direct and diffuse shading factors for analysed surfaces
C           based on common block data.
C INSYNP:   Display internal surface insolation proportions for analysed sources.

C ********** SHSYNP **********
C Displays a table showing the percentage direct and diffuse
C surface shading for each hour based on information held within
C a zone transitional shading file.

      subroutine shsynp(icomp)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      common/outin/iuout,iuin
      common/filep/ifil
      common/filech/ixopen,ixunit,ixpunit
      common/prec8/slat,slon
      common/g6/ssname(mcon),ssotf(mcon),ssmlcn(mcon),ssvfc(mcon),
     &          ssother(mcon,3),ssparent(mcon),ssuse(mcon,2)
      common/contr/mon,isc(ms),iyd
      common/headl/irecs(ms)
      common/c24/izstocn(mcom,ms)
      common/data2/pso(ms,mt),psof(ms,mt)

      dimension month(12),iavdec(12)
      character ssname*12,ssotf*32,ssmlcn*32,ssvfc*4,ssother*24,
     &          ssparent*12,ssuse*8
      character outs*124,month*3,sn*12
      logical ok,dok

      data iavdec/17,15,16,15,15,11,17,16,16,16,15,11/
      data month/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec'/

C If output to file, alter the edisp unit number.
      itru=iuout
      if(ixopen.eq.1)then
        itru = ixunit
        call edisp(iuout,' ')
        call edisp(iuout,'Output directed to file.')
      endif

      iunit=ifil

      call edisp(itru,' ')
      write(outs,'(a,i2,1x,2a)')'External surface shading for ',
     &                           iavdec(mon),month(mon),'.'
      call edisp(itru,outs)

C Retrieve shading information from zone transitional shading file
C and display on a surface-by-surface basis.
      do 10 i=1,nzsur(icomp)
        if(isc(i).eq.0)goto 10
        icn=izstocn(icomp,i)

C shddt & shdft are shading summations, ishdh is number of sun-up
C hours. 
        shddt=0
        shdft=0
        ishdh=0

C Write output header information.
        sn=ssname(icn)
        write(outs,'(a,i2,3a)') 'Surface ',i,' (',sn(1:lnblnk(sn)),')'
        call edisp(itru,outs)
        write(outs,'(3a)') ' Hour   Direct   Diffuse   Solar   Solar'
        call edisp(itru,outs)
        write(outs,'(3a)')
     &                 '        Shading  Shading   Azimuth Altitude'
        call edisp(itru,outs)

C Recover calculated shading for the current surface hour-by-hour.
        irec=irecs(i)+2
        iprint=0
        do 20 j=1,24
           stime=float(j)
	   
C << the value of isunupx should be the same as what is
C << read from the transitional file.
           call eazalts(stime,iyd,slat,slon,isunupx,sazi,salt)
           isunup=isunupx
           read(iunit,rec=irec,iostat=istat,err=1000)isunup
           irec=irec+1
           if(isunup.eq.0)then			! Sun not up.
              if(iprint.eq.2)then
                 write(outs,'(i2,a)')j,':00   After sun-set'
                 call edisp(itru,outs)
C Don't write again.
                 iprint=-1
              else
                 iprint=1
              endif
              pso(i,j)=0.0
              psof(i,j)=0.0
           else					! Sun up.
              if(iprint.eq.1)then
                 write(outs,'(i2,a)')j-1,':00   Before sun-rise'
                 call edisp(itru,outs)
              endif

C inds indicates whether direct shading is total, partial or zero.
C << if inds were written to the ascii file then we would not need
C << to use the transitional file for the summary it would need to
C << be a 2D array of 24 columns * nzsur rows

              read(iunit,rec=irec,iostat=istat,err=1000)inds
              irec=irec+1
              iprint=2

C No direct shading.
              if(inds.eq.-2)then
                 read(iunit,rec=irec,iostat=istat,err=1000)pso(i,j),
     &                                                     psof(i,j)
                 irec=irec+1
                 write(outs,'(i2,a,f8.1,a,2x,2f8.1)')j,
     &                    ':00     0.0%',psof(i,j)*100.0,'%',sazi,salt
                 call edisp(itru,outs)
                 shdft=shdft+psof(i,j)*100.0
                 ishdh=ishdh+1

C Total direct shading.
              elseif(inds.eq.-1)then
                 read(iunit,rec=irec,iostat=istat,err=1000)pso(i,j),
     &                                                     psof(i,j)
                 irec=irec+1
                 write(outs,'(i2,a,f8.1,a,2x,2f8.1)')j,
     &                    ':00   100.0%',psof(i,j)*100.0,'%',sazi,salt
                 call edisp(itru,outs)
                 shddt=shddt+100.0
                 shdft=shdft+psof(i,j)*100.0
                 ishdh=ishdh+1

C Partial direct shading: increment irec by nox to skip over
C ioshd array.
              elseif(inds.eq.0)then
                 irec=irec+nox(icomp)
                 read(iunit,rec=irec,iostat=istat,err=1000)pso(i,j),
     &                                                     psof(i,j)
                 irec=irec+1
                 write(outs,'(i2,a,f8.1,a,f8.1,a,2x,2f8.1)')j,':00',
     &                pso(i,j)*100.0,'%',psof(i,j)*100.0,'%',sazi,salt
                 call edisp(itru,outs)
                 shddt=shddt+pso(i,j)*100.0
                 shdft=shdft+psof(i,j)*100.0
                 ishdh=ishdh+1
              endif
           endif
   20   continue
        write(outs,'(a,f5.1,a)')
     &      'Averages for sun-up hours: Direct  = ',shddt/ishdh,'%'
        call edisp(itru,outs)
        write(outs,'(a,f5.1,a)')
     &      '                           Diffuse = ',shdft/ishdh,'%'
        call edisp(itru,outs)

C Determine if results are available for another surface?
        if(i.eq.nzsur(icomp))return
        mores=0
        do 30 j=i+1,nzsur(icomp)
           if(isc(j).ne.0)mores=1
   30   continue
        if(mores.eq.0)return
        dok=.true.
        call askok(' ','Consider next surface?',ok,dok,0)
        if(.not.ok)return
   10 continue

 1000 write(outs,1001)irec
 1001 format('SHSYNP: Zone transitional shading file error, record',i6)
      call edisp(iuout,' ')
      call edisp(iuout,outs)
      return
      end

C ********** SHADSYNP **********
C Displays a table showing the percentage direct and diffuse
C surface shading for each hour based on common block data.

      subroutine shadsynp(icomp)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      common/outin/iuout,iuin
      common/filech/ixopen,ixunit,ixpunit
      common/prec8/slat,slon
      common/g6/ssname(mcon),ssotf(mcon),ssmlcn(mcon),ssvfc(mcon),
     &          ssother(mcon,3),ssparent(mcon),ssuse(mcon,2)
      common/contr/mon,isc(ms),iyd
      common/c24/izstocn(mcom,ms)
      common/data2/pso(ms,mt),psof(ms,mt)

      dimension month(12),iavdec(12)
      character ssname*12,ssotf*32,ssmlcn*32,ssvfc*4,ssother*24,
     &          ssparent*12,ssuse*8
      character outs*124,month*3,sn*12
      logical ok,dok

      data iavdec/17,15,16,15,15,11,17,16,16,16,15,11/
      data month/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec'/

C If output to file, alter the edisp unit number.
      itru=iuout
      if(ixopen.eq.1)then
        itru = ixunit
        call edisp(iuout,' ')
        call edisp(iuout,'Output directed to file.')
      endif

      call edisp(itru,' ')
      write(outs,'(a,i2,1x,2a)')'External surface shading for ',
     &                           iavdec(mon),month(mon),'.'
      call edisp(itru,outs)

C Retrieve shading information from common blocks
C and display on a surface-by-surface basis.

C Re-establish what isc(i) is.
      icont=0
      call findisc(icomp,ier,'s',icont)
      do 10 i=1,nzsur(icomp)
        if(isc(i).eq.0)goto 10
        icn=izstocn(icomp,i)
        sn=ssname(icn)

C shddt & shdft are shading summations, ishdh is number of sun-up
C hours. 
        shddt=0
        shdft=0
        ishdh=0

C Write output header information.
        write(outs,'(a,i2,3a)') 'Surface ',i,' (',sn(1:lnblnk(sn)),')'
        call edisp(itru,outs)
        write(outs,'(3a)') ' Hour   Direct   Diffuse   Solar   Solar'
        call edisp(itru,outs)
        write(outs,'(3a)')
     &                 '        Shading  Shading   Azimuth Altitude'
        call edisp(itru,outs)

        iprint=0
        do 20 j=1,24
           stime=float(j)
	   
C The value of isunupx indicates if sun is up.
           call eazalts(stime,iyd,slat,slon,isunupx,sazi,salt)
           isunup=isunupx
           if(isunup.eq.0)then			! Sun not up.
              if(iprint.eq.2)then
                 write(outs,'(i2,a)')j,':00   After sun-set'
                 call edisp(itru,outs)
C Don't write again.
                 iprint=-1
              else
                 iprint=1
              endif
              pso(i,j)=0.0
              psof(i,j)=0.0
           else					! Sun up.
              if(iprint.eq.1)then
                 write(outs,'(i2,a)')j-1,':00   Before sun-rise'
                 call edisp(itru,outs)
              endif

C inds indicates whether direct shading is total, partial or zero.
C the value is not recorded in the binary or ascii file (yet) so
C assume format for mixed.
              iprint=2


C Partial direct shading: increment irec by nox to skip over
C ioshd array.
              write(outs,'(i2,a,f8.1,a,f8.1,a,2x,2f8.1)')j,':00',
     &              pso(i,j)*100.0,'%',psof(i,j)*100.0,'%',sazi,salt
              call edisp(itru,outs)
              shddt=shddt+pso(i,j)*100.0
              shdft=shdft+psof(i,j)*100.0
              ishdh=ishdh+1
           endif
   20   continue
        write(outs,'(a,f5.1,a)')
     &      'Averages for sun-up hours: Direct  = ',shddt/ishdh,'%'
        call edisp(itru,outs)
        write(outs,'(a,f5.1,a)')
     &      '                           Diffuse = ',shdft/ishdh,'%'
        call edisp(itru,outs)

C Determine if results are available for another surface?
        if(i.eq.nzsur(icomp))return
        mores=0
        do 30 j=i+1,nzsur(icomp)
           if(isc(j).ne.0)mores=1
   30   continue
        if(mores.eq.0)return
        dok=.true.
        call askok(' ','Consider next surface?',ok,dok,0)
        if(.not.ok)return
   10 continue

      return
      end

C ********** INSYNP **********
C Outputs information on internal surface insolation.
C parameter act (1 char) is 's' for silent mode and 'i' for interacitve
      subroutine insynp(icomp,act)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      common/outin/iuout,iuin
      common/filech/ixopen,ixunit,ixpunit
      common/prec8/slat,slon
      common/contr/mon,isc(ms),iyd
      common/g6/ssname(mcon),ssotf(mcon),ssmlcn(mcon),ssvfc(mcon),
     &          ssother(mcon,3),ssparent(mcon),ssuse(mcon,2)
      common/c24/izstocn(mcom,ms)
      common/data2/pso(ms,24),psof(ms,24)
      common/data3/ishd(12),isadd(12),ntmc(ms),ioffs(ms)
      common/data4/insst(mgt,24,misur),pinsst(mgt,24,misur)

      character month*3,outs*124,sn*12
      character ssname*12,ssotf*32,ssmlcn*32,ssvfc*4,ssother*24,
     &          ssparent*12,ssuse*8
      character act*1
      dimension month(12),iavdec(12)
      logical close,ok,dok

      data iavdec/17,15,16,15,15,11,17,16,16,16,15,11/
      data month/'Jan','Feb','Mar','Apr','May','Jun','Jul',
     &'Aug','Sep','Oct','Nov','Dec'/

      if(isadd(mon).eq.0.or.ishd(mon).eq.1)then
         call edisp(iuout,' ')
         write(outs,'(3a)')'No insolation data for ',month(mon),'!'
         call edisp(iuout,outs)
         return
      endif

C If output to file, alter the edisp unit number.
      itru = iuout
      if(ixopen.eq.1)then
        itru = ixunit
      endif

      kount=0
      call edisp(itru,' ')
      write(outs,'(a,i2,1x,2a)')'Internal surface insolation for ',
     &                           iavdec(mon),month(mon),
     &                  '.'
      call edisp(itru,outs)

C Display on a surface-by-surface basis.
      do 10 i=1,nzsur(icomp)
         if(ntmc(i).eq.0)goto 10
         icn=izstocn(icomp,i)
         sn=ssname(icn)

C ntmc is 0 or 1.
         kount=kount+ntmc(i)
         write(outs,'(a,i2,3a)') 'Source surface is ',i,' (',
     &                           sn(1:lnblnk(sn)),').'
         call edisp(itru,outs)
         call edisp(itru,
     & ' Hour  Source   Receiving     Proportion   Solar   Solar')
         call edisp(itru,
     & '       Shading  Surface       Received     Azimuth Altitude')

         iprint=0
         do 20 j=1,24
            stime=float(j)
            call eazalts(stime,iyd,slat,slon,isunupx,sazi,salt)
C Sun not up.
            if(insst(kount,j,1).eq.-1)then
               if(iprint.eq.2)then
                  write(outs,'(i2,a)')j,':00  After sun-set'
                  call edisp(itru,outs)
C Don't write again.
                  iprint=-1
               else
                  iprint=1
               endif
C Surface shaded.
            elseif(insst(kount,j,1).eq.0)then
               if(iprint.eq.1)then
                  write(outs,'(i2,a)')j-1,':00  Before sun-rise'
                  call edisp(itru,outs)
               endif
               write(outs,'(i2,a,28x,2f8.1)')  j,':00  100.0%',sazi,salt
               call edisp(itru,outs)
               iprint=2
C Surface insolated.
            else
               if(iprint.eq.1)then
                  write(outs,'(i2,a)')j-1,':00  Before sun-rise'
                  call edisp(itru,outs)
               endif
               if(pso(i,j).gt.0.999)then
                  write(outs,1000)j,sazi,salt
 1000             format(i2,':00 100.0%',2f8.1)
                  call edisp(itru,outs)
               else
                  do 30 k=1,misur
C Of misur possibilities only print internal surfaces with
C non zero pinsst.
                     call eclose(pinsst(kount,j,k),0.0,0.001,close)
                     if(.not.close)then
                        if(k.eq.1)then
                           l=insst(kount,j,k)
                           icm=izstocn(icomp,l)
                           sn=ssname(icm)
                           write(outs,1001)j,pso(i,j)*100.0,l,
     &                       sn(1:7),pinsst(kount,j,k)*100.0,sazi,salt
 1001                      format(i2,':00',f6.1,'%   ',i2,' (',a,')',
     &                             f9.1,'%',4x,2f8.1)
                           call edisp(itru,outs)
                        else
                           l=insst(kount,j,k)
                           icm=izstocn(icomp,l)
                           sn=ssname(icm)
                           write(outs,1002)l,sn(1:7),
     &                             pinsst(kount,j,k)*100.0
 1002                      format(15x,i2,' (',a,')',f9.1,'%')
                           call edisp(itru,outs)
                        endif
                     endif
   30             continue
               endif
               iprint=2
            endif
   20    continue

C Determine if results are available for another surface? If silent
C mode then list all surfaces.
         if(i.eq.nzsur(icomp))return
         mores=0
         do 40 j=i+1,nzsur(icomp)
            if(ntmc(j).ne.0)mores=1
   40    continue
         if(mores.eq.0)return
         dok=.true.
	 if(act.eq.'s')then
	   ok=.true.
	 else
           call askok(' ','Consider next surface?',ok,dok,0)
         endif
         if(.not.ok)return
   10 continue
      return
      end
