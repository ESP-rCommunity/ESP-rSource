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

C SIFOPS - allows editing/importing/exporting of zone shading &
C          insolation db contents.
C SIFRD  - reads the contents of a zone shading & insolation db for
C          a given month.
C SIFWRT - writes shading/insolation data to a zone shading &
C          insolation db for a given month.

C ************* SIFOPS *************
C Allows the shading and insolation factors as held in a zone
C shading & insolation db to be listed, edited or imported/exported
C from/to a mirror ascii file. Uses SIFRD and SIFWRT to read/write
C db contents. Routines SIFILE, SSAVE and ISAVE are used to interact
C with the db at simulation time.

      subroutine sifops(icomp)
#include "building.h"

      common/outin/iuout,iuin
      common/pophelp/h(60)
      common/filep/ifil
      common/g6/ssname(mcon),ssotf(mcon),ssmlcn(mcon),ssvfc(mcon),
     &          ssother(mcon,3),ssparent(mcon),ssuse(mcon,2)
      common/c2/lsnam,nccode(mcom),lproj(mcom),lgeom(mcom),
     &          lshad(mcom),lthrm(mcom),indutl(mcom),lutil(mcom)
      common/c20/nzsur(mcom),nztv(mcom)
      common/c24/izstocn(mcom,ms)
      common/tmc/itmc(ms),nwins
      common/data2/pso(ms,24),psof(ms,24)
      common/data3/ishd(12),isadd(12),ntmc(ms),ioffs(ms)
      common/data4/insst(mgt,24,misur),pinsst(mgt,24,misur)
      common/filech/ixopen,ixunit,ixpunit

      character ssname*12,ssotf*32,ssmlcn*32,ssvfc*4,ssother*24,
     &          ssparent*12,ssuse*8
      character*72 lsnam,lproj,lgeom,lshad,lthrm,lutil
      character sn*12
      character*3 month(12)
      character outs*124,h*72,hold*74,expfil*72
      character outl*124,outld*124,outstr*124

      logical ok,dok,close

      DATA month/'Jan','Feb','Mar','Apr','May','Jun','Jul',
     &           'Aug','Sep','Oct','Nov','Dec'/

C Display contents of the zone shading & insolation db.
      ifilsi=ifil+3
      irec=1
      read(ifilsi,rec=irec,iostat=istat,err=1000)(ishd(i),i=1,12),
     &                                           (isadd(i),i=1,12)
      write(outs,'(2a)')'Content of zone shading & insolation db ',
     &                   lshad(icomp)
      call edisp(iuout,' ')
      call edisp(iuout,outs)
      write(outs,'(5x,a)')' Month   Saved data'
      call edisp(iuout,outs)
      ims=0
      mnth=1
      ifirst=0
      do 10 i=1,12
         if(ishd(i).ne.0)then
            ims=ims+1
            if(ifirst.eq.0)then
               mnth=i
               ifirst=1
            endif
         endif
         if(ishd(i).eq.1)then
            write(outs,'(5x,i2,4a)')
     &                     i,' (',month(i),')',' Shading only'
            call edisp(iuout,outs)
         elseif(ishd(i).eq.2)then
            write(outs,'(5x,i2,4a)')
     &                     i,' (',month(i),')',' Insolation only'
            call edisp(iuout,outs)
         elseif(ishd(i).eq.3)then
            write(outs,'(5x,i2,4a)')
     &                     i,' (',month(i),')',' Shading & insolation'
            call edisp(iuout,outs)
         endif
   10 continue
      if(ims.eq.0)then
         write(outs,'(9x,a)') '- Empty (returning to menu)'
         call edisp(iuout,outs)
         return
      endif

      h(1)='Specify the month as an integer where Jan=1, Dec=12 etc.'
    1 call easki(mnth,' ','Month number of interest?',1,'F',12,
     &           'F',1,'month number',ier,1)
      if(ier.ne.0)goto 1
      if(ishd(mnth).eq.0)then
         call edisp(iuout,' ')
         write(outs,'(2A)')
     &           'No data in Zone Shading & Insolation db for ',
     &            month(mnth),'!'
         call edisp(iuout,outs)
         goto 1
      endif

C Read db and fill commons DATA2, DATA3, DATA4 and DATA5 for
C specified month.
      call sifrd(icomp,mnth,ier)

C Present options menu.
      h(1)='The list option tabluates the shading and insolation'
      h(2)='factors as held for the specified month, the edit option'
      h(3)='allows these factors to be altered and the export/import'
      h(4)='options transfers the shading factors to/from a text file'
      h(5)='to support more complex editing, analysis by another tool'
      h(6)='or trasfser to an operating system with a different binary'
      h(7)='file representation'
      call easkatog(' ','Options:','list','edit','export','import',
     &              'cancel',' ',' ',ipick,7)

C *** Listing requested.
      if(ipick.eq.1)then
         icout=iuout
         if(ixopen.eq.1)icout=ixunit
         write(outs,'(3a)')'External surface shading for `',
     &                      month(mnth),'`.'
         call edisp(icout,' ')
         call edisp(icout,outs)
         write(outs,'(2a)')' Surface ___  Shading factors ',
     &         '___________________________________________________'
         call edisp(icout,outs)
         do 20 i=1,nzsur(icomp)
            icn=izstocn(icomp,i)
            sn=ssname(icn)
            write(outs,2)i,sn(1:7),(pso(i,j),j=1,12)
    2       format(I2,' (',a,')  Direct,  01-12 hrs ',12(f4.1))
            call edisp(icout,outs)
            write(outs,3)(pso(i,j),j=13,24)
    3       format(14x,'         13-24 hrs ',12(f4.1))
            call edisp(icout,outs)
            write(outs,4)(psof(i,j),j=1,12)
    4       format(14x,'Diffuse, 01-12 hrs ',12(f4.1))
            call edisp(icout,outs)
            write(outs,5)(psof(i,j),j=13,24)
    5       format(14x,'         13-24 hrs ',12(f4.1))
            call edisp(icout,outs)
   20    continue

C Insolation factors.
         if(nwins.eq.0.or.ishd(mnth).eq.1)goto 6
         kount=0
         write(outs,'(3a)')'Internal surface insolation for `',
     &                         month(mnth),'`.'
         call edisp(icout,' ')
         call edisp(icout,outs)
         do 30 i=1,nzsur(icomp)
            if(ioffs(i).eq.0) goto 30
            icn=izstocn(icomp,i)
            sn=ssname(icn)
            kount=kount+ntmc(i)
            write(outs,'(A,I2,3A)')'Source surface is ',i,' (`',
     &                            sn(1:lnblnk(sn)),'`).'
            call edisp(icout,outs)
            call edisp(icout,
     &                      ' Hour  Source   Receiving     Proportion')
            call edisp(icout,'       Shading  Surface       Received')
            iprint=0
            do 40 j=1,24
               if(insst(kount,j,1).eq.-1)then
                  if(iprint.eq.2)then
                     write(outs,'(i2,a)')j,':00  Sun not up'
                     call edisp(icout,outs)
C Don't write again.
                     iprint=-1
                  else
                     iprint=1
                  endif
               elseif(insst(kount,j,1).eq.0.or.pso(i,j).gt.0.999)
     &                                                            then
                  if(iprint.eq.1)then
                     write(outs,'(i2,a)')j-1,':00  Before sunrise'
                     call edisp(icout,outs)
                  endif
                  write(outs,'(i2,a)')j,':00  100.0%'
                  call edisp(icout,outs)
                  iprint=2
               else
                  if(iprint.eq.1)then
                     write(outs,'(i2,a)')j-1,':00  Before sunrise'
                     call edisp(icout,outs)
                  endif
                  do 50 k=1,misur
                     call eclose(pinsst(kount,j,k),0.0,0.001,close)
                     if(.not.close)then
                        if(k.eq.1)then
                           l=insst(kount,j,k)
                           icm=izstocn(icomp,l)
                           sn=ssname(icm)
                           write(outs,7)j,pso(i,j)*100.0,l,
     &                             sn(1:7),pinsst(kount,j,k)*100.0
    7                      format(i2,':00',f6.1,'%   ',i2,' (',a,')',
     &                             f9.1,'%')
                           call edisp(icout,outs)
                        else
                           l=insst(kount,j,k)
                           icm=izstocn(icomp,l)
                           sn=ssname(icm)
                           write(outs,8)l,sn(1:7),
     &                             pinsst(kount,j,k)*100.0
    8                      format(15x,i2,' (',a,')',f9.1,'%')
                           call edisp(icout,outs)
                        endif
                     endif
   50             continue
                  iprint=2
               endif
   40       continue
   30    continue

C *** Edit requested.
      elseif(ipick.eq.2)then
    9    h(1)='Integer number of surface within model.'
         call easki(isur,' ','Surface number?',
     &              1,'F',nzsur(icomp),'F',1,'surf number',ier,1)
         IF(ier.ne.0)goto 9

C Direct shading.
         icn=izstocn(icomp,isur)
         sn=ssname(icn)
   11    write(outs,'(3a)')'Direct shading factors for surface `',
     &                         sn(1:lnblnk(sn)),'`,'
         write(hold,'(12f5.2)') (pso(isur,i),i=1,12)
         h(1)='Direct shading factors for hours 01:00-12:00.'
         call easks(hold,outs,'01:00-12:00; edit as required:',
     &      60,' 0 0 0 0 0 0 0 0 0 0 0 0 ','1-12 dir shading',ier,1)
         if(ier.ne.0)goto 11
         ok=.true.
         k=0
         do 60 i=1,12
            call egetwr(hold,k,pso(isur,i),0.,1.,'W','1h00',ier)
            if(pso(isur,i).lt.0.0.or.pso(isur,i).gt.1.0)ok=.false.
   60    continue
         if(.not.ok)goto 11

   12    write(outs,'(3a)')'Direct shading factors for surface `',
     &                         sn(1:lnblnk(sn)),'`,'
         write(hold,'(12f5.2)') (pso(isur,i),i=13,24)
         h(1)='Direct shading factors for hours 13:00-24:00.'
         call easks(hold,outs,'13:00-24:00; edit as required:',
     &      60,' 0 0 0 0 0 0 0 0 0 0 0 0 ','13-24 dir shading',ier,1)
         if(ier.ne.0)goto 12
         ok=.true.
         k=0
         do 70 i=13,24
           call egetwr(hold,k,pso(isur,i),0.,1.,'W','1h00',ier)
           if(pso(isur,i).lt.0.0.or.pso(isur,i).gt.1.0)ok=.false.
   70    continue
         if(.not.ok)goto 12

C Diffuse shading.
   13    write(outs,'(3a)')'Diffuse shading factors for surface `',
     &                         sn(1:lnblnk(sn)),'`,'
         write(hold,'(12f5.2)') (psof(isur,i),i=1,12)
         h(1)='Diffuse shading factors for hours 01:00-12:00.'
         call easks(hold,outs,'01:00-12:00; edit as required:',
     &      60,' 0 0 0 0 0 0 0 0 0 0 0 0 ','1-12 dif shading',ier,1)
         if(ier.ne.0)goto 13
         ok=.true.
         k=0
         do 80 i=1,12
           call egetwr(hold,k,psof(isur,i),0.,1.,'W','1h00',ier)
           if(psof(isur,i).lt.0.0.or.psof(isur,i).gt.1.0)ok=.false.
   80    continue
         if(.not.ok)goto 13

   14    write(outs,'(3a)')'Diffuse shading factors for surface `',
     &                         sn(1:lnblnk(sn)),'`,'
         write(hold,'(12f5.2)') (psof(isur,i),i=13,24)
         h(1)='Diffuse shading factors for hours 13:00-24:00.'
         call easks(hold,outs,'13:00-24:00; edit as required:',
     &      60,' 0 0 0 0 0 0 0 0 0 0 0 0 ','13-24 dif shading',ier,1)
         if(ier.ne.0)goto 14
         ok=.true.
         k=0
         do 90 ik=13,24
           call egetwr(hold,k,psof(isur,ik),0.,1.,'W','1h00',ier)
           if(psof(isur,i).lt.0.0.or.psof(isur,i).gt.1.0)ok=.false.
   90    continue
         if(.not.ok)goto 14

         dok=.false.
         h(1)='Option to edit the shading factors for another surface.'
         call askok(' ','Edit another surface?',ok,dok,1)
         if(ok)goto 9

         dok=.true.
         h(1)='Unless saved to the zone shading & insolation file, the'
         h(2)='edited shading factors will not be preserved.'
         call askok(' ','Update Shading & Insolation db?',ok,dok,2)
         if(ok)call sifwrt(icomp,mnth)
					! End of `edit` menu selection.

C *** Export to ascii file requested.
      elseif(ipick.eq.3)then
         iuj=ifil+10
         h(1)='A text file to which the contents of the zone'
         h(2)='shading & insolation file will be written.'
         expfil='siexp.txt'
         call easks(expfil,' ','Export file name?',72,'siexp.txt',
     &              'shdins export',ier,2)
         call efopseq(iuj,expfil,3,ier)
         if(ier.lt.0)then
           ier=1
           goto 1002
         endif

         write(iuj,'(2a)',iostat=istat,err=1002)
     &      '* Shading and insolation data in db ',lshad(icomp)
         write(iuj,'(2i3,a)',iostat=istat,err=1002)icomp,nzsur(icomp),
     &      ' # zone index & number of surfaces'
   16    write(iuj,'(3a)',iostat=istat,err=1002) '* month: `',
     &      month(mnth),'`'

C Write direct and diffuse shading factors.
         write(iuj,'(a)',iostat=istat,err=1002) 
     &                         '24 hour external surface shading'
         do 100 i = 1,nzsur(icomp)
            write(outl,'(12f6.3)') (pso(i,j),j=1,12)
            call sdelim(outl,outld,'S',iw)
            write(iuj,'(a)',iostat=istat,err=1002)
     &                                     outld(1:lnblnk(outld))
            write(outl,'(12f6.3,a)') (pso(i,j),j=13,24),' # direct'
            call sdelim(outl,outld,'S',iw)
            write(iuj,'(a)',iostat=istat,err=1002)
     &                                     outld(1:lnblnk(outld))
            write(outl,'(12f6.3)') (psof(i,j),j=1,12)
            call sdelim(outl,outld,'S',iw)
            write(iuj,'(a)',iostat=istat,err=1002)
     &                                     outld(1:lnblnk(outld))
            write(outl,'(12f6.3,a,i2)') (psof(i,j),j=13,24),
     &                                      ' # diffuse - surface ',i
            call sdelim(outl,outld,'S',iw)
            write(iuj,'(a)',iostat=istat,err=1002)
     &                                     outld(1:lnblnk(outld))
 100     continue

C Write insolation factors.
         if(ishd(mnth).eq.2.or.ishd(mnth).eq.3)then
            write(iuj,'(a)',iostat=istat,err=1002) 
     &                        '24 hour internal surface insolation'
            write(iuj,'(a)',iostat=istat,err=1002) 'record offsets'
            write(outl,'(50i4)') (ioffs(i),i=1,nzsur(icomp))
            call sdelim(outl,outld,'S',iw)
            write(iuj,'(a)',iostat=istat,err=1002)
     &                                     outld(1:lnblnk(outld))
            do 110 i=1,nwins
               write(iuj,'(a,i2)',iostat=istat,err=1002)
     &                        'surfaces insolated by source ',i
               do 120 j=1,misur
                  write(outl,'(24i3)')(insst(i,k,j),k=1,24)
                  call sdelim(outl,outld,'S',iw)
                  write(iuj,'(a)',iostat=istat,err=1002)
     &                                     outld(1:lnblnk(outld))
  120          continue
               write(iuj,'(a)',iostat=istat,err=1002)
     &                        'matched surface insolation factors'
               do 130 j=1,misur
                  write(outl,'(24f5.2)')(pinsst(i,k,j),k=1,24)
                  call sdelim(outl,outld,'S',iw)
                  write(iuj,'(a)',iostat=istat,err=1002)
     &                                     outld(1:lnblnk(outld))
  130          continue
  110       continue
         endif

         write(outs,'(3a)') 'Data for `',month(mnth),'` written.'
         call edisp(iuout,' ')
         call edisp(iuout,outs)
         dok=.true.
         h(1)='Option to write data for next month.'
   15    call askok(' ','Continue with next month?',ok,dok,1)
         if(ok)then
           mnth=mnth+1
           if(mnth.gt.12)then
              write(iuj,'(A)',iostat=istat,err=1002) '* end'
              call erpfree(iuj,istat)
              call edisp(iuout,' ')
              call edisp(iuout,'Export file closed.')
              goto 6
           endif
           call sifrd(icomp,mnth,ier)
           if(ier.ne.0)then
              call edisp(iuout,' ')
              write(outs,'(2a)') month(mnth),' has no data!'
              call edisp(iuout,outs)
              goto 15
           endif
           goto 16
         else
            write(iuj,'(A)',iostat=istat,err=1002) '* end'
            call erpfree(iuj,istat)
            call edisp(iuout,' ')
            call edisp(iuout,'Export file closed.')
         endif
		     		     ! End of `export` menu selection.

C *** Import from ascii file requested.
      elseif(ipick.eq.4)then
         iuj=ifil+10
         H(1)='A text file from which the contents of the zone'
         H(2)='shading & insolation db may be written.'
         expfil='siimp.txt'
   17    call easks(expfil,' ','Import file name?',72,'siimp.txt',
     &             'shdins import',ier,2)
         call efopseq(iuj,expfil,1,ier)
         if(ier.ne.0)goto 1002
         call stripc(iuj,outstr,0,nd,1,'header',ier)
         if(ier.ne.0)goto 1002
         if(outstr(1:24).ne.'* Shading and insolation')then
            call usrmsg('Not an ascii zone shading & insolation file!',
     &                  expfil,'W')
            call erpfree(iuj,istat)
            goto 17
         endif
         call stripc(iuj,outstr,0,nd,1,'zone & surf',ier)
         if(ier.ne.0)goto 1002
         k=0
         call egetwi(outstr,k,iret,0,0,'-','zone index',ier)
         if(ier.ne.0)goto 1002
         if(iret.ne.icomp)then
           call usrmsg('These data are for a different zone!',
     &                                     expfil,'W')
           call erpfree(iuj,istat)
           goto 17
         endif
         call egetwi(outstr,k,iret,0,0,'-','surfaces',ier)
         if(ier.ne.0)goto 1002
         if(iret.ne.nzsur(icomp))then
           call usrmsg(
     &     'Number of surfaces in file does not match current model!',
     &     expfil,'W')
           call erpfree(iuj,istat)
           goto 17
         endif

         call stripc(iuj,outstr,0,nd,1,'month',ier)
         if(outstr(1:5).eq.'* end')goto 18
         if(ier.ne.0.or.outstr(1:7).ne.'* month')goto 1002
         if(outstr(11:13).eq.month(mnth))then
            call stripc(iuj,outstr,0,nd,1,'month',ier)
            if(outstr(1:16).ne.'24 hour external')goto 1002
            do 150 i=1,nzsur(icomp)
               call stripc(iuj,outstr,0,nd,1,'dir shading 01-12',ier)
               if(ier.ne.0)goto 1002
               k=0
               do 160 j=1,12
                  call egetwr(outstr,k,ret,0.,1.,'W',
     &                                        'dir shading 01-12',ier)
                  if(ier.ne.0)goto 1002
                  pso(i,j)=ret
  160          continue
               call stripc(iuj,outstr,0,nd,1,'dir shading 12-24',ier)
               if(ier.ne.0)goto 1002
               k=0
               do 170 j=13,24
                  call egetwr(outstr,k,ret,0.,1.,'W',
     &                                        'dir shading 12-24',ier)
                  if(ier.ne.0)goto 1002
                  pso(i,j)=ret
  170          continue
               call stripc(iuj,outstr,0,nd,1,'dif shading 01-12',ier)
               if(ier.ne.0)goto 1002
               k=0
               do 180 j=1,12
                  call egetwr(outstr,k,ret,0.,1.,'W',
     &                                        'dif shading 01-12',ier)
                  if(ier.ne.0)goto 1002
                  psof(i,j)=ret
  180          continue
               call stripc(iuj,outstr,0,nd,1,'dif shading 12-24',ier)
               if(ier.ne.0)goto 1002
               k=0
               do 190 j=13,24
                  call egetwr(outstr,k,ret,0.,1.,'W',
     &                                        'dif shading 12-24',ier)
                  if(ier.ne.0)goto 1002
                  psof(i,j)=ret
  190          continue

  150       continue
         else
            call usrmsg('Data in file is not for the specified month!',
     &                               expfil,'W')
            call erpfree(iuj,istat)
            goto 17
         endif

C Read insolation factors.
         call stripc(iuj,outstr,0,nd,1,'insolation header',ier)
         if(ier.ne.0.or.outstr(1:16).ne.'24 hour internal')goto 1002
         if(outstr(1:5).eq.'* end')goto 18
         call stripc(iuj,outstr,0,nd,1,'offsets header',ier)
         if(ier.ne.0.or.outstr(1:14).ne.'record offsets')goto 1002
         call stripc(iuj,outstr,0,nd,1,'offsets data',ier)
         if(ier.ne.0)goto 1002
         k=0
         do 200 i=1,nzsur(icomp)
            call egetwi(outstr,k,iret,0,0,'-','offset',ier)
            if(ier.ne.0)goto 1002
            ioffs(i)=iret
  200    continue

         do 210 i=1,nzsur(icomp)
            nwins=0
            if(ioffs(i).ne.0)then
               nwins=nwins+1
               call stripc(iuj,outstr,0,nd,1,'insst surf header',ier)
               if(ier.ne.0.or.outstr(1:21).ne.'surfaces insolated by')
     &                                                        goto 1002
               do 220 j=1,misur
                  call stripc(iuj,outstr,0,nd,1,'insst data',ier)
                  if(ier.ne.0)goto 1002
                  kk=0
                  do 230 k=1,24
                     call egetwi(outstr,kk,iret,-1,ms,'-','insst',ier)
                     if(ier.ne.0)goto 1002
                     insst(i,k,j)=iret
  230             continue
  220          continue
               call stripc(iuj,outstr,0,nd,1,'pinsst header',ier)
               if(ier.ne.0)goto 1002
               if(outstr(1:26).ne.'matched surface insolation')
     &                                                        goto 1002
               do 240 j=1,misur
                  call stripc(iuj,outstr,0,nd,1,'insst data',ier)
                  if(ier.ne.0)goto 1002
                  kk=0
                  do 250 k=1,24
                     call egetwr(outstr,kk,ret,0.,1.,'W','pinsst',ier)
                     if(ier.ne.0)goto 1002
                     pinsst(i,k,j)=ret
  250             continue
  240          continue
            endif
  210    continue
         if(nwins.eq.0)then
            call usrmsg('File has no insolation data!',expfil,'W')
            call erpfree(iuj,istat)
            goto 17
         endif
         call stripc(iuj,outstr,0,nd,1,'* end',ier)
         if(ier.ne.0)goto 1002
         if(outstr(1:5).ne.'* end')goto 1002

  18     call erpfree(iuj,istat)
         call edisp(iuout,' ')
         call edisp(iuout,'Import file closed.')
         dok=.true.
         h(1)='Imported data is held in memory and will be lost'
         h(2)='unless you update the zone shading & insolation db.'
         call askok(' ','Update zone shading & insolation db?',
     &              ok,dok,2)
         if(ok)call sifwrt(icomp,mnth)
					 ! End `import` menu selection.

C *** Cancel requested.
      elseif(ipick.eq.5)then
         return			   ! End of `cancel` menu selection.

      endif			   ! End of menu selections.           

C Consider another month or output option?
    6 dok=.false.
      h(1)='Option to edit the shading factors for another surface.'
      call askok(' ','Consider another month and/or output option?',
     &           ok,dok,1)
      if(ok)goto 1
      return

C Error handling.
 1000 write(outs,1001)
 1001 format('SIOPT: Zone Shading & Insolation text file error!')
      call edisp(iuout,' ')
      call edisp(iuout,outs)
      return

 1002 write(outs,1003)irec
 1003 format('SIOPT: Zone Shading & Insolation db error, record ',I6)
      call edisp(iuout,' ')
      call edisp(iuout,outs)
      return

      END

C ************* SIFRD *************
C Reads the contents of a zone shading & insolation db for the
C given month.

      subroutine sifrd(icomp,mnth,ier)
#include "building.h"

      common/outin/iuout,iuin
      common/filep/ifil
      common/c20/nzsur(mcom),nztv(mcom)
      common/tmc/itmc(ms),nwins

C isadd()  - start address of shading/insolation data for each
C            month with data.
C ishd()   - data type for month (0 no data, 1 shading only,
C            2 insolation only, 3 shading and insolation).
C pso()    - direct shading factor for each surface and hour.
C psof()   - diffuse shading factor for each surface and hour.
C ntmc()   - if 0 then surface is not an insolation source,
C            if 1 then it is.
C ioffs()  - the record offset to the insolation data for each
C            insolated surface associated with each insolation
C            source.
C insst()  - the index of the surface being insolated from source i
C            at hour j (0 indicates that no surface is shaded, -1
C            that the sun is not up). The index k refers to
C            the list of misur possibile insolated surfaces 
C pinsst(  - proportion received by each insolated surface from
C            each insolation source at each hour.
      common/data2/pso(ms,24),psof(ms,24)
      common/data3/ishd(12),isadd(12),ntmc(ms),ioffs(ms)
      common/data4/insst(mgt,24,misur),pinsst(mgt,24,misur)
      common/data5/irecx,nsurs,msurs

      character outs*124

      ier=0
      ifilsi=ifil+3
      irec=1
      read(ifilsi,rec=irec,iostat=istat,err=1000)(ishd(i),i=1,12),
     &                                           (isadd(i),i=1,12)
      irec=2
      read(ifilsi,rec=irec,iostat=istat,err=1000)irecx,nsurs,msurs

C Cosistency check.
      if(nzsur(icomp).ne.nsurs.or.msurs.ne.misur)goto 1002

C Record 3 reserved for future use, skip to 4.
      irec=4
      read(ifilsi,rec=irec,iostat=istat,err=1000)
     &                           (ntmc(i),i=1,nzsur(icomp))

C Number of insolation sources in zone.
      nwins=0
      irec=isadd(mnth)
      if(irec.eq.0)then
         ier=1
         return
      endif
      do 10 i=1,nzsur(icomp)
         nwins=nwins+ntmc(i)

C Get shading factors.
         read(ifilsi,rec=irec,iostat=istat,err=1000)(pso(i,j),j=1,24)
         irec=irec+1
         read(ifilsi,rec=irec,iostat=istat,err=1000)(psof(i,j),j=1,24)
         irec=irec+1
   10 continue

C Get insolation factors.
      if(ishd(mnth).eq.0.or.ishd(mnth).eq.1.or.nwins.eq.0)return
      read(ifilsi,rec=irec,iostat=istat,err=1000)
     &                              (ioffs(i),i=1,nzsur(icomp))
      irec=irec+1
      do 20 i=1,nwins
         do 30 j=1,misur
            read(ifilsi,rec=irec,iostat=istat,err=1000)
     &                                    (insst(i,k,j),k=1,24)
            irec=irec+1
   30    continue
         do 40 j=1,misur
            read(ifilsi,rec=irec,iostat=istat,err=1000)
     &                                    (pinsst(i,k,j),k=1,24)
            irec=irec+1
   40    continue
   20 continue
      return

C Error handling.
 1000 write(outs,1001)irec
 1001 format('SIFLST: Zone Shading & Insolation db error, record',I6)
      call edisp(iuout,' ')
      call edisp(iuout,outs)
      return

 1002 call edisp (iuout,' ')
      call edisp(iuout,'Geometry referenced in Zone Shading &')
      call edisp(iuout,'Insolation db is different to that currently')
      call edisp(iuout,'being used.')
      return
      end

C ************* SIFWRT *************
C Writes shading/insolation data to a zone shading & insolation db
C for the given month.

      subroutine sifwrt(icomp,mnth)
#include "building.h"

      common/outin/iuout,iuin
      common/filep/ifil
      common/c20/nzsur(mcom),nztv(mcom)
      common/tmc/itmc(ms),nwins

C isadd()  - start address of shading/insolation data for each
C            month with data.
C ishd()   - data type for month (0 no data, 1 shading only,
C            2 insolation only, 3 shading and insolation).
C pso()    - direct shading factor for each surface and hour.
C psof()   - diffuse shading factor for each surface and hour.
C ntmc()   - if 0 then surface is not an insolation source,
C            if 1 then it is.
C ioffs()  - the record offset to the insolation data for each
C            insolated surface associated with each insolation
C            source.
C insst(i,j,k)  - the index of kth internal surface being insolated
C                 from source i at hour j (0 indicates whole surface
C                 shading, -1 that the sun is not up).
C pinsst(i,j,k) - proportion of insolation associated with source i
C                 at hour j that reaches the kth internal surface.
      common/data2/pso(ms,24),psof(ms,24)
      common/data3/ishd(12),isadd(12),ntmc(ms),ioffs(ms)
      common/data4/insst(mgt,24,misur),pinsst(mgt,24,misur)
      common/data5/irecx,nsurs,msurs

      character outs*124

C SI db unit number.
      ifilsi=ifil+3
      if(isadd(mnth).eq.0)then
         isadd(mnth)=irecx
         ishd(mnth)=1
      endif
      irec=1
      write(ifilsi,rec=irec,iostat=istat,err=1000)(ishd(i),i=1,12),
     &                                            (isadd(i),i=1,12)
      irec=2
      write(ifilsi,rec=irec,iostat=istat,err=1000)irecx,nsurs,msurs

C Record 3 reserved for future use, skip to 4.
      irec=4
      write(ifilsi,rec=irec,iostat=istat,err=1000)
     &                           (ntmc(i),i=1,nzsur(icomp))

C Number of insolation sources in zone.
      nwins=0
      irec=isadd(mnth)
      do 10 i=1,nzsur(icomp)
         nwins=nwins+ntmc(i)

C Write shading factors.
         write(ifilsi,rec=irec,iostat=istat,err=1000)(pso(i,j),j=1,24)
         irec=irec+1
         write(ifilsi,rec=irec,iostat=istat,err=1000)(psof(i,j),j=1,24)
         irec=irec+1
   10 continue

C Write insolation factors.
      if(ishd(mnth).eq.0.or.ishd(mnth).eq.1.or.nwins.eq.0) return
      write(ifilsi,rec=irec,iostat=istat,err=1000)
     &                                (ioffs(i),i=1,nzsur(icomp))
      irec=irec+1
      do 20 i=1,nwins
         do 30 j=1,misur
            write(ifilsi,rec=irec,iostat=istat,err=1000)
     &                                     (insst(i,k,j),k=1,24)
            irec=irec+1
   30    continue
         do 40 j=1,misur
            write(ifilsi,rec=irec,iostat=istat,err=1000)
     &                                     (pinsst(i,k,j),k=1,24)
            irec=irec+1
   40    continue
   20 continue
      return

C Error handling.
 1000 write(outs,1001)irec
 1001 format('SIFWRT: Zone Shading & Insolation db error, record',I6)
      call edisp(iuout,' ')
      call edisp(iuout,outs)
      return
      end
