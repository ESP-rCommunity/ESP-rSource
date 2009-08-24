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
C SIFEXP exports the shading and insolation factors as held in a zone
C          shading db to a mirror ascii file (but silently).
C SIFIMPORT imports the shading and insolation factors as held in an
C         ASCII zone shading & insolation db to bimary file.

C ************* SIFOPS *************
C Allows the shading and insolation factors as held in a zone
C shading & insolation db to be listed, edited or imported/exported
C from/to a mirror ascii file. Uses SIFRD and SIFWRT to read/write
C db contents. Routines SIFILE, SSAVE and ISAVE are used to interact
C with the db at simulation time.

      subroutine sifops(icomp)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      common/outin/iuout,iuin
      common/pophelp/h(60)
      common/filep/ifil
      common/g6/ssname(mcon),ssotf(mcon),ssmlcn(mcon),ssvfc(mcon),
     &          ssother(mcon,3),ssparent(mcon),ssuse(mcon,2)

      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      common/c2/lsnam,nccode(mcom),lproj(mcom),lgeom(mcom),
     &          lshad(mcom),lthrm(mcom),indutl(mcom),lutil(mcom)
      common/c24/izstocn(mcom,ms)
      common/tmc/itmc(ms),nwins
      common/data2/pso(ms,24),psof(ms,24)
      common/data3/ishd(12),isadd(12),ntmc(ms),ioffs(ms)
      common/data4/insst(mgt,24,misur),pinsst(mgt,24,misur)
      common/data5/irecx,nsurs,msurs
      common/filech/ixopen,ixunit,ixpunit
      common/mtfile/ltrns,multic,mons,monf

      character ltrns*72
      character ssname*12,ssotf*32,ssmlcn*32,ssvfc*4,ssother*24,
     &          ssparent*12,ssuse*8
      character sn*12
      character*12 sourcename(MS)   ! name of insolation source
      character*3 month(12)
      character outs*124,louts*248,h*72,hold*74,expfil*72
      character outl*248,outld*248,outstr*124,loutstr*248
      character message*64
      integer isourcecount  ! to help with sourcename list
      integer nboftmc       ! number of insolation sources returned from findtmc
      integer itu           ! for reporting

      logical ok,dok

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
         elseif(ishd(i).eq.0)then
            write(outs,'(5x,i2,4a)')
     &                     i,' (',month(i),')',' No data'
            call edisp(iuout,outs)
         endif
   10 continue
   
C If there is nothing in the shading file then the user may wish to
C import from ascii file to fill it.
      if(ims.eq.0)then
        h(1)='The current shading file is empty. You can fill it with'
        h(2)='data from an ascii version of the file (if you have one)'
        h(3)='otherwise you should cancel your request.'
        h(4)=' '
        h(5)='The the ASCII file has several months of data the  '
        h(6)='subsequent months will be scanned. '
        call easkabc('Zone Shading & Insolation db is empty!',
     &    'Options:','import ascii','return & calculate','cancel',iw,6)
        if(iw.eq.1)then

C An empty shading file so setup some intial information. User is assumed
C to know which months are included in the ASCII file. Identify a month.
          h(1)='Specify the month as an integer where Jan=1, Dec=12.'
          h(2)='You are expected to know the contents of the ASCII'
          h(3)='that you are about to report.'
          mnth=1
 22       call easki(mnth,' ','Month number to start importing?',1,
     &      'F',12,'F',1,'month number',ier,3)
          if(ier.ne.0)goto 22
          ipick=4                 ! this is the import option
          if(irecx.eq.0)irecx=5   ! irecx probably was not set so do it for initial month
          isadd(mnth)=irecx       ! so code knows where to write data 
          nsurs=nzsur(icomp)      ! needed for the header
          msurs=misur
          call findtmc(icomp,'s',nboftmc) ! establish insolation sources and copy to ntmc
          do 44 ij=1,nsurs
            ntmc(ij)=itmc(ij)
 44       continue
          goto 77                 ! jump to the point where the task is invoked
        elseif(iw.eq.2)then
          write(outs,'(9x,a)') '- Empty (returning to menu)'
          call edisp(iuout,outs)
          return
        elseif(iw.eq.3)then
          write(outs,'(9x,a)') '- Empty (returning to menu)'
          call edisp(iuout,outs)
          return
        endif
      endif

C Identify a month.
      h(1)='Specify the month as an integer where Jan=1, Dec=12 etc.'
    1 call easki(mnth,' ','Month number of interest?',1,'F',12,
     &           'F',1,'month number',ier,1)
      if(ier.ne.0)goto 1
      if(ishd(mnth).eq.0)then
         call edisp(iuout,' ')
         write(outs,'(2A)')
     &     'No data in Zone Shading & Insolation file for ',
     &     month(mnth),'!'
         call edisp(iuout,outs)
         goto 1
      endif

C Read db and fill commons DATA2, DATA3, DATA4 and DATA5 for
C specified month.
      call sifrd(icomp,mnth,ier)

C Present options menu.
      h(1)='The list option tabulates the shading and insolation'
      h(2)='factors as held for the specified month, the edit option'
      h(3)='allows these factors to be altered and the export/import'
      h(4)='options transfers the shading factors to/from a text file'
      h(5)='to support more complex editing, analysis by another tool'
      h(6)='or trasfser to an operating system with a different binary'
      h(7)='file representation.'
      h(8)=' '
      h(9)='After calculations are completed, during a normal exit'
      h(10)='from the ish module a backup ASCII file will normally be'
      h(11)='created. If it is not then used the export option. '
      call easkatog(' ','Options:','list','edit','export','import',
     &              'cancel',' ',' ',ipick,11)

C *** Listing requested.
  77  continue
      if(ipick.eq.1)then
         itu=iuout
         if(ixopen.eq.1)itu=ixunit
         write(outs,'(3a)')'External surface shading for `',
     &                      month(mnth),'`.'
         call edisp(itu,' ')
         call edisp(itu,outs)
         write(outs,'(2a)')' Surface ___  Shading factors ',
     &         '___________________________________________________'
         call edisp(itu,outs)
         do 20 i=1,nzsur(icomp)
            icn=izstocn(icomp,i)
            sn=ssname(icn)
            write(outs,2)i,sn(1:7),(pso(i,j),j=1,12)
    2       format(I2,' (',a,')  Direct,  01-12 hrs ',12(f4.1))
            call edisp(itu,outs)
            write(outs,3)(pso(i,j),j=13,24)
    3       format(14x,'         13-24 hrs ',12(f4.1))
            call edisp(itu,outs)
            write(outs,4)(psof(i,j),j=1,12)
    4       format(14x,'Diffuse, 01-12 hrs ',12(f4.1))
            call edisp(itu,outs)
            write(outs,5)(psof(i,j),j=13,24)
    5       format(14x,'         13-24 hrs ',12(f4.1))
            call edisp(itu,outs)
   20    continue

C Insolation factors. Call insynp in silent mode.
         if(nwins.eq.0.or.ishd(mnth).eq.1)goto 6
         call insynp(icomp,'s')

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
         if(ok)then
           call sifwrt(icomp,mnth,lastrec)
         endif

C End of `edit` menu selection.

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
            icn=izstocn(icomp,i)
            sn=ssname(icn)
            write(outl,'(12f7.4)') (pso(i,j),j=1,12)
            call sdelim(outl,outld,'S',iw)
            write(iuj,'(a)',iostat=istat,err=1002)
     &                                     outld(1:lnblnk(outld))
            write(outl,'(12f7.4,2a)') (pso(i,j),j=13,24),
     &        ' # direct - surface ',sn(1:lnblnk(sn))
            call sdelim(outl,outld,'S',iw)
            write(iuj,'(a)',iostat=istat,err=1002)
     &                                     outld(1:lnblnk(outld))
            write(outl,'(12f7.4)') (psof(i,j),j=1,12)
            call sdelim(outl,outld,'S',iw)
            write(iuj,'(a)',iostat=istat,err=1002)
     &                                     outld(1:lnblnk(outld))
            write(outl,'(12f7.4,2a)') (psof(i,j),j=13,24),
     &        ' # diffuse - surface ',sn(1:lnblnk(sn))
            call sdelim(outl,outld,'S',iw)
            write(iuj,'(a)',iostat=istat,err=1002)
     &                                     outld(1:lnblnk(outld))
 100     continue
 
C Generate packed list of names of insolation sources.
         isourcecount=0
         do 101 i = 1,nzsur(icomp)
           if(ioffs(i).ne.0)then
             isourcecount=isourcecount+1
             icn=izstocn(icomp,i)
             sourcename(isourcecount)=ssname(icn)
           endif
 101     continue

C Write insolation factors. Note: string buffer outl will only
C hold 62 indices before it hits an overflow state.
         if(ishd(mnth).eq.2.or.ishd(mnth).eq.3)then
            write(iuj,'(a)',iostat=istat,err=1002) 
     &                        '24 hour internal surface insolation'
            write(iuj,'(2a)',iostat=istat,err=1002) 'record offsets',
     &        ' # to each insolation source (marked by non-zero)'
            if(nzsur(icomp).lt.99)then
              write(outl,'(80i3)') (ioffs(i),i=1,nzsur(icomp))
            else
              write(outl,'(62i4)') (ioffs(i),i=1,nzsur(icomp))
            endif
            call sdelim(outl,outld,'S',iw)
            write(iuj,'(a)',iostat=istat,err=1002)
     &                                     outld(1:lnblnk(outld))
            do 110 i=1,nwins
               write(iuj,'(a,i2,3a)',iostat=istat,err=1002)
     &           'surfaces insolated by source ',i,' ',sourcename(i),
     &           ' # hourly columns, ranking via rows'
               do 120 j=1,misur
                  write(outl,'(24i3)')(insst(i,k,j),k=1,24)
                  call sdelim(outl,outld,'S',iw)
                  write(iuj,'(a)',iostat=istat,err=1002)
     &                                     outld(1:lnblnk(outld))
  120          continue
               write(iuj,'(2a)',iostat=istat,err=1002)
     &           'matched surface insolation factors',
     &           ' # hourly columns, ranking via rows'
               do 130 j=1,misur
                  write(outl,'(24f6.3)')(pinsst(i,k,j),k=1,24)
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
C End of `export` menu selection.

C *** Import from ascii file requested.  Note this code does not yet
C update the value of irecx. The previous call to sifwrt returns a
C value lastrec which is equivalent to irecx.
      elseif(ipick.eq.4)then
         iuj=ifil+10
         H(1)='A text file from which the contents of the zone'
         H(2)='shading & insolation db may be written.'
         write(expfil,'(2a)') 
     &     lshad(icomp)(1:lnblnk(lshad(icomp))),'a'
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

C Read data for a month. If we have a month that matches then
C the code looks for either '24 hour external' in which case
C the data is shading data. If
         multic=2  ! signal data is known
         call stripc(iuj,outstr,0,nd,1,'month',ier)
 172     if(outstr(1:5).eq.'* end')goto 18
         if(ier.ne.0.or.outstr(1:7).ne.'* month')goto 1002
         if(outstr(11:13).eq.month(mnth))then
            call stripc(iuj,outstr,0,nd,1,'24 hour external',ier)
            if(outstr(1:16).ne.'24 hour external')then
              if(outstr(1:16).ne.'24 hour internal')then
                ishd(mnth)=2   ! no shading only insolation
                goto 173
              endif
              message='expecting 24 hours external'
              goto 1002
            endif
            ishd(mnth)=1   ! at least shading
            do 150 i=1,nzsur(icomp)
               message='reading direct shading'
               call lstripc(iuj,loutstr,0,nd,1,'dir shading 01-12',ier)
               if(ier.ne.0)goto 1004
               k=0
               do 160 j=1,12
                  call egetwr(loutstr,k,ret,0.,1.,'W',
     &                                        'dir shading 01-12',ier)
                  if(ier.ne.0)goto 1004
                  pso(i,j)=ret
  160          continue
               call lstripc(iuj,loutstr,0,nd,1,'dir shading 12-24',ier)
               if(ier.ne.0)goto 1004
               k=0
               do 170 j=13,24
                  call egetwr(loutstr,k,ret,0.,1.,'W',
     &                                        'dir shading 12-24',ier)
                  if(ier.ne.0)goto 1004
                  pso(i,j)=ret
  170          continue
       
               message='reading diffuse shading'
               call lstripc(iuj,loutstr,0,nd,1,'dif shading 01-12',ier)
               if(ier.ne.0)goto 1004
               k=0
               do 180 j=1,12
                  call egetwr(loutstr,k,ret,0.,1.,'W',
     &                                        'dif shading 01-12',ier)
                  if(ier.ne.0)goto 1004
                  psof(i,j)=ret
  180          continue
               call lstripc(iuj,loutstr,0,nd,1,'dif shading 12-24',ier)
               if(ier.ne.0)goto 1004
               k=0
               do 190 j=13,24
                  call egetwr(loutstr,k,ret,0.,1.,'W',
     &                                        'dif shading 12-24',ier)
                  if(ier.ne.0)goto 1004
                  psof(i,j)=ret
  190          continue

  150       continue
         else
            if(mnth.gt.12) goto 18  ! have reached past december
            call usrmsg('Data in file is not for the specified month!',
     &                               expfil,'W')
            call erpfree(iuj,istat)
            goto 17
         endif

C Read insolation factors. But the next line might also hold
C a Month string.
         call stripc(iuj,outstr,0,nd,1,'insolation header',ier)
 173     if(ier.ne.0.or.outstr(1:16).ne.'24 hour internal')then
           if(outstr(1:7).eq.'* month')then
             dok=.true.
             h(1)='Imported data is held in memory and will be lost'
             h(2)='unless you update the zone shading & insolation db.'
             call askok(' ','Update zone shading & insolation db?',
     &              ok,dok,2)
             if(ok)then
               call sifwrt(icomp,mnth,lastrec)
               if(lastrec.gt.5)then
                 if(isadd(mnth+1).eq.0) isadd(mnth+1)=lastrec
               endif
             endif
             mnth=mnth+1
             goto 172
           elseif(outstr(1:5).eq.'* end')then
	     goto 18
           endif
           message='expecting 24 hours internal'
           goto 1002
         endif
         if(outstr(1:5).eq.'* end')goto 18
         call stripc(iuj,outstr,0,nd,1,'offsets header',ier)
         if(ier.ne.0.or.outstr(1:14).ne.'record offsets')then
           message='expecting record offsets'
           goto 1002
         endif
         if(ishd(mnth).eq.1) ishd(mnth)=3   ! set to shading + insul
 
C Read the offsets line. Items with a zero are not insolation
C sources, non-zero entries are offsets to use within the binary
C file and the position in the list indicates with surface in
C the zone is an insolation source.
         call lstripc(iuj,loutstr,0,nd,1,'offsets data',ier)
         if(ier.ne.0)goto 1004
         k=0
         do 200 i=1,nzsur(icomp)
            call egetwi(loutstr,k,iret,0,0,'-','offset',ier)
            if(ier.ne.0)goto 1004
            ioffs(i)=iret
  200    continue

C Reset the counter for insolation sources. Use the nwins
C value for the first index of insst
         nwins=0
         do 210 i=1,nzsur(icomp)
            if(ioffs(i).ne.0)then
               nwins=nwins+1
               call stripc(iuj,outstr,0,nd,1,'insst surf header',ier)
               if(ier.ne.0.or.outstr(1:21).ne.
     &           'surfaces insolated by')then
                 message='expecting surfaces insolated by'
                 goto 1002
               endif
               do 220 j=1,misur
                  call stripc(iuj,outstr,0,nd,1,'insst data',ier)
                  if(ier.ne.0)goto 1002
                  kk=0
                  do 230 k=1,24
                     call egetwi(outstr,kk,iret,-1,ms,'-','insst',ier)
                     if(ier.ne.0)then
                       message='while reading insst data'
                       goto 1002
                     endif
                     insst(nwins,k,j)=iret
  230             continue
  220          continue
       
               call stripc(iuj,outstr,0,nd,1,'pinsst header',ier)
               if(ier.ne.0)goto 1002

               if(outstr(1:26).ne.'matched surface insolation')then
                 message='expecting matched surface insolation'
                 goto 1002
               endif
               do 240 j=1,misur
                  call lstripc(iuj,loutstr,0,nd,1,'pinsst line',ier)
                  if(ier.ne.0)goto 1004
                  kk=0
                  do 250 k=1,24
                     call egetwr(loutstr,kk,ret,0.,1.,'W','pinsst',ier)
                     if(ier.ne.0)then
                       message='while reading pinsst data'
                       goto 1004
                     endif
                     pinsst(nwins,k,j)=ret
  250             continue
  240          continue
            endif
  210    continue

C << why abort if there is no insolation data? >>
C         if(nwins.eq.0)then
C            call usrmsg('File has no insolation data!',expfil,'W')
C            call erpfree(iuj,istat)
C            goto 17
C         endif
         call stripc(iuj,outstr,0,nd,1,'* end',ier)
         if(ier.ne.0)goto 1002
         if(outstr(1:5).ne.'* end')then
           if(outstr(1:7).eq.'* month')then
             dok=.true.
             h(1)='Imported data is held in memory and will be lost'
             h(2)='unless you update the zone shading & insolation db.'
             call askok(' ','Update zone shading & insolation db?',
     &              ok,dok,2)
             if(ok)then
               call sifwrt(icomp,mnth,lastrec)
               if(lastrec.gt.5)then
                 if(isadd(mnth+1).eq.0) isadd(mnth+1)=lastrec
               endif
             endif
             mnth=mnth+1
             goto 172
           endif
           message='Expecting * end'
           goto 1002
         endif

  18     call erpfree(iuj,istat)
         call edisp(iuout,' ')
         call edisp(iuout,'Import file closed.')
         dok=.true.
         h(1)='Imported data is held in memory and will be lost'
         h(2)='unless you update the zone shading & insolation db.'
         call askok(' ','Update zone shading & insolation db?',
     &              ok,dok,2)
         if(ok)then
           call sifwrt(icomp,mnth,lastrec)
           if(lastrec.gt.5)then
             if(isadd(mnth+1).eq.0) isadd(mnth+1)=lastrec
           endif
         endif

C End `import` menu selection.

C *** Cancel requested.
      elseif(ipick.eq.5)then
         return       ! End of `cancel` menu selection.

      endif           ! End of menu selections.           

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
      call edisp(iuout,outstr)
      return

C Error written out in several lines with message identifying the
C type of data involved.
 1002 write(louts,'(3a)') 'sifops: Zone Shading & Insolation ',
     &   message(1:lnblnk(message)),' in the line '
      call edisp248(iuout,louts,90)
      call edisp(iuout,outstr)
      return

C Error written out in several lines with message identifying the
C type of data involved.
 1004 write(louts,'(3a)') 'sifops: Zone Shading & Insolation ',
     &   message(1:lnblnk(message)),' in the line '
      call edisp248(iuout,louts,90)
      call edisp248(iuout,loutstr,90)
      return

      END

C ************* SIFRD *************
C Reads the contents of a zone shading & insolation db for the
C given month.

      subroutine sifrd(icomp,mnth,ier)
#include "building.h"
#include "geometry.h"

      common/outin/iuout,iuin
      common/filep/ifil
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
C The lastrecwritten is an integer to pass back in case the calling
C code needs up update irecx
      subroutine sifwrt(icomp,mnth,lastrecwritten)
#include "building.h"
#include "geometry.h"

      common/outin/iuout,iuin
      common/filep/ifil
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

C Debug.
C      write(6,*)irecx,nsurs,msurs

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

C Debug.
C         write(6,*)(pso(i,j),j=1,24)

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
 
C Debug.
C            write(6,*)(insst(i,k,j),k=1,24)

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
      lastrecwritten=irec
      return

C Error handling.
 1000 write(outs,1001)irec
 1001 format('SIFWRT: Zone Shading & Insolation db error, record',I6)
      call edisp(iuout,' ')
      call edisp(iuout,outs)
      return
      end


C ************* SIFEXP *************
C SIFEXP exports the shading and insolation factors as held in a zone
C shading & insolation db to a mirror ascii file. Uses SIFRD to read
C db contents. Uses some of the same code blocks as sifops but is
C written for silent operation. If the format changes in sifops then
C sifexp should also be updated.
C Passed parameters:
C  icomp (integer) is the current zone index
C  exportfile (72 char) is the ascii file to generate (typically
C    this would be a name derived from the zone shading file).
C  ier (integer) is zero if no problems, -3 if empty.
      subroutine sifexp(icomp,exportfile,ier)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      common/outin/iuout,iuin
      common/filep/ifil
      common/g6/ssname(mcon),ssotf(mcon),ssmlcn(mcon),ssvfc(mcon),
     &          ssother(mcon,3),ssparent(mcon),ssuse(mcon,2)

      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      common/c2/lsnam,nccode(mcom),lproj(mcom),lgeom(mcom),
     &          lshad(mcom),lthrm(mcom),indutl(mcom),lutil(mcom)
      common/c24/izstocn(mcom,ms)
      common/tmc/itmc(ms),nwins
      common/data2/pso(ms,24),psof(ms,24)
      common/data3/ishd(12),isadd(12),ntmc(ms),ioffs(ms)
      common/data4/insst(mgt,24,misur),pinsst(mgt,24,misur)

      character ssname*12,ssotf*32,ssmlcn*32,ssvfc*4,ssother*24,
     &          ssparent*12,ssuse*8
      character sn*12
      character*12 sourcename(MS)   ! name of insolation source
      character*3 month(12)
      character outs*124,exportfile*72
      character outl*248,outld*248
      integer isourcecount  ! to help with sourcename list

      DATA month/'Jan','Feb','Mar','Apr','May','Jun','Jul',
     &           'Aug','Sep','Oct','Nov','Dec'/

C Re-scan contents of the zone shading & insolation db.
      ifilsi=ifil+3
      irec=1
      read(ifilsi,rec=irec,iostat=istat,err=1000)(ishd(i),i=1,12),
     &                                           (isadd(i),i=1,12)

C Identify the initial month with data.
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
   10 continue
      if(ims.eq.0)then
         ier=-3
         return
      endif

C Read db and fill commons DATA2, DATA3, DATA4 and DATA5 for
C specified month.
      call sifrd(icomp,mnth,ier)

C Open the ascii file for export.
      iuj=ifil+10
      call efopseq(iuj,exportfile,3,ier)
      if(ier.lt.0)then
        ier=1
        goto 1002
      endif

      write(iuj,'(2a)',iostat=istat,err=1002)
     &   '* Shading and insolation data in db ',lshad(icomp)
      write(iuj,'(2i3,a)',iostat=istat,err=1002)icomp,nzsur(icomp),
     &   ' # zone index & number of surfaces'
   16 write(iuj,'(3a)',iostat=istat,err=1002) '* month: `',
     &   month(mnth),'`'

C Write direct and diffuse shading factors.
      write(iuj,'(a)',iostat=istat,err=1002) 
     &                      '24 hour external surface shading'
      do 100 i = 1,nzsur(icomp)
         icn=izstocn(icomp,i)
         sn=ssname(icn)
         write(outl,'(12f7.4)') (pso(i,j),j=1,12)
         call sdelim(outl,outld,'S',iw)
         write(iuj,'(a)',iostat=istat,err=1002)
     &                                  outld(1:lnblnk(outld))
         write(outl,'(12f7.4,2a)') (pso(i,j),j=13,24),
     &      ' # direct - surface ',sn(1:lnblnk(sn))
         call sdelim(outl,outld,'S',iw)
         write(iuj,'(a)',iostat=istat,err=1002)
     &                                  outld(1:lnblnk(outld))
         write(outl,'(12f7.4)') (psof(i,j),j=1,12)
         call sdelim(outl,outld,'S',iw)
         write(iuj,'(a)',iostat=istat,err=1002)
     &                                  outld(1:lnblnk(outld))
         write(outl,'(12f7.4,2a)') (psof(i,j),j=13,24),
     &      ' # diffuse - surface ',sn(1:lnblnk(sn))
         call sdelim(outl,outld,'S',iw)
         write(iuj,'(a)',iostat=istat,err=1002)
     &                                  outld(1:lnblnk(outld))
 100  continue
 
C Generate packed list of names of insolation sources.
      isourcecount=0
      do 101 i = 1,nzsur(icomp)
         if(ioffs(i).ne.0)then
           isourcecount=isourcecount+1
           icn=izstocn(icomp,i)
           sourcename(isourcecount)=ssname(icn)
         endif
 101  continue

C Write insolation factors. Note the record offset line is
C sensitive to the number of surfaces in the zone.
      if(ishd(mnth).eq.2.or.ishd(mnth).eq.3)then
         write(iuj,'(a)',iostat=istat,err=1002) 
     &                     '24 hour internal surface insolation'
         write(iuj,'(2a)',iostat=istat,err=1002) 'record offsets',
     &     ' # to each insolation source (marked by non-zero)'
         if(nzsur(icomp).lt.99)then
           write(outl,'(80i3)') (ioffs(i),i=1,nzsur(icomp))
         else
           write(outl,'(62i4)') (ioffs(i),i=1,nzsur(icomp))
         endif
         call sdelim(outl,outld,'S',iw)
         write(iuj,'(a)',iostat=istat,err=1002)
     &                                  outld(1:lnblnk(outld))
         do 110 i=1,nwins
            write(iuj,'(a,i2,3a)',iostat=istat,err=1002)
     &        'surfaces insolated by source ',i,' ',sourcename(i),
     &        ' # hourly columns, ranking via rows'
            do 120 j=1,misur
               write(outl,'(24i3)')(insst(i,k,j),k=1,24)
               call sdelim(outl,outld,'S',iw)
               write(iuj,'(a)',iostat=istat,err=1002)
     &                                  outld(1:lnblnk(outld))
  120       continue
            write(iuj,'(2a)',iostat=istat,err=1002)
     &        'matched surface insolation factors',
     &        ' # hourly columns, ranking via rows'
            do 130 j=1,misur
               write(outl,'(24f6.3)')(pinsst(i,k,j),k=1,24)
               call sdelim(outl,outld,'S',iw)
               write(iuj,'(a)',iostat=istat,err=1002)
     &                                  outld(1:lnblnk(outld))
  130       continue
  110    continue
      endif

C Try incrementing the month, if have just written December then
C write the '* end' close file and return. If there is no data
C for the next month loop again looking for more. If there is data
C for the next month then jump back to the '* month' writing line.
   15 mnth=mnth+1
      if(mnth.gt.12)then
         write(iuj,'(A)',iostat=istat,err=1002) '* end'
         call erpfree(iuj,istat)
         call edisp(iuout,' ')
         call edisp(iuout,'Export file closed.')
         return
      endif
      call sifrd(icomp,mnth,ier)
      if(ier.ne.0)then
         call edisp(iuout,' ')
         write(outs,'(2a)') month(mnth),' has no data!'
         call edisp(iuout,outs)
         goto 15  ! increment mnth to see if there is more data.
      endif
      goto 16  ! go back and write another months data.

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

C ************* SIFIMPORT *************
C SIFIMPORT imports the shading and insolation factors as held in an
C ASCII zone shading & insolation db to bimary file. Uses SIFWRT to
C update db contents. Uses some of the same code blocks as sifops but is
C written for silent operation. If the format changes in sifops then
C sifexp should also be updated.
C Passed parameters:
C  icomp (integer) is the current zone index
C  exportfile (72 char) is the ascii file to import (typically
C    this would be a name derived from the zone shading file).
C  ier (integer) is zero if no problems, -3 if empty,
C     -2 not found, -1 wrong type of file, -4 wrong zone.
      subroutine sifimport(icomp,exportfile,ier)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      common/outin/iuout,iuin
      common/filep/ifil

      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      common/c2/lsnam,nccode(mcom),lproj(mcom),lgeom(mcom),
     &          lshad(mcom),lthrm(mcom),indutl(mcom),lutil(mcom)
      common/tmc/itmc(ms),nwins
      common/data2/pso(ms,24),psof(ms,24)
      common/data3/ishd(12),isadd(12),ntmc(ms),ioffs(ms)
      common/data4/insst(mgt,24,misur),pinsst(mgt,24,misur)
      common/data5/irecx,nsurs,msurs
      common/mtfile/ltrns,multic,mons,monf

      character ltrns*72
      character*3 month(12)
      character outs*124,louts*248,exportfile*72
      character outstr*124,loutstr*248
      character message*64
      integer nboftmc       ! number of insolation sources returned from findtmc
      logical ok

      DATA month/'Jan','Feb','Mar','Apr','May','Jun','Jul',
     &           'Aug','Sep','Oct','Nov','Dec'/

C Preread logic to determine the month and other useful
C data.

C Scan header of the zone shading & insolation db.
      ifilsi=ifil+3
      irec=1
      read(ifilsi,rec=irec,iostat=istat,err=1000)(ishd(i),i=1,12),
     &                                           (isadd(i),i=1,12)
      write(outs,'(2a)')'Content of zone shading & insolation db ',
     &                   lshad(icomp)(1:lnblnk(lshad(icomp)))
      call edisp(iuout,' ')
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
   10 continue
   
C Assume there is nothing in the shading file and the
C import from ascii file is used to fill it.

C An empty shading file so setup some intial information. Find the
C initial month in the import file and then close the file.
      iuj=ifil+10
      mnth=1
      call efopseq(iuj,exportfile,1,ier)
      call stripc(iuj,outstr,0,nd,1,'month',ier)
      if(outstr(1:7).eq.'* month')then
        do 16 im=1,12
          if(outstr(11:13).eq.month(im))mnth=im
 16     continue
        call erpfree(iuj,istat)
      elseif(outstr(1:5).eq.'* end')then
        call erpfree(iuj,istat)
      endif

      if(irecx.eq.0)irecx=5   ! irecx probably was not set so do it for initial month
      isadd(mnth)=irecx       ! so code knows where to write data 
      nsurs=nzsur(icomp)      ! needed for the header
      msurs=misur
      call findtmc(icomp,'s',nboftmc) ! establish insolation sources and copy to ntmc
      do 44 ij=1,nsurs
        ntmc(ij)=itmc(ij)
 44   continue

C Based on the file name being passed to this subroutine.
      call efopseq(iuj,exportfile,1,ier)
      if(ier.ne.0)goto 1002
      call stripc(iuj,outstr,0,nd,1,'header',ier)
      if(ier.ne.0)goto 1002
      if(outstr(1:24).ne.'* Shading and insolation')then
         call usrmsg('Not an ascii zone shading & insolation file!',
     &               exportfile,'W')
         call erpfree(iuj,istat)
         ier=-1
         return
      endif
           
      call stripc(iuj,outstr,0,nd,1,'zone & surf',ier)
      if(ier.ne.0)goto 1002
      k=0
      call egetwi(outstr,k,iret,0,0,'-','zone index',ier)
      if(ier.ne.0)goto 1002
      if(iret.ne.icomp)then
        call usrmsg('These data are for a different zone!',
     &                                     exportfile,'W')
        call erpfree(iuj,istat)
        ier=-4
        return
      endif
      
      call egetwi(outstr,k,iret,0,0,'-','surfaces',ier)
      if(ier.ne.0)goto 1002
      if(iret.ne.nzsur(icomp))then
        call usrmsg(
     &    'Number of surfaces in file does not match current model!',
     &     exportfile,'W')
        call erpfree(iuj,istat)
        ier=-4
        return
      endif

C Read data for a month. If we have a month that matches then
C the code looks for either '24 hour external' in which case
C the data is shading data. If
      multic=2  ! signal data is known
      call stripc(iuj,outstr,0,nd,1,'month',ier)
 172  if(outstr(1:5).eq.'* end')goto 18
      if(ier.ne.0.or.outstr(1:7).ne.'* month')goto 1002
      if(outstr(11:13).eq.month(mnth))then
         call stripc(iuj,outstr,0,nd,1,'24 hour external',ier)
         if(outstr(1:16).ne.'24 hour external')then
           if(outstr(1:16).ne.'24 hour internal')then
             ishd(mnth)=2   ! no shading only insolation
             goto 173
           endif
           message='expecting 24 hours external'
           multic=0  ! signal data is not known
           goto 1002
         endif
         ishd(mnth)=1   ! at least shading
         do 150 i=1,nzsur(icomp)
            message='reading direct shading'
            call lstripc(iuj,loutstr,0,nd,1,'dir shading 01-12',ier)
            if(ier.ne.0)goto 1004
            k=0
            do 160 j=1,12
               call egetwr(loutstr,k,ret,0.,1.,'W',
     &                                     'dir shading 01-12',ier)
               if(ier.ne.0)goto 1004
               pso(i,j)=ret
  160       continue
            call lstripc(iuj,loutstr,0,nd,1,'dir shading 12-24',ier)
            if(ier.ne.0)goto 1004
            k=0
            do 170 j=13,24
               call egetwr(loutstr,k,ret,0.,1.,'W',
     &                                     'dir shading 12-24',ier)
               if(ier.ne.0)goto 1004
               pso(i,j)=ret
  170       continue
       
            message='reading diffuse shading'
            call lstripc(iuj,loutstr,0,nd,1,'dif shading 01-12',ier)
            if(ier.ne.0)goto 1004
            k=0
            do 180 j=1,12
               call egetwr(loutstr,k,ret,0.,1.,'W',
     &                                     'dif shading 01-12',ier)
               if(ier.ne.0)goto 1004
               psof(i,j)=ret
  180       continue
            call lstripc(iuj,loutstr,0,nd,1,'dif shading 12-24',ier)
            if(ier.ne.0)goto 1004
            k=0
            do 190 j=13,24
               call egetwr(loutstr,k,ret,0.,1.,'W',
     &                                     'dif shading 12-24',ier)
               if(ier.ne.0)goto 1004
               psof(i,j)=ret
  190       continue
  150    continue
      else
         if(mnth.gt.12) goto 18  ! have reached past december
         call usrmsg('Data in file is not for the specified month!',
     &                               exportfile,'W')
         call erpfree(iuj,istat)
         ier=-3
         return
      endif

C Read insolation factors. But the next line might also hold
C a Month string or end marker.
      call stripc(iuj,outstr,0,nd,1,'insolation header',ier)
 173  if(ier.ne.0.or.outstr(1:16).ne.'24 hour internal')then
        if(outstr(1:7).eq.'* month')then
          ok=.true.
          if(ok)then
            call sifwrt(icomp,mnth,lastrec)
            if(lastrec.gt.5)then
              if(isadd(mnth+1).eq.0) isadd(mnth+1)=lastrec
            endif
          endif
          mnth=mnth+1
          goto 172
        elseif(outstr(1:5).eq.'* end')then
          goto 18   ! end of file reached
        endif
        message='expecting 24 hours internal'
        multic=0  ! signal data is not known
        goto 1002
      endif
      if(outstr(1:5).eq.'* end')goto 18
      call stripc(iuj,outstr,0,nd,1,'offsets header',ier)
      if(ier.ne.0.or.outstr(1:14).ne.'record offsets')then
        message='expecting record offsets'
        goto 1002
      endif
      if(ishd(mnth).eq.1) ishd(mnth)=3   ! set to shading + insul
 
C Read the offsets line. Items with a zero are not insolation
C sources, non-zero entries are offsets to use within the binary
C file and the position in the list indicates with surface in
C the zone is an insolation source.
      call lstripc(iuj,loutstr,0,nd,1,'offsets data',ier)
      if(ier.ne.0)goto 1004
      k=0
      do 200 i=1,nzsur(icomp)
         call egetwi(loutstr,k,iret,0,0,'-','offset',ier)
         if(ier.ne.0)goto 1004
         ioffs(i)=iret
  200 continue

C Reset the counter for insolation sources. Use the nwins
C value for the first index of insst
      nwins=0
      do 210 i=1,nzsur(icomp)
         if(ioffs(i).ne.0)then
            nwins=nwins+1
            call stripc(iuj,outstr,0,nd,1,'insst surf header',ier)
            if(ier.ne.0.or.outstr(1:21).ne.
     &        'surfaces insolated by')then
               message='expecting surfaces insolated by'
              goto 1002
            endif
            do 220 j=1,misur
               call stripc(iuj,outstr,0,nd,1,'insst data',ier)
               if(ier.ne.0)goto 1002
               kk=0
               do 230 k=1,24
                  call egetwi(outstr,kk,iret,-1,ms,'-','insst',ier)
                  if(ier.ne.0)then
                    message='while reading insst data'
                    goto 1002
                  endif
                  insst(nwins,k,j)=iret
  230          continue
  220       continue
       
            call stripc(iuj,outstr,0,nd,1,'pinsst header',ier)
            if(ier.ne.0)goto 1002

            if(outstr(1:26).ne.'matched surface insolation')then
              message='expecting matched surface insolation'
              goto 1002
            endif
            do 240 j=1,misur
               call lstripc(iuj,loutstr,0,nd,1,'pinsst line',ier)
               if(ier.ne.0)goto 1004
               kk=0
               do 250 k=1,24
                  call egetwr(loutstr,kk,ret,0.,1.,'W','pinsst',ier)
                  if(ier.ne.0)then
                    message='while reading pinsst data'
                    goto 1004
                  endif
                  pinsst(nwins,k,j)=ret
  250          continue
  240       continue
         endif
  210 continue

      call stripc(iuj,outstr,0,nd,1,'* end',ier)
      if(ier.ne.0)goto 1002
      if(outstr(1:5).ne.'* end')then
        if(outstr(1:7).eq.'* month')then
          ok=.true.
          if(ok)then
            call sifwrt(icomp,mnth,lastrec)
            if(lastrec.gt.5)then
              if(isadd(mnth+1).eq.0) isadd(mnth+1)=lastrec
            endif
          endif
          mnth=mnth+1
          goto 172
        endif
        message='Expecting * end'
        goto 1002
      endif

C End marker of the file reached. Close it and update isadd
C if not december.
  18  call erpfree(iuj,istat)
      call edisp(iuout,' ')
      call edisp(iuout,'Import file closed.')
      ok=.true.
      if(ok)then
        call sifwrt(icomp,mnth,lastrec)
        if(lastrec.gt.5)then
          if(mnth.le.11)then
            if(isadd(mnth+1).eq.0)isadd(mnth+1)=lastrec
          endif
        endif
      endif
      
      return

C Error handling.
 1000 write(outs,1001)
 1001 format('SIFIMPORT: Zone Shading & Insolation text file error!')
      call edisp(iuout,' ')
      call edisp(iuout,outs)
      call edisp(iuout,outstr)
      return

C Error written out in several lines with message identifying the
C type of data involved.
 1002 write(louts,'(3a)') 'sifimport: Zone Shading & Insolation ',
     &   message(1:lnblnk(message)),' in the line '
      call edisp248(iuout,louts,90)
      call edisp(iuout,outstr)
      return

C Error written out in several lines with message identifying the
C type of data involved.
 1004 write(louts,'(3a)') 'sifimport: Zone Shading & Insolation ',
     &   message(1:lnblnk(message)),' in the line '
      call edisp248(iuout,louts,90)
      call edisp248(iuout,loutstr,90)
      return
      
      end


