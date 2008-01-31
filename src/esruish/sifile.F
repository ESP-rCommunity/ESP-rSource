<<<<<<< .working
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

C SIFILE.F contains the following routines relating to the 
C management of a zone shading & insolation db at simulation
C time.

C SIFILE: Opens the zone shading & insolation db.
C SSAVE:  Writes shading (direct and diffuse) data to the zone
C         shading/insolation db.
C ISAVE:  Writes insolation data to the zone shading/insolation db.

C Format of a zone shading & insolation db:
C Minimum width is 24 words, normal width is nzsur(icomp) words.
C Record 1: first 12 words holds ishd(), the analysis type for
C           each day-month (0 no data, 1 shading only, 2 insolation
C           only, 3 shading and insolation), followed by isadd(),
C           the record where the shading and/or insolation data for
C           each day-month commences.
C Record 2: first word is irecx (the last record written to + 1, i.e.
C           the next record available for new data), second is nsurs
C           (the number of surfaces in the zone at the time the db
C           was created), third is msurs (the maximum number of
C           insolated surfaces defined when the db was created). The
C           second and third items are used as consistency checks.
C Record 3: reserved for future use.
C Record 4: itmc(), if 1, indicating that a surface is a TMC, i.e.
C           and insolation source.
C Record 5: beginning of shading and/or insolation data for first
C           month considered.
C Shading records:
C   24 hour values of direct, pso(), and diffuse, psof(), shading
C   factors are written in record pairs occupying 2*nsur records.
C Insolation records:
C   These start at record isadd(mon)+2*nsur. The first record of
C   the block holds the ioffs array, which gives the record offset
C   to the start of the data for each considered insolation source.
C   These data correspond to each of the possible misur insolation
C   sources and comprise 24 hour values of the insolated surface
C   index, insst(), and the insolation factor, pinwst(), written
C   in record pairs. Thus, igc() for a 6 surface zone might, for
C   example, be 0,1,x,0,y,0 (where x and y have specific integer
C   values) indicating that surfaces 2, 3 and 5 are insolation
C   sources, that the data for surface 3 commences at the next
C   record (i.e. with an offset of 1), the data for surface 3
C   commences with an offset of x=2*misur and the data for surface 5
C   commences with an offset of y=2*(2*misur).

C ************** SIFILE **************
C Opens a zone shading & insolation db.

      subroutine sifile(icomp)
#include "building.h"

      common/outin/iuout,iuin
      common/filep/ifil
      
C insrts - if 0 then shading data has not yet been saved;
C is set to 1 after data is written and reset to 0 in shdcon
C when the user changes the shading directive.
C insrti - if 0 then insolation data has not yet been saved;
C is set to 1 after data has been written and reset to 0 in
C inscon when the user changes the insolation directive.
      common/sif1/insrts,insrti
      common/c2/lsnam,nccode(mcom),lproj(mcom),lgeom(mcom),
     &          lshad(mcom),lthrm(mcom),indutl(mcom),lutil(mcom)
      common/c20/nzsur(mcom),nztv(mcom)
      common/indics/ivf(mcom),isi(mcom),ihc(mcom),
     &              itw(mcom),icgc(mcom),iobs(mcom)
      common/ract/paction

      dimension isadd(12),ishd(12)
      character*72 lsnam,lproj,lgeom,lshad,lthrm,lutil,fname
      character outs*124,paction*16
      logical xst

C Close any file already open then assign a new file.
      ifilsi=ifil+3
      call erpfree(ifilsi,istat)
      if(isi(icomp).eq.1)then
        fname=lshad(icomp)
      else
        call usrmsg('No zone shading/insolation db name found in the',
     &              'model configuration file; please define.','W')
        return
      endif
      maxrec=24
      if(nzsur(icomp).gt.24)maxrec=nzsur(icomp)
      call findfil(fname,xst)
      if(xst)then
        call efopran(ifilsi,fname,maxrec,1,ier)
        if(ier.ne.0)then
          call edisp(iuout,
     &           'Error opening the Zone Shading & Insolation db!')
          return
        endif

C Check file has been previously written to. If a problem occurs then
C try again at maxrec=24 and then at ms words wide.
        irec=2
        read(ifilsi,rec=irec,iostat=istat,err=1000)inext,ns
        if(inext.le.3.or.ns.ne.nzsur(icomp))then
          call erpfree(ifilsi,istat)
          maxrec=24
          call efopran(ifilsi,fname,maxrec,1,ier)
          if(ier.ne.0)then
            call edisp(iuout,
     &       'Error opening Zone Shading & Insolation db at 24 words!')
            return
          endif
          read(ifilsi,rec=irec,iostat=istat,err=1000)inext,ns
          if(inext.le.3.or.ns.ne.nzsur(icomp))then
            call erpfree(ifilsi,istat)
            maxrec=ms
            call efopran(ifilsi,fname,maxrec,1,ier)
            if(ier.ne.0)then
              call edisp(iuout,
     &       'Error opening Zone Shading & Insolation db at MS words!')
              return
            endif
            read(ifilsi,rec=irec,iostat=istat,err=1000)inext,ns
            if(inext.le.3.or.ns.ne.nzsur(icomp))goto 1003
          endif
        endif
      else

C If new file, fill first record with zeros so that it will be
C interpreted later as empty.
        call efopran(ifilsi,fname,maxrec,3,ier)
        irec=1
        do 10 i=1,12
          ishd(i)=0
          isadd(i)=0
  10    continue
        write(ifilsi,rec=irec,iostat=istat,err=1001)(ishd(i),i=1,12),
     &                                              (isadd(i),i=1,12)
      endif

C Return after advising user and setting insrts & instri to 0 to
C indicate to shadcon and inscon that no data have yet been saved.
      if(paction(1:13).ne.'update_silent')then
        call edisp(iuout,' ')
        write(outs,'(3a)')'Opened ',fname,'.'
        call edisp(iuout,outs)
      endif
      insrts=0
      insrti=0
      return

 1000 call edisp(iuout,' ')
      call edisp(iuout,'SIFILE: Zone Shading & Insolation read error!')
      return
 1001 call edisp(iuout,' ')
      call edisp(iuout,'SIFILE: Zone Shading & Insolation write error!')
      return
 1003 call edisp(iuout,' ')
      call edisp(iuout,
     &   'Zone Shading & Insolation db mismatch with current model.')
      write(outs,'(a,i2,a,i2,a,i2)') 'Model NSUR = ',nzsur(icomp),
     &   ' db NSUR = ',ns,' db last record = ',inext
      call edisp(iuout,outs)
      return
      end

C ************** SSAVE **************
C Writes the 24 hour direct and diffuse shading factors to the
C zone shading & insolation db for all zone surfaces regardless
C of their selection for shading calculation. Is called from shdcon
C once per day-month.

      subroutine ssave(ier)
#include "building.h"

      common/outin/iuout,iuin
      common/filep/ifil
      common/data1/ical
      common/g1/x(mtv),y(mtv),z(mtv),nsur,jvn(ms,mv),nver(ms),ntv
      common/contr/mon,isc(ms),iyd
      common/pophelp/h(60)
      common/sif1/insrts,insrti
      common/data2/pso(ms,24),psof(ms,24)

      dimension isadd(12),ishd(12)
      character outs*124,h*72
      logical ok,dok

C SI db unit number.
      ifilsi=ifil+3
      ier=0

C Read zone shading & insolation db. If at least one matched
C pair of the ishd() and isadd() arrays is not zero then there
C is data in the file.
      irec=1
      read(ifilsi,rec=irec,iostat=istat,err=1000)(ishd(i),i=1,12),
     &                                           (isadd(i),i=1,12)
      do 10 i=1,12
        if(ishd(i).ne.0.and.isadd(i).ne.0)goto 1
   10 continue

C db has no data.
      irecx=5
      isadd(mon)=irecx
      goto 2

C db has data.
C isadd(): record address corresponding to the start of each month.
C ishd():  0 - nothing saved for this month;
C          1 - shading data only;
C          2 - insolation data only;
C          3 - shading and insolation data.
C irecx:   next available record in db.
C nsurs:   number of zone surfaces in model when db first created.
C msurs:   maximum number of internal insolated surfaces in model
C          when db first created.
    1 if(isadd(mon).eq.0)isadd(mon)=irecx

C Consistency check.
      irec=2
      read(ifilsi,rec=irec,iostat=istat,err=1000)irecx,nsurs,msurs
      if(nsur.ne.nsurs.or.msurs.ne.misur)goto 1002

C Confirm that there are shading data to be saved.
    2 if(ical.eq.0)then
         call edisp(iuout,' ')
         call edisp(iuout,'No shading data to be saved!')
         return
      endif

C Check that data have not been saved already.
      if(insrts.ne.0)then
         call edisp(iuout,' ')
         call edisp(iuout,'Shading data have already been saved!')
         return
      endif

C Update saved data index.
      ishdt=ishd(mon)
      if(ishdt.eq.0.or.ishdt.eq.2)then
         ishd(mon)=ishd(mon)+1
         goto 3
      endif

C Shading data already exists for this month.
      h(1)='This month has previously been processed. You may wish'
      h(2)='to overwrite the shading data with the new calculation'
      h(3)='results. Note however that it is possibly safer to replace'
      h(4)='the Zone Shading & Insolation db with a new one.'
      write(outs,'(a,i2,a)') 'Shading data already saved for month ',
     &                        mon,'.' 
      dok=.true.
      call askok(outs,'OK to overwrite?',ok,dok,4)
      if(.not.ok)return

C For each zone surface (whether included in the shading calculation
C or not), write out 24 hours of direct then diffuse shading data.
    3 irec=isadd(mon)
      do 20 i=1,nsur
        write(ifilsi,rec=irec,iostat=istat,err=1000)(pso(i,j),j=1,24)
        irec=irec+1
        write(ifilsi,rec=irec,iostat=istat,err=1000)(psof(i,j),j=1,24)
        irec=irec+1
   20 continue

C Re-write header records to record changes.
      irec=1
      write(ifilsi,rec=irec,iostat=istat,err=1000)(ishd(i),i=1,12),
     &                                            (isadd(i),i=1,12)
      irec=2
      write(ifilsi,rec=irec,iostat=istat,err=1000)irecx,nsur,misur
      insrts=1

C Return to menu.
      return

C Error handling.
 1000 write(outs,1001)irec
 1001 format('SSAVE: Zone Shading & Insolation db error, record',i5)
      call edisp(iuout,' ')
      call edisp(iuout,outs)
      ier=1
      return
 1002 call edisp(iuout,'The geometry referenced in the Zone Shading')
      call edisp(iuout,'& Insolation db does not correspond to that')
      call edisp(iuout,'in the specified Zone Geometry File! Try')
      call edisp(iuout,'another database.')
      ier=1
      return
      end

C ************** ISAVE **************
C Writes the 24 hour insolation data for all misur possible
C internal insolated surfaces to the zone shading & insolation
C db for each defined insolation source. Is called by inscon
C once per day-month.

      subroutine isave(ier)
#include "building.h"

      common/outin/iuout,iuin
      common/data1/ical
      common/g1/x(mtv),y(mtv),z(mtv),nsur,jvn(ms,mv),nver(ms),ntv
      common/contr/mon,isc(ms),iyd
      common/pophelp/h(60)
      common/filep/ifil
      common/tmc/itmc(ms),nwins
      common/stins/insst(mgt,24,misur),pinsst(mgt,24,misur),
     &             shadst(mgt,24)
      common/sif1/insrts,insrti
      common/ract/paction

      dimension pso(ms,24),psof(ms,24),insstx(24),pinsstx(24)
      character outs*124,paction*16,h*72
      logical ok,dok,close

      dimension isadd(12),ishd(12),ioffs(ms),ntmc(ms)
      
C SI db unit number.
      ifilsi=ifil+3
      ier=0

      irec=1
      read(ifilsi,rec=irec,iostat=istat,err=2)(ishd(i),i=1,12),
     &                                        (isadd(i),i=1,12)

C Check for contents.
      multi=0
      do 10 i=1,12
        if(ishd(i).ne.0)multi=1
        if(ishd(i).ne.0.or.isadd(i).ne.0)goto 1
   10 continue
   
C db has no data.
      irecx=5
      isadd(mon)=irecx
      goto 2

C db has data.
C isadd: record address corresponding to the start of each month.
C ishd:  0 - nothing saved for this month;
C        1 - shading only;
C        2 - insolation only;
C        3 - shading and insolation.
C irecx: next available record in db.
C nsurs: number of zone surfaces in model when db first created.
C msurs: maximum number of internal insolated surfaces in model
C        when db first created.
C ntmc:  0 - surface is not an insolation source;
C        1 - surface is an insolation source.
    1 irec=2
      read(ifilsi,rec=irec,iostat=istat,err=1000)irecx,nsurs,msurs
C Consistency check.
      if(nsur.ne.nsurs.or.msurs.ne.misur)goto 1002
      if(isadd(mon).eq.0)isadd(mon)=irecx

C Confirm that there are insolation data is to be saved.
    2 if(ical.eq.0)then
         call edisp(iuout,' ')
         call edisp(iuout,'No insolation data to be saved!')
         return
      endif

C Check that data have not been saved already.
      if(insrti.ne.0)then
         call edisp(iuout,' ')
         call edisp(iuout,'Insolation data have already been saved!')
         return
      endif

C Establish if the insolation data to be written will result in an
C expansion of the number of records required (new = 0 for no, 1
C for yes).
      new=0
      irec=4
      read(ifilsi,rec=irec,iostat=istat,err=1000)(ntmc(i),i=1,nsur)
      if(paction(1:13).ne.'update_silent')call edisp(iuout,' ')
      do 20 i=1,nsur
         if(itmc(i).ne.0)then
            if(ntmc(i).eq.0)then
               if(multi.eq.1)new=1
               if(paction(1:13).ne.'update_silent')then
                  write(outs,'(a,i2)')
     &               'Adding db entry for insolation source surface ',i
                  call edisp(iuout,outs)
               endif
            else
               if(paction(1:13).ne.'update_silent')then
                  write(outs,'(a,i2)')
     &            'Replacing db entry for insolation source surface ',i
                  call edisp(iuout,outs)
               endif
            endif
         endif
   20 continue

C Possibly update saved data index.
      ishdt=ishd(mon)
      if(ishdt.eq.0.or.ishdt.eq.1)then
         ishd(mon)=ishd(mon)+2
         if(multi.eq.1)new=1
      elseif(ishdt.eq.2.or.ishdt.eq.3)then
         dok=.true.
         h(1)='This month has previously been processed. You may'
         h(2)='wish to update the insolation data with the new'
         h(3)='calculation results. Where this will result in'
         h(4)='the replacement of existing entries you may wish'
         h(5)='to replace the current Zone Shading & Insolation db'
         h(6)='with a new one.'
         write(outs,'(a,i2,a)')
     &       'Insolation data already saved for month ',mon,'.' 
         call askok(outs,'OK to overwrite/append?',ok,dok,6)
         if(.not.ok)return
      endif

C Write data for insolation sources, moving entry for month if the
C data-set has expanded (i.e. new = 1).
      ireco=isadd(mon)
      if(new.eq.1)then
         isadd(mon)=irecx
         irecn=isadd(mon)
      endif
      iof=1
      do 30 i=1,nsur
        if(new.eq.1)then
           read(ifilsi,rec=ireco,iostat=istat,err=1000)
     &                                        (pso(i,j),j=1,24)
           ireco=ireco+1
           write(ifilsi,rec=irecn,iostat=istat,err=1000)
     &                                        (pso(i,j),j=1,24)
           irecn=irecn+1
           read(ifilsi,rec=ireco,iostat=istat,err=1000)
     &                                        (psof(i,j),j=1,24)
           ireco=ireco+1
           write(ifilsi,rec=irecn,iostat=istat,err=1000)
     &                                        (psof(i,j),j=1,24)
           irecn=irecn+1
        else
           ireco=ireco+2
        endif
        ioffs(i)=0
        if(itmc(i).ne.0.or.ntmc(i).ne.0)then

C Current surface is an insolation source - save record 
C offset (ioffs) and then increment by 2 times the maximum 
C number of possible internal insolated surfaces (misur).
           ioffs(i)=iof
           iof=iof+(2*misur)
        endif
   30 continue
      if(new.eq.1)then
         write(ifilsi,rec=irecn,iostat=istat,err=1000)
     &                                        (ioffs(i),i=1,nsur)
         irecn=irecn+1
      else
         write(ifilsi,rec=ireco,iostat=istat,err=1000)
     &                                        (ioffs(i),i=1,nsur)
         ireco=ireco+1
      endif

C Move/write hourly insolation index number for each of the misur
C surfaces that can potentially be insolated.
      isrc=0
      do 40 i=1,nsur
         if(ioffs(i).eq.0)goto 40
         if(itmc(i).ne.0)isrc=isrc+1
         do 50 j=1,misur

C Reset index number to 0 if corresponding proportion is 0.
            do 501 k=1,24
               call eclose(pinsst(isrc,k,j),0.0,0.001,close)
               if((close).and.insst(isrc,k,j).gt.0)insst(isrc,k,j)=0
  501       continue
            if(new.eq.1)then

C Retain previous data if not to be overwritten.
               if(itmc(i).eq.0)then
                  read(ifilsi,rec=ireco,iostat=istat,err=1000)
     &                                        (insstx(k),k=1,24)
                  ireco=ireco+1
                  write(ifilsi,rec=irecn,iostat=istat,err=1000)
     &                                        (insstx(k),k=1,24)
                  irecn=irecn+1
                else
                  write(ifilsi,rec=irecn,iostat=istat,err=1000)
     &                                        (insst(isrc,k,j),k=1,24)
                  irecn=irecn+1
                endif
            else
               write(ifilsi,rec=ireco,iostat=istat,err=1000)
     &                                        (insst(isrc,k,j),k=1,24)
               ireco=ireco+1
            endif
   50    continue

C Move/write hourly insolation factor for each of the misur
C surfaces that can potentially be insolated.
         do 60 j=1,misur
            if(new.eq.1)then

C Retain previous data if not to be overwritten.
               if(itmc(i).eq.0)then
                  read(ifilsi,rec=ireco,iostat=istat,err=1000)
     &                                        (pinsstx(k),k=1,24)
                  ireco=ireco+1
                  write(ifilsi,rec=irecn,iostat=istat,err=1000)
     &                                        (pinsstx(k),k=1,24)
                  irecn=irecn+1
                else
                  write(ifilsi,rec=irecn,iostat=istat,err=1000)
     &                                        (pinsst(isrc,k,j),k=1,24)
                  irecn=irecn+1
                endif
            else
               write(ifilsi,rec=ireco,iostat=istat,err=1000)
     &                                        (pinsst(isrc,k,j),k=1,24)
               ireco=ireco+1
            endif
   60    continue
   40 continue
      if(new.eq.1)then
         irecx=irecn
      else
         irecx=ireco
      endif

C Write header items to capture potential changes.
      irec=1
      write(ifilsi,rec=irec,iostat=istat,err=1000)(ishd(i),i=1,12),
     &                                            (isadd(i),i=1,12)
      irec=2
      write(ifilsi,rec=irec,iostat=istat,err=1000)irecx,nsur,misur
      irec=4
      write(ifilsi,rec=irec,iostat=istat,err=1000)(itmc(i),i=1,nsur)
      insrti=1
      return

C Error handling.
 1000 write(outs,1001)irec
 1001 format('ISAVE: Zone Shading & Insolation db error, record',I6)
      call edisp(iuout,' ')
      call edisp(iuout,outs)
      ier=1
      return
 1002 call edisp(iuout,' ')
      call edisp(iuout,'Geometry saved in Zone Shading & Insolation db')
      call edisp(iuout,'does not correspond to that contained in the')
      call edisp(iuout,'current zone geometry file.')
      ier=1
      return
      end
=======
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

C SIFILE.F contains the following routines relating to the 
C management of a zone shading & insolation db at simulation
C time.

C SIFILE: Opens the zone shading & insolation db.
C SSAVE:  Writes shading (direct and diffuse) data to the zone
C         shading/insolation db.
C ISAVE:  Writes insolation data to the zone shading/insolation db.

C Format of a zone shading & insolation db:
C Minimum width is 24 words, normal width is nzsur(icomp) words.
C Record 1: first 12 words holds ishd(), the analysis type for
C           each day-month (0 no data, 1 shading only, 2 insolation
C           only, 3 shading and insolation), followed by isadd(),
C           the record where the shading and/or insolation data for
C           each day-month commences.
C Record 2: first word is irecx (the last record written to + 1, i.e.
C           the next record available for new data), second is nsurs
C           (the number of surfaces in the zone at the time the db
C           was created), third is msurs (the maximum number of
C           insolated surfaces defined when the db was created). The
C           second and third items are used as consistency checks.
C Record 3: reserved for future use.
C Record 4: itmc(), if 1, indicating that a surface is a TMC, i.e.
C           and insolation source.
C Record 5: beginning of shading and/or insolation data for first
C           month considered.
C Shading records:
C   24 hour values of direct, pso(), and diffuse, psof(), shading
C   factors are written in record pairs occupying 2*nsur records.
C Insolation records:
C   These start at record isadd(mon)+2*nsur. The first record of
C   the block holds the ioffs array, which gives the record offset
C   to the start of the data for each considered insolation source.
C   These data correspond to each of the possible misur insolation
C   sources and comprise 24 hour values of the insolated surface
C   index, insst(), and the insolation factor, pinwst(), written
C   in record pairs. Thus, igc() for a 6 surface zone might, for
C   example, be 0,1,x,0,y,0 (where x and y have specific integer
C   values) indicating that surfaces 2, 3 and 5 are insolation
C   sources, that the data for surface 3 commences at the next
C   record (i.e. with an offset of 1), the data for surface 3
C   commences with an offset of x=2*misur and the data for surface 5
C   commences with an offset of y=2*(2*misur).

C ************** SIFILE **************
C Opens a zone shading & insolation db.

      subroutine sifile(icomp)
#include "building.h"

      common/outin/iuout,iuin
      common/filep/ifil
      
C insrts - if 0 then shading data has not yet been saved;
C is set to 1 after data is written and reset to 0 in shdcon
C when the user changes the shading directive.
C insrti - if 0 then insolation data has not yet been saved;
C is set to 1 after data has been written and reset to 0 in
C inscon when the user changes the insolation directive.
      common/sif1/insrts,insrti
      common/c2/lsnam,nccode(mcom),lproj(mcom),lgeom(mcom),
     &          lshad(mcom),lthrm(mcom),indutl(mcom),lutil(mcom)
      common/c20/nzsur(mcom),nztv(mcom)
      common/indics/ivf(mcom),isi(mcom),ihc(mcom),
     &              itw(mcom),icgc(mcom),iobs(mcom)
      common/ract/paction

      dimension isadd(12),ishd(12),idum(ms)
      character*72 lsnam,lproj,lgeom,lshad,lthrm,lutil,fname
      character outs*124,paction*16
      logical xst

C Close any file already open then assign a new file.
      ifilsi=ifil+3
      call erpfree(ifilsi,istat)
      if(isi(icomp).eq.1)then
        fname=lshad(icomp)
      else
        call usrmsg('No zone shading/insolation db name found in the',
     &              'model configuration file; please define.','W')
        return
      endif
      maxrec=24
      if(nzsur(icomp).gt.24)maxrec=nzsur(icomp)
      call findfil(fname,xst)
      if(xst)then
        call efopran(ifilsi,fname,maxrec,1,ier)
        if(ier.ne.0)then
          call edisp(iuout,
     &           'Error opening the Zone Shading & Insolation db!')
          return
        endif

C Check file has been previously written to. If a problem occurs then
C try again at maxrec=24 and then at ms words wide.
        irec=2
        read(ifilsi,rec=irec,iostat=istat,err=1000)inext,ns
        if(inext.le.3.or.ns.ne.nzsur(icomp))then
          call erpfree(ifilsi,istat)
          maxrec=24
          call efopran(ifilsi,fname,maxrec,1,ier)
          if(ier.ne.0)then
            call edisp(iuout,
     &       'Error opening Zone Shading & Insolation db at 24 words!')
            return
          endif
          read(ifilsi,rec=irec,iostat=istat,err=1000)inext,ns
          if(inext.le.3.or.ns.ne.nzsur(icomp))then
            call erpfree(ifilsi,istat)
            maxrec=ms
            call efopran(ifilsi,fname,maxrec,1,ier)
            if(ier.ne.0)then
              call edisp(iuout,
     &       'Error opening Zone Shading & Insolation db at MS words!')
              return
            endif
            read(ifilsi,rec=irec,iostat=istat,err=1000)inext,ns
            if(inext.le.3.or.ns.ne.nzsur(icomp))goto 1003
          endif
        endif
      else

C If new file, fill first record with zeros so that it will be
C interpreted later as empty.
        call efopran(ifilsi,fname,maxrec,3,ier)
        do 10 i=1,12
          if(i.gt.12)then
             idum(i)=0
          else
             idum(i)=0
             ishd(i)=0
             isadd(i)=0
          endif
  10    continue
        irec=1
        write(ifilsi,rec=irec,iostat=istat,err=1001)(ishd(i),i=1,12),
     &                                              (isadd(i),i=1,12)
        irec=4
        write(ifilsi,rec=irec,iostat=istat,err=1001)
     &                                     (idum(i),i=1,nzsur(icomp))
      endif

C Return after advising user and setting insrts & instri to 0 to
C indicate to shadcon and inscon that no data have yet been saved.
      if(paction(1:13).ne.'update_silent')then
        call edisp(iuout,' ')
        write(outs,'(3a)')'Opened ',fname,'.'
        call edisp(iuout,outs)
      endif
      insrts=0
      insrti=0
      return

 1000 call edisp(iuout,' ')
      call edisp(iuout,'SIFILE: Zone Shading & Insolation read error!')
      return
 1001 call edisp(iuout,' ')
      call edisp(iuout,'SIFILE: Zone Shading & Insolation write error!')
      return
 1003 call edisp(iuout,' ')
      call edisp(iuout,
     &   'Zone Shading & Insolation db mismatch with current model.')
      write(outs,'(a,i2,a,i2,a,i2)') 'Model NSUR = ',nzsur(icomp),
     &   ' db NSUR = ',ns,' db last record = ',inext
      call edisp(iuout,outs)
      return
      end

C ************** SSAVE **************
C Writes the 24 hour direct and diffuse shading factors to the
C zone shading & insolation db for all zone surfaces regardless
C of their selection for shading calculation. Is called from shdcon
C once per day-month.

      subroutine ssave(ier)
#include "building.h"

      common/outin/iuout,iuin
      common/filep/ifil
      common/data1/ical
      common/g1/x(mtv),y(mtv),z(mtv),nsur,jvn(ms,mv),nver(ms),ntv
      common/contr/mon,isc(ms),iyd
      common/pophelp/h(60)
      common/sif1/insrts,insrti
      common/data2/pso(ms,24),psof(ms,24)

      dimension isadd(12),ishd(12)
      character outs*124,h*72
      logical ok,dok

C SI db unit number.
      ifilsi=ifil+3
      ier=0

C Read zone shading & insolation db. If at least one matched
C pair of the ishd() and isadd() arrays is not zero then there
C is data in the file.
      irec=1
      read(ifilsi,rec=irec,iostat=istat,err=1000)(ishd(i),i=1,12),
     &                                           (isadd(i),i=1,12)
      do 10 i=1,12
        if(ishd(i).ne.0.and.isadd(i).ne.0)goto 1
   10 continue

C db has no data.
      irecx=5
      isadd(mon)=irecx
      goto 2

C db has data.
C isadd(): record address corresponding to the start of each month.
C ishd():  0 - nothing saved for this month;
C          1 - shading data only;
C          2 - insolation data only;
C          3 - shading and insolation data.
C irecx:   next available record in db.
C nsurs:   number of zone surfaces in model when db first created.
C msurs:   maximum number of internal insolated surfaces in model
C          when db first created.
    1 if(isadd(mon).eq.0)isadd(mon)=irecx

C Consistency check.
      irec=2
      read(ifilsi,rec=irec,iostat=istat,err=1000)irecx,nsurs,msurs
      if(nsur.ne.nsurs.or.msurs.ne.misur)goto 1002

C Confirm that there are shading data to be saved.
    2 if(ical.eq.0)then
         call edisp(iuout,' ')
         call edisp(iuout,'No shading data to be saved!')
         return
      endif

C Check that data have not been saved already.
      if(insrts.ne.0)then
         call edisp(iuout,' ')
         call edisp(iuout,'Shading data have already been saved!')
         return
      endif

C Update saved data index.
      ishdt=ishd(mon)
      if(ishdt.eq.0.or.ishdt.eq.2)then
         ishd(mon)=ishd(mon)+1
         goto 3
      endif

C Shading data already exists for this month.
      h(1)='This month has previously been processed. You may wish'
      h(2)='to overwrite the shading data with the new calculation'
      h(3)='results. Note however that it is possibly safer to replace'
      h(4)='the Zone Shading & Insolation db with a new one.'
      write(outs,'(a,i2,a)') 'Shading data already saved for month ',
     &                        mon,'.' 
      dok=.true.
      call askok(outs,'OK to overwrite?',ok,dok,4)
      if(.not.ok)return

C For each zone surface (whether included in the shading calculation
C or not), write out 24 hours of direct then diffuse shading data.
    3 irec=isadd(mon)
      do 20 i=1,nsur
        write(ifilsi,rec=irec,iostat=istat,err=1000)(pso(i,j),j=1,24)
        irec=irec+1
        write(ifilsi,rec=irec,iostat=istat,err=1000)(psof(i,j),j=1,24)
        irec=irec+1
   20 continue

C Re-write header records to record changes.
      irec=1
      write(ifilsi,rec=irec,iostat=istat,err=1000)(ishd(i),i=1,12),
     &                                            (isadd(i),i=1,12)
      irec=2
      write(ifilsi,rec=irec,iostat=istat,err=1000)irecx,nsur,misur
      insrts=1

C Return to menu.
      return

C Error handling.
 1000 write(outs,1001)irec
 1001 format('SSAVE: Zone Shading & Insolation db error, record',i5)
      call edisp(iuout,' ')
      call edisp(iuout,outs)
      ier=1
      return
 1002 call edisp(iuout,'The geometry referenced in the Zone Shading')
      call edisp(iuout,'& Insolation db does not correspond to that')
      call edisp(iuout,'in the specified Zone Geometry File! Try')
      call edisp(iuout,'another database.')
      ier=1
      return
      end

C ************** ISAVE **************
C Writes the 24 hour insolation data for all misur possible
C internal insolated surfaces to the zone shading & insolation
C db for each defined insolation source. Is called by inscon
C once per day-month.

      subroutine isave(ier)
#include "building.h"

      common/outin/iuout,iuin
      common/data1/ical
      common/g1/x(mtv),y(mtv),z(mtv),nsur,jvn(ms,mv),nver(ms),ntv
      common/contr/mon,isc(ms),iyd
      common/pophelp/h(60)
      common/filep/ifil
      common/tmc/itmc(ms),nwins
      common/stins/insst(mgt,24,misur),pinsst(mgt,24,misur),
     &             shadst(mgt,24)
      common/sif1/insrts,insrti
      common/ract/paction

      dimension pso(ms,24),psof(ms,24),insstx(24),pinsstx(24)
      character outs*124,paction*16,h*72
      logical ok,dok,close

      dimension isadd(12),ishd(12),ioffs(ms),ntmc(ms)
      
C SI db unit number.
      ifilsi=ifil+3
      ier=0

      irec=1
      read(ifilsi,rec=irec,iostat=istat,err=2)(ishd(i),i=1,12),
     &                                        (isadd(i),i=1,12)

C Check for contents.
      multi=0
      do 10 i=1,12
        if(ishd(i).ne.0)multi=1
        if(ishd(i).ne.0.or.isadd(i).ne.0)goto 1
   10 continue
   
C db has no data.
      irecx=5
      isadd(mon)=irecx
      goto 2

C db has data.
C isadd: record address corresponding to the start of each month.
C ishd:  0 - nothing saved for this month;
C        1 - shading only;
C        2 - insolation only;
C        3 - shading and insolation.
C irecx: next available record in db.
C nsurs: number of zone surfaces in model when db first created.
C msurs: maximum number of internal insolated surfaces in model
C        when db first created.
C ntmc:  0 - surface is not an insolation source;
C        1 - surface is an insolation source.
    1 irec=2
      read(ifilsi,rec=irec,iostat=istat,err=1000)irecx,nsurs,msurs
C Consistency check.
      if(nsur.ne.nsurs.or.msurs.ne.misur)goto 1002
      if(isadd(mon).eq.0)isadd(mon)=irecx

C Confirm that there are insolation data is to be saved.
    2 if(ical.eq.0)then
         call edisp(iuout,' ')
         call edisp(iuout,'No insolation data to be saved!')
         return
      endif

C Check that data have not been saved already.
      if(insrti.ne.0)then
         call edisp(iuout,' ')
         call edisp(iuout,'Insolation data have already been saved!')
         return
      endif

C Establish if the insolation data to be written will result in an
C expansion of the number of records required (new = 0 for no, 1
C for yes).
      new=0
      irec=4
      read(ifilsi,rec=irec,iostat=istat,err=1000)(ntmc(i),i=1,nsur)
      if(paction(1:13).ne.'update_silent')call edisp(iuout,' ')
      do 20 i=1,nsur
         if(itmc(i).ne.0)then
            if(ntmc(i).eq.0)then
               if(multi.eq.1)new=1
               if(paction(1:13).ne.'update_silent')then
                  write(outs,'(a,i2)')
     &               'Adding db entry for insolation source surface ',i
                  call edisp(iuout,outs)
               endif
            else
               if(paction(1:13).ne.'update_silent')then
                  write(outs,'(a,i2)')
     &            'Replacing db entry for insolation source surface ',i
                  call edisp(iuout,outs)
               endif
            endif
         endif
   20 continue

C Possibly update saved data index.
      ishdt=ishd(mon)
      if(ishdt.eq.0.or.ishdt.eq.1)then
         ishd(mon)=ishd(mon)+2
         if(multi.eq.1)new=1
      elseif(ishdt.eq.2.or.ishdt.eq.3)then
         dok=.true.
         h(1)='This month has previously been processed. You may'
         h(2)='wish to update the insolation data with the new'
         h(3)='calculation results. Where this will result in'
         h(4)='the replacement of existing entries you may wish'
         h(5)='to replace the current Zone Shading & Insolation db'
         h(6)='with a new one.'
         write(outs,'(a,i2,a)')
     &       'Insolation data already saved for month ',mon,'.' 
         call askok(outs,'OK to overwrite/append?',ok,dok,6)
         if(.not.ok)return
      endif

C Write data for insolation sources, moving entry for month if the
C data-set has expanded (i.e. new = 1).
      ireco=isadd(mon)
      if(new.eq.1)then
         isadd(mon)=irecx
         irecn=isadd(mon)
      endif
      iof=1
      do 30 i=1,nsur
        if(new.eq.1)then
           read(ifilsi,rec=ireco,iostat=istat,err=1000)
     &                                        (pso(i,j),j=1,24)
           ireco=ireco+1
           write(ifilsi,rec=irecn,iostat=istat,err=1000)
     &                                        (pso(i,j),j=1,24)
           irecn=irecn+1
           read(ifilsi,rec=ireco,iostat=istat,err=1000)
     &                                        (psof(i,j),j=1,24)
           ireco=ireco+1
           write(ifilsi,rec=irecn,iostat=istat,err=1000)
     &                                        (psof(i,j),j=1,24)
           irecn=irecn+1
        else
           ireco=ireco+2
        endif
        ioffs(i)=0
        if(itmc(i).ne.0.or.ntmc(i).ne.0)then

C Current surface is an insolation source - save record 
C offset (ioffs) and then increment by 2 times the maximum 
C number of possible internal insolated surfaces (misur).
           ioffs(i)=iof
           iof=iof+(2*misur)
        endif
   30 continue
      if(new.eq.1)then
         write(ifilsi,rec=irecn,iostat=istat,err=1000)
     &                                        (ioffs(i),i=1,nsur)
         irecn=irecn+1
      else
         write(ifilsi,rec=ireco,iostat=istat,err=1000)
     &                                        (ioffs(i),i=1,nsur)
         ireco=ireco+1
      endif

C Move/write hourly insolation index number for each of the misur
C surfaces that can potentially be insolated.
      isrc=0
      do 40 i=1,nsur
         if(ioffs(i).eq.0)goto 40
         if(itmc(i).ne.0)isrc=isrc+1
         do 50 j=1,misur

C Reset index number to 0 if corresponding proportion is 0.
            do 501 k=1,24
               call eclose(pinsst(isrc,k,j),0.0,0.001,close)
               if((close).and.insst(isrc,k,j).gt.0)insst(isrc,k,j)=0
  501       continue
            if(new.eq.1)then

C Retain previous data if not to be overwritten.
               if(itmc(i).eq.0)then
                  read(ifilsi,rec=ireco,iostat=istat,err=1000)
     &                                        (insstx(k),k=1,24)
                  ireco=ireco+1
                  write(ifilsi,rec=irecn,iostat=istat,err=1000)
     &                                        (insstx(k),k=1,24)
                  irecn=irecn+1
                else
                  write(ifilsi,rec=irecn,iostat=istat,err=1000)
     &                                        (insst(isrc,k,j),k=1,24)
                  irecn=irecn+1
                endif
            else
               write(ifilsi,rec=ireco,iostat=istat,err=1000)
     &                                        (insst(isrc,k,j),k=1,24)
               ireco=ireco+1
            endif
   50    continue

C Move/write hourly insolation factor for each of the misur
C surfaces that can potentially be insolated.
         do 60 j=1,misur
            if(new.eq.1)then

C Retain previous data if not to be overwritten.
               if(itmc(i).eq.0)then
                  read(ifilsi,rec=ireco,iostat=istat,err=1000)
     &                                        (pinsstx(k),k=1,24)
                  ireco=ireco+1
                  write(ifilsi,rec=irecn,iostat=istat,err=1000)
     &                                        (pinsstx(k),k=1,24)
                  irecn=irecn+1
                else
                  write(ifilsi,rec=irecn,iostat=istat,err=1000)
     &                                        (pinsst(isrc,k,j),k=1,24)
                  irecn=irecn+1
                endif
            else
               write(ifilsi,rec=ireco,iostat=istat,err=1000)
     &                                        (pinsst(isrc,k,j),k=1,24)
               ireco=ireco+1
            endif
   60    continue
   40 continue
      if(new.eq.1)then
         irecx=irecn
      else
         irecx=ireco
      endif

C Write header items to capture potential changes.
      irec=1
      write(ifilsi,rec=irec,iostat=istat,err=1000)(ishd(i),i=1,12),
     &                                            (isadd(i),i=1,12)
      irec=2
      write(ifilsi,rec=irec,iostat=istat,err=1000)irecx,nsur,misur
      irec=4
      write(ifilsi,rec=irec,iostat=istat,err=1000)(itmc(i),i=1,nsur)
      insrti=1
      return

C Error handling.
 1000 write(outs,1001)irec
 1001 format('ISAVE: Zone Shading & Insolation db error, record',I6)
      call edisp(iuout,' ')
      call edisp(iuout,outs)
      ier=1
      return
 1002 call edisp(iuout,' ')
      call edisp(iuout,'Geometry saved in Zone Shading & Insolation db')
      call edisp(iuout,'does not correspond to that contained in the')
      call edisp(iuout,'current zone geometry file.')
      ier=1
      return
      end
>>>>>>> .merge-right.r2419
