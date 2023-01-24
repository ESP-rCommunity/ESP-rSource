C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C This file includes the following subroutines.
C rdblist     - scan list of weather databases and present selection to user.
C matchcdb    - scan list of weather databases for name and return database.
C scancdblist - scan list for documentation and typical periods.
C itemclimatelist - parse and extract information about one entry in
C                   climatelist file.

C NOTE: currently up to 124 weather file references are supported. If
C the climatelist file exceeds this, the array name should be revised.

C ******************** rdblist ********************
C Scan list of available weather files and present to user.

      subroutine rdblist(idblfil,clmdb,parent,actreturn,ier)
#include "epara.h"
#include "esprdbfile.h"
#include "espriou.h"

C Manual inclusion of a parameter from building.h
      integer MSPS,MIPVA
      parameter (MSPS=30)
      parameter (MIPVA=MSPS)

#include "seasons.h"
#include "help.h"

      integer lnblnk  ! function definition

C Parameters
      integer idblfil ! file unit for list of known
      character clmdb*144
      character parent*1     ! parent calling application 'p' or 'c'
      character actreturn*1  ! 's' do not start clm 'u' start clm
      integer ier

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/warncounter/iecount
      COMMON/FILEP/IFIL
      common/clmltext/clmlnam,clmlaid,clmldbfile,clmlavail,clmlhelp(60)
      character clmlnam*42,clmlaid*72,clmldbfile*144,clmlavail*10
      character clmlhelp*72

      dimension name(124), mitem(35)
      character name*42,mitem*42,OUTSTR*124,WORD*20
      character group*32,key*1,look*42,tlook*42
      character llclmdb*144
      integer NCO,ICO ! max items and current menu item
      integer ISTRW

      helpinsub='rdblist'

C Set default early winter, spring, summer, autumn, late winter periods.
      CALL EDAY(9,1,ia1wins)
      CALL EDAY(15,1,ia1winf)
      CALL EDAY(6,3,ia1sprs)
      CALL EDAY(12,3,ia1sprf)
      CALL EDAY(11,7,iasums)
      CALL EDAY(17,7,iasumf)
      CALL EDAY(2,10,ia2sprs)
      CALL EDAY(8,10,ia2sprf)
      CALL EDAY(20,11,ia2wins)
      CALL EDAY(26,11,ia2winf)

C Default season definitions.
      CALL EDAY(1,1,is1wins)
      CALL EDAY(28,2,is1winf)
      CALL EDAY(1,11,is2wins)
      CALL EDAY(31,12,is2winf)
      CALL EDAY(1,3,is1sprs)
      CALL EDAY(30,4,is1sprf)
      CALL EDAY(1,9,is2sprs)
      CALL EDAY(31,10,is2sprf)
      CALL EDAY(1,5,is1sums)
      CALL EDAY(31,8,is1sumf)

C Open the list of weather databases. If we got here with a zero
C file unit number then reset it to something safe.
      if(idblfil.eq.0)then
        idblfil=ifil+1
      endif
      CALL EFOPSEQ(idblfil,cdblfil,1,IER)
      if(ier.ne.0)return

C Read the header and see if '*CLIMATE_LIST' is a string.
      CALL STRIPC(idblfil,OUTSTR,0,ND,0,'header',IER)
      if(outstr(1:13).ne.'*CLIMATE_LIST')then
        call edisp(iuout,'Not a list of weather files!')
        ier=1
        return
      endif
      write(currentfile,'(a)') cdblfil(1:lnblnk(cdblfil))

C In the first pass, make up the display, beginning with
C an organizational header and the `names' of each weather db.
      clmdb='UNKNOWN'
      llclmdb='UNKNOWN'
      MHEAD=2
      MCTL=5

C Find how many items to display.
      ILEN=0
      in=0
 33   CALL STRIPC(idblfil,OUTSTR,0,ND,0,'menu scan',IER)
      if(ier.ne.0)goto 90
      K=0
      CALL EGETW(outstr,K,WORD,'W','group id',IER)
      if(WORD(1:6).eq.'*group')then
        call egetrm(outstr,K,group,'W','group id',IER)
        goto 33
      elseif(WORD(1:5).eq.'*name')then
        in=in+1
        call egetrm(outstr,K,name(in),'W','name',IER)
        goto 33
      elseif(WORD(1:4).eq.'*end')then
        goto 90
      else
        goto 33
      endif
  90  CALL ERPFREE(idblfil,ISTAT)
      ILEN=IN
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Build and present the display.
  42  ICO= -2

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(mitem(M),'(a1,1x,a)')KEY,name(L)(1:40)
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      NCO=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        mitem(M+1)='  __________________________  '
      ELSE
        WRITE(mitem(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' ------')
      ENDIF
      write(mitem(1),'(2x,a30)') group(1:30)
      mitem(2)  =  '  __________________________  '
      mitem(M+2)=  '< other weather file          '
      mitem(M+3)=  '* empty weather file          '
      mitem(M+4)=  '? help                        '
      mitem(M+5)=  '- exit menu                   '

      CALL EMENU('Available weather files:',mitem,NCO,ICO)

C Exit menu.
      IF(ICO.EQ.NCO)THEN
        CALL ERPFREE(idblfil,ISTAT)
        return

      ELSEIF(ICO.LE.MHEAD)THEN
        ICO= -1
        goto 42

C Menu help.
      ELSEIF(ICO.EQ.(NCO-1))THEN
        helptopic='climatelist'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('climatelist',nbhelp,'-',0,0,IER)

C New file.
      ELSEIF(ICO.EQ.(NCO-2))THEN
        clmdb='USER_DEFINED'
        llclmdb='USER_DEFINED'
        CALL ERPFREE(idblfil,ISTAT)
        return

C User supplied file. Use file browser with the GTK version.
      ELSEIF(ICO.EQ.(NCO-3))THEN
        clmdb=' '
        llclmdb=' '
        helptopic='request_weather_file'
        call gethelptext(helpinsub,helptopic,nbhelp)
 289    llt=lnblnk(llclmdb)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            ISTRW=96
          elseif(llt.ge.96.and.llt.lt.124)then
            ISTRW=124
          elseif(llt.ge.124.and.llt.le.144)then
            ISTRW=144
          endif
        elseif(iglib.eq.2)then
          ISTRW=144
        else
          ISTRW=96
        endif
        CALL EASKF(llclmdb,' ','Weather file name?',
     &    ISTRW,DCLIM,'weather file name',IER,nbhelp)
        IF(llclmdb(1:2).EQ.'  ')GOTO 289

C Cast back to clmdb.
        llt=lnblnk(llclmdb)
        write(clmdb,'(a)') llclmdb(1:llt)
        CALL ERPFREE(idblfil,ISTAT)
        IER=0
        return
      ELSEIF(ICO.EQ.(NCO-4))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(ICO.GT.MHEAD.AND.ICO.LT.(NCO-MCTL+1))THEN

C Present information on a weather db.
        CALL KEYIND(NCO,ICO,IFOC,IO)
        look=name(IFOC)
        CALL EFOPSEQ(idblfil,cdblfil,1,IER)
        CALL STRIPC(idblfil,OUTSTR,0,ND,0,'header',IER)
        CALL STRIPC(idblfil,OUTSTR,0,ND,0,'h group id',IER)
  3     CALL STRIPC(idblfil,OUTSTR,0,ND,0,'item lines',IER)
        if(ier.ne.0)goto 92

        clmdb=' '
        K=0
        CALL EGETW(outstr,K,WORD,'W','*name tag',IER)
        if(WORD(1:5).eq.'*name')then
          call egetrm(outstr,K,tlook,'W','name',IER)
          if(tlook(1:lnblnk(tlook)).eq.look(1:lnblnk(look)))then

C Parse this item of the climatelist file.
C Use an actreturn 's' that says do not start clm module and
C actreturn 'u' to start clm module.
            write(clmlnam,'(a)') tlook(1:42)  ! remember menu entry 
            call itemclimatelist(idblfil,'r',parent,actreturn,clmdb,ier)
            if(actreturn.eq.'e')then
              goto 92
            elseif(actreturn.eq.'i')then
              goto 3
            elseif(actreturn.eq.'m')then
              goto 42
            elseif(actreturn.eq.'u')then
              CALL ERPFREE(idblfil,ISTAT)
              return
            elseif(actreturn.eq.'s')then
              CALL ERPFREE(idblfil,ISTAT)
              return
            elseif(actreturn.eq.'f')then
              if(parent.eq.'p')then
                CALL ERPFREE(idblfil,ISTAT)
                write(ieout,'(a)') 'Climate list scan failed!'
              elseif(parent.eq.'c')then
                CALL ERPFREE(idblfil,ISTAT)
                write(ieout,'(a)') 'Climate list scan failed!'
              endif
              call pauses(3)   ! pause for 3 seconds then abort program.
              iecount=iecount+1
              close(ieout)
              CALL EPAGEND
              STOP
            endif
          else
            goto 3
          endif
        else
          goto 3
        endif
      else
        goto 42
      endif
      goto 42

  92  CALL ERPFREE(idblfil,ISTAT)
      goto 42

      end

C ******************** matchcdb ********************
C Scan list of weather files.

      subroutine matchcdb(idblfil,alias,clmdb,ier)
#include "espriou.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/clmltext/clmlnam,clmlaid,clmldbfile,clmlavail,clmlhelp(60)
      character clmlnam*42,clmlaid*72,clmldbfile*144,clmlavail*10
      character clmlhelp*72

      dimension name(124)
      character name*42,OUTSTR*124,WORD*20
      character group*32,tlook*42,outs*124,louts*248
      character clmdb*144,avail*10,alias*32

C Open the list of weather files.
      CALL EFOPSEQ(idblfil,cdblfil,1,IER)
      if(ier.ne.0)return
      write(currentfile,'(a)') cdblfil(1:lnblnk(cdblfil))

C Read the header and see if '*CLIMATE_LIST' is a string.
      CALL STRIPC(idblfil,OUTSTR,0,ND,0,'header',IER)
      if(outstr(1:13).ne.'*CLIMATE_LIST')then
        call edisp(iuout,' ')
        call edisp(iuout,' Not a list of weather files!')
        ier=1
        return
      endif

C In the first pass, make up list of `names' of each weather db.
      clmdb='UNKNOWN'
      in=0
  2   CALL STRIPC(idblfil,OUTSTR,0,ND,0,'menu scan',IER)
      if(ier.ne.0)goto 90
      K=0
      CALL EGETW(outstr,K,WORD,'W','group id',IER)
      if(WORD(1:6).eq.'*group')then
        call egetrm(outstr,K,group,'W','group id',IER)
        goto 2
      elseif(WORD(1:5).eq.'*name')then
        in=in+1
        call egetrm(outstr,K,name(in),'W','name',IER)
        goto 2
      elseif(WORD(1:4).eq.'*end')then
        goto 90
      else
        goto 2
      endif
  90  CALL ERPFREE(idblfil,ISTAT)

C Get info for db matching the alias.
      CALL EFOPSEQ(idblfil,cdblfil,1,IER)
      CALL STRIPC(idblfil,OUTSTR,0,ND,0,'header',IER)
      CALL STRIPC(idblfil,OUTSTR,0,ND,0,'group id',IER)
  3   CALL STRIPC(idblfil,OUTSTR,0,ND,0,'item lines',IER)
      if(ier.ne.0)goto 92

      clmdb='UNKNOWN'
      K=0
      CALL EGETW(outstr,K,WORD,'W','*name tag',IER)
      if(WORD(1:5).eq.'*name')then
        tlook = ' '
        call egetrm(outstr,K,tlook,'W','name',IER)
        if(tlook(1:lnblnk(tlook)).eq.alias(1:lnblnk(alias)))then
          write(clmlnam,'(a)') tlook(1:42)  ! remember menu entry 

  4       CALL STRIPC(idblfil,OUTSTR,0,ND,0,'item line',IER)
          if(ier.ne.0)goto 92
          K=0
          CALL EGETW(outstr,K,WORD,'W','*? tag',IER)
          if(WORD(1:4).eq.'*end')then
            goto 92
          elseif(WORD(1:5).eq.'*dbfl')then
            call egetrm(outstr,K,clmdb,'W','db file',IER)
            write(louts,'(a,a)') ' Weather file: ',
     &        clmdb(1:lnblnk(clmdb))
            call edisp248(iuout,louts,100)
            goto 4
          elseif(WORD(1:5).eq.'*aide')then
            call egetrm(outstr,K,clmlaid,'W','brief description',IER)
            write(outs,'(a,a)') ' Description: ',clmlaid
            call edisp(iuout,outs)
            goto 4
          elseif(WORD(1:6).eq.'*avail')then
            call egetrm(outstr,K,avail,'W','db avail',IER)
            if(avail(1:7).eq.'OFFLINE')then
              call usrmsg(
     &        'Weather file not online; ask your ESP-r',
     &        'administrator to change its status.','W')
              clmdb='UNKNOWN'
              return
            elseif(avail(1:6).eq.'ONLINE')then
              CALL ERPFREE(idblfil,ISTAT)
              return
            endif
          elseif(WORD(1:11).eq.'*help_start')then
            goto 4
          elseif(WORD(1:9).eq.'*help_end')then
            goto 4
          elseif(WORD(1:5).eq.'*item')then
            goto 3
          else
            goto 3
          endif
        else
          goto 3
        endif
      else
        goto 3
      endif
      return

  92  CALL ERPFREE(idblfil,ISTAT)
      return

      end

C ******************** scancdblist ********************
C Scan climatelist file for documentation and typical periods
C which might match the current database. Should be called if
C the climate file was not selected via scan of climatelist.

      subroutine scancdblist(idblfil,clmdb,parent,ok,ier)
#include "espriou.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/clmltext/clmlnam,clmlaid,clmldbfile,clmlavail,clmlhelp(60)
      character clmlnam*42,clmlaid*72,clmldbfile*144,clmlavail*10
      character clmlhelp*72

      character*(*) clmdb
      character OUTSTR*124,WORD*20,outs*124
      character tlook*72
      character actreturn*1,parent*1
      logical ok
      logical unixok

C Open the list of weather files.
      ok=.false.
      CALL EFOPSEQ(idblfil,cdblfil,1,IER)

C If fail during file opening.
      if(ier.ne.0)then
        call isunix(unixok)
        if(unixok)then
          write(6,*) 'problem opening ',cdblfil(1:lnblnk(cdblfil))
        endif
        ok=.false.
        return
      endif

C Read the header and see if '*CLIMATE_LIST' is a string.
      CALL STRIPC(idblfil,OUTSTR,0,ND,0,'header',IER)
      if(outstr(1:13).ne.'*CLIMATE_LIST')then
        call edisp(iuout,' Not a list of weather files!')
        ier=1
        ok=.false.
        return
      endif
     
      write(currentfile,'(a)') cdblfil(1:lnblnk(cdblfil))

C Get info for db matching the alias.
      CALL STRIPC(idblfil,OUTSTR,0,ND,0,'group id',IER)
  3   CALL STRIPC(idblfil,OUTSTR,0,ND,0,'item lines',IER)
      if(ier.ne.0)goto 92

      K=0
      CALL EGETW(outstr,K,WORD,'W','*name tag',IER)
      if(WORD(1:5).eq.'*aide')then
        call egetrm(outstr,K,clmlaid,'W','brief description',IER)
        goto 3
      elseif(WORD(1:5).eq.'*name')then
        call egetrm(outstr,K,clmlnam,'W','menu entry',IER)
        goto 3
      elseif(WORD(1:5).eq.'*dbfl')then
        call egetrm(outstr,K,tlook,'W','file name',IER)
        if(tlook(1:lnblnk(tlook)).eq.clmdb(1:lnblnk(clmdb)))then

C Parse this item of the climatelist file.
          write(outs,'(a,a)') ' Description : ',clmlaid
          call edisp(iuout,outs)
          write(outs,'(a,a)') ' Menu entry : ',
     &      clmlnam(1:lnblnk(clmlnam))
          call edisp(iuout,outs)

          call itemclimatelist(idblfil,'s',parent,actreturn,clmdb,ier)
          if(actreturn.eq.'e')then
            if(ier.eq.0) ok = .true.
            goto 92
          elseif(actreturn.eq.'i')then
            if(ier.eq.0) ok = .true.
            goto 3
          endif
        else
          goto 3
        endif
      else
        goto 3
      endif
      return

  92  CALL ERPFREE(idblfil,ISTAT)
      return

      end

C ******************** itemclimatelist ********************
C Parse and extract information about one entry in climatelist file.
C It is assumed that the climatelist file is already open on idbfil.
C Act - if 's' then itemclimatelist has been called from scancdblist
C and if 'r' then itemclimatelist has been called from rdblist.

C Actreturn passes back instructions to the calling code as follows:
C  if 'i' then jump to read another item in climatelist (3)
C  if 'm' then jump to rebuild climatelist menu (42)
C  if 'f' then stop the application
C  if 'u' then jump to 'useit' part of calling subroutine
C  if 's' then jump to 'selectedit' part of the calling subroutine
C         (does not start the clm module)
C  if 'e' then found 'end' so close unit number (92)

      subroutine itemclimatelist(idblfil,act,parent,actreturn,clmdb,ier)

C Manual inclusion of a parameter from building.h
      integer MSPS,MIPVA
      parameter (MSPS=30)
      parameter (MIPVA=MSPS)

#include "seasons.h"
#include "help.h"

      integer lnblnk  ! function definition
      integer iCountWords

C Parameters passed.
      character act*1,parent*1,actreturn*1,clmdb*144

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C Strings associated with one climatelist item.
C clmlnam    - the selection list string
C clmlaid    - aide memoire for list item
C clmldbfile - the weather file (with path info)
C clmlavail  - either ONLINE or OFFLINE
C clmlhelp   - array of help strings
      common/clmltext/clmlnam,clmlaid,clmldbfile,clmlavail,clmlhelp(60)
      character clmlnam*42,clmlaid*72,clmldbfile*144,clmlavail*10
      character clmlhelp*72

C Numbers associated with one climatelist item.
      common/clmlnum/nbclmlhelp

      character OUTSTR*124,WORD*20,louts*248
      character outs*124
      character pstring*40

  4   CALL STRIPC(idblfil,OUTSTR,0,ND,0,'item line',IER)

      if(ier.ne.0)then
        actreturn='e'
        return
      endif
      K=0
      CALL EGETW(outstr,K,WORD,'W','*? tag',IER)
      if(WORD(1:4).eq.'*end')then
        actreturn='e'
        return
      elseif(WORD(1:5).eq.'*dbfl')then
        if(act.eq.'s')then
          goto 4
        elseif(act.eq.'r')then
          call egetrm(outstr,K,clmdb,'W','db file',IER)
          write(louts,'(a,a)') ' File location: ',
     &      clmdb(1:lnblnk(clmdb))
          call edisp248(iuout,louts,100)
          write(clmldbfile,'(a)') clmdb(1:lnblnk(clmdb))
          goto 4
        endif
      elseif(WORD(1:5).eq.'*aide')then
        call egetrm(outstr,K,clmlaid,'W','brief description',IER)
        write(outs,'(a,a)') ' Description: ',clmlaid
        call edisp(iuout,' ')
        call edisp(iuout,outs)
        goto 4
      elseif(WORD(1:5).eq.'*name')then
        call egetrm(outstr,K,clmlnam,'W','menu entry',IER)
        write(outs,'(a,a)')' Menu entry  : ',clmlnam(1:lnblnk(clmlnam))
        call edisp(iuout,outs)
        goto 4
      elseif(WORD(1:6).eq.'*avail')then
        call egetrm(outstr,K,clmlavail,'W','db avail',IER)
        if(act.eq.'s')then
          write(outs,'(a,a)') ' Weather is : ',clmlavail
          call edisp(iuout,outs)
        endif
        goto 4
      elseif(WORD(1:9).eq.'*winter_t')then

C Instantiate typper common block variables ia1wins,ia1winf,ia2wins,ia2winf 
        call egetrm(outstr,K,pstring,'W','winter typ',IER)
        ni = iCountWords(pstring)
        if(ni.eq.4)then

C Only has the start and end day of early winter so create a default later winter.
          K=0
          CALL EGETWI(pstring,K,ID1,1,31,'F','ewin s day',IER)
          CALL EGETWI(pstring,K,IM1,1,12,'F','ewin s month',IER)
          CALL EGETWI(pstring,K,ID2,1,31,'F','ewin f day',IER)
          CALL EGETWI(pstring,K,IM2,1,12,'F','ewin f month',IER)
          CALL EDAY(ID1,IM1,ia1wins)
          CALL EDAY(ID2,IM2,ia1winf)
          CALL EDAY(20,11,ia2wins)
          CALL EDAY(26,11,ia2winf)
        elseif(ni.eq.8)then

C Has start and end day of both winters.
          K=0
          CALL EGETWI(pstring,K,ID1,1,31,'F','s day',IER)
          CALL EGETWI(pstring,K,IM1,1,12,'F','s month',IER)
          CALL EGETWI(pstring,K,ID2,1,31,'F','f day',IER)
          CALL EGETWI(pstring,K,IM2,1,12,'F','f month',IER)
          CALL EDAY(ID1,IM1,ia1wins)
          CALL EDAY(ID2,IM2,ia1winf)
          CALL EGETWI(pstring,K,ID1,1,31,'F','s day',IER)
          CALL EGETWI(pstring,K,IM1,1,12,'F','s month',IER)
          CALL EGETWI(pstring,K,ID2,1,31,'F','f day',IER)
          CALL EGETWI(pstring,K,IM2,1,12,'F','f month',IER)
          CALL EDAY(ID1,IM1,ia2wins)
          CALL EDAY(ID2,IM2,ia2winf)
        else
          CALL EDAY(9,1,ia1wins)
          CALL EDAY(15,1,ia1winf)
          CALL EDAY(20,11,ia2wins)
          CALL EDAY(26,11,ia2winf)
        endif
        goto 4
      elseif(WORD(1:9).eq.'*spring_t')then

C Instantiate typper common block variables ia1sprs,ia1sprf,ia2sprs,ia2sprf .
        call egetrm(outstr,K,pstring,'W','spring typ',IER)
        ni = iCountWords(pstring)
        if(ni.eq.4)then

C Only has the start and end day of spring so create a default autumn.
          K=0
          CALL EGETWI(pstring,K,ID1,1,31,'F','day',IER)
          CALL EGETWI(pstring,K,IM1,1,12,'F','month',IER)
          CALL EGETWI(pstring,K,ID2,1,31,'F','day',IER)
          CALL EGETWI(pstring,K,IM2,1,12,'F','month',IER)
          CALL EDAY(ID1,IM1,ia1sprs)
          CALL EDAY(ID2,IM2,ia1sprf)
          CALL EDAY(2,10,ia2sprs)
          CALL EDAY(8,10,ia2sprf)
        elseif(ni.eq.8)then

C Has start and end day and typical day of spring and autumn.
          K=0
          CALL EGETWI(pstring,K,ID1,1,31,'F','day',IER)
          CALL EGETWI(pstring,K,IM1,1,12,'F','month',IER)
          CALL EGETWI(pstring,K,ID2,1,31,'F','day',IER)
          CALL EGETWI(pstring,K,IM2,1,12,'F','month',IER)
          CALL EDAY(ID1,IM1,ia1sprs)
          CALL EDAY(ID2,IM2,ia1sprf)
          CALL EGETWI(pstring,K,ID1,1,31,'F','s day',IER)
          CALL EGETWI(pstring,K,IM1,1,12,'F','s month',IER)
          CALL EGETWI(pstring,K,ID2,1,31,'F','f day',IER)
          CALL EGETWI(pstring,K,IM2,1,12,'F','f month',IER)
          CALL EDAY(ID1,IM1,ia2sprs)
          CALL EDAY(ID2,IM2,ia2sprf)
        else
          CALL EDAY(6,3,ia1sprs)
          CALL EDAY(12,3,ia1sprf)
          CALL EDAY(2,10,ia2sprs)
          CALL EDAY(8,10,ia2sprf)
        endif
        goto 4
      elseif(WORD(1:9).eq.'*summer_t')then

C Instantiate common block variables iasums & iasumf.
        call egetrm(outstr,K,pstring,'W','summer typ',IER)
        ni = iCountWords(pstring)
        K=0
        CALL EGETWI(pstring,K,ID1,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM1,1,12,'F','month',IER)
        CALL EGETWI(pstring,K,ID2,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM2,1,12,'F','month',IER)
        CALL EDAY(ID1,IM1,iasums)
        CALL EDAY(ID2,IM2,iasumf)
        goto 4
      elseif(WORD(1:9).eq.'*winter_s')then

C Read winter season - two periods e.g. nov-dec & jan-feb
C common block typsea is1wins,is1winf,is2wins,is2winf.
        call egetrm(outstr,K,pstring,'W','winter sea',IER)
        ni = iCountWords(pstring)
        K=0
        CALL EGETWI(pstring,K,ID1,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM1,1,12,'F','month',IER)
        CALL EGETWI(pstring,K,ID2,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM2,1,12,'F','month',IER)
        CALL EGETWI(pstring,K,ID3,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM3,1,12,'F','month',IER)
        CALL EGETWI(pstring,K,ID4,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM4,1,12,'F','month',IER)
        CALL EDAY(ID1,IM1,is1wins)
        CALL EDAY(ID2,IM2,is1winf)
        CALL EDAY(ID3,IM3,is2wins)
        CALL EDAY(ID4,IM4,is2winf)
        goto 4
      elseif(WORD(1:9).eq.'*spring_s')then

C Read transition season - two periods e.g. mar-may & sep-oct
C common block typsea is1sprs,is1sprf,is2sprs,is2sprf
        call egetrm(outstr,K,pstring,'W','trans sea',IER)
        ni = iCountWords(pstring)
        K=0
        CALL EGETWI(pstring,K,ID1,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM1,1,12,'F','month',IER)
        CALL EGETWI(pstring,K,ID2,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM2,1,12,'F','month',IER)
        CALL EGETWI(pstring,K,ID3,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM3,1,12,'F','month',IER)
        CALL EGETWI(pstring,K,ID4,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM4,1,12,'F','month',IER)
        CALL EDAY(ID1,IM1,is1sprs)
        CALL EDAY(ID2,IM2,is1sprf)
        CALL EDAY(ID3,IM3,is2sprs)
        CALL EDAY(ID4,IM4,is2sprf)
        goto 4
      elseif(WORD(1:9).eq.'*summer_s')then

C Read summer season - e.g. jun-aug (common block typsea is1sums,is1sumf).
        call egetrm(outstr,K,pstring,'W','summer sea',IER)
        ni = iCountWords(pstring)
        K=0
        CALL EGETWI(pstring,K,ID1,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM1,1,12,'F','month',IER)
        CALL EGETWI(pstring,K,ID2,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM2,1,12,'F','month',IER)
        CALL EDAY(ID1,IM1,is1sums)
        CALL EDAY(ID2,IM2,is1sumf)
        goto 4
      elseif(WORD(1:11).eq.'*help_start')then

C Read and store the comments into the HELP array for later display.
        nbclmlhelp=0
 142    CALL STRIPC(idblfil,OUTSTR,0,ND,0,'help line',IER)
        if(ier.ne.0)then
          actreturn='e'
          return
        endif
        if(OUTSTR(1:9).eq.'*help_end')then
          goto 4
        else
          nbclmlhelp=nbclmlhelp+1
          lastcc = MIN0(lnblnk(OUTSTR),72)
          if(nbclmlhelp.lt.60)then
            write(h(nbclmlhelp),'(a)')OUTSTR(1:lastcc)
            write(clmlhelp(nbclmlhelp),'(a)')OUTSTR(1:lastcc)
          endif
        endif
        goto 142
      elseif(WORD(1:9).eq.'*help_end')then
        goto 4
      elseif(WORD(1:5).eq.'*item')then

C Reached the end of the item, check if it is to be chosen. Reset
C the first line of the scanned documentation/help to compensate
C for the line used in EASKOK.
        if(act.eq.'r')then
          lasth = MIN0(nbclmlhelp,60)
          do 1546 iasth=1,lasth
            call edisp(iuout,clmlhelp(iasth))
 1546     continue
          if(clmlavail(1:7).eq.'OFFLINE')then
            if(parent(1:1).eq.'c')then
              call usrmsg('Weather file is offline!',' ','W')
              CALL EASKAB(' ','Options:','Exit clm',
     &          'Select another from list',IW,nbhelp)
              if(IW.eq.2)then
                clmdb='UNKNOWN'
                actreturn='m'
                return
              elseif(IW.eq.1)then
                clmdb='UNKNOWN'
                actreturn='f'
                return
              endif
            elseif(parent(1:1).eq.'p')then
              call usrmsg('Weather file offline!',' ','W')
              CALL EASKAB(' ','Options:','continue',
     &          'select another',IW,nbhelp)
              if(IW.eq.2)then
                clmdb='UNKNOWN'
                actreturn='m'
                return
              elseif(IW.eq.1)then
                clmdb='UNKNOWN'
                actreturn='f'
                return
              endif
            endif
          elseif(clmlavail(1:6).eq.'ONLINE')then
            if(parent(1:1).eq.'c')then
              CALL EASKAB(' ','Options:','use it','reselect',IW,nbhelp)
              if(IW.eq.2)then
                clmdb='UNKNOWN'
                actreturn='m'
                return
              elseif(IW.eq.1)then
                CALL ERPFREE(idblfil,ISTAT)
                actreturn='u'   ! use it via clm
                return
              endif
            elseif(parent(1:1).eq.'p')then
              if(IW.eq.2)then
                clmdb='UNKNOWN'
                actreturn='m'
                return
              elseif(IW.eq.1)then
                CALL ERPFREE(idblfil,ISTAT)
                actreturn='s'   ! 
                return
              endif
            endif
          endif
        elseif(act.eq.'s')then
          if(clmlavail(1:7).eq.'OFFLINE')then
            call usrmsg('Weather file offline!',' ','W')
            clmdb='UNKNOWN'
            actreturn='m'
            return
          elseif(clmlavail(1:6).eq.'ONLINE')then
            actreturn='e'
            return
          endif
        endif
      else
        actreturn='i'
        return
      endif

      end

