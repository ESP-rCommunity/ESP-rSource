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

C rcdblist.F includes:
C  rdblist - scan list of climate databases and present selection to user.
C  matchcdb - scan list of climate databases for name and return database.
C  scancdblist - scan list for documentation and typical periods
C  itemclimatelist - parse and extract information about one entry
C     in climatelist file.
C  editemclimatelist - edit/create an item for climatelist file.
C  mkitemclimatelist - write an item for climatelist file.
C DDRADSUM - looks for typical week in each season based on closest
C     degree days and radiation patterns.

C ********* rdblist
C Scan list of climate databases and present selection to user.
      subroutine rdblist(idblfil,cdblfil,clmdb,ier)
#include "epara.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/curfile/currentfile
      common/pophelp/h(60)
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      COMMON/DEFLT1/DCLIM,DAPRES,DFCON,DFMUL,DOPTDB,DPRFDB,DPCDB

C Typical periods win1|trn1|sum|trn2|win2 with start & finish
      common/typper/ia1wins,ia1winf,ia1sprs,ia1sprf,
     &  iasums,iasumf,ia2sprs,ia2sprf,ia2wins,ia2winf

C Season definitions. 2 periods for winter (i.e. nov-dec and
C jan-feb), transition (i.e. mar-may & sep-oct) and one period for summer.
      common/typsea/is1wins,is1winf,is2wins,is2winf,is1sprs,is1sprf,
     &              is2sprs,is2sprf,is1sums,is1sumf

      dimension name(60), mitem(35)
      character*72 DCLIM,DAPRES,DFCON,DFMUL,DOPTDB,DPRFDB,DPCDB
      character name*32,mitem*33,OUTSTR*124,WORD*20
      character cdblfil*72, group*32,key*1,look*32,tlook*32
      character clmdb*72,h*72,actreturn*1,currentfile*72

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

C Open the list of climate databases. If we got here with a zero
C file unit number then reset it to something safe.
      if(idblfil.eq.0)then
        idblfil=ifil+1
      endif
      CALL EFOPSEQ(idblfil,cdblfil,1,IER)
      if(ier.ne.0)return

C Read the header and see if '*CLIMATE_LIST' is a string.
      CALL STRIPC(idblfil,OUTSTR,0,ND,0,'header',IER)
      if(outstr(1:13).ne.'*CLIMATE_LIST')then
        call edisp(iuout,' Not a list of climate databases... ')
        ier=1
        return
      endif
      currentfile=cdblfil

C In the first pass, make up the display, beginning with
C an organizational header and the `names' of each climate db.
      clmdb='UNKNOWN'
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

C debug...
C      write(6,*)cdblfil

C Build and present the display.
  42  ICO= -2

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(mitem(M),'(a1,1x,a30)')KEY,name(L)(1:30)
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      NCO=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        mitem(M+1)='  ______________________________ '
      ELSE
        WRITE(mitem(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' ------')
      ENDIF
      write(mitem(1),'(2x,a30)') group(1:30)
      mitem(2)   =  '  __________________________  '
      mitem(M+2)=  '< user defined climate db     '
      mitem(M+3)=  '* new climate db (all zeros)  '
      mitem(M+4)=  '? help                        '
      mitem(M+5)=  '- exit                        '

      CALL EMENU('  Climate sets',mitem,NCO,ICO)
      IF(ICO.EQ.NCO)THEN
        CALL ERPFREE(idblfil,ISTAT)
        return
      ELSEIF(ICO.LE.MHEAD)THEN
        ICO= -1
        goto 42
      ELSEIF(ICO.EQ.(NCO-1))THEN

C Help @ db file level.
        h(1)='Several climate databases are available for browsing.'
        h(2)='To find out more about one of them simply select and'
        h(3)='additional information will be presented.  If you'
        h(4)='wish to choose one, use the choose option.'
        h(5)=' '
        h(6)=' NOTE: administrators should update this file to match'
        h(7)='   the databases, paths and documentation for their'
        h(8)='   group.  Use a `tab` between the tag and the string.'
        h(9)='      '
        h(10)='For a sample of the file use `show me more`, '
        CALL PHELPD('clm list',10,'clm_dblist  ',0,0,IER)
      ELSEIF(ICO.EQ.(NCO-2))THEN

C New db.
        clmdb='USER_DEFINED'
        CALL ERPFREE(idblfil,ISTAT)
        return
      ELSEIF(ICO.EQ.(NCO-3))THEN

C User supplied (which assumes default periods).
C << this code should be using easkf, but there is an issue >>
C << that clmdb is still 72 characters. >>
        clmdb=' '
        h(1)='This climate db is assumed to be in ESP-r format. '
 289    CALL EASKS(clmdb,' Climate file (esp-r binary format) ?',
     &    ' ',72,DCLIM,'climate file',IER,1)
        IF(clmdb.EQ.' ')GOTO 289
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

C Present information on a climate db.
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
            call itemclimatelist(idblfil,'r',actreturn,clmdb,ier)
            if(actreturn.eq.'e')then
              goto 92
            elseif(actreturn.eq.'i')then
              goto 3
            elseif(actreturn.eq.'m')then
              goto 42
            elseif(actreturn.eq.'u')then
              CALL ERPFREE(idblfil,ISTAT)
              return
            elseif(actreturn.eq.'f')then
              CALL ERPFREE(idblfil,ISTAT)
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

C ********* matchcdb
C Scan list of climate databases for name and return database.
      subroutine matchcdb(idblfil,cdblfil,alias,clmdb,ier)
      COMMON/OUTIN/IUOUT,IUIN
      common/curfile/currentfile

      dimension name(60)
      character name*32,OUTSTR*124,WORD*20
      character cdblfil*72, group*32,tlook*32,line*72,outs*124
      character clmdb*72,avail*10,alias*32,currentfile*72

C Open the list of climate databases.
      CALL EFOPSEQ(idblfil,cdblfil,1,IER)
      if(ier.ne.0)return
      currentfile=cdblfil

C Read the header and see if '*CLIMATE_LIST' is a string.
      CALL STRIPC(idblfil,OUTSTR,0,ND,0,'header',IER)
      if(outstr(1:13).ne.'*CLIMATE_LIST')then
        call edisp(iuout,' Not a list of climate databases... ')
        ier=1
        return
      endif

C In the first pass, make up list of `names' of each climate db.
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

      line=' '
      clmdb='UNKNOWN'
      K=0
      CALL EGETW(outstr,K,WORD,'W','*name tag',IER)
      if(WORD(1:5).eq.'*name')then
        tlook = ' '
        call egetrm(outstr,K,tlook,'W','name',IER)
        if(tlook(1:lnblnk(tlook)).eq.alias(1:lnblnk(alias)))then
  4       CALL STRIPC(idblfil,OUTSTR,0,ND,0,'item line',IER)
          if(ier.ne.0)goto 92
          K=0
          CALL EGETW(outstr,K,WORD,'W','*? tag',IER)
          if(WORD(1:4).eq.'*end')then
            goto 92
          elseif(WORD(1:5).eq.'*dbfl')then
            call egetrm(outstr,K,clmdb,'W','db file',IER)
            write(outs,'(a,a)') ' Climate database: ',clmdb
            call edisp(iuout,outs)
            goto 4
          elseif(WORD(1:5).eq.'*aide')then
            call egetrm(outstr,K,line,'W','brief description',IER)
            write(outs,'(a,a)') ' Description  : ',line
            call edisp(iuout,outs)
            goto 4
          elseif(WORD(1:6).eq.'*avail')then
            call egetrm(outstr,K,avail,'W','db avail',IER)
            write(outs,'(a,a)') ' Climate is : ',avail
            call edisp(iuout,outs)
            if(avail(1:7).eq.'OFFLINE')then
              call usrmsg(
     & 'The selected climate file is not `ONLINE` and so you will have',
     & 'to ask your systems administrator to bring it on line. ','W')
              clmdb='UNKNOWN'
              return
            elseif(avail(1:6).eq.'ONLINE')then
              CALL ERPFREE(idblfil,ISTAT)
              write(6,*)'found match ',clmdb
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

C ********* scancdblist
C Scan climatelist file of for documentation and typical periods
C which might match the current database. Should be called if
C the climate file was not selected via scan of climatelist.
      subroutine scancdblist(idblfil,cdblfil,clmdb,ok,ier)
      COMMON/OUTIN/IUOUT,IUIN
      common/curfile/currentfile

      character*(*) clmdb
      character OUTSTR*124,WORD*20
      character cdblfil*72,tlook*72
      character actreturn*1,currentfile*72
      logical ok

C Open the list of climate databases.
      ok=.false.
      CALL EFOPSEQ(idblfil,cdblfil,1,IER)
      if(ier.ne.0)then
        write(6,*) 'problem opening ',cdblfil(1:lnblnk(cdblfil))
        ok=.false.
        return
      endif

C Read the header and see if '*CLIMATE_LIST' is a string.
      CALL STRIPC(idblfil,OUTSTR,0,ND,0,'header',IER)
      if(outstr(1:13).ne.'*CLIMATE_LIST')then
        call edisp(iuout,' Not a list of climate databases... ')
        ier=1
        ok=.false.
        return
      endif
      currentfile=cdblfil

C Get info for db matching the alias.
      CALL STRIPC(idblfil,OUTSTR,0,ND,0,'group id',IER)
  3   CALL STRIPC(idblfil,OUTSTR,0,ND,0,'item lines',IER)
      if(ier.ne.0)goto 92

      K=0
      CALL EGETW(outstr,K,WORD,'W','*name tag',IER)
      if(WORD(1:5).eq.'*dbfl')then
        call egetrm(outstr,K,tlook,'W','file name',IER)
        if(tlook(1:lnblnk(tlook)).eq.clmdb(1:lnblnk(clmdb)))then

C Parse this item of the climatelist file.
          call itemclimatelist(idblfil,'s',actreturn,clmdb,ier)
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

C ******* itemclimatelist(idbfil,act,actreturn,clmdb,ier)
C Parse and extract information about one entry in climatelist file.
C It is assumed that the climatelist file is already open on idbfil.
C Act - if 's' then itemclimatelist has been called from scancdblist
C and if 'r' then itemclimatelist has been called from rdblist.

C Actreturn passes back instructions to the calling code as follows:
C  if 'i' then jump to read another item in climatelist (3)
C  if 'm' then jump to rebuild climatelist menu (42)
C  if 'f' then stop the application
C  if 'u' then jump to 'useit' part of calling subroutine
C  if 'e' then found 'end' so close unit number (92)
      subroutine itemclimatelist(idblfil,act,actreturn,clmdb,ier)
C << clmdb might need to be a longer name >>
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

C Typical periods win1|trn1|sum|trn2|win2 with start & finish
      common/typper/ia1wins,ia1winf,ia1sprs,ia1sprf,
     &  iasums,iasumf,ia2sprs,ia2sprf,ia2wins,ia2winf

C Season definitions. 2 periods for winter (i.e. nov-dec and
C jan-feb), transition (i.e. mar-may & sep-oct) and one period for summer.
      common/typsea/is1wins,is1winf,is2wins,is2winf,is1sprs,is1sprf,
     &              is2sprs,is2sprf,is1sums,is1sumf

C Strings associated with one climatelist item.
C clmlnam (char*32) is the selection list string
C clmlaid is aide (char*72) memoire for list item
C clmldbfile (char*72) is the climate file (with path info)
C clmlavail (char*10) is either ONLINE or OFFLINE
C clmlhelp (char*72) is array of help strings
      common/clmltext/clmlnam,clmlaid,clmldbfile,clmlavail,clmlhelp(60)

C Numbers associated with one climatelist item.
      common/clmlnum/nbclmlhelp

      character OUTSTR*124,WORD*20
      character sethead*72,outs*124
      character clmdb*72,h*72,pstring*40,act*1,actreturn*1
      character clmlnam*32,clmlaid*72,clmldbfile*72,clmlavail*10
      character clmlhelp*72

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
          write(outs,'(a,a)') ' Climate database: ',clmdb
          call edisp(iuout,outs)
          clmldbfile=clmdb
          goto 4
        endif
      elseif(WORD(1:5).eq.'*aide')then
        call egetrm(outstr,K,clmlaid,'W','brief description',IER)
        write(outs,'(a,a)') ' Description  : ',clmlaid
        call edisp(iuout,outs)
        if(act.eq.'r')sethead = clmlaid
        goto 4
      elseif(WORD(1:6).eq.'*avail')then
        call egetrm(outstr,K,clmlavail,'W','db avail',IER)
        if(act.eq.'s')then
          write(outs,'(a,a)') ' Climate is : ',clmlavail
          call edisp(iuout,outs)
        endif
        goto 4
      elseif(WORD(1:9).eq.'*winter_t')then
        call egetrm(outstr,K,pstring,'W','winter typ',IER)
        CALL CHITMS(pstring,ni)
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
        call egetrm(outstr,K,pstring,'W','spring typ',IER)
        CALL CHITMS(pstring,ni)
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
        call egetrm(outstr,K,pstring,'W','summer typ',IER)
        CALL CHITMS(pstring,ni)
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
C is1wins,is1winf,is2wins,is2winf
        call egetrm(outstr,K,pstring,'W','winter sea',IER)
        CALL CHITMS(pstring,ni)
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
C is1sprs,is1sprf,is2sprs,is2sprf
        call egetrm(outstr,K,pstring,'W','trans sea',IER)
        CALL CHITMS(pstring,ni)
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

C Read summer season - one period e.g. jun-aug (is1sums,is1sumf).
        call egetrm(outstr,K,pstring,'W','summer sea',IER)
        CALL CHITMS(pstring,ni)
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

C debug...
C      write(6,*)iawins,iawinf,iasprs,iasprf,iasums,iasumf
C      write(6,*)is1wins,is1winf,is2wins,is2winf,is1sprs,is1sprf,
C     &              is2sprs,is2sprf,is1sums,is1sumf

C Reached the end of the item, check if it is to be chosen. Reset
C the first line of the scanned documentation/help to compensate
C for the line used in ASKOK.
        if(act.eq.'r')then
          h(1)='The documentation could include useful tips!'
          lasth = MIN0(nbclmlhelp,60)
          do 1546 iasth=1,lasth
            call edisp(iuout,clmlhelp(iasth))
 1546     continue
          if(clmlavail(1:7).eq.'OFFLINE')then
            call usrmsg(
     & 'The selected climate file is not `ONLINE` and so you will have',
     & 'to ask your systems administrator to bring it on line. ','W')
            CALL EASKAB(sethead,' ','Exit clm','Select another ? ',IW,1)
            if(IW.eq.2)then
              clmdb='UNKNOWN'
              actreturn='m'
              return
            elseif(IW.eq.1)then
              clmdb='UNKNOWN'
              actreturn='f'
              return
            endif
          elseif(clmlavail(1:6).eq.'ONLINE')then
            CALL EASKAB(sethead,' ','Use it','Select another ? ',IW,1)
            if(IW.eq.2)then
              clmdb='UNKNOWN'
              actreturn='m'
              return
            elseif(IW.eq.1)then
              CALL ERPFREE(idblfil,ISTAT)
              actreturn='u'
              return
            endif
          endif
        elseif(act.eq.'s')then
          write(outs,'(a,a)') ' Climate is : ',clmlavail
          call edisp(iuout,outs)
          if(clmlavail(1:7).eq.'OFFLINE')then
            call usrmsg(
     & 'The selected climate file is not `ONLINE` and so you will have',
     & 'to ask your systems administrator to bring it on line. ','W')
            clmdb='UNKNOWN'
            actreturn='m'
            return
          elseif(clmlavail(1:6).eq.'ONLINE')then
            call edisp(iuout,
     &        'found matching seasons & asessment periods for')
            call edisp(iuout,clmdb)
            actreturn='e'
            return
          endif
        endif
      else
        actreturn='i'
        return
      endif

      end

C ************ editemclimatelist
C Edit/create an item for climatelist file. Data fields include
C five seasons and typical assessment periods, documentation
C strings.
      subroutine editemclimatelist()
#include "building.h"
      COMMON/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      COMMON/C22/ICLIM,LCLIM
      COMMON/CLMDT1/CLMLOC
      COMMON/CLMDT0/CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     &              CDIR(MT),CHUM(MT)
C Typical periods and season definitions.
      common/typper/ia1wins,ia1winf,ia1sprs,ia1sprf,
     &  iasums,iasumf,ia2sprs,ia2sprf,ia2wins,ia2winf
      common/typsea/is1wins,is1winf,is2wins,is2winf,is1sprs,is1sprf,
     &              is2sprs,is2sprf,is1sums,is1sumf
      common/clmltext/clmlnam,clmlaid,clmldbfile,clmlavail,clmlhelp(60)
      common/clmlnum/nbclmlhelp

C ITM is main item editing menu.
      character ITM(23)*40
      character NMTHNM(12)*3
      character descr*7,descrst*10,descrfn*10,h*72
      character clmlnam*32,clmlaid*72,clmldbfile*72,clmlavail*10
      character clmlhelp*72,ltfil*72,DFILE*72,outs*124
      character DESCRX*7,DESCR1X*10,DESCR1*10,hold*40
      CHARACTER CLMLOC*30,t32*32,d32*32,t72*72,d72*72,LCLIM*72
      
      DATA NMTHNM/'Jan','Feb','Mar','Apr','May','Jun','Jul',
     &'Aug','Sep','Oct','Nov','Dec'/

    3 INO=-4
      IIER=0

      WRITE(ITM(1),'(2A)')     'a menu str: ',clmlnam(1:28)
      WRITE(ITM(2),'(2A)')     'b menu aid: ',clmlaid(1:28)
      WRITE(ITM(3),'(2A)')     'c clm file: ',clmldbfile(1:28)
      WRITE(ITM(4),'(2A)')     'd available: ',clmlavail
      WRITE(ITM(5),'(A)')      ' ----seasons----------------------days'
      call stdate(iyear,is1wins,descr,descrst)
      call stdate(iyear,is1winf,descr,descrfn)
      jjd1=(is1winf-is1wins)+1
      WRITE(ITM(6),'(4a,i4)')  'e  1st win ',descrst,'  ',descrfn,jjd1
      call stdate(iyear,is1sprs,descr,descrst)
      call stdate(iyear,is1sprf,descr,descrfn)
      jjd2=(is1sprf-is1sprs)+1
      WRITE(ITM(7),'(4a,i4)')  'f  spring  ',descrst,'  ',descrfn,jjd2
      call stdate(iyear,is1sums,descr,descrst)
      call stdate(iyear,is1sumf,descr,descrfn)
      jjd3=(is1sumf-is1sums)+1
      WRITE(ITM(8),'(4a,i4)')  'g  summer  ',descrst,'  ',descrfn,jjd3
      call stdate(iyear,is2sprs,descr,descrst)
      call stdate(iyear,is2sprf,descr,descrfn)
      jjd4=(is2sprf-is2sprs)+1
      WRITE(ITM(9),'(4a,i4)')  'h  autumn  ',descrst,'  ',descrfn,jjd4
      call stdate(iyear,is2wins,descr,descrst)
      call stdate(iyear,is2winf,descr,descrfn)
      jjd5=(is2winf-is2wins)+1
      WRITE(ITM(10),'(4a,i4)') 'i  2nd win ',descrst,'  ',descrfn,jjd5
      WRITE(ITM(11),'(A)')     ' ----typical periods--------------days'
      call stdate(iyear,ia1wins,descr,descrst)
      call stdate(iyear,ia1winf,descr,descrfn)
      jjd1=(ia1winf-ia1wins)+1
      WRITE(ITM(12),'(4a,i4)') 'j  1st win ',descrst,'  ',descrfn,jjd1
      call stdate(iyear,ia1sprs,descr,descrst)
      call stdate(iyear,ia1sprf,descr,descrfn)
      jjd2=(ia1sprf-ia1sprs)+1
      WRITE(ITM(13),'(4a,i4)') 'k  spring  ',descrst,'  ',descrfn,jjd2
      call stdate(iyear,iasums,descr,descrst)
      call stdate(iyear,iasumf,descr,descrfn)
      jjd3=(iasumf-iasums)+1
      WRITE(ITM(14),'(4a,i4)') 'l  summer  ',descrst,'  ',descrfn,jjd3
      call stdate(iyear,ia2sprs,descr,descrst)
      call stdate(iyear,ia2sprf,descr,descrfn)
      jjd4=(ia2sprf-ia2sprs)+1
      WRITE(ITM(15),'(4a,i4)') 'm  autumn  ',descrst,'  ',descrfn,jjd4
      call stdate(iyear,ia2wins,descr,descrst)
      call stdate(iyear,ia2winf,descr,descrfn)
      jjd5=(ia2winf-ia2wins)+1
      WRITE(ITM(16),'(4a,i4)') 'n  2nd win ',descrst,' ',descrfn,jjd5
      WRITE(ITM(17),'(A)')     '< scan climate for best-fit weeks'
      WRITE(ITM(18),'(A,i2,a)')' ----documentation (',nbclmlhelp,
     &  ')----------'
      WRITE(ITM(19),'(A)')     'o list/generate/edit documentation'
      WRITE(ITM(20),'(A)')     ' _________________________________'
      WRITE(ITM(21),'(A)')     '> save                            '
      WRITE(ITM(22),'(A)')     '? help                            '
      WRITE(ITM(23),'(A)')     '- end                             '
      nitms=23
      CALL EMENU('Climate list item',ITM,nitms,INO)
      if(INO.EQ.nitms)then
        return
      elseif(INO.eq.1)then

C String for the selection menu.
        H(1)='Text for the selection list menu (<=32 char). '
        t32=clmlnam
        if(t32(1:15).eq.'not yet entered')write(t32,'(2a)') CLMLOC,'  '
        write(d32,'(2a)') CLMLOC,'  '
        CALL EASKS(t32,'Text for the selection list menu',' ',32,
     &    d32,'clmlist name',IER,1)
        if(t32(1:2).ne.'  ')clmlnam=t32
      elseif(INO.eq.2)then

C Aide memorie for the menu.
        H(1)='Aide Memorie for the selection list menu (<=72 char). '
        t72=clmlaid
        if(t72(1:15).eq.'not yet entered')write(t72,'(2a)') CLMLOC,
     &    ' was sourced from ... '
        write(d72,'(2a)') CLMLOC,' was sourced from... '
        CALL EASKS(t72,'Aide memorie for the selection list menu',' ',
     &    72,d72,'clmlist aide',IER,1)
        if(t72(1:2).ne.'  ')clmlaid=t72
      elseif(INO.eq.3)then

C Installed location of climate file.
        H(1)='This is the `installed` location of the climate file.'
        h(2)='(remember to copy new climate files to this folder)'
        t72=clmldbfile
        if(t72(1:2).eq.'  ') t72 = LCLIM
        d72 = LCLIM
        CALL EASKS(t72,'associated climate file (installed location)',
     &    ' ',72,d72,'clm file',IER,2)
        if(t72(1:2).ne.'  ')clmldbfile=t72
      elseif(INO.eq.4)then

C Toggle for available << ? check for file existance >>
        h(1)='Set to OFFLINE if climate files tend to be kept as'
        h(2)='compressed files or are held in a secure location. '
        call easkabc('Choices:',' ','online','offline','continue',IW,2)
        if(iw.eq.1) clmlavail = 'ONLINE'
        if(iw.eq.2) clmlavail = 'OFFLINE'
      elseif(INO.eq.6)then

C 1st winter season
C Edit the current period in terms of day and month. Also check to
C see if next period overlaps and correct.
   91   hold = ' '
        H(1)='Season start day & month and finish day and month. '
        call stdate(iyear,is1wins,descr,descrst)
        call stdate(iyear,is1winf,descr,descrfn)
        WRITE(outs,'(4a)')  ' 1st winter ',descrst,' - ',descrfn
        call edisp(iuout,outs)
        call edayr(is1wins,id1,im1)
        call edayr(is1winf,id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Season: start (day & month) and',
     &    'finish (day & month):',40,' 1 1 28 2','season',IIER,1)
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','season st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','season st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','season fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','season fn month',IIER)
        CALL EDAY(id1,im1,is1wins)
        CALL EDAY(id2,im2,is1winf)
        if(is1sprs.eq.is1winf+1)then
          continue
        else
          is1sprs=is1winf+1
        endif
        if(iier.ne.0)goto 91
      elseif(INO.eq.7)then

C Spring season.
C Edit the current period in terms of day and month.
   92   hold = ' '
        H(1)='Season start day & month and finish day and month. '
        call stdate(iyear,is1sprs,descr,descrst)
        call stdate(iyear,is1sprf,descr,descrfn)
        WRITE(outs,'(4a)')  ' Spring ',descrst,' - ',descrfn
        call edisp(iuout,outs)
        call edayr(is1sprs,id1,im1)
        call edayr(is1sprf,id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Season: start (day & month) and',
     &    'finish (day & month):',40,' 1 3 30 4','season',IIER,1)
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','season st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','season st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','season fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','season fn month',IIER)
        CALL EDAY(id1,im1,is1sprs)
        CALL EDAY(id2,im2,is1sprf)
        if(is1sums.eq.is1sprf+1)then
          continue
        else
          is1sums=is1sprf+1
        endif
        if(iier.ne.0)goto 92
      elseif(INO.eq.8)then

C Summer season.
C Edit the current period in terms of day and month.
   93   hold = ' '
        H(1)='Season start day & month and finish day and month. '
        call stdate(iyear,is1sums,descr,descrst)
        call stdate(iyear,is1sumf,descr,descrfn)
        WRITE(outs,'(4a)')  ' Summer ',descrst,' - ',descrfn
        call edisp(iuout,outs)
        call edayr(is1sums,id1,im1)
        call edayr(is1sumf,id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Season: start (day & month) and',
     &    'finish (day & month):',40,' 1 5 31 8','season',IIER,1)
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','season st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','season st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','season fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','season fn month',IIER)
        CALL EDAY(id1,im1,is1sums)
        CALL EDAY(id2,im2,is1sumf)
        if(is2sprs.eq.is1sumf+1)then
          continue
        else
          is2sprs=is1sumf+1
        endif
        if(iier.ne.0)goto 93
      elseif(INO.eq.9)then

C Autumn season.
C Edit the current period in terms of day and month.
   94   hold = ' '
        H(1)='Season start day & month and finish day and month. '
        call stdate(iyear,is2sprs,descr,descrst)
        call stdate(iyear,is2sprf,descr,descrfn)
        WRITE(outs,'(4a)')  ' Autumn ',descrst,' - ',descrfn
        call edisp(iuout,outs)
        call edayr(is2sprs,id1,im1)
        call edayr(is2sprf,id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Season: start (day & month) and',
     &    'finish (day & month):',40,' 1 9 31 10','season',IIER,1)
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','season st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','season st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','season fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','season fn month',IIER)
        CALL EDAY(id1,im1,is2sprs)
        CALL EDAY(id2,im2,is2sprf)
        if(is2wins.eq.is2sprf+1)then
          continue
        else
          is2wins=is2sprf+1
        endif
        if(iier.ne.0)goto 94
      elseif(INO.eq.10)then

C 2nd winter season.
C Edit the current period in terms of day and month.
   95   hold = ' '
        H(1)='Season start day & month and finish day and month. '
        call stdate(iyear,is2wins,descr,descrst)
        call stdate(iyear,is2winf,descr,descrfn)
        WRITE(outs,'(4a)')  ' 2nd winter ',descrst,' - ',descrfn
        call edisp(iuout,outs)
        call edayr(is2wins,id1,im1)
        call edayr(is2winf,id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Season: start (day & month) and',
     &    'finish (day & month):',40,' 1 11 31 12','season',IIER,1)
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','season st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','season st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','season fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','season fn month',IIER)
        CALL EDAY(id1,im1,is2wins)
        CALL EDAY(id2,im2,is2winf)
        if(iier.ne.0)goto 95
      elseif(INO.eq.12)then

C 1st winter period
C Edit the current period in terms of day and month.
   96   hold = ' '
        H(1)='Period start day & month and finish day and month. '
        call stdate(iyear,ia1wins,descr,descrst)
        call stdate(iyear,ia1winf,descr,descrfn)
        WRITE(outs,'(4a)')  ' 1st winter typical ',descrst,' - ',
     &    descrfn
        call edisp(iuout,outs)
        call edayr(ia1wins,id1,im1)
        call edayr(ia1winf,id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Typical: start (day & month) and',
     &    'finish (day & month):',40,' 9 1 15 1','typical',IIER,1)
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','typical st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','typical st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','typical fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','typical fn month',IIER)
        CALL EDAY(id1,im1,ia1wins)
        CALL EDAY(id2,im2,ia1winf)
        if(iier.ne.0)goto 96
      elseif(INO.eq.13)then

C Spring period.
C Edit the current period in terms of day and month.
   97   hold = ' '
        H(1)='Period start day & month and finish day and month. '
        call stdate(iyear,ia1sprs,descr,descrst)
        call stdate(iyear,ia1sprf,descr,descrfn)
        WRITE(outs,'(4a)')  ' Spring typical ',descrst,' - ',
     &    descrfn
        call edisp(iuout,outs)
        call edayr(ia1sprs,id1,im1)
        call edayr(ia1sprf,id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Typical: start (day & month) and',
     &    'finish (day & month):',40,' 6 3 12 3','typical',IIER,1)
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','typical st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','typical st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','typical fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','typical fn month',IIER)
        CALL EDAY(id1,im1,ia1sprs)
        CALL EDAY(id2,im2,ia1sprf)
        if(iier.ne.0)goto 97
      elseif(INO.eq.14)then

C Summer period.
C Edit the current period in terms of day and month.
   98   hold = ' '
        H(1)='Period start day & month and finish day and month. '
        call stdate(iyear,iasums,descr,descrst)
        call stdate(iyear,iasumf,descr,descrfn)
        WRITE(outs,'(4a)')  ' Summer typical ',descrst,' - ',
     &    descrfn
        call edisp(iuout,outs)
        call edayr(iasums,id1,im1)
        call edayr(iasumf,id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Typical: start (day & month) and',
     &    'finish (day & month):',40,' 11 7 17 7','typical',IIER,1)
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','typical st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','typical st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','typical fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','typical fn month',IIER)
        CALL EDAY(id1,im1,iasums)
        CALL EDAY(id2,im2,iasumf)
        if(iier.ne.0)goto 98
      elseif(INO.eq.15)then

C Autumn period.
C Edit the current period in terms of day and month.
   99   hold = ' '
        H(1)='Period start day & month and finish day and month. '
        call stdate(iyear,ia2sprs,descr,descrst)
        call stdate(iyear,ia2sprf,descr,descrfn)
        WRITE(outs,'(4a)')  ' Autumn typical ',descrst,' - ',
     &    descrfn
        call edisp(iuout,outs)
        call edayr(ia2sprs,id1,im1)
        call edayr(ia2sprf,id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Typical: start (day & month) and',
     &    'finish (day & month):',40,' 2 10 8 10','typical',IIER,1)
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','typical st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','typical st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','typical fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','typical fn month',IIER)
        CALL EDAY(id1,im1,ia2sprs)
        CALL EDAY(id2,im2,ia2sprf)
        if(iier.ne.0)goto 99
      elseif(INO.eq.16)then

C 2nd winter period.
C Edit the current period in terms of day and month.
  100   hold = ' '
        H(1)='Period start day & month and finish day and month. '
        call stdate(iyear,ia2wins,descr,descrst)
        call stdate(iyear,ia2winf,descr,descrfn)
        WRITE(outs,'(4a)')  ' 2nd winter typical ',descrst,' - ',
     &    descrfn
        call edisp(iuout,outs)
        call edayr(ia2wins,id1,im1)
        call edayr(ia2winf,id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Typical: start (day & month) and',
     &    'finish (day & month):',40,' 20 11 26 11','typical',IIER,1)
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','typical st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','typical st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','typical fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','typical fn month',IIER)
        CALL EDAY(id1,im1,ia2wins)
        CALL EDAY(id2,im2,ia2winf)
        if(iier.ne.0)goto 100
      elseif(INO.eq.17)then

C Scan climate for best-fit weeks.
          call DDRADSUM('?')
      elseif(INO.eq.19)then

C Documentations text option.
        h(1)='The list option displays each of the lines of '
        h(2)='documentation. The initialise option creates a '
        h(3)='standard summary of the climate data. The Edit '
        h(4)='option is not yet enabled (save and use a text editor).'
        call easkabcd('Documentation options:',' ','List','Initialise',
     &    'Edit','Continue',idw,4)
        if(idw.eq.1)then
          if(nbclmlhelp.ge.0)then
            call edisp(iuout,' ')
            do 43 ij=1,nbclmlhelp
              call edisp(iuout,clmlhelp(ij))
  43        continue
          else
            call edisp(iuout,'There is no documentation.')
          endif
        elseif(idw.eq.2)then
          write(clmlhelp(1),'(2a)') 'Climate is ',clmloc 
          ACLAT=ABS(CLAT)
          ACLONG=ABS(CLONG)
          IF(CLONG.LT.0.0.AND.CLAT.GE.0.0)then
            WRITE(clmlhelp(2),102)ACLAT,ACLONG,IYEAR
 102        FORMAT(' Location:',F6.2,'N',F6.2,'W :',I5)
          elseif(CLONG.GE.0.0.AND.CLAT.LT.0.0)then
            WRITE(clmlhelp(2),103)ACLAT,ACLONG,IYEAR
 103        FORMAT(' Location:',F6.2,'S',F6.2,'E :',I5)
          elseif(CLONG.LT.0.0.AND.CLAT.LT.0.0)then
            WRITE(clmlhelp(2),104)ACLAT,ACLONG,IYEAR
 104        FORMAT(' Location:',F6.2,'S',F6.2,'W :',I5)
          else
            WRITE(clmlhelp(2),101)ACLAT,ACLONG,IYEAR
 101        FORMAT(' Location:',F6.2,'N',F6.2,'E :',I5)
          endif

C Use variant of code in clmmmm.F to generate text.
          write(clmlhelp(3),'(a)')
     &    ' Month    Minimum  Time          Maximum  Time         Mean'

C Initialise parameters for whole year.
          ID1=1
          IM1=1
          IT1=1
          ID2=31
          IM2=12
          IT2=24
          IDS=1
          IDF=365
          KDS=1
          KDF=24
          XMAXI=-1E+10
          XMINI=1E+10
          XMEANI=0
          HRTOT=0.

C INITIALISE PARAMETERS FOR MONTHLY TOTALS.
          MTH1=IM1
          XMAXM=-1E10
          XMINM=1E10
          XMEANM=0.
          HRM=0.

C Find day of week for start of the period.
          CALL EDAYR(IDS,IDAYN,IMTHN)
      
C Loop for each day, reading data from climate file.
          DO 10 I=IDS,IDF
            IDD=I
            CALL CLMGET(IDD)

C Set parameters for current day and analyse max, min mean.
            XMAXJ=-1E+10
            XMINJ=1E+10
            XMEANJ=0
            JS=KDS
            JF=KDF
            IF(I.EQ.IDS)JS=MAX0(KDS,IT1)
            IF(I.EQ.IDF)JF=MIN0(KDF,IT2)
            DAYL=JF-JS+1
            DO 20 J=JS,JF
              X=CTMP(J)
              IF(X.LE.XMAXJ)GO TO 2
              XMAXJ=X
              JTMX=J
   2          IF(X.GE.XMINJ)GO TO 4
              XMINJ=X
              JTMN=J
   4          XMEANJ=XMEANJ+X
   20       CONTINUE

C Convert day to D,M values and compute parameters for this week or month.
            II=I
            CALL EDAYR(II,IDD,IMM)
            CALL EWEEKD(IDD,IMM,IYEAR,IDWK)
            if(IMM.EQ.MTH1)GO TO 40

            XMEAN=XMEANM/HRM
            XMAX=XMAXM
            XMIN=XMINM
            CALL EDAYR(KDMN,KDN,KMN)
            call stdate(iyear,KDMN,DESCR,DESCR1)
            CALL EDAYR(KDMX,KDX,KMX)
            call stdate(iyear,KDMX,DESCRX,DESCR1X)
            line = 3+kmn
            write(clmlhelp(line),
     &        '(1x,a,f9.1,a,i2,2a,F11.1,a,i2,2a,f8.1)')
     &        NMTHNM(KMN),XMIN,' @',KTMN,'h00 ',DESCR1(1:6),
     &        XMAX,' @',KTMX,'h00 ',DESCR1X(1:6),XMEAN

C Update parameters for whole period and reinitialise
            IF(XMAXM.LE.XMAXI)GO TO 22
            XMAXI=XMAXM
            IDMX=KDMX
            ITMX=KTMX
  22        IF(XMINM.GE.XMINI)GO TO 24
            XMINI=XMINM
            IDMN=KDMN
            ITMN=KTMN
  24        XMEANI=XMEANI+XMEANM
            HRTOT=HRTOT+HRM
            XMAXM=-1E10
            XMINM=1E10
            XMEANM=0.
            HRM=0.
            MTH1=IMM
  40        CONTINUE

C Update parameters for this month and go to the next day.
            IF(XMAXJ.LE.XMAXM)GO TO 42
            XMAXM=XMAXJ
            KDMX=I
            KTMX=JTMX
  42        IF(XMINJ.GE.XMINM)GO TO 44
            XMINM=XMINJ
            KDMN=I
            KTMN=JTMN
  44        XMEANM=XMEANM+XMEANJ
            HRM=HRM+DAYL
   10     CONTINUE

C Now compute parameters for last month.
          XMEAN=XMEANM/HRM
          XMAX=XMAXM
          XMIN=XMINM
          CALL EDAYR(KDMN,KDN,KMN)
          call stdate(iyear,KDMN,DESCR,DESCR1)
          CALL EDAYR(KDMX,KDX,KMX)
          call stdate(iyear,KDMX,DESCRX,DESCR1X)
          line = 15
          write(clmlhelp(line),
     &      '(1x,a,f9.1,a,i2,2a,F11.1,a,i2,2a,f8.1)')
     &      NMTHNM(KMN),XMIN,' @',KTMN,'h00 ',DESCR1(1:6),
     &      XMAX,' @',KTMX,'h00 ',DESCR1X(1:6),XMEAN

C Update parameters for whole period.
          IF(XMAXM.LE.XMAXI)GO TO 62
          XMAXI=XMAXM
          IDMX=KDMX
          ITMX=KTMX
  62      IF(XMINM.GE.XMINI)GO TO 64
          XMINI=XMINM
          IDMN=KDMN
          ITMN=KTMN
  64      XMEANI=XMEANI+XMEANM
          HRTOT=HRTOT+HRM

C Now for the `all line'.
          XMEAN=XMEANI/HRTOT
          XMAX=XMAXI
          XMIN=XMINI
          CALL EDAYR(IDMN,KDMN,KMMN)
          call stdate(iyear,IDMN,DESCR,DESCR1)
          CALL EDAYR(IDMX,KDMX,KMMX)
          call stdate(iyear,IDMX,DESCRX,DESCR1X)
          line = 16
          write(clmlhelp(line),
     &      '(a,f6.1,a,i2,2a,F7.1,a,i2,2a,f8.1)')
     &      ' Annual',XMIN,' @',ITMN,'h00 ',DESCR1,
     &      XMAX,' @',ITMX,'h00 ',DESCR1X,XMEAN
          write(clmlhelp(17),'(a)') ' ---Seasons & typical periods---'
          call stdate(iyear,is1wins,descr,descrst)
          call stdate(iyear,is1winf,descr,descrfn)
          write(clmlhelp(18),'(4a)') 'Winter season is ',descrst,' - ',
     &      descrfn
          call stdate(iyear,ia1wins,descr,descrst)
          write(clmlhelp(19),'(2a)') 'Typical winter week begins ',
     &      descrst
          call stdate(iyear,is1sprs,descr,descrst)
          call stdate(iyear,is1sprf,descr,descrfn)
          write(clmlhelp(20),'(4a)') 'Spring season is ',descrst,' - ',
     &      descrfn
          call stdate(iyear,ia1sprs,descr,descrst)
          write(clmlhelp(21),'(2a)') 'Typical spring week begins ',
     &      descrst
          call stdate(iyear,is1sums,descr,descrst)
          call stdate(iyear,is1sumf,descr,descrfn)
          write(clmlhelp(22),'(4a)') 'Summer season is ',descrst,' - ',
     &      descrfn
          call stdate(iyear,iasums,descr,descrst)
          write(clmlhelp(23),'(2a)') 'Typical summer week begins ',
     &      descrst
          call stdate(iyear,is2sprs,descr,descrst)
          call stdate(iyear,is2sprf,descr,descrfn)
          write(clmlhelp(24),'(4a)') 'Autumn season is ',descrst,' - ',
     &      descrfn
          call stdate(iyear,ia2sprs,descr,descrst)
          write(clmlhelp(25),'(2a)') 'Typical autumn week begins ',
     &      descrst
          call stdate(iyear,is2wins,descr,descrst)
          call stdate(iyear,is2winf,descr,descrfn)
          write(clmlhelp(26),'(4a)') 'Winter season is ',descrst,' - ',
     &      descrfn
          call stdate(iyear,ia2wins,descr,descrst)
          write(clmlhelp(27),'(2a)') 'Typical winter week begins ',
     &      descrst
          nbclmlhelp=27
        elseif(idw.eq.3)then
          continue
        elseif(idw.eq.4)then
          continue
        endif
      elseif(INO.eq.21)then

C Save to file, confirm file name and open, call mkitemclimatelist
C and then close file.
        ltfil='/tmp/climateitem'
        DFILE=ltfil
        h(1)='Information on the current climate file (seasons etc.)'
        h(2)='will be written to a small file which can be copied into'
        h(3)='the /usr/esru/esp-r/climatelist file. '
        CALL EASKS(ltfil,' (new) file to write item data to? ',
     &     ' ',72,DFILE,'climate list item file',IER,3)
        iuf=ifil+1
        CALL EFOPSEQ(iuf,ltfil,4,IER)
        call mkitemclimatelist(iuf,ier)
        CALL ERPFREE(iuf,ISTAT)
      elseif(INO.eq.22)then

C << Include a note about how to take typical week information
C << generated elsewhere.
        h(1)='How to use the climate list facility: '
        h(2)=' '
        h(3)='Climate data resides in individual files and ESP includes'
        h(4)='a `climatelist` file which holds high level information'
        h(5)='about where climate these files are located as well as'
        h(6)='summary information on seasons and typical periods.'
        h(7)=' '
        h(8)='This menu allows you to set the definition of seasons'
        h(9)='during a year - winter @ start of year, spring, summer'
        h(10)='autumn, winter @ end of year (or summer @ start of year'
        h(11)='etc. at negative latitudes). One way to do this is to'
        h(12)='use your own criteria when scanning the heating and'
        h(13)='cooling degree day weekly reports, ambient temperature'
        h(13)='reports or graphs etc in this climate module. Then'
        h(14)='record the start and ending dates of each season.'
        h(15)=' '
        h(16)='This menu also allows you to specify typical assessment'
        h(17)='periods (one per season). The clm module includes an'
        h(18)='automaitc search for weeks which are closest to the'
        h(19)='seasonal values of (use weighted) heating and cooling'
        h(20)='degree days & solar radiation. If you use that facility' 
        h(21)='a typical week in each season will be found and can be'
        h(22)='saved into the `climatelist` file. '
        CALL PHELPD('clm list mgt',22,'-',0,0,IER)
      else
        INO=-4
        goto 3
      endif
      INO=-4
      goto 3
      
      end
 
C ************ mkitemclimatelist 
C Write an item for climatelist file to previously opened file on unit
C iuf, return ier=0 if ok. This item can either
C be as a fragment to be included in the climatelist file by
C the user or as part of an item management facility.
      subroutine mkitemclimatelist(iuf,ier)

C Typical periods and season definitions.
      common/typper/ia1wins,ia1winf,ia1sprs,ia1sprf,
     &  iasums,iasumf,ia2sprs,ia2sprf,ia2wins,ia2winf
      common/typsea/is1wins,is1winf,is2wins,is2winf,is1sprs,is1sprf,
     &              is2sprs,is2sprf,is1sums,is1sumf
      common/clmltext/clmlnam,clmlaid,clmldbfile,clmlavail,clmlhelp(60)
      common/clmlnum/nbclmlhelp

      character clmlnam*32,clmlaid*72,clmldbfile*72,clmlavail*10
      character clmlhelp*72,tab*1

      tab=CHAR(9) 
     
      write(iuf,'(a)')'*item'
      write(iuf,'(3a)')'*name',tab,clmlnam(1:lnblnk(clmlnam))
      write(iuf,'(3a)')'*aide',tab,clmlaid(1:lnblnk(clmlaid))
      write(iuf,'(3a)')'*dbfl',tab,clmldbfile(1:lnblnk(clmldbfile))
      write(iuf,'(3a)')'*avail',tab,clmlavail(1:lnblnk(clmlavail))
      call edayr(is1wins,id1,im1)
      call edayr(is1winf,id2,im2)
      call edayr(is2wins,id3,im3)
      call edayr(is2winf,id4,im4)
      write(iuf,'(a,8i3)')'*winter_s',id1,im1,id2,im2,id3,im3,id4,im4
      call edayr(is1sprs,id1,im1)
      call edayr(is1sprf,id2,im2)
      call edayr(is2sprs,id3,im3)
      call edayr(is2sprf,id4,im4)
      write(iuf,'(a,8i3)')'*spring_s',id1,im1,id2,im2,id3,im3,id4,im4
      call edayr(is1sums,id1,im1)
      call edayr(is1sumf,id2,im2)
      write(iuf,'(a,4i3)')'*summer_s',id1,im1,id2,im2
      call edayr(ia1wins,id1,im1)
      call edayr(ia1winf,id2,im2)
      call edayr(ia2wins,id3,im3)
      call edayr(ia2winf,id4,im4)
      write(iuf,'(a,8i3)')'*winter_t',id1,im1,id2,im2,id3,im3,id4,im4
      call edayr(ia1sprs,id1,im1)
      call edayr(ia1sprf,id2,im2)
      call edayr(ia2sprs,id3,im3)
      call edayr(ia2sprf,id4,im4)
      write(iuf,'(a,8i3)')'*spring_t',id1,im1,id2,im2,id3,im3,id4,im4
      call edayr(iasums,id1,im1)
      call edayr(iasumf,id2,im2)
      write(iuf,'(a,4i3)')'*summer_t',id1,im1,id2,im2
      write(iuf,'(a)')'*help_start'
      if(nbclmlhelp.eq.0)then
        write(iuf,'(a)')' Supporting text has yet to be entered.'
      else
        do 42 ij=1,nbclmlhelp
          write(iuf,'(a)') clmlhelp(ij)(1:lnblnk(clmlhelp(ij)))
  42    continue
      endif
      write(iuf,'(a)')'*help_end'

      return
      end

C ******* DDRADSUM *******
C DDRADSUM looks for typical week in each season based on closest
C degree days and radiation patterns. If act = `?` then user is
C asked to confirm whether the best-fit week is to be used in the
C climatelist.
      SUBROUTINE DDRADSUM(act)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTPCH/ICOUT
      common/pophelp/h(60)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/CLMDT0/CLAT,CLONG
      COMMON/RADTYP/IDNGH
      common/exporttgi/ixopen,ixloc,ixunit

      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     &              CDIR(MT),CHUM(MT)

C Season definitions. 2 periods for winter (i.e. nov-dec and
C jan-feb), transition (i.e. mar-may & sep-oct) and one period for summer.
      common/typsea/is1wins,is1winf,is2wins,is2winf,is1sprs,is1sprf,
     &              is2sprs,is2sprf,is1sums,is1sumf
      common/typper/ia1wins,ia1winf,ia1sprs,ia1sprf,
     &  iasums,iasumf,ia2sprs,ia2sprf,ia2wins,ia2winf

C dm* are the degree-day and day-ratio multipliers between the
C typical periods and the whole season for heating (DD), cooling (DD),
C lights (ratio), small power (ratio), fans (ratio), domestic hot
C water (ratio). For nipvassmt=1 these are initially set to 1.0.
C This information is potentially used to support IPV calculations.
      common/CLMDM/dmheat(MSPS),dmcool(MSPS),dmlight(MSPS),
     &             dmsmlpw(MSPS),dmfan(MSPS),dmdhw(MSPS)

      character H*72,outs*124,PERST1*14,PERST2*44,PERST3*44
      character DESCR*7,DESCR1*10,title*40,hold*32,act*1
      DIMENSION ALT(24),AZI(24),GHSUM(MSPS),DAYGLOB(MSPS)
      dimension seacddwk(MSPS),seahddtot(MSPS),seacddtot(MSPS)
      dimension seahddwk(MSPS)
      logical ok,dok

      if(is1wins.eq.0.or.is2wins.eq.0.or.is1sprs.eq.0)then
        call usrmsg('No winter|transition|summer season definitions',
     &    'found in the climate database. Skipping.','W')
        return
      endif

C Initial weightings for heading dd, cooling dd, solar radiation.
      hddw=1.0
      cddw=1.0
      radw=1.0

      H(1)='Degree day calculations are derived from a specific '
      H(2)='base temperature (deg C). '
      HBT=17.
      CALL EASKR(HBT,' ',' Heating base temperature ? ',
     &           -10.,'W',40.,'W',17.0,'DD heating base temp',IER,2)
      CBT=21.
      CALL EASKR(CBT,' ',' Cooling base temperature ? ',
     &           -10.,'W',40.,'W',21.0,'DD cooling base temp',IER,2)

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        call usrmsg(' Output being directed to file... ',' ','-')
      endif

C Reset text feedback.
      CALL CHEAD2(30)
      DNM=0.
      HRM=0.
      UPMC=0.
      HRMC=0.

      WRITE(outs,12)HBT,CBT
  12  FORMAT(' Degree day analysis: heating base at',F6.1,' & cooling',
     &       F6.1,' Deg C')
      call edisp(itru,outs)
      call edisp(itru,'  ')

C INITIALISE PARAMETERS FOR WHOLE PERIOD
      DNI=0
      HRTOT=0.
      UPIC=0
      HRTOTC=0.

C Loop through each seasonal period.
      do 42 k=1,5
        GHSUM(k)=0.
        DAYGLOB(k)=0.
        seahddwk(k)=0.
        seacddwk(k)=0.
        seahddtot(k)=0.
        seacddtot(k)=0.
        dmheat(k)=1.0
        dmcool(k)=1.0
        dmlight(k)=1.0
        dmsmlpw(k)=1.0
        dmfan(k)=1.0
        dmdhw(k)=1.0
        if(k.eq.1)then
          title='Winter (early year) season '
          DNI=0
          HRTOT=0.
          UPIC=0
          HRTOTC=0.
          IDSS=is1wins
          IDSF=is1winf
        elseif(k.eq.2)then
          title='Spring season '
          IDSS=is1sprs
          IDSF=is1sprf
        elseif(k.eq.3)then
          title='Summer season '
          IDSS=is1sums
          IDSF=is1sumf
        elseif(k.eq.4)then
          title='Autumn season '
          IDSS=is2sprs
          IDSF=is2sprf
        elseif(k.eq.5)then
          title='Winter (late year) season '
          IDSS=is2wins
          IDSF=is2winf
        endif
        CALL EPERSTR(IYEAR,IDSS,0,IDSF,24,1,
     &             IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)

C For each day read climate data, set parameters and analyse.
        DO 10 I=IDSS,IDSF
          IDD=I
          CALL CLMGET(IDD)
          CALL AZALT(IDD,1,CLONG,CLAT,ALT,AZI)
          DNJ=0
          UPJC=0.
          DAYL=24.0
          DO 20 J=1,24
            IF(ALT(J).LT.0.001)THEN
              DIRH=0.
            else
              IF(IDNGH.EQ.0)THEN
                DIRH=CDNR(J)*ESIND(ALT(J))
              ELSE
                DIRH=CDNR(J)-CDIF(J)
              ENDIF
              GHSUM(k)=(GHSUM(k)+DIRH+CDIF(J))
            endif

            TT=CTMP(J)
            IF(TT.GT.HBT)then
            else

C T below heating BASE,SAVE DEG-HRS IN DNJ
              DNJ=DNJ+(HBT-TT)
            endif
            IF(TT.GT.CBT)then

C T above cooling BASE ,SAVE DEG-HRS IN UPJC
              UPJC=UPJC+(TT-CBT)
            endif
   20     CONTINUE
          XDN=DNJ/24
          XUPC=UPJC/24

C Update seasonal parameters and go to the next day.
          DNM=DNM+DNJ
          HRM=HRM+DAYL
          UPMC=UPMC+UPJC
          HRMC=HRMC+DAYL
   10   CONTINUE

C Compute parameters (total & average degree days) for this season.
        XHR=24.0/HRM
        XHRC=24.0/HRMC
        XDN=DNM/24
        XUPC=UPMC/24
        YDN=XHR*XDN
        seahddwk(k)=YDN*7.
        YUPC=XHRC*XUPC
        seacddwk(k)=YUPC*7.

C Number of days in season is IDSF-IDSS+1. DAYGLOB is daily mean
C global horizontal solar for season.
        DAYGLOB(k)=(GHSUM(k)/(IDSF-IDSS+1))/1000.

C Report on the current season.
        WRITE(outs,'(3a)')title(1:lnblnk(title)),' ',PERST2
        call eddisp(itru,outs)
        WRITE(outs,'(a,F8.2,a,F8.2,a,F9.2)')' Heat DD avg/day   ',YDN,
     &    '  Cool DD avg/day  ',YUPC,'  Rad avg/day ',DAYGLOB(k)
        call eddisp(itru,outs)
        WRITE(outs,'(a,F8.2,a,F8.2,a,F9.2)')' Heat DD avg/week  ',
     &    seahddwk(k),'  Cool DD avg/week ',seacddwk(k),
     &    '  Rad avg/week',DAYGLOB(k)*7.
        call eddisp(itru,outs)
        WRITE(outs,'(a,F8.2,a,F8.2,a,F9.2)')' Season heat DD    ',XDN,
     &     '  Season cool DD   ',XUPC,'  Rad total   ',GHSUM(k)/1000.
        call eddisp(itru,outs)
        seahddtot(k)=XDN
        seacddtot(k)=XUPC

C UPDATE WHOLE PERIOD
        HRTOT=HRTOT+HRM
        DNI=DNI+DNM
        DNM=0.
        HRM=0.
        HRTOTC=HRTOTC+HRMC
        UPIC=UPIC+UPMC
        UPMC=0.
        HRMC=0.
  42  continue

C Loop through each week in each season and check against the
C calculated average data.
C Loop through each seasonal period.
      h(1)='Typical weeks are found by scanning the climate data'
      h(2)='for a week in each season which has the least deviation'
      h(3)='in heating degree days (DD), cooling degree days, and'
      h(4)='solar radiation. '
      h(5)=' '
      h(6)='It reports the parameters it uses to determine the'
      h(7)='best fit as well as several ratios between the best'
      h(8)='week and the season (ratio of julian days, ratio of'
      h(9)='heating degree days and ratio of cooling degree days'
      h(10)='all of which are parameters required for generating'
      h(11)='an integrated performance view (IPV).'
      call easkab('Options:',' ','search for typical weeks',
     &  'continue',iok,11)
      if(iok.eq.2)return

C Allow user to define weightings.
      H(1)='Weightings for heating degree days, cooling degree days'
      H(2)='and solar radiation can be given. The default is equal'
      H(3)='weightings for each.'
      write(hold,'(3f6.3)') hddw,cddw,radw
 143  CALL EASKS(HOLD,'Weightings for HDD CDD Solar:',' ',32,
     &  ' 1.0 1.0 1.0 ','weightings for hdd cdd solar',IER,3)
      K=0
      CALL EGETWR(HOLD,K,hddw,0.0,1.0,'W','hdd weight',IER)
      CALL EGETWR(HOLD,K,cddw,0.0,1.0,'W','cdd weight',IER)
      CALL EGETWR(HOLD,K,radw,0.0,1.0,'W','solar weight',IER)
      if(ier.ne.0)goto 143
      IWK=0
      do 43 k=1,5
        if(k.eq.1)then
          call edisp(itru,'Winter (early year) season.')
          IDSS=is1wins
          IDSF=is1winf
        elseif(k.eq.2)then
          call edisp(itru,'Spring season.')
          IDSS=is1sprs
          IDSF=is1sprf
        elseif(k.eq.3)then
          call edisp(itru,'Summer season.')
          IDSS=is1sums
          IDSF=is1sumf
        elseif(k.eq.4)then
          call edisp(itru,'Autumn season.')
          IDSS=is2sprs
          IDSF=is2sprf
        elseif(k.eq.5)then
          call edisp(itru,'Winter (late year) season.')
          IDSS=is2wins
          IDSF=is2winf
        endif
        iwkbest=0
        iwkbstrt=0
        wkdiff=3.
        GHSUMW=0.
        CALL EPERSTR(IYEAR,IDSS,0,IDSF,24,1,
     &             IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
        WRITE(outs,'(a)')PERST2
        call edisp(itru,outs)
        if(itru.ne.iuout)call edisp(iuout,outs)

C For each day read climate data, set parameters and analyse.
C Find day of week for start of the period. IWK is week number, IDSOW
C is the julian day at the start of the week.
        CALL EDAYR(IDSS,IDAYN,IMTHN)
        CALL EWEEKD(IDAYN,IMTHN,IYEAR,IXDWK)
        IDSOW=IDSS
        ix=0

C For each day read climate data, set parameters and analyse.
        DO 11 I=IDSS,IDSF
          IDD=I
          ix=ix+1
          CALL CLMGET(IDD)
          CALL AZALT(IDD,1,CLONG,CLAT,ALT,AZI)
          DNJ=0
          UPJC=0
          DO 21 J=1,24
            IF(ALT(J).LT.0.001)THEN
              DIRH=0.
            else
              IF(IDNGH.EQ.0)THEN
                DIRH=CDNR(J)*ESIND(ALT(J))
              ELSE
                DIRH=CDNR(J)-CDIF(J)
              ENDIF
              GHSUMW=(GHSUMW+DIRH+CDIF(J))
            endif

            TT=CTMP(J)
            IF(TT.GT.HBT)then
            else

C T below heating BASE,SAVE DEG-HRS IN DNJ
              DNJ=DNJ+(HBT-TT)
            endif
            IF(TT.GT.CBT)then

C T above cooling BASE ,SAVE DEG-HRS IN UPJC
              UPJC=UPJC+(TT-CBT)
            endif
   21     CONTINUE

C Convert day to D,M
          II=I
          CALL EDAYR(II,IDD,IMM)
          CALL EWEEKD(IDD,IMM,IYEAR,IDWK)

C Prepare for output, check if week complete. << IDS ? >>
          IF(II.eq.IDSS)GO TO 41
          IF(IDWK.ne.IXDWK)GO TO 41
          iwk=iwk+1

C Compute parameters (total & average degree days) for this week.
          XHR=24.0/HRM
          XHRC=24.0/HRMC
          XDN=DNM/24
          XUPC=UPMC/24
          call stdate(iyear,IDSOW,DESCR,DESCR1)
          YDN=XHR*XDN
          YUPC=XHRC*XUPC
          WRITE(outs,'(a,i2,3a,2F8.2,a,F8.3)')' Week:',iwk,
     &      ' starting ',DESCR1,' DD total/week  ',XDN,XUPC,
     &      '  Rad total/week ',GHSUMW/1000.
          call edisp(itru,outs)
          if(itru.ne.iuout)call edisp(iuout,outs)

C Deal with radiation.
          if(seahddwk(k).gt.0.001)then
            PARM1=ABS(XDN-seahddwk(k))/seahddwk(k)
          else
            PARM1=0.
          endif
          if(seacddwk(k).gt.0.001)then
            PARM2=ABS(XUPC-seacddwk(k))/seacddwk(k)
          else
            PARM2=0.
          endif
          PARM3=ABS((GHSUMW/1000.)-(DAYGLOB(k)*7.))/(DAYGLOB(k)*7.)
          PARMTOT=(PARM1*hddw)+(PARM2*cddw)+(PARM3*radw)
          WRITE(outs,'(a,3F9.3,a,F9.3)')' Params (heat|cool|solar)',
     &      PARM1,PARM2,PARM3,' Params total ',PARMTOT
          call edisp(itru,outs)
          if(PARMTOT.lt.wkdiff)then
            wkdiff=PARMTOT
            iwkbest=iwk
            iwkbstrt=IDSOW
            wkheatdd=XDN
            wkcooldd=XUPC
          endif

C Reset weekly total global horizontal.
          GHSUMW=0.

C Update whole period.
          IDSOW=II
          HRTOT=HRTOT+HRM
          DNI=DNI+DNM
          DNM=0.
          HRM=0.
          HRTOTC=HRTOTC+HRMC
          UPIC=UPIC+UPMC
          UPMC=0.
          HRMC=0.

C CONVERT TO DEG-DAYS PER DAY
  41      CONTINUE
          XDN=DNJ/24
          XUPC=UPJC/24

C UPDATE PARAMETERS FOR THIS week
          DNM=DNM+DNJ
          HRM=HRM+DAYL
          UPMC=UPMC+UPJC
          HRMC=HRMC+DAYL

C AND GO TO NEXT DAY
   11   CONTINUE

C NOW LAST week, compute parameters for last week
        XHR=24.0/HRM
        XHRC=24.0/HRMC
        XDN=DNM/24
        XUPC=UPMC/24
        iwk=iwk+1
        call stdate(iyear,IDSOW,DESCR,DESCR1)
        YDN=XHR*XDN
        YUPC=XHRC*XUPC
        WRITE(outs,'(a,i2,3a,2F8.2,a,F8.3)')' Week:',iwk,
     &    ' starting ',DESCR1,' DD total/week  ',XDN,XUPC,
     &    '  Rad total/week ',GHSUMW/1000.
        call edisp(itru,outs)
        if(itru.ne.iuout)call edisp(iuout,outs)

C Deal with radition here for last week of season.
        if(seahddwk(k).gt.0.001)then
          PARM1=ABS(XDN-seahddwk(k))/seahddwk(k)
        else
          PARM1=0.
        endif
        if(seacddwk(k).gt.0.001)then
          PARM2=ABS(XUPC-seacddwk(k))/seacddwk(k)
        else
          PARM2=0.
        endif
        PARM3=ABS((GHSUMW/1000.)-(DAYGLOB(k)*7.))/(DAYGLOB(k)*7.)
        PARMTOT=(PARM1*hddw)+(PARM2*cddw)+(PARM3*radw)
        WRITE(outs,'(a,3F9.3,a,F9.3)')' Params (heat|cool|solar)',
     &    PARM1,PARM2,PARM3,' Params total ',PARMTOT
        call edisp(itru,outs)
        if(PARMTOT.lt.wkdiff)then
          wkdiff=PARMTOT
          iwkbest=iwk
          iwkbstrt=IDSOW
          wkheatdd=XDN
          wkcooldd=XUPC
        endif
        call stdate(iyear,iwkbstrt,DESCR,DESCR1)
        WRITE(outs,'(a,i3,3a,F6.2)')' Best week is nb ',iwkbest,' ',
     &    DESCR1,' with minimum weighted difference of ',wkdiff
        call edisp(itru,outs)
        if(itru.ne.iuout)call edisp(iuout,outs)

C Convert from julian day of the start of the assessment to the end
C of the assessment to number of days in period and get ratio with
C the number of days in the season.
        ijulst=iwkbstrt
        ijulfn=iwkbstrt+6

C Ratio of seasonal totoal heating and cooling degree days and the
C best assessment week heating and cooling degree days.
        if(wkheatdd.gt.0.01)then
          hddratio=seahddtot(k)/wkheatdd
        else
          hddratio=1.0
        endif
        if(wkcooldd.gt.0.01)then
          cddratio=seacddtot(k)/wkcooldd
        else
          cddratio=1.0
        endif
        if(k.eq.1)then
          dayratio=((is1winf-is1wins)+1)/((ijulfn-ijulst)+1)
          write(outs,'(a,F6.3)') 
     &      'early winter ratio of season/assessment days: ',dayratio
          call edisp(iuout,outs)
          write(outs,'(a,F6.3,A,F6.3)') 
     &      'early winter ratio of season/assessment heat DD: ',
     &      hddratio,' & cool DD:',cddratio
          call edisp(iuout,outs)
          if(act.eq.'?')then
            dok=.true.
            h(1)='Unless you have better information, say yes. '
            call askok('Use best-fit week for the 1st winter season',
     &       'typical assessment period in the climatelist?',ok,dok,1)
            if(ok)then
              ia1wins = iwkbstrt
              ia1winf = iwkbstrt+6
            endif
          endif
        elseif(k.eq.2)then
          dayratio=((is1sprf-is1sprs)+1)/((ijulfn-ijulst)+1)
C Debug...
          write(outs,'(a,F6.3)') 
     &      'spring ratio of season/assessment days: ',dayratio
          call edisp(iuout,outs)
          write(outs,'(a,F6.3,A,F6.3)') 
     &      'spring ratio of season/assessment heat DD: ',
     &      hddratio,' & cool DD:',cddratio
          call edisp(iuout,outs)
          if(act.eq.'?')then
            dok=.true.
            h(1)='Unless you have better information, say yes. '
            call askok('Use best-fit week for the spring season',
     &       'typical assessment period in the climatelist?',ok,dok,1)
            if(ok)then
              ia1sprs = iwkbstrt
              ia1sprf = iwkbstrt+6
            endif
          endif
        elseif(k.eq.3)then
          dayratio=((is1sumf-is1sums)+1)/((ijulfn-ijulst)+1)
          write(outs,'(a,F6.3)') 
     &      'summer ratio of season/assessment days: ',dayratio
          call edisp(iuout,outs)
          write(outs,'(a,F6.3,A,F6.3)') 
     &      'summer ratio of season/assessment heat DD: ',
     &      hddratio,' & cool DD:',cddratio
          call edisp(iuout,outs)
          if(act.eq.'?')then
            dok=.true.
            h(1)='Unless you have better information, say yes. '
            call askok('Use best-fit week for the summer season',
     &       'typical assessment period in the climatelist?',ok,dok,1)
            if(ok)then
              iasums = iwkbstrt
              iasumf = iwkbstrt+6
            endif
          endif
        elseif(k.eq.4)then
          dayratio=((is2sprf-is2sprs)+1)/((ijulfn-ijulst)+1)
          write(outs,'(a,F6.3)') 
     &      'autumn ratio of season/assessment days: ',dayratio
          call edisp(iuout,outs)
          write(outs,'(a,F6.3,A,F6.3)') 
     &      'autumn ratio of season/assessment heat DD: ',
     &      hddratio,' & cool DD:',cddratio
          call edisp(iuout,outs)
          if(act.eq.'?')then
            dok=.true.
            h(1)='Unless you have better information, say yes. '
            call askok('Use best-fit week for the autumn season',
     &       'typical assessment period in the climatelist?',ok,dok,1)
            if(ok)then
              ia2sprs = iwkbstrt
              ia2sprf = iwkbstrt+6
            endif
          endif
        elseif(k.eq.5)then
          dayratio=((is2winf-is2wins)+1)/((ijulfn-ijulst)+1)
          write(outs,'(a,F6.3)') 
     &      'late winter ratio of season/assessment days: ',dayratio
          call edisp(iuout,outs)
          write(outs,'(a,F6.3,A,F6.3)') 
     &      'late winter ratio of season/assessment heat DD: ',
     &      hddratio,' & cool DD:',cddratio
          call edisp(iuout,outs)
          if(act.eq.'?')then
            dok=.true.
            h(1)='Unless you have better information, say yes. '
            call askok('Use best-fit week for the 2nd winter season',
     &       'typical assessment period in the climatelist?',ok,dok,1)
            if(ok)then
              ia2wins = iwkbstrt
              ia2winf = iwkbstrt+6
            endif
          endif
        endif

C For the current season set degree day or day ratios.
        dmheat(k)=hddratio
        dmcool(k)=cddratio
        dmlight(k)=dayratio
        dmsmlpw(k)=dayratio
        dmfan(k)=dayratio
        dmdhw(k)=dayratio
        if(act.eq.'?')then
          continue
        else
          h(1)='You might wish to record this information before going'
          h(2)='to the next season.'
          call easkab('Options:',' ','look at next season','continue',
     &      iok,2)
          if(iok.eq.2)return
        endif
        ix=1
 43   continue

      RETURN
      END

C *********** CHEAD2 (copy from esruclm/chead.F)
C Print climate heading for text based routines. Lines is the number
C of lines to include in the text portion of the display.
      SUBROUTINE CHEAD2(LINES)

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/appw/iappw,iappx,iappy
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      COMMON/CLMDT1/CLMLOC
      COMMON/CLMDT0/CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/GFONT/IFS,ITFS,IMFS

      CHARACTER*72 ETEXT,GTEXT
      CHARACTER*30 CLMLOC
      character PERST1*14,PERST2*44,PERST3*44

C If in type 8 terminal make the text display area larger before
C displaying the following data ( but only clear it if LINES != LIMTTY)
      if(LIMTTY.NE.LINES.AND.MMOD.EQ.8)then
#ifdef X11
        call userfonts(IFS,ITFS,IMFS)
#endif
        if(iappw.gt.0.and.iappw.lt.100)then
          menuchw = MAX0(int(27*iappw*0.01),20)
          LIMTTY=LINES
        else
          menuchw = 27
          LIMTTY=LINES
        endif

#ifdef X11
        call winclr
        CALL feedbox(menuchw,2,igfw,igfh)
        CALL opengdisp(menuchw,LIMTTY,2,igdw,igdh)
        CALL win3d(menuchw,10,22,5,3,igl,igr,igt,igb,igw,igwh)
#endif
      endif

      ACLAT=ABS(CLAT)
      ACLONG=ABS(CLONG)
      IF(CLONG.LT.0.0.AND.CLAT.GE.0.0)then
        WRITE(ETEXT,102)CLMLOC,ACLAT,ACLONG,IYEAR
 102    FORMAT(' Climate analysis:',1X,A30,':',F6.2,'N',F6.2,'W :',I5)
      elseif(CLONG.GE.0.0.AND.CLAT.LT.0.0)then
        WRITE(ETEXT,103)CLMLOC,ACLAT,ACLONG,IYEAR
 103    FORMAT(' Climate analysis:',1X,A30,':',F6.2,'S',F6.2,'E :',I5)
      elseif(CLONG.LT.0.0.AND.CLAT.LT.0.0)then
        WRITE(ETEXT,104)CLMLOC,ACLAT,ACLONG,IYEAR
 104    FORMAT(' Climate analysis:',1X,A30,':',F6.2,'S',F6.2,'W :',I5)
      else
        WRITE(ETEXT,101)CLMLOC,ACLAT,ACLONG,IYEAR
 101    FORMAT(' Climate analysis:',1X,A30,':',F6.2,'N',F6.2,'E :',I5)
      endif
      GTEXT=ETEXT
      if(MMOD.gt.0)then
        CALL viewtext(GTEXT,1,0,1)
      else
        call edisp(iuout,GTEXT)
      endif

      NTSPH=1
      CALL EPERSTR(IYEAR,IDS,IT1,IDF,IT2,NTSPH,
     &             IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
      write(ETEXT,'(1X,A44)')PERST2
      GTEXT=ETEXT
      if(MMOD.gt.0)then
        CALL viewtext(GTEXT,2,0,1)
      else
        call edisp(iuout,GTEXT)
      endif

      RETURN
      END
