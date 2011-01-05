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

C This file contains the following routines:
C  MOTABL Produces tabular output for a 24 hour period.
C  ZONTAB Multi-column tabular listing for performance metrics.
C  CZMRT  returns one day's temperatures of zone mean radiant temp.
C  CZRESL returns one day's temperatures of zone resultant temp.

C ******************** MOTABL ********************
C MOTABL produces tabular output for a 24 hour period.
      SUBROUTINE MOTABL
#include "building.h"
#include "net_flow.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C default file names for databases
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      COMMON/OUTPCH/ICOUT

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/ZONPIK/NZ,NZNO(MCOM)

C ihflag = 0 write 13h30, ihflag = 1 write 0.5625
C idhflg = 0 no day demarcations, idhflg = 1 write demarcation
C          between tabular reporting days.
C ilflag = 0 tabular labels on multi-lines, ilflag = 1 on one line.
      COMMON/GRTOOL/IHFLAG,IDHFLG,ILFLAG
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      common/prec7/itcnst

C Special materials file flag
      common/spmfxst/ispmxist,spflnam  
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG 
      common/getmenu/menutype,igetind(65),igetflux(65)

      CHARACTER*23 ITEM(15)
      character outs*124,xfile*144,H*72,tg*1,delim*1,spflnam*72
      integer IWM  ! for radio button
      integer NITMS,INO ! max items and current menu item

C Default value for GRTOOL file = 0 (off), day hash marks off.
      IHFLAG=0
      IDHFLG=0
      ILFLAG=0

  503 ITEM(1)   ='2 select result set    '
      ITEM(2)   ='3 define period        '
      ITEM(3)   ='4 select zones         '
      ITEM(4)   ='  ___________________  '
      ITEM(5)   ='g performance metrics  '
      ITEM(6)   ='h special material data'
      ITEM(7)   ='i network air/wtr flow '
      ITEM(8)   ='  ___________________  '
      ITEM(9)   ='  formatting...        '
      if(ixopen.eq.1)then
        ITEM(10)='> output >> file       '
      elseif(ixopen.eq.0)then
        ITEM(10)='> output >> screen     '
      endif
      if(IHFLAG.eq.0)then
        ITEM(11)='* time >> 10h30        '
      elseif(IHFLAG.eq.1)then
        ITEM(11)='* time >> 0.4375       '
      endif
      if(ILFLAG.eq.0)then
        ITEM(12)='* labels >> multiline  '
      elseif(ILFLAG.eq.1)then
        ITEM(12)='* labels >> on one line'
      endif
      if(delim.eq.'-')then
        ITEM(13)='^ delim >> normal      '
      elseif(delim.eq.'T')then
        ITEM(13)='^ delim >> TAB         '
      elseif(delim.eq.'C')then
        ITEM(13)='^ delim >> comma       '
      elseif(delim.eq.'S')then
        ITEM(13)='^ delim >> space       '
      elseif(delim.eq.'X')then
        ITEM(13)='^ delim >> tagged      '
      endif
      ITEM(14)  ='? help                 '
      ITEM(15)  ='- exit                 '
      NITMS=15
      INO=-2

C Instanciate h() string array.
      H(1) ='The output format selection allows you to direct '
      H(2) ='tabular listing to the standard text feedback area '
      H(3) ='or to a named file.  You may also define the time  '
      H(4) ='format for use in 3rd party graphing facilities.  '

  504 CALL EMENU('Tabular Output',ITEM,NITMS,INO)

C Test for illegal menu pick.
      IF(INO.EQ.1)then
        CALL MORESS
      elseif(INO.EQ.2)then
        CALL MOOPER
      elseif(INO.EQ.3)then
        CALL MOZDFN
      elseif(INO.EQ.5)then
        if(ISAVE.GT.1)then
          CALL ZONTAB('-')
        else
          call edisp(iuout,'Not available with save level 1')
        endif
      elseif(INO.EQ.6)then

C Special material output. << use alternative data recovery >>.

      elseif(INO.EQ.7)then

C Network flow tabular output.
        MENUTYPE=4
        call MFOUTP('-')
      elseif(INO.EQ.10.AND.ISAVE.GE.1)then
        call ctlexp(xfile,ixopen,ixunit,ixpunit,'X','Tabular',IER)
      elseif(INO.EQ.11.AND.ISAVE.GE.1)then

C Toggle time format and day separators.
        IHFLAG=IHFLAG+1
        if(IHFLAG.GT.1)IHFLAG=0
        if(IHFLAG.eq.0)then
          call edisp(iuout,' ')
          call edisp(iuout,'standard display time = 10h30 set ')
        elseif(IHFLAG.eq.1)then
          call edisp(iuout,' ')
          call edisp(iuout,'3rd party graphing time = day fraction set')
        endif
        H(1)='Places an # separator between days of tabular data.'
        CALL EASKAB('Include mark between days when ',
     &              'displaying or writing data :','no','yes',IDH,1)
        IDHFLG=IDH-1
      elseif(INO.EQ.12.AND.ISAVE.GE.1)then

C Toggle tabular listing label format.
        ILFLAG=ILFLAG+1
        if(ILFLAG.GT.1)ILFLAG=0
        if(ILFLAG.eq.0)then
          call edisp(iuout,' ')
          call edisp(iuout,'column labels on multi-lines ')
        elseif(ILFLAG.eq.1)then
          call edisp(iuout,' ')
          call edisp(iuout,'column labels on one line (spreadsheets)')
        endif
      elseif(INO.EQ.13.AND.ISAVE.GE.1)then

C Toggle delimeter.
        H(1) ='Tabular data can be sent to file with various '
        H(2) ='delimiters: '
        H(3) ='  spaces (format using spaces to lineup columns)'
        H(4) ='  single space between columns'
        H(5) ='  comma separator (for excel)'
        H(6) ='  tab separator (for excel)'
        H(7) ='  tagged - marked up data'
        IWM=1
        CALL EASKATOG('Delimeter to use between columns of data:',' ',
     &    'normal spaces','single space','tab','comma','tagged',
     &    'continue',' ',IWM,7)
        if(iwm.eq.1)then
          delim = '-'
        elseif(iwm.eq.2)then
          delim = 'S'
        elseif(iwm.eq.3)then
          delim = 'T'
        elseif(iwm.eq.4)then
          delim = 'C'
        elseif(iwm.eq.5)then
          delim = 'X'
        endif
      elseif(INO.EQ.14)then
        CALL PHELPD('tabular',4,'-',0,0,IER)
      elseif(INO.GE.15)then
        RETURN
      else

C Output menu error signal and allow reselection from menu.
       INO=-1
        goto 504
      endif
      goto 503

      END

C ******************** ZONTAB
C ZONTAB A multi column tabular listing for performance metrics
C such as found in TGRAPH and STATS.
C act='p' iget parameters already set.
C act='m' called from ipv metric with parameters already set.

      SUBROUTINE ZONTAB(act)
#include "building.h"
#include "ipvdata.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTPCH/ICOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

      COMMON/IGETFLG/IOCUPF,ialstused,IROC
      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      common/getmenu/menutype,igetind(65),igetflux(65)
      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS)
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/SETNAM/RSNAME(MNRS)
      COMMON/SIMPKA/NSIM

      COMMON/GRTOOL/IHFLAG,IDHFLG,ILFLAG
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

      character outs*124,outs248*248 ! string buffers
      character outs400*400,outs400d*400  ! really long string buffers
      character act*1,sq*1,unit*7,t20*20,dg*1
      character SLABEL*32,GLABEL*20,LTIME*5,LJTIME*8
      character prompt*124,tg*1,delim*1,xfile*144
      CHARACTER PDESCR*60,SDESCR*44,RSNAME*40,DESCR*7,DESCR1*10
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      integer lnb   ! length of buffer to write to

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        if(NGET.ge.1)then
          write(prompt,'(a,a)') SLABEL(1)(1:lnblnk(SLABEL(1))),
     &      ' >> file.'
        else
          write(prompt,'(a)')' Output being directed to file. ' 
        endif
        call edisp(icout,prompt)
      endif

C Make up a single quote for IPV
      sq = char(39)

C Call the menu of choices (this also sets some default options).
 1    if(act(1:1).eq.'-')then
        MENUTYPE=4
        call GOMSETUP
        call GOMENU
        if(ixopen.eq.1)then
          itru = ixunit
          if(NGET.ge.1)then
            write(prompt,'(a,a)') SLABEL(1)(1:lnblnk(SLABEL(1))),
     &       ' >> file.'
          else
            write(prompt,'(a)')' Output being directed to file. ' 
          endif
          call edisp(icout,prompt)
        else
          itru = icout
        endif
        if (MENUTYPE.eq.-1) return
      endif
      IINLVDT=0

C Call GOGET for each output day to get required data.
C GOGET recovers the data in VAL2 (and averages output if required.)
C Set start and finish times.
C TSTART and TFINISH - start and finish times in hours from IOH1 on the 
C first day of output.
      TSTART=FLOAT(IOH1)
      TFINSH=FLOAT(((IODF)*24+IOH2)-(IODS)*24)

C NDTS - the number of timesteps in a day.
      NDTS=24*NTS

C Write tag line if tagged output requested.
      if (delim.eq.'X') then
        write(outs,'(a)')'*table'
        call edisp(itru,outs)
        write(outs,'(a)')'*col 1 time'
        call eddisp(itru,outs)
        do 15 IL=1,NGET
          write(outs,'(a,6i3)')'*col ',IL+1,(IGETNO(IL,ILL),ILL=1,5)
          call eddisp(itru,outs)
 15     continue
      endif
      
C Write general header information (skip if IPV mode).
      if(act.ne.'m')then
        CALL HDDATE(PDESCR)
        CALL HDSTEP(SDESCR)
        
C If requested or writing to file start lines with a '#'.
        if (IHFLAG.eq.1.or.ixopen.eq.1) then
          call edisp(itru,'# Timestep performance metrics.')
          lsn1=MIN0(lnblnk(RFILE),36)
          if(NSIM.gt.1)then
            write(outs248,'(3A,I4,2A)')'# Lib: ',RFILE(1:lsn1),' Set:',
     &      ISIM,': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
          else
            write(outs248,'(4A)')'# Lib: ',RFILE(1:lsn1),
     &        ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
          endif
          if(ixopen.eq.1)then
            lnb=lnblnk(outs248)   ! write directly to file
            write(itru,'(A)',iostat=ios,err=1)outs248(1:lnb)
          else
            lnb=MIN0(lnblnk(outs248),124)
            if(lnb.lt.72)then
              call edisp(itru,outs248(1:lnb))
            else
              call edisp248(itru,outs248,lnb)
            endif
          endif
          WRITE(outs,'(4A)')'# ',PDESCR(1:lnblnk(PDESCR)),' ',
     &      SDESCR(11:lnblnk(SDESCR))
          call edisp(itru,outs)
        else
          call edisp(itru,'Timestep performance metrics.')
          lsn1=MIN0(lnblnk(RFILE),36)
          if(NSIM.gt.1)then
            write(outs248,'(3A,I4,2A)')'Lib: ',RFILE(1:lsn1),' Set:',
     &        ISIM,': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
          else
            write(outs248,'(4A)')'Lib: ',RFILE(1:lsn1),
     &        ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
          endif
          if(ixopen.eq.1)then
            lnb=lnblnk(outs248)   ! write directly to file
            write(itru,'(A)',iostat=ios,err=1)outs248(1:lnb)
          else
            lnb=MIN0(lnblnk(outs248),124)
            if(lnb.lt.72)then
              call edisp(itru,outs248(1:lnb))
            else
              call edisp248(itru,outs248,lnb)
            endif
          endif
          WRITE(outs,'(3A)')PDESCR(1:lnblnk(PDESCR)),' ',
     &      SDESCR(11:lnblnk(SDESCR))
          call edisp(itru,outs)
        endif
      endif

      do 10 IDAY=IODS,IODF

C Write day of results and column headings information (skip if IPV mode).
        CALL STDATE(IYEAR,IDAY,DESCR,DESCR1)
        if(act.ne.'m')then
          if (IDHFLG.eq.1.and.ixopen.eq.1) then
            outs='# '
            call edisp(itru,outs)
            write(outs,'(3A)')'# ',DESCR1,' timestep misc. listing'
            call edisp(itru,outs)
            outs='# '
            call edisp(itru,outs)
          elseif (IDHFLG.eq.0.and.ixopen.eq.1) then
            continue
          elseif (IDHFLG.eq.0.and.ixopen.eq.0) then
            continue
          elseif (IDHFLG.eq.1.and.ixopen.eq.0) then
            outs='  '
            call edisp(itru,outs)
            write(outs,'(3A)')' ',DESCR1,' timestep misc. listing'
            call edisp(itru,outs)
          else
            outs='  '
            call edisp(itru,outs)
          endif
          
C If printing to screen then split column headings over three rows 
C   don't do this if printing to file.
          irows=3
          if (ixopen.eq.1) irows=1
          is=1
          do 100 ir=1,irows
            K=6   ! initial value for 248 buffer
            K4=6  ! initial value for 400 buffer
            if (ir.gt.1) then
              outs248='     '  ! clear for 248 buffer
              outs400='     '  ! clear for 400 buffer
            else
              if (IHFLAG.eq.1.and.ixopen.eq.1) then
                if (delim.eq.'X') then
                  outs248='*time'
                  outs400='*time'
                else
                  outs248='#Time'
                  outs400='#Time'
                endif
              elseif (IHFLAG.eq.1.and.ixopen.eq.0) then
                if (delim.eq.'X') then
                  outs248='*time'
                  outs400='*time'
                else
                  outs248='Time'
                  outs400='Time'
                endif
              else
                outs248='Time '
                outs400='Time '
              endif
            endif
            do 20 IL=1,NGET
            
C Define units.
              if (IGETNO(IL,8).eq.1) then
                unit='(degC)'
              elseif (IGETNO(IL,8).eq.2) then
                if(IGETNO(IL,1).eq.8.or.IGETNO(IL,1).eq.9.or.
     &             IGETNO(IL,1).eq.10.or.IGETNO(IL,1).eq.30.or.
     &             IGETNO(IL,1).eq.31.or.IGETNO(IL,1).eq.32.or.
     &             IGETNO(IL,1).eq.33) then
                  unit='(kW)'
                else
                  unit='(W)'
                endif
              elseif (IGETNO(IL,8).eq.3) then
                unit='(W/m^2)'
              else
                unit='(-)'
              endif
              
C Set end character for output string (KE or KE4) and for section of 
C  glabel (ie) to be printed (if to screen).
              KE=K+(33/irows)   ! set for 248
              KE4=K4+(33/irows) ! set for 400
              ie=is+9
              if(KE.le.248)then
                
C If writing to file remove blanks from label.
                if (ixopen.eq.1) then
                  call SDELIM(GLABEL(IL),t20,'N',IW)
                  write (outs248(K:KE),'(3a)') ' ',t20(1:lnblnk(t20)),
     &              unit
                  K=K+lnblnk(t20)+lnblnk(unit)+1
                else
                  if (ir.lt.3) then
                    write (outs248(K:KE),'(2a)') '|',GLABEL(IL)(is:ie)
                  else
                    write (outs248(K:KE),'(2a)') '| ',unit
                  endif
                  K=KE
                endif
              endif

C For writing to file also fill the 400 char buffer
              if(KE4.le.400)then
                if (ixopen.eq.1) then
                  call SDELIM(GLABEL(IL),t20,'N',IW)
                  write (outs400(K4:KE4),'(3a)') ' ',t20(1:lnblnk(t20)),
     &              unit
                  K4=K4+lnblnk(t20)+lnblnk(unit)+1
                else
                  if (ir.lt.3) then
                    write (outs400(K4:KE4),'(2a)') '|',GLABEL(IL)(is:ie)
                  else
                    write (outs400(K4:KE4),'(2a)') '| ',unit
                  endif
                  K4=KE4
                endif
              endif
 20         continue
            is=ie+1

C Print titles on first day and only on subsequent ones if day
C demarcations are omitted.
            lnb=lnblnk(outs248)+1   ! ensure we do not warp line
            if (IDAY.eq.IODS)then
              if(ixopen.eq.1)then
                lnb=lnblnk(outs400)   ! write directly to file
                write(itru,'(A)',iostat=ios,err=1)outs400(1:lnb)
              else
                lnb=MIN0(lnblnk(outs248),124)
                if(lnb.lt.72)then
                  call edisp(itru,outs248(1:lnb))
                else
                  call edisp248(itru,outs248,lnb)
                endif
              endif
            else
              if (IDHFLG.eq.1.and.ixopen.eq.1)then
                lnb=lnblnk(outs400)   ! write directly to file
                write(itru,'(A)',iostat=ios,err=1)outs400(1:lnb)
              elseif (IDHFLG.eq.1.and.ixopen.eq.0)then
                lnb=MIN0(lnblnk(outs248),124)
                if(lnb.lt.72)then
                  call edisp(itru,outs248(1:lnb))
                else
                  call edisp248(itru,outs248,lnb)
                endif
              else
                continue
              endif
            endif
 100      continue
        endif

C Recover results for day.
        call GOGET(IDAY)

C Loop through day's timesteps.
        DO 421 J = 1,NDTS,NOUT

C Compute current time.
C IHRD - number of days since start of plotting period in hours.
C TIME - time in hours since start of first day plotted.
          IHRD=(IDAY-IODS)*24
          call DATIME(J,ATIME)
          TIME=float(IHRD)+ATIME

C Within requested output period.
          IF(TIME.LT.(TSTART-1.0).or.TIME.GT.TFINSH)goto 421

C Display data. Create both data strings for this timestep.
          outs248='  '
          outs400='  '
          if (IHFLAG.eq.0) then 
            if(ipvform.eq.3)then
              call SJTIME(J,LJTIME)
              write (outs248,'(3a)') sq,LJTIME,sq
              write (outs400,'(3a)') sq,LJTIME,sq
              K=11
            else
              call STIME(J,LTIME)
              write (outs248,'(a5)') LTIME
              write (outs400,'(a5)') LTIME
              K=6
            endif
          else
            if(ipvform.eq.3)then
              call SJTIME(J,LJTIME)
              write (outs248,'(3a)') sq,LJTIME,sq
              write (outs400,'(3a)') sq,LJTIME,sq
              K=11
            else
              RDOTY=FLOAT(IDAY)+(ATIME/24.)
              write (outs248,'(f10.4)') RDOTY
              write (outs400,'(f10.4)') RDOTY
              K=11
            endif
          endif

C First write data to the 248 char string buffer.
          klast=k
          do 410 IG=1,NGET
            KE=K+11
            if(KE.le.248)then
              IZONE=IGETNO(IG,2)

C If there is occupancy filter and occupancy then include in check.
C Assume fully occupied.
              ih=int(ATIME+1.)
              ioc=1
              if(iocupf.eq.1) call getocup(IZONE,IDAY,ih,ioc,ier)
              if(ioc.ne.0) then
                write (outs248(K:KE),'(f11.2)') VAL2(IG,J)
              elseif (IGETNO(IG,1).ge.50.AND.IGETNO(IG,1).le.59) then
                if (VAL2(IG,J).gt.100.0) then
                  write (outs248(K:KE),'(a)') '  invl dT  '
                  IINLVDT=1
                else
                  write (outs248(K:KE),'(f11.2)') VAL2(IG,J)
                endif
              else
                write (outs248(K:KE),'(a)') '  not occ  '
              endif
              K=K+11
            endif
 410      continue

C If writing to file then write data to the 400 char string buffer.
          if(ixopen.eq.1)then
            k=klast   ! re-establish k position value
            do 411 IG=1,NGET
              KE=K+11
              if(KE.le.400)then
                IZONE=IGETNO(IG,2)

C If there is occupancy filter and occupancy then include in check.
C Assume fully occupied.
                ih=int(ATIME+1.)
                ioc=1
                if(iocupf.eq.1) call getocup(IZONE,IDAY,ih,ioc,ier)
                if(ioc.ne.0) then
                  write (outs400(K:KE),'(f11.2)') VAL2(IG,J)
                elseif (IGETNO(IG,1).ge.50.AND.IGETNO(IG,1).le.59) then
                  if (VAL2(IG,J).gt.100.0) then
                    write (outs400(K:KE),'(a)') '  invl dT  '
                    IINLVDT=1
                  else
                    write (outs400(K:KE),'(f11.2)') VAL2(IG,J)
                  endif
                else
                  write (outs400(K:KE),'(a)') '  not occ  '
                endif
                K=K+11
              endif
 411        continue
          endif

          if(ixopen.eq.1)then
            lnb=lnblnk(outs400)   ! write directly to file

            if(delim.eq.'-')then
              write(itru,'(A)',iostat=ios,err=1)outs400(1:lnb)
            else

C If delimiter set to alternative then process text before writing.
C If using X delimeter (tagged data) then set the delimeter to a comma.
              dg=delim
              if (delim.eq.'X') dg='C'
              call SDELIM(outs400,outs400d,dg,IW)
              lnb=lnblnk(outs400d)   ! write directly to file
              write(itru,'(A)',iostat=ios,err=1) outs400d(1:lnb)
            endif
          else

C Writing to screen so use shorter file buffer.
            lnb=MIN0(lnblnk(outs248),124)
            if(lnb.lt.72)then
              call eddisp(itru,outs248(1:lnb))
            else
              call eddisp248(itru,outs248,lnb)
            endif
          endif
 421    CONTINUE
 10   continue

C Explain inlv dT comment.
      if (IINLVDT.eq.1) then
        call edisp(itru,'invl dT: the supplied temperature difference')
        call edisp(itru,' to the comfort algorithm was outside the ')
        call edisp(itru,' valid range, thus no data could be returned.')
      endif

C End tag for tagged data
      if (delim.eq.'X') then
        write(outs,'(a)')'*end_table'
        call edisp(itru,outs)
      endif
      
C Allow other/ re- display?
      if(act(1:1).eq.'p'.or.act(1:1).eq.'m')then
        return
      else
        goto 1
      endif

      END

C ******************** MGLTBL ********************
C MGLTBL: monthly multi-column tabular listing for selected
C gains and losses.
      SUBROUTINE MGLTBL
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTPCH/ICOUT
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      common/recver/izver,ipver,iever
      COMMON/ZONPIK/NZ,NZNO(MCOM)
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS)
      common/getmenu/menutype,igetind(65),igetflux(65)

      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/SETNAM/RSNAME(MNRS)
      COMMON/SIMPKA/NSIM


      CHARACTER PDESCR*60,SDESCR*44,CSTR*25,NMTHNM(12)*3
      character outs*124,prompt*124,outsl*144
      CHARACTER RSNAME*40
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      character SLABEL*32,GLABEL*20

      DIMENSION VAL(12),TVAL(12,12)
      DIMENSION NDYMTH(12), CQ(MTS), CQR(MTS),CQOUT(MTS)

      DATA NDYMTH/31,59,90,120,151,181,212,243,273,304,334,365/
      DATA NMTHNM/'Jan','Feb','Mar','Apr','May','Jun','Jul',
     &'Aug','Sep','Oct','Nov','Dec'/

C Set chosen result set.
      ISET=ISIM
      IEND=24*NTS

C Generate descriptive strings & output general header information.
C If output to file alter the edisp unit number.
      prompt = ' '
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
      endif

C Call the menu of choices (this also sets some default options).
      MENUTYPE=13
      call GOMSETUP
      call GOMENU
      if(ixopen.eq.1)then
        itru = ixunit
        if(NGET.ge.1)then
          write(prompt,'(a,a)') SLABEL(1)(1:lnblnk(SLABEL(1))),
     &    ' >> file.'
        else
          write(prompt,'(a)')' Output being directed to file. ' 
        endif
        call usrmsg(prompt,'Scanning for range of values...','-')
      else
        write(prompt,'(3a)') 'Scanning for ',
     &    SLABEL(1)(1:lnblnk(SLABEL(1))),'...'
        itru = icout
        call usrmsg(prompt,'  ','-')
      endif

C Recover 100Wh vs kWh from IGETNO().
      IR=IGETNO(1,4)

C Set header text.
      CALL HDDATE(PDESCR)
      CALL HDSTEP(SDESCR)
      call edisp(itru,' ')
      lsn1=MIN0(lnblnk(RFILE),22)
      if(NSIM.gt.1)then
        WRITE(outs,'(3A,I4,2A)')'Lib: ',RFILE(1:lsn1),' Set:',ISIM,
     &    ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
      else
        WRITE(outs,'(4A)')'Lib: ',RFILE(1:lsn1),
     &    ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
      endif
      call edisp(itru,outs)
      WRITE(outs,'(3A)')PDESCR(1:lnblnk(PDESCR)),' ',
     &   SDESCR(11:lnblnk(SDESCR))
      call edisp(itru,outs)
      call edisp(itru,' ')

      if(IR.eq.1)call edisp(itru,
     &' Monthly selection of gains & losses (to nearest 100Wh).')
      if(IR.eq.2)call edisp(itru,
     &' Monthly selection of gains & losses (to nearest kWh).')
      call edisp(itru,' ')

C Monthly statistics of energy gains and losses.
      write(outs,602)
  602 format(
     &  ' Zone      Period| Transp surfs   | Opaque surfs  | Casual',
     &  ' Gains |Infilt.|Ventil |    Plant      |Solar radiation|')
      call edisp(itru,outs)
      write(outs,6021)
 6021 format(
     &  '             in  |extern  |other  |extern |other  |convec ',
     &  'radiant|       |       |Heating|Cooling|absor |entering|')
      call edisp(itru,outs)
      write(outs,6022)
 6022 format(
     &  '                 |facing  |facing |facing |facing |       ',
     &  '       |       |       |       |       |inside|zone    |')
      call edisp(itru,outs)

C Loop for 12 topics and then each month.
      DO 620 IJK=1,12
        DO 610 IMNTH=1,12
          TVAL(IJK,IMNTH)=0.0
  610   CONTINUE
  620 CONTINUE

C Per zone statistics.
      DO 660 KGET=1,NGET
        izn=IGETNO(KGET,2)
        DO 621 IJK=1,12
          VAL(IJK)=0.0
  621   CONTINUE

C Get statistics for each day.
        IMNTH=IOM1
        DO 650 J=IODS,IODF
          JD=J
          NEL=0
          NN=NZSUR(IZN)
          CALL CHKTIME(JD,ISTART,IEND)

C For each of the items perform a low level get for the days data,
C filter it according to IAV and get the sums for the day.

C Column 1 tmc external, 2 tmc internal, 3 opq external, 4 opq internal.
C If version 2 of library then recover via goutopq etc.
          if(ISAVE.lt.4.and.izver.ge.2)then
            CALL GOUTOPQ(JD,IZN,ISET)
            CALL FLTIAV(JD,GVAL,CQ,NEL)
            do 161 ISTP=ISTART,IEND
              VAL(3)=VAL(3)+CQ(ISTP)
  161       continue
            CALL GOPQIN(JD,IZN,ISET)
            CALL FLTIAV(JD,GVAL,CQ,NEL)
            do 162 ISTP=ISTART,IEND
              VAL(4)=VAL(4)+CQ(ISTP)
  162       continue
            CALL GOUTTRN(JD,IZN,ISET)
            CALL FLTIAV(JD,GVAL,CQ,NEL)
            do 163 ISTP=ISTART,IEND
              VAL(1)=VAL(1)+CQ(ISTP)
  163       continue
            CALL GTRNIN(JD,IZN,ISET)
            CALL FLTIAV(JD,GVAL,CQ,NEL)
            do 164 ISTP=ISTART,IEND
              VAL(2)=VAL(2)+CQ(ISTP)
  164       continue
          elseif(ISAVE.eq.4)then

C Surface convection contributions, loop through each surface,
C find if interior or exterior etc. Note convection is calculated
C in terms of surface so negate it to have contribution to air node.
            DO 60 ISN= 1,NN
              CALL SURADJ(IZN,ISN,IE,T,IZC,ISC,ICN,CSTR)
              CALL CSCONV(JD,IZN,ISN,CQ)
              CALL FLTIAV(JD,CQ,CQOUT,NEL)
              DO 61 ISTP=ISTART,IEND
                IF(IE.EQ.0)THEN
                  if(SSOTF(ICN)(1:4).EQ.'OPAQ')then
                    VAL(3)=VAL(3)+CQOUT(ISTP)*(-1.0)
                  else
                    VAL(1)=VAL(1)+CQOUT(ISTP)*(-1.0)
                  endif
                ELSE
                  if(SSOTF(ICN)(1:4).EQ.'OPAQ')then
                    VAL(4)=VAL(4)+CQOUT(ISTP)*(-1.0)
                  else
                    VAL(2)=VAL(2)+CQOUT(ISTP)*(-1.0)
                  endif
                ENDIF
   61         CONTINUE
   60       CONTINUE
          else
            call usrmsg('Library version or save level doesn`t support',
     &        'recovery of convection data.','W')
          endif

C Get casual gains. Unlike other facilities, only the total
C casual convective and radiant are required so there is
C little overhead with the GCASC and GCASR method.
          CALL GCASC(JD,IZN,ISET) ! for older versions.
          CALL FLTIAV(JD,GVAL,CQ,NEL)
          CALL SUM1V(CQ,ISTART,IEND,SGT,SLT,IGT,IEQ,ILT)
          VAL(5)=VAL(5)+SGT+SLT

          CALL GCASR(JD,IZN,ISET) ! for older versions.
          CALL FLTIAV(JD,GVAL,CQ,NEL)
          CALL SUM1V(CQ,ISTART,IEND,SGT,SLT,IGT,IEQ,ILT)
          VAL(6)=VAL(6)+SGT+SLT

C Infiltration.
          CALL GQV1(JD,IZN,ISET)
          CALL FLTIAV(JD,GVAL,CQ,NEL)
          CALL SUM1V(CQ,ISTART,IEND,SGT,SLT,IGT,IEQ,ILT)
          VAL(7)=VAL(7)+SGT+SLT

C Ventilation.
          CALL GQV2(JD,IZN,ISET)
          CALL FLTIAV(JD,GVAL,CQ,NEL)
          CALL SUM1V(CQ,ISTART,IEND,SGT,SLT,IGT,IEQ,ILT)
          VAL(8)=VAL(8)+SGT+SLT

C Convection portion of plant.
          CALL GZQM(JD,IZN,ISET)
          CALL FLTIAV(JD,GVAL,CQ,NEL)
          CALL SUM1V(CQ,ISTART,IEND,SGT,SLT,IGT,IEQ,ILT)
          VAL(9)=VAL(9)+SGT
          VAL(10)=VAL(10)+SLT

C Solar absorbed in the zone.
          call GSOLABS(JD,IZN,ISET)
          CALL FLTIAV(JD,GVAL,CQ,NEL)
          CALL SUM1V(CQ,ISTART,IEND,SGT,SLT,IGT,IEQ,ILT)
          VAL(11)=VAL(11)+SGT

C Solar entering zone from outside.
          call GSOLX(JD,IZN,ISET)
          CALL FLTIAV(JD,GVAL,CQ,NEL)
          CALL SUM1V(CQ,ISTART,IEND,SGT,SLT,IGT,IEQ,ILT)
          VAL(12)=VAL(12)+SGT

C Check if month finished.
          IF(JD.LT.NDYMTH(IMNTH).AND.JD.NE.IODF)goto 650

C Dump this month's statistics.
          DO 638 IJK=1,12
            VAL(IJK)=VAL(IJK)/1000.
  638     CONTINUE

          IF(IMNTH.EQ.IOM1)THEN
            if(IR.eq.1)then
              WRITE(outs,701)zname(IZN)(1:12),NMTHNM(IMNTH),
     &                      (VAL(IJ),IJ=1,12)
  701         FORMAT(1X,A12,1X,A3,1X,12F8.1)
            elseif(IR.eq.2)then
              WRITE(outs,703)zname(IZN)(1:12),NMTHNM(IMNTH),
     &                      (VAL(IJ),IJ=1,12)
  703         FORMAT(1X,A12,1X,A3,1X,12F8.0)
            endif
            call eddisp(itru,outs)
          ELSE
            if(IR.eq.1)then
              WRITE(outs,702)NMTHNM(IMNTH),(VAL(IJ),IJ=1,12)
  702         FORMAT(14x,A3,1X,12F8.1)
            elseif(IR.eq.2)then
              WRITE(outs,704)NMTHNM(IMNTH),(VAL(IJ),IJ=1,12)
  704         FORMAT(14x,A3,1X,12F8.0)
            endif
            call eddisp(itru,outs)
          ENDIF

          DO 640 IJK=1,12
            TVAL(IJK,IMNTH)=TVAL(IJK,IMNTH)+VAL(IJK)
            VAL(IJK)=0.0
  640     CONTINUE
          IMNTH=IMNTH+1
  650   CONTINUE
  660 CONTINUE

C Dump 'all-zone' statistics if more than one zone.
      DO 680 IMNTH=IOM1,IOM2
        if (NZ.GT.1) then
          IF(IMNTH.EQ.IOM1)THEN
            write(outs,705)NMTHNM(IMNTH),(TVAL(IJK,IMNTH),IJK=1,12)
  705       format(' All zones    ',A3,1X,12(F8.0))
            call edisp(itru,' ')
            call eddisp(itru,outs)
          ELSE
            write(outs,706)NMTHNM(IMNTH),(TVAL(IJK,IMNTH),IJK=1,12)
  706       format(14X,A3,1X,12(F8.0))
            call eddisp(itru,outs)
          ENDIF
        endif
        DO 685 IJK=1,12
          VAL(IJK)=VAL(IJK)+TVAL(IJK,IMNTH)
  685   CONTINUE
  680 CONTINUE

C Dump the annual totals if run was for more than one month.
      if (IOM2.GT.IOM1) then
        write(outsl,'(A,12(F10.0))') ' Annual   ',(VAL(IJK),IJK=1,12)
        call edisp(itru,' ')
        call eddisp(itru,outsl)
      endif
      call edisp(itru,' ')
      if(ixopen.eq.1)then
        call usrmsg(prompt,'Scanning for range of values...done.','-')
      else
        call usrmsg('  ','  ','-')
      endif

      RETURN
      END


C ******************** CZMRT ********************
C CZMRT is a mid level routine to return one days temperatures
C of zone mean radiant temperature.  Use VAL3(15,I) to VAL3(20,I)
C for temporary storage. Returns value via XDUM1 in common block
C GET2.
C CALLS: GZTMS.

      SUBROUTINE CZMRT(IDAY,IZONE,ISET)
#include "building.h"
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

      DIMENSION ZTMS(MS),YDUM(MS),TSO(MS)

      N=24*NTS
      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        DO 74,JC1=1,N
          XDUM1(JC1)=0.0
          IS=ISET
          CALL GZTMS(IDAY,IZONE,IS,JC1,ZTMS,TSO,TAIR,TMCON,A1,A2,A5,RH)
          YDUM(1)=A1
          YDUM(2)=A2
          YDUM(3)=0.
          YDUM(4)=0.
          YDUM(5)=A5
          CALL MOMNRD(IZONE,TAIR,ZTMS,YDUM,NZSUR(IZONE),TMNRD)
          XDUM1(JC1)=TMNRD
   74   CONTINUE
      ELSE
        call edisp(iuout,' CZMRT: Save option does not allow access.')
      ENDIF
      RETURN
      END

C ******************** CZRESL ********************

C CZRESL is a mid level routine to return one day's values for
C zone resultant temperature.  To save memory, use a portion of
C VAL3(15,I) to VAL3(20,I) for temporary storage. Returns value
C via XDUM1 in common GET2.
C CALLS:  GZTMS.

      SUBROUTINE CZRESL(IDAY,IZONE,ISET)
#include "building.h"
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)
      DIMENSION ZTMS(MS),YDUM(MS),TSO(MS)

      N=24*NTS

      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        DO 74,JC1=1,N
          XDUM1(JC1)=0.0
          IS=ISET
          CALL GZTMS(IDAY,IZONE,IS,JC1,ZTMS,TSO,TAIR,TMCON,A1,A2,A5,RH)
          YDUM(1)=A1
          YDUM(2)=A2
          YDUM(3)=0.
          YDUM(4)=0.
          YDUM(5)=A5
          CALL MORESL(IZONE,TAIR,ZTMS,YDUM,NZSUR(IZONE),TRESL)
          XDUM1(JC1)=TRESL
   74   CONTINUE
      ELSE
        call edisp(iuout,' CZRESL: Save option does not allow access.')
      ENDIF
      RETURN
      END

