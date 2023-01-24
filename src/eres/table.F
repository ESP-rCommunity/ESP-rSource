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


C This file contains the following routines:
C  MOTABL Produces tabular output for a 24 hour period.
C  ZONTAB Multi-column tabular listing for performance metrics.
C  MGLTBL monthly multi-column tabular listing for selected gains & losses.
C  CZMRT  returns one day's temperatures of zone mean radiant temp.
C  CZRESL returns one day's temperatures of zone resultant temp.

C ******************** MOTABL ********************
C Produces tabular output for a 24 hour period.

      SUBROUTINE MOTABL
#include "building.h"
#include "model.h"
#include "prj3dv.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "help.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
C      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      common/AFN/IAIRN,LAPROB,ICAAS(MCOM)

C ihflag = 0 write 13h30, ihflag = 1 write 0.5625, = 2 write yyyy-mm-dd 13:30:00
C idhflg = 0 no day demarcations, idhflg = 1 write demarcation
C          between tabular reporting days.
C ilflag = 0 tabular labels on multi-lines, ilflag = 1 on one line
C          ilflag = 2 do not include # header lines in file.
      COMMON/GRTOOL/IHFLAG,IDHFLG,ILFLAG
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
C      common/prec7/itcnst

C Special materials file flag
C      common/spmfxst/ispmxist,spflnam  
C      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
C     &nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
C      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG 
      common/getmenu/menutype,igetind(65),igetflux(65)

      CHARACTER*23 ITEM(15)
      character xfile*144,tg*1,delim*1
C      character spflnam*72
      integer IWM  ! for radio button
      character outs*124,LAPROB*72
      integer NITMS,INO ! max items and current menu item
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

      helpinsub='table'  ! set for subroutine

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
      elseif(IHFLAG.eq.2)then
        ITEM(11)='* time >>mm-dd 10:30:00'
      endif
      if(ILFLAG.eq.0)then
        ITEM(12)='& labels >> multiline  '
      elseif(ILFLAG.eq.1)then
        ITEM(12)='& labels >> on one line'
      elseif(ILFLAG.eq.2)then
        ITEM(12)='& labels >> one ln no #'
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

C Instantiate help string array for all interactions.
      helptopic='res_tabular_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

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

C Network flow tabular output. If iairn=3 we can display the
C flow network in 3D at this point.
        if(iairn.eq.3)then
          if (MMOD.eq.8) then

C Initialise coordinates for eye point, view point and angle of view.
            EYEM(1)=-100.; EYEM(2)=-100.; EYEM(3)=100.
            VIEWM(1)=10.; VIEWM(2)=10.; VIEWM(3)=10.
            ANG=40.

C General image option flags.
            ITDSP=0; ITBND=1; ITEPT=0
            ITZNM=0; ITSNM=1; ITVNO=1
            ITORG=0; ITSNR=1; ITOBS=0
            ITGRD=0; GRDIS=0.0
            ITPPSW=0

C Clear current viewing box and re-establish image bounds.
            CALL startbuffer

C Setup and pass in parameters to win3d.
            iiw1=4; iiw2=4; iiw3=3; iiw4=3; iimenu=menuchw
            iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
            CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &        iigl,iigr,iigt,iigb,iigw,iigwh)
            igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
            igw=int(iigw); igwh=int(iigwh)
            igwid=igw; igheight=igwh

C Get bounds for image.
            nzg=NCOMP
            do 29 iz=1,nzg
              nznog(iz)=iz
  29        continue
            call bndobj(0,IER)

            HANG=ANG/2.0; HITH=1.0; YON=1300.0
            CALL LENS(IER)

C Determine scaling ratios for the drawing and axis.
C Determine the 2D coords of the site extremes.
            CALL SITE2D(SXMX,SXMN,SYMX,SYMN,ier)
C          write(6,*) SXMX,SXMN,SYMX,SYMN
            CALL axiscale(igwid,igheight,SXMN,SXMX,SYMN,SYMX,xsc,ysc,
     &        sca,Xadd,Yadd)
C          write(6,*) SXMN,SXMX,SYMN,SYMX,xsc,ysc,sca,Xadd,Yadd
            call linescale(iigl,Xadd,sca,iigb,Yadd,sca)
            call INICLP(ier)
          endif

C Read 3D flow network, report contents and draw the network.
          IUM=IFIL+6
          CALL ERPFREE(IUM,ISTAT)
          CALL MFCDAT
          CALL EMF3DREAD(IUM,'S',IER)
          call edisp(iuout,' ')
          CALL MFLIST(iuout,'s')
          if(NNOD.gt.50.or.NCMP.gt.50)then
            call MFWFDRAW('r',0,0)  ! Draw flow network without names if complex
          else
            call MFWFDRAW('r',1,1)  ! Draw flow network with names
          endif 
          CALL ERPFREE(IUM,ISTAT)
          call pauses(2)
        endif
        MENUTYPE=4
        call MFOUTP('-')
      elseif(INO.EQ.10.AND.ISAVE.GE.1)then
        call ctlexp(xfile,ixopen,ixunit,ixpunit,'X','Tabular',IER)
      elseif(INO.EQ.11.AND.ISAVE.GE.1)then

C Toggle time format and day separators.
        IHFLAG=IHFLAG+1
        if(IHFLAG.GT.2)IHFLAG=0
        if(IHFLAG.eq.0)then
          call edisp(iuout,' ')
          call edisp(iuout,'setting standard display time = 10h30')
        elseif(IHFLAG.eq.1)then
          call edisp(iuout,' ')
          call edisp(iuout,'setting time = day.fraction 10.5000')
        elseif(IHFLAG.eq.2)then
          call edisp(iuout,' ')
          call edisp(iuout,'setting time = yyyy-mm-dd hh:mm:ss')
        endif
        CALL EASKAB('Include mark between days when ',
     &              'displaying or writing data :','no','yes',
     &              IDH,nbhelp)
        IDHFLG=IDH-1
      elseif(INO.EQ.12.AND.ISAVE.GE.1)then

C Toggle tabular listing label format.
        ILFLAG=ILFLAG+1
        if(ILFLAG.GT.2)ILFLAG=0
        if(ILFLAG.eq.0)then
          call edisp(iuout,' ')
          call edisp(iuout,'column labels on multi-lines ')
        elseif(ILFLAG.eq.1)then
          call edisp(iuout,' ')
          call edisp(iuout,'column labels on one line (spreadsheets)')
        elseif(ILFLAG.eq.2)then
          call edisp(iuout,' ')
          call edisp(iuout,
     &      'column labels on one line & no # header lines in files')
        endif
      elseif(INO.EQ.13.AND.ISAVE.GE.1)then

C Toggle delimeter.
        IWM=1
        CALL EASKATOG('Delimeter to use between columns of data:',' ',
     &    'normal spaces','single space','tab','comma','tagged',
     &    'continue',' ',IWM,nbhelp)
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
        helptopic='res_tabular_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('tabular',nbhelp,'-',0,0,IER)
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
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS

      character SLABEL*32,GLABEL*24,TABLABEL*36
      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS),TABLABEL(MZS)
      integer LNSLABEL,LNGLABEL,LNTABLABEL  ! lengths for label strings
      COMMON/LNGETLABEL/LNSLABEL(MZS),LNGLABEL(MZS),LNTABLABEL(MZS)
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/SETNAM/RSNAME(MNRS)
      COMMON/SIMPKA/NSIM

      COMMON/GRTOOL/IHFLAG,IDHFLG,ILFLAG
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

      integer ireportunit ! zero is default one is W two is kW three Joules
      common/repunit/ireportunit

      character outs*124,outs248*248 ! string buffers
      character outs800*800,outs800d*800  ! really long string buffers
      character act*1,sq*1,unit*7,t28*28,dg*1
      character LTIME*5,LJTIME*8
      character prompt*124,tg*1,delim*1,xfile*144
      CHARACTER PDESCR*64,SDESCR*44,RSNAME*40,DESCR*7,DESCR1*10,DESCR2*8
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      integer lnb    ! length of buffer to write to
      integer K,K4   ! counters
      integer KE,KE4 ! current end points
      double precision atime
      logical nextday

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        if(NGET.ge.1)then
          write(prompt,'(a,a)') SLABEL(1)(1:LNSLABEL(1)),' >> file.'
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
            write(prompt,'(a,a)') SLABEL(1)(1:LNSLABEL(1)),' >> file.'
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
      INOCFD=0

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

C Switch to fixed font for text feedback.
      if(MMOD.EQ.8)then
        lastmenufont=IMFS
        lastbuttonfont=IFS
        lasttextfont=ITFS
        if(ITFS.eq.4) ITFS=0
        if(ITFS.eq.5) ITFS=1
        if(ITFS.eq.6) ITFS=2
        if(ITFS.eq.7) ITFS=3
        call userfonts(IFS,ITFS,IMFS)
        call winfnt(itfs)
      endif
      
C Write general header information (skip if IPV mode).
      if(act(1:1).ne.'m')then
        CALL HDDATE(PDESCR)
        CALL HDSTEP(SDESCR)
        
C If julian or 13:30:00 requested or writing to file start lines with 
C a '#'. If user has set ILFLAG=2 then do not write # header lines.
        if (IHFLAG.eq.1.or.IHFLAG.eq.2.or.ixopen.eq.1) then
          if(ILFLAG.le.1)then
            call edisp(itru,'# Timestep performance metrics.')
          endif
          lsn1=MIN0(lnblnk(RFILE),42)
          if(NSIM.gt.1)then
            if(ILFLAG.le.1)then
              write(outs248,'(3A,I4,3A)')'# Results library: ',
     &         RFILE(1:lsn1),'; results set:',ISIM,': (',
     &         RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM))),')'
            else
              write(outs248,'(3A,I4,3A)')' Results library: ',
     &         RFILE(1:lsn1),'; results set:',ISIM,': (',
     &         RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM))),')'
            endif
          else
            if(ILFLAG.le.1)then
              write(outs248,'(5A)')'# Results library: ',RFILE(1:lsn1),
     &        '; (',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM))),')'
            else
              write(outs248,'(5A)')' Results library: ',RFILE(1:lsn1),
     &        '; (',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM))),')'
            endif
          endif
          if(ixopen.eq.1)then
            lnb=lnblnk(outs248)   ! write directly to file
            if(ILFLAG.le.1)then
              write(itru,'(A)',iostat=ios,err=1)outs248(1:lnb)
            endif
          else
            lnb=MIN0(lnblnk(outs248),124)
            if(lnb.lt.124)then
              call edisp(itru,outs248(1:lnb))
            else
              call edisp248(itru,outs248,124)
            endif
          endif
          WRITE(outs,'(4A)')'# ',PDESCR(1:lnblnk(PDESCR)),' ',
     &      SDESCR(12:lnblnk(SDESCR))
          if(ILFLAG.le.1)then
            call edisp(itru,outs)
          endif
        else

C Writing to the screen. It would be great to figure out how wide
C we can write to << ?? to be done >>
          call edisp(itru,'Timestep performance metrics.')
          lsn1=MIN0(lnblnk(RFILE),42)
          if(NSIM.gt.1)then
            write(outs248,'(3A,I4,3A)')'Results library: ',
     &        RFILE(1:lsn1),'; results set:',
     &        ISIM,' (',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM))),')'
          else
            write(outs248,'(5A)')'Results library: ',RFILE(1:lsn1),
     &        '; (',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM))),')'
          endif
          if(ixopen.eq.1)then
            lnb=lnblnk(outs248)   ! write directly to file
            write(itru,'(A)',iostat=ios,err=1)outs248(1:lnb)
          else
            lnb=MIN0(lnblnk(outs248),124)
            if(lnb.lt.124)then
              call edisp(itru,outs248(1:lnb))
            else
              call edisp248(itru,outs248,124)
            endif
          endif

          WRITE(outs,'(3A)')PDESCR(1:lnblnk(PDESCR)),' ',
     &      SDESCR(12:lnblnk(SDESCR))
          call edisp(itru,outs)
        endif
      endif

      do 10 IDAY=IODS,IODF

C Write day of results and column headings information (skip if IPV mode).
        CALL STDATE(IYEAR,IDAY,DESCR,DESCR1,DESCR2)
        if(act(1:1).ne.'m')then
          if (IDHFLG.eq.1.and.ixopen.eq.1) then
            outs='# '
            if(ILFLAG.le.1) call edisp(itru,outs)
            write(outs,'(3A)')'# ',DESCR1,' timestep misc. listing'
            if(ILFLAG.le.1) call edisp(itru,outs)
            outs='# '
            if(ILFLAG.le.1) call edisp(itru,outs)
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
          
C If printing to screen then split column headings over four rows 
C don't do this if printing to file.
          irows=4
          if (ixopen.eq.1) irows=1
          is=1
          do 100 ir=1,irows
            K=6   ! initial value for 248 buffer
            K4=6  ! initial value for 800 buffer
            if (ir.gt.1) then
              outs248='     '  ! clear for 248 buffer
              outs800='     '  ! clear for 800 buffer
            else
              if ((IHFLAG.eq.1.or.IHFLAG.eq.2).and.ixopen.eq.1) then
                if (delim.eq.'X') then
                  outs248='*time'
                  outs800='*time'
                else
                  if(ILFLAG.le.1)then
                    outs248='#Time'
                    outs800='#Time'
                  else
                    outs248='Time'
                    outs800='Time'
                    K=5   ! initial value for 248 buffer
                    K4=5  ! initial value for 800 buffer
                  endif
                endif
              elseif ((IHFLAG.eq.1.or.IHFLAG.eq.2).and.ixopen.eq.0)then
                if (delim.eq.'X') then
                  outs248='*time'
                  outs800='*time'
                else
                  outs248='Time'
                  outs800='Time'
                endif
              else
                outs248='Time '
                outs800='Time '
              endif
            endif
            do 20 IL=1,NGET
            
C Define units. Note: climate data will base its units on 
C the IGETNO(IL,8) value along with all other items. Take
C into account the value of ireportunit.
              if (IGETNO(IL,8).eq.1) then
                unit='(C)'
              elseif (IGETNO(IL,8).eq.2) then
                if(IGETNO(IL,1).eq.8.or.IGETNO(IL,1).eq.9.or.
     &             IGETNO(IL,1).eq.10.or.IGETNO(IL,1).eq.30.or.
     &             IGETNO(IL,1).eq.31.or.IGETNO(IL,1).eq.32.or.
     &             IGETNO(IL,1).eq.33) then
                  if(ireportunit.eq.0.or.ireportunit.eq.2)then
                    unit='(kW)'
                  else
                    unit='(W)'
                  endif
                else
                  unit='(W)'
                endif
              elseif (IGETNO(IL,8).eq.3) then
                unit='(W/m^2)'
              else
                unit='(-)'
              endif
              
C Set end character for output string (KE or KE4) and for section of 
C  tablabel (ie) to be printed (if to screen).
              KE=K+(44/irows)   ! set for 248
              KE4=K4+(44/irows) ! set for 800
              ie=is+9
              if(KE.le.248)then
                
C If writing to file remove blanks from label (up to 28 char with
C a bit more for the units) lets assume average is 24 char which
C says that ~33 labels can be written in the 800 char width. If data
C relates to surfaces then labels take up more space. If there is
C a delimeter being used in the data columns also apply this in
C the generation of the heading of text file.
                if (ixopen.eq.1) then
                  call SDELIM(TABLABEL(IL),t28,'N',IW)
                  if(delim.eq.'-'.or.delim.eq.'S')then
                    write(outs248(K:KE),'(3a)') ' ',
     &                t28(1:lnblnk(t28)),unit
                  elseif(delim.eq.'C')then
                    write(outs248(K:KE),'(3a)') ',',
     &                t28(1:lnblnk(t28)),unit
                  elseif(delim.eq.'T')then
                    write(outs248(K:KE),'(3a)') CHAR(9),
     &                t28(1:lnblnk(t28)),unit
                  endif
                  K=K+lnblnk(t28)+lnblnk(unit)+1
                else
                  if (ir.lt.4) then
                    write (outs248(K:KE),'(2a)') '|',TABLABEL(IL)(is:ie)
                  else
                    write (outs248(K:KE),'(2a)') '| ',unit
                  endif
                  K=KE
                endif
              endif

C For writing to file also fill the 800 char buffer.
              if(KE4.le.800)then
                if (ixopen.eq.1) then
                  call SDELIM(TABLABEL(IL),t28,'N',IW)
                  if(delim.eq.'-'.or.delim.eq.'S')then
                    write(outs800(K4:KE4),'(3a)') ' ',
     &                t28(1:lnblnk(t28)),unit
                  elseif(delim.eq.'C')then
                    write(outs800(K4:KE4),'(3a)') ',',
     &                t28(1:lnblnk(t28)),unit
                  elseif(delim.eq.'T')then
                    write(outs800(K4:KE4),'(3a)') CHAR(9),
     &                t28(1:lnblnk(t28)),unit
                  endif
                  K4=K4+lnblnk(t28)+lnblnk(unit)+1
                else
                  if (ir.lt.4) then
                    write (outs800(K4:KE4),'(2a)') '|',
     &                TABLABEL(IL)(is:ie)
                  else
                    write (outs800(K4:KE4),'(2a)') '| ',unit
                  endif
                  K4=KE4
                endif
              endif
 20         continue
            is=ie+1

C Print titles on first day and only on subsequent ones if day
C demarcations are omitted.
C Debug.
C            write(6,*) outs248(1:lnblnk(outs248))
C            write(6,*) ' '
C            write(6,*) outs800(1:lnblnk(outs800))
C            write(6,*) ' '

            lnb=lnblnk(outs248)+1   ! ensure we do not warp line
            if (IDAY.eq.IODS)then
              if(ixopen.eq.1)then
                lnb=lnblnk(outs800)   ! write directly to file
                write(itru,'(A)',iostat=ios,err=1)outs800(1:lnb)
              else
                lnb=MIN0(lnblnk(outs248),124)
                if(lnb.lt.124)then
                  call edisp(itru,outs248(1:lnb))
                else
                  call edisp248(itru,outs248,124)
                endif
              endif

            else
              if (IDHFLG.eq.1.and.ixopen.eq.1)then
                lnb=lnblnk(outs800)   ! write directly to file
                write(itru,'(A)',iostat=ios,err=1)outs800(1:lnb)
              elseif (IDHFLG.eq.1.and.ixopen.eq.0)then
                lnb=MIN0(lnblnk(outs248),124)
                if(lnb.lt.124)then
                  call edisp(itru,outs248(1:lnb))
                else
                  call edisp248(itru,outs248,124)
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
C Omit last time step on 31st Dec as the timestamp for this is into next
C year.
        if (IDAY.eq.365) then
          xndts=NDTS-1
        else
          xndts=NDTS
        endif
        DO 421 J = 1,xndts,NOUT

C Compute current time.
C IHRD - number of days since start of plotting period in hours.
C TIME - time in hours since start of first day plotted.
          IHRD=(IDAY-IODS)*24
          call DDTIME(J,ATIME)
          TIME=real(dble(IHRD)+ATIME)

C Within requested output period.
          IF(TIME.LT.(TSTART-1.0).or.TIME.GT.TFINSH)goto 421

C Display data. Create both data strings for this timestep.
          outs248='  '
          outs800='  '
          if (IHFLAG.eq.0) then 
            call STIME(J,LTIME)
            write (outs248,'(a5)') LTIME
            write (outs800,'(a5)') LTIME
            K=6
          elseif (IHFLAG.eq.1) then
            RDOTY=real(dble(IDAY)+(ATIME/24d0))
            write (outs248,'(f11.6)') RDOTY
            write (outs800,'(f11.6)') RDOTY
            K=11
          elseif (IHFLAG.eq.2) then

C Write in format that spreadsheets understand as time.
            call SJTIME(J,LJTIME,nextday)
            if (nextday) then
              call edayr(IDAY+1,ID,IM)
            else
              call edayr(IDAY,ID,IM)
            endif
            write (outs248,'(i4,a,i2.2,a,i2.2,2a)') 
     &        IYEAR,'-',IM,'-',ID,' ',LJTIME
            write (outs800,'(i4,a,i2.2,a,i2.2,2a)') 
     &        IYEAR,'-',IM,'-',ID,' ',LJTIME
            K=20
          endif

C First write data to the 248 char string buffer.
          klast=k
          do 410 IG=1,NGET
            KE=K+11
            if(KE.le.248)then
              IZONE=IGETNO(IG,2)

C If there is occupancy filter and occupancy then include in check.
C Assume fully occupied.
              ih=int(ATIME+1d0)
              ioc=1
              if(iocupf.eq.1) call getocup(IZONE,IDAY,J,ioc,ier)
              if(ioc.ne.0) then
                if (IGETNO(IG,1).ge.50.AND.IGETNO(IG,1).le.59) then
                  if (VAL2(IG,J).gt.101.5) then                    
                    write (outs248(K:KE),'(a)') '  no CFD   '
                    INOCFD=1
                  elseif (VAL2(IG,J).gt.100.0) then
                    write (outs248(K:KE),'(a)') '  invl ipt '
                    IINLVDT=1
                  else
                    write (outs248(K:KE),'(f11.2)') VAL2(IG,J)
                  endif
                else                  
                  write (outs248(K:KE),'(f11.2)') VAL2(IG,J)
                endif
              else
                write (outs248(K:KE),'(a)') '  not occ  '
              endif
              K=K+11
            endif
 410      continue

C If writing to file then write data to the 800 char string buffer.
          if(ixopen.eq.1)then
            k=klast   ! re-establish k position value
            do 411 IG=1,NGET
              KE=K+11
              if(KE.le.800)then
                IZONE=IGETNO(IG,2)

C If there is occupancy filter and occupancy then include in check.
C Assume fully occupied.
                ih=int(ATIME+1d0)
                ioc=1
                if(iocupf.eq.1) call getocup(IZONE,IDAY,J,ioc,ier)
                if(ioc.ne.0) then
                  if (IGETNO(IG,1).ge.50.AND.IGETNO(IG,1).le.59) then
                    if (VAL2(IG,J).gt.101.5) then                    
                      write (outs800(K:KE),'(a)') '  no CFD   '
                      INOCFD=1
                    elseif (VAL2(IG,J).gt.100.0) then
                      write (outs800(K:KE),'(a)') '  invl ipt '
                      IINLVDT=1
                    else
                      write (outs800(K:KE),'(f11.2)') VAL2(IG,J)
                    endif
                  else
                    write (outs800(K:KE),'(f11.2)') VAL2(IG,J)
                  endif
                else
                  write (outs800(K:KE),'(a)') '  not occ  '
                endif
                K=K+11
              endif
 411        continue
          endif

          if(ixopen.eq.1)then
            lnb=lnblnk(outs800)   ! write directly to file

            if(delim.eq.'-')then
              write(itru,'(A)',iostat=ios,err=1)outs800(1:lnb)
            else

C If delimiter set to alternative then process text before writing.
C If using X delimeter (tagged data) then set the delimeter to a comma.
C If IHFLAG is 2 then the initial 19 or 21 characters will be a time
C stamp that needs the 11th character to be a literal space.
              dg=delim
              if (delim.eq.'X') dg='C'
              call SDELIM(outs800,outs800d,dg,IW)
              if(IHFLAG.eq.2) then
                write(outs800d(11:11),'(1a)') ' '
              endif
              lnb=lnblnk(outs800d)   ! write directly to file
              write(itru,'(A)',iostat=ios,err=1) outs800d(1:lnb)
            endif
          else

C Writing to screen so use shorter file buffer.
            lnb=MIN0(lnblnk(outs248),124)
            if(lnb.lt.124)then
              call eddisp(itru,outs248(1:lnb))
            else
              call eddisp248(itru,outs248,124)
            endif
          endif

 421    CONTINUE
 10   continue
 
      if(MMOD.EQ.8)then
        IMFS=lastmenufont
        ITFS=lasttextfont    ! reset to proportional font in text feedback
        IFS=lastbuttonfont
        call userfonts(IFS,ITFS,IMFS)
        call winfnt(imfs)  ! back to menu font     
        call usrmsg(' ',' ','-')  ! refresh dialog
      endif

C Explain inlv dT comment.
      if (IINLVDT.eq.1) then
        call edisp(itru,'invl ipt: the supplied input to the comfort')
        call edisp(itru,' algorithm was outside the valid range, thus')
        call edisp(itru,' no data could be returned.')
      endif

C Explain no CFD comment.
      if (INOCFD.eq.1) then
        call edisp(itru,'no CFD: CFD results were not found for this')
        call edisp(itru,' time step, thus no data could be returned.')
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
#include "schedule.h"
      
      integer lnblnk  ! function definition

C      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      COMMON/OUTPCH/ICOUT
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      common/recver/izver,ipver,iever
      COMMON/ZONPIK/NZ,NZNO(MCOM)
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      character SLABEL*32,GLABEL*24,TABLABEL*36
      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS),TABLABEL(MZS)
      integer LNSLABEL,LNGLABEL,LNTABLABEL  ! lengths for label strings
      COMMON/LNGETLABEL/LNSLABEL(MZS),LNGLABEL(MZS),LNTABLABEL(MZS)
      common/getmenu/menutype,igetind(65),igetflux(65)

C Markdown flag.
      logical markdown
      common/markdownflag/markdown
      COMMON/GRTOOL/IHFLAG,IDHFLG,ilflag

      logical libheading ! have we printed report heading (library name etc).
      logical headingcontext ! has set or period changed so heading needs reprint
      common/libhead/libheading,headingcontext

      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/SETNAM/RSNAME(MNRS)
      COMMON/SIMPKA/NSIM
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      CHARACTER PDESCR*64,SDESCR*44,CSTR*25,NMTHNM(12)*3
      character outs*124,prompt*124,outsl*174
      CHARACTER RSNAME*40
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72

      DIMENSION VAL(15),TVAL(15,12)
      DIMENSION NDYMTH(12), CQ(MTS), CQOUT(MTS)
      DIMENSION CASDAY(MTS,4)  ! to hold casual type data
      integer looplimit
      real biggest  ! guage size of annual numbers
      real perocupc,perocupr,perocupl ! average occupant to write out
      real perlightc,perlightr,perlightl ! average lighting to write out
      real perequipc,perequipr,perequipl ! average equipment to write out
      real otherc,otherr,otherl ! average other (future expansion) to write out
      integer theonectld  ! if non-zero the casual gain type that is controlled.
      logical XST,dmdsok  ! for dispersed demands
      real overallm2,zonem2frac(MCOM)  ! total of all the selected zones base areas

C User preferences for Monthly table.
      integer imshtr,imtcas,imtaiv,imtctl,imtsol       ! monthly topic inclusion
      common/monthlyoptions/imshtr,imtcas,imtaiv,imtctl,imtsol
      
      integer imshtrl,imtcasl,imtaivl,imtctll,imtsoll,imdmdsl  ! topic column text lengths
      character ZPER*19,SHTR*45,TCAS*37,TAIV*26,TCTL*18,TSOL*18
      dimension ZPER(4),SHTR(4),TCAS(4),TAIV(4),TCTL(4),TSOL(4)
      character TDMD*10
      dimension TDMD(4)
      character SLPER*12,SLHTR*72,SLCAS*36,SLAIV*26,SLCTL*16,SLSOL*30
      character SLDMD*18

      DATA NDYMTH/31,59,90,120,151,181,212,243,273,304,334,365/
      DATA NMTHNM/'Jan','Feb','Mar','Apr','May','Jun','Jul',
     &'Aug','Sep','Oct','Nov','Dec'/

C Set chosen result set.
      ISET=ISIM
      IEND=24*NTS

C Read project demands file if it exists.
      IUO=IFIL+1
      call FINDFIL(bdmds,XST)
      IF(XST)THEN
        CALL ERPFREE(IUO,ISTAT)
        CALL ERBDMD(0,IUO,IER)
        dmdsok=.true.
      else
        dmdsok=.false.
      endif

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
          write(prompt,'(a,a)') SLABEL(1)(1:LNSLABEL(1)),' >> file.'
        else
          write(prompt,'(a)')' Output being directed to file. ' 
        endif
        call usrmsg(prompt,'Scanning for range of values...','-')
      else
        write(prompt,'(3a)') 'Scanning for ',
     &    SLABEL(1)(1:LNSLABEL(1)),'...'
        itru = icout
        call usrmsg(prompt,'  ','-')
      endif

      IR=IGETNO(1,4)   ! Recover 100Wh vs kWh from IGETNO().
      if(markdown)then ! Initial columns.
        imzperl=19
        ZPER(1)='------------- -----'
        ZPER(2)=' Zone         Month'
        ZPER(3)='                   '
        ZPER(4)='                   '
      else
        if(ilflag.eq.0)then
          imzperl=18
          ZPER(1)=' Zone      Period|'
          ZPER(2)='           in    |'
          ZPER(3)='           month |'
          ZPER(4)='                 |'
        else
          imzperl=11
          SLPER='Zone Month '
        endif
      endif
      if(imshtr.eq.1)then  ! Surface heat transfer
        if(markdown)then
          imshtrl=45
          SHTR(1)='  ---------- ---------- ---------- ----------'
          SHTR(2)='  Convection Convection Convection Convection'
          SHTR(3)='  glazing    glazing    opaque     opaque    '
          SHTR(4)='  facade     other      facade     other     '
        else
          if(ilflag.eq.0)then
            imshtrl=32
            SHTR(1)=' Surface convection at face of |'
            SHTR(2)='   transparent |    opaque     |'
            SHTR(3)='facade |other  |facade |other  |'
            SHTR(4)='facing |facing |facing |facing |'
          else
            imshtrl=70
            write(SLHTR,'(2a)')
     &        'Conv-glazing-facade Conv-glazing-other ',
     &        'Conv-opq-facade Conv-opq-other '
          endif
        endif
      elseif(imshtr.eq.2)then
        if(markdown)then
          imshtrl=23
          SHTR(1)='  ---------- ----------'
          SHTR(2)='  Convection Convection'
          SHTR(3)='  glazing    opaque    '
          SHTR(4)='  facade     facade    '
        else
          if(ilflag.eq.0)then
            imshtrl=19
            SHTR(1)='Surface convection|'
            SHTR(2)='    at facade     |'
            SHTR(3)='glazing |opaque   |'
            SHTR(4)='        |         |' 
          else
            imshtrl=36
            SLHTR='Conv-glazing-facade Conv-opq-facade '
          endif
        endif
      endif
      if(imtcas.eq.1)then
        if(markdown)then
          imtcasl=37
          TCAS(1)=' -------- -------- --------  --------'
          TCAS(2)=' Occupant Lights   Small     Other   '
          TCAS(3)='                   power             '
          TCAS(4)='                                     '
        else
          if(ilflag.eq.0)then
            imtcasl=32
            TCAS(1)=' Casual Gains (Radiant+Conv)   |'
            TCAS(2)='Occu-  |Lights |Small  |Other  |'
            TCAS(3)='pant   |       |Power  |       |'
            TCAS(4)='       |       |       |       |'
          else
            imtcasl=35
            SLCAS='Occupants Lights Small-power Other '
          endif
        endif
      elseif(imtcas.eq.2)then
        if(markdown)then
          imtcasl=9
          TCAS(1)=' --------'
          TCAS(2)=' Casual  '
          TCAS(3)=' Gains   '
          TCAS(4)=' R+C     '
        else
          if(ilflag.eq.0)then
            imtcasl=9
            TCAS(1)=' Casual |'
            TCAS(2)=' Gains  |'
            TCAS(3)=' R+C    |'
            TCAS(4)='        |'
          else
            imtcasl=17
            SLCAS='Casual-gains-R+C '
          endif
        endif
      endif
      if(imtaiv.eq.1)then
        if(markdown)then
          imtaivl=26
          TAIV(1)='  ------------ -----------'
          TAIV(2)='  Infiltration Ventilation'
          TAIV(3)='                          '
          TAIV(4)='                          '
        else
          if(ilflag.eq.0)then
            imtaivl=16
            TAIV(1)='Air movement   |'
            TAIV(2)='Infil- |Venti- |'
            TAIV(3)='tration|lation |'
            TAIV(4)='       |       |'
          else
            imtaivl=25
            SLAIV='Infiltration Ventilation '
          endif
        endif
      elseif(imtaiv.eq.2)then
        if(markdown)then
          imtaivl=14
          TAIV(1)='  ------------'
          TAIV(2)='  Infiltration'
          TAIV(3)='              '
          TAIV(4)='              '
        else
          if(ilflag.eq.0)then
            imtaivl=9
            TAIV(1)='Infil-  |'
            TAIV(2)='tration |'
            TAIV(3)='        |'
            TAIV(4)='        |'
          else
            imtaivl=13
            SLAIV='Infiltration '
          endif
        endif
      elseif(imtaiv.eq.3)then
        if(markdown)then
          imtaivl=13
          TAIV(1)='  -----------'
          TAIV(2)='  Ventilation'
          TAIV(3)='             '
          TAIV(4)='             '
        else
          if(ilflag.eq.0)then
            imtaivl=9
            TAIV(1)='Venti-  |'
            TAIV(2)='lation  |'
            TAIV(3)='        |'
            TAIV(4)='        |'
          else
            imtaivl=12
            SLAIV='Ventilation '
          endif
        endif
      endif
      if(imtctl.eq.1)then
        if(markdown)then
          imtctll=18
          TCTL(1)='  -------  -------'
          TCTL(2)='  Heating  Cooling'
          TCTL(3)='                  '
          TCTL(4)='                  '
        else
          if(ilflag.eq.0)then
            imtctll=16
            TCTL(1)=' Environmental |'
            TCTL(2)='   controls    |'
            TCTL(3)='Heating|Cooling|'
            TCTL(4)='       |       |'
          else
            imtctll=16
            SLCTL='Heating Cooling '
          endif
        endif
      elseif(imtctl.eq.2)then
        if(markdown)then
          imtctll=9
          TCTL(1)='  -------'
          TCTL(2)='  Heating'
          TCTL(3)='         '
          TCTL(4)='         '
        else
          if(ilflag.eq.0)then
            imtctll=8
            TCTL(1)='Heating|'
            TCTL(2)='       |'
            TCTL(3)='       |'
            TCTL(4)='       |'
          else
            imtctll=8
            SLCTL='Heating '
          endif
        endif
      elseif(imtctl.eq.3)then
        if(markdown)then
          imtctll=9
          TCTL(1)='  -------'
          TCTL(2)='  Cooling'
          TCTL(3)='         '
          TCTL(4)='         '
        else
          if(ilflag.eq.0)then
            imtctll=8
            TCTL(1)='Cooling|'
            TCTL(2)='       |'
            TCTL(3)='       |'
            TCTL(4)='       |'
          else
            imtctll=8
            SLCTL='Cooling '
          endif
        endif
      endif
      if(imtsol.eq.1)then
        if(markdown)then
          imtsoll=18
          TSOL(1)='  ------- --------'
          TSOL(2)='  Solar   Solar   '
          TSOL(3)='  absorbd entering'
          TSOL(4)='  in zone zone    '
        else
          if(ilflag.eq.0)then
            imtsoll=17
            TSOL(1)='Solar radiation |'
            TSOL(2)='absorbd|entering|'
            TSOL(3)='within |the     |'
            TSOL(4)='zone   |zone    |'
          else
            imtsoll=29
            SLSOL='Solar-absorbed Solar-entring '
          endif
        endif
      elseif(imtsol.eq.2)then
        if(markdown)then
          imtsoll=9
          TSOL(1)='  -------'
          TSOL(2)='  Solar  '
          TSOL(3)='  absorbd'
          TSOL(4)='  in zone'
        else
          if(ilflag.eq.0)then
            imtsoll=8
            TSOL(1)='Solar  |'
            TSOL(2)='absorbd|'
            TSOL(3)='within |'
            TSOL(4)='zone   |'
          else
            imtsoll=15
            SLSOL='Solar-absorbed '
          endif
        endif
      elseif(imtsol.eq.3)then
        if(markdown)then
          imtsoll=10
          TSOL(1)='  --------'
          TSOL(2)='  Solar   '
          TSOL(3)='  entering'
          TSOL(4)='  zone    '
        else
          if(ilflag.eq.0)then
            imtsoll=9
            TSOL(1)='Solar   |'
            TSOL(2)='entering|'
            TSOL(3)='the     |'
            TSOL(4)='zone    |'
          else
            imtsoll=15
            SLSOL='Solar-entering '
          endif
        endif
      endif
      if(dmdsok)then
        if(markdown)then
          imdmdsl=10
          TDMD(1)=' ---------'
          TDMD(2)=' dispersed'
          TDMD(3)=' demands  '
          TDMD(4)=' for model'
        else
          if(ilflag.eq.0)then
            imdmdsl=9
            TDMD(1)='dispersed'
            TDMD(2)='demands  '
            TDMD(3)='for model'
            TDMD(4)='         '
          else
            imdmdsl=18
            SLDMD='Dispersed-demands '
          endif
        endif
      else      ! if no dispersed just a single blank
        imdmdsl=1
        TDMD(1)=' '
        TDMD(2)=' '
        TDMD(3)=' '
        TDMD(4)=' '
        SLDMD=' '
      endif

C Set header text and write it out if this has not yet been done.
      CALL HDDATE(PDESCR)
      CALL HDSTEP(SDESCR)
      call edisp(itru,' ')
      lsn1=MIN0(lnblnk(RFILE),42)
      if((.NOT.libheading).and.(.NOT.headingcontext))then
        libheading=.true.; headingcontext=.true.
        if(NSIM.gt.1)then
          if(markdown)then
            write(outs,'(2a)') '# Performance Report for ',
     &        RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
            call edisp(itru,outs)
    
            write(outs,'(3A,I4)')'### Library ',RFILE(1:lsn1),
     &        ' Set:',ISIM
            call edisp(itru,outs)
          else
            WRITE(outs,'(3A,I4,3A)')'Results library: ',RFILE(1:lsn1),
     &         '; results set:',ISIM,
     &        ' (',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM))),')'
            call edisp(itru,outs)
          endif
        else
          if(markdown)then
            write(outs,'(2a)') '# Performance Report for ',
     &        RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
            call edisp(itru,outs)
            write(outs,'(2A)')'### Library ',RFILE(1:lsn1)
            call edisp(itru,outs)
          else
            WRITE(outs,'(5A)')'Results library: ',RFILE(1:lsn1),
     &        '; (',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM))),')'
            call edisp(itru,outs)
          endif
        endif
        if(markdown)then
          write(outs,'(4A)')'### ',PDESCR(1:lnblnk(PDESCR)),' ',
     &      SDESCR(12:lnblnk(SDESCR))
        else
          write(outs,'(3A)')PDESCR(1:lnblnk(PDESCR)),' ',
     &      SDESCR(12:lnblnk(SDESCR))
        endif
        call edisp(itru,outs)
        call edisp(itru,' ')
      endif

C Echo header with units.
      if(IR.eq.1)then
        if(markdown)then
          call edisp(itru,
     &    '## Monthly selection of gains & losses')
          call edisp(itru,
     &    ': Monthly selection of gains & losses (to nearest 100Wh).')
        else
          call edisp(itru,
     &    ' Monthly selection of gains & losses (to nearest 100Wh).')
        endif
      endif
      if(IR.eq.2)then
        if(markdown)then
          call edisp(itru,
     &    '## Monthly selection of gains & losses')
          call edisp(itru,
     &    ': Monthly selection of gains & losses (to nearest kWh).')
        else
          call edisp(itru,
     &    ' Monthly selection of gains & losses (to nearest kWh).')
        endif
      endif

C For each zone in model sum the zone base areas and then get the
C fraction of model base area for the zone.
      overallm2=0.0
      DO izn=1,NCOMP
        overallm2=overallm2+ZBASEA(izn)
      ENDDO
      DO izn=1,NCOMP
        zonem2frac(izn)=ZBASEA(izn)/overallm2
      ENDDO
      write(outs,'(a,f7.2,a)') ' The model overall base area is ',
     &  overallm2,'m2.'
      if(.NOT.markdown) call edisp(itru,outs)
      call edisp(itru,' ')

C If current version, display monthly summary.
      if(izver.ge.4)then

C Monthly statistics of energy gains and losses.
        if(ilflag.eq.0)then
          write(outsl,'(7a)') ZPER(1)(1:imzperl),SHTR(1)(1:imshtrl),
     &      TCAS(1)(1:imtcasl),TAIV(1)(1:imtaivl),
     &      TCTL(1)(1:imtctll),TSOL(1)(1:imtsoll),TDMD(1)(1:imdmdsl)
          call edisp(itru,outsl)
          write(outsl,'(7a)') ZPER(2)(1:imzperl),SHTR(2)(1:imshtrl),
     &      TCAS(2)(1:imtcasl),TAIV(2)(1:imtaivl),
     &      TCTL(2)(1:imtctll),TSOL(2)(1:imtsoll),TDMD(2)(1:imdmdsl)
          call edisp(itru,outsl)
          write(outsl,'(7a)') ZPER(3)(1:imzperl),SHTR(3)(1:imshtrl),
     &      TCAS(3)(1:imtcasl),TAIV(3)(1:imtaivl),
     &      TCTL(3)(1:imtctll),TSOL(3)(1:imtsoll),TDMD(3)(1:imdmdsl)
          call edisp(itru,outsl)
          write(outsl,'(7a)') ZPER(4)(1:imzperl),SHTR(4)(1:imshtrl),
     &      TCAS(4)(1:imtcasl),TAIV(4)(1:imtaivl),
     &      TCTL(4)(1:imtctll),TSOL(4)(1:imtsoll),TDMD(4)(1:imdmdsl)
          call edisp(itru,outsl)
        else
          write(outsl,'(7a)') SLPER(1:imzperl),SLHTR(1:imshtrl),
     &      SLCAS(1:imtcasl),SLAIV(1:imtaivl),
     &      SLCTL(1:imtctll),SLSOL(1:imtsoll),SLDMD(1:imdmdsl)
          call eddisp(itru,outsl)
        endif

C Markdown -> docx does not accept intermediate horizontal rule in this
C format. Explore alternatives.
C        if(markdown)then   ! add the bottom ----- line for markdown
C          write(outsl,'(7a)') ZPER(1)(1:imzperl),SHTR(1)(1:imshtrl),
C     &      TCAS(1)(1:imtcasl),TAIV(1)(1:imtaivl),
C     &      TCTL(1)(1:imtctll),TSOL(1)(1:imtsoll),TDMD(1)(1:imdmdsl)
C          call edisp(itru,outsl)
C        endif
      endif

C Loop for 15 topics and then each month.
      DO 620 IJK=1,15
        DO 610 IMNTH=1,12
          TVAL(IJK,IMNTH)=0.0
  610   CONTINUE
  620 CONTINUE

C Per zone statistics.
      DO 660 KGET=1,NGET
        izn=IGETNO(KGET,2)
        DO 621 IJK=1,15
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
          if(ISAVE.lt.4)then
            CALL GOUTOPQ(JD,IZN,ISET)
            CALL FLTIAV(JD,GVAL,CQ,NEL)
            do ISTP=ISTART,IEND
              VAL(3)=VAL(3)+CQ(ISTP)
            enddo
            CALL GOPQIN(JD,IZN,ISET)
            CALL FLTIAV(JD,GVAL,CQ,NEL)
            do ISTP=ISTART,IEND
              VAL(4)=VAL(4)+CQ(ISTP)
            enddo
            CALL GOUTTRN(JD,IZN,ISET)
            CALL FLTIAV(JD,GVAL,CQ,NEL)
            do ISTP=ISTART,IEND
              VAL(1)=VAL(1)+CQ(ISTP)
            enddo
            CALL GTRNIN(JD,IZN,ISET)
            CALL FLTIAV(JD,GVAL,CQ,NEL)
            do ISTP=ISTART,IEND
              VAL(2)=VAL(2)+CQ(ISTP)
            enddo
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
                  if(SOTF(IZN,ISN)(1:4).EQ.'OPAQ')then
                    VAL(3)=VAL(3)+CQOUT(ISTP)*(-1.0)
                  else
                    VAL(1)=VAL(1)+CQOUT(ISTP)*(-1.0)
                  endif
                ELSE
                  if(SOTF(IZN,ISN)(1:4).EQ.'OPAQ')then
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

          if(izver.ge.4)then

C Get seperate casual gain total rad+convec. NOTE: this has been tested
C for use with zone operations - but not yet temporal.
            DO J2=ISTART,IEND
              call getallcas(JD,IZN,ISET,J2,QCASR,QCASC,QCASL,FRAC,
     &          perocupc,perlightc,perequipc,perotherc,perocupr,
     &          perlightr,perequipr,perotherr,perocupl,perlightl,
     &          perequipl,perotherl,theonectld)
              CASDAY(J2,1)=perocupc+perocupr ! Total occupant gain (W)
              CASDAY(J2,2)=perlightc+perlightr ! Total lighting gain (W)
              CASDAY(J2,3)=perequipc+perequipr ! Total small power gain (W)
              CASDAY(J2,4)=perotherc+perotherr ! Total other gain (W)
            ENDDO

C Pass across the occupant gains into gval and then filter and sum.
            DO J2=ISTART,IEND
              GVAL(J2)=CASDAY(J2,1)
            ENDDO
            CALL FLTIAV(JD,GVAL,CQ,NEL)
            CALL SUM1V(CQ,ISTART,IEND,SGT,SLT,IGT,IEQ,ILT)
            VAL(5)=VAL(5)+SGT+SLT

C Pass across the lighiting gains into gval and then filter and sum.
            DO J2=ISTART,IEND
              GVAL(J2)=CASDAY(J2,2)
            ENDDO
            CALL FLTIAV(JD,GVAL,CQ,NEL)
            CALL SUM1V(CQ,ISTART,IEND,SGT,SLT,IGT,IEQ,ILT)
            VAL(6)=VAL(6)+SGT+SLT

C Pass across the small power gains into gval and then filter and sum.
            DO J2=ISTART,IEND
              GVAL(J2)=CASDAY(J2,3)
            ENDDO
            CALL FLTIAV(JD,GVAL,CQ,NEL)
            CALL SUM1V(CQ,ISTART,IEND,SGT,SLT,IGT,IEQ,ILT)
            VAL(7)=VAL(7)+SGT+SLT

C Pass across the other slot gains into gval and then filter and sum.
            DO J2=ISTART,IEND
              GVAL(J2)=CASDAY(J2,4)
            ENDDO
            CALL FLTIAV(JD,GVAL,CQ,NEL)
            CALL SUM1V(CQ,ISTART,IEND,SGT,SLT,IGT,IEQ,ILT)
            VAL(8)=VAL(8)+SGT+SLT

C Infiltration.
            CALL GQV1(JD,IZN,ISET)
            CALL FLTIAV(JD,GVAL,CQ,NEL)
            CALL SUM1V(CQ,ISTART,IEND,SGT,SLT,IGT,IEQ,ILT)
            VAL(9)=VAL(9)+SGT+SLT

C Ventilation.
            CALL GQV2(JD,IZN,ISET)
            CALL FLTIAV(JD,GVAL,CQ,NEL)
            CALL SUM1V(CQ,ISTART,IEND,SGT,SLT,IGT,IEQ,ILT)
            VAL(10)=VAL(10)+SGT+SLT

C Convection portion of plant.
            CALL GZQM(JD,IZN,ISET)
            CALL FLTIAV(JD,GVAL,CQ,NEL)
            CALL SUM1V(CQ,ISTART,IEND,SGT,SLT,IGT,IEQ,ILT)
            VAL(11)=VAL(11)+SGT
            VAL(12)=VAL(12)+SLT

C Solar absorbed in the zone.
            call GSOLABS(JD,IZN,ISET)
            CALL FLTIAV(JD,GVAL,CQ,NEL)
            CALL SUM1V(CQ,ISTART,IEND,SGT,SLT,IGT,IEQ,ILT)
            VAL(13)=VAL(13)+SGT

C Solar entering zone from outside.
            call GSOLX(JD,IZN,ISET)
            CALL FLTIAV(JD,GVAL,CQ,NEL)
            CALL SUM1V(CQ,ISTART,IEND,SGT,SLT,IGT,IEQ,ILT)
            VAL(14)=VAL(14)+SGT

C If dispersed demands (via VAL3 arrays) sum them to VAL(15).
C As dispersed demands are for whole model in the reporting
C for each zone factor it by the base area of that zone.
            if(dmdsok)then
              call DDMDS(JD)
              DO J2=ISTART,IEND
                GVAL(J2)=VAL3(15,J2)+VAL3(16,J2)+VAL3(17,J2)+
     &            VAL3(18,J2)+VAL3(19,J2)+VAL3(20,J2)+VAL3(21,J2)
              ENDDO
              CALL FLTIAV(JD,GVAL,CQ,NEL)
              CALL SUM1V(CQ,ISTART,IEND,SGT,SLT,IGT,IEQ,ILT)
              VAL(15)=VAL(15)+(SGT*zonem2frac(izn))
            else
              VAL(15)=0.0
            endif

          endif

C Check if month finished.
          IF(JD.LT.NDYMTH(IMNTH).AND.JD.NE.IODF)goto 650

C Dump this month's statistics.
          DO IJK=1,15
            VAL(IJK)=VAL(IJK)/1000.
          ENDDO

          if(dmdsok)then  ! Loop limits for TVAL summations.
            looplimit=15
          else
            looplimit=14
          endif

C Take topics in order to build the outsl string.
          IF(IMNTH.EQ.IOM1)THEN   ! Include zone name.
            if(markdown)then
              write(outsl,'(1X,a,1x,a,2x)') 
     &          zname(IZN)(1:12),NMTHNM(IMNTH)
              K=lnblnk(outsl)+3      ! Initial value for next tranche.
            else
              write(outsl,'(1X,a,1x,a3,1x)') 
     &          zname(IZN)(1:12),NMTHNM(IMNTH)
              K=lnblnk(outsl)+2      ! Initial value for next tranche.
            endif
          else
            if(markdown)then
              write(outsl,'(14X,a,2x)') NMTHNM(IMNTH)
              K=lnblnk(outsl)+3      ! Initial value for next tranche.
            else
              if(ilflag.eq.0)then
                write(outsl,'(14X,a3,1x)') NMTHNM(IMNTH)
              else
                write(outsl,'(a,a3,1x)') '-- ',NMTHNM(IMNTH)
              endif
              K=lnblnk(outsl)+2      ! Initial value for next tranche.
            endif
          endif
          if(imshtr.eq.1)then    ! Surface heat transfer 4 columns.
            if(IR.eq.1)then      ! Resolution to 100Whr.
              if(markdown)then
                KE=K+44
                write(outsl(k:ke),'(4(F11.1))') (VAL(IJ),IJ=1,4)
              else
                KE=K+32
                write(outsl(k:ke),'(4(F8.1))') (VAL(IJ),IJ=1,4)
              endif
            elseif(IR.eq.2)then  ! Resolution to nearest kWhr.
              if(markdown)then
                KE=K+44
                write(outsl(k:ke),'(4(F11.0))') (VAL(IJ),IJ=1,4)
              else
                KE=K+32
                write(outsl(k:ke),'(4(F8.0))') (VAL(IJ),IJ=1,4)
              endif
            endif
          elseif(imshtr.eq.2)then
            if(IR.eq.1)then      ! Resolution to 100Whr.
              if(markdown)then
                KE=K+22
                write(outsl(k:ke),'(2(F11.1))') VAL(1),VAL(3)
              else
                KE=K+19
                write(outsl(k:ke),'(F9.1,F10.1)') VAL(1),VAL(3)
              endif
            elseif(IR.eq.2)then  ! Resolution to nearest kWhr.
              if(markdown)then
                KE=K+22
                write(outsl(k:ke),'(2(F11.0))') VAL(1),VAL(3)
              else
                KE=K+19
                write(outsl(k:ke),'(F9.0,F10.0)') VAL(1),VAL(3)
              endif
            endif
          endif
          K=KE
          if(imtcas.eq.1)then
            if(IR.eq.1)then      ! Resolution to 100Whr.
              if(markdown)then
                KE=K+38
                write(outsl(k:ke),'(2(F9.1),2(F10.1))') (VAL(IJ),IJ=5,8)
              else
                KE=K+32
                write(outsl(k:ke),'(4(F8.1))') (VAL(IJ),IJ=5,8)
              endif
            elseif(IR.eq.2)then  ! Resolution to nearest kWhr.
              if(markdown)then
                KE=K+38
                write(outsl(k:ke),'(2F9.0,2F10.0)') (VAL(IJ),IJ=5,8)
              else
                KE=K+32
                write(outsl(k:ke),'(4(F8.0))') (VAL(IJ),IJ=5,8)
              endif
            endif
          elseif(imtcas.eq.2)then
            CASVAL=VAL(5)+VAL(6)+VAL(7)+VAL(8)
            if(IR.eq.1)then      ! Resolution to 100Whr.
              if(markdown)then
                KE=K+10
                write(outsl(k:ke),'(F10.1)') CASVAL
              else
                KE=K+9
                write(outsl(k:ke),'(F9.1)') CASVAL
              endif
            elseif(IR.eq.2)then  ! Resolution to nearest kWhr.
              if(markdown)then
                KE=K+10
                write(outsl(k:ke),'(F10.0)') CASVAL
              else
                KE=K+9
                write(outsl(k:ke),'(F9.0)') CASVAL
              endif
            endif
          endif
          K=KE
          if(imtaiv.eq.1)then
            if(IR.eq.1)then      ! Resolution to 100Whr.
              if(markdown)then
                KE=K+26
                write(outsl(k:ke),'(2(F13.1))') VAL(9),VAL(10)
              else
                KE=K+16
                write(outsl(k:ke),'(2(F8.1))') VAL(9),VAL(10)
              endif
            elseif(IR.eq.2)then  ! Resolution to nearest kWhr.
              if(markdown)then
                KE=K+26
                write(outsl(k:ke),'(2(F13.0))') VAL(9),VAL(10)
              else
                KE=K+16
                write(outsl(k:ke),'(2(F8.0))') VAL(9),VAL(10)
              endif
            endif
          elseif(imtaiv.eq.2)then
            if(IR.eq.1)then      ! Resolution to 100Whr.
              if(markdown)then
                KE=K+13
                write(outsl(k:ke),'(F13.1)') VAL(9)
              else
                KE=K+9
                write(outsl(k:ke),'(F9.1)') VAL(9)
              endif
            elseif(IR.eq.2)then  ! Resolution to nearest kWhr.
              if(markdown)then
                KE=K+13
                write(outsl(k:ke),'(F13.0)') VAL(9)
              else
                KE=K+9
                write(outsl(k:ke),'(F9.0)') VAL(9)
              endif
            endif
          elseif(imtaiv.eq.3)then
            if(IR.eq.1)then      ! Resolution to 100Whr.
              if(markdown)then
                KE=K+13
                write(outsl(k:ke),'(F13.1)') VAL(10)
              else
                KE=K+9
                write(outsl(k:ke),'(F9.1)') VAL(10)
              endif
            elseif(IR.eq.2)then  ! Resolution to nearest kWhr.
              if(markdown)then
                KE=K+13
                write(outsl(k:ke),'(F13.0)') VAL(10)
              else
                KE=K+9
                write(outsl(k:ke),'(F9.0)') VAL(10)
              endif
            endif
          endif
          K=KE
          if(imtctl.eq.1)then
            if(IR.eq.1)then      ! Resolution to 100Whr.
              if(markdown)then
                KE=K+18
                write(outsl(k:ke),'(2(F9.1))') VAL(11),VAL(12)
              else
                KE=K+16
                write(outsl(k:ke),'(2(F8.1))') VAL(11),VAL(12)
              endif
            elseif(IR.eq.2)then  ! Resolution to nearest kWhr.
              if(markdown)then
                KE=K+18
                write(outsl(k:ke),'(2(F9.0))') VAL(11),VAL(12)
              else
                KE=K+16
                write(outsl(k:ke),'(2(F8.0))') VAL(11),VAL(12)
              endif
            endif
          elseif(imtctl.eq.2)then
            if(IR.eq.1)then      ! Resolution to 100Whr.
              if(markdown)then
                KE=K+9
                write(outsl(k:ke),'(F9.1)') VAL(11)
              else
                KE=K+8
                write(outsl(k:ke),'(F8.1)') VAL(11)
              endif
            elseif(IR.eq.2)then  ! Resolution to nearest kWhr.
              if(markdown)then
                KE=K+9
                write(outsl(k:ke),'(F9.0)') VAL(11)
              else
                KE=K+8
                write(outsl(k:ke),'(F8.0)') VAL(11)
              endif
            endif
          elseif(imtctl.eq.3)then
            if(IR.eq.1)then      ! Resolution to 100Whr.
              if(markdown)then
                KE=K+9
                write(outsl(k:ke),'(F9.1)') VAL(12)
              else
                KE=K+8
                write(outsl(k:ke),'(F8.1)') VAL(12)
              endif
            elseif(IR.eq.2)then  ! Resolution to nearest kWhr.
              if(markdown)then
                KE=K+9
                write(outsl(k:ke),'(F9.0)') VAL(12)
              else
                KE=K+8
                write(outsl(k:ke),'(F8.0)') VAL(12)
              endif
            endif
          endif
          K=KE
          if(imtsol.eq.1)then
            if(IR.eq.1)then      ! Resolution to 100Whr.
              if(markdown)then
                KE=K+18
                write(outsl(k:ke),'(2(F9.1))') VAL(13),VAL(14)
              else
                KE=K+16
                write(outsl(k:ke),'(2(F8.1))') VAL(13),VAL(14)
              endif
            elseif(IR.eq.2)then  ! Resolution to nearest kWhr.
              if(markdown)then
                KE=K+18
                write(outsl(k:ke),'(2(F9.0))') VAL(13),VAL(14)
              else
                KE=K+16
                write(outsl(k:ke),'(2(F8.0))') VAL(13),VAL(14)
              endif
            endif
          elseif(imtsol.eq.2)then
            if(IR.eq.1)then      ! Resolution to 100Whr.
              if(markdown)then
                KE=K+9
                write(outsl(k:ke),'(F9.1)') VAL(13)
              else
                KE=K+8
                write(outsl(k:ke),'(F8.1)') VAL(13)
              endif
            elseif(IR.eq.2)then  ! Resolution to nearest kWhr.
              if(markdown)then
                KE=K+9
                write(outsl(k:ke),'(F9.0)') VAL(13)
              else
                KE=K+8
                write(outsl(k:ke),'(F8.0)') VAL(13)
              endif
            endif
          elseif(imtsol.eq.3)then
            if(IR.eq.1)then      ! Resolution to 100Whr.
              if(markdown)then
                KE=K+9
                write(outsl(k:ke),'(F9.1)') VAL(14)
              else
                KE=K+8
                write(outsl(k:ke),'(F8.1)') VAL(14)
              endif
            elseif(IR.eq.2)then  ! Resolution to nearest kWhr.
              if(markdown)then
                KE=K+9
                write(outsl(k:ke),'(F9.0)') VAL(14)
              else
                KE=K+8
                write(outsl(k:ke),'(F8.0)') VAL(14)
              endif
            endif
          endif
          K=KE
          if(dmdsok)then
            if(markdown)then
            else
            endif
          endif
          call eddisp(itru,outsl)  ! Display the month's data.
          DO IJK=1,looplimit       ! Update TVAL.
            TVAL(IJK,IMNTH)=TVAL(IJK,IMNTH)+VAL(IJK)
            VAL(IJK)=0.0
          ENDDO
          IMNTH=IMNTH+1
  650   CONTINUE
  660 CONTINUE

C Dump 'all-zone' statistics if more than one zone.
      DO 681 IMNTH=IOM1,IOM2
        if (NZ.GT.1) then
          if(IMNTH.EQ.IOM1)then   ! Include zone name.
            if(markdown)then
              write(outsl,'(2a,2x)') ' All zones    ',NMTHNM(IMNTH)
              K=lnblnk(outsl)+2      ! initial value for next tranche
            else
              if(ilflag.eq.0)then
                write(outsl,'(2a,1x)') ' All zones    ',NMTHNM(IMNTH)
              else
                write(outsl,'(2a,1x)') ' All ',NMTHNM(IMNTH)
              endif
              K=lnblnk(outsl)+2      ! initial value for next tranche
            endif
          else
            if(markdown)then
              write(outsl,'(2a,2x)') '              ',NMTHNM(IMNTH)
              K=lnblnk(outsl)+3      ! initial value for next tranche
            else
              if(ilflag.eq.0)then
                write(outsl,'(14X,a3,1x)') NMTHNM(IMNTH)
              else
                write(outsl,'(a,a3,1x)') '-- ',NMTHNM(IMNTH)
              endif
              K=lnblnk(outsl)+2      ! initial value for next tranche
            endif
          endif
          if(imshtr.eq.1)then    ! Surface heat transfer 4 columns.
            if(markdown)then
              KE=K+44
              write(outsl(k:ke),'(4(F11.0))') (TVAL(IJK,IMNTH),IJK=1,4)
            else
              KE=K+32
              write(outsl(k:ke),'(4(F8.0))') (TVAL(IJK,IMNTH),IJK=1,4)
            endif
          elseif(imshtr.eq.2)then
            if(markdown)then
              KE=K+22
              write(outsl(k:ke),'(2(F11.0))') TVAL(1,IMNTH),
     &          TVAL(3,IMNTH)
            else
              KE=K+19
              write(outsl(k:ke),'(F9.0,F10.0)') TVAL(1,IMNTH),
     &          TVAL(3,IMNTH)
            endif
          endif
          K=KE
          if(imtcas.eq.1)then
            if(markdown)then
              KE=K+38
              write(outsl(k:ke),'(2F9.0,2F10.0)') 
     &          (TVAL(IJK,IMNTH),IJK=5,8)
            else
              KE=K+32
              write(outsl(k:ke),'(4(F8.0))') 
     &          (TVAL(IJK,IMNTH),IJK=5,8)
            endif
          elseif(imtcas.eq.2)then
            CASVAL=TVAL(5,IMNTH)+TVAL(6,IMNTH)+TVAL(7,IMNTH)+
     &             TVAL(8,IMNTH)
            if(markdown)then
              KE=K+10
              write(outsl(k:ke),'(F10.0)') CASVAL
            else
              KE=K+9
              write(outsl(k:ke),'(F9.0)') CASVAL
            endif
          endif
          K=KE
          if(imtaiv.eq.1)then
            if(markdown)then
              KE=K+26
              write(outsl(k:ke),'(2(F13.0))') TVAL(9,IMNTH),
     &          TVAL(10,IMNTH)
            else
              KE=K+16
              write(outsl(k:ke),'(2(F8.0))') TVAL(9,IMNTH),
     &          TVAL(10,IMNTH)
            endif
          elseif(imtaiv.eq.2)then
            if(markdown)then
              KE=K+13
              write(outsl(k:ke),'(F13.0)') TVAL(9,IMNTH)
            else
              KE=K+9
              write(outsl(k:ke),'(F9.0)') TVAL(9,IMNTH)
            endif
          elseif(imtaiv.eq.3)then
            if(markdown)then
              KE=K+13
              write(outsl(k:ke),'(F13.0)') TVAL(10,IMNTH)
            else
              KE=K+9
              write(outsl(k:ke),'(F9.0)') TVAL(10,IMNTH)
            endif
          endif
          K=KE
          if(imtctl.eq.1)then
            if(markdown)then
              KE=K+18
              write(outsl(k:ke),'(2(F9.0))') TVAL(11,IMNTH),
     &          TVAL(12,IMNTH)
            else
              KE=K+16
              write(outsl(k:ke),'(2(F8.0))') TVAL(11,IMNTH),
     &          TVAL(12,IMNTH)
            endif
          elseif(imtctl.eq.2)then
            if(markdown)then
              KE=K+9
              write(outsl(k:ke),'(F9.0)') TVAL(11,IMNTH)
            else
              KE=K+8
              write(outsl(k:ke),'(F8.0)') TVAL(11,IMNTH)
            endif
          elseif(imtctl.eq.3)then
            if(markdown)then
              KE=K+9
              write(outsl(k:ke),'(F9.0)') TVAL(12,IMNTH)
            else
              KE=K+8
              write(outsl(k:ke),'(F8.0)') TVAL(12,IMNTH)
            endif
          endif
          K=KE
          if(imtsol.eq.1)then
            if(markdown)then
              KE=K+18
              write(outsl(k:ke),'(2(F9.0))') TVAL(13,IMNTH),
     &          TVAL(14,IMNTH)
            else
              KE=K+16
              write(outsl(k:ke),'(2(F8.0))') TVAL(13,IMNTH),
     &          TVAL(14,IMNTH)
            endif
          elseif(imtsol.eq.2)then
            if(markdown)then
              KE=K+9
              write(outsl(k:ke),'(F9.0)') TVAL(13,IMNTH)
            else
              KE=K+8
              write(outsl(k:ke),'(F8.0)') TVAL(13,IMNTH)
            endif
          elseif(imtsol.eq.3)then
            if(markdown)then
              KE=K+9
              write(outsl(k:ke),'(F9.0)') TVAL(14,IMNTH)
            else
              KE=K+8
              write(outsl(k:ke),'(F8.0)') TVAL(14,IMNTH)
            endif
          endif
C          write(6,'(2i4)') K,KE
C          write(6,'(a)') outsl(1:lnblnk(outsl))
          K=KE
          call eddisp(itru,outsl)   ! Dump the all zones data
        endif
  681 continue
      if(dmdsok)then
        looplimit=15
      else
        looplimit=14
      endif
      DO 680 IMNTH=IOM1,IOM2   !  Detect greatest magnitude.
        biggest=0.0
        DO IJK=1,looplimit
          VAL(IJK)=VAL(IJK)+TVAL(IJK,IMNTH)
          if(VAL(IJK).gt.biggest) biggest=VAL(IJK)
        ENDDO
  680 CONTINUE

C Dump the annual totals if run was for more than one month.
      if (IOM2.GT.IOM1) then
        if(markdown)then
          write(outsl,'(a,2x)') ' Annual             '
          K=20      ! initial value for next tranche
        else
          if(ilflag.eq.0)then
            write(outsl,'(a,1x)') ' Annual            '
            K=19      ! initial value for next tranche
          else
            write(outsl,'(a,1x)') '-- Annual '
            K=10      ! initial value for next tranche
          endif
        endif

        if(biggest.gt.999999.0)then
          if(imshtr.eq.1)then    ! Surface heat transfer 4 columns
            if(markdown)then
              KE=K+48
              write(outsl(k:ke),'(4(F12.0))') (VAL(IJK),IJK=1,4)
            else
              KE=K+40
              write(outsl(k:ke),'(4(F10.0))') (VAL(IJK),IJK=1,4)
            endif
          elseif(imshtr.eq.2)then
            if(markdown)then
              KE=K+24
              write(outsl(k:ke),'(2(F12.0))') VAL(1),VAL(3)
            else
              KE=K+19
              write(outsl(k:ke),'(F9.0,F10.0)') VAL(1),VAL(3)
            endif
          endif
          K=KE
          if(imtcas.eq.1)then
            if(markdown)then
              KE=K+38
              write(outsl(k:ke),'(2F9.0,2F10.0)') 
     &          (VAL(IJK),IJK=5,8)
            else
              KE=K+32
              write(outsl(k:ke),'(4(F8.0))') 
     &          (VAL(IJK),IJK=5,8)
            endif
          elseif(imtcas.eq.2)then
            CASVAL=VAL(5)+VAL(6)+VAL(7)+VAL(8)
            if(markdown)then
              KE=K+10
              write(outsl(k:ke),'(F10.0)') CASVAL
            else
              KE=K+9
              write(outsl(k:ke),'(F9.0)') CASVAL
            endif
          endif
          K=KE
          if(imtaiv.eq.1)then
            if(markdown)then
              KE=K+26
              write(outsl(k:ke),'(2(F13.0))') VAL(9),VAL(10)
            else
              KE=K+16
              write(outsl(k:ke),'(2(F8.0))') VAL(9),VAL(10)
            endif
          elseif(imtaiv.eq.2)then
            if(markdown)then
              KE=K+13
              write(outsl(k:ke),'(F13.0)') VAL(9)
            else
              KE=K+9
              write(outsl(k:ke),'(F9.0)') VAL(9)
            endif
          elseif(imtaiv.eq.3)then
            if(markdown)then
              KE=K+13
              write(outsl(k:ke),'(F13.0)') VAL(10)
            else
              KE=K+9
              write(outsl(k:ke),'(F9.0)') VAL(10)
            endif
          endif
          K=KE
          if(imtctl.eq.1)then
            if(markdown)then
              KE=K+18
              write(outsl(k:ke),'(2(F9.0))') VAL(11),VAL(12)
            else
              KE=K+16
              write(outsl(k:ke),'(2(F8.0))') VAL(11),VAL(12)
            endif
          elseif(imtctl.eq.2)then
            if(markdown)then
              KE=K+9
              write(outsl(k:ke),'(F9.0)') VAL(11)
            else
              KE=K+8
              write(outsl(k:ke),'(F8.0)') VAL(11)
            endif
          elseif(imtctl.eq.3)then
            if(markdown)then
              KE=K+9
              write(outsl(k:ke),'(F9.0)') VAL(12)
            else
              KE=K+8
              write(outsl(k:ke),'(F8.0)') VAL(12)
            endif
          endif
          K=KE
          if(imtsol.eq.1)then
            if(markdown)then
              KE=K+18
              write(outsl(k:ke),'(2(F9.0))') VAL(13),VAL(14)
            else
              KE=K+16
              write(outsl(k:ke),'(2(F8.0))') VAL(13),VAL(14)
            endif
          elseif(imtsol.eq.2)then
            if(markdown)then
              KE=K+9
              write(outsl(k:ke),'(F9.0)') VAL(13)
            else
              KE=K+8
              write(outsl(k:ke),'(F8.0)') VAL(13)
            endif
          elseif(imtsol.eq.3)then
            if(markdown)then
              KE=K+9
              write(outsl(k:ke),'(F9.0)') VAL(14)
            else
              KE=K+8
              write(outsl(k:ke),'(F8.0)') VAL(14)
            endif
          endif
          K=KE
          call eddisp(itru,outsl)   ! Dump the all zones data
        elseif(biggest.gt.99999.0.and.biggest.lt.999999.0)then
          if(imshtr.eq.1)then       ! Surface heat transfer 4 columns
            if(markdown)then
              KE=K+44
              write(outsl(k:ke),'(4(F11.0))') (VAL(IJK),IJK=1,4)
            else
              KE=K+32
              write(outsl(k:ke),'(4(F8.0))') (VAL(IJK),IJK=1,4)
            endif
          elseif(imshtr.eq.2)then
            if(markdown)then
              KE=K+22
              write(outsl(k:ke),'(2(F11.0))') VAL(1),VAL(3)
            else
              KE=K+19
              write(outsl(k:ke),'(F9.0,F10.0)') VAL(1),VAL(3)
            endif
          endif
          K=KE
          if(imtcas.eq.1)then
            if(markdown)then
              KE=K+38
              write(outsl(k:ke),'(2F9.0,2F10.0)') 
     &          (VAL(IJK),IJK=5,8)
            else
              KE=K+32
              write(outsl(k:ke),'(4(F8.0))') 
     &          (VAL(IJK),IJK=5,8)
            endif
          elseif(imtcas.eq.2)then
            CASVAL=VAL(5)+VAL(6)+VAL(7)+VAL(8)
            if(markdown)then
              KE=K+10
              write(outsl(k:ke),'(F10.0)') CASVAL
            else
              KE=K+9
              write(outsl(k:ke),'(F9.0)') CASVAL
            endif
          endif
          K=KE
          if(imtaiv.eq.1)then
            if(markdown)then
              KE=K+26
              write(outsl(k:ke),'(2(F13.0))') VAL(9),VAL(10)
            else
              KE=K+16
              write(outsl(k:ke),'(2(F8.0))') VAL(9),VAL(10)
            endif
          elseif(imtaiv.eq.2)then
            if(markdown)then
              KE=K+13
              write(outsl(k:ke),'(F13.0)') VAL(9)
            else
              KE=K+9
              write(outsl(k:ke),'(F9.0)') VAL(9)
            endif
          elseif(imtaiv.eq.3)then
            if(markdown)then
              KE=K+13
              write(outsl(k:ke),'(F13.0)') VAL(10)
            else
              KE=K+9
              write(outsl(k:ke),'(F9.0)') VAL(10)
            endif
          endif
          K=KE
          if(imtctl.eq.1)then
            if(markdown)then
              KE=K+18
              write(outsl(k:ke),'(2(F9.0))') VAL(11),VAL(12)
            else
              KE=K+16
              write(outsl(k:ke),'(2(F8.0))') VAL(11),VAL(12)
            endif
          elseif(imtctl.eq.2)then
            if(markdown)then
              KE=K+9
              write(outsl(k:ke),'(F9.0)') VAL(11)
            else
              KE=K+8
              write(outsl(k:ke),'(F8.0)') VAL(11)
            endif
          elseif(imtctl.eq.3)then
            if(markdown)then
              KE=K+9
              write(outsl(k:ke),'(F9.0)') VAL(12)
            else
              KE=K+8
              write(outsl(k:ke),'(F8.0)') VAL(12)
            endif
          endif
C          write(6,'(2i4)') K,KE
C          write(6,'(a)') outsl(1:lnblnk(outsl))
          K=KE
          if(imtsol.eq.1)then
            if(markdown)then
              KE=K+18
              write(outsl(k:ke),'(2(F9.0))') VAL(13),VAL(14)
            else
              KE=K+16
              write(outsl(k:ke),'(2(F8.0))') VAL(13),VAL(14)
            endif
          elseif(imtsol.eq.2)then
            if(markdown)then
              KE=K+9
              write(outsl(k:ke),'(F9.0)') VAL(13)
            else
              KE=K+8
              write(outsl(k:ke),'(F8.0)') VAL(13)
            endif
          elseif(imtsol.eq.3)then
            if(markdown)then
              KE=K+9
              write(outsl(k:ke),'(F9.0)') VAL(14)
            else
              KE=K+8
              write(outsl(k:ke),'(F8.0)') VAL(14)
            endif
          endif
          K=KE
          call eddisp(itru,outsl)   ! Dump the all zones data
        else                        ! Same spacing as for All zones
          if(imshtr.eq.1)then       ! Surface heat transfer 4 columns
            if(markdown)then
              KE=K+44
              write(outsl(k:ke),'(4(F11.0))') (VAL(IJK),IJK=1,4)
            else
              KE=K+32
              write(outsl(k:ke),'(4(F8.0))') (VAL(IJK),IJK=1,4)
            endif
          elseif(imshtr.eq.2)then    ! Surface heat transfer @ facade
            if(markdown)then
              KE=K+22
              write(outsl(k:ke),'(2(F11.0))') VAL(1),VAL(3)
            else
              KE=K+19
              write(outsl(k:ke),'(F9.0,F10.0)') VAL(1),VAL(3)
            endif
          endif
          K=KE
          if(imtcas.eq.1)then
            if(markdown)then
              KE=K+38
              write(outsl(k:ke),'(2F9.0,2F10.0)') 
     &          (VAL(IJK),IJK=5,8)
            else
              KE=K+32
              write(outsl(k:ke),'(4(F8.0))') 
     &          (VAL(IJK),IJK=5,8)
            endif
          elseif(imtcas.eq.2)then
            CASVAL=VAL(5)+VAL(6)+VAL(7)+VAL(8)
            if(markdown)then
              KE=K+10
              write(outsl(k:ke),'(F10.0)') CASVAL
            else
              KE=K+9
              write(outsl(k:ke),'(F9.0)') CASVAL
            endif
          endif
          K=KE
          if(imtaiv.eq.1)then
            if(markdown)then
              KE=K+26
              write(outsl(k:ke),'(2(F13.0))') VAL(9),VAL(10)
            else
              KE=K+16
              write(outsl(k:ke),'(2(F8.0))') VAL(9),VAL(10)
            endif
          elseif(imtaiv.eq.2)then
            if(markdown)then
              KE=K+13
              write(outsl(k:ke),'(F13.0)') VAL(9)
            else
              KE=K+9
              write(outsl(k:ke),'(F9.0)') VAL(9)
            endif
          elseif(imtaiv.eq.3)then
            if(markdown)then
              KE=K+13
              write(outsl(k:ke),'(F13.0)') VAL(10)
            else
              KE=K+9
              write(outsl(k:ke),'(F9.0)') VAL(10)
            endif
          endif
          K=KE
          if(imtctl.eq.1)then
            if(markdown)then
              KE=K+18
              write(outsl(k:ke),'(2(F9.0))') VAL(11),VAL(12)
            else
              KE=K+16
              write(outsl(k:ke),'(2(F8.0))') VAL(11),VAL(12)
            endif
          elseif(imtctl.eq.2)then
            if(markdown)then
              KE=K+9
              write(outsl(k:ke),'(F9.0)') VAL(11)
            else
              KE=K+8
              write(outsl(k:ke),'(F8.0)') VAL(11)
            endif
          elseif(imtctl.eq.3)then
            if(markdown)then
              KE=K+9
              write(outsl(k:ke),'(F9.0)') VAL(12)
            else
              KE=K+8
              write(outsl(k:ke),'(F8.0)') VAL(12)
            endif
          endif
          K=KE
          if(imtsol.eq.1)then
            if(markdown)then
              KE=K+18
              write(outsl(k:ke),'(2(F9.0))') VAL(13),VAL(14)
            else
              KE=K+16
              write(outsl(k:ke),'(2(F8.0))') VAL(13),VAL(14)
            endif
          elseif(imtsol.eq.2)then
            if(markdown)then
              KE=K+9
              write(outsl(k:ke),'(F9.0)') VAL(13)
            else
              KE=K+8
              write(outsl(k:ke),'(F8.0)') VAL(13)
            endif
          elseif(imtsol.eq.3)then
            if(markdown)then
              KE=K+9
              write(outsl(k:ke),'(F9.0)') VAL(14)
            else
              KE=K+8
              write(outsl(k:ke),'(F8.0)') VAL(14)
            endif
          endif
          K=KE
          call eddisp(itru,outsl)   ! Dump the all zones data
        endif
      endif

      if(markdown)then   ! add the bottom ----- line for markdown
        write(outsl,'(7a)') ZPER(1)(1:imzperl),SHTR(1)(1:imshtrl),
     &    TCAS(1)(1:imtcasl),TAIV(1)(1:imtaivl),
     &    TCTL(1)(1:imtctll),TSOL(1)(1:imtsoll),TDMD(1)(1:imdmdsl)
        call edisp(itru,outsl)
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
C of zone mean radiant temperature. Returns value via XDUM1 
C in common block GET2.
C CALLS: GZTMS.

      SUBROUTINE CZMRT(IDAY,IZONE,ISET)
#include "building.h"
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
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
C zone resultant temperature.  Returns value via XDUM1 in common GET2.
C CALLS:  GZTMS.

      SUBROUTINE CZRESL(IDAY,IZONE,ISET)
#include "building.h"
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
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

