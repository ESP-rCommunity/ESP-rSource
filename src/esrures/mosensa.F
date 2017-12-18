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

C multisets.F contains the following:

C MOSA    Controls the retrieving of data for the purposes 
C         of a uncertainty analysis.
C SAINTRA calculates summary statistics within result sets.
C SAINTER calculates summary statistics within result sets.
C SAPARSEN calculates individual parameter uncertainties.
C SANOVA calculates summary statistics between result sets - most
C        importantly an analysis of variance table.
C DIFFOW calculates differences between data lists for the construction 
C        of a one way analysis of variance table for NT treatments. 
C WHSET displays currently chosen sets and lets user toggle selections.
C FACTORIAL calculate n (function)!
C INTERACT calculate the all the interactions given an interaction 
C          of NINTER parameters. For NPAR parameters.

C ******************** MOSA ********************
C MOSA presents the user with a table of options to apply a uncertainty 
C analysis calculation on a results library. The result sets, zone of 
C interest and output metric must be specified before the calculations 
C can proceed.

      SUBROUTINE MOSA
#include "building.h"
#include "geometry.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/DEFLT/IDEFLT

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      COMMON/SETPIK/NS,NSNO(MNRS),ISETON(MNRS),IMET,IFAFLG(MNRS,MNFA)
      COMMON/ZONPIK/NZ,NZNO(MCOM)

      CHARACTER ITEM(10)*25,outs*124
      integer NITMS,INO ! max items and current menu item

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MOSA'  ! set for subroutine

C Check that the result library contains the results from a 
C uncertainty analysis.
      if (NS.eq.0) then
        write (outs,'(a,a)')'The result library does not contain data',
     &        ' from a uncertainty analysis.'
        call edisp(iuout,outs)
        return
      endif

C Setup for menu.
 1    ITEM(1) ='2 Result sets            '
      ITEM(2) ='3 Display period         '
      ITEM(3) ='4 Select zone            '
      ITEM(4) ='  ---------------------- '
      ITEM(5) ='a Intra set statistics   '
      ITEM(6) ='b Inter set statistics   '
      ITEM(7) ='c Parameter uncertainties'
      ITEM(8) ='  ---------------------- '
      ITEM(9) ='? Help                   '
      ITEM(10)='- Exit                   '

 2    NITMS=10
      if(MMOD.eq.8)then
        INO=-1
      else
        INO=-2
      endif
C      INO=-2

C Instanciate help strings for this menu.
      helptopic='res_param_uncertainty'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('Uncertainty analysis',ITEM,NITMS,INO)

      IF(INO.EQ.1)then
        CALL MORESS
        IDEFLT=0
      elseif(INO.EQ.2)then
        IDEFLT=0  ! toggle so user can confirm period
        CALL MOOPER
      elseif(INO.EQ.3)then
        NZ=1
        helptopic='res_param_zone_list'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EPICKS(NZ,NZNO,' ','Which zone to include:',
     &    12,NCOMP,zname,' zone list',IER,1)
      elseif(INO.EQ.5)then
        CALL SAINTRA
      elseif(INO.EQ.6) then
        CALL SAINTER
      elseif(INO.EQ.7) then
        CALL SAPARSEN
      elseif(INO.EQ.(NITMS-1)) then
        helptopic='res_param_uncertainty'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Uncertainty analysis',9,'-',0,0,IER)
      elseif(INO.EQ.NITMS) then
        RETURN
      else
        goto 2
      endif
      goto 1
      END

C ******************** SAINTRA ********************
C SAINTRA calculates summary statistics within result sets.

      SUBROUTINE SAINTRA
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      COMMON/SETPIK/NS,NSNO(MNRS),ISETON(MNRS),IMET,IFAFLG(MNRS,MNFA)
      COMMON/SETNAM/RSNAME(MNRS)

      COMMON/IGETFLG/IOCUPF,ialstused,IROC
      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      common/getmenu/menutype,igetind(65),igetflux(65)

      character SLABEL*32,GLABEL*20,TABLABEL*36
      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS),TABLABEL(MZS)
      integer LNSLABEL,LNGLABEL,LNTABLABEL  ! lengths for label strings
      COMMON/LNGETLABEL/LNSLABEL(MZS),LNGLABEL(MZS),LNTABLABEL(MZS)
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

C ilflag = 0 tabular labels on multi-lines, ilflag = 1 on one line
C          ilflag = 2 do not include # header lines in file.
      COMMON/GRTOOL/IHFLAG,IDHFLG,ILFLAG
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      common/exporttg/xfile,tg,delim
      character xfile*144,tg*1,delim*1

      dimension YMAX(MZS), YMIN(MZS), YTOT(MZS), YTOTSD(MZS)
      dimension YAVE(MZS), YSTD(MZS), NY(MZS)
      character outs*124,louts*248,loutsd*248,louts1k*1000,louts1kd*1000
      character RSNAME*40
      character HEAD1*1000, HEAD2*1000,dg*1,dq*1
      character LTIME*5,LJTIME*8

      logical display, TOTALS

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='SAINTRA'  ! set for subroutine

C Call the menu of choices (this also sets some default options).
C The logical variable totals controls the displaying of cumulative 
C data for all the zones chosen.  
 1    display=.FALSE.
      TOTALS=.FALSE.
      MENUTYPE=6
      call GOMSETUP
      call GOMENU
      if (MENUTYPE.eq.-1) return
      dq = char(34)
      
C Set output unit number (assume default initially).
      ITRU=ICOUT
      if (IXOPEN.eq.1) ITRU=IXUNIT

C Ask for set selection.
      call WHSET
      if (NS.eq.0) goto 1

C Ask if a summary or tabular listing is required.
 2    IFMT=0
      helptopic='res_summary_stats'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKAB(' ','Output format:',
     &    'Summary table','Tabular list',IFMT,nbhelp)
      if(IFMT.eq.0) goto 2

C Copy current sub-set of sets into IGETNO array.
      do 201 ISET=1,NS
        if (ISET.gt.1) then
          NGET=NGET+1
          IGETNO(NGET,1)=IGETNO((NGET-1),1)
          IGETNO(NGET,2)=IGETNO((NGET-1),2)
          IGETNO(NGET,3)=IGETNO((NGET-1),3)
          IGETNO(NGET,4)=IGETNO((NGET-1),4)
          IGETNO(NGET,5)=NSNO(ISET)
          SLABEL(NGET)=SLABEL(NGET-1)
          LNSLABEL(NGET)=LNSLABEL(NGET-1)
        else
          IGETNO(NGET,5)=ISET
        endif
 201  continue

C Setup parameters and call GOGET for each output day to get required data.
C GOGET recovers the data in VAL2 (and averages output if required.)
C Variables begining X are for all sets selected whereas variables 
C starting Y are set based.
      do 5 I=1,MZS
        YMAX(I)=-1.E+10; YMIN(I)=1.E+10; YTOT(I)=0.0; YTOTSD(I)=0.0
        YAVE(I)=0.0; YSTD(I)=0.0; NY(I)=0
 5    continue
      XMAX=-1.E+10; XMIN=1.E+10; XTOT=0.0; XTOTSD=0.0
      XAVE=0.0; XSTD=0.0; NX=0
      call usrmsg('Scanning data for range of values...',' ','-')

C Write header information (if ILFLAG is 0 or 1).
      if(ILFLAG.le.1)then
        call edisp(itru,'  ')
        write (outs,'(a)') SLABEL(1)(1:LNSLABEL(1))
        call edisp(itru,outs)
        write (outs,'(a,a)') 'For zone: ',zname(IGETNO(1,2))
        call edisp(itru,outs)
        call edisp(itru,'  ')
      endif
      
C TSTART and TFINISH - start and finish times in hours from 0000 on the 
C first day of output.
      TSTART=FLOAT(IOH1)
      TFINSH=FLOAT(((IODF)*24+IOH2)-(IODS)*24)

C NDTS - the number of timesteps in a day.
      NDTS=24*NTS
 99   do 10 IDAY=IODS,IODF
        call GOGET(IDAY)

C Add values in VAL2 to there correct bins. Loop through selected sets.
        do 400 IX=1,NGET
          IZONE=IGETNO(IX,2)
          if (IGETNO(IX,5).gt.1) TOTALS=.true.
          DO 421 J = 1,NDTS,NOUT

C Compute current time.
C IHRD - number of days since start of plotting period in hours.
C TIME - time in hours since start of first day plotted.
C ATIME - actual time in the day for timestep J.
            IHRD=(IDAY-IODS)*24
            call DATIME(J,ATIME)
            TIME=float(IHRD)+ATIME

C Within requested output period.
            IF(TIME.LT.(TSTART-1.0).or.TIME.GT.TFINSH)goto 421

C If there is occupancy filter and occupancy then include in check.
C Assume fully occupied.
            ih=int(ATIME+1.)
            ioc=1
            if(iocupf.eq.1) call getocup(IZONE,IDAY,J,ioc,ier)
            if(ioc.ne.0) then
              if (display) then
                XTOTSD=XTOTSD+((XAVE-VAL2(IX,J))**2)
                YTOTSD(IX)=YTOTSD(IX)+((YAVE(IX)-VAL2(IX,J))**2)
              else
                IF (VAL2(IX,J).GT.XMAX) XMAX=VAL2(IX,J)
                IF (VAL2(IX,J).LT.XMIN) XMIN=VAL2(IX,J)
                XTOT=XTOT+VAL2(IX,J)
                NX=NX+1
                IF (VAL2(IX,J).GT.YMAX(IX)) YMAX(IX)=VAL2(IX,J)
                IF (VAL2(IX,J).LT.YMIN(IX)) YMIN(IX)=VAL2(IX,J)
                YTOT(IX)=YTOT(IX)+VAL2(IX,J)
                NY(IX)=NY(IX)+1
              endif
            endif
 421      CONTINUE
 400    CONTINUE

C If format is tabular listing then dump out a days data.
        if (IFMT.eq.2) then
          if (NGET.ge.100) then
            NC=99
          else
            NC=NGET
          endif
          call usrmsg('Scanning data for range of values..done.',
     &      ' ','-')

C If printing to screen then split column headings over two rows 
C don't do this if printing to file.
          irows=2
          if (ixopen.eq.1) irows=1
          is=1
          K=6   ! initial value for 248 buffer
          K4=6  ! initial value for 800 buffer
          if ((IHFLAG.eq.1.or.IHFLAG.eq.2).and.ixopen.eq.1) then
            if (delim.eq.'X') then
              louts  ='*time '
              louts1k='*time '
              K=7; K4=7   ! initial value for buffers
            else
              if(ILFLAG.le.1)then
                louts  ='#Time '
                louts1k='#Time '
                K=7; K4=7   ! initial value for buffers
              else
                louts  ='Time '
                louts1k='Time '
                K=6; K4=6   ! case of ILFLAG=2
              endif
            endif
          elseif ((IHFLAG.eq.1.or.IHFLAG.eq.2).and.ixopen.eq.0)then
            if (delim.eq.'X') then
              louts  ='*time '
              louts1k='*time '
              K=7; K4=7   ! initial value for buffers
            else
              louts  ='Time '
              louts1k='Time '
              K=6; K4=6   ! initial value for buffers
            endif
          else
            louts  ='Time '
            louts1k='Time '
            K=6; K4=6   ! initial value for buffers
          endif

C Logic for single or multiple header lines. For single
C header line write 16 char column title in double quotes
C and make them comma separated.
          HEAD1='  '; HEAD2='  '
          if(irows.eq.1)then
            do IH=1,NC
              IADS=((IH-1)*19)+1
              IADF=IADS+18
              write (HEAD1(IADS:IADF),'(4a)')
     &          dq,RSNAME(IGETNO(IH,5))(1:16),dq,','
              lnhead=lnblnk(HEAD1)
            enddo  ! of IH
            KE=K+IADF; KE4=K4+IADF
            if(KE.le.248)then
              write (louts(K:KE),'(a)') HEAD1(1:lnhead)
            endif
            if(KE4.le.1000)then
              write (louts1k(K4:KE4),'(a)') HEAD1(1:lnhead)
            endif

C Print titles on first day and only on subsequent ones if day
C demarcations are omitted.
C Debug.
            write(6,*) louts(1:lnblnk(louts))
            write(6,*) ' '
            write(6,*) louts1k(1:lnblnk(louts1k))
            write(6,*) ' '

            if (IDAY.eq.IODS)then
              if(ixopen.eq.1)then
                lnb=lnblnk(louts1k)   ! write directly to file
                write(itru,'(A)',iostat=ios,err=1)louts1k(1:lnb)
              else
                if(NC.le.12)then
                  call edisp(itru,louts)
                elseif(NC.gt.12.and.NC.le.26)then
                  call edisp248(itru,louts,120)
                elseif(NC.gt.26.and.NC.le.100)then
                  call edisp248(itru,louts1k,140)
                endif
              endif
            else
              if (IDHFLG.eq.1.and.ixopen.eq.1)then
                lnb=lnblnk(louts1k)   ! write directly to file
                write(itru,'(A)',iostat=ios,err=1)louts1k(1:lnb)
              elseif (IDHFLG.eq.1.and.ixopen.eq.0)then
                if(NC.le.12)then
                  call edisp(itru,louts)
                elseif(NC.gt.12.and.NC.le.26)then
                  call edisp248(itru,louts,120)
                elseif(NC.gt.26.and.NC.le.100)then
                  call edisp248(itru,louts1k,140)
                endif
              endif
            endif
          else

C Show two heading lines with items separated with |
            do IH=1,NC
              IADS=((IH-1)*9)+1
              IADF=IADS+9
              write (HEAD1(IADS:IADF),'(2a)')
     &          RSNAME(IGETNO(IH,5))(1:8),'|'
              lnhead=lnblnk(HEAD1)
              write (HEAD2(IADS:IADF),'(2a)')
     &          RSNAME(IGETNO(IH,5))(9:16),'|'
              lnhead2=lnblnk(HEAD2)
            enddo  ! of IH
            KE=K+IADF; KE4=K4+IADF
            if(KE.le.248)then
              write (louts(K:KE),'(a)') HEAD1(1:lnhead)
            endif
            if(KE4.le.1000)then
              write (louts1k(K4:KE4),'(a)') HEAD1(1:lnhead)
            endif
            if (IDAY.eq.IODS)then
              if(ixopen.eq.1)then
                lnb=lnblnk(louts1k)   ! write directly to file
                write(itru,'(A)',iostat=ios,err=1)louts1k(1:lnb)
              else
                if(NC.le.12)then
                  call edisp(itru,louts)
                elseif(NC.gt.12.and.NC.le.26)then
                  call edisp248(itru,louts,120)
                elseif(NC.gt.26.and.NC.le.100)then
                  call edisp248(itru,louts1k,140)
                endif
              endif
            else
              if (IDHFLG.eq.1.and.ixopen.eq.1)then
                lnb=lnblnk(louts1k)   ! write directly to file
                write(itru,'(A)',iostat=ios,err=1)louts1k(1:lnb)
              elseif (IDHFLG.eq.1.and.ixopen.eq.0)then
                if(NC.le.12)then
                  call edisp(itru,louts)
                elseif(NC.gt.12.and.NC.le.26)then
                  call edisp248(itru,louts,120)
                elseif(NC.gt.26.and.NC.le.100)then
                  call edisp248(itru,louts1k,140)
                endif
              endif
            endif
C            write(6,*) louts(1:lnblnk(louts))
C            write(6,*) ' '
C            write(6,*) louts1k(1:lnblnk(louts1k))
C            write(6,*) ' '

C Clear the 2nd heading line start.
            louts  ='     '
            louts1k='     '

            if(KE.le.248)then
              write (louts(K4:KE4),'(a)') HEAD2(1:lnhead2)
            endif
            if(KE4.le.1000)then
              write (louts1k(K4:KE4),'(a)') HEAD2(1:lnhead2)
            endif
            if (IDAY.eq.IODS)then
              if(ixopen.eq.1)then
                lnb=lnblnk(louts1k)   ! write directly to file
                write(itru,'(A)',iostat=ios,err=1)louts1k(1:lnb)
              else
                if(NC.le.12)then
                  call edisp(itru,louts)
                elseif(NC.gt.12.and.NC.le.26)then
                  call edisp248(itru,louts,120)
                elseif(NC.gt.26.and.NC.le.100)then
                  call edisp248(itru,louts1k,140)
                endif
              endif
            else
              if (IDHFLG.eq.1.and.ixopen.eq.1)then
                lnb=lnblnk(louts1k)   ! write directly to file
                write(itru,'(A)',iostat=ios,err=1)louts1k(1:lnb)
              elseif (IDHFLG.eq.1.and.ixopen.eq.0)then
                if(NC.le.12)then
                  call edisp(itru,louts)
                elseif(NC.gt.12.and.NC.le.26)then
                  call edisp248(itru,louts,120)
                elseif(NC.gt.26.and.NC.le.100)then
                  call edisp248(itru,louts1k,140)
                endif
              endif
            endif
          endif
  

C For each timestep make up the string to show and/or export.
          do 521 J = 1,NDTS,NOUT
            IHRD=(IDAY-IODS)*24
            call DATIME(J,ATIME)
            TIME=float(IHRD)+ATIME

C Display data. Create both data strings for this timestep and
C include users time preferences.
            louts='   '
            louts1k='   '
            if (IHFLAG.eq.0) then 
              call STIME(J,LTIME)
              write (louts,'(a)') LTIME
              write (louts1k,'(a)') LTIME
              K=6
            elseif (IHFLAG.eq.1) then
              RDOTY=REAL(IDAY)+(ATIME/24.)
              write (louts,'(f11.6)') RDOTY
              write (louts1k,'(f11.6)') RDOTY
              K=11
            elseif (IHFLAG.eq.2) then

C Write in format that spreadsheets understand as time.
              call edayr(IDAY,ID,IM)
              call SJTIME(J,LJTIME)
              write (louts,'(i4,a,i2.2,a,i2.2,2a)') 
     &          IYEAR,'-',IM,'-',ID,' ',LJTIME
              write (louts1k,'(i4,a,i2.2,a,i2.2,2a)') 
     &          IYEAR,'-',IM,'-',ID,' ',LJTIME
              K=20
            endif

C For each column append the values (as is done in table.F).

C First write data to the 248 char string buffer.
            klast=k
            do 410 IG=1,NC
              KE=K+9
              if(KE.le.248)then
                write (louts(K:KE),'(f9.4)') VAL2(IG,J)
                K=K+9
              endif
 410        continue

C If writing to file then write data to the 1k char string buffer.
            if(ixopen.eq.1)then
              k=klast   ! re-establish k position value
              do 411 IG=1,NC
                KE=K+9
                if(KE.le.1000)then
                  write (louts1k(K:KE),'(f9.4)') VAL2(IG,J)
                  K=K+9
                endif
 411          continue
            endif

            if(ixopen.eq.1)then
              lnb=lnblnk(louts1k)   ! write directly to file

              if(delim.eq.'-')then
                write(itru,'(A)',iostat=ios,err=1)louts1k(1:lnb)
              else

C If delimiter set to alternative then process text before writing.
C If using X delimeter (tagged data) then set the delimeter to a comma.
C If IHFLAG is 2 then the initial 19 or 21 characters will be a time
C stamp that needs the 10th character to be a literal space.
                dg=delim
                if (delim.eq.'X') dg='C'
                call SDELIM(louts1k,louts1kd,dg,IW)
                if(IHFLAG.eq.2) then
                  write(louts1kd(10:10),'(1a)') ' '
                endif
                lnb=lnblnk(louts1kd)   ! write directly to file
                write(itru,'(A)',iostat=ios,err=1) louts1kd(1:lnb)
              endif
            else

C Writing to screen so use shorter file buffer.
              lnb=MIN0(lnblnk(louts),124)
              if(lnb.lt.124)then
                call eddisp(itru,louts)
              else
                call eddisp248(itru,louts,124)
              endif
            endif

C            if(NC.le.12)then
C             write (outs,'(f6.2,12(1x,f8.4))')TIME,(VAL2(II,J),II=1,NC)
C             call edisp(itru,outs)
C            elseif(NC.gt.12.and.NC.le.26)then
C             write (louts,'(f6.2,26(1x,f8.4))')TIME,(VAL2(II,J),II=1,NC)
C             call edisp248(itru,louts,120)
C            elseif(NC.gt.26.and.NC.le.100)then
C             write (louts1k,'(f6.2,100(1x,f8.4))')TIME,
C     &         (VAL2(II,J),II=1,NC)
C             call edisp248(itru,louts1k,140)
C            endif
 521      continue
          call edisp(itru,'  ')
        endif
 10   continue

C Only proceed if IFMT=1
      if (IFMT.ne.1) goto 1

C Calculate averages and rescan the data to calculate std deviations.
      if (.not.display) then
        do 20 I=1,NGET
          if (NY(I).eq.0) then
            YAVE(I)=0.
          else
            YAVE(I)=YTOT(I)/float(NY(I))
          endif
 20     continue
        if (NX.eq.0) then
          XAVE=0.
        else
          XAVE=XTOT/float(NX)
        endif
        display=.TRUE.
        goto 99
      endif
      call usrmsg('Scanning data for range of values...done.',' ','-')

C Now display the calculated data.

C If climate data then do not print zone headings.
      if (IGETNO(1,1).eq.2.or.IGETNO(1,1).eq.19.or.
     &    IGETNO(1,1).eq.20.or.IGETNO(1,1).eq.21.or.
     &    IGETNO(1,1).eq.22.or.IGETNO(1,1).eq.26) then
        write (outs,'(a,a)')'                     Maximum   Minimum ',
     &                       '     Mean     Standard'
        call edisp(itru,outs)
        if ((NX-1).eq.0) then
          XSTD=0.
        else
          XSTD=sqrt(XTOTSD/float(NX-1))
        endif
        if (NX.eq.0) then
          write (outs,'(a)') 'No data: probably due to filtering.'
        else
          write (outs,667) XMAX, XMIN, XAVE, XSTD
        endif
 667    format (18x,3f10.2,f10.3)
        call edisp(itru,outs)
      else
        XTAVE=0.
        write (outs,'(a,a)')'   Set               Maximum   Minimum ',
     &                            '     Mean     Standard     Integral'
        call edisp(itru,outs)
        write (outs,'(a,40x,a)')'id name   ','   deviation   (unit*hrs)'
        call edisp(itru,outs)
        do 30 I=1,NGET
          if ((NY(I)-1).eq.0) then
            YSTD(I)=0.
          else
            YSTD(I)=sqrt(YTOTSD(I)/float(NY(I)-1))
          endif
          if (NY(I).eq.0) then
            write (outs,'(i2,1x,a15,a)') NSNO(I), RSNAME(NSNO(I)), 
     &                           'No data: probably due to filtering.'
          else
            write (outs,'(i2,1x,a15,4f10.3,3x,f10.3)') NSNO(I),
     &         RSNAME(NSNO(I)),YMAX(I),YMIN(I),YAVE(I),YSTD(I),
     &         YTOT(I)/real(NOUT)
          endif
          call edisp(itru,outs)
          XTAVE=XTAVE+YTOT(I)
 30     continue
        call edisp(itru,'  ')
        XTAVE=XTAVE/float(NS)
      endif
      if (TOTALS) then
        if ((NX-1).eq.0) then
          XSTD=0.
        else
          XSTD=sqrt(XTOTSD/float(NX-1))
        endif
        if ((NS-1).eq.0) then
          XTSTD=0.
        else
          XTSTD=0.
          do 567 Iset=1,NGET
            XTSTD=XTSTD+((XTAVE-YTOT(Iset))**2)
 567      continue
          XTSTD=sqrt(XTSTD/float(NGET-1))
        endif
        if (NX.eq.0) then
          write (outs,'(a)') 'No data: probably due to filtering.'
        else
          write (outs,666) 'All',XMAX,XMIN,XAVE,XSTD,XTAVE,XTSTD
        endif
 666    format (3x,a3,12x,4f10.3,3x,f10.3,', std dev',f7.4)
        call edisp(itru,outs)
      endif
      goto 1

      END

C ******************** SAINTER ********************
C SAINTER calculates summary statistics within result sets.

      SUBROUTINE SAINTER
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/GRAF1/YMAX(5),YMIN(5),YAXSET(5),ADDLIN,horaxisdiv
      COMMON/GRAF2/YSC(5),Yadd(5),TSC,Xadd,IGX(5),ILR(5),DT

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

      COMMON/SETPIK/NS,NSNO(MNRS),ISETON(MNRS),IMET,IFAFLG(MNRS,MNFA)
      COMMON/IGETFLG/IOCUPF,ialstused,IROC
      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      common/getmenu/menutype,igetind(65),igetflux(65)

      character SLABEL*32,GLABEL*20,TABLABEL*36
      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS),TABLABEL(MZS)
      integer LNSLABEL,LNGLABEL,LNTABLABEL  ! lengths for label strings
      COMMON/LNGETLABEL/LNSLABEL(MZS),LNGLABEL(MZS),LNTABLABEL(MZS)
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS

      dimension VAL(4,MTS), NVAL(MTS),VALHIGH(MTS), VALLOW(MTS)
      logical graphit,colok
      integer YAXSET,ADDLIN,horaxisdiv
      integer iglib   ! if iglib is 2 then using GTK library otherwise X11
#ifdef OSI
      integer IGX  ! see common graf2
      integer igwid,igheight  ! for use with axiscale
      integer iupdown,isym    ! passed to etplot
      integer iicol
      integer iigl,iigr,iigt,iigb,iigw,iigwh
#else
      integer*8 IGX  ! see common graf2
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iupdown,isym    ! passed to etplot
      integer*8 iicol
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
#endif

      character outs*124
      character prompt*124

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='SAINTER'  ! set for subroutine

C Define prompt
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      prompt='  '

C Check if can draw in colour.
      colok=.false.
      if(nzonec.ge.NCOMP)colok=.true.

C Cast values for linescale.
      iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
      
C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        if(NGET.ge.1)then
          write(prompt,'(a,a)') SLABEL(1)(1:LNSLABEL(1)),' >> file.'
        else
          write(prompt,'(a)')' Output being directed to file. ' 
        endif
      elseif(ixopen.eq.0)then
        if(MMOD.eq.8)call startbuffer
      endif

C Call the menu of choices (this also sets some default options).
C The logical variable totals controls the displaying of cumulative 
C data for all the zones chosen.  
 1    MENUTYPE=6
      call GOMSETUP
      call GOMENU
      if (MENUTYPE.eq.-1) return
      graphit=.false.

C Ask for set selection.
      call WHSET

C Ask if a summary or tabular listing is required.
 2    IFMT=0
      helptopic='res_summary_stats_vs'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKABC('Output format:',' ','Summary table',
     &    'Tabular list','Time series plot',IFMT,nbhelp)
      if(IFMT.eq.0) goto 2
      if (NS.eq.0) goto 1

C Setup parameters and call GOGET for each output day to get required data.
C GOGET recovers the data in VAL2 (and averages output if required.)
      VMAX=1.E-6
      VMIN=1.E+6

C Print header information.
      call usrmsg('Scanning data for range of values...',' ','-')
      call edisp(iuout,'  ')
      write (outs,'(a,a)') 'For zone: ',ZNAME(IGETNO(1,2))
      call edisp(ITRU,outs)
      if (NS.lt.38) then
        write (outs,22) 'Sets: ',(NSNO(ISX),ISX=1,NS)
        call edisp(ITRU,outs)
      else
        write (outs,22) 'Sets: ',(NSNO(ISX),ISX=1,38)
        call edisp(ITRU,outs)
        if (NS.lt.77) then
          write (outs,22) 'Sets: ',(NSNO(ISX),ISX=39,NS)
          call edisp(ITRU,outs)
        else
          write (outs,22) 'Sets: ',(NSNO(ISX),ISX=39,77)
          call edisp(ITRU,outs)
          if (NS.lt.117) then
            write (outs,22) 'Sets: ',(NSNO(ISX),ISX=78,NS)
            call edisp(ITRU,outs)
          else
            write (outs,22) 'Sets: ',(NSNO(ISX),ISX=78,116)
            call edisp(ITRU,outs)
          endif
        endif
      endif
 22   format(a,39(i2,','))
      call edisp(ITRU,'  ')

C TSTART and TFINISH - start and finish times in hours from 0000 on the 
C first day of output.
      TSTART=FLOAT(IOH1)
      TFINSH=FLOAT(((IODF)*24+IOH2)-(IODS)*24)

C Debug.
C      write(6,*) TSTART,TFINSH

C Copy current sub-set of sets into IGETNO array.
      NGET=0
      do 20 ISET=1,NS
        NGET=NGET+1
        if (NGET.gt.1) then
          IGETNO(NGET,1)=IGETNO((NGET-1),1)
          IGETNO(NGET,2)=IGETNO((NGET-1),2)
          IGETNO(NGET,3)=IGETNO((NGET-1),3)
          IGETNO(NGET,4)=IGETNO((NGET-1),4)
          IGETNO(NGET,5)=NSNO(ISET)
          SLABEL(NGET)=SLABEL(NGET-1)
          LNSLABEL(NGET)=LNSLABEL(NGET-1)
        else
          IGETNO(NGET,5)=NSNO(ISET)
        endif
 20   continue
      write(6,'(a,a)') SLABEL(1)(1:LNSLABEL(1)),' is focus'
      write(6,'(a,a)') GLABEL(1)(1:LNGLABEL(1)),' is focus'

C NDTS - the number of timesteps in a day.
      NDTS=24*NTS
 4    do 10 IDAY=IODS,IODF
      ISCAN=0
      do 5 J=1,MTS
        NVAL(J)=0
        VAL(1,J)=0.0; VAL(2,J)=0.0; VAL(3,J)=0.0
        VAL(4,J)=0.0; VALHIGH(J)=0.0; VALLOW(J)=0.0
 5    continue

      call GOGET(IDAY)
 999  ISCAN=ISCAN+1

C Get time series of max, min, mean and delta for each timestep.
C store in      VAL(1,*)   (2,*)   (3,*)   (4,*).
C For DSA:      Max        Min     Base    Max-Min
C For MCSA      Mean+s     Mean-s  Mean    s range
      do 400 IX=1,NGET
        IZONE=IGETNO(IX,2)
        ISET=IGETNO(IX,5)
        DO 421 J = 1,NDTS,NOUT

C Compute current time.
C IHRD - number of days since start of plotting period in hours.
C TIME - time in hours since start of first day plotted.
          IHRD=(IDAY-IODS)*24
          call DATIME(J,ATIME)
          TIME=float(IHRD)+ATIME

C Within requested output period.
          IF(TIME.LT.(TSTART-1.0).or.TIME.GT.TFINSH)goto 421

C If there is occupancy filter and occupancy then include in check.
C Assume fully occupied.
          ih=int(ATIME+1.)
          ioc=1
          if(iocupf.eq.1) call getocup(IZONE,IDAY,J,ioc,ier)
          if(ioc.ne.0) then
            if (ISCAN.eq.1) then
              if (IMET.eq.1) then
              
C Differential analysis.
                if (ISET.eq.1) then 
                  VAL(3,J)=VAL2(IX,J)
                elseif (MOD(ISET,2).eq.0) then
                  VALHIGH(J)=VAL2(IX,J)
                else
                  VALLOW(J)=VAL2(IX,J)
                  diff1=VALHIGH(J)-VAL(3,J)
                  diff2=VAL(3,J)-VALLOW(J)
                  if (diff1.gt.0.0001.and.diff2.gt.0.0001) then
                    VAL(1,J)=VAL(1,J)+diff1**2
                    VAL(2,J)=VAL(2,J)+diff2**2
                  elseif (diff1.lt.0.0001.and.diff2.lt.0.0001) then
                    VAL(1,J)=VAL(1,J)+diff2**2
                    VAL(2,J)=VAL(2,J)+diff1**2
                  elseif (diff1.gt.0.0001.and.diff2.lt.0.0001) then
                    temp1=diff1**2
                    temp2=diff2**2
                    if(temp1.gt.temp2) then
                      VAL(1,J)=VAL(1,J)+temp1
                    else
                      VAL(1,J)=VAL(1,J)+temp2
                    endif
                  elseif (diff1.lt.0.0001.and.diff2.gt.0.0001) then
                    temp1=diff1**2
                    temp2=diff2**2
                    if(temp1.gt.temp2) then
                      VAL(2,J)=VAL(2,J)+temp1
                    else
                      VAL(2,J)=VAL(2,J)+temp2
                    endif
                  endif
                endif
                NVAL(J)=NVAL(J)+1
              else
                VAL(3,J)=VAL(3,J)+VAL2(IX,J)
                NVAL(J)=NVAL(J)+1
              endif
            elseif (ISCAN.eq.2) then
              if (IMET.eq.1) then
                VMAX=max(VMAX,(VAL(3,J)+sqrt(VAL(1,J))))
                VMIN=min(VMIN,(VAL(3,J)-sqrt(VAL(2,J))))
              else

C Monte carlo: mean and std deviation. Calc mean for only one set.
                if (IX.eq.1) then 
                  VAL(3,J)=VAL(3,J)/NVAL(J)
                endif
                VAL(4,J)=VAL(4,J)+(VAL2(IX,J)-VAL(3,J))**2
              endif
            elseif (ISCAN.eq.3) then
              if (IMET.ne.1) then
                if (IX.eq.1) then 
                  VMAX=max(VMAX,
     &                   (VAL(3,J)+ (sqrt(VAL(4,J)/float(NVAL(1)-1)))))
                  VMIN=min(VMIN,
     &                   (VAL(3,J)- (sqrt(VAL(4,J)/float(NVAL(1)-1)))))
                endif
              endif
            endif
          endif
 421    CONTINUE
 400  CONTINUE


C Calculate averages and range by rescaning the data.
      if (IMET.eq.1) then 
        if (ISCAN.lt.2) goto 999
      else
        if (ISCAN.lt.3) goto 999
      endif
      
C Display according to IFMT.
      if (IFMT.eq.2) then

C If climate data then do not print zone headings.
        if (IGETNO(1,1).eq.2.or.IGETNO(1,1).eq.19.or.
     &        IGETNO(1,1).eq.20.or.IGETNO(1,1).eq.21.or.
     &        IGETNO(1,1).eq.22.or.IGETNO(1,1).eq.26) then
          write (outs,'(a,a)')'  Hour      Maximum       Minimum ',
     &                         '       Mean          Standard'
          call edisp(ITRU,outs)
          write (outs,'(a,a)')'             value         value  ',
     &                         '       value         deviation'
          call edisp(ITRU,outs)
        else
          write (outs,'(a,a)')'  Hour      Maximum       Minimum ',
     &                         '       Mean          Standard'
          call edisp(ITRU,outs)
          write (outs,'(a,a)')'             value         value  ',
     &                         '       value         deviation'
          call edisp(ITRU,outs)
        endif
      endif
      if (IFMT.eq.1.or.IFMT.eq.2) then
        SDTOT=0.
        NSDTOT=0
        DO 4211 J = 1,NDTS,NOUT

C Compute current time.
C IHRD - number of days since start of plotting period in hours.
C TIME - time in hours since start of first day plotted.
          IHRD=(IDAY-IODS)*24
          call DATIME(J,ATIME)
          TIME=float(IHRD)+ATIME

C Within requested output period.
          IF(TIME.LT.(TSTART-1.0).or.TIME.GT.TFINSH)goto 4211

C If there is occupancy filter and occupancy then include in check.
C Assume fully occupied.
          ih=int(ATIME+1.)
          ioc=1
          if(iocupf.eq.1) call getocup(IZONE,IDAY,J,ioc,ier)
          if(ioc.ne.0) then
            if (IMET.eq.1) then 
              VHI=VAL(3,J)+sqrt(VAL(1,J))
              VLO=VAL(3,J)-sqrt(VAL(2,J))
              SD=(sqrt(VAL(1,J))+sqrt(VAL(2,J)))/2.
              SDTOT=SDTOT+SD
              NSDTOT=NSDTOT+1
              if (IFMT.eq.2) then
                write(outs,'(f6.2,4(2x,f12.4))')TIME,VHI,VLO,VAL(3,J),SD
              endif
            else
              SD=sqrt(VAL(4,J)/float(NVAL(1)-1))
              SDTOT=SDTOT+SD
              NSDTOT=NSDTOT+1
              SDH=VAL(3,J)+SD
              SDL=VAL(3,J)-SD
              if (IFMT.eq.2) then
                write(outs,'(f6.2,4(2x,f12.4))')TIME,SDH,SDL,VAL(3,J),SD
              endif
            endif
          else
            write (outs,'(f6.2,a)') HR,'No data due to filtering.'
          endif
          call edisp(ITRU,outs)
 4211   CONTINUE
        call EDISP(ITRU,'  ')
      elseif (IFMT.eq.3.and.graphit) then

C Debug.
C        write(6,*) 'igx values ',igx
C        write(6,*) 'yaxset ',YAXSET
C        write(6,*) 'igl igr igw igwh ',igl,igr,igw,igwh
C        write(6,*) 'fonts ifs itfs  & imfs & iglib ',IFS,ITFS,IMFS,iglib

C Draw data on graph.
        HRold=(24.0/float(NDTS))+float(IDAY-IODS)*24.0
        DO 4212 J = 1,NDTS,NOUT
          ih=J/NTS
          if(ih.eq.0)ih = 1
          IF(IDAY.LE.IODS.AND.(FLOAT(J)/NTS).LT.IOH1)goto 4212
          IF(IDAY.eq.IODF.AND.(FLOAT(J)/NTS).gt.IOH2)goto 4212

C If there is occupancy filter and occupancy then include in check.
C Assume fully occupied.
          IHRD=(IDAY-IODS)*24
          HR=(float(J)/float(NDTS))*24.0+float(IHRD)
          ioc=1
          if(iocupf.eq.1) call getocup(IZONE,IDAY,J,ioc,ier)
          if(ioc.ne.0) then
            if (IMET.eq.1) then 
              VHI=VAL(3,J)+sqrt(VAL(1,J))
              VLO=VAL(3,J)-sqrt(VAL(2,J))
              iupdown=0; isym=0  ! move to position
              if (J.eq.IOH1.AND.IHRD.eq.0) then
                call etplot(HR,VAL(3,J),iupdown,isym)
              else
                call etplot(HRold,VMEANold,iupdown,isym)
              endif
              VMEANold=VAL(3,J)
C              iupdown=-303; isym=4  ! draw thick line
              iupdown=1; isym=0  ! draw line
              call etplot(HR,VAL(3,J),iupdown,isym)
              iupdown=0
              isym=0
              if (J.eq.IOH1.AND.IHRD.eq.0) then
                call etplot(HR,VHI,iupdown,isym)
              else
                call etplot(HRold,VHIold,iupdown,isym)
              endif
C              iupdown=1; isym=2  ! draw solid line
              iupdown=-206; isym=0  ! draw sparse dotted line
              call etplot(HR,VHI,iupdown,isym)
              VHIold=VHI
              iupdown=0; isym=0  ! move to position
              if (J.eq.IOH1.AND.IHRD.eq.0) then
                call etplot(HR,VLO,iupdown,isym)
              else
                call etplot(HRold,VLOold,iupdown,isym)
              endif
C              iupdown=1; isym=3 ! draw solid line
              iupdown=-206; isym=0 ! draw dotted line
              call etplot(HR,VLO,iupdown,isym)
              VLOold=VLO
              HRold=HR
              call forceflush()
            else
              SD=sqrt(VAL(4,J)/float(NVAL(1)-1))
              SDH=VAL(3,J)+SD
              SDL=VAL(3,J)-SD
              iupdown=0; isym=0  ! move to position
              if (J.eq.IOH1.AND.IHRD.eq.0) then
                call etplot(HR,VAL(3,J),iupdown,isym)
              else
                call etplot(HRold,VMEANold,iupdown,isym)
              endif
              VMEANold=VAL(3,J)
C              iupdown=-303; isym=0  ! draw thick line
              iupdown=1; isym=0  ! draw line
              call etplot(HR,VAL(3,J),iupdown,isym)
              iupdown=0; isym=0  ! move to position
              if (J.eq.IOH1.AND.IHRD.eq.0) then
                call etplot(HR,SDH,iupdown,isym)
              else
                call etplot(HRold,SDHold,iupdown,isym)
              endif
C              iupdown=1; isym=2  ! draw solid line
              iupdown=-206; isym=0  ! draw sparse dotted line
              call etplot(HR,SDH,iupdown,isym)
              SDHold=SDH
              iupdown=0; isym=0  ! move to position
              if (J.eq.IOH1.AND.IHRD.eq.0) then
                call etplot(HR,SDL,iupdown,isym)
              else
                call etplot(HRold,SDLold,iupdown,isym)
              endif
C              iupdown=1; isym=3  ! draw solid line
              iupdown=-206; isym=0  ! draw dotted line
              call etplot(HR,SDL,iupdown,isym)
              SDLold=SDL
              HRold=HR
              call forceflush()
            endif
          else
            write (outs,'(f6.2,a)') HR,'No data due to filtering.'
            call edisp(ITRU,outs)
          endif
 4212   CONTINUE
        call EDISP(ITRU,'  ')
      endif
 10   continue

C Calculate overall standard deviation and display depending on IFMT.
      call usrmsg('Scanning data for range of values... done.',
     &                                                        ' ','-')
      if (IFMT.eq.1.or.IFMT.eq.2) then
        write (outs,'(a,f8.3)') 'Average standard deviation: ',
     &                    SDTOT/real(NSDTOT)
        call edisp(ITRU,outs)
      elseif (IFMT.eq.3.and.(.not.graphit)) then
        call MOGHED
        YMAX(1)=VMAX
        YMIN(1)=VMIN
        YAXSET(1)=1  ! lets try left axis
        DHR=FLOAT(((IODF-1)*24+IOH2)-((IODS-1)*24+IOH1))
        TMIN=FLOAT(IOH1)-1.0/FLOAT(NTS)
        TMAX=FLOAT(IOH1)+DHR+1.0/FLOAT(NTS)
        if(colok)then
          iicol=0
          call winscl('-',iicol)  ! ensure axis drawn in black
        endif
        call dintervalf(TMIN,TMAX+1.,DT,NDEC,1)
        IY1=1
        igwid=igw
        igheight=igwh
        call axiscale(igwid,igheight,TMIN,TMAX+1.,YMIN(1),YMAX(1),TSC,
     &    YSC(1),sca,Xadd,Yadd(1))

C Adapt this label to match what user requested.
        call dintervalf(YMIN(1),YMAX(1),ddy1,ny,0)
        call vrtaxisdd(YMIN(1),YMAX(1),iigl,iigb,iigt,Yadd(1),YSC(1),0,
     &    ddy1,ny,0,GLABEL(1))
        call dintervalf(TMIN,TMAX+1.,ddy1,ny,1)
        call horaxisdd(TMIN,TMAX+1.,iigl,iigr,iigb,Xadd,TSC,1,
     &    ddy1,ny,'Time Hours')
        call linescale(iigl,Xadd,TSC,iigb,Yadd(1),YSC(1))
        graphit=.true.
        goto 4
      endif
      call EDISP(ITRU,'  ')
      goto 1

      END

C ******************** SAPARSEN ********************
C SAPARSEN calculates individual parameter uncertainties.

      SUBROUTINE SAPARSEN
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/SIMPKA/NSIM
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

      COMMON/SETPIK/NS,NSNO(MNRS),ISETON(MNRS),IMET,IFAFLG(MNRS,MNFA)
      COMMON/SETNAM/RSNAME(MNRS)

      COMMON/IGETFLG/IOCUPF,ialstused,IROC
      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      common/getmenu/menutype,igetind(65),igetflux(65)

      character SLABEL*32,GLABEL*20,TABLABEL*36
      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS),TABLABEL(MZS)
      integer LNSLABEL,LNGLABEL,LNTABLABEL  ! lengths for label strings
      COMMON/LNGETLABEL/LNSLABEL(MZS),LNGLABEL(MZS),LNTABLABEL(MZS)
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      
      dimension YTOT(MZS), YTOTAB(MZS)
      dimension IUNC(MNFA), IPT(MNRS,MNFA)
      
      character outs*124,RSNAME*40

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='SAPARSEN'  ! set for subroutine
      
C Call the menu of choices (this also sets some default options).
 1    MENUTYPE=6
      call GOMSETUP
      call GOMENU
      if (MENUTYPE.eq.-1) return

C Set output unit number (assume default initially).
      ITRU=ICOUT
      if (IXOPEN.eq.1) ITRU=IXUNIT

C Ask if data should be integrated or averaged.
      helptopic='res_sensitivity_aver'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call EASKAB('Should data be:',' ','integrated','averaged',
     &            INTAVE,nbhelp)

C Ask for set selection if DSA or MCA.
      if (IMET.eq.1.or.IMET.eq.3) then
        call WHSET
        NLOOP=1
      else
        NS=0
        do 3 I=1,NSIM
          ISETON(I)=1
          NS=NS+1
          NSNO(NS)=I
 3      continue
        ISETON(1)=0
        NFA=nint(log10(real(NSIM-1))/log10(real(2)))
        call EASKABCD('Effect:',' ','average','main',
     &    '2-way interactions','3-way interactions',IEFF,nbhelp)
        IEFF=IEFF-1
        NLOOP=FACTORIAL(NFA)/(FACTORIAL(IEFF)*FACTORIAL(NFA-IEFF))
        if (IEFF.gt.1) call INTERACT(NFA,IEFF,NLOOP,IPT)
      endif
      
C Setup parameters and call GOGET for each output day and for each 
C result set to get required data.
C GOGET recovers the data in VAL2 (and averages output if required.)
C Variables begining X are for all sets selected whereas variables 
C starting Y are set based.
      NY=0
      YBT=0.0
      do 5 I=1,MZS
        YTOT(I)=0.0
        YTOTAB(I)=0.0
 5    continue
      call usrmsg('Scanning data for range of values...',' ','-')

C TSTART and TFINISH - start and finish times in hours from 0000 on the 
C first day of output.
      TSTART=FLOAT(IOH1)
      TFINSH=FLOAT(((IODF)*24+IOH2)-(IODS)*24)

C NDTS - the number of timesteps in a day.
      NDTS=24*NTS
      
C Copy sets into IGETNO array.
      NGET=0
      do 20 ISET=1,NS
        NGET=NGET+1
        if (NGET.gt.1) then
          IGETNO(NGET,1)=IGETNO((NGET-1),1)
          IGETNO(NGET,2)=IGETNO((NGET-1),2)
          IGETNO(NGET,3)=IGETNO((NGET-1),3)
          IGETNO(NGET,4)=IGETNO((NGET-1),4)
          IGETNO(NGET,5)=NSNO(ISET)
          SLABEL(NGET)=SLABEL(NGET-1)
        else
          IGETNO(NGET,5)=NSNO(ISET)
        endif
 20   continue

C Loop through a day at a time.
      NY=0
      IZONE=IGETNO(1,2)
      do 10 IDAY=IODS,IODF
        call GOGET(IDAY)

C Add values in VAL2 to there correct bins. Loop through selected sets.
        do 400 IX=2,NGET
          ISET=IGETNO(IX,5)

C debug          write(6,*) 'IX, ',IX
C debug          write(6,*) 'IV, ',IV

          DO 421 J = 1,NDTS,NOUT

C Compute current time.
C IHRD - number of days since start of plotting period in hours.
C TIME - time in hours since start of first day plotted.
            IHRD=(IDAY-IODS)*24
            call DATIME(J,ATIME)
            TIME=float(IHRD)+ATIME

C Within requested output period.
            IF(TIME.LT.(TSTART-1.0).or.TIME.GT.TFINSH)goto 421

C If there is occupancy filter and occupancy then include in check.
C Assume fully occupied.
            ih=int(ATIME+1.)
            ioc=1
            if(iocupf.eq.1) call getocup(IZONE,IDAY,J,ioc,ier)
            if(ioc.ne.0) then
              if (IMET.eq.1.or.IMET.eq.3) then
                YTOT(IX)=YTOT(IX)+(VAL2(IX,J)-VAL2(1,J))
                YTOTAB(IX)=YTOTAB(IX)+ABS(VAL2(IX,J)-VAL2(1,J))
                if (IX.eq.2) NY=NY+1
              else
      
C Factorial analysis.  Need to determine if VAL2(IX,J) is active 
C for each YTOT(IL).      
                do 500 IL=1,NLOOP
                  if (IEFF.eq.0) then
                    ACT=1.
                  elseif (IEFF.eq.1) then
                    ACT=real(IFAFLG(IX,IL))
                  elseif (IEFF.eq.2) then
                    ACT=real(IFAFLG(IX,ipt(IL,1))*IFAFLG(IX,IPT(IL,2)))
                  elseif (IEFF.eq.3) then
                    ACT=real(IFAFLG(IX,IPT(IL,1))*IFAFLG(IX,IPT(IL,2))*
     &                  IFAFLG(IX,IPT(IL,3)))
                  endif
                  YTOT(IL)=YTOT(IL)+VAL2(IX,J)*ACT
 500            continue
                if (IX.eq.2) NY=NY+1
              endif
            endif
 421      CONTINUE
 400    CONTINUE
 10   continue

C Calculate averages.
      if (IMET.eq.1.or.IMET.eq.3) then
        do 200 IRS=2,NGET
          YTOT(IRS)=YTOT(IRS)/real(NY)
          YTOTAB(IRS)=YTOTAB(IRS)/real(NY)
 200    continue
      else
        do 205 IRS=1,NLOOP
          YTOT(IRS)=YTOT(IRS)/real(NGET-1)
          if (IEFF.gt.0) YTOT(IRS)=YTOT(IRS)*2.
 205    continue
      endif
 
C debug      write(6,*) 'NY, ',NY

      call usrmsg('Scanning data for range of values...done.',' ','-')

C Now display the calculated data.
      call edisp(ITRU,'  ')
      write (outs,'(a)') SLABEL(1)
      call edisp(ITRU,outs)
      write (outs,'(a,a)') 'For zone: ',zname(IGETNO(1,2))
      call edisp(ITRU,outs)
      call edisp(ITRU,'  ')

      if (IMET.eq.1.or.IMET.eq.3) then
        write (outs,'(a)')
     &  '   Set                         Parameter uncertainty'
        call edisp(ITRU,outs)
        write (outs,'(a)')
     &  'id name                         standard   absolute'
        call edisp(ITRU,outs)
        do 30 I=2,NGET
          write (outs,'(i2,1x,a40,2(f8.3,2x))') I,RSNAME(I),
     &      YTOT(I),YTOTAB(I)
          call edisp(ITRU,outs)
 30     continue
      else
      
C Factorial analysis output.
        if (IEFF.eq.0) then
          write (outs,'(a)')'   Average effect'
        elseif (IEFF.eq.1) then
          write (outs,'(a)')'   Main effects'
        elseif (IEFF.eq.2) then
          write (outs,'(a)')'   2-way interactions'
        elseif (IEFF.eq.3) then
          write (outs,'(a)')'   3-way interactions'
        endif
        call edisp(ITRU,outs)
        do 35 I=1,NLOOP
          write (outs,'(f8.3)') YTOT(I)
          call edisp(ITRU,outs)
 35     continue
      endif
      goto 1

      END

C ******************** SANOVA ********************
C SANOVA calculates summary statistics between result sets - most
C importantly an analysis of variance table.

      SUBROUTINE SANOVA
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      common/getmenu/menutype,igetind(65),igetflux(65)
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      DIMENSION TT(MZS), NO(MZS), TA(MZS)
      character outs*124

C Call the menu of choices (this also sets some default options).
C The logical variable totals controls the displaying of cumulative 
C data for all the zones chosen.  
 1    MENUTYPE=6
      call GOMSETUP
      call GOMENU
      if (MENUTYPE.eq.-1) return

C Setup parameters and call GOGET for each output day to get required data.
C GOGET recovers the data in VAL2 (and averages output if required.)
C Variables begining X are for all sets selected whereas variables 
C starting Y are set based.

      call usrmsg('Scanning data for range of values...',' ','-')
      NT=NGET

C Clear output arrays.
      GT=0.
      NTO=0
      do 5 I=1,MZS
        TT(I)=0.
        NO(I)=0
 5    continue

 99   do 10 IDAY=IODS,IODF
        call GOGET(IDAY)

C Generate an array of differences between sets, data stored in VAL3, max 
C 7 sets (=21 difference arrays).
        call DIFFOW(NT,ND,TT,GT,NO,NTO)
 10   continue
      write (outs,'(4x,10f7.4)') (TT(IX),IX=1,10)
      call edisp(iuout,outs)
      write (outs,'(4x,10i7)') (NO(IX),IX=1,10)
      call edisp(iuout,outs)
      write (outs,'(a,f8.4,a,i4)') 'Grand total ',GT,', Total obs ',NTO
      call edisp(iuout,outs)
      call usrmsg('Scanning data for range of values...done.',' ','-')

C Calculate averages (TA= treatment ave, GA= grand ave.)
      do 20 I=1,ND
        if (NO(I).ne.0) then
          TA(I)=TT(I)/NO(I)
        else
          TA(I)=0.
          write (outs,'(a,i2,a)')'Difference column ',I,' has no data.'
          call EDISP(iuout,outs)
        endif
 20   continue
      if (NTO.ne.0) then
        GA=GT/NTO
      else
        call EDISP(iuout,'Error: total number of observations = zero')
        goto 1
      endif

C Calculate the correction factor (FACC)
      FACC=(GA**2)/NTO

C Calculate between treatment sum of squares (BT)
      BT=0.
      do 30 I=1,ND
        BT=BT+((TT(I)**2)/NO(I))
 30   continue
      BT=BT-FACC

C Calculate the within treatment sum of squares (WT)
      WT=0.
      do 40 I=1,ND
        do 50 J=1,NTS,NOUT
          WT=WT+VAL3(I,J)**2
 50     continue
 40   continue
      WT=WT-FACC-BT

C Calculate the mean squares BTMS and WTMS
      BTMS=BT/(ND-1)
      WTMS=WT/(NTO-ND)

C Display.
      write (outs,'(a,f8.3)') 'Between differences mean square ',BTMS
      call EDISP(iuout,outs)
      write (outs,'(a,f8.3)') 'Within differences mean square ',WTMS
      call EDISP(iuout,outs)
      
      goto 1

      END

C ******************** DIFFOW ********************
C DIFFOW calculates differences between data lists for the construction 
C of a one way analysis of variance table for NT treatments. 
C  TT - treatment totals
C  GT - grand total
C  NO - number of observations
C NTO - total number of observations

      SUBROUTINE DIFFOW(NT,ND,TT,GT,NO,NTO)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      DIMENSION TT(MZS), NO(MZS)
      CHARACTER OUTS*124

C Calculate number of difference columns.
      ND=0
      do 10 I=2,NT
        ND=ND+I-1
 10   continue

C Carry out calculation of a whole days data.
      NDTS=24*NTS
      DO 421 ITS = 1,NDTS,NOUT

C Calculate differences.
        ID=0
        do 20 I=1,NT
          IX=I+1
          do 30 J=IX,NT
            ID=ID+1
            if (ID.gt.ND) call edisp (iuout,' Error 1 in DIFFOW')
            VAL3(ID,ITS)=VAL2(I,ITS)-VAL2(J,ITS)
            TT(ID)=TT(ID)+VAL3(ID,ITS)
            GT=GT+VAL3(ID,ITS)
            NO(ID)=NO(ID)+1
            NTO=NTO+1
 30       continue
 20     continue
 421  continue

      write (outs,'(a)') 'Difference table:'
      call EDISP (iuout,outs)
      do 40 I=1,NDTS,NOUT
        write (outs,'(i4,10f7.4)') I,(VAL3(IX,I),IX=1,10)
        call EDISP (iuout,outs)
 40   continue


      if (ID.ne.ND) call edisp (iuout,' Error 2 in DIFFOW')

      return
      END

C ******************** WHSET ********************
C WHSET displays currently chosen sets and lets user toggle selections.
      SUBROUTINE WHSET
#include "building.h"
#include "epara.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      COMMON/SIMPKA/NSIM

      COMMON/SETPIK/NS,NSNO(MNRS),ISETON(MNRS),IMET,IFAFLG(MNRS,MNFA)
      COMMON/SETNAM/RSNAME(MNRS)

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      CHARACTER VERT(35)*65, RSNAME*40, KEY*1
      LOGICAL SELECT
      integer MVERT,IVERT ! max items and current menu item

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='WHSET'  ! set for subroutine

C Display all sets in a menu
C Initialise surface menu variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
      SELECT=.FALSE.
      MCTL=6
      MHEAD=0
      if (IMET.eq.1) then 
        ILEN=(NSIM+1)/2
      else
        ILEN=NSIM
      endif
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      M=0
      if(MMOD.eq.8)then
        IVERT=-1
      else
        IVERT=-2
      endif

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      DO 10 Lx=1,ILEN
        IF(Lx.GE.IST.AND.(Lx.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(Lx,KEY,IER)
          if (Lx.eq.1.or.IMET.ne.1) then
            if (ISETON(Lx).eq.1) then
              WRITE(VERT(M),'(A1,1X,A40,A8)')KEY,RSNAME(Lx),': ACTIVE'
            else
              WRITE(VERT(M),'(A1,1X,A40,8X)')KEY,RSNAME(Lx)
            endif
          else
            LSET=2*(Lx-1)
            if (ISETON(LSET).eq.1) then
              WRITE(VERT(M),'(A1,1X,A40,A23)')KEY,RSNAME(LSET),
     &                  ' both changes: ACTIVE'
            else
              WRITE(VERT(M),'(A1,1X,A40,23X)')KEY,RSNAME(LSET)
            endif
          endif
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  --------------------------  '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT ('0 page part: ',I1,' of ',I1)
      ENDIF
      VERT(M+2)  ='* Activate all sets'
      VERT(M+3)  ='! Clear all selections '
      VERT(M+4)  ='  --------------------------'
      VERT(M+5)  ='? help              '
      VERT(M+6)  ='- exit              '

C Instanciate help strings for this menu.
      helptopic='res_set_activation'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Display the menu.
      CALL EMENU('Result set activation',VERT,MVERT,IVERT)
      IF(IVERT.EQ.MVERT)THEN

C If no alterations have been made before exit then return, else 
C copy new set selection into NSNO().
        IF(.NOT.SELECT) then
          call usrmsg(' ',' ','-')
          RETURN
        else
          NS=0
          do 100 I=1,NSIM
            if (ISETON(I).eq.1) then
              NS=NS+1
              NSNO(NS)=I
            endif
 100      continue
          RETURN
        endif
      ELSEIF(IVERT.EQ.(MVERT-1))THEN
        helptopic='res_set_activation'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Result set activation',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C Clear all selections.
        SELECT=.true.
        do 110 I=1,NSIM
          ISETON(I)=0
 110    continue  
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C Activate all sets.
        SELECT=.true.
        do 120 I=1,NSIM
          ISETON(I)=1
 120    continue
      ELSEIF(IVERT.EQ.(MVERT-5))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.gt.0.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Decode from the potential long list to the zone number via KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        SELECT=.TRUE.
        if (IFOC.eq.1) then 
          ISETON(1)=ISETON(1)+1
          if (ISETON(1).gt.1) ISETON(1)=0
        elseif (IMET.eq.1) then 
          ISETON((IFOC-1)*2)=ISETON((IFOC-1)*2)+1
          if (ISETON((IFOC-1)*2).gt.1) ISETON((IFOC-1)*2)=0
          ISETON((IFOC-1)*2+1)=ISETON((IFOC-1)*2+1)+1
          if (ISETON((IFOC-1)*2+1).gt.1) ISETON((IFOC-1)*2+1)=0
        else
          ISETON(IFOC)=ISETON(IFOC)+1
          if (ISETON(IFOC).gt.1) ISETON(IFOC)=0
        endif
        
      ELSE
C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-2
      goto 92

      END
      
C ****************** FACTORIAL ******************
C FACTORIAL - calculate n!
      real function FACTORIAL(I)
      
      FACTORIAL=1.
      if (I.le.1) return
      do 10 J=2,I
        FACTORIAL=FACTORIAL*real(J)
 10   continue
 
      return
      end

C ****************** INTERACT ******************
C INTERACT - calculate the all the interactions given an interaction 
C of NINTER parameters. For NPAR parameters.
C IPT(x,y) - pointer to parameters 'y' for interaction 'x'
      subroutine INTERACT(NPAR,NINTER,ICOMB,IPT)
#include "building.h"

      dimension IPT(MNRS,MNFA)

      if (NINTER.lt.2) then
        return
      endif
      
C Clear and set up initial pointer array.
      do 5 J=1,MNRS
        do 10 I=1,MNFA
          IPT(J,I)=0
 10      continue
 5    continue
      do 20 I=1,NINTER
        IPT(1,I)=I
 20   continue
 
C Step through pointers ICOMB times.
      K=1
      if (ICOMB.gt.1) then
        do 30 I=2,ICOMB
          do 45 J=1,NINTER
            IPT(K+1,J)=IPT(K,J)
 45       continue
          K=K+1
 40       if (IPT(K,NINTER).lt.NPAR) then
            IPT(K,NINTER)=IPT(K,NINTER)+1
          else
            do 35 J=NINTER-1,1,-1
              if ((IPT(K,J)+1).lt.IPT(K,J+1)) then
                IPT(K,J)=IPT(K,J)+1
                IPT(K,J+1)=IPT(K,J)+1
                do 55 Jx=1,NINTER
                 IPT(K+1,Jx)=IPT(K,Jx)
 55             continue
                K=K+1
                goto 40
              endif
 35         continue
          endif
 30     continue
      endif
 
      return
      end
