C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-.

C ESP-r is free software. You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C This subroutine reads data from the moisture results file and 
C writes the recovered data to array GOTDAT.

      subroutine vread
#include "building.h"
#include "mould.h"
#include "espriou.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/grphlim/xmin,xmax,ymin,ymax       
      COMMON/GOTDAT/NDP,DAT1(MTS),DAT2(MTS),DAT3(MTS)
      common/mstats/imstat,ifloat(mould),ifloatp(mould),imdur1(mould)
     &,imdur0(mould),i1max(mould),i0max(mould),imup(mould),
     &imdn(mould)

      DIMENSION IZNDX(24)
      CHARACTER FILENM*72,OUTSTR*124,OUTSTR2*124,txt*30
      character outs*124
      integer ncomp  ! this might clash with common c1 so revise

#ifdef OSI
      integer ipos,isize,iix,iiy     ! for use with etlabel
#else
      integer*8 ipos,isize,iix,iiy      ! for use with etlabel
#endif

      helpinsub='vfr'  ! set for subroutine
      helptopic='moisture_results_file'
      call gethelptext(helpinsub,helptopic,nbhelp)

      if(lnblnk(FILENM).le.1) FILENM=' '
      CALL EASKS(FILENM,'moisture result file name ?',
     &    ' ',72,'HUMIDITY','moisture file name',IER,nbhelp)
      OPEN(23,FILE=FILENM,STATUS='OLD')
      write(currentfile,'(a)') FILENM(1:lnblnk(FILENM))
      helptopic='option_to_plot_or_write'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call easkab('Do you want to '
     &,' ','plot data','write to file',IW,nbhelp)
      if(iw.eq.2) then
        CALL EASKS(FILENM,'output file name ?',
     &' ',72,'HUMID','output file name',IER,nbhelp)
        OPEN(24,FILE=FILENM,STATUS='NEW')
      endif

C Determine the number of active zones (ncomp is filled by stripc call).
C For each of the zones that have data encode the list izndx as follows:
C  if there are 5 zones in the model and the 3rd zone has data then
C  the first line of the msr file will look like:
C  0 0 1 0 0
C and NAZ will be 1 and izndx(3) = 1.
      NCOMP=2
      CALL STRIPC(23,OUTSTR,99,NCOMP,1,'indices',IER)
      K=0
      NAZ=0
      DO 7 ICHK=1,NCOMP
        CALL EGETWI(OUTSTR,K,INDX,0,0,'-','INDEX',IER)
        IF(INDX.EQ.1)then 
          NAZ=NAZ+1 
          IZNDX(ICHK)=NAZ
        else
          IZNDX(ICHK)=0
        endif
   7  CONTINUE

C Read the 2nd and 3rd lines of the file to see how many surfaces there are
C (one less than the number of 2nd lineitems). Then rewind the file and
C re-scan the first line.
      CALL STRIPC(23,OUTSTR,99,ITEMS,1,'2nd line of msr file',IER)
      CALL STRIPC(23,OUTSTR2,99,ITEMS2,1,'3rd line of msr file',IER)
      ns=ITEMS-1
      rewind(23,ERR=92)
      CALL STRIPC(23,OUTSTR,0,ND,1,'msr first line',IER)

      iz=0
      if(NAZ.eq.0)then
        call usrmsg('No zone data available!',' ','W')
        return
      elseif(NAZ.eq.1)then
        do 9 ichk=1,ncomp
          if(IZNDX(ICHK).ne.0)then
            izn=IZNDX(ICHK)
            write(outs,'(a,i2,a,i2,a)') 'Information for model zone ',
     &        ichk,' (',IZNDX(ICHK),' in list)'
            call edisp(iuout,outs)
          endif
   9    continue
        if(izn.ne.0)then
          iz=izn
        else
          return
        endif
      else
        do 13 ichk=1,ncomp
          if(IZNDX(ICHK).ne.0)then
            write(outs,'(a,i2,a,i2,a)') 'Information for model zone ',
     &        ichk,' (',IZNDX(ICHK),' in list)'
            call edisp(iuout,outs)
          endif
  13    continue
        helptopic='option_to_select_zone'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKI(IZ,'Which zone in the list (1st, 2nd etc.)? ',
     &    '(zero to return)',1,'W',NCOMP,'W',1,'zone index',IER,1)
        IF(IZ.GT.0) THEN
          IZN=IZ
        ELSE
          return
        ENDIF
      endif

  12  write(outs,'(a,i2,a)') 'Information found for ',ns,' surfaces.'
      call edisp(iuout,outs)
      is=0
      helptopic='option_to_select_surface'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKI(IS,'Which surface index to plot? ','(zero to return)',
     &  0,'W',32,'W',1,'surface index to assess',IER,nbhelp)
      if(is.eq.0)return
      IS=IS+2
      ITS=1
      if(IS.GT.ITEMS.OR.IS.GT.ITEMS2+1)then 
        call edisp(iuout,'un-acceptable surface number.')
        goto 12
      endif
      write(txt,'(a,i2,a,i2)') 'Zone (in list) ',iz,' Surface ',is-2
      ipos=0
      isize=2
      call etlabel(txt,xmax/2+xmin/2,ymax+4.,ipos,isize)

   10 CONTINUE

C Scan the moisture results file.
      DO 20 ICOMP=1,NAZ
        IF(ICOMP.EQ.IZ)THEN
          CALL STRIPC(23,OUTSTR,99,ITEMS,1,'relative humidity',IER)
          CALL STRIPC(23,OUTSTR2,99,ITEMS2,1,'temperature',IER)
          IF(IER.EQ.2)THEN
            CLOSE(23)
            if(iw.eq.2) CLOSE(24)
            return
          ELSEIF(IER.EQ.1)THEN
            CLOSE(23)
            if(iw.eq.2) CLOSE(24)
            return
          ELSE
            if(IS.GT.ITEMS.OR.IS.GT.ITEMS2+1)then 
              call edisp(iuout,'un-acceptable surface number.')
              close(23)
              return
            endif
            K=0
            k2=0
            CALL EGETWR(OUTSTR,K,TIM,0.,365.,'W','time',IER)
            DO 30 IC=2,IS
              CALL EGETWR(OUTSTR,K,RH,0.,99.,'W','RH',IER)
   30       CONTINUE
            DO 31 ID=1,IS-1
              CALL EGETWR(OUTSTR2,K2,TS,0.,99.,'W','TEMP',IER)
   31       CONTINUE

C Store recovered points in array and plot on chart.
            DAT1(ITS)=TIM
            DAT2(ITS)=RH
            DAT3(ITS)=TS

C If statistics are active then call the statistics get data routine to
c log the point.
            if(imstat.eq.1) then
              call mycostat(2,TS,RH)
            endif

C Check the point is within bounds
            ibound=1
            if(TS.GT.Xmax.OR.TS.LT.Xmin) ibound=0
            if(RH.GT.Ymax.OR.RH.LT.Ymin) ibound=0            
            if(iw.eq.1.and.ibound.eq.1) then
              call u2pixel(TS,RH,iix,iiy)
              call ecirc(iix,iiy,2,0) 
            endif            
            if(iw.eq.2) WRITE(24,'(F10.5,2X,F5.1,2X,F5.1)')TIM,RH,TS
            ITS=ITS+1
            NDP=ITS
          ENDIF
        ELSE
          CALL STRIPC(23,OUTSTR,99,ITEMS,1,'relative humidity',IER)
          CALL STRIPC(23,OUTSTR,99,ITEMS,1,'temperatures',IER)
          IF(IER.EQ.2)THEN
            CLOSE(23)
           if(iw.eq.2) CLOSE(24)
            return
          ELSEIF(IER.EQ.1)THEN
            CLOSE(23)
            if(iw.eq.2) CLOSE(24)
            return
          ENDIF
        ENDIF
   20 CONTINUE
      GOTO 10

   92 continue
      call edisp(iuout,'problem rewinding moisture results file.')
      return

      END
