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

C CLMSEQ identifies days and hours within the user-specified period that
C contain values with a range of values and reports the occurrances. The
C weather data values to be scanned are passed via the IP parameter:
C   EXTERNAL AIR TEMPERATURE (IP = 2),
C  DIRECT NORMAL SOLAR RADIATION (IP = 3),
C  DIFFUSE HORIZONTAL SOLAR RADIATION (IP = 1),
C  WIND SPEED (IP = 4), WIND DIRECTION (IP = 5),
C  OR EXTERNAL RELATIVE HUMIDITY (IP = 6).
C This facility allows 'cold' or 'hot' or 'nice' days to be located.

      SUBROUTINE CLMSEQ(IP,XL,XU,nbdays,nbhours)
      PARAMETER (MT=24)

C Passed parameters:
      integer IP  ! which weather variable
      real XL     ! lower range
      real XU     ! upper range
      integer nbdays    ! returned number of days meeting criteria
      integer nbhours   ! returned number of hours meeting criteria

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      COMMON/CLMVAR/VNAME(6)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      common/exporttgi/ixopen,ixloc,ixunit

      integer NDAY
      real CDIF,CTMP,CDNR,CVEL,CDIR,CHUM
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     &              CDIR(MT),CHUM(MT)
      character PERST1*14,PERST2*44,PERST3*44,outs*124,VNAME*32
      integer nbhoursinday   ! occurrances for current day
      character descr*7,DESCR1*10 
    
C Output header information.
      call edisp(iuout,' ')
      call edisp(iuout,' Synoptic analysis of climatic data.')
      call edisp(iuout,' ')

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        call usrmsg(' Output being directed to file...',' ','-')
      endif
      nbdays = 0; nbhours = 0; nbhoursinday = 0  ! reset values
      NTSPH=1
      IFDAY=2
      IFTIME=0
      CALL EPERSTR(IYEAR,IDS,IT1,IDF,IT2,NTSPH,
     &             IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
      WRITE(outs,'(1X,A44)')PERST2
      call edisp(iuout,outs)

      WRITE(outs,'(2A)')' Analysis of ',VNAME(IP)
      call edisp(itru,outs)
      WRITE(outs,51)XL,XU
51    FORMAT(' specified range :',F8.2,' to ',F8.2)
      call edisp(itru,outs)
      call edisp(itru,' ')

      call edisp(itru,' Days (within period) during which the above')
      call edisp(itru,' parameter falls within specified range:')
      call edisp(itru,' ')
      XXL=XL    ! use local variables
      XXU=XU
      DO 10 I=IDS,IDF   ! days of the current user period

C Read weather data for this day.
        ID=I
        CALL CLMGET(ID)
        JS=1
        JF=24
        nbhoursinday = 0     ! reset for today
        call stdate(iyear,ID,DESCR,DESCR1)  ! get string describing the day
        IF(I.EQ.IDS)JS=IT1   ! if start day also check start hour
        IF(I.EQ.IDF)JF=IT2   ! if end day also check end hour
        DO 20 J=JS,JF        ! loop hours of the current period

C Determine if appropriate parameter is within range.
          if(IP.eq.1)then
            X=CDIF(J)
          elseif(IP.eq.2)then
            X=CTMP(J)
          elseif(IP.eq.3)then
            X=CDNR(J)
          elseif(IP.eq.4)then
            X=CVEL(J)
          elseif(IP.eq.5)then
            X=CDIR(J)
          elseif(IP.eq.6)then
            X=CHUM(J)
          endif

          if(XXU.GT.X.AND.XXL.LT.X)then
            nbhours = nbhours+1            ! increment overall summation
            nbhoursinday = nbhoursinday+1  ! increment day summation
          endif
   20   CONTINUE

C If a hit this day increment day counter.
        if(nbhoursinday.gt.0) nbdays = nbdays + 1

C Conver year day to day and month number format.
        WRITE(outs,'(3a,i3,a)') ' ',DESCR1,' had ',
     &    nbhoursinday,' hours within the range.'
        call edisp(itru,outs)
   10 CONTINUE

C If no hits then tell user about this
      if(nbhours.eq.0)then
        call edisp(iuout,' ')
        call edisp(iuout,' No days in specified period contain')
        call edisp(iuout,' values within the given range !')
      endif
      RETURN
 
      END
