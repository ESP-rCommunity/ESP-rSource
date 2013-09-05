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

C 'CLMSEQ' IDENTIFIES THOSE DAYS WITHIN THE USER-SPECIFIED PERIOD THAT
C CONTAIN VALUES OF EXTERNAL AIR TEMPERATURE (IP = 2), DIRECT NORMAL
C SOLAR RADIATION (IP = 3), DIFFUSE HORIZONTAL SOLAR RADIATION (IP = 1),
C WIND SPEED (IP = 4), WIND DIRECTION (IP = 5) OR EXTERNAL RELATIVE
C HUMIDITY (IP = 6) WHOSE MAGNITUDE IS WITHIN SOME USER-SPECIFIED RANGE.
C THIS FACILITY ALLOWS 'COLD' OR 'HOT' DAYS ETC. TO BE LOCATED:
C SUBSEQUENT GRAPHICAL ANALYSIS COULD BE USED TO ESTABLISH IF THE
C DAYS SO IDENTIFIED WERE SUITABLE FOR THE INTENDED SIMULATION.

      SUBROUTINE CLMSEQ(IP,XL,XU)
      PARAMETER (MT=24)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      common/trc/itrc
      COMMON/CLMVAR/VNAME(6)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      common/exporttgi/ixopen,ixloc,ixunit

      integer NDAY
      real CDIF,CTMP,CDNR,CVEL,CDIR,CHUM
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     &              CDIR(MT),CHUM(MT)
      character PERST1*14,PERST2*44,PERST3*44,outs*124,VNAME*32

C OUTPUT HEADER INFORMATION.
      call edisp(iuout,' ')
      call edisp(iuout,' Synoptic analysis of climatic data.')
      call edisp(iuout,' ')

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        call usrmsg(' Output being directed to file...',' ','-')
      endif

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
      IC=0
      XXL=XL
      XXU=XU
      DO 10 I=IDS,IDF

C READ VALUES OF THE CLIMATIC PARAMETERS FOR THIS DAY.
      ID=I
      CALL CLMGET(ID)
      JS=1
      JF=24
      IF(I.EQ.IDS)JS=IT1
      IF(I.EQ.IDF)JF=IT2
      DO 20 J=JS,JF

C NOW DETERMINE IF APPROPRIATE PARAMETER LIES WITHIN
C SPECIFIED RANGE.
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

      IF(XXU.GT.X.AND.XXL.LT.X)GO TO 11
   20 CONTINUE
      goto 10
   11 II=I

C CONVERT YEAR DAY TO DAY AND MONTH NUMBER FORMAT.
      CALL EDAYR(II,ID,IM)
      WRITE(outs,12)ID,IM
   12 FORMAT(' Day',I3,' of month',I3)
      call edisp(itru,outs)
      IC=1
   10 CONTINUE
      IF(IC.EQ.1)goto 13
      call edisp(iuout,' ')
      call edisp(iuout,' No days in specified period contain')
      call edisp(iuout,' values within the given range !')
   13 RETURN

      END
