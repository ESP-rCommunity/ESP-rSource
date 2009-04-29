C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-2009.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C The file commonclm.f is a collection of subroutines associated
C with climate data that are used by multiple applications.

C CLMGET Recover one day's data from climate file into common block for use.
C AZALT  Calculate the solar azimuth and elevation angles at each hour
C        of the day for any site.

C ---------- CLMGET
C Recover one day's data from climate file into common block for use.
      SUBROUTINE CLMGET(IDAY)
      implicit none
      
      integer MT
      PARAMETER (MT=24)

      integer iday  ! passed in julian day of the year
      integer ifil,iuout,iuin,idngh
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/RADTYP/IDNGH

      integer NDAY  ! current day that data has been scanned for
      real CDIF     ! diffuse solar
      real CTMP     ! ambient dry bulb temperature
      real CDNR     ! direct normal solar
      real CVEL     ! wind velocity
      real CDIR     ! wind direction
      real CHUM     ! ambient RH
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     &              CDIR(MT),CHUM(MT)
      COMMON/CLMVAR/VNAME(6)

      integer iclm,irec,j,k,istat
      DIMENSION ICLM(24,6)
      character outs*124,VNAME*32

      VNAME(1)='Diffuse horiz. radiation (W/m^2)'
      VNAME(2)='Dry bulb temperature (C)        '
      if(IDNGH.eq.0)then
        VNAME(3)='Direct normal radiation (W/m^2) '
      else
        VNAME(3)='Global horiz. radiation (W/m^2) '
      endif
      VNAME(4)='Wind velocity (m/s)             '
      VNAME(5)='Wind direction (degrees)        '
      VNAME(6)='Relative humidity (%)           '

      IF(IDAY.EQ.NDAY)GO TO 50
      IREC=IDAY
      READ(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1000)((ICLM(J,K),K=1,6),
     &J=1,24)
      DO 40 J=1,24
        CDIF(J)=real(ICLM(J,1))
        CTMP(J)=0.1* real(ICLM(J,2))
        CDNR(J)=real(ICLM(J,3))
        CVEL(J)=0.1* real(ICLM(J,4))
        CDIR(J)=real(ICLM(J,5))
        CHUM(J)=real(ICLM(J,6))
  40  CONTINUE
      NDAY=IDAY

  50  CONTINUE
      RETURN

C Error messages.
1000  write(outs,'(A,I4)')' Problem reading record ',IREC
      call edisp(iuout,outs)
      call edisp(iuout,' substituting zeros for data.')
      DO 10 J=1,24
        CDIF(J)=0.0
        CTMP(J)=0.0
        CDNR(J)=0.0
        CVEL(J)=0.0
        CDIR(J)=0.0
        CHUM(J)=0.0
  10  CONTINUE

      RETURN
      END

C ************ AZALT
C AZALT Calculate the solar azimuth and elevation angles at each hour
C of the day for any site.
      SUBROUTINE AZALT(IYD,ITS,RLONGT,RLATIT,SOLALT,SOLAZI)
      implicit none
      
      real esind,ecosd  ! function definitions

C Parameters:
      integer IYD,ITS
      real RLONGT,RLATIT,SOLALT,SOLAZI
      DIMENSION SOLALT(24),SOLAZI(24)
      
      integer INIT
      real GLAT,GLON
      COMMON/UAZALT/INIT,GLAT,GLON
      character h*72
      common/pophelp/h(60)

C Local definitions:
      logical OK,dok,close
      integer IHR
      real PI,DECLIN,SDDECL,CDDECL,CDTIME,TIME,TIMCOE,ABSTMC,ASDTIM
      real ALT,SDLAT,CDLAT,AZMUTH
      real A,B,EQTIME,X,Y

      PI = 4.0 * ATAN(1.0)
      INIT = 0
      GLAT = 55.9
      GLON = -4.15

C  IYD = DAY NUMBER OF THE YEAR (JAN 1=1, JAN 2=2,..., DEC 31=365)
C  ITS = TIME SYSTEM UNDER ANALYSIS, I.e. IF LOCAL MEAN
C        TIME (LMT), INPUT 1, IF LOCAL APPARENT TIME ,(LAT) ,INPUT 2.
C  RLONGT = LONGITUDINAL DIFFERENCE OF THE SITE MEASURED EAST(+VE)
C           OR WEST(-VE) OF THE REFERENCE TIME ZONE. IN BRITAIN THE
C           REFERENCE TIME ZONE IS AT GREENWICH (0 DEG LONGITUDE). (DEG)
C  RLATIT = LATITUDE OF SITE (NORTH +VE, SOUTH -VE) (DEGREES)

C CHECK LATITUDE AND LONGITUDE.
      IF(INIT.EQ.1)GOTO 18
      IF(RLATIT.GT.90.0.OR.RLATIT.LT.-90.0)goto 14
      IF(RLONGT.GT.15.0.OR.RLONGT.LT.-15.0)goto 14
 
C THE SIN AND COS OF THE LATITUDE ARE EVALUATED.
   15 SDLAT=ESIND(RLATIT)
      CDLAT=ECOSD(RLATIT)

C CALCULATION OF THE EQUATION OF TIME.
      IF(ITS.EQ.2)goto 11
      A=1.978*IYD-160.22
      B=0.989*IYD-80.11
      EQTIME=0.1645*ESIND(A)-0.1255*ECOSD(B)-0.025*ESIND(B)

C CALCULATION OF THE DECLINATION.
   11 A=280.1+0.9863*IYD
      DECLIN=23.45*ESIND(A)
      SDDECL=ESIND(DECLIN)
      CDDECL=ECOSD(DECLIN)

C IF LMT IS USED THEN THE LONGITUDE AND EQUATION OF TIME
C WILL EFFECT THE SOLAR TIME AT THE USERS SITE. A
C CORRECTION WILL THEREFORE BE REQUIRED.
      DO 10 IHR=1,24
      IF(ITS.EQ.2)GOTO 1
      TIME=IHR+(EQTIME+(RLONGT/15.0))
      GOTO 2
    1 TIME=IHR
    2 TIMCOE=15.0*(12.0-TIME)
      CDTIME=ECOSD(TIMCOE)
      ABSTMC=ABS(TIMCOE)
C      SDTIME=ESIND(TIMCOE)
      ASDTIM=ESIND(ABSTMC)

C CALCULATION OF THE SOLAR ALTITUDE.
      ALT=SDLAT*SDDECL+CDLAT*CDDECL*CDTIME
      SOLALT(IHR)=ASIN(ALT)*180.0/PI

C SOLAR AZIMUTH.
      AZMUTH=(CDDECL*ASDTIM)/ECOSD(SOLALT(IHR))
      IF(AZMUTH.LT.-1.0)AZMUTH=-1.0
      IF(AZMUTH.GT.1.0)AZMUTH=1.0
      SOLAZI(IHR)=ASIN(AZMUTH)*180.0/PI

C Correct the azimuthal angle for time of day and whether
C in north or south hemispheres.
      X=CDTIME
      call eclose(RLATIT,0.0,0.001,close)
      IF(close)goto 13
      call eclose(RLATIT,90.0,0.001,close)
      IF(close)goto 8
      Y=(CDLAT/SDLAT)*(SDDECL/CDDECL)
      goto 9
    8 Y=0.0
      goto 9
   13 Y=100.0*(SDDECL/CDDECL)
    9 IF(Y-X)3,4,5
    3 IF(RLATIT.GE.0.0)goto 6
      goto 7
    5 IF(RLATIT.LT.0.0)goto 6
      goto 7
    4 IF(TIME.LE.12.0)SOLAZI(IHR)=90.0
      IF(TIME.GT.12.0)SOLAZI(IHR)=270.0
      goto 10
    6 IF(TIME.LE.12.0)SOLAZI(IHR)=180.0-SOLAZI(IHR)
      IF(TIME.GT.12.0)SOLAZI(IHR)=180.0+SOLAZI(IHR)
      goto 10
    7 IF(TIME.GT.12.0)SOLAZI(IHR)=360.0-SOLAZI(IHR)
   10 CONTINUE
      RETURN

   14 dok=.true.
      h(1)='There are some limits that should not be exceeded.'
      h(2)='If you say no the the program will stop. '
      CALL ASKOK('Lat was outwith +-90.0 or Long difference more than',
     &  '+- 15 deg. Continue with default Lat/Long?',OK,dok,2)
      if(.NOT.OK)then
        CALL EPAGEND
        STOP
      endif
   18 RLATIT=GLAT
      RLONGT=GLON
      INIT=1
      goto 15
      END

