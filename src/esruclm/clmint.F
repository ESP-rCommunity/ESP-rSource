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

C 'CLMINT' sums direct (or global) and diffuse radiation
C time-series data over any period. For the vertical surface
C component data is given for both isotropic and anisotropic skies.

      SUBROUTINE CLMINT
      PARAMETER (MT=24)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTPCH/ICOUT
      common/pophelp/h(60)
      COMMON/CLMDT0/CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/RADTYP/IDNGH
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      common/exporttgi/ixopen,ixloc,ixunit

      integer NDAY
      real CDIF,CTMP,CDNR,CVEL,CDIR,CHUM
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     A              CDIR(MT),CHUM(MT)
      DIMENSION ALT(24),AZI(24)
      character PERST1*14,PERST2*44,PERST3*44
      character outs*124,h*72
      logical ok,dok

    6 FHSUM=0.
      GHSUM=0.
      DNSUM=0.
      DHSUM=0.
      SSUM=0.
      SSUM1=0.
      SSUM2=0.
      NHRS=0

C Default cloud cover for use during sun down periods.
      CLOUDC=0.5

C Confirm the period to transfer.
      NTSPH=1
      CALL EPERSTR(IYEAR,IDS,IT1,IDF,IT2,NTSPH,
     &             IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
      dok=.true.
      h(1)='Just in case you made an error.... '
      CALL ASKOK(' Is the analysis period (see below) correct?',
     &  PERST3,OK,dok,1)
      IF(.NOT.OK)THEN
        CALL selper(ier)
      ENDIF

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        call usrmsg(' Output being directed to file...',' ','-')
      endif

C NOW READ VALUES OF THE CLIMATIC PARAMETERS FOR THIS DAY.
      DO 10 I=IDS,IDF
      IDAY=I
      CALL CLMGET(IDAY)
      CALL AZALT(IDAY,1,CLONG,CLAT,ALT,AZI)
      DO 20 J=1,24
      IF(ALT(J).LT.0.001)THEN
        DIRH=0.
        DIRN=0.
        CAI=0.
        SRADT=0.
        goto 20
      ENDIF

      NHRS=NHRS+1

      IF(IDNGH.EQ.0)THEN
        DIRH=CDNR(J)*ESIND(ALT(J))
        SRADT=CDIF(J)+DIRH
        DIRN=CDNR(J)
      ELSE
        DIRH=CDNR(J)-CDIF(J)
        SRADT=CDNR(J)
        DIRN=DIRH/ESIND(ALT(J))
      ENDIF

      IF(SRADT.GT.0.0001)THEN
        CLOUDC=1.-((CDIF(J)**2)/(SRADT**2))
      ENDIF

      FHSUM=FHSUM+CDIF(J)
      GHSUM=GHSUM+DIRH+CDIF(J)
      DNSUM=DNSUM+DIRN

C Set BETA for a vertical surface, elevation = 0.
      ELEV=0.
      BETA=90.-ELEV

C Set CAI for a vertical south surface.
      CAI=ECOSD(ALT(J))*ESIND(BETA)*ECOSD(ABS(180.-AZI(J)))+
     &    ESIND(ALT(J))*ECOSD(BETA)
      IF(CAI.LT.0.)CAI=0.

C Anisotropic case (Klucher model).
      SSUM=SSUM+DIRN*CAI+0.5*0.25*(1.-ECOSD(BETA))*SRADT+
     &     CDIF(J)*0.5*(1.+ECOSD(BETA))*
     &     (1.0+(CLOUDC*((ESIND(0.5*(BETA)))**3)))*
     &     (1.0+(CLOUDC*((ESIND(90.-ALT(J)))**3)*CAI*CAI))

C Anisotropic case (Muneer model).
      R=3.14159/180.
      XXE=1353.*(1.+.033*COS(.017203*IDAY))*ESIND(ALT(J))
      XIN=ACOS(CAI)
      IF(XIN.GE.1.57)THEN

C Surface shaded.
         TILTF=ECOSD(BETA/2.)**2+.25*(ESIND(BETA)-BETA*R*ECOSD(BETA)
     &         -3.14159*ESIND(BETA/2.)**2)
         SSUM1=SSUM1+DIRN*CAI+CDIF(J)*TILTF

      ELSEIF((SRADT-CDIF(J)).LT.5.)then

C Surface sun-lit under overcast sky.
         TILTF=ECOSD(BETA/2.)**2+.13*(ESIND(BETA)-BETA*R*ECOSD(BETA)
     &         -3.14159*ESIND(BETA/2.)**2)
         SSUM1=SSUM1+DIRN*CAI+CDIF(J)*CAI*(1.+ESIND(BETA/2.)**3)
         IF(ALT(J).GT.6.)SSUM1=SSUM1+DIRN*CAI+CDIF(J)*TILTF

      ELSE

C Surface sun-lit under non-overcast sky.
         TILTF=ECOSD(BETA/2.)**2-.5*(ESIND(BETA)-BETA*R*ECOSD(BETA)
     &         -3.14159*ESIND(BETA/2.)**2)
         SSUM1=SSUM1+DIRN*CAI+CDIF(J)*CAI*(1.+ESIND(BETA/2.)**3)
         IF(ALT(J).GT.6.)THEN
            FFX=(SRADT-CDIF(J))/XXE
            SSUM1=SSUM1+DIRN*CAI+CDIF(J)*((FFX*CAI/ESIND(ALT(J)))
     &                 +TILTF*(1.-FFX))
         ENDIF
      ENDIF

C Isotropic case.
      SSUM2=SSUM2+DIRN*CAI+0.5*0.25*(1.-ECOSD(90.-0.))*SRADT+
     &CDIF(J)*0.5*(1.+ECOSD(90.-0.))
   20 CONTINUE
   10 CONTINUE

      FHSUM=FHSUM/1000.
      GHSUM=GHSUM/1000.
      DHSUM=GHSUM-FHSUM
      DNSUM=DNSUM/1000.
      SSUM=SSUM/1000.
      SSUM1=SSUM1/1000.
      SSUM2=SSUM2/1000.

      H1=FHSUM*1000./NHRS
      H2=DHSUM*1000./NHRS
      H3=GHSUM*1000./NHRS
      H4=DNSUM*1000./NHRS
      H5=SSUM*1000./NHRS
      H6=SSUM1*1000./NHRS
      H7=SSUM2*1000./NHRS

      NDAYS=IDF-IDS+1
      D1=FHSUM/NDAYS
      D2=DHSUM/NDAYS
      D3=GHSUM/NDAYS
      D4=DNSUM/NDAYS
      D5=SSUM/NDAYS
      D6=SSUM1/NDAYS
      D7=SSUM2/NDAYS

      CALL CSITEH
      NTSPH=1
      CALL EPERSTR(IYEAR,IDS,IT1,IDF,IT2,NTSPH,
     &             IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
      write(outs,'(1X,A44)')PERST2
      call edisp(itru,outs)
      WRITE(outs,33)NDAYS
   33 FORMAT(' Number of days during period (D)         = ',I4)
      call edisp(itru,' ')
      call edisp(itru,outs)
      WRITE(outs,331)NHRS
  331 FORMAT(' Number of sun-up hours during period (N) = ',I4)
      call edisp(itru,outs)

      call edisp(itru,
     & '                        Solar radiation values')
      call edisp(itru,
     & '                        Total (T)  Daily (T/D)   Mean(T/N)')
      call edisp(itru,
     & '                        kWh/m^2    kWh/m^2       W/m^2')
      WRITE(outs,2)FHSUM,D1,H1
    2 FORMAT(' Diffuse horizontal       = ',F9.2,3X,F9.2,4X,F9.2)
      call eddisp(itru,outs)
      WRITE(outs,21)DHSUM,D2,H2
   21 FORMAT(' Direct horizontal        = ',F9.2,3X,F9.2,4X,F9.2)
      call eddisp(itru,outs)
      WRITE(outs,22)GHSUM,D3,H3
   22 FORMAT(' Global horizontal        = ',F9.2,3X,F9.2,4X,F9.2)
      call eddisp(itru,outs)
      WRITE(outs,23)DNSUM,D4,H4
   23 FORMAT(' Direct normal            = ',F9.2,3X,F9.2,4X,F9.2)
      call eddisp(itru,outs)
      call edisp(itru,' ')
      WRITE(outs,24)SSUM,D5,H5
   24 FORMAT(' South V.(anisotropic; K) = ',F9.2,3X,F9.2,4X,F9.2)
      call eddisp(itru,outs)
      WRITE(outs,25)SSUM1,D6,H6
   25 FORMAT(' South V.(anisotropic; M) = ',F9.2,3X,F9.2,4X,F9.2)
      call eddisp(itru,outs)
      WRITE(outs,26)SSUM2,D7,H7
   26 FORMAT(' South V.(isotropic)      = ',F9.2,3X,F9.2,4X,F9.2)
      call eddisp(itru,outs)

C CONSIDER ANOTHER PERIOD ?
      dok=.true.
      h(1)='Say no to stop looking at the data. '
      CALL ASKOK(' ',' Continue with another period?',OK,dok,1)
      IF(OK)GOTO 6
      RETURN
 
      END

C ************
C 'AZALT' CALCULATES THE SOLAR AZIMUTH AND THE SOLAR ALTITUDE
C ANGLES AT EACH HOUR OF THE DAY , FOR ANY SITE.
      SUBROUTINE AZALT(IYD,ITS,RLONGT,RLATIT,SOLALT,SOLAZI)
      COMMON/UAZALT/INIT,GLAT,GLON
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      DIMENSION SOLALT(24),SOLAZI(24)
      logical OK,dok,close
      character h*72

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

C  CHECK LATITUDE AND LONGITUDE.
      IF(INIT.EQ.1)GOTO 18
      IF(RLATIT.GT.90.0.OR.RLATIT.LT.-90.0)goto 14
      IF(RLONGT.GT.15.0.OR.RLONGT.LT.-15.0)goto 14
 
C  THE SIN AND COS OF THE LATITUDE ARE EVALUATED.
   15 SDLAT=ESIND(RLATIT)
      CDLAT=ECOSD(RLATIT)

C  CALCULATION OF THE EQUATION OF TIME.
      IF(ITS.EQ.2)goto 11
      A=1.978*IYD-160.22
      B=0.989*IYD-80.11
      EQTIME=0.1645*ESIND(A)-0.1255*ECOSD(B)-0.025*ESIND(B)

C  CALCULATION OF THE DECLINATION.
   11 A=280.1+0.9863*IYD
      DECLIN=23.45*ESIND(A)
      SDDECL=ESIND(DECLIN)
      CDDECL=ECOSD(DECLIN)

C  IF LMT IS USED THEN THE LONGITUDE AND EQUATION OF TIME
C  WILL EFFECT THE SOLAR TIME AT THE USERS SITE. A
C  CORRECTION WILL THEREFORE BE REQUIRED.
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

C  CALCULATION OF THE SOLAR ALTITUDE.
      ALT=SDLAT*SDDECL+CDLAT*CDDECL*CDTIME
      SOLALT(IHR)=ASIN(ALT)*180.0/PI

C  SOLAR AZIMUTH.
      AZMUTH=(CDDECL*ASDTIM)/ECOSD(SOLALT(IHR))
      IF(AZMUTH.LT.-1.0)AZMUTH=-1.0
      IF(AZMUTH.GT.1.0)AZMUTH=1.0
      SOLAZI(IHR)=ASIN(AZMUTH)*180.0/PI

C  Correct the azimuthal angle for time of day and whether
C  in north or south hemispheres.
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

   14 call edisp(iuout,' Lat/Lon error in AZALT :')
      call edisp(iuout,' default site Glasgow (55.9N, 4.15W)')
      dok=.true.
      h(1)='There are some limits that should not be exceeded.'
      h(2)='If you say no the the program will stop. '
      CALL ASKOK('Lat was outwith +-90.0 or Long difference more than',
     &  '+- 15 deg. Continue with default Lat/Long?',OK,dok,2)
      if(.NOT.OK)STOP
   18 RLATIT=GLAT
      RLONGT=GLON
      INIT=1
      goto 15
      END

C********** CSITEH
C   SUBROUTINE TO PRINT CLIMATE HEADING

      SUBROUTINE CSITEH
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      CHARACTER*30 CLMLOC
      COMMON/CLMDT1/CLMLOC
      COMMON/CLMDT0/CLAT,CLONG
      common/exporttgi/ixopen,ixloc,ixunit
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      character outs*124

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
      endif
      ACLAT=ABS(CLAT)
      ACLONG=ABS(CLONG)
      IK=1
      IF(CLONG.LT.0.0.AND.CLAT.GE.0.0)IK=2
      IF(CLONG.GE.0.0.AND.CLAT.LT.0.0)IK=3
      IF(CLONG.LT.0.0.AND.CLAT.LT.0.0)IK=4
      if(IK.EQ.1)then
        WRITE(outs,101)CLMLOC,ACLAT,ACLONG,IYEAR
 101    FORMAT(' Climate analysis:',1X,A30,':',F7.2,'N',F7.2,'E :',I5)
        call edisp(itru,outs)
      elseif(IK.EQ.2)then
        WRITE(outs,102)CLMLOC,ACLAT,ACLONG,IYEAR
 102    FORMAT(' Climate analysis:',1X,A30,':',F7.2,'N',F7.2,'W :',I5)
        call edisp(itru,outs)
      elseif(IK.EQ.3)then
        WRITE(outs,103)CLMLOC,ACLAT,ACLONG,IYEAR
 103    FORMAT(' Climate analysis:',1X,A30,':',F7.2,'S',F7.2,'E :',I5)
        call edisp(itru,outs)
      elseif(IK.EQ.4)then
        WRITE(outs,104)CLMLOC,ACLAT,ACLONG,IYEAR
 104    FORMAT(' Climate analysis:',1X,A30,':',F7.2,'S',F7.2,'W :',I5)
        call edisp(itru,outs)
      endif
      RETURN
      END

