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
#include "climate.h"
#include "help.h"

      COMMON/OUTPCH/ICOUT
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/RADTYP/IDNGH
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      common/exporttgi/ixopen,ixloc,ixunit

      DIMENSION ALT(24),AZI(24)
      character PERST1*14,PERST2*44,PERST3*44
      character outs*124
      logical ok

      helpinsub='clmint'  ! set for cfiles

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

CC Confirm the period to transfer.
C      NTSPH=1
C      CALL EPERSTR(IYEAR,IDS,IT1,IDF,IT2,NTSPH,
C     &             IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
C      helptopic='clm_just_in_case'
C      call gethelptext(helpinsub,helptopic,nbhelp)
C      CALL EASKOK(PERST3,'Analysis period OK?',
C     &         OK,nbhelp)
C      IF(.NOT.OK)THEN
C        CALL selper(ier)
C      ENDIF

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        call usrmsg(' Output being directed to file ...',' ','-')
      endif

C NOW READ VALUES OF THE WEATHER PARAMETERS FOR THIS DAY.
      DO 10 I=IDS,IDF
      IDAY=I
      CALL CLMGET(IDAY,IER)
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
        DIRH=CMRVAL(3,J)*ESIND(ALT(J))
        SRADT=CMRVAL(2,J)+DIRH
        DIRN=CMRVAL(3,J)
      ELSE
        DIRH=CMRVAL(4,J)-CMRVAL(2,J)
        SRADT=CMRVAL(4,J)
        DIRN=DIRH/ESIND(ALT(J))
      ENDIF

      IF(SRADT.GT.0.0001)THEN
        CLOUDC=1.-((CMRVAL(2,J)**2)/(SRADT**2))
      ENDIF

      FHSUM=FHSUM+CMRVAL(2,J)
      GHSUM=GHSUM+DIRH+CMRVAL(2,J)
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
     &     CMRVAL(2,J)*0.5*(1.+ECOSD(BETA))*
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
         SSUM1=SSUM1+DIRN*CAI+CMRVAL(2,J)*TILTF

      ELSEIF((SRADT-CMRVAL(2,J)).LT.5.)then

C Surface sun-lit under overcast sky.
         TILTF=ECOSD(BETA/2.)**2+.13*(ESIND(BETA)-BETA*R*ECOSD(BETA)
     &         -3.14159*ESIND(BETA/2.)**2)
         SSUM1=SSUM1+DIRN*CAI+CMRVAL(2,J)*CAI*(1.+ESIND(BETA/2.)**3)
         IF(ALT(J).GT.6.)SSUM1=SSUM1+DIRN*CAI+CMRVAL(2,J)*TILTF

      ELSE

C Surface sun-lit under non-overcast sky.
         TILTF=ECOSD(BETA/2.)**2-.5*(ESIND(BETA)-BETA*R*ECOSD(BETA)
     &         -3.14159*ESIND(BETA/2.)**2)
         SSUM1=SSUM1+DIRN*CAI+CMRVAL(2,J)*CAI*(1.+ESIND(BETA/2.)**3)
         IF(ALT(J).GT.6.)THEN
            FFX=(SRADT-CMRVAL(2,J))/XXE
            SSUM1=SSUM1+DIRN*CAI+CMRVAL(2,J)*((FFX*CAI/ESIND(ALT(J)))
     &                 +TILTF*(1.-FFX))
         ENDIF
      ENDIF

C Isotropic case.
      SSUM2=SSUM2+DIRN*CAI+0.5*0.25*(1.-ECOSD(90.-0.))*SRADT+
     &CMRVAL(2,J)*0.5*(1.+ECOSD(90.-0.))
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
      helptopic='clm_another_say_no'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKOK(' ','Continue with another period?',OK,nbhelp)
      IF(OK)GOTO 6
      RETURN
 
      END

C********** CSITEH
C   SUBROUTINE TO PRINT WEATHER HEADING

      SUBROUTINE CSITEH
C      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/OUTPCH/ICOUT
      CHARACTER*42 CLMLOC
      COMMON/CLMDT1/CLMLOC
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/exporttgi/ixopen,ixloc,ixunit
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
        WRITE(outs,101)CLMLOC(1:lnblnk(CLMLOC)),ACLAT,CLONG,IYEAR
 101    FORMAT(' Weather analysis:',1X,A,':',F7.2,'N',F7.2,'E :',I5)
        call edisp(itru,outs)
      elseif(IK.EQ.2)then
        WRITE(outs,102)CLMLOC(1:lnblnk(CLMLOC)),ACLAT,CLONG,IYEAR
 102    FORMAT(' Weather analysis:',1X,A,':',F7.2,'N',F7.2,'W :',I5)
        call edisp(itru,outs)
      elseif(IK.EQ.3)then
        WRITE(outs,103)CLMLOC(1:lnblnk(CLMLOC)),ACLAT,CLONG,IYEAR
 103    FORMAT(' Weather analysis:',1X,A,':',F7.2,'S',F7.2,'E :',I5)
        call edisp(itru,outs)
      elseif(IK.EQ.4)then
        WRITE(outs,104)CLMLOC(1:lnblnk(CLMLOC)),ACLAT,CLONG,IYEAR
 104    FORMAT(' Weather analysis:',1X,A,':',F7.2,'S',F7.2,'W :',I5)
        call edisp(itru,outs)
      endif
      RETURN
      END

