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

C This file (rlibget.f) contains the following routines:
C  GCFRAC: read values of FRAC (controlled casual gains).
C  GCASR:  read zone casual radiant flux.
C  GCASC:  read zone casual convective flux.
C  GZRH:   read zone relative humidity values.
C  GZLLH:  read zone humidification load.
C  GZLLD:  read zone dehumidification load.
C  GZLL:   read zone latent load.
C  GZQM:   read zone sensible load.
C  GZQSL:  read zone sensible plus latent load.
C  GZQMH:  read zone sensible heating flux.
C  GZQMC:  read zone sensible cooling flux.
C  GZSPM1: read zone special material output 1
C  GZSPM2: reas zone special material output 2
C  GTMCON: read zone control point temperature.
C  GOUTOPQ: read zone outside facing opaque convection.
C  GOUTTRN: read zone outside facing trn convection.
C  GOPQIN: read zone inside opaque convection.
C  GTRNIN: read zone inside trn convection.
C  GZAIRT: read zone air node temperatures.
C  GTMS:   read inside surface temperatures.
C  GQV1:   read zone infiltration.
C  GQV2:   read interzone ventilation.
C  GTMC:   read MLC node temperature.
C  GTMSO:  read `other side' surface temperature
C  GQSI:   read solar absorbed on inside surface.
C  GSOLEX: read total solar entering from outside.
C  GSOLADJ: read total solar entering from other zones.
C  GSURIC: read surface convection.
C  GILLUM: recover sky lux level from solar data as in INTLUM.
C  GPCR1:  read zone real power consumption.
C  GPCQ2:  read zone reactive power consumption.
C  GSOLABS: read total solar absorbed in zone.
C  GQSE:   read solar absorbed on outside surface.
C  GTBR:   read thermal bridge flux for zone.
C GAIRSTOR reads heat stored in the zone air node
C  GZTMS:  read zone air and surf temps at a timestep/halfstep.
C  GZSOLA: read one timestep solar absorption inside & outside.
C  GSURFB: read one timestep surface balance data (save=4 only)
C  G4ALLS: read one timesteps selected data for surfaces (save=4 only).
C  Getallcas: read one timestep of the total conv/radiant and separate
C     casual gain conv/rad/latent data for a zone and control fraction.
C  GFIXED: read days single fixed surface balance items.
C  HASHL   calculate offset to base record at each timestep and zone.

C ******************** GCFRAC ********************
C GCFRAC is a low level routine to read values of FRAC, the control
C fraction used with casual gain control, from the results
C library for a given day and zone.
C Given IDAY, IZONE returns FRAC (in common GVAL) at each time step.
C CALLS: MOREC1 MOREC2

      SUBROUTINE GCFRAC(IDAY,IZONE,ISET)
#include "building.h"
#include "geometry.h"
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)
      COMMON/HASH/IHASHT(MTS),IHASHZ(MCOM),IHASHD(365),IHASHS(MNRS)

      character outs*124

      DIMENSION XD(MZS)

      IUNIT=IFIL

C Get FRAC only if save is level 2 - 4, then compute start record
C address for requested recovery day and recover all timesteps.
      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        N=24*NTS
        DO 20 I=1,N
          IREC=IHASHD(IDAY)+IHASHT(I)+IHASHZ(IZONE)+IHASHS(ISET)+2
          if(irec.eq.0) goto 999
          NN=NZSUR(IZONE)
          JB1=NN+5
          READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=5)(XD(K),K=1,JB1)

C Test that 0.0 < FRAC > 1.0.
          IF(XD(JB1).GE.0.0.AND.XD(JB1).LE.1.0)THEN

C Express fraction as a percentage.
            GVAL(I)=XD(JB1)*100.
          ELSE
            goto 5
          ENDIF
          goto 6

C Error in reading frac, probably from an old version of bps so reread
C this record and assign an assumed value to FRAC.
    5     if(IOS.eq.2)then
            CALL USRMSG(
     &        'No permission to read value of casual gain fraction',
     &        ' ','W')
          else
            JB1=NN+4
            READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)(XD(K),K=1,JB1)
            GVAL(I)=100.0
          endif
    6     CONTINUE
   20   CONTINUE
      ELSE
        call edisp(iuout,
     &' GCFRAC: Save option does not allow access to FRAC.')
        goto  8
      ENDIF

    8 RETURN
 1000 if(IOS.eq.2)then
        write(outs,'(a,i5,a)')
     &    ' GCFRAC: no permission to read FRAC at record',IREC,'.'
      else
        write(outs,'(a,i5,a)')
     &    ' GCFRAC: error reading FRAC at record',IREC,'.'
      endif
      call edisp(iuout,outs)
      CALL EPWAIT
      goto 8

 999  write(outs,'(a,4i6)') 
     &  'GCFRAC: record out of range (day? zone?) scanning xd @ rec ',
     &  IREC,IDAY,IZONE,ISET
      call usrmsg(outs,' cannot continue results recovery.','W')
      CALL EPWAIT
      goto 8

      END

C ******************** GCASR ********************
C GCASR is a low level routine to read zone casual radiant flux values
C from the results library for a given day and zone.
C Given IDAY, IZONE returns QCASR (within common GVAL) if save level 2 - 4.
C CALLS: GFIXED
C NOTE: icjump nzsur+1
      SUBROUTINE GCASR(IDAY,IZONE,ISET)
#include "building.h"
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

C Send record and column jump to GFIXED for location of QCASR.
      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 2
        ICJUMP = NZSUR(IZONE)+1
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &  ' GCASR: Save option does not allow access to QCASR.')
      ENDIF
      RETURN
      END

C ******************** GCASC ********************
C GCASC is a low level routine to read zone casual convective flux values
C from the results library for a given day and zone, ISAVE 2 - 4 only.
C Given IDAY, IZONE returns values in the array QCASC (in common GVAL).
C CALLS: GFIXED
C NOTE: icjump nzsur+2
      SUBROUTINE GCASC(IDAY,IZONE,ISET)
#include "building.h"
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 2
        ICJUMP = NZSUR(IZONE)+2
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &  ' GCASC: Save option does not allow access to QCASC.')
      ENDIF
      RETURN
      END

C ******************** GZRH ********************
C GZRH is a low level routine to read zone relative humidity values
C from the results library for a given day and zone, ISAVE 2 - 4 only.
C Given IDAY, IZONE returns values in common GVAL.
C CALLS: GFIXED
C NOTE: icjump nzsur+3
      SUBROUTINE GZRH(IDAY,IZONE,ISET)
#include "building.h"
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 2
        ICJUMP = NZSUR(IZONE)+3
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &  ' GZRH: Save option does not allow access to ZRH.')
      ENDIF
      RETURN
      END

C ******************** GZDP ********************
C GZDP is a low level routine to read zone dew point values.
C from the results library for a given day and zone, ISAVE 2 - 4 only.
C Given IDAY, IZONE returns values in common GVAL.
C CALLS: GFIXED
C NOTE: icjump nzsur+3
      SUBROUTINE GZDP(IDAY,IZONE,ISET)
#include "building.h"
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN

C Get zone RH into GVAL and then into XDUM.
        IRJUMP = 2
        ICJUMP = NZSUR(IZONE)+3
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
        N=24*NTS
        DO 20 I=1,N
          XDUM(I)=GVAL(I)
  20    continue

C Get zone db into GVAL and then into XDUM1.
        IRJUMP = 0
        ICJUMP = 1
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
        DO 21 I=1,N
          XDUM1(I)=GVAL(I)
  21    continue
        DO 22 IQQ=1,N
          call MODEWP(XDUM1(iqq),XDUM(iqq),GVAL(iqq))

C Debug.
C          write(6,*) 'modewp dbt rh dp',XDUM1(iqq),XDUM(iqq),GVAL(iqq)

  22    continue
      ELSE
        call edisp(iuout,
     &  ' GZDP: Save option does not allow access to Zone DP.')
      ENDIF
      RETURN
      END

C ******************** GZLLH ********************
C GZLLH read zone humidification load from the results
C library for a given day and zone, ISAVE 2 - 4 only.
C Given IDAY, IZONE returns values in (in common GVAL).
C CALLS: GFIXED
C NOTE: icjump nzsur+4
      SUBROUTINE GZLLH(IDAY,IZONE,ISET)
#include "building.h"
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 2
        ICJUMP = NZSUR(IZONE)+4
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &  ' GZLLH: Save option does not allow access')
        call edisp(iuout,' to zone latent loading.')
      ENDIF

C Read only +ve values
      N=24*NTS
      DO 20 I=1,N
        if(GVAL(I).LT.0.)GVAL(I)=0.
  20  continue
      RETURN
      END

C ******************** GZLLC ********************
C GZLLC reads zone dehumidification load from the results
C library for a given day and zone, ISAVE 2 - 4 only.
C Given IDAY, IZONE returns values in (in common GVAL).
C CALLS: GFIXED
C NOTE: icjump nzsur+4
      SUBROUTINE GZLLC(IDAY,IZONE,ISET)
#include "building.h"
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 2
        ICJUMP = NZSUR(IZONE)+4
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &  ' GZLLC: Save option does not allow access')
        call edisp(iuout,' to zone latent loading.')
      ENDIF

C Read -ve values.
      N=24*NTS
      DO 20 I=1,N
        if(GVAL(I).GT.0.)GVAL(I)=0.
  20  continue

      RETURN
      END

C ******************** GZLL ********************
C GZLL reads the zone latent load, ISAVE 2 - 4 only.
C Given IDAY, IZONE returns values in (in common GVAL).
C CALLS: GFIXED
C NOTE: icjump nzsur+4
      SUBROUTINE GZLL(IDAY,IZONE,ISET)
#include "building.h"
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 2
        ICJUMP = NZSUR(IZONE)+4
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &  ' GZLL: Save option does not allow access')
        call edisp(iuout,' to zone latent loading.')
      ENDIF

      RETURN
      END

C ******************** GZSPM1 ********************
C GZSPM1 reads the special materials output, ISAVE 2 - 4 only.
C Given IDAY, IZONE returns values in (in common GVAL).
C CALLS: GFIXED
C NOTE: icjump nzsur+6
      SUBROUTINE GZSPM1(IDAY,IZONE,ISET)
#include "building.h"
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 2
        ICJUMP = NZSUR(IZONE)+6
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &  ' GZSPM1: Save option does not allow access')
        call edisp(iuout,' to zone latent loading.')
      ENDIF

      RETURN
      END

C ******************** GZSPM2 ********************
C GZSPM2 reads the special materials output, ISAVE 2 - 4 only.
C Given IDAY, IZONE returns values in (in common GVAL).
C CALLS: GFIXED
C NOTE: icjump nzsur+7
      SUBROUTINE GZSPM2(IDAY,IZONE,ISET)
#include "building.h"
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 2
        ICJUMP = NZSUR(IZONE)+7
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &  ' GZSPM2: Save option does not allow access')
        call edisp(iuout,' to zone latent loading.')
      ENDIF

      RETURN
      END

C ******************** GZAIRT ********************
C GZAIRT is a low level routine to read zone air node temperatures
C from the results library for a given day and zone for ISAVE= 1 - 4.
C Given IDAY, IZONE returns values for each timestep in common GVAL.
C CALLS: GFIXED

      SUBROUTINE GZAIRT(IDAY,IZONE,ISET)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      IF(ISAVE.GE.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 0
        ICJUMP = 1
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &  ' GZAIRT: Save option does not allow access')
        call edisp(iuout,' to zone air temperature.')
      ENDIF
      RETURN
      END



C ******************** GZQSL ********************
C GZQSL reads zone total load (sensible + latent) from the results
C library for a given day and zone. Read location based on ISAVE.
C Given IDAY, IZONE returns values for each timestep in common GVAL.
C CALLS: GFIXED

      SUBROUTINE GZQSL(IDAY,IZONE,ISET)
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

      DIMENSION TPVAL(MTS)

C Firstly recover the sensible values. 
      CALL GZQM(IDAY,IZONE,ISET)

C Store in a temporary array 
      N=24*NTS
      DO 20 I=1,N
        TPVAL(I)=GVAL(I)
  20  continue

C Call the routine to get the latent loading.
      CALL GZLL(IDAY,IZONE,ISET)       

C Output the sum of the two variables. 
      DO 30 I=1,N
        GVAL(I)=GVAL(I)+TPVAL(I)
  30  continue

      RETURN
      END

C ******************** GZQM ********************
C GZQM is a low level routine to read zone plant load from the results
C library for a given day and zone. Read location based on ISAVE.
C Given IDAY, IZONE returns values for each timestep in common GVAL.
C CALLS: GFIXED

      SUBROUTINE GZQM(IDAY,IZONE,ISET)
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

      IF(ISAVE.GE.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 0
        IF(ISAVE.EQ.1)ICJUMP=3
        IF(ISAVE.GE.2.AND.ISAVE.LE.4)ICJUMP=2
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &  ' GZQM: Save option does not allow access')
        call edisp(iuout,' to zone mean plant flux.')
      ENDIF

      RETURN
      END

C ******************** GZQMH ********************

C GZQMH is a low level routine to read zone plant heating flux from the results
C library for a given day and zone. Read location based on ISAVE.
C Given IDAY, IZONE returns values for each timestep in common GVAL.
C CALLS: GFIXED

      SUBROUTINE GZQMH(IDAY,IZONE,ISET)
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

      IF(ISAVE.GE.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 0
        IF(ISAVE.EQ.1)ICJUMP=3
        IF(ISAVE.GE.2.AND.ISAVE.LE.4)ICJUMP=2
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &  ' GZQM: Save option does not allow access')
        call edisp(iuout,' to zone mean plant flux.')
      ENDIF
      N=24*NTS
      DO 20 I=1,N
        if(GVAL(I).LT.0.)GVAL(I)=0.
  20  continue
      RETURN
      END

C ******************** GZQMC ********************

C GZQMC is a low level routine to read zone plant cooling flux from the results
C library for a given day and zone. Read location based on ISAVE.
C Given IDAY, IZONE returns values for each timestep in common GVAL.
C CALLS: GFIXED

      SUBROUTINE GZQMC(IDAY,IZONE,ISET)
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

      IF(ISAVE.GE.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 0
        IF(ISAVE.EQ.1)ICJUMP=3
        IF(ISAVE.GE.2.AND.ISAVE.LE.4)ICJUMP=2
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &  ' GZQM: Save option does not allow access')
        call edisp(iuout,' to zone mean plant flux.')
      ENDIF
      N=24*NTS
      DO 20 I=1,N
        if(GVAL(I).GT.0.)GVAL(I)=0.
  20  continue
      RETURN
      END

C ******************** GTMCON ********************

C GTMCON is a low level routine to read control point temperature
C from the results library for a given day and zone for ISAVE=1 - 4.
C Given IDAY, IZONE returns values for each timestep in common GVAL.
C CALLS: MOREC1 MOREC2
C NOTE: icjump nzsur+5
      SUBROUTINE GTMCON(IDAY,IZONE,ISET)
#include "building.h"
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      IF(ISAVE.GE.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 0
        IF(ISAVE.EQ.1)ICJUMP=2
        IF(ISAVE.GE.2.AND.ISAVE.LE.4)ICJUMP=NZSUR(IZONE)+5
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &  ' GTMCON: Save option does not allow access')
        call edisp(iuout,' to zone control point temperature.')
      ENDIF
      RETURN
      END

C ******************** GOUTOPQ ********************

C GOUTOPQ is a low level routine to read outside opaque convection
C from the results library for a given day and zone for ISAVE=2 - 4.
C Given IDAY, IZONE returns values for each timestep in common GVAL.
C NOTE: icjump nzsur+6
      SUBROUTINE GOUTOPQ(IDAY,IZONE,ISET)
#include "building.h"
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      IF(ISAVE.GE.2.AND.ISAVE.LE.4)THEN
        IRJUMP = 0
        IF(ISAVE.GE.2.AND.ISAVE.LE.4)ICJUMP=NZSUR(IZONE)+6
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &  ' GOUTOPQ: Save option does not allow access')
        call edisp(iuout,' to zone control point temperature.')
      ENDIF
      RETURN
      END

C ******************** GOUTTRN ********************

C GOUTTRN is a low level routine to read outside trn convection
C from the results library for a given day and zone for ISAVE=2 - 4.
C Given IDAY, IZONE returns values for each timestep in common GVAL.
C NOTE: icjump nzsur+7

      SUBROUTINE GOUTTRN(IDAY,IZONE,ISET)
#include "building.h"
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      IF(ISAVE.GE.2.AND.ISAVE.LE.4)THEN
        IRJUMP = 0
        IF(ISAVE.GE.2.AND.ISAVE.LE.4)ICJUMP=NZSUR(IZONE)+7
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &  ' GOUTTRN: Save option does not allow access')
        call edisp(iuout,' to zone control point temperature.')
      ENDIF
      RETURN
      END

C ******************** GOPQIN ********************

C GOPQIN is a low level routine to read inside opaque convection
C from the results library for a given day and zone for ISAVE=2 - 4.
C Given IDAY, IZONE returns values for each timestep in common GVAL.
C NOTE: icjump nzsur+6
      SUBROUTINE GOPQIN(IDAY,IZONE,ISET)
#include "building.h"
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      IF(ISAVE.GE.2.AND.ISAVE.LE.4)THEN
        IRJUMP = 1
        IF(ISAVE.GE.2.AND.ISAVE.LE.4)ICJUMP=NZSUR(IZONE)+6
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &  ' GOPQIN: Save option does not allow access')
        call edisp(iuout,' to zone control point temperature.')
      ENDIF
      RETURN
      END

C ******************** GTRNIN ********************

C GTRNIN is a low level routine to read inside trn convection
C from the results library for a given day and zone for ISAVE=2 - 4.
C Given IDAY, IZONE returns values for each timestep in common GVAL.
C NOTE: icjump nzsur+7
      SUBROUTINE GTRNIN(IDAY,IZONE,ISET)
#include "building.h"
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      IF(ISAVE.GE.2.AND.ISAVE.LE.4)THEN
        IRJUMP = 1
        IF(ISAVE.GE.2.AND.ISAVE.LE.4)ICJUMP=NZSUR(IZONE)+7
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &  ' GTRNIN: Save option does not allow access')
        call edisp(iuout,' to zone control point temperature.')
      ENDIF
      RETURN
      END

C ******************** GTMS ********************
C GTMS is a low level routine to read inside surface temperature
C from the results library for a given day and zone and surface.
C Given IDAY, IZONE, ISFN  returns values at each timestep in
C common GVAL.
C CALLS: GFIXED

      SUBROUTINE GTMS(IDAY,IZONE,ISFN,ISET)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

C Get surface temperature only is save is level 2 - 4.
      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 0
        ICJUMP = ISFN+2
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &  ' GTMS: Save option does not allow access')
        call edisp(iuout,' to inside surface temperatures.')
      ENDIF

      RETURN
      END

C ******************** GQV1 ********************
C GQV1 is a low level routine to read zone infiltration
C from the results library for a given day and zone, ISAVE 2 - 4 only.
C Given IDAY, IZONE, ISFN  returns values at each timestep in common GVAL.
C CALLS: GFIXED
C NOTE: icjump nzsur+3

      SUBROUTINE GQV1(IDAY,IZONE,ISET)
#include "building.h"
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 0
        ICJUMP = NZSUR(IZONE)+3
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &  ' GQV1: Save option does not allow access')
        call edisp(iuout,' to zone infiltration.')
      ENDIF
      RETURN
      END

C ******************** GQV2 ********************
C GQV2 is a low level routine to read interzone ventilation
C from the results library for a given day and zone, ISAVE 2 - 4 only.
C Given IDAY, IZONE, ISFN  returns values at each timestep in common GVAL.
C CALLS: GFIXED
C NOTE: icjump nzsur+4

      SUBROUTINE GQV2(IDAY,IZONE,ISET)
#include "building.h"
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 0
        ICJUMP = NZSUR(IZONE)+4
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &  ' GQV2: Save option does not allow access')
        call edisp(iuout,' to interzone ventilation.')
      ENDIF
      RETURN
      END

C ******************** GTMC ********************
C GTMC is a low level routine to read multilayer construction node
C temperature from the results library for a given day, zone, surface,
C and node number. Given IDAY, IZONE, ISFN, ISNN returns values
C at each time-step in common GVAL.
C CALLS: GFIXED

      SUBROUTINE GTMC(IDAY,IZONE,ISFN,ISNN,ISET)
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/RECV3/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)

C Get MLC node temperature only is save is level 3 (not 4).
      IF(ISAVE.EQ.3)THEN
        IRJUMP = 3+(ISFN-1)
        ICJUMP = ISNN
        NNODE=2*NELTS(IZONE,ISFN)
        IF(ICJUMP.LE.NNODE)CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &  ' GTMC: Save option does not allow access')
        call edisp(iuout,' to TMC node temperatures.')
      ENDIF
      RETURN
      END

C ******************** GTMSO ********************
C GTMSO is a low level routine to read `other side' surface
C temperature from the results library for a given day, zone
C and surface. Given IDAY, IZONE, ISFN it returns values at
C each time-step in common GVAL.
C CALLS: MOREC1 MOREC2

      SUBROUTINE GTMSO(IDAY,IZONE,ISFN,ISET)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

C Get MLC node temperature only if save is level 3 or 4.
      IF(ISAVE.EQ.3.OR.ISAVE.EQ.4)THEN
        IRJUMP = 3+(ISFN-1)
        ICJUMP = 1
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &  ' GTMSO: Save option does not allow access')
        call edisp(iuout,' to other side surface temperatures.')
      ENDIF
      RETURN
      END

C ******************** GQSI ********************
C GQSI is a low level routine to read values of QSI (the
C solar absorbed at an inside surface) from the results
C library for a given day and zone for ISAVE = 2 - 4.
C Given IDAY, IZONE, ISFN returns QSI in common GVAL.
C CALLS: MOREC1 MOREC2

      SUBROUTINE GQSI(IDAY,IZONE,ISFN,ISET)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

C Get QSI only if save is level 2 - 4.
      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 1
        ICJUMP = ISFN+5
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &  ' GQSI: Save option does not allow access to QSI.')
      ENDIF
      RETURN
      END

C ******************** GSOLEX ********************
C GSOLEX is a low level routine to read values of solar radiation
C entering from outside (W) (slot used to be used for
C external default window conduction) from the results
C library for a given day and zone, ISAVE 2 - 4 only.
C Given IDAY, IZONE returns Q1OUTS at each time step in common GVAL.
C CALLS: GFIXED

      SUBROUTINE GSOLX(IDAY,IZONE,ISET)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 1
        ICJUMP = 1
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &  ' GSOLX: Save option does not allow access to Q1OUTS.')
      ENDIF
      RETURN
      END

C ******************** GSOLADJ ********************

C GSOLADJ is a low level routine to read values of Q1ADJ the
C solar radiation (W) from adjacent zones. (Slot used to be used
C for internal default window conduction) from the results
C library for a given day and zone, ISAVE 2 - 4 only.
C Given IDAY, IZONE returns Q1ADJ at each time step in common GVAL.
C CALLS: GFIXED

      SUBROUTINE GSOLADJ(IDAY,IZONE,ISET)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 1
        ICJUMP = 2
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
         call edisp(iuout,
     &   ' GSOLADJ: Save option does not allow access to Q1ADJ.')
      ENDIF
      RETURN
      END

C ******************** GSURIC ********************
C GSURIC recover total inside surface convection for all surfaces
C for a given day and zone. If ISAVE 2 or 3 and version 2 of library
C or ISAVE = 4.
C Given IDAY, IZONE returns GSURIC at each time step in common XDUM.
C CALLS:  CSCONV
      SUBROUTINE GSURIC(IDAY,IZONE,ISET)
#include "building.h"
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)
      common/recver/izver,ipver,iever
      DIMENSION CQ(MTS)

C Only if save is level 2 - 4.
      CALL CHKTIME(IDAY,ISTART,IEND)
      NN=NZSUR(IZONE)
      do 31 JTS=ISTART,IEND
        XDUM(JTS)=0.0
        CQ(JTS)=0.0
 31   continue
      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN

C Surface convection. If version 2 of library then can recover directly
C from records.
        if(izver.ge.2)then

C Store ext opq in a temporary array. 
          CALL GOUTOPQ(IDAY,IZONE,ISET)
          DO 20 I=ISTART,IEND
            XDUM(I)=GVAL(I)
  20      continue

C Add int opq to temporary array. 
          CALL GOPQIN(IDAY,IZONE,ISET)
          DO 21 I=ISTART,IEND
            XDUM(I)=XDUM(I)+GVAL(I)
  21      continue

C Add ext trn to temporary array.
          CALL GOUTTRN(IDAY,IZONE,ISET)
          DO 22 I=ISTART,IEND
            XDUM(I)=XDUM(I)+GVAL(I)
  22      continue

C Add int trn to temporary array.
          CALL GTRNIN(IDAY,IZONE,ISET)
          DO 23 I=ISTART,IEND
            XDUM(I)=XDUM(I)+GVAL(I)
  23      continue
        else
          call edisp(iuout,'GSURIC: Save option or version of library')
          call edisp(iuout,'does not allow recovery of convection.')
        endif
      elseif(ISAVE.eq.4)then
        DO 52 ISN=1,NN
          CALL CSCONV(IDAY,IZONE,ISN,CQ)
          do 32 JTS=ISTART,IEND
            XDUM(JTS)=XDUM(JTS)+CQ(JTS)*(-1.0)
  32      continue
 52     continue
      else
        call edisp(iuout,
     &  ' GSURIC: Save option does not allow recovery of convection.')
      endif
      return
      END

C ******************** GILLUM ********************
C GILLUM recover external lux level from solar data as in INTLUM.
C Given IDAY, returns sky lux level at each time step in common XDUM.
C CALLS: 
      SUBROUTINE GILLUM(IDAY)
#include "building.h"
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/AVRAGE/IDAVER
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)
      COMMON/PREC8/SLAT,SLON

C Start definition of Perez model coefficients
C (Solar Energy Vol. 44. No. 5. pp. 271-289, 1990):
      DIMENSION  ASUN(8),ASKY(8),CSUN(8),CSKY(8),DSUN(8),DSKY(8)
      logical closea

C   Coefficients:
      DATA ASUN/57.20,98.99,109.83,110.34,106.36,107.19,105.75,101.18/
      DATA ASKY/97.24,107.22,104.97,102.39,100.71,106.42,141.88,152.23/
      DATA CSUN/-2.98,-1.21,-1.71,-1.99,-1.75,-1.51,-1.26,-1.10/
      DATA CSKY/12.00,0.59,-5.53,-13.95,-22.75,-36.15,-53.24,-45.27/
      DATA DSUN/117.12,12.38,-8.81,-4.56,-6.16,-26.73,-34.44,-8.29/
      DATA DSKY/-8.91,-3.95,-8.77,-13.90,-23.74,-28.83,-14.03,-7.98/
C END Perez definitions.

C Any save level.
      CALL CHKTIME(IDAY,ISTART,IEND)
      do 31 JTS=ISTART,IEND
        XDUM(JTS)=0.0
 31   continue

C Get direct (VAL3(3,?) and diffuse (VAL3(1,?) solar.
      CALL MOCLIM (IDAY,24)
      CALL MOCLIM (IDAY,25)

      PI = 4.0 * ATAN(1.0)
      rad=PI/180.
      do 32 JTS=ISTART,IEND

C Find altitude and azimuth (note calculate each timestep
C the data will be "display averaged" elsewhere).
        ADJUST=0.
        IF(IDAVER.EQ.0)ADJUST=1.0/(FLOAT(NTS)*2.0)
        HOUR=FLOAT(JTS)/FLOAT(NTS)
        HOUR=HOUR-ADJUST
        DAY=FLOAT(IDAY)

C Calculate equation of time.
        A=1.978*DAY-160.22
        B=0.989*DAY-80.11
        EQT=0.1645*ESIND(A)-0.1255*ECOSD(B)-0.025*ESIND(B)

C Compute declination.
        A=280.1+0.9863*DAY
        DEC=23.45*ESIND(A)
        SDEC=SIN(DEC*rad)
        CDEC=COS(DEC*rad)

C Compute solar altitude.
        TIME=HOUR+(EQT+SLON/15.)
        TIMCOE=15.*(12.-TIME)
        CDTIME=COS(TIMCOE*rad)
        SSLAT=SIN(SLAT*rad)
        CSLAT=COS(SLAT*rad)
        SALT=ASIN(SSLAT*SDEC+CSLAT*CDEC*CDTIME)/rad
        if(SALT.LT.0.)then

C Sun below horizon, return 0. illum.
          XDUM(JTS)=0.
        else

C Calculate sky condition from the ratio of horizontal sky irradiance
C to global horizontal irradiance.
          QDIF=VAL3(1,JTS)
          QDIR=VAL3(3,JTS)
          dir=QDIR*sin(SALT*rad)

C Average sky.  Note that climatic data and sun position may
C be slightly incompatable ie. diffuse radiation available
C after the sun has set.  In this case 'ratio' may be extreme
C so set suneff=0 and skyeff=0.
          call eclose(QDIF,0.00,0.01,closea)
          if(closea.OR.(SALT.LT.0.))then
            suneff=0.
            skyeff=0.
          else

C Calculate efficacy of direct sun and sky.
            ZENN=(90.-SALT)*rad
            GONN=1367.*(1.+0.033*COS(0.017214*IDAY))
            CONN1=1./0.001572
            CONN2=(COS(ZENN))**2+0.0031465
            OPTM=CONN1*(SQRT(CONN2)-COS(ZENN))
            DELTT=QDIF*OPTM/GONN
            EPSS=((QDIF+QDIR)/QDIF+1.041*ZENN**3)/(1.+1.041*ZENN**3)
            IF((EPSS.GE.1.).AND.(EPSS.LT.1.065))THEN
              I=1
            ELSEIF((EPSS.GE.1.065).AND.(EPSS.LT.1.23))THEN
              I=2
            ELSEIF((EPSS.GE.1.23).AND.(EPSS.LT.1.50))THEN
              I=3
            ELSEIF((EPSS.GE.1.50).AND.(EPSS.LT.1.95))THEN
              I=4
            ELSEIF((EPSS.GE.1.95).AND.(EPSS.LT.2.80))THEN
              I=5
            ELSEIF((EPSS.GE.2.80).AND.(EPSS.LT.4.50))THEN
              I=6
            ELSEIF((EPSS.GE.4.50).AND.(EPSS.LT.6.20))THEN
              I=7
            ELSEIF(EPSS.GE.6.20)THEN 
              I=8
            ENDIF

            call eclose(QDIR,0.00,0.01,closea)
            IF(closea)THEN
              suneff=0.
              skyeff=(ASKY(I)+CSKY(I)*COS(ZENN)+DSKY(I)*LOG(DELTT))
            ELSE
              suneff=(ASUN(I)+CSUN(I)*EXP(5.73*ZENN-5.)+DSUN(I)*DELTT)
              skyeff=(ASKY(I)+CSKY(I)*COS(ZENN)+DSKY(I)*LOG(DELTT))
            ENDIF
          endif

C Calculate source illuminance. NOTE: only return sky portion at
C this time. 
          esun=dir*suneff
          esky=QDIF*skyeff
C          XDUM(JTS)=esun+esky
          XDUM(JTS)=esky
        endif
  32  continue
      return
      END

C ******************** GPCR1 ********************
C GPCR1 is a low level routine to read values of QPCR1 (the
C zone real power consumption) from the results
C library for a given day and zone, ISAVE 2 - 4 only.
C Given IDAY, IZONE returns QPCR1 at each time step in common GVAL.

      SUBROUTINE GPCR1(IDAY,IZONE,ISET)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 1
        ICJUMP = 3
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
         call edisp(iuout,
     &   ' GPCR1: Save option does not allow access to  PCR1.')
      ENDIF
      RETURN
      END

C ******************** GPCQ2 ********************
C GPCQ2 is a low level routine to read values of PCQ2 (the
C zone reactive power consumption) from the results
C library for a given day and zone, ISAVE 2 - 4 only.
C Given IDAY, IZONE returns QPCR22 at each time step in common GVAL.
C CALLS: GFIXED

      SUBROUTINE GPCQ2(IDAY,IZONE,ISET)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 1
        ICJUMP = 4
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
         call edisp(iuout,
     &   ' GPCQ2: Save option does not allow access to PCQ2.')
      ENDIF
      RETURN
      END

C ******************** GSOLABS ********************
C GSOLABS is a low level routine to read Q2ABS the total
C solar absorbed within a zone (W). (This slot used to be used
C for the def windows solar air point)
C from the results library for a given day and zone, ISAVE 2 - 4 only.
C Given IDAY, IZONE returns Q2ABS at each time step in common GVAL.
C CALLS: GFIXED

      SUBROUTINE GSOLABS(IDAY,IZONE,ISET)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 1
        ICJUMP = 5
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
         call edisp(iuout,
     &   ' GSOLABS: Save option does not allow access to Q2ABS.')
      ENDIF
      RETURN
      END

C ******************** GQSE ********************
C GQSE is a low level routine to read values of QSE (the
C solar absorbed at 'outside' surface) from results
C library for a given day and zone.
C Given IDAY, IZONE returns QSE at each time step in common GVAL.
C CALLS: GFIXED

      SUBROUTINE GQSE(IDAY,IZONE,ISFN,ISET)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

C Get QSE only if save is level 2 -4.
      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 2
        ICJUMP=ISFN
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
         call edisp(iuout,
     &   ' GQSE: Save option does not allow access to QSE.')
      ENDIF
      RETURN
      END

C ******************** GTBR ********************
C GTBR reads flux associated with thermal bridges in a zone
C from results library for a given day and zone.
C Given IDAY, IZONE returns value at each time step in common GVAL.
C CALLS: GFIXED
C NOTE: icjump nzsur+12
      SUBROUTINE GTBR(IDAY,IZONE,ISET)
#include "building.h"
#include "geometry.h"
      common/recver/izver,ipver,iever
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

C Get thermal bridge data only if save is level 2 -4.
      if(izver.lt.4)then
        call edisp(iuout,
     &   'GTBR: Library version does not include thermal bridge.')
        return
      endif
      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 1
        ICJUMP=NZSUR(IZONE)+12
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
         call edisp(iuout,
     &   'GTBR: Save option does not allow access to thermal bridge.')
      ENDIF
      RETURN
      END

C ******************** GAIRSTOR ********************
C GAIRSTOR reads heat stored in the zone air node (due to temp fluxuations)
C from results library for a given day and zone.
C Given IDAY, IZONE returns value at each time step in common GVAL.
C CALLS: GFIXED
C NOTE: icjump nzsur+12
      SUBROUTINE GAIRSTOR(IDAY,IZONE,ISET)
#include "building.h"
#include "geometry.h"
      common/recver/izver,ipver,iever
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

C Get heat stored in the zone air only if save is level 2 -4.
      if(izver.lt.4)then
        call edisp(iuout,
     &   'GAIRSTOR: Library version does not include air storage.')
        return
      endif
      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        IRJUMP = 0
        ICJUMP=NZSUR(IZONE)+12
        CALL GFIXED(IDAY,IZONE,ISET,IRJUMP,ICJUMP)
      ELSE
        call edisp(iuout,
     &   'GAIRSTOR: Save option does not allow access to air storage.')
      ENDIF
      RETURN
      END

C ******************** GZTMS ********************
C GZTMS is a low level routine to read from the results library ZTMS
C (the temperatures at the inside & outside face of all surfaces
C at a  particular time step) along with air node, control point
C window conductions, air point solar and zone RH. Given IDAY, IZONE
C & ITS it returns ZTMS(MS), ZTMSO(MS), TMA,TMCON,QW1,QW2,QSA,RH.
C CALLS: MOREC1 MOREC2

      SUBROUTINE GZTMS(IDAY,IZ,ISET,ITS,ZTMS,ZTMSO,TMA,TMCON,QW1,QW2,
     &                                                          QSA,RH)
#include "building.h"
#include "geometry.h"
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/HASH/IHASHT(MTS),IHASHZ(MCOM),IHASHD(365),IHASHS(MNRS)

      character outs*124

      DIMENSION XD(MZS),ZTMS(MS),ZTMSO(MS)

      IUNIT=IFIL
      if(iz.eq.0.or.iday.eq.0.or.iset.eq.0.or.its.eq.0)then
        call edisp(iuout,'Error: One of the GZTMS parameters is zero.')
        write(outs,'(a,i3,a,i2,a,i2)') 'iday= ',iday,' zone= ',iz,
     &    ' set= ',iset
        call edisp(iuout,outs)
        return
       endif

C Get ZTMS only if save is level 2 - 4.
      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        IREC=IHASHD(IDAY)+IHASHT(ITS)+IHASHZ(IZ)+IHASHS(ISET)
        if(irec.eq.0) goto 999
        NN=NZSUR(IZ)
        READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)
     &            TMA,X1,(ZTMS(K),K=1,NN),X2,X3,TMCON
        IREC=IREC+1
        READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)QW1,QW2,X1,X2,QSA
        IREC=IREC+1
        READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)(XD(I),I=1,NN),
     &    X1,X2,RH
        IREC=IREC+1

C Read first node temp (outside surface) for each surf in zone
C if save level is 3 or 4, otherwise set ZTMSO to 0.
        DO 80 K=1,NN
          if(ISAVE.GT.2.AND.ISAVE.LE.4)THEN
            READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)ZTMSO(K)
            IREC=IREC+1
          else
            ZTMSO(K)=0.0
          endif
   80   CONTINUE
      ELSE
         call edisp(iuout,
     &   ' GZTMS: Save option does not allow access to ZTMS.')
      ENDIF

    8 RETURN
 1000 if(IOS.eq.2)then
        write(outs,'(a,i5,a)')
     &    ' GZTMS: no permission to read ZTMS at record',IREC,'.'
      else
        write(outs,'(a,i5,a)')
     &    ' GZTMS: error reading ZTMS at record',IREC,'.'
      endif
      call edisp(iuout,outs)
      CALL EPWAIT
      goto 8

 999  write(outs,'(a,4i6)') 
     &  'GZTMS: record out of range (day? zone?) scanning xd @ rec ',
     &  IREC,IDAY,IZ,ISET
      call edisp(iuout,outs)
      CALL EPWAIT
      goto 8

      END


C ******************** GZSOLA ********************
C GZSOLA reads from the results library QI(MS), QE(MS)
C (the solar absorption at inside and outside face of all surfaces
C at a  particular time step) along with air point solar and total
C solar absorbed at inside and outside.
C Given IDAY, IZONE & ITS it returns QI(MS),QE(MS),QSA,TQI,TQE.
C CALLS: MOREC1 MOREC2

      SUBROUTINE GZSOLA(IDAY,IZ,ITS,QI,QE,QSA,TQI,TQE)
#include "building.h"
#include "geometry.h"
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/RECV5/SNA(MCOM,MS)
      COMMON/HASH/IHASHT(MTS),IHASHZ(MCOM),IHASHD(365),IHASHS(MNRS)

      character outs*124

      DIMENSION QI(MS),QE(MS)

      IUNIT=IFIL

C Only if save is level 2 - 4.
      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        IREC=IHASHD(IDAY)+IHASHT(ITS)+IHASHZ(IZ)
        if(irec.eq.0) goto 999
        NN=NZSUR(IZ)
        IREC=IREC+1
        READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)QW1,QW2,X1,X2,QSA,
     &    (QI(K),K=1,NN)
        IREC=IREC+1
        READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)(QE(I),I=1,NN)

        TQE=0.
        TQI=0.
        DO 10 I=1,NN
          TQE=TQE+QE(I)*SNA(IZ,I)
          TQI=TQI+QI(I)*SNA(IZ,I)
   10   CONTINUE
      ELSE
         call edisp(iuout,
     &   ' GZSOLA: Save option does not allow recovery.')
      ENDIF

    8 RETURN
 1000 if(IOS.eq.2)then
        write(outs,'(a,i5,a)')
     &    ' GZSOLA: no permission to read data at record',IREC,'.'
      else
        write(outs,'(a,i5,a)')
     &    ' GZSOLA: error reading data at record',IREC,'.'
      endif
      call edisp(iuout,outs)
      CALL EPWAIT
      goto 8

 999  write(outs,'(a,4i6)') 
     &  'GZSOLA: record out of range (day? zone?) scanning xd @ rec ',
     &  IREC,IDAY,IZ,ITS
      call edisp(iuout,outs)
      CALL EPWAIT
      goto 8

      END


C ******************** GATMC ********************
C GATMC is a low level routine to read from the results library
C ZTMC (the node temperatures within a surface at a particular
C time step).  Given IDAY, IZONE, ISFN & ITS it returns ATMC(ME).

      SUBROUTINE GATMC(IDAY,IZONE,ISFN,ITS,ATMC)
#include "building.h"
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/RECV3/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/HASH/IHASHT(MTS),IHASHZ(MCOM),IHASHD(365),IHASHS(MNRS)

      character outs*124

      DIMENSION ATMC(MN)

      IUNIT=IFIL

C Get ATMC only if save is level 3. Jump to start of node
C temperatures, clear atmc and read node data.
      IF(ISAVE.eq.3)THEN
        do 10 k=1,MN
  10    atmc(k)=0.0
        IRJUMP = 3+(ISFN-1)
        IREC=IHASHD(IDAY)+IHASHT(ITS)+IHASHZ(IZONE)+IRJUMP
        NN=2*NELTS(IZONE,ISFN)
        READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)(ATMC(K),K=1,NN)
      ELSE
         call edisp(iuout,
     &   ' GATMC: Save option does not allow access to ATMC.')
      ENDIF

    8 RETURN
 1000 if(IOS.eq.2)then
        write(outs,'(a,i5,a)')
     &  'GATMC: no permission to read ATMC (all node temp) at record',
     &   IREC,'.'
      else
        write(outs,'(a,i5,a)')
     &  'GATMC: error reading ATMC (all node temp) at record',IREC,'.'
      endif
      call edisp(iuout,outs)
      CALL EPWAIT
      goto 8

      END

C ******************** GSURFB ********************
C GSURFB is a low level routine to read from locations of the save 3
C multilayer construction node temperature locations data for
C surface balances under save option 4. Given day (IDAY),
C zone (IZONE), surface (ISFN), and timestep (IT) returns surface
C balance data:
C tsout  = outside surface temperature (same location as ISAVE=3),
C qswrdi = short wave radiation absorbed inside,
C qlwrdi = long wave radiation flux, qconvi = inside surface convection,
C qcondi = inside surface conduction, qstori = heat stored @ inside
C          face in a layer 1/4 thickness of innermost element,
C qstore heat storage @ other side, qconde = other side conduction,
C qconve = otherside convection, qlwrde = outside longwave to ground,
C qlwrbd = outside longwave to buildings,
C qlwrsk = outside longwave to sky, qswrde = outside shortwave radiation

C For izver >=4 the following are explicitly recovered:
C avqcrioc = occupant radiant casual gain absorbed at surface
C avqcrili = lighting radiant casual gain absorbed at surface
C avqcrisp = small power radiant casual gain absorbed at surface
C avqcrioth = other radiant casual gain absorbed at surface
      SUBROUTINE GSURFB(IDAY,IZONE,ISFN,ITS,TSout,qswrdi,qlwrdi,qconvi,
     &  qcondi,qstori,qstore,qconde,qconve,qlwrde,qlwrbd,qlwrsk,
     &  qswrde,avqcrioc,avqcrili,avqcrisp,avqcrioth)
#include "building.h"
#include "geometry.h"
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/HASH/IHASHT(MTS),IHASHZ(MCOM),IHASHD(365),IHASHS(MNRS)
      common/recver/izver,ipver,iever

      character outs*124

      IUNIT=IFIL

C Get only if save is level 4, then compute start record address.
      IF(ISAVE.EQ.4)THEN
        IREC=IHASHD(IDAY)+IHASHT(ITS)+IHASHZ(IZONE)+3+(ISFN-1)
        NN=NZSUR(IZONE)
        if(izver.lt.4)then
          READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)TSout,qswrdi,
     &          qlwrdi,qconvi,qcondi,qstori,qstore,qconde,qconve,
     &          qlwrde,qlwrbd,qlwrsk,qswrde
          avqcrioc=0.0
          avqcrili=0.0
          avqcrisp=0.0
          avqcrioth=0.0
        else
          READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)TSout,qswrdi,
     &          qlwrdi,qconvi,qcondi,qstori,qstore,qconde,qconve,
     &          qlwrde,qlwrbd,qlwrsk,qswrde,avqcrioc,avqcrili,
     &          avqcrisp,avqcrioth
        endif

C Debug.
C        write(6,'(a,4i3,17f8.2)') 'GSURFB data at ',
C     &    IDAY,IZONE,ISFN,ITS,TSout,qswrdi,
C     &    qlwrdi,qconvi,qcondi,qstori,qstore,qconde,qconve,
C     &    qlwrde,qlwrbd,qlwrsk,qswrde,avqcrioc,avqcrili,
C     &    avqcrisp,avqcrioth

      ELSE
         call edisp(iuout,
     &   ' GSURFB: Save option does not allow access')
         call edisp(iuout,' to surface energy balance data.')
      ENDIF

    8 RETURN
 1000 if(IOS.eq.2)then
        write(outs,'(a,i3,a,i5)')
     &    'GSURFB: no permission to read surface ',
     &    ISFN,' balance data at record',IREC
      else
        write(outs,'(a,i3,a,i5)')'GSURFB: error reading surface ',
     &    ISFN,' balance data at record',IREC
      endif
      call edisp(iuout,outs)
      CALL EPWAIT
      goto 8

      END

C ******************** G4ALLS ********************
C G4ALLS is a low level routine to read one timestep's data for
C all surfaces in a zone for save option 4.
C Given day (IDAY), zone (IZONE), and timestep (IT) and IGET
C where IGET is as follows:
C 1 = outside surface temperatures, 2 = short wave rad,
C 3 = longwave radiation, 4 = inside surface convection,
C 5 = inside surface conduction, 6 = inside heat storage,
C 7 = outside heat storage, 8 = outside surface conduction,
C 9 = outside convection, 10 = long wave > buildings,
C 11 = long wave > sky, 12 = long wave > ground,
C 13 = shortwave radiation.
C returns FLUX(MS).
C CALLS: MOREC1 MOREC2

      SUBROUTINE G4ALLS(IDAY,IZONE,ITS,IGET,FLUX)
#include "building.h"
#include "geometry.h"
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/HASH/IHASHT(MTS),IHASHZ(MCOM),IHASHD(365),IHASHS(MNRS)
      DIMENSION FLUX(MS),XD(MZS)

      character outs*124

      IUNIT=IFIL

C Get only if save is level 4, then compute start record address.
      IF(ISAVE.EQ.4.AND.IGET.LE.13)THEN
        IREC=IHASHD(IDAY)+IHASHT(ITS)+IHASHZ(IZONE)+3
        NN=NZSUR(IZONE)
        DO 40 ISN=1,NN
          READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=100)(XD(JX),JX=1,IGET)
          FLUX(ISN)=XD(IGET)
          IREC=IREC+1
   40   CONTINUE

      ELSE
         call edisp(iuout,
     &   ' G4ALLS: Save option or item requested error')
         call edisp(iuout,' for surface energy balance data.')
        GOTO 8
      ENDIF

    8 RETURN
  100 if(IOS.eq.2)then
        write(outs,'(a,i5,a)')
     &    'G4ALLSG: no permission to read surface balance at record',
     &    IREC,'.'
      else
        write(outs,'(a,i5,a)')
     &    'G4ALLS: error reading surface balance at record',IREC,'.'
      endif
      call edisp(iuout,outs)
      CALL EPWAIT
      goto 8

      END

C ******************** G4FLUX ********************
C G4FLUX is a low level routine to read a selected item from the
C surface balance data (save option 4)
C from the results library for a given day, zone & surface.
C Given IDAY, IZONE, ISFN, IGET returns values in FLUX(MTS).
C IGET is as follwos:
C 1 = outside surface temperatures, 2 = short wave rad,
C 3 = longwave radiation, 4 = inside surface convection,
C 5 = inside surface conduction, 6 = inside heat storage,
C 7 = outside heat storage, 8 = outside surface conduction,
C 9 = outside convection, 10 = long wave > buildings,
C 11 = long wave > sky, 12 = long wave > ground,
C 13 = shortwave radiation.
C CALLS: MOREC1 MOREC2

      SUBROUTINE G4FLUX(IDAY,IZONE,ISFN,IGET,FLUX)
#include "building.h"
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/HASH/IHASHT(MTS),IHASHZ(MCOM),IHASHD(365),IHASHS(MNRS)

      character outs*124

      DIMENSION XD(MZS), FLUX(MTS)

      IUNIT=IFIL

C Get only if save is level 4, then compute start record address for
C requested recovery day and recover values for all timesteps.
      IF(ISAVE.EQ.4.AND.IGET.LE.13)THEN
        CALL MOREC1(IDAY,IREC)
        N=24*NTS
        DO 20 I=1,N
          FLUX(I)=0.
          IREC=IHASHD(IDAY)+IHASHT(I)+IHASHZ(IZONE)+3+(ISFN-1)

          READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=99)(XD(K),K=1,IGET)
          FLUX(I)=XD(IGET)
   20   CONTINUE

      ELSE
         call edisp(iuout,
     &   ' G4FLUX: Save option or IGET does not allow access')
         call edisp(iuout,' to surface energy balance data.')
      ENDIF

    8 RETURN
   99 if(IOS.eq.2)then
        write(outs,'(a,i3,a,i5)')
     &    'G4FLUX: no permission to read surface ',
     &    ISFN,' temerature at record',IREC
      else
        write(outs,'(a,i3,a,i5)')'G4FLUX: error reading surface ',
     &    ISFN,' temperature at record',IREC
      endif
      call edisp(iuout,outs)
      CALL EPWAIT
      goto 8

      END

C ******************** getallcas ********************
C Getallcas is a low level routine to read one timestep of the
C total conv/radiant and separate casual gain conv/rad/latent
C data for a zone and control fraction.
C Given IDAY, IZONE & ITS it returns:
C QCASR the total radiant of all gains
C QCASC the total convective of all gains
C QCASL the total latent of all gains
C FRAC the fraction controlled (0.0-1.0)
C perocupc,perlightc,perequipc,perotherc: sensible con for occup lights equip other.
C perocupr,perlightr,perequipr,perotherr: sensible rad for occup lights equip other.
C perocupl,perlightl,perequipl,perotherl: latent for occup lights equip other.
C theonectld (integer index) if non-zero the index of the gain to control.

      SUBROUTINE getallcas(IDAY,IZ,ISET,ITS,QCASR,QCASC,QCASL,FRAC,
     & perocupc,perlightc,perequipc,perotherc,perocupr,perlightr,
     & perequipr,perotherr,perocupl,perlightl,perequipl,perotherl,
     & theonectld)
#include "building.h"
#include "geometry.h"
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/HASH/IHASHT(MTS),IHASHZ(MCOM),IHASHD(365),IHASHS(MNRS)

      character outs*124
      real QCASR,QCASC,QCASL  ! total radiant/convective/latent
      real FRAC ! controlled fraction.
      real perocupc,perocupr,perocupl ! average occupant to write out
      real perlightc,perlightr,perlightl ! average lighting to write out
      real perequipc,perequipr,perequipl ! average equipment to write out
      real perotherc,perotherr,perotherl ! average other to write out
      integer theonectld  ! if non-zero the casual gain type that is controlled.
      real X1,X2,X3,X4,X5
      DIMENSION XD(MS+7) ! for scanning past unneeded data

      IUNIT=IFIL
      if(iz.eq.0.or.iday.eq.0.or.iset.eq.0.or.its.eq.0)then
        call edisp(iuout,
     &    'Error: One of the getallcas parameters is zero.')
        write(outs,'(a,i3,a,i2,a,i2)') 'iday= ',iday,' zone= ',iz,
     &    ' set= ',iset
        call edisp(iuout,outs)
        return
      endif

C Get ZTMS only if save is level 2 - 4.
      IF(ISAVE.GT.1.AND.ISAVE.LE.4)THEN
        IREC=IHASHD(IDAY)+IHASHT(ITS)+IHASHZ(IZ)+IHASHS(ISET)
        NN=NZSUR(IZ)
        NA=NZSUR(IZ)+7
        READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000) (XD(I),I=1,NA),
     &    perocupc,perlightc,perequipc,perotherc     
        IREC=IREC+1
        READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000) (XD(I),I=1,NA),
     &    perocupr,perlightr,perequipr,perotherr
        IREC=IREC+1
        READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000) (XD(I),I=1,NN),
     &    QCASR,QCASC,X1,X2,FRAC,X3,X4,perocupl,perlightl,
     &    perequipl,perotherl,theonectld

C Make up QCASL from the separate latent gains.
        QCASL=perocupl+perlightl+perequipl+perotherl

C Dummy use of the temporary variables.
        X5=X1+X2+X3+X4+XD(1)

C Debug.
C        write(6,'(a,4i3,16F6.1,i3)') 'getallcas: ',IDAY,IZ,ISET,ITS,
C     &    QCASR,QCASC,QCASL,
C     &    FRAC,perocupc,perlightc,perequipc,perotherc,
C     &    perocupr,perlightr,perequipr,perotherr,
C     &    perocupl,perlightl,perequipl,perotherl,theonectld

      ELSE
         call edisp(iuout,
     & 'getallcas: Save option does not allow access to casual gains.')
      ENDIF

    8 RETURN
 1000 if(IOS.eq.2)then
        write(outs,'(a,i5,a)')
     &    'getallcas: no permission to read casual gains at record',
     &     IREC,'.'
      else
        write(outs,'(a,i5,a)')
     &    'getallcas: error reading casual gains at record',IREC,'.'
      endif
      call edisp(iuout,outs)
      CALL EPWAIT
      goto 8

      END

C ******************** GFIXED ********************
C GFIXED is a low level routine to read single fixed items
C from the results library for a given day and zone. Used
C for items in save levels 1 - 4.
C Given IDAY, IZONE returns days values in the common array GVAL.
C IRJ is record to jump to (passed by higher level).
C ICJ is column to jump to (passed by higher level).
C ISET is the results set number to be scanned.

      SUBROUTINE GFIXED(IDAY,IZONE,ISET,IRJ,ICJ)
#include "building.h"
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/HASH/IHASHT(MTS),IHASHZ(MCOM),IHASHD(365),IHASHS(MNRS)
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

      character outs*124

      DIMENSION XD(MZS+12)

      IUNIT=IFIL

C Get data only if save is level 1 - 4.
      IF(ISAVE.GE.1.AND.ISAVE.LE.4)THEN
        if(iset.gt.0)then
          N=24*NTS
          DO 20 I=1,N
            GVAL(I)=0.

C Return the record number using hash.
            IREC=IHASHD(IDAY)+IHASHT(I)+IHASHZ(IZONE)+IHASHS(ISET)+IRJ
            if(irec.eq.0) goto 999
            READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=99)(XD(K),K=1,ICJ)
            GVAL(I)=XD(ICJ)
   20     CONTINUE

C Debug.
C         write(6,*) iset,izone,irj,icj,irec,(GVAL(I),i=1,12)

        else
          call edisp(iuout,'GFIXED: results set number is zero. Skip.')
        endif
      ELSE
        call edisp(iuout,'GFIXED: Save option does not allow access.')
      ENDIF

    8 RETURN
   99 if(IOS.eq.2)then
        write(outs,'(a,i5,a)')
     &    ' GFIXED: no permission to read at record',IREC,'.'
      else
        write(outs,'(a,i5,a)')
     &    ' GFIXED: error reading at record',IREC,'.'
      endif
      call edisp(iuout,outs)
      CALL EPWAIT
      goto 8

 999  write(outs,'(a,4i6)') 
     &  'GFIXED: record out of range (day? zone?) scanning xd @ rec ',
     &  IREC,IDAY,IZONE,ISET
      call edisp(iuout,outs)
      CALL EPWAIT
      goto 8

      END


C ******************** MMAPER ********************
C MMAPER is a low level routine to scan the common results records for save
C levels 2, 3 and 4 finding the max, min and average values and times of 
C max and min occurances as offsets from the output period start day in 
C hours.

C The data structure e.g.for RDMAX holds the peak value, RDMIN the minimum
C value and RDAVE the average value and rdt ax & RDTMIN are the times of
C occurrance for the max and min.  The data structure is an array of three
C where the first is by zone, the 2nd is for three records associated with
C zone information and the 3rd dimension is the width of the record.

      SUBROUTINE MMAPER
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/HASH/IHASHT(MTS),IHASHZ(MCOM),IHASHD(365),IHASHS(MNRS)

C Pre calculated data (not filtered).
      common/MMADAT/RDMAX(MCOM,3,MZRL),RDMIN(MCOM,3,MZRL),
     &       RDAVE(MCOM,3,MZRL),RDTMAX(MCOM,3,MZRL),RDTMIN(MCOM,3,MZRL)

C Pre calculated data (filtered).
      common/MMAFDAT/IFILT,RDFMAX(MCOM,3,MZRL),RDFMIN(MCOM,3,MZRL),
     &    RDFAVE(MCOM,3,MZRL),RDTFMAX(MCOM,3,MZRL),RDTFMIN(MCOM,3,MZRL)

      dimension DATA(3,MZRL),XD(MZRL)
      character outs*124

C Check save level.
      if (ISAVE.lt.2.or.ISAVE.gt.4) then
        call edisp(iuout,'MMAPER: Save option does not allow access')
        call edisp(iuout,'so some graph axis will not be correct.')
        return
      endif

C Set unit number.
      IUNIT=IFIL

C Clear arrays.
      do 3 I=1,MCOM
        do 5 J=1,3
          do 7 K=1,MZRL
            RDMAX(I,J,K)=-1.E+10
            RDMIN(I,J,K)=1.E+10
            RDAVE(I,J,K)=0.
            RDTMAX(I,J,K)=0.
            RDTMIN(I,J,K)=0.
            RDFMAX(I,J,K)=-1.E+10
            RDFMIN(I,J,K)=1.E+10
            RDFAVE(I,J,K)=0.
            RDTFMAX(I,J,K)=0.
            RDTFMIN(I,J,K)=0.
 7        continue
 5      continue
 3    continue

C TSTART and TFINISH - start and finish times in hours from 0000 on the 
C first day of output.
      TSTART=FLOAT(IOH1)
      TFINSH=FLOAT(((IODF)*24+IOH2)-(IODS)*24)

C NDTS - the number of timesteps in a day.
      NDTS=24*NTS

C Loop through zones, days then minutes
C Need to loop through the number of output timesteps and average if 
C required.
      do 100 IZONE=1,NCOMP

C NZI - number of zome data items = NSUR +12. Note use NZI3
C for 3rd record in each set (two fewer data items to scan).
        NZI = NZSUR(IZONE)+12
        NZI3 = NZI - 2
        NITMS=0
        NFITMS=0
        do 200 IDAY=IODS,IODF
          do 300 ITS=1,NDTS,NOUT

C Compute current time.
C IHRD - number of days since start of plotting period in hours.
C TIME - time in hours since start of first day plotted.
C Add half a timestep to TIME if data averaged.
            IHRD=(IDAY-IODS)*24
            call DATIME(ITS,ATIME)
            TIME=float(IHRD)+ATIME

C Within requested output period.
            IF(TIME.LT.(TSTART-1.0).or.TIME.GT.TFINSH)goto 300

C Read first record and set values, make up potential error messages.
            IREC=IHASHD(IDAY)+IHASHT(ITS)+IHASHZ(IZONE)+IHASHS(ISIM)
            if(irec.eq.0) goto 999

            READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=99)(DATA(1,K),K=1,NZI)
            IREC=IREC+1
            READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=99)(DATA(2,K),K=1,NZI)
            IREC=IREC+1
            READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=99)(DATA(3,K),K=1,NZI3)

C Average over required time steps.
            if (NOUT.gt.1) then
              do 400 IV=2,NOUT
                J=ITS+IV-1
                IREC=IHASHD(IDAY)+IHASHT(J)+IHASHZ(IZONE)+IHASHS(ISIM)
                if(irec.eq.0) goto 999

                READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=98)(XD(K),K=1,NZI)
                do 500 K=1,NZI
                  DATA(1,K)=DATA(1,K)+XD(K)
 500            continue
                IREC=IREC+1
                READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=98)(XD(K),K=1,NZI)
                do 501 K=1,NZI
                  DATA(2,K)=DATA(2,K)+XD(K)
 501            continue
                IREC=IREC+1
                READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=98)(XD(K),K=1,NZI3)
                do 502 K=1,NZI3
                  DATA(3,K)=DATA(3,K)+XD(K)
 502            continue
 400          CONTINUE
            endif

C Average filtered and non filtered data and compare to 
C current max/min values.
            ih=int(ATIME+1.)
            ioc=1
            call getocup(IZONE,IDAY,ITS,ioc,ier)
            do 600 K=1,3
              do 610 KI=1,NZI
                if (NOUT.gt.1) DATA(K,KI)=DATA(K,KI)/float(NOUT)
                if(ioc.eq.1) then
                  if (DATA(K,KI).gt.RDFMAX(IZONE,K,KI)) then
                    RDFMAX(IZONE,K,KI)=DATA(K,KI)
                    RDTFMAX(IZONE,K,KI)=TIME
                  endif
                  if (DATA(K,KI).lt.RDFMIN(IZONE,K,KI)) then
                    RDFMIN(IZONE,K,KI)=DATA(K,KI)
                    RDTFMIN(IZONE,K,KI)=TIME
                  endif
                  RDFAVE(IZONE,K,KI)=RDFAVE(IZONE,K,KI)+DATA(K,KI)
                endif
                if (DATA(K,KI).gt.RDMAX(IZONE,K,KI)) then
                  RDMAX(IZONE,K,KI)=DATA(K,KI)
                  RDTMAX(IZONE,K,KI)=TIME
                endif
                if (DATA(K,KI).lt.RDMIN(IZONE,K,KI)) then
                  RDMIN(IZONE,K,KI)=DATA(K,KI)
                  RDTMIN(IZONE,K,KI)=TIME
                endif
                RDAVE(IZONE,K,KI)=RDAVE(IZONE,K,KI)+DATA(K,KI)
 610          continue
 600        continue
            NITMS=NITMS+1
            if(ioc.ne.0) NFITMS=NFITMS+1
 300      CONTINUE
 200    CONTINUE

C Calculate average values for period.
        xNITMS=float(NITMS)
        xNFITMS=float(NFITMS)
        do 700 K=1,3
          do 710 KI=1,NZI
            RDAVE(IZONE,K,KI)=RDAVE(IZONE,K,KI)/xNITMS
            if (NFITMS.gt.0) then
              RDFAVE(IZONE,K,KI)=RDFAVE(IZONE,K,KI)/xNFITMS
            endif
 710      continue
 700    continue
 100  CONTINUE

      return

  99  if(IOS.eq.2)then
        write(outs,'(a,4i6)') 
     &    'MMAPER: no permission to scan data @ rec ',
     &    IREC,NZI,IZONE,ISIM
      else
        write(outs,'(a,4i6)') 'MMAPER: scanning data @ rec ',
     &    IREC,NZI,IZONE,ISIM
      endif
      call usrmsg(outs,' cannot continue results recovery.','F')
      return

  98  if(IOS.eq.2)then
        write(outs,'(a,4i6)') 
     &    'MMAPER: no permission to scan xd @ rec ',
     &    IREC,NZI,IZONE,ISIM
      else
        write(outs,'(a,4i6)') 'MMAPER: scanning xd @ rec ',
     &    IREC,NZI,IZONE,ISIM
      endif
      call usrmsg(outs,' cannot continue results recovery.','F')
      return

 999  write(outs,'(a,4i6)') 
     &  'MMAPER: record out of range (day? zone?) scanning xd @ rec ',
     &  IREC,IDAY,IZONE,ISIM
      call usrmsg(outs,' cannot continue results recovery.','W')
      return
      end


C ******************** HASHL ********************
C HASHL is a low level routine to calculate the offset record
C (from start of day) for each zone and timestep. It takes into
C account the save level and returns the common HASH:
C IHASHD is the first results library record for a year-day.
C IHASHT is the offset from IHASHD to the record for zone 1 at each
C timestep in a typical day.
C IHASHZ is the offset from IHASHT for each zone in a simulation.
C Together the hash variables allow the location of a record for any
C given day, zone, and timestep without computational overhead.
C CALLS: MOREC1 MOREC2

      SUBROUTINE HASHL
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/SIMPKA/NSIM
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/HASH/IHASHT(MTS),IHASHZ(MCOM),IHASHD(365),IHASHS(MNRS)
      COMMON/FILEP/IFIL

      character outs*124

      IUNIT=IFIL

C Compute start record address for typical recovery day and
C find record at all time-steps for zone 1.
      IF(ISAVE.GE.1.AND.ISAVE.LE.4)THEN
        CALL MOREC1(ISDS,IREC)
        IBASE=IREC
        N=24*NTS
        DO 20 I=1,N
          DO 30 J=1,NCOMP
            IF(J.EQ.1)IHASHT(I)=IREC-IBASE
            CALL MOREC2(NZSUR(J),IREC)
   30     CONTINUE
   20   CONTINUE

C Compute the offset to each zone for a typical timestep.
        IREC=0
        DO 40,IZ=1,NCOMP
          IHASHZ(IZ)=IREC
          CALL MOREC2(NZSUR(IZ),IREC)
   40   CONTINUE

C Compute the first record for each day of the simulation.
        DO 50,IDR=ISDS,ISDF
          CALL MOREC1(IDR,IREC)
          IHASHD(IDR)=IREC
   50   CONTINUE

      ELSE
        call edisp(iuout,' HASHL: Save option does not allow access.')
        goto  8
      ENDIF

C Compute the start record offset for each result set including the 
C base case (which should equal zero).
      do 61, I=1,MNRS
        IHASHS(I)=0
 61   continue
      DO 60, I=1,NSIM
        IREC=I+1
        READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)NST1,NST2
        IHASHS(I)=NST2-IBASE
   60 CONTINUE

    8 RETURN

 1000 if(IOS.eq.2)then
        write(outs,'(A,I5)')
     &    'HASHL: no permission to scan library at record',IREC
      else
        write(outs,'(A,I5)')'HASHL: library error at record',IREC
      endif
      call edisp(iuout,outs)

      RETURN
      END
