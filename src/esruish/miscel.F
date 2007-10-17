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

C MISCEL.F contains the following miscellaneous routines used by ESRUshd:

C AREA:    Returns area for any polygon.
C MESH:    Computes centre point X&Z coord of grid squares.
C PICKSSUR selects surfaces for shading & insolation analysis.
C POINT1: Decides if a point is within a face boundary.
C POINT2: Decides if a point is within a shadow boundary.

C This routine draws a mercator sun path diagram
C ************* AREA 
      SUBROUTINE AREA(N,X,Y,A)
C AREA calculates the area for any polygon where N is the number
C of points, X & Y are arrays of coordinates around the surface 
C and A is the calculated area.
#include "building.h"
      DIMENSION X(MV),Y(MV)
      A=0
      DA=(X(2)-X(1))**2+(Y(2)-Y(1))**2
      DO 10 I=3,N
      DB=(X(I)-X(1))**2+(Y(I)-Y(1))**2
      DC=(X(I)-X(I-1))**2+(Y(I)-Y(I-1))**2
      SI=(Y(1)-Y(I-1))*(X(I)-X(I-1))-
     &   (Y(I)-Y(I-1))*(X(1)-X(I-1))
      IF(SI.GT.0.)SI=1.
      IF(SI.LT.0.)SI=-1.
      IF(abs(SI).lt.0.0001)GO TO 10

C Check square root is not negative.
      temp=2.*(DA*DB+DB*DC+DC*DA)-(DA*DA+DB*DB+DC*DC)
      if (temp.gt.0.00001)then
        AREX=.25*SQRT(temp)
      else
        AREX=0.0
      endif
      A=A+SI*AREX
  10  DA=DB
      A=ABS(A)
      RETURN
      END


C *************** MESH ***********************
C 'MESH' Computes the centre point X & Z coordinate of each rectangular
C grid square of a mesh superimposed on the opaque surface in question
C and any associated windows.
      SUBROUTINE MESH(IS)
#include "building.h"
      COMMON/OUTPCH/ICOUT
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G1T/XFT(MV),ZFT(MV)
      COMMON/GS6/NOX,NOZ,NGX,NGZ,BLKNAME(MB),BLKMAT(MB)
      COMMON/GRID11/XOP(MOX,MOZ),ZOP(MOX,MOZ)
      COMMON/GRID3/OGRIDA(MS)
      CHARACTER BLKNAME*8,BLKMAT*12,outs*124
      logical OK
      logical tok

C If verbose trace requested enable writing.
      tok=.false.
      if(icout.eq.33.and.itc.gt.1)tok=.true.

C Derive bounding box (in 2D) for surface `is`. First find max and min
C X & Z values for this surface by scanning each vertex.
      ngldum=0
      XMAX=-1.E+10
      ZMAX=-1.E+10
      XMIN=1.E+10
      ZMIN=1.E+10
      NV=NVER(IS)
      DO 10 I=1,NV
        XMAX=AMAX1(XMAX,XFT(I))
        ZMAX=AMAX1(ZMAX,ZFT(I))
        XMIN=AMIN1(XMIN,XFT(I))
        ZMIN=AMIN1(ZMIN,ZFT(I))
   10 CONTINUE

C Mesh length and height is given by:
      DXMESH=XMAX-XMIN
      DZMESH=ZMAX-ZMIN

C Establish grid interval for this surface. Ogrida is the area of
C one grid square.
      DIVX=DXMESH/NOX
      DIVZ=DZMESH/NOZ
      OGRIDA(IS)=DIVX*DIVZ

C Compute X & Z values for centre point of each grid square.
      DO 20 I=1,NOX
        DO 30 J=1,NOZ
          XOP(I,J)=DIVX/2.+(I-1)*DIVX
          ZOP(I,J)=DIVZ/2.+(J-1)*DIVZ
   30   CONTINUE
   20 CONTINUE

C Trace if verbose level.
      IF(.NOT.TOK)goto 9985

      WRITE(outs,9986)IS
 9986 FORMAT(
     &' Opaque surface grid centre coordinates for surface ',I2,':')
      call edisp(icout,outs)
      call edisp(icout,' NOX|NOZ|     X   |    Z')
      DO 9996 I=1,NOX
        DO 9995 J=1,NOZ
          WRITE(outs,9994)I,J,XOP(I,J),ZOP(I,J)
 9994     FORMAT(I3,',',I3,2F10.2)
          call edisp(icout,outs)
 9995   CONTINUE
 9996 CONTINUE
      call edisp(icout,'  ')
 9985 RETURN
      END

C ******************** PICKSSUR 
C PICKSSUR selects surfaces for shading & insolation analysis.
C IZONE is the focus zone, NP is the number of surfaces selected,
C act = 's or S' shading, act = 'i or I' insolation,
C act = 'l or L' view shading surfs, act = '-' neither. 
C IVA array of selections. Uses information from ZONDF.

      SUBROUTINE PICKSSUR(IZONE,NP,act,IVA,IER)
#include "building.h"

      common/pophelp/h(60)
      COMMON/C1/NCOMP,NCON
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/CONTR/MON,ISC(MS),IYD

C iaplic(1) toggle for shading; iaplic(2) toggle for insolation; iaplic(3)
C   toggle for assessment where:
C   1 is 'all_applicable', 0 is manual selection of surfaces.
C nsurfcalc nb of shaded surfaces, lstsfcalc() list of applicable surfaces.
C nsurfinso nb of insolation sources, isurfinso() list of insolation sources.
      common/ishdirec/iaplic(MCOM,2),nsurfcalc(MCOM),lstsfcalc(MCOM,MS),
     &       nsurfinso(MCOM),isurfinso(MCOM,MS)

      DIMENSION STMP(MS),IVA(MS),IVALS(MS)
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      character H*72,STMP*33,act*1

      IER=0
      IF(IZONE.GT.NCOMP.OR.IZONE.EQ.0)THEN
        CALL USRMSG(' ',' Zone number out of range! ','W')
        IER=1
        RETURN
      ENDIF

 43   h(1) ='A shading analysis finds the temporal patterns of '
      h(2) ='shading on exterior surfaces.  Insolation analysis'
      h(3) ='tracks direct solar radiation entering a zone and '
      h(4) ='it distribution. '
      h(5) =' '
      h(6) ='You are being asked to select surfaces. If you select'
      h(7) ='All applicable then this is: '
      h(8) ='  shading - all outside facing opaque & tansparent '
      h(9) ='  insolation - all outside facing tansparent '
      h(10)=' '
      h(11)='You may also manually select surfaces. '
      h(12)='Note: this process works best if the problem topology '
      h(13)='(boundary conditions) and surface conposition (so '
      h(14)=' '
      if(act.eq.'s'.or.act.eq.'S')then
         CALL EASKABC('Shading  options:',' ','all applicable surfaces',
     &          'manual surf selection','continue ? ',IIC,14)
         if(iic.eq.1)iaplic(izone,1)=1
         if(iic.eq.2)iaplic(izone,1)=0
      elseif(act.eq.'i'.or.act.eq.'I')then
         CALL EASKABC('Insolation options:',' ',
     &     'all applicable surfaces','manual surf selection',
     &     'continue ? ',IIC,14)
         if(iic.eq.1)iaplic(izone,2)=1
         if(iic.eq.2)iaplic(izone,2)=0
      elseif(act.eq.'l'.or.act.eq.'L')then
        IIC=4
      else
        IIC=2
      endif

C Clear return array.
      do 14 i=1,NZSUR(IZONE)
        IVA(I)=0
  14  continue

C Process initial selection.
      if(IIC.eq.3)then
        return
      elseif(IIC.eq.1)then
        if(act.eq.'s'.or.act.eq.'S')then
          NP=0
          DO 13 I=1,NZSUR(IZONE)
            ICN=IZSTOCN(izone,i)
            if(SSOTHER(icn,1)(1:5).eq.'EXTER')then
              NP=NP+1
              IVA(NP)=I
            endif
  13      continue
          if(NP.eq.0)then
            CALL usrmsg('No applicable surfaces found:',' ','W')
            goto 43
          endif
          return
        elseif(act.eq.'i'.or.act.eq.'I')then
          NP=0
          DO 15 I=1,NZSUR(IZONE)
            ICN=IZSTOCN(izone,i)
            if(SSOTHER(icn,1)(1:5).eq.'EXTER'.and.
     &         SSOTF(icn)(1:4).ne.'OPAQ')then
              NP=NP+1
              IVA(NP)=I
            endif
   15     continue
          if(NP.eq.0)then
            CALL usrmsg('No applicable surfaces found:',' ','W')
            goto 43
          endif
          return
        endif
      elseif(IIC.eq.2)then
        INPIC=NZSUR(IZONE)
        DO 11 I=1,INPIC
         ICN=IZSTOCN(izone,i)
         if(act.eq.'s'.or.act.eq.'S')then
           if(SSOTHER(ICN,1)(1:5).eq.'EXTER')then
             write(STMP(I),'(5a)')SSNAME(ICN),'|',SSOTF(ICN)(1:4),'|',
     &         SSOTHER(ICN,1)(1:15)
           else
             write(STMP(I),'(2a)')SSNAME(ICN),' not applicable'
           endif
         elseif(act.eq.'-')then
           write(STMP(I),'(5a)')SSNAME(ICN),'|',SSOTF(ICN)(1:4),'|',
     &       SSOTHER(ICN,1)(1:15)
         elseif(act.eq.'i'.or.act.eq.'I')then
           if(SSOTHER(ICN,1)(1:5).eq.'EXTER'.and.
     &        SSOTF(ICN)(1:4).ne.'OPAQ')then
             write(STMP(I),'(5a)')SSNAME(ICN),'|',SSOTF(ICN)(1:4),'|',
     &         SSOTHER(ICN,1)(1:15)
           else
             write(STMP(I),'(a,a)')SSNAME(ICN),' not applicable'
           endif
         endif
         IVA(I)=0
   11   CONTINUE

        H(1)=' Pick one, several or All surfaces for inclusion'
        CALL EPICKS(INPIC,IVALS,' ',' Which surfaces to include: ',
     &   33,NZSUR(IZONE),STMP,' name & type & exposure',IER,1)
        if(INPIC.eq.0)then
          RETURN
        else
          NP=INPIC
          DO 12 I=1,NP
            IVA(I)=IVALS(I)
   12     CONTINUE
        endif

C Debug...
C        write(6,*) 'act np iva ',act,np,iva
      elseif(IIC.eq.4)then
        INPIC=NZSUR(IZONE)
        DO 21 I=1,INPIC
          ICN=IZSTOCN(izone,i)
          if(ISC(I).EQ.1)then
            write(STMP(I),'(5a)')SSNAME(ICN),'|',SSOTF(ICN)(1:4),'|',
     &         SSOTHER(ICN,1)(1:15)
          else
            write(STMP(I),'(a,a)')SSNAME(ICN),' not assessed'
          endif
          IVA(I)=0
   21   CONTINUE
        INPIC=1
        H(1)=' Surfaces which have been assessed are listed.'
        CALL EPICKS(INPIC,IVALS,' ',' Which surface to view: ',
     &   33,NZSUR(IZONE),STMP,' shading view surf',IER,1)
        if(INPIC.eq.0)then
          RETURN
        else
          NP=INPIC
          IVA(1)=IVALS(1)
        endif

C Debug...
C        write(6,*) 'act np iva ',act,np,iva
      endif
      RETURN
      END

C ******************** ASKDAY 
C ASKDAY is a standard call to enquire a date. 
C Returns IMO (month), IDO (day of month), IJDAY (day of year).
C IFDAY is a toggle to control the display and input of periods -
C       0 = julian day, 1 or 2 = day of month.
      SUBROUTINE ASKDAY(IFDAY,IMO,IDO,IJDAY,IER)

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      character H*72
      DIMENSION ID(12)
      DATA ID/31,28,31,30,31,30,31,31,30,31,30,31/

      IER=0
      H(1)='The output time defines the day and month.'
  281 IF(IFDAY.EQ.0)THEN
        CALL EASKI(IJDAY,' ',' day-of-year ? ',
     &             1,'F',365,'F',1,'start day',IER,1)
        CALL EDAYR(IJDAY,IDO,IMO)
        if(IER.ne.0)goto 281
      ELSE
        CALL EASKI(IDO,' ', ' Day of the month ? ',
     &             1,'F',31,'F',1,'start day',IER,1)
        CALL EASKI(IMO,' ', ' Month number     ? ',
     &             1,'F',12,'F',1,'end day',IER,1)
        CALL EDAY(IDO,IMO,IJDAY)
      ENDIF

C Check range.
      IF(IDO.GT.ID(IMO))THEN
        call edisp(iuout,' Day past end of month... try again. ')
        goto 281
      ENDIF
      CALL EDAY(IDO,IMO,IJDAY)

      RETURN
      END

C ************* POINT1 **********************
C 'POINT1' determines if a specified point is within or outwith any
C specified object body face boundary.
      SUBROUTINE POINT1(IS,XX,ZZ,IANS)
#include "building.h"
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G1T/XFT(MV),ZFT(MV)
      DIMENSION C(2*MV+2)
      IANS=0

C Determine if point XX,ZZ is within this face.
      NP=NVER(IS)
      DO 10 I=1,NP
        II=2*I-1
        C(II)=XFT(I)
        II=II+1
        C(II)=ZFT(I)
   10 CONTINUE
      LAST1=2*NP+1
      LAST2=2*NP+2
      C(LAST1)=XFT(1)
      C(LAST2)=ZFT(1)
      LN=0
      IP=NP+1
      DO 20 I=2,IP
        N=2*I
        IF((C(N-2)-ZZ)*(ZZ-C(N)))20,1,2
    2   IF((ZZ-C(N-2))*(C(N-1)-C(N-3))/(C(N)-C(N-2))+C(N-3)-XX)20,3,4
    1   IF(C(N-2)-C(N))5,6,7
    6   IF((C(N-3)-XX)*(XX-C(N-1)))20,3,3
    5   IF((ZZ-C(N-2))*(C(N-1)-C(N-3))/(C(N)-C(N-2))+C(N-3)-XX)20,3,8
    7   IF((ZZ-C(N-2))*(C(N-1)-C(N-3))/(C(N)-C(N-2))+C(N-3)-XX)20,3,9
    9   LN=LN-2
    8   LN=LN-1
    4   LN=LN+2
   20 CONTINUE
      IF((LN/4)*4.NE.LN)goto 3
      goto 11
    3 IANS=1
   11 RETURN
      END

C ************* POINT2 **********************
C 'POINT2' decides if a specified point (XX,ZZ) is within or 
C outwith any shadow boundary from common SHAD2.
      SUBROUTINE POINT2(XX,ZZ,IANS)
      COMMON/SHAD2/ISHD,XS(6),ZS(6)
      DIMENSION C(14)

      IANS=0
      NP=6
      DO 10 I=1,NP
        II=2*I-1
        C(II)=XS(I)
        II=II+1
        C(II)=ZS(I)
   10 CONTINUE
      LAST1=2*NP+1
      LAST2=2*NP+2
      C(LAST1)=XS(1)
      C(LAST2)=ZS(1)
      LN=0
      IP=NP+1
      DO 20 I=2,IP
        N=2*I
        IF((C(N-2)-ZZ)*(ZZ-C(N)))20,1,2
    2   IF((ZZ-C(N-2))*(C(N-1)-C(N-3))/(C(N)-C(N-2))+C(N-3)-XX)20,3,4
    1   IF(C(N-2)-C(N))5,6,7
    6   IF((C(N-3)-XX)*(XX-C(N-1)))20,3,3
    5   IF((ZZ-C(N-2))*(C(N-1)-C(N-3))/(C(N)-C(N-2))+C(N-3)-XX)20,3,8
    7   IF((ZZ-C(N-2))*(C(N-1)-C(N-3))/(C(N)-C(N-2))+C(N-3)-XX)20,3,9
    9   LN=LN-2
    8   LN=LN-1
    4   LN=LN+2
   20 CONTINUE
      IF((LN/4)*4.NE.LN)goto 3
      goto 11
    3 IANS=1
   11 RETURN
      END

C 'INWPT1' decides if a specified point is within or outwith
C the internal surface default window section. Since there
C are no default windows this subroutine is not needed.

