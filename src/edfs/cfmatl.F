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

C This file contains subroutines to write property fields.
C Contains:
C  PRNMAT - Write PHI files (The format required by MATLAB)
C  STRFUN - Evaluates the stream funtion of a given flow.

C ********************* PRNMAT *********************
C PRNMAT - Write the field values of variables in ASCII format.
C  The file can be read into MATLAB, resulting in automatically
C  appearing graphs comprising the results of the simulation.
      SUBROUTINE PRNMAT(ifl)
#include "building.h"
#include "cfd.h"

C      common/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz) 
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      common/INCALC/INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,
     1              IZEROT,IZanKE,IMITZ
      common/utlcom/photon,matlab,textout,flwvis,xslice,yslice,zslice
      common/slicom/islice,jslice,kslice
      COMMON/STRF/SF(MCEL1D,MCEL1D)

      LOGICAL INCALU,INCALV,INCALW,INCALK,INCALD,INCALT
      LOGICAL IZEROT,IZanKE,IMITZ
      logical photon,matlab,textout,flwvis,xslice,yslice,zslice

      character q*1

      q=CHAR(39)

C Printing grid points.

      WRITE(ifl,499)
 499  FORMAT(//'% *********MATLAB-RESULTS-FILE***********'//
     &        '% --------- ESP-r   dfs  ----------------'//)

      if(incalu) then

C Xp 
        WRITE(ifl,'(a)') 'Xp=['
        WRITE(ifl,518)(XP(I),I=2,NIM1)
        WRITE(ifl,'(a)') '];'

C Xu
        WRITE(ifl,'(a)') 'Xu=['
        WRITE(ifl,518)(XU(I),I=2,NIM1)
        WRITE(ifl,'(a)') '];'
      endif
      if(incalv) then

C Yp
        WRITE(ifl,'(a)') 'Yp=['
        WRITE(ifl,518)(YP(J),J=2,NJM1)
        WRITE(ifl,'(a)') '];'

C Yv
        WRITE(ifl,'(a)') 'Yv=['
        WRITE(ifl,518)(YV(J),J=2,NJM1)
        WRITE(ifl,'(a)') '];'
      endif
      if(incalw) then

C Zp
        WRITE(ifl,'(a)') 'Zp=['
        WRITE(ifl,518)(ZP(K),K=2,NKM1)
        WRITE(ifl,'(a)') '];'

C Zw
        WRITE(ifl,'(a)') 'Zw=['
        WRITE(ifl,518)(ZW(K),K=2,NKM1)
        WRITE(ifl,'(a)') '];'
      endif

 518  FORMAT(6(E13.6,';'))

C Results are put in matrices for the different variables.
C Determination of the slice for which results are written.
C Slice number is defined at initialisation or in dfs-program (3-D).
      if(.NOT.incalu.or.xslice) then
        GOTO 610
      else if(.NOT.incalv.or.yslice) then
        GOTO 620
      else if(.NOT.incalw.or.zslice) then
        GOTO 630
      endif

C I-level.
 610  I=islice
      WRITE(ifl,'(a)') 'P=['
      WRITE(ifl,698)((P(I,J,K),K=2,NKM1),J=2,NJM1)
      WRITE(ifl,'(a)') '];'
      WRITE(ifl,'(a)',IOSTAT=ios,ERR=1) 
     &  'P1=reshape(P,length(Zp),length(Yp));'

      if(incalv)then
        WRITE(ifl,'(a)') 'V=['
        WRITE(ifl,698)((0.5*(Vf(I,J+1,K)+Vf(I,J,K)),K=2,NKM1),J=2,NJM1)
        WRITE(ifl,'(a)') '];'
        WRITE(ifl,'(a)',IOSTAT=ios,ERR=1)
     &   'V1=reshape(V,length(Zp),length(Yp));'
      endif
      if(incalw)then
        WRITE(ifl,'(a)') 'W=['
        WRITE(ifl,698)((0.5*(Wf(I,J,K+1)+Wf(I,J,K)),K=2,NKM1),J=2,NJM1)
        WRITE(ifl,'(a)') '];'
        WRITE(ifl,'(a)',IOSTAT=ios,ERR=1)
     &    'W1=reshape(W,length(Zp),length(Yp));'
      endif

C Stream function field.
      if(incalv.and.incalw) then
        CALL STRFUN('YZ',I)
        WRITE(ifl,'(a)') 'STR=['
        WRITE(ifl,698)((SF(J,K),K=2,NKM1),J=2,NJM1)
        WRITE(ifl,'(a)') '];'
        WRITE(ifl,'(a)',IOSTAT=ios,ERR=1)
     &    'STR1=reshape(STR,length(Zw),length(Yv));'
        I=islice
      endif
      if(incalk)then
        WRITE(ifl,'(a)') 'K=['
        WRITE(ifl,698)((TEf(I,J,K),K=2,NKM1),J=2,NJM1)
        WRITE(ifl,'(a)') '];'
        WRITE(ifl,'(a)',IOSTAT=ios,ERR=1)
     &    'K1=reshape(K,length(Zp),length(Yp));'
      endif
      if(incald)then
        WRITE(ifl,'(a)') 'E=['
        WRITE(ifl,698)((EDf(I,J,K),K=2,NKM1),J=2,NJM1)
        WRITE(ifl,'(a)') '];'
        WRITE(ifl,'(a)',IOSTAT=ios,ERR=1)
     &    'E1=reshape(E,length(Zp),length(Yp));'
      endif
      if(incalt)then
        WRITE(ifl,'(a)') 'T=['
        WRITE(ifl,698)((Tf(I,J,K),K=2,NKM1),J=2,NJM1)
        WRITE(ifl,'(a)') '];'
        WRITE(ifl,'(a)',IOSTAT=ios,ERR=1)
     &    'T1=reshape(T,length(Zp),length(Yp));'
      endif

C Start generation of graphs I-level.
      WRITE(ifl,611)
 611  FORMAT('Z1=(V1.^2+W1.^2).^0.5;')
      WRITE(ifl,612)islice
 612  FORMAT('figure(1);clg;cla;colormap(jet);pcolor(Yp,Zp,P1);view(2);
     & shading interp;colorbar;xlabel(''y[m]'');ylabel(''z[m]'');',
     &'title(''Pressure [Pa] Y-Z plan (I=',i2,')'');axis image;drawnow')

      if(incalv.AND.incalw)then
        WRITE(ifl,613)islice
 613  FORMAT('figure(2);clg;cla;colormap(jet);pcolor(Yp,Zp,Z1);view(2);
     &shading interp;colorbar;hold on;quiver(Yp,Zp,V1,W1,3,''k'');',
     &'xlabel(''y[m]'');ylabel(''z[m]'');title(''Velocity [m/s] Y-Z ',
     &'plan (I=',i2,')'');axis image;drawnow')

        WRITE(ifl,614)islice
 614  FORMAT('figure(3);clg;cla;contour(Yv,Zw,STR1,10,''w'');view(2);
     &xlabel(''y[m]'');ylabel(''z[m]'');title(''Stream Lines Y-Z ',
     &'plan (I=',i2,')'');axis image;drawnow')

      endif

      if(incalt)then
        WRITE(ifl,615)islice
 615  FORMAT('figure(4);clg;cla;colormap(jet);pcolor(Yp,Zp,T1);view(2);
     &shading interp;colorbar;xlabel(''y[m]'');ylabel(''z[m]'');',
     &'title(''Temperature [Cel. degrees ] Y-Z plan (I=',i2,')'');',
     &'axis image;drawnow')

      endif
      GOTO 799

C J-level.
 620  J=jslice
      WRITE(ifl,'(a)') 'P=['
      WRITE(ifl,698)((P(I,J,K),K=2,NKM1),I=2,NIM1)
      WRITE(ifl,'(a)') '];'
      WRITE(ifl,'(a)') 'P1=reshape(P,length(Zp),length(Xp));'
      if(incalu)then
        WRITE(ifl,'(a)') 'U=['
        WRITE(ifl,698)((0.5*(Uf(I+1,J,K)+Uf(I,J,K)),K=2,NKM1),I=2,NIM1)
        WRITE(ifl,'(a)') '];'
        WRITE(ifl,'(a)')'U1=reshape(U,length(Zp),length(Xp));'
      endif
      if(incalw)then
        WRITE(ifl,'(a)') 'W=['
        WRITE(ifl,698)((0.5*(Wf(I,J,K+1)+Wf(I,J,K)),K=2,NKM1),I=2,NIM1)
        WRITE(ifl,'(a)') '];'
        WRITE(ifl,'(a)')'W1=reshape(W,length(Zp),length(Xp));'
      endif

C Stream function field.
      if(incalu.and.incalw) then
        CALL STRFUN('XZ',J)
        WRITE(ifl,'(a)') 'STR=['
        WRITE(ifl,698)((SF(I,K),K=2,NKM1),I=2,NIM1)
        WRITE(ifl,'(a)') '];'
        WRITE(ifl,'(a)') 'STR1=reshape(STR,length(Zw),length(Xu));'
        J=jslice
      endif
      if(incalk)then
        WRITE(ifl,'(a)') 'K=['
        WRITE(ifl,698)((TEf(I,J,K),K=2,NKM1),I=2,NIM1)
        WRITE(ifl,'(a)') '];'
        WRITE(ifl,'(a)') 'K1=reshape(K,length(Zp),length(Xp));'
      endif
      if(incald)then
        WRITE(ifl,'(a)') 'E=['
        WRITE(ifl,698)((EDf(I,J,K),K=2,NKM1),I=2,NIM1)
        WRITE(ifl,'(a)') '];'
        WRITE(ifl,'(a)') 'E1=reshape(E,length(Zp),length(Xp));'
      endif
      if(incalt)then
        WRITE(ifl,'(a)') 'T=['
        WRITE(ifl,698)((Tf(I,J,K),K=2,NKM1),I=2,NIM1)
        WRITE(ifl,'(a)') '];'
        WRITE(ifl,'(a)') 'T1=reshape(T,length(Zp),length(Xp));'
      endif

C Start generation of graphs J-level.
      WRITE(ifl,621)
 621  FORMAT('Z1=(U1.^2+W1.^2).^0.5;')

      WRITE(ifl,622)jslice
 622  FORMAT('figure(1);clg;cla;colormap(jet);pcolor(Xp,Zp,P1);view(2);
     &shading interp;colorbar;xlabel(''x[m]'');ylabel(''z[m]'');',
     &'title(''Pressure [Pa] X-Z plan (J=',i2,')'');axis image;drawnow')

      if(incalu.AND.incalw)then
        WRITE(ifl,623)jslice
 623  FORMAT('figure(2);clg;cla;colormap(jet);pcolor(Xp,Zp,Z1);view(2);
     &shading interp;colorbar;hold on;quiver(Xp,Zp,U1,W1,3,''k'');',
     &'xlabel(''x[m]'');ylabel(''z[m]'');title(''Velocity [m/s] X-Z ',
     &'plan (J=',i2,')'');axis image;drawnow')

        WRITE(ifl,624)jslice
 624  FORMAT('figure(3);clg;cla;contour(Xu,Zw,STR1,10,''w'');view(2);
     &xlabel(''x[m]'');ylabel(''z[m]'');title(''Stream Lines  X-Z ',
     &'plan (J=',i2,')'');axis image;drawnow')

      endif

      if(incalt)then
         WRITE(ifl,625)jslice
 625  FORMAT('figure(4);clg;cla;colormap(jet);pcolor(Xp,Zp,T1);view(2);
     &shading interp;colorbar;xlabel(''x[m]'');ylabel(''z[m]'');',
     &'title(''Temperature [Cel. degrees ] X-Z plan (J=',i2,')'');',
     &'axis image;drawnow')

      endif
      GOTO 799

C Z-level.
 630  K=kslice
      WRITE(ifl,'(a)') 'P=['
      WRITE(ifl,698)((P(I,J,K),J=2,NJM1),I=2,NIM1)
      WRITE(ifl,'(a)') '];'
      WRITE(ifl,'(a)') 'P1=reshape(P,length(Yp),length(Xp));'
      if(incalu)then
        WRITE(ifl,'(a)') 'U=['
        WRITE(ifl,698)((0.5*(Uf(I+1,J,K)+Uf(I,J,K)),J=2,NJM1),I=2,NIM1)
        WRITE(ifl,'(a)') '];'
        WRITE(ifl,'(a)') 'U1=reshape(U,length(Yp),length(Xp));'
      endif
      if(incalv)then
        WRITE(ifl,'(a)') 'V=['
        WRITE(ifl,698)((0.5*(Vf(I,J+1,K)+Vf(I,J,K)),J=2,NJM1),I=2,NIM1)
        WRITE(ifl,'(a)') '];'
        WRITE(ifl,'(a)') 'V1=reshape(V,length(Yp),length(Xp));'
      endif

C Stream function field.
      if(incalu.and.incalv) then
        CALL STRFUN('XY',K)
        WRITE(ifl,'(a)') 'STR=['
        WRITE(ifl,698)((SF(I,J),J=2,NJM1),I=2,NIM1)
        WRITE(ifl,'(a)') '];'
        WRITE(ifl,'(a)') 'STR1=reshape(STR,length(Yv),length(Xu));'
        K=kslice
      endif
      if(incalk)then
        WRITE(ifl,'(a)') 'K=['
        WRITE(ifl,698)((TEf(I,J,K),J=2,NJM1),I=2,NIM1)
        WRITE(ifl,'(a)') '];'
        WRITE(ifl,'(a)') 'K1=reshape(K,length(Yp),length(Xp));'
      endif
      if(incald)then
        WRITE(ifl,'(a)') 'E=['
        WRITE(ifl,698)((EDf(I,J,K),J=2,NJM1),I=2,NIM1)
        WRITE(ifl,'(a)') '];'
        WRITE(ifl,'(a)') 'E1=reshape(E,length(Yp),length(Xp));'
      endif
      if(incalt)then
        WRITE(ifl,'(a)') 'T=['
        WRITE(ifl,698)((Tf(I,J,K),J=2,NJM1),I=2,NIM1)
        WRITE(ifl,'(a)') '];'
        WRITE(ifl,'(a)') 'T1=reshape(T,length(Yp),length(Xp));'
      endif

C Start generation of graphs K-level.
      WRITE(ifl,631)
 631  FORMAT('Z1=(U1.^2+V1.^2).^0.5;')
      WRITE(ifl,632)kslice
 632  FORMAT('figure(1);clg;cla;colormap(jet);pcolor(Xp,Yp,P1);view(2);
     &shading interp;colorbar;xlabel(''x[m]'');ylabel(''y[m]'');',
     &'title(''Pressure [Pa] X-Y plan (K=',i2,')'');axis image;drawnow')

      if(incalu.AND.incalv)then
        WRITE(ifl,633)kslice
 633  FORMAT('figure(2);clg;cla;colormap(jet);pcolor(Xp,Yp,Z1);view(2);
     &shading interp;colorbar;hold on;quiver(Xp,Yp,U1,V1,3,''k'');',
     &'xlabel(''x[m]'');ylabel(''y[m]'');title(''Velocity [m/s] X-Y ',
     &'plan (K=',i2,')'');axis image;drawnow')

        WRITE(ifl,634)kslice
 634  FORMAT('figure(3);clg;cla;contour(Xu,Yv,STR1,10,''w'');view(2);
     &xlabel(''x[m]'');ylabel(''y[m]'');title(''Stream Lines X_Y ',
     &'plan (K=',i2,')'');axis image;drawnow')

      endif

      if(incalt)then
        WRITE(ifl,635)kslice
 635  FORMAT('figure(4);clg;cla;colormap(jet);pcolor(Xp,Yp,T1);view(2);
     &shading interp;colorbar;xlabel(''x[m]'');ylabel(''y[m]'');',
     &'title(''temperature [Cel. degrees ] X_Y plan (K=',i2,')'');',
     &'axis image;drawnow')

      endif
      GOTO 799

 698   FORMAT(6(E13.6,';'))

 799   CONTINUE
      RETURN

 1    CALL USRMSG(' Problem writing matlab.',' ','W')
      CALL  ERPFREE(ifl,ISTAT)
      return
      END


C ********************* STRFUN *********************
C STRFUN - Evaluates the stream funtion of a given flow.
      SUBROUTINE STRFUN(OPC,I) 
#include "building.h"
#include "cfd.h"

      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
C      COMMON/FILEP/IFIL
C      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz) 
      COMMON/STRF/SF(MCEL1D,MCEL1D)

      CHARACTER*2 OPC

      SF(2,2)=0.0

      IF(OPC.EQ.'XY') THEN  

C x-y plan.
      K=I
      DO 100 I=2,NIM1
         IF(I.NE.2) SF(I,2)=SF(I-1,2)-DENf(I-1,2,K)*Vf(I-1,2,K)*
     &                                               (XU(I)-XU(I-1))
         DO 110 J=3,NJM1
           IF(I.EQ.2)THEN
             RHO=DENf(I,J,K)
           ELSEIF(I.EQ.NI)THEN
             RHO=DENf(I-1,J,K)
           ELSE
             RHO=(DENf(I,J,K)+DENf(I-1,J,K))*0.5
           ENDIF
           SF(I,J)=SF(I,J-1)+RHO*Uf(I,J-1,K)*(YV(J)-YV(J-1))
  110     CONTINUE
  100   CONTINUE     
      ELSE IF(OPC.EQ.'YZ') THEN

C y-z plan.
      DO 200 J=2,NJM1
         IF(J.NE.2) SF(J,2)=SF(J-1,2)-DENf(I,J-1,2)*Wf(I,J-1,2)
     &                                             *(YV(J)-YV(J-1))
         DO 210 K=3,NKM1
           IF(J.EQ.2)THEN
             RHO=DENf(I,J,K)
           ELSEIF(J.EQ.NJ)THEN
             RHO=DENf(I,J-1,K)
           ELSE
             RHO=(DENf(I,J,K)+DENf(I,J-1,K))*0.5
           ENDIF
           SF(J,K)=SF(J,K-1)+RHO*Vf(I,J,K-1)*(ZW(K)-ZW(K-1))
  210     CONTINUE
  200   CONTINUE
      ELSE IF(OPC.EQ.'XZ') THEN

C x-z plan.
      J=I
      DO 300 I=2,NIM1
         IF(I.NE.2) SF(I,2)=SF(I-1,2)-DENf(I-1,J,2)*Wf(I-1,J,2)
     &                                             *(XU(I)-XU(I-1))
         DO 310 K=3,NKM1
           IF(I.EQ.2)THEN
             RHO=DENf(I,J,K)
           ELSEIF(I.EQ.NI)THEN
             RHO=DENf(I-1,J,K)
           ELSE
             RHO=(DENf(I,J,K)+DENf(I-1,J,K))*0.5
           ENDIF
           SF(I,K)=SF(I,K-1)+RHO*Uf(I,J,K-1)*(ZW(K)-ZW(K-1))
  310     CONTINUE
  300   CONTINUE

      ENDIF
      

      RETURN
      END
