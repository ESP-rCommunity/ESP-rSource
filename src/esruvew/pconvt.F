      SUBROUTINE PCONVT(NFILE,N1,N2)

      INCLUDE 'viewer.h2'

      PARAMETER (MNF=10, MTR=100)

      COMMON /TRANSC/ NTR(MNF),IPTR(MNF),IPNXT,
     &                ITR(MTR),XTR(MTR),YTR(MTR),ZTR(MTR)
      logical close

      IF (NFILE .EQ. 1) RETURN

      PIA=3.14159/180.0

C Scale and rotate and shift vertices N1 to N2
      DO 90 K=N1,N2

C Get coords
        X0=PUNT(K,1)
        Y0=PUNT(K,2)
        Z0=PUNT(K,3)

C Transforms are applied in reverse order eg 4,3,2
        DO 50 I=NFILE,2,-1
C Set origin zero for this file-trtansform set
          XOR=0.
          YOR=0.
          ZOR=0.
          JNT=NTR(I)
          DO 60 KK=1,JNT
            JP=IPTR(I)+KK-1
            JTR=ITR(JP)
            IF (JTR.EQ.0)THEN
C Origin set
              XOR=XTR(JP)
              YOR=YTR(JP)
              ZOR=ZTR(JP)
            ELSE IF (JTR.EQ.1) THEN
C Scale
              X0=(X0-XOR)*XTR(JP)
              Y0=(Y0-YOR)*YTR(JP)
              Z0=(Z0-ZOR)*ZTR(JP)
            ELSE IF (JTR.EQ.2)THEN
C Translate by
              X0=X0+XTR(JP)
              Y0=Y0+YTR(JP)
              Z0=Z0+ZTR(JP)
            ELSEIF (JTR.EQ.3)THEN

C Rotate about X,Y,Z axis
              call eclose(xtr(jp),0.0,0.001,close)
              if(.NOT.close)then
                cosx=cos(pia*xtr(jp))
                sinx=sin(pia*xtr(jp))
                yn=(y0-YOR)*cosx-(z0-ZOR)*sinx
                zn=(y0-YOR)*sinx+(z0-ZOR)*cosx
                y0=yn
                z0=zn
              endif
              call eclose(ytr(jp),0.0,0.001,close)
              if(.NOT.close)then
                cosy=cos(pia*ytr(jp))
                siny=sin(pia*ytr(jp))
                Zn=(Z0-ZOR)*cosy-(X0-XOR)*siny
                Xn=(Z0-ZOR)*siny+(X0-XOR)*cosy
                Z0=Zn
                X0=Xn
              endif
              call eclose(ztr(jp),0.0,0.001,close)
              if(.NOT.close)then
                cosZ=cos(pia*Ztr(jp))
                sinZ=sin(pia*Ztr(jp))
                Xn=(X0-XOR)*cosZ-(Y0-YOR)*sinZ
                Yn=(X0-XOR)*sinZ+(Y0-YOR)*cosZ
                X0=Xn
                Y0=Yn
              endif
            endif
 60       CONTINUE

C If the  origin moved then we need to translate the object
C back to where it came from
          x0 = x0 + xor
          y0 = y0 + yor
          z0 = z0 + zor
 50     CONTINUE
 
        PUNT(K,1)=X0
        PUNT(K,2)=Y0
        PUNT(K,3)=Z0
 90   CONTINUE
      RETURN
      END
