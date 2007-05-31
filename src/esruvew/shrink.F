C COPYRIGHT R. P. PARKINS, OF THE ABACUS UNIT, 1977
      SUBROUTINE SHRINK(NP1,NP2)

      INCLUDE 'viewer.h'
      INCLUDE 'viewer.h2'
C        include 'viewer.h'
C        include 'viewer.h2'

      SF=0.0005
      XM=0.
      YM=0.
      ZM=0.
      NP3=NP2-NP1+1
      DO 80 I=NP1,NP2
           XM=XM+PUNT(I,1)
           YM=YM+PUNT(I,2)
  80       ZM=ZM+PUNT(I,3)
      XM=XM/FLOAT(NP3)
      YM=YM/FLOAT(NP3)
      ZM=ZM/FLOAT(NP3)
      DO 81 I=NP1,NP2
           ZMXP=AMAX1(ZMXP,PUNT(I,3))
           ZMNP=AMIN1(ZMNP,PUNT(I,3))
           PUNT(I,1)=PUNT(I,1)+SF*(XM-PUNT(I,1))
           PUNT(I,2)=PUNT(I,2)+SF*(YM-PUNT(I,2))
           PUNT(I,3)=PUNT(I,3)+SF*(ZM-PUNT(I,3))
           POZ(I)=PUNT(I,3)
           XMX=AMAX1(XMX,PUNT(I,1))
           XMN=AMIN1(XMN,PUNT(I,1))
           YMX=AMAX1(YMX,PUNT(I,2))
           YMN=AMIN1(YMN,PUNT(I,2))
           ZMX=AMAX1(ZMX,PUNT(I,3))
           ZMN=AMIN1(ZMN,PUNT(I,3))
  81  CONTINUE
      RETURN
      END
