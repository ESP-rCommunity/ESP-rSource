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

C WSHAD.F calculates shadow cast by overhang and side fins.

C PHI.....solar azimuth angle
C COSZ....cosine of solar zenith angle
C SHRAT...shade ratio: ratio of the sunlit area to the total window area
C HT......window height
C FL......window width
C FP......depth of the overhang
C AW......distance from the top of the window to the overhang
C BWL.....distance of the overhang extended beyond the left edge of the window
C BWR.....distance of the overhang extended beyond the right edge of the window
C D.......depth of vertical projection at the end of the overhang
C FP1.....depth of the left fin
C A1......distance of the left fin extended above the top of the window
C B1......distance from the left edge of the window to the left fin
C C1......distance of the left fin stop short above the bottom of the window
C FP2.....depth of the right fin
C A2......distance of the right fin extended above the top of the window
C B2......distance from the right edge of the window to the right fin
C C2......distance of the right fin stop short above the bottom of the window
C WAZI....window azimuth angle
      SUBROUTINE WSHAD(SHDX,PHI,COSZ,SHRAT)
      DIMENSION SHDX(20)
      FL=SHDX(1)
      HT=SHDX(2)
      FP=SHDX(3)
      AW=SHDX(4)
      BWL=SHDX(5)
      BWR=SHDX(6)
      D=SHDX(7)
      FP1=SHDX(8)
      A1=SHDX(9)
      B1=SHDX(10)
      C1=SHDX(11)
      FP2=SHDX(12)
      A2=SHDX(13)
      B2=SHDX(14)
      C2=SHDX(15)
      WAZI=SHDX(16)
      SHRAT=1.


      A=AW
      H=HT
      GAMMA=PHI-WAZI
      COSG=COS(GAMMA)
      IF(COSG)100,100,104
100   SHRAT=0.0
      GO TO 2000
104   CONTINUE
      SBETA=COSZ
      IF(SBETA)100,100,152
152   SING=SIN(GAMMA)
      VERT=SBETA/SQRT(1.0-SBETA*SBETA)/COSG
      HORIZ=ABS(SING)/COSG
      TCETA=VERT/HORIZ
      IF(GAMMA)155,154,154

C Sun on left.
154   B=BWL
      GO TO 156

C Sun on right.
155   B=BWR
156   ARSHF=0.0
      AREAV=0.0
      ARSIF=0.0
      AREAO=0.0
      AREA1=0.0
      ARSH1=0.0
      FL3=0.0
      H3=0.0
      H1=H
      FL1=FL
      K=1
      L=1
      T1=FP*VERT
      FM1=FP*HORIZ
      IF(FP)37,37,153
153   T=T1
      FM=FM1
      AB=B*TCETA
      UG=(FL+B)*TCETA
      DE=(H+A)/TCETA

C Horizontal overhang 'AREAO'.
      IF(T-A)27,27,2
2     IF(AB-A)14,14,3
3     IF(DE-B)12,12,4
4     IF(FM-B)11,11,5
5     IF(DE-(FL+B))8,8,6
6     IF(FM-(FL+B))9,7,7

C Horizontal 9.
7     AREAO=FL*(0.5*(AB+UG)-A)
      GO TO 37
8     IF(T-(H+A))9,10,10

C Horizontal 7.
9     AREAO=(T-A)*FL-((FM-B)**2)*TCETA*0.5
      L=2
      GO TO 21

C Horizontal 8.
10     AREAO=H*FL-(DE-B)**2*TCETA*0.5
      GO TO 37

C Horizontal 3.
11    AREAO=FL*(T-A)
      L=2
      GO TO 24
12    IF(T-(H+A))11,13,13

C Horizontal 2.
13    AREAO=H*FL
      GO TO 68
14    IF(UG-A)27,27,15
15    IF(DE-(FL+B))18,18,16
16    IF(FM-(FL+B))20,17,17

C Horizontal 6.
17    AREAO=(UG-A)**2/TCETA*0.5
      GO TO 37
18    IF(T-(H+A))20,19,19

C Horizontal 5.
19    AREAO=H*(FL-(A+0.5*H)/TCETA+B)
      GO TO 37

C Horizontal 4.
20    AREAO=(T-A)*(FL+B-FM*(1.0+A/T)*0.5)
      L=2

C Vertical projection 'AREAV'.
21    FL3=FL+B-FM
      IF(T+D-(H+A))22,22,23

C Vertical 8.
22    H3=D
      GO TO 3700

C Vertical 9.
23    H3=H+A-T
      GO TO 3700
24    FL3=FL
      IF(T+D-(H+A))26,26,25

C Vertical 7.
25    H3=H+A-T
      AREAV=H3*FL3
      GO TO 68

C Vertical 6.
26    H3=D
      GO TO 3700
27    IF(T+D-A)37,37,28
28    IF(FM-B)34,34,29

C FN not defined!
29    IF(FN-(FL+B))31,37,37
31    FL3=FL+B-FM
      IF(T+D-(H+A))33,33,32

C Vertical 5.
32    H3=H
      GO TO 3700

C Vertical 4.
33    H3=T+D-A
      GO TO 3700
34    IF(T+D-(H+A))36,35,35

C Vertical 2.
35    AREAV=H*FL
      GO TO 68

C Vertical 3.
36    H3=T+D-A
      FL3=FL
3700  AREAV=FL3*H3

C Side fin and short side fin.
C Side fin 'AREA1','ARSIF'
37    IF(GAMMA)66,68,74
74    FPF=FP1
      AF=A1
      BF=B1
      CX=C1
      GO TO 84
66    FPF=FP2
      AF=A2
      BF=B2
      CX=C2
84    IF(FPF)68,68,67
67    T=FPF*VERT
      FM=FPF*HORIZ
      AF1=AF
      IF(AREAO)73,73,88

C Test for overlap of fin and overhang shadow.
88    AT=A+(BF-B)*TCETA
      IF(AT-AF)711,73,73

C Overlap exists.
C Set L=2 if overhang shadow has horizontal edge in window.
711   GO TO (621,712),L

C Test for type of overlap.
712   IF((FM-BF)-(FM1-B))621,622,622

C Set L=1, shadow intersects on inclined edge of overhang shadow.
C Fin shadow is below the inclined edge of overhang shadow.
621   AF=AT
      L=1
      GO TO 73

C L is 2, horizontal edge of overhang shadow - portion above 
C horizontal edge not in overhang shadow is fin shadow.
622   AREA1=FL*(T1-A)-AREAO

C Reset to calculate fin shadow below horizontal edge 
C of overhang shadow.
      AF=T1-A+AF1
      H=H+AF1-AF

C Shadow of fin (K=1 on glass, K=2 on vertical projection shadow).
73    AB=BF*TCETA
      UG=(FL+BF)*TCETA
      DE=(H+AF)/TCETA
      DJ=CX/TCETA
      IF(FM-BF)69,69,38
38    IF(AB-AF)39,50,50
39    IF(UG-AF)48,48,40
40    IF(T-AF)47,47,41
41    IF(UG-(H+AF))44,44,42
42    IF(T-(H+AF))91,80,80

C Fin 9.
80    AREA1=H*((AF+H*0.5)/TCETA-BF)+AREA1
      GO TO 58
44    IF(FM-(FL+BF))91,89,89

C Fin 8.
89    AREA1=H*FL-(UG-AF)**2/TCETA*0.5+AREA1
      GO TO 58

C Fin 7.
91    AREA1=(FM-BF)*H-(T-AF)**2/TCETA*0.5+AREA1
      GO TO 63
48    IF(FM-(FL+BF))47,47,49

C Fin 3.
47    AREA1=H*(FM-BF)+AREA1
      GO TO 63

C Fin 2.
49    AREA1=H*FL+AREA1
      GO TO 58
50    IF(DE-BF)69,69,51
51    IF(UG-(H+AF))55,55,52
52    IF(T-(H+AF))93,94,94

C Fin 6.
94    AREA1=(DE-BF)**2*TCETA*0.5+AREA1
      GO TO 58

C Fin 4.
93    AREA1=(FM-BF)*(H+AF-(T+AB)*0.5)+AREA1
      GO TO 63
55    IF(FM-(FL+BF))93,99,99

C Fin 5.
99    AREA1=FL*(H-(BF+FL*0.5)*TCETA+AF)+AREA1

C Short side fin 'ARSH1','ARSHF'
58    IF(DJ-BF)69,69,59
59    IF(DJ-(FL+BF))61,61,60

C Short 3.
60    ARSH1=-FL*(CX-(BF+FL/2)*TCETA)
      GO TO 69

C Short 4.
61    ARSH1=-(CX-AB)**2/TCETA*0.5
      GO TO 69
63    IF(DJ-BF)69,69,64
64    IF(DJ-FM)61,61,65

C Short 2.
65    ARSH1=-(FM-BF)*(CX-(T+AB)*0.5)
69    GO TO (77,76),K
76    ARSH1=-ARSH1
      AREA1=-AREA1
77    ARSHF=ARSHF+ARSH1
      ARSIF=ARSIF+AREA1
      GO TO (78,68),K
78    IF(AREAV)68,68,72

C Reset parameters to deduct fin shadow overlap on 
C vertical projection shadow.
72    K=2
      AREA1=0.0
      ARSH1=0.0
      BBF=BF
      BF=FM1-B+BF
      IF(BF)186,185,185
186    BF=BBF
185   IF(HT+A-T1-D)87,87,188
188   CX=CX-(HT+A-T1-D)
      IF(CX)85,87,87
85    CX=0.0
87    AF=T1-A+AF
      H=H3
      FL=FL3
      GO TO 73

C Shaded area 'ARSHA'
68    ARSHA=AREAO+AREAV+ARSHF+ARSIF
      SHRAT=(FL1*H1-ARSHA)/(FL1*H1)
      FL=FL1
2000  CONTINUE
      RETURN
      END
