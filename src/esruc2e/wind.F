c Parameters (Described in Specific Subroutines):
c
c Read (IF) or     Dimension       Parameter               Passed to (SR) or      
c Passed from (SR)                                         Written in (OF)
c
c ENVBUILD (IF)    (deg)     AzB (Azimuth of the Building)    CPGEOM (SR)   
c     "            (deg)     AnWind (Wind Direction Angle)       "
c     "             (%)      PAD (Plan Area Density)          CPCALC (SR)   
c     "             (m)      SbH (Surrounding Buildings'
c                                 Height)                     CPGEOM (SR)  
c     "             (m)      BdZ, BdX, BdY (Rough Building
c                                 Dimensions)                 CPGEOM (SR)
c     "             (-)      VeExp (Wind Velocity Profile 
c                                 Exponent)                   CPCALC (SR)   
c CPGEOM (SR)      (deg)     AnbW (Wall Incidence Wind Angle)    "
c     "             (-)      RbH (Relative Building Height)      "
c     "             (-)      FAR (Frontal Aspect Ratio)          "
c     "             (-)      SAR (Side Aspect Ratio)             "
c FACADE (IF)       (-)      iface (Number of a Building's 
c                                 Facade)                        "
c     "             (-)      numelm (Number of Facades'
c                                 Elements)                      "
c     "             (-)      ielm (Number of a Facade's 
c                                 Element)                       "
c     "             (-)      XL (Relative Horizontal Position 
c                                 of a Facade's Element)         "
c     "             (-)      ZH (Relative Vertical Position 
c                                 of a Facade's Element)         "
c CPCALC (SR)       (-)      Cp (Wind Pressure Coefficient)   OUTPUT.CPC (OF)
c COMMON            (-)      *666 (Error Return)
c    "              (-)      KEY (Error Key; it is set to 1 
c                                 or 2 when error is found)
c
c Limits of the Maximum Parameters' Variation Range: 
c          0.0 _< AnWind _< 360.0 
c          -180.0 _< AnbW _< +180.0
c          0.0 _< AzB _< 360.0 
c          0.10 _< VeExp _< 0.45
c          0.0 _< PAD _< 50.0;
c              If FAR or SAR or RbH /= 1, then 0.0 _< PAD _< 12.5   
c          0.0 < SbH; if the considered building is isolated, 
c                     SbH = Building's Height (Routine CPGEOM)
c          0.0 < RbH
c          If 0< PAD <7.5,      then FAR < 6.6 and  SAR < 4.6;
c          If 7.5 < PAD < 10.0, then FAR < 7.8 and SAR < 4.5;
c          If 10. < PAD < 12.5, then FAR < 9.5 and SAR < 4.5
c          0.0 < BdX; 0.0 < BdY; 0.0 < BdZ
c          1 _< iface _< 6
c          1 _< numelm (ielm) _< 2000
c          0.0 _< ZH _< 1.0
c          0.0 _< XL _< 1.0 
c
c Limits of the Confidence Parameters' Variation Range 
c (if different from above limits): 
c          0.10 _< VeEXP _< 0.40
c          0.5 < RbH _< 4.0
c          0.5 < FAR _< 4.0
c          0.5 < SAR _< 4.0 
c          If VeExp /= 0.22, then 0.1 _< ZH _< 0.9
c          If PAD /= 0.0 or RbH /= 1.0 or FAR /= 1.0 or SAR /= 1.0,
c             then 0.07 _< ZH _< 0.93
c
c***********************************************************************
        COMMON /ERROR/ERF
        Common /unit/DOU,KBD,CRT,DIN
        common /silmode/ issilent
        INTEGER KBD,CRT,DIN,DOU,ERF
c Variables for unit numbers to allow for system dependencies.
c KBD - unit number for console input; here at LBL =  5
c CRT - unit number for console output.               6
c DIN - unit number for data input.                   8
c DOU - unit number for output file.                  9
c ERF - unit number for error messages                6

        INTEGER KEY,KEW
        CHARACTER*1  ANS
        CHARACTER*40 desc

        DIMENSION desc(50)

        parameter (maxelm=2000)
        INTEGER iface(maxelm),numelm,ielm,Islope,k,j,h,test(maxelm)
        REAL AnbW,PAD,SbH,VeExp,Phi,BdZ,BdX,BdY,XCO,YCO,ZCO,
     &  FAR(6),SAR(6),AspR(6),XL(maxelm),ZH(maxelm)
        logical issilent, ex

        KBD=5
        CRT=6
        DIN=8
        DOU=9
        ERF=6
        KEY=0
        keyb=0
        KEW=0
        msg=10

        issilent = .false.
        if (issilent) then
          open(msg, file='MSG.ERR')
          write(msg, *) 1
          close(msg)
        end if

C check the existence of the input files
        inquire(file='ENVBUILD', exist = ex)
        if (.not.ex) then
          if (.not.issilent) write(crt, 2200)
          key=1
          go to 666
        end if

C read input file first line
        open (unit=DIN,file='ENVBUILD')
        read (DIN,*,err=31,end=41) PAD,SbH,
     &    BdX,BdY,BdZ,VeExp,Islope,Phi

        do 40 i = 1,maxelm
          read (DIN,*,err=31,end=799) ifc
          read (DIN,*,err=31,end=799) numrow
          do 30 j=1,numrow
            read (DIN,'(I2,2F8.2,A40)',err=31) ielm,XL(ielm),
     &            ZH(ielm),desc(ielm)
            IF (ifc.EQ.1.OR.ifc.EQ.3) THEN
              XL(ielm)=XL(ielm)/BdX
              ZH(ielm)=ZH(ielm)/BdZ
            END IF
            IF (ifc.EQ.2.OR.ifc.EQ.4) THEN
              XL(ielm)=XL(ielm)/BdY
              ZH(ielm)=ZH(ielm)/BdZ
            END IF
            IF (ifc.EQ.5) THEN
              XL(ielm)=XL(ielm)/BdX
              ZH(ielm)=ZH(ielm)/BdY
            END IF
            iface(ielm) = ifc
            numelm = max(numelm,ielm)

30        continue
40      continue
799     continue

        if (SbH.le.0.) then
          if (.not.issilent) then
            write (ERF,*) 'SbH out of range (>0)'
          end if
          KEY=1
          go to 666
        endif   

        if (BdZ.le.0.) then
          if (.not.issilent) then
            write (ERF,*) 'Bdz out of range (>0)'
          end if
          KEY=1
          go to 666
        endif

        if (BdX.le.0.) then
          if (.not.issilent) then
            write (ERF,*) 'BdX out of range (>0)'
          end if
          KEY=1
          go to 666
        endif

        if (BdY.le.0.) then
          if (.not.issilent) then
            write (ERF,*) 'BdY out of range (>0)'
          end if
          KEY=1
          go to 666
        endif

C check PAD limits
        IF (PAD.LT.0..OR.PAD.GT.50.) THEN
          if (.not.issilent) then
            WRITE (ERF,*)  'PAD out of range (0-50)'
          end if
          KEY=1
          go to 666
        ENDIF

C check VeEXP limits
        IF (VeEXP.LT..10.OR.VeEXP.GT..45) THEN
          if (.not.issilent) then
            WRITE (ERF,*)  'VeEXP out of range (.10-.45)'
          end if
          KEY = 1
          GO TO 666       
        ENDIF

C check Islope limits
        IF (Islope.LT.0..OR.Islope.GT.2) THEN
          if (.not.issilent) then
            WRITE (ERF,*)  'Islope out of range (0-2)'
          end if
          KEY = 1
          GO TO 666       
        ENDIF

C check Phi limits
        IF (Phi.LT.0..OR.Phi.GT.50.0) THEN
          if (.not.issilent) then
            WRITE (ERF,*)  'Phi out of range (0-50)'
          end if
          KEY = 1
          GO TO 666   
        ENDIF

C Warning return                                                      
        IF (VeEXP.GT..40) THEN
         if(.not.issilent) then
           WRITE (crt,*)  'VeEXP out of confidence range (.10-.40)'

5          WRITE (crt,2000)
           READ (KBD,2100) ANS
           IF (ANS.EQ. 'N'.OR. ANS .EQ.'n') THEN
             WRITE (erf,*) 'VeEXP out of  confidence range (.10-.40)'
             KEY = 1
             GO TO 666
           ENDIF
           IF (ANS.NE. 'Y'.AND. ANS .NE. 'y') GO TO  5
         end if
        ENDIF

C calculate relative building height, and wind incidence angle, aspect ratios 
C for each facade
C NOTE: Replaced CPGEOM and put the coding into the main part of the 
C       program, since  a big part of CPGEOM is not required by c2e.

        if (SbH.le.0.) then
          if (.not.issilent) then
            write (ERF,*) 'SbH out of range (>0)'
          end if
          KEY=1
          GO TO 666
        end if   

C calculate relative building height
        RbH = BdZ/SbH

C calculate frontal and side aspect ratios for each facade
        k=0
        l=1
        do 55 i=1,4
          k=1-k
          l=1-l
          AspR(i) = k* BdX/BdZ + l* BdY/BdZ

55      continue

        AspR(5) = AspR(1)
        AspR(6) = AspR(1)

        do 20 i=1,6
          FAR(i) = AspR(i) 
          j=i+1
          if (j.gt.5) then
            j=2
          endif
          SAR(i) = AspR(j)
20      continue

C error return
        IF (RbH.LE.0.OR.RbH.GT.8.) THEN
          if (.not.issilent) then
            WRITE (ERF,*)  'RbH out of range (0.-8.)'
          end if
          KEY=1
          GO TO 666
        ENDIF

C error return
        IF (FAR(1).LE.0.) THEN
          if (.not.issilent) then
            WRITE (ERF,*)  'FAR out of range (<0.)'
          end if
          KEY=1
          GO TO 666
        ENDIF

C error return
        IF (SAR(1).LE.0.) THEN
          if (.not.issilent) then
            WRITE (ERF,*)  'SAR out of range (<0.)'
          endif
          KEY=1
          GO TO 666
        ENDIF

C error return
        IF (RbH.NE.1..AND.PAD.GT.12.5) THEN
          if (.not.issilent) then
            WRITE (ERF,*)  'PAD out of range (0-12.5)'
          end if
          KEY=1
          GO TO 666
        ENDIF

C error return
        IF (FAR(1).NE.1..AND.PAD.GT.12.5) THEN
          if (.not.issilent) then
            WRITE (ERF,*)  'PAD out of range (0-12.5)'
          end if
          KEY=1
          go to 666
        ENDIF

C error return
        IF (SAR(1).NE.1..AND.PAD.GT.12.5) THEN
          if (.not.issilent) then
            WRITE (ERF,*)  'PAD out of range (0-12.5)'
          end if
          KEY=1
          GO TO 666
        ENDIF

C RBH Warning return                                                      
        IF (RbH.LT..5.OR.RbH.GT.4.) THEN
          if (.not.issilent) then
            WRITE (crt,*)  'RbH out of confidence range (.5-4.)'
8           WRITE (crt,2000)
            READ (KBD,2100) ANS
            IF (ANS.EQ. 'N'.OR. ANS .EQ.'n') THEN
              WRITE (ERF,*)  'RbH out of  confidence range (.5-4.)'
              KEY = 1
              GO TO 666
            ENDIF
            IF (ANS.NE. 'Y'.AND. ANS .NE. 'y') GO TO  8
          end if
        ENDIF

C FAR Warning return                                                      
        IF (FAR(1).LT..5.OR.FAR(1).GT.4.) THEN
          if (.not.issilent) then
            WRITE (crt,*)  'FAR out of confidence range (.5-4.)'
100         WRITE (crt,2000)
            READ (KBD,2100) ANS
            IF (ANS.EQ. 'N'.OR. ANS .EQ.'n') THEN
              WRITE (ERF,*)  'FAR out of  confidence range (.5-4.)'
              KEY = 1
              GO TO 666
            ENDIF
            IF (ANS.NE. 'Y'.AND. ANS .NE. 'y') GO TO  100
          end if
        ENDIF

C SAR Warning return                                                      
        IF (SAR(1).LT..5.OR.SAR(1).GT.4.) THEN               
          if (.not.issilent) then
            WRITE (crt,*)  'SAR out of confidence range (.5-4.)'
120         WRITE (crt,2000)                                                   
            READ (KBD,2100) ANS                                                 
            IF (ANS.EQ. 'N'.OR. ANS .EQ.'n') THEN                                               
              WRITE (ERF,*)  'SAR out of confidence range (.5-4.)'           
              KEY = 1                                                       
              GO TO 666                                                        
            ENDIF
            IF (ANS.NE. 'Y'.AND. ANS .NE. 'y') GO TO  120
          end if
        ENDIF

        if (.not.issilent) then
          write (crt,1055) 'RbH', RbH
          write (CRT,1060) 'FAR1','FAR2','FAR3','FAR4','FAR5','FAR6'
          write (CRT,1080) (FAR(j),j=1,6)
          write (CRT,1060) 'SAR1','SAR2','SAR3','SAR4','SAR5','SAR6'
          write (CRT,1080) (SAR(j),j=1,6)
        end if  
       

        do 201 k=1,numelm 
          test(k)=0
C XL error return
          if (XL(K).LT.0..OR.XL(K).GT.1.) THEN
            if (.not.issilent) then
              write (crt,*) 'XL out of range (0.0-1.0) for ielm=',k
            end if
            KEY = 1
            CP = 10000.0
            GO TO 45
          ENDIF

C ZH error returns
          if (ZH(K).LT.0..OR.ZH(K).GT.1.) THEN
            if (.not.issilent) then
              write (crt,*) 'ZH out of range (0.0-1.0) for ielm=',k
            end if
            KEY = 1
            CP = 10000.0
            GO TO 45
          ENDIF
          if (iface(k).lt.5) then
            if (VeEXP.NE..22.AND.ZH(K).LT..1.OR.VeEXP.NE..22.AND.ZH(k)
     &         .GT..9) then
              if (issilent) then
                ans = 'Y'
              else
                write (crt,*) 'ZH out of range (.1-.9) for ielm=',k
                WRITE (crt,2020)
                READ (kbd,2100) ANS
                IF (ANS.EQ. 'N'.OR. ANS .EQ.'n') test(k)=1 
              end if
              if(zh(K).lt..1)zh(K)=.1
              if (zh(K).gt..9)zh(K)=.9
            endif

            IF (ZH(k).lt..07.or.ZH(k).gt..93) THEN
              ifc=iface(k)
              IF(RbH.NE.1..OR.PAD.ne.0..or.SAR(ifc).ne.1..or.
     &           FAR(ifc).ne.1.) THEN
                if (issilent) then
                  ans= 'y'
                else
                  WRITE (crt,*) 
     &             'ZH out of range (.07-.93) for element =',k
                  WRITE (crt,2020)
                  READ (KBD,2100) ANS
                  if (ANS.EQ. 'N'.OR. ANS .EQ.'n') test(k)=1                                
                end if
                if(zh(k).lt..07)zh(K)=.07
                if (zh(K).gt..93)zh(K)=.93
              ENDIF
            ENDIF
          else
            if (ZH(K).LT..07.OR.ZH(k).gt..93) THEN
              if (issilent) then
                ans= 'y'
              else
                WRITE
     &          (crt,*) 'ZH out of confidence range  for element =',k
                WRITE (crt,2020)
                READ (KBD,2100) ANS
                IF (ANS.EQ. 'N'.OR. ANS .EQ.'n') test(k)=1 
              end if
            endif
          end if


 
C OPEN RESULT FILE (CP database)
          open (unit=DOU, file='c2e.cpdb')
          IF (k.EQ.1) THEN
            write (dou,'(a)') '# Pressure Coefficient sets via CPCALC'
            write (dou,'(a)') '# '
            write (dou,'(a)') '# Building dimensions'
            write (dou,1120)  '# Building length=',bdx
            write (dou,1120)  '#  Building width=',bdy
            write (dou,1120)  '#  Building hight=',bdz
            write (dou,1130)  '# Roof type=',Islope,
     &  '   NOTE: 0=flat roof  1=single slope roof  2=gable roof'
            write (dou,1140)  '# Roof tilt angle=',Phi
            write (dou,'(a)') '# '
            write (dou,'(a)') '# Other parameters required by CPCALC'
            write (dou,1150)  '# Plan area density=',pad
            write (dou,1120)  '# Surrounding building hight=',sbh
            write (dou,1120)  '# Terrain roughness=', veexp
            write (dou,'(a)') '# '
            write (dou,'(a)') '# Parameters calculated by CPCALC'
            write (dou,1120)  '# Relative building hight=',rbh
            write (dou,'(a)') '# Front aspect ratios'
            write (dou,1120)  '#  Long facade and roof=',far(1)
            write (dou,1120)  '#          short facade=',far(3)
            write (dou,'(a)') '# Side aspect ratios'
            write (dou,1120) '#  Long facade and roof=',sar(1)
            write (dou,1120) '#          Short facade=',sar(3)       
            write (dou,'(a)') '# '
            write (dou,'(a)') '# '
            write (dou,'(i3,a)') numelm,'  # Number of sets'

            do 546 h=1,numelm

              IF (iface(h).EQ.1.OR.iface(h).EQ.3) THEN
                XCO=XL(h)*BDX
                ZCO=ZH(h)*BDZ
                write (dou,1160) '# Set ',-h,' is located at facade ',
     &                      iface(h),' with XL=',XCO,' and ZH=',ZCO

              ELSEIF (iface(h).EQ.2.OR.iface(h).EQ.4) THEN
                XCO=XL(h)*BDY
                ZCO=ZH(h)*BDZ
                write (dou,1160) '# Set ',-h,' is located at facade ',
     &                      iface(h),' with XL=',XCO,' and ZH=',ZCO

              ELSE 
                XCO=XL(h)*BDX
                YCO=ZH(h)*BDY
                write (dou,1170) '# Set ',-1*h, 
     &                     ' is located at the roof with XL=',XCO,
     &                     ' and YL=',YCO
              END IF 

 546        CONTINUE

1160        format (a,I3,a,I2,2(a,F5.1))
1170        format (a,I3,2(a,F5.1))

            write (dou,'(a)') '# '
            write (dou,'(a)') '# ' 
          END IF 

 
C calculate CP values, calling CPCALC for each element in turn
C and 16 different wind angles 
          AnbW=0.   
          do 200 j=1,16       
            AnbW=(j-1)*22.5
            if (AnbW.ge.180) AnbW=AnbW-360.

C calculate CP for the nth facade element (ielm) error return
            if (iface(k).lt.1.or.iface(k).gt.6) then
              if (.not.issilent) then
                write (ERF,*) 'iface out of range (1-6)'
             end if
             KEY=1
             goto 666
           endif   

C vertical facades: windward side
           IF (iface(k).le.4.and.AnbW.gt.-90..and.AnbW.lt.90.) THEN
             CALL CPVEWI (VeEXP,PAD,RbH,
     &         FAR(iface(k)),SAR(iface(k)),AnbW,
     &         ZH(k),XL(k),CP,*666,KEY)

C vertical facades: leeward side
           ELSEIF (iface(k).le.4.and.AnbW.le.-90.
     &         .or.iface(k).le.4.and.AnbW.ge.90.) THEN
             CALL CPVELE (VeEXP,PAD,RbH,FAR(iface(k)),
     &         SAR(iface(k)),AnbW,ZH(k),XL(k),CP,*666,KEY)

C horizontal facades: roof
            ELSEIF (iface(k).eq.5) then
              call CPROOF (VeEXP,PAD,RbH,FAR(iface(k)),
     &          SAR(iface(k)),AnbW,ZH(k),XL(k),Islope,Phi,CP,*666,KEY)      

C horizontal facades: floor 
C NOTE: No values get calculated, since required code is not available
            ELSEIF (iface(k).eq.6) then
              CP = 999.0
              if (.not.issilent) then
                write (ERF,*) 'floor facade: no Cp values'
              end if
              KEY=1
            ENDIF

            If (test(k).EQ.1)cp=99999

            write (DOU,66) cp
            if (j.eq.10) write (DOU,'(a)') '  '
            if (j.eq.16) write (DOU,69)' ',desc(k)


 200      continue
 201    continue
        write (CRT,'(a)') '  ' 
        write (CRT,'(a)') 
     &    'Pressure coefficients successfully calculated'
        write (CRT,'(a)') '  ' 


 45     if (KEY.eq.2) then
          key=1
          IF(iface(K).eq.kew) go to 50
          if (.not.issilent) then
            write (erf,*) 'If 0< PAD <7.5    FAR < 6.6 SAR < 4.6'
            write (erf,*) 'If 7.5 < PAD < 10.   FAR < 7.8 SAR < 4.5'
            write (erf,*) 'If 7.5 < PAD < 12.5   FAR < 9.5 SAR < 4.5'
            write (dou,*) 'If 0< PAD <7.5    FAR < 6.6 SAR < 4.6'
            write (dou,*) 'If 7.5 < PAD < 10.   FAR < 7.8 SAR < 4.5'
            write (dou,*) 'If 7.5 < PAD < 12.5   FAR < 9.5 SAR < 4.5'
          end if
          kew=iface(K)
          endif

50      continue

        if (KEY.ne.0) go to 666
        if (issilent) then
          open(msg, file='MSG.ERR')
          write(msg, *) 0
          close(msg)
        end if
899     go to 9900

31      key=1
        write(crt,'(a)')' Error when reading the file ENVBUILD'
        go to 666
41      key=1
        write(crt,'(a)')' The  file ENVBUILD is not complete'
        go to 666

C the program comes here if an error is detected if error messages went to 
C the screen (6) then stop now
666     continue
        if (issilent) then
          open(msg, file='MSG.ERR')
           if (keyb.eq.1) then
              write(msg, *) 3
           else
              write(msg, *) 2
           end if
          close(msg)
        end if
        if (erf.eq.6) then
          go to 9900

C Otherwise, rewind the error file and print it
        else if (.not.issilent) then
          rewind ERF
60        read (ERF,1110,end=899) ermsg
          print *, ermsg
          go to 60
        endif

C FORMATS

69    format (a,a)
66    format (1x,f6.3,$)

1055    format (10x,a3,6x,f3.1)
1060    format (/,10x,a4,5x,a4,5x,a4,5x,a4,5x,a4,5x,a4)
1080    format (10x,f5.2,5x,f5.2,5x,f5.2,5x,f5.2,5x,f5.2,5x,f5.2)
1110    format (a80) 
1120    format (a,F6.2)
1130    format (a,I2,a)
1140    format (a,I2)
1150    format (a,F5.1)
2000    format (1x,'do you want to continue? (N/Y) ',$)
2020    format (1x,'Do you want to assign to the element the CP value
     *of the upper /lower limit ? ',$)
2100    FORMAT (A1)
2200    format (1x, 'The input file ENVBUILD has to be created.')

9900    continue
        end
      
