C prcoef.f contains the main files that are called by WIND to calculate the
C pressure coefficients in in the surrounding of a building
C 
C CPVEWI
C CPVELE
C CPROOF

	SUBROUTINE CPVEWI (VeEXP,PAD,RbH,FAR,SAR,AnbW,ZH,XL,CP,*,KEY)
c************************************************************************
c Purpose: this routine calculates the Surface Wind Pressure Coefficient
c          (CP) for any facade element of a windward wall.
c
c Date   : As a Module of COMIS (# 4.3.1.2., TG#3, mxg) March 1,1989
c Changes: November 2, 1989 (added coefficients and subroutines for Aspect Ratios)
c          December 1, 1989 (deleted fitting equation coefficients as pass parameters) 
c
c Date   : As a Module of CPCALC-PC, November 1, 1992
c          (Changed Error Message Parameter from UNIX Fortran - RETURN1 - 
c          to MS-DOS Fortran - *)
c
c Date   : As a Module of WIND1, January 6, 1994
c          (Transferred limits for XL and ZH to main routine)
c
c Pass parameters:
c
c IO # Name    unit         description                       ref.value
c
c I  1 VeEXP   (-)   Wind Velocity Profile Exponent              (.22)
c I  2 PAD     (-)   Plan Area Density                            (0) 
c I  3 RbH     (-)   Relative Height of the Building              (1)
c I  4 FAR     (-)   Frontal Aspect Ratio for a facade            (1)
c I  5 SAR     (-)   Side Aspect Ratio for a facade               (1)
c I  6 AnbW    (deg) Wind incident angle to a facade              (0)
c I  7 ZH      (-)   Relative vertical position of a facade elm.  (-)
c I  8 XL      (-)   Relative horiz. position of a facade elm.   (0.5)
c O  9 CP      (-)   Surface Wind Pressure Coefficient           (f(ZH,XL=.5))
c                    at a facade element
c O 10 *       (-)   Error return
c O 11 KEY     (-)   Error key; it is set to 1 if error is found
c
c Example:
c CALL CPVEWI (.28, 7.5, 2., 1.5, 2., 45., .65, .35, .39, *999, 0)
c***********************************************************************
        COMMON /ERROR/ERF
        INTEGER erf
        INTEGER KEY
 
        REAL VeEXP,PAD,RbH,FAR,SAR,AnbW,ZH,XL
        REAL CPREF,COBOL,COPAD,COREL,COFAR,COSAR,COHOR
        REAL CP
   
c calculate the reference value of CP(zh)
        CPREF = -2.381082*ZH**3.+2.89756*ZH**2.
     &  -.774649*ZH+0.745543

c calculate the correction coefficient for the Boundary Layer
        CALL CPBOUL (ZH,VeEXP,COBOL,*999,KEY)

c calculate the correction coefficient for the Surrounding Area Density
        CALL CPDENS (ZH,PAD,COPAD,*999,KEY)
   
c calculate the correction coefficient for the Relative Height
        CALL CPRELH (ZH,PAD,RbH,COREL,*999,KEY)
        COREL = COREL/RbH**(VeEXP*2.)

c calculate the correction coefficients for the Frontal Aspect Ratio
        CALL CPASPF (ZH,FAR,PAD,COFAR,*999,KEY)

c calculate the correction coefficients for the Side Aspect Ratio
        CALL CPASPS (ZH,SAR,PAD,COSAR,*999,KEY)

c calculate the correction coefficient for the Horizontal Distribution
c in relation to the Wind Angle
        CALL CPHORG (ZH,AnbW,XL,COHOR)

c calculate the CP value for the surface point(link) on the wall
        CP = CPREF*COBOL*COPAD*COREL*COFAR*COSAR*COHOR
 
999     continue
        RETURN
        END


        SUBROUTINE CPVELE (VeEXP,PAD,RbH,FAR,SAR,AnbW,ZH,XL,CP,*,KEY)
c  pass parameter  # =        1   2   3   4   5    
c        6   7 8   9   10    11
c************************************************************************
c Purpose: this routine calculates the Surface Wind Pressure Coefficient
c          (CP) for any facade element of a leeward wall.
c
c Module : # 4.3.1.2., TG#3, mxg/March 1,1989
c Changes: January 29, 1990 (built CPVELE from CPVEWI)
c          December 1, 1989 (deleted fitting equation coefficients as pass parameters) 
c Limits : 0. _< XL _< 1. (unless different limits are set in the parameter subroutines)
c          0. _< ZH _< 1. (unless different limits are set in the parameter subroutines)
c          for limits of the other parameters see the relevant subroutines
c
c Pass parameters:
c
c IO # Name    unit         description                       ref.value
c
c I  1 VeEXP   (-)   Wind Velocity Profile Exponent              (.22)
c I  2 PAD     (-)   Plan Area Density                            (0) 
c I  3 RbH     (-)   Relative Height of the Building              (1)
c I  4 FAR     (-)   Frontal Aspect Ratio for a facade            (1)
c I  5 SAR     (-)   Side Aspect Ratio for a facade               (1)
c I  6 AnbW    (deg) Wind incident angle to a facade              (0)
c I  7 ZH      (-)   Relative vertical position of a facade elm.  (-)
c I  8 XL      (-)   Relative horiz. position of a facade elm.   (0.5)
c O  9 CP      (-)   Surface Wind Pressure Coefficient           (f(ZH,XL=.5))
c                    at a facade element
c O 10 RETURN1 (-)   Error return
c O 11 KEY     (-)   Error key; it is set to 1 if error is found
c
c ERROR RETURN IF    XL and ZH out of limits
c
c Example:
c CALL CPVELE (.28, 7.5, 2., 2., 1.5, 135.,.6, .5, -.07, 999, 0)
c***********************************************************************
        COMMON /ERROR/ERF
        common /silmode/ issilent
        logical issilent
        INTEGER erf
        INTEGER KEY
 
        REAL VeEXP,PAD,RbH,FAR,SAR,AnbW,ZH,XL
        REAL CPREF,COBOL,COPAD,COREL,COFAR,COSAR,COHOR
        REAL CP
       
c error return
        IF (XL.lt.0..or.XL.gt.1.) then
          if (.not.issilent) then
             WRITE (ERF,*)  'XL out of range (0.- 1.)'
          end if
          KEY=1
          RETURN 1
        ENDIF 
        IF (ZH.lt.0..or.ZH.gt.1.) then
           if (.not.issilent) then
             WRITE (ERF,*)  'ZH out of range (0.- 1.)'
           end if
           KEY=1
           RETURN 1
        ENDIF 

c calculate the reference value of CP(zh)
        CPREF = -0.079239*ZH**3 +0.542317*ZH**2 -0.496769*ZH -0.331533

c calculate the correction coefficient for the Boundary Layer
        CALL CPBOLL (ZH,VeEXP,COBOL,*999,KEY)

c calculate the correction coefficient for the Surrounding Area Density
        CALL CPDENL (ZH,PAD,COPAD,*999,KEY)
   
c calculate the correction coefficient for the Relative Height
        CALL CPRELL (ZH,PAD,RbH,COREL,*999,KEY)
        COREL = COREL/RbH**(VeEXP*2.)

c calculate the correction coefficients for the Frontal Aspect Ratio
        CALL CPFARL (ZH,FAR,PAD,COFAR,*999,KEY)

c calculate the correction coefficients for the Side Aspect Ratio
        CALL CPSARL (ZH,SAR,PAD,COSAR,*999,KEY)

c calculate the correction coefficient for the Horizontal Distribution
c in relation to the Wind Angle
        CALL CPHORL (ZH,AnbW,XL,COHOR)
        if (ZH.lt..2) COHOR = COHOR*.86     
        if (ZH.ge..2.and.ZH.lt..4) COHOR = COHOR*.83
        if (ZH.ge..4.and.ZH.lt..6) COHOR = COHOR*.73
        if (ZH.ge..6.and.ZH.lt..8) COHOR = COHOR*.64
        if (ZH.ge..8) COHOR = COHOR*.61

c calculate the CP value for the surface point(link) on the wall
        CP = CPREF*COBOL*COPAD*COREL*COFAR*COSAR*COHOR

999     continue
        RETURN
        END

        SUBROUTINE CPROOF (VeEXP,PAD,RbH,FAR,SAR,AnbW,ZH,XL,Islope,Phi,
     &CP,*,KEY)
c************************************************************************
c Purpose: this routine calculates the Surface Wind Pressure Coefficient
c          (CP) for any element on a flat or tilted roof of a block-shaped
c          building.
c
c Date   : As a Module of WIND1, January 5, 1994
c
c Pass parameters:
c
c IO # Name    unit         description                       ref.value
c
c I  1 VeEXP   (-)   Wind Velocity Profile Exponent              (.22)
c I  2 PAD     (-)   Plan Area Density                            (0) 
c I  3 RbH     (-)   Relative Height of the Building              (1)
c I  4 FAR     (-)   Frontal Aspect Ratio for facade 1            (1)
c I  5 SAR     (-)   Side Aspect Ratio for facade 1               (1)
c I  6 AnbW    (deg) Wind incident angle from the normal to 
c                    facade 1                                     (0)
c I  7 ZH      (-)   Relative y-position of an element on the  
c                    roof (relative distance from facade 1)       (-)
c I  8 XL      (-)   Relative x-position of an element on the    
c                    roof (relative distance from facade 4)       (0.5)
c O  9 CP      (-)   Surface Wind Pressure Coefficient           
c                    at a roof's element                       (f(ZH,XL=.5))
c O 10 *       (-)   Error return
c O 11 KEY     (-)   Error key; it is set to 1 if error is found
c
c Example:
c CALL CPROOF (.28, 7.5, 2., 1.5, 2., 45., .65, .35, .39, *999, 0)
c***********************************************************************
        COMMON /ERROR/ERF
        common /silmode/ issilent
        logical issilent
        INTEGER erf
        INTEGER KEY ,Islope
        parameter (Nmax=10,lmax=4)
 
        REAL VeEXP,PAD,RbH,FAR,SAR,AnbW,ZH,XL,Phi,ang
        REAL CPREF,COBOL,COPAD,COREL,COFAR,COSAR,COHOR
        REAL CP,polD(Nmax,Lmax)
c coefficients for a 3rd order polinomial equation yelding
c******** CFANG as fitting function of teta
        data polD/1.,1.,1.,1.,1.,-6.07229,4.41429,5.09091,37.2778,
     & 19.3889,
     & +0.0117805,-0.00515873,-0.0227273,+0.012037,-0.0277778,+0.150602,
     & - 0.0859524, - 0.0681818 ,- 0.813272 ,- 0.520988,
     & - 0.000461847,-0.000309524,+0.000252525,-0.000493827,+0.00148148,
     & - 0.0011245,+ 0.000587302,+0.000252525,+0.00583333,+ 0.00472222,
     & + 2.75175e-6,+ 3.35097e-6,-1.99646e-18,+ 5.14403e-6,
     & - 0.0000123457,2.67738E-06,-1.32275e-6,+1.49782e-16,
     & -0.0000130316,- 0.0000126886  /

c error return
        IF (XL.lt.0..or.XL.gt.1.) then
          if (.not.issilent) then
             WRITE (ERF,*)  'XL out of range (0.- 1.)'
          end if
          KEY=1
          RETURN 1
        ENDIF 
        IF (ZH.lt.0..or.ZH.gt.1.) then
          if (.not.issilent) then
            WRITE (ERF,*)  'ZH out of range (0.- 1.)'
          end if
          KEY=1
          RETURN 1
        ENDIF

        teta=-AnbW
   
c calculate the reference value of CP(zh)
        CPREF = -0.784174-1.26264*ZH+9.58738*ZH**2-13.2731*ZH**3
     &   +5.56465*ZH**4

        if (abs(teta).eq.0.) then
          CFang=1.0
        else
          y=zh
          ang=abs(teta)

c calculate the list number of the coefficients (polD)
c for the polinomial equation
              K= INT(y*5.+1.)
              IF (K.gt.5) THEN
                 K=5
              ENDIF
          if (ang.gt.90.0 ) then
             k=K+5
          endif
c calculate CFang
          CFang = 0
          DO 10 J=1,4
          CFang = CFang + polD(K,J)*ang**(j-1)
10        CONTINUE
        endif
c calculate the correction coefficient for the Boundary Layer
        CALL BLROOF (ZH,VeEXP,teta,COBOL,*999,KEY)
        COBOL=COBOL*CFang

c calculate the correction coefficient for the Surrounding Area Density
        CALL DENROOF (ZH,PAD,COPAD,*999,KEY)
   
c calculate the correction coefficient for the Relative Height
        CALL RELHROOF (ZH,PAD,RbH,COREL,*999,KEY)
             COREL = COREL/RbH**(VeEXP*2.)

c calculate the correction coefficients for the Frontal Aspect Ratio
        CALL FARROOF (ZH,FAR,PAD,COFAR,*999,KEY)

c calculate the correction coefficients for the Side Aspect Ratio
        CALL SARROOF (ZH,SAR,PAD,COSAR,*999,KEY)

c calculate the correction coefficient for the Horizontal Distribution
c in relation to the Wind Angle

         IF(teta.LT.0) THEN
             teta=abs(teta)
             XL=1.0-XL
         ENDIF
         IF (Islope.EQ.0) THEN
            y=ZH
            If(teta.gt.90.0)then
                y=1.0-y
                teta=180.0-teta
            endif
            CALL HORROOF (y,teta,XL,COHOR)
         ELSEIF (Islope.EQ.1) then
            IF (XL.eq.0.5) then
                 CALL  PHISING (ZH,teta,Phi,COHOR)
            ELSE
                 CALL  PHIXSIN (ZH,teta,Phi,XL,COHOR)

            ENDIF
         ELSE
            y=ZH
            If(teta.gt.90.0)then
               y=1.0-y
               teta=180.0-teta
            endif
            IF (XL.eq.0.5) then
                 CALL  PHIDOUB (y,teta,Phi,COHOR)
            ELSE
                 CALL  PHIXDOU (y,teta,Phi,XL,COHOR)
            ENDIF
         ENDIF
c calculate the CP value for the surface point(link) on the wall
        CP = CPREF*COBOL*COPAD*COREL*COFAR*COSAR*COHOR

999     continue
        RETURN
        END
