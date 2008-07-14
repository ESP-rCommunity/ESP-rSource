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

C This file contains the routines for calculating surface convection.

C     HTBUOY  Calculates HTCs using buoyancy-driven flow correlations.
C     HTCEXT  Calculates HTCs for forced flow on exterior surfaces.
C     SELCOR1 Selects convection approach based on simulation toggle.


C ******************** HTBUOY ********************
C HTBUOY calculates convection coefficients for interior surfaces using
C empirical correlations.

C A number of empirical correlations and fixed values are currently supported:

C The Alamdari & Hammond correlations are from:
C Alamdari F and Hammond G P (1983), `Improved Data Correlations for
C Buoyancy-Driven Convection in Rooms', Building Services Engineering Research
C and Technology, 4(3) 106-12.

C The Khalifa & Marshall correlations are from:
C Khalifa A.J.N. and Marshall R.H. (1990), `Validation of Heat Transfer
C Coefficients on Interior Building Surfaces Using a Real-Sized Indoor Test
C Cell', Int. J. Heat and Mass Transfer, 33 (10) 2219-2236.

C The values for the Halcrow high and low method are from 
C 'Report on heat transfer at internal building surfaces', 
C Department of Energy, ref: ETSU S 1193-P1

C The CIBSE values are from the 1988 Guide (5th edition) Table A3.4.

C The CEN values are from CEN/TC89/WG6 N.225 / JWG-N79, Thermal Performance
C of Buildings, Room Cooling Load Calculation, General Criteria for a
C Simplified Calculation Method, Section 6.2.2, December 1995.

C The Fisher `thesis' correlations are from:
C Fisher D.E. (1995), An Experimental Investigation of Mixed Convection Heat
C Transfer in a Rectangular Enclosure, PhD Thesis, University of Illinois,
C Urbana USA.

C The Fisher & Pedersen `ASHRAE' correlations are from:
C Fisher D.E. and Pedersen C.O. (1997), `Convective Heat Transfer
C in Building Energy and Thermal Load Calculations', ASHRAE Transactions,
C 103 (2) 137-148.

C The mixed convection model is described in:
C Beausoleil-Morrison I. (1999), `Modelling Mixed Convection Heat
C Transfer at Internal Building Services', Building Simulation '99.

C The Awbi and Hatton correlations are from:
C Awbi H.B. and Hatton A. (1999), `Natural Convection from Heated
C Room Surfaces', Energy and Buildings, 30 233-244.

C The ISO 15099:2003 correlations.

      SUBROUTINE HTBUOY(HC,ICOR,ICOMP,ISUR,DT,HEIGHT,APRAT)

#include "building.h"
#include "net_flow.h"
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/TC/ITC,ICNT
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/PREC2/VOL(MCOM)
      COMMON/Fisher/Vdot(MCOM),SAT(MCOM),ADJREF(MCOM,MS),DELTAR(MCOM)
      COMMON/HCcomb/iAandH,iassist
      COMMON/adaptHC/HCctl5
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)

C Commons to support vertical channel hc regime.
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      COMMON/VERTC/IAORZ,CWIDTH,CHEIGHT

C Commons added for ISO 15099 algorithm
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)

      CHARACTER LAPROB*72

      character outs*124
      logical dotrace

      real veloc

      DOUBLE PRECISION NUAIR,LAMBAIR,RHOAIR,CPAIR,TFILM,BETA
      DOUBLE PRECISION NUSS,RAS,vel2,CNUS
      DOUBLE PRECISION FLIN,FLUT

C Trace output. If there is a zone htc file note that data may
C be superceeded by user specifications.
      dotrace=.false.
      IF(ITC.LE.0.OR.NSINC.LT.ITC)goto 9999
      IF(ITRACE(6).EQ.0.OR.NSINC.GT.ITCF)goto  9999
      IF(IZNTRC(ICOMP).NE.1)goto 9999 ! check if zone output is rq'd
      IF( IHC(ICOMP).EQ.1 ) THEN
        write(outs,'(A,I4,a,i2,a,i2,a)')
     &    ' Subroutine HTBUOY  Trace output',ICNT,
     &    ' for zone ',icomp,' & surface ',isur,
     &    ' (data may be superceeded by the zone convective regime).'
      else
        write(outs,'(A,I4,a,i2,a,i2)')
     &    ' Subroutine HTBUOY  Trace output',ICNT,
     &    ' for zone ',icomp,' & surface ',isur
      endif
      call edisp(itu,outs)
      CALL DAYCLK(IDYP,BTIMEF,ITU)
      dotrace=.true.
 9999 continue

      IF(ICOR.EQ.1)THEN

C######   VERTICAL SURFACES   ##################################################
C Alamdari and Hammond.
        HC=((1.50*((DT/HEIGHT)**0.25))**6.0
     &     +(1.23*((DT)**(1.0/3.0)))**6.0)**(1.0/6.0)
        if(dotrace)then
          write(outs,'(A,2F9.3)')' Alamdari: HC & DT',HC,DT
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.2)THEN
C Halcrow, time invariant low.
        HC=1.0
        if(dotrace)then
          write(outs,'(A,F9.3)')' Halcrow low (vert): HC',HC
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.3)THEN
C Halcrow, time invariant high.
        HC=6.0
        if(dotrace)then
          write(outs,'(A,F9.3)')' Halcrow high (vert): HC',HC
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.4)THEN
C CIBSE
        HC=3.0
        if(dotrace)then
          write(outs,'(A,F9.3)')' CIBSE (vert): HC',HC
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.5)then

C Khalifa & Marshall correlation for walls for the following
C room configurations (Table 2, eq.6 of K&M paper):
C 1) radiator located under window;
C 2) room heated by heated vertical wall, but NOT applicable for
C    the heated surface.
        HC=2.30*(DT**0.24)
        if(dotrace)then
          write(outs,'(A,2F9.3)')
     &      ' Khalifa & Marshall Tbl 2 eq 6 (vert): HC & DT',HC,DT
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.6)THEN

C Khalifa & Marshall correlation for the following
C room configurations (Table 2, eq.3 of K&M paper):
C 1) fan heater in room, surfaces not opposite fan;
C 2) room with floor heating;
C 3) radiator in room, but radiator NOT located under a window,
C    surface under consideration NOT next to radiator.
        HC = 2.07*(DT**0.23)
        if(dotrace)then
          write(outs,'(A,2F9.3)')
     &      ' Khalifa & Marshall Tbl 2 eq 3 (vert): HC & DT',HC,DT
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.7)THEN

C Khalifa & Marshall correlation for the following
C room configuration (Table 2, eq.5 of K&M paper):
C 1) radiator in room, but radiator NOT located under a window,
C    surface under consideration located next to radiator.
        HC = 1.98*(DT**0.32)
        if(dotrace)then
          write(outs,'(A,2F9.3)')
     &      ' Khalifa & Marshall Tbl 2 eq 5 (vert): HC & DT',HC,DT
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.8)THEN

C Khalifa & Marshall correlation for the following
C room configuration (Table 2, eq.8 of K&M paper):
C 1) fan heater in room, surfaces opposite fan.
        HC = 2.92*(DT**0.25)
        if(dotrace)then
          write(outs,'(A,2F9.3)')
     &      ' Khalifa & Marshall Tbl 2 eq 8 (vert): HC & DT',HC,DT
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.9)THEN

C CEN
        HC=2.5
        if(dotrace)then
          write(outs,'(A,F9.3)')' CEN (vert): HC ',HC
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.10)THEN

C Fisher & Pedersen `ASHRAE' correlation for radial ceiling diffusers.
        ACH = Vdot(ICOMP)*3600./VOL(ICOMP)
        HCFish = 0.19*ACH**0.8
        HC = HCFish*ADJREF(ICOMP,ISUR)
        if(dotrace)then
          write(outs,'(A,3F9.3)')
     &    ' Fisher ASHRAE radial ceil: HC & HCFish & ACH',HC,HCFish,ACH
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.11)THEN

C Fisher `thesis' correlation for radial ceiling diffusers.
        ACH = Vdot(ICOMP)*3600./VOL(ICOMP)
        HCFish = -0.199 + 0.190*ACH**0.8
        HC = HCFish*ADJREF(ICOMP,ISUR)
        if(dotrace)then
          write(outs,'(A,3F9.3)')
     &    ' Fisher thesis radial ceil: HC & HCFish & ACH',HC,HCFish,ACH
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.12)THEN

C Fisher `thesis' correlation for free horizontal jet.
        ACH = Vdot(ICOMP)*3600./VOL(ICOMP)
        HCFish = -0.110 + 0.132*ACH**0.8
        HC = HCFish*ADJREF(ICOMP,ISUR)
        if(dotrace)then
          write(outs,'(A,3F9.3)')
     &    ' Fisher thesis free hor jet: HC & HCFish & ACH',HC,HCFish,ACH
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.13)THEN

C Mixed convection model using Fisher `thesis' correlation
C for radial ceiling diffusers and Alamdari & Hammond's correlation.
C Combination of HC depends on whether buoyant and mechanical forces
C assist (iassist=1) or oppose (iassist=-1).
        ACH = Vdot(ICOMP)*3600./VOL(ICOMP)
        HCFish = -0.199 + 0.190*ACH**0.8
        HCforc = HCFish*ADJREF(ICOMP,ISUR)
        HCfree = ((1.50*((DT/HEIGHT)**0.25))**6.0
     &           +(1.23*((DT)**(1.0/3.0)))**6.0)**(1.0/6.0)

C Assisting forces. HCmixed > HCforc and HCmixed > HCfree:
        if(iassist.eq.1) HC = ( HCforc**3. + HCfree**3. )**(1./3.)

C Opposing forces. HCmixed <= max(HCforc,HCfree):
        if(iassist.eq.-1)then
          derate = 0.8
          HCmax = derate*AMAX1( HCforc,HCfree )
          HCblend = (ABS( HCforc**3. - HCfree**3. ))**(1./3.)
          HC = AMAX1(HCblend,HCmax)
        endif
        if(dotrace)then
          write(outs,'(A,5F9.3)')
     &    ' Fisher thesis mixed radial: HC HCFish HCforc HCfree ACH',
     &    HC,HCFish,HCforc,HCfree,ACH
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.14)THEN

C Awbi and Hatton.
C Note that this need to be changed. Rather than operating on the hydraulic
C diameter of the model surface, this needs to operate on the hydraulic
C diameter of the entire surface.
        HC = (1.823*DT**0.293) / (APRAT**0.121)
        if(dotrace)then
          write(outs,'(A,2F9.3)')
     &      ' Awbi & Hatton (vert): HC & DT',HC,DT
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.15)THEN

C Correlations for the ventilated channels
C Molina & Maestre correlation for SOLVENT ventilated channel
        if (IAORZ.gt.0) then
          HC=3.00*(ABS(TFS(ICOMP,ISUR)-TFA(IAORZ)))**0.333

          if(dotrace)then 
            write(outs,'(A,F4.1,A,F4.1)')'Surf Temp* ',TFS(ICOMP,ISUR),
     &      ' AORZ Temp ',TFA(IAORZ)
            call edisp(itu,outs)  
            write(outs,'(A,I2,A,F4.1)')'Just used ICOR ',ICOR,' HC=',HC
            call edisp(itu,outs)
          endif

C If IAORZ=0, use external temperature.
        elseif (IAORZ.eq.0) then
          HC=3.00*(ABS(TFS(ICOMP,ISUR)-TF))**0.333

          if(dotrace)then 
            write(outs,'(A,F4.1,A,F4.1)')'Surf Temp ',TFS(ICOMP,ISUR),
     &        ' Outdoor Temp ',TF
            call edisp(itu,outs)  
            write(outs,'(A,I2,A,F4.1)')'Just used ICOR ',ICOR,' HC=',HC
            call edisp(itu,outs)
          endif 
        else

C Error message IAORZ out of range.
          if(dotrace)then
            write(outs,'(A)')'The origin zone is not valid'
            call edisp(itru,outs)
          endif
        endif
      ELSEIF((ICOR.EQ.16).AND.(IAORZ.EQ.0))THEN

C Bar-Cohen and Rohsenow general correlation for buoyancy driven flow in vertical channels.
C Air entering from outdoors.
C CWIDTH is the Channel width and CHEIGHT the channel height
C RAS is the Rayleigh rumber and CNUSS the Nusselt number
C Air properties fixed at 300 K values.

        RAS=9.8*1/TFA(IAORZ)*ABS(TFS(ICOMP,ISUR)-TF)*
     &      CWIDTH**3*0.707/(1.59E-5)**2
         
        CNUS=(576*(RAS*CWIDTH/CHEIGHT)**(-2)+
     &        2.87*(RAS*CWIDTH/CHEIGHT)**(-0.5))**(-0.5)
         
        HC=real(CNUS)*0.0263/CWIDTH       

        if(dotrace) then
          write(outs,'(A,E10.2,E10.2,F4.1)')'RAS,CNUS,HC ',RAS,CNUS,HC
          call edisp(itu,outs)
        endif
      ELSEIF((ICOR.EQ.16).AND.(IAORZ.GT.0))THEN

C Bar-Cohen and Rohsenow general correlation for buoyancy driven flow in vertical channels.
C Air entering from another building zone.
        RAS=9.81/TFA(IAORZ)*ABS(TFS(ICOMP,ISUR)-TFA(IAORZ))*
     &      CWIDTH**3.0*0.707/(1.59E-5)**2.0
         
        CNUS=(576.0*(RAS*CWIDTH/CHEIGHT)**(-2.0)+
     &        2.87*(RAS*CWIDTH/CHEIGHT)**(-0.5))**(-0.5)
         
        HC=real(CNUS)*0.0263/CWIDTH

        if(dotrace) then
          write(outs,'(A,E10.2,E10.2,E10.2,E10.2,E10.2,E10.2,F4.1)')
     &     'TS,TA,CWIDTH,CHEIGHT,RAS,CNUS,HC ',
     &     TFS(ICOMP,ISUR),TFA(IAORZ),CWIDTH,CHEIGHT,RAS,CNUS,HC
          call edisp(itu,outs)
        endif

      ELSEIF((ICOR.EQ.17).AND.(IAORZ.EQ.0))THEN

C ISO 15099 correlation for buoyancy driven flow in ventilated vertical 
C channels. Eqns. taken from version 2003, sec. 5.3 and eqn. 111.
C CWIDTH is the Channel width and CHEIGHT the channel height. Hereby,
C it is generally assumed the the ventilated vertical channel is split into 
C more than one zone to better model stack effects in an afn.
C PR is the Prandtl number, RAS is the Rayleigh number based on channel 
C width and NUSS the corresponding Nusselt number
C Air properties are fixed to dry air values at 300 K and 101325 Pa.
C
C Air entering from outdoors, i.e. the "lowermost" zone of the vertical
C channel - "forced" velocity is assumed to be zero.
        LAMBAIR=2.63D-02  ! W/(m K)
        RHOAIR=1.174D0     ! kg/m3
        CPAIR=1.007D0       ! kJ/(kg K)
        NUAIR=15.72D-6     ! m2/s
        TFILM=(TFS(ICOMP,ISUR)+TF)/2.0
        BETA=1.0D0/(TFILM+2.7315D02)

        RAS=1000.0*9.81*BETA*ABS(TFS(ICOMP,ISUR)-TF)*
     &      CWIDTH**3.0*RHOAIR*CPAIR/(NUAIR*LAMBAIR)
         
        IF(RAS.le.1.0D+04) THEN
          NUSS=1.0D0+1.7596678D-10*RAS**2.2984755D0
        ELSEIF ((RAS.gt.1.0D+04).AND.(RAS.le.5.0D+04))THEN
          NUSS=0.028154*RAS**0.4134
        ELSE
           NUSS=0.0673838*RAS**0.3333
        ENDIF

C If air is entering from the ambient, it is assumed that the velocity is zero.
C This assumption is based on the approach of most buoyancy heat transfer 
C algorithms according to literature. These do not take a superimposed "forced"
C flow into account, which seems plausible for the "first" or "lowermost" zone
C of a multi-zone stacked channel.

        NUSS=MAX(real(NUSS),(0.242*(real(RAS)*CWIDTH/CHEIGHT)**0.272))

        HC=2.0*real(NUSS*LAMBAIR)/CWIDTH

        if(dotrace) then
          write(outs,'(A,E10.3,E10.3,E10.3,E10.3,E12.4,E12.4,F5.2)')
     &     '15099: TS,Tamb,CWIDTH,CHEIGHT,RAS,NUS,HC ',
     &     TFS(ICOMP,ISUR),TF,CWIDTH,CHEIGHT,real(RAS),real(NUSS),HC
          call edisp(itu,outs)
        endif
      ELSEIF((ICOR.EQ.17).AND.(IAORZ.GT.0))THEN

C ISO 15099 correlation for buoyancy driven flow in vertical channels w/
C air entering from another (lower) building zone.
        LAMBAIR=2.63D-02  ! W/(m K)
        RHOAIR=1.174D0     ! kg/m3
        CPAIR=1.007D0       ! kJ/(kg K)
        NUAIR=15.72D-6     ! m2/s
        veloc=0.0              ! m/s
        TFILM=(TFS(ICOMP,ISUR)+TFA(IAORZ))/2.0
        BETA=1.0/(TFILM+273.15)    ! 1/K

        RAS=1000.0*9.81*BETA*ABS(TFS(ICOMP,ISUR)-TFA(IAORZ))*
     &     CWIDTH**3.0*RHOAIR*CPAIR/(NUAIR*LAMBAIR)
         
        IF(RAS.le.1.0D+04) THEN
          NUSS=1.0+1.7596678D-10*RAS**2.2984755
        ELSEIF ((RAS.gt.1.0D+04).AND.(RAS.le.5.0D+04)) THEN
          NUSS=0.028154*RAS**0.4134
        ELSE
           NUSS=0.0673838*RAS**0.3333
        ENDIF

        NUSS=MAX(real(NUSS),(0.242*(real(RAS)*CWIDTH/CHEIGHT)**0.272))

C Get flow rate through channel from air flow network and calculate average velocity
        IF (IAIRN.ne.0) THEN
          IF ((ICAAS(ICOMP).gt.0).and.(ICAAS(IAORZ).gt.0)) THEN
            call MFSNFL(2,ICAAS(ICOMP),ICAAS(IAORZ),FLIN,FLUT)
            veloc=real(FLIN)*CHEIGHT/3600./VOL(ICOMP)
            if(dotrace) then
                write(outs,'(A,4I5,3E10.3)') 
     &         'ICOMP,ICAAS(ICOMP),IAORZ,ICAAS(IAORZ),Vol,veloc,FLIN ',
     &         ICOMP,ICAAS(ICOMP),IAORZ,ICAAS(IAORZ),
     &         VOL(ICOMP),veloc,REAL(FLIN)
                call edisp(itu,outs)
             endif
          ENDIF
        ENDIF

C eqn. 111 from ISO 15099:2003
        HC=2.0*real(NUSS*LAMBAIR)/CWIDTH + 4.0*veloc

        if(dotrace) then
          write(outs,'(A,4E10.3,E12.4,E12.4,F5.2)')
     &     '15099: TS,TA,CWIDTH,CHEIGHT,RAS,NUS,HC',
     &     TFS(ICOMP,ISUR),TFA(IAORZ),CWIDTH,CHEIGHT,
     &     real(RAS),real(NUSS),HC
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.30)THEN

C######   WINDOWS   ############################################################
C Khalifa & Marshall correlation for windows when a radiator is located
C under the window (Table 2, eq.9 of K&M paper).
        HC = 8.07*(DT**0.11)
        if(dotrace) then
          write(outs,'(A,2F9.3)')
     &    ' Khalifa for windows (radiator under): HC & DT',HC,DT
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.31)THEN

C Khalifa & Marshall correlation for windows when there is NOT a radiator
C located under the window (Table 2, eq.10 of K&M paper).
        HC = 7.61*(DT**0.06)
        if(dotrace)then
          write(outs,'(A,2F10.3)')
     &    ' Khalifa for windows (no radiator under): HC & DT',HC,DT
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.50)then

C######   BUOYANT FLOW ABOVE OR BELOW HORIZONTAL SURFACES  #####################
C Alamdari and Hammond.
        HC=((1.4*((DT/APRAT)**0.25))**6.0
     &     +(1.63*(DT**(1.0/3.0)))**6.0)**(1.0/6.0)
        if(dotrace) then
          write(outs,'(A,2F9.3)')
     &    ' Alamdari (icor 50 horiz): HC & DT',HC,DT
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.51)then

C Halcrow, time invariant low.
        HC=1.0
        if(dotrace) then
          write(outs,'(A,F9.3)')' Halcrow low (horiz): HC',HC
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.52)then

C Halcrow, time invariant high.
        HC=6.0
        if(dotrace) then
          write(outs,'(A,F9.3)')' Halcrow high (vert): HC',HC
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.53)then

C CIBSE
        HC=4.3
        if(dotrace) then
          write(outs,'(A,F9.3)')' CIBSE (horiz): HC',HC
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.54)then

C CEN
        HC=5.0
        if(dotrace) then
          write(outs,'(A,F9.3)')' CEN (horiz): HC ',HC
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.55)then

C Awbi and Hatton.
C Note that this need to be changed. Rather than operating on the hydraulic
C diameter of the model surface, this needs to operate on the hydraulic
C diameter of the entire surface.
        HC = (2.175*DT**0.308) / (APRAT**0.076)
        if(dotrace) then
          write(outs,'(A,2F9.3)')
     &      ' Awbi & Hatton (horiz): HC & DT',HC,DT
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.60)then

C######   BUOYANT FLOW UNDER CEILINGS ##########################################
C Khalifa & Marshall correlation for ceilings for the following
C room configurations (Table 2, eq.7 of K&M paper):
C 1) radiator located under window;
C 2) room heated by heated vertical wall.
        HC=3.10*(DT**0.17)
        if(dotrace) then
          write(outs,'(A,2F9.3)')
     &      ' Khalifa & Marshall Tbl 2 eq 7 (ceil): HC & DT',HC,DT
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.61)THEN

C Khalifa & Marshall correlation for ceilings for the following
C room configurations (Table 2, eq.4 of K&M paper):
C 1) fan heater in room;
C 2) room with floor heating;
C 3) radiator in room, but radiator not located under a window.
        HC = 2.72*(DT**0.13)
        if(dotrace) then
          write(outs,'(A,2F9.3)')
     &      ' Khalifa & Marshall Tbl 2 eq 4 (ceil): HC & DT',HC,DT
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.70)then

C######   STABLY STRATIFIED CONVECTION ABOVE OR BELOW HORIZONTAL SURFACES ######
C Alamdari and Hammond.
        HC=0.60*((DT/(APRAT**2.0))**0.2)
        if(dotrace) then
          write(outs,'(A,2F9.3)')' Alamdari (stratified): HC & DT',HC,DT
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.71)THEN

C Halcrow, time invariant low.
        HC=0.1
        if(dotrace) then
          write(outs,'(A,F9.3)')' Halcrow low (stratified): HC',HC
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.72)THEN

C Halcrow, time invariant high.
        HC=1.2
        if(dotrace) then
          write(outs,'(A,F9.3)')' Halcrow high (stratified): HC',HC
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.73)THEN

C CIBSE
        HC=1.5
        if(dotrace) then
          write(outs,'(A,F9.3)')' CIBSE (stratified): HC',HC
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.74)THEN

C CEN
        HC=0.7
        if(dotrace) then
          write(outs,'(A,F9.3)')' CEN (stratified): HC ',HC
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.80)THEN

C######   FORCED FLOW AND MIXED FLOW OVER FLOORS ######
C Fisher & Pedersen `ASHRAE' correlation for radial ceiling diffusers.
        ACH = Vdot(ICOMP)*3600./VOL(ICOMP)
        HCFish = 0.13*ACH**0.8
        HC = HCFish*ADJREF(ICOMP,ISUR)
        if(dotrace) then
          write(outs,'(A,3F9.3)')
     &    ' Fisher ASHRAE radial ceil (floor): HC & HCFish & ACH',HC,
     &    HCFish,ACH
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.81)THEN

C Fisher `thesis' correlation for radial ceiling diffusers.
        ACH = Vdot(ICOMP)*3600./VOL(ICOMP)
        HCFish = 0.159 + 0.116*ACH**0.8
        HC = HCFish*ADJREF(ICOMP,ISUR)
        if(dotrace) then
          write(outs,'(A,3F9.3)')
     &    ' Fisher thesis radial ceil (floor): HC & HCFish & ACH',HC,
     &    HCFish,ACH
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.82)THEN

C Fisher `thesis' correlation for free horizontal jet.
        ACH = Vdot(ICOMP)*3600./VOL(ICOMP)
        HCFish = 0.704 + 0.168*ACH**0.8
        HC = HCFish*ADJREF(ICOMP,ISUR)
        if(dotrace) then
          write(outs,'(A,3F9.3)')
     &    ' Fisher thesis free hor jet (floor): HC & HCFish & ACH',HC,
     &    HCFish,ACH
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.83)THEN

C Mixed forced-free model using Fisher `thesis' correlation
C for radial ceiling diffusers and Alamdari & Hammond's correlation.
C A&H equation depends on direction of heat flow (iAandH).
        ACH = Vdot(ICOMP)*3600./VOL(ICOMP)
        HCFish = 0.159 + 0.116*ACH**0.8
        HCforc = HCFish*ADJREF(ICOMP,ISUR)
        if(iAandH.eq.1) HCfree = ((1.4*((DT/APRAT)**0.25))**6.0
     &                          +(1.63*(DT**(1.0/3.0)))**6.0)**(1.0/6.0)
        if(iAandH.eq.2) HCfree = 0.60*((DT/(APRAT**2.0))**0.2)
        HC = ( HCforc**3. + HCfree**3. )**(1./3.)
        if(dotrace) then
          write(outs,'(A,4F9.3)')
     & ' Fisher thesis mixed radial (flr): HC HCFish HCforc HCfree ACH',
     &    HC,HCFish,HCforc,ACH
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.90)THEN

C######   FORCED FLOW AND MIXED FLOW UNDER CEILINGS ######
C Fisher & Pedersen `ASHRAE' correlation for radial ceiling diffusers.
        ACH = Vdot(ICOMP)*3600./VOL(ICOMP)
        HCFish = 0.49*ACH**0.8
        HC = HCFish*ADJREF(ICOMP,ISUR)
        if(dotrace) then
          write(outs,'(A,3F9.3)')
     &  ' Fisher ASHRAE radial ceil (under ceil): HC & HCFish & ACH',
     &    HC,HCFish,ACH
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.91)THEN

C Fisher `thesis' correlation for radial ceiling diffusers.
        ACH = Vdot(ICOMP)*3600./VOL(ICOMP)
        HCFish = -0.166 + 0.484*ACH**0.8
        HC = HCFish*ADJREF(ICOMP,ISUR)
        if(dotrace) then
          write(outs,'(A,3F9.3)')
     &  ' Fisher thesis radial ceil (under ceil): HC & HCFish & ACH',
     &    HC,HCFish,ACH
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.92)THEN

C Fisher `thesis' correlation for free horizontal jet.
        ACH = Vdot(ICOMP)*3600./VOL(ICOMP)
        HCFish = 0.064 + 0.00444*(ACH**2.8)/DELTAR(ICOMP)
        HC = HCFish*ADJREF(ICOMP,ISUR)
        if(dotrace) then
          write(outs,'(A,3F9.3)')
     &  ' Fisher thesis free hor jet (undere ceil): HC & HCFish & ACH',
     &    HC,HCFish,ACH
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.93)THEN

C Mixed convection model using Fisher `thesis' correlation
C for radial ceiling diffusers and Alamdari & Hammond's correlation.
C A&H equation depends on direction of heat flow (iAandH).
        ACH = Vdot(ICOMP)*3600./VOL(ICOMP)
        HCFish = -0.166 + 0.484*ACH**0.8
        HCforc = HCFish*ADJREF(ICOMP,ISUR)
        if(iAandH.eq.1) HCfree = ((1.4*((DT/APRAT)**0.25))**6.0
     &                          +(1.63*(DT**(1.0/3.0)))**6.0)**(1.0/6.0)
        if(iAandH.eq.2) HCfree = 0.60*((DT/(APRAT**2.0))**0.2)
        HC = ( HCforc**3. + HCfree**3. )**(1./3.)
        if(dotrace) then
          write(outs,'(A,4F9.3)')
     &  ' Fisher thesis mixed radial (undr ceil): HC HCFish HCforc ACH',
     &    HC,HCFish,HCforc,ACH
          call edisp(itu,outs)
        endif
      ELSEIF(ICOR.EQ.99)THEN

C ADAPTIVE CONTROL USING FIXED COEFFICIENTS.
        HC = HCctl5
      ENDIF

      RETURN
      END


C ******************** HTCEXT ********************
C HTCEXT calculates convection coefficients for exterior surfaces.
C ICOR is index of heat transfer correlation:
C  icor = 1 default equations from McAdams wind tunnel test(Energy Simulation 
C         in Building Design by Clarke J A)
C  icor = 2 MoWiTT model from M.Yazdanian(Measurement of the Exterior Convective 
C         Film Coefficient for Windows in Low-rise Buildings ),
C  icor = 3 Aya Hagishima & Jun Tanimoto equations(Field Measureemnts for Estimating 
C         the Convective Heat Transfer Coefficient at building Surfaces),
C  icor = 4 Ya Liu equations derived from roof wind speed(PhD thesis),
C  icor = 5 Ya Liu equations derived from wall wind speed(PhD thesis),
C  icor = 6 Ya Liu eqations derived from weather station wind speed(PhD thesis)
C  icor = 7 D.L.Loveday & A.H.Taki derived from roof wind speed(Convective Heat Transfer
C         Coefficients at a Plane Surface on a Full-scale Building Facade),
C  icor = 8 D.L.Loveday & A.H.Taki derived from wall wind speed(Convective Heat Transfer 
C         Coefficients at a Plane Surface on a Full-scale Building Facade),
C  icor = 9 CIBSE guide (CIBSE Guide C3),
C  icor = 10 D.L.Loveday mixed convecive correlation(CIBSE Guide C3)
C  icor = 11 British Standard equation(British Standard)
C  icor = 12 ASHRAE Task Group equations(Full-scale Measurements of Convective 
C         Energy Losses from Exterior Building Surfaces by S.Sharples), 
C  icor = 13 Sturrock equation(Convective Heat Transfer Coefficients at a Plane 
C         Surface on a Full-scale Building Facade),
C  icor = 14 Keith Nicol equation(The Energy Balance of an Exterior Window
C         Surface,Inuvik,N.W.T.,Canada),
C  icor = 15 S.E .G.Jayamaha equations(Measurement of the Heat Transfer 
C         Coefficient for Walls).

C NOTE: only Aya Hagishima measured horizontal surfaces, none of the other sources
C mention horizontal surfaces. So, except for Hagishima, horizontal surfaces use
C the same correlation as vertical surfaces.

      SUBROUTINE HTCEXT(ICOR,HC,ICOMP,ISUR)

#include "building.h"
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/TC/ITC,ICNT
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/BTIME/BTIMEP,BTIMEF

      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)

      character outs*124
      logical dotrace

C Trace output ?
      dotrace=.false.
      IF(ITC.LE.0.OR.NSINC.LT.ITC)goto 9999
      IF(ITRACE(6).EQ.0.OR.NSINC.GT.ITCF)goto  9999
      write(outs,'(A,I4)')
     &  ' Subroutine HTCEXT  Trace output',ICNT
      call edisp(itu,outs)
      CALL DAYCLK(IDYP,BTIMEF,ITU)
      dotrace=.true.
 9999 continue

C The MoWiTT method needs external surface temperatue TFC(icomp,isur,1)
C and ambient temperature TF.

C In the equations below (for all cases) the variables
C VR is roof wind speed and VF is weather station wind speed and
C VS is wall wind speed.

      PI = 4.0 * ATAN(1.0)
      R=PI/180.

C Depending on the correlation, use different equations.
      if(ICOR.eq.1)then
      
C If default correlation do the following...
C The orientation of the surface and the wind direction relative to
C the surface determine which correlation to use.

C Predominantly horizontal surface.
        IF(PELV(ICOMP,ISUR).LT.-45. .OR. PELV(ICOMP,ISUR).GT.45.) THEN
          V=VF
          HC=2.8+3.0*V
        ELSE

C Predominantly vertical surface.
C Determine wind direction relative to surface.
          WSA=ABS(PAZI(ICOMP,ISUR)-DF)
          IF(WSA.GT.180.0)WSA=360.0-WSA

C Determine direction of wind relative to surface, then apply correlation.
          if(WSA.LT.10.0) then

C Wind directly impinging on face.
            V=.5*VF
            IF(V.GT.0.5)V=0.5
            IF(VF.GT.2.)V=.25*VF
            HC=2.8+3.0*V
          elseif(WSA.GT.90.0) then

C Surface 'shaded' from wind.
            V=VF*0.25*ABS(SIN(WSA*R))
            HC=2.8+3.0*V
          else

C Surface 'facing' wind direction, but not directly impinging.
            V=VF*SIN(WSA*R)
            HC=2.8+3.0*V
          endif
        endif
        if(dotrace) then
          write(outs,'(A,3F9.3)')
     &    ' default equations from McAdams: VF V HC',VF,V,HC
          call edisp(itu,outs)
        endif
      elseif (ICOR.eq.2) then
      
C Use M.Yazdanan's MoWiTT model which was derived from a van in USA in 1994.
C DT is temperature difference between outside surface and ambient temperature.
        DT = TFC(icomp,isur,1) - TF

C Determine wind direction relative to surface. 
        WSA=ABS(PAZI(ICOMP,ISUR)-DF)
        IF(WSA.GT.180.0)WSA=360.0-WSA
        if(WSA.le.90.0)then

C If WSA < 90. assume to be on windward side.
C Note: 0.001 is added to VF to ensure it is non-zero.
          HC = SQRT((0.84*(ABS(DT))**(1.0/3.0))**2+
     &         (2.38*(ABS(VF)+0.001)**0.89)**2)
        else

C Assume leeward side.
          HC = SQRT((0.84*(ABS(DT))**(1.0/3.0))**2+
     &         (2.86*(ABS(VF)+0.001)**0.617)**2)
        endif
        if(dotrace) then
          write(outs,'(A,3F9.3)')
     &    ' MoWiTT model equations: VF DT HC',VF,DT,HC
          call edisp(itu,outs)
        endif
      elseif (ICOR.eq.3) then

C Use Aya Hagishima's correlations which were derived from a 4-storey building attached to
C a 2-storey building in Japan in 2000.
        IF(PELV(ICOMP,ISUR).LT.-45. .OR. PELV(ICOMP,ISUR).GT.45.) THEN

C Predominantly horizontal surface.
          VR=VF
          HC=2.28*VR+8.18
        ELSE

C Predominantly vertical surface.
          VR=VF
          VS=(2.0/3.0)*VR
          HC=10.21*VS+4.47
        endif
        if(dotrace) then
          write(outs,'(A,4F9.3)')
     &    ' Aya Hagishima equations: VF VR VS HC',VF,VR,VS,HC
          call edisp(itu,outs)
        endif
      elseif (ICOR.eq.4) then

C Use Ya Liu correlations derived from roof wind speed for low-rised buildings surrounded by shelterbelt trees
C Determine wind direction relative to surface. 
        WSA=ABS(PAZI(ICOMP,ISUR)-DF)
        IF(WSA.GT.180.0)WSA=360.0-WSA
        if(WSA.le.90.0)then

C If WSA < 90. assume to be on windward side.
           VR=0.55*VF+0.67
           HC=2.08*VR+2.97
         else

C Assume leeward side.
           VR=0.43*VF+0.24
           HC=1.57*VR+2.64
         endif
        if(dotrace) then
          write(outs,'(A,3F9.3)')
     &    ' Ya Liu roof wind speed equations: VF VR HC',VF,VR,HC
          call edisp(itu,outs)
        endif
      elseif (ICOR.eq.5) then

C Use Ya Liu correlations derived from wall wind speed for low-rised buildings surrounded by shelterbelt trees
C Determine wind direction relative to surface. 
        WSA=ABS(PAZI(ICOMP,ISUR)-DF)
        IF(WSA.GT.180.0)WSA=360.0-WSA
        if(WSA.le.90.0)then

C If WSA < 90. assume to be on windward side.
          VS=0.26*VF+0.06
          HC=6.31*VS+3.32
        else

C Assume leeward side.
          VS=0.19*VF+0.14
          HC=5.03*VS+3.19
        endif
        if(dotrace) then
          write(outs,'(A,3F9.3)')
     &    ' Ya Liu wall wind speed equations: VF VS HC',VF,VS,HC
          call edisp(itu,outs)
        endif
      elseif (ICOR.eq.6) then

C Use Ya Liu correlations derived from local weather station wind speed for low-rised buildings
C surrounded by shelterbelt trees
C Determine wind direction relative to surface. 
        WSA=ABS(PAZI(ICOMP,ISUR)-DF)
        IF(WSA.GT.180.0)WSA=360.0-WSA
        if(WSA.le.90.0)then

C If WSA < 90. assume to be on windward side.
          HC=1.53*VF+1.43
        else

C Assume leeward side.
          HC=0.90*VF+3.28
        endif
        if(dotrace) then
          write(outs,'(A,2F9.3)')
     &    ' Ya Liu weather station wind speed equations: VF HC',VF,HC
          call edisp(itu,outs)
        endif
      elseif (ICOR.eq.7) then

C Use Loveday correlations derived from roof wind speed for high-rised buildings
C in urban environmental conditions.
C Determine wind direction relative to surface.
        WSA=ABS(PAZI(ICOMP,ISUR)-DF)
        IF(WSA.GT.180.0)WSA=360.0-WSA
        if(WSA.le.90.0)then

C If WSA < 90. assume to be on windward side.
          VR=VF
          HC=2.00*VR+8.91
        else

C Assume leeward side.
          VR=VF
          HC=1.77*VR+4.93
        endif
        if(dotrace) then
          write(outs,'(A,3F9.3)')
     &    ' Loveday roof wind speed equations: VF VR HC',VF,VR,HC
          call edisp(itu,outs)
        endif
      elseif (ICOR.eq.8) then

C Use Loveday correlations derived from wall wind speed for high-rised buildings
C in urban environmental conditions.
C Determine wind direction relative to surface. 
        WSA=ABS(PAZI(ICOMP,ISUR)-DF)
        IF(WSA.GT.180.0)WSA=360.0-WSA
        if(WSA.le.90.0)then

C If WSA < 90. assume to be on windward side.
          VR=VF
          VS=0.68*VR-0.5
          if(VS.LE.0) then
            
C Note: 0.001 is added to VS to ensure it is non-zero.
            VS=0.001
            HC=16.15*(VS**0.397)
          else
            HC=16.15*(VS**0.397)
          endif
        else

C Assume leeward side.
          VR=VF
          VS=0.157*VR-0.027
          if(VS.LE.0) then
            
C Note: 0.001 is added to VS to ensure it is non-zero.
            VS=0.001
            HC=16.25*(VS**0.503)
          else
            HC=16.25*(VS**0.503)
          endif            
        endif
        if(dotrace) then
          write(outs,'(A,4F9.3)')
     &    ' Loveday wind wind speed equations: VF VR VS HC',VF,VR,VS,HC
          call edisp(itu,outs)
        endif
      elseif (ICOR.eq.9) then

C Use CIBSE Guide equation
        VS=2.0/3.0*VR
        VR=VF
        HC=4.1*VS+5.8
        if(dotrace) then
          write(outs,'(A,4F9.3)')
     &    ' CIBSE guide equations: VF VR VS HC',VF,VR,VS,HC
          call edisp(itu,outs)
        endif
      elseif (ICOR.eq.10) then

C Use Loveday mixed correlation from four equations quoted by Guide Guide C3.
        VS=2.0/3.0*VR
        VR=VF
        HC=16.7*VS**0.5
        if(dotrace) then
          write(outs,'(A,4F9.3)')
     &    ' Loveday mixed equations: VF VR VS HC',VF,VR,VS,HC
          call edisp(itu,outs)
        endif
      elseif (ICOR.eq.11) then

C Use British Standard equation.
C V is wind speed.    
        V=VF
        HC=4.0*V+4.0
        if(dotrace) then
          write(outs,'(A,3F9.3)')
     &    ' British standard equations: VF V HC',VF,V,HC
          call edisp(itu,outs)
        endif
      elseif (ICOR.eq.12) then

C Use ASHRAE Task Group equations
C Determine wind direction relative to surface 
        WSA=ABS(PAZI(ICOMP,ISUR)-DF)
        IF(WSA.GT.180.)WSA=360.0-WSA
        if(WSA.le.90.0)then

C If WSA < 90. assume to be on windward side.
          if(VF.lt.2.0) then
            VS=0.5
            HC=18.6*VS**0.605
          else
            VS=0.25*VF
            HC=18.6*VS**0.605
          endif
        else

C Assume leeward side.
          VS=0.05*VF+0.3
          HC=18.6*VS**0.605
        endif
        if(dotrace) then
          write(outs,'(A,3F9.3)')
     &    ' ASHRAE Task Group equations: VF VS HC',VF,VS,HC
          call edisp(itu,outs)
        endif
      elseif (ICOR.eq.13) then

C Use Sturrock equation for normal exposure conditions and high-rised buildings
        VR=VF
        HC=6.00*VR+5.70
        if(dotrace) then
          write(outs,'(A,3F9.3)')
     &    ' Sturrock equations: VF VR HC',VF,VR,HC
          call edisp(itu,outs)
        endif
      elseif (ICOR.eq.14) then

C Use Keith Nicol equation 
        VR=VF
        HC=7.55*VR+4.35
        if(dotrace) then
          write(outs,'(A,3F9.3)')
     &    ' Keith Nicol equations: VF VR HC',VF,VR,HC
          call edisp(itu,outs)
        endif
      elseif (ICOR.eq.15) then

C Use S.E.G.Jayamaha equation (experiment was made on a mast)
        VR=VF
        HC=1.444*VR+4.955
        if(dotrace) then
          write(outs,'(A,3F9.3)')
     &    ' S.E.G.Jayamaha equations: VF VR HC',VF,VR,HC
          call edisp(itu,outs)
        endif
      else
        call usrmsg('Icor is not a recognised exterior convection',
     &    'correlation.','W')
      endif 

      RETURN
      END


C ******************** SELCOR1 ********************
C SELCOR1 selects the convection-calculation approach to use for a given
C surface based on the surface orientation, the direction of heat flow,
C and the user-specified simulation toggle.
      SUBROUTINE SELCOR1(ICOMP,ISUR,inRout,TA,TS,ICOR)

#include "building.h"
      COMMON/HCTHRY/IHCT,icorexhct
      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)

C Determine orientation of surface.  ANGLE=0 for walls and other vertical
C surfaces; ANGLE=-90 for floors; and ANGLE=+90 for ceilings.
      ANGLE = PELV(ICOMP,ISUR)
      if(inRout.eq.2)then

C This ensures that, for example, the `ceiling' of a zone is recognized as
C a `floor' when calculating the HTC for its `outside' surface.
        ANGLE = -1.*ANGLE
      endif

C Select correlation based on surface orientation, direction of heat flow,
C and simulation toggle.

C Surface is predominately vertical.
      IF(ANGLE.LE.45..AND.ANGLE.GE.-45.) THEN

C Alamdari & Hammond requested => use A&H.
        if(IHCT.eq.1) ICOR = 1

C Khalifa & Marshall `radiator under window' requested => use k&M eq.6,
C even for windows.
        if(IHCT.eq.2) ICOR = 5

C Khalifa & Marshall `radiator NOT under window' requested => use k&M
C eq.3, even for windows.
        if(IHCT.eq.3) ICOR = 6

C Halcrow low requested => use Halcrow low.
        if(IHCT.eq.4) ICOR = 2

C Halcrow high requested => use Halcrow high.
        if(IHCT.eq.5) ICOR = 3

C CIBSE requested => use CIBSE.
        if(IHCT.eq.6) ICOR = 4

C CEN requested => use CEN.
        if(IHCT.eq.7) ICOR = 9

C Surface is predominantly horizontal and heat flows up (ie. thermal
C plumes develop above a hot floor or below a cold ceiling).
      ELSEIF( (ANGLE.LT.-45. .and. TS.gt.TA) .or.
     &        (ANGLE.GT.45. .and. TS.lt.TA) )THEN

C Alamdari & Hammond requested => use A&H.
        if(IHCT.eq.1) ICOR = 50

C Khalifa & Marshall `radiator under window' requested => use k&M eq.7 if
C surface is a ceiling.  But, if surface is a floor, use A&H as there is no
C K&M correlation for floors.
        if(IHCT.eq.2) then
          if(ANGLE.GT.45.)then
            ICOR = 60
          else
            ICOR = 50
          endif
        endif

C Khalifa & Marshall `radiator NOT under window' requested => use k&M eq.4
C if surface is a ceiling.  But, if surface is a floor, use A&H as there
C is no K&M correlation for floors.
        if(IHCT.eq.3)then
          if(ANGLE.GT.45.)then
            ICOR = 61
          else
            ICOR = 50
          endif
        endif

C Halcrow low requested => use Halcrow low.
        if(IHCT.eq.4) ICOR = 51

C Halcrow high requested => use Halcrow high.
        if(IHCT.eq.5) ICOR = 52

C CIBSE requested => use CIBSE.
        if(IHCT.eq.6) ICOR = 53

C CEN requested => use CEN.
        if(IHCT.eq.7) ICOR = 54

C Surface is predominantly horizontal and heat flows down (ie. stably
C stratified convection above a cold floor or a hot ceiling).
      ELSEIF( (ANGLE.LT.-45. .and. TS.le.TA) .or.
     &        (ANGLE.GT.45. .and. TS.ge.TA) )THEN

C Alamdari & Hammond requested => use A&H.
        if(IHCT.eq.1) ICOR = 70

C Khalifa & Marshall `radiator under window' requested => no k&M
C correlation exists for this case, so use A&H.
        if(IHCT.eq.2) ICOR = 70

C Khalifa & Marshall `radiator NOT under window' requested =>  no k&M
C correlation exists for this case, so use A&H.
        if(IHCT.eq.3) ICOR = 70

C Halcrow low requested => use Halcrow low.
        if(IHCT.eq.4) ICOR = 71

C Halcrow high requested => use Halcrow high.
        if(IHCT.eq.5) ICOR = 72

C CIBSE requested => use CIBSE.
        if(IHCT.eq.6) ICOR = 73

C CEN requested => use CEN.
        if(IHCT.eq.7) ICOR = 74
      ENDIF

      RETURN
      END

