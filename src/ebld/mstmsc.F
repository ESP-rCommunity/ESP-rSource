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


C This file contains the following functions.
C BETA01   - Derives the mass transfer coefficient from heat transfer
C            coefficient using the Lewis relation.
C BETA02   -
C U01      - Determines the moisture content as a function of relative humidity 
C            and sorption isotherm.
C U02      - Moisture content (kg/kg dry material).
C XI01     - Determines the value for moisture capacity (kg/kg) (slope for sorption
C            isotherm) using curve fitting techniques for sorption isotherms.
C XI02     - Moisture capacity (kg/kg).
C XI03     - The numerical equivalent of XI01.
C XIair01  - Determines the value for moisture capacity (slope for sorption
C            isotherm) for air node by numerical differentiation.
C XIair02  - Determines the value for moisture capacity (slope for sorption
C            isotherm) for air node by differentiating the moisture content formulae
C            with respect to relative humidity at fixed temperature and total pressure.
C XIair03  - Equivalent to XIair02 but based on a numerical approach.
C PHI02    - Determines the value for relative humidity from vapour ratio,
C            pressure, and temperature.
C EVAP01   - Determines the latent heat of evapouration (transformation) as 
C            a function of temperature.
C DELT01   - Determines the vapour permeability of the material as a function of RH.
C DELT03   - Determines the vapour permeability of the material as a function of RH.
C CPair01  - Calculates the specific heat capacity of dry air (J/kg K) as a function
C            of temperature.
C CPvpr01  - Calculates the specific heat capacity of water vapour (J/kg K) as a
C            function of temperature.
C CPmst01  - Calculates the specific heat capacity of moist air (J/kg K) as a function
C            of temperature (C) and moisture content (kg/kg dry air).
C RHOdry01 - Calculates the density of dry air (kg/m^3) at known temperature
C            and pressure using the ideal gas law.
C RHOdry02 -
C Psat01   - Calculates the saturation vapour pressure (Pa) as a function of
C            temperature (C).
C Pvpr01   - Calculates the vapour pressure (Pa) knowing the total pressure (Pa) 
C            and vapour ratio (kg/kg).
C Pvpr02   - Calculates the vapour pressure (Pa) given the temperature (C)
C            and relative humidity (-).
C RHOmst01 - Calculates the density of moist air (kg/m^3) given the
C            temperature (C), vapour pressure (Pa) and total pressure (Pa).
C RHOmst02 - Calculates the density of moist air (kg/m^3) given the
C            temperature (C), vapour pressure (Pa) and total pressure (Pa).
C RHOmst03 - Calculates the density of moist air (kg/m^3) given the
C            temperature (C) and relative humidity (-).
C Pt01     - Calculates the total pressure (Pa) given the altitude (m)
C            and temperature (C).
C X01      - Calculates the vapour ratio (kg vapour/kg dry air) using vapour
C            pressure (Pa).
C X02      - Calculates the vapour ratio (kg vapour/kg dry air) using vapour
C            pressure (Pa).
C X03      - Calculates the vapour ratio (kg vapour/kg dry air) using vapour
C            pressure (Pa).
C ENTHP01  - Calculates the enthalpy of moist air using temperature (C)
C            and vapour ratio (kg/kg).
C RH01     - Calculates the relative humidity using vapour pressure (Pa) and 
C            saturation vapour pressure (Pa).
C Hv01     - Calculates enthalpy of vapour (J/kg).
C Hl01     - Calculates enthalpy of water (J/kg).

C R      - Universal gas constant, 8314.4 J/mole K.
C MOLH2O - Molecular mass of water, 18.016 kg/kmole).
C ScPrn1 - (Sc/Pr)^(n-1)
C ALPHA  - Convective heat transfer coefficient (W/m^2 K).
C TC     - Ambient temperature (C).
C TK     - Ambient temperature (K).
C CP     - Air specific heat capacity (J/kg K).
C RhoA   - Air density (kg/m^3).


C ******************** BETA01 ********************
C Derives the mass transfer coefficient from heat transfer
C coefficient using the Lewis relation.

      FUNCTION BETA01(ALPHA,CP,TC)

      H2OMOL=18.016
      ScPrn1=0.85
      Ru=8314.4
      TK=TC+273.15
      RhoA=1.2
      BETA01=ALPHA*H2OMOL*ScPrn1/(CP*Ru*TK*RhoA)
      IF(BETA01.LT.0.0)
     &  STOP 'Error (701): negative mass transfer coefficient.'
      RETURN
      END

C ******************** BETA02 ********************

      FUNCTION BETA02(ALPHA)

      BETA02=7.4e-9*ALPHA
      IF(BETA02.LT.0.0)
     &  STOP 'Error (702): negative mass transfer coefficient.'
      RETURN
      END

C ******************** U01 ********************
C Determines the moisture content as a function of
C relative humidity and sorption isotherms provided by (Marieke 1994,
C Hansen 1986 Technical Report 162/86, Technical University of Denmark).
C U01  - moisture content (kg/m^3)
C RH   - relative humidity
C Uh   - max hygroscopic moisture content (kg/m^3) (unit?)
C CFF  - coefficient used to calculate u=f(RH) and xi
C EXPT - exponent used to calculate u=f(RH) and xi
C
C Otto Model: PhD Thesis by Frank Otto, 1995, University of Kassel,
C 'The Influence of Sorption Processes on Indoor Air Humidity'
C (in German).
C   Parameter mapping:
C     Uh -> a1
C    CFF -> a2
C   EXPT -> a3

      FUNCTION U01(iMM,RH,Uh,CFF,EXPT)
      COMMON/MOIST17/RHmax

      IF(RH.GT.RHmax)RH=RHmax
      if (iMM.eq.1) then

C Legacy model.
        U01=Uh*(1.0-ALOG(RH)/CFF)**(-1.0/EXPT)
      elseif (iMM.eq.2) then
C Otto model.

        U01=1000.*(exp(Uh*RH+CFF*RH**2+EXPT*RH**3)-1.)
      endif
cx      IF(U01.LT.0.0)STOP 'Error (703): negative moisture content.'
      IF (U01.LT.0.0) U01=0.00001
      RETURN
      END

C ******************* U02 *******************
C Moisture content (kg/kg dry material).

      FUNCTION U02(iMM,RH,Uh,CFF,EXPT,DNSTY)
      COMMON/MOIST17/RHmax

      if ((DNSTY.lt.0.01))then
C Air layer, set average value.
        DNSTY=1.18
      endif

cx      IF(RH.GT.RHmax)RH=RHmax  ! Covered in U01.
      U02=U01(iMM,RH,Uh,CFF,EXPT)/DNSTY
      IF(U02.LT.0.0)STOP 'Error (704): negative moisture content.'
      RETURN
      END

C ******************** XI01 ********************
C Determines the value for moisture capacity (slope for sorption
C isotherm) using curve fitting techniques for sorption isotherms 
C provided by (Marieke 1994, Hansen 1986 Technical Report 162/86,
C Technical University of Denmark).
C XI01 - Moisture capacity (kg/kg.
C RH   - Relative humidity (-).
C Uh   - Maximum hygroscopic moisture content (kg/kg).
C CFF  - Empirical coefficient used to calculate.
C EXPT - Empirical exponent used to calculate.
C
C Otto Model parameter mapping:
C     Uh -> a1
C    CFF -> a2
C   EXPT -> a3

      FUNCTION XI01(iMM,RH,Uh,CFF,EXPT)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/MOIST17/RHmax
      DOUBLE PRECISION XI01
      SMALL=1.0E-15

      IF(RH.GT.RHmax)RH=RHmax
      if (iMM.eq.1) then

C Legacy model.
        XI01=Uh/(EXPT*CFF*RH)*(1.0-ALOG(RH)/CFF)**(-(EXPT+1.0)/EXPT) ! Not the Hansen form.
      elseif (iMM.eq.2) then

C Otto model.
        XI01=1000.*exp(Uh*RH+CFF*RH**2+EXPT*RH**3)
     &         *(3.*EXPT*RH**2+2.*CFF*RH+Uh)
        XI01=max(DBLE(SMALL),XI01)
      endif
      IF(XI01.LT.DBLE(SMALL))then
        call edisp(iuout,'XI01: (705) very small moisture capacity.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** XI02 ********************
C  Moisture capacity (kg/kg).

      FUNCTION XI02(iMM,RH,Uh,CFF,EXPT,DNSTY)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/MOIST17/RHmax
      DOUBLE PRECISION XI01, XI02
      SMALL=1.0E-15

      IF(RH.GT.RHmax)RH=RHmax

      if ((DNSTY.lt.0.01))then

C Air layer, set average value.
        DNSTY=1.18
      endif

      XI02=XI01(iMM,RH,Uh,CFF,EXPT)/DBLE(DNSTY)
      IF(XI02.LT.DBLE(SMALL))then
        call edisp(iuout,'XI02: (706) very small moisture capacity.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** XI03 ********************
C Moisture capacity (kg/m^3); the numerical equivalent of XI01.

      FUNCTION XI03(iMM,RHp,RHf,Uh,CFF,EXPT)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/MOIST17/RHmax
      DOUBLE PRECISION XI01, XI03
      SMALL=1.0E-15

      IF(RHp.GT.RHmax)RHp=RHmax
      IF(RHf.GT.RHmax)RHf=RHmax
      RHfp=RHf-RHp
      IF(ABS(RHfp).LT.1.0e-6)THEN
        XI03=XI01(iMM,RHp,Uh,CFF,EXPT)
      ELSE
        Up=U01(iMM,RHp,Uh,CFF,EXPT)
        Uf=U01(iMM,RHf,Uh,CFF,EXPT)
        XI03=(Uf-Up)/RHfp
        IF(XI03.LE.0.0)XI03=XI01(iMM,RHp,Uh,CFF,EXPT)
      ENDIF
      IF(XI03.LT.DBLE(SMALL))then
        call edisp(iuout,'XI03: very small moisture capacity.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** XIair01 ********************
C Determines the value for moisture capacity (slope for sorption
C isotherm) for air node by numerical differentiation.
C XIair01 - Moisture capacity (kg/kg).
C Pvp     - Present time vapour pressure (Pa).
C Pvf     - Future time vapour pressure (Pa).
C TCp     - Present time temperature (C).
C TCf     - Future time temperature (C).

      FUNCTION XIair01(Pvp,Pvf,TCp,TCf,Pt)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/MOIST17/RHmax
      character outs*124
      SMALL=1.0E-15

      Psp=Psat01(TCp)
      Psf=Psat01(TCf)
      RHp=Pvp/Psp
      IF(RHp.GT.RHmax)RHp=RHmax
      RHf=Pvf/Psf
      IF(RHf.GT.RHmax)RHf=RHmax
      RHfp=RHf-RHp
      IF(ABS(RHfp).LT.1.0e-6)THEN
        XIair01=XIair02(Pvp,TCp,Pt)
        iSource=1
      ELSE
        Xp=X03(TCp,RHp,Pt) ! kg (vapour)/kg (dry air)
        Xf=X03(TCf,RHf,Pt)
        XIair01=(Xf-Xp)/RHfp
        iSource=2
        IF(XIair01.LE.0.0) then
          XIair01=XIair02(Pvp,TCp,Pt)
          iSource=3
        ENDIF
      ENDIF
      IF(XIair01.LT.DBLE(SMALL))then
        write(outs,'(A,A)')'XIair01: small moist. cap for case of:',
     &   ' Pvp Pvf TCp TCf  RHfp Xp Xf  Pabs iS'
        call edisp(iuout,outs)
        write(outs,'(4(F8.3,1X),3(F8.5,1X),I6,I2,A)')
     &    Pvp,Pvf,TCp,TCf,RHfp,Xp,Xf,int(Pt),iSource,' resetting'
        call edisp(iuout,outs)
        XIair01=1.0E-10
      endif
      RETURN
      END

C ******************** XIair02 ********************
C Determines the value for moisture capacity (slope for sorption
C isotherm) for air node by differentiating the moisture content formulae
C with respect to relative humidity at fixed temperature and total 
C pressure.

      FUNCTION XIair02(Pv,TC,Pt)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/MOIST17/RHmax
      SMALL=1.0E-15

      Ra=287.06
      Rv=461.5
      RATIO=Ra/Rv
      Ps=Psat01(TC)
      RH=Pv/Ps
      IF(RH.GT.RHmax)RH=RHmax
      XIair02=RATIO*Ps*Pt/(Pt-Ps*RH)**2 ! Unit [-]? Should be kg/m3 !!?
      IF(XIair02.LT.DBLE(SMALL))then
        call edisp(iuout,'XIair02: very small moisture capacity.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** XIair03 ********************
C Equivalent to XIair02 but based on a numerical approach.

      FUNCTION XIair03(Pv,TC,Pt)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/MOIST17/RHmax
      SMALL=1.0E-15

      Ps=Psat01(TC)
      RH=Pv/Ps
      IF(RH.GT.RHmax)RH=RHmax
      IF(RH.GT.(RHmax-0.002))THEN
        RH1=RH-0.002
        RH2=RH
      ELSEIF(RH.LT.0.01)THEN
        RH1=RH
        RH2=RH+0.002
      ELSE
        RH1=RH-0.001
        RH2=RH+0.001
      ENDIF
      RH1001=RH1*100.0
      RH1002=RH2*100.0
      PATMOS=Pt/100.0
      X1=HUMRT1(TC,RH1001,PATMOS,0)
      X2=HUMRT1(TC,RH1002,PATMOS,0)
      XIair03=(X2-X1)/(RH2-RH1)
      IF(XIair03.LT.DBLE(SMALL))then
        call edisp(iuout,'XIair03: very small moisture capacity.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** PHI02 ********************
C Determines the value for relative humidity from vapour ratio,
C pressure, and temperature (Heating, Ventilating, and Air Conditioning
C by McQuiston and Parker).

      FUNCTION PHI01(U,Uh,CFF,EXPT)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      PHI01=EXP(CFF-CFF*(U/Uh)**(-EXPT))
      IF(PHI01.LT.0.0)then
        call edisp(iuout,'PHI01: negative relative humidity.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** PHI02 ********************
C Determines the value for relative humidity from vapour ratio,
C pressure, and temperature (Heating, Ventilating, and Air Conditioning
C by McQuiston and Parker).

      FUNCTION PHI02(TC,X,Pt)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      Ra=287.06
      Rv=461.5
      Ps=Psat01(TC)
      Pv=Pvpr01(X,Pt)
      Pa=Pt-Pv
      PHI02=X*Pa*Rv/(Ps*Ra)
      IF(PHI02.LT.0.0)then
        call edisp(iuout,'PHI02: negative relative humidity.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** EVAP01 ********************
C Determines the latent heat of evapouration (transformation) as 
C a function of temperature (Marieke 1994).
C EVAP01  - Latent heat of evapouration (J/kgv).
C EVAPr   - Reference heat of condensation at T=0 C (J/kgv).
C CPlqd   - Specific heat capacity for liquid water (J/kgv K).
C CPvap   - Specific heat capacity for vapour water (J/kgv K).
C DCP=CPlqd-CPvap
C TC      - Temperature (C).

      FUNCTION EVAP01(TC)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      character outs*124

      EVAPr=2.5008E6
      DCP=2430.0
      EVAP01=EVAPr-DCP*TC
      IF(EVAP01.LT.0.0)then
        write(outs,'(a,F12.6,a)') 
     &   'EVAP01: negative latent heat of evapouration',
     &    EVAP01,' resetting to 1.0E-10'
        call edisp(iuout,outs)
        EVAP01=1.0E-10
      endif
      RETURN
      END

C ******************** DELT01 ********************
C Determines the vapour permeability of the material as a function  
C of RH (catalogue of material properties-upgraded version).
C DELT01 - Water vapour permeability (s).
C VPRMa  - Vapour permeability factor a.
C VPRMb  - Vapour permeability factor b.
C VPRMc  - Vapour permeability factor c.
C RH     - Relative humidity (-).
C
C Otto Model: PhD Thesis by Frank Otto, 1995, University of Kassel
C 'The Influence of Sorption Processes on Indoor Air Humidity'
C (in German).
C   Parameter mapping:
C   VPRMa -> a0
C   VPRMb -> a2
C   VPRMc -> a4
C   a1 = a3 = 0.

      FUNCTION DELT01(iMM,RH,VPRMa,VPRMb,VPRMc)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/MOIST17/RHmax
      character outs*124
      real my
      SMALL=1.0E-15

      IF(RH.GT.RHmax)RH=RHmax
      if (iMM.eq.1) then

C Legacy model.
        DELT01=1.89923e-10*(VPRMa+VPRMb*EXP(VPRMc*RH))
      elseif (iMM.eq.2) then

C Otto model.
        my=exp(VPRMa+VPRMb*RH**2+VPRMc*RH**4)
        if (my.gt.0.) then
          DELT01=1.883e-10/my
        else
          write(outs,'(a,4F12.6,a)')
     &     'DELT01: small my for: VPRMa VPRMb VPRMc RH',
     &      VPRMa,VPRMb,VPRMc,RH,' resetting to 1.883e-9'
          call edisp(iuout,outs)
          DELT01=1.883e-9
        endif
      endif
      IF(DELT01.LT.SMALL)then
        write(outs,'(a,4F12.6,a)') 
     &   'DELT01: small vapour perm for: VPRMa VPRMb VPRMc RH',
     &    VPRMa,VPRMb,VPRMc,RH,' resetting to 1.0E-15'
        call edisp(iuout,outs)
        DELT01=1.0E-15
      endif
      RETURN
      END

C ******************** DELT03 ********************
C Determines the vapour permeability of the material as 
C a function of RH (Marieke 1994, Ricken 1989).
C DELT03 - Water vapour permeability (s).
C VRF0   - Water vapour resistance factor at RH=0%.
C VRF100 - Water vapour resistance factor at RH=100%.
C B      - Coefficient.
C C      - Constant.
C RH     - Relative humidity (-).

      FUNCTION DELT03(RH,VRF0,VRF100,b)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/MOIST17/RHmax
      SMALL=1.0E-15

      IF(RH.GT.RHmax)RH=RHmax
      DELT03=1.875e-10/(VRF0-(VRF0-VRF100)*RH**b)
      IF(DELT03.LT.SMALL)then
        call edisp(iuout,'DELT03: very small vapour permeability.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** CPair01 ********************
C Calculates the specific heat capacity of dry air (J/kg K)
C as a function of temperature (C) (Marieke 1994, Ham 1984).

      FUNCTION CPair01(TC)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      CPair01=1006.0+TC/200.0+TC*TC/7.5E3
      IF(CPair01.LT.0.0)then
        call edisp(iuout,'CPair01: negative specific heat capacity.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** CPvpr01 ********************
C Calculates the specific heat capacity of water vapour (J/kg K)
C as a function of temperature (C) (Marieke 1994, Ham 1984).

      FUNCTION CPvpr01(TC)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      CPvpr01=1858.4+0.10875*TC+3.083E-4*TC*TC
      IF(CPvpr01.LT.0.0)then
        call edisp(iuout,'CPvpr01: negative specific heat capacity.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** CPmst01 ********************
C Calculates the specific heat capacity of moist air (J/kg K)
C as a function of temperature (C) and moisture content (kg/kg dry air)
C (Marieke 1994, Ham 1984).

      FUNCTION CPmst01(TC,X)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      CPmst01=CPair01(TC)+X*CPvpr01(TC)
      IF(CPmst01.LT.0.0)then
        call edisp(iuout,'CPvpr01: negative specific heat capacity.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** RHOdry01 ********************
C Calculates the density of dry air (kg/m^3) at known temperature
C and pressure using the ideal gas law.
C Ra   - Gas constant for dry air (J/kgK).
C TC   - Temperature (C).
C Pdry - Pressure (Pa).

      FUNCTION RHOdry01(TC)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      SMALL=1.0E-15

      RHOdry01=DENSIT(1,TC)
      IF(RHOdry01.LT.SMALL)then
        call edisp(iuout,'RHOdry01: very small density.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** RHOdry02 ********************

      FUNCTION RHOdry02(TC,Pt)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      SMALL=1.0E-15

      Ra=287.06
      TK=TC+273.15
      RHOdry02=Pt/(TK*Ra)
      IF(RHOdry02.LT.SMALL)then
        call edisp(iuout,'RHOdry02: very small density.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** Psat01 ********************
C Calculates the saturation vapour pressure (Pa) as a function of
C temperature (C) (Marieke 1994, IEA 1991, vol 1, page 4.8).

      FUNCTION Psat01(TC)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      SMALL=1.0E-15

      IF(TC.GT.-30.0.AND.TC.LT.0.0)THEN
        A = 82.9E-3
        B = 288.1E-6
        C = 4.403E-6
        Psat01=611.0*EXP(A*TC-B*TC*TC+C*TC**3)
      ELSEIF(TC.GT.-0.1.AND.TC.LT.40.0)THEN
        A = 72.5E-3
        B = 288.1E-6
        C = 0.79E-6
        Psat01=611.0*EXP(A*TC-B*TC*TC+C*TC**3)
      ELSE
        Psat01=100.0*SATVP(TC)
      ENDIF
      IF(Psat01.LT.SMALL)then
        call edisp(iuout,'Psat01: small saturation vapour pressure.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** Pvpr01 ********************
C Calculates the vapour pressure (Pa) knowing the total pressure
C (Pa) and vapour ratio (kg/kg) (Marieke 1994).

      FUNCTION Pvpr01(X,Pt)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      Ra=287.06
      Rv=461.5
      Pvpr01=X*Pt/((Ra/Rv)+X)
      IF(Pvpr01.LT.0.0)then
        call edisp(iuout,'Pvpr01: negative vapour pressure.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** Pvpr02 ********************
C Calculates the vapour pressure (Pa) given the temperature (C)
C and relative humidity (-) (Marieke 1994).

      FUNCTION Pvpr02(TC,RH)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/MOIST17/RHmax

      IF(RH.GT.RHmax)RH=RHmax
      Pvpr02=RH*Psat01(TC)
      IF(Pvpr02.LT.0.0)then
        call edisp(iuout,'Pvpr02: negative vapour pressure.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** RHOmst01 ********************
C Calculates the density of moist air (kg/m^3) given the
C temperature (C), vapour pressure (Pa) and total pressure (Pa)
C (Marieke 1994).
C Ra - gas constant for dry air (J/kg K).
C Rv - gas constant for water vapour (J/kg K).

      FUNCTION RHOmst01(TC,Pv,Pt)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      SMALL=1.0E-15

      Ra=287.06
      Rv=461.5
      TK=TC+273.15
      RHOmst01=(Pt-Pv)/(Ra*TK)+Pv/(Rv*TK)
      IF(RHOmst01.LT.SMALL)then
        call edisp(iuout,'RHOmst01: very small density.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** RHOmst02 ********************
C Calculates the density of moist air (kg/m^3) given the
C temperature (C), vapour pressure (Pa) and total pressure (Pa)
C (Marieke 1994).

      FUNCTION RHOmst02(TC,X,Pt)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      SMALL=1.0E-15

      Pvpr=Pvpr01(X,Pt)
      RHOmst02=RHOmst01(TC,Pvpr,Pt)
      IF(RHOmst02.LT.SMALL)then
        call edisp(iuout,'RHOmst02: very small density.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** RHOmst03 ********************
C Calculates the density of moist air (kg/m^3) given the
C temperature (C)and relative humidity (-) (Marieke 1994).

      FUNCTION RHOmst03(TC,RH,Pt)
      COMMON/MOIST17/RHmax
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      SMALL=1.0E-15

      IF(RH.GT.RHmax)RH=RHmax
      Pvpr=RH*Psat01(TC)
      RHOmst03=RHOmst01(TC,Pvpr,Pt)
      IF(RHOmst03.LT.SMALL)then
        call edisp(iuout,'RHOmst03: very small density.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** Pt01 ********************
C Calculates the total pressure (Pa) given the altitude (m)
C and temperature (C) (Marieke 1994, Ham 1984).
C G     - Acceleration due to gravity (9.81 m/s^2).
C Pstnd - Standard atmospheric pressure (Pa).
C Rav   - Gas constant for moist air (J/kg K)

C Function superseded: use information in common block ALTPRESDEN in 
C site.h to use pressure from weather file if available or calculated
C from altitude if not.

      FUNCTION Pt01(TC,H)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      Pstnd=101325
      G=9.81
      Rav=286.8
      TK=TC+273.15
      Pt01=Pstnd*EXP(-G*H/(Rav*TK))
      IF(Pt01.LT.0.0)then
        call edisp(iuout,'Pt01: negative total pressure.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** X02 ********************
C Calculates the vapour ratio (kg vapour/kg dry air) using vapour
C pressure (Pa) (Marieke 1994).
C Ra - Gas constant for dry air (J/kg K).
C Rv - Gas constant for water vapour (J/kg K).
C Pt - Total pressure  (Pa).
C TC - Temperature (C).

      FUNCTION X01(Pv,Pt)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      Ra=287.06
      Rv=461.5
      Pa=Pt-Pv
      X01=Pv*Ra/(Pa*Rv)
      IF(X01.LT.0.0)then
        call edisp(iuout,'X01: negative vapour ratio.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** X02 ********************
C Calculates the vapour ratio (kg vapour/kg dry air) using vapour
C pressure (Pa) (Marieke 1994).
C Ra - Gas constant for dry air (J/kg K).
C Rv - Gas constant for water vapour (J/kg K).
C Pt - Total pressure  (Pa).
C TC - Temperature (C).

      FUNCTION X02(TC,RH,Pt)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/MOIST17/RHmax

      IF(RH.GT.RHmax)RH=RHmax
      Ps=Psat01(TC)
      Pv=RH*Ps
      X02=X01(Pv,Pt)
      IF(X02.LT.0.0)then
        call edisp(iuout,'X02: negative vapour ratio.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** X03 ********************
C Calculates the vapour ratio (kg vapour/kg dry air) using vapour
C pressure (Pa).

      FUNCTION X03(TC,RH,Pt)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/MOIST17/RHmax

      IF(RH.GT.RHmax)RH=RHmax
      RH100=RH*100.0
      PATMOS=Pt/100.0
      X03=HUMRT1(TC,RH100,PATMOS,0)
      IF(X03.LT.0.0)then
        call edisp(iuout,'X03: negative vapour ratio.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** ENTHP01 ********************
C Calculates the enthalpy of moist air using temperature (C)
C and vapour ratio (kg/kg) (Marieke 1994).
C CPair - Specific heat capacity of dry air (J/kg K).
C CPvpr - Specific heat capacity of water vapour (J/kg K).

      FUNCTION ENTHP01(TC,X)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      ENTHP01=CPair01(TC)*TC+CPvpr01(TC)*X*TC+X*EVAP01(TC)
      IF(ENTHP01.LT.0.0)then
        call edisp(iuout,'ENTHP01: negative enthalpy.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** RH01 ********************
C Calculates the relative humidity using vapour pressure (Pa) and 
C saturation vapour pressure (Pa) (Marieke 1994).

      FUNCTION RH01(Pv,Ps)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/MOIST17/RHmax

      RH01=Pv/Ps
      IF(RH01.GT.RHmax)RH01=RHmax
      IF(RH01.LT.0.0)then
        call edisp(iuout,'RH01:  relative humidity.')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C ******************** Hv01 ********************
C Calculates enthalpy of vapour (J/kg) (Fundamental data for B.S. Engineers,
C Section 8, Eqns 7-20.)

      FUNCTION Hv01(TC)

      ISW=INT((TC+20.0)/10.0)
      IF(ISW.LE.1)THEN
        HvkJ=2500.822+1.8375*TC
      elseif(ISW.EQ.2)THEN
        HvkJ=2500.92+1.84*TC
      elseif(ISW.EQ.3)THEN
        HvkJ=2519.32+1.83*(TC-10.0)
      elseif(ISW.EQ.4)THEN
        HvkJ=2537.62+1.82*(TC-20.0)
      elseif(ISW.EQ.5)THEN
        HvkJ=2555.82+1.8*(TC-30.0)
      elseif(ISW.EQ.6)THEN
        HvkJ=2573.82+1.77*(TC-40.0)
      elseif(ISW.GE.7)THEN
        HvkJ=2591.52+1.76*(TC-50.0)
      ENDIF
      Hv01=HvkJ*1000.0
      RETURN
      END

C ******************** Hl01 ********************
C Calculates enthalpy of water (J/kg).

      FUNCTION Hl01(TC)

      Hfg=EVAP01(TC)
      Hg=Hv01(TC)
      Hl01=Hg-Hfg
      RETURN
      END
