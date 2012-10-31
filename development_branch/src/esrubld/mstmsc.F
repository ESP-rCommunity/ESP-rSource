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

C *********************************************************************
C BETA01 derives the mass transfer coefficient from heat transfer
C coefficient using Lewis relation (Marieke 1994, Webb 1991 ASHREA
C transactions, V97, pp.11411-7).
C BETA01       convective mass transfer coefficient (s/m)
C R            universal gas constant  8314.4  (J/mole K)
C MOLH2O       molecular mass of water 18.016  (kg/kmole)
C ScPrn1       (Sc/Pr)^(n-1)
C ALPHA        convective heat transfer coefficient (W/m2 K)
C TC           ambient temperature    ( C )
C TK           ambient temperature    ( K )
C CP           air specific heat capacity  (J/kg K)
C *********************************************************************
      FUNCTION BETA01(ALPHA,CP,TC)

      H2OMOL=18.016
      ScPrn1=0.85
      Ru=8314.4
      TK=TC+273.15
      BETA01=ALPHA*H2OMOL*ScPrn1/(CP*Ru*TK)
      IF(BETA01.LT.0.0)
     &  STOP 'Error (701): negative mass transfer coefficient.'
      RETURN
      END
C *********************************************************************
      FUNCTION BETA02(ALPHA)

      BETA02=7.4e-9*ALPHA
      IF(BETA02.LT.0.0)
     &  STOP 'Error (702): negative mass transfer coefficient.'
      RETURN
      END

C *********************************************************************
C U01 determines the value for moisture content as a function of
C relative humidity and sorption isotherms provided by (Marieke 1994,
C Hansen 1986 Technical reprort 162/86 Technical University of Denmark).
C U01   moisture content (kg/m3)
C RH    relative humidity
C Uh    max hygroscopic moisture content (kg/kg)
C CFF   coefficient used to calculate u=f(RH) and xi
C EXPT  exponent used to calculate u=f(RH) and xi
C *********************************************************************
      FUNCTION U01(RH,Uh,CFF,EXPT)
      COMMON/MOIST17/RHmax

      IF(RH.GT.RHmax)RH=RHmax
      U01=Uh*(1.0-ALOG(RH)/CFF)**(-1.0/EXPT)
      IF(U01.LT.0.0)STOP 'Error (703): negative moisture content.'
      RETURN
      END
C *********************************************************************
C U02   moisture content (kg/kg dry material)
C *********************************************************************
      FUNCTION U02(RH,Uh,CFF,EXPT,DNSTY)
      COMMON/MOIST17/RHmax

      IF(RH.GT.RHmax)RH=RHmax
      U02=U01(RH,Uh,CFF,EXPT)/DNSTY
      IF(U02.LT.0.0)STOP 'Error (704): negative moisture content.'
      RETURN
      END

C *********************************************************************
C XI01 determines the value for moisture capacity ( slope for sorption
C isotherm) using curve fitting techniques for sorption isotherms 
C provided by (Marieke 1994, Hansen 1986 Technical reprort 162/86
C Technical University of Denmark).
C XI01  moisture capacity (kg/m3)
C RH    relative humidity (-)
C Uh    max hygroscopic moisture content (kg/kg)
C CFF   coefficient used to calculate u=f(RH) and xi
C EXPT  exponent used to calculate u=f(RH) and xi
C *********************************************************************
      FUNCTION XI01(RH,Uh,CFF,EXPT)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MOIST17/RHmax
      DOUBLE PRECISION XI01
      SMALL=1.0E-15

      IF(RH.GT.RHmax)RH=RHmax
      XI01=Uh/(EXPT*CFF*RH)*(1.0-ALOG(RH)/CFF)**(-(EXPT+1.0)/EXPT)
      IF(XI01.LT.DBLE(SMALL))then
        call edisp(iuout,'XI01: (705) very small moisture capacity.')
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C *********************************************************************
C XI02  moisture capacity (kg/kg)
C *********************************************************************
      FUNCTION XI02(RH,Uh,CFF,EXPT,DNSTY)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MOIST17/RHmax
      DOUBLE PRECISION XI01, XI02
      SMALL=1.0E-15

      IF(RH.GT.RHmax)RH=RHmax
      XI02=XI01(RH,Uh,CFF,EXPT)/DNSTY
      IF(XI02.LT.DBLE(SMALL))then
        call edisp(iuout,'XI02: (706) very small moisture capacity.')
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C *********************************************************************
C XI03  moisture capacity (kg/m3).  The numerical equivalent for XI01.
C *********************************************************************
      FUNCTION XI03(RHp,RHf,Uh,CFF,EXPT)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MOIST17/RHmax
      DOUBLE PRECISION XI01, XI03
      SMALL=1.0E-15

      IF(RHp.GT.RHmax)RHp=RHmax
      IF(RHf.GT.RHmax)RHf=RHmax
      RHfp=RHf-RHp
      IF(ABS(RHfp).LT.1.0e-6)THEN
        XI03=XI01(RHp,Uh,CFF,EXPT)
      ELSE
        Up=U01(RHp,Uh,CFF,EXPT)
        Uf=U01(RHf,Uh,CFF,EXPT)
        XI03=(Uf-Up)/RHfp
        IF(XI03.LT.0.0)XI03=XI01(RHp,Uh,CFF,EXPT)
      ENDIF
      IF(XI03.LT.DBLE(SMALL))then
        call edisp(iuout,'XI03: very small moisture capacity.')
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C *********************************************************************
C XIair01 determines the value for moisture capacity ( slope for sorption
C isotherm) for air node by numerical differentiation.
C XIair01  moisture capacity (kg/kg)
C Pvp      present time vapour pressure ( Pa )
C Pvf      future time vapour pressure ( Pa )
C TCp      present time temperature ( C )
C TCf      future time temperature ( C )
C *********************************************************************
      FUNCTION XIair01(Pvp,Pvf,TCp,TCf,Pt)
      COMMON/OUTIN/IUOUT,IUIN
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
      ELSE
        Xp=X03(TCp,RHp,Pt)
        Xf=X03(TCf,RHf,Pt)
        XIair01=(Xf-Xp)/RHfp
        IF(XIair01.LT.0.0)XIair01=XIair02(Pvp,TCp,Pt)
      ENDIF
      IF(XIair01.LT.DBLE(SMALL))then
        call edisp(iuout,
     &'XIair01: small moisture cap for case of: Pvp Pvf TCp TCf Pt')
        write(outs,*) Pvp,Pvf,TCp,TCf,Pt,' resetting'
        call edisp(iuout,outs)
        XIair01=1.0E-10
      endif
      RETURN
      END

C *********************************************************************
C XIair02 determines the value for moisture capacity ( slope for sorption
C isotherm) for air node by differentiating the moisture content formulae
C with respect to relative humidity at fixed temperature and total 
C pressure. (No reference.)
C *********************************************************************
      FUNCTION XIair02(Pv,TC,Pt)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MOIST17/RHmax
      SMALL=1.0E-15

      Ra=287.06
      Rv=461.5
      RATIO=Ra/Rv
      Ps=Psat01(TC)
      RH=Pv/Ps
      IF(RH.GT.RHmax)RH=RHmax
      XIair02=RATIO*Ps*Pt/(Pt-Ps*RH)**2
      IF(XIair02.LT.DBLE(SMALL))then
        call edisp(iuout,'XIair02: very small moisture capacity.')
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C *********************************************************************
C XIair03 equivalent to XIair02 but based on numerical approach.
C *********************************************************************
      FUNCTION XIair03(Pv,TC,Pt)
      COMMON/OUTIN/IUOUT,IUIN
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
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C *********************************************************************
C PHI01 determines the value for relative humidity from moisture
C content and sorption isotherms. provided by (Marieke 1994, Hansen 
C 1986 Technical reprort 162/86 Technical University of Denmark).
C PHI01  relative humidity (-)
C U      moisture content (kg/kg dry material)
C Uh    max hygroscopic moisture content (kg/kg)
C CFF   coefficient used to calculate u=f(RH) and xi
C EXPT  exponent used to calculate u=f(RH) and xi
C *********************************************************************
      FUNCTION PHI01(U,Uh,CFF,EXPT)
      COMMON/OUTIN/IUOUT,IUIN

      PHI01=EXP(CFF-CFF*(U/Uh)**(-EXPT))
      IF(PHI01.LT.0.0)then
        call edisp(iuout,'PHI01: negative relative humidity.')
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C *********************************************************************
C PHI02 determines the value for relative humidity from vapour ratio,
C pressure, and temperature (Heating, Ventilating, and Air Conditioning
C by McQuiston and Parker).
C *********************************************************************
      FUNCTION PHI02(TC,X,Pt)
      COMMON/OUTIN/IUOUT,IUIN

      Ra=287.06
      Rv=461.5
      Ps=Psat01(TC)
      Pv=Pvpr01(X,Pt)
      Pa=Pt-Pv
      PHI02=X*Pa*Rv/(Ps*Ra)
      IF(PHI02.LT.0.0)then
        call edisp(iuout,'PHI02: negative relative humidity.')
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C *********************************************************************
C EVAP01 determines the latent heat of evapouration (transformation) as 
C a function of temperature (Marieke 1994).
C EVAP01   latent heat of evapouration  (J/kgv)
C EVAPr    reference heat of condensation at T=0 C  (J/kgv)
C CPlqd    specific heat capacity for liquid water  (J/kgv K)
C CPvap    specific heat capacity for vapour water  (J/kgv K)
C DCP=CPlqd-CPvap
C TC       temperature in ( C )
C *********************************************************************
      FUNCTION EVAP01(TC)
      COMMON/OUTIN/IUOUT,IUIN
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

C *********************************************************************
C DELT01 determines the vapour permeability of the material as 
C a function of (RH). (Catalogue of Material properties-upgraded version)
C DELT01  water vapour permeability (s)
C VPRMa   vapour permeability factor a
C VPRMb   vapour permeability factor b
C VPRMc   vapour permeability factor c
C RH      relative humidity (-)
C *********************************************************************
      FUNCTION DELT01(RH,VPRMa,VPRMb,VPRMc)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MOIST17/RHmax
      character outs*124
      SMALL=1.0E-15

      IF(RH.GT.RHmax)RH=RHmax
      DELT01=1.89923e-10*(VPRMa+VPRMb*EXP(VPRMc*RH))
      IF(DELT01.LT.SMALL)then
        write(outs,'(a,4F12.6,a)') 
     &   'DELT01: small vapour perm for: VPRMa VPRMb VPRMc RH',
     &    VPRMa,VPRMb,VPRMc,RH,' resetting to 1.0E-13'
        call edisp(iuout,outs)
        DELT01=1.0E-13
      endif
      RETURN
      END

C *********************************************************************
C DELT03 determines the vapour permeability of the material as 
C a function of (RH). (Marieke 1994, Ricken 1989)
C DELT03  water vapour permeability (s)
C VRF0    water vapour resistance factor at RH=0%
C VRF100  water vapour resistance factor at RH=100%
C B       coefficient
C C       constant
C RH      relative humidity (-)
C *********************************************************************
      FUNCTION DELT03(RH,VRF0,VRF100,b)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MOIST17/RHmax
      SMALL=1.0E-15

      IF(RH.GT.RHmax)RH=RHmax
      DELT03=1.875e-10/(VRF0-(VRF0-VRF100)*RH**b)
      IF(DELT03.LT.SMALL)then
        call edisp(iuout,'DELT03: very small vapour permeability.')
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C *********************************************************************
C CPair01 calculates the specific heat capacity of dry air (J/kg K)
C as a function of temperature ( C ) (Marieke 1994, Ham 1984)
C *********************************************************************
      FUNCTION CPair01(TC)
      COMMON/OUTIN/IUOUT,IUIN

      CPair01=1006.0+TC/200.0+TC*TC/7.5E3
      IF(CPair01.LT.0.0)then
        call edisp(iuout,'CPair01: negative specific heat capacity.')
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C *********************************************************************
C CPvpr01 calculates the specific heat capacity of water vapour (J/kg K)
C as a function of temperature ( C ) (Marieke 1994, Ham 1984)
C *********************************************************************
      FUNCTION CPvpr01(TC)
      COMMON/OUTIN/IUOUT,IUIN

      CPvpr01=1858.4+0.10875*TC+3.083E-4*TC*TC
      IF(CPvpr01.LT.0.0)then
        call edisp(iuout,'CPvpr01: negative specific heat capacity.')
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C *********************************************************************
C CPmst01 calculates the specific heat capacity of moist air (J/kg K)
C as a function of temperature ( C ) and moisture content (kg/kg dry air)
C (Marieke 1994, Ham 1984).
C *********************************************************************
      FUNCTION CPmst01(TC,X)
      COMMON/OUTIN/IUOUT,IUIN

      CPmst01=CPair01(TC)+X*CPvpr01(TC)
      IF(CPmst01.LT.0.0)then
        call edisp(iuout,'CPvpr01: negative specific heat capacity.')
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C *********************************************************************
C RHOdry01 calculates the density of dry air (kg/m^3) at known temperature
C and pressure using the ideal gas law
C Ra    gas constant for dry air (J/kgK)
C TC    temperature ( C)
C Pdry  pressure (Pa)
C *********************************************************************
      FUNCTION RHOdry01(TC)
      COMMON/OUTIN/IUOUT,IUIN
      SMALL=1.0E-15

      RHOdry01=DENSIT(1,TC)
      IF(RHOdry01.LT.SMALL)then
        call edisp(iuout,'RHOdry01: very small density.')
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C *********************************************************************
      FUNCTION RHOdry02(TC,Pt)
      COMMON/OUTIN/IUOUT,IUIN
      SMALL=1.0E-15

      Ra=287.06
      TK=TC+273.15
      RHOdry02=Pt/(TK*Ra)
      IF(RHOdry02.LT.SMALL)then
        call edisp(iuout,'RHOdry02: very small density.')
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END
C *********************************************************************
C Psat01 calculates the saturation vapour pressure (Pa) as a function of
C temperature ( C ) (Marieke 1994, IEA 1991, vol 1, page 4.8)
C *********************************************************************
      FUNCTION Psat01(TC)
      COMMON/OUTIN/IUOUT,IUIN
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
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C *********************************************************************
C Pvpr01 calculates the vapour pressure ( Pa ) knowing the total pressure
C ( Pa ) and vapour ratio (kg/kg) (Marieke 1994).
C *********************************************************************
      FUNCTION Pvpr01(X,Pt)
      COMMON/OUTIN/IUOUT,IUIN

      Ra=287.06
      Rv=461.5
      Pvpr01=X*Pt/((Ra/Rv)+X)
      IF(Pvpr01.LT.0.0)then
        call edisp(iuout,'Pvpr01: negative vapour pressure.')
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C *********************************************************************
C Pvpr02 calculates the vapour pressure ( Pa ) knowing the temperature
C ( C ) and relative humidity (-) (Marieke 1994).
C *********************************************************************
      FUNCTION Pvpr02(TC,RH)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MOIST17/RHmax

      IF(RH.GT.RHmax)RH=RHmax
      Pvpr02=RH*Psat01(TC)
      IF(Pvpr02.LT.0.0)then
        call edisp(iuout,'Pvpr02: negative vapour pressure.')
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C *********************************************************************
C RHOmst01 calculates the density of moist air (kg/m^3) knowing the
C temperature ( C ), vapour pressure ( Pa ) and total pressure ( Pa )
C (Marieke 1994)
C Ra   gas constant for dry air     (J/kgK)
C Rv   gas constant for water vapour (J/kgK)
C *********************************************************************
      FUNCTION RHOmst01(TC,Pv,Pt)
      COMMON/OUTIN/IUOUT,IUIN
      SMALL=1.0E-15

      Ra=287.06
      Rv=461.5
      TK=TC+273.15
      RHOmst01=(Pt-Pv)/(Ra*TK)+Pv/(Rv*TK)
      IF(RHOmst01.LT.SMALL)then
        call edisp(iuout,'RHOmst01: very small density.')
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C *********************************************************************
C RHOmst02 calculates the density of moist air (kg/m^3) knowing the
C temperature ( C ), vapour pressure ( Pa ) and total pressure ( Pa )
C (Marieke 1994)
C *********************************************************************
      FUNCTION RHOmst02(TC,X,Pt)
      COMMON/OUTIN/IUOUT,IUIN
      SMALL=1.0E-15

      Pvpr=Pvpr01(X,Pt)
      RHOmst02=RHOmst01(TC,Pvpr,Pt)
      IF(RHOmst02.LT.SMALL)then
        call edisp(iuout,'RHOmst02: very small density.')
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C *********************************************************************
C RHOmst03 calculates the density of moist air (kg/m^3) knowing the
C temperature ( C )and relative humidity (-) (Marieke 1994)
C *********************************************************************
      FUNCTION RHOmst03(TC,RH,Pt)
      COMMON/MOIST17/RHmax
      COMMON/OUTIN/IUOUT,IUIN
      SMALL=1.0E-15

      IF(RH.GT.RHmax)RH=RHmax
      Pvpr=RH*Psat01(TC)
      RHOmst03=RHOmst01(TC,Pvpr,Pt)
      IF(RHOmst03.LT.SMALL)then
        call edisp(iuout,'RHOmst03: very small density.')
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C *********************************************************************
C Pt01 calculates the total pressure ( Pa ) knowing the altitude H (m)
C and temperature T ( C ) (Marieke 1994, Ham 1984)
C G     acceleration due to gravity = 9.81 (m/s^2)
C Pstnd standard total (atmospheric) pressure altitude (Pa)
C Rav   gas constant for moist air (average) (J/kgK)
C **********************************************************************
      FUNCTION Pt01(TC,H)
      COMMON/OUTIN/IUOUT,IUIN

      Pstnd=101325
      G=9.81
      Rav=286.8
      TK=TC+273.15
      Pt01=Pstnd*EXP(-G*H/(Rav*TK))
      IF(Pt01.LT.0.0)then
        call edisp(iuout,'Pt01: negative total pressure.')
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C *********************************************************************
C X01 calculates the vapour ratio (kg vapour/kg dry air) using vapour
C pressure (Pa) (Marieke 1994).
C Ra   gas constant for dry air     (J/kgK)
C Rv   gas constant for water vapour (J/kgK)
C Pt   total pressure  ( Pa )
C **********************************************************************
      FUNCTION X01(Pv,Pt)
      COMMON/OUTIN/IUOUT,IUIN

      Ra=287.06
      Rv=461.5
      Pa=Pt-Pv
      X01=Pv*Ra/(Pa*Rv)
      IF(X01.LT.0.0)then
        call edisp(iuout,'X01: negative vapour ratio.')
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C *********************************************************************
C X02 calculates the vapour ratio (kg vapour/kg dry air) using vapour
C pressure (Pa) (Marieke 1994).
C Ra   gas constant for dry air     (J/kgK)
C Rv   gas constant for water vapour (J/kgK)
C Pt   total pressure  ( Pa )
C TC   temperature ( C )
C (Heating, Ventilating, and Air Conditioning by McQuiston and Parker).
C **********************************************************************
      FUNCTION X02(TC,RH,Pt)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MOIST17/RHmax

      IF(RH.GT.RHmax)RH=RHmax
      Ps=Psat01(TC)
      Pv=RH*Ps
      X02=X01(Pv,Pt)
      IF(X02.LT.0.0)then
        call edisp(iuout,'X02: negative vapour ratio.')
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C *********************************************************************
C X03 calculates the vapour ratio (kg vapour/kg dry air) using vapour
C pressure (Pa).
C **********************************************************************
      FUNCTION X03(TC,RH,Pt)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MOIST17/RHmax

      IF(RH.GT.RHmax)RH=RHmax
      RH100=RH*100.0
      PATMOS=Pt/100.0
      X03=HUMRT1(TC,RH100,PATMOS,0)
      IF(X03.LT.0.0)then
        call edisp(iuout,'X03: negative vapour ratio.')
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C *********************************************************************
C ENTHP01 calculates the enthalpy of moist air using temperature ( C )
C and vapour ratio (kg/kg) (Marieke 1994).
C CPair specific heat capacity of dry air (J/kg K)
C CPvpr specific heat capacity of water vapour (J/kg K)
C *********************************************************************
      FUNCTION ENTHP01(TC,X)
      COMMON/OUTIN/IUOUT,IUIN

      ENTHP01=CPair01(TC)*TC+CPvpr01(TC)*X*TC+X*EVAP01(TC)
      IF(ENTHP01.LT.0.0)then
        call edisp(iuout,'ENTHP01: negative enthalpy.')
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C *********************************************************************
C RH01 calculates the relative humidity using vapour pressure ( Pa ) and 
C saturation vapour pressure (Pa) (Marieke 1994).
C *********************************************************************
      FUNCTION RH01(Pv,Ps)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MOIST17/RHmax

      RH01=Pv/Ps
      IF(RH01.GT.RHmax)RH01=RHmax
      IF(RH01.LT.0.0)then
        call edisp(iuout,'RH01:  relative humidity.')
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END

C *********************************************************************
C Hv01 Calculates enthalpy of vapour (J/kg).
C fundamental data for B.S. engineers, Section 8, Eqns 7-20.
C *********************************************************************
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

C *********************************************************************
C Hl01 Calculates enthalpy of water (J/kg).
C *********************************************************************
      FUNCTION Hl01(TC)

      Hfg=EVAP01(TC)
      Hg=Hv01(TC)
      Hl01=Hg-Hfg
      RETURN
      END
