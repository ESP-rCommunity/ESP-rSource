
C This file contains the following routines:
C This code is still under testing and develping 
C DynamicOcc: return the latent and sensible load from one typical occupant
C Indentation and tabs corrected 18 July 2016.
	
C************Dynamic occupant subroutine**********************************
      Subroutine DynamicOcc(ICOMP,METAo,CLOTHO)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "schedule.h"
      common/CountTimesteps/i_countsteps(mcom),numbStartTimesteps,
     &   numTotTimstps
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

      common/FVALG/GFA(MCOM)
      common/FVALA/TFA(MCOM)
      common/OCCCFD/TaCFD(MCOM),TTRACK(MCOM)
      common/occload/sklat(MCOM),skcon(MCOM),skrad(MCOM),ttcr(MCOM),
     &     ttsk(MCOM)
      common/ocflux/COflux,H2Oexp,H2Oswt
      common/MOIST53/Ppair(MCOM),Pfair(MCOM),Spair(MCOM)
      COMMON/PVALA/TPA(MCOM),QPA(MCOM)
      REAL TPA,TAIR,H2Oexp,HRexp,Mresp
      COMMON/ATPRES/PATMOS
      REAL TMRT,TOP,Tamb
      real PATMOS ! Ambient pressure 
      integer ICOMP
      REAL  Tcr(2),Tsk(2)
      REAL  Cp_blood   ! Blood specific heat (J/Kg oC)
      parameter (Cp_blood=4000.0)
      real  ro_blood   ! Blood density (g/cm3)
      parameter (ro_blood=1.050)
      real  Cp_body    ! Mean body specific heat (J/Kg oC)
      parameter (Cp_body=3100)
      real  hfg        ! Water heat of evaporation (J/Kg)
      parameter (hfg=2.43e6)
      real  cpair      ! Air specific heat (J/Kg oC)
      parameter (cpair=1005.7)
      real  mtotal     ! Total body mass (kg)
      parameter (mtotal=75.33)
      real  Ask        ! Total body area (m2)
      parameter (Ask=1.8518)
      real  Patm       ! Atmospheric pressure ! Ambiant relative humidity
      parameter (Patm=1.013e2)
      real  Psat_amb,Psatsk  ! Ambiant saturation pressure (kPa)/ skin saturation pressure
      real  Pv_amb           ! Ambiant vapor pressure (kPa)
      real  HR_amb           ! Ambiant humidity ratio
      real  fs_thick                    ! Fat skin thickness (mm)
      parameter (fs_thick=10)
C Heat exchange coefficients at skin
      real  hc,hr,he         ! Convection coefficient at still ambiant air (W/m2 oC) 
                             ! Radiation coefficient (W/m2 oC)Evaporation coefficient (W/m2 kPa)
      real  Rcl,Rcle,fcl     !Clothing resistanve 
      real  shiv,W,Qcrsk,K,Psat,msw,Psk,mshiv,Ccr
      real  resl,ress,sres,lres      ! latent and sensible heat loss from respiration
      real  Pskin,mblsk,Qboold,Csk,PVAMB,mbsk
      real  sklat,skcon,skrad,expo   ! latent, convective, radiative heat loss
      real  METAo,CLOTHO,META
      real  SWEAT                    ! Sweating weighting factor
      real  COLD                     ! Shivering weighting factor
      real t_scounter,time_step,t_sB
      expo=1.5
      SWEAT=1.4
      COLD=1.3                       ! Shivering weighting factor W is the mechanical work
      Ccr=Cp_body*mtotal*0.9
      Csk=Cp_body*mtotal*0.1
      Tcr(1)=36.88                 ! initial conditions
      Tsk(1)=32.00

C Clo = 1 - corresponds to the insulating value of a person wearing a business suit
C sitting in comfort at rest in a room at 21C with air movement of 0.1 m/s and humidity less than 50%
      Rcl=0.155*CLOTHO    !m2k/w
      Rcle=0.02
      fcl=1.4
      t_sB=3600./float(NTSTEP)
      time_step=0.1
      t_scounter=0.1
      INDX=ICOMP*(-1)   ! negate the zone index for use in mzmixt.
      CALL MZMIXT(INDX,TMRT,TMIX)
!      if (TTRACK(ICOMP).LT.1) then
      TAIR=TPA(ICOMP)
!      else
!      TAIR=TaCFD(ICOMP)
!      endif
      TOP=0.5*TMRT+0.5*TAIR 

10    if (t_scounter.LT.t_sB) then

       if (TTcr(ICOMP).GT.5) then  ! if first time called for a zone TTcr(ICOMP) 
                                 ! will be equal to zero if not it will be set to present core temperature
         Tcr(1)=TTcr(ICOMP)
         Tsk(1)=TTsk(ICOMP)
       end if

      META=METAo
      Tamb=TOP + 273.0
      RH=PCRH2(TPA(ICOMP),GFA(ICOMP),PATMOS)/100.0
      Psat_amb=Psat(Tamb)
      Pv_amb=RH*psat_amb
      HR_amb=0.622*pv_amb/(Patm-Pv_amb)

      skT=Tsk(1)+273
      crT=Tcr(1)
      resl=lres(META,Tamb-273.0,HR_amb)
      ress=sres(META,Tamb-273.0,HR_amb)

C Calculation of the skin conductance from the correlations of Havenith.
      Kmuscle=(1.0/0.05)*Ask
      Kfatskin=Ask/((fs_thick-2.0)*0.0048+0.0044)
      K=1.0/(1.0/Kmuscle+1.0/Kfatskin)
      Psatsk=Psat(skT)
      Qcrsk=K*(Tcr(1)-Tsk(1))     ! W=W/K*(K)

C heat transfer coeficients
      hr=0.7*4.0*5.67e-8*0.95*(273.0+(Tsk(1)+Tamb-273)/2)**3
      hc=3      ! could be modified to be calculated from de Dear hc=10.3V^0.6 W/(m2•ºC) 
      he=16.5*hc
      Pskin=Psk(Psatsk,he,Pv_amb,msw(Tcr(1),Tsk(1),Ask,SWEAT))
      shiv=Mshiv(Tcr(1),Tsk(1),cold)
      mblsk=mbsk(Tsk(1),Tcr(1))*ro_blood*Ask/3600.0
      Qbld=mblsk*Cp_blood*(Tcr(1)-Tsk(1))
      Mresp=1.43e-6*META                       !kg or kg/s??
      HRexp=0.2*HR_amb+0.0277+6.5e-5*Tamb
      H2Oexp=Mresp*(HRexp-HR_amb)              !!kg/s?
      H2Oswt=msw(Tcr(1),Tsk(1),Ask,SWEAT)*ASK  !Kg/s
C Calculating the future skin and core temp
      Tsk(2)=(Qcrsk-(Ask/expo)*((Tsk(1)-(Tamb-273))/
     &       (Rcl+1/(fcl*(hc+hr)))+(Pskin-Pv_amb)/(Rcle+1/(fcl*he)))+
     &       mblsk*Cp_blood*(Tcr(1)-Tsk(1)))*
     &       time_step/Csk+Tsk(1)
      Tcr(2)=(META+shiv-resl-ress-Qcrsk+
     &       Cp_blood*mblsk*(Tsk(1)-Tcr(1)))*
     &       time_step/Ccr+Tcr(1)
      sklat(ICOMP)=((Pskin-Pv_amb)/(Rcle+1/(fcl*he)))*Ask/expo+resl
      skcon(ICOMP)=(((Tsk(1)-(Tamb-273))/(Rcl+1/(fcl*(hc+hr)))))*
     &             (Ask/expo)+ress
      skrad(ICOMP)=(((Tsk(1)-(Tamb-273))/(Rcl+1/(fcl*(hc+hr)))))*
     &             (Ask/expo)*0.6
      COflux=skcon(ICOMP)/Ask
      TTcr(ICOMP)=Tcr(2)
      TTSK(ICOMP)=Tsk(2)
      t_scounter=t_scounter+0.1
      Go to 10
      endif
      print*, COflux,' ',H2Oexp,' ',H2Oswt,' ',META,' ',
     &       Tsk(2),' ',Tcr(2),' ',Tamb
      write(33,3330) Tsk(2),Tcr(2)

 3330     format(F9.3,F9.3)
      END

      real function Sres(M,T,HR)
      implicit none
      real, intent(in):: M,T,HR
      real  Tex,mres
      real  cpair
      parameter (cpair=1005.7)
      mres=1.43e-6*M
      Tex=32.6+0.06*T+32.0*HR
      Sres=mres*(cpair*(Tex-T))
      return
      end function Sres

      real function Lres(M1,T1,HR1)
      implicit none
      real, intent(in):: M1,T1,HR1
      real  HRex,mres1
      real  hfg
      parameter (hfg=2.43e6)
      mres1=1.43e-6*M1
      HRex=0.2*HR1+0.0277+6.5e-5*T1
      Lres=mres1*(hfg*(HRex-HR1))      
      return
      end function Lres
      
C Calculates the Control metabolic rate (shivering) (in Watts)
      real function Mshiv(Tcr,Tsk,w)
      implicit none
      real, intent(in):: Tcr,Tsk,w
      real  Tshiver,Mshivmax
      if (Tcr.LE.35.8) then
        Tshiver=35.5
      else
        Tshiver=-1.0222e4+570.97*Tcr-7.9455*Tcr**2
      end if

      if (Tcr.LT.36.7) then
        Mshivmax=-1.1861e9+6.552e7*Tcr-9.0418e5*Tcr**2
      else
        Mshivmax=0.0
      end if
      if (Mshivmax.LE.0) then
        Mshivmax=0.0
      end if
      if ((Tsk.GT.(40-Tshiver)).or.(Tcr.LT.Tshiver)) then
        Mshiv=Mshivmax*(1.0-((Tsk-20.0)/(Tshiver-20.0))**2)
     &       *w/3600.0
        return
      else
        Mshiv=0.0
        return
      end if
      if (Mshiv.LE.0) then
        Mshiv=0.0
      end if
      return
      end function Mshiv

C Calculates the sudomotor response (sweat rate) (Kg/s/m2)
      real function msw(Tcr,Tsk,A,w)
      implicit none
      real, intent(in)::  Tcr,Tsk,w,A
      real  Tsweat

      if (Tsk.LT.33) then
        Tsweat=42.084-0.17833*Tsk
      else
        Tsweat=36.85
      end if

      if (Tcr.GT.Tsweat) then
        msw=(45.8+739.4*(Tcr-Tsweat))*w/3600.0/1000.0/A
        return
      else
        msw=0.0058e-3/A
        return
      end if
      end function msw

C Calculates saturated vapor pressure for a given temperature (kPa)
      real function Psat(T)
      real, intent(in)::  T

C Coefficients of saturation pressure correlation according to ASHRAE
      real  c8,c9,c10,c11,c12,c13
      parameter (c8=-5800.2206,c9=1.3914993,c10=-0.04860239,
     &  c11=0.41764768e-4,c12=-0.14452093e-7,c13=6.549673)
     
      Psat=(exp(c8/T+c9+c10*T+c11*T**2+c12*T**3+c13*log(T)))/1000.0 ! Saturation pressure correlation according to ASHRAE
      return
      end function Psat

C Calculates the vapor pressure at skin (kPa)
      real function Psk(Psatsk,he,Pa,msw)
      real, intent(in)::  Psatsk,he,Pa,msw
        Psk=(Psatsk/he+Pa*0.3+msw*0.3*2.43e6/he)/(1/he+0.3)
      if (Psk>=Psatsk) then
        Psk=Psatsk
      endif
      return
      end function Psk

C Calculates skin blood flow in (L/m2hr)
      real function mbsk(Tsk,Tcr)
      real, intent(in)::  Tsk,Tcr
      real Con,Dil,mbbsk
      Con=0.5*(Tsk-33.7)
      Dil=3.43*(Tcr-36.8)
      mbbsk=(6.3+60*Dil)/(1+Con)
      if (Tsk.LE.29 .or. Tcr.LE.36.2 .or. mbbsk.LT.0.5) then
         mbsk=0.5
      else if(Tcr.GE.37.5 .or. mbbsk .GT.90)then
         mbsk=90
      else
         mbsk=mbbsk
      end if
      return
      end function mbsk
