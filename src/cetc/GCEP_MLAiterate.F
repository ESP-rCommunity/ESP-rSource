C This file is part of the ESP-r system.
C Copyright CANMET Energy Technology Centre 
C Natural Resources Canada, Government of Canada
C 2004. Please Contact Ian Beausoliel-Morrison for details 
C concerning licensing.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

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
C GCEP_MLAiterate
C
C Created by: Patrice Pinel
C Initial Creation Date: October 8, 2002
C
C 
C This subroutine	calculates the inlet and outlet fluid temperature in the borefield
C iterates with a system that provides the ground load and fluid flow
C uses the Multiple Load Agregation Algorithm developped by Michel Bernier
C
C This routine is called from 
C
C This subroutine is called for every iteration until convergence is obtained
C
C INPUTS:
C	GCEPM_INPUT module
C especially
C	Gload       Ground load
C	Flow        Flow of fluid
C 
C OUTPUTS:
C	Rb		thermal resistance of the boreholes
C	Re		Reynold number in the tubes
C	OWT  	Fluid temperature at the entrance of the borefield (exit of the HP)
C	EWT     Fluid temperature at the exit of the borefield (entrance of the HP)
C
C *********************************************************************
      SUBROUTINE GCEP_MLAiterate

C Use module for GCEPM data
C      USE GCEP_INPUT


	IMPLICIT NONE
      INCLUDE 'gcep_parameters.h'
      INCLUDE 'gcep_common.h'
C Average temperature of the fluid
      REAL Tave
      INTEGER i
C Function that calculates the G function of the cylindrical heat source (located in file GCEP_SIM.F)
      REAL FonG

C Initialise the last term of the non agregated loads and the load history to the actual load
      GCEP_ P(GCEP_Xh)=GCEP_Gload
      GCEP_LoadHis(GCEP_NHis)=GCEP_Gload
C Calculate the non agregated hours term
      GCEP_T_h=0.0
      DO 10 i=1,GCEP_Xh-1
        GCEP_T_h = GCEP_T_h+GCEP_P(i)*
     &             (FonG(GCEP_alpha_s,GCEP_D_bore,GCEP_Xh+1-i+0.0001)-
     &              FonG(GCEP_alpha_s,GCEP_D_bore,GCEP_Xh-i+0.0001))
10    CONTINUE
      GCEP_T_h = GCEP_T_h+GCEP_P(GCEP_Xh)*
     &           FonG(GCEP_alpha_s,GCEP_D_bore,1.0001)

C Calculate the thermal resistance of a borehole
      CALL CalcRb(GCEP_Tube_Case,GCEP_K_t,GCEP_D_bore,GCEP_Deapth_bore,
     &            GCEP_K_g,GCEP_K_f,GCEP_Rho_f,GCEP_Cp_f,GCEP_Mu_f,
     &            GCEP_N_bor_x,GCEP_N_bor_y,GCEP_Flow,GCEP_Gload,
     &            GCEP_Di_tubes,GCEP_Do_tubes,GCEP_Rb,GCEP_Re)

C MLA Equation	  
      Tave = (GCEP_Tg_init+GCEP_Tpen-
     &       (GCEP_Gload*GCEP_Rb+1.0/GCEP_K_s*
     &       (GCEP_T_y+GCEP_T_m+GCEP_T_w+GCEP_T_d+GCEP_T_h))/
     &	      GCEP_Deapth_bore/GCEP_N_bor_x/GCEP_N_bor_y)

C Evaluate inlet and outlet temperature of the fluid
      IF(GCEP_Flow.GT.0.0) THEN
        GCEP_OWT=Tave-GCEP_Gload/(0.002*GCEP_Flow*GCEP_Rho_f*GCEP_Cp_f)
      ELSE
        GCEP_OWT=Tave
      ENDIF 
      GCEP_EWT=2*Tave-GCEP_OWT

      END

**************************************************************************
*1
C *********************************************************************
C CalcRb
C
C Created by: Patrice Pinel
C Initial Creation Date: October 8, 2002
C
C This routine calculates the thermal resistance of a borehole
C
C This routine is called from GCEP_MLAiterate
C
C This subroutine is called for every iteration
C since the resistance is a function of the fluid flow
C 
C
C INPUTS:
C	Tube_Case	Tubes layout in borehole (choice)
C	K_t		    Tube properties (k=conductivity)
C	D_bore	    Borehole diameter
C	Deapth_bore	Borehole length
C	K_g		    Grout properties (k=conductivity)
C Fluid property (k=conductivity, rho=density, Cp=specific heat, Mu=dynamic viscosity)
C	K_f
C	Rho_f
C	Cp_f
C	Mu_f
C	N_bor_x	    Number of bores in the X direction
C	N_bor_y	    Number of bores in the Y direction
C	Gload	    Ground load at the actual iteration
C	Flow        Flow of fluid at the actual iteration
C	Di_tubes    Inside tube diameter
C	Do_tubes    Outside tube diameter
C 
C OUTPUTS:
C	Rb          thermal resistance of the boreholes
C	Re	        Reynold number in the tubes
C
C *********************************************************************
      SUBROUTINE CalcRb(Tube_Case,K_t,D_bore,Deapth_bore,K_g,K_f,Rho_f,
     &Cp_f,Mu_f,N_bor_x,N_bor_y,Flow,Gload,Di_tubes,Do_tubes,Rb,Re)

      IMPLICIT NONE 
C     INPUTS
      INTEGER Tube_Case,N_bor_x,N_bor_y
      REAL K_t,D_bore,Deapth_bore,K_g,K_f,Rho_f,Cp_f,Mu_f,Flow,
     &     Gload,Di_tubes,Do_tubes,Rb,Re
C Coefficients for 2 tubes
      REAL Beta0,Beta1,Sc
C Coefficients for 4 tubes (Bu=wall to wall distance between 2 tubes)(B=Bu/D_bore)
      REAL Bu,B
C Flow in one tube (l/s)
      REAL Flow_tube
C Thermal resistance between the fluid and the external wall of the tubes
      REAL Rtube
C Thermal resistance between the the external wall of the tubes and the wall of the borehole
      REAL Rgrout
C Convection coefficient in the tubes
      REAL Hi
C pi universal constant
      REAL pi

      Pi=ATAN(1.0)*4.0
C Coefficients of the Remund method for 2 pipes (1U)
      IF (Tube_Case.EQ.1) THEN
        Beta0=20.1
        Beta1=-0.9447
        Flow_tube=Flow/N_bor_x/N_bor_y
      ELSE IF (Tube_Case.EQ.2) THEN
        Beta0=17.44
        Beta1=-0.6052
        Flow_tube=Flow/N_bor_x/N_bor_y	
      ELSE IF (Tube_Case.EQ.3) THEN
        Beta0=21.91
        Beta1=-0.3796
        Flow_tube=Flow/N_bor_x/N_bor_y	
C Coefficients of Helstrom's equation for 4 pipes (2U)
      ELSE IF (Tube_Case.EQ.4) THEN
        Bu=Do_tubes*(2.0**0.5-1)
        Flow_tube=Flow/N_bor_x/N_bor_y/2.0
      ELSE IF (Tube_Case.EQ.5) THEN
        Bu=(D_bore-2.0*Do_tubes)/3.0
        Flow_tube=Flow/N_bor_x/N_bor_y/2.0
      ELSE
        Bu=D_bore-2.0*Do_tubes
        Flow_tube=Flow/N_bor_x/N_bor_y/2.0
      ENDIF
      CALL Calchi(Deapth_bore,K_f,Rho_f,Cp_f,Mu_f,Flow_tube,Di_tubes,
     &            Gload,Hi,Re)
      Rtube=LOG(Do_tubes/Di_tubes)/(2.0*pi*K_t)+1.0/(pi*Di_tubes*Hi)
C Remund method for 2 pipes (1U)
      IF (Tube_Case.LE.3) THEN
        Sc=Beta0*(D_bore/Do_tubes)**Beta1
        Rgrout=1/(Sc*K_g)
        Rb=Rgrout+Rtube/2.0
      ELSE
C Helstrom's equation for 4 pipes (2U)
        B=Bu/D_bore
        Rgrout=1/(2.0*pi*K_g)*(LOG(D_bore/Do_tubes)-0.75+B**2.0-
     &         0.25*LOG(1.0-B**8.0)-0.5*LOG(2.0**0.5*B*D_bore/Do_tubes)-
     &         0.25*LOG(2.0*B*D_bore/Do_tubes))
        Rb=Rgrout+Rtube/4.0
      ENDIF

      END

***********************************************************************	
*2
C *********************************************************************
C CalcRb
C
C Created by: Patrice Pinel
C Initial Creation Date: October 8, 2002
C
C This routine calculates the convection coefficient inthe tubes
C
C This routine is called from CalcRc
C
C This subroutine is called for every iteration
C since the convection is a function of the fluid flow
C 
C
C INPUTS:
C	Deapth_bore	    Borehole length
C Fluid property (k=conductivity, rho=density, Cp=specific heat, Mu=dynamic viscosity)
C	K_f
C	Rho_f
C	Cp_f
C	Mu_f
C	Gload           Ground load at the actual iteration
C	Di_tubes        Inside tube diameter
C	Flow_tube       Flow in one tube (l/s)
C 
C OUTPUTS:
C	Hi              Convection coefficient in the tubes
C	Re              Reynold number in the tubes
C
C *********************************************************************
      SUBROUTINE Calchi(Deapth_bore,K_f,Rho_f,Cp_f,Mu_f,Flow_tube,
     &                  Di_tubes,Gload,Hi,Re)

      IMPLICIT NONE
C     INPUTS
      REAL Deapth_bore,K_f,Rho_f,Cp_f,Mu_f,Flow_tube,Di_tubes,Gload,
     &     Hi,Re
C fluid velocity in the tubes
      REAL velocity
C friction coeficient in the tubes
      REAL F
C load indicator (0.4 heating, 0.3 cooling)
      REAL N
C Prandtl number of the fluid
      REAL Pr
C Nusselt number in the tubes
      REAL Nu
C pi universal constant
      REAL pi
C Dummy variable
      REAL Tamp
	  
      pi=ATAN(1.0)*4.0
C evaluation of the flow velocity
      velocity=Flow_tube/1000.0/(pi*Di_tubes**2.0/4.0)
C calculation of the Reynold and Prandtle numbers
      Re=Rho_f*velocity*Di_tubes/Mu_f
      Pr=Cp_f*Mu_f/K_f

C evaluation of Nu (3 relations: Laminar, intermediate, turbulent)
      IF(Re.LE.2200.0) THEN
        Tamp=(Re*Pr*Di_tubes/Deapth_bore/2.0)**0.333
        IF (Tamp.GE.2.0) THEN
          Nu=1.86*Tamp
        ELSE
          Nu=3.66
        ENDIF
      ELSE IF(Re.LT.10000.0) THEN
        F=(1.58*LOG(Re)-3.28)**(-2.0)
        Nu=(F/2.0)*(Re-1000.0)*Pr/
     &     (1.0+12.7*(F/2.0)**0.5*(Pr**(1.0/3.0)-1.0))
      ELSE
        IF(Gload.LE.0.0) THEN
* Charge au sol nëgative = climatisation
          N=0.3
        ELSE
*	Chauffage
          N=0.4
        ENDIF
        Nu=0.023*Re**0.8*Pr**N
      ENDIF
C evaluation of the convection in the tubes
      Hi=Nu*K_f/Di_tubes

      END	    

************************************END**********************************
