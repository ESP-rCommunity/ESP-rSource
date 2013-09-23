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

C*********************************************************************
C GCEPtground
C
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C
C 
C This subroutine calculates numerically the temperatures in the ground
C
C This routine is called from GCEP_MLAInit
C
C This subroutine is called every time Inter_Tg hours pass
C By default this should be for every 2 weeks simulated
C
C INPUTS:
C	GCEP_INPUT module
C	Files containing previous temperatures and Patankar coefficients
C 
C OUTPUTS:
C	Tg:		Average temperature of the soil in the borefield region
C	Tw:		Average temperature at the walls of the boreholes
C	Tw_1b:	Average temperature at the walls of the boreholes if ther were only one borehole
C	Tp:		Temperature penalty for boreholes thermal interference (Tw-Tw_1b)
C*********************************************************************
      SUBROUTINE GCEPtground

C Use module for GCEPM data
C      USE GCEP_INPUT

      IMPLICIT NONE

#include "gcep_parameters.h"
#include "gcep_common.h"

C average ground load since last calculation
      REAL Pave
C Vectors containing the dimensions of the elements
      REAL dx(GCEP_Ndx),dy(GCEP_Ndy),
     &     dx_1b(GCEP_Ndx_1b),dy_1b(GCEP_Ndy_1b)
C Tables containing all temperatures in the ground
      REAL Ttemp(GCEP_Ndy,GCEP_Ndx),Ttemp_1b(GCEP_Ndy_1b,GCEP_Ndx_1b)
C Tables containing the coefficients of conduction (Ae,An), of thermal inertia (Aop) and the sum (Ap)
      REAL Ae(GCEP_Ndy,GCEP_Ndx),An(GCEP_Ndy,GCEP_Ndx),
     &     Aop(GCEP_Ndy,GCEP_Ndx),Ap(GCEP_Ndy,GCEP_Ndx),
     &     Ae_1b(GCEP_Ndy_1b,GCEP_Ndx_1b),
     &     An_1b(GCEP_Ndy_1b,GCEP_Ndx_1b),
     &     Aop_1b(GCEP_Ndy_1b,GCEP_Ndx_1b),
     &     Ap_1b(GCEP_Ndy_1b,GCEP_Ndx_1b)
C Function that calculates the average temperature in the borefield
      REAL CalcTg
C Function that calculates the average temperature at the borehole walls
      REAL CalcTw
C Function that calculates the average ground load
      REAL AveLoad

      
C Reading of the element dimensions, Patankar coefficients and temperatures at the last calculation
      CALL ReadDim(GCEP_Ndx,GCEP_Ndy,GCEP_Ndx_1b,GCEP_Ndy_1b,
     &             dx,dy,dx_1b,dy_1b)
      CALL ReadPat(GCEP_Ndx,GCEP_Ndy,Ae,An,Aop,Ap)
      CALL ReadTemp(GCEP_Ndx,GCEP_Ndy,Ttemp)
      CALL ReadPat_1b(GCEP_Ndx_1b,GCEP_Ndy_1b,Ae_1b,An_1b,Aop_1b,Ap_1b)
      CALL ReadTemp_1b(GCEP_Ndx_1b,GCEP_Ndy_1b,Ttemp_1b)

C Calculation of the average load since the last calculation of Tg
      Pave=-(AveLoad(GCEP_NHis,GCEP_LoadHis,
     &       GCEP_NHis-INT(GCEP_Inter_Tg),GCEP_NHis-1))/
     &      (GCEP_Deapth_bore*GCEP_N_bor_x*GCEP_N_bor_y)

	  
C Calculation of The temperature of each element
      CALL Calc_Te(GCEP_N_bor_x,GCEP_N_bor_y,GCEP_Tg_ND,
     &            GCEP_Ndx,GCEP_Ndy,Ae,An,Aop,Ap,
     &            GCEP_Ndx_res,GCEP_Ndy_res,GCEP_Ndx_bor,
     &            GCEP_Ndy_bor,GCEP_Ndx_bor_c,GCEP_Ndy_bor_c,
     &            Ttemp,Pave)
      CALL Calc_Te(1,1,GCEP_Tg_ND,GCEP_Ndx_1b,GCEP_Ndy_1b,
     &            Ae_1b,An_1b,Aop_1b,Ap_1b,
     &            GCEP_Ndx_res,GCEP_Ndy_res,GCEP_Ndx_bor,GCEP_Ndy_bor,
     &            GCEP_Ndx_bor_c,GCEP_Ndy_bor_c,Ttemp_1b,
     &            Pave)
C calculation of the average ground temperature in the borefield
      GCEP_Tg=CalcTg(GCEP_Ndx,GCEP_Ndy,Aop,Ttemp,
     &               GCEP_Ndx_res,GCEP_Ndy_res)
C calculation of borehole walls temperature
      GCEP_Tw=CalcTw(GCEP_N_bor_x,GCEP_N_bor_y,GCEP_Ndx,GCEP_Ndy,
     &          GCEP_Ndx_res,GCEP_Ndy_res,
     &          GCEP_Ndx_bor,GCEP_Ndy_bor,GCEP_Ndx_bor_c,GCEP_Ndy_bor_c,
     &          Ttemp,dx,dy)
      GCEP_Tw_1b=CalcTw(1,1,GCEP_Ndx_1b,GCEP_Ndy_1b,
     &             GCEP_Ndx_res,GCEP_Ndy_res,GCEP_Ndx_bor,GCEP_Ndy_bor,
     &             GCEP_Ndx_bor_c,GCEP_Ndy_bor_c,
     &             Ttemp_1b,dx_1b,dy_1b)
C calculation of the temperature penalty
      GCEP_TPen=GCEP_Tw-GCEP_Tw_1b
		
C saving of the temperatures, Subroutines located in GCEPInit File
      CALL StockTemp(GCEP_ndx,GCEP_ndy,Ttemp)
      CALL StockTemp_1b(GCEP_ndx_1b,GCEP_ndy_1b,Ttemp_1b)

      END

*********************************************************************************
*1
C *********************************************************************
C Calc_Te
C
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C
C This subroutine calculates the temperatures of the all elements 
C
C This routine is called from GCEPtground
C
C This subroutine is called every time Inter_Tg hours pass
C By default this should be for every 2 weeks simulated
C
C INPUTS:
C	N_bor_x	    Number of bores in the X direction
C	N_bor_y	    Number of bores in the Y direction
C	Tg_ND		Undisturbed ground temperature
C     Ndx_bor		Number of elements to mesh a complete bore in the X direction (11)
C	Ndy_bor		Number of elements to mesh a complete bore in the Y direction (11)
C	Ndx_bor_c	Number of elements to mesh half a bore (side of a borehole) in the X direction (5)
C	Ndy_bor_c	Number of elements to mesh half a bore (side of a borehole) in the Y direction (5)
C	Ndx_res		Number of elements to mesh a thermal reservoir in the X direction (20)
C	Ndy_res		Number of elements to mesh a thermal reservoir in the Y direction (20)
C	Ndx         Number of elements to mesh the entire domain in the X direction
C	Ndy	        Number of elements to mesh the entire domain in the Y direction
C	Ae	        Table containing the east-west conduction coefficients of the elements
C	An          Table containing the north-south conduction coefficients of the elements
C	Aop	        Table containing the thermal inertia of the elements
C	Ap	        Table containing the sum of the Patanker coefficients for each element
C	Pave        average ground load since last calculation
C 
C OUTPUTS:
C	Ttemp       Table containing all temperatures in the ground
C
C *********************************************************************
	SUBROUTINE Calc_Te(N_bor_x,N_bor_y,Tg_ND,Ndx,Ndy,Ae,An,
     &    Aop,Ap,Ndx_res,Ndy_res,Ndx_bor,Ndy_bor,
     &    Ndx_bor_c,Ndy_bor_c,Ttemp,Pave)

       
C     INPUTS
      INTEGER N_bor_x,N_bor_y,Ndx,Ndy,Ndx_res,Ndy_res
      INTEGER Ndx_bor,Ndy_bor,Ndx_bor_c,Ndy_bor_c
      REAL Tg_ND,Ae(Ndy,Ndx),An(Ndy,Ndx),Aop(Ndy,Ndx)
      REAL Ap(Ndy,Ndx),Ttemp(Ndy,Ndx),Pave

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      integer ifil,iuout,iuin

C Counter for the number of iterations
      INTEGER Niter
C Convergence criteria
      REAL ConvLoc
C Table containing the source term for every element
      REAL Source(Ndy,Ndx)
C Table containing the sum of the internal energy and the source term for each element
      REAL B(Ndy,Ndx)
C Table containing temperatures from the previous iteration
      REAL Ttampon(Ndy,Ndx)
C Residu is the larges difference between Ttemp and Ttampon and CalcResidu is the function to calculate it
      REAL Residu,CalcResidu
C Initial convergence criteria
      REAL ConvInit
      PARAMETER (ConvInit=0.0001)
C Relaxation factor
	REAL Relax
	PARAMETER (Relax=1.0)
C Number of iterations before the convergence criteria is doubled
	INTEGER NiterMax
	PARAMETER (NiterMax=10)

C If convergence (Residu<convergence) is not obtained after NiterMax iterations
C The convergence criteria is doubled, a warning is given and the calculations are continued
      Niter=0
      ConvLoc=ConvInit
C Initialise the source term
      CALL init_source(N_bor_x,N_bor_y,Ndx,Ndy,Pave,
     &	             Ndx_res,Ndy_res,Ndx_bor,Ndy_bor,
     &	             Ndx_bor_c,Ndy_bor_c,Source)
      CALL InitB(Ndx,Ndy,Aop,Source,Ttemp,B)
C Ttampon=Ttemp
      CALL ReinitTtemp(Ndx,Ndy,Ttemp,Ttampon)
110   CONTINUE
C If the NiterMax iterations have been done, double the convergence criteria and emit warning 
        IF (Niter.GT.NiterMax) THEN
           ConvLoc=2.0*ConvLoc
           Niter=0
	ENDIF
        Niter=Niter+1
C Solve using sweeps in the Y direction of X solutions
        CALL ItereX(Relax,Tg_ND,Ndx,Ndy,Ae,An,Ap,B,Ttemp)
        CALL ReinitTtemp(Ndx,Ndy,Ttemp,Ttampon)
C Solve using sweeps in the X direction of Y solutions
        CALL ItereY(relax,Tg_ND,Ndx,Ndy,Ae,An,Ap,B,Ttemp)
C Compare results from the two sweeps
        Residu=CalcResidu(Ndx,Ndy,Ttemp,Ttampon)
C Check for convergence
        IF (Residu.GT.ConvLoc) THEN
          GOTO 110
        ENDIF
        IF (ConvLoc.GT.0.05) THEN
          WRITE(IUOUT,*) 'Warning GCEP: Troublesome convergence: Verify'
        ENDIF
      END

***************************************************************
*2
C *********************************************************************
C CalcTg
C
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C
C This subroutine calculates the average temperature in the borefield 
C
C This routine is called from GCEPtground
C
C This subroutine is called every time Inter_Tg hours pass
C By default this should be for every 2 weeks simulated
C
C INPUTS:
C     Ndx_res   Number of elements to mesh a thermal reservoir in the X direction (20)
C     Ndy_res	  Number of elements to mesh a thermal reservoir in the Y direction (20)
C     Ndx	      Number of elements to mesh the entire domain in the X direction
C     Ndy	      Number of elements to mesh the entire domain in the Y direction
C     Ttemp     Table containing all temperatures in the groundC
C     Aop	      Table containing the thermal inertia of the elements
C     Ap	      Table containing the sum of the Patanker coefficients for each element
C 
C OUTPUTS:
C	Tg:       Average temperature of the soil in the borefield region
C
C *********************************************************************
	REAL FUNCTION CalcTg(Ndx,Ndy,Aop,Ttemp,Ndx_res,Ndy_res)

      IMPLICIT NONE 
C     INPUTS
      INTEGER Ndx,Ndy,Ndx_res,Ndy_res,i,j
      REAL Aop(ndy,ndx),Ttemp(Ndy,Ndx)

C Partial sums somme1=thermal inertia*Temperature, somme2=thermal inertia
      REAL Somme1,Somme2

      Somme1=0.0
      Somme2=0.0
      DO 220 j=Ndy_res+2,Ndy-Ndy_res-1
        DO 210 i=Ndx_res+2,Ndx-Ndx_res-1
          Somme1=Somme1+Aop(j,i)*Ttemp(j,i)
          Somme2=Somme2+Aop(j,i)
210     CONTINUE
220   CONTINUE
      CalcTg=Somme1/Somme2
      
	END
*****************************************************************
*3
C *********************************************************************
C CalcTw
C
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C
C This subroutine calculates the average temperature of the borehole walls 
C
C This routine is called from GCEPtground
C
C This subroutine is called every time Inter_Tg hours pass
C By default this should be for every 2 weeks simulated
C
C INPUTS:
C	N_bor_x	    Number of bores in the X direction
C	N_bor_y	    Number of bores in the Y direction
C     Ndx_bor		Number of elements to mesh a complete bore in the X direction (11)
C	Ndy_bor		Number of elements to mesh a complete bore in the Y direction (11)
C	Ndx_bor_c	Number of elements to mesh half a bore (side of a borehole) in the X direction (5)
C	Ndy_bor_c	Number of elements to mesh half a bore (side of a borehole) in the Y direction (5)
C	Ndx_res		Number of elements to mesh a thermal reservoir in the X direction (20)
C	Ndy_res		Number of elements to mesh a thermal reservoir in the Y direction (20)
C	Ndx		    Number of elements to mesh the entire domain in the X direction
C	Ndy		    Number of elements to mesh the entire domain in the Y direction
C	dx		    Vector containing the dimensions of the elements in the X direction
C	dy		    Vector containing the dimensions of the elements in the Y direction
C	Ttemp		Table containing all temperatures in the ground
C 
C OUTPUTS:
C	Tw:		    Average temperature at the walls of the boreholes
C
C *********************************************************************
	REAL FUNCTION CalcTw(N_bor_x,N_bor_y,Ndx,Ndy,Ndx_res,Ndy_res,
     &                     Ndx_bor,Ndy_bor,Ndx_bor_c,Ndy_bor_c,
     &                     Ttemp,dx,dy)

      IMPLICIT NONE 
C     INPUTS
      INTEGER N_bor_x,N_bor_y,Ndx,Ndy,Ndx_res,Ndy_res,
     &        Ndx_bor,Ndy_bor,Ndx_bor_c,Ndy_bor_c,i,j,px,py
      REAL Ttemp(Ndy,Ndx),dx(Ndx),dy(Ndy)

C Partial Sum
      REAL Somme

      Somme=0.0
      DO 320 j=1,N_bor_y
C Y position
        py=Ndy_res+(j-1)*Ndy_bor+Ndy_bor_c+2
        DO 310 i=1,N_bor_x
C X position
          px=Ndx_res+(i-1)*Ndx_bor+Ndx_bor_c+2
C Interpolation between boreholes and neighboring elements
          Somme=Somme
     &+(Ttemp(py,px)*dx(px-1)+Ttemp(py,px-1)*dx(px))/(dx(px-1)+dx(px))
     &+(Ttemp(py,px)*dx(px+1)+Ttemp(py,px+1)*dx(px))/(dx(px+1)+dx(px))
     &+(Ttemp(py,px)*dy(py-1)+Ttemp(py-1,px)*dy(py))/(dy(py-1)+dy(py))
     &+(Ttemp(py,px)*dy(py+1)+Ttemp(py+1,px)*dy(py))/(dy(py+1)+dy(py))
310     CONTINUE
320   CONTINUE
      CalcTw=Somme/(N_bor_x*N_bor_y*4.0)
      
	END

*****************************************************************
*4
C *********************************************************************
C MoyC
C
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C
C This subroutine calculates the average ground charge value between two times 
C
C This routine is called from GCEPtground	and CalcPMoys (file GCEP_MLAinit.f)
C
C This subroutine is called every time Inter_Tg hours pass
C By default this should be for every 2 weeks simulated
C
C INPUTS:
C	H1		First hour
C	H2		Last hour
C	nHis    number of terms in the HisC vector
C	HisC    vector containing the history of the ground loads
C 
C OUTPUTS:
C	MoyC:   average ground charge value
C
C *********************************************************************
	REAL FUNCTION AveLoad(NHis,HistC,H1,H2)

      IMPLICIT NONE 
C     INPUTS
      INTEGER NHis,H1,H2,i
      REAL HistC(NHis)
C Partial sum of the ground charges
      REAL Somme

      Somme=0.0
      DO 410 i=H1,H2
        Somme=Somme+HistC(i)
410   CONTINUE
        AveLoad=Somme/(H2-H1+1.0)
      
	END

***************************************************************************
*5
***************************************************************************
C Init_source
C
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C
C This subroutine initialise the source term 
C
C This routine is called from CalcTe
C
C This subroutine is called every time Inter_Tg hours pass
C By default this should be for every 2 weeks simulated
C
C INPUTS:
C	N_bor_x	    Number of bores in the X direction
C	N_bor_y	    Number of bores in the Y direction
C       Ndx_bor	Number of elements to mesh a complete bore in the X direction (11)
C	Ndy_bor		Number of elements to mesh a complete bore in the Y direction (11)
C	Ndx_bor_c	Number of elements to mesh half a bore (side of a borehole) in the X direction (5)
C	Ndy_bor_c	Number of elements to mesh half a bore (side of a borehole) in the Y direction (5)
C	Ndx_res		Number of elements to mesh a thermal reservoir in the X direction (20)
C	Ndy_res		Number of elements to mesh a thermal reservoir in the Y direction (20)
C	Ndx		    Number of elements to mesh the entire domain in the X direction
C	Ndy		    Number of elements to mesh the entire domain in the Y direction
C	Pave		average ground charge since last calculation
C 
C OUTPUTS:
C	Source		Table containing the source term for every element
C
C *********************************************************************
      SUBROUTINE init_source(N_bor_x,N_bor_y,Ndx,Ndy,Pave,
     &                       Ndx_res,Ndy_res,Ndx_bor,Ndy_bor,
     &                       Ndx_bor_c,Ndy_bor_c,Source)

      IMPLICIT NONE 
C     INPUTS
      INTEGER Ndx,Ndy,Ndx_res,Ndy_res,Ndx_bor,Ndy_bor,
     &        Ndx_bor_c,Ndy_bor_c,N_bor_x,N_bor_y,i,j
      REAL Pave,Source(ndy,ndx)
C X and Y Positions
      INTEGER Py,Px

      DO 520 j=1,Ndy
        DO 510 i=1,Ndx
          source(j,i)=0.0
510     CONTINUE
520   CONTINUE 
C Find Y position of the boreholes
      DO 540 j=1,N_bor_y
        Py=2+Ndy_res+(j-1)*Ndy_bor+Ndy_bor_c
C Find X position of the boreholes
        DO 530 i=1,N_bor_x
          Px=2+Ndx_res+(i-1)*Ndx_bor+Ndx_bor_c
C Initialise source term
          Source(Py,Px)=Pave
530     CONTINUE
540   CONTINUE 
   
      END

***************************************************************************
*6
***************************************************************************
C Init_B
C
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C
C This subroutine initialise the source+internal energy term 
C
C This routine is called from CalcTe
C
C This subroutine is called every time Inter_Tg hours pass
C By default this should be for every 2 weeks simulated
C
C INPUTS:
C	Ndx		Number of elements to mesh the entire domain in the X direction
C	Ndy		Number of elements to mesh the entire domain in the Y direction
C	Aop		Table containing the thermal inertia of the elements
C	Source		Table containing the source term for every element
C	Ttemp		Table containing all temperatures in the ground
C 
C OUTPUTS:
C	B		Table containing the sum of the internal energy and the source term for each element
C
C *********************************************************************
      SUBROUTINE InitB(Ndx,Ndy,Aop,Source,Ttemp,B)

      IMPLICIT NONE 
C     INPUTS
      INTEGER i,j,Ndx,Ndy
      REAL Aop(Ndy,Ndx),Source(Ndy,Ndx),Ttemp(Ndy,Ndx),B(Ndy,Ndx)
	  
      DO 620 j=1,Ndy
        DO 610 i=1,Ndx
          B(j,i)=Aop(j,i)*Ttemp(j,i)+Source(j,i)
610     CONTINUE
620   CONTINUE	
      
	END

****************************************************************************
*7
*****************************************************************************
C ReinitTtemp
C
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C
C This subroutine equals all values in Ttampon to those in Ttemp 
C
C This routine is called from CalcTe
C
C This subroutine is called at every iteration
C
C INPUTS:
C	Ndx		Number of elements to mesh the entire domain in the X direction
C	Ndy		Number of elements to mesh the entire domain in the Y direction
C	Ttemp	Table containing all temperatures in the ground
C 
C OUTPUTS:
C	Ttampon	Table containing temperatures from the previous iteration
C
C *********************************************************************
      SUBROUTINE ReinitTtemp(Ndx,Ndy,Ttemp,Ttampon)

      IMPLICIT NONE 
C     INPUTS
      INTEGER Ndx,Ndy,i,j
      REAL Ttemp(Ndy,Ndx),Ttampon(Ndy,Ndx)
	  
      DO 720 j=1,Ndy
        DO 710 i=1,Ndx
          Ttampon(j,i)=Ttemp(j,i)
710     CONTINUE
720   CONTINUE
      
	END

********************************************************************
*8
*******************************************************************
C ItereX
C
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C
C This subroutine solves the temperature table
C doing sweeps in the Y direction of TDMA solutions in the X direction 
C
C This routine is called from CalcTe
C
C This subroutine is called for every iteration
C
C INPUTS:
C	Relax	Relaxation factor
C	Tg_ND	Undisturbed ground temperature
C	Ndx		Number of elements to mesh the entire domain in the X direction
C	Ndy		Number of elements to mesh the entire domain in the Y direction
C	Ae		Table containing the east-west conduction coefficients of the elements
C	An		Table containing the north-south conduction coefficients of the elements
C	Aop		Table containing the thermal inertia of the elements
C	Ap		Table containing the sum of the Patanker coefficients for each element
C	B		Table containing the sum of the internal energy and the source term for each element
C 
C OUTPUTS:
C	Ttemp	Table containing all temperatures in the ground
C
C *********************************************************************
      SUBROUTINE ItereX(relax,Tg_ND,Ndx,Ndy,Ae,An,Ap,B,Ttemp)

      IMPLICIT NONE 
C     INPUTS
      INTEGER Ndx,Ndy,i,j
      REAL Tg_ND,relax,Ae(Ndy,Ndx),An(Ndy,Ndx),Ap(Ndy,Ndx),B(Ndy,Ndx),
     &     Ttemp(Ndy,Ndx)
C TDMA Coefficients
      REAL ai(Ndx),bi(Ndx),ci(Ndx),Ti(Ndx),ri(Ndx)

C Sweep from south to north
      DO 830 j=2,Ndy-1
C Transform coefficients to the form used by TDMA from west to east
        DO 810 i=1,Ndx
          IF (i.EQ.1) THEN
            ai(i)=0.0
          ELSE
            ai(i)=-Ae(j,i-1)
          ENDIF
          ci(i)=-Ae(j,i)
          bi(i)=Ap(j,i)
          Ti(i)=Ttemp(j,i)
          ri(i)=B(j,i)+An(j,i)*Ttemp(j+1,i)+An(j-1,i)*Ttemp(j-1,i)
810     CONTINUE
C Solve using TDMA
        CALL TDMA(Ndx,ai,bi,ci,Ti,ri)
C Write calculated values in the 2-D table
        DO 820 i=2,Ndx-1
          Ttemp(j,i)=relax*Ti(i)+(1-relax)*Ttemp(j,i)
820     CONTINUE
830   CONTINUE
C Boundary conditions
      CALL Tfrontiere(Ndx,Ndy,Tg_ND,Ttemp)
      
	END

***************************************************************
*9
****************************************************************
C ItereY
C
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C
C This subroutine solves the temperature table
C doing sweeps in the X direction of TDMA solutions in the Y direction 
C
C This routine is called from CalcTe
C
C This subroutine is called for every iteration
C
C INPUTS:
C	Relax   Relaxation factor
C	Tg_ND   Undisturbed ground temperature
C	Ndx		Number of elements to mesh the entire domain in the X direction
C	Ndy		Number of elements to mesh the entire domain in the Y direction
C	Ae		Table containing the east-west conduction coefficients of the elements
C	An		Table containing the north-south conduction coefficients of the elements
C	Aop		Table containing the thermal inertia of the elements
C	Ap		Table containing the sum of the Patanker coefficients for each element
C	B		Table containing the sum of the internal energy and the source term for each element
C 
C OUTPUTS:
C	Ttemp   Table containing all temperatures in the ground
C
C *********************************************************************
      SUBROUTINE ItereY(relax,Tg_ND,Ndx,Ndy,Ae,An,Ap,B,Ttemp)

      IMPLICIT NONE 
C     INPUTS
      INTEGER Ndx,Ndy,i,j
      REAL Tg_ND,relax,Ae(Ndy,Ndx),An(Ndy,Ndx),Ap(Ndy,Ndx),B(Ndy,Ndx),
     &     Ttemp(Ndy,Ndx)
C TDMA Coefficients
      REAL ai(Ndy),bi(Ndy),ci(Ndy),Ti(Ndy),ri(Ndy)
C Position in the table
      INTEGER pos

C Sweep from east to west
      DO 930 i=Ndx-1,2,-1
C Transform coefficients to the form used by TDMA from north to south
        DO 910 j=1,Ndy
          pos=Ndy-j+1
          ai(j)=-An(pos,i)
          IF (j.EQ.Ndy) THEN
            ci(j)=0.0
          ELSE
            ci(j)=-An(pos-1,i)
          ENDIF
            bi(j)=Ap(pos,i)
            Ti(j)=Ttemp(pos,i)
            ri(j)=B(pos,i)+Ae(pos,i)*Ttemp(pos,i+1)+
     &	        Ae(pos,i-1)*Ttemp(pos,i-1)
910     CONTINUE
C Solve using TDMA
        CALL TDMA(Ndy,ai,bi,ci,Ti,ri)
C Write calculated values in the 2-D table
        DO 920 j=1,Ndy
          Ttemp(j,i)=relax*Ti(j)+(1-relax)*Ttemp(j,i)
920     CONTINUE
930   CONTINUE
C Boundary conditions
      CALL Tfrontiere(Ndx,Ndy,Tg_ND,Ttemp)

	END

***************************************************************
*10
***************************************************************
C CalcResidu
C
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C
C This subroutine calculates the largest difference between Ttemp and Ttampon
C
C This routine is called from CalcTe
C
C This subroutine is called at every iteration
C
C INPUTS:
C	Ndx		Number of elements to mesh the entire domain in the X direction
C	Ndy		Number of elements to mesh the entire domain in the Y direction
C	Ttemp	Table containing all temperatures in the ground
C	Ttampon	Table containing temperatures from the previous iteration
C 
C OUTPUTS:
C	CalcResidu	Largest difference between Ttemp and Ttampon
C
C *********************************************************************
      REAL FUNCTION CalcResidu(Ndx,Ndy,Ttemp,Ttampon)

      IMPLICIT NONE 
C     INPUTS
      INTEGER Ndx,Ndy,i,j
      REAL Ttemp(Ndy,Ndx),Ttampon(Ndy,Ndx)
C Residu (largest difference)	  
      REAL DifMax
C Local difference between Ttemp(j,i) and Ttampon(j,i)
      REAL Dif

      DifMax=0.0
      DO 1020 j=1,Ndy
        DO 1010 i=1,Ndx
          Dif=ABS(Ttemp(j,i)-Ttampon(j,i))
          IF (Dif>DifMax) DifMax=Dif
1010    CONTINUE
1020  CONTINUE 
      CalcResidu=DifMax

      END

***************************************************************
*11
****************************************************************
C TDMA
C
C From: Numerical Recipies in Fortran 77
C
C This subroutine solves a one dimensional temperature problem
C
C This routine is called from IterX and ItereY
C
C This subroutine is called for every iteration
C
C INPUTS:
C	X direction	ai=Aw, ci=Ae, bi=Ap, ri=b+AnTn+AsTs
C	Y direction	ai=As, ci=An, bi=Ap, ri=b+AeTe+AwTw
C
C 
C OUTPUTS:
C	Ti		Vector containing the calculated temperatures
C
C *********************************************************************
	SUBROUTINE TDMA(nd,ai,bi,ci,Ti,ri)

      IMPLICIT NONE 
C     INPUTS
      INTEGER nd,j
      REAL ai(nd),bi(nd),ci(nd),Ti(nd),ri(nd)
      REAL bet
      REAL gam(nd)

      bet=bi(1)
      Ti(1)=ri(1)/bet
      DO 1110 j=2,nd
        gam(j)=ci(j-1)/bet
        bet=bi(j)-ai(j)*gam(j)
        IF(bet.EQ.0) bet=1e-6
        Ti(j)=(ri(j)-ai(j)*Ti(j-1))/bet
1110  CONTINUE
      DO 1120 j=nd-1,1,-1
        Ti(j)=Ti(j)-gam(j+1)*Ti(j+1)
1120  CONTINUE
     
      END

***************************************************************
*12
***************************************************************      
C Tfrontiere
C
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C
C This subroutine applies the boundary conditions to the domain
C
C This routine is called from ItereX and ItereY
C
C This subroutine is called for every iteration
C
C INPUTS:
C	Tg_ND   Undisturbed ground temperature
C	Ndx	    Number of elements to mesh the entire domain in the X direction
C	Ndy	    Number of elements to mesh the entire domain in the Y direction
C 
C OUTPUTS:
C	Ttemp   Table containing all temperatures in the ground
C
C *********************************************************************
      SUBROUTINE Tfrontiere(Ndx,Ndy,Tg_ND,Ttemp)

      IMPLICIT NONE 
C     INPUTS
      INTEGER Ndx,Ndy,i
      REAL Ttemp(Ndy,Ndx),Tg_ND
	
      DO 1210 i=1,Ndx
        Ttemp(1,i)=Tg_ND
        Ttemp(Ndy,i)=Tg_ND
1210  CONTINUE
      DO 1220 i=2,Ndy-1
        Ttemp(i,1)=Tg_ND
        Ttemp(i,Ndx)=Tg_ND
1220  CONTINUE
      
	END

**********************************************************************    
*13
**********************************************************************
C ReadTemp
C
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C
C This subroutine reads all temperatures in the domain
C in the file Ttemp.bin
C
C This routine is called from GCEPtground
C
C This subroutine is called every time Inter_Tg hours pass
C By default this should be for every 2 weeks simulated
C
C INPUTS:
C	Ndx		Number of elements to mesh the entire domain in the X direction
C	Ndy		Number of elements to mesh the entire domain in the Y direction
C 
C OUTPUTS:
C	Ttemp	Table containing all temperatures in the ground
C
C *********************************************************************
      SUBROUTINE ReadTemp(Ndx,Ndy,Ttemp)

C     INPUTS
      INTEGER Ndx,Ndy
      REAL Ttemp(Ndy,Ndx)

      INTEGER NumUNIT
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN

C Read Ttemp
      NumUNIT =  IFIL+62
      READ(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) Ttemp

      END

***************************************************************
*14
**************************************************************
C ReadTemp_1b
C
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C
C This subroutine reads all temperatures in the domain for a borefield containing only 1 borehole
C in the file Ttemp1c.bin
C
C This routine is called from GCEPtground
C
C This subroutine is called every time Inter_Tg hours pass
C By default this should be for every 2 weeks simulated
C
C INPUTS:
C	Ndx_1b		Number of elements to mesh the entire domain in the X direction
C	Ndy_1b		Number of elements to mesh the entire domain in the Y direction
C 
C OUTPUTS:
C	Ttemp_1b	Table containing all temperatures in the ground
C
C *********************************************************************
      SUBROUTINE ReadTemp_1b(Ndx,Ndy,Ttemp)

C     INPUTS
      INTEGER Ndx,Ndy
      REAL Ttemp(Ndy,Ndx)

      INTEGER NumUNIT
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN

C Read Ttemp_1b
      NumUNIT =  IFIL+63
      READ(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) Ttemp

      END

***************************************************************
*15
****************************************************************
C ReadPat
C
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C
C This subroutine reads all Patankar coefficients
C in the files Ae.bin,An.bin,Aop.bin and Ap.bin 
C
C This routine is called from GCEPtground
C
C This subroutine is called every time Inter_Tg hours pass
C By default this should be for every 2 weeks simulated
C
C INPUTS:
C	Ndx	Number of elements to mesh the entire domain in the X direction
C	Ndy	Number of elements to mesh the entire domain in the Y direction
C 
C OUTPUTS:
C	Ae	Table containing the east-west conduction coefficients of the elements
C	An	Table containing the north-south conduction coefficients of the elements
C	Aop	Table containing the thermal inertia of the elements
C	Ap	Table containing the sum of the Patanker coefficients for each element
C
C *********************************************************************
      SUBROUTINE ReadPat(Ndx,Ndy,Ae,An,Aop,Ap)

C     INPUTS
      INTEGER Ndx,Ndy
      REAL Ae(Ndy,Ndx),An(Ndy,Ndx),Aop(Ndy,Ndx),Ap(Ndy,Ndx)

      INTEGER NumUNIT
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN

C Read Ae
      NumUNIT =  IFIL+54
      READ(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) Ae

C Read An
      NumUNIT =  IFIL+55
      READ(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) An

C Read Aop
      NumUNIT =  IFIL+56
      READ(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) Aop
 
C Read Ap
      NumUNIT =  IFIL+57
      READ(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) Ap

      END

***************************************************************
*16
****************************************************************
C ReadPat_1b
C
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C
C This subroutine reads all Patankar coefficients for a domain containing only 1 borehole
C in the files Ae1c.bin,An1c.bin,Aop1c.bin and Ap1c.bin 
C
C This routine is called from GCEPtground
C
C This subroutine is called every time Inter_Tg hours pass
C By default this should be for every 2 weeks simulated
C
C INPUTS:
C	Ndx_1b		Number of elements to mesh the entire domain in the X direction
C	Ndy_1b		Number of elements to mesh the entire domain in the Y direction
C 
C OUTPUTS:
C	Ae_1b		Table containing the east-west conduction coefficients of the elements
C	An_1b		Table containing the north-south conduction coefficients of the elements
C	Aop_1b		Table containing the thermal inertia of the elements
C	Ap_1b		Table containing the sum of the Patanker coefficients for each element
C
C *********************************************************************
      SUBROUTINE ReadPat_1b(Ndx,Ndy,Ae,An,Aop,Ap)

C     INPUTS
      INTEGER Ndx,Ndy
      REAL Ae(Ndy,Ndx),An(Ndy,Ndx),Aop(Ndy,Ndx),Ap(Ndy,Ndx)
      
      INTEGER NumUNIT
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN

C Read Ae_1b
      NumUNIT =  IFIL+58
      READ(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) Ae

C Read An_1b
      NumUNIT =  IFIL+59
      READ(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) An

C Read Aop_1b
      NumUNIT =  IFIL+60
      READ(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) Aop
 
C Read Ap_1b
      NumUNIT =  IFIL+61
      READ(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) Ap

      END

***************************************************************
*17
***************************************************************
C ReadDim
C
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C
C This subroutine reads all vectors containing elements dimensions
C for the full borefield and a borefield containing only one borehole
C
C This routine is called from GCEPtground
C
C This subroutine is called every time Inter_Tg hours pass
C By default this should be for every 2 weeks simulated
C
C INPUTS:
C	Ndx		Number of elements to mesh the entire domain in the X direction
C	Ndy		Number of elements to mesh the entire domain in the Y direction
C	Ndx_1b	Number of elements to mesh a domain containing only one bore in the X direction
C	Ndy_1b	Number of elements to mesh a domain containing only one bore in the Y direction
C 
C OUTPUTS:
C	dx		Vector containing the dimensions of the elements in the X direction
C	dy		Vector containing the dimensions of the elements in the Y direction
C	dx_1b	Vector containing the dimensions of the elements in the X direction for a domain containing only one bore
C	dy_1b	Vector containing the dimensions of the elements in the Y direction for a domain containing only one bore
C
C *********************************************************************
      SUBROUTINE ReadDim(Ndx,Ndy,Ndx_1b,Ndy_1b,dx,dy,dx_1b,dy_1b)

C     INPUTS
      INTEGER Ndx,Ndy,Ndx_1b,Ndy_1b
      REAL dx(Ndx),dy(Ndy),dx_1b(Ndx_1b),dy_1b(Ndy_1b)

      INTEGER NumUNIT
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      

C Read dx
      NumUNIT =  IFIL+50
      READ(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) dx

C Read dy
      NumUNIT =  IFIL+51
      READ(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) dy

C Read dx_1b
      NumUNIT =  IFIL+52
      READ(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) dx_1b

C Read dy_1b
      NumUNIT =  IFIL+53
      READ(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) dy_1b
      
      END

********************************************************************
*18
********************************************************************
C StockTemp
C     
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C     
C     
C This subroutine stores all temperatures in the domain for a complete borefield
C in the file Ttemp.bin
C     
C This routine is called from InitGrnd and GCEPtground
C     
C This subroutine is called once at the begining of the simulation
C     
C INPUTS:
C     Ndx	    Number of elements to mesh the entire domain in the X direction
C     Ndy	    Number of elements to mesh the entire domain in the Y direction
C     Ttemp   Table containing all temperatures in the ground
C     
C OUTPUTS:
C     File containing Ttemp
C     
C*********************************************************************
      SUBROUTINE StockTemp(Ndx,Ndy,Ttemp)

C     INPUTS
      IMPLICIT NONE
      INTEGER Ndx,Ndy
      REAL Ttemp(Ndy,Ndx)

      INTEGER NumUNIT
      COMMON/FILEP/IFIL
      INTEGER IFIL
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN
      INTEGER ISTAT

      NumUNIT =  IFIL+62
C    Close and reopen file if required..Not neccesary because file is already open in GCEPInit.F.
C      CALL ERPFREE(NumUNIT,ISTAT)
C      CALL FPRAND(NumUNIT,ISTAT,Ndx*Ndy,1,'Ttemp.bin')  ! File should already exist.
      WRITE(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) Ttemp
  
      END

*************************************************************************
*19
*************************************************************************
C StockTemp_1b
C     
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C     
C     
C This subroutine stores all temperatures in the domain for a borefield containing only 1 borehole
C in the file Ttemp1c.bin
C     
C This routine is called from InitGrnd and GCEPtground
C     
C This subroutine is called once at the begining of the simulation
C     
C INPUTS:
C     Ndx_1b	    Number of elements to mesh the entire domain in the X direction
C     Ndy_1b	    Number of elements to mesh the entire domain in the Y direction
C     Ttemp_1b    Table containing all temperatures in the ground
C     
C OUTPUTS:
C     File containing Ttemp_1b
C     
C*********************************************************************
      SUBROUTINE StockTemp_1b(Ndx,Ndy,Ttemp)

C     INPUTS
      INTEGER Ndx,Ndy
      REAL Ttemp(Ndy,Ndx)

      INTEGER NumUNIT
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN

      NumUNIT =  IFIL+63
C    Not required. ALready open in GCEPInit.F See above function.
c      CALL ERPFREE(NumUNIT,ISTAT)
c      CALL FPRAND(NumUNIT,ISTAT,Ndx*Ndy,1,'Ttemp.bin')  ! File should already exist.
      WRITE(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) Ttemp
      
      END
