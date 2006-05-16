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
C GCEPInit
C
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C 
C
C 
C This subroutine calculates the number of elements in each dimensions
C It also evaluates the dimensions of the tubes
C Calls the procedures that 
C	-initialize the vectors and tables,
C	-calculate dimensions and positions of mesh elements 
C	-calculate the coefficients for the Patankar Finite Volumes method 
C	-save all coefficients in files
C
C This routine is called from GCEP_MLAinit
C
C This subroutine is called once when the problem is read
C
C INPUTS:
C	GCEP_INPUT module
C 
C OUTPUTS:
C	Tube diameters
C	Number of elements in each direction for complete domain and for domain containing only one borehole
C	Dimensions of the elements composing the mesh and Value of the Patenkar coefficients all stored in files
C
C ********************************************************************
      SUBROUTINE GCEPInit

CUse module for GCEP data
C        USE GCEP_INPUT

	IMPLICIT NONE
#include "gcep_parameters.h"
#include "gcep_common.h"
      COMMON/OUTIN/IUOUT,IUIN
	INTEGER IUOUT,IUIN

C This subroutine should only be called once during the simulation.
C gcep_init_call is a dummy variable used to determine if the GCEP_init
C subroutine has been called.  If it is TRUE then the subroutine has not been called.
C If it is false, then the subroutine has been called and will not be called again
      IF (gcep_init_call) THEN

C Evaluates the inside and outside diameter of the tubes based on the 'TypeTubes' variable
        CALL D_tubes(GCEP_TypeTubes,GCEP_Di_tubes,GCEP_Do_tubes)
        WRITE(IUOUT,*) GCEP_Di_tubes, GCEP_Do_tubes, GCEP_D_bore
C Verify that the tubes are not larger than the borehole
        IF(2.0*GCEP_Do_tubes.GT.GCEP_D_bore) THEN
C Display error message and quit
          WRITE(IUOUT,*) ' Error GCEP: Tubes larger than borehole'
          RETURN
        ENDIF

C Initialisation of the total amount of elements in each direction
C Ndx,Ndy,Ndx_1c and Ndy_1c are part of the GCEPM_INPUT module
        GCEP_Ndx=2*(GCEP_Ndx_res+1)+GCEP_N_bor_x*GCEP_Ndx_bor
        GCEP_Ndy=2*(GCEP_Ndy_res+1)+GCEP_N_bor_y*GCEP_Ndy_bor
        GCEP_Ndx_1b=2*(GCEP_Ndx_res+1)+GCEP_Ndx_bor
        GCEP_Ndy_1b=2*(GCEP_Ndy_res+1)+GCEP_Ndy_bor

C Initialize the times to zero     
	GCEP_Time_Tg=0.0
        GCEP_Time=0.0
          
C Initiation of the history vector
        CALL InitHistFirst(GCEP_NHis,GCEP_LoadHis)

C Initialize and store the coefficients that will be used to calculate numerically
C the temperatures in the ground
        CALL InitGrnd(GCEP_K_s,GCEP_Alpha_s,GCEP_D_bore,
     &       GCEP_N_bor_x,GCEP_N_bor_y, GCEP_L_bor_x,GCEP_L_bor_y,
     &       GCEP_Tg_init,GCEP_Inter_Tg,
     &       GCEP_Ndx_bor,GCEP_Ndy_bor,GCEP_Ndx_bor_c,GCEP_Ndy_bor_c,
     &       GCEP_L_res_x,GCEP_L_res_y,GCEP_Ndx_res,GCEP_Ndy_res,
     &       GCEP_Ndx,GCEP_Ndy,GCEP_Ndx_1b,GCEP_Ndy_1b)
        GCEP_Tg=GCEP_Tg_init
        GCEP_Tpen=0.0

        gcep_init_call = .FALSE.
      ELSE
        gcep_init_call = .FALSE.
        RETURN
      ENDIF

      END

**************************************************************************
*1
C*********************************************************************
C InitGrnd
C     
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C     
C     
C This subroutine initialize tables
C calculates dimensions and positions of mesh elements 
C and the coefficients for the Patankar Finite Volumes method 
C Saves all coefficients in files
C     
C This routine is called from GCEP_MLAInit
C     
C This subroutine is called once when the problem is read
C     
C INPUTS:
C     K_s		    thermal conductivity of the ground
C     Alpha_s		thermal difusivity of the ground
C     D_bore		Borehole diameter
C     N_bort_x	Number of bores in the X direction
C     N_bort_y	Number of bores in the Y direction
C     Inter_Tg	Number of hours between calculations of ground temperatures
C     L_bort_x	Distance between boreholes in X direction
C     L_bort_y	Distance between boreholes in Y direction
C     Tg_init		Initial ground temperature
C     Ndx_bor		Number of elements to mesh a complete bore in the X direction (11)
C     Ndy_bor		Number of elements to mesh a complete bore in the Y direction (11)
C     Ndx_bor_c	Number of elements to mesh half a bore (side of a borehole) in the X direction (5)
C     Ndy_bor_c	Number of elements to mesh half a bore (side of a borehole) in the Y direction (5)
C     L_res_x		Length of a the thermal reservoirs in the X direction (500m)
C     L_res_y		Length of a the thermal reservoirs in the Y direction (500m)
C     Ndx_res		Number of elements to mesh a thermal reservoir in the X direction (20)
C     Ndy_res		Number of elements to mesh a thermal reservoir in the Y direction (20)
C     Ndx		    Number of elements to mesh the entire domain in the X direction
C     Ndy		    Number of elements to mesh the entire domain in the Y direction
C     Ndx_1c		Number of elements to mesh a domain containing only one borehole in the X direction
C     Ndy_1c		Number of elements to mesh a domain containing only one borehole in the Y direction
C     
C     OUTPUTS:
C     Dimensions of the elements composing the mesh and Value of the Patenkar coefficients
C     All stored in files
C     *********************************************************************
      SUBROUTINE InitGrnd(K_s,Alpha_s,D_bore,N_bort_x,N_bort_y,
     &  L_bort_x,L_bort_y,Tg_init,Inter_Tg,
     &  Ndx_bor,Ndy_bor,Ndx_bor_c,Ndy_bor_c,
     &  L_res_x,L_res_y,Ndx_res,Ndy_res,Ndx,Ndy,Ndx_1b,Ndy_1b)
      
C Subscript _1b is for a borefield containing only one borehole       
      
C     INPUTS
      IMPLICIT NONE 
      INTEGER N_bort_x,N_bort_y,Ndx_bor,Ndy_bor,
     &        Ndx_bor_c,Ndy_bor_c,Ndx_res,Ndy_res,Ndx,Ndy,
     &        Ndx_1b,Ndy_1b
      REAL K_s,Alpha_s,D_bore,L_bort_x,L_bort_y,Tg_init,
     &     L_res_x,L_res_y,Inter_Tg

C     Used Variables
C Vectors containing the dimensions of the elements
      REAL dx(Ndx),dy(Ndy),dx_1b(Ndx_1b),dy_1b(Ndy_1b)
C Tables containing all temperatures in the ground
      REAL Ttemp(Ndy,Ndx),Ttemp_1b(Ndy_1b,Ndx_1b)
C Tables containing the coefficients of conduction (Ae,An), of thermal inertia (Aop) and the sum (Ap)
      REAL Ae(Ndy,Ndx),An(Ndy,Ndx),Aop(Ndy,Ndx),Ap(Ndy,Ndx),
     &   Ae_1b(Ndy_1b,Ndx_1b),An_1b(Ndy_1b,Ndx_1b),
     &	 Aop_1b(Ndy_1b,Ndx_1b),Ap_1b(Ndy_1b,Ndx_1b)

C Calculate mesh dimensions and positions and Patankar coefficients for complete borefield
      CALL CalcCoefPat(K_s,Alpha_s,D_bore,N_bort_x,N_bort_y,
     & Inter_Tg,L_bort_x,L_bort_y,Ndx_bor,Ndy_bor,Ndx_bor_c,Ndy_bor_c
     & ,L_res_x,L_res_y,Ndx_res,Ndy_res,Ndx,Ndy,Ae,An,Aop,Ap,dx,dy)
C Calculate mesh dimensions and positions and Patankar coefficients for borefield containing only one borehole
      CALL CalcCoefPat(K_s,Alpha_s,D_bore,1,1,Inter_Tg,
     & L_bort_x,L_bort_y,Ndx_bor,Ndy_bor,Ndx_bor_c,Ndy_bor_c,
     & L_res_x,L_res_y,Ndx_res,Ndy_res,Ndx_1b,Ndy_1b,
     & Ae_1b,An_1b,Aop_1b,Ap_1b,dx_1b,dy_1b)

C Initialize Temperature for complete borefield
      CALL InitTemps(Ndx,Ndy,Tg_init,Ttemp)
C Initialize Temperature for borefield containing only one borehole
      CALL InitTemps(Ndx_1b,Ndy_1b,Tg_init,Ttemp_1b)

C Store dx,dy,dx_1b,dy_1b,Ae,An,Aop,Ap,Ttemp,Ae_1b,An_1b,Aop_1b,Ap_1b,Ttemp_1b in appropriate files
      CALL StockDim(Ndx,Ndy,Ndx_1b,Ndy_1b,dx,dy,dx_1b,dy_1b)
      CALL StockTempInit(Ndx,Ndy,Ttemp)
      CALL StockTemp_1bInit(Ndx_1b,Ndy_1b,Ttemp_1b)
      CALL StockPat(Ndx,Ndy,Ae,An,Aop,Ap)
      CALL StockPat_1b(Ndx_1b,Ndy_1b,Ae_1b,An_1b,Aop_1b,Ap_1b)
      END
      
**************************************************************************
*2
C*********************************************************************
C InitTemps
C     
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C     
C     
C This subroutine initialize temperatures in the ground 
C     
C This routine is called from InitGrnd
C     
C This subroutine is called once when the problem is read
C
C INPUTS:
C     Tg_init    Initial ground temperature
C     Ndx        Number of elements to mesh the entire domain in the X direction
C     Ndy        Number of elements to mesh the entire domain in the Y direction
C     
C     OUTPUTS:
C     Ttemp      Table containing the temperature of each element
C     *********************************************************************
      SUBROUTINE InitTemps(Ndx,Ndy,Tg_init,Ttemp)

      IMPLICIT NONE 
      INTEGER Ndx,Ndy,i,j
      REAL Ttemp(Ndy,Ndx),Tg_init

      DO 220 j=1,Ndy
         DO 210 i=1,Ndx
            Ttemp(j,i)=Tg_init
 210     CONTINUE
 220  CONTINUE
      END

**************************************************************************
*3
C*********************************************************************
C CalcCoefPat
C
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C     
C     
C This subroutine calculates dimensions and positions of mesh elements 
C and the coefficients for the Patankar Finite Volumes method 
C     
C This routine is called from InitGrnd
C     
C This subroutine is called once when the problem is read
C     
C INPUTS:
C     K_s		    thermal conductivity of the ground
C     Alpha_s	    thermal difusivity of the ground
C     D_bore      Bore diameter
C     N_bort_x    Number of bores in the X direction
C     N_bort_y    Number of bores in the Y direction
C     Inter_Tg    Number of hours between calculations of ground temperatures
C     L_bort_x    Distance between boreholes in X direction
C     L_bort_y    Distance between boreholes in Y direction
C     Ndx_bor	    Number of elements to mesh a complete bore in the X direction (11)
C     Ndy_bor	    Number of elements to mesh a complete bore in the Y direction (11)
C     Ndx_bor_c   Number of elements to mesh half a bore (side of a borehole) in the X direction (5)
C     Ndy_bor_c   Number of elements to mesh half a bore (side of a borehole) in the Y direction (5)
C     L_res_x	    Length of a the thermal reservoirs in the X direction (500m)
C     L_res_y	    Length of a the thermal reservoirs in the Y direction (500m)
C     Ndx_res	    Number of elements to mesh a thermal reservoir in the X direction (20)
C     Ndy_res	    Number of elements to mesh a thermal reservoir in the Y direction (20)
C     Ndx	        Number of elements to mesh the entire domain in the X direction
C     Ndy	        Number of elements to mesh the entire domain in the Y direction
C     
C     OUTPUTS:
C     Ae	    Table containing the east-west conduction coefficients of the elements
C     An	    Table containing the north-south conduction coefficients of the elements
C     Aop	    Table containing the thermal inertia of the elements
C     Ap	    Table containing the sum of the Patanker coefficients for each element
C     Dx	    Vector containing the X dimensions of the elements
C     Dy	    Vector containing the Y dimensions of the elements
C     
C     *********************************************************************
      SUBROUTINE CalcCoefPat(K_s,Alpha_s,D_bore,N_bort_x,N_bort_y,
     &     Inter_Tg,L_bort_x,L_bort_y,Ndx_bor,Ndy_bor,
     &     Ndx_bor_c,Ndy_bor_c,L_res_x,L_res_y,Ndx_res,Ndy_res,
     &     Ndx,Ndy,Ae,An,Aop,Ap,Dx,Dy)

C     INPUTS
      IMPLICIT NONE 
      INTEGER N_bort_x,N_bort_y,Ndx_bor,Ndy_bor,
     &        Ndx_bor_c,Ndy_bor_c,Ndx_res,Ndy_res,Ndx,Ndy
      REAL K_s,Alpha_s,D_bore,L_bort_x,L_bort_y,L_res_x,L_res_y,
     &	 Ae(Ndy,Ndx),An(Ndy,Ndx),Aop(Ndy,Ndx),Ap(Ndy,Ndx),
     &	 Dx(Ndx),Dy(Ndy),Inter_Tg

C Vectors containing the X and Y position of the center of each element
      REAL X_mesh(Ndx),Y_mesh(Ndy)

C Calculate the dimensions of the elements
      CALL Init_Dim(D_bore,N_bort_x,L_bort_x,Ndx_bor,Ndx_bor_c,L_res_x,
     &     Ndx_res,Ndx,Dx)
      CALL Init_Dim(D_bore,N_bort_y,L_bort_y,Ndy_bor,Ndy_bor_c,L_res_y,
     &     Ndy_res,Ndy,Dy)
C Calculate the positions of the elements
      CALL Init_mesh(Dx,Ndx,X_mesh)
      CALL Init_mesh(Dy,Ndy,Y_mesh)
C Calculate the conduction coefficients (Ae and An)
      CALL Init_A(K_s,Ndx,Ndy,X_mesh,Y_mesh,Dx,Dy,Ae,An)
C Calculate the thermal inertia coefficients
      CALL Init_Aop(K_s,Alpha_s,Ndx,Ndy,Dx,Dy,Inter_Tg,Aop)
C Calculate the sum of the coefficients
      CALL Init_Ap(Ndx,Ndy,Ae,An,Aop,Ap)
      END

*********************************************************************************
*4
C*********************************************************************
C D_tubes
C     
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C     
C     
C This subroutine  determines the dimensions of the tubes used
C This routine is called from GCEPMInit
C     
C This subroutine is called once when the problem is read in
C     
C INPUTS:
C     TypesTubes  Type of tubes used (choice 1..12)
C     
C OUTPUTS:
C     Di_tubes	Inside tube diameter
C     Do_tubes	Outside tube diameter
C*********************************************************************
      SUBROUTINE D_tubes(TypeTubes,Di_tubes,Do_tubes)

      IMPLICIT NONE 
C     INPUTS
      INTEGER TypeTubes
      REAL Di_tubes,Do_tubes

      IF (TypeTubes.EQ.1) THEN
         Di_tubes=0.0218
         Do_tubes=0.0267
      ELSE IF (TypeTubes.EQ.2) THEN
         Di_tubes=0.0273
         Do_tubes=0.0334
      ELSE IF (TypeTubes.EQ.3) THEN
         Di_tubes=0.0345
         Do_tubes=0.0422
      ELSE IF (TypeTubes.EQ.4) THEN
         Di_tubes=0.0204
         Do_tubes=0.0250
      ELSE IF (TypeTubes.EQ.5) THEN
         Di_tubes=0.0260
         Do_tubes=0.0320
      ELSE IF (TypeTubes.EQ.6) THEN
         Di_tubes=0.0326
         Do_tubes=0.0400
      ELSE IF (TypeTubes.EQ.7) THEN
         Di_tubes=0.0227
         Do_tubes=0.0267
      ELSE IF (TypeTubes.EQ.8) THEN
         Di_tubes=0.0285
         Do_tubes=0.0334
      ELSE IF (TypeTubes.EQ.9) THEN
         Di_tubes=0.0359
         Do_tubes=0.0422
      ELSE IF (TypeTubes.EQ.10) THEN
         Di_tubes=0.0204
         Do_tubes=0.0250
      ELSE IF (TypeTubes.EQ.11)  THEN
         Di_tubes=0.0262
         Do_tubes=0.0320
      ELSE
         Di_tubes=0.0340
         Do_tubes=0.0400
      ENDIF
      END

***********************************************************************
*5
C*********************************************************************
C Init_Dim
C     
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C     
C     
C This subroutine calculates dimensions of mesh elements 
C     
C This routine is called from CalcCoefPat
C     
C This subroutine is called once when the problem is read
C     
C INPUTS:
C     D_bort	  Bore diameter
C     N_bort    Number of bores in the direction
C     L_bort	  Distance between boreholes in direction
C     Nd_bor	  Number of elements to mesh a complete bore in the direction (11)
C     Nd_bor_c  Number of elements to mesh half a bore (side of a borehole) in the direction (5)
C     L_res	  Length of a the thermal reservoirs in the direction (500m)
C     Nd_res	  Number of elements to mesh a thermal reservoir in the direction (20)
C     Nd	      Number of elements to mesh the entire domain in the direction
C     
C OUTPUTS:
C     Dims      Vector containing the dimensions of the elements
C     
C*********************************************************************
      SUBROUTINE Init_Dim(D_bore,N_bort,L_bort,Nd_bor,Nd_bor_c,
     &                    L_res,Nd_res,Nd,Dims)

      IMPLICIT NONE 
C     INPUTS
      INTEGER N_bort,Nd_bor,Nd_bor_c,Nd_res,Nd,i,j,pos
      REAL D_bore,L_bort,L_res,Dims(Nd)
C minimum, maximum and average width of the elements in the thermal reservoirs
      REAL dmin_r,dmax_r,dmoy_r
C Bias (max/min element) in the reservoirs
      REAL Biais_r
C minimum, maximum and average width of the elements in the bores
      REAL dmin_c,dmax_c,dmoy_c
C Bias (max/min element) in the bores
      REAL Biais_c
C Width of the elements used to model the boreholes
      REAL d_cen
C Width of the element actually calculated
      REAL di
C Pi universal constant
      REAL pi

      pi=ATAN(1.0)*4.0
C We want d_cen**2=pi(D_bore/2)**2   element surface=borehole surface
      d_cen=D_bore/2*pi**0.5
C Calculate elements dimensions in the bores
      dmoy_c=(L_bort-d_cen)/2/Nd_bor_c
      IF (Nd_bor_c.EQ.1) THEN
         Biais_c=1
         dmin_c=dmoy_c
         dmax_c=dmoy_c
      ELSE
         Biais_c=2*dmoy_c/d_cen-1
         dmin_c=d_cen
         dmax_c=dmin_c*Biais_c
      ENDIF
C Calculate elements dimensions in the reservoirs
      dmoy_r=L_res/Nd_res
      Biais_r=2*dmoy_r/dmax_c-1.0
      dmin_r=dmax_c
      dmax_r=dmin_r*Biais_r
C Assign elements dimensions in the reservoirs
      Dims(1)=0
      Dims(Nd)=0
      DO 510 i=2,Nd_res+1
         di=dmax_r-(i-2.0)/(Nd_res-1.0)*(dmax_r-dmin_r)
         Dims(i)=di
         Dims(Nd-i+1)=di
510   CONTINUE
C Assign elements dimensions in the bores
      DO 530 i=1,N_bort
         pos=1+Nd_res+(i-1)*nd_bor
         DO 520 j=1,Nd_bor_c
            IF (Nd_bor_c.EQ.1) THEN
               di=dmoy_c
            ELSE
               di=dmax_c-(j-1)/(Nd_bor_c-1.0)*(dmax_c-dmin_c)
            ENDIF
            Dims(pos+j)=di
            Dims(pos+Nd_bor+1-j)=di
520     CONTINUE
C     Assign boreholes dimensions
         Dims(pos+Nd_bor_c+1)=d_cen
530   CONTINUE
      END

***********************************************************************
*6
C*********************************************************************
C Init_mesh
C     
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C     
C     
C This subroutine calculates the positions of mesh elements 
C     
C This routine is called from CalcCoefPat
C     
C This subroutine is called once when the problem is read
C     
C INPUTS:
C     Nd		Number of elements to mesh the entire domain in the direction
C     Dims    Vector containing the dimensions of the elements
C     
C OUTPUTS:
C     mail		Vector containing the positions of the elements
C     
C*********************************************************************
      SUBROUTINE Init_mesh(Dims,Nd,mail)
      
      IMPLICIT NONE
C     INPUTS
      INTEGER Nd,i
      REAL Dims(Nd),mail(Nd)

      mail(1)=0.0
      DO 610 i=2,Nd
         mail(i)=mail(i-1)+(Dims(i-1)+Dims(i))/2.0
610   CONTINUE
      END

************************************************************************
*7
C*********************************************************************
C Init_A
C     
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C     
C     
C This subroutine calculates the conductivity coefficients of mesh elements
C note: Aw[j][i]=Ae[j][i-1] et As[j][i]=An[j-1][i] so only Ae and An are calculated
C     
C This routine is called from CalcCoefPat
C     
C This subroutine is called once when the problem is read
C
C INPUTS:
C     K_s	    thermal conductivity of the ground
C     Ndx	    Number of elements to mesh the entire domain in the X direction
C     Ndy	    Number of elements to mesh the entire domain in the Y direction
C     X_mail  Vector containing the X positions of the elements
C     Y_mail  Vector containing the Y positions of the elements
C     Dx	    Vector containing the X dimensions of the elements
C     Dy	    Vector containing the Y dimensions of the elements
C     
C OUTPUTS:
C     Ae	    Table containing the east-west conduction coefficients of the elements
C     An	    Table containing the north-south conduction coefficients of the elements
C     
C***************************************************************************
      SUBROUTINE Init_A(K_s,Ndx,Ndy,X_mail,Y_mail,Dx,Dy,Ae,An)

      IMPLICIT NONE 
C     INPUTS
      INTEGER Ndx,Ndy,i,j
      REAL K_s,X_mail(Ndx),Y_mail(Ndy),Dx(Ndx),Dy(Ndy),
     &     Ae(Ndy,Ndx),An(Ndy,Ndx)

      DO 720 j=1,Ndy
         DO 710 i=1,Ndx-1
            Ae(j,i)=K_s/(X_mail(i+1)-X_mail(i))*Dy(j)
710      CONTINUE
720   CONTINUE        
      DO 740 j=1,Ndy-1
         DO 730 i=1,Ndx
            An(j,i)=K_s/(Y_mail(j+1)-Y_mail(j))*Dx(i)
730      CONTINUE
740   CONTINUE        
      END

*************************************************************************
*8
C*********************************************************************
C Init_Aop
C     
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C     
C     
C This subroutine calculates the thermal inertia coefficients of mesh elements
C     
C This routine is called from CalcCoefPat
C     
C This subroutine is called once when the problem is read
C     
C INPUTS:
C     K_s	        thermal conductivity of the ground
C     Alpha_s	    thermal difusivity of the ground
C     Ndx	        Number of elements to mesh the entire domain in the X direction
C     Ndy	        Number of elements to mesh the entire domain in the Y direction
C     Dx	        Vector containing the X dimensions of the elements
C     Dy	        Vector containing the Y dimensions of the elements
C     Inter_Tg    Number of hours between calculations of ground temperatures
C     
C OUTPUTS:
C     Aop	        Table containing the thermal inertia of the elements
C     
C*********************************************************************

      SUBROUTINE Init_Aop(K_s,Alpha_s,Ndx,Ndy,Dx,Dy,Inter_Tg,Aop)

      IMPLICIT NONE 
C     INPUTS
      INTEGER Ndx,Ndy,i,j
      REAL K_s,Alpha_s,Dx(Ndx),Dy(Ndy),Aop(Ndy,Ndx),Inter_Tg
C Thermal inertia
      REAL RhoCp

      RhoCp=K_s/Alpha_s*24.0/Inter_Tg
      DO 820 j=1,Ndy
        DO 810 i=1,Ndx
          Aop(j,i)=RhoCp*Dx(i)*Dy(j)
810     CONTINUE
820   CONTINUE
      END    

***************************************************************************
*9
C*********************************************************************
C Init_Ap
C     
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C     
C     
C This subroutine calculates the sum of the Patankar coefficients for each element
C     
C This routine is called from CalcCoefPat
C     
C This subroutine is called once when the problem is read
C     
C INPUTS:
C     Ndx	   Number of elements to mesh the entire domain in the X direction
C     Ndy	   Number of elements to mesh the entire domain in the Y direction
C     Ae	   Table containing the east-west conduction coefficients of the elements
C     An	   Table containing the north-south conduction coefficients of the elements
C     Aop	   Table containing the thermal inertia of the elements
C     
C OUTPUTS:
C     Ap	   Table containing the sum of the Patankar coefficients for each element
C     
C*********************************************************************

      SUBROUTINE Init_Ap(Ndx,Ndy,Ae,An,Aop,Ap)

      IMPLICIT NONE 
C     INPUTS
      INTEGER i,j,Ndx,Ndy
      REAL Ae(Ndy,Ndx),An(Ndy,Ndx),Aop(Ndy,Ndx),Ap(Ndy,Ndx)
      
      DO 920 j=2,Ndy
         DO 910 i=2,Ndx
            Ap(j,i)=Ae(j,i)+Ae(j,i-1)+An(j,i)+An(j-1,i)+Aop(j,i)
910      CONTINUE
         Ap(j,1)=Ae(j,1)
920   CONTINUE
      DO 930 i=1,Ndx
         Ap(1,i)=An(1,i)
930   CONTINUE
      END

********************************************************************
*10
********************************************************************
C StockTempInit
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
      SUBROUTINE StockTempInit(Ndx,Ndy,Ttemp)

      IMPLICIT NONE
C     INPUTS
      INTEGER Ndx,Ndy
      REAL Ttemp(Ndy,Ndx)

      INTEGER NumUNIT
      COMMON/FILEP/IFIL
      INTEGER IFIL
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN
C Local Vars
      INTEGER ISTAT


      NumUNIT =  IFIL+62
      CALL ERPFREE(NumUNIT,ISTAT)
      CALL FPRAND(NumUNIT,ISTAT,Ndx*Ndy,2,'Ttemp.bin')  ! File should not already exist.
      WRITE(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) Ttemp
  
      END

*************************************************************************
*11
*************************************************************************
C StockTemp_1bInit
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
      SUBROUTINE StockTemp_1bInit(Ndx,Ndy,Ttemp)
	IMPLICIT NONE
C     INPUTS
      INTEGER Ndx,Ndy
      REAL Ttemp(Ndy,Ndx)

      INTEGER NumUNIT
      COMMON/FILEP/IFIL
      INTEGER IFIL
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

C Local Vars
      INTEGER ISTAT


      NumUNIT =  IFIL+63
      CALL ERPFREE(NumUNIT,ISTAT)
      CALL FPRAND(NumUNIT,ISTAT,Ndx*Ndy,2,'Ttemp1c.bin')  ! File should not already exist.
      WRITE(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) Ttemp
      
      END

***********************************************************************
*12
***********************************************************************   
C StockPat
C     
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C     
C     
C This subroutine stores all Patankar coefficients for the complete domain
C in the files Ae.bin,An.bin,Aop.bin and Ap.bin 
C     
C This routine is called from InitGrnd
C     
C This subroutine is called once at the begining of the simulation
C     
C INPUTS:
C     Ndx	    Number of elements to mesh the entire domain in the X direction
C     Ndy	    Number of elements to mesh the entire domain in the Y direction
C     Ae	    Table containing the east-west conduction coefficients of the elements
C     An	    Table containing the north-south conduction coefficients of the elements
C     Aop	    Table containing the thermal inertia of the elements
C     Ap	    Table containing the sum of the Patanker coefficients for each element
C     
C OUTPUTS:
C     Files containing the tables in the INPUTS
C     
C     *********************************************************************
      SUBROUTINE StockPat(Ndx,Ndy,Ae,An,Aop,Ap)
	IMPLICIT NONE
C     INPUTS
      INTEGER Ndx,Ndy
      REAL Ae(Ndy,Ndx),An(Ndy,Ndx),Aop(Ndy,Ndx),Ap(Ndy,Ndx)

      INTEGER NumUNIT
      COMMON/FILEP/IFIL
      INTEGER IFIL
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

C Local Vars
      INTEGER ISTAT

C write Ae
      NumUNIT =  IFIL+54
      CALL ERPFREE(NumUNIT,ISTAT)
      CALL FPRAND(NumUNIT,ISTAT,Ndx*Ndy,2,'Ae.bin')  ! File should not already exist.
      WRITE(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) Ae
     
C write An
      NumUNIT =  IFIL+55
      CALL ERPFREE(NumUNIT,ISTAT)
      CALL FPRAND(NumUNIT,ISTAT,Ndx*Ndy,2,'An.bin')  ! File should not already exist.
      WRITE(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) An

C write Aop
      NumUNIT =  IFIL+56
      CALL ERPFREE(NumUNIT,ISTAT)
      CALL FPRAND(NumUNIT,ISTAT,Ndx*Ndy,2,'Aop.bin')  ! File should not already exist.
      WRITE(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) Aop

C write Ap
      NumUNIT =  IFIL+57
      CALL ERPFREE(NumUNIT,ISTAT)
      CALL FPRAND(NumUNIT,ISTAT,Ndx*Ndy,2,'Ap.bin')  ! File should not already exist.
      WRITE(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) Ap

      CLOSE(UNIT=105)
      END
***************************************************************
*13
****************************************************************
C StockPat_1b
C     
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C     
C     
C This subroutine stores all Patankar coefficients for a domain containing only 1 borehole
C in the files Ae1c.bin,An1c.bin,Aop1c.bin and Ap1c.bin 
C     
C This routine is called from InitGrnd
C     
C This subroutine is called once at the begining of the simulation
C     
C INPUTS:
C     Ndx_1b   Number of elements to mesh the entire domain in the X direction
C     Ndy_1b   Number of elements to mesh the entire domain in the Y direction
C     Ae_1b    Table containing the east-west conduction coefficients of the elements
C     An_1b    Table containing the north-south conduction coefficients of the elements
C     Aop_1b   Table containing the thermal inertia of the elements
C     Ap_1b    Table containing the sum of the Patanker coefficients for each element
C     
C OUTPUTS:
C     Files containing the tables in the INPUTS
C     
C*********************************************************************
      SUBROUTINE StockPat_1b(Ndx,Ndy,Ae,An,Aop,Ap)
       IMPLICIT NONE
C     INPUTS
      INTEGER Ndx,Ndy
      REAL Ae(Ndy,Ndx),An(Ndy,Ndx),Aop(Ndy,Ndx),Ap(Ndy,Ndx)
     
      INTEGER NumUNIT
      COMMON/FILEP/IFIL
      INTEGER IFIL
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

C Local Vars
      INTEGER ISTAT

C write Ae_1c
      NumUNIT =  IFIL+58
      CALL ERPFREE(NumUNIT,ISTAT)
      CALL FPRAND(NumUNIT,ISTAT,Ndx*Ndy,2,'Ae1c.bin')  ! File should not already exist.
      WRITE(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) Ae

C write An_1c
      NumUNIT =  IFIL+59
      CALL ERPFREE(NumUNIT,ISTAT)
      CALL FPRAND(NumUNIT,ISTAT,Ndx*Ndy,2,'An1c.bin')  ! File should not already exist.
      WRITE(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) An

C write Aop_1c
      NumUNIT =  IFIL+60
      CALL ERPFREE(NumUNIT,ISTAT)
      CALL FPRAND(NumUNIT,ISTAT,Ndx*Ndy,2,'Aop1c.bin')  ! File should not already exist.
      WRITE(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) Aop
   
C write Ap_1c
      NumUNIT =  IFIL+61
      CALL ERPFREE(NumUNIT,ISTAT)
      CALL FPRAND(NumUNIT,ISTAT,Ndx*Ndy,2,'Ap1c.bin')  ! File should not already exist.
      WRITE(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) Ap

      END

***************************************************************
*14
***************************************************************
C StockDim
C     
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C     
C     
C This subroutine stores all vectors containing elements dimensions
C for the full borefield and a borefield containing only one borehole
C     
C This routine is called from InitGrnd
C     
C This subroutine is called once at the begining of the simulation
C     
C     
C INPUTS:
C     Ndx	    Number of elements to mesh the entire domain in the X direction
C     Ndy	    Number of elements to mesh the entire domain in the Y direction
C     Ndx_1b  Number of elements to mesh a domain containing only one bore in the X direction
C     Ndy_1b  Number of elements to mesh a domain containing only one bore in the Y direction
C     dx	    Vector containing the dimensions of the elements in the X direction
C     dy	    Vector containing the dimensions of the elements in the Y direction
C     dx_1b   Vector containing the dimensions of the elements in the X direction for a domain containing only one bore
C     dy_1b   Vector containing the dimensions of the elements in the Y direction for a domain containing only one bore
C     
C OUTPUTS:
C     Files containing the vectors in the INPUTS
C     
C*********************************************************************
      SUBROUTINE StockDim(Ndx,Ndy,Ndx_1b,Ndy_1b,dx,dy,dx_1b,dy_1b)
      IMPLICIT NONE
C     INPUTS
      INTEGER Ndx,Ndy,Ndx_1b,Ndy_1b
      REAL dx(Ndx),dy(Ndy),dx_1b(Ndx_1b),dy_1b(Ndy_1b)

      INTEGER NumUNIT
      COMMON/FILEP/IFIL
      INTEGER IFIL
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

C Local Vars
      INTEGER ISTAT
C---------------------------------------------------------------------------------
C ERPFREE checks to see if unit is free
C FPRAND Opens a file with direct access (i.e. a binary file) .
C---------------------------------------------------------------------------------
C write dx
      NumUNIT =  IFIL+50
      CALL ERPFREE(NumUNIT,ISTAT)
      CALL FPRAND(NumUNIT,ISTAT,ndx,2,'dx.bin')  ! File should not already exist.
      WRITE(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) dx

C write dy
      NumUNIT =  IFIL+51
      CALL ERPFREE(NumUNIT,ISTAT)
      CALL FPRAND(NumUNIT,ISTAT,ndy,2,'dy.bin') 
      WRITE(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) dy
 
C write dx_1c
      NumUNIT =  IFIL+52
      CALL ERPFREE(NumUNIT,ISTAT)
      CALL FPRAND(NumUNIT,ISTAT,Ndx_1b,2,'dx1c.bin')  ! File should not already exist.
      WRITE(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) dx_1b

C write dy_1c
      NumUNIT =  IFIL+53
      CALL ERPFREE(NumUNIT,ISTAT)
      CALL FPRAND(NumUNIT,ISTAT,Ndy_1b,2,'dy1c.bin')  ! File should not already exist.
      WRITE(UNIT=NumUNIT,REC=1,IOSTAT=ISTAT) dy_1b
    
      END

***************************************************************
*15
C*********************************************************************
C InitHist
C     
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C     
C     
C This subroutine initializes the LoadHis vector 
C Containing the history of the ground loads
C
C This routine is called from InitGrnd
C     
C This subroutine is called once when the problem is read in and
C Calls StockageHist to create a file to store LoadHis
C     
C INPUTS:
C     nHis    number of terms in the LoadHis vector
C     LoadHis    vector containing the history of the ground loads
C     
C OUTPUTS:
C     LoadHis    vector containing the history of the ground loads
C     *********************************************************************
      SUBROUTINE InitHistFirst(nHis,LoadHis)

      IMPLICIT NONE 
C     INPUTS
      INTEGER nHis,i
      REAL LoadHis(nHis)

      DO 1510 i=1,nHis
         LoadHis(i)=0.0
1510  CONTINUE
      END

***************************************END***********************************
