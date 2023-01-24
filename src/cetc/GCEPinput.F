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

C This file contains the following subroutines used for reading in the
C input data required for the calculation of the ground loop in a GCHP system:
C  GCEPINPUT: control reading the GCEP data from the GCEP input file
C  GCEP_VARIABLE_READ subroutine that read in all the GCEP input from the .gcep file
C
C *********************************************************************
C *********************************************************************
C GCEPINPUT

c Created by: Patrice Pinel
C Initial Creation Date: October 2002
C
C This subroutine controls reading the data associated with the 
C GCEP system. This routine is called from MZINPUT. 
C
C This subroutine is called once when the problem is read in and 
C is used for subsequent time step simulations.
C The data read from the .gcepm input file is stored in GCEP_MODULE.
C
C INPUTS:
C    gcepfile        name of gcep data file
C 
C OUTPUTS:
C Input data saved to the following modules:
C GCEP_INPUT     the main inputs for the GCEP model
C *********************************************************************
      SUBROUTINE GCEPINPUT

C Use module for GCEP data
C     USE GCEP_INPUT

      IMPLICIT NONE
#include "gcep_parameters.h"
#include "gcep_common.h"
      COMMON/FILEP/IFIL
      COMMON/ER/IER
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/gcepinfo/igcep
      common/gcepfile/gcepfile

C Declaration of local variables
      INTEGER igcep
      INTEGER IUNIT_load,IUNIT,IFIL,K,IER,ND,i,ISTAT
      INTEGER IUOUT,IUIN,IEOUT
      INTEGER ival
      REAL val
       
      character gcepfile*72

      common/gcep/init_called
      LOGICAL init_called

C Define the characters variables associated with the utility 
C functions STRIPC,EGETWI,and EGETWR.
      character OUTSTR*124,word*124
      IUNIT = 99

c      Call GET_NEXT_UNIT (IUNIT,IER)

      IF ( IER .eq. 0 ) THEN 
        CALL ERPFREE(IUNIT,ISTAT)
        CALL EFOPSEQ(IUNIT,gcepfile,1,IER)
      ELSE     ! Unable to assign unit #
        Call edisp (IUOUT,'GCEP: Cannot assign input unit #.')
      ENDIF
 

C STRIPC, EGETWI, and EGETWR are standard ESP-r functions for reading 
C from data files. The MSG_EGETWI and MSG_EGETWR are functions desiged to 
C perform the same function as EGETWI and EGETWR. They use the IER error 
C to write an error message.

C      IF ( IER .eq. 0 ) THEN
C        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'GCEP_input',IER)
C      ENDIF

C call subroutine to read in all the GCEP input from the .gcep file
      CALL GCEP_VARIABLE_READ(IUNIT)

c The following is a dummy variable used to determine if the GCEP_init subroutine 
c has been called. If it is TRUE then the subroutine has not been called, 
c if it is FALSE, then the subroutine has been called and will not be called again.
      gcep_init_call = .TRUE.
 
c Close out the input file defined in the cfg file.
      CALL ERPFREE(IUNIT,ISTAT)

       
      return
      end

C ************************************************************************
C ************************************************************************
C  GCEP_VARIABLE_READ
C
C  Subroutine to exclusively read variables for GCEP_INPUT
C  These variables are found in the GCEP input file


C  INPUTS:
C Ground properties
C       K_s             ground thermal conductivity [W/mK]
C       Alpha_s         ground thermal difusivity [m^2/day]
C       Rho_s           ground density [Kg/m^2]
C Grout properties
C       K_g             grout thermal conductivity [W/mK]
C Borehole dimensions
C       D_bore          diameter [m]
C       Deapth_bore     length [m]
C Borehole layout in field (N_x= , L_x=distance between boreholes in x direction, N_y=..., L_y=...)
C       N_bor_x         number in x direction [boreholes]
C       N_bor_y         number in y direction [boreholes]
C       L_bort_x        distance between boreholes in x direction [m]
C       L_bor_y         distance between boreholes in y direction [m]
C Tubes layout in borehole (choice)
C       Tube_Case       [1..6]
C               1=2 tubes (1U) Tubes in contact at the center of the borehole
C               2=2 tubes (1U) Tubes midway between borehole center and wall
C               3=2 tubes (1U) Tubes in contact with borehole wall (max distance)
C               4=4 tubes (2U) Tubes in contact at the center of the borehole
C               5=4 tubes (2U) Tubes midway between borehole center and wall
C               6=4 tubes (2U) Tubes in contact with borehole wall (max distance)
C Tubes dimensions (choice)
C       TypeTubes  [1..12]        OD(cm) ID(cm)
C       1       0.5"    SDR-11    2.67   2.18
C       2       1"      SDR-11    3.34   2.73
C       3       1­.5"    SDR-11    4.22   3.45
C       4       25 mm   SDR-11    2.50   2.04
C       5       32 mm   SDR-11    3.20   2.60
C       6       40 mm   SDR-11    4.00   3.26
C       7       0.5"    SDR-135   2.67   2.27
C       8       1"      SDR-135   3.34   2.85
C       9       1.5­"    SDR-135   4.22   3.59
C      10       25 mm   SDR-135   2.50   2.04
C      11       32 mm   SDR-135   3.20   2.62
C      12       40 mm   SDR-135   4.00   3.40
C Tube properties (k=conductivity)
C       K_t            Tubes thermal conductivity [W/m.K]
C Fluid property (k=conductivity, rho=density, Cp=specific heat, Mu=dynamic viscosity)
C       K_f             Fluid thermal conductivity [W/m.K]
C       Rho_f           Fluid density [kg/m^3]
C       Cp_f            Fluid specific heat [J/kg.K]
C       Mu_f            Fluid dynamic viscosity [kg/m.s]
C Undisturbed ground temperature
C       Tg_ND           [K]
C Initial ground temperature
C       Tg_init         [K]
C Flow of fluid to the ground
C       GCEP_Flow       [l/s]
C
C  OUTPUTS:
C  This subroutine reads in the above variables and makes them available
C  for the simulation in the module GCEP_INPUT.
C ************************************************************************
      SUBROUTINE GCEP_VARIABLE_READ(IUNIT)

C Initiates reading of global variables (common for all)
C      USE GCEP_INPUT
       IMPLICIT NONE

#include "gcep_parameters.h"
#include "gcep_common.h"
      common/gcepinfo/igcep
      common/gcepfile/gcepfile
       

      REAL    val
      INTEGER igcep
      INTEGER ival, IUNIT, ISTAT, IER, ND, K, i
      INTEGER IUOUT, IUIN,IEOUT
      INTEGER FixPara         

      character gcepfile*72
      character OUTSTR*124,word*124

c GCEP 
c All the 'read' functions have been modified from the original code to 
c incorporate standard ESP-r read functions.


c************************Ground Properties******************
      IF ( IER .eq. 0 ) THEN
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'GCEP ground prop',IER)
      ENDIF
c Read the ground thermal conductivity [0..+inf[
      IF ( IER .eq. 0 ) THEN       
        K = 0
        CALL MSG_EGETWR(OUTSTR,K,val,0.,100000.,'F','K_s',IUOUT,IER)
        GCEP_K_s = val
      ENDIF
c Read the ground thermal diffusivity [0..+inf[
      IF ( IER .eq. 0 ) THEN       
        CALL MSG_EGETWR(OUTSTR,K,val,0.,100000.,'F','Alpha_s',IUOUT,IER)
        GCEP_Alpha_s = val
      ENDIF
c Read the ground density [0..+inf[
      IF ( IER .eq. 0 ) THEN       
        CALL MSG_EGETWR(OUTSTR,K,val,0.,100000.,'F','Rho_s',IUOUT,IER)
        GCEP_Rho_s = val
      ENDIF
c************************Grout Properties******************
      IF ( IER .eq. 0 ) THEN
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'GCEP grout prop',IER)
      ENDIF
c Read the grout thermal conductivity [0..+inf[
      IF ( IER .eq. 0 ) THEN       
        K = 0
        CALL MSG_EGETWR(OUTSTR,K,val,0.,100000.,'F','K_c',IUOUT,IER)
        GCEP_K_g = val
      ENDIF
c************************Boreholes dimensions******************
      IF ( IER .eq. 0 ) THEN
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'GCEP bore dim',IER)
      ENDIF
c Read the Boreholes diameter [0..+inf[
      IF ( IER .eq. 0 ) THEN       
        K = 0
        CALL MSG_EGETWR(OUTSTR,K,val,0.,100000.,'F','D_bore',IUOUT,IER)
        GCEP_D_bore = val
      ENDIF
c Read the Boreholes vertical length [0..+inf[
      IF ( IER .eq. 0 ) THEN       
        CALL MSG_EGETWR(OUTSTR,K,val,0.,100000.,'F','Deapth_bore',IUOUT,
     & IER)
        GCEP_Deapth_bore = val
      ENDIF
c***********************Boreholes Layout***********************
      IF ( IER .eq. 0 ) THEN
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'GCEP bore layout',IER)
      ENDIF
c Read the number of boreholes in X direction [1..+inf[
      IF ( IER .eq. 0 ) THEN
         K = 0
         CALL MSG_EGETWI(OUTSTR,K,ival,1,100000,'F','N_bor_x',IUOUT,IER)
         GCEP_N_bor_x = ival
      ENDIF
c Read the number of boreholes in Y direction [1..+inf[
      IF ( IER .eq. 0 ) THEN
         CALL MSG_EGETWI(OUTSTR,K,ival,1,100000,'F','N_bor_y',IUOUT,IER)
         GCEP_N_bor_y = ival
      ENDIF
c Read the distance between Boreholes in X direction [0..+inf[
      IF ( IER .eq. 0 ) THEN       
        CALL MSG_EGETWR(OUTSTR,K,val,0.,100000.,'F','L_bor_x',IUOUT,
     &  IER)
        GCEP_L_bor_x = val
      ENDIF
c Read the distance between Boreholes in Y direction [0..+inf[
      IF ( IER .eq. 0 ) THEN       
        CALL MSG_EGETWR(OUTSTR,K,val,0.,100000.,'F','L_bor_y',IUOUT,
     &  IER)
        GCEP_L_bor_y = val
      ENDIF
c***********************Tube Layout***********************
      IF ( IER .eq. 0 ) THEN
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'GCEP tube layout',IER)
      ENDIF
c Read the layout of the tubes in the borehole [1..6]
      IF ( IER .eq. 0 ) THEN
        K = 0
        CALL MSG_EGETWI(OUTSTR,K,ival,1,6,'F','Tube_Case',IUOUT,IER)
        GCEP_Tube_Case = ival
      ENDIF
c Read the tubes dimensions [1..12]
      IF ( IER .eq. 0 ) THEN
        CALL MSG_EGETWI(OUTSTR,K,ival,1,12,'F','TypeTubes',IUOUT,IER)
        GCEP_TypeTubes = ival
      ENDIF
c Read the tubes thermal conductivity [0..+inf[
      IF ( IER .eq. 0 ) THEN       
        CALL MSG_EGETWR(OUTSTR,K,val,0.,100000.,'F','K_t',IUOUT,IER)
        GCEP_K_t = val
      ENDIF
c Read the pipe length [0..+inf[
      IF ( IER .eq. 0 ) THEN       
        CALL MSG_EGETWR(OUTSTR,K,val,0.,100000.,'F','GCEP_pipe_length',
     &IUOUT,IER)
        GCEP_pipe_length = val
      ENDIF
c***********************Fluide Properties***********************
      IF ( IER .eq. 0 ) THEN
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'GCEP fluide',IER)
      ENDIF
c Read the fluid thermal conductivity [0..+inf[
      IF ( IER .eq. 0 ) THEN       
        K = 0
        CALL MSG_EGETWR(OUTSTR,K,val,0.,100000.,'F','K_f',IUOUT,IER)
        GCEP_K_f = val
      ENDIF
c Read the fluid density [0..+inf[
      IF ( IER .eq. 0 ) THEN       
        CALL MSG_EGETWR(OUTSTR,K,val,0.,100000.,'F','Rho_f',IUOUT,IER)
        GCEP_Rho_f = val
      ENDIF
c Read the fluid specific heat [0..+inf[
      IF ( IER .eq. 0 ) THEN       
        CALL MSG_EGETWR(OUTSTR,K,val,0.,100000.,'F','Cp_f',IUOUT,IER)
        GCEP_Cp_f = val
      ENDIF
c Read the fluid dynamic viscosity [0..+inf[
      IF ( IER .eq. 0 ) THEN       
        CALL MSG_EGETWR(OUTSTR,K,val,0.,100000.,'F','Mu_f',IUOUT,IER)
        GCEP_Mu_f = val
      ENDIF
c Read the fluid flow [0..+inf[
      IF ( IER .eq. 0 ) THEN       
        CALL MSG_EGETWR(OUTSTR,K,val,0.,100000.,'F','GCEP_Flow',IUOUT,
     &  IER)
        GCEP_Flow = val
      ENDIF

c***********************Reference Temperatures***********************
      IF ( IER .eq. 0 ) THEN
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'GCEP ref temp',IER)
      ENDIF
c Read the undisturbed ground temperature ]-inf..+inf[
      IF ( IER .eq. 0 ) THEN       
        K = 0
        CALL MSG_EGETWR(OUTSTR,K,val,-1000.,1000.,'F','Tg_ND',IUOUT,IER)
        GCEP_Tg_ND = val
      ENDIF
c Read the initial ground temperature ]-inf..+inf[
      IF ( IER .eq. 0 ) THEN       
        CALL MSG_EGETWR(OUTSTR,K,val,0.,1000.,'F','Tg_init',IUOUT,IER)
        GCEP_Tg_init= val
      ENDIF


C Close .gcep input file.
       CALL ERPFREE(IUNIT,ISTAT)

       return
       end
C *********************************************************************
