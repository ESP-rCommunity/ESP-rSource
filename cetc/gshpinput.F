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
C input data required for the calculation of the ground loop in a GSHP system:
C     GSHPINPUT: control reading the gshp data from the gshp input file
C     GSHP_VERTICAL_READ: reads in data specific to a vertical GSHP system
C     GSHP_HORIZONTAL_2PIPE_READ: reads in data specific to a horizontal 2-pipe GSHP system
C     GSHP_HORIZONTAL_4PIPE_READ: reads in data specific to a horizontal 4-pipe GSHP system
C     GSHP_SLINKY_READ: reads in data specific to a horizontal slinky GSHP system
C     GSHP_GLOBAL_READ: used by all system types to read in global variables
C     GSHP_init: initializes GSHP variables
C
C *********************************************************************
C *********************************************************************
C GSHPINPUT

c Created by: Julia Purdy
C Initial Creation Date: December 28, 2001
C Copyright 2000: NRCan Buildings Group

C This subroutine controls reading the data associated with the 
C GSHP system. This routine is called from MZINPUT. 
C
C The routine calls one of 4 other subroutines based on the GSHP 
C system specified in the input file.
C
C This subroutine is called once when the problem is read in and 
C is used for subsequent time step simulations.
C The data read from the .gshp input file is stored in GSHP_MODULE.
C
C INPUTS:
C    gshpfile        name of gshp data file
C    igshp_type      GSHP system type being simulated
C 
C OUTPUTS:
C Input data saved to the following modules:
C    GSHP_INPUT      the main inputs for the GSHP model
C    V1_INPUT        inputs specific to vertical systems
C    HS_INPUT        inputs specific to horizontal systems
C    SL_INPUT        inputs specific to slinky systems
C *********************************************************************

       SUBROUTINE GSHPINPUT

C Use module for GSHP data


       IMPLICIT NONE
#include "hvac_parameters.h"
#include "gshp_common.h"
       COMMON/FILEP/IFIL
       COMMON/ER/IER
       COMMON/OUTIN/IUOUT,IUIN
       common/gshpinfo/igshp
       common/gshpfile/gshpfile

C Declaration of local variables
       INTEGER igshp
       INTEGER IUNIT_load,IUNIT,IFIL,K,IER,ND,i,ISTAT
       INTEGER IUOUT,IUIN
       INTEGER ival
       REAL val

c       REAL fGSHPVersionNumber   ! version number of GSHP input file
                  
       character gshpfile*72

c      common/gshp/init_called
c      LOGICAL init_called


C Define the characters variables associated with the utility 
C functions STRIPC,EGETWI,and EGETWR.
       character OUTSTR*124,word*124
       IUNIT = 99

C Set the version number
c       fGSHPVersionNumber = 0.
                      
c       Call GET_NEXT_UNIT (IUNIT,IER)

       IF ( IER .eq. 0 ) THEN 
         CALL ERPFREE(IUNIT,ISTAT)
         CALL EFOPSEQ(IUNIT,gshpfile,1,IER)
       ELSE     ! Unable to assign unit #
         Call edisp (IUOUT,'GSHP: Cannot assign input unit #.')
       ENDIF


C Read in the GSHP system type:
C  igshp_type(i)     indicates the system type 
C                 1: vertical single u-tube per borehole (V1)
C                 2: horizontal 4 pipe 2x2 arrangement (H4)
C                 3: horizontal 2 pipe side-by-side arrangement (HS)
C                 4: horizontal slinky (SL)
C  num_gshp_zones(i) the total number of zones served by the GSHP system
C  igshp_zones(i)    the specific zone numbers served by the GSHP system

C STRIPC, EGETWI, and EGETWR are standard ESP-r functions for reading 
C from data files. The MSG_EGETWI and MSG_EGETWR are functions desiged to 
C perform the same function as EGETWI and EGETWR. They use the IER error 
C to write an error message.


C Versioning Control - Determine the version number of this file.
c If it is an old file (with no version number), then we will read in the 
c values of Tmean, Atemp and DAYo (in subroutine GSHP_GLOBAL_READ), 
c but they won't be used anywhere. For newer versions, these values won't be 
c read in at all. 
c This is required for backward file compatability.

c Get first line.
       IF ( IER .eq. 0 ) THEN
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'GSHP_input',IER)
       ENDIF

C-----Read the file version number, if applicable
c       IF ( outstr(1:13).eq.'* FileVersion' ) then
c         call stripc(iunit,outstr,99,nd,0,'file ver',ier)
c         k = 13
c         call egetwr(outstr,k,fGSHPVersionNumber,0.01,100.,'F',
c     &              'version number',IER)
c--------if there is a version number present, then we need to skip to the next line
c--------to read in the next value in the GSHP file, the GSHP system type.
c         CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'GSHP_input',IER)
c       ELSE   
c--------if there is no version number, continue with reading in the BASESIMP data
c         continue    
c       ENDIF
       
C Read the type of the gshp system igshp_type
       IF ( IER .eq. 0 ) THEN
         K = 0
         CALL MSG_EGETWI(OUTSTR,K,ival,0,4,'F','igshp_type',IUOUT, IER)
         igshp_type = ival
       ENDIF

C call subroutine to read in all the general GSHP input from the .gshp file
       CALL GSHP_GLOBAL_READ(IUNIT)
c       CALL GSHP_GLOBAL_READ(IUNIT,fGSHPVersionNumber)

C The data required to simulate the performance of the gshp system depends
C on the gshp system type. For each type of gshp system, call the appropriate
C subroutine to read input data.

C If the system is a vertical single U-tube, call V1_INPUT
       if(igshp_type == 1 ) then
         call GSHP_VERTICAL_READ(IUNIT)
C If the system is a horizontal 4 pipe 2x2 arrangement, call H4_INPUT
       elseif(igshp_type == 2) then
         call GSHP_HORIZONTAL_4PIPE_READ(IUNIT)
C If the system ishorizontal 2 pipe side-by-side arrangement, call HS_INPUT
       elseif(igshp_type == 3) then
         call GSHP_HORIZONTAL_2PIPE_READ(IUNIT)
C If the system is a horizontal slinky, call SL_INPUT
       elseif(igshp_type == 4) then
         call GSHP_SLINKY_READ(IUNIT)
       else
         WRITE(IUOUT,*) ' Error GSHPINPUT: incorrect refernce number '
         WRITE(IUOUT,*) ' for the GSHP system type ',igshp_type
         return   
       endif

c The following is a dummy variable used to determine if the GSHP_init subroutine 
c has been called. If it is TRUE then the subroutine has not been called, 
c if it is FALSE, then the subroutine has been called and will not be called again.
       gshp_init_call = .TRUE.
 
c Close out the input file defined in the cfg file.
       CALL ERPFREE(IUNIT,ISTAT)

       return
       end

C ************************************************************************
C ************************************************************************
C  Subroutine to exclusively read variables for GSHP_HEATXV1
C  These variables are found in the GSHP input file
C
C  INPUTS:
C    Utube      Number of U-bends per bore hole
C    Nsoil      Number of soil layers
C    Depth      Depth of soil layers
C    Ks         Soil conductivity of each soil layer in summer in W/mK 
C    Kw           Soil conductivity of each soil layer in winter in W/mK
C    Ds         Soil diffusivity of each soil layer in summer in m^2/s
C    Dw         Soil diffusivity of each soil layer in winter in m^2/s
C    SP         Distance between pipes of U-tube in cm's
C    Dtop       Distance below surface of the top of U-tube in m's
C    Dbottom    Depth below surface of bottom of U-tube in m's
C    Nbore      Number of boreholes
C    Kgrout     Conductivity of the grouting material, in W/mK


C  OUTPUTS:
C  This subroutine reads in the above variables and makes them available
C  for the simulation in the module GSHP_VERTICAL_INPUT.
C ************************************************************************

      SUBROUTINE GSHP_VERTICAL_READ(IUNIT)

      IMPLICIT NONE
C Initiates reading of global variables (common for all)
#include "hvac_parameters.h"
#include "gshp_common.h"
C Use inputs specific to vertical configuration
#include "gshp_v1_common.h"



       common/gshpinfo/igshp
       common/gshpfile/gshpfile
       

       REAL val
       INTEGER igshp
       INTEGER ival, IUNIT, ISTAT, IER, ND, K, i
       INTEGER IUOUT, IUIN
       INTEGER FixPara         

       character gshpfile*72
       character OUTSTR*124,word*124

c GSHP 
c All the 'read' functions have been modified from the original code to 
c incorporate standard ESP-r read functions.

       IF ( IER .eq. 0 ) THEN
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'GSHP input',IER)
       ENDIF
       
c Read the number of U-bends per bore hole
       IF ( IER .eq. 0 ) THEN
          K = 0
          CALL MSG_EGETWR(OUTSTR,K,val,0.,100.,'F',
     &        'Utube',IUOUT,IER)
          Utube = val
       ENDIF

c Read the number and depths of soil layers
       IF ( IER .eq. 0 ) THEN
          CALL MSG_EGETWI(OUTSTR,K,ival,1,11,'F','Nsoil',IUOUT,IER)
          Nsoil = ival

          do 10 i = 1, Nsoil-1
            CALL MSG_EGETWR(OUTSTR,K,val,0.,100.,'F',
     &          'Depth',IUOUT,IER)
            Depth(i)= val
 10        continue
       ENDIF


c Read the data associated with each of the soil layer. Loop is
C repeated for as many times as there are soil layers.
       IF ( IER .eq. 0 ) THEN
         do 20 i = 1,Nsoil
           CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'GSHP soil',IER)

C Read in the soil properties of each soil layer, where
C K = soil conductivity, D = soil diffusivity
C (subscripts: s = summer, w = winter)
           IF ( IER .eq. 0 ) THEN
             K = 0
             CALL MSG_EGETWR(OUTSTR,K,val,0.,10000.,'F',
     &           'Ks',IUOUT,IER)
             Ks(i) = val
              
             IF ( IER .eq. 0 ) THEN
               CALL MSG_EGETWR(OUTSTR,K,val,0.,10000.,'F',
     &             'Kw',IUOUT,IER)
               Kw(i) = val
               IF ( IER .eq. 0 ) THEN
                 CALL MSG_EGETWR(OUTSTR,K,val,0.,100.,'F',
     &               'Ds',IUOUT,IER)
                 Ds(i) = val
                 IF ( IER .eq. 0 ) THEN
                   CALL MSG_EGETWR(OUTSTR,K,val,0.,100.,'F',
     &                 'Dw',IUOUT,IER)
                   Dw(i) = val
                 ENDIF
               ENDIF
             ENDIF
           ENDIF 
20       continue
       ENDIF

C Next line in .gshp input file
       IF ( IER .eq. 0 ) THEN
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'GSHP bore',IER)            
       ENDIF 

c Read in the distance between pipes of U-tube
       IF ( IER .eq. 0 ) THEN
          K = 0
          CALL MSG_EGETWR(OUTSTR,K,val,0.,100.,'F','SP',IUOUT,IER)
          SP = val
          SP = SP / 100.0          ! Converts Piping Space cm --> m
       ENDIF

c Read in the distance below surface of the top of U-tube
      IF ( IER .eq. 0 ) THEN
      CALL MSG_EGETWR(OUTSTR,K,val,0.,100.,'F','Dtop',IUOUT,IER)
          Dtop = val
       ENDIF

C Read in the depth of the borehole in m's (Dbore)
       IF ( IER .eq. 0 ) THEN 
          CALL MSG_EGETWR(OUTSTR,K,val,0.,1000.,'F',
     &        'Dbore',IUOUT,IER)
          Dbore = val
          Dbore = Dbore / 100.0      ! Bore hole diameter cm --> m
       ENDIF

C Read in the conductivity of the grouting material (Kgrout)
       IF ( IER .eq. 0 ) THEN 
          CALL MSG_EGETWR(OUTSTR,K,val,0.,1000.,'F',
     &        'Kgrout',IUOUT,IER)
          Kgrout = val
       ENDIF

       IF ( IER .eq. 0 ) THEN
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'GSHP_Misc.',IER)
       ENDIF

c FixPara = is a flag to determine whether following value is 
c Nbore or Dbottom (FixPara = 0 then value is Nbore)
      IF ( IER .eq. 0 ) THEN
          K = 0
      CALL MSG_EGETWI(OUTSTR,K,ival,0,11,'F','FixPara',IUOUT,IER)
          FixPara = ival
      ENDIF
    
c Number of Boreholes (Nbore) or bottom depth (Dbottom) depending on FixPara
       IF (FixPara .EQ. 0) THEN
          IF ( IER .eq. 0 ) THEN
             CALL MSG_EGETWI(OUTSTR,K,ival,1,100,'F',
     &           'Nbore',IUOUT,IER)
             Nbore = ival
        Dbottom = Dtop + 0.5 * (pipe_length/Nbore)/Utube
          ENDIF
       ELSE
          IF ( IER .eq. 0 ) THEN
             CALL MSG_EGETWR(OUTSTR,K,val,0.,1000.,'F',
     &           'Dbottom',IUOUT,IER)
             Dbottom = val
        Nbore = INT(0.5 * pipe_length/(Dbottom - Dtop)/Utube + 0.5)
           IF (Nbore .LT. 1) Nbore = 1
             Dbottom = Dtop + 0.5 * (pipe_length/Nbore)/Utube  ! Must adjust Dbottom
               ENDIF                                                ! since Nbore must 
          ENDIF                                                     ! remain an Integer
    
C Close .gshp input file.
       CALL ERPFREE(IUNIT,ISTAT)

       return
       end


C ****************************************************************************
C ****************************************************************************
C GSHP_HORIZONTAL_2PIPE_READ
C Subroutine to exclusively read variables for HEATXHS
C   
C  These variables are found in the GSHP input file   
C  INPUTS:
C    Ks          Soil conductivity of each soil layer in summer in W/mK 
C    Kw            Soil conductivity of each soil layer in winter in W/mK
C    Ds          Soil diffusivity of each soil layer in summer in m^2/s
C    Dw          Soil diffusivity of each soil layer in winter in m^2/s
C    SD          Distance between horizontal lying pipes in m
C    BD          Depth of the heat exchanger pipe in m

C  OUTPUTS:
C  This subroutine reads in the above variables and makes them available
C  for the simulation in the module GSHP_HORIZONTAL_INPUT.   
C ****************************************************************************

       SUBROUTINE GSHP_HORIZONTAL_2PIPE_READ(IUNIT)


       IMPLICIT NONE

C Initiates reading of global variables (common for all)
#include "hvac_parameters.h"
#include "gshp_common.h"
C Use inputs specific to configuration
#include "gshp_hs_common.h"


       COMMON /OUTIN/ IUOUT, IUIN

       common/gshpinfo/igshp
       common/gshpfile/gshpfile
       
       REAL val
       INTEGER igshp
       INTEGER ival, IUNIT, ISTAT, IER, ND, K, i
       INTEGER IUOUT, IUIN


       character gshpfile*72
       character OUTSTR*124,word*124   

       IF ( IER .eq. 0 ) THEN
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'GSHP_input',IER)
       ENDIF

C Read in the soil properties, where K = soil conductivity, 
C D = soil diffusivity. The subscripts represent s = summer, w = winter.
       IF ( IER .eq. 0 ) THEN
          K = 0
      CALL MSG_EGETWR(OUTSTR,K,val,0.,10000.,'F','Ks',IUOUT,IER)
          Ks = val
       ENDIF

       IF ( IER .eq. 0 ) THEN       
          CALL MSG_EGETWR(OUTSTR,K,val,0.,10000.,'F','Kw',IUOUT,IER)
          Kw = val
       ENDIF

       IF ( IER .eq. 0 ) THEN
          CALL MSG_EGETWR(OUTSTR,K,val,0.,100.,'F','Ds',IUOUT,IER)
          Ds = val
       ENDIF

       IF ( IER .eq. 0 ) THEN 
          CALL MSG_EGETWR(OUTSTR,K,val,0.,100.,'F','Dw',IUOUT,IER)
          Dw = val
       ENDIF

C Next line in the .gshp input file
       IF ( IER .eq. 0 ) THEN
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'GSHP_bore',IER)  
       ENDIF

C Read in depth of the HX pipe (BD) and the distance between horizontal 
c lying pipes (SD).
       IF ( IER .eq. 0 ) THEN
          K = 0
          CALL MSG_EGETWR(OUTSTR,K,val,0.,1000.,'F',
     &        'SD',IUOUT,IER)
          SD = val
       ENDIF

       IF ( IER .eq. 0 ) THEN       
          CALL MSG_EGETWR(OUTSTR,K,val,0.,100.,'F',
     &        'BD',IUOUT,IER)
          BD = val
       ENDIF   

C Close .gshp input file.   
       CALL ERPFREE(IUNIT,ISTAT)
      
       return
       end


C ****************************************************************************
c ****************************************************************************
c GSHP_HORIZONTAL_4PIPE_READ
C Subroutine to exclusively read variables for GSHP_HEATXH4 
C
C  These variables are found in the GSHP input file   
C  INPUTS:
C    Ks         Soil conductivity of each soil layer in summer, W/mK 
C    Kw           Soil conductivity of each soil layer in winter, W/mK
C    Ds         Soil diffusivity of each soil layer in summer, m^2/s
C    Dw         Soil diffusivity of each soil layer in winter, m^2/s
C    PD1        Depth of the first heat exchanger pipes, m
C    PD2        Depth of the second heat exchanger pipes, m
C    SD         Distance between horizontal lying pipes, m
C
C  OUTPUTS:
C  This subroutine reads in the above variables and makes them available
C  for the simulation in the module GSHP_HORIZONTAL_INPUT.   
C *****************************************************************************

       SUBROUTINE GSHP_HORIZONTAL_4PIPE_READ(IUNIT)

       IMPLICIT NONE
C Initiates reading of global variables (common for all)
#include "hvac_parameters.h"
#include "gshp_common.h"
C Use inputs specific to vertical configuration
#include "gshp_hs_common.h"
       COMMON /OUTIN/ IUOUT, IUIN

       common/gshpinfo/igshp
       common/gshpfile/gshpfile

       REAL val
       INTEGER igshp
       INTEGER ival, IUNIT, ISTAT, IER, ND, K, i
       INTEGER IUOUT, IUIN

       character gshpfile*72
       character OUTSTR*124,word*124

C STRIPC is a common ESP-r subroutine to indicate a new record
       IF ( IER .eq. 0 ) THEN  
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'GSHP_input',IER)
       ENDIF

C Read in the soil properties from the GSHP input file, where 
C K = soil conductivity in W/mK 
c D = soil diffusivity in m^2/s (subscripts: s = summer, w = winter)
       IF ( IER .eq. 0 ) THEN  
          K = 0
       CALL MSG_EGETWR(OUTSTR,K,val,0.,10000.,'F','Ks',IUOUT,IER)
          Ks = val
       ENDIF

       IF ( IER .eq. 0 ) THEN  
          CALL MSG_EGETWR(OUTSTR,K,val,0.,10000.,'F','Kw',IUOUT,IER)
          Kw = val
       ENDIF

       IF ( IER .eq. 0 ) THEN  
          CALL MSG_EGETWR(OUTSTR,K,val,0.,100.,'F','Ds',IUOUT,IER)
          Ds = val
       ENDIF

       IF ( IER .eq. 0 ) THEN  
          CALL MSG_EGETWR(OUTSTR,K,val,0.,100.,'F','Dw',IUOUT,IER)
          Dw = val
       ENDIF

       IF ( IER .eq. 0 ) THEN  
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'GSHP_pipe',IER)
       ENDIF      
C Read in depth of the HX pipes 1 and 2(PD) and the distance between horizontal 
c lying pipes (SD)
       IF ( IER .eq. 0 ) THEN  
          K = 0       
          CALL MSG_EGETWR(OUTSTR,K,val,0.,1000.,'F','PD1',IUOUT,IER)
          PD1 = val
       ENDIF

       IF ( IER .eq. 0 ) THEN  
          CALL MSG_EGETWR(OUTSTR,K,val,0.,1000.,'F','PD2',IUOUT,IER)
          PD2 = val
       ENDIF

       IF ( IER .eq. 0 ) THEN  
          CALL MSG_EGETWR(OUTSTR,K,val,0.,1000.,'F','SD',IUOUT,IER)
          SD = val
       ENDIF

C Close .gshp input file.   
       CALL ERPFREE(IUNIT,ISTAT)

       return
       end

   
C ****************************************************************************
c ****************************************************************************
c GSHP_SLINKY_READ
C Subroutine to exclusively read variables for GSHP_HEATXSL 
C
C These variables are found in the GSHP input file
C
C  INPUTS:
C    Ks         Soil conductivity of each soil layer in summer, W/mK 
C    Kw           Soil conductivity of each soil layer in winter, W/mK
C    Ds         Soil diffusivity of each soil layer in summer, m^2/s
C    Dw         Soil diffusivity of each soil layer in winter, m^2/s
C    NumP       Number of pipes per trench (between 2 and 8)
C    Spiral     Diameter of slinky spirals, m
C    BD         Depth of the heat exchanger pipe, m
C
C  OUTPUTS:
C  This subroutine reads in the above variables and makes them available
C  for the simulation in the module GSHP_SLINKY_INPUT.   
C ****************************************************************************
       SUBROUTINE GSHP_SLINKY_READ(IUNIT)
   


       IMPLICIT NONE
C Initiates reading of global variables (common for all)
#include "hvac_parameters.h"
#include "gshp_common.h"
C Use inputs specific to vertical configuration
#include "gshp_sl_common.h"
       COMMON /OUTIN/ IUOUT, IUIN
       common/gshpinfo/igshp
       common/gshpfile/gshpfile
       
       REAL val
       INTEGER igshp
       INTEGER ival, IUNIT, ISTAT, IER, ND, K, i
       INTEGER IUOUT, IUIN

       character gshpfile*72
       character OUTSTR*124,word*124


       CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'GSHP_input',IER)
      
C Read in the soil properties from the GSHP input file, where 
C K = soil conductivity in W/mK 
c D = soil diffusivity in m^2/s (subscripts: s = summer, w = winter)
       IF ( IER .eq. 0 ) THEN  
          K = 0
       CALL MSG_EGETWR(OUTSTR,K,val,0.,10000.,'F','Ks',IUOUT,IER)
          Ks = val
       ENDIF

       IF ( IER .eq. 0 ) THEN  
          CALL MSG_EGETWR(OUTSTR,K,val,0.,10000.,'F','Kw',IUOUT,IER)
          Kw = val
       ENDIF

       IF ( IER .eq. 0 ) THEN  
          CALL MSG_EGETWR(OUTSTR,K,val,0.,100.,'F','Ds',IUOUT,IER)
          Ds = val
       ENDIF

       IF ( IER .eq. 0 ) THEN  
          CALL MSG_EGETWR(OUTSTR,K,val,0.,100.,'F','Dw',IUOUT,IER)
          Dw = val
       ENDIF

C Next line in .gshp input file
       IF ( IER .eq. 0 ) THEN 
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'GSHP_slinky',IER)
       ENDIF

C Read in depth of the HX pipes (BD) 
       IF ( IER .eq. 0 ) THEN 
          K = 0 
          CALL MSG_EGETWR(OUTSTR,K,val,0.,1000.,'F','BD',IUOUT,IER)
          BD = val
       ENDIF

c Read in the the diameter of Slinky spirals in metres (Spiral)
       IF ( IER .eq. 0 ) THEN
          CALL MSG_EGETWR(OUTSTR,K,val,0.,1000.,'F',
     &        'Spiral',IUOUT,IER)
          Spiral = val
       ENDIF

c Read in the number of pipes per trench between 2 and 8 (NumP)
       IF ( IER .eq. 0 ) THEN
          CALL MSG_EGETWR(OUTSTR,K,val,2.,8.,'F','NumP',IUOUT,IER)
          NumP = val
       ENDIF

C Close .gshp input file
       CALL ERPFREE(IUNIT,ISTAT)
      
       return
       end
   

C ****************************************************************************
C ****************************************************************************
C GSHP_GLOBAL_READ
C This subroutine is used by all types of GSHP calculations.
C The variables, data types, and layout read in this file are
C identical for each GSHP calculation and are found in the GSHP input file.
C These variables are saved in the USE module GSHP_INPUT.
C
C  INPUTS:
C    ID           Inside diameter of pipe, m
C    OD           Outside diameter of pipe, m
C    Kpipe        Conductivity of pipe, W/mK
C    pipe_length  Length of pipe, m
C    Density      Fluid density, kg/m3
C    Cp           Specific heat of fluid, kJ/kgC
C    Flow         Fluid flow rate, L/s
C    Tmean        Mean ground temperature, C
C    Atemp        Temperaure of the ground surface, C
C    DAYo         Time of the minimum surface temperature, Day
C
C  OUTPUTS:
C  This subroutine reads in the above variables and makes them available
C  for the simulation.   
C
C ****************************************************************************

       SUBROUTINE GSHP_GLOBAL_READ(IUNIT)
c       SUBROUTINE GSHP_GLOBAL_READ(IUNIT,fGSHPVersionNumber)



       IMPLICIT NONE
C Initiates reading of global variables (common for all)
#include "hvac_parameters.h"
#include "gshp_common.h"

       COMMON /OUTIN/ IUOUT, IUIN
       common/gshpinfo/igshp
       common/gshpfile/gshpfile
       

       REAL val
       INTEGER igshp
       INTEGER ival, IUNIT, ISTAT, IER, ND, K, i
       INTEGER IUOUT, IUIN      
c      REAL fGSHPVersionNumber   ! Version number of GSHP file
       
       
C Define the characters variables associated with the utility 
C functions STRIPC,EGETWI,and EGETWR.
       character gshpfile*72
       character OUTSTR*124,word*124
   
C Pipe Sizing and Properties
       IF ( IER .eq. 0 ) THEN 
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'GSHP pipe',IER)
       ENDIF
C Read in the GSHP pipe inside diameter using a subroutine (MSG_EGETWR)
C that uses the same procedure as EGETWR - a standard ESP-r subroutine -
C except that it inserts " Cannot read " to the start of the message string 
C and calls edisp when errors occur.
       IF ( IER .eq. 0 ) THEN       
          K = 0
          CALL MSG_EGETWR(OUTSTR,K,val,0.,1000.,'F','ID',IUOUT, IER)          
          ID = val
          ID = ID/1000        ! Convert to metres
       ENDIF

C Read in the GSHP pipe outside diameter         
       IF ( IER .eq. 0 ) THEN       
          CALL MSG_EGETWR(OUTSTR,K,val,0.,1000.,'F','OD',IUOUT, IER)
          OD = val
          OD = OD/1000        ! Convert to metres
       ENDIF

C Read in the GSH pipe conductivity
       IF ( IER .eq. 0 ) THEN
          CALL MSG_EGETWR(OUTSTR,K,val,0.,10.,'F','Kpipe',IUOUT, IER)
          Kpipe = val
       ENDIF

C Read in the lenght of the pipe
       IF ( IER .eq. 0 ) THEN
          CALL MSG_EGETWR(OUTSTR,K,val,0.,10000.,'F','pipe_length',
     &                    IUOUT, IER)
          pipe_length = val
       ENDIF 

C New record in .gshp file.
C Fluid Properties
       IF ( IER .eq. 0 ) THEN
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'GSHP_fluid',IER)
       ENDIF

C Read in the fluid density
       IF ( IER .eq. 0 ) THEN
          K = 0
          CALL MSG_EGETWR(OUTSTR,K,val,0.,10000.,'F',
     &         'Density',IUOUT,IER)
          Density = val
       ENDIF 

C Read in the fluid heat capacity
       IF ( IER .eq. 0 ) THEN
          CALL MSG_EGETWR(OUTSTR,K,val,0.,100.,'F','Cp',IUOUT,IER)
          Cp = val
       ENDIF

C Read in the heat exchanger flow rate
       IF ( IER .eq. 0 ) THEN
          CALL MSG_EGETWR(OUTSTR,K,val,0.,100.,'F','Flow',IUOUT,IER)
          Flow = val
       ENDIF

c***********************************************************************
c These references to ground temperature have been removed from the gshp
c input file. The ground temperatures are now calculated from the climate
c data in /cetc/ground_temp_mains_common.h
c***********************************************************************       

c------If there is a version number associated with the GSHP input file, then 
c------the variables Tmean, Atemp and DAYo are not read in, and the subroutine
c------returns to the calling subroutine.
C       IF(fGSHPVersionNumber .NE. 0.) RETURN

c------If there is no version number associated with the GSHP input file, 
c------then, these values are read in, but are not used in the simulation.

C Read annually-averaged soil temperature.

C New record in .gshp file.
C Local ground properties
        IF ( IER .eq. 0 ) THEN
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'GSHP_ground',IER)
        ENDIF
c
c Read in the mean ground temperature (Tmean)
        IF ( IER .eq. 0 ) THEN
          K = 0
          CALL MSG_EGETWR(OUTSTR,K,val,0.,100.,'F','Tmean',IUOUT,IER)
         Tmean = val
        ENDIF
c
C Read in the surface temperature amplitude(Atemp)
        IF ( IER .eq. 0 ) THEN
          CALL MSG_EGETWR(OUTSTR,K,val,0.,100.,'F','Atemp',IUOUT, IER)
         Atemp = val
        ENDIF
c
C Read in the time of the minimum surface temperature (DAYo)
        IF ( IER .eq. 0 ) THEN
          CALL MSG_EGETWI(OUTSTR,K,ival,1,365,'F','DAYo',IUOUT,IER)
         DAYo = ival 
        ENDIF

       return
       end


c *********************************************************************
c *********************************************************************
c GSHP_init

c Created by: Julia Purdy
c Initial Creation Date: May 7, 2002
c Copyright 2000: NRCan Buildings Group

c Subroutine to initialize variables associated with GSHP modelling. This
c subroutine is called every time the gshpsim is called, but the 
c initialization will take place only once per simulation.

c INPUTS:
c    GSHP_DAILY   module 
c
c OUTPUTS:
c    EWT          Ground loop entering water temperature (degrees Celcius)
c    TimeDay      The length in time of each period (Days)
c    StartDay     Simulation start day (Days)
c    StopDay      Simulation stop day (Days)
c    Period       Simulation length (Days)
c    Day          Day of simulation (Days)
c 
c *********************************************************************
      SUBROUTINE GSHP_init




       IMPLICIT NONE
C Initiates reading of global variables (common for all)
#include "hvac_parameters.h"
#include "gshp_common.h"

       common/simsdy/iss,isf

       INTEGER iss,isf


c This subroutine should only be called once during the simulation.
c gshp_init_call is a dummy variable used to determine if the GSHP_init 
c subroutine has been called. If it is TRUE then the subroutine has not been 
c called, if it is FALSE, then the subroutine has been called and will not be 
c called again. . 
        if (gshp_init_call) then 
        
c Initialize the entering water temperature.
          EWT = 5.

C Time of each period in days (ie: always 1)
          TimeDay = 1

c Define StartDay and StopDay in terms of common block variables - located in simsdy. 
C In the original program, they would be read in from the input file.
          StartDay = iss
          StopDay = isf
C Period of Simulation
          Period = (StopDay - StartDay) + 1

          Day = StartDay - TimeDay   ! Start calc @ begining of simulation
                 ! Note: TimeDay added before calc's done.
               ! So need to subtract from StartDay
               ! (ie: StartDay =1 then Day = 0)


          gshp_init_call = .FALSE.
        else
          gshp_init_call = .FALSE.  
         return
        end if
 
      return
      end
   
