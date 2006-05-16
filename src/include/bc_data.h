C
C     bc_data.h
C
C     This file contains data structures used by the flexible
C     boundary condition definition facility
C
C
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C     Maximum number of boundry conditions
C----------------------------------------------------------------------
      integer mBC_max_count
      parameter ( mBC_max_count = 20 )


C----------------------------------------------------------------------
C     Flag indicating boundry conditions have been defined
C----------------------------------------------------------------------
      logical bBC_data_defined

C----------------------------------------------------------------------
C     Flag indicating that boundary condition procedures have
C     encountered an error
C----------------------------------------------------------------------
      logical bBC_data_err
      
C----------------------------------------------------------------------      
C     Name of boundary condition definition file
C----------------------------------------------------------------------
      character*72 cBC_data_file_name

C----------------------------------------------------------------------
C     Unit number for temporary 
C----------------------------------------------------------------------
      integer iBC_TmpFile

C----------------------------------------------------------------------
C     Version number
C----------------------------------------------------------------------
      real fBC_file_version

C----------------------------------------------------------------------
C     Frequency of BC samples (seconds)
C----------------------------------------------------------------------
      real fBC_frequency

C----------------------------------------------------------------------
C     Start and end of boundary condition period
C----------------------------------------------------------------------      
      integer iBC_Start_day
      integer iBC_End_day


C----------------------------------------------------------------------
C     # of defined boundary conditions
C----------------------------------------------------------------------
      integer iBC_col_count           ! columns
      integer iBC_row_count
C----------------------------------------------------------------------
C     Boundry condition data names, units & corresponding column #'s
C----------------------------------------------------------------------
      character*248 cBC_names ( mBC_max_count )
      character*248 cBC_units ( mBC_max_count )

C----------------------------------------------------------------------
C     Common data structure
C----------------------------------------------------------------------      
      common / BC_data_status / bBC_data_defined,
     &                          cBC_data_file_name,
     &                          iBC_TmpFile,
     &                          cBC_names,
     &                          cBC_units,
     &                          iBC_Start_day,
     &                          iBC_End_day,
     &                          fBC_frequency,
     &                          iBC_col_count,
     &                          iBC_row_count  

C----------------------------------------------------------------------
C     Named constant for temp file name 
C----------------------------------------------------------------------     
      character*72 cBC_TmpFile_name
      data cBC_TmpFile_name /'BCD.tmp'/
      character*72 cTmpFile
      data cTmpFile /'BCD_int.tmp'/
C----------------------------------------------------------------------
C     Named constants for interpolation methods
C----------------------------------------------------------------------
      integer iStep_Interp          ! Stepwise interpolation
      integer iLinear_Interp        ! Linear interpolation 
      parameter ( iStep_Interp = 1, iLinear_Interp = 2 )

