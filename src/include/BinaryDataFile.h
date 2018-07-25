C ********************************************************************************
C This file contains common blocks, declarations, and assignments related
C to binary data file handling.
C ********************************************************************************

      integer MBDF

      PARAMETER ( MBDF = 3 )        !-maximum number of binary data files

C---------------------------------------------------------------------------------
C Functionality specific common
C---------------------------------------------------------------------------------
      common/BinaryDataFileName/fnamBDFlds(MBDF)
      character fnamBDFlds*72     ! ASCII file name

      common/BinaryDataFile/iBDFlds(MBDF),
     &   iBDF_lds_type(MBDF),iFIL_BDF(MBDF),
     &   BDF_lds_per_rec(MBDF),BDF_lds_per_rec_bin(MBDF)
      integer iBDFlds             ! Flag for "first call" tracking
      integer iBDF_lds_type       ! 1 ... yearly data, 2 ... daily data
      integer iFIL_BDF            ! Binary file number
      INTEGER BDF_lds_per_rec     ! Number of data columns in ASCII file
      INTEGER BDF_lds_per_rec_bin ! Number of data fields per record in binary file


C---------------------------------------------------------------------------------
C Method for prescribing electrical loads on fuel cell.
C    FC_lds_annual    named constant to indicate there is a record for each hour
C                     of the year (i.e. 8760 records).
C    FC_lds_daily     named constant to indicate there is record for each minute
C                     of a single day (i.e. 1440 records).
C---------------------------------------------------------------------------------
      INTEGER BDF_lds_annual,BDF_lds_daily
      PARAMETER ( BDF_lds_annual = 1 )
      PARAMETER ( BDF_lds_daily  = 2 )

C---------------------------------------------------------------------------------
C Number of data items per record of electric loads data in user-specified
C .fcl ASCII file. For `FC_lds_annual' input, the user can specify loads for
C up to 12 increments per hour (i.e. every 5 minutes). For `FC_lds_daily' input,
C the user can specify loads for up to 6 increments per minute (i.e. every 10
C seconds). The direct access file used during the time-step simulation always
C holds 12 increments of data for `FC_lds_annual' and 6 increments of
C data for `FC_lds_daily'.
C    FC_lds_per_rec_annual  named constant to indicate number of records of
C                           loads data for `FC_lds_annual'
C    FC_lds_per_rec_daily   named constant to indicate number of records of
C                           loads of data for `FC_lds_daily'
C---------------------------------------------------------------------------------
      INTEGER BDF_lds_per_rec_annual,BDF_lds_per_rec_daily
      PARAMETER ( BDF_lds_per_rec_annual = 12 )
      PARAMETER ( BDF_lds_per_rec_daily  =  6 )

