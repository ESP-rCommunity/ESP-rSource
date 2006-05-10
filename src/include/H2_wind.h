C *******************************************************************************
C This file contains common blocks, declarations and assignments related to the
C CETC hydrogen component models developed to support the modelling of
C wind-generated hydrogen-based cogeneration systems.
C *******************************************************************************


C *******************************************************************************
C Declare variables for the user-defined electrical supply generated from the
C wind farm.
C *******************************************************************************
   
      COMMON/H2_WIND_SUPP/IWndSupp_ascii, IWinSupp, Wnd_supp_input,
     &                    Wnd_supp_per_rec, Wnd_supp_per_rec_bin


C -------------------------------------------------------------------------------
C Unit numbers for wind-generated electric supply data files.
C    IWndSupp_ascii     the ASCII file that holds the user input data
C                       (i.e. the wnd file).
C    IWinSupp           the direct access (binary) file that is accessed
C                       during the simulation.
C -------------------------------------------------------------------------------

      INTEGER     IWndSupp_ascii
      INTEGER     IWinSupp

C -------------------------------------------------------------------------------
C Flag indicating the method used for specifying wind-generated electrical
C supply data. Note that only annual data is supported at this time.
C    Wnd_supp_input    variable holding user selection (indicated in .wnd file)
C    Wnd_supp_annual   named constant to indicate there is a record for each
C                      hour of the year. Only this option is supported at present.
C -------------------------------------------------------------------------------

      INTEGER     Wnd_supp_input
      INTEGER     Wnd_supp_annual
      PARAMETER   ( Wnd_supp_annual = 1)

C -------------------------------------------------------------------------------
C Number of data items per record of wind-generated electric supply data file
C (.wnd file). Hourly data is the only format currently supported.
C   Wnd_supp_per_rec        variable holding user-specified number of increments
C                           per record.
C   Wnd_supp_per_rec_annual named constant to indicated number of records of 
C                           electrical supply data. Only hourly data supported
C                           at this time (= 1)
C   Wnd_supp_per_rec_bin    variable holding number of increments per record
C                           in binary file
C --------------------------------------------------------------------------------

      INTEGER   Wnd_supp_per_rec, Wnd_supp_per_rec_bin
      INTEGER   Wnd_supp_per_rec_annual
      PARAMETER ( Wnd_supp_per_rec_annual = 1 )



