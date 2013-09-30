C This file is part of the ESP-r system.
C Copyright Natural Resources Canada, Government
C of Canada 2004/2005. Please Contact Ian
C Beausoliel-Morrison for details concerning licensing.

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
C
C ====================================================================
C Hydrogen_appliances.F
C Created: July 11, 2005
C
C This file contains routines associated with the hydrogen
C appliance plant component models.
C
C PROCEDURES:
C
C H2_appliance_coeff_gen: Generates matrix coefficients for the
C   hydrogen appliance component model. Note: the hydrogen
C   appliance model presently does not permit any connections
C   in the plant network, so the returned matrix boils down
C   to Ix = 0 for all values of ISTATS. This routien also
C   retrieves the hydrogen appliance load, and calcualates
C   the required hydrogen flow rate.
C
C convert_lds_to_bin_file: Collects hourly loads from an ascii
C   file and converts them to binary format.
C
C recover_lds_from_bin_file: Recovers loads from binary temp
C   file at runtime
C
C*************************************H2_appliance_coeff_gen*************************************
C
C This subroutine generates the coefficients needed for plant simulation for a Hydrogen appliance.
C This component represents an appliance that burns Hydrogen. At this point the component does
C not connect to other components under the plant network. The H2 consumption is based on a load
C profile read from a data file.
C
C Input:
C IPCOMP: Number for the appliance plant component in the plant network
C ISTATS: Flag indicating which matrix is being solved
C          1 -> temperature
C          2 -> 1st phase flow
C          3 -> 2nd phase flow
C          4 -> hydrogen flow         1
C
C Output:
C COUT:   Array holding matrix coefficients for component
C
C--------------------------------------------------------------------------------------------------
      subroutine H2_appliance_coeff_gen( iPComp, COut, iStats )
      use h3kmodule
      implicit none

#include "plant.h"
#include "chemical_properties.h"
#include "Hydrogen_demand_controller.h"

C External functions.
      integer lnblnk

C Passed arguements
      integer iPComp        ! pointer to component
      integer iStats        ! flag indicating which matrix is being solved
                            ! istats = 1 -> temperature
                            ! istats = 2 -> 1st phase flow
                            ! istats = 3 -> 2nd phase flow
                            ! istats = 4 -> hydrogen flow
      real COut(mpcoe)      ! array of matrix coefficients for
                            ! given component

C-------------------------------------------------------------------------------
C     ESP-r variables
C-------------------------------------------------------------------------------
C.....I/O channes
      integer iuout,iuin
      common/outin/iuout,iuin
C.....Plant component parameters
      real adata(mpcom,madata),bdata(mpcom,mbdata)
      common/pdbdt/adata,bdata
C.....Plant component name
      common/pcnam/pcname(mpcom)        ! Plant component names
      character*15, pcname

C-------------------------------------------------------------------------------
C     Local variables
C-------------------------------------------------------------------------------
      integer iCol_index     ! Index of column containing hydrogen appliance load

      integer iInterp_Method ! Flag specifying boundary condition
                             ! interpolation method

      real    fApp_Scale_Factor    ! Scale factor to be applied to appliance electric
                                   ! load read in data file

      real fApp_Efficiency   ! Appliance effective efficiency (-)

      real fDay              ! Current day, and fraction thereof (days)

      logical bInitialized          ! Flag for initialization
      data bInitialized /.false./   ! Note: initialization applies to procdure,
                                    !   and is not done component-by-component
      save bInitialized

      integer iCount

      real fH2_HHV            ! Higher heating value of hydrogen (J/kg)
      save fH2_HHV

      real fH2_mass_flow      ! hydrogen flow rate (kg/s)

      real fH2_app_load       ! Appliance heating load (W)

      logical bClose          ! Result of close-to-zero comparison

      character*248 cName_buffer ! Buffer passed to BC data facility

      integer hold_len           !Stores the lenght of the comp pass to reporting

C.....Chemical composition buffer
      real fComposition ( iCompound_count )

C-------------------------------------------------------------------------------
C     Named constants
C-------------------------------------------------------------------------------
      INTEGER iPropTemp       ! named constant for temperature properity
      INTEGER iProp1stFlow    ! named constant for first phase flow properity
      INTEGER iProp2ndFlow    ! named constant for second phase flow properity
      INTEGER iPropH2Flow     ! named constant for hydrogen flow
      PARAMETER ( iPropTemp     = 1,
     &            iProp1stFlow = 2,
     &            iProp2ndFlow = 3,
     &            iPropH2Flow  = 4 )

C-------------------------------------------------------------------------------
C     References
C-------------------------------------------------------------------------------
      real fConvert_current_TS_to_DAY    ! Function returing current day
      real fEval_Mixture_HV         ! Function to evaluate a mixture's
                                    !   heating value
      real fEval_Mixture_MM         ! Function to evaluate a mixture's
                                    !   molar mass
      real fGet_BC_data_by_name     ! Function retrieving data from
                                    !   boundary condition routine



      if ( .not. bInitialized ) then
C-------------------------------------------------------------------------------
C        Calculate the higher heating value of hydrogen under stp conditions
C-------------------------------------------------------------------------------
         do iCount = 1, iCompound_Count
            fComposition ( iCompound_Count ) = 0.0
         enddo
         fComposition ( iHydrogen ) = 1.0
C--------------------------------------------------------------------
C        Evaluate fuel lower heating value.
C         -> function fEval_Mixture_HV returns heating value (J/kmol)
C         -> function fEval_Mixture_MM returns molar mass (kg/kmol)
C
C        Note: fEval_Mixture_HV can return both the higher heating
C              value and lower heating value. Use iLHV to specify
C              lower heating value.
C--------------------------------------------------------------------
         fH2_HHV =   fEval_Mixture_HV ( fComposition, iHHV )
     &             / fEval_Mixture_MM ( fComposition )       ! J/kg

         bInitialized = .true.

      endif

C-------------------------------------------------------------------------------
C     Write out contextual buffer
C-------------------------------------------------------------------------------
      write (cMessage, '(A,I2,A)')
     & 'Collecting appliance hydrogen demand for plant component ',
     & iPComp,' from boundary conditions.'

C-------------------------------------------------------------------------------
C     Write out component name
C-------------------------------------------------------------------------------
      write (cName_buffer, '(A)') pcname (iPComp )

C-------------------------------------------------------------------------------
C     Get inputs
C-------------------------------------------------------------------------------
      iInterp_Method        = int ( ADATA(iPComp, 1) )
      fApp_Efficiency       = ADATA(iPComp, 2)
      fApp_Scale_Factor     = ADATA(iPComp, 3)


C-------------------------------------------------------------------------------
C     Get current day, or fraction thereof
C-------------------------------------------------------------------------------
      fDay = fConvert_current_TS_to_DAY()

C-------------------------------------------------------------------------------
C     Get appliance heating load
C-------------------------------------------------------------------------------
      fH2_app_load = fGet_BC_data_by_name(
     &                                     cName_buffer,
     &                                     fDay,
     &                                     iInterp_method,
     &                                     cMessage       )

C.....Apply scale factor

      fH2_app_load = fH2_app_load * fApp_Scale_Factor


C-------------------------------------------------------------------------------
C     Check if efficiency is not zero. If not, calculate hydrogen flow rate
C-------------------------------------------------------------------------------
      call eclose ( fApp_Efficiency, 0.0, 0.01, bClose )

      if ( .not. bClose ) then
C---------------------------------------------------------------------------------
C        Hydrogen flow rate:
C
C                         appliance load
C         H2 flow = -----------------------------
C                   H2 HHV * appliance efficiency
C
C---------------------------------------------------------------------------------
         fH2_mass_flow = fH2_app_load / ( fH2_HHV * fApp_Efficiency )  ! kg/s

      else
         call edisp ( iUout, cMessage )
         call edisp ( iUout,
     &                'Hydrogen appliance efficieny cannot be zero!' )
         stop 'H2_appliance_coeff_gen(): Fatal error!'
      endif

C-------------------------------------------------------------------------------
C     Assign hydrogen appliance load and required hydrogen to commons
C-------------------------------------------------------------------------------
      fH2_plt_app_load ( iPComp )   = fH2_app_load            ! (W)
      fH2_plt_IMP_demand ( iPComp ) = fH2_mass_flow           ! (kg/s)

C-------------------------------------------------------------------------------
C     Check value of ISTATS, and set coeffArray accordingly:
C
C     .   State eqn:  COUT(1) * T_(i) + COUT(2) * T_(i-1) = COUT(3)
C
C     Note: in it's present impletation, the H2 appliance is inert ---
C     it has no incomming or outgoing connections.  The state variables
C     (COUT(1)) are set to zero for all values of istats.
C
C-------------------------------------------------------------------------------

      IF ( istats .EQ. iPropTemp ) THEN

C-------------------------------------------------------------------------------
C     .  Temperature flow: set equal to inlet conditions
C-------------------------------------------------------------------------------

         COUT(1) = 1.0
         COUT(2) = 0.0
         COUT(3) = 0.0

      ELSEIF ( istats .EQ. iProp1stFlow ) THEN

C-------------------------------------------------------------------------------
C     .  1st phase flow: no flow
C-------------------------------------------------------------------------------

         COUT(1) = 1.0
         COUT(2) = 0.0
         COUT(3) = 0.0

      ELSEIF ( istats .EQ. iProp2ndFlow ) THEN

C-------------------------------------------------------------------------------
C     .  2nd phase flow: no flow
C-------------------------------------------------------------------------------

         COUT(1) = 1.0
         COUT(2) = 0.0
         COUT(3) = 0.0

      ELSEIF ( istats .EQ. iPropH2Flow ) THEN

C-------------------------------------------------------------------------------
C     .  H2 flow: no flow
C-------------------------------------------------------------------------------

        COUT(1) = 1.0
        COUT(2) = 0.0
        COUT(3) = 0.0

      ENDIF

C----------------------------------------------------------------------------------------
C XML output
C----------------------------------------------------------------------------------------

C.....Get component name's lenght
      hold_len = lnblnk(pcname(IPCOMP))
C.....Push values to reporting engine
       call AddToReport(rvPltH2AppLoad%Identifier,
     &         fH2_app_load,
     &         pcname(IPCOMP)(1:hold_len))

      call AddToReport(rvPltH2AppDemand%Identifier,
     &         fH2_mass_flow,
     &         pcname(IPCOMP)(1:hold_len))

      RETURN
      END

C************************************************************************************************

C*************************** convert_lds_to_bin_file ********************************************
C
C Subroutine is called to read an ascii hourly load file and convert it to a binary file
C for random access.
C
C Inputs:
C   load_file_name: Name of ascii file to be converted
C   load_file_num:  binary file access number
C
C Output:
C   None.
C
C--------------------------------------------------------------------------------------------------
      SUBROUTINE convert_lds_to_bin_file( load_file_name,
     &                                    load_file_num,
     &                                    bError)

      IMPLICIT NONE

#include "plant.h"

C External functions.
      integer lnblnk

C Passed arguements
      INTEGER load_file_num             ! Unit number for binary file
      character*72 load_file_name       ! Ascii file name

C-------------------------------------------------------------------------------
C     ESP-r commons
C-------------------------------------------------------------------------------
      COMMON/FILEP/IFIL                 ! Base ESP-r file unit number
      INTEGER IFIL

      COMMON/OUTIN/IUOUT,IUIN           ! Number for default output and input screens
      INTEGER IUOUT,IUIN

C-------------------------------------------------------------------------------
C     Local variables and common blocks
C-------------------------------------------------------------------------------

      INTEGER app_load_method           ! Method used to specify appliance load profile
      INTEGER app_num_dataperhour       ! Number of data items per hour used in the
                                        ! appliance load profile data file
      INTEGER K,ND                      ! Variables used to read a string of variables in load
                                        ! profile data file
      INTEGER nonblank_name_length      ! Length of non blank part of the name
                                        ! of the appliance plant component
      INTEGER ISTAT,IER                 ! Flags used in the opening of ascii file
      INTEGER num_recs                  ! Total number of records in the appliance data file
      INTEGER I                         ! Do loop counter
      INTEGER num_col                   ! Number of columns in each of the records in the load
                                        ! profile data file

      CHARACTER OUTSTR*124              ! Data record from load profile data file
      LOGICAL bError                  ! Flag indicating if there were any problems opening
                                        ! data file and reading data from file
      LOGICAL load_file_exists          ! Flag to check whether binary load file
                                        ! exits
      REAL app_load_rec(60)             ! appliance load data held per record in the load
                                        ! data file. The assumption here is that the max
                                        ! number of data points per hour  can not exceed 60.
                                        ! This can be changed in the future to any number
                                        ! desired
      INTEGER inc                       ! loop  counter
      INTEGER II                        ! loop counter
      INTEGER record_num_error          ! Record number in data file with data input problem
      INTEGER next_plant_comp_num       ! Next plant componenent number in the plant network

      integer ascii_file_num            ! ascii file number


      INTEGER iNumber_of_columns        ! Number of columns in binary file
C-------------------------------------------------------------------------------
C     Named constants
C-------------------------------------------------------------------------------
      INTEGER app_load_single           ! Flag for the case when there is only one profile
                                        ! specified for the whole year
      INTEGER app_load_double           ! Flag for when there is one weekday and one weekend
                                        ! profile specified for the whole year
      INTEGER app_load_twelve           ! Flag for when there are 12 profiles specified for
                                        ! the whole year (1 for each month)
      INTEGER app_load_twenty_four      ! Flag for when there are 24 profiles specified
                                        ! for the whole year (12 for weekday for each
                                        ! month and 12 for weekend for each month)
      PARAMETER(app_load_single = 1,
     &          app_load_double = 2,
     &          app_load_twelve = 12,
     &          app_load_twenty_four = 24)


C-----This subroutine will create a temporary binary file for the load profile data
C-----for the appliance. The binary file generated from the ascii file will use the
C-----name 'Hrly_Load.tmp'

C-----Find the number of non-blank characters in the assigned file name
      nonblank_name_length = lnblnk(load_file_name)

C-----Assign unit number for load profile data file. Use ifil+1 for temporarly-opened
C-----files.
      ascii_file_num = IFIL+1

C-----Notify user that we're processing the hourly file
      call edisp(iuout,'  ->Processing hourly profile ' //
     &                   load_file_name(1:nonblank_name_length) )

      bError = .false.

C-----Open load profile data file
      CALL ERPFREE(ascii_file_num,ISTAT)
      CALL EFOPSEQ(ascii_file_num,
     &             load_file_name(1:nonblank_name_length),1,IER)

C-----Check if there was a problem opening the data file
      IF(IER /= 0)THEN
        WRITE(IUOUT,*) '  ->Error opening appliance input file.'
        bError = .true.
      ENDIF

C-----Read the first row of data in data file
        CALL STRIPC(ascii_file_num,OUTSTR,0,ND,1,
     &              ' Appl lds data type',IER)

C-----Check if there was a problem reading file
      IF(IER /= 0)THEN
        WRITE(IUOUT,*) 'Error reading appliance loads input data.'
        bError = .true.
      ENDIF

C-----Read flag indicating the method used to describe load profile
C-----data
      K=0
      CALL EGETWI(OUTSTR,K,app_load_method,1,24,'F',
     &            ' Appl loads method ',IER)

C-----Check of there was a problem reading the flag
      IF(IER /= 0)THEN
        WRITE(IUOUT,*) 'Error reading appliance loads input data.'
        bError = .true.
      ENDIF

C-----Check that the value of the flag for the load profile data
C-----specification method is ok. If the value is not acceptable,
C-----then issue message to user
      IF((app_load_method.NE.app_load_single).AND.
     &   (app_load_method.NE.app_load_double).AND.
     &   (app_load_method.NE.app_load_twelve).AND.
     &   (app_load_method.NE.app_load_twenty_four)) THEN

      WRITE(IUOUT,*) 'Incorrect total number of load profiles for'
      WRITE(IUOUT,*) 'the year'
      WRITE(IUOUT,*) 'Valid options are:'
      WRITE(IUOUT,*) '1  for a single average profile for the whole'
      WRITE(IUOUT,*) '   year'
      WRITE(IUOUT,*) '2  for an average annual profile for a week'
      WRITE(IUOUT,*) '   day and an average annual profile for a'
      WRITE(IUOUT,*) '   weekend'
      WRITE(IUOUT,*) '12 for an average profile for each of the months'
      WRITE(IUOUT,*) '   of the year'
      WRITE(IUOUT,*) '24 for an average week day and weekend profile'
      WRITE(IUOUT,*) '   for each of the months of the year'

          bError = .true.
      ENDIF

C-----Read number of data points per hour
      CALL EGETWI(OUTSTR,K,app_num_dataperhour,1,60,'F',
     &              ' Appl loads num of data per hour ',IER)

C-----Check if there was a problem reading number of data points
C-----per hour
      IF(IER /= 0) THEN
        WRITE(IUOUT,*) 'Error reading appliance loads input data.'
        bError = .true.
      ENDIF

C-----Check if namber of data points per hour is larger than 60.
C-----If it is, then issue message to the user
      IF(app_num_dataperhour.GT.60) THEN
        WRITE(IUOUT,*) 'Maximum number of appliance load data '
        WRITE(IUOUT,*) 'points per hour can not be larger than 60'
      ENDIF

C-----Find the total number of records in the data file based on
C-----specification method used
      IF(app_load_method.EQ.app_load_single) THEN
        num_recs = 24
      ELSEIF(app_load_method.EQ.app_load_double) THEN
        num_recs = 48
      ELSEIF(app_load_method.EQ.app_load_twelve) THEN
        num_recs = 288
      ELSEIF(app_load_method.EQ.app_load_twenty_four) THEN
        num_recs = 576
      ENDIF

C-----Read all the records in the data file and make sure that
C-----they are all present. If not then issue message to the
C-----user
      DO 10 I = 1,num_recs
        CALL STRIPC(ascii_file_num,OUTSTR,99,num_col,1,
     &              ' Test read of a record',IER)

        IF(IER /= 0) THEN
          WRITE(IUOUT,*) 'End of appliance loads file.'
          bError = .true.
        ENDIF

C-------Record number in data file with input error
        record_num_error = I + 1
        IF( num_col/=app_num_dataperhour) THEN
          WRITE(IUOUT,*) 'Incorrect number of data items '
          WRITE(IUOUT,*) 'per record in appliance load data'
          WRITE(IUOUT,*) ' file.'
          WRITE(IUOUT,*) 'Executation stopped at record number: ',
     &    record_num_error, ' of the load profile input file',
     &    load_file_name(1:lnblnk(load_file_name))
          bError = .true.
        ENDIF

 10   CONTINUE

C-----Error handling on reading of applinace load profile file.
      IF(bError)THEN
        STOP 'Error in appliance loads input file.'
      ENDIF

C-----Reposition load profile file to the first record containing
C-----loads data.
      REWIND(ascii_file_num)
      CALL STRIPC(ascii_file_num,OUTSTR,0,ND,1,
     &            ' skip header',IER)

C---------------------------------------------------------------------------------
C Open a file with direct access (i.e. a binary file) that will be used during
C the time-step simulation to set the hydrogen load on the appliance. This
C temporary file will be deleted following completion of the time-step simulation.
C---------------------------------------------------------------------------------

      CALL ERPFREE(load_file_num,ISTAT)
      INQUIRE (UNIT=load_file_num,EXIST=load_file_exists) ! check if file exists

C-----If the binary load file exits, then delete file
      IF (load_file_exists) THEN
         CALL EFDELET(load_file_num,ISTAT) ! Delete file if it exists
      ENDIF

C-----Open binary file with the right number of columns read earlier from the ascii
C-----load profile data file
      if ( app_num_dataperhour .lt. 2 ) then
        iNumber_of_columns = 2
      else
        iNumber_of_columns = app_num_dataperhour
      endif
      CALL FPRAND(load_file_num,ISTAT,iNumber_of_columns,3,
     &            'Hrly_Load.tmp')  ! File may exist

C-----Write the load profile specification method flag and the number of
C-----data points per hour to the binary file
      WRITE(load_file_num,REC=1,IOSTAT=ISTAT)
     &      app_load_method, app_num_dataperhour
C---------------------------------------------------------------------------------
C Iterate through each record of appliance loads data. For each record of data in
C ASCII file, read the load at each time increment specified and write data to the
C binary file
C---------------------------------------------------------------------------------

C-----Iterate through each record in the appliance load data file.
      DO I = 2,num_recs+1
C-------Read the loads data from the ASCII file.
        READ(ascii_file_num,*) (app_load_rec(inc),
     &                             inc=1,app_num_dataperhour)
C-------Write the loads data to the binary file.
        WRITE(load_file_num,REC=I,IOSTAT=ISTAT)
     &       (app_load_rec(inc),inc=1,app_num_dataperhour)
      END DO

C Processing complete. Close ascii file, and leave binary file
C open for later access

      call  ERPFREE(ascii_file_num,ISTAT)


      RETURN
      END

C************************************ recover_lds_from_bin_file ****************************************
C
C This subroutine returns the appropriate load for a given hour/day/year according
C to the supplied profile.
C
C Input:
C
C   load_file_num: IO channel for binary load file (already open)
C   fHour: hour of the day
C   iDay_of_Year: day of the year
C   iYear: year

C
C Output:
C
C   requested load profile value  (W)
C
C--------------------------------------------------------------------------------------------------

      real function fRecover_lds_from_bin_file(load_file_num,
     &                  fHour,iDay_of_Year,iYear)

      IMPLICIT NONE

#include "plant.h"

C-------------------------------------------------------------------------------
C     Passed variables
C-------------------------------------------------------------------------------
      integer load_file_num                ! IO channel to binary load file
      real fHour                           ! Hour of the day for which data is
                                           ! requested (0.0->24.0)
      integer iDay_of_Year                 ! Day of year for which data is
                                           ! requested (1->365)
      integer iYear                        ! year for which data is requested


C-------------------------------------------------------------------------------
C     ESP-r commons
C-------------------------------------------------------------------------------
      COMMON/PCTINC/IPTS
      INTEGER IPTS                         ! Plant domain current time step within
                                           ! the building domain



C-------------------------------------------------------------------------------
C     Local variables and common blocks
C-------------------------------------------------------------------------------

      INTEGER record_num                   ! Record number in the binary load profile file
                                           ! corresponding to requested time row.

      REAL app_load_rec(60)                ! Array for the appliance load profile data for the hour
                                           ! A maximum of 60 data points per hour is allowed at this time

      INTEGER inc                          ! Counter


      INTEGER ISTAT                        ! Status of read attepts to binary file

      INTEGER app_load_method              ! Method used to specify appliance load profile

      INTEGER app_num_dataperhour          ! Number of data items per hour used in the
                                           ! appliance load profile data file. This number
                                           ! can not exceed 60 at this point

      INTEGER app_load_col_num             ! Number associated with the position of the time
                                           ! step within the hour corresponding the future
                                           ! plant domain time step

      INTEGER day_type                     ! Flag to indicate day of the week (1 for Monday, 2 for Tuesday,
                                           ! 3 for Wednesday, 4 for Thursday, 5 for Friday,6 for Saturday,
                                           ! 7 for Sunday)

      integer hour_index                   ! Current hour
      real hour_fraction                   ! Fraction of current hour that has lapsed


      INTEGER month_index                  ! Index to keep track of the month of the year (varies from 1 to 12)

      INTEGER dayofmonth_index             ! Index to keep track of day number for the month

      INTEGER last_day_ofmonth(12)         ! List of last day numbers for the month
      DATA last_day_ofmonth/31,28,31,30,31,30,31,31,30,31,30,31/
                                           ! List of the numbers for the last days of the month
      REAL frac_hr                         ! Fraction of the hour past the beginning of the hour

      logical bClose                       ! Flag for numerical comparisons

C-------------------------------------------------------------------------------
C     Named constants
C-------------------------------------------------------------------------------
      INTEGER app_load_single              ! Flag for the case when there is only one profile
                                           ! specified for the whole year
      INTEGER app_load_double              ! Flag for when there is one weekday and one weekend
                                           ! profile specified for the whole year
      INTEGER app_load_twelve              ! Flag for when there are 12 profiles specified for
                                           ! the whole year (1 for each month)
      INTEGER app_load_twenty_four         ! Flag for when there are 24 profiles specified
                                           ! for the whole year (12 for weekday for each
                                           ! month and 12 for weekend for each month)

      PARAMETER(app_load_single = 1,
     &          app_load_double = 2,
     &          app_load_twelve = 12,
     &          app_load_twenty_four = 24)


C-----Convert passed date into DD/MM/YY, and determine year type
      call eDayR(iDay_of_Year,dayofmonth_index,month_index)
      call eWeekD(dayofmonth_index,month_index,iYear,day_type)
C-----Determine current hour and fraction of hour remaining.
      hour_index = int ( fHour ) + 1
      hour_fraction = fHour - float ( hour_index - 1  )
C-----Check for error introduced by machine precision, and round if necessary
C-----use 1/10th of 1 minute as precision threshold.
      call eClose ( hour_fraction, 1.0, 0.1/60.0, bClose )
      if ( bClose ) then
C--------Round hour up.
         hour_index = hour_index + 1
         hour_fraction = 0.0
      endif

C-----The 1st row of the load profile data file holds the load profile specification
C-----method indicator and the number of data points per hour contained in the file
      READ(load_file_num,REC=1,IOSTAT=ISTAT)
     &     app_load_method,app_num_dataperhour

C-----Set the appropriate record number, in the binary load profile data file, for the load
C-----profile data for the future plant domain time step
C-----When the load profile data file contains only one average profile for the whole year,
C-----then the future hour of the building domain points to the proper hour in the binary
C-----data file. hour_index is augmented by 1 because the first record of the data file holds the
C-----2 integers read previously.
      IF(app_load_method.EQ.app_load_single) THEN
        record_num = 1 + hour_index

C-----If the load profile specification method is based on specifying two profiles: an average
C-----profile for weekdays and another average profile for the weekends:
      ELSEIF(app_load_method.EQ.app_load_double) THEN
C-------If the day type for the future day in plant domain is a weekday, then record number
C-------in the load profile data file is hour_index + 1
        IF((day_type.GE.1).AND.(day_type.LE.5)) THEN
          record_num = 1 + hour_index
C-------If the day type for the future day in plant domain is a weekend, then the record
C-------number in the load profile file is the same as that for a weekday + 24
        ELSEIF((day_type.EQ.6).OR.(day_type.EQ.7)) THEN
          record_num = 1 + hour_index + 24
        ENDIF
C-----For the case when the load profile file has an average profile for each of the months
      ELSEIF(app_load_method.EQ.app_load_twelve) THEN
C-------The record number for the future time in the plant domain
        record_num = 1 + (month_index - 1) * 24 + hour_index

C-----In the case that the load profile specification method is based on giving two average
C-----profiles for each month: one for the weekday and one for the weekend
      ELSEIF(app_load_method.EQ.app_load_twenty_four ) THEN
C-------If the day type for the future day in plant domain is a weekday, then record number
C-------in the load profile data file is
        IF((day_type.GE.1).AND.(day_type.LE.5)) THEN
          record_num = 1 + (month_index - 1) * 48 + hour_index
C-------If the day type for the future day in plant domain is a weekend, then the record
C-------number in the load profile file is
        ELSEIF((day_type.EQ.6).OR.(day_type.EQ.7)) THEN
          record_num = 1 + 24 + (month_index - 1) * 48 +  hour_index
        ENDIF
      ENDIF

C-----Read the loads for each time increment for the current hour.
      READ(load_file_num,REC=record_num,IOSTAT=ISTAT)
     &    (app_load_rec(inc),inc=1,app_num_dataperhour)

C-----Now determine which of these increments corresponds to the current
C-----time-step.
      app_load_col_num = INT( hour_fraction * app_num_dataperhour +
     &                        0.001 ) + 1 ! 0.001 prevents rounding error.

C-----Return requested load (W)
      fRecover_lds_from_bin_file = app_load_rec(app_load_col_num)

      RETURN
      END
C**********************************************************************************
