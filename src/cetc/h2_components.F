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

C This file contains the following routines related to the
C wind-generated hydrogen component models:

C H2_wind_convert           Checks the .wnd file and creates a binary file
C                           with time-series wind-generated electrical supply.
C H2_wind_establish         Establishes the electric supply from the wind farm
C                           for the current time step.



C *********************************************************************************************
C ******************************* H2_wind_convert *******************************************
C Created by: Maria Mottillo
C Initial Creation Date: August 26, 2004
C Copyright CETC 2004

C The subroutine FC_eloads_convert created by Ian Beausoleil-Morrison has been
C adapted for user-specified wind-generated electric supply data. This subroutine
C checks the integrity of a user-specified electric supply data file and
C produces a binary format file that can be used for direct access during
C the time-step simulation. This is executed once per simulation, prior
C to the time-step calculations. All supply data are
C specified in Watts.

C INPUTS:
C    LWndSupp            name of .wnd file specified by user
C    iWndSupp            flag indicating if .wnd exists (if so, = 1). the flag is not
C                        used in this subroutine.
C    IFIL                unit number of climate file (ESP-r convention is to number all other
C                        file units relative to IFIL)

C OUTPUTS:
C    IWinsupp            unit number of binary file containing time-series of electrical
C                        supply
C -------------------------------------------------------------------------------------------

      SUBROUTINE H2_wind_convert
      IMPLICIT NONE
#include "H2_wind.h"

      COMMON/FILEP/IFIL
      INTEGER  IFIL

      common/H2wind/LWndSupp,iWndSupp
      CHARACTER LWndSupp*72
      INTEGER   iWndSupp

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER  IUOUT
      INTEGER  IUIN
      INTEGER  IEOUT

C---------------------------------------------------------------------------------
C Declare local variables.
C---------------------------------------------------------------------------------
      INTEGER ND,K,num_recs,num_col, irec

      CHARACTER OUTSTR*124

      LOGICAL wnderror, wnd_supp_exist

      REAL elecsupp
      INTEGER ISTAT,IER,I

C---------------------------------------------------------------------------------
C Read the header information from the user-specified wind-generated electrical
C supply data file (.wnd) and perform some checks to ensure that the correct
C number of supply data are specified. Note that this does not check the validity
C of the supply data, but rather ensures that the correct number of data items
C are specified.
C It is assumed that the user specified data file contains hourly data for an
C entire year. At the present time, this subroutine does not allow
C data for one day simulation to be input nor does it allow the input of subhourly
C data for a one-year simulation.
C The existence of the .wnd file was confirmed when the .cfg file was read.
C `wnderror' is a flag indicating whether there were errors reading the .wnd file.
C---------------------------------------------------------------------------------
C-----Notify user that wind-generated electric supply data are
C     being processed (this can take some time).

      call usrmsg(' Processing wind-gen electric data...',' ','-')
C-----Open the .wnd file.
      wnderror = .false.
      IWndSupp_ascii = IFIL+15  ! Should be a safe unit number to use.
      CALL ERPFREE(IWndSupp_ascii,ISTAT)
      CALL EFOPSEQ(IWndSupp_ascii,LWndSupp,1,IER)
      IF(IER /= 0)THEN
        WRITE(IUOUT,*) ' Error opening wind-gen electric  input file.'
        wnderror = .true.
      ENDIF
C-----Read the flag indicating the method used for prescribing the data . 
C-----Note that only hourly data for an entire year is currenly supported and
C-----therefore the flag must be equal to 1.
      CALL STRIPC(IWndSupp_ascii,OUTSTR,0,ND,1,
     &           ' Wind supply data type',IER)
      IF(IER /= 0)THEN
        WRITE(IUOUT,*) 'Error reading Wind elec supply input type.'
        wnderror = .true.
      ENDIF
      K = 0
      CALL EGETWI(OUTSTR,K,Wnd_supp_input,1,1,'F',
     &                   ' Wind supply type ',IER)
      IF(IER /= 0)THEN
         WRITE(IUOUT,*) ' Error reading Wind supply input type.'
         wnderror = .true.
      ENDIF
      IF( Wnd_supp_input /= Wnd_supp_annual)THEN
         WRITE(IUOUT,*) ' Only annual data supported at this time.'
      ENDIF
C-----Read the number of data points specified for each record of data. 
C-----Only hourly data is accepted at this time.
      CALL STRIPC(IWndSupp_ascii,OUTSTR,0,ND,1,
     &           ' Wind supply per hour',IER)
      IF(IER /= 0)THEN
        WRITE(IUOUT,*) 'Error reading number of data items per record'
        wnderror = .true.
      ENDIF
      K = 0
      CALL EGETWI(OUTSTR,K,Wnd_supp_per_rec,1,1,'F',
     &            ' Wind supply per rec',IER)
      IF(IER /= 0)THEN
         WRITE(IUOUT,*) 'Error reading number of data items per record'
         wnderror = .true.
      ENDIF
      IF(Wnd_supp_per_rec /= Wnd_supp_per_rec_annual)THEN
         WRITE(IUOUT,*) ' Sub-hourly data not supported at this time.'
      ENDIF
C-----Check that the correct number of records (8760) of electric supply data are present
          num_recs = 8760
      DO I=1,num_recs
        CALL STRIPC(IWndSupp_ascii,OUTSTR,99,num_col,1,
     &              ' Test read of a record',IER)
        if(IER /= 0)then
          WRITE(IUOUT,*) ' End of wind generated elec supply file.'
          wnderror = .true.
        endif
        if( num_col/= Wnd_supp_per_rec )then
          WRITE(IUOUT,*) ' Incorrect number of data items per record.'
          wnderror = .true.
        endif
      END DO
C-----Error handling on reading of .wnd file.
      IF(wnderror)THEN
        STOP ' Error in wind generated elec supply input file.'
      ENDIF
C-----Reposition .wnd file to the first record containing data.
      REWIND(IWndSupp_ascii)
      DO I = 1,2
         CALL STRIPC(IWndSupp_ascii,OUTSTR,0,ND,1, ' skip header',IER)
      END DO


C---------------------------------------------------------------------------------
C Open a file with direct access (i.e. a binary file) that will be used during
C the time-step simulation to provide the electrical supply from the wind farm. This
C temporary file will be deleted following completion of the time-step simulation.
C---------------------------------------------------------------------------------
      IWinSupp =  IFIL+16
      CALL ERPFREE(IWinsupp,ISTAT)
      INQUIRE (UNIT=IWinSupp,EXIST=wnd_supp_exist) ! check if file exists
      IF (wnd_supp_exist ) then 
         CALL EFDELET(IWinsupp,ISTAT) ! Delete file if it exists
      endif

      CALL FPRAND(IWinSupp,ISTAT,1,3,
     &     'WindGen.tmp')       ! File may exist if prev. simulation was
                                ! interrupted 


C---------------------------------------------------------------------------------
C Iterate through each record of electric supply data. For each record of data in
C .wnd ASCII file, read the value. Write the supply data to the binary file.
C---------------------------------------------------------------------------------

C-----Set number of increments in binary file.  Note at the present time only annual
C-----annual input is permitted.

      Wnd_supp_per_rec_bin = Wnd_supp_per_rec_annual


C-----Iterate through each record in the .wnd ASCII file.
      DO irec=1,num_recs
C-------Read the supply data from the ASCII file.
        READ(IWndSupp_ascii,*) elecsupp


C-------Write the supply data to the binary file.
        WRITE(IWinSupp,REC=irec,IOSTAT=ISTAT)
     &        elecsupp

      END DO

C for debugging



C Processing complete. Leave binary file open for use during time-step simulation.
      call usrmsg(' Processing wind-gen electric data...done',' ','P')

      RETURN

      END


C *********************************************************************************************
C ******************************* H2_wind_establish *****************************************
C Created by: Maria Mottillo
C Initial Creation Date: August 26, 2004
C Copyright CETC 2004

C This subroutine establishes the electrical supply from the wind farm
C for the current time-step. This subroutine reads the data from the binary
C file created by H2_wind_convert.
C This subroutine has been adapted from the FC_eloads_establish subroutine created
C by Ian Beausoleil-Morrison.

C INPUTS:
C    IWinSupp              unit number of binary file containing time-series of electrical
C                          supply data.
C    btimep                `present' time of current day in hour fraction (0. to 24.) for
C                          current building simulation increment (standard ESP-r variable
C                          taken from COMMON/BTIME)
C    idyp                  number of present day (0 to 365) (standard ESP-r variable taken
C                          from COMMON/SIMTIM)
C    ptimef                `future' time of current day in hour fraction (0. to 24.) for
C                          current plant simulation increment (standard ESP-r variable
C                          taken from COMMON/ptime).
C OUTPUTS:
C    QElecT                The electrical power produced by the wind turbines
C                          for the current plant simulation increment (W).
C -------------------------------------------------------------------------------------------

      SUBROUTINE H2_wind_establish(IDYP,BTIMEP,PTIMEF,QElecT)
      IMPLICIT NONE

#include "H2_wind.h"


C for debugging - writing to console
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER  IUOUT
      INTEGER  IUIN
      INTEGER  IEOUT


C---------------------------------------------------------------------------------
C Declare local variables.
C---------------------------------------------------------------------------------
      INTEGER record,after_midnight
      REAL elecsupp(1)
      REAL QElecT
      LOGICAL CLOSE
      INTEGER IDYP,ISTAT
      REAL BTIMEP,PTIMEF

      INTEGER iRecord_adjust

C---------------------------------------------------------------------------------
C Determine which record of the binary file contains the non-HVAC electrical loads data
C for the `future' time that is currently being simulated in the explicit plant
C domain. There is currently only one possible format for the binary file: one record
C for each hour of the year (indicated by `Wnd_supp_annual').
C---------------------------------------------------------------------------------


C---------------------------------------------------------------------------------
C One record for each hour of the year.
C The hour of the year defines the record in the file: the first
C record of the binary file contains the data for 0h00m00s to 0h59m59s of
C January 1; the 8760th record contains the data for 23h00m00s to 23h59m59s of
C December 31.
C Special attention has to be paid when the `future' time in the plant domain
C crosses over midnight, as the `present' day will not have yet been incremented.
C When this occurs, the `present' time in the building domain will be 24 hours
C and the `future' time in the plant domain will be less than or equal to 1 hour.
C The electrical supply at 24h00 is taken as the load at 0h00 of the next day
C (as described above, each record covers the period from 0h00m00s to 23h59m59s).
C Care has to be taken when the simulation crosses over the New Year: the load for
C midnight December 31 is set using the data at 0h00 on January 1.
C---------------------------------------------------------------------------------

C---------Have we just crossed over midnight? 
          CALL ECLOSE(BTIMEP,24.0,0.0001,CLOSE)
          IF( CLOSE .and. PTIMEF<=1. )THEN
            after_midnight = 24
          ELSEIF( BTIMEP<1.0 )THEN  ! IDYP does not increment until BTIMEP=1.0.
            after_midnight = 24
          ELSE
            after_midnight = 0
          END IF

C---------Determine which record to use.
          record = 24*(IDYP-1) + INT(PTIMEF) + 1 + after_midnight
C---------Has midnight on New Year's eve just struck?
          CALL ECLOSE(PTIMEF,24.0,0.0001,CLOSE)
          IF( CLOSE .and. IDYP==365 )THEN
            record = record - 8760
          END IF
          CALL ECLOSE(BTIMEP,24.0,0.0001,CLOSE)
          IF( CLOSE .and. PTIMEF<=1. .and. IDYP==365 )THEN
            record = record - 8760
          END IF
          IF( BTIMEP<1.0 .and. IDYP==365 )THEN ! IDYP does not increment until BTIMEP=1.0.
            record = record - 8760
          END IF
C---------Read the loads for each time increment for the current hour.
          READ(IWinSupp,REC=record,IOSTAT=ISTAT) elecsupp(1)

C---------Set the electrical supply from the wind farm for the current time-step.
C---------Note this value does not take into account the scalar multiplier
          QElecT = elecsupp(1)



      RETURN

      END
