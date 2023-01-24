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


C Date: April 6, 2009
C Authors: Maria Mottillo & Alex Ferguson, Natural Resources Canada

C This file contains the following subroutines:
C Find_season_parameters: This subroutine loops through the year and finds
C a week in each season that best represents the climate in that season. The
C year is split into 4 seasons: spring, summer, fall and winter. The best fit
C week is found by comparing the average HDD/day, average CDD/day and/or average
C global horizontal irradiance/day for the week and the corresponding average
C for the season - the week with the closest parameter compared to the seasonal
C parameter is deemed the best week. Based on a comparison of HDDs and CDDs for
C the season, the season is deemed as heating only, cooling only or as having both heating and cooling,
C which affects whether HDD and/or CDD or both, are used as comparison parameters.
C The best week is the best 7-day period within the season and a 7-day period starting at each
C day in the year is investigated (this differs from the IPV methodology).
C This subroutine also calculates the heating and cooling multipliers to
C apply to the energy flux results for the best week. These multipliers are used to
C estimate the energy fluxes for the season.


C It is hoped that in future work, this methodology can be combined with the IPV
C methodology.


C*******
      subroutine Find_season_parameters()


      implicit none
#include "building.h"
#include "esprdbfile.h"
#include "qrun.h"

C Index of climate file.
      common/filep/ifil
      integer ifil

C Arrays contain hourly climate data for each day of the year.
      integer idif(365,24),itmp(365,24),idnr(365,24),ivel(365,24),
     &        idir(365,24),ihum(365,24)



      integer iStat     ! I/O flag.
      integer iClmErr, iRec

      integer iyear,idngh
      real clat, clong
      character*42 clmloc

      integer iSeasonStartDay(iMaxSeasons)   ! start day for each season defined in the year
      integer iSeasonEndDay(iMaxSeasons)     ! end day for each season defined in the year

      integer iCurrSeason, iCurrDay, iCurrWeekEnd     ! season, day, and week counters
      integer iDayofWeek, iHour, iDayofYear, iDay, j  ! counters and array indices
      integer iLengthSeason

      integer iCandidateWeekStart(365) ! array holding day number corresponding to start of week
      integer iCandidateWeekEnd(365)   ! array holding day number corresponding to end of week
      integer iBestWeekIndex           ! start day number corresponding to the best week found in the season
      integer iCandidateWeekIndex      ! index used to loop through all the weeks in the season (to find best week)
      integer iNumberofWeeks           ! number of weeks in the season investigated to find best week


      real fCandidateWeekHDD(365)        ! average HDD per day calculated for the week
      real fCandidateWeekCDD(365)        ! average CDD per day calculated for the week
      real fCandidateWeekSolar(365)

      real fHDDBaseTemp       ! Base temperature used for heating degree day calculation
      parameter ( fHDDBaseTemp = 18. )
      real fCDDBaseTemp       ! Base temperature used for cooling degree day calculation
      parameter ( fCDDBaseTemp = 18. )

      real fSeasonHDDTotal        ! total HDD for the season, deg-day-hrs
      real fAvgSeasonalHDDperDay  ! average HDD per day for the season
      real fHDDCurrHour           ! HDD of current hour
      real fWeekHDDTotal          ! total HDD for the week, deg-day-hrs
      real fAvgWeekHDDperDay      ! average HDD per day for the week
      real fCurrHourTemp          ! hourly temperature from the climate file, deg

      real fSeasonCDDTotal        ! total CDD for the season, deg-day-hrs
      real fAvgSeasonalCDDperDay  ! average CDD per day for the season
      real fCDDCurrHour           ! CDD of current hour
      real fWeekCDDTotal          ! total CDD for the week, deg-day-hrs
      real fAvgWeekCDDperDay      ! average HDD per day for the week

      real fCurrHDDDiff   ! difference in weekly and seasonal average HDD/day for current week
      real fBestHDDDiff   ! difference in weekly and seasonal average HDD/day for best week
      real fCurrCDDDiff   ! difference in weekly and seasonal average CDD/day for current week
      real fBestCDDDiff   ! difference in weekly and seasonal average CDD/day for best week
      real fCurrSolarDiff ! difference in weekly and seasonal average global horizontal irradiance for current week
      real fBestSolarDiff ! difference in weekly and seasonal average global horizontal irradiance for best week

      real fTotalCurrDiff  ! total difference between weekly and seasonal averages for current week
      real fTotalBestDiff  ! total difference between weekly and seasonal averages for best week

      logical bHeating    ! boolean used to identify heating season
      logical bCooling    ! boolean used to identify cooling season
      logical bClose      ! boolean used to compare two real numbers

      real fHtgWeighting  ! weighting factor applied to HDD comparison (differs for each season)
      real fClgWeighting  ! weighing factor applied to CDD comparison (differs for each season)
      real fSlrWeighting  ! weighting factor applied to comparison of solar (global horizontal irradiance)
      parameter ( fSlrWeighting = 1. )

      real fSolarAltitudeTemp(24), fSolarAzimuth(24)     ! arrays holding solar altitude and solar azimuth (temporary variables)
      real fSolarAltitude(365,24)                        ! array holds solar altitude for each hour of each day in the year (calculated from climate data)
      real fCurrDiffuseSolar              ! diffuse solar radiation of current hour, W/m2
      real fCurrDirSolar                  ! solar radiation data of current hour (W/m2), can be direct normal or global horizontal depending on idngh flag
      real fCurrDirHoriz                  ! direct horizontal radiation of current hour, W/m2
      real fSeasonGlobalHoriz             ! total seasonal global horizontal radiation, W/m2
      real fWeekGlobalHoriz               ! total weekly global horizontal radiation, W/m2
      real esind                  ! fortran sine function for variables in degrees
      real fAvgSeasonalGlobHorperDay      ! average global horizontal radiation per day, calculated for season
      real fAvgWeekGlobHorperDay          ! average global horizontal radiation per day, calculated for week


      character*72 cSeasonName(iMaxSeasons)    ! season names



      cSeasonName(1) = "Spring"
      cSeasonName(2) = "Summer"
      cSeasonName(3) = "Fall"
      cSeasonName(4) = "Winter"

C Season start and end days correspond to the day numbers in the year
      iSeasonStartDay(1) = 60
      iSeasonEndDay(1)   = 120
      iSeasonStartDay(2) = 121
      iSeasonEndDay(2)   = 240
      iSeasonStartDay(3) = 241
      iSeasonEndDay(3)   = 300
      iSeasonStartDay(4) = 301
      iSeasonEndDay(4)   = 59

C testing (begin)
C      iSeasonStartDay(1) = 45
C      iSeasonEndDay(1)   = 135
C      iSeasonStartDay(2) = 136
C      iSeasonEndDay(2)   = 226
C      iSeasonStartDay(3) = 227
C      iSeasonEndDay(3)   = 317
C      iSeasonStartDay(4) = 318
C      iSeasonEndDay(4)   = 44
C testing (end)



C Open climate file
      call EFOPRAN(ifil,LCLIM,144,0,IClmErr)

C read climate file data
      irec=366
      read(ifil,rec=irec,iostat=istat,err=1000)
     &     iyear,idngh
      if ( iyear .eq. 0 ) goto 1000
      irec = 367
      read(ifil,rec=irec,iostat=istat,err=1000)
     &     clmloc
      irec = 368
      read(ifil,rec=irec,iostat=istat,err=1000)
     &     clat, clong


C Populate arrays with climate data for the entire year
      do iDay  = 1,365
        read(ifil,rec=iDay,iostat=istat,err=1000)
     &      (idif(iDay,j),itmp(iDay,j),idnr(iDay,j),
     &       ivel(iDay,j),idir(iDay,j),ihum(iDay,j),j=1,24)
C calculate solar altitude
        call azalt(iDay,1,clong,clat,fSolarAltitudeTemp,fSolarAzimuth)

        do j = 1, 24
           fSolarAltitude(iDay,j) = fSolarAltitudeTemp(j)
        enddo

      enddo


C Loop through each season, and pick best week.
      season_loop: do iCurrSeason = 1, iNumberOfSeasons
C Determine the number of days in each season
        if ( iSeasonEndDay(iCurrSeason) >
     &           iSeasonStartDay(iCurrSeason) ) then
          iLengthSeason  = iSeasonEndDay(iCurrSeason) -
     &                  iSeasonStartDay(iCurrSeason)
        else
C season loops through to the next year
          iLengthSeason = iSeasonEndDay(iCurrSeason) +
     &               365 - iSeasonStartDay(iCurrSeason)
        endif

        fSeasonHDDTotal = 0.     ! initialize variable used to sum up HDD in season
        fSeasonCDDTotal = 0.     ! initialize variable used to sum up CDD in season
        fSeasonGlobalHoriz = 0.  ! initialize variable used to sum up global horizontal irradiance in season

C This loop determines the number of heating and cooling days for each season.
        seasonal_day_loop: do iDay = 1, iLengthSeason

          iCurrDay = iSeasonStartDay(iCurrSeason) + ( iDay - 1)
          if ( iCurrDay > 365 ) iCurrDay = iCurrDay - 365
C This loop computes heating and cooling degree days, and solar irradiance
C for each hour in the current day. It appends these data to the running
C totals for the entire season.
          seasonal_hr_loop: do iHour = 1, 24           ! add up hourly HDDs for the season

            fCurrHourTemp = real(itmp(iCurrDay,iHour))/10.    ! divide by 10 because temperatures in weather file are in units of (tenth deg)

            if  ( fCurrHourTemp < fHDDBaseTemp ) then
                   fHDDCurrHour = fHDDBaseTemp - fCurrHourTemp
            else
                   fHDDCurrHour = 0.
            endif

            if ( fCurrHourTemp > fCDDBaseTemp ) then
                   fCDDCurrHour = fCurrHourTemp - fCDDBaseTemp
            else
                   fCDDCurrHour = 0.
            endif

            fSeasonHDDTotal = fSeasonHDDTotal + fHDDCurrHour  ! total HDD-hr for season
            fSeasonCDDTotal = fSeasonCDDTotal + fCDDCurrHour  ! total CDD-hr for season


            fCurrDiffuseSolar = real(idif(iCurrDay,iHour))
            fCurrDirSolar = real(idnr(iCurrDay,iHour))

C Check if fCurrDirSolar holds direct normal or global horizontal
C data.
            if ( idngh .eq. 0 ) then      ! idnr array holds direct normal radiation, convert to direct horizontal
              fCurrDirHoriz = fCurrDirSolar *
     &                          esind(fSolarAltitude(iCurrDay, iHour))
            else                          ! idnr array holds global horizontal radiation, convert to direct horizontal
              fCurrDirHoriz = fCurrDirSolar - fCurrDiffuseSolar
            endif

            fSeasonGlobalHoriz = fSeasonGlobalHoriz + fCurrDirHoriz +
     &                           fCurrDiffuseSolar
          enddo seasonal_hr_loop

        enddo  seasonal_day_loop


C Now loop through the season day by day, and examine every seven day
C period as a week that might be a good candidate for representing
C the entire season. This is an unbounded loop, but it terminates
C when the last day of the week is also the last day of the season.
C Therefore, it should run X-6 times, where X is the number of days
C in the season.

        iCurrDay = iSeasonStartDay(iCurrSeason)

        iNumberofWeeks = 0            ! initialize variable used to loop through the season


        week_loop: do

          iNumberofWeeks = iNumberofWeeks + 1

C Get the start day of the current week: correspinds to the current day
C in the season.
        iCandidateWeekStart(iNumberofWeeks) = iCurrDay     ! variable used to compare weekly and seasonal parameters

C Establish the end day of the current analysis week.
C (six days from today).
        iCurrWeekEnd = iCurrDay + (iWeekLength -1)

        if ( iCurrWeekEnd > 365 )               ! this would be the case if end of year is reached
     &         iCurrWeekEnd = iCurrWeekEnd - 365  ! loop to beginning of the year

        iCandidateWeekEnd(iNumberofWeeks) = iCurrWeekEnd     ! variable used to compare weekly and seasonal parameters

        fWeekHDDTotal = 0.         ! initialize variable used to sum up HDD for the week
        fWeekCDDTotal = 0.         ! initialize variable used to sum up CDD for the week
        fWeekGlobalHoriz = 0.

C Loop through a seven-day period that constitutes the candidate week.
        day_loop: do iDayofWeek = 1, iWeekLength
          iDayofYear = iDayofWeek + iCurrDay - 1
          if ( iDayofYear > 365 )             ! this would be the case if end of year is reached
     &               iDayofYear = iDayofYear - 365  ! loop to beginning of year

          hour_loop: do iHour = 1, 24

            fCurrHourTemp = real(itmp(iDayofYear,iHour))/10.  ! divide by 10 because temperatures in weather file are in units of (tenth deg)

            if  ( fCurrHourTemp < fHDDBaseTemp ) then
              fHDDCurrHour = fHDDBaseTemp - fCurrHourTemp
            else
              fHDDCurrHour = 0.
            endif

            if  ( fCurrHourTemp > fCDDBaseTemp ) then
               fCDDCurrHour = fCurrHourTemp - fCDDBaseTemp
            else
               fCDDCurrHour = 0.
            endif

            fWeekHDDTotal = fWeekHDDTotal + fHDDCurrHour     ! total deg-hrs for week
            fWeekCDDTotal = fWeekCDDTotal + fCDDCurrHour     ! total deg-hrs for week

            fCurrDiffuseSolar = real(idif(iDayofYear,iHour))
            fCurrDirSolar = real(idnr(iDayofYear,iHour))

            if ( idngh .eq. 0 ) then      ! idnr array holds direct normal radiation, convert to direct horizontal
              fCurrDirHoriz = fCurrDirSolar *
     &                      esind(fSolarAltitude(iDayofYear, iHour))
            else                          ! idnr array holds global horizontal radiation, convert to direct horizontal
             fCurrDirHoriz = fCurrDirSolar - fCurrDiffuseSolar
            endif

            fWeekGlobalHoriz = fWeekGlobalHoriz + fCurrDirHoriz +
     &                         fCurrDiffuseSolar

          enddo  hour_loop
        enddo day_loop
        fAvgWeekHDDperDay = fWeekHDDTotal / ( 24. * real(iWeekLength)) ! average HDD per hour for the week
        fAvgWeekCDDperDay = fWeekCDDTotal / ( 24. * real(iWeekLength)) ! average CDD per hour for the week
        fAvgWeekGlobHorperDay = fWeekGlobalHoriz / real(iWeekLength)

C Need to store weekly average values in an array to
C compare with seasonal average  values later on.
        fCandidateWeekHDD(iNumberofWeeks) = fAvgWeekHDDperDay
        fCandidateWeekCDD(iNumberofWeeks) = fAvgWeekCDDperDay
        fCandidateWeekSolar(iNumberofWeeks) = fAvgWeekGlobHorperDay




        if ( iCurrWeekEnd == iSeasonEndDay (iCurrSeason) )
     &               exit week_loop



        iCurrDay = iCurrDay + 1          ! go to next day if still within season
        if ( iCurrDay > 365 )            ! this would be the case if end of year is reached
     &     iCurrDay = iCurrDay - 365     ! loop to beginning of year

        enddo week_loop



C Determine average seasonal HDD/hour, CDD/hour and global horizontal radiation/hour
         fAvgSeasonalHDDperDay = fSeasonHDDTotal /
     &                        ( 24. * real(iLengthSeason) )
         fAvgSeasonalCDDperDay = fSeasonCDDTotal /
     &                        ( 24. * real(iLengthSeason) )
         fAvgSeasonalGlobHorperDay = fSeasonGlobalHoriz /
     &                        real(iLengthSeason)

C Flag if only heating or cooling season
         bHeating = .true.
         bCooling = .true.
         call eclose(fSeasonHDDTotal,0.00,0.0001,bClose)
         if ( bClose ) bHeating = .false.
         call eclose(fSeasonCDDTotal,0.00,0.0001,bClose)
         if ( bClose ) bCooling = .false.

C Determine if season is predominantly heating or cooling and set
C the weighting factors for comparison accordingly
C C.L 2012-02-07, protect agains Floating-point exception / 0, causes crash in debug mode
         if ( fSeasonCDDTotal .ne. 0.) then
          if ( ( fSeasonHDDTotal / fSeasonCDDTotal ) > 10. ) then
             bHeating = .true.
             bCooling = .false.
             fHtgWeighting = 1.0
             fClgWeighting = 0.
          elseif ( ( fSeasonHDDTotal / fSeasonCDDTotal ) < 0.1 ) then
             bHeating = .false.
             bCooling = .true.
             fHtgWeighting = 0.
             fClgWeighting = 1.
          else
             bHeating = .true.
             bCooling = .true.
             fHtgWeighting = 1.
             fClgWeighting = 1.
          endif
         endif
        fHtgWeighting = 1.
        fClgWeighting = 1.
C Compare seasonal average to weekly average to find week that
C best represents the entire season.
        iBestWeekIndex = 1

         compare_weeks: do iCandidateWeekIndex = 2, iNumberofWeeks

          if ( bHeating ) then
             fCurrHDDDiff = ABS( fCandidateWeekHDD(iCandidateWeekIndex)
     &                        - fAvgSeasonalHDDperDay ) /
     &                     fAvgSeasonalHDDperDay                    ! compare dimensionless value

             fBestHDDDiff = ABS( fCandidateWeekHDD(iBestWeekIndex) -
     &                               fAvgSeasonalHDDperDay ) /
     &                     fAvgSeasonalHDDperDay                    ! compare dimensionless value
          else
             fCurrHDDDiff = 0.
             fBestHDDDiff = 0.
          endif

          if ( bCooling ) then
             fCurrCDDDiff = ABS( fCandidateWeekCDD(iCandidateWeekIndex)
     &                       -  fAvgSeasonalCDDperDay ) /
     &                     fAvgSeasonalCDDperDay                    ! compare dimensionless value

             fBestCDDDiff = ABS( fCandidateWeekCDD(iBestWeekIndex) -
     &                               fAvgSeasonalCDDperDay ) /
     &                     fAvgSeasonalCDDperDay                    ! compare dimensionless value
          else
             fCurrCDDDiff = 0.
             fBestCDDDiff = 0.
          endif

          fCurrSolarDiff = ABS( fCandidateWeekSolar(iCandidateWeekIndex)
     &                          -     fAvgSeasonalGlobHorperDay ) /
     &                      fAvgSeasonalGlobHorperDay

          fBestSolarDiff = ABS( fCandidateWeekSolar(iBestWeekIndex) -
     &                               fAvgSeasonalGlobHorperDay ) /
     &                      fAvgSeasonalGlobHorperDay

          fTotalCurrDiff = ( fHtgWeighting * fCurrHDDDiff ) **2 +
     &                       ( fClgWeighting * fCurrCDDDiff ) **2 +
     &                       ( fSlrWeighting * fCurrSolarDiff ) **2

          fTotalBestDiff = ( fHtgWeighting * fBestHDDDiff ) **2 +
     &                       ( fClgWeighting * fBestCDDDiff ) **2 +
     &                       ( fSlrWeighting * fBestSolarDiff ) **2


             if ( fTotalCurrDiff < fTotalBestDiff )
     &             iBestWeekIndex = iCandidateWeekIndex



         enddo compare_weeks


C Save representative week for each season to pass to other subroutines.
         iSeasonWeekStart(iCurrSeason) = iCandidateWeekStart
     &                                         (iBestWeekIndex)
         iSeasonWeekEnd(iCurrSeason) = iCandidateWeekEnd
     &                                         (iBestWeekIndex)



C Calculate heating and cooling multipliers (these multipliers are applied to the weekly energy fluxes
C to approximate the seasonal energy fluxes for the quick run simulation).
         if ( bHeating )  then
             fHtgMultiplier(iCurrSeason) = ( fSeasonHDDTotal  ) /
     &   ( fCandidateWeekHDD(iBestWeekIndex) * 24. * real(iWeekLength))
         else
             fHtgMultiplier(iCurrSeason) = 0.
         endif

         if ( bCooling ) then
             fClgMultiplier(iCurrSeason) = ( fSeasonCDDTotal ) /
     &   ( fCandidateWeekCDD(iBestWeekIndex) * 24.* real(iWeekLength))
         else
             fClgMultiplier(iCurrSeason) = 0.
         endif

         fGenMultiplier(iCurrSeason) = real(iLengthSeason)/
     &               real(iWeekLength )



      enddo season_loop


C Close climate file
      call ERpFree( ifil, istat )


C Error handling goes here!
 1000 Continue

      return
      end
