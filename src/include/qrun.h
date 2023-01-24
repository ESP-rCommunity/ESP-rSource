
      integer iMaxSeasons     ! maximum number of seasons that can be defined in the year
      parameter (iMaxSeasons = 10 )

C Arrays contain start and end day numbers of best week and heating and cooling multipliers for each season      
      common/season/iSeasonWeekStart, iSeasonWeekEnd, fHtgMultiplier,
     &              fClgMultiplier, fGenMultiplier, iWeekLength
      integer iSeasonWeekStart(iMaxSeasons)   ! start day of best representative week for the season
      integer iSeasonWeekEnd(iMaxSeasons)     ! end day of best representative week for the season
      real fHtgMultiplier(iMaxSeasons)        ! heating multiplier to be applied to the weekly energy fluxes to estimate seasonal energy fluxes
      real fClgMultiplier(iMaxSeasons)        ! cooling multiplier to be applied to the weekly energy fluxes to estimate seasonal energy fluxes
      real fGenMultiplier(iMaxSeasons) 
      integer iWeekLength
      
C Quick run method: best week is found for each season defined in the year. The
C best week is defined as the week that best represents the climate for the season. 
C Time-step controller controls bps to run once for all the best weeks found. 
C The flag (iQuick_run) is set in the cfg file with the tag *quick_run
C iQuick_run = 0: no; iQuick_run = 1: yes.  
      common/TS_ctl/iQuick_run, iFirst_day_num, iLast_day_num,
     &              bSimThisDay, iSeasonIndex, iThisSeason, iStart_flag
      integer iQuick_run
      integer iFirst_day_num      !- day number corresponding to the start of the continuous bps run
      integer iLast_day_num       !- day number corresponding to the end of the continuous bps run     
      logical bSimThisDay(750)    !- array indicating whether simulation should run on current day
      integer iSeasonIndex(750)   !- array mapping season indicies to individual days.
      integer iThisSeason         !- scalar containing current season.
      integer iStart_flag(750)    !- array indicating whether current day is in start-up period
      
      integer iNumberOfSeasons
      parameter ( iNumberOfSeasons = 4 )