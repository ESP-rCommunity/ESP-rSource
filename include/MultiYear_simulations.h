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
C================== MultiYear_simulations.h ============================
C
C Date: May 5, 2005
C Author: Alex Ferguson
C Copyright: Natural Resources Canada
C
C This file contains data structures used by the multi-year
C simulation procedures in ESP-r.
C
C======================================================================


C----------------------------------------------------------------------
C     Temporary character buffer
C----------------------------------------------------------------------
      character*72 cTemp

C----------------------------------------------------------------------
C     Number of climate files suppored in multi-year simulations
C----------------------------------------------------------------------
      integer iMax_Climate_Years
      parameter ( iMax_Climate_Years = 99 )

C----------------------------------------------------------------------
C     cMY_climate_db_name contains the master multi-year climate
C     database file name
C----------------------------------------------------------------------
      character*72 cMY_climate_db_name

C----------------------------------------------------------------------
C     fMY_version_number: database version number
C----------------------------------------------------------------------
      real fMY_version_number
      real fMY_default_version
      parameter (fMY_default_version = 0.10 )

C----------------------------------------------------------------------      
C     iMY_clm_file_count contains the number of clm files that
C     have been defined. 
C----------------------------------------------------------------------
      integer iMY_clm_file_count

C----------------------------------------------------------------------
C     iMY_clm_file_order contains the order in which the climate files
C     will be used.
C--------------------------------------------------------------------
      integer iMY_clm_file_order(iMax_Climate_Years)
      
C----------------------------------------------------------------------      
C     iMY_clm_file_years is an array holding the years that the clm
C     files correspond to.
C----------------------------------------------------------------------
      integer iMY_clm_file_years(iMax_Climate_Years)

C----------------------------------------------------------------------      
C     cMY_clm_file_names is an array holding the names of the climate
C     files used in multi-year simulations
C----------------------------------------------------------------------
      character*72 cMY_clm_file_names(iMax_Climate_Years)

C----------------------------------------------------------------------      
C     bClm_file_open is an array of logical flags used to determine if
C     the appropriate climate file is open 
C----------------------------------------------------------------------      
      logical bMY_clm_file_open(iMax_Climate_Years)
      
C----------------------------------------------------------------------
C     bMY_climates_defined is a logical variable indicating if
C     multi-year climate files have been defined
C----------------------------------------------------------------------
      logical bMY_climates_defined

C----------------------------------------------------------------------
C     bSY_climate_defined is a logical variable indicating if
C     a single-year climate file has been defined
C----------------------------------------------------------------------
      logical bSY_climate_defined
      
C----------------------------------------------------------------------
C     Multi-year climate data structure      
C----------------------------------------------------------------------
      common / MY_Climate / bMY_climates_defined,
     &                      bSY_climate_defined,
     &                      cMY_climate_db_name,
     &                      fMY_version_number,
     &                      iMY_clm_file_count,
     &                      iMY_clm_file_order,
     &                      iMY_clm_file_years,
     &                      cMY_clm_file_names,
     &                      bMY_clm_file_open
     
C----------------------------------------------------------------------
C     Flags for multi-year simulations 
C----------------------------------------------------------------------
      logical bMY_sim_enabled

C----------------------------------------------------------------------
C     Start year, end year and current year and future year
C     in the simulation
C----------------------------------------------------------------------
      integer iMY_start_year, iMY_end_year, iMY_current_year
      integer iMY_future_year

C----------------------------------------------------------------------
C     Simulation start and end years, as defined in simulation presets
C----------------------------------------------------------------------            
      integer iMY_period_start_years(MSPS), iMY_period_end_years(MSPS)

C----------------------------------------------------------------------
C     Flag indicating that pre-defined sumulation period is a
C     multi-year definition
C----------------------------------------------------------------------
      logical bMY_period_support(MSPS)

C----------------------------------------------------------------------
C     Copy of the 'real day' unadjusted for normal 1->365 day bounds
C----------------------------------------------------------------------      
      integer iMY_real_day
      
C----------------------------------------------------------------------
C     Multi-year general data structure
C----------------------------------------------------------------------
      common / MY_Simulations / bMY_sim_enabled,
     &                          iMY_period_start_years,
     &                          iMY_period_end_years,
     &                          bMY_period_support,
     &                          iMY_start_year,
     &                          iMY_end_year,
     &                          iMY_current_year,
     &                          iMY_future_year,
     &                          iMY_real_day

