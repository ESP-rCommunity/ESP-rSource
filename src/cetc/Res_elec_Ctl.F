C-----------------------------------------------------------------------------------------
C This is a customized controller for purposes of modelling a lithium ion battery coupled
C with a micro-cogeneration device and the electric grid.  Unlike conventional ESP-r controllers
C this controller senses the state of numerous plant and electrical domain components
C and actuates numerous plant and electrical domain components.  Its structure is highly
C customized to a specific system configuration.  It is anticipated that in the future a
C more generalized control structure will be established to provide this functionality
C and thus supplant this controller.
C
C This controller is invoked from ENETSOLV at the beginning of the execution of the
C electric domain.  Consequently, it senses the state of electric domain components
C from the previous solution of the electric domain.  If bps is iterating within the
C time-step between the plant and electrical domains to converge a solution, then the
C controller is sensing the state of the electric domain components at the current
C time-step.  Otherwise, if this is the first pass through the electric domain for the
C current time-step then the controller is sensing the state of electric domain components
C at the previous time-step.  Since the plant domain is solved prior to the invocation
C of the electric domain, the controller is sensing the state of plant components at the
C current time-step.
C
C The controller actuates a number of plant domain components.  If one of these control
C actions results in a different operational mode for a plant component, a flag is set
C that forces another iteration through the plant and electric domains at the current
C time-step.
C
C This is adapted from src/cetc/RE-H2-ctl.F
C For more information on the strategies see Neil Saldanha, Carleton U's master thesis
C-----------------------------------------------------------------------------------------

      SUBROUTINE Res_elec_storage_ctl
      IMPLICIT NONE
      integer ctl_mode
      
C Control Mode Declaration
      ctl_mode = 2
C This dictates which of the following algorithms to follow
C = 0 : Do nothing. enetsolv.F skips over this subroutine
C = 1 : Sets a constant draw on the battery. See simple_ctl subroutine below
C = 2 : Low cost strategy control. See low_cost subroutine below
C = 3 : Thermal following strategy control. See thermal_follow subroutine below

      if (ctl_mode .eq. 1) then
        call simple_ctl          !call up constant power draw
      elseif (ctl_mode .eq. 2) then
        call low_cost            !call up low cost
      elseif (ctl_mode .eq. 3) then
        call thermal_follow      !call up thermal following control
      else                       !do nothing and exit this subroutine
      endif
      END ! End main subroutine


C-------------------------simple control mode---------------------------------------
C This routine simply applies a constant power draw on the battery.
C It is meant mostly for debugging.
C-----------------------------------------------------------------------------------
      SUBROUTINE simple_ctl
      IMPLICIT NONE

#include "BATTERY.h"

C This is the demand in Watts imposed on the battery
      batDemandP_Lion = 4.3
C +ve indicates discharging the battery, -ve charge

      END ! End simple control algorithm


C-------------------------low cost control mode-------------------------------------
C This routine applies the low cost strategy on the system
C-----------------------------------------------------------------------------------
      SUBROUTINE low_cost
      IMPLICIT NONE

#include "BATTERY.h"
c #include "building.h"
c #include "plant.h"
c #include "model.h"

C House hvac + non hvac load determined in subroutine POWOC_OUTSIDE_ld
      common/Res_elec_ctl/house_load

C ESP-r COMMONs
c       COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
c       INTEGER IHRP            !-hour of present time-step
c       INTEGER IHRF            !-hour of future time-step
c       INTEGER IDYP            !-year day number of present day
c       INTEGER IDYF            !-year day number of future day
c       INTEGER IDWP            !-day of the week of present day
c       INTEGER IDWF            !-day of the week of future day
c       INTEGER NSINC           !-number of building-side time increments
                               !-since start of simulation
c       INTEGER ITS             !-current building time-step within
                               !-current hour

c       COMMON/PCTIME/TIMSEC
c       REAL    TIMSEC          !-length of time increment (second)
      
C Variable declaration and common blocks
c      real off_cost,  ! Cost of electricity in off period ($/kWh)
c     &     mid_cost,  ! Cost of electricity in mid period ($/kWh)
c     &     on_cost,   ! Cost of electricity in on  period ($/kWh)
c     &     fuel_cost, ! Cost of cogen fuel ($/m^3)
c     &     cost,      ! The cost of fuel based on the simulation timestep
 
       real     house_load ! The house load in (W)

c      integer summ_month_s, ! The month that TOU summer pricing scheme begins
c     &        summ_day_s,   ! The day that TOU summer pricing scheme begins
c     &        wint_month_s, ! The month that TOU winter pricing scheme begins
c     &        wint_day_s,    ! The day that TOU winter pricing scheme begins
c     &        summ_month_e, ! The month that TOU summer pricing scheme ends
c     &        summ_day_e,   ! The day that TOU summer pricing scheme ends
c     &        wint_month_e, ! The month that TOU winter pricing scheme ends
c     &        wint_day_e    ! The day that TOU winter pricing scheme ends
          
C Assign the cost to purchase electricity from the grid 
C and summer and winter starts according to 
C Ontario Power Authority's TOU Pricing Scheme

c      off_cost = 0.043
c      mid_cost = 0.071
c      on_cost = 0.105
      
c      summ_month_s = 8 ! Summer starts on Aug 1
c      summ_day_s = 1
      
c      summ_month_e = 10 ! Summer ends on October 31
c      summ_day_e = 31
      
c      wint_month_s = 11 ! Winter starts on Nov 1
c      wint_day_s = 1  
      
c      wint_month_e = 2 ! Winter ends on Nov 28
c      wint_day_e = 28
      
      batDemandP_Lion = -825.-house_load

C If the house demand is above a certain value, evaluate
C which is cheaper to supply it: the cogen system or the
C grid based of fuel cost and TOU rates


C If it's the grid, it should meet the demand and
C keep the battery above a certain SOC (say 90-99%)

C Else it's the cogen, ie. turn the cogen on.


C If the cogen output isless than the present house
C demand and the battery SOC at the end of the
C time step < 20%, use the Pcogen + Pbatt(safe) and
C the grid to make it up

C Else if t


      END ! low cost control mode

C-------------------------thermal follow control mode-------------------------------------
C fill me in
C-----------------------------------------------------------------------------------
      SUBROUTINE thermal_follow
      IMPLICIT NONE

c #include "BATTERY.h"
c #include "building.h"
c #include "plant.h"


      END ! thermal follow
