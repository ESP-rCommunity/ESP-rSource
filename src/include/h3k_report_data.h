C Flags indicating if first call to reporting routines is complete.
C These flags are used for integrating output variables for quick run (seasonal) simulations
      logical bFirst_call_bld
      logical bFirst_call_hvac(25) ! 12-Apr-2011 BAL: increased to match max_sys in hvac_parameters.h.
      logical bFirst_call_fan
      logical bFirst_call_hrv
      logical bFirst_call_dhw
      common/h3k_reports_flags/bFirst_call_bld, bFirst_call_hvac,
     &                         bFirst_call_fan, bFirst_call_hrv,
     &                         bFirst_call_dhw