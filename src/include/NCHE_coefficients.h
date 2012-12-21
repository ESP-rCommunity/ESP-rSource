C
C NCHE_coefficients.h
C
C This header contains data structures associated with the natural
C convection heat exchanger (NCHE) model.
C
C----------------------------------------------------------------------
C     Named constants
C----------------------------------------------------------------------
C.....Convergence tolerance
C      REAL fConvergence_tolerance
C      parameter (fConvergence_tolerance   = 1.0E-06 )
      
C.....Close to zero tolerance      
C      REAL fClose_to_zero_tolerance 
C      parameter ( fClose_to_zero_tolerance = 1.0E-06 )


C----------------------------------------------------------------------
C     Data structures
C----------------------------------------------------------------------
    
C.....Model parameters
      COMMON/NCHE_coefficients/fHX_eff_CR_a, fHX_eff_CR_b,
     &                         fHX_flow_P_c, fHX_flow_P_d,
     &                         fHX_height,
     &                         fMC_HX_hot, fMC_HX_cold,
     &                         fTank_Height, 
     &                         iStrat_tank_CompIndex,
     &                         iStrat_tank_number

C.....Modified effectiveness vs. capacitance ratio coefficients
      REAL fHX_eff_CR_a(MPCom)
      REAL fHX_eff_CR_b(MPCom)
      
C.....Natural convection flow rate vs. pressure drop coefficients
      REAL fHX_flow_P_c(MPCom)
      REAL fHX_flow_P_d(MPCom)

C.....Heat exchanger heat capacity 
      REAL fMC_HX_hot(MPCom)
      REAL fMC_HX_cold(MPCom)

C.....Heat exchanger height (m)
      REAL fHX_height(MPCom)  

C.....Height of connected storage tank (m)
      REAL fTank_Height

C.....Component number of attached stratified storage tank
      INTEGER iStrat_tank_CompIndex

C.....Number of the stratified storage tank component attached to the NCHE component,
C.....is always set to one for the first release. Could be not equal to 1 if there are more than one 
C.....stratified storage tank components in the network.
      INTEGER iStrat_tank_number

C----------------------------------------------------------------------
C     Functions
C----------------------------------------------------------------------      

C.....Heat exchanger calculated flow rate (kg/s) 
C.....(in subroutine esruplt/NCHE.F/NCHE_flow)
      REAL fNCHE_flow

C.......stratified storage tank (in subroutine esruplt/stratified_tank.F)
      REAL fHX_get_stratified_tank_height
      REAL fHX_get_stratified_tank_avg_temp
C.......stratified tank with in-tank heat exchanger    
      REAL fHX_get_in_tank_hx_height
      REAL fHX_get_in_tank_hx_avg_temp



C----------------------------------------------------------------------
C     Functions for reporting to H3K Reports at the end of the timestep
C----------------------------------------------------------------------
      COMMON/NCHE_results/fReport_HX_effectiveness,
     &                    fReport_HX_q,
     &                    fReport_HX_hot_mass_flow,
     &                    fReport_HX_cold_mass_flow,
     &                    fReport_HX_hot_temp_in,
     &                    fReport_HX_hot_temp_out,
     &                    fReport_HX_cold_temp_in,
     &                    fReport_HX_cold_temp_out


      REAL fReport_HX_effectiveness(mpcom)  ! Heat Exchanger modified effectiveness (-)
      REAL fReport_HX_q (mpcom)             ! Heat Exchanger heat transfer rate (W)
      REAL fReport_HX_hot_mass_flow(mpcom)  ! Hot stream flow rate
      REAL fReport_HX_cold_mass_flow(mpcom) ! Cold stream flow rate
      REAL fReport_HX_hot_temp_in(mpcom)    ! Hot stream inlet water temperature
      REAL fReport_HX_hot_temp_out(mpcom)   ! Hot stream outlet water temperature
      REAL fReport_HX_cold_temp_in(mpcom)   ! Cold stream inlet water temperature
      REAL fReport_HX_cold_temp_out(mpcom)  ! Cold stream outlet water temperature
