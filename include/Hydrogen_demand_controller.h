C
C
C Hydrogen_demand_controller.h:
C
C This file contains data structures associated with the hydrogen
C demand controller. 
C 


C.....Component hydrogen demands
      common / PLT_H2_demands /
     &   fH2_plt_EXP_demand,
     &   fH2_plt_IMP_demand,
     &   fH2_plt_EXP_supply,
     &   fH2_plt_IMP_supply_TOTAL,
     &   fH2_plt_EXP_supply_TOTAL,
     &   fH2_plt_app_load 
     
C.....Demands of components directly connected to the H2 supply
C.....device using ESP-r's plant connection modelling facilities
C.....(kg/s)
      real fH2_plt_EXP_demand ( MPCom )
      
C.....Demands of components directly not connected to the H2 supply
C.....device using ESP-r's plant connection modelling facilities
C.....(kg/s)
      real fH2_plt_IMP_demand ( MPCom )

C.....Actual amount of hydrogen delivered though 'implicitly modelled'
C.....connections (kg/s)
      real fH2_plt_EXP_supply ( MPCom )

C.....Total amount of hydrogen supplied through implicit connections
C.....(kg/s)
      real fH2_plt_IMP_supply_TOTAL( MPCom )

C.....Total amount of hydrogen supplied through explicit connections
C.....(kg/s)
      real fH2_plt_EXP_supply_TOTAL( MPCom )

C.....Total amount of hydrogen supplied through explicit connections
C.....(kg/s)
      real fH2_plt_app_load( MPCom )
      
