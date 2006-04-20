C ********************************************************************************
C ***************************** FC_ctl_h3khvacloads ******************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: June 7, 2002
C Copyright CETC 2002

C This module declares variables that track the electrical draws of the
C HOT3000 HVAC equipment (ie. HVAC simulated in the building thermal domain).
C The first implementation considers only the HOT3000 equipment that is used
C in the FCT simulation tool.  This needs to be updated to track other equipment
C in the future.
C---------------------------------------------------------------------------------
c Use common for electrical draws of hvac equipment
      common/FC_ctl_h3khvacloads/
     & FCctl_HRV_draw, FCctl_AC_compressor, FCctl_AC_circfan,
     & FCctl_furnace_draft, FCctl_furnace_circfan,
     & FCctl_pumps, FCctl_fans, fCurrent_TS_q_total_elec,
     & FCctl_boiler_draft, FCctl_boiler_circfan,
     & FCctl_baseboard_fan, FCctl_ashp_compressor,
     & FCctl_ashp_circfan, FCctl_gshp_pump,
     & FCctl_TAC_draw, FCctl_elec_water_tank

C---------------------------------------------------------------------------------
C Electrical draw (W) of HRV.  This includes the supply fan, exhaust fan,
C and electrical resistance pre-heater.
C---------------------------------------------------------------------------------
        REAL  FCctl_HRV_draw

C---------------------------------------------------------------------------------
C Electrical draw (W) of A/C equipment (ie. ASHP operating in cooling mode).
C One variable for the A/C unit (compressor and condensor fan) and another for the
C ciculating fan that distributes cool air through the house's ductwork.
C---------------------------------------------------------------------------------
        REAL  FCctl_AC_compressor
        REAL  FCctl_AC_circfan   

C---------------------------------------------------------------------------------
C Electrical draw (W) of gas-fired furnace.  One variable for the draft fan
C and another for the ciculating fan that distributes warm air through the
C house's ductwork. If the furnace is a backup system, the circulating fan
C power draw will be contained in the FCctl_ashp_circfan variable.
C---------------------------------------------------------------------------------
        REAL  FCctl_furnace_draft
        REAL  FCctl_furnace_circfan

C---------------------------------------------------------------------------------
C Electrical draw (W) of explicit plant pumps and fan.
C---------------------------------------------------------------------------------
        REAL  FCctl_pumps
        REAL  FCctl_fans 
        REAL  fCurrent_TS_q_total_elec

C---------------------------------------------------------------------------------
C Electrical draw (W) of gas-fired boiler. One variable for the draft fan
C (applicable for induced draft and condensing boilers). another for the
C circulating fan (though likely there is no circulating fan power for a boiler).
C Power draw of electric boiler not included. If the boiler is a backup system,
C the circulating fan power draw will be contained in the FCctl_ashp_circfan variable.
C---------------------------------------------------------------------------------
        REAL  FCctl_boiler_draft
        REAL  FCctl_boiler_circfan

C---------------------------------------------------------------------------------
C Electric draw (W) of circulating fan for baseboards. Power draw of 
C electric baseboards not included. If the baseboard is a backup system,
C the circulating fan power draw will be contained in the FCctl_ashp_circfan variable.
C---------------------------------------------------------------------------------
        REAL  FCctl_baseboard_fan

C---------------------------------------------------------------------------------
C Electric draw (W) of air source heat pump in heating mode. One variable for the
C compressor and condenser fan. One variable for the circulating fan that distributes
C warm air to the house's ductwork. The fan power of the circulating fan for the 
C backup furnace, boiler or baseboard is also included in FCctl_ashp_circfan.
C---------------------------------------------------------------------------------
        REAL  FCctl_ashp_compressor
        REAL  FCctl_ashp_circfan

C---------------------------------------------------------------------------------
C Electric draw (W) of ground source heat pump. One variable for the pump that
C circulates fluid through the ground loop.  Electric draw  
C for the fan that distributes warm air through the house's ductwork and the 
C compressor is captured in FCctl_ashp_circfan and FCctl_ashp_compressor, respectively.
C---------------------------------------------------------------------------------
        REAL  FCctl_gshp_pump 

C---------------------------------------------------------------------------------
C Electric draw (W) of thermally activated cooling equipment. This includes the
C refrigerant pump power and the condenser fan power.
C---------------------------------------------------------------------------------
        REAL  FCctl_TAC_draw

C---------------------------------------------------------------------------------
C Electric draw (W) of electrically heated water tank.
C---------------------------------------------------------------------------------
        REAL  FCctl_elec_water_tank




