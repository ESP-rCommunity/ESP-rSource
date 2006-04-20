c Common for furnace input data
      common/FURNACE_INPUT_DATA/ifuel_type(max_fuels),
     & ss_efficiency(max_sys), pilot_power(max_sys),
     & af(max_sys), bf(max_sys), cf(max_sys), df(max_sys)

c Declare integers in FURNACE_INPUT_DATA common
      INTEGER ifuel_type

c Declare real in FURNACE_INPUT_DATA common
      REAL ss_efficiency
      REAL pilot_power 
      REAL af,bf,cf,df
