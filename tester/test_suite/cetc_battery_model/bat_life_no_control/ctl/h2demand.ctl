*CONTROL
*cdoc control for RES-H2 sys
*building
*zdoc simple heating
*scope HEATCOOL    
   1                        # number of loops
*loop   1 heat to 15C
    0    0    0    0        # senses the temperature of the current zone.
    0    0    0             # actuates air point of the current zone
    1                       # all day types have same control
    1  365    1             # valid Sat-01-Jan - Sun-31-Dec, periods in weekdays
    0    1   0.000   7.     # ctl type, law (basic control), start @, data items
  20000.000 0.000 0.000 0.000 15.000 100.000 0.000  # basic control: heating capacity 20000.0W. cooling capacity 0.0W. heating setpoint 15.00C cooling setpoint 100.00C.
# Function:Zone links
 1
*plant
*pdoc pump_h2storage
   1                        # number of loops
*loop   1 plant_loop_01
   -1    5    1    0    0  # senses var in compt.  5:pump-1 @ node no.  1
   -1    4    1    0        # actuates plant component   4:h2-storage @ node no.  1
    1                       # all day types have same control
    1  365    1             # valid Sat-01-Jan - Sun-31-Dec, periods in weekdays
   12   11   0.000   6.     # ctl type, law (H2 dmd. control), start @, data items
  1.00000 2.00000 3.00000 9.00000 10.00000 11.00000  # not yet coded for system controls.
