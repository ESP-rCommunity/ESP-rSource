*CONTROL
*cdoc free float except for office hours - 20C heating 24C cooling
*building
*zdoc all zones reference the same control logic.
*scope HEATCOOL    
   1                        # number of loops
*loop   1 bld_loop_01
    0    0    0    0        # senses the temperature of the current zone.
    0    0    0             # actuates air point of the current zone
    1                       # all day types have same control
    1  365    3             # valid Sat-01-Jan - Sun-31-Dec, periods in weekdays
    0    2   0.000   0.     # ctl type, law (free floating), start @, data items
    0    1   7.000   7.     # ctl type, law (basic control), start @, data items
  5000.000 0.000 5000.000 0.000 20.000 24.000 0.000  # basic control: heating capacity 5000.0W. cooling capacity 5000.0W. heating setpoint 20.00C cooling setpoint 24.00C.
    0    2  18.000   0.     # ctl type, law (free floating), start @, data items
# Function:Zone links
 1,1,1,1
