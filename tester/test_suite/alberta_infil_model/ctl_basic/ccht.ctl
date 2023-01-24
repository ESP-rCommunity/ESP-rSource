*CONTROL
*cdoc control for CCHT house
*building
*zdoc first loop free-floats 2nd loop with night setback.
*scope HEATCOOL    
   2                        # number of loops
*loop   1 bld_loop_01
    0    0    0    0        # senses the temperature of the current zone.
    0    0    0             # actuates air point of the current zone
    1                       # all day types have same control
    1  365    1             # valid Fri-01-Jan - Fri-31-Dec, periods in weekday
    0    2   0.000   0.     # ctl type, law (free floating), start @, data items
*loop   2 bld_loop_02
    0    0    0    0        # senses the temperature of the current zone.
    0    0    0             # actuates air point of the current zone
    1                       # all day types have same control
    1  365    3             # valid Fri-01-Jan - Fri-31-Dec, periods in weekday
    0    1   0.000   7.     # ctl type, law (basic control), start @, data items
  20000.000 0.000 0.000 0.000 18.000 100.000 0.000  # basic control: heating capacity 20000.0W. cooling capacity 0.0W. heating setpoint 18.00C cooling setpoint 100.00C.
    0    1   8.000   7.     # ctl type, law (basic control), start @, data items
  20000.000 0.000 6000.000 0.000 21.000 25.000 0.000  # basic control: heating capacity 20000.0W. cooling capacity 6000.0W. heating setpoint 21.00C cooling setpoint 25.00C.
    0    1  20.000   7.     # ctl type, law (basic control), start @, data items
  20000.000 0.000 0.000 0.000 18.000 100.000 0.000  # basic control: heating capacity 20000.0W. cooling capacity 0.0W. heating setpoint 18.00C cooling setpoint 100.00C.
# Function:Zone links
 2,2,1,1
