*CONTROL
*cdoc no overall control description supplied
*building
*zdoc separate zone specific controls with the same logic.
*scope HEATCOOL    
   2                        # number of loops
*loop   1 bld_loop_01
    1    0    0    0        # senses dry bulb temperature in Zone_1.
    1    0    0             # actuates the air point in Zone_1.
    1                       # all day types have same control
    1  365    1             # valid Fri-01-Jan - Fri-31-Dec, periods in weekday
    0    1   0.000   7.     # ctl type, law (basic control), start @, data items
  100000.000 0.000 100000.000 0.000 15.000 15.000 0.000  # basic control: heating capacity 100000.0W. cooling capacity 100000.0W. heating setpoint 15.00C cooling setpoint 15.00C.
*loop   2 bld_loop_02
    2    0    0    0        # senses dry bulb temperature in Zone_2.
    2    0    0             # actuates the air point in Zone_2.
    1                       # all day types have same control
    1  365    1             # valid Fri-01-Jan - Fri-31-Dec, periods in weekday
    1    1   0.000   7.     # ctl type, law (basic control), start @, data items
  100000.000 0.000 100000.000 0.000 15.000 15.000 0.000  # basic control: heating capacity 100000.0W. cooling capacity 100000.0W. heating setpoint 15.00C cooling setpoint 15.00C.
# Function:Zone links
 1,2
