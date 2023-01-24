*CONTROL
*cdoc air based environmental control
*building
*zdoc maintain between 30 and 35C
*scope HEATCOOL    
   1                        # number of loops
*loop   1 bld_loop_01
    0    0    0    0        # senses the temperature of the current zone.
    0    0    0             # actuates air point of the current zone
    1                       # all day types have same control
    1  365    1             # valid Sat-01-Jan - Sun-31-Dec, periods in weekdays
    0    1   0.000   7.     # ctl type, law (basic control), start @, data items
  10000.000 0.000 10000.000 0.000 30.000 35.000 0.000  # basic control: heating capacity 10000.0W. cooling capacity 10000.0W. heating setpoint 30.00C cooling setpoint 35.00C.
# Function:Zone links
 1
