*CONTROL
*cdoc no overall control description supplied
*building
*zdoc senses room db and drives a 60% convective heater.
*scope HEATCOOL    
   1                        # number of loops
*loop   1 bld_loop_01
    0    0    0    0        # senses the temperature of the current zone.
   -2    1   60             # actuates mixed convective 60% /radiant flux in TheSpace.
    1                       # all day types have same control
    1  365    1             # valid Tue-01-Jan - Tue-31-Dec, periods in weekdays
    0    1   0.000   7.     # ctl type, law (basic control), start @, data items
  1000.000 0.000 1000.000 0.000 20.000 24.000 0.000  # basic control: heating capacity 1000.0W. cooling capacity 1000.0W. heating setpoint 20.00C cooling setpoint 24.00C.
# Function:Zone links
 1,0,0,0,0
