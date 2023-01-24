*CONTROL
*cdoc bld_simple.ctl
*building
*zdoc convective heating to 15C at 7h and 20C at 9h with 3kW capacity, free float otherwise.
*scope HEATCOOL    
   1                        # number of loops
*loop   1 bld_loop_01
    0    0    0    0        # senses the temperature of the current zone.
    0    0    0             # actuates air point of the current zone
    0                       # day types follow calendar  3
    1  365    4             # valid Sun-01-Jan - Sun-31-Dec, periods in weekday
    0    2   0.000   0.     # ctl type, law (free floating), start @, data items
    0    1   7.000   6.     # ctl type, law (basic control), start @, data items
  3000.000 0.000 0.000 0.000 15.000 100.000  # basic control: heating capacity 3000.0W cooling capacity 0.0W. heating setpoint 15.00C cooling setpoint 100.00C. (missing 7th item)
    0    1   9.000   6.     # ctl type, law (basic control), start @, data items
  3000.000 0.000 0.000 0.000 20.000 100.000  # basic control: heating capacity 3000.0W cooling capacity 0.0W. heating setpoint 20.00C cooling setpoint 100.00C. (missing 7th item)
    0    2  18.000   0.     # ctl type, law (free floating), start @, data items
    1  365    1             # valid Sun-01-Jan - Sun-31-Dec, periods in saturday
    0    2   0.000   0.     # ctl type, law (free floating), start @, data items
    1  365    1             # valid Sun-01-Jan - Sun-31-Dec, periods in sunday
    0    2   0.000   0.     # ctl type, law (free floating), start @, data items
# Function:Zone links
 1
