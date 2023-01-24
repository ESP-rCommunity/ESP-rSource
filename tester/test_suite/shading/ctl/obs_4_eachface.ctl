*CONTROL
*cdoc control for obs_4_eachface
*building
*zdoc building cntl fn model
*scope HEATCOOL    
   3                        # number of loops
*loop   1 bld_loop_01
    1    0    0    0        # senses dry bulb temperature in Main.
    0    0    0             # actuates air point of the current zone
    5                       # uses dates of validity
    1   91    5             # valid Sat-01-Jan - Sat-01-Apr, periods in weekday
    0    1   0.000   7.     # ctl type, law (basic control), start @, data items
  5985.081 0.000 0.000 0.000 17.000 100.000 0.000  # basic control: heating capacity 5985.1W. cooling capacity 0.0W. heating setpoint 17.00C cooling setpoint 100.00C.
    0    1   6.000   7.     # ctl type, law (basic control), start @, data items
  5985.081 0.000 0.000 0.000 20.000 100.000 0.000  # basic control: heating capacity 5985.1W. cooling capacity 0.0W. heating setpoint 20.00C cooling setpoint 100.00C.
    0    1   9.000   7.     # ctl type, law (basic control), start @, data items
  5985.081 0.000 0.000 0.000 17.000 100.000 0.000  # basic control: heating capacity 5985.1W. cooling capacity 0.0W. heating setpoint 17.00C cooling setpoint 100.00C.
    0    1  16.000   7.     # ctl type, law (basic control), start @, data items
  5985.081 0.000 0.000 0.000 20.000 100.000 0.000  # basic control: heating capacity 5985.1W. cooling capacity 0.0W. heating setpoint 20.00C cooling setpoint 100.00C.
    0    1  22.000   7.     # ctl type, law (basic control), start @, data items
  5985.081 0.000 0.000 0.000 17.000 100.000 0.000  # basic control: heating capacity 5985.1W. cooling capacity 0.0W. heating setpoint 17.00C cooling setpoint 100.00C.
   92  154    1             # valid Sun-02-Apr - Sat-03-Jun, periods in saturday
    0    2   0.000   0.     # ctl type, law (free floating), start @, data items
  155  259    5             # valid Sun-04-Jun - Sat-16-Sep, periods in sunday
    0    1   0.000   7.     # ctl type, law (basic control), start @, data items
  0.000 0.000 0.000 0.000 10.000 27.000 0.000  # basic control: heating capacity 0.0W. cooling capacity 0.0W. heating setpoint 10.00C cooling setpoint 27.00C.
    0    1   6.000   7.     # ctl type, law (basic control), start @, data items
  0.000 0.000 0.000 0.000 10.000 24.000 0.000  # basic control: heating capacity 0.0W. cooling capacity 0.0W. heating setpoint 10.00C cooling setpoint 24.00C.
    0    1   9.000   7.     # ctl type, law (basic control), start @, data items
  0.000 0.000 0.000 0.000 10.000 27.000 0.000  # basic control: heating capacity 0.0W. cooling capacity 0.0W. heating setpoint 10.00C cooling setpoint 27.00C.
    0    1  16.000   7.     # ctl type, law (basic control), start @, data items
  0.000 0.000 0.000 0.000 10.000 24.000 0.000  # basic control: heating capacity 0.0W. cooling capacity 0.0W. heating setpoint 10.00C cooling setpoint 24.00C.
    0    1  22.000   7.     # ctl type, law (basic control), start @, data items
  0.000 0.000 0.000 0.000 10.000 27.000 0.000  # basic control: heating capacity 0.0W. cooling capacity 0.0W. heating setpoint 10.00C cooling setpoint 27.00C.
  260  280    1             # valid Sun-17-Sep - Sat-07-Oct, periods in holiday
    0    2   0.000   0.     # ctl type, law (free floating), start @, data items
  281  365    5             # valid Sun-08-Oct - Sun-31-Dec, periods in other
    0    1   0.000   7.     # ctl type, law (basic control), start @, data items
  5985.081 0.000 0.000 0.000 17.000 100.000 0.000  # basic control: heating capacity 5985.1W. cooling capacity 0.0W. heating setpoint 17.00C cooling setpoint 100.00C.
    0    1   6.000   7.     # ctl type, law (basic control), start @, data items
  5985.081 0.000 0.000 0.000 20.000 100.000 0.000  # basic control: heating capacity 5985.1W. cooling capacity 0.0W. heating setpoint 20.00C cooling setpoint 100.00C.
    0    1   9.000   7.     # ctl type, law (basic control), start @, data items
  5985.081 0.000 0.000 0.000 17.000 100.000 0.000  # basic control: heating capacity 5985.1W. cooling capacity 0.0W. heating setpoint 17.00C cooling setpoint 100.00C.
    0    1  16.000   7.     # ctl type, law (basic control), start @, data items
  5985.081 0.000 0.000 0.000 20.000 100.000 0.000  # basic control: heating capacity 5985.1W. cooling capacity 0.0W. heating setpoint 20.00C cooling setpoint 100.00C.
    0    1  22.000   7.     # ctl type, law (basic control), start @, data items
  5985.081 0.000 0.000 0.000 17.000 100.000 0.000  # basic control: heating capacity 5985.1W. cooling capacity 0.0W. heating setpoint 17.00C cooling setpoint 100.00C.
*loop   2 bld_loop_02
    1    0    0    0        # senses dry bulb temperature in Main.
    3    0    0             # actuates the air point in Foundation-1.
    1                       # all day types have same control
    1  365    1             # valid Sat-01-Jan - Sun-31-Dec, periods in weekday
    0   21   0.000   3.     # ctl type, law (slave capacity controller), start @, data items
1.000 2565.035 0.000        # master/slave control: index of master control loop 1 slave maximum heating capacity 2565.03 slave maximum cooling capacity
*loop   3 bld_loop_03
    0    0    0    0        # senses the temperature of the current zone.
    0    0    0             # actuates air point of the current zone
    1                       # all day types have same control
    1  365    1             # valid Sat-01-Jan - Sun-31-Dec, periods in weekday
    0    2   0.000   0.     # ctl type, law (free floating), start @, data items
# Function:Zone links
 1,3,2
