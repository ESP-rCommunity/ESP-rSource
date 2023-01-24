*CONTROL
*cdoc no overall control description supplied
*building
*zdoc the 3rd loop is referenced
*scope HEATCOOL    
   3                        # number of loops
*loop   1 bld_loop_01
    0    0    0    0        # senses the temperature of the current zone.
    0    0    0             # actuates air point of the current zone
    1                       # all day types have same control
    1  365    1             # valid Mon-01-Jan - Mon-31-Dec, periods in weekdays
    0    1   0.000   7.     # ctl type, law (basic control), start @, data items
  1000.000 0.000 0.000 0.000 20.000 100.000 0.000  # basic control: heating capacity 1000.0W. cooling capacity 0.0W. heating setpoint 20.00C cooling setpoint 100.00C.
*loop   2 coupling to plant
    1    0    0    0        # senses dry bulb temperature in room.
    1    0    0             # actuates the air point in room.
    1                       # all day types have same control
    1  365    1             # valid Mon-01-Jan - Mon-31-Dec, periods in weekdays
    0    6   0.000   7.     # ctl type, law (flux zone/plant), start @, data items
  3.000 2.000 1.000 99000.000 99000.000 4.000 1.000  # plant/zone coupling: source plant component 3 plant component node 2 coupling type mCp(0s-0a) sequential. Max heating 99000.00W max cooling 99000.00W. Extract plant component 4 and extract node 1.
*loop   3 conv heat 20C
    0    0    0    0        # senses the temperature of the current zone.
    0    0    0             # actuates air point of the current zone
    1                       # all day types have same control
    1  365    1             # valid Mon-01-Jan - Mon-31-Dec, periods in weekdays
    0    1   0.000   7.     # ctl type, law (basic control), start @, data items
  10000.000 0.000 0.000 0.000 20.000 100.000 0.000  # basic control: heating capacity 10000.0W. cooling capacity 0.0W. heating setpoint 20.00C cooling setpoint 100.00C.
# Function:Zone links
 3
