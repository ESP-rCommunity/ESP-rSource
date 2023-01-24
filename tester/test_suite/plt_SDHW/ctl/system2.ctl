*CONTROL
*cdoc no overall control description supplied
*building
*zdoc no zone control description supplied
*scope HEATCOOL    
   1                        # number of loops
*loop   1 bld_loop_01
    0    0    0    0        # senses the temperature of the current zone.
    0    0    0             # actuates air point of the current zone
    1                       # all day types have same control
    1  365    1             # valid Fri-01-Jan - Fri-31-Dec, periods in weekday
    0    1   0.000   7.     # ctl type, law (basic control), start @, data items
  100000.000 0.000 0.000 0.000 21.000 100.000 0.000  # basic control: heating capacity 100000.0W. cooling capacity 0.0W. heating setpoint 21.00C cooling setpoint 100.00C.
# Function:Zone links
 1,0,1,1,1,0
*plant
*pdoc no plant control description supplied
   3                        # number of loops
*loop   1 plant_loop_01
   -1    3    1    0    0  # senses var in compt.  3:dhw_tank @ node no.  1
   -1    3    1    0        # actuates plant component   3:dhw_tank @ node no.  1
    1                       # all day types have same control
    1  365    1             # valid Fri-01-Jan - Fri-31-Dec, periods in weekday
   12    8   0.000   7.     # ctl type, law (On-Off control.), start @, data items
  1.00000 54.00000 56.00000 1.00000 0.00000 0.00000 0.00000  # OnOff std mode 1.0 on setpt 54.00C off setpt 56.00C output @ hi 1.000 output @ lo 0.000 sensor lag 0.0s actuator lag 0.0s.
*loop   2 plant_loop_02
   -1    1    1    4    1  # sen var diff bet compt.  1:solar_collector @ node  1and compt  4:solar_tank @ node  1
   -1    2    1    0        # actuates plant component   2:collector_pump @ node no.  1
    1                       # all day types have same control
    1  365    1             # valid Fri-01-Jan - Fri-31-Dec, periods in weekday
   24    8   0.000   7.     # ctl type, law (On-Off control.), start @, data items
  1.00000 1.00000 5.55000 0.00000 0.00004 0.00000 0.00000  # OnOff std mode 1.0 on setpt 1.00C off setpt 5.55C output @ hi 0.000 output @ lo 0.000 sensor lag 0.0s actuator lag 0.0s.
*loop   3 plant_loop_03
   -1    1    1    4    1  # sen var diff bet compt.  1:solar_collector @ node  1and compt  4:solar_tank @ node  1
   -1    6    1    0        # actuates plant component   6:tank_pump @ node no.  1
    1                       # all day types have same control
    1  365    1             # valid Fri-01-Jan - Fri-31-Dec, periods in weekday
   24    8   0.000   7.     # ctl type, law (On-Off control.), start @, data items
  1.00000 1.00000 5.55000 0.00000 0.00004 0.00000 0.00000  # OnOff std mode 1.0 on setpt 1.00C off setpt 5.55C output @ hi 0.000 output @ lo 0.000 sensor lag 0.0s actuator lag 0.0s.
