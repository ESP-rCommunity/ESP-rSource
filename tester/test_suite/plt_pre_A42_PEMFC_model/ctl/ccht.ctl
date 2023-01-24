*CONTROL
*cdoc control for CCHT house
*building
*zdoc mix of floating and ideal control and plant coupling.
*scope HEATCOOL    
   3                        # number of loops
*loop   1 floating
    0    0    0    0        # senses the temperature of the current zone.
    0    0    0             # actuates air point of the current zone
    1                       # all day types have same control
    1  365    1             # valid Fri-01-Jan - Fri-31-Dec, periods in weekday
    0    2   0.000   0.     # ctl type, law (free floating), start @, data items
*loop   2 bld_loop_02
    0    0    0    0        # senses the temperature of the current zone.
    0    0    0             # actuates air point of the current zone
    3                       # uses dates of validity
    1  120    1             # valid Fri-01-Jan - Fri-30-Apr, periods in weekday
    0    1   0.000   7.     # ctl type, law (basic control), start @, data items
  0.000 0.000 0.000 0.000 0.000 100.000 0.000  # basic control: heating capacity 0.0W. cooling capacity 0.0W. heating setpoint 0.00C cooling setpoint 100.00C.
  121  273    1             # valid Sat-01-May - Thu-30-Sep, periods in saturday
    0    1   0.000   7.     # ctl type, law (basic control), start @, data items
  0.000 0.000 0.000 0.000 0.000 100.000 0.000  # basic control: heating capacity 0.0W. cooling capacity 0.0W. heating setpoint 0.00C cooling setpoint 100.00C.
  274  365    1             # valid Fri-01-Oct - Fri-31-Dec, periods in sunday
    0    1   0.000   7.     # ctl type, law (basic control), start @, data items
  0.000 0.000 0.000 0.000 20.000 100.000 0.000  # basic control: heating capacity 0.0W. cooling capacity 0.0W. heating setpoint 20.00C cooling setpoint 100.00C.
*loop   3 coupling to plant
    2    0    0    0        # senses dry bulb temperature in main_second.
    2    0    0             # actuates the air point in main_second.
    3                       # uses dates of validity
    1  120    1             # valid Fri-01-Jan - Fri-30-Apr, periods in weekday
    0    6   0.000   7.     # ctl type, law (flux zone/plant), start @, data items
  6.000 2.000 1.000 99000.000 99000.000 10.000 1.000  # plant/zone coupling: source plant component 6 plant component node 2 coupling type mCp(0s-0a) sequential. Max heating 99000.00W max cooling 99000.00W. Extract plant component 10 and extract node 1.
  121  273    1             # valid Sat-01-May - Thu-30-Sep, periods in saturday
    0    1   0.000   7.     # ctl type, law (basic control), start @, data items
  0.000 0.000 0.000 0.000 0.000 100.000 0.000  # basic control: heating capacity 0.0W. cooling capacity 0.0W. heating setpoint 0.00C cooling setpoint 100.00C.
  274  365    1             # valid Fri-01-Oct - Fri-31-Dec, periods in sunday
    0    6   0.000   7.     # ctl type, law (flux zone/plant), start @, data items
  6.000 2.000 1.000 99000.000 99000.000 10.000 1.000  # plant/zone coupling: source plant component 6 plant component node 2 coupling type mCp(0s-0a) sequential. Max heating 99000.00W max cooling 99000.00W. Extract plant component 10 and extract node 1.
# Function:Zone links
 1,3,1,2
*plant
*pdoc no descrip
   4                        # number of loops
*loop   1 plant_loop_01
   -1    2    1    0    0  # senses var in compt.  2:hot-water-tank @ node no.  1
   -1    2    2    0        # actuates plant component   2:hot-water-tank @ node no.  2
    1                       # all day types have same control
    1  365    1             # valid Fri-01-Jan - Fri-31-Dec, periods in weekday
   12    8   0.000   7.     # ctl type, law (On-Off control.), start @, data items
  1.00000 50.00000 60.00000 1.00000 0.00000 0.00000 0.00000  # OnOff std mode 1.0 on setpt 50.00C off setpt 60.00C output @ hi 1.000 output @ lo 0.000 sensor lag 0.0s actuator lag 0.0s.
*loop   2 plant_loop_02
    2    0    0    0    0  # senses dry bulb temperature in main_second.
   -1    5    1    0        # actuates plant component   5:coil-fan @ node no.  1
    3                       # uses dates of validity
    1  120    1             # valid Fri-01-Jan - Fri-30-Apr, periods in weekday
    1    8   0.000   7.     # ctl type, law (On-Off control.), start @, data items
  1.00000 19.50000 20.50000 0.31000 0.00000 0.00000 0.00000  # OnOff std mode 1.0 on setpt 19.50C off setpt 20.50C output @ hi 0.310 output @ lo 0.000 sensor lag 0.0s actuator lag 0.0s.
  121  273    1             # valid Sat-01-May - Thu-30-Sep, periods in saturday
    1    8   0.000   7.     # ctl type, law (On-Off control.), start @, data items
  1.00000 10.00000 50.00000 0.31000 0.00000 0.00000 0.00000  # OnOff std mode 1.0 on setpt 10.00C off setpt 50.00C output @ hi 0.310 output @ lo 0.000 sensor lag 0.0s actuator lag 0.0s.
  274  365    1             # valid Fri-01-Oct - Fri-31-Dec, periods in sunday
    1    8   0.000   7.     # ctl type, law (On-Off control.), start @, data items
  1.00000 19.50000 20.50000 0.31000 0.00000 0.00000 0.00000  # OnOff std mode 1.0 on setpt 19.50C off setpt 20.50C output @ hi 0.310 output @ lo 0.000 sensor lag 0.0s actuator lag 0.0s.
*loop   3 plant_loop_03
    2    0    0    0    0  # senses dry bulb temperature in main_second.
   -1    4    1    0        # actuates plant component   4:coil-pump @ node no.  1
    3                       # uses dates of validity
    1  120    1             # valid Fri-01-Jan - Fri-30-Apr, periods in weekday
    0    8   0.000   7.     # ctl type, law (On-Off control.), start @, data items
  1.00000 19.50000 20.50000 0.00030 0.00000 0.00000 0.00000  # OnOff std mode 1.0 on setpt 19.50C off setpt 20.50C output @ hi 0.000 output @ lo 0.000 sensor lag 0.0s actuator lag 0.0s.
  121  273    1             # valid Sat-01-May - Thu-30-Sep, periods in saturday
    1    8   0.000   7.     # ctl type, law (On-Off control.), start @, data items
  1.00000 10.00000 50.00000 0.00030 0.00000 0.00000 0.00000  # OnOff std mode 1.0 on setpt 10.00C off setpt 50.00C output @ hi 0.000 output @ lo 0.000 sensor lag 0.0s actuator lag 0.0s.
  274  365    1             # valid Fri-01-Oct - Fri-31-Dec, periods in sunday
    0    8   0.000   7.     # ctl type, law (On-Off control.), start @, data items
  1.00000 19.50000 20.50000 0.00030 0.00000 0.00000 0.00000  # OnOff std mode 1.0 on setpt 19.50C off setpt 20.50C output @ hi 0.000 output @ lo 0.000 sensor lag 0.0s actuator lag 0.0s.
*loop   4 plant_loop_04
   -1    2    1    0    0  # senses var in compt.  2:hot-water-tank @ node no.  1
   -1    2    2    0        # actuates plant component   2:hot-water-tank @ node no.  2
    1                       # all day types have same control
    1  365    1             # valid Fri-01-Jan - Fri-31-Dec, periods in weekday
   12   10   0.000  18.     # ctl type, law (Cogen. controller), start @, data items
  2700.00000 2.00000 2.00000 1.00000 -1.00000 -1.00000 1.00000 365.00000 3.00000 62.00000 72.00000 0.80000 0.80000 0.00004 0.00000 2.00000 0.25000 1.00000  # not yet coded for system controls.
