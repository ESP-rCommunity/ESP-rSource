*CONTROL
*cdoc control for CCHT house
*building
*zdoc mix of floating / controlled / plant linkage
*scope HEATCOOL    
   3                        # number of loops
*loop   1 float
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
*loop   3 coupling plt cmp
    0    0    0    0        # senses the temperature of the current zone.
    0    0    0             # actuates air point of the current zone
    1                       # all day types have same control
    1  365    1             # valid Fri-01-Jan - Fri-31-Dec, periods in weekday
    0    6   0.000   7.     # ctl type, law (flux zone/plant), start @, data items
  3.000 2.000 5.000 99000.000 0.000 2.000 1.000  # plant/zone coupling: source plant component 3 plant component node 2 coupling type mCp(0s-0a) concurrent. Max heating 99000.00W max cooling 0.00W. Extract plant component 2 and extract node 1.
# Function:Zone links
 2,1,3,2,1,1
*plant
*pdoc no descrip
   2                        # number of loops
*loop   1 plant_loop_01
   -1    1    1    0    0  # senses var in compt.  1:elec_tank @ node no.  1
   -1    1    1    0        # actuates plant component   1:elec_tank @ node no.  1
    1                       # all day types have same control
    1  365    1             # valid Fri-01-Jan - Fri-31-Dec, periods in weekday
    1    8   0.000   7.     # ctl type, law (On-Off control.), start @, data items
  1.00000 58.00000 60.00000 1.00000 0.00000 0.00000 0.00000  # OnOff std mode 1.0 on setpt 58.00C off setpt 60.00C output @ hi 1.000 output @ lo 0.000 sensor lag 0.0s actuator lag 0.0s.
*loop   2 plant_loop_02
    3    0    0    0    0  # senses dry bulb temperature in mainfloor.
   -1    4    1    0        # actuates plant component   4:pump @ node no.  1
    1                       # all day types have same control
    1  365    1             # valid Fri-01-Jan - Fri-31-Dec, periods in weekday
    1    8   0.000   7.     # ctl type, law (On-Off control.), start @, data items
  1.00000 20.00000 21.00000 0.00050 0.00000 0.00000 0.00000  # OnOff std mode 1.0 on setpt 20.00C off setpt 21.00C output @ hi 0.001 output @ lo 0.000 sensor lag 0.0s actuator lag 0.0s.
