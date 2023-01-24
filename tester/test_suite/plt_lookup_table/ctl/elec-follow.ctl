*CONTROL
*cdoc demo A
*building
*zdoc plant injects heat to main
*scope HEATCOOL    
   2                        # number of loops
*loop   1 floating
    0    0    0    0        # senses the temperature of the current zone.
    0    0    0             # actuates air point of the current zone
    1                       # all day types have same control
    1  365    1             # valid Fri-01-Jan - Fri-31-Dec, periods in weekday
    0    2   0.000   0.     # ctl type, law (free floating), start @, data items
*loop   2 coupling plt cmp 6
    0    0    0    0        # senses the temperature of the current zone.
    0    0    0             # actuates air point of the current zone
    1                       # all day types have same control
    1  365    1             # valid Fri-01-Jan - Fri-31-Dec, periods in weekday
    0    6   0.000   7.     # ctl type, law (flux zone/plant), start @, data items
  6.000 2.000 5.000 99000.000 99000.000 5.000 1.000  # plant/zone coupling: source plant component 6 plant component node 2 coupling type mCp(0s-0a) concurrent. Max heating 99000.00W max cooling 99000.00W. Extract plant component 5 and extract node 1.
# Function:Zone links
 1,2,1,1
*plant
*pdoc DG controller
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
    1                       # all day types have same control
    1  365    1             # valid Fri-01-Jan - Fri-31-Dec, periods in weekday
    1    8   0.000   7.     # ctl type, law (On-Off control.), start @, data items
  1.00000 19.50000 20.50000 0.31000 0.00000 0.00000 0.00000  # OnOff std mode 1.0 on setpt 19.50C off setpt 20.50C output @ hi 0.310 output @ lo 0.000 sensor lag 0.0s actuator lag 0.0s.
*loop   3 plant_loop_03
    2    0    0    0    0  # senses dry bulb temperature in main_second.
   -1    4    1    0        # actuates plant component   4:coil-pump @ node no.  1
    1                       # all day types have same control
    1  365    1             # valid Fri-01-Jan - Fri-31-Dec, periods in weekday
    1    8   0.000   7.     # ctl type, law (On-Off control.), start @, data items
  1.00000 19.50000 20.50000 0.00030 0.00000 0.00000 0.00000  # OnOff std mode 1.0 on setpt 19.50C off setpt 20.50C output @ hi 0.000 output @ lo 0.000 sensor lag 0.0s actuator lag 0.0s.
*loop   4 plant_loop_04
   -1    2    1    0    0  # senses var in compt.  2:hot-water-tank @ node no.  1
   -1    2    2    0        # actuates plant component   2:hot-water-tank @ node no.  2
    1                       # all day types have same control
    1  365    1             # valid Fri-01-Jan - Fri-31-Dec, periods in weekday
   12   10   0.000  18.     # ctl type, law (Cogen. controller), start @, data items
  2000.00000 2.00000 2.00000 1.00000 -1.00000 -1.00000 1.00000 365.00000 2.00000 62.00000 72.00000 0.80000 0.80000 0.00004 0.00000 2.00000 0.25000 1.00000  # not yet coded for system controls.
