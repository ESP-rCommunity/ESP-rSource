*CONTROL
*cdoc Ideal control for dual office model. Weekdays normal office hours, saturday reduced occupied hours, sunday stand-by only. One person per office, 100W lighting and 150W small power.
*building
*zdoc this is a base case set of assumptions and also includes flow controls and global controls.
*scope HEATCOOL    
   1                        # number of loops
*loop   1 bld_loop_01
    0    0    0    0        # senses the temperature of the current zone.
    0    0    0             # actuates air point of the current zone
    0                       # day types follow calendar  3
    1  365    3             # valid Sun-01-Jan - Sun-31-Dec, periods in weekdays
    0    1   0.000   7.     # ctl type, law (basic control), start @, data items
  1000.000 0.000 300.000 0.000 15.000 25.000 0.000  # basic control: heating capacity 1000.0W. cooling capacity 300.0W. heating setpoint 15.00C cooling setpoint 25.00C.
    0    1   6.000   7.     # ctl type, law (basic control), start @, data items
  1000.000 0.000 300.000 0.000 19.000 24.000 0.000  # basic control: heating capacity 1000.0W. cooling capacity 300.0W. heating setpoint 19.00C cooling setpoint 24.00C.
    0    1  18.000   7.     # ctl type, law (basic control), start @, data items
  1000.000 0.000 300.000 0.000 15.000 25.000 0.000  # basic control: heating capacity 1000.0W. cooling capacity 300.0W. heating setpoint 15.00C cooling setpoint 25.00C.
    1  365    3             # valid Sun-01-Jan - Sun-31-Dec, periods in saturday
    0    1   0.000   7.     # ctl type, law (basic control), start @, data items
  1000.000 0.000 300.000 0.000 15.000 25.000 0.000  # basic control: heating capacity 1000.0W. cooling capacity 300.0W. heating setpoint 15.00C cooling setpoint 25.00C.
    0    1   9.000   7.     # ctl type, law (basic control), start @, data items
  1000.000 0.000 300.000 0.000 19.000 24.000 0.000  # basic control: heating capacity 1000.0W. cooling capacity 300.0W. heating setpoint 19.00C cooling setpoint 24.00C.
    0    2  17.000   0.     # ctl type, law (free floating), start @, data items
    1  365    1             # valid Sun-01-Jan - Sun-31-Dec, periods in sunday
    0    1   0.000   7.     # ctl type, law (basic control), start @, data items
  1000.000 0.000 300.000 0.000 10.000 30.000 0.000  # basic control: heating capacity 1000.0W. cooling capacity 300.0W. heating setpoint 10.00C cooling setpoint 30.00C.
# Function:Zone links
 1,1,1
*mass flow
*fdoc no flow control description supplied
   6                        # number of loops
*loop   1 flow_loop_01
   -4    1    0    0        # senses node (1) manager_a
   -3    8    0             # actuates flow connection:   8 corid_left - manager_a via fan_8lps
    0                       # day types follow calendar  3
    1  365    3             # valid Sun-01-Jan - Sun-31-Dec, periods in weekdays
    1    0   0.000   3.     # type (dry bulb > flow), law (on / off), start@
50.00000 1.00000 1.00000    # on/off setpoint 50.00 direct action ON fraction 1.000.
    1    0   8.500   3.     # type (dry bulb > flow), law (on / off), start@
-50.00000 1.00000 1.00000  # on/off setpoint -50.00 direct action ON fraction 1.000.
    1    0  17.500   3.     # type (dry bulb > flow), law (on / off), start@
50.00000 1.00000 1.00000    # on/off setpoint 50.00 direct action ON fraction 1.000.
    1  365    1             # valid Sun-01-Jan - Sun-31-Dec, periods in saturday
    1    0   0.000   3.     # type (dry bulb > flow), law (on / off), start@
50.00000 1.00000 1.00000    # on/off setpoint 50.00 direct action ON fraction 1.000.
    1  365    1             # valid Sun-01-Jan - Sun-31-Dec, periods in sunday
    1    0   0.000   3.     # type (dry bulb > flow), law (on / off), start@
50.00000 1.00000 1.00000    # on/off setpoint 50.00 direct action ON fraction 1.000.
*loop   2 flow_loop_02
   -4    2    0    0        # senses node (2) manager_b
   -3    9    0             # actuates flow connection:   9 corid_left - manager_b via fan_8lps
    0                       # day types follow calendar  3
    1  365    3             # valid Sun-01-Jan - Sun-31-Dec, periods in weekdays
    1    0   0.000   3.     # type (dry bulb > flow), law (on / off), start@
50.00000 1.00000 1.00000    # on/off setpoint 50.00 direct action ON fraction 1.000.
    1    0   8.500   3.     # type (dry bulb > flow), law (on / off), start@
-50.00000 1.00000 1.00000  # on/off setpoint -50.00 direct action ON fraction 1.000.
    1    0  17.500   3.     # type (dry bulb > flow), law (on / off), start@
50.00000 1.00000 1.00000    # on/off setpoint 50.00 direct action ON fraction 1.000.
    1  365    1             # valid Sun-01-Jan - Sun-31-Dec, periods in saturday
    1    0   0.000   3.     # type (dry bulb > flow), law (on / off), start@
50.00000 1.00000 1.00000    # on/off setpoint 50.00 direct action ON fraction 1.000.
    1  365    1             # valid Sun-01-Jan - Sun-31-Dec, periods in sunday
    1    0   0.000   3.     # type (dry bulb > flow), law (on / off), start@
50.00000 1.00000 1.00000    # on/off setpoint 50.00 direct action ON fraction 1.000.
*loop   3 flow_loop_03
   -4    1    0    0        # senses node (1) manager_a
   -3   10    0             # actuates flow connection:  10 corid_left - manager_a via fan_8lps
    0                       # day types follow calendar  3
    1  365    3             # valid Sun-01-Jan - Sun-31-Dec, periods in weekdays
    1    0   0.000   3.     # type (dry bulb > flow), law (on / off), start@
50.00000 1.00000 1.00000    # on/off setpoint 50.00 direct action ON fraction 1.000.
    1    0   8.500   3.     # type (dry bulb > flow), law (on / off), start@
26.00000 1.00000 1.00000    # on/off setpoint 26.00 direct action ON fraction 1.000.
    1    0  17.500   3.     # type (dry bulb > flow), law (on / off), start@
50.00000 1.00000 1.00000    # on/off setpoint 50.00 direct action ON fraction 1.000.
    1  365    1             # valid Sun-01-Jan - Sun-31-Dec, periods in saturday
    1    0   0.000   3.     # type (dry bulb > flow), law (on / off), start@
50.00000 1.00000 1.00000    # on/off setpoint 50.00 direct action ON fraction 1.000.
    1  365    1             # valid Sun-01-Jan - Sun-31-Dec, periods in sunday
    1    0   0.000   3.     # type (dry bulb > flow), law (on / off), start@
50.00000 1.00000 1.00000    # on/off setpoint 50.00 direct action ON fraction 1.000.
*loop   4 flow_loop_04
   -4    2    0    0        # senses node (2) manager_b
   -3    3    0             # actuates flow connection:   3 man_blow - manager_b via crack
    0                       # day types follow calendar  3
    1  365    3             # valid Sun-01-Jan - Sun-31-Dec, periods in weekdays
    1    0   0.000   3.     # type (dry bulb > flow), law (on / off), start@
50.00000 1.00000 1.00000    # on/off setpoint 50.00 direct action ON fraction 1.000.
    1    0   8.500   3.     # type (dry bulb > flow), law (on / off), start@
26.00000 1.00000 1.00000    # on/off setpoint 26.00 direct action ON fraction 1.000.
    1    0  17.500   3.     # type (dry bulb > flow), law (on / off), start@
50.00000 1.00000 1.00000    # on/off setpoint 50.00 direct action ON fraction 1.000.
    1  365    1             # valid Sun-01-Jan - Sun-31-Dec, periods in saturday
    1    0   0.000   3.     # type (dry bulb > flow), law (on / off), start@
50.00000 1.00000 1.00000    # on/off setpoint 50.00 direct action ON fraction 1.000.
    1  365    1             # valid Sun-01-Jan - Sun-31-Dec, periods in sunday
    1    0   0.000   3.     # type (dry bulb > flow), law (on / off), start@
50.00000 1.00000 1.00000    # on/off setpoint 50.00 direct action ON fraction 1.000.
*loop   5 flow_loop_05
   -4    2    1    2        # senses node (2) manager_b, contaminant: CO2
   -3    4    0             # actuates flow connection:   4 man_bhi - manager_b via crack
    0                       # day types follow calendar  3
    1  365    3             # valid Sun-01-Jan - Sun-31-Dec, periods in weekdays
   41    0   0.000   3.     # type (contam conc > flow), law (on / off), start@
50.00000 1.00000 1.00000    # on/off setpoint 50.00 direct action ON fraction 1.000.
   41    0   8.500   3.     # type (contam conc > flow), law (on / off), start@
1.30000 1.00000 1.00000     # on/off setpoint 1.30 direct action ON fraction 1.000.
   41    0  17.500   3.     # type (contam conc > flow), law (on / off), start@
50.00000 1.00000 1.00000    # on/off setpoint 50.00 direct action ON fraction 1.000.
    1  365    1             # valid Sun-01-Jan - Sun-31-Dec, periods in saturday
   41    0   0.000   3.     # type (contam conc > flow), law (on / off), start@
50.00000 1.00000 1.00000    # on/off setpoint 50.00 direct action ON fraction 1.000.
    1  365    1             # valid Sun-01-Jan - Sun-31-Dec, periods in sunday
   41    0   0.000   3.     # type (contam conc > flow), law (on / off), start@
50.00000 1.00000 1.00000    # on/off setpoint 50.00 direct action ON fraction 1.000.
*loop   6 flow_loop_06
   -4    2    0    0        # senses node (2) manager_b
   -3   11    0             # actuates flow connection:  11 corid_left - manager_b via fan_8lps
    1                       # all day types have same control
    1  365    1             # valid Sun-01-Jan - Sun-31-Dec, periods in weekdays
    1    0   0.000   2.     # type (dry bulb > flow), law (on / off), start@
0.00000 1.00000             # on/off setpoint 0.00 direct action ON fraction 0.000.
*global
*gdoc no global control description supplied
   1                        # number of loops
*loop   1 global_loop_01
    0    0    0             # sensor data
    0    0    0             # actuator data
    0                       # day types follow calendar  3
    1  365    1             # valid Sun-01-Jan - Sun-31-Dec, periods in weekdays
    5    5   0.000   7.     # ctl type, law (Global multisensor), start @, data items
  1.000 4.000 2.000 1.000 5.000 1.000 6.000
    1  365    1             # valid Sun-01-Jan - Sun-31-Dec, periods in saturday
    0    2   0.000   0.     # ctl type, law (undefined law), start @, data items
    1  365    1             # valid Sun-01-Jan - Sun-31-Dec, periods in sunday
    0    2   0.000   0.     # ctl type, law (undefined law), start @, data items
