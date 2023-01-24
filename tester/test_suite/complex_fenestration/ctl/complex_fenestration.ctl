*CONTROL
*cdoc typical air based environmental control with complex fenistration controlled.
*building
*zdoc each zone gets a zone-specific controller.
*scope HEATCOOL    
   3                        # number of loops
*loop   1 bld_loop_01
    1    0    0    0        # senses dry bulb temperature in zone1.
    1    0    0             # actuates the air point in zone1.
    1                       # all day types have same control
    1  365    1             # valid Mon-01-Jan - Mon-31-Dec, periods in weekdays
    0    1   0.000   7.     # ctl type, law (basic control), start @, data items
  10000.000 0.000 10000.000 0.000 20.000 24.000 0.000  # basic control: heating capacity 10000.0W. cooling capacity 10000.0W. heating setpoint 20.00C cooling setpoint 24.00C.
*loop   2 bld_loop_02
    2    0    0    0        # senses dry bulb temperature in zone2.
    2    0    0             # actuates the air point in zone2.
    1                       # all day types have same control
    1  365    1             # valid Mon-01-Jan - Mon-31-Dec, periods in weekdays
    0    1   0.000   7.     # ctl type, law (basic control), start @, data items
  10000.000 0.000 10000.000 0.000 20.000 24.000 0.000  # basic control: heating capacity 10000.0W. cooling capacity 10000.0W. heating setpoint 20.00C cooling setpoint 24.00C.
*loop   3 bld_loop_03
    3    0    0    0        # senses dry bulb temperature in zone3.
    3    0    0             # actuates the air point in zone3.
    1                       # all day types have same control
    1  365    1             # valid Mon-01-Jan - Mon-31-Dec, periods in weekdays
    0    1   0.000   7.     # ctl type, law (basic control), start @, data items
  10000.000 0.000 10000.000 0.000 20.000 100.000 0.000  # basic control: heating capacity 10000.0W. cooling capacity 10000.0W. heating setpoint 20.00C cooling setpoint 100.00C.
# Function:Zone links
 1,2,3
*cfc
*cfcdoc no complex fen. control description supplied
   6                        # number of loops
*loop   1 CFC_loop_01
   -4    1    1    0        # senses incident sol rad on Wall-1 in zone1.
    0    1    2             # actuates Shading ON/OFF in CFC type  2 in zone1.
    1                       # all day types have same control
    1  365    1             # valid Mon-01-Jan - Mon-31-Dec, periods in weekdays
    3    1   0.000   2.     # ctl type, law (basic control), start @, data items
200.000 150.000             # Basic control: [C] shade ON (close) setpoint 200.0[C] shade OFF (open) setpoint 150.0
*loop   2 CFC_loop_02
   -4    1    1    0        # senses incident sol rad on Wall-1 in zone1.
    0    1    3             # actuates Shading ON/OFF in CFC type  3 in zone1.
    1                       # all day types have same control
    1  365    1             # valid Mon-01-Jan - Mon-31-Dec, periods in weekdays
    3    1   0.000   2.     # ctl type, law (basic control), start @, data items
200.000 150.000             # Basic control: [C] shade ON (close) setpoint 200.0[C] shade OFF (open) setpoint 150.0
*loop   3 CFC_loop_03
    0    0    0    0        # no sensor - schedule only.
    2    2    1             # actuates Shade ON/OFF and slat angle(schedule)  1 in zone2.
    0                       # day types follow calendar  3
    1  365    3             # valid Mon-01-Jan - Mon-31-Dec, periods in weekdays
    7    2   0.000   2.     # ctl type, law (schedule), start @, data items
0.000 45.000                # Schedule: shade ON (1) or OFF (0) 0[deg] slat angle 45.0
    7    2  12.000   2.     # ctl type, law (schedule), start @, data items
1.000 89.000                # Schedule: shade ON (1) or OFF (0) 1[deg] slat angle 89.0
    7    2  15.000   2.     # ctl type, law (schedule), start @, data items
0.000 45.000                # Schedule: shade ON (1) or OFF (0) 0[deg] slat angle 45.0
    1  365    1             # valid Mon-01-Jan - Mon-31-Dec, periods in saturday
    7    2   0.000   2.     # ctl type, law (schedule), start @, data items
1.000 89.000                # Schedule: shade ON (1) or OFF (0) 1[deg] slat angle 89.0
    1  365    1             # valid Mon-01-Jan - Mon-31-Dec, periods in sunday
    7    2   0.000   2.     # ctl type, law (schedule), start @, data items
0.000 45.000                # Schedule: shade ON (1) or OFF (0) 0[deg] slat angle 45.0
*loop   4 CFC_loop_04
    0    0    0    0        # no sensor - schedule only.
    2    2    2             # actuates Shade ON/OFF and slat angle(schedule)  2 in zone2.
    0                       # day types follow calendar  3
    1  365    3             # valid Mon-01-Jan - Mon-31-Dec, periods in weekdays
    7    2   0.000   2.     # ctl type, law (schedule), start @, data items
0.000 45.000                # Schedule: shade ON (1) or OFF (0) 0[deg] slat angle 45.0
    7    2  12.000   2.     # ctl type, law (schedule), start @, data items
1.000 89.000                # Schedule: shade ON (1) or OFF (0) 1[deg] slat angle 89.0
    7    2  15.000   2.     # ctl type, law (schedule), start @, data items
0.000 45.000                # Schedule: shade ON (1) or OFF (0) 0[deg] slat angle 45.0
    1  365    1             # valid Mon-01-Jan - Mon-31-Dec, periods in saturday
    7    2   0.000   2.     # ctl type, law (schedule), start @, data items
1.000 89.000                # Schedule: shade ON (1) or OFF (0) 1[deg] slat angle 89.0
    1  365    1             # valid Mon-01-Jan - Mon-31-Dec, periods in sunday
    7    2   0.000   2.     # ctl type, law (schedule), start @, data items
0.000 45.000                # Schedule: shade ON (1) or OFF (0) 0[deg] slat angle 45.0
*loop   5 CFC_loop_05
    3    0    0    0        # senses dry bulb temperature in zone3.
    1    3    1             # actuates Slat angle in CFC type  1 in zone3.
    2                       # uses dates of validity
    1  153    1             # valid Mon-01-Jan - Sat-02-Jun, periods in weekdays
    2    1   0.000   4.     # ctl type, law (basic control), start @, data items
15.000 12.000 89.000 0.000  # Basic control: [C] shade ON (close) setpoint 15.0[C] shade OFF (open) setpoint 12.0[deg] close slat angle 89.0[deg] open sl
  154  365    1             # valid Sun-03-Jun - Mon-31-Dec, periods in saturday
    2    1   0.000   4.     # ctl type, law (basic control), start @, data items
26.000 25.000 89.000 0.000  # Basic control: [C] shade ON (close) setpoint 26.0[C] shade OFF (open) setpoint 25.0[deg] close slat angle 89.0[deg] open sl
*loop   6 CFC_loop_06
    3    0    0    0        # senses dry bulb temperature in zone3.
    1    3    2             # actuates Slat angle in CFC type  2 in zone3.
    2                       # uses dates of validity
    1  153    1             # valid Mon-01-Jan - Sat-02-Jun, periods in weekdays
    2    1   0.000   4.     # ctl type, law (basic control), start @, data items
15.000 12.000 89.000 0.000  # Basic control: [C] shade ON (close) setpoint 15.0[C] shade OFF (open) setpoint 12.0[deg] close slat angle 89.0[deg] open sl
  154  365    1             # valid Sun-03-Jun - Mon-31-Dec, periods in saturday
    2    1   0.000   4.     # ctl type, law (basic control), start @, data items
26.000 25.000 89.000 0.000  # Basic control: [C] shade ON (close) setpoint 26.0[C] shade OFF (open) setpoint 25.0[deg] close slat angle 89.0[deg] open sl
