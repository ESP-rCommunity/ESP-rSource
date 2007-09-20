control for T_S_Door2WindowsL  # overall descr 
* Building
building cntl fn model  # bld descr
   2  # No. of functions
* Control Function # 1[main] [Foundation] 
# senses dry bulb temperature in main
    0    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0    0  # actuator data
    1 # No. day types
# ++++++ Day Type: 1 +++++
    1  365  # valid Thu  1 Jan - Thu 31 Dec
     4  # No. of periods in day
#    >>>>Day Type: 1 - Period: 1<<<<
    0     1  0.000  # ctl type, law (basic control), start @
7  # No. of data items
7500.000 0.000 0.000 0.000 21.000 100.000 0.000   #  MaxHtgCap/MinHtgCap/MaxClCap/MinClCap/HtgStPt/ClStPt/
#    >>>>Day Type: 1 - Period: 2<<<<
    0     1  6.000  # ctl type, law (basic control), start @
7  # No. of data items
7500.000 0.000 0.000 0.000 21.000 100.000 0.000   #  MaxHtgCap/MinHtgCap/MaxClCap/MinClCap/HtgStPt/ClStPt/
#    >>>>Day Type: 1 - Period: 3<<<<
    0     1  16.000  # ctl type, law (basic control), start @
7  # No. of data items
7500.000 0.000 0.000 0.000 21.000 100.000 0.000   #  MaxHtgCap/MinHtgCap/MaxClCap/MinClCap/HtgStPt/ClStPt/
#    >>>>Day Type: 1 - Period: 4<<<<
    0     1  23.000  # ctl type, law (basic control), start @
7  # No. of data items
7500.000 0.000 0.000 0.000 21.000 100.000 0.000   #  MaxHtgCap/MinHtgCap/MaxClCap/MinClCap/HtgStPt/ClStPt/
# END  Control Function   # 1
* Control Function # 2[attic] 
# senses dry bulb temperature in Foundation
    0    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0    0  # actuator data
    1 # No. day types
# ++++++ Day Type: 1 +++++
    1  365  # valid Thu  1 Jan - Thu 31 Dec
     4  # No. of periods in day
#    >>>>Day Type: 1 - Period: 1<<<<
    0     2  0.000  # ctl type, law (free floating), start @
0  # No. of data items
#    >>>>Day Type: 1 - Period: 2<<<<
    0     2  6.000  # ctl type, law (free floating), start @
0  # No. of data items
#    >>>>Day Type: 1 - Period: 3<<<<
    0     2  16.000  # ctl type, law (free floating), start @
0  # No. of data items
#    >>>>Day Type: 1 - Period: 4<<<<
    0     2  23.000  # ctl type, law (free floating), start @
0  # No. of data items
# END  Control Function   # 2
# Function:Zone links
1 1 2 
