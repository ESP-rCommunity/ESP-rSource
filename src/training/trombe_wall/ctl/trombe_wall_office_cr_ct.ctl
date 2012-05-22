open upper damper if trombe_up is at least 5 deg over office T
* Mass Flow
try different delta T weekdays sat and sun
   1  # No. of controls
* Control mass    1
# sensors are nodes trombe_up (  4) & office (  6)
   -4    4    6    0  # sensor data
# actuates flow connection:  12 trombe_up - office via small_dampe
   -3   12    6  # actuator data
    0 # day types follow calendar  3
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: weekdays    
   24    0   0.000  # ctl type (delt T > flow), law (on/off setpoint 5.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  5.00000 1.00000 1.00000
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: saturday    
   24    0   0.000  # ctl type (delt T > flow), law (on/off setpoint 8.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  8.00000 1.00000 1.00000
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: sunday      
   24    0   0.000  # ctl type (delt T > flow), law (on/off setpoint 3.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  3.00000 1.00000 1.00000
