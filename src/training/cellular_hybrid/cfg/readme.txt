Exploration of a hybrid natural and mechanical vent design
Dr. Jon W. Hand
2 August 2011

Introduction

One ESP-r exemplar model (listed as technical features -> with natural and 
hybrid ventilation) demonstrates complementary control of natural ventilation 
and mechanical cooling in one zone and with only mechanical cooling in the 
other.  The model is shown in Figure 1.

< look in images folder for cellular_mctl_wire_big.gif >

Figure 1 the hybrid ventilation test model

The model comprises the standard side by side cellular offices with two 
additional zones added to represent abstract air handling units. In 
manager_a the air handling unit is turned off if the window is opened.  
In manager_b the air handling unit is available during offices hours if 
reqired and its window remains closed at all times.  The air handling 
unit zones are maintained at 16C and each has flow to and from the adjacent 
office if required to provide cooling.  Thus air movement is the way cooling 
is delivered into the offices.
 
In manager_a there is an upper and lower window opening included in the 
flow network.  This allows buoyancy to take part in the air flow.   There 
is also a crack under each door as well as a return grill between each 
office and the corridor. Both of the air handling units uses the same 
fan component to move air into their offices.

< look in images folder for ctl_logic.gif >

Figure 2  Control logic

The window opening logic is shown in Figure 2.  This is an example of a 
multi-sensor flow controller.  Two criteria are used: first the ambient 
temperature must be between 10 and 25C, secondly the room temperature must 
be over 21C.  This control is associated with the upper and lower portion 
of the window opening and with a ?normally closed? logic. The fan between 
the air handling zone and manager_a is assumed to be ?normally on? and 
tested against the same criteria. Thus if both conditions are true the 
window opens and the fan turns off.  If only one condition is true the 
fan continues to run.

The manager_b office fan is set to run during office hours (the flow rate 
is not tuned for comfort, simply to provide a difference against the 
other office.  

In practice, there are some hours when natural ventilation is sufficient 
to control temperatures in the room and other times when the criteria 
for the use of natural ventilation are not met and flow from the air 
handling unit is needed.  Figure 3 shows the performance on 6 August.  The 
black line is the ambient temperature, the solid blue line the temperature 
in manager_a and the dashed blue lines are the energy impact of air 
movement.  The dashed yellow line near the ?mix box on? shows the
Warm air returned to the air handling unit when the fan is on.

Figure 4 shows the differences in cooling of the AHU for manager_a and for 
manager_b.  Clearly the natural ventilation has reduced the cooling 
requirement during this assessment period.


< look in images folder for switching_manager_a.gif >

Figure 3  Switching between natural ventilation and mechanical cooling


The windows are opened soon after 8h00.  Between 14h30 and16h00 the fan 
is turned on (with a dip in the temperature of manager_a.  The cooling 
from the window drops off as the fan takes over.

< look in images folder for savings_via_natural_vent.gif >

Figure 4  Savings in cooling

In manager_b Figure 5 shows the fan is on during office hours and the 
heat transfer induced by the air movement to and from the mixing box 
zone is clearly seen.

Control loops

The control comprises one ideal zone control applied to the air handling 
zones to maintain them at 16C at all hours.  For the flow network there 
are six control loops. One for the manager_a low window, one for the 
manager_a high window, one for the fan from plant to manager_a.  The 
remaining control loops are associated with the windows and fan for manager_b 
to ensure the windows remain closed and the fan is on during office 
hours.  The control loops associated with manager_b could be altered easily 
to test alternative logic if required.


< look in images folder for switching_manager_b.gif >

Figure 5  Switching of mechanical cooling in manager_b

Notes:

The 6th of August is the test day for the simulation
which shows the clearest case of hybrid ventilation.

If you invoke run.sh it will start of the script
win_run.sh which does a demonstration day analysis
and then reports data on the infiltration and
ventilation.
