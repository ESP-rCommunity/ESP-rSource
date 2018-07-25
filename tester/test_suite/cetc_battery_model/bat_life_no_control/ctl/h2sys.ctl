# Comments are denoted by the number sign and are ignored.
# This file contains inputs required by the residential hydrogen cogen
# controller (subroutine RESH2_Ctl).
# Inputs required by controller in the following order:
3                       # index number of PCU that is connected between the PV and the DC bus
3                       # node number of the AC bus
5                       # index number of the grid-connection power-only component
6                       # index number of the DC-AC converter (a PCU power-only component)
2                       # index number of the battery power-only component
125                      # battery SOC at which electrolyzer turns on
100                      # battery SOC at which electrolyzer switches off
-20                      # battery SOC at which PEM fuel cell switches on
20                      # battery SOC at which PEM fuel cell switches off
1                       # index number of the electrolyzer plant component
2                       # index number of the PEM fuel cell plant component
5                       # index number of the PCU that is connected between the PEMFC and the DC bus
4                       # index number of the PCU that is connected between the electrolyzer and DC bus
5                       # index number of the electrolyzer cooling pump plant component
6                       # index number of the PEMFC cooling pump plant component
0.0003                  # flow rate of the electrolyzer cooling pump (m3/s)
0.0003                  # flow rate of the PEMFC cooling pump (m3/s)
400.                    # Controller deadband for H2 production (W)


