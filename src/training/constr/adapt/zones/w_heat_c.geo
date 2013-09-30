*Geometry 1.1,GEN,w_heat_c # tag version, format, zone name
*date Fri Jul 17 13:33:43 2009  # latest file modification 
w_heat_c describes the boiler space
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,4.50000,5.00000,2.20000  #   1
*vertex,4.50000,4.00000,2.20000  #   2
*vertex,6.00000,4.00000,2.20000  #   3
*vertex,6.00000,5.00000,2.20000  #   4
*vertex,4.50000,5.00000,4.40000  #   5
*vertex,4.50000,4.00000,4.40000  #   6
*vertex,6.00000,4.00000,4.40000  #   7
*vertex,6.00000,5.00000,4.40000  #   8
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,4,5,6,7,8  #  5
*edges,4,4,3,2,1  #  6
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,door_hall2,VERT,-,-,-,int_doors,OPAQUE,ANOTHER,09,06  #   1 ||< door_whc:hall_2
*surf,hall2,VERT,-,-,-,int_part,OPAQUE,ANOTHER,09,05  #   2 ||< w_heater_c:hall_2
*surf,east,VERT,-,-,-,ext_part,OPAQUE,SIMILAR,00,00  #   3 ||< identical environment
*surf,bathrm,VERT,-,-,-,int_part,OPAQUE,ANOTHER,06,03  #   4 ||< w_heat_c:bathrm
*surf,loft,CEIL,-,-,-,loft_ceil,OPAQUE,ANOTHER,12,09  #   5 ||< w_heat_c:loft
*surf,stairs,FLOR,-,-,-,upper_flor,OPAQUE,ANOTHER,02,07  #   6 ||< w_heat_c:stairs
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,     1.50 0  # zone base list
