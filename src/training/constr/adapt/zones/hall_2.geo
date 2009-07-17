*Geometry 1.1,GEN,hall_2 # tag version, format, zone name
*date Fri Jul 17 13:33:43 2009  # latest file modification 
hall_2 describes the upper level stair landing
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,4.50000,5.00000,2.20000  #   1
*vertex,3.00000,5.00000,2.20000  #   2
*vertex,3.00000,3.00000,2.20000  #   3
*vertex,6.00000,3.00000,2.20000  #   4
*vertex,6.00000,4.00000,2.20000  #   5
*vertex,4.50000,4.00000,2.20000  #   6
*vertex,4.50000,5.00000,4.40000  #   7
*vertex,3.00000,5.00000,4.40000  #   8
*vertex,3.00000,3.00000,4.40000  #   9
*vertex,6.00000,3.00000,4.40000  #  10
*vertex,6.00000,4.00000,4.40000  #  11
*vertex,4.50000,4.00000,4.40000  #  12
*vertex,3.00000,4.00000,2.20000  #  13
*vertex,3.00000,4.00000,4.40000  #  14
*vertex,4.50000,3.00000,2.20000  #  15
*vertex,4.00000,3.00000,2.20000  #  16
*vertex,4.00000,3.00000,4.40000  #  17
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,8,7  #  1
*edges,4,2,13,14,8  #  2
*edges,5,16,15,4,10,17  #  3
*edges,4,4,5,11,10  #  4
*edges,4,5,6,12,11  #  5
*edges,4,6,1,7,12  #  6
*edges,8,7,8,14,9,17,10,11,12  #  7
*edges,7,6,15,16,3,13,2,1  #  8
*edges,4,13,3,9,14  #  9
*edges,4,6,5,4,15  # 10
*edges,4,3,16,17,9  # 11
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,door_bath,VERT,-,-,-,int_doors,OPAQUE,ANOTHER,06,10  #   1 ||< door_hall2:bathrm
*surf,door_bed1,VERT,-,-,-,int_doors,OPAQUE,ANOTHER,07,07  #   2 ||< door_hall2:bed1
*surf,bed3,VERT,-,-,-,int_part,OPAQUE,ANOTHER,11,01  #   3 ||< hall2:bed3
*surf,east,VERT,-,-,-,ext_part,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,w_heater_c,VERT,-,-,-,int_part,OPAQUE,ANOTHER,08,02  #   5 ||< hall2:w_heat_c
*surf,door_whc,VERT,-,-,-,int_doors,OPAQUE,ANOTHER,08,01  #   6 ||< door_hall2:w_heat_c
*surf,ceiling,CEIL,-,-,-,loft_ceil,OPAQUE,ANOTHER,12,08  #   7 ||< hall2:loft
*surf,hall1,FLOR,-,-,-,upper_flor,OPAQUE,ANOTHER,03,05  #   8 ||< hall_2:hall_1
*surf,door_bed2,VERT,-,-,-,int_doors,OPAQUE,ANOTHER,10,01  #   9 ||< door_hall2:bed2
*surf,stairs,FLOR,-,-,-,int_doors,OPAQUE,ANOTHER,02,08  #  10 ||< hall2:stairs
*surf,door_bed3,VERT,-,-,-,int_doors,OPAQUE,ANOTHER,11,08  #  11 ||< door_hall2:bed3
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,2,8,10,     4.50 0  # zone base list
