*Geometry 1.1,GEN,kitchen # tag version, format, zone name
*date Fri Jul 17 13:33:43 2009  # latest file modification 
kitchen describes a room adjacent to living and hall
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,3.00000,7.00000,0.00000  #   1
*vertex,0.00000,7.00000,0.00000  #   2
*vertex,0.00000,3.00000,0.00000  #   3
*vertex,3.00000,3.00000,0.00000  #   4
*vertex,3.00000,7.00000,2.20000  #   5
*vertex,0.00000,7.00000,2.20000  #   6
*vertex,0.00000,3.00000,2.20000  #   7
*vertex,3.00000,3.00000,2.20000  #   8
*vertex,3.00000,4.00000,2.20000  #   9
*vertex,0.00000,4.00000,2.20000  #  10
*vertex,3.00000,5.50000,2.20000  #  11
*vertex,3.00000,5.50000,0.00000  #  12
*vertex,2.00000,7.00000,0.80000  #  13
*vertex,0.80000,7.00000,0.80000  #  14
*vertex,0.80000,7.00000,2.00000  #  15
*vertex,2.00000,7.00000,2.00000  #  16
*vertex,3.00000,4.00000,0.00000  #  17
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,6,5,1,13,16,15,14,13  #  1
*edges,5,2,3,7,10,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,17,12,11,9  #  4
*edges,5,5,6,10,9,11  #  5
*edges,6,4,3,2,1,12,17  #  6
*edges,4,9,10,7,8  #  7
*edges,4,12,1,5,11  #  8
*edges,4,13,14,15,16  #  9
*edges,4,4,17,9,8  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,north,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,west,VERT,-,-,-,ext_part,OPAQUE,SIMILAR,00,00  #   2 ||< identical environment
*surf,living,VERT,-,-,-,int_part,OPAQUE,ANOTHER,05,08  #   3 ||< kitch:liv_room
*surf,hall1,VERT,-,-,-,int_part,OPAQUE,ANOTHER,03,02  #   4 ||< kitchn:hall_1
*surf,bed1,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,07,06  #   5 ||< kitchn:bed1
*surf,floor,FLOR,-,-,-,grnd_floor,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,bed2,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,10,08  #   7 ||< kitch:bed2
*surf,east,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,window,VERT,-,-,-,dbl_glz,DCF7365_10nb,EXTERIOR,0,0  #   9 ||< external
*surf,door_hall1,VERT,-,-,-,int_doors,OPAQUE,ANOTHER,03,08  #  10 ||< door_kitchn:hall_1
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    12.00 0  # zone base list
