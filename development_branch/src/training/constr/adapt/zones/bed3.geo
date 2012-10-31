*Geometry 1.1,GEN,bed3 # tag version, format, zone name
*date Fri Jul 17 13:33:43 2009  # latest file modification 
bed3 describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,6.00000,3.00000,2.20000  #   1
*vertex,3.00000,3.00000,2.20000  #   2
*vertex,3.00000,0.00000,2.20000  #   3
*vertex,6.00000,0.00000,2.20000  #   4
*vertex,6.00000,3.00000,4.40000  #   5
*vertex,3.00000,3.00000,4.40000  #   6
*vertex,3.00000,0.00000,4.40000  #   7
*vertex,6.00000,0.00000,4.40000  #   8
*vertex,4.00000,0.00000,3.20000  #   9
*vertex,5.00000,0.00000,3.20000  #  10
*vertex,5.00000,0.00000,4.20000  #  11
*vertex,4.00000,0.00000,4.20000  #  12
*vertex,4.00000,3.00000,2.20000  #  13
*vertex,4.00000,3.00000,4.40000  #  14
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,13,14,5  #  1
*edges,4,2,3,7,6  #  2
*edges,10,3,4,8,7,3,9,12,11,10,9  #  3
*edges,4,4,1,5,8  #  4
*edges,5,5,14,6,7,8  #  5
*edges,5,1,4,3,2,13  #  6
*edges,4,9,10,11,12  #  7
*edges,4,13,2,6,14  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,hall2,VERT,-,-,-,int_part,OPAQUE,ANOTHER,09,03  #   1 ||< bed3:hall_2
*surf,bed2,VERT,-,-,-,int_part,OPAQUE,ANOTHER,10,05  #   2 ||< bed3:bed2
*surf,south,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,east,VERT,-,-,-,ext_part,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,loft,CEIL,-,-,-,loft_ceil,OPAQUE,ANOTHER,12,10  #   5 ||< bed3:loft
*surf,living,FLOR,-,-,-,upper_flor,OPAQUE,ANOTHER,05,10  #   6 ||< bed3:liv_room
*surf,window,VERT,-,-,-,dbl_glz,DCF7365_10nb,EXTERIOR,0,0  #   7 ||< external
*surf,door_hall2,VERT,-,-,-,int_doors,OPAQUE,ANOTHER,09,11  #   8 ||< door_bed3:hall_2
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,     9.00 0  # zone base list
