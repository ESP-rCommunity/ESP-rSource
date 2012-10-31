*Geometry 1.1,GEN,bed2 # tag version, format, zone name
*date Fri Jul 17 13:33:43 2009  # latest file modification 
bed2 is a south facing bedroom over the lounge
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,3.00000,3.00000,2.20000  #   1
*vertex,3.00000,4.00000,2.20000  #   2
*vertex,0.00000,4.00000,2.20000  #   3
*vertex,0.00000,0.00000,2.20000  #   4
*vertex,3.00000,0.00000,2.20000  #   5
*vertex,3.00000,3.00000,4.40000  #   6
*vertex,3.00000,4.00000,4.40000  #   7
*vertex,0.00000,4.00000,4.40000  #   8
*vertex,0.00000,0.00000,4.40000  #   9
*vertex,3.00000,0.00000,4.40000  #  10
*vertex,0.00000,3.00000,2.20000  #  11
*vertex,1.00000,0.00000,3.20000  #  12
*vertex,2.00000,0.00000,3.20000  #  13
*vertex,2.00000,0.00000,4.20000  #  14
*vertex,1.00000,0.00000,4.20000  #  15
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,7,6  #  1
*edges,4,2,3,8,7  #  2
*edges,5,3,11,4,9,8  #  3
*edges,10,4,5,10,9,4,12,15,14,13,12  #  4
*edges,4,5,1,6,10  #  5
*edges,5,7,8,9,10,6  #  6
*edges,4,1,5,4,11  #  7
*edges,4,1,11,3,2  #  8
*edges,4,12,13,14,15  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,door_hall2,VERT,-,-,-,int_doors,OPAQUE,ANOTHER,09,09  #   1 ||< door_bed2:hall_2
*surf,bed1,VERT,-,-,-,int_part,OPAQUE,ANOTHER,07,03  #   2 ||< bed2:bed1
*surf,west,VERT,-,-,-,ext_part,OPAQUE,SIMILAR,00,00  #   3 ||< identical environment
*surf,south,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,bed3,VERT,-,-,-,int_part,OPAQUE,ANOTHER,11,02  #   5 ||< bed2:bed3
*surf,loft,CEIL,-,-,-,loft_ceil,OPAQUE,ANOTHER,12,06  #   6 ||< bed2:loft
*surf,living,FLOR,-,-,-,upper_flor,OPAQUE,ANOTHER,05,05  #   7 ||< bed2:liv_room
*surf,kitch,FLOR,-,-,-,upper_flor,OPAQUE,ANOTHER,04,07  #   8 ||< bed2:kitchen
*surf,window,VERT,-,-,-,dbl_glz,DCF7365_10nb,EXTERIOR,0,0  #   9 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,2,7,8,    12.00 0  # zone base list
