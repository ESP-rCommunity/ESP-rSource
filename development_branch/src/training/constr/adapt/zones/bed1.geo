*Geometry 1.1,GEN,bed1 # tag version, format, zone name
*date Fri Jul 17 13:33:43 2009  # latest file modification 
bed1 describes a 1st level north facing room
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,3.00000,7.00000,2.20000  #   1
*vertex,0.00000,7.00000,2.20000  #   2
*vertex,0.00000,4.00000,2.20000  #   3
*vertex,3.00000,4.00000,2.20000  #   4
*vertex,3.00000,7.00000,4.40000  #   5
*vertex,0.00000,7.00000,4.40000  #   6
*vertex,0.00000,4.00000,4.40000  #   7
*vertex,3.00000,4.00000,4.40000  #   8
*vertex,3.00000,5.00000,2.20000  #   9
*vertex,3.00000,5.00000,4.40000  #  10
*vertex,2.00000,7.00000,3.00000  #  11
*vertex,0.80000,7.00000,3.00000  #  12
*vertex,0.80000,7.00000,4.00000  #  13
*vertex,2.00000,7.00000,4.00000  #  14
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,6,5,1,11,14,13,12,11  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,9,1,5,10  #  4
*edges,5,5,6,7,8,10  #  5
*edges,5,4,3,2,1,9  #  6
*edges,4,4,9,10,8  #  7
*edges,4,11,12,13,14  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,north,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,west,VERT,-,-,-,ext_part,OPAQUE,SIMILAR,00,00  #   2 ||< identical environment
*surf,bed2,VERT,-,-,-,int_part,OPAQUE,ANOTHER,10,02  #   3 ||< bed1:bed2
*surf,bathrm,VERT,-,-,-,int_part,OPAQUE,ANOTHER,06,02  #   4 ||< bed1:bathrm
*surf,loft,CEIL,-,-,-,loft_ceil,OPAQUE,ANOTHER,12,07  #   5 ||< bed1:loft
*surf,kitchn,FLOR,-,-,-,upper_flor,OPAQUE,ANOTHER,04,05  #   6 ||< bed1:kitchen
*surf,door_hall2,VERT,-,-,-,int_doors,OPAQUE,ANOTHER,09,02  #   7 ||< door_bed1:hall_2
*surf,window,VERT,-,-,-,dbl_glz,DCF7365_10nb,EXTERIOR,0,0  #   8 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,     9.00 0  # zone base list
