*Geometry 1.1,GEN,mixed_test # tag version, format, zone name
*date Tue Jul 21 13:27:29 2009  # latest file modification 
mixed_test describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,14.50000,1.00000,0.00000  #   1
*vertex,19.00000,1.00000,0.00000  #   2
*vertex,19.00000,4.50000,0.00000  #   3
*vertex,14.50000,4.50000,0.00000  #   4
*vertex,14.50000,1.00000,2.70000  #   5
*vertex,19.00000,1.00000,2.70000  #   6
*vertex,19.00000,4.50000,2.70000  #   7
*vertex,14.50000,4.50000,2.70000  #   8
*vertex,16.00000,1.00000,1.00000  #   9
*vertex,18.00000,1.00000,1.00000  #  10
*vertex,18.00000,1.00000,2.10000  #  11
*vertex,16.00000,1.00000,2.10000  #  12
*vertex,18.50000,4.50000,0.00000  #  13
*vertex,17.70000,4.50000,0.00000  #  14
*vertex,17.70000,4.50000,2.10000  #  15
*vertex,18.50000,4.50000,2.10000  #  16
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,6,5,1,9,12,11,10,9  #  1
*edges,4,2,3,7,6  #  2
*edges,8,3,13,16,15,14,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,4,5,6,7,8  #  5
*edges,6,1,4,14,13,3,2  #  6
*edges,4,9,10,11,12  #  7
*edges,4,13,14,15,16  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,south,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,right,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,back_dg,VERT,-,-,-,block_part,OPAQUE,ANOTHER,05,04  #   3 ||< prt_mix:passage
*surf,left,VERT,-,-,-,block_part,OPAQUE,ANOTHER,03,02  #   4 ||< right:heatexch
*surf,ceiling,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,06,04  #   5 ||< ceil_mix:roof
*surf,floor,FLOR,-,-,-,floor,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,glaz,VERT,-,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #   7 ||< external
*surf,door,VERT,-,-,-,door,OPAQUE,ANOTHER,05,13  #   8 ||< door_mix:passage
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    15.75 0  # zone base list
