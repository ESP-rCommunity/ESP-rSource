*Geometry 1.1,GEN,room # tag version, format, zone name
*date Thu Aug 23 18:51:43 2007  # latest file modification 
room describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,4.00000,0.00000,0.00000  #   2
*vertex,4.00000,5.00000,0.00000  #   3
*vertex,0.00000,5.00000,0.00000  #   4
*vertex,0.00000,0.00000,3.00000  #   5
*vertex,4.00000,0.00000,3.00000  #   6
*vertex,4.00000,5.00000,3.00000  #   7
*vertex,0.00000,5.00000,3.00000  #   8
*vertex,0.80000,0.00000,0.90000  #   9
*vertex,3.20000,0.00000,0.90000  #  10
*vertex,3.20000,0.00000,2.70000  #  11
*vertex,0.80000,0.00000,2.70000  #  12
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,6,5,1,9,12,11,10,9  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,4,5,6,7,8  #  5
*edges,4,1,4,3,2  #  6
*edges,4,9,10,11,12  #  7
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Surf-1,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,Surf-2,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,Surf-3,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,Surf-4,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,Surf-5,CEIL,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,Surf-6,FLOR,-,-,-,grnd_floor,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,window,VERT,Surf-1,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #   7 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    20.00 0  # zone base list
