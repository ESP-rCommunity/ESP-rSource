*Geometry 1.1,GEN,sunroom # tag version, format, zone name
*date Sun Jun 23 20:24:07 2013  # latest file modification 
sunroom describes the only zone in the problem
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,3.00000,0.00000,0.00000  #   2
*vertex,3.00000,4.00000,0.00000  #   3
*vertex,0.00000,4.00000,0.00000  #   4
*vertex,0.00000,0.00000,2.70000  #   5
*vertex,3.00000,0.00000,2.70000  #   6
*vertex,3.00000,4.00000,2.70000  #   7
*vertex,0.00000,4.00000,2.70000  #   8
*vertex,0.50000,0.00000,0.50000  #   9
*vertex,2.50000,0.00000,0.50000  #  10
*vertex,2.50000,0.00000,2.20000  #  11
*vertex,0.50000,0.00000,2.20000  #  12
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
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,sud,VERT,-,-,-,typical_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,est,VERT,-,-,-,typical_wall,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,nord,VERT,-,-,-,typical_wall,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,ouest,VERT,-,-,-,typical_wall,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,haut,CEIL,-,-,-,typical_wall,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,bas,FLOR,-,-,-,typical_wall,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,fenetre,VERT,sud,-,-,d_glz,DCF7671_06nb,EXTERIOR,0,0  #   7 ||< external
# 
*insol,1,6,0,6  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    12.00 0  # zone base list
