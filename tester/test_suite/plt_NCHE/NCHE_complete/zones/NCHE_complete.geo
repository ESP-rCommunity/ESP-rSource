*Geometry 1.1,GEN,Default # tag version, format, zone name
*date Mon May  3 08:28:56 2021  # latest file modification 
Default describes a dummy support for the system
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,5.00000,0.00000,0.00000  #   2
*vertex,5.00000,3.00000,0.00000  #   3
*vertex,0.00000,3.00000,0.00000  #   4
*vertex,0.00000,0.00000,2.70000  #   5
*vertex,5.00000,0.00000,2.70000  #   6
*vertex,5.00000,3.00000,2.70000  #   7
*vertex,0.00000,3.00000,2.70000  #   8
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #   1
*edges,4,2,3,7,6  #   2
*edges,4,3,4,8,7  #   3
*edges,4,4,1,5,8  #   4
*edges,4,5,6,7,8  #   5
*edges,4,1,4,3,2  #   6
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Wall-1,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,000,000  #   1 ||< external
*surf,Wall-2,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,000,000  #   2 ||< external
*surf,Wall-3,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,000,000  #   3 ||< external
*surf,Wall-4,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,000,000  #   4 ||< external
*surf,Top-5,CEIL,-,-,-,extern_wall,OPAQUE,EXTERIOR,000,000  #   5 ||< external
*surf,Base-6,FLOR,-,-,-,extern_wall,OPAQUE,EXTERIOR,000,000  #   6 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    15.00 0  # zone base list
