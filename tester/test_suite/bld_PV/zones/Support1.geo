*Geometry 1.1,GEN,Support1 # tag version, format, zone name
*date Wed May  5 16:51:50 2021  # latest file modification 
Support1 holds the pyranometer
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,0.00000,3.00000,0.00000  #   2
*vertex,2.00000,3.00000,0.00000  #   3
*vertex,2.00000,0.00000,0.00000  #   4
*vertex,0.00000,0.00000,1.26800  #   5
*vertex,0.00000,3.00000,3.00000  #   6
*vertex,2.00000,3.00000,3.00000  #   7
*vertex,2.00000,0.00000,1.26800  #   8
*vertex,0.00000,1.00000,3.00000  #   9
*vertex,2.00000,1.00000,3.00000  #  10
# 
# tag, number of vertices followed by list of associated vert
*edges,5,1,5,9,6,2  #   1
*edges,4,2,6,7,3  #   2
*edges,5,4,3,7,10,8  #   3
*edges,4,4,8,5,1  #   4
*edges,4,9,10,7,6  #   5
*edges,4,1,2,3,4  #   6
*edges,4,5,8,10,9  #   7
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,WWall,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,000,000  #   1 ||< external
*surf,NWall,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,000,000  #   2 ||< external
*surf,EWall,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,000,000  #   3 ||< external
*surf,SWall,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,000,000  #   4 ||< external
*surf,Ceiling,CEIL,-,-,-,extern_wall,OPAQUE,EXTERIOR,000,000  #   5 ||< external
*surf,Floor,FLOR,-,-,-,floor_1,OPAQUE,GROUND,001,000  #   6 ||< ground profile  1
*surf,Pyranometer,SLOP,-,-,-,pyranometer,OPAQUE,EXTERIOR,000,000  #   7 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,3,6,5,7,    14.00 0  # zone base list
