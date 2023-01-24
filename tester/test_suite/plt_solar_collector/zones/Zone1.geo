*Geometry 1.1,GEN,Zone1 # tag version, format, zone name
*date Sat May  1 18:26:53 2021  # latest file modification 
Zone1 describes a dummy zone
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,2.00000,0.00000,0.00000  #   2
*vertex,2.00000,3.00000,0.00000  #   3
*vertex,0.00000,3.00000,0.00000  #   4
*vertex,0.00000,1.41400,3.00000  #   5
*vertex,2.00000,1.41400,3.00000  #   6
*vertex,2.00000,3.00000,3.00000  #   7
*vertex,0.00000,3.00000,3.00000  #   8
*vertex,0.00000,0.00000,1.58600  #   9
*vertex,2.00000,0.00000,1.58600  #  10
# 
# tag, number of vertices followed by list of associated vert
*edges,4,9,10,6,5  #   1
*edges,5,2,3,7,6,10  #   2
*edges,4,3,4,8,7  #   3
*edges,5,4,1,9,5,8  #   4
*edges,4,5,6,7,8  #   5
*edges,4,4,3,2,1  #   6
*edges,4,1,2,10,9  #   7
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Pyrano,SLOP,-,-,-,Pynamometer,OPAQUE,EXTERIOR,00,000  #   1 ||< external
*surf,Surf-2,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,00,000  #   2 ||< external
*surf,Surf-3,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,00,000  #   3 ||< external
*surf,Surf-4,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,00,000  #   4 ||< external
*surf,Surf-5,CEIL,-,-,-,roof,OPAQUE,EXTERIOR,00,000  #   5 ||< external
*surf,Surf-6,FLOR,-,-,-,grnd_floor,OPAQUE,GROUND,001,000  #   6 ||< ground profile  1
*surf,Surf-7,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,00,000  #   7 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,     6.00 0  # zone base list
