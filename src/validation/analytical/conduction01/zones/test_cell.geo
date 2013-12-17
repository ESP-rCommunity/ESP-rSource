*Geometry 1.1,GEN,test_cell # tag version, format, zone name
*date Sun Dec  8 17:33:17 2013  # latest file modification 
Test Cell
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,1.00000,0.00000,0.00000  #   2
*vertex,1.00000,1.00000,0.00000  #   3
*vertex,0.00000,1.00000,0.00000  #   4
*vertex,0.00000,0.00000,1.00000  #   5
*vertex,1.00000,0.00000,1.00000  #   6
*vertex,1.00000,1.00000,1.00000  #   7
*vertex,0.00000,1.00000,1.00000  #   8
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,4,5,6,7,8  #  5
*edges,4,1,4,3,2  #  6
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Surf-1,VERT,-,-,-,intern_wall,OPAQUE,CONSTANT,00,00  #   1 ||< constant @  0dC &   0W rad
*surf,Surf-2,VERT,-,-,-,intern_wall,OPAQUE,CONSTANT,00,00  #   2 ||< constant @  0dC &   0W rad
*surf,Surf-3,VERT,-,-,-,intern_wall,OPAQUE,CONSTANT,00,00  #   3 ||< constant @  0dC &   0W rad
*surf,Surf-4,VERT,-,-,-,intern_wall,OPAQUE,CONSTANT,00,00  #   4 ||< constant @  0dC &   0W rad
*surf,Surf-5,CEIL,-,-,-,intern_wall,OPAQUE,CONSTANT,00,00  #   5 ||< constant @  0dC &   0W rad
*surf,Surf-6,FLOR,-,-,-,intern_wall,OPAQUE,CONSTANT,00,00  #   6 ||< constant @  0dC &   0W rad
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,     1.00 0  # zone base list
