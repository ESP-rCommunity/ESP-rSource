*Geometry 1.1,GEN,bottom # tag version, format, zone name
*date Sun Dec  8 16:51:31 2013  # latest file modification 
bottom describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,3.00000,0.00000,0.00000  #   2
*vertex,3.00000,9.00000,0.00000  #   3
*vertex,0.00000,9.00000,0.00000  #   4
*vertex,0.00000,0.00000,3.00000  #   5
*vertex,3.00000,0.00000,3.00000  #   6
*vertex,3.00000,9.00000,3.00000  #   7
*vertex,0.00000,9.00000,3.00000  #   8
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
*surf,south,VERT,-,-,-,sgl_glz,TRAN,EXTERIOR,0,0  #   1 ||< external
*surf,east,VERT,-,-,-,extern_wall,OPAQUE,ADIABATIC,0,0  #   2 ||< adiabatic
*surf,north,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,west,VERT,-,-,-,extern_wall,OPAQUE,ADIABATIC,0,0  #   4 ||< adiabatic
*surf,ceiling,CEIL,-,-,-,partition,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,floor,FLOR,-,-,-,partition,OPAQUE,EXTERIOR,0,0  #   6 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    27.00 0  # zone base list
