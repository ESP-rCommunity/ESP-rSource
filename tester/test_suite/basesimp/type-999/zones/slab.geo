*Geometry 1.1,GEN,slab # tag version, format, zone name
*date Wed Sep  8 12:48:07 2021  # latest file modification 
slab describes a slab on grade foundation
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,12.00000,0.00000,0.00000  #   2
*vertex,12.00000,12.00000,0.00000  #   3
*vertex,0.00000,12.00000,0.00000  #   4
*vertex,0.00000,0.00000,2.70000  #   5
*vertex,12.00000,0.00000,2.70000  #   6
*vertex,12.00000,12.00000,2.70000  #   7
*vertex,0.00000,12.00000,2.70000  #   8
# 
# tag, number of vertices followed by list of associated vert
*edges,4,4,3,2,1  #   1
*edges,4,1,2,6,5  #   2
*edges,4,2,3,7,6  #   3
*edges,4,3,4,8,7  #   4
*edges,4,1,5,8,4  #   5
*edges,4,5,6,7,8  #   6
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,first,FLOR,-,-,-,EIA_Slab,OPAQUE,BASESIMP,999,100  #   1 ||< BASESIMP config type 999
*surf,Surf-2,VERT,-,-,-,EIA_Wall,OPAQUE,ADIABATIC,000,000  #   2 ||< adiabatic
*surf,Surf-3,VERT,-,-,-,EIA_Wall,OPAQUE,ADIABATIC,000,000  #   3 ||< adiabatic
*surf,Surf-4,VERT,-,-,-,EIA_Wall,OPAQUE,ADIABATIC,000,000  #   4 ||< adiabatic
*surf,Surf-5,VERT,-,-,-,EIA_Wall,OPAQUE,ADIABATIC,000,000  #   5 ||< adiabatic
*surf,Surf-6,CEIL,-,-,-,EIA_Wall,OPAQUE,ADIABATIC,000,000  #   6 ||< adiabatic
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,144.00,1  # zone base
