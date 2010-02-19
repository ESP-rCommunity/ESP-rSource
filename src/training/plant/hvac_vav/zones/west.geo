*Geometry 1.1,GEN,west # tag version, format, zone name
*date Tue Jul 21 16:10:46 2009  # latest file modification 
south facing perimeter zone
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,30.00000,0.00000  #   1
*vertex,0.00000,0.00000,0.00000  #   2
*vertex,5.00000,0.00000,0.00000  #   3
*vertex,5.00000,30.00000,0.00000  #   4
*vertex,0.00000,30.00000,2.70000  #   5
*vertex,0.00000,0.00000,2.70000  #   6
*vertex,5.00000,0.00000,2.70000  #   7
*vertex,5.00000,30.00000,2.70000  #   8
*vertex,0.00000,29.00000,1.00000  #   9
*vertex,0.00000,1.00000,1.00000  #  10
*vertex,0.00000,1.00000,2.00000  #  11
*vertex,0.00000,29.00000,2.00000  #  12
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
*surf,west,VERT,-,-,-,wall_NECB,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,south,VERT,-,-,-,wall_NECB,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,internal,VERT,-,-,-,partition,OPAQUE,ANOTHER,02,03  #   3 ||< internal:east
*surf,north,VERT,-,-,-,wall_NECB,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,ceiling,CEIL,-,-,-,roof_NECB,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,floor,FLOR,-,-,-,conc_deck,OPAQUE,ADIABATIC,0,0  #   6 ||< adiabatic
*surf,glazing,VERT,-,-,-,window_NECB,DCF7447_06nb,EXTERIOR,0,0  #   7 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,   150.00 0  # zone base list
