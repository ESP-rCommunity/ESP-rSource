*Geometry 1.1,GEN,L2_kitchen # tag version, format, zone name
*date Thu Jul 14 19:34:38 2011  # latest file modification 
L2_kitchen describes L2 kitchenette on service road facade
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,41.37145,35.70000,52.45000  #   1
*vertex,44.69636,35.70000,52.45000  #   2
*vertex,44.74386,40.70000,52.45000  #   3
*vertex,44.22137,40.70000,52.45000  #   4
*vertex,42.17893,40.70000,52.45000  #   5
*vertex,41.32395,40.70000,52.45000  #   6
*vertex,41.37145,35.70000,55.59300  #   7
*vertex,44.69636,35.70000,55.59300  #   8
*vertex,44.74386,40.70000,55.59300  #   9
*vertex,44.22137,40.70000,55.59300  #  10
*vertex,42.17893,40.70000,55.59300  #  11
*vertex,41.32395,40.70000,55.59300  #  12
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,8,7  #  1
*edges,4,2,3,9,8  #  2
*edges,4,3,4,10,9  #  3
*edges,4,4,5,11,10  #  4
*edges,4,5,6,12,11  #  5
*edges,4,6,1,7,12  #  6
*edges,6,7,8,9,10,11,12  #  7
*edges,6,1,6,5,4,3,2  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Wall-1,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,06,11  #   1 ||< Wall-11:L2_corid_a
*surf,Wall-2,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   2 ||< not yet defined
*surf,Wall-3,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,Wall-4,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,Wall-5,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,Wall-6,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,08,10  #   6 ||< ptn_stair:l2_cell_c
*surf,Top-7,CEIL,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   7 ||< not yet defined
*surf,Base-8,FLOR,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   8 ||< not yet defined
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,8,    17.25 0  # zone base list
