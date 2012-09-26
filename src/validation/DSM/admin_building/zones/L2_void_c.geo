*Geometry 1.1,GEN,L2_void_c # tag version, format, zone name
*date Tue Jul 12 22:13:45 2011  # latest file modification 
L2_void_c describes void adj to circular level two
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,34.25000,13.06200,52.45000  #   1
*vertex,34.50000,12.11219,52.45000  #   2
*vertex,34.60000,10.97222,52.45000  #   3
*vertex,34.38913,9.92725,52.45000  #   4
*vertex,33.86664,8.78728,52.45000  #   5
*vertex,36.24158,9.35726,52.45000  #   6
*vertex,36.76406,9.49976,52.45000  #   7
*vertex,35.91460,13.46780,52.45000  #   8
*vertex,34.25000,13.06200,56.35000  #   9
*vertex,34.50000,12.11219,56.35000  #  10
*vertex,34.60000,10.97222,56.35000  #  11
*vertex,34.38913,9.92725,56.35000  #  12
*vertex,33.86664,8.78728,56.35000  #  13
*vertex,36.24158,9.35726,56.35000  #  14
*vertex,36.76406,9.49976,56.35000  #  15
*vertex,35.91460,13.46780,56.35000  #  16
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,10,9  #  1
*edges,4,2,3,11,10  #  2
*edges,4,3,4,12,11  #  3
*edges,4,4,5,13,12  #  4
*edges,4,5,6,14,13  #  5
*edges,4,6,7,15,14  #  6
*edges,4,7,8,16,15  #  7
*edges,4,8,1,9,16  #  8
*edges,8,9,10,11,12,13,14,15,16  #  9
*edges,8,1,8,7,6,5,4,3,2  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Wall-1,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,42,12  #   1 ||< seg_l:L2_circul
*surf,Wall-2,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,42,13  #   2 ||< seg_m:L2_circul
*surf,Wall-3,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,42,14  #   3 ||< seg_n:L2_circul
*surf,Wall-4,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,42,15  #   4 ||< seg_o:L2_circul
*surf,Wall-5,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,Wall-6,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,Wall-7,VERT,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   7 ||< not yet defined
*surf,Wall-8,VERT,-,-,-,UNKNOWN,OPAQUE,ANOTHER,03,42  #   8 ||< ptn_void_c:L2_op_pln
*surf,Top-9,CEIL,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #   9 ||< not yet defined
*surf,Base-10,FLOR,-,-,-,UNKNOWN,OPAQUE,UNKNOWN,0,0  #  10 ||< not yet defined
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,10,     8.16 0  # zone base list
