*Geometry 1.1,GEN,floor # tag version, format, zone name
*date Thu Aug 23 19:31:15 2007  # latest file modification 
floor is a thin zone representing piping for floor heating
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,-0.20000  #   1
*vertex,6.10000,0.00000,-0.20000  #   2
*vertex,6.10000,5.55000,-0.20000  #   3
*vertex,0.00000,5.55000,-0.20000  #   4
*vertex,0.00000,0.00000,0.00000  #   5
*vertex,6.10000,0.00000,0.00000  #   6
*vertex,6.10000,5.55000,0.00000  #   7
*vertex,0.00000,5.55000,0.00000  #   8
*vertex,0.00000,2.35000,0.00000  #   9
*vertex,0.00000,2.50000,0.00000  #  10
*vertex,0.00000,4.50000,0.00000  #  11
*vertex,3.00000,4.50000,0.00000  #  12
*vertex,3.00000,0.00000,0.00000  #  13
*vertex,6.10000,2.35000,0.00000  #  14
*vertex,3.10000,0.00000,0.00000  #  15
*vertex,3.10000,4.50000,0.00000  #  16
*vertex,6.10000,4.50000,0.00000  #  17
*vertex,6.10000,2.50000,0.00000  #  18
*vertex,0.00000,4.55000,0.00000  #  19
*vertex,6.10000,4.55000,0.00000  #  20
# 
# tag, number of vertices followed by list of associated vert
*edges,6,1,2,6,15,13,5  #  1
*edges,8,2,3,7,20,17,18,14,6  #  2
*edges,4,3,4,8,7  #  3
*edges,8,4,1,5,9,10,11,19,8  #  4
*edges,4,1,4,3,2  #  5
*edges,6,9,5,13,12,11,10  #  6
*edges,6,6,14,18,17,16,15  #  7
*edges,4,8,19,20,7  #  8
*edges,8,13,15,16,17,20,19,11,12  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,edge_s,VERT,-,-,-,extern_wall,OPAQUE,SIMILAR,00,00  #   1 ||< identical environment
*surf,edge_e,VERT,-,-,-,structure,OPAQUE,SIMILAR,00,00  #   2 ||< identical environment
*surf,edge_n,VERT,-,-,-,structure,OPAQUE,SIMILAR,00,00  #   3 ||< identical environment
*surf,edge_w,VERT,-,-,-,structure,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,Surf-6,FLOR,-,-,-,structure,OPAQUE,CONSTANT,20,00  #   5 ||< constant @ 20dC &   0W rad
*surf,floor_ma,CEIL,-,-,-,screed_rev,OPAQUE,ANOTHER,01,06  #   6 ||< floor:manager_a
*surf,floor_mb,CEIL,-,-,-,screed_rev,OPAQUE,ANOTHER,02,06  #   7 ||< floor:manager_b
*surf,floor_cor,CEIL,-,-,-,screed_rev,OPAQUE,ANOTHER,03,05  #   8 ||< floor:corridor
*surf,filler,CEIL,-,-,-,screed_rev,OPAQUE,ADIABATIC,0,0  #   9 ||< adiabatic
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,2,6,5,    47.35 0  # zone base list
