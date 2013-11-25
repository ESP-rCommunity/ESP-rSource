*Geometry 1.1,GEN,mvhr_lefta # tag version, format, zone name
*date Fri Aug  3 10:26:26 2012  # latest file modification 
mvhr_lefta describes 2nd section of left mvhr
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,6.00000,8.00000,0.00000  #   1
*vertex,6.20000,8.00000,0.00000  #   2
*vertex,6.20000,11.00000,0.00000  #   3
*vertex,6.00000,11.00000,0.00000  #   4
*vertex,6.00000,8.00000,3.00000  #   5
*vertex,6.20000,8.00000,3.00000  #   6
*vertex,6.20000,11.00000,3.00000  #   7
*vertex,6.00000,11.00000,3.00000  #   8
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
*surf,Wall-1,VERT,-,-,-,insul_mtl_p,OPAQUE,ANOTHER,05,03  #   1 ||< Wall-3:mvhr_left
*surf,plate,VERT,-,-,-,exch_plate,OPAQUE,ANOTHER,10,04  #   2 ||< plate:mvhr_righta
*surf,Wall-3,VERT,-,-,-,insul_mtl_p,OPAQUE,ANOTHER,11,01  #   3 ||< Wall-1:mvhr_leftb
*surf,Wall-4,VERT,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,Top-5,CEIL,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,Base-6,FLOR,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #   6 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,0.60,0  # zone base
