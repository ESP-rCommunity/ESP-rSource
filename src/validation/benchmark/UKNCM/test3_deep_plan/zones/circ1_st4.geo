*Geometry 1.1,GEN,circ1_st4 # tag version, format, zone name
*date Fri May  8 10:44:28 2009  # latest file modification 
circ1_st4 circulation area 1 on 4th level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,15.00000,15.00000,12.00000  #   1
*vertex,35.00000,15.00000,12.00000  #   2
*vertex,35.00000,20.00000,12.00000  #   3
*vertex,15.00000,20.00000,12.00000  #   4
*vertex,15.00000,15.00000,16.00000  #   5
*vertex,35.00000,15.00000,16.00000  #   6
*vertex,35.00000,20.00000,16.00000  #   7
*vertex,15.00000,20.00000,16.00000  #   8
*vertex,20.00000,20.00000,12.00000  #   9
*vertex,20.00000,20.00000,16.00000  #  10
*vertex,30.00000,20.00000,12.00000  #  11
*vertex,30.00000,20.00000,16.00000  #  12
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,4,4,1,5,8  #  3
*edges,6,5,6,7,12,10,8  #  4
*edges,6,1,4,9,11,3,2  #  5
*edges,4,9,4,8,10  #  6
*edges,4,3,11,12,7  #  7
*edges,4,11,9,10,12  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_of9,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,55,04  #   1 ||< ptn_circ1:office9_st4
*surf,ptn_of9a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,55,10  #   2 ||< ptn_circ1b:office9_st4
*surf,ptn_of9b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,55,09  #   3 ||< ptn_circ1a:office9_st4
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,65,05  #   4 ||< floor:circ1_st5
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,37,04  #   5 ||< ceiling:circ1_st3
*surf,ptn_m1,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,53,01  #   6 ||< ptn_circ1:meetR1_st4
*surf,ptn_m2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,54,01  #   7 ||< ptn_circ1:meetR2_st4
*surf,ptn_atr,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,71,15  #   8 ||< Wall-3bst4:atrium
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,   100.00 2  # zone base list
