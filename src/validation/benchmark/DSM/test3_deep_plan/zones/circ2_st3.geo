*Geometry 1.1,GEN,circ2_st3 # tag version, format, zone name
*date Fri May  8 10:44:04 2009  # latest file modification 
circ2_st3 is 2nd circulation space in level 3
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,15.00000,30.00000,8.00000  #   1
*vertex,35.00000,30.00000,8.00000  #   2
*vertex,35.00000,35.00000,8.00000  #   3
*vertex,15.00000,35.00000,8.00000  #   4
*vertex,15.00000,30.00000,12.00000  #   5
*vertex,35.00000,30.00000,12.00000  #   6
*vertex,35.00000,35.00000,12.00000  #   7
*vertex,15.00000,35.00000,12.00000  #   8
*vertex,20.00000,30.00000,8.00000  #   9
*vertex,20.00000,30.00000,12.00000  #  10
*vertex,30.00000,30.00000,8.00000  #  11
*vertex,30.00000,30.00000,12.00000  #  12
# 
# tag, number of vertices followed by list of associated vert
*edges,4,2,3,7,6  #  1
*edges,4,3,4,8,7  #  2
*edges,4,4,1,5,8  #  3
*edges,6,5,10,12,6,7,8  #  4
*edges,6,1,4,3,2,11,9  #  5
*edges,4,1,9,10,5  #  6
*edges,4,11,2,6,12  #  7
*edges,4,9,11,12,10  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_of10a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,42,10  #   1 ||< ptn_circ2c:off10_st3
*surf,ptn_of10b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,42,02  #   2 ||< ptn_circ2a:off10_st3
*surf,ptn_of10c,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,42,09  #   3 ||< ptn_circ2b:off10_st3
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,52,05  #   4 ||< floor:circ2_st4
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,24,04  #   5 ||< ceiling:circ2_st2
*surf,ptn_m1,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,39,03  #   6 ||< ptn_circ2:meetR1_st3
*surf,ptn_m2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,40,02  #   7 ||< ptn_circ2:meetR2_st3
*surf,ptn_atr,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,71,12  #   8 ||< Wall-1bst3:atrium
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,   100.00 2  # zone base list
