*Geometry 1.1,GEN,meetR2_st3 # tag version, format, zone name
*date Fri May  8 10:46:38 2009  # latest file modification 
meetR2_st3 describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,30.00000,20.00000,8.00000  #   1
*vertex,35.00000,20.00000,8.00000  #   2
*vertex,35.00000,30.00000,8.00000  #   3
*vertex,30.00000,30.00000,8.00000  #   4
*vertex,30.00000,20.00000,12.00000  #   5
*vertex,35.00000,20.00000,12.00000  #   6
*vertex,35.00000,30.00000,12.00000  #   7
*vertex,30.00000,30.00000,12.00000  #   8
*vertex,35.00000,25.00000,8.00000  #   9
*vertex,35.00000,25.00000,12.00000  #  10
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,3,4,8,7  #  2
*edges,4,4,1,5,8  #  3
*edges,5,5,6,10,7,8  #  4
*edges,5,1,4,3,9,2  #  5
*edges,4,2,9,10,6  #  6
*edges,4,9,3,7,10  #  7
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_circ1,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,37,07  #   1 ||< ptn_m2:circ1_st3
*surf,ptn_circ2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,38,07  #   2 ||< ptn_m2:circ2_st3
*surf,ptn_atrium,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,71,14  #   3 ||< Wall-4st3:atrium
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,54,05  #   4 ||< floor:meetR2_st4
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,26,04  #   5 ||< ceiling:meetR2_st2
*surf,ptn_off9,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,41,12  #   6 ||< ptn_meet1b:office9_st3
*surf,ptn_off10,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,42,12  #   7 ||< ptn_m2:off10_st3
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,    50.00 2  # zone base list
