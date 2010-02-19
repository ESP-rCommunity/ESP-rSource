*Geometry 1.1,GEN,meetR1_st4 # tag version, format, zone name
*date Fri May  8 10:46:56 2009  # latest file modification 
meetR1_st4 describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,15.00000,20.00000,12.00000  #   1
*vertex,20.00000,20.00000,12.00000  #   2
*vertex,20.00000,30.00000,12.00000  #   3
*vertex,15.00000,30.00000,12.00000  #   4
*vertex,15.00000,20.00000,16.00000  #   5
*vertex,20.00000,20.00000,16.00000  #   6
*vertex,20.00000,30.00000,16.00000  #   7
*vertex,15.00000,30.00000,16.00000  #   8
*vertex,15.00000,25.00000,12.00000  #   9
*vertex,15.00000,25.00000,16.00000  #  10
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,5,5,6,7,8,10  #  4
*edges,5,1,9,4,3,2  #  5
*edges,4,9,1,5,10  #  6
*edges,4,4,9,10,8  #  7
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_circ1,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,51,06  #   1 ||< ptn_m1:circ1_st4
*surf,ptn_atrium,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,71,17  #   2 ||< Wall-2st4:atrium
*surf,ptn_circ2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,52,06  #   3 ||< ptn_m1:circ2_st4
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,67,05  #   4 ||< floor:meetR1_st5
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,39,04  #   5 ||< ceiling:meetR1_st3
*surf,ptn_of9,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,55,11  #   6 ||< ptn_m1:office9_st4
*surf,ptn_of10,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,56,11  #   7 ||< ptn_m1:off10_st4
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,    50.00 2  # zone base list
