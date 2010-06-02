*Geometry 1.1,GEN,bath3-4 # tag version, format, zone name
*date Thu May  7 11:37:56 2009  # latest file modification 
bath3-4 describes the bath for guest rooms 3 and 4
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,12.00000,3.50000,3.50000  #   1
*vertex,16.00000,3.50000,3.50000  #   2
*vertex,16.00000,5.00000,3.50000  #   3
*vertex,12.00000,5.00000,3.50000  #   4
*vertex,12.00000,3.50000,7.00000  #   5
*vertex,16.00000,3.50000,7.00000  #   6
*vertex,16.00000,5.00000,7.00000  #   7
*vertex,12.00000,5.00000,7.00000  #   8
*vertex,14.00000,3.50000,3.50000  #   9
*vertex,14.00000,3.50000,7.00000  #  10
# 
# tag, number of vertices followed by list of associated vert
*edges,4,2,3,7,6  #  1
*edges,4,3,4,8,7  #  2
*edges,4,4,1,5,8  #  3
*edges,5,5,10,6,7,8  #  4
*edges,5,1,4,3,2,9  #  5
*edges,4,1,9,10,5  #  6
*edges,4,9,2,6,10  #  7
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,11,04  #   1 ||< ptn_c:bed4_st2
*surf,ptn_b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,32,21  #   2 ||< ptn_be:cirArea_St2
*surf,ptn_c,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,10,04  #   3 ||< ptn_c:bed3_st2
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,34,05  #   4 ||< floor_:bath34_st3
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,01,13  #   5 ||< Base-10:eating_area
*surf,ptn_d,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,10,03  #   6 ||< ptn_b:bed3_st2
*surf,ptn_e,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,11,05  #   7 ||< ptn_d:bed4_st2
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,     6.00 2  # zone base list
