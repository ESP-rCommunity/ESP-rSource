*Geometry 1.1,GEN,bath9-10 # tag version, format, zone name
*date Thu May  7 11:42:17 2009  # latest file modification 
bath9-10 describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,5.00000,6.50000,3.50000  #   1
*vertex,9.00000,6.50000,3.50000  #   2
*vertex,9.00000,8.00000,3.50000  #   3
*vertex,5.00000,8.00000,3.50000  #   4
*vertex,5.00000,6.50000,7.00000  #   5
*vertex,9.00000,6.50000,7.00000  #   6
*vertex,9.00000,8.00000,7.00000  #   7
*vertex,5.00000,8.00000,7.00000  #   8
*vertex,7.00000,8.00000,3.50000  #   9
*vertex,7.00000,8.00000,7.00000  #  10
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,4,4,1,5,8  #  3
*edges,5,5,6,7,10,8  #  4
*edges,5,1,4,9,3,2  #  5
*edges,4,9,4,8,10  #  6
*edges,4,3,9,10,7  #  7
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,32,30  #   1 ||< ptn_cc:cirArea_St2
*surf,ptn_b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,17,02  #   2 ||< ptn_b:bed10_st2
*surf,ptn_c,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,16,02  #   3 ||< ptn_b:bed9_st2
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,37,05  #   4 ||< floor:bath9-10St3
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,04,09  #   5 ||< Base-11:cell_office
*surf,ptn_d,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,16,03  #   6 ||< ptn_c:bed9_st2
*surf,ptn_e,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,17,01  #   7 ||< ptn_a:bed10_st2
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,     6.00 2  # zone base list
