*Geometry 1.1,GEN,bath1-2 # tag version, format, zone name
*date Thu May  7 12:43:59 2009  # latest file modification 
bath1-2 describes bathroom space for guest rooms 1 and 2
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,5.00000,3.50000,3.50000  #   1
*vertex,9.00000,3.50000,3.50000  #   2
*vertex,9.00000,5.00000,3.50000  #   3
*vertex,5.00000,5.00000,3.50000  #   4
*vertex,5.00000,3.50000,7.00000  #   5
*vertex,9.00000,3.50000,7.00000  #   6
*vertex,9.00000,5.00000,7.00000  #   7
*vertex,5.00000,5.00000,7.00000  #   8
*vertex,7.00000,3.50000,3.50000  #   9
*vertex,7.00000,3.50000,7.00000  #  10
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
*surf,ptn_a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,09,04  #   1 ||< ptn_c:bed2_st2
*surf,ptn_b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,32,18  #   2 ||< ptn_bb:cirArea_St2
*surf,ptn_c,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,08,04  #   3 ||< ptn_c:bed1_st2
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,33,05  #   4 ||< floor:bath12_st3
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,06,16  #   5 ||< Base-6:circ_area
*surf,ptn_e,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,08,03  #   6 ||< ptn_b:bed1_st2
*surf,ptn_f,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,09,05  #   7 ||< ptn_d:bed2_st2
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,     6.00 2  # zone base list
