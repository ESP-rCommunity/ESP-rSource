*Geometry 1.1,GEN,office9_st2 # tag version, format, zone name
*date Fri May  8 10:38:03 2009  # latest file modification 
office9_st2 is the complex inner office on 2nd level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,6.00000,6.00000,4.00000  #   1
*vertex,44.00000,6.00000,4.00000  #   2
*vertex,44.00000,25.00000,4.00000  #   3
*vertex,35.00000,25.00000,4.00000  #   4
*vertex,35.00000,15.00000,4.00000  #   5
*vertex,15.00000,15.00000,4.00000  #   6
*vertex,15.00000,25.00000,4.00000  #   7
*vertex,6.00000,25.00000,4.00000  #   8
*vertex,6.00000,6.00000,8.00000  #   9
*vertex,44.00000,6.00000,8.00000  #  10
*vertex,44.00000,25.00000,8.00000  #  11
*vertex,35.00000,25.00000,8.00000  #  12
*vertex,35.00000,15.00000,8.00000  #  13
*vertex,15.00000,15.00000,8.00000  #  14
*vertex,15.00000,25.00000,8.00000  #  15
*vertex,6.00000,25.00000,8.00000  #  16
*vertex,15.00000,20.00000,4.00000  #  17
*vertex,15.00000,20.00000,8.00000  #  18
*vertex,35.00000,20.00000,4.00000  #  19
*vertex,35.00000,20.00000,8.00000  #  20
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,10,9  #  1
*edges,4,2,3,11,10  #  2
*edges,4,3,4,12,11  #  3
*edges,4,5,6,14,13  #  4
*edges,4,7,8,16,15  #  5
*edges,4,8,1,9,16  #  6
*edges,10,9,10,11,12,20,13,14,18,15,16  #  7
*edges,10,1,8,7,17,6,5,19,4,3,2  #  8
*edges,4,6,17,18,14  #  9
*edges,4,19,5,13,20  # 10
*edges,4,17,7,15,18  # 11
*edges,4,4,19,20,12  # 12
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_of2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,16,03  #   1 ||< ptn_o9:office2_st2
*surf,ptn_of4,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,18,07  #   2 ||< ptn_o9:office4_st2
*surf,ptn_of10,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,28,03  #   3 ||< ptn_of9a:off10_st2
*surf,ptn_circ1,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,23,01  #   4 ||< ptn_o9a:circ1_st2
*surf,ptn_of10a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,28,01  #   5 ||< ptn_of9:off10_st2
*surf,ptn_of8,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,22,06  #   6 ||< ptn_o9:office8_st2
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,41,08  #   7 ||< floor:office9_st3
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,13,07  #   8 ||< ceiling:office_9
*surf,ptn_circ1a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,23,03  #   9 ||< ptn_o9c:circ1_st2
*surf,ptn_circ1b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,23,02  #  10 ||< ptn_o9b:circ1_st2
*surf,ptn_m1a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,25,06  #  11 ||< ptn_of9:meetR1_st2
*surf,ptn_m1b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,26,06  #  12 ||< ptn_of9:meetR2_st2
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,8,   522.00 2  # zone base list