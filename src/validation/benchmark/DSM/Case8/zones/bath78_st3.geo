*Geometry 1.1,GEN,bath78_st3 # tag version, format, zone name
*date Wed Jul 22 09:19:55 2009  # latest file modification 
bath78_st3 describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,26.00000,3.50000,7.00000  #   1
*vertex,30.00000,3.50000,7.00000  #   2
*vertex,30.00000,5.00000,7.00000  #   3
*vertex,26.00000,5.00000,7.00000  #   4
*vertex,26.00000,3.50000,10.50000  #   5
*vertex,30.00000,3.50000,10.50000  #   6
*vertex,30.00000,5.00000,10.50000  #   7
*vertex,26.00000,5.00000,10.50000  #   8
*vertex,28.00000,3.50000,7.00000  #   9
*vertex,28.00000,3.50000,10.50000  #  10
*vertex,27.10557,3.91459,10.50000  #  11
*vertex,28.89443,3.91459,10.50000  #  12
*vertex,28.89443,4.58541,10.50000  #  13
*vertex,27.10557,4.58541,10.50000  #  14
*vertex,27.25167,3.96938,10.50000  #  15
*vertex,28.74833,3.96938,10.50000  #  16
*vertex,28.74833,4.53062,10.50000  #  17
*vertex,27.25167,4.53062,10.50000  #  18
# 
# tag, number of vertices followed by list of associated vert
*edges,4,2,3,7,6  #  1
*edges,4,3,4,8,7  #  2
*edges,4,4,1,5,8  #  3
*edges,11,5,10,6,7,8,5,11,14,13,12,11  #  4
*edges,5,1,4,3,2,9  #  5
*edges,4,1,9,10,5  #  6
*edges,4,9,2,6,10  #  7
*edges,10,11,12,13,14,11,15,18,17,16,15  #  8
*edges,4,15,16,17,18  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,47,04  #   1 ||< ptn_b:bed8_st3
*surf,ptn_b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,56,27  #   2 ||< ptn_ak:cirArea_st3
*surf,ptn_c,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,46,04  #   3 ||< ptn_c:bed7_st3
*surf,ceiling,CEIL,-,ROOF,FLAT,Fl_roof_noti,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,27,04  #   5 ||< ceiling:bath7-8
*surf,ptn_d,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,46,03  #   6 ||< ptn_b:bed7_st3
*surf,ptn_e,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,47,05  #   7 ||< ptn_c:bed8_st3
*surf,RL_frame,CEIL,ceiling,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,RL_glz,CEIL,RL_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #   9 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,     6.00 2  # zone base list
# 
*bridge_start,0.100,0.402,0.402,4.022  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
