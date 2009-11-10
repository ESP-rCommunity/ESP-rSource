*Geometry 1.1,GEN,bed8_st2 # tag version, format, zone name
*date Thu May  7 11:34:47 2009  # latest file modification 
bed8_st2 is guest room eight on 2nd level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,28.00000,0.00000,3.50000  #   1
*vertex,31.50000,0.00000,3.50000  #   2
*vertex,31.50000,5.00000,3.50000  #   3
*vertex,30.00000,5.00000,3.50000  #   4
*vertex,30.00000,3.50000,3.50000  #   5
*vertex,28.00000,3.50000,3.50000  #   6
*vertex,28.00000,0.00000,7.00000  #   7
*vertex,31.50000,0.00000,7.00000  #   8
*vertex,31.50000,5.00000,7.00000  #   9
*vertex,30.00000,5.00000,7.00000  #  10
*vertex,30.00000,3.50000,7.00000  #  11
*vertex,28.00000,3.50000,7.00000  #  12
*vertex,28.79148,0.00000,4.29149  #  13
*vertex,30.70852,0.00000,4.29149  #  14
*vertex,30.70852,0.00000,6.20851  #  15
*vertex,28.79148,0.00000,6.20851  #  16
*vertex,31.50000,1.13069,4.29149  #  17
*vertex,31.50000,3.86931,4.29149  #  18
*vertex,31.50000,3.86931,6.20851  #  19
*vertex,31.50000,1.13069,6.20851  #  20
*vertex,28.84067,0.00000,4.34068  #  21
*vertex,30.65933,0.00000,4.34068  #  22
*vertex,30.65933,0.00000,6.15932  #  23
*vertex,28.84067,0.00000,6.15932  #  24
*vertex,31.50000,1.20096,4.34068  #  25
*vertex,31.50000,3.79904,4.34068  #  26
*vertex,31.50000,3.79904,6.15932  #  27
*vertex,31.50000,1.20096,6.15932  #  28
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,8,7,1,13,16,15,14,13  #  1
*edges,10,2,3,9,8,2,17,20,19,18,17  #  2
*edges,4,3,4,10,9  #  3
*edges,4,4,5,11,10  #  4
*edges,4,5,6,12,11  #  5
*edges,4,6,1,7,12  #  6
*edges,6,7,8,9,10,11,12  #  7
*edges,6,1,6,5,4,3,2  #  8
*edges,10,13,14,15,16,13,21,24,23,22,21  #  9
*edges,10,17,18,19,20,17,25,28,27,26,25  # 10
*edges,4,21,22,23,24  # 11
*edges,4,25,26,27,28  # 12
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,south,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,east,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,ptn_a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,32,28  #   3 ||< ptn_bm:cirArea_St2
*surf,ptn_b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,27,01  #   4 ||< ptn_a:bath7-8
*surf,ptn_c,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,27,07  #   5 ||< ptn_e:bath7-8
*surf,ptn_d,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,14,02  #   6 ||< ptn_a:bed7_st2
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,47,08  #   7 ||< floor:bed8_st3
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,02,10  #   8 ||< Base-9:food_area
*surf,win_1_frame,VERT,south,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,win_2_frame,VERT,east,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,win1_glz,VERT,win_1_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  11 ||< external
*surf,win2_glz,VERT,win_2_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  12 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,14.50,1  # zone base
# 
*bridge_start,0.100,2.685,2.685,26.850  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
