*Geometry 1.1,GEN,food_area # tag version, format, zone name
*date Thu May  7 12:23:26 2009  # latest file modification 
food_area is the ground level kitchen
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,24.50000,0.00000,0.00000  #   1
*vertex,31.50000,0.00000,0.00000  #   2
*vertex,31.50000,6.50000,0.00000  #   3
*vertex,24.50000,6.50000,0.00000  #   4
*vertex,24.50000,0.00000,3.50000  #   5
*vertex,31.50000,0.00000,3.50000  #   6
*vertex,31.50000,6.50000,3.50000  #   7
*vertex,24.50000,6.50000,3.50000  #   8
*vertex,26.08297,0.00000,0.79149  #   9
*vertex,29.91703,0.00000,0.79149  #  10
*vertex,29.91703,0.00000,2.70851  #  11
*vertex,26.08297,0.00000,2.70851  #  12
*vertex,31.50000,1.46990,0.79149  #  13
*vertex,31.50000,5.03010,0.79149  #  14
*vertex,31.50000,5.03010,2.70851  #  15
*vertex,31.50000,1.46990,2.70851  #  16
*vertex,24.50000,5.00000,3.50000  #  17
*vertex,26.00000,6.50000,3.50000  #  18
*vertex,30.00000,6.50000,3.50000  #  19
*vertex,31.50000,5.00000,3.50000  #  20
*vertex,30.00000,5.00000,3.50000  #  21
*vertex,26.00000,5.00000,3.50000  #  22
*vertex,26.00000,3.50000,3.50000  #  23
*vertex,28.00000,3.50000,3.50000  #  24
*vertex,28.00000,0.00000,3.50000  #  25
*vertex,30.00000,3.50000,3.50000  #  26
*vertex,26.18134,0.00000,0.84068  #  27
*vertex,29.81866,0.00000,0.84068  #  28
*vertex,29.81866,0.00000,2.65932  #  29
*vertex,26.18134,0.00000,2.65932  #  30
*vertex,31.50000,1.56125,0.84068  #  31
*vertex,31.50000,4.93875,0.84068  #  32
*vertex,31.50000,4.93875,2.65932  #  33
*vertex,31.50000,1.56125,2.65932  #  34
# 
# tag, number of vertices followed by list of associated vert
*edges,11,1,2,6,25,5,1,9,12,11,10,9  #  1
*edges,11,2,3,7,20,6,2,13,16,15,14,13  #  2
*edges,6,3,4,8,18,19,7  #  3
*edges,5,4,1,5,17,8  #  4
*edges,4,1,4,3,2  #  5
*edges,10,9,10,11,12,9,27,30,29,28,27  #  6
*edges,10,13,14,15,16,13,31,34,33,32,31  #  7
*edges,8,8,17,22,21,20,7,19,18  #  8
*edges,6,17,5,25,24,23,22  #  9
*edges,6,24,25,6,20,21,26  # 10
*edges,5,22,23,24,26,21  # 11
*edges,4,27,28,29,30  # 12
*edges,4,31,32,33,34  # 13
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,south,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,east,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,ptn_a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,03,01  #   3 ||< ptn_a:store_area
*surf,ptn_b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,01,05  #   4 ||< ptn-2b:eating_area
*surf,floor,FLOR,-,FLOOR,EXTGRND,Sol_grnd_not,OPAQUE,GROUND,01,00  #   5 ||< ground profile  1
*surf,win1_frame,VERT,south,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,win2_frame,VERT,east,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,Base-14,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,32,41  #   8 ||< floor_c:cirArea_St2
*surf,Base-8,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,14,08  #   9 ||< floor:bed7_st2
*surf,Base-9,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,15,08  #  10 ||< floor:bed8_st2
*surf,Base-10,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,27,05  #  11 ||< floor:bath7-8
*surf,win1_glz,VERT,win1_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  12 ||< external
*surf,win2_glz,VERT,win2_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  13 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,45.50,1  # zone base
# 
*bridge_start,0.100,5.384,5.384,53.841  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
