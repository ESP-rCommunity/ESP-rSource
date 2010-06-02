*Geometry 1.1,GEN,store_area # tag version, format, zone name
*date Thu May  7 11:40:41 2009  # latest file modification 
store_area describes a ground level storage
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,24.50000,6.50000,0.00000  #   1
*vertex,31.50000,6.50000,0.00000  #   2
*vertex,31.50000,11.50000,0.00000  #   3
*vertex,24.50000,11.50000,0.00000  #   4
*vertex,24.50000,6.50000,3.50000  #   5
*vertex,31.50000,6.50000,3.50000  #   6
*vertex,31.50000,11.50000,3.50000  #   7
*vertex,24.50000,11.50000,3.50000  #   8
*vertex,31.50000,7.63069,0.79149  #   9
*vertex,31.50000,10.36931,0.79149  #  10
*vertex,31.50000,10.36931,2.70851  #  11
*vertex,31.50000,7.63069,2.70851  #  12
*vertex,29.91703,11.50000,0.79149  #  13
*vertex,26.08297,11.50000,0.79149  #  14
*vertex,26.08297,11.50000,2.70851  #  15
*vertex,29.91703,11.50000,2.70851  #  16
*vertex,28.00000,11.50000,3.50000  #  17
*vertex,28.00000,8.00000,3.50000  #  18
*vertex,26.00000,8.00000,3.50000  #  19
*vertex,26.00000,6.50000,3.50000  #  20
*vertex,30.00000,6.50000,3.50000  #  21
*vertex,30.00000,8.00000,3.50000  #  22
*vertex,31.50000,7.70096,0.84068  #  23
*vertex,31.50000,10.29904,0.84068  #  24
*vertex,31.50000,10.29904,2.65932  #  25
*vertex,31.50000,7.70096,2.65932  #  26
*vertex,29.81866,11.50000,0.84068  #  27
*vertex,26.18134,11.50000,0.84068  #  28
*vertex,26.18134,11.50000,2.65932  #  29
*vertex,29.81866,11.50000,2.65932  #  30
# 
# tag, number of vertices followed by list of associated vert
*edges,6,1,2,6,21,20,5  #  1
*edges,10,2,3,7,6,2,9,12,11,10,9  #  2
*edges,11,3,4,8,17,7,3,13,16,15,14,13  #  3
*edges,4,4,1,5,8  #  4
*edges,4,1,4,3,2  #  5
*edges,10,9,10,11,12,9,23,26,25,24,23  #  6
*edges,10,13,14,15,16,13,27,30,29,28,27  #  7
*edges,6,8,5,20,19,18,17  #  8
*edges,6,17,18,22,21,6,7  #  9
*edges,5,19,20,21,22,18  # 10
*edges,4,23,24,25,26  # 11
*edges,4,27,28,29,30  # 12
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,02,03  #   1 ||< ptn_a:food_area
*surf,east,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,north,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,ptn_b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,01,04  #   4 ||< ptn-2a:eating_area
*surf,floor,FLOR,-,FLOOR,EXTGRND,Sol_grnd_not,OPAQUE,GROUND,01,00  #   5 ||< ground profile  1
*surf,win1_frame,VERT,east,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,win2_frame,VERT,north,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,Base-8,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,22,08  #   8 ||< floor:bed15_st2
*surf,Base-9,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,23,08  #   9 ||< floor:bed16_st2
*surf,Base-10,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,31,05  #  10 ||< floor:bath15-16
*surf,win1_glz,VERT,win1_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  11 ||< external
*surf,win2_glz,VERT,win2_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  12 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,    35.00 2  # zone base list
# 
*bridge_start,0.100,4.652,4.652,46.519  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
