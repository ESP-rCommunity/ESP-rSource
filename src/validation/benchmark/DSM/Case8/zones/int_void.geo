*Geometry 1.1,GEN,int_void # tag version, format, zone name
*date Thu May  7 11:41:12 2009  # latest file modification 
int_void is at the north west corner
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,7.50000,0.00000  #   1
*vertex,2.00000,7.50000,0.00000  #   2
*vertex,2.00000,11.50000,0.00000  #   3
*vertex,0.00000,11.50000,0.00000  #   4
*vertex,0.00000,7.50000,3.50000  #   5
*vertex,2.00000,7.50000,3.50000  #   6
*vertex,2.00000,11.50000,3.50000  #   7
*vertex,0.00000,11.50000,3.50000  #   8
*vertex,1.54772,11.50000,0.79149  #   9
*vertex,0.45228,11.50000,0.79149  #  10
*vertex,0.45228,11.50000,2.70851  #  11
*vertex,1.54772,11.50000,2.70851  #  12
*vertex,0.00000,10.59545,0.79149  #  13
*vertex,0.00000,8.40455,0.79149  #  14
*vertex,0.00000,8.40455,2.70851  #  15
*vertex,0.00000,10.59545,2.70851  #  16
*vertex,1.51961,11.50000,0.84068  #  17
*vertex,0.48039,11.50000,0.84068  #  18
*vertex,0.48039,11.50000,2.65932  #  19
*vertex,1.51961,11.50000,2.65932  #  20
*vertex,0.00000,10.53924,0.84068  #  21
*vertex,0.00000,8.46076,0.84068  #  22
*vertex,0.00000,8.46076,2.65932  #  23
*vertex,0.00000,10.53924,2.65932  #  24
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,10,3,4,8,7,3,9,12,11,10,9  #  3
*edges,10,4,1,5,8,4,13,16,15,14,13  #  4
*edges,4,5,6,7,8  #  5
*edges,4,1,4,3,2  #  6
*edges,10,9,10,11,12,9,17,20,19,18,17  #  7
*edges,10,13,14,15,16,13,21,24,23,22,21  #  8
*edges,4,17,18,19,20  #  9
*edges,4,21,22,23,24  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_ca,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,06,07  #   1 ||< ptn_d:circ_area
*surf,ptn_cb,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,06,06  #   2 ||< ptn_b:circ_area
*surf,north,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,west,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,07,06  #   5 ||< floor:intVoid_st2
*surf,floor,FLOR,-,FLOOR,EXTGRND,Sol_grnd_not,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,win1_frame,VERT,north,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,win2_frame,VERT,west,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,win1_glz,VERT,win1_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #   9 ||< external
*surf,win2_glz,VERT,win2_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  10 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,     8.00 2  # zone base list
# 
*bridge_start,0.100,2.092,2.092,20.922  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
