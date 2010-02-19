*Geometry 1.1,GEN,intVoid_st3 # tag version, format, zone name
*date Thu May  7 11:46:02 2009  # latest file modification 
intVoid_st3 is the NW corner of 3rd level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,7.50000,7.00000  #   1
*vertex,2.00000,7.50000,7.00000  #   2
*vertex,2.00000,11.50000,7.00000  #   3
*vertex,0.00000,11.50000,7.00000  #   4
*vertex,0.00000,7.50000,10.50000  #   5
*vertex,2.00000,7.50000,10.50000  #   6
*vertex,2.00000,11.50000,10.50000  #   7
*vertex,0.00000,11.50000,10.50000  #   8
*vertex,1.54772,11.50000,7.79149  #   9
*vertex,0.45228,11.50000,7.79149  #  10
*vertex,0.45228,11.50000,9.70851  #  11
*vertex,1.54772,11.50000,9.70851  #  12
*vertex,0.00000,10.59545,7.79149  #  13
*vertex,0.00000,8.40455,7.79149  #  14
*vertex,0.00000,8.40455,9.70851  #  15
*vertex,0.00000,10.59545,9.70851  #  16
*vertex,0.55279,8.60557,10.50000  #  17
*vertex,1.44721,8.60557,10.50000  #  18
*vertex,1.44721,10.39443,10.50000  #  19
*vertex,0.55279,10.39443,10.50000  #  20
*vertex,1.51961,11.50000,7.84068  #  21
*vertex,0.48039,11.50000,7.84068  #  22
*vertex,0.48039,11.50000,9.65932  #  23
*vertex,1.51961,11.50000,9.65932  #  24
*vertex,0.00000,10.53924,7.84068  #  25
*vertex,0.00000,8.46076,7.84068  #  26
*vertex,0.00000,8.46076,9.65932  #  27
*vertex,0.00000,10.53924,9.65932  #  28
*vertex,0.62584,8.75167,10.50000  #  29
*vertex,1.37416,8.75167,10.50000  #  30
*vertex,1.37416,10.24833,10.50000  #  31
*vertex,0.62584,10.24833,10.50000  #  32
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,10,3,4,8,7,3,9,12,11,10,9  #  3
*edges,10,4,1,5,8,4,13,16,15,14,13  #  4
*edges,10,5,6,7,8,5,17,20,19,18,17  #  5
*edges,4,1,4,3,2  #  6
*edges,10,9,10,11,12,9,21,24,23,22,21  #  7
*edges,10,13,14,15,16,13,25,28,27,26,25  #  8
*edges,10,17,18,19,20,17,29,32,31,30,29  #  9
*edges,4,21,22,23,24  # 10
*edges,4,25,26,27,28  # 11
*edges,4,29,30,31,32  # 12
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,56,07  #   1 ||< ptn_d:cirArea_st3
*surf,ptn_b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,56,06  #   2 ||< ptn_c:cirArea_st3
*surf,north,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,west,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,ceiling,CEIL,-,ROOF,FLAT,Fl_roof_noti,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,07,05  #   6 ||< ceiling:intVoid_st2
*surf,winn_frame,VERT,north,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,winw_frame,VERT,west,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,RL_frame,CEIL,ceiling,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,winn_glz,VERT,winn_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  10 ||< external
*surf,winw_glz,VERT,winw_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  11 ||< external
*surf,RL_glz,CEIL,RL_frame,S-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  12 ||< external
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
*bridge_start,0.100,2.432,2.432,24.316  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
