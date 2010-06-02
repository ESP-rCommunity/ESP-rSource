*Geometry 1.1,GEN,intVoid_st2 # tag version, format, zone name
*date Thu Dec 11 13:51:36 2008  # latest file modification 
intVoid_st2 is 2nd level NW corner
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,7.50000,3.50000  #   1
*vertex,2.00000,7.50000,3.50000  #   2
*vertex,2.00000,11.50000,3.50000  #   3
*vertex,0.00000,11.50000,3.50000  #   4
*vertex,0.00000,7.50000,7.00000  #   5
*vertex,2.00000,7.50000,7.00000  #   6
*vertex,2.00000,11.50000,7.00000  #   7
*vertex,0.00000,11.50000,7.00000  #   8
*vertex,1.54772,11.50000,4.29149  #   9
*vertex,0.45228,11.50000,4.29149  #  10
*vertex,0.45228,11.50000,6.20851  #  11
*vertex,1.54772,11.50000,6.20851  #  12
*vertex,0.00000,10.59545,4.29149  #  13
*vertex,0.00000,8.40455,4.29149  #  14
*vertex,0.00000,8.40455,6.20851  #  15
*vertex,0.00000,10.59545,6.20851  #  16
*vertex,1.51961,11.50000,4.34068  #  17
*vertex,0.48039,11.50000,4.34068  #  18
*vertex,0.48039,11.50000,6.15932  #  19
*vertex,1.51961,11.50000,6.15932  #  20
*vertex,0.00000,10.53924,4.34068  #  21
*vertex,0.00000,8.46076,4.34068  #  22
*vertex,0.00000,8.46076,6.15932  #  23
*vertex,0.00000,10.53924,6.15932  #  24
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
*surf,ptn_a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,32,07  #   1 ||< ptn_d:cirArea_St2
*surf,ptn_b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,32,06  #   2 ||< ptn_c:cirArea_St2
*surf,north,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,west,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,41,06  #   5 ||< floor:intVoid_st3
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,05,05  #   6 ||< ceiling:int_void
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
*base_list,1,6,     8.00 0  # zone base list
# 
*bridge_start,0.100,1.895,1.895,18.953  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
