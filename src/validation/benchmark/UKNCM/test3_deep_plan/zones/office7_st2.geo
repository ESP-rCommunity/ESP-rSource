*Geometry 1.1,GEN,office7_st2 # tag version, format, zone name
*date Wed Dec 17 17:55:32 2008  # latest file modification 
office7_st2 is NW corner office on 2nd level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,44.00000,4.00000  #   1
*vertex,6.00000,44.00000,4.00000  #   2
*vertex,6.00000,50.00000,4.00000  #   3
*vertex,0.00000,50.00000,4.00000  #   4
*vertex,0.00000,44.00000,8.00000  #   5
*vertex,6.00000,44.00000,8.00000  #   6
*vertex,6.00000,50.00000,8.00000  #   7
*vertex,0.00000,50.00000,8.00000  #   8
*vertex,4.89737,50.00000,4.73509  #   9
*vertex,1.10263,50.00000,4.73509  #  10
*vertex,1.10263,50.00000,7.26491  #  11
*vertex,4.89737,50.00000,7.26491  #  12
*vertex,0.00000,48.89737,4.73509  #  13
*vertex,0.00000,45.10263,4.73509  #  14
*vertex,0.00000,45.10263,7.26491  #  15
*vertex,0.00000,48.89737,7.26491  #  16
*vertex,4.80000,50.00000,4.80000  #  17
*vertex,1.20000,50.00000,4.80000  #  18
*vertex,1.20000,50.00000,7.20000  #  19
*vertex,4.80000,50.00000,7.20000  #  20
*vertex,0.00000,48.80000,4.80000  #  21
*vertex,0.00000,45.20000,4.80000  #  22
*vertex,0.00000,45.20000,7.20000  #  23
*vertex,0.00000,48.80000,7.20000  #  24
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
*surf,ptn_o8,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,22,01  #   1 ||< ptn_o7:office8_st2
*surf,ptn_o6,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,20,04  #   2 ||< ptn_o7:office6_st2
*surf,north,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,west,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,35,06  #   5 ||< floor:office7_st3
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,07,05  #   6 ||< ceiling:office_7
*surf,winn_frame,VERT,north,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,winw_frame,VERT,west,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,winn_glz,VERT,winn_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #   9 ||< external
*surf,winw_glz,VERT,winw_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  10 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    36.00 0  # zone base list
# 
*bridge_start,0.100,5.221,5.221,52.211  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
