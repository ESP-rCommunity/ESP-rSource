*Geometry 1.1,GEN,office_7 # tag version, format, zone name
*date Fri Dec 12 16:43:06 2008  # latest file modification 
office_7 is the NW corner office on ground level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,44.00000,0.00000  #   1
*vertex,6.00000,44.00000,0.00000  #   2
*vertex,6.00000,50.00000,0.00000  #   3
*vertex,0.00000,50.00000,0.00000  #   4
*vertex,0.00000,44.00000,4.00000  #   5
*vertex,6.00000,44.00000,4.00000  #   6
*vertex,6.00000,50.00000,4.00000  #   7
*vertex,0.00000,50.00000,4.00000  #   8
*vertex,4.89737,50.00000,0.73509  #   9
*vertex,1.10263,50.00000,0.73509  #  10
*vertex,1.10263,50.00000,3.26491  #  11
*vertex,4.89737,50.00000,3.26491  #  12
*vertex,4.80000,50.00000,0.80000  #  13
*vertex,1.20000,50.00000,0.80000  #  14
*vertex,1.20000,50.00000,3.20000  #  15
*vertex,4.80000,50.00000,3.20000  #  16
*vertex,0.00000,48.89737,0.73509  #  17
*vertex,0.00000,45.10263,0.73509  #  18
*vertex,0.00000,45.10263,3.26491  #  19
*vertex,0.00000,48.89737,3.26491  #  20
*vertex,0.00000,48.80000,0.80000  #  21
*vertex,0.00000,45.20000,0.80000  #  22
*vertex,0.00000,45.20000,3.20000  #  23
*vertex,0.00000,48.80000,3.20000  #  24
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,10,3,4,8,7,3,9,12,11,10,9  #  3
*edges,10,4,1,5,8,4,17,20,19,18,17  #  4
*edges,4,5,6,7,8  #  5
*edges,4,1,4,3,2  #  6
*edges,10,9,10,11,12,9,13,16,15,14,13  #  7
*edges,4,13,14,15,16  #  8
*edges,10,17,18,19,20,17,21,24,23,22,21  #  9
*edges,4,21,22,23,24  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_o8,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,08,01  #   1 ||< Wall-3:office_8
*surf,ptn_o6,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,06,04  #   2 ||< ptn_o7:office_6
*surf,north,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,west,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,21,06  #   5 ||< Base-6:office7_st2
*surf,floor,FLOR,-,FLOOR,EXTGRND,Sol_grnd_not,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,winn_frame,VERT,north,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,winn_glz,VERT,winn_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #   8 ||< external
*surf,winw_frame,VERT,west,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   9 ||< external
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
*bridge_start,0.100,6.107,6.107,61.070  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
