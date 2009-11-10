*Geometry 1.1,GEN,office_1 # tag version, format, zone name
*date Fri Dec 12 13:55:49 2008  # latest file modification 
office_1 is the SW corner office on ground level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,6.00000,0.00000,0.00000  #   2
*vertex,6.00000,6.00000,0.00000  #   3
*vertex,0.00000,6.00000,0.00000  #   4
*vertex,0.00000,0.00000,4.00000  #   5
*vertex,6.00000,0.00000,4.00000  #   6
*vertex,6.00000,6.00000,4.00000  #   7
*vertex,0.00000,6.00000,4.00000  #   8
*vertex,1.10263,0.00000,0.73509  #   9
*vertex,4.89737,0.00000,0.73509  #  10
*vertex,4.89737,0.00000,3.26491  #  11
*vertex,1.10263,0.00000,3.26491  #  12
*vertex,0.00000,4.89737,0.73509  #  13
*vertex,0.00000,1.10263,0.73509  #  14
*vertex,0.00000,1.10263,3.26491  #  15
*vertex,0.00000,4.89737,3.26491  #  16
*vertex,1.20000,0.00000,0.80000  #  17
*vertex,4.80000,0.00000,0.80000  #  18
*vertex,4.80000,0.00000,3.20000  #  19
*vertex,1.20000,0.00000,3.20000  #  20
*vertex,0.00000,4.80000,0.80000  #  21
*vertex,0.00000,1.20000,0.80000  #  22
*vertex,0.00000,1.20000,3.20000  #  23
*vertex,0.00000,4.80000,3.20000  #  24
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,6,5,1,9,12,11,10,9  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
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
*surf,south,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,ptn_a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,02,04  #   2 ||< Wall-4:office_2
*surf,ptn_b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,08,08  #   3 ||< Wall-1:office_8
*surf,west,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,15,06  #   5 ||< Base-6:office1_st2
*surf,floor,FLOR,-,FLOOR,EXTGRND,Sol_grnd_not,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,win1_frame,VERT,south,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   7 ||< external
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
*base_list,1,6,    36.00 0  # zone base list
# 
*bridge_start,0.100,6.107,6.107,61.070  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
