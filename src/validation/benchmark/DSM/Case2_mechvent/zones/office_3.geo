*Geometry 1.1,GEN,office_3 # tag version, format, zone name
*date Fri May  8 11:23:21 2009  # latest file modification 
office_3 is ground level east facing
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,34.00000,0.00000,0.00000  #   1
*vertex,40.00000,0.00000,0.00000  #   2
*vertex,40.00000,18.00000,0.00000  #   3
*vertex,34.00000,18.00000,0.00000  #   4
*vertex,34.00000,0.00000,3.50000  #   5
*vertex,40.00000,0.00000,3.50000  #   6
*vertex,40.00000,18.00000,3.50000  #   7
*vertex,34.00000,18.00000,3.50000  #   8
*vertex,34.00000,12.00000,0.00000  #   9
*vertex,34.00000,12.00000,3.50000  #  10
*vertex,34.00000,6.00000,0.00000  #  11
*vertex,34.00000,6.00000,3.50000  #  12
*vertex,35.10263,0.00000,0.64320  #  13
*vertex,38.89737,0.00000,0.64320  #  14
*vertex,38.89737,0.00000,2.85680  #  15
*vertex,35.10263,0.00000,2.85680  #  16
*vertex,40.00000,3.30790,0.64320  #  17
*vertex,40.00000,14.69210,0.64320  #  18
*vertex,40.00000,14.69210,2.85680  #  19
*vertex,40.00000,3.30790,2.85680  #  20
*vertex,38.89737,18.00000,0.64320  #  21
*vertex,35.10263,18.00000,0.64320  #  22
*vertex,35.10263,18.00000,2.85680  #  23
*vertex,38.89737,18.00000,2.85680  #  24
*vertex,35.20000,0.00000,0.70000  #  25
*vertex,38.80000,0.00000,0.70000  #  26
*vertex,38.80000,0.00000,2.80000  #  27
*vertex,35.20000,0.00000,2.80000  #  28
*vertex,40.00000,3.60000,0.70000  #  29
*vertex,40.00000,14.40000,0.70000  #  30
*vertex,40.00000,14.40000,2.80000  #  31
*vertex,40.00000,3.60000,2.80000  #  32
*vertex,38.80000,18.00000,0.70000  #  33
*vertex,35.20000,18.00000,0.70000  #  34
*vertex,35.20000,18.00000,2.80000  #  35
*vertex,38.80000,18.00000,2.80000  #  36
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,6,5,1,13,16,15,14,13  #  1
*edges,10,2,3,7,6,2,17,20,19,18,17  #  2
*edges,10,3,4,8,7,3,21,24,23,22,21  #  3
*edges,6,5,6,7,8,10,12  #  4
*edges,6,1,11,9,4,3,2  #  5
*edges,4,4,9,10,8  #  6
*edges,4,9,11,12,10  #  7
*edges,4,11,1,5,12  #  8
*edges,10,13,14,15,16,13,25,28,27,26,25  #  9
*edges,10,17,18,19,20,17,29,32,31,30,29  # 10
*edges,10,21,22,23,24,21,33,36,35,34,33  # 11
*edges,4,25,26,27,28  # 12
*edges,4,29,30,31,32  # 13
*edges,4,33,34,35,36  # 14
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Wall-1,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,Wall-2,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,Wall-3,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,10,05  #   4 ||< floor:off3_st2
*surf,floor,FLOR,-,FLOOR,EXTGRND,Sol_grnd_not,OPAQUE,GROUND,01,00  #   5 ||< ground profile  1
*surf,Wall-4a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,04,01  #   6 ||< Wall-2:office_4
*surf,Wall-4b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,06,02  #   7 ||< Wall-2:circ_area2
*surf,Wall-4c,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,02,02  #   8 ||< Wall-2:office_2
*surf,win1_frame,VERT,Wall-1,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,win2_frame,VERT,Wall-2,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,win3_frame,VERT,Wall-3,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,win1_glz,VERT,win1_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  12 ||< external
*surf,win2_glz,VERT,win2_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  13 ||< external
*surf,win3_glz,VERT,win3_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  14 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,   108.00 2  # zone base list
# 
*bridge_start,0.100,14.082,14.082,140.817  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
