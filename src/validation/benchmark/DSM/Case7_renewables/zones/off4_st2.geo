*Geometry 1.1,GEN,off4_st2 # tag version, format, zone name
*date Fri May  8 11:24:28 2009  # latest file modification 
off4_st2 is a level 2 open plan office facing north
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,6.00000,12.00000,3.50000  #   1
*vertex,34.00000,12.00000,3.50000  #   2
*vertex,34.00000,18.00000,3.50000  #   3
*vertex,6.00000,18.00000,3.50000  #   4
*vertex,6.00000,12.00000,7.00000  #   5
*vertex,34.00000,12.00000,7.00000  #   6
*vertex,34.00000,18.00000,7.00000  #   7
*vertex,6.00000,18.00000,7.00000  #   8
*vertex,10.50000,12.00000,3.50000  #   9
*vertex,10.50000,12.00000,7.00000  #  10
*vertex,29.50000,12.00000,3.50000  #  11
*vertex,29.50000,12.00000,7.00000  #  12
*vertex,28.85438,18.00000,4.14320  #  13
*vertex,11.14562,18.00000,4.14320  #  14
*vertex,11.14562,18.00000,6.35680  #  15
*vertex,28.85438,18.00000,6.35680  #  16
*vertex,28.40000,18.00000,4.20000  #  17
*vertex,11.60000,18.00000,4.20000  #  18
*vertex,11.60000,18.00000,6.30000  #  19
*vertex,28.40000,18.00000,6.30000  #  20
# 
# tag, number of vertices followed by list of associated vert
*edges,4,2,3,7,6  #  1
*edges,10,3,4,8,7,3,13,16,15,14,13  #  2
*edges,4,4,1,5,8  #  3
*edges,6,5,10,12,6,7,8  #  4
*edges,6,1,4,3,2,11,9  #  5
*edges,4,1,9,10,5  #  6
*edges,4,9,11,12,10  #  7
*edges,4,11,2,6,12  #  8
*edges,10,13,14,15,16,13,17,20,19,18,17  #  9
*edges,4,17,18,19,20  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Wall-2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,10,06  #   1 ||< Wall-4a:off3_st2
*surf,Wall-3,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,Wall-4,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,08,06  #   3 ||< Wall-2a:off1_st2
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,18,05  #   4 ||< floor:off4_st3
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,04,04  #   5 ||< ceiling:office_4
*surf,Wall-1a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,12,03  #   6 ||< Wall-3:circ1_st2
*surf,Wall-1b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,14,03  #   7 ||< Wall-3:off5_st2
*surf,Wall-1c,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,13,03  #   8 ||< Wall-3:circ2_st2
*surf,win_frame,VERT,Wall-3,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,win_glz,VERT,win_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  10 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,   168.00 2  # zone base list
# 
*bridge_start,0.100,10.660,10.660,106.597  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
