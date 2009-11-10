*Geometry 1.1,GEN,office_8 # tag version, format, zone name
*date Fri Dec 12 16:46:39 2008  # latest file modification 
office_8 is west facing perimeter office on ground level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,6.00000,0.00000  #   1
*vertex,6.00000,6.00000,0.00000  #   2
*vertex,6.00000,44.00000,0.00000  #   3
*vertex,0.00000,44.00000,0.00000  #   4
*vertex,0.00000,6.00000,4.00000  #   5
*vertex,6.00000,6.00000,4.00000  #   6
*vertex,6.00000,44.00000,4.00000  #   7
*vertex,0.00000,44.00000,4.00000  #   8
*vertex,0.00000,37.01665,0.73509  #   9
*vertex,0.00000,12.98335,0.73509  #  10
*vertex,0.00000,12.98335,3.26491  #  11
*vertex,0.00000,37.01665,3.26491  #  12
*vertex,6.00000,25.00000,0.00000  #  13
*vertex,6.00000,25.00000,4.00000  #  14
*vertex,0.00000,36.39999,0.80000  #  15
*vertex,0.00000,13.60000,0.80000  #  16
*vertex,0.00000,13.60000,3.20000  #  17
*vertex,0.00000,36.39999,3.20000  #  18
# 
# tag, number of vertices followed by list of associated vert
*edges,4,3,4,8,7  #  1
*edges,10,4,1,5,8,4,9,12,11,10,9  #  2
*edges,5,5,6,14,7,8  #  3
*edges,5,1,4,3,13,2  #  4
*edges,10,9,10,11,12,9,15,18,17,16,15  #  5
*edges,4,2,13,14,6  #  6
*edges,4,13,3,7,14  #  7
*edges,4,1,2,6,5  #  8
*edges,4,15,16,17,18  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_o7,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,07,01  #   1 ||< ptn_o8:office_7
*surf,west,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,ceiling,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,22,04  #   3 ||< Base-6:office8_st2
*surf,floor,FLOR,-,FLOOR,EXTGRND,Sol_grnd_not,OPAQUE,GROUND,01,00  #   4 ||< ground profile  1
*surf,win_frame,VERT,west,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,ptn_o9,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,13,06  #   6 ||< Wall-8:office_9
*surf,ptn_o10,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,14,06  #   7 ||< Wall-8:office_10
*surf,ptn_o1,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,01,03  #   8 ||< ptn_b:office_1
*surf,win_glz,VERT,win_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #   9 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,4,   228.00 0  # zone base list
# 
*bridge_start,0.100,22.144,22.144,221.444  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
