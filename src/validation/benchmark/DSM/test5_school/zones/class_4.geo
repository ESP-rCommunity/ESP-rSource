*Geometry 1.1,GEN,class_4 # tag version, format, zone name
*date Fri Dec 12 08:55:07 2008  # latest file modification 
class_4 describes class 4
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,32.00000,0.00000,0.00000  #   1
*vertex,40.00000,0.00000,0.00000  #   2
*vertex,40.00000,6.00000,0.00000  #   3
*vertex,32.00000,6.00000,0.00000  #   4
*vertex,32.00000,0.00000,3.50000  #   5
*vertex,40.00000,0.00000,3.50000  #   6
*vertex,40.00000,6.00000,3.50000  #   7
*vertex,32.00000,6.00000,3.50000  #   8
*vertex,34.21115,1.65836,3.50000  #   9
*vertex,37.78885,1.65836,3.50000  #  10
*vertex,37.78885,4.34164,3.50000  #  11
*vertex,34.21115,4.34164,3.50000  #  12
*vertex,33.47018,0.00000,0.64320  #  13
*vertex,38.52982,0.00000,0.64320  #  14
*vertex,38.52982,0.00000,2.85680  #  15
*vertex,33.47018,0.00000,2.85680  #  16
*vertex,34.50334,1.87750,3.50000  #  17
*vertex,37.49666,1.87750,3.50000  #  18
*vertex,37.49666,4.12250,3.50000  #  19
*vertex,34.50334,4.12250,3.50000  #  20
*vertex,33.60000,0.00000,0.70000  #  21
*vertex,38.40000,0.00000,0.70000  #  22
*vertex,38.40000,0.00000,2.80000  #  23
*vertex,33.60000,0.00000,2.80000  #  24
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,6,5,1,13,16,15,14,13  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,10,5,6,7,8,5,9,12,11,10,9  #  5
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
*surf,ptn_a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,06,04  #   2 ||< Wall-4:toilet
*surf,ptn_b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,07,14  #   3 ||< Wall-1e:circ_area
*surf,ptn_c,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,04,02  #   4 ||< ptn_a:class_3
*surf,ceiling,CEIL,-,ROOF,FLAT,Fl_roof_noti,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,floor,FLOR,-,FLOOR,EXTGRND,Sol_grnd_not,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,RL_frame,CEIL,ceiling,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,win_frame,VERT,south,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,rooflight,CEIL,RL_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #   9 ||< external
*surf,window,VERT,win_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  10 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    48.00 0  # zone base list
# 
*bridge_start,0.100,7.445,7.445,74.448  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
