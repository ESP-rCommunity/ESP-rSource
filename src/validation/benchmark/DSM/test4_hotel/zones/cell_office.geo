*Geometry 1.1,GEN,cell_office # tag version, format, zone name
*date Thu May  7 12:22:28 2009  # latest file modification 
cell_office is ground level office area
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,3.50000,6.50000,0.00000  #   1
*vertex,10.50000,6.50000,0.00000  #   2
*vertex,10.50000,11.50000,0.00000  #   3
*vertex,3.50000,11.50000,0.00000  #   4
*vertex,3.50000,6.50000,3.50000  #   5
*vertex,10.50000,6.50000,3.50000  #   6
*vertex,10.50000,11.50000,3.50000  #   7
*vertex,3.50000,11.50000,3.50000  #   8
*vertex,8.91703,11.50000,0.79149  #   9
*vertex,5.08297,11.50000,0.79149  #  10
*vertex,5.08297,11.50000,2.70851  #  11
*vertex,8.91703,11.50000,2.70851  #  12
*vertex,7.00000,11.50000,3.50000  #  13
*vertex,7.00000,8.00000,3.50000  #  14
*vertex,5.00000,8.00000,3.50000  #  15
*vertex,5.00000,6.50000,3.50000  #  16
*vertex,9.00000,6.50000,3.50000  #  17
*vertex,9.00000,8.00000,3.50000  #  18
*vertex,8.81866,11.50000,0.84068  #  19
*vertex,5.18135,11.50000,0.84068  #  20
*vertex,5.18135,11.50000,2.65932  #  21
*vertex,8.81866,11.50000,2.65932  #  22
# 
# tag, number of vertices followed by list of associated vert
*edges,6,1,2,6,17,16,5  #  1
*edges,4,2,3,7,6  #  2
*edges,11,3,4,8,13,7,3,9,12,11,10,9  #  3
*edges,4,4,1,5,8  #  4
*edges,4,1,4,3,2  #  5
*edges,10,9,10,11,12,9,19,22,21,20,19  #  6
*edges,6,8,5,16,15,14,13  #  7
*edges,6,13,14,18,17,6,7  #  8
*edges,5,15,16,17,18,14  #  9
*edges,4,19,20,21,22  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_c,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,06,03  #   1 ||< ptn_c:circ_area
*surf,ptn_e,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,01,08  #   2 ||< ptn_2:eating_area
*surf,north,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,ptn-a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,06,04  #   4 ||< ptn_a:circ_area
*surf,floor,FLOR,-,FLOOR,EXTGRND,Sol_grnd_not,OPAQUE,GROUND,01,00  #   5 ||< ground profile  1
*surf,win_frame,VERT,north,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,Base-8,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,16,08  #   7 ||< floor:bed9_st2
*surf,Base-10,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,17,08  #   8 ||< floor:bed10_st2
*surf,Base-11,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,28,05  #   9 ||< floor:bath9-10
*surf,win_glz,VERT,win_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  10 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,35.00,1  # zone base
# 
*bridge_start,0.100,3.072,3.072,30.725  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
