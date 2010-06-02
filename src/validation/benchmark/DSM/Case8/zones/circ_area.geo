*Geometry 1.1,GEN,circ_area # tag version, format, zone name
*date Thu May  7 11:47:02 2009  # latest file modification 
circ_area is circulation space at south west corner
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,10.50000,0.00000,0.00000  #   2
*vertex,10.50000,6.50000,0.00000  #   3
*vertex,3.50000,6.50000,0.00000  #   4
*vertex,3.50000,11.50000,0.00000  #   5
*vertex,2.00000,11.50000,0.00000  #   6
*vertex,2.00000,7.50000,0.00000  #   7
*vertex,0.00000,7.50000,0.00000  #   8
*vertex,0.00000,0.00000,3.50000  #   9
*vertex,10.50000,0.00000,3.50000  #  10
*vertex,10.50000,6.50000,3.50000  #  11
*vertex,3.50000,6.50000,3.50000  #  12
*vertex,3.50000,11.50000,3.50000  #  13
*vertex,2.00000,11.50000,3.50000  #  14
*vertex,2.00000,7.50000,3.50000  #  15
*vertex,0.00000,7.50000,3.50000  #  16
*vertex,3.16079,11.50000,0.79149  #  17
*vertex,2.33921,11.50000,0.79149  #  18
*vertex,2.33921,11.50000,2.70851  #  19
*vertex,3.16079,11.50000,2.70851  #  20
*vertex,0.00000,5.80396,0.79149  #  21
*vertex,0.00000,1.69604,0.79149  #  22
*vertex,0.00000,1.69604,2.70851  #  23
*vertex,0.00000,5.80396,2.70851  #  24
*vertex,2.37446,0.00000,0.79149  #  25
*vertex,8.12554,0.00000,0.79149  #  26
*vertex,8.12554,0.00000,2.70851  #  27
*vertex,2.37446,0.00000,2.70851  #  28
*vertex,5.00000,6.50000,3.50000  #  29
*vertex,9.00000,6.50000,3.50000  #  30
*vertex,10.50000,5.00000,3.50000  #  31
*vertex,9.00000,5.00000,3.50000  #  32
*vertex,5.00000,5.00000,3.50000  #  33
*vertex,3.50000,5.00000,3.50000  #  34
*vertex,3.50000,0.00000,3.50000  #  35
*vertex,5.00000,3.50000,3.50000  #  36
*vertex,7.00000,3.50000,3.50000  #  37
*vertex,7.00000,0.00000,3.50000  #  38
*vertex,9.00000,3.50000,3.50000  #  39
*vertex,3.13971,11.50000,0.84068  #  40
*vertex,2.36029,11.50000,0.84068  #  41
*vertex,2.36029,11.50000,2.65932  #  42
*vertex,3.13971,11.50000,2.65932  #  43
*vertex,0.00000,5.69856,0.84068  #  44
*vertex,0.00000,1.80144,0.84068  #  45
*vertex,0.00000,1.80144,2.65932  #  46
*vertex,0.00000,5.69856,2.65932  #  47
*vertex,2.52202,0.00000,0.84068  #  48
*vertex,7.97798,0.00000,0.84068  #  49
*vertex,7.97798,0.00000,2.65932  #  50
*vertex,2.52202,0.00000,2.65932  #  51
# 
# tag, number of vertices followed by list of associated vert
*edges,12,1,2,10,38,35,9,1,25,28,27,26,25  #  1
*edges,5,2,3,11,31,10  #  2
*edges,6,3,4,12,29,30,11  #  3
*edges,4,4,5,13,12  #  4
*edges,10,5,6,14,13,5,17,20,19,18,17  #  5
*edges,4,6,7,15,14  #  6
*edges,4,7,8,16,15  #  7
*edges,10,8,1,9,16,8,21,24,23,22,21  #  8
*edges,8,1,8,7,6,5,4,3,2  #  9
*edges,10,17,18,19,20,17,40,43,42,41,40  # 10
*edges,10,21,22,23,24,21,44,47,46,45,44  # 11
*edges,10,25,26,27,28,25,48,51,50,49,48  # 12
*edges,14,16,9,35,34,33,32,31,11,30,29,12,13,14,15  # 13
*edges,6,34,35,38,37,36,33  # 14
*edges,6,37,38,10,31,32,39  # 15
*edges,5,33,36,37,39,32  # 16
*edges,4,40,41,42,43  # 17
*edges,4,44,45,46,47  # 18
*edges,4,48,49,50,51  # 19
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,south,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,ptn_2d,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,01,09  #   2 ||< ptn-2d:eating_area
*surf,ptn_c,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,04,01  #   3 ||< ptn_c:cell_office
*surf,ptn_a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,04,04  #   4 ||< ptn-a:cell_office
*surf,north,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,ptn_b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,05,02  #   6 ||< ptn_cb:int_void
*surf,ptn_d,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,05,01  #   7 ||< ptn_ca:int_void
*surf,west,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,floor,FLOR,-,FLOOR,EXTGRND,Sol_grnd_not,OPAQUE,GROUND,01,00  #   9 ||< ground profile  1
*surf,win1_frame,VERT,north,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,win2_frame,VERT,west,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,win3_frame,VERT,south,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,Base-12,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,32,10  #  13 ||< floor_a:cirArea_St2
*surf,Base-8,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,08,08  #  14 ||< floor:bed1_st2
*surf,Base-9,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,09,08  #  15 ||< floor:bed2_st2
*surf,Base-6,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,24,05  #  16 ||< floor:bath1-2
*surf,win1_glz,VERT,win1_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  17 ||< external
*surf,win2_glz,VERT,win2_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  18 ||< external
*surf,win3_glz,VERT,win3_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  19 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,9,    77.75 2  # zone base list
# 
*bridge_start,0.100,8.073,8.073,80.730  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
