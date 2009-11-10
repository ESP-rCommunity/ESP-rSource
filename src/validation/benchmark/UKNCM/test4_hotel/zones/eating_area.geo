*Geometry 1.1,GEN,eating_area # tag version, format, zone name
*date Thu May  7 12:25:23 2009  # latest file modification 
eating_area is ground level dining room
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,10.50000,0.00000,0.00000  #   1
*vertex,24.50000,0.00000,0.00000  #   2
*vertex,24.50000,11.50000,0.00000  #   3
*vertex,10.50000,11.50000,0.00000  #   4
*vertex,10.50000,0.00000,3.50000  #   5
*vertex,24.50000,0.00000,3.50000  #   6
*vertex,24.50000,11.50000,3.50000  #   7
*vertex,10.50000,11.50000,3.50000  #   8
*vertex,24.50000,6.50000,0.00000  #   9
*vertex,24.50000,6.50000,3.50000  #  10
*vertex,13.66594,0.00000,0.79149  #  11
*vertex,21.33406,0.00000,0.79149  #  12
*vertex,21.33406,0.00000,2.70851  #  13
*vertex,13.66594,0.00000,2.70851  #  14
*vertex,21.33406,11.50000,0.79149  #  15
*vertex,13.66594,11.50000,0.79149  #  16
*vertex,13.66594,11.50000,2.70851  #  17
*vertex,21.33406,11.50000,2.70851  #  18
*vertex,10.50000,6.50000,0.00000  #  19
*vertex,10.50000,6.50000,3.50000  #  20
*vertex,10.50000,5.00000,3.50000  #  21
*vertex,12.00000,6.50000,3.50000  #  22
*vertex,16.00000,6.50000,3.50000  #  23
*vertex,17.50000,6.50000,3.50000  #  24
*vertex,19.00000,6.50000,3.50000  #  25
*vertex,23.00000,6.50000,3.50000  #  26
*vertex,24.50000,5.00000,3.50000  #  27
*vertex,23.00000,5.00000,3.50000  #  28
*vertex,19.00000,5.00000,3.50000  #  29
*vertex,17.50000,5.00000,3.50000  #  30
*vertex,16.00000,5.00000,3.50000  #  31
*vertex,12.00000,5.00000,3.50000  #  32
*vertex,12.00000,3.50000,3.50000  #  33
*vertex,14.00000,3.50000,3.50000  #  34
*vertex,14.00000,0.00000,3.50000  #  35
*vertex,16.00000,3.50000,3.50000  #  36
*vertex,17.50000,0.00000,3.50000  #  37
*vertex,19.00000,3.50000,3.50000  #  38
*vertex,21.00000,3.50000,3.50000  #  39
*vertex,21.00000,0.00000,3.50000  #  40
*vertex,23.00000,3.50000,3.50000  #  41
*vertex,14.00000,11.50000,3.50000  #  42
*vertex,14.00000,8.00000,3.50000  #  43
*vertex,12.00000,8.00000,3.50000  #  44
*vertex,17.50000,11.50000,3.50000  #  45
*vertex,16.00000,8.00000,3.50000  #  46
*vertex,21.00000,11.50000,3.50000  #  47
*vertex,21.00000,8.00000,3.50000  #  48
*vertex,19.00000,8.00000,3.50000  #  49
*vertex,23.00000,8.00000,3.50000  #  50
*vertex,13.86269,0.00000,0.84068  #  51
*vertex,21.13731,0.00000,0.84068  #  52
*vertex,21.13731,0.00000,2.65932  #  53
*vertex,13.86269,0.00000,2.65932  #  54
*vertex,21.13731,11.50000,0.84068  #  55
*vertex,13.86269,11.50000,0.84068  #  56
*vertex,13.86269,11.50000,2.65932  #  57
*vertex,21.13731,11.50000,2.65932  #  58
# 
# tag, number of vertices followed by list of associated vert
*edges,13,1,2,6,40,37,35,5,1,11,14,13,12,11  #  1
*edges,13,3,4,8,42,45,47,7,3,15,18,17,16,15  #  2
*edges,6,1,19,4,3,9,2  #  3
*edges,4,9,3,7,10  #  4
*edges,5,2,9,10,27,6  #  5
*edges,10,11,12,13,14,11,51,54,53,52,51  #  6
*edges,10,15,16,17,18,15,55,58,57,56,55  #  7
*edges,4,4,19,20,8  #  8
*edges,5,19,1,5,21,20  #  9
*edges,14,20,21,32,31,30,29,28,27,10,26,25,24,23,22  # 10
*edges,6,21,5,35,34,33,32  # 11
*edges,6,34,35,37,30,31,36  # 12
*edges,5,32,33,34,36,31  # 13
*edges,6,30,37,40,39,38,29  # 14
*edges,6,39,40,6,27,28,41  # 15
*edges,5,29,38,39,41,28  # 16
*edges,6,8,20,22,44,43,42  # 17
*edges,6,42,43,46,23,24,45  # 18
*edges,5,44,22,23,46,43  # 19
*edges,6,45,24,25,49,48,47  # 20
*edges,6,47,48,50,26,10,7  # 21
*edges,5,49,25,26,50,48  # 22
*edges,4,51,52,53,54  # 23
*edges,4,55,56,57,58  # 24
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,south,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,north,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,floor,FLOR,-,FLOOR,EXTGRND,Sol_grnd_not,OPAQUE,GROUND,01,00  #   3 ||< ground profile  1
*surf,ptn-2a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,03,04  #   4 ||< ptn_b:store_area
*surf,ptn-2b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,02,04  #   5 ||< ptn_b:food_area
*surf,win1_frame,VERT,south,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,win2_frame,VERT,north,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,ptn_2,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,04,02  #   8 ||< ptn_e:cell_office
*surf,ptn-2d,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,06,02  #   9 ||< ptn_2d:circ_area
*surf,Base-13,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,32,16  #  10 ||< floor_b:cirArea_St2
*surf,Base-8,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,10,08  #  11 ||< floor:bed3_st2
*surf,Base-9,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,11,08  #  12 ||< floor:bed4_st2
*surf,Base-10,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,25,05  #  13 ||< floor:bath3-4
*surf,Base-11,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,12,08  #  14 ||< floor:bed5_st2
*surf,Base-12,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,13,08  #  15 ||< floor:bed6_st2
*surf,Base13b,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,26,05  #  16 ||< floor:bath5-6
*surf,Base-14,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,18,08  #  17 ||< floor:bed11_st2
*surf,Base-15,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,19,08  #  18 ||< floor:bed12_st2
*surf,Base-16,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,29,05  #  19 ||< floor:bath11-12
*surf,Base-17,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,20,08  #  20 ||< floor:bed13_st2
*surf,Base17b,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,21,08  #  21 ||< floor:bed14_st2
*surf,Base-19,CEIL,-,PARTN,-,inv_pa_fl_no,OPAQUE,ANOTHER,30,05  #  22 ||< floor:bath13-14
*surf,win1_glz,VERT,win1_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  23 ||< external
*surf,win2_glz,VERT,win2_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  24 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,161.00,1  # zone base
# 
*bridge_start,0.100,12.807,12.807,128.067  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
