*Geometry 1.1,GEN,office_e_3 # tag version, format, zone name
*date Mon Apr  2 09:21:23 2012  # latest file modification 
office_e_3 is the office on the 4th level east side
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,25.00000,8.00000,11.90000  #   1
*vertex,40.00000,8.00000,11.90000  #   2
*vertex,40.00000,26.00000,11.90000  #   3
*vertex,25.00000,26.00000,11.90000  #   4
*vertex,29.00000,14.00000,11.90000  #   5
*vertex,25.00000,14.00000,11.90000  #   6
*vertex,25.00000,8.00000,14.70000  #   7
*vertex,40.00000,8.00000,14.70000  #   8
*vertex,40.00000,26.00000,14.70000  #   9
*vertex,25.00000,26.00000,14.70000  #  10
*vertex,29.00000,14.00000,14.70000  #  11
*vertex,25.00000,14.00000,14.70000  #  12
*vertex,39.90000,26.00000,12.70000  #  13
*vertex,39.90000,26.00000,14.10000  #  14
*vertex,40.00000,12.00000,12.70000  #  15
*vertex,40.00000,15.00000,12.70000  #  16
*vertex,40.00000,15.00000,14.10000  #  17
*vertex,40.00000,12.00000,14.10000  #  18
*vertex,40.00000,20.00000,12.70000  #  19
*vertex,40.00000,20.00000,14.10000  #  20
*vertex,27.00000,26.00000,12.70000  #  21
*vertex,27.00000,26.00000,14.10000  #  22
*vertex,40.00000,17.30000,12.70000  #  23
*vertex,40.00000,17.30000,14.10000  #  24
*vertex,40.00000,16.00000,11.90000  #  25
*vertex,40.00000,16.00000,14.70000  #  26
*vertex,25.30000,8.00000,12.90000  #  27
*vertex,39.70000,8.00000,12.90000  #  28
*vertex,39.70000,8.00000,14.05000  #  29
*vertex,25.30000,8.00000,14.05000  #  30
*vertex,25.00000,20.00000,11.90000  #  31
*vertex,25.00000,20.00000,14.70000  #  32
*vertex,29.00000,20.00000,14.70000  #  33
*vertex,29.00000,20.00000,11.90000  #  34
*vertex,34.00000,14.00000,11.90000  #  35
*vertex,34.00000,20.00000,11.90000  #  36
# 
# tag, number of vertices followed by list of associated vert
*edges,10,2,25,26,8,2,15,18,17,16,15  #  1
*edges,10,3,4,10,9,3,13,14,22,21,13  #  2
*edges,4,4,31,32,10  #  3
*edges,4,5,6,12,11  #  4
*edges,4,6,1,7,12  #  5
*edges,9,7,8,26,9,10,32,33,11,12  #  6
*edges,11,1,6,5,35,36,34,31,4,3,25,2  #  7
*edges,4,13,21,22,14  #  8
*edges,4,15,16,17,18  #  9
*edges,4,23,19,20,24  # 10
*edges,10,25,3,9,26,25,23,24,20,19,23  # 11
*edges,10,1,2,8,7,1,27,30,29,28,27  # 12
*edges,4,27,28,29,30  # 13
*edges,4,31,34,33,32  # 14
*edges,4,34,5,11,33  # 15
*edges,4,35,5,34,36  # 16
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,east,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,north_1,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,west_1,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,06,03  #   3 ||< west_1:office_w_3
*surf,north_2,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,10,03  #   4 ||< north_2:core_top
*surf,west_2,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,06,05  #   5 ||< west_2:office_w_3
*surf,top,CEIL,-,-,-,ceiling_dr,OPAQUE,ANOTHER,19,07  #   6 ||< top:roof
*surf,bottom,FLOR,-,-,-,inter_flr,OPAQUE,ANOTHER,18,04  #   7 ||< top_e:ceiling_3
*surf,window_north,VERT,north_1,-,-,double_glz,DC_8074_04nb,EXTERIOR,0,0  #   8 ||< external
*surf,window_est_1,VERT,east,-,-,double_glz,DC_8074_04nb,EXTERIOR,0,0  #   9 ||< external
*surf,window_est_2,VERT,east_b,-,-,double_glz,DC_8074_04nb,EXTERIOR,0,0  #  10 ||< external
*surf,east_b,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,south_wall,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,south_glaz,VERT,south_wall,-,-,double_glz,DC_8074_04nb,EXTERIOR,0,0  #  13 ||< external
*surf,ptn_core_a,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,10,01  #  14 ||< ptn_core_a:core_top
*surf,ptn_cor_b,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,10,02  #  15 ||< ptn_cor_b:core_top
*surf,top_e,FLOR,bottom,-,-,inter_flr,OPAQUE,ANOTHER,09,18  #  16 ||< top_e:core_three
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,all_applicable   4 # insolation sources
  8  9 10 13
# 
*base_list,2,7,16,   246.00 0  # zone base list
