*Geometry 1.1,GEN,office_e_1 # tag version, format, zone name
*date Mon Apr  2 16:33:59 2012  # latest file modification 
office_e_1 is the east office on 1st level up
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,25.00000,8.00000,4.20000  #   1
*vertex,40.00000,8.00000,4.20000  #   2
*vertex,40.00000,26.00000,4.20000  #   3
*vertex,25.00000,26.00000,4.20000  #   4
*vertex,29.00000,14.00000,4.20000  #   5
*vertex,25.00000,14.00000,4.20000  #   6
*vertex,25.00000,8.00000,7.00000  #   7
*vertex,40.00000,8.00000,7.00000  #   8
*vertex,40.00000,26.00000,7.00000  #   9
*vertex,25.00000,26.00000,7.00000  #  10
*vertex,29.00000,14.00000,7.00000  #  11
*vertex,25.00000,14.00000,7.00000  #  12
*vertex,39.90000,26.00000,5.00000  #  13
*vertex,39.90000,26.00000,6.40000  #  14
*vertex,40.00000,12.00000,5.00000  #  15
*vertex,40.00000,15.00000,5.00000  #  16
*vertex,40.00000,15.00000,6.40000  #  17
*vertex,40.00000,12.00000,6.40000  #  18
*vertex,40.00000,20.00000,5.00000  #  19
*vertex,40.00000,20.00000,6.40000  #  20
*vertex,27.00000,26.00000,5.00000  #  21
*vertex,27.00000,26.00000,6.40000  #  22
*vertex,40.00000,17.30000,5.00000  #  23
*vertex,40.00000,17.30000,6.40000  #  24
*vertex,40.00000,16.00000,4.20000  #  25
*vertex,40.00000,16.00000,7.00000  #  26
*vertex,25.30000,8.00000,5.20000  #  27
*vertex,39.70000,8.00000,5.20000  #  28
*vertex,39.70000,8.00000,6.35000  #  29
*vertex,25.30000,8.00000,6.35000  #  30
*vertex,25.00000,20.00000,4.20000  #  31
*vertex,25.00000,20.00000,7.00000  #  32
*vertex,29.00000,20.00000,7.00000  #  33
*vertex,29.00000,20.00000,4.20000  #  34
*vertex,39.82000,26.00000,5.08000  #  35
*vertex,27.08000,26.00000,5.08000  #  36
*vertex,27.08000,26.00000,6.32000  #  37
*vertex,39.82000,26.00000,6.32000  #  38
*vertex,40.00000,12.08000,5.08000  #  39
*vertex,40.00000,14.92000,5.08000  #  40
*vertex,40.00000,14.92000,6.32000  #  41
*vertex,40.00000,12.08000,6.32000  #  42
*vertex,40.00000,17.38000,5.08000  #  43
*vertex,40.00000,19.92000,5.08000  #  44
*vertex,40.00000,19.92000,6.32000  #  45
*vertex,40.00000,17.38000,6.32000  #  46
*vertex,25.38000,8.00000,5.28000  #  47
*vertex,39.62000,8.00000,5.28000  #  48
*vertex,39.62000,8.00000,6.27000  #  49
*vertex,25.38000,8.00000,6.27000  #  50
# 
# tag, number of vertices followed by list of associated vert
*edges,10,2,25,26,8,2,15,18,17,16,15  #  1
*edges,10,3,4,10,9,3,13,14,22,21,13  #  2
*edges,4,4,31,32,10  #  3
*edges,4,5,6,12,11  #  4
*edges,4,6,1,7,12  #  5
*edges,9,7,8,26,9,10,32,33,11,12  #  6
*edges,9,1,6,5,34,31,4,3,25,2  #  7
*edges,10,13,21,22,14,13,35,38,37,36,35  #  8
*edges,10,15,16,17,18,15,39,42,41,40,39  #  9
*edges,10,23,19,20,24,23,43,46,45,44,43  # 10
*edges,10,25,3,9,26,25,23,24,20,19,23  # 11
*edges,10,1,2,8,7,1,27,30,29,28,27  # 12
*edges,10,27,28,29,30,27,47,50,49,48,47  # 13
*edges,4,31,34,33,32  # 14
*edges,4,34,5,11,33  # 15
*edges,4,35,36,37,38  # 16
*edges,4,39,40,41,42  # 17
*edges,4,43,44,45,46  # 18
*edges,4,47,48,49,50  # 19
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,east,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,north_1,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,west_1,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,04,03  #   3 ||< west_1:office_w_1
*surf,north_2,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,08,03  #   4 ||< north_e1:core_one
*surf,west_2,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,04,05  #   5 ||< west_2:office_w_1
*surf,top,CEIL,-,-,-,ceiling_dr,OPAQUE,ANOTHER,17,05  #   6 ||< ovr_off_e:ceiling_1
*surf,bottom,FLOR,-,-,-,inter_flr,OPAQUE,ANOTHER,16,05  #   7 ||< top_e:ceiling_g
*surf,frm_north,VERT,north_1,-,-,gl_frame,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,frm_est_1,VERT,east,-,-,gl_frame,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,frm_est_2,VERT,east_b,-,-,gl_frame,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,east_b,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,south_wall,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,south_frm,VERT,south_wall,-,-,gl_frame,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,ptn_core_a,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,08,01  #  14 ||< ptn_core_a:core_one
*surf,ptn_cor_b,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,08,02  #  15 ||< ptn_cor_b:core_one
*surf,gl_north,VERT,frm_north,C-WINDOW,CLOSED,double_glz,DC_8074_04nb,EXTERIOR,0,0  #  16 ||< external
*surf,gl_est_1,VERT,frm_est_1,C-WINDOW,CLOSED,double_glz,DC_8074_04nb,EXTERIOR,0,0  #  17 ||< external
*surf,gl_est_2,VERT,frm_est_2,C-WINDOW,CLOSED,double_glz,DC_8074_04nb,EXTERIOR,0,0  #  18 ||< external
*surf,south_gl,VERT,south_frm,C-WINDOW,CLOSED,double_glz,DC_8074_04nb,EXTERIOR,0,0  #  19 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,all_applicable  12 # list of surfs
  1  2  8  9 10 11 12 13 16 17 18 19
# 
*insol_calc,all_applicable   4 # insolation sources
  8  9 10 13
# 
*base_list,1,7,   246.00 0  # zone base list
# 
# block entities:
#  *obs = obstructions
*block_start, 20 20 # geometric blocks
*obs,25.290,7.245,6.400,14.420,0.020,0.200,0.000,1.00,fin_ra,window_frame  # block   1
*obs,25.290,7.345,6.400,14.420,0.020,0.200,0.000,1.00,fin_ra,window_frame  # block   2
*obs,25.290,7.445,6.400,14.420,0.020,0.200,0.000,1.00,fin_rb,window_frame  # block   3
*obs,25.290,7.545,6.400,14.420,0.020,0.200,0.000,1.00,fin_rc,window_frame  # block   4
*obs,25.290,7.645,6.400,14.420,0.020,0.200,0.000,1.00,fin_rd,window_frame  # block   5
*obs,25.290,7.745,6.400,14.420,0.020,0.200,0.000,1.00,fin_re,window_frame  # block   6
*obs,25.290,7.845,6.400,14.420,0.020,0.200,0.000,1.00,fin_rf,window_frame  # block   7
*obs,25.290,7.945,6.400,14.420,0.020,0.200,0.000,1.00,fin_rg,window_frame  # block   8
*obs,40.055,8.100,6.400,17.800,0.020,0.200,90.000,1.00,wine1a,window_frame  # block   9
*obs,40.155,8.100,6.400,17.800,0.020,0.200,90.000,1.00,wine1b,window_frame  # block  10
*obs,40.255,8.100,6.400,17.800,0.020,0.200,90.000,1.00,wine1c,window_frame  # block  11
*obs,40.355,8.100,6.400,17.800,0.020,0.200,90.000,1.00,wine1d,window_frame  # block  12
*obs,40.455,8.100,6.400,17.800,0.020,0.200,90.000,1.00,wine1e,window_frame  # block  13
*obs,40.555,8.100,6.400,17.800,0.020,0.200,90.000,1.00,wine1f,window_frame  # block  14
*obs,40.655,8.100,6.400,17.800,0.020,0.200,90.000,1.00,wine1g,window_frame  # block  15
*obs,40.755,8.100,6.400,17.800,0.020,0.200,90.000,1.00,wine1g,window_frame  # block  16
*end_block
