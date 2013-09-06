*Geometry 1.1,GEN,office_2_sth # tag version, format, zone name
*date Mon Apr  2 16:47:52 2012  # latest file modification 
office_2_sth is the south perimeter cellular offices
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,25.00000,8.00000,8.05000  #   1
*vertex,10.00000,8.00000,8.05000  #   2
*vertex,25.00000,8.00000,10.85000  #   3
*vertex,10.00000,8.00000,10.85000  #   4
*vertex,24.70000,8.00000,9.05000  #   5
*vertex,10.30000,8.00000,9.05000  #   6
*vertex,10.30000,8.00000,10.20000  #   7
*vertex,24.70000,8.00000,10.20000  #   8
*vertex,25.00000,13.00000,8.05000  #   9
*vertex,25.00000,13.00000,10.85000  #  10
*vertex,15.00000,13.00000,8.05000  #  11
*vertex,15.00000,13.00000,10.85000  #  12
*vertex,16.00000,8.10000,8.15000  #  13
*vertex,16.00000,12.90000,8.15000  #  14
*vertex,16.00000,12.90000,10.75000  #  15
*vertex,16.00000,8.10000,10.75000  #  16
*vertex,20.50000,8.10000,8.15000  #  17
*vertex,20.50000,12.90000,8.15000  #  18
*vertex,20.50000,12.90000,10.75000  #  19
*vertex,20.50000,8.10000,10.75000  #  20
*vertex,40.00000,8.00000,8.05000  #  21
*vertex,40.00000,8.00000,10.85000  #  22
*vertex,25.30000,8.00000,9.05000  #  23
*vertex,25.30000,8.00000,10.20000  #  24
*vertex,39.70000,8.00000,10.20000  #  25
*vertex,39.70000,8.00000,9.05000  #  26
*vertex,34.00000,8.10000,8.15000  #  27
*vertex,34.00000,12.90000,8.15000  #  28
*vertex,34.00000,12.90000,10.75000  #  29
*vertex,34.00000,8.10000,10.75000  #  30
*vertex,29.50000,8.10000,8.15000  #  31
*vertex,29.50000,12.90000,8.15000  #  32
*vertex,29.50000,12.90000,10.75000  #  33
*vertex,29.50000,8.10000,10.75000  #  34
*vertex,35.00000,13.00000,8.05000  #  35
*vertex,35.00000,13.00000,10.85000  #  36
*vertex,10.38000,8.00000,9.13000  #  37
*vertex,24.62000,8.00000,9.13000  #  38
*vertex,24.62000,8.00000,10.12000  #  39
*vertex,10.38000,8.00000,10.12000  #  40
*vertex,25.38000,8.00000,9.13000  #  41
*vertex,39.62000,8.00000,9.13000  #  42
*vertex,39.62000,8.00000,10.12000  #  43
*vertex,25.38000,8.00000,10.12000  #  44
# 
# tag, number of vertices followed by list of associated vert
*edges,4,4,3,10,12  #  1
*edges,4,9,1,2,11  #  2
*edges,10,2,1,3,4,2,6,7,8,5,6  #  3
*edges,10,6,5,8,7,6,37,40,39,38,37  #  4
*edges,4,9,11,12,10  #  5
*edges,4,11,2,4,12  #  6
*edges,4,14,13,16,15  #  7
*edges,4,13,14,15,16  #  8
*edges,4,18,17,20,19  #  9
*edges,4,17,18,19,20  # 10
*edges,10,1,21,22,3,1,23,24,25,26,23  # 11
*edges,10,23,26,25,24,23,41,44,43,42,41  # 12
*edges,4,27,28,29,30  # 13
*edges,4,28,27,30,29  # 14
*edges,4,31,32,33,34  # 15
*edges,4,32,31,34,33  # 16
*edges,4,35,9,10,36  # 17
*edges,4,21,35,36,22  # 18
*edges,4,3,22,36,10  # 19
*edges,4,21,1,9,35  # 20
*edges,4,37,38,39,40  # 21
*edges,4,41,42,43,44  # 22
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,top,CEIL,-,-,-,ceiling_dr,OPAQUE,ANOTHER,18,18  #   1 ||< over_sw:ceiling_2
*surf,bottom,FLOR,-,-,-,inter_flr,OPAQUE,ANOTHER,17,17  #   2 ||< under_off_sw:ceiling_1
*surf,south_wall,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,south_swfrm,VERT,south_wall,-,-,gl_frame,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,corid_a,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,15,09  #   5 ||< corid_aw:passage_2
*surf,sw_ptn,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,14,07  #   6 ||< sw_ptn:office_w2w
*surf,wf_ptn_a,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,12,08  #   7 ||< wf_ptn_aa:office_2_sth
*surf,wf_ptn_aa,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,12,07  #   8 ||< wf_ptn_a:office_2_sth
*surf,wf_ptn_b,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,12,10  #   9 ||< wf_ptn_bb:office_2_sth
*surf,wf_ptn_bb,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,12,09  #  10 ||< wf_ptn_b:office_2_sth
*surf,south_walle,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,south_sefrm,VERT,south_walle,-,-,gl_frame,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,ff_ptn_a,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,12,14  #  13 ||< ff_ptn_aa:office_2_sth
*surf,ff_ptn_aa,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,12,13  #  14 ||< ff_ptn_a:office_2_sth
*surf,ff_ptn_b,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,12,16  #  15 ||< ff_ptn_bb:office_2_sth
*surf,ff_ptn_bb,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,12,15  #  16 ||< ff_ptn_b:office_2_sth
*surf,corid_ae,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,15,06  #  17 ||< corid_a:passage_2
*surf,se_ptn,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,13,07  #  18 ||< se_ptn:office_e2e
*surf,top_e,CEIL,-,-,-,ceiling_dr,OPAQUE,ANOTHER,18,17  #  19 ||< over_se:ceiling_2
*surf,base_east,FLOR,-,-,-,inter_flr,OPAQUE,ANOTHER,17,18  #  20 ||< under_off_se:ceiling_1
*surf,south_swgl,VERT,south_swfrm,C-WINDOW,CLOSED,double_glz,DC_8074_04nb,EXTERIOR,0,0  #  21 ||< external
*surf,south_segl,VERT,south_sefrm,C-WINDOW,CLOSED,double_glz,DC_8074_04nb,EXTERIOR,0,0  #  22 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,all_applicable   6 # list of surfs
  3  4 11 12 21 22
# 
*insol_calc,all_applicable   2 # insolation sources
  4 12
# 
*base_list,0,125.00,1  # zone base
# 
# block entities:
#  *obs = obstructions
*block_start, 20 20 # geometric blocks
*obs,10.290,7.245,10.250,29.420,0.020,0.200,0.000,1.00,fin_ra,window_frame  # block   1
*obs,10.290,7.345,10.250,29.420,0.020,0.200,0.000,1.00,fin_rb,window_frame  # block   2
*obs,10.290,7.445,10.250,29.420,0.020,0.200,0.000,1.00,fin_rc,window_frame  # block   3
*obs,10.290,7.545,10.250,29.420,0.020,0.200,0.000,1.00,fin_rd,window_frame  # block   4
*obs,10.290,7.645,10.250,29.420,0.020,0.200,0.000,1.00,fin_re,window_frame  # block   5
*obs,10.290,7.745,10.250,29.420,0.020,0.200,0.000,1.00,fin_rf,window_frame  # block   6
*obs,10.290,7.845,10.250,29.420,0.020,0.200,0.000,1.00,fin_rg,window_frame  # block   7
*obs,10.290,7.945,10.250,29.420,0.020,0.200,0.000,1.00,fin_rh,window_frame  # block   8
*end_block
