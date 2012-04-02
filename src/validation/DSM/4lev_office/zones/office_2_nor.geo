*Geometry 1.1,GEN,office_2_nor # tag version, format, zone name
*date Mon Apr  2 13:34:30 2012  # latest file modification 
office_2_nor is on the north side of 2nd level passage
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,10.00000,26.00000,8.05000  #   1
*vertex,25.00000,26.00000,8.05000  #   2
*vertex,10.00000,26.00000,10.85000  #   3
*vertex,25.00000,26.00000,10.85000  #   4
*vertex,10.10000,26.00000,8.85000  #   5
*vertex,10.10000,26.00000,10.25000  #   6
*vertex,23.00000,26.00000,8.85000  #   7
*vertex,23.00000,26.00000,10.25000  #   8
*vertex,15.00000,21.00000,8.05000  #   9
*vertex,15.00000,21.00000,10.85000  #  10
*vertex,25.00000,21.00000,10.85000  #  11
*vertex,25.00000,21.00000,8.05000  #  12
*vertex,16.00000,21.10000,8.15000  #  13
*vertex,16.00000,25.90000,8.15000  #  14
*vertex,16.00000,25.90000,10.75000  #  15
*vertex,16.00000,21.10000,10.75000  #  16
*vertex,20.50000,21.10000,8.15000  #  17
*vertex,20.50000,25.90000,8.15000  #  18
*vertex,20.50000,25.90000,10.75000  #  19
*vertex,20.50000,21.10000,10.75000  #  20
*vertex,39.90000,26.00000,8.85000  #  21
*vertex,27.00000,26.00000,8.85000  #  22
*vertex,27.00000,26.00000,10.25000  #  23
*vertex,39.90000,26.00000,10.25000  #  24
*vertex,40.00000,26.00000,8.05000  #  25
*vertex,40.00000,26.00000,10.85000  #  26
*vertex,35.00000,21.00000,8.05000  #  27
*vertex,35.00000,21.00000,10.85000  #  28
*vertex,34.00000,21.10000,8.15000  #  29
*vertex,34.00000,25.90000,8.15000  #  30
*vertex,34.00000,25.90000,10.75000  #  31
*vertex,34.00000,21.10000,10.75000  #  32
*vertex,29.50000,21.10000,8.15000  #  33
*vertex,29.50000,25.90000,8.15000  #  34
*vertex,29.50000,25.90000,10.75000  #  35
*vertex,29.50000,21.10000,10.75000  #  36
*vertex,22.92000,26.00000,8.93000  #  37
*vertex,10.18000,26.00000,8.93000  #  38
*vertex,10.18000,26.00000,10.17000  #  39
*vertex,22.92000,26.00000,10.17000  #  40
*vertex,39.82000,26.00000,8.93000  #  41
*vertex,27.08000,26.00000,8.93000  #  42
*vertex,27.08000,26.00000,10.17000  #  43
*vertex,39.82000,26.00000,10.17000  #  44
# 
# tag, number of vertices followed by list of associated vert
*edges,10,2,1,3,4,2,7,8,6,5,7  #  1
*edges,6,10,11,28,26,4,3  #  2
*edges,6,1,2,25,27,12,9  #  3
*edges,10,7,5,6,8,7,37,40,39,38,37  #  4
*edges,4,1,9,10,3  #  5
*edges,4,9,12,11,10  #  6
*edges,4,14,13,16,15  #  7
*edges,4,13,14,15,16  #  8
*edges,4,18,17,20,19  #  9
*edges,4,17,18,19,20  # 10
*edges,10,21,22,23,24,21,41,44,43,42,41  # 11
*edges,10,25,2,4,26,25,21,24,23,22,21  # 12
*edges,4,12,27,28,11  # 13
*edges,4,29,30,31,32  # 14
*edges,4,30,29,32,31  # 15
*edges,4,33,34,35,36  # 16
*edges,4,34,33,36,35  # 17
*edges,4,27,25,26,28  # 18
*edges,4,37,38,39,40  # 19
*edges,4,41,42,43,44  # 20
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,north_1,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,top,CEIL,-,-,-,ceiling_dr,OPAQUE,ANOTHER,18,19  #   2 ||< over_north:ceiling_2
*surf,bottom,FLOR,-,-,-,inter_flr,OPAQUE,ANOTHER,17,19  #   3 ||< under_north:ceiling_1
*surf,frm_north,VERT,north_1,-,-,gl_frame,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,nw_ptn,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,14,08  #   5 ||< nw_ptn:office_w2w
*surf,corid_c,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,15,12  #   6 ||< corid_cw:passage_2
*surf,wf_ptn_c,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,11,08  #   7 ||< wf_ptn_cc:office_2_nor
*surf,wf_ptn_cc,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,11,07  #   8 ||< wf_ptn_c:office_2_nor
*surf,wf_ptn_d,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,11,10  #   9 ||< wf_ptn_dd:office_2_nor
*surf,wf_ptn_dd,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,11,09  #  10 ||< wf_ptn_d:office_2_nor
*surf,frm_neh,VERT,north_1e,-,-,gl_frame,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,north_1e,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,corid_ce,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,15,07  #  13 ||< corid_c:passage_2
*surf,ff_ptn_c,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,11,15  #  14 ||< ff_ptn_cc:office_2_nor
*surf,ff_ptn_cc,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,11,14  #  15 ||< ff_ptn_c:office_2_nor
*surf,ff_ptn_d,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,11,17  #  16 ||< ff_ptn_dd:office_2_nor
*surf,ff_ptn_dd,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,11,16  #  17 ||< ff_ptn_d:office_2_nor
*surf,ne_ptn,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,13,08  #  18 ||< ne_ptn:office_e2e
*surf,gl_north,VERT,frm_north,C-WINDOW,CLOSED,double_glz,TRAN,EXTERIOR,0,0  #  19 ||< external
*surf,gl_neh,VERT,frm_neh,C-WINDOW,CLOSED,double_glz,TRAN,EXTERIOR,0,0  #  20 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,3,   125.00 0  # zone base list
