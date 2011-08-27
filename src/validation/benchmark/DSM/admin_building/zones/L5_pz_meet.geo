*Geometry 1.1,GEN,L5_pz_meet # tag version, format, zone name
*date Sat Aug 27 09:24:29 2011  # latest file modification 
L5_pz_meet is the L5 print zone plus meeting room adj stair2
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,46.25000,36.00000,64.15000  #   1
*vertex,46.25000,32.75000,64.15000  #   2
*vertex,46.25000,30.50000,64.15000  #   3
*vertex,46.25000,28.50000,64.15000  #   4
*vertex,46.25000,23.17941,64.15000  #   5
*vertex,51.00000,23.17941,64.15000  #   6
*vertex,51.00000,28.50000,64.15000  #   7
*vertex,51.00000,30.50000,64.15000  #   8
*vertex,50.00000,30.50000,64.15000  #   9
*vertex,50.00000,36.00000,64.15000  #  10
*vertex,48.75000,36.00000,64.15000  #  11
*vertex,46.25000,36.00000,67.37500  #  12
*vertex,46.25000,32.75000,67.37500  #  13
*vertex,46.25000,30.50000,67.37500  #  14
*vertex,46.25000,28.50000,67.37500  #  15
*vertex,46.25000,23.17941,67.37500  #  16
*vertex,51.00000,23.17941,67.37500  #  17
*vertex,51.00000,28.50000,67.37500  #  18
*vertex,51.00000,30.50000,67.37500  #  19
*vertex,50.00000,30.50000,67.37500  #  20
*vertex,50.00000,36.00000,67.37500  #  21
*vertex,48.75000,36.00000,67.37500  #  22
*vertex,49.93402,36.00000,64.32024  #  23
*vertex,48.81598,36.00000,64.32024  #  24
*vertex,48.81598,36.00000,67.20477  #  25
*vertex,49.93402,36.00000,67.20477  #  26
*vertex,48.61803,36.00000,64.32024  #  27
*vertex,46.38197,36.00000,64.32024  #  28
*vertex,46.38197,36.00000,67.20477  #  29
*vertex,48.61803,36.00000,67.20477  #  30
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,13,12  #  1
*edges,4,2,3,14,13  #  2
*edges,4,3,4,15,14  #  3
*edges,4,4,5,16,15  #  4
*edges,4,5,6,17,16  #  5
*edges,4,6,7,18,17  #  6
*edges,4,7,8,19,18  #  7
*edges,4,8,9,20,19  #  8
*edges,4,9,10,21,20  #  9
*edges,10,10,11,22,21,10,23,26,25,24,23  # 10
*edges,10,11,1,12,22,11,27,30,29,28,27  # 11
*edges,11,12,13,14,15,16,17,18,19,20,21,22  # 12
*edges,11,1,11,10,9,8,7,6,5,4,3,2  # 13
*edges,4,23,24,25,26  # 14
*edges,4,27,28,29,30  # 15
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_cell,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,70,10  #   1 ||< ptn_pz:L5_srv_cell
*surf,ptn_op_a,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,80,08  #   2 ||< ptn_op_a:L5_open_rt
*surf,ptn_op_b,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,80,09  #   3 ||< ptn_op_b:L5_open_rt
*surf,ptn_op_c,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,80,10  #   4 ||< ptn_op_c:L5_open_rt
*surf,ptn_op_d,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,80,11  #   5 ||< ptn_op_d:L5_open_rt
*surf,prn_toil,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,74,08  #   6 ||< ptna_opn_l5:L5_toil_psg
*surf,ptn_psg,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,74,07  #   7 ||< ptn_cel_b:L5_toil_psg
*surf,ptn_staira,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,02,44  #   8 ||< ptnc_str_l5:stair_2
*surf,ptn_stairb,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,02,48  #   9 ||< ptnd_str_l5:stair_2
*surf,fac_fr_a,VERT,-,-,-,mull_90,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,fac_fr_b,VERT,-,-,-,mull_90,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,roof,CEIL,-,-,-,roof_0.2,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,floor_pz_m,FLOR,-,-,-,top_pln,OPAQUE,ANOTHER,81,17  #  13 ||< floor_pz_m:L5_uf_plen
*surf,fac_gl_a,VERT,fac_fr_a,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  14 ||< external
*surf,fac_gl_b,VERT,fac_fr_b,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  15 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,13,    55.40 0  # zone base list
