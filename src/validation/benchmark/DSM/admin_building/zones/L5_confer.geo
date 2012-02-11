*Geometry 1.1,GEN,L5_confer # tag version, format, zone name
*date Sat Feb 11 18:22:12 2012  # latest file modification 
L5_confer describes conference room on level five
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,7.50000,64.15000  #   1
*vertex,1.50000,7.75000,64.15000  #   2
*vertex,4.00000,8.25000,64.15000  #   3
*vertex,6.75000,9.00000,64.15000  #   4
*vertex,8.25000,9.25000,64.15000  #   5
*vertex,10.75000,10.00000,64.15000  #   6
*vertex,13.70000,10.50000,64.15000  #   7
*vertex,13.70000,17.75000,64.15000  #   8
*vertex,10.00000,17.75000,64.15000  #   9
*vertex,6.75000,17.75000,64.15000  #  10
*vertex,3.75000,17.75000,64.15000  #  11
*vertex,0.00000,17.75000,64.15000  #  12
*vertex,0.00000,16.00000,64.15000  #  13
*vertex,0.00000,13.50000,64.15000  #  14
*vertex,0.00000,11.00000,64.15000  #  15
*vertex,0.00000,8.50000,64.15000  #  16
*vertex,0.00000,7.50000,67.37500  #  17
*vertex,1.50000,7.75000,67.37500  #  18
*vertex,4.00000,8.25000,67.37500  #  19
*vertex,6.75000,9.00000,67.37500  #  20
*vertex,8.25000,9.25000,67.37500  #  21
*vertex,10.75000,10.00000,67.37500  #  22
*vertex,13.70000,10.50000,67.37500  #  23
*vertex,13.70000,17.75000,67.37500  #  24
*vertex,10.00000,17.75000,67.37500  #  25
*vertex,6.75000,17.75000,67.37500  #  26
*vertex,3.75000,17.75000,67.37500  #  27
*vertex,0.00000,17.75000,67.37500  #  28
*vertex,0.00000,16.00000,67.37500  #  29
*vertex,0.00000,13.50000,67.37500  #  30
*vertex,0.00000,11.00000,67.37500  #  31
*vertex,0.00000,8.50000,67.37500  #  32
*vertex,0.49320,7.58220,64.15000  #  33
*vertex,0.49320,7.58220,67.37500  #  34
*vertex,0.54635,7.59106,64.32024  #  35
*vertex,1.44685,7.74114,64.32024  #  36
*vertex,1.44685,7.74114,67.20477  #  37
*vertex,0.54635,7.59106,67.20477  #  38
*vertex,1.63197,7.77639,64.32024  #  39
*vertex,3.86803,8.22361,64.32024  #  40
*vertex,3.86803,8.22361,67.20477  #  41
*vertex,1.63197,7.77639,67.20477  #  42
*vertex,4.14516,8.28959,64.32024  #  43
*vertex,6.60484,8.96041,64.32024  #  44
*vertex,6.60484,8.96041,67.20477  #  45
*vertex,4.14516,8.28959,67.20477  #  46
*vertex,6.82918,9.01320,64.32024  #  47
*vertex,8.17082,9.23680,64.32024  #  48
*vertex,8.17082,9.23680,67.20477  #  49
*vertex,6.82918,9.01320,67.20477  #  50
*vertex,8.38197,9.28959,64.32024  #  51
*vertex,10.61803,9.96041,64.32024  #  52
*vertex,10.61803,9.96041,67.20477  #  53
*vertex,8.38197,9.28959,67.20477  #  54
*vertex,10.90572,10.02639,64.32024  #  55
*vertex,13.54428,10.47361,64.32024  #  56
*vertex,13.54428,10.47361,67.20477  #  57
*vertex,10.90572,10.02639,67.20477  #  58
# 
# tag, number of vertices followed by list of associated vert
*edges,10,33,2,18,34,33,35,38,37,36,35  #  1
*edges,10,2,3,19,18,2,39,42,41,40,39  #  2
*edges,10,3,4,20,19,3,43,46,45,44,43  #  3
*edges,10,4,5,21,20,4,47,50,49,48,47  #  4
*edges,10,5,6,22,21,5,51,54,53,52,51  #  5
*edges,10,6,7,23,22,6,55,58,57,56,55  #  6
*edges,4,7,8,24,23  #  7
*edges,4,8,9,25,24  #  8
*edges,4,9,10,26,25  #  9
*edges,4,10,11,27,26  # 10
*edges,4,11,12,28,27  # 11
*edges,4,12,13,29,28  # 12
*edges,4,13,14,30,29  # 13
*edges,4,14,15,31,30  # 14
*edges,4,15,16,32,31  # 15
*edges,4,16,1,17,32  # 16
*edges,17,17,34,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32  # 17
*edges,17,1,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,33  # 18
*edges,4,1,33,34,17  # 19
*edges,4,35,36,37,38  # 20
*edges,4,39,40,41,42  # 21
*edges,4,43,44,45,46  # 22
*edges,4,47,48,49,50  # 23
*edges,4,51,52,53,54  # 24
*edges,4,55,56,57,58  # 25
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,fac_gl_a,VERT,-,C-WINDOW,CLOSED,mull_90,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,fac_frb,VERT,-,WALL,-,mull_90,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,fac_frc,VERT,-,WALL,-,mull_90,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,fac_frd,VERT,-,WALL,-,mull_90,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,fac_fre,VERT,-,WALL,-,mull_90,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,fac_frf,VERT,-,WALL,-,mull_90,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,ptn_open,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,69,22  #   7 ||< ptn_conf:L5_open_lft
*surf,ptn_lobby,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,67,01  #   8 ||< ptn_conf:L5_lobby
*surf,ptn_kita,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,66,01  #   9 ||< pnt_cnf:L5_kitch2
*surf,ptn_kitb,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,66,12  #  10 ||< ptn_kitb:L5_kitch2
*surf,ptn_kitc,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,66,13  #  11 ||< ptn_kitc:L5_kitch2
*surf,ptn_opn_a,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,77,09  #  12 ||< gptn_e:L5_sw_void
*surf,ptn_opn_b,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,77,08  #  13 ||< gptn_d:L5_sw_void
*surf,ptn_opn_c,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,77,07  #  14 ||< gptn_c:L5_sw_void
*surf,ptn_opn_d,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,77,06  #  15 ||< gptn_b:L5_sw_void
*surf,ptn_opn_fr,VERT,-,-,-,mull_90,OPAQUE,ANOTHER,77,05  #  16 ||< ptn_fr:L5_sw_void
*surf,roof,CEIL,-,ROOF,FLAT,roof_0.2,OPAQUE,EXTERIOR,0,0  #  17 ||< external
*surf,floor_conf,FLOR,-,-,-,top_pln,OPAQUE,ANOTHER,81,11  #  18 ||< floor_conf:L5_uf_plen
*surf,fac_fr,VERT,-,WALL,-,mull_90,OPAQUE,EXTERIOR,0,0  #  19 ||< external
*surf,glaz_a,VERT,fac_gl_a,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  20 ||< external
*surf,fac_glb,VERT,fac_frb,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  21 ||< external
*surf,fac_glc,VERT,fac_frc,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  22 ||< external
*surf,fac_gld,VERT,fac_frd,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  23 ||< external
*surf,fac_gle,VERT,fac_fre,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  24 ||< external
*surf,fac_glf,VERT,fac_frf,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  25 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,all_applicable  14 # list of surfs
  1  2  3  4  5  6 17 19 20 21 22 23 24 25
# 
*insol_calc,all_applicable   6 # insolation sources
 20 21 22 23 24 25
# 
*base_list,1,18,   120.03 0  # zone base list
# 
# block entities:
#  *obs = obstructions
*block_start, 20 20 # geometric blocks
*obs,0.000,5.800,67.375,20.000,1.600,0.300,12.500,1.00,overhang,stone_eldon  # block   1
*end_block
