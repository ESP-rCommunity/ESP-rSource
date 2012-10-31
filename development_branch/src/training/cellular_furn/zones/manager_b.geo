*Geometry 1.1,GEN,manager_b # tag version, format, zone name
*date Fri Jul 10 10:17:48 2009  # latest file modification 
manager_b describes right higher resolution office
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,6.10000,0.00000,0.00000  #   1
*vertex,3.10000,0.00000,0.00000  #   2
*vertex,3.10000,4.50000,0.00000  #   3
*vertex,4.90000,4.50000,0.00000  #   4
*vertex,7.10000,4.50000,0.00000  #   5
*vertex,6.10000,0.00000,3.00000  #   6
*vertex,3.10000,0.00000,3.00000  #   7
*vertex,3.10000,4.50000,3.00000  #   8
*vertex,4.90000,4.50000,3.00000  #   9
*vertex,7.10000,4.50000,3.00000  #  10
*vertex,6.10000,0.00000,0.90000  #  11
*vertex,3.10000,0.00000,0.90000  #  12
*vertex,5.10000,0.00000,0.90000  #  13
*vertex,4.10000,0.00000,0.90000  #  14
*vertex,6.00000,0.00000,1.00000  #  15
*vertex,3.20000,0.00000,1.00000  #  16
*vertex,3.20000,0.00000,2.90000  #  17
*vertex,6.00000,0.00000,2.90000  #  18
*vertex,3.20000,4.50000,0.10000  #  19
*vertex,4.80000,4.50000,0.10000  #  20
*vertex,4.80000,4.50000,2.90000  #  21
*vertex,3.20000,4.50000,2.90000  #  22
*vertex,5.70000,4.50000,0.00000  #  23
*vertex,5.70000,4.50000,3.00000  #  24
*vertex,7.10000,2.50000,0.00000  #  25
*vertex,6.10000,2.35000,0.00000  #  26
*vertex,7.10000,2.50000,3.00000  #  27
*vertex,6.10000,2.35000,3.00000  #  28
*vertex,5.70000,4.50000,2.90000  #  29
*vertex,4.90000,4.50000,2.90000  #  30
*vertex,6.10000,1.00000,0.00000  #  31
*vertex,6.10000,1.00000,3.00000  #  32
*vertex,3.10000,1.00000,0.00000  #  33
*vertex,3.10000,2.35000,0.00000  #  34
*vertex,3.10000,1.00000,3.00000  #  35
*vertex,3.10000,2.35000,3.00000  #  36
*vertex,3.20000,0.10000,0.90000  #  37
*vertex,6.00000,0.10000,0.90000  #  38
*vertex,6.00000,1.10000,0.90000  #  39
*vertex,3.20000,1.10000,0.90000  #  40
*vertex,6.10000,2.70000,0.10000  #  41
*vertex,7.00000,2.70000,0.10000  #  42
*vertex,7.00000,2.70000,2.10000  #  43
*vertex,6.10000,2.70000,2.10000  #  44
*vertex,6.10000,3.70000,0.10000  #  45
*vertex,7.00000,3.70000,0.10000  #  46
*vertex,7.00000,3.70000,2.10000  #  47
*vertex,6.10000,3.70000,2.10000  #  48
# 
# tag, number of vertices followed by list of associated vert
*edges,5,33,2,12,7,35  #  1
*edges,11,4,3,19,20,21,22,19,3,8,9,30  #  2
*edges,4,23,4,30,29  #  3
*edges,4,25,5,10,27  #  4
*edges,4,7,6,32,35  #  5
*edges,4,31,1,2,33  #  6
*edges,6,2,1,11,13,14,12  #  7
*edges,12,13,11,15,16,17,18,15,11,6,7,12,14  #  8
*edges,4,16,15,18,17  #  9
*edges,4,20,19,22,21  # 10
*edges,7,5,23,29,30,9,24,10  # 11
*edges,4,26,25,27,28  # 12
*edges,4,31,26,28,32  # 13
*edges,4,34,33,35,36  # 14
*edges,4,3,34,36,8  # 15
*edges,5,1,31,32,6,11  # 16
*edges,4,31,33,34,26  # 17
*edges,7,26,34,3,4,23,5,25  # 18
*edges,4,35,32,28,36  # 19
*edges,7,36,28,27,10,24,9,8  # 20
*edges,4,37,38,39,40  # 21
*edges,4,38,37,40,39  # 22
*edges,4,41,42,43,44  # 23
*edges,4,42,41,44,43  # 24
*edges,4,45,46,47,48  # 25
*edges,4,46,45,48,47  # 26
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,pt_general,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,ANOTHER,01,01  #   1 ||< pt_general:manager_a
*surf,part_frame,VERT,-,-,-,insul_frame,OPAQUE,ANOTHER,02,10  #   2 ||< part_frameb:coridor
*surf,door,VERT,-,-,-,door,OPAQUE,ANOTHER,02,11  #   3 ||< door_b:coridor
*surf,pt_other,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,ceiling,CEIL,-,-,-,ceiling,OPAQUE,SIMILAR,00,00  #   5 ||< identical environment
*surf,floor,FLOR,-,-,-,susp_flr_re,OPAQUE,SIMILAR,00,00  #   6 ||< identical environment
*surf,spandral,VERT,-,-,-,insul_frame,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,frame,VERT,-,-,-,insul_frame,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,glazing,VERT,-,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #   9 ||< external
*surf,part_glaz,VERT,-,-,-,dbl_glz,DCF7671_06nb,ANOTHER,02,13  #  10 ||< part_glazb:coridor
*surf,ptn_corid,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,ANOTHER,02,12  #  11 ||< ptn_coridb:coridor
*surf,pt_other_a,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,SIMILAR,00,00  #  12 ||< identical environment
*surf,pt_other_b,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,SIMILAR,00,00  #  13 ||< identical environment
*surf,pt_gen_a,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,ANOTHER,01,14  #  14 ||< pt_gen_a:manager_a
*surf,pt_gen_b,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,ANOTHER,01,15  #  15 ||< pt_gen_b:manager_a
*surf,pt_other_c,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,SIMILAR,00,00  #  16 ||< identical environment
*surf,floor_b,FLOR,-,-,-,susp_flr_re,OPAQUE,SIMILAR,00,00  #  17 ||< identical environment
*surf,floor_c,FLOR,-,-,-,susp_flr_re,OPAQUE,SIMILAR,00,00  #  18 ||< identical environment
*surf,ceiling_b,CEIL,-,-,-,ceiling,OPAQUE,SIMILAR,00,00  #  19 ||< identical environment
*surf,ceiling_c,CEIL,-,-,-,ceiling,OPAQUE,SIMILAR,00,00  #  20 ||< identical environment
*surf,desk,CEIL,-,-,-,door,OPAQUE,ADIABATIC,0,0  #  21 ||< adiabatic
*surf,desk_a,FLOR,-,-,-,door,OPAQUE,ADIABATIC,0,0  #  22 ||< adiabatic
*surf,cabinet_a,VERT,-,-,-,door,OPAQUE,ADIABATIC,0,0  #  23 ||< adiabatic
*surf,cabinet_aa,VERT,-,-,-,door,OPAQUE,ADIABATIC,0,0  #  24 ||< adiabatic
*surf,cabinet_b,VERT,-,-,-,door,OPAQUE,ADIABATIC,0,0  #  25 ||< adiabatic
*surf,cabinet_bb,VERT,-,-,-,door,OPAQUE,ADIABATIC,0,0  #  26 ||< adiabatic
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,all_applicable   3 # list of surfs
  7  8  9
# 
*insol_calc,all_applicable   1 # insolation sources
  9
# 
*base_list,4,6,17,18,22,    18.37 0  # zone base list
# 
# block entities:
#  *obs = obstructions
*block_start,15 15 # geometric blocks
*obs,6.100,2.700,0.010,0.900,0.700,2.000,0.000,cab_a,insul_frame  # block  1
*obs,6.100,3.700,0.010,0.900,0.700,2.000,0.000,cab_b,insul_frame  # block  2
*end_block
