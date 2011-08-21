*Geometry 1.1,GEN,L4_open_s # tag version, format, zone name
*date Sun Aug 21 10:58:37 2011  # latest file modification 
L4_open_s describes left half of level four open plan area
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,4.49566,8.24205,60.25000  #   1
*vertex,7.63326,9.08499,60.25000  #   2
*vertex,7.63326,7.49277,60.25000  #   3
*vertex,12.92503,8.80401,60.25000  #   4
*vertex,13.06552,10.53671,60.25000  #   5
*vertex,17.84216,11.75429,60.25000  #   6
*vertex,17.88899,9.97475,60.25000  #   7
*vertex,23.22759,11.19233,60.25000  #   8
*vertex,23.32125,12.97186,60.25000  #   9
*vertex,27.50000,14.00000,60.25000  #  10
*vertex,27.50000,21.25000,60.25000  #  11
*vertex,27.50000,35.75000,60.25000  #  12
*vertex,12.00000,35.75000,60.25000  #  13
*vertex,11.98844,30.48622,60.25000  #  14
*vertex,11.98844,24.21102,60.25000  #  15
*vertex,4.72981,23.97687,60.25000  #  16
*vertex,4.49566,20.97976,60.25000  #  17
*vertex,4.49566,13.95529,60.25000  #  18
*vertex,4.49566,8.24205,63.39300  #  19
*vertex,7.63326,9.08499,63.39300  #  20
*vertex,7.63326,7.49277,63.39300  #  21
*vertex,12.92503,8.80401,63.39300  #  22
*vertex,13.06552,10.53671,63.39300  #  23
*vertex,17.84216,11.75429,63.39300  #  24
*vertex,17.88899,9.97475,63.39300  #  25
*vertex,23.22759,11.19233,63.39300  #  26
*vertex,23.32125,12.97186,63.39300  #  27
*vertex,27.50000,14.00000,63.39300  #  28
*vertex,27.50000,21.25000,63.39300  #  29
*vertex,27.50000,35.75000,63.39300  #  30
*vertex,12.00000,35.75000,63.39300  #  31
*vertex,11.98844,30.48622,63.39300  #  32
*vertex,11.98844,24.21102,63.39300  #  33
*vertex,4.72981,23.97687,63.39300  #  34
*vertex,4.49566,20.97976,63.39300  #  35
*vertex,4.49566,13.95529,63.39300  #  36
*vertex,4.49566,8.24205,61.25000  #  37
*vertex,7.63326,9.08499,61.25000  #  38
*vertex,13.06552,10.53671,61.25000  #  39
*vertex,17.84216,11.75429,61.25000  #  40
*vertex,23.32125,12.97186,61.25000  #  41
*vertex,27.50000,14.00000,61.25000  #  42
*vertex,17.45000,21.30000,61.25000  #  43
*vertex,17.45000,21.30000,63.39300  #  44
*vertex,13.70000,21.30000,63.39300  #  45
*vertex,13.70000,21.30000,61.25000  #  46
*vertex,17.45000,28.50000,61.25000  #  47
*vertex,17.45000,28.50000,63.39300  #  48
*vertex,17.45000,28.50000,60.25000  #  49
*vertex,13.70000,28.50000,60.25000  #  50
*vertex,13.70000,28.50000,61.25000  #  51
*vertex,13.70000,28.50000,63.39300  #  52
*vertex,13.70000,21.30000,60.25000  #  53
*vertex,17.45000,21.30000,60.25000  #  54
*vertex,17.45000,35.75000,63.39300  #  55
*vertex,17.45000,35.75000,60.25000  #  56
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,38,37  #  1
*edges,5,2,3,21,20,38  #  2
*edges,4,3,4,22,21  #  3
*edges,5,4,5,39,23,22  #  4
*edges,4,5,6,40,39  #  5
*edges,5,6,7,25,24,40  #  6
*edges,4,7,8,26,25  #  7
*edges,5,8,9,41,27,26  #  8
*edges,4,9,10,42,41  #  9
*edges,5,10,11,29,28,42  # 10
*edges,4,11,12,30,29  # 11
*edges,6,12,56,13,31,55,30  # 12
*edges,4,13,14,32,31  # 13
*edges,4,14,15,33,32  # 14
*edges,4,15,16,34,33  # 15
*edges,4,16,17,35,34  # 16
*edges,4,17,18,36,35  # 17
*edges,5,18,1,37,19,36  # 18
*edges,25,19,20,21,22,23,24,25,26,27,28,29,30,55,48,44,45,52,48,55,31,32,33,34,35,36  # 19
*edges,25,1,18,17,16,15,14,13,56,49,50,53,54,49,56,12,11,10,9,8,7,6,5,4,3,2  # 20
*edges,4,37,38,20,19  # 21
*edges,4,39,40,24,23  # 22
*edges,4,41,42,28,27  # 23
*edges,4,44,43,46,45  # 24
*edges,4,48,47,43,44  # 25
*edges,6,50,49,47,48,52,51  # 26
*edges,4,45,46,51,52  # 27
*edges,4,53,50,51,46  # 28
*edges,4,54,53,46,43  # 29
*edges,4,49,54,43,47  # 30
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,balc_a,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,77,07  #   1 ||< balc_a:L5_atroofltl
*surf,wall_a,VERT,-,-,-,stone_atr_pt,OPAQUE,ANOTHER,77,08  #   2 ||< wall_a:L5_atroofltl
*surf,wall_b,VERT,-,-,-,stone_atr_pt,OPAQUE,UNKNOWN,0,0  #   3 ||< not yet defined
*surf,wall_c,VERT,-,-,-,stone_atr_pt,OPAQUE,UNKNOWN,0,0  #   4 ||< not yet defined
*surf,balc_b,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #   5 ||< not yet defined
*surf,wall_f,VERT,-,-,-,stone_atr_pt,OPAQUE,UNKNOWN,0,0  #   6 ||< not yet defined
*surf,wall_g,VERT,-,-,-,stone_atr_pt,OPAQUE,UNKNOWN,0,0  #   7 ||< not yet defined
*surf,wall_h,VERT,-,-,-,stone_atr_pt,OPAQUE,UNKNOWN,0,0  #   8 ||< not yet defined
*surf,balc_c,VERT,-,-,-,glaz_ptn,SCF8477_10nb,UNKNOWN,0,0  #   9 ||< not yet defined
*surf,fic_to_ra,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,58,07  #  10 ||< open_fict2:L4_open_plr
*surf,fic_to_rb,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,58,06  #  11 ||< open_fict:L4_open_plr
*surf,ptn_other,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,0,0  #  12 ||< not yet defined
*surf,ptn_stair,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,01,44  #  13 ||< ptn_corc_l4:stair_1
*surf,ptn_corid,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,56,06  #  14 ||< Wall-6:L4_sw_meet
*surf,ptn_cor_b,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,56,05  #  15 ||< Wall-5:L4_sw_meet
*surf,ptn_cor_c,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,56,04  #  16 ||< Wall-4:L4_sw_meet
*surf,ptn_meet_a,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,56,03  #  17 ||< Wall-3:L4_sw_meet
*surf,ptn_meet_b,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,56,02  #  18 ||< Wall-2:L4_sw_meet
*surf,ceiling_l4op,CEIL,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #  19 ||< not yet defined
*surf,floor_l4op,FLOR,-,-,-,flr_ov_pln,OPAQUE,UNKNOWN,0,0  #  20 ||< not yet defined
*surf,open_a,VERT,-,-,-,fictitious,SC_fictit,UNKNOWN,0,0  #  21 ||< not yet defined
*surf,open_b,VERT,-,-,-,fictitious,SC_fictit,UNKNOWN,0,0  #  22 ||< not yet defined
*surf,open_c,VERT,-,-,-,fictitious,SC_fictit,UNKNOWN,0,0  #  23 ||< not yet defined
*surf,l3_lw2_fr,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,59,01  #  24 ||< l3_lw2_fr:L4_light_w1
*surf,l3_lw2_ri,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,59,02  #  25 ||< l3_lw2_ri:L4_light_w1
*surf,l3_lw2_bk,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,59,03  #  26 ||< l3_lw2_bk:L4_light_w1
*surf,l3_lw2_lef,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,59,04  #  27 ||< l3_lw2_lef:L4_light_w1
*surf,left_rail,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,59,07  #  28 ||< left_rail:L4_light_w1
*surf,front_rail,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,59,08  #  29 ||< front_rail:L4_light_w1
*surf,right_rail,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,59,09  #  30 ||< right_rail:L4_light_w1
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,20,   468.27 0  # zone base list
