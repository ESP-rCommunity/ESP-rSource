*Geometry 1.1,GEN,L4_sw_meet # tag version, format, zone name
*date Thu Aug 25 21:32:31 2011  # latest file modification 
L4_sw_meet describes meeting rds 406 407 on level four
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,7.36231,60.25000  #   1
*vertex,4.50000,8.25000,60.25000  #   2
*vertex,4.50000,14.00000,60.25000  #   3
*vertex,4.50000,21.00000,60.25000  #   4
*vertex,4.75000,24.00000,60.25000  #   5
*vertex,12.00000,24.25000,60.25000  #   6
*vertex,12.00000,30.50000,60.25000  #   7
*vertex,0.00000,30.50000,60.25000  #   8
*vertex,0.00000,28.50000,60.25000  #   9
*vertex,3.50000,28.50000,60.25000  #  10
*vertex,3.50000,24.00000,60.25000  #  11
*vertex,0.00000,24.00000,60.25000  #  12
*vertex,0.00000,7.36231,63.39300  #  13
*vertex,4.50000,8.25000,63.39300  #  14
*vertex,4.50000,14.00000,63.39300  #  15
*vertex,4.50000,21.00000,63.39300  #  16
*vertex,4.75000,24.00000,63.39300  #  17
*vertex,12.00000,24.25000,63.39300  #  18
*vertex,12.00000,30.50000,63.39300  #  19
*vertex,0.00000,30.50000,63.39300  #  20
*vertex,0.00000,28.50000,63.39300  #  21
*vertex,3.50000,28.50000,63.39300  #  22
*vertex,3.50000,24.00000,63.39300  #  23
*vertex,0.00000,24.00000,63.39300  #  24
*vertex,8.35979,30.50000,60.25000  #  25
*vertex,8.35979,30.50000,63.39300  #  26
*vertex,0.00000,8.59728,60.25000  #  27
*vertex,0.00000,8.59728,63.39300  #  28
*vertex,0.00000,11.06722,60.25000  #  29
*vertex,0.00000,11.06722,63.39300  #  30
*vertex,0.00000,13.63215,60.25000  #  31
*vertex,0.00000,13.63215,63.39300  #  32
*vertex,0.00000,16.10209,60.25000  #  33
*vertex,0.00000,16.10209,63.39300  #  34
*vertex,0.00000,18.66702,60.25000  #  35
*vertex,0.00000,18.66702,63.39300  #  36
*vertex,0.00000,21.13696,60.25000  #  37
*vertex,0.00000,21.13696,63.39300  #  38
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,14,13  #  1
*edges,4,2,3,15,14  #  2
*edges,4,3,4,16,15  #  3
*edges,4,4,5,17,16  #  4
*edges,4,5,6,18,17  #  5
*edges,4,6,7,19,18  #  6
*edges,4,8,9,21,20  #  7
*edges,4,9,10,22,21  #  8
*edges,4,10,11,23,22  #  9
*edges,4,11,12,24,23  # 10
*edges,19,13,14,15,16,17,18,19,26,20,21,22,23,24,38,36,34,32,30,28  # 11
*edges,19,1,27,29,31,33,35,37,12,11,10,9,8,25,7,6,5,4,3,2  # 12
*edges,4,25,8,20,26  # 13
*edges,4,7,25,26,19  # 14
*edges,4,27,1,13,28  # 15
*edges,4,29,27,28,30  # 16
*edges,4,31,29,30,32  # 17
*edges,4,33,31,32,34  # 18
*edges,4,35,33,34,36  # 19
*edges,4,37,35,36,38  # 20
*edges,4,12,37,38,24  # 21
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Wall-1,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,77,42  #   1 ||< to_meeting:L5_atroofltl
*surf,Wall-2,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,56,17  #   2 ||< ptn_meet_b:L4_open_s
*surf,Wall-3,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,56,16  #   3 ||< ptn_meet_a:L4_open_s
*surf,Wall-4,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,56,15  #   4 ||< ptn_cor_c:L4_open_s
*surf,Wall-5,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,56,14  #   5 ||< ptn_cor_b:L4_open_s
*surf,Wall-6,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,56,13  #   6 ||< ptn_corid:L4_open_s
*surf,Wall-8,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,51,13  #   7 ||< Wall-13:L4_sw_void
*surf,Wall-9,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,03,15  #   8 ||< right_4:lifts
*surf,Wall-10,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,03,14  #   9 ||< front_4:lifts
*surf,Wall-11,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,03,13  #  10 ||< left_4:lifts
*surf,Tl4_sw_mtcei,CEIL,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #  11 ||< not yet defined
*surf,Base-16,FLOR,-,-,-,flr_ov_pln,OPAQUE,ANOTHER,78,25  #  12 ||< Base-16:L4_flrvoid
*surf,ptn_cora_l4,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,01,42  #  13 ||< ptn_cora_l4:stair_1
*surf,ptn_corb_l4,VERT,-,-,-,conc_250,OPAQUE,ANOTHER,01,43  #  14 ||< ptn_corb_l4:stair_1
*surf,ptn_fr,VERT,-,-,-,mull_90,OPAQUE,ANOTHER,51,05  #  15 ||< Wall-5:L4_sw_void
*surf,gl_ptn_b,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,51,06  #  16 ||< Wall-6:L4_sw_void
*surf,Wall-7,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,51,07  #  17 ||< Wall-7:L4_sw_void
*surf,gl_ptn_d,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,51,08  #  18 ||< Wall-8:L4_sw_void
*surf,gl_ptn_f,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,51,09  #  19 ||< Wall-9:L4_sw_void
*surf,gl_ptn_g,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,51,10  #  20 ||< Wall-10:L4_sw_void
*surf,gl_ptn_h,VERT,-,-,-,glaz_ptn,SCF8477_10nb,ANOTHER,51,11  #  21 ||< Wall-11:L4_sw_void
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,12,   134.59 0  # zone base list
