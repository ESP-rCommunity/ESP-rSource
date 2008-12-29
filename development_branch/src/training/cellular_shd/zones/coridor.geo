*Geometry 1.1,GEN,coridor # tag version, format, zone name
*date Thu Aug 23 17:28:43 2007  # latest file modification 
coridor describes passage adjacent to offices
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,4.55000,0.00000  #   1
*vertex,6.10000,4.55000,0.00000  #   2
*vertex,6.10000,5.55000,0.00000  #   3
*vertex,0.00000,5.55000,0.00000  #   4
*vertex,0.00000,4.55000,3.00000  #   5
*vertex,6.10000,4.55000,3.00000  #   6
*vertex,6.10000,5.55000,3.00000  #   7
*vertex,0.00000,5.55000,3.00000  #   8
*vertex,1.20000,4.55000,0.00000  #   9
*vertex,0.40000,4.55000,0.00000  #  10
*vertex,0.40000,4.55000,2.90000  #  11
*vertex,1.20000,4.55000,2.90000  #  12
*vertex,0.40000,4.55000,3.00000  #  13
*vertex,1.20000,4.55000,3.00000  #  14
*vertex,3.00000,4.55000,0.00000  #  15
*vertex,3.00000,4.55000,3.00000  #  16
*vertex,2.90000,4.55000,0.10000  #  17
*vertex,2.90000,4.55000,2.90000  #  18
*vertex,1.30000,4.55000,2.90000  #  19
*vertex,1.30000,4.55000,0.10000  #  20
*vertex,4.90000,4.55000,0.00000  #  21
*vertex,3.10000,4.55000,0.00000  #  22
*vertex,3.20000,4.55000,0.10000  #  23
*vertex,4.80000,4.55000,0.10000  #  24
*vertex,4.80000,4.55000,2.90000  #  25
*vertex,3.20000,4.55000,2.90000  #  26
*vertex,3.10000,4.55000,3.00000  #  27
*vertex,4.90000,4.55000,3.00000  #  28
*vertex,4.90000,4.55000,2.90000  #  29
*vertex,5.70000,4.55000,0.00000  #  30
*vertex,5.70000,4.55000,2.90000  #  31
*vertex,5.70000,4.55000,3.00000  #  32
# 
# tag, number of vertices followed by list of associated vert
*edges,4,2,3,7,6  #  1
*edges,4,3,4,8,7  #  2
*edges,4,4,1,5,8  #  3
*edges,10,5,13,14,16,27,28,32,6,7,8  #  4
*edges,10,1,4,3,2,30,21,22,15,9,10  #  5
*edges,4,10,9,12,11  #  6
*edges,7,1,10,11,12,14,13,5  #  7
*edges,11,9,15,17,20,19,18,17,15,16,14,12  #  8
*edges,4,20,17,18,19  #  9
*edges,11,22,21,29,28,27,22,23,26,25,24,23  # 10
*edges,4,21,30,31,29  # 11
*edges,7,30,2,6,32,28,29,31  # 12
*edges,4,23,24,25,26  # 13
*edges,4,15,22,27,16  # 14
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,right,VERT,-,-,-,door,OPAQUE,SIMILAR,00,00  #   1 ||< identical environment
*surf,wall,VERT,-,-,-,gyp_blk_ptn,OPAQUE,SIMILAR,00,00  #   2 ||< identical environment
*surf,left,VERT,-,-,-,door,OPAQUE,SIMILAR,00,00  #   3 ||< identical environment
*surf,ceiling,CEIL,-,-,-,ceiling,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,floor,FLOR,-,-,-,susp_flr_re,OPAQUE,SIMILAR,00,00  #   5 ||< identical environment
*surf,door,VERT,-,-,-,door,OPAQUE,ANOTHER,01,03  #   6 ||< door:manager_a
*surf,ptn_corid,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,ANOTHER,01,11  #   7 ||< ptn_corid:manager_a
*surf,part_frame,VERT,-,-,-,insul_frame,OPAQUE,ANOTHER,01,02  #   8 ||< part_frame:manager_a
*surf,part_glaz,VERT,part_frame,-,-,dbl_glz,DCF7671_06nb,ANOTHER,01,10  #   9 ||< part_glaz:manager_a
*surf,part_frameb,VERT,-,-,-,insul_frame,OPAQUE,ANOTHER,02,02  #  10 ||< part_frame:manager_b
*surf,door_b,VERT,-,-,-,door,OPAQUE,ANOTHER,02,03  #  11 ||< door:manager_b
*surf,ptn_coridb,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,ANOTHER,02,11  #  12 ||< ptn_corid:manager_b
*surf,part_glazb,VERT,part_frameb,-,-,dbl_glz,DCF7671_06nb,ANOTHER,02,10  #  13 ||< part_glaz:manager_b
*surf,filler,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,ADIABATIC,0,0  #  14 ||< adiabatic
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,2,6,5,     8.42 0  # zone base list
