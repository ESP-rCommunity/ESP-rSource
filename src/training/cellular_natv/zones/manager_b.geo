*Geometry 1.1,GEN,manager_b # tag version, format, zone name
*date Thu Aug 23 17:33:39 2007  # latest file modification 
manager_b describes right office
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,6.10000,0.00000,0.00000  #   1
*vertex,3.10000,0.00000,0.00000  #   2
*vertex,3.10000,4.50000,0.00000  #   3
*vertex,4.90000,4.50000,0.00000  #   4
*vertex,6.10000,4.50000,0.00000  #   5
*vertex,6.10000,0.00000,3.00000  #   6
*vertex,3.10000,0.00000,3.00000  #   7
*vertex,3.10000,4.50000,3.00000  #   8
*vertex,4.90000,4.50000,3.00000  #   9
*vertex,6.10000,4.50000,3.00000  #  10
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
*vertex,6.10000,2.50000,0.00000  #  25
*vertex,6.10000,2.35000,0.00000  #  26
*vertex,6.10000,2.50000,3.00000  #  27
*vertex,6.10000,2.35000,3.00000  #  28
*vertex,4.90000,4.50000,2.90000  #  29
*vertex,5.70000,4.50000,2.90000  #  30
# 
# tag, number of vertices followed by list of associated vert
*edges,5,3,2,12,7,8  #  1
*edges,11,4,3,19,20,21,22,19,3,8,9,29  #  2
*edges,4,23,4,29,30  #  3
*edges,4,25,5,10,27  #  4
*edges,8,7,6,28,27,10,24,9,8  #  5
*edges,8,26,1,2,3,4,23,5,25  #  6
*edges,6,2,1,11,13,14,12  #  7
*edges,12,13,11,15,16,17,18,15,11,6,7,12,14  #  8
*edges,4,16,15,18,17  #  9
*edges,4,20,19,22,21  # 10
*edges,7,5,23,30,29,9,24,10  # 11
*edges,4,26,25,27,28  # 12
*edges,5,1,26,28,6,11  # 13
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,pt_general,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,ANOTHER,01,01  #   1 ||< pt_general:manager_a
*surf,part_frame,VERT,-,-,-,insul_frame,OPAQUE,ANOTHER,03,10  #   2 ||< part_frameb:corridor
*surf,door,VERT,-,-,-,door,OPAQUE,ANOTHER,03,11  #   3 ||< door_b:corridor
*surf,pt_other,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,ceiling,CEIL,-,-,-,ceiling,OPAQUE,SIMILAR,00,00  #   5 ||< identical environment
*surf,floor,FLOR,-,-,-,susp_flr_re,OPAQUE,SIMILAR,00,00  #   6 ||< identical environment
*surf,spandral,VERT,-,-,-,insul_frame,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,frame,VERT,-,-,-,insul_frame,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,glazing,VERT,-,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #   9 ||< external
*surf,part_glaz,VERT,-,-,-,dbl_glz,DCF7671_06nb,ANOTHER,03,13  #  10 ||< part_glazb:corridor
*surf,ptn_corid,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,ANOTHER,03,12  #  11 ||< ptn_coridb:corridor
*surf,pt_other_a,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,SIMILAR,00,00  #  12 ||< identical environment
*surf,pt_other_b,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,SIMILAR,00,00  #  13 ||< identical environment
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    13.50 0  # zone base list
