*Geometry 1.1,GEN,manager_a # tag version, format, zone name
*date Tue Jul 14 09:56:28 2009  # latest file modification 
manager describes cellular office on left
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,3.00000,0.00000,0.00000  #   2
*vertex,3.00000,4.50000,0.00000  #   3
*vertex,1.20000,4.50000,0.00000  #   4
*vertex,0.00000,4.50000,0.00000  #   5
*vertex,0.00000,0.00000,3.00000  #   6
*vertex,3.00000,0.00000,3.00000  #   7
*vertex,3.00000,4.50000,3.00000  #   8
*vertex,1.20000,4.50000,3.00000  #   9
*vertex,0.00000,4.50000,3.00000  #  10
*vertex,0.00000,0.00000,0.90000  #  11
*vertex,3.00000,0.00000,0.90000  #  12
*vertex,1.00000,0.00000,0.90000  #  13
*vertex,2.00000,0.00000,0.90000  #  14
*vertex,0.10000,0.00000,1.00000  #  15
*vertex,2.90000,0.00000,1.00000  #  16
*vertex,2.90000,0.00000,2.90000  #  17
*vertex,0.10000,0.00000,2.90000  #  18
*vertex,2.90000,4.50000,0.10000  #  19
*vertex,1.30000,4.50000,0.10000  #  20
*vertex,1.30000,4.50000,2.90000  #  21
*vertex,2.90000,4.50000,2.90000  #  22
*vertex,0.40000,4.50000,0.00000  #  23
*vertex,0.40000,4.50000,3.00000  #  24
*vertex,0.00000,2.50000,0.00000  #  25
*vertex,0.00000,2.35000,0.00000  #  26
*vertex,0.00000,2.50000,3.00000  #  27
*vertex,0.00000,2.35000,3.00000  #  28
*vertex,0.40000,4.50000,2.90000  #  29
*vertex,1.20000,4.50000,2.90000  #  30
# 
# tag, number of vertices followed by list of associated vert
*edges,5,2,3,8,7,12  #  1
*edges,11,3,4,30,9,8,3,19,22,21,20,19  #  2
*edges,4,4,23,29,30  #  3
*edges,4,5,25,27,10  #  4
*edges,8,6,7,8,9,24,10,27,28  #  5
*edges,8,1,26,25,5,23,4,3,2  #  6
*edges,6,1,2,12,14,13,11  #  7
*edges,12,11,13,14,12,7,6,11,15,18,17,16,15  #  8
*edges,4,15,16,17,18  #  9
*edges,4,19,20,21,22  # 10
*edges,7,23,5,10,24,9,30,29  # 11
*edges,4,25,26,28,27  # 12
*edges,5,26,1,11,6,28  # 13
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,pt_general,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,ANOTHER,02,01  #   1 ||< pt_general:manager_b
*surf,part_frame,VERT,-,-,-,insul_frame,OPAQUE,ANOTHER,03,08  #   2 ||< part_frame:coridor
*surf,door,VERT,-,-,-,door,OPAQUE,ANOTHER,03,06  #   3 ||< door:coridor
*surf,pt_other,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,ceiling,CEIL,-,-,-,ceiling,OPAQUE,SIMILAR,00,00  #   5 ||< identical environment
*surf,floor,FLOR,-,-,-,susp_flr_re,OPAQUE,SIMILAR,00,00  #   6 ||< identical environment
*surf,spandral,VERT,-,-,-,insul_frame,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,frame,VERT,-,-,-,insul_frame,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,glazing,VERT,-,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #   9 ||< external
*surf,part_glaz,VERT,-,-,-,dbl_glz,DCF7671_06nb,ANOTHER,03,09  #  10 ||< part_glaz:coridor
*surf,ptn_corid,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,ANOTHER,03,07  #  11 ||< ptn_corid:coridor
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
