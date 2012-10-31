*Geometry 1.1,GEN,manager_a # tag version, format, zone name
*date Fri Jul 17 08:07:58 2009  # latest file modification 
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
*vertex,0.00000,0.00000,1.20900  #  11
*vertex,3.00000,0.00000,1.20900  #  12
*vertex,0.10000,0.00000,1.30900  #  13
*vertex,2.90000,0.00000,1.30900  #  14
*vertex,2.90000,0.00000,2.90000  #  15
*vertex,0.10000,0.00000,2.90000  #  16
*vertex,2.90000,4.50000,0.10000  #  17
*vertex,1.30000,4.50000,0.10000  #  18
*vertex,1.30000,4.50000,2.90000  #  19
*vertex,2.90000,4.50000,2.90000  #  20
*vertex,0.40000,4.50000,0.00000  #  21
*vertex,0.40000,4.50000,3.00000  #  22
*vertex,0.00000,2.50000,0.00000  #  23
*vertex,0.00000,2.35000,0.00000  #  24
*vertex,0.00000,2.50000,3.00000  #  25
*vertex,0.00000,2.35000,3.00000  #  26
*vertex,0.40000,4.50000,2.90000  #  27
*vertex,1.20000,4.50000,2.90000  #  28
*vertex,2.68500,0.00000,0.00000  #  29
*vertex,2.68500,0.00000,1.20900  #  30
# 
# tag, number of vertices followed by list of associated vert
*edges,5,2,3,8,7,12  #  1
*edges,11,3,4,28,9,8,3,17,20,19,18,17  #  2
*edges,4,4,21,27,28  #  3
*edges,4,5,23,25,10  #  4
*edges,8,6,7,8,9,22,10,25,26  #  5
*edges,9,1,24,23,5,21,4,3,2,29  #  6
*edges,4,29,2,12,30  #  7
*edges,11,11,30,12,7,6,11,13,16,15,14,13  #  8
*edges,4,13,14,15,16  #  9
*edges,4,17,18,19,20  # 10
*edges,7,21,5,10,22,9,28,27  # 11
*edges,4,23,24,26,25  # 12
*edges,5,24,1,11,6,26  # 13
*edges,4,1,29,30,11  # 14
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,pt_general,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,ANOTHER,02,01  #   1 ||< pt_general:manager_b
*surf,part_frame,VERT,-,-,-,insul_frame,OPAQUE,ANOTHER,03,08  #   2 ||< part_frame:corridor
*surf,door,VERT,-,-,-,door,OPAQUE,ANOTHER,03,06  #   3 ||< door:corridor
*surf,pt_other,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,ceiling,CEIL,-,-,-,ceiling,OPAQUE,SIMILAR,00,00  #   5 ||< identical environment
*surf,floor,FLOR,-,-,-,susp_flr_re,OPAQUE,SIMILAR,00,00  #   6 ||< identical environment
*surf,spandrel,VERT,-,-,-,insul_frame,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,frame,VERT,-,-,-,insul_frame,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,glazing,VERT,-,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #   9 ||< external
*surf,part_glaz,VERT,-,-,-,dbl_glz,DCF7671_06nb,ANOTHER,03,09  #  10 ||< part_glaz:corridor
*surf,ptn_corid,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,ANOTHER,03,07  #  11 ||< ptn_corid:corridor
*surf,pt_other_a,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,SIMILAR,00,00  #  12 ||< identical environment
*surf,pt_other_b,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,SIMILAR,00,00  #  13 ||< identical environment
*surf,PVmod,VERT,-,-,-,PV_constr,PVmod_opt,EXTERIOR,0,0  #  14 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    13.50 0  # zone base list
