*Geometry 1.1,GEN,man_opr_inf # tag version, format, zone name
*date Tue Aug 14 10:22:43 2007  # latest file modification 
man_opr_inf is an office with temp controlled infiltration
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,4.00000,0.00000,0.00000  #   1
*vertex,7.00000,0.00000,0.00000  #   2
*vertex,7.00000,4.50000,0.00000  #   3
*vertex,5.00000,4.50000,0.00000  #   4
*vertex,4.00000,4.50000,0.00000  #   5
*vertex,4.00000,0.00000,3.00000  #   6
*vertex,7.00000,0.00000,3.00000  #   7
*vertex,7.00000,4.50000,3.00000  #   8
*vertex,5.00000,4.50000,3.00000  #   9
*vertex,4.00000,4.50000,3.00000  #  10
*vertex,4.00000,0.00000,0.90000  #  11
*vertex,7.00000,0.00000,0.90000  #  12
*vertex,5.00000,0.00000,0.90000  #  13
*vertex,6.00000,0.00000,0.90000  #  14
*vertex,5.00000,0.00000,0.50000  #  15
*vertex,6.00000,0.00000,0.50000  #  16
*vertex,4.10000,0.00000,1.00000  #  17
*vertex,6.90000,0.00000,1.00000  #  18
*vertex,6.90000,0.00000,2.90000  #  19
*vertex,4.10000,0.00000,2.90000  #  20
*vertex,6.90000,4.50000,0.10000  #  21
*vertex,5.10000,4.50000,0.10000  #  22
*vertex,5.10000,4.50000,2.90000  #  23
*vertex,6.90000,4.50000,2.90000  #  24
# 
# tag, number of vertices followed by list of associated vert
*edges,5,2,3,8,7,12  #  1
*edges,10,3,4,9,8,3,21,24,23,22,21  #  2
*edges,4,4,5,10,9  #  3
*edges,5,5,1,11,6,10  #  4
*edges,5,6,7,8,9,10  #  5
*edges,5,1,5,4,3,2  #  6
*edges,8,1,2,12,14,16,15,13,11  #  7
*edges,4,15,16,14,13  #  8
*edges,12,11,13,14,12,7,6,11,17,20,19,18,17  #  9
*edges,4,17,18,19,20  # 10
*edges,4,21,22,23,24  # 11
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,pt_general,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,SIMILAR,00,00  #   1 ||< identical environment
*surf,part_frame,VERT,-,-,-,insul_frame,OPAQUE,SIMILAR,00,00  #   2 ||< identical environment
*surf,door,VERT,-,-,-,door,OPAQUE,SIMILAR,00,00  #   3 ||< identical environment
*surf,pt_other,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,ceiling,CEIL,-,-,-,ceiling,OPAQUE,SIMILAR,00,00  #   5 ||< identical environment
*surf,floor,FLOR,-,-,-,susp_flr_re,OPAQUE,SIMILAR,00,00  #   6 ||< identical environment
*surf,spandral,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,vent,VERT,spandral,-,-,insul_frame,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,frame,VERT,-,-,-,insul_frame,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,glazing,VERT,frame,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #  10 ||< external
*surf,part_glaz,VERT,part_frame,-,-,dbl_glz,DCF7671_06nb,SIMILAR,00,00  #  11 ||< identical environment
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    13.50 0  # zone base list
