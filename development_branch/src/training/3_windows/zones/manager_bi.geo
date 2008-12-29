*Geometry 1.1,GEN,manager_bi # tag version, format, zone name
*date Thu Aug 23 17:51:32 2007  # latest file modification 
manager_bi describes an office with bi-directional opening
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,8.00000,0.00000,0.00000  #   1
*vertex,11.00000,0.00000,0.00000  #   2
*vertex,11.00000,4.50000,0.00000  #   3
*vertex,9.00000,4.50000,0.00000  #   4
*vertex,8.00000,4.50000,0.00000  #   5
*vertex,8.00000,0.00000,3.00000  #   6
*vertex,11.00000,0.00000,3.00000  #   7
*vertex,11.00000,4.50000,3.00000  #   8
*vertex,9.00000,4.50000,3.00000  #   9
*vertex,8.00000,4.50000,3.00000  #  10
*vertex,8.00000,0.00000,0.90000  #  11
*vertex,11.00000,0.00000,0.90000  #  12
*vertex,9.00000,0.00000,0.90000  #  13
*vertex,10.00000,0.00000,0.90000  #  14
*vertex,8.10000,0.00000,1.00000  #  15
*vertex,10.90000,0.00000,1.00000  #  16
*vertex,10.90000,0.00000,2.90000  #  17
*vertex,8.10000,0.00000,2.90000  #  18
*vertex,10.90000,4.50000,0.10000  #  19
*vertex,9.10000,4.50000,0.10000  #  20
*vertex,9.10000,4.50000,2.90000  #  21
*vertex,10.90000,4.50000,2.90000  #  22
*vertex,10.01600,0.00000,1.00000  #  23
*vertex,10.01600,0.00000,2.90000  #  24
# 
# tag, number of vertices followed by list of associated vert
*edges,5,2,3,8,7,12  #  1
*edges,10,3,4,9,8,3,19,22,21,20,19  #  2
*edges,4,4,5,10,9  #  3
*edges,5,5,1,11,6,10  #  4
*edges,5,6,7,8,9,10  #  5
*edges,5,1,5,4,3,2  #  6
*edges,6,1,2,12,14,13,11  #  7
*edges,14,11,13,14,12,7,6,11,15,18,24,17,16,23,15  #  8
*edges,4,15,23,24,18  #  9
*edges,4,19,20,21,22  # 10
*edges,4,23,16,17,24  # 11
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
*surf,frame,VERT,-,-,-,insul_frame,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,glazing,VERT,-,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #   9 ||< external
*surf,part_glaz,VERT,-,-,-,dbl_glz,DCF7671_06nb,SIMILAR,00,00  #  10 ||< identical environment
*surf,bi-glaz,VERT,-,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #  11 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    13.50 0  # zone base list
