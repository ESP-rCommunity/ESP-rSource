*Geometry 1.1,GEN,plenum # tag version, format, zone name
*date Tue Aug 10 11:41:24 2010  # latest file modification 
plenum is over the one bed 400 zone
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,7.20000,4.30000,2.70000  #   1
*vertex,0.00000,0.00000,3.25000  #   2
*vertex,7.20000,0.00000,3.25000  #   3
*vertex,7.20000,4.30000,3.25000  #   4
*vertex,0.00000,4.30000,3.25000  #   5
*vertex,7.20000,0.60000,2.70000  #   6
*vertex,0.40000,0.40000,2.70000  #   7
*vertex,0.40000,4.30000,2.70000  #   8
*vertex,3.60000,0.40000,2.70000  #   9
*vertex,3.60000,4.30000,2.70000  #  10
*vertex,3.60000,0.60000,2.70000  #  11
*vertex,3.60000,0.40000,2.75000  #  12
*vertex,3.60000,0.00000,2.75000  #  13
*vertex,0.40000,0.40000,2.75000  #  14
*vertex,0.40000,4.30000,2.75000  #  15
*vertex,0.00000,0.00000,2.75000  #  16
*vertex,0.00000,4.30000,2.75000  #  17
*vertex,3.60000,0.60000,2.75000  #  18
*vertex,7.20000,0.60000,2.75000  #  19
*vertex,7.20000,0.00000,2.75000  #  20
# 
# tag, number of vertices followed by list of associated vert
*edges,5,16,13,20,3,2  #  1
*edges,6,20,19,6,1,4,3  #  2
*edges,7,1,10,8,15,17,5,4  #  3
*edges,4,17,16,2,5  #  4
*edges,4,2,3,4,5  #  5
*edges,5,9,7,8,10,11  #  6
*edges,4,6,11,10,1  #  7
*edges,4,7,9,12,14  #  8
*edges,4,8,7,14,15  #  9
*edges,6,13,16,17,15,14,12  # 10
*edges,4,11,6,19,18  # 11
*edges,4,9,11,18,12  # 12
*edges,5,20,13,12,18,19  # 13
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,exterior,VERT,-,-,-,ext_wall_a,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,Surf-2,VERT,-,-,-,ptn_type_d,OPAQUE,SIMILAR,00,00  #   2 ||< identical environment
*surf,Surf-3,VERT,-,-,-,ptn_type_d,OPAQUE,SIMILAR,00,00  #   3 ||< identical environment
*surf,Surf-4,VERT,-,-,-,ptn_type_d,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,top,CEIL,-,-,-,struc_ceil,OPAQUE,SIMILAR,00,00  #   5 ||< identical environment
*surf,ceiling,FLOR,-,-,-,susp_ceil,OPAQUE,ANOTHER,01,05  #   6 ||< ceiling:one_bed_400
*surf,ceil_600,FLOR,-,-,-,susp_ceil,OPAQUE,ANOTHER,04,12  #   7 ||< ceil_600:one_bed_600
*surf,back_a_edge,VERT,-,-,-,alum_insu,OPAQUE,ANOTHER,05,03  #   8 ||< back_a_edge:rad_400
*surf,back_b_edge,VERT,-,-,-,alum_insu,OPAQUE,ANOTHER,05,04  #   9 ||< back_b_edge:rad_400
*surf,upper,FLOR,-,-,-,alum_insu,OPAQUE,ANOTHER,05,07  #  10 ||< upper:rad_400
*surf,back_edge,VERT,-,-,-,alum_insu,OPAQUE,ANOTHER,06,03  #  11 ||< back_edge:rad_600
*surf,left_edge,VERT,-,-,-,alum_insu,OPAQUE,ANOTHER,06,04  #  12 ||< left_edge:rad_600
*surf,upper,FLOR,-,-,-,alum_insu,OPAQUE,ANOTHER,06,06  #  13 ||< upper:rad_600
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,15.48,1  # zone base
