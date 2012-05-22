*Geometry 1.1,GEN,bedroom_1 # tag version, format, zone name
*date Thu Aug 23 18:13:58 2007  # latest file modification 
bedroom_1 describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,5.01900,7.82800,2.40000  #   1
*vertex,1.92800,7.00000,2.40000  #   2
*vertex,2.18500,6.04300,2.40000  #   3
*vertex,2.62500,4.40000,2.40000  #   4
*vertex,3.00000,3.00000,2.40000  #   5
*vertex,5.60800,3.69900,2.40000  #   6
*vertex,5.34900,4.66500,2.40000  #   7
*vertex,5.83200,4.79400,2.40000  #   8
*vertex,5.27500,6.87100,2.40000  #   9
*vertex,1.92800,7.00000,4.26200  #  10
*vertex,5.01900,7.82800,4.80000  #  11
*vertex,1.92800,7.00000,4.80000  #  12
*vertex,3.00000,3.00000,4.80000  #  13
*vertex,5.60800,3.69900,4.80000  #  14
*vertex,5.34900,4.66500,4.80000  #  15
*vertex,5.83200,4.79400,4.80000  #  16
*vertex,5.27500,6.87100,4.80000  #  17
*vertex,3.52200,3.14000,2.70000  #  18
*vertex,5.35700,3.63200,2.70000  #  19
*vertex,5.35700,3.63200,4.50000  #  20
*vertex,3.52200,3.14000,4.50000  #  21
# 
# tag, number of vertices followed by list of associated vert
*edges,5,1,2,10,12,11  #  1
*edges,4,2,3,4,10  #  2
*edges,5,10,4,5,13,12  #  3
*edges,10,5,6,14,13,5,18,21,20,19,18  #  4
*edges,4,6,7,15,14  #  5
*edges,4,7,8,16,15  #  6
*edges,4,8,9,17,16  #  7
*edges,4,9,1,11,17  #  8
*edges,7,11,12,13,14,15,16,17  #  9
*edges,4,9,3,2,1  # 10
*edges,7,7,6,5,4,3,9,8  # 11
*edges,4,18,19,20,21  # 12
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,p_bath_b1,VERT,-,-,-,int_part_2,OPAQUE,ANOTHER,05,05  #   1 ||< bth_b1:bath
*surf,p_garage,VERT,-,-,-,ext_wall,OPAQUE,ANOTHER,14,07  #   2 ||< gar_b1:garage
*surf,w_ext,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,s_ext,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,p_bed4b,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,09,04  #   5 ||< prt_b1_b4b:bedroom_4
*surf,p_bed4a,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,09,03  #   6 ||< prt_b1_b4a:bedroom_4
*surf,p_bed4,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,09,02  #   7 ||< prt_b1_b4:bedroom_4
*surf,p_hall,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,10,04  #   8 ||< prt_b1:upper_hall
*surf,ceil,CEIL,-,-,-,1st_ceiling,OPAQUE,ANOTHER,13,08  #   9 ||< ceil_b1:loft
*surf,flr_kit,FLOR,-,-,-,grd_ceil_rev,OPAQUE,ANOTHER,04,09  #  10 ||< ceil_k_b1:kitchen
*surf,flr_din,FLOR,-,-,-,grd_ceil_rev,OPAQUE,ANOTHER,02,07  #  11 ||< ceil_b1:dining_room
*surf,window,VERT,-,-,-,double_glaz,DCF7672_06nb,EXTERIOR,0,0  #  12 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,12.75,1  # zone base
