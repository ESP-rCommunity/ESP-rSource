*Geometry 1.1,GEN,Unit_cd # tag version, format, zone name
*date Thu Aug 23 18:32:19 2007  # latest file modification 
Unit_cd describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,12.00000,12.00000,3.80000  #   1
*vertex,29.00000,12.00000,3.80000  #   2
*vertex,29.00000,14.50000,3.80000  #   3
*vertex,34.00000,14.50000,3.80000  #   4
*vertex,34.00000,36.00000,3.80000  #   5
*vertex,16.00000,36.00000,3.80000  #   6
*vertex,12.00000,36.00000,3.80000  #   7
*vertex,12.00000,12.00000,6.50000  #   8
*vertex,29.00000,12.00000,6.50000  #   9
*vertex,29.00000,14.50000,6.50000  #  10
*vertex,34.00000,14.50000,6.50000  #  11
*vertex,34.00000,36.00000,6.50000  #  12
*vertex,16.00000,36.00000,6.50000  #  13
*vertex,12.00000,36.00000,6.50000  #  14
*vertex,12.00000,28.00000,3.80000  #  15
*vertex,12.00000,20.00000,3.80000  #  16
*vertex,12.00000,20.00000,5.90000  #  17
*vertex,12.00000,28.00000,5.90000  #  18
*vertex,34.00000,15.50000,5.00000  #  19
*vertex,34.00000,30.50000,5.00000  #  20
*vertex,34.00000,30.50000,5.90000  #  21
*vertex,34.00000,15.50000,5.90000  #  22
*vertex,12.20000,24.00000,3.90000  #  23
*vertex,33.80000,24.00000,3.90000  #  24
*vertex,33.80000,24.00000,6.40000  #  25
*vertex,12.10000,24.00000,6.40000  #  26
# 
# tag, number of vertices followed by list of associated vert
*edges,8,7,15,18,17,16,1,8,14  #  1
*edges,4,1,2,9,8  #  2
*edges,4,2,3,10,9  #  3
*edges,4,3,4,11,10  #  4
*edges,10,4,5,12,11,4,19,22,21,20,19  #  5
*edges,4,5,6,13,12  #  6
*edges,4,6,7,14,13  #  7
*edges,7,8,9,10,11,12,13,14  #  8
*edges,9,7,6,5,4,3,2,1,16,15  #  9
*edges,4,15,16,17,18  # 10
*edges,4,19,20,21,22  # 11
*edges,4,23,24,25,26  # 12
*edges,4,24,23,26,25  # 13
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,passg,VERT,-,-,-,mass_part,OPAQUE,ANOTHER,01,03  #   1 ||< prt_d-cor:corid_1
*surf,prt_de,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,08,05  #   2 ||< prt_d-e:Unit_e
*surf,str_2,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,15,13  #   3 ||< prt_d1:stair_deij
*surf,str_3,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,15,15  #   4 ||< prt_d:stair_deij
*surf,east,VERT,-,-,-,insul_mtl_p,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,prt_bc,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,07,01  #   6 ||< prt_b-c:Unit_b
*surf,prt_tc,VERT,-,-,-,gyp_blk_ptn,OPAQUE,ANOTHER,16,10  #   7 ||< prt_ctc:toilets
*surf,ceil,CEIL,-,-,-,susp_ceil,OPAQUE,ANOTHER,17,05  #   8 ||< ceil_c:roof
*surf,floor,FLOR,-,-,-,susp_floor,OPAQUE,ANOTHER,12,15  #   9 ||< upper:ceil_chi
*surf,door,VERT,-,-,-,dbl_glz,DCF7671_06nb,ANOTHER,01,13  #  10 ||< door_dcor:corid_1
*surf,w_glaz,VERT,-,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #  11 ||< external
*surf,inner_p1,VERT,-,-,-,mass_part,OPAQUE,ADIABATIC,0,0  #  12 ||< adiabatic
*surf,inner_p2,VERT,-,-,-,mass_part,OPAQUE,ADIABATIC,0,0  #  13 ||< adiabatic
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,9,   515.50 0  # zone base list
