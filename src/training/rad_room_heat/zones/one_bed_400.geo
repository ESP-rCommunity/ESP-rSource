*Geometry 1.1,GEN,one_bed_400 # tag version, format, zone name
*date Tue Aug 10 12:19:10 2010  # latest file modification 
bed_400 describes a single bed room with 400mm wide pnls
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,3.60000,0.00000,0.00000  #   2
*vertex,3.60000,4.30000,0.00000  #   3
*vertex,0.00000,4.30000,0.00000  #   4
*vertex,0.00000,0.00000,2.70000  #   5
*vertex,3.60000,0.00000,2.70000  #   6
*vertex,3.60000,4.30000,2.70000  #   7
*vertex,0.00000,4.30000,2.70000  #   8
*vertex,0.90000,0.00000,0.65000  #   9
*vertex,2.70000,0.00000,0.65000  #  10
*vertex,2.70000,0.00000,2.25000  #  11
*vertex,0.90000,0.00000,2.25000  #  12
*vertex,0.97024,0.00000,0.71244  #  13
*vertex,2.62976,0.00000,0.71244  #  14
*vertex,2.62976,0.00000,2.18756  #  15
*vertex,0.97024,0.00000,2.18756  #  16
*vertex,1.20000,4.30000,0.00000  #  17
*vertex,0.10000,4.30000,0.00000  #  18
*vertex,0.10000,4.30000,2.10000  #  19
*vertex,1.20000,4.30000,2.10000  #  20
*vertex,1.40000,1.60000,0.90000  #  21
*vertex,3.40000,1.60000,0.90000  #  22
*vertex,3.40000,2.50000,0.90000  #  23
*vertex,1.40000,2.50000,0.90000  #  24
*vertex,0.40000,0.40000,2.70000  #  25
*vertex,0.40000,4.30000,2.70000  #  26
*vertex,3.60000,0.40000,2.70000  #  27
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,6,5,1,9,12,11,10,9  #  1
*edges,5,2,3,7,27,6  #  2
*edges,9,3,17,20,19,18,4,8,26,7  #  3
*edges,4,4,1,5,8  #  4
*edges,4,25,27,7,26  #  5
*edges,6,1,4,18,17,3,2  #  6
*edges,10,9,10,11,12,9,13,16,15,14,13  #  7
*edges,4,13,14,15,16  #  8
*edges,4,17,18,19,20  #  9
*edges,4,21,22,23,24  # 10
*edges,4,22,21,24,23  # 11
*edges,6,5,6,27,25,26,8  # 12
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,exterior_wl,VERT,-,-,-,ext_wall_a,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,left_ptn,VERT,-,-,-,ptn_type_d,OPAQUE,ANOTHER,04,04  #   2 ||< right_ptn:one_bed_600
*surf,coridor_ptn,VERT,-,-,-,ptn_type_d,OPAQUE,CONSTANT,21,00  #   3 ||< constant @ 21dC &   0W rad
*surf,right_ptn,VERT,-,-,-,ptn_type_d,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,ceiling,CEIL,-,-,-,susp_ceil,OPAQUE,ANOTHER,02,06  #   5 ||< ceiling:plenum
*surf,floor,FLOR,-,-,-,struc_flr,OPAQUE,ANOTHER,03,07  #   6 ||< floor:lower_plen
*surf,frame,VERT,exterior_wl,-,-,door,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,window,VERT,frame,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #   8 ||< external
*surf,door,VERT,coridor_ptn,-,-,door,OPAQUE,CONSTANT,21,00  #   9 ||< constant @ 21dC &   0W rad
*surf,bed,CEIL,-,-,-,door,OPAQUE,ADIABATIC,0,0  #  10 ||< adiabatic
*surf,bed_base,FLOR,-,-,-,door,OPAQUE,ADIABATIC,0,0  #  11 ||< adiabatic
*surf,rad_pnl,CEIL,-,-,-,alum_panel,OPAQUE,ANOTHER,05,08  #  12 ||< base:rad_400
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,all_applicable   1 # insolation sources
  8
# 
*base_list,2,6,11,    17.28 0  # zone base list
