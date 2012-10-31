*Geometry 1.1,GEN,Refrig # tag version, format, zone name
*date Mon Feb 13 19:46:16 2012  # latest file modification 
Refrig describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,68.00000,13.50000,0.00000  #   1
*vertex,100.00000,13.50000,0.00000  #   2
*vertex,100.00000,69.00000,0.00000  #   3
*vertex,68.00000,69.00000,0.00000  #   4
*vertex,68.00000,13.50000,6.00000  #   5
*vertex,100.00000,13.50000,6.00000  #   6
*vertex,100.00000,69.00000,6.00000  #   7
*vertex,68.00000,69.00000,6.00000  #   8
*vertex,100.00000,32.47468,2.05132  #   9
*vertex,100.00000,50.02532,2.05132  #  10
*vertex,100.00000,50.02532,3.94868  #  11
*vertex,100.00000,32.47468,3.94868  #  12
*vertex,77.50000,42.50000,0.00000  #  13
*vertex,93.50000,42.50000,0.00000  #  14
*vertex,93.50000,42.50000,2.50000  #  15
*vertex,77.50000,42.50000,2.50000  #  16
*vertex,93.50000,62.50000,0.00000  #  17
*vertex,93.50000,62.50000,2.50000  #  18
*vertex,77.50000,62.50000,0.00000  #  19
*vertex,77.50000,62.50000,2.50000  #  20
*vertex,77.50000,20.00000,0.00000  #  21
*vertex,93.50000,20.00000,0.00000  #  22
*vertex,93.50000,20.00000,2.50000  #  23
*vertex,77.50000,20.00000,2.50000  #  24
*vertex,93.50000,40.00000,0.00000  #  25
*vertex,93.50000,40.00000,2.50000  #  26
*vertex,77.50000,40.00000,0.00000  #  27
*vertex,77.50000,40.00000,2.50000  #  28
*vertex,68.00000,69.00000,3.00000  #  29
*vertex,100.00000,69.00000,3.00000  #  30
*vertex,100.00000,13.50000,3.00000  #  31
*vertex,68.00000,13.50000,3.00000  #  32
*vertex,68.00000,41.00000,0.00000  #  33
*vertex,100.00000,41.00000,0.00000  #  34
*vertex,92.00000,69.00000,0.00000  #  35
*vertex,92.00000,69.00000,3.00000  #  36
*vertex,100.00000,41.25000,7.00000  #  37
*vertex,68.00000,41.25000,7.00000  #  38
# 
# tag, number of vertices followed by list of associated vert
*edges,4,31,6,5,32  #  1
*edges,14,2,34,3,30,7,37,6,31,2,9,12,11,10,9  #  2
*edges,8,4,33,1,32,5,38,8,29  #  3
*edges,4,5,6,37,38  #  4
*edges,4,9,10,11,12  #  5
*edges,4,14,13,16,15  #  6
*edges,4,17,14,15,18  #  7
*edges,4,19,17,18,20  #  8
*edges,4,13,19,20,16  #  9
*edges,4,15,16,20,18  # 10
*edges,4,22,21,24,23  # 11
*edges,4,25,22,23,26  # 12
*edges,4,27,25,26,28  # 13
*edges,4,21,27,28,24  # 14
*edges,4,23,24,28,26  # 15
*edges,4,1,2,31,32  # 16
*edges,10,1,33,34,2,1,21,22,25,27,21  # 17
*edges,11,3,34,33,4,35,3,17,19,13,14,17  # 18
*edges,4,3,35,36,30  # 19
*edges,7,35,4,29,8,7,30,36  # 20
*edges,4,7,8,38,37  # 21
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_staff,VERT,-,-,-,mass_part,OPAQUE,ANOTHER,08,03  #   1 ||< ptn_shop:Staff_traing
*surf,right_fac,VERT,-,WALL,-,SMexwal2008,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,fict_store,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,01,01  #   3 ||< fict_refr:Sales
*surf,ceiling,SLOP,-,ROOF,FLAT,SM_roof2008,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,fac_glaz,VERT,right_fac,C-WINDOW,CLOSED,SM_glazing,DCF7671_06nb,EXTERIOR,0,0  #   5 ||< external
*surf,low_cab_fr,VERT,-,-,-,insul_cab,OPAQUE,ANOTHER,11,01  #   6 ||< cab_front:Cabinet_hi
*surf,low_cab_ri,VERT,-,-,-,insul_cab,OPAQUE,ANOTHER,11,02  #   7 ||< cab_right:Cabinet_hi
*surf,low_cab_bk,VERT,-,-,-,insul_cab,OPAQUE,ANOTHER,11,03  #   8 ||< cab_back:Cabinet_hi
*surf,low_cab_lft,VERT,-,-,-,insul_cab,OPAQUE,ANOTHER,11,04  #   9 ||< cab_left:Cabinet_hi
*surf,low_cab_top,FLOR,-,-,-,fictitious,SC_fictit,ANOTHER,11,05  #  10 ||< cab_top:Cabinet_hi
*surf,cab_fr,VERT,-,-,-,insul_cab,OPAQUE,ANOTHER,12,01  #  11 ||< lt_cab_fr:Cabinet_lw
*surf,cab_ri,VERT,-,-,-,insul_cab,OPAQUE,ANOTHER,12,02  #  12 ||< lt_cab_ri:Cabinet_lw
*surf,cab_bk,VERT,-,-,-,insul_cab,OPAQUE,ANOTHER,12,03  #  13 ||< lt_cab_bk:Cabinet_lw
*surf,cab_lft,VERT,-,-,-,insul_cab,OPAQUE,ANOTHER,12,04  #  14 ||< lt_cab_lft:Cabinet_lw
*surf,cab_top,FLOR,-,-,-,fictitious,SC_fictit,ANOTHER,12,05  #  15 ||< lt_cab_top:Cabinet_lw
*surf,ptn_cafe,VERT,-,-,-,mass_part,OPAQUE,ANOTHER,05,03  #  16 ||< ptn_shop:Cafe
*surf,floor_a,FLOR,-,-,-,SM_grnd_floo,OPAQUE,GROUND,01,00  #  17 ||< ground profile  1
*surf,floor_b,FLOR,-,-,-,SM_grnd_floo,OPAQUE,GROUND,01,00  #  18 ||< ground profile  1
*surf,ptn_refr,VERT,-,-,-,insul_cab,OPAQUE,ANOTHER,13,01  #  19 ||< ptn_refr:cold_store
*surf,ptn_refra,VERT,-,-,-,mass_part,OPAQUE,ANOTHER,10,01  #  20 ||< ptn_refr:Store2
*surf,roof_b,SLOP,-,ROOF,FLAT,SM_roof2008,OPAQUE,EXTERIOR,0,0  #  21 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,1776.00,1  # zone base
