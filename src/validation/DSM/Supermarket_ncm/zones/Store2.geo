*Geometry 1.1,GEN,Store2 # tag version, format, zone name
*date Sat Feb 18 19:57:32 2012  # latest file modification 
Store2 describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,69.00000,3.00000  #   1
*vertex,100.00000,69.00000,3.00000  #   2
*vertex,100.00000,78.50000,3.00000  #   3
*vertex,0.00000,78.50000,3.00000  #   4
*vertex,0.00000,69.00000,6.00000  #   5
*vertex,100.00000,69.00000,6.00000  #   6
*vertex,100.00000,78.50000,6.00000  #   7
*vertex,0.00000,78.50000,6.00000  #   8
*vertex,65.81139,78.50000,4.02566  #   9
*vertex,34.18861,78.50000,4.02566  #  10
*vertex,34.18861,78.50000,4.97434  #  11
*vertex,65.81139,78.50000,4.97434  #  12
*vertex,68.00000,69.00000,3.00000  #  13
*vertex,68.00000,69.00000,6.00000  #  14
*vertex,92.00000,69.00000,3.00000  #  15
*vertex,92.00000,78.50000,3.00000  #  16
*vertex,68.00000,78.50000,3.00000  #  17
*vertex,68.00000,78.50000,6.00000  #  18
*vertex,68.00000,69.00000,0.00000  #  19
*vertex,68.00000,78.50000,0.00000  #  20
*vertex,92.00000,78.50000,0.00000  #  21
*vertex,92.00000,69.00000,0.00000  #  22
*vertex,90.00000,78.50000,0.00000  #  23
*vertex,86.00000,78.50000,0.00000  #  24
*vertex,86.00000,78.50000,4.00000  #  25
*vertex,90.00000,78.50000,4.00000  #  26
# 
# tag, number of vertices followed by list of associated vert
*edges,7,19,22,15,2,6,14,13  #  1
*edges,4,2,3,7,6  #  2
*edges,10,17,4,8,18,17,9,12,11,10,9  #  3
*edges,4,4,1,5,8  #  4
*edges,6,5,14,6,7,18,8  #  5
*edges,4,1,4,17,13  #  6
*edges,4,9,10,11,12  #  7
*edges,4,1,13,14,5  #  8
*edges,4,2,15,16,3  #  9
*edges,4,20,19,13,17  # 10
*edges,4,22,21,16,15  # 11
*edges,6,22,19,20,24,23,21  # 12
*edges,11,21,23,26,25,24,20,17,18,7,3,16  # 13
*edges,4,23,24,25,26  # 14
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_refr,VERT,-,-,-,mass_part,OPAQUE,ANOTHER,02,20  #   1 ||< ptn_refra:Refrig
*surf,right_fac,VERT,-,WALL,-,SMexwal2008,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,back_fac,VERT,-,WALL,-,SMexwal2008,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,left_fac,VERT,-,WALL,-,SMexwal2008,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,roof,CEIL,-,ROOF,FLAT,SM_roof2008,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,floor,FLOR,-,-,-,susp_flr_re,OPAQUE,ANOTHER,09,03  #   6 ||< ceiling:Store1
*surf,fac_glz,VERT,back_fac,C-WINDOW,CLOSED,SM_glazing,DCF7671_06nb,EXTERIOR,0,0  #   7 ||< external
*surf,ptn_sales,VERT,-,-,-,mass_part,OPAQUE,ANOTHER,01,02  #   8 ||< ptn_st2:Sales
*surf,ptn_storage,FLOR,-,-,-,insul_cab,OPAQUE,ANOTHER,13,04  #   9 ||< ceiling:cold_store
*surf,ptn_st1,VERT,-,ANOTHER,09,insul_cab,OPAQUE,ANOTHER,09,07  #  10 ||< ptn_storage:Store1
*surf,ptn_cold,VERT,-,-,-,insul_cab,OPAQUE,ANOTHER,13,06  #  11 ||< ptn_storage:cold_store
*surf,grnd_floor,FLOR,-,FLOOR,EXTGRND,SM_grnd_floo,OPAQUE,GROUND,01,0  #  12 ||< ground profile  1
*surf,double_back,VERT,-,WALL,-,SMexwal2008,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,freight,VERT,double_back,V-DOOR,CLOSED,Vehicle_door,OPAQUE,EXTERIOR,0,0  #  14 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,   950.00 0  # zone base list
