*Geometry 1.1,GEN,roof # tag version, format, zone name
*date Wed Apr 18 18:09:05 2012  # latest file modification 
roof describes a cold roof for the bungalow
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,2.00000,2.40000  #   1
*vertex,0.00000,0.00000,2.40000  #   2
*vertex,2.10000,0.00000,2.40000  #   3
*vertex,2.90000,0.00000,2.40000  #   4
*vertex,3.10000,0.00000,2.40000  #   5
*vertex,3.10000,2.00000,2.40000  #   6
*vertex,2.90000,2.00000,2.40000  #   7
*vertex,2.00000,2.00000,2.40000  #   8
*vertex,0.90000,2.00000,2.40000  #   9
*vertex,4.10000,2.00000,2.40000  #  10
*vertex,4.10000,2.30000,2.40000  #  11
*vertex,4.10000,3.60000,2.40000  #  12
*vertex,4.10000,3.80000,2.40000  #  13
*vertex,3.40000,3.80000,2.40000  #  14
*vertex,3.20000,3.80000,2.40000  #  15
*vertex,2.40000,3.80000,2.40000  #  16
*vertex,2.00000,3.80000,2.40000  #  17
*vertex,2.00000,3.70000,2.40000  #  18
*vertex,2.00000,2.70000,2.40000  #  19
*vertex,0.00000,3.80000,2.40000  #  20
*vertex,0.00000,3.70000,2.40000  #  21
*vertex,0.00000,2.90000,2.40000  #  22
*vertex,0.00000,2.70000,2.40000  #  23
*vertex,3.40000,5.10000,2.40000  #  24
*vertex,3.40000,6.60000,2.40000  #  25
*vertex,3.20000,6.60000,2.40000  #  26
*vertex,2.50000,6.60000,2.40000  #  27
*vertex,0.00000,6.60000,2.40000  #  28
*vertex,0.00000,4.70000,2.40000  #  29
*vertex,4.10000,5.10000,2.40000  #  30
*vertex,4.10000,6.60000,2.40000  #  31
*vertex,5.10000,2.00000,2.40000  #  32
*vertex,6.00000,2.00000,2.40000  #  33
*vertex,7.10000,2.00000,2.40000  #  34
*vertex,8.10000,2.00000,2.40000  #  35
*vertex,8.10000,3.70000,2.40000  #  36
*vertex,8.10000,6.60000,2.40000  #  37
*vertex,6.80000,6.60000,2.40000  #  38
*vertex,5.40000,6.60000,2.40000  #  39
*vertex,3.30000,0.00000,2.40000  #  40
*vertex,4.00000,0.00000,2.40000  #  41
*vertex,5.10000,0.00000,2.40000  #  42
*vertex,6.20000,0.00000,2.40000  #  43
*vertex,7.00000,0.00000,2.40000  #  44
*vertex,8.10000,0.00000,2.40000  #  45
*vertex,8.10000,3.30000,4.40000  #  46
*vertex,0.00000,3.30000,4.40000  #  47
# 
# tag, number of vertices followed by list of associated vert
*edges,9,2,1,9,8,7,6,5,4,3  #  1
*edges,13,7,8,19,18,17,16,15,14,13,12,11,10,6  #  2
*edges,10,8,9,1,23,22,21,20,17,18,19  #  3
*edges,11,17,20,29,28,27,26,25,24,14,15,16  #  4
*edges,6,13,14,24,25,31,30  #  5
*edges,14,32,10,11,12,13,30,31,39,38,37,36,35,34,33  #  6
*edges,7,40,5,6,10,32,42,41  #  7
*edges,8,43,42,32,33,34,35,45,44  #  8
*edges,9,1,2,47,28,29,20,21,22,23  #  9
*edges,5,36,37,46,45,35  # 10
*edges,12,44,45,46,47,2,3,4,5,40,41,42,43  # 11
*edges,10,27,28,47,46,37,38,39,31,25,26  # 12
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,br2_ceiling,FLOR,-,-,-,Floortk_rf,OPAQUE,ANOTHER,01,10  #   1 ||< br2_ceiling:BR_two
*surf,hall_ceiling,FLOR,-,-,-,Floortk_rf,OPAQUE,ANOTHER,02,14  #   2 ||< hall_ceiling:hall
*surf,vest_ceiling,FLOR,-,-,-,Floortk_rf,OPAQUE,ANOTHER,03,11  #   3 ||< vest_ceiling:lobby_stor
*surf,br1_ceiling,FLOR,-,-,-,Floortk_rf,OPAQUE,ANOTHER,04,12  #   4 ||< br1_ceiling:BR_one
*surf,stor_ceil,FLOR,-,-,-,Floortk_rf,OPAQUE,ANOTHER,05,07  #   5 ||< stor_ceil:storage
*surf,liv_ceiling,FLOR,-,-,-,Floortk_rf,OPAQUE,ANOTHER,06,15  #   6 ||< liv_ceiling:livingdin
*surf,bath_ceil,FLOR,-,-,-,Floortk_rf,OPAQUE,ANOTHER,07,08  #   7 ||< bath_ceil:bath
*surf,kit_ceiling,FLOR,-,-,-,Floortk_rf,OPAQUE,ANOTHER,08,09  #   8 ||< kit_ceiling:kitchen
*surf,left_end,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,right_end,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,roof_front,SLOP,-,ROOF,PITCHED,Roof,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,roof_back,SLOP,-,ROOF,PITCHED,Roof,OPAQUE,EXTERIOR,0,0  #  12 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,53.46,0  # zone base
