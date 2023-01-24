*Geometry 1.1,GEN,main_second # tag version, format, zone name
*date Sat Jan  4 17:31:21 2014  # latest file modification 
simplified elevation of main and second floors
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,2.00000,0.00000,2.60000  #   1
*vertex,12.00000,0.00000,2.60000  #   2
*vertex,2.00000,12.20000,2.60000  #   3
*vertex,2.00000,0.00000,8.08500  #   4
*vertex,12.00000,0.00000,8.08500  #   5
*vertex,12.00000,12.20000,8.08500  #   6
*vertex,2.00000,12.20000,8.08500  #   7
*vertex,3.97000,12.20000,3.10000  #   8
*vertex,3.43000,12.20000,3.10000  #   9
*vertex,3.43000,12.20000,4.94000  #  10
*vertex,3.97000,12.20000,4.94000  #  11
*vertex,3.04000,12.20000,3.10000  #  12
*vertex,2.50000,12.20000,3.10000  #  13
*vertex,2.50000,12.20000,4.94000  #  14
*vertex,3.04000,12.20000,4.94000  #  15
*vertex,12.00000,1.00000,3.10000  #  16
*vertex,12.00000,3.43000,3.10000  #  17
*vertex,12.00000,3.43000,4.94000  #  18
*vertex,12.00000,1.00000,4.94000  #  19
*vertex,12.00000,1.00000,6.00000  #  20
*vertex,12.00000,3.43000,6.00000  #  21
*vertex,12.00000,3.43000,7.64000  #  22
*vertex,12.00000,1.00000,7.64000  #  23
*vertex,12.00000,8.64000,6.39000  #  24
*vertex,12.00000,10.86000,6.39000  #  25
*vertex,12.00000,10.86000,7.64000  #  26
*vertex,12.00000,8.64000,7.64000  #  27
*vertex,2.00000,2.76000,3.10000  #  28
*vertex,2.00000,1.13000,3.10000  #  29
*vertex,2.00000,1.13000,4.94000  #  30
*vertex,2.00000,2.76000,4.94000  #  31
*vertex,2.00000,2.76000,6.40000  #  32
*vertex,2.00000,0.93000,6.40000  #  33
*vertex,2.00000,0.93000,7.64000  #  34
*vertex,2.00000,2.76000,7.64000  #  35
*vertex,6.00000,0.00000,5.60000  #  36
*vertex,7.25000,0.00000,5.60000  #  37
*vertex,7.25000,0.00000,6.85000  #  38
*vertex,6.00000,0.00000,6.85000  #  39
*vertex,2.00000,8.00000,6.40000  #  40
*vertex,2.00000,6.14000,6.40000  #  41
*vertex,2.00000,6.14000,7.64000  #  42
*vertex,2.00000,8.00000,7.64000  #  43
*vertex,4.97000,12.20000,2.60000  #  44
*vertex,4.97000,12.20000,5.62000  #  45
*vertex,12.00000,12.20000,5.62000  #  46
*vertex,12.00000,6.79000,2.60000  #  47
*vertex,4.97000,6.79000,2.60000  #  48
*vertex,4.97000,6.79000,5.62000  #  49
*vertex,12.00000,6.79000,5.62000  #  50
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,5,4,39,38,37,36,39,4  #  1
*edges,24,46,25,24,27,26,25,46,6,5,23,22,21,20,23,5,2,16,19,18,17,16,2,47,50  #  2
*edges,18,7,6,46,45,44,8,11,10,9,8,44,3,13,12,15,14,13,3  #  3
*edges,22,3,28,31,30,29,28,3,1,33,32,35,34,33,1,4,7,40,43,42,41,40,7  #  4
*edges,4,4,5,6,7  #  5
*edges,6,1,3,44,48,47,2  #  6
*edges,4,8,9,10,11  #  7
*edges,4,12,13,14,15  #  8
*edges,4,16,17,18,19  #  9
*edges,4,20,21,22,23  # 10
*edges,4,24,25,26,27  # 11
*edges,4,28,29,30,31  # 12
*edges,4,32,33,34,35  # 13
*edges,4,36,37,38,39  # 14
*edges,4,40,41,42,43  # 15
*edges,4,47,48,49,50  # 16
*edges,4,44,45,49,48  # 17
*edges,4,45,46,50,49  # 18
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Surf-1,VERT,-,-,-,ccht_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,Surf-2,VERT,-,-,-,ccht_wall,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,Surf-3,VERT,-,-,-,ccht_wall,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,Surf-4,VERT,-,-,-,ccht_wall,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,ceiling,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,03,10  #   5 ||< ceiling:roof
*surf,floor,FLOR,-,-,-,floors,OPAQUE,ANOTHER,01,05  #   6 ||< to_upstairs:basement
*surf,W5,VERT,-,-,-,ccht_window,ccht-win,EXTERIOR,0,0  #   7 ||< external
*surf,W6,VERT,-,-,-,ccht_window,ccht-win,EXTERIOR,0,0  #   8 ||< external
*surf,W1,VERT,-,-,-,ccht_window,ccht-win,EXTERIOR,0,0  #   9 ||< external
*surf,W2,VERT,-,-,-,ccht_window,ccht-win,EXTERIOR,0,0  #  10 ||< external
*surf,W4,VERT,-,-,-,ccht_window,ccht-win,EXTERIOR,0,0  #  11 ||< external
*surf,W9,VERT,-,-,-,ccht_window,ccht-win,EXTERIOR,0,0  #  12 ||< external
*surf,W12,VERT,-,-,-,ccht_window,ccht-win,EXTERIOR,0,0  #  13 ||< external
*surf,W13,VERT,-,-,-,ccht_window,ccht-win,EXTERIOR,0,0  #  14 ||< external
*surf,W10_11,VERT,-,-,-,ccht_window,ccht-win,EXTERIOR,0,0  #  15 ||< external
*surf,garage_in,VERT,-,-,-,ccht_wall,OPAQUE,ANOTHER,04,01  #  16 ||< main_share:garage
*surf,garage_back,VERT,-,-,-,ccht_wall,OPAQUE,ANOTHER,04,04  #  17 ||< garage_back:garage
*surf,garage_ceil,FLOR,-,-,-,exp_flr,OPAQUE,ANOTHER,04,05  #  18 ||< garage_ceiln:garage
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    83.97 0  # zone base list
