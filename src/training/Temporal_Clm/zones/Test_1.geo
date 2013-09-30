*Geometry 1.1,GEN,Test_1 # tag version, format, zone name
*date Thu Apr  7 02:11:37 2011  # latest file modification 
Test_1 describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,5.50000,0.00000,0.00000  #   2
*vertex,5.50000,3.60000,0.00000  #   3
*vertex,0.00000,3.60000,0.00000  #   4
*vertex,0.00000,0.00000,2.80000  #   5
*vertex,5.50000,0.00000,2.80000  #   6
*vertex,5.50000,3.60000,2.80000  #   7
*vertex,0.00000,3.60000,2.80000  #   8
*vertex,0.00000,3.20000,0.10000  #   9
*vertex,0.00000,0.40000,0.10000  #  10
*vertex,0.00000,0.40000,2.60000  #  11
*vertex,0.00000,3.20000,2.60000  #  12
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,10,4,1,5,8,4,9,12,11,10,9  #  4
*edges,4,5,6,7,8  #  5
*edges,4,1,4,3,2  #  6
*edges,4,9,10,11,12  #  7
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,South,VERT,-,-,-,T_n.2_09em0a,OPAQUE,ADIABATIC,0,0  #   1 ||< adiabatic
*surf,East,VERT,-,-,-,T_n.2_09em0a,OPAQUE,ADIABATIC,0,0  #   2 ||< adiabatic
*surf,North,VERT,-,-,-,T_n.2_09em0a,OPAQUE,ADIABATIC,0,0  #   3 ||< adiabatic
*surf,West,VERT,-,-,-,Typ_n.1_09em,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,Ceiling,CEIL,-,-,-,T_n.4_09em0a,OPAQUE,ADIABATIC,0,0  #   5 ||< adiabatic
*surf,Floor,FLOR,-,-,-,T_n4fl09em0a,OPAQUE,ADIABATIC,0,0  #   6 ||< adiabatic
*surf,window,VERT,-,-,-,WISdblshd09e,Gunt2_shadd,EXTERIOR,0,0  #   7 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    19.80 0  # zone base list
