*Geometry 1.1,GEN,Drumoss-war # tag version, format, zone name
*date Thu Aug 23 18:04:04 2007  # latest file modification 
Drumoss-war describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,28.77494,8.46459,0.00000  #   1
*vertex,31.70007,5.73629,0.00000  #   2
*vertex,31.70007,5.73629,2.60000  #   3
*vertex,30.23751,7.10044,3.21146  #   4
*vertex,28.77494,8.46459,2.60000  #   5
*vertex,32.51856,6.61383,0.00000  #   6
*vertex,29.59343,9.34213,0.00000  #   7
*vertex,29.59343,9.34213,2.60000  #   8
*vertex,32.51856,6.61383,2.60000  #   9
*vertex,34.41989,4.84044,0.00000  #  10
*vertex,43.28686,14.34711,-0.00001  #  11
*vertex,20.30000,18.01000,0.00000  #  12
*vertex,34.42000,4.84000,2.60000  #  13
*vertex,43.28700,14.34700,2.60000  #  14
*vertex,20.30000,18.01000,2.60000  #  15
*vertex,38.85341,9.59341,6.60000  #  16
*vertex,31.73365,8.70452,3.21146  #  17
*vertex,20.30000,35.78723,0.00000  #  18
*vertex,20.30000,35.78700,2.60000  #  19
*vertex,20.30000,26.89854,6.60000  #  20
# 
# tag, number of vertices followed by list of associated vert
*edges,4,10,11,14,13  #  1
*edges,4,18,12,15,19  #  2
*edges,4,11,18,19,14  #  3
*edges,4,6,10,13,9  #  4
*edges,4,2,6,9,3  #  5
*edges,8,11,10,6,2,1,7,12,18  #  6
*edges,5,1,2,3,4,5  #  7
*edges,4,12,7,8,15  #  8
*edges,4,8,5,4,17  #  9
*edges,4,3,9,17,4  # 10
*edges,4,7,1,5,8  # 11
*edges,7,9,13,16,20,15,8,17  # 12
*edges,4,14,19,20,16  # 13
*edges,3,13,14,16  # 14
*edges,3,19,15,20  # 15
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Wall-v-s2,VERT,-,-,-,Ward-wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,Wall-v-n3,VERT,-,-,-,Ward-wall,OPAQUE,ANOTHER,09,02  #   2 ||< Wall-v-n3:Torvean-war
*surf,Wall-v-n4,VERT,-,-,-,Ward-wall,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,Wall-v-s3,VERT,-,-,-,Ward-wall,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,Wall-v-s4,VERT,-,-,-,Ward-wall,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,Floor,FLOR,-,-,-,Ward-wall,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,Wall-v-s5,VERT,-,-,-,Ward-wall,OPAQUE,ANOTHER,08,04  #   7 ||< Fictive-sim:East_4
*surf,Wall-v-s6,VERT,-,-,-,Ward-wall,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,Roof-n1,SLOP,-,-,-,Ward-roof,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,Roof-s1,SLOP,-,-,-,Ward-roof,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,Wall-v-n1,VERT,-,-,-,Ward-wall,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,Roof-s2,SLOP,-,-,-,Ward-roof,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,Roof-n2,SLOP,-,-,-,Ward-roof,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,Wall-v-s1,VERT,-,-,-,Ward-wall,OPAQUE,EXTERIOR,0,0  #  14 ||< external
*surf,Wall-v-n2,VERT,-,-,-,Ward-wall,OPAQUE,ANOTHER,09,15  #  15 ||< Wall-v-n2:Torvean-war
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,   334.62 0  # zone base list
