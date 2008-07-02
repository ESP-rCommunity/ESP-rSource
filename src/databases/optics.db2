# optical properties db for glazing, TMC files and casual gain control
# the first line of each item is column sensitive and holds
# an identifier (12 char) followed by a description
# the 2nd line holds:
# a) the number of default and tmc layers (always the same)
# b) visable trans 
# c) solar reflectance (outside)
# d) overall solar absorbed
# e) U value (for reporting purposes only)
# the 3rd line holds:
# a) direct solar tran at 0deg 40deg 55deg 70deg 80deg from normal
# b) total heat gain at the same angles (for reporting purposes only)
# then for each layer there is a line containing
# a) refractive index
# b) solar absorption at 0deg 40deg 55deg 70deg 80deg from normal
# 
# Glazed info follows:
# 12 char id |  description       | thick | blind
SC_8985_04nb  :Clear glass 89/85,     4mm, no blind
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   1  0.890  0.070  0.110  5.400
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.819 0.802 0.761 0.621 0.376 0.860 0.850 0.800 0.650 0.420
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.106 0.116 0.124 0.129 0.125
# 12 char id |  description       | thick | blind
SC_8985_04ib  :Clear glass 89/85,     4mm, int blin
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   1  0.890  0.390  0.520  5.400
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.087 0.067 0.053 0.035 0.018 0.348 0.332 0.315 0.272 0.195
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.869 0.883 0.873 0.789 0.590
# 12 char id |  description       | thick | blind
SCF8783_06nb  :Clear float 87/83,     6mm, no blind
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   1  0.870  0.070  0.150  5.400
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.779 0.759 0.717 0.581 0.348 0.824 0.808 0.769 0.635 0.398
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.149 0.163 0.173 0.179 0.169
# 12 char id |  description       | thick | blind
SCF8783_06ib  :Clear float 87/83,     6mm, int blin
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   1  0.890  0.390  0.520  5.400
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.087 0.068 0.053 0.035 0.018 0.348 0.333 0.315 0.272 0.195
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.869 0.883 0.873 0.789 0.590
# 12 char id |  description       | thick | blind
SCF8477_10nb  :Clear float 84/77,    10mm, no blind
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   1  0.840  0.070  0.230  5.300
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.699 0.672 0.629 0.503 0.294 0.769 0.749 0.710 0.585 0.368
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.235 0.255 0.269 0.273 0.247
# 12 char id |  description       | thick | blind
SCF8477_10ib  :Clear float 84/77,    10mm, int blin
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   1  0.890  0.320  0.600  5.300
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.087 0.068 0.053 0.035 0.018 0.348 0.333 0.315 0.272 0.195
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.869 0.883 0.873 0.789 0.590
# 12 char id |  description       | thick | blind
SCF8285_12nb  :Clear float 82/75,    12mm, no blind
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   1  0.820  0.060  0.270  5.200
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.669 0.641 0.597 0.475 0.276 0.749 0.728 0.688 0.567 0.358
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.267 0.289 0.304 0.306 0.273
# 12 char id |  description       | thick | blind
SCF8285_12ib  :Clear float 82/85,    12mm, int blin
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   1  0.890  0.290  0.630  5.300
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.079 0.060 0.047 0.031 0.015 0.342 0.327 0.311 0.269 0.193
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.869 0.883 0.873 0.789 0.590
# 12 char id |  description       | thick | blind
SSF5165_06nb  :Spectrafloat 51/66,    6mm, no blind
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   1  0.510  0.100  0.360  5.400
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.537 0.503 0.460 0.357 0.202 0.659 0.634 0.595 0.490 0.314
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.406 0.435 0.452 0.443 0.374
# 12 char id |  description       | thick | blind
SSF5165_06ib  :Spectrafloat 51/66,    6mm, int blin
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   1  0.510  0.250  0.690  5.400
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.087 0.068 0.053 0.035 0.018 0.348 0.333 0.315 0.272 0.195
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.869 0.883 0.873 0.789 0.590
# 12 char id |  description       | thick | blind
SRF3352_06nb  :Reflectafloat 33/52,   6mm, no blind
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   1  0.330  0.280  0.290  5.400
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.428 0.391 0.351 0.266 0.148 0.584 0.557 0.521 0.430 0.281
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.520 0.551 0.567 0.544 0.443
# 12 char id |  description       | thick | blind
SRF3352_06ib  :Reflectafloat 33/52,   6mm, int blin
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   1  0.330  0.380  0.550  5.400
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.087 0.068 0.053 0.035 0.018 0.348 0.333 0.315 0.272 0.195
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.869 0.883 0.873 0.789 0.590
# 12 char id |  description       | thick | blind
SAZ5060_06nb  :Antisun bronze 50/60,  6mm, no blind
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   1  0.500  0.050  0.490  5.400
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.457 0.421 0.379 0.290 0.161 0.604 0.577 0.541 0.446 0.289
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.489 0.521 0.537 0.518 0.426
# 12 char id |  description       | thick | blind
SAZ3348_10nb  :Antisun bronze 33/48, 10mm, no blind
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   1  0.330  0.040  0.670  5.300
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.282 0.247 0.213 0.156 0.084 0.483 0.457 0.426 0.355 0.240
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.671 0.701 0.710 0.664 0.518
# 12 char id |  description       | thick | blind
SC_fictit     :Fictitious 99/99,      2mm, no blind
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   1  0.990  0.001  0.001  9.000
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.999 0.999 0.999 0.999 0.999 0.999 0.999 0.999 0.999 0.999
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.001 0.001 0.001 0.001 0.001
# 12 char id |  description       | thick | blind
DC_8074_04nb  :Clear glass 80/75,     4mm, no blind
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   3  0.800  0.120  0.210  2.800
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.676 0.651 0.604 0.441 0.201 0.749 0.731 0.687 0.520 0.265
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.113 0.124 0.134 0.148 0.154
  1.000 0.001 0.002 0.003 0.004 0.005
  1.520 0.087 0.094 0.097 0.087 0.062
# 12 char id |  description       | thick | blind
DCF7671_06nb  :Clear float 76/71,     6mm, no blind
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   3  0.760  0.110  0.280  2.800
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.611 0.583 0.534 0.384 0.170 0.711 0.690 0.645 0.487 0.250
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.157 0.172 0.185 0.201 0.202
  1.000 0.001 0.002 0.003 0.004 0.005
  1.520 0.117 0.124 0.127 0.112 0.077
# 12 char id |  description       | thick | blind
DCF7671_06ib  :Clear float 76/71,     6mm, int blnd
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   3  0.070  0.390  0.540  2.800
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.060 0.045 0.033 0.019 0.006 0.196 0.182 0.168 0.141 0.097
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.893 0.892 0.890 0.803 0.600
  1.000 0.001 0.002 0.003 0.004 0.005
  1.520 0.004 0.003 0.002 0.002 0.001
# 12 char id |  description       | thick | blind
DCF7365_10nb  :Clear float 73/66,    10mm, no blind
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   3  0.730  0.100  0.350  2.700
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.560 0.533 0.488 0.352 0.159 0.657 0.635 0.593 0.449 0.232
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.239 0.258 0.271 0.280 0.258
  1.000 0.001 0.002 0.003 0.004 0.005
  1.520 0.094 0.098 0.099 0.084 0.054
# 12 char id |  description       | thick | blind
DSF4554_06nb  :Spectrafloat 45/54,    6mm, no blind
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   3  0.450  0.100  0.350  2.700
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.421 0.395 0.359 0.258 0.112 0.532 0.511 0.478 0.369 0.197
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.375 0.394 0.405 0.403 0.357
  1.000 0.001 0.002 0.003 0.004 0.005
  1.520 0.085 0.088 0.089 0.077 0.049
# 12 char id |  description       | thick | blind
DAG6349_06nb  :Antisun green 63/49,   6mm, no blind
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   3  0.630  0.060  0.580  2.800
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.381 0.353 0.316 0.221 0.096 0.487 0.462 0.425 0.323 0.176
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.502 0.527 0.539 0.522 0.432
  1.000 0.001 0.002 0.003 0.004 0.005
  1.520 0.047 0.046 0.044 0.036 0.023
# 12 char id |  description       | thick | blind
DAZ5060_06nb  :Antisun bronze 50/60,  6mm, no blind
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   3  0.440  0.060  0.580  2.800
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.363 0.328 0.287 0.194 0.079 0.482 0.452 0.412 0.310 0.169
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.505 0.537 0.556 0.550 0.464
  1.000 0.001 0.002 0.003 0.004 0.005
  1.520 0.066 0.065 0.064 0.052 0.031
# 12 char id |  description       | thick | blind
DAG4260_06nb  :Antisun grey 42/60,    6mm, no blind
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   3  0.370  0.060  0.610  2.800
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.328 0.294 0.255 0.171 0.069 0.451 0.421 0.383 0.290 0.160
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.545 0.577 0.594 0.581 0.483
  1.000 0.001 0.002 0.003 0.004 0.005
  1.520 0.063 0.062 0.061 0.049 0.029
# 12 char id |  description       | thick | blind
DAZ3348_10nb  :Antisun bronze 33/48, 10mm, no blind
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   3  0.290  0.050  0.720  2.700
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.228 0.196 0.164 0.105 0.041 0.357 0.328 0.295 0.226 0.133
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.684 0.716 0.728 0.692 0.549
  1.000 0.001 0.002 0.003 0.004 0.005
  1.520 0.040 0.038 0.035 0.027 0.015
# 12 char id |  description       | thick | blind
DAG2549_10nb  :Antisun grey 25/49,   10mm, no blind
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   3  0.220  0.040  0.760  2.700
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.192 0.161 0.132 0.083 0.032 0.326 0.297 0.268 0.207 0.126
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.726 0.755 0.765 0.720 0.563
  1.000 0.001 0.002 0.003 0.004 0.005
  1.510 0.038 0.035 0.032 0.024 0.014
# 12 char id |  description       | thick | blind
DCF7671_6omb  :Clear float 76/71, 6mm open mid blnd
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   5  0.760  0.110  0.280  2.800
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.604 0.578 0.521 0.384 0.170 0.711 0.690 0.645 0.487 0.250
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.157 0.172 0.185 0.201 0.202
  1.000 0.001 0.002 0.003 0.004 0.005
  1.000 0.110 0.108 0.116 0.121 0.097
  1.000 0.001 0.002 0.003 0.004 0.005
  1.520 0.117 0.124 0.127 0.112 0.077
# 12 char id |  description       | thick | blind
DCF7671_06mb  :Clear float 76/71,     6mm, mid blnd
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   5  0.090  0.390  0.380  2.400
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.071 0.070 0.073 0.070 0.038 0.233 0.232 0.232 0.218 0.147
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.216 0.234 0.242 0.238 0.209
  1.000 0.001 0.002 0.003 0.004 0.005
  1.000 0.316 0.309 0.395 0.261 0.181
  1.000 0.001 0.002 0.003 0.004 0.005
  1.520 0.014 0.016 0.018 0.022 0.019
# 12 char id |  description       | thick | blind
DCF7671_06tb  :Clear float 76/71,  6mm, trnsul blnd
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   5  0.120  0.390  0.200  2.400
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.128 0.125 0.125 0.115 0.062 0.351 0.346 0.334 0.277 0.156
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.201 0.216 0.226 0.232 0.218
  1.000 0.001 0.002 0.003 0.004 0.005
  1.520 0.172 0.181 0.178 0.151 0.097
  1.000 0.001 0.002 0.003 0.004 0.005
  1.000 0.192 0.183 0.164 0.116 0.052
# 12 char id |  description       | thick | blind
DCF7447_06nb  :6_12_6 CF low_e(3) + Krypton
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   3  0.743  0.174  0.000  5.400
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.471 0.440 0.387 0.258 0.114 0.634 0.613 0.559 0.420 0.217
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.175 0.190 0.205 0.219 0.214
  1.000 0.001 0.001 0.001 0.001 0.001
  1.520 0.172 0.181 0.178 0.151 0.097
# 12 char id |  description       | thick | blind
TCF6330_06nb  :6_12_6_12_6 2low_e(2,5) + Krypton
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   5  0.630  0.235  0.000  5.400
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.294 0.264 0.218 0.122 0.041 0.454 0.429 0.378 0.257 0.123
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.250 0.272 0.289 0.324 0.278
  1.000 0.001 0.001 0.001 0.001 0.001
  1.520 0.109 0.114 0.115 0.103 0.077
  1.000 0.001 0.001 0.001 0.001 0.001
  1.520 0.113 0.115 0.108 0.083 0.039
# 12 char id |  description       | thick | blind
DCF6533_06n   :skn-172 St-Roch 1low_e(2)
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   3  0.655  0.263  0.000  5.400
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.332 0.310 0.273 0.184 0.083 0.406 0.388 0.351 0.254 0.134
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.340 0.354 0.361 0.367 0.294
  1.000 0.001 0.001 0.001 0.001 0.001
  1.520 0.066 0.069 0.069 0.059 0.042
# 12 char id |  description       | thick | blind
DCF5246_06n   :Glaverbel_mgs
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   3  0.518  0.315  0.720  5.400
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.280 0.266 0.248 0.198 0.100 0.419 0.414 0.403 0.352 0.218
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.258 0.263 0.266 0.267 0.250
  1.000 0.001 0.001 0.001 0.001 0.001
  1.520 0.147 0.157 0.164 0.163 0.124
# 12 char id |  description       | thick | blind
SCF7747_06n   :Single_low_e
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   1  0.770  0.300  0.000  5.400
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.430 0.413 0.382 0.298 0.179 0.467 0.452 0.422 0.338 0.210
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.270 0.284 0.289 0.292 0.231
# 12 char id |  description       | thick | blind
DCF6741_06n   :dbl_air_low
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   3  0.673  0.314  0.000  5.400
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.346 0.324 0.287 0.197 0.092 0.418 0.400 0.363 0.268 0.145
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.275 0.290 0.297 0.308 0.251
  1.000 0.001 0.001 0.001 0.001 0.001
  1.520 0.066 0.069 0.069 0.061 0.044
# 12 char id |  description       | thick | blind
dispGlz_not   :display glazing for notional UK buil
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   1  0.690  0.070  0.110  5.400
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.588 0.561 0.517 0.412 0.238 0.200 0.200 0.200 0.200 0.200
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.337 0.358 0.370 0.365 0.315
# 12 char id |  description       | thick | blind
Glaz_notiona  :Notional UK glazing optical properts
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   3  0.690  0.070  0.110  5.400
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.557 0.536 0.505 0.341 0.164 0.200 0.200 0.200 0.200 0.200
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.119 0.130 0.142 0.158 0.162
  1.000 0.001 0.001 0.001 0.001 0.001
  1.520 0.177 0.175 0.165 0.132 0.074
