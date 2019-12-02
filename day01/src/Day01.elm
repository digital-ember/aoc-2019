module Day01 exposing(netFuel, totalFuel)

import Maybe exposing(Maybe)

type alias Mass = Int

type alias Fuel = Int

type alias ModuleMasses = List Mass

{-- part 1 --}
netFuel : Fuel
netFuel =
  List.foldl massToNetFuel 0 moduleMasses

massToNetFuel : Mass -> Mass -> Fuel
massToNetFuel mass seed =
  seed + (massToFuel mass)

{-- part 2 --}
totalFuel : Fuel
totalFuel =
  List.foldl massToTotalFuel 0 moduleMasses

massToTotalFuel : Mass -> Mass -> Fuel
massToTotalFuel mass seed =
  let
    fuel = massToFuel mass
  in
    if fuel < 1 then
      0 
    else
      seed + fuel + (massToTotalFuel fuel 0)

{-- common --}
massToFuel : Mass -> Fuel
massToFuel mass =
  (mass // 3) - 2

moduleMasses : ModuleMasses
moduleMasses =
  input 
    |> String.lines
    |> List.map String.toInt 
    |> List.map (Maybe.withDefault 0)

input = 
  """66016
85415
51706
96238
62503
61186
119728
69237
54386
137211
65936
129665
104711
121892
54525
124849
69572
105755
101883
62077
50281
50495
95260
132134
70793
73917
89493
146104
50175
147230
50620
116767
53185
101222
66030
143778
128607
65723
67468
130416
140049
106694
59226
55728
70444
142973
122909
100359
135329
112347
91435
117106
74651
58224
77738
102308
144916
144013
82723
54674
97031
94405
137411
61090
77394
59677
96188
70689
115428
100885
52367
69072
77555
77178
99734
143520
51012
59534
90749
100616
129295
102922
115119
112385
70400
137409
133502
92323
88413
121153
87291
144171
75094
68871
74335
53503
70425
89354
134887
63969"""