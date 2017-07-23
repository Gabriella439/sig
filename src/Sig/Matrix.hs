{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Sig.Matrix where

import Data.Binary (Binary(..))
import Dhall (Interpret)
import GHC.Generics (Generic)
import Sig.Transition (Transition(..))

data Matrix = Matrix
    { onByte000 :: !Transition
    , onByte001 :: !Transition
    , onByte002 :: !Transition
    , onByte003 :: !Transition
    , onByte004 :: !Transition
    , onByte005 :: !Transition
    , onByte006 :: !Transition
    , onByte007 :: !Transition
    , onByte008 :: !Transition
    , onByte009 :: !Transition
    , onByte010 :: !Transition
    , onByte011 :: !Transition
    , onByte012 :: !Transition
    , onByte013 :: !Transition
    , onByte014 :: !Transition
    , onByte015 :: !Transition
    , onByte016 :: !Transition
    , onByte017 :: !Transition
    , onByte018 :: !Transition
    , onByte019 :: !Transition
    , onByte020 :: !Transition
    , onByte021 :: !Transition
    , onByte022 :: !Transition
    , onByte023 :: !Transition
    , onByte024 :: !Transition
    , onByte025 :: !Transition
    , onByte026 :: !Transition
    , onByte027 :: !Transition
    , onByte028 :: !Transition
    , onByte029 :: !Transition
    , onByte030 :: !Transition
    , onByte031 :: !Transition
    , onByte032 :: !Transition
    , onByte033 :: !Transition
    , onByte034 :: !Transition
    , onByte035 :: !Transition
    , onByte036 :: !Transition
    , onByte037 :: !Transition
    , onByte038 :: !Transition
    , onByte039 :: !Transition
    , onByte040 :: !Transition
    , onByte041 :: !Transition
    , onByte042 :: !Transition
    , onByte043 :: !Transition
    , onByte044 :: !Transition
    , onByte045 :: !Transition
    , onByte046 :: !Transition
    , onByte047 :: !Transition
    , onByte048 :: !Transition
    , onByte049 :: !Transition
    , onByte050 :: !Transition
    , onByte051 :: !Transition
    , onByte052 :: !Transition
    , onByte053 :: !Transition
    , onByte054 :: !Transition
    , onByte055 :: !Transition
    , onByte056 :: !Transition
    , onByte057 :: !Transition
    , onByte058 :: !Transition
    , onByte059 :: !Transition
    , onByte060 :: !Transition
    , onByte061 :: !Transition
    , onByte062 :: !Transition
    , onByte063 :: !Transition
    , onByte064 :: !Transition
    , onByte065 :: !Transition
    , onByte066 :: !Transition
    , onByte067 :: !Transition
    , onByte068 :: !Transition
    , onByte069 :: !Transition
    , onByte070 :: !Transition
    , onByte071 :: !Transition
    , onByte072 :: !Transition
    , onByte073 :: !Transition
    , onByte074 :: !Transition
    , onByte075 :: !Transition
    , onByte076 :: !Transition
    , onByte077 :: !Transition
    , onByte078 :: !Transition
    , onByte079 :: !Transition
    , onByte080 :: !Transition
    , onByte081 :: !Transition
    , onByte082 :: !Transition
    , onByte083 :: !Transition
    , onByte084 :: !Transition
    , onByte085 :: !Transition
    , onByte086 :: !Transition
    , onByte087 :: !Transition
    , onByte088 :: !Transition
    , onByte089 :: !Transition
    , onByte090 :: !Transition
    , onByte091 :: !Transition
    , onByte092 :: !Transition
    , onByte093 :: !Transition
    , onByte094 :: !Transition
    , onByte095 :: !Transition
    , onByte096 :: !Transition
    , onByte097 :: !Transition
    , onByte098 :: !Transition
    , onByte099 :: !Transition
    , onByte100 :: !Transition
    , onByte101 :: !Transition
    , onByte102 :: !Transition
    , onByte103 :: !Transition
    , onByte104 :: !Transition
    , onByte105 :: !Transition
    , onByte106 :: !Transition
    , onByte107 :: !Transition
    , onByte108 :: !Transition
    , onByte109 :: !Transition
    , onByte110 :: !Transition
    , onByte111 :: !Transition
    , onByte112 :: !Transition
    , onByte113 :: !Transition
    , onByte114 :: !Transition
    , onByte115 :: !Transition
    , onByte116 :: !Transition
    , onByte117 :: !Transition
    , onByte118 :: !Transition
    , onByte119 :: !Transition
    , onByte120 :: !Transition
    , onByte121 :: !Transition
    , onByte122 :: !Transition
    , onByte123 :: !Transition
    , onByte124 :: !Transition
    , onByte125 :: !Transition
    , onByte126 :: !Transition
    , onByte127 :: !Transition
    , onByte128 :: !Transition
    , onByte129 :: !Transition
    , onByte130 :: !Transition
    , onByte131 :: !Transition
    , onByte132 :: !Transition
    , onByte133 :: !Transition
    , onByte134 :: !Transition
    , onByte135 :: !Transition
    , onByte136 :: !Transition
    , onByte137 :: !Transition
    , onByte138 :: !Transition
    , onByte139 :: !Transition
    , onByte140 :: !Transition
    , onByte141 :: !Transition
    , onByte142 :: !Transition
    , onByte143 :: !Transition
    , onByte144 :: !Transition
    , onByte145 :: !Transition
    , onByte146 :: !Transition
    , onByte147 :: !Transition
    , onByte148 :: !Transition
    , onByte149 :: !Transition
    , onByte150 :: !Transition
    , onByte151 :: !Transition
    , onByte152 :: !Transition
    , onByte153 :: !Transition
    , onByte154 :: !Transition
    , onByte155 :: !Transition
    , onByte156 :: !Transition
    , onByte157 :: !Transition
    , onByte158 :: !Transition
    , onByte159 :: !Transition
    , onByte160 :: !Transition
    , onByte161 :: !Transition
    , onByte162 :: !Transition
    , onByte163 :: !Transition
    , onByte164 :: !Transition
    , onByte165 :: !Transition
    , onByte166 :: !Transition
    , onByte167 :: !Transition
    , onByte168 :: !Transition
    , onByte169 :: !Transition
    , onByte170 :: !Transition
    , onByte171 :: !Transition
    , onByte172 :: !Transition
    , onByte173 :: !Transition
    , onByte174 :: !Transition
    , onByte175 :: !Transition
    , onByte176 :: !Transition
    , onByte177 :: !Transition
    , onByte178 :: !Transition
    , onByte179 :: !Transition
    , onByte180 :: !Transition
    , onByte181 :: !Transition
    , onByte182 :: !Transition
    , onByte183 :: !Transition
    , onByte184 :: !Transition
    , onByte185 :: !Transition
    , onByte186 :: !Transition
    , onByte187 :: !Transition
    , onByte188 :: !Transition
    , onByte189 :: !Transition
    , onByte190 :: !Transition
    , onByte191 :: !Transition
    , onByte192 :: !Transition
    , onByte193 :: !Transition
    , onByte194 :: !Transition
    , onByte195 :: !Transition
    , onByte196 :: !Transition
    , onByte197 :: !Transition
    , onByte198 :: !Transition
    , onByte199 :: !Transition
    , onByte200 :: !Transition
    , onByte201 :: !Transition
    , onByte202 :: !Transition
    , onByte203 :: !Transition
    , onByte204 :: !Transition
    , onByte205 :: !Transition
    , onByte206 :: !Transition
    , onByte207 :: !Transition
    , onByte208 :: !Transition
    , onByte209 :: !Transition
    , onByte210 :: !Transition
    , onByte211 :: !Transition
    , onByte212 :: !Transition
    , onByte213 :: !Transition
    , onByte214 :: !Transition
    , onByte215 :: !Transition
    , onByte216 :: !Transition
    , onByte217 :: !Transition
    , onByte218 :: !Transition
    , onByte219 :: !Transition
    , onByte220 :: !Transition
    , onByte221 :: !Transition
    , onByte222 :: !Transition
    , onByte223 :: !Transition
    , onByte224 :: !Transition
    , onByte225 :: !Transition
    , onByte226 :: !Transition
    , onByte227 :: !Transition
    , onByte228 :: !Transition
    , onByte229 :: !Transition
    , onByte230 :: !Transition
    , onByte231 :: !Transition
    , onByte232 :: !Transition
    , onByte233 :: !Transition
    , onByte234 :: !Transition
    , onByte235 :: !Transition
    , onByte236 :: !Transition
    , onByte237 :: !Transition
    , onByte238 :: !Transition
    , onByte239 :: !Transition
    , onByte240 :: !Transition
    , onByte241 :: !Transition
    , onByte242 :: !Transition
    , onByte243 :: !Transition
    , onByte244 :: !Transition
    , onByte245 :: !Transition
    , onByte246 :: !Transition
    , onByte247 :: !Transition
    , onByte248 :: !Transition
    , onByte249 :: !Transition
    , onByte250 :: !Transition
    , onByte251 :: !Transition
    , onByte252 :: !Transition
    , onByte253 :: !Transition
    , onByte254 :: !Transition
    , onByte255 :: !Transition
    } deriving (Generic, Interpret, Show)

instance Binary Matrix where
    put (Matrix {..}) = do
        put onByte000
        put onByte001
        put onByte002
        put onByte003
        put onByte004
        put onByte005
        put onByte006
        put onByte007
        put onByte008
        put onByte009
        put onByte010
        put onByte011
        put onByte012
        put onByte013
        put onByte014
        put onByte015
        put onByte016
        put onByte017
        put onByte018
        put onByte019
        put onByte020
        put onByte021
        put onByte022
        put onByte023
        put onByte024
        put onByte025
        put onByte026
        put onByte027
        put onByte028
        put onByte029
        put onByte030
        put onByte031
        put onByte032
        put onByte033
        put onByte034
        put onByte035
        put onByte036
        put onByte037
        put onByte038
        put onByte039
        put onByte040
        put onByte041
        put onByte042
        put onByte043
        put onByte044
        put onByte045
        put onByte046
        put onByte047
        put onByte048
        put onByte049
        put onByte050
        put onByte051
        put onByte052
        put onByte053
        put onByte054
        put onByte055
        put onByte056
        put onByte057
        put onByte058
        put onByte059
        put onByte060
        put onByte061
        put onByte062
        put onByte063
        put onByte064
        put onByte065
        put onByte066
        put onByte067
        put onByte068
        put onByte069
        put onByte070
        put onByte071
        put onByte072
        put onByte073
        put onByte074
        put onByte075
        put onByte076
        put onByte077
        put onByte078
        put onByte079
        put onByte080
        put onByte081
        put onByte082
        put onByte083
        put onByte084
        put onByte085
        put onByte086
        put onByte087
        put onByte088
        put onByte089
        put onByte090
        put onByte091
        put onByte092
        put onByte093
        put onByte094
        put onByte095
        put onByte096
        put onByte097
        put onByte098
        put onByte099
        put onByte100
        put onByte101
        put onByte102
        put onByte103
        put onByte104
        put onByte105
        put onByte106
        put onByte107
        put onByte108
        put onByte109
        put onByte110
        put onByte111
        put onByte112
        put onByte113
        put onByte114
        put onByte115
        put onByte116
        put onByte117
        put onByte118
        put onByte119
        put onByte120
        put onByte121
        put onByte122
        put onByte123
        put onByte124
        put onByte125
        put onByte126
        put onByte127
        put onByte128
        put onByte129
        put onByte130
        put onByte131
        put onByte132
        put onByte133
        put onByte134
        put onByte135
        put onByte136
        put onByte137
        put onByte138
        put onByte139
        put onByte140
        put onByte141
        put onByte142
        put onByte143
        put onByte144
        put onByte145
        put onByte146
        put onByte147
        put onByte148
        put onByte149
        put onByte150
        put onByte151
        put onByte152
        put onByte153
        put onByte154
        put onByte155
        put onByte156
        put onByte157
        put onByte158
        put onByte159
        put onByte160
        put onByte161
        put onByte162
        put onByte163
        put onByte164
        put onByte165
        put onByte166
        put onByte167
        put onByte168
        put onByte169
        put onByte170
        put onByte171
        put onByte172
        put onByte173
        put onByte174
        put onByte175
        put onByte176
        put onByte177
        put onByte178
        put onByte179
        put onByte180
        put onByte181
        put onByte182
        put onByte183
        put onByte184
        put onByte185
        put onByte186
        put onByte187
        put onByte188
        put onByte189
        put onByte190
        put onByte191
        put onByte192
        put onByte193
        put onByte194
        put onByte195
        put onByte196
        put onByte197
        put onByte198
        put onByte199
        put onByte200
        put onByte201
        put onByte202
        put onByte203
        put onByte204
        put onByte205
        put onByte206
        put onByte207
        put onByte208
        put onByte209
        put onByte210
        put onByte211
        put onByte212
        put onByte213
        put onByte214
        put onByte215
        put onByte216
        put onByte217
        put onByte218
        put onByte219
        put onByte220
        put onByte221
        put onByte222
        put onByte223
        put onByte224
        put onByte225
        put onByte226
        put onByte227
        put onByte228
        put onByte229
        put onByte230
        put onByte231
        put onByte232
        put onByte233
        put onByte234
        put onByte235
        put onByte236
        put onByte237
        put onByte238
        put onByte239
        put onByte240
        put onByte241
        put onByte242
        put onByte243
        put onByte244
        put onByte245
        put onByte246
        put onByte247
        put onByte248
        put onByte249
        put onByte250
        put onByte251
        put onByte252
        put onByte253
        put onByte254
        put onByte255

    get = do
        onByte000 <- get
        onByte001 <- get
        onByte002 <- get
        onByte003 <- get
        onByte004 <- get
        onByte005 <- get
        onByte006 <- get
        onByte007 <- get
        onByte008 <- get
        onByte009 <- get
        onByte010 <- get
        onByte011 <- get
        onByte012 <- get
        onByte013 <- get
        onByte014 <- get
        onByte015 <- get
        onByte016 <- get
        onByte017 <- get
        onByte018 <- get
        onByte019 <- get
        onByte020 <- get
        onByte021 <- get
        onByte022 <- get
        onByte023 <- get
        onByte024 <- get
        onByte025 <- get
        onByte026 <- get
        onByte027 <- get
        onByte028 <- get
        onByte029 <- get
        onByte030 <- get
        onByte031 <- get
        onByte032 <- get
        onByte033 <- get
        onByte034 <- get
        onByte035 <- get
        onByte036 <- get
        onByte037 <- get
        onByte038 <- get
        onByte039 <- get
        onByte040 <- get
        onByte041 <- get
        onByte042 <- get
        onByte043 <- get
        onByte044 <- get
        onByte045 <- get
        onByte046 <- get
        onByte047 <- get
        onByte048 <- get
        onByte049 <- get
        onByte050 <- get
        onByte051 <- get
        onByte052 <- get
        onByte053 <- get
        onByte054 <- get
        onByte055 <- get
        onByte056 <- get
        onByte057 <- get
        onByte058 <- get
        onByte059 <- get
        onByte060 <- get
        onByte061 <- get
        onByte062 <- get
        onByte063 <- get
        onByte064 <- get
        onByte065 <- get
        onByte066 <- get
        onByte067 <- get
        onByte068 <- get
        onByte069 <- get
        onByte070 <- get
        onByte071 <- get
        onByte072 <- get
        onByte073 <- get
        onByte074 <- get
        onByte075 <- get
        onByte076 <- get
        onByte077 <- get
        onByte078 <- get
        onByte079 <- get
        onByte080 <- get
        onByte081 <- get
        onByte082 <- get
        onByte083 <- get
        onByte084 <- get
        onByte085 <- get
        onByte086 <- get
        onByte087 <- get
        onByte088 <- get
        onByte089 <- get
        onByte090 <- get
        onByte091 <- get
        onByte092 <- get
        onByte093 <- get
        onByte094 <- get
        onByte095 <- get
        onByte096 <- get
        onByte097 <- get
        onByte098 <- get
        onByte099 <- get
        onByte100 <- get
        onByte101 <- get
        onByte102 <- get
        onByte103 <- get
        onByte104 <- get
        onByte105 <- get
        onByte106 <- get
        onByte107 <- get
        onByte108 <- get
        onByte109 <- get
        onByte110 <- get
        onByte111 <- get
        onByte112 <- get
        onByte113 <- get
        onByte114 <- get
        onByte115 <- get
        onByte116 <- get
        onByte117 <- get
        onByte118 <- get
        onByte119 <- get
        onByte120 <- get
        onByte121 <- get
        onByte122 <- get
        onByte123 <- get
        onByte124 <- get
        onByte125 <- get
        onByte126 <- get
        onByte127 <- get
        onByte128 <- get
        onByte129 <- get
        onByte130 <- get
        onByte131 <- get
        onByte132 <- get
        onByte133 <- get
        onByte134 <- get
        onByte135 <- get
        onByte136 <- get
        onByte137 <- get
        onByte138 <- get
        onByte139 <- get
        onByte140 <- get
        onByte141 <- get
        onByte142 <- get
        onByte143 <- get
        onByte144 <- get
        onByte145 <- get
        onByte146 <- get
        onByte147 <- get
        onByte148 <- get
        onByte149 <- get
        onByte150 <- get
        onByte151 <- get
        onByte152 <- get
        onByte153 <- get
        onByte154 <- get
        onByte155 <- get
        onByte156 <- get
        onByte157 <- get
        onByte158 <- get
        onByte159 <- get
        onByte160 <- get
        onByte161 <- get
        onByte162 <- get
        onByte163 <- get
        onByte164 <- get
        onByte165 <- get
        onByte166 <- get
        onByte167 <- get
        onByte168 <- get
        onByte169 <- get
        onByte170 <- get
        onByte171 <- get
        onByte172 <- get
        onByte173 <- get
        onByte174 <- get
        onByte175 <- get
        onByte176 <- get
        onByte177 <- get
        onByte178 <- get
        onByte179 <- get
        onByte180 <- get
        onByte181 <- get
        onByte182 <- get
        onByte183 <- get
        onByte184 <- get
        onByte185 <- get
        onByte186 <- get
        onByte187 <- get
        onByte188 <- get
        onByte189 <- get
        onByte190 <- get
        onByte191 <- get
        onByte192 <- get
        onByte193 <- get
        onByte194 <- get
        onByte195 <- get
        onByte196 <- get
        onByte197 <- get
        onByte198 <- get
        onByte199 <- get
        onByte200 <- get
        onByte201 <- get
        onByte202 <- get
        onByte203 <- get
        onByte204 <- get
        onByte205 <- get
        onByte206 <- get
        onByte207 <- get
        onByte208 <- get
        onByte209 <- get
        onByte210 <- get
        onByte211 <- get
        onByte212 <- get
        onByte213 <- get
        onByte214 <- get
        onByte215 <- get
        onByte216 <- get
        onByte217 <- get
        onByte218 <- get
        onByte219 <- get
        onByte220 <- get
        onByte221 <- get
        onByte222 <- get
        onByte223 <- get
        onByte224 <- get
        onByte225 <- get
        onByte226 <- get
        onByte227 <- get
        onByte228 <- get
        onByte229 <- get
        onByte230 <- get
        onByte231 <- get
        onByte232 <- get
        onByte233 <- get
        onByte234 <- get
        onByte235 <- get
        onByte236 <- get
        onByte237 <- get
        onByte238 <- get
        onByte239 <- get
        onByte240 <- get
        onByte241 <- get
        onByte242 <- get
        onByte243 <- get
        onByte244 <- get
        onByte245 <- get
        onByte246 <- get
        onByte247 <- get
        onByte248 <- get
        onByte249 <- get
        onByte250 <- get
        onByte251 <- get
        onByte252 <- get
        onByte253 <- get
        onByte254 <- get
        onByte255 <- get
        return (Matrix {..})
