{-  LANGUAGE OverloadedStrings  -}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}

module Sig where

import Data.Binary (Binary(..))
import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Dhall (Interpret)
import GHC.Generics (Generic)
import Foreign (Ptr)
import Foreign.C.Types

import qualified Control.Concurrent
import qualified Control.Parallel.Strategies
import qualified Data.Binary
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Unsafe
import qualified Foreign
import qualified Foreign.Marshal.Unsafe
import qualified System.IO.MMap

data State
    = S00
    | S01
    | S02
    | S03
    | S04
    | S05
    | S06
    | S07
    | S08
    | S09
    | S10
    | S11
    | S12
    | S13
    | S14
    | S15
    deriving (Generic, Interpret, NFData, Show)

instance Binary State where
    put S00 = put ( 0 :: Word8)
    put S01 = put ( 1 :: Word8)
    put S02 = put ( 2 :: Word8)
    put S03 = put ( 3 :: Word8)
    put S04 = put ( 4 :: Word8)
    put S05 = put ( 5 :: Word8)
    put S06 = put ( 6 :: Word8)
    put S07 = put ( 7 :: Word8)
    put S08 = put ( 8 :: Word8)
    put S09 = put ( 9 :: Word8)
    put S10 = put (10 :: Word8)
    put S11 = put (11 :: Word8)
    put S12 = put (12 :: Word8)
    put S13 = put (13 :: Word8)
    put S14 = put (14 :: Word8)
    put S15 = put (15 :: Word8)

    get = do
        n <- get
        case n :: Word8 of
            0  -> return S00
            1  -> return S01
            2  -> return S02
            3  -> return S03
            4  -> return S04
            5  -> return S05
            6  -> return S06
            7  -> return S07
            8  -> return S08
            9  -> return S09
            10 -> return S10
            11 -> return S11
            12 -> return S12
            13 -> return S13
            14 -> return S14
            15 -> return S15
            _  -> fail ("Data.Binary.get[State]: Unexpected Word8: " ++ show n)

data Transition = Transition
    { fromState00To :: State
    , fromState01To :: State
    , fromState02To :: State
    , fromState03To :: State
    , fromState04To :: State
    , fromState05To :: State
    , fromState06To :: State
    , fromState07To :: State
    , fromState08To :: State
    , fromState09To :: State
    , fromState10To :: State
    , fromState11To :: State
    , fromState12To :: State
    , fromState13To :: State
    , fromState14To :: State
    , fromState15To :: State
    } deriving (Generic, Interpret, NFData, Show)

instance Monoid Transition where
    mempty = Transition {..}
      where
        fromState00To = S00
        fromState01To = S01
        fromState02To = S02
        fromState03To = S03
        fromState04To = S04
        fromState05To = S05
        fromState06To = S06
        fromState07To = S07
        fromState08To = S08
        fromState09To = S09
        fromState10To = S10
        fromState11To = S11
        fromState12To = S12
        fromState13To = S13
        fromState14To = S14
        fromState15To = S15

    mappend transitionL transitionR = Transition
        { fromState00To = go fromState00To
        , fromState01To = go fromState01To
        , fromState02To = go fromState02To
        , fromState03To = go fromState03To
        , fromState04To = go fromState04To
        , fromState05To = go fromState05To
        , fromState06To = go fromState06To
        , fromState07To = go fromState07To
        , fromState08To = go fromState08To
        , fromState09To = go fromState09To
        , fromState10To = go fromState10To
        , fromState11To = go fromState11To
        , fromState12To = go fromState12To
        , fromState13To = go fromState13To
        , fromState14To = go fromState14To
        , fromState15To = go fromState15To
        }
      where
        go f = toAccessor (f transitionL) transitionR

        toAccessor S00 = fromState00To
        toAccessor S01 = fromState01To
        toAccessor S02 = fromState02To
        toAccessor S03 = fromState03To
        toAccessor S04 = fromState04To
        toAccessor S05 = fromState05To
        toAccessor S06 = fromState06To
        toAccessor S07 = fromState07To
        toAccessor S08 = fromState08To
        toAccessor S09 = fromState09To
        toAccessor S10 = fromState10To
        toAccessor S11 = fromState11To
        toAccessor S12 = fromState12To
        toAccessor S13 = fromState13To
        toAccessor S14 = fromState14To
        toAccessor S15 = fromState15To

instance Binary Transition where
    put (Transition {..}) = do
        put fromState00To
        put fromState01To
        put fromState02To
        put fromState03To
        put fromState04To
        put fromState05To
        put fromState06To
        put fromState07To
        put fromState08To
        put fromState09To
        put fromState10To
        put fromState11To
        put fromState12To
        put fromState13To
        put fromState14To
        put fromState15To

    get = do
        fromState00To <- get
        fromState01To <- get
        fromState02To <- get
        fromState03To <- get
        fromState04To <- get
        fromState05To <- get
        fromState06To <- get
        fromState07To <- get
        fromState08To <- get
        fromState09To <- get
        fromState10To <- get
        fromState11To <- get
        fromState12To <- get
        fromState13To <- get
        fromState14To <- get
        fromState15To <- get
        return (Transition {..})

data Matrix = Matrix
    { onByte000 :: Transition
    , onByte001 :: Transition
    , onByte002 :: Transition
    , onByte003 :: Transition
    , onByte004 :: Transition
    , onByte005 :: Transition
    , onByte006 :: Transition
    , onByte007 :: Transition
    , onByte008 :: Transition
    , onByte009 :: Transition
    , onByte010 :: Transition
    , onByte011 :: Transition
    , onByte012 :: Transition
    , onByte013 :: Transition
    , onByte014 :: Transition
    , onByte015 :: Transition
    , onByte016 :: Transition
    , onByte017 :: Transition
    , onByte018 :: Transition
    , onByte019 :: Transition
    , onByte020 :: Transition
    , onByte021 :: Transition
    , onByte022 :: Transition
    , onByte023 :: Transition
    , onByte024 :: Transition
    , onByte025 :: Transition
    , onByte026 :: Transition
    , onByte027 :: Transition
    , onByte028 :: Transition
    , onByte029 :: Transition
    , onByte030 :: Transition
    , onByte031 :: Transition
    , onByte032 :: Transition
    , onByte033 :: Transition
    , onByte034 :: Transition
    , onByte035 :: Transition
    , onByte036 :: Transition
    , onByte037 :: Transition
    , onByte038 :: Transition
    , onByte039 :: Transition
    , onByte040 :: Transition
    , onByte041 :: Transition
    , onByte042 :: Transition
    , onByte043 :: Transition
    , onByte044 :: Transition
    , onByte045 :: Transition
    , onByte046 :: Transition
    , onByte047 :: Transition
    , onByte048 :: Transition
    , onByte049 :: Transition
    , onByte050 :: Transition
    , onByte051 :: Transition
    , onByte052 :: Transition
    , onByte053 :: Transition
    , onByte054 :: Transition
    , onByte055 :: Transition
    , onByte056 :: Transition
    , onByte057 :: Transition
    , onByte058 :: Transition
    , onByte059 :: Transition
    , onByte060 :: Transition
    , onByte061 :: Transition
    , onByte062 :: Transition
    , onByte063 :: Transition
    , onByte064 :: Transition
    , onByte065 :: Transition
    , onByte066 :: Transition
    , onByte067 :: Transition
    , onByte068 :: Transition
    , onByte069 :: Transition
    , onByte070 :: Transition
    , onByte071 :: Transition
    , onByte072 :: Transition
    , onByte073 :: Transition
    , onByte074 :: Transition
    , onByte075 :: Transition
    , onByte076 :: Transition
    , onByte077 :: Transition
    , onByte078 :: Transition
    , onByte079 :: Transition
    , onByte080 :: Transition
    , onByte081 :: Transition
    , onByte082 :: Transition
    , onByte083 :: Transition
    , onByte084 :: Transition
    , onByte085 :: Transition
    , onByte086 :: Transition
    , onByte087 :: Transition
    , onByte088 :: Transition
    , onByte089 :: Transition
    , onByte090 :: Transition
    , onByte091 :: Transition
    , onByte092 :: Transition
    , onByte093 :: Transition
    , onByte094 :: Transition
    , onByte095 :: Transition
    , onByte096 :: Transition
    , onByte097 :: Transition
    , onByte098 :: Transition
    , onByte099 :: Transition
    , onByte100 :: Transition
    , onByte101 :: Transition
    , onByte102 :: Transition
    , onByte103 :: Transition
    , onByte104 :: Transition
    , onByte105 :: Transition
    , onByte106 :: Transition
    , onByte107 :: Transition
    , onByte108 :: Transition
    , onByte109 :: Transition
    , onByte110 :: Transition
    , onByte111 :: Transition
    , onByte112 :: Transition
    , onByte113 :: Transition
    , onByte114 :: Transition
    , onByte115 :: Transition
    , onByte116 :: Transition
    , onByte117 :: Transition
    , onByte118 :: Transition
    , onByte119 :: Transition
    , onByte120 :: Transition
    , onByte121 :: Transition
    , onByte122 :: Transition
    , onByte123 :: Transition
    , onByte124 :: Transition
    , onByte125 :: Transition
    , onByte126 :: Transition
    , onByte127 :: Transition
    , onByte128 :: Transition
    , onByte129 :: Transition
    , onByte130 :: Transition
    , onByte131 :: Transition
    , onByte132 :: Transition
    , onByte133 :: Transition
    , onByte134 :: Transition
    , onByte135 :: Transition
    , onByte136 :: Transition
    , onByte137 :: Transition
    , onByte138 :: Transition
    , onByte139 :: Transition
    , onByte140 :: Transition
    , onByte141 :: Transition
    , onByte142 :: Transition
    , onByte143 :: Transition
    , onByte144 :: Transition
    , onByte145 :: Transition
    , onByte146 :: Transition
    , onByte147 :: Transition
    , onByte148 :: Transition
    , onByte149 :: Transition
    , onByte150 :: Transition
    , onByte151 :: Transition
    , onByte152 :: Transition
    , onByte153 :: Transition
    , onByte154 :: Transition
    , onByte155 :: Transition
    , onByte156 :: Transition
    , onByte157 :: Transition
    , onByte158 :: Transition
    , onByte159 :: Transition
    , onByte160 :: Transition
    , onByte161 :: Transition
    , onByte162 :: Transition
    , onByte163 :: Transition
    , onByte164 :: Transition
    , onByte165 :: Transition
    , onByte166 :: Transition
    , onByte167 :: Transition
    , onByte168 :: Transition
    , onByte169 :: Transition
    , onByte170 :: Transition
    , onByte171 :: Transition
    , onByte172 :: Transition
    , onByte173 :: Transition
    , onByte174 :: Transition
    , onByte175 :: Transition
    , onByte176 :: Transition
    , onByte177 :: Transition
    , onByte178 :: Transition
    , onByte179 :: Transition
    , onByte180 :: Transition
    , onByte181 :: Transition
    , onByte182 :: Transition
    , onByte183 :: Transition
    , onByte184 :: Transition
    , onByte185 :: Transition
    , onByte186 :: Transition
    , onByte187 :: Transition
    , onByte188 :: Transition
    , onByte189 :: Transition
    , onByte190 :: Transition
    , onByte191 :: Transition
    , onByte192 :: Transition
    , onByte193 :: Transition
    , onByte194 :: Transition
    , onByte195 :: Transition
    , onByte196 :: Transition
    , onByte197 :: Transition
    , onByte198 :: Transition
    , onByte199 :: Transition
    , onByte200 :: Transition
    , onByte201 :: Transition
    , onByte202 :: Transition
    , onByte203 :: Transition
    , onByte204 :: Transition
    , onByte205 :: Transition
    , onByte206 :: Transition
    , onByte207 :: Transition
    , onByte208 :: Transition
    , onByte209 :: Transition
    , onByte210 :: Transition
    , onByte211 :: Transition
    , onByte212 :: Transition
    , onByte213 :: Transition
    , onByte214 :: Transition
    , onByte215 :: Transition
    , onByte216 :: Transition
    , onByte217 :: Transition
    , onByte218 :: Transition
    , onByte219 :: Transition
    , onByte220 :: Transition
    , onByte221 :: Transition
    , onByte222 :: Transition
    , onByte223 :: Transition
    , onByte224 :: Transition
    , onByte225 :: Transition
    , onByte226 :: Transition
    , onByte227 :: Transition
    , onByte228 :: Transition
    , onByte229 :: Transition
    , onByte230 :: Transition
    , onByte231 :: Transition
    , onByte232 :: Transition
    , onByte233 :: Transition
    , onByte234 :: Transition
    , onByte235 :: Transition
    , onByte236 :: Transition
    , onByte237 :: Transition
    , onByte238 :: Transition
    , onByte239 :: Transition
    , onByte240 :: Transition
    , onByte241 :: Transition
    , onByte242 :: Transition
    , onByte243 :: Transition
    , onByte244 :: Transition
    , onByte245 :: Transition
    , onByte246 :: Transition
    , onByte247 :: Transition
    , onByte248 :: Transition
    , onByte249 :: Transition
    , onByte250 :: Transition
    , onByte251 :: Transition
    , onByte252 :: Transition
    , onByte253 :: Transition
    , onByte254 :: Transition
    , onByte255 :: Transition
    } deriving (Generic, Interpret, NFData, Show)

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

foreign import ccall "process" c_process
    :: Ptr CChar -> CSize -> Ptr CChar -> Ptr CChar -> IO ()

-- Wrap C `process` in a pure interface
process :: Matrix -> ByteString -> Transition
process matrix bytes = Data.Binary.decode (Data.ByteString.Lazy.fromStrict (
    Foreign.Marshal.Unsafe.unsafeLocalState (do
        Data.ByteString.Unsafe.unsafeUseAsCStringLen tBytes (\(ptrTBytes, _) ->
            Data.ByteString.Unsafe.unsafeUseAsCStringLen bytes (\(ptrIn, len) ->
                Foreign.allocaBytes 16 (\ptrOut -> do
                    c_process ptrIn (fromIntegral len) ptrTBytes ptrOut
                    Data.ByteString.packCStringLen (ptrOut, 16) ) ) ) ) ))
  where
    tBytes = Data.ByteString.Lazy.toStrict (Data.Binary.encode matrix)

-- Example transition matrix from paper for matching C-style comments
cStyleComments :: Matrix
cStyleComments = Matrix {..}
  where
    def = mempty
        { fromState00To = S00
        , fromState01To = S00
        , fromState02To = S02
        , fromState03To = S02
        }

    onByte000 = def
    onByte001 = def
    onByte002 = def
    onByte003 = def
    onByte004 = def
    onByte005 = def
    onByte006 = def
    onByte007 = def
    onByte008 = def
    onByte009 = def
    onByte010 = def
    onByte011 = def
    onByte012 = def
    onByte013 = def
    onByte014 = def
    onByte015 = def
    onByte016 = def
    onByte017 = def
    onByte018 = def
    onByte019 = def
    onByte020 = def
    onByte021 = def
    onByte022 = def
    onByte023 = def
    onByte024 = def
    onByte025 = def
    onByte026 = def
    onByte027 = def
    onByte028 = def
    onByte029 = def
    onByte030 = def
    onByte031 = def
    onByte032 = def
    onByte033 = def
    onByte034 = def
    onByte035 = def
    onByte036 = def
    onByte037 = def
    onByte038 = def
    onByte039 = def
    onByte040 = def
    onByte041 = def
    onByte042 = mempty
        { fromState00To = S00
        , fromState01To = S02
        , fromState02To = S03
        , fromState03To = S03
        }
    onByte043 = def
    onByte044 = def
    onByte045 = def
    onByte046 = def
    onByte047 = mempty
        { fromState00To = S01
        , fromState01To = S01
        , fromState02To = S02
        , fromState03To = S00
        }
    onByte048 = def
    onByte049 = def
    onByte050 = def
    onByte051 = def
    onByte052 = def
    onByte053 = def
    onByte054 = def
    onByte055 = def
    onByte056 = def
    onByte057 = def
    onByte058 = def
    onByte059 = def
    onByte060 = def
    onByte061 = def
    onByte062 = def
    onByte063 = def
    onByte064 = def
    onByte065 = def
    onByte066 = def
    onByte067 = def
    onByte068 = def
    onByte069 = def
    onByte070 = def
    onByte071 = def
    onByte072 = def
    onByte073 = def
    onByte074 = def
    onByte075 = def
    onByte076 = def
    onByte077 = def
    onByte078 = def
    onByte079 = def
    onByte080 = def
    onByte081 = def
    onByte082 = def
    onByte083 = def
    onByte084 = def
    onByte085 = def
    onByte086 = def
    onByte087 = def
    onByte088 = def
    onByte089 = def
    onByte090 = def
    onByte091 = def
    onByte092 = def
    onByte093 = def
    onByte094 = def
    onByte095 = def
    onByte096 = def
    onByte097 = def
    onByte098 = def
    onByte099 = def
    onByte100 = def
    onByte101 = def
    onByte102 = def
    onByte103 = def
    onByte104 = def
    onByte105 = def
    onByte106 = def
    onByte107 = def
    onByte108 = def
    onByte109 = def
    onByte110 = def
    onByte111 = def
    onByte112 = def
    onByte113 = def
    onByte114 = def
    onByte115 = def
    onByte116 = def
    onByte117 = def
    onByte118 = def
    onByte119 = def
    onByte120 = def
    onByte121 = def
    onByte122 = def
    onByte123 = def
    onByte124 = def
    onByte125 = def
    onByte126 = def
    onByte127 = def
    onByte128 = def
    onByte129 = def
    onByte130 = def
    onByte131 = def
    onByte132 = def
    onByte133 = def
    onByte134 = def
    onByte135 = def
    onByte136 = def
    onByte137 = def
    onByte138 = def
    onByte139 = def
    onByte140 = def
    onByte141 = def
    onByte142 = def
    onByte143 = def
    onByte144 = def
    onByte145 = def
    onByte146 = def
    onByte147 = def
    onByte148 = def
    onByte149 = def
    onByte150 = def
    onByte151 = def
    onByte152 = def
    onByte153 = def
    onByte154 = def
    onByte155 = def
    onByte156 = def
    onByte157 = def
    onByte158 = def
    onByte159 = def
    onByte160 = def
    onByte161 = def
    onByte162 = def
    onByte163 = def
    onByte164 = def
    onByte165 = def
    onByte166 = def
    onByte167 = def
    onByte168 = def
    onByte169 = def
    onByte170 = def
    onByte171 = def
    onByte172 = def
    onByte173 = def
    onByte174 = def
    onByte175 = def
    onByte176 = def
    onByte177 = def
    onByte178 = def
    onByte179 = def
    onByte180 = def
    onByte181 = def
    onByte182 = def
    onByte183 = def
    onByte184 = def
    onByte185 = def
    onByte186 = def
    onByte187 = def
    onByte188 = def
    onByte189 = def
    onByte190 = def
    onByte191 = def
    onByte192 = def
    onByte193 = def
    onByte194 = def
    onByte195 = def
    onByte196 = def
    onByte197 = def
    onByte198 = def
    onByte199 = def
    onByte200 = def
    onByte201 = def
    onByte202 = def
    onByte203 = def
    onByte204 = def
    onByte205 = def
    onByte206 = def
    onByte207 = def
    onByte208 = def
    onByte209 = def
    onByte210 = def
    onByte211 = def
    onByte212 = def
    onByte213 = def
    onByte214 = def
    onByte215 = def
    onByte216 = def
    onByte217 = def
    onByte218 = def
    onByte219 = def
    onByte220 = def
    onByte221 = def
    onByte222 = def
    onByte223 = def
    onByte224 = def
    onByte225 = def
    onByte226 = def
    onByte227 = def
    onByte228 = def
    onByte229 = def
    onByte230 = def
    onByte231 = def
    onByte232 = def
    onByte233 = def
    onByte234 = def
    onByte235 = def
    onByte236 = def
    onByte237 = def
    onByte238 = def
    onByte239 = def
    onByte240 = def
    onByte241 = def
    onByte242 = def
    onByte243 = def
    onByte244 = def
    onByte245 = def
    onByte246 = def
    onByte247 = def
    onByte248 = def
    onByte249 = def
    onByte250 = def
    onByte251 = def
    onByte252 = def
    onByte253 = def
    onByte254 = def
    onByte255 = def

-- | Split a `ByteString` into chunks of size @n@
chunkBytes :: Int -> ByteString -> [ByteString]
chunkBytes n bytes =
    if Data.ByteString.null bytes
    then []
    else prefix : chunkBytes n suffix
  where
    ~(prefix, suffix) = Data.ByteString.splitAt n bytes

-- Split the `ByteString` into @k@ chunks and call `process` in parallel
parallelProcess :: Matrix -> Int -> ByteString -> Transition
parallelProcess matrix k bytes =
    mconcat
        (Control.Parallel.Strategies.parMap
            Control.Parallel.Strategies.rdeepseq
            (process matrix)
            (chunkBytes (len `div` k) bytes) )
  where
    len = Data.ByteString.length bytes

main :: IO ()
main = do
    k     <- Control.Concurrent.getNumCapabilities
    bytes <- System.IO.MMap.mmapFileByteString "test.txt" Nothing
    -- Not yet implemented, combine the `k` transition matrices into the final
    -- transition matrix.  This is cheap since `k` is the number of cores, I
    -- just haven't had a chance to complete this yet.
    print (parallelProcess cStyleComments k bytes)
