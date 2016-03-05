-- |
-- Module      :  Codec.Archive.Zip.CP437
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Support for decoding of CP437 text.

module Codec.Archive.Zip.CP437
  ( decodeCP437 )
where

import Data.ByteString (ByteString)
import Data.Char
import Data.Monoid
import Data.Text (Text)
import Data.Word (Word8)
import qualified Data.ByteString        as B
import qualified Data.Text as T

-- | Decode a 'ByteString' containing CP 437 encoded text.

decodeCP437 :: ByteString -> Text
decodeCP437 = T.unfoldr (fmap (\(b,rest) -> (decodeByteCP437 b, rest)) . B.uncons)


-- | Decode single byte of CP437 encoded text.

decodeByteCP437 :: Word8 -> Char
decodeByteCP437 byte = chr $ case byte of
  128 -> 199
  129 -> 252
  130 -> 233
  131 -> 226
  132 -> 228
  133 -> 224
  134 -> 229
  135 -> 231
  136 -> 234
  137 -> 235
  138 -> 232
  139 -> 239
  140 -> 238
  141 -> 236
  142 -> 196
  143 -> 197
  144 -> 201
  145 -> 230
  146 -> 198
  147 -> 244
  148 -> 246
  149 -> 242
  150 -> 251
  151 -> 249
  152 -> 255
  153 -> 214
  154 -> 220
  155 -> 162
  156 -> 163
  157 -> 165
  158 -> 8359
  159 -> 402
  160 -> 225
  161 -> 237
  162 -> 243
  163 -> 250
  164 -> 241
  165 -> 209
  166 -> 170
  167 -> 186
  168 -> 191
  169 -> 8976
  170 -> 172
  171 -> 189
  172 -> 188
  173 -> 161
  174 -> 171
  175 -> 187
  176 -> 9617
  177 -> 9618
  178 -> 9619
  179 -> 9474
  180 -> 9508
  181 -> 9569
  182 -> 9570
  183 -> 9558
  184 -> 9557
  185 -> 9571
  186 -> 9553
  187 -> 9559
  188 -> 9565
  189 -> 9564
  190 -> 9563
  191 -> 9488
  192 -> 9492
  193 -> 9524
  194 -> 9516
  195 -> 9500
  196 -> 9472
  197 -> 9532
  198 -> 9566
  199 -> 9567
  200 -> 9562
  201 -> 9556
  202 -> 9577
  203 -> 9574
  204 -> 9568
  205 -> 9552
  206 -> 9580
  207 -> 9575
  208 -> 9576
  209 -> 9572
  210 -> 9573
  211 -> 9561
  212 -> 9560
  213 -> 9554
  214 -> 9555
  215 -> 9579
  216 -> 9578
  217 -> 9496
  218 -> 9484
  219 -> 9608
  220 -> 9604
  221 -> 9612
  222 -> 9616
  223 -> 9600
  224 -> 945
  225 -> 223
  226 -> 915
  227 -> 960
  228 -> 931
  229 -> 963
  230 -> 181
  231 -> 964
  232 -> 934
  233 -> 920
  234 -> 937
  235 -> 948
  236 -> 8734
  237 -> 966
  238 -> 949
  239 -> 8745
  240 -> 8801
  241 -> 177
  242 -> 8805
  243 -> 8804
  244 -> 8992
  245 -> 8993
  246 -> 247
  247 -> 8776
  248 -> 176
  249 -> 8729
  250 -> 183
  251 -> 8730
  252 -> 8319
  253 -> 178
  254 -> 9632
  255 -> 160
  x   -> fromIntegral x -- the rest of characters translate directly
