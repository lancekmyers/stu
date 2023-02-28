module Util where 

import Text.Megaparsec.Pos (SourcePos(..), unPos)
import Error.Diagnose.Position (Position(..))

type SrcSpan = Position

mkPos :: SourcePos -> SourcePos -> SrcSpan
mkPos (SourcePos file l c) (SourcePos _ l' c') 
    = Position (unPos l, unPos l') (unPos c, unPos c') file

joinSrcSpan :: SrcSpan -> SrcSpan -> SrcSpan 
joinSrcSpan 
    (Position (l11, l12) (c11, c12) fname) 
    (Position (l21, l22) (c21, c22) _) 
    = Position (l, l') (c, c') fname 
    where 
        (l,  c)  = (l11, c11) `min` (l21, c21)
        (l', c') = (l12, c12) `max` (l22, c22)