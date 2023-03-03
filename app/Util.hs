module Util where 

import Text.Megaparsec.Pos (SourcePos(..), unPos)
import Error.Diagnose.Position (Position(..))

type SrcSpan = Position

mkPos :: SourcePos -> SourcePos -> SrcSpan
mkPos (SourcePos file l c) (SourcePos _ l' c') 
    = Position (unPos l, unPos c) (unPos l', unPos c') file

joinSrcSpan :: SrcSpan -> SrcSpan -> SrcSpan 
joinSrcSpan 
    (Position start _stop fname) 
    (Position _start stop _) 
    = Position start stop fname 
