{-# LANGUAGE FlexibleContexts #-}
module Ylang.Parser
 ( module Ylang.Parser.Lexer
 , getInfo
 , term
 , literal
 , binding
 , apply
 ) where

import Control.Applicative hiding ((<|>), many)
import Text.Parsec
import Text.Parsec.Text (Parser)

import Ylang.Info
import Ylang.Type
import Ylang.Context
import Ylang.Syntax.Term hiding (Context)
import Ylang.Parser.Combinator
import Ylang.Parser.Lexer
import qualified Ylang.Parser.Token as T

getInfo :: Parser Info
getInfo = constructor <$> getPosition
  where
  constructor pos = FileInput {
      fileName = sourceName   pos
    , line     = sourceLine   pos
    , column   = sourceColumn pos
  }

term :: Context -> Parser Term
term ctx
   =  literal
  </> abstruct ctx
  </> apply    ctx
  </> variable ctx

literal :: Parser Term
literal = TmLit <$> getInfo <*> lit
  where
  lit =  T.hole
     </> T.unit
     </> T.rational
     </> T.float
     </> T.integer
     </> T.keyword
     </> T.charactor
     </> T.string

identifier :: Parser String
identifier = (:) <$> letter <*> many alphaNum

variable :: Context -> Parser Term
variable ctx = drawCtx <$> getInfo <*> identifier
  where
  drawCtx :: Info -> Name -> Term
  drawCtx info name =
    let len = length ctx in
    case nameToIndex' ctx info name of
      Right idx -> TmVar info idx len
      Left  _   -> TmSym info name

parseType :: Parser Type
parseType = arrow </> uniq
  where
  arrow = TyArrow <$> uniq <*> parseArrow
  parseArrow = whiteSpace *> string "->" *> whiteSpace *> parseType
  uniq
    =  (TyTop     <! "Set")
   </> (TyBottom  <! "_|_")
   </> (TyUnit    <! "()")
   </> (TyBool    <! "Bool")
   </> (TyChar    <! "Char")
   </> (TyString  <! "String")
   </> (TyKeyword <! "Keyword")
   </> (TyNatural <! "Nat")
   </> (TyInteger <! "Integer")
   </> (TyFlonum  <! "Flonum")
   </> (TyRatio   <! "Rational")

binding :: Parser (Name, Binding)
binding = typeBind
  where
  typeBind  = bind <$> identifier <*> colonBind
  colonBind = whiteSpace *> char ':' *> whiteSpace *> parseType
  bind n t = (n, VarBind t)

-- Lambda Expression ((x:T1, y:T2, z:T3) -> <term>)
abstruct :: Context -> Parser Term
abstruct ctx = form
  where
  form :: Parser Term
  form = parens $ do
    inf <- getInfo
    ags <- parens $ sepBy1 (whiteSpace *> binding) (char ',')
    let ctx' = foldr (:) ctx (reverse ags)
    bdy <- whiteSpace *> string "->" *> whiteSpace *> term ctx'
    return $ foldr (\(n, VarBind t) r -> TmAbs inf n t r) bdy ags

apply :: Context -> Parser Term
apply ctx = form
  where
  form = TmApp <$> getInfo <*> variable ctx <*> (whiteSpace *> term ctx)


--
