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
getInfo = info <$> getPosition
  where
  info pos = FileInput {
      fileName = sourceName   pos
    , line     = sourceLine   pos
    , column   = sourceColumn pos
  }

term :: Context -> Parser Term
term ctx
　 = literal
  </> apply    ctx
  </> abstruct ctx
  </> variable ctx
  </> parens 　(term ctx)

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
  drawCtx info name =
    let len = length ctx in
    case nameToIndex' ctx info name of
      Right idx -> TmVar info idx len
      Left  _   -> TmSym info name

parseType :: Parser Type
parseType = arrowType </> constantType
  where
  arrowType = TyArrow <$> constantType <*> arrow
  arrow = whiteSpace *> string "->" *> whiteSpace *> parseType

constantType :: Parser Type
constantType
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
  form = parens $ do
    info <- getInfo
    args <- vargs
    retn <- body (foldr (:) ctx (reverse args))
    return $ foldr (folding info) retn args
  vargs   = parens $ sepBy1 (whiteSpace *> binding) (char ',')
  body cx = whiteSpace *> string "->" *> whiteSpace *> term cx
  folding fi (n, VarBind t) = TmAbs fi n t

-- Function Apply form (f x)
apply :: Context -> Parser Term
apply ctx = form
  where
  form   = app <$> getInfo <*> callee <*> caller
  callee = parens (apply ctx) </> abstruct ctx </> variable ctx
  caller = many1 (whiteSpace *> term ctx)
  app _ f []     = f
  app i f (t:[]) = TmApp i f t
  app i f (t:ts) = app i (TmApp i f t) ts


--
