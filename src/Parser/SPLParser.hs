module Parser.SPLParser where

import qualified Debug.Trace as Trace
import Data.Token
import Data.Pos
import qualified Text.Parsec.Pos as SourcePos
import Text.Parsec.Prim
import Text.Parsec.Combinator
import qualified Text.Parsec as Parsec
import qualified Data.AST as AST

type Parser a = Parsec [TokenP] () a

type AST = (AST', Pos)
data AST' = AChar Char
         | AExpr AST AST
           deriving (Show)

parseDet :: [TokenP] -> AST.SPL
parseDet = parseDet' pSPL

parseDet' :: Parser a -> [TokenP] -> a
parseDet' parser ts = case Parser.SPLParser.parse' parser ts of
    Left err -> Trace.trace (show err) undefined
    Right ast -> ast

parse :: [TokenP] -> Either Parsec.ParseError AST.SPL
parse = parse' pSPL

parse' :: Parser a -> [TokenP] -> Either Parsec.ParseError a
parse' parser = Text.Parsec.Prim.parse parser ""

posToSourcePos :: Pos -> SourcePos.SourcePos
posToSourcePos (Pos name line column) = SourcePos.newPos name line column

advance :: SourcePos.SourcePos -> t -> [TokenP] -> SourcePos.SourcePos
advance _ _ (TP t pos : _) = posToSourcePos pos
advance pos _ [] = pos

satisfy :: (TokenP -> Bool) -> Parser TokenP
satisfy f = tokenPrim show advance
    (\tp -> if f tp then Just tp else Nothing)

tok :: Token -> Parser TokenP
tok t = satisfy ((== t) . Data.Token.token) <?> show t

pPeek :: Parser Token
pPeek = lookAhead $ tokenPrim show advance (\(TP t p) -> Just t)

pSPL :: Parser AST.SPL
pSPL = do
    includes <- many pInclude
    decls <- many1 pDecl
    tok TEOF
    return $ includes ++ decls

pInclude :: Parser AST.Decl
pInclude = tokenPrim show advance
    (
        \tp -> case tp of
            TP (TInclude s) p -> Just (AST.DeclI (AST.IncludeDecl s, p), p)
            _ -> Nothing
    ) <?> "an include statement"

-- TODO: get rid of 'try'
pDecl :: Parser AST.Decl
pDecl = try (do
        (varDecl, p) <- pVarDecl
        return (AST.DeclV (varDecl, p), p)
    ) <|> (do
        (funDecl, p) <- pFunDecl
        return (AST.DeclF (funDecl, p), p)
    ) <?> "a variable or function declaration"

pVarDecl :: Parser AST.VarDecl
pVarDecl = (do
        t <- pPeek
        case t of
            TKeyword KVar -> do
                TP _ p <- tok (TKeyword KVar)
                identifier <- pIdentifier
                t' <- pPeek
                case t' of
                    TOperator OAssignment -> do
                        tok (TOperator OAssignment)
                        expression <- pExpression
                        tok (TPunctuator PSeparator)
                        return (AST.VarDeclUntyped identifier expression, p)
                    _ -> do
                        tok (TPunctuator PSeparator)
                        return (AST.VarDeclUntypedUnitialized identifier, p)
            _ -> do
                (type', p) <- pType
                identifier <- pIdentifier
                t' <- pPeek
                case t' of
                    TOperator OAssignment -> do
                        tok (TOperator OAssignment)
                        expression <- pExpression
                        tok (TPunctuator PSeparator)
                        return (AST.VarDeclTyped (type', p) identifier expression, p)
                    _ -> do
                        tok (TPunctuator PSeparator)
                        return (AST.VarDeclTypedUnitialized (type', p) identifier, p)
    ) <?> "a variable declaration"

pFunDecl :: Parser AST.FunDecl
pFunDecl = (do
        (identifier, p) <- pIdentifier
        tok (TPunctuator PParenOpen)
        args <- pFunArgsDef
        tok (TPunctuator PParenClose)
        t <- pPeek
        case t of
            TPunctuator PFunType -> do
                tok (TPunctuator PFunType)
                funType <- pFunType
                tok (TPunctuator PBraceOpen)
                statements <- many1 pStatement
                tok (TPunctuator PBraceClose) <?> "a closing brace"
                return (AST.FunDeclTyped (identifier, p) args funType statements, p)
            _ -> do
                tok (TPunctuator PBraceOpen)
                statements <- many1 pStatement
                tok (TPunctuator PBraceClose) <?> "a closing brace"
                return (AST.FunDeclUntyped (identifier, p) args statements, p)
    ) <?> "a function declaration"

pFunArgsDef :: Parser [AST.Identifier]
pFunArgsDef = (
    do
        maybeIdentifier <- optionMaybe pIdentifier
        case maybeIdentifier of
            Just identifier -> do
                identifiers <- many pFunArgsDef'
                return $ identifier : identifiers
            Nothing -> return []
    ) <?> "function arguments"
    where
        pFunArgsDef' :: Parser AST.Identifier
        pFunArgsDef' = do
            tok (TPunctuator PComma)
            pIdentifier

-- TODO: set position to the position of the first type, if given, instead of the position of <-
pFunType :: Parser AST.FunType
pFunType = (do
        types <- many pType <?> "argument types"
        TP _ p <- tok (TPunctuator PMapTo) <?> "a 'maps to' symbol (->)"
        returnType <- pType <?> "a return type"
        return (AST.FunType types returnType, p)
    ) <?> "a function type"

pType :: Parser AST.Type
pType = do
    t <- pTypeBasic <|> pTypeTuple <|> pTypeList <|> pTypeIdentifier
    pType' t
    where
        pType' :: AST.Type -> Parser AST.Type
        pType' t@(_, p) = do
            token <- pPeek
            case token of
                TPunctuator PAsterisk -> do
                    tok token
                    pType' (AST.TypePointer t, p)
                _ -> return t

pTypeTuple :: Parser AST.Type
pTypeTuple = (do
        TP _ p <- tok (TPunctuator PParenOpen)
        t1 <- pType
        tok (TPunctuator PComma)
        t2 <-  pType
        tok (TPunctuator PParenClose)
        return (AST.TypeTuple t1 t2, p)
    ) <?> "a tuple type"

pTypeList :: Parser AST.Type
pTypeList = (do
        TP _ p <- tok (TPunctuator PSquareBracketOpen)
        t <- pType
        tok (TPunctuator PSquareBracketClose)
        return (AST.TypeList t, p)
    ) <?> "a list type"

pTypeBasic :: Parser AST.Type
pTypeBasic = ((do
        TP _ p <- tok (TType Data.Token.TypeVoid)
        return (AST.TypeVoid, p)
    ) <|>
    (do
        TP _ p <- tok (TType Data.Token.TypeInt)
        return (AST.TypeInt, p)
    ) <|>
    (do
        TP _ p <- tok (TType Data.Token.TypeBool)
        return (AST.TypeBool, p)
    ) <|>
    (do
        TP _ p <- tok (TType Data.Token.TypeChar)
        return (AST.TypeChar, p)
    )) <?> "a basic type (Void, Int, Bool, Char)"

pTypeIdentifier :: Parser AST.Type
pTypeIdentifier = (do
    (identifier, p) <- pIdentifier
    return (AST.TypeIdentifier (identifier, p), p)) <?> "a type identifier"

pStatement :: Parser AST.Statement
pStatement = (do
        (varDecl, p) <- try pVarDecl
        return (AST.StmtVarDecl (varDecl, p), p)
    ) <|> pStatementConditional <|> pStatementWhile <|> pStatementReturn <|> pStatementBlock <|> try pStatementFunCall <|> pStatementAssignment

pStatementConditional :: Parser AST.Statement
pStatementConditional =
    (do
        TP _ p <- tok (TKeyword KIf)
        condition <- pCondition
        statements <- pStatement
        t <- pPeek
        case t of
            TKeyword KElse -> do
                tok (TKeyword KElse)
                statements' <- pStatement
                return (AST.StmtIfElse condition statements statements', p)
            _ -> return (AST.StmtIf condition statements, p)
    ) <?> "an if or if-else statement"

pStatementWhile :: Parser AST.Statement
pStatementWhile =
    (do
        TP _ p <- tok (TKeyword KWhile)
        condition <- pCondition
        statements <- pStatement
        return (AST.StmtWhile condition statements, p)
    ) <?> "a while statement"

pCondition :: Parser AST.Expression
pCondition =
    (do
        tok (TPunctuator PParenOpen)
        condition <- pExpression
        tok (TPunctuator PParenClose)
        return condition
    ) <?> "a parenthesized condition"

pStatementBlock :: Parser AST.Statement
pStatementBlock =
    (do
        TP _ p <- tok (TPunctuator PBraceOpen)
        statements <- many pStatement
        tok (TPunctuator PBraceClose)
        return (AST.StmtBlock statements, p)
    ) <?> "a statement block"

pStatementFunCall :: Parser AST.Statement
pStatementFunCall = do
    (expr, p) <- pExpressionIdentifier
    let AST.ExprFunCall identifier args = expr
    tok (TPunctuator PSeparator)
    return (AST.StmtFunCall identifier args, p)
    <?> "a function call"

pStatementAssignment :: Parser AST.Statement
pStatementAssignment =
    (do
        expr1@(_, p) <- pExpression
        tok (TOperator OAssignment)
        expr2 <- pExpression
        tok (TPunctuator PSeparator)
        return (AST.StmtAssignment expr1 expr2, p)
    ) <?> "an assignment"

pStatementReturn :: Parser AST.Statement
pStatementReturn = (do
    TP _ p <- tok (TKeyword KReturn)
    maybeExpr <- optionMaybe pExpression
    tok (TPunctuator PSeparator)
    case maybeExpr of
        Just expr -> return (AST.StmtReturn expr, p)
        Nothing -> return (AST.StmtReturnVoid, p)) <?> "a return statement"

pExpression :: Parser AST.Expression
pExpression = pExpression' 1
    where
        pExpression' :: Int -> Parser AST.Expression
        pExpression' 8          = pExprBase
        pExpression' precedence = do {
            (expr, p) <- pExpression' (precedence + 1); (do
                binaryOperator <- try (lookAhead pBinaryOperator)
                if AST.binaryOperatorPrecedence binaryOperator == precedence
                    then
                        case AST.binaryOperatorAssociativity binaryOperator of
                            AST.ALeft -> -- Left associativity
                                pLeftAssocExpression (expr, p) precedence
                            AST.ARight -> do -- Right associativity
                                pBinaryOperator -- Consume binary operator
                                expr' <- pExpression' precedence
                                return (AST.ExprBinaryOp binaryOperator (expr, p) expr', p)
                    else return (expr, p)
            ) <|> return (expr, p)}
        pLeftAssocExpression :: AST.Expression -> Int -> Parser AST.Expression
        pLeftAssocExpression (e1, p) precedence = do {
                bOp <- try (lookAhead pBinaryOperator);
                if AST.binaryOperatorPrecedence bOp == precedence && AST.binaryOperatorAssociativity bOp == AST.ALeft
                    then do
                        pBinaryOperator -- Consume binary operator
                        e2 <- pExpression' (precedence + 1);
                        pLeftAssocExpression (AST.ExprBinaryOp bOp (e1, p) e2, p) precedence
                    else return (e1, p)
            } <|> return (e1, p)

pExprBase :: Parser AST.Expression
pExprBase = do
     expr <- (
        try pExpressionUnaryOperator
         <|> pExprGroupOrTuple
         <|> pExpressionConst
         <|> pExpressionIdentifier <?> "a base expression")
     pExprBase' expr
     where
         pExprBase' :: AST.Expression -> Parser AST.Expression
         pExprBase' expr@(_, p) = do
             field <- many pField
             case field of
                 [] -> return expr
                 _ -> return (AST.ExprField expr field, p)

pExpressionIdentifier :: Parser AST.Expression
pExpressionIdentifier = do
    (identifier, p) <- pIdentifier
    t <- pPeek
    case t of
        (TPunctuator PParenOpen) -> do
            tok (TPunctuator PParenOpen)
            args <- pFunArgs
            tok (TPunctuator PParenClose) <?> "closing function call parenthesis"
            return (AST.ExprFunCall (identifier, p) args, p)
        _ -> return (AST.ExprIdentifier (identifier, p), p)

pFunArgs :: Parser [AST.Expression]
pFunArgs = (
    do
        maybeExpr <- optionMaybe pExpression
        case maybeExpr of
            Just expr -> do
                args <- many pFunArgs'
                return $ expr : args
            Nothing -> return []
    ) <?> "function arguments"
    where
        pFunArgs' :: Parser AST.Expression
        pFunArgs' = do
            tok (TPunctuator PComma)
            pExpression

pExpressionUnaryOperator :: Parser AST.Expression
pExpressionUnaryOperator = do
    (unaryOp, p) <- pUnaryOperator
    expression <- pExprBase
    return (AST.ExprUnaryOp (unaryOp, p) expression, p)

pExpressionConst :: Parser AST.Expression
pExpressionConst = do
    (constant, p) <- pConstant
    return (AST.ExprConstant (constant, p), p)

pExprGroupOrTuple :: Parser AST.Expression
pExprGroupOrTuple = do
    tok (TPunctuator PParenOpen) <?> "a parenthesized expression or tuple"
    (expression, p) <- pExpression
    t <- pPeek
    (tok (TPunctuator PParenClose) >> return (expression, p) <?> "a parenthesized expression") <|>
        (
            do
                tok (TPunctuator PComma)
                expression' <- pExpression
                tok (TPunctuator PParenClose)
                return (AST.ExprTuple (expression, p) expression', p)
            <?> "a tuple"
        )

pConstant :: Parser AST.Constant
pConstant = tokenPrim show advance
    (
        \tp -> case tp of
            TP (TConstant (CBool b)) p -> Just (AST.ConstBool b, p)
            TP (TConstant (CInt i)) p -> Just (AST.ConstInt i, p)
            TP (TConstant (CChar c)) p -> Just (AST.ConstChar c, p)
            TP (TConstant CEmptyList) p -> Just (AST.ConstEmptyList, p)
            _ -> Nothing
    ) <?> "a constant"

pUnaryOperator :: Parser AST.UnaryOperator
pUnaryOperator =
        tokenPrim show advance
        (
            \tp -> case tp of
                TP (TOperator ONeg) p -> Just (AST.UnaryOpNeg, p)
                TP (TPunctuator PMinus) p -> Just (AST.UnaryOpSubtr, p)
                TP (TPunctuator PAmpersand) p -> Just (AST.UnaryOpReference, p)
                TP (TPunctuator PAsterisk) p -> Just (AST.UnaryOpDereference, p)
                TP (TPunctuator PTilde) p -> Just (AST.UnaryOpBitwiseNot, p)
                _ -> Nothing
        ) <|> ( do
            TP _ p <- tok (TPunctuator PParenOpen)
            t <- pType
            tok (TPunctuator PParenClose)
            return (AST.UnaryOpCast t, p)
        ) <?> "a unary operator"

pBinaryOperator :: Parser AST.BinaryOperator
pBinaryOperator = tokenPrim show advance
    (
        \tp -> case tp of
            TP (TOperator OEq) p -> Just (AST.BinaryOpEq, p)
            TP (TOperator ONEq) p -> Just (AST.BinaryOpNEq, p)
            TP (TOperator OLT) p -> Just (AST.BinaryOpLT, p)
            TP (TOperator OGT) p -> Just (AST.BinaryOpGT, p)
            TP (TOperator OLTE) p -> Just (AST.BinaryOpLTE, p)
            TP (TOperator OGTE) p -> Just (AST.BinaryOpGTE, p)
            TP (TOperator OConcat) p -> Just (AST.BinaryOpConcat, p)
            TP (TOperator OPlus) p -> Just (AST.BinaryOpPlus, p)
            TP (TPunctuator PMinus) p -> Just (AST.BinaryOpSubtr, p)
            TP (TOperator OReferencePlus) p -> Just (AST.BinaryOpReferencePlus, p)
            TP (TOperator OReferenceMinus) p -> Just (AST.BinaryOpReferenceSubtr, p)
            TP (TOperator OReferenceReferenceMinus) p -> Just (AST.BinaryOpReferenceReferenceSubtr, p)
            TP (TPunctuator PAsterisk) p -> Just (AST.BinaryOpMult, p)
            TP (TOperator ODivide) p -> Just (AST.BinaryOpDiv, p)
            TP (TOperator OMod) p -> Just (AST.BinaryOpMod, p)
            TP (TOperator OBitShiftLeft) p -> Just (AST.BinaryOpBitShiftLeft, p)
            TP (TOperator OBitShiftRight) p -> Just (AST.BinaryOpBitShiftRight, p)
            _ -> Nothing
    ) <|> try ( do
        TP _ p <- tok (TPunctuator PAmpersand)
        t <- pPeek
        case t of
            TPunctuator PAmpersand -> do
                tok (TPunctuator PAmpersand)
                return (AST.BinaryOpAnd, p)
            _ -> return (AST.BinaryOpBitwiseAnd, p)
    ) <|> try ( do
        TP _ p <- tok (TPunctuator PPipe)
        tok (TPunctuator PPipe)
        return (AST.BinaryOpOr, p)
    ) <?> "a binary operator"

pField :: Parser AST.Field
pField = tokenPrim show advance
    (
        \tp -> case tp of
            TP (TField FHd) p -> Just (AST.FieldHd, p)
            TP (TField FTl) p -> Just (AST.FieldTl, p)
            TP (TField FFst) p -> Just (AST.FieldFst, p)
            TP (TField FSnd) p -> Just (AST.FieldSnd, p)
            _ -> Nothing
    ) <?> "a field identifier"

pIdentifier :: Parser AST.Identifier
pIdentifier = tokenPrim show advance
    (
        \tp -> case tp of
            TP (TIdentifier identifier) p -> Just (AST.Identifier identifier, p)
            _ -> Nothing
    ) <?> "an identifier"

pSeparator :: Parser TokenP
pSeparator = tok (TPunctuator PSeparator)

parseTest :: (Show a) => Parser a -> [TokenP] -> IO ()
parseTest = Parsec.parseTest
