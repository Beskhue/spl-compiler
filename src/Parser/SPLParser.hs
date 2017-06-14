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
            TP (TInclude s) p -> Just (AST.DeclI (AST.IncludeDecl s, AST.metaFromPos p), AST.metaFromPos p)
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
    ) <|> (do
        (classDecl, p) <- pClassDecl
        return (AST.DeclC (classDecl, p), p)
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
                        return (AST.VarDeclUntyped identifier expression, AST.metaFromPos p)
                    _ -> do
                        tok (TPunctuator PSeparator)
                        return (AST.VarDeclUntypedUnitialized identifier, AST.metaFromPos p)
            _ -> do
                (type', m) <- pType
                identifier <- pIdentifier
                t' <- pPeek
                case t' of
                    TOperator OAssignment -> do
                        tok (TOperator OAssignment)
                        expression <- pExpression
                        tok (TPunctuator PSeparator)
                        return (AST.VarDeclTyped (type', m) identifier expression, m)
                    _ -> do
                        tok (TPunctuator PSeparator)
                        return (AST.VarDeclTypedUnitialized (type', m) identifier, m)
    ) <?> "a variable declaration"

pFunDecl :: Parser AST.FunDecl
pFunDecl = (do
        (identifier, m) <- pIdentifier
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
                return (AST.FunDeclTyped (identifier, m) args funType statements, m)
            _ -> do
                tok (TPunctuator PBraceOpen)
                statements <- many pStatement
                tok (TPunctuator PBraceClose) <?> "a closing brace"
                return (AST.FunDeclUntyped (identifier, m) args statements, m)
    ) <?> "a function declaration"

pClassDecl :: Parser AST.ClassDecl
pClassDecl = do
    tok $ TKeyword KClass
    identifier@(_, m) <- pIdentifier
    tok $ TPunctuator PBraceOpen
    varDecls <- many $ try pVarDecl
    funDecls <- many pFunDecl
    tok $ TPunctuator PBraceClose
    return (AST.ClassDecl identifier varDecls funDecls, m)

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
pFunType :: Parser AST.Type
pFunType = (do
        types <- many pType <?> "argument types"
        TP _ p <- tok (TPunctuator PMapTo) <?> "a 'maps to' symbol (->)"
        returnType <- pType <?> "a return type"
        return (AST.TypeFunction types returnType, AST.metaFromPos p)
    ) <?> "a function type"

pType :: Parser AST.Type
pType = do
    t <- pTypeBasic <|> pTypeTuple <|> pTypeList <|> pTypeIdentifier
    pType' t
    where
        pType' :: AST.Type -> Parser AST.Type
        pType' t@(_, m) = do
            token <- pPeek
            case token of
                TPunctuator PAsterisk -> do
                    tok token
                    pType' (AST.TypePointer t, m)
                _ -> return t

pTypeTuple :: Parser AST.Type
pTypeTuple = (do
        TP _ p <- tok (TPunctuator PParenOpen)
        t1 <- pType
        tok (TPunctuator PComma)
        t2 <-  pType
        tok (TPunctuator PParenClose)
        return (AST.TypeTuple t1 t2, AST.metaFromPos p)
    ) <?> "a tuple type"

pTypeList :: Parser AST.Type
pTypeList = (do
        TP _ p <- tok (TPunctuator PSquareBracketOpen)
        t <- pType
        tok (TPunctuator PSquareBracketClose)
        return (AST.TypeList t, AST.metaFromPos p)
    ) <?> "a list type"

pTypeBasic :: Parser AST.Type
pTypeBasic = ((do
        TP _ p <- tok (TType Data.Token.TypeVoid)
        return (AST.TypeVoid, AST.metaFromPos p)
    ) <|>
    (do
        TP _ p <- tok (TType Data.Token.TypeInt)
        return (AST.TypeInt, AST.metaFromPos p)
    ) <|>
    (do
        TP _ p <- tok (TType Data.Token.TypeBool)
        return (AST.TypeBool, AST.metaFromPos p)
    ) <|>
    (do
        TP _ p <- tok (TType Data.Token.TypeChar)
        return (AST.TypeChar, AST.metaFromPos p)
    )) <?> "a basic type (Void, Int, Bool, Char)"

pTypeIdentifier :: Parser AST.Type
pTypeIdentifier = (do
    (identifier, m) <- pIdentifier
    return (AST.TypeIdentifier (identifier, m), m)) <?> "a type identifier"

pStatement :: Parser AST.Statement
pStatement = (do
        (varDecl, m) <- try pVarDecl
        return (AST.StmtVarDecl (varDecl, m), m)
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
                return (AST.StmtIfElse condition statements statements', AST.metaFromPos p)
            _ -> return (AST.StmtIf condition statements, AST.metaFromPos p)
    ) <?> "an if or if-else statement"

pStatementWhile :: Parser AST.Statement
pStatementWhile =
    (do
        TP _ p <- tok (TKeyword KWhile)
        condition <- pCondition
        statements <- pStatement
        return (AST.StmtWhile condition statements, AST.metaFromPos p)
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
        return (AST.StmtBlock statements, AST.metaFromPos p)
    ) <?> "a statement block"

pStatementFunCall :: Parser AST.Statement
pStatementFunCall = do
    (AST.ExprFunCall expr args, m) <- pExpression
    tok (TPunctuator PSeparator)
    return (AST.StmtFunCall expr args, m)
    <?> "a function call"

pStatementAssignment :: Parser AST.Statement
pStatementAssignment =
    (do
        expr1@(_, m) <- pExpression
        tok (TOperator OAssignment)
        expr2 <- pExpression
        tok (TPunctuator PSeparator)
        return (AST.StmtAssignment expr1 expr2, m)
    ) <?> "an assignment"

pStatementReturn :: Parser AST.Statement
pStatementReturn = (do
    TP _ p <- tok (TKeyword KReturn)
    maybeExpr <- optionMaybe pExpression
    tok (TPunctuator PSeparator)
    case maybeExpr of
        Just expr -> return (AST.StmtReturn expr, AST.metaFromPos p)
        Nothing -> return (AST.StmtReturnVoid, AST.metaFromPos p)) <?> "a return statement"

pExpression :: Parser AST.Expression
pExpression = do
    expr@(_, m) <- pExpression' 1
    t <- pPeek
    case t of
        (TPunctuator PParenOpen) -> do
            tok (TPunctuator PParenOpen)
            args <- pFunArgs
            tok (TPunctuator PParenClose) <?> "closing function call parenthesis"
            return (AST.ExprFunCall expr args, m)
        _ -> return expr
    where
        pExpression' :: Int -> Parser AST.Expression
        pExpression' 9          = pExprBase
        pExpression' precedence = do {
            (expr, m) <- pExpression' (precedence + 1); (do
                binaryOperator <- try (lookAhead pBinaryOperator)
                if AST.binaryOperatorPrecedence binaryOperator == precedence
                    then
                        case AST.binaryOperatorAssociativity binaryOperator of
                            AST.ALeft -> -- Left associativity
                                pLeftAssocExpression (expr, m) precedence
                            AST.ARight -> do -- Right associativity
                                pBinaryOperator -- Consume binary operator
                                expr' <- pExpression' precedence
                                return (AST.ExprBinaryOp binaryOperator (expr, m) expr', m)
                    else return (expr, m)
            ) <|> return (expr, m)}
        pLeftAssocExpression :: AST.Expression -> Int -> Parser AST.Expression
        pLeftAssocExpression (e1, m) precedence = do {
                bOp <- try (lookAhead pBinaryOperator);
                if AST.binaryOperatorPrecedence bOp == precedence && AST.binaryOperatorAssociativity bOp == AST.ALeft
                    then do
                        pBinaryOperator -- Consume binary operator
                        e2 <- pExpression' (precedence + 1);
                        pLeftAssocExpression (AST.ExprBinaryOp bOp (e1, m) e2, m) precedence
                    else return (e1, m)
            } <|> return (e1, m)

pExprBase :: Parser AST.Expression
pExprBase = do
    expr <- (
        try pExpressionUnaryOperator
         <|> pExprGroupOrTuple
         <|> pExpressionConst
         <|> pExpressionClass
         <|> pExpressionIdentifier <?> "a base expression")
    t <- pPeek
    case t of
        TPunctuator PSquareBracketOpen -> try (do
            tokenP <- tok $ TPunctuator PSquareBracketOpen
            let m' = AST.metaFromPos $ Data.Token.pos tokenP
            expr' <- pExpression
            tok $ TPunctuator PSquareBracketClose
            pExprBase' (AST.ExprUnaryOp (AST.UnaryOpDereference, m')
                       (AST.ExprBinaryOp (AST.BinaryOpReferencePlus, m') expr expr', m'), m')
                ) <|> pExprBase' expr
        _ -> pExprBase' expr
    where
        pExprBase' :: AST.Expression -> Parser AST.Expression
        pExprBase' expr@(_, m) = do
            field <- many pField
            case field of
                [] -> return expr
                _ -> return (AST.ExprField expr field, m)

pExpressionIdentifier :: Parser AST.Expression
pExpressionIdentifier = do
    (identifier, m) <- pIdentifier
    return (AST.ExprIdentifier (identifier, m), m)

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
    (unaryOp, m) <- pUnaryOperator
    expression <- pExprBase
    return (AST.ExprUnaryOp (unaryOp, m) expression, m)

pExpressionConst :: Parser AST.Expression
pExpressionConst = do
    (constant, m) <- pConstant
    return (AST.ExprConstant (constant, m), m)

pExprGroupOrTuple :: Parser AST.Expression
pExprGroupOrTuple = do
    tok (TPunctuator PParenOpen) <?> "a parenthesized expression or tuple"
    (expression, m) <- pExpression
    t <- pPeek
    (tok (TPunctuator PParenClose) >> return (expression, m) <?> "a parenthesized expression") <|>
        (
            do
                tok (TPunctuator PComma)
                expression' <- pExpression
                tok (TPunctuator PParenClose)
                return (AST.ExprTuple (expression, m) expression', m)
            <?> "a tuple"
        )

pExpressionClass :: Parser AST.Expression
pExpressionClass = (do
        tokenP <- tok $ TKeyword KNew
        identifier <- pIdentifier
        return (AST.ExprNew identifier, AST.metaFromPos $ Data.Token.pos tokenP))
    <|> (do
        tokenP <- tok $ TKeyword KDelete
        expr <- pExpression
        return (AST.ExprDelete expr, AST.metaFromPos $ Data.Token.pos tokenP)
    )

pConstant :: Parser AST.Constant
pConstant = tokenPrim show advance
    (
        \tp -> case tp of
            TP (TConstant (CBool b)) p -> Just (AST.ConstBool b, AST.metaFromPos p)
            TP (TConstant (CInt i)) p -> Just (AST.ConstInt i, AST.metaFromPos p)
            TP (TConstant (CChar c)) p -> Just (AST.ConstChar c, AST.metaFromPos p)
            TP (TConstant CEmptyList) p -> Just (AST.ConstEmptyList, AST.metaFromPos p)
            _ -> Nothing
    ) <?> "a constant"

pUnaryOperator :: Parser AST.UnaryOperator
pUnaryOperator =
        tokenPrim show advance
        (
            \tp -> case tp of
                TP (TOperator ONeg) p -> Just (AST.UnaryOpNeg, AST.metaFromPos p)
                TP (TPunctuator PMinus) p -> Just (AST.UnaryOpSubtr, AST.metaFromPos p)
                TP (TPunctuator PAmpersand) p -> Just (AST.UnaryOpReference, AST.metaFromPos p)
                TP (TPunctuator PAsterisk) p -> Just (AST.UnaryOpDereference, AST.metaFromPos p)
                TP (TPunctuator PTilde) p -> Just (AST.UnaryOpBitwiseNot, AST.metaFromPos p)
                _ -> Nothing
        ) <|> ( do
            TP _ p <- tok (TPunctuator PParenOpen)
            t <- pType
            tok (TPunctuator PParenClose)
            return (AST.UnaryOpCast t, AST.metaFromPos p)
        ) <?> "a unary operator"

pBinaryOperator :: Parser AST.BinaryOperator
pBinaryOperator = tokenPrim show advance
    (
        \tp -> case tp of
            TP (TOperator OEq) p -> Just (AST.BinaryOpEq, AST.metaFromPos p)
            TP (TOperator ONEq) p -> Just (AST.BinaryOpNEq, AST.metaFromPos p)
            TP (TOperator OLT) p -> Just (AST.BinaryOpLT, AST.metaFromPos p)
            TP (TOperator OGT) p -> Just (AST.BinaryOpGT, AST.metaFromPos p)
            TP (TOperator OLTE) p -> Just (AST.BinaryOpLTE, AST.metaFromPos p)
            TP (TOperator OGTE) p -> Just (AST.BinaryOpGTE, AST.metaFromPos p)
            TP (TOperator OConcat) p -> Just (AST.BinaryOpConcat, AST.metaFromPos p)
            TP (TOperator OPlus) p -> Just (AST.BinaryOpPlus, AST.metaFromPos p)
            TP (TPunctuator PMinus) p -> Just (AST.BinaryOpSubtr, AST.metaFromPos p)
            TP (TOperator OReferencePlus) p -> Just (AST.BinaryOpReferencePlus, AST.metaFromPos p)
            TP (TOperator OReferenceMinus) p -> Just (AST.BinaryOpReferenceSubtr, AST.metaFromPos p)
            TP (TOperator OReferenceReferenceMinus) p -> Just (AST.BinaryOpReferenceReferenceSubtr, AST.metaFromPos p)
            TP (TPunctuator PAsterisk) p -> Just (AST.BinaryOpMult, AST.metaFromPos p)
            TP (TOperator ODivide) p -> Just (AST.BinaryOpDiv, AST.metaFromPos p)
            TP (TOperator OMod) p -> Just (AST.BinaryOpMod, AST.metaFromPos p)
            TP (TOperator OBitShiftLeft) p -> Just (AST.BinaryOpBitShiftLeft, AST.metaFromPos p)
            TP (TOperator OBitShiftRight) p -> Just (AST.BinaryOpBitShiftRight, AST.metaFromPos p)
            TP (TOperator ODot) p -> Just (AST.BinaryOpMember, AST.metaFromPos p)
            _ -> Nothing
    ) <|> try ( do
        TP _ p <- tok (TPunctuator PAmpersand)
        t <- pPeek
        case t of
            TPunctuator PAmpersand -> do
                tok (TPunctuator PAmpersand)
                return (AST.BinaryOpAnd, AST.metaFromPos p)
            _ -> return (AST.BinaryOpBitwiseAnd, AST.metaFromPos p)
    ) <|> try ( do
        TP _ p <- tok (TPunctuator PPipe)
        tok (TPunctuator PPipe)
        return (AST.BinaryOpOr, AST.metaFromPos p)
    ) <?> "a binary operator"

pField :: Parser AST.Field
pField = tokenPrim show advance
    (
        \tp -> case tp of
            TP (TField FHd) p -> Just (AST.FieldHd, AST.metaFromPos p)
            TP (TField FTl) p -> Just (AST.FieldTl, AST.metaFromPos p)
            TP (TField FFst) p -> Just (AST.FieldFst, AST.metaFromPos p)
            TP (TField FSnd) p -> Just (AST.FieldSnd, AST.metaFromPos p)
            _ -> Nothing
    ) <?> "a field identifier"

pIdentifier :: Parser AST.Identifier
pIdentifier = tokenPrim show advance
    (
        \tp -> case tp of
            TP (TIdentifier identifier) p -> Just (AST.Identifier identifier, AST.metaFromPos p)
            _ -> Nothing
    ) <?> "an identifier"

pSeparator :: Parser TokenP
pSeparator = tok (TPunctuator PSeparator)

parseTest :: (Show a) => Parser a -> [TokenP] -> IO ()
parseTest = Parsec.parseTest
