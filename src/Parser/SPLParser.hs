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
advance _ _ ((TP t pos) : _) = posToSourcePos pos
advance pos _ [] = pos

satisfy :: (TokenP -> Bool) -> Parser TokenP
satisfy f = tokenPrim show advance
    (\tp -> if f tp then Just tp else Nothing)

tok :: Token -> Parser TokenP
tok t = (satisfy $ (== t) . Data.Token.token) <?> show t

pPeek :: Parser Token
pPeek = lookAhead $ tokenPrim show advance (\(TP t p) -> Just t)

pSPL :: Parser AST.SPL
pSPL = many1 pDecl

-- TODO: get rid of 'try'
pDecl :: Parser AST.Decl
pDecl = try (do
        (varDecl, p) <- pVarDecl
        return (AST.DeclV (varDecl, p), p)
    ) <|> (do
        (funDecl, p) <- pFunDecl
        return (AST.DeclF (funDecl, p), p)
    )

pVarDecl :: Parser AST.VarDecl
pVarDecl = (do
        t <- pPeek
        case t of
            TKeyword KVar -> do
                TP _ p <- tok (TKeyword KVar)
                identifier <- pIdentifier
                tok (TOperator OAssignment)
                expression <- pExpression
                tok (TPunctuator PSeparator)
                return (AST.VarDeclUntyped identifier expression, p)
            _ -> do
                 (type', p) <- pType
                 identifier <- pIdentifier
                 tok (TOperator OAssignment)
                 expression <- pExpression
                 tok (TPunctuator PSeparator)
                 return (AST.VarDeclTyped (type', p) identifier expression, p)
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
                varDecls <- many (try pVarDecl) -- try can be removed, if identifiers cannot be used as types
                statements <- many1 pStatement
                tok (TPunctuator PBraceClose) <?> "a closing brace"
                return (AST.FunDeclTyped (identifier, p) args funType varDecls statements, p)
            _ -> do
                tok (TPunctuator PBraceOpen)
                varDecls <- many (try pVarDecl)
                statements <- many1 pStatement
                tok (TPunctuator PBraceClose) <?> "a closing brace"
                return (AST.FunDeclUntyped (identifier, p) args varDecls statements, p)
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
        t <- pPeek
        case t of
            TType Data.Token.TypeVoid -> do
                tok (TType Data.Token.TypeVoid)
                return (AST.FunTypeVoid types, p)
            _ -> do
                returnType <- pType <?> "a return type"
                return (AST.FunType types returnType, p)
    ) <?> "a function type"

pType :: Parser AST.Type
pType = pTypeBasic <|> pTypeTuple <|> pTypeList <|> pTypeIdentifier

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
    )) <?> "a basic type (int, bool, char)"

pTypeIdentifier :: Parser AST.Type
pTypeIdentifier = (do
    (identifier, p) <- pIdentifier
    return (AST.TypeIdentifier (identifier, p), p)) <?> "a type identifier"

pStatement :: Parser AST.Statement
pStatement = pStatementConditional <|> pStatementWhile <|> pStatementFunCallAssignment <|> pStatementReturn

pStatementConditional :: Parser AST.Statement
pStatementConditional =
    (do
        TP _ p <- tok (TKeyword KIf)
        condition <- pCondition
        statements <- pStatementBlock
        t <- pPeek
        case t of
            TKeyword KElse -> do
                tok (TKeyword KElse)
                statements' <- pStatementBlock
                return (AST.StmtIfElse condition statements statements', p)
            _ -> return (AST.StmtIf condition statements, p)
    ) <?> "an if or if-else statement"

pStatementWhile :: Parser AST.Statement
pStatementWhile =
    (do
        TP _ p <- tok (TKeyword KWhile)
        condition <- pCondition
        statements <- pStatementBlock
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

pStatementBlock :: Parser [AST.Statement]
pStatementBlock =
    (do
        tok (TPunctuator PBraceOpen)
        statements <- many pStatement
        tok (TPunctuator PBraceClose)
        return statements
    ) <?> "a statement block"

pStatementFunCallAssignment :: Parser AST.Statement
pStatementFunCallAssignment =
    (do
        (expr, p) <- pExpressionIdentifier
        case expr of
            AST.ExprIdentifier identifier -> (do
                tok (TOperator OAssignment)
                expr <- pExpression
                tok (TPunctuator PSeparator)
                return (AST.StmtAssignment identifier expr, p)) <?> "a variable assignment"
            AST.ExprIdentifierField identifier field -> (do
                tok (TOperator OAssignment)
                expr <- pExpression
                tok (TPunctuator PSeparator)
                return (AST.StmtAssignmentField identifier field expr, p)) <?> "a field variable assignment"
            AST.ExprFunCall identifier args -> (do
                tok (TPunctuator PSeparator)
                return (AST.StmtFunCall identifier args, p)) <?> "a function call"
    ) <?> "an assignment or function call"

pStatementReturn :: Parser AST.Statement
pStatementReturn = (do
    TP _ p <- tok (TKeyword KReturn)
    maybeExpr <- optionMaybe pExpression
    tok (TPunctuator PSeparator)
    case maybeExpr of
        Just expr -> return (AST.StmtReturn expr, p)
        Nothing -> return (AST.StmtReturnVoid, p)) <?> "a return statement"

-- TODO: associativity
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
{-
pExpression :: Parser AST.Expression
pExpression = pExpression' 1
    where
        pExpression' :: Int -> Parser AST.Expression
        pExpression' 8          = pExprBase
        pExpression' precedence = do {
            (expr, p) <- pExpression' (precedence + 1); (do
                binaryOperator <- try (lookAhead pBinaryOperator)
                if AST.binaryOperatorPrecedence binaryOperator == precedence
                    then do
                        pBinaryOperator -- consume binary operator
                        expr' <- pExpression
                        return (AST.ExprBinaryOp binaryOperator (expr, p) expr', p)
                    else return (expr, p)
            ) <|> return (expr, p)}
-}

pExprBase :: Parser AST.Expression
pExprBase = pExprGroupOrTuple
         <|> pExpressionConst
         <|> pExpressionIdentifier
         <|> pExpressionUnaryOperator

pExpressionIdentifier :: Parser AST.Expression
pExpressionIdentifier = do
    (identifier, p) <- pIdentifier
    t <- pPeek
    case t of
        (TField _) -> do
            field <- many1 pField
            return (AST.ExprIdentifierField (identifier, p) field, p)
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
            TP (TConstant (CBool b)) p -> Just $ (AST.ConstBool b, p)
            TP (TConstant (CInt i)) p -> Just $ (AST.ConstInt i, p)
            TP (TConstant (CChar c)) p -> Just $ (AST.ConstChar c, p)
            TP (TConstant CEmptyList) p -> Just $ (AST.ConstEmptyList, p)
            _ -> Nothing
    ) <?> "a constant"

pUnaryOperator :: Parser AST.UnaryOperator
pUnaryOperator = tokenPrim show advance
    (
        \tp -> case tp of
            TP (TOperator ONeg) p -> Just $ (AST.UnaryOpNeg, p)
            TP (TPunctuator PMinus) p -> Just $ (AST.UnaryOpSubtr, p)
            _ -> Nothing
    ) <?> "a unary operator"

pBinaryOperator :: Parser AST.BinaryOperator
pBinaryOperator = tokenPrim show advance
    (
        \tp -> case tp of
            TP (TOperator OOr) p -> Just $ (AST.BinaryOpOr, p)
            TP (TOperator OAnd) p -> Just $ (AST.BinaryOpAnd, p)
            TP (TOperator OEq) p -> Just $ (AST.BinaryOpEq, p)
            TP (TOperator ONEq) p -> Just $ (AST.BinaryOpNEq, p)
            TP (TOperator OLT) p -> Just $ (AST.BinaryOpLT, p)
            TP (TOperator OGT) p -> Just $ (AST.BinaryOpGT, p)
            TP (TOperator OLTE) p -> Just $ (AST.BinaryOpLTE, p)
            TP (TOperator OGTE) p -> Just $ (AST.BinaryOpGTE, p)
            TP (TOperator OConcat) p -> Just $ (AST.BinaryOpConcat, p)
            TP (TOperator OPlus) p -> Just $ (AST.BinaryOpPlus, p)
            TP (TPunctuator PMinus) p -> Just $ (AST.BinaryOpSubtr, p)
            TP (TOperator OMultiply) p -> Just $ (AST.BinaryOpMult, p)
            TP (TOperator ODivide) p -> Just $ (AST.BinaryOpDiv, p)
            TP (TOperator OMod) p -> Just $ (AST.BinaryOpMod, p)
            _ -> Nothing
    ) <?> "a binary operator"

pField :: Parser AST.Field
pField = tokenPrim show advance
    (
        \tp -> case tp of
            TP (TField FHd) p -> Just $ (AST.FieldHd, p)
            TP (TField FTl) p -> Just $ (AST.FieldTl, p)
            TP (TField FFst) p -> Just $ (AST.FieldFst, p)
            TP (TField FSnd) p -> Just $ (AST.FieldSnd, p)
            _ -> Nothing
    ) <?> "a field identifier"

pIdentifier :: Parser AST.Identifier
pIdentifier = tokenPrim show advance
    (
        \tp -> case tp of
            TP (TIdentifier identifier) p -> Just $ (AST.Identifier identifier, p)
            _ -> Nothing
    ) <?> "an identifier"

pSeparator :: Parser TokenP
pSeparator = tok (TPunctuator PSeparator)

parseTest :: (Show a) => Parser a -> [TokenP] -> IO ()
parseTest = Parsec.parseTest
