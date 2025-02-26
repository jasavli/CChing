"""
Za naprej:
Stringi in inti skupaj
Logicne operacije med stringi (in stevilkami)
"""



#Importi
from string_with_arrows import * 

import string


IDENTIFIKATOR = 'IDENTIFIKATOR'
KEYWORD = 'KEYWORD'
ENAKO = 'ENAKO'
DVOJNIENAKO = 'DVOJNIENAKO'
NIENAKO = 'NIENAKO'
MANJ = 'MANJ'
VEC = 'VEC'
MANJALIENAKO = 'MANJALIENAKO'
VECALIENAKO = 'VECALIENAKO'
INT = 'INT'
FLOAT = 'FLOAT'
STRING = 'STRING'
PLUS = 'PLUS'
MINUS = 'MINUS'
KRAT = 'KRAT'
DEL = 'DEL'
PROCENT = 'PROCENT'
POTENCA = 'POTENCA'
OKLEPAJ = 'OKLEPAJ'
ZAKLEPAJ = 'ZAKLEPAJ'
OGLATIOKLEPAJ = 'OGLATIOKLEPAJ'
OGLATIZAKLEPAJ = 'OGLATIZAKLEPAJ'
KONEC = 'KONEC'
VEJICA = 'VEJICA'
PUSCICA = 'PUSCICA'

KEYWORDS = [
    'VAR',
    'AND', 
    'OR', 
    'NOT',
    'IF',
    'THEN',
    'ELIF',
    'ELSE',
    'FOR',
    'TO',
    'STEP',
    'WHILE',
    'FUN'
]

#konstante
DIGITS = '0123456789'
LETTERS = string.ascii_letters
LETTERS_DIGITS = LETTERS + DIGITS 
#Tokeni
class Token:
    #konstruktor
    def __init__(self, type_, value = None, posStart = None, posEnd = None):
        self.type = type_
        self.value = value
        if posStart:  
            self.posStart = posStart.copy()
            self.posEnd = posStart.copy()
            self.posEnd.advance()

        if posEnd:  self.posEnd = posEnd.copy()
    #metoda za reprezentacijo objekta
    def __repr__(self):
        if self.value: return f'{self.type}:{self.value}'
        return f'{self.type}'
    
    def matches(self, tokenType, value):
        return self.type == tokenType and self.value == value
    
#Lexer
class Lexer:
    def __init__(self, fileName, text):    
        self.fileName = fileName   
        self.text = text
        self.pos = Position(-1, 0, -1, fileName, text)
        self.currentChar = None
        self.advance()
 
    def advance(self):
        self.pos.advance(self.currentChar)
        self.currentChar = self.text[self.pos.ind] if self.pos.ind < len(self.text) else None

    def makeTokens(self):
        tokens = []
        while self.currentChar != None:
            if self.currentChar in ' \t\n':
                self.advance()
                continue
            elif self.currentChar in DIGITS:
                tokens.append(self.makeNumber())
            elif self.currentChar in LETTERS:
                 tokens.append(self.makeIdentifier())
            elif self.currentChar == '"':
                tokens.append(self.makeString())
            elif self.currentChar == '+':
                tokens.append(Token(PLUS, posStart = self.pos))
                self.advance()
            elif self.currentChar == '-':
                tokens.append(self.makeMinusOrArrow())
            elif self.currentChar == '/':
                tokens.append(Token(DEL, posStart = self.pos))
                self.advance()
            elif self.currentChar == '^':
                tokens.append(Token(POTENCA, posStart = self.pos))
                self.advance()
            elif self.currentChar == '*':   
                tokens.append(Token(KRAT, posStart = self.pos))
                self.advance()
            elif self.currentChar == '%':   
                tokens.append(Token(PROCENT, posStart = self.pos))
                self.advance()
            elif self.currentChar == '(':
                tokens.append(Token(OKLEPAJ, posStart = self.pos))
                self.advance()
            elif self.currentChar == ')':
                tokens.append(Token(ZAKLEPAJ, posStart = self.pos))
                self.advance()
            elif self.currentChar == '[':
                tokens.append(Token(OGLATIOKLEPAJ, posStart = self.pos))
                self.advance()
            elif self.currentChar == ']':
                tokens.append(Token(OGLATIZAKLEPAJ, posStart = self.pos))
                self.advance()
            elif self.currentChar == '!':
                tok, error = self.makeNotEquals()
                if error: return [], error
                tokens.append(tok)
            elif self.currentChar == '=':
                tokens.append(self.makeEquals())
            elif self.currentChar == '<':
                tokens.append(self.makeLess())
            elif self.currentChar == '>':
                tokens.append(self.makeMore())
            elif self.currentChar == ',':
                tokens.append(Token(VEJICA, posStart = self.pos))
                self.advance()
            else:
                posStart = self.pos.copy()
                char = self.currentChar
                self.advance()
                return [], NotSupportedCharacterError(posStart, self.pos,"'" + char + "'")
        tokens.append(Token(KONEC, posStart = self.pos))
        return tokens, None
    

    def makeNumber(self):
        numStr = ''
        dotCount = 0
        posStart = self.pos.copy()
        while self.currentChar != None and self.currentChar in DIGITS + '.':
            if self.currentChar == '.':
                if dotCount == 1: break
                dotCount += 1
                numStr += '.'
            else:
                numStr += self.currentChar
            self.advance()
        if  dotCount == 0:
            return Token(INT, int(numStr), posStart, self.pos)
        else:
            return Token(FLOAT, float(numStr), posStart, self.pos)
        
    def makeString(self):
        string = ''
        posStart = self.pos.copy()
        escapeCharacter = False
        self.advance()

        escapeCharacters = {
            'n': '\n',
            't': '\t'
        }


        while self.currentChar != None and (self.currentChar != '"' or escapeCharacter):
            if escapeCharacter:
                string += escapeCharacters.get(self.currentChar, self.currentChar)
                escapeCharacter = False
            else:
                if self.currentChar == '\\':
                    escapeCharacter = True
                else:
                    string += self.currentChar
            self.advance()

        self.advance()
        return Token(STRING, string, posStart, self.pos)



    def makeIdentifier(self):
        idStr = ''
        posStart = self.pos.copy()

        while self.currentChar != None and self.currentChar in LETTERS_DIGITS + '_':
            idStr += self.currentChar
            self.advance() 
        
        tokType = KEYWORD if idStr in KEYWORDS else IDENTIFIKATOR
        return Token(tokType, idStr, posStart, self.pos)
    
    def makeMinusOrArrow(self):
        tokType = MINUS
        posStart = self.pos.copy()
        self.advance()

        if self.currentChar == '>':
            self.advance()
            tokType = PUSCICA

        return Token(tokType, posStart = posStart, posEnd = self.pos)



    def makeNotEquals(self):
        posStart = self.pos.copy()
        self.advance()

        if self.currentChar == '=':
            self.advance()
            return Token(NIENAKO, posStart = posStart, posEnd = self.pos), None
        self.advance()
        return None, ExpectedCharError(posStart, self.pos, "'=' (after !)")
    
    def makeEquals(self):
        tokType = ENAKO
        posStart = self.pos.copy()
        self.advance()
        
        if self.currentChar == "=":
            self.advance()
            tokType = DVOJNIENAKO
        return Token(tokType, posStart = posStart, posEnd = self.pos)

    def makeLess(self):
        tokType = MANJ
        posStart = self.pos.copy()
        self.advance()
        
        if self.currentChar == "=":
            self.advance()
            tokType = MANJALIENAKO
        return Token(tokType, posStart = posStart, posEnd = self.pos)

    def makeMore(self):
        tokType = VEC
        posStart = self.pos.copy()
        self.advance()
        
        if self.currentChar == "=":
            self.advance()
            tokType = VECALIENAKO
        return Token(tokType, posStart = posStart, posEnd = self.pos)


#Razred za napake
class Error:
    def __init__(self, posStart, posEnd, errorName, details):
        self.errorName = errorName
        self.details = details
        self.posStart = posStart
        self.posEnd = posEnd
    def as_string(self):
        result = f'{self.errorName}: {self.details}\n'
        result += f' File {self.posStart.fileName}, line {self.posStart.line + 1}'
        result += '\n\n' + string_with_arrows(self.posStart.fileText, self.posStart, self.posEnd)
        return result
class NotSupportedCharacterError(Error):
    def __init__(self, posStart, posEnd, details):
        super().__init__(posStart, posEnd, 'Character is not supported', details)

class InvalidSyntaxError(Error):
    def __init__(self, posStart, posEnd, details):
        super().__init__(posStart, posEnd, 'Invalid Syntax', details)

class ExpectedCharError(Error):
        def __init__(self, posStart, posEnd, details):
            super().__init__(posStart, posEnd, 'Expected character', details)

class RuntimeError(Error):
    def __init__(self, posStart, posEnd, details, context):
        super().__init__(posStart, posEnd, 'Runtime error', details)
        self.context = context
    def as_string(self):
        result = self.generateTraceback()
        result += f'{self.errorName}: {self.details}'
        result += '\n\n' + string_with_arrows(self.posStart.fileText, self.posStart, self.posEnd)
        return result
    def generateTraceback(self):
        result = ''
        pos = self.posStart
        context = self.context

        while context:
            result = f' File {pos.fileName}, line {str(pos.line + 1)}, in {context.displayName}\n' + result
            pos = context.parentEntryPos
            context = context.parent
        return 'Traceback: \n' + result
    
    


#Pozicija
class Position:
    def __init__(self, ind, line, col, fileName, fileText):
        self.ind = ind
        self.line = line
        self.col = col
        self.fileName = fileName
        self.fileText = fileText
    def advance(self, currentChar = None):
        self.ind += 1
        self.col += 1

        if currentChar == '\n':
            self.line += 1
            self.col = 0
        
        return self
    def copy(self):
        return Position(self.ind, self.line, self.col, self.fileName, self.fileText)
#Nodes
class NumberNode:
    def __init__(self, tok):
        self.tok = tok
        self.posStart = self.tok.posStart
        self.posEnd = self.tok.posEnd

    def __repr__(self):
        return f'{self.tok}'
    
class StringNode:
    def __init__(self, tok):
        self.tok = tok
        self.posStart = self.tok.posStart
        self.posEnd = self.tok.posEnd

    def __repr__(self):
        return f'{self.tok}'
    
class VarAccessNode:
    def __init__(self, varNameTok):
        self.varNameTok = varNameTok
        self.posStart = self.varNameTok.posStart
        self.posEnd = self.varNameTok.posEnd

class VarAssignNode:
    def __init__(self, varNameTok, valueNode):
        self.varNameTok = varNameTok
        self.valueNode = valueNode
        self.posStart = self.varNameTok.posStart
        self.posEnd = self.varNameTok.posEnd


class BinOpNode:
    def __init__(self, lNode, opTok, rNode):
        self.lNode = lNode
        self.opTok = opTok
        self.rNode = rNode
        self.posStart = self.lNode.posStart
        self.posEnd = self.rNode.posEnd 

    def __repr__(self):
        return f'({self.lNode}, {self.opTok}, {self.rNode})'

class UnaryOpNode:
    def __init__(self, opTok, node):
        self.opTok = opTok
        self.node = node
        self.posStart = self.opTok.posStart
        self.posEnd = node.posEnd

    def __repr__(self):
        return f'({self.opTok}, {self.node})'

class IfNode:
    def __init__(self, cases, elseCase):
        self.cases = cases
        self.elseCase = elseCase
        self.posStart = self.cases[0][0].posStart
        self.posEnd = self.elseCase or self.cases[len(self.cases) - 1][0].posEnd

class ForNode:
    def __init__(self, varNameTok, startValueNode, endValueNode, stepValueNode, bodyNode):
        self.varNameTok = varNameTok
        self.startValueNode = startValueNode
        self.endValueNode = endValueNode
        self.stepValueNode = stepValueNode
        self.bodyNode = bodyNode

        self.posStart = self.varNameTok.posStart
        self.posEnd = self.bodyNode.posEnd
class WhileNode:
    def __init__(self, conditionNode, bodyNode):
        self.conditionNode = conditionNode
        self.bodyNode = bodyNode

        self.posStart = self.conditionNode.posStart
        self.posEnd = self.bodyNode.posEnd 
class FuncDefNode:
    def __init__(self, varNameTok, argNameToks, bodyNode):
        self.varNameTok = varNameTok
        self.argNameToks = argNameToks
        self.bodyNode = bodyNode

        if self.varNameTok:
            self.posStart = self.varNameTok.posStart
        elif len(self.argNameToks)  > 0:
            self.posStart = self.argNameToks[0].posStart
        else:
            self.posStart = self.bodyNode.posStart
        
        self.posEnd = self.bodyNode.posEnd
class CallNode:
    def __init__(self, nodeToCall, argNodes):
        self.nodeToCall = nodeToCall
        self.argNodes = argNodes

        self.posStart = self.nodeToCall.posStart

        if len(self.argNodes) > 0:
            self.posEnd = self.argNodes[len(self.argNodes) - 1].posEnd
        else:
            self.posEnd = self.nodeToCall.posEnd
class StringConcatNode:
    def __init__(self, string_node, expr_node):
        self.string_node = string_node  
        self.expr_node = expr_node     
        self.posStart = self.string_node.posStart
        self.posEnd = self.expr_node.posEnd

    def __repr__(self):
        return f'(StringConcat: {self.string_node} + {self.expr_node})'
    
class ListNode:
    def __init__(self, elementNodes, posStart, posEnd):
        self.elementNodes = elementNodes
        self.posStart = posStart
        self.posEnd = posEnd
        


        

#Rezultat parserja
class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None
        self.advanceCount = 0

    def register(self, res):
        self.advanceCount += res.advanceCount
        if res.error: self.error = res.error 
        return res.node

    def registerAdvancement(self):
        self.advanceCount += 1
    
    def success(self, node):
        self.node = node
        return self


    def failure(self, error):
        if not self.error or self.advanceCount == 0:
            self.error = error
        return self
    

#Parser
class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.tokInd = -1
        self.advance()
        
    def advance(self):
        self.tokInd  += 1
        if self.tokInd < len(self.tokens):
            self.currentTok = self.tokens[self.tokInd]
        return self.currentTok
    
    def parse(self):
        res = self.expr()
        if not res.error and self.currentTok.type != KONEC: 
            return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, "Expected '+', '-', '*' or '/'"))
        return res

    def power(self):
        return self.binOp(self.call, (POTENCA,), self.factor)

    #enota
    def factor(self):
        res = ParseResult()
        tok = self.currentTok

        if tok.type in (PLUS, MINUS):
            res.registerAdvancement()
            self.advance()
            factor = res.register(self.factor())
            if res.error: return res
            else: return res.success(UnaryOpNode(tok, factor))
        
        return self.power()

    # factor *|/ factor
    def term(self):
        return self.binOp(self.factor, (KRAT, DEL, PROCENT))

    # term +|- term
    def expr(self):
        res = ParseResult()
        if self.currentTok.matches(KEYWORD, 'VAR'):
            res.registerAdvancement()
            self.advance()

            if self.currentTok.type != IDENTIFIKATOR:
                return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, "Expected identifier"))
            varName = self.currentTok
            res.registerAdvancement()
            self.advance()

            if self.currentTok.type != ENAKO:
                return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, "Expected '='"))
            res.registerAdvancement()
            self.advance()

            expr = res.register(self.expr())
            if res.error: return res
            return res.success(VarAssignNode(varName, expr))
        node = res.register(self.binOp(self.compExpr, ((KEYWORD, "AND"),(KEYWORD, "OR"))))

        if res.error: return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, "Expected 'VAR', 'IF', 'FOR', 'WHILE', int, float, identifier, '+', '-', or '('"))
        return res.success(node)

    def call(self):
        res = ParseResult()
        atom = res.register(self.atom())
        if res.error: return res

        if self.currentTok.type == OKLEPAJ:
            res.registerAdvancement()
            self.advance()
            argNodes = []

            if self.currentTok.type == ZAKLEPAJ:
                res.registerAdvancement()
                self.advance()
            else:
                argNodes.append(res.register(self.expr()))
                if res.error: return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, "Expected 'VAR', 'IF', 'FOR', 'WHILE', 'FUN', int, float, identifier, '+', '-', '(' or ')'"))
                
                while self.currentTok.type == VEJICA:
                    res.registerAdvancement()
                    self.advance()

                    argNodes.append(res.register(self.expr()))
                    if res.error: return res

                if self.currentTok.type != ZAKLEPAJ:
                    return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, "Expected ',' or ')'"))
                res.registerAdvancement()
                self.advance()
            return res.success(CallNode(atom, argNodes))
        return res.success(atom)

    

    #atom
    def atom(self):
        res = ParseResult()
        tok = self.currentTok 
        
        if tok.type in (INT, FLOAT):
            res.registerAdvancement()
            self.advance()
            return res.success(NumberNode(tok))
        elif tok.type in STRING:
            res.registerAdvancement()
            self.advance()

            if self.currentTok.type not in (PLUS, MINUS):
                return res.success(StringNode(tok))
            expr_node = res.register(self.arithExpr())
            if res.error: return res

            return res.success(StringConcatNode(StringNode(tok), expr_node))
        elif tok.type  == IDENTIFIKATOR:
            res.registerAdvancement()
            self.advance()
            return res.success(VarAccessNode(tok))
        elif tok.type == OKLEPAJ:
            res.registerAdvancement()
            self.advance()
            expr = res. register(self.expr())
            if res.error: return res
            if self.currentTok.type == ZAKLEPAJ:
                res.registerAdvancement()
                self.advance()
                return res.success(expr)
            else: return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, "Expected ')'"))
        elif tok.type == OGLATIOKLEPAJ:
            listExpr = res.register(self.listExpr())
            if res.error : return res
            return res.success(listExpr)
        
        elif tok.matches(KEYWORD, 'IF'):
            ifExpr = res.register(self.ifExpr())
            if res.error: return res
            return res.success(ifExpr)
        
        elif tok.matches(KEYWORD, 'FOR'):
            forExpr = res.register(self.forExpr())
            if res.error: return res
            return res.success(forExpr)
        
        elif tok.matches(KEYWORD, 'WHILE'):
            whileExpr = res.register(self.whileExpr())
            if res.error: return res
            return res.success(whileExpr)
        
        elif tok.matches(KEYWORD, 'FUN'):
            funcDef = res.register(self.funcDef())
            if res.error: return res
            return res.success(funcDef)

        return res.failure(InvalidSyntaxError(tok.posStart, tok.posEnd, "Expected 'VAR', 'IF', 'FOR', 'WHILE', 'FUN', int, float, identifier, '+', '-', '(', ')', '[' OR ']'"))
    

    def listExpr(self):
        res = ParseResult()
        elementNodes = []
        posStart = self.currentTok.posStart.copy()
        
        if self.currentTok.type != OGLATIOKLEPAJ:
            return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, "Expected '['"))
        res.registerAdvancement()
        self.advance()

        if self.currentTok.type == OGLATIZAKLEPAJ:
            res.registerAdvancement()
            self.advance()
        else:
            elementNodes.append(res.register(self.expr()))
            if res.error: return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, "Expected 'VAR', 'IF', 'FOR', 'WHILE', 'FUN', int, float, identifier, '+', '-', '(' or ')'"))
                
            while self.currentTok.type == VEJICA:
                res.registerAdvancement()
                self.advance()

                elementNodes.append(res.register(self.expr()))
                if res.error: return res

            if self.currentTok.type != OGLATIZAKLEPAJ:
                return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, "Expected ',' or ']'"))
            res.registerAdvancement()
            self.advance()
        return res.success(ListNode(elementNodes, posStart, self.currentTok.posEnd.copy()))


    def ifExpr(self):
        res = ParseResult()
        cases = []
        elseCase = None
        if not self.currentTok.matches(KEYWORD, 'IF'):
            return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, f"Expected 'IF'"))
        res.registerAdvancement()
        self.advance()

        condition = res.register(self.expr())
        if res.error: return res

        if not self.currentTok.matches(KEYWORD, 'THEN'):
            return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, f"Expected 'THEN'"))
        res.registerAdvancement()
        self.advance()

        expr = res.register(self.expr())
        if res.error: return res
        cases.append((condition, expr))

        

        while self.currentTok.matches(KEYWORD, 'ELIF'):
            res.registerAdvancement()
            self.advance()

            condition = res.register(self.expr())
            if res.error: return res

            if not self.currentTok.matches(KEYWORD, 'THEN'):
                return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, f"Expected 'THEN'"))
            
            res.registerAdvancement()
            self.advance()

            expr = res.register(self.expr())
            if res.error: return res
            cases.append((condition, expr))

        if self.currentTok.matches(KEYWORD, 'ELSE'):
            res.registerAdvancement()
            self.advance()

            elseCase = res.register(self.expr())
            if res.error: return res

        return res.success(IfNode(cases, elseCase))

    def forExpr(self):
        res = ParseResult()

        if not self.currentTok.matches(KEYWORD, 'FOR'):
            return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, f"Expected 'FOR'"))
        res.registerAdvancement()
        self.advance()

        if self.currentTok.type != IDENTIFIKATOR:
            return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, f"Expected identifier"))

        varName = self.currentTok
        res.registerAdvancement()
        self.advance()

        if self.currentTok.type != ENAKO:
            return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, f"Expected '='"))

        res.registerAdvancement()
        self.advance()

        startValue = res.register(self.expr())
        if res.error: return res

        if not self.currentTok.matches(KEYWORD, 'TO'):
            return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, f"Expected 'TO'"))
        res.registerAdvancement()
        self.advance()

        endValue = res.register(self.expr())
        if res.error: return res

        if self.currentTok.matches(KEYWORD, 'STEP'):
            res.registerAdvancement()
            self.advance()

            stepValue = res.register(self.expr())
            if res.error: return res
        else:
            stepValue = None
        
        if not self.currentTok.matches(KEYWORD, 'THEN'):
            return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, f"Expected 'THEN'"))
        res.registerAdvancement()
        self.advance()

        body = res.register(self.expr())
        if res.error: return res
        return res.success(ForNode(varName, startValue, endValue, stepValue, body))
    
    def whileExpr(self):
        res = ParseResult()

        if not self.currentTok.matches(KEYWORD, 'WHILE'):
            return res.failure(self.currentTok.posStart, self.currentTok.posEnd, f"Expected 'WHILE'")
        res.registerAdvancement()
        self.advance()

        condition = res.register(self.expr())
        if res.error: return res

        if not self.currentTok.matches(KEYWORD, 'THEN'):
            return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, f"Expected 'THEN'"))
        
        res.registerAdvancement()
        self.advance()

        body = res.register(self.expr())
        if res.error: return res

        return res.success(WhileNode(condition, body))

    





    def funcDef(self):
        res = ParseResult()

        if not self.currentTok.matches(KEYWORD, 'FUN'):
            return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, f"Expected 'FUN'"))
        
        res.registerAdvancement()
        self.advance()


        if self.currentTok.type == IDENTIFIKATOR:
            varNameTok = self.currentTok
            res.registerAdvancement()
            self.advance()
            if self.currentTok.type != OKLEPAJ:
                return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, f"Expected '('"))
        else:
            varNameTok = None
            if self.currentTok.type != OKLEPAJ:
                return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, f"Expected identifier or '('"))
        res.registerAdvancement()
        self.advance()

        argNameToks = []
        if self.currentTok.type == IDENTIFIKATOR:
            argNameToks.append(self.currentTok)
            res.registerAdvancement()
            self.advance()

            while self.currentTok.type == VEJICA:
                res.registerAdvancement()
                self.advance()

                if self.currentTok.type != IDENTIFIKATOR:
                    return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, f"Expected identifier"))
                argNameToks.append(self.currentTok)
                res.registerAdvancement()
                self.advance()

            if self.currentTok.type != ZAKLEPAJ:
                    return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, f"Expected ',' ')'"))
        else: 
            if self.currentTok.type != ZAKLEPAJ:
                    return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, f"Expected ',' ')'"))
        res.registerAdvancement()
        self.advance()

        if self.currentTok.type != PUSCICA:
            return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, f"Expected '->'"))
        res.registerAdvancement()
        self.advance()
        nodeToReturn = res.register(self.expr())
        if res.error: return res

        return res.success(FuncDefNode(varNameTok, argNameToks, nodeToReturn))




    def compExpr(self):
        res = ParseResult()

        if self.currentTok.matches(KEYWORD, 'NOT'):
            opTok = self.currentTok
            res.registerAdvancement()
            self.advance()

            node = res.register(self.compExpr())
            if res.error: return res
            return res.success(UnaryOpNode(opTok, node))

        node = res.register(self.binOp(self.arithExpr, (DVOJNIENAKO, NIENAKO, VEC, MANJ, VECALIENAKO, MANJALIENAKO)))
        if res.error: return res.failure(InvalidSyntaxError(self.currentTok.posStart, self.currentTok.posEnd, "Expected int, float, identifier, '+', '-', '(' or 'NOT'"))
        return res.success(node)
    def arithExpr(self):
        return self.binOp(self.term, (PLUS, MINUS))
    
    def binOp(self, func_a, ops, func_b = None):
        if func_b == None: func_b = func_a
        res = ParseResult()
        left = res.register(func_a())
        if res.error: return res 

        while self.currentTok.type in ops or (self.currentTok.type, self.currentTok.value) in ops:
            opTok = self.currentTok
            res.registerAdvancement()
            self.advance()
            right = res.register(func_b())
            if res.error: return res
            left = BinOpNode(left, opTok, right)
            
        return res.success(left)
    




#runtime rezultat
class RTResult:
    def __init__(self):
        self.value = None
        self.error = None

    def register(self, res):
        if res.error: self.error = res.error
        return res.value
    
    def success(self, value):
        self.value = value
        return self
    
    def failure(self, error):
        self.error = error
        return self


#vrednosti
class Value:
	def __init__(self):
		self.setPos()
		self.setContext()

	def setPos(self, posStart=None, posEnd=None):
		self.posStart = posStart
		self.posEnd = posEnd
		return self

	def setContext(self, context=None):
		self.context = context
		return self

	def addedTo(self, other):
		return None, self.illegalOperation(other)

	def subtractedBy(self, other):
		return None, self.illegalOperation(other)

	def multiplyBy(self, other):
		return None, self.illegalOperation(other)

	def divideBy(self, other):
		return None, self.illegalOperation(other)

	def powerBy(self, other):
		return None, self.illegalOperation(other)

	def getComparisonEqual(self, other):
		return None, self.illegalOperation(other)

	def getComparisonNotEqual(self, other):
		return None, self.illegalOperation(other)

	def getComparisonLess(self, other):
		return None, self.illegalOperation(other)

	def getComparisonMore(self, other):
		return None, self.illegalOperation(other)

	def getComparisonLessOrEqual(self, other):
		return None, self.illegalOperation(other)

	def getComparisonMoreOrEqual(self, other):
		return None, self.illegalOperation(other)

	def andBy(self, other):
		return None, self.illegalOperation(other)

	def orBy(self, other):
		return None, self.illegalOperation(other)

	def notted(self, other):
		return None, self.illegalOperation(other)

	def execute(self, args):
		return RTResult().failure(self.illegalOperation())

	def copy(self):
		raise Exception('No copy method defined')

	def is_true(self):
		return False

	def illegalOperation(self, other=None):
		if not other: other = self
		return RuntimeError(
			self.posStart, other.posEnd,
			'Illegal operation',
			self.context
		)
        


class Number(Value):
    def __init__(self, value):
        self.value = value
        self.setPos()
        self.setContext()

    def setPos(self, posStart = None, posEnd = None):
        self.posStart = posStart
        self.posEnd = posEnd
        return self
    def setContext(self, context = None):
        self.context = context
        return self

    def addedTo(self, other):
        if isinstance(other, Number):
            return Number(self.value + other.value).setContext(self.context), None
        elif isinstance(other, String):
            # Če je drugi operand (niz) v celoti številski, seštejemo številke…
            try:
                otherNumeric = float(other.value)
                result = self.value + otherNumeric
                if result.is_integer():
                    result = int(result)
                # Vrni številko (lahko bi jo tudi pretvorili v niz, odvisno od želene semantike)
                return Number(result).setContext(self.context), None
            except ValueError:
                # Drugače pa pretvorimo prvo število v niz in združimo
                return String(str(self.value) + other.value).setContext(self.context), None
        else:
            return None, Value.illegalOperation(self, other)
    def subtractedBy(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value).setContext(self.context), None
        else: 
            return None, Value.illegalOperation(self, other)
    def multiplyBy(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value).setContext(self.context), None
        else: 
            return None, Value.illegalOperation(self, other)
    def divideBy(self, other):
        if isinstance(other, Number):
            if other.value == 0: return None, RuntimeError(other.posStart, other.posEnd, "Division by zero", self.context)
            return Number(self.value / other.value).setContext(self.context), None
        else: 
            return None, Value.illegalOperation(self, other)
    def powerBy(self, other):
        if isinstance(other, Number):
            return Number(self.value ** other.value).setContext(self.context), None
        else: 
            return None, Value.illegalOperation(self, other)
    def moduloBy(self, other):
        if isinstance(other, Number):
            return Number(self.value % other.value).setContext(self.context), None
        else: 
            return None, Value.illegalOperation(self, other)
    def getComparisonEqual(self, other):
        if isinstance(other, Number):
            return Number(int(self.value == other.value)).setContext(self.context), None
        else: 
            return None, Value.illegalOperation(self, other)
    def getComparisonNotEqual(self, other):
        if isinstance(other, Number):
            return Number(int(self.value != other.value)).setContext(self.context), None
        else: 
            return None, Value.illegalOperation(self, other)
    def getComparisonLess(self, other):
        if isinstance(other, Number):
            return Number(int(self.value < other.value)).setContext(self.context), None
        else: 
            return None, Value.illegalOperation(self, other)
    def getComparisonMore(self, other):
        if isinstance(other, Number):
            return Number(int(self.value > other.value)).setContext(self.context), None
        else: 
            return None, Value.illegalOperation(self, other)
    def getComparisonLessOrEqual(self, other):
        if isinstance(other, Number):
            return Number(int(self.value <= other.value)).setContext(self.context), None
        else: 
            return None, Value.illegalOperation(self, other)
    def getComparisonMoreOrEqual(self, other):
        if isinstance(other, Number):
            return Number(int(self.value >= other.value)).setContext(self.context), None
        else: 
            return None, Value.illegalOperation(self, other)
    def andBy(self, other):
        if isinstance(other, Number):
            return Number(int(self.value and other.value)).setContext(self.context), None
        else: 
            return None, Value.illegalOperation(self, other)
    def orBy(self, other):
        if isinstance(other, Number):
            return Number(int(self.value or other.value)).setContext(self.context), None
        else: 
            return None, Value.illegalOperation(self, other)
    def notted(self):
        return Number(1 if self.value == 0 else 0). setContext(self.context), None

    def copy(self):
        copy = Number(self.value)
        copy.setPos(self.posStart,self.posEnd)
        copy.setContext(self.context)
        return copy
    
    def isTrue(self):
        return self.value != 0



    def __repr__(self):
        return str(self.value)
    
class Function(Value):
    def __init__(self, name, bodyNode, argNames):
        super().__init__()
        self.name = name or "<anonymous>"
        self.bodyNode = bodyNode
        self.argNames = argNames
    def execute(self, args):
        res = RTResult()

        interpreter = Interpreter()
        newContext = Context(self.name, self.context, self.posStart)
        newContext.symbolTable = SymbolTable(newContext.parent.symbolTable)

        if len(args) > len(self.argNames):
            return res.failure(RuntimeError(self.posStart, self.posEnd, f"{len(args) - len(self.argNames)} too many argy passed into '{self.name}'", self.context))
        if len(args) < len(self.argNames):
            return res.failure(RuntimeError(self.posStart, self.posEnd, f"{len(self.argNames) - len(self.args)} too few argy passed into '{self.name}'", self.context))
        
        for i in range(len(args)):
            argName = self.argNames[i]
            argValue = args[i]
            argValue.setContext(newContext)
            newContext.symbolTable.set(argName, argValue )
        value = res.register(interpreter.visit(self.bodyNode, newContext))
        if res.error: return res
        return res.success(value)
    
    def copy(self):
        copy = Function(self.name, self.bodyNode, self.argNames)
        copy.setContext(self.context)
        copy.setPos(self.posStart, self.posEnd)
        return copy
    def __repr__(self):
        return f"<function {self.name}>"

class String(Value):
    def __init__(self, value):
        super().__init__()
        self.value = value

    def isNumeric(self):
        try:
            float(self.value)
            return True
        except ValueError:
            return False

    def addedTo(self, other):
        if isinstance(other, String):
            return String(self.value + other.value).setContext(self.context), None
        elif isinstance(other, Number):
            if self.isNumeric():
                result = float(self.value) + other.value
                if result.is_integer():
                    result = int(result)
                return Number(result).setContext(self.context), None
            else:
                return String(self.value + str(other.value)).setContext(self.context), None
        else:
            return None, Value.illegalOperation(self, other)
    
    def multiplyBy(self, other):
        if isinstance(other, Number):
            return String(self.value * other.value).setContext(self.context), None
        else:
            return None, Value.illegalOperation(self, other)
        
    def getComparisonEqual(self, other):
        if isinstance(other, String):
            return Number(int(self.value == other.value)).setContext(self.context), None
        else: 
            return None, Value.illegalOperation(self, other)
    def getComparisonNotEqual(self, other):
        if isinstance(other, String):
            return Number(int(self.value != other.value)).setContext(self.context), None
        else: 
            return None, Value.illegalOperation(self, other)
    def getComparisonLess(self, other):
        if isinstance(other, String):
            return Number(int(len(self.value) < len(other.value))).setContext(self.context), None
        else: 
            return None, Value.illegalOperation(self, other)
    def getComparisonMore(self, other):
        if isinstance(other, String):
            return Number(int(len(self.value) > len(other.value))).setContext(self.context), None
        else: 
            return None, Value.illegalOperation(self, other)
    def getComparisonLessOrEqual(self, other):
        if isinstance(other, String):
            return Number(int(len(self.value) <= len(other.value))).setContext(self.context), None
        else: 
            return None, Value.illegalOperation(self, other)
    def getComparisonMoreOrEqual(self, other):
        if isinstance(other, String):
            return Number(int(len(self.value) >= len(other.value))).setContext(self.context), None
        else: 
            return None, Value.illegalOperation(self, other)



    def is_true(self):
        return len(self.value) > 0

    def copy(self):
        copy = String(self.value)
        copy.setPos(self.posStart, self.posEnd)
        copy.setContext(self.context)
        return copy

    def __repr__(self):
        return f'"{self.value}"'

class List(Value):
    def __init__(self, elements):
        super().__init__()
        self.elements = elements
    
    def addedTo(self, other):
        newList = self.copy()
        newList.elements.append(other)
        return newList, None
    
    def subtractedBy(self, other):
        if isinstance(other, Number):
            newList = self.copy()
            try:
                newList.elements.pop(other.value)
                return newList, None
            except:
                return None, RuntimeError(other.posStart, other.posEnd, "Index is out of bounds", self.context)
        else:
            return None, Value.illegalOperation(self, other)

    def multiplyBy(self, other):
        if isinstance(other, List):
            newList = self.copy()
            newList.elements.extend(other.elements)
            return newList, None
        else:
            return None, Value.illegalOperation(self, other)
    
    def divideBy(self, other):
        if isinstance(other, Number):
            try:
                return self.elements[other.value], None
            except:
                return None, RuntimeError(other.posStart, other.posEnd, "Index is out of bounds", self.context)
        else:
            return None, Value.illegalOperation(self, other)

    def copy(self):
        copy = List(self.elements[:])
        copy.setPos(self.posStart, self.posEnd)
        copy.setContext(self.context)
        return copy
    def __repr__(self):
        return f'[{", ".join([str(x) for x in self.elements])}]'




#Bolj podrobni error
class Context:
    def __init__(self, displayName, parent = None, parentEntryPos = None):
        self.displayName = displayName
        self.parent = parent
        self.parentEntryPos = parentEntryPos
        self.symbolTable = None
         
#Shranjevanje spremenljivk
class SymbolTable:
    def __init__(self, parent = None):
        self.symbols = {}
        self.parent = parent 
    def get(self, name):
        value = self.symbols.get(name, None)
        if value == None and self.parent:
            return self.parent.get(name)
        return value
    def set(self, name, value):
        self.symbols[name] = value   

    def remove(self, name):
        del self.symbols[name]

       



#interpreter
class Interpreter:
    def visit(self, node, context):
        methodName = f'visit{type(node).__name__}'
        method = getattr(self, methodName, self.noVisitMethod)
        return method(node, context)
    def noVisitMethod(self, node, context):
        raise Exception(f'No visit{type(node).__name__} method defined')
    
    def visitNumberNode(self, node, context):
        return RTResult().success(Number(node.tok.value).setContext(context).setPos(node.posStart, node.posEnd))

    def visitStringNode(self, node, context):
        return RTResult().success(String(node.tok.value).setContext(context).setPos(node.posStart, node.posEnd))
        

    def visitBinOpNode(self, node, context):  
        res = RTResult()
        left = res.register(self.visit(node.lNode, context))
        if res.error: return res
        right = res.register(self.visit(node.rNode, context))

        if node.opTok.type == PLUS:
            result, error = left.addedTo(right)
        elif node.opTok.type == MINUS:
            result, error = left.subtractedBy(right)
        elif node.opTok.type == KRAT:
            result, error = left.multiplyBy(right)
        elif node.opTok.type == DEL:
            result, error = left.divideBy(right)
        elif node.opTok.type == POTENCA:
            result, error = left.powerBy(right)
        elif node.opTok.type == PROCENT:
            result, error = left.moduloBy(right)
        elif node.opTok.type == DVOJNIENAKO:
            result, error = left.getComparisonEqual(right)
        elif node.opTok.type == NIENAKO:
            result, error = left.getComparisonNotEqual(right)
        elif node.opTok.type == VEC:
            result, error = left.getComparisonMore(right)
        elif node.opTok.type == MANJ:
            result, error = left.getComparisonLess(right)
        elif node.opTok.type == VECALIENAKO:
            result, error = left.getComparisonMoreOrEqual(right)
        elif node.opTok.type == MANJALIENAKO:
            result, error = left.getComparisonLessOrEqual(right)
        elif node.opTok.matches(KEYWORD, 'AND'):
            result, error = left.andBy(right)
        elif node.opTok.matches(KEYWORD, 'OR'):
            result, error = left.orBy(right)

        if error: return res.failure(error)
        else: return res.success(result.setPos(node.posStart, node.posEnd))

    def visitUnaryOpNode(self, node, context):
        res = RTResult()
        number = res.register(self.visit(node.node, context))
        if res.error: return res

        error = None
        if node.opTok.type == MINUS: 
            number, error = number.multiplyBy(Number(-1))
        elif node.opTok.matches(KEYWORD, 'NOT'):
            number,error = number.notted()

        if error: return res.failure(error)
        else: return res.success(number.setPos(node.posStart, node.posEnd))

    def visitVarAccessNode(self, node, context):
        res = RTResult()
        varName = node.varNameTok.value
        value = context.symbolTable.get(varName)

        if not value:
            return res.failure(RuntimeError(node.posStart, node.posEnd, f"'{varName}' is not defined"))

        value = value.copy().setPos(node.posStart, node.posEnd)
        return res.success(value)
    
    def visitVarAssignNode(self, node, context):
        res = RTResult()
        varName = node.varNameTok.value
        value = res.register(self.visit(node.valueNode, context))
        if res.error: return res

        context.symbolTable.set(varName, value)
        return res.success(value)
    
    def visitIfNode(self, node, context):
        res = RTResult()

        for condition, expr in node.cases:
            conditionValue = res.register(self.visit(condition, context))
            if res.error: return res
            
            if conditionValue.isTrue():
                exprValue = res.register(self.visit(expr, context))
                if res.error: return res
                return res.success(exprValue)
        if node.elseCase:
            elseValue = res.register(self.visit(node.elseCase, context))
            if res.error: return res
            return res.success(elseValue)
        return res.success(None)
    
    def visitForNode(self, node, context):
        res = RTResult()
        elements = []

        startValue = res.register(self.visit(node.startValueNode, context))
        if res.error: return res

        endValue = res.register(self.visit(node.endValueNode, context))
        if res.error: return res

        if node.stepValueNode:
            stepValue = res.register(self.visit(node.stepValueNode, context))
            if res.error: return res
        else:
            stepValue = Number(1)
        
        i = startValue.value

        if stepValue.value >= 0:
            condition = lambda: i < endValue.value
        else:
            condition = lambda: i > endValue.value

        while condition():
            context.symbolTable.set(node.varNameTok.value, Number(i))
            i += stepValue.value

            elements.append(res.register(self.visit(node.bodyNode, context)))
            if res.error: return res

        return res.success(List(elements).setContext(context).setPos(node.posStart, node.posEnd))

    def visitWhileNode(self, node, context):
        res = RTResult()
        elements = []

        while True:
            condition = res.register(self.visit(node.conditionNode, context))
            if res.error: return res

            if not condition.isTrue(): break

            elements.append(res.register(self.visit(node.bodyNode, context)))
            if res.error: return res
        return res.success(List(elements).setContext(context).setPos(node.posStart, node.posEnd))
    
    def visitFuncDefNode(self, node, context):
        res = RTResult()

        funcName = node.varNameTok.value if node.varNameTok else None
        bodyNode = node.bodyNode
        argNames = [argName.value for argName in node.argNameToks]
        funcValue = Function(funcName, bodyNode, argNames).setContext(context).setPos(node.posStart, node.posEnd)

        if node.varNameTok:
            context.symbolTable.set(funcName, funcValue)
        
        return res.success(funcValue)
    
    def visitCallNode(self, node, context):
        res = RTResult()
        args = []

        valueToCall = res.register(self.visit(node.nodeToCall, context))
        if res.error: return res
        valueToCall = valueToCall.copy().setPos(node.posStart, node.posEnd)

        for argNode in node.argNodes:
            args.append(res.register(self.visit(argNode, context)))
            if res.error: return res

        returnvalue = res.register(valueToCall.execute(args))
        if res.error: return res

        return res.success(returnvalue)
    def visitStringConcatNode(self, node, context):
        res = RTResult()
        left = res.register(self.visit(node.string_node, context)) 
        if res.error: return res
        right = res.register(self.visit(node.expr_node, context))  
        if res.error: return res
        result = String(left.value + str(right.value)).setContext(context)
        return res.success(result.setPos(node.posStart, node.posEnd))
    
    def visitListNode(self, node, context):
        res = RTResult()
        elements = []

        for elementNode in node.elementNodes:
            elements.append(res.register(self.visit(elementNode, context)))
            if res.error: return res

        return res.success(List(elements).setContext(context).setPos(node.posStart, node.posEnd))

#Run
globalSymbolTable = SymbolTable()
globalSymbolTable.set("null", Number(0))

def run(fileName, text):
    lexer = Lexer(fileName, text)
    tokens, error = lexer.makeTokens()
    if error: return None, error

    parser = Parser(tokens)
    #abstract sintax tree
    ast = parser.parse()
    if ast.error: return None, ast.error

    #zaženi program
    interpreter = Interpreter()
    context = Context("<program>")
    context.symbolTable = globalSymbolTable
    result = interpreter.visit(ast.node, context)


    return result.value, result.error
