"""
Za naprej:
Runtime error context!!!
Izboljšanje napak!
"""



#Importi
from string_with_arrows import * 

import string


IDENTIFIKATOR = 'IDENTIFIKATOR'
KEYWORD = 'KEYWORD'
ENAKO = 'ENAKO'
INT = 'INT'
FLOAT = 'FLOAT'
PLUS = 'PLUS'
MINUS = 'MINUS'
KRAT = 'KRAT'
DEL = 'DEL'
POTENCA = 'POTENCA'
OKLEPAJ = 'OKLEPAJ'
ZAKLEPAJ = 'ZAKLEPAJ'
KONEC = 'KONEC'

KEYWORDS = [
    'VAR'
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
            elif self.currentChar == '+':
                tokens.append(Token(PLUS, posStart = self.pos))
                self.advance()
            elif self.currentChar == '-':
                tokens.append(Token(MINUS, posStart = self.pos))
                self.advance()
            elif self.currentChar == '/':
                tokens.append(Token(DEL, posStart = self.pos))
                self.advance()
            elif self.currentChar == '^':
                tokens.append(Token(POTENCA, posStart = self.pos))
                self.advance()
            elif self.currentChar == '=':
                tokens.append(Token(ENAKO, posStart = self.pos))
                self.advance()
            elif self.currentChar == '*':   
                tokens.append(Token(KRAT, posStart = self.pos))
                self.advance()
            elif self.currentChar == '(':
                tokens.append(Token(OKLEPAJ, posStart = self.pos))
                self.advance()
            elif self.currentChar == ')':
                tokens.append(Token(ZAKLEPAJ, posStart = self.pos))
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
        
    def makeIdentifier(self):
        idStr = ''
        posStart = self.pos.copy()

        while self.currentChar != None and self.currentChar in LETTERS_DIGITS + '_':
            idStr += self.currentChar
            self.advance() 
        
        tokType = KEYWORD if idStr in KEYWORDS else IDENTIFIKATOR
        return Token(tokType, idStr, posStart, self.pos)



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
        return self.binOp(self.atom, (POTENCA,), self.factor)

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
        return self.binOp(self.factor, (KRAT, DEL))

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
        node = res.register(self.binOp(self.term, (PLUS, MINUS)))

        if res.error: return res.failure(self.currentTok.posStart, self.currentTok.posEnd, "Expected 'VAR', int, float, identifier, '+', '-', or '('")
        return res.success(node)

    #atom
    def atom(self):
        res = ParseResult()
        tok = self.currentTok
        
        if tok.type in (INT, FLOAT):
            res.registerAdvancement()
            self.advance()
            return res.success(NumberNode(tok))
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
        return res.failure(InvalidSyntaxError(tok.posStart, tok.posEnd, "Expected int, float, identifier, '+', '-' or '('"))
    
    
    def binOp(self, func_a, ops, func_b = None):
        if func_b == None: func_b = func_a
        res = ParseResult()
        left = res.register(func_a())
        if res.error: return res 

        while self.currentTok.type in ops:
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
class Number:
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
        
    def subtractedBy(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value).setContext(self.context), None
        
    def multiplyBy(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value).setContext(self.context), None
        
    def divideBy(self, other):
        if isinstance(other, Number):
            if other.value == 0: return None, RuntimeError(other.posStart, other.posEnd, "Division by zero", self.context)
            return Number(self.value / other.value).setContext(self.context), None

    def powerBy(self, other):
        if isinstance(other, Number):
            return Number(self.value ** other.value).setContext(self.context), None
        
    def copy(self):
        copy = Number(self.value)
        copy.setPos(self.posStart,self.posEnd)
        copy.setContext(self.context)
        return copy


    def __repr__(self):
        return str(self.value)
    
#Bolj podrobni error
class Context:
    def __init__(self, displayName, parent = None, parentEntryPos = None):
        self.displayName = displayName
        self.parent = parent
        self.parentEntryPos = parentEntryPos
        self.symbolTable = None
         
class SymbolTable:
    def __init__(self):
        self.symbols = {}
        self.parent = None 
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

        if error: return res.failure(error)
        else: return res.success(result.setPos(node.posStart, node.posEnd))

    def visitUnaryOpNode(self, node, context):
        res = RTResult()
        number = res.register(self.visit(node.node, context))
        if res.error: return res

        error = None
        if node.opTok.type == MINUS: 
            number, error = number.multiplyBy(Number(-1))

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
