INT = 'INT'
FLOAT = 'FLOAT'
PLUS = 'PLUS'
MINUS = 'MINUS'
MUL = 'MUL'
DIV = 'DIV'
OKLEPAJ = 'OKLEPAJ'
ZAKLEPAJ = 'ZAKLEPAJ'
#konstanta za preverjanje števk
DIGITS = '0123456789'
#Tokeni
class Token:
    #konstruktor
    def __init__(self, type_, value = None):
        self.type = type_
        self.value = value
    #metoda za reprezentacijo objekta
    def __repr__(self):
        if self.value: return f'{self.type}:{self.value}'
        return f'{self.type}'
    
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
            elif self.currentChar == '+':
                tokens.append(Token(PLUS))
                self.advance()
            elif self.currentChar == '-':
                tokens.append(Token(MINUS))
                self.advance()
            elif self.currentChar == '/':
                tokens.append(Token(DIV))
                self.advance()
            elif self.currentChar == '*':   
                tokens.append(Token(MUL))
                self.advance()
            elif self.currentChar == '(':
                tokens.append(Token(OKLEPAJ))
                self.advance()
            elif self.currentChar == ')':
                tokens.append(Token(ZAKLEPAJ))
                self.advance()
            else:
                posStart = self.pos.copy()
                char = self.currentChar
                self.advance()
                return [], NotSupportedCharacterError(posStart, self.pos,"'" + char + "'")
        return tokens, None
    

    def makeNumber(self):
        numStr = ''
        dotCount = 0
        while self.currentChar != None and self.currentChar in DIGITS + '.':
            if self.currentChar == '.':
                if dotCount == 1: break
                dotCount += 1
                numStr += '.'
            else:
                numStr += self.currentChar
            self.advance()
        if  dotCount == 0:
            return Token(INT, int(numStr))
        else:
            return Token(FLOAT, float(numStr))
#Razred za napake
class Error:
    def __init__(self, posStart, posEnd, errorName, details):
        self.errorName = errorName
        self.details = details
        self.posStart = posStart
        self.posEnd = posEnd
    def as_string(self):
        result = f'{self.errorName}: {self.details}'
        result += f' File {self.posStart.fileName}, line {self.posStart.line + 1}'
        return result
class NotSupportedCharacterError(Error):
    def __init__(self, posStart, posEnd, details):
        super().__init__(posStart, posEnd, 'Character is not supported', details)
#Pozicija
class Position:
    def __init__(self, ind, line, col, fileName, fileText):
        self.ind = ind
        self.line = line
        self.col = col
        self.fileName = fileName
        self.fileText = fileText
    def advance(self, currentChar):
        self.ind += 1
        self.col += 1

        if currentChar == '\n':
            self.line += 1
            self.col = 0
        
        return self
    def copy(self):
        return Position(self.ind, self.line, self.col, self.fileName, self.fileText)
        
#Run
def run(fileName, text):
    lexer = Lexer(fileName, text)
    tokens, error = lexer.makeTokens()

    return tokens, error