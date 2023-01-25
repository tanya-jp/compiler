# -----------------------------------------------------------------------------
# calc.py
#
# A simple calculator with variables -- all in one file.
# -----------------------------------------------------------------------------

reserved = {
    'if' : 'IF',
    'then' : 'THEN',
    'else' : 'ELSE',
    'while' : 'WHILE',
    'do':'DO',
    'program' : 'PROGRAM',
    'var':'VAR',
    'int':'INT',
    'real':'REAL',
    'begin':'BEGIN',
    'end':'END',
    'print': 'PRINT',
    'switch':'SWITCH',
    'of':'OF',
    'done':'DONE',
    'default':'DEFAULT',
    'mod':'MOD',
    'and': 'AND',
    'or': 'OR',
    'not': 'NOT'
 }

tokens = [
             'TRUE', 'FALSE',
             'LPAREN', 'RPAREN',
             'PLUS',
             'MINUS',
             'TIMES',
             'DIVIDE',
             'INTEGERCONSTANT',
             'REALCONSTANT',
             'ID',
             'EQ',
             'LT',
             'LTEQ',
             'GT',
             'GTEQ',
             'NEQ',
             'SEMICOLON',
             'COLON',
             'COMMA',
             'ASSIGN',
             'UMINUS'
         ] + list(keywords.values())

# Tokens
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = keywords.get(t.value, 'ID')
    return t


def t_INTEGERCONSTANT(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_REALCONSTANT(t):
    r'[+-]?([0-9]*[.])?[0-9]+'
    t.value = float(t.value)
    return t

t_OR = r'or'
t_AND = r'and'
t_NOT = r'not'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_TRUE = r'true'
t_FALSE = r'false'
t_PLUS = r'\+'
t_MINUS = r'\-'
t_DIVIDE = r'\/'
t_TIMES = r'\*'
t_EQ = r'\='
t_LT = r'\<'
t_LTEQ = r'\<='
t_NEQ = r'\<>'
t_GT = r'\>'
t_GTEQ = r'\>='
t_SEMICOLON = r'\;'
t_COLON = r'\:'
t_COMMA = r'\,'
t_ASSIGN = r'\:='
t_UMINUS = r'\-'

# Ignored characters
t_ignore = " \t"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")
    
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)
    
# Build the lexer
import ply.lex as lex
lexer = lex.lex()

# Parsing rules

precedence = (
    ('right','OR'),
    ('right','AND'),
    ('right','NOT'),
    ('right','PLUS', 'MINUS'),
    ('right','TIMES', 'DIVIDE'),
    ('nonassoc', 'MOD'),
    ('right', 'UMINUS')
    )

# dictionary of names
names = []

quadruples = []

def backpatch(l: list, i: int):
    for line_number in l:
        quadruples[line_number - 1] = ("goto", i)

def nextinstr():
    return len(quadruples) + 1

def p_marker(t):
    'marker : '
    t[0] = nextinstr()

class E:
    def __init__(self, t, f):
        self.truelist = t
        self.falselist = f

def p_expression_or(t):
    '''expression : expression OR marker expression'''
    backpatch(t[1].falselist, t[3])
    truelist = t[1].truelist + t[4].truelist
    falselist = t[4].falselist
    t[0] = E(truelist, falselist)

def p_expression_and(t):
    'expression : expression AND marker expression'
    backpatch(t[1].truelist, t[3])
    truelist = t[4].truelist
    falselist = t[4].falselist + t[1].falselist
    t[0] = E(truelist, falselist)

# def p_expression_unot(t):
#     'expression : NOT expression %prec UNOT'
#     t[0] = E(t[2].falselist, t[2].truelist)

def p_expression_group(t):
    'expression : LPAREN expression RPAREN'
    t[0] = t[2]
    quadruples.append(('('+str(t[2])+')'))

def p_expression_true(t):
    'expression : TRUE'
    t[0] = E([nextinstr()], [])
    quadruples.append(("goto", ))

def p_expression_false(t):
    'expression : FALSE'
    t[0] = E([], [nextinstr()])
    quadruples.append(("goto", ))

def p_expression_integer(t):
    'expression : INTEGERCONSTANT'
    t[0] = t[1]

def p_expression_real(t):
    'expression : REALCONSTANT'
    t[0] = t[1]

def p_expression_id(t):
    'expression : ID'
    t[0] = t[1]
    names.append(t[1])

def p_expression_uminus(t):
    'expression : UMINUS expression'
    t[0] = -t[2]

def p_expression_plus(t):
    'expression : expression PLUS expression'
    quadruples.append((str(t[0])+'='+str(t[1])+'+'+str(t[3])))

def p_expression_minus(t):
    'expression : expression MINUS expression'
    quadruples.append((str(t[0])+'='+str(t[1])+'-'+str(t[3])))

def p_expression_times(t): #TODO
    'expression : expression TIMES expression'
    quadruples.append((str(t[0])+'='+str(t[1])+'*'+str(t[3])))

def p_expression_divide(t):#TODO
    'expression : expression DIVIDE expression'
    quadruples.append((str(t[0])+'='+str(t[1])+'/'+str(t[3])))

def p_expression_mod(t):#TODO
    'expression : expression MOD expression'
    quadruples.append((str(t[0])+'='+str(t[1])+'%'+str(t[3])))

def p_expression_less(t):#TODO
    'expression : expression LT expression'
    quadruples.append((str(t[0])+'='+str(t[1])+'<'+str(t[3])))

def p_expression_lessequal(t):#TODO
    'expression : expression LTEQ expression'
    quadruples.append((str(t[0])+'='+str(t[1])+'<='+str(t[3])))

def p_expression_greater(t):#TODO
    'expression : expression GT expression'
    quadruples.append((str(t[0])+'='+str(t[1])+'>'+str(t[3])))

def p_expression_greaterqual(t):#TODO
    'expression : expression GTEQ expression'
    quadruples.append((str(t[0])+'='+str(t[1])+'>='+str(t[3])))

def p_expression_notqual(t):#TODO
    'expression : expression NEQ expression'
    quadruples.append((str(t[0])+'='+str(t[1])+'<>'+str(t[3])))

def p_expression_qual(t):#TODO
    'expression : expression EQ expression'
    quadruples.append((str(t[0])+'='+str(t[1])+'='+str(t[3])))

def p_error(t):
    print("Syntax error at '%s'" % t.value)

import ply.yacc as yacc
parser = yacc.yacc(start="expression")

while True:
    try:
        s = input('calc > ')   # Use raw_input on Python 2
    except EOFError:
        break
    r = parser.parse(s)
    print(quadruples)
    print(r.truelist, r.falselist)
    quadruples.clear()
    