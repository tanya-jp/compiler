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
         ] + list(reserved.values())

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

# list of names
names = []

quadruples = []

class Temp:
    def __init__(self):
        self.temp = []

    def newTemp(self):
        self.temp.append('temp' + str(len(self.temp)+1))
        return 'temp' + str(len(self.temp))


temps = Temp()

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

def p_expression_unot(t):
    'expression : NOT expression'
    t[0] = E(t[2].falselist, t[2].truelist)

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
    quadruples.append(((t[0])+'='+str(t[1])+'+'+str(t[3])))

def p_expression_minus(t):
    'expression : expression MINUS expression'
    quadruples.append(((t[0])+'='+str(t[1])+'-'+str(t[3])))

def p_expression_times(t): 
    'expression : expression TIMES expression'
    quadruples.append(((t[0])+'='+str(t[1])+'*'+str(t[3])))

def p_expression_divide(t):
    'expression : expression DIVIDE expression'
    quadruples.append(((t[0])+'='+str(t[1])+'/'+str(t[3])))

def p_expression_mod(t):
    'expression : expression MOD expression'
    quadruples.append(((t[0])+'='+str(t[1])+'%'+str(t[3])))

def p_expression_less(t):
    'expression : expression LT expression'
    t[0] = E([nextinstr()], [nextinstr() + 1])
    quadruples.append(('if '+'='+str(t[1])+'<'+str(t[3])+' goto',))
    quadruples.append(('goto',))

def p_expression_lessequal(t):
    t[0] = E([nextinstr()], [nextinstr() + 1])
    'expression : expression LTEQ expression'
    quadruples.append(('if '+'='+str(t[1])+'<='+str(t[3])+ ' goto',))
    quadruples.append(('goto',))

def p_expression_greater(t):
    'expression : expression GT expression'
    t[0] = E([nextinstr()], [nextinstr() + 1])
    quadruples.append(('if '+'='+str(t[1])+'>'+str(t[3])+ ' goto',))
    quadruples.append(('goto',))

def p_expression_greaterqual(t):
    'expression : expression GTEQ expression'
    t[0] = E([nextinstr()], [nextinstr() + 1])
    quadruples.append(('if '+'='+str(t[1])+'>='+str(t[3])+ ' goto',))
    quadruples.append(('goto',))

def p_expression_notqual(t):
    'expression : expression NEQ expression'
    t[0] = E([nextinstr()], [nextinstr() + 1])
    quadruples.append(('if '+'='+str(t[1])+'<>'+str(t[3])+ ' goto',))
    quadruples.append(('goto',))

def p_expression_qual(t):
    'expression : expression EQ expression'
    t[0] = E([nextinstr()], [nextinstr() + 1])
    quadruples.append(('if '+'='+str(t[1])+'='+str(t[3])+ ' goto',))
    quadruples.append(('goto',))

class C:
    def __init__(self, t, f):
        self.truelist = t
        self.falselist = f

def p_constant_integer(t):
    'constant : INTEGERCONSTANT'
    t[0] = t[1]

def p_constant_real(t):
    'constant : REALCONSTANT'
    t[0] = t[1]

class CL:
    def __init__(self, t, f):
        self.truelist = t
        self.falselist = f

def p_constantList_integer(t):
    'constantList : constant'
    t[0] = t[1]

def p_constantList_real(t):
    'constantList : constantList COMMA constant'
    t[0] = t[1] + t[3]

class N:
    def __init__(self, nextlist):
        self.nextlist = nextlist
        quadruples.append(('goto',))


def p_N(t):
    'N : '
    t[0] = N([])
    pass

class Statement:
    def __init__(self, nextlist):
        self.nextlist = nextlist


def p_statement_assign(t):
    'statement : ID ASSIGN expression'
    quadruples.append((str(t[1]) + '=' + str(t[3]),))
    names.append(t[1])
    t[0] = Statement([])


def p_statement_ifthen(t):
    'statement : IF expression THEN marker statement'
    backpatch(t[2].truelist, t[4])
    if t[5].nextlist == None:
        nextlist = t[2].falselist
    else:
        nextlist = t[2].falselist + t[5].nextlist
    t[0] = Statement(nextlist)


def p_statement_ifthenelse(t):
    'statement : IF expression THEN marker statement N ELSE marker statement'
    backpatch(t[2].truelist, t[4])
    backpatch(t[2].falselist, t[8])
    temp = t[5].nextlist + t[6].nextlist
    nextlist = temp + t[9].nextlist
    t[0] = Statement(nextlist)


def p_statement_while(t):
    'statement : WHILE marker expression DO marker statement'
    backpatch(t[6].nextlist, t[2])
    backpatch(t[3].truelist, t[5])
    nextlist = t[3].falselist
    quadruples.append(('goto', t[2]))
    t[0] = Statement(nextlist)


def p_statement_compound(t):
    'statement : compoundStatement'
    t[0] = t[1]


def p_statement_print(t):
    'statement : PRINT LPAREN expression RPAREN'
    t[3] = str(t[3])
    quadruples.append(('printf("%d\n",' + t[3].addr + ")",))


class StatementList:
    def __init__(self, nextlist):
        self.nextlist = nextlist

def p_statementList_statement(t):
    'statementList : statement'
    nextlist = t[1].nextlist
    t[0] = StatementList(nextlist)

def p_statementList_statementList(t):
    'statementList : statementList SEMICOLON marker statement'
    backpatch(t[1].nextlist, t[3])
    t[0] = StatementList(t[4].nextlist)


class CompoundStatement:
    def __init__(self, nextlist):
        self.nextlist = nextlist

def p_compoundStatement_beginEnd(t):
    'compoundStatement : BEGIN statementList END'
    nextlist = t[2].nextlist
    t[0] = CompoundStatement(nextlist)
    quadruples.append('end')

class Type:
    def __init__(self):
        pass


def p_type_integer(t):
    'type : INTEGERCONSTANT'
    t[0] = t[1]

def p_type_real(t):
    'type : REALCONSTANT'
    t[0] = t[1]


class IdList:
    def __init__(self):
        pass

def p_idList_id(t):
    """idList : ID"""
    t[0] = [t[1]]
    names.append(t[1])


def p_idList_idListId(t):
    """idList : idList COMMA ID"""
    t[0] = t[1] + t[3]
    names.append(t[1])

class DeclarationList:
    def __init__(self):
        pass


# class Program:
#     def __init__(self):
#         pass

# def p_program(t):
#     """program : PROGRAM ID declarations compoundStatement"""
#     names.append(t[2])




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
    