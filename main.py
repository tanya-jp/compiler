import ply.lex as lex


class SampleLexer:
    tokens = [
        "TOKEN",
        "STARTCOMMENT",
        "ENDCOMMENT"
    ]

    states = [
        ("COMMENT", "exclusive")
    ]

    t_TOKEN = r"[0-9a-zA-Z]"

    t_ignore = " \t\n"
    t_COMMENT_ignore = " \t\n"

    def __init__(self):
        self.depth = 0

        self.stack = []

    def t_STARTCOMMENT(self, token):
        r'\/\*|\(\*'
        token.lexer.begin("COMMENT")
     
        self.stack.append(token.value[0])

    def t_COMMENT_STARTCOMMENT(self, token):
        r'\/\*|\(\*'
        
        self.stack.append(token.value[0])

    def t_COMMENT_ENDCOMMENT(self, token):
        r'\*\/|\*\)'
        last_stack = self.stack[-1]

        last_char = '/'

        if token.value[1] == ')':
            last_char = '('
        
        if last_char == last_stack:
            self.stack.pop()
        
        else:
            raise Exception("Invalid Closing Comment Symbol") 

        if(len(self.stack) == 0) :
            token.lexer.begin("INITIAL")

    def t_COMMENT_error(self, t):
        t.lexer.skip(1)

    def t_error(self, t):
        print("Illigal Character in Language %s" % t.value[0])
        t.lexer.skip(1)

    def doLexicalAnalysis(self, inp):
        self.lexer = lex.lex(module=self)
        self.lexer.input(inp)
        while True:
            tok = self.lexer.token()
            if not tok:
                break
            print(tok)

s = SampleLexer()
s.doLexicalAnalysis('first \n1 + /* assignment /* of*/ */compiler (* course /* test */ *) (*test*/')
      
            
