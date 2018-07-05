import ply.yacc as yacc
import ply.lex as lex
import sys

variables = { }

class Node:
    def __init__(self, type, children = None, leaf = None, lineno = 0):
         self.type = type
         self.leaf = leaf
         self.lineno = lineno
         if children:
              self.children = children
         else:
              self.children = []
              
def interpret(node: Node):
    if node != None:
        if len(node.children) > 0:
            if(node.type == "program" or node.type == "body" or node.type == "statement"):
                i = 0
                while i < len(node.children):
                    interpret(node.children[i])
                    i = i + 1

        if(node.type == "assign"):
            try:
                temp = interpret(node.children[0])
                if temp != None:
                    variables[node.leaf[0]] = temp
                else:
                    print ("line %d >> Cannot assign value to variable." % (node.lineno))
            except ValueError:
                     print ("line %d >> Cannot assign value to variable." % (node.lineno))

        if(node.type == "number"):
            if len(node.leaf) == 2:
                return -node.leaf[1]
            elif len(node.leaf) == 1:
                return node.leaf[0]

        if(node.type == "assign_var"):
            try:
                if len(node.leaf) == 2:
                    return -1*variables[node.leaf[1]]
                elif len(node.leaf) == 1:
                    return variables[node.leaf[0]]
            except LookupError:
                print ("line %d >> Undefined variable." % (node.lineno))
                return None

        if(node.type == "iter"):
            return interpret(node.children[0])

        if(node.type == "print"):
            string = interpret(node.children[0])
            if string != None: 
                print(string, end="")
            else:
                print ("line %d >> Cannot print." % (node.lineno))

        if(node.type == "bin_op"):
            try:
                a = interpret(node.children[0])
                b = interpret(node.children[1])
                if a == None or b == None:
                    print ("line %d >> Operation cannot be done." % (node.lineno))
                    return None
                if(node.leaf[0] == "+"):
                    return a + b
                elif(node.leaf[0] == "-"):
                    return a - b
                elif(node.leaf[0] == "*"):
                    return a * b
                elif(node.leaf[0] == "/"):
                    return a // b
                elif(node.leaf[0] == "^"):
                    return a ** b
                elif(node.leaf[0] == "%"):
                    return a % b
            except ValueError:
                print ("line %d >> Operation cannot be done." % (node.lineno))
            
        if node.type == "printables":
            if len(node.children) == 2:
                try:
                    x = interpret(node.children[0])
                    y = interpret(node.children[1])
                    if x == None or y == None:
                        return None
                    return str(x) + str(y)
                except TypeError:
                    return None
            elif len(node.children) == 1:
                try:
                    x = interpret(node.children[0])
                    if x == None:
                        return None
                    return str(x)
                except TypeError:
                    return None

        if node.type == 'newline':
            return '\n'

        if node.type == "make_arr":
            elem = []
            try:
                if len(node.children) == 0:
                    return elem
                elif len(node.children) == 1:
                    c = interpret(node.children[0])
                    if c == None:
                        return None
                    elem.append(c)
                    return elem
                elif len(node.children) == 2:
                    d = interpret(node.children[0])
                    f = interpret(node.children[1])
                    if d == None or f == None:
                        return None
                    elem.extend(d+f)
                    return elem
            except TypeError:
                return None

        if(node.type == "if_statement"):
            try:
                x = interpret(node.children[0])
                if x:
                    interpret(node.children[1])
            except TypeError:
                print ("line %d >> Statement is not allowed." % (node.lineno))
    
        if(node.type == "elseif_statement"):
            try:
                x = interpret(node.children[0])
                if x:
                    interpret(node.children[1])
                else:
                    interpret(node.children[2]) 
            except TypeError:
                print ("line %d >> Statement is not allowed." % (node.lineno))
        
        if(node.type == "else_statement"):
            try:
                x = interpret(node.children[0])
                if x:
                    interpret(node.children[1])
                else:
                    interpret(node.children[2])
            except TypeError:
                print ("line %d >> Statement is not allowed." % (node.lineno))

        if(node.type == "loop"):
            try:
                x = interpret(node.children[0])
                while x:
                    interpret(node.children[1])
                    x = interpret(node.children[0])
            except TypeError:
                print ("line %d >> Statement is not allowed." % (node.lineno))

        if(node.type == "loop2"):
            try:
                x = interpret(node.children[1])
                while True:
                    interpret(node.children[0])
                    if not x:
                        return
            except TypeError:
                print ("line %d >> Statement is not allowed." % (node.lineno))        
        
        if(node.type == "bool"):
            return node.leaf[0]

        if node.type == "bool_op":
            try:
                a = interpret(node.children[0])
                b = interpret(node.children[1])
                if a == None or b == None:
                    '''print ("line %d >> Operation cannot be done." % (node.lineno))'''
                    return None
                if node.leaf[0] == "&&":
                    return a and b
                elif node.leaf[0] == "||":
                    return a or b
                elif node.leaf[0] == "==":
                    return a == b   
                elif node.leaf[0] == "!=":
                    return a != b
                elif node.leaf[0] == "<":
                    return a < b
                elif node.leaf[0] == ">":
                    return a > b
                elif node.leaf[0] == "<=":
                    return a <= b   
                elif node.leaf[0] == ">=":
                    return a >= b
            except TypeError:
                print ("line %d >> Operation cannot be done." % (node.lineno))
                return None   

        if node.type == "from_array":
            try:
                tempo = variables[node.leaf[0]]
                index = interpret(node.children[0])
                if len(tempo) > index and index != None:
                    return (tempo[index])
                else:
                    '''print ("line %d >> Index out of bounds." % (node.lineno))'''               
            except LookupError:
                print ("line %d >> Undefined variable." % (node.lineno))
        
        if(node.type == "getinput_int"):
            try:
                if(len(node.children) == 0):
                    return int(input())
                else:
                    interpret(node.children[0])
                    return int(input())
            except TypeError:
                print ("line %d >> Input cannot be read." % (node.lineno))
                return None
        
        if(node.type == "getinput_float"):
            try:
                if(len(node.children) == 0):
                    return float(input())
                else:
                    interpret(node.children[0])
                    return float(input())
            except TypeError:
                print ("line %d >> Input cannot be read." % (node.lineno))
                return None
        
        if node.type == "getinput_string":
            try:
                if(len(node.children) == 0):
                    return input()
                else:
                    interpret(node.children[0])
                    return input()
            except TypeError:
                print ("line %d >> Input cannot be read." % (node.lineno))
                return None

        #Functions for array
        if node.type == "get_length":
            try:
                temp = interpret(node.children[0])                
                if temp == None:
                    return None
                else:
                    return len(temp)                
            except ValueError:
                print ("line %d >> Cannot get length." % (node.lineno))

        if node.type == "get_index":
            try:
                temp = interpret(node.children[0])
                search = interpret(node.children[1])             
                if temp == None:
                    return None
                else:
                    return temp.index(search)      
            except ValueError:
                return -1
        
        if node.type == "func":
            interpret(node.children[0])

        if node.type == "push":
            try:
                x = interpret(node.children[0])
                if x != None:
                    variables[node.leaf[0]].append(x)
                else:
                    print ("line %d >> Item cannot be pushed." % (node.lineno))
                    return
            except LookupError:
                print ("line %d >> Undefined variable." % (node.lineno))

        if node.type == "replace":
            try:
                x = interpret(node.children[0])
                y = interpret(node.children[1])
                if x != None and y != None:
                    variables[node.leaf[0]][x] = y
                else:
                    print ("line %d >> Item cannot be replaced." % (node.lineno))
                    return
            except LookupError:
                print ("line %d >> Item cannot be replaced." % (node.lineno))

        if node.type == "delete":
            try:
                x = interpret(node.children[0])
                num = interpret(node.children[1])
                if x != None and num < len(x) and num != None:
                    x.pop(num)
                else:
                    print ("line %d >> Cannot be deleted." % (node.lineno))
            except ValueError:
                print ("line %d >> Index out of bounds." % (node.lineno))
                return

        if node.type == "concat":
            try:
                x = variables[node.leaf[0]]
                y = variables[node.leaf[1]]
                return x + y
            except LookupError:
                print ("line %d >> Undefined variable." % (node.lineno)) 

############################
# precedence of operations #
############################
precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULT', 'DIVIDE'),
    ('right', 'EXPO', 'MODULO'),
    ('left', 'AND', 'OR'),
    ('left', 'EQUIV', 'NEQUIV'),
    ('left', 'MTHAN', 'LTHAN'),
    ('left', 'MEQUAL', 'LEQUAL'),
    ('left', 'COMMA')
)



##########################
# list of reserved words #
##########################
reserved = {
    'if' 			: 'IF',
    'else' 			: 'ELSE',
    'while' 		: 'WHILE',
    'do'            : 'DO',
    'true' 			: 'TRUE',
    'false' 		: 'FALSE',
    'printk' 		: 'PRINT',
    'scank'			: 'INPUT',
    'int' 			: 'INTD',
    'float' 		: 'FLOATD',
    'str' 			: 'STRINGD',
    'bool' 			: 'BOOLD',
    'intarr'	 	: 'INTAD',
    'floatarr'		: 'FLOATAD',
    'stringarr'		: 'STRAD',
    'len' 			: 'LENGTH',
    'index' 		: 'INDEX',
    'delete'		: 'DELETE',
    '~'             : 'TILDE'
}



##################
# list of tokens #
##################
tokens = [
    # Characters
    'INT',
    'FLOAT',
    'STRING',
    # Arithmetic Operators
    'PLUS', 
    'MINUS',
    'DIVIDE', 
    'MODULO', 
    'MULT',
    'EXPO', 
    # Logical Operators
    'OR',
    'AND',
    'EQUIV',
    'NEQUIV',
    'LTHAN',
    'MTHAN',
    'LEQUAL',
    'MEQUAL',
    # Assign
    'EQUAL',
    # Del
    'LPAREN',
    'RPAREN',
    'LBRACKET',
    'RBRACKET',
    'LBRACE',
    'RBRACE',
    'COMMA',
    'DOT',
    'NEWLINE'] + list(reserved.values())



############################
# definition of characters #
############################
# ignored characters
t_ignore = ' \t'

# arithmetic operators
t_PLUS = r'\+'
t_MINUS = r'\-'
t_DIVIDE = r'/'
t_MULT = r'\*'
t_MODULO = r'\%'
t_EXPO = r'\^'

# assignment operator
t_EQUAL = r'\='

# logical operators
t_OR = r'[|]{2}'
t_AND = r'[&]{2}'
t_EQUIV = r'[=]{2}'
t_NEQUIV = r'!='
t_LTHAN = r'<'
t_MTHAN = r'>'
t_LEQUAL = r'<='
t_MEQUAL = r'>='

# delimiters
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_COMMA = r'\,'
t_DOT = r'\.'

# reserved words
t_IF = r'if'
t_ELSE = r'else'
t_WHILE = r'while'
t_DO = r'do'
t_PRINT = r'printk'
t_INPUT = r'scank'
t_INDEX = r'index'
t_DELETE = r'delete'

# end of statement
t_TILDE = r'\~'



########################
# assignment of values #
########################
def t_TRUE(t):
    r'true'
    t.value = True
    return t

def t_FALSE(t):
    r'false'
    t.value = False
    return t

def t_EOL(t):
    r'\n'
    t.lexer.lineno += 1
    pass

def t_NEWLINE(t):
    r'\:n'
    return t

def t_COMMENT(t):
    r'\$.*'
    pass

#Functions
def t_LENGTH(t):
    r'len'
    return t



####################
# single variables #
####################
# example: hello.oppa
def t_INTD(t):
    r'[A-Za-z]+[A-Za-z]*\.oppa'
    return t

# example: hello.noona
def t_BOOLD(t):
    r'[A-Za-z]+[A-Za-z]*\.noona'
    return t

# example: hello.hyung
def t_FLOATD(t):
    r'[A-Za-z]+[A-Za-z]*\.hyung'
    return t

# example: hello.unnie
def t_STRINGD(t):
    r'[A-Za-z]+[A-Za-z]*\.unnie'
    return t



###################
# array variables #
###################
# example: hello.nim
def t_INTAD(t):
    r'[A-Za-z]+[A-Za-z]*\.nim'
    return t

# example: hello.ah
def t_FLOATAD(t):
    r'[A-Za-z]+[A-Za-z]*\.ah'
    return t

# example: hello.ssi
def t_STRAD(t):
    r'[A-Za-z]+[A-Za-z]*\.ssi'
    return t



##################
# literal values #
##################
# example: 5.5
def t_FLOAT(t):
    r'\d*\.\d+'
    t.value = float(t.value)
    return t

# example: 5
def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

# exmple: "string"
def t_STRING(t):
    r'\"([^\\"]|(\\.))*\"'
    t.value = t.value[1:-1]
    return t



##################
# error handling #
##################
def t_error(t):
    print("line %d \n   > Error. Illegal name." % (t.lexer.lineno))
    sys.exit()



###########
# grammar #
###########
lex.lex()



################
# main program #
################
def p_program(p):
    '''program : codelines'''
    p[0] = Node("program", [p[1]],[], p.lexer.lineno)
    interpret(p[0])

def p_codelines(p):
    '''codelines : statement TILDE
                 | codelines statement TILDE'''
    if len(p) == 3:
        p[0] = Node("body", [p[1]], [])
    else:
        p[0] = Node("body", [p[1], p[2]], [])

def p_statement(p):
    '''statement : assign
                 | ifstatement
                 | whileloop
                 | dowhileloop
                 | func
                 | PRINT print'''
    if p[1] == None:
        return
    if len(p) == 2:
        p[0] = Node("statement", [p[1]], [], p.lexer.lineno)
    else:
        p[0] = Node("statement", [p[2]], [], p.lexer.lineno)

def p_assign(p):
    '''assign   : INTD EQUAL integer
                | FLOATD EQUAL float
                | BOOLD EQUAL bool
                | STRINGD EQUAL string
                | INTD EQUAL getinput_int
                | FLOATD EQUAL getinput_float
                | STRINGD EQUAL getinput_string
                | INTAD EQUAL arr_int
                | FLOATAD EQUAL arr_float
                | STRAD EQUAL arr_str
                | arr_int EQUAL arr_int
                | arr_int EQUAL INTAD'''
    p[0] = Node("assign", [p[3]], [p[1]], p.lexer.lineno)



##################
# array creation #
##################
def p_arr_int(p):
    '''arr_int : LBRACKET make_arrint RBRACKET
            | concat_int'''
    if len(p) == 4:
        p[0] = Node("iter", [p[2]], [], p.lexer.lineno)
    elif len(p) == 2:
        p[0] = Node("iter", [p[1]], [], p.lexer.lineno)

def p_arr_float(p):
    '''arr_float : LBRACKET make_arrfloat RBRACKET
            | concat_float'''
    if len(p) == 4:
        p[0] = Node("iter", [p[2]], [], p.lexer.lineno)
    elif len(p) == 2:
        p[0] = Node("iter", [p[1]], [], p.lexer.lineno)

def p_arr_str(p):
    '''arr_str : LBRACKET make_arrstr RBRACKET
            | concat_str'''
    if len(p) == 4:
        p[0] = Node("iter", [p[2]], [], p.lexer.lineno)
    elif len(p) == 2:
        p[0] = Node("iter", [p[1]], [], p.lexer.lineno)

def p_make_arrint(p):
    '''make_arrint : integer
            | make_arrint COMMA make_arrint
            |'''
    if len(p) == 4:
        p[0] = Node("make_arr", [p[1], p[3]], [], p.lexer.lineno)
    elif len(p) == 2:
        p[0] = Node("make_arr", [p[1]], [], p.lexer.lineno)
    else:
        p[0] = Node("make_arr", [], [], p.lexer.lineno)

def p_make_arrfloat(p):
    '''make_arrfloat : float
            | make_arrfloat COMMA make_arrfloat
            |'''
    if len(p) == 4:
        p[0] = Node("make_arr", [p[1], p[3]], [], p.lexer.lineno)
    elif len(p) == 2:
        p[0] = Node("make_arr", [p[1]], [], p.lexer.lineno)
    else:
        p[0] = Node("make_arr", [], [], p.lexer.lineno)

def p_make_arrstr(p):
    '''make_arrstr : string
            | make_arrstr COMMA make_arrstr
            |'''
    if len(p) == 4:
        p[0] = Node("make_arr", [p[1], p[3]], [], p.lexer.lineno)
    elif len(p) == 2:
        p[0] = Node("make_arr", [p[1]], [], p.lexer.lineno)
    else:
        p[0] = Node("make_arr", [], [], p.lexer.lineno)



###########################
# get elements from array #
###########################
def p_get_intarray(p):
    'get_intarray : INTAD LBRACKET integer RBRACKET'
    p[0] = Node("from_array", [p[3]], [p[1]], p.lexer.lineno)

def p_get_floatarray(p):
    'get_floatarray : FLOATAD LBRACKET integer RBRACKET'
    p[0] = Node("from_array", [p[3]], [p[1]], p.lexer.lineno)

def p_get_strarray(p):
    'get_strarray : STRAD LBRACKET integer RBRACKET'
    p[0] = Node("from_array", [p[3]], [p[1]], p.lexer.lineno)



##############
# user input #
##############
def p_getinput_int(p):
    '''getinput_int :  INPUT print
                | INPUT LPAREN RPAREN'''
    if len(p) == 3:
        p[0] = Node("getinput_int", [p[2]], [], p.lexer.lineno)
    else:
        p[0] = Node("getinput_int", [], [], p.lexer.lineno)

def p_getinput_float(p):
    '''getinput_float :  INPUT print
                | INPUT LPAREN RPAREN'''
    if len(p) == 3:
        p[0] = Node("getinput_int", [p[2]], [], p.lexer.lineno)
    else:
        p[0] = Node("getinput_int", [], [], p.lexer.lineno)

def p_getinput_string(p):
    '''getinput_string : INPUT print
                | INPUT LPAREN RPAREN'''
    if len(p) == 3:
        p[0] = Node("getinput_string", [p[2]], [], p.lexer.lineno)
    else:
        p[0] = Node("getinput_string", [], [], p.lexer.lineno)



###########
# integer #
###########
def p_integer_name(p):
    '''integer : INTD
            | MINUS INTD'''
    if len(p) == 3:
        p[0] = Node("assign_var", [], [p[1], p[2]], p.lexer.lineno)
    else:
        p[0] = Node("assign_var", [], [p[1]], p.lexer.lineno)

def p_integer_variables_terminal(p):
    '''integer : INT
            | MINUS INT'''
    if len(p) == 2:
        p[0] = Node("number", [], [p[1]], p.lexer.lineno)
    else:
        p[0] = Node("number", [], [p[1], p[2]], p.lexer.lineno)

def p_integer_variables(p):
    '''integer : LPAREN integer RPAREN
            | get_intarray'''
    if len(p) == 2:
        p[0] = Node("iter", [p[1]], [], p.lexer.lineno)
    else:
        p[0] = Node("iter", [p[2]], [], p.lexer.lineno)

def p_integer_arithmetic(p):
    '''integer : integer PLUS integer
            | integer MINUS integer
            | integer MULT integer
            | integer DIVIDE integer
            | integer EXPO integer
            | integer MODULO integer'''
    p[0] = Node("bin_op", [p[1], p[3]], p[2], p.lexer.lineno)



###################
# array functions #
###################
def p_typesof_arr(p):
    '''typesof_arr : INTAD
            | FLOATAD
            | STRAD'''
    p[0] = Node("assign_var", [], [p[1]], p.lexer.lineno)

def p_typesof_data(p):
    '''typesof_data : integer
            | float
            | string'''
    p[0] = Node("iter", [p[1]], [], p.lexer.lineno)

def p_get_length(p):
    '''integer : string DOT LENGTH LPAREN RPAREN 
            | typesof_arr DOT LENGTH LPAREN RPAREN'''
    p[0] = Node("get_length", [p[1]], [], p.lexer.lineno)

def p_get_index(p):
    'integer : typesof_arr DOT INDEX LPAREN typesof_data RPAREN'
    p[0] = Node("get_index", [p[1], p[5]], [], p.lexer.lineno)



#################
# concatenation #
#################
def p_concat_int(p):
    'concat_int : INTAD PLUS INTAD'
    p[0] = Node("concat", [], [p[1], p[3]], p.lexer.lineno)

def p_concat_float(p):
    'concat_float : FLOATAD PLUS FLOATAD'
    p[0] = Node("concat", [], [p[1], p[3]], p.lexer.lineno)

def p_concat_str(p):
    'concat_str : STRAD PLUS STRAD'
    p[0] = Node("concat", [], [p[1], p[3]], p.lexer.lineno)



#######################
# additional function #
#######################
def p_functions(p):
    '''func : push
            | replace
            | delete'''
    p[0] = Node("func", [p[1]], [], p.lexer.lineno)

def p_push(p):
    '''push : INTAD PLUS integer
            | FLOATAD PLUS float
            | STRAD PLUS string'''
    p[0] = Node("push", [p[3]], [p[1]], p.lexer.lineno)

def p_replace(p):
    '''replace : INTAD LBRACKET integer RBRACKET EQUAL integer
            | FLOATAD LBRACKET integer RBRACKET EQUAL float
            | STRAD LBRACKET integer RBRACKET EQUAL string'''
    p[0] = Node("replace", [p[3], p[6]], [p[1]], p.lexer.lineno)

def p_delete(p):
    '''delete : typesof_arr DOT DELETE LPAREN integer RPAREN'''
    p[0] = Node("delete", [p[1], p[5]], [], p.lexer.lineno)



#########
# float #
#########
def p_float_name(p):
    '''float : FLOATD
            | MINUS FLOATD'''
    if len(p) == 2:
        p[0] = Node("assign_var", [], [p[1]], p.lexer.lineno)
    else:
        p[0] = Node("assign_var", [], [p[1], p[2]], p.lexer.lineno)

def p_float_variables_terminal(p):
    '''float : FLOAT
            | MINUS FLOAT'''
    if len(p) == 2:
        p[0] = Node("number", [], [p[1]], p.lexer.lineno)
    else:
        p[0] = Node("number", [], [p[1], p[2]], p.lexer.lineno)

def p_float_variables(p):
    '''float : LPAREN float RPAREN
            | get_floatarray'''
    if len(p) == 2:
        p[0] = Node("iter", [p[1]], [], p.lexer.lineno)
    else:
        p[0] = Node("iter", [p[2]], [], p.lexer.lineno)

def p_float_arithmetic(p):
    '''float : float PLUS float
            | float MINUS float
            | float MULT float
            | float DIVIDE float
            | float EXPO float
            | float MODULO float'''
    p[0] = Node("bin_op", [p[1], p[3]], p[2], p.lexer.lineno)



##########
# string #
##########
def p_string_name(p):
    '''string : STRINGD'''
    p[0] = Node("assign_var", [], [p[1]], p.lexer.lineno)

def p_string_variables(p):
    '''string : get_strarray'''
    p[0] = Node("iter", [p[1]], [], p.lexer.lineno)

def p_string_variables_terminal(p):
    '''string : STRING'''
    p[0] = Node("number", [], [p[1]], p.lexer.lineno)



###########
# boolean #
###########
def p_bool_name(p):
    'bool : BOOLD'
    p[0] = Node("assign_var", [], [p[1]], p.lexer.lineno)

def p_bool_variables(p):
    '''bool : LPAREN bool RPAREN
            | TRUE
            | FALSE'''
    if len(p) == 2:
        p[0] = Node("bool", [], [p[1]], p.lexer.lineno)
    else :
        p[0] = Node("iter", [p[2]], [], p.lexer.lineno)

def p_bool_operator(p):
    '''bool : integer AND integer
            | integer OR integer
            | integer EQUIV integer
            | integer NEQUIV integer
            | integer LTHAN integer
            | integer MTHAN integer
            | integer LEQUAL integer  
            | integer MEQUAL integer
            | float AND float
            | float OR float
            | float EQUIV float
            | float NEQUIV float
            | float LTHAN float
            | float MTHAN float
            | float LEQUAL float  
            | float MEQUAL float
            | bool AND bool
            | bool OR bool
            | bool EQUIV bool
            | bool NEQUIV bool
            | bool LTHAN bool
            | bool MTHAN bool
            | bool LEQUAL bool  
            | bool MEQUAL bool'''
    p[0] = Node("bool_op", [p[1], p[3]], [p[2]], p.lexer.lineno)



##########################
# conditional statements #
##########################
def p_if_statement(p):
    '''ifstatement : IF LPAREN bool RPAREN LBRACE codelines RBRACE'''
    p[0] = Node("if_statement", [p[3], p[6]], [], p.lexer.lineno)

def p_else_statement(p):
    '''ifstatement : IF LPAREN bool RPAREN LBRACE codelines RBRACE ELSE ifstatement
                   | IF LPAREN bool RPAREN LBRACE codelines RBRACE ELSE LBRACE codelines RBRACE'''
    if len(p) == 10:
        p[0] = Node("elseif_statement", [p[3],p[6],p[9]], [], p.lexer.lineno)
    elif len(p) == 12:
        p[0] = Node("else_statement", [p[3],p[6],p[10]], [], p.lexer.lineno)



###################
# loop statements #
###################
def p_whileloop(p):
    '''whileloop : WHILE LPAREN bool RPAREN LBRACE codelines RBRACE'''
    p[0] = Node("loop", [p[3], p[6]], [], p.lexer.lineno)

def p_dowhileloop(p):
    '''dowhileloop : DO LBRACE codelines RBRACE WHILE LPAREN bool RPAREN'''
    p[0] = Node("loop2", [p[3], p[7]], [], p.lexer.lineno)


#########
# error #
#########
def p_error(p):
    print ("line %d >> Syntax error." % (p.lexer.lineno))
    p[0] = Node("walalang", [],[],p.lexer.lineno)



############
# printing #
############
def p_print(p):
    '''print : LPAREN printables RPAREN'''
    p[0] = Node("print", [p[2]], [], p.lexer.lineno)

def p_printables(p):
    '''printables : integer
            | string
            | float
            | printables COMMA printables
            | newline
            | typesof_arr
            | bool'''
    if len(p) == 4:
        p[0] = Node("printables", [p[1], p[3]], [], p.lexer.lineno)
    else:
        p[0] = Node("printables", [p[1]], [], p.lexer.lineno)

def p_newline(p):
    '''newline : NEWLINE'''
    p[0] = Node("newline", [], [], p.lexer.lineno)

yacc.yacc()

try:
    i = input("Enter filename (include .kl extension): ")
    # i = sys.argv[1]
    # print(i)
    if i[-3:] != ".kl":
        raise EOFError
    txt = open(i, "r")
except:
    print("> File cannot be read <")
    quit()

try:
    with txt as f:
        yacc.parse(f.read())
except:
    print("> Error <")