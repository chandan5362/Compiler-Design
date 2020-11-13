import ply.lex as lex
import sys


"""
SDT is :->
L' -> L
L -> E 
E -> E + T
E -> T
T -> T * F
T -> F
F -> F ^ G
F -> G
G -> - G
G -> ( E )
G -> id
"""
nt = []

def actions(st, symb):
    """
    shift = 1
    reduce = 0
    return action taken after reaching a particular state st and the current 
    input symbol symb
    """
    if symb.isdigit():
        """
        checks if a symbol encounterd is number or not.
        If it is number then it returns a string "id" corresponding 
        to that number to ease the parsing. for example :
        if input syting is '3*4+5',
        the it becomnes 'id*id+id'
        """
        symb = 'id'

    action = {
        0 : {'-' : (1,6),'(' : (1,7), 'id': (1,8)}, #0
        1 : {'$' : 'acc'}, #1
        2 : {'+' : (1,9), '$' : (0,1)}, #2
        3 : {'+' : (0,3), '*' : (1,10), '$' : (0,3)},
        4 : {'+' : (0,5), '*' : (0,5), '^' : (1,11), '$' : (0,5)},
        5 : {'+' : (0,7), '*' : (0,7), '^' : (0,7), '$' : (0,7)},
        6 : {'-' : (1,6),'(' : (1,7), 'id' : (1,8)},
        7 : {'-': (1,17),'(' : (1,18), 'id' : (1,19)},
        8 : {'+' : (0,10), '*' : (0,10), '^' : (0,10), '$' : (0,10)},
        9 : {'-': (1,6),'(' : (1,7), 'id' : (1,8)},
        10 : {'-': (1,6),'(' : (1,7), 'id' : (1,8)},
        11 : {'-': (1,6),'(' : (1,7), 'id' : (1,8)},
        12 : {'+' : (0,8), '*' : (0,8), '^' : (1,8), '$' : (0,8)},
        13 : {'+' : (1,24),')' : (1,23)},
        14 : {'+' : (0,3), '*' : (1,25), ')' : (0,3)},
        15 : {'+' : (0,5), '*' : (0,5) , '^' : (1,26), ')' : (0,5)},
        16 : {'+' : (0,7), '*' : (0,7) ,'^' : (0,7), ')' : (0,7)},
        17 : {'-': (1,17),'(' : (1,18), 'id' : (1,19)},
        18 : {'-': (1,17),'(' : (1,18), 'id' : (1,19)},
        19 : {'+' : (0,10), '*' : (0,10) ,'^' : (0,10), ')' : (0,10)},
        20 : {'+' : (0,2), '*' : (1,10), '$' : (0,2)},
        21 : {'+' : (0,4), '*' : (0,4), '^' : (1,11), '$' : (0,4)},
        22 : {'+' : (0,6), '*' : (0,6), '^' : (0,6), '$' : (0,6)},
        23 : {'+' : (0,9), '*' : (0,9), '^' : (0,9), '$' : (0,9)},
        24:  {'-': (1,17),'(' : (1,18), 'id' : (1,19)},
        25 : {'-': (1,17),'(' : (1,18), 'id' : (1,19)},
        26 : {'-': (1,17),'(' : (1,18), 'id' : (1,19)},
        27 : {'+' : (0,8), '*' : (0,8) ,'^' : (0,8), ')' : (0,8)},
        28 : {'+' : (1, 24), ')' : (1, 32)},
        29 : {'+' : (0,2), '*': (1,25), ')' : (0, 2)},
        30 : {'+' : (0,4), '*' : (0,4) ,'^' : (1,26), ')' : (0,4)},
        31 : {'+' : (0,6), '*' : (0,6) ,'^' : (0,6), ')' : (0,6)},
        32 : {'+' : (0,9), '*' : (0,9) ,'^' : (0,9), ')' : (0,9)}

    }
    
    return action[st][symb]
    
def goto(st,symb):
    """
    it return the state just after the reduction by a production.
    """
    goto = {
        0 : {'L' : 1,'E' : 2 ,'T' : 3, 'F' : 4, 'G': 5},
        6 : {'G' : 12},
        7 : {'E' : 13, 'T' : 14, 'F' : 15, 'G': 16},
        9 :  {'T' : 20, 'F' : 4, 'G': 5},
        10 : {'F' : 21, 'G': 5},
        11 : {'G' : 22},
        17 : {'G' : 27},
        18 : {'E' : 28, 'T' : 14, 'F' : 15, 'G': 16},
        24 : {'T' : 29, 'F' : 15, 'G': 16},
        25 : {'F' : 30, 'G': 16},
        26 : {'G' : 31}
    }
    return goto[st][symb]

def production(p,stack,state):
    """describes action correspondig to every production.
    It stores the value in global stack called "nt"""
    if p == 1:
        print("\noutput for the given expression = ",nt[-1])
        state.pop()
        stack[-1] = 'L'

    elif p == 10:
        nt.append(int(stack[-1]))
        stack[-1] = 'G'
        state.pop()

    elif p == 9:
        
        stack = stack[:len(stack)-3]
        stack.append('G')
        state = state[:len(state)-3]

    elif p == 8:
        nt[-1] = (-nt[-1])
        stack = stack[:len(stack)-2]
        stack.append('G')
        state = state[:len(state)-2]
    
    elif p == 7:

        stack[-1] = 'F'
        state.pop()

    elif p == 6:
        l = nt.pop()
        nt[-1] = (nt[-1]**l)
        stack = stack[:len(stack)-3]
        stack.append('F')
        state = state[:len(state)-3]
    
    elif p == 5:

        stack[-1] = 'T'
        state.pop()

    elif p ==4:
        l = nt.pop()
        nt[-1] = (nt[-1]*l)
        stack = stack[:len(stack)-3]
        stack.append('T')
        state = state[:len(state)-3]

    elif p == 3:
        stack[-1] = 'E'
        state.pop()

    elif p == 2:
        l = nt.pop()
        nt[-1] = nt[-1]+l
        stack = stack[:len(stack)-3]
        stack.append('E')
        state = state[:len(state)-3]
    return stack,state


def shift_reduce(a,b,state,stack,inp_symb):
    """
    perform shift or reduce 
    """
    flag = 1
    if not a: #reduce
        stack,state = production(b,stack,state) 

        state.append(goto(state[-1],stack[-1]))
        flag-=1

    elif a: #shift
        state.append(b)
        stack.append(inp_symb)
    return flag,stack,state



def LR_parser(inp):
    """
    generates LR(1) parser.
    append $ to the right of input string.
    """
    stack = []
    state = []
    state.append(0)
    inp =inp.replace(" ","")
    inp+='$'
    stack.append('$')
    ci = inp[0]
    print("States                                ","stack                                    ","input")
    for i in range(500):
        a,b = actions(state[-1],ci)

        print('%-40s%-40s%-40s' % (str(state)[1:len(str(state))-1].replace(',',""),"".join(stack[1:len(str(stack))-1]),inp))
        fg,stack,state =shift_reduce(a,b,state,stack,ci)
        if  fg:
            inp= inp[1:]
        
        ci = inp[0]
        if stack[-1] == 'L':
            print("\nstring parsing successful")
            break 
        


tokens = [
    'INT',
    'FLOAT',
    'NAME',
    'PLUS',
    'MINUS',
    'DIVIDE',
    'MULTIPLY',
    'EQUALS',
    'LEFT_BRACKET',
    'POWER',
    'RIGHT_BRACKET'
]

t_PLUS = r'\+'
t_MINUS = r'\-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'\/'
t_EQUALS = r'\='
t_LEFT_BRACKET =r'\('
t_RIGHT_BRACKET =r'\)'
t_POWER =r'\^'


t_ignore = r' '

def t_FLOAT(t):
    r'\d\.\d+'
    t.value = float(t.value)
    return t
    
def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t



def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = 'NAME'
    return t

def t_error(t):
    print("illegal characters")
    t.lexer.skip(1)


lexer = lex.lex()


if __name__ == "__main__":
    try:
        print("Enter an expression to calculate = ")
        s = input("")
        print("-------------------------------------")
    except EOFError:
        sys.exit(1)
    lexer.input(s)
    num = 0
    # print("\n")
    while True:
        tok = lexer.token()
        
        if not tok:
            break
        else:
            num+=1
        print(tok)
    print("\nTotal number of token is  = ",num)
    print("---------------------------------------------------------------------------------------")
    LR_parser(s)
   