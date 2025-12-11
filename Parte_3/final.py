import ply.lex as lex
import ply.yacc as yacc
import ast as py_ast 
import sys

# ==============================================================================
# 1. ANÁLISE LÉXICA (Tokens)
# ==============================================================================

tokens = (
    'NUMBER', 'ID',
    'LPAREN', 'RPAREN',
    'PLUS', 'MINUS', 'TIMES', 'DIV',
    'GT', 'LT', 'EQ',       
    'DEFUN', 'IF', 'CONS'   # Adicionado CONS
)

# Regras de Expressão Regular
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIV     = r'/'
t_GT      = r'>'
t_LT      = r'<'
t_EQ      = r'='
t_ignore  = ' \t\n'

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    # Mapeia palavras reservadas para tokens específicos
    if t.value == 'defun': t.type = 'DEFUN'
    elif t.value == 'if': t.type = 'IF'
    elif t.value == 'cons': t.type = 'CONS'
    return t

def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_error(t):
    print(f"Caractere ilegal '{t.value[0]}'")
    t.lexer.skip(1)

# Wrapper do Lexer
def run_lexer(code):
    lexer = lex.lex()
    lexer.input(code)
    token_list = []
    while True:
        tok = lexer.token()
        if not tok: break
        token_list.append(tok)
    return token_list

# ==============================================================================
# 2. ANÁLISE SINTÁTICA (Parser)
# ==============================================================================

class ASTNode: pass

class Number(ASTNode):
    def __init__(self, val): self.val = val
    def __repr__(self): return str(self.val)

class Symbol(ASTNode):
    def __init__(self, name): self.name = name
    def __repr__(self): return self.name

class List(ASTNode):
    def __init__(self, elements): self.elements = elements
    def __repr__(self): return f"({', '.join(map(str, self.elements))})"

# --- REGRAS DA GRAMÁTICA ---

def p_program(p):
    '''program : expr_list'''
    p[0] = p[1]

def p_expr_list(p):
    '''expr_list : expr expr_list
                 | expr'''
    if len(p) == 3: p[0] = [p[1]] + p[2]
    else: p[0] = [p[1]]

# CORREÇÃO PRINCIPAL AQUI:
# Ensinamos ao parser que palavras reservadas também são 'átomos' válidos na árvore
def p_expr_atom(p):
    '''expr : NUMBER
            | ID
            | DEFUN
            | IF
            | CONS
            | GT
            | LT
            | EQ
            | PLUS
            | MINUS
            | TIMES
            | DIV'''
    # Se for número, cria nó Number, senão cria nó Symbol
    if isinstance(p[1], int):
        p[0] = Number(p[1])
    else:
        # Transforma tokens como 'defun' ou '>' em Símbolos na AST
        p[0] = Symbol(p[1])

def p_expr_list_struct(p):
    '''expr : LPAREN elements RPAREN'''
    p[0] = List(p[2])

def p_elements(p):
    '''elements : expr elements
                | empty'''
    if len(p) == 3: p[0] = [p[1]] + p[2]
    else: p[0] = []

def p_empty(p):
    'empty :'
    pass

def p_error(p):
    if p:
        print(f"Erro de sintaxe no token '{p.value}' (tipo: {p.type})")
    else:
        print("Erro de sintaxe: Fim de arquivo inesperado")

def run_parser(code):
    # Recria lexer e parser a cada execução para limpar estado
    lexer = lex.lex()
    parser = yacc.yacc()
    return parser.parse(code)

# ==============================================================================
# 3. GERAÇÃO DE CÓDIGO (IR)
# ==============================================================================
class CodeGenerator:
    def __init__(self):
        self.code = []
        self.label_counter = 0

    def new_label(self):
        self.label_counter += 1
        return f"L{self.label_counter}"

    def emit(self, op, arg=None):
        self.code.append((op, arg))

    def visit(self, node):
        if isinstance(node, Number):
            self.emit('PUSH', node.val)
        
        elif isinstance(node, Symbol):
            self.emit('LOAD', node.name)
        
        elif isinstance(node, List):
            if not node.elements: # Lista vazia ()
                self.emit('PUSH_NIL')
                return

            op = node.elements[0]
            args = node.elements[1:]

            # Estrutura (if cond then else)
            if isinstance(op, Symbol) and op.name == 'if':
                cond, then_block, else_block = args[0], args[1], args[2]
                label_else = self.new_label()
                label_end = self.new_label()

                self.visit(cond)
                self.emit('JMP_FALSE', label_else)
                
                self.visit(then_block)
                self.emit('JMP', label_end)
                
                self.emit('LABEL', label_else)
                self.visit(else_block)
                
                self.emit('LABEL', label_end)

            # Estrutura (defun nome (args) corpo)
            elif isinstance(op, Symbol) and op.name == 'defun':
                func_name = args[0].name
                func_args = args[1].elements
                body = args[2]
                
                self.emit('FUNC_START', func_name)
                for arg in reversed(func_args):
                    self.emit('STORE_ARG', arg.name)
                
                self.visit(body)
                self.emit('RET')

            # Estrutura (cons a b)
            elif isinstance(op, Symbol) and op.name == 'cons':
                self.visit(args[0]) # Push item 1
                self.visit(args[1]) # Push item 2
                self.emit('CONS')

            # Operações Matemáticas e Lógicas
            elif isinstance(op, Symbol) and op.name in ['+', '-', '*', '/', '>', '<', '=']:
                for arg in args:
                    self.visit(arg)
                mapping = {'+':'ADD', '-':'SUB', '*':'MUL', '/':'DIV', '>':'GT', '<':'LT', '=':'EQ'}
                self.emit(mapping[op.name])

            # Chamada de Função Genérica
            elif isinstance(op, Symbol):
                for arg in args:
                    self.visit(arg)
                self.emit('CALL', op.name)

    def generate(self, ast_list):
        for node in ast_list:
            self.visit(node)
            # Imprime o topo da pilha se não for definição de função
            if isinstance(node, List) and isinstance(node.elements[0], Symbol):
                if node.elements[0].name != 'defun':
                    self.emit('PRINT_TOP')
        return self.code

    def dump_to_file(self, filename="programa.bin"):
        with open(filename, "w") as f:
            for instr in self.code: f.write(f"{instr}\n")
        print(f"[IO] Código intermediário salvo em: {filename}")

# ==============================================================================
# 4. MÁQUINA VIRTUAL (VM)
# ==============================================================================
class VirtualMachine:
    def __init__(self):
        self.stack = []
        self.memory = {}       
        self.call_stack = []   
        self.functions = {}    
        self.pc = 0

    def load_and_scan(self, filename):
        code = []
        labels = {}
        try:
            with open(filename, "r") as f:
                raw_lines = [line.strip() for line in f if line.strip()]
            
            # 1ª Passada: Mapear Labels e Funções
            for i, line in enumerate(raw_lines):
                instr = py_ast.literal_eval(line)
                code.append(instr)
                if instr[0] == 'LABEL':
                    labels[instr[1]] = i
                if instr[0] == 'FUNC_START':
                    self.functions[instr[1]] = i 
            return code, labels
        except FileNotFoundError:
            print("Arquivo não encontrado!"); return [], {}

    def run(self, filename):
        print("\n--- INICIANDO VM LISP ---")
        self.code, self.labels = self.load_and_scan(filename)
        self.pc = 0
        
        while self.pc < len(self.code):
            op, arg = self.code[self.pc]
            
            if op == 'PUSH': self.stack.append(arg)
            elif op == 'PUSH_NIL': self.stack.append([])
            elif op == 'LOAD': self.stack.append(self.memory.get(arg, 0))
            
            elif op == 'ADD': b=self.stack.pop(); a=self.stack.pop(); self.stack.append(a+b)
            elif op == 'SUB': b=self.stack.pop(); a=self.stack.pop(); self.stack.append(a-b)
            elif op == 'MUL': b=self.stack.pop(); a=self.stack.pop(); self.stack.append(a*b)
            elif op == 'DIV': b=self.stack.pop(); a=self.stack.pop(); self.stack.append(a//b)
            
            elif op == 'GT': b=self.stack.pop(); a=self.stack.pop(); self.stack.append(1 if a > b else 0)
            elif op == 'LT': b=self.stack.pop(); a=self.stack.pop(); self.stack.append(1 if a < b else 0)
            
            elif op == 'CONS':
                b = self.stack.pop() # Segundo elemento
                a = self.stack.pop() # Primeiro elemento
                self.stack.append([a, b]) # Cria lista [a, b]

            elif op == 'JMP': 
                self.pc = self.labels[arg]
                continue
            elif op == 'JMP_FALSE':
                cond = self.stack.pop()
                if cond == 0:
                    self.pc = self.labels[arg]
                    continue

            elif op == 'FUNC_START': 
                # Pula corpo da função se encontrado linearmente
                while self.pc < len(self.code) and self.code[self.pc][0] != 'RET':
                    self.pc += 1
            
            elif op == 'CALL':
                if arg not in self.functions:
                    print(f"[ERRO FATAL] Função '{arg}' não definida/encontrada.")
                    return
                self.call_stack.append(self.pc)
                self.pc = self.functions[arg]
            
            elif op == 'STORE_ARG':
                val = self.stack.pop()
                self.memory[arg] = val

            elif op == 'RET':
                if self.call_stack:
                    self.pc = self.call_stack.pop()
                else:
                    # Se RET for atingido sem call stack (fim da função linear), apenas segue
                    pass
            
            elif op == 'PRINT_TOP':
                if self.stack: print(f">>> SAÍDA: {self.stack.pop()}")

            self.pc += 1
        print("--- FIM DA EXECUÇÃO ---")

# ==============================================================================
# MENU
# ==============================================================================
if __name__ == "__main__":
    # Limpa aviso do PLY na primeira execução
    
    print("\n" + "="*40)
    print("   COMPILADOR LISP (VERSÃO FINAL)")
    print("="*40)
    print("1. Digitar código no terminal")
    print("2. Ler de arquivo")
    
    opt = input("Opção: ").strip()
    codigo = ""
    
    if opt == '1':
        print("Digite o código. 'FIM' para terminar.")
        while True:
            line = input()
            if line.strip() == 'FIM': break
            codigo += line + "\n"
    elif opt == '2':
        name = input("Nome do arquivo (ex: lisp.txt): ").strip()
        try:
            with open(name, 'r') as f: codigo = f.read()
        except: print("Erro ao ler arquivo."); sys.exit()

    if codigo.strip():
        print("\n1. CÓDIGO FONTE:\n", codigo)
        try:
            # Parse
            ast = run_parser(codigo)
            if not ast:
                print("Erro: Nenhuma árvore gerada. Verifique a sintaxe.")
                sys.exit()
                
            print("\n2. ÁRVORE (AST):")
            for node in ast: print(f"  {node}")
            
            # Geração
            gen = CodeGenerator()
            gen.generate(ast)
            gen.dump_to_file("programa.bin")
            
            # Execução
            vm = VirtualMachine()
            vm.run("programa.bin")
            
        except Exception as e:
            print(f"\n[ERRO DE EXECUÇÃO] {e}")
