import ply.lex as lex
import ply.yacc as yacc
import ast as py_ast 
import sys

# 1. ANÁLISE LÉXICA (Tokens)
tokens = (
    'NUMBER', 'ID',          # Números e Identificadores (nomes de variáveis/funções)
    'LPAREN', 'RPAREN',      # Parênteses Esquerdo ( e Direito )
    'PLUS', 'MINUS', 'TIMES', 'DIV', # Operadores Matemáticos (+, -, *, /)
    'GT', 'LT', 'EQ',        # Operadores de Comparação (>, <, =)
    'DEFUN', 'IF', 'CONS'    # Palavras-chave da linguagem Lisp
)

# Expressões Regulares (Regex) para identificar símbolos simples.
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIV     = r'/'
t_GT      = r'>'
t_LT      = r'<'
t_EQ      = r'='
t_ignore  = ' \t\n' # Ignora espaços, tabs e novas linhas

# Função para identificar Identificadores (ID) e palavras reservadas.
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    # Verifica se o ID é uma palavra reservada e muda o tipo do token
    if t.value == 'defun': t.type = 'DEFUN'
    elif t.value == 'if': t.type = 'IF'
    elif t.value == 'cons': t.type = 'CONS'
    return t

# Função para identificar Números inteiros.
def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

# Função para tratar erros léxicos (caracteres não reconhecidos).
def t_error(t):
    print(f"Caractere ilegal '{t.value[0]}'")
    t.lexer.skip(1)

# Função auxiliar para rodar o lexer (cria a lista de tokens).
def run_lexer(code):
    lexer = lex.lex()
    lexer.input(code)
    token_list = []
    while True:
        tok = lexer.token()
        if not tok: break
        token_list.append(tok)
    return token_list

# 2. ANÁLISE SINTÁTICA (Parser)
# Classes para representar os nós da Árvore Sintática Abstrata (AST).
class ASTNode: pass

class Number(ASTNode): # Nó para números
    def __init__(self, val): self.val = val
    def __repr__(self): return str(self.val)

class Symbol(ASTNode): # Nó para símbolos (variáveis, nomes de função)
    def __init__(self, name): self.name = name
    def __repr__(self): return self.name

class List(ASTNode): # Nó para listas Lisp: ( elemento1 elemento2 ... )
    def __init__(self, elements): self.elements = elements
    def __repr__(self): return f"({', '.join(map(str, self.elements))})"

# O programa é uma lista de expressões.
def p_program(p):
    '''program : expr_list'''
    p[0] = p[1]

# Lista de expressões (pode ter várias expressões em sequência).
def p_expr_list(p):
    '''expr_list : expr expr_list
                 | expr'''
    if len(p) == 3: p[0] = [p[1]] + p[2]
    else: p[0] = [p[1]]

# Definição de "átomo" (unidade básica): número, ID ou palavra reservada.
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
    if isinstance(p[1], int):
        p[0] = Number(p[1])
    else:
        p[0] = Symbol(p[1])

# Definição da estrutura de lista Lisp
def p_expr_list_struct(p):
    '''expr : LPAREN elements RPAREN'''
    p[0] = List(p[2])

# Elementos dentro de uma lista (pode ser vazio).
def p_elements(p):
    '''elements : expr elements
                | empty'''
    if len(p) == 3: p[0] = [p[1]] + p[2]
    else: p[0] = []

# Regra para vazio (epsilon).
def p_empty(p):
    'empty :'
    pass

def p_error(p):
    if p:
        print(f"Erro de sintaxe no token '{p.value}' (tipo: {p.type})")
    else:
        print("Erro de sintaxe: Fim de arquivo inesperado")

# Função auxiliar para rodar o parser.
def run_parser(code):
    lexer = lex.lex()
    parser = yacc.yacc()
    return parser.parse(code)

# 3. GERAÇÃO DE CÓDIGO INTERMEDIÁRIO (IR)
# Transforma a Árvore (AST) em uma lista linear de instruções simples (Tuplas).
class CodeGenerator:
    def __init__(self):
        self.code = []          # Vetor que armazenará as instruções finais
        self.label_counter = 0  # Contador para gerar rótulos únicos (L1, L2...)

    # Cria um novo rótulo (Label) para saltos (GOTO).
    def new_label(self):
        self.label_counter += 1
        return f"L{self.label_counter}"

    # Adiciona uma instrução (tupla) ao vetor de código.
    def emit(self, op, arg=None):
        self.code.append((op, arg))

    # Função principal que percorre a árvore (Visitor Pattern).
    def visit(self, node):
        # Se for número, gera instrução para empilhar.
        if isinstance(node, Number):
            self.emit('PUSH', node.val)
        
        # Se for símbolo (variável), gera instrução para carregar da memória.
        elif isinstance(node, Symbol):
            self.emit('LOAD', node.name)
        
        # Se for uma lista, analisa o primeiro elemento (Operador).
        elif isinstance(node, List):
            if not node.elements: # Lista vazia ()
                self.emit('PUSH_NIL')
                return

            op = node.elements[0] # O primeiro item é a operação (ex: if, +, defun)
            args = node.elements[1:] # O resto são os argumentos

            #TRADUÇÃO DO IF
            if isinstance(op, Symbol) and op.name == 'if':
                cond, then_block, else_block = args[0], args[1], args[2]
                label_else = self.new_label()
                label_end = self.new_label()

                self.visit(cond)               # Gera código da condição
                self.emit('JMP_FALSE', label_else) # Se falso, pula para o else
                
                self.visit(then_block)         # Gera código do bloco "então"
                self.emit('JMP', label_end)    # Pula para o fim
                
                self.emit('LABEL', label_else) # Marca onde começa o else
                self.visit(else_block)         # Gera código do bloco "senão"
                
                self.emit('LABEL', label_end)  # Marca o fim do IF

            # TRADUÇÃO DE FUNÇÃO (DEFUN)
            elif isinstance(op, Symbol) and op.name == 'defun':
                func_name = args[0].name 
                func_args = args[1].elements
                body = args[2]
                
                self.emit('FUNC_START', func_name) # Marca início da função
                # Gera código para guardar os argumentos na memória
                for arg in reversed(func_args):
                    self.emit('STORE_ARG', arg.name)
                
                self.visit(body) # Gera código do corpo da função
                self.emit('RET') # Gera instrução de retorno

            # OPERAÇÃO DE LISTA (CONS)
            elif isinstance(op, Symbol) and op.name == 'cons':
                self.visit(args[0])
                self.visit(args[1])
                self.emit('CONS')

            # OPERAÇÕES MATEMÁTICAS E LÓGICAS
            elif isinstance(op, Symbol) and op.name in ['+', '-', '*', '/', '>', '<', '=']:
                for arg in args:
                    self.visit(arg) # Visita/Empilha operandos
                # Mapeia o símbolo para a instrução da VM correspondente
                mapping = {'+':'ADD', '-':'SUB', '*':'MUL', '/':'DIV', '>':'GT', '<':'LT', '=':'EQ'}
                self.emit(mapping[op.name])

            # CHAMADA DE FUNÇÃO GENÉRICA
            elif isinstance(op, Symbol):
                for arg in args:
                    self.visit(arg)
                self.emit('CALL', op.name)

    # Função que inicia o processo de geração para toda a lista de nós da AST.
    def generate(self, ast_list):
        for node in ast_list:
            self.visit(node)
            # Verifica se não é definição de função para imprimir o resultado na tela.
            if isinstance(node, List) and isinstance(node.elements[0], Symbol):
                if node.elements[0].name != 'defun':
                    self.emit('PRINT_TOP')
        return self.code

    # Salva o código gerado em um arquivo de texto/binário.
    def dump_to_file(self, filename="programa.bin"):
        with open(filename, "w") as f:
            for instr in self.code: f.write(f"{instr}\n")
        print(f"[IO] Código intermediário salvo em: {filename}")

# 4. MÁQUINA VIRTUAL (VM)
# Simula um computador com Pilha, Memória e Processador para executar o código
class VirtualMachine:
    def __init__(self):
        self.stack = []       # Pilha de Operandos (para cálculos)
        self.memory = {}      # Memória de Dados (variáveis)
        self.call_stack = []  # Pilha de Chamadas (para retorno de funções)
        self.functions = {}   # Tabela de endereços das funções
        self.pc = 0           # Program Counter (aponta a linha atual)

    # Lê o arquivo gerado e faz uma pré-varredura para achar Labels e Funções.
    def load_and_scan(self, filename):
        code = []
        labels = {}
        try:
            with open(filename, "r") as f:
                raw_lines = [line.strip() for line in f if line.strip()]
            
            # 1ª Passada: Mapear Labels e Funções
            for i, line in enumerate(raw_lines):
                instr = py_ast.literal_eval(line) # Converte string para tupla
                code.append(instr)
                if instr[0] == 'LABEL':
                    labels[instr[1]] = i
                if instr[0] == 'FUNC_START':
                    self.functions[instr[1]] = i 
            return code, labels
        except FileNotFoundError:
            print("Arquivo não encontrado!"); return [], {}

    # Loop principal de execução (Fetch-Decode-Execute).
    def run(self, filename):
        print("\n--- INICIANDO VM LISP ---")
        self.code, self.labels = self.load_and_scan(filename)
        self.pc = 0
        
        while self.pc < len(self.code):
            op, arg = self.code[self.pc] # Busca instrução
            
            # OPERAÇÕES DE PILHA
            if op == 'PUSH': self.stack.append(arg)
            elif op == 'PUSH_NIL': self.stack.append([])
            elif op == 'LOAD': self.stack.append(self.memory.get(arg, 0))
            
            # MATEMÁTICA
            elif op == 'ADD': b=self.stack.pop(); a=self.stack.pop(); self.stack.append(a+b)
            elif op == 'SUB': b=self.stack.pop(); a=self.stack.pop(); self.stack.append(a-b)
            elif op == 'MUL': b=self.stack.pop(); a=self.stack.pop(); self.stack.append(a*b)
            elif op == 'DIV': b=self.stack.pop(); a=self.stack.pop(); self.stack.append(a//b)
            
            # COMPARAÇÃO
            elif op == 'GT': b=self.stack.pop(); a=self.stack.pop(); self.stack.append(1 if a > b else 0)
            elif op == 'LT': b=self.stack.pop(); a=self.stack.pop(); self.stack.append(1 if a < b else 0)
            
            # LISTAS
            elif op == 'CONS':
                b = self.stack.pop() # Segundo elemento
                a = self.stack.pop() # Primeiro elemento
                self.stack.append([a, b]) # Cria lista [a, b]

            # CONTROLE DE FLUXO (SALTOS)
            elif op == 'JMP': 
                self.pc = self.labels[arg]
                continue # Pula direto para o label
            elif op == 'JMP_FALSE':
                cond = self.stack.pop()
                if cond == 0: # Se for falso (0)
                    self.pc = self.labels[arg] # Pula
                    continue

            # FUNÇÕES
            elif op == 'FUNC_START': 
                # Se encontrar definição de função linearmente, pula o corpo dela.
                while self.pc < len(self.code) and self.code[self.pc][0] != 'RET':
                    self.pc += 1
            
            elif op == 'CALL':
                if arg not in self.functions:
                    print(f"[ERRO FATAL] Função '{arg}' não definida/encontrada.")
                    return
                self.call_stack.append(self.pc) # Salva onde voltar
                self.pc = self.functions[arg]   # Pula para a função
            
            elif op == 'STORE_ARG':
                val = self.stack.pop()
                self.memory[arg] = val

            elif op == 'RET':
                if self.call_stack:
                    self.pc = self.call_stack.pop() # Volta para quem chamou
                else:
                    pass
            
            # SAÍDA
            elif op == 'PRINT_TOP':
                if self.stack: print(f">>> SAÍDA: {self.stack.pop()}")

            self.pc += 1 # Vai para a próxima instrução
        print("--- FIM DA EXECUÇÃO ---")

# MENU PRINCIPAL (Interface com o Usuário)
if __name__ == "__main__":
    
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
            # 1. Parse (Análise Sintática)
            ast = run_parser(codigo)
            if not ast:
                print("Erro: Nenhuma árvore gerada. Verifique a sintaxe.")
                sys.exit()
                
            print("\n2. ÁRVORE (AST):")
            for node in ast: print(f"  {node}")
            
            # 2. Geração de Código
            gen = CodeGenerator()
            gen.generate(ast)
            gen.dump_to_file("programa.bin")
            
            # 3. Execução (VM)
            vm = VirtualMachine()
            vm.run("programa.bin")
            
        except Exception as e:
            print(f"\n[ERRO DE EXECUÇÃO] {e}")
