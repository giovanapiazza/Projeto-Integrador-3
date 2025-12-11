import re
import ast as py_ast 
import time

# ==============================================================================
# FASE 1: ANÁLISE LÉXICA (O "Lexer")
# Objetivo: Quebrar o texto cru em pedaços significativos (Tokens).
# ==============================================================================

class Token:
    """Representa uma unidade de significado (ex: um número, um sinal de +)."""
    def __init__(self, type, value):
        self.type = type   # Ex: 'NUMBER', 'OP', 'ID'
        self.value = value # Ex: 10, '+', 'x'
    
    def __repr__(self): 
        return f"Token({self.type}, {repr(self.value)})"

def lexer(code):
    print(f"\n[1. LEXER] Iniciando análise léxica do código...")
    
    # Aqui definimos as regras (Expressões Regulares) para cada token
    token_spec = [
        ('NUMBER',   r'\d+'),             # Números inteiros
        ('ASSIGN',   r'='),               # Sinal de igual
        ('END',      r';'),               # Ponto e vírgula
        ('ID',       r'[A-Za-z_]\w*'),    # Variáveis (Identificadores)
        ('OP',       r'[+\-*/]'),         # Operadores matemáticos
        ('LPAREN',   r'\('),              # Parênteses
        ('RPAREN',   r'\)'),
        ('SKIP',     r'[ \t\n]+'),        # Espaços (serão ignorados)
        ('MISMATCH', r'.'),               # Erro (qualquer outra coisa)
    ]
    
    # Compila as regras em uma única expressão gigante
    tok_regex = '|'.join('(?P<%s>%s)' % pair for pair in token_spec)
    tokens = []
    
    for mo in re.finditer(tok_regex, code):
        kind = mo.lastgroup
        value = mo.group()
        
        if kind == 'SKIP': 
            continue # Ignora espaços
        elif kind == 'MISMATCH': 
            raise RuntimeError(f'Erro Léxico: Caractere inválido {value!r}')
            
        t = Token(kind, value)
        tokens.append(t)
        # print(f"   -> Encontrado: {t}") # (Opcional: descomente para ver cada token)
        
    print(f"[1. LEXER] Total de tokens encontrados: {len(tokens)}")
    return tokens

# ==============================================================================
# FASE 2: ANÁLISE SINTÁTICA (O "Parser")
# Objetivo: Organizar os tokens em uma Árvore de Derivação (AST).
# Cumpre o PASSO 1 da sua imagem.
# ==============================================================================

# -- Definição dos Nós da Árvore --
class ASTNode: pass

class BinOp(ASTNode): # Operação Binária (Esquerda Operador Direita)
    def __init__(self, left, op, right): self.left = left; self.op = op; self.right = right
    def __repr__(self): return f"({self.left} {self.op} {self.right})"

class Number(ASTNode): # Número (Folha da árvore)
    def __init__(self, value): self.value = value
    def __repr__(self): return str(self.value)

class Assign(ASTNode): # Atribuição (Variável = Expressão)
    def __init__(self, name, expr): self.name = name; self.expr = expr
    def __repr__(self): return f"Atribuir({self.name} <- {self.expr})"

class Var(ASTNode): # Uso de Variável
    def __init__(self, name): self.name = name
    def __repr__(self): return f"Var({self.name})"

class Print(ASTNode): # Comando Print
    def __init__(self, expr): self.expr = expr
    def __repr__(self): return f"Print({self.expr})"

# -- O Parser propriamente dito --
class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

    def consume(self, expected_type):
        """Verifica se o token atual é o esperado e avança para o próximo."""
        if self.pos < len(self.tokens) and self.tokens[self.pos].type == expected_type:
            self.pos += 1
            return self.tokens[self.pos-1]
        raise SyntaxError(f"Erro Sintático: Esperado {expected_type}, mas acabou o código ou veio outro token.")

    # Regra: Fator -> Número | Variável | (Expressão)
    def factor(self):
        t = self.tokens[self.pos]
        if t.type == 'NUMBER': 
            self.consume('NUMBER')
            return Number(int(t.value))
        elif t.type == 'ID': 
            self.consume('ID')
            return Var(t.value)
        elif t.type == 'LPAREN':
            self.consume('LPAREN')
            node = self.expr()
            self.consume('RPAREN')
            return node
        raise SyntaxError("Erro no Fator: esperava número ou variável.")

    # Regra: Termo -> Fator * Fator | Fator / Fator
    def term(self):
        node = self.factor()
        while self.pos < len(self.tokens) and self.tokens[self.pos].value in '*/':
            op = self.consume('OP').value
            node = BinOp(node, op, self.factor())
        return node

    # Regra: Expressão -> Termo + Termo | Termo - Termo
    def expr(self):
        node = self.term()
        while self.pos < len(self.tokens) and self.tokens[self.pos].value in '+-':
            op = self.consume('OP').value
            node = BinOp(node, op, self.term())
        return node

    # Regra: Declaração -> Atribuição | Print
    def statement(self):
        if self.tokens[self.pos].type == 'ID':
            if self.tokens[self.pos].value == 'print':
                self.consume('ID'); self.consume('LPAREN')
                node = Print(self.expr())
                self.consume('RPAREN'); self.consume('END')
                return node
            else:
                name = self.consume('ID').value
                self.consume('ASSIGN')
                node = Assign(name, self.expr())
                self.consume('END')
                return node
        raise SyntaxError("Instrução desconhecida.")

    def parse(self):
        print(f"\n[2. PARSER] Construindo Árvore Sintática...")
        stmts = []
        while self.pos < len(self.tokens):
            stmts.append(self.statement())
        return stmts

# ==============================================================================
# FASE 3: GERAÇÃO DE CÓDIGO (Back-end)
# Objetivo: Converter a árvore em instruções lineares (Vetor).
# Cumpre os PASSOS 2, 3, 4 e 7 da imagem.
# ==============================================================================

class CodeGenerator:
    def __init__(self):
        # [PASSO 2] Criar um vetor para armazenamento das instruções
        self.code = [] 

    def emit(self, op, arg=None):
        # [PASSO 3] Determinar as tuplas que devem ser inseridas no vetor
        # Formato da tupla: (OPCODE, ARGUMENTO)
        instruction = (op, arg)
        self.code.append(instruction)

    def visit(self, node):
        # [PASSO 4] Percorrer a árvore em profundidade (Pós-Ordem)
        # Lógica: Primeiro resolvemos os filhos, depois o pai.
        
        if isinstance(node, Number):
            self.emit('PUSH', node.value) # Empilha número direto
            
        elif isinstance(node, Var):
            self.emit('LOAD', node.name)  # Busca valor da variável e empilha
            
        elif isinstance(node, BinOp):
            # Recursão: Visita esquerda, depois direita
            self.visit(node.left)
            self.visit(node.right)
            
            # Só depois de visitar os filhos, emite a operação
            if node.op == '+': self.emit('ADD')
            elif node.op == '-': self.emit('SUB')
            elif node.op == '*': self.emit('MUL')
            elif node.op == '/': self.emit('DIV')
            
        elif isinstance(node, Assign):
            self.visit(node.expr)         # Resolve a expressão matemática
            self.emit('STORE', node.name) # Guarda o resultado na variável
            
        elif isinstance(node, Print):
            self.visit(node.expr)
            self.emit('PRINT')

    def generate(self, statements):
        print(f"\n[3. CODEGEN] Gerando código intermediário (percorrendo árvore)...")
        for stmt in statements:
            self.visit(stmt)
        return self.code

    def dump_to_file(self, filename="codigo.txt"):
        # [PASSO 7] Construir "descarregador" para arquivo de texto
        print(f"[3. CODEGEN] Salvando instruções em disco: '{filename}'")
        with open(filename, "w") as f:
            for instr in self.code:
                f.write(f"{instr}\n")

# ==============================================================================
# FASE 4: AMBIENTE DE EXECUÇÃO (Máquina Virtual)
# Objetivo: Simular o hardware que executa as tuplas.
# Cumpre os PASSOS 5, 6 e 8 da imagem.
# ==============================================================================

class VirtualMachine:
    def __init__(self):
        # [PASSO 8] Estruturas de dados (Pilha e Memória de Dados)
        self.stack = []     # Pilha de avaliação
        self.memory = {}    # Tabela de símbolos (valores das variáveis)
        self.pc = 0         # Program Counter (aponta qual linha executar)

    def load(self, filename):
        print(f"\n[4. VM] Carregando programa do arquivo '{filename}'...")
        code = []
        try:
            with open(filename, "r") as f:
                for line in f:
                    if line.strip():
                        # Converte a string "(PUSH, 10)" de volta para tupla real
                        code.append(py_ast.literal_eval(line.strip()))
            return code
        except FileNotFoundError:
            print("Erro: Arquivo não encontrado.")
            return []

    def run(self, program_code):
        print(f"[4. VM] Iniciando execução...\n")
        self.code = program_code
        self.pc = 0
        
        # Loop Principal da CPU
        while self.pc < len(self.code):
            op, arg = self.code[self.pc] # Busca instrução (Fetch)
            
            # Debug visual para mostrar o estado atual
            print(f"   [PC:{self.pc:02d}] {op:<6} {str(arg) if arg else '':<4} | Pilha: {self.stack}")
            
            # [PASSO 5] Traduzir instrução para ação
            if op == 'PUSH':
                self.stack.append(arg)
                
            elif op == 'LOAD':
                val = self.memory.get(arg, 0) # Se não existe, assume 0
                self.stack.append(val)
                
            elif op == 'STORE':
                val = self.stack.pop()
                self.memory[arg] = val
                print(f"         >>> Memória atualizada: {arg} = {val}")
                
            elif op == 'ADD':
                b = self.stack.pop(); a = self.stack.pop()
                self.stack.append(a + b)
                
            elif op == 'SUB':
                b = self.stack.pop(); a = self.stack.pop()
                self.stack.append(a - b)
                
            elif op == 'MUL':
                b = self.stack.pop(); a = self.stack.pop()
                self.stack.append(a * b)
                
            elif op == 'PRINT':
                val = self.stack.pop()
                print(f"         >>> SAÍDA DO PROGRAMA: {val}")
            
            self.pc += 1 # Avança para próxima instrução
            time.sleep(0.1) # Pequena pausa para efeito visual

        print("\n[4. VM] Execução finalizada.")
        print(f"[4. VM] Estado final da memória: {self.memory}")

# ==============================================================================
# EXECUÇÃO DO TRABALHO (MAIN)
# ==============================================================================
if __name__ == "__main__":
    # Nosso código fonte de exemplo
    codigo_fonte = """
    x = 10;
    y = 5;
    z = (x + y) * 2;
    print(z);
    """
    
    print("="*60)
    print("       COMPILADOR E MÁQUINA VIRTUAL - MINI LINGUAGEM")
    print("="*60)
    print(f"CÓDIGO FONTE:\n{codigo_fonte}")
    
    # 1. Lexer
    tokens = lexer(codigo_fonte)
    
    # 2. Parser
    parser = Parser(tokens)
    arvore = parser.parse()
    # Mostra a estrutura da árvore
    print("\n[2. PARSER] Visualização da Árvore:")
    for node in arvore: 
        print(f"   Declaração: {node}")

    # 3. Geração de Código
    gen = CodeGenerator()
    bytecode = gen.generate(arvore)
    gen.dump_to_file("programa_compilado.txt") # Salva no disco
    
    # 4. Máquina Virtual
    vm = VirtualMachine()
    codigo_carregado = vm.load("programa_compilado.txt") # Lê do disco
    vm.run(codigo_carregado)
