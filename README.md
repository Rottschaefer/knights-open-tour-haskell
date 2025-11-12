# Knight's Open Tour em Haskell

Este projeto é uma implementação do problema "Passeio do Cavalo" (Knight's Tour), desenvolvido para a disciplina de Linguagens de Programação do curso de Ciência da Computação da Universidade Federal Fluminense (UFF).

## Descrição do Problema

O objetivo é encontrar um "passeio aberto" para um cavalo em um tabuleiro de xadrez de dimensões `M x N`. Um passeio aberto significa que o cavalo visita cada casa do tabuleiro exatamente uma vez, partindo de uma posição inicial `(L, C)`. A restrição adicional é que a casa final do percurso não pode ter um movimento válido para a casa inicial.

## Funcionalidades

- **Leitura de Cenários**: O programa lê um arquivo de texto (`exe.txt` por padrão) que define múltiplos cenários.
- **Estrutura do Arquivo de Entrada**: Cada linha do arquivo deve conter quatro números inteiros, separados por espaços:
  1.  `M`: Número de linhas do tabuleiro.
  2.  `N`: Número de colunas do tabuleiro.
  3.  `L`: Linha inicial do cavalo.
  4.  `C`: Coluna inicial do cavalo.
- **Saída**: Para cada cenário, o programa exibirá na tela o caminho encontrado ou uma mensagem indicando que não foi possível encontrar uma solução.

## Como Compilar e Executar

### Pré-requisitos

- GHC (Glasgow Haskell Compiler) versão 8.0 ou superior
- Make (opcional, mas recomendado)

### Compilação e Execução

#### Usando Makefile (Recomendado)

```bash
# Compilar o projeto
make

# Apenas executar (após compilar)
make run

# Limpar arquivos de compilação
make clean

```

#### Usando GHC diretamente

```bash
# Compilar com otimizações
ghc -O2 --make main.hs -o main

# Executar
./main
```

### Arquivo de Entrada

O programa lerá os cenários do arquivo `exe.txt` na mesma pasta do executável. Cada linha deve conter quatro números inteiros separados por espaços, representando: `M N L C`.

Exemplo de arquivo `exe.txt`:
```
5 5 0 0
3 4 0 0
6 6 2 2
```

Para mais detalhes, consulte o arquivo `INSTRUCOES_COMPILACAO.md`.

## Estrutura do Projeto

- `hello.hs` - Código fonte principal
- `exe.txt` - Arquivo de entrada com casos de teste
- `Makefile` - Script de compilação automatizado
- `README.md` - Documentação do projeto
- `INSTRUCOES_COMPILACAO.md` - Roteiro detalhado de compilação e execução

## Entregáveis do Trabalho

- Código Fonte: Implementação em Haskell (`hello.hs`)
- Instruções de Compilação: Roteiro completo (`INSTRUCOES_COMPILACAO.md`)
- Makefile: Script de compilação automatizado
- Exemplos de Uso: Arquivos de entrada para teste (`exe.txt`)
- Documentação: README com descrição do projeto

---

**Disciplina**: Linguagens de Programação  
**Curso**: Ciência da Computação  
**Universidade**: Universidade Federal Fluminense (UFF)