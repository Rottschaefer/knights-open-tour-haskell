# Knight's Open Tour em Haskell

Este projeto é uma implementação do problema "Passeio do Cavalo" (Knight's Tour), desenvolvido para a disciplina de Linguagens de Programação do curso de Ciência da Computação da Universidade Federal Fluminense (UFF).

## 📝 Descrição do Problema

O objetivo é encontrar um "passeio aberto" para um cavalo em um tabuleiro de xadrez de dimensões `M x N`. Um passeio aberto significa que o cavalo visita cada casa do tabuleiro exatamente uma vez, partindo de uma posição inicial `(L, C)`. A restrição adicional é que a casa final do percurso não pode ter um movimento válido para a casa inicial.

## ⚙️ Funcionalidades

- **Leitura de Cenários**: O programa lê um arquivo de texto (`exe.txt` por padrão) que define múltiplos cenários.
- **Estrutura do Arquivo de Entrada**: Cada linha do arquivo deve conter quatro números inteiros, separados por espaços:
  1.  `M`: Número de linhas do tabuleiro.
  2.  `N`: Número de colunas do tabuleiro.
  3.  `L`: Linha inicial do cavalo.
  4.  `C`: Coluna inicial do cavalo.
- **Saída**: Para cada cenário, o programa exibirá na tela o caminho encontrado ou uma mensagem indicando que não foi possível encontrar uma solução.

## 🚀 Como Compilar e Executar

### Pré-requisitos

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)

### Compilação

Para compilar o projeto, utilize o seguinte comando no terminal:

```bash
ghc --make hello
```

### Execução

Após a compilação, um executável chamado `hello` será criado. Para executá-lo, use:

```bash
./hello
```

O programa lerá os cenários do arquivo `exe.txt` e imprimirá os resultados na saída padrão.

## 📦 Entregáveis do Trabalho

- **Código Fonte**: Implementação em Haskell.
- **Instruções de Compilação**: Roteiro completo para compilar e executar o projeto.
- **Exemplos de Uso**: Arquivos de entrada para teste.
- **Relatório de Contribuição** (para grupos): Documento descrevendo a participação de cada membro da equipe.

---

**Disciplina**: Linguagens de Programação  
**Curso**: Ciência da Computação  
**Universidade**: Universidade Federal Fluminense (UFF)