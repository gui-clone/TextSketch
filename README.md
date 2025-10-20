# TextSketch DSL

## Descrição resumida da DSL
### Contextualização da linguagem
TextSketch é um Lisp-based DSL para criar desenhos em SVG.

### Motivação
Uma DSL baseada em Lisp permite descrever elementos gráficos de forma declarativa e estruturada, facilitando a reutilização de
desenhos e permite criar formas genéricas que podem ser utilizadas em momentos diferentes.

### Relevância
A possibilidade de descrever elementos gráficos de forma declarativa e reutilizável torna essa DSL uma ferramenta valiosa para a
criação de bibliotecas visuais modulares, favorecendo a consistência e a produtividade em projetos gráficos.

## Slides

[Apresentação](https://docs.google.com/presentation/d/1HeqdyV5coyy11g1CQSkbuJo9nluPk5S59jS896N-iak/edit?usp=sharing)

## Notebook

[basic rules](notebook/basic_rules.ipynb)

## Sintaxe da Linguagem

### Tipos Básicos
- **Ponto**: tupla de números, formado como: `(x y)` — e.g., `(10 20)` .
- **Cor**: escrito como os nomes das cores (em inglês) suportados pelo [SVG](https://www.w3.org/TR/SVG11/types.html#ColorKeywords)
  — e.g., `red`, `blue`, `lightgreen`.

### Elementos Básicos
- **Forma**: um único elemento que pode ter suas características alteradas, além de formar novas formas.
- **Grupo**: conjunto de formas. Não é possível alterar as características das formas que o compõem.
### Rotinas/macros
- **Bézier Curve**: curva ondulada (quadrática) formada por um ponto inicial, ponto final e ponto de controle.
```scheme
(bezier START_POINT END_POINT CONTROL_POINT)
```

- **Form**: cria uma nova forma a partir de outras.
```scheme
(form SHAPES)
```

- **Union**: cria um grupo de formas e de outros grupos.
```scheme
(union SHAPES/GROUPS)
```

- **Fill**: preenche uma forma com uma cor
```scheme
(fill COLOR SHAPE)
```

- **Define SVG**: salva uma forma ou grupo genéricos, recebendo parâmetros para gerar a saída.
```scheme
(defineSVG NAME (ARGS) SHAPE/GROUP)
```

- **New Panel**: cria um painel com largura e altura dados, e retorna uma função que recebe uma forma ou grupo, mostrando-o no painel.
```scheme
(define show (new-panel WIDTH HEIGHT))
(show SHAPE/GROUP)
```

## Exemplos Selecionados
### Exemplo 1:
Input:
```scheme
(define show (new-panel 300 200))
(show (fill green
            (bezier (0 150) (150 150) (75 0))))
```
Output:
```scheme
(<svg width= "300" height= "300" viewBox= "0 0 300 200" > <path d= "M 0 150 Q 75 0 150 150" stroke= "black" fill= "green" stroke-width= "2" /> </svg>)
```
### Exemplo 2:
Input:
```scheme
(define show (new-panel 50 50))

(defineSVG circle ((cx cy) radius)
    (let ((x cx)
          (y cy)
          (r radius))
      (form
        (bezier ((- x r) y ) ((+ x r) y) ( x (+ y (* 2 r))))
        (bezier ((- x r) y ) ((+ x r) y) ( x (- y (* 2 r)))))))

(show (circle (25 25) 10))
```

Output:
```scheme
(<svg width= "300" height= "300" viewBox= "0 0 50 50" > <path d= "M 15 25 Q 25 45 35 25 M 15 25 Q 25 5 35 25" stroke= "black" fill= "none" stroke-width= "2" /> </svg>)
```

## Discussão
A DSL criada cumpre satisfatoriamente a proposta de ser uma liguagem de criação gráfica utilizando SVG de forma declarativa e
reutilizável. A capacidade de criar elementos genéricos e definir formas reaproveitáveis pode facilitar a prototipação
programática de desenhos e possíveis automações em Scheme. Ao invés de expor a interface direta do SVG, que pode se tornar
extremamente complexa, essa DSL possibilita a criação de formas num nível mais alto, além de facilitar a modularização do código,
o que facilita o seu uso em contextos onde o tempo de desenvolvimento é mais crítico.

Apesar de ser propositalmente minimalista, bibliotecas adicionais que definem suas próprias formas e grupos podem tornar o
desenvolvimento ainda mais simples, sem abrir mão da flexibilidade da curva de Bézier. Ao exportar para SVG, um formato aberto,
bem definido e muito usado na indústria, reutilizamos a infraestrutura já existente acerca da exposição dessas imagens, abrangendo
desde formatos digitais compactos até a criação de mídia impressa de alta fidelidade. Por meio de integração com outras
ferramentas, por exemplo, essa DSL torna-se igualmente capaz de exportar imagens para o formato HEIC.

## Conclusão
O desenvolvimento da DSL baseada em Lisp para geração de gráficos em SVG permitiu concluir que é viável criar uma linguagem
reutilizável e adequada para descrever construções gráficas de forma declarativa. Entre os principais desafios enfrentados
estiveram a definição de como guardar as propriedades de cada elemento, a utilização de funções envolvendo strings e o
aprofundamento de como usar macro-higiênico e funções `let/let*` de forma que se extraia a sua capacidade máxima. Dentre as
principais lições aprendidas, destacam-se a utilidade de macros na construção de DSLs e a importância de aprender a estrutura LISP
de código.

# Trabalhos Futuros
O principal ponto de melhora é desenvolver uma função de criar formas que seja compatível com preenchimento de cores. Além disso,
é possível ampliar as ferramentas de criação, como ter a possibilidade de alterar a grossura e cor das bordas, entre outras
capacidades do SVG que não são suportadas pelo modelo atual. Também podem ser implementadas ferramentas de visualização integrada
que facilitem seu uso de forma interativa, como em Jupyter Notebooks.

# Referências Bibliográficas
1. [Manual do Scheme](https://man.scheme.org/)
2. [Manual do Guile](https://www.gnu.org/software/guile/manual/guile.html)
3. [Scalable Vector Graphics (SVG) 1.1](https://www.w3.org/TR/SVG11/)
