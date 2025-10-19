# DSL TextSketch

## Descrição Resumida da DSL
##### Contextualização da linguagem
TextSketch é um Lisp-based DSL para criar desenhos em SVG.
##### Motivação
Uma DSL baseada em Lisp permite descrever elementos gráficos de forma declarativa e estruturada, facilitando a reutilização de desenhos e permite criar formas genéricas que podem ser utilizadas em momentos diferentes.
##### Relevância
A possibilidade de descrever elementos gráficos de forma declarativa e reutilizável torna essa DSL uma ferramenta valiosa para a criação de bibliotecas visuais modulares, favorecendo a consistência e a produtividade em projetos gráficos.

## Slides

[Apresentação](https://docs.google.com/presentation/d/1HeqdyV5coyy11g1CQSkbuJo9nluPk5S59jS896N-iak/edit?usp=sharing)

## Notebook

[basic rules](notebook/basic_rules.ipynb)

## Sintaxe da Linguagem

#### Tipos Básicos
- Ponto: tupla de números, formado como: `(x y)` — e.g., `(10 20)` 
- Cor: escrito como os nomes das cores (em inglês) suportados pelo [SVG](https://www.w3.org/TR/SVG11/types.html#ColorKeywords) — e.g., `red`, `blue`, `lightgreen`
#### Elementos Básicos
- Formma: um único elemento que pode ter suas características alteradas, além de formar novas formas
- Grupo: conjunto de formas. Não é possível alterar as características das formas que o compõem
#### Funções
- Bézier curve: curva formada por um ponto inicial, ponto final e ponto de controle
```
(bezier START_POINT END_POINT CONTROL_POINT)
```
- Form: cria uma nova forma a partir de outras
```
(form SHAPES)
```
- Union: cria um grupo de formas e de outros grupos
```
(union SHAPES/GROUPS)
```
- Fill: preenche uma forma com uma cor
```
(fill COLOR SHAPE)
```
- Define SVG: salva uma forma ou grupo genéricos, recebendo parâmetros para gerar a saída
```
(defineSVG NAME (ARGS) SHAPE/GROUP)
```
#### Criar Painel
- New Panel: cria um painel com largura e altura dados, retorna uma função que recebe uma forma ou grupo, mostrando-o no painel
```
(define show (new-panel WIDTH HEIGHT))
(show SHAPE/GROUP)
```
## Exemplos Selecionados
##### Exemplo 1:
Input
```
(define show (new-panel 300 200))
(show 
    (fill 
          green
          (bezier (0 150) (150 150) (75 0) )
      )
)
```
Output
```
(<svg width= "300" height= "300" viewBox= "0 0 300 200" > <path d= "M 0 150 Q 75 0 150 150" stroke= "black" fill= "green" stroke-width= "2" /> </svg>)
```
##### Exemplo 2:
Input
```
(define show (new-panel 50 50))

(defineSVG circle ((cx cy) radius)
    (let ((x cx)
          (y cy)
          (r radius))
              (form
              (bezier ((- x r) y ) ((+ x r) y) ( x (+ y (* 2 r) )) )
              (bezier ((- x r) y ) ((+ x r) y) ( x (- y (* 2 r) )) )
         )
    )

)

(show (circle (25 25) 10))
```
Output
```
(<svg width= "300" height= "300" viewBox= "0 0 50 50" > <path d= "M 15 25 Q 25 45 35 25 M 15 25 Q 25 5 35 25" stroke= "black" fill= "none" stroke-width= "2" /> </svg>)
```

> Coloque um conjunto de exemplos selecionados e os resultados alcançados.

## Discussão

A linguagem DLS criada cumpre satisfatoriamente a proposta de ser uma liguagem de criação gráfica utilizando SVG de forma declarativa e reutilizável.
A capacidade de criar elementos genéricos e definir formas reaproveitáveis pode facilitar a prototipação progrmática de desenhos e possíveis automações.

## Conclusão

O desenvolvimento da DSL baseada em Lisp para geração de gráficos em SVG permitiu concluir que é viável criar uma linguagem reutilizável e adequada para descrever construções gráficas de forma declarativa. Entre os principais desafios enfrentados estiveram a definição de como guadar as propriedades de cada elemento, a utilização de funções envolvendo strings e o aprofundamento de como usar macro-higiênico e funções 'let/let*' de forma que extraia a capacidade máxima. As principais lições aprendidas, destacam-se a utilidade de macros na construção de DSLs e aprender a estrutura LISP de código.

# Trabalhos Futuros

O principal ponto de melhora é desenvolver uma função de criar formas que seja compatível com preenchimento de cores.
Além disso, é possível ampliar as ferramentas de criação, como ter a possibilidade de alterar a grossura e cor das bordas.

# Referências Bibliográficas

[Manual Scheme](https://man.scheme.org/) </br>
[Define-syntax](https://www.gnu.org/software/guile/manual/html_node/Syntax-Rules.html) </br>
[Local Variable](https://www.gnu.org/software/guile/manual/html_node/Local-Bindings.html) </br>
