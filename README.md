# DSL TextSketch

## Descrição Resumida da DSL

TextSketch is a Lisp-based DSL for creating SVG drawings.
> Descrição resumida do tema do projeto. Sugestão de roteiro (cada item tipicamente tratado em uma ou poucas frases):
>
> Contextualização da linguagem
>
> Motivação
>
> Relevância
>

## Slides

> Coloque aqui o link para o PDF da apresentação.

## Notebook

[basic rules](notebook/basic_rules.ipynb)

## Sintaxe da Linguagem

#### Basic types
- Point: tuple of numbers, formatted as: `(x y)` — e.g., `(10 20)`
- Color: written name of a color supported by [SVG](https://www.w3.org/TR/SVG11/types.html#ColorKeywords) — e.g., `red`, `blue`, `lightgreen`

#### Atoms
- Bézier curve: curve formed with start point, end point and control point
```
(bezier START_POINT END_POINT CONTROL_POINT)
```
- Group: unite a list of shapes into a new shape
```
(group SHAPES)
```
- Fill: fill a shape with a color
```
(fill COLOR SHAPE)
```
- Define SVG: save a shape with a name. Can be used in other parts of the code
```
(defineSVG NAME (ARGS) SHAPE)
```
#### Create Panel
- New Panel: create a penal with the width and height given, return a function that receive a shape and show it in the panel
```
(define show (new-panel WIDTH HEIGHT))
(show SHAPE)
```
## Exemplos Selecionados

> Coloque um conjunto de exemplos selecionados e os resultados alcançados.

## Discussão

> Discussão dos resultados. Relacionar os resultados com as perguntas de pesquisa ou hipóteses avaliadas.
>
> A discussão dos resultados também pode ser feita opcionalmente na seção de Resultados, na medida em que os resultados são apresentados. Aspectos importantes a serem discutidos: Por que seu modelo alcançou (ou não) um bom resultado? É possível tirar conclusões dos resultados? Quais? Há indicações de direções para estudo? São necessários trabalhos mais profundos?

## Conclusão

> Destacar as principais conclusões obtidas no desenvolvimento do projeto.
>
> Destacar os principais desafios enfrentados.
>
> Principais lições aprendidas.

# Trabalhos Futuros

> O que poderia ser melhorado se houvesse mais tempo?
> Quais possíveis desdobramentos este projeto pode ter?

# Referências Bibliográficas

> Lista de artigos, links e referências bibliográficas.
>
> Fiquem à vontade para escolher o padrão de referenciamento preferido pelo grupo.


