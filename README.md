# FeynmanDraw
A Tikz macro for easily drawing high-quality Feynman diagrams and a fortran program to automatically draw Feynamn diagrams in condensed matter physics.


## src
#### <font color=blue> tikz.tex </font>
The tikz macro that defines primitives for drawing Feynman diagrams. For example, the following code 

```latex
\begin{equation*}
    \Phi = 
    \frac{1}{2} \times 
    \begin{tikzpicture}[baseline=-0.5ex, scale=2]
        \draw[boldG] (0,0) to[bend left=60] (1,0) ;
        \draw[boldU] (0,0) -- (1,0);
        \draw[boldG] (1,0) to[bend left=60] (0,0) ;
    \end{tikzpicture}
    + \frac{1}{4} \times 
    \begin{tikzpicture}[baseline=-0.5ex, bend angle=70]
        \draw[boldG] (0,0) -- (1,0); \draw[boldG] (1,0) -- (2,0);
        \draw[boldG] (2,0) -- (3,0); \draw[boldG] (3,0) to[bend left=50] (0,0);
        \draw[boldU] (0,0) to[bend left] (2,0);
        \draw[boldU] (1,0) to[bend left] (3,0);
    \end{tikzpicture}
    + \cdots 
\end{equation*}
```
draws the following diagram

![](https://raw.githubusercontent.com/ZhiyuanYao/FeynmanDraw/master/example/Phi.png)                       

Detailed instruction on the usage of the defined primitive is contained in the file `tikz.tex`.

#### tikzDraw.f90
A fortran program that automatically generates Latex source code when the topology of the diagram is specified.

##### diagram topolgoy
* For an order `n` diagram, there are `2n` vertexes. We specify the two vertexes of a single interaction line take integer numbers `2i-1` and `2i`,  `2i-1 ~~~~~~ 2i`.

* The topology is then determined by the `next(1:2n)` array where `i` and `next(i)` are connected by a Green's function line propagating from vertex `i` to `next(i)`, `i --->--- next(i)`.

## example



