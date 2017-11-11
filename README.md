# FeynmanDraw
A Tikz macro for easily drawing high-quality Feynman diagrams and a fortran program to automatically draw Feynamn diagrams in condensed matter physics.


## src
#### tikz.tex
The tikz macro that defines primitives for drawing Feynman diagrams. For example, the following code will draw a particle-hole bubble,

```latex
\begin{tikzpicture}
    \draw[bareG] (0,0) to[bend left=60] (1,0);
    \draw[bareG] (1,0) to[bend left=60] (0,0);
\end {tikzpicture}
```
Detailed instruction on the usage of the defined primitive is contained in the file `tikz.tex`.

#### tikzDraw.f90
A fortran program that automatically generates Latex source code when the topology of the diagram is specified.

##### diagram topolgoy
* For an order $n$ diagram, there are $2n$ vertexes. We specify the two vertexes of a single interaction line take consecutive numbers such as `1` and `2`,  `3` and `4`.

* The topology is then determined by the `next(1:2n)` array where `i` and `next(i)` are connected by a Green's function line propagating from vertex `i` to `next(i)`, `i --->--- next(i)`.

## example



