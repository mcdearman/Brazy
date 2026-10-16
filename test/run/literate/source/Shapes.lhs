\documentclass{article}
\begin{document}

Shapes, written in the LaTeX style of literate Haskell.

\begin{code}
module Shapes (Shape (..), area) where

data Shape = Square Double | Circle Double
\end{code}

The area of each, where a circle's is taken with $\pi$ as three.

\begin{code}
area :: Shape -> Double
area (Square side) = side * side
area (Circle radius) = 3 * radius * radius
\end{code}

\end{document}
