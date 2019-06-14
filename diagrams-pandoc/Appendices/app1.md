
# Derivation of Forces


## Intramolecular Force
We have the expression of potential from intramolecular interaction:
\begin{equation} \label{eq:appendix A-1}
U_{intra} \simeq \frac{1}{2}k(r - r_{0})^{2} + g(r - r_{0})^3
\end{equation}
With value of $ r $ is the distance between atom $i$ and its pair in the molecule and $ r_{0} $, $ k $, $ g $ all are constants,
\begin{equation} \label{eq:appendix A-2}
r = \mid \mathbf{r}_{i} - \mathbf{r}_{pair-i} \mid
\end{equation}
For the expression of force for each atom $ i $ is:
\begin{equation} \label{eq:appendix A-3}
\mathbf{F}_{i} = - \frac{\partial U_{intra}}{\partial \mathbf{r}_{i}}
\end{equation}
We substitute \eqref{eq:appendix A-1} to \eqref{eq:appendix A-3}, then the expression of force become:
\begin{eqnarray}
\nonumber \mathbf{F}_{i} & = & - \frac{\partial (\frac{1}{2}k(r - r_{0})^{2} + g(r - r_{0})^3)}{\partial \mathbf{r}_{i}} \\
\nonumber & = & - \frac{d(\frac{1}{2}k(r - r_{0})^{2} + g(r - r_{0})^3)}{dr} \frac{\partial r}{\partial \mathbf{r}_{i}} \\
\nonumber & = & - (k(r - r_{0}) + 3g(r - r_{0})^2) \frac{\partial r}{\partial \mathbf{r}_i} \\
& = & - (k(r - r_{0}) + 3g(r - r_{0})^2) \frac{(\mathbf{r}_{i} - \mathbf{r}_{pair-i})}{r} \label{eq:appendix A-4}
\end{eqnarray}

##Intermolecular Force: van der Waals Interaction
The expression of potential from repulsive van der Waals interaction is:
\begin{equation} \label{eq:appendix A-5}
U_{v}(k,l) = \sum_{s=1}^{4} \Big\{ Ae^{-\alpha r_{s}} - f(r_{s}) \Big[ \, \Big( \frac{B_{6}}{r_{s}^{6}} \Big) + \Big( \frac{B_{8}}{r_{s}^{8}} \Big) + \Big( \frac{B_{10}}{r_{s}^{10}} \Big) \Big] \, \Big\}
\end{equation}
With $ k $ and $ l $ are the index of interacting molecules, $ \alpha $, $ B_{6} $, $ B_{8} $, $ B_{10} $ all are constants and $ r_{s} $ is the distance between neighboring atoms in molecule $ k $ and $ l $,
\begin{equation} \label{eq:appendix A-6}
r_{s} = \mid \mathbf{r}_{i} - \mathbf{r}_{j} \mid_{i,j = 1,2}
\end{equation}
$ j $ is the index of neighboring atoms at different molecule from $ i $-atom's molecule. The term of $ f(r_{s}) $ has this separate form at some distance $ r_{s} $:
\begin{equation} \label{eq:appendix A-7}
f(r_{s}) =
\begin{cases}
            e^{-\big[ \, \big( \frac{1.28r_{m}}{r_{s}} \big) - 1 \big] \,^2 }, &         \text{if } r_{s} \leq 1.28r_{m},\\
            1, &         \text{if } r_{s} > 1.28r_{m}.
\end{cases}
\end{equation}
With $ r_{m} $ is a constant value. The expression of force for each atom $ i $ is:
\begin{equation} \label{eq:appendix A-8}
\mathbf{F}_{i} = - \sum_{l} \frac{\partial U_{v}(k,l)}{\partial \mathbf{r}_{i}}
\end{equation}
To avoid mixed-up in calculating the force, we separate $ U_{v}(k,l) $ by its separated $ f(r_{s}) $ of distance $ r_{s} $. Substitute $ f(r_{s}) $ from \eqref{eq:appendix A-7} into the equation \eqref{eq:appendix A-5}, then substitute $ U_{v}(k,l) $ in \eqref{eq:appendix A-8} with substituted \eqref{eq:appendix A-5}. For first case, when the distance $ r_{s} $ larger than $ 1.28r_{m} $, the force become:
\begin{equation} \label{eq:appendix A-9}
\mathbf{F}_{i} = - \sum_{l} \frac{\partial}{\partial \mathbf{r}_{i}} \Bigg(\sum_{s=1}^{2} \Big\{ Ae^{-\alpha r_{s}} - \Big[ \, \Big(\frac{B_{6}}{r_{s}^{6}}\Big) + \Big(\frac{B_{8}}{r_{s}^{8}}\Big) + \Big(\frac{B_{10}}{r_{s}^{10}}\Big) \Big] \, \Big\}\Bigg)
\end{equation}
Using chain rule, we can get
\begin{eqnarray}
\nonumber \mathbf{F}_{i} & = & - \sum_{l} \frac{d}{d r_{s}} \Bigg(\sum_{s=1}^{2} \Big\{ Ae^{-\alpha r_{s}} - \Big[ \, \Big(\frac{B_{6}}{r_{s}^{6}}\Big) + \Big(\frac{B_{8}}{r_{s}^{8}}\Big) + \Big(\frac{B_{10}}{r_{s}^{10}}\Big) \Big] \, \Big\}\Bigg) \frac{\partial r_{s}}{\partial \mathbf{r}_{i}} \\
\nonumber & = & - \sum_{l} \sum_{s=1}^{2} \Big\{ -\alpha Ae^{-\alpha r_{s}} + \Big[ \, \Big(\frac{6B_{6}}{r_{s}^{7}}\Big) + \Big(\frac{8B_{8}}{r_{s}^{9}}\Big) + \Big(\frac{10B_{10}}{r_{s}^{11}}\Big) \Big] \, \Big\} \frac{\partial r_{s}}{\partial \mathbf{r}_{i}} \\
\nonumber & = & - \sum_{l} \sum_{s=1}^{2} \Big\{ -\alpha Ae^{-\alpha r_{s}} + \Big[ \, \Big(\frac{6B_{6}}{r_{s}^{7}}\Big) + \Big(\frac{8B_{8}}{r_{s}^{9}}\Big) + \Big(\frac{10B_{10}}{r_{s}^{11}}\Big) \Big] \, \Big\} \\
& & \frac{(\mathbf{r}_{i} - \mathbf{r}_{j})}{r_{s}} \label{eq:appendix A-10}
\end{eqnarray}
Summation of $ U_{v}(k,l) $ become only two is because atom $ i $ in molecule $ k $ only interacting with two atoms from molecule $ l $. The second case is when the distance $ r_{s} $ smaller or equal to $ 1.28r_{m} $, the expression of force become:
\begin{equation} \label{eq:appendix A-11}
\mathbf{F}_{i} = - \sum_{l} \frac{\partial}{\partial \mathbf{r}_{i}} \Bigg(\sum_{s=1}^{2} \Big\{ Ae^{-\alpha r_{s}} - e^{-\big[ \, \big(\frac{1.28r_{m}}{r_{s}}\big) - 1 \big] \,^2 } \Big[ \, \Big(\frac{B_{6}}{r_{s}^{6}}\Big) + \Big(\frac{B_{8}}{r_{s}^{8}}\Big) + \Big(\frac{B_{10}}{r_{s}^{10}}\Big) \Big] \, \Big\}\Bigg)
\end{equation}
Using chain rule, we can get
\begin{eqnarray}
\nonumber \mathbf{F}_{i} & = & - \sum_{l} \frac{d}{d r_{s}} \Bigg(\sum_{s=1}^{2} \{ Ae^{-\alpha r_{s}} - e^{-\big[ \, \big(\frac{1.28r_{m}}{r_{s}}\big) - 1 \big] \,^2 } \Big[ \, \Big(\frac{B_{6}}{r_{s}^{6}}\Big) + \Big(\frac{B_{8}}{r_{s}^{8}}\Big) + \Big(\frac{B_{10}}{r_{s}^{10}}\Big) \Big] \, \Big\} \Bigg) \frac{\partial r_{s}}{\partial \mathbf{r}_{i}} \\
\nonumber & = & - \sum_{l} \sum_{s=1}^{2} \Big\{ Ae^{-\alpha r_{s}} - e^{-\big[ \, \big(\frac{1.28r_{m}}{r_{s}}\big) - 1 \big] \,^2 } \Big( - \Big[ \, \Big( \frac{6B_{6}}{r_{s}^{7}}\Big) + \Big(\frac{8B_{8}}{r_{s}^{9}}\Big) + \Big(\frac{10B_{10}}{r_{s}^{11}}\Big) \Big] \, \\
\nonumber & & + \frac{2.56r_{m}}{r^2_{s}}\Big(\frac{1.28r_{m}}{r_{s}} - 1\Big) \Big[ \, \Big(\frac{B_{6}}{r_{s}^{6}}\Big) + \Big(\frac{B_{8}}{r_{s}^{8}}\Big) + \Big(\frac{B_{10}}{r_{s}^{10}}\Big) \Big] \, \Big) \Big\} \frac{\partial r_{s}}{\partial \mathbf{r}_{i}} \\
\nonumber & = & - \sum_{l} \sum_{s=1}^{2} \Big\{ Ae^{-\alpha r_{s}} - e^{-\big[ \, \big(\frac{1.28r_{m}}{r_{s}}\big) - 1 \big] \,^2 } \Big( - \Big[ \, \Big( \frac{6B_{6}}{r_{s}^{7}}\Big) + \Big(\frac{8B_{8}}{r_{s}^{9}}\Big) + \Big(\frac{10B_{10}}{r_{s}^{11}}\Big) \Big] \, \\
& & + \frac{2.56r_{m}}{r^2_{s}}\Big(\frac{1.28r_{m}}{r_{s}} - 1\Big) \Big[ \, \Big(\frac{B_{6}}{r_{s}^{6}}\Big) + \Big(\frac{B_{8}}{r_{s}^{8}}\Big) + \Big(\frac{B_{10}}{r_{s}^{10}}\Big) \Big] \, \Big) \Big\} \frac{(\mathbf{r}_{i} - \mathbf{r}_{j})}{r_{s}} \label{eq:appendix A-12}
\end{eqnarray}
In general, we can write the expression of force as:
\begin{eqnarray}
\nonumber \mathbf{F}_{i} & = & - \sum_{l} \sum_{s=1}^{2} \Big\{ Ae^{-\alpha r_{s}} - f(r_{s}) \Big( - \Big[ \, \Big( \frac{6B_{6}}{r_{s}^{7}}\Big) + \Big(\frac{8B_{8}}{r_{s}^{9}}\Big) + \Big(\frac{10B_{10}}{r_{s}^{11}}\Big) \Big] \, \\
& & + \frac{f'(r_{s})}{f(r_{s})} \Big[ \, \Big(\frac{B_{6}}{r_{s}^{6}}\Big) + \Big(\frac{B_{8}}{r_{s}^{8}}\Big) + \Big(\frac{B_{10}}{r_{s}^{10}}\Big) \Big] \, \Big) \Big\} \frac{(\mathbf{r}_{i} - \mathbf{r}_{j})}{r_{s}} \label{eq:appendix A-13}
\end{eqnarray}
With expression of $ f(r_{s}) $ as in \eqref{eq:appendix A-7} and expression of $ f'(r_{s}) $ as
\begin{equation} \label{eq:appendix A-14}
f'(r_{s}) =
\begin{cases}
            \frac{2.56r_{m}}{r^2_{s}}\Big(\frac{1.28r_{m}}{r_{s}} - 1\Big) e^{-\big[ \, \big( \frac{1.28r_{m}}{r_{s}} \big) - 1 \big] \,^2 }, &         \text{if } r_{s} \leq 1.28r_{m},\\
            0, &         \text{if } r_{s} > 1.28r_{m}.
\end{cases}
\end{equation}

##Intermolecular Force: Antiferromagnetic Heisenberg Interaction
The expression of potential from classical antiferromagnetic Heisenberg is:
\begin{equation} \label{eq:appendix A-15}
U_{m}(k,l) = J(R_{kl}) \hat{\mathbf{S}}_{k} \cdot \hat{\mathbf{S}}_{l}
\end{equation}
With $ R_{kl} $ is the distance between center of molecule $ k $ and $ l $ and $ \hat{\mathbf{S}}_{k} $ also $ \hat{\mathbf{S}}_{l} $ are the unit vector of molecule spin direction independent to distance. The exchange energy $ J(R_{kl}) $ depends on the distance $ R_{kl} $
\begin{equation} \label{eq:appendix A-16}
J(R_{kl}) =
\begin{cases}
            J_{1}e^{[ \, -a_{1}(R_{kl} - R_{0}) ] \,}, &         \text{if } R_{kl} \leq R_{min},\\
            J_{2}e^{[ \, -a_{2}(R_{kl} - R_{0}) + a_{3}(R_{kl} - R_{0})^2] \,}, &         \text{if } R_{min} < R_{kl} < R_{max},\\
            J_{3}e^{[ \, -a_{4}(R_{kl} - R_{0}) ] \,}, &         \text{if } R_{kl} \geq R_{min}
\end{cases}
\end{equation}
With $ J_{1} $, $ J_{2} $, $ J_{3} $, $ a_{1} $, $ a_{2} $, $ a_{3} $, $ a_{4} $, and $ R_{0} $ are constants value. The expression of force for each atom $ i $ is:
\begin{equation} \label{eq:appendix A-17}
\mathbf{F}_{i} = - \sum_{l} \frac{\partial U_{m}(k,l)}{\partial \mathbf{r}_{i}}
\end{equation}
Where $ R_{kl} $ can be defined as;
\begin{equation} \label{eq:appendix A-18}
R_{kl} = \Bigg| \frac{(\mathbf{r}_{i} + \mathbf{r}_{pair-i})}{2} - \frac{(\mathbf{r}_{j} + \mathbf{r}_{pair-j})}{2} \Bigg|
\end{equation}
We separate the three cases in $ R_{kl} $, to avoid mixed up. First, for the case when $ R_{kl} $ lower or equal to $ R_{min} $
\begin{equation} \label{eq:appendix A-19}
\mathbf{F}_{i} = - \sum_{l} \frac{\partial}{\partial \mathbf{r}_{i}} \Big( J_{1}e^{[ \, -a_{1}(R_{kl} - R_{0}) ] \,} (\hat{\mathbf{S}}_{k} \cdot \hat{\mathbf{S}}_{l}) \Big)
\end{equation}
Using chain rule, we can get:
\begin{eqnarray}
\nonumber \mathbf{F}_{i} & = & - \sum_{l} (\mathbf{S}_{k} \cdot \mathbf{S}_{l}) \frac{d}{d r_{s}} \Big( J_{1}e^{[ \, -a_{1}(R_{kl} - R_{0}) ] \,} \Big) \frac{\partial R_{kl}}{\partial \mathbf{r}_{i}} \\
\nonumber & = & - \sum_{l} \Big( -a_{1}J_{1}e^{[ \, -a_{1}(R_{kl} - R_{0}) ] \,} \Big) \Bigg( \frac{(\mathbf{r}_{i} + \mathbf{r}_{pair-i}) - (\mathbf{r}_{j} + \mathbf{r}_{pair-j})}{4R_{kl}} \Bigg) \\
& & (\hat{\mathbf{S}}_{k} \cdot \hat{\mathbf{S}}_{l})\label{eq:appendix A-20}
\end{eqnarray}
The second case is when $ R_{kl} $ is between $ R_{min} $ and $ R_{max} $,
\begin{equation} \label{eq:appendix A-21}
\mathbf{F}_{i} = - \sum_{l} \frac{\partial}{\partial \mathbf{r}_{i}} \Big( J_{2}e^{[ \, -a_{2}(R_{kl} - R_{0}) + a_{3}(R_{kl} - R_{0})^2] \,} (\hat{\mathbf{S}}_{k} \cdot \hat{\mathbf{S}}_{l}) \Big)
\end{equation}
Using chain rule, we can get:
\begin{eqnarray}
\nonumber \mathbf{F}_{i} & = & - \sum_{l} (\mathbf{S}_{k} \cdot \mathbf{S}_{l}) \frac{d}{d r_{s}} \Big( J_{2}e^{[ \, -a_{2}(R_{kl} - R_{0}) + a_{3}(R_{kl} - R_{0})^2] \,} \Big) \frac{\partial R_{kl}}{\partial \mathbf{r}_{i}} \\
\nonumber & = & - \sum_{l} \Big( -a_{2} J_{2}e^{[ \, -a_{2}(R_{kl} - R_{0}) + a_{3}(R_{kl} - R_{0})^2] \,} + 2a_{3}(R_{kl} - R_{0}) \\
\nonumber & & J_{2}e^{[ \, -a_{2}(R_{kl} - R_{0}) + a_{3}(R_{kl} - R_{0})^2] \,} \Big) \Bigg( \frac{(\mathbf{r}_{i} + \mathbf{r}_{pair-i}) - (\mathbf{r}_{j} + \mathbf{r}_{pair-j})}{4R_{kl}} \Bigg) \\
& & (\hat{\mathbf{S}}_{k} \cdot \hat{\mathbf{S}}_{l})\label{eq:appendix A-22}
\end{eqnarray}
And for the third case, when $ R_{kl} $ is larger than $ R_{max} $
\begin{equation} \label{eq:appendix A-23}
\mathbf{F}_{i} = - \sum_{l} \frac{\partial}{\partial \mathbf{r}_{i}} \Big( J_{3}e^{[ \, -a_{4}(R_{kl} - R_{0}) ] \,} (\hat{\mathbf{S}}_{k} \cdot \hat{\mathbf{S}}_{l}) \Big)
\end{equation}
Using chain rule, we can get:
\begin{eqnarray}
\nonumber \mathbf{F}_{i} & = & - \sum_{l} (\mathbf{S}_{k} \cdot \mathbf{S}_{l}) \frac{d}{d r_{s}} \Big( J_{3}e^{[ \, -a_{4}(R_{kl} - R_{0}) ] \,} \Big) \frac{\partial R_{kl}}{\partial \mathbf{r}_{i}} \\
\nonumber & = & - \sum_{l} \Big( -a_{4}J_{3}e^{[ \, -a_{4}(R_{kl} - R_{0}) ] \,} \Big) \Bigg( \frac{(\mathbf{r}_{i} + \mathbf{r}_{pair-i}) - (\mathbf{r}_{j} + \mathbf{r}_{pair-j})}{4R_{kl}} \Bigg) \\
& & (\hat{\mathbf{S}}_{k} \cdot \hat{\mathbf{S}}_{l})\label{eq:appendix A-24}
\end{eqnarray}
In general, we can write the force as
\begin{equation} \label{eq:appendix A-25}
\mathbf{F}_{i} = - \sum_{l} J(R_{kl})g(R_{kl}) \Bigg( \frac{(\mathbf{r}_{i} + \mathbf{r}_{pair-i}) - (\mathbf{r}_{j} + \mathbf{r}_{pair-j})}{4R_{kl}} \Bigg) (\hat{\mathbf{S}}_{k} \cdot \hat{\mathbf{S}}_{l})
\end{equation}
Where term of $ J(R_{kl}) $ expressed as in \eqref{eq:appendix A-16} and the term of $ g(R_{kl}) $ expressed as
\begin{equation} \label{eq:appendix A-26}
g(R_{kl}) =
\begin{cases}
            -a_{1}                         , &         \text{if } R_{kl} \leq R_{min},\\
            -a_{2} + 2a_{3}(R_{kl} - R_{0}), &         \text{if } R_{min} < R_{kl} < R_{max},\\
            -a_{4}                         , &         \text{if } R_{kl} \geq R_{max}
\end{cases}
\end{equation}

