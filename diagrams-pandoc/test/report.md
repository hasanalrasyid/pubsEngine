---
title:    "Report \\ Cooperative Studies with JAIST"
author:   "Marleni Wirmas (1715011068)"
---
# Basic properties of protein and nucleic acids
## Basic principle of protein
### Unit structure of protein
There are 4 levels of protein organization:

* Primary structure \
  It consists of a chain arrangement/sequence of amino acids that are joined together to make protein. It usually uses abbreviations for the amino and residues. The example in figure (1) is Polypeptide amino.

* Secondary structure \
  This is the region within the long protein chains organized into regular structures known as alpha-helices ($\alpha$-helices) and beta sheet.

* Tertiary structure \
  Tertiary structure is a description of the way that whole chain from one or several units folds and forms 3-dimensional shape, called domain.

* Fourth order structure \
  This structure is formed from several tertiary structure proteins or domains with more complex shape.

![Structure of protein](./Figures/Figure1)

<!--
\begin{figure}[h!]
  \centering
  \includegraphics[width=0.7\linewidth]{Figure1}
  \caption{Structure of protein}
  \label{fig:figure1}
\end{figure}
-->

### Basic motif of protein
There are two types of secondary structure for hydrophobic type:

* $\alpha$-helix\
  This is one of common motif in secondary structure of protein. It consists of hydrogen bond with conformation of N-H group donates a hydrogen bond to the C=O group of amino acid. This structure has small Coulomb interaction in hydrogen bond and has dipole moment.

* $\beta$ sheet\
  Beta sheet is also one of common motif in protein, consist of $\beta$ strands connected hydrogen bonds, forming a generally twisted and pleated sheet.

![Secondary alpha-helix and beta sheet structures](./Figures/Alpha_beta_structure_full)
<!--
\begin{figure}[h!]
  \centering
  \includegraphics[width=0.7\linewidth]{Alpha_beta_structure_(full)}
  \caption{Secondary alpha-helix and beta sheet structures}
  \label{fig:alphabetastructurefull}
\end{figure}
-->

## Structure of nucleic acid, DNA and protein

###  3 structures of DNA:
  DNA has 3 conformation that include A-DNA, B-DNA, and Z-DNA forms

* A-DNA, has same periodic double helical structure\
* B-DNA, slightly similar to A-DNA, but with longer and less compact than A-DNA\
* Z-DNA, has double helical structure which the helix winds to the left in a zigzag pattern.

![3 types of DNA conformations: A-DNA, B-DNA, and Z-DNA](./Figures/Dnaconformations)
<!--
\begin{figure}[h!]
  \centering
  \includegraphics[width=0.4\linewidth]{Dnaconformations}
  \caption{3 types of DNA conformations: A-DNA, B-DNA, and Z-DNA}
  \label{fig:dnaconformations}
\end{figure}
-->

# Lipid and membrane protein
  Membrane is a function of protein that divides the biological cell into inside and outside part.
  Functions of lipid bilayer are formation of boundary, permeability, and domain formation.
  Lipids are divided into 2 types, hydrophobic and amphiphilic molecule. \

##  Phospholipid
  This is one kind of lipid which is a major component in all cell membranes.
  Phospholipids can form lipid bilayers because of their amphiphilic characteristic.
  There are 3 kinds of self-assemble and conformation of phospholipid:

* Spherical liposome\
* Bilayer\
* Micelle\
  Micelles and bilayers form in the polar medium by a process of hydrophobic effect.

![Self-organization of phospholipids: a spherical liposome, a micelle, and a lipid bilayer.](./Figures/phospholipid)
<!--
  \begin{figure}[h!]
    \centering
    \includegraphics[width=0.5\linewidth]{phospholipid}
    \caption{Self-organization of phospholipids: a spherical liposome, a micelle, and a lipid bilayer.}
    \label{fig:phospholipid}
  \end{figure}
-->


# Molecular dynamics simulation
## Simulation (Computational science)
  Simulation is the one that connect the theory and experiment in science.
  When the theory contributes in analysis and prediction, and experiments are contributing in confirming the theory and developing, simulation takes part in calculation in advanced theoretical and virtual simulation experiment.
  Simulation is using the computer to do things, such as reproduce, understand, and predict the theory and also the experiment.
  Simulation is applied in many fields, such as science, pharmacy, climate, earthquake, finance and social, and many other things.

## Supercomputer\
  Supercomputer is a computer with high level of performance.
  One of the biggest supercomputer in Japan is K Computer, located in Kobe, is having 705,024 cores and 10.510 PFlops (floating-point operations per second).
  Supercomputer is practically used in computational science, such as biological, material, earth, and fundamental physical sciences.
  Also, supercomputer is used in simulation of next-generation technology.

## Computational science in biology\
  Computational science is usually used in exploring phenomena in biological system; substances, structure and functions of proteins, membranes, and enzymes, energy and chemical reaction, and signal transduction.
  These can be modeled using molecular dynamics (MD) simulation.
  The fundamental parts in MD are analytical mechanics (Newton's equation), Schrodinger's equation in quantum mechanics, statistical mechanics, and electromagnetization principle.


###  _Brownian Dynamics_
  Brownian dynamics describes the physical phenomena of zig-zag motion of particle. The motion is arising from collision between the particles. This motion resulted diffusion equation that obeys the law of mass conservation, formulated in the equation:
  \begin{equation}
    \frac{\partial \rho (x,t)}{\partial t} = -\frac{\partial J(x,t)}{\partial x}
  \end{equation}
  where $\rho (x,t)$ is density of material and $J (x,t)$ is flux, the rate of flow.

###  _Langevin Equation_
  This equation describes the \textit{Brownian motion} that uses Newton's equation of motion and degree of freedom. $\varrho$ is denoted as the friction coefficient.
  \begin{equation}
    m\frac{d^2 x}{d t^2} = F - \varrho \frac{dx}{dt} + \xi (t)
  \end{equation}

###  Einstein's relational expression:
  \begin{equation}
    D=\frac{k_BT}{\varrho}
  \end{equation}

