---
title: Matrix Product State (MPS) Circuit Simulation in Rust
author: Dan Vonk
tags: tech, maths, quantum, physics
---

![IBM Quantum Computer](/images/ibm_quantum_comp.jpg "Unless you have one of
these in your cupboard, you might have to settle for classical simulation.")


I was recently playing around with a research artifact from the paper Grafeyn

<blockquote>
<p>
In this paper, we present a hybrid Schrödinger-Feynman
technique which takes advantage of sparsity by selectively
synchronizing Feynman paths. Our hybrid technique partitions the
circuit into kernels (groups of gates) and uses Feynman simulation
within each kernel. It then synchronizes across the kernels by
using Schrödinger-style simulation
</p>
</blockquote>






General-purpose, practical quantum computers do not yet actually exist but with
every passing day there is more and more hullabaloo around them. If they do in
fact emerge at some point in time, then they would offer some quite compelling
properties when compared to classical computers. In particular, they are able to
solve some problems significantly faster than a classical machine would. For
example, integer factorisation is the problem of factoring a positive integer
into its prime factors. It's a decision problem which has a best-known time
complexity $\mathcal{O}(\exp (\log{N}))$ on classical computers. However, the
well-known _Shor's Algorithm_ is able to solve this problem in
$\mathcal{O}(N^{3})$ time, i.e. polynomial time on a quantum computer. This
result has implications in the field of cryptography, where it becomes possible
to break RSA encryption, which currently relies on the difficulty of factoring a
large number $N$ into two prime factors to provide security. In fact, many
quantum algorithms exist which are faster than their classical counterparts and
these could speed up a whole range of tasks from modelling of chemical processes
to linear algebra.

However, quantum computers in their current state are only of limited use. One
reason is because they are highly susceptible to noise, which leads to
errors in computations. Therefore, quantum computers are kept in laboratory
conditions at close to absolute zero with high isolation from the external
environment to maintain coherence. 

Fortunately, one doesn't actually need a quantum computer to design quantum
algorithms or get a better understanding of the technology. For small quantum circuits,
it's perfectly feasible to simulate them on classical computers. This article
will discuss how you can create your own simulator using Rust and give an
introduction into an interesting technique for maintaining performance: tensor networks.

<!--more-->

The fundamental unit of a quantum computer is the qubit, which is analogue to a
classical bit, except that it can be in a superposition (a ``combination'' of)
of 0 and 1 at the same time. Specifically, a qubit $\ket{\psi}$ is an element of
a Hilbert space $\mathcal{H}$ with the basis vectors \[\{ \ket{0}, \ket{1} \} =
\{ {(1,0)}^{T}, {(0,1)}^{T}\},\] such that \[\ket{\psi} = \alpha \ket{0} + \beta
\ket{1}.\] If strictly either $\alpha$ or $\beta$ is equal to 0, then
$\ket{\psi}$ is a classical bit. A qubit must also remain normalised, meaning
that \[|\alpha|^{2} + |\beta^{2}| = 1.\]

Operations can be performed on $\ket{\psi}$ to modify its state and these are
represented by matrices $U$, but because all operations must preserve the
aforementioned norm, these operations must be unitary, i.e. $U U^{\dagger} = I$.
One of the most common operations (or gates) is the Hadamard gate (H gate),
defined as

\[ H = \frac{1}{\sqrt{2}} \begin{pmatrix} 1 & 1 \\ 1 & -1 \\ \end{pmatrix} \]

This operation is often used to balance the amplitudes for $\ket{0}$ and
$\ket{1}$, but for a general state $\ket{\psi}$ performs \[H \ket{\psi} =
\frac{1}{\sqrt{2}} [(\alpha + \beta) \ket{0} + (\alpha - \beta) \ket{1}].\]

Another fundamental operation on a qubit is to \textit{measure} it on the
standard basis, where it collapses to one of the classical basis states
according to probabilities $|\alpha|^{2}$ and $|\beta|^{2}$ respectively. This
operation is usually performed at the end of an algorithm to return a classical
answer with a certain probability, and most quantum algorithms are designed to
``push'' this pair towards one basis vector to avoid ambiguity.

Until now, all of these operations have been simple to simulate on classical
hardware. However, the difficulty arises when one wants to simulate multiple
qubits. This is because the overall state-space $\mathcal{H}$ is the tensor
product between all of the other states

\[ \mathcal{H} = \mathcal{H}_{1} \otimes \mathcal{H}_{2} \otimes \cdots \otimes
\mathcal{H}_{n}\]

and this value grows exponentially such that $\dim(\mathcal{H}) = 2^{n}$. For
example, a quantum system with 10 qubits would require 1024 complex numbers to
describe, while a system with 30 qubits would require approximately one billion
complex numbers to describe. Although this exponential nature is what provides
the additional abilities of quantum computers, it makes it very challenging to
simulate even small systems on classical hardware. However, because simulating
quantum circuits on classical computers is often necessary, due to noise and
difficulty of getting hardware, certain techniques have been developed to more
efficiently perform these simulations.
%-------------------------------------------------------------------------------
\section{GraFeyn}
%-------------------------------------------------------------------------------

\subsection{Motivation} There are two main ways to simulate quantum circuits on
classical machines, Schrödinger-style and Feynman-style simulation and both of
these have trade-offs in terms of time and space requirements.

Schrödinger simulation is perhaps the simpler scheme to understand. Here, the
unitary transformation for each gate in the circuit is directly applied to the
full state vector, i.e for a state $\ket{\psi} \in \mathbb{C}^{2^{n}}$ and a
gate $U \in \mathbb{C}^{2 \times 2}$, the following process occurs. Firstly,
because $U$ operates only on a single qubit, it must be expanded using identity
transformations for the other qubits

\[U_{k} = I_{1} \otimes ... \otimes U \otimes ... \otimes I_{k}\]

then the state can be updated using a matrix transformation

\[ \ket{\psi'} = U_{k} \ket{\psi}.\]

In practice however, programs such as Grafeyn \cite{westrick2024grafeyn} store
the amplitudes for each qubit separately, so a gate can be directly applied to
the intended qubit. Due to needing to store the entire state vector, the space
complexity of this style of simulation is $\mathcal{O}(2^{n})$ for any number of
gates, whilst the time complexity is $\mathcal{O}(m \cdot 2^{N})$ for $m$ gates.

The other approach to quantum circuit simulation is Feynman-style simulation,
which is based on the path integral formulation of quantum mechanics, where a
path is a sequence of computations that the system \textit{might} traverse
during the gate computation. Once all possible paths have been enumerated, they
are summed together and this represents the total amplitudes of the measurement
until the end of the chosen gate.

The advantage of this method is that it does not require storing the entire
(large) state vector. Memory is only needed to track the intermediate values
along each path, which can in fact be done in $\mathcal{O}(n)$ time for a single
path of $n$ qubits. By contrast, the time complexity is much higher, as it must
loop over all $2^{n}$ possible paths.

Therefore, neither of these methods dominate the other, which leads to the idea
that they could be used in conjunction. Indeed, the paper Grafeyn by Westrick
and Liu et al.\ introduces a hybrid circuit simulator with support for both
Schrödinger and Feynman style simulation in an efficient and parallel manner.
The important realisation of the paper is that Feynman simulation becomes
extremely inefficient when the computation branches significantly and returns to
a single point (``combinatorial explosion'') whereas Schrödinger simulation is
more efficient in these cases, as only the state vector must be stored. However,
many parts of circuits have no branching at all (such as \texttt{CX} or
controlled-not gates), therefore it is possible to group these non-interfering
sub-circuits into ``kernels'', which can be simulated using the Feynman
technique, whilst leaving the interfering parts to be simulated using the
Schrödinger technique.

The second realisation in terms of efficiency of the paper is the focus on
\textit{sparsity}. The unitary transformations of many gates in quantum
computing are sparse, meaning they have more zero entries than non-zero ones.
Special algorithms for sparse matrix-vector multiplication exist and are
significantly faster than a naive implementation. This is beneficial for
Schrödinger simulation, which is based on these operations. Similarly,
Feynman-style simulation also benefits from sparsity because fewer paths must be
considered. Grafeyn can therefore perform re-ordering operations on gates (e.g.\
if they have commutativity) in order to improve sparsity both inside of a kernel
(i.e.\ Feynman simulation) and outside (i.e.\ Schrödinger simulation).
