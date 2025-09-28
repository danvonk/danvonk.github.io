---
title: Matrix Product State (MPS) Circuit Simulation in Rust
author: Dan Vonk
tags: tech, quantum, physics, Rust
---

![IBM Quantum Computer](/images/ibm_quantum_comp.jpg "Bells and whistles: unless you have one of
these in your cupboard, you might have to settle for classical simulation.")

I was recently playing around with a research artefact from the paper
[Grafeyn](https://cs.nyu.edu/~shw8119/24/qce24-grafeyn.pdf) which implemented some
interesting techniques for circuit simulation. Also, it was written in Rust, so
that doubly piqued my interest! Here's what the paper has to say about itself...

<!--more-->

<blockquote>
<p>
In this paper, we present a hybrid Schrödinger-Feynman
technique which takes advantage of sparsity by selectively
synchronizing Feynman paths. Our hybrid technique partitions the
circuit into kernels (groups of gates) and uses Feynman simulation
within each kernel. It then synchronizes across the kernels by
using Schrödinger-style simulation.
</p>
</blockquote>

Background
----------

So there are two main ways to simulate quantum circuits on classical machines:
Schrödinger-style and Feynman-style simulation. Both of these have trade-offs in
terms of time and space requirements. Schrödinger simulation is the simpler
scheme to understand, where the unitary transformation for each gate in the
circuit is directly applied to the full state vector. Note that in practice,
simulators store the amplitudes for each qubit separately, so a gate can be
directly applied to the intended qubit without formulating a global state over
all qubits. Nevertheless, due to needing to store every qubit's amplitude, the
space complexity is $\mathcal{O}(2^{n})$  for any number of gates, whilst the
time complexity is $\mathcal{O}(m \cdot 2^{N})$ for $m$ gates.

The other approach to quantum circuit simulation is Feynman-style simulation,
which is based on the path integral formulation of quantum mechanics, where a
path is a sequence of computations that the system _might_ traverse during the
gate computation. Once all possible paths have been enumerated, they are summed
together and this represents the total amplitudes of the measurement until the
end of the chosen gate. The advantage of this method is that it does not require
storing the entire (large) state vector. Memory is only needed to track the
intermediate values along each path, which can be done in
$\mathcal{O}(n)$ time for a single path of $n$ qubits. However, by contrast, the time
complexity is much higher, as it must loop over all $2^{n}$ possible paths.

Therefore, neither of these methods dominate the other, which leads to the idea
that they could be used in conjunction. The paper Grafeyn does just this with
the additional ealisation that Feynman simulation becomes inefficient
when the computation branches significantly and then returns to a single point
("combinatorial explosion"), whereas Schrödinger simulation is more efficient in
these cases as only the state vector must be stored. However, many parts of
circuits have no branching at all (such as CX or CNOT gates). Therefore it is
possible to group these non-interfering sub-circuits into "kernels", which can
be simulated using the Feynman technique, whilst leaving the interfering parts
to be simulated using the Schrödinger technique.


Matrix Product States
---------------------

An interesting middle-ground between these two methods are _Tensor
Networks_, which are a collection of methods for representing wave functions in
a more compact manner. The simplest method is the _matrix product state_
(MPS), which is a one-dimensional chain of tensors connected to each other by a
_bond dimension_. I thought it would be interesting to compare MPS with the
hybrid method from the paper, so I implemented it alongside the existing code in Rust.

Back to MPS: instead of storing the $c_{1}, \dots ,c_{n}$ weights (amplitudes
for each qubit) explicitly, it decomposes them into a product of $N$
tensors

$$ c_{1}, \dots, c_{n} = A_{i 1} A_{i 2} \dots A_{i N} $$

Each of these corresponds to one of the qubits (called a _site_) and is a rank-3 tensor
$A^{[k]}(\alpha_{k-1},i_{k},\alpha_{k})$. The index $\alpha_{k-1}$ is the
left-bond index and represents the entanglement between the current site and
all other sites to the left. Meanwhile $i_{k} \in \{0,1\}$ represents the _physical
dimension_ and corresponds to the state of 0 and 1 at this
site. Finally, $\alpha_{k}$ is the _right bond index_ and represents the same
information as the subsequent site's left bond index.

In order to model the MPS form in code, a numerics library is needed so that
tensors can be represented and a higher-order singular-value decomposition (SVD) can be applied on them to
bring the tensors back into MPS form once a gate has been applied. As Grafeyn is
written in Rust, this choice was more complicated than initially expected. The
most common crate in the Rust ecosystem for representing tensors is
_ndarray_. However, this crate does not contain any linear algebra
operations such as the SVD. Therefore, a companion crate _ndarray-linalg_
must be used to provide these features. This sounds acceptable in theory, but
some problems were encountered while trying to actually use these crates!
Firstly, _ndarray-linalg_ is not compatible with the latest version of
_ndarray_ (quite strange I must say), so an older version must be used, which means important
library features become missing. For example, _ndarray_ stores its
tensors in row-major format, but when one wants to use the SVD on this tensor,
the _ndarray-linalg_ crate will call its bindings into the
OpenBLAS C library, which uses column-major ("Fortran") format.
However, no automatic conversion between formats will occur, so one must do this
manually. Unfortunately, this older version of _ndarray_ does not support
the `into_col_major` functions, so this also needs to be done by hand
by transposing the tensors. This is also not completely trivial as the tensor
objects in this library are kept as "views" into the underlying data and so it
is difficult to know when the actual operations will be executed. This was all
pretty dissapointing for a Rust fan as I know this would have all been trivial
in C++ with a library like _Eigen_.

Due to all of these difficulties, I decided to avoid using any tensor
libraries and decompose the rank-3 tensors into collections of matrices, which
allows the use of the _nalgebra_ crate, which supports SVD natively and
does not require any conversions from row-major format:

```rust
pub struct MPSState {
    // One component in the pair for Left |0> and Right |1> respectively.
    pub tensors: Vec<(DMatrix<Complex>, DMatrix<Complex>)>,
    // Bond dimensions between sites: (dL, dR)
    pub bond_dims: Vec<(usize, usize)>,
    // Number of sites (qubits)
    pub n_sites: usize,
}
```
Given the `MPSState` representation, it is now possible to apply gates to
the matrix product state. The simplest gates to apply are ones which operate
only on one qubit, such as the T, H or Sdg gates.
Here one simply needs to identify the site which the gate operates on and apply
the gate's unitary matrix to the physical dimensions of the tensors:

```rust
let (tensor_0, tensor_1) = &mut self.tensors[site];
let mat = &gate.mat;

// Apply the gate to the |0> and |1> components of the site
let new_tensor_0 = tensor_0.clone() * mat[(0, 0)]
                 + tensor_1.clone() * mat[(0, 1)];
let new_tensor_1 = tensor_0.clone() * mat[(1, 0)]
                 + tensor_1.clone() * mat[(1, 1)];

*tensor_0 = new_tensor_0;
*tensor_1 = new_tensor_1;
```

However, applying a two-qubit gate is significantly more complex and is a bit too long
to be shown as a code snippet. The first case to consider is when a gate is
applied to two adjacent qubits and goes something like this:

1. Input: Site indices $l$ and $r$, gate $g$. Check $l < r$ else swap indices.
3. Retrieve tensors pairs \texttt{(tensor1\_0, tensor1\_1)} and \texttt{(tensor2\_0, tensor2\_1)} from MPS state.
4. Build joint $A$ tensor of shape $(d_{L} \cdot d_{R}, 4)$ by iterating over the tensor and copying values from the site tensors:
    a. Apply gate: $A' = A \cdot g$.
    b. Reshape $A'$ to $(d_{L} \cdot 2, d_{R} \cdot 2)$ by manual iteration otherwise the site
        will become permanently fused after SVD application.
    c. Perform SVD: $A' = U \Sigma V^{T}$ and truncate the number of singular
        values $\Sigma$ by a maximum threshold defined in the configuration (\texttt{max\_bond\_threshold}).
    d. Scale the columns of $U$ and the rows of $V^{T}$ by the singluar
        values (balanced splitting of the entanglement information).
    e. $U$ now has shape $(d_{L} \cdot 2, |\Sigma|)$, now split into two
        $(d_{L}, |\Sigma|)$ for each physical dimension and store back in the
        correct site in \texttt{MPSState}.
    f. $V^{T}$ has shape $(|\Sigma|, d_{R} \cdot 2)$ so likewise split it
        back into a slice for each physical dimension and store in the correct site.


This process only works for applying gates to adjacent qubits. If the sites are
not adjacent, they must be swapped until they become adjacent. This is done by
inserting Swap gates into the gate stream. Once the gate has been
successfully applied, the sites are simply swapped in the reverse direction until
their original sites are reached again.

Grafeyn also supports gates that affect three sites, such as CSwap or CCX.
Although there are advanced methods that are able to apply these gates directly
to the MPS state, I took the easier path and used algebraic rewriting patterns
to decompose them into two-qubit gates, which greatly simplified the process.

Performance
-----------

The performance of the existing simulators was compared to the new MPS simulator
in some example circuits. The first circuit, _cascading entanglement_ has 10
qubits and starts with an H gate on site 0, it then entangles this qubit with
the rest of the qubits by using CX gates. The second experiment is
`grover_n2` is taken from QASMBench and is a well-known quantum search
algorithm. It is able to search a list in $\mathcal{O} (N^{1/2})$ time rather
than $\mathcal{O}(N)$ for a classical computer. The third experiment,
_rotations_, uses several rotation gates but does not create
entangelement between distant qubits. Finally, _VQE_ is another standard
circuit taken from QASMBench and is an algorithm which approximates the lowest
eigenvalue of a Hamiltonian, a common operation in quantum computing. As seen in
the chart, the performance of the MPS simulator
usually lies somewhere in between the sparse and dense simulators. This was 
expected considering many of these experiments have moderate
entanglement and the performance of the MPS simulator is directly related to the
dimensions of its tensors, which is governed by the maximum bond dimension
$\chi$, which is itself related to the amount of entangelement in the system.


![Runtime performance on selected
experiments](/images/mps_runtime_performance.svg "MPS simulation compared with
the existing Grafeyn simulators.")

One of the limitations of the current MPS setup is that it is hard to achieve
good parallelism with it. It's relatively trivial to apply 1Q gates in parallel
with it, as long as these touch different sites. However, as soon as 2Q gates
need to be applied, things get more complicated, in part because the whole state
might be need to locked if the gate operation must swap two sites on opposite ends of the MPS
until they become adjacent, preventing the application of any other gates. By
contrast, both the dense simulator and sparse simulators make use of parallelism
and the dense one can even make use of CUDA through the scripting language
_Futhark_. Nevertheless, switching to and from MPS might still make sense in
some scenarios, e.g. dense states with low entanglement.

