---
title: GPU-Accelerating a Neutron Scattering Simulation with CUDA
author: Dan Vonk
tags: cuda, programming, physics
---


I recently completed a project where I improved the performance of a neutron
scattering physics simulation using CUDA. Despite the existing application being
written for super-computer clusters with MPI, writing a CUDA implementation and
allowing for a "hybrid" CPU/GPU computation model that still supports clusters
worked surprisingly well. In the blog, I'll give some of the physics context
behind the program, why neutron scattering simulations are even needed, and then
finally talk about how exactly the CUDA implementation worked. Note that the content is
adapted from an academic report, so the style of writing is a bit more formal
than might be expected from a blog...

Introduction
---


Molecular dynamics simulations involving millions of atoms and timescales in the
microseconds are commonplace in contemporary physics simulations. These
trajectories can then further be verified using the tools of neutron or X-ray
scattering theory to produce scattering intensities which are useful in domains
such as crystallography, where the positions of the peaks in the graph determine
important constants such as the structure factor.

In the Born approximation, the _total scattering amplitude_ is calculated as the
sum of the contributions of all $N$ individual scatterers, i.e. atoms. Given a position vector $bold(r)_n (t)$ provided by
the MD simulation, the overall amplitude arises from the constructive
interference amplitudes, calculated as:

$ A\(bold(q),t\) = sum_(n in {1,...,N}) b_n\(bold(q) \) dot e^(dotless.i dot
bold(q) dot bold(r)_n\(t\)) $

for a scattering vector $bold(q)$, given the atomic prefactor $b_n (bold(q))$,
which for neutron scattering is a fixed constant and for X-rays is calculated as

$ b_n (bold(q)) = sum_(j=1)^x c_j dot e^(-d_j dot |bold(q)|^2) $

where $c_j$ and $d_j$ are tabulated constants. The other relevant equation in
scattering theory is the associated _total scattering intensity_, which is given
by

$ F(bold(q), t) = A(bold(q), t) dot A^\* (bold(q), t) $

where $A^\* (bold(q), t)$ is the complex conjugate of $A(bold(q),t)$
#cite(<lindner2012sassena>). However, in certain experiments, e.g. involving
liquids, the sample may be _isotropic_, that is having no preferred orientation.
Therefore, the scattering intensity becomes independent of the specific
direction of $bold(q)$ and only depends on the magnitude, which usually requires
averaging the signal over all possible orientations of $bold(q)$. Similarly, in
experiments focusing on the dynamics of the system, such as inelastic neutron
scattering, the average behaviour of the system over a timescale is of interest
and therefore requires summing the signal over time.

Although these operations are all computationally simple, the large data-sets
mean that effective use of modern computer hardware is paramount. Specifically,
programs for computing these scattering intensities must be massively
parallelisable in order to scale well on multi-core machines and even clusters.
Furthermore, in high-performance computing on many threads, careful attention
must be paid to memory access patterns in order to make the most effective use
of limited throughput between memory and processors.



<!--more-->
