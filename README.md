# A 4-state Cache Coherence Protocol
<b>Description:</b><br>

A 4-state 3-hop MESI cache coherence protocol with 4 virtual channels and self-downgrade. The protocol can be verified by [Mur&phi;](https://github.com/tyler-utah/Murphi2019), which is a cache coherence verification system include the Compiler and the description language. It was created by D. L. Dill and R. Melton, and developed by C. Norris Ip and U. Stern from Stanford University. The protocol achieves better performance and less traffic compared to the baseline MSI protocol by adding the extra Exclusive (E) state and enforcing the owner of the cache line to downgrade from Modified (M) to Shared (S) under certain conditions. The codes were prepared for Prof. M. Jeffrey as submission for the University of Toronto ECE1755H "Parallel Computer Architecture and Programming" course assignment 2.

<br><b>Copyright © 2021 [Weixuan Yang](https://www.linkedin.com/in/weixuanyang/)</b>

