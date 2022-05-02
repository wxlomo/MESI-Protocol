# A 4-state Cache Coherence Protocol
<b>Description:</b><br>

A 4-state 3-hop MESI cache coherence protocol with 4 virtual channels and self-downgrade. The protocol can be verified by [Mur&phi;](https://github.com/tyler-utah/Murphi2019), which is a cache coherence verification system that includes the Compiler and the description language. It was created by D. L. Dill and R. Melton, and developed by C. Norris Ip and U. Stern from Stanford University. The protocol achieves better performance and less traffic compared to the baseline MSI protocol by adding the extra Exclusive (E) state and enforcing the owner of the cache line to downgrade from Modified (M) to Shared (S) under certain conditions. The codes were prepared for Prof. M. Jeffrey as a submission for the University of Toronto ECE1755H "Parallel Computer Architecture and Programming" course assignment 2.

For running the code, please:
1) Copy the Mur&phi; source code file "mesi.m" to the Mur&phi; compiler folder.
2) Change directory to the compiler folder.
3) Build the Mur&phi; compiler binary:
    ```
    cd src
    make
    make install
    ```
4) Compile the source code:
    ```
    cd ..
    bin/mu.x86_64 mesi.m
    ```
5) Compile the verifier executable:
    ```
    make
    ```
6) Run the executable:
    ```
    ./mesi -m20
    ``` 
   `-m20` - Expand the memory limit of the hash table to 20 Mbytes.

<br><b>File List:</b><br>
<table border="0">
    <tr>
        <td>Murphi</td>
        <td>Mur&phi; verifier compiler foler</td>
    </tr>
    <tr>
        <td>README.md</td>
        <td>This file</td>
    </tr>
    <tr>
        <td>mesi.m</td>
        <td>Protocol source code</td>
    </tr>
</table>

<br><b>Copyright © 2021 [Weixuan Yang](https://www.linkedin.com/in/weixuanyang/)</b>

