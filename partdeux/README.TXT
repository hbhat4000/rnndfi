f subfolder:

Different values of the factor by which we adjust the echo state weight matrix (f in paper). In general, f is kept near 1 to obtain the echo state property, but values above and below are sometimes used. Above 1 tends to produce more divergent trajectories in reservoir activations -- more chaotic. Below 1 tends to generate more convergent, stable activation patterns. I used .75, 1, and 1.25. These networks have size 10, and alpha of .7 (on balance, more impacted by the input). I also ran the PCA and found the top 5 components that predict the counting (a^kb^k).

alpha subfolder:

Setting f to 1, and size to 10 again, I set different values of alpha. This can be more dependent upon its prior state of reservoir (.3), more on input (.7) and in between.

n_echo subfolder:

Exploring different sizes of the reservoir. I explore 10, 20, and 50, and do the PCAs for each (taking the top 5).

For each, I do 10 iterations.

"no_input" is just letting the reservoir play with itself... "with_input" is with the counting task (a^kb^k prediction).