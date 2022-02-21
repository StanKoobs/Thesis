# Econometrics thesis
I wrote this thesis for my BSc in Econometrics and Operations Research from the University of Groningen. 
The title of the thesis is: "Analysis of the finite sample properties of the Power Enhancement technique".
This thesis investigates the finite sample properties of a recently introduced power enhancement technique by Fan et al. (2015) and Kock and Preinerstorfer (2019). This technique has outstandingly strong asymptotic results. Especially in a high-dimensional setting, this technique can be advantageous compared to conventional tests. Yet, it remains unclear how these results carry over to samples of a practical size. In my research, I assumed a Gaussian location model and used this to study some finite sample properties.

There are several files in which I verify my theoretical results. For example, in TheoreticalAnalysisNew.R, I calculate a lower and upper bound here, next to the exact integration and the Monte Carlo approximation.
Moreover, for this study I also ran several simulations which can be found in the files SimulationKP.R and SimulationsFan.R.

### References
Fan, J., Y. Liao, and J. Yao (2015). Power enhancement in high-dimensional cross-sectional tests. Econometrica 83, 1497–1541.

Kock, A.B. and D. Preinerstorfer (2019). Power in high-dimensional testing problems.Econometrica 87, 1055–1069.
