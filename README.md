Manuscript, code, and data for "Unreliable Inferences: A Critique of Partial Observability Models." [[Paper](http://www.carlislerainey.com/files/unreliable.pdf)]

> Methodologists and econometricians advocate the partial observability model as a tool that enables researchers to estimate the distinct effects of a single explanatory variable on two partially observable outcome variables. However, we show that when the explanatory variable of interest influences both partially observable outcomes, the partial observability model estimates are extremely sensitive to misspecification. We use Monte Carlo simulations to show that, under partial observability, minor, unavoidable misspecification of the functional form can lead to substantial large-sample bias, even though the same misspecification leads to little or no bias under full observability.

If you have any comments or suggestion, please [open an issue](https://github.com/carlislerainey/unreliable-inferences/issues) or just [e-mail](mailto:carlislerainey@gmail.com) me.

To replicate these results, you can use Git to clone this repository or download it as a `.zip` archive. The code for the simulations is a bit of a mess at this point, since I've set it up to add to the simulations on each run rather than replicate them. I'll clean that up at some point in the future. The script `fit-anes.R` replicates the results for the illustration with self-reported turnout data.

There are three R scripts in the subdirectory `R`:

- `example.R`: creates the figure for the motivating example
- `link-sims.R`: performs Simulation Study 1
- `mon-sims.R`: performs Simulation Study 2
- `plot-simulations.R`: creates the figures summarizing the simulation studies

Run these scripts in the obvious order to reproduce the results.
