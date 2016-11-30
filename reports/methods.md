## Primary production

Photosynthetic available radiation (PAR) just bellow ice was derived from the total energy measured every 10 minutes over a period of 24 hours by the Polarstern weatherstation. Hourly PAR were then estimated by integrating measurements taken at each hour (n = 6). At each hour, PAR at different depths (2.1m, 5.0m, 10.0m, 15.0m, 30.0m, 50.0m) were estimated using an attenuation coefficient (kdPAR) of 0.15 m-1 (Christian Katlein, pers. comm.). Hourly primary production at each depth was then estimated using fitted photosynthetic parameters estimated from P vs E curves with the following equation (Platt et al., 1980):

EQUATION HERE

where x, y, z

The non-linear fitting was done using the Levenberg-Marquardt algorithm from the minpack.lm R package (Timur2016). At each depth primary production was estimated using photosynthetic parameter and PAR estimates. Finally, daily primary production was calculated by integrating production over the depths.

## Error estimation

A Monte-Carlo procedure has been used to propagate the incertitude of the fitted photosynthetic parameters on the estimate daily primary production. At each depth, a total of 10 000 simulations (i.e. P vs E curves) were performed by randomly sampling parameter values based a multivariate normal distribution of the fitted parameters. Using the generated curves, depth and daily integrated primary production rates were calculated as previously explained. The standard deviation of the 10 000 integrated daily primary production rates was then used as a measure of the uncertainty around the estimated value of primary production.


## References

Plat, T., C.L. Gallegos and W.G. Harrison (1980). Photoinhibition o photosynthesis in natural assemblages of marine phytoplankton. J. Mar. Res. 38:687-701.
Timur V. Elzhov, Katharine M. Mullen, Andrej-Nikolai Spiess and Ben Bolker (2016). minpack.lm: R Interface to the Levenberg-Marquardt Nonlinear Least-Squares Algorithm Found in MINPACK, Plus Support for Bounds. R package version 1.2-1. https://CRAN.R-project.org/package=minpack.lm
