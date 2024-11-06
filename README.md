Doubly robust approach to difference-in-differences which combines elements of imputation-based approach by [Gardner (2021)](doi:10.48550/arXiv.2207.05943) and [Borusyak (2021)](doi.org/10.48550/arXiv.2108.12419) with doubly robust group-time estimation by [Callaway & Sant'Anna (2021)](doi.org/10.1016/j.jeconom.2020.12.001). Building on the mentioned approaches, the estimator is also robust to heterogeneity in treatment effects in cases with mutiple periods and staggered treatment adoption.

The basic idea is to estimate global outcome and propensity models instead of local ones as in [Callaway & Sant'Anna (2021)](doi.org/10.1016/j.jeconom.2020.12.001).

Desirable features from [Callaway & Sant'Anna (2021)](doi.org/10.1016/j.jeconom.2020.12.001)

- Disaggregation and aggregation of group-time average treatment effects allows to target various ATTs
- Base period can be varying instead of fixed base period (typically t = -1)

Issues encountered with [Callaway & Sant'Anna (2021)](doi.org/10.1016/j.jeconom.2020.12.001)

- Local nuisance models need enough treatment and control group observations in each group-time, particularly when multiple covariates are included. Therefore, estimation often fails

Solution:

TODO: add details.
