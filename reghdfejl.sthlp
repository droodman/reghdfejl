{smcl}
{* *! version 0.2.1 23nov2023}{...}

{title:Title}

{p2colset 5 18 20 2}{...}
{p2col :{cmd:reghdfejl} {hline 2}}Linear regression with multiple fixed effects, accelerated by Julia{p_end}
{p2colreset}{...}

{marker syntax}{...}
{title:Syntax}

{pstd}
{bf:Fixed effects regressions:}

{p 8 15 2} {cmd:reghdfejl}
{depvar} [{indepvars}] 
{ifin} {weight} {cmd:,} {opth a:bsorb(reghdfejl##absorb:absvars)} [{help reghdfejl##options_table:options}]{p_end}

{pstd}
{bf:Instrumented fixed effects regressions:}

{p 8 15 2} {cmd:reghdfejl}
{depvar} [{indepvars}] {cmd:(}{it:endogvars} {cmd:=} {it:instruments}{cmd:)}
{ifin} {weight} {cmd:,} {opth a:bsorb(reghdfejl##absorb:absvars)} [{help reghdfejl##options_table:options}]{p_end}

{marker options_table}{...}
{synoptset 22 tabbed}{...}
{synopthdr}
{synoptline}
{syntab:Standard FEs {help reghdfejl##opt_absorb:[+]}}
{synopt: {opth a:bsorb(reghdfejl##absorb:absvars)}}categorical variables representing the fixed effects to be absorbed{p_end}
{synopt: {cmdab:a:bsorb(}{it:...}{cmd:,} {cmdab:save:fe)}}save all fixed effect estimates with the {it:__hdfe*} prefix{p_end}

{syntab:Model {help reghdfejl##opt_model:[+]}}
{synopt: {opth vce:(reghdfejl##model_opts:vcetype)}}{it:vcetype} may be {opt un:adjusted} (default), {opt r:obust} or {opt cl:uster} {help fvvarlist} (allowing multi-way clustering){p_end}
{synopt: {opth res:iduals(newvar)}}save regression residuals; required for postestimation "{it:predict <varname>, d}" {p_end}

{syntab:Optimization {help reghdfejl##opt_optimization:[+]}}
{synopt:{opt tol:erance(#)}}criterion for convergence. default is 1e-8{p_end}
{synopt:{opt iter:ate(#)}}maximum number of iterations; default is 16,000{p_end}
{synopt:{opt nosamp:le}}will not create {it:e(sample)}, saving some space and speed{p_end}
{synopt:{opt threads(#)}}number of CPU threads Julia should use{p_end}
{synopt:{opt gpu}}use NVIDIA or Apple Silicon GPU{p_end}

{syntab:Reporting {help reghdfejl##opt_reporting:[+]}}
{synopt:{opt l:evel(#)}}set confidence level; default is {cmd:level(95)}{p_end}
{synopt:{it:display options}}{help ml##display_options:standard options} governing results display{p_end}
{synoptline}
{p 4 6 2}{it:depvar} and {it:indepvars} may contain {help tsvarlist:factor variables} and {help tsvarlist:time-series operators}.
{it:depvar} cannot be of the form {it:i.y} though, only {it:#.y} (where # is a number){p_end}


{marker description}{...}
{title:Description}

{pstd}
{cmd:reghdfejl} is designed as a slot-in replacement for {help reghdfe}. It is missing some features of {cmd:reghdfe}. But it
can run much faster because it relies on the Julia package {browse "https://github.com/FixedEffects/FixedEffectModels.jl":FixedEffectsModel.jl},
{cmd:FixedEffectsModel.jl} implements the same core method as {cmd:reghdfe} (Correia 2016). It can also fit instrumental variables models
with two-stage least squares. In this capacitiy it is not as full-featured as {cmd:ivreghdfe}, because it does not (yet) work as a wrapper for 
{cmd:ivreg2}. Here too, though, it can be much faster.

{pstd}
To run, {cmd:reghdfejl} requires the Stata command {cmd:jl} be installed; {stata ssc install jl} should suffice. It also of course requires
that {browse "https://github.com/JuliaLang/juliaup#installation":Julia be installed}, which is free. Because Julia performs just-in-time
compilation, and because {cmd:reghdfejl} may need to install Julia packages, there can be long lags on first use. Just give
it time. After, it can run as much as 10 times faster on hard problems.

{pstd}
{cmd:reghdfejl} lacks the following {cmd:reghdfe} features:

{p 4 6 0}
* It does not correct for collinearity the estimate of the degrees of freedom consumed by absorbed fixed effects. {cmd:reghdfe}
displays these corrections in a table after the main results. {cmd:reghdfejl} does not.

{p 4 6 0}
* It does not offer the {help reghdfe##opt_group_fes:Group FE} features.

{p 4 6 0}
* It does not allow control over whether the constant term is reported. The constant is always absorbed.

{p 4 6 0}
* It does not offer options such as {cmdab:tech:nique()} that give finer control over the algorithm, for the sake of speed.

{pstd}
{cmd:reghdfejl} starts by copying the data needed for estimation into a Julia DataFrame. Duplicating the data takes time, and it takes memory. In
extreme cases, it will more than double the storage demand because even variables stored in Stata in small types, such as {cmd:byte}, will be stored 
double-precision--8 bytes per value--in Julia. If the memory demand is too great, performance will plummet. {cmd:reghdfejl} therefore is most 
useful when the number of non-absorbed regressors is not too high and the number of absorbed terms is high, for then the computational efficiency
of Julia shines.

{pstd}
{cmd:reghdfejl} offers two novel options that can increase speed, though in the author's experience they usually don't matter. The {opt threads(#)}
option can reduce the number of CPU threads Julia uses. The default number---and the maximum that {cmd:reghdfejl} can access--is set by 
the {browse "https://docs.julialang.org/en/v1/manual/multi-threading/":system environment variable JULIA_NUM_THREADS}. The variable can be 
an integer or it can be {cmd:auto}. On CPUs with efficiency as well as performance cores, it is sometimes faster to reduce the number of threads
to the number of performance cores, in order to reduce dependence on the slower efficiency cores. Further reductions can also help, if the overhead
of multithreading exceeds the benefits.

{pstd}
The other novel option, {cmd:gpu} specifies the use of NVIDIA or Apple Silicon GPUs for computation. This {it:might} increase speed.


{marker absorb}{...}
{title:Absorb() syntax}

{synoptset 22}{...}
{synopthdr:absvar}
{synoptline}
{synopt:{it:varname}}categorical variable to be absorbed{p_end}
{synopt:{cmd:i.}{it:varname}}categorical variable to be absorbed (same as above; the {cmd:i.} prefix is always implicit){p_end}
{synopt:{cmd:i.}{it:var1}{cmd:#i.}{it:var2}}absorb the interactions of multiple categorical variables{p_end}
{synopt:{cmd:i.}{it:var1}{cmd:#}{cmd:c.}{it:var2}}absorb heterogeneous slopes, where {it:var2} has a different slope estimate depending on {it:var1}. Use carefully (see below!){p_end}
{synopt:{it:var1}{cmd:##}{cmd:c.}{it:var2}}absorb heterogenous intercepts and slopes. Equivalent to "{cmd:i.}{it:var1} {cmd:i.}{it:var1}{cmd:#}{cmd:c.}{it:var2}", but {it:much} faster{p_end}
{synopt:{it:var1}{cmd:##c.(}{it:var2 var3}{cmd:)}}multiple heterogeneous slopes are allowed together. Alternative syntax: {it:var1}{cmd:##(c.}{it:var2} {cmd:c.}{it:var3}{cmd:)}{p_end}
{synopt:{it:v1}{cmd:#}{it:v2}{cmd:#}{it:v3}{cmd:##c.(}{it:v4 v5}{cmd:)}}factor operators can be combined{p_end}
{synoptline}
{p2colreset}{...}
{p 4 6 2}- To save the estimates of specific absvars, write {newvar}{inp:={it:absvar}}.{p_end}
{p 4 6 2}-  However, be aware that estimates for the fixed effects are generally inconsistent and not econometrically identified.{p_end}
{p 4 6 2}- Using categorical interactions (e.g. {it:x}{cmd:#}{it:z}) is easier and faster than running {it:egen group(...)} beforehand.{p_end}
{p 4 6 2}- {browse "http://scorreia.com/research/singletons.pdf":Singleton observations} are dropped iteratively until no more singletons are found (see the linked article for details).{p_end}
{p 4 6 2}- Slope-only absvars ("state#c.time") have poor numerical stability and slow convergence.
If you need those, either i) increase tolerance or
ii) use slope-and-intercept absvars ("state##c.time"), even if the intercept is redundant.
For instance if absvar is "i.zipcode i.state##c.time" then i.state is redundant given i.zipcode, but
convergence will still be {it:much} faster.{p_end}


{marker options}{...}
{title:Options}

{marker opt_absorb}{...}

{phang}
{opth a:bsorb(reghdfejl##absorb:absvars)} list of categorical variables (or interactions) representing the fixed effects to be absorbed.
This is equivalent to including an indicator/dummy variable for each category of each {it:absvar}. {cmd:absorb()} is required.

{pmore}
To save a fixed effect, prefix the absvar with "{newvar}{cmd:=}".
For instance, the option {cmd:absorb(firm_id worker_id year_coefs=year_id)} will include firm, worker, and year fixed effects,
but will only save the estimates for the year fixed effects (in the new variable {it:year_coefs}).

{pmore}
If you want to run {help reghdfejl##postestimation:predict} afterward but don't particularly care about the names of each fixed effect, use the {cmdab:save:fe} suboption.
This will delete all preexisting variables matching {it:__hdfe*__} and create new ones as required.
Example: {it:reghdfejl price weight, absorb(turn trunk, savefe)}.

{marker opt_model}{...}

{marker opt_vce}{...}
{phang}
{opth vce:(reghdfejl##model_opts:vcetype)} specifies the type of standard error reported.

{pmore}
{opt un:adjusted}|{opt ols:} estimates conventional standard errors, valid under the assumptions of homoscedasticity and no correlation between observations even in small samples.

{pmore}
{opt r:obust} estimates heteroscedasticity-consistent standard errors (Huber/White/sandwich estimators), which still assume independence between observations.

{pmore}
{opt cl:uster} {it:clustervars} estimates consistent standard errors even when the observations
are correlated within groups. Multi-way-clustering is allowed.

{phang}
{opth res:iduals(newvar)} saves the regression residuals in a new variable. 

{pmore} {opt res:iduals} (without parenthesis) saves the residuals
in the variable {it:_reghdfe_resid} (overwriting it if it already exists).

{pmore}
This option does not require additional computations and is required for
subsequent calls to {cmd:predict, d}.


{phang}
{opth tol:erance(#)} specifies the tolerance criterion for convergence; default is {cmd:tolerance(1e-8)}.
In general, high tolerances (1e-8 to 1e-14) return more accurate results, but more slowly.

{pmore}
Warning: when absorbing heterogeneous slopes without the accompanying heterogeneous intercepts,
convergence is quite poor and a higher tolerance is strongly suggested (i.e. higher than the default).
In other words, an absvar of {it:var1##c.var2} converges easily, but an absvar of {it:var1#c.var2} will converge slowly and may require a higher tolerance.

{phang}
{opth it:erations(#)}
specifies the maximum number of iterations; the default is {cmd:iterations(16000)}.

{phang}
{opt nosample} will not create {it:e(sample)}, saving some space and speed.

{marker opt_reporting}{...}

{phang}
{opt l:evel(#)} sets confidence level; default is {cmd:level(95)}; see {helpb estimation options##level():[R] Estimation options}


{phang}
{opt nohead:er} suppresses the display of the table of summary
statistics at the top of the output; only the coefficient table is displayed.
This option is often used in programs and ado-files.

{phang}
{opt notable} suppresses display of the coefficient table.

{phang}
{opt nofoot:note} suppresses display of the footnote table that lists the absorbed fixed effects, including the number of categories/levels of each fixed effect,
redundant categories (collinear or otherwise not counted when computing degrees-of-freedom), and the difference between both.



{marker postestimation}{...}
{title:Postestimation syntax}


{pstd}
Only {cmd:estat summarize}, {cmd:predict}, and {cmd:test} are currently supported.

{pstd}
The syntax of {it: estat summarize} and {it:predict} is:

{p 8 13 2}
{cmd:estat summarize}
{p_end}{col 23}Summarizes {it:depvar} and the variables described in {it:_b} (i.e. not the excluded instruments)

{p 8 16 2}
{cmd:predict} 
{newvar} 
{ifin}
[{cmd:,} {it:statistic}]
{p_end}{col 23}May require you to previously save the fixed effects (except for option {opt xb}).
{col 23}To see how, see the details of the {help reghdfejl##absvar:absorb} option
{col 23}Equation: y = xb + d_absorbvars + e

{synoptset 20 tabbed}{...}
{synopthdr:statistic}
{synoptline}
{syntab :Main}
{p2coldent: {opt xb}}xb fitted values; the default{p_end}
{p2coldent: {opt xbd}}xb + d_absorbvars{p_end}
{p2coldent: {opt d}}d_absorbvars{p_end}
{p2coldent: {opt r:esiduals}}residual{p_end}
{p2coldent: {opt sc:ore}}score; equivalent to {opt residuals}{p_end}
{p2coldent: {opt stdp}}standard error of the prediction (of the xb component){p_end}
{synoptline}
{p2colreset}{...}
{p 4 6 2}although {cmd:predict} {help data_types:type} {help newvar} is allowed,
the resulting variable will always be of type {it:double}.{p_end}

{marker examples}{...}
{title:Examples}

{hline}
{pstd}Setup{p_end}
{phang2}{cmd:. sysuse auto}{p_end}

{pstd}Simple case - one fixed effect{p_end}
{phang2}{cmd:. reghdfejl price weight length, absorb(rep78)}{p_end}
{hline}

{pstd}As above, but also compute clustered standard errors{p_end}
{phang2}{cmd:. reghdfejl price weight length, absorb(rep78) vce(cluster rep78)}{p_end}
{hline}

{pstd}Two and three sets of fixed effects{p_end}
{phang2}{cmd:. webuse nlswork}{p_end}
{phang2}{cmd:. reghdfejl ln_w grade age ttl_exp tenure not_smsa south , absorb(idcode year)}{p_end}
{phang2}{cmd:. reghdfejl ln_w grade age ttl_exp tenure not_smsa south , absorb(idcode year occ)}{p_end}
{hline}


{marker results}{...}
{title:Stored results}

{pstd}
{cmd:reghdfejl} stores the following in {cmd:e()}:

{synoptset 24 tabbed}{...}
{syntab:Scalars}
{synopt:{cmd:e(N)}}number of observations{p_end}
{synopt:{cmd:e(num_singletons)}}number of singleton observations{p_end}
{synopt:{cmd:e(N_full)}}number of observations including singletons{p_end}

{synopt:{cmd:e(N_hdfe)}}number of absorbed fixed-effects{p_end}
{synopt:{cmd:e(tss)}}total sum of squares{p_end}
{synopt:{cmd:e(tss)}}total sum of squares after partialling-out{p_end}
{synopt:{cmd:e(rss)}}residual sum of squares{p_end}
{synopt:{cmd:e(rss)}}model sum of squares (tss-rss){p_end}
{synopt:{cmd:e(r2)}}R-squared{p_end}
{synopt:{cmd:e(r2_a)}}adjusted R-squared{p_end}
{synopt:{cmd:e(r2_within)}}Within R-squared{p_end}
{synopt:{cmd:e(r2_a_within)}}Adjusted Within R-squared{p_end}
{synopt:{cmd:e(df_a)}}degrees of freedom lost due to the fixed effects{p_end}
{synopt:{cmd:e(rmse)}}root mean squared error{p_end}
{synopt:{cmd:e(ll)}}log-likelihood{p_end}
{synopt:{cmd:e(ll_0)}}log-likelihood of fixed-effect-only regression{p_end}
{synopt:{cmd:e(F)}}F statistic{p_end}
{synopt:{cmd:e(rank)}}rank of {cmd:e(V)}{p_end}
{synopt:{cmd:e(N_clustervars)}}number of cluster variables{p_end}
        
{synopt:{cmd:e(N_clust}#{cmd:)}}number of clusters for the #th cluster variable{p_end}
{synopt:{cmd:e(N_clust)}}number of clusters; minimum of {it:e(clust#)}{p_end}

{synopt:{cmd:e(df_m)}}model degrees of freedom{p_end}
{synopt:{cmd:e(df_r)}}residual degrees of freedom{p_end}

{synopt:{cmd:e(sumweights)}}sum of weights{p_end}
{synopt:{cmd:e(ic)}}number of iterations{p_end}
{synopt:{cmd:e(converged)}}{cmd:1} if converged, {cmd:0} otherwise{p_end}
{synopt:{cmd:e(drop_singletons)}}{cmd:1} if singletons were dropped, {cmd:0} otherwise{p_end}

{synoptset 24 tabbed}{...}
{syntab:Macros}
{synopt:{cmd:e(cmd)}}{cmd:reghdfejl}{p_end}
{synopt:{cmd:e(cmdline)}}command as typed{p_end}
{synopt:{cmd:e(depvar)}}name of dependent variable{p_end}
{synopt:{cmd:e(indepvars)}}names of independent variables{p_end}
{synopt:{cmd:e(absvars)}}name of the absorbed variables or interactions{p_end}
{synopt:{cmd:e(clustvar)}}name of cluster variable{p_end}
{synopt:{cmd:e(clustvar}#{cmd:)}}name of the #th cluster variable{p_end}
{synopt:{cmd:e(vce)}}{it:vcetype} specified in {cmd:vce()}{p_end}
{synopt:{cmd:e(vcetype)}}title used to label Std. Err.{p_end}
{synopt:{cmd:e(properties)}}{cmd:b V}{p_end}
{synopt:{cmd:e(predict)}}program used to implement {cmd:predict}{p_end}
{synopt:{cmd:e(estat_cmd)}}program used to implement {cmd:estat}{p_end}
{synopt:{cmd:e(footnote)}}program used to display footnote{p_end}
{synopt:{cmd:e(marginsnotok)}}predictions not allowed by {cmd:margins}{p_end}
{synopt:{cmd:e(title)}}title in estimation output{p_end}
{synopt:{cmd:e(title2)}}subtitle in estimation output, indicating how many FEs were being absorbed{p_end}

{synoptset 24 tabbed}{...}
{syntab:Matrices}
{synopt:{cmd:e(b)}}coefficient vector{p_end}
{synopt:{cmd:e(V)}}variance-covariance matrix of the estimators{p_end}

{synoptset 24 tabbed}{...}
{syntab:Functions}
{synopt:{cmd:e(sample)}}marks estimation sample{p_end}
{p2colreset}{...}


{title:Author}

{pstd}David Roodman{break}
Email: {browse "mailto:david@davidroodman.com":david@davidroodman.com}
{p_end}


{marker acknowledgements}{...}
{title:Acknowledgements}

{pstd}
More than with most packages, the author of this stands on the shoulders of giants. {cmd:reghdfejl} is merely a wrapper
for {browse "https://www.matthieugomez.com/":Matthieu Gomez}'s {browse "https://github.com/FixedEffects/FixedEffectModels.jl":FixedEfectModels.jl},
which is itself a Julia implementation of {browse "http://scorreia.com/":Sergio Correia}'s path-breaking {help reghdfe}. The
Julia programming language is a free, open-source project.

{pstd}

{marker references}{...}
{title:References}

{p 4 8 2}Correia, S. 2016. A feasible estimator for linear models with multi-way fixed effects. {browse "http://scorreia.com/research/hdfe.pdf"}.{p_end}


