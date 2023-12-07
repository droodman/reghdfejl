{smcl}
{* *! version 0.4.2 7dec2023}{...}

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

{p 8 15 2} {cmd:reghdfejl mask}

{p 8 15 2} {cmd:reghdfejl unmask}

{marker options_table}{...}
{synoptset 27 tabbed}{...}
{synopthdr}
{synoptline}
{synopt: {cmdab:a:bsorb}({it:absvars}, [{cmdab:save:fe}])}categorical variables representing the fixed effects to be absorbed{p_end}
{synopt: {opth vce:(reghdfejl##model_opts:vcetype)}}{it:vcetype} may be {opt un:adjusted} (default), {opt r:obust} or {opt cl:uster} {help fvvarlist} (allowing multi-way clustering){p_end}
{synopt: {opth res:iduals(newvar)}}save regression residuals; required for postestimation "{it:predict <varname>, d}" {p_end}
{synopt:{opt tol:erance(#)}}criterion for convergence. default is 1e-8{p_end}
{synopt:{opt iter:ate(#)}}maximum number of iterations; default is 16,000{p_end}
{synopt:{opt nosamp:le}}will not create {it:e(sample)}, saving some space and speed{p_end}
{synopt:{opt compact}}temporarily saves all data to disk in order to free memory{p_end}
{synopt:{opt threads(#)}}number of CPU threads Julia should use{p_end}
{synopt:{opt gpu}}use NVIDIA or Apple Silicon GPU{p_end}
{synopt:{opt l:evel(#)}}set confidence level; default is normally 95{p_end}
{synopt:{it:display options}}{help ml##display_options:standard options} governing results display{p_end}
{synoptline}
{p 4 6 2}{it:depvar} and {it:indepvars} may contain {help tsvarlist:factor variables} and {help tsvarlist:time-series operators}. {it:depvar} 
cannot be of the form {it:i.y} though, only {it:#.y} (where # is a number){p_end}


{marker description}{...}
{title:Description}

{pstd}
{cmd:reghdfejl} is designed as a slot-in replacement for {help reghdfe}. It is missing some features of {cmd:reghdfe}. And it is
{it:not} guarantee to exactly match {cmd:reghdfe}'s results. But it
can run ~10 times faster because it relies on the Julia program {browse "https://github.com/FixedEffects/FixedEffectModels.jl":FixedEffectsModel.jl},
by Matthieu Gomez, which implements similar methods (Correia 2016).
{cmd:reghdfejl} also fits instrumental variables models
with two-stage least squares. In this capacitiy it is not as full-featured as {cmd:ivreghdfe}, because it does not (yet) work as a wrapper for 
{cmd:ivreg2}.

{pstd}
To run, {cmd:reghdfejl} requires that the Stata command {cmd:jl} be installed; "{stata ssc install julia}" should suffice. It also needs
Julia 1.9.4 or later, which is free. See these {help jl##installation:installation instructions}.

{pstd}
Because Julia performs just-in-time compilation, and because {cmd:reghdfejl} may need to install Julia packages, there can be long lags on first 
use. It can also take longer the first time in a session that you use a feature, such as multiway clustering, that forces compilation
of another function in the Julia package.

{pstd}
If {cmd:reghdfejl} appears to be failing to install the needed packages,
you can try intervening manually: start Julia outside of Stata, hit the "]" key to enter the package manager, and type
{cmd:add <pkgname>} for each package. The needed packages are Vcov, FixedEffectModels, DataFrames, and either Metal (for
Macs) or CUDA (otherwise).

{pstd}
{cmd:reghdfejl} lacks some {cmd:reghdfe} features that are typically secondary for users:

{p 4 6 0}
* It does not correct the estimates of the degrees of freedom consumed by absorbed fixed effects for collinearity
and redundance among fixed-effect dummies. {cmd:reghdfe}
displays these corrections in a table after the main results. {cmd:reghdfejl} does not.

{p 4 6 0}
* It does not offer the {help reghdfe##opt_group_fes:Group FE} features.

{p 4 6 0}
* It does not allow control over whether the constant term is reported. The constant is always absorbed.

{p 4 6 0}
* It does not offer options such as {cmdab:tech:nique()} that give finer control over the algorithm. But these are largely obviated
by {cmd:reghdfejl}'s speed.

{pstd}
{cmd:reghdfejl} starts by copying the data needed for estimation into a Julia DataFrame. Duplicating the data takes a bit of time and potentially
a lot of RAM. In
extreme cases, it will more than double the storage demand because even variables stored in Stata in small types, such as {cmd:byte}, will be stored 
double-precision--8 bytes per value--in Julia. If the memory demand is too great, performance will plummet. {cmd:reghdfejl} therefore is most 
useful when you have plenty of RAM, when the number of non-absorbed regressors is low, and when the number of absorbed terms is high
(for then the computational efficiency of Julia shines).

{pstd}
{cmd:reghdfejl} offers two novel features that can increase speed. The first is access to multithreading in Julia, even in 
flavors of Stata that do not offer multiprocessing. The {opt threads(#)}
option pertains to this feature. But it can only {it:reduce} the number of CPU threads Julia uses. The default number--and the 
maximum--is set by the {browse "https://docs.julialang.org/en/v1/manual/multi-threading/":system environment variable JULIA_NUM_THREADS}. It is 
possible for the default to be too high as well as too low. If you set it high, then you can experiment using {opt threads(#)}. See 
{help jl##threads:help jl} for more on determining and controlling the number of threads.

{pstd}
The other novel feature is access to GPU-based computation. The {cmd:gpu} specifies the use of NVIDIA or Apple Silicon 
GPUs for computation. Typically this modestly increases speed.

{pstd}
The command {cmd:reghdfejl mask} redirects all {cmd:reghdfe} calls to {cmd:reghdfejl}. {cmd:reghdfejl unmask} stops the redirection. This is useful is when using other Stata packages that call {cmd:reghdfe}, such as 
{stata findit eventdd:eventdd}. It can speed up those commands as well. However, since {cmd:reghdfe} and {cmd:reghdfejl} do not accept exactly the same options
and return exactly the same result sets, no guarantee can be given that this hack will work in any particular case.


{marker absorb}{...}
{title:absorb() syntax}

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
{cmdab:a:bsorb}({it:absvars}, [{cmdab:save:fe}]) list of categorical variables (or interactions) representing the fixed effects to be absorbed.
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
{cmdab:res:iduals[(}{help newvar}{cmd:})]} saves the regression residuals in a new variable. {opt res:iduals} without parenthesis saves them
in the variable {it:_reghdfejl_resid}, overwriting it if it already exists.

{pmore}
This option carries a small time cost but is required for subsequent calls to {cmd:predict, d}.

{phang}
{opth tol:erance(#)} specifies the tolerance criterion for convergence. The default is 1e-8.
In general, low tolerances (1e-8 to 1e-14) return more accurate results, more slowly.

{phang}
{opth it:erations(#)}
specifies the maximum number of iterations; the default is 16,000.

{phang}
{opt nosample} will not create {it:e(sample)}, saving some space and speed.

{phang}
{opt compact} temporarily saves all data to disk in orer to free memory for estimation--at the cost of a bit of time.

{phang}
{opt l:evel(#)} sets the confidence level for reported confidence intervals. The default is controlled by {help set level} and is usually 95.

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

{phang}. {stata sysuse auto}{p_end}
{phang}. {stata reghdfejl price weight length, absorb(rep78)}{p_end}
{phang}. {stata reghdfejl price weight length, absorb(rep78) vce(cluster rep78)}{p_end}

{phang}. {stata webuse nlswork}{p_end}
{phang}. {stata reghdfejl ln_wage grade age ttl_exp tenure not_smsa south , absorb(idcode year)}{p_end}
{phang}. {stata reghdfejl ln_wage grade age ttl_exp tenure not_smsa south , absorb(idcode year occ_code)}{p_end}


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


{title:Benchmark}

{pstd}
This benchmark creates a data set with 100 million observations and runs regressions with 1 or 2 sets of fixed 
effects using {help areg}, {help reghdfe}, and {cmd:reghdfejl} in Stata/MP. The number of processors is set to 1 or 8. THe script is run on a Windows 
laptop with an NVIDIA RTX 4070 GPU and an Intel i9-13900H, the latter having 6 performance and 8 (slower) efficiency cores. The log is lightly edited for parsimony.

{pstd}With 1 set of FE, {cmd:reghdfe} is 2-7 times faster than {cmd:areg}, and {cmd:reghdfejl} is 3-4 faster again, slightly more so with the {cmd:gpu} 
option. With 2 sets of FE, {cmd:reghdfejl} is 10 times faster than {cmd:reghdfe} without {cmd:gpu} and 12 times faster with.

{pstd}
On a Mac with an M2 Pro chip--with 8 performance cores and 4 efficiency cores--the absolute times are somewhat higher and the ratios slightly lower (not shown).

{pstd}{hilite:Script}{p_end}
{phang}scalar N = 100000000{p_end}
{phang}scalar K = 100{p_end}
{phang}set obs `=N'{p_end}
{phang}gen id1 = runiformint(1, N/K){p_end}
{phang}gen id2 = runiformint(1,   K){p_end}

{phang}drawnorm x1 x2{p_end}
{phang}gen double y = 3 * x1 + 2 * x2 + sin(id1) + cos(id2) + runiform(){p_end}

{phang}set rmsg on{p_end}

{phang}set processors 1{p_end}
{phang}qui areg      y x1 x2, a(id1) cluster(id1){p_end}
{phang}qui reghdfe   y x1 x2, a(id1) cluster(id1){p_end}
{phang}qui reghdfejl y x1 x2, a(id1) cluster(id1){p_end}
{phang}qui reghdfejl y x1 x2, a(id1) cluster(id1) gpu{p_end}
{phang}qui reghdfe   y x1 x2, a(id1 id2) cluster(id1 id2){p_end}
{phang}qui reghdfejl y x1 x2, a(id1 id2) cluster(id1 id2){p_end}
{phang}qui reghdfejl y x1 x2, a(id1 id2) cluster(id1 id2) gpu{p_end}

{phang}set processors 8  // requires Stata/MP{p_end}
{phang}qui areg      y x1 x2, a(id1) cluster(id1){p_end}
{phang}qui reghdfe   y x1 x2, a(id1) cluster(id1){p_end}
{phang}qui reghdfejl y x1 x2, a(id1) cluster(id1){p_end}
{phang}qui reghdfejl y x1 x2, a(id1) cluster(id1) gpu{p_end}
{phang}qui reghdfe   y x1 x2, a(id1 id2) cluster(id1 id2){p_end}
{phang}qui reghdfejl y x1 x2, a(id1 id2) cluster(id1 id2){p_end}
{phang}qui reghdfejl y x1 x2, a(id1 id2) cluster(id1 id2) gpu{p_end}

{pstd}{hilite:Log}{p_end}
{phang}. set processors 1{p_end}

{phang}. qui areg      y x1 x2, a(id1) cluster(id1){p_end}
{phang}t=490.93{p_end}

{phang}. qui reghdfe   y x1 x2, a(id1) cluster(id1){p_end}
{phang}t=68.74{p_end}

{phang}. qui reghdfejl y x1 x2, a(id1) cluster(id1){p_end}
{phang}t=16.86{p_end}

{phang}. qui reghdfejl y x1 x2, a(id1) cluster(id1) gpu{p_end}
{phang}t=15.92{p_end}

{phang}. qui reghdfe   y x1 x2, a(id1 id2) cluster(id1 id2){p_end}
{phang}t=315.05{p_end}

{phang}. qui reghdfejl y x1 x2, a(id1 id2) cluster(id1 id2){p_end}
{phang}t=29.70{p_end}

{phang}. qui reghdfejl y x1 x2, a(id1 id2) cluster(id1 id2) gpu{p_end}
{phang}t=24.48{p_end}

{phang}. set processors 8{p_end}

{phang}. qui areg      y x1 x2, a(id1) cluster(id1){p_end}
{phang}t=99.43 {p_end}

{phang}. qui reghdfe   y x1 x2, a(id1) cluster(id1){p_end}
{phang}t=44.15{p_end}

{phang}. qui reghdfejl y x1 x2, a(id1) cluster(id1){p_end}
{phang}t=13.38{p_end}

{phang}. qui reghdfejl y x1 x2, a(id1) cluster(id1) gpu{p_end}
{phang}t=12.20{p_end}

{phang}. qui reghdfe   y x1 x2, a(id1 id2) cluster(id1 id2){p_end}
{phang}t=238.35{p_end}

{phang}. qui reghdfejl y x1 x2, a(id1 id2) cluster(id1 id2){p_end}
{phang}t=24.80{p_end}

{phang}. qui reghdfejl y x1 x2, a(id1 id2) cluster(id1 id2) gpu{p_end}
{phang}t=20.100{p_end}

{title:Author}

{pstd}David Roodman{break}
Email: {browse "mailto:david@davidroodman.com":david@davidroodman.com}
{p_end}


{marker acknowledgements}{...}
{title:Acknowledgements}

{pstd}
More so than for most packages, in writing this one, the author stands on the shoulders of giants. {cmd:reghdfejl} is merely a wrapper
for {browse "https://www.matthieugomez.com/":Matthieu Gomez}'s {browse "https://github.com/FixedEffects/FixedEffectModels.jl":FixedEfectModels.jl},
which is itself a Julia implementation of {browse "http://scorreia.com/":Sergio Correia}'s path-breaking {help reghdfe}. {cmd:reghdfejl}'s code for
postestimation functionality is copied from {cmd:reghdfe}, as are parts of this help file. The Julia programming language is a free, open-source project
with many contributors.

{pstd}

{marker references}{...}
{title:References}

{p 4 8 2}Correia, S. 2016. A feasible estimator for linear models with multi-way fixed effects. {browse "http://scorreia.com/research/hdfe.pdf"}.{p_end}


