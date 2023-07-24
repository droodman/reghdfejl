# reghdfejl
High-dimensional fixed-effect estimation in Stata using Julia.

This package bridges between Stata and the Julia package [FixedEffectModels.jl](https://github.com/FixedEffects/FixedEffectModels.jl), which is inspired by, but faster than, [`reghdfe`](https://github.com/sergiocorreia/reghdfe). It is designed as a slot-in replacement for `reghdfe` and [`ivreghdfe`](https://github.com/sergiocorreia/ivreghdfe). It accepts both the standard OLS and 2SLS specification syntaxes. It offers most options and return values of `reghdfe`. But since, unlike `ivreghdfe`, it is not currently a wrapper for [`ivreg2`](https://ideas.repec.org/c/boc/bocode/s425401.html), so it does not offer advanced features such as CUE, LIML, and first-stage checks.

## Requirements
* Stata 16 or later.
* [Python 3](https://www.python.org/downloads/).
* Stata [configured to work with Python](https://www.stata.com/python](https://www.stata.com/manuals/ppystataintegration.pdf#page=12).
* [Julia 1.9 or later](https://julialang.org/downloads/), with its installed location included in the system path.
* The Python packages NumPy and JuliaCall.
* The Julia packages FixedEffectModels.jl, CUDA.jl, DataFrames.jl, and Vcov.jl.

Except for Stata, all required components are freely available. `reghdfejl` will automatically attempt to install the needed Python and Julia packages, and even to install Julia via JuliaCall.

## Installation
When more mature, this package will be posted on SSC. For now, install it from Stata with
```
net install reghdfejl, from(https://raw.github.com/droodman/reghdfejl/v[X.Y.Z])
```
where "[X.Y.Z]" represents the [latest release version number](https://github.com/droodman/reghdfejl/releases).


## Usage
Because Julia uses just-in-time compilation, the first time you run `reghdfejl` in a Stata session, it is slow. The same goes for the first time you trigger the use of different code within the underlying Julia package, such as by running the first instrumental-variables or GPU-based estimate within a session.

`reghdfejl` ignores reghdfe options that affect the best-fit search algorithm, as well as the rarely-used dofadjustments() option. It accepts three novel options:
* `threads()` specifies the number of CPU threads FixedEffectModels.jl should use, for speed. The default is 4.
* `gpu` specifies that an [NVIDIA GPU be used for computation](https://github.com/FixedEffects/FixedEffectModels.jl#nvidia-gpu).
* `precision()` sets the precision of GPU computation. 64 (double precision) is the default. 32 is also accepted, potentially reducing precision but saving time.

## Examples
```
webuse nlswork
reghdfejl ln_wage grade age ttl_exp tenure not_smsa south, absorb(idcode year)
reghdfejl ln_wage grade age ttl_exp tenure not_smsa south, absorb(idcode year) vce(robust)
reghdfejl ln_wage grade age ttl_exp tenure not_smsa south, absorb(idcode year) vce(cluster idcode) threads(8)
reghdfejl ln_wage grade age ttl_exp tenure (not_smsa = south), absorb(idcode year) vce(cluster idcode year)
```

## Development plans
* Switch to a C plug-in for Stata, to bypass Python and greatly speed data transfer between Stata and Julia.
* Add suport for `estat summarize` post-estimation.
* Expand non-absorbed factor variables in Julia rather than Stata, to reduce data transfer between the two.
* Possibly make it a wrapper for `ivreg2` like `ivreghdfe`.
* Add help file.
* Post to SSC.
