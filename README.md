# reghdfejl
High-dimensional fixed-effect estimation in Stata using Julia.

This package bridges between Stata and the Julia package [FixedEffectModels.jl](https://github.com/FixedEffects/FixedEffectModels.jl), which is modeled on, but faster than, [`reghdfe`](https://github.com/sergiocorreia/reghdfe). It is designed as a slot-in replacement for `reghdfe` and [`ivreghdfe`](https://github.com/sergiocorreia/ivreghdfe). It accepts both the standard OLS and 2SLS specification syntaxes. It offers most options and return values of `reghdfe`. But since, unlike `ivreghdfe`, it is not currently a wrapper for [`ivreg2`](https://ideas.repec.org/c/boc/bocode/s425401.html), it does not offer advanced features such as CUE and LIML. It does provide the Kleibergen-Paap _F_ statistic.

## Requirements
* Stata 16 or later.
* The Stata package [julia.ado](https://github.com/droodman/julia.ado).
* Julia 1.9.4 or later, installed following the instructions obtained via `help jl` in Stata.

## Installation
Install from SSC with
```
ssc install reghdfejl
```
Sometimes SSC will lag a bit behind this repository. To get the latest version from here, do:
```
net install reghdfejl, replace from(https://raw.github.com/droodman/reghdfejl/v[X.Y.Z])
```
where "[X.Y.Z]" represents the [latest release version number](https://github.com/droodman/reghdfejl/releases).


## Usage
Because Julia uses just-in-time compilation, the first time you run `reghdfejl` in a Stata session, it is slow. The same goes for the first time you trigger the use of different code within the underlying Julia package, such as by running the first instrumental-variables or GPU-based estimate within a session.

`reghdfejl` ignores reghdfe options that affect the best-fit search algorithm, as well as the rarely-used dofadjustments() option. It accepts three novel options:
* `threads()` specifies the number of CPU threads FixedEffectModels.jl should use, for speed. The default is 4.
* `gpu` specifies that a [GPU be used for computation](https://github.com/FixedEffects/FixedEffectModels.jl#nvidia-gpu) (works better on NVIDIA GPUs than Apple Silicon).
* `bs()` a suboption of `vce()` for high-speed bootstrapping with parallel processing.

## Examples
```
webuse nlswork
reghdfejl ln_wage grade age ttl_exp tenure not_smsa south, absorb(idcode year)
reghdfejl ln_wage grade age ttl_exp tenure not_smsa south, absorb(idcode year) vce(robust)
reghdfejl ln_wage grade age ttl_exp tenure not_smsa south, absorb(idcode year) vce(cluster idcode) threads(4)
reghdfejl ln_wage grade age ttl_exp tenure (not_smsa = south), absorb(idcode year) vce(cluster idcode year)
reghdfejl ln_wage age ttl_exp tenure not_smsa south, absorb(year occ_code) vce(bs, cluster(occ_code) reps(1000) seed(42) procs(4))

```

## Development plans
* Expand non-absorbed factor variables in Julia rather than Stata, to reduce data transfer.
* Possibly make it a wrapper for `ivreg2` like `ivreghdfe`.

