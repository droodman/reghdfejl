*! reghdfejl_load  1.1.9 11 October 2026

cap program drop reghdfejl_load
program define reghdfejl_load
  version 15
  
  local jlversion 2.1.0

  if `"$reghdfejl_loaded"'=="" {
    cap jl version
    if _rc {
      di as err `"Can't access Julia. {cmd:reghdfejl} requires that the {cmd:jl} command be installed, via {stata ssc install julia}."
      di as err "And it requires that Julia be installed, following the instruction under Installation in {help jl##installation:help jl}."
      exit 198
    }

    _jl: Int(v"`r(version)'" < v"`jlversion'")
    if `r(ans)' {
      di as txt "The Stata package {cmd:julia} is not up to date. Attempting to update it with {stata ssc install julia, replace}." _n
      ssc install julia, replace
    }

    qui findfile reghdfejl_project.toml
    local projectfile `r(fn)'
    qui findfile reghdfejl_manifest.toml
    jl SetEnv @reghdfejl, quietly update project(`projectfile') manifest(`r(fn)') pin  // update the project environment if the definition files in the ado path are newer

    _jl: using Distributions, Vcov, FixedEffectModels;

    _jl: module reghdfejl global k, sizedf, p, res, esample, D, s, id, reps, b, bbs, V, Vbs, coefnames, rngs, dfs, Nclust, bssize, wts, dst end;  // name space for the package

    global reghdfejl_loaded 1
  }
  else jl SetEnv @reghdfejl, quietly
end
