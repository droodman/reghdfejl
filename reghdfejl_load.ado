*! reghdfejl_load  1.1.6 18 July 2025

cap program drop reghdfejl_load
program define reghdfejl_load
  version 15
  
  local JLVERSION 1.0.1

  if `"$reghdfejl_loaded"'=="" {
    cap jl version
    if _rc {
      di as err `"Can't access Julia. {cmd:reghdfejl} requires that the {cmd:jl} command be installed, via {stata ssc install julia}."
      di as err "And it requires that Julia be installed, following the instruction under Installation in {help jl##installation:help jl}."
      exit 198
    }

    parse "`r(version)'", parse(".")
    local v1 `1'
    local v2 `3'
    local v3 `5'
    parse "`JLVERSION'", parse(".")
    if `v1'<`1' | `v1'==`1' & `v2'<`3' | `v1'==`1' & `v2'==`3' & `v3'<`5' {
      di as txt "The Stata package {cmd:julia} is not up to date. Attempting to update it with {stata ssc install julia, replace}." _n
      ssc install julia, replace
    }

    if c(os)=="MacOSX" {
      jl AddPkg AppleAccelerate
      _jl: using AppleAccelerate;
    }
    else if c(lapack_mkl)=="on" {
      jl AddPkg MKL
      _jl: using MKL;
    }  // else for AMD systems, default to OpenBLAS

    local gpulib = cond(c(os)=="MacOSX", "Metal", "CUDA")
    jl AddPkg `gpulib'
    jl AddPkg StableRNGs
    jl AddPkg OrderedCollections
    jl AddPkg FixedEffectModels, minver(1.11.0)
    jl AddPkg GLFixedEffectModels, minver(0.5.3)
    jl AddPkg Distributions, minver(0.25.107)
    jl AddPkg Vcov, minver(0.8.1)
    _jl: using `gpulib', FixedEffectModels, Vcov, StableRNGs, Distributed, DataFrames, GLFixedEffectModels, Distributions, OrderedCollections;
    _jl: module reghdfejl global k, sizedf, p, res, esample, D, s, id, reps, b, bbs, V, Vbs, coefnames, rngs, dfs, Nclust, bssize, wts, dst end;  // name space for the package

    global reghdfejl_loaded 1
  }
end
