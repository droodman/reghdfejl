*! reghdfejl_load  1.1.7 8 November 2025

cap program drop reghdfejl_load
program define reghdfejl_load
  version 15
  
  local JLVERSION 1.2.0

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
      jl AddPkg AppleAccelerate, ver(0.4.5)
      jl AddPkg Metal, ver(1.9.0)
      _jl: using AppleAccelerate, Metal;
    }
    else if c(lapack_mkl)=="on" {
      jl AddPkg MKL, ver(0.9.0)
      jl AddPkg CUDA, ver(5.9.2)
      _jl: using MKL, CUDA;
    }  // else for AMD systems, default to OpenBLAS

    jl AddPkg StableRNGs, ver(1.0.3)
    jl AddPkg OrderedCollections, ver(1.8.1)
    jl AddPkg FixedEffectModels, ver(1.12.0)
    jl AddPkg GLFixedEffectModels, ver(0.5.5)
    jl AddPkg Distributions, ver(0.25.122)
    jl AddPkg Vcov, ver(0.8.1)
    _jl: using FixedEffectModels, Vcov, StableRNGs, Distributed, GLFixedEffectModels, Distributions, OrderedCollections;
    _jl: module reghdfejl global k, sizedf, p, res, esample, D, s, id, reps, b, bbs, V, Vbs, coefnames, rngs, dfs, Nclust, bssize, wts, dst end;  // name space for the package

    global reghdfejl_loaded 1
  }
end
