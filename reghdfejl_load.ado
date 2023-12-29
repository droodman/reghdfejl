*! reghdfejl 0.6.0 20 December 2023

cap program drop reghdfejl_load
program define reghdfejl_load
  local JLVERSION 0.8.0

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

    local gpulib = cond(c(os)=="MacOSX", "Metal", "CUDA")
    local blaslib = cond(c(os)=="MacOSX", "AppleAccelerate", "BLISBLAS")
    jl SetEnv reghdfejl
    jl AddPkg `blaslib'
    jl AddPkg `gpulib'
    jl AddPkg StableRNGs
    jl AddPkg FixedEffectModels, minver(1.11.0)
    jl AddPkg Vcov, minver(0.8.1)
    jl, qui: using `blaslib', `gpulib', FixedEffectModels, Vcov, StableRNGs, Distributed, DataFrames
    global reghdfejl_loaded 1
  }
end
