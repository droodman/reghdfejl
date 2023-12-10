// Perform HDFE partialling in a common sample
cap program drop partialhdfejl
program define partialhdfejl
  syntax varlist [if] [in] [aw pw fw iw/], Absorb(string) [GENerate(string) PREfix(string) replace]
  
  if (`"`generate'"'!="") + (`"`prefix'"'!="") != 1 {
    di as err "Specify exactly one of {cmdab:gen:erate()} or {cmdab:pre:fix()}."
    exit 198
  }
  
  marksample touse

  if strpos("`varlist'", ".") | strpos("`varlist'", "#") | strpos("`varlist'", "-") | strpos("`varlist'", "?") | strpos("`varlist'", "*") | strpos("`varlist'", "~") {
    fvexpand `varlist' if `touse'
    local varlist
    foreach var in `r(varlist)' {
      _ms_parse_parts `var'
      if !r(omit) local varlist `varlist' `var'
    }
    fvrevar `varlist' if `touse'
    local varlist `r(varlist)'
  }

  if `"`generate'"'!="" _assert `:word count `generate''==`:word count `varlist'', msg("generate() option has wrong number of variable names") rc(198)

  if `"`exp'"' != "" {
    cap confirm var `exp'
    if _rc {
      tempname wtvar
      gen double `wtvar' = `exp' if `touse'
    }
    else local wtvar `exp'
    local wtopt , weights = :`wtvar'
  }

  local blaslib = cond(c(os)=="MacOSX", "AppleAccelerate", "BLISBLAS")
  local gpulib  = cond(c(os)=="MacOSX", "Metal", "CUDA")

  jl AddPkg `blaslib'
  jl AddPkg `gpulib'
  jl AddPkg FixedEffectModels, minver(1.10.2)
  jl, qui: using `blaslib', `gpulib', FixedEffectModels

  tokenize `absorb'
  local absorb `*'  // remove extra spaces
  local feterms i.`: subinstr local absorb " " " i.", all'

  local absorbvars `feterms'
  local feterms: subinstr local feterms "##c." ")*(", all
  local feterms: subinstr local feterms "#c." ")&(", all
  local feterms: subinstr local feterms "##i." ")*fe(", all
  local feterms: subinstr local feterms "##" "#", all
  local feterms: subinstr local feterms "#" "#i.", all
  local feterms: subinstr local feterms "i.i." "i.", all
  local feterms: subinstr local feterms "#i." ")&fe(", all
  local feterms: subinstr local feterms "i." "fe(", all
  local feterms: subinstr local feterms " " ") + ", all
  local feterms: subinstr local feterms ")" " )", all
  local feterms: subinstr local feterms "(" "( ", all
  local feterms + `feterms' )

  local absorbvars: subinstr local absorbvars "i." " ", all
  local absorbvars: subinstr local absorbvars "c." " ", all
  local absorbvars: subinstr local absorbvars "#" " ", all

  foreach var in `absorbvars' {
    cap confirm numeric var `var'
    if _rc {
      tempvar t
      egen long `t' = group(`var') if `touse'
      local absorbvars: subinstr local absorbvars "`var'" "`t'", word all
      local feterms   : subinstr local feterms    "`var'" "`t'", word all
    }
  }
  
  markout `touse' `absorbvars'

  jl PutVarsToDFNoMissing `varlist' `absorbvars' `wtvar' if `touse'
  jl, qui: p = partial_out(df, @formula(`:subinstr local varlist " " " + ", all' ~ 1 `feterms') `wtopt', add_mean=false)
  
  if `"`prefix'"' != "" local generate `prefix'`: subinstr local varlist " " " `prefix'", all'
    else if "`replace'"!="" {
      tempname t
      jl, qui: SF_scal_save("`t'", size(df)[1])
      if `t' < _N {
        foreach var in `generate' {
          cap replace `var' = .
        }
      }
    }

  jl GetVarsFromDF `generate' if `touse', source(p[1]) cols(`varlist') `replace'
  
  jl: df = p = nothing
end
