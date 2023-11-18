// Perform HDFE partialling in a common sample
cap program drop jresidualize
program define jresidualize
  syntax varlist [if] [in] [aw pw fw iw/], Absorb(string) [GENerate(string) PREfix(string) replace]
  
  if (`"`generate'"'!="") + (`"`prefix'"'!="") != 1 {
    di as err "Specify exactly one of {cmdab:gen:erate()} or {cmdab:pre:fix()}."
    exit 198
  }
  
  marksample touse
  fvexpand `varlist'
  local varlist `r(varlist)'
  
  if `"`exp'"' != "" {
    cap confirm var `exp'
    if _rc {
      tempname wtvar
      gen double `wtvar' = `exp' if `touse'
    }
    else local wtvar `exp'
    local wtopt , weights = :`wtvar'
  }

  julia, qui: using FixedEffectModels
  ParseAbsorb `absorb'
  local feterms `r(feterms)'
  markout `touse' `r(absorbvars)'

  julia PutVarsToDFNoMissing `varlist' `r(absorbvars)' `wtvar' if `touse'
  julia, qui: p = partial_out(df, @formula(`:subinstr local varlist " " " + ", all' ~ 1 `feterms') `wtopt', add_mean=false)
  
  if `"`prefix'"' != "" local generate `prefix'`: subinstr local varlist " " " `prefix'", all'
  confirm `=cond("`replace'"=="", "new var", "names")' `generate'

  julia GetVarsFromDFNoMissing `generate' if `touse', df(p[1]) source(`varlist') `replace'
  
  julia, qui: df=p=1
end
