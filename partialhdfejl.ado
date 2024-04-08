// Perform HDFE partialling in a common sample

cap program drop partialhdfejl
program define partialhdfejl
  version 15

  syntax varlist [if] [in] [aw pw fw iw/], Absorb(string) [GENerate(string) PREfix(string) replace ITerations(integer 16000) gpu TOLerance(real 1e-8) compact]

  _assert `iterations'>0, msg("{cmdab:It:erations()} must be positive.") rc(198)
  _assert `tolerance'>0, msg("{cmdab:tol:erance()} must be positive.") rc(198)

  if (`"`generate'"'!="") + (`"`prefix'"'!="") != 1 {
    di as err "Specify exactly one of {cmdab:gen:erate()} or {cmdab:pre:fix()}."
    exit 198
  }
  
  marksample touse

  local gpulib = cond(c(os)=="MacOSX", "Metal", "CUDA")
  if "`gpu'"!="" local methodopt , method = :`gpulib'

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

  reghdfejl_load
  
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

  if "`compact'"!="" {
    tempfile compactfile
    save "`compactfile'"
    keep `varlist' `absorbvars' `wtvar' `touse'
    qui keep if `touse'
  }

  local vars `varlist' `absorbvars' `wtvar'
  local vars: list uniq vars

  jl PutVarsToDF `vars' if `touse', nomissing double
  qui jl: size(df,1)
  _assert `r(ans)', rc(2001) msg(insufficient observations)

  if "`compact'" !="" drop _all

  jl: p = partial_out(df, @formula(`:subinstr local varlist " " " + ", all' ~ 1 `feterms') `wtopt', tol=`tolerance', maxiter=`iterations' `methodopt');

  if "`compact'"!="" {
    jl: GC.gc();
    use `compactfile'
  }

  if `"`prefix'"' != "" local generate `prefix'`: subinstr local varlist " " " `prefix'", all'
    else if "`replace'"!="" {
      tempname t
      jl: SF_scal_save("`t'", size(df)[1]);
      if `t' < _N {
        foreach var in `generate' {
          cap replace `var' = .
        }
      }
    }

  jl GetVarsFromDF `generate' if `touse', source(p[1]) cols(`varlist') `replace'

  foreach var in `generate' {
    label var `var' "Residuals"
  }

  jl: df = p = nothing;
end
