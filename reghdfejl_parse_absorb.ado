*! reghdfejl 1.1.1 30 March 2025

cap program drop reghdfejl_parse_absorb
program define reghdfejl_parse_absorb, rclass
  syntax anything(equalok), [SAVEfe]
  tempname vars

  * like tokenize `anything', parse(" ="), but with bind option to keep parenthesized expressions together
  local i 0
  gettoken term anything: anything, bind parse(" =")
  while `"`term'"'!="" {
    local `++i': copy local term
    gettoken term anything: anything, bind parse(" =")
  }
  local `++i'
  local `++i'

  while `"`1'"' != "" {
    local t = "`2'"' == "="
    if `t' {
      confirm new var `1'
      return local fenames = `"`return(fenames)'"' + " `1'"
      macro shift 2
      return local namedfe 1
    }
    cap confirm var `1'
    if !_rc local 1 i.`1'  // prefix plain var name with "i."
    fvunab varlist: `1'
    mata `vars'=tokens(st_local("varlist")); `vars'=select(`vars', strmatch(`vars', "*.*") :| strmatch(`vars', "*#*")); st_local("varlist", length(`vars')? invtokens(`vars') : "")  // drop any continuous "var2" term generated from "var2##c.var2"
    local feterms `feterms' `varlist'
    return local fenames = `"`return(fenames)'"' + `" "" "' * (`:word count `varlist'' - `t')
    macro shift
  }
  return local absorb: copy local feterms
  return local N_hdfe: word count `feterms'

  local absorbvars: copy local feterms
  local feterms i.`: subinstr local feterms " " " i.", all'
  local feterms: subinstr local feterms "i.i." "i.", all
  local feterms: subinstr local feterms "i.c." "c.", all
  local feterms: subinstr local feterms "#c." ")&(", all
  local feterms: subinstr local feterms "#i." ")&fe(", all
  local feterms: subinstr local feterms "i." "fe(", all
  local feterms: subinstr local feterms " " ") + ", all
  local feterms: subinstr local feterms ")" " )", all
  local feterms: subinstr local feterms "(" "( ", all
  local feterms + `feterms' )

  local absorbvars: subinstr local absorbvars "i." " ", all
  local absorbvars: subinstr local absorbvars "c." " ", all
  local absorbvars: subinstr local absorbvars "#" " ", all
  local absorbvars: list uniq absorbvars

  foreach var in `absorbvars' {
    cap confirm numeric var `var'
    if _rc {
      tempvar t
      qui egen long `t' = group(`var') if `touse'
      local absorbvars: subinstr local absorbvars "`var'" "`t'", word all
      local feterms   : subinstr local feterms    "`var'" "`t'", word all
    }
  }
  return local feterms: copy local feterms
  return local absorbvars: copy local absorbvars
end
