*! reghdfejl 0.2.1 24 November 2023

// The MIT License (MIT)
//
// Copyright (c) 2023 David Roodman
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.


*! Version history at bottom

cap program drop reghdfejl
program define reghdfejl, eclass
  version 16

  if replay() {
		if "`e(cmd)'" != "reghdfejl" error 301
		if _by() error 190
		Display `0'
		exit 0
	}

  local cmdline `0'
	syntax anything [if] [in] [aw pw iw/], [Absorb(string) Robust CLuster(string) vce(string) RESIDuals ITerations(integer 16000) gpu THReads(integer 0) ///
                                          noSAMPle TOLerance(real 1e-8) Level(real `c(level)') NOHEADer NOTABLE *]
  local sample = "`sample'"==""

  _assert `iterations'>0, msg("{cmdab:It:erations()} must be positive.") rc(198)
  _assert `tolerance'>0, msg("{cmdab:tol:erance()} must be positive.") rc(198)
  
  _get_diopts diopts options, `options'

  marksample touse
  
  if "`gpu'"!="" {
    local gpu = cond(c(os)=="MacOSX", "Metal", "CUDA")
    local methodopt , method = :`gpu'
  }

  if `threads' local threadsopt , nthreads = `threads'
  
  local hascons = `"`constant'`absorb'"'==""

  if `"$reghdfejl_loaded"'=="" {
    cap jl: nothing
    if _rc {
      di as err `"Can't access Julia. {cmd:reghdfejl} requires that {browse "https://github.com/JuliaLang/juliaup#installation":Julia be installed}."'
      di as err "And it requires that the Stata package {cmd:jl} be installed, via {stata ssc install jl}."
      exit 198
    }
    jl      AddPkg `gpu' FixedEffectModels DataFrames Vcov
    jl, qui: using `gpu',FixedEffectModels,DataFrames,Vcov
    global reghdfejl_loaded 1
  }

  local wtvar `exp'
  if "`weight'"=="pweight" & `"`robust'`cluster'`vce'"'=="" local robust robust  // pweights implies robust

  if `"`absorb'"' != "" {
    ParseAbsorb `absorb'
    local N_hdfe `r(N_hdfe)'
    local absorb `r(absorb)'
    local absorbvars `r(absorbvars)'
    local feterms `r(feterms)'
    local fenames `r(fenames)'
    local savefe `r(savefe)'
    local namedfe `r(namedfe)'

    markout `touse' `absorbvars'
  }
  else if !0`hascons' local feterms + 0

  tokenize `anything'
  local depname `1'
  macro shift

  local hasiv = strpos(`"`*'"', "=")
  if `hasiv' {
    local t = regexm(`"`*'"', "^([^\(]*)\(([^=]*)=([^\)]*)\)(.*)$")  // standard IV syntax
    fvexpand `=regexs(1)' `=regexs(4)'
    local inexogname `r(varlist)'
    fvexpand `= regexs(2)'
    local instdname `r(varlist)'
    fvexpand `=regexs(3)'
    local instsname `r(varlist)'
  }
  else {
    fvexpand `*'
    local inexogname `r(varlist)'
  }

  local varlist `depname' `instdname' `inexogname' `instsname'
  markout `touse' `varlist'

  if `"`cluster'"' != "" local vce cluster `cluster'  // move clustering vars in cluster() to vce() because _vce_parse can only handle >1 var in latter
  _vce_parse, optlist(Robust UNadjusted ols) argoptlist(CLuster) pwallowed(robust) old: `wgtexp', `robust' vce(`vce')
	local vce `r(vceopt)'
	local robust `r(robust)'
	local cluster `r(cluster)'
	if "`cluster'"!="" markout `touse' `cluster', strok

  if "`cluster'"=="" {
    if "`robust'"!="" {
      local vcovopt , Vcov.robust()
    }
  }
  else {
    mata st_local("vcovopt", invtokens(":":+tokens("`cluster'"),","))
    local vcovopt , Vcov.cluster(`vcovopt')
  }

  foreach varset in dep inexog instd insts {
    if strpos("``varset'name'", ".") {
      fvrevar ``varset'name' if `touse'
      local `varset' `r(varlist)'
    }
    else local `varset' ``varset'name'
    local k`varset': word count ``varset''
  }
  _assert `kdep'==1, msg("Multiple dependent variables specified.") rc(198) 

  if `"`wtvar'"' != "" {
    cap confirm var `wtvar'
    if _rc {
      tempname wtvar
      gen double `wtvar' = `exp' if `touse'
    }
    local wtopt , weights = :`wtvar'
  }

  local vars `dep' `inexog' `instd' `insts' `cluster' `wtvar' `absorbvars'
  local vars: list uniq vars

  if "`residuals'" != "" {
    cap drop _reghdfejl_resid
    local residuals _reghdfejl_resid
  }
  else {
    local 0, `options'
    syntax, [RESIDuals(name) *]
  }

  if `"`options'"' != "" di as inp `"`options'"' as txt " ignored" _n

  local saveopt = cond("`residuals'`savefe'`namedfe'"=="", "", ", save = :" + cond("`residuals'"=="", "fe", cond("`savefe'`namedfe'"=="", "residuals", "all")))

  jl PutVarsToDFNoMissing `vars' if `touse'  // put all vars in Julia DataFrame named df

  mata: st_local("inexog", invtokens(tokens("`inexog'"), "+"))  // put +'s in var lists
  if `hasiv' {
    mata: st_local("instd", invtokens(tokens("`instd'"), "+"))
    mata: st_local("insts", invtokens(tokens("`insts'"), "+"))
    local ivarg + (`instd' ~ `insts')
  }

  * Estimate!
  jl, qui: m = reg(df, @formula(`dep' ~ `inexog' `ivarg' `feterms') `wtopt' `vcovopt' `methodopt' `threadsopt' `saveopt', tol=`tolerance', maxiter=`iterations')

  if "`savefe'`namedfe'" != "" {
    jl, qui: FEs = fe(m); SF_macro_save("reghdfejl_FEnames", join(names(FEs), " "))
    forvalues a = 1/`N_hdfe' {
      local fename: word `a' of `fenames'
      if "`savefe'`fename'"!="" {
        if "`fename"=="" local fename __hdfe`a'__
        jl GetVarsFromDF `fename' if `touse', source(FEs) col(`:word `a' of $reghdfejl_FEnames')
        label var `fename' "[FE] `:word `a' of `absorb''"
      }
    }
    jl, qui: FEs = nothing
  }

  if "`residuals'"!="" {
    jl, quietly: res = residuals(m); replace!(res, missing=>NaN)
    jl GetVarsFromMat `residuals' if `touse', source(res) `replace'
    label var `residuals' "Residuals"
    jl, qui: res = nothing
  }

  tempname t

  jl, qui: SF_scal_save("`t'", nobs(m))
  if `sample' {
    jl, qui: esample = Vector{Float64}(m.esample)
    jl GetVarsFromMat `touse' if `touse', source(esample) replace
    jl, qui: esample = nothing
  }

  if "`inexog'`ivarg'" != "" {  // if there are no coefficient estimates...
    tempname b V

    jl, qui: I = [1+`kinexog'+`hascons':length(coef(m)) ; 1+`hascons':`kinexog'+`hascons' ; 1:`hascons']  // cons-exog-endog -> endog-exog-cons
    jl, qui: `b' = collect(coef(m)[I]')
    jl, qui: `V' = replace!(vcov(m)[I,I], NaN=>0.)
    jl GetMatFromMat `b'
    jl GetMatFromMat `V'
    local coefnames `instdname' `inexogname' `=cond(`hascons', "_cons", "")'

    mat colnames `b' = `coefnames'
    mat colnames `V' = `coefnames'
    mat rownames `V' = `coefnames'
   
    forvalues i=1/`=`kinexog'+`kinstd'' {
      if `V'[`i',`i']==0 di as txt "note: `:word `i' of `coefnames'' omitted because of collinearity"
    }
  }
  ereturn post `b' `V', depname(`depname') obs(`=`t'') buildfvinfo findomitted `=cond(`sample', "esample(`touse')", "")'

  ereturn scalar N_hdfe = 0`N_hdfe'
  jl, qui: SF_scal_save("`t'", size(df,1))
  ereturn scalar N_full = `t'
  mata st_numscalar("e(rank)", rank(st_matrix("e(V)")))
  ereturn scalar df_m = e(rank)
  jl, qui: SF_scal_save("`t'", dof_fes(m))
  ereturn scalar df_a = `t'
  jl, qui: SF_scal_save("`t'", dof_residual(m))
  ereturn scalar df_r = `t'
  jl, qui: SF_scal_save("`t'", rss(m))
  ereturn scalar rss = `t'
  jl, qui: SF_scal_save("`t'", mss(m))
  ereturn scalar mss = `t'
  jl, qui: SF_scal_save("`t'", r2(m))
  ereturn scalar r2`' = `t'
  jl, qui: SF_scal_save("`t'", adjr2(m))
  ereturn scalar r2_a = `t'
  jl, qui: SF_scal_save("`t'", m.F)
  ereturn scalar F = `t'
  jl, qui: SF_scal_save("`t'", m.iterations)
  ereturn scalar ic = `t'
  jl, qui: SF_scal_save("`t'", m.converged)
  ereturn scalar converged = `t'
  jl, qui: SF_scal_save("`t'", size(df,1) - nobs(m))
  ereturn scalar num_singletons = `t'
  ereturn scalar rmse = sqrt(e(rss) / (e(N) - e(df_a) - e(rank)))
  ereturn scalar ll  = -e(N)/2*(1 + log(2*_pi / e(N) *  e(rss)          ))
  ereturn scalar ll0 = -e(N)/2*(1 + log(2*_pi / e(N) * (e(rss) + e(mss))))

  if 0`N_hdfe' {
    jl, qui: SF_scal_save("`t'", m.r2_within)
    ereturn scalar r2_within = `t'
  }

  if "`wtvar'"=="" ereturn scalar sumweights = e(N)
  else {
    jl, qui: SF_scal_save("`t'", mapreduce((w,s)->(s ? w : 0), +, df.`wtvar', m.esample; init = 0))
    ereturn scalar sumweights = `t'
  }

  if "`cluster'`robust'"=="" ereturn local vce ols
  else {
    ereturn local vcetype Robust
    if "`cluster'"=="" {
      ereturn local vce robust
      ereturn local title3 Statistics robust to heteroskedasticity
    }
    else {
      ereturn local vce cluster
      ereturn local clustvar `cluster'
      ereturn scalar N_clustervars = `:word count `cluster''
      tokenize `cluster'
      forvalues i=1/`e(N_clustervars)' {
        ereturn local clustvar`i' ``i''
        jl, qui: SF_scal_save("`t'", m.nclusters[`i'])
        ereturn scalar N_clust`i' = `t'
      }
      jl, qui: SF_scal_save("`t'", minimum(m.nclusters))
      ereturn scalar N_clust = `t'
      ereturn local title3 Statistics cluster-robust
    }
  }

  ereturn scalar drop_singletons = e(num_singletons) > 0
  ereturn scalar report_constant = `hascons'
  ereturn local depvar `depname'
  ereturn local indepvars `inexogname' `instdname'
  ereturn local resid `residuals'

  if `hasiv' {
    ereturn local model iv
    ereturn local inexog `inexogname'
    ereturn local instd `instdname'
    ereturn local insts `instsname'
  }
  else ereturn local model ols

  ereturn local title HDFE `=cond(`hasiv', "2SLS", "linear")' regression with Julia
  if 0`N_hdfe' ereturn local title2 Absorbing `N_hdfe' HDFE `=plural(0`N_hdfe', "group")'
  ereturn local absvars `absorb'
  ereturn local marginsnotok Residuals SCore
  ereturn local predict reghdfejl_p
  ereturn local estat_cmd reghdfejl_estat
  ereturn local cmdline `cmdline'
  ereturn local cmd reghdfejl

  jl, qui: df = nothing  // yield memory

  Display, `diopts' level(`level') `noheader' `notable'
end

cap program drop Display
program define Display
  version 16
  syntax [, Level(real `c(level)') noHEADer notable *]
  _get_diopts diopts, `options'

  if "`header'"=="" {
    if e(drop_singletons) di as txt `"(dropped `e(num_singletons)' {browse "http://scorreia.com/research/singletons.pdf":singleton observations})"'
    di as txt `"({browse "http://scorreia.com/research/hdfe.pdf":MWFE estimator} converged in `e(ic)' iterations)"'
    di
    di as txt "`e(title)' " _col(51) "Number of obs" _col(67) "= " as res %10.0fc e(N)
    di as txt "`e(title2)'" _col(51) "F(" as res %4.0f e(df_m) as txt "," as res %7.0f e(df_r)-e(report_constant) as txt ")" _col(67) "= " as res %10.2f e(F)
    di as txt "`e(title3)'" _col(51) "Prob > F"      _col(67) "= " as res %10.4f Ftail(e(df_m), e(df_r)-e(report_constant), e(F))
    di as txt               _col(51) "R-squared"     _col(67) "= " as res %10.4f e(r2)
    di as txt               _col(51) "Adj R-squared" _col(67) "= " as res %10.4f e(r2_a)

    forvalues i=1/0`e(N_clustervars)' {
      local line`i' as txt "Number of clusters (" as res e(clustvar`i') as txt ")" _col(29) " = " as res %10.0f e(N_clust`i')
    }
    di `line1' _col(51) as txt "Within R-sq." _col(67) "= " as res %10.4f e(r2_within)
    di `line2' _col(51) as txt "Root MSE"     _col(67) "= " as res %10.4f e(rmse)
    forvalues i=3/0`e(N_clustervars)' {
      di `line`i''
    }
    di
  }

  if "`table'"=="" ereturn display, level(`level') `diopts'
end

cap program drop ParseAbsorb
program define ParseAbsorb, rclass
  version 16

  syntax anything(equalok), [SAVEfe]
  tokenize `anything', parse(" =")

  return local savefe `savefe'

  while `"`1'"' != "" {
    if `"`2'"' == "=" {
      confirm new var `1'
      local fenames = `"`fenames'"' + " `1'"
      macro shift 2
      return local namedfe 1
    }
    else local fenames = `"`fenames'"' + `" "" "'
    local feterms `feterms' `1'
    macro shift
  }
  return local absorb `feterms'
  return local fenames `fenames'

  return local N_hdfe: word count `feterms'
  local feterms: subinstr local feterms " " " i.", all
  fvunab _feterms: i.`feterms'
  local feterms
  foreach exp in `_feterms' {  // drop un-prefixed continuous terms from ## expansion
    if substr("`exp'",1,2)=="i." local feterms `feterms' `exp'
  }

  local absorbvars `feterms'
  local feterms: subinstr local feterms "##c." ")*(", all
  local feterms: subinstr local feterms "#c." ")&(", all
  local feterms: subinstr local feterms "##i." ")&fe(", all
  local feterms: subinstr local feterms "#i." ")&fe(", all
  local feterms: subinstr local feterms "i." "fe(", all
  local feterms: subinstr local feterms " " ") + ", all
  return local feterms + `feterms')

  local absorbvars: subinstr local absorbvars "i." " ", all
  local absorbvars: subinstr local absorbvars "c." " ", all
  return local absorbvars: subinstr local absorbvars "#" " ", all
end
