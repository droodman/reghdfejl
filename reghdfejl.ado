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
	syntax anything [if] [in] [aw pw iw/], [Absorb(string) Robust VCE(string) CLuster(string) RESiduals(name) replace gpu noConstant NOAbsorb THReads(integer 4) noSAMPle TOLerance(real 1e-8) *]
  local sample = "`sample'"==""

  _get_diopts diopts options, `options'

  if `"`options'"' != "" di as inp `"`options'"' as txt " ignored" _n

  marksample touse

  if `tolerance' <=0 {
    di as err _n "{cmdab:tol:erance()} option not positive."
    exit 198
  }
  
  if "`gpu'"!="" {
    if c(os)=="MacOSX" {
      di _n "{cmd:gpu} option ignored because it only works on computers with NVIDIA GPUs." _n
      local gpu
    }
    else local methodopt , method = :CUDA
  }
  local threadsopt , nthreads = `threads'
  
  local hascons = `"`constant'`absorb'"'==""

  if `"$reghdfejl_julia_loaded"'=="" {
    julia      AddPkg CUDA FixedEffectModels DataFrames Vcov
    julia, qui: using CUDA,FixedEffectModels,DataFrames,Vcov
    global reghdfejl_julia_loaded 1
  }

  local wtvar `exp'
  if "`weight'"=="pweight" & `"`robust'`cluster'`vce'"'=="" local robust robust  // pweights implies robust

  if `"`absorb'"' != "" {
    local hasfe 1

    ParseAbsorb `absorb'
    local N_hdfe `r(N_hdfe)'
    local absorb `r(absorb)'
    local absorbvars `r(absorbvars)'
    local feterms `r(feterms)'
    markout `touse' `absorbvars'
  }
  else if !0`hascons' local feterms + 0

  tokenize `anything'
  _fv_check_depvar `1'
  local depname `1'
  macro shift

  local hasiv = strpos(`"`*'"', "=")
  if `hasiv' {
    local t = regexm(`"`*'"', "^([^\(]*)\(([^=]*)=([^\)]*)\)(.*)$")
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
  _vce_parse, optlist(Robust) argoptlist(CLuster) pwallowed(robust) old: `wgtexp', `robust' vce(`vce')
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
    if "``varset'name'" != "" {
      fvrevar ``varset'name' if `touse'
      local `varset' `r(varlist)'
    }
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
 
  local saveopt = cond("`residuals'`savefe'`namedfe'"=="", "", ", save = :" + cond("`residuals'"=="", "fe", cond("`savefe'`namedfe'"=="", "residuals", "all")))

  julia PutVarsToDFNoMissing `vars' if `touse'  // put all vars in Julia DataFrame named df

  mata: st_local("inexog", invtokens(tokens("`inexog'"), "+"))  // put +'s in var lists
  if `hasiv' {
    mata: st_local("instd", invtokens(tokens("`instd'"), "+"))
    mata: st_local("insts", invtokens(tokens("`insts'"), "+"))
    local ivarg + (`instd' ~ `insts')
  }

  * Estimate!
  julia, qui: m = reg(df, @formula(`dep' ~ `inexog' `ivarg' `feterms') `wtopt' `vcovopt' `methodopt' `threadsopt' `saveopt', tol=`tolerance')
  tempname b V N t

  if "`savefe'`namedfe'" != "" {
    julia, qui: FEs = fe(m); replace!.(eachcol(FE), missing=>NaN); join(names(FEs), " ")
    local FEnamesjl `r(ans)'
    forvalues a = 1/`N_hdfe' {
      if "`savefe'`fename`a''"!="" {
        if "`fename`a''"=="" local fename`a' __hdfe`a'__
        julia GetVarsFromDF `fename`a'' if `touse', source(FEs) `:word `a' of `FEnamesjl''
        label var `fename`a'' "`:word `a' of `absorb''"
      }
    }
    julia, qui: FEs = nothing
  }

  if "`residuals'"!="" {
    julia, quietly: res = residuals(m); replace!(res, missing=>NaN)
    julia GetVarsFromMat `residuals' if `touse', source(res) `replace'
    julia, qui: res = nothing
  }

  julia, qui: SF_scal_save("`N'", nobs(m))
  if `sample' {
    julia, qui: esample = Vector{Float64}(m.esample)
    julia GetVarsFromMat `touse' if `touse', source(esample) replace
    julia, qui: esample = nothing
  }

  if "`inexog'`ivarg'" == "" {  // if there are no coefficient estimates...
    local b
    local V
  }
  else {
    julia, qui: I = [1+`kinexog'+`hascons':length(coef(m)) ; 1+`hascons':`kinexog'+`hascons' ; 1:`hascons']  // cons-exog-endog -> endog-exog-cons
    julia, qui: `b' = collect(coef(m)[I]')
    julia, qui: `V' = replace!(vcov(m)[I,I], NaN=>0.)
    julia GetMatFromMat `b'
    julia GetMatFromMat `V'
    local coefnames `instdname' `inexogname' `=cond(`hascons', "_cons", "")'

    mat colnames `b' = `coefnames'
    mat colnames `V' = `coefnames'
    mat rownames `V' = `coefnames'
   
    forvalues i=1/`=`kinexog'+`kinstd'' {
      if `V'[`i',`i']==0 di as txt "note: `:word `i' of `coefnames'' omitted because of collinearity"
    }
  }
  ereturn post `b' `V', depname(`depname') obs(`=`N'') buildfvinfo findomitted `=cond(`sample', "esample(`touse')", "")'

  ereturn scalar N_hdfe = 0`N_hdfe'
  julia, qui: SF_scal_save("`t'", size(df,1))
  ereturn scalar N_full = `t'
  mata st_numscalar("e(rank)", rank(st_matrix("e(V)")))
  ereturn scalar df_m = e(rank)
  julia, qui: SF_scal_save("`t'", dof_fes(m))
  ereturn scalar df_a = `t'
  julia, qui: SF_scal_save("`t'", dof_residual(m))
  ereturn scalar df_r = `t'
  julia, qui: SF_scal_save("`t'", rss(m))
  ereturn scalar rss = `t'
  julia, qui: SF_scal_save("`t'", mss(m))
  ereturn scalar mss = `t'
  julia, qui: SF_scal_save("`t'", r2(m))
  ereturn scalar r2`' = `t'
  julia, qui: SF_scal_save("`t'", adjr2(m))
  ereturn scalar r2_a = `t'
  julia, qui: SF_scal_save("`t'", m.F)
  ereturn scalar F = `t'
  julia, qui: SF_scal_save("`t'", m.iterations)
  ereturn scalar ic = `t'
  julia, qui: SF_scal_save("`t'", m.converged)
  ereturn scalar converged = `t'
  julia, qui: SF_scal_save("`t'", size(df,1) - nobs(m))
  ereturn scalar num_singletons = `t'
  ereturn scalar rmse = sqrt(e(rss) / (e(N) - e(df_a) - e(rank)))
  ereturn scalar ll  = -e(N)/2*(1 + log(2*_pi / e(N) *  e(rss)          ))
  ereturn scalar ll0 = -e(N)/2*(1 + log(2*_pi / e(N) * (e(rss) + e(mss))))

  if 0`hasfe' {
    julia, qui: SF_scal_save("`t'", m.r2_within)
    ereturn scalar r2_within = `t'
  }

  if "`wtvar'"=="" ereturn scalar sumweights = e(N)
  else {
    julia, qui: SF_scal_save("`t'", mapreduce((w,s)->(s ? w : 0), +, df.`wtvar', m.esample; init = 0))
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
        julia, qui: SF_scal_save("`t'", m.nclusters[`i'])
        ereturn scalar N_clust`i' = `t'
      }
      julia, qui: SF_scal_save("`t'", minimum(m.nclusters))
      ereturn scalar N_clust = `t'
      ereturn local title3 Statistics cluster-robust
    }
  }

  ereturn scalar drop_singletons = e(num_singletons) > 0
  ereturn scalar report_constant = `hascons'
  ereturn local depvar `depname'
  ereturn local indepvars `inexogname' `instdname'

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
  ereturn local predict reghdfe_p
  ereturn local cmdline `cmdline'
  ereturn local cmd reghdfejl

  julia, qui: df = nothing  // yield memory

  Display, `diopts'
end

cap program drop Display
program define Display
  version 16
  syntax [, Level(real `c(level)') *]
  _get_diopts diopts, `options'

  if e(drop_singletons) di as txt `"(dropped `e(num_singletons)' {browse "http://scorreia.com/research/singletons.pdf":singleton observations})"'
  di as txt `"({browse "http://scorreia.com/research/hdfe.pdf":MWFE estimator} converged in `e(ic)' iterations)"'
  di
  di as txt "`e(title)' " _col(51) "Number of obs" _col(67) "= " as res %10.0fc e(N)
  di as txt "`e(title2)'" _col(51) "F(" as res %4.0f e(df_m) as txt "," as res %7.0f e(df_r)-e(report_constant) as txt ")" _col(67) "= " as res %10.2f e(F)
  di as txt "`e(title3)'" _col(51) "Prob > F"      _col(67) "= " as res %10.4f Ftail(e(df_m),e(df_r)-e(report_constant),e(F))
  di as txt               _col(51) "R-squared"     _col(67) "= " as res %10.4f e(r2)
  di as txt               _col(51) "Adj R-squared" _col(67) "= " as res %10.4f e(r2_a)

  forvalues i=1/0`e(N_clustervars)' {
    local line`i' as txt "Number of clusters (" as res e(clustvar`i') as txt ")" _col(29) " = " as res %10.0f e(N_clust`i')
  }
  di `line1' _col(51) as txt "Within R-sq." _col(67) "= " as res %10.4f e(r2_within)
  di `line2' _col(51) as txt "Root MSE"     _col(67) "= " as res %10.4f e(rmse)
  forvalues i=3/0`e(N_clustervars)' {
    di line`i'
  }
  di
  ereturn display, level(`level') `diopts'
end

cap program drop ParseAbsorb
program define ParseAbsorb, rclass
  syntax anything(equalok), [SAVEfe]
  tokenize `anything', parse(" =")

  local a 1
  while `"`1'"' != "" {
    if `"`2'"' == "=" {
      local fename`a' `1'
      confirm new var fename`a'
      macro shift 2
      local namedfe 1
    }
    local ++a
    local feterms `feterms' `1'
    macro shift
  }
  return local absorb `feterms'

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