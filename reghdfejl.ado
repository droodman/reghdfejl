*! reghdfejl 1.1.8 11 January 2026

// The MIT License (MIT)
//
// Copyright (c) 2023-26 David Roodman
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


* Version history at bottom
cap program drop reghdfejl
program define reghdfejl
  version 15
  
  if replay() {
    if "`e(cmd)'" != "reghdfejl" error 301
    if _by() error 190
    Display `0'
    exit 0
  }

  qui jl GetEnv
  local env `r(env)'
  reghdfejl_load

  cap noi _reghdfejl `0'

  local rc = _rc
  qui jl SetEnv `env'
  
  if `rc' & "`noncompactfile'"!="" use `noncompactfile'
  forvalues i=1/0$reghdfejl_stringvar_ct {
    cap drop reghdfejl_stringvar_`i'
  }
  macro drop reghdfejl_stringvar_ct
  exit `rc'
end

cap program drop _reghdfejl
program define _reghdfejl, eclass
  version 15

  local cmdline: copy local 0

  if `"`0'"'=="mask" {
    cap findfile reghdfe.ado
    if _rc {
      qui findfile reghdfejl_masker.ado
      mata pathsplit("`r(fn)'", _reghdfejlp1="", _reghdfejlp2=""); st_local("dest", _reghdfejlp1+"/reghdfe.ado")
      copy "`r(fn)'" "`dest'", replace
      di _n as txt "File copied:"
      di as res "  `r(fn)' -> `dest'"
    }
    else {
      local reghdfeado `r(fn)'
      qui mata st_local("firstline", fget(fh = fopen("`r(fn)'", "r"))); _fclose(fh)
      if `"`firstline'"'=="*! REGHDFEJLMASKER" {
        di as txt "Already masked."
        exit 0
      }
      mata st_local("dest", pathrmsuffix("`reghdfeado'") + "_backup_.ado")
      copy "`reghdfeado'" "`dest'", replace
      qui findfile reghdfejl_masker.ado
      cap noi copy "`r(fn)'" "`reghdfeado'", replace
      if _rc==608 {
        di as err "If you have run reghdfe in this Stata session, it can't be masked now. Restart Stata first."
        error 608
      }
      di _n as txt "Files copied:"
      di as res "  `reghdfeado' -> `dest'"
      di as res "  `r(fn)' -> `reghdfeado'"
    }
    exit 0
  }

  if `"`0'"'=="unmask" {
    cap findfile reghdfe_backup_.ado
    if _rc {
      di as err `"Can't find the reghdfe.ado backup file, "reghdfe_backup_.ado". Reinstall reghdfe by typing or clicking on:"'
      di "{stata ssc install reghdfe, replace}"
      exit 198
    }
    local source `r(fn)'
    cap findfile reghdfe_p.ado
    if _rc qui findfile reghdfejl.ado
    mata pathsplit("`r(fn)'", _reghdfejlp1="", _reghdfejlp2=""); st_local("dest", _reghdfejlp1+"/reghdfe.ado")
    copy "`source'" "`dest'", replace
    di _n as txt "File copied:"
    di as res "  `source' -> `dest'"
    di as txt "The change will take effect after you restart Stata."
    exit 0
  }

	syntax anything [if] [in] [aw pw iw/], [Absorb(string) Robust CLuster(string) SMall vce(string) RESIDuals ITerations(integer 16000) gpu THReads(integer 0) ///
                                          noSAMPle TOLerance(real 1e-8) Level(real `c(level)') NOHEADer NOTABLE compact VERBose INTERruptible noCONStant ///
                                          /*EXPosure(varlist max=1) OFFset(varlist max=1)*/ KEEPSINgletons SEParation(string) FAMily(string) link(string) ivreg2 *]
  local sample = "`sample'"==""
  local compact = "`compact'"!=""
  local ivreg2 = "`ivreg2'"!=""
  local GLM = `"`family'`link'"'!=""

  _assert `iterations'>0, msg({cmdab:it:erations()} must be positive) rc(198)
  local iteropt , maxiter=`iterations'

	_get_eformopts, soptions eformopts(`options') allowed(hr shr IRr or RRr)
	local eformopts `s(eform)'
  _get_diopts diopts _options, `s(options)'

  marksample touse

  local gpulib = cond(c(os)=="MacOSX", "Metal", "CUDA")
  if "`gpu'"!="" local methodopt , method = :`gpulib'

  if `threads' local threadsopt , nthreads = `threads'

  if "`keepsingletons'"!="" local singletonopt , drop_singletons = false

  local haswt = `"`exp'"' != ""
  if `haswt' {
    local wtype: copy local weight
    local wexp `"=`exp'"'
    cap confirm var `exp'
    if _rc {
      tempname wtvar
      qui gen double `wtvar' = `exp' if `touse'
    }
    else local wtvar: copy local exp
    local wtopt , weights = :`wtvar'
    if "`weight'"=="pweight" & `"`vce'"'=="" local robust robust
  }
  
  local hasiv 0
  gettoken depname anything: anything, bind
  _fv_check_depvar `depname'
  while "`anything'"!="" {
    gettoken term anything: anything, match(parenflag) bind
    if "`parenflag'"=="(" {
      _assert !`GLM', msg(IV only for linear models) rc(198)
      local hasiv 1
      tokenize "`term'", parse("=")
      local instdname: copy local 1
      local instsname: copy local 3
      local inexogname `inexogname' `anything'
      continue, break
    }
    else if subinstr(`"`term'"'," ","",.) != "[]" local inexogname `inexogname' `term'  // handle rare event of empty weight opt, "[]"
  }
  if !`hasiv' local ivreg2 0

  markout `touse' `wtvar' `depname' `instdname' `inexogname' `instsname'

  if `"`vce'"' != "" {
    _assert `"`cluster'"'=="", msg(only one of cluster() and vce() can be specified) rc(198)
    _assert `"`robust'"' =="", msg(only one of robust and vce() can be specified   ) rc(198)
    tokenize `"`vce'"', parse(" ,")
    local 0, `1'
    syntax, [Robust CLuster UNadjusted ols bs BOOTstrap]
    _assert "`robust'`cluster'`unadjusted'`ols'`bs'`bootstrap'"!="", msg("vcetype '`0'' not allowed") rc(198)
    local bs = "`bs'`bootstrap'" != ""
    macro shift
    if `bs' {
      _assert !`ivreg2', msg(fast bootstrapping not available when using ivreg2) rc(198)
      local 0 `*'
      syntax, [CLuster(string) Reps(integer 50) mse seed(string) SIze(integer 0) PROCs(integer 1) SAving(string)]
      _assert `reps'>1, msg(reps() must be an integer greater than 1) rc(198)
      _assert `size'>=0, msg(size() must be a positive integer) rc(198)
      _assert `procs'>=0, msg(procs() must be a positive integer) rc(198)
      if `procs'==0 local procs 1

      if `"`saving'"'!="" {
        _assert c(stata_version)>=16, rc(198) msg(vce(bs, saving()) requires Stata 16 or later)
        local 0 using `saving'
        syntax using/, [DOUBle replace]
        local saving `using'`=cond(regexm("`using'", "^.*\.dta$"),"",".dta")'
        if "`replace'"=="" confirm new file `saving'
      }

      cap confirm numeric var `cluster'
      if _rc {
        tempvar bscluster
        qui egen long `bscluster' = group(`cluster')
      }
      else local bscluster: copy local cluster

      if `"`seed'"'!="" set seed `seed'
    }
    else if "`cluster'"!="" local cluster `*'
  }

  if `"`cluster'"'=="" {
    if "`robust'"!="" local vcovopt , Vcov.robust()
  }
  else {
    tokenize `"`cluster'"', parse(" #")
    local cluster `*'  // enforce uniform use of spaces
    local cluster: subinstr local cluster " # # " "#", all
    local cluster: subinstr local cluster " # " "#", all
    foreach term in `cluster' {
      cap confirm numeric var `term'
      if _rc {  // allow clustering on interactions
        if strpos(`"`term'"', "#") local term: subinstr local term "#" " ", all
        tempvar t
        qui egen long `t' = group(`term')
        local _cluster `_cluster' `t'
      }
      else local _cluster `_cluster' `term'
    }
    markout `touse' `_cluster', strok
    mata st_local("vcovopt", " , Vcov.cluster(" + invtokens(":":+tokens("`_cluster'"),",") + ")")
  }

  if `"`absorb'"' != "" {
    local 0: copy local absorb
    syntax anything(equalok), [SAVEfe]
    reghdfejl_parse_absorb `anything' if `touse'
    foreach macro in fenames feterms namedfe absorb N_hdfe absorbvars {
      local `macro' `"`r(`macro')'"'
    }
    markout `touse' `absorbvars'
  }
  else local feterms + `="`constant'"==""'

  if `GLM' {
    local nl nl
    _assert `"`absorb'"'!="", msg(Doesn't yet accept nonlinear models with no fixed effects. Use {help glm} instead.) rc(198)

    _assert !`hasiv', msg(instrumental variables not accepted for nonlinear models) rc(198)
    _assert !`haswt', msg(weights not yet supported for nonlinear models) rc(198)

    if `"`separation'"'!="" {
      local 0, `separation'
      syntax, [fe ir simplex mu]
      _assert "`simplex'"=="", msg(separation(simplex) not yet supported) rc(198)
      local separation `fe' `ir' `simplex' `mu'
      local sepopt , separation=[:`:subinstr local separation " " ", :", all']
    }

    local families gaussian igaussian binomial nbinomial poisson gamma bernoulli geometric

    if `"`family'"'=="" local family gaussian
    else {
      tokenize `family'
      local 0, `1'
      syntax, [GAUssian IGaussian BInomial NBinomial Poisson Gamma BErnoulli GEOmetric]
      local family `gaussian' `igaussian' `binomial' `nbinomial' `poisson' `gamma' `bernoulli' `geometric'
      _assert `:word count `family''==1, msg(family(`*') not allowed) rc(198)
      if "`nbinomial'" != "" {
        if "`2'"=="" local 2 1  // default to (negative) binomial with denominator=1
          else confirm integer number `2'
        local familyopt , NegativeBinomial(`2')
      }
      else {
        if "`binomial'" != "" {
          sum `dep' if `touse', meanonly
          if r(max) != 1 {
            if "`dep'"!="`depname'" replace `dep' = `dep' / r(max) if `touse'
            else {
              tempname t
              gen double `t' = `dep' / r(max) if `touse'  // rescale dep var to [0,1]
              local dep: copy local t
            }
          }
        }
        local n: list posof "`family'" in families
        local familyopt , `:word `n' of Normal InverseGuassian Binomial NegativeBinomial Poisson Gamma Bernoulli Geometric'()
      }
    }

    if `"`link'"'=="" {
      local n: list posof "`family'" in families
      local linkopt , `:word `n' of Identity InverseSquareLink LogitLink LogLink LogLink InverseLink LogitLink LogLink'()  // canonical links but log for nbinomial
    }
    else {
      tokenize `0'
      local 0, `1'
      syntax, [Identity log Logit Probit Cloglog POWer OPOwer NBinomial LOGLog logc]
      _assert "`opower'`loglog'`logc'`probit'"=="", msg(link(`link') not supported) rc(198)
      local link `identity' `log' `logit' `cloglog' `nbinomial'
      _assert `:word count `link''==1, msg(link(`link') not allowed) rc(198)
      if "`power'"!="" {
        confirm number `2'
        local linkopt , PowerLink(`power')
      }
      else {
        local links identity log logit cloglog nbinomial
        local n: list posof "`link'" in links
        local linkopt , `:word `n' of Identity Log Logit Cloglog NegativeBinomial'Link()
      }
    }
  }
  else {  // only applicable to linear models fit with FixedEffectModels.jl
    _assert `tolerance'>0, msg({cmdab:tol:erance()} must be positive) rc(198)
    local tolopt, tol=`tolerance', progress_bar=false
  }

  if "`residuals'" != "" {
    local residuals
    forvalues i=1/120000 {
      cap confirm new var _reghdfejl_resid`i'
      if !_rc {
        local residuals _reghdfejl_resid`i'
        continue, break
      }
    }
  }
  else {
    local 0, `_options'
    syntax, [RESIDuals(name) *]
    local _options: copy local options
  }

  if `"`_options'"' != "" di as inp `"`_options'"' as txt " ignored" _n

  if "`residuals'`savefe'`namedfe'"!="" {
    _assert !`ivreg2', msg(residuals, savefe, and namedfe features not available when using ivreg2) rc(198)
    local saveopt , save = :`=cond("`residuals'"=="", "fe", cond("`savefe'`namedfe'"=="", "residuals", "all"))'
  }

  if `ivreg2' {
    foreach varset in dep inexog instd insts {
      fvexpand ``varset'name' if `touse'
      local `varset'expanded `r(varlist)'
      fvrevar ``varset'name' if `touse'
      local `varset'vars `r(varlist)'
      foreach var of local `varset'vars {
        tempname t
        local `varset'partialled ``varset'partialled' `t'
      }
      local allvars `allvars' ``varset'vars'
      local allpartialled `allpartialled' ``varset'partialled'
    }
  }
  else {
    // translate varlists into StatsModels formulas, translating factor terms directly when possible, fvrevar'ing otherwise; do in main proc because may create temp vars
    tempname termtab _termtab dummyrows freqs
    mata `termtab' = J(0,3,"")  // combined table of factor var terms with cols for i/c, ib value, varname
    foreach varset in dep inexog instd insts {
      if `'"``varset'name'"'!="" {
        if `hasiv' {
          fvexpand ``varset'name' if `touse'
          local `varset'expanded `r(varlist)'
        }

        fvunab varlist: ``varset'name'
        gettoken term varlist: varlist, bind
        local goodterms
        while "`term'"!="" {
          mata `_termtab' = J(0,3,"")  // table of factor var terms with cols for i/c, ib value, varname
          local norevar 1
          tokenize `term', parse("#")
          local _term `*'
          local newterm
          foreach factor of local _term {
            if "`factor'" != "#" {
              if regexm("`factor'", "^i\.(.*)$") {  // "i.[varname]"?
                sum `=regexs(1)' if `touse', meanonly
                if r(max)>r(min) mata `_termtab' = `_termtab' \ "i", "nothing", "`=regexs(1)'"  // skip i.var if var is constant in the sample
              }
              else if regexm("`factor'", "^i(b([0-9]+))\.(.*)$") {  // "ib[...].[varname]"?
                sum `=regexs(3)' if `touse', meanonly
                if r(max)>r(min) mata `_termtab' = `_termtab' \ "i", "`=regexs(2)'", "`=regexs(3)'"
              }
              else {  // continuous var, with or without "c."
                if substr(`"`factor'"',1,2)=="c." local factor = substr("`factor'", 3, .)
                cap confirm var `factor'
                if _rc {  // bad syntax; or ts op or "i()...." that can't be expressed with StatsModels.jl DummyCoding()
                  fvexpand `term' if `touse'
                  local `varset'names ``varset'names' `r(varlist)'
                  foreach var in `r(varlist)' {  // equivalent to fvrevar `r(varlist)' but a bit faster on big data sets
                    tempvar t
                    qui gen double `t' = `var' if `touse'
                    local `varset'vars ``varset'vars' `t'
                    local `varset'formula ``varset'formula' `t'
                    local putvars `putvars' `t'
                  }
                  local norevar 0
                  continue, break
                }
                mata `_termtab' = `_termtab' \ "c", "", "`factor'"
              }
            }
          }
          if `norevar' {
            mata st_local("term", invtokens(`_termtab'[,3]'))
            local term: subinstr local term " " "#", all
            local `varset'formula ``varset'formula' `term'
            local goodterms `goodterms' `term'
            mata `termtab' = `termtab' \ `_termtab'
          }
          gettoken term varlist: varlist, bind
        }
        fvrevar `goodterms', list
        local `varset'vars ``varset'vars' `r(varlist)'
        local `varset'names ``varset'names' `r(varlist)'
        local putvars `putvars' `r(varlist)'

        local `varset'formula: subinstr local `varset'formula " " " + ", all
        local `varset'formula: subinstr local `varset'formula "#" "&", all
      }
    }
    mata `termtab' = uniqrows(`termtab')  // get list of all primary variables to copy to Julia, deleting duplicates
    mata `dummyrows' = selectindex(`termtab'[,1]:=="i")
    mata st_local("dummyopt", invtokens(":" :+ `termtab'[`dummyrows',3]' :+ "=>DummyCoding(base=" :+ `termtab'[`dummyrows',2]' :+ "), "))
    local dummyopt , contrasts=Dict{Symbol, DummyCoding}(`dummyopt')

    mata `dummyrows' = uniqrowsfreq(uniqrows(`termtab'[, 1\3])[,2], `freqs'=.)
    cap mata st_local("dups", invtokens(`dummyrows'[selectindex(`freqs':>1)]'))
    foreach dup in `dups' {  // any vars appearing with both i. and c.? (rare). reg() contrasts option needs them to have different names for different usages
      tempname t
      local dfaliascmds `dfaliascmds' df.`t' = df.`dup'; 
      foreach varset in dep inexog instd insts {
        local `varset'formula: subinstr local `varset'formula "c.`dup'" "c.`t'", word all
        local `varset'formula: subinstr local `varset'formula "`dup'"   "`t'"  , word all
        local `varset'names ``varset'names' `dup'
        local `varset'vars ``varset'vars' `t'
      }
    }
  }

  unab putvars: `putvars' `_cluster' `wtvar' `absorbvars' `bscluster'
  local putvars: list uniq putvars

  if `compact' {
    tempfile noncompactfile
    save "`noncompactfile'"
    c_local noncompactfile `noncompactfile'
    keep `putvars' `touse'
    qui keep if `touse'
  }
  else local iftouse if `touse'

  
  jl PutVarsToDF `putvars' `iftouse', nomissing doubleonly nolabel  // put all vars in Julia DataFrame named df; making it a global makes it visible to workers, for bs
  _jl: `dfaliascmds';  // create duplicate copies of vars used by both i. and c. (rare)

  if "`verbose'"!="" jl: df

  _jl: size(df,1)
  _assert `r(ans)', rc(2001) msg(insufficient observations)

  if `compact' drop _all

  if `ivreg2' {
    tempname ic df_a
    forvalues i=1/`:word count `allvars'' {
      local var: word `i' of `allvars'
      _jl: reghdfejl.p = reg(df, @formula(`:word `i' of `allvars'' ~ 1 `feterms') `wtopt' `tolopt' `iteropt' `methodopt', progress_bar=false, save=:residuals);
      _jl: reghdfejl.res = residuals(reghdfejl.p); replace!(reghdfejl.res, missing=>NaN);
      jl GetVarsFromMat `:word `i' of `allpartialled'' `iftouse', source(reghdfejl.res)
      _jl: reghdfejl.res = nothing;
    }
    _jl: st_numscalar("`df_a'", dof_fes(reghdfejl.p))
    _jl: st_numscalar("`ic'", reghdfejl.p.iterations);
    di as txt `"({browse "http://scorreia.com/research/hdfe.pdf":MWFE estimator} converged in `=`ic'' iterations)"'
    _jl: reghdfejl.esample = Vector{Float64}(reghdfejl.p.esample);
//     _jl: reghdfejl.p = partial_out(df, @formula(`:subinstr local allvars " " " + ", all' ~ 1 `feterms') `wtopt' `tolopt' `iteropt' `methodopt');
//     jl GetVarsFromDF `allpartialled' `iftouse', source(reghdfejl.p[1]) cols(`allvars')
//     _jl: st_numscalar("`df_a'", reghdfejl.p[5] - 1)
//     _jl: st_numscalar("`ic'", maximum(reghdfejl.p[3]))
//     _jl: reghdfejl.esample = Vector{Float64}(reghdfejl.p[2]);

    jl GetVarsFromMat `touse' `iftouse', source(reghdfejl.esample) replace
    _jl: reghdfejl.esample = nothing;

    local 0, `_options'
    syntax, [partial(string) first sfirst ffirst rf level(passthru) NOHEader NOFOoter EForm(passthru) DEPname(passthru) plus *]
    if `"`partial'"'!="" {
      fvexpand `partial' `iftouse'
      foreach var in `r(varlist)' {
        cap local _partial `_partial': word `:posof "`var'" in `inexog'expanded' of "`inexog'partialled'"
        if _rc _assert 0, msg(variable `var' not found) rc(111)
      }
    }

    * Estimate with ivreg2!
    cap ivreg2 `deppartialled' `inexogpartialled' (`instdpartialled' = `instspartialled') if `touse', partial(`_partial') cluster(`_cluster') `robust' `small' `options' nocons sdofminus(`=`df_a'') // XXX other VCE types
    if _rc error `=_rc'
    
    tempname M
    mat `M' = e(b)
    local colnames: colnames `M'
    _jl: reghdfejl.D = Dict(x=>y for (x,y) in zip(split("`allpartialled'"), split("`allvars'")));  // mapping from partialled, revar'd var names for ivreg2 back to display names
    _jl: st_global("reghdfejl_ans", join(getindex.(Ref(reghdfejl.D), split("`colnames'")), " "))
    mat colnames `M' = $reghdfejl_ans
    ereturn repost b=`M', rename
    foreach mat in S W {
      cap mat `M' = e(`mat')
      cap mat colnames `M' = `colnames'
      cap mat rownames `M' = `colnames'
      ereturn matrix `mat' = `M'
    }
    foreach macro in depvar depvar0 depvar1 instd instd0 instd1 insts insts0 insts1 exexog exexog0 exexog1 inexog inexog0 inexog1 partial partial0 partial1 {
      local t `e(`macro')'
      if "`t'"!="" {
        _jl: st_global("reghdfejl_ans", join(getindex.(Ref(reghdfejl.D), split("`t'")), " "))
        ereturn local `macro' $reghdfejl_ans
      }
    }

    ereturn scalar df_a = `df_a'
    ereturn scalar N_hdfe = `N_hdfe'
    ereturn scalar ic = `ic'
    _jl: st_numscalar("`M'", size(df,1) - nobs(reghdfejl.p));
    ereturn scalar num_singletons = `M'
    if `M' di as txt `"(dropped `e(num_singletons)' {browse "http://scorreia.com/research/singletons.pdf":singleton observations})"'

    ereturn local title `e(title)' with Julia
    ereturn local cmdline reghdfejl `cmdline'
    ivreg2, `diopts' `first' `sfirst' `ffirst' `rf' `level' `noheader' `nofooter' `eform' `depname' `plus'
    exit
  }

  if `hasiv' local ivarg + (`instdformula' ~ `instsformula')

  * Estimate!
  local flinejl f = @formula(`depformula' ~ `inexogformula' `ivarg' `feterms')
  local cmdlinejl `nl'reg(df, f `familyopt' `linkopt' `wtopt' `vcovopt' `methodopt' `threadsopt' `singletonopt' `saveopt' `sepopt' `tolopt' `iteropt' `dummyopt')
  _jl: `flinejl';
  _jl: m = 0;  // make m exist even if estimation fails
  if "`verbose'"!="" {
    di `"`flinejl'"'
    di `"m = `cmdlinejl'"'
    jl, `interruptible': m = `cmdlinejl'
  }
  else _jl, `interruptible': m = `cmdlinejl';

  _assert `"`r(ans)'"'!="sample is empty", msg(no observations) rc(2000)
  
  _jl: Int(m==0)
  _assert !`r(ans)', msg(estimation failed) rc(199)
  
  tempname k
  _jl: reghdfejl.k = length(coef(m)); st_numscalar("`k'", reghdfejl.k);
  _jl: reghdfejl.sizedf = size(df);
  if `haswt' _jl: sumweights = mapreduce((w,s)->(s ? w : 0), +, df.`wtvar', m.esample; init = 0);

  if `k' & 0`bs' {  // bootstrap vce
    local hasclust = "`bscluster'"!=""

    _jl: batches = [floor(Int, `reps'/`procs'*(t-1))+1:floor(Int,`reps'/`procs'*t) for t ∈ 1:`procs'];  // indexes to split simulations by CPU thread

    _jl: reghdfejl.bbs = Matrix{Float64}(undef, `reps', reghdfejl.k);
    _jl: reghdfejl.Vbs = zeros(Float64,`procs', reghdfejl.k, reghdfejl.k);
    _jl: reghdfejl.rngs = [StableRNG(`=runiformint(0, 1e6)' * t + 42) for t ∈ 1:`procs'];  // different, deterministic seeds for each thread
    _jl: reghdfejl.dfs = [DataFrame(df, copycols=false) for _ ∈ 1:`procs']  // copies of df with same underlying data; will hold different bs weights

    if `hasclust' {
      _jl: reghdfejl.s = OrderedSet(df.`bscluster'); reghdfejl.Nclust = length(reghdfejl.s);
      _jl: reghdfejl.id = getindex.(Ref(Dict(zip(reghdfejl.s, 1:reghdfejl.Nclust))), df.`bscluster');  // ordinalize cluster id
      _jl: reghdfejl.s = nothing
    }
    else _jl: reghdfejl.Nclust = size(df,1); reghdfejl.id = Colon();

    _jl: reghdfejl.bssize = iszero(0`size') ? reghdfejl.Nclust : 0`size';                                                             ///
         reghdfejl.wts = [Vector{Int}(undef, reghdfejl.Nclust) for _ ∈ 1:`procs'];                                                    ///
         Threads.@threads for t ∈ 1:`procs'                                                                                           ///
           @inbounds for m ∈ batches[t]                                                                                               ///
             fill!(reghdfejl.wts[t], 0);                                                                                              ///
             for _ ∈ 1:reghdfejl.bssize                                                                                               ///
               reghdfejl.wts[t][rand(reghdfejl.rngs[t], 1:reghdfejl.Nclust)] += 1                                                     ///
             end;                                                                                                                     ///
             reghdfejl.dfs[t].__reghdfejl_bswt .= reghdfejl.wts[t][reghdfejl.id];                                                     ///
             `=cond(`haswt', "reghdfejl.dfs[t].__reghdfejl_bswt .*= df.`wtvar';", "")'                                                ///
             reghdfejl.b = coef(`nl'reg(reghdfejl.dfs[t], f `familyopt' `linkopt', weights=:__reghdfejl_bswt, nthreads=1 `methodopt' `sepopt' `tolopt' `dummyopt')); ///
             reghdfejl.bbs[m,:] = reghdfejl.b;                                                                                        ///
             reghdfejl.Vbs[t,:,:] += reghdfejl.b * reghdfejl.b'                                                                       ///
           end                                                                                                                        ///
         end;                                                                                                                         ///
         reghdfejl.b = sum(reghdfejl.bbs; dims=1);                                                                                    ///
         reghdfejl.Vbs = (sum(reghdfejl.Vbs; dims=1)[1,:,:] .- reghdfejl.b' ./ `reps' .* reghdfejl.b) ./ (`reps' - `="`mse'"==""');   ///
         reghdfejl.id = reghdfejl.V = reghdfejl.rngs = reghdfejl.dfs = reghdfejl.dst = reghdfejl.wts = nothing;
  }

  if "`verbose'"=="" _jl: df = nothing;  // yield memory
  if `compact' {
    _jl: GC.gc();
    use `noncompactfile'
    c_local noncompactfile
  }

  if "`savefe'`namedfe'" != "" {
    _jl: FEs = fe(m); rename!(FEs, "FE" .* string.(1:`N_hdfe'));
    forvalues a = 1/`N_hdfe' {
      local fename: word `a' of `fenames'
      if "`savefe'`fename'"!="" {
        if "`fename'"=="" local fename __hdfe`a'__
        jl GetVarsFromDF `fename' if `touse', source(FEs) col(FE`a')
        label var `fename' "[FE] `:word `a' of `absorb''"
      }
    }
    _jl: FEs = nothing;
  }

  if "`residuals'"!="" {
    _jl: res = residuals(m); replace!(res, missing=>NaN);
    jl GetVarsFromMat `residuals' if `touse', source(res)
    label var `residuals' "Residuals"
    _jl: res = nothing;
  }

  tempname t N I used_df_r

  _jl: st_numscalar("`N'", nobs(m));

  if `sample' {
    tempname esample
    _jl: reghdfejl.esample = Vector{Float64}(m.esample);
    jl GetVarsFromMat `touse' if `touse', source(reghdfejl.esample) replace
    _jl: reghdfejl.esample = nothing;
  }

  if `k' {
    tempname b V
    _jl: st_numscalar("`t'", coefnames(m)[1]=="(Intercept)");
    local hascons = `t'
    _jl: reghdfejl.b = coef(m);
    _jl: reghdfejl.V = iszero(0`bs') ? vcov(m) : reghdfejl.Vbs;
    _jl: reghdfejl.V = replace!(reghdfejl.V, NaN=>0.);
    _jl: st_global("reghdfejl_ans", join(coefnames(m), "|"));
    varlistJ2S, jlcoefnames($reghdfejl_ans) vars(`inexogvars' `instdvars') varnames(`inexognames' `instdnames')
    global reghdfejl__coefnames `r(stcoefs)'
    global reghdfejl__instdnames `instdnames'
    _jl: reghdfejl.coefnames = "reghdfejl__coefnames" |> st_global |> split;
    _jl: `I' = [s=="_cons" ? 3 : s in split(st_global("reghdfejl__instdnames")) ? 1 : 2 for s in reghdfejl.coefnames] |> sortperm;  // order endog-exog-cons
    _jl: reghdfejl.b = collect(reghdfejl.b[`I']');
    _jl: reghdfejl.V = reghdfejl.V[`I',`I'];
    _jl: st_global("reghdfejl__coefnames", join(reghdfejl.coefnames[`I'], " "));
    jl GetMatFromMat `b', source(reghdfejl.b)
    jl GetMatFromMat `V', source(reghdfejl.V)
    mat colnames `b' = $reghdfejl__coefnames
    mat colnames `V' = $reghdfejl__coefnames
    mat rownames `V' = $reghdfejl__coefnames
    global reghdfejl__instdnames

    forvalues i=1/`:word count `coefnames'' {
      if `V'[`i',`i']==0 di as txt "note: `:word `i' of `coefnames'' omitted because of collinearity"
    }
    
    if 0`bs' & "`saving'"!="" {
      qui pwf
      local currentframe `r(currentframe)'
      tempname frame
      cap frame drop `frame'
      frame create `frame'
      cap noi {
        cwf `frame'
        qui set obs `reps'
        forvalues i=1/`=`k'' {
          local coefname: word `i' of $reghdfejl__coefnames
          local savvar = cond(strpos("`coefname'",".") | strpos("`coefname'","#"), "_bs_`i'", "_b_`coefname'")
          local savvars `savvars' `savvar'
          qui gen `double' `savvar' = .
          label var `savvar' "_b[`coefname']"
        }
        jl GetVarsFromMat `savvars', source(view(reghdfejl.bbs,:,`I')) replace
        label data "(bootstrap: reghdfejl)"
        save `saving', `replace'
      }
      cwf `currentframe'
      frame drop `frame'
      if _rc error _rc
    }
    _jl: reghdfejl.b = nothing
    global reghdfejl__coefnames
  }
  else local hascons 0

  ereturn post `b' `V', depname(`depname') obs(`=`N'') buildfvinfo findomitted `=cond(`sample', "esample(`touse')", "")'

  ereturn local wtype: copy local wtype
  ereturn local wexp: copy local wexp

  ereturn scalar N_hdfe = 0`N_hdfe'
  _jl: st_numscalar("`t'", reghdfejl.sizedf[1]);
  ereturn scalar N_full = `t'
  mata st_numscalar("e(rank)", rank(st_matrix("e(V)")))
  ereturn scalar df_m = e(rank)

  _jl: st_numscalar("`t'", m.iterations);
  ereturn scalar ic = `t'
  _jl: st_numscalar("`t'", m.converged);
  ereturn scalar converged = `t'
  _jl: st_numscalar("`t'", reghdfejl.sizedf[1] - nobs(m));
  ereturn scalar num_singletons = `t'

  if "`nl'"!="" {
    _jl: st_numscalar("`t'", m.loglikelihood);
    ereturn scalar ll = `t'
    _jl: st_numscalar("`t'", m.nullloglikelihood);
    ereturn scalar ll0 = `t'
    ereturn local family `family'
    ereturn local link `link'
  }
  else {
    _jl: st_numscalar("`t'", dof_fes(m));
    ereturn scalar df_a = `t'
    _jl: st_numscalar("`t'", dof_residual(m));
    ereturn scalar df_r = `t'
    _jl: st_numscalar("`t'", m.tss);
    ereturn scalar tss = `t'
    _jl: st_numscalar("`t'", m.rss / (1-m.r2_within));
    ereturn scalar tss_within = `t'
    _jl: st_numscalar("`t'", rss(m));
    ereturn scalar rss = `t'
    _jl: st_numscalar("`t'", mss(m));
    ereturn scalar mss = `t'
    _jl: st_numscalar("`t'", r2(m));
    ereturn scalar r2`' = `t'
    _jl: st_numscalar("`t'", adjr2(m));
    ereturn scalar r2_a = `t'
    _jl: st_numscalar("`t'", m.F);
    ereturn scalar F = `t'
    if `hasiv' {
      _jl: st_numscalar("`t'", m.F_kp);
      ereturn scalar widstat = `t'
    }
    scalar `used_df_r' = e(N) - e(df_a) - e(rank)
    ereturn scalar r2_a_within = 1 - (e(rss) / `used_df_r') / (e(tss_within) / (e(N) - e(df_a)))

    ereturn scalar rmse = sqrt(e(rss) / `used_df_r')
    ereturn scalar ll  = -e(N)/2*(1 + log(2*_pi / e(N) *  e(rss)          ))
    ereturn scalar ll0 = -e(N)/2*(1 + log(2*_pi / e(N) * (e(rss) + e(mss))))

    if 0`N_hdfe' {
      _jl: st_numscalar("`t'", m.r2_within);
      ereturn scalar r2_within = `t'
    }
  }

  if `haswt' {
    _jl: st_numscalar("`t'", sumweights);
    ereturn scalar sumweights = `t'
  }
  else ereturn scalar sumweights = e(N)

  if 0`bs' {
    ereturn local vce bootstrap
    ereturn local vcetype Bootstrap
    _jl: st_numscalar("`t'", reghdfejl.Nclust);
    ereturn scalar N_clust = `t'
    ereturn scalar N_clust1 = `t'
    if "`bscluster'"!="" {
      ereturn local cluster: copy local bscluster
      ereturn local clustvar1: copy local bscluster
      ereturn local title3 Statistics cluster-robust
    }
    else ereturn local title3 Statistics robust to heteroskedasticity
  }
  else if "`cluster'`robust'"=="" ereturn local vce ols
  else {
    ereturn local vcetype Robust
    if "`cluster'"=="" {
      ereturn local vce robust
      ereturn local title3 Statistics robust to heteroskedasticity
    }
    else {
      ereturn local vce cluster
      ereturn local clustvar: copy local cluster
      ereturn scalar N_clustervars = `:word count `cluster''
      tokenize `cluster'
      forvalues i=1/`e(N_clustervars)' {
        ereturn local clustvar`i': copy local `i'
        _jl: st_numscalar("`t'", m.nclusters[`i']);
        ereturn scalar N_clust`i' = `t'
      }
      _jl: st_numscalar("`t'", minimum(m.nclusters));
      ereturn scalar N_clust = `t'
      ereturn local title3 Statistics cluster-robust
    }
  }

  ereturn scalar drop_singletons = "`keepsingletons'"==""
  ereturn scalar report_constant = `hascons'
  ereturn local depvar: copy local depname
  ereturn local indepvars `inexogname' `instdname'
  ereturn local resid: copy local residuals

  if `hasiv' {
    ereturn local model iv
    ereturn local inexog: copy local inexogexpanded
    ereturn local instd: copy local instdexpanded
    ereturn local insts: copy local instsexpanded
  }
  else ereturn local model ols

  ereturn local title HDFE `=cond(`hasiv', "2SLS", cond("`nl'"=="","linear","nonlinear"))' regression with Julia
  if 0`N_hdfe' ereturn local title2 Absorbing `N_hdfe' HDFE `=plural(0`N_hdfe', "group")'
  ereturn local absvars: copy local absorb
  ereturn local marginsnotok Residuals SCore
  ereturn local predict reghdfejl_p
  ereturn local estat_cmd reghdfejl_estat
  ereturn local cmdline reghdfejl `cmdline'
  ereturn local flinejl: copy local flinejl
  ereturn local cmdlinejl: copy local cmdlinejl
  ereturn local cmd reghdfejl
//   ereturn local exposure `exposure'
//   ereturn local offset `offset'

  Display, `diopts' `eformopts' level(`level') `noheader' `notable'
end


// translate a pipe-delimited coefficient list back to Stata syntax, and replace temp vars with their names
cap program drop varlistJ2S
program define varlistJ2S, rclass
  version 15
  syntax, jlcoefnames(string) [vars(string) varnames(string)]
  gettoken jlcoef jlcoefnames: jlcoefnames, parse("|")
  while "`jlcoef'"!="" {
    if "`jlcoef'"=="(Intercept)" {
      return local hascons 1
      return local stcoefs `return(stcoefs)' _cons
    }
    else if "`jlcoef'"!="|" {
      tokenize `jlcoef', parse("&")
      local cdot = cond("`2'"!="", "c.", "")
      local stcoef
      while "`1'"!="" {
        if regexm(strtrim("`1'"), "^([^:&]*)$") {  // "[coef]"
          local stcoef `=cond("`stcoef'"=="","","`stcoef'#")'`:word `:list posof "`=regexs(1)'" in vars' of `varnames''
        }
        else if regexm(strtrim("`1'"), "^([^:&]*)(:(.*))$") {  // "[coef]: [x]"
          local stcoef `=cond("`stcoef'"=="","","`stcoef'#")'`=cond(regexs(3)!="","`=real(regexs(3))'.", "`cdot'")'`:word `:list posof "`=regexs(1)'" in vars' of `varnames''
        }
        macro shift
      }
      return local stcoefs `return(stcoefs)' `stcoef'
    }
    gettoken jlcoef jlcoefnames: jlcoefnames, parse("|")
  }
end

// cap program drop Display
program define Display
  version 15
  syntax [, Level(real `c(level)') noHEADer notable *]

  if !e(drop_singletons) di as err `"WARNING: Singleton observations not dropped; statistical significance is biased {browse "http://scorreia.com/reghdfe/nested_within_cluster.pdf":(link)}"'
  if e(num_singletons) di as txt `"(dropped `e(num_singletons)' {browse "http://scorreia.com/research/singletons.pdf":singleton observations})"'
  di as txt `"({browse "http://scorreia.com/research/hdfe.pdf":MWFE estimator} converged in `e(ic)' iterations)"' _n

  if "`header'"=="" {
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

  if "`e(vce)'"=="bootstrap" & "`e(cluster)'"!="" {
    local N_clust = strtrim(string(e(N_clust),"%10.0gc"))
    di _col(`=42-strlen("`N_clust'`e(cluster)'")') as txt "(Replications based on " as res "`N_clust'" as txt " clusters in " as res e(cluster) as txt ")"
  }

  if "`table'"=="" ereturn display, level(`level') `options'

  if e(model)=="iv" {
    local width `s(width)'
    local res = trim(string(e(widstat), "%10.3f"))
    di "Weak identification test (Kleibergen-Paap rk Wald F statistic):" _col(`=`width'-strlen("`res'")+1') as res `res'
    di as txt "{hline `width'}"
  }
end

cap program _julia_reghdfejl, plugin using(jl.plugin)  // create an extra handle to the plugin to reduce the chance that Stata unloads it


* Version history
* 0.3.0  Add support for absorbing string vars and clustering on interactions
* 0.3.1  Add compact option
* 0.3.2  Much better handling of interactions. Switched to BLISBLAS.jl.
* 0.3.3  Fixed bugs in handling of interactions and constant term
* 0.4.0  Added mask and unmask
* 0.4.1  Handle varlists with -/?/*/~
* 0.4.2  Set version minima for some packages
* 0.4.3  Add julia.ado version check. Fix bug in posting sample size. Prevent crash on insufficient observations
* 0.5.0  Add gpu & other options to partialhdfejl. Document the command. Create reghdfejl_load.ado
* 0.5.1  Fix dropping of some non-absorbed interaction terms. Handle noconstant when no a()
* 0.6.0  Added vce(bs)
* 0.6.1  Bug fixes. Added interruptible option.
* 0.6.2  Bug fixes. Add Kleibergen-Paap return value. Catch small option.
* 0.6.3  Bug fixes, including [pw] not triggering robust. Bump to julia.ado 0.10.0. Speed up handling of non-absorbed factor variables--don't fvrevar and then copy.
* 1.0.0  Support wildcards in absorb(). Added ivreg2 option.
* 1.0.1  Add vce(bs, saving()) suboption. Made rng seeds more deterministic. Refined the bootstrap code. Fix crash in varlistJ2S.
* 1.0.2  Bug fix for 1.0.1 bug fix.
* 1.0.3  Fix crashes with 100s of non-absorbed regressors
* 1.0.4  Fix crash in Stata<18 from using {n} in regexm()
* 1.0.5  Redo translation of fv vars from Stata to Julia
* 1.0.6  Fix crash on vce(bs) with non-absorbed factor vars
* 1.0.7  Fix crashes on i.x when x is constant in sample
* 1.0.8  Make compatible with Julia 1.11
* 1.0.9  Make sure to unab all variables before PutVarsToDF in order to catch all duplicates
* 1.0.10 Clean up sum-of-squares return values and their documentation
* 1.0.11 Make ado crash if estimation fails. Fix crash on absorb(v1##c.(v2 v3)).
* 1.1.1  Change default residuals filename to _reghdfejl_resid[N] where N chosen by program to create unique var name
*        Switch bs from Distributed to Threads & guarantee reproducible order of simulations with bs(, saving)
*        Move parsing of absorb() into reghdfejl_parse_absorb, to share with partialhdfejl
* 1.1.2  Fix 1.1.1 crash when absorbing string vars
* 1.1.3  Fix 1.1.1 crash on savefe
* 1.1.4  Add reference to jl.plugin to reduce chance Stata unloads it and causes crash
* 1.1.5  Fix stupid partialhdfejl crash
* 1.1.6  Change default tolerance() from 1e-6 to 1e-8. Tidy up display of KP F stat.
* 1.1.7  BLISBLAS 0.2.0 causing crash, so switch to MKL/OpenBLAS