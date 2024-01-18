*! reghdfejl 0.6.0 20 December 2023

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


* Version history at bottom

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

	syntax anything [if] [in] [aw pw iw/], [Absorb(string) Robust CLuster(string) vce(string) RESIDuals ITerations(integer 16000) gpu THReads(integer 0) ///
                                          noSAMPle TOLerance(real 1e-8) Level(real `c(level)') NOHEADer NOTABLE compact VERBose noCONStant KEEPSINgletons *]
  local sample = "`sample'"==""

  _assert `iterations'>0, msg("{cmdab:It:erations()} must be positive.") rc(198)
  _assert `tolerance'>0, msg("{cmdab:tol:erance()} must be positive.") rc(198)

  _get_diopts diopts _options, `options'

  marksample touse
  
  local gpulib = cond(c(os)=="MacOSX", "Metal", "CUDA")
  if "`gpu'"!="" local methodopt , method = :`gpulib'

  if `threads' local threadsopt , nthreads = `threads'
  if "`keepsingletons'"!="" local singletonopt , drop_singletons = false
  
  reghdfejl_load

  if `"`exp'"' != "" {
    cap confirm var `exp'
    if _rc {
      tempname wtvar
      gen double `wtvar' = `exp' if `touse'
    }
    else local wtvar `exp'
    local wtopt , weights = :`wtvar'
    local haspw = "`weight'"=="pweight"
  }

  fvunab anything: `anything'
  tokenize `anything'
  local depname `1'
  macro shift

  local hasiv = strpos(`"`*'"', "=")
  if `hasiv' {
    local t = regexm(`"`*'"', "^([^\(]*)\(([^=]*)=([^\)]*)\)(.*)$")  // standard IV syntax
    local inexogname `=regexs(1)' `=regexs(4)'
    local instdname = regexs(2)
    local instsname = regexs(3)
  }
  else local inexogname `*'

  markout `touse' `depname' `instdname' `inexogname' `instsname'

  if `"`vce'"' != "" {
    _assert `"`cluster'"'=="", msg("only one of cluster() and vce() can be specified") rc(198)
    _assert `"`robust'"' =="", msg("only one of robust and vce() can be specified"   ) rc(198)
    tokenize `"`vce'"', parse(" ,")
    local 0, `1'
    syntax, [Robust CLuster UNadjusted ols bs BOOTstrap]
    _assert "`robust'`cluster'`unadjusted'`ols'`bs'`bootstrap'"!="", msg("vcetype '`0'' not allowed") rc(198)
    local bs = "`bs'`bootstrap'" != ""
    macro shift
    if `bs' {
      local 0 `*'
      syntax, [CLuster(string) Reps(integer 50) mse seed(string) SIze(integer 0) PROCs(integer 1)]
      _assert `reps'>1, msg("reps() must be an integer greater than 1") rc(198)
      _assert `size'>=0, msg("size() must be a positive integer") rc(198)
      _assert `procs'>=0, msg("procs() must be a positive integer") rc(198)
      if `procs'==0 local procs 1

      cap confirm numeric var `cluster'
      if _rc {
        tempvar t
        qui egen long `t' = group(`cluster')
        local bslcuster `t'
      }
      else local bscluster `cluster'

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

  foreach varset in dep inexog instd insts {
    if strpos("``varset'name'", ".") | strpos("``varset'name'", "#") | strpos("``varset'name'", "-") | strpos("``varset'name'", "?") | strpos("``varset'name'", "*") | strpos("``varset'name'", "~") {
      fvexpand ``varset'name' if `touse'
      local `varset'
      local `varset'name
      foreach var in `r(varlist)' {
        _ms_parse_parts `var'
        if !r(omit) {
          local `varset'name ``varset'name' `var'
          tempvar t
          qui gen double `t' = `var' if `touse'
          local `varset' ``varset'' `t'
        }
      }
    }
    else local `varset' ``varset'name'
    local k`varset': word count ``varset''
  }
  _assert `kdep'==1, msg("Multiple dependent variables specified.") rc(198) 

  if `"`absorb'"' != "" {
    ExpandAbsorb `absorb'
    local 0 `r(exp)'
    syntax anything(equalok), [SAVEfe]
    tokenize `anything', parse(" =")

    while `"`1'"' != "" {
      if `"`2'"' == "=" {
        confirm new var `1'
        local fenames = `"`fenames'"' + " `1'"
        macro shift 2
        local namedfe 1
      }
      else local fenames = `"`fenames'"' + `" "" "'
      local feterms `feterms' `1'
      macro shift
    }
    local absorb `feterms'
    local N_hdfe: word count `feterms'

    local feterms i.`: subinstr local feterms " " " i.", all'

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
        qui egen long `t' = group(`var') if `touse'
        local absorbvars: subinstr local absorbvars "`var'" "`t'", word all
        local feterms   : subinstr local feterms    "`var'" "`t'", word all
      }
    }
    
    markout `touse' `absorbvars'
  }
  else if "`constant'"!="" local feterms + 0

  local vars `dep' `inexog' `instd' `insts' `_cluster' `wtvar' `absorbvars' `bscluster'
  local vars: list uniq vars

  if "`residuals'" != "" {
    cap drop _reghdfejl_resid
    local residuals _reghdfejl_resid
  }
  else {
    local 0, `_options'
    syntax, [RESIDuals(name) *]
    local _options `options'
  }

  if `"`_options'"' != "" di as inp `"`_options'"' as txt " ignored" _n

  local saveopt = cond("`residuals'`savefe'`namedfe'"=="", "", ", save = :" + cond("`residuals'"=="", "fe", cond("`savefe'`namedfe'"=="", "residuals", "all")))

  if "`compact'"!="" {
    tempfile compactfile
    save "`compactfile'"
    keep `vars' `touse'
    qui keep if `touse'
  }

  jl PutVarsToDFNoMissing `vars' if `touse'  // put all vars in Julia DataFrame named df

  if "`verbose'"!="" jl: df

  qui jl: size(df,1)
  _assert `r(ans)', rc(2001) msg(insufficient observations)

  if "`compact'" !="" drop _all

  mata: st_local("inexog", invtokens(tokens("`inexog'"), "+"))  // put +'s in var lists
  if `hasiv' {
    mata: st_local("instd", invtokens(tokens("`instd'"), "+"))
    mata: st_local("insts", invtokens(tokens("`insts'"), "+"))
    local ivarg + (`instd' ~ `insts')
  }

  * Estimate!
  jl, qui: f = @formula(`dep' ~ `inexog' `ivarg' `feterms')
  if "`verbose'"!="" {
    di "`reg(df, @formula(`dep' ~ `inexog' `ivarg' `feterms') `wtopt' `vcovopt' `methodopt' `threadsopt' `singletonopt' `saveopt', tol=`tolerance', maxiter=`iterations')'"
    jl: m = reg(df, f `wtopt' `vcovopt' `methodopt' `threadsopt' `singletonopt' `saveopt', tol=`tolerance', maxiter=`iterations')
  }
  else jl, qui: m = reg(df, f `wtopt' `vcovopt' `methodopt' `threadsopt' `singletonopt' `saveopt', tol=`tolerance', maxiter=`iterations')

  tempname k
  jl, qui: k = length(coef(m)); SF_scal_save("`k'", k)
  jl, qui: sizedf = size(df)
  if "`wtvar'"!="" jl, qui: sumweights = mapreduce((w,s)->(s ? w : 0), +, df.`wtvar', m.esample; init = 0)

  if `k' {
    tempname b V

    * bootstrap
    if 0`bs' {
      local hasclust = "`bscluster'"!=""
      tempname bswt

      qui jl: nworkers()
      if `procs' != `r(ans)' {
        jl, qui: rmprocs(procs())
        if `procs'>1 jl, qui:  addprocs(`procs', exeflags="-t1");  /* single-threaded workers */                                                                  ///
                               @everywhere using `=cond(c(os)=="MacOSX", "Metal, AppleAccelerate", "CUDA, BLISBLAS")', StableRNGs, DataFrames, FixedEffectModels, SharedArrays
      }

      jl, qui: rngs = [StableRNG(`=runiformint(0, 1e6)' * i + 42) for i in 1:maximum(workers())]  // different, deterministic seeds for each worker

      if `hasclust' ///
        jl, qui: s = Set(df.`bscluster');                                                                               ///
                 Nclust = length(s);                                                                                    ///
                 _id = SharedVector(getindex.(Ref(Dict(zip(s, 1:Nclust))), df.`bscluster')) /* ordinalize cluster id */ 
      else                                                                                                              ///
        jl, qui: Nclust = size(df,1);                                                                                   ///
                 _id = Colon()
       
      jl, qui: bssize = `=cond(0`size',"`size'","Nclust")';                                                             ///
               bsweights = Vector{Int}(undef, Nclust);                                                                  ///
               @everywhere function reghdfejlbs(bsweights, bssize, rngs, Nclust, df, _id, f)                            ///
                 fill!(bsweights, 0);                                                                                   ///
                 rng = rngs[myid()];                                                                                    ///
                 @inbounds for i in 1:bssize  /* bs draws */                                                            ///
                   bsweights[rand(rng, 1:Nclust)] += 1                                                                  ///
                 end;                                                                                                   ///
                 df.`bswt' = bsweights[_id];                                                                            ///
                 `=cond("`wtopt'"!="", "df.`bswt' .*= df.`wtvar';", "")'                                                ///
                 b = coef(reg(df, f, weights=:`bswt' `methodopt' `threadsopt', tol=`tolerance', maxiter=`iterations')); ///
                 [b, b*b']                                                                                              ///
               end;                                                                                                     ///
               retval = @distributed (+) for m in 1:`reps'                                                              ///
                 reghdfejlbs(bsweights, bssize, rngs, Nclust, df, _id, f)                                               ///
               end;                                                                                                     ///
               Vbs = retval[2];                                                                                         ///
               Vbs .-= retval[1] ./ `reps' .* retval[1]';                                                               ///
               Vbs ./= `reps' - `="`mse'"==""'
    }
  }

  if "`verbose'"=="" jl, qui: df = nothing  // yield memory
  if "`compact'"!="" {
    jl, qui: GC.gc()
    use `compactfile'
  }

  if "`savefe'`namedfe'" != "" {
    jl, qui: FEs = fe(m); rename!(FEs, "FE" .* string.(1:`N_hdfe'))
    forvalues a = 1/`N_hdfe' {
      local fename: word `a' of `fenames'
      if "`savefe'`fename'"!="" {
        if "`fename"=="" local fename __hdfe`a'__
        jl GetVarsFromDF `fename' if `touse', source(FEs) col(FE`a')
        label var `fename' "[FE] `:word `a' of `absorb''"
      }
    }
    jl, qui: FEs = nothing
  }

  if "`residuals'"!="" {
    jl, qui: res = residuals(m); replace!(res, missing=>NaN)
    jl GetVarsFromMat `residuals' if `touse', source(res) `replace'
    label var `residuals' "Residuals"
    jl, qui: res = nothing
  }

  tempname t N

  jl, qui: SF_scal_save("`N'", nobs(m))

  if `sample' {
    jl, qui: esample = Vector{Float64}(m.esample)
    jl GetVarsFromMat `touse' if `touse', source(esample) replace
    jl, qui: esample = nothing
  }

  if `k' {
    jl, qui: SF_scal_save("`t'", coefnames(m)[1]=="(Intercept)")
    local hascons = `t'

    jl, qui: I = [1+`kinexog'+`hascons':`kinexog'+`hascons'+`kinstd' ; 1+`hascons':`kinexog'+`hascons' ; 1:`hascons']  // cons-exog-endog -> endog-exog-cons
    jl, qui: `b' = collect(coef(m)[I]')
    jl, qui: `V' = iszero(0`bs') ? vcov(m) : Vbs
    jl, qui: `V' = replace!(`V'[I,I], NaN=>0.)
    jl GetMatFromMat `b'
    jl GetMatFromMat `V'
    local coefnames `instdname' `inexogname' `=cond(`hascons', "_cons", "")'

    mat colnames `b' = `coefnames'
    mat colnames `V' = `coefnames'
    mat rownames `V' = `coefnames'
   
    forvalues i=1/`:word count `coefnames'' {
      if `V'[`i',`i']==0 di as txt "note: `:word `i' of `coefnames'' omitted because of collinearity"
    }
  }
  else local hascons = 0

  ereturn post `b' `V', depname(`depname') obs(`=`N'') buildfvinfo findomitted `=cond(`sample', "esample(`touse')", "")'

  ereturn scalar N_hdfe = 0`N_hdfe'
  jl, qui: SF_scal_save("`t'", sizedf[1])
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
  jl, qui: SF_scal_save("`t'", sizedf[1] - nobs(m))
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
    jl, qui: SF_scal_save("`t'", sumweights)
    ereturn scalar sumweights = `t'
  }

  if 0`bs' {
    ereturn local vce bootstrap
    ereturn local vcetype Bootstrap
    jl, qui: SF_scal_save("`t'", Nclust)
    ereturn scalar N_clust = `t'
    ereturn scalar N_clust1 = `t'
    if "`bscluster'"!="" {
      ereturn local cluster `bscluster'
      ereturn local clustvar1 `bscluster'
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

  ereturn scalar drop_singletons = "`keepsingletons'"==""
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
  ereturn local cmdline reghdfejl `cmdline'
  ereturn local cmd reghdfejl

  Display, `diopts' level(`level') `noheader' `notable'
end

* Expand nested expression like a#c.(b c) without using fvunab, which apparently scans all vars for their levels, taking time
program define ExpandAbsorb, rclass
  while `"`0'"' != "" {
    gettoken car 0: 0, bind
    if regexm("`car'", "([^\(]*)\((.*)\)([^\)]*)") {
      local prefix = regexcapture(1)
      local suffix = regexcapture(3)
      Parse `=regexcapture(2)'
      mata st_local("car", invtokens("`prefix'" :+ tokens("`r(exp)'") :+ "`suffix'"))
    }
    local exp `exp' `car'
  }
  return local exp `exp'
end

program define Display
  version 16
  syntax [, Level(real `c(level)') noHEADer notable *]
  _get_diopts diopts, `options'

  if !e(drop_singletons) di as err `"WARNING: Singleton observations not dropped; statistical significance is biased {browse "http://scorreia.com/reghdfe/nested_within_cluster.pdf":(link)}"'
  if e(num_singletons) di as txt `"(dropped `e(num_singletons)' {browse "http://scorreia.com/research/singletons.pdf":singleton observations})"'
  di as txt `"({browse "http://scorreia.com/research/hdfe.pdf":MWFE estimator} converged in `e(ic)' iterations)"'
  di

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
    di _col(`=50-strlen("`N_clust'`e(cluster)'")') as txt "(Replications based on " as res "`N_clust'" as txt " clusters in " as res e(cluster) as txt ")"
  }

  if "`table'"=="" ereturn display, level(`level') `diopts'
end


* Version history
* 0.3.0 Added support for absorbing string vars and clustering on interactions
* 0.3.1 Added compact option
* 0.3.2 Much better handling of interactions. Switched to BLISBLAS.jl.
* 0.3.3 Fixed bugs in handling of interactions and constant term
* 0.4.0 Added mask and unmask
* 0.4.1 Handle varlists with -/?/*/~
* 0.4.2 Set version minima for some packages
* 0.4.3 Add julia.ado version check. Fix bug in posting sample size. Prevent crash on insufficient observations.
* 0.5.0 Add gpu & other options to partialhdfejl. Document the command. Create reghdfejl_load.ado.
* 0.5.1 Fix dropping of some non-absorbed interaction terms. Handle noconstant when no a().
* 0.6.0 Added vce(bs).