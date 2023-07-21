cap program drop reghdfejl
program define reghdfejl, eclass
  version 16

  if replay() {
		if "`e(cmd)'" != "reghdfejl" error 301
		if _by() error 190
		Display `0'
		exit 0
	}

	syntax anything [if] [in] [aw pw iw/], [Absorb(string) Robust VCE(string) CLuster(string) RESiduals(string) gpu PRECision(integer 64) noConstant NOAbsorb *]

  _assert inlist(`precision',32,64), msg("{cmdab:prec:ision}() option must be 32 or 64.")

  _get_diopts diopts options, `options'

  if "`gpu'"!="" local methodopt , method = :gpu

  if `"`options'"' != "" di as inp "`options'" as txt " ignored" _n

  local hascons = `"`constant'`absorb'"'==""

  local cmdline `0'

  if !0$reghdfejl_julia_loaded {
    cap python: 1
    if _rc {
      di as err "The {cmd:julia} option requires that Python be installed and Stata be configured to use it."
      di as err `"See {browse "https://blog.stata.com/2020/08/18/stata-python-integration-part-1-setting-up-stata-to-use-python":instructions}."'
      exit 198
    }

    qui python query
    if "`r(version)'" < "3" {
      di as err "Stata is currently configured to use Python " `r(version)' ". The {cmd:julia} option requires Python 3."
      di as err `"See {help python}."'
      exit 198
    }

    local pipline = "!py" +  cond(c(os)=="Windows","","thon"+substr("`r(version)'",1,1)) + " -m pip install --user"  // https://packaging.python.org/en/latest/tutorials/installing-packages/#use-pip-for-installing
    python: from sfi import Data, Matrix, Scalar, Macro

    cap python: import numpy as np
    if _rc {
      di "Installing NumPy..."
      `pipline' numpy
      cap python: import numpy as np
      if _rc {
        di as err _n "The {cmd:julia} option requires the Python package NumPy. Unable to install it automatically, or to find it if installed.."
        di as err _n "If it just installed successfully, try restarting Stata."
        di as err `"You can install it {browse "https://numpy.org/install":manually}."'
        exit 198
      }
    }
    
    di as txt "Invoking the Julia implementation. The first call in each Stata session is slow."
    mata displayflush()

    cap python: import juliacall

    if _rc {
      di "Installing JuliaCall..."
      `pipline' julia
      cap python: import juliacall
    }
    if _rc {
      di as err _n "The {cmd:julia} option requires the Python package JuliaCall. Unable to install it automatically."
      di as err `"You can install it {browse "https://cjdoris.github.io/PythonCall.jl/stable/juliacall/#Installation":manually}."'
      di as err _n "If it just installed successfully, try restarting Stata."
      exit 198
    }

    local pyline import juliacall; jl = juliacall.newmodule("reghdfejl"); from juliacall import Pkg
    cap python: `pyline'
    if _rc {
      di as err _n "Could not automatically initialize the JuliaCall package."
      exit 198
    }

    qui python: Scalar.setValue("rc", jl.seval('VERSION < v"1.9.0"')) 
    if 0`rc' {
      di as err _n "The {cmd:julia} option requires that Julia 1.9 or higher be installed and accessible by default through the system path."
      di as err `"Follow {browse "https://julialang.org/downloads/platform":these instructions} for installing it and adding it to the system path."'
      exit 198
    }

    foreach pkg in CUDA FixedEffectModels DataFrames Vcov {
      qui python: jl.seval('using Pkg; p=[v for v in values(Pkg.dependencies()) if v.name=="`pkg'"]')
      python: Macro.setLocal("rc", str(jl.seval('length(p)')))
      if `rc'==0 {
        di "Installing `pkg'.jl..."
        cap python: Pkg.add("`pkg'")
        if _rc {
          di as err _n "Failed to automatically install the Julia package `pkg'.jl."
          di as err `"You should be able to install it by running Julia and typing {cmd:using Pkg; Pkg.add("`pkg'")}."'
          exit 198
        }
      }
      python: jl.seval("using `pkg'")
    }

    global reghdfejl_julia_loaded 1
  }

  local _anything `anything'  // save results likely about to be overwritten by next syntax command
  local wtvar `exp'

  if `"`absorb'"' != "" {
    local hasfe 1

    local 0 `absorb'
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
      local fearg `fearg' `1'
      macro shift
    }
    local absorb `fearg'

    local N_hdfe: word count `fearg'
    local fearg: subinstr local fearg " " " i.", all
    fvunab fearg: i.`fearg'
    local absorbvars `fearg'
    local fearg: subinstr local fearg "##c." ")*(", all
    local fearg: subinstr local fearg "#c." ")&(", all
    local fearg: subinstr local fearg "##i." ")&fe(", all
    local fearg: subinstr local fearg "#i." ")&fe(", all
    local fearg: subinstr local fearg "i." "fe(", all
    local fearg: subinstr local fearg " " ") + ", all
    local fearg + `fearg')

    local absorbvars: subinstr local absorbvars "i." " ", all
    local absorbvars: subinstr local absorbvars "c." " ", all
    local absorbvars: subinstr local absorbvars "#" " ", all
  }
  else if !0`hascons' local fearg + 0

  tokenize `_anything'
  _fv_check_depvar `1'
  local depname `1'
  macro shift

  local hasiv = strpos(`"`*'"', "=")
  if `hasiv' {
    local t = regexm(`"`*'"', "^([^\(]*)\(([^=]*)=([^\)]*)\)(.*)$")
    fvunab inexogname: `=regexs(1)' `=regexs(4)'
    fvunab instdname: `= regexs(2)'
    fvunab instsname: `=regexs(3)'
  }
  else local inexogname `*'

  local varlist `depvarname' `instdname' `inexogname' `instsname'
  marksample touse
  markout `touse' `absorbvars'

  if `"`cluster'"' != "" local vce cluster `cluster'  // move clustering vars in cluster() to vce() because _vce_parse can only handle >1 var in latter
  _vce_parse, optlist(Robust) argoptlist(CLuster) pwallowed(robust) old: `wgtexp', `robust' vce(`vce')
	local vce `r(vceopt)'
	local robust `r(robust)'
	local cluster `r(cluster)'
	markout `touse' `cluster', strok

  if "`cluster'"=="" {
    if "`robust'"!="" {
      local vcovarg , Vcov.robust()
    }
  }
  else {
    mata st_local("vcovarg", invtokens(":":+tokens("`cluster'"),","))
    local vcovarg , Vcov.cluster(`vcovarg')
  }

  foreach varset in dep inexog instd insts {
    fvrevar ``varset'name' if `touse'
    local `varset' `r(varlist)'
    local k`varset': word count ``varset''
  }

  if `"`wtvar'"' != "" {
    cap confirm var `wtvar'
    if _rc {
      tempname wtvar
      gen double `wtvar' = `exp' if `touse'
    }
    local wtarg , weights = :`wtvar'
  }

  local vars `dep' `inexog' `instd' `insts' `cluster' `wtvar' `absorbvars'
  local vars: list uniq vars
  
  local savearg = cond("`residuals'`savefe'`namedfe'"=="", "", ", save = :" + cond("`residuals'"=="", "fe", cond("`savefe'`namedfe'"=="", "residuals", "all")))

  qui foreach var in `vars' {
    python: jl._`var' = np.asarray(Data.get("`var'", selectvar="`touse'", missingval=jl.missing))  # prepend with "_" to avoid conflicts with reserved words
  }
  qui python: jl.seval('df = DataFrame(Dict(s=>eval(Meta.parse("_"*s)) for s in split("`vars'"))); 0')

  mata: st_local("inexog", invtokens(tokens("`inexog'"), "+"))  // put +'s in var lists
  if `hasiv' {
    mata: st_local("instd", invtokens(tokens("`instd'"), "+"))
    mata: st_local("insts", invtokens(tokens("`insts'"), "+"))
    local ivarg + (`instd' ~ `insts')
  }

  * Estimate!
  qui python: jl.seval('m = reg(df, @formula(`dep' ~ `inexog' `ivarg' `fearg') `wtarg' `vcovarg' `methodopt' `savearg', double_precision=`precision'==64); 0')

  tempname b V N
  if "`savefe'`namedfe'" != "" {
    qui python: jl.seval('FEs = fe(m); 0')
    forvalues a = 1/`N_hdfe' {
      if "`savefe'`fename`a''"!="" {
        if "`fename`a''"=="" local fename`a' __hdfe`a'__
        gen double `fename`a'' = .
        python: Data.store("`fename`a''", None, jl.seval('FE=FEs[!,`a']; replace!(FE, missing=>NaN)'), "`touse'")
        label var `fename`a'' "`:word `a' of `absorb''"
      }
    }
  }

  if "`residuals'"!="" {
    gen `residuals' = .
    qui python: jl.seval('res = residuals(m); replace!(res, missing=>NaN); 0')
    python: Data.store("`residuals'", None, jl.res, "`touse'")
  }

  python: Scalar.setValue("`N'", jl.seval('nobs(m)'))
  python: Data.store("`touse'", None, jl.seval('m.esample'), "`touse'")
  qui python: jl.seval('b = coef(m); 0')
  qui python: jl.seval('I = [1+`kinexog'+`hascons':length(b) ; 1+`hascons':`kinexog'+`hascons' ; 1:`hascons']; 0')  # cons-exog-endog -> endog-exog-cons
  python: Matrix.store("`b'", np.asarray(jl.seval(" b[I]' ")))
  python: Matrix.store("`V'", np.asarray(jl.seval('replace!(vcov(m)[I,I], NaN=>0.)')))
  local coefnames `instdname' `inexogname' `=cond(`hascons', "_cons", "")'

  mat colnames `b' = `coefnames'
  mat colnames `V' = `coefnames'
  mat rownames `V' = `coefnames'
  
  forvalues i=1/`=`kinexog'+`kinstd'' {
    if `V'[`i',`i']==0 di as txt "note: `:word `i' of `coefnames'' omitted because of collinearity"
  }

  ereturn post `b' `V', depname(`depname') obs(`=`N'') esample(`touse') buildfvinfo findomitted

  ereturn scalar N_hdfe = 0`N_hdfe'
  python: Scalar.setValue("e(N_full)", jl.seval('size(df,1)'))
  
  mata st_numscalar("e(rank)", rank(st_matrix("e(V)")))
  ereturn scalar df_m = e(rank)
  python: Scalar.setValue("e(df_r)", jl.seval('dof_residual(m)'))
  python: Scalar.setValue("e(rss)", jl.seval('rss(m)'))
  python: Scalar.setValue("e(mss)", jl.seval('mss(m)'))
  python: Scalar.setValue("e(r2)", jl.seval('r2(m)'))
  python: Scalar.setValue("e(r2_a)", jl.seval('adjr2(m)'))
  python: Scalar.setValue("e(F)", jl.seval('m.F'))
  python: Scalar.setValue("e(ic)", jl.seval('m.iterations'))
  python: Scalar.setValue("e(converged)", jl.seval('m.converged'))
  python: Scalar.setValue("e(num_singletons)", jl.seval('size(df,1) - nobs(m)'))
  scalar rmse = e(rss) / (e(N) - e(df_a) - e(rank))
  scalar ll  = -e(N)/2*(1 + log(2*_pi / e(N) *  e(rss)          ))
  scalar ll0 = -e(N)/2*(1 + log(2*_pi / e(N) * (e(rss) + e(mss))))
  if 0`hasfe' python: Scalar.setValue("e(r2_within)", jl.seval('m.r2_within'))
  if "`wtvar'"=="" ereturn scalar sumweights = e(N)
    else python: Scalar.setValue("e(sumweights)", jl.seval('mapreduce((w,s)->(s ? w : 0), +, _`wtvar', m.esample; init = 0)'))

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
        python: Scalar.setValue("e(N_clust`i')", jl.seval('m.nclusters[`i']'))
      }
      python: Scalar.setValue("e(N_clust)", jl.seval('minimum(m.nclusters)'))
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
    local line`i' as txt "Number of clusters(" as res e(clustvar`i') ")" _col(30) "= " as res %10.0f e(N_clust`i')
  }
  di `line1' _col(51) as txt "Within R-sq." _col(67) "= " as res %10.4f e(r2_within)
  di `line2' _col(51) as txt "Root MSE"     _col(67) "= " as res %10.4f e(rmse)
  forvalues i=3/0`e(N_clustervars)' {
    di line`i'
  }
  di
  ereturn display, level(`level') `diopts'
end
