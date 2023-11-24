// The MIT License (MIT)
//
// Copyright (c) 2014 Sergio Correia
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

* Copied from reghdfe 6.12.3 with almost no change

program reghdfejl_estat, rclass
	version `=cond(c(version)<14, c(version), 13)'
	if !inlist("`e(cmd)'", "reghdfe", "reghdfejl", "ppmlhdfe") {
		error 301
	}
	
	gettoken key 0 : 0, parse(", ")
	local lkey = length(`"`key'"')

	if `"`key'"' == substr("summarize",1,max(2,`lkey')) {

		local 0 `rest'
		syntax [anything] , [*] [noheader] // -noheader- gets silently ignored b/c it will always be -on-

		**if ("`anything'"=="") {
		**	* By default include the instruments
		**	local anything // `e(depvar)' `e(indepvars)' `e(endogvars)' `e(instruments)'
		**}

		* Need to use -noheader- as a workaround to the bug in -estat_summ-
		estat_summ `anything' , `options' noheader

	}
	else if `"`key'"' == "vce" {
		vce `0'
	}
	else if `"`key'"' == "ic" {
		syntax, [*]
		estat_default ic, df(`=e(df_m)+1') `options'
	}
	else {
		di as error `"invalid subcommand `key'"'
		exit 321
	}
	return add // ?
end
