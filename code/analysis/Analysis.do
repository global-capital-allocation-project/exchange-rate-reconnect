cap mkdir "$graphs/"
cap mkdir "$graphs/appendix"
cap mkdir "$regs/"
cap mkdir "$regs/unformatted"

***************************************************************************
******************* MAIN PAPER ********************************************
***************************************************************************

*************************
*** 1. FIGURE : Disconnect / reconnect multipanel graphs and two regression line. U.S. Foreign bond purchases plot, with crisis period in red
*************************

use "$user_dir/data/output/allmerged.dta", clear

* Figure 1 subplots

twoway (scatter d_e_eq_wgt L.i_diff, color(black))  (lfit d_e_eq_wgt L.i_diff, lcolor(black) lpattern(dash))  if depvar=="USD" & date_q < $startq & date_q >= tq(1977q1),  graphregion(color(white)) xtitle("Interest Rate Differential") ytitle("Change in Broad Dollar") legend(off)
graph export "$graphs/erd_i_pre.eps", replace
twoway (scatter d_e_eq_wgt pi_q_diff, color(black)) (lfit d_e_eq_wgt pi_q_diff, lcolor(black) lpattern(dash)) if depvar=="USD" & date_q < $startq & date_q >= tq(1977q1), graphregion(color(white)) xtitle("Inflation Differential") ytitle("Change in Broad Dollar") legend(off)
graph export "$graphs/erd_pi_pre.eps", replace
twoway (scatter d_e_eq_wgt d_gz_spread, color(black)) (lfit d_e_eq_wgt d_gz_spread, lcolor(black) lpattern(dash)) if depvar=="USD" & date_q < $startq & date_q >= tq(1977q1), graphregion(color(white)) xtitle("Change in U.S. Corporate Bond Spread") ytitle("Change in Broad Dollar") legend(off)
graph export "$graphs/erd_gz_pre.eps", replace
twoway (scatter d_e_eq_wgt f_B_x_Om_i_ni, color(black)) (lfit d_e_eq_wgt f_B_x_Om_i_ni, lcolor(black) lpattern(dash)) if depvar=="USD" & date_q < $startq & date_q >= tq(1977q1), graphregion(color(white)) xtitle("U.S. Foreign Bond Purchases") ytitle("Change in Broad Dollar") legend(off)
graph export "$graphs/erd_bopcf_pre.eps", replace

reg d_e_eq_wgt L.i_diff if depvar=="USD" & date_q < $startq & date_q >= tq(1977q1)
reg d_e_eq_wgt pi_q_diff if depvar=="USD" & date_q < $startq & date_q >= tq(1977q1)
reg d_e_eq_wgt d_gz_spread if depvar=="USD" & date_q < $startq & date_q >= tq(1977q1)
reg d_e_eq_wgt f_B_x_Om_i_ni if depvar=="USD" & date_q < $startq & date_q >= tq(1977q1)

* Create 1Q to 4Q horizon variables for all lowercase f regressors, all LHS ex rate changes
ren f_* f_*_1Q
ren (i_diff pi_q_diff dy_q_diff d_gz_spread) (i_diff_1Q pi_q_diff_1Q dy_q_diff_1Q d_gz_spread_1Q)

ren d_e* d_e*_1Q 
foreach var of varlist f_* d_e* i_diff_1Q pi_q_diff_1Q dy_q_diff_1Q d_gz_spread_1Q {
	foreach i of num 1/3 {
		local next_lag = `i'+1
		local _var = substr("`var'", 1, strrpos("`var'", "_")) + "`i'Q"
		local next_var = substr("`var'", 1, strrpos("`var'", "_")) + "`next_lag'Q"
		local last_var = substr("`var'", 1, strrpos("`var'", "_")) + "`i'Q"
		gen `next_var' = `last_var' + L`i'.`var'
	}
}
order d_e* f_* i_diff_1Q pi_q_diff_1Q dy_q_diff_1Q d_gz_spread_1Q

* Figure 3

twoway (scatter d_e_eq_wgt_1Q f_B_x_Om_i_ni_1Q if date_q >$crisisend, msize(1) mcolor(black) mlabel(date_q) mlabcolor(black) mlabsize(2.5)) (lfit d_e_eq_wgt_1Q f_B_x_Om_i_ni_1Q, lcolor(black)) ///
	(lfit d_e_eq_wgt_1Q f_B_x_Om_i_ni_1Q if date_q >$crisisend, lcolor(red) lpattern(dash) range(-.068, .08)) (scatter d_e_eq_wgt_1Q f_B_x_Om_i_ni_1Q if date_q <=$crisisend, msize(1) msymbol(square) mcolor(red) mlabel(date_q) mlabcolor(red) mlabsize(2.5)) ///
	if depvar=="USD" & date_q>=$startq, ///
	xtitle("U.S. Foreign Bond Purchases") ytitle("Change in Broad Dollar") xlabel(-.15(.05).15) graphregion(color(white)) ///
	legend(order(2 "2007:Q1 - 2019:Q2" 3 "Ex-crisis: 2009:Q3 - 2019:Q2"))
graph export "$graphs/bd_flow_1Q.eps", replace
twoway (scatter d_e_eq_wgt_4Q f_B_x_Om_i_ni_4Q if date_q >$crisisend+3, msize(1) mcolor(black) mlabel(date_q) mlabcolor(black) mlabsize(2.5)) (lfit d_e_eq_wgt_4Q f_B_x_Om_i_ni_4Q, lcolor(black)) ///
	(lfit d_e_eq_wgt_4Q f_B_x_Om_i_ni_4Q if date_q >$crisisend+3, lcolor(red) lpattern(dash) range(-.135, .25)) (scatter d_e_eq_wgt_4Q f_B_x_Om_i_ni_4Q if date_q <=$crisisend+3, msize(1) msymbol(square) mcolor(red) mlabel(date_q) mlabcolor(red) mlabsize(2.5)) if depvar=="USD" & date_q>=$startq, ///
	xtitle("U.S. Foreign Bond Purchases") ytitle("Change in Broad Dollar") xlabel(-.15(.05).25) graphregion(color(white)) ///
	legend(order(2 "2007:Q1 - 2019:Q2" 3 "Ex-crisis: 2009:Q3 - 2019:Q2"))
graph export "$graphs/appendix/bd_flow_4Q.eps", replace

reg d_e_eq_wgt_1Q f_B_x_Om_i_ni_1Q if depvar=="USD" & date_q>=$startq, r
reg d_e_eq_wgt_1Q f_B_x_Om_i_ni_1Q if depvar=="USD" & date_q>$crisisend, r

* App: Figure 1 equivalent
replace i_diff_4Q = i_diff_4Q/4

twoway (scatter d_e_eq_wgt_4Q L.i_diff_4Q, color(black))  (lfit d_e_eq_wgt_4Q L.i_diff_4Q, lcolor(black) lpattern(dash))  if depvar=="USD" & date_q < $startq,  graphregion(color(white)) xtitle("Interest Rate Differential") ytitle("Change in Broad Dollar") legend(off)
graph export "$graphs/appendix/erd_i_pre_4Q.eps", replace
twoway (scatter d_e_eq_wgt_4Q pi_q_diff_4Q, color(black)) (lfit d_e_eq_wgt_4Q pi_q_diff_4Q, lcolor(black) lpattern(dash)) if depvar=="USD" & date_q < $startq, graphregion(color(white)) xtitle("Inflation Differential") ytitle("Change in Broad Dollar") legend(off)
graph export "$graphs/appendix/erd_pi_pre_4Q.eps", replace
twoway (scatter d_e_eq_wgt_4Q d_gz_spread_4Q, color(black)) (lfit d_e_eq_wgt_4Q d_gz_spread_4Q, lcolor(black) lpattern(dash)) if depvar=="USD" & date_q < $startq, graphregion(color(white)) xtitle("Change in U.S. Corporate Bond Spread") ytitle("Change in Broad Dollar") legend(off)
graph export "$graphs/appendix/erd_gz_pre_4Q.eps", replace
twoway (scatter d_e_eq_wgt_4Q f_B_x_Om_i_ni_4Q, color(black)) (lfit d_e_eq_wgt_4Q f_B_x_Om_i_ni_4Q, lcolor(black) lpattern(dash)) if depvar=="USD" & date_q < $startq, graphregion(color(white)) xtitle("U.S. Foreign Bond Purchases") ytitle("Change in Broad Dollar") legend(off)
graph export "$graphs/appendix/erd_bopcf_pre_4Q.eps", replace

*************************
*** 2a. GRAPH: Rolling 60m rsq Broad usd versus Financial factors (monthly)
*** 2b. GRAPH: Rolling 60m rsq U.S.outflows versus Financial factors (monthly)
*************************

use "$user_dir/data/output/monthlymerged.dta", clear
tsset date_m
rename int_value_weighted_inve hkm
rename d_treasbasis d_dis

cap erase "$regs/monthly_riskfactors.xls"
cap erase "$regs/monthly_riskfactors.txt"

local rhslist  ""d_ln_vxo" "d_log_spx" "d_gz_spread" "d_gf" "d_dis" "hkm""
foreach rhs of local rhslist {
	reg d_log_s_eq_wgt `rhs' if date_m>=tm(2002m1) & date_m <= tm(2006m12), r
	outreg2 using "$regs/monthly_riskfactors.xls", ctitle("`rhs' 2005")
	reg d_log_s_eq_wgt `rhs' if date_m>=tm(2008m1) & date_m <= tm(2012m12), r
	outreg2 using "$regs/monthly_riskfactors.xls", ctitle("`rhs' 2013")
	reg d_log_s_eq_wgt `rhs' if date_m>=tm(2013m1) & date_m <= tm(2018m12), r
	outreg2 using "$regs/monthly_riskfactors.xls", ctitle("`rhs' 2018")
}

* Standardize into z-scores, and such that an increase is always an increase in risk bearing capacity
foreach rhs of local rhslist {
	egen z_`rhs' = std(`rhs')
}
replace z_d_ln_vxo = - z_d_ln_vxo
replace z_d_gz_spread = - z_d_gz_spread
replace z_d_dis = - z_d_dis 

foreach rhs of local rhslist {
	preserve
		keep if !missing(z_`rhs')
		rolling _b _se r2=e(r2), window(60) clear : reg d_log_s_eq_wgt z_`rhs'
		tsset end
		gen rhs_var="_`rhs'_60"
		save "$user_dir/data/temp/monthly_rolling_60m_`rhs'.dta", replace emptyok
	restore, preserve
		keep if !missing(z_`rhs')
		rolling _b _se r2=e(r2), window(120) clear : reg d_log_s_eq_wgt z_`rhs'
		tsset end
		gen rhs_var="_`rhs'_120"
		save "$user_dir/data/temp/monthly_rolling_120m_`rhs'.dta", replace emptyok	
	restore
}

clear

local rhslist  ""d_ln_vxo" "d_log_spx" "d_gz_spread" "d_gf" "d_dis" "hkm""

foreach rhs of local rhslist {
	append using "$user_dir/data/temp/monthly_rolling_60m_`rhs'.dta"
	append using "$user_dir/data/temp/monthly_rolling_120m_`rhs'.dta"
}

drop start
drop *_cons*
rename _se* _s* // name length limit in stata
reshape wide _*, i(end) j(rhs_var) string
missings dropvars, force

gen zero=0 // for dashed zero reference line

foreach months of numlist 60 120 {
	local years = `months'/12
	foreach rhs of local rhslist {
		gen upper_95_`rhs'_`months' = _b_z_`rhs'_`rhs'_`months' + 2*_s_z_`rhs'_`rhs'_`months'
		gen lower_95_`rhs'_`months' = _b_z_`rhs'_`rhs'_`months' - 2*_s_z_`rhs'_`rhs'_`months'
		replace lower_95_`rhs'_`months' = -0.025 if lower_95_`rhs'_`months' < -0.025 // for graphing, stata cannot restrict axis
		twoway rarea upper_95_`rhs'_`months' lower_95_`rhs'_`months' end, color(navy%20) lcolor(navy%0) || ///
		line zero end, lcolor(red) lpattern(dash) || ///
		line _b_z_`rhs'_`rhs'_`months' end, lcolor(black) ///
		xtitle("") ytitle("{&beta}{subscript:t}: rolling `years'Y window") graphregion(color(white)) ///
		ylabel(-0.025[0.0125]0.0375) ///
		legend(off)
		graph export "$graphs/rolling_`months'm_beta_se_`rhs'.pdf", replace
		drop *95*
	}

	tsline _eq*`months' if end>tm(1988m1), graphregion(color(white)) ///
		lcolor(forest_green sienna blue dkorange ltblue purple) lpattern("longdash_dot" "solid" "shortdash" "dash" "longdash" "shortdash_dot") ///
		xtitle("") ytitle("R{superscript:2}: rolling `years'Y window") title() ///
		legend(order(2 "Global Factor" 3 "GZ Spread" 4 "VXO" 5 "S&P500" 1 "Treasury Premium" 6 "Intermediary Returns") rows(2) size(3) region(lstyle(none) col(white)))
	graph export "$graphs/r2_monthly_all_`months'm.eps", replace

	tsline _b*`months' if end>tm(1988m1), graphregion(color(white)) ///
		lcolor(forest_green sienna blue dkorange ltblue purple) lpattern("longdash_dot" "solid" "shortdash" "dash" "longdash" "shortdash_dot") ///
		xtitle("") ytitle("{&beta}{subscript:t}") title() ///
		legend(order(2 "Global Factor" 3 "GZ Spread" 4 "VXO" 5 "S&P500" 1 "Treasury Premium" 6 "Intermediary Returns") rows(2) size(3) region(lstyle(none) col(white)))
	graph export "$graphs/beta_monthly_all_`months'm.eps", replace
}

/*
foreach rhs of local rhslist {
	tsline _eq2_r2_`rhs'_60 _eq2_r2_`rhs'_120  if end>tm(1987m1) , lpattern("solid" "dash") graphregion(color(white)) xtitle("") ytitle("R{superscript:2}") title("`rhs'") ///
		legend(order(1 "(Rolling 5y)" 2 "(Rolling 10y)") ///
		rows(1) size(3) region(lstyle(none) col(white))) 
	graph export "$graphs/r2_monthly_`rhs'_120m_60m.eps", replace
}
*/


/* Most recent obs always has a higher rsq than any pre-2007 obs
keep end _eq2_r2*
foreach var of varlist _* {
	gen cummax`var' = 0
	replace cummax`var' = `var'[_n] if `var'[_n] > cummax`var'[_n-1] & !missing(`var'[_n]) & end[_n] <= $startm
	replace cummax`var' = cummax`var'[_n-1] if cummax`var'[_n-1] > cummax`var'[_n] & !missing(cummax`var'[_n-1])
	replace `var' = `var'[_n-1] if missing(`var')
}
keep if _n==_N
keep *60
*/

*************************
*** 3. GRAPH: Rolling 40q / 20q rsq US outflows versus financial factors
*************************

foreach window of numlist 20 40 {
	local years = `window'/4
	use "$user_dir/data/output/allmerged.dta", clear
	local lhs="d_e_eq_wgt"
	local curr="USD"
	local rhslist  ""f_B_x_Om_i_ni" "d_ln_vxo" "d_log_spx" "d_gz_spread" "d_treasbasis" "int_value_weighted_inve" "d_gf""
	foreach exclusion in "allperiods" {

		foreach rhs of local rhslist {
			use "$user_dir/data/output/allmerged.dta", clear
			if "`exclusion'" == "excrisis" {
				drop if date_q >=$crisisstart & date_q <= $crisisend
			}
			egen start_date_t=min(date_q) if `lhs'~=. & `rhs'~=. & depvar=="USD"
			egen start_date=max(start_date_t)
			format start_date %tq
			local start_date=start_date[1]
			drop if missing(`rhs')
			rolling _b _se r2=e(r2), window(`window') clear : reg `lhs' `rhs'  if depvar=="USD" & date_q<=tq(2018q4) 
			keep if end>=`start_date'+`window'
			save "$user_dir/data/temp/imf_bop/`rhs'_rolling_`exclusion'.dta", replace
		}

		use "$user_dir/data/temp/imf_bop/f_B_x_Om_i_ni_rolling_`exclusion'.dta", clear
		keep end _eq2_r2 
		rename _eq2 f_B_x_Om_i_ni
		foreach rhs of local rhslist {
			if "`rhs'" != "f_B_x_Om_i_ni" {
				mmerge end using "$user_dir/data/temp/imf_bop/`rhs'_rolling_`exclusion'.dta", ukeep(_eq2_r2)
				rename _eq2_r2 `rhs'
			}
		}

		twoway (line f_B_x_Om_i_ni end, lcolor(black) lwidth(medthick)) (line d_ln_vxo end, lcolor(dkorange) lpattern("dash")) (line d_treasbasis end, lcolor(forest_green) lpattern("shortdash")) ///
			(line d_gz_spread end, lcolor(blue) lpattern("dash_dot")) (line d_log_spx end, lcolor(ltblue) lpattern("longdash")) (line int_value_weighted_inve end, lcolor(purple) lpattern("dot")) /// 
			(line d_gf end, lcolor(sienna) lpattern("shortdash_dot")), ///
			graphregion(color(white)) xtitle("") ytitle("R{superscript:2}: rolling `years'Y window") yscale(range(0(0.1)0.6)) ///
			legend(order(1 "U.S. Foreign Bond Purchases" 2 "VXO" 3 "Treasury Premium" 4 "GZ Spread" 5 "S&P500" 6 "Intermediaries" 7 "Global Return Factor") rows(3) region(lstyle(none) col(white)) size(*.75))
		graph export "$graphs/r2_`window'q_comparison_riskexpanded_`exclusion'.eps", replace

		local lhs="f_B_x_Om_i_ni"
		local curr="USD"
		local rhslist ""d_ln_vxo" "d_log_spx" "d_gz_spread" "d_treasbasis" "int_value_weighted_inve" "d_gf""

		foreach rhs of local rhslist {
			use "$user_dir/data/output/allmerged.dta", clear
			if "`exclusion'" == "excrisis" {
				drop if date_q >=$crisisstart & date_q <= $crisisend
			}
			egen start_date_t=min(date_q) if `lhs'~=. & `rhs'~=. & depvar=="USD"
			egen start_date=max(start_date_t)
			format start_date %tq
			local start_date=start_date[1]
			drop if missing(`rhs')
			rolling _b _se r2=e(r2), window(`window') clear : reg `lhs' `rhs'  if depvar=="USD" & date_q<=tq(2018q4) 
			keep if end>=`start_date'+`window'
			save "$user_dir/data/temp/imf_bop/us_outflow_`rhs'_rolling_`exclusion'.dta", replace
		}

		use "$user_dir/data/temp/imf_bop/us_outflow_d_ln_vxo_rolling_`exclusion'.dta", clear
		keep end _eq2_r2 
		rename _eq2 d_ln_vxo
		foreach rhs of local rhslist {
			if "`rhs'" != "d_ln_vxo" {
				mmerge end using "$user_dir/data/temp/imf_bop/us_outflow_`rhs'_rolling_`exclusion'.dta", ukeep(_eq2_r2)
				rename _eq2_r2 `rhs'
			}
		}

		twoway (line d_ln_vxo end, lcolor(orange) lpattern("dash")) (line d_treasbasis end, lcolor(forest_green) lpattern("shortdash")) (line d_gz_spread end, lcolor(blue) lpattern("dash_dot")) ///
			(line d_log_spx end, lcolor(ltblue) lpattern("longdash")) (line int_value_weighted_inve end, lcolor(purple) lpattern("dot")) (line d_gf end, lcolor(sienna) lpattern("shortdash_dot")) ///
			, graphregion(color(white)) xtitle("") ytitle("R{superscript:2}: rolling `years'Y window") ///
			legend(order(6 "Global Return Factor" 3 "GZ Spread" 1 "VXO" 4 "S&P500" 2 "Treasury Premium"  5 "Intermediaries") rows(2) size(3) region(lstyle(none) col(white))) ///
			yscale(range(0(0.1)0.6))
		graph export "$graphs/r2_`window'q_comparison_us_outflow_v_riskexpanded_`exclusion'.eps", replace
	}
}

*************************
*** 4. GRAPH: INDIVIDUAL CURRENCY COEFFICIENTS ON GZ SPREAD
***    Both bilateral and G10 equally weighted.
*************************

local lhs_set = `"d_e"'

foreach exrate in `lhs_set' {
	cap erase "$regs/`exrate'_usd_gz_spread.dta"
	cap erase "$regs/`exrate'_usd_gz_spread.txt"
	cap erase "$regs/`exrate'_usd_gz_spread.xls"
	use "$user_dir/data/output/monthlymerged.dta", clear
	keep if !missing(d_gz_spread)
	drop iso_curr
	merge 1:m date using "$user_dir/data/output/ER_data/log_exrate_bilateral_m.dta", keep(3)
	drop if iso_curr=="USD"
	foreach x of global currlist {
		if "`x'" != "USD" {
			di "currency `x'"
			reg `exrate' d_gz_spread if iso_curr=="`x'" & date_m>=$startm, r
			outreg2 using "$regs/`exrate'_usd_gz_spread.xls", ctitle("`x'") dta
		}
	}

	use "$regs/`exrate'_usd_gz_spread_dta.dta", clear
	keep if _n==2 | _n==4 | _n==5 | _n==10
	replace v1="se" if v1==""
	foreach x of varlist _all {
		local temp=`x'[1]
		replace `x'=subinstr(`x',"(","",.)
		replace `x'=subinstr(`x',")","",.)
		replace `x'=subinstr(`x',`"*"',"",.)
		rename `x' xxx`temp'
	}	
	drop if _n==1
	foreach x of varlist _all {
		cap destring `x', replace
	}
	rename xxxVAR var
	reshape long xxx, i(var) j(curr) str
	replace var="Rsq" if regexm(var,"R-s")==1
	reshape wide xxx, i(curr) j(var) str
	renpfix xxx
	rename d_gz_spread coeff
	gen c_2se_plus=coeff+2*se
	gen c_2se_minus=coeff-2*se
	gsort coeff
	gen cid=_n
	local command = "label define cid "
	count
	foreach i of numlist 1/`r(N)' {
		local add = curr[`i']
		local command = "`command' `i' `add'" 
	}
	local command "`command' , replace"
	`command'
	label values cid cid
	twoway (scatter coeff cid) (rcap c_2se_minus c_2se_plus cid), yline(0, lpattern(dash) lcolor(grey)) xlabel(1(1)25,valuelabel angle(vertical)) xtitle("") legend(off) graphregion(color(white)) ytitle("{&beta}{subscript:i}")
	graph export "$graphs/`exrate'_beta.eps", replace
}

*************************
*** 5. TABLE: ALL BILATERAL COMBINATION RSQUAREDS ON GZ SPREAD
*************************

*MAKE THE MATRIX OF BILATERALS
use "$user_dir/data/output/monthlymerged.dta", clear
keep if !missing(d_gz_spread)
drop iso_curr
keep d_gz_spread date

merge 1:m date using "$user_dir/data/output/ER_data/bilateral_matrix_m.dta", keep(3)
drop _merge
order date d_gz_spread*
save "$user_dir/data/temp/reg_matrix.dta", replace 

forvalues i=1/3 {
	use "$user_dir/data/temp/reg_matrix.dta", clear
	if `i'==1 {
		local app="_full"
	}	
	if `i'==2 {
		keep if date>=$startm
		local app="_post"
	}	
	if `i'==3 {
		keep if date<$startm
		local app="_pre"		
	}	
	quietly {
		foreach x of global g10_currency {
			foreach y of global g10_currency {
				if "`x'"~="`y'" {
					reg d_e_`x'_`y' d_gz_spread, r
					gen r2_e_`x'_`y'=round(e(r2),.001)*100
					gen beta_gz_`x'_`y'=_b[d_gz_spread]
				}			
			}	
		}
	}
	keep if _n==1
	drop d*
	gen n=_n
	save "$regs/matrix`app'_long.dta", replace
	reshape long r2_e_ beta_gz_, i(n) j(pairs) str
	split pairs, p("_")
	rename pairs1 iso_currency_code
	rename pairs2 base
	save "$regs/matrix`app'.dta", replace
}

foreach app in _full _pre _post {
	use "$regs/matrix`app'.dta", clear
	drop n pairs 
	reshape wide r2_e_ beta_gz_, i(base) j(iso_cu) str
	order base* r2_* beta*

	set obs 12
	replace base = "Mean" in 11
		foreach x of varlist r2* beta* {
			summ `x'
			replace `x'=r(mean) if base=="Mean"
		}	

	replace base = "ExUSD" in 12
	foreach x of global g10_currency {
		foreach y in r2_e_ beta_gz_ {
			summ `y'`x' if base~="Mean" & base~="USD"
			replace `y'`x'=r(mean) if base=="ExUSD"
			label var `y'`x' "`x'"
		}	
	}

	foreach x of varlist r2* {
		replace `x'=round(`x',1)
	}
	foreach x of varlist beta* {
		replace `x'=round(`x',.001)
	}	

	label var base " "
	save "$regs/matrix`app'_usdmerge.dta", replace
	foreach var in r2_e beta_gz {
		export excel base `var'* using "$regs/bilaterals_`var'`app'.xls", firstrow(varlabels) replace
	}	
}

copy "$regs/bilaterals_r2_e_post.xls" "$regs/final/table1.xls", replace
copy "$regs/bilaterals_r2_e_pre.xls" "$regs/final/z_appendix_table1.xls", replace

*************************
*** 6. Figure: Broad USD Rolling Rsquare with and without crisis, 20Q & 40Q
*************************

foreach quarters of numlist 20 40 {

	use "$user_dir/data/output/allmerged.dta", clear
	gen crisis=0
	replace crisis=1 if date_q>=$crisisstart & date_q<=$crisisend

	preserve
	rolling _b _se r2=e(r2), window(`quarters') clear : reg d_e_eq_wgt f_B_x_Om_i_ni if depvar=="USD" & date_q<=tq(2018q4) & date_q>=tq(1977q1)
	save "$user_dir/data/temp/imf_bop/bop_rolling_`quarters'.dta", replace
	restore

	rolling _b _se r2=e(r2), window(`quarters') clear : reg d_e_eq_wgt f_B_x_Om_i_ni if depvar=="USD" & date_q<=tq(2018q4) & date_q>=tq(1977q1) & crisis==0
	mmerge end using "$user_dir/data/temp/imf_bop/bop_rolling_`quarters'.dta", uname(full)
	twoway  (line _e end, lpattern("-") lcolor(red)) (line full_eq2_r2 end, lpattern("l") lcolor(black) lwidth(medthick)), legend(order(2 "Full Sample (1977:Q1-2019:Q2)" 1 "Excluding 2007:Q1-2009:Q2") size(3)) graphregion(color(white)) xtitle("") ytitle("R{superscript:2}: rolling 5Y window")
	graph export "$graphs/bop_rolling_r2_`quarters'.eps", replace

}

clear

foreach quarters of numlist 20 40 {

	append using "$user_dir/data/temp/imf_bop/bop_rolling_`quarters'.dta"
	cap gen quarters = `quarters'
	cap replace quarters = `quarters' if missing(quarters)
	
}
keep quarters _eq2_r2 end
reshape wide _eq2_r2, i(end) j(quarters)
tsset end
tsline _eq2*, lcolor(red black blue) lwidth(medium medthick) lpattern(dash solid) ///
	legend(order(1 "5 year" 2 "10 year") size(4) rows(1) region(lstyle(none) col(white))) graphregion(color(white)) ///
	xtitle("") ytitle("R{superscript:2}: rolling window")
graph export "$graphs/bop_rolling_r2_multiple_window.eps", replace

*************************
*** 7. FIGURE & TABLE: MEESE ROGOFF SCATTER: RMSE AND PVALUE / FUNDAMENTALS INSAMPLE ROLLING 40 PERIOD RSQUARE GRAPH
*************************

foreach window of numlist 20 40 {

	local numyears = `window'/4

	*CLEAR OLD RESULTS
	cap erase "$regs/unformatted/`lhs'_fundamentals_insample_`numyears'.xls"
	cap erase "$regs/unformatted/`lhs'_fundamentals_insample_`numyears'.txt"
	cap erase "$regs/unformatted/`lhs'_fundamentals_insample_pre_`numyears'.xls"
	cap erase "$regs/unformatted/`lhs'_fundamentals_insample_pre_`numyears'.txt"

	*DEFINE WHAT EXCHANGE RATE TO FORECAST, AND WHAT SET OF RHS VARIABLES
	local lhs="d_e_eq_wgt"
	local curr="USD"
	local rhslist ""f_B_x_Om_i_ni" "pi_q_diff dy_q_diff" "pi_q_diff dc_q_diff" "pi_q_diff y_gap_diff" "L_i_diff""

	use "$user_dir/data/output/allmerged.dta", clear
	tsset cid date_q
	gen L_i_diff = L.i_diff
	preserve

	*ESTIMATE ROLLING PARAMATERS, SAVE DOWN
	*NB: Code works for only up to 4 RHS variables. Extend code if more.
	foreach rhs of local rhslist {
		restore, preserve
		local rhs_filename = subinstr("`rhs'"," ","_",.)
		local rhs_filename = subinstr("`rhs_filename'",".","_",.)
		display "filename suffix is `rhs_filename'"
		local var_a : word 1 of `rhs'
		local var_b : word 2 of `rhs'
		local var_c : word 3 of `rhs'
		local var_d : word 4 of `rhs'
		display "Var 1 is `var_a'"
		display "Var 2 is `var_b'"
		display "Var 3 is `var_c'"
		display "Var 4 is `var_d'"
		keep if depvar=="`curr'"
		tsset date_q 
		if "`rhs'" == "f_B_x_Om_i_ni" {
			keep if date_q>=$mrstart_macro
		}
		else {
			keep if date_q>=$mrstart_macro & date_q <= $mrend
		}
		
		reg `lhs' `rhs' if date_q>=$startq & date_q<=$endq, r
		outreg2 using "$regs/unformatted/`lhs'_fundamentals_insample_`numyears'.xls", auto(2)
		reg `lhs' `rhs' if date_q<$startq & date_q>=tq(1977q1), r
		outreg2 using "$regs/unformatted/`lhs'_fundamentals_insample_pre_`numyears'.xls", auto(2)

		* Rolling estimates including crisis
		rolling _b _se r2=e(r2), window(`window') clear : reg `lhs' `rhs', r
		
		gen df = _n + $burnin - 2
		merge 1:1 df using "$user_dir/data/raw/ttable/ttable.dta", keep(3) nogen

		gen _b_`var_a'_95plus= _b_`var_a' + tinv*_se_`var_a'
		gen _b_`var_a'_95minus= _b_`var_a' - tinv*_se_`var_a'
		if "`var_b'" != "" {
			gen _b_`var_b'_95plus= _b_`var_b' + tinv*_se_`var_b'
			gen _b_`var_b'_95minus= _b_`var_b' - tinv*_se_`var_b'
		}
		if "`var_c'" != "" {
			gen _b_`var_c'_95plus= _b_`var_c' + tinv*_se_`var_c'
			gen _b_`var_c'_95minus= _b_`var_c' - tinv*_se_`var_c'
		}
		if "`var_d'" != "" {
			gen _b_`var_d'_95plus= _b_`var_d' + tinv*_se_`var_d'
			gen _b_`var_d'_95minus= _b_`var_d' - tinv*_se_`var_d'
		}
		* Check to see whether we are using 1-4 RHS variables
		if "`var_b'" == "" {
			save "$fcast/`lhs'_`rhs_filename'_`curr'.dta", replace
		}
		else if "`var_c'" == "" {
			save "$fcast/`lhs'_`rhs_filename'_`curr'.dta", replace
		}
		else if "`var_d'" == "" {
			save "$fcast/`lhs'_`rhs_filename'_`curr'.dta", replace
		}
		else if "`var_d'" != "" {
			save "$fcast/`lhs'_`rhs_filename'_`curr'.dta", replace
		}
	}
	restore

	*USE BACKWARDLOOKING PARAMETERS AND OOS X VARS TO MAKE OOS FORECASTS
	use "$user_dir/data/output/allmerged.dta", clear
	tsset cid date_q
	gen L_i_diff = L.i_diff
	preserve

	foreach exclusion in "allperiods" "excrisis" {
		foreach rhs of local rhslist {
			restore, preserve
			local rhs_filename = subinstr("`rhs'"," ","_",.) 
			local rhs_filename = subinstr("`rhs_filename'",".","_",.)
			local var_a : word 1 of `rhs'
			local var_b : word 2 of `rhs'
			local var_c : word 3 of `rhs'
			local var_d : word 4 of `rhs'
			display "Var 1 is `var_a'"
			display "Var 2 is `var_b'"
			display "Var 3 is `var_c'"
			display "Var 4 is `var_d'"
			keep if depvar=="`curr'"
			mmerge date_q using "$fcast/`lhs'_`rhs_filename'_`curr'.dta", umatch(end)
			tsset date_q
			rename _b_cons cons
			
			* Generate forecast versus actual
			gen actual=f.`lhs'
			if "`var_b'" == "" { 
				gen forecast=cons + _b_`var_a' *f.`var_a'
			}
			else if "`var_c'" == "" {
				gen forecast=cons + _b_`var_a' *f.`var_a' + _b_`var_b' *f.`var_b' 
			}
			else if "`var_d'" == "" {
				gen forecast=cons + _b_`var_a' *f.`var_a' + _b_`var_b' *f.`var_b' + _b_`var_c' *f.`var_c' 
			}
			else if "`var_d'" != "" {
				gen forecast=cons + _b_`var_a' *f.`var_a' + _b_`var_b' *f.`var_b' + _b_`var_c' *f.`var_c' + _b_`var_d' *f.`var_d' 
			}	
			gen rw=0
			
			* Generate forecast and rw RMSE and MAE	

			gen rw_mse=actual^2
			gen rw_mae=abs(actual)
			gen fcast_mse=(forecast-actual)^2
			gen fcast_mae=abs(forecast-actual)
			
			* Rolling $windowsize forecast 
			sort date
			gen roll_dm_mse_pval = .
			gen roll_dm_mae_pval = .
			
			foreach measure_name in rw_mse rw_mae fcast_mse fcast_mae {
				gen `measure_name'_sum_temp = sum(`measure_name')
				gen `measure_name'_sum_rolling = `measure_name'_sum[_n] - `measure_name'_sum[_n-`window']
				drop `measure_name'_sum_temp
			}
			
			* Rolling $windowsize ratios
			gen roll_ratio_rmse = (fcast_mse_sum_rolling)^(1/2)/(rw_mse_sum_rolling)^(1/2) 
			gen roll_ratio_mae = (fcast_mae_sum_rolling)/(rw_mae_sum_rolling) 
			drop *_sum_rolling
			
			keep if actual~=. & forecast~=.

			* Rolling $windowsize DM p-values
			levels date, local(datevalues)
			foreach enddate of local datevalues {
				quietly count if date_q > `enddate'-`window' & date_q <= `enddate' & !missing(actual) & !missing(forecast)
				if `r(N)' == `window' {
					cap dmariano actual forecast rw if date_q > `enddate'-`window' & date_q <= `enddate', crit(MSE) kernel(Bartlett)
					replace roll_dm_mse_pval=r(p) if date_q == `enddate'
					cap dmariano actual forecast rw if date_q > `enddate'-`window' & date_q <= `enddate', crit(MSE) // if missing, change kernel
					replace roll_dm_mse_pval=r(p) if date_q == `enddate' & missing(roll_dm_mse_pval)
					cap dmariano actual forecast rw if date_q > `enddate'-`window' & date_q <= `enddate', crit(MAE) kernel(Bartlett) 
					replace roll_dm_mae_pval=r(p) if date_q == `enddate'
					cap dmariano actual forecast rw if date_q > `enddate'-$mr_rollsize & date_q <= `enddate', crit(MAE) // if missing, change kernel
					replace roll_dm_mae_pval=r(p) if date_q == `enddate' & missing(roll_dm_mae_pval)
				}
			}

			if "`exclusion'"=="excrisis" {
				drop if date >= $crisisstart & date <= $crisisend
			}

			*DIEBOLD MARIANO TEST USING 2006 onwards sample
			cap dmariano actual forecast rw if date_q>=$mrevalstart+1, crit(MSE)
			local s1_dm_mse_u=r(s1)
			local p_dm_mse_u=r(p)
			cap dmariano actual forecast rw if date_q>=$mrevalstart+1, crit(MAE)
			local s1_dm_mae_u=r(s1)
			local p_dm_mae_u=r(p)
			cap dmariano actual forecast rw if date_q>=$mrevalstart+1, crit(MSE) kernel(bartlett)
			local s1_dm_mse_b=r(s1)
			local p_dm_mse_b=r(p)
			cap dmariano actual forecast rw if date_q>=$mrevalstart+1, crit(MAE)  kernel(bartlett)
			local s1_dm_mae_b=r(s1)
			local p_dm_mae_b=r(p)
			
			foreach x in rw_mse rw_mae fcast_mse fcast_mae {
				egen `x'_sum=sum(`x') if date_q>=$mrevalstart
			}
			gen rw_rmse=(rw_mse_sum/$NQ)^(1/2)
			gen fcast_rmse=(fcast_mse_sum/$NQ)^(1/2)

			replace rw_mae=(rw_mae_sum/$NQ)
			replace fcast_mae=(fcast_mae_sum/$NQ)

			foreach x in fcast_rmse rw_rmse rw_mae fcast_mae {
				summ `x', detail
				local `x'_temp=r(p50)
			}
			local ratio=`fcast_rmse_temp'/`rw_rmse_temp'
			local ratio=round(`ratio',.0001) 
			if "`exclusion'"=="allperiods" {
				save "$fcast/fcast_`lhs'_`rhs_filename'_`curr'.dta", replace
			}
			if "`exclusion'"=="allperiods" {
				twoway (line actual date_q, lcolor(blue) lpattern(solid)) (line forecast date_q, lcolor(red) lpattern(dash)) (line rw date_q, lcolor(grey) lpattern(dash)), name("bd", replace) legend(order(1 "Actual FX" 2 "Forecast FX")) ytitle("Log Change") xtitle("") graphregion(color(white))
				graph export  "$graphs/mr_`lhs'_`rhs_filename'_`curr'_`window'.eps", replace
				if "`rhs_filename'"=="f_B_x_Om_i_ni" {
					twoway (line actual date_q if date_q>=$startq, lcolor(blue) lpattern(solid)) (line forecast date_q if date_q>=$startq, lcolor(red) lpattern(dash)) (line rw date_q if date_q>=$startq, lcolor(grey) lpattern(dash)), name("bd", replace) legend(order(1 "Actual FX" 2 "Forecast FX")) ytitle("Log Change") xtitle("Quarter") graphregion(color(white))
					graph export  "$graphs/mr_`lhs'_`rhs_filename'_`curr'_`window'.eps", replace
				}
			}
			clear
			gen var1=""
			set obs 2
			gen S_1=.
			gen pval=.
			replace var1="MAE" if _n==1
			replace var1="MSE" if _n==2
			replace S_1=`s1_dm_mse_b' if var1=="MSE"
			replace pval=`p_dm_mse_b' if var1=="MSE"
			replace S_1=`s1_dm_mae_b' if var1=="MAE"
			replace pval=`p_dm_mae_b' if var1=="MAE"

			gen Forecast=.
			gen RW=.
			replace Forecast=`fcast_rmse_temp' if var1=="MSE"
			replace RW=`rw_rmse_temp' if var1=="MSE"
			replace Forecast=`fcast_mae_temp' if var1=="MAE"
			replace RW=`rw_mae_temp' if var1=="MAE"
			gen ratio=Forecast/RW
			export excel using "$regs/mr_`lhs'_`rhs_filename'_`curr'_eval_`exclusion'.xlsx", firstrow(variables) replace
		}
	}
	restore
	clear

	* GRAPH ALL OOS RATIOS
	local lhs="d_e_eq_wgt"
	local curr="USD"
	local rhslist ""f_B_x_Om_i_ni" "pi_q_diff dy_q_diff" "pi_q_diff dc_q_diff" "pi_q_diff y_gap_diff" "L_i_diff""

	foreach rhs of local rhslist {
		local rhs_filename = subinstr("`rhs'"," ","_",.) 
		local rhs_filename = subinstr("`rhs_filename'",".","_",.)

		append using "$fcast/fcast_`lhs'_`rhs_filename'_`curr'.dta"
			if "`rhs_filename'" == "f_B_x_Om_i_ni" {
			gen fcast_type="f_B_x_Om_i_ni"
		}
		replace fcast_type = "`rhs_filename'" if missing(fcast_type)
	}
	order roll* fcast_type
	preserve
		keep date_q depvar fcast_type roll_*
		keep date_q depvar fcast_type *mse*
		separate roll_dm_mse_pval, by(fcast_type)
		drop roll_dm_mse_pval
		bysort fcast_type (date_q): gen last=1 if _n==_N
		graph twoway (scatter roll_dm_mse_pval* roll_ratio_rmse if inrange(roll_ratio_rmse, .75, 1.25) & quarter(dofq(date_q))==4 & date_q < tq(2008q1), mcolor(navy%90 black%90 red%90 forest_green%90 purple%90) ///
			 msymbol(x x x x x) ytitle("p-value (Diebold-Mariano)") xtitle("RMSE Ratio") graphregion(color(white)) ///
			 xscale(r(0.75 1.25)) xlabel(0.75(0.125)1.25) ylabel(0.05 0.1 0.5 1, angle(0)) ///
			 xline(1, lcolor(black%50) lpattern(dash)) yline(0.05 0.1, lcolor(black%50) lpattern(dash))) ///
			 (scatter roll_dm_mse_pval* roll_ratio_rmse if inrange(roll_ratio_rmse, .75, 1.25) & quarter(dofq(date_q))==4 & inrange(date_q, tq(2008q1), tq(2018q3)), mcolor(navy%90 black%90 red%90 forest_green%90 purple%90) ///
			 msymbol(oh oh oh oh oh)) ///
			 (scatter roll_dm_mse_pval* roll_ratio_rmse if last==1, mcolor(navy%90 black%90 red%90 forest_green%90 purple%90)), ///
			 legend(order(12 "U.S. Foreign Bond Purchases" 11 "UIP" 13 "Backus-Smith" 14 "Monetary" 15 "Taylor Rule") rows(2) size(3) region(lstyle(none) col(white)))
		graph export "$graphs/appendix/mr_scatter_mse_`lhs'_`curr'_`window'.eps", replace
	restore
		keep date_q depvar fcast_type roll_*
		keep date_q depvar fcast_type *mae*
		separate roll_dm_mae_pval, by(fcast_type)
		drop roll_dm_mae_pval
		bysort fcast_type (date_q): gen last=1 if _n==_N
		graph twoway (scatter roll_dm_mae_pval* roll_ratio_mae if inrange(roll_ratio_mae, .75, 1.25) & quarter(dofq(date_q))==4 & date_q < tq(2008q1), mcolor(blue%90 black%90 red%90 forest_green%90 sienna%90) ///
			 msymbol(x x x x x) ytitle("p-value (Diebold-Mariano)") xtitle("MAE Ratio") graphregion(color(white)) ///
			 xscale(r(0.75 1.25)) xlabel(0.75(0.125)1.25) ylabel(0.05 0.1 0.5 1, angle(0)) ///
			 xline(1, lcolor(black%50) lpattern(dash)) yline(0.05 0.1, lcolor(black%50) lpattern(dash))) ///
			 (scatter roll_dm_mae_pval* roll_ratio_mae if inrange(roll_ratio_mae, .75, 1.25) & quarter(dofq(date_q))==4 & inrange(date_q, tq(2008q1), tq(2018q3)), mcolor(blue%90 black%90 red%90 forest_green%90 sienna%90) ///
			 msymbol(oh oh oh oh oh)) ///
			 (scatter roll_dm_mae_pval* roll_ratio_mae if last==1, mcolor(navy%90 black%90 red%90 forest_green%90 purple%90)), ///
			 legend(order(12 "U.S. Foreign Bond Purchases" 11 "UIP" 13 "Backus-Smith" 14 "Monetary" 15 "Taylor Rule") rows(2) size(3) region(lstyle(none) col(white)))
		graph export "$graphs/appendix/mr_scatter_mae_`lhs'_`curr'_`window'.eps", replace
	clear

	* GRAPH ALL ROLLING RSQUAREDS

	clear
	local lhs="d_e_eq_wgt"
	local curr="USD"
	local rhslist ""f_B_x_Om_i_ni" "pi_q_diff dy_q_diff" "pi_q_diff dc_q_diff" "pi_q_diff y_gap_diff" "L_i_diff""

	foreach rhs of local rhslist {
		local rhs_filename = subinstr("`rhs'"," ","_",.) 
		local rhs_filename = subinstr("`rhs_filename'",".","_",.)

		append using "$fcast/`lhs'_`rhs_filename'_`curr'.dta"
			if "`rhs_filename'" == "f_B_x_Om_i_ni" {
			gen fcast_type="f_B_x_Om_i_ni"
		}
		replace fcast_type = "`rhs_filename'" if missing(fcast_type)
	}
	drop if end <= tq(2002q1) & fcast_type=="pi_q_diff_dc_q_diff" // This could be implemented better
	keep end _eq2_r2 fcast_type 
	reshape wide _eq2_r2, i(end) j(fcast_type) string 
	tsset end
	tsline _eq* if end >= tq(1990q1) & end <= tq(2018q4), ///
		lpattern("shortdash" "solid" "dash_dot" "dash" "dot" ) graphregion(color(white)) xtitle("") ytitle("R{superscript:2}: rolling `numyears'Y window") ///
		lwidth(med medthick med med med) ///
		lcolor(blue black red forest_green sienna) ///
		legend(order(2 "U.S. Foreign Bond Purchases" 1 "UIP" 3 "Backus-Smith" 4 "Monetary" 5 "Taylor") rows(2) region(lstyle(none) col(white)))
		graph export "$graphs/rolling_`lhs'_`curr'_fundamentals_`window'.eps", replace

	* Values for text
	import excel using "$regs/mr_d_e_eq_wgt_f_B_x_Om_i_ni_USD_eval_allperiods.xlsx", clear
	renvars, map(strtoname(@[1]))
	keep if var1=="MSE"
	gen benchmark="All Periods"
	save "$regs/benchmarking", replace emptyok
	import excel using "$regs/mr_d_e_eq_wgt_f_B_x_Om_i_ni_USD_eval_excrisis.xlsx", clear
	renvars, map(strtoname(@[1]))
	keep if var1=="MSE"
	append using "$regs/benchmarking"
	replace benchmark="Ex crisis" if _n==1
	replace var1="RMSE"
	export excel "$graphs/mr_scatter_mse_d_e_eq_wgt_USD.xls", firstrow(varlabels) replace
	
}

* FORMAT FINAL TABLE
import delimited using "$regs/unformatted/d_e_eq_wgt_fundamentals_insample_10.txt", clear
gen period="post"
save "$regs/unformatted/d_e_eq_wgt_fundamentals_insample_post_10.dta", replace emptyok
import delimited using "$regs/unformatted/d_e_eq_wgt_fundamentals_insample_pre_10.txt", clear
gen period="pre"
append using "$regs/unformatted/d_e_eq_wgt_fundamentals_insample_post_10.dta"
drop if v2=="(1)" | v2=="" & v3=="" & v4=="" & v5=="" & v6=="" | v1=="Constant" | v1[_n-1]=="Constant"
gen order=0 if v1=="VARIABLES"
replace order=1 if v1=="f_B_x_Om_i_ni"
replace order=3 if v1=="L_i_diff"
replace order=5 if v1=="pi_q_diff"
replace order=7 if v1=="dc_q_diff"
replace order=9 if v1=="dy_q_diff"
replace order=11 if v1=="y_gap_diff"
replace order=13 if v1=="Observations"
replace order=14 if v1=="R-squared"
replace order=order[_n-1]+1 if missing(order)
replace order=order+15 if period=="post"
sort order
order v1 v2 v6 v4 v3 v5
drop order
outsheet using "$regs/final/table2.xls", replace 

*************************
*** 8. FIGURE & TABLE: : MEESE ROGOFF SCATTER: RMSE AND PVALUE / ROLLING 120 PERIOD RSQUARE GRAPH
*************************

foreach window of numlist 60 120 {

	local numyears = `window'/12

	*CLEAR OLD RESULTS
	cap erase "$regs/unformatted/`lhs'_riskmonthly_insample.xls"
	cap erase "$regs/unformatted/`lhs'_riskmonthly_insample.txt"
	cap erase "$regs/unformatted/`lhs'_riskmonthly_insample_pre.xls"
	cap erase "$regs/unformatted/`lhs'_riskmonthly_insample_pre.txt"

	*DEFINE WHAT EXCHANGE RATE TO FORECAST, AND WHAT SET OF RHS VARIABLES
	local lhs="d_log_s_eq_wgt"
	local rhslist ""d_ln_vxo" "d_log_spx" "d_gz_spread" "d_gf" "d_dis" "hkm""

	use "$user_dir/data/output/monthlymerged.dta", clear
	tsset date_m
	rename int_value_weighted_inve hkm
	rename d_treasbasis d_dis
	preserve

	*ESTIMATE ROLLING PARAMATERS, SAVE DOWN
	*NB: Code works for only up to 4 RHS variables. Extend code if more.
	foreach rhs of local rhslist {
		restore, preserve
		local rhs_filename = subinstr("`rhs'"," ","_",.)
		local rhs_filename = subinstr("`rhs_filename'",".","_",.)
		display "filename suffix is `rhs_filename'"
		local var_a : word 1 of `rhs'
		local var_b : word 2 of `rhs'
		local var_c : word 3 of `rhs'
		local var_d : word 4 of `rhs'
		display "Var 1 is `var_a'"
		display "Var 2 is `var_b'"
		display "Var 3 is `var_c'"
		display "Var 4 is `var_d'"
		tsset date_m 
		
		reg `lhs' `rhs' if date_m>=$startm & date_m<=$mrendm, r
		outreg2 using "$regs/unformatted/`lhs'_riskmonthly_insample.xls"
		reg `lhs' `rhs' if date_m<$startm & date_m>=$mrstart_macro_m, r
		outreg2 using "$regs/unformatted/`lhs'_riskmonthly_insample_pre.xls"

		* Rolling estimates including crisis
		rolling _b _se r2=e(r2), window(`window') clear : reg `lhs' `rhs', r
		
		gen df = _n + $burnin - 2
		merge 1:1 df using "$user_dir/data/raw/ttable/ttable.dta", keep(1 3) nogen
		replace tinv = 1.96 if df>200

		gen _b_`var_a'_95plus= _b_`var_a' + tinv*_se_`var_a'
		gen _b_`var_a'_95minus= _b_`var_a' - tinv*_se_`var_a'
		if "`var_b'" != "" {
			gen _b_`var_b'_95plus= _b_`var_b' + tinv*_se_`var_b'
			gen _b_`var_b'_95minus= _b_`var_b' - tinv*_se_`var_b'
		}
		if "`var_c'" != "" {
			gen _b_`var_c'_95plus= _b_`var_c' + tinv*_se_`var_c'
			gen _b_`var_c'_95minus= _b_`var_c' - tinv*_se_`var_c'
		}
		if "`var_d'" != "" {
			gen _b_`var_d'_95plus= _b_`var_d' + tinv*_se_`var_d'
			gen _b_`var_d'_95minus= _b_`var_d' - tinv*_se_`var_d'
		}
		* Check to see whether we are using 1-4 RHS variables
		if "`var_b'" == "" {
			save "$fcast/`lhs'_`rhs_filename'_`curr'.dta", replace
		}
		else if "`var_c'" == "" {
			save "$fcast/`lhs'_`rhs_filename'_`curr'.dta", replace
		}
		else if "`var_d'" == "" {
			save "$fcast/`lhs'_`rhs_filename'_`curr'.dta", replace
		}
		else if "`var_d'" != "" {
			save "$fcast/`lhs'_`rhs_filename'_`curr'.dta", replace
		}
	}
	restore

	*USE BACKWARDLOOKING PARAMETERS AND OOS X VARS TO MAKE OOS FORECASTS
	use "$user_dir/data/output/monthlymerged.dta", clear
	tsset date_m
	rename int_value_weighted_inve hkm
	rename d_treasbasis d_dis
	preserve

	foreach exclusion in "allperiods" "excrisis" {
		foreach rhs of local rhslist {
			restore, preserve
			local rhs_filename = subinstr("`rhs'"," ","_",.) 
			local rhs_filename = subinstr("`rhs_filename'",".","_",.)
			local var_a : word 1 of `rhs'
			local var_b : word 2 of `rhs'
			local var_c : word 3 of `rhs'
			local var_d : word 4 of `rhs'
			display "Var 1 is `var_a'"
			display "Var 2 is `var_b'"
			display "Var 3 is `var_c'"
			display "Var 4 is `var_d'"
			mmerge date_m using "$fcast/`lhs'_`rhs_filename'_`curr'.dta", umatch(end)
			tsset date_m
			rename _b_cons cons
			
			* Generate forecast versus actual
			gen actual=f.`lhs'
			if "`var_b'" == "" { 
				gen forecast=cons + _b_`var_a' *f.`var_a'
			}
			else if "`var_c'" == "" {
				gen forecast=cons + _b_`var_a' *f.`var_a' + _b_`var_b' *f.`var_b' 
			}
			else if "`var_d'" == "" {
				gen forecast=cons + _b_`var_a' *f.`var_a' + _b_`var_b' *f.`var_b' + _b_`var_c' *f.`var_c' 
			}
			else if "`var_d'" != "" {
				gen forecast=cons + _b_`var_a' *f.`var_a' + _b_`var_b' *f.`var_b' + _b_`var_c' *f.`var_c' + _b_`var_d' *f.`var_d' 
			}	
			gen rw=0
			
			* Generate forecast and rw RMSE and MAE	

			gen rw_mse=actual^2
			gen rw_mae=abs(actual)
			gen fcast_mse=(forecast-actual)^2
			gen fcast_mae=abs(forecast-actual)
			
			* Rolling $windowsize forecast 
			sort date
			gen roll_dm_mse_pval = .
			gen roll_dm_mae_pval = .
			
			foreach measure_name in rw_mse rw_mae fcast_mse fcast_mae {
				gen `measure_name'_sum_temp = sum(`measure_name')
				gen `measure_name'_sum_rolling = `measure_name'_sum[_n] - `measure_name'_sum[_n-`window']
				drop `measure_name'_sum_temp
			}
			
			* Rolling $windowsize ratios
			gen roll_ratio_rmse = (fcast_mse_sum_rolling)^(1/2)/(rw_mse_sum_rolling)^(1/2) 
			gen roll_ratio_mae = (fcast_mae_sum_rolling)/(rw_mae_sum_rolling) 
			drop *_sum_rolling
			
			keep if actual~=. & forecast~=.

			* Rolling $windowsize DM p-values
			levels date, local(datevalues)
			foreach enddate of local datevalues {
				quietly count if date_m > `enddate'-`window' & date_m <= `enddate' & !missing(actual) & !missing(forecast)
				if `r(N)' == `window' {
					cap dmariano actual forecast rw if date_m > `enddate'-`window' & date_m <= `enddate', crit(MSE) kernel(Bartlett)
					replace roll_dm_mse_pval=r(p) if date_m == `enddate'
					cap dmariano actual forecast rw if date_m > `enddate'-`window' & date_m <= `enddate', crit(MSE) // if missing, change kernel
					replace roll_dm_mse_pval=r(p) if date_m == `enddate' & missing(roll_dm_mse_pval)
					cap dmariano actual forecast rw if date_m > `enddate'-`window' & date_m <= `enddate', crit(MAE) kernel(Bartlett) 
					replace roll_dm_mae_pval=r(p) if date_m == `enddate'
					cap dmariano actual forecast rw if date_m > `enddate'-$mr_rollsizem & date_m <= `enddate', crit(MAE) // if missing, change kernel
					replace roll_dm_mae_pval=r(p) if date_m == `enddate' & missing(roll_dm_mae_pval)
				}
			}

			if "`exclusion'"=="excrisis" {
				drop if date >= $crisisstartm & date <= $crisisendm
			}

			*DIEBOLD MARIANO TEST USING 2006 onwards sample
			cap dmariano actual forecast rw if date_m>=$mrevalstart+1, crit(MSE)
			local s1_dm_mse_u=r(s1)
			local p_dm_mse_u=r(p)
			cap dmariano actual forecast rw if date_m>=$mrevalstart+1, crit(MAE)
			local s1_dm_mae_u=r(s1)
			local p_dm_mae_u=r(p)
			cap dmariano actual forecast rw if date_m>=$mrevalstart+1, crit(MSE) kernel(bartlett)
			local s1_dm_mse_b=r(s1)
			local p_dm_mse_b=r(p)
			cap dmariano actual forecast rw if date_m>=$mrevalstart+1, crit(MAE)  kernel(bartlett)
			local s1_dm_mae_b=r(s1)
			local p_dm_mae_b=r(p)
			
			foreach x in rw_mse rw_mae fcast_mse fcast_mae {
				egen `x'_sum=sum(`x') if date_m>=$mrevalstart
			}
			gen rw_rmse=(rw_mse_sum/$NQ)^(1/2)
			gen fcast_rmse=(fcast_mse_sum/$NQ)^(1/2)

			replace rw_mae=(rw_mae_sum/$NQ)
			replace fcast_mae=(fcast_mae_sum/$NQ)

			foreach x in fcast_rmse rw_rmse rw_mae fcast_mae {
				summ `x', detail
				local `x'_temp=r(p50)
			}
			local ratio=`fcast_rmse_temp'/`rw_rmse_temp'
			local ratio=round(`ratio',.0001) 
			if "`exclusion'"=="allperiods" {
				save "$fcast/fcast_`lhs'_`rhs_filename'_`curr'.dta", replace
			}
			if "`exclusion'"=="allperiods" {
				twoway (line actual date_m, lcolor(blue) lpattern(solid)) (line forecast date_m, lcolor(red) lpattern(dash)) (line rw date_m, lcolor(grey) lpattern(dash)), name("bd", replace) legend(order(1 "Actual FX" 2 "Forecast FX")) ytitle("Log Change") xtitle("") graphregion(color(white))
				graph export  "$graphs/mr_`lhs'_`rhs_filename'_`window'.eps", replace
				if "`rhs_filename'"=="d_ln_vxo" {
					twoway (line actual date_m if date_m>=$crisisstartm, lcolor(blue) lpattern(solid)) (line forecast date_m if date_m>=$crisisstartm, lcolor(red) lpattern(dash)) (line rw date_m if date_m>=$crisisstartm, lcolor(grey) lpattern(dash)), name("bd", replace) legend(order(1 "Actual FX" 2 "Forecast FX")) ytitle("Log Change") xtitle("Quarter") graphregion(color(white))
					graph export  "$graphs/mr_`lhs'_`rhs_filename'_`window'.eps", replace
				}
			}
			clear
			gen var1=""
			set obs 2
			gen S_1=.
			gen pval=.
			replace var1="MAE" if _n==1
			replace var1="MSE" if _n==2
			replace S_1=`s1_dm_mse_b' if var1=="MSE"
			replace pval=`p_dm_mse_b' if var1=="MSE"
			replace S_1=`s1_dm_mae_b' if var1=="MAE"
			replace pval=`p_dm_mae_b' if var1=="MAE"

			gen Forecast=.
			gen RW=.
			replace Forecast=`fcast_rmse_temp' if var1=="MSE"
			replace RW=`rw_rmse_temp' if var1=="MSE"
			replace Forecast=`fcast_mae_temp' if var1=="MAE"
			replace RW=`rw_mae_temp' if var1=="MAE"
			gen ratio=Forecast/RW
			export excel using "$regs/mr_`lhs'_`rhs_filename'_eval_`exclusion'.xlsx", firstrow(variables) replace
		}
	}
	restore
	clear

	* GRAPH ALL OOS RATIOS
	local lhs="d_log_s_eq_wgt"
	local rhslist ""d_ln_vxo" "d_log_spx" "d_gz_spread" "d_gf" "d_dis" "hkm""

	foreach rhs of local rhslist {
		local rhs_filename = subinstr("`rhs'"," ","_",.) 
		local rhs_filename = subinstr("`rhs_filename'",".","_",.)

		append using "$fcast/fcast_`lhs'_`rhs_filename'_`curr'.dta"
			if "`rhs_filename'" == "d_ln_vxo" {
			gen fcast_type="d_ln_vxo"
		}
		replace fcast_type = "`rhs_filename'" if missing(fcast_type)
	}
	order roll* fcast_type
	preserve
		keep date_m fcast_type roll_*
		keep date_m fcast_type *mse*
		separate roll_dm_mse_pval, by(fcast_type)
		drop roll_dm_mse_pval
		bysort fcast_type (date_m): gen last=1 if _n==_N
		graph twoway (scatter roll_dm_mse_pval* roll_ratio_rmse if inrange(roll_ratio_rmse, .625, 1.325) & month(dofm(date_m))==12 & date_m < tm(2008m1), mcolor(forest_green%90 sienna%90 blue%90 dkorange%90 ltblue%90 purple%90) ///
			 msymbol(x x x x x x x) ytitle("p-value (Diebold-Mariano)") xtitle("RMSE Ratio") graphregion(color(white)) ///
			 xscale(r(0.625 1.375)) xlabel(0.625(0.125)1.375) ylabel(0.05 0.1 0.5 1, angle(0)) ///
			 xline(1, lcolor(black%50) lpattern(dash)) yline(0.05 0.1, lcolor(black%50) lpattern(dash))) ///
			 (scatter roll_dm_mse_pval* roll_ratio_rmse if inrange(roll_ratio_rmse, .625, 1.325) & month(dofm(date_m))==12 & inrange(date_m, tm(2008m1), tm(2018m11)), mcolor(forest_green%90 sienna%90 blue%90 dkorange%90 ltblue%90 purple%90) ///
			 msymbol(oh oh oh oh oh oh oh)) ///
			 (scatter roll_dm_mse_pval* roll_ratio_rmse if last==1, mcolor(forest_green%90 sienna%90 blue%90 dkorange%90 ltblue%90 purple%90)), ///
			 legend(order(14 "Global Factor" 15 "GZ Spread" 16 "VXO" 17 "S&P500" 13 "Treasury Premium" 18 "Intermediary Returns") rows(2) size(3) region(lstyle(none) col(white)))
		graph export "$graphs/mr_scatter_monthlyrisk_mse_`lhs'_`window'.eps", replace
	restore
		keep date_m fcast_type roll_*
		keep date_m fcast_type *mae*
		separate roll_dm_mae_pval, by(fcast_type)
		drop roll_dm_mae_pval
		bysort fcast_type (date_m): gen last=1 if _n==_N
		graph twoway (scatter roll_dm_mae_pval* roll_ratio_mae if inrange(roll_ratio_mae, .625, 1.325) & month(dofm(date_m))==12 & date_m < tm(2008m1), mcolor(forest_green%90 sienna%90 blue%90 dkorange%90 ltblue%90 purple%90) ///
			 msymbol(x x x x x x x) ytitle("p-value (Diebold-Mariano)") xtitle("MAE Ratio") graphregion(color(white)) ///
			 xscale(r(0.625 1.375)) xlabel(0.625(0.125)1.375) ylabel(0.05 0.1 0.5 1, angle(0)) ///
			 xline(1, lcolor(black%50) lpattern(dash)) yline(0.05 0.1, lcolor(black%50) lpattern(dash))) ///
			 (scatter roll_dm_mae_pval* roll_ratio_mae if inrange(roll_ratio_mae, .625, 1.325) & month(dofm(date_m))==12 & inrange(date_m, tm(2008m1), tm(2018m11)), mcolor(forest_green%90 sienna%90 blue%90 dkorange%90 ltblue%90 purple%90) ///
			 msymbol(oh oh oh oh oh oh oh)) ///
			 (scatter roll_dm_mae_pval* roll_ratio_mae if last==1, mcolor(forest_green%90 sienna%90 blue%90 dkorange%90 ltblue%90 purple%90)), ///
			 legend(order(14 "Global Factor" 15 "GZ Spread" 16 "VXO" 17 "S&P500" 13 "Treasury Premium" 18 "Intermediary Returns") rows(2) size(3) region(lstyle(none) col(white)))
		graph export "$graphs/mr_scatter_monthlyrisk_mae_`lhs'_`window'.eps", replace
	clear
	
}


*************************
*** 9. TABLE: Compare BoP (1973-2017), BOP: 2007-2017 v Morningstar 2007-2017: results
*************************
capture confirm file "$user_dir/data/output/aggregates/global_Q_regression_plus.dta"
if _rc==0 {
	use if depvar=="USD" using "$user_dir/data/output/allmerged.dta", clear
	cap rm "$regs/final/table3.xls"
	cap rm "$regs/final/table3.txt"

	mmerge date depvar using "$user_dir/data/output/aggregates/global_Q_regression_plus.dta", uname(lmns_) unmatch(master)

	reg d_e_eq_wgt f_B_x_Om_i_ni if date_q >= tq(1977q1) & date_q <= tq(2006q4), r
	outreg2 using "$regs/final/table3.xls", ctitle("BoP") auto(2)
	reg d_e_eq_wgt f_B_x_Om_i_ni if date_q >= $startq & date_q <= tq(2017q4), r
	outreg2 using "$regs/final/table3.xls", ctitle("BoP post-2007") auto(2)
	reg d_e_eq_wgt lmns_f_B_x_Om_i_ni if date_q >= $startq & date_q <= tq(2017q4), r
	outreg2 using "$regs/final/table3.xls", ctitle("LMNS post-2007") auto(2)
	cap rm "$regs/final/table3.txt"
}

*************************
*** 10. TABLE: Individual exchange rates on flows unrelated to that country and specifically to that country
*************************

capture confirm file "$user_dir/data/output/aggregates/split_Q_regression_plus.dta"
if _rc==0 {
	use "$user_dir/data/output/aggregates/split_Q_regression_plus.dta", clear

	local lhs_set = `"d_e d_e_eq_wgt d_e_eq_wgt_exusd d_broad_dollar"'
	foreach exrate in `lhs_set' {
		cap erase "$regs/unformatted/us_retrench_`exrate'.txt"
		cap erase "$regs/unformatted/us_retrench_`exrate'.xls"
		cap erase "$regs/unformatted/us_retrench_inflow_us_`exrate'.txt"
		cap erase "$regs/unformatted/us_retrench_inflow_us_`exrate'.xls"

		* U.S. retrench only
		levels depvar, local(valuescurrency)
		foreach cur of local valuescurrency {
			if !("`cur'" == "USD") {
				reg `exrate' f_B_x_ni_ni_niUSA if date_q>=$startq & date_q<=$endq & depvar=="`cur'", r
				outreg2 using  "$regs/unformatted/us_retrench_`exrate'.xls", ctitle("`cur'") dta auto(2)
			}
		}
		
		* U.S. retrench and U.S. inflow to country		
		levels depvar, local(valuescurrency)
		foreach cur of local valuescurrency {
			if !("`cur'" == "USD") {
				reg `exrate' f_B_x_ni_ni_niUSA f_B_x_Om_ni_iUSA if date_q>=$startq & date_q<=$endq & depvar=="`cur'", r
				outreg2 using  "$regs/unformatted/us_retrench_inflow_us_`exrate'.xls", ctitle("`cur'") dta auto(2)
			}
		}
	}

	use "$regs/unformatted/us_retrench_d_e_dta.dta", clear
	append using "$regs/unformatted/us_retrench_inflow_us_d_e_dta.dta"
	sxpose, clear
	keep _var2 _var4 _var9 _var10 _var16 _var18 _var24
	replace _var16="us_to_unrelated" if _n==1
	replace _var24="R-squareds" if _n==1
	renvars, map(strtoname(@[1]))
	drop if _n==1
	destring R_squared*, replace
	gen expander = 0
	replace expander = 2 if _n==_N
	expand expander
	drop expander
	replace VARIABLES="Average" if _n==_N
	replace f_B_x_ni_ni_niUSA="" if _n==_N
	replace us_to_unrelated="" if _n==_N
	replace Observations="" if _n==_N
	replace f_B_x_Om_ni_iUSA="" if _n==_N
	replace R_squared=. if _n==_N
	replace R_squareds=. if _n==_N
	sum R_squared
	replace R_squared= r(mean) if _n==_N
	sum R_squareds
	replace R_squareds= r(mean) if _n==_N
	gen diff = R_squareds - R_squared
	save "$regs/unformatted/us_retrench_inflow", replace emptyok

	drop if VARIABLES == "ARS"
	outsheet using "$regs/final/table4.xls", replace
}

*************************
*** 11: TABLE: Broad Dollar and Subcomponents of outward flows (Corp/Sov x Dollar/NonDollar)
*************************

capture confirm file "$user_dir/data/output/aggregates/split_Q_regression_plus.dta"
if _rc==0 {

	use if depvar=="USD" using "$user_dir/data/output/aggregates/global_Q_regression_plus.dta", clear
	cap rm "$regs/final/table5.xls"
	cap rm "$regs/final/table5.txt"

	reg d_e_eq_wgt f_BC_x_i_i_ni f_BC_x_ni_i_ni f_BS_x_i_i_ni f_BS_x_ni_i_ni if depvar=="USD" & date_q>=$startq, r
	outreg2 using "$regs/final/table5.xls", ctitle("BD") auto(2)
	reg d_e_eq_wgt f_BC_x_i_i_ni if depvar=="USD" & date_q>=$startq, r
	outreg2 using "$regs/final/table5.xls", ctitle("BD") auto(2)
	reg d_e_eq_wgt f_BC_x_ni_i_ni if depvar=="USD" & date_q>=$startq, r
	outreg2 using "$regs/final/table5.xls", ctitle("BD") auto(2)
	reg d_e_eq_wgt f_BS_x_i_i_ni if depvar=="USD" & date_q>=$startq, r
	outreg2 using "$regs/final/table5.xls", ctitle("BD") auto(2)
	reg d_e_eq_wgt f_BS_x_ni_i_ni if depvar=="USD" & date_q>=$startq, r
	outreg2 using "$regs/final/table5.xls", ctitle("BD") auto(2)
}
	
*************************
*** 12. TABLE: U.S. Domestic Investment flows and ER
*************************

capture confirm file "$user_dir/data/output/aggregates/split_Q_regression_plus.dta"
if _rc==0 {

	cap rm "$regs/final/table6.xls"
	cap rm "$regs/final/table6.txt"

	use "$user_dir/data/output/aggregates/split_Q_regression_plus.dta", clear	
	reg d_e_eq_wgt f_B_w_i_i_iUSA if depvar=="USD" & date_q>=$startq, r
	outreg2 using "$regs/final/table6.xls", auto(2)
	reg d_e_eq_wgt f_BS_w_i_i_iUSA if depvar=="USD" & date_q>=$startq, r
	outreg2 using "$regs/final/table6.xls", auto(2)
	reg d_e_eq_wgt f_BC_w_i_i_iUSA if depvar=="USD" & date_q>=$startq, r
	outreg2 using "$regs/final/table6.xls", auto(2)
	reg d_e_eq_wgt f_BS_w_i_i_iUSA f_BC_w_i_i_iUSA if depvar=="USD" & date_q>=$startq, r
	outreg2 using "$regs/final/table6.xls", auto(2)
	cap rm "$regs/final/table6.txt"

}

*************************
*** 13. TABLE: Broad USD Rsquare pre-and-post crisis using {bond/equity}x{in/out/net}
*************************

local typelist="B E D DB DE OL"
use "$user_dir/data/output/allmerged.dta", clear

foreach type of local typelist {
	gen GrossQ_`type'=(1/2)*(Q_`type'_x_Om_ni_i+Q_`type'_x_Om_i_ni)
	gen Net_`type'=F_`type'_x_Om_ni_i-F_`type'_x_Om_i_ni
	gen net_`type'=Net_`type'/l.GrossQ_`type'
	gen net_v2_`type'=f_`type'_x_Om_ni_i-f_`type'_x_Om_i_ni
	gen fout_`type'=f_`type'_x_Om_i_ni
	gen fin_`type'=f_`type'_x_Om_ni_i
}
drop Gr* Net*
save "$user_dir/data/temp/imf_bop/imf_bop_lmns_regression_q_ipolate_additional.dta", replace

use "$user_dir/data/temp/imf_bop/imf_bop_lmns_regression_q_ipolate_additional.dta", clear
keep if depvar=="USD"

cap erase "$regs/various_usd_outflows.txt"
cap erase "$regs/various_usd_outflows.xls"

foreach flow in net net_v2 fout fin {
	foreach type of local typelist {
		reg d_e_eq_wgt `flow'_`type' if depvar=="USD" & date_q<$startq & date_q>=tq(1977q1), r
		outreg2 using "$regs/various_usd_outflows.xls", ctitle("Pre `flow'_`type'") auto(2)
		reg d_e_eq_wgt `flow'_`type' if depvar=="USD" & date_q>=$startq & date_q<=$endq, r
		outreg2 using "$regs/various_usd_outflows.xls", ctitle("Post `flow'_`type'") auto(2)
	}
}

foreach type of local typelist {
	gen temp_`type'=date_q if net_`type'~=.
	egen start_bop_`type'=min(temp_`type')
}
keep depvar start*
keep if _n==1
save "$user_dir/data/temp/imf_bop/usa_start.dta", replace


foreach flow in net net_v2 fout fin {
	foreach type of local typelist {
		use "$user_dir/data/temp/imf_bop/imf_bop_lmns_regression_q_ipolate_additional.dta", clear	
		rolling _b _se r2=e(r2), window(40) clear : reg d_e_eq_wgt `flow'_`type' if depvar=="USD" & date_q<=$endq & date_q>=tq(1977q1)
		rename _b_`flow'_`type' `flow'_`type'_b
		rename _se_`flow'_`type' `flow'_`type'_se
		rename _eq2 `flow'_`type'_r2
		keep cid start end `flow'*
		save "$user_dir/data/temp/imf_bop/bop_rolling_USD_`flow'_`type'_d_e_eq_wgt.dta", replace
	}
}

clear 
use "$user_dir/data/temp/imf_bop/bop_rolling_USD_fin_B_d_e_eq_wgt.dta", clear
drop fin*
foreach flow in net net_v2 fout fin {
	foreach type of local typelist {
		mmerge cid start end using "$user_dir/data/temp/imf_bop/bop_rolling_USD_`flow'_`type'_d_e_eq_wgt.dta"
	}
}
gen depvar="USD"
mmerge depvar using "$user_dir/data/temp/imf_bop/usa_start.dta"
foreach type of local typelist {
	gen data_length_`type'=end-start_bop_`type'
}	
foreach type of local typelist {
	foreach flow in fin fout net net_v2 {
		foreach var in b se r2 {
			replace `flow'_`type'_`var'=. if data_length_`type'<40
		}
	}
}	

insheet using "$regs/various_usd_outflows.txt", clear
drop if _n==1 | _n==3 
keep v1-v5 v26-v29 v38-v41
drop if inrange(_n, 6, 25) | inrange(_n, 30, 37) | inrange(_n, 42, 49)
outsheet using "$regs/final/z_appendix_table2.xls", replace


*************************
*** 14. GRAPH: Broad Dollar and Purchases of Funds, Subdivided into Quintiles by Characteristics
***            (Foreign country specialization, foreign currency specialization, Size, Passiveness)
*************************

capture confirm file "$user_dir/data/output/aggregates/global_Q_regression_plus_qtile.dta"
if _rc==0 {

	clear all
	clear matrix
	set maxvar 10000
	use "$user_dir/data/output/aggregates/global_Q_regression_plus_qtile.dta", clear
	* drop bet wit res mis because otherwise the reshape command with F* Q* f* takes 100k characters when expanded which stata cannot handle
	* drop E e because stata cannot handle this many i variables with reshape. says "too many variables"
	drop cid *bet* *wit* *res* *mis* E e _merge

	replace qtile_type = "_q_ccy" if qtile_type == "foreign_ccy"
	replace qtile_type = "_q_iss" if qtile_type == "foreign_issuer"
	replace qtile_type = "_q_mv" if qtile_type == "mvusd"
	replace qtile_type = "_q_pss" if qtile_type == "passive"
	reshape wide F* Q* f* , i(depvar date_q d_* non_rep qtile_num) j(qtile_type) string
	reshape wide F* Q* f* , i(depvar date_q d_* non_rep) j(qtile_num)
	order *ccy*, after(non_rep)
	order *iss*, after(f_BS_x_Om_USA_ni_q_ccy5)
	order *mv*, after(f_BS_x_Om_USA_ni_q_iss5)
	order *pss*, after(f_BS_x_Om_USA_ni_q_mv5)
	drop *0
	encode depvar, gen(cid)
	drop if d_e == .
	tsset cid date_q 

	*** RUN REGRESSIONS STORE OUTPUT
	cap erase "$regs_quantiles/main_bd_quantiles.txt"
	cap erase "$regs_quantiles/main_bd_quantiles.xls"

	keep if depvar=="USD" & date >= $startq

	foreach rhs_var of varlist f_B_x_Om_i_ni* {
		reg d_broad_dollar `rhs_var', r
		outreg2 using "$regs_quantiles/main_bd_quantiles.xls", ctitle("`rhs_var'")
	}

	*** PLOT OUTPUT

	local graphlist "main_bd_quantiles"

	foreach graph_name of local graphlist {
		clear
		import delimited "$regs_quantiles/`graph_name'.txt"
		foreach var of varlist * {
			ren `var' `=strtoname(`var'[2])'
		}
		drop if _n>_N-2 | _n > _N-7 & _n < _N-2 | _n>= 1 & _n<=3
		destring f*, replace ignore("*()")
		count
		foreach i of numlist 1/`r(N)' {
			if (mod(`i',2)==1) replace VARIABLES="Beta" if _n==`i'
			if (mod(`i',2)==0) replace VARIABLES="SE" if _n==`i'
			if (`i'==`r(N)') replace VARIABLES="Rsquared" if _n==`i'
		}
		foreach var of varlist f* {
			di "`var'"
			foreach i of numlist 1/`r(N)' {
				if (mod(`i',2)==1) replace `var'=`var'[`i'] if _n==1 & missing(`var')
				if (mod(`i',2)==0) replace `var'=`var'[`i'] if _n==2 & missing(`var')
			}
		}
		keep if _n==1 | _n==2 | _n==_N
		xpose, clear varname
		ren (v1-v3) (Beta SE Rsquare)
		drop if _n==1
		encode _varname, gen(n_varname)
		gen Beta_2se_minus = Beta - 2*SE
		gen Beta_2se_plus = Beta + 2*SE
		generate qtile_type = regexs(1) if regexm(_varname, "^.*_(.*_.*)$")
		replace qtile_type = substr(qtile_type, 3, .)
		replace qtile_type = substr(qtile_type, 1, length(qtile_type)-1)
		replace qtile_type = "Foreign Currency Specialist" if qtile_type=="ccy"
		replace qtile_type = "Foreign Issuer Specialist" if qtile_type=="iss"
		replace qtile_type = "Fund Size" if qtile_type=="mv"
		replace qtile_type = "Passive" if qtile_type=="pss"
		label define qtile_label 1 "Quintile 1" ///
								 2 "Quintile 2" ///
								 3 "Quintile 3" ///
								 4 "Quintile 4" ///
								 5 "Quintile 5" ///
								 6 "Quintile 1" ///
								 7 "Quintile 2" ///
								 8 "Quintile 3" ///
								 9 "Quintile 4" ///
								 10 "Quintile 5" ///
								 11 "Quintile 1" ///
								 12 "Quintile 2" ///
								 13 "Quintile 3" ///
								 14 "Quintile 4" ///
								 15 "Quintile 5" ///
								 16 "Quintile 1" ///
								 17 "Quintile 2" ///						 
								 18 "Quintile 3" ///
								 19 "Quintile 4" ///
								 20 "Quintile 5" 
								 
		label values n_varname qtile_label

		levelsof qtile_type, local(qtile_categories) 
		local label_min 1
		local label_max = `label_min' + 4
		local graph_num = 1
		foreach qtile_category of local qtile_categories {
			twoway (scatter Beta n_varname) (rcap Beta_2se_minus Beta_2se_plus n_varname) if qtile_type=="`qtile_category'", yline(0, lpattern(dash) lcolor(gray)) ///
				xlabel(`label_min'(1)`label_max', valuelabel labsize(small)) xtitle("Rank") legend(off) graphregion(color(white)) ytitle("{&beta}") title("`qtile_category'") ///
				name(g`graph_num', replace)
			local label_min = `label_min'+5
			local label_max = `label_max'+5
			local graph_names "`graph_names' g`graph_num'"
			local graph_num `++graph_num'
		}

		graph combine `graph_names', col(2) graphregion(color(white))

		graph export "$graphs/coef_`graph_name'.eps", replace

		local graph_names ""
		levelsof qtile_type, local(qtile_categories) 
		local label_min 1
		local label_max = `label_min' + 4
		local graph_num = 1
		foreach qtile_category of local qtile_categories {
			twoway bar Rsquare n_varname if qtile_type=="`qtile_category'", yline(0, lpattern(dash) lcolor(gray)) yscale(range(0 0.5)) ylabel(0(0.1)0.5) ///
				xlabel(`label_min'(1)`label_max', valuelabel labsize(small)) xtitle("Rank") legend(off) graphregion(color(white)) ytitle("R{superscript:2}") title("`qtile_category'") ///
				name(g`graph_num', replace)
			local label_min = `label_min'+5
			local label_max = `label_max'+5
			local graph_names "`graph_names' g`graph_num'"
			local graph_num `++graph_num'
		}

		graph combine `graph_names', col(2) graphregion(color(white))

		graph export "$graphs/rsq_`graph_name'.eps", replace
	}
}
	
****************************************************************************
**************************** APPENDIX **************************************
****************************************************************************

*************************
*** A1&2: FIGURE: KEY RESULTS FOR 4Q HORIZON
*************************

use if depvar=="USD" using "$user_dir/data/output/allmerged.dta", clear

* Create 1Q to 4Q horizon variables for all lowercase f regressors, all LHS ex rate changes
ren f_* f_*_1Q
ren (i_diff pi_q_diff dy_q_diff) (i_diff_1Q pi_q_diff_1Q dy_q_diff_1Q)

ren d_e* d_e*_1Q 
foreach var of varlist f_* d_e* i_diff_1Q pi_q_diff_1Q dy_q_diff_1Q {
	foreach i of num 1/3 {
		local next_lag = `i'+1
		local _var = substr("`var'", 1, strrpos("`var'", "_")) + "`i'Q"
		local next_var = substr("`var'", 1, strrpos("`var'", "_")) + "`next_lag'Q"
		local last_var = substr("`var'", 1, strrpos("`var'", "_")) + "`i'Q"
		gen `next_var' = `last_var' + L`i'.`var'
	}
}
order d_e* f_* i_diff_1Q pi_q_diff_1Q dy_q_diff_1Q

* Figure 3
twoway (scatter d_e_eq_wgt_4Q f_B_x_Om_i_ni_4Q if date_q >$crisisend+3, msize(1) mcolor(black) mlabel(date_q) mlabcolor(black) mlabsize(2.5)) (lfit d_e_eq_wgt_4Q f_B_x_Om_i_ni_4Q, lcolor(black)) ///
	(lfit d_e_eq_wgt_4Q f_B_x_Om_i_ni_4Q if date_q >$crisisend+3, lcolor(red) lpattern(dash)) (scatter d_e_eq_wgt_4Q f_B_x_Om_i_ni_4Q if date_q <=$crisisend+3, msize(1) msymbol(square) mcolor(red) mlabel(date_q) mlabcolor(red) mlabsize(2.5)) if depvar=="USD" & date_q>=$startq, ///
	xtitle("U.S. Foreign Bond Purchases") ytitle("Change in Broad Dollar") xlabel(-.15(.05).25) graphregion(color(white)) ///
	legend(order(2 "2007:Q1 - 2019:Q2" 3 "Ex-crisis: 2009:Q3 - 2019:Q2"))
graph export "$graphs/appendix/bd_flow_4Q.eps", replace

reg d_e_eq_wgt_1Q f_B_x_Om_i_ni_1Q if depvar=="USD" & date_q>=$startq, r
reg d_e_eq_wgt_1Q f_B_x_Om_i_ni_1Q if depvar=="USD" & date_q>$crisisend, r

* App: Figure 1 equivalent
replace i_diff_4Q = i_diff_4Q/4

twoway (scatter d_e_eq_wgt_4Q L.i_diff_4Q, color(black))  (lfit d_e_eq_wgt_4Q L.i_diff_4Q, lcolor(black) lpattern(dash))  if depvar=="USD" & date_q < $startq,  graphregion(color(white)) xtitle("Interest Rate Differential") ytitle("Change in Broad Dollar") legend(off)
graph export "$graphs/appendix/erd_i_pre_4Q.eps", replace
twoway (scatter d_e_eq_wgt_4Q pi_q_diff_4Q, color(black)) (lfit d_e_eq_wgt_4Q pi_q_diff_4Q, lcolor(black) lpattern(dash)) if depvar=="USD" & date_q < $startq, graphregion(color(white)) xtitle("Inflation Differential") ytitle("Change in Broad Dollar") legend(off)
graph export "$graphs/appendix/erd_pi_pre_4Q.eps", replace
twoway (scatter d_e_eq_wgt_4Q dy_q_diff_4Q, color(black)) (lfit d_e_eq_wgt_4Q dy_q_diff_4Q, lcolor(black) lpattern(dash)) if depvar=="USD" & date_q < $startq, graphregion(color(white)) xtitle("GDP Growth Rate Differential") ytitle("Change in Broad Dollar") legend(off)
graph export "$graphs/appendix/erd_y_pre_4Q.eps", replace
twoway (scatter d_e_eq_wgt_4Q f_B_x_Om_i_ni_4Q, color(black)) (lfit d_e_eq_wgt_4Q f_B_x_Om_i_ni_4Q, lcolor(black) lpattern(dash)) if depvar=="USD" & date_q < $startq, graphregion(color(white)) xtitle("U.S. Foreign Bond Purchases") ytitle("Change in Broad Dollar") legend(off)
graph export "$graphs/appendix/erd_bopcf_pre_4Q.eps", replace

*************************
*** A3: FIGURE: OOS
*************************

* See section 8

*************************
*** A4. FIGURE: How correlated is f_B_x_Om_i_ni between LMNS and BoP
*************************

capture confirm file "$user_dir/data/output/aggregates/global_Q_regression_plus.dta"
if _rc==0 {

	use if depvar=="USD" using "$user_dir/data/output/allmerged.dta", clear
	mmerge date depvar using "$user_dir/data/output/aggregates/global_Q_regression_plus.dta", uname(lmns_)  unmatch(master)
	tsline f_B_x_Om_i_ni lmns_f_B_x_Om_i_ni if date_q >= tq(2005q1), ///
		lpattern("solid" "dash") graphregion(color(white)) xtitle("") ytitle("%") ///
		legend(order(1 "IMF" 2 "Morningstar") rows(1))
		graph export "$graphs/appendix/compare_imf_mstar.eps", replace

}

