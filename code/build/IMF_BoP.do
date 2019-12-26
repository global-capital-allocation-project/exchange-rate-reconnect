
* This .do file prepares the raw IMF BoP data for use in analysis and merges in
* the exchange rate variables.

********************************************************************************
* MAKE CROSSWALK FOR IMF CODES
* Our coding denotes: whether the variable is a flow or stock (F/Q), the
* investment type (Bonds=B, Equities=E, etc.), and the source and destination
* country (i_ni=source country is "me", the country in the country field, and
* the destination is "not me", so every other country).
********************************************************************************

import excel "$user_dir/Data/raw/imf_bop/IMF_BOP_Codes.xlsx", sheet("toinclude") firstrow clear

rename ii* codeii*
rename bop* codebop*
reshape long code, i(lmns_id indicator_name) j(type) str

gen lmns_suffix="_x_Om_i_ni" if regexm(type,"asset")==1
replace lmns_suffix="_x_Om_ni_i" if regexm(type,"liability")==1
gen lmns_prefix="Q_" if regexm(type,"iip")==1
replace lmns_prefix="F_" if regexm(type,"bop")==1
gen lmns_not=lmns_prefix+lmns_id+lmns_suffix
keep code lmns_not
rename code indicator_code

save "$user_dir/Data/temp/imf_bop/lmns_imf_crosswalk.dta", replace

********************************************************************************
* IMPORT AND PREP RAW IMF DATA
********************************************************************************

import delimited "$user_dir/Data/raw/imf_bop/BOP_10-28-2019 21-23-30-18_timeSeries.csv", varnames(nonames) clear

save "$user_dir/Data/temp/imf_bop/imf_bop.dta", replace
use "$user_dir/Data/temp/imf_bop/imf_bop.dta", clear

rename v1 country
rename v2 ccode
rename v3 indicator_name
rename v4 indicator_code
rename v5 attribute
drop if regexm(indicator_name,", National Currency")==1
drop if regexm(indicator_name,", Euros")==1
cap describe
local var_num=r(k)-1
forvalues i=6/`var_num' {
	cap tostring v`i', replace
	local temp=v`i'[1]
	rename v`i' q`temp'
	destring q`temp', force replace
}
save "$user_dir/Data/temp/imf_bop/imf_bop_smaller.dta", replace

drop v*
drop if _n==1
compress
destring ccode, replace
mmerge ccode using "$user_dir/Data/raw/imf_bop/code_list.dta", umatch(ifscode) ukeep(wbcode)
keep if _merge==3
drop _merge
rename wbcode iso_country_code
replace iso_co="EMU" if iso_co=="EUR"

save "$user_dir/Data/temp/imf_bop/imf_bop_smaller.dta", replace

********************************************************************************
* SEPARATE QUARTERLY AND ANNUAL SERIES, GENERATE FLOW RATIOS
********************************************************************************

use "$user_dir/Data/temp/imf_bop/imf_bop_smaller.dta", clear

* Store first and last year covered in data as f_year and l_year respectively.

local i="first"
foreach x of varlist q* {
	if "`i'"=="first" {
		local f_year = substr("`x'",2,4)
		local i = "not"
	}
	local l_year = substr("`x'",2,4)
}

* Separate annual and quarterly time series. Apply crosswalk.

foreach freq in "y" "q" {
	use "$user_dir/Data/temp/imf_bop/imf_bop_smaller.dta", clear
	if "`freq'"=="q"{
		forvalues year=`f_year'/`l_year' {
			if `year' < `l_year' {
				drop q`year'
			}
		}
	}
	
	if "`freq'"=="y"{
		forvalues year=`f_year'/`l_year' {
			drop q`year'Q*
		}
	}


	keep if attribute=="Value"
	drop attribute
	mmerge indicator_code using "$user_dir/Data/temp/imf_bop/lmns_imf_crosswalk.dta"
	keep if _merge==3

  * Reshape data so that each observation is a country-period, variables are stocks and flows.

	order iso_country_code
	collapse (lastnm) q*, by(iso_country_code indicator_name lmns_not indicator_code)
	reshape long q, i(iso_country_code indicator_name indicator_code lmns_not) j(date_`freq') str
	rename q value
	if "`freq'"=="q"{
		gen month=""
		local q_to_months "1mar 2jun 3sep 4dec"
		foreach q_month of local q_to_months {
		    replace month = substr("`q_month'",2,3) if substr(date_q,6,1)==substr("`q_month'",1,1)
		}
		gen date = date("30"+month+substr(date_q,1,4), "DMY")
		drop date_q
		gen date_q=qofd(date)
		format date_q %tq
		drop month date
	}
	cap destring date_`freq', replace

	drop indicator*
	reshape wide value, i(iso_co date_`freq') j(lmns_not) str
	renpfix value

	encode iso_co, gen(cid)
	tsset cid date_`freq'

  * Calculate flow ratios.

	foreach x of varlist F* {
		local Q=subinstr("`x'","F","Q",.)
		local f=subinstr("`x'","F","f",.)
		gen `f'=`x'/L.`Q'
	}

  * Merge in currency variable and broad exchange rate.

	mmerge iso_country_code using "$user_dir/Data/raw/concordance/country_currencies_last.dta"
	keep if _merge==3
	drop _merge
	replace iso_curr="EUR" if iso_co=="EMU"

	mmerge iso_currency_code date_`freq' using "$user_dir/Data/raw/ER_data/log_exrate_eqwgt_`freq'.dta", ukeep(d_log_s_eq_wgt) unmatched(master)
	drop _merge
	rename d_log_s_eq_wgt d_e_eq_wgt
	rename iso_curr depvar

	save "$user_dir/Data/output/imf_bop/imf_bop_lmns_regression_`freq'.dta", replace
}

********************************************************************************
* INTERPOLATE TO ANNUAL SERIES, MERGE TO EXTEND QUARTERLY SERIES
********************************************************************************

* Interpolate annual stock values.

use "$user_dir/Data/output/imf_bop/imf_bop_lmns_regression_y.dta", clear
keep Q* iso_co date_y cid depvar
gen date_q=qofd(dofy(date_y)+364)
tsset cid date_q
tsfill
order cid date_q
foreach x of varlist Q* {
	bysort cid: ipolate `x' date_q , gen(i_`x')
	replace `x'=i_`x'
	drop i_`x'
}
keep cid date_q Q*

save "$user_dir/Data/temp/imf_bop/imf_bop_lmns_regression_ipolate.dta", replace

* Merge and recalculate flow ratio variables.

use "$user_dir/Data/output/imf_bop/imf_bop_lmns_regression_q.dta", clear
mmerge cid date_q using "$user_dir/Data/temp/imf_bop/imf_bop_lmns_regression_ipolate.dta", update
sort cid date_q
foreach x of varlist F* {
	local Q=subinstr("`x'","F","Q",.)
	local f=subinstr("`x'","F","f",.)
	replace `f'=`x'/L.`Q'
}

mmerge date_q depvar using "$user_dir/Data/raw/ER_data/market_fx_prices_q.dta", umatch(date_q iso_curr) ukeep(lcu_per_usd_SPOT_eop)
drop if _merge==2
sort cid date
gen e=log(lcu_per_usd_SPOT_eop)
gen d_e=e-l.e
drop if _merge==2
save "$user_dir/Data/output/imf_bop/imf_bop_lmns_regression_q_ipolate.dta", replace

