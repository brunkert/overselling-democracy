
* This do-file replicates all information displayed in the appendix

		use "overselling_data", clear

		center overselling_min

////////////////////////////////////////////////////////////////////////////////
********************************** APPENDIX ************************************
////////////////////////////////////////////////////////////////////////////////


*_______________________________________________________________________________
* APPENDIX TABLE 1 Countrywave table
	sort country_name
	asdoc table country_name S002, save(countrywaves.doc) replace
	
*_______________________________________________________________________________
* APPENDIX TABLE 2 descriptives table
	
	asdoc fsum democ_weight2 c_overselling_min c_lnd_anti_and c_education_n ///
	c_incomescales c_pol_interest c_trust_media c_financesatis c_evi age ///
	v2xnp_client_n regimeduration censorship_n PTS_S evi_m loggdp libdem_stock45_log, ///
	uselabel addstats(p25 p50 p75) save(desc_stats.doc) replace

	
*_______________________________________________________________________________
* APPENDIX FIGURE 1 & 2

		use "overselling_data", clear
		
		keep if !missing(democ_weight2,overselling_min,lnd_anti_and, /// 
		education_n,incomescales,pol_interest,trust_media,financesatis,evi,age, /// 
		v2xnp_client_n,regimeduration,censorship_n,PTS_S,evi_m,loggdp,libdem_stock45_log,S002)

		drop if missing(overselling_min, lnd_anti_and, democ_weight2)

		egen ccode5 = group(overselling_min S024) if S002 == 5
		egen ccode6 = group(overselling_min S024) if S002 == 6
		egen ccode7 = group(overselling_min S024) if S002 == 7
		
		decode S024, gen(countrywave_string)
		labmask ccode5, values(countrywave_string)
		labmask ccode6, values(countrywave_string)
		labmask ccode7, values(countrywave_string)
		
		tab ccode5, nolab
		
		* WITH SD INSTEAD OF CI
		foreach var of varlist lnd_anti_and democ_weight2 {
		egen sd_`var' = sd(`var'), by(S024)
		egen mean_`var' = mean(`var'), by(S024) 
		gen u_`var' = mean_`var' + sd_`var' 
		gen l_`var' = mean_`var' - sd_`var'
		}
		
		// wave 5
		levelsof ccode5 if S002 == 5, local(wave5) 		
		twoway  (scatter ccode5 overselling_min if S002 == 5, msymbol(Oh) mc(gs0)), ///
		scheme(s1mono) legend(off) ylab(`wave5', valuelabel ang(h)) name(overselling, replace) xline(0, lpattern(dash)) ///
		subtitle("Overselling", fcolor(gs2*0.2)) xtitle("", size(small)) note("") ///
		ytitle("") ylab(, grid glw(vthin) glc(gs12)) graphregion(margin(zero)) xscale(range(-.7 .7)) xla(#5, labsize(medium)) ysize(20) xsize(14) 
		
		twoway (rcap u_lnd_anti_and l_lnd_anti_and ccode5 if S002 == 5, horizontal) (scatter ccode5 mean_lnd_anti_and if S002 == 5, msymbol(Sh) mc(gs0)), ///
		scheme(s1mono) legend(off) name(lndand, replace) ///
		subtitle("LND anti AND +/- one SD", fcolor(gs2*0.2)) xtitle("", size(small)) note("") ///
		ytitle("") ylab(`wave5', valuelabel ang(h) grid glw(vthin) glc(gs12)) yscale(off) graphregion(margin(zero)) xscale(range(-.5 1)) xla(#5, labsize(medium)) ysize(20) xsize(14) nodraw
		
		twoway (rcap u_democ_weight2 l_democ_weight2 ccode5 if S002 == 5, horizontal) (scatter ccode5 mean_democ_weight2 if S002 == 5, msymbol(Th) mc(gs0)), ///
		scheme(s1mono) legend(off) name(democ, replace) ///
		subtitle("Democ. Eval. +/- one SD", fcolor(gs2*0.2)) xtitle("", size(small)) note("") ///
		ytitle("") ylab(`wave5', valuelabel ang(h) grid glw(vthin) glc(gs12)) yscale(off) graphregion(margin(zero)) xscale(range(0 1)) xla(#5, labsize(medium)) ysize(20) xsize(14) nodraw
		
		graph combine overselling lndand democ, row(1) altshrink name(wave5, replace) //graphregion(margin(zero))
		graph export "wave5.tif", replace width(2000) as(tif)

		// Wave 7
		levelsof ccode7 if S002 == 7, local(wave7) 		
		twoway  (scatter ccode7 overselling_min if S002 == 7, msymbol(Oh) mc(gs0)), ///
		scheme(s1mono) legend(off) ylab(`wave7', valuelabel ang(h)) name(overselling, replace) xline(0, lpattern(dash)) ///
		subtitle("Overselling", fcolor(gs2*0.2)) xtitle("", size(small)) note("") ///
		ytitle("") ylab(, grid glw(vthin) glc(gs12)) graphregion(margin(zero)) xscale(range(-.7 .7)) xla(#5, labsize(medium)) ysize(20) xsize(14) nodraw
		
		twoway (rcap u_lnd_anti_and l_lnd_anti_and ccode7 if S002 == 7, horizontal) (scatter ccode7 mean_lnd_anti_and if S002 == 7, msymbol(Sh) mc(gs0)), ///
		scheme(s1mono) legend(off) name(lndand, replace) ///
		subtitle("LND anti AND +/- one SD", fcolor(gs2*0.2)) xtitle("", size(small)) note("") ///
		ytitle("") ylab(`wave7', valuelabel ang(h) grid glw(vthin) glc(gs12)) yscale(off) graphregion(margin(zero)) xscale(range(-.5 1)) xla(#5, labsize(medium)) ysize(20) xsize(14) nodraw
		
		twoway (rcap u_democ_weight2 l_democ_weight2 ccode7 if S002 == 7, horizontal) (scatter ccode7 mean_democ_weight2 if S002 == 7, msymbol(Th) mc(gs0)), ///
		scheme(s1mono) legend(off) name(democ, replace) ///
		subtitle("Democ. Eval. +/- one SD", fcolor(gs2*0.2)) xtitle("", size(small)) note("") ///
		ytitle("") ylab(`wave7', valuelabel ang(h) grid glw(vthin) glc(gs12)) yscale(off) graphregion(margin(zero)) xscale(range(0 1)) xla(#5, labsize(medium)) ysize(20) xsize(14) nodraw
		
		graph combine overselling lndand democ, row(1) altshrink name(wave7, replace) //graphregion(margin(zero))
		graph export "wave7.tif", replace width(2000) as(tif)


*_______________________________________________________________________________
* CONNECTED LINE GRAPHS FOR MAIN VARIABLES: FULL SAMPLE (not included in appendix - too long)
		/*
		use "overselling_data", clear
		keep if !missing(democ_weight2,overselling_min,lnd_anti_and, /// 
		education_n,incomescales,pol_interest,trust_media,financesatis,evi,age, /// 
		v2xnp_client_n,regimeduration,censorship_n,PTS_S,evi_m,loggdp,libdem_stock45_log,S002)

		collapse polyarchy_min_n overselling_min lnd_anti_and democ_weight2, by(S024 COWcode S002)
		drop if missing(overselling_min) | missing(democ_weight2) | missing(lnd_anti_and)
		
		gen var1 = overselling_min
		gen var2 = lnd_anti_and
		gen var3 = democ_weight2
		*gen var4 = polyarchy_min_n
		xtile overselling_cat = overselling_min, nquantiles(3)
		
		reshape long var, i(S024) j(values)
		drop overselling_min lnd_anti_and democ_weight2
		
		label define values 1 "Overselling" 2 "LND vs. AND" 3 "Democ. Eval."
		label values values values		
		sort S024
			
		separate var, by(S002)
		label var var5 "5th wave"
		label var var6 "6th wave"
		label var var7 "7th wave"	
		
		levelsof overselling_cat, local(os)
		foreach i of local os {
		preserve
		keep if overselling_cat == `i'
		sort S024
		twoway connected var5 values if overselling_cat == `i' || ///
		connected var6 values if overselling_cat == `i' || ///
		connected var7 values if overselling_cat == `i', by(COWcode, note("")) yline(0) scheme(lean1) xlabel(1 "Overselling" 2 "LND vs. AND" 3 "Democ. Eval.", valuelabel angle(90)) ///
		legend(pos(6) row(1) size(2)) name(g`i', replace) aspectratio(1) xtitle("") title(,size(1))
		restore
		}
		*/
		
********************************************************************************
/////////////////// REPLICATION AND ROBUSTNESS CHECKS //////////////////////////
********************************************************************************

use "overselling_data", clear

		center overselling_min
		
		svyset S024 [pweight = S017]		
		
		est clear
*_______________________________________________________________________________
* TABLE 3 MODEL 1: Last wave only for each country
		preserve
		bys COWcode: egen maxwave = max(S002)
		keep if S002 == maxwave
		
		eststo: mixed democ_weight2 c.c_overselling_min##c.c_lnd_anti_and  /// Main Variables
		c_education_n c_incomescales c_pol_interest c_trust_media c_financesatis c_evi c_age /// Individual Controls  -education
		v2xnp_client_n regimeduration censorship_n PTS_S evi_m loggdp libdem_stock45_log i.S002 /// Level 2 Controls
		[pweight = S017] || S024: c_lnd_anti_and, variance cov(un) 
		estadd scalar v1  = exp(2*[lns1_1_1]_b[_cons])
		estadd scalar v2  = exp(2*[lns1_1_2]_b[_cons])
		estadd scalar cov = tanh([atr1_1_1_2]_b[_cons]) * exp([lns1_1_1]_b[_cons]) * exp([lns1_1_2]_b[_cons])
		estadd scalar v_e = exp(2*[lnsig_e]_b[_cons])
		estat icc
		estadd scalar icc2 = r(icc2) 
		
		restore
		
*_______________________________________________________________________________
* TABLE 3 MODEL 2: without PTS > 3
		
		preserve
		drop if PTS_S > 3 
		eststo: mixed democ_weight2 c.c_overselling_min##c.c_lnd_anti_and  /// Main Variables
		c_education_n c_incomescales c_pol_interest c_trust_media c_financesatis c_evi c_age /// Individual Controls  -education
		v2xnp_client_n regimeduration censorship_n PTS_S evi_m loggdp libdem_stock45_log i.S002 /// Level 2 Controls
		[pweight = S017] || S024: c_lnd_anti_and, variance cov(un) 
		estadd scalar v1  = exp(2*[lns1_1_1]_b[_cons])
		estadd scalar v2  = exp(2*[lns1_1_2]_b[_cons])
		estadd scalar cov = tanh([atr1_1_1_2]_b[_cons]) * exp([lns1_1_1]_b[_cons]) * exp([lns1_1_2]_b[_cons])
		estadd scalar v_e = exp(2*[lnsig_e]_b[_cons])
		estat icc
		estadd scalar icc2 = r(icc2) 
		restore
		
		esttab using robust.rtf, replace b(3) se(3) keep(democ_weight2:) onecell ///
			aic bic eqlabels(none) varwidth(15) sfmt(4) ///
			scalars(icc2 N_clust "v1 var(RS: LND anti LND)" "v2 var(RI: Country-Wave)" "cov cov(RS, RI)" "v_e var(Residual)") obslast

		esttab using robustwide.rtf, replace b(3) se(3) keep(democ_weight2:) wide ///
			aic bic eqlabels(none) varwidth(15) sfmt(3) label ///
			scalars(icc2 N_clust "v1 var(RS: LND anti LND)" "v2 var(RI: Country-Wave)" "cov cov(RS, RI)" "v_e var(Residual)") obslast

		
*_______________________________________________________________________________
* MEDIATION ANALYSIS ROBUSTNESS CHECK
		
				
		stata2mplus S024 democ_weight2 c_overselling_min c_lnd_anti_and overselling_min lnd_anti_and c_education_n ///
		c_incomescales c_pol_interest c_trust_media c_financesatis c_evi c_age ///
		v2xnp_client_n regimeduration censorship_n PTS_S evi_m loggdp libdem_stock45_log S002 ///
		using mlmediation, replace
		
		// RUN THE MPLUS INPUTFILE TO REPLICATE THE MEDIATION ANALYSIS
