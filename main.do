
	* Data merging and variable creation not included here. Please consult author
	* for additional .do files if needed. 

		use "overselling_data", clear

////////////////////////////////////////////////////////////////////////////////
********************************** GRAPHS **************************************
////////////////////////////////////////////////////////////////////////////////

		
*_______________________________________________________________________________
* FIGURE 1: Having a democratic system, democracy important	
		
		preserve 
		collapse having_democ_n democ_important_n, by(S024)
		
		graph hbox having_democ_n democ_important_n, scheme(s1mono) yscale(range(0 1)) ylabel(0(.1)1) name(supportdemoc, replace) ///
		fysize(50) fxsize(150) legend(lab(1 "Having a Democratic System") lab(2 "Importance of Democracy") lab(3 "Overselling Democracy") size(2.5))
		
		graph export "supportdemoc.tif", replace as(tif) width(2000)
		restore
		
*_______________________________________________________________________________
* FIGURE 2: Cumulative frequencies of ‘democraticness of own country’ by levels of repression 

		stripplot democ_country, over(PTS_S) cumul cumprob box centre vertical iqr ///
		refline yla(, ang(h) labsize(3)) xtitle("") xla(, noticks labsize(3.5)) scheme(s1mono) /// xsc(titlegap(*5)) 
		ytitle("Democraticness of own country", size(3)) msymbol(oh) msize(1) name(publies2, replace) ///
		xtitle("Political Terror Scale", size(3))
		graph export publies2.tif, width(2000) as(tif) replace
		
*_______________________________________________________________________________
* FIGURE 3: ‘Overselling’ and ‘underselling’ democracy
			
		collapse v2exl_legitratio_n polyarchy_min_n, by(S024 S002)
		
		levelsof S002, local(wave)
		foreach i of local wave {
		preserve
		drop if S002 != `i'
		egen ml = mlabvpos(polyarchy_min_n v2exl_legitratio_n) if S002 == `i'
		twoway line polyarchy_min_n polyarchy_min_n || ///
		scatter v2exl_legitratio_n polyarchy_min_n if S002 == `i', /// yline(.5, lpattern(dot) lw(vthin)) xline(.5, lpattern(dot) lw(vthin)) ///
		scheme(s1mono) msize(1) mlab(S024) mlabsize(1.5) mlabvpos(ml) /// title("Rational-legal Claims and Procedural Democracy", size(3.5))
		ylab(.2(.2)1, labsize(3)) xlab(.2(.2)1, labsize(3)) name(g_`i', replace) ///
		xtitle("Procedural Democracy", size(3)) ytitle(, size(3)) legend(off) ///
		ytitle("Rational-legal Claims to Legitimacy", size(3)) ytitle(, size(3)) text(1 .6 "Overselling") text(.17 .6 "Underselling")
		graph export "ratio_poly_scatter_`i'.tif", replace as(tif) width(2000)
		restore
		}

*_______________________________________________________________________________
* FIGURE 4 Country-level values for main variables 
		
		use "overselling_data", clear
		
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
		
		// wave 6
		levelsof ccode6 if S002 == 6, local(wave6) 		
		twoway  (scatter ccode6 overselling_min if S002 == 6, msymbol(Oh) mc(gs0)), ///
		scheme(s1mono) legend(off) ylab(`wave6', valuelabel ang(h)) name(overselling, replace) xline(0, lpattern(dash)) ///
		subtitle("Overselling", fcolor(gs2*0.2)) xtitle("", size(small)) note("") ///
		ytitle("") ylab(, grid glw(vthin) glc(gs12)) graphregion(margin(zero)) xscale(range(-.7 .7)) xla(#5, labsize(medium)) ysize(20) xsize(14) nodraw
		
		twoway (rcap u_lnd_anti_and l_lnd_anti_and ccode6 if S002 == 6, horizontal) (scatter ccode6 mean_lnd_anti_and if S002 == 6, msymbol(Sh) mc(gs0)), ///
		scheme(s1mono) legend(off) name(lndand, replace) ///
		subtitle("LND anti AND +/- one SD", fcolor(gs2*0.2)) xtitle("", size(small)) note("") ///
		ytitle("") ylab(`wave6', valuelabel ang(h) grid glw(vthin) glc(gs12)) yscale(off) graphregion(margin(zero)) xscale(range(-.5 1)) xla(#5, labsize(medium)) ysize(20) xsize(14) nodraw
		
		twoway (rcap u_democ_weight2 l_democ_weight2 ccode6 if S002 == 6, horizontal) (scatter ccode6 mean_democ_weight2 if S002 == 6, msymbol(Th) mc(gs0)), ///
		scheme(s1mono) legend(off) name(democ, replace) ///
		subtitle("Democ. Eval. +/- one SD", fcolor(gs2*0.2)) xtitle("", size(small)) note("") ///
		ytitle("") ylab(`wave6', valuelabel ang(h) grid glw(vthin) glc(gs12)) yscale(off) graphregion(margin(zero)) xscale(range(0 1)) xla(#5, labsize(medium)) ysize(20) xsize(14) nodraw
		
		graph combine overselling lndand democ, row(1) altshrink name(wave6, replace) //graphregion(margin(zero))
		graph export "wave6.tif", replace width(2000) as(tif)

		
*_______________________________________________________________________________
* FIGURE 5 IV, moderator and DV – country-wave mean examples 

		preserve
		use "overselling_data", clear
		collapse overselling_min lnd_anti_and democ_weight2, by(S024 COWcode S002)
		drop if missing(overselling_min) | missing(democ_weight2) | missing(lnd_anti_and) 
		
		
		gen var1 = overselling_min
		gen var2 = lnd_anti_and
		gen var3 = democ_weight2
		xtile overselling_cat = overselling_min, nquantiles(3)
		
		reshape long var, i(S024) j(values)
		
		decode COWcode, gen(cname)
		keep if cname == "Trinidad and Tobago" | cname == "New Zealand" | cname == "Argentina" | | cname == "Germany" | ///
		cname == "Belarus" | cname == "Indonesia" | cname == "Russia" | cname == "Iran" | ///
		cname == "Vietnam" | cname == "China" | cname == "Rwanda" |  cname == "Jordan" 
		
		label define values 1 "Overselling" 2 "LND-vs-AND" 3 "Democ. Eval."
		label values values values // HAHA		
		sort overselling_cat overselling_min values S024 
			
		separate var, by(S002)
		label var var5 "5th wave"
		label var var6 "6th wave"
		label var var7 "7th wave"
		
		* replace for ordering
		replace COWcode = 1 if COWcode == 920
		replace COWcode = 2 if COWcode == 52
		replace COWcode = 3 if COWcode == 160
		replace COWcode = 4 if COWcode == 255
		replace COWcode = 5 if COWcode == 370
		replace COWcode = 6 if COWcode == 850
		replace COWcode = 7 if COWcode == 365
		replace COWcode = 8 if COWcode == 630
		replace COWcode = 9 if COWcode == 710
		replace COWcode = 10 if COWcode == 816
		replace COWcode = 11 if COWcode == 517
		replace COWcode = 12 if COWcode == 663
		
		labmask COWcode, values(cname)
		
		sort COWcode values
		twoway connected var5 values,  msize(small) || ///
		connected var6 values,  msize(small) || ///
		connected var7 values,  msize(small) ylab(,labsize(small)) by(COWcode, note("")) yline(0) scheme(lean1) xlabel(1 "Overselling" 2 "LND-vs-AND" 3 "Democ. Eval.", valuelabel angle(45) labsize(small)) ///
		legend(pos(6) row(1) size(2)) name(manuscriptgraphs, replace) aspectratio(1) subtitle(,size(small)) xtitle("")
		
		graph export "variable_examples.tif", replace width(2000) as(tif)
		restore


*_______________________________________________________________________________
* T-TEST INDONESIA EXAMPLE 
		use "overselling_data", clear
		
		preserve 
		keep if COWcode == 850
		ttest lnd_anti_and, by(S002)	
		ttest democ_weight2, by(S002)	
		restore
		
		
		
////////////////////////////////////////////////////////////////////////////////
**************************** MULTILEVEL MODELS *********************************
////////////////////////////////////////////////////////////////////////////////

		
use "overselling_data", clear
center overselling_min_n
		svyset S024 [pweight = S017]		
		
		est clear
		
*_______________________________________________________________________________
* MAIN MODELS

		est drop _all
		// 1
		eststo: mixed democ_weight2 c.c_overselling_min##c.c_lnd_anti_and  /// Main Variables
		[pweight = S017] || S024: c_lnd_anti_and, variance cov(un)
		estadd scalar v1  = exp(2*[lns1_1_1]_b[_cons])
		estadd scalar v2  = exp(2*[lns1_1_2]_b[_cons])
		estadd scalar cov = tanh([atr1_1_1_2]_b[_cons]) * exp([lns1_1_1]_b[_cons]) * exp([lns1_1_2]_b[_cons])
		estadd scalar ve = exp(2*[lnsig_e]_b[_cons])
		estat icc
		estadd scalar icc2 = r(icc2) 

		// 2 
		eststo: mixed democ_weight2 c.c_overselling_min##c.c_lnd_anti_and  /// Main Variables
		c_education_n c_incomescales c_pol_interest c_trust_media c_financesatis c_evi c_age /// Individual Controls 
		[pweight = S017] || S024: c_lnd_anti_and, variance cov(un)
		estadd scalar v1  = exp(2*[lns1_1_1]_b[_cons])
		estadd scalar v2  = exp(2*[lns1_1_2]_b[_cons])
		estadd scalar cov = tanh([atr1_1_1_2]_b[_cons]) * exp([lns1_1_1]_b[_cons]) * exp([lns1_1_2]_b[_cons])
		estadd scalar v_e = exp(2*[lnsig_e]_b[_cons])
		estat icc
		estadd scalar icc2 = r(icc2) 

		// 3
		eststo: mixed democ_weight2 c.c_overselling_min##c.c_lnd_anti_and  /// Main Variables
		c_education_n c_incomescales c_pol_interest c_trust_media c_financesatis c_evi c_age /// Individual Controls 
		v2xnp_client_n regimeduration censorship_n PTS_S evi_m loggdp libdem_stock45_log i.S002 /// Level 2 Controls
		[pweight = S017] || S024: c_lnd_anti_and, variance cov(un) 
		estadd scalar v1  = exp(2*[lns1_1_1]_b[_cons])
		estadd scalar v2  = exp(2*[lns1_1_2]_b[_cons])
		estadd scalar cov = tanh([atr1_1_1_2]_b[_cons]) * exp([lns1_1_1]_b[_cons]) * exp([lns1_1_2]_b[_cons])
		estadd scalar v_e = exp(2*[lnsig_e]_b[_cons])
		estat icc
		estadd scalar icc2 = r(icc2) 
				
		esttab using results.rtf, replace b(3) se(3) keep(democ_weight2:) onecell ///
			aic bic eqlabels(none) varwidth(15) sfmt(4) ///
			scalars(icc2 N_clust "v1 var(RS: LND anti LND)" "v2 var(RI: Country-Wave)" "cov cov(RS, RI)" "v_e var(Residual)") obslast

		esttab using resultswide.rtf, replace b(3) se(3) keep(democ_weight2:) wide ///
			aic bic eqlabels(none) varwidth(15) sfmt(3) label ///
			scalars(icc2 N_clust "v1 var(RS: LND anti LND)" "v2 var(RI: Country-Wave)" "cov cov(RS, RI)" "v_e var(Residual)") obslast

		
		margins, dydx(c_overselling_min) at(c_lnd_anti_and = (-1.6(.1)1))
		marginsplot
		marginsplot, name(andlnd, replace) scheme(s1mono) recastci(rarea) ciopts(color(gs10%50)) recast(line) yline(0, lpattern(dot)) ///
		title("Average Marginal Effects of 'Overselling'", size(3.5)) ylab(, labsize(2.5)) xlab(, labsize(2.5)) ///
		xtitle("LND vs. AND (centered)", size(3)) ytitle(, size(3))
		graph export "margins_democ.tif", replace width(2000) as(tif)


		
				
		
