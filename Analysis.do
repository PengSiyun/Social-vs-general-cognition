****Project: SNACK-SNAD
****Author:  Siyun Peng
****Date:    2023/01/11
****Purpose: general vs. social cognitive performance 



use "C:\Users\peng_admin\OneDrive - Indiana University\SNAD SHARED FOLDER\PUBLIC\SNAD Data\Cleaned data\SNAD-SNACK Merged Data\old\Short_merged_01_12_2023",clear

cd "C:\Users\peng_admin\Dropbox\peng\Academia\Work with Brea\SNAD\SNAD data\Peng\Social vs. general cognition\result"

rename FaceMem_CR Office_*,lower
keep if source_study==1 //use SNACK
drop residual



/*

***************************************************************
//	#1 Social cognition: Latent variable analysis
***************************************************************
/*Affective theory of mind uses Office_emotion and Office_fauxpas. Cognitive theory of mind uses Office_belief, Office_motiv, and Office_deceit. These could be averaged or potentially factorized.  For what it is worth, I believe the single-factor solution will probably be best! This is based on what I have done with other analyses as well as some poking around I just did with our data.
*/

/*EFA*/

*ToM separated (drop office_control, office_seenbefore, office_familiarity)
factor office_deceit office_emotion office_faux office_infer office_motiv,ipf
screeplot
factor  office_deceit office_emotion office_faux office_infer office_motiv facemem_cr,ml

factor office_deceit office_emotion office_faux office_infer office_motiv facemem_cr,ipf factor(1)


*ToM total
factor office_total facemem_cr,ipf factor(1)


/*CFA*/

*ToM+face memory
sem (SC -> office_deceit office_emotion office_faux office_infer office_motiv facemem_cr) ///
    , var(SC@1) method(mlmv) cov(e.office_emotion*e.facemem_cr)
estat gof, stats(all)
estat mindices
predict residual,latent(SC) 
egen soc_cog=std(residual) 
label var soc_cog "Social cognition"
drop residual

*ToM only
sem (SC -> office_deceit office_emotion office_faux office_infer office_motiv) ///
    , var(SC@1) method(mlmv) 
estat gof, stats(all)
estat mindices
predict residual,latent(SC) 
egen tom=std(residual) 
label var tom "Theory of mind"
drop residual


***************************************************************
//	#2 General cognition: Latent variable analysis
***************************************************************

/*EFA*/

*drop visual, speed, moca_raw
factor attention execfxn epmem language,ipf
screeplot

factor attention execfxn epmem language,ipf factor(1)
 

/*CFA*/


*without speed (TLI=0.759, RMSEA=0.123)
sem (GC -> attention execfxn epmem language) ///
    , var(GC@1) method(mlmv) cov(e.attention*e.execfxn)
estat gof, stats(all)
estat mindices

*add speed
sem (GC -> attention execfxn epmem language speed) ///
    , var(GC@1) method(mlmv) cov(e.execfxn*e.speed) cov(e.execfxn*e.epmem)
estat gof, stats(all)
estat mindices
predict residual,latent(GC) 
egen gen_cog_spd=std(residual) 
label var gen_cog_spd "General cognition"
drop residual

*add visual
sem (GC -> attention execfxn epmem language visual) ///
    , var(GC@1) method(mlmv) cov(e.epmem*e.language) cov(e.execfxn*e.epmem)
estat gof, stats(all)
estat mindices
predict residual,latent(GC) 
egen gen_cog_vsl=std(residual) 
label var gen_cog_vsl "General cognition"
drop residual

*add visual+speed
sem (GC -> attention execfxn epmem language visual speed) ///
    , var(GC@1) method(mlmv) cov(e.execfxn*e.speed) cov(e.execfxn*e.epmem)
estat gof, stats(all)
estat mindices
predict residual,latent(GC) 
egen gen_cog_sdp_vsl=std(residual) 
label var gen_cog_sdp_vsl "General cognition"
drop residual



*/


***************************************************************
//	#3 predict bridging & bonding
***************************************************************



sem (Bonding -> mfreq msupport mstrength mclose), var(Bonding@1) method(mlmv) 
predict bonding,latent(Bonding)
label var bonding "Bonding capital"
*adjust bonding when netsize=0 or missing
replace bonding=. if missing(netsize) 
sum bonding
local min=r(min)
replace bonding=`min' if netsize==0 //set bonding to minimum value when netsize=0

*static TOM
rename RTME tom_afct_s
rename TheoryOfMind tom_cog_s

*dynamic TOM
alpha office_emotion office_faux,gen(tom_afct)
alpha office_deceit office_infer office_motiv,gen(tom_cog)

drop white
encode race_string,gen(white)
recode white (1=.) (2/5=0) (6=1)

drop execfxn negTraila negTrailaSD negTrailb Trailb_resid
egen sd_digib = std(digib)
gen negTraila= -trail_a_time
egen negTrailaSD = std(negTraila)
gen negTrailb= -trail_b_time
regress negTrailb negTraila
predict Trailb_resid, residuals
factor sd_digib negTrailaSD Trailb_resid, pcf //to confirm underlying factor exists; it should
egen execfxn = rowmean(sd_digib negTrailaSD Trailb_resid) 

drop if missing(edu, bridging, facemem_cr, tom_afct, tom_cog, execfxn, epmem)

*table 1
desctable age female white i.edu execfxn epmem facemem_cr tom_afct tom_cog ///
		,filename("descriptives") stats(mean sd range) listwise

*Table S1: ssc install asdoc
asdoc pwcorr bridging bonding facemem_cr epmem tom_afct tom_afct_s execfxn tom_cog tom_cog_s, sig listwise dec(3) replace 

foreach i of varlist bridging bonding execfxn epmem attention facemem_cr tom_afct* tom_cog* {
	egen `i'_std=std(`i')
	drop `i'
	rename `i'_std `i'
}
label var bonding "Bonding social capital"
label var bridging "Bridging social capital"
label var facemem_cr "Face memory"
label var tom_afct "Theory of mind-affective (Dynamic)"
label var tom_cog "Theory of mind-cognitive (Dynamic)"
label var tom_afct_s "Theory of mind-affective (Static)"
label var tom_cog_s "Theory of mind-cognitive (Static)"
label var execfxn "Executive function"
label var epmem "Episodic memory"


/*individual measures*/




*table 2
eststo clear
eststo mbase: reg bridging age female white i.edu , vce(robust)
eststo mfull: reg bridging facemem_cr epmem tom_afct execfxn tom_cog age female white i.edu , vce(robust)
eststo mbase2: reg bonding age female white i.edu , vce(robust)
eststo mfull2: reg bonding facemem_cr epmem tom_afct execfxn tom_cog age female white i.edu , vce(robust)

esttab mbase mfull mbase2 mfull2 using "reg.csv", replace nobaselevels b(%5.2f) se(%5.2f) star r2(%5.2f) aic(%5.2f) bic(%5.2f) nogap noconstant scalars(F)

eststo m1: reg bridging execfxn, vce(robust)
eststo m2: reg bridging epmem, vce(robust)
eststo m3: reg bridging facemem_cr, vce(robust)
eststo m4: reg bridging tom_afct, vce(robust)
eststo m5: reg bridging tom_cog, vce(robust)

coefplot (m3 m2 m4 m1 m5) (mfull) , xline(0) mlabel format(%9.2f) mlabposition(12) drop(age female white *.edu _cons) ylabel(,labsize(med)) legend(off) tit("Bridging social capital") saving(bridging,replace) 

eststo m6: reg bonding execfxn, vce(robust)
eststo m7: reg bonding epmem, vce(robust)
eststo m8: reg bonding facemem_cr, vce(robust)
eststo m9: reg bonding tom_afct, vce(robust)
eststo m10: reg bonding tom_cog, vce(robust)

coefplot (m8 m7 m9 m6 m10) (mfull2) , xline(0) mlabel format(%9.2f) mlabposition(12) drop(age female white *.edu _cons) ylabel("") legend(order(1 "Bivariate" "correlation" 3 "Full model")) tit("Bonding social capital") saving(bonding,replace)
graph combine "bridging" "bonding"
graph export "plot.tif",replace //figure 1

*Table 3,4: Deconstructing the contributions of executive function and cognitive theory of mind 
eststo clear
eststo m1: reg bridging epmem execfxn age female white i.edu , vce(robust)
eststo m2: reg bridging tom_afct execfxn age female white i.edu , vce(robust)
eststo m3: reg bridging epmem tom_cog age female white i.edu , vce(robust)
eststo m4: reg bridging tom_afct tom_cog age female white i.edu , vce(robust)
eststo m5: reg bonding tom_cog tom_afct age female white i.edu , vce(robust)
eststo m6: reg bonding tom_cog execfxn age female white i.edu , vce(robust)

esttab * using "reg.csv", append nobaselevels order(epmem execfxn tom_afct tom_cog) b(%5.2f) se(%5.2f) star r2(%5.2f) aic(%5.2f) bic(%5.2f) nogap noconstant scalars(F)

*Table S2,S3
eststo clear
eststo m1: reg bridging age female white i.edu facemem_cr, vce(robust)
eststo m2: reg bridging age female white i.edu epmem, vce(robust)
eststo m3: reg bridging age female white i.edu tom_afct, vce(robust)
eststo m3a: reg bridging age female white i.edu tom_afct_s, vce(robust)
eststo m4: reg bridging age female white i.edu execfxn, vce(robust)
eststo m5: reg bridging age female white i.edu tom_cog, vce(robust)
eststo m5a: reg bridging age female white i.edu tom_cog_s, vce(robust)
eststo m6: reg bonding age female white i.edu facemem_cr, vce(robust)
eststo m7: reg bonding age female white i.edu epmem, vce(robust)
eststo m8: reg bonding age female white i.edu tom_afct, vce(robust)
eststo m8a: reg bonding age female white i.edu tom_afct_s, vce(robust)
eststo m9: reg bonding age female white i.edu execfxn, vce(robust)
eststo m10: reg bonding age female white i.edu tom_cog, vce(robust)
eststo m10a: reg bonding age female white i.edu tom_cog_s, vce(robust)

esttab * using "reg.csv", append nobaselevels b(%5.2f) se(%5.2f) star r2(%5.2f) aic(%5.2f) bic(%5.2f) nogap noconstant scalars(F)

*Table S4

eststo clear
eststo m1: reg bridging age female white i.edu facemem_cr epmem tom_afct tom_afct_s execfxn tom_cog tom_cog_s, vce(robust)
eststo m2: reg bonding age female white i.edu facemem_cr epmem tom_afct tom_afct_s execfxn tom_cog tom_cog_s , vce(robust)

esttab m1 m2 using "reg.csv", append nobaselevels b(%5.2f) se(%5.2f) star r2(%5.2f) aic(%5.2f) bic(%5.2f) nogap noconstant scalars(F)

*Dominance analysis
domin bridging facemem_cr epmem tom_afct execfxn tom_cog, all(age female white i.edu) //ssc install domin


/*
*why episodic memory not matter: tom_cog eat up the variance
eststo clear
eststo m0: reg bridging epmem, vce(robust)
eststo m1: reg bridging epmem age female white i.edu, vce(robust)
eststo m2: reg bridging epmem execfxn age female white i.edu , vce(robust)
eststo m3: reg bridging epmem tom_afct age female white i.edu , vce(robust)
eststo m4: reg bridging epmem tom_cog age female white i.edu , vce(robust)
esttab * using "reg.csv", append nobaselevels order(epmem execfxn tom_afct tom_cog) b(%5.2f) se(%5.2f) star r2(%5.2f) aic(%5.2f) bic(%5.2f) nogap noconstant

coefplot *, xline(0) mlabel format(%9.2f) mlabposition(12) keep(epmem) ylabel(,labsize(med)) legend(order(1 "Base model" 3 "Covariates" 5 "Covariates+Executive function" 7 "Covariates+TOM-Affective" 9 "Covariates+TOM-Cognitive")) tit("Bridging social capital")
graph export "plot2.tif",replace

*why tom_afct not matter: epmem, tom_cog eat up the variance
eststo clear
eststo m0: reg bridging tom_afct, vce(robust)
eststo m1: reg bridging tom_afct age female white i.edu, vce(robust)
eststo m2: reg bridging tom_afct execfxn age female white i.edu , vce(robust)
eststo m3: reg bridging tom_afct epmem age female white i.edu , vce(robust)
eststo m4: reg bridging tom_afct tom_cog age female white i.edu , vce(robust)
esttab * using "reg.csv", append nobaselevels order(tom_afct execfxn epmem tom_cog) b(%5.2f) se(%5.2f) star r2(%5.2f) aic(%5.2f) bic(%5.2f) nogap noconstant

*why face memory not matter: controls eat up the variance
eststo clear
eststo m1: reg bridging facemem_cr, vce(robust)
eststo m2: reg bridging facemem_cr age female white i.edu , vce(robust)
esttab * using "reg.csv", append nobaselevels b(%5.2f) se(%5.2f) star r2(%5.2f) aic(%5.2f) bic(%5.2f) nogap noconstant


/*
***************************************************************
//	#4 SNAD
***************************************************************



import excel using "C:\Users\siypeng\Dropbox\peng\Academia\Work with Brea\SNAD\SNAD data\Peng\Social vs. general cognition\SNAD_TheoryofMind",clear firstrow
rename Date date_tom
rename SubID SUBID
duplicates drop SUBID,force
rename AffectiveToM tom_afct
rename CogToM tom_cog
rename Control control
label var tom_afct "Theory of mind-affective"
label var tom_cog "Theory of mind-cognitive"
merge 1:m SUBID using "C:\Users\siypeng\OneDrive - Indiana University\SNAD SHARED FOLDER\PUBLIC\SNAD Data\Cleaned data\Focal\snad-analysis-focal-r01match-20230824.dta"
keep if _merge==3
drop match minval bridging

gen match=.
replace match=1 if date_tom==date_snad & !missing(date_tom) //exact match
gen diff=abs(date_tom-date_snad) //calculate difference between visits
order match, after(date_tom)

*match within 180 days
egen minval = rowmin(diff) //SNAD date closest to IADC date
	replace diff=. if minval!=diff //only keep the matched wave

	fre diff if match==.
	replace match=1 if diff<=180 & missing(match)
keep if match==1
drop if missing(tom_cog) 


	/*compute bridging ties*/
	
	
	sem (Bridging -> netsize diverse weakest density efctsize sole) if netsize>0 & !missing(netsize) ///
    , var(Bridging@1) method(mlmv) cov(e.netsize*e.efctsize) cov(e.netsize*e.density) cov(e.diverse*e.density) cov(e.density*e.sole) cov(e.diverse*e.weakest) cov(e.netsize*e.diverse) cov(e.weakest*e.sole)
estat gof, stats(all)
estat mindices

predict bridging,latent(Bridging) 
label var bridging "Bridging capital"

*adjust bridging when netsize=0 or missing
replace bridging=. if missing(netsize) 
sum bridging
local min=r(min)
replace bridging=`min' if netsize==0 //set bridging to minimum value when netsize=0


sem (Bonding -> mfreq msupport mstrength mclose), var(Bonding@1) method(mlmv)  
estat gof, stats(all)
estat mindices
predict bonding,latent(Bonding)
label var bonding "Bonding capital"
*adjust bonding when netsize=0 or missing
replace bonding=. if missing(netsize) 
sum bonding
local min=r(min)
replace bonding=`min' if netsize==0 //set bonding to minimum value when netsize=0

eststo clear
eststo m1: reg bridging agesnad female white i.edu tom_afct control, vce(robust)
eststo m2: reg bridging agesnad female white i.edu tom_cog control, vce(robust)
eststo m3: reg bonding agesnad female white i.edu tom_afct control, vce(robust)
eststo m4: reg bonding agesnad female white i.edu tom_cog control, vce(robust)
esttab * using "reg.csv", replace nobaselevels b(%5.2f) se(%5.2f) star r2(%5.2f) aic(%5.2f) bic(%5.2f) nogap noconstant

asdoc pwcorr bridging bonding epmem tom_afct execfxn tom_cog, sig listwise dec(3) replace 

