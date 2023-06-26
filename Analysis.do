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
//	#3 predict bridging
***************************************************************



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
asdoc pwcorr bridging facemem_cr epmem tom_afct execfxn tom_cog, sig listwise dec(2) replace 

foreach i of varlist bridging execfxn epmem attention facemem_cr tom_afct tom_cog {
	egen `i'_std=std(`i')
	drop `i'
	rename `i'_std `i'
}
label var bridging "Bridging social capital"
label var facemem_cr "Face memory"
label var tom_afct "Theory of mind-affective"
label var tom_cog "Theory of mind-cognitive"
label var execfxn "Executive function"
label var epmem "Episodic memory"


/*individual measures*/




domin bridging attention execfxn epmem facemem_cr office_total, all(age female white i.edu) //ssc install domin
domin bridging attention execfxn epmem facemem_cr office_total age female white edu //ssc install domin

*table 2
eststo clear
eststo mbase: reg bridging age female white i.edu , vce(robust)
eststo mfull: reg bridging facemem_cr epmem tom_afct execfxn tom_cog age female white i.edu , vce(robust)
esttab mbase mfull using "reg.csv", replace nobaselevels b(%5.2f) se(%5.2f) star r2(%5.2f) aic(%5.2f) bic(%5.2f) nogap noconstant

eststo m1: reg bridging execfxn, vce(robust)
eststo m2: reg bridging epmem, vce(robust)
eststo m3: reg bridging facemem_cr, vce(robust)
eststo m4: reg bridging tom_afct, vce(robust)
eststo m5: reg bridging tom_cog, vce(robust)

coefplot (m3 m2 m4 m1 m5) (mfull) , xline(0) mlabel format(%9.2f) mlabposition(12) drop(age female white *.edu _cons) ylabel(,labsize(med)) legend(order(1 "Base model" 3 "Full model")) tit("Bridging social capital")
graph export "plot.tif",replace //figure 1

*Table 3: Deconstructing the contributions of executive function and cognitive theory of mind 
eststo clear
eststo m1: reg bridging epmem execfxn age female white i.edu , vce(robust)
eststo m2: reg bridging tom_afct execfxn age female white i.edu , vce(robust)
eststo m3: reg bridging epmem tom_cog age female white i.edu , vce(robust)
eststo m4: reg bridging tom_afct tom_cog age female white i.edu , vce(robust)
esttab * using "reg.csv", append nobaselevels order(epmem execfxn tom_afct tom_cog) b(%5.2f) se(%5.2f) star r2(%5.2f) aic(%5.2f) bic(%5.2f) nogap noconstant

*Table S2
eststo clear
eststo m1: reg bridging age female white i.edu facemem_cr, vce(robust)
eststo m2: reg bridging age female white i.edu epmem, vce(robust)
eststo m3: reg bridging age female white i.edu tom_afct, vce(robust)
eststo m4: reg bridging age female white i.edu execfxn, vce(robust)
eststo m5: reg bridging age female white i.edu tom_cog, vce(robust)
esttab * using "reg.csv", append nobaselevels b(%5.2f) se(%5.2f) star r2(%5.2f) aic(%5.2f) bic(%5.2f) nogap noconstant

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

