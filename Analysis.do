****Project: SNACK-SNAD
****Author:  Siyun Peng
****Date:    2023/01/11
****Purpose: general vs. social cognitive performance 



use "C:\Users\peng_admin\OneDrive - Indiana University\SNAD SHARED FOLDER\PUBLIC\SNAD Data\Cleaned data\SNAD-SNACK Merged Data\Short_merged_01_12_2023",clear
cd "C:\Users\peng_admin\Dropbox\peng\Academia\Work with Brea\SNAD\SNAD data\Peng\Social vs. general cognition\result"

rename FaceMem_CR Office_*,lower
keep if source_study==1 //use SNACK
drop residual


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






***************************************************************
//	#3 predict bridging
***************************************************************


alpha office_emotion office_faux,gen(tom_afct)
alpha office_deceit office_infer office_motiv,gen(tom_cog)
drop white
encode race_string,gen(white)
recode white (1=.) (2/5=0) (6=1)

pwcorr bridging netsize facemem_cr tom_afct tom_cog attention execfxn epmem language visual speed,sig



/*individual measures*/



domin bridging attention execfxn epmem facemem_cr office_total, all(age female white i.edu) //ssc install domin
domin bridging attention execfxn epmem facemem_cr office_total age female white edu //ssc install domin
eststo clear
eststo mfull: reg bridging execfxn epmem facemem_cr tom_afct tom_cog age female white i.edu , vce(robust)
esttab *mfull using "reg.csv", replace nobaselevels drop(age female white *.edu) b(%5.2f) se(%5.2f) star r2(%5.2f) aic(%5.2f) bic(%5.2f) nogap noconstant

*why episodic memory not matter: tom_cog eat up the variance
eststo clear
eststo m1: reg bridging epmem age female white i.edu, vce(robust)
eststo m2: reg bridging epmem execfxn age female white i.edu , vce(robust)
eststo m3: reg bridging epmem tom_afct age female white i.edu , vce(robust)
eststo m4: reg bridging epmem tom_cog age female white i.edu , vce(robust)
esttab * using "reg.csv", append nobaselevels drop(age female white *.edu) b(%5.2f) se(%5.2f) star r2(%5.2f) aic(%5.2f) bic(%5.2f) nogap noconstant

*why tom_afct not matter: epmem, tom_cog eat up the variance
eststo clear
eststo m1: reg bridging tom_afct age female white i.edu, vce(robust)
eststo m2: reg bridging tom_afct execfxn age female white i.edu , vce(robust)
eststo m3: reg bridging tom_afct epmem age female white i.edu , vce(robust)
eststo m4: reg bridging tom_afct tom_cog age female white i.edu , vce(robust)
esttab * using "reg.csv", append nobaselevels drop(age female white *.edu) b(%5.2f) se(%5.2f) star r2(%5.2f) aic(%5.2f) bic(%5.2f) nogap noconstant




/*latent*/


eststo clear
eststo m1: reg bridging facemem_cr office_deceit office_emotion office_faux office_infer office_motiv attention execfxn epmem age female white i.edu, vce(robust)
eststo m2: reg bridging facemem_cr tom_afct tom_cog attention execfxn epmem age female white i.edu, vce(robust)
eststo m3: reg bridging facemem_cr office_total attention execfxn epmem age female white i.edu, vce(robust)
eststo m4: reg bridging facemem_cr tom attention execfxn epmem age female white i.edu, vce(robust)
eststo m5: reg bridging facemem_cr tom attention execfxn epmem age female white i.edu, vce(robust)
eststo m6: reg bridging facemem_cr tom gen_cog_spd age female white i.edu, vce(robust)

esttab * using "reg.csv", replace nobaselevels drop(age female white *.edu) b(%5.2f) se(%5.2f) star r2(%5.2f) aic(%5.2f) bic(%5.2f) nogap noconstant
