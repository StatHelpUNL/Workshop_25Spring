 /* Step 1 */
data rptm_means;
	input Inoculation_Method $ Thickness $ @@;
	do Week=1 to 5 by 1;
 	input mu @@;
 	output;
 	end;
datalines;
Dry 1/4 4.26 4.25 4.47 4.33 4.54
Dry 1/8 4.91 4.95 4.67 4.56 4.97
Wet 1/4 4.21 4.57 4.65 4.49 4.38
Wet 1/8 4.86 4.78 4.62 4.32 4.22
;

proc print; run;

 
data rptm_design;
	set rptm_means;
	do Batches =1 to 5;
	output;
end;
run;
 
proc print data=rptm_design;
run;
 
 
 
 
 
 /* Step 2 */
proc glimmix data=rptm_design;
	class Batches Inoculation_Method Thickness Week;
	model mu = Inoculation_Method|Thickness|Week;
	random intercept /subject=Batches;
	random Week/ subject=Batches*Inoculation_Method*Thickness type=ar(1) residual;
	parms(0.029)(0.017)(0.028)/hold=1,2,3;
	contrast'Dry vs Wet at 1/4 Inches' Inoculation_Method 1 -1 Inoculation_Method*Thickness 1 0 -1 0;
	contrast 'Dry vs Wet at 1/8 Inches' Inoculation_Method 1 -1 Inoculation_Method*Thickness 0 1 0 -1;
ods output contrasts=f_contrast tests3=f_anova;
run;






/* Step 3 */
data power;
	set f_contrast f_anova;
	alpha=0.05;
	lambda = numdf*fvalue;
	fcrit=finv(1-alpha,numdf,dendf,0);
	power=1-probf(fcrit,numdf,dendf,lambda);
run;

proc print data=power;
run;
 
 