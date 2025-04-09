 /* Step 1 */
data donutpower;
	input trt mu;
	do batch = 1 to 6; *6 batches per treatment;
		output;
	end;
datalines;
1 172
2 185
3 176
4 162
;

proc print data=donutpower;
run;




 /* Step 2 */
proc glimmix data=donutpower;
	class trt;
	model mu = trt;
	parms(100)/hold=1;
	contrast 'Animal Fat vs Vegetable Oil' trt 1 1 -1 -1;
ods output tests3=fix contrasts=con;
run;

proc print data=fix;
proc print data = con;
run;



 /* Step 3 */
data poweranalysis;
	set fix con; * The datasets we made above.;
	do alpha = 0.10, 0.05, 0.01;
		lambda = numdf*fvalue; * Noncentrality parameter;
		fcrit = finv(1-alpha, numdf, dendf, 0); *Finds the critical value for F;
		power = 1 - probf(fcrit, numdf, dendf, lambda);
		output;
	end;

proc print;
run;