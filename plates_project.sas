proc import out = plates
datafile = "/home/u63558571/applications.csv"
dbms = csv replace;
guessingrows = max;
run;

proc contents data = plates;
run;

proc freq data = plates;
tables review_reason_code;
run;
/*
Data is very messy, Reason code is our best X right now and there's a lot of variability
Let's fix reason code
*/
proc freq data = plates;
tables status;
run;


data plates;
set  plates;
code_clean = review_reason_code;
if review_reason_code in ("1") then code_clean = "1";
	else if review_reason_code in ("2") then code_clean = "2";
	else if review_reason_code in ("3") then code_clean = "3";
	else if review_reason_code in ("4") then code_clean = "4";
	else if review_reason_code in ("5") then code_clean = "5";
	else if review_reason_code in ("6") then code_clean = "6";
	else if review_reason_code in ("7", "7(B)", "7B","7B?") then code_clean = "7B";
	else if review_reason_code in ("7D", "7d", "D7", "d7") then code_clean = "7D";
	else code_clean = "NA";
run;

data plates;
set plates;
where code_clean ~= "NA";
run; 

proc freq data = plates;
tables code_clean;
run;

data plates;
set plates;
status_clean = status;
if status in ("Y") then status_clean = "1";
	else if status in ("N") then status_clean = "0";
	else status_clean = "NA";
run;

data plates;
set plates;
where status_clean ~= "NA";
run; 

proc freq data = plates;
tables code_clean*status_clean;
run;

data plates;
set plates;
customer_length = length(customer_meaning);
reviewer_length = length(reviewer_comments);
run;

proc sgplot data = plates;
scatter x = reviewer_length y = status_clean;
run;

data plates;
set plates;
where customer_meaning not in ("NO MICRO","NO MICRO AVAILABLE", " ");
run;
 


proc logistic data = plates;
class code_clean / param = reference;
model status_clean(event = "1") = code_clean customer_length reviewer_length / clparm = both;






