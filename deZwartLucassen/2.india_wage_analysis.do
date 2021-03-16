 

use 1north_indian_wages.dta, clear	
 


** UNSKILLED DECADAL ESTIMATES **
  
* BENGAL
reg dwage i.decade rural if hisclass ==11 & province ==5 & gender_age ==0 & european==0

margins decade
  
marginsplot

* AGRA

reg dwage i.decade rural if hisclass ==11 & province ==1 & gender_age ==0 & european==0

margins decade

marginsplot

* ALLAHABAD

reg dwage i.decade rural if hisclass ==11 & province ==2 & gender_age ==0 & european==0

margins decade

marginsplot

* BIHAR

reg dwage i.decade rural if hisclass ==11 & province ==7 & gender_age ==0 & european==0

margins decade

marginsplot


* GUJARAT

reg dwage i.decade rural if hisclass ==11 & province ==11 & gender_age ==0 & european==0

margins decade

marginsplot


* DELHI

reg dwage i.decade rural if hisclass ==11 & province ==9 & gender_age ==0 & european==0

margins decade

marginsplot

* UNSKILLED WOMEN BENGAL

reg dwage i.decade rural if hisclass ==11 & gender_age ==1 & european==0 & province==5

margins decade

marginsplot


** SKILLED DECADAL ESTIMATES **


* BENGAL

reg dwage i.decade rural if hisclass ==7 & province ==5 & gender_age ==0 & european ==0

margins decade

marginsplot

* AGRA

reg dwage i.decade rural if hisclass ==7 & province ==1 & gender_age ==0 & european==0

margins decade

marginsplot

* ALLAHABAD

reg dwage i.decade rural if hisclass ==7 & province ==2 & gender_age ==0 & european==0

margins decade

marginsplot

* BIHAR

reg dwage i.decade rural if hisclass ==7 & province ==7 & gender_age ==0 & european==0

margins decade

marginsplot

* GUJARAT

reg dwage i.decade rural if hisclass ==7 & province ==11 & gender_age ==0 & european==0

margins decade

marginsplot

* DELHI 

reg dwage i.decade rural if hisclass ==7 & province ==9 & gender_age ==0 & european==0

margins decade

marginsplot

 
** MONTHLY FLUCTUATIONS IN WAGES **


* ALL

reg dwage i.month trend rural gender age european if hisclass ==11

margins month

marginsplot 

* BENGAL

reg dwage i.month trend rural gender age european if hisclass ==11 & province ==5

margins month

marginsplot 


* AGRA

reg dwage i.month trend rural gender age european if hisclass ==11 & province ==1

margins month

marginsplot 


* ALLAHABAD

reg dwage i.month trend rural gender age european if hisclass ==11 & province ==2

margins month

marginsplot 

* BIHAR

reg dwage i.month trend rural gender age european if hisclass ==11 & province ==7

margins month

marginsplot 

* GUJARAT

reg dwage i.month trend rural gender age european if hisclass ==11 & province ==11

margins month

marginsplot 

* DELHI 

reg dwage i.month trend rural gender age european if hisclass ==11 & province ==9

margins month

marginsplot 


** REGRESSIONS TESTING EFFECTS OF SOURCES **


** UNSKILLED / ALL: 

reg dwage trend trend2 trend3 supervision rural gender age european nerrick dutchsource if hisclass ==11


** SKILLED / ALL:

reg dwage trend trend2 trend3 supervision rural gender age european nerrick dutchsource if hisclass ==7


** UNSKILLED / BENGAL:

 reg dwage trend trend2 trend3  rural gender age european nerrick dutchsource if hisclass ==11 & province==5

 
** SKILLED / BENGAL:

 reg dwage trend trend2 trend3  rural gender age european nerrick dutchsource if hisclass ==7 & province==5
