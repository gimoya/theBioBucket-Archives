## Online questionnaire with Google Docs and report with knitr markdown





I use a function ([source](https://github.com/gimoya/theBioBucket-Archives/blob/master/R/Functions/google_ss.R)) to read the data from Google Docs ([source](https://https://docs.google.com/spreadsheet/ccc?key=0AmwAunwURQNsdFplUTBZUTRLREtLUDhabGxBMHBRWmc#gid=0)) .
With the data I produce a simple barplot with the responses to the questionnaire (this was uploaded to imgur.com). Then I push the .md-file to github for publishing.
Here is the .Rmd file ([source](https://github.com/gimoya/theBioBucket-Archives/blob/master/R/knitr/Questionnaire.Rmd)) that I used for knitting to produce the .md file ([source](https://raw.github.com/gimoya/theBioBucket-Archives/master/R/knitr/Questionnaire.md)) which is rendered here by Github.
I use some inline code here - like so: There were `11` responses with 'NO', `5` responses with 'YES' and `2` responses were indefinite. 

![plot of chunk barplot](http://i.imgur.com/iWYyN.png) 

