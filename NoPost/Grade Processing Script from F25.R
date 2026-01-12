



gr = fread("C:\\Users\\jkirk\\OneDrive - Michigan State University\\Teaching\\EC242_FS25 Grading\\Final Grades\\FS25-EC-242-001 - Social Science Data Analytics_GradesExport_2025-12-15-19-17 AS SUBMITTED.csv")



gr = gr[,score:=100*`Calculated Final Grade Numerator`/`Calculated Final Grade Denominator`]
gr = gr[,score:=100*round(`Calculated Final Grade Numerator`/`Calculated Final Grade Denominator`,2)]
gr = gr[,c(1,3,4,8)]
gr[,grade:=case_when(score>=94 ~ 4.0, # NEEDS TO UPDATE FOR S26
                     score>=88 ~ 3.5,
                     score>=83 ~ 3.0,
                     score>=77 ~ 2.5,
                     score>=72 ~ 2.0,
                     score>=66 ~ 1.5,
                     score>=58 ~ 1.0,
                     score<=58.00 ~ 0.0)]

grunr = copy(gr)




doit<-function(num){
  splitnum = str_split(as.character(num), '')[[1]]
  
  splitnum.low = splitnum[order(splitnum)]
  splitnum.high = as.integer(paste0(rev(splitnum.low), collapse = ''))
  splitnum.low = as.integer(paste0(splitnum.low, collapse = ''))
  return(splitnum.high - splitnum.low)
}

nn = 140268
while(nn!=6174){
nn = doit(nn)
print(nn)
}




