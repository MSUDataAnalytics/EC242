



# F26 gr = fread("C:\\Users\\jkirk\\OneDrive - Michigan State University\\Teaching\\EC242_FS25 Grading\\Final Grades\\FS25-EC-242-001 - Social Science Data Analytics_GradesExport_2025-12-15-19-17 AS SUBMITTED.csv")

# S26 gr = fread("C:\\Users\\jkirk\\OneDrive - Michigan State University\\Teaching\\EC242_SS26 Grading\\Final Grades\\SS26-EC-242-001 - Social Science Data Analytics_GradesExport_2026-05-05-14-09.csv")

gr = gr[,scoreUnround:=100*`Calculated Final Grade Numerator`/`Calculated Final Grade Denominator`]
gr = gr[,score:=100*round(`Calculated Final Grade Numerator`/`Calculated Final Grade Denominator`,2)]
gr = gr[,.(OrgDefinedId, `Last Name`, `First Name`, scoreUnround, score)]

# gr = gr[`First Name` !='Demo',]


gr[,grade:=case_when(score>=95 ~ 4.0, # Update here (check syllabus)
                     score>=88 ~ 3.5,
                     score>=83 ~ 3.0,
                     score>=77 ~ 2.5,
                     score>=72 ~ 2.0,
                     score>=66 ~ 1.5,
                     score>=58 ~ 1.0,
                     score<=58.00 ~ 0.0)]
gr[`Last Name`=='Jain', grade:=4.0]  # S26, was on bubble, allowed extra Weekly Writing

gr = gr[order(`Last Name`, `First Name`),]
mean(gr$grade[gr$`Last Name`!='Student'])



write_csv(gr, file.path("C:\\Users\\jkirk\\OneDrive - Michigan State University\\Teaching\\EC242_SS26 Grading\\Final Grades\\SS26-EC-242-001 - Social Science Data Analytics_GradesExport_2026-05-05-14-09 SUBMITTED.csv"))



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




