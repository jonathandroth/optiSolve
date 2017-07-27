
"print.copValidation"<-function(x, ...){
  Sy   <- x$summary
  Info <- x$info
  pad  <- max(max(nchar(Sy$Var)), 10)
  
  rownames(Info)<-paste0("           ")
  "report"<-function(n, v, d, cv, OK){
    if(is.na(v)){ v <-""}else{v <-round(v, 4)}
    if(is.na(cv)){cv<-""}else{cv<-round(cv,4)}
    if(is.na(OK)){OK<-"    "}
    
    if(str_detect(n,"lower bounds")){v<-"all x"; cv<-"lb"}
    if(str_detect(n, "upper bounds")){v<-"all x"; cv<-"ub"}
    n  <- paste0("   ", n)
    n  <- str_pad(n, pad+3, "right")
    #n <- str_replace_all(n, "\\.", " ")
    d  <- str_pad(d,   3, "right")
    v  <- str_pad(as.character(v),  6, "right")
    cv <- str_pad(as.character(cv), 6, "right")
    
    cat(paste(n, v, d, cv, ":", OK, "\n"))
  }
  print(Info)
  cat("\n")
  cat(paste0("   Variable",strrep(" ",pad-8)," Value      Bound    OK?\n"))
  cat(paste0("   -------------------------", strrep("-",pad), "\n"))
  for(i in 1:nrow(Sy)){
    report(Sy$Var[i], Sy$Val[i], Sy$Dir[i], Sy$Bound[i], Sy$OK[i])
    if(i==1){cat(paste0("   -------------------------", strrep("-",pad), "\n"))}
  }
  cat(cat(paste0("   -------------------------", strrep("-",pad), "\n")))
  cat("\n")
  invisible(x)
}