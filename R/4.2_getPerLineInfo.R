getPerLineInfo<-function(x,y,K,membership){
  result<-as.data.frame(matrix(data=NA,nrow=K,ncol=3))
  colnames(result)<-c("lineIndex","R2","pValue")
  result$lineIndex<-1:K

  for(k in 1:K){
    # k<-1
    xThisLine<-x[membership==k]
    yThisLine<-y[membership==k]

    # mod<-lm(yThisLine~xThisLine)
    # modSummary<-summary(mod)
    modSummary<-summary(lm(yThisLine~xThisLine))
    R2<-modSummary$r.squared
    pValue<-modSummary$coefficients["xThisLine","Pr(>|t|)"]

    # #The above approach is approximately equivalent to:
    # R2Alt<-cor(x=xThisLine,y=yThisLine)^2
    # pValueAlt<-cor.test(x=xThisLine,y=yThisLine)$p.value

    result[k,2]<-R2
    result[k,3]<-pValue
  }

  return(result)
}




