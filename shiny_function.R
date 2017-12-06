test <- function(s, conf, snow, rain, sun, temp){
  
  print(paste("support:",s))
  print(paste("confidence:",conf))
  print(paste("snow:",snow))
  print(paste("rain:",rain))
  print(paste("sun:",sun))
  print(paste("temp:",temp))
  
  rules = arule_mining(mydf,s=s,c=conf,len=3)
  
  # RAIN FIXEN!!!!!!!!!!!!!!!!! CC
  
  mined_rules = getRules(rules,rain,sun,snow,temp)
  
  if(!is.null(mined_rules)){
    var <- data.frame(
      Name=mined_rules$name,
      support=mined_rules$support,
      confidence=mined_rules$confidence,
      lift=mined_rules$lift,
      count=mined_rules$count
    )
    var = var[with(var,order(-support,-confidence)),]
  }else{
    var <- data.frame(result=c("HARTZ IV und der Tag gehört Dir!"))
  }
  return(var)
}