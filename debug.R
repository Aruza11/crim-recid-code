date_in_intlist=function(person_id,offense_date,JudgementDate,intlist){
  # offense_date = row$offense_date
  # JudgementDate = row$JudgementDate
  # intlist = row$interval_list

  # print(length(intlist))
  # print(intlist)
  # print(person_id)
  # print(offense_date)
  # print(JudgementDate)

    # print(length(intlist[[1]]))
  # print("entered function")
  
  if(!is.na(JudgementDate)){
    current_interval=interval(start=offense_date,end=JudgementDate)
  }
  
  for(int in intlist){
    # print("entered loop")
    
    int=strsplit(int,split="--",fixed=T)
    # print(int)
    
    startint=int[[1]][1]
    endint=int[[1]][2]
    # print(startint)
    # print (endint)
    
    if(startint=="NA"|endint=="NA"){
      # print("na")
      
    # if(is.na(startint)|is.na(endint)){print("na")
      return (NA)}
    loop_interval=interval(start=startint,end=endint)
    # print(loop_interval)
    
    
    if(!is.na(JudgementDate)){
      if(current_interval==loop_interval){
      next
      }
    }
    if(offense_date %within% loop_interval){
      return(1)
    }
    
  }
  return(0) 
}

f_test =function(row){
  1
  
}


f_test2 = function(JudgementDate, interval_list) {
  length(JudgmentDate)
}


