change_value <- function(tab,col,before,after){
  tab_col <- paste0(tab,"$",col)
  for (i in eval(parse(text = tab_col))) {
    eval(parse(text = paste0(tab,"$",col,"<<-as.character(",tab,"$",col,")")))
    g <- grepl(before, i)
    if (g == T) {
      eval(parse(text = paste0(tab,"$",col,"[",tab,"$",col,"%in% i] <<-after")))
    }
  }
  eval(parse(text = paste0(tab,"$",col,"<<-as.factor(",tab,"$",col,")")))
  print("Done")
}


aggregate_by <- function(data, value_mean, value_check_1, value_check_2,value_check_3){
  eval(parse(text= paste0(data,'<<- aggregate(',data,'$',value_mean,', list(',
                          data,'$',value_check_1,',',data,'$',value_check_2,',',
                          data,'$',value_check_2,')',' FUN=median)')))
                          
}
  


aggregate_all <- function(data, range, value_check_1, value_check_2,value_check_3){
  eval(parse(text= paste0(data,'<<- aggregate(',data,range,' ,list(',
                          data,'$',value_check_1,'),FUN=median)')))
  
}
