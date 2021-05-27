###
#   title: "R-Pivot Table"
# author: "Punay Mehra"
# date: "5/20/2021"
# output:
#   html_document:
#   df_print: paged
# ###


PivatTable<-function(d1){
  if( ! require("dplyr")) {install.packages("dplyr")} + library(dplyr)
  if( ! require("svDialogs")) {install.packages("svDialogs")} + library(svDialogs)
  if( ! require("readall")) {install.packages("readall")} + library(readall)

  d1<-Readfile()
  d2<-names(d1)
  ####Select Columns
  coln<-dlg_list(d2,title = "Select Columns", multiple = TRUE,)$res

  ####Select value
  value<-dlg_list(d2,title = "Select Value", multiple = TRUE)$res


  #####Summarise value by
  form<-c("1.Sum","2.Count","3.Average","4.Max","5.Min","6.Product","7.Varience")
  summby<-dlg_list(form,title = "Summarise value by-", multiple = F)$res

  ####Filter Value
  colum <- dlg_list(d2,title = "Select Column filter", multiple = TRUE)$res
  if (!length(colum)) {
    cat("You cancelled the choice\n")
    if (summby=="1.Sum") {
      dt<-d1  %>% group_by_(.dots=coln) %>% summarise_each(value,funs = sum)
    } else if(summby=="2.Count") {
      dt<-d1 %>% group_by_(.dots=coln) %>%  summarise_each(value,funs = length)
    } else if(summby=="3.Average") {
      dt<-d1 %>% group_by_(.dots=coln) %>% summarise_each(value,funs = mean)
    } else if(summby=="4.Max") {
      dt<-d1 %>% group_by_(.dots=coln) %>% summarise_each(value,funs = max)
    } else if(summby=="5.Min") {
      dt<-d1 %>% group_by_(.dots=coln) %>% summarise_each(value,funs = min)
    } else if(summby=="6.Product") {
      dt<-d1 %>% group_by_(.dots=coln) %>% summarise_each(value,funs = prod)
    } else if(summby=="7.Varience") {
      dt<-d1 %>% group_by_(.dots=coln) %>% summarise_each(value,funs = var)
    }
  } else {
    column<-unique(d1[,colum])
    fltval<-dlg_list(column,title = "Select filter Value", multiple = TRUE)$res
    if (summby=="1.Sum") {
      dt<-d1  %>%filter(get(colum)==fltval[1]|get(colum)==fltval[2]|get(colum)==fltval[3]|get(colum)==fltval[4]|get(colum)==fltval[5]|get(colum)==fltval[6]|get(colum)==fltval[7]|get(colum)==fltval[8]|get(colum)==fltval[9]|get(colum)==fltval[10]|get(colum)==fltval[11])%>%  group_by_(.dots=coln) %>% summarise_each(value,funs = sum)
    } else if(summby=="2.Count") {
      dt<-d1 %>% group_by_(.dots=coln)%>%filter(get(colum)==fltval[1]|get(colum)==fltval[2]|get(colum)==fltval[3]|get(colum)==fltval[4]|get(colum)==fltval[5]|get(colum)==fltval[6]|get(colum)==fltval[7]|get(colum)==fltval[8]|get(colum)==fltval[9]|get(colum)==fltval[10]|get(colum)==fltval[11]) %>% summarise_each(value,funs = length)
    } else if(summby=="3.Average") {
      dt<-d1 %>% group_by_(.dots=coln)%>%filter(get(colum)==fltval[1]|get(colum)==fltval[2]|get(colum)==fltval[3]|get(colum)==fltval[4]|get(colum)==fltval[5]|get(colum)==fltval[6]|get(colum)==fltval[7]|get(colum)==fltval[8]|get(colum)==fltval[9]|get(colum)==fltval[10]|get(colum)==fltval[11]) %>% summarise_each(value,funs = mean)
    } else if(summby=="4.Max") {
      dt<-d1 %>% group_by_(.dots=coln)%>%filter(get(colum)==fltval[1]|get(colum)==fltval[2]|get(colum)==fltval[3]|get(colum)==fltval[4]|get(colum)==fltval[5]|get(colum)==fltval[6]|get(colum)==fltval[7]|get(colum)==fltval[8]|get(colum)==fltval[9]|get(colum)==fltval[10]|get(colum)==fltval[11]) %>% summarise_each(value,funs = max)
    } else if(summby=="5.Min") {
      dt<-d1 %>% group_by_(.dots=coln)%>%filter(get(colum)==fltval[1]|get(colum)==fltval[2]|get(colum)==fltval[3]|get(colum)==fltval[4]|get(colum)==fltval[5]|get(colum)==fltval[6]|get(colum)==fltval[7]|get(colum)==fltval[8]|get(colum)==fltval[9]|get(colum)==fltval[10]|get(colum)==fltval[11]) %>% summarise_each(value,funs = min)
    } else if(summby=="6.Product") {
      dt<-d1 %>% group_by_(.dots=coln)%>%filter(get(colum)==fltval[1]|get(colum)==fltval[2]|get(colum)==fltval[3]|get(colum)==fltval[4]|get(colum)==fltval[5]|get(colum)==fltval[6]|get(colum)==fltval[7]|get(colum)==fltval[8]|get(colum)==fltval[9]|get(colum)==fltval[10]|get(colum)==fltval[11]) %>% summarise_each(value,funs = prod)
    } else if(summby=="7.Varience") {
      dt<-d1 %>% group_by_(.dots=coln)%>%filter(get(colum)==fltval[1]|get(colum)==fltval[2]|get(colum)==fltval[3]|get(colum)==fltval[4]|get(colum)==fltval[5]|get(colum)==fltval[6]|get(colum)==fltval[7]|get(colum)==fltval[8]|get(colum)==fltval[9]|get(colum)==fltval[10]|get(colum)==fltval[11]) %>% summarise_each(value,funs = var)
    }
  }

  return(dt)

}


