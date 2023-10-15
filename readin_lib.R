read_item_XWalk<-function(file_path, sheet_name){
  read_excel(file_path, sheet = sheet_name)
}

read_state_achievement<-function(file_path, year, subject){
  if(subject == "PHY"){
    read_excel(file_path, skip = 1, col_names = c("School Name", "School Code", "Subject", 
                                                  "M+E Count", "M+E%", "E Count", "E%", 
                                                  "M Count", "M%", "PM Count",
                                                  "PM%", "NM Count", "NM%", 
                                                  "Student Count", "delete",
                                                  "Avg. Scaled Score", "delete", "delete"))%>%
      select(!contains("delete"))%>%
      mutate(`M+E Count` = as.integer(`M+E Count`))%>%
      mutate(`M+E%` = as.numeric(`M+E%`))%>%
      mutate(`E Count` = as.integer(`E Count`))%>%
      mutate(`E%` = as.numeric(`E%`))%>%
      mutate(`M Count` = as.integer(`M Count`))%>%
      mutate(`M%` = as.numeric(`M%`))%>%
      mutate(`PM Count` = as.integer(`PM Count`))%>%
      mutate(`PM%` = as.numeric(`PM%`))%>%
      mutate(`NM Count` = as.integer(`NM Count`))%>%
      mutate(`NM%` = as.numeric(`NM%`))%>%
      mutate(`Student Count` = as.integer(`Student Count`))%>%
      mutate(`Avg. Scaled Score` = as.integer(`Avg. Scaled Score`))%>%
     
  
      mutate(`Year` = year)%>%
      filter(`Subject`== subject)
    
  }
}

# subject should be PHY or BIO
 read_school_item<-function(file_path, year, subject){
   if(subject == "PHY"){
     read_excel(file_path, skip = 1)%>%
       select(!contains("Subject"))%>%
       select(!contains("Part. Rate"))%>%
       pivot_longer(!(contains("School")|contains("Tested")|contains("Rate")), names_to = "ITEM", values_to = "School%")%>%
       mutate(ITEM = as.integer(ITEM))%>%
       mutate(Tested = as.integer(Tested))%>%
       mutate(`Year` = year)%>%
       mutate(`Subject` = subject)%>%
         select(`Year`, `Subject`, `School Name`, `School Code`, `Tested`, `ITEM`, `School%`)
   }
   
 }
 
 
# subject should be PHY or BIO,
read_state_item<-function(file_path, year, subject){
  if(subject == "PHY"){
    State_Percent<-read_excel(file_path, skip = 1)%>%
    select(!contains("Subject"))%>%
    pivot_longer(!(contains("District")|contains("Tested")|contains("Rate")), names_to = "ITEM", values_to = "District%")%>%
     mutate(ITEM = as.integer(ITEM))%>%
     mutate(Tested = as.integer(Tested))%>%
      filter(str_detect(`District Name`,"State"))%>%
      mutate(`State%` = `District%`)%>%
      select(`ITEM`, `State%`)%>%
      mutate(`Year` = year)%>%
      mutate(`Subject` = subject)%>%
      select(`Year`, `Subject`, `ITEM`, `State%`)
    #view(State_Percent)
    
    # if(year > 2019){
    #   read_excel(file_path,
    #              skip = 1) %>%
    #     select(!contains("Subject"))%>%
    #     select(!contains("Part."))%>%
    #     pivot_longer(!(contains("District")|contains("Tested")), names_to = "ITEM", values_to = "District%")%>%
    #     mutate(ITEM = as.integer(ITEM))%>%
    #     filter(!str_detect(`District Name`,"State"))%>%
    #     left_join(State_Percent, "ITEM")%>%
    #     mutate(`District-State Diff` = `District%`-`State%`)
    #   
    # }
    # 
    # else if(year == 2019){
    #   read_excel(file_path,
    #              skip = 1) %>%
    #     select(!contains("Subject"))%>%
    #     pivot_longer(!(contains("District")|contains("Tested")), names_to = "ITEM", values_to = "District%")%>%
    #     mutate(ITEM = as.integer(ITEM))%>%
    #     mutate(Tested = as.integer(Tested))%>%
    #     filter(!str_detect(`District Name`,"State"))%>%
    #     left_join(State_Percent, "ITEM")%>%
    #     mutate(`District-State Diff` = `District%`-`State%`)
    #   
    # }
  }else if(subject == "ELA"){
    # State_Percent<-read_excel(file_path, sheet = sheet_name, 
    #                           skip = 1) %>%
    #   select(!contains("Subject"))%>%
    #   pivot_longer(!(contains("District")|contains("Tested")), names_to = "ITEM", values_to = "District%")%>%
    #   filter(!str_detect(`ITEM`, "conv"))%>%
    #   filter(!str_detect(`ITEM`, "idea"))%>%
    #   mutate(ITEM = as.integer(ITEM))%>%
    #   filter(ITEM <= 25)%>%
    #   mutate(Tested = as.integer(Tested))%>%
    #   filter(str_detect(`District Name`,"State"))%>%
    #   mutate(`State %` = `District%`)%>%
    #   select(`ITEM`, `State %`)
    # view(State_Percent)
    if(year == 2019){
      read_excel(file_path,  
                 skip = 1) %>%
        select(!contains("Subject"))%>%
        pivot_longer(!(contains("District")|contains("Tested")), names_to = "ITEM", values_to = "District%")%>%
        filter(!str_detect(`ITEM`, "conv"))%>%
        filter(!str_detect(`ITEM`, "idea"))%>%
        mutate(ITEM = as.integer(ITEM))%>%
        filter(ITEM <= 25)%>%
        filter(!str_detect(`District Name`,"State"))%>%
        left_join(State_Percent, "ITEM")
      
    }
  }
}