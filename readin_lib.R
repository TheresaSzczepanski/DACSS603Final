read_item_XWalk<-function(file_path, sheet_name){
  read_excel(file_path, sheet = sheet_name)
}


read_state_item<-function(file_path, sheet_name, year, subject){
  if(subject == "math"){
    State_Percent<-read_excel(file_path, sheet = sheet_name, 
                              skip = 1) %>%
      select(!contains("Subject"))%>%
      pivot_longer(!(contains("District")|contains("Tested")), names_to = "ITEM", values_to = "District%")%>%
      mutate(ITEM = as.integer(ITEM))%>%
      mutate(Tested = as.integer(Tested))%>%
      filter(str_detect(`District Name`,"State"))%>%
      mutate(`State%` = `District%`)%>%
      select(`ITEM`, `State%`)
    view(State_Percent)
    if(year == 2019){
      read_excel(file_path, sheet = sheet_name, 
                 skip = 1) %>%
        select(!contains("Subject"))%>%
        pivot_longer(!(contains("District")|contains("Tested")), names_to = "ITEM", values_to = "District%")%>%
        mutate(ITEM = as.integer(ITEM))%>%
        mutate(Tested = as.integer(Tested))%>%
        filter(!str_detect(`District Name`,"State"))%>%
        left_join(State_Percent, "ITEM")%>%
        mutate(`District-State Diff` = `District%`-`State%`)
      
    }else if(year > 2019){
      read_excel(file_path, sheet = sheet_name, 
                 skip = 1) %>%
        select(!contains("Subject"))%>%
        select(!contains("Part."))%>%
        pivot_longer(!(contains("District")|contains("Tested")), names_to = "ITEM", values_to = "District%")%>%
        mutate(ITEM = as.integer(ITEM))%>%
        filter(!str_detect(`District Name`,"State"))%>%
        left_join(State_Percent, "ITEM")%>%
        mutate(`District-State Diff` = `District%`-`State%`)
      
    }
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
      read_excel(file_path, sheet = sheet_name, 
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