read_item_XWalk<-function(file_path, sheet_name){
  read_excel(file_path, sheet = sheet_name)
}
## Read in IT301 HS Physics reports to extract Item standards, Science Practices
## Possible Points,  Item Type, Reporting Category

read_IT301 <- function(file_path, year, subject){
  if(subject == "PHY"){
    IT301_DF<-read_excel(file_path, skip = 15, col_names = c("delete", "ITEM", "delete", "Item Type",
                                                   "delete", "delete",
                                                   "Reporting Category", "delete",
                                                   "Standard", "delete",
                                                   "Item Desc", "delete",
                                                   "Practice Category", "delete",
                                                   "delete",
                                                   "Item Possible Points", "District%",
                                                   "delete",
                                                   "State%", "District-State Diff"))%>%
      select(`ITEM`, `Item Type`, `Reporting Category`, `Standard`, `Item Desc`,
             `Practice Category`, `Item Possible Points`)%>%
      mutate(`Year` = year)
    IT301_DF<-head(IT301_DF, -5)
    IT301_DF%>%
      mutate(`Item Possible Points` = as.integer(`Item Possible Points`))%>%
      mutate(`ITEM` = as.integer(`ITEM`))%>%
      #na_if(`Practice Category', "None")%>%
      mutate(`Subject` = subject)%>%
      select(`Year`, `Subject`, `ITEM`, `Item Type`, `Standard`, `Reporting Category`,
             `Practice Category`, `Item Possible Points`, `Item Desc`)
  }
}

## Read in NextGenMCAS School reports with Achievement levels from DESE Statewide reports
read_state_achievement<-function(file_path, year, subject){
  if(subject == "PHY"){
    state_perf_sum <-read_excel(file_path, skip = 1, col_names = c("delete", "School Code", "Subject", 
                                                                   "delete", "delete", "E#", "E%", 
                                                                   "M#", "M%", "PM#",
                                                                   "PM%", "NM#", "NM%", 
                                                                   "delete", "delete",
                                                                   "State Avg. Scaled Score", 
                                                                   "delete", "delete"))%>%
      select(!contains("delete"))%>%
      filter(`Subject`== subject) %>%
      filter(`School Code` == "00000000")%>%
      # pivot_longer(contains("%"), names_to = "Performance Level", values_to = "State Performance%")%>%
      #mutate(`Tested Students` = as.numeric(`Tested Students`))%>%
      mutate(`E#` = as.integer(gsub(",", "", `E#`)))%>%
      mutate(`E%` = as.numeric(`E%`))%>%
      mutate(`M#` = as.integer(gsub(",", "", `M#`)))%>%
      mutate(`M%` = as.numeric(`M%`))%>%
      mutate(`PM#` = as.integer(gsub(",", "", `PM#`)))%>%
      mutate(`PM%` = as.numeric(`PM%`))%>%
      mutate(`NM#` = as.integer(gsub(",", "", `NM#`)))%>%
      mutate(`NM%` = as.numeric(`NM%`))%>%
      mutate(`E%State` = `E%`)%>%
      mutate(`E#State` = `E#`)%>%
      mutate(`M%State` = `M%`)%>%
      mutate(`M#State` = `M#`)%>%
      mutate(`PM%State` = `PM%`)%>%
      mutate(`PM#State` = `PM#`)%>%
      mutate(`NM%State` = `NM%`)%>%
      mutate(`NM#State` = `NM#`)%>%
        # `Performance Level` == "E%" ~ "E",
      #   `Performance Level` == "M%" ~ "M",
      #   `Performance Level` == "PM%" ~ "PM",
      #   `Performance Level` == "NM%" ~ "NM"
      # ))%>%
      # mutate(`Performance Level` = recode_factor(`Performance Level`,
      #                                            "E" = "E",
      #                                            "M" = "M",
      #                                            "PM" = "PM",
      #                                            "NM" = "NM",
      #                                            .ordered = TRUE))%>%
      mutate(`State Avg. Scaled Score` = as.numeric(`State Avg. Scaled Score`))%>%
      mutate(`Year` = year)%>%
      mutate(`State Tested` = `E#State` + `M#State` + `PM#State` + `NM#State`)%>%
      select(`Year`, `State Tested`, `E#State`, `E%State`, `M#State`, `M%State`, 
             `PM#State`, `PM%State`, `NM#State`, `NM%State`, `State Avg. Scaled Score`)
    view(state_perf_sum)
    read_excel(file_path, skip = 1, col_names = c("School Name", "School Code", "Subject", 
                                                  "delete", "delete", "E#", "E%", 
                                                  "M#", "M%", "PM#",
                                                  "PM%", "NM#", "NM%", 
                                                  "Tested Students", "delete",
                                                  "Avg. Scaled Score", "delete", "delete"))%>%
      select(!contains("delete"))%>%
      #mutate(`M+E Count` = as.integer(`M+E Count`))%>%
      #mutate(`M+E%` = as.numeric(`M+E%`))%>%
      mutate(`E#` = as.integer(`E#`))%>%
      mutate(`E%` = as.numeric(`E%`))%>%
      mutate(`M#` = as.integer(`M#`))%>%
      mutate(`M%` = as.numeric(`M%`))%>%
      mutate(`PM#` = as.integer(`PM#`))%>%
      mutate(`PM%` = as.numeric(`PM%`))%>%
      mutate(`NM#` = as.integer(`NM#`))%>%
      mutate(`NM%` = as.numeric(`NM%`))%>%
      mutate(`Tested Students` = as.integer(`Tested Students`))%>%
      mutate(`Avg. Scaled Score` = as.integer(`Avg. Scaled Score`))%>%
      mutate(`Year` = year)%>%
      filter(`Subject`== subject)%>%
      # pivot_longer(contains("Count"), names_to = "Performance Level", values_to = "Performance Count")%>%
      # mutate(`Performance Level` = case_when(
      #               `Performance Level` == "E Count" ~ "E",
      #               `Performance Level` == "M Count" ~ "M",
      #               `Performance Level` == "PM Count" ~ "PM",
      #               `Performance Level` == "NM Count" ~ "NM"
      # ))%>%
      # mutate(`Performance Level` = recode_factor(`Performance Level`,
      #                                            "E" = "E",
      #                                            "M" = "M",
      #                                            "PM" = "PM",
      #                                            "NM" = "NM",
      #                                            .ordered = TRUE))%>%
      # mutate(`Performance%` = round(100*`Performance Count`/`Tested Students`))%>%
      # mutate(`State Tested` = sum(`Tested Students`))%>%
       select(`Year`, `Subject`, `School Name`, `School Code`, `Tested Students`, 
              `E#`, `E%`, `M#`, `M%`, `PM#`, `PM%`, `NM#`, `NM%`,`Avg. Scaled Score`)%>%
       #       `Performance Count`, `Performance%`, `Avg. Scaled Score`)%>%
      left_join(state_perf_sum, by= "Year")%>%
     # mutate(`State Performance%` = as.numeric(`State Performance%`))%>%
      #mutate(`Performance Diff` = `Performance%` - `State Performance%`)%>%
      select(`Year`, `Subject`, `School Name`, `School Code`, `Tested Students`, 
             `E#`, `E%`, `M#`, `M%`, `PM#`, `PM%`, `NM#`, `NM%`, 
             `E#State`, `E%State`, `M#State`, `M%State`, `PM#State`, `PM%State`, 
             `NM#State`, `NM%State`,
             `Avg. Scaled Score`, `State Avg. Scaled Score`)%>%
      filter(`School Code` != "00000000")
    
  }
}

## Read NextGenMCASItem School wide reports
# subject should be PHY or BIO,
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
       mutate(`Tested Students` = Tested)%>%
         select(`Year`, `Subject`, `School Name`, `School Code`, `Tested Students`, `ITEM`, `School%`)
       
   }
   
 }
 
 ## Read NextGenMCASItem District wide reports to extract state aggregate data
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

