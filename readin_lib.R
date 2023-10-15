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
                                                                   "delete", "delete", "delete", "E%", 
                                                                   "delete", "M%", "delete",
                                                                   "PM%", "delete", "NM%", 
                                                                   "delete", "delete",
                                                                   "State Avg. Scaled Score", 
                                                                   "delete", "delete"))%>%
      select(!contains("delete"))%>%
      filter(`Subject`== subject) %>%
      filter(`School Code` == "00000000")%>%
      pivot_longer(contains("%"), names_to = "Performance Level", values_to = "State Performance%")%>%
      mutate(`Performance Level` = case_when(
        `Performance Level` == "E%" ~ "E",
        `Performance Level` == "M%" ~ "M",
        `Performance Level` == "PM%" ~ "PM",
        `Performance Level` == "NM%" ~ "NM"
      ))%>%
      mutate(`Performance Level` = recode_factor(`Performance Level`,
                                                 "E" = "E",
                                                 "M" = "M",
                                                 "PM" = "PM",
                                                 "NM" = "NM",
                                                 .ordered = TRUE))%>%
      select(`Performance Level`, `State Performance%`, `State Avg. Scaled Score`)
    #view(state_perf_sum)
    read_excel(file_path, skip = 1, col_names = c("School Name", "School Code", "Subject", 
                                                  "delete", "delete", "E Count", "delete", 
                                                  "M Count", "delete", "PM Count",
                                                  "delete", "NM Count", "delete", 
                                                  "Tested Students", "delete",
                                                  "Avg. Scaled Score", "delete", "delete"))%>%
      select(!contains("delete"))%>%
      #mutate(`M+E Count` = as.integer(`M+E Count`))%>%
      #mutate(`M+E%` = as.numeric(`M+E%`))%>%
      mutate(`E Count` = as.integer(`E Count`))%>%
      #mutate(`E%` = as.numeric(`E%`))%>%
      mutate(`M Count` = as.integer(`M Count`))%>%
      #mutate(`M%` = as.numeric(`M%`))%>%
      mutate(`PM Count` = as.integer(`PM Count`))%>%
      #mutate(`PM%` = as.numeric(`PM%`))%>%
      mutate(`NM Count` = as.integer(`NM Count`))%>%
      #mutate(`NM%` = as.numeric(`NM%`))%>%
      mutate(`Tested Students` = as.integer(`Tested Students`))%>%
      mutate(`Avg. Scaled Score` = as.integer(`Avg. Scaled Score`))%>%
      mutate(`Year` = year)%>%
      filter(`Subject`== subject)%>%
      pivot_longer(contains("Count"), names_to = "Performance Level", values_to = "Performance Count")%>%
      mutate(`Performance Level` = case_when(
                    `Performance Level` == "E Count" ~ "E",
                    `Performance Level` == "M Count" ~ "M",
                    `Performance Level` == "PM Count" ~ "PM",
                    `Performance Level` == "NM Count" ~ "NM"
      ))%>%
      mutate(`Performance Level` = recode_factor(`Performance Level`,
                                                 "E" = "E",
                                                 "M" = "M",
                                                 "PM" = "PM",
                                                 "NM" = "NM",
                                                 .ordered = TRUE))%>%
      mutate(`Performance%` = round(100*`Performance Count`/`Tested Students`))%>%
      mutate(`State Tested` = sum(`Tested Students`))%>%
      select(`Year`, `Subject`, `School Name`, `School Code`, `Tested Students`, `Performance Level`, 
             `Performance Count`, `Performance%`, `Avg. Scaled Score`)%>%
      left_join(state_perf_sum, by= "Performance Level")%>%
      select(`Year`, `Subject`, `School Name`, `School Code`, `Tested Students`, 
             `Performance Level`, `Performance Count`, `Performance%`, `State Performance%`, 
             `Avg. Scaled Score`, `State Avg. Scaled Score`)
    
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