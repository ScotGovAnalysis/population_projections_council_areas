# dataset ----------------------------------------------------------------------
pop<-"#2DA197"

#load("dummy_dataset.RData")
load("final_dataset.RData")

list_of_places = as.list(as.character(unique(dat$Area)))
list_of_variants = as.list(as.character(unique(dat$Projection)))
list_of_age_group = as.list(as.character(unique(dat$Age_group)))

shinyServer(function(input, output){

#########
# Tab 1 #
#########

    output$my_plot1<- renderPlot({
    
    # --------------------------------------------------------------------------
    # preparing dataset
    # --------------------------------------------------------------------------
    #
    # Keep this order, othewise it might not work properly. The dataset needs to
    # be aggregated (summarised) before Perc_change can be calculated.
    
    # Aggregating --------------------------------------------------------------
    total_dat <- group_by(dat, Projection, Area, Year) %>%
      summarise(Base=sum(Base),
                Population=sum(Population)) %>%
      mutate(Perc_change=(Population-Base)/Base*100)
    
    # Allocating variable to plot ----------------------------------------------
    ifelse(input$Type1=="Absolute change",
           total_dat$change<-total_dat$Population,
           total_dat$change<-total_dat$Perc_change)
    
    # Filtering observations to plot -------------------------------------------
    plot_data1<-filter(total_dat, Area %in% input$Area1 &
                                  Projection %in% input$Variants1 &
                                  Year <= input$end_year1)  
    
    # --------------------------------------------------------------------------
    # plotting dataset
    # --------------------------------------------------------------------------
    
    ggplot(data = plot_data1, mapping=aes(x=Year, 
                                          y=change,
                                          group=interaction(Area, Projection),
                                          colour=Area,
                                          size=Area)) +
      geom_line() +
      
      scale_size_manual(values = rep(1.25, length(unique(plot_data1$Area)))) +
      
      #scale_colour_manual(values = rep(pop, length(unique(plot_data1$Area)))) +
      
      scale_x_continuous(limits = range(plot_data1$Year)+c(0, diff(range(plot_data1$Year)/2))) +
      
      scale_y_continuous(name=NULL, labels = scales::comma, 
                         limits = range(subset(total_dat$change, 
                                               total_dat$Area %in% input$Area1 &
                                               total_dat$Year <= input$end_year1))) +
      

      geom_text_repel(data = subset(plot_data1, Year == max(Year)),
                      aes(label = paste(Area, Projection, sep=" - ")),
                      size = 4,
                      nudge_x = 3,
                      show.legend =FALSE,
                      segment.color = NA)+
      theme(legend.position="none") +
      
      labs(title=paste("Population Projections for Selected Areas"),
           subtitle=paste(input$Type1, ", ",
                          min(dat$Year), " - ", 
                          input$end_year1, 
                          sep=""))
    # --------------------------------------------------------------------------
    # plotting dataset
    # --------------------------------------------------------------------------
     

  })
  
  #########
  # Tab 2 #
  #########
    output$my_plot2<- renderPlot({
      
      # Aggregating --------------------------------------------------------------
      total_dat2 <- group_by(dat, Projection, Area, Year) %>%
        
        summarise(Base=sum(Base),
                  Population=sum(Population)) %>%
        
        mutate(Perc_change=(Population-Base)/Base*100)
      
      # Allocating variable to plot ----------------------------------------------
      ifelse(input$Type1=="Absolute change",
             total_dat2$change<-total_dat2$Population,
             total_dat2$change<-total_dat2$Perc_change)
      
      # Filtering observations to plot -------------------------------------------
      plot_data2<-filter(total_dat2, Area %in% input$Area1 &
                           Projection %in% input$Variants1 &
                           Year <= input$end_year1)  

      plot_data2 <- group_by(dat, Year, Area) %>%
                 filter(Area %in% input$Area2 &
                 Projection %in% input$Variants2 &
                 Year %in% c(input$start_year2, input$end_year2))
      
      # Get max limits of data for all years - so plot limits remain equal all years 
      plot_limits <- group_by(dat, Year, Area) %>% filter ( Area %in% input$Area2)
      max_pop <- max(plot_limits$Population)

      # Plot lines/area ----------------------------------------------
      plot_solid <- filter(plot_data2, Year==input$start_year2)
      plot_line <- filter(plot_data2, Year==input$end_year2)
      
      plot_annot<-data.frame(x=c(5,5), 
                             y=c(-max(plot_data2$Population),
                                 max(plot_data2$Population))*1.15,
                             value=c("Males", "Females"))
      
       #Code to set break lines in plot area - values depend on the population numbers
          #For places with high population we need wider breaks so that the break labels do not overlap.
          #Scotland - breaks every 20,000. Edinburgh, Glasgow and Fife - larger populations so break every 2000
          #IF max population across all years is greater than 1000 - break every 1000
          # otherwise every 200 - for generally small populations
      
      if (input$Area2=="Scotland"){grid<-seq(-80000,80000,20000)}
      else if (input$Area2 %in% c("City of Edinburgh", "Glasgow City", "Fife")){grid <-seq(-10000, 10000, 2000)} 
      else if (max_pop>1000){grid <-seq(-10000, 10000, 1000)} 
      else {grid <- seq(-10000, 10000, 200)}
      
      # --------------------------------------------------------------------------
      # plotting dataset
      # --------------------------------------------------------------------------
      ggplot(plot_solid) +
        
        geom_bar(aes(x=Age, y=Population), fill="grey80", colour="grey80",
                 stat="identity", subset(plot_solid, plot_solid$Gender=="F"))  +
        
        geom_bar(aes(x=Age, y=-Population), fill="grey80", colour="grey80",
                 stat="identity", subset(plot_solid, plot_solid$Gender=="M"))  +
        
        scale_fill_manual(values = c("Males" = "grey70", "Females" = "grey70")) +
        
        geom_step(aes(x=Age-0.5, y=Population),
                  data=subset(plot_line, plot_line$Gender=="F"),
                  size=1.1, colour=pop) +
        
        geom_step(aes(x=Age-0.5,y=-Population),
                  data=subset(plot_line,  plot_line$Gender=="M"),
                  size=1.1, colour=pop) +
        
        geom_hline(yintercept = 0, colour = "white") +
        
        geom_vline(xintercept = c(16, 64), lty=3) +
        
        geom_text(data=plot_annot, aes(x, y, label=value), hjust="inward", fontface="bold") +
        
        scale_x_continuous(expand = c(0,0), breaks = NULL) +
        
        scale_y_continuous(expand = c(0.1,0.1), 
                            labels = scales::comma(abs(grid)),
                             breaks = grid,
                             limits = c(-max_pop, 
                                        max_pop)*1.2) +
        coord_flip() +
        labs(title=paste(input$Area2),
             subtitle=paste("Projected Population by age and gender\n", 
                            input$Variants2, " projection, ", 
                            input$start_year2, " (solid) and ", 
                            input$end_year2, " (line)", sep=""))
    })
    
  
  #########
  # Tab 3 #
  #########
  output$my_plot3a<- renderPlot({
    
    
    # Aggregating --------------------------------------------------------------
    total_dat3a <- group_by(dat, Projection,Age_group, Area, Year ) %>%
      
      summarise(Base=sum(Base),
      Population=sum(Population)) %>%
      
      mutate(Perc_change=(Population-Base)/Base*100)
    
    # Allocating variable to plot ----------------------------------------------
    ifelse(input$Type3=="Absolute change",
         
           total_dat3a$change<-total_dat3a$Population,
           total_dat3a$change<-total_dat3a$Perc_change)
    
    # Filtering observations to plot -------------------------------------------
    plot_data3<-filter(total_dat3a, Area %in% input$Area1 &
                         Projection %in% input$Variants1 &
                         Year <= input$end_year1)  
    

    # --------------------------------------------------------------------------
    # preparing dataset
    # --------------------------------------------------------------------------
    
    plot_data3 <- group_by(dat, Projection, Area, Age_group, Year) %>%
      
      summarise(Base = sum(Base), 
                Population = sum(Population)) %>%
      
      mutate(Perc_change=(Population-Base)/Base*100) %>%
      
      filter(Area %in% input$Area3 &
             Projection %in% input$Variants3 &
             Year %in% c(min(Year), input$end_year3))
    
    # --------------------------------------------------------------------------
    # plotting dataset
    # --------------------------------------------------------------------------
    ggplot(data = plot_data3) +
      geom_bar(mapping=aes(x=Age_group,
                           y=Population, #change
                           group=interaction(Year, Area),
                           fill=Area, alpha=factor(Year)),
                           stat="identity", position = "dodge") +

      #scale_fill_manual(values = c("Scotland" = pop, # "United Kingdom" = "grey40")) +
      
      scale_alpha_discrete(name="Year", range=c(0.5, 1)) +  
      
      scale_y_continuous(name=NULL, expand=c(0.1, .01), labels=scales::comma,
                         limits = c(0, max(plot_data3$Population*1.25)))+
      
      scale_x_discrete(name="Age group", labels=str_wrap(levels(dat$Age_group), width=8))+
      
      labs(title=paste("Population Projections by Age Group for Selected Areas"),
               subtitle=paste( input$Variants3,"projection","\nAbsolute change,", "2016 and", input$end_year3))
    })

  output$my_plot3b<- renderPlot({
    
    # --------------------------------------------------------------------------
    # preparing dataset
    # --------------------------------------------------------------------------
    plot_data3 <- group_by(dat, Projection, Area, Age_group, Year) %>%
      summarise(Base = sum(Base),
                Population = sum(Population)) %>%

      mutate(Perc_change=(Population-Base)/Base*100) %>%
      
      filter(Area %in% input$Area3 &
               Projection %in% input$Variants3 &
               Year %in% c(min(Year), input$end_year3))

    # --------------------------------------------------------------------------
    # plotting dataset
    # --------------------------------------------------------------------------
     ggplot(data = plot_data3) +
      geom_bar(mapping=aes(x=Age_group, y=Perc_change, group=Area, fill=Area),
               label = rownames(pop), stat="identity", position = "dodge") +
      
      #scale_fill_manual(values = c("Scotland" = pop,"United Kingdom" = "grey40")) +
      
      scale_y_continuous(name=NULL, expand=c(0.1, 0.1), labels=scales::comma,
                         limits = range(plot_data3$Perc_change)*1.1)+
      
      scale_x_discrete(name="Age group", labels=str_wrap(levels(dat$Age_group), width=8))+
      
      labs(title=paste("Population Projections by Age Group for Selected Areas"),
           subtitle=paste(input$Variants3,"projection","\nPercentage change,", "2016 and", input$end_year3))
   
  })
})
