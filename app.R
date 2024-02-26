library(shiny)
library(tidymodels)
library(reshape)
library(rsconnect)
library(data.table)
library(factoextra)



rm(list=ls())

file_path={my_data_path}


calendar <- seq(as.Date("2022-01-01",'%Y-%m-%d'),as.Date("2022-12-31",'%Y-%m-%d'),1)


#mall
DT_E_MALLID <- fread(file.path(file_path,"DT_E_MALLID.csv"),encoding = "UTF-8")
DT_E_GENDER <- fread(file.path(file_path,"DT_E_CATEGORY_GENDER.csv"),encoding = "UTF-8")
DT_E_AGE <- fread(file.path(file_path,"DT_E_CATEGORY_AGE.csv"),encoding = "UTF-8")
DT_E_STYLE<- fread(file.path(file_path,"DT_E_CATEGORY_STYLE.csv"),encoding = "UTF-8")
DT_E_CATEGORY<- fread(file.path(file_path,"DT_E_CATEGORY_CATEGORY.csv"),encoding = "UTF-8")


DT_E_GENDER$mallid <- as.character(DT_E_GENDER$mallid)
DT_E_AGE$mallid <- as.character(DT_E_AGE$mallid)
DT_E_STYLE$mallid <- as.character(DT_E_STYLE$mallid)
DT_E_CATEGORY$mallid <- as.character(DT_E_CATEGORY$mallid)
DT_E_MALLID$id <- as.character(DT_E_MALLID$id)




DT_E_GENDER1 <-  DT_E_GENDER %>% dplyr::filter(industry == "뷰티" | industry =="패션" | industry =="스포츠/레저") 
DT_E_GENDER1[industry=="스포츠/레저",3] <- "패션"
DT_E_GENDER2 <- DT_E_GENDER1 %>% mutate(gender="전체")
DT_E_GENDER1 <-  rbind(DT_E_GENDER1,DT_E_GENDER2)
DT_E_AGE1 <-  DT_E_AGE %>% dplyr::filter(industry == "뷰티" | industry =="패션" | industry =="스포츠/레저")
DT_E_AGE1[industry=="스포츠/레저",3] <- "패션"
DT_E_AGE2 <- DT_E_AGE1 %>% mutate(age="전체")
DT_E_AGE1 <- rbind(DT_E_AGE1,DT_E_AGE2)
DT_E_STYLE1 <-  DT_E_STYLE %>% dplyr::filter(industry == "뷰티" | industry =="패션" | industry =="스포츠/레저") 
DT_E_STYLE1[industry=="스포츠/레저",3] <- "패션"
DT_E_STYLE2 <- DT_E_STYLE1 %>% mutate(style="전체")
DT_E_STYLE1 <- rbind(DT_E_STYLE1,DT_E_STYLE2)
DT_E_CATEGORY1 <-  DT_E_CATEGORY %>% dplyr::filter(industry == "뷰티" | industry =="패션" | industry =="스포츠/레저")
DT_E_CATEGORY1[industry=="스포츠/레저",3] <- "패션"
DT_E_CATEGORY2 <- DT_E_CATEGORY1 %>% mutate(category="전체")
DT_E_CATEGORY1 <- rbind(DT_E_CATEGORY1,DT_E_CATEGORY2)



S_CATEGORY <- unlist(as.list(distinct(DT_E_CATEGORY1,category)),use.name=F)
S_STYLE <- unlist(as.list(distinct(DT_E_STYLE1,style)),use.name=F)
S_AGE <- unlist(as.list(distinct(DT_E_AGE1,age)),use.name=F)
S_GENDER <- unlist(as.list(distinct(DT_E_GENDER1,gender)),use.name=F)
S_AVERAGE_SALES <-  c("2억 미만","2-4억 사이","4-7억 사이","7-10억 사이","10억 이상","전체")

#product
FT_E_PRODUCT_PURCHASE <-  fread(file.path(file_path,"FT_E_PRODUCT_PURCHASE.csv"),encoding = "UTF-8")
FT_E_PRODUCT_PURCHASE$mallid <- as.character(FT_E_PRODUCT_PURCHASE$mallid)
DT_E_PRODUCT_INFO<- fread(file.path(file_path,"DT_E_PRODUCT_INFO.csv"),encoding = "UTF-8")

DT_E_PRODUCT_INFO <- DT_E_PRODUCT_INFO %>%
  filter(mallid_pid %in% unlist(distinct(DT_E_PRODUCT_INFO,mallid_pid),use.names = F)) %>%
  filter(product_code != "crema")


#np_cluster
FT_E_MALL_KEYWORD<- fread(file.path(file_path,"FT_E_MALL_KEYWORD.csv"),encoding = "UTF-8")



#formula
Y <- expression(exp^(-10*x+5))

ZZ <- D( expression( 1/(1+Y)) , "Y")
YY <- D( expression( exp^(-10*x+5)) , "x")
ZY <- paste("-(exp^(-10 * x + 5) * (log(exp) * 10))", "*" , "-(1/(1 + exp^(-10*x+5))^2)")

new_fomuula <- expression(-1/10*ln(1/x-1)+1/2)



#histogram_rand
ratio <- seq(from=0.01,to=1,by=0.01)
table1 <- as.data.frame(ratio)



##reviews histogram
FT_E_MALL_REVIEWS<- fread(file.path(file_path,"FT_E_MALL_REVIEWS.csv"),encoding = "UTF-8")


FT_E_MALL_REVIEWS <- FT_E_MALL_REVIEWS %>%
  mutate(datediff = as.numeric(difftime(created_at,delivered_final,  units = "days")))

FT_E_MALL_REVIEWS <- FT_E_MALL_REVIEWS[complete.cases(FT_E_MALL_REVIEWS$delivered_final),]


quant1 <- quantile(FT_E_MALL_REVIEWS$datediff)[[2]]
quant3 <- quantile(FT_E_MALL_REVIEWS$datediff)[[4]]

iqr <-  quant3-quant1
min_range <- quant1-1.5*iqr
max_range <- quant3+1.5*iqr

FT_E_MALL_REVIEWS <- FT_E_MALL_REVIEWS %>%
  filter(FT_E_MALL_REVIEWS$datediff>= min_range & FT_E_MALL_REVIEWS$datediff <= max_range)












#dashboard
ui <- fluidPage(
  shinythemes::themeSelector(),
  sidebarPanel(  
    sliderInput("calendar","CALENDAR",min = min(calendar), max = max(calendar),value= c(min(calendar),max(calendar)) ),    
    selectInput("category","CATEGORY",choices = S_CATEGORY,"전체"),
    selectInput("age","AGE",choices = S_AGE,"전체"),
    selectInput("gender","GENDER",choices = S_GENDER,"전체"),
    selectInput("style","STYLE",choices = S_STYLE,"전체"),
    selectInput("average_sales","AVERAGE_SALES",choices = S_AVERAGE_SALES,"전체"),
    radioButtons("cluster","CLUSTER",c("1CLUSTER"=1,
                                       "2CLUSTER"=2,
                                       "3CLUSTER"=3,
                                       "4CLUSTER"=4),1),
    sliderInput("hist_bin","HIST_BIN",min=20,max=60,value=40),
    radioButtons("s","select",c("penalty1"="Exp","penalty2"="Exp2")),
    tags$hr(),
    p("penalty1 select"),
    numericInput("e","exp",2,min=1,max=10),
    tags$hr(),
    p("penalty2 select"),
    sliderInput("a","A",min=0,max=20,value=10),
    sliderInput("b","B",min=0,max=1,value=0.5,step=0.1),
    tags$hr(),
    p("hist rbeta select"),
    sliderInput("c","C",min=1,max=49,value=25),
    tags$hr(),
    p("etc"),
    helpText(ZY)
  ),
  
  mainPanel(

  tabsetPanel(
    tabPanel("FORMULA",plotOutput("plot1")),
    tabPanel("HIST",plotOutput("plot2")),
    tabPanel("FILTERED_MALLID",textOutput("count1")),
    tabPanel("MALL_CLUSTER",plotOutput("plot3")),
    tabPanel("NEW_FILTERED_MALL",textOutput("count2")),
    tabPanel("CMALL_STATUS",tableOutput("table1")),
    tabPanel("CNP_STATUS",tableOutput("table2")),
    tabPanel("REVIEW_DATE_HIST",plotOutput("plot4")),
    tabPanel("CNP_boxplot",plotOutput("plot5"))
  ))
  )



server <- function(input,output,session){
  
  DT_E_MALLID_filter <- reactive({
    
    FT_E_PRODUCT_PURCHASE_sum <- 
      left_join(FT_E_PRODUCT_PURCHASE,(DT_E_PRODUCT_INFO %>% select(mallid_pid,org_price_cents)), by ="mallid_pid") %>%
      filter(delivered_final>= min(input$calendar) & delivered_final<= max(input$calendar)) %>%
      mutate(order_value=org_price_cents*sub_orders_count) %>%
      group_by(mallid) %>%
      summarise(order_values=sum(order_value)/
                  as.numeric(
                    if(difftime(max(delivered_final),min(delivered_final),  units = "days")==0)
                    {1} else
                      difftime(max(delivered_final),min(delivered_final),  units = "days"))*30) %>%
      mutate(criteria=case_when(
        is.na(order_values) ~ "2억 미만",
        order_values<200000000 ~ "2억 미만",
        order_values>=200000000 & order_values<400000000 ~ "2-4억 사이",
        order_values>=400000000 & order_values<700000000 ~ "4-7억 사이",
        order_values>=700000000 & order_values<1000000000 ~ "7-10억 사이",
        order_values>=1000000000 ~ "10억 이상")
      )
    FT_E_PRODUCT_PURCHASE_sum2 <- FT_E_PRODUCT_PURCHASE_sum %>% mutate(criteria="전체")
    FT_E_PRODUCT_PURCHASE_sum3 <- rbind(FT_E_PRODUCT_PURCHASE_sum ,FT_E_PRODUCT_PURCHASE_sum2 )
    
    
    DT_E_MALLID_JOIN1<-  
      inner_join(DT_E_MALLID, (DT_E_CATEGORY1 %>% mutate(id = DT_E_CATEGORY1$mallid)%>%select(id,category,industry)), by="id") %>%
      inner_join( (DT_E_GENDER1 %>% mutate(id = DT_E_GENDER1$mallid)%>%select(id,gender)), by="id") %>%
      inner_join( (DT_E_AGE1 %>% mutate(id = DT_E_AGE1$mallid)%>%select(id,age)), by="id") %>%
      inner_join( (DT_E_STYLE1 %>% mutate(id = DT_E_STYLE1$mallid)%>%select(id,style)), by="id") %>%
      inner_join( (FT_E_PRODUCT_PURCHASE_sum3 %>% mutate(id = FT_E_PRODUCT_PURCHASE_sum3$mallid) %>% select(id,criteria)), by="id") %>%
      filter(category==input$category & 
               age==input$age &
               gender==input$gender &
               style==input$style &
               criteria==input$average_sales &
               industry == "패션")
    
    DT_E_MALLID_filter <- unlist(distinct(DT_E_MALLID_JOIN1,id),use.names = F)
    DT_E_MALLID_filter
    
  })
  
  
  FT_E_MALL_KEYWORD_count_cast_cleaning <- reactive({
    
    FT_E_MALL_KEYWORD_count <- FT_E_MALL_KEYWORD %>%
      filter(mallid %in% DT_E_MALLID_filter()) %>%
      filter(industry == 2) %>%
      filter(created_at>= min(input$calendar) & created_at<= max(input$calendar)) %>%
      filter(type!="count_sentiment") %>%
      group_by(mallid,theme,type) %>%
      summarise(count=sum(total_count))
    
    FT_E_MALL_KEYWORD_count$type <- gsub("count_nsentiment","N",FT_E_MALL_KEYWORD_count$type)
    FT_E_MALL_KEYWORD_count$type <- gsub("count_psentiment","P",FT_E_MALL_KEYWORD_count$type)
    FT_E_MALL_KEYWORD_count_cast <- cast(FT_E_MALL_KEYWORD_count,mallid~theme+type)
    FT_E_MALL_KEYWORD_count_cast_cleaning <- as.data.frame(na.omit(FT_E_MALL_KEYWORD_count_cast))
    FT_E_MALL_KEYWORD_count_cast_cleaning
    
  })
  
  FT_E_MALL_KEYWORD_count_cast_cleaning2 <- reactive({
    
    
    FT_E_MALL_KEYWORD_count_cast_cleaning <- FT_E_MALL_KEYWORD_count_cast_cleaning()[,-1]
    FT_E_MALL_KEYWORD_count_cast_cleaning2 <- scale(FT_E_MALL_KEYWORD_count_cast_cleaning)
    FT_E_MALL_KEYWORD_count_cast_cleaning2
    
  })
  
  kdist <- reactive({
    kdist <- kmeans(FT_E_MALL_KEYWORD_count_cast_cleaning(), center=4)
    kdist$cluster <- as.factor(kdist$cluster)
    kdist 
    
  })
  
  
  new_mall_filter <- reactive({
    kdist_cluster <- as.data.frame(kdist()$cluster)
    kdist_cluster_mallid <- FT_E_MALL_KEYWORD_count_cast_cleaning()[,1]
    kdist_cluster_mallid <- cbind(kdist_cluster,kdist_cluster_mallid)
    names(kdist_cluster_mallid) <- c("cluster","mallid")
    kdist_cluster_mallid <- kdist_cluster_mallid %>%
      filter(cluster == input$cluster)
    
    kdist_cluster_mallid
    
    
    kdist_cluster_mallid_filter <- unlist(distinct(kdist_cluster_mallid,mallid),use.names = F)
    kdist_cluster_mallid_filter
  })
  
  CMALL_STATUS <- reactive({
    
    DT_E_MALLID_JOIN1<-  
      inner_join(DT_E_MALLID, (DT_E_CATEGORY1 %>% mutate(id = DT_E_CATEGORY1$mallid)%>%select(id,category,industry)), by="id") %>%
      inner_join( (DT_E_GENDER1 %>% mutate(id = DT_E_GENDER1$mallid)%>%select(id,gender)), by="id") %>%
      inner_join( (DT_E_AGE1 %>% mutate(id = DT_E_AGE1$mallid)%>%select(id,age)), by="id") %>%
      inner_join( (DT_E_STYLE1 %>% mutate(id = DT_E_STYLE1$mallid)%>%select(id,style)), by="id") %>%
      filter(id %in% new_mall_filter() & 
               category != "전체"& 
               age != "전체"& 
               gender != "전체"& 
               style != "전체")
    
    DT_E_MALLID_JOIN1
  })
  
  CNP_STATUS <- reactive({
    
    CNP_STATUS <- FT_E_MALL_KEYWORD_count_cast_cleaning() %>%
      filter(mallid %in% new_mall_filter()) 
    
    CNP_STATUS
  })
  
  boxplot <- reactive({
    
    boxplot <-  FT_E_MALL_KEYWORD_count_cast_cleaning() %>%
      filter(mallid %in% new_mall_filter()) %>%
      mutate(mallid = as.factor(mallid)) %>%
      melt(id.vars=1,measure.vars=c(2:length(FT_E_MALL_KEYWORD_count_cast_cleaning())))
    
    boxplot
    
  })
  
  textformula <- reactive({
    
    aa <- paste("1/exp(1)^",as.character(input$e) )
    bb <- paste("1/(1+exp(1)^((-", input$a, ")*(x-", input$b, ")))")
    switch(input$s,"Exp"=aa,"Exp2"=bb)  
    
  })
  
  table2 <- reactive({
    
    table_a <- 
      table1 %>% 
      mutate(
        formula=1/exp(ratio)^input$e
      )
    
    table_b <- 
      table1 %>% 
      mutate(
        formula=1/(1+exp(1)^((-input$a)*(ratio-input$b)))
      )
    
    switch(input$s,"Exp"=table_a,"Exp2"=table_b)
    
  })
  
  
  output$plot1 <- renderPlot({
    ggplot(table2()) +
      geom_line(aes(y=formula,x=ratio)) +
      coord_cartesian(xlim = c(0, 1), ylim = c(0,1))+
      annotate("text", x = 0.5, y = 0.95, size = 6,label = textformula() )
  })
  
  output$plot2 <- renderPlot({
    
    ggplot()+
      geom_histogram(aes(x=rbeta(10000, input$c, 50-input$c)),binwidth=0.005)+
      geom_vline(xintercept=mean(rbeta(10000, input$c, 50-input$c)),color='red', size=1)+
      geom_vline(xintercept=median(rbeta(10000, input$c, 50-input$c)),color='blue', size=1)
    
  })
  
  output$count1 <- renderText({
    as.character(paste(DT_E_MALLID_filter(),COLLAPSE=" "))
    
  })
  
  
  output$plot3 <- renderPlot({
    fviz_cluster(kdist(),FT_E_MALL_KEYWORD_count_cast_cleaning2())
  })
  
  output$count2 <- renderText({
    as.character(paste(new_mall_filter(),COLLAPSE=" "))
  }) 
  
  output$table1 <- renderTable({
    CMALL_STATUS()
  }) 
  
  output$table2 <- renderTable({
    CNP_STATUS()
  }) 
  
  output$plot4 <- renderPlot({
    ggplot(FT_E_MALL_REVIEWS)+
      geom_histogram(aes(x=FT_E_MALL_REVIEWS$datediff),bins=input$hist_bin)
  })
  
  output$plot5 <- renderPlot({
    
    ggplot(boxplot())+
      geom_boxplot(aes(x=reorder(variable,-value),y=value), show.legend = FALSE) +
      theme_bw()
    
  })
  
  
  
}

shinyApp(ui,server)