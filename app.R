source("global.R")
library(ggnewscale)
open_dataset("data/fbs") ->fbs
open_dataset("data/sua") ->sua
open_dataset("data/consumerprices") ->cprices

library(shinyTree)
suaitems <- readRDS("data/suaitems.rds")
sua.item.list <- readRDS("data/suaitemlist.rds")
item.list <- readRDS("data/fbssuaitemlist.rds")
fbs.sua.items <- rbind(unique(suaitems[,.(Code=Code1,Item="Grand Total",Group="FBS")]),
                       unique(suaitems[,.(Code=Code2,Item=Code2Desc,Group="FBS")]),
                       unique(suaitems[,.(Code=Code3,Item=Code3Desc,Group="FBS")]),
                       unique(suaitems[,.(Code=FBS_Item_Code,Item=FBSName,Group="FBS")]),
                       unique(suaitems[,.(Code=SUA_Item_Code,Item=SUA_Item_Name,Group="SUA")])
                       )

fbs |> select(Area,Area_Code) |> unique() |>
    as.data.table() -> fbs.areas
fbs.areas[order(Area_Code)]->fbs.areas
fbs.areas$Area[c(82,175)] <- c("Code d'Ivore", "Turkey")

fbs |> select(Element,Element_Code,Unit) |>
    unique() |> as.data.table() |>
    subset(Element_Code!=511) -> fbs.elements
fbs.elements[Unit=="1000 t",Scale:="Quantity"]
fbs.elements[Element_Code==5301,Element:="Domestic supply"]
fbs.elements[Element_Code==661,Element:="Domestic supply"]
fbs.elements[Element_Code==681,Element:="Domestic supply"]
fbs.elements[Element_Code==671,Element:="Domestic supply"]
fbs.elements[Element_Code==661,Scale:="Calories"]
fbs.elements[Element_Code==681,Scale:="Fat"]
fbs.elements[Element_Code==671,Scale:="Protein"]
fbs.elements[is.na(Scale),Scale:="Per capita"]
fbs.elements[,.(ElementNew=Element,Element_Code,Scale)]->fbs.elements

sua |> select(Element,Element_Code,Unit) |>
    unique() |> as.data.table() |>
    subset(Element_Code!=511) -> sua.elements
sua.elements[Unit=="t",Scale:="Quantity"]
sua.elements[Unit=="Kcal",Scale:="Calories"]
sua.elements[Unit=="g/cap/d",Scale:="Per capita"]
sua.elements[Unit=="kcal/cap/d",Scale:="Per capita"]
sua.elements[,.(ElementNew=Element,Element_Code,Scale)]->sua.elements

fbs |> filter(Series=="New") |> select(Year) |> unique() |> as.data.table()->yearrange

fbsplot <- function(data1=fbs1, data2=fbs2, item, percap, flag) {
    if (item != "All") {
        data1 <- data1 |> filter(Item_Code==item)
        data2 <- data2 |> filter(Item_Code==item)
    }
    ggplot()+
        geom_bar(data = data1, stat="identity",
                 aes(x=Year,y=Value,group=Element,fill=Element),
                 width=0.3, size=0.1)+
        scale_fill_viridis_d(name="Supply", direction=-1)+
        new_scale_fill()+
        geom_bar(data = data2, stat="identity",
                 aes(x=Year,y=Value,group=Element,fill=Element),
                 width=0.3, size=0.1) +
        scale_fill_brewer(name="Utilization",palette="Set1", direction=-1)+
        facet_wrap(~Item, ncol=1, scales="free_y")->p


    if(percap==TRUE) {
        p <- p + scale_y_continuous("Kilograms per capita")
    } else if (item < 2000) {
        p <- p + scale_y_continuous("Tonnes")
    } else {
        p <- p + scale_y_continuous("Thousand tonnes")
    }
    if(flag==TRUE) {
        p <- p+geom_text(data=data1,
                         aes(x=Year, y=Value, label=Flag,
                             group=Element), angle=90,
                         position = position_stack(vjust = 0.5),
                         size=3,vjust="middle",lineheight=0.3
                         )+
            geom_text(data=data2,
                      aes(x=Year, y=Value, label=Flag,
                          group=Element), angle=90,

                      position = position_stack(vjust = 0.5),
                      size=3,vjust="middle",lineheight=0.3
                      )
    }
    return(p)
}

calplot <- function(data1=fbs1, data2=fbs2, data3=food,
                    unit, percap, fillscale) {
    datalabels<-rbind(data1[,.(Label="S", yposition=-round(max(data1$Value)/50)),Year],
                      data2[,.(Label="U", yposition=-round(max(data1$Value)/50)),Year],
                      data3[,.(Label="F", yposition=-round(max(data1$Value)/50)),Year])
    p <- ggplot()+
        geom_bar_interactive(data = data1, stat="identity",
                             aes(x=Year,y=Value,group=Element,fill=Element,
                                 tooltip=paste0(Element," (",
                                                round(Value),
                                                ")")),
                 width=0.22, linewidth=0.1)+
        scale_fill_viridis_d(name="Supply (S)", direction=-1)+
        new_scale_fill() +
        geom_bar_interactive(data = data2, stat="identity",
                             aes(x=Year,y=Value,group=Element,fill=Element,
                                 tooltip=paste0(Element," (",
                                                round(Value),
                                                ")")),
                             width=0.22, linewidth=0.1) +
        scale_fill_manual(name = "Utilization (U)",
                          values = fillscale) +
        # scale_fill_viridis_d(name="Supply", direction=-1)+
        new_scale_fill() +
        geom_bar_interactive(data = data3, stat="identity",
                             aes(x=Year,y=Value,group=Item,fill=Item,
                                 tooltip=paste0(Item," (",
                                                round(Value),
                                                ")")),
                             width=0.22, linewidth=0.1) +
        paletteer::scale_fill_paletteer_d("PrettyCols::Autumn", name="Food (F)",
                          guide = guide_legend(position="bottom")) +
        ## scale_fill_discrete(name = "Food") +
        geom_text(data=datalabels, aes(x=Year, y=yposition, label=Label), size=3) +
        scale_x_continuous("Year", breaks = c(2012, 2016, 2020)) +
        scale_y_continuous(unit)
    return(p)
}


priceplot <- function(pricedata){
  p <- ggplot(pricedata, aes(x=Year, y=Value, group = Area)) +
      geom_point() +
      geom_line() +
      scale_x_continuous("Year",
                         limits = c(min(pricedata$Year) - 0.22,
                                    max(pricedata$Year) + 0.22),
                         breaks = c(2012, 2016, 2020)) +
      ggtitle(paste0("Consumer price index for food in ",pricedata$Area[1])) +
      theme(plot.margin = margin(t = 5.5, r = 140, b = 5.5, l = 5.5, unit = "pt"))
  return(p)
}

## ,"#999999"
world_data <- ggplot2::map_data('world')
world_data <- fortify(world_data)
countrycode(world_data$region,origin="country.name",destination="iso3c")->world_data$Country.Code
world_data$sno<-c(1:nrow(world_data))
map_theme <- function () {
    theme_bw() + theme(axis.text = element_blank(),
                       axis.title = element_blank(),
                       strip.text = element_text(size = 14),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(),
                       legend.position = "bottom",
                       panel.border = element_blank(),
                       strip.background = element_rect(fill = 'white', colour = 'white'))
}


ui <- fluidPage(
    titlePanel("Analysis of food balances"),
    includeCSS("../../solarized-dark.css"),
    includeCSS("../../styles.css"),
    tabsetPanel(
        tabPanel("SUA data",
                 HTML("<br><br>"),
                 sidebarLayout(
                     sidebarPanel(sliderInput("selectcalyearsin",
                                              "Select the range of years to be included in the plot:",
                                              min = min(yearrange$Year),
                                              max= max(yearrange$Year),
                                              value = c(min(yearrange$Year),max(yearrange$Year))
                                              ),
                                  selectInput("selectcalcountriesin",
                                              "Select the countries/regions for which you want to see the trends:",
                                              choices = fbs.areas$Area,
                                              selected="World"
                                              ),
                                  checkboxInput("calpercapita",
                                                "Compute per capita per day values",
                                                value=TRUE),
                                  checkboxInput("calprocessing",
                                                "Show use for processing separately",
                                                value=FALSE),
                                  selectInput("selectcalgm",
                                              "Plot quantity (tonnes/gms) or calories:",
                                              choices = c("Quantity","Calories"),
                                              selected="Calories"
                                              ),
                                  ## checkboxInput("calflags",
                                  ##               "Show data flags",
                                  ##               value=TRUE),
                                  h3("Select the items/groups for which you want to see the trends:"),
                                  shinyTree("selectcalitems", checkbox=TRUE),
                                  numericInput("itemnos",
                                               "Number of top items to be displayed",
                                               min=1, max=7,
                                               value=5
                                               ),
                                  actionButton("showcalplot", label="Plot"),
                                  width=2),
                     mainPanel(
                         id="maintab0",
                         uiOutput("giraphcalplot"),
                         ))
                 ),
        tabPanel("Trends",
                 HTML("<br><br>"),
                 sidebarLayout(
                     sidebarPanel(sliderInput("selecttrendyearsin",
                                              "Select the range of years to be included in the plot:",
                                              min = min(yearrange$Year),
                                              max= max(yearrange$Year),
                                              value = c(min(yearrange$Year),max(yearrange$Year))
                                              ),
                                  selectInput("selectcountriesin",
                                              "Select the countries/regions for which you want to see the trends:",
                                              choices = fbs.areas$Area,
                                              selected="World"
                                              ),
                                  checkboxInput("percapita",
                                                "Compute per capita values",
                                                value=FALSE),
                                  checkboxInput("flags",
                                                "Show data flags",
                                                value=TRUE),
                                  h3("Select the items/groups for which you want to see the trends:"),
                                  shinyTree("selectitems", checkbox=TRUE),
                                  actionButton("showplot", label="Plot"),
                                  width=3),
                     mainPanel(
                         id="maintab2",
                         uiOutput("giraphplot"),
                         ))
                 ),
        tabPanel("Maps",
                 HTML("<br><br>"),
                 sidebarLayout(
                     sidebarPanel(sliderInput("selectmapyearsin",
                                              "Select the range of years to be included in the plot:",
                                              min = min(yearrange$Year),
                                              max= max(yearrange$Year),
                                              value = c(min(yearrange$Year),max(yearrange$Year))
                                              ),
                                  h3("Select the items/groups for which you want to see the trends:"),
                                  shinyTree("selectmapitems", checkbox=TRUE),
                                  selectInput("selectmapelementin",
                                              "Select the variable to be mapped:",
                                              choices = sua.elements$ElementNew,
                                              selected="Food supply quantity (tonnes)"
                                              ),
                                  checkboxInput("mappercapita",
                                                "Compute per capita values",
                                                value=FALSE),
                                  checkboxInput("mapflags",
                                                "Show data flags",
                                                value=TRUE),
                                  actionButton("showmap", label="Map"),
                                  width=3),
                     mainPanel(
                         id="maintab3",
                         uiOutput("giraphmapplot"),
                         ))
                 )
    )
)



server <- function(input, output, session) {

    outvalues <- reactiveValues(plot1=NULL)

    output$selectitems <- renderTree({
        item.list
    })

    output$selectmapitems <- renderTree({
        item.list
    })

    output$selectcalitems <- renderTree({
        sua.item.list
    })

    observeEvent(input$showplot,{
        req(input$selectcountriesin)
        req(input$selecttrendyearsin)
        req(input$selectitems)
        selected <- get_selected(input$selectitems, format = "names")
        selected <-stringr::str_split_fixed(unlist(lapply(selected, `[[`, 1)),": ", n=2)[,1]
        fbsdata <- fbs |>
            filter(Element_Code!=5301)  |>
            filter(as.character(Item_Code) %in% selected) |>
            filter(Year>=input$selecttrendyearsin[1]&Year<=input$selecttrendyearsin[2]) |>
            filter(Area %in% input$selectcountriesin) |>
            as.data.table()
        fbsdata <- fbsdata[(Series=="New" & Year >=2010)|(Series=="Old" & Year <2010)]
        fbsdata <- merge(fbsdata,fbs.elements,by="Element_Code")
        fbsdata[Scale=="Quantity"]->fbsdata
        gsub(" ","",fbsdata$ElementNew)->fbsdata$ElementNew
        gsub("(","_",fbsdata$ElementNew,fixed=TRUE)->fbsdata$ElementNew
        gsub(")","",fbsdata$ElementNew,fixed=TRUE)->fbsdata$ElementNew
        fbsdata[Element_Code %in% c(5511,5611,5072),Side:="Supply"]
        fbsdata[Element_Code %in% c(5142, 5154, 5521, 5527, 5911, 5123, 5131,
                                    5171, 5170),Side:="Utilization"]
        fbsdata[Element_Code==5072,Value:=-Value]
        suadata <- sua |>
            filter(Element_Code!=5301)  |>
            filter(Item_Code %in% as.numeric(selected)) |>
            filter(Year>=input$selecttrendyearsin[1]&Year<=input$selecttrendyearsin[2]) |>
            filter(Area %in% input$selectcountriesin) |>
            as.data.table()
        suadata <- merge(suadata,sua.elements,by="Element_Code")
        suadata[Scale=="Quantity"]->suadata
        gsub(" ","",suadata$ElementNew)->suadata$ElementNew
        gsub("(","_",suadata$ElementNew,fixed=TRUE)->suadata$ElementNew
        gsub(")","",suadata$ElementNew,fixed=TRUE)->suadata$ElementNew
        suadata[Element_Code %in% c(5510,5610,5071),Side:="Supply"]
        suadata[Element_Code %in% c(5141, 5165, 5520, 5525, 5910, 5016, 5023,
                                    5164, 5166),Side:="Utilization"]
        suadata[Element_Code==5071,Value:=-Value]
        if(input$percapita==TRUE) {
            fbs |>
                filter(Area %in% input$selectcountriesin) |>
                filter(Item_Code==2501)  |>
                select(Year, Series, Pop=Value) |>
                as.data.table() -> pop
            pop <- pop[(Series=="New" & Year >=2010)|(Series=="Old" & Year <2010),
                       .(Year,Pop)]
            merge(fbsdata,pop,by="Year")->fbsdata
            fbsdata[,Value:=Value*1000/Pop]
            merge(suadata,pop,by="Year")->suadata
            suadata[,Value:=Value/Pop]
        }
        fbs1 <- rbind(fbsdata[Side=="Supply"], suadata[Side=="Supply"], fill=TRUE)
        fbs1[, Year := Year - 0.15]
        fbs1$Element <- factor(fbs1$Element,
                               levels=c("Import quantity",
                                        "Production", "Stock Variation"))
        fbs2 <- rbind(fbsdata[Side=="Utilization"], suadata[Side=="Utilization"], fill=TRUE)
        fbs2[, Year := Year + 0.15]
        fbs2[Element=="Food supply quantity (tonnes)", Element:="Food"]
        fbs2[Element=="Loss",Element:="Losses"]
        fbs2[Element=="Processed",Element:="Processing"]
        fbs2$Element <- factor(fbs2$Element,
                               levels=rev(c("Food", "Feed", "Seed",
                                            "Processing",
                                            "Other uses (non-food)",
                                            "Export quantity",
                                            "Tourist consumption",
                                            "Losses", "Residuals")))
        p <-list()
        for (i in 1:length(selected)) {
            p1 <- fbsplot(data1=fbs1, data2=fbs2, item=selected[i],
                          percap=input$percapita, flag=input$flags)
            p[[i]] <- p1
        }

        get_plot_output_list <- function() {
            plot_output_list <- lapply(1:length(p), function(i) {
                plotname <- paste("plot", i, sep="")
                ## plotname <- renderedplots[[i]]
                plot_output_object  <- girafeOutput(plotname, width = "100%", height = "700px")
                plot_output_object  <- renderGirafe({
                    girafe(ggobj = p[[i]],
                           options = list(
                               ## opts_sizing(rescale = TRUE,width=1),
                               ## opts_zoom = opts_zoom(min = 1, max = 4),
                               opts_selection(type = "single",
                                              css = "fill:yellow;stroke:gray;r:5pt;")
                           ),
                           width_svg=12,height_svg=6)
                })
            })
            do.call(tagList, plot_output_list)
            return(plot_output_list)
        }

        output$giraphplot <- renderUI({ get_plot_output_list() })

    })

    observeEvent(input$showcalplot,{
        selected <- get_selected(input$selectcalitems, format = "names")
        selected <-stringr::str_split_fixed(unlist(lapply(selected, `[[`, 1)),": ", n=2)[,1]
        suadata <- sua |>
            filter(Element_Code!=5301)  |>
            filter(as.character(Item_Code) %in% selected) |>
            filter(Year>=input$selecttrendyearsin[1] & Year<=input$selecttrendyearsin[2]) |>
            filter(Area %in% input$selectcalcountriesin) |>
            as.data.table()
        suadata <- merge(suadata, sua.elements, by="Element_Code")
        suadata[, Area_Code_M49 := as.numeric(gsub("'","",Area_Code_M49))]
        suadata <- suadata[!(ElementNew %in% c("Proteins/Year", "Fats/Year", "Calories/Year"))]
        suadata <- suadata[Scale=="Quantity"]
        glo <- fread("data/GLO.csv")
        names(glo)<-gsub(" ","_",names(glo))
        edibleportion<-glo[measuredElement==1061]
        glo<-glo[measuredElement!=1061]
        edi0 <- edibleportion[as.numeric(geographicAreaM49)!=0,
                              .(Area_Code_M49=as.numeric(geographicAreaM49),
                                Item_Code_CPC=measuredItemCPC, EdiblePortion=Value)]
        edi <- edibleportion[as.numeric(geographicAreaM49)==0,
                             .(Item_Code_CPC=measuredItemCPC, EdiblePortionG=Value)]
        g0 <- glo[as.numeric(geographicAreaM49)!=0,
                  .(Area_Code_M49=as.numeric(geographicAreaM49),
                    Item_Code_CPC=measuredItemCPC, Conversion=Value)]
        glo <- glo[as.numeric(geographicAreaM49)==0,
                   .(Item_Code_CPC=measuredItemCPC, Calories=Value)]
        suadata <- merge(suadata,edi0,by=c("Area_Code_M49","Item_Code_CPC"),
                         all.x=TRUE)
        suadata <- merge(suadata,edi,by=c("Item_Code_CPC"), all.x=TRUE)
        suadata[is.na(EdiblePortion), EdiblePortion:=EdiblePortionG]
        suadata[, EdiblePortionG:=NULL]
        suadata <- merge(suadata,g0,by=c("Area_Code_M49","Item_Code_CPC"),
                         all.x=TRUE)
        suadata <- merge(suadata,glo,by=c("Item_Code_CPC"), all.x=TRUE)
        suadata[is.na(Conversion), Conversion:=Calories]
        suadata[, Calories:=NULL]
        gsub(" ","",suadata$ElementNew)->suadata$ElementNew
        gsub("(","_",suadata$ElementNew,fixed=TRUE)->suadata$ElementNew
        gsub(")","",suadata$ElementNew,fixed=TRUE)->suadata$ElementNew
        suadata[Element_Code %in% c(5510,5610,5071),Side:="Supply"]
        suadata[Element_Code %in% c(5141, 5165, 5520, 5525, 5910, 5016, 5023,
                                    5164, 5166),Side:="Utilization"]
        ## suadata[Value<0&Element_Code==5071,Side:="Utilization"]
        ## suadata[Value<0&Element_Code==5071,Value:=-Value]
        if (input$selectcalgm=="Calories"){
            suadata[,Value:=Value*EdiblePortion*10000*Conversion]
            if (input$calpercapita==TRUE) {
                plotunit<-"Kcal per capita per day"
            } else {
                plotunit<-"Kilocalories"
            }
        } else if (input$calpercapita==TRUE) {
            suadata[,Value:=Value*1000000]
            plotunit<-"Grams per capita per day"
        } else {
            plotunit<-"Tonnes"
        }
        suadata[Element_Code==5071, Value := -Value]
        if (input$calprocessing == FALSE) {
            suadata[Element_Code==5023,Value:=-Value]
            suadata[Element_Code==5023,Element:="Production"]
            suadata[Element_Code==5023,Side:="Supply"]
        }
        if (input$calpercapita==TRUE) {
            fbs |>
                filter(Area %in% input$selectcalcountriesin) |>
                filter(Item_Code==2501)  |>
                select(Year, Series, Pop=Value) |>
                as.data.table() -> pop
            pop <- pop[(Series=="New" & Year >=2010)|(Series=="Old" & Year <2010),
                       .(Year,Pop)]
            merge(suadata,pop,by="Year")->suadata
            suadata[,Value:=Value/(Pop*1000*365)]
        }
        suadata[Element=="Tourist consumption", Element := "Residuals"]
        suadata1 <- suadata[, .(Value = sum(Value, na.rm=TRUE),
                               Item="All"),
                           .(Side,Element,Year)]
        suadata1[Value<0&Element=="Stock Variation",Side:="Utilization"]
        suadata1[Value<0&Element=="Stock Variation",Value:=-Value]
        fbs1 <- suadata1[Side=="Supply"]
        fbs1[, Year := Year - 0.22]
        fbs1$Element <- factor(fbs1$Element,
                               levels=c("Import quantity",
                                        "Production", "Stock Variation"))
        fbs2 <- suadata1[Side=="Utilization"]
        ## fbs2[, Year := Year - 0.075]
        fbs2[Element=="Food supply quantity (tonnes)", Element:="Food"]
        fbs2[Element=="Loss",Element:="Losses"]
        fbs2[Element=="Processed",Element:="Processing"]
        food <- suadata[Element=="Food supply quantity (tonnes)",
                        .(Value = sum(Value, na.rm=TRUE)),
                        .(Item,Year)][order(-Value)]
        ## food <- food[,.(percent = Value*100/sum(Value),
        ##                 Value,Item),
        ##              Year][order(-Value)]
        food[, calrank:=rank(-Value),Year]
        food[calrank > input$itemnos, Item := "Rest"]
        food[calrank > input$itemnos, calrank := input$itemnos+1]
        food <- food[,.(Value=sum(Value)),.(Year, Item, calrank)]
        food$Item <- factor(food$Item,
                            levels = unique(food[,.(Item=unique(Item)),
                                                 calrank]$Item))
        food[, Year := Year + 0.22]
        if (input$calprocessing == FALSE) {
            fbs2$Element <- factor(fbs2$Element,
                                   levels=rev(c("Food", "Feed", "Seed",
                                                "Other uses (non-food)",
                                                "Export quantity",
                                                "Losses", "Residuals",
                                                "Stock Variation")))
            utscale <- rev(c("#E41A1C", "#377EB8", "#4DAF4A",
                             "#984EA3", "#FF7F00", "#FFFF33",
                             "#A65628", "#440154"))
        } else {
            fbs2$Element <- factor(fbs2$Element,
                                   levels=rev(c("Food", "Feed", "Seed",
                                                "Processing",
                                                "Other uses (non-food)",
                                                "Export quantity",
                                                "Losses", "Residuals",
                                                "Stock Variation")))
            utscale <- rev(c("#E41A1C", "#377EB8", "#4DAF4A",
                             "#984EA3", "#FF7F00", "#FFFF33", "#A65628",
                             "#999999", "#440154"))
        }
        cpricesDT <- cprices |>
            ## filter(Element_Code!=5301)  |>
            filter(as.character(Item_Code) %in% 23013) |>
            filter(Year>=input$selecttrendyearsin[1] & Year<=input$selecttrendyearsin[2]) |>
            filter(Area %in% input$selectcalcountriesin) |>
            as.data.table()
        cpricesDT <- cpricesDT[,.(Value=mean(Value)),
                               .(Area, Year)]
        p <-list()
        ## for (i in 1:length(selected)) {
        p[[1]] <- calplot(data1=fbs1, data2=fbs2, data3 = food,
                      percap=FALSE, unit=plotunit,
                      fillscale=utscale)
        p[[2]] <- priceplot(pricedata=cpricesDT)
        ##       }
        get_plot_output_list <- function() {
            plot_output_list <- lapply(1:length(p), function(i) {
                plotname <- paste("plot", i, sep="")
                ## plotname <- renderedplots[[i]]
                plot_output_object  <- girafeOutput(plotname, width = "100%", height = "700px")
                plot_output_object  <- renderGirafe({
                    girafe(ggobj = p[[i]],
                           options = list(
                               ## opts_sizing(rescale = TRUE,width=1),
                               ## opts_zoom = opts_zoom(min = 1, max = 4),
                               opts_selection(type = "single",
                                              css = "fill:yellow;stroke:gray;r:5pt;")
                           ),
                           width_svg=12,height_svg=6)
                })
            })
            do.call(tagList, plot_output_list)
            return(plot_output_list)
        }


        output$giraphcalplot <- renderUI({ get_plot_output_list() })

    })


    observeEvent(input$showmap,{
        req(input$selectmapyearsin)
        req(input$selectmapitems)
        req(input$selectmapelementin)
        selected <- get_selected(input$selectmapitems, format = "names")
        selected <-stringr::str_split_fixed(unlist(lapply(selected, `[[`, 1)),": ", n=2)[,1]
        ## fbsdata <- fbs |>
        ##     filter(Element_Code!=5301)  |>
        ##     filter(as.character(Item_Code) %in% selected) |>
        ##     filter(Year>=input$selecttrendyearsin[1]&Year<=input$selecttrendyearsin[2]) |>
        ##     filter(Area %in% input$selectcountriesin) |>
        ##     as.data.table()
        ## fbsdata <- fbsdata[(Series=="New" & Year >=2010)|(Series=="Old" & Year <2010)]
        ## fbsdata <- merge(fbsdata,fbs.elements,by="Element_Code")
        ## fbsdata[Scale=="Quantity"]->fbsdata
        ## gsub(" ","",fbsdata$ElementNew)->fbsdata$ElementNew
        ## gsub("(","_",fbsdata$ElementNew,fixed=TRUE)->fbsdata$ElementNew
        ## gsub(")","",fbsdata$ElementNew,fixed=TRUE)->fbsdata$ElementNew
        ## fbsdata[Element_Code %in% c(5511,5611,5072),Side:="Supply"]
        ## fbsdata[Element_Code %in% c(5142, 5154, 5521, 5527, 5911, 5123, 5131,
        ##                             5171, 5170),Side:="Utilization"]
        ## fbsdata[Element_Code==5072,Value:=-Value]
        suadata <- sua |>
            filter(Element==input$selectmapelementin)  |>
            filter(Item_Code %in% selected) |>
            filter(Year>=input$selectmapyearsin[1]&Year<=input$selectmapyearsin[2]) |>
            as.data.table()
        suadata[, Country.Code:=countrycode(Area_Code, origin="fao",
                                            destination= "iso3c")]
        suadata <- suadata[,.(Value = mean(Value, na.rm=TRUE)),
                           .(Country.Code, Country.Name=Area, Item_Code, Item, Element)]
        suadata <- merge(world_data, suadata, by="Country.Code", all.x=TRUE)
        suadata <- suadata[order(suadata$sno),]
        g <- ggplot()+
            geom_polygon_interactive(data = suadata,
                                     color = 'gray70', size = 0.1,
                                     aes(x = long, y = lat, fill = Value, group = group,
                                         tooltip = sprintf("%s<br/>%s", Country.Name, round(Value,1)),
                                         data_id=Country.Code))+
            scale_fill_gradient("white",high = scales::muted("green"),
                                ## midpoint=homevalues$midvalue,
                                na.value = 'grey')+
            map_theme()+
            guides(fill = guide_colourbar(theme = theme(
                                              legend.key.width  = unit(20, "lines"),
                                              legend.key.height = unit(1, "lines")
                                          )))
        outvalues$plotlyplot<-g
        outvalues$plotcaption<-paste0(input$mapselectvariable1,", ",
                                      input$mapyear)

        output$mapcaption<-renderPrint({HTML(paste0("<h3>",outvalues$plotcaption,"</h3>"))})
        output$giraphmapplot<-renderGirafe(girafe(ggobj = outvalues$plotlyplot,
                                                  options = list(
                                                      opts_sizing(rescale = TRUE,width=1),
                                                      opts_zoom = opts_zoom(min = 1, max = 4),
                                                      opts_selection(type = "single",
                                                                     css = "fill:yellow;stroke:gray;r:5pt;",
                                                                     only_shiny = FALSE)
                                                  ),
                                                  width_svg=12,height_svg=8))

    })

}

### Run the app ----

shinyApp(ui = ui, server = server)
