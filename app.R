source("../../global.R")
library(ggnewscale)
open_dataset("data/fbs") ->fbs
open_dataset("data/sua") ->sua
library(shinyTree)


suaitems <- readRDS("data/suaitems.rds")
item.list <- readRDS("data/suaitemlist.rds")

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
    ggplot()+
        geom_bar(data = data1 |> filter(Item_Code==item), stat="identity",
                 aes(x=Year,y=Value,group=Element,fill=Element),
                 width=0.3, size=0.1)+
        scale_fill_viridis_d(name="Supply", direction=-1)+
        new_scale_fill()+
        geom_bar(data = data2 |> filter(Item_Code==item), stat="identity",
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
        p <- p+geom_text(data=data1 |> filter(Item_Code==item),
                         aes(x=Year, y=Value, label=Flag,
                             group=Element), angle=90,
                         position = position_stack(vjust = 0.5),
                         size=3,vjust="middle",lineheight=0.3
                         )+
            geom_text(data=data2 |> filter(Item_Code==item),
                      aes(x=Year, y=Value, label=Flag,
                          group=Element), angle=90,
                      position = position_stack(vjust = 0.5),
                      size=3,vjust="middle",lineheight=0.3
                      )
    }
    return(p)
}


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
            filter(Item_Code %in% selected) |>
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
        browser()
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
