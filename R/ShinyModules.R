#' UI to produce a plotly output (bar chart) of height 200px
#' @param id an ID wrapped in double quotes that matches the id of the corresponding server function
#' @return UI with a plotly output
#' @importFrom shiny NS
#' @importFrom plotly plotlyOutput
#' @importFrom shiny tagList
#' @importFrom shinycssloaders withSpinner
#' @example
#'    \donttest{
#'     mod_mainchart("homepage1stchart")
#'       }
#' @export
#' @keywords Plotly, Bar Chart, Shiny

mod_mainchart<- function(id){
  ns<-NS(id)
  tagList(
    withSpinner(plotlyOutput(ns("barchart"), height = "200px"))
  )
}

#' Server to produce a plotly output (bar chart) of height 200px
#' @param id an ID wrapped in double quotes that matches the id of the corresponding UI function
#' @param df a data frame that has 'name' as a columns of factors and 'value' as a column of numbers corresponding to value
#' @param title defaults to empty. Can be used to add title to the poltly chart.
#' @return UI and x axis value of click on the plotly chart
#' @importFrom shiny moduleServer reactive
#' @importFrom  plotly plot_ly layout event_data renderPlotly config
#' @example
#'    \donttest{
#'     mod_mainchartServer("homepage1stchart", data.frame(name=c("category1","category2","category3","category4"), value=c(4,2,3,5)))
#'       }
#' @export
#' @keywords Plotly, Bar Chart, Shiny, Plotly event

mod_mainchartServer<- function(id, df, title=""){
  moduleServer(
    id,
    function(input,output,session){
      output$barchart<- renderPlotly({
        df%>%
          plot_ly(
            x =  ~ reorder(name,-value),
            y =  ~ value,
            type = "bar",
            opacity = 0.7,
            source = id
          ) %>%
          layout(
            #title= title,
            xaxis = list(title = title, color = "white"),
            yaxis = list(title = "", color = "white"),
            plot_bgcolor = 'transparent',
            paper_bgcolor = 'transparent'#,
            #dragmode = "select"
          ) %>% config(displayModeBar = FALSE)
      })

      barclick<- reactive({
        d<- event_data("plotly_click", source = id)
        return(d$x)
        # names<- reorder(df$name,-df$value)
        # print(clickdata$x)
        # names[clickdata$x]
      })
      return(barclick())
    }



  )
}

#' UI to produce a plotly output (line chart with fill i.e. area) of height 200px
#' @param id an ID wrapped in double quotes that matches the id of the corresponding server function
#' @return UI with a plotly output
#' @importFrom shiny NS
#' @importFrom plotly plotlyOutput
#' @importFrom shiny tagList
#' @importFrom shinycssloaders withSpinner
#' @example
#'    \donttest{
#'     mod_linechart("homepage1stchart")
#'       }
#' @export
#' @keywords Plotly, Line Chart, Area Chart, Shiny

mod_linechart<- function(id){
  ns<-NS(id)
  tagList(
    withSpinner(plotlyOutput(ns("linechart"), height = "200px"))
  )
}

#' Server to produce a plotly output (line chart with fill i.e. area) of height 200px
#' @param id an ID wrapped in double quotes that matches the id of the corresponding UI function
#' @param df a data frame that has 'datetime' as a columns of date/datetime and 'value' as a column of numbers corresponding to value
#' @param yaxistitle defaults to empty. Can be used to add title to y axis of the poltly chart.
#' @param fill defaults to "tozeroy". Defines the colour of the filled area
#' @return UI and x axis values (min and max) of selection on plot
#' @importFrom shiny moduleServer reactive
#' @importFrom  plotly plot_ly layout event_data renderPlotly config
#' @example
#'    \donttest{
#'     mod_mainchartServer("homepage1stchart", df=dataframe)
#'       }
#' @export
#' @keywords Plotly, Line Chart, Area Chart, Shiny, Plotly event

mod_linechartServer<- function(id, df, yaxistitle="", fill='tozeroy'){
  moduleServer(
    id,
    function(input,output,session){
      output$linechart<- renderPlotly({
        df%>%
          plot_ly(
            x=~datetime,
            y=~value,
            color = ~name,
            type = 'scatter',
            mode = 'lines',
            line= list(width = 5),
            fill = 'tozeroy',
            source = id
          )%>%
          layout(xaxis = list(title = "Months", color = "white"),
                 yaxis = list(title = yaxistitle, color = "white"),
                 plot_bgcolor = 'transparent',
                 paper_bgcolor = 'transparent',
                 legend = list(orientation = 'h'),
                 dragmode = "select"
          )%>% config(displayModeBar = FALSE)
      })

      selectedrange<- reactive({
        d<-event_data("plotly_brushed", source = id)
        return(d$x)
      })

      return(selectedrange())

    }
  )
}

#' UI to produce a plotly output (gauge chart) of height 150px
#' @param id an ID wrapped in double quotes that matches the id of the corresponding server function
#' @return UI with a plotly output
#' @importFrom shiny NS
#' @importFrom plotly plotlyOutput
#' @importFrom shiny tagList
#' @importFrom shinycssloaders withSpinner
#' @example
#'    \donttest{
#'     mod_gauge("homepage1stchart")
#'       }
#' @export
#' @keywords Plotly, Gauge Chart, Shiny

mod_gauge<- function(id){
  ns<-NS(id)
  tagList(
    withSpinner(plotlyOutput(ns("gaugechart"), height = "150px"))
  )
}

#' Server to produce a plotly output (line chart with fill i.e. area) of height 200px
#' @param id an ID wrapped in double quotes that matches the id of the corresponding UI function
#' @param value an integer value to show the current value
#' @param title Text to add title to the graph
#' @param lastvalue Integer value that shows last value of the indicated value. This is used to calculate the delta vis-a-vis current value
#' @param maxval Integer to indicate maximum limit of the gauge
#' @param rangeval1 Integer used to define the first segment of the gauge i.e. from 0 to this value is section 1
#' @param rangeval2 Integer used to define the second segment. i.e. from rangeval1 to rangeval2 is second segment
#' @param threshold Integer to mark a threshold point in the gauge
#' @return UI and with a gauge chart
#' @importFrom shiny moduleServer reactive
#' @importFrom  plotly plot_ly layout event_data renderPlotly config
#' @example
#'    \donttest{
#'     mod_gaugeServer("homepage1stchart",  value=350, title="", lastvalue=300, maxval=500, rangeval1=250, rangeval2=400, threshold= 490)
#'       }
#' @export
#' @keywords Plotly, Gauge chart, Shiny

mod_gaugeServer<- function(id, value, title="", lastvalue, maxval=500, rangeval1=250, rangeval2=400, threshold= 490) {
  moduleServer(
    id,
    function(input,output,session){
      output$gaugechart<- renderPlotly({
        plot_ly(
          domain = list(x = c(0, 1), y = c(0, 1)),
          value = value,
          title = "",#list(text = title),
          type = "indicator",
          mode = "gauge+number+delta",
          delta = list(reference = lastvalue),
          gauge = list(
            axis =list(range = list(NULL, maxval)),
            steps = list(
              list(range = c(0, rangeval1), color = "lightgray"),
              list(range = c(rangeval1, rangeval2), color = "gray")),
            threshold = list(
              line = list(color = "red", width = 4),
              thickness = 0.75,
              value = threshold)))%>%
          layout(
            #title= title,
            #margin = list(l=10,r=5),
            xaxis = list(title = title, color = "white"),
            yaxis = list(title = "", color = "white"),
            plot_bgcolor = 'transparent',
            paper_bgcolor = 'transparent',
            font = list(color = "white")
            #dragmode = "select"
          ) %>% config(displayModeBar = FALSE)
      })
    }

  )
}

#' UI to produce a plotly output (horizontal bar chart) of height 200px
#' @param id an ID wrapped in double quotes that matches the id of the corresponding server function
#' @return UI with a plotly output
#' @importFrom shiny NS
#' @importFrom plotly plotlyOutput
#' @importFrom shiny tagList
#' @importFrom shinycssloaders withSpinner
#' @example
#'    \donttest{
#'     mod_horizontalmainchart("homepage1stchart")
#'       }
#' @export
#' @keywords Plotly, Horizontal bar chart, Shiny

mod_horizontalmainchart<- function(id){
  ns<-NS(id)
  tagList(
    withSpinner(plotlyOutput(ns("horizontalchart"), height = "200px"))
  )
}

#' Server to produce a plotly output (line chart with fill i.e. area) of height 200px
#' @param id an ID wrapped in double quotes that matches the id of the corresponding UI function
#' @param df a data frame with column 'name' (that contains factors) and 'value' (that contains numbers).
#' @param title Text to add title to the graph
#' @return UI and yaxis value of the click on the plot
#' @importFrom shiny moduleServer reactive
#' @importFrom  plotly plot_ly layout event_data renderPlotly config
#' @example
#'    \donttest{
#'     mod_horizontalmainchartServer("homepage1stchart", df=data.frame(name=c("category1","category2","category3","category4"), value=c(4,2,3,5)))
#'       }
#' @export
#' @keywords Plotly, Horizontal bar chart, Shiny

mod_horizontalmainchartServer<- function(id, df, title=""){
  moduleServer(
    id,
    function(input,output,session){
      output$horizontalchart<- renderPlotly({
        df%>%
          plot_ly(
            x =  ~ value,
            y =  ~ reorder(name,-value),
            type = "bar",
            opacity = 0.7,
            source = id
          ) %>%
          layout(
            #title= title,
            xaxis = list(title = title, color = "white"),
            yaxis = list(title = "", color = "white"),
            plot_bgcolor = 'transparent',
            paper_bgcolor = 'transparent'#,
            #dragmode = "select"
          ) %>% config(displayModeBar = FALSE)
      })

      barclick<- reactive({
        d<- event_data("plotly_click", source = id)
        return(d$y)
      })
      return(barclick())
    }
  )
}

#' UI to produce a plotly output (horizontal bar chart + scatter plot) of height 200px
#' @param id an ID wrapped in double quotes that matches the id of the corresponding server function
#' @return UI with a plotly output
#' @importFrom shiny NS
#' @importFrom plotly plotlyOutput
#' @importFrom shiny tagList
#' @importFrom shinycssloaders withSpinner
#' @example
#'    \donttest{
#'     mod_barscatterchart("homepage1stchart")
#'       }
#' @export
#' @keywords Plotly, Horizontal bar and scatter chart, Shiny, Multiple trace

mod_barscatterchart<- function(id){
  ns<-NS(id)
  tagList(
    withSpinner(plotlyOutput(ns("barscatplot"), height = "200px"))
  )
}


#' Server to produce a plotly output (line chart with fill i.e. area) of height 200px
#' @param id an ID wrapped in double quotes that matches the id of the corresponding UI function
#' @param df a data frame with column 'name' (that contains factors), 'value1' and 'value2' columns that contain numbers .
#' @param title Text to add title to the graph
#' @param name1 Text to add name to trace 1 (bar)
#' @param name2 Text to add name to trace 2 (Scatter)
#' @return UI and yaxis value of the click on the plot
#' @importFrom shiny moduleServer reactive
#' @importFrom  plotly plot_ly layout event_data renderPlotly config
#' @example
#'    \donttest{
#'     mod_barscatterchartServer("homepage1stchart", df= data.frame(name=c("category1","category2","category3","category4"), value1=c(4,2,3,5), value2=c(3,6,4,2)))
#'       }
#' @export
#' @keywords Plotly, Horizontal bar and scatter chart, Shiny, Multiple trace

mod_barscatterchartServer<- function(id, df, title="", name1="", name2=""){
  moduleServer(
    id,
    function(input,output,session){
      output$barscatplot<- renderPlotly({
        df%>%
          plot_ly(
            x =  ~ value1,
            y =  ~ reorder(name,-value1),
            type = "bar",
            opacity = 0.7,
            name = name1
          ) %>%
          add_trace(
            x= ~ value2,
            type="scatter",
            size=4,
            name=name2
          )%>%
          layout(
            #title= title,
            legend = list(orientation = 'h'),
            xaxis = list(title = title, color = "white"),
            yaxis = list(title = "", color = "white"),
            plot_bgcolor = 'transparent',
            paper_bgcolor = 'transparent'
          ) %>% config(displayModeBar = FALSE)
      })
    }
  )
}

#' UI to produce a plotly output (line chart without fill) of height 200px
#' @param id an ID wrapped in double quotes that matches the id of the corresponding server function
#' @return UI with a plotly output
#' @importFrom shiny NS
#' @importFrom plotly plotlyOutput
#' @importFrom shiny tagList
#' @importFrom shinycssloaders withSpinner
#' @example
#'    \donttest{
#'     mod_linechartnofill("homepage1stchart")
#'       }
#' @export
#' @keywords Plotly, Line chart, Shiny,

mod_linechartnofill<- function(id){
  ns<-NS(id)
  tagList(
    withSpinner(plotlyOutput(ns("linechart"), height = "200px"))
  )
}


#' Server to produce a plotly output (line chart without fill) of height 200px
#' @param id an ID wrapped in double quotes that matches the id of the corresponding UI function
#' @param df a data frame that has 'datetime' as a columns of date/datetime and 'value' as a column of numbers corresponding to value
#' @param yaxistitle defaults to empty. Can be used to add title to y axis of the poltly chart
#' @return UI and x axis values (min and max) of selection on plot
#' @importFrom shiny moduleServer reactive
#' @importFrom  plotly plot_ly layout event_data renderPlotly config
#' @example
#'    \donttest{
#'     mod_linechartnofillServer("homepage1stchart", df= dataframe)
#'       }
#' @export
#' @keywords Plotly,line chart, Shiny

mod_linechartnofillServer<- function(id, df, yaxistitle=""){
  moduleServer(
    id,
    function(input,output,session){
      output$linechart<- renderPlotly({
        df%>%
          plot_ly(
            x=~datetime,
            y=~value,
            color = ~name,
            type = 'scatter',
            mode = 'lines',
            source = id
          )%>%
          layout(xaxis = list(title = "Datetime", color = "white"),
                 yaxis = list(title = yaxistitle, color = "white"),
                 plot_bgcolor = 'transparent',
                 paper_bgcolor = 'transparent',
                 legend = list(orientation = 'h'),
                 dragmode = "select"
          )%>% config(displayModeBar = FALSE)
      })

      selectedrange<- reactive({
        d<-event_data("plotly_brushed", source = id)
        return(d$x)
      })

      return(selectedrange())

    }
  )
}

#' UI to produce infobox
#' @param id an ID wrapped in double quotes that matches the id of the corresponding server function
#' @return UI with a infobox output
#' @importFrom shiny NS
#' @importFrom shinydashboard infoBoxOutput
#' @importFrom shiny tagList
#' @importFrom shinycssloaders withSpinner
#' @example
#'    \donttest{
#'     mod_info("homepage1stinfobox")
#'       }
#' @export
#' @keywords infobox, Shiny



mod_info <- function(id){
  ns<-NS(id)
  tagList(
    withSpinner(infoBoxOutput(ns("infobox"), width=3))
  )
}

#' Server to produce infobox
#' @param id an ID wrapped in double quotes that matches the id of the corresponding UI function
#' @param value as the value to show in infobox
#' @param title as title to show in infobox
#' @param colour as colour of the infobox
#' @param icon name as font awesome icon name to be shown in the infobox
#' @param subtitle as text to show as subtitle in the infobox
#' @param yaxistitle defaults to empty. Can be used to add title to y axis of the poltly chart
#' @return UI and x axis values (min and max) of selection on plot
#' @importFrom shiny moduleServer
#' @importFrom  shinydashboard infobox, renderInfoBox
#' #' @return UI and yaxis value of the click on the plot
#' @example
#'    \donttest{
#'     mod_infoServer("homepage1stinfobox",value =10, title="Title", colour="red", icon="money", subtitle="subtitle")
#'       }
#' @export
#' @keywords infobox, Shiny

mod_infoServer<- function(id, value, title, colour, icon, subtitle=""){
  moduleServer(
    id,
    function(input,output,session){
      output$infobox<- renderInfoBox({
        infoBox(
          title = title,
          value = value,
          icon = icon(icon),
          color = colour,
          subtitle = subtitle
        )
      })
    }
  )
}
