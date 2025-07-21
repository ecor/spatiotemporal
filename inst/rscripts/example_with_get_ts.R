library(spatiotemporal)
library(ggplot2)

data(db)
p <- get_p(x=db)
px <- p[49,] |> st_buffer(dist=250*1000) 
out <- get_ts(x=db,get_p_args=list(p=px),get_variable_args=list(name=c("MIN","MAX","PRCP")))

## Precipitation in one site

px <- p[49,] |> st_buffer(dist=1)  
prcp <- get_ts(x=db,get_p_args=list(p=px),
               get_variable_args=list(name=c("PRCP")),
               regular_timestamptz=TRUE)




## Aggregation through summarize ## chech quality after 1981


ddata <- prcp |> filter(timestamptz>as.POSIXct("2020-12-31")) |> 
  group_by(attrvalue) |> summarize(nvalues=length(value)) |> ungroup()

# PRCP_ATTRIBUTES -
#   
#   A = 1 report of 6-hour precipitation amount;
# 

# 
# C = Summation of 3 reports of 6-hour precipitation amount;
# 
# D = Summation of 4 reports of 6-hour precipitation amount;
# 
# E = 1 report of 12-hour precipitation amount;
# 
# F = Summation of 2 reports of 12-hour precipitation amount;
# 
# G = 1 report of 24-hour precipitation amount;
# 
# H = Station reported ‘0’ as the amount for the day (e.g. from 6-hour reports), but also reported at least one occurrence of precipitation in hourly observations–this could indicate a trace occurred, but should be considered as incomplete data for the day;
# 
# I = Station did not report any precipitation data for the day and did not report any occurrences of precipitation in its hourly observations–it’s still possible that precipitation occurred but was not reported;
# 
# 
# 
ccols <- list(
A="#a50026",#   A = 1 report of 6-hour precipitation amount;
B="#d73027",# B = Summation of 2 reports of 6-hour precipitation amount;
E="#f46d43",# H = Station reported ‘0’ as the amount for the day (e.g. from 6 hour reports), but also reported at least one occurrence of precipitation in hourly observations–this could indicate a trace occurred, but should be considered as incomplete data for the day;
H="#fdae61",
I="#fee08b",# I = Station did not report any precipitation data for the day and did not report any occurrences of precipitation in its hourly observations–it’s still possible that precipitation occurred but was not reported;
##"#ffffbf",
C="#d9ef8b",# C = Summation of 3 reports of 6-hour precipitation amount;
##"#a6d96a"
G="#66bd63",# G = 1 report of 24-hour precipitation amount;
F="#1a9850",# F = Summation of 2 reports of 12-hour precipitation amount;
D="#006837") |> unlist()# D = Summation of 4 reports of 6-hour precipitation amount;



title <- sprintf("Precipitation Atrributes since %s",px$name2[1])

gg <- ggplot(ddata, aes(x = "", y = nvalues, fill = attrvalue)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = ccols) +  # Aggiungi i colori personalizzati
  theme_void() +
  labs(title = title)

gg


#' ## 
#' ## PRCP_ATTRIBUTES 
#' ## 
#' ##   A = 1 report of 6-hour precipitation amount;
#' ## 
#' ## B = Summation of 2 reports of 6-hour precipitation amount;
#' ## 
#' ## C = Summation of 3 reports of 6-hour precipitation amount;
#' ## 
#' ## D = Summation of 4 reports of 6-hour precipitation amount;
#' ## 
#' ## E = 1 report of 12-hour precipitation amount;
#' ## 
#' ## F = Summation of 2 reports of 12-hour precipitation amount;
#' ## 
#' ## G = 1 report of 24-hour precipitation amount;
#' ## 
#' ## H = Station reported 0 as the amount for the day (e.g. from 6-hour reports), but also reported at least one occurrence of precipitation in hourly observations–this could indicate a trace occurred, but should be considered as incomplete data for the day;
#' ## 
#' ## I = Station did not report any precipitation data for the day and did not report any occurrences of precipitation in its hourly observations–it’s still possible that precipitation occurred but was not reported;
#' ## 
#' ## 