#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(pcaprojection)

# test if there is at least two arguments: if not, return an error
if (length(args)==0) {
  stop("Both plot type and a json file) are needed", call.=FALSE)
} else if (length(args)>=2) {
  plottype <- args[1]
  parameters <- args[2]
  switch( plottype,
          "histogram" = if (args[2] == "help")
                          ?histogram
                        else
                          histogram(parameters),
          "violin" = if (args[2] == "help")
                      ?cm_ggviolin
                     else
                        cm_ggviolin(parameters),
          "pca" = if (args[2] == "help")
                    ?pcaproj
                  else
                    pcaproj(parameters),
          print(paste0("'",plottype,"' plot is not found in this package"))
          )
}

# docker container run --rm  -v "$PWD":/app/data pcap parameters.json
# Rscript cm_plots.R histogram ../data/hist_parameters.json
# Rscript cm_plots.R histogram help
