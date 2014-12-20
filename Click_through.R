# USAGE
# source("Click_through.R")

require(data.table)

# http://stackoverflow.com/questions/1727772/quickly-reading-very-large-tables-as-dataframes-in-r/15058684#15058684

# validate ==> 
# http://stat.ethz.ch/R-manual/R-patched/library/stats/html/aggregate.html
# http://stats.stackexchange.com/questions/8225/how-to-summarize-data-by-group-in-r


# Convert
# Round time => convert to xxxx0000
# convert to time => http://stackoverflow.com/questions/13456241/convert-unix-epoch-to-date-object-in-r


read_data <- function(file_name, limit=0) {
    # read data by type
    file_path = paste("Raw", file_name, sep = "/")
    
    if (limit) {
        data = fread(file_path, nrows=limit)
    } else {
        data = fread(file_path)
    }
    
    data
}

