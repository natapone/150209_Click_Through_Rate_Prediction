# USAGE
# source("Click_through_plot.R")
# plot_category_relation(data_source="web")
# plot_device_relation("web")

source("Click_through.R")

# banner_pos -> category
# banner_pos -> 
# banner_pos -> 
# banner_pos -> 
# banner_pos -> 
plot_banner_misc <- function(data_source="web") {
    observe_main = "banner_pos"
    
    observe_cols = c(
        "device_model"
#         "traffic",
#         "device_type",
#         "device_conn_type",
#         "C1",
#         "C15",
#         "C16",
#         "C17",
#         "C14",
#         "C18",
#         "C19",
#         "C20",
#         "C21"
    )
    
    plot_category_relation(data_source, observe_main, observe_cols)
    return(1)
}

plot_category_misc <- function(data_source="web") {
    
    observe_cols = c(
#         "banner_pos",
        "traffic"
    )
    
    plot_category_relation(data_source, observe_main=NULL, observe_cols)
    return(1)
}

# device_model -> device_conn_type
# device_model -> device_type
plot_device_relation <- function(data_source="web") {
    observe_main = "device_conn_type"
    
    observe_cols = c(
        "device_conn_type",
        "device_model",
        "device_type"
    )
    
    plot_category_relation(data_source, observe_main, observe_cols)
    return(1)
}

# plot device -> category relationship
plot_device_cat_relation <- function(data_source="web") {
    observe_main = "device_model"
    
    observe_cols = c(
        "C1",
        "C15",
        "C16",
        "C17",
        "C14",
        "C18",
        "C19",
        "C20",
        "C21"
    )
    
    plot_category_relation(data_source, observe_main, observe_cols)
    return(1)
}

# Expect some dots which click > 0, that means the feature has click > no-click
plot_category_relation <-function(data_source="web", observe_main=NULL, observe_cols=NULL, mode="doc") {
    
    if (data_source == "web") {
        main_cat = "site_category"
    } else if (data_source == "app") {
        main_cat = "app_category"
    } else if(is.null(data_source)) {
        print("Please define data_source")
        return()
    }
    
    if(!is.null(observe_main)) {
        main_cat = observe_main
    }
    
    if(is.null(observe_cols)) {
        observe_cols = c(
            "C1",
            "C15",
            "C16",
            "C17",
            "C14",
            "C18",
            "C19",
            "C20",
            "C21"
        )
    }
    
    
    for(ob_col in observe_cols) {
        print( paste(ob_col, "-", main_cat)  )
        
        col_list = c(
            main_cat,
            ob_col,
            "click"
        )
        
        data = read_clean_data(col_list, data_source=data_source)
#         data = head(data, 1000)
#         return(data)
        # data is too large
        inTrain = createDataPartition(y=data$click, p=0.1, list=FALSE)
        data = data[inTrain[,1],]
        
        matrix_formular = paste(main_cat, ":", ob_col, sep="")
        matrix_formular = paste("click", "~", matrix_formular)
        matrix_formular = as.formula(matrix_formular)
        
        agg_plot = cal_aggregate_category(data, matrix_formular)
        remove(data)
        
        #plot
        plot_title = paste(data_source,main_cat,ob_col,sep=" - ")
#         return(agg_plot)
        p = ggplot(data=agg_plot, aes(x=click, y=names, colour=population, size=population)) + 
            geom_point( alpha=0.7) +
            scale_colour_gradientn( colours=rainbow(7)) +
            geom_vline(yintercept=0, colour="darkgreen", linetype = "longdash") +
            theme(
                panel.background=element_rect(fill="grey97")
            ) + ggtitle(plot_title)
        
        if (mode == 'doc') {
            print(p)
        } else {
            # save plot data
            file_name = paste(data_source, main_cat, ob_col, sep="_")
            file_name = paste("plot/cat", file_name, sep="/")
            
            saveRDS(agg_plot, paste(file_name, "RData", sep="."))
            
            file_name = paste(file_name, "png", sep=".")
            
            cat("Save plot ")
            print(file_name)
            
            # g_height = round((nrow(agg_plot) / 20) + 0.5 )
            ggsave(file=file_name, width=10, height=6)
        }
        
        
        
#         return(agg_plot)
    }
    return (1)
    #######
    
    
}

cal_aggregate_category <- function(d, matrix_formular) {
    m = model.matrix(matrix_formular, d)
    
    # drop (intercept)
    m = m[,-1]
    m = as.data.frame(m)
    
    # aggregate list
    agg_list = names(m)
    
    # merge click
    click = as.numeric(d$click)
    m = cbind(m, click)
    
    print("Cal aggregate")
    agg = aggregate(. ~ click, data = m, sum)
    agg = agg[,-1]
    agg_plot = apply(agg,2,FUN = function(x) {
            # click(0,1)
            
            # return Percentage Difference
            if((x[2] + x[1]) == 0) {
                p = 0
            } else {
                p = (x[2] - x[1]) / (x[2] + x[1])
            }
            
            pop = (x[2] + x[1]) +1
            pop = round(log(pop))
            # percentage and population
            return(c(p, pop))
        }
    )
    
    # transpose
    print("Prepare for plotting")
    agg_plot = t(agg_plot)
    agg_plot = as.data.frame(agg_plot)
    names(agg_plot) = c("click","population")
    
    # add name
    agg_plot$names <- rownames(agg_plot)
    
    # sort
    agg_plot = agg_plot[order(rownames(agg_plot)), ] 
    agg_plot
}
