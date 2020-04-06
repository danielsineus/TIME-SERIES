# TIME Series Forecasting
In this tutorial, the goal is to provide the steps and foundation for a good forecasting. Many steps will be considered. Analyses from various levels will be presented. The presentation will be succint and only the major points will be outlined. some of the steps and comments are also outlined in the R format.


## Convert the Data in time Series (ts)
* Convert in ts object
* convert in xts object
* Manipulate the periodicity of the XTS object
The periodicity is important after transforming the data in ts data, periodicity which is a function from xts library can be used to know the beginning of the data time and the end of the data time.
Window function can help subset the data from one period to a another period by determining the frequency. If the data is too large, to forecast it is sometime important to reduce the length of the data to more practical length of time.

## Difficulties and challenges

Diffculties encountered will be discussed in order to help find a way to go around them. Most of the difficulties come from tidying the data and make it conducive to conducting the forecasting. 

#### Handling dates 

the date was in a character format. It has be to converted into a date format.

### Changing the periodicity

Honestly i was not able to use the cumsum() when i had to use the endpoint. Cumsum() doesn't really work with inserting it in the function lapply(), mean ()

***Split temps by week***

temps_weekly <- split(__, f = "___")

***Create a list of weekly means, temps_avg, and print this list***

temps_avg <- lapply(X = ___, FUN = ___)

***The method used instead***

x_split <- split(x, f = "months")

x_list <- lapply(x_split, cummax)

x_list_rbind <- do.call(rbind, x_list)
