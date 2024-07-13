##############################################################################
# USE OF DLNM IN CASE-CROSSOVER ANALYSIS
# (c) Antonio Gasparrini 2011-2017
##############################################################################

# CREATE A FUNCTION FUNCCMAKE TO CREATE THE DATASET FROM THE ORIGINAL SERIES
# EACH OBS IS LINKED WITH CONTROLS FOLLOWING A TIME-STRATIFIED SAMPLING
# THE FUNCTION CREATE A DATA FRAME WITH THE OBJECTS:
# index: INDEX TO DUPLICATE THE ORIGINAL DATASET
#	status: CASE/CONTROL STATUS IN EACH STRATUM
#	stratum: STRATUM FOR MATCHED CASE+CONTROLS
#	weights: WEIGHTS CORRESPONDING TO THE ORIGINAL EVENTS IN THE CASE DAY
# 

# THE FUNCTION HAS ARGUMENT:
#	date: EITHER A DATE OBJECT OR AN INDICATOR FOR DIFFERENT MONTHS
#	cases: THE SERIES OF CASE COUNTS
# vars: A MATRIX OR DATA FRAME WITH PREDICTORS
# dow: LOGICAL: REFERENT DAYS AT SAME DAY OF THE WEEK. DEFAULT TO TRUE

funccmake <-  function(date,cases,vars=NULL,dow) {
  #  
  # DERIVE STRATUM VARIABLES
  if(missing(dow)) dow <- ifelse(class(date)=="Date",TRUE,FALSE)
  if(class(date)=="Date") {
    day <- if(dow) weekdays(date) else rep(1,length(date))
    month <- months(date)
    year <- format(date, format="%Y")    
  } else {
    day <- rep(1,length(date))
    month <- date
    year <- rep(1,length(date))
    if(dow) stop("'dow' only available when 'date' is a date")
  }
  #
  # DERIVE INDEXING VARIABLES
  gfactor <- factor(day):factor(month):factor(year)
  gnumber <- match(gfactor,unique(gfactor))
  gindex <- lapply(1:length(date), 
                   function(x) (1:length(date))[gnumber%in%gnumber[x]])
  gstatus <- lapply(1:length(date), function(x) gindex[[x]]==x)
  #  
  # EXPAND PREDICTORS
  if(!is.null(vars)) {
    varnames <- if(is.vector(vars)) deparse(substitute(vars)) else colnames(vars)
    vars <- as.matrix(vars)
    dimnames(vars) <- list(NULL,varnames)
  }
  #	
  # RESULTS
  res <- data.frame(
    index=unlist(gindex),
    status=unlist(gstatus)+0,
    stratum=rep(1:length(date),sapply(gindex,length)),
    weights=as.numeric(rep(cases,sapply(gindex,length)))
  )
  if(!is.null(vars)) res <- cbind(res,vars[res$index,])
  #
  return(res)
}


                   