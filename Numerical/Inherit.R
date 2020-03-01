#Example of canti-lever beam
library(R6)
Strength <- R6Class("Strength",
                    public = list(
                      initialize = function(section,mdata){
                          sm <- SectionCalc$new(section)
                          private$Z <- sm$zCalc()
                          private$I <- sm$ICalc()
                          private$mdata <- mdata
                        },
                      failure = function(){print("Set failure()")},
                      eval = function(){print("Set eval()")},
                      GetZ=function()return(private$Z),
                      GetI=function()return(private$I),
                      GetMean=function()return(private$mdata)
                    ),
                    private = list(
                      pf=0.0,
                      Z=0.0,
                      I=0.0,
                      mdata=data.frame(x=0)
                    )
                    )
Canti_stress <- R6Class("Canti_stress",
                      inherit=Strength,
                      public=list(
                        failure = function(){
                          indata <- super$GetMean()
                          Sa <- indata$Sa
                          if(self$eval()>Sa){ans="yes"}else{ans="no"}
                          ans
                          },
                        eval = function(){
                          indata <- super$GetMean()
                          P <-indata$P
                          l <- indata$l
                          z <- super$GetZ()
                          P*l/z
                        }
                      )
                      )
Canti_disp <- R6Class("Canti_disp",
                        inherit=Strength,
                        public=list(
                          failure = function(){
                            indata <- super$GetMean()
                            dlim <- indata$dlim
                            if(self$eval()>dlim){ans="yes"}else{ans="no"}
                            ans
                            },
                          eval = function(){
                            indata <- super$GetMean()
                            P <-indata$P
                            l <- indata$l
                            E <- indata$E
                            I <- super$GetI()
                            v <- P*l^3/(3*E*I)
                            v
                          }
                        )
)
SectionCalc <- R6Class("SectionCalc",
                       public = list(
                         initialize = function(section){
                           private$sec_data <- section
                         },
                         ICalc = function(){
                           if(private$sec_data$geom == "quad"){
                             b <- private$sec_data$b
                             h <- private$sec_data$h
                             return(h^3*b/12)
                           }
                           if(private$sec_data$geom=="circle"){
                             r <- private$sec_data$r
                             return(pi * r^4/2)
                           }
                           
                         },
                         zCalc = function(){
                           if(private$sec_data$geom == "quad"){
                             b <- private$sec_data$b
                             h <- private$sec_data$h
                             return(h^3*b/12/(h/2))
                           }
                           if(private$sec_data$geom=="circle"){
                             r <- private$sec_data$r
                             return(pi * r^4/2/r)
                           }
                         }
                       ),
                       private = list(
                         sec_data = data.frame(x=0)
                       )
                       )
quad <- data.frame(geom="quad",h=80,b=40)
circle <- data.frame(geom="circle",r=32)
indata <- data.frame(P=4000,l=1000,E=205000,Sa=100,dlim=10)
dd <- Canti_stress$new(quad,indata)
dd$eval()
print(dd$failure())
dd <- Canti_stress$new(circle,indata)
dd$eval()
print(dd$failure())
dd <- Canti_disp$new(quad,indata)
dd$eval()
print(dd$failure())
dd <- Canti_disp$new(circle,indata)
dd$eval()
print(dd$failure())
