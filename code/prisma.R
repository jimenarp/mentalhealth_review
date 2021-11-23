library(PRISMAstatement)
library(rsvg)
library(DiagrammeRsvg)
library(devtools)
install_github("nealhaddaway/PRISMA2020")


# prisma(found = 1530,
#        found_other = 103,
#        no_dupes = 1583, 
#        screened = 1583, 
#        screen_exclusions = 1463, 
#        full_text = 120,
#        full_text_exclusions = 50, 
#        qualitative = 70, 
#        quantitative = 29,
#        width = 800, height = 800)
# https://estech.shinyapps.io/prisma_flowdiagram/

a<-prisma(found = 1530,
       found_other = 109,
       no_dupes = 1589, 
       screened = 1589, 
       screen_exclusions = 1463, 
       full_text = 126,
       full_text_exclusions = 52, 
       qualitative = 74, 
       quantitative = 56,
       width = 800, height = 900)

a<-
        prisma(found = 1530,
               found_other = 110,
               no_dupes = 1590, 
               screened = 1590, 
               screen_exclusions = 1463, 
               full_text = 127,
               full_text_exclusions = 70, 
               qualitative = 57,
               quantitative = 57,
               width = 800, height = 900)
setwd(diroverleaf)

#save(a, file='figures/prisma.pdf')      
tmp_pdf <- tempfile()
PRISMAstatement:::prisma_pdf(a, tmp_pdf)
knitr::include_graphics(path = tmp_pdf)
unlink(tmp_pdf)


pdf(file = 'figures/prisma.pdf', height=13, width=7) 

prisma(found = 1530,
       found_other = 110,
       no_dupes = 1590, 
       screened = 1590, 
       screen_exclusions = 1463, 
       full_text = 127,
       full_text_exclusions = 70, 
       qualitative = 57,
       quantitative = 57,
       width = 800, height = 900)
dev.off() 