# Decomposition-effects-of-widowhood-in-old-age-mortality-in-Argentina-
Paper, Code and Materials for Replication for Paper "Riesgo Adicional en la mortalidad de adultos mayores viudos en Argentina 2015-16"/
"Additional mortality risk in old age widows and widowers in Argentina, 2015-16" 
Paper Published in the Argentine Public Health Journal No 13, 2021 (Revista Argentina de Salud Pública, Nº13, 2021)


http://rasp.msal.gov.ar/rasp/articulos/vol13/AO_Bramajoe28.pdf


The code contains instructions to upload the dataset and basically is a walkthrough to show how to do a 
age-standardization of death rates, a linear Kitagawa/Oaxaca-Blinder decomposition 
(although I suggest using the DemoDecomp package by Tim Riffe https://cran.r-project.org/web/packages/DemoDecomp/index.html)
and an abridged life table using (mainly) base R functions. 
I mixed the code using both tidyverse and base R functions because I´m not a good programmer,
but everything works alright and I believe it is straightforward to understand. 
However, since this article was built in purpose to help students to get familiar with these methods and their logic, 
questions and suggestions are welcome :) 

Many thanks to José Manuel Aburto, Christian Dudel, Tim Riffe, Alyson van Raalte and my EDSD friends (special shoutout to Daniel Zazueta Borboa and Qi Cui for their patience)
for teaching me how to code this and for some of the methods I used here, and to
Daniel Zazueta for his opinions and judgement. 

You can reach me via email (onbramajo@gmail.com / obramajo@ced.uab.es) or by Twitter DM (@octaviobramajo)
