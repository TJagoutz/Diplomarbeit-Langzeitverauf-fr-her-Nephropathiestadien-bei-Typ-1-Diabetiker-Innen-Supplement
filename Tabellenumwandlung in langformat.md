# Tabellenumwandlung in langformat

## Prompt 1

Hi, I did this:
table_list <- list()

absh_relh_table <- function(spalten_urspr, da_daten) {
  for(spalte in spalten_urspr){
    myfunc_spal_exist(spalte, da_daten) 
    #absh
    absh <- table(da_daten[[spalte]])
    #relh
    relh <- round(prop.table(table(da_daten[[spalte]]))*100,2)
    
    new_table <- cbind(absh, relh)
    
    table_list[[paste0("tabelle_",spalte)]] <<- new_table
    
  }  
}

absh_relh_table(spalten_urspr, da_daten)
print(table_list[['tabelle_Hyperfiltration130_1_ja']])

but instead of this list I want to have a long table like this:

absh_kind, relh_kind, absh_tot, relh_tot,....
0 | 102, 0.56, ....
1 | 202, ....

## Prompt 2

perfect, how can i change x and y axis?

## Prompt 3

i would like to change the forat in a way that has the variable names listed vertically and the values listed horizontally:

## Prompt 4

one variable has the values 1,2,3,4 instead of 0,1, can i bring the programm to ignoring this disparity?#

## Prompt 5

how can i change 0 to na ?

## Prompt 6

why do i not get barplots?:
func_barplot <- function(spalten_urspr, da_daten) {
  n<-0
  for(spalte in spalten_urspr) {
   barplot <- barplot(table(da_daten[[spalte]]), main = paste0(spalte,"_Barplot"), ylab ="Anzahl", xlab="1=True", col = n)
  }
  n <- n+1
  return(barplot)
  print(barplot)
}
func_barplot

## Prompt 7

why do i not getbarplots?

## Prompt 8

i used this code, it doesnt print barplots

## Prompt 9

can i improve the function in a way that
1) the y axis never ends lower that the highest ba

## Prompt 10

also i would like an indication of the exact number for every bar
