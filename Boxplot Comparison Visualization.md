# Boxplot Comparison Visualization

## Prompt 1

"FINISH"
please in the boxplots use the a dotted line to indicate the mean
also i would like if every variable got a different colour, however nothing to shiny
Please give me the code for colab so i can create the plot ther e

## Prompt 2

Alter bei Totwenntot": [
        [19.19, 58.74, 64.91, 73.79, 92.12],  # Gesamtpopulation
        [19.19, 58.74, 64.91, 73.79, 92.12],  # HF140
        [19.19, 58.74, 64.91, 73.79, 92.12],  # HF130
        [19.19, 58.74, 64.91, 73.79, 92.12],  # HF150
        [19.19, 58.74, 64.91, 73.79, 92.12],  # HF_Median
    ],
    "GF (ml)": [
        [131.0, 140.5, 152.0, 170.5, 239.0],
        [131.0, 140.5, 152.0, 170.5, 239.0],
        [131.0, 140.5, 152.0, 170.5, 239.0],
        [131.0, 140.5, 152.0, 170.5, 239.0],
        [131.0, 140.5, 152.0, 170.5, 239.0],
    ],
    "NE surv a tim": [
        [26.75, 31.82, 46.43, 62.04, 76.87],
        [26.75, 31.82, 46.43, 62.04, 76.87],
        [26.75, 31.82, 46.43, 62.04, 76.87],
        [26.75, 31.82, 46.43, 62.04, 76.87],
        [26.75, 31.82, 46.43, 62.04, 76.87],
    ],
    "Diabetesdauer totwenntot a tim": [
        [9.19, 45.54, 52.4, 58.31, 73.16],
        [9.19, 45.54, 52.4, 58.31, 73.16],
        [9.19, 45.54, 52.4, 58.31, 73.16],
        [9.19, 45.54, 52.4, 58.31, 73.16],
        [9.19, 45.54, 52.4, 58.31, 73.16],
    ],
    "Manifestationsalter a": [
        [1.0, 9.0, 13.0, 20.0, 30.0],
        [1.0, 9.0, 13.0, 20.0, 30.0],
        [1.0, 9.0, 13.0, 20.0, 30.0],
        [1.0, 9.0, 13.0, 20.0, 30.0],
        [1.0, 9.0, 13.0, 20.0, 30.0],
    ],
    "Highest measured HbA1c": [
        [4.9, 6.7, 7.6, 8.5, 14.8],
        [4.9, 6.7, 7.6, 8.5, 14.8],
        [4.9, 6.7, 7.6, 8.5, 14.8],
        [4.9, 6.7, 7.6, 8.5, 14.8],
        [4.9, 6.7, 7.6, 8.5, 14.8],
What does this section do?

## Prompt 3

id like the latter (code that automates the export, and please make it so i can add new data sets or variables if needed)

## Prompt 4

spalten_urspr <- list(
  "Alter_beitotwenntot_a",
  "GF__ml_",
  "NE_surv_a_tim",
  "Diabetesdauer_totwenntot_a_tim",
  "Manifestationsalter_a_",
  "highest_measured_hba1c_naisna"
)
datensatz<- subset_HyperfiltrationMedian
  
func_stat_descr <- function(spalten_urspr, datensatz){
  for (spalte in spalten_urspr){
    myfunc_spal_exist(spalte, datensatz)
    descr <- describe(datensatz[[spalte]])
    summary <- summary(datensatz[[spalte]])
    print(paste0("descriptive_data",spalte))
    print(descr)
    print(summary)
    print("BREAK")
    Sys.sleep(0.5)
  } 
}


func_stat_descr(spalten_urspr, datensatz)

Thats how i got the data in the first place, can you change that to a way so it isstores and your export works

## Prompt 5

Hi ChatGPT,

I am currently working in RStudio. I have one dataframe with all the data and some subsets that fullfill certain requirements. So in total I have five dataframes and want to compare their distribution using boxplots. I want to implement them using python and pandas for instance. 

However, I am currently struggeling with how to export the data from R so I can use it with python.
I want to save a  dataframe containing the 6(or so) variables as raw data for each dataframe i want to analyse, whats a way to create a .csv like that inR?
it should lookm something like this:

variable_name, ds1, ds2, ds3
v1,5,43,63
v2,534,53,54
v3,543,543,534

## Prompt 6

Cool that works, can you adjust the function in a way so i have all my datasets in one csv (for one variable)?

## Prompt 7

import pandas as pd
import matplotlib.pyplot as plt

# === Helper: Load CSV summaries into a dictionary === #
def load_summary(filename):
    df = pd.read_csv(filename)
    stats = {
        "min": float(df.loc[0, "min"]),
        "q1": float(df.loc[0, "1st Qu."]),
        "median": float(df.loc[0, "Median"]),
        "mean": float(df.loc[0, "mean"]),
        "q3": float(df.loc[0, "3rd Qu."]),
        "max": float(df.loc[0, "max"]),
        "n": int(df.loc[0, "n"]),
        "na": int(df.iloc[1, -1]) if "NA's" in df.columns else 0  # Adjust if NA is on second row
    }
    return stats

# === Configure your dataset groups and variables === #
datasets = ["Gesamt", "HF140", "HF130", "HF150", "HF_Median"]
variables = {
    "Alter bei Tot":       ["Alter_beitotwenntot_a_"],
    "GF (ml)":             ["GF_ml_"],
    "NE surv a tim":       ["NE_surv_a_tim_"],
    "Diabetesdauer":       ["Diabetesdauer_totwenntot_a_tim_"],
    "Manifestationsalter": ["Manifestationsalter_a_"],
    "HbA1c":               ["HbA1c_"]
}

# === Define colors per variable (muted palette) === #
colors = {
    "Alter bei Tot": "#a6bddb",
    "GF (ml)": "#b2df8a",
    "NE surv a tim": "#fdbf6f",
    "Diabetesdauer": "#cab2d6",
    "Manifestationsalter": "#ff9999",
    "HbA1c": "#8da0cb"
}

# === Loop over variables and create boxplots === #
for var, file_roots in variables.items():
    stats_per_dataset = []
    n_labels = []

    for ds in datasets:
        filename = f"{file_roots[0]}{ds}.csv"
        summary = load_summary(filename)

        stats_per_dataset.append([
            summary["min"],
            summary["q1"],
            summary["median"],
            summary["q3"],
            summary["max"]
        ])
        n_labels.append(f"n={summary['n']}, NA={summary['na']}")

    # === Plot === #
    fig, ax = plt.subplots(figsize=(8, 6))
    bp = ax.boxplot(stats_per_dataset, patch_artist=True, widths=0.6)

    # Coloring
    for patch in bp['boxes']:
        patch.set_facecolor(colors[var])

    # Dotted mean lines
    for i, ds in enumerate(datasets):
        mean_val = load_summary(f"{file_roots[0]}{ds}.csv")["mean"]
        ax.hlines(mean_val, i + 0.75, i + 1.25, linestyles='dotted', colors='black')

    # Labeling
    ax.set_title(var)
    ax.set_xticks(range(1, 6))
    ax.set_xticklabels(datasets, rotation=45)
    ax.set_ylabel(var)
    for i, label in enumerate(n_labels):
        y_max = max(stats_per_dataset[i])
        ax.text(i + 1, y_max + 0.05 * y_max, label, ha='center', fontsize=8)

    plt.tight_layout()
    plt.show()

can you adjust this code, i ahve the csv uploaded to colab with the names given, do i even need adjustemnts?

## Prompt 8

FileNotFoundError                         Traceback (most recent call last)
<ipython-input-3-4f9223399f3d> in <cell line: 0>()
     25 for var_name, filename in variables.items():
     26     # Load the combined summary table
---> 27     df = pd.read_csv(filename)
     28 
     29     # Extract five-number summaries

4 frames
/usr/local/lib/python3.11/dist-packages/pandas/io/common.py in get_handle(path_or_buf, mode, encoding, compression, memory_map, is_text, errors, storage_options)
    871         if ioargs.encoding and "b" not in ioargs.mode:
    872             # Encoding
--> 873             handle = open(
    874                 handle,
    875                 ioargs.mode,

FileNotFoundError: [Errno 2] No such file or directory: 'Manifestationsalter_a_ALL.csv'

## Prompt 9

cool thanks, however i want to change the median line to a thick black line instead of a thin orange one, also more spacing between title and plot would be cool, 
the n and NA information is being shown under or over the upper frame line depending on how high the plot reaches, if we could increase the height of the frame so this information is always inside that would be nice too
finally showing a grid in the background could help with interpretation

## Prompt 10

i got this:# Required packages
library(psych)
library(dplyr)
library(tibble)
library(flextable)
library(officer)

# Define your datasets
datasets <- list(ds1 = da_daten)

# Original variable names (as in the data)
original_vars <- c(
  "Alter_beitotwenntot_a", 
  "GF__ml_", 
  "NE_surv_a_tim", 
  "Diabetesdauer_totwenntot_a_tim", 
  "Manifestationsalter_a_", 
  "highest_measured_hba1c_naisna"
)

# New readable names (used in final table)
readable_names <- c(
  "Alter", 
  "Filtrationsrate", 
  "Nierenüberleben", 
  "Diabetesdauer", 
  "Manifestationsalter", 
  "höchster dokumentierter HbA1c"
)

# Function to compute and format the statistics
get_formatted_stats <- function(datensatz, varname) {
  values <- datensatz[[varname]]
  descr <- describe(values)
  
  na_count <- sum(is.na(values))
  total <- length(values)
  na_percent <- round((na_count / total) * 100, 1)
  
  paste0(
    round(descr$mean, 2), " (", round(descr$min, 2), " / ", round(descr$max, 2), ")\n",
    "n = ", descr$n, ", SD = ", round(descr$sd, 2), "\n",
    "NA = ", na_count, " (", na_percent, "%)"
  )
}

# Create the final table
pretty_table <- tibble(Variable = readable_names)

for (ds_name in names(datasets)) {
  datensatz <- datasets[[ds_name]]
  column <- sapply(original_vars, function(var) get_formatted_stats(datensatz, var))
  pretty_table[[ds_name]] <- column
}

# Create the flextable
ft <- flextable(pretty_table)
ft <- autofit(ft)
ft <- set_table_properties(ft, width = 1.0, layout = "autofit")

# Export to Word
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
print(doc, target = "Deskriptive_Statistik_Tabelle1.docx")
but i want to change it in a way so that its one table but each value (mean/sd/n/min/max...has a seperate column

## Prompt 11

cool, what would i have to change in order to append a % to the NA percent values?

## Prompt 12

back to the boxplots, i tweaked it a bit and now it works like a charm 
i want to save them so i can use them in my paper (word) how do i do that?

## Prompt 13

nice i just want to change the colour of one boxplot to more greyish, as th two blue tones ware quite similar, can you just give me the colour code, i can change it in colab myself

## Prompt 14

i want to change the boxplots, so that i can fit two of them ext to each other onto one A4 page n and change thickness of th lines background grid as well as the size of the text involved accordingly
and i saw there is a possibility to plot the distribution as dots next to the boxplot and id like to include that in the graph

 also please just explain quickly what i would have to do if i want to include another variable in the boxplot code

## Prompt 15

cool please give me the updated code,
and for the stripplot id need the raw data, which means id like to get a code from r that allows to export one variable from my dataset in form of a 1*x dataframe as cvs to load it into colab
also colab must be adjusted so it would give me all the parameters i have previously calculated in r for the boxplots

## Prompt 16

To combine the raw export into a loop on the R side

## Prompt 17

Can i not just upload the raw data and  calculate the summary information in colab? 
Also is there a way to change the export of the raw data in a way so that i only get one file per dataset with all raw date of the needed variables?

## Prompt 18

Let me know if you'd like the full updated plotting code to work with this new structure. You're almost at one-click figure generation
yes please give me the updated code

## Prompt 19

could you make it so the stripplot is shown next to the respective boxplot instead of behind them

## Prompt 20

also please give me a code for a violin plot. id like to see how it looks

## Prompt 21

File "<tokenize>", line 100
    for i, values in enumerate(raw_data_points):
    ^
IndentationError: unindent does not match any outer indentation level

## Prompt 22

can you just give me the updated stripplot version? because the error only occured after the last amendment to the stripplot (It happens in the boxplot code, the violin pcode runs fine i think)

## Prompt 23

can you please include all exported variables into the violin plot code

## Prompt 24

oh sorry i kept you out of the loop when introducing 3 new variables (they are exported) "BMI",
  "egfr_ckdepi2009_tim",
  "Clearance_MDRD2"
can you eupdate both plot codes to include them please

## Prompt 25

i think im almost done with the boxplots, only the fontsize of NA= ... and n=... on top of ecch greaph coould be al little bigger, can you show me wehere i can chage that?

## Prompt 26

Thanks I have changed the code a tiny little bit and added one more column (and changed a few variables): import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import os

# === Configure variables === #
variables = {
    "Alter": "Alter_a_einschluss",
    "GF (ml)": "Kreatininclearance_endogen",
    "Nierenüberleben": "NE_surv_a_tim",
    "Diabetesdauer": "Diabetesdauer_a_",
    "Manifestationsalter": "Manifestationsalter_a_",
    "HbA1c": "highest_measured_hba1c_naisna",
    "BMI": "BMI",
    "eGFR (CKD-EPI)": "egfr_ckdepi2009_tim",
    "eGFR (MDRD2)": "Clearance_MDRD2"
}

# Define dataset files
dataset_files = {
    "Gesamt": "Gesamt_raw.csv",
    "HF130": "HF130_raw.csv",
    "HF140": "HF140_raw.csv",
    "HF150": "HF150_raw.csv",
    "HF120": "HF120_raw.csv",
    "HF_Median": "HF_Median_raw.csv"
}

# Define variable colors
colors = {
    "Alter": "#b2df8a",
    "GF (ml)": "#9ea7aa",
    "Nierenüberleben": "#fdbf6f",
    "Diabetesdauer": "#cab2d6",
    "Manifestationsalter": "#ff9999",
    "HbA1c": "#8da0cb",
    "BMI": "#cfcfcf",
    "eGFR (CKD-EPI)": "#a1dab4",
    "eGFR (MDRD2)": "#c6dbef"
}
# === Loop over variables === #
for var_label, var_column in variables.items():
    stats_per_dataset = []
    means = []
    n_labels = []
    dataset_labels = []
    raw_data_points = []

    for ds_name, file in dataset_files.items():
        if not os.path.exists(file):
            print(f"⚠️ File missing: {file}")
            continue

        df = pd.read_csv(file)
        if var_column not in df.columns:
            print(f"⚠️ Column {var_column} not in {file}")
            continue

        values = df[var_column].dropna()
        if len(values) == 0:
            continue

        stats = {
            "min": values.min(),
            "q1": values.quantile(0.25),
            "median": values.median(),
            "mean": values.mean(),
            "q3": values.quantile(0.75),
            "max": values.max(),
            "n": values.count(),
            "na": df[var_column].isna().sum()
        }

        stats_per_dataset.append([
            stats["min"], stats["q1"], stats["median"],
            stats["q3"], stats["max"]
        ])
        means.append(stats["mean"])
        n_labels.append(f"n={stats['n']}, NA={stats['na']}")
        dataset_labels.append(ds_name)
        raw_data_points.append(values)

    # === Plotting === #
    fig, ax = plt.subplots(figsize=(5.8, 8))  # Half A4 portrait

    bp = ax.boxplot(
        stats_per_dataset,
        patch_artist=True,
        widths=0.5,
        medianprops=dict(color='black', linewidth=2.5),
        boxprops=dict(linewidth=1.5),
        whiskerprops=dict(linewidth=1.5),
        capprops=dict(linewidth=1.5),
        flierprops=dict(marker='o', markersize=4, alpha=0.4)
    )

    # Box colors
    for patch in bp['boxes']:
        patch.set_facecolor(colors[var_label])

    # Mean line
    for i, mean_val in enumerate(means):
        ax.hlines(mean_val, i + 0.75, i + 1.25, linestyles='dotted', colors='black')


  # === Stripplot (dot distribution NEXT to the box) === #
    for i, values in enumerate(raw_data_points):
      colour= colors[var_label]
      offset = 0.37  # horizontal shift from the box center
      jitter_x = np.random.normal(loc=i + 1 + offset, scale=0.03, size=len(values))
      ax.scatter(jitter_x, values, alpha=0.3, s=10, color=colour )

    # Grid and styling
    ax.set_title(var_label, fontsize=14, pad=20)
    ax.set_ylabel(var_label, fontsize=12)
    ax.set_xticks(range(1, len(dataset_labels)+1))
    ax.set_xticklabels(dataset_labels, rotation=45, fontsize=10)
    ax.yaxis.grid(True, linestyle='--', linewidth=0.5, color='gray', alpha=0.6)

    # Extend height to fit text
    y_max = max([x[4] for x in stats_per_dataset]) * 1.15
    ax.set_ylim(top=y_max)
    for i, label in enumerate(n_labels):
        y_box_max = stats_per_dataset[i][4]
        y_pos = y_box_max + (y_max - y_box_max) * 0.05
        ax.text(i + 1, y_pos, label, ha='center', fontsize=9)

    # Save and show
    plt.tight_layout()
    filename = f"{var_label.replace(' ', '_')}_boxplot_with_strip.png"
    plt.savefig(filename, dpi=300, bbox_inches='tight')
    plt.show()

    print(f"✅ Saved: {filename}")

However now the plots look like this (see picture) and especially the wiriting n=... is a bit packed and the loeftmost plot the n is in the line, i think it might help to put the n= and the ANA= on top of each other
