# R Stats Table Erweiterung

## Prompt 1

get_stats_table <- function(datensatz, original_vars, readable_names) {
  results <- lapply(seq_along(original_vars), function(i) {
    var <- original_vars[i]
    values <- datensatz[[var]]
    descr <- psych::describe(values)
    
    na_count <- sum(is.na(values))
    total <- length(values)
    na_percent <- round((na_count / total) * 100, 1)
    
    tibble(
      Variable = readable_names[i],
      Mean = round(descr$mean, 2),
      SD = round(descr$sd, 2),
      Min = round(descr$min, 2),
      Max = round(descr$max, 2),
      n = descr$n,
      NA_Count = na_count,
      NA_Percent = paste0(na_percent, "%")
    )
  })
  

Ich habe das und ich möchte (in R) die Parameter um Median und interquartilsrange (IQR) ergänzen, (zwischen SD und Min), was wären die richrtigen commands dafür?

## Prompt 2

okay thanks, now im working in google colab using python and i have this for my boxplots: import pandas as pd
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
    "eGFR (CKD-EPI)": "egfr_ckdepi2009_tim"
}

# Define dataset files
dataset_files = {
    "Gesamt": "Gesamt_raw.csv",
    "HF120": "HF120_raw.csv",
    "HF130": "HF130_raw.csv",
    "HF140": "HF140_raw.csv",
    "HF150": "HF150_raw.csv",
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
    "eGFR (CKD-EPI)": "#a1dab4"
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
        n_labels.append(f"n={stats['n']}\nNA={stats['na']}")
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

It works fairly well, however in some plots the top whiskers are missing

## Prompt 3

import pandas as pd
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
    "eGFR (CKD-EPI)": "egfr_ckdepi2009_tim"
}

# Define dataset files
dataset_files = {
    "Gesamt": "Gesamt_raw.csv",
    "HF120": "HF120_raw.csv",
    "HF130": "HF130_raw.csv",
    "HF140": "HF140_raw.csv",
    "HF150": "HF150_raw.csv",
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
    "eGFR (CKD-EPI)": "#a1dab4"
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
        n_labels.append(f"n={stats['n']}\nNA={stats['na']}")
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

thats the whole function, would you mind altering it using fix B
