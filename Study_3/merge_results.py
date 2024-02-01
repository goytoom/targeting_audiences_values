#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Aggregates annotated tweet files
Takes annotated tweet files one file for each month) as input
Outputs a single merged file containing all tweets in the individual files

@author: suhaib
"""

import pandas as pd
import glob

modes = ["moral", "binding"]
for mode in modes:
    files = glob.glob("results/tweets/*_" + mode + ".csv")
    df_0 = pd.read_csv(files[0], engine='python')
    for file in files[1:]:
        df = pd.read_csv(file, engine='python')
        df_0 = pd.concat([df_0, df])
    df_0.reset_index(drop=True).to_csv("results/final_tweets_" + mode + ".csv", index = False)