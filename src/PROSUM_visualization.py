# # METHOD 1
#
# import pandas as pd
# import matplotlib.pyplot as plt
# import os
# import re
#
# # Load PROSUM output
# csv_path = os.path.join(os.path.dirname(__file__), 'PROSUM_outputs.csv')
# df = pd.read_csv(csv_path, skipinitialspace=True)
# df.columns = df.columns.str.strip()
#
# time = df['Month']
#
# # Select columns
# nut_cols = [c for c in df if c.startswith('Uptake_') and c.endswith('mo')]
# rld_cols = [c for c in df if c.startswith('RootLengthDensity_Layer')]
# litter_cols = [c for c in df if c.startswith('Litterfall_C_Layer')]
#
# # Create 2x2 grid of subplots
# fig, axes = plt.subplots(2, 2, figsize=(12, 10), sharex=True)
#
# # 1) Plant C and N Stocks
# ax1 = axes[0, 0]
# ax1.scatter(time, df['Plant_C_Mpm2'], s=15, label='Plant C stock', alpha=0.7)
# ax1.scatter(time, df['Plant_N_Mpm2'], s=15, label='Plant N stock', alpha=0.7)
# ax1.set_ylabel('Stock (mol m$^{-2}$)')
# ax1.set_title('Plant Carbon & Nitrogen Stocks')
# ax1.grid(True)
# ax1.legend(title='Variables', fontsize='small', loc='upper left')
#
# # 2) Nutrient Uptake
# ax2 = axes[0, 1]
# for col in nut_cols:
#     nutrient = col.split('_')[1]
#     ax2.scatter(time, df[col], s=15, alpha=0.6, label=f'Uptake {nutrient}')
# ax2.set_ylabel('Uptake (mol m$^{-2}$ mo$^{-1}$)')
# ax2.set_title('Monthly Nutrient Uptake Rates')
# ax2.grid(True)
# ax2.legend(title='Nutrient', fontsize='small', ncol=3, loc='upper left')
#
# # 3) Root Length Density
# ax3 = axes[1, 0]
# for i, col in enumerate(rld_cols):
#     layer = re.search(r'Layer(\d+)', col).group(1)
#     ax3.scatter(time, df[col], s=15, alpha=0.6, marker=['o','s','^','d'][i % 4], label=f'Layer {layer}')
# ax3.set_xlabel('Month')
# ax3.set_ylabel('Root Length Density (m m$^{-3}$)')
# ax3.set_title('Root Length Density by Soil Layer')
# ax3.grid(True)
# ax3.legend(title='Soil Layer', fontsize='small', loc='upper left')
#
# # 4) Litterfall Carbon by Layer
# ax4 = axes[1, 1]
# offsets = [-0.2, 0, 0.2, 0.4]
# for i, col in enumerate(litter_cols):
#     layer = re.search(r'Layer(\d+)', col).group(1)
#     ax4.scatter(time + offsets[i], df[col], s=15, alpha=0.6, label=f'Layer {layer}')
# ax4.set_xlabel('Month')
# ax4.set_ylabel('Litterfall C (mol m$^{-2}$ mo$^{-1}$)')
# ax4.set_title('Litterfall Carbon by Layer')
# ax4.grid(True)
# ax4.legend(title='Soil Layer', fontsize='small', loc='upper left')
#
# plt.tight_layout()
# plt.show()

# METHOD 2

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import os, re

csv_path = os.path.join(os.path.dirname(__file__), 'PROSUM_outputs.csv')
df = pd.read_csv(csv_path, skipinitialspace=True)
df.columns = df.columns.str.strip()

df['MonthIndex'] = np.arange(1, len(df) + 1)
df10 = df.loc[df['MonthIndex'] <= 120].copy()

time10     = df10['MonthIndex']
nut_cols    = [c for c in df10 if c.startswith('Uptake_') and c.endswith('mo')]
rld_cols    = [c for c in df10 if c.startswith('RootLengthDensity_Layer')]
litter_cols = [c for c in df10 if c.startswith('Litterfall_C_Layer')]


fig, axes = plt.subplots(2, 2, figsize=(12, 10), sharex=True)
alpha = 0.9

ax1 = axes[0,0]
ax1.plot(time10, df10['Plant_C_Mpm2'],   label='Plant C stock', linewidth=1.5, alpha=alpha)
ax1.plot(time10, df10['Plant_N_Mpm2'],   label='Plant N stock', linewidth=1.5, alpha=alpha)
ax1.set_title('Plant Carbon & Nitrogen Stocks (first 10 yrs)')
ax1.set_ylabel('Stock (mol m$^{-2}$)')
ax1.grid(True)
ax1.legend(loc='upper left', fontsize='small')

ax2 = axes[0,1]
for col in nut_cols:
    nut = col.split('_')[1]
    ax2.plot(time10, df10[col], label=f'Uptake {nut}', linewidth=1.2, alpha=alpha)
ax2.set_title('Monthly Nutrient Uptake Rates')
ax2.set_ylabel('Uptake (mol m$^{-2}$ mo$^{-1}$)')
ax2.grid(True)
ax2.legend(title='Nutrient', fontsize='small', ncol=3, loc='upper left')

ax3 = axes[1,0]
markers = ['o','s','^','d']
for i, col in enumerate(rld_cols):
    lyr = re.search(r'Layer(\d+)', col).group(1)
    ax3.plot(time10, df10[col], marker=markers[i], markevery=6,
             label=f'Layer {lyr}', linewidth=1.2, alpha=alpha)
ax3.set_title('Root Length Density by Soil Layer')
ax3.set_xlabel('Month Index')
ax3.set_ylabel('RLD (m m$^{-3}$)')
ax3.grid(True)
ax3.legend(fontsize='small')

ax4 = axes[1,1]
offsets = [-0.6, -0.2, 0.2, 0.6]
for i, col in enumerate(litter_cols):
    lyr = re.search(r'Layer(\d+)', col).group(1)
    # plot with a slight horizontal offset
    ax4.plot(time10 + offsets[i], df10[col], marker=markers[i], markevery=6,
             label=f'Layer {lyr}', linewidth=1.2, alpha=alpha)
ax4.set_title('Litterfall Carbon by Layer')
ax4.set_xlabel('Month Index')
ax4.set_ylabel('Litterfall C (mol m$^{-2}$ mo$^{-1}$)')
ax4.grid(True)
ax4.legend(fontsize='small')

plt.tight_layout()
plt.show()
