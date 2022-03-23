import seaborn as sns
import pandas as pd

sheet1, sheet2 = None, None
with pd.ExcelFile(
    "/home/erika.kvalem/Documents/Relecov/relecov_qc_exercise/files/QC_final.xlsx"
) as reader:
    sheet1 = pd.read_excel(reader, sheet_name="resultados")
    # sheet2 = pd.read_excel(reader, sheet_name='Sheet2')


print(sheet1["var_readcount"])

"""
sns.set_theme(style="whitegrid")

tips = sns.load_dataset("tips")

#ax = sns.boxplot(x=tips["total_bill"])

ax = sns.boxplot(x="day", y="total_bill", data=tips)
"""
