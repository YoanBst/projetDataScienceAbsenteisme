import csv
from datetime import datetime
import re

file_path = './projet-data-science/AbsencesDaMS.tsv'
col1_output = './projet-data-science/Cleaned_Absences.csv'

# regex case 1
pattern_case1 = r"^(.*?)(?:\s+(CTD|TD|CM|TP|CMTD))?(?:\s+(G[12]))?$"
pattern_case2 = r"^\s*(?:(CTD|TD|CM|TP|CMTD)\s+)?(Option [12])$"
# regex case 3
pattern_case3 = r"^(\w+)(?:\s+(.+))?$"

# regex for case when 

# Open the file
with open(file_path, mode='r', newline='', encoding='utf-8') as input_file, \
    open(col1_output, mode='w', newline='', encoding='utf-8') as output_file:
    # delimiter is tab
    tsv_reader = csv.reader(input_file, delimiter='\t')
    csv_writer = csv.writer(output_file)

    csv_writer.writerow(['intitule_cours', 'type_cours', 'groupe_eleve', 'code_cours', 'date', 'civilité', 'code_eleve', 'groupes'])

    count = -1
    for row in tsv_reader:
        count +=1
        if count == 0:
            continue
        col1 = row[0].split(" - ")
        #   CASE 1: 3 cols
        if len(col1) == 3:
            # pour IG3: Intitulé matière, type de cours, groupe  
            # print(match.group(1), " | " , match.group(2), " | " , match.group(3))
            match_case1 = re.match(pattern_case1, col1[0])
            intitule_cours_case1 = match_case1.group(1)
            # "TD" or "CM"
            if match_case1.group(2) is None:
                type_cours_case1 = "PROJ"
            else:
                type_cours_case1 = match_case1.group(2)
            # "G1" or "G2"
            groupe_eleve_case1 = match_case1.group(3)
            csv_writer.writerow([intitule_cours_case1, type_cours_case1, groupe_eleve_case1, col1[1], col1[2], row[3], row[4], row[5]])
        #   CASE 2: 4 cols
        elif len(col1) == 4:
            match_case2 = re.match(pattern_case2, col1[1])
            if match_case2:
                # Group 1: "CMTD" or ""
                type_cours_case2 = match_case2.group(1)
                if type_cours_case2 is None:
                    type_cours_case2 = "CMTD"
                # Group 2: "Option 2" or "Option 1"
                option_eleve_case2 = match_case2.group(2).strip()
                csv_writer.writerow([col1[0], type_cours_case2, option_eleve_case2, col1[2], col1[3], row[3], row[4], row[5]])
            else:
                match_case2_prime = re.match(pattern_case2, col1[0]+" "+col1[1])
                if match_case2_prime:
                    intitule_cours_case2_prime = match_case1.group(1)
                    type_cours_case2_prime = match_case2_prime.group(2)
                    option_eleve_case2_prime = match_case2_prime.group(3)
                    csv_writer.writerow([intitule_cours_case2_prime, type_cours_case2_prime, option_eleve_case2_prime, col1[2], col1[3], row[3], row[4], row[5]])
                else:                    
                    type_cours_case2 = "CM/CMTD"
                    option_eleve_case2 = ""
                    csv_writer.writerow([col1[0]+" "+col1[1], type_cours_case2, option_eleve_case2, col1[2], col1[3], row[3], row[4], row[5]])

        #   CASE 3: 5 cols
        elif len(col1) == 5:
            match_case3 = re.match(pattern_case3, col1[1])
            # "TD" or "CM"
            type_cours_case3 = match_case3.group(1) 
            # "option 1" or None
            option_eleve_case3 = match_case3.group(2)
            csv_writer.writerow([col1[0]+" "+col1[2], type_cours_case3, option_eleve_case3, col1[3], col1[4], row[3], row[4], row[5]])

        elif len(col1) == 6:
            csv_writer.writerow([col1[0]+" "+col1[3],col1[1],col1[2] , col1[4], col1[5], row[3], row[4], row[5]])
        # clean la première colone: 
        # nom de matière + type de cours, code de la matière, heure de début 
        # date_log = datetime.strptime(col1[2], "%d/%m/%Y %H:%M")

input_csv_path = './projet-data-science/Cleaned_Absences.csv'
output_csv_path = './projet-data-science/Final_Absences.csv'

print("Starting cleanup pass...")

with open(input_csv_path, mode='r', newline='', encoding='utf-8') as infile, \
     open(output_csv_path, mode='w', newline='', encoding='utf-8') as outfile:
    
    # Read as dictionary so we can access columns by name
    reader = csv.DictReader(infile)
    fieldnames = reader.fieldnames
    
    # Write to new file keeping the same headers
    writer = csv.DictWriter(outfile, fieldnames=fieldnames)
    writer.writeheader()
    
    rows_processed = 0
    
    for row in reader:
        # --- CLEANUP LOGIC ---

        # 1. Clean 'groupe_eleve': Map "Option 1" -> "Opt1"
        current_group = row['groupe_eleve'].strip() # Remove extra spaces just in case
        
        if current_group.lower() == "option 1":
            row['groupe_eleve'] = "Opt1"
        elif current_group.lower() == "option 2":
            row['groupe_eleve'] = "Opt2"

        # 2. Clean 'type_cours': Map "CTD" -> "CMTD"
        if row['type_cours'] == "CTD":
            row['type_cours'] = "CMTD"
        
        # Write the modified row
        writer.writerow(row)
        rows_processed += 1

print(f"Cleanup complete. {rows_processed} rows processed.")
print(f"File saved to: {output_csv_path}")

import csv

# Paths
input_csv_path = './projet-data-science/Final_Absences.csv' # Use your most recent cleaned file
metadata_output_path = './projet-data-science/Metadata_Cours.csv'

# Dictionary to store the unique mapping
# Key = code_cours, Value = intitule_cours
cours_mapping = {}

with open(input_csv_path, mode='r', newline='', encoding='utf-8') as infile:
    reader = csv.DictReader(infile)
    
    for row in reader:
        code = row['code_cours'].strip()
        name = row['intitule_cours'].strip()
        
        # Only add if we haven't seen this code before
        # and ignore rows where the code might be empty
        if code and code not in cours_mapping:
            cours_mapping[code] = name

# Write the result to a new CSV
with open(metadata_output_path, mode='w', newline='', encoding='utf-8') as outfile:
    writer = csv.writer(outfile)
    
    # Header
    writer.writerow(['code_cours', 'intitule_cours'])
    
    # Write the unique pairs
    for code, name in cours_mapping.items():
        writer.writerow([code, name])

print(f"Metadata file created at {metadata_output_path}")
print(f"Found {len(cours_mapping)} unique courses.")