
import numpy as np

import camelot
import json 

from pathlib import Path 
    
def extract_tables_to_json(pdf_path):
    # Use 'lattice' for PDFs with visible borders
    tables = camelot.read_pdf(
        pdf_path, 
        pages='all', 
        flavor='stream')
    
    result = {
        "total_tables": len(tables),
        "tables": []
    }
    
    for i, table in enumerate(tables):
        result["tables"].append({
            "table_number": i + 1,
            "accuracy": table.parsing_report['accuracy'],
            "data": table.df.to_dict('records')
        })
    
    return json.dumps(result, indent=2)


from pathlib import Path

folder = Path("OneDrive_1_6-10-2026")

for pdf_file in folder.glob("*.pdf"):
    print(pdf_file)

    for pdf_file in folder.glob("*.pdf"):

        json_data=extract_tables_to_json(str(pdf_file))

        output_file = pdf_file.with_suffix(".json")

        with open(output_file, "w") as f:
            f.write(json_data)
      