import camelot

def extract_tables_to_json(pdf_path):

    tables = camelot.read_pdf(
        pdf_path,
        pages='all',
        flavor='stream'
    )

    result = {
        "total_tables": len(tables),
        "tables": []
    }

    for i, table in enumerate(tables):
        result["tables"].append({
            "table_number": i + 1,
            "accuracy": table.parsing_report["accuracy"],
            "data": table.df.to_dict("records")
        })

    return result