import sys
import argparse
import os
try:
    import fitz  # PyMuPDF
    HAS_FITZ = True
except ImportError:
    HAS_FITZ = False

from presidio_analyzer import AnalyzerEngine
from presidio_anonymizer import AnonymizerEngine
from presidio_anonymizer.entities import OperatorConfig

def extract_text(file_path):
    ext = os.path.splitext(file_path)[1].lower()
    if ext == '.pdf':
        if not HAS_FITZ:
            print("Error: PyMuPDF (fitz) is required for PDF files. Install with 'pip install pymupdf'.")
            sys.exit(1)
        doc = fitz.open(file_path)
        text = ""
        for page in doc:
            page_text = page.get_text()
            if not page_text.strip():
                # Try OCR if no text found
                try:
                    tp = page.get_textpage_ocr(language='eng')
                    page_text = tp.extractText()
                except Exception as e:
                    print(f"Warning: OCR failed on a page: {e}")
            text += page_text
        return text
    else:
        with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
            return f.read()

def main():
    parser = argparse.ArgumentParser(description="PII Anonymizer CLI")
    parser.add_argument("input", help="Path to input file")
    parser.add_argument("-o", "--output", help="Output file path")
    parser.add_argument("-m", "--mode", choices=["replace", "redact", "mask", "hash"], 
                        default="replace", help="Anonymization mode")
    args = parser.parse_args()

    # Read/Extract Text
    if not os.path.exists(args.input):
        print(f"Error: File {args.input} not found.")
        sys.exit(1)
    
    print(f"Extracting text from {args.input}...")
    text = extract_text(args.input)
    
    if not text.strip():
        print("Error: No text could be extracted from the file.")
        sys.exit(1)

    # Analyze, Anonymize
    print("Analyzing for PII...")
    analyzer = AnalyzerEngine()
    anonymizer = AnonymizerEngine()
    results = analyzer.analyze(text=text, language='en')

    # Operator Configuration
    operators = {"DEFAULT": OperatorConfig(args.mode, {})}
    if args.mode == "mask":
        operators = {"DEFAULT": OperatorConfig("mask", {"masking_char": "*", "chars_to_mask": 15})}

    # Apply and Save
    print("Anonymizing...")
    anonymized = anonymizer.anonymize(text=text, analyzer_results=results, operators=operators)
    
    out_path = args.output
    if not out_path:
        base, ext = os.path.splitext(args.input)
        if ext.lower() == '.pdf':
            out_path = f"anonymized_{os.path.basename(base)}.txt"
        else:
            out_path = f"anonymized_{os.path.basename(args.input)}"
            
    with open(out_path, 'w', encoding='utf-8') as f:
        f.write(anonymized.text)
    print(f"Saved to {out_path}")
    
    # Bypass Bus error on exit caused by some native libraries in the 'ai' environment
    sys.stdout.flush()
    os._exit(0)

if __name__ == '__main__': main()

