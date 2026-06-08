import sys
import argparse
from presidio_analyzer import AnalyzerEngine
from presidio_anonymizer import AnonymizerEngine
from presidio_anonymizer.entities import OperatorConfig

def main():
    parser = argparse.ArgumentParser(description="PII Anonymizer CLI")
    parser.add_argument("input", help="Path to input file")
    parser.add_argument("-o", "--output", help="Output file path")
    parser.add_argument("-m", "--mode", choices=["replace", "redact", "mask", "hash"], 
                        default="replace", help="Anonymization mode")
    args = parser.parse_args()

    # Read, Analyze, Anonymize
    with open(args.input, 'r') as f: text = f.read()
    analyzer = AnalyzerEngine()
    anonymizer = AnonymizerEngine()
    results = analyzer.analyze(text=text, language='en')

    # Operator Configuration
    operators = {"DEFAULT": OperatorConfig(args.mode, {})}
    if args.mode == "mask":
        operators = {"DEFAULT": OperatorConfig("mask", {"masking_char": "*", "chars_to_mask": 15})}

    # Apply and Save
    anonymized = anonymizer.anonymize(text=text, analyzer_results=results, operators=operators)
    out_path = args.output or f"anonymized_{args.input}"
    with open(out_path, 'w') as f: f.write(anonymized.text)
    print(f"Saved to {out_path}")

if __name__ == '__main__': main()

