import sys
import argparse
import os
try:
    import fitz  # PyMuPDF
    HAS_FITZ = True
except ImportError:
    HAS_FITZ = False

from presidio_analyzer import AnalyzerEngine, PatternRecognizer, Pattern
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
    parser.add_argument("input", nargs="?", default="-", help="Path to input file or '-' for stdin (default: -)")
    parser.add_argument("-o", "--output", help="Output file path or '-' for stdout")
    parser.add_argument("-m", "--mode", choices=["replace", "redact", "mask", "hash"], 
                        default="replace", help="Anonymization mode")
    args = parser.parse_args()

    # Read/Extract Text
    if args.input != "-":
        if not os.path.exists(args.input):
            print(f"Error: File {args.input} not found.", file=sys.stderr)
            sys.exit(1)
        print(f"Extracting text from {args.input}...", file=sys.stderr)
        text = extract_text(args.input)
    else:
        if sys.stdin.isatty():
            print("Reading from stdin... (Press Ctrl+D to end)", file=sys.stderr)
        text = sys.stdin.read()
    
    if not text.strip():
        print("Error: No text could be extracted or read.", file=sys.stderr)
        sys.exit(1)

    # Analyze, Anonymize
    print("Analyzing for PII...", file=sys.stderr)
    
    # Custom recognizers for keys, secrets, tokens
    bearer_pattern = Pattern(
        name="bearer_token_pattern",
        regex=r"(?i)\bbearer\s+[a-zA-Z0-9_\-\.~+\/=]+",
        score=0.95
    )

    generic_key_pattern = Pattern(
        name="generic_key_pattern",
        regex=r"(?i)(?:['\"]?(?:api[_-]?key|secret[_-]?key|access[_-]?token|auth[_-]?token|private[_-]?key|db[_-]?password|password|db[_-]?conn)['\"]?\s*[:=]\s*['\"]?[a-zA-Z0-9_\-\.~+\/=]{8,}['\"]?)",
        score=0.85
    )

    aws_pattern = Pattern(
        name="aws_key_pattern",
        regex=r"\bAKIA[A-Z0-9]{16}\b",
        score=0.85
    )

    jwt_pattern = Pattern(
        name="jwt_pattern",
        regex=r"\b(ey[a-zA-Z0-9_-]{10,}\.[a-zA-Z0-9_-]{10,}\.[a-zA-Z0-9_-]{10,})\b",
        score=0.95
    )

    bearer_recognizer = PatternRecognizer(
        supported_entity="KEY",
        patterns=[bearer_pattern],
        context=["bearer", "token", "auth", "authorization"]
    )

    key_recognizer = PatternRecognizer(
        supported_entity="KEY",
        patterns=[generic_key_pattern],
        context=["api", "key", "secret", "token", "password"]
    )

    aws_recognizer = PatternRecognizer(
        supported_entity="KEY",
        patterns=[aws_pattern],
        context=["aws", "amazon", "access", "key"]
    )

    jwt_recognizer = PatternRecognizer(
        supported_entity="KEY",
        patterns=[jwt_pattern],
        context=["jwt", "token"]
    )

    # Custom recognizer for local filepaths
    filepath_regex = (
        r"(?:"
        r"(?i:file://)(?:localhost)?/(?:[a-zA-Z0-9_.\-~+@]+[/\\])*[a-zA-Z0-9_.\-~+@]*[a-zA-Z0-9_~+@/]"
        r"|"
        r"(?<![a-zA-Z0-9_\-.~+@/\\:])(?:[a-zA-Z]:[/\\](?:[a-zA-Z0-9_.\-~+@ ]+[/\\])*[a-zA-Z0-9_.\-~+@]*[a-zA-Z0-9_~+@/\\])"
        r"|"
        r"(?<![a-zA-Z0-9_\-.~+@/\\:])(?:\\\\[a-zA-Z0-9_\.\-]+(?:\\[a-zA-Z0-9_.\-~+@ ]+)*\\[a-zA-Z0-9_.\-~+@]*[a-zA-Z0-9_~+@\\])"
        r"|"
        r"(?<![a-zA-Z0-9_\-.~+@/\\:])(?:~[a-zA-Z0-9_\.\-]*(?:/[a-zA-Z0-9_.\-~+@]+)+[a-zA-Z0-9_.\-~+@]*[a-zA-Z0-9_~+@/])"
        r"|"
        r"(?<![a-zA-Z0-9_\-.~+@/\\:])(?:\.{1,2}/(?:[a-zA-Z0-9_.\-~+@]+/)*[a-zA-Z0-9_.\-~+@]*[a-zA-Z0-9_~+@/])"
        r"|"
        r"(?<![a-zA-Z0-9_\-.~+@/\\:])/(?:[a-zA-Z0-9_.\-~+@]+(?:\\ [a-zA-Z0-9_.\-~+@]+)*/)+[a-zA-Z0-9_.\-~+@]*[a-zA-Z0-9_~+@/]"
        r")"
    )

    filepath_pattern = Pattern(
        name="filepath_pattern",
        regex=filepath_regex,
        score=0.95
    )

    filepath_recognizer = PatternRecognizer(
        supported_entity="FILE_PATH",
        patterns=[filepath_pattern],
        context=["path", "file", "directory", "folder"]
    )

    analyzer = AnalyzerEngine()
    analyzer.registry.add_recognizer(bearer_recognizer)
    analyzer.registry.add_recognizer(key_recognizer)
    analyzer.registry.add_recognizer(aws_recognizer)
    analyzer.registry.add_recognizer(jwt_recognizer)
    analyzer.registry.add_recognizer(filepath_recognizer)

    anonymizer = AnonymizerEngine()
    results = analyzer.analyze(text=text, language='en')

    # Operator Configuration
    operators = {"DEFAULT": OperatorConfig(args.mode, {})}
    if args.mode == "mask":
        operators = {"DEFAULT": OperatorConfig("mask", {"masking_char": "*", "chars_to_mask": 15, "from_end": True})}

    # Apply and Save
    print("Anonymizing...", file=sys.stderr)
    anonymized = anonymizer.anonymize(text=text, analyzer_results=results, operators=operators)
    
    out_path = args.output
    if out_path == "-":
        sys.stdout.write(anonymized.text)
        sys.stdout.flush()
    elif not out_path and args.input == "-":
        sys.stdout.write(anonymized.text)
        sys.stdout.flush()
    else:
        if not out_path:
            base, ext = os.path.splitext(args.input)
            if ext.lower() == '.pdf':
                out_path = f"anonymized_{os.path.basename(base)}.txt"
            else:
                out_path = f"anonymized_{os.path.basename(args.input)}"
                
        with open(out_path, 'w', encoding='utf-8') as f:
            f.write(anonymized.text)
        print(f"Saved to {out_path}", file=sys.stderr)
    
    # Bypass Bus error on exit caused by some native libraries in the 'ai' environment
    sys.stdout.flush()
    os._exit(0)

if __name__ == '__main__': main()

