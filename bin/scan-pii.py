import sys
from presidio_analyzer import AnalyzerEngine

# 1. Load the text file from the command line argument
file_path = sys.argv[1]
with open(file_path, 'r') as f:
    text = f.read()

# 2. Initialize the engine and analyze
analyzer = AnalyzerEngine()
results = analyzer.analyze(text=text, language='en')

# 3. Print findings to terminal
print(f"\n--- PII Found in {file_path} ---")
for res in results:
    found_text = text[res.start:res.end]
    print(f"[{res.entity_type}] Found: '{found_text}' (Score: {res.score:.2f})")

