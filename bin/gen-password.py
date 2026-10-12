#!/usr/bin/env python3
import sys
import os
import secrets
import argparse

FALLBACK_WORDS = [
    # 4 letters
    "blue", "gold", "gray", "pink", "pure", "soft", "bold", "cool", "warm", "dark",
    "kind", "wild", "safe", "calm", "fine", "neat", "open", "free", "glad", "good",
    "star", "moon", "sun", "wind", "rain", "snow", "fire", "wave", "lake", "pond",
    "tree", "leaf", "bark", "root", "seed", "rose", "lily", "pine", "fern", "moss",
    "bird", "fish", "deer", "bear", "lion", "wolf", "frog", "hawk", "swan", "dove",
    "hope", "wish", "love", "life", "time", "path", "road", "gate", "door", "home",
    "book", "page", "desk", "boat", "ship", "city", "town", "rock", "sand", "clay",
    # 5 letters
    "green", "brown", "white", "black", "sweet", "quiet", "brave", "happy", "smart",
    "water", "earth", "river", "beach", "ocean", "cloud", "storm", "light", "night",
    "sound", "music", "dream", "truth", "peace", "world", "stone", "grass", "plant",
    "maple", "birch", "tulip", "daisy", "sheep", "tiger", "horse", "camel", "eagle",
    "shark", "whale", "robin", "clock", "house", "table", "chair", "paper", "glass",
    # 6 letters
    "yellow", "orange", "purple", "silver", "bronze", "copper", "gentle", "strong",
    "clever", "bright", "autumn", "spring", "summer", "winter", "forest", "stream",
    "canyon", "valley", "meadow", "island", "shadow", "breeze", "melody", "camera",
    "garden", "bamboo", "rabbit", "monkey", "falcon", "dolphin"
]

INDIAN_CITIES = [
    "Agra", "Ahmedabad", "Amritsar", "Bengaluru", "Bhopal", "Bhubaneswar",
    "Chandigarh", "Chennai", "Coimbatore", "Dehradun", "Delhi", "Faridabad",
    "Ghaziabad", "Guwahati", "Gwalior", "Hyderabad", "Indore", "Jaipur",
    "Jalandhar", "Jamshedpur", "Jodhpur", "Kanpur", "Kochi", "Kolkata",
    "Kota", "Lucknow", "Ludhiana", "Madurai", "Mangaluru", "Meerut",
    "Mumbai", "Mysuru", "Nagpur", "Nashik", "Noida", "Panaji",
    "Patna", "Puducherry", "Pune", "Raipur", "Rajkot", "Ranchi",
    "Shimla", "Srinagar", "Surat", "Thane", "Thiruvananthapuram", "Udaipur",
    "Varanasi", "Vijayawada", "Visakhapatnam"
]

ANIMALS = [
    "Bear", "Bison", "Camel", "Cheetah", "Cobra", "Deer",
    "Dolphin", "Eagle", "Elephant", "Falcon", "Fox", "Gaur",
    "Hawk", "Jackal", "Jaguar", "Leopard", "Lion", "Lynx",
    "Mongoose", "Monkey", "Otter", "Owl", "Panda", "Panther",
    "Peacock", "Python", "Raven", "Rhino", "Swan", "Tiger",
    "Wolf"
]

def load_words(min_len=4, max_len=6):
    dict_paths = [
        "/usr/share/dict/words",
        "/usr/dict/words",
        "/etc/dictionaries-common/words"
    ]
    words = []
    for path in dict_paths:
        if os.path.exists(path):
            try:
                with open(path, "r", encoding="utf-8", errors="ignore") as f:
                    for line in f:
                        w = line.strip()
                        if w.isalpha() and min_len <= len(w) <= max_len:
                            words.append(w.lower())
                if words:
                    break
            except Exception:
                pass
    
    if not words:
        # Fallback list filter
        words = [w for w in FALLBACK_WORDS if min_len <= len(w) <= max_len]
        if not words:
            words = FALLBACK_WORDS  # If custom bounds are completely outside, use the full fallback list
            
    return sorted(list(set(words)))

def generate_password(words_list, num_words, separator, digits_count, case):
    # Select random words
    chosen = [secrets.choice(words_list) for _ in range(num_words)]
    
    # Apply casing
    if case == "title":
        chosen = [w.capitalize() for w in chosen]
    elif case == "upper":
        chosen = [w.upper() for w in chosen]
    elif case == "camel":
        chosen = [w.capitalize() for w in chosen]
        separator = ""  # camelCase/PascalCase generally has no separator
    elif case == "lower":
        chosen = [w.lower() for w in chosen]
        
    pwd_base = separator.join(chosen)
    
    # Generate random digits
    if digits_count > 0:
        # E.g. for 4 digits, range is 1000 to 9999
        low = 10 ** (digits_count - 1)
        high = (10 ** digits_count) - 1
        rand_num = secrets.randbelow(high - low + 1) + low
        # Append digit with separator or directly for camel
        if case == "camel" or separator == "":
            return f"{pwd_base}{rand_num}"
        else:
            return f"{pwd_base}{separator}{rand_num}"
    
    return pwd_base

def generate_indian_password(separator="-", digits_count=4, case="title"):
    city = secrets.choice(INDIAN_CITIES)
    animal = secrets.choice(ANIMALS)
    
    if case == "title":
        city = city.capitalize()
        animal = animal.capitalize()
    elif case == "upper":
        city = city.upper()
        animal = animal.upper()
    elif case == "lower":
        city = city.lower()
        animal = animal.lower()
    elif case == "camel":
        city = city[0].lower() + city[1:] if len(city) > 0 else city
        animal = animal.capitalize()
        separator = ""
        
    if digits_count > 0:
        low = 10 ** (digits_count - 1)
        high = (10 ** digits_count) - 1
        rand_num = secrets.randbelow(high - low + 1) + low
        num_str = str(rand_num)
    else:
        num_str = ""
        
    special_char = secrets.choice(['@', '_', '*'])
    
    if num_str:
        return f"{city}{separator}{animal}{special_char}{num_str}"
    else:
        return f"{city}{separator}{animal}{special_char}"

def main():
    parser = argparse.ArgumentParser(
        description="Generate robust, human-readable random passwords using dictionary words or Indian city/animal combinations."
    )
    parser.add_argument("-w", "--words", type=int, default=3,
                        help="Number of words in the password (default: 3)")
    parser.add_argument("-n", "--count", type=int, default=1,
                        help="Number of passwords to generate (default: 1)")
    parser.add_argument("-s", "--separator", type=str, default="-",
                        help="Separator between words (default: '-')")
    parser.add_argument("-d", "--digits", type=int, default=4,
                        help="Number of random digits to append (default: 4, set to 0 for none)")
    parser.add_argument("--min", type=int, default=4,
                        help="Minimum word length (default: 4)")
    parser.add_argument("--max", type=int, default=6,
                        help="Maximum word length (default: 6)")
    parser.add_argument("-c", "--case", choices=["lower", "upper", "title", "camel"], default=None,
                        help="Casing style: lower, upper, title, camel (default: lower for words, title for indian)")
    parser.add_argument("-i", "--indian", action="store_true",
                        help="Generate password using Indian cities, animals, single special char (@, _, *), and trailing 4-digit number")
    parser.add_argument("-m", "--mode", choices=["words", "indian"], default="words",
                        help="Password generation mode: words or indian (default: words)")
    
    args = parser.parse_args()

    mode = "indian" if (args.indian or args.mode == "indian") else "words"
    case = args.case if args.case is not None else ("title" if mode == "indian" else "lower")

    if args.count <= 0:
        print("Error: Count must be at least 1.", file=sys.stderr)
        sys.exit(1)
        
    if mode == "indian":
        for _ in range(args.count):
            print(generate_indian_password(separator=args.separator, digits_count=args.digits, case=case))
    else:
        if args.words <= 0:
            print("Error: Number of words must be at least 1.", file=sys.stderr)
            sys.exit(1)
            
        if args.min > args.max:
            print("Error: Minimum word length cannot be greater than maximum word length.", file=sys.stderr)
            sys.exit(1)
            
        words_list = load_words(args.min, args.max)
        
        for _ in range(args.count):
            print(generate_password(words_list, args.words, args.separator, args.digits, case))

if __name__ == "__main__":
    main()

