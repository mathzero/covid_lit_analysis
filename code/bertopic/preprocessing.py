# preprocessing.py

import pyreadr
import pandas as pd
import re
import string
from nltk.corpus import stopwords, words
from nltk.stem import WordNetLemmatizer
import nltk
from bs4 import BeautifulSoup
from langdetect import detect, LangDetectException
import os
import argparse
from concurrent.futures import ProcessPoolExecutor, as_completed
from tqdm import tqdm

# Step 0: Download necessary NLTK resources
nltk.download('stopwords')
nltk.download('wordnet')
nltk.download('omw-1.4')  # Optional: For extended WordNet data
nltk.download('words')    # Download the words corpus

def read_rds_to_dataframe(file_path):
    """
    Reads an RDS file and extracts the DataFrame.
    """
    # Read the .rds file
    result = pyreadr.read_r(file_path)
    
    # Check the keys in the result
    print("Keys in RDS file:", result.keys())
    
    # Extract the DataFrame (assuming there's only one object)
    df = list(result.values())[0]
    
    # Verify the DataFrame
    print("DataFrame Head:")
    print(df.head())
    print(f"DataFrame shape: {df.shape}")
    
    return df

def preprocess_dataframe(df, num_workers=4):
    """
    Preprocesses the DataFrame by handling missing values, cleaning text,
    and filtering non-English documents using batch processing.
    
    Args:
        df (pd.DataFrame): The original DataFrame.
        num_workers (int): Number of parallel workers for language detection.
    
    Returns:
        pd.DataFrame: The cleaned and filtered DataFrame.
    """
    # Check for missing values in 'abstract_preferred'
    missing_values = df['abstract_preferred'].isnull().sum()
    print(f"Missing values in 'abstract_preferred': {missing_values}")
    
    # Drop rows with missing 'abstract_preferred'
    df = df.dropna(subset=['abstract_preferred'])
    print(f"DataFrame shape after dropping NaNs: {df.shape}")
    
    # Ensure 'abstract_preferred' is of string type
    df['abstract_preferred'] = df['abstract_preferred'].astype(str)
    
    # Initialize lemmatizer, stopwords, and English words
    lemmatizer = WordNetLemmatizer()
    stop_words = set(stopwords.words('english'))
    english_words = set(w.lower() for w in words.words())
    
    # Clean the text
    print("Cleaning the 'abstract_preferred' column...")
    df['abstract_cleaned'] = df['abstract_preferred'].apply(
        lambda x: clean_text(x, lemmatizer, stop_words, english_words)
    )
    
    # Exclude documents where the cleaned text is too short
    df['cleaned_length'] = df['abstract_cleaned'].apply(lambda x: len(x.split()))
    initial_count = df.shape[0]
    df = df[df['cleaned_length'] >= 20]
    final_count = df.shape[0]
    print(f"Filtered out {initial_count - final_count} documents with less than 20 words after cleaning.")
    
    # Drop the 'cleaned_length' column
    df = df.drop(columns=['cleaned_length'])
    
    # Filter out non-English documents using batch processing
    print("Detecting language and filtering out non-English documents...")
    texts = df['abstract_cleaned'].tolist()
    
    languages = batch_detect_language(texts, num_workers=num_workers)
    
    df['language'] = languages
    initial_count = df.shape[0]
    df = df[df['language'] == 'en'].copy()
    final_count = df.shape[0]
    print(f"Filtered out {initial_count - final_count} non-English documents.")
    
    # Drop the 'language' column if not needed
    df = df.drop(columns=['language'])
    
    return df

def clean_text(text, lemmatizer, stop_words, english_words):
    """
    Cleans the input text by removing URLs, HTML, lowercasing, removing punctuation, numbers,
    stopwords, non-English words, and lemmatizing.
    
    Args:
        text (str): The original text.
        lemmatizer (WordNetLemmatizer): Lemmatizer object.
        stop_words (set): Set of stopwords.
        english_words (set): Set of English words.
        
    Returns:
        str: The cleaned text.
    """
    # Remove URLs
    text = re.sub(r'http\S+|www\S+', '', text)
    
    # Remove HTML using BeautifulSoup
    soup = BeautifulSoup(text, "html.parser")
    text = soup.get_text(separator=" ")
    
    # Lowercase
    text = text.lower()
    # Remove punctuation
    text = text.translate(str.maketrans('', '', string.punctuation))
    # Remove numbers
    text = re.sub(r'\d+', '', text)
    # Remove extra whitespace
    text = re.sub(r'\s+', ' ', text).strip()
    # Tokenize
    words_in_text = text.split()
    # Remove stopwords, lemmatize, and filter non-English words
    tokens = [
        lemmatizer.lemmatize(word) for word in words_in_text
        if word not in stop_words and word in english_words
    ]
    text = ' '.join(tokens)
    return text

def detect_language(text):
    """
    Detects the language of the given text.
    Returns the ISO 639-1 language code.
    
    Args:
        text (str): The text to analyze.
    
    Returns:
        str: The detected language code (e.g., 'en', 'fr'), or 'unknown' if detection fails.
    """
    try:
        return detect(text)
    except LangDetectException:
        return 'unknown'

def batch_detect_language(texts, num_workers=4):
    """
    Detects the language of each text in the list using parallel processing.
    
    Args:
        texts (list of str): The list of texts to analyze.
        num_workers (int): Number of parallel workers.
    
    Returns:
        list of str: The detected language codes for each text.
    """
    languages = []
    with ProcessPoolExecutor(max_workers=num_workers) as executor:
        # Map returns results in the order of the input
        for lang in tqdm(executor.map(detect_language, texts), total=len(texts), desc="Language Detection"):
            languages.append(lang)
    
    return languages

def save_cleaned_data(df, output_path):
    """
    Saves the cleaned DataFrame to a CSV file.
    
    Args:
        df (pd.DataFrame): The cleaned DataFrame.
        output_path (str): The path to save the CSV file.
    """
    df.to_csv(output_path, index=False)
    print(f"Cleaned data saved to '{output_path}'.")

def main():
    # Argument parser for command-line options
    parser = argparse.ArgumentParser(description="Preprocess text data for BERTopic modeling.")
    parser.add_argument('--input_rds', type=str, required=True, help='Path to the input .rds file.')
    parser.add_argument('--output_csv', type=str, default='cleaned_data.csv', help='Path to save the cleaned CSV data.')
    parser.add_argument('--num_workers', type=int, default=4, help='Number of parallel workers for language detection.')
    
    args = parser.parse_args()
    
    input_rds = args.input_rds
    output_csv = args.output_csv
    num_workers = args.num_workers
    
    # Check if input file exists
    if not os.path.exists(input_rds):
        print(f"Input file '{input_rds}' does not exist.")
        return
    
    # Step 1: Read the .rds file and extract the DataFrame
    df = read_rds_to_dataframe(input_rds)
    
    # Step 2: Preprocess the DataFrame (includes cleaning and language filtering)
    df = preprocess_dataframe(df, num_workers=num_workers)
    
    # Optional: Ensure there is an 'id' column for later use
    if 'id' not in df.columns:
        df.reset_index(inplace=True)
        df.rename(columns={'index': 'id'}, inplace=True)
        print("'id' column not found. Created a new 'id' column based on DataFrame index.")
    
    # Step 3: Save the cleaned DataFrame
    save_cleaned_data(df, output_csv)

if __name__ == "__main__":
    main()