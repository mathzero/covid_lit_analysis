# bertopic_modeling.py

import pandas as pd
from bertopic import BERTopic
from sentence_transformers import SentenceTransformer
from nltk.corpus import stopwords
from nltk.stem import WordNetLemmatizer
import nltk
import torch
import os
import argparse
import numpy as np

# Step 0: Download necessary NLTK resources
nltk.download('stopwords')
nltk.download('wordnet')
nltk.download('omw-1.4')  # Optional: For extended WordNet data

def load_cleaned_data(file_path):
    """
    Loads the cleaned and filtered data from a CSV file.
    """
    if not os.path.exists(file_path):
        print(f"Cleaned data file '{file_path}' does not exist.")
        return None
    df = pd.read_csv(file_path)
    print(f"Loaded cleaned data from '{file_path}'.")
    print(f"DataFrame shape: {df.shape}")
    print("DataFrame Head:")
    print(df.head())
    return df

def sample_data(df, sample_size=100, random_state=42):
    """
    Samples a subset of the DataFrame for testing.
    """
    if sample_size == -1:
        print("Using all documents for BERTopic modeling.")
        return df.copy()
    if sample_size > len(df):
        print(f"Sample size {sample_size} is greater than the number of documents {len(df)}. Using all documents.")
        return df.copy()
    sampled_df = df.sample(n=sample_size, random_state=random_state)
    print(f"Sampled {sample_size} rows from the DataFrame.")
    print(sampled_df.head())
    return sampled_df

def prepare_documents(df, cleaned=False):
    """
    Prepares the documents list for BERTopic.
    If cleaned=True, uses the cleaned abstracts.
    """
    if cleaned:
        # Ensure that 'abstract_cleaned' exists
        if 'abstract_cleaned' not in df.columns:
            raise KeyError("'abstract_cleaned' column not found in DataFrame.")
        documents = df['abstract_cleaned'].tolist()
    else:
        documents = df['abstract_preferred'].tolist()
    return documents

def run_bertopic(documents, use_cleaned=False, nr_topics=None, device="cpu"):
    """
    Runs BERTopic on the provided documents and returns the model, topics, and probabilities.
    """
    # Initialize SentenceTransformer with GPU
    print(f"Initializing SentenceTransformer on device: {device}...")
    embedding_model = SentenceTransformer("all-MiniLM-L6-v2", device=device)
    
    # Verify embedding model device
    print(f"Embedding model device: {embedding_model.device}")
    
    # Initialize BERTopic with the embedding model and calculate_probabilities=True
    print("Initializing BERTopic...")
    topic_model = BERTopic(embedding_model=embedding_model, nr_topics=nr_topics,
                           verbose=True, 
                           calculate_probabilities=True)
    
    # Fit and transform the documents
    print("Fitting BERTopic model...")
    topics, probabilities = topic_model.fit_transform(documents)
    
    print("BERTopic model fitted successfully.")
    return topic_model, topics, probabilities

def save_model(topic_model, model_path):
    """
    Saves the BERTopic model to the specified path.
    """
    topic_model.save(model_path)
    print(f"BERTopic model saved to '{model_path}'.")

def save_topics(topic_model, topics_csv):
    """
    Saves the topics with their top words to a CSV file.
    """
    topics_dict = topic_model.get_topics()
    topics_df = pd.DataFrame([
        {"Topic": topic_id, "Words": ", ".join([word for word, _ in words[:10]])}
        for topic_id, words in topics_dict.items()
    ])
    topics_df.to_csv(topics_csv, index=False)
    print(f"Topics saved to '{topics_csv}'.")

def save_probabilities(df, probabilities, output_path):
    """
    Saves the document-topic probabilities matrix to a CSV file.
    """
    print("Saving document-topic probabilities matrix...")
    
    # Convert probabilities to NumPy array if it's a list
    if isinstance(probabilities, list):
        probabilities = np.array(probabilities)
        print("Converted probabilities to NumPy array.")
    
    # Handle case where probabilities might be 1D
    if probabilities.ndim == 1:
        probabilities = probabilities.reshape(-1, 1)
        print("Reshaped probabilities to 2D array with one column.")
    
    # Create DataFrame for probabilities
    prob_df = pd.DataFrame(probabilities, columns=[f"Topic_{i}" for i in range(probabilities.shape[1])])
    
    # Ensure 'id' column exists
    if 'id' not in df.columns:
        # If 'id' column doesn't exist, use the index as identifier
        prob_df.insert(0, 'id', df.index)
        print("'id' column not found in DataFrame. Using DataFrame index as document ID.")
    else:
        prob_df.insert(0, 'id', df['id'].values)
    
    # Save to CSV
    prob_df.to_csv(output_path, index=False)
    print(f"Document-topic probabilities matrix saved to '{output_path}'.")

def save_annotated_df(df, topics, output_csv):
    """
    Saves the DataFrame annotated with topics to a CSV file.
    """
    print("Saving annotated DataFrame with topics...")
    df['topic'] = topics
    df.to_csv(output_csv, index=False)
    print(f"Annotated DataFrame saved to '{output_csv}'.")

def main():
    # Argument parser for command-line options
    parser = argparse.ArgumentParser(description="Run BERTopic modeling on cleaned data.")
    parser.add_argument('--input_csv', type=str, required=True, help='Path to the cleaned CSV data.')
    parser.add_argument('--sample_size', type=int, default=100, help='Number of documents to sample. Use -1 to use all.')
    parser.add_argument('--use_cleaned', action='store_true', help='Use the cleaned abstracts for modeling.')
    parser.add_argument('--model_path', type=str, default='bertopic_model', help='Path to save the BERTopic model.')
    parser.add_argument('--topics_csv', type=str, default='bertopic_topics.csv', help='Path to save the topics information.')
    parser.add_argument('--probabilities_csv', type=str, default='document_topic_matrix.csv', help='Path to save the document-topic probabilities matrix CSV.')
    parser.add_argument('--annotated_csv', type=str, default='abstracts_with_topics.csv', help='Path to save the annotated CSV data with topics.')
    
    args = parser.parse_args()
    
    input_csv = args.input_csv
    sample_size = args.sample_size
    use_cleaned = args.use_cleaned
    model_path = args.model_path
    topics_csv = args.topics_csv
    probabilities_csv = args.probabilities_csv
    annotated_csv = args.annotated_csv
    
    # Step 1: Load the cleaned and filtered data
    df = load_cleaned_data(input_csv)
    if df is None:
        return
    
    # Step 2: Sample a subset if needed
    sampled_df = sample_data(df, sample_size=sample_size, random_state=42)
    
    # Step 3: Prepare the documents for BERTopic
    try:
        documents = prepare_documents(sampled_df, cleaned=use_cleaned)
    except KeyError as e:
        print(f"Error: {e}")
        return
    
    # Step 4: Check if GPU is available and set device accordingly
    if torch.backends.mps.is_available():
        device = "mps"
        print("MPS (GPU) is available. Using GPU for embeddings.")
    elif torch.cuda.is_available():
        device = "cuda"
        print("CUDA (GPU) is available. Using GPU for embeddings.")
    else:
        device = "cpu"
        print("GPU not available. Using CPU for embeddings.")
    
    # Step 5: Run BERTopic on the sample
    topic_model, topics, probabilities = run_bertopic(documents, use_cleaned=use_cleaned, nr_topics=None, device=device)
    
    # Step 6: Save the BERTopic model
    save_model(topic_model, model_path)
    
    # Step 7: Save topics information
    save_topics(topic_model, topics_csv)
    
    # Step 8: Save document-topic probabilities
    save_probabilities(sampled_df, probabilities, probabilities_csv)
    
    # Step 9: Save annotated DataFrame with topics
    save_annotated_df(sampled_df, topics, annotated_csv)
    
    print("BERTopic modeling completed successfully.")

if __name__ == "__main__":
    main()