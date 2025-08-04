import pyreadr
import pandas as pd
import re
import string
from bertopic import BERTopic
from sentence_transformers import SentenceTransformer
from nltk.corpus import stopwords
from nltk.stem import WordNetLemmatizer
import nltk
import torch
import random
import json
import os
from bs4 import BeautifulSoup  # Added for HTML cleaning
from langdetect import detect, LangDetectException  # Added for language detection
import matplotlib.pyplot as plt  # Added for additional plots
from wordcloud import WordCloud  # Added for word cloud visualization

# Step 0: Download necessary NLTK resources
nltk.download('stopwords')
nltk.download('wordnet')
nltk.download('omw-1.4')  # Optional: For extended WordNet data

def read_rds_to_dataframe(file_path):
    """
    Reads an RDS file and extracts the DataFrame.
    """
    # Read the .rds file
    result = pyreadr.read_r(file_path)
    
    # Check the keys in the result
    print("Keys in RDS file:", result.keys())
    
    # Extract the DataFrame (assuming there's only one object with key None)
    df = list(result.values())[0]
    
    # Verify the DataFrame
    print("DataFrame Head:")
    print(df.head())
    print(f"DataFrame shape: {df.shape}")
    
    return df

def preprocess_dataframe(df):
    """
    Preprocesses the DataFrame by handling missing values and ensuring correct data types.
    Also filters out non-English documents.
    """
    # Check for missing values in 'abstract_preferred'
    missing_values = df['abstract_preferred'].isnull().sum()
    print(f"Missing values in 'abstract_preferred': {missing_values}")
    
    # Drop rows with missing 'abstract_preferred'
    df = df.dropna(subset=['abstract_preferred'])
    print(f"DataFrame shape after dropping NaNs: {df.shape}")
    
    # Ensure 'abstract_preferred' is of string type
    df['abstract_preferred'] = df['abstract_preferred'].astype(str)
    
    # Filter out non-English documents
    print("Detecting language and filtering out non-English documents...")
    df['language'] = df['abstract_preferred'].apply(detect_language)
    initial_count = df.shape[0]
    df = df[df['language'] == 'en'].copy()
    final_count = df.shape[0]
    print(f"Filtered out {initial_count - final_count} non-English documents.")
    
    # Optionally, drop the 'language' column if not needed
    df = df.drop(columns=['language'])
    
    return df

def detect_language(text):
    """
    Detects the language of the given text.
    Returns the ISO 639-1 language code.
    """
    try:
        return detect(text)
    except LangDetectException:
        return 'unknown'

def clean_text(text):
    """
    Cleans the input text by removing HTML, lowercasing, removing punctuation, numbers,
    stopwords, and lemmatizing.
    """
    # Initialize lemmatizer and stopwords
    lemmatizer = WordNetLemmatizer()
    stop_words = set(stopwords.words('english'))
    
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
    # Remove stopwords and lemmatize
    text = ' '.join([lemmatizer.lemmatize(word) for word in text.split() if word not in stop_words])
    return text

def sample_data(df, sample_size=100, random_state=42):
    """
    Samples a subset of the DataFrame for testing.
    """
    # Set random seed for reproducibility
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
    
    # Initialize BERTopic with the embedding model
    print("Initializing BERTopic...")
    topic_model = BERTopic(embedding_model=embedding_model, nr_topics=nr_topics)
    
    # Fit and transform the documents
    print("Fitting BERTopic model...")
    topics, probabilities = topic_model.fit_transform(documents)
    
    print("BERTopic model fitted successfully.")
    return topic_model, topics, probabilities

def save_results(df, topics, probabilities, topic_model, output_csv='abstracts_with_topics_sample.csv', 
                model_path='bertopic_model_sample', topics_csv='bertopic_topics.csv', 
                plots_dir='bertopic_plots', doc_topic_matrix_csv='document_topic_matrix.csv'):
    """
    Saves the topics and probabilities to the DataFrame and exports the results.
    Also saves the BERTopic model and topic details.
    Generates and saves interactive and additional plots.
    Outputs a document-topic matrix with probabilities.
    """
    # Ensure the plots directory exists
    if not os.path.exists(plots_dir):
        os.makedirs(plots_dir)
        print(f"Created directory '{plots_dir}' for saving plots.")
    
    # Assign topics and probabilities to the DataFrame
    df['topic'] = topics
    df['probability'] = probabilities
    
    # Get topic information
    topic_info = topic_model.get_topic_info()
    print("Topic Information:")
    print(topic_info)
    
    # Create a mapping from topic IDs to topic names
    topic_names = topic_info.set_index('Topic')['Name'].to_dict()
    
    # Map topic IDs to names
    df['topic_name'] = df['topic'].map(topic_names)
    
    # Handle outliers if needed (optional)
    # For example, you can label outliers or filter them out
    # Here, we'll keep them with the name "Outliers"
    df['topic_name'] = df['topic_name'].fillna('Outliers')
    
    # Optional: Convert probability lists to separate columns for the document-topic matrix
    print("Creating document-topic matrix...")
    prob_df = pd.DataFrame(probabilities, columns=[f"Topic_{i}" for i in range(probabilities.shape[1])])
    prob_df.insert(0, 'id', df['id'].values)  # Assuming 'id' column exists
    prob_df.to_csv(doc_topic_matrix_csv, index=False)
    print(f"Document-topic matrix saved to '{doc_topic_matrix_csv}'.")
    
    # Export the DataFrame to CSV
    df.to_csv(output_csv, index=False)
    print(f"Results saved to '{output_csv}'.")
    
    # Save the BERTopic model
    topic_model.save(model_path)
    print(f"BERTopic model saved to '{model_path}'.")
    
    # Save topics with top words to a separate CSV
    topics = topic_model.get_topics()
    topics_df = pd.DataFrame([
        {"Topic": topic_id, "Words": ", ".join([word for word, _ in words[:10]])}
        for topic_id, words in topics.items()
    ])
    
    topics_df.to_csv(topics_csv, index=False)
    print(f"Topics saved to '{topics_csv}'.")
    
    # Generate and save BERTopic visualizations
    try:
        # 1. Visualize Topics
        fig = topic_model.visualize_topics()
        plot_path = os.path.join(plots_dir, "visualize_topics.html")
        fig.write_html(plot_path)
        print(f"'visualize_topics' saved to '{plot_path}'.")
    except Exception as e:
        print(f"Failed to generate 'visualize_topics': {e}")
    
    try:
        # 2. Visualize Hierarchy
        fig = topic_model.visualize_hierarchy()
        plot_path = os.path.join(plots_dir, "visualize_hierarchy.html")
        fig.write_html(plot_path)
        print(f"'visualize_hierarchy' saved to '{plot_path}'.")
    except Exception as e:
        print(f"Failed to generate 'visualize_hierarchy': {e}")
    
    try:
        # 3. Visualize Heatmap
        fig = topic_model.visualize_heatmap()
        plot_path = os.path.join(plots_dir, "visualize_heatmap.html")
        fig.write_html(plot_path)
        print(f"'visualize_heatmap' saved to '{plot_path}'.")
    except Exception as e:
        print(f"Failed to generate 'visualize_heatmap': {e}")
    
    try:
        # 4. Visualize Barchart
        fig = topic_model.visualize_barchart()
        plot_path = os.path.join(plots_dir, "visualize_barchart.html")
        fig.write_html(plot_path)
        print(f"'visualize_barchart' saved to '{plot_path}'.")
    except Exception as e:
        print(f"Failed to generate 'visualize_barchart': {e}")
    
    try:
        # 5. Visualize Topic Similarity Heatmap
        print("Generating topic similarity heatmap...")
        similarity = topic_model.visualize_similarity()
        plot_path = os.path.join(plots_dir, "visualize_similarity.html")
        similarity.write_html(plot_path)
        print(f"'visualize_similarity' saved to '{plot_path}'.")
    except Exception as e:
        print(f"Failed to generate 'visualize_similarity': {e}")
    
    try:
        # 6. Generate Word Clouds for Each Topic
        print("Generating word clouds for each topic...")
        for topic in topic_model.get_topics().keys():
            if topic == -1:
                continue  # Skip outliers
            words = dict(topic_model.get_topic(topic))
            wordcloud = WordCloud(width=800, height=400, background_color='white').generate_from_frequencies(words)
            plt.figure(figsize=(10, 5))
            plt.imshow(wordcloud, interpolation='bilinear')
            plt.axis('off')
            plt.title(f"Word Cloud for Topic {topic}")
            wc_path = os.path.join(plots_dir, f"wordcloud_topic_{topic}.png")
            plt.savefig(wc_path)
            plt.close()
            print(f"Word cloud for Topic {topic} saved to '{wc_path}'.")
    except Exception as e:
        print(f"Failed to generate word clouds: {e}")
    
    try:
        # 7. Visualize Distribution for First 5 Documents
        print("Generating distribution plots for the first 5 documents...")
        for idx in range(min(5, len(probabilities))):
            fig = topic_model.visualize_distribution(probabilities[idx])
            plot_path = os.path.join(plots_dir, f"visualize_distribution_doc_{idx}.html")
            fig.write_html(plot_path)
            print(f"'visualize_distribution_doc_{idx}' saved to '{plot_path}'.")
    except Exception as e:
        print(f"Failed to generate 'visualize_distribution': {e}")
    
    print(f"All visualizations saved to the '{plots_dir}' directory.")

def main():
    # Define the path to your .rds file
    file_path = "/Users/mw418/codebase/covid_lit_analysis_transfer/data/bq_exports_mar_2023/clean/dat_main_abs.rds"
    
    # Step 1: Read the .rds file and extract the DataFrame
    df = read_rds_to_dataframe(file_path)
    
    # Step 2: Preprocess the DataFrame (includes filtering non-English documents)
    df = preprocess_dataframe(df)
    
    # Step 3: Clean the text
    print("Cleaning the 'abstract_preferred' column...")
    df['abstract_cleaned'] = df['abstract_preferred'].apply(clean_text)
    
    # Step 4: Sample a subset of 100 papers for testing
    sample_size = 100
    sampled_df = sample_data(df, sample_size=sample_size, random_state=42)
    
    # Step 5: Prepare the documents for BERTopic
    # Choose whether to use cleaned abstracts or original
    use_cleaned = True  # Set to True to use 'abstract_cleaned'
    documents = prepare_documents(sampled_df, cleaned=use_cleaned)
    
    # Step 6: Check if GPU is available and set device accordingly
    if torch.backends.mps.is_available():
        device = "mps"
        print("MPS (GPU) is available. Using GPU for embeddings.")
    elif torch.cuda.is_available():
        device = "cuda"
        print("CUDA (GPU) is available. Using GPU for embeddings.")
    else:
        device = "cpu"
        print("GPU not available. Using CPU for embeddings.")
    
    # Step 7: Run BERTopic on the sample
    topic_model, topics, probabilities = run_bertopic(documents, use_cleaned=use_cleaned, nr_topics=None, device=device)
    
    # Step 8: Save and explore the results, including visualizations
    save_results(sampled_df, topics, probabilities, topic_model,
                output_csv='abstracts_with_topics_sample.csv',
                model_path='bertopic_model_sample',
                topics_csv='bertopic_topics.csv',
                plots_dir='bertopic_plots',
                doc_topic_matrix_csv='document_topic_matrix.csv')
    
    # Optional: Further exploration
    print("Most Frequent Topics:")
    print(topic_model.get_topic_info())
    
    # Optional: Visualize topics interactively (uncomment if running in an interactive environment like Jupyter)
    """
    try:
        # Visualize Topics
        fig = topic_model.visualize_topics()
        fig.show()
        
        # Visualize Hierarchy
        fig = topic_model.visualize_hierarchy()
        fig.show()
        
        # Visualize Heatmap
        fig = topic_model.visualize_heatmap()
        fig.show()
        
        # Visualize Barchart
        fig = topic_model.visualize_barchart()
        fig.show()
        
        # Visualize Topic Distribution for a Specific Document
        doc_index = 0  # Change as needed
        fig = topic_model.visualize_distribution(probabilities[doc_index])
        fig.show()
        
    except Exception as e:
        print(f"Visualization error: {e}")
    """

if __name__ == "__main__":
    main()