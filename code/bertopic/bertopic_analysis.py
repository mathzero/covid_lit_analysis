# bertopic_analysis.py

import pandas as pd
from bertopic import BERTopic
import matplotlib.pyplot as plt
from wordcloud import WordCloud
import os
import numpy as np
import plotly.io as pio

# Set default renderer for Plotly (Change if necessary)
pio.renderers.default = "browser"  # Options: 'browser', 'notebook', etc.

# -----------------------------
# Hardcoded Paths
# -----------------------------
BASE_DIR = "/Users/mw418/codebase/covid_lit_analysis_transfer/code/berttopic"

MODEL_PATH = os.path.join(BASE_DIR, "bertopic_model")
TOPICS_CSV = os.path.join(BASE_DIR, "bertopic_topics.csv")
PROBABILITIES_CSV = os.path.join(BASE_DIR, "document_topic_matrix.csv")
ANNOTATED_CSV = os.path.join(BASE_DIR, "abstracts_with_topics.csv")

OUTPUT_DIR = os.path.join(BASE_DIR, "analysis_results")
PLOTS_DIR = os.path.join(OUTPUT_DIR, "plots")
INTERACTIVE_PLOTS_DIR = os.path.join(OUTPUT_DIR, "interactive_plots")
WORDCLOUDS_DIR = os.path.join(OUTPUT_DIR, "wordclouds")

# -----------------------------
# Ensure Output Directories Exist
# -----------------------------
for directory in [OUTPUT_DIR, PLOTS_DIR, INTERACTIVE_PLOTS_DIR, WORDCLOUDS_DIR]:
    if not os.path.exists(directory):
        os.makedirs(directory)
        print(f"Created directory '{directory}'.")

# -----------------------------
# Function Definitions
# -----------------------------

def load_model(model_path):
    """
    Loads the BERTopic model from the specified path.
    """
    if not os.path.exists(model_path):
        print(f"BERTopic model directory '{model_path}' does not exist.")
        return None
    model = BERTopic.load(model_path)
    print(f"Loaded BERTopic model from '{model_path}'.")
    return model

def load_topics(topics_csv):
    """
    Loads the topics information from a CSV file.
    """
    if not os.path.exists(topics_csv):
        print(f"Topics CSV file '{topics_csv}' does not exist.")
        return None
    topics_df = pd.read_csv(topics_csv)
    print(f"Loaded topics information from '{topics_csv}'.")
    print(f"Topics DataFrame shape: {topics_df.shape}")
    print("Topics DataFrame Head:")
    print(topics_df.head())
    return topics_df

def load_probabilities(probabilities_csv):
    """
    Loads the document-topic probabilities matrix from a CSV file.
    """
    if not os.path.exists(probabilities_csv):
        print(f"Probabilities CSV file '{probabilities_csv}' does not exist.")
        return None
    prob_df = pd.read_csv(probabilities_csv)
    print(f"Loaded document-topic probabilities from '{probabilities_csv}'.")
    print(f"Probabilities DataFrame shape: {prob_df.shape}")
    print("Probabilities DataFrame Head:")
    print(prob_df.head())
    return prob_df

def load_annotated_df(annotated_csv):
    """
    Loads the annotated DataFrame with topics from a CSV file.
    """
    if not os.path.exists(annotated_csv):
        print(f"Annotated CSV file '{annotated_csv}' does not exist.")
        return None
    annotated_df = pd.read_csv(annotated_csv)
    print(f"Loaded annotated DataFrame from '{annotated_csv}'.")
    print(f"Annotated DataFrame shape: {annotated_df.shape}")
    print("Annotated DataFrame Head:")
    print(annotated_df.head())
    return annotated_df

def merge_probabilities(prob_df, annotated_df, id_column='id'):
    """
    Merges the probabilities DataFrame with the annotated DataFrame based on the identifier column.
    
    Args:
        prob_df (pd.DataFrame): Document-topic probabilities.
        annotated_df (pd.DataFrame): Annotated DataFrame with topics.
        id_column (str): The column name used as identifier.
        
    Returns:
        pd.DataFrame: Merged DataFrame.
    """
    merged_df = pd.merge(prob_df, annotated_df, on=id_column, how='left')
    print("Merged probabilities with annotated DataFrame.")
    print(f"Merged DataFrame shape: {merged_df.shape}")
    print("Merged DataFrame Head:")
    print(merged_df.head())
    return merged_df

def analyze_dominant_topics(merged_df, topic_column='topic', id_column='id'):
    """
    Identifies the dominant topic for each document based on the highest probability.
    
    Args:
        merged_df (pd.DataFrame): Merged DataFrame containing probabilities and topics.
        topic_column (str): The column name for assigned topics.
        id_column (str): The column name for document identifiers.
        
    Returns:
        pd.DataFrame: DataFrame with dominant topics.
    """
    # Extract probability columns
    prob_cols = [col for col in merged_df.columns if col.startswith('Topic_')]
    
    # Identify the topic with the highest probability
    merged_df['dominant_topic'] = merged_df[prob_cols].idxmax(axis=1).str.replace('Topic_', '').astype(int)
    
    # Compare with assigned topic
    merged_df['is_dominant'] = merged_df['dominant_topic'] == merged_df[topic_column]
    
    print("Identified dominant topics for each document.")
    print("Dominant Topics Head:")
    print(merged_df[['id', 'dominant_topic', topic_column, 'is_dominant']].head())
    return merged_df

def generate_wordclouds(model, output_dir=WORDCLOUDS_DIR):
    """
    Generates and saves word clouds for each topic.
    
    Args:
        model (BERTopic): The BERTopic model.
        output_dir (str): Directory to save word cloud images.
    """
    print("Generating word clouds for each topic...")
    for topic in model.get_topics().keys():
        if topic == -1:
            continue  # Skip outliers
        words = dict(model.get_topic(topic))
        if not words:
            continue  # Skip empty topics
        wordcloud = WordCloud(width=800, height=400, background_color='white').generate_from_frequencies(words)
        plt.figure(figsize=(10, 5))
        plt.imshow(wordcloud, interpolation='bilinear')
        plt.axis('off')
        plt.title(f"Word Cloud for Topic {topic}")
        wc_path = os.path.join(output_dir, f"wordcloud_topic_{topic}.png")
        plt.savefig(wc_path)
        plt.close()
        print(f"Word cloud for Topic {topic} saved to '{wc_path}'.")

def generate_topic_distribution(merged_df, prob_columns=None, output_dir=PLOTS_DIR):
    """
    Generates and saves a bar chart showing the distribution of topics.
    
    Args:
        merged_df (pd.DataFrame): Merged DataFrame containing probabilities and topics.
        prob_columns (list of str): List of topic probability columns.
        output_dir (str): Directory to save the plot.
    """
    if prob_columns is None:
        prob_columns = [col for col in merged_df.columns if col.startswith('Topic_')]
    
    print("Generating topic distribution bar chart...")
    topic_counts = merged_df['topic'].value_counts().sort_index()
    plt.figure(figsize=(12, 6))
    topic_counts.plot(kind='bar', color='skyblue')
    plt.xlabel('Topic')
    plt.ylabel('Number of Documents')
    plt.title('Distribution of Topics Across Documents')
    plt.xticks(rotation=0)
    plt.tight_layout()
    plot_path = os.path.join(output_dir, "topic_distribution.png")
    plt.savefig(plot_path)
    plt.close()
    print(f"Topic distribution bar chart saved to '{plot_path}'.")

def generate_interactive_visualizations(model, output_dir=INTERACTIVE_PLOTS_DIR):
    """
    Generates and saves interactive HTML visualizations provided by BERTopic.
    
    Args:
        model (BERTopic): The BERTopic model.
        output_dir (str): Directory to save interactive plots.
    """
    print("Generating interactive BERTopic visualizations...")
    try:
        # 1. Visualize Topics
        print("Generating 'visualize_topics' plot...")
        fig = model.visualize_topics()
        plot_path = os.path.join(output_dir, "visualize_topics.html")
        fig.write_html(plot_path)
        print(f"'visualize_topics' saved to '{plot_path}'.")
    except Exception as e:
        print(f"Failed to generate 'visualize_topics': {e}")
    
    try:
        # 2. Visualize Hierarchy
        print("Generating 'visualize_hierarchy' plot...")
        fig = model.visualize_hierarchy()
        plot_path = os.path.join(output_dir, "visualize_hierarchy.html")
        fig.write_html(plot_path)
        print(f"'visualize_hierarchy' saved to '{plot_path}'.")
    except Exception as e:
        print(f"Failed to generate 'visualize_hierarchy': {e}")
    
    try:
        # 3. Visualize Heatmap
        print("Generating 'visualize_heatmap' plot...")
        fig = model.visualize_heatmap()
        plot_path = os.path.join(output_dir, "visualize_heatmap.html")
        fig.write_html(plot_path)
        print(f"'visualize_heatmap' saved to '{plot_path}'.")
    except Exception as e:
        print(f"Failed to generate 'visualize_heatmap': {e}")
    
    try:
        # 4. Visualize Barchart
        print("Generating 'visualize_barchart' plot...")
        fig = model.visualize_barchart()
        plot_path = os.path.join(output_dir, "visualize_barchart.html")
        fig.write_html(plot_path)
        print(f"'visualize_barchart' saved to '{plot_path}'.")
    except Exception as e:
        print(f"Failed to generate 'visualize_barchart': {e}")
    
    try:
        # 5. Visualize Topic Similarity Heatmap
        print("Generating 'visualize_similarity' plot...")
        fig = model.visualize_similarity()
        plot_path = os.path.join(output_dir, "visualize_similarity.html")
        fig.write_html(plot_path)
        print(f"'visualize_similarity' saved to '{plot_path}'.")
    except Exception as e:
        print(f"Failed to generate 'visualize_similarity': {e}")
    
    try:
        # 6. Visualize Distribution for First 5 Documents
        print("Generating distribution plots for the first 5 documents...")
        for idx in range(min(5, len(model.probabilities_))):
            fig = model.visualize_distribution(model.probabilities_[idx])
            plot_path = os.path.join(output_dir, f"visualize_distribution_doc_{idx}.html")
            fig.write_html(plot_path)
            print(f"'visualize_distribution_doc_{idx}' saved to '{plot_path}'.")
    except Exception as e:
        print(f"Failed to generate 'visualize_distribution': {e}")
    
    print("Interactive visualizations generation completed.")

def main():
    # Step 1: Load the BERTopic model
    model = load_model(MODEL_PATH)
    if model is None:
        return
    
    # Step 2: Load the topics information
    topics_df = load_topics(TOPICS_CSV)
    if topics_df is None:
        return
    
    # Step 3: Load the document-topic probabilities
    prob_df = load_probabilities(PROBABILITIES_CSV)
    if prob_df is None:
        return
    
    # Step 4: Load the annotated DataFrame with topics
    annotated_df = load_annotated_df(ANNOTATED_CSV)
    if annotated_df is None:
        return
    
    # Step 5: Merge probabilities with annotated DataFrame
    merged_df = merge_probabilities(prob_df, annotated_df, id_column='id')
    
    # Step 6: Analyze dominant topics
    merged_df = analyze_dominant_topics(merged_df, topic_column='topic', id_column='id')
    
    # Step 7: Save the merged analysis
    merged_analysis_csv = os.path.join(OUTPUT_DIR, 'merged_analysis.csv')
    merged_df.to_csv(merged_analysis_csv, index=False)
    print(f"Merged analysis saved to '{merged_analysis_csv}'.")
    
    # Step 8: Generate Word Clouds
    generate_wordclouds(model, output_dir=WORDCLOUDS_DIR)
    
    # Step 9: Generate Topic Distribution Plot
    generate_topic_distribution(merged_df, output_dir=PLOTS_DIR)
    
    # Step 10: Generate Interactive Visualizations
    generate_interactive_visualizations(model, output_dir=INTERACTIVE_PLOTS_DIR)
    
    print("BERTopic analysis and visualization completed successfully.")

if __name__ == "__main__":
    main()