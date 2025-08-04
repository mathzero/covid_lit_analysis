#!/bin/bash

# ------------------------------------------------------------------------------
# Script Name: run_bertopic.sh
# Description: Automates the BERTopic modeling and analysis workflow.
# Author: [Your Name]
# Date: [Date]
# ------------------------------------------------------------------------------

# Exit immediately if a command exits with a non-zero status.
set -e

# Define project directory
PROJECT_DIR="/Users/mw418/codebase/covid_lit_analysis_transfer/code/berttopic"

# Define virtual environment directory
VENV_DIR="$PROJECT_DIR/venv"

# Navigate to the project directory
echo "Navigating to the project directory: $PROJECT_DIR"
cd "$PROJECT_DIR"

# Activate the virtual environment
echo "Activating the virtual environment..."
source "$VENV_DIR/bin/activate"

# Define input and output files for modeling
INPUT_CSV="cleaned_data.csv"

# Since the model is saved at the top level, set model_path to the project directory
MODEL_PATH="$PROJECT_DIR"  # Saving the model directly in the project directory

# Define input files for analysis
ANALYSIS_MODEL_PATH="$MODEL_PATH"  # Model is saved at the top level
ANALYSIS_TOPICS_CSV="$TOPICS_CSV"
ANALYSIS_PROBABILITIES_CSV="$PROBABILITIES_CSV"
ANALYSIS_ANNOTATED_CSV="$ANNOTATED_CSV"

# Run the BERTopic Analysis script
echo "Running BERTopic Analysis..."
python bertopic_analysis.py \
    --model_path "$ANALYSIS_MODEL_PATH" \
    --topics_csv "$ANALYSIS_TOPICS_CSV" \
    --probabilities_csv "$ANALYSIS_PROBABILITIES_CSV" \
    --annotated_csv "$ANALYSIS_ANNOTATED_CSV" \
    --output_dir "$OUTPUT_DIR"

# Deactivate the virtual environment
echo "Deactivating the virtual environment..."
deactivate

echo "BERTopic modeling and analysis completed successfully."