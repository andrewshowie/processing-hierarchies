import torch
import torch.nn.functional as F
from transformers import AutoModel, AutoTokenizer
from sentence_transformers import SentenceTransformer
from torch.utils.data import DataLoader, Dataset
import pandas as pd
import numpy as np
import os

# Load KR-SBERT model and tokenizer
model_name = 'snunlp/KR-SBERT-V40K-klueNLI-augSTS'
model = SentenceTransformer(model_name)
tokenizer = AutoTokenizer.from_pretrained(model_name)

# Function to compute cosine similarity
def compute_cosine_similarity(original_sentence, recalled_sentence):
    # Encode sentences using KR-SBERT
    embedding1 = model.encode(original_sentence, convert_to_tensor=True, device='cpu')
    embedding2 = model.encode(recalled_sentence, convert_to_tensor=True, device='cpu')

    # Compute cosine similarity
    similarity = F.cosine_similarity(embedding1, embedding2, dim=0).item()
    return similarity

# Load your new data from Excel
current_directory = os.path.dirname(os.path.abspath(__file__))
input_file_path = os.path.join(current_directory, 'input.xlsx')
input_data = pd.read_excel(input_file_path)

# Initialize output DataFrame efficiently
similarity_scores_output = pd.DataFrame(
    index=range(len(input_data)),
    columns=input_data.columns[1:],  # All columns except A
    data=np.nan  # Pre-fill with NaN values
)

# Process the data in a pairwise manner for each row
print("Processing sentences...")
for index, row in input_data.iterrows():
    original_sentence = row.iloc[0]  # The sentence in column A
    
    # Process each comparison sentence
    for col_idx, col in enumerate(input_data.columns[1:]):
        comparison_sentence = row[col]
        if not pd.isna(comparison_sentence):
            # Compute similarity
            score = compute_cosine_similarity(original_sentence, comparison_sentence)
            similarity_scores_output.iloc[index, col_idx] = score

    if (index + 1) % 5 == 0:  # Progress update every 5 rows
        print(f"Processed {index + 1}/{len(input_data)} rows")

# Save results to a new Excel file
output_file_path = os.path.join(current_directory, 'kr_sbert_semantic_similarity.xlsx')
similarity_scores_output.to_excel(output_file_path, index=False)
print("Processing complete. Results saved to:", output_file_path)
