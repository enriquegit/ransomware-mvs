# File that contains global variables.

DATASET_NAME = "radar-multiclass" # Directory name where the data.csv file is located.

ITERATIONS = 30

PCT_TRAIN = 0.7

DATA_PATH = "data/" # Root path where all datasets reside.

DATASET_PATH = DATA_PATH + DATASET_NAME + "/"

FILE_PATH = DATA_PATH + DATASET_NAME + "/data.csv"

NUMCORES = 1 # Number of CPU cores to use depending on your machine.

NTREES = 30

################## Functions ################

import pandas as pd
from sklearn.metrics import accuracy_score, f1_score, recall_score, precision_score

def classification_metrics_row(it, method, predictions, ground_truth, df=None, decimals=3):
    """
    Compute overall and per-class accuracy, precision, F1-score, and recall.
    Optionally append results to an existing DataFrame, automatically handling
    new classes that may appear in later iterations.
    
    Parameters:
        it (int): Iteration or run ID
        method (str): Method/model name
        predictions (list or array): Model predictions
        ground_truth (list or array): True labels
        df (pd.DataFrame, optional): Existing results DataFrame (default: None)
        decimals (int): Number of decimal places to round the metrics (default: 4)
    
    Returns:
        pd.DataFrame: Updated DataFrame with consistent columns and one new row added.
    """
    
    preds = pd.Series(predictions)
    truths = pd.Series(ground_truth)
    
    # Detect all classes in this run
    current_classes = sorted(list(set(truths.unique()) | set(preds.unique())))
    
    # If a previous DataFrame exists, merge all seen classes
    if df is not None:
        existing_classes = sorted({
            col.split('_')[0]
            for col in df.columns
            if '_' in col and not col.startswith("overall")
        })
        all_classes = sorted(set(existing_classes) | set(current_classes))
    else:
        all_classes = current_classes
    
    # Initialize results
    result = {"it": it, "method": method}
    
    # Per-class metrics
    for cls in all_classes:
        y_true_bin = (truths == cls).astype(int)
        y_pred_bin = (preds == cls).astype(int)
        
        result[f"{cls}_accuracy"]  = accuracy_score(y_true_bin, y_pred_bin)
        result[f"{cls}_precision"] = precision_score(y_true_bin, y_pred_bin, zero_division=0)
        result[f"{cls}_f1"]        = f1_score(y_true_bin, y_pred_bin, zero_division=0)
        result[f"{cls}_recall"]    = recall_score(y_true_bin, y_pred_bin, zero_division=0)
    
    # Overall (macro) metrics
    result["overall_accuracy"]  = accuracy_score(truths, preds)
    result["overall_precision"] = precision_score(truths, preds, average='macro', zero_division=0)
    result["overall_f1"]        = f1_score(truths, preds, average='macro', zero_division=0)
    result["overall_recall"]    = recall_score(truths, preds, average='macro', zero_division=0)
    
    # Convert to DataFrame
    new_row = pd.DataFrame([result])
    
    # Order columns
    ordered_cols = (
        ["it", "method"] +
        ["overall_accuracy", "overall_precision", "overall_f1", "overall_recall"] +
        [f"{cls}_{metric}" for cls in all_classes for metric in ["accuracy", "precision", "f1", "recall"]]        
    )
    
    # Combine with existing DataFrame
    if df is not None:
        df = pd.concat([df, new_row], ignore_index=True)
        df = df.reindex(columns=ordered_cols, fill_value=None)
    else:
        df = new_row[ordered_cols]
    
    # Round numeric columns
    numeric_cols = df.select_dtypes(include="number").columns
    df[numeric_cols] = df[numeric_cols].round(decimals)
    
    return df

