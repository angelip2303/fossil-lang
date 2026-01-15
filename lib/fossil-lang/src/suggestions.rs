//! Error suggestion utilities
//!
//! This module provides functionality for generating helpful error suggestions,
//! including edit distance calculation for "did you mean" suggestions.

/// Calculate Levenshtein edit distance between two strings
///
/// Returns the minimum number of single-character edits (insertions, deletions,
/// or substitutions) required to transform string `a` into string `b`.
///
/// # Examples
/// ```
/// use fossil_lang::suggestions::edit_distance;
///
/// assert_eq!(edit_distance("kitten", "sitting"), 3);
/// assert_eq!(edit_distance("lenght", "length"), 2); // Swap requires 2 ops
/// assert_eq!(edit_distance("hello", "hello"), 0);
/// ```
///
/// # Algorithm
/// Uses dynamic programming (Wagner-Fischer algorithm) with O(m*n) time complexity
/// and O(m*n) space complexity, where m and n are the lengths of the input strings.
pub fn edit_distance(a: &str, b: &str) -> usize {
    let len_a = a.len();
    let len_b = b.len();

    // Handle empty strings
    if len_a == 0 {
        return len_b;
    }
    if len_b == 0 {
        return len_a;
    }

    // Create matrix for dynamic programming
    let mut matrix = vec![vec![0; len_b + 1]; len_a + 1];

    // Initialize first column (transforming a[0..i] to empty string)
    for i in 0..=len_a {
        matrix[i][0] = i;
    }

    // Initialize first row (transforming empty string to b[0..j])
    for j in 0..=len_b {
        matrix[0][j] = j;
    }

    // Fill the matrix
    let a_chars: Vec<char> = a.chars().collect();
    let b_chars: Vec<char> = b.chars().collect();

    for i in 1..=len_a {
        for j in 1..=len_b {
            let cost = if a_chars[i - 1] == b_chars[j - 1] {
                0 // Characters match, no edit needed
            } else {
                1 // Characters differ, need substitution
            };

            matrix[i][j] = (matrix[i - 1][j] + 1) // Deletion
                .min(matrix[i][j - 1] + 1) // Insertion
                .min(matrix[i - 1][j - 1] + cost); // Substitution
        }
    }

    matrix[len_a][len_b]
}

/// Find the best matching identifier from a list of candidates
///
/// Searches through candidates to find the one with the smallest edit distance
/// from the target string. Only considers candidates within a reasonable
/// edit distance threshold (33% of target length).
///
/// # Arguments
/// * `target` - The misspelled or undefined identifier
/// * `candidates` - List of valid identifiers to compare against
///
/// # Returns
/// * `Some((suggestion, confidence))` - Best match with confidence score (0.0-1.0)
/// * `None` - No sufficiently similar match found
///
/// # Examples
/// ```
/// use fossil_lang::suggestions::find_similar;
///
/// let candidates = vec!["length".to_string(), "append".to_string()];
/// let (suggestion, confidence) = find_similar("lenght", &candidates).unwrap();
/// assert_eq!(suggestion, "length");
/// assert!(confidence > 0.5); // ~0.67 for edit distance 2 on 6 chars
/// ```
pub fn find_similar(target: &str, candidates: &[String]) -> Option<(String, f32)> {
    if candidates.is_empty() {
        return None;
    }

    // Maximum allowed edit distance (allow up to 33% edits)
    let max_distance = (target.len() / 3).max(1);

    let mut best_match = None;
    let mut best_distance = usize::MAX;

    for candidate in candidates {
        let distance = edit_distance(target, candidate);

        // Only consider candidates within reasonable distance
        if distance <= max_distance && distance < best_distance {
            best_distance = distance;
            best_match = Some(candidate.clone());
        }
    }

    // Calculate confidence score
    best_match.map(|suggestion| {
        let confidence = 1.0 - (best_distance as f32 / target.len().max(1) as f32);
        (suggestion, confidence.clamp(0.0, 1.0))
    })
}

/// Find all similar identifiers within a threshold
///
/// Returns all candidates that are within the edit distance threshold,
/// sorted by similarity (most similar first).
///
/// # Arguments
/// * `target` - The misspelled or undefined identifier
/// * `candidates` - List of valid identifiers to compare against
/// * `max_results` - Maximum number of suggestions to return (default: 3)
///
/// # Returns
/// List of (suggestion, confidence) tuples, sorted by confidence descending
pub fn find_all_similar(
    target: &str,
    candidates: &[String],
    max_results: usize,
) -> Vec<(String, f32)> {
    if candidates.is_empty() {
        return Vec::new();
    }

    let max_distance = (target.len() / 3).max(1);

    let mut matches: Vec<(String, usize)> = candidates
        .iter()
        .map(|candidate| {
            let distance = edit_distance(target, candidate);
            (candidate.clone(), distance)
        })
        .filter(|(_, distance)| *distance <= max_distance)
        .collect();

    // Sort by distance (closest first)
    matches.sort_by_key(|(_, distance)| *distance);

    // Take top N and convert to confidence scores
    matches
        .into_iter()
        .take(max_results)
        .map(|(suggestion, distance)| {
            let confidence = 1.0 - (distance as f32 / target.len().max(1) as f32);
            (suggestion, confidence.clamp(0.0, 1.0))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_edit_distance_identical() {
        assert_eq!(edit_distance("hello", "hello"), 0);
        assert_eq!(edit_distance("", ""), 0);
    }

    #[test]
    fn test_edit_distance_empty() {
        assert_eq!(edit_distance("", "hello"), 5);
        assert_eq!(edit_distance("hello", ""), 5);
    }

    #[test]
    fn test_edit_distance_examples() {
        assert_eq!(edit_distance("kitten", "sitting"), 3);
        // "lenght" -> "length" requires swapping 'h' and 't', which is 2 operations
        assert_eq!(edit_distance("lenght", "length"), 2);
        assert_eq!(edit_distance("saturday", "sunday"), 3);
    }

    #[test]
    fn test_edit_distance_single_char() {
        assert_eq!(edit_distance("a", "b"), 1);
        assert_eq!(edit_distance("a", "a"), 0);
    }

    #[test]
    fn test_find_similar_exact_match() {
        let candidates = vec!["length".to_string(), "width".to_string()];
        let result = find_similar("length", &candidates);
        assert!(result.is_some());
        let (suggestion, confidence) = result.unwrap();
        assert_eq!(suggestion, "length");
        assert_eq!(confidence, 1.0);
    }

    #[test]
    fn test_find_similar_typo() {
        let candidates = vec!["length".to_string(), "width".to_string()];
        let result = find_similar("lenght", &candidates);
        assert!(result.is_some());
        let (suggestion, confidence) = result.unwrap();
        assert_eq!(suggestion, "length");
        // With edit distance 2 on 6 chars, confidence is ~0.67
        assert!(confidence > 0.5);
    }

    #[test]
    fn test_find_similar_no_match() {
        let candidates = vec!["length".to_string(), "width".to_string()];
        let result = find_similar("completely_different", &candidates);
        assert!(result.is_none());
    }

    #[test]
    fn test_find_similar_empty_candidates() {
        let candidates: Vec<String> = Vec::new();
        let result = find_similar("anything", &candidates);
        assert!(result.is_none());
    }

    #[test]
    fn test_find_all_similar() {
        let candidates = vec![
            "length".to_string(),
            "lengthen".to_string(),
            "width".to_string(),
        ];
        let results = find_all_similar("lenght", &candidates, 3);

        assert!(!results.is_empty());
        assert_eq!(results[0].0, "length"); // Best match
        // With edit distance 2 on 6 chars, confidence is ~0.67
        assert!(results[0].1 > 0.5);
    }

    #[test]
    fn test_find_all_similar_max_results() {
        let candidates = vec![
            "length".to_string(),
            "lengthen".to_string(),
            "lengthy".to_string(),
            "width".to_string(),
        ];
        let results = find_all_similar("lenght", &candidates, 2);

        // Should return at most 2 results
        assert!(results.len() <= 2);
    }
}
