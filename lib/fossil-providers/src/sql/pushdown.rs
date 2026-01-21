//! SQL Query Builder with pushdown optimization support
//!
//! Captures operations and translates them to SQL for efficient
//! database-side execution.

use std::fmt;

/// Sort direction
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SortOrder {
    Ascending,
    Descending,
}

impl fmt::Display for SortOrder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SortOrder::Ascending => write!(f, "ASC"),
            SortOrder::Descending => write!(f, "DESC"),
        }
    }
}

/// A filter condition for WHERE clauses
#[derive(Debug, Clone)]
pub enum FilterCondition {
    /// column = value
    Equals(String, SqlValue),
    /// column != value
    NotEquals(String, SqlValue),
    /// column > value
    GreaterThan(String, SqlValue),
    /// column >= value
    GreaterThanOrEqual(String, SqlValue),
    /// column < value
    LessThan(String, SqlValue),
    /// column <= value
    LessThanOrEqual(String, SqlValue),
    /// column LIKE pattern
    Like(String, String),
    /// column IS NULL
    IsNull(String),
    /// column IS NOT NULL
    IsNotNull(String),
    /// column IN (values)
    In(String, Vec<SqlValue>),
    /// AND of multiple conditions
    And(Vec<FilterCondition>),
    /// OR of multiple conditions
    Or(Vec<FilterCondition>),
    /// Raw SQL condition
    Raw(String),
}

/// SQL value for filter conditions
#[derive(Debug, Clone)]
pub enum SqlValue {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Null,
}

impl fmt::Display for SqlValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SqlValue::Int(n) => write!(f, "{}", n),
            SqlValue::Float(n) => write!(f, "{}", n),
            SqlValue::String(s) => write!(f, "'{}'", escape_sql_string(s)),
            SqlValue::Bool(b) => write!(f, "{}", if *b { "TRUE" } else { "FALSE" }),
            SqlValue::Null => write!(f, "NULL"),
        }
    }
}

impl fmt::Display for FilterCondition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FilterCondition::Equals(col, val) => write!(f, "{} = {}", col, val),
            FilterCondition::NotEquals(col, val) => write!(f, "{} != {}", col, val),
            FilterCondition::GreaterThan(col, val) => write!(f, "{} > {}", col, val),
            FilterCondition::GreaterThanOrEqual(col, val) => write!(f, "{} >= {}", col, val),
            FilterCondition::LessThan(col, val) => write!(f, "{} < {}", col, val),
            FilterCondition::LessThanOrEqual(col, val) => write!(f, "{} <= {}", col, val),
            FilterCondition::Like(col, pattern) => {
                write!(f, "{} LIKE '{}'", col, escape_sql_string(pattern))
            }
            FilterCondition::IsNull(col) => write!(f, "{} IS NULL", col),
            FilterCondition::IsNotNull(col) => write!(f, "{} IS NOT NULL", col),
            FilterCondition::In(col, values) => {
                let vals: Vec<String> = values.iter().map(|v| v.to_string()).collect();
                write!(f, "{} IN ({})", col, vals.join(", "))
            }
            FilterCondition::And(conditions) => {
                let parts: Vec<String> = conditions.iter().map(|c| format!("({})", c)).collect();
                write!(f, "{}", parts.join(" AND "))
            }
            FilterCondition::Or(conditions) => {
                let parts: Vec<String> = conditions.iter().map(|c| format!("({})", c)).collect();
                write!(f, "{}", parts.join(" OR "))
            }
            FilterCondition::Raw(sql) => write!(f, "{}", sql),
        }
    }
}

/// SQL Query Builder for pushdown optimization
///
/// Captures operations that can be pushed down to the database
/// and generates optimized SQL queries.
#[derive(Debug, Clone)]
pub struct SqlQueryBuilder {
    /// Base query or table name
    base_query: String,
    /// Columns to select (None = all)
    projections: Option<Vec<String>>,
    /// Filter conditions
    filters: Vec<FilterCondition>,
    /// Sort columns with order
    sorts: Vec<(String, SortOrder)>,
    /// LIMIT clause
    limit: Option<usize>,
    /// OFFSET clause
    offset: Option<usize>,
}

impl SqlQueryBuilder {
    /// Create a new query builder from a base query
    pub fn new(base_query: impl Into<String>) -> Self {
        Self {
            base_query: base_query.into(),
            projections: None,
            filters: Vec::new(),
            sorts: Vec::new(),
            limit: None,
            offset: None,
        }
    }

    /// Create from a table name
    pub fn from_table(table_name: impl Into<String>) -> Self {
        let table = table_name.into();
        Self::new(format!("SELECT * FROM {}", table))
    }

    /// Add column projections (SELECT)
    pub fn select(mut self, columns: Vec<String>) -> Self {
        self.projections = Some(columns);
        self
    }

    /// Add a filter condition (WHERE)
    pub fn filter(mut self, condition: FilterCondition) -> Self {
        self.filters.push(condition);
        self
    }

    /// Add sort order (ORDER BY)
    pub fn order_by(mut self, column: impl Into<String>, order: SortOrder) -> Self {
        self.sorts.push((column.into(), order));
        self
    }

    /// Set LIMIT
    pub fn limit(mut self, n: usize) -> Self {
        self.limit = Some(n);
        self
    }

    /// Set OFFSET
    pub fn offset(mut self, n: usize) -> Self {
        self.offset = Some(n);
        self
    }

    /// Build the final SQL query
    pub fn build(&self) -> String {
        let mut sql = String::new();

        // Handle projections
        if let Some(cols) = &self.projections {
            let cols_str = cols.join(", ");
            sql.push_str(&format!("SELECT {} FROM ({}) AS _base", cols_str, self.base_query));
        } else {
            sql.push_str(&format!("SELECT * FROM ({}) AS _base", self.base_query));
        }

        // Handle filters
        if !self.filters.is_empty() {
            let conditions: Vec<String> = self.filters.iter().map(|f| format!("({})", f)).collect();
            sql.push_str(&format!(" WHERE {}", conditions.join(" AND ")));
        }

        // Handle sorts
        if !self.sorts.is_empty() {
            let order_parts: Vec<String> = self
                .sorts
                .iter()
                .map(|(col, order)| format!("{} {}", col, order))
                .collect();
            sql.push_str(&format!(" ORDER BY {}", order_parts.join(", ")));
        }

        // Handle limit and offset
        if let Some(limit) = self.limit {
            sql.push_str(&format!(" LIMIT {}", limit));
        }

        if let Some(offset) = self.offset {
            sql.push_str(&format!(" OFFSET {}", offset));
        }

        sql
    }

    /// Check if any optimizations have been applied
    pub fn has_optimizations(&self) -> bool {
        self.projections.is_some()
            || !self.filters.is_empty()
            || !self.sorts.is_empty()
            || self.limit.is_some()
            || self.offset.is_some()
    }

    /// Get the base query without any optimizations
    pub fn base_query(&self) -> &str {
        &self.base_query
    }
}

/// Escape a string for SQL
fn escape_sql_string(s: &str) -> String {
    s.replace('\'', "''")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_query() {
        let query = SqlQueryBuilder::from_table("users").build();
        assert_eq!(query, "SELECT * FROM (SELECT * FROM users) AS _base");
    }

    #[test]
    fn test_query_with_projection() {
        let query = SqlQueryBuilder::from_table("users")
            .select(vec!["id".to_string(), "name".to_string()])
            .build();
        assert_eq!(
            query,
            "SELECT id, name FROM (SELECT * FROM users) AS _base"
        );
    }

    #[test]
    fn test_query_with_filter() {
        let query = SqlQueryBuilder::from_table("users")
            .filter(FilterCondition::Equals(
                "status".to_string(),
                SqlValue::String("active".to_string()),
            ))
            .build();
        assert_eq!(
            query,
            "SELECT * FROM (SELECT * FROM users) AS _base WHERE (status = 'active')"
        );
    }

    #[test]
    fn test_query_with_limit_offset() {
        let query = SqlQueryBuilder::from_table("users")
            .limit(10)
            .offset(20)
            .build();
        assert_eq!(
            query,
            "SELECT * FROM (SELECT * FROM users) AS _base LIMIT 10 OFFSET 20"
        );
    }

    #[test]
    fn test_complex_query() {
        let query = SqlQueryBuilder::from_table("orders")
            .select(vec!["id".to_string(), "amount".to_string()])
            .filter(FilterCondition::GreaterThan(
                "amount".to_string(),
                SqlValue::Float(100.0),
            ))
            .order_by("amount", SortOrder::Descending)
            .limit(10)
            .build();
        assert_eq!(
            query,
            "SELECT id, amount FROM (SELECT * FROM orders) AS _base WHERE (amount > 100) ORDER BY amount DESC LIMIT 10"
        );
    }
}
