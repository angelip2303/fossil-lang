//! Connection pooling for SQL databases
//!
//! Uses a global registry of connection pools to reuse connections
//! across multiple queries and type provider invocations.

use std::collections::HashMap;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::sync::Arc;
use std::time::Duration;

use once_cell::sync::Lazy;
use sqlx::AnyPool;
use sqlx::any::AnyPoolOptions;
use tokio::sync::RwLock;

use super::error::SqlError;

/// Global registry of connection pools
static POOL_REGISTRY: Lazy<RwLock<HashMap<u64, Arc<ConnectionPool>>>> =
    Lazy::new(|| RwLock::new(HashMap::new()));

/// Connection pool wrapper
#[derive(Debug)]
pub struct ConnectionPool {
    pub pool: AnyPool,
    pub connection_string: String,
}

impl ConnectionPool {
    /// Get or create a connection pool for the given connection string
    pub async fn get_or_create(
        connection_string: &str,
        timeout_secs: u64,
    ) -> Result<Arc<ConnectionPool>, SqlError> {
        let key = hash_connection_string(connection_string);

        // Check if pool exists
        {
            let registry = POOL_REGISTRY.read().await;
            if let Some(pool) = registry.get(&key) {
                return Ok(Arc::clone(pool));
            }
        }

        // Create new pool
        let pool = create_pool(connection_string, timeout_secs).await?;
        let pool = Arc::new(pool);

        // Store in registry
        {
            let mut registry = POOL_REGISTRY.write().await;
            registry.insert(key, Arc::clone(&pool));
        }

        Ok(pool)
    }

    /// Get an existing pool without creating a new one
    pub async fn get(connection_string: &str) -> Option<Arc<ConnectionPool>> {
        let key = hash_connection_string(connection_string);
        let registry = POOL_REGISTRY.read().await;
        registry.get(&key).cloned()
    }

    /// Close all pools in the registry
    pub async fn close_all() {
        let mut registry = POOL_REGISTRY.write().await;
        for (_, pool) in registry.drain() {
            pool.pool.close().await;
        }
    }
}

/// Create a new connection pool
async fn create_pool(
    connection_string: &str,
    timeout_secs: u64,
) -> Result<ConnectionPool, SqlError> {
    // Install any-driver support
    sqlx::any::install_default_drivers();

    let pool = AnyPoolOptions::new()
        .max_connections(10)
        .min_connections(1)
        .acquire_timeout(Duration::from_secs(timeout_secs))
        .idle_timeout(Duration::from_secs(300)) // 5 minutes
        .connect(connection_string)
        .await
        .map_err(|e| SqlError::ConnectionFailed(e.to_string()))?;

    Ok(ConnectionPool {
        pool,
        connection_string: connection_string.to_string(),
    })
}

/// Hash connection string for use as registry key
fn hash_connection_string(s: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    s.hash(&mut hasher);
    hasher.finish()
}

/// Runtime for async operations
///
/// Creates or gets a tokio runtime for executing async database operations
/// from sync code paths.
pub fn get_runtime() -> &'static tokio::runtime::Runtime {
    static RUNTIME: Lazy<tokio::runtime::Runtime> = Lazy::new(|| {
        tokio::runtime::Builder::new_multi_thread()
            .worker_threads(2)
            .enable_all()
            .build()
            .expect("Failed to create tokio runtime")
    });
    &RUNTIME
}
