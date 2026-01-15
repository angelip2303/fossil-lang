use tower_lsp::{LspService, Server};

mod compiler_bridge;
mod document;
mod handlers;
mod server;

#[tokio::main]
async fn main() {
    // Initialize tracing for logging
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::INFO.into()),
        )
        .init();

    tracing::info!("Starting Fossil LSP server");

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| server::FossilLanguageServer::new(client));

    Server::new(stdin, stdout, socket).serve(service).await;
}
