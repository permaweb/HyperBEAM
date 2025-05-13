use tracing_subscriber::{fmt, prelude::*, EnvFilter};

pub fn init_tracing() {
    let fmt_layer = fmt::layer()
        .with_target(true)             // show module path
        .with_timer(fmt::time::SystemTime) // milliseconds & ISO-8601
        .with_thread_ids(true);

    tracing_subscriber::registry()
        .with(fmt_layer)
        .with(EnvFilter::from_default_env()) // honours RUST_LOG
        .init();                             // *must* run only once
}
