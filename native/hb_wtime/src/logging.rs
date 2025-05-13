use tracing_subscriber::{fmt, prelude::*, EnvFilter};

pub fn init_tracing() {
    let fmt_layer = fmt::layer()
        .with_timer(fmt::time::SystemTime)
        .with_target(false)
        .with_file(true)
        .with_line_number(true);

    tracing_subscriber::registry()
        .with(fmt_layer)
        .with(EnvFilter::from_default_env()) // honours RUST_LOG
        .init();                             // *must* run only once
}
