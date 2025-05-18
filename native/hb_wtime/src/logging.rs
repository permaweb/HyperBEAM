use tracing::level_filters::LevelFilter;
use tracing_subscriber::{filter::Directive, fmt, prelude::*, EnvFilter};

pub fn init_tracing() {
    let fmt_layer = fmt::layer()
        .with_timer(fmt::time::SystemTime)
        .with_target(false)
        .with_file(true)
        .with_line_number(true);

    let filter_layer = EnvFilter::builder()
        .with_default_directive(Directive::from(LevelFilter::ERROR))
        .from_env_lossy();

    tracing_subscriber::registry()
        .with(fmt_layer)
        .with(filter_layer)
        .init();                             // *must* run only once
}
