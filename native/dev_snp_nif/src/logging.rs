use std::thread;
use std::time::SystemTime;

/// Logs messages with details including thread ID, timestamp, file, and line number.
///
/// # Arguments
/// - `log_level`: The log level (e.g., "INFO", "ERROR").
/// - `file`: The file where the log is being generated.
/// - `line`: The line number of the log statement.
/// - `message`: The log message.
///
/// # Example
/// ```rust
/// log_message("INFO", file!(), line!(), "This is a log message.");
/// ```
pub fn log_message(log_level: &str, file: &str, line: u32, message: &str) {
    let thread_id = thread::current().id();
    let now = SystemTime::now();
    let timestamp = now
        .duration_since(SystemTime::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);

    println!(
        "[{}#{:?} @ {}:{}] [{}] {}",
        log_level, thread_id, file, line, timestamp, message
    );
}
