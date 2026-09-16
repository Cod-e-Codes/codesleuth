use chrono::{DateTime, Utc};
use serde::Serialize;
use std::time::Instant;

#[derive(Serialize)]
pub struct BenchmarkSummary {
    #[serde(rename = "Enabled")]
    pub enabled: bool,
    #[serde(rename = "FilesProcessed")]
    pub files_processed: usize,
    #[serde(rename = "TotalTimeSec")]
    pub total_time_sec: f64,
    #[serde(rename = "Throughput")]
    pub throughput: f64,
    #[serde(rename = "StartTime")]
    pub start_time: DateTime<Utc>,
    #[serde(rename = "EndTime")]
    pub end_time: DateTime<Utc>,
}

impl BenchmarkSummary {
    pub fn run<T>(files_processed: usize, work: impl FnOnce() -> T) -> (T, Self) {
        let start_time = Utc::now();
        let wall = Instant::now();
        let result = work();
        let elapsed = wall.elapsed().as_secs_f64();
        let end_time = Utc::now();
        let throughput = if elapsed > 0.0 {
            files_processed as f64 / elapsed
        } else {
            0.0
        };
        (
            result,
            BenchmarkSummary {
                enabled: true,
                files_processed,
                total_time_sec: elapsed,
                throughput,
                start_time,
                end_time,
            },
        )
    }

    pub fn print_table(&self) {
        println!("\nBenchmark Summary:");
        println!("Files Processed: {}", self.files_processed);
        println!("Total Time (s): {:.2}", self.total_time_sec);
        println!("Throughput (files/sec): {:.2}", self.throughput);
    }

    pub fn print_json(&self) {
        if let Ok(s) = serde_json::to_string_pretty(self) {
            println!("{}", s);
        }
    }
}
