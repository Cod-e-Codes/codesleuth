package main

import (
	"encoding/json"
	"fmt"
	"os"
	"time"
)

type BenchmarkMode struct {
	Enabled        bool
	FilesProcessed int
	TotalTimeSec   float64
	Throughput     float64
	StartTime      time.Time
	EndTime        time.Time
}

func RunBenchmark(files []string, nworkers int, worker func(string)) BenchmarkMode {
	var bm BenchmarkMode
	bm.Enabled = true
	bm.StartTime = time.Now()
	if len(files) > 0 {
		runPool(files, nworkers, worker)
	}
	bm.EndTime = time.Now()
	bm.FilesProcessed = len(files)
	bm.TotalTimeSec = bm.EndTime.Sub(bm.StartTime).Seconds()
	if bm.TotalTimeSec > 0 {
		bm.Throughput = float64(bm.FilesProcessed) / bm.TotalTimeSec
	}
	return bm
}

func (bm BenchmarkMode) PrintSummaryJSON() {
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	_ = enc.Encode(bm)
}

func (bm BenchmarkMode) PrintSummaryTable() {
	fmt.Printf("\nBenchmark Summary:\n")
	fmt.Printf("Files Processed: %d\n", bm.FilesProcessed)
	fmt.Printf("Total Time (s): %.2f\n", bm.TotalTimeSec)
	fmt.Printf("Throughput (files/sec): %.2f\n", bm.Throughput)
}
