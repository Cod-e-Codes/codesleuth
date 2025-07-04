package main

import (
	"testing"
	"time"
)

func TestBenchmarkMode(t *testing.T) {
	files := []string{"file1.cob", "file2.cob", "file3.cob"}
	var processed []string
	worker := func(f string) {
		processed = append(processed, f)
		time.Sleep(10 * time.Millisecond)
	}
	bm := RunBenchmark(files, worker)
	if bm.FilesProcessed != len(files) {
		t.Errorf("expected %d files processed, got %d", len(files), bm.FilesProcessed)
	}
	if bm.TotalTimeSec <= 0 {
		t.Errorf("expected nonzero total time, got %f", bm.TotalTimeSec)
	}
	if bm.Throughput <= 0 {
		t.Errorf("expected positive throughput, got %f", bm.Throughput)
	}
}
