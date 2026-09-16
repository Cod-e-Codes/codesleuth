package main

import (
	"sync/atomic"
	"testing"
	"time"
)

func TestBenchmarkMode(t *testing.T) {
	files := []string{"file1.cob", "file2.cob", "file3.cob"}
	var processed atomic.Int32
	worker := func(f string) {
		processed.Add(1)
		time.Sleep(10 * time.Millisecond)
	}
	bm := RunBenchmark(files, 2, worker)
	if bm.FilesProcessed != len(files) {
		t.Errorf("expected %d files processed, got %d", len(files), bm.FilesProcessed)
	}
	if processed.Load() != int32(len(files)) {
		t.Errorf("expected worker to run %d times, got %d", len(files), processed.Load())
	}
	if bm.TotalTimeSec <= 0 {
		t.Errorf("expected nonzero total time, got %f", bm.TotalTimeSec)
	}
	if bm.Throughput <= 0 {
		t.Errorf("expected positive throughput, got %f", bm.Throughput)
	}
}
