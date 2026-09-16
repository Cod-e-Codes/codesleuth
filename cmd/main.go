package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"sync"

	"github.com/spf13/cobra"
)

var verbose bool
var debug bool
var workers int
var benchmark bool

func init() {
	analyzeCmd.Flags().BoolVar(&verbose, "verbose", false, "Extra progress on stderr")
	analyzeCmd.Flags().BoolVar(&debug, "debug", false, "Internal trace on stderr")
	analyzeCmd.Flags().IntVar(&workers, "workers", runtime.NumCPU(), "Concurrent workers (default: logical CPUs)")
	analyzeCmd.Flags().BoolVar(&benchmark, "benchmark", false, "Print wall-clock throughput after analysis")
}

func main() {
	rootCmd := &cobra.Command{
		Use:   "codesleuth",
		Short: "Analyze COBOL source files",
	}

	if err := AddConfigFlags(rootCmd); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	rootCmd.AddCommand(analyzeCmd)

	if err := rootCmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

func rustBackendName() string {
	if runtime.GOOS == "windows" {
		return "codesleuth.exe"
	}
	return "codesleuth"
}

func defaultRustBackend() (string, error) {
	exePath, err := os.Executable()
	if err != nil {
		return "", err
	}
	return filepath.Join(filepath.Dir(exePath), "..", "codesleuth", "target", "release", rustBackendName()), nil
}

func reportPath(outputDir, cobolPath string) string {
	rel := filepath.ToSlash(cobolPath)
	rel = strings.NewReplacer(":", "", "/", "_", "\\", "_").Replace(rel)
	ext := filepath.Ext(rel)
	rel = strings.TrimSuffix(rel, ext)
	return filepath.Join(outputDir, rel+".md")
}

func workerCount(nfiles int) int {
	n := workers
	if n < 1 {
		n = 1
	}
	if nfiles > 0 && n > nfiles {
		n = nfiles
	}
	return n
}

func runPool(files []string, nworkers int, worker func(string)) {
	fileCh := make(chan string, nworkers*2)
	var wg sync.WaitGroup
	wg.Add(nworkers)
	for i := 0; i < nworkers; i++ {
		go func() {
			defer wg.Done()
			for f := range fileCh {
				worker(f)
			}
		}()
	}
	for _, f := range files {
		fileCh <- f
	}
	close(fileCh)
	wg.Wait()
}

var analyzeCmd = &cobra.Command{
	Use:   "analyze [path]",
	Short: "Analyze COBOL files in a path",
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		config, err := LoadConfig()
		if err != nil {
			fmt.Fprintf(os.Stderr, "Config error: %v\n", err)
			os.Exit(1)
		}
		root := config.InputDir
		if root == "" {
			root = args[0]
		}
		var files []string
		err = filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}
			ext := strings.ToLower(filepath.Ext(path))
			if !info.IsDir() && (ext == ".cob" || ext == ".cbl" || ext == ".cobol") {
				files = append(files, path)
			}
			return nil
		})
		if err != nil {
			fmt.Printf("Error walking the path %q: %v\n", root, err)
			return
		}
		fmt.Printf("Found %d COBOL files:\n", len(files))
		for _, f := range files {
			fmt.Println(f)
		}

		if config.OutputDir != "" {
			if err := os.MkdirAll(config.OutputDir, 0755); err != nil {
				fmt.Fprintf(os.Stderr, "failed to create output directory %s: %v\n", config.OutputDir, err)
				os.Exit(1)
			}
		}

		workerFunc := func(f string) {
			rustBin := config.RustBackendPath
			if rustBin == "" {
				var resolveErr error
				rustBin, resolveErr = defaultRustBackend()
				if resolveErr != nil {
					fmt.Fprintf(os.Stderr, "failed to get executable path: %v\n", resolveErr)
					return
				}
			}
			if _, statErr := os.Stat(rustBin); statErr != nil {
				fmt.Fprintf(os.Stderr, "rust backend not found at %s: %v\n", rustBin, statErr)
				return
			}
			rustArgs := []string{rustBin, "analyze", f}
			if verbose {
				rustArgs = append(rustArgs, "--verbose")
			}
			if debug {
				rustArgs = append(rustArgs, "--debug")
			}
			if config.OutputDir != "" {
				rustArgs = append(rustArgs, "--output", reportPath(config.OutputDir, f))
			}
			rustCmd := exec.Command(rustArgs[0], rustArgs[1:]...)
			var rustStdout, rustStderr strings.Builder
			rustCmd.Stdout = &rustStdout
			rustCmd.Stderr = &rustStderr
			runErr := rustCmd.Run()
			if (verbose || debug) && rustStderr.Len() > 0 {
				fmt.Fprint(os.Stderr, rustStderr.String())
			}
			if runErr != nil {
				fmt.Fprintf(os.Stderr, "error running codesleuth analyze on %s: %v\n", f, runErr)
				return
			}
			if config.OutputDir != "" {
				fmt.Printf("Analyzed: %s -> %s\n", f, reportPath(config.OutputDir, f))
				return
			}
			fmt.Printf("Analyzed: %s\n%s", f, rustStdout.String())
		}

		nworkers := workerCount(len(files))
		if benchmark {
			bm := RunBenchmark(files, nworkers, workerFunc)
			bm.PrintSummaryTable()
			bm.PrintSummaryJSON()
			return
		}
		if len(files) == 0 {
			return
		}
		runPool(files, nworkers, workerFunc)
	},
}
