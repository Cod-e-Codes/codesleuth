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
	analyzeCmd.Flags().BoolVar(&verbose, "verbose", false, "Enable verbose output")
	analyzeCmd.Flags().BoolVar(&debug, "debug", false, "Enable debug output")
	analyzeCmd.Flags().IntVar(&workers, "workers", runtime.NumCPU(), "Number of concurrent workers (default: number of logical CPUs)")
	analyzeCmd.Flags().BoolVar(&benchmark, "benchmark", false, "Enable benchmarking mode (measure throughput, resource usage, etc.)")
}

func main() {
	rootCmd := &cobra.Command{
		Use:   "codesleuth",
		Short: "CodeSleuth is a multi-language code intelligence CLI tool",
	}

	AddConfigFlags(rootCmd)
	rootCmd.AddCommand(analyzeCmd)

	if err := rootCmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

var analyzeCmd = &cobra.Command{
	Use:   "analyze [path]",
	Short: "Analyze legacy code in the specified path",
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

		fileCh := make(chan string, workers*2)
		resultCh := make(chan string, workers)
		errCh := make(chan error, workers)
		var wg sync.WaitGroup

		workerFunc := func(f string) {
			rustBin := config.RustBackendPath
			if rustBin == "" {
				exePath, err := os.Executable()
				if err != nil {
					fmt.Fprintf(os.Stderr, "failed to get executable path: %v\n", err)
					return
				}
				exeDir := filepath.Dir(exePath)
				rustBin = filepath.Join(exeDir, "..", "codesleuth", "target", "release", "codesleuth.exe")
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
			rustCmd := exec.Command(rustArgs[0], rustArgs[1:]...)
			var rustStdout, rustStderr strings.Builder
			rustCmd.Stdout = &rustStdout
			rustCmd.Stderr = &rustStderr
			err := rustCmd.Run()
			if (verbose || debug) && rustStderr.Len() > 0 {
				fmt.Fprint(os.Stderr, rustStderr.String())
			}
			if err != nil {
				fmt.Fprintf(os.Stderr, "error running codesleuth analyze on %s: %v\n", f, err)
				return
			}
			fmt.Printf("Analyzed: %s\n%s", f, rustStdout.String())
		}

		if benchmark {
			bm := RunBenchmark(files, workerFunc)
			bm.PrintSummaryTable()
			bm.PrintSummaryJSON()
			return
		}

		wg.Add(workers)
		for i := 0; i < workers; i++ {
			go func() {
				defer wg.Done()
				for f := range fileCh {
					workerFunc(f)
				}
			}()
		}

		go func() {
			for _, f := range files {
				fileCh <- f
			}
			close(fileCh)
		}()

		for i := 0; i < len(files); i++ {
			select {
			case res := <-resultCh:
				fmt.Println(res)
			case err := <-errCh:
				fmt.Fprintln(os.Stderr, err)
			}
		}
		wg.Wait()
	},
}
