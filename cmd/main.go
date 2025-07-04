package main

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/spf13/cobra"
)

var verbose bool
var debug bool

func init() {
	analyzeCmd.Flags().BoolVar(&verbose, "verbose", false, "Enable verbose output")
	analyzeCmd.Flags().BoolVar(&debug, "debug", false, "Enable debug output")
}

func main() {
	rootCmd := &cobra.Command{
		Use:   "codesleuth",
		Short: "CodeSleuth is a multi-language code intelligence CLI tool",
	}

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
		root := args[0]
		var files []string
		err := filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
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
			parserArgs := []string{"..\\parser\\target\\release\\parser.exe", f}
			if verbose {
				parserArgs = append(parserArgs, "--verbose")
			}
			if debug {
				parserArgs = append(parserArgs, "--debug")
			}
			parserCmd := exec.Command(parserArgs[0], parserArgs[1:]...)
			var parserStdout, parserStderr strings.Builder
			parserCmd.Stdout = &parserStdout
			parserCmd.Stderr = &parserStderr
			err := parserCmd.Run()
			if (verbose || debug) && parserStderr.Len() > 0 {
				fmt.Fprint(os.Stderr, parserStderr.String())
			}
			if err != nil {
				fmt.Printf("Error running parser on %s: %v\n", f, err)
				continue
			}
			var ir struct {
				ProgramName string `json:"program_name"`
				SourceFile  string `json:"source_file"`
			}
			if err := json.Unmarshal([]byte(parserStdout.String()), &ir); err != nil {
				fmt.Printf("Error parsing IR JSON for %s: %v\n", f, err)
				continue
			}
			fmt.Printf("Parsed: %s (program_name: %s)\n", ir.SourceFile, ir.ProgramName)

			// Call Rust summarizer binary
			sumArgs := []string{"..\\summarizer\\target\\release\\summarizer.exe"}
			if verbose {
				sumArgs = append(sumArgs, "--verbose")
			}
			if debug {
				sumArgs = append(sumArgs, "--debug")
			}
			sumCmd := exec.Command(sumArgs[0], sumArgs[1:]...)
			var sumStdout, sumStderr strings.Builder
			sumCmd.Stdout = &sumStdout
			sumCmd.Stderr = &sumStderr
			sumIn, err := sumCmd.StdinPipe()
			if err != nil {
				fmt.Printf("Error getting stdin pipe for Rust summarizer for %s: %v\n", f, err)
				continue
			}
			go func() {
				sumIn.Write([]byte(parserStdout.String()))
				sumIn.Close()
			}()
			err = sumCmd.Run()
			if (verbose || debug) && sumStderr.Len() > 0 {
				fmt.Fprint(os.Stderr, sumStderr.String())
			}
			if err != nil {
				fmt.Printf("Rust summarizer failed for %s: %v\n", f, err)
			}
			fmt.Println(sumStdout.String())
		}
		// TODO: Call Rust parser and Python pipeline here
	},
}
