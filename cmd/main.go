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

func init() {
	analyzeCmd.Flags().BoolVar(&verbose, "verbose", false, "Enable verbose debug output")
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
			parserArgs := []string{"..\\parser\\target\\release\\parser", f}
			if verbose {
				parserArgs = append(parserArgs, "--verbose")
			}
			cmd := exec.Command(parserArgs[0], parserArgs[1:]...)
			output, err := cmd.Output()
			if err != nil {
				fmt.Printf("Error running parser on %s: %v\n", f, err)
				continue
			}
			var ir struct {
				ProgramName string `json:"program_name"`
				SourceFile  string `json:"source_file"`
			}
			if err := json.Unmarshal(output, &ir); err != nil {
				fmt.Printf("Error parsing IR JSON for %s: %v\n", f, err)
				continue
			}
			fmt.Printf("Parsed: %s (program_name: %s)\n", ir.SourceFile, ir.ProgramName)

			// Call Python summary script
			pyCmd := exec.Command("python", "..\\pytools\\summary.py")
			pyIn, err := pyCmd.StdinPipe()
			if err != nil {
				fmt.Printf("Error getting stdin pipe for Python summary for %s: %v\n", f, err)
				continue
			}
			go func() {
				pyIn.Write(output)
				pyIn.Close()
			}()
			summary, err := pyCmd.CombinedOutput()
			if err != nil {
				fmt.Printf("Python summary script failed for %s: %v\n", f, err)
			}
			fmt.Println(string(summary))
		}
		// TODO: Call Rust parser and Python pipeline here
	},
}
