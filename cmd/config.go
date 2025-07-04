package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

// Config holds CLI and path configuration
// Implements config file and CLI flag parsing, with path normalization

type Config struct {
	RustBackendPath string
	InputDir        string
	OutputDir       string
}

// AddConfigFlags adds config-related flags to the CLI
func AddConfigFlags(cmd *cobra.Command) {
	cmd.PersistentFlags().String("rust-backend", "", "Path to the Rust backend binary (codesleuth.exe)")
	cmd.PersistentFlags().String("input", "", "Input directory or file to analyze")
	cmd.PersistentFlags().String("output", "", "Output directory for reports (default: stdout)")
	cmd.PersistentFlags().String("config", "", "Path to config file (YAML or JSON)")
	viper.BindPFlag("rust-backend", cmd.PersistentFlags().Lookup("rust-backend"))
	viper.BindPFlag("input", cmd.PersistentFlags().Lookup("input"))
	viper.BindPFlag("output", cmd.PersistentFlags().Lookup("output"))
	viper.BindPFlag("config", cmd.PersistentFlags().Lookup("config"))
}

// LoadConfig loads config from file and CLI flags, normalizes paths
func LoadConfig() (Config, error) {
	cfgFile := viper.GetString("config")
	if cfgFile != "" {
		viper.SetConfigFile(cfgFile)
		if err := viper.ReadInConfig(); err != nil {
			return Config{}, fmt.Errorf("failed to read config file: %v", err)
		}
	}
	// Merge CLI flags (override config file)
	rustBackend := viper.GetString("rust-backend")
	inputDir := viper.GetString("input")
	outputDir := viper.GetString("output")
	// Normalize paths (cross-platform)
	rustBackend = normalizePath(rustBackend)
	inputDir = normalizePath(inputDir)
	outputDir = normalizePath(outputDir)
	return Config{
		RustBackendPath: rustBackend,
		InputDir:        inputDir,
		OutputDir:       outputDir,
	}, nil
}

func normalizePath(p string) string {
	if p == "" {
		return p
	}
	p = filepath.Clean(p)
	if !filepath.IsAbs(p) {
		cwd, _ := os.Getwd()
		p = filepath.Join(cwd, p)
	}
	return filepath.FromSlash(strings.ReplaceAll(p, "\\", "/"))
}
