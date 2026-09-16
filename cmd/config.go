package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

type Config struct {
	RustBackendPath string
	InputDir        string
	OutputDir       string
}

func AddConfigFlags(cmd *cobra.Command) error {
	flags := cmd.PersistentFlags()
	flags.String("rust-backend", "", "Path to the Rust backend binary")
	flags.String("input", "", "Input directory or file")
	flags.String("output", "", "Directory for Markdown reports (default: stdout)")
	flags.String("config", "", "Config file (YAML or JSON)")
	for _, name := range []string{"rust-backend", "input", "output", "config"} {
		if err := viper.BindPFlag(name, flags.Lookup(name)); err != nil {
			return err
		}
	}
	return nil
}

func LoadConfig() (Config, error) {
	cfgFile := viper.GetString("config")
	if cfgFile != "" {
		viper.SetConfigFile(cfgFile)
		if err := viper.ReadInConfig(); err != nil {
			return Config{}, fmt.Errorf("failed to read config file: %v", err)
		}
	}
	rustBackend := normalizePath(viper.GetString("rust-backend"))
	inputDir := normalizePath(viper.GetString("input"))
	outputDir := normalizePath(viper.GetString("output"))
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
		cwd, err := os.Getwd()
		if err != nil {
			return filepath.FromSlash(strings.ReplaceAll(p, "\\", "/"))
		}
		p = filepath.Join(cwd, p)
	}
	return filepath.FromSlash(strings.ReplaceAll(p, "\\", "/"))
}
