package main

import (
	"path/filepath"
	"testing"

	"github.com/spf13/viper"
)

func TestLoadConfig(t *testing.T) {
	viper.Set("rust-backend", "./bin/codesleuth.exe")
	viper.Set("input", "./input")
	viper.Set("output", "./output")
	cfg, err := LoadConfig()
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !filepath.IsAbs(cfg.RustBackendPath) {
		t.Errorf("rust-backend path not absolute: %s", cfg.RustBackendPath)
	}
	if !filepath.IsAbs(cfg.InputDir) {
		t.Errorf("input path not absolute: %s", cfg.InputDir)
	}
	if !filepath.IsAbs(cfg.OutputDir) {
		t.Errorf("output path not absolute: %s", cfg.OutputDir)
	}
	// Clean up viper state
	viper.Reset()
}
