package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"
)

func main() {
	count := flag.Int("count", 100, "Number of dummy COBOL files to generate")
	outdir := flag.String("outdir", "dummy-cobol", "Output directory for dummy COBOL files")
	flag.Parse()

	if err := os.MkdirAll(*outdir, 0755); err != nil {
		fmt.Fprintf(os.Stderr, "Failed to create output dir: %v\n", err)
		os.Exit(1)
	}

	template := "IDENTIFICATION DIVISION.\nPROGRAM-ID. DUMMY%d.\nPROCEDURE DIVISION.\nSTOP RUN.\n"
	for i := 1; i <= *count; i++ {
		fname := filepath.Join(*outdir, fmt.Sprintf("dummy%05d.cob", i))
		f, err := os.Create(fname)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Failed to create file %s: %v\n", fname, err)
			continue
		}
		fmt.Fprintf(f, template, i)
		f.Close()
	}
	fmt.Printf("Generated %d dummy COBOL files in %s\n", *count, *outdir)
}
