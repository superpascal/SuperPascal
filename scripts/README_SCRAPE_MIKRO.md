# Scrape Mikro Documentation Archive

## Overview

This script scrapes the Atari demoscene documentation archive at [https://mikro.naprvyraz.sk/docs/](https://mikro.naprvyraz.sk/docs/) and downloads all linked content.

The archive contains:
- **Motorola 680x0 resources** (68030, 68060, FPU documentation)
- **Algorithms** (3D graphics, sorting, compression, etc.)
- **Pascal and Assembly code** examples
- **Atari ST/Falcon hardware documentation**
- **GEM programming guides**
- **Optimization guides**

## Requirements

```bash
pip install requests beautifulsoup4
```

## Usage

```bash
python3 scripts/scrape_mikro_docs.py
```

Or make it executable and run directly:

```bash
chmod +x scripts/scrape_mikro_docs.py
./scripts/scrape_mikro_docs.py
```

## Output

The script will:
1. Create `docs/mikro_docs_archive/` directory
2. Download all HTML pages and linked files
3. Preserve the directory structure from the website
4. Generate a `scrape_report.txt` with statistics

## Features

- **Recursive scraping**: Follows all links within the site
- **Rate limiting**: 1 second delay between requests (polite scraping)
- **Error handling**: Continues on errors, logs failures
- **File type detection**: Downloads text files, archives, source code, etc.
- **Directory preservation**: Maintains original site structure
- **Progress logging**: Shows what's being downloaded

## File Types Downloaded

- `.txt`, `.html`, `.htm` - Text and HTML files
- `.zip`, `.tgz`, `.tar`, `.gz` - Archives
- `.asm`, `.s` - Assembly source
- `.c`, `.pas`, `.p` - C and Pascal source
- `.pdf`, `.doc`, `.docx` - Documents
- `.md`, `.markdown` - Markdown files

## Configuration

Edit the script to adjust:
- `OUTPUT_DIR`: Where to save files (default: `docs/mikro_docs_archive/`)
- `DELAY`: Seconds between requests (default: 1.0)
- `DOWNLOAD_EXTENSIONS`: File types to download
- `MAX_DEPTH`: Maximum recursion depth (default: 10)

## Notes

- The script respects the site structure and only downloads from `mikro.naprvyraz.sk`
- External links are skipped
- Failed downloads are logged in the report
- The script is polite (rate-limited) to avoid overloading the server

## Integration with SuperPascal

After scraping, the downloaded content can be:
1. Referenced in documentation (`docs/EXTERNAL_RESOURCES.md`)
2. Used as reference material for Motorola 680x0 backend
3. Studied for algorithm implementations
4. Referenced for Pascal/Assembly code examples

