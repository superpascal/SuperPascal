#!/usr/bin/env python3
"""
Scrape Mikro Documentation Archive
https://mikro.naprvyraz.sk/docs/

Downloads all linked content from the Atari demoscene documentation archive,
including Motorola 680x0 resources, algorithms, Pascal/Assembly code, and more.
"""

import os
import sys
import time
import urllib.parse
from pathlib import Path
from typing import Set, Optional
import requests
from bs4 import BeautifulSoup
import logging

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

# Base URL
BASE_URL = "https://mikro.naprvyraz.sk/docs/"
BASE_DOMAIN = "mikro.naprvyraz.sk"

# Output directory
OUTPUT_DIR = Path(__file__).parent.parent / "docs" / "mikro_docs_archive"

# File extensions to download
DOWNLOAD_EXTENSIONS = {
    '.txt', '.html', '.htm', '.zip', '.asm', '.s', '.c', '.pas', '.p', '.pdf',
    '.doc', '.docx', '.rtf', '.md', '.markdown', '.tgz', '.tar', '.gz'
}

# Headers to mimic a browser
HEADERS = {
    'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36',
    'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
    'Accept-Language': 'en-US,en;q=0.5',
    'Accept-Encoding': 'gzip, deflate',
    'Connection': 'keep-alive',
}

# Rate limiting (seconds between requests)
DELAY = 1.0


class MikroScraper:
    def __init__(self, base_url: str, output_dir: Path):
        self.base_url = base_url
        self.output_dir = output_dir
        self.visited_urls: Set[str] = set()
        self.downloaded_files: Set[str] = set()
        self.failed_urls: Set[str] = set()
        self.session = requests.Session()
        self.session.headers.update(HEADERS)
        
        # Create output directory
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
    def normalize_url(self, url: str, base: str = None) -> str:
        """Normalize and resolve relative URLs."""
        if base is None:
            base = self.base_url
            
        # Handle absolute URLs
        if url.startswith('http://') or url.startswith('https://'):
            parsed = urllib.parse.urlparse(url)
            if parsed.netloc == BASE_DOMAIN or parsed.netloc.endswith('.' + BASE_DOMAIN):
                return url
            else:
                return None  # External link, skip
        
        # Handle relative URLs
        if url.startswith('/'):
            return urllib.parse.urljoin(BASE_URL, url)
        else:
            return urllib.parse.urljoin(base, url)
    
    def get_file_path(self, url: str) -> Path:
        """Convert URL to local file path."""
        parsed = urllib.parse.urlparse(url)
        path = parsed.path.strip('/')
        
        # Remove leading 'docs/' if present
        if path.startswith('docs/'):
            path = path[5:]
        
        # Create safe filename
        if not path:
            path = 'index.html'
        
        # Handle query parameters in filename
        if parsed.query:
            # Add query as part of filename (sanitized)
            safe_query = urllib.parse.quote(parsed.query, safe='')
            name, ext = os.path.splitext(path)
            if not ext:
                ext = '.html'
            path = f"{name}_{safe_query}{ext}"
        
        return self.output_dir / path
    
    def should_download(self, url: str) -> bool:
        """Check if URL should be downloaded."""
        parsed = urllib.parse.urlparse(url)
        path = parsed.path.lower()
        
        # Check file extension
        _, ext = os.path.splitext(path)
        if ext in DOWNLOAD_EXTENSIONS:
            return True
        
        # Check if it's an HTML page (no extension or .html/.htm)
        if not ext or ext in {'.html', '.htm'}:
            return True
        
        return False
    
    def download_file(self, url: str) -> bool:
        """Download a file from URL."""
        if url in self.downloaded_files:
            logger.debug(f"Already downloaded: {url}")
            return True
        
        try:
            logger.info(f"Downloading: {url}")
            response = self.session.get(url, timeout=30, allow_redirects=True)
            response.raise_for_status()
            
            # Get file path
            file_path = self.get_file_path(url)
            file_path.parent.mkdir(parents=True, exist_ok=True)
            
            # Write file
            if 'text' in response.headers.get('Content-Type', '').lower():
                # Text file - write as UTF-8
                try:
                    content = response.text
                    file_path.write_text(content, encoding='utf-8')
                except UnicodeDecodeError:
                    # Fallback to binary
                    file_path.write_bytes(response.content)
            else:
                # Binary file
                file_path.write_bytes(response.content)
            
            self.downloaded_files.add(url)
            logger.info(f"Saved: {file_path}")
            
            # Rate limiting
            time.sleep(DELAY)
            
            return True
            
        except requests.exceptions.RequestException as e:
            logger.error(f"Failed to download {url}: {e}")
            self.failed_urls.add(url)
            return False
        except Exception as e:
            logger.error(f"Unexpected error downloading {url}: {e}")
            self.failed_urls.add(url)
            return False
    
    def extract_links(self, html_content: str, base_url: str) -> Set[str]:
        """Extract all links from HTML content."""
        soup = BeautifulSoup(html_content, 'html.parser')
        links = set()
        
        # Find all <a> tags with href
        for tag in soup.find_all('a', href=True):
            href = tag['href']
            normalized = self.normalize_url(href, base_url)
            if normalized:
                links.add(normalized)
        
        return links
    
    def scrape_page(self, url: str) -> Set[str]:
        """Scrape a single page and return links found."""
        if url in self.visited_urls:
            return set()
        
        self.visited_urls.add(url)
        
        try:
            logger.info(f"Scraping: {url}")
            response = self.session.get(url, timeout=30, allow_redirects=True)
            response.raise_for_status()
            
            # Save the page
            file_path = self.get_file_path(url)
            file_path.parent.mkdir(parents=True, exist_ok=True)
            
            if 'text/html' in response.headers.get('Content-Type', '').lower():
                # HTML page - extract links
                content = response.text
                file_path.write_text(content, encoding='utf-8')
                logger.info(f"Saved HTML: {file_path}")
                
                # Extract links
                links = self.extract_links(content, url)
                return links
            else:
                # Non-HTML file - just save it
                file_path.write_bytes(response.content)
                logger.info(f"Saved file: {file_path}")
                return set()
            
        except requests.exceptions.RequestException as e:
            logger.error(f"Failed to scrape {url}: {e}")
            self.failed_urls.add(url)
            return set()
        except Exception as e:
            logger.error(f"Unexpected error scraping {url}: {e}")
            self.failed_urls.add(url)
            return set()
        finally:
            # Rate limiting
            time.sleep(DELAY)
    
    def scrape_recursive(self, start_url: str, max_depth: int = 10, current_depth: int = 0):
        """Recursively scrape all linked pages."""
        if current_depth >= max_depth:
            logger.warning(f"Max depth reached for {start_url}")
            return
        
        # Download/scrape the page
        links = self.scrape_page(start_url)
        
        # Process each link
        for link in links:
            if link not in self.visited_urls:
                if self.should_download(link):
                    # Check if it's an HTML page (recursive) or a file (download only)
                    parsed = urllib.parse.urlparse(link)
                    path = parsed.path.lower()
                    _, ext = os.path.splitext(path)
                    
                    if not ext or ext in {'.html', '.htm'}:
                        # HTML page - scrape recursively
                        self.scrape_recursive(link, max_depth, current_depth + 1)
                    else:
                        # File - just download
                        self.download_file(link)
    
    def generate_report(self):
        """Generate a summary report."""
        report_path = self.output_dir / "scrape_report.txt"
        
        with open(report_path, 'w', encoding='utf-8') as f:
            f.write("Mikro Documentation Archive Scrape Report\n")
            f.write("=" * 60 + "\n\n")
            f.write(f"Base URL: {self.base_url}\n")
            f.write(f"Output Directory: {self.output_dir}\n")
            f.write(f"Date: {time.strftime('%Y-%m-%d %H:%M:%S')}\n\n")
            
            f.write(f"Pages Visited: {len(self.visited_urls)}\n")
            f.write(f"Files Downloaded: {len(self.downloaded_files)}\n")
            f.write(f"Failed URLs: {len(self.failed_urls)}\n\n")
            
            if self.failed_urls:
                f.write("Failed URLs:\n")
                f.write("-" * 60 + "\n")
                for url in sorted(self.failed_urls):
                    f.write(f"  {url}\n")
                f.write("\n")
        
        logger.info(f"Report saved to: {report_path}")


def main():
    """Main entry point."""
    scraper = MikroScraper(BASE_URL, OUTPUT_DIR)
    
    logger.info(f"Starting scrape of {BASE_URL}")
    logger.info(f"Output directory: {OUTPUT_DIR}")
    
    try:
        # Start recursive scraping from the base URL
        scraper.scrape_recursive(BASE_URL, max_depth=10)
        
        # Generate report
        scraper.generate_report()
        
        logger.info("Scraping completed!")
        logger.info(f"Total pages visited: {len(scraper.visited_urls)}")
        logger.info(f"Total files downloaded: {len(scraper.downloaded_files)}")
        logger.info(f"Failed URLs: {len(scraper.failed_urls)}")
        
        if scraper.failed_urls:
            logger.warning(f"Some URLs failed. Check {OUTPUT_DIR / 'scrape_report.txt'} for details.")
        
    except KeyboardInterrupt:
        logger.warning("Scraping interrupted by user")
        scraper.generate_report()
        sys.exit(1)
    except Exception as e:
        logger.error(f"Fatal error: {e}", exc_info=True)
        scraper.generate_report()
        sys.exit(1)


if __name__ == "__main__":
    main()

