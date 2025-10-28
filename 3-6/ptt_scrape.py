import requests
from bs4 import BeautifulSoup
import re

def scrape_spans_to_txt(url, output_file='scraped_text.txt'):
    try:
        # Send GET request
        headers = {
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36'
        }
        response = requests.get(url, headers=headers)
        response.raise_for_status()
        
        # Parse HTML
        soup = BeautifulSoup(response.content, 'html.parser')
        
        # Find all spans with class="f3 push-content"
        spans = soup.find_all('span', class_='f3 push-content')
        
        # Extract text and remove hyperlinks
        cleaned_texts = []
        for span in spans:
            text = span.get_text()
            # Remove http/https links using regex
            text = re.sub(r'https?://\S+', '', text)
            # Remove leading colon and any leading spaces
            text = re.sub(r'^\s*:\s*', '', text)
            # Clean up extra spaces and add to list
            cleaned_text = ' '.join(text.split())
            if cleaned_text:  # Only add non-empty strings
                cleaned_texts.append(cleaned_text)
        
        # Save to text file
        with open(output_file, 'w', encoding='utf-8') as f:
            for text in cleaned_texts:
                f.write(text + '\n')
        
        print(f"Successfully scraped {len(cleaned_texts)} entries to {output_file}")
        
    except Exception as e:
        print(f"Error: {e}")

# Usage
url = "https://www.ptt.cc/bbs/Stock/M.1761611402.A.333.html"
scrape_spans_to_txt(url, "251025_ptt.txt")