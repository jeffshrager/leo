#!/usr/bin/env python3
"""
PNG to Life RLE Converter
Converts a PNG image to Conway's Game of Life RLE (Run-Length Encoding) format.
Black or dark pixels are treated as live cells, white or light pixels as dead cells.

python png2rle.py input.png -o output.rle -t 128
"""

import argparse
from PIL import Image
import numpy as np

def png_to_life_rle(input_file, output_file=None, threshold=128):
    """
    Convert a PNG image to Life RLE format.
    
    Args:
        input_file (str): Path to the input PNG file
        output_file (str, optional): Path to the output RLE file. If None, prints to console.
        threshold (int, optional): Brightness threshold (0-255) to determine live/dead cells.
                                  Pixels darker than this are treated as live cells.
    """
    # Open the image and convert to grayscale
    try:
        img = Image.open(input_file).convert('L')
    except Exception as e:
        print(f"Error opening image: {e}")
        return
    
    # Convert to numpy array and binarize
    data = np.array(img)
    binary = (data < threshold).astype(int)  # Dark pixels (< threshold) become live cells (1)
    
    # Get dimensions
    height, width = binary.shape
    
    # Prepare RLE header
    header = f"x = {width}, y = {height}, rule = B3/S23\n"
    
    # Generate RLE content
    rle_lines = []
    line_content = ""
    
    for row in binary:
        run_count = 0
        last_cell = None
        
        for cell in row:
            if cell == last_cell:
                run_count += 1
            else:
                if last_cell is not None:
                    symbol = 'o' if last_cell == 1 else 'b'
                    run_str = str(run_count) if run_count > 1 else ""
                    line_content += f"{run_str}{symbol}"
                
                run_count = 1
                last_cell = cell
        
        # Don't forget the last run
        if last_cell is not None:
            symbol = 'o' if last_cell == 1 else 'b'
            run_str = str(run_count) if run_count > 1 else ""
            line_content += f"{run_str}{symbol}"
        
        line_content += "$"
        
        # Check if line_content is getting too long (RLE spec recommends lines <= 70 chars)
        if len(line_content) > 70:
            rle_lines.append(line_content)
            line_content = ""
    
    # Add the final content and termination mark
    if line_content:
        rle_lines.append(line_content)
    
    # Make sure the last line ends with !
    if rle_lines:
        rle_lines[-1] = rle_lines[-1][:-1] + "!"  # Replace the last $ with !
    else:
        rle_lines.append("!")
    
    # Combine everything
    rle_content = header + "\n".join(rle_lines)
    
    # Output
    if output_file:
        try:
            with open(output_file, 'w') as f:
                f.write(rle_content)
            print(f"RLE file saved to {output_file}")
        except Exception as e:
            print(f"Error writing to file: {e}")
    else:
        print(rle_content)
    
    return rle_content

def main():
    parser = argparse.ArgumentParser(description="Convert PNG images to Life RLE format")
    parser.add_argument("input_file", help="Input PNG file")
    parser.add_argument("-o", "--output", help="Output RLE file (if omitted, prints to console)")
    parser.add_argument("-t", "--threshold", type=int, default=128, 
                        help="Brightness threshold (0-255) to determine live/dead cells")
    
    args = parser.parse_args()
    png_to_life_rle(args.input_file, args.output, args.threshold)

if __name__ == "__main__":
    main()
    
