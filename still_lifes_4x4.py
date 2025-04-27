def int_to_grid(n):
    """Convert integer to 5x5 grid with 4x4 pattern in middle"""
    grid = [[0]*5 for _ in range(5)]
    binary = format(n, '016b')  # Convert to 16-bit binary
    idx = 0
    for i in range(1, 4+1):
        for j in range(1, 4+1):
            grid[i][j] = int(binary[idx])
            idx += 1
    return grid

def grid_to_int(grid):
    """Convert middle 4x4 of grid back to integer"""
    binary = ''
    for i in range(1, 4+1):
        for j in range(1, 4+1):
            binary += str(grid[i][j])
    return int(binary, 2)

def count_neighbors(grid, i, j):
    """Count live neighbors of cell at i,j"""
    count = 0
    for di in [-1, 0, 1]:
        for dj in [-1, 0, 1]:
            if di == 0 and dj == 0:
                continue
            ni, nj = i + di, j + dj
            # Check if neighbor coordinates are within grid bounds
            if 0 <= ni < 5 and 0 <= nj < 5:
                if grid[ni][nj] == 1:
                    count += 1
    return count

def next_generation(grid):
    """Compute next generation of the grid"""
    new_grid = [[0]*5 for _ in range(5)]
    
    # Apply rules to all cells first
    for i in range(5):
        for j in range(5):
            neighbors = count_neighbors(grid, i, j)
            if grid[i][j] == 1:
                if neighbors in [2, 3]:
                    new_grid[i][j] = 1
            else:
                if neighbors == 3:
                    new_grid[i][j] = 1
    
    # Force the frame to be 0 regardless of rules
    # Top and bottom rows
    for j in range(5):
        new_grid[0][j] = 0
        new_grid[4][j] = 0
    # Left and right columns
    for i in range(5):
        new_grid[i][0] = 0
        new_grid[i][4] = 0
        
    return new_grid

def grids_equal(grid1, grid2):
    """Check if two grids are equal"""
    for i in range(5):
        for j in range(5):
            if grid1[i][j] != grid2[i][j]:
                return False
    return True

def is_empty(grid):
    """Check if grid is completely empty"""
    return all(grid[i][j] == 0 for i in range(5) for j in range(5))

def test(n):
    """Test if pattern n leads to a still life"""
    if n == 0 or n >= 2**16:
        return False
        
    grid = int_to_grid(n)
    
    # Run for 25 generations
    for _ in range(25):
        new_grid = next_generation(grid)
        if grids_equal(grid, new_grid):
            # Found a still life - make sure it's not empty
            if not is_empty(grid):
                return grid_to_int(grid)
            return False
        grid = new_grid
    
    return False

def display_pattern(n):
    """Display 4x4 pattern as ASCII art"""
    binary = format(n, '016b')
    print("Pattern:", n)
    for i in range(0, 16, 4):
        row = binary[i:i+4]
        print(''.join(['%' if bit == '1' else '_' for bit in row]))
    print()

def run():
    """Find all patterns that lead to still lifes"""
    print("Still Life Patterns:")
    print("-----------------")
    still_lifes = set()
    
    for i in range(1, 2**16):
        result = test(i)
        if result:
            still_lifes.add(result)
    
    # Display unique still life patterns
    for pattern in sorted(still_lifes):
        display_pattern(pattern)

run()
