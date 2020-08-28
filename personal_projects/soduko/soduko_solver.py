def solve(bo):
    # solve uses the backtracking algorithm to find if a Sudoku is solvable
    # Input: A (9x9) Int array representing 'empty spaces' with 0s
    # Output: Bool

    find = find_empty(bo)
    if not find:
        return True
    else:
        row, col = find

    for i in range(1, 10):
        if is_valid(bo, i, (row, col)):
            bo[row][col] = i

            if solve(bo):
                return True
            else:
                bo[row][col] = 0

    return False


def is_valid(bo, num, index):
    # Checks to see if a given number is valid in an empty square
    # Input: [9x9] Int array,
    #        Int (1:9),
    #        [1x2] Int array (0:8)
    # Output: Bool

    # Check row
    if num in bo[index[0]]:
        return False

    # Check col
    col = []
    for i in range(len(bo)):
        col.append(bo[i][index[1]])

    if num in col:
        return False

    # Check square
    square = []
    box_i = 3 * (index[1] // 3)
    box_j = 3 * (index[0] // 3)

    for i in range(box_j, box_j + 3):
        for j in range(box_i, box_i + 3):
            square.append(bo[i][j])

    if num in square:
        return False

    return True


def find_empty(bo):
    # Finds index an 'empty space' in the array
    # Input: A (9x9) int array
    # Output: [1x2] int array (0:8)
    #         False if board complete

    for i in range(len(bo)):
        for j in range(len(bo)):
            if bo[i][j] == 0:
                return i, j

    return False


def print_board(bo):
    for i in range(len(bo)):
        if i % 3 == 0 and i != 0:
            print("- - - - - - - - - - - - - ")

        for j in range(len(bo)):
            if j % 3 == 0 and j != 0:
                print(" | ", end="")

            if j == 8:
                print(bo[i][j])
            else:
                print(str(bo[i][j]) + " ", end="")


board = [[5, 1, 7, 6, 0, 0, 0, 3, 4], [2, 8, 9, 0, 0, 4, 0, 0, 0], [3, 4, 6, 2, 0, 5, 0, 9, 0],
         [6, 0, 2, 0, 0, 0, 0, 1, 0], [0, 3, 8, 0, 0, 6, 0, 4, 7], [0, 0, 0, 0, 0, 0, 0, 0, 0],
         [0, 9, 0, 0, 0, 0, 0, 7, 8], [7, 0, 3, 4, 0, 0, 5, 6, 0], [0, 0, 0, 0, 0, 0, 0, 0, 0]]

solve(board)
print_board(board)
