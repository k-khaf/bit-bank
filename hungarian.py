import numpy as np


def hungarian(m, row_name, col_name):
    n = len(m)

    # Subtract minimal value of the row from every element
    for i in range(n):
        m[i] = m[i] - min(m[i])

    # Subtract minimal value of the column for every element
    for j in range(n):
        m[:, j] = m[:, j] - min(m[:, j])

    iter1 = 0
    while iter1 < n:
        # By drawing only full-length horizontal or vertical lines, attempt to ‘cover’ all the
        # zeroes in the matrix using as few lines as possible. If n lines are required, then a
        # solution of cost 0 currently exists – find it and stop.
        d = m.copy()

        row_covered = []    # Empty array to store what rows covered
        col_covered = []    # Empty array to store what cols are covered

        iter1 = 0
        while np.any(d == 0):
            # Stores number of 0s in each row
            store1 = [0] * n
            for i in range(n):
                store1[i] = sum(d[i] == 0)

            # Stores number of 0s in each col
            store2 = [0] * n
            for j in range(n):
                store2[j] = sum(d[:, j] == 0)

            # Delete row/col with highest no. 0s
            if max(store1) >= max(store2):
                del1 = np.argmax(store1)
                d[del1, :] = 10000
                row_covered.append(del1)
            elif max(store1) < max(store2):
                del2 = np.argmax(store2)
                d[:, del2] = 10000
                col_covered.append(del2)

            iter1 += 1

        if iter1 == n:
            soln = solve(m, -1, -1, row_name, col_name)
        else:
            # Otherwise, draw on the lines from Step 3, and find the smallest non-zero element
            # (call its value k) which is not covered. Now subtract k from every uncovered row,
            # and add to every covered column. Then return to Step 3

            k = np.min(d)

            v = np.arange(n)
            row_uncovered = np.setdiff1d(v, row_covered)
            for i in row_uncovered:
                m[i] -= k

            for j in col_covered:
                m[:, j] += k

    print(soln)
    return soln


# solve() is a recursive function which inputs m, pivotRow and pivotCol and outputs a matrix containing which person is
# assigned to certain task. When the function is initially called the latter two entries are 0. This is how the
# function works:
# It stores the person - job allocation for the left most 0 on each row, and with that stores which jobs have been
# taken in 'seen'. If it gets to a person-job allocation where there is a clash in the jobs (i.e the job we're
# inspecting is an element of seen) then it will continue to go through each job for that person until it finds a
# match.  If it gets to the last column and no match has been made, then function is called again.  This time
# pivotRow and pivotCol take the values i and j such that i is the row (person) we were at and j is the index for
# the job where the clash occurred.  These values are accounted for accordingly in the function, which continues on
# to return an augmented allocation of rows 1:i-1.  We break out of this stack and continue to input the values in
# the storage matrix for person i and the no-longer clashing job j.

def solve(m, pivot_row, pivot_col, row_name, col_name):
    n = len(m)
    n1 = len(m)
    store = []
    seen = np.array([-1] * (n + 1))

    if pivot_row >= 0:
        n = pivot_row
        seen[0] = pivot_col

    for i in range(n):
        for j in range(n1):
            if m[i][j] == 0:
                if np.any(seen == [j]):  # Criteria to show if the job we're looking at has been take
                    pivot_col = j
                else:  # Populates store3 and seen when there are no collisions
                    store.append([row_name[i], col_name[j]])
                    seen[i + 1] = j

                    i += 1
                    break

            elif j == n1 - 1:  # If no other entries in row are 0 after job collision then function gets recalled
                pivot_row = i
                store = solve(m, pivot_row, pivot_col, row_name, col_name)

                store.append([row_name[i], col_name[pivot_col]])

    return store


A = np.reshape((1, 4, 5, 12, 5, 8, 9, 0, 6, 10, 2, 1, 6, 7, 11, 19), (4, 4))
row = ["1", "2", "3", "4"]
col = ["a", "b", "c", "d"]
hungarian(A, row, col)

B = np.reshape((2, 3, 4, 3, 6, 4, 5, 7, 8), (3, 3))
row = ["1", "2", "3", "4"]
col = ["a", "b", "c", "d"]
hungarian(B, row, col)

C = np.reshape((7, 5, 8, 2, 7, 8, 9, 4, 3, 5, 7, 9, 5, 5, 6, 7), (4, 4))
row = ["1", "2", "3", "4"]
col = ["a", "b", "c", "d"]
hungarian(C, row, col)
