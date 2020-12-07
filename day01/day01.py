from typing import List, Set

# --- Day 1: Report Repair ---

# After saving Christmas five years in a row, you've decided to take a vacation at a nice resort on a tropical island. Surely, Christmas will go on without you.

# The tropical island has its own currency and is entirely cash-only. The gold coins used there have a little picture of a starfish; the locals just call them stars. None of the currency exchanges seem to have heard of them, but somehow, you'll need to find fifty of these coins by the time you arrive so you can pay the deposit on your room.

# To save your vacation, you need to get all fifty stars by December 25th.

# Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!

# Before you leave, the Elves in accounting just need you to fix your expense report (your puzzle input); apparently, something isn't quite adding up.

# Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.

# For example, suppose your expense report contained the following:

# 1721
# 979
# 366
# 299
# 675
# 1456
# In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.

# Of course, your expense report is much larger. Find the two entries that sum to 2020; what do you get if you multiply them together?


def two_sum(lst: List[int], total: int) -> (int, int):
    numbers: Set[int] = set(n for n in lst)

    for n in numbers:
        remainder = total - n
        if remainder in numbers and remainder != n:
            return n, remainder


def fix_report(report: List[int]) -> int:
    n1, n2 = two_sum(report, 2020)
    return n1 * n2


# --- Part Two ---

# The Elves in accounting are thankful for your help; one of them even offers you a starfish coin they had left over from a past vacation. They offer you a second one if you can find three numbers in your expense report that meet the same criteria.

# Using the above example again, the three entries that sum to 2020 are 979, 366, and 675. Multiplying them together produces the answer, 241861950.

# In your expense report, what is the product of the three entries that sum to 2020?


def three_sum(lst: List[int], total: int) -> (int, int, int):
    for n in lst:
        remainder = total - n
        vals = two_sum(lst, remainder)
        if vals is not None:
            return n, *vals


def fix_report_v2(report: List[int]) -> int:
    n1, n2, n3 = three_sum(report, 2020)
    return n1 * n2 * n3


if __name__ == "__main__":
    with open("input.txt", "r") as f:
        data = [int(line) for line in f.readlines()]
        print(f"Part 1: {fix_report(data)}")
        print(f"Part 2: {fix_report_v2(data)}")
