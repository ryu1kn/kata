from main import Solution


class TestMergeIntervals:
    def setup_method(self):
        self.solution = Solution()

    def test_empty_input_returns_empty(self):
        assert self.solution.merge([]) == []

    def test_single_interval(self):
        assert self.solution.merge([[1, 2]]) == [[1, 2]]

    def test_two_overlapping_intervals(self):
        assert self.solution.merge([[1, 3], [2, 4]]) == [[1, 4]]

    def test_more(self):
        assert self.solution.merge([[1,3],[2,6],[8,10],[15,18]]) == [[1, 6], [8, 10], [15, 18]]

    def test_more_2(self):
        assert self.solution.merge([[1,4],[4,5]]) == [[1, 5]]

    def test_more_3(self):
        assert self.solution.merge([[4,7],[1, 4]]) == [[1, 7]]
