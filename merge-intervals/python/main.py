from typing import List
from collections import defaultdict


class Solution:
    def merge(self, intervals: List[List[int]]) -> List[List[int]]:
        g = IntervalRegistry()

        for interval in intervals:
            g.register(interval)

        return g.calculateMergedIntervals()


class IntervalRegistry:
    def __init__(self):
        self.group_id_mappings = {}
        self.group_id = 0

    def register(self, incoming_interval):
        incoming = tuple(incoming_interval)

        id = self.lookup_group_id(incoming) or self.issue_new_id()

        self.group_id_mappings[incoming] = id

    def lookup_group_id(self, interval):
        found = next(
            ((existing, id) for existing, id in self.group_id_mappings.items()
                if has_overlap(interval, existing)),
            None
        )
        return found[1] if found else None

    def issue_new_id(self):
        self.group_id += 1
        return self.group_id

    def calculateMergedIntervals(self):
        groups_by_id = defaultdict(list)

        for interval, group_id in self.group_id_mappings.items():
            groups_by_id[group_id].append(interval)

        return [[
            min(itv[0] for itv in intervals),
            max(itv[1] for itv in intervals)
        ] for _, intervals in groups_by_id.items()]

def has_overlap(interval1: tuple[int, int], interval2: tuple[int, int]):
    return not (interval1[1] < interval2[0] or interval1[0] > interval2[1])
