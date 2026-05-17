package main

import (
	"fmt"
	"strconv"
	"testing"
)

func TestNumber(t *testing.T) {
	var testCases = []struct {
		calculated *ListNode
		expected   *ListNode
		string_rep string
	}{
		{toListNode(0), &ListNode{0, nil}, "0"},
		{toListNode(10), &ListNode{0, &ListNode{1, nil}}, "10"},
		{toListNode(342), &ListNode{2, &ListNode{4, &ListNode{3, nil}}}, "342"},
	}

	for _, tt := range testCases {
		t.Run(tt.string_rep, func(t *testing.T) {
			if !sameNumber(tt.calculated, tt.expected) {
				t.Errorf("toListNode: got %s, want %s", tt.calculated.toString(), tt.expected.toString())
			}
			if calc_string := tt.calculated.toString(); calc_string != tt.string_rep {
				t.Errorf("toString: got %s, want %s", calc_string, tt.string_rep)
			}
		})
	}
}

func TestAdditions(t *testing.T) {
	var testCases = []struct {
		num1  *ListNode
		num2  *ListNode
		total *ListNode
	}{
		{toListNode(0), toListNode(0), toListNode(0)},
		{toListNode(1), toListNode(1), toListNode(2)},
		{toListNode(12), toListNode(12), toListNode(24)},
		{toListNode(123), toListNode(123), toListNode(246)},
		{toListNode(1), toListNode(10), toListNode(11)},
		{toListNode(10), toListNode(1), toListNode(11)},
		{toListNode(9), toListNode(1), toListNode(10)},
		{toListNode(342), toListNode(465), toListNode(807)},
		{toListNode(9999999), toListNode(9999), toListNode(10009998)},
	}

	for _, tt := range testCases {
		testName := fmt.Sprintf("%s + %s", tt.num1.toString(), tt.num2.toString())

		t.Run(testName, func(t *testing.T) {
			if actual := addTwoNumbers(tt.num1, tt.num2); !sameNumber(actual, tt.total) {
				t.Errorf("got %s, want %s", actual.toString(), tt.total.toString())
			}
		})
	}
}

func sameNumber(n_start *ListNode, m_start *ListNode) bool {
	n := n_start
	m := m_start

	for {
		if n == nil || m == nil {
			break
		}

		if n.Val != m.Val {
			return false
		}

		n = n.Next
		m = m.Next
	}

	return n == m
}

func toListNode(number int) *ListNode {
	if number == 0 {
		return &ListNode{0, nil}
	}
	return toListNode_(number)
}

func toListNode_(number int) *ListNode {
	if number == 0 {
		return nil
	}
	return &ListNode{
		Val:  number % 10,
		Next: toListNode_(number / 10),
	}
}

func (l *ListNode) toString() string {
	if l != nil {
		return l.Next.toString() + strconv.Itoa(l.Val)
	}
	return ""
}
