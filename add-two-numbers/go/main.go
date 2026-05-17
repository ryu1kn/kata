package main

type ListNode struct {
	Val  int
	Next *ListNode
}

func addTwoNumbers(l1 *ListNode, l2 *ListNode) *ListNode {
	return addTwoNumbers_([]*ListNode{l1, l2})
}

func addTwoNumbers_(lists []*ListNode) *ListNode {
	nonNils := filter(lists, isNotNil)

	switch len(nonNils) {
	case 0:
		return nil
	case 1:
		return nonNils[0]
	default:
		sum := reduce(nonNils, func (acc int, c *ListNode) int {
            return acc + c.Val
        }, 0)

        quotient := sum / 10

        nexts := append(
            map_(nonNils, func (nonNil *ListNode) *ListNode { return nonNil.Next }),
            ifElse(quotient > 0, &ListNode{quotient, nil}, nil),
        )

		return &ListNode{ sum % 10, addTwoNumbers_(nexts) }
	}
}

func filter[T any](l []T, test func (T) bool) []T {
    var newL []T
    for _, el := range l {
        if test(el) {
            newL = append(newL, el)
        }
    }
    return newL
}

func map_[T, R any](list []T, converter func (T) R) []R {
    newList := make([]R, len(list))

    for i, val := range list {
        newList[i] = converter(val)
    }

    return newList
}

func reduce[T any, R any](l []T, reducer func (R, T) R, initial R) R {
    accumulated := initial
    for _, c := range l {
        accumulated = reducer(accumulated, c)
    }
    return accumulated
}

func ifElse[T any](test bool, cIf T, cElse T) T {
    if test {
        return cIf
    } else {
        return cElse
    }
}

func isNotNil[T any](v *T) bool {
    return v != nil
}
