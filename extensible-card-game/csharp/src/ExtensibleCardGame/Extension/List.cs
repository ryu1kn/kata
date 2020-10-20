using System;
using System.Collections.Generic;
using System.Linq;

namespace ExtensibleCardGame.Extension
{
    public static class List
    {
        public static int CountBy<T1, T2>(this IEnumerable<T1> items, Func<T1, T2> convert) =>
            items.Select(convert).Distinct().Count();
    }
}
