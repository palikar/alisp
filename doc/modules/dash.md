### Dash

#### Description

Dash helps you work with lists. This module is entirely based on the [dash.el](https://github.com/magnars/dash.el) library for emacs lisp. The functions in the module are implemented pureley in alisp.
#### Functions

**d-dotimes** : *(d-dotimes NUM FN)*

Repeatedly calls fn (presumably for side-effects) passing in integers from 0 through num-1.

**d-each-r-while** : *(d-each-r-while LIST PRED FN)*

Call fn with every item in reversed list while (pred item) is non-nil. Return nil, used for side-effects only.

**d-each-r** : *(d-each-r LIST FN)*

Call fn with every item in list in reversed order. Return nil, used for side-effects only.

**d-each** : *(d-each LIST FN)*

Call fn with every item in list. Return nil, used for side-effects only.

**d-last-item** : *(d-last-item LIST)*

Return the last item of list, or nil on an empty list.

**d-fifth-item** : *(d-fifth-item ARG1)*

Return the fifth item of list, or nil if list is too short.

**d-second-item** : *(d-second-item ARG1)*

Return the second item of list, or nil if list is too short.

**d-first-item** : *(d-first-item LIST)*

Return the first item of list, or nil on an empty list.

**d-last** : *(d-last PRED LIST)*

Return the last x in list where (pred x) is non-nil, else nil.

**d-some** : *(d-some PRED LIST)*

Return (pred x) for the first list item where (pred x) is non-nil, else nil.

**d-distinct** : *(d-distinct LIS)*

Return a new list with all duplicates removed. The test for equality is done with equal, or with -compare-fn if that's non-nil.

**d-permutations** : *(d-permutations LIS)*

Return the permutations of list.

**d-powerset** : *(d-powerset LIS)*

Return the power set of list.

**d-fourth-item** : *(d-fourth-item ARG1)*

Return the fourth item of list, or nil if list is too short.

**d-intersection** : *(d-intersection LIST1 LIST2)*

Return a new list containing only the elements that are members of both list and list2. The test for equality is done with equal, or with -compare-fn if that's non-nil.

**d-difference** : *(d-difference LIST LIST2)*

Return a new list with only the members of list that are not in list2. The test for equality is done with equal, or with -compare-fn if that's non-nil.

**d-find-indices** : *(d-find-indices PRED LIS)*

Return the indices of all elements in list satisfying the predicate pred, in ascending order.

**d-elem-indices** : *(d-elem-indices ELEM LIS)*

Return the indices of all elements in list equal to the query element elem, in ascending order.

**d-elem-index** : *(d-elem-index ELEM LIST)*

Return the index of the first element in the given list which is equal to the query element elem, or nil if there is no such element.

**d-third-item** : *(d-third-item ARG1)*

Return the third item of list, or nil if list is too short.

**d-group-by** : *(d-group-by FN LIS)*

Separate list into an alist whose keys are fn applied to the elements of list. Keys are compared by equal.

**d-partition-by-header** : *(d-partition-by-header FN LIS)*

Apply fn to the first item in list. That is the header value. Apply fn to each item in list, splitting it each time fn returns the header value, but only after seeing at least one other value (the body).

**d-partition-by** : *(d-partition-by FN LIS)*

Apply fn to each item in list, splitting it each time fn returns a new value.

**d-partition-all** : *(d-partition-all N LIS)*

Return a new list with the items in list grouped into n-sized sublists. The last group may contain less than n items.

**d-partition** : *(d-partition N LIS)*

Return a new list with the items in list grouped into n-sized sublists. If there are not enough items to make the last group n-sized, those items are discarded.

**d-split-on** : *(d-split-on ITEM LIS)*

Split the list each time item is found.

**d-split-with** : *(d-split-with PRED LIS)*

Return a list of ((-take-while pred list) (-drop-while pred list)), in no more than one pass through the list.

**d-is-infix?** : *(d-is-infix? INFIX LIS)*

Return non-nil if infix is infix of list.

**d-is-prefix?** : *(d-is-prefix? PREFIX LIS)*

Return non-nil if prefix is prefix of list.

**d-same-items?** : *(d-same-items? LIST1 LIST2)*

Return true if list and list2 has the same items.

**d-contains?** : *(d-contains? LIST ELEMENT)*

Return non-nil if list contains element.

**d-split-at** : *(d-split-at N LIS)*

Return a list of ((-take n list) (-drop n list)), in no more than one pass through the list.

**d-none?** : *(d-none? PRED LIST)*

Return t if (pred x) is nil for all x in list, else nil.

**d-any?** : *(d-any? PRED LIS)*

Return t if (pred x) is non-nil for any x in list, else nil.

**d-each-indexed** : *(d-each-indexed LIST FN)*

Call (fn index item) for each item in list.

**d-repeat** : *(d-repeat N X)*

Return a list with x repeated n times. Return nil if n is less than 1.

**d-iterate** : *(d-iterate FUN INIT N)*

Return a list of iterated applications of fun to init.

**d-replace-first** : *(d-replace-first OLD NEW L)*

Replace the first occurrence of old with new in list.

Elements are compared using equal.

**d-drop-while** : *(d-drop-while PRED LIS)*

Return the tail of list starting from the first item for which (pred item) returns nil.

**d-tails** : *(d-tails LIS)*

Return all suffixes of list

**d-union** : *(d-union LIST1 LIST2)*

Return a new list containing the elements of list and elements of list2 that are not in list. The test for equality is done with equal, or with -compare-fn if that's non-nil.

**d-select-column** : *(d-select-column COLUMN TABLE)*

Select column from table.

table is a list of lists where each element represents one row. It is assumed each row has the same length.

The single selected column is returned as a list.

**d-select-by-indices** : *(d-select-by-indices INDICES LIS)*

Return a list whose elements are elements from list selected as (nth i list) for all i from indices.

**d-non-nil** : *(d-non-nil LIST)*

Return all non-nil elements of list.

**d-remove-last** : *(d-remove-last PRED LIS)*

Return a new list with the last item matching pred removed.

**d-remove-first** : *(d-remove-first PRED LIS)*

Return a new list of the items in list for which pred returns nil.

**d-separate** : *(d-separate PRED LIS)*

Return a list of ((-filter pred list) (-remove pred list)), in one pass through the list.

**d-running-product** : *(d-running-product LIS)*

Return a list with running products of items in list.

**d-map-when** : *(d-map-when PRED REP L)*

Return a new list where the elements in `L` that do not match the `PRED` function are unchanged, and where the elements in `L` that do match the pred function are mapped through the `REP` function.

**d-map** : *(d-map FN LIST)*

Return a new list consisting of the result of applying `FN` to the items in `LIST`.

**d-remove** : *(d-remove PRED LIS)*

Return a new list of the items in list for which pred returns nil.

**d-split-when** : *(d-split-when FN LIS)*

Split the list on each element where fn returns non-nil.

**d-concat** : *(d-concat &REST LISTS)*

Return a new list with the concatenation of the elements in the supplied lists.

**d-list** : *(d-list &REST ARGS)*

Return a list with args.

**d-first** : *(d-first PRED LIS)*

Return the first x in list where (pred x) is non-nil, else nil.

**d-common-prefix** : *(d-common-prefix &REST L)*

Return the longest common prefix of lists.

**d-slice** : *(d-slice LIS FROM &OPTIONAL TO STEP)*

Return copy of list, starting from index from to index to.

**d-take-while** : *(d-take-while PRED LIS)*

Return a new list of successive items from list while (pred item) returns a non-nil value.

**d--partition-all-in-steps-reversed** : **

**d-map-last** : *(d-map-last PRED REP L)*

Replace last item in list satisfying pred with result of rep called on this item.

**d-is-suffix?** : *(d-is-suffix? SUFFIX LIST)*

Return non-nil if suffix is suffix of list.

**d-select-columns** : *(d-select-columns COLUMNS TABLE)*

Select columns from table.

table is a list of lists where each element represents one row. It is assumed each row has the same length.

**d-each-while** : *(d-each-while LIST PRED FN)*

Call fn with every item in list while (pred item) is non-nil. Return nil, used for side-effects only.

**d-remove-item** : *(d-remove-item ITEM LIST)*

Remove all occurrences of item from list.

**d-annotate** : *(d-annotate FN LIS)*

Return a list of cons cells where each cell is fn applied to each element of list paired with the unmodified element of list.

**d-reduce-r** : *(d-reduce-r FN LIS)*

Replace conses with fn and evaluate the resulting expression. The final nil is ignored. If list contains no items, return the result of calling fn with no arguments. If list contains a single item, return that item and do not call fn.

The first argument of fn is the new item, the second is the accumulated value.

**d-map-first** : *(d-map-first PRED REP L)*

Replace first item in `L` satisfying `PRED` with result of `REP` called on this item.

**d-replace** : *(d-replace OLD NEW LIS)*

Replace all old items in list with new.

Elements are compared using equal.

**d-splice** : *(d-splice PRED FUN LIS)*

Splice lists generated by fun in place of elements matching pred in list. `fun` takes the element matching pred as input.

**d-reduce-r-from** : *(d-reduce-r-from FN INITIAL-VALUE LIS)*

Replace conses with fn, nil with initial-value and evaluate the resulting expression. If list is empty, initial-value is returned and fn is not called.

**d-partition-in-steps** : *(d-partition-in-steps N STEP LIS)*

Return a new list with the items in list grouped into n-sized sublists at offsets step apart. If there are not enough items to make the last group n-sized, those items are discarded.

**d-mapcat** : *(d-mapcat FN LIS)*

Return the concatenation of the result of mapping fn over list. Thus function fn should return a list.

**d-filter** : *(d-filter PRED LIS)*

Return a new list of the items in list for which pred returns a non-nil value.

**d-take-last** : *(d-take-last N LIS)*

Return the last n items of list in order.

**d-only-some?** : *(d-only-some? PRED LIS)*

Return t if at least one item of list matches pred and at least one item of list does not match pred. Return nil both if all items match the predicate or if none of the items match the predicate.

**d-product** : *(d-product LIS)*

Return the product of list.

**d-unfold** : *(d-unfold FUN SEED)*

Build a list from seed using fun.

**d-map-indexed** : *(d-map-indexed FN LIS)*

Return a new list consisting of the result of (fn index item) for each item in list.

**d-splice-list** : *(d-splice-list PRED NEW-LIST LIS)*

Splice new-list in place of elements matching pred in list.

**d-find-index** : *(d-find-index PRED LIS)*

Take a predicate pred and a list and return the index of the first element in the list satisfying the predicate, or nil if there is no such element.

**d-copy** : *(d-copy ARG)*

Create a shallow copy of list.

**d-inits** : *(d-inits LIS)*

Return all prefixes of list.

**d-min-by** : *(d-min-by COMPARATOR LIS)*

Take a comparison function comparator and a list and return the least element of the list by the comparison function.

**d-flatten** : *(d-flatten L)*

Take a nested list l and return its contents as a single, flat list.

**d-keep** : *(d-keep FN LIS)*

Return a new list of the non-nil results of applying fn to the items in list.

**d-running-sum** : *(d-running-sum LIS)*

Return a list with running sums of items in list.

**d-insert-at** : *(d-insert-at N X LIS)*

Return a list with x inserted into list at position n.

**d-replace-at** : *(d-replace-at N X LIS)*

Return a list with element at Nth position in list replaced with x.

**d-count** : *(d-count PRED LIS)*

Counts the number of items in list where (pred item) is non-nil.

**d-butlast** : *(d-butlast LIST)*

Return a list of all items in list except for the last.

**d-remove-at** : *(d-remove-at N LIS)*

Return a list with element at Nth position in list removed.

**d-find-last-index** : *(d-find-last-index PRED LIS)*

Take a predicate pred and a list and return the index of the last element in the list satisfying the predicate, or nil if there is no such element.

**d-remove-at-indices** : *(d-remove-at-indices INDICES LIS)*

Return a list whose elements are elements from list without elements selected as (nth i list) for all i from indices.

**d-partition-all-in-steps** : *(d-partition-all-in-steps N STEP LIS)*

Return a new list with the items in list grouped into n-sized sublists at offsets step apart. The last groups may contain less than n items.

**d-reduce-from** : *(d-reduce-from FN INITIAL-VALUE LIS)*

Return the result of applying fn to initial-value and the first item in list, then applying fn to that result and the 2nd item, etc. If list contains no items, return initial-value and do not call fn.

**d-reduce** : *(d-reduce FN LIS)*

Return the result of applying fn to the first 2 items in list, then applying fn to that result and the 3rd item, etc. If list contains no items, return the result of calling fn with no arguments. If list contains a single item, return that item and do not call fn.

**d-all?** : *(d-all? PRED LIS)*

Return t if (pred x) is non-nil for all x in list, else nil.

**d-drop** : *(d-drop N LIS)*

Return the tail of list without the first n items.

**d-reductions-from** : *(d-reductions-from FN INIT LIS)*

Return a list of the intermediate values of the reduction.

**d-reductions-r-from** : *(d-reductions-r-from FN INITIAL-VALUE LIS)*

Return a list of the intermediate values of the reduction.

**d-take** : *(d-take N LIS)*

Return a new list of the first n items in list, or all items if there are fewer than n.

**d-min** : *(d-min LIS)*

Return the smallest value from list of numbers or markers.

**d-drop-last** : *(d-drop-last N LIS)*

Remove the last n items of list and return a copy.

**d-reductions** : *(d-reductions FN LIS)*

Return a list of the intermediate values of the reduction.

**d-update-at** : *(d-update-at N FUNC LIS)*

Return a list with element at Nth position in list replaced with (func (nth n list)).

**d-sum** : *(d-sum LIS)*

Return the sum of list.

**d-common-suffix** : *(d-common-suffix &REST LISTS)*

Return the longest common suffix of lists.

**d-replace-last** : *(d-replace-last OLD NEW L)*

Replace the last occurrence of old with new in list.

Elements are compared using equal.

**d-max** : *(d-max LIS)*

Return the largest value from list of numbers or markers.

**d-reductions-r** : *(d-reductions-r FN LIS)*

Return a list of the intermediate values of the reduction.

**d-max-by** : *(d-max-by COMPARATOR LIS)*

Take a comparison function comparator and a list and return the greatest element of the list by the comparison function.

#### Constants


