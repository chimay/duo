# Duo : Elisp list operations w/o copy

 Duo is a toolbox that allows you to manipulate lists in place, without
 the cost of creating a copy of it. Its functions will modify the list
 you pass as argument, whenever possible or reasonable.

More information [here](https://github.com/chimay/duo)

# Available operations

- Finding cons in list
- Finding previous / next cons
- Replace element
- Map
- Join
- Truncate
- Next / Previous in group
- Next / Previous matching filter
- Push, Add
- Pop, Drop
- Rotate <- or ->
- Roll until a cons is first or last
- Reverse
- Insert
- Remove, Delete
- Teleport : move after or before another cons or element
- Move previous or next
- Exchange cons or elements
- Insert in sorted list
- Insert at group beginning or end
- Partition with a key function to form an alist

# Circular list

Some functions simulate virtual circular lists by :

- Continuing at the beginning once arrived at the end
- Continuing at the end once arrived at the beginning
