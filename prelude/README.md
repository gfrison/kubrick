# Kubrick Prelude

A Scala 3 library providing functional data structures and utilities for the Kubrick project.

## Lem - Recursive Data Structure

Lem is a powerful recursive data structure that unifies sequences, multisets, dictionaries, and choice types under a single elegant abstraction. It features intelligent type promotion, automatic balancing (where appropriate), and guaranteed invariants.

### Core Concepts

#### Atoms (Leaves)

Atoms are the fundamental building blocks and **do not inherit from collection traits**:

- **`L0`**: The empty Lem, acting as the identity element
- **`L1[T](value: T)`**: A single-value leaf containing one element
- **`Pair[T](key: Lem[T], value: Lem[T])`**: A key-value atom for dictionary semantics

#### Collection Traits (Marker Interfaces)

Collections are formed when atoms are combined. The following marker traits identify collection semantics:

- **`Sek[T]`**: Ordered sequence (position-based) - prioritizes fast sequential access
- **`Bag[T]`**: Unordered multiset (existence-based) - allows duplicates
- **`Dict[T]`**: Dictionary/mapping - automatically triggered when at least one `Pair` is present
- **`Choice[T]`**: Unordered set of alternative options - represents possibilities

#### The Branch Container

Collections with ≥2 elements are represented by:

```scala
Branch[T](
  left: Lem[T],
  right: Lem[T],
  size: Int,        // Total leaf count for indexing
  minHash: Int,     // Minimum hash for lookup optimization
  maxHash: Int,     // Maximum hash for lookup optimization  
  height: Int       // Tree height (meaningful for balanced trees)
)
```

### The Min-2 Rule

**Collections must contain at least 2 elements.** When an operation reduces a collection to a single element, it automatically collapses to an atom:

```scala
val sek = L1(1) +: L1(2) +: L1(3)  // Branch with 3 elements
val head +: tail = sek              // head = L1(1), tail = Branch(2,3)
val next +: last = tail             // next = L1(2), last = L1(3) (collapsed to atom)
```

### Dual Topology Architecture

Lem uses **two different tree topologies** optimized for different use cases:

#### Sek: Unbalanced Spined Tree

**Structure**: Left = Head, Right = Tail (like a cons list)

**Performance**:
- Prepend (`+:`): **O(1)** - new head becomes left child
- Head extraction: **O(1)** - pattern match on left child
- Append (`:+`): **O(1)** for single elements*
- Index access: **O(N)** worst case

**Use When**: Sequential access patterns, frequent prepend/append operations, order matters

```scala
val seq = L1(1) +: L1(2) +: L1(3)  // Left-spined: (1, (2, 3))
```

#### Bag/Choice/Dict: Balanced AVL Tree

**Structure**: Self-balancing binary tree with automatic rotations

**Performance**:
- Add (`+`): **O(log N)** - with automatic rebalancing
- Lookup: **O(log N)** - binary search via hash ranges
- Choice (`||`): **O(log N)** - balanced combination
- Height: Always **≤ 1.44 × log₂(N)**

**Use When**: Unordered collections, frequent lookups, set semantics needed

```scala
val bag = L1(1) + L1(2) + L1(3)  // Balanced tree structure
```

### Operators and Construction

#### Sequence Operations (Sek)

```scala
import kubrick.prelude.lem.all.*

val empty = L0
val single = L1(42)

// Prepend (creates Sek)
val seq1 = L1(1) +: L1(2)           // Sek: (1, 2)
val seq2 = L1(0) +: seq1            // Sek: (0, 1, 2)

// Append (creates Sek)  
val seq3 = seq1 :+ L1(3)            // Sek: (1, 2, 3)

// Pattern matching with collapse
val head +: tail = seq3             // head = L1(1), tail = Branch
val init :+ last = seq3             // init = Branch, last = L1(3)
```

#### Bag Operations (Multiset)

```scala
// Add to bag (creates balanced tree)
val bag1 = L1(1) + L1(2)            // Bag: {1, 2}
val bag2 = bag1 + L1(3) + L1(2)     // Bag: {1, 2, 2, 3} - allows duplicates

// Extraction (with collapse)
val elem + rest = bag2              // Extract one element
```

#### Choice Operations (Alternatives)

```scala
// Create alternatives
val opt1 = L1("a") || L1("b")       // Choice: a | b
val opt2 = opt1 || L1("c")          // Choice: a | b | c

// Pattern matching
val first || others = opt2          // first = one option, others = remaining
```

#### Dictionary Operations

```scala
// Create key-value pairs
val kv1 = L1("name") --> L1("Alice")           // Pair
val kv2 = L1("age") --> L1(30)                 // Pair

// Combine into dictionary
val dict = kv1 + kv2                           // Dict & Bag: auto-promotion

// Pattern matching
val key --> value = kv1                        // key = L1("name"), value = L1("Alice")
```

### Smart Constructors

The module provides smart constructors that enforce the Min-2 rule and apply appropriate topology:

```scala
makeSek(left, right)     // Creates unbalanced Sek (O(1) prepend)
makeBag(left, right)     // Creates balanced Bag (O(log N) lookup)
makeChoice(left, right)  // Creates balanced Choice (O(log N) lookup)
```

These are automatically used by the operators (`+:`, `+`, `||`).

### Cats Integration

Lem provides `Traverse` instances for functional programming:

```scala
import cats.implicits.*
import kubrick.prelude.lem.all.*

val nums = L1(1) +: L1(2) +: L1(3)

// Map
val doubled = nums.map(_ * 2)                  // Sek: (2, 4, 6)

// Traverse
val validated: Option[Lem[Int]] = 
  nums.traverse(n => if (n > 0) Some(n) else None)

// Fold
val sum = nums.foldLeft(0)(_ + _)              // 6
```

### Performance Summary

| Operation | Sek (Unbalanced) | Bag/Choice (Balanced) | Dict (Balanced) |
|-----------|------------------|----------------------|-----------------|
| Prepend (`+:`) | **O(1)** | N/A | N/A |
| Append (`:+`) | **O(1)*** | N/A | N/A |
| Add (`+`) | N/A | **O(log N)** | **O(log N)** |
| Choice (`||`) | N/A | **O(log N)** | N/A |
| Head extraction | **O(1)** | N/A | N/A |
| Lookup by hash | O(N) | **O(log N)** | **O(log N)** |
| Index access | O(N) | O(N) | O(N) |
| Map/Traverse | O(N) | O(N) | O(N) |
| Size | **O(1)** (cached) | **O(1)** (cached) | **O(1)** (cached) |

*Append is O(1) for single elements in the common case; may degrade with larger right subtrees.

### Type Safety Guarantees

1. **Atoms are NOT collections**: `L1` and `Pair` do not extend `Sek`, `Bag`, `Dict`, or `Choice`
2. **Min-2 enforcement**: Collections with <2 elements automatically collapse to atoms
3. **Type preservation**: Extractors maintain collection type through pattern matching
4. **No casts**: Entire implementation is `asInstanceOf`-free
5. **Covariant**: All types are covariant in `T` (`+T`)

### Design Rationale

#### Why Dual Topology?

Different access patterns require different data structures:

- **Sequential data** (logs, streams, lists): Unbalanced spine provides O(1) prepend, perfect for cons-style construction
- **Unordered data** (sets, bags, choices): Balanced tree provides O(log N) lookup, preventing worst-case linear search

#### Why Single Branch Type?

Using one `Branch` type for both topologies:
- Simplifies pattern matching (no type casting needed)
- Reduces code duplication
- Allows topology to be a construction detail, not a type-level constraint
- Enables future optimizations (adaptive balancing, lazy evaluation)

#### Why Atoms Don't Extend Collections?

Atoms represent **singular values**, not collections:
- Clearer semantics: A single element is not a sequence/bag/choice
- Type safety: Prevents confusion between `L1(x)` and a collection containing `x`
- Min-2 rule enforcement: Natural boundary between atoms and collections

### Usage Examples

#### Building a Sequence

```scala
import kubrick.prelude.lem.all.*

// Incremental construction
val log = L1("start") +: 
          L1("processing") +: 
          L1("done")
// Result: Sek with O(1) prepend

// Pattern matching
log match {
  case first +: rest => 
    println(s"First entry: $first")
    // Process rest...
  case L1(single) => 
    println("Single entry")
  case L0 => 
    println("Empty log")
}
```

#### Building a Bag

```scala
// Word count
val words = L1("hello") + L1("world") + L1("hello")
// Result: Bag with duplicates allowed

// Merge bags (balanced)
val more = L1("scala") + L1("cats")
val combined = words + more  // Still balanced
```

#### Building a Choice

```scala
// Configuration options
val colors = L1("red") || L1("green") || L1("blue")

// Extract first option
val selected || remaining = colors
```

#### Building a Dictionary

```scala
// Configuration map
val config = 
  (L1("host") --> L1("localhost")) +
  (L1("port") --> L1("8080")) +
  (L1("debug") --> L1("true"))

// Pattern match pairs
config match {
  case (key --> value) + rest => 
    println(s"$key = $value")
    // Process rest...
  case _ => ()
}

// Dict methods: containsKey and get
config.containsKey(L1("host"))        // true
config.containsKey(L1("missing"))     // false

config.get(L1("host"))                // Some(L1("localhost"))
config.get(L1("missing"))             // None
```

#### SekDict - Ordered Dictionary

A Sek containing Pairs becomes both a sequence and a dictionary:

```scala
// Ordered key-value pairs
val orderedMap = 
  (L1("first") --> L1("1")) +:
  (L1("second") --> L1("2")) +:
  (L1("third") --> L1("3"))

// Access as dictionary
orderedMap.get(L1("second"))          // Some(L1("2"))
orderedMap.containsKey(L1("first"))   // true

// Maintains insertion order (Sek property)
// Fold traverses pairs in order
```

### Dictionary Methods

All `Lem[T]` structures containing `Pair` elements support dictionary operations:

#### `containsKey(key: Lem[T]): Boolean`

Checks if a key exists in the dictionary by recursively searching for a matching Pair.

```scala
val dict = (L1("a") --> L1("1")) + (L1("b") --> L1("2"))
dict.containsKey(L1("a"))   // true
dict.containsKey(L1("c"))   // false
```

#### `get(key: Lem[T]): Option[Lem[T]]`

Retrieves the value associated with a key, returning `Some(value)` if found, `None` otherwise.

```scala
val dict = (L1("name") --> L1("Alice"))
dict.get(L1("name"))        // Some(L1("Alice"))
dict.get(L1("age"))         // None
```

**Note**: These methods work on any `Lem[T]` but only return meaningful results on structures containing `Pair` elements. For non-dict structures, they return `false` or `None` respectively.


### Requirements

- **Scala**: 3.7.4+
- **Cats**: For `Traverse` typeclass and functional combinators
- **JVM/JS**: Cross-compiled for both platforms

### Module Structure

```
prelude/
├── shared/src/main/scala/kubrick/prelude/lem/
│   ├── core.scala          # Core types, atoms, Branch, smart constructors
│   ├── TraverseOps.scala   # Cats Traverse instances
│   ├── all.scala           # Unified exports
│   └── [legacy files]      # Backward compatibility
└── jvm/src/test/scala/kubrick/prelude/
    └── lemTest.scala       # Comprehensive test suite
```

### Further Reading

For implementation details, see:
- [core.scala](shared/src/main/scala/kubrick/prelude/lem/core.scala) - Type definitions and core logic
- [TraverseOps.scala](shared/src/main/scala/kubrick/prelude/lem/TraverseOps.scala) - Functional programming support
- [lemTest.scala](jvm/src/test/scala/kubrick/prelude/lemTest.scala) - Usage examples and test cases
