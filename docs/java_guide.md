# Java Development Guide in Neovim

A lightweight, on-demand Java environment designed for both standalone algorithmic/competitive programming and full project development (Eclipse, Maven, Gradle).

---

## ⚡ Core Philosophy: Zero-Overhead On-Demand

By default, Java tooling introduces **no background daemons, no language server memory overhead, and no startup lag**:
- Syntax highlighting and fast compilation/running work out of the box.
- Curated, bespoke snippets are available immediately via LuaSnip.
- Heavy IDE features (Eclipse JDT Language Server and automated boilerplate generators) stay dormant until explicitly requested.

To initiate full IDE capabilities in any Java buffer:
- **Command**: `:JavaInit`
- **Keymap**: `<leader>ji`

---

## 🛠️ Build, Run & Layout Keymaps

The build and run commands automatically detect whether you are in an **Eclipse project** (`.classpath`, `.project`), **Maven/Gradle project**, or a **standalone file**, and resolve package paths and output directories (`bin/` or `build/`).

| Keymap / Command | Description |
| :--- | :--- |
| `<leader>m` | **Compile current file** (`javac`). Opens quickfix on errors; notifies on success. |
| `<leader>r` or `<F5>` | **Run current class** with input redirection (`< stem.input \| tee stem.output`). |
| `<leader>co` | **Toggle Compiler Output / Quickfix window**. |
| `:CP [name]` | **Competitive Programming Layout**: Opens 3 panes (`name.java`, `name.input`, `name.output`). |

### Automatic Project & Package Detection
When compiling or running:
1. **Package Statement**: Scans for `package <pkg>;` in the file.
2. **Root Detection**: Locates `.classpath`, `.project`, `pom.xml`, `build.gradle`, or walks package directory levels.
3. **Classpath & Sourcepath**:
   - Eclipse projects: Compiles with `-d <project>/bin -sourcepath <project>/src -cp <project>/bin`.
   - Standalone files: Compiles with `-d <dir>/build` and runs with `java -cp build <Class>`.
   - Correctly runs fully-qualified classes (e.g., `firstPackage.Pow`) without package mismatch errors.

---

## 🧩 Snippets Reference (`~/my/notebook/dots/vim-neosnippets/java.snippets`)

Snippets expand via **`<C-k>`** or autocomplete selection in `nvim-cmp`.

### 1. Competitive Programming & Core Algorithms
Parallels the algorithms in `cpp.snippets`:

| Trigger | Description |
| :--- | :--- |
| `comp` | Full Competitive Programming template (`FastScanner`, `PrintWriter`, testcase loop, debug, modular arithmetic, gcd) |
| `fastio` | High-performance standalone `FastScanner` (`BufferedReader` + `StringTokenizer`) |
| `debug` | Debug printer using `Arrays.deepToString()` to `System.err` |
| `print_matrix` | 2D matrix printer |
| `INF` | Standard infinity constants (`1e18`, `1e9`) |
| `MOD` | Modulo constants (`10^9+7`, `998244353`) |
| `rng` | `Random` number generator instance |
| `fenwick` | Fenwick Tree / Binary Indexed Tree (point update & range sum) |
| `segtree` | Segment Tree (point update & range queries) |
| `segtree_lazy` | Lazy Segment Tree (range update & range query) |
| `unionfind` / `dsu` | Disjoint Set Union with path compression and rank/size |
| `modpow` | Fast modular exponentiation and modular inverse |
| `sieve` | Sieve of Eratosthenes prime boolean array |
| `sparsetable` | Sparse Table for $O(1)$ Range Minimum Queries (RMQ) |
| `binary_jump` | Binary lifting for Lowest Common Ancestor (LCA) and tree distance |
| `sssp_djkstra` | Dijkstra's shortest path algorithm using `PriorityQueue` |
| `sssp_bellman_ford` | Bellman-Ford shortest path algorithm |
| `toposort` | Kahn's topological sort algorithm |
| `maxflow` | Dinic's Maximum Network Flow algorithm |
| `kmp` | Knuth-Morris-Pratt prefix function and string search |
| `trie` | Prefix tree (`insert`, `search`, `startsWith`) |
| `SCC` | Tarjan's Strongly Connected Components |

### 2. Java Language Boilerplate & Idioms

| Trigger | Description |
| :--- | :--- |
| `psvm` | `public static void main(String[] args)` |
| `main` | `main` method initialized with `FastScanner` and `PrintWriter` |
| `sout` | `System.out.println()` |
| `serr` | `System.err.println()` |
| `souf` | `System.out.printf()` |
| `fori` | Indexed loop (`for (int i = 0; i < n; i++)`) |
| `forr` | Reverse loop (`for (int i = n - 1; i >= 0; i--)`) |
| `foreach` / `iter` | Enhanced for-each loop (`for (var x : collection)`) |
| `pair` | Generic Comparable `Pair<U, V>` class with `compareTo` and `toString` |
| `pq` | `PriorityQueue` min-heap |
| `pqmax` | `PriorityQueue` max-heap (`Collections.reverseOrder()`) |
| `sort` | `Arrays.sort(arr)` |
| `sortcmp` | `Arrays.sort` with lambda comparator |
| `cls` | Standard class skeleton with constructor |
| `rec` | Java Record definition (`public record Name(...) {}`) |
| `inter` | Interface definition |
| `enum` | Enum definition |
| `builder` | Static Builder pattern boilerplate |
| `singleton` | Bill Pugh thread-safe Singleton pattern |
| `trycatch` | Standard `try { ... } catch (Exception e) { ... }` block |
| `tryres` | `try-with-resources` block |
| `hashcode_equals` | `equals()` and `hashCode()` method overrides |
| `tostring` | `toString()` method override |
| `getset` | Getter and setter pair |

---

## 🤖 On-Demand IDE Boilerplate Generation (`:JavaInit` / `<leader>ji`)

When `:JavaInit` or `<leader>ji` is executed, `nvim-jdtls` starts and binds buffer-local keymaps for automated code generation:

| Keymap | Action |
| :--- | :--- |
| `<leader>jgs` | **Generate Getters & Setters** for fields |
| `<leader>jc` | **Generate Constructor** using fields |
| `<leader>jts` | **Generate `toString()`** |
| `<leader>jeq` | **Generate `equals()` & `hashCode()`** |
| `<leader>joi` | **Organize & clean imports** |
| `<leader>jev` | **Extract Variable** (normal & visual mode) |
| `<leader>jec` | **Extract Constant** (normal & visual mode) |
| `<leader>jem` | **Extract Method** (visual mode) |
