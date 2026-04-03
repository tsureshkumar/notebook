# The Ultimate GTD & Org Mode Workflow Guide

This guide details a unified, high-performance system for managing life and technical work using the Getting Things Done (GTD) methodology within Org Mode.

---

## Part 1: General Productive Workflow (The 5 Pillars)

### The GTD Process Flow
```text
                                 [ INPUT: STUFF ]
                                        |
                                 Is it Actionable?
                                 /              \
                         [ NO ]                  [ YES ]
                           |                        |
             +-------------+------------+    What is the Next Action?
             |             |            |           |
         [ TRASH ]    [ INCUBATE ]  [ REFERENCE ]   |
                       (Someday)                    |
                                        +-----------+-----------+
                                        |           |           |
                                  [ DO IT ]    [ DELEGATE ] [ DEFER ]
                                  (< 2 min)    (Waiting For)    |
                                                                |
                                                 +--------------+--------------+
                                                 |                             |
                                           [ NEXT ACTIONS ]              [ PROJECTS ]
                                           (Context Lists)             (Multi-step Plans)
                                                 |
                                           [ TICKLER ]
                                        (Future Reminders)
```

For anyone looking to clear their mind and stay organized, the workflow follows David Allen's five stages of GTD, mapped directly to Org Mode features.

### 1. Capture (Clear Your Head)
**Goal:** Collect everything that has your attention into an "Inbox" so you don't have to remember it.
- **Action:** Use `SPC X` (Emacs) or `<leader>gc` (Neovim) to trigger a capture template.
- **Implementation:** All new thoughts go to `0-Inbox/inbox.org`. Don't worry about formatting; just get the "what" down.
- **Tip:** If it takes less than 2 minutes, just do it. Otherwise, capture it.

### 2. Clarify (Process the Inbox)
**Goal:** Decide what each captured item is and what to do with it.
- **Action:** Use `<leader>gp` (Neovim) or open `inbox.org` manually.
- **Questions to Ask:**
    - Is it actionable? If no: Trash it, Incubate it (`someday-maybe.org`), or File it (Reference).
    - What is the *Next Action*? If it's a multi-step task, it's a **Project**.
- **Org Tip:** Use `M-S-RET` to quickly add a TODO state to an item.

### 3. Organize (Put Everything in its Place)
**Goal:** Park reminders of your actions in the right lists.
- **Next Actions:** Move to `GTD-System/next-actions.org`. These are your "do ASAP" items.
- **Waiting For:** Move to `GTD-System/waiting-for.org`. Tasks you've delegated.
- **Tickler:** Move to `GTD-System/tickler.org`. Future reminders (e.g., "Renew passport in 6 months").
- **Projects:** Mark the top-level heading with a `:PROJECT:` tag.

#### Deep Dive: What is a Tickler?
A **Tickler** file (traditionally "43 folders") is a system to "tickle" your memory at a future date. It is the bridge between "Someday/Maybe" and "Next Actions."
- **Definition:** Items that you don't want to see, think about, or be distracted by until a specific day in the future.
- **Examples:** Follow up on an invoice next Tuesday; check for a package delivery on Friday; remind yourself of a concert ticket pre-sale in 3 months.
- **Org Mode Implementation:** Use the `T` capture template (`SPC X T` or `<leader>gt`). Always add a `SCHEDULED` timestamp. The item will "disappear" and only resurface in your **Agenda** on the day you need to see it.

### 4. Reflect (The Weekly, Monthly, and Quarterly Review)
**Goal:** Keep the system "clean, clear, and current" across all horizons.
- **Weekly Review (W):** Focus on the next 7 days. Clear inboxes and update "Next Actions."
- **Monthly Review (M):** Focus on broader goals and deadlines for the month. 
- **Quarterly Review (Q):** High-level alignment. Check if projects align with your 3-month objectives.
- **Project Health Checks:**
    - **Recently Added (r):** Review newly captured projects to ensure they are clarified.
    - **Empty Projects (E):** Find projects that have no active tasks—these are usually projects that have stalled or need new actions defined.
    - **Stuck Projects (S) [Emacs]:** Find projects that are missing a `NEXT` action. In GTD, every project should have at least one defined physical next step.

### 5. Engage (Just Do It)
**Goal:** Choose your best action based on Context, Time, and Energy.
- **Action:** Use the **Daily Action List** (`C-c a D` or `<leader>oa D`) to focus only on today's tasks and deadlines.
- **Contexts:** Use tags like `@home`, `@office`, or `@computer` to filter tasks based on where you are.

---

## Part 2: Workflow for Developers & Software Architects

For technical professionals, Org Mode isn't just a TODO list—it's a technical workbench and a "Second Brain."

### 1. Architecting with Subtrees
Instead of flat lists, use Org's hierarchical structure to map system architectures.
- **Example:**
    * TODO [Project] Implement Auth Service
    ** TODO Design Database Schema :ARCH:
    ** TODO Implement JWT Logic :CODE:
    ** TODO Write Integration Tests :TEST:
- **Architect's Tip:** Use **Properties** to store metadata like Jira IDs, RFC links, or PR status.

### 2. Literate Engineering & Documentation
Org Mode allows you to mix executable code blocks with rich text.
- **Workflow:** Use code blocks (`#+BEGIN_SRC`) to document complex CLI commands, SQL queries, or API calls you use during development.
- **Benefit:** You can "tangle" (export) these blocks into source files or execute them directly to test assumptions.

### 3. Technical Research & Second Brain (Org-Roam)
As an architect, you deal with complex decisions (ADRs). Use a [Zettelkasten method](#appendix-the-zettelkasten-method) via `org-roam`.
- **Action:** Use `SPC n s` to search or create permanent notes for architectural patterns, obscure bugs, or library research.
- **Linking:** Link your GTD tasks to your technical notes. "TODO Fix Bug X" should link to the note where you've analyzed the root cause.

### 4. Deep Work & Time Tracking
Developers often struggle with "where did the day go?"
- **Action:** Clock-in (`C-c C-x C-i` or `<leader>oi`) when you start a task and Clock-out (`<leader>oo`) when you finish.
- **Benefit:** Use Org's clock reports to see exactly how much time was spent on "Meetings" vs. "Feature Dev."

### 5. Handling "Context Switching" Interruptions
When a critical bug or a "quick question" interrupts your flow:
1. **Quick Capture:** Use `<leader>gc` to note the interruption.
2. **Clock Out:** Org will stop the timer on your current deep work.
3. **Engage Interruption:** Do the task.
4. **Clock Back In:** Use `C-c C-x C-j` (Emacs) to jump back to your last clocked task and resume instantly.

---

## Summary of the "Golden Rule"
**If it's not in your Org files, it doesn't exist.** 

Trusting your system allows you to achieve "Mind Like Water," where your brain is used for *creating* and *architecting*, not for *remembering* TODOs.

---

## Appendix: The Zettelkasten Method

The **Zettelkasten** (slip-box) method is a system for knowledge management and creative thinking. It focuses on making connections between individual "atoms" of information.

### Core Principles:
1. **Atomicity:** Each note should contain one idea only.
2. **Connectivity:** Notes are useless in isolation; they must link to other notes.
3. **No Hierarchy:** Let the structure emerge organically through links rather than predefined folders.

### Org Mode Implementation (Org-Roam):
- Use **Backlinks** to see what other notes refer to your current idea.
- Use **Daily Notes** for fleeting thoughts that later get processed into permanent Zettels.
- **Goal:** Move from "collecting information" to "connecting information."
