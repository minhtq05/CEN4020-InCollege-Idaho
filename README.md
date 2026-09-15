# CEN4020-repo

COBOL project environment for Software Engineering (CEN4020).

## Setup: open in a dev container

1. Install [Docker Desktop](https://www.docker.com/products/docker-desktop/) and start it.
2. In VS Code, install the **Dev Containers** extension (`ms-vscode-remote.remote-containers`).
3. Press `Cmd+Shift+P` (macOS) or `Ctrl+Shift+P` (Windows/Linux) and run **Dev Containers: Reopen in Container**.

The first build takes about 3–5 minutes: VS Code pulls an Ubuntu 22.04 image, installs GnuCOBOL and build tools, and sets up the COBOL extensions. Reopening later takes seconds.

You'll know it worked when the bottom-left corner of VS Code shows **Dev Container: Software Engineering - COBOL Project Environment**. Verify the compiler with:

```sh
cobc --version
```

To return to your local machine: `Cmd+Shift+P` → **Dev Containers: Reopen Folder Locally**.

## Compile and run

### Option 1: the ▷ Run button

Open a `.cbl` file (e.g. `HelloWorld.cbl`) and click the **▷ Run Code** button in the editor's top-right corner. The Code Runner extension (installed automatically by the dev container) compiles the file with GnuCOBOL and runs the executable in the terminal.

### Option 2: the terminal

In the dev container terminal (`` Ctrl+` ``):

```sh
cobc -x -o HelloWorld HelloWorld.cbl
./HelloWorld
```

Expected output (either way):

```
Hello, World!
```

The `-x` flag produces an executable. Without it, `cobc` builds a shared module (`.so`) that cannot be run directly.

## InCollege (Epic #1: Log In, Part 1)

The InCollege console application handles account registration, login, and post-login
navigation (profiles, skills, and more — see Epic #2 below). All input is read from a file and
every line shown on screen is also written to an output file, so the two are always identical.

### Module layout

The program is split into a main program, copybooks (`.cpy`, shared data layouts included via
`COPY`), and CALL-based subprogram modules (`.cob`, one or more `PROGRAM-ID`s each), so no single
file is a monolith:

| File | Contains |
|---|---|
| `InCollege.cob` | Main program: top-level and post-login menu orchestration only. |
| `InCollege-IO.cob` | `WRITE-LINE` / `READ-LINE` — shared screen+file echo I/O. |
| `InCollege-Account.cob` | `ACCOUNT-LOAD` / `ACCOUNT-CREATE` / `ACCOUNT-LOGIN`. |
| `InCollege-Profile.cob` | `PROFILE-LOAD` / `PROFILE-SAVE` / `PROFILE-EDIT`. |
| `InCollege-ProfileEntries.cob` | `PROFILE-EXPERIENCE` / `PROFILE-EDUCATION` (repeatable entries). |
| `InCollege-ProfileView.cob` | `PROFILE-VIEW`. |
| `InCollege-Skills.cob` | `SKILL-MENU`. |
| `InCollege-*.cpy` | Shared record layouts and `EXTERNAL` session/table data, included via `COPY` where more than one program needs the same shape. |

### Compile

```sh
make
```

This compiles all the `.cob` modules above together into the `InCollege` executable (see
`Makefile`). `make run` builds and runs it in one step; `make clean` removes the built binary.

Because the program now spans multiple files, the single-file "▷ Run Code" button / Code Runner
mapping only compiles whichever file is open and will not produce a working build — use `make` /
`make run` instead. (Code Runner is still fine for standalone single-file programs like
`HelloWorld.cbl`.)

### Prepare the input file

Create `InCollege-Input.txt` in the same directory as the executable, with one input value per
line, in the exact order the program will ask for it (menu choices, usernames, passwords). A
sample walkthrough is committed at [`InCollege-Input.txt`](InCollege-Input.txt) — it creates an
account, fails a login once, logs in successfully, visits every post-login menu option, and logs
out.

Menu choices are entered as numbers (`1`/`2` at the top level, `1`-`4` after logging in, `1`-`5`
plus `6` for "Go Back" in the skills menu). Passwords must be 8-12 characters with at least one
uppercase letter, one digit, and one special character.

### Run

```sh
./InCollege
```

(or `make run` to build and run in one step)

This reads `InCollege-Input.txt`, prints every prompt/message to the screen, and writes the same
lines to `InCollege-Output.txt` (created/overwritten in the current directory). A sample expected
output for the committed input file is at [`InCollege-Output.txt`](InCollege-Output.txt).

### Account persistence

Created accounts are saved to `InCollege-Accounts.txt` (also in the current directory) and are
loaded back in on the next run, so registered users can log in across separate executions without
re-registering. Delete this file to reset to zero accounts.

## InCollege (Epic #2: User Profile Creation, Part 1)

Logged-in users can now create and view a personal profile from the post-login menu:

```
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
6. Logout
```

### Preparing input for profile creation

Choosing **1** walks through the profile prompts, one input line per prompt, in this order:
First Name, Last Name, University/College Attended, Major, Graduation Year, About Me (optional —
send a blank line to skip). Graduation Year must be a 4-digit number greater than 2025 and less
than 2034; any required field left blank aborts the save with an error message and returns to the
post-login menu without writing anything.

After About Me, the program prompts for up to 3 work experience entries and then up to 3
education entries. For each, it shows an "Add ... (Enter 'DONE' to finish)" prompt — supply the
line `DONE` to stop adding entries for that section, or any other line to add one more (that line
itself is just the "continue" signal and is not used as data). Each experience entry then reads
Title, Company/Organization, Dates, and an optional Description (blank to skip); each education
entry reads Degree, University/College, and Years Attended. All four/three fields in an entry
(other than Description) are required once you've chosen to add that entry.

Choosing **2** displays the current user's saved profile, or a message that no profile has been
created yet.

[`InCollege-Input.txt`](InCollege-Input.txt) extends the Epic #1 walkthrough with a full profile
creation and a profile view; the matching expected output is in
[`InCollege-Output.txt`](InCollege-Output.txt).

### Profile persistence

Profiles are saved to `InCollege-Profiles.txt` (created/overwritten in the current directory),
one record per profile keyed by username, and reloaded on the next run so a profile persists
across restarts just like accounts do.
