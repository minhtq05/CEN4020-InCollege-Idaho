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

`InCollege.cob` is the alpha console application: account registration, login, and initial
post-login navigation. All input is read from a file and every line shown on screen is also
written to an output file, so the two are always identical.

### Compile

```sh
cobc -x -o InCollege InCollege.cob
```

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

This reads `InCollege-Input.txt`, prints every prompt/message to the screen, and writes the same
lines to `InCollege-Output.txt` (created/overwritten in the current directory). A sample expected
output for the committed input file is at [`InCollege-Output.txt`](InCollege-Output.txt).

### Account persistence

Created accounts are saved to `InCollege-Accounts.txt` (also in the current directory) and are
loaded back in on the next run, so registered users can log in across separate executions without
re-registering. Delete this file to reset to zero accounts.
