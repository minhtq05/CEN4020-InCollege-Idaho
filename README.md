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

In the dev container terminal (`` Ctrl+` ``):

```sh
cobc -x -o HelloWorld HelloWorld.cbl
./HelloWorld
```

Expected output:

```
Hello, World!
```

The `-x` flag produces an executable. Without it, `cobc` builds a shared module (`.so`) that cannot be run directly.
