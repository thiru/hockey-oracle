# Hockey Oracle

## Overview

Intended to be a general management tool for pick-up hockey, or amateur leagues.

**Note**: currently, it's only possible to generate teams by randomly selecting from a pool of players.

## Development

### Prerequisites

1. A modern implementation of Common Lisp such as [SBCL](http://www.sbcl.org/)
  * I've only been testing with SBCL but I believe the code is portable
2. [Quicklisp](http://www.quicklisp.org/)
3. [Glu](https://github.com/thiru/glu)
  * This is a small (opinionated) utility project I maintain that's not available through Quicklisp (and is not intended to be)

### Running the website

1. Open a Common Lisp REPL
2. Make sure this project and Glu are visible to Quicklisp (e.g. ~/quicklisp/local-projects)
2. `(ql:quickload :hockey-oracle)`
3. `(hockey-oracle.web:start-server! :port 9090)`
