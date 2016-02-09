# Toy Robot Simulator in Haskell
Haskell solutions to the Toy Robot Simulator coding challenge

## Tooling
Since the program was built using [Haskell Stack](http://haskellstack.org), I'll
introduce it very briefly. For more information, please visit the documentation for Haskell Stack at http://docs.haskellstack.org/en/stable/index.html

### Installing on OS X Using Homebrew
Installing Haskell Stack with [brew](http://brew.sh/) is really simple. Just do:
```
brew install haskell-stack
```

### Creating a new project
After `stack` is installed, you can create a new project with `stack new`:
```
stack new project-name
```

### Project setup
The next step would be performing necessary setup for your new project:
```
cd project-name
stack setup
```

If you don't already have the compiler (default GHC) installed, this command will download and install it for you. The nice thing about `stack` is it will install GHC or other Haskell compiler in an isolated location (default ~/.stack), which won't interfere with any existing system-level installations.

### Build and Run
After the compiler is properly installed, you can build and run the project:
```
stack build
stack exec project-name-exe
```

## Usage
The program works exactly like its Ruby counterpart: it expects to receive commands from standard input and send the results to standard output. But before you could run the program, you need to compile it with `stack` in haskell directory:
```
cd haskell
stack build
```

If there wasn't any compilation error, test case 1 could be run with `stack exec` in haskell directory:
```
cat ../test_data/case1.log | stack exec toy-robot-exe
```
Or it could also be run with the pre-built executable `toy-robot`:
```
cat ../test_data/case1.log | ./toy-robot
```

## Testing
Run the tests in haskell directory with `stack`
```
cd haskell
stack test
```
