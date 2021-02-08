This repo contains MBSA domain specific language. It is an embedded DSL. Dotty (Scala 3) is the host language.

# Running Dotty code on your machine
To run this project, you'll need sbt installed on your machine.
Then you can view the code in VS Code by running
```
cd dsl
sbt launchIDE
```

# DSL use
The DSL documentation is available for principles of the language and how to use it in terms of modelling and analysis.
The whole code is available in this repo and some model examples are available in the tests.
To run one of them use `test:run` in the sbt interactive shell.
You will then have the choice between available test main files. Check the corresponding files to see the model and what analysis is launched when running the test.
