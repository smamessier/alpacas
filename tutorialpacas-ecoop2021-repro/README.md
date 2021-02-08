Alpacas ECOOP 2021 reproductibility package.

## sbt project compiled with Dotty

### Usage

Install a java runtime v8 or v11 or graalvm using using https://sdkman.io or the method of your choice

Install the sbt scala build tool using https://sdkman.io 

  - RunningExample.scala contains the running example defined in section 4 and used to illustrate section 5
  - DesignSpaceExploration.scala contains the design space exploration example of section 7
  - BDDReliability.scala contains library code allowing to compute unreliability from minimal cutsets or sequences

This is a normal sbt project, you can compile code with `sbt compile` and run it
with `sbt run`, `sbt console` will start a Dotty REPL.

`sbt "runMain tutorialpacas.runningexample.RunningExample"` reproduces results of tables 1 and 2

`sbt "runMain tutorialpacas.dse.DesignSpaceExploration"` reproduces results of table 3


For more information on the sbt-dotty plugin, see the
	[dotty-example-project](https://github.com/lampepfl/dotty-example-project/blob/master/README.md).
