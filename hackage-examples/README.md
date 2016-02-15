This repo contains the examples using the Hackage remote-monad library 
There is really one example that is using different bundling strategies.

The Weak.hs is sending each Command and Procedure one at a time to
the 'remote' process, whereas the Strong sends bundles of Commands
with an ending (optional) Procedure. The Applicative example bundles
up Commands and multiple Procedures that are used in an applicative
context (no binds).

These examples are running on the same machine, but you can plug in
a function that sends these Commands and Procedures through the network
and returns the result. 
