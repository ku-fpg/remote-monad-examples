This repo contains the code examples used in our remote monad paper.
Each example is in a directory containing a main that can be built using
the included remote-monad-example.cabal file.

Example | Desciption                           | Regular Experession of Communication
-------|---------------------------------------|-------
ASync  | send without reply                    | `Command`
Sync   | send and wait for reply               | `Procedure`
Weak   | send primitives piecemeal             | `Command|Procedure`
Strong | send packet                           | `Command+ | (Command* Procedure)`
Applicative | send all commands and procedures | `(Command|Procedure)+`
Deep   | Procedures are embedded as commands   | `Command* (Command|Procedure)`

`Remote` is a monad, which provides `Command`, `Procedure`, and
the monadic, applicative and functor operations. In the Deep packet,
the `Procedure` is simply a `Reply` constructor. In the other examples,
`Command` and `Procedure` are:

    data Command = Say String
    data Procedure :: * -> * where
      Temperature ::        Procedure Int
      Toast       :: Int -> Procedure ()

As is done in the remote monad paper, we include use non-GADT versions of
our structures to simulate remote exectution. 
We have GADT versions of the remote intepreters; just ask.









