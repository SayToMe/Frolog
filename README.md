# Frolog

This project can be used as a simple Prolog interpreter implementation in F#. Be aware of current experimental stage of development, that can occur API changes. The documentation is under construction too, so that you might better to see Frolog usage in measurement module.

In order to start using it you need:

    clone repository
    build
	reference Frolog.dll

For example to define new rule you need to reference next namespaces

    $ open Frolog
    $ open Frolog.DefineRule
    $ open Frolog.DefineRule.DefPublic
    $ open Frolog.DefineRule.DefPublicDirectOperators

To use Frolog interpreter mind that you need at least Search machine to execute queries and keep state about facts and rules, that were added to it.

    $ defFact (signf "myfact(1)")
    $ defCall (signf "myEqual(X, Y)") (signf "=(X, Y)")

There are control flow operators such as and, or, cut, not.
To use most of them it is suggested to use their operator notation:

    * (!) (string signature) -> defines a fact
    * (=>) (string signature) (string call) -> defines a simple call to another rule.
    * (|&) (rule) (string call) -> combines current rules body with another call continiously.
    * (|!) (rule) (bool cut result) -> cuts rule execution with true or false result.
    
## Maintainer(s)

- [@saytome](https://github.com/saytome)
