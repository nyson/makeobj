# MakeObj - A JSON Generator!

This is a small tool to generate JSON objects from definitions.

## Introduction
Let us start with what it can do!
```
$ makeobj -p "{students: [{grade: 1 to 5, name: /[A-Z][a-z]{3,5}/}]}"
{
    "students": [
        {
            "name": "Lzcl",
            "grade": 5
        },
        {
            "name": "Otfm",
            "grade": 5
        },
        {
            "name": "Ragtoo",
            "grade": 4
        },
        {
            "name": "Zbyipc",
            "grade": 4
        }
}
```

As you see, we gave a definition and it generated a JSON blob conforming to the generated form. 
It was neatly formatted because of the `-p` flag!

You can also create premade definitions by making a file in the folder `~/.makeobj/` with the extension 
`.defs`.

For instance, if we have a file called `~/.makeobj/Friends.defs`
```
Friend = /(Ha|Si|Poké)mon (Gunnar|Anders|Jesper|Saftar)sson/
```

We can generate an object using the below syntax
```
$ makeobj Friend
"Pokémon Gunnarsson"
```

This makes it easy for us to create complex structures:

`~/.makeobj/School.defs`
```
Name = /(Ha|Si|Poké)mon (Gunnar|Anders|Jesper|Saftar)sson/
Grade = 1 to 5
Person = { name: Name }

Student = { name: Name
          , grade: Grade
          }

School = { teacher: Person
         , class: 
           { courseName: /(Philosoph|Letharg)y/
           , students: list of Student
           }
         }
```

Let us try it out:
```
$ makeobj School
{"teacher":{"name":"Hamon Jespersson"},"class":{"courseName":"Lethargy","students":[{"name":"Pokémon Anderssson","grade":1},{"name":"Pokémon Anderssson","grade":2},{"name":"Hamon Saftarsson","grade":2},{"name":"Simon Gunnarsson","grade":4},{"name":"Simon Saftarsson","grade":3},{"name":"Simon Gunnarsson","grade":5},{"name":"Simon Jespersson","grade":2}]}}
```

## Syntax Specification
### Simple generators
#### Regexes
Syntax: `/Regular Expression/`

Example `/[a-z]{2}_[A-Z]{2}/`

Generates a string from a regular expression.

#### Type labels
Syntax: `TypeLabel`

Example: `Car`

Generates a symbol from a predefined definition.

#### Literals
Syntax: `Literal`

Example: `true`, `null`, `10.22`, `"Hagrid"`, `1985-10-25`

Constructs a literal in the current generator. Supports boolean, null, numbers, strings and ISO8601 datetimes.

### Lists
Syntax: `[Generator1, Generator2, ...]`

Example: `[Car, Car, "Hagrid", Car]`

Constructs a list with the given generators.

### Complex generators
#### Lists
Syntax: `list of Generator`, `Int of Generator`, `Int to Int of Generator`

Example: `[/(Harry|Hagrid) from (Hogwarts|a shed)/]`, `5 of 9 to 17`, `1 to 5 of /a+/`

Generates a list of elements from a generator.

#### Objects
Syntax: `{}`, `{field: Generator}`, `{fieldA: Generator, fieldB: Generator}`

Example: `{name: /(Harry|Hagrid)/, from: /(Hogwarts|a shed)/}`

Generates an JSON compatible object.

#### Ranges
Syntax: `Int to Int`, `Float to Float`, `ISO8601 to ISO8601`

Example: `1 to 5`, `0.0 to 1.0`, `2014-12-25 to 2018-12-26`

Generates a number from an inclusive range of symbols. Currently supports integers, floats and date ranges.


## Contribute
I'll happily accept any contributions to the project, even if it's just a feature request in an issue!
