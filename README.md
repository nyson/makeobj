# MakeObj - A JSON Generator!

This is a small tool to generate JSON objects from definitions.

A definition should be in your `~/.makebobj/`, have `.defs` 
as a suffix and can look like this:
```
Name = /(Ha|Si|Poké)mon (Gunnar|Anders|Jesper|Saftar)sson/
```
or
```
Grade = 1 to 5
```

And can be invoked like this
```
nyson@jotaro:~/projects/makeobj$ makeobj Grade
3
```

For example, lets try out with a neat `"/.makeobj/School.defs`
```
Name = /(Ha|Si|Poké)mon (Gunnar|Anders|Jesper|Saftar)sson/
Grade = 1 to 5
Person = { name: Name }

Student = { name: Name
          , grade: Grade
          }

School = { teacher: Person
         , class: { 
             courseName: /(Philosoph|Letharg)y/,
             students: [Student]
           }
         }
```

We can generate an object using `makeobj -p Student` (-p is for prettyprinting)
```
nyson@jotaro:~/projects/makeobj$ makeobj -p Student
{
    "name": "Pokémon Saftarsson",
    "grade": 5
}
```

This section will be expanded in the future, with a better FAQ and some syntax specifications :D
