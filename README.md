# elmi-to-json

```elm
module Lambda exposing (f)


f : a -> b -> a
f a _ =
    a
```

```json
[
  {
    "unions": {},
    "aliases": {},
    "types": {
      "f": {
        "annotation": {
          "lambda": [
            {
              "name": "a",
              "type": "Var"
            },
            {
              "name": "b",
              "type": "Var"
            },
            {
              "name": "a",
              "type": "Var"
            }
          ]
        },
        "vars": [
          "a",
          "b"
        ]
      }
    },
    "binops": {}
  }
]
```

## Usage

```shell
npm install -g elmi-to-json
elm make src/Main.elm
elmi-to-json
```

# TODO

- [ ] setup travis for generating bins
- [ ] setup npm


---- cleanup

```elm
module Union exposing (Person, foo)


foo : ( Int, Int, Int ) -> ( Int, Int )
foo ( a, b, _ ) =
    ( a, b )


type alias Person =
    { name : String, email : String }
```

```json
[
  {
    "unions": {},
    "aliases": {
      "Person": {
        "alias": {
          "name": null,
          "type": "Record",
          "fields": {
            "email": {
              "moduleName": {
                "module": "String",
                "package": "elm-lang/core"
              },
              "types": [],
              "name": "String",
              "type": "Type"
            },
            "name": {
              "moduleName": {
                "module": "String",
                "package": "elm-lang/core"
              },
              "types": [],
              "name": "String",
              "type": "Type"
            }
          }
        },
        "vars": []
      }
    },
    "types": {
      "foo": {
        "annotation": {
          "lambda": [
            {
              "_3": {
                "moduleName": {
                  "module": "Basics",
                  "package": "elm-lang/core"
                },
                "types": [],
                "name": "Int",
                "type": "Type"
              },
              "_2": {
                "moduleName": {
                  "module": "Basics",
                  "package": "elm-lang/core"
                },
                "types": [],
                "name": "Int",
                "type": "Type"
              },
              "type": "Tuple",
              "_1": {
                "moduleName": {
                  "module": "Basics",
                  "package": "elm-lang/core"
                },
                "types": [],
                "name": "Int",
                "type": "Type"
              }
            },
            {
              "_2": {
                "moduleName": {
                  "module": "Basics",
                  "package": "elm-lang/core"
                },
                "types": [],
                "name": "Int",
                "type": "Type"
              },
              "type": "Tuple",
              "_1": {
                "moduleName": {
                  "module": "Basics",
                  "package": "elm-lang/core"
                },
                "types": [],
                "name": "Int",
                "type": "Type"
              }
            }
          ]
        },
        "vars": []
      }
    },
    "binops": {}
  }
]
```
