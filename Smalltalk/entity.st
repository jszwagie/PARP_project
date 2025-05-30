Object subclass: Entity [
    | name description takeable |

    Entity class >> itemNamed: aName description: aDesc takeable: aBool [
        ^ self new
            name: aName;
            description: aDesc;
            takeable: aBool;
            yourself
    ]

    Entity class >> personNamed: aName description: aDesc [
        ^ self itemNamed: aName description: aDesc takeable: false
    ]

    name [ ^ name ]
    name: aString [ name := aString ]
    description [ ^ description ]
    description: aString [ description := aString ]
    takeable [ ^ takeable ]
    takeable: aBoolean [ takeable := aBoolean ]
    printOn: aStream [ aStream nextPutAll: name ]
] !