const structure =
`{
  name: /Harry|Hagrid/,
  occupation: /(Former |)Wizard/,
  horriblePets: 1 to 2 of {
    species: /(Dragon|Rat|Human|Salad|Cat|Dog) and (Dragon|Rat|Human|Salad|Cat|Dog) chimera/,
    age: 0 to 1000,
    medianGrade: 0.0 to 5.0
  },
  birthday: 1965-01-01 to 2005-01-01,
  favouriteFood: Pancake
}`;

const defs =
`
Topping= /Maple syrup|Blueberry jam|Cream/
Pancake= {
  style: /American|French/,
  topping: Topping,
  secondTopping: Topping
}
`;

const genObj = {
  "favouriteFood": {
    "style": "French",
    "topping": "Blueberry jam",
    "secondTopping": "Maple syrup"
  },
  "birthday": "1966-02-11",
  "name": "Harry",
  "horriblePets": [
    {
      "species": "Human and Dog chimera",
      "age": 648,
      "medianGrade": 0.5163898086976143
    },
    {
      "species": "Rat and Salad chimera",
      "age": 584,
      "medianGrade": 4.749108613992912
    }
  ],
  "occupation": "Former Wizard",
  "rugged": true
};

export {structure, defs, genObj};
