# cl-markov-chains

A library for modeling Markov chains in Common Lisp.

It works with multiple data types and first- as well as *n*th-order chains.

## Usage examples

```lisp
(generate (analyze '(sunny rainy rainy sunny sunny rainy sunny sunny sunny sunny rainy rainy rainy) 1) 10)

-> (SUNNY SUNNY RAINY SUNNY SUNNY SUNNY RAINY RAINY RAINY RAINY)
```

```lisp
(generate (analyze "aaabcbbdeeeaafafcbeefabc" 1) 30)

-> "abeafcbeeaafaafcbdeeefafcbefaa"
```

## Todo

- documentation
- transition matrix representation
- model properties
- ...

## License

MIT

