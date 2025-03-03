Structured Headers parser for Javascript
========================================

This library implements a parser and serializer for the [Structured Field Values for HTTP][2]
specification. ([RFC9651][3], [RFC8941][1]).

This specification defines a standard serialization for complex HTTP header
values, including lists (arrays), dictionaries (maps) and also numbers,
booleans, binary data, timestamps and Unicode strings.

The library is written in Typescript, and the examples in this document are
too, but plain Javascript is also fully supported. It ships with ESM and
CommonJS builds and has 0 dependencies.

Compatibility
-------------

This package has 2805 unittests, the vast majority are supplied from the
official [HTTP WG test suite][2].

However, there are 2 differences in the serializer:

1. Javascript can't differentiate between `1.0` and `1`. As a result we're
   skipping the tests that require a serialiation output of `1.0`.
2. Javascript rounds slightly different from the spec. The tests suggest that
   `0.0025` should round to the nearest event number (`0.002`), but Javascript
   rounds to `0.003`.

No fix is planned for #1, because there's no reasonably way to fix this
without wrapping every number in a custom class, and this will negatively
impact the developer experience. We do intend to fix #2 in the future with a
custom rounding algorithm.

This library emits and expects the _exact_ data structures as they are
suggested by the RFC. The result of this is that the returned types can be
a bit complex.

In the future we intend to loosen the required types for the serializer, and
add new helper functions that give you simpler structures _if_ you don't need
certain features for a header (such as `Parameters`).

Let us know what you would like to see here!

Installation
------------

Using npm:

```
npm install structured-headers
```

API
---

### Parsing an item

The following are examples of `item` headers:

Parsed as string

```
# Parsed an ASCII string
Header: "foo"

# A simple string, called a 'Token' in the spec
Header: foo

# A Unicode string, called a 'Display String' in the spec. They use
# percent encoding, but encode a different set of characters than
# URLs.
Header %"Frysl%C3%A2n"

# Parsed as number
Header: 5
Header: -10
Header: 5.01415

# Parsed into boolean
Header: ?1
Header: ?0

# Binaries are base64 encoded
Header: :RE0gbWUgZm9yIGEgZnJlZSBjb29raWU=:

# Items can have parameters
Header: "Hello world"; a="5"

# Parsed into a Date object
Header: @1686634251
```

To parse these header values, use the `parseItem`:

```typescript
import { parseItem } from 'structured-headers';

console.log(
  parseItem(header)
);
```

parseItem returns a tuple (array with 2 items), the first item is the value,
the second is a `Map` object with parameters.

The type is roughly:

```typescript
// The raw value
type BareItem = number | string | Token | ByteSequence | boolean | Date | DisplayString;

// The return type of parseItem
type Item = [
  BareItem,
  Map<string, BareItem>
];
```

### Parsing a list

A list is an array of items. Some examples:

```
# A simple list
Header: 5, "foo", bar, ?1

# Each element can have parameters
Header: sometoken; param1; param2=hi, 42

# A list can also contain lists itself. These are called 'inner lists' and
# use parenthesis
Header: sometoken, (innerlistitem1 innerlistitem2), (anotherlist)
```


To parse these:

```typescript
import { parseList } from 'structured-headers';

console.log(
  parseList(header)
);
```

`parseList` returns an array with each member. The return type is:

```typescript
type InnerList = [Item[], Parameters];
type List = (InnerList|Item)[];
```

### Parsing a dictionary

A dictionary is a key->value object. Examples:

```
# A simple dictionary
Header: fn="evert", ln="pot", coffee=?1

# Each item may have parameters too
Header: foo=123; q=1, bar=123, q=0.5

# A dictionary value may be an inner list again
Header: foo=(1 2 3)
```

To parse dictionaries:

```typescript
import { parseDictionary } from 'structured-headers';

console.log(
  parseDictionary(header)
);
```

The return type for `parseDictionary` is a `Map`.

```typescript
type Dictionary = Map<string, Item|InnerList>;
```


### Serializing

The serialiser functions work the exact same way, but in opposite direction.
They all return strings.

Currently the serializes expect the *exact type* that the parsers return, but
the intention is to loosen the types for serialization, so it's a bit more
ergnomic to call. Want this? Let me know by opening an issue.


```javascript
import {
  serializeDictionary,
  serializeList,
  serializeItem
} from 'structured-headers';

// Returns "foo", "bar"
serializeList([
  ['foo', new Map()],
  ['bar', new Map()],
]);

// Returns a=1, b=?0
sh.serializeDictionary({
  a: 1,
  b: false,
});

// Returns 42
serializeItem(42);

// Returns 5.5
serializeItem(5.5);

// Returns "hello world"
serializeItem("hello world");

// Returns %"Frysl%C3%A2n"
serializeItem("Fryslân");

// Returns ?1
serializeItem(true);

// Returns a base-64 representation like: *aGVsbG8=*
serializeItem(new ByteSequence('aGVsbG8='));

// Returns a unix timestamp
serializeItem(new Date());

// Parameters to items can be passed as the second argument
// Returns "hello", q=5
serializeItem(
  "hello",
  new Map(['q', 5])
);
```

Browser support
---------------

There is a minified version of this library in the `browser/` directory. This minified
file will expose a global variable called 'structuredHeader' which contains the rest
of the api.


[1]: https://datatracker.ietf.org/doc/html/rfc8941
[2]: https://github.com/httpwg/structured-field-tests
[3]: https://www.rfc-editor.org/rfc/rfc9651.html
