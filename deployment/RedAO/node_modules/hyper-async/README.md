# hyper-async

Async is a tiny FP library for javascript

## install

```sh
npm install hyper-async
```

## Usage

```js
import Async from 'hyper-async'

async function main() {
  const x = await Async.of(1)
    .chain(x => Async.fromPromise(fetch)('https://jsonplaceholder.typicode.com/posts/' + x))
    .chain(res => Async.fromPromise(res.json.bind(res))())
    .map(prop('title'))
    .toPromise()
  console.log(x)
})

main()
```

