const header = [0, 9, 10, 10]
const snakes = [
  [[1, 3], [6, 9], [2, 4], [1, 3], [1, 3]],
  [[1, 4], [2, 7], [1, 2], [1, 2]]
]

/*
[
  state: 0 - playing, ,
  tick,
  width,
  height,
  [live, index],       1 bit: live, rest: snake index
  x,
  y,
  [direction, length]  2 bits: direction, rest length
]
*/

function encode(header, snakes) {
  const encoded = new Uint16Array(header.length + snakes.reduce((r, s) => r + s.length + 1, 0))

  let i
  let x = 0

  for (i = 0; i < header.length; i++)
    encoded[x++] = header[i]

  snakes.forEach((snake) => {
    const [[live, index], [x_, y_], ...rest] = snake

    encoded[x++] = live << 15 | index
    encoded[x++] = x_
    encoded[x++] = y_
console.log(rest)
    for (i = 0; i < rest.length; i++) {
      const [direction, length] = rest[i]
      encoded[x++] = direction << 14 | length
    }
  })

  return encoded
}

function decode(bytes) {
  const [state, tick, width, height, ...snakes] = bytes
  const decoded = []

  console.log(snakes)

  for (let i = 0, j = 0, s = [], prev = ''; i < snakes.length; i++) {
    if (j === 0) {
      const live = snakes[i] >> 15
      const index = snakes[i] ^ (live << 15)
      s.push([live, index])
      j++
    } else if (j === 1 || j === 2) {
      console.log(snakes[i])
      s.push(snakes[i])
      j++
    } else if (j > 2) {
      const direction = snakes[i] >> 14
      const length = snakes[i] ^ (direction << 14)
      const hash = direction + '' + length
console.log('>>', direction, length)
console.log('--', hash, prev)
      if (hash === prev) {
        decoded.push(s)
        s = []
        j = 0
      } else {
        prev = hash
        s.push([direction, length])
        j++
      }
    }
  }

  return decoded
}

console.log(encode(header, snakes))
console.log(decode(encode(header, snakes)))
