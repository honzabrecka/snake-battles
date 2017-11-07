let state = {}

const connection = new WebSocket(window['__WS_URL__'])

connection.onopen = () => {
  console.log('open')
};

connection.onerror = (error) => {
  console.log('error', error)
}

connection.onmessage = (message) => {
  const loader = new FileReader();
  loader.onload = function (e) {
    state = decode(new Uint16Array(e.target.result))
    draw(state)
  };
  loader.readAsArrayBuffer(message.data)
}

window.onkeydown = (e) => {
  const code = {37: 0, 39: 1, 38: 2, 40: 3, 32: 4}[e.keyCode]
  if (code !== undefined) connection.send(JSON.stringify([state.tick, code]))
}

const canvas = document.getElementById('canvas')
const context = canvas.getContext('2d')

const s = 10
const w = 50
const h = 50

const add = ([x, y], [a, b]) => [x + a, y + b]

const directionToP = (d) => {
  if (d === 0) return [1, 0]
  if (d === 1) return [-1, 0]
  if (d === 2) return [0, 1]
  return [0, -1]
}

const inRange = ([x, y]) => {
  if (x < 0)   return [w - 1, y]
  if (x == w) return [0, y]
  if (y < 0)   return [x, h - 1]
  if (y == h) return [x, 0]
  return [x, y]
}

function drawPoint(context, live, color, [x, y]) {
  const colors = [
    '#0033cc',
    '#339966',
    '#800000',
    '#ff9900'
  ]

  context.beginPath()
  context.globalAlpha = live ? 1 : 0.2
  context.rect(x * s, y * s, s, s)
  context.fillStyle = colors[color]
  context.fill()
  context.globalAlpha = 1
}

function drawFood(context, [x, y]) {
  context.beginPath()
  context.rect(x * s, y * s, s, s)
  context.fillStyle = '#ff3300'
  context.fill()
}

function draw({ snakes, food }) {
  context.clearRect(0, 0, context.canvas.width, context.canvas.height)
  drawFood(context, food)
  snakes.forEach(((snake) => {
    let p
    const [[live, color], x, y, ...body] =  snake
    p = [x, y]

    drawPoint(context, live, color, p)

    body.forEach(([direction, length]) => {
      for (let i = 0; i < length; i++) {
        p = inRange(add(p, directionToP(direction)))
        drawPoint(context, live, color, p)
      }
    })
  }))
}

function decode(bytes) {
  const [state, tick, width, height, foodX, foodY, ...snakes] = bytes
  const decoded = []

  for (let i = 0, j = 0, s = [], prev = ''; i < snakes.length; i++) {
    if (j === 0) {
      const live = snakes[i] >> 15
      const index = snakes[i] ^ (live << 15)
      s.push([live, index])
      j++
    } else if (j === 1 || j === 2) {
      s.push(snakes[i])
      j++
    } else if (j > 2) {
      const direction = snakes[i] >> 14
      const length = snakes[i] ^ (direction << 14)
      const hash = direction + '' + length
      if (hash === prev) {
        prev = ''
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

  return {
    state,
    tick,
    width,
    height,
    food: [foodX, foodY],
    snakes: decoded
  }
}
