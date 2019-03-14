let state = {}

const INFO = {
  CREATED: 0,
  DESTROYED: 9,
  GAME: 3
}

const colors = [
  '#0033cc',
  '#339966',
  '#800000',
  '#ff9900'
]
const bgColors = [
  '#A6E5FF',
  '#A0E89C',
  '#E89D9A',
  '#FFD9BB'
]

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
    const bytes = new Uint16Array(e.target.result)

    if (bytes[0] === INFO.GAME) {
      state = { ...state, ...decode(bytes) }
      draw(state)
    } else if (bytes[0] === INFO.CREATED) {
      console.log('>>', bytes[1])
      state.playerIndex = bytes[1]
      canvas.style.backgroundColor = bgColors[state.playerIndex]
    }
  };
  loader.readAsArrayBuffer(message.data)
}

window.onkeydown = (e) => {
  const code = {37: 0, 39: 1, 38: 2, 40: 3, 32: 4}[e.keyCode]
  if (code !== undefined) {
    const bytes = new Uint16Array(2)
    bytes[0] = state.tick
    bytes[1] = state.state === 3 ? code + 1 : code
    connection.send(bytes)
  }
}

const canvas = document.getElementById('canvas')
const context = canvas.getContext('2d')

let s = 10
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
  const [_, state, tick, width, height, foodX, foodY, ...snakes] = bytes
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

function resize() {
  const bw = window.innerWidth
  const bh = window.innerHeight
  const d = bw > bh ? bh : bw
  const rd = d - (d % 10)
  s = Math.floor((rd / 500) * 10)
  canvas.width = s * 50
  canvas.height = s * 50
  if (state.food && state.snakes) draw(state)
}

window.addEventListener('resize', resize, false)
resize()

document.addEventListener('touchstart', handleTouchStart);
document.addEventListener('touchmove', handleTouchMove);

let xDown = null
let yDown = null

function getTouches(evt) {
  return evt.touches
}

function handleTouchStart(evt) {
  const firstTouch = getTouches(evt)[0]
  xDown = firstTouch.clientX
  yDown = firstTouch.clientY
  // evt.preventDefault()
}

function handleTouchMove(evt) {
  console.log('move')

  if (!xDown || !yDown) return

  const xUp = evt.touches[0].clientX
  const yUp = evt.touches[0].clientY
  const xDiff = xDown - xUp
  const yDiff = yDown - yUp
  let code = null

  if (Math.abs(xDiff) > Math.abs(yDiff)) {
    if (xDiff > 0) {
      // left
      code = 0
    } else {
      // right
      code = 1
    }
  } else {
    if (yDiff > 0) {
      // up
      code = 2
    } else {
      // down
      code = 3
    }
  }

  const bytes = new Uint16Array(2)
  bytes[0] = state.tick
  bytes[1] = state.state === 3 ? code + 1 : code
  connection.send(bytes)

  // xDown = null
  // yDown = null

  evt.preventDefault()
}
