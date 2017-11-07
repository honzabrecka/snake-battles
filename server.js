const http = require('http')
const express = require('express')
const WebSocket = require('ws')
const uuid = require('uuid')

const snake = require('./output/Main')

const app = express()

const minPlayers = 2

// { id :: UUID, ws :: WebSocketClient, gameId :: Maybe UUID }
const players = {}
// { gameId :: UUID, playerIds :: Array UUID, game :: GAME }
const games = {}

app.use(express.static('public'))

app.use(function (req, res) {
  res.send({ msg: "hello" });
});

const server = http.createServer(app)
const wss = new WebSocket.Server({ server })

const STATE = {
  WAITING: 0,
  CONNECTED_PREPARE: 1,
  CONNECTED_PLAYING: 2,
  CONNECTED_END: 3,
  DESTROYED: 8
}

// WebSocketClient -> Eff (eff) UUID
function addPlayer(ws) {
  const id = uuid.v4()
  players[id] = { id, ws, gameId: null }
  console.log('addPlayer :: ', Object.keys(players).length)
  return id
}

// String -> Eff (eff) (Maybe UUID)
function removePlayer(id) {
  const { gameId } = players[id]
  delete players[id]
  console.log('removePlayer :: ', Object.keys(players).length)
  return gameId
}

// UUID -> Eff (eff) Unit
function createGame(playerId) {
  const minWaitingPlayers = minPlayers - 1
  const waitingPlayers = Object.values(players).filter(({ id, gameId }) => gameId === null && id !== playerId)

  if (waitingPlayers.length >= minWaitingPlayers) {
    const gameId = uuid.v4()
    const playerIds = [
      playerId,
      ...waitingPlayers.slice(0, minWaitingPlayers).map(({ id }) => id)
    ]

    const game = snake.initGame(50)(50)(2)()

    games[gameId] = {
      gameId,
      playerIds,
      game
    }

    playerIds.forEach((id) => {
      players[id] = { ...players[id], gameId }
    })
    sendGame(playerIds, game)
  } else {
    // waiting
  }
}

// UUID -> Eff (eff) Unit
function destroyGame(gameId) {
  const { playerIds } = games[gameId]
  delete games[gameId]

  playerIds.forEach((id) => {
    if (!players[id]) return

    players[id] = { ...players[id], gameId: null }
    players[id].ws.send(STATE.DESTROYED)
  })
}

wss.on('connection', (ws) => {
  ws.binaryType = 'arraybuffer'

  const id = addPlayer(ws)
  createGame(id)
  console.log('connect :: ', Object.keys(players).length)

  ws.on('message', (data) => {
    const [tick, code] = new Uint16Array(data)
    const { gameId } = players[id]

    if (!games[gameId]) return

    if (new Set([0, 1, 2, 3]).has(code)) {
      games[gameId] = {
        ...games[gameId],
        game: snake.updateDirection
          (snake.codeToDirection(code))
          (games[gameId].playerIds.findIndex(($id) => $id === id))
          (tick)
          (games[gameId].game)
      }
    }

    if (code === 4) {
      games[gameId] = {
        ...games[gameId],
        game: snake.switchPause(id)(tick)(games[gameId].game)
      }
    }
  })

  ws.on('close', () => {
    const gameId = removePlayer(id)
    if (gameId) {
      destroyGame(gameId)
      // TODO createGame ??
    }
    console.log('close :: ', Object.keys(players).length)
  })
})

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

    for (i = 0; i < rest.length; i++) {
      const [direction, length] = rest[i]
      encoded[x++] = direction << 14 | length
    }
  })

  return encoded
}

function sendGame(playerIds, game) {
  const { header, snakes } = snake.encode(game)
  playerIds.forEach((id) => {
    players[id].ws.send(encode(header, snakes).buffer, { binary: true })
  })
}

server.listen(process.env.PORT || 8080, () => {
  console.log('Listening on %d', server.address().port);

  setInterval(() => {
    Object.values(games).forEach(({ gameId, game, playerIds }) => {
      const updatedGame = snake.tick(game)()
      games[gameId] = { ...games[gameId], game: updatedGame }
      sendGame(playerIds, updatedGame)
    })
  }, 100)
})
