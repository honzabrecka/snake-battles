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

    games[gameId] = {
      gameId,
      playerIds,
      game: snake.main()
    }

    playerIds.forEach((id) => {
      players[id] = { ...players[id], gameId }
      players[id].ws.send(STATE.CONNECTED_PREPARE)
    })
  } else {
    players[playerId].ws.send(STATE.WAITING)
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
  const id = addPlayer(ws)
  createGame(id)
  console.log('connect :: ', Object.keys(players).length)

  ws.on('message', (data) => {
    console.log('message :: ', data)

    //

    const { gameId } = players[id]

    if (gameId) {
      const { playerIds, game } = games[gameId]
      const index = playerIds.find((player) => player.id === id)

      games[gameId] = {
        ...games[gameId],
        game: snake.updateDirection(index)(data)(game)
      }

      console.log(games[gameId].game.snakes)
    }

    // action (key)
    // pause
    // resume
    // restart
    // (close)

    // wss.clients.forEach((client) => {
    //   if (client !== ws && client.readyState === WebSocket.OPEN) {
    //     client.send(data)
    //   }
    // })
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

server.listen(8080, () => {
  console.log('Listening on %d', server.address().port);

  setInterval(() => {
    Object.values(games).forEach(({ game, playerIds }) => {
      const encodedGame = snake.encode(game)
      playerIds.forEach((id) => {
        players[id].ws.send(encodedGame)
      })
    })
  }, 1000)
})
