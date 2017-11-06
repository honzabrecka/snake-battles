const snake = require('./output/Main')

const game = snake.initGame(10)(10)(1)()

console.log(game)

console.log(snake.tick(game)())
