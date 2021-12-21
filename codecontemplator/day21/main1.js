let x = 444356092776315
console.log(x)

function Die() {
    this.count = 0
    this.numRolls = 0
    this.roll = () => {
        this.count++
        this.numRolls++
        if (this.count > 100)  {
            this.count = 1
        }
        return this.count
    }
}

function Player(name, initialPosition) {
    this.name = name
    this.position = initialPosition
    this.score = 0
    this.moveForward = (steps) => {
        this.position = this.position + steps
        if (this.position > 10) {
            this.position = 1 + (this.position - 1) % 10
        }
        this.score += this.position
    }
    this.finished = () => this.score >= 1000
    this.status = () => {
        return { name: this.name, position: this.position, score: this.score }
    }
}

function step(player, die) {
    const v1 = die.roll()
    const v2 = die.roll()
    const v3 = die.roll()
    const v = v1 + v2 + v3
    player.moveForward(v) 
}

const die = new Die()
const player1 = new Player("player1", 5)
const player2 = new Player("player2", 10)
let winner = null
let looser = null
let stepCount = 0

while(++stepCount < 1000) {

    step(player1, die)    
    console.log(player1.status())
    if (player1.finished()) {
        winner = player1
        looser = player2
        break
    }

    step(player2, die)
    console.log(player2.status())
    if (player2.finished()) {
        winner = player2
        looser = player1
        break
    }    
}

console.log("winner", winner)
console.log("looser", looser)
console.log(die.numRolls * looser.score)

