var canvas = document.getElementById('canvas')
var ctx = canvas.getContext('2d')
var hue = 0

var mouse = {
  x: 0,
  y: 0
}

var vectorParticles = []

class Particle {
  constructor() {
    this.x = mouse.x
    this.y = mouse.y
    this.directionX = Math.random() * 3 - 1.5
    this.directionY = Math.random() * 3 - 1.5
    this.size = Math.random() * 10 + 3
    this.color = 'hsl(' + hue + ', 100%, 50%)'
  }
  process() {
    this.x += this.directionX
    this.y += this.directionY
    this.size -= 0.1
  }
  draw() {
    ctx.beginPath()
    ctx.fillStyle = this.color
    ctx.arc(this.x, this.y, this.size, 0, Math.PI * 2, false)
    ctx.fill()
  }
}

canvas.width = window.innerWidth
canvas.height = window.innerHeight

window.addEventListener('resize', function () {
  canvas.width = window.innerWidth;
  canvas.height = window.innerHeight;
})

canvas.addEventListener('click', function (event) {
  mouse.x = event.x
  mouse.y = event.y

  for (var i = 0; i < 8; i++) {
    vectorParticles.push(new Particle())
  }
})

canvas.addEventListener('mousemove', function (event) {
  mouse.x = event.x
  mouse.y = event.y
  for (var i = 0; i < 3; i++) {
    vectorParticles.push(new Particle())
  }

})

function animate() {
  ctx.clearRect(0, 0, canvas.width, canvas.height)
  //ctx.fillStyle = 'rgba(0, 0, 0, 0.1)'
  //ctx.fillRect(0, 0, canvas.width, canvas.height)
  hue += 5

  for (var c = 0; c < vectorParticles.length; c++) {
    vectorParticles[c].process()
    vectorParticles[c].draw()

    for (var i = c; i < vectorParticles.length; i++) {
      const dx = vectorParticles[c].x - vectorParticles[i].x
      const dy = vectorParticles[c].y - vectorParticles[i].y
      const distance = Math.sqrt(dx * dx + dy * dy)
      if (distance < 50) {
        ctx.beginPath()
        ctx.strokeStyle = vectorParticles[c].color
        ctx.moveTo(vectorParticles[c].x, vectorParticles[c].y)
        ctx.lineTo(vectorParticles[i].x, vectorParticles[i].y)
        ctx.stroke()
      }
    }

    if (vectorParticles[c].size < 1) {
      vectorParticles.splice(c, 1)
      c--
    }
  }

  requestAnimationFrame(animate)
}

animate()