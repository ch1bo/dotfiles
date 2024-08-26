function animate(duration, fn) {
  var start
  function go(fn, time) {
    fn(time-start)
    if (time-start < duration) {
      window.requestAnimationFrame(go.bind(undefined, fn))
    }
  }
  window.requestAnimationFrame(function(time) {
    start = time
    go(fn, time)
  })
}

window.addEventListener('load', function() {
  var url = 'wallis.jpg'
  var img = new Image()
  img.onload = function() {
    var bg = document.querySelector('.bg')
    bg.style.backgroundImage = 'url(' + url + ')'
    var duration = 500
    animate(duration, function(time) {
      var blur = Math.floor(Math.abs((duration-time)/duration*20), 0)
      bg.style.filter = 'blur(' + blur  + 'px)'
    })
  }
  img.src = url
})
