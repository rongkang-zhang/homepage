<script>
// 粒子插件（优化：粒子更多更小 + 推动更柔和）
const particle = {
    init: function(e) {
        const t = document.getElementById(e);
        if (!t) return;
        const n = t.getContext("2d"),
              o = 1;
        let a = t.width = window.innerWidth * o,
            c = t.height = window.innerHeight * o;
        const i = .07 * a, // 粒子连接距离（优化：比之前稍紧凑，配合更多粒子）
              s = .18 * a,  // 鼠标影响范围（优化：比之前稍小，推动更温和）
              d = function(e) { return Math.random() * (e || 1); },
              l = Math.PI,
              r = 2 * l;
        let p = new Date;
        const h = (e, t, n) => (1 - n) * e + n * t,
              m = (e, t, n, o) => {
                  const a = e - n,
                        c = t - o;
                  return Math.sqrt(a * a + c * c);
              },
              // 粒子数量（优化：200→280，稍微多一点但不拥挤）
              y = new Array(280).fill({}).map(() => ({
                  x: .5 * a + Math.cos(d(r)) * d(.5 * a),
                  y: .5 * c + Math.sin(d(r)) * d(.5 * c),
                  angle: d(r),
                  speed: d(.12), // 初始速度（优化：比之前稍慢，更平稳）
                  normalSpeed: d(.12),
                  oscAmplitudeX: d(1.5), // 振动幅度（优化：1.5→1.2，运动更平缓）
                  oscSpeedX: .001 + d(.006), // 振动速度（优化：稍慢）
                  oscAmplitudeY: d(1.5),
                  oscSpeedY: .001 + d(.006),
                  connectDistance: d(i),
                  color: { r: 255, g: 255, b: 255 }
              })),
              g = () => {
                  p = new Date;
                  y.forEach(e => {
                      e.x += (Math.cos(e.angle) + Math.cos(p * e.oscSpeedX) * e.oscAmplitudeX) * e.speed;
                      e.y += (Math.sin(e.angle) + Math.cos(p * e.oscSpeedY) * e.oscAmplitudeY) * e.speed;
                      e.speed = h(e.speed, e.normalSpeed * o, .1);
                      (e.x > a || e.x < 0) && (e.angle = l - e.angle);
                      (e.y > c || e.y < 0) && (e.angle = -e.angle);
                      d() < .005 && (e.oscAmplitudeX = d(1.5));
                      d() < .005 && (e.oscSpeedX = .001 + d(.006));
                      d() < .005 && (e.oscAmplitudeY = d(1.5));
                      d() < .005 && (e.oscSpeedY = .001 + d(.006));
                      e.x = Math.max(-.01, Math.min(e.x, a + .01));
                      e.y = Math.max(-.01, Math.min(e.y, c + .01));
                  });
                  n.clearRect(0, 0, a, c);
                  y.map(e => {
                      y.filter(t => e != t && !(m(e.x, e.y, t.x, t.y) > e.connectDistance)).map(t => {
                          const n = m(e.x, e.y, t.x, t.y);
                          return e.speed = h(e.speed, e.speed + .03 / e.connectDistance * n, .2), { // 粒子间速度影响（优化：.05→.03，更柔和）
                              p1: e,
                              p2: t,
                              color: e.color,
                              opacity: Math.floor(100 / e.connectDistance * (e.connectDistance - n)) / 100
                          };
                      }).forEach((e, t) => {
                          const o = Math.sin(p * e.p1.oscSpeedX);
                          n.beginPath(), 
                          n.globalAlpha = e.opacity * 0.8; // 透明度（优化：×0.8，视觉更小）
                          n.moveTo(e.p1.x, e.p1.y), 
                          n.lineTo(e.p2.x, e.p2.y), 
                          n.strokeStyle = `rgb(${Math.floor(e.color.r * o)},${Math.floor(.5 * e.color.g + .5 * e.color.g * o)},${e.color.b})`, 
                          n.lineWidth = 2 * e.opacity; // 线条宽度（优化：4→2，视觉更小）
                          n.stroke(), 
                          n.closePath();
                      });
                  });
                  window.requestAnimationFrame(g);
              };
        g();
        // 鼠标交互（优化：推动速度.3→.18，更慢更柔和）
        window.addEventListener("mousemove", e => {
            const t = e.clientX * o,
                  n = e.clientY * o;
            y.forEach(e => {
                const o = m(t, n, e.x, e.y);
                if (o < s && o > 0) {
                    e.angle = ((e, t, n, o) => Math.atan2(o - t, n - e))(t, n, e.x, e.y);
                    const a = .18 * (s - o); // 推动速度（核心优化：更慢）
                    e.speed = h(e.speed, a, .2);
                }
            });
        });
        // 窗口大小适配
        window.addEventListener("resize", e => {
            a = t.width = window.innerWidth * o;
            c = t.height = window.innerHeight * o;
        });
    }
};

// 页面加载完成后初始化（ID：my_canvas，与HTML一致）
document.addEventListener("DOMContentLoaded", function() {
    particle.init("my_canvas");
});
</script>