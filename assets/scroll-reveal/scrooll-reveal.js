// scroll-reveal.js - 滚动渐入动效核心逻辑
document.addEventListener('DOMContentLoaded', function() {
  // 获取所有带scroll-reveal类的元素
  const revealElements = document.querySelectorAll('.scroll-reveal');
  
  // 监听滚动事件
  function checkReveal() {
    revealElements.forEach(element => {
      // 获取元素相对于视口的位置
      const elementTop = element.getBoundingClientRect().top;
      const windowHeight = window.innerHeight;
      
      // 元素进入视口80%时激活动效
      if (elementTop < windowHeight * 0.8) {
        element.classList.add('active');
      }
    });
  }
  
  // 初始检查（页面加载时）
  checkReveal();
  
  // 滚动时检查
  window.addEventListener('scroll', checkReveal);
  
  // 窗口大小变化时重新检查（适配响应式）
  window.addEventListener('resize', checkReveal);
});