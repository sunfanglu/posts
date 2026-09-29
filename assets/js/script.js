'use strict';

// 移动端菜单开关
const nav = document.querySelector('.mobile-nav');
const navMenuBtn = document.querySelector('.nav-menu-btn');
const navCloseBtn = document.querySelector('.nav-close-btn');

const navToggleFunc = function () { nav.classList.toggle('active'); }

navMenuBtn.addEventListener('click', navToggleFunc);
navCloseBtn.addEventListener('click', navToggleFunc);

// 浅色 / 深色主题切换，桌面和手机按钮一起变
const themeBtn = document.querySelectorAll('.theme-btn');

for (let i = 0; i < themeBtn.length; i++) {

  themeBtn[i].addEventListener('click', function () {

    document.body.classList.toggle('light-theme');
    document.body.classList.toggle('dark-theme');

    for (let j = 0; j < themeBtn.length; j++) {
      themeBtn[j].classList.toggle('light');
      themeBtn[j].classList.toggle('dark');
    }

  })

}

// 首页按研究主题筛选卡片；其他页面没有这些节点时直接跳过
const cards = document.querySelectorAll('.blog-card');
const filterButtons = document.querySelectorAll('[data-filter]');

function applyFilter(topic) {
  // 主题按钮按 data-topic，标签按 data-tags，两者都能筛出对应文章
  cards.forEach(function (card) {
    const tags = (card.dataset.tags || '').split(/\s+/);
    const match = topic === 'all' || card.dataset.topic === topic || tags.indexOf(topic) !== -1;
    card.hidden = !match;
  });

  filterButtons.forEach(function (button) {
    button.classList.toggle('is-active', button.dataset.filter === topic);
  });
}

filterButtons.forEach(function (button) {
  button.addEventListener('click', function () {
    applyFilter(button.dataset.filter);
    if (nav.classList.contains('active')) nav.classList.remove('active');
    const blog = document.querySelector('#writing');
    if (blog) blog.scrollIntoView({ behavior: 'smooth', block: 'start' });
  });
});
