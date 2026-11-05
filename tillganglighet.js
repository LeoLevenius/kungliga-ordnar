// Reactable places its live message directly inside an ARIA rowgroup.
// Keep the announcement outside the table while React continues to own its DOM.
(function () {
  function update() {
    document.querySelectorAll('.reactable .rt-no-data').forEach(function (message) {
      var table = message.closest('.reactable');
      var live = table._liveMessage;
      if (!live || !live.isConnected) {
        live = document.createElement('div');
        live.className = 'visually-hidden';
        live.setAttribute('role', 'status');
        live.setAttribute('aria-live', 'polite');
        table.after(live);
        table._liveMessage = live;
      }
      if (message.hasAttribute('aria-live')) message.removeAttribute('aria-live');
      if (live.textContent !== message.textContent) live.textContent = message.textContent;
    });
  }
  new MutationObserver(update).observe(document.body, {subtree: true, childList: true, characterData: true, attributes: true, attributeFilter: ['aria-live']});
  update();
})();
