(function () {
  // Reactable puts its "no data" message inside an ARIA rowgroup, which requires row children.
  // The announcement is mirrored to a live region outside the table, and the empty rowgroup is made presentational.
  function reactable() {
    document.querySelectorAll('.reactable .rt-tbody').forEach(function (body) {
      var m = body.querySelector('.rt-no-data');
      var tom = !!(m && m.textContent.trim());
      var roll = body.getAttribute('role');
      if (tom && roll !== 'presentation') body.setAttribute('role', 'presentation');
      else if (!tom && roll === 'presentation') body.setAttribute('role', 'rowgroup');
    });
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

  // Quarto's search button and the heading anchors have English labels that cannot be set in the Quarto configuration.
  function etiketter() {
    document.querySelectorAll('.aa-DetachedSearchButton, .aa-Autocomplete [aria-label="Search"], .aa-Autocomplete [title="Search"], [role="combobox"][aria-label="Search"]').forEach(function (el) {
      if (el.getAttribute('aria-label') === 'Search') el.setAttribute('aria-label', 'Sök');
      if (el.getAttribute('title') === 'Search') el.setAttribute('title', 'Sök');
    });
    document.querySelectorAll('a.anchorjs-link[aria-label="Anchor"]').forEach(function (a) {
      a.setAttribute('aria-label', 'Länk till avsnittet');
    });
  }

  function update() {
    reactable();
    etiketter();
  }
  new MutationObserver(update).observe(document.body, {subtree: true, childList: true, characterData: true, attributes: true, attributeFilter: ['aria-live', 'aria-label', 'title']});
  update();
})();
