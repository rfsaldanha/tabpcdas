(function () {
  'use strict';
  let serial = 0;
  function cancelPending(control) {
    if (control.tmPending) {
      const pending = control.tmPending;
      control.tmPending = null;
      pending.callback([]);
    }
  }
  window.tmFilterType = function (control, text) {
    if (control.tmPending && control.tmPending.text !== text) cancelPending(control);
  };
  window.tmFilterLoad = function (control, inputId, field, text, callback) {
    cancelPending(control);
    const ticket = ++serial;
    control.tmPending = {ticket, text, callback};
    // Keep chosen options while replacing suggestions from the previous search.
    Object.keys(control.options).forEach(function (value) {
      if (control.items.indexOf(value) === -1) control.removeOption(value, true);
    });
    control.loadedSearches = {[text]: true};
    control.addOption({value: '__TM_MISSING__', label: 'Não informado'});
    Shiny.setInputValue(inputId, {ticket, field, text}, {priority: 'event'});
  };
  $(function () {
    Shiny.addCustomMessageHandler('tm-filter-options', function (message) {
      const element = document.getElementById(message.id);
      const control = element && element.selectize;
      if (!control || !control.tmPending) return;
      const pending = control.tmPending;
      if (pending.ticket !== message.ticket || pending.text !== control.$control_input.val()) return;
      control.tmPending = null;
      if (message.failed) delete control.loadedSearches[pending.text];
      pending.callback(message.options || []);
    });
    Shiny.addCustomMessageHandler('tm-filter-reload', function (message) {
      const element = document.getElementById(message.id);
      const control = element && element.selectize;
      if (!control) return;
      cancelPending(control);
      control.loadedSearches = {};
      control.onSearchChange(control.$control_input.val() || '');
    });
  });
}());
