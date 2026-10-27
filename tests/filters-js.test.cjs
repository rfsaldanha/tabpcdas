// The transport must ignore old searches, even if a response arrives before
// the debounce for the next search has elapsed or after the widget is rebuilt.
const assert = require('node:assert/strict');
const vm = require('node:vm');
const fs = require('node:fs');
const handlers = {}, sent = [], completed = [];
const control = {
  text: '', tmPending: null, loadedSearches: {}, items: ['120040'],
  options: {'120040': {value:'120040',label:'Rio Branco — 120040'}},
  $control_input: {val() { return control.text; }},
  removeOption(value) { delete this.options[value]; },
  addOption(option) { this.options[option.value] = option; },
  onSearchChange(text) { this.reloaded = text; }
};
const context = {
  window: {}, $: fn => fn(),
  document: {getElementById: () => ({selectize: control})},
  Shiny: {
    setInputValue(id, payload) { sent.push(payload); },
    addCustomMessageHandler(name, handler) { handlers[name] = handler; }
  }
};
vm.runInNewContext(fs.readFileSync('www/filters.js', 'utf8'), context);
function search(text) {
  control.text = text;
  context.window.tmFilterType(control, text);
  context.window.tmFilterLoad(control, 'filter1-lookup', 'TM_MUN_RES', text,
    options => completed.push({text, options}));
  return sent.at(-1).ticket;
}
const old = search('rio');
control.text = 'acre';
context.window.tmFilterType(control, 'acre');
assert.equal(completed.length, 1); // cancelled callback frees the loading state
handlers['tm-filter-options']({id: 'filter1-value', ticket: old, options: [{value:'120040',label:'Rio Branco — 120040'}]});
assert.equal(completed.length, 1);
const current = search('acre');
assert.equal(control.items[0], '120040');
assert.equal(control.options['120040'].label, 'Rio Branco — 120040');
handlers['tm-filter-options']({id: 'filter1-value', ticket: old, options: []});
assert.equal(completed.length, 1);
handlers['tm-filter-options']({id: 'filter1-value', ticket: current, options: [{value:'120001',label:'Acrelândia — 120001'}]});
assert.equal(completed.at(-1).options[0].value, '120001');
const pending = search('rio');
handlers['tm-filter-reload']({id:'filter1-value'});
assert.equal(control.tmPending, null);
assert.equal(control.reloaded, 'rio');
const before = completed.length;
handlers['tm-filter-options']({id:'filter1-value',ticket:pending,options:[]});
assert.equal(completed.length, before);
console.log('Filter transport: stale responses, cancellation and reload passed');
