'use strict';

require('./static/style.css');
const {Elm} = require('./elm/Main.elm');

Elm.Main.init({
    node: document.getElementById('elm')
});