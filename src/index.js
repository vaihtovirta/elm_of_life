import 'mini.css';
import './main.css';
import { Elm } from './lib/Main.elm';
import registerServiceWorker from './registerServiceWorker';

Elm.Main.init({
  node: document.getElementById('root')
});

registerServiceWorker();
