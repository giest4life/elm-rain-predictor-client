import 'bootstrap/dist/css/bootstrap.min.css';

import Elm from './Main.elm';

const mountNode = document.getElementById('main');
const app = Elm.Main.embed(mountNode);