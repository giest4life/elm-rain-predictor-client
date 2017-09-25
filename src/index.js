import 'bootstrap/dist/css/bootstrap.min.css';
import './style/style.css'
import Elm from './Main.elm';

const mountNode = document.getElementById('main');
const app = Elm.Main.embed(mountNode);