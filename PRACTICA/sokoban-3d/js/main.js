/**
 * Punto de entrada principal:
 * - Inicializa la escena 3D compartida
 * - Construye la leyenda desde js/catalogo.js
 * - Gestiona el cambio limpio entre Modo Editor y Modo Visor
 */

import { ELEMENTOS_LEYENDA } from './catalogo.js';
import { crearContextoEscena } from './escena.js';
import { crearEditor } from './editor.js';
import { crearVisor } from './visor.js';

const ctx = crearContextoEscena(document.body);

// Generar leyenda dinámicamente a partir del catálogo
const leyendaEl = document.getElementById('leyenda');
ELEMENTOS_LEYENDA.forEach(({ label, color }) => {
  const div = document.createElement('div');
  div.className = 'item';
  div.innerHTML = `<span class="dot" style="background:${color}"></span>${label}`;
  leyendaEl.appendChild(div);
});

const editor = crearEditor(ctx);
const visor = crearVisor(ctx);

let modoActual = 'editor';
const btnModeEditor = document.getElementById('btnModeEditor');
const btnModeVisor = document.getElementById('btnModeVisor');
const btnTransfer = document.getElementById('btnTransfer');

function cambiarModo(nuevoModo, estadoTransferido = null) {
  modoActual = nuevoModo;
  btnModeEditor.classList.toggle('active', modoActual === 'editor');
  btnModeVisor.classList.toggle('active', modoActual === 'visor');

  if (modoActual === 'editor') {
    visor.desactivar();
    editor.activar(estadoTransferido);
    btnTransfer.textContent = '▶ Probar en Visor';
  } else {
    editor.desactivar();
    visor.activar(estadoTransferido);
    btnTransfer.textContent = '✏️ Editar este paso';
  }
}

btnModeEditor.onclick = () => { if (modoActual !== 'editor') cambiarModo('editor'); };
btnModeVisor.onclick = () => { if (modoActual !== 'visor') cambiarModo('visor'); };

// Botón de traspaso directo entre Editor y Visor
btnTransfer.onclick = () => {
  if (modoActual === 'editor') {
    cambiarModo('visor', editor.obtenerEstadoActual());
  } else {
    const estadoPaso = visor.obtenerEstadoActual();
    cambiarModo('editor', estadoPaso);
  }
};

// Arrancar en Modo Editor y ejecutar el bucle de renderizado compartido
cambiarModo('editor');
ctx.iniciarBucle(() => visor.actualizar());