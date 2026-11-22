/**
 * Módulo autocontenido del Modo Visor 3D.
 * Renderiza soluciones paso a paso usando exactamente las mismas skins de js/skins.js.
 */

import * as THREE from 'three';
import { crearMallaCasilla, crearMallaObjeto, crearMallaItem, crearMallaJugador } from './skins.js';
import { normalizarJsonVisor } from './formato.js';

export function crearVisor(ctx) {
  let activo = false;
  let juegos = [];
  let juegoActual = 0;
  let estados = [];
  let pasoActual = 0;
  let timer = null;
  let animProgress = 1;
  const animatedMeshes = [];

  function detenerReproduccion() {
    if (timer) {
      clearInterval(timer);
      timer = null;
      document.getElementById('btnPlay').textContent = '▶ Play';
    }
  }

  function renderizarPaso(idx, prevIdx = null) {
    if (!estados.length || !activo) return;
    pasoActual = Math.max(0, Math.min(idx, estados.length - 1));
    const est = estados[pasoActual];
    const prevEst = (prevIdx !== null && estados[prevIdx]) ? estados[prevIdx] : est;

    ctx.limpiarMundo();
    animatedMeshes.length = 0;

    const ocupadasSet = new Set();
    est.objetos.forEach(o => ocupadasSet.add(`${o.pos[0]},${o.pos[1]},${o.pos[2]}`));
    est.jugadores.forEach(j => ocupadasSet.add(`${j.pos[0]},${j.pos[1]},${j.pos[2]}`));

    // Mapa y suelo base en Z = 0
    const baseVistas = new Set();
    est.mapa.forEach((celda) => {
      const [x, y, z] = celda.pos;
      if (z === 0 && !baseVistas.has(`${x},${y}`)) {
        baseVistas.add(`${x},${y}`);
        ctx.worldGroup.add(crearMallaCasilla({ tipo: 'SueloBase' }, x, y, -1));
      }
      if (celda.tipo !== 'Aire' && celda.tipo !== 'Suelo') {
        ctx.worldGroup.add(crearMallaCasilla(celda, x, y, z));
      }
    });

    // Ítems recogibles
    est.items.forEach((item) => {
      const [x, y, z] = item.pos;
      ctx.worldGroup.add(crearMallaItem(item, x, y, z));
    });

    // Objetos móviles y mecanismos
    est.objetos.forEach((obj, i) => {
      const [x, y, z] = obj.pos;
      const mesh = crearMallaObjeto(obj, x, y, z, ocupadasSet);
      const prevObj = prevEst.objetos[i];
      if (prevObj && prevObj.tipo === obj.tipo) {
        const [px, py, pz] = prevObj.pos;
        mesh.userData.from = new THREE.Vector3(px, pz, py);
        mesh.userData.to = new THREE.Vector3(x, z, y);
        mesh.position.copy(mesh.userData.from);
        animatedMeshes.push(mesh);
      }
      ctx.worldGroup.add(mesh);
    });

    // Jugadores con su rol activo
    const rolesMap = new Map(est.roles.map(r => [r.id, r.rol]));
    const prevJugadoresMap = new Map(prevEst.jugadores.map(j => [j.id, j.pos]));

    est.jugadores.forEach((jug) => {
      const [x, y, z] = jug.pos;
      const rol = rolesMap.get(jug.id) || 'Normal';
      const mesh = crearMallaJugador(rol, x, y, z);
      const prevPos = prevJugadoresMap.get(jug.id);
      if (prevPos) {
        const [px, py, pz] = prevPos;
        mesh.userData.from = new THREE.Vector3(px, pz, py);
        mesh.userData.to = new THREE.Vector3(x, z, y);
        mesh.position.copy(mesh.userData.from);
        animatedMeshes.push(mesh);
      }
      ctx.worldGroup.add(mesh);
    });

    animProgress = 0;

    const total = estados.length;
    document.getElementById('stepLabel').textContent = `Paso: ${pasoActual + 1} / ${total}`;
    document.getElementById('pctLabel').textContent = `${Math.round(((pasoActual + 1) / total) * 100)}%`;
    const slider = document.getElementById('stepSlider');
    slider.max = total - 1;
    slider.value = pasoActual;
  }

  function seleccionarJuego(idx) {
    if (!juegos.length) return;
    detenerReproduccion();
    juegoActual = Math.max(0, Math.min(idx, juegos.length - 1));
    estados = juegos[juegoActual].pasos;
    document.getElementById('levelSelect').value = String(juegoActual);
    document.getElementById('btnPrevLevel').disabled = juegoActual <= 0;
    document.getElementById('btnNextLevel').disabled = juegoActual >= juegos.length - 1;
    if (estados.length && activo) {
      ctx.centrarCamaraEnMapa(estados[0].mapa);
      renderizarPaso(0);
    }
  }

  function cargarJuegos(nuevosJuegos, etiquetaArchivo = 'Nivel cargado') {
    if (!nuevosJuegos.length) return;
    detenerReproduccion();
    juegos = nuevosJuegos;

    document.getElementById('fileName').textContent = etiquetaArchivo;
    const levelSelect = document.getElementById('levelSelect');
    levelSelect.innerHTML = '';
    juegos.forEach((j, idx) => {
      const opt = document.createElement('option');
      opt.value = String(idx);
      opt.textContent = j.nombre;
      levelSelect.appendChild(opt);
    });

    ['btnReset', 'btnPrev', 'btnPlay', 'btnNext', 'stepSlider', 'levelSelect'].forEach(id => {
      document.getElementById(id).disabled = false;
    });

    seleccionarJuego(0);
  }

  function iniciarReproduccion() {
    if (!estados.length) return;
    if (pasoActual >= estados.length - 1) renderizarPaso(0);
    document.getElementById('btnPlay').textContent = '⏸ Pausa';
    const delay = Number(document.getElementById('speedSelect').value);
    timer = setInterval(() => {
      if (pasoActual < estados.length - 1) renderizarPaso(pasoActual + 1, pasoActual);
      else detenerReproduccion();
    }, delay);
  }

  function reiniciarVisor() {
    if (!estados.length) return;
    detenerReproduccion();
    ctx.centrarCamaraEnMapa(estados[0].mapa);
    renderizarPaso(0);
  }

  // Eventos UI del Visor
  document.getElementById('visorFileInput').addEventListener('change', async (e) => {
    const files = Array.from(e.target.files || []);
    if (!files.length) return;
    const nuevosJuegos = [];
    for (const file of files) {
      const parsed = JSON.parse(await file.text());
      nuevosJuegos.push(...normalizarJsonVisor(parsed, file.name.replace(/\.json$/i, '')));
    }
    const etiqueta = files.length === 1
      ? `${files[0].name} (${nuevosJuegos.length})`
      : `${files.length} archivos (${nuevosJuegos.length} niveles)`;
    cargarJuegos(nuevosJuegos, etiqueta);
    e.target.value = '';
  });

  document.getElementById('levelSelect').addEventListener('change', (e) => seleccionarJuego(Number(e.target.value)));
  document.getElementById('btnPrevLevel').onclick = () => seleccionarJuego(juegoActual - 1);
  document.getElementById('btnNextLevel').onclick = () => seleccionarJuego(juegoActual + 1);
  document.getElementById('stepSlider').addEventListener('input', (e) => { detenerReproduccion(); renderizarPaso(Number(e.target.value)); });
  document.getElementById('speedSelect').addEventListener('change', () => { if (timer) { detenerReproduccion(); iniciarReproduccion(); } });
  document.getElementById('btnReset').onclick = reiniciarVisor;
  document.getElementById('btnPrev').onclick = () => { detenerReproduccion(); renderizarPaso(pasoActual - 1, pasoActual); };
  document.getElementById('btnNext').onclick = () => { detenerReproduccion(); renderizarPaso(pasoActual + 1, pasoActual); };
  document.getElementById('btnPlay').onclick = () => timer ? detenerReproduccion() : iniciarReproduccion();

  window.addEventListener('keydown', (e) => {
    if (!activo || !estados.length) return;
    if (e.key === 'ArrowRight') { detenerReproduccion(); renderizarPaso(pasoActual + 1, pasoActual); }
    if (e.key === 'ArrowLeft')  { detenerReproduccion(); renderizarPaso(pasoActual - 1, pasoActual); }
    if (e.key === 'PageDown')   { e.preventDefault(); seleccionarJuego(juegoActual + 1); }
    if (e.key === 'PageUp')     { e.preventDefault(); seleccionarJuego(juegoActual - 1); }
    if (e.key === ' ')          { e.preventDefault(); timer ? detenerReproduccion() : iniciarReproduccion(); }
    if (e.key === 'r' || e.key === 'R') reiniciarVisor();
  });

  return {
    activar(estadoDesdeEditor = null) {
      activo = true;
      document.getElementById('visorUI').classList.remove('hidden');
      ctx.controls.mouseButtons = {
        LEFT: THREE.MOUSE.ROTATE,
        MIDDLE: THREE.MOUSE.DOLLY,
        RIGHT: THREE.MOUSE.PAN
      };
      if (estadoDesdeEditor) {
        cargarJuegos([{ nombre: 'Vista previa desde Editor', pasos: [estadoDesdeEditor] }], 'Desde Editor');
      } else if (estados.length) {
        ctx.centrarCamaraEnMapa(estados[0].mapa);
        renderizarPaso(pasoActual);
      } else {
        ctx.limpiarMundo();
      }
    },
    desactivar() {
      activo = false;
      detenerReproduccion();
      document.getElementById('visorUI').classList.add('hidden');
    },
    actualizar() {
      if (activo && animProgress < 1) {
        animProgress = Math.min(1, animProgress + 0.14);
        const ease = 1 - Math.pow(1 - animProgress, 3);
        animatedMeshes.forEach(m => m.position.lerpVectors(m.userData.from, m.userData.to, ease));
      }
    },
    obtenerEstadoActual() {
      return estados[pasoActual] || null;
    }
  };
}