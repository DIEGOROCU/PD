/**
 * Módulo autocontenido del Modo Editor 3D.
 * Utiliza las skins compartidas de js/skins.js y el catálogo de js/catalogo.js.
 */

import * as THREE from 'three';
import { HERRAMIENTAS_EDITOR, ROLES } from './catalogo.js';
import { crearMallaCasilla, crearMallaObjeto, crearMallaItem, crearMallaJugador } from './skins.js';
import { extraerPrimerEstado, generarCodigoHaskell } from './formato.js';

export function crearEditor(ctx) {
  let activo = false;
  let dimX = 6, dimY = 4, dimZ = 3;
  let currentZ = 0;
  let selectedTool = 'Aire';
  let linkMode = false;
  let pendingLinkSource = null;

  const grid = new Map();
  const keyOf = (x, y, z) => `${x},${y},${z}`;
  const defaultCasilla = () => document.getElementById('baseFill').value;
  const setStatus = (msg) => { document.getElementById('statusMsg').textContent = msg; };

  // Cursor 3D de selección en la planta activa
  const cursorMesh = new THREE.LineSegments(
    new THREE.EdgesGeometry(new THREE.BoxGeometry(1.02, 1.02, 1.02)),
    new THREE.LineBasicMaterial({ color: 0x38bdf8 })
  );
  cursorMesh.visible = false;
  ctx.scene.add(cursorMesh);

  // Rellenar selector de roles iniciales del jugador desde el catálogo
  const roleSelect = document.getElementById('playerRoleSelect');
  ROLES.forEach(r => {
    const opt = document.createElement('option');
    opt.value = r.id;
    opt.textContent = r.label;
    roleSelect.appendChild(opt);
  });

  // Construir paleta de herramientas desde el catálogo
  const paletteEl = document.getElementById('palette');
  HERRAMIENTAS_EDITOR.forEach(t => {
    const btn = document.createElement('button');
    btn.className = 'tool-btn' + (t.id === selectedTool ? ' active' : '');
    btn.innerHTML = `<span class="dot" style="background:${t.color}"></span>${t.label}`;
    btn.onclick = () => {
      selectedTool = t.id;
      linkMode = false;
      pendingLinkSource = null;
      document.getElementById('btnLinkMode').classList.remove('active');
      document.querySelectorAll('.tool-btn').forEach(b => b.classList.remove('active'));
      btn.classList.add('active');
      setStatus(`Pincel activo: ${t.label}`);
    };
    paletteEl.appendChild(btn);
  });

  function initGrid(customBase = null) {
    grid.clear();
    const base = customBase || defaultCasilla();
    const duracion = Number(document.getElementById('tempDuration').value) || 6;
    for (let x = 0; x < dimX; x++) {
      for (let y = 0; y < dimY; y++) {
        for (let z = 0; z < dimZ; z++) {
          grid.set(keyOf(x, y, z), {
            x, y, z,
            casilla: z === 0 ? 'Aire' : base,
            objeto: null,
            item: null,
            jugador: false,
            jugadorId: null,
            jugadorRol: 'Normal',
            target: null,
            duracion
          });
        }
      }
    }
    currentZ = Math.min(currentZ, dimZ - 1);
    if (activo) rebuildScene();
  }

  function rebuildScene() {
    if (!activo) return;
    ctx.limpiarMundo();
    document.getElementById('zLabel').textContent = `Planta Z = ${currentZ} / ${dimZ - 1}`;

    // Rejilla visual en la planta activa
    for (let x = 0; x < dimX; x++) {
      for (let y = 0; y < dimY; y++) {
        const cellFrame = new THREE.LineSegments(
          new THREE.EdgesGeometry(new THREE.PlaneGeometry(0.98, 0.98)),
          new THREE.LineBasicMaterial({ color: 0x38bdf8, transparent: true, opacity: 0.28 })
        );
        cellFrame.rotation.x = -Math.PI / 2;
        cellFrame.position.set(x, currentZ - 0.49, y);
        ctx.worldGroup.add(cellFrame);
      }
    }

    // Conjunto de posiciones ocupadas para conectar cuerdas visualmente
    const ocupadasSet = new Set();
    grid.forEach(c => {
      if (c.z <= currentZ && (c.objeto || c.jugador)) {
        ocupadasSet.add(keyOf(c.x, c.y, c.z));
      }
    });

    grid.forEach(cell => {
      if (cell.z > currentZ) return;
      const isCurrent = cell.z === currentZ;
      const opacidad = isCurrent ? (cell.casilla === 'Pared' ? 0.58 : 1) : 0.22;

      if (cell.casilla !== 'Aire') {
        const datosCasilla = {
          tipo: cell.casilla,
          abierta: false,
          tiempo: -1,
          duracion: cell.duracion
        };
        ctx.worldGroup.add(crearMallaCasilla(datosCasilla, cell.x, cell.y, cell.z, { opacidad }));
      }

      if (cell.objeto) {
        const tipoObj = cell.objeto === 'Palanca' ? 'PalancaCerrada' : cell.objeto;
        ctx.worldGroup.add(crearMallaObjeto({ tipo: tipoObj }, cell.x, cell.y, cell.z, ocupadasSet, { opacidad: isCurrent ? 1 : 0.25 }));
      }

      if (cell.item) {
        ctx.worldGroup.add(crearMallaItem({ tipo: cell.item }, cell.x, cell.y, cell.z, { opacidad: isCurrent ? 1 : 0.25 }));
      }

      if (cell.jugador) {
        ctx.worldGroup.add(crearMallaJugador(cell.jugadorRol, cell.x, cell.y, cell.z, { opacidad: isCurrent ? 1 : 0.25 }));
      }

      if (cell.target && isCurrent) {
        const pts = [
          new THREE.Vector3(cell.x, cell.z + 0.15, cell.y),
          new THREE.Vector3(cell.target[0], cell.target[2] + 0.15, cell.target[1])
        ];
        ctx.worldGroup.add(new THREE.Line(
          new THREE.BufferGeometry().setFromPoints(pts),
          new THREE.LineBasicMaterial({ color: 0xfbbf24 })
        ));
      }
    });

    document.getElementById('haskellOutput').value = generarCodigoHaskell(grid, { dimX, dimY, dimZ }, defaultCasilla());
  }

  // Exporta el estado actual en el formato canónico del motor
  function obtenerEstadoActual() {
    const jugadores = [], mapa = [], objetos = [], items = [], roles = [];
    let nextId = 1;

    grid.forEach(c => {
      if (c.jugador) {
        const id = c.jugadorId || nextId++;
        jugadores.push({ id, pos: [c.x, c.y, c.z] });
        if (c.jugadorRol && c.jugadorRol !== 'Normal') {
          roles.push({ id, rol: c.jugadorRol });
        }
      }
      if (c.casilla === 'PuertaTemporizada') {
        mapa.push({ pos: [c.x, c.y, c.z], tipo: 'PuertaTemporizada', abierta: false, tiempo: -1, duracion: c.duracion });
      } else {
        mapa.push({ pos: [c.x, c.y, c.z], tipo: c.casilla });
      }
      if (c.objeto) {
        const tipo = c.objeto === 'Palanca' ? 'PalancaCerrada' : c.objeto;
        objetos.push({ pos: [c.x, c.y, c.z], tipo });
      }
      if (c.item) {
        items.push({ pos: [c.x, c.y, c.z], tipo: c.item });
      }
    });

    return { jugadores, mapa, objetos, items, roles };
  }

  function cargarEstado(estadoRaw) {
    const estado = extraerPrimerEstado(estadoRaw);
    const todasCoords = [
      ...estado.mapa.map(c => c.pos),
      ...estado.jugadores.map(j => j.pos),
      ...estado.objetos.map(o => o.pos),
      ...estado.items.map(i => i.pos)
    ].filter(Boolean);

    if (!todasCoords.length) throw new Error('No hay coordenadas en el estado.');

    dimX = Math.max(...todasCoords.map(p => p[0])) + 1;
    dimY = Math.max(...todasCoords.map(p => p[1])) + 1;
    dimZ = Math.max(...todasCoords.map(p => p[2])) + 1;

    document.getElementById('dimX').value = dimX;
    document.getElementById('dimY').value = dimY;
    document.getElementById('dimZ').value = dimZ;

    const numParedes = estado.mapa.filter(c => c.tipo === 'Pared').length;
    const baseDetectada = numParedes >= estado.mapa.length / 2 ? 'Pared' : 'Aire';
    document.getElementById('baseFill').value = baseDetectada;

    initGrid(baseDetectada);

    const rolesMap = new Map(estado.roles.map(r => [r.id, r.rol]));

    estado.mapa.forEach(c => {
      const cell = grid.get(keyOf(...c.pos));
      if (!cell) return;
      cell.casilla = c.tipo || 'Aire';
      if (c.tipo === 'PuertaTemporizada' && typeof c.duracion === 'number') {
        cell.duracion = c.duracion;
        document.getElementById('tempDuration').value = c.duracion;
      }
      if (Array.isArray(c.destino) && c.destino.length === 3) cell.target = c.destino;
    });

    estado.objetos.forEach(o => {
      const cell = grid.get(keyOf(...o.pos));
      if (!cell) return;
      if (cell.casilla === 'Pared') cell.casilla = 'Aire';
      cell.objeto = (o.tipo === 'PalancaCerrada' || o.tipo === 'PalancaAbierta') ? 'Palanca' : o.tipo;
      if (Array.isArray(o.destino) && o.destino.length === 3) cell.target = o.destino;
    });

    estado.items.forEach(it => {
      const cell = grid.get(keyOf(...it.pos));
      if (!cell) return;
      if (cell.casilla === 'Pared') cell.casilla = 'Aire';
      cell.item = it.tipo;
    });

    estado.jugadores.forEach(j => {
      const cell = grid.get(keyOf(...j.pos));
      if (!cell) return;
      if (cell.casilla === 'Pared') cell.casilla = 'Aire';
      cell.jugador = true;
      cell.jugadorId = j.id;
      cell.jugadorRol = rolesMap.get(j.id) || 'Normal';
    });

    autoVincularSiUnico('Boton', 'PuertaBoton');
    autoVincularSiUnico('Palanca', 'PuertaPalanca');
    autoVincularSiUnico('PlacaTemporizada', 'PuertaTemporizada');

    currentZ = 0;
    if (activo) {
      ctx.centrarCamaraEnMapa(estado.mapa);
      rebuildScene();
    }
    setStatus(`✅ Nivel cargado (${dimX}×${dimY}×${dimZ}). Revisa enlaces con 🔗 si hay varias puertas.`);
  }

  function autoVincularSiUnico(tipoOrigen, tipoPuerta) {
    const origenes = [], puertas = [];
    grid.forEach(c => {
      if (c.objeto === tipoOrigen || c.casilla === tipoOrigen) origenes.push(c);
      if (c.casilla === tipoPuerta) puertas.push(c);
    });
    if (origenes.length > 0 && puertas.length === 1) {
      const p = puertas[0];
      origenes.forEach(o => { if (!o.target) o.target = [p.x, p.y, p.z]; });
    }
  }

  // Raycasting sobre la planta Z activa
  const raycaster = new THREE.Raycaster();
  const pointer = new THREE.Vector2();
  const hitPlane = new THREE.Plane(new THREE.Vector3(0, 1, 0), 0);

  function getHoveredCell(event) {
    const rect = ctx.renderer.domElement.getBoundingClientRect();
    pointer.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
    pointer.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;
    hitPlane.constant = -currentZ;
    raycaster.setFromCamera(pointer, ctx.camera);
    const pt = new THREE.Vector3();
    if (!raycaster.ray.intersectPlane(hitPlane, pt)) return null;
    const x = Math.round(pt.x);
    const y = Math.round(pt.z);
    if (x < 0 || x >= dimX || y < 0 || y >= dimY) return null;
    return grid.get(keyOf(x, y, currentZ));
  }

  ctx.renderer.domElement.addEventListener('pointermove', (e) => {
    if (!activo) return;
    const cell = getHoveredCell(e);
    if (cell) {
      cursorMesh.visible = true;
      cursorMesh.position.set(cell.x, cell.z, cell.y);
    } else {
      cursorMesh.visible = false;
    }
  });

  ctx.renderer.domElement.addEventListener('pointerdown', (e) => {
    if (!activo || e.button !== 0) return;
    const cell = getHoveredCell(e);
    if (!cell) return;

    if (linkMode) {
      if (!pendingLinkSource) {
        const hasMech = cell.objeto === 'Boton' || cell.objeto === 'Palanca' || cell.casilla === 'PlacaTemporizada';
        if (!hasMech) {
          setStatus('⚠️ Selecciona primero un Botón, Palanca o Placa Temp.');
          return;
        }
        pendingLinkSource = cell;
        setStatus(`Origen (${cell.x},${cell.y},${cell.z}) seleccionado. Haz clic en la puerta destino...`);
      } else {
        pendingLinkSource.target = [cell.x, cell.y, cell.z];
        setStatus(`✅ Enlazado (${pendingLinkSource.x},${pendingLinkSource.y},${pendingLinkSource.z}) → (${cell.x},${cell.y},${cell.z})`);
        pendingLinkSource = null;
        rebuildScene();
      }
      return;
    }

    const tool = HERRAMIENTAS_EDITOR.find(t => t.id === selectedTool);
    if (tool.kind === 'casilla') {
      cell.casilla = cell.casilla === tool.id ? 'Aire' : tool.id;
      if (tool.id === 'Pared') { cell.objeto = null; cell.item = null; cell.jugador = false; }
      if (tool.id === 'PuertaTemporizada') {
        cell.duracion = Number(document.getElementById('tempDuration').value) || 6;
      }
    } else if (tool.kind === 'objeto') {
      if (cell.casilla === 'Pared') cell.casilla = 'Aire';
      cell.jugador = false;
      cell.objeto = cell.objeto === tool.id ? null : tool.id;
    } else if (tool.kind === 'item') {
      if (cell.casilla === 'Pared') cell.casilla = 'Aire';
      cell.item = cell.item === tool.id ? null : tool.id;
    } else if (tool.kind === 'jugador') {
      if (cell.casilla === 'Pared') cell.casilla = 'Aire';
      cell.objeto = null;
      const rolElegido = roleSelect.value;
      if (cell.jugador && cell.jugadorRol === rolElegido) {
        cell.jugador = false;
      } else {
        cell.jugador = true;
        cell.jugadorRol = rolElegido;
      }
    }

    setStatus(`Editado (${cell.x}, ${cell.y}, ${cell.z}) → ${tool.label}`);
    rebuildScene();
  });

  // Eventos UI del Editor
  document.getElementById('btnExportJson').onclick = () => {
    const estado = obtenerEstadoActual();
    const payload = [{ nombre: 'Nivel Custom (0 movs)', pasos: [estado] }];
    const blob = new Blob([JSON.stringify(payload, null, 2)], { type: 'application/json' });
    const a = document.createElement('a');
    a.href = URL.createObjectURL(blob);
    a.download = 'nivel_custom.json';
    a.click();
  };

  document.getElementById('editorJsonFileInput').addEventListener('change', async (e) => {
    const file = e.target.files?.[0];
    if (!file) return;
    const text = await file.text();
    document.getElementById('jsonInput').value = text;
    try { cargarEstado(JSON.parse(text)); } catch (err) { setStatus(`❌ ${err.message}`); }
    e.target.value = '';
  });

  document.getElementById('btnLoadJsonText').onclick = () => {
    const text = document.getElementById('jsonInput').value.trim();
    if (!text) return setStatus('⚠️ Pega primero un JSON en el recuadro inferior.');
    try { cargarEstado(JSON.parse(text)); } catch (err) { setStatus(`❌ ${err.message}`); }
  };

  document.getElementById('btnCopy').onclick = () => {
    const ta = document.getElementById('haskellOutput');
    ta.select();
    navigator.clipboard.writeText(ta.value);
    setStatus('📋 Código Haskell copiado al portapapeles');
  };

  document.getElementById('btnLinkMode').onclick = () => {
    linkMode = !linkMode;
    pendingLinkSource = null;
    document.getElementById('btnLinkMode').classList.toggle('active', linkMode);
    setStatus(linkMode ? '🔗 Modo enlace: haz clic en un Botón, Palanca o Placa Temp.' : 'Modo enlace desactivado');
  };

  ['dimX', 'dimY', 'dimZ'].forEach(id => {
    document.getElementById(id).addEventListener('change', () => {
      dimX = Math.max(1, Number(document.getElementById('dimX').value));
      dimY = Math.max(1, Number(document.getElementById('dimY').value));
      dimZ = Math.max(1, Number(document.getElementById('dimZ').value));
      initGrid();
    });
  });

  document.getElementById('baseFill').addEventListener('change', () => initGrid());
  document.getElementById('btnResetMap').onclick = () => initGrid();
  document.getElementById('btnZUp').onclick = () => { if (currentZ < dimZ - 1) { currentZ++; rebuildScene(); } };
  document.getElementById('btnZDown').onclick = () => { if (currentZ > 0) { currentZ--; rebuildScene(); } };

  window.addEventListener('keydown', (e) => {
    if (!activo) return;
    if (document.activeElement.tagName === 'TEXTAREA' || document.activeElement.tagName === 'INPUT') return;
    if (e.key === 'e' || e.key === 'E') { if (currentZ < dimZ - 1) { currentZ++; rebuildScene(); } }
    if (e.key === 'q' || e.key === 'Q') { if (currentZ > 0) { currentZ--; rebuildScene(); } }
  });

  initGrid();

  return {
    activar(estadoTransferido = null) {
      activo = true;
      document.getElementById('editorUI').classList.remove('hidden');
      ctx.controls.mouseButtons = {
        LEFT: null,
        MIDDLE: THREE.MOUSE.DOLLY,
        RIGHT: THREE.MOUSE.ROTATE
      };
      if (estadoTransferido) {
        cargarEstado(estadoTransferido);
      } else {
        ctx.controls.target.set((dimX - 1) / 2, 0, (dimY - 1) / 2);
        rebuildScene();
      }
    },
    desactivar() {
      activo = false;
      cursorMesh.visible = false;
      document.getElementById('editorUI').classList.add('hidden');
    },
    obtenerEstadoActual
  };
}