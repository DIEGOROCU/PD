/**
 * Serialización y normalización unificada entre JSON (Haskell / Visor / Editor) y código Haskell.
 * Compatible tanto con el formato actual (jugadores con id, items, roles) como con JSONs antiguos.
 */

import { haskellIdDeItem, itemPorHaskellId } from './catalogo.js';

// Normaliza un estado individual a la estructura canónica completa
export function normalizarEstado(raw) {
  if (!raw || !Array.isArray(raw.mapa)) {
    throw new Error('El estado JSON debe contener el campo "mapa".');
  }

  const jugadoresRaw = Array.isArray(raw.jugadores) ? raw.jugadores : [];
  const jugadores = jugadoresRaw.map((j, idx) => {
    if (Array.isArray(j)) return { id: idx + 1, pos: j };
    return { id: j.id ?? (idx + 1), pos: j.pos };
  });

  const objetos = Array.isArray(raw.objetos) ? raw.objetos : [];
  const items = Array.isArray(raw.items)
    ? raw.items.map(it => ({ ...it, tipo: itemPorHaskellId(it.tipo) }))
    : [];
  const roles = Array.isArray(raw.roles) ? raw.roles : [];

  return {
    jugadores,
    mapa: raw.mapa,
    objetos,
    items,
    roles
  };
}

// Normaliza cualquier archivo JSON para el Visor:
// 1) [ { nombre, pasos: [...] }, ... ]
// 2) [ [estado0, estado1, ...], [...] ]
// 3) [ estado0, estado1, ... ]
// 4) { jugadores, mapa, objetos, items, roles }
export function normalizarJsonVisor(data, nombreBase = 'Nivel') {
  if (!data) return [];
  if (!Array.isArray(data)) {
    if (Array.isArray(data.mapa)) {
      return [{ nombre: `${nombreBase} (0 movs)`, pasos: [normalizarEstado(data)] }];
    }
    return [];
  }
  if (data.length === 0) return [];

  const primero = data[0];
  if (primero && Array.isArray(primero.pasos)) {
    return data.map((item, i) => ({
      nombre: item.nombre || `${nombreBase} - Nivel ${i + 1}`,
      pasos: item.pasos.map(normalizarEstado)
    }));
  }
  if (Array.isArray(primero)) {
    return data.map((pasos, i) => ({
      nombre: `Nivel ${i + 1} (${Math.max(0, pasos.length - 1)} movs)`,
      pasos: pasos.map(normalizarEstado)
    }));
  }
  if (primero && Array.isArray(primero.mapa)) {
    return [{
      nombre: `${nombreBase} (${Math.max(0, data.length - 1)} movs)`,
      pasos: data.map(normalizarEstado)
    }];
  }
  return [];
}

// Extrae el primer estado válido para cargarlo en el Editor
export function extraerPrimerEstado(data) {
  const niveles = normalizarJsonVisor(data, 'Importado');
  if (!niveles.length || !niveles[0].pasos.length) {
    throw new Error('No se encontró ningún estado válido en el JSON.');
  }
  return niveles[0].pasos[0];
}

// Genera el código Haskell listo para pegar en conSolucion / sinSolucion de Juego_Sokoban.hs
export function generarCodigoHaskell(grid, { dimX, dimY, dimZ }, baseFill) {
  const jugadores = [];
  const casillas = [];
  const objetos = [];
  const roles = [];
  const items = [];

  let nextId = 1;
  grid.forEach(c => {
    const pos = `(${c.x}, ${c.y}, ${c.z})`;

    if (c.jugador) {
      const jId = c.jugadorId || nextId++;
      jugadores.push(`jugadorConId ${jId} ${pos}`);
      if (c.jugadorRol && c.jugadorRol !== 'Normal') {
        roles.push(`(${jId}, ${c.jugadorRol})`);
      }
    }

    if (c.casilla !== baseFill) {
      if (c.casilla === 'PlacaTemporizada') {
        const t = c.target ? `(${c.target[0]}, ${c.target[1]}, ${c.target[2]})` : '(0, 0, 0)';
        casillas.push(`(${pos}, PlacaActivadoraTemporizada ${t})`);
      } else if (c.casilla === 'PuertaTemporizada') {
        casillas.push(`(${pos}, PuertaTemporizada False (-1) ${c.duracion})`);
      } else {
        casillas.push(`(${pos}, ${c.casilla})`);
      }
    }

    if (c.objeto) {
      if (c.objeto === 'Boton') {
        const t = c.target ? `(${c.target[0]}, ${c.target[1]}, ${c.target[2]})` : '(0, 0, 0)';
        objetos.push(`(${pos}, Boton ${t})`);
      } else if (c.objeto === 'Palanca') {
        const t = c.target ? `(${c.target[0]}, ${c.target[1]}, ${c.target[2]})` : '(0, 0, 0)';
        objetos.push(`(${pos}, Palanca ${t} False)`);
      } else {
        objetos.push(`(${pos}, ${c.objeto})`);
      }
    }

    if (c.item) {
      items.push(`(${pos}, ${haskellIdDeItem(c.item)})`);
    }
  });

  const baseExpr = baseFill === 'Aire'
    ? `(mapa ${dimX} ${dimY} ${dimZ})`
    : `(M.fromList [((x, y, z), Pared) | x <- [0..${dimX - 1}], y <- [0..${dimY - 1}], z <- [0..${dimZ - 1}]])`;

  const casillasExpr = casillas.length
    ? `(M.union\n            (M.fromList\n              [ ${casillas.join('\n              , ')}\n              ])\n            ${baseExpr})`
    : baseExpr;

  const objetosExpr = objetos.length
    ? `(M.fromList\n            [ ${objetos.join('\n            , ')}\n            ])`
    : 'M.empty';

  const rolesExpr = roles.length
    ? `(M.fromList [ ${roles.join(', ')} ])`
    : 'M.empty';

  const itemsExpr = items.length
    ? `(M.fromList\n            [ ${items.join('\n            , ')}\n            ])`
    : 'M.empty';

  return (
`    crearNivel [${jugadores.join(', ')}]
        ${casillasExpr}
        ${objetosExpr}
        ${rolesExpr}
        ${itemsExpr}`
  );
}