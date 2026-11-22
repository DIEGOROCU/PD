/**
 * Catálogo declarativo único del dominio Sokoban 3D.
 * Para añadir un nuevo Rol o Ítem, basta con registrarlo aquí y darle skin en js/skins.js.
 */

export const CASILLAS = [
  { id: 'Aire',              label: 'Aire (Paso)',    color: '#475569', editable: true,  enLeyenda: false },
  { id: 'Pared',             label: 'Pared',          color: '#334155', editable: true,  enLeyenda: false },
  { id: 'Limite',            label: 'Límite',         color: '#1e293b', editable: false, enLeyenda: false },
  { id: 'Placa',             label: 'Placa Obj.',     color: '#10b981', editable: true,  enLeyenda: true  },
  { id: 'Escalera',          label: 'Escalera',       color: '#a855f7', editable: true,  enLeyenda: true  },
  { id: 'PuertaBoton',       label: 'Puerta Botón',   color: '#ef4444', editable: true,  enLeyenda: true  },
  { id: 'PuertaPalanca',     label: 'Puerta Palanca', color: '#f97316', editable: true,  enLeyenda: true  },
  { id: 'PlacaTemporizada',  label: 'Placa Temp.',    color: '#f59e0b', editable: true,  enLeyenda: true  },
  { id: 'PuertaTemporizada', label: 'Puerta Temp.',   color: '#fbbf24', editable: true,  enLeyenda: true  }
];

export const OBJETOS = [
  { id: 'Caja',      label: 'Caja Madera', color: '#d97706', editable: true, enLeyenda: true  },
  { id: 'CajaMetal', label: 'Caja Metal',  color: '#94a3b8', editable: true, enLeyenda: true  },
  { id: 'Barril',    label: 'Barril',      color: '#b45309', editable: true, enLeyenda: true  },
  { id: 'Cuerda',    label: 'Cuerda',      color: '#eab308', editable: true, enLeyenda: true  },
  { id: 'Globo',     label: 'Globo',       color: '#f43f5e', editable: true, enLeyenda: true  },
  { id: 'Boton',     label: 'Botón',       color: '#22d3ee', editable: true, enLeyenda: false },
  { id: 'Palanca',   label: 'Palanca',     color: '#22c55e', editable: true, enLeyenda: false }
];

// Ítems recogibles: enlazan directamente su constructor Haskell con el Rol que otorgan
export const ITEMS = [
  { id: 'Jetpack', haskellId: 'JetpackItem', rolOtorgado: 'Jetpack', label: 'Ítem Jetpack', color: '#38bdf8', editable: true, enLeyenda: true },
  { id: 'Pico',    haskellId: 'PicoItem',    rolOtorgado: 'Minero',  label: 'Ítem Pico',    color: '#f59e0b', editable: true, enLeyenda: true }
];

// Roles de jugador: añadir un rol aquí actualiza automáticamente el selector y la leyenda
export const ROLES = [
  { id: 'Normal',  label: 'Normal',          color: '#0284c7', enLeyenda: true },
  { id: 'Jetpack', label: 'Rol Jetpack',     color: '#38bdf8', enLeyenda: true },
  { id: 'Minero',  label: 'Rol Minero',      color: '#f59e0b', enLeyenda: true }
];

// Herramientas construidas automáticamente para la paleta del Editor
export const HERRAMIENTAS_EDITOR = [
  ...CASILLAS.filter(c => c.editable).map(c => ({ ...c, kind: 'casilla' })),
  { id: 'Jugador', label: 'Jugador', color: '#38bdf8', kind: 'jugador' },
  ...OBJETOS.filter(o => o.editable).map(o => ({ ...o, kind: 'objeto' })),
  ...ITEMS.filter(i => i.editable).map(i => ({ ...i, kind: 'item' }))
];

// Elementos visibles en la leyenda del Visor
export const ELEMENTOS_LEYENDA = [
  ...ROLES.filter(r => r.enLeyenda).map(r => ({ label: r.id === 'Normal' ? 'Jugador' : r.label, color: r.color })),
  ...CASILLAS.filter(c => c.enLeyenda).map(c => ({ label: c.label, color: c.color })),
  ...OBJETOS.filter(o => o.enLeyenda).map(o => ({ label: o.label, color: o.color })),
  ...ITEMS.filter(i => i.enLeyenda).map(i => ({ label: i.label, color: i.color }))
];

const MAPA_COLORES = Object.fromEntries([
  ...CASILLAS, ...OBJETOS, ...ITEMS, ...ROLES,
  { id: 'Jugador', color: '#38bdf8' },
  { id: 'PalancaCerrada', color: '#ef4444' },
  { id: 'PalancaAbierta', color: '#22c55e' }
].map(e => [e.id, e.color]));

export function obtenerColor(tipo) {
  return MAPA_COLORES[tipo] || '#64748b';
}

export function haskellIdDeItem(itemId) {
  return ITEMS.find(i => i.id === itemId)?.haskellId || `${itemId}Item`;
}

export function itemPorHaskellId(hId) {
  return ITEMS.find(i => i.haskellId === hId || i.id === hId)?.id || hId;
}