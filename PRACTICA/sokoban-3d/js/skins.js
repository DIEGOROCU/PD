/**
 * Librería completa de "skins" 3D de alta calidad para Sokoban 3D.
 * Incluye texturas procedurales (<canvas>), materiales PBR, geometrías esculpidas
 * y animaciones continuas compartidas entre el Modo Editor y el Modo Visor.
 */

import * as THREE from 'three';

// ============================================================================
// 1. GENERADORES DE TEXTURAS PROCEDURALES (<canvas>)
// ============================================================================

function crearTexturaCanvas(size, dibujar) {
  const canvas = document.createElement('canvas');
  canvas.width = size;
  canvas.height = size;
  const ctx = canvas.getContext('2d');
  dibujar(ctx, size);
  const tex = new THREE.CanvasTexture(canvas);
  tex.colorSpace = THREE.SRGBColorSpace;
  tex.wrapS = THREE.RepeatWrapping;
  tex.wrapT = THREE.RepeatWrapping;
  return tex;
}

const TEX_MADERA = crearTexturaCanvas(256, (ctx, s) => {
  ctx.fillStyle = '#b45309';
  ctx.fillRect(0, 0, s, s);
  const tablas = 4;
  const h = s / tablas;
  for (let i = 0; i < tablas; i++) {
    const y0 = i * h;
    ctx.fillStyle = i % 2 === 0 ? 'rgba(251, 191, 36, 0.08)' : 'rgba(69, 26, 3, 0.14)';
    ctx.fillRect(0, y0, s, h);
    // Vetas de madera
    for (let v = 0; v < 14; v++) {
      ctx.strokeStyle = v % 2 === 0 ? 'rgba(69, 26, 3, 0.22)' : 'rgba(245, 158, 11, 0.12)';
      ctx.lineWidth = 1.5 + (v % 3);
      ctx.beginPath();
      const yLinea = y0 + (v / 14) * h;
      ctx.moveTo(0, yLinea);
      ctx.bezierCurveTo(s * 0.33, yLinea + Math.sin(v + i) * 4, s * 0.66, yLinea - Math.cos(v) * 4, s, yLinea);
      ctx.stroke();
    }
    // Junta entre tablones
    ctx.fillStyle = 'rgba(28, 10, 0, 0.65)';
    ctx.fillRect(0, y0, s, 3);
  }
});

const TEX_METAL = crearTexturaCanvas(256, (ctx, s) => {
  const grad = ctx.createLinearGradient(0, 0, s, s);
  grad.addColorStop(0, '#cbd5e1');
  grad.addColorStop(0.5, '#94a3b8');
  grad.addColorStop(1, '#64748b');
  ctx.fillStyle = grad;
  ctx.fillRect(0, 0, s, s);

  // Cepillado metálico fino
  for (let i = 0; i < 90; i++) {
    ctx.fillStyle = i % 2 === 0 ? 'rgba(255,255,255,0.06)' : 'rgba(15,23,42,0.06)';
    ctx.fillRect(0, (i / 90) * s, s, 1.5);
  }
  // Marco biselado
  ctx.strokeStyle = 'rgba(15, 23, 42, 0.45)';
  ctx.lineWidth = 10;
  ctx.strokeRect(5, 5, s - 10, s - 10);
  ctx.strokeStyle = 'rgba(248, 250, 252, 0.35)';
  ctx.lineWidth = 3;
  ctx.strokeRect(12, 12, s - 24, s - 24);
});

const TEX_PIEDRA = crearTexturaCanvas(256, (ctx, s) => {
  ctx.fillStyle = '#334155';
  ctx.fillRect(0, 0, s, s);

  // Micro-grano mineral
  for (let i = 0; i < 900; i++) {
    const x = (i * 73) % s;
    const y = (i * 137) % s;
    ctx.fillStyle = i % 2 === 0 ? 'rgba(255,255,255,0.04)' : 'rgba(9,13,22,0.12)';
    ctx.fillRect(x, y, 3, 3);
  }

  // Sillares de piedra
  ctx.strokeStyle = 'rgba(15, 23, 42, 0.6)';
  ctx.lineWidth = 4;
  const filas = 4;
  const altoFila = s / filas;
  for (let r = 0; r < filas; r++) {
    const y = r * altoFila;
    ctx.beginPath();
    ctx.moveTo(0, y);
    ctx.lineTo(s, y);
    ctx.stroke();

    const offset = (r % 2) * (s / 4);
    for (let c = 0; c < 2; c++) {
      const x = (offset + c * (s / 2)) % s;
      ctx.beginPath();
      ctx.moveTo(x, y);
      ctx.lineTo(x, y + altoFila);
      ctx.stroke();
    }
  }
});

const TEX_PELIGRO = crearTexturaCanvas(128, (ctx, s) => {
  ctx.fillStyle = '#eab308';
  ctx.fillRect(0, 0, s, s);
  ctx.fillStyle = '#1e293b';
  for (let i = -s; i < s * 2; i += 24) {
    ctx.beginPath();
    ctx.moveTo(i, 0);
    ctx.lineTo(i + 12, 0);
    ctx.lineTo(i - 12, s);
    ctx.lineTo(i - 24, s);
    ctx.closePath();
    ctx.fill();
  }
});

// ============================================================================
// 2. LIBRERÍA DE MATERIALES PBR
// ============================================================================

export const MATERIALES = {
  // Arquitectura y casillas
  Pared:          new THREE.MeshStandardMaterial({ map: TEX_PIEDRA, bumpMap: TEX_PIEDRA, bumpScale: 0.025, roughness: 0.78, metalness: 0.12 }),
  ParedColumna:   new THREE.MeshStandardMaterial({ color: 0x1e293b, roughness: 0.6, metalness: 0.35 }),
  Limite:         new THREE.MeshStandardMaterial({ color: 0x0f172a, roughness: 0.85, metalness: 0.3 }),
  SueloBase:      new THREE.MeshStandardMaterial({ map: TEX_PIEDRA, color: 0x475569, roughness: 0.85, metalness: 0.1 }),
  SueloBorde:     new THREE.MeshStandardMaterial({ color: 0x1e293b, roughness: 0.5, metalness: 0.5 }),

  // Placas y puertas
  Placa:          new THREE.MeshStandardMaterial({ color: 0x10b981, emissive: 0x059669, emissiveIntensity: 0.55, roughness: 0.25, metalness: 0.2 }),
  PlacaTemp:      new THREE.MeshStandardMaterial({ color: 0xf59e0b, emissive: 0xd97706, emissiveIntensity: 0.6, roughness: 0.25, metalness: 0.2 }),
  PuertaBoton:    new THREE.MeshStandardMaterial({ color: 0xdc2626, roughness: 0.35, metalness: 0.45 }),
  PuertaPalanca:  new THREE.MeshStandardMaterial({ color: 0xea580c, roughness: 0.35, metalness: 0.45 }),
  PuertaTemp:     new THREE.MeshPhysicalMaterial({
    color: 0xfbbf24, emissive: 0xd97706, emissiveIntensity: 0.25,
    roughness: 0.12, metalness: 0.1, transmission: 0.55, thickness: 0.5,
    transparent: true, opacity: 0.85
  }),
  Escalera:       new THREE.MeshStandardMaterial({ color: 0x9333ea, roughness: 0.35, metalness: 0.45 }),
  EscaleraPeldano:new THREE.MeshStandardMaterial({ color: 0xc084fc, emissive: 0x7e22ce, emissiveIntensity: 0.25, roughness: 0.3, metalness: 0.5 }),

  // Objetos móviles
  Caja:           new THREE.MeshStandardMaterial({ map: TEX_MADERA, bumpMap: TEX_MADERA, bumpScale: 0.03, roughness: 0.65 }),
  CajaMarco:      new THREE.MeshStandardMaterial({ color: 0x78350f, roughness: 0.75 }),
  CajaMetal:      new THREE.MeshStandardMaterial({ map: TEX_METAL, bumpMap: TEX_METAL, bumpScale: 0.02, metalness: 0.88, roughness: 0.2 }),
  AceroOscuro:    new THREE.MeshStandardMaterial({ color: 0x334155, metalness: 0.85, roughness: 0.28 }),
  Cromo:          new THREE.MeshStandardMaterial({ color: 0xf1f5f9, metalness: 0.95, roughness: 0.12 }),
  Barril:         new THREE.MeshStandardMaterial({ map: TEX_MADERA, color: 0xb45309, bumpMap: TEX_MADERA, bumpScale: 0.025, roughness: 0.68 }),
  AroMetal:       new THREE.MeshStandardMaterial({ color: 0x334155, metalness: 0.85, roughness: 0.28 }),
  MaderaMango:    new THREE.MeshStandardMaterial({ color: 0x78350f, roughness: 0.72 }),
  Cuerda:         new THREE.MeshStandardMaterial({ color: 0xeab308, roughness: 0.8 }),
  CuerdaNudo:     new THREE.MeshStandardMaterial({ color: 0xb45309, roughness: 0.65 }),
  Bronce:         new THREE.MeshStandardMaterial({ color: 0xd97706, metalness: 0.85, roughness: 0.25 }),
  Globo:          new THREE.MeshPhysicalMaterial({ color: 0xf43f5e, roughness: 0.12, metalness: 0.05, clearcoat: 1.0, clearcoatRoughness: 0.08 }),
  Peligro:        new THREE.MeshStandardMaterial({ map: TEX_PELIGRO, roughness: 0.5, metalness: 0.2 }),
  Boton:          new THREE.MeshPhysicalMaterial({ color: 0x22d3ee, emissive: 0x06b6d4, emissiveIntensity: 0.55, roughness: 0.15, clearcoat: 1.0 }),
  PalancaCerrada: new THREE.MeshStandardMaterial({ color: 0xef4444, emissive: 0xdc2626, emissiveIntensity: 0.5, roughness: 0.25 }),
  PalancaAbierta: new THREE.MeshStandardMaterial({ color: 0x22c55e, emissive: 0x16a34a, emissiveIntensity: 0.5, roughness: 0.25 }),

  // Ítems y roles
  ItemJetpack:    new THREE.MeshStandardMaterial({ color: 0x38bdf8, metalness: 0.8, roughness: 0.2 }),
  LlamaPropulsor: new THREE.MeshStandardMaterial({ color: 0xfb923c, emissive: 0xf97316, emissiveIntensity: 1.2, roughness: 0.2 }),
  ItemPicoCabeza: new THREE.MeshStandardMaterial({ color: 0xe2e8f0, metalness: 0.94, roughness: 0.16 }),
  CascoMinero:    new THREE.MeshStandardMaterial({ color: 0xf59e0b, roughness: 0.3, metalness: 0.35 }),
  LuzMinero:      new THREE.MeshStandardMaterial({ color: 0xfef9c3, emissive: 0xfacc15, emissiveIntensity: 1.4 }),

  // Jugador
  Jugador:        new THREE.MeshPhysicalMaterial({ color: 0x0284c7, roughness: 0.25, metalness: 0.35, clearcoat: 0.6 }),
  JugadorTraje:   new THREE.MeshStandardMaterial({ color: 0x0f172a, roughness: 0.5, metalness: 0.4 }),
  JugadorAnillo:  new THREE.MeshStandardMaterial({ color: 0x7dd3fc, emissive: 0x38bdf8, emissiveIntensity: 1.0, roughness: 0.15 })
};

// ============================================================================
// 3. GEOMETRÍAS ESCULPIDAS REUTILIZABLES Y UTILIDADES
// ============================================================================

const GEOS = (() => {
  // Perfil torneado de barril abombado con base cerrada y tapa superior hundida
  const barrilPts = [
    new THREE.Vector2(0.0,  -0.44), // Centro base inferior
    new THREE.Vector2(0.31, -0.44)  // Borde exterior inferior
  ];
  for (let i = 1; i < 12; i++) {
    const t = i / 12;
    const y = (t - 0.5) * 0.88;
    const r = 0.31 + Math.sin(t * Math.PI) * 0.085;
    barrilPts.push(new THREE.Vector2(r, y));
  }
  barrilPts.push(
    new THREE.Vector2(0.31,  0.44),  // Borde exterior superior
    new THREE.Vector2(0.275, 0.44),  // Grosor del reborde de madera
    new THREE.Vector2(0.275, 0.405), // Escalón interior de la tapa
    new THREE.Vector2(0.0,   0.405)  // Centro tapa superior
  );
  const barril = new THREE.LatheGeometry(barrilPts, 24);

  // Perfil torneado en lágrima para globo aerostático
  const globoPts = [
    new THREE.Vector2(0.06, -0.26),
    new THREE.Vector2(0.11, -0.22),
    new THREE.Vector2(0.24, -0.10),
    new THREE.Vector2(0.37,  0.04),
    new THREE.Vector2(0.41,  0.18),
    new THREE.Vector2(0.36,  0.32),
    new THREE.Vector2(0.22,  0.42),
    new THREE.Vector2(0.00,  0.46)
  ];
  const globo = new THREE.LatheGeometry(globoPts, 28);

  // Cabeza de pico forjada y curvada con bisel
  const picoShape = new THREE.Shape();
  picoShape.moveTo(-0.30, -0.07);
  picoShape.quadraticCurveTo(-0.15,  0.04, 0.0,  0.045);
  picoShape.quadraticCurveTo( 0.15,  0.04, 0.30, -0.07);
  picoShape.quadraticCurveTo( 0.14, -0.01, 0.0, -0.015);
  picoShape.quadraticCurveTo(-0.14, -0.01, -0.30, -0.07);
  const picoCabeza = new THREE.ExtrudeGeometry(picoShape, {
    depth: 0.036,
    bevelEnabled: true,
    bevelThickness: 0.01,
    bevelSize: 0.008,
    bevelSegments: 2
  });
  picoCabeza.translate(0, 0, -0.018);

  return {
    barril,
    globo,
    picoCabeza,
    remache: new THREE.SphereGeometry(0.026, 8, 8)
  };
})();

function addEdges(mesh, color = 0x0f172a, opacity = 0.35) {
  const edges = new THREE.LineSegments(
    new THREE.EdgesGeometry(mesh.geometry),
    new THREE.LineBasicMaterial({ color, transparent: true, opacity })
  );
  mesh.add(edges);
}

function crearBadgeNumero(texto) {
  const canvas = document.createElement('canvas');
  canvas.width = 128;
  canvas.height = 128;
  const ctx = canvas.getContext('2d');
  ctx.fillStyle = 'rgba(15, 23, 42, 0.9)';
  ctx.beginPath();
  ctx.arc(64, 64, 48, 0, Math.PI * 2);
  ctx.fill();
  ctx.strokeStyle = '#fbbf24';
  ctx.lineWidth = 8;
  ctx.stroke();
  ctx.fillStyle = '#fef08a';
  ctx.font = 'bold 54px system-ui, sans-serif';
  ctx.textAlign = 'center';
  ctx.textBaseline = 'middle';
  ctx.fillText(texto, 64, 68);

  const tex = new THREE.CanvasTexture(canvas);
  const sprite = new THREE.Sprite(new THREE.SpriteMaterial({ map: tex, depthTest: false }));
  sprite.scale.set(0.65, 0.65, 1);
  return sprite;
}

function crearTensorCuerda(dx, dy, dz, longitud = 0.55) {
  const tramo = new THREE.Group();
  const dir = new THREE.Vector3(dx, dy, dz).normalize();
  const perp = Math.abs(dir.y) < 0.9 ? new THREE.Vector3(0, 1, 0) : new THREE.Vector3(1, 0, 0);
  const sideDir = new THREE.Vector3().crossVectors(dir, perp).normalize();
  const upDir = new THREE.Vector3().crossVectors(sideDir, dir).normalize();

  // Tres cabos trenzados alrededor del eje
  [0, (Math.PI * 2) / 3, (Math.PI * 4) / 3].forEach(ang => {
    const offset = sideDir.clone().multiplyScalar(Math.cos(ang) * 0.032)
      .add(upDir.clone().multiplyScalar(Math.sin(ang) * 0.032));
    const cabo = new THREE.Mesh(new THREE.CylinderGeometry(0.026, 0.026, longitud, 8), MATERIALES.Cuerda);
    cabo.castShadow = true;
    cabo.quaternion.setFromUnitVectors(new THREE.Vector3(0, 1, 0), dir);
    cabo.position.copy(dir.clone().multiplyScalar(longitud / 2)).add(offset);
    tramo.add(cabo);
  });

  // Abrazadera de bronce intermedia
  const anilla = new THREE.Mesh(new THREE.TorusGeometry(0.055, 0.015, 8, 16), MATERIALES.Bronce);
  anilla.quaternion.setFromUnitVectors(new THREE.Vector3(0, 0, 1), dir);
  anilla.position.copy(dir.clone().multiplyScalar(longitud * 0.55));
  tramo.add(anilla);

  // Nudo terminal reforzado
  const nudo = new THREE.Mesh(new THREE.SphereGeometry(0.088, 14, 14), MATERIALES.CuerdaNudo);
  nudo.position.copy(dir.clone().multiplyScalar(longitud));
  nudo.castShadow = true;
  tramo.add(nudo);

  return tramo;
}

// Aplica opacidad opcional sin mutar los materiales globales (útil para plantas inferiores en el Editor)
function aplicarEstiloGrupo(group, { opacidad = 1, paredTranslucida = false } = {}) {
  if (opacidad >= 0.99 && !paredTranslucida) return group;
  group.traverse(child => {
    if (child.isMesh && child.material) {
      const matClon = child.material.clone();
      matClon.transparent = true;
      matClon.opacity = Math.min(matClon.opacity ?? 1, opacidad);
      child.material = matClon;
    }
  });
  return group;
}

// ============================================================================
// 4. SKINS DE CASILLAS
// ============================================================================

export function crearMallaCasilla(celda, x, y, z, opciones = {}) {
  const tipo = celda.tipo;
  const group = new THREE.Group();
  group.position.set(x, z, y);

  if (tipo === 'SueloBase') {
    const losa = new THREE.Mesh(new THREE.BoxGeometry(0.92, 0.14, 0.92), MATERIALES.SueloBase);
    losa.position.y = 0.43;
    losa.receiveShadow = true;

    const marco = new THREE.Mesh(new THREE.BoxGeometry(0.98, 0.10, 0.98), MATERIALES.SueloBorde);
    marco.position.y = 0.40;
    marco.receiveShadow = true;
    addEdges(marco, 0x0f172a, 0.5);
    group.add(marco, losa);

  } else if (tipo === 'Placa' || tipo === 'PlacaTemporizada') {
    const esTemp = tipo === 'PlacaTemporizada';
    const matLuz = esTemp ? MATERIALES.PlacaTemp : MATERIALES.Placa;

    // Base octogonal metálica biselada
    const base = new THREE.Mesh(new THREE.CylinderGeometry(0.42, 0.46, 0.08, 8), MATERIALES.AceroOscuro);
    base.position.y = -0.46;
    base.receiveShadow = true;

    // Núcleo energético iluminado
    const nucleo = new THREE.Mesh(new THREE.CylinderGeometry(0.32, 0.35, 0.09, 24), matLuz);
    nucleo.position.y = -0.445;

    // Anillo holográfico flotante que gira suavemente
    const anillo = new THREE.Mesh(
      new THREE.TorusGeometry(0.28, 0.018, 10, 32),
      matLuz
    );
    anillo.rotation.x = Math.PI / 2;
    anillo.position.y = -0.35;

    // 4 pines sensores en las esquinas
    [[-0.32, -0.32], [0.32, -0.32], [-0.32, 0.32], [0.32, 0.32]].forEach(([px, pz]) => {
      const pin = new THREE.Mesh(new THREE.CylinderGeometry(0.03, 0.03, 0.12, 8), MATERIALES.Cromo);
      pin.position.set(px, -0.43, pz);
      group.add(pin);
    });

    group.add(base, nucleo, anillo);
    group.userData.onTick = (t) => {
      anillo.position.y = -0.34 + Math.sin(t * 3 + x + y) * 0.025;
      anillo.rotation.z = t * 1.4;
    };

  } else if (tipo === 'Escalera') {
    // 4 largueros cilíndricos metálicos con anclajes
    const posteGeo = new THREE.CylinderGeometry(0.055, 0.055, 0.98, 12);
    const anclajeGeo = new THREE.CylinderGeometry(0.085, 0.085, 0.08, 12);
    [-0.36, 0.36].forEach(dx => {
      [-0.36, 0.36].forEach(dz => {
        const p = new THREE.Mesh(posteGeo, MATERIALES.Escalera);
        p.position.set(dx, 0, dz);
        p.castShadow = true;
        p.receiveShadow = true;
        group.add(p);

        [-0.45, 0.45].forEach(ay => {
          const cap = new THREE.Mesh(anclajeGeo, MATERIALES.AceroOscuro);
          cap.position.set(dx, ay, dz);
          group.add(cap);
        });
      });
    });

    // 4 peldaños antideslizantes iluminados
    const peldanoGeo = new THREE.BoxGeometry(0.76, 0.065, 0.76);
    [-0.36, -0.12, 0.12, 0.36].forEach(dy => {
      const s = new THREE.Mesh(peldanoGeo, MATERIALES.EscaleraPeldano);
      s.position.y = dy;
      s.castShadow = true;
      s.receiveShadow = true;
      addEdges(s, 0x3b0764, 0.4);
      group.add(s);
    });

  } else if (tipo === 'PuertaBoton' || tipo === 'PuertaPalanca') {
    const esBoton = tipo === 'PuertaBoton';
    const matPuerta = esBoton ? MATERIALES.PuertaBoton : MATERIALES.PuertaPalanca;

    // Bloque blindado central
    const core = new THREE.Mesh(new THREE.BoxGeometry(0.88, 0.90, 0.88), matPuerta);
    core.castShadow = true;
    core.receiveShadow = true;
    group.add(core);

    // Marco estructural oscuro superior, inferior y esquinas
    [-0.44, 0.44].forEach(dy => {
      const cornisa = new THREE.Mesh(new THREE.BoxGeometry(0.96, 0.08, 0.96), MATERIALES.AceroOscuro);
      cornisa.position.y = dy;
      cornisa.castShadow = true;
      group.add(cornisa);
    });
    [-0.42, 0.42].forEach(dx => {
      [-0.42, 0.42].forEach(dz => {
        const pilar = new THREE.Mesh(new THREE.BoxGeometry(0.12, 0.96, 0.12), MATERIALES.AceroOscuro);
        pilar.position.set(dx, 0, dz);
        group.add(pilar);
      });
    });

    // Emblema de cierre en las 4 caras laterales
    const rotaciones = [0, Math.PI / 2, Math.PI, -Math.PI / 2];
    rotaciones.forEach(ry => {
      const cara = new THREE.Group();
      cara.rotation.y = ry;

      const aro = new THREE.Mesh(new THREE.TorusGeometry(0.20, 0.032, 10, 20), MATERIALES.Cromo);
      aro.position.z = 0.45;
      cara.add(aro);

      const centro = new THREE.Mesh(
        new THREE.CylinderGeometry(0.08, 0.08, 0.06, esBoton ? 16 : 6),
        esBoton ? MATERIALES.Boton : MATERIALES.Bronce
      );
      centro.rotation.x = Math.PI / 2;
      centro.position.z = 0.45;
      cara.add(centro);

      group.add(cara);
    });

  } else if (tipo === 'PuertaTemporizada') {
    // 4 emisores de esquina siempre presentes
    [-0.42, 0.42].forEach(dx => {
      [-0.42, 0.42].forEach(dz => {
        const emisor = new THREE.Mesh(new THREE.CylinderGeometry(0.055, 0.055, 0.96, 12), MATERIALES.AceroOscuro);
        emisor.position.set(dx, 0, dz);
        group.add(emisor);
      });
    });

    if (!celda.abierta) {
      // Campo de fuerza translúcido de cristal energético
      const campo = new THREE.Mesh(new THREE.BoxGeometry(0.88, 0.92, 0.88), MATERIALES.PuertaTemp);
      campo.castShadow = true;
      campo.receiveShadow = true;
      addEdges(campo, 0xfef08a, 0.75);
      group.add(campo);

      // Barras láser horizontales que oscilan
      const laser = new THREE.Mesh(new THREE.BoxGeometry(0.90, 0.04, 0.90), MATERIALES.LuzMinero);
      group.add(laser);
      group.userData.onTick = (t) => {
        laser.position.y = Math.sin(t * 4) * 0.36;
      };
    } else {
      const marco = new THREE.LineSegments(
        new THREE.EdgesGeometry(new THREE.BoxGeometry(0.94, 0.94, 0.94)),
        new THREE.LineBasicMaterial({ color: 0xfbbf24, transparent: true, opacity: 0.65 })
      );
      group.add(marco);
      if (celda.tiempo > 0) {
        const badge = crearBadgeNumero(String(celda.tiempo));
        badge.position.y = 0.65;
        group.add(badge);
      }
    }

  } else if (tipo === 'Pared') {
    // Bloque arquitectónico con núcleo de sillería y pilastras biseladas
    const core = new THREE.Mesh(new THREE.BoxGeometry(0.90, 0.92, 0.90), MATERIALES.Pared);
    core.castShadow = true;
    core.receiveShadow = true;
    group.add(core);

    [-0.43, 0.43].forEach(dy => {
      const moldura = new THREE.Mesh(new THREE.BoxGeometry(0.96, 0.08, 0.96), MATERIALES.ParedColumna);
      moldura.position.y = dy;
      moldura.castShadow = true;
      moldura.receiveShadow = true;
      group.add(moldura);
    });

    [-0.42, 0.42].forEach(dx => {
      [-0.42, 0.42].forEach(dz => {
        const esquina = new THREE.Mesh(new THREE.BoxGeometry(0.11, 0.95, 0.11), MATERIALES.ParedColumna);
        esquina.position.set(dx, 0, dz);
        group.add(esquina);
      });
    });

  } else {
    // Limite u otras casillas estructurales
    const m = new THREE.Mesh(new THREE.BoxGeometry(0.96, 0.96, 0.96), MATERIALES[tipo] || MATERIALES.Limite);
    m.castShadow = true;
    m.receiveShadow = true;
    addEdges(m, 0x334155, 0.45);
    group.add(m);
  }

  return aplicarEstiloGrupo(group, opciones);
}

// ============================================================================
// 5. SKINS DE OBJETOS
// ============================================================================

export function crearMallaObjeto(obj, x, y, z, ocupadasSet = null, opciones = {}) {
  const tipo = obj.tipo;
  const group = new THREE.Group();
  group.position.set(x, z, y);

  if (tipo === 'Caja') {
    // Núcleo de tablones de madera
    const nucleo = new THREE.Mesh(new THREE.BoxGeometry(0.78, 0.78, 0.78), MATERIALES.Caja);
    nucleo.castShadow = true;
    nucleo.receiveShadow = true;
    group.add(nucleo);

    // 12 listones exteriores de carpintería
    [-0.40, 0.40].forEach(dy => {
      const tapa = new THREE.Mesh(new THREE.BoxGeometry(0.88, 0.09, 0.88), MATERIALES.CajaMarco);
      tapa.position.y = dy;
      tapa.castShadow = true;
      group.add(tapa);
    });
    [-0.39, 0.39].forEach(dx => {
      [-0.39, 0.39].forEach(dz => {
        const poste = new THREE.Mesh(new THREE.BoxGeometry(0.10, 0.88, 0.10), MATERIALES.CajaMarco);
        poste.position.set(dx, 0, dz);
        poste.castShadow = true;
        group.add(poste);
      });
    });

    // Travesaños diagonales en las caras frontales/traseras
    [-0.40, 0.40].forEach(dz => {
      const diag = new THREE.Mesh(new THREE.BoxGeometry(0.92, 0.08, 0.05), MATERIALES.CajaMarco);
      diag.position.z = dz;
      diag.rotation.z = Math.PI / 4;
      group.add(diag);
    });

  } else if (tipo === 'CajaMetal') {
    // Cubo blindado de acero con refuerzos en X y 8 esquineras remachadas
    const core = new THREE.Mesh(new THREE.BoxGeometry(0.82, 0.82, 0.82), MATERIALES.CajaMetal);
    core.castShadow = true;
    core.receiveShadow = true;
    group.add(core);

    // Costillas cruzadas en las 6 caras
    const ribA = new THREE.Mesh(new THREE.BoxGeometry(0.86, 0.12, 0.86), MATERIALES.AceroOscuro);
    const ribB = new THREE.Mesh(new THREE.BoxGeometry(0.12, 0.86, 0.86), MATERIALES.AceroOscuro);
    const ribC = new THREE.Mesh(new THREE.BoxGeometry(0.86, 0.86, 0.12), MATERIALES.AceroOscuro);
    group.add(ribA, ribB, ribC);

    // 8 esquineras metálicas con remaches cromados
    [-0.38, 0.38].forEach(dx => {
      [-0.38, 0.38].forEach(dy => {
        [-0.38, 0.38].forEach(dz => {
          const esquina = new THREE.Mesh(new THREE.BoxGeometry(0.15, 0.15, 0.15), MATERIALES.AceroOscuro);
          esquina.position.set(dx, dy, dz);
          esquina.castShadow = true;
          group.add(esquina);

          const remache = new THREE.Mesh(GEOS.remache, MATERIALES.Cromo);
          remache.position.set(dx * 1.15, dy * 1.15, dz * 1.15);
          group.add(remache);
        });
      });
    });

  } else if (tipo === 'Barril') {
      // Cuerpo torneado abombado de roble (ya cerrado arriba y abajo)
      const body = new THREE.Mesh(GEOS.barril, MATERIALES.Barril);
      body.castShadow = true;
      body.receiveShadow = true;
      group.add(body);

      // 4 aros de hierro forjado ajustados a la curvatura
      [
        { y: -0.34, r: 0.355 },
        { y: -0.13, r: 0.395 },
        { y:  0.13, r: 0.395 },
        { y:  0.34, r: 0.355 }
      ].forEach(({ y: dy, r }) => {
        const aro = new THREE.Mesh(new THREE.TorusGeometry(r, 0.024, 8, 24), MATERIALES.AroMetal);
        aro.rotation.x = Math.PI / 2;
        aro.position.y = dy;
        group.add(aro);
      });

      // Travesaño y tapón en la tapa superior hundida
      const travesano = new THREE.Mesh(new THREE.BoxGeometry(0.52, 0.025, 0.09), MATERIALES.CajaMarco);
      travesano.position.y = 0.415;
      travesano.receiveShadow = true;

      const taponSup = new THREE.Mesh(new THREE.CylinderGeometry(0.038, 0.038, 0.03, 12), MATERIALES.Bronce);
      taponSup.position.set(0.14, 0.418, 0.12);

      // Tapón lateral de bronce
      const taponLat = new THREE.Mesh(new THREE.CylinderGeometry(0.045, 0.045, 0.06, 12), MATERIALES.Bronce);
      taponLat.rotation.z = Math.PI / 2;
      taponLat.position.set(0.39, 0, 0);

      group.add(travesano, taponSup, taponLat);

  } else if (tipo === 'Globo') {
    const subGrupo = new THREE.Group();

    // Envoltura aerostática barnizada en lágrima
    const esfera = new THREE.Mesh(GEOS.globo, MATERIALES.Globo);
    esfera.castShadow = true;
    subGrupo.add(esfera);

    // Meridianos dorados alrededor del globo
    [0, Math.PI / 2].forEach(ry => {
      const meridiano = new THREE.Mesh(new THREE.TorusGeometry(0.405, 0.012, 8, 32), MATERIALES.Bronce);
      meridiano.position.y = 0.14;
      meridiano.rotation.y = ry;
      subGrupo.add(meridiano);
    });

    // Válvula superior y boquilla inferior de bronce
    const corona = new THREE.Mesh(new THREE.CylinderGeometry(0.06, 0.09, 0.05, 14), MATERIALES.Bronce);
    corona.position.y = 0.46;
    const boquilla = new THREE.Mesh(new THREE.CylinderGeometry(0.08, 0.06, 0.06, 14), MATERIALES.Bronce);
    boquilla.position.y = -0.27;

    // Cuerda de amarre y lastre inferior
    const hilo = new THREE.Mesh(new THREE.CylinderGeometry(0.018, 0.018, 0.18, 8), MATERIALES.Cuerda);
    hilo.position.y = -0.37;
    const lastre = new THREE.Mesh(new THREE.TorusGeometry(0.06, 0.018, 8, 16), MATERIALES.Bronce);
    lastre.position.y = -0.46;

    subGrupo.add(corona, boquilla, hilo, lastre);
    group.add(subGrupo);

    // Flotación suave continua
    group.userData.onTick = (t) => {
      subGrupo.position.y = Math.sin(t * 2.6 + x + y) * 0.035;
      subGrupo.rotation.z = Math.sin(t * 1.8 + x) * 0.04;
    };

  } else if (tipo === 'Cuerda') {
    // Nudo central esculpido con abrazaderas de bronce
    const nudoCentral = new THREE.Mesh(new THREE.SphereGeometry(0.14, 18, 18), MATERIALES.CuerdaNudo);
    nudoCentral.castShadow = true;
    const aroCentral = new THREE.Mesh(new THREE.TorusGeometry(0.145, 0.022, 10, 20), MATERIALES.Bronce);
    aroCentral.rotation.x = Math.PI / 2;
    group.add(nudoCentral, aroCentral);

    const vecinos = [
      { h: [1, 0, 0],  t: [1, 0, 0] },
      { h: [-1, 0, 0], t: [-1, 0, 0] },
      { h: [0, 1, 0],  t: [0, 0, 1] },
      { h: [0, -1, 0], t: [0, 0, -1] },
      { h: [0, 0, 1],  t: [0, 1, 0] },
      { h: [0, 0, -1], t: [0, -1, 0] }
    ];

    let conectados = 0;
    if (ocupadasSet) {
      vecinos.forEach(v => {
        const key = `${x + v.h[0]},${y + v.h[1]},${z + v.h[2]}`;
        if (ocupadasSet.has(key)) {
          group.add(crearTensorCuerda(v.t[0], v.t[1], v.t[2], 0.56));
          conectados++;
        }
      });
    }

    if (conectados === 0) {
      group.add(crearTensorCuerda(-1, 0, 0, 0.44));
      group.add(crearTensorCuerda(1, 0, 0, 0.44));
    }

  } else if (tipo === 'Boton') {
    // Pedestal industrial octogonal con anillo de peligro y cúpula luminosa
    const base = new THREE.Mesh(new THREE.CylinderGeometry(0.36, 0.42, 0.14, 8), MATERIALES.AceroOscuro);
    base.position.y = -0.43;
    base.castShadow = true;

    const collar = new THREE.Mesh(new THREE.CylinderGeometry(0.28, 0.31, 0.06, 16), MATERIALES.Peligro);
    collar.position.y = -0.34;

    const pulsador = new THREE.Mesh(
      new THREE.SphereGeometry(0.22, 20, 14, 0, Math.PI * 2, 0, Math.PI / 2),
      MATERIALES.Boton
    );
    pulsador.position.y = -0.32;
    pulsador.castShadow = true;

    group.add(base, collar, pulsador);

  } else if (tipo.startsWith('Palanca')) {
    const abierta = tipo === 'PalancaAbierta';
    const matEstado = abierta ? MATERIALES.PalancaAbierta : MATERIALES.PalancaCerrada;

    // Caja de engranajes anclada al suelo
    const base = new THREE.Mesh(new THREE.BoxGeometry(0.56, 0.14, 0.44), MATERIALES.AceroOscuro);
    base.position.y = -0.43;
    base.castShadow = true;

    const tambor = new THREE.Mesh(new THREE.CylinderGeometry(0.16, 0.16, 0.16, 16), MATERIALES.Cromo);
    tambor.rotation.x = Math.PI / 2;
    tambor.position.y = -0.34;

    // Brazo articulado de la palanca
    const pivote = new THREE.Group();
    pivote.position.y = -0.34;
    pivote.rotation.z = abierta ? -0.48 : 0.48;

    const mastil = new THREE.Mesh(new THREE.CylinderGeometry(0.038, 0.048, 0.54, 12), MATERIALES.Cromo);
    mastil.position.y = 0.27;
    mastil.castShadow = true;

    const empunadura = new THREE.Mesh(new THREE.CylinderGeometry(0.065, 0.065, 0.16, 14), MATERIALES.JugadorTraje);
    empunadura.position.y = 0.48;

    const cristal = new THREE.Mesh(new THREE.OctahedronGeometry(0.095, 0), matEstado);
    cristal.position.y = 0.60;

    pivote.add(mastil, empunadura, cristal);

    // Piloto LED indicador en la base
    const led = new THREE.Mesh(new THREE.BoxGeometry(0.12, 0.05, 0.06), matEstado);
    led.position.set(abierta ? 0.18 : -0.18, -0.35, 0.21);

    group.add(base, tambor, pivote, led);

  } else {
    const m = new THREE.Mesh(new THREE.BoxGeometry(0.88, 0.88, 0.88), MATERIALES[tipo] || MATERIALES.Caja);
    m.castShadow = true;
    m.receiveShadow = true;
    addEdges(m, 0x0f172a, 0.45);
    group.add(m);
  }

  return aplicarEstiloGrupo(group, opciones);
}

// ============================================================================
// 6. SKINS DE ÍTEMS RECOGIBLES
// ============================================================================

const SKIN_ITEMS = {
  Jetpack: () => {
    const g = new THREE.Group();

    // Placa dorsal y regulador central
    const placa = new THREE.Mesh(new THREE.BoxGeometry(0.18, 0.32, 0.08), MATERIALES.AceroOscuro);
    placa.position.z = -0.04;
    const nucleo = new THREE.Mesh(new THREE.OctahedronGeometry(0.075, 0), MATERIALES.Boton);
    nucleo.position.set(0, 0.04, 0.03);
    g.add(placa, nucleo);

    // Twin thrusters con aletas y llamas
    [-0.13, 0.13].forEach(dx => {
      const tanque = new THREE.Mesh(new THREE.CylinderGeometry(0.075, 0.075, 0.42, 16), MATERIALES.ItemJetpack);
      tanque.position.set(dx, 0, 0);
      tanque.castShadow = true;

      const cupula = new THREE.Mesh(new THREE.SphereGeometry(0.075, 16, 12), MATERIALES.Cromo);
      cupula.position.set(dx, 0.21, 0);

      const anillo = new THREE.Mesh(new THREE.TorusGeometry(0.08, 0.015, 8, 16), MATERIALES.Bronce);
      anillo.rotation.x = Math.PI / 2;
      anillo.position.set(dx, -0.06, 0);

      const tobera = new THREE.Mesh(new THREE.CylinderGeometry(0.055, 0.085, 0.09, 14), MATERIALES.AceroOscuro);
      tobera.position.set(dx, -0.24, 0);

      const llama = new THREE.Mesh(new THREE.ConeGeometry(0.06, 0.16, 12), MATERIALES.LlamaPropulsor);
      llama.rotation.x = Math.PI;
      llama.position.set(dx, -0.35, 0);

      g.add(tanque, cupula, anillo, tobera, llama);
    });

    g.rotation.z = 0.22;
    return g;
  },

  Pico: () => {
    const g = new THREE.Group();

    // Mango ergonómico con empuñadura y virola de bronce
    const mango = new THREE.Mesh(new THREE.CylinderGeometry(0.032, 0.040, 0.58, 12), MATERIALES.MaderaMango);
    mango.castShadow = true;

    const grip = new THREE.Mesh(new THREE.CylinderGeometry(0.042, 0.042, 0.16, 12), MATERIALES.JugadorTraje);
    grip.position.y = -0.18;

    const virola = new THREE.Mesh(new THREE.CylinderGeometry(0.052, 0.052, 0.08, 12), MATERIALES.Bronce);
    virola.position.y = 0.21;

    // Cabeza de pico forjada y biselada
    const cabeza = new THREE.Mesh(GEOS.picoCabeza, MATERIALES.ItemPicoCabeza);
    cabeza.position.y = 0.22;
    cabeza.castShadow = true;

    g.add(mango, grip, virola, cabeza);
    g.rotation.z = -0.42;
    return g;
  }
};

export function crearMallaItem(item, x, y, z, opciones = {}) {
  const group = new THREE.Group();
  const baseY = z - 0.06;
  group.position.set(x, baseY, y);

  const modelo = new THREE.Group();
  const fabrica = SKIN_ITEMS[item.tipo];
  if (fabrica) {
    modelo.add(fabrica());
  } else {
    modelo.add(new THREE.Mesh(new THREE.OctahedronGeometry(0.24, 0), MATERIALES.JugadorAnillo));
  }
  group.add(modelo);

  // Doble halo holográfico en el suelo
  const haloExt = new THREE.Mesh(
    new THREE.RingGeometry(0.27, 0.33, 28),
    new THREE.MeshBasicMaterial({ color: 0x38bdf8, side: THREE.DoubleSide, transparent: true, opacity: 0.75 })
  );
  haloExt.rotation.x = -Math.PI / 2;
  haloExt.position.y = -0.38;

  const haloInt = new THREE.Mesh(
    new THREE.RingGeometry(0.14, 0.18, 24),
    new THREE.MeshBasicMaterial({ color: 0xfef08a, side: THREE.DoubleSide, transparent: true, opacity: 0.65 })
  );
  haloInt.rotation.x = -Math.PI / 2;
  haloInt.position.y = -0.375;

  group.add(haloExt, haloInt);

  // Flotación y giro continuo
  group.userData.onTick = (t) => {
    modelo.position.y = Math.sin(t * 3.2 + x + y) * 0.055;
    modelo.rotation.y = t * 1.8;
    haloExt.scale.setScalar(1 + Math.sin(t * 4) * 0.06);
  };

  return aplicarEstiloGrupo(group, opciones);
}

// ============================================================================
// 7. SKINS DE JUGADOR Y EQUIPAMIENTO POR ROL
// ============================================================================

const ADORNOS_ROL = {
  Normal: () => null,

  Jetpack: () => {
    const g = new THREE.Group();
    const mochila = new THREE.Mesh(new THREE.BoxGeometry(0.22, 0.28, 0.08), MATERIALES.AceroOscuro);
    mochila.position.set(0, 0.02, -0.22);
    g.add(mochila);

    const llamas = [];
    [-0.12, 0.12].forEach(dx => {
      const tanque = new THREE.Mesh(new THREE.CylinderGeometry(0.065, 0.065, 0.34, 14), MATERIALES.ItemJetpack);
      tanque.position.set(dx, 0.02, -0.27);
      tanque.castShadow = true;

      const tapa = new THREE.Mesh(new THREE.SphereGeometry(0.065, 12, 10), MATERIALES.Cromo);
      tapa.position.set(dx, 0.19, -0.27);

      const tobera = new THREE.Mesh(new THREE.CylinderGeometry(0.045, 0.07, 0.07, 12), MATERIALES.AceroOscuro);
      tobera.position.set(dx, -0.17, -0.27);

      const llama = new THREE.Mesh(new THREE.ConeGeometry(0.055, 0.16, 12), MATERIALES.LlamaPropulsor);
      llama.rotation.x = Math.PI;
      llama.position.set(dx, -0.28, -0.27);
      llamas.push(llama);

      g.add(tanque, tapa, tobera, llama);
    });

    g.userData.llamas = llamas;
    return g;
  },

  Minero: () => {
    const g = new THREE.Group();

    // Casco de minero con visera y nervaduras
    const casco = new THREE.Mesh(
      new THREE.SphereGeometry(0.24, 20, 14, 0, Math.PI * 2, 0, Math.PI * 0.52),
      MATERIALES.CascoMinero
    );
    casco.position.y = 0.25;
    casco.castShadow = true;

    const visera = new THREE.Mesh(
      new THREE.CylinderGeometry(0.245, 0.26, 0.035, 20, 1, false, -Math.PI * 0.35, Math.PI * 0.7),
      MATERIALES.CascoMinero
    );
    visera.position.set(0, 0.25, 0.03);

    // Linterna frontal con reflector cromado y haz luminoso
    const carcasaFoco = new THREE.Mesh(new THREE.CylinderGeometry(0.062, 0.052, 0.08, 14), MATERIALES.AceroOscuro);
    carcasaFoco.rotation.x = Math.PI / 2;
    carcasaFoco.position.set(0, 0.33, 0.23);

    const lente = new THREE.Mesh(new THREE.CylinderGeometry(0.052, 0.052, 0.02, 14), MATERIALES.LuzMinero);
    lente.rotation.x = Math.PI / 2;
    lente.position.set(0, 0.33, 0.27);

    // Pico en bandolera a la espalda
    const funda = new THREE.Group();
    const mangoEspalda = new THREE.Mesh(new THREE.CylinderGeometry(0.024, 0.028, 0.44, 10), MATERIALES.MaderaMango);
    const cabezaEspalda = new THREE.Mesh(GEOS.picoCabeza, MATERIALES.ItemPicoCabeza);
    cabezaEspalda.scale.setScalar(0.75);
    cabezaEspalda.position.y = 0.16;
    funda.add(mangoEspalda, cabezaEspalda);
    funda.position.set(0, 0.02, -0.25);
    funda.rotation.z = 0.55;

    g.add(casco, visera, carcasaFoco, lente, funda);
    return g;
  }
};

export function crearMallaJugador(rol = 'Normal', x = 0, y = 0, z = 0, opciones = {}) {
  const group = new THREE.Group();
  group.position.set(x, z, y);

  const avatar = new THREE.Group();
  const matAcento = rol === 'Minero' ? MATERIALES.CascoMinero : MATERIALES.JugadorAnillo;

  // Botas articuladas
  [-0.11, 0.11].forEach(bx => {
    const bota = new THREE.Mesh(new THREE.BoxGeometry(0.14, 0.11, 0.20), MATERIALES.JugadorTraje);
    bota.position.set(bx, -0.43, 0.02);
    bota.castShadow = true;
    avatar.add(bota);
  });

  // Torso blindado
  const torso = new THREE.Mesh(new THREE.CylinderGeometry(0.21, 0.24, 0.34, 20), MATERIALES.Jugador);
  torso.position.y = -0.18;
  torso.castShadow = true;
  avatar.add(torso);

  // Cinturón energético
  const cinturon = new THREE.Mesh(new THREE.TorusGeometry(0.235, 0.025, 10, 28), matAcento);
  cinturon.rotation.x = Math.PI / 2;
  cinturon.position.y = -0.26;
  avatar.add(cinturon);

  // Hombreras flotantes
  [-0.27, 0.27].forEach(sx => {
    const hombrera = new THREE.Mesh(new THREE.SphereGeometry(0.085, 14, 12), MATERIALES.Jugador);
    hombrera.scale.set(0.8, 1.15, 1.0);
    hombrera.position.set(sx, -0.05, 0);
    hombrera.castShadow = true;
    avatar.add(hombrera);
  });

  // Cabeza / Casco con visor frontal luminoso
  const cabeza = new THREE.Mesh(new THREE.SphereGeometry(0.22, 22, 18), MATERIALES.Jugador);
  cabeza.position.y = 0.22;
  cabeza.castShadow = true;
  avatar.add(cabeza);

  const visor = new THREE.Mesh(
    new THREE.CylinderGeometry(0.226, 0.226, 0.085, 20, 1, false, -Math.PI * 0.32, Math.PI * 0.64),
    matAcento
  );
  visor.position.y = 0.22;
  avatar.add(visor);

  // Halo dorsal superior
  const halo = new THREE.Mesh(new THREE.TorusGeometry(0.14, 0.016, 10, 24), matAcento);
  halo.rotation.x = Math.PI / 2;
  halo.position.y = 0.49;
  avatar.add(halo);

  // Equipamiento específico del rol
  const adorno = ADORNOS_ROL[rol]?.();
  if (adorno) avatar.add(adorno);

  group.add(avatar);

  // Animación continua: respiración/levitación y parpadeo de propulsores
  group.userData.onTick = (t) => {
    avatar.position.y = Math.sin(t * 3.5 + x + y) * (rol === 'Jetpack' ? 0.045 : 0.018);
    halo.rotation.z = t * 1.5;
    if (adorno?.userData?.llamas) {
      const flicker = 0.85 + Math.sin(t * 28) * 0.25;
      adorno.userData.llamas.forEach(l => { l.scale.y = flicker; });
    }
  };

  return aplicarEstiloGrupo(group, opciones);
}