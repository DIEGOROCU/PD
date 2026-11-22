/**
 * Contexto compartido de Three.js (escena, cámara, iluminación de estudio, sombras,
 * OrbitControls y bucle de animación continua).
 */

import * as THREE from 'three';
import { OrbitControls } from 'three/addons/controls/OrbitControls.js';
import { RoomEnvironment } from 'three/addons/environments/RoomEnvironment.js';

export function crearContextoEscena(contenedor = document.body) {
  const scene = new THREE.Scene();
  scene.fog = new THREE.FogExp2(0x162033, 0.008);

  const camera = new THREE.PerspectiveCamera(45, window.innerWidth / window.innerHeight, 0.1, 1000);
  camera.position.set(8, 10, 11);

  const renderer = new THREE.WebGLRenderer({ antialias: true, alpha: true });
  renderer.setSize(window.innerWidth, window.innerHeight);
  renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
  renderer.shadowMap.enabled = true;
  renderer.shadowMap.type = THREE.PCFSoftShadowMap;
  renderer.toneMapping = THREE.ACESFilmicToneMapping;
  renderer.toneMappingExposure = 1.5;
  contenedor.appendChild(renderer.domElement);

  // Entorno de estudio para reflejos reales en metales y cristales
  const pmrem = new THREE.PMREMGenerator(renderer);
  scene.environment = pmrem.fromScene(new RoomEnvironment(), 0.04).texture;
  pmrem.dispose();

  const controls = new OrbitControls(camera, renderer.domElement);
  controls.enableDamping = true;
  controls.dampingFactor = 0.06;

  // 1. Luz ambiental suave para levantar las sombras más oscuras
  scene.add(new THREE.AmbientLight(0xffffff, 0.45));

  // 2. Luz hemisférica con rebote de suelo claro
  scene.add(new THREE.HemisphereLight(0xf0f9ff, 0x64748b, 1.25));

  // 3. Luz direccional principal con sombras suaves
  const dirLight = new THREE.DirectionalLight(0xffffff, 2.0);
  dirLight.position.set(14, 24, 16);
  dirLight.castShadow = true;
  dirLight.shadow.mapSize.set(2048, 2048);
  dirLight.shadow.camera.near = 1;
  dirLight.shadow.camera.far = 60;
  const d = 18;
  dirLight.shadow.camera.left = -d;
  dirLight.shadow.camera.right = d;
  dirLight.shadow.camera.top = d;
  dirLight.shadow.camera.bottom = -d;
  dirLight.shadow.bias = -0.0005;
  scene.add(dirLight);

  // 4. Luz de relleno lateral/trasera para iluminar las caras opuestas al sol
  const fillLight = new THREE.DirectionalLight(0xbae6fd, 0.95);
  fillLight.position.set(-14, 12, -12);
  scene.add(fillLight);

  const worldGroup = new THREE.Group();
  scene.add(worldGroup);

  function limpiarMundo() {
    while (worldGroup.children.length) {
      worldGroup.remove(worldGroup.children[0]);
    }
  }

  function centrarCamaraEnMapa(mapa) {
    if (!mapa?.length) return;
    const xs = mapa.map(c => c.pos[0]);
    const ys = mapa.map(c => c.pos[1]);
    const zs = mapa.map(c => c.pos[2]);
    const cx = (Math.min(...xs) + Math.max(...xs)) / 2;
    const cy = (Math.min(...ys) + Math.max(...ys)) / 2;
    const cz = (Math.min(...zs) + Math.max(...zs)) / 2;
    const span = Math.max(
      Math.max(...xs) - Math.min(...xs),
      Math.max(...ys) - Math.min(...ys),
      Math.max(...zs) - Math.min(...zs),
      3
    );
    controls.target.set(cx, cz, cy);
    camera.position.set(cx + span * 0.9, cz + span * 0.9, cy + span * 1.15);
  }

  window.addEventListener('resize', () => {
    camera.aspect = window.innerWidth / window.innerHeight;
    camera.updateProjectionMatrix();
    renderer.setSize(window.innerWidth, window.innerHeight);
  });

  function iniciarBucle(onFrame) {
    const clock = new THREE.Clock();
    function animate() {
      requestAnimationFrame(animate);
      const t = clock.getElapsedTime();
      worldGroup.traverse(obj => obj.userData?.onTick?.(t));
      onFrame?.();
      controls.update();
      renderer.render(scene, camera);
    }
    animate();
  }

  return {
    scene,
    camera,
    renderer,
    controls,
    worldGroup,
    limpiarMundo,
    centrarCamaraEnMapa,
    iniciarBucle
  };
}