
;;; ==================== Paquetes base ====================


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Asegura que use-package esté disponible
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)


;;  Arreglo para highlight-indent-guides en modo -nw o sin tema cargado
(with-eval-after-load 'highlight-indent-guides
  ;; Evita el cálculo automático de colores (que falla sin face `default`)
  (setq highlight-indent-guides-auto-enabled nil)
  (setq highlight-indent-guides-method 'character)
  ;; Usa un color fijo más seguro
  (set-face-foreground 'highlight-indent-guides-character-face "gray35"))

;; Si se estaba activando automáticamente, desactívalo:
(remove-hook 'prog-mode-hook #'highlight-indent-guides-mode)

;; Puedes reactivarlo manualmente si quieres:
;; (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)


;;  Desactiva highlight-indent-guides globalmente
(when (boundp 'highlight-indent-guides-mode)
  (remove-hook 'prog-mode-hook #'highlight-indent-guides-mode)
  (remove-hook 'java-mode-hook #'highlight-indent-guides-mode)
  (remove-hook 'after-change-major-mode-hook #'highlight-indent-guides-mode))

;; Y fuerza que arranque desactivado
(setq-default highlight-indent-guides-mode nil)



;;; ==================== Desarrollo ====================

(require 'eglot)

(defun my/maybe-eglot ()
  "Usar Eglot en prog-mode, excepto en Java."
  (unless (derived-mode-p 'java-mode)
    (eglot-ensure)))

(add-hook 'prog-mode-hook #'my/maybe-eglot)



;; Portapapeles
(setq select-enable-primary t
      select-enable-clipboard t)

;; Paréntesis automáticos
(electric-pair-mode 1)

;; Guardado automático cada 60s
(run-with-timer 0 60 (lambda () (save-some-buffers t)))

;; Re-map Ctrl+Z a undo
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)

;;; ==================== Org mode (base) ====================
(use-package org
  :pin gnu
  :config 
  ;; Selector de tags de una sola tecla
  (setq org-use-fast-tag-selection t
        org-fast-tag-selection-single-key t)

  ;; Estados TODO (flujo académico)
  (setq org-todo-keywords
        '((sequence "POR_HACER(t)" "EN_PROGRESO(p)" "BLOQUEADO(b)" "|" "HECHO(d)" "ENTREGADO(e)")))

  ;; Calidad de vida
  (setq org-log-done 'time
        org-return-follows-link t
        org-hide-emphasis-markers t)

  ;; Asociar .org a org-mode y hooks útiles
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (add-hook 'org-mode-hook #'org-indent-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)

  ;; Atajos útiles
  (define-key global-map (kbd "C-c l") #'org-store-link)
  (define-key global-map (kbd "C-c a") #'org-agenda)
  (define-key global-map (kbd "C-c c") #'org-capture)
  (define-key org-mode-map (kbd "C-c <up>")   #'org-priority-up)
  (define-key org-mode-map (kbd "C-c <down>") #'org-priority-down)
  (define-key org-mode-map (kbd "C-c C-g C-r") #'org-shiftmetaright)

  ;; --- Catálogo de ETIQUETAS (SIN '/': Org no acepta '/')
  (setq org-tag-alist
        '(
          ;; === Materia (elige 1) ===
          (:startgroup . nil)
          ("m_ciberseguridad"   . ?1)
          ("m_pentesting"       . ?2)
          ("m_prog_red"         . ?3)
          ("m_enrutamiento"     . ?4)
          ("m_paradigmas"       . ?5)
          ("m_conmutacion_lan"  . ?6)
          (:endgroup . nil)

          ;; === Tipo de actividad (elige 1) ===
          (:startgroup . nil)
          ("t_tarea"    . ?t)
          ("t_lab"      . ?l)
          ("t_proyecto" . ?p)
          ("t_examen"   . ?x)
          ("t_lectura"  . ?r)
          (:endgroup . nil)

          ;; === Fase pentesting (elige 1 si aplica) ===
          (:startgroup . nil)
          ("pt_recon"   . ?R)
          ("pt_enum"    . ?N)
          ("pt_exploit" . ?E)
          ("pt_postex"  . ?O)
          ("pt_report"  . ?I)
          (:endgroup . nil)

          ;; === Paradigma (elige 1 si aplica) ===
          (:startgroup . nil)
          ("par_imperativo" . ?I)
          ("par_oo"         . ?O)
          ("par_funcional"  . ?F)
          ("par_logico"     . ?L)
          (:endgroup . nil)

          ;; === Urgencia (elige 1) ===
          (:startgroup . nil)
          ("u_hoy"    . ?h)
          ("u_pronto" . ?u)
          ("u_normal" . ?n)
          (:endgroup . nil)

          ;; === Tags libres ===
          ;; Herramientas
          ("tool_wireshark"    . ?w)
          ("tool_nmap"         . ?m)
          ("tool_metasploit"   . ?s)
          ("tool_python"       . ?y)
          ("tool_bash"         . ?b)
          ("tool_gns3"         . ?g)
          ("tool_packettracer" . ?k)

          ;; Red / capa / protocolos
          ("net_L2"   . ?2)
          ("net_L3"   . ?3)
          ("net_vlan" . ?v)
          ("net_stp"  . ?z)
          ("net_ospf" . ?o)
          ("net_rip"  . ?R)  ;; comparte tecla con pt_recon
          ("net_nat"  . ?a)
          ("net_dhcp" . ?d)

          ;; Contexto / flags
          ("flag_critico"   . ?c)
          ("ctx_individual" . ?i)
          ("ctx_equipo"     . ?e)
          ("ctx_campus"     . ?C)
          ("ctx_remoto"     . ?M)
          )))

;;; === Colores de etiquetas + refresco robusto (GUI y -nw) ===
(with-eval-after-load 'org
  ;; Neutraliza color base del tema para etiquetas
  (set-face-attribute 'org-tag nil
                      :inherit 'default :foreground nil :background nil :weight 'bold)

  ;; Colores por etiqueta (nombres EXACTOS con org-tag-alist)
  (setq org-tag-faces
        '(
          ;; Materias
          ("m_ciberseguridad"  . (:inherit nil :foreground "#8be9fd" :weight bold))
          ("m_pentesting"      . (:inherit nil :foreground "#bd93f9" :weight bold))
          ("m_prog_red"        . (:inherit nil :foreground "#50fa7b" :weight bold))
          ("m_enrutamiento"    . (:inherit nil :foreground "#f1fa8c" :weight bold))
          ("m_paradigmas"      . (:inherit nil :foreground "#ffb86c" :weight bold))
          ("m_conmutacion_lan" . (:inherit nil :foreground "#ff79c6" :weight bold))
          ;; Tipo de actividad
          ("t_tarea"    . (:inherit nil :foreground "#82aaff" :weight bold))
          ("t_lab"      . (:inherit nil :foreground "#89ddff" :weight bold))
          ("t_proyecto" . (:inherit nil :foreground "#c792ea" :weight bold))
          ("t_examen"   . (:inherit nil :foreground "#ff5370" :weight bold))
          ("t_lectura"  . (:inherit nil :foreground "#c3e88d" :weight bold))
          ;; Fase pentesting
          ("pt_recon"   . (:inherit nil :foreground "#00bcd4" :weight bold))
          ("pt_enum"    . (:inherit nil :foreground "#26a69a" :weight bold))
          ("pt_exploit" . (:inherit nil :foreground "#ff5555" :weight bold))
          ("pt_postex"  . (:inherit nil :foreground "#ffb86c" :weight bold))
          ("pt_report"  . (:inherit nil :foreground "#b39ddb" :weight bold))
          ;; Paradigmas
          ("par_imperativo" . (:inherit nil :foreground "#a6accd" :weight bold))
          ("par_oo"         . (:inherit nil :foreground "#82aaff" :weight bold))
          ("par_funcional"  . (:inherit nil :foreground "#c3e88d" :weight bold))
          ("par_logico"     . (:inherit nil :foreground "#f78c6c" :weight bold))
          ;; Urgencia
          ("u_hoy"    . (:inherit nil :foreground "#ff1744" :weight bold :box t))
          ("u_pronto" . (:inherit nil :foreground "#ffa000" :weight bold))
          ("u_normal" . (:inherit nil :foreground "#9e9e9e" :weight bold))
          ;; Herramientas
          ("tool_wireshark"    . (:inherit nil :foreground "#00bcd4" :weight bold))
          ("tool_nmap"         . (:inherit nil :foreground "#64b5f6" :weight bold))
          ("tool_metasploit"   . (:inherit nil :foreground "#ab47bc" :weight bold))
          ("tool_python"       . (:inherit nil :foreground "#9ccc65" :weight bold))
          ("tool_bash"         . (:inherit nil :foreground "#a1887f" :weight bold))
          ("tool_gns3"         . (:inherit nil :foreground "#00acc1" :weight bold))
          ("tool_packettracer" . (:inherit nil :foreground "#26c6da" :weight bold))
          ;; Red / protocolos
          ("net_L2"   . (:inherit nil :foreground "#00e5ff" :weight bold))
          ("net_L3"   . (:inherit nil :foreground "#1de9b6" :weight bold))
          ("net_vlan" . (:inherit nil :foreground "#ff80ab" :weight bold))
          ("net_stp"  . (:inherit nil :foreground "#ffd740" :weight bold))
          ("net_ospf" . (:inherit nil :foreground "#69f0ae" :weight bold))
          ("net_rip"  . (:inherit nil :foreground "#b388ff" :weight bold))
          ("net_nat"  . (:inherit nil :foreground "#ffab91" :weight bold))
          ("net_dhcp" . (:inherit nil :foreground "#a5d6a7" :weight bold))
          ;; Contexto / flags
          ("flag_critico"   . (:inherit nil :foreground "red1" :weight bold :box t))
          ("ctx_individual" . (:inherit nil :foreground "#b0bec5" :weight bold))
          ("ctx_equipo"     . (:inherit nil :foreground "#81a1c1" :weight bold))
          ("ctx_campus"     . (:inherit nil :foreground "#a3be8c" :weight bold))
          ("ctx_remoto"     . (:inherit nil :foreground "#d08770" :weight bold))
          ))

  ;; Reconstruye regex internas y refresca buffers
  (defun my/org-rebuild-tag-faces-and-refresh ()
    "Reconstruye regex de tags y refresca buffers Org."
    (org-set-regexps-and-options)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'org-mode)
          (if (fboundp 'org-restart-font-lock)
              (org-restart-font-lock)
            (font-lock-flush)
            (font-lock-ensure))))))

  ;; Hooks/advice para mantener colores
  (add-hook 'org-mode-hook #'my/org-rebuild-tag-faces-and-refresh)
  (add-hook 'after-make-frame-functions (lambda (_f) (my/org-rebuild-tag-faces-and-refresh)))
  (advice-add 'load-theme :after (lambda (&rest _args) (my/org-rebuild-tag-faces-and-refresh)))

  ;; Comando manual
  (defun my/apply-org-tag-faces () (interactive) (my/org-rebuild-tag-faces-and-refresh))

  ;; Ejecuta una vez al cargar org
  (my/org-rebuild-tag-faces-and-refresh))

;;; ==================== Agenda: mejoras y vista "d" ====================
(with-eval-after-load 'org-agenda
  ;; Ajustes de calidad de vida en la Agenda
  (setq org-agenda-window-setup 'current-window
        org-agenda-start-on-weekday nil
        org-agenda-start-day "today"
        org-agenda-span 7
        org-deadline-warning-days 7
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-deadline-prewarning-if-scheduled t
        org-agenda-skip-timestamp-if-deadline-is-shown t
        org-agenda-show-all-dates t
        org-agenda-format-date "%a %d %b"
        org-agenda-prefix-format '((agenda  . " %i %-12:c%?-12t% s")
                                   (timeline . " % s")
                                   (todo    . " %i %-12:c%T ")
                                   (tags    . " %i %-12:c")
                                   (search  . " %i %-12:c"))
        org-agenda-sorting-strategy
        '((agenda time-up priority-down category-keep)
          (todo   priority-down deadline-up)
          (tags   priority-down)
          (search category-keep)))

  ;; Función para saltar por prioridad (A/B/C)
  (defun air-org-skip-subtree-if-priority (priority)
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (pri-value (* 1000 (- org-lowest-priority priority)))
          (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current) subtree-end nil)))

  ;; Vista "d": diario + TODOs por prioridad
  (setq org-agenda-custom-commands
        '(("d" "Daily agenda and all TODOs"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "High-priority unfinished tasks:")))
            (agenda "" ((org-agenda-span 7)))
            (alltodo ""
                     ((org-agenda-skip-function
                       '(or (air-org-skip-subtree-if-priority ?A)
                            (air-org-skip-subtree-if-priority ?C)
                            (org-agenda-skip-if nil '(scheduled deadline))))
                      (org-agenda-overriding-header "ALL normal priority tasks:")))
            (tags "PRIORITY=\"C\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "Low-priority unfinished tasks:"))))
           ((org-agenda-compact-blocks nil)))))

  ;; Atajo directo para abrir "d"
  (global-set-key (kbd "C-c A")
                  (lambda () (interactive) (org-agenda nil "d"))))

;;; ==================== Fechas estilo Día-Mes-Año ====================
(setq org-display-custom-times t)
(setq org-time-stamp-custom-formats '("<%d-%m-%Y>" . "<%d-%m-%Y %H:%M>"))

(with-eval-after-load 'org-agenda
  (setq org-agenda-format-date "%a %d-%m-%Y"))

;;; ==================== (Opcional) Clipboard en terminal ====================
;; Si quieres compartir portapapeles en -nw, descomenta UNO:
;; (use-package wl-clipboard :if (and (not (display-graphic-p)) (executable-find "wl-copy"))
;;   :init (wl-clipboard-mode 1))
;; (use-package xclip :if (and (not (display-graphic-p)) (executable-find "xclip"))
;;   :init (xclip-mode 1))
;; (use-package clipetty :init (global-clipetty-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(beacon centaur-tabs company-anaconda dashboard doom-modeline
	    doom-themes eglot-java emms forge highlight-indent-guides
	    indent-bars indent-guide lsp-java lsp-javacomp lsp-ui
	    org-appear org-modern org-roam org-super-agenda
	    org-superstar rainbow-delimiters telega toc-org
	    treemacs-nerd-icons valign yasnippet-snippets)))

;; --------------- CONFIRUACIÓN PARA QUE LOS .ORG TENGAN ASI ESTILO BONITO EN AUTOMATICO

;; Recomendado si incluyes hyperref en el preámbulo de cada .org:
;; evita que Org cargue su propia versión de hyperref y lo duplique.
(with-eval-after-load 'ox-latex
  (setq org-latex-with-hyperref nil))

;; TREEMACS WE EL ṔINXE TREEMACS PARA VFERSE MAS PRO ALAVERGA
;;(use-package treemacs
;;  :ensure t
;;  :defer t
;;  :bind
;;  (("M-0"       . treemacs-select-window)
;;   ("C-x t 1"   . treemacs-delete-other-windows)
;;   ("C-x t t"   . treemacs)
;;   ("C-x t B"   . treemacs-bookmark)
;;   ("C-x t C-t" . treemacs-find-file))
;;  :config
;;  (setq treemacs-is-never-other-window t))
;;  (setq treemacs-default-directory default-directory)
;;  (setq treemacs-project-follow-mode t)

(use-package treemacs
  :ensure t
  :defer t
  :bind
  (("M-0"       . treemacs-select-window)
   ("C-x t 1"   . treemacs-delete-other-windows)
   ("C-x t t"   . treemacs)
   ("C-x t B"   . treemacs-bookmark)
   ("C-x t C-t" . treemacs-find-file))
  :config
  (setq treemacs-is-never-other-window t
        treemacs-follow-mode t
        treemacs-persist-file nil
        max-lisp-eval-depth 5000)

  (defun my/treemacs-open-current-dir ()
    "Abrir Treemacs en el directorio actual del buffer."
    (interactive)
    (let ((dir (if (buffer-file-name)
                   (file-name-directory (buffer-file-name))
                 default-directory)))
      (treemacs-select-window)
      (treemacs-display-current-project-exclusively)
      (treemacs-root-up)
      (treemacs-find-file)))

  ;; tecla rápida para abrir en el directorio actual
  (global-set-key (kbd "C-x t h") 'my/treemacs-open-current-dir))



(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))


(use-package which-key
  :ensure t
  :init (which-key-mode))



(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-outrun-electric t))  ;; tema



(doom-themes-visual-bell-config)  ;; campanita visual cuando hay error
(doom-themes-org-config)          ;; mejora visual de Org mode


(use-package treemacs-nerd-icons
  :after treemacs nerd-icons
  :ensure t
  :config
  (treemacs-load-theme "nerd-icons"))



;; Colorea los paréntesis por niveles (brutal para programar)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :ensure t)

;;  Muestra guías visuales de indentación (para no perderte en el código)
(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'column
        highlight-indent-guides-responsive 'top))

;;  Opcional pero útil: muestra número de línea y columna
(setq display-line-numbers-type 'absolute)
(global-display-line-numbers-mode t)
(column-number-mode t)


;;(use-package centaur-tabs
;  :ensure t
;  :config
;  (centaur-tabs-mode t)
;  (setq centaur-tabs-set-icons t
;        centaur-tabs-style "bar"
;        centaur-tabs-height 32))



(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))


;;  Transparencia de ventana (solo GUI)
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))  ;; (ACTUAL . INACTIVO)
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))



(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  ;;(setq dashboard-startup-banner 'logo) ;; o 'official, o una ruta a imagen
  (setq dashboard-startup-banner "/home/dankbian/.emacs.d/ascii/intro.txt")
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)
                          (bookmarks . 5)
                          (agenda . 5))))

;; ---------------------------
;; Fondo translúcido en Emacs -nw
;; ---------------------------
(when (not (display-graphic-p))  ;; Solo en terminal
  ;; Ajusta la transparencia del fondo (últimos dos dígitos son alfa hexadecimal)
  (set-face-background 'default "#000000B0")  ;; B0 ~ 70% opaco
  ;; Opcional: texto menos intenso si quieres
  (set-face-foreground 'default "#DDDDDD"))




;; JAVA

;(use-package lsp-mode
;  :ensure t
;  :hook (java-mode . lsp)
;  :commands lsp)



;; org-appear

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode))


;; ESTO ES PARA LOS * Y LOS TRIANGULOS DINAMICOS
(require 'org)
(require 'outline)

;;;; org-custom-triangles-mode --- Reemplaza asteriscos de Org por triángulos de colores dinámicos
;; Definición de caras (faces) para cada nivel de encabezado:
(defface org-custom-triangle-level-1
  '((t :foreground "#00fdfd"))
  "Face para triángulo de encabezado nivel 1.")
(defface org-custom-triangle-level-2
  '((t :foreground "#39FF14"))
  "Face para triángulo de encabezado nivel 2.")
(defface org-custom-triangle-level-3
  '((t :foreground "#fe0ab7"))
  "Face para triángulo de encabezado nivel 3 (morado neón).")
(defface org-custom-triangle-level-4
  '((t :foreground "#55ffe2"))
  "Face para triángulo de encabezado nivel 4 (cian).")
(defface org-custom-triangle-level-5
  '((t :foreground "#c2ff05"))
  "Face para triángulo de encabezado nivel 5 (verde fosforescente).")

(defun org-custom-triangles--symbol (level has-child folded)
  "Devuelve el símbolo correcto según LEVEL, HAS-CHILD y FOLDED.
Niveles 1–2: triángulo sólido.
Niveles 3+: triángulo hueco."
  (cond
   ;; Sin hijos: siempre como expandido
   ((not has-child)
    (if (<= level 2) "▼ " "▽ "))
   ;; Con hijos y PLEGADO
   (folded
    (if (<= level 2) "▼ " "▷ "))  ;; puedes cambiar ▷ si quieres otro cerrado para niveles bajos
   ;; Con hijos y EXPANDIDO
   (t
    (if (<= level 2) "▼ " "▽ "))))
  

;; Variables para los símbolos de triángulo:
;;(defvar org-custom-triangle-open "▾" "Símbolo de triángulo para encabezado expandido.")
;;(defvar org-custom-triangle-closed "▸" "Símbolo de triángulo para encabezado plegado.")

;; Variable interna para almacenar los overlays creados (por buffer):
(defvar-local org-custom-triangles--overlays nil
  "Lista de overlays de triángulos aplicados en el buffer actual.")

;; Función auxiliar: determina si el encabezado en POINT tiene hijos.
(defun org-custom-triangles--has-child-p (point)
  "Devuelve t si el encabezado en POINT (inicio de línea) tiene al menos un subencabezado hijo."
  (save-excursion
    (goto-char point)
    (org-goto-first-child)))  ;; org-goto-first-child mueve al primer hijo si existe, devuelve t si tuvo éxito.

;; Función auxiliar: verifica si un encabezado en POINT está actualmente plegado.
(defun org-custom-triangles--folded-p (point)
  "Devuelve t si el encabezado en POINT está plegado (subárbol oculto)."
  (save-excursion
    (goto-char point)
    (let ((has-child (org-custom-triangles--has-child-p point)))
      (when has-child
        ;; Si tiene hijo, comprobar si el primer hijo está invisible:
        (org-goto-first-child)
        (let ((child-pos (point)))
          (goto-char point)  ;; volver al encabezado original
          (outline-invisible-p child-pos))))))

;; Función principal para crear un overlay de triángulo en un encabezado dado:
(defun org-custom-triangles--make-overlay (point level)
  "Crea un overlay de triángulo en el encabezado de nivel LEVEL que comienza en POINT."
  (save-excursion
    (goto-char point)
    ;; Determinar símbolo y face según estado:
    (let* ((has-child (org-custom-triangles--has-child-p point))
           (folded (org-custom-triangles--folded-p point))
           (symbol (org-custom-triangles--symbol level has-child folded))
           (face-symbol (pcase level
                          (1 'org-custom-triangle-level-1)
                          (2 'org-custom-triangle-level-2)
                          (3 'org-custom-triangle-level-3)
                          (4 'org-custom-triangle-level-4)
                          (5 'org-custom-triangle-level-5)
                          (_ 'org-custom-triangle-level-5))))  ;; niveles >5 reutilizan nivel 5 (se puede ajustar)
      ;; Calcular rango de asteriscos a cubrir (desde inicio de línea hasta el espacio después de los asteriscos):
      (when (looking-at "^\\*+ ")
        (let ((stars-end (match-end 0))
              (ov (make-overlay (match-beginning 0) (match-end 0))))
          ;; Configurar overlay: mostrar el símbolo en lugar de los asteriscos
          (overlay-put ov 'display (propertize symbol 'face face-symbol))
          (overlay-put ov 'org-custom-triangle t)      ; marca para identificar nuestros overlays
          (overlay-put ov 'org-level level)            ; guarda nivel (opcional, por si se usa más adelante)
          (push ov org-custom-triangles--overlays)
          ov)))))

;; Función para inicializar todos los overlays en el buffer actual:
(defun org-custom-triangles--apply-all ()
  "Aplica overlays de triángulo a todos los encabezados del buffer Org actual."
  (save-excursion
    (goto-char (point-min))
    ;; Recorrer todos los encabezados
    (while (re-search-forward "^\\*+ " nil t)
      (let* ((beg (match-beginning 0))
             (level (length (match-string 0))));; match-string 0 incluye los '*' y el espacio
        ;; La longitud de match-string 0 será (# de * + 1), pero como incluye el espacio final, el nivel es length-1
        (setq level (max 1 (1- level)))
        (org-custom-triangles--make-overlay beg level)))))

;; Función para actualizar el triángulo de un encabezado en POINT (inicio de línea)
(defun org-custom-triangles--update-at (point)
  "Actualiza el símbolo de triángulo en el encabezado que comienza en POINT."
  (save-excursion
    (goto-char point)
    (when (looking-at "^\\*+ ")
      (let* ((level (max 1 (1- (length (match-string 0)))))
             (has-child (org-custom-triangles--has-child-p point))
             (folded (org-custom-triangles--folded-p point))
             (symbol (org-custom-triangles--symbol level has-child folded))
             (face-symbol (pcase level
                            (1 'org-custom-triangle-level-1)
                            (2 'org-custom-triangle-level-2)
                            (3 'org-custom-triangle-level-3)
                            (4 'org-custom-triangle-level-4)
                            (5 'org-custom-triangle-level-5)
                            (_ 'org-custom-triangle-level-5))))
        ;; Buscar el overlay existente en este encabezado:
        (let ((ovs (overlays-at point)) ov-found)
          (dolist (ov ovs)
            (when (overlay-get ov 'org-custom-triangle)
              (setq ov-found ov)))
          ;; Si existe, actualizarlo; si no existe (por alguna razón), crearlo:
          (if ov-found
              (overlay-put ov-found 'display (propertize symbol 'face face-symbol))
            (org-custom-triangles--make-overlay point level)))))))

;; Función para actualizar todos los triángulos (por ejemplo, tras un cambio global de visibilidad)
(defun org-custom-triangles--update-all ()
  "Actualiza todos los triángulos segun el estado actual de cada encabezado."
  (dolist (ov org-custom-triangles--overlays)
    (let ((pos (overlay-start ov)))
      (when pos
        (org-custom-triangles--update-at pos)))))

(defun my/org-refresh-all-heading-bullets ()
  "Activa triángulos y refresca encabezados Org en todos los buffers."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'org-mode)
        ;; Asegura que el modo esté activo
        (unless (bound-and-true-p org-custom-triangles-mode)
          (org-custom-triangles-mode 1))
        ;; Fuerza recalcular los triángulos según folded/children
        (when (fboundp 'org-custom-triangles--update-all)
          (org-custom-triangles--update-all))))))

(add-hook 'after-init-hook #'my/org-refresh-all-heading-bullets)


;; Funciones manejadoras de hooks:
(defun org-custom-triangles--after-cycle (state)
  "Función para `org-cycle-hook`. Actualiza triángulo(s) tras plegar/desplegar.
STATE es el estado de visibilidad resultante (ej. 'folded, 'children, 'subtree, 'overview, 'contents, 'all)."
  ;; Si es un ciclo local (folded/children/subtree):
  (cond
   ((eq state 'folded)
    ;; El encabezado actual se plegó:
    (org-custom-triangles--update-at (line-beginning-position)))
   ((memq state '(children subtree))
    ;; El encabezado actual se desplegó (parcial o totalmente):
    (org-custom-triangles--update-at (line-beginning-position)))
   ((memq state '(overview contents all))
    ;; Ciclo global: actualizar todos los visibles
    (org-custom-triangles--update-all))))

(defun org-custom-triangles--after-insert ()
  "Función para `org-insert-heading-hook`. Crea/actualiza triángulos tras insertar un nuevo encabezado."
  ;; El nuevo encabezado comienza en la línea actual (después de M-RET):
  (let ((pt (line-beginning-position))
        (level (org-current-level)))
    (when level
      ;; Crear overlay para el nuevo encabezado:
      (org-custom-triangles--make-overlay pt level)
      ;; Si no es de nivel 1, entonces tiene un padre al que quizá haya que actualizar:
      (when (> level 1)
        (save-excursion
          (org-up-heading-safe) ;; mover al encabezado padre
          (org-custom-triangles--update-at (line-beginning-position)))))))

(defun org-custom-triangles--after-promote ()
  "Función para `org-after-promote-entry-hook`. Actualiza triángulos tras promover encabezado(s)."
  ;; Después de promover, el encabezado actual es de nivel menor (más alto en jerarquía).
  ;; Actualizar su propio triángulo (nuevo color, símbolo igual que antes):
  (org-custom-triangles--update-at (line-beginning-position))
  ;; Además, si tenía un padre antes, ese padre pudo haber perdido un hijo:
  (save-excursion
    (when (org-up-heading-safe)  ;; mover al antiguo padre (que ahora está arriba)
      (org-custom-triangles--update-at (line-beginning-position)))))

(defun org-custom-triangles--after-demote ()
  "Función para `org-after-demote-entry-hook`. Actualiza triángulos tras degradar encabezado(s)."
  ;; Después de degradar, el encabezado actual es de nivel mayor (más bajo en jerarquía).
  ;; Actualizar su triángulo (nuevo color):
  (org-custom-triangles--update-at (line-beginning-position))
  ;; Su nuevo padre (antes era un hermano de nivel superior) ahora ganó un hijo:
  (save-excursion
    (when (org-up-heading-safe)
      (org-custom-triangles--update-at (line-beginning-position)))))

(defun org-custom-triangles--after-refile-insert ()
  "Función para `org-after-refile-insert-hook`. Actualiza triángulos tras mover un subárbol (refile)."
  ;; Después de insertar el subárbol en nueva ubicación (antes de quitarlo de la antigua).
  ;; Actualizar nuevo padre:
  (save-excursion
    (when (org-up-heading-safe)
      (org-custom-triangles--update-at (line-beginning-position))))
  ;; Programar actualización del antiguo padre tras refile (usando un pequeño delay para después de la eliminación):
  (run-at-time 0.1 nil
               (lambda ()
                 (when (buffer-live-p org-refile-old-buffer)
                   (with-current-buffer org-refile-old-buffer
                     (save-excursion
                       (when org-refile-old-hash
                         ;; org-refile guarda org-refile-old-hash y org-refile-old-cmd-marker
                         ;; Podemos mover al marcador viejo y actualizar padre:
                         (goto-char (point-min))
                         (when (and org-refile-old-cmd-marker
                                    (marker-buffer org-refile-old-cmd-marker))
                           (goto-char org-refile-old-cmd-marker)
                           (when (org-up-heading-safe)
                             (org-custom-triangles--update-at (line-beginning-position)))))))))))

;; Definir la minor mode:
(define-minor-mode org-custom-triangles-mode
  "Minor mode para mostrar triángulos de color en encabezados Org en vez de asteriscos."
  :lighter ""  ;; sin indicador en el modeline
  (if org-custom-triangles-mode
      (progn
        ;; Al activar: crear overlays en todos los encabezados y registrar hooks.
        (org-custom-triangles--apply-all)
        ;; Hooks locales:
        (add-hook 'org-cycle-hook #'org-custom-triangles--after-cycle nil t)
        (add-hook 'org-insert-heading-hook #'org-custom-triangles--after-insert nil t)
        (add-hook 'org-after-promote-entry-hook #'org-custom-triangles--after-promote nil t)
        (add-hook 'org-after-demote-entry-hook #'org-custom-triangles--after-demote nil t)
        (add-hook 'org-after-refile-insert-hook #'org-custom-triangles--after-refile-insert nil t))
    ;; Al desactivar: quitar overlays y eliminar hooks.
    (remove-hook 'org-cycle-hook #'org-custom-triangles--after-cycle t)
    (remove-hook 'org-insert-heading-hook #'org-custom-triangles--after-insert t)
    (remove-hook 'org-after-promote-entry-hook #'org-custom-triangles--after-promote t)
    (remove-hook 'org-after-demote-entry-hook #'org-custom-triangles--after-demote t)
    (remove-hook 'org-after-refile-insert-hook #'org-custom-triangles--after-refile-insert t)
    ;; Eliminar todos los overlays creados:
    (mapc #'delete-overlay org-custom-triangles--overlays)
    (setq org-custom-triangles--overlays nil)))

;; Activar automáticamente la minor mode en org-mode:
(add-hook 'org-mode-hook #'org-custom-triangles-mode)



;; AQUI ACABAN LOS TRIANGUILOS DINAMICOS

(defun my/org-hide-stars ()
  "Oculta los asteriscos de los encabezados OrgMode (visualmente)."
  (font-lock-add-keywords
   nil
   '(("^\\(\\*+\\) "
      (1 '(face nil invisible org-hide) prepend)))
   'append))

(add-hook 'org-mode-hook #'my/org-hide-stars)


;====================================================================
;============= AQUI ACABA LA OCONFIGURACIÓN DE LOS TRIANGULOS
;============================================================

;; ESTO ES PARA QUE LAS TABLAS SE VEAN BONITAS 


;; ocultar title
(setq org-hidden-keywords '(title author date startup))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autokeywords t
        org-appear-delay 0.05))


;;; LINEAS DE IDENTACIÓN 


;;(setq org-startup-indented t)
;;(setq org-indent-indentation-per-level 2)
;; Caracter que rellena la indentación virtual
;;(require 'org-indent)
;;(setq org-indent-boundary-char ?│)   ;; o ?· 
;;(set-face-foreground 'org-indent "#07c0e0")  ;; FUNCIONA PERO MEH

;; desaparecer la barra fea de menu inservidble
(menu-bar-mode -1)


(defun my/header-line-breadcrumb ()
  "Muestra solo las últimas 3 carpetas de la ruta en la header-line."
  (if (or (minibufferp)
          (string-prefix-p " *" (buffer-name))) ; buffers internos tipo \" *Messages*\"
      ""
    (let* ((dir (or (if buffer-file-name
                        (file-name-directory buffer-file-name)
                      default-directory)
                   ""))
           ;; Abreviar $HOME a ~
           (abbr (abbreviate-file-name dir))
           ;; Partir en componentes y quitar vacíos
           (parts (split-string abbr "/" t))
           (len   (length parts))
           ;; Nos quedamos con los últimos 3 (o menos si no hay tantos)
           (tail  (last parts (min 3 len)))
           ;; Unir con ' > '
           (crumbs (mapconcat #'identity tail " > ")))
      (concat " " crumbs))))

;; Usar esto como header-line global
(setq-default header-line-format '((:eval (my/header-line-breadcrumb))))


;; barra de arriba
;; Estilo general de la header-line (la “barra” de arriba)
(set-face-attribute 'header-line nil
                    :background "#202020"    ;; color de fondo
                    :foreground "grey80"     ;; color base del texto
                    :box '(:line-width -1 :style released-button)) ;; borde tipo modeline

;; COLORES PARA LAS CARPETAS EN EL HEADER LINE
(defface my-header-breadcrumb-1
  '((t :foreground "white" :weight bold))
  "Carpeta más externa del breadcrumb."
  :group 'my-header-faces)

(defface my-header-breadcrumb-2
  '((t :foreground "#00afff" :weight bold))   ;; azul neón
  "Carpeta intermedia del breadcrumb."
  :group 'my-header-faces)

(defface my-header-breadcrumb-3
  '((t :foreground "#f700ff" :weight bold))   ;; verde chillón
  "Carpeta más interna (la actual)."
  :group 'my-header-faces)

(defface my-header-breadcrumb-separator
  '((t :foreground "grey60"))
  "Separador entre carpetas en el breadcrumb."
  :group 'my-header-faces)


(defun my/header-line-breadcrumb ()
  "Muestra solo las últimas 3 carpetas de la ruta, coloreadas por nivel."
  (if (or (minibufferp)
          (string-prefix-p " *" (buffer-name))) ; no mostrar en buffers internos
      ""
    (let* ((dir (or (if buffer-file-name
                        (file-name-directory buffer-file-name)
                      default-directory)
                   ""))
           ;; ~ en vez de /home/...
           (abbr  (abbreviate-file-name dir))
           ;; dividir en componentes
           (parts (split-string abbr "/" t))
           (len   (length parts))
           ;; últimas 3 (o menos si no hay tantas)
           (tail  (last parts (min 3 len)))
           ;; caras para cada nivel (izquierda→derecha)
           (faces (list 'my-header-breadcrumb-1
                        'my-header-breadcrumb-2
                        'my-header-breadcrumb-3))
           (i 0)
           (segments '()))
      ;; construir cada segmento coloreado
      (dolist (p tail)
        (let* ((face (nth i faces))
               (seg  (propertize p 'face face)))
          (push seg segments)
          (setq i (1+ i))))
      (setq segments (nreverse segments))
      (concat
       " "
       (mapconcat #'identity
                  segments
                  (propertize " > " 'face 'my-header-breadcrumb-separator))))))

(setq-default header-line-format '((:eval (my/header-line-breadcrumb))))
                    


;; QUE SE ILUMINE LA LINEA EN LA QUE ESTOY EN UNA TABLA

;; Asegúrate de cargar hl-line
(require 'hl-line)

;; Cambiar el color de hl-line DESPUÉS de que se cargue
(require 'org)

(with-eval-after-load 'hl-line
  (set-face-background 'hl-line "#663366") 
  (set-face-foreground 'hl-line nil))

(defun my/org-hl-line-only-in-tables ()
  "Resalta solo la parte de la fila de tabla donde está el punto."
  (setq-local
   hl-line-range-function
   (lambda ()
     (when (org-at-table-p)
       (save-excursion
         (beginning-of-line)
         ;; buscar la primera barra vertical de la tabla
         (if (search-forward "|" (line-end-position) t)
             ;; resaltar desde esa `|` hasta el final de la línea
             (cons (1- (point)) (line-end-position))
           ;; por si acaso no hay `|`, resaltar toda la línea
           (cons (line-beginning-position)
                 (line-end-position))))))))

(add-hook 'org-mode-hook #'my/org-hl-line-only-in-tables)
(add-hook 'org-mode-hook #'hl-line-mode)



;; Mode line activo (buffer actual)
(set-face-attribute 'mode-line nil
                    :background "#202020"          ;; fondo
                    :foreground "grey80"           ;; texto
                    :box '(:line-width 1
                          :color "#ff5fff"         ;; borde rosa intenso
                          :style released-button))

;; Mode line inactivo (otras ventanas)
(set-face-attribute 'mode-line-inactive nil
                    :background "#151515"
                    :foreground "grey50"
                    :box '(:line-width 1
                          :color "#444444"
                          :style released-button))


;; Mode line activo (buffer actual)
(set-face-attribute 'mode-line nil
                    :background "#202020"          ;; fondo
                    :foreground "grey80"           ;; texto
                    :box '(:line-width 1
                          :color "#ff5fff"         ;; borde rosa intenso
                          :style released-button))

;; Mode line inactivo (otras ventanas)
(set-face-attribute 'mode-line-inactive nil
                    :background "#151515"
                    :foreground "grey50"
                    :box '(:line-width 1
                          :color "#444444"
                          :style released-button))




;; FOCUS??????

(defun my/toggle-narrow-reading ()
  "Centrar un poco el texto para leer más cómodo."
  (interactive)
  (if (bound-and-true-p visual-fill-column-mode)
      (progn
        (visual-fill-column-mode -1)
        (visual-line-mode 1))
    (progn
      (visual-line-mode 1)
      (setq visual-fill-column-width 100
            visual-fill-column-center-text t)
      (visual-fill-column-mode 1))))

(use-package visual-fill-column
  :commands visual-fill-column-mode
  :bind (("C-c z" . my/toggle-narrow-reading)))









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  80 Java — configuración ligera y moderna


;; --- JDTLS / Eglot configuración sólida ---
(use-package eglot
  :ensure t
  :config
  ;; --- 1. Tiempo de espera y conexión ---
  (setq eglot-connect-timeout 60)
  (setq eglot-sync-connect nil)

  ;; --- 2. Desactivar features pesadas que causan lag ---
  (setq eglot-ignored-server-capabilities
        '(:inlayHintProvider :signatureHelpProvider :hoverProvider))

  ;; --- 3. Workspace persistente por proyecto ---
  (defun my/jdtls-workspace-dir ()
    "Devuelve un directorio de workspace único para cada proyecto."
    (let* ((root (or (project-root (project-current))
                     (expand-file-name "~/Java")))
           (dir (expand-file-name (md5 root) "~/.emacs.d/workspace-jdtls/")))
      (make-directory dir t)
      dir))

  ;; --- 4. Ruta al binario JDTLS ---
  (defvar my/jdtls-path
    (expand-file-name "~/.emacs.d/.cache/lsp/bin/jdtls"))

  ;; --- 5. Configurar Eglot para usar JDTLS ---
  (add-to-list 'eglot-server-programs
               '((java-mode java-ts-mode)
                 . (lambda ()
                     (list my/jdtls-path
                           "-configuration" (expand-file-name "~/.emacs.d/.cache/lsp/config_linux")
                           "-data" (my/jdtls-workspace-dir)))))

  ;; --- 6. Configuración base para JDTLS ---
  (setq eglot-workspace-configuration
        '((:java (:format (:settings (:url nil :profile nil))))))

  ;; --- 7. Mensaje de confirmación ---
  (message "Configuración JDTLS+Eglot cargada correctamente."))

;; --- Fin del bloque ---



;; --- Mejora de rendimiento y timeouts para Eglot + JDTLS ---
(setq eglot-connect-timeout 120)     ;; Espera hasta 120 segundos (default es 10)
(setq eglot-events-buffer-size 0)    ;; Sin límite para logs (útil para depurar)
(setq eglot-autoshutdown t)          ;; Cierra el servidor al cerrar el último buffer
(setq eglot-sync-connect nil)        ;; Conexión asincrónica (no bloquea Emacs)
(setq eglot-ignored-server-capabilities '(:inlayHintProvider))

;; Más memoria para JDTLS si el proyecto es grande
(defvar tkj/jdtls-max-mem "2G"
  "Memoria máxima para JDTLS. Usa valores como '1G', '2G', etc.")


(with-eval-after-load 'eglot
  (let* ((jdtls-dir (expand-file-name "~/.emacs.d/.cache/lsp/"))
         (launcher (car (file-expand-wildcards
                         (concat jdtls-dir "plugins/org.eclipse.equinox.launcher_*.jar"))))
         (config-dir (expand-file-name "config_linux" jdtls-dir))
         (workspace-dir (expand-file-name "~/.emacs.d/workspace-java/")))
    (if (and launcher (file-exists-p launcher) (file-exists-p config-dir))
        (progn
          (message "Usando JDTLS de %s" jdtls-dir)
          (add-to-list 'eglot-server-programs
                       `(java-mode . ("java"
                                      "-Declipse.application=org.eclipse.jdt.ls.core.id1"
                                      "-Dosgi.bundles.defaultStartLevel=4"
                                      "-Declipse.product=org.eclipse.jdt.ls.core.product"
                                      "-Dlog.protocol=true"
                                      "-Dlog.level=ALL"
                                      "--add-modules=ALL-SYSTEM"
                                      "-Xmx1G"
                                      "-jar" ,launcher
                                      "-configuration" ,config-dir
                                      "-data" ,workspace-dir))))
      (message "Could not find valid launcher/config for JDTLS at %s" jdtls-dir))))

(add-hook 'java-mode-hook #'eglot-ensure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun tkj/eglot-rename ()
  "Rename the symbol at point, using its current name as default."
  (interactive)
  (let ((symbol (thing-at-point 'symbol t)))
    (eglot-rename
     (minibuffer-with-setup-hook 'beginning-of-line
       (read-string "Rename to: " symbol)))))

(defun tkj/default-code-style-hook ()
  "Default Java code style: 2 spaces, no tabs, no final newline."
  (setq c-basic-offset 2
        c-label-offset 0
        indent-tabs-mode nil
        compile-command "~/bin/tc"
        require-final-newline nil))
(add-hook 'java-mode-hook 'tkj/default-code-style-hook)

(defun my-java-mode-hook ()
  "Extra setup for `java-mode'."
  (auto-fill-mode 1)
  (subword-mode 1)
  (yas-minor-mode 1)
  ;; Prefer flycheck instead of flymake
  (when (fboundp 'flymake-mode) (flymake-mode -1))
  (when (fboundp 'flycheck-mode) (flycheck-mode 1))
  ;; Reduce visual clutter in GUI
  (when window-system
    (set-fringe-style '(8 . 0)))
  ;; Indentation fixes
  (c-set-offset 'substatement-open 0)
  (when (assoc 'inexpr-class c-offsets-alist)
    (c-set-offset 'inexpr-class 0))
  (c-set-offset 'arglist-intro '++))
(add-hook 'java-mode-hook 'my-java-mode-hook)

(defun tkj/java-run-junit-at-point ()
  "Run the JUnit test method or class at point using Maven.
If point is on a method, only that test runs; otherwise runs the whole class."
  (interactive)
  (let* ((method-name (thing-at-point 'symbol t))
         (class-name (file-name-sans-extension
                      (file-name-nondirectory buffer-file-name)))
         (pom-file (locate-dominating-file
                    (or (buffer-file-name) default-directory) "pom.xml")))
    (if (and method-name class-name pom-file)
        (let* ((default-directory (file-name-directory pom-file))
               (test-param (if (string-equal class-name method-name)
                               class-name
                             (format "%s#%s" class-name method-name)))
               (command (format
                         "mvn --batch-mode --file %s test -Dtest=%s"
                         (expand-file-name "pom.xml" pom-file)
                         test-param)))
          (compilation-start command 'compilation-mode
                             `(lambda (_) (format "Run JUnit: %s" ,test-param))))
      (message "Unable to determine method, class, or pom.xml file."))))
(add-hook 'java-mode-hook
          (lambda () (local-set-key (kbd "C-c t") #'tkj/java-run-junit-at-point)))

(defun tkj/java-decompile-class ()
  "Decompile the current .class file with FernFlower and open the result.
Requires the `fernflower` command to be in your PATH."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (output-dir (concat (file-name-directory current-file) "decompiled/"))
         (decompiled-file (concat output-dir (file-name-base current-file) ".java"))
         (command (format "fernflower %s %s"
                          (shell-quote-argument current-file)
                          (shell-quote-argument output-dir))))
    (if (and current-file (string-equal (file-name-extension current-file) "class"))
        (progn
          (unless (file-directory-p output-dir)
            (make-directory output-dir t))
          (message "Running FernFlower decompiler...")
          (shell-command command)
          (if (file-exists-p decompiled-file)
              (find-file-other-window decompiled-file)
            (message "Error: Decompiled file not found at %s" decompiled-file)))
      (message "Error: This command can only be run on .class files."))))

(defun tkj/java-maven-coordinate-to-dependency ()
  "Convert the selected Maven coordinate (group:artifact:type:version)
to a full <dependency> XML block and replace the region."
  (interactive)
  (if (use-region-p)
      (let* ((coordinate (buffer-substring-no-properties
                          (region-beginning) (region-end)))
             (parts (split-string coordinate ":"))
             (group-id (nth 0 parts))
             (artifact-id (nth 1 parts))
             (type (nth 2 parts))
             (version (nth 3 parts))
             (dependency (format
                          "<dependency>
  <groupId>%s</groupId>
  <artifactId>%s</artifactId>
  <type>%s</type>
  <version>%s</version>
</dependency>"
                          group-id artifact-id type version))
             (start (region-beginning)))
        (delete-region (region-beginning) (region-end))
        (insert dependency)
        (indent-region start (point)))
    (message "No active region. Select a Maven coordinate to convert.")))


;======================================================================
;============ JAVA AUTOACOMPLETADO ===================================
;======================================================================
;; ===========================

;; ===========================
;; Configuración Java + Eglot
;; ===========================

(defun my/java--ensure-pom-safe ()
  "Crea un pom.xml mínimo si no existe, *después* de que Eglot se haya conectado.
Evita interferir con JDTLS en el hook de `java-mode`."
  (when (and buffer-file-name
             (derived-mode-p 'java-mode)
             (file-exists-p buffer-file-name))
    (run-with-idle-timer
     1 nil ;; espera 1 segundo
     (lambda ()
       (let* ((root (or (ignore-errors (project-root (project-current)))
                        (file-name-directory buffer-file-name)))
              (pom (and root (expand-file-name "pom.xml" root))))
         (when (and pom (not (file-exists-p pom)))
           (with-temp-file pom
             (insert
              "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
              "<project>\n"
              "  <modelVersion>4.0.0</modelVersion>\n"
              "  <groupId>auto.generated</groupId>\n"
              "  <artifactId>" (file-name-base buffer-file-name) "</artifactId>\n"
              "  <version>1.0-SNAPSHOT</version>\n"
              "</project>\n"))
           (message "Se creó automáticamente un pom.xml en %s" root)))))))

;; Solo añade el hook con la versión segura
(add-hook 'eglot-managed-mode-hook #'my/java--ensure-pom-safe)

;; ===========================
;;  Compilación y ejecución Java
;; ===========================

;;; ========= ASCII helpers =========
;;; ===============================
;;; ASCII display helper
;;; ===============================
(defun my/display-ascii-art-buffer (path)
  "Muestra ASCII art de PATH en buffer *Java ASCII* sin robar foco."
  (when (file-exists-p path)
    (let ((buf (get-buffer-create "*Java ASCII*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert-file-contents path)
          (goto-char (point-min))
          (view-mode 1)))
      (display-buffer buf '((display-buffer-at-bottom)
                            (window-height . fit-window-to-buffer))))))

;;; ===============================
;;; Configuración rutas
;;; ===============================
;; === CONFIG ASCII ===
(setq my/ascii-dir "~/.emacs.d/ascii/")
(setq my/ascii-building (concat my/ascii-dir "java_building.txt"))
(setq my/ascii-ok (concat my/ascii-dir "java_ok.txt"))
(setq my/ascii-run (concat my/ascii-dir "java_run.txt"))
;;; ===============================
;;; Compilar / Ejecutar
;;; ===============================
(defun my/java-close-compilation-window ()
  "Cierra la ventana de compilación si existe."
  (when-let ((win (get-buffer-window "*Java Compilation*")))
    (delete-window win)))

(defun my/java-compile ()
  "Compila el archivo Java actual mostrando ASCII y errores a la derecha."
  (interactive)
  (if (and buffer-file-name (file-exists-p buffer-file-name))
      (let* ((default-directory (file-name-directory buffer-file-name))
             (file (file-name-nondirectory buffer-file-name))
             (cmd (format "javac %s" (shell-quote-argument file)))
             (output-buffer (get-buffer-create "*Java Compilation*"))
             ;; Mostrar ventana a la derecha
             (display-buffer-overriding-action
              '((display-buffer-reuse-window display-buffer-in-side-window)
                (side . right)
                (window-width . 0.4))))
        (save-buffer)

        ;; Mostrar ASCII inicial tras crear el buffer
        (compilation-start cmd nil (lambda (_) "*Java Compilation*"))
        (run-at-time
         "0.15 sec" nil
         (lambda ()
           (when (get-buffer "*Java Compilation*")
             (with-current-buffer "*Java Compilation*"
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (when (file-exists-p my/ascii-building)
                   (insert-file-contents my/ascii-building))
                 (goto-char (point-max))                 
                 (compilation-mode))))))

        ;; Hook de finalización — local al buffer
        (add-hook
         'compilation-finish-functions
         (lambda (buf msg)
           (when (string= (buffer-name buf) "*Java Compilation*")
             (run-at-time
              "0.3 sec" nil
              (lambda ()
                (when (buffer-live-p buf)
                  (with-current-buffer buf
                    (let ((inhibit-read-only t))
                      (goto-char (point-max))
                      (insert "\n\n---------------------------------\n")
                      (if (string-match "finished" msg)
                          (progn
                            (when (file-exists-p my/ascii-ok)
                              (insert-file-contents my/ascii-ok))
                            (insert "\n Compilación exitosa.\n"))
                        (insert "\n Errores detectados.\n")))
                    (goto-char (point-max)))))))))
         :append :local)

        (message "[*] Compilando %s..." file))
    (user-error "[!] Este buffer no está asociado a un archivo Java."))
    
(defun my/java-run ()
  "Ejecuta la clase Java actual en una nueva ventana inferior."
  (interactive)
  (save-buffer)
  (my/java-close-compilation-window)
  (let* ((class (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
         (output-buffer (get-buffer-create "*Java Run*")))
    (with-current-buffer output-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when (file-exists-p my/ascii-run)
          (insert-file-contents my/ascii-run))
        (goto-char (point-max))
        (comint-mode)))
    (display-buffer-in-side-window output-buffer '((side . bottom) (window-height . 0.3)))
    (start-process "java-run" output-buffer "java" class)))

;;; ===============================
;;; Keybindings
;;; ===============================
(with-eval-after-load 'cc-mode
  (define-key java-mode-map (kbd "<f4>") #'my/java-compile)
  (define-key java-mode-map (kbd "<f5>") #'my/java-run))

;;; ===============================
;;; GIT
;;; ===============================

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package forge
  :ensure t
  :after magit)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; =======================
;; AUTOACOMPLETADO (SNIPPETS)
;; =======================
(use-package yasnippet-snippets
  :ensure t)

;; todos los snippets activos (Java, etc.)
(global-set-key (kbd "<f1>") #'yas-describe-tables)


;; Asegura que Emacs vea los binarios instalados con pipx
(add-to-list 'exec-path (expand-file-name "~/.local/bin"))
(setenv "PATH" (concat (expand-file-name "~/.local/bin") ":" (getenv "PATH")))

;; ==============================
;; Eglot + python-lsp-server
;; ==============================
(use-package eglot
  :ensure t
  :hook (python-mode . eglot-ensure)
  :config
  ;; Usa pylsp como servidor
  (setq eglot-server-programs
        '((python-mode . ("pylsp"))))

  ;; Configura complementos (linters, formateadores)
  (setq eglot-workspace-configuration
        '(:pylsp (:plugins (
            :pyflakes (:enabled t)
            :flake8 (:enabled t)
            :mccabe (:enabled t)
            :pylint (:enabled t)
            :rope_autoimport (:enabled t)
            :yapf (:enabled nil)
            :autopep8 (:enabled nil)
            :black (:enabled t)
            :isort (:enabled t)
            :pydocstyle (:enabled t)
            :jedi_completion (:enabled t)
            :jedi_definition (:enabled t)
            :jedi_hover (:enabled t)
            :jedi_references (:enabled t)
            :jedi_signature_help (:enabled t))))))

;; ==============================
;;  COMPANY: Autocompletado
;; ==============================
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-selection-wrap-around t))

;; ==============================
;;  Autoformato con black + isort
;; ==============================
(defun my/python-format ()
  "Formatea el buffer actual con black e isort."
  (interactive)
  (when (executable-find "isort")
    (shell-command (format "isort %s" (shell-quote-argument buffer-file-name))))
  (when (executable-find "black")
    (shell-command (format "black %s" (shell-quote-argument buffer-file-name))))
  (revert-buffer :ignore-auto :noconfirm))




;; ============================
;; ⚙️ Atajos Python básicos
;; ============================

(defun my/python-show-doc ()
  "Muestra documentación con Eldoc o desde anaconda/eglot."
  (interactive)
  (cond
   ;; Si está activo Eglot
   ((bound-and-true-p eglot-managed-mode)
    (call-interactively 'eldoc))
   ;; Fallback
   (t (message "No hay servidor activo para mostrar documentación."))))

(defun my/python-run ()
  "Ejecuta el script actual en un buffer *Python Run*."
  (interactive)
  (if buffer-file-name
      (let ((buf "*Python Run*"))
        (save-buffer)
        (pop-to-buffer
         (make-comint-in-buffer "Python Run" buf "bash" nil
                                "-c" (format "python3 %s"
                                             (shell-quote-argument buffer-file-name))))
        (message "[PY] Ejecutando %s..." (file-name-nondirectory buffer-file-name)))
    (user-error "[PY] Este buffer no está asociado a un archivo.")))

(defun my/python-debug ()
  "Ejecuta el script actual con el depurador pdb en un buffer interactivo."
  (interactive)
  (if buffer-file-name
      (let ((buf "*Python Debug*"))
        (save-buffer)
        (pop-to-buffer
         (make-comint-in-buffer "Python Debug" buf "bash" nil
                                "-c" (format "python3 -m pdb %s"
                                             (shell-quote-argument buffer-file-name))))
        (message "[PY] Depurando %s con pdb..." (file-name-nondirectory buffer-file-name)))
    (user-error "[PY] Este buffer no está asociado a un archivo.")))

;; ============================
;;  Asignar teclas F3, F4 y F5
;; ============================

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "<f3>") #'my/python-show-doc)
            (local-set-key (kbd "<f4>") #'my/python-debug)
            (local-set-key (kbd "<f5>") #'my/python-run)))











;======================================
; MUSICA PERRAS
;======================================

(use-package emms
  :ensure t
  :init
  (require 'emms-setup)
  (emms-all)
  (setq emms-source-file-default-directory "~/Música/")
  (setq emms-player-list '(emms-player-mpv))
  (setq emms-info-functions '(emms-info-native))
  (setq emms-player-mpv-parameters '("--no-video"))
  (message "[EMMS] Configuración cargada correctamente")

  :config
  (global-set-key (kbd "C-c e p") #'emms)
  (global-set-key (kbd "C-c e a") #'emms-add-directory)
  (global-set-key (kbd "C-c e n") #'emms-next)
  (global-set-key (kbd "C-c e b") #'emms-previous)
  (global-set-key (kbd "C-c e SPC") #'emms-pause)
  (global-set-key (kbd "C-c e s") #'emms-stop))



;==================================
;==== RADIO-MODE
;==================================
(load-file "~/.emacs.d/radio-mode/radio-mode.el")

(use-package toc-org
  :hook (org-mode . toc-org-enable))


;======================================================
;======= OCULTAR #+BEGIN_SRC
;======================================================




;======================================================
;======= Terminar de ocultar los ** pq son molestos y feos y no son aestetis y así
;======================================================

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-table nil)
  (setq org-modern-block-name '((t . ""))) ; oculta totalmente los delimitadores
  (setq org-modern-block-fringe nil))

;; =====================
;;  TABLAS BONITAS SIEMPRE (org-pretty-table sin interferencias)
;; =====================
(use-package org-pretty-table
  :load-path "~/.emacs.d/packetes_externos_descargados/org-pretty-table"
  :hook (org-mode . org-pretty-table-mode))

;; Desactiva completamente la tabla de org-modern
(setq org-modern-table nil)

;; Si tienes org-modern, asegúrate que no interfiera con las tablas
(with-eval-after-load 'org-modern
  ;; Opcional: si quieres mantener otras partes de org-modern
  (setq org-modern-block-name '((t . "")))
  (setq org-modern-block-fringe nil)

  ;; Asegúrate que org-pretty-table se reactive después de org-modern
  (add-hook 'org-mode-hook
            (lambda ()
              (org-modern-mode 1)
              (org-pretty-table-mode 1))))

;; Refresca automáticamente las tablas cuando cambias cosas en ellas
(defun my/org-pretty-table-refresh ()
  "Re-aplica org-pretty-table si estamos en una tabla."
  (when (and org-pretty-table-mode (org-at-table-p))
    (org-pretty-table--fontify)))

(add-hook 'post-command-hook #'my/org-pretty-table-refresh)



(defvar my/init-el-reloaded nil
  "Evita que init.el se recargue infinitamente.")

(unless my/init-el-reloaded
  (setq my/init-el-reloaded t)
  (run-with-timer
   0.5 nil
   (lambda ()
     (message "Recargando init.el una sola vez...")
     (load-file user-init-file))))
