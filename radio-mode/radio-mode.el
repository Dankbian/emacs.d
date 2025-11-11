;;; radio-mode.el --- Radio para Emacs -*- lexical-binding: t; -*-

;; Author: Dankbian 
;; Version: 1.0
;; Description: Reproductor de radio en emacs con asciis bonitos :3
;; Dependencies: mpv installed in system path

;;; Code:

(defvar radio/ascii-dir "~/.emacs.d/radio-mode/ascii/"
  "Directorio que contiene los archivos ASCII del modo radio.")

(defvar radio/ascii-header (concat radio/ascii-dir "header.txt"))
(defvar radio/ascii-menu (concat radio/ascii-dir "menu.txt"))
(defvar radio/ascii-playing (concat radio/ascii-dir "playing.txt"))
(defvar radio/ascii-stopped (concat radio/ascii-dir "stopped.txt"))
(defvar radio/ascii-bye (concat radio/ascii-dir "bye.txt"))

(defvar radio/stations
  '(("1.FM 80s & 90s Hits" . "https://strm112.1.fm/80s_90s_mobile_mp3")
    ("Radio Caprice 80s Pop" . "http://79.111.119.111:8000/pop80s")
    ("1.FM Classic Rock Replay" . "https://strm112.1.fm/classicrock_mobile_mp3")
    ("Radio Swiss Classic Rock" . "https://stream.srg-ssr.ch/m/rsrock/mp3_128")
    ("Radio Paradise Mix" . "https://stream.radioparadise.com/mp3-192"))
  "Lista de estaciones disponibles para reproducir en radio-mode.")

(defvar radio/current-process nil
  "Proceso actual de mpv para la radio.")

(defvar radio/current-station nil
  "Estación actualmente en reproducción.")

;;;###autoload
(defun radio-mode ()
  "Abre la interfaz de Retro Radio Mode."
  (interactive)
  (let ((buf (get-buffer-create "*Retro Radio*")))
    (with-current-buffer buf
      (radio-mode-setup))
    (switch-to-buffer buf)))

(defun radio-mode-setup ()
  "Inicializa el buffer principal del modo radio."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (radio--read-ascii radio/ascii-header))
    (insert "\n\n")
    (insert (radio--read-ascii radio/ascii-menu))
    (goto-char (point-min))
    (radio-mode-map-setup)
    (read-only-mode 1)
    (setq major-mode 'radio-mode
          mode-name "Radio")))

(defun radio--read-ascii (file)
  "Lee contenido ASCII desde FILE si existe."
  (if (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string))
    (format "[Archivo ASCII no encontrado: %s]" file)))

(defun radio-mode-map-setup ()
  "Asigna las teclas del modo radio."
  (use-local-map (make-sparse-keymap))
  (local-set-key (kbd "r") #'radio/play)
  (local-set-key (kbd "s") #'radio/stop)
  (local-set-key (kbd "q") #'radio/quit)
  (local-set-key (kbd "1") (lambda () (interactive) (radio/play-by-index 1)))
  (local-set-key (kbd "2") (lambda () (interactive) (radio/play-by-index 2)))
  (local-set-key (kbd "3") (lambda () (interactive) (radio/play-by-index 3)))
  (local-set-key (kbd "4") (lambda () (interactive) (radio/play-by-index 4)))
  (local-set-key (kbd "5") (lambda () (interactive) (radio/play-by-index 5))))

(defun radio/play-by-index (index)
  "Reproduce la estación según su número INDEX."
  (let ((station (nth (1- index) radio/stations)))
    (when station
      (radio/play (car station)))))

(defun radio/play (&optional name)
  "Reproduce la estación NAME seleccionada, o pregunta si es nil."
  (interactive)
  (when radio/current-process
    (radio/stop))
  (let* ((station-name (or name (completing-read "Elige estación: "
                                                 (mapcar #'car radio/stations))))
         (url (cdr (assoc station-name radio/stations))))
    (setq radio/current-station station-name
          radio/current-process
          (start-process "radio" "*Radio Player*" "mpv" "--no-video" url))
    (radio--update-screen-playing station-name)))

(defun radio--update-screen-playing (station)
  "Actualiza el buffer principal mostrando el estado de reproducción."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (radio--read-ascii radio/ascii-playing))
    (insert (format "\nReproduciendo: %s\n" station))
    (insert "\n[s] detener   [q] salir\n")
    (read-only-mode 1)))

(defun radio/stop ()
  "Detiene la reproducción actual."
  (interactive)
  (when (process-live-p radio/current-process)
    (delete-process radio/current-process))
  (setq radio/current-process nil
        radio/current-station nil)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (radio--read-ascii radio/ascii-stopped))
    (insert "\n[r] reproducir otra vez   [q] salir\n")
    (read-only-mode 1))
  (message "Radio detenida."))

(defun radio/quit ()
  "Cierra el modo radio y detiene cualquier proceso."
  (interactive)
  (radio/stop)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (radio--read-ascii radio/ascii-bye)))
  (run-at-time "1.5 sec" nil (lambda () (kill-buffer "*Retro Radio*"))))

(provide 'radio-mode)

