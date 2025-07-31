;;;; dsl-threadpool.lisp
;; Простая реализация DSL на S-expr для пула потоков и глобальной очереди
;; на Common Lisp с использованием bordeaux-threads

(ql:quickload "bordeaux-threads")

(defpackage :dsl-threadpool
  (:use :cl :bordeaux-threads))

(in-package :dsl-threadpool)

;;; --- Простая очередь FIFO ---

(defclass queue ()
  ;; Использование двух списков (front и back) позволяет реализовать
  ;; очередь FIFO с амортизированным O(1) на операции enqueue и dequeue.
  ;; Элементы добавляются в back, а при извлечении — если front пуст, мы
  ;; реверсируем back в front, что обеспечивает эффективное перемещение
  ;; сразу нескольких элементов.
  ((front :initform '() :accessor queue-front)
   (back  :initform '() :accessor queue-back)))

(defun make-queue ()
  "Создает новую очередь FIFO."
  (make-instance 'queue))

(defun queue-empty-p (q)
  "Возвращает истину, если очередь пуста."
  (and (null (queue-front q))
       (null (queue-back q))))

(defun queue-enqueue (q item)
  "Добавляет ITEM в конец очереди Q."
  (push item (queue-back q)))

(defun queue-dequeue (q)
  "Извлекает следующий элемент из очереди Q. Возвращает NIL, если очередь пуста."
  (when (or (queue-front q) (queue-back q))
    (unless (queue-front q)
      (setf (queue-front q) (nreverse (queue-back q))
            (queue-back q) '()))
    (pop (queue-front q))))

;;; --- Глобальные переменные ---

(defparameter *global-queue* (make-queue)
  "Глобальная очередь задач (FIFO).")

;; Используем bordeaux-threads
(defparameter *queue-lock* (bt:make-lock)
  "Lock для защиты глобальной очереди.")

(defparameter *queue-cond* (bt:make-condition-variable)
  "Условная переменная для пробуждения воркеров при появлении задач.")

(defparameter *worker-threads* nil
  "Список потоков-воркеров.")

;;; --- DSL: структура задачи с ограниченным API ---

(defmacro make-task ((&key name) &body body)
  (let ((name (if (null name)
                  (setf name (symbol-name (gensym "T-")))
                  (format nil "~A" name))))
    `(labels ((get-task-name () ,name)
              (display (param)
                (format t "~%~A" param))
              (get-current-thread ()
                (bt:thread-name (bt:current-thread))))
       (lambda () ,@body))))


;;; --- Реализация пула воркеров ---

(defun make-worker-pool (size)
  "Создает пул из SIZE воркеров, запускает их и возвращает список потоков."
  (loop for i from 1 to size :collect (bt:make-thread
                                       (lambda () (worker-loop))
                                       :name (format nil "worker-~A" i))))

(defmacro define-worker-pool (name &key size)
  "Определяет пул воркеров с именем NAME и количеством SIZE."
  `(setf ,name (make-worker-pool ,size)))

(defun worker-loop ()
  "Цикл воркера: достать задачу и выполнить"
  (loop
    (let ((task (bt:with-lock-held (*queue-lock*)
                  ;; ждем задачу или timeout
                  (unless (queue-dequeue *global-queue*)
                    (bt:condition-wait *queue-cond* *queue-lock* :timeout 0.1))
                  ;; после пробуждения пытаемся снова
                  (queue-dequeue *global-queue*))))
      (when task
        (handler-case
            (funcall task)
          (error (e)
            (format *error-output* "Error in task: ~A~%" e)))))))

;;; --- Операции над задачами ---

(defun push-task (task)
  (bt:with-lock-held (*queue-lock*)
    (queue-enqueue *global-queue* task)
    (bt:condition-notify *queue-cond*))
  task)

;;; --- Пример использования ---

;; Определяем пул из 4 воркеров:
(define-worker-pool *pool* :size 4)

;; Отправляем задачи:

(push-task (make-task ()
             (display
              (concatenate 'string "Hello from "
                           (get-task-name) " in " (get-current-thread)))))
