;;;; dsl-threadpool.lisp
;; Простая реализация DSL на S-expr для пула потоков и глобальной очереди
;; на Common Lisp с использованием bordeaux-threads

(ql:quickload "bordeaux-threads")
(ql:quickload "cl-cpus")

(load "queue.lisp")

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

;;; --- make-task macro API ---

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

(defun worker-loop ()
  "Цикл воркера: достать задачу и выполнить"
  (loop
    (let ((task (bt:with-lock-held (*queue-lock*)
                  ;; ждем задачу или timeout
                  (unless (queue-dequeue *global-queue*)
                    (bt:condition-wait *queue-cond* *queue-lock*
                                       :timeout 0.1))
                  ;; после пробуждения пытаемся снова
                  (queue-dequeue *global-queue*))))
      (when task
        (handler-case
            (funcall task)
          (error (e)
            (format *error-output* "Error in task: ~A~%" e)))))))

(defun make-worker-pool (size)
  "Создает пул из SIZE воркеров, запускает их и возвращает список потоков."
  (loop for i from 1 to size
        collect (bt:make-thread
                 (lambda () (worker-loop))
                 :name (format nil "worker-~A" i))))

;;; --- Операции над задачами ---

(defun push-task (task)
  (bt:with-lock-held (*queue-lock*)
    (queue-enqueue *global-queue* task)
    (bt:condition-notify *queue-cond*))
  task)

;;; --- Пример ---

;; Определяем пул из воркеров:
(defparameter *pool* (make-worker-pool (cpus:get-number-of-processors)))

;; Отправляем задачи:
(push-task (make-task ()
             (display
              (concatenate 'string "Hello from "
                           (get-task-name) " in " (get-current-thread)))))
