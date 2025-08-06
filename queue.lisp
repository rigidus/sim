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
