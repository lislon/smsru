(require 'request)

(defconst smsru/send-sms-gateway "http://sms.ru/sms/send")

(defvar smsru/send-sms-api-id ""
  "Sms api key can be obtained from https://sms.ru/?panel=api&subpanel=method&show=sms/send page.")

(defvar smsru/send-sms-to ""
  "Phone number for sending sms. Must be same as account name on sms.ru. Eg. 79210000000")

(defvar smsru/message-prefix ""
  "Prefix prepended to each message (As signature). For example: 'Bob: '")

(defconst smsru/send-sms-return-codes '(
    ("100"	. "Сообщение принято к отправке. На следующих строчках вы найдете идентификаторы отправленных сообщений в том же порядке, в котором вы указали номера, на которых совершалась отправка.")
    ("200"	. "Неправильный api_id")
    ("201"	. "Не хватает средств на лицевом счету")
    ("202"	. "Неправильно указан получатель")
    ("203"	. "Нет текста сообщения")
    ("204"	. "Имя отправителя не согласовано с администрацией")
    ("205"	. "Сообщение слишком длинное (превышает 8 СМС)")
    ("206"	. "Будет превышен или уже превышен дневной лимит на отправку сообщений")
    ("207"	. "На этот номер (или один из номеров) нельзя отправлять сообщения, либо указано более 100 номеров в списке получателей")
    ("208"	. "Параметр time указан неправильно")
    ("209"	. "Вы добавили этот номер (или один из номеров) в стоп-лист")
    ("210"	. "Используется GET, где необходимо использовать POST")
    ("211"	. "Метод не найден")
    ("212"	. "Текст сообщения необходимо передать в кодировке UTF-8 (вы передали в другой кодировке)")
    ("220"	. "Сервис временно недоступен, попробуйте чуть позже.")
    ("230"	. "Превышен общий лимит количества сообщений на этот номер в день.")
    ("231"	. "Превышен лимит одинаковых сообщений на этот номер в минуту.")
    ("232"	. "Превышен лимит одинаковых сообщений на этот номер в день.")
    ("300"	. "Неправильный token (возможно истек срок действия, либо ваш IP изменился)")
    ("301"	. "Неправильный пароль, либо пользователь не найден")
    ("302"	. "Пользователь авторизован, но аккаунт не подтвержден (пользователь не ввел код, присланный в регистрационной смс)"))
  "Response codes")

;;;###autoload
(defun smsru/send-sms (message)
  "Send sms to fixed number `smsru/send-sms-to'"
  (interactive "sSms: ")
  (setq message (format "%s%s" smsru/message-prefix message))
  (let* ((max-lenght (if (string-match-p "^[a-zA-Z0-9~!@#$%^&*()`{};':,./<>?| ]*$" message) 160 70)))

    (when (> (length message) max-lenght)
      (error (format "Message is too long (Max is %d) Yours is %d" max-lenght (length message))))

    (message "sending...")
    (request
     smsru/send-sms-gateway
     :params `(("api_id" . ,smsru/send-sms-api-id)
               ("to" . ,smsru/send-sms-to)
               ("text" . ,message))
     :parser 'buffer-string
     :error (function* (lambda (&key error-thrown &allow-other-keys&rest _)
                  (message "Got error: %S" error-thrown)))
     :success (function*
               (lambda (&key data &allow-other-keys)
                 (let* ((return-code (car (split-string data)))
                        (result-message (assoc-default return-code smsru/send-sms-return-codes)))

                   (if (string= return-code "200")
                       (message (format "Message '%s' sent." message))
                     (if result-message
                         (message result-message)
                       (message data)))))))))

(provide 'smsru)
