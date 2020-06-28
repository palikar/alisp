AL_STRINGIFY(

             (defun server-path-not-found (serv callback)
               ([serv] server-not-found-handler
                (lambda (req res)
                  (callback req res (lambda () (request-end res))))))



             ;; Low level routing

             (defun http--route-get (serv rout callback)
               (route-handler serv rout "GET" callback))

             (defun http--route-post (serv rout callback)
               (route-handler serv rout "POST" callback))

             (defun http--route-head (serv rout callback)
               (route-handler serv rout "HEAD" callback))

             (defun http--route-delete (serv rout callback)
               (route-handler serv rout "DELETE" callback))

             (defun http--route-patch (serv rout callback)
               (route-handler serv rout "PATCH" callback))



             ;; user friendly routing 

             (defun route-post (serv index callback)
               ([serv] http--route-post index
                (lambda (req res)
                  (callback req res (lambda () (request-end res))))))


             (defun route-get (serv index callback)
               ([serv] http--route-get index
                (lambda (req res)
                  (callback req res (lambda () (request-end res))))))





             ;; Response handling

             (defun set-content (res cont)
               (push res :content)
               (push res cont))

             (defun set-status-code (res code)
               (push res :code)
               (push res code))


             (defun set-header (res name value)
               (push res :header)
               (push res `(,name ,value)))


             (defun set-cookie (res name value)
               (push res :cookie)
               (push res `(,name ,value)))




             ;; Request handling

             (defun body (req)
               ([req] nth 1))

             (defun method (req)
               ([req] nth 3))

             (defun host (req)
               ([req] nth 5))

             (defun path (req)
               ([req] nth 7))

             (defun protocol (req)
               ([req] nth 9))

             (defun version (req)
               ([req] nth 11))

             (defun headers (req)
               ([req] nth 13))

             (defun path-parameters (req)
               ([req] nth 15))

             (defun query-parameters (req)
               ([req] nth 17))

             (defun port (req)
               ([req] nth 19))


             ;; Headers

             (defun has-header (req name)
               (dolist (header (headers req))
                 (when (equal name ([header] nth 0))
                   (return t)))
               (return nil))

             (defun header (req name)
               (dolist (header (headers req))
                 (when (equal name ([header] nth 0))
                   (return ([header] nth 1))))
               (return nil))



             ;; Query parameters

             (defun has-query-parameter (req name)
               (dolist (parameter (query-parameters req))
                 (when (equal name ([parameter] nth 0))
                   (return t)))
               (return nil))

             (defun query-parameter (req name)
               (dolist (parameter (query-parameters req))
                 (when (equal name ([parameter] nth 0))
                   (return ([parameter] nth 1))))
               (return nil))




             ;; Path parameters

             (defun has-path-parameter (req name)
               (dolist (parameter (path-parameters req))
                 (when (equal name ([parameter] nth 0))
                   (return t)))
               (return nil))

             (defun path-parameter (req name)
               (dolist (parameter (path-parameters req))
                 (when (equal name ([parameter] nth 0))
                   (return ([parameter] nth 1))))
               (return nil))


             )
