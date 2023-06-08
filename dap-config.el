(dap-register-debug-template
  "Launch File"
  (list :type "go"
        :request "launch"
        :name "Launch File"
        :mode "auto"
        :program "."
        :buildFlags nil
        :args nil
        :env nil))
