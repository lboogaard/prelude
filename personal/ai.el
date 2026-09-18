(setq gptel-backend (gptel-make-ollama "Ollama"
                      :host "localhost:11434"
                      :stream t
                      :models '(gemma4))
      gptel-model 'gemma4)
