# load libaries: ----------------------------------
source("dependencies.R")

# test connection: ----------------------------------
test_connection()

pull("llama3.2")

list_models()
# try: ----------------------------------

resp <- generate("llama3.2", "tell me a 100-word story", output = "text")

resp <- generate("llama3.2", "Hi How are you ?", output = "text")

resp
# resp_process(resp, "text")
