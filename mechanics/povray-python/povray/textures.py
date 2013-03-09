class Texture:

    def __init__(self, text):
        self.text = text     # really want something smarter than this

    def render(self, output):
        output.write(self.text)

Wood = Texture("texture {\n  T_Wood7\n  scale 4\n}\n")
Stone = Texture("texture {\n  T_Stone25\n}\n")
