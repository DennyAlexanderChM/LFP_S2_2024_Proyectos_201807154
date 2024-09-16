import tkinter as tk
from tkinter import messagebox as mbox, filedialog, scrolledtext
from PIL import Image, ImageTk # Libreria para redimensionar las imagenes (grafos y banderas)
import subprocess

class MyAplication(tk.Tk):
    def __init__(self):
        super().__init__()
        # Configurando los valores iniciales de la ventana
        self.title("Soluciones modernas S.A.")
        self.geometry("850x550") # Dimensión de la ventana
        self.config(background="#669bbc") # Color de la ventana
        self.resizable(False, False)
        self.initUI()
    
    def initUI(self): # Función para inicializar y agregar los elementos la ventana
        
        menubar = self.menubar()
        self.configure(menu=menubar)

        self.text_area = scrolledtext.ScrolledText(self, wrap="word") #Text area que mostrara el texto leído
        self.text_area.place(x=50, y=50, width=325, height=375)
       
        self.label_habitantes = tk.Label(self, text="Habitantes: ") # Label para mostrar la cantidad de habirantes
        self.label_habitantes.place(x=425, y=400)
        
        self.label_country = tk.Label(self, text="Pais seleccionado:") # Label para mostrar el país seleccionado
        self.label_country.place(x=425, y=375)
        
        self.label_image = tk.Label(self, borderwidth=2, relief="raised") # Label que contrendra la imagen  del grafo
        self.label_image.place(x=425, y=50, width=325, height=300)
        
        self.label_image_flag = tk.Label(self, borderwidth=2, relief="raised") # Label que contrendra la imagen de la bandera
        self.label_image_flag.place(x=630, y=375, width=120, height=80)

        button = tk.Button(self, text="Analizar", width=12, command=self.enviarDatos) # Botón que enviara el texto a analizar
        button.place(x=50, y=445)

    def menubar(self): # Función que crea un menu con las funciones del programa
        menu_bar = tk.Menu(self, bd=3, relief=tk.RAISED, activebackground="#80B9DC")

        archivo_menu = tk.Menu(menu_bar, tearoff=0)
        menu_bar.add_cascade(label="Archivo", menu=archivo_menu) # Menú archivo
        archivo_menu.add_command(label="Nuevo", command=self.openFile)
        archivo_menu.add_command(label="Abrir")
        archivo_menu.add_command(label="Guardar")

        ayuda_menu = tk.Menu(menu_bar, tearoff=0)
        menu_bar.add_cascade(label="Ayuda", menu=ayuda_menu) # Crear el menú Ayuda con la opción Acerca de
        ayuda_menu.add_command(label="Acerca de", command=self.aboutUs)

        menu_bar.add_command(label="Salir", command=self.quit) # Crear el botón Salir en la barra de menú

        return menu_bar # Devuelve el componente creado 

    def aboutUs(self):
        # Despliega una alerta con los datos del desarrollador
        mbox.showinfo("Acerca de", "Soluciones Modernas S.A\nVersión 1.0\nDesarrollado por Denny Chalí") 
    
    def enviarDatos(self):
        text = self.text_area.get(1.0, tk.END)
        
        if text != '':
            resultado = subprocess.run(

                ["./main.exe"],  # Ejecutable compilado
                input=text,
                stdout=subprocess.PIPE,  # Capturar la salida del programa
                text=True  # Asegurarse de que la salida se maneje como texto
                )
            
            if resultado.stdout.strip() != 'Error':
                print(resultado.stdout.strip())
                mbox.showinfo("Correcto", "!Analisis realizado correctamente!")
            else:
                # Despliega una alerta con los datos del desarrollador
                mbox.showerror("Error", "!A ocurrido un error al analizar la información!")

    def openFile(self):

        self.text_area.delete(1.0, tk.END)  # Borra el contenido del cuadro de texto

        ftypes = [('ORG Files', '*.ORG'), ('All files', '*')] # Creamos la lista de archivos admitidos
        dlg = filedialog.Open(self, filetypes = ftypes) # Creamos el elemento del archivo
        fl = dlg.show() # Mostramos la ventana

        if fl != '': # Se verifica si se selecciono un elemento

            text = self.readFile(fl)
            self.text_area.insert(tk.END, text) # Muestra el contenido del archivo

    def readFile(self, filename): # Lee el contenido del archivo

        with open(filename, "r") as f:

            text = f.read()

        return text # Retorna el contenido del archivo

# Iniciar el bucle principal de la interfaz
if __name__ == "__main__":
    app = MyAplication()
    app.mainloop()