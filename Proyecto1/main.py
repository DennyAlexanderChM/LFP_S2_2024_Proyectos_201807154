import tkinter as tk
from tkinter import messagebox as mbox, filedialog, scrolledtext
from PIL import Image, ImageTk # Libreria para redimensionar las imagenes (grafos y banderas)
import subprocess
import os

class MyAplication(tk.Tk):
    def __init__(self):
        super().__init__()
        # Configurando los valores iniciales de la ventana
        self.title("Soluciones modernas S.A.")
        self.geometry("1000x550") # Dimensión de la ventana
        self.config(background="#669bbc") # Color de la ventana
        self.resizable(False, False)
        self.filename = None
        self.text_changed = False
        self.initUI()
    
    def initUI(self): # Función para inicializar y agregar los elementos la ventana
        
        menubar = self.menubar()
        self.configure(menu=menubar)
        #Text area que mostrara el texto leído
        self.text_area = tk.Text(self, wrap="word", undo=True, font = ("Roboto", 10), relief="raised")
        self.text_area.bind("<<Modified>>", self.on_text_change)
        self.text_area.place(x=50, y=50, width=430, height=375)
        self.label_habitantes = tk.Label(self, text="Habitantes: ", font = 'Roboto 11 bold') # Label para mostrar la cantidad de habirantes
        self.label_habitantes.place(x=525, y=430)
        self.label_pais = tk.Label(self, text="Pais seleccionado:", font = 'Roboto 11 bold') # Label para mostrar el país seleccionado
        self.label_pais.place(x=525, y=375)
        self.label_imagen_grafo = tk.Label(self, borderwidth=2, relief="raised") # Label que contrendra la imagen  del grafo
        self.label_imagen_grafo.place(x=525, y=50, width=425, height=300)
        self.label_imagen_bandera = tk.Label(self, borderwidth=2, relief="raised") # Label que contrendra la imagen de la bandera
        self.label_imagen_bandera.place(x=780, y=375, width=170, height=110)

        button = tk.Button(self, text="Analizar", width=12, command=self.enviarDatos) # Botón que enviara el texto a analizar
        button.place(x=50, y=445)

    def menubar(self): # Función que crea un menu con las funciones del programa
        menu_bar = tk.Menu(self, bd=3, relief=tk.RAISED, activebackground="#80B9DC")

        archivo_menu = tk.Menu(menu_bar, tearoff=0)
        menu_bar.add_cascade(label="Archivo", menu=archivo_menu) # Menú archivo
        archivo_menu.add_command(label="Abrir", command=self.open_file)
        archivo_menu.add_command(label="Guardar", command=self.save_file)
        archivo_menu.add_command(label="Guardar como...", command=self.save_as_file)

        ayuda_menu = tk.Menu(menu_bar, tearoff=0)
        menu_bar.add_cascade(label="Ayuda", menu=ayuda_menu) # Crear el menú Ayuda con la opción Acerca de
        ayuda_menu.add_command(label="Acerca de", command=self.aboutUs)

        menu_bar.add_command(label="Salir", command=self.quit) # Crear el botón Salir en la barra de menú

        return menu_bar # Devuelve el componente creado 

    def aboutUs(self):
        # Despliega una alerta con los datos del desarrollador
        mbox.showinfo("Acerca de", "Soluciones Modernas S.A\nVersión 1.0\nDesarrollador: Denny Alexander Chalí Miza\nCarne: 201807154") 
    
    def enviarDatos(self):
        success = True
        if self.text_changed:
            success = self.check_for_save()
        if success and self.filename:
            text = self.text_area.get(1.0, tk.END)
            peticion = subprocess.run(
                ["./main.exe"],
                input=text,
                stdout=subprocess.PIPE,
                text=True
                )
                
            mensaje = peticion.stdout.strip() #Obtenemos el texto y eliminamos espacios innecesarios    
            if mensaje != 'Error':
                datos = mensaje.split(',')
                self.label_pais.config(text='Pais seleccionado: \n' + datos[0])
                self.label_habitantes.config(text="Habitantes: " + datos[1])
                # Abrimos y redimencionamos la imagen con Pillow
                url = datos[3].replace('"', '')
                imagen = Image.open(url) 
                imagen = imagen.resize((170, 110)) # Redimensionar la imagen
                imagen_tk = ImageTk.PhotoImage(imagen)
                self.label_imagen_bandera.config(image=imagen_tk)
                self.label_imagen_bandera.image = imagen_tk

                imagen_grafica = Image.open('GRAFICA.png') # Path por defecto de la grafica
                imagen_grafica = imagen_grafica.resize((425,300)) # Redimensionamos con Pillow
                imagen_grafica = ImageTk.PhotoImage(imagen_grafica)
                self.label_imagen_grafo.config(image=imagen_grafica)
                self.label_imagen_grafo.image = imagen_grafica
                
                mbox.showinfo("Correcto", "!Analisis realizado correctamente!") # Mensaje emergente (correcto)
            else:
                self.reload_label()
                mbox.showerror("Error", "!Ha ocurrido un error al analizar la información!") # Mensaje emergente (error)

    def new_file(self):
        if self.text_changed:
            self.check_for_save()
        self.text_area.delete(1.0, tk.END)
        self.filename = None
        self.title("Nuevo archivo - Soluciones modernas S.A.")
        self.text_changed = False

    def open_file(self):
        if self.text_changed:
            self.check_for_save()
        self.filename = filedialog.askopenfilename(
            defaultextension=".ORG",
            filetypes=[("ORG Files", "*.ORG"), ("All files", "*.*")]
        )
        if self.filename:
            with open(self.filename, "r", encoding="utf-8") as file:
                self.text_area.delete(1.0, tk.END)
                self.text_area.insert(tk.END, file.read())
            self.title(os.path.basename(self.filename) + " - Soluciones modernas S.A.")
            self.reload_label()
            self.text_changed = False
            self.text_area.edit_modified(False)

    def save_file(self): # Guardar archivo
        if not self.filename: # Verifica si el archivo no ha sido guardado
            self.save_as_file()
        else:
            with open(self.filename, "w", encoding="utf-8") as file:
                file.write(self.text_area.get(1.0, tk.END))
            self.text_changed = False
            self.title(os.path.basename(self.filename) + " - Soluciones modernas S.A.")

    def save_as_file(self):
        new_file = filedialog.asksaveasfilename(
            defaultextension=".ORG",
            filetypes=[("ORG Files", "*.ORG"), ("All files", "*.*")]
        )
        if new_file:
            self.filename = new_file
            print(self.filename)
            self.save_file()

    def on_text_change(self, event=None):
        if self.text_area.edit_modified():
            self.text_changed = True
            self.title("*" + (os.path.basename(self.filename) if self.filename else "Nuevo archivo") + " - Soluciones modernas S.A.")
        self.text_area.edit_modified(False)

    def check_for_save(self):
        if self.text_changed:
            answer = mbox.askyesnocancel("Guardar cambios", "¿Deseas guardar los cambios?")
            if answer:  # Si elige guardar
                self.save_file()
            return answer # Si cancela, devolver False para detener acciones
    
    def reload_label(self):
        self.label_imagen_bandera.config(image=None) # Eliminamos la imagen del Label (Bandera)
        self.label_imagen_bandera.image = None
        self.label_imagen_grafo.config(image=None) # Eliminamos la imagen del Label (Grafica)
        self.label_imagen_grafo.image = None
        self.label_pais.config(text="Pais seleccionado: ") # Restablecemos el texto del Label (Pais)
        self.label_habitantes.config(text="Habitantes: ") # Restablecemos el texto del Label (Habitantes)

# Iniciar el bucle principal de la interfaz
if __name__ == "__main__":
    app = MyAplication()
    app.mainloop()