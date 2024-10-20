import tkinter as tk
from tkinter import messagebox as mbox, filedialog, ttk
import subprocess
import os

class MyAplication(tk.Tk):
    def __init__(self):
        super().__init__()
        # Configurando los valores iniciales de la ventana
        self.title("Soluciones modernas S.A.")
        self.geometry("1000x600") # Dimensión de la ventana
        self.config(background="#219ebc") # Color de la ventana
        self.resizable(False, False) # No dimensionable
        self.filename = None
        self.text_changed = False
        self.ventana_emergente_abierta = False
        self.initUI()
    
    def initUI(self): # Función para inicializar y agregar los elementos la ventana
        # Creacion y asignación de la barra de menú en la ventana
        menubar = self.menubar()
        self.configure(menu=menubar)
        #Text area que mostrara el texto leído
        self.text_area = tk.Text(self, wrap="word", undo=True, font = ("Roboto", 11), relief="flat")
        self.text_area.bind("<<Modified>>", self.on_text_change) # Detecta si se efectuan cambios en el text_area
        self.text_area.place(x=200, y=30, width=600, height=250) # Modificamos su posición y tamaño
        
        self.frame = tk.Frame(self) # Frame que almacenará la tabla de datos 
        self.frame.place(x=40, y=300)
        
        # Crear una etiqueta para mostrar la posición
        self.position_label = tk.Label(self, text="Fila: 1, Columna: 0")
        self.position_label.place(x= 50, y= 50)
        
        # Vincular eventos de tecla y clic para actualizar la posición
        self.text_area.bind('<KeyRelease>', self.update_position)
        self.text_area.bind('<ButtonRelease>', self.update_position)
        
        # Definir las columnas de la tabla
        columns = ("tipo", "linea", "columna", "token", "descripcion")
        # Crear el widget Treeview con las columnas
        self.tree = ttk.Treeview(self.frame, columns=columns, show="headings", height=10)
        # Definir los encabezados de las columnas
        self.tree.heading("tipo", text="Tipo")
        self.tree.heading("linea", text="Línea")
        self.tree.heading("columna", text="Columna")
        self.tree.heading("token", text="Token")
        self.tree.heading("descripcion", text="Descripción")
        # Ajustar el tamaño de las columnas
        self.tree.column("tipo", width=100)
        self.tree.column("linea", width=100)
        self.tree.column("columna", width=100)
        self.tree.column("token", width=300)
        self.tree.column("descripcion", width=300)
        # Crear un scrollbar vertical y vincularlo al Treeview
        scrollbar = ttk.Scrollbar(self.frame, orient=tk.VERTICAL, command=self.tree.yview)
        self.tree.configure(yscroll=scrollbar.set)
        # Empaquetar el Treeview y el Scrollbar en el frame
        self.tree.grid(row=0, column=0)
        scrollbar.grid(row=0, column=1, sticky='ns')

    def menubar(self): # Función que crea un menu con las funciones del programa
        menu_bar = tk.Menu(self, bd=3, relief=tk.RAISED, activebackground="#80B9DC")
        archivo_menu = tk.Menu(menu_bar, tearoff=0)
        menu_bar.add_cascade(label="Archivo", menu=archivo_menu) # Menú archivo
        archivo_menu.add_command(label="Nuevo", command=self.new_file)
        archivo_menu.add_command(label="Abrir", command=self.open_file)
        archivo_menu.add_separator() 
        archivo_menu.add_command(label="Guardar", command=self.save_file)
        archivo_menu.add_command(label="Guardar como...", command=self.save_as_file)
        archivo_menu.add_separator() 
        archivo_menu.add_command(label="Salir", command=self.quit)
      
        menu_bar.add_command(label="Análizar", command=self.enviarDatos)
        menu_bar.add_command(label="Tokens", command=self.abrir_ventana_emergente)
# Verifica si la venta emergente ya a sido abierta
        return menu_bar # Devuelve el componente creado
     
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
             
            if mensaje != 'ERROR':
                
                mbox.showinfo("Correcto", "!Analisis realizado correctamente!") # Mensaje emergente (correcto)
                self.borrar_datos()
                
            else:
                
                self.borrar_datos()
                # self.reload_label()
                mbox.showerror("Error", "!Ha ocurrido un error al analizar la información!") # Mensaje emergente (error)
                # Leer el archivo línea por línea
                try:
                    with open('errores.txt', 'r', encoding='utf-8') as archivo:
                        for linea in archivo:
                            datos = linea.strip().split('&&')  # Asume que las columnas están separadas por comas
                            
                            # Insertar los datos en la tabla
                            self.tree.insert("", tk.END, values=datos)
                            # print(f"Columna 1: {datos[0]}, Columna 2: {datos[1]} Columna 3: {datos[2]},  Columna 3: {datos[3]}, ,  Columna 3: {datos[4]} ")
                
                except FileNotFoundError:
                    print("El archivo no fue encontrado.")
                except Exception as e:
                    print(f"Ocurrió un error: {e}")
                    
    # Función para borrar todos los datos de la tabla
    def borrar_datos(self):
        self.tree.delete(*self.tree.get_children())  # Elimina todas las filas de la tabla
    
    def new_file(self): # 
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
            defaultextension=".LFP",
            filetypes=[("LFP Files", "*.LFP"), ("All files", "*.*")]
        )
        if self.filename:
            with open(self.filename, "r", encoding="utf-8") as file:
                self.text_area.delete(1.0, tk.END)
                self.text_area.insert(tk.END, file.read())
            self.title(os.path.basename(self.filename) + " - Soluciones modernas S.A.")
            # self.reload_label()
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

    def save_as_file(self): # Abre una ventana emergente solicitando la ubicación del archivo a guardar
        new_file = filedialog.asksaveasfilename(
            defaultextension=".LFP",
            filetypes=[("LFP Files", "*.LFP"), ("All files", "*.*")]
        )
        if new_file:
            self.filename = new_file
            self.save_file()

    def on_text_change(self, event=None): # Verifica si el texto a sido modificado
        if self.text_area.edit_modified():
            self.text_changed = True
            self.title("*" + (os.path.basename(self.filename) if self.filename else "Nuevo archivo") + " - Soluciones modernas S.A.")
        self.text_area.edit_modified(False)

    def check_for_save(self): # Verifica si el documento a sido guardado
        if self.text_changed:
            answer = mbox.askyesnocancel("Guardar cambios", "¿Deseas guardar los cambios?")
            if answer:  # Si elige guardar
                self.save_file()
            return answer # Si cancela, devolver False para detener acciones
        
    def abrir_ventana_emergente(self):
        # Verifica si la venta emergente ya a sido abierta
        if self.ventana_emergente_abierta:
            return
        # Crear ventana emergente
        ventana_emergente = tk.Toplevel(self)
        ventana_emergente.title("Tabla de Datos")
        self.ventana_emergente_abierta = True
        # Crear el Treeview (tabla)
        tree = ttk.Treeview(ventana_emergente, columns=("#", "Lexema", "Tipo", "Fila", "Columna"), show="headings", height=17)
        # Evento para actualizar el estado cuando la ventana se cierre
        ventana_emergente.protocol("WM_DELETE_WINDOW", lambda: self.cerrar_ventana_emergente(ventana_emergente))
        # Definir encabezados de la tabla
        tree.heading("#", text="#")
        tree.heading("Lexema", text="Lexema")
        tree.heading("Tipo", text="Tipo")
        tree.heading("Fila", text="Fila")
        tree.heading("Columna", text="Columna")
        # Ajustar el tamaño de las columnas
        tree.column("#", width=100)
        tree.column("Lexema", width=200)
        tree.column("Tipo", width=200)
        tree.column("Fila", width=100)
        tree.column("Columna", width=100)

        try:
            with open('tokens.txt', 'r', encoding='utf-8') as archivo:
                for linea in archivo:
                    datos = linea.strip().split('&&')  # Asume que las columnas están separadas por comas
                    # Insertar los datos en la tabla       
                    tree.insert("", tk.END, values=datos)
                    
        except FileNotFoundError:
            print("El archivo no fue encontrado.")
        except Exception as e:
            print(f"Ocurrió un error: {e}")
            
        # Empacar la tabla
        tree.pack(fill=tk.BOTH, expand=True)
        # Añadir scrollbars en caso de que los datos excedan la ventana
        scrollbar_vertical = ttk.Scrollbar(ventana_emergente, orient=tk.VERTICAL, command=tree.yview)
        tree.configure(yscrollcommand=scrollbar_vertical.set)
        # Empaquetar el Treeview y el Scrollbar en el frame
        tree.grid(row=0, column=0)
        scrollbar_vertical.grid(row=0, column=1, sticky='ns')
    
    def cerrar_ventana_emergente(self, ventana): # Función para cerrar la ventana emergente y actualizar la variable de control
        self.ventana_emergente_abierta = False  # Marcar que la ventana está cerrada
        ventana.destroy()  # Cerrar la ventana
        
    # Función para actualizar la posición de fila y columna del cursor
    def update_position(self, event=None):
        # Obtener la posición actual del cursor en el Text (formato "linea.columna")
        cursor_position = self.text_area.index(tk.INSERT)
        # Dividir la posición en línea y columna
        line, column = cursor_position.split('.')
        # Actualizar la etiqueta con la posición actual
        self.position_label.config(text=f"Fila: {line}, Columna: {column}")
    
# Iniciar el bucle principal de la interfaz
if __name__ == "__main__":
    app = MyAplication()
    app.mainloop()