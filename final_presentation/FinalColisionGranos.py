from manim import *
from manim_slides import Slide
import numpy as np
import pandas as pd
from tensorflow import keras
#from manim.mobject.graphical_unit.video_mobject import VideoMobject
#from manim import VideoMobject
from PIL import Image  # <--- nuevo

class GifMobject(Group):
    """
    Muestra un GIF animado usando ImageMobject + updater.
    Funciona en Manim Community 0.19.x.
    """
    def __init__(self, gif_path, scale=1.0, **kwargs):
        super().__init__(**kwargs)
        self.frames, self.durations = self._load_gif(gif_path)
        self.n_frames = len(self.frames)
        self.total_duration = sum(self.durations)
        self.time = 0.0

        # Primer frame como imagen inicial
        first_img = ImageMobject(self.frames[0])
        if scale != 1.0:
            first_img.scale(scale)
        self.add(first_img)

        # Registrar updater en el propio Group
        self.add_updater(self._update_frame)

    def _load_gif(self, path):
        im = Image.open(path)
        frames = []
        durations = []
        try:
            while True:
                frame = im.convert("RGBA")
                frames.append(np.array(frame))
                dur_ms = im.info.get("duration", 100)
                durations.append(dur_ms / 1000.0)  # a segundos
                im.seek(im.tell() + 1)
        except EOFError:
            pass
        return frames, durations

    def _update_frame(self, mob, dt):
        # Avanzar tiempo total (cíclico)
        self.time = (self.time + dt) % self.total_duration

        # Elegir índice de frame según self.time y las duraciones
        t = self.time
        idx = 0
        for d in self.durations:
            if t < d:
                break
            t -= d
            idx += 1

        idx = min(idx, self.n_frames - 1)

        # Mobject actual (único submobject: la imagen)
        current_img = mob.submobjects[0]

        # Crear nueva imagen para el frame actual
        new_img = ImageMobject(self.frames[idx])

        # Mantener tamaño y posición del frame anterior
        new_img.match_width(current_img)
        new_img.move_to(current_img)

        # Reemplazar submobject
        mob.remove(current_img)
        mob.add(new_img)


# Activation functions
def relu(X):
    return np.maximum(0,X)

# def softmax(X):
#     return np.exp(X)/sum(np.exp(X))

# stable version of the softmax
def softmax(X):
    # print(X)
    Z = X - np.max(X)
#max(X)
    numerator = np.exp(Z)
    denominator = np.sum(numerator)
    return numerator/denominator

# Calculates the output of a given layer
def calculate_layer_output(w, prev_layer_output, b, activation_type="relu"):
    # Steps 1 & 2
    g = w @ prev_layer_output + b

    # Step 3
    if activation_type == "relu":
        return relu(g)
    if activation_type == "softmax":
        return softmax(g)

# Initialize weights & biases
def init_layer_params(row, col):
    w = np.random.randn(row, col)
    b = np.random.randn(row, 1)
    return w, b

# Calculate ReLU derivative
def relu_derivative(g):
    derivative = g.copy()
    derivative[derivative <= 0] = 0
    derivative[derivative > 0] = 1
    return np.diag(derivative.T[0])

def layer_backprop(previous_derivative, layer_output, previous_layer_output
                   , w, activation_type="relu"):
    # 1. Calculate the derivative of the activation func
    dh_dg = None
    if activation_type == "relu":
        dh_dg = relu_derivative(layer_output)
    elif activation_type == "softmax":
        dh_dg = softmax_derivative(layer_output)

    # 2. Apply chain rule to get derivative of Loss function with respect to:
    dL_dg = dh_dg @ previous_derivative # activation function

    # 3. Calculate the derivative of the linear function with respect to:
    dg_dw = previous_layer_output.T     # a) weight matrix
    dg_dh = w.T                         # b) previous layer output
    dg_db = 1.0                         # c) bias vector

    # 4. Apply chain rule to get derivative of Loss function with respect to:
    dL_dw = dL_dg @ dg_dw               # a) weight matrix
    dL_dh = dg_dh @ dL_dg               # b) previous layer output
    dL_db = dL_dg * dg_db               # c) bias vector

    return dL_dw, dL_dh, dL_db

def gradient_descent(w, b, dL_dw, dL_db, learning_rate):
    w -= learning_rate * dL_dw
    b -= learning_rate * dL_db
    return w, b

def get_prediction(o):
    return np.argmax(o)

# Compute Accuracy (%) across all training data
def compute_accuracy(train, label, w1, b1, w2, b2, w3, b3):
    # Set params
    correct = 0
    total = train.shape[0]

    # Iterate through training data
    for index in range(0, total):
        # Select a single data point (image)
        X = train[index: index+1,:].T

        # Forward pass: compute Output/Prediction (o)
        h1 = calculate_layer_output(w1, X, b1, activation_type="relu")
        h2 = calculate_layer_output(w2, h1, b2, activation_type="relu")
        o = calculate_layer_output(w3, h2, b3, activation_type="softmax")

        # If prediction matches label Increment correct count
        if label[index] == get_prediction(o):
            correct+=1

    # Return Accuracy (%)
    return (correct / total) * 100


# Calculate Softmax derivative
def softmax_derivative(o):
    derivative = np.diag(o.T[0])

    for i in range(len(derivative)):
        for j in range(len(derivative)):
            if i == j:
                derivative[i][j] = o[i] * (1 - o[i])
            else:
                derivative[i][j] = -o[i] * o[j]
    return derivative



class FinalColisionGranos(Slide):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.HEADER_FONT_SIZE = 24  # Definir dentro de la instancia
        self.HEADER_HEIGHT = -3.5  # Definir dentro de la instancia
        self.TRAINING_DATA_POINT = 10  # Definir dentro de la instancia
        self.ANIMATION_RUN_TIME = 0.1  # Definir dentro de la instancia
            
    def create_input_image(self, training_image, left_shift):
        flat_image = training_image.flatten()  # Asegurar un vector de 784 elementos

        # Crear lista de cuadrados para representar píxeles
        squares = [
            Square(fill_color=BLACK, fill_opacity=flat_image[i], stroke_width=0.5, stroke_color=DARK_GRAY).scale(0.03)
            for i in range(784)
        ]

        # Agrupar en una grilla 28x28
        group = VGroup(*squares).arrange_in_grid(rows=28, cols=28, buff=0)

        # Posicionar en la escena
        group.shift(left_shift * LEFT)
        return group

    
    def create_nodes(self, left_shift, down_shift, num_nodes, layer_output=None):
      # Create VGroup & list to hold created nodes
      node_group = VGroup()
      nodes = []
    
      # Create list of circles to represent nodes
      for i in range(num_nodes):
          # Set fill opacity to 0
          opacity = 0.0
          text = "0.0"
          # If a layer output has been passed and the max value is not 0
          if layer_output is not None and np.max(layer_output) != 0.0:
              # Set opacity as normalised layer output value
              opacity = (layer_output[i] / np.max(np.absolute(layer_output)))[0]
              # Set text as layer output
              text = f'{layer_output[i][0]:.1f}'
    
          # Create node
          node = Circle(radius=0.23
              , stroke_color=BLACK
              , stroke_width=0.7
              , fill_color=GRAY
              , fill_opacity=opacity
              )
    
          # Add to nodes list
          nodes += [node]
    
          fill_text = Text(text, font_size=12, color=BLACK)
          # Position fill text in circle
          fill_text.move_to(node)
    
          # Group fill text and node and add to node_group
          group = VGroup(node, fill_text)
          node_group.add(group)
    
    
      # Arrange & position node_group
      node_group.arrange(DOWN, buff=0.2)
      node_group.shift(left_shift * LEFT).shift(down_shift * DOWN)
    
      return node_group, nodes
    
    def create_connections(self, left_layer_nodes, right_layer_nodes, w):
      # Create VGroup to hold created connections
      connection_group = VGroup()
    
      # Iterate through right layer nodes
      for l in range(len(right_layer_nodes)):
          # Iterate through left layer nodes
          for r in range(len(left_layer_nodes)):
              # Calculate opacity from normalised weight matrix values
              opacity = 0.0 if np.max(np.absolute(w[l, :])) == 0.0 \
                  else w[l, r] / np.max(np.absolute(w[l, :]))
              # Set colour
              colour = GREEN if opacity >= 0 else RED
    
              # Create connection line
              line = Line(start=right_layer_nodes[l].get_edge_center(LEFT)
                          , end=left_layer_nodes[r].get_edge_center(RIGHT)
                          , color=colour
                          , stroke_opacity=abs(opacity)
                          )
    
              # Add to connection group
              connection_group.add(line)
      return connection_group
    
    def create_text(self, text, font_size, left_shift, down_shift):
        # Create text
        text = Text(text, font_size=font_size, color=BLACK)
    
        # Position text
        text.shift(left_shift * LEFT)
        text.shift(down_shift * DOWN)
    
        return text
    
    def create_prediction_text(self, prediction, left_shift):
        # Create group
        prediction_text_group = VGroup()
    
        # Create & position text
        prediction_text = Text(f'{prediction}', font_size=40, color = BLACK)
        prediction_text.shift(left_shift * LEFT)
    
        # Create text box (helps with positioning Prediction Header)
        prediction_text_box = Square(fill_opacity=0
                                     , stroke_opacity=0
                                     , side_length=0.75)
        prediction_text_box.move_to(prediction_text)
    
        # Create Header Text
        prediction_header = Text("Prediction"
                                 , font_size=self.HEADER_FONT_SIZE, color = BLACK)
        prediction_header.next_to(prediction_text_box, UP)
    
        # Group items
        prediction_text_group.add(prediction_header)
        prediction_text_group.add(prediction_text)
        prediction_text_group.add(prediction_text_box)
    
        return prediction_text_group
    
    # Animate Methods
    def animate_input_image(self, input_image, X, left_shift):
        # 1. Create input image with new parameters
        new_input_image = self.create_input_image(X, left_shift)
        # 2. Transform old input image to new image
        self.play(Transform(input_image, new_input_image)
                  , run_time=1)
    
    def animate_nodes(self, layer_group, layer_output
                      , left_shift, down_shift, num_neurons):
        # 1. Create nodes with new parameters
        new_layer_group, _ = self.create_nodes(left_shift
                                               , down_shift
                                               , num_neurons
                                               , layer_output)
        # 2. Transform old nodes to new nodes
        self.play(Transform(layer_group, new_layer_group)
                  , run_time=self.ANIMATION_RUN_TIME)
    
    def animate_connections(self, left_layer_centers, right_layer_centers
                            , line_group, w):
        # 1. Create connections with new parameters
        new_line_group = self.create_connections(left_layer_centers
                                                 , right_layer_centers
                                                 , w)
        # 2. Transform old connections to new connections
        self.play(Transform(line_group, new_line_group)
                  , run_time=self.ANIMATION_RUN_TIME)
    
    def animate_text(self, text, new_string, font_size, left_shift, down_shift):
        # 1. Create text with new parameters
        new_text = self.create_text(new_string
                                    , font_size
                                    , left_shift
                                    , down_shift)
        # 2. Transform old text to new text
        self.play(Transform(text, new_text)
                  , run_time=self.ANIMATION_RUN_TIME)
    
    def animate_prediction_text(self, prediction_text_group, prediction, left_shift):
        # 1. Create prediction text with new parameters
        new_prediction_text_group = self.create_prediction_text(prediction
                                                                , left_shift)
        # 2. Transform old prediction text to new prediction text
        self.play(Transform(prediction_text_group, new_prediction_text_group)
                  , run_time=self.ANIMATION_RUN_TIME)

    def construct(self):


        self.camera.background_color = LIGHTER_GRAY 
        self.wait_time_between_slides = 0.1

        title_talk = Text("Colision de granos", line_spacing=2, t2c={"granos": BLUE_E}, font_size=40,color=BLACK)
        author = Text("Anibal Varela\nInstituto de Ciencias Fisicas (ICIFI) - Universidad de San Martin", line_spacing=1, t2w={"ICIFI": BOLD}, t2c={"Anibal Varela": PURPLE_E},font_size=30,color=BLACK)
        # test = Text("test", line_spacing=1, t2w={"test": BOLD}, t2c={"t": YELLOW},font_size=25) 
        title = Group(title_talk,author).arrange(DOWN, buff=3)
        self.play(FadeIn(title))
        self.next_slide()
        self.clear()
        # =======================
        # Slide: Colisiones de polvo: teoría clásica y modelos de contacto
        # =======================
                
        slide_title = Text(
            "Colisiones de polvo: teoría clásica y modelos de contacto",
            font_size=35,
            color=BLACK
        ).to_edge(UP)
        
        # Aumentamos font_size de 25 → 32
        bullet1 = Text(
            "• Las colisiones de granos controlan el crecimiento en discos protoplanetarios.",
            font_size=20, color=BLACK
        )
        bullet2 = Text(
            "• Hertz describe el contacto elástico sin adhesión entre esferas de vidrio (siglo XVIII).",
            font_size=20, color=BLACK
        )
        bullet3 = Text(
            "• En los años 70, Johnson-Kendall-Roberts (JKR) incorporan la adhesión superficial y la",
            font_size=20, color=BLACK
        )
        bullet31 = Text(
                    "descripción física de la dinámica del área de contacto bajo carga y deformación.",
                    font_size=20, color=BLACK
                )
        bullet4 = Text(
            "• La teoría predice umbrales de sticking o rebote según propiedades del material.",
            font_size=20, color=BLACK
        )
        bullet5 = Text(
            "• JKR permite calcular la velocidad crítica de rebote y el coeficiente de restitución.",
            font_size=20, color=BLACK
        )
        
        bullets = VGroup(
            bullet1, bullet2, bullet3, bullet31, bullet4, bullet5
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.55)
        # bullets.scale(0.9)
        # =============== Auto-scaling suave ===============
        # max_width = config.frame_width * 0.85  # un poco más de ancho permitido
        # if bullets.width > max_width:
        #     bullets.scale(max_width / bullets.width)
        
        # Colocar debajo del título
        bullets.next_to(slide_title, DOWN, buff=0.8)
        
        # =============== Animación ===============
        self.add(slide_title)
        self.wait(0.5)
        
        for b in bullets:
            self.play(FadeIn(b, shift=RIGHT))
            self.wait(0.25)
        
        self.wait(2)
        self.next_slide()
        self.clear()


        
        # =======================
        # Slide: Hertz y JKR (con imagen)
        # =======================
        slide_title = Text(
            "Hertz y JKR",
            font_size=35,
            color=BLACK
        ).to_edge(UP)
        
        image = ImageMobject(
            "/home/anibal/sim_com_sabato/final_presentation/figures_lecture/teorias_viejas.png"
        )
        image.scale(1.5)
        
        caption = Text(
            "La figura c) describe una simulación de dinámica molecular",
            font_size=20,
            color=BLACK
        )
        
        # 1) Colocar el caption debajo de la imagen
        caption.next_to(image, DOWN, buff=0.5)
        
        # 2) Agrupar (sin usar arrange)
        figure = Group(image, caption)
        
        # 3) Colocar el conjunto debajo del título
        figure.next_to(slide_title, DOWN, buff=1.2)
        
        # 4) Animación
        self.add(slide_title)
        self.wait(0.3)
        self.play(FadeIn(figure, shift=DOWN))
        self.wait(1)
        self.next_slide()
        self.clear()


        # =======================
        # Slide: ¿Por qué usar Dinámica Molecular en colisiones de polvo astrofísico?
        # =======================
        
        slide_title = Text(
            "¿Por qué usar Dinámica Molecular en colisiones de polvo?",
            font_size=35,
            color=BLACK
        ).to_edge(UP)
        
        bullet1 = Text(
            "• Permite probar la validez de modelos macroscópicos como Hertz y JKR a escala nanométrica.",
            font_size=25, color=BLACK
        )
        
        bullet2 = Text(
            "• Accede a mecanismos que los modelos continuos no capturan: adhesión, deformación inelástica y ruptura.",
            font_size=25, color=BLACK
        )
        
        bullet3 = Text(
            "• Describe de forma explícita interacciones atómicas que gobiernan sticking, rebote y fragmentación.",
            font_size=25, color=BLACK
        )
        
        bullet4 = Text(
            "• Permite variar composición y estructura del grano (silica, hielo, porosidad) y estudiar su impacto en el choque.",
            font_size=25, color=BLACK
        )
        
        bullet5 = Text(
            "• Genera trayectorias completas de los átomos, actuando como 'datos observacionales' para ajustar teoría.",
            font_size=25, color=BLACK
        )
        
        bullet6 = Text(
            "• Revela fenómenos ausentes en JKR: eyección de clusters, fusión local del hielo, adhesión inducida por agua.",
            font_size=25, color=BLACK
        )
        
        bullet7 = Text(
            "• Identifica claramente el umbral de coagulación-bouncing y cómo se modifica por la microestructura.",
            font_size=25, color=BLACK
        )
        
        bullets = VGroup(
            bullet1, bullet2, bullet3, bullet4, bullet5, bullet6, bullet7
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.5)
        
        # max_width = config.frame_width * 0.8
        # if bullets.width > max_width:
        bullets.scale(1)
        
        bullets.next_to(slide_title, DOWN, buff=1)
        
        self.add(slide_title)
        self.wait(0.5)
        
        for b in bullets:
            self.play(FadeIn(b, shift=RIGHT))
            self.wait(0.25)
        
        self.wait(2)
        self.next_slide()
        self.clear()


        
        # Slide X: Sistema estudiado en la simulación
        slide_title = Text("Sistema estudiado", font_size=35, color=BLACK).to_edge(UP)
        
        # Bullet 1: descripción general del grano
        bullet1 = Text(
            "• El grano esta compuesto por N partículas idénticas.", 
            font_size=25, color=BLACK
        )

        # Bullet 2: potencial de Lennard-Jones
        bullet2_text = Text(
            "• Interactúan mediante el potencial de Lennard-Jones:", 
            font_size=25, color=BLACK
        )
        bullet2_eq = MathTex(
            r"v(r) = 4\varepsilon \left[ \left(\frac{\sigma}{r}\right)^{12} - \left(\frac{\sigma}{r}\right)^6 \right]",
            font_size=28, color=BLACK
        )
        bullet2 = VGroup(bullet2_text, bullet2_eq).arrange(DOWN, aligned_edge=LEFT, buff=0.1)

        # Bullet 3: control de temperatura
        bullet3 = Text(
            "• La temperatura se controla con un termostato de Langevin.", 
            font_size=25, color=BLACK
        )

        # Bullet 4: preparación del grano (quench from the melt)
        bullet4 = Text(
            "• El grano se genera mediante 'quench from the melt':", 
            font_size=25, color=BLACK
        )
        bullet4b = Text(
            "   se funde el sólido y luego se enfría rápidamente hasta la T objetivo.", 
            font_size=25, color=BLACK
        )
        bullet4_group = VGroup(bullet4, bullet4b).arrange(DOWN, aligned_edge=LEFT, buff=0.1)

        # Agrupar todos los bullets
        bullets = VGroup(
            bullet1, bullet2, bullet3, bullet4_group
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.5)
        bullets.next_to(slide_title, DOWN, buff=1)

        # Add title first
        self.add(slide_title)
        self.wait(0.5)
        
        # Animate each bullet separately
        self.play(FadeIn(bullet1, shift=RIGHT))
        self.wait(0.5)
        self.play(FadeIn(bullet2, shift=RIGHT))
        self.wait(0.5)
        self.play(FadeIn(bullet3, shift=RIGHT))
        self.wait(0.5)
        self.play(FadeIn(bullet4_group, shift=RIGHT))

        self.wait(2)
        self.next_slide()
        self.clear()


   # Slide with an image
        slide_title = Text(
            "Creacion del grano",
            font_size=35,
            color=BLACK,
        ).to_edge(UP)

        # Usamos GifMobject en lugar de ImageMobject
        gif = GifMobject("figures_lecture/sphere_sss_no_fix_center.gif", scale=1)
        gif.next_to(slide_title, DOWN, buff=1)
        caption = Tex(
            "Se simula un fluido a una temperatura $T=1$ y $rh0=0.7$",
            font_size=15,
            color=BLACK,
        )

        figure = Group(gif, caption).arrange(DOWN, buff=0.5)
        
        self.add(slide_title)
        self.wait(0.1)
        self.play(FadeIn(figure))
        self.wait(30)  # aquí el GIF se va a ver animado
        self.next_slide()
        self.clear()
        
   # Slide with an image
        slide_title = Text("g(r) de la gota", font_size=35, color=BLACK).to_edge(UP)
        image = ImageMobject("/home/anibal/sim_com_sabato/final_presentation/figures_lecture/gr_T1_gota.png")  # Replace with your image path
        image.scale(1.5)  # Resize if needed
        image.next_to(slide_title, DOWN, buff=1)
        caption = Text("Una g(r) de un fluido.", font_size=15,color=BLACK)
        # caption.next_to(image, DOWN, buff=1)
        figure = Group(image,caption).arrange(DOWN, buff=0.5)
        # self.play(Write(slide_title))
        self.add(slide_title)
        self.wait(0.1)
        self.play(FadeIn(figure))
        self.wait(0.1)
        # self.add(caption)
        # self.wait(0.1)
        self.next_slide()
        self.clear()



        slide_title = Text(
            "Generacion de granos de diferentes materiales",
            font_size=35,
            color=BLACK,
        ).to_edge(UP)

        # Usamos GifMobject en lugar de ImageMobject
        # gif = GifMobject("figures_lecture/collision.gif", scale=0.7)
        # gif.next_to(slide_title, DOWN, buff=1)
        caption = Text(
            "Tocamos los coeficientes del potencial de Lennard Jones ",
            font_size=15,
            color=BLACK,
        )

        figure = Group(gif, caption).arrange(DOWN, buff=0.5)
        
        self.add(slide_title)
        self.wait(0.1)
        self.play(FadeIn(figure))
        self.wait(30)  # aquí el GIF se va a ver animado
        self.next_slide()
        self.clear()

        slide_title = Text(
            "Collision de granos de diferentes materiales",
            font_size=35,
            color=BLACK,
        ).to_edge(UP)

        # Usamos GifMobject en lugar de ImageMobject
        gif = GifMobject("figures_lecture/collision.gif", scale=0.7)
        gif.next_to(slide_title, DOWN, buff=1)
        caption = Text(
            "Simulación de colisión (dinámica molecular).",
            font_size=15,
            color=BLACK,
        )

        figure = Group(gif, caption).arrange(DOWN, buff=0.5)
        
        self.add(slide_title)
        self.wait(0.1)
        self.play(FadeIn(figure))
        self.wait(30)  # aquí el GIF se va a ver animado
        self.next_slide()
        self.clear()



        # 



        # slide_title = Text("Varias teorias", font_size=35,color=BLACK).to_edge(UP)
        # self.add(slide_title)
        # self.wait(0.5)

        # bias = Circle(radius=0.4, color=RED).shift(LEFT * 3 + 3.5*DOWN)        
        # input1 = Circle(radius=0.4, color=BLUE).shift(LEFT * 3 + 2*UP)
        # input2 = Circle(radius=0.4, color=BLUE).shift(LEFT * 3)
        # input3 = Circle(radius=0.4, color=BLUE).shift(LEFT * 3 + 2*DOWN)

        # # Define the larger output circle
        # output = Circle(radius=1, color=BLACK).shift(RIGHT * 2)

        # # Divide output circle into two halves
        # divider = Line(output.get_top(), output.get_bottom(), color=BLACK)

        # w1 = MathTex(r"w_1",color=BLACK,font_size=35).next_to(input1,2.4*RIGHT)
        # w2 = MathTex(r"w_2",color=BLACK,font_size=35).next_to(input2,RIGHT+UP)
        # w3 = MathTex(r"w_3",color=BLACK,font_size=35).next_to(input3,RIGHT+UP)
        
        # bias_label = MathTex(r"b",color=BLACK,font_size=35).move_to(bias.get_center())
        
        # # Labels for summation and activation function
        # sigma_label = MathTex(r"\Sigma",color=BLACK).move_to(output.get_center() + LEFT * 0.5)
        # f_label = MathTex(r"Step",color=BLACK,font_size=30).move_to(output.get_center() + RIGHT * 0.5)

        # # Define different target points on the left edge of the output circle
        # output_left_top = output.get_left() + UP * 0.5
        # output_left_middle = output.get_left()
        # output_left_bottom = output.get_left() + DOWN * 0.5
        # output_left_bottom2 = output.get_left() + DOWN * 0.7
        # # Define arrows connecting inputs to different parts of the output circle
        # arrow1 = Arrow(input1.get_right(), output_left_top, buff=0.2, color=BLACK)
        # arrow2 = Arrow(input2.get_right(), output_left_middle, buff=0.2,color=BLACK)
        # arrow3 = Arrow(input3.get_right(), output_left_bottom, buff=0.2,color=BLACK)
        # arrow_bias = Arrow(bias.get_right(), output_left_bottom2, buff=0.2,color=BLACK)
        
        # arrow4 = Arrow(output.get_right(), 1.5*output.get_right(), buff=0.2,color=BLACK)
        # # Labels for inputs (correcting the Text constructor)
        # input1_label = MathTex("X_1", font_size=40, color=BLACK).next_to(input1, LEFT*5)
        # input2_label = MathTex("X_2", font_size=40, color=BLACK).next_to(input2, LEFT*5)
        # input3_label = MathTex("X_3", font_size=40, color=BLACK).next_to(input3, LEFT*5)

        # arrow01 = Arrow(input1_label.get_right(), input1.get_left(), buff=0.2, color=BLACK)
        # arrow02 = Arrow(input2_label.get_right(), input2.get_left(), buff=0.2,color=BLACK)
        # arrow03 = Arrow(input3_label.get_right(), input3.get_left(), buff=0.2,color=BLACK)
     
        
        # # Animate elements appearing
        # input_label = Text("Input Data", font_size=24, color=BLACK).next_to(input1_label, UP)
        # weights_label = Text("Input Layer", font_size=24, color=BLACK).next_to(input1, UP)
        # bias_name = Text("Bias", font_size=24, color=BLACK).next_to(bias, UP)
        # self.add(input_label)
        # self.add(input1_label, input2_label, input3_label)
        # self.play(Create(arrow01), Create(arrow02), Create(arrow03))
        # self.wait()
        # self.play(FadeIn(input1, input2, input3,bias))
        
        # self.add(weights_label)

        # self.add(bias_label)
        # self.add(bias_name)
        # # Add arrows
        # self.wait()
        # self.play(Create(arrow1), Create(arrow2), Create(arrow3), Create(arrow_bias))

        # self.add(w1)
        # self.add(w2)
        # self.add(w3)
        # self.wait()

        # # Add the output circle and the divider
        # self.play(FadeIn(output))
        # self.add(divider)
        
        # # Add labels for the output
        # self.add(sigma_label, f_label)
        
        # # Add the output arrow
        # # output_arrow = Arrow(output.get_right(), RIGHT * 3, buff=0.2)
        # self.play(Create(arrow4))

        # # Add the "output" label below the larger circle
        # output_label = Text("Output", font_size=24, color=BLACK).next_to(arrow4, DOWN)

        # self.add(output_label)

        # # Add the equation below the output label
        # equation = MathTex(r"h(x) = \sigma(x_1 w_1 +x_2 w_2 +x_3 w_3+b)",color=BLACK,font_size=30).next_to(output_label, DOWN*3)
        # self.add(equation)

        # self.wait(5)
        # self.next_slide()
        # self.clear()

        # Apply a scale factor to reduce the overall size
        scale_factor = 0.7  # Adjust this value (e.g., 0.5 to 0.8) to make it smaller or larger


       # Slide X: Key Concepts with Animated Bullets
        slide_title = Text("Coeficiente de restitucion (COR)", font_size=35,color=BLACK).to_edge(UP)
        
        # Bullet points

        # bullet1 = Text("• A Neuron is composed by a part that is Linear Transformation", font_size=25,color=BLACK)
        # bullet2 = Text("• Linear Transformation: ", font_size=25,color=BLACK)
        bullet1 = Tex(
                    r"$COR = \sqrt{1-\left(\frac{v_{sep}}{v}\right)^2}$",
                    font_size=25,
                    color=BLACK
                )
        def_LT = Tex(
            r"COR: es una medida de la elasticidad de una colision\\"
            r"COR va de 0 a 1 describe una colision perfectamente inelastica y elastica, respectivamente.\\"
            r"$COR > 0$ denota separacion del grano",
            font_size=25,
            color=BLACK
        )


        bullet1.next_to(slide_title, DOWN, buff=1)
        
        def_LT.next_to(bullet1, DOWN, buff=1)
        
        # Add title first
        self.add(slide_title)
        self.wait(0.5)
        
        # Animate each bullet separately
        self.play(FadeIn(bullet1, shift=RIGHT))
        self.wait(0.5)
        self.play(FadeIn(def_LT, shift=RIGHT))
        # self.wait(0.5)
        self.wait(1)


        self.next_slide()
        self.clear()


        
       # Slide X: Key Concepts with Animated Bullets
        # slide_title = Text("Components of a Neuron", font_size=35, color=BLACK).to_edge(UP)

        # # Bullet points
        # bullet0 = Text("• A Neuron is composed by a part that is Linear Transformation", font_size=25,color=BLACK)
        # b1 = Text(r"• And an activation function", font_size=25, color=BLACK)
        # eqb1 = Tex(r"e.g $\sigma(x) = \frac{1}{1 + e^{-x}}$", font_size=25, color=BLACK)
        # bullet1 = VGroup(b1, eqb1).arrange(DOWN, buff=0.2)
        
        # bullet0.next_to(slide_title, DOWN, buff=0.5)
        # # bullet1.next_to(slide_title, DOWN, buff=1.5)
        # bullet1.next_to(slide_title, DOWN, buff=1.5).align_to(bullet0, LEFT)

        # # Definition of Linear Transformation

        # # Axes for the sigmoid plot
        # axes = Axes(
        #     x_range=[-6, 6, 1],  # X-axis range
        #     y_range=[0, 1.2, 0.2],  # Y-axis range
        #     axis_config={"color": BLACK},
        # ).scale(0.5)  # Scale down for better fit

        # # Positioning the axes below the bullet points
        # axes.next_to(bullet1, DOWN, buff=1).align_to(slide_title, LEFT)
        # axes.set_color(BLACK)
        # # Labels for axes
        # x_label = axes.get_x_axis_label("x")
        # y_label = axes.get_y_axis_label(r"\sigma(x)")
        # x_label.set_color(BLACK)
        # y_label.set_color(BLACK)
        # # Define the sigmoid function
        # sigmoid_function = lambda x: 1 / (1 + np.exp(-x))
        # # relu_function = lambda x: np.maximum(0, x)
        # # softmax_function = lambda x: np.exp(x) / np.sum(np.exp(x), axis=0)

        # # Plot the sigmoid function
        # sigmoid_graph = axes.plot(sigmoid_function, color=BLUE)
        # # relu_graph = axes.plot(relu_function, color=GREEN)
        # # softmax_graph = axes.plot(softmax_function, color=RED)

        # # Sigmoid equation label
        # # equation = MathTex(r"\sigma(x) = \frac{1}{1 + e^{-x}}", color=WHITE)
        # # equation.next_to(axes, 0.1*LEFT)

        # # Add title first
        # self.add(slide_title)
        # self.wait(0.5)
        # # Animate each bullet separately
        # self.play(FadeIn(bullet0, shift=RIGHT))
        # self.wait(0.5)
        # self.play(FadeIn(bullet1, shift=RIGHT))
        # self.wait(0.5)
        # # self.play(FadeIn(bullet1, shift=RIGHT))
        # # self.wait(1)

        # # Draw the sigmoid function
        # self.play(Create(axes), Write(x_label), Write(y_label))
        # self.play(Create(sigmoid_graph))
        # # self.play(Create(relu_graph))
        # # self.play(Create(softmax_graph))
        # self.wait(2)

        # # Transition to next slide (optional)
        # self.next_slide()
        # self.clear()

        


        # Final Slide: Contact Info
        thanks = Text("Gracias por la atencion.", font_size=50,color= BLACK).to_edge(UP)
        contact_title = Text("Contacto:", font_size=30,color= BLACK).next_to(thanks,DOWN)
        contact_info = Text("anibal.varela@unsam.edu.ar", font_size=25, color=BLUE).next_to(contact_title, DOWN)
        self.play(FadeIn(thanks),FadeIn(contact_title), FadeIn(contact_info))
        self.next_slide()
