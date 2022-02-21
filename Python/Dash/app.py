import dash
import dash_bootstrap_components as dbc
import dash_html_components as html
import dash_core_components as dcc
import plotly.express as px
from dash.dependencies import Input, Output
import pandas as pd
import base64
import functions as f
import sys

#Sidebar reference:https://github.com/Coding-with-Adam/Dash-by-Plotly/blob/master/Bootstrap/Side-Bar/side_bar.py

# data source: https://www.kaggle.com/chubak/iranian-students-from-1968-to-2017
# data owner: Chubak Bidpaa
df = pd.read_csv('https://raw.githubusercontent.com/Coding-with-Adam/Dash-by-Plotly/master/Bootstrap/Side-Bar/iranian_students.csv')

app = dash.Dash(__name__, external_stylesheets=[dbc.themes.CERULEAN])
predicted_value = 0.00
card_content = [
    
    dbc.CardHeader(html.H5("Previsão do preço de fechamento")),
    dbc.CardBody(
        [
            html.H5(f"R$ {predicted_value}", className="card-title"),
            
        ]
    ),
]
# styling the sidebar
SIDEBAR_STYLE = {
    "position": "fixed",
    "top": 0,
    "left": 0,
    "bottom": 0,
    "width": "16rem",
    "padding": "2rem 1rem",
    "background-color": "#f8f9fa",
}

# padding for the page content
CONTENT_STYLE = {
    "margin-left": "18rem",
    "margin-right": "2rem",
    "padding": "2rem 1rem",
}
test_png = 'teste.png'
test_base64 = base64.b64encode(open(test_png, 'rb').read()).decode('ascii')
sidebar = html.Div(
    [
        #html.H2("Bem vindo!", className="display-4"),
        #html.Img(src='data:image/png;base64,{}'.format(test_base64), style={'height':'10%',}),
        html.Hr(),
        # html.P(
        #     "Barra de Navegação", className="lead"
        # ),
        dbc.Nav(
            [
                dbc.NavLink("Início", href="/", active="exact"),
                dbc.NavLink("Predição em tempo real", href="/pagina_predicao", active="exact"),
                dbc.NavLink("Sobre os desenvolvedores", href="/page-2", active="exact"),
            ],
            vertical=True,
            pills=True,
        ),
    ],
    style=SIDEBAR_STYLE,
)

content = html.Div(id="page-content", children=[], style=CONTENT_STYLE)

app.layout = html.Div([
    dcc.Location(id="url"),
    sidebar,
    content
])


@app.callback(
    Output("page-content", "children"),
    [Input("url", "pathname")]
)
def render_page_content(pathname):
    if pathname == "/":
        return [
                html.H1('Início',
                        style={'textAlign':'center'}),
                html.P('Teste de Dash'),
                ]
    elif pathname == "/pagina_predicao":
        return [
                html.H1('Predição em tempo real',
                        style={'textAlign':'center'}),

                html.Br(),
                html.P('Página em construção'),
                html.B(html.Label('Escolha o ativo:')),                
                dcc.Dropdown(id = 'acao_selecionada',
                    options=[
                        {'label': i, 'value': i} for i in f.lista_acoes_disponiveis()
                    ],
                    value=''
                ),

                html.Br(),
                html.B(html.Label('Preço de abertura: ')),
                html.Br(),
                dcc.Input(id='preco_abertura', value='0.00', type='text'),

                html.Br(),
                html.Br(),
                html.B(html.Label('Preço mais alto até agora: ')),
                html.Br(),
                dcc.Input(id='maior_preco', value='0.00', type='text'),

                html.Br(),
                html.Br(),
                html.B(html.Label('Preço mais baixo até agora: ')),
                html.Br(),
                dcc.Input(id='menor_preco', value='0.00', type='text'),

                html.Br(),
                html.Br(),
                html.B(html.Label('Volume: ')),
                html.Br(),
                dcc.Input(id='volume_ativo', value='00000000', type='text'),
                
                html.Br(),
                html.Br(),
                html.Button(id='calcular', n_clicks=0, children='Realizar Previsão'),


                html.Br(),
                html.Br(),
    
                
                dbc.Row(
            [
                dbc.Col(dbc.Card(card_content, color="dark", inverse=True)),
            ]
        ),
        

                
                
                
                
                
                ]
    elif pathname == "/page-2":
        return [
                html.H1('High School in Iran',
                        style={'textAlign':'center'}),
                dcc.Graph(id='bargraph',
                         figure=px.bar(df, barmode='group', x='Years',
                         y=['Girls High School', 'Boys High School']))
                ]
    # If the user tries to reach a different page, return a 404 message
    return dbc.Jumbotron(
        [
            html.H1("404: Not found", className="text-danger"),
            html.Hr(),
            html.P(f"The pathname {pathname} was not recognised..."),
        ]
    )


if __name__=='__main__':
    app.run_server(debug=True, port=3000)