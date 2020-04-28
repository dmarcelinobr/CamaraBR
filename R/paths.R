#Casas
.CAMARA <- 'camara'
.SENADO <- 'senado'

# Link default da API
.CAMARA_API_LINK <- "https://dadosabertos.camara.leg.br"
.SENADO_API_LINK <- "http://legis.senado.leg.br"

# Links Alternativos
.CAMARA_WEBSITE_LINK <- "http://www.camara.gov.br"
.CAMARA_WEBSITE_LINK_2 <- "https://www.camara.leg.br"

# Mensagens de erro
.ERRO_RETORNO_JSON <- "API did not return json"
.WARNING_PROPOSICAO_ID <- "Pode haver campos incorretos na proposicao. Verifique o tipo, numero e ano."

# Paths Câmara
.CAMARA_PROPOSICOES_PATH <- "/api/v2/proposicoes"
.VOTACOES_PATH <- "/api/v2/votacoes"
.DEPUTADOS_PATH <- "/api/v2/deputados"
.PARTIDOS_PATH <- "/api/v2/partidos"
.CAMARA_SESSOES_PATH <- "/proposicoesWeb/sessoes_e_reunioes"
.TIPOS_PROPOSICOES_PATH <- "/api/v2/referencias/proposicoes/siglaTipo"
.ORGAOS_CAMARA_PATH <- "/api/v2/orgaos"
.AGENDA_CAMARA_PATH <- "/api/v2/eventos"
.ORGAOS_FILE_CAMARA_PATH <- "/arquivos/orgaos/json/orgaos.json"
.PAUTAS_CAMARA <- "/api/v2/eventos/"


# Path site Câmara
.PLENARY_CAMARA_PATH <- "SitCamaraWS/Proposicoes.asmx/ListarProposicoesVotadasEmPlenario"
.APENSADAS_CAMARA_PATH <- "/SitCamaraWS/Proposicoes.asmx/ObterProposicaoPorID"
.EMENDAS_SUBSTITUTIVOS_REDACAOFINAL_CAMARA_PATH <- "/SitCamaraWS/Orgaos.asmx/ObterEmendasSubstitutivoRedacaoFinal"
.EVENTOS_PROPOSICAO_CAMARA_PATH <- "/proposicoesWeb/sessoes_e_reunioes"
.AUTORES_CAMARA_PATH <- "/proposicoesWeb/prop_autores"


# Link do repositório do rcongresso
.CamaraBR_LINK <- "https://github.com/danielmarcelino/CamaraBR"

# Regex Pattern
.REGEX_PATTERN <- "(?=[A-Z][^A-Z])"
.REGEX_DEFERIMENTO_INDEFERIDO <- "^Indefiro"
.REGEX_DEFERIMENTO_DEFERIDO <- "^(Defiro)|(Aprovado)"