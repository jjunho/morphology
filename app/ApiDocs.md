## Endpoints da API de Morfologia Verbal

### Informações Gerais

- **Autenticação**: Nenhuma/autenticação com chave de API.
- **Limite de Requisições**: Até 100 requisições por minuto.
- **Formato da Resposta**: Todas as respostas estão no formato JSON.  
- **Códigos de Status HTTP**:
  - `200 OK`: Requisição bem-sucedida.
  - `400 Bad Request`: Parâmetros inválidos.
  - `500 Internal Server Error`: Erro interno da API.

---

### GET /morphology/v1/health

**Descrição:**
Retorna um JSON com o status da API, usado para verificar se a API está operacional. Ideal para rotinas de monitoramento.

**Estrutura da Resposta:**

- `status` (String): Estado atual da API, como "ok" ou "error".
- `message` (String): Mensagem informativa ou complementar.
- `content` (Object): Dados principais retornados. Neste caso, um objeto vazio `{}`.

**Valores possíveis para `status`:**

- `"ok"`: A API está funcionando normalmente.
- `"error"`: Ocorreu algum problema na API.

**Exemplo de Resposta:**

```json
{
  "status": "ok",
  "message": "A API está em execução",
  "content": {}
}
```

---

### GET /morphology/v1/paradigm/:citation

**Descrição:**
Retorna um JSON contendo o paradigma verbal para a forma de citação do verbo especificado.  
Um paradigma verbal é o conjunto de todas as formas conjugadas de um verbo, organizadas por tempos, modos e pessoas gramaticais.  

Esse endpoint é útil em aplicativos que manipulam flexões verbais, como conjugadores automáticos ou editores linguísticos.

**Estrutura da Resposta:**

- `status` (String): Estado da resposta, como "ok" ou "error".
- `message` (String): Mensagem informativa ou complementar.
- `content` (Object): Contém o paradigma verbal solicitado.

**Parâmetro:**

- `:citation` (String): Forma de citação do verbo (e.g., "correr", "amar").

**Exemplo de Requisição:**

```http
GET /morphology/v1/paradigm/correr
```

**Exemplo de Resposta Bem-sucedida:**

```json
{
  "status": "ok",
  "message": "",
  "content": { "paradigma": "..." }
}
```

**Exemplo de Resposta com Erro:**

```json
{
  "status": "error",
  "message": "Verbo não encontrado",
  "content": {}
}
```

---

### GET /morphology/v1/tense_paradigm/:citation/:tense

**Descrição:**
Retorna um JSON com o paradigma verbal para a forma de citação informada, filtrado por um tempo verbal específico.  

Esse endpoint permite obter flexões verbais detalhadas para usos mais específicos.

**Estrutura da Resposta:**

- `status` (String): Estado da resposta, como "ok" ou "error".
- `message` (String): Mensagem informativa ou complementar.
- `content` (Object): Contém o paradigma verbal no tempo solicitado.

**Parâmetros:**

- `:citation` (String): Forma de citação do verbo (e.g., "correr", "amar").
- `:tense` (String): Tempo verbal solicitado.

**Valores possíveis para `:tense`:**

- `"IPRS"`: Presente do Indicativo
- `"IPRF"`: Pretérito Perfeito do Indicativo
- `"IIPF"`: Pretérito Imperfeito do Indicativo
- `"IPPF"`: Pretérito Mais-que-perfeito do Indicativo
- `"IFUT"`: Futuro do Indicativo
- `"COND"`: Condicional Presente
- `"SPRS"`: Presente do Subjuntivo
- `"SIPF"`: Pretérito Imperfeito do Subjuntivo
- `"SFUT"`: Futuro do Subjuntivo
- `"IMPA"`: Imperativo Afirmativo
- `"IMPN"`: Imperativo Negativo
- `"INFP"`: Infinitivo Pessoal
- `"INF"`: Infinitivo
- `"GER"`: Gerúndio
- `"PPP"`: Particípio Passado Passivo

**Exemplo de Requisição:**

```http
GET /morphology/v1/tense_paradigm/correr/IPRS
```

**Exemplo de Resposta Bem-sucedida:**

```json
{
  "status": "ok",
  "message": "",
  "content": { "paradigma": "..." }
}
```

**Exemplo de Resposta com Erro:**

```json
{
  "status": "error",
  "message": "Tempo verbal não encontrado",
  "content": {}
}
```
