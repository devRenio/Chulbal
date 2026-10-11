source("R/recommend.R", encoding = "UTF-8")

library(shiny)

station_choices <- setNames(sample_stations$station_id, sample_stations$station_name)

situation_choices <- c(
  "제시간 도착" = "on_time",
  "조금 늦춰 출발" = "delay_departure",
  "비 오기 전" = "before_rain",
  "앉아서 가기" = "seated",
  "막차 전 한산한 차" = "last_quiet",
  "사이에 집 갔다 오기" = "home_roundtrip",
  "맞춰서 만나기" = "meet",
  "지연이 끝난 뒤" = "after_delay"
)

situation_rules <- c(
  on_time = "도착 시각을 지키는 차 가운데, 경로 혼잡도 최댓값이 가장 낮은 출발을 고릅니다. 같으면 더 늦은 차입니다.",
  delay_departure = "출발 구간 안에서 경로 혼잡도 최댓값이 가장 낮은 차를 고릅니다. 같으면 더 늦은 차입니다.",
  before_rain = "제시간 도착과 같되, 강수확률이 높아지는 시각 이후 출발은 뺍니다. 혼잡도 숫자는 바꾸지 않습니다.",
  seated = "경로 혼잡도가 34% 미만이면서 제시간에 도착하는 차 가운데 가장 늦은 출발을 고릅니다.",
  last_quiet = "혼잡도 상한 이하인 차 가운데 가장 늦은 출발을 고릅니다. 없으면 실제 막차를 같이 알려 줍니다.",
  home_roundtrip = "나갔다가 다음 일정 전에 돌아오고, 두 편 모두 상한 이하인 조합 가운데 혼잡도 최댓값이 가장 낮은 것을 고릅니다.",
  meet = "두 사람이 만날 역에 제시간에 도착하도록, 각자 혼잡도가 낮은 출발을 따로 고릅니다.",
  after_delay = "지연 종료 시각 이후 출발만 남기고, 제시간 도착과 같게 고릅니다."
)

candidate_table <- function(frame) {
  if (is.null(frame) || nrow(frame) == 0) {
    return(NULL)
  }
  header <- tags$tr(lapply(names(frame), tags$th))
  body <- lapply(seq_len(nrow(frame)), function(row_index) {
    picked <- "판정" %in% names(frame) && identical(as.character(frame[["판정"]][[row_index]]), "추천")
    tags$tr(
      style = if (isTRUE(picked)) "background:#e5f3e5;font-weight:600;" else NULL,
      lapply(as.character(frame[row_index, ]), tags$td)
    )
  })
  tags$table(class = "table", tags$thead(header), tags$tbody(body))
}

ui <- fluidPage(
  titlePanel("Chulbal"),
  p("샘플 시각표입니다. 분기 평균 혼잡도 파일이 들어오기 전의 화면입니다."),
  sidebarLayout(
    sidebarPanel(
      selectInput("situation", "상황", choices = situation_choices, selected = "on_time"),
      selectInput("origin", "출발 역", choices = station_choices, selected = "226"),
      selectInput("dest", "도착 역", choices = station_choices, selected = "221"),
      selectInput("day_type", "요일 구분", choices = c("평일", "토요일", "일요일", "공휴일")),
      conditionalPanel(
        "input.situation == 'on_time' || input.situation == 'before_rain' || input.situation == 'seated' || input.situation == 'after_delay'",
        textInput("arrive_by", "도착 시각", value = "09:00"),
        conditionalPanel(
          "input.situation == 'seated'",
          helpText("34%는 좌석이 가득 찬 상태입니다. 저녁 샘플은 도착 시각을 22:00으로 두면 보입니다.")
        )
      ),
      conditionalPanel(
        "input.situation == 'delay_departure'",
        textInput("earliest", "가장 이른 출발", value = "18:00"),
        textInput("latest", "가장 늦은 출발", value = "22:00")
      ),
      conditionalPanel(
        "input.situation == 'before_rain'",
        textInput("rain_from", "강수확률이 높아지는 시각", value = "08:30"),
        helpText("비우면 예보가 없다고 하고, 이 필터는 적용하지 않습니다.")
      ),
      conditionalPanel(
        "input.situation == 'last_quiet' || input.situation == 'home_roundtrip'",
        numericInput("cap", "혼잡도 상한(%)", value = 80, min = 1, max = 200)
      ),
      conditionalPanel(
        "input.situation == 'home_roundtrip'",
        selectInput("home", "집 역", choices = station_choices, selected = "221"),
        textInput("free_from", "비는 시각", value = "18:00"),
        textInput("next_event", "다음 일정", value = "21:00"),
        helpText("출발 역이 지금 있는 역입니다. 사당에서 강남 집으로, 비는 시각 18:00·다음 일정 21:00이면 19:10에 나가 19:25에 돌아옵니다.")
      ),
      conditionalPanel(
        "input.situation == 'meet'",
        selectInput("origin_b", "다른 사람 출발 역", choices = station_choices, selected = "232"),
        textInput("meet_at", "만날 시각", value = "09:00"),
        helpText("도착 역이 만날 역입니다.")
      ),
      conditionalPanel(
        "input.situation == 'after_delay'",
        textInput("alert_ends", "지연 종료 시각", value = "08:30"),
        helpText("비우면 지연이 없다고 보고 제시간 도착과 같게 계산합니다.")
      )
    ),
    mainPanel(
      textOutput("rule"),
      h3(textOutput("headline", container = span)),
      uiOutput("candidates")
    )
  )
)

server <- function(input, output, session) {
  recommendation <- reactive({
    if (input$situation != "meet" && input$situation != "home_roundtrip" && input$origin == input$dest) {
      return(empty_result("출발 역과 도착 역이 같습니다."))
    }
    switch(
      input$situation,
      on_time = recommend_on_time(input$origin, input$dest, input$arrive_by, input$day_type),
      delay_departure = recommend_delay_departure(input$origin, input$dest, input$earliest, input$latest, input$day_type),
      before_rain = recommend_before_rain(input$origin, input$dest, input$arrive_by, input$day_type, input$rain_from),
      seated = recommend_seated(input$origin, input$dest, input$arrive_by, input$day_type),
      last_quiet = recommend_last_quiet(input$origin, input$dest, input$day_type, input$cap),
      home_roundtrip = recommend_home_roundtrip(input$origin, input$home, input$free_from, input$next_event, input$day_type, input$cap),
      meet = recommend_meet(input$origin, input$origin_b, input$dest, input$meet_at, input$day_type),
      after_delay = recommend_after_delay(input$origin, input$dest, input$arrive_by, input$day_type, input$alert_ends),
      empty_result("상황을 고르지 못했습니다.")
    )
  })

  output$headline <- renderText(recommendation()$message)
  output$rule <- renderText(situation_rules[[input$situation]])
  output$candidates <- renderUI(candidate_table(recommendation()$table))
}

shinyApp(ui, server)
