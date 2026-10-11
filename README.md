# Chulbal

서울 1–8호선에서, 도착 시각과 혼잡도를 같이 보고 출발 시각을 고르는 앱이다.

추천 계산은 R이 한다. 화면은 Shiny다.

## 문서

- [docs/01-planning.md](docs/01-planning.md) — 누가 여는지, 이미 있는 앱과 어디가 다른지
- [docs/02-data.md](docs/02-data.md) — 쓰는 파일과 조인 키
- [docs/03-recommendation.md](docs/03-recommendation.md) — 후보, 필터, 고르는 규칙
- [docs/04-scope.md](docs/04-scope.md) — 넣는 화면과 넣지 않는 기능

## 앱 켜기

R에서 패키지 `shiny`가 있어야 한다.

```r
source("app.R")
```

PowerShell에서는 이렇게 켠다.

```powershell
& "C:\Program Files\R\R-4.2.2\bin\x64\Rscript.exe" -e "shiny::runApp('c:/GitHub/Chulbal')"
```

완성된 기능은 [docs/04-scope.md](docs/04-scope.md)에 있다. 공공데이터 파일은 `data/raw/`에 둔다. 그 폴더는 git에 올리지 않는다.
