msg_make_dialog <- list(
  title = "個体識別番号検索",
  input = "入力",
  output = "出力",
  from_csv = "csvから入力",
  from_cb = "クリップボードから入力",
  choose_file = "ファイルを選択",
  col_label = "列番号：",
  col_at = "列目（空欄：一列目）",
  row_label = "行番号：",
  row_from = "行目（空欄：一行目）～",
  row_to = "行目（空欄：最終行）",
  append = "出力ファイルが既に存在するとき、ファイルを新規作成するのではなく追記する",
  row_err = "開始行は最終行より小さい行番号を指定してください"
)

msg_dialog_finished <- list(
  title = "検索終了",
  message = "個体識別番号の検索が終了しました。"
)

msg_info <- list(
  cattle = c("個体識別番号", "出生の年月日", "雌雄の別", "母牛の個体識別番号",
             "種別"),
  farm = c("異動内容", "異動年月日", "都道府県", "市町村", "氏名または名称")
)

msg_scrape <- list(
  estimate = "終了予想時刻（目安）：",
  searching = "個体識別番号検索中...",
  after = "以降",
  finished = "検索が終了しました",
  error = "エラーが発生しました：",
  cannot_find = "以下の個体の情報が得られませんでした："
)

msg_make_dialog <- lapply(msg_make_dialog, iconv, to = "UTF-8")
msg_dialog_finished <- lapply(msg_dialog_finished, iconv, to = "UTF-8")
msg_info <- lapply(msg_info, iconv, to = "UTF-8")
msg_scrape <- lapply(msg_scrape, iconv, to = "UTF-8")

devtools::use_data(msg_make_dialog, msg_dialog_finished, msg_info, msg_scrape,
                   internal = T, overwrite = T)
