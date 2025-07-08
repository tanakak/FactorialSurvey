# install.packages("AlgDesign") # 未インストールの場合は実行
library(AlgDesign)

#' @title D効率計画による要因サーベイの組み合わせ生成
#'
#' @description 要因サーベイ実験の要因と水準から、D効率基準に基づいて
#'   最適なプロファイル（ビネット）の組み合わせを生成します。
#'
#' @param factors 要因と水準を定義したリスト。リストの各要素名が要因名、
#'   その中身のベクトルが水準名になります。
#' @param n_vignettes 生成したい合計プロファイル（ビネット）数。
#' @param n_blocks 作成するブロック（回答者に見せるセット）の数。
#'   `n_vignettes`は`n_blocks`で割り切れる必要があります。
#'
#' @return D効率的に選択されたプロファイルと、それらがどのブロックに
#'   属するかを示す`block`列を含むデータフレーム。
#'
#' @examples
#' # --- 1. 要因と水準の定義 ---
#' factor_list <- list(
#'   school_reputation = c("有名", "無名"),
#'   teacher_support = c("手厚い", "普通", "最低限"),
#'   tuition_fee = c("高い", "普通", "安い"),
#'   remote_learning = c("あり", "なし")
#' )
#'
#' # --- 2. 関数の実行 ---
#' # 合計36個のプロファイルを、12個のブロック（セット）に分ける
#' # (1ブロックあたり 36/12 = 3個のプロファイル)
#' survey_design <- create_fse_design(
#'   factors = factor_list,
#'   n_vignettes = 36,
#'   n_blocks = 12
#' )
#'
#' # --- 3. 結果の確認 ---
#' print(head(survey_design, 10))
#' table(survey_design$block) # 各ブロックのプロファイル数を確認
#'
create_fse_design <- function(factors, n_vignettes, n_blocks = 1) {
  
  # --- Step 1: 全組み合わせ（完全実施計画）の生成 ---
  full_factorial <- expand.grid(factors, KEEP.OUT.ATTRS = FALSE)
  cat(paste("全組み合わせの数:", nrow(full_factorial)), "\n")
  
  # --- Step 2: D効率計画によるプロファイルの抽出 ---
  # optFederov関数で、統計的に最も効率の良い組み合わせをn_vignettes個選ぶ
  # nRepeatsを増やすと、より良いデザインが見つかる可能性が高まる
  cat(paste(n_vignettes, "個のプロファイルをD効率基準で抽出中..."), "\n")
  
  efficient_design <- AlgDesign::optFederov(
    frml = ~., # 全ての主効果を考慮
    data = full_factorial,
    nTrials = n_vignettes,
    criterion = "D", # D効率基準
    nRepeats = 50
  )
  
  # --- Step 3: 抽出されたプロファイルをブロックに分割 ---
  # 回答者ごとに異なるセットを見せるため、プロファイルを均等なブロックに分ける
  # optBlock関数が、ブロックによる影響を最小化するように割り当ててくれる
  cat(paste(n_blocks, "個のブロックに分割中..."), "\n")
  
  blocked_design <- AlgDesign::optBlock(
    frml = ~.,
    withinData = efficient_design$design,
    blocksizes = rep(n_vignettes / n_blocks, n_blocks),
    nRepeats = 50
  )
  
  cat("処理が完了しました。\n\n")
  
  # 結果を一つのデータフレームにまとめる
  final_design <- blocked_design$designs[[1]]
  final_design$block <- blocked_design$blocks
  
  return(final_design)
}