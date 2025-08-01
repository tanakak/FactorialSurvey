#' @title D効率計画による要因サーベイの組み合わせ生成
#'
#' @description 要因サーベイ実験の要因と水準から、D効率基準に基づいて
#'   最適なプロファイル（ビネット）の組み合わせを生成します。
#'
#' @param factors 要因と水準を定義したリスト。
#' @param n_vignettes 生成したい合計プロファイル（ビネット）数。
#' @param n_blocks 作成するブロック（回答者に見せるセット）の数。
#'   `n_vignettes`は`n_blocks`で割り切れる必要があります。
#' @param seed 再現性を確保するための乱数シード。`NULL`の場合は設定しません。
#' @param n_repeats D効率設計時の探索試行回数。
#' @param shuffle_within_blocks 各ブロック内で行順をランダムにするか。
#' @param export_csv 結果をCSVとして保存するか。
#' @param file_name 保存するCSVファイル名。指定しない場合は自動命名。
#' @param verbose TRUEなら進行状況を出力。
#'
#' @return D効率的に選択されたプロファイルとブロック列を含むデータフレーム（class: fse_design）
#'
#' @importFrom utils write.csv
#' @export
fs_defficient <- function(
    factors,
    n_vignettes,
    n_blocks = 1,
    seed = NULL,
    n_repeats = 50,
    shuffle_within_blocks = TRUE,
    export_csv = FALSE,
    file_name = NULL,
    verbose = TRUE
) {
  if (!is.list(factors)) stop("引数 'factors' はリストである必要があります。")
  if (n_vignettes %% n_blocks != 0) {
    stop("引数 'n_vignettes' は 'n_blocks' で割り切れる必要があります。")
  }

  full_factorial <- expand.grid(factors, KEEP.OUT.ATTRS = FALSE)
  total_combinations <- nrow(full_factorial)
  if (verbose) message("全組み合わせの数: ", total_combinations)

  if (n_vignettes > total_combinations) {
    stop("生成したいビネット数が全組み合わせ数を超えています。")
  }

  if (!is.null(seed)) set.seed(seed)
  if (verbose) message(n_vignettes, "個のプロファイルをD効率基準で抽出中...")

  efficient_design <- AlgDesign::optFederov(
    frml = ~.,
    data = full_factorial,
    nTrials = n_vignettes,
    criterion = "D",
    nRepeats = n_repeats
  )

  if (verbose) message("D-efficiency: ", round(efficient_design$D, 4))

  if (n_blocks > 1) {
    if (verbose) message(n_blocks, "個のブロックに分割中...")
    blocked <- AlgDesign::optBlock(
      frml = ~.,
      withinData = efficient_design$design,
      blocksizes = rep(n_vignettes / n_blocks, n_blocks),
      nRepeats = n_repeats
    )
    design <- blocked$designs[[1]]
    design$block <- blocked$blocks
  } else {
    design <- efficient_design$design
    design$block <- 1
  }

  # ブロック内のシャッフル（提示順効果の回避）
  if (shuffle_within_blocks) {
    # 修正点: as.data.frame() で明示的に変換し、split() のエラーを回避
    design_list <- split(as.data.frame(design), design$block)
    design <- do.call(rbind, lapply(design_list, function(df) df[sample(nrow(df)), ]))
  }

  # 属性付与
  attr(design, "factors") <- factors
  class(design) <- c("fse_design", "data.frame")

  # CSV出力
  if (export_csv) {
    if (is.null(file_name)) {
      file_name <- paste0("fse_design_", format(Sys.Date(), "%Y%m%d"), ".csv")
    }
    write.csv(design, file = file_name, row.names = FALSE, fileEncoding = "UTF-8")
    if (verbose) message("CSVとして保存されました: ", file_name)
  }

  if (verbose) message("処理が完了しました。\n")
  return(design)
}