# FactorialSurvey パッケージ — 現状診断と CRAN 公開までのロードマップ

作成日: 2026-07-25
対象: `github.com/tanakak/FactorialSurvey` (master, `c24a2ec`) およびローカル作業フォルダ
守備範囲の前提: **Factorial survey(ビネット実験)特化**（離散選択実験は将来拡張として扱う）
異質性への対応方針: **マルチレベル + 有限混合 + ランダムパラメータの3本立て**

---

## 0. エグゼクティブサマリ

現状は「動く関数が4本ある実験段階のリポジトリ」で、CRAN 提出可能な状態からは距離があります。ただし**構想は良く、ニッチが空いている**のが重要な点です。最も近い競合だった `factorEx`（Factorial Experiments の設計と分析）は依存パッケージの Archive に巻き込まれ **2022-08-12 に CRAN から削除**されており、ビネット実験に特化した設計〜推定〜WTP を一気通貫で扱うパッケージは現在 CRAN に存在しません。

やるべきことは大きく3層に分かれます。

| 層 | 内容 | 工数感 |
|---|---|---|
| **A. 衛生管理** | 破損ファイルの復旧、DESCRIPTION 整備、命名の統一、テストの作り直し、`R CMD check` を通す | 2〜3週 |
| **B. 機能拡充** | ご指摘の6領域（D効率設計・post-estimation・可視化・異質性・有限混合・予測/シミュレーション） | 3〜4か月 |
| **C. CRAN 提出** | vignette、サンプルデータ、examples、多プラットフォーム check、cran-comments | 3〜4週 |

以下、優先度順に詳述します。

---

## 1. 【最優先・データ損失リスク】ローカルフォルダが破損しています

Google Drive 上の作業フォルダ `Projects/FactorialSurvey/` にある以下のファイルは、**中身が R コードではなく GitHub のウェブページ HTML** になっています（各 240〜300 KB）。おそらくブラウザから「別名で保存」してしまった結果です。

```
R/fs_defficient.R          ← HTML (GitHub blob ページ)
R/fs_mwtp.R                ← HTML
R/set_lavels.R             ← HTML
man/fs_betareg.Rd          ← HTML
man/fs_defficient.Rd       ← HTML
man/fs_mwtp.Rd             ← HTML
man/set_lavels.Rd          ← HTML
tests/testthat/test-fs_betareg.R    ← HTML
tests/testthat/test-fs_defficient.R ← HTML
tests/testthat/test-set_lavels.R    ← HTML
```

加えて `DESCRIPTION.html` / `NAMESPACE.html` / `LICENSE.html` / `git.html` / `FactorialSurvey (1).Rproj`（実体は HTML）などのゴミファイルが散在し、ローカルの `DESCRIPTION` はパッケージ名が `factorialSurvey`（小文字 f）のまま初期テンプレート状態で、GitHub 側の `FactorialSurvey` と食い違っています。

**幸い GitHub 側は健全です。** 損失はありません。ローカルを捨てて clone し直すのが最短です。

```bash
# 1) 現在のフォルダを退避（削除ではなくリネーム）
cd ~/Library/CloudStorage/GoogleDrive-katsuyat@gmail.com/マイドライブ/Projects
mv FactorialSurvey FactorialSurvey_broken_20260725

# 2) GitHub から clone し直す
git clone https://github.com/tanakak/FactorialSurvey.git FactorialSurvey

# 3) 中身を確認してから退避フォルダを削除
cd FactorialSurvey && git log --oneline | head
```

> **今後の運用上の注意**: Google Drive のクラウド同期フォルダ配下に Git リポジトリを置くと、`.git/` の同期競合でリポジトリが壊れることがあります。開発用リポジトリは `~/Documents/` や `~/dev/` などローカルディスクに置き、バックアップは GitHub に任せる運用を強く推奨します（`Test.R` の中に `setwd("/Users/katsuyat/Documents/FactorialSurvey")` と `.../GoogleDrive.../factorialSurvey` の両方が残っているのも、この二重管理が原因と思われます）。

---

## 2. 現状の棚卸し（GitHub master 基準）

```
FactorialSurvey/
├── DESCRIPTION              ← Title/Description/Authors がテンプレートのまま
├── NAMESPACE                ← roxygen 生成、4関数 export
├── LICENSE / LICENSE.md     ← MIT、形式は正しい
├── R/
│   ├── fs_defficient.R      121行  D効率設計の生成
│   ├── fs_betareg.R         133行  ベータ回帰 + 限界効果
│   ├── fs_mwtp.R            136行  MWTP（デルタ法）
│   ├── set_lavels.R          14行  水準ラベル付与
│   └── fs_betaregのコピー.R  131行  ★不要ファイル（後述）
├── man/                     4つの .Rd（roxygen 生成）
├── tests/testthat/
│   ├── test-fs_betareg.R    124行  ★中身は妥当
│   ├── test-fs_defficient.R  94行  ★中身がテストではなく関数の旧ソース
│   └── test-set_lavels.R     12行
├── Test_250705.R            動作確認用スクリプト（.Rbuildignore 済み）
└── git                      ★0バイトの空ファイル（誤コミット）
```

依存: `Imports: betareg, margins, AlgDesign, msm` / `Suggests: testthat, withr`

---

## 3. CRAN 提出を阻む問題（Blocker）

優先度 P0 = 提出前に必ず解消 / P1 = 強く推奨 / P2 = あると良い

| # | 優先 | 問題 | 対応 |
|---|---|---|---|
| B1 | **P0** | `R/fs_betaregのコピー.R` が存在。**日本語ファイル名**かつ `fs_betareg` を**二重定義**。`R CMD check` で確実に落ちる | ファイルを削除。差分（本家より優れた部分がある）は本体にマージしてから削除 |
| B2 | **P0** | `tests/testthat/test-fs_defficient.R` の中身が**テストではなく `fs_defficient` の旧バージョンのソースコード**。テスト実行時に関数が上書き定義される | 本物のテストに書き直す |
| B3 | **P0** | DESCRIPTION の `Title` が "What the Package Does (One Line, Title Case)"、`Description` が "What the package does (one paragraph)."、`Authors@R` が `person("First","Last", "first.last@example.com")` のまま | 実際の内容に差し替え（後述の雛形参照） |
| B4 | **P0** | 公開関数に**実行可能な `@examples` が無い**。`fs_mwtp` のみ `\dontrun{}`。CRAN は実行される例を要求 | 全 export 関数に `@examples` を付与。重いものは `\donttest{}`（`\dontrun{}` は避ける） |
| B5 | **P0** | `fs_defficient` が**関数内で `set.seed()` を呼び、グローバルな RNG 状態を書き換える**。CRAN Policy 違反 | `on.exit()` で `.Random.seed` を復元する、または `withr::with_seed()` を使う |
| B6 | **P0** | パッケージ名の不一致（GitHub: `FactorialSurvey` / ローカル DESCRIPTION: `factorialSurvey`） | `FactorialSurvey` に統一。**CRAN 上で同名・同名異ケースの既存パッケージが無いことは確認済み**（利用可能） |
| B7 | **P0** | 関数名 `set_lavels` は `set_labels` の**タイプミス** | `set_labels()` にリネーム。CRAN 公開前なので互換 shim は不要（公開後の改名は非常に面倒なので今のうちに） |
| B8 | **P0** | R コード中に非 ASCII 文字（`R/fs_betareg.R` の `★★★ 修正点 ★★★` 等）。`R CMD check` が NOTE を出す | コメントを英語化。ユーザー向け文字列は絶対に非 ASCII を含めない |
| B9 | **P1** | `Imports: margins` — `margins` は **2017年3月の v0.3.0 が最後**で実質メンテナンス停止。作者自身が後継として `marginaleffects` を案内している | `marginaleffects` へ移行（後述 §6.4）。移行期は `margins` を `Suggests` に落とす |
| B10 | **P1** | `betareg` / `margins` / `msm` を `requireNamespace()` で条件分岐しているのに `Imports` に置いている（設計の不整合） | 条件付き利用は `Suggests`、無条件利用は `Imports` に整理 |
| B11 | **P1** | 0バイトの `git` ファイルが commit されている | `git rm git` |
| B12 | **P1** | README.md / NEWS.md / vignette / pkgdown / CI が無い | すべて整備（§8） |
| B13 | **P2** | `fs_betareg` / `fs_mwtp` が `cat()` で無条件に出力する | 計算関数と表示を分離し、`print()` / `summary()` メソッドに委譲 |
| B14 | **P2** | S3 クラス名が `fse_design` と `fs_` プレフィックスで不統一 | `fs_design` に統一 |

---

## 4. 既存関数のバグ・設計上の問題

### 4.1 `fs_betareg()` — 擬似R²の計算が壊れています

```r
cat("Log-Likelihood (Null Model, LL(0)):", model$loglik.null, "\n")
pseudo_r2 <- 1 - (as.numeric(logLik(model)) / model$loglik.null)
cat("Pseudo-R² (McFadden):", round(pseudo_r2, 4), "\n")
```

`betareg` オブジェクトの返り値に **`loglik.null` という要素は存在しません**（`coefficients, residuals, fitted.values, optim, method, control, start, weights, offset, n, nobs, df.null, df.residual, phi, loglik, vcov, pseudo.r.squared, link, converged, call, formula, terms, levels, contrasts, model, y, x`）。したがって `model$loglik.null` は `NULL` を返し、`pseudo_r2` は `numeric(0)` になり、`cat()` は**何も表示しません**（エラーも出ないので気付きにくい）。

さらに、`betareg` が持つ `pseudo.r.squared` は **McFadden の擬似R²ではなく「線形予測子とリンク変換した応答変数の相関の2乗」**です。ラベルも誤りになります。

**修正案**:
```r
# 切片のみモデルを明示的に推定して McFadden を計算する
null_fit  <- betareg::betareg(update(formula, . ~ 1), data = data_internal)
ll_full   <- as.numeric(logLik(model))
ll_null   <- as.numeric(logLik(null_fit))
mcfadden  <- 1 - ll_full / ll_null
# betareg 本来の擬似R²も別ラベルで併記する
r2_betareg <- model$pseudo.r.squared
```

### 4.2 `fs_betareg()` — その他

- `transform = TRUE` が**0〜10点尺度を暗黙の前提にしている**（`y / 10` がハードコード）。7段階リッカートや0〜100点には使えません。→ `scale_range = c(0, 10)` 引数を追加し、`y_scaled <- (y - min) / (max - min)` に一般化。
- 端点の扱いが Smithson–Verkuilen 変換のみ。**評定データは 0点/10点への回答集中（端点集積）が非常に多い**ので、`ordbeta`（Kubinec 2022 の ordered beta regression）を選べるようにすべきです。`glmmTMB` が `ordbeta()` family を実装済みなので実装コストは低いです。
- `data_internal <- data` によるコピーで副作用は回避できていますが、`weights` / `subset` / `na.action` を受け取れません。
- 返り値が素のリストで S3 クラスが無いため、`print`/`summary`/`plot`/`predict` を後付けできません。

### 4.3 `fs_mwtp()` — 統計的な弱点

- **`betareg` オブジェクトにハードコード**（`inherits(model_results$model, "betareg")`）。他のモデルに使えません。
- 95%CI が `estimate ± 1.96 × SE` の正規近似。**WTP は係数の比なので分布が強く非対称**（分母がゼロ近傍だと発散）。デルタ法の正規近似は文献上しばしば不適切とされます。→ **Krinsky–Robb（パラメトリック・ブートストラップ）と Fieller 区間**を追加し、`method = c("delta", "krinsky-robb", "fieller", "bootstrap")` で選べるように。
- リンク関数（logit）スケール上の係数比を WTP と解釈している点を、ドキュメントで明示する必要があります。応答スケール上の限界効果比とは一致しません。
- 一度に1属性しか扱えません。→ 全属性を一括で返す `fs_mwtp(fit)`（`variable` 省略時）を既定に。

### 4.4 `fs_defficient()` — 機能不足

- **禁止組み合わせ（restrictions）を指定できない**。ビネット実験では「築年数1年の空き家」のような論理的に矛盾する組み合わせを除外する必要があり、実務上ほぼ必須です。
- `expand.grid()` で全組み合わせを毎回展開するため、**要因数が増えると爆発**します（例: 2水準×15要因 = 32,768行）。候補集合のサブサンプリングや `optFederov` の `augment` オプションの活用が必要。
- モデル式が `~.`（主効果のみ）に固定。**交互作用を推定したい設計に対応できません**。→ `model = ~ .` を引数化して `~ .^2` を許す。
- ブロッキングが**単なるランダム分割**（`sample()` して `rep(1:n_blocks, each=...)`）。`AlgDesign::optBlock()` による最適ブロック化を選べるようにすべきです（旧版のテストファイルには `optBlock` を使ったコードが残っており、意図的にランダム分割へ後退させたようですが、両方を提供するのが妥当です）。
- **設計の診断が D効率の数値ひとつだけ**。水準バランス表、属性間の相関行列、VIF、A効率・G効率、直交設計に対する相対効率が出ません。
- 提示順の統制が「ブロック内シャッフル」のみ。**順序効果の除去には Williams design（バランス型ラテン方格）**が必要です。

### 4.5 `set_lavels()`

- 名前のタイプミス（B7）。
- `levels = seq_along(labels)` を仮定しており、**元の変数が 1,2,3,… の整数コードである場合しか動きません**。0始まりや文字列コードには対応不可。→ `levels` 引数を追加。
- `NA` の扱い、既に factor である場合の扱いが未定義。

---

## 5. エコシステム上の立ち位置（CRAN 競合調査）

| パッケージ | 状態 | 役割 | FactorialSurvey との関係 |
|---|---|---|---|
| **factorEx** | **CRAN から削除済み（2022-08-12）** | Factorial experiments の設計と分析 | **最大の競合が不在。ここが空きニッチ** |
| **cregg** | 現役 | Conjoint（選択型）の AMCE 推定と可視化 | 可視化 API（forest plot）の作法は大いに参考にすべき。ただし選択型が主 |
| **idefix** | 現役 | 離散選択実験の D効率／Bayesian efficient design | 選択型の設計。今回の守備範囲外だが将来の DCE 拡張時の参照先 |
| **AlgDesign** | 現役 | 汎用の最適計画（`optFederov`, `optBlock`） | **現行の設計エンジン。継続利用で問題なし** |
| **betareg** | 現役 (3.2-5) | ベータ回帰 + **`betamix()` による有限混合ベータ回帰（flexmix ベース）** | 推定エンジン。**有限混合モデルは `betamix` をラップするのが最短経路** |
| **glmmTMB** | 現役 | 混合効果モデル。`beta_family()` と **`ordbeta()`** を実装 | **マルチレベル／ランダムパラメータの推定エンジンとして最適** |
| **ordbetareg** | 現役 (0.8) | `brms` ベースの ordered beta regression（ベイズ） | ベイズ版のオプションとして `Suggests` 候補 |
| **marginaleffects** | 現役・活発 | 予測・限界効果・対比・仮説検定を100超のモデルクラスで統一 | **post-estimation の中核基盤にすべき。`margins` からの移行先** |
| **afex / ez** | 現役 | 心理実験の要因分散分析 | 目的が異なる（ANOVA 系）。競合しない |

**差別化のポジショニング**:
> 「ビネット（factorial survey）実験に特化し、**実験計画の作成 → 調査票への書き出し → 評定データの推定 → 選好の異質性 → 限界支払意思額 → 政策シナリオのシミュレーション** までを一貫した API で提供する唯一のパッケージ」

`cregg` は選択型の分析のみ、`idefix` は選択型の設計のみ、`AlgDesign` は汎用の計画のみ。**設計と分析を同じ語彙（属性・水準・ビネット・ブロック・回答者）で繋ぐ**ことが本パッケージの価値になります。

---

## 6. 追加すべき機能の設計

### 6.0 全体アーキテクチャ — 「統一 `fs_fit()` + S3 メソッド群」

現状のように機能ごとに独立した関数を増やしていくと、post-estimation・可視化・予測をモデルの種類ごとに書き直す羽目になります。**推定の返り値を単一の S3 クラスに揃える**のが設計上いちばん効きます。

```r
fs_fit(formula, data,
       family   = c("ordbeta", "beta", "gaussian", "ordinal"),
       random   = NULL,          # ~ 1 | id  /  ~ attr1 + attr2 | id
       mixture  = NULL,          # k = 1:5 で潜在クラス
       scale    = c(0, 10),      # 評定尺度の範囲
       cluster  = NULL,          # クラスター頑健SE用の ID
       ...)
```

内部でバックエンドへディスパッチ:

| 指定 | バックエンド |
|---|---|
| `family="beta"`, `random=NULL` | `betareg::betareg()` |
| `family="ordbeta"` | `glmmTMB::glmmTMB(family = ordbeta())` |
| `random = ~ 1 \| id` | `glmmTMB` （切片のランダム効果） |
| `random = ~ attrs \| id` | `glmmTMB` （**ランダムパラメータ**） |
| `mixture = 2:5` | `betareg::betamix()` / `flexmix` |
| `family="gaussian"` | `stats::lm` / `lme4::lmer` |
| `family="ordinal"` | `ordinal::clmm` |

返り値はすべて `class = c("fs_fit", "fs_fit_<backend>")` とし、以下を共通で提供:

```
print()  summary()  plot()  predict()  coef()  vcov()  logLik()  nobs()
tidy()   glance()   augment()          # broom
fs_ame() fs_mwtp()  fs_contrast()  fs_gof()  fs_simulate()
```

`tidy.fs_fit()` と `glance.fs_fit()` を書いておくと、`modelsummary` / `texreg` / `broom` エコシステムに**自動的に乗れます**（論文の表作成が一気に楽になる、費用対効果が非常に高い実装です）。

---

### 6.1 D効率性にもとづくビネット組み合わせリストの作成

`fs_defficient()` を `fs_design()` に改名・拡張（旧名は非公開のまま廃止）。

```r
fs_design(
  factors,                       # named list of levels
  n_vignettes,
  n_blocks       = 1,
  model          = ~ .,          # ~ .^2 で交互作用も最適化
  restrictions   = NULL,         # 禁止組み合わせ: list(~ 築年数 == 1 & 状態 == "空き家")
  candidate      = NULL,         # 候補集合を直接与える（大規模設計用）
  criterion      = c("D", "A", "I"),
  blocking       = c("optimal", "random", "none"),  # optBlock or ランダム
  order_balance  = c("none", "williams", "random"), # 提示順の統制
  n_repeats      = 50,
  seed           = NULL,         # RNG 状態は on.exit で復元
  max_candidates = 1e5           # 候補集合の上限（超えたらサブサンプリング）
)
```

新規に追加する周辺関数:

| 関数 | 役割 |
|---|---|
| `fs_design_eval(design)` | **設計診断**: D/A/G効率、直交設計比の相対効率、水準バランス表、属性間相関行列、VIF、水準の出現回数 |
| `fs_design_compare(...)` | 複数の設計案を並べて効率を比較（ビネット数を変えたときの効率曲線） |
| `plot(design)` | 水準バランスの棒グラフ + 相関ヒートマップ |
| `fs_vignette_text(design, template)` | **テンプレートからビネット文章を自動生成**。`"この住宅は{{築年数}}で、{{立地}}にあります。年間費用は{{価格}}です。"` のような書式を展開 |
| `fs_export(design, format)` | **調査票プラットフォームへの書き出し**: Qualtrics(QSF/loop&merge CSV)、Google Forms、LimeSurvey、SurveyMonkey、素の CSV/xlsx |
| `fs_assign(design, n_respondents, method)` | ブロックの回答者への割付表を生成（バランス割付／ランダム割付） |
| `fs_power(design, effect_size, n_respondents, ...)` | **検定力分析**。「回答者N人 × 1人あたりMビネットで、この効果量を検出できるか」をシミュレーションで評価。**調査設計の実務でいちばん聞かれる問いで、他パッケージにほぼ無い差別化要素** |

> 実装メモ: `restrictions` は候補集合 `full_factorial` を `subset()` でフィルタしてから `optFederov()` に渡すだけで実現できます。ただし禁止制約が強いと D効率が落ちるので、`fs_design_eval()` で「制約なしの理論最大効率に対する相対効率」を報告すると親切です。

---

### 6.2 Post-estimation

**`marginaleffects` を基盤に据える**のが最善です。`margins` は2017年で更新が止まっており、`glmmTMB` や `betamix` には対応していません。`marginaleffects` は100超のモデルクラスに対応し、デルタ法・ブートストラップ・シミュレーションベースの推論を備えています。

| 関数 | 内容 |
|---|---|
| `fs_ame(fit, ...)` | 平均限界効果 / 水準ごとの AMCE 相当量。**評定尺度の元単位（0〜10点）へのリスケールを自動で行う**（現行の rescale ロジックを一般化） |
| `fs_contrast(fit, variable, ...)` | 水準間の対比。多重比較補正（Holm, BH）付き |
| `fs_mwtp(fit, ...)` | 全属性のMWTPを一括算出。`method = c("delta","krinsky-robb","fieller","bootstrap")` |
| `fs_gof(fit)` | logLik, AIC, BIC, **複数定義の擬似R²を明示的にラベル分けして**併記、ICC（マルチレベル時）、RMSE |
| `fs_vcov(fit, type)` | **回答者クラスター頑健SE**（`sandwich::vcovCL`）。1人が複数ビネットに回答する構造上、単純SEは過小推定になるため**必須** |
| `fs_check(fit)` | 残差診断（`DHARMa` のシミュレーション残差）、外れ値・影響度、収束診断 |
| `tidy()` / `glance()` / `augment()` | broom メソッド。`modelsummary` 連携 |

> **統計的に重要な指摘**: 現行の `fs_betareg` は回答者クラスタリングを一切考慮していません。ビネット実験は1人がM個のビネットに回答するパネル構造なので、**独立性の仮定が破れておりSEが過小になります**。`fs_vcov()` によるクラスター頑健SE、またはマルチレベルモデル（§6.4）のどちらかが事実上必須です。これは v0.4 までに必ず入れるべき項目です。

---

### 6.3 分析結果の視覚化

`ggplot2` ベース。`cregg` の forest plot が事実上の標準なので、そこに合わせつつビネット実験固有の図を足します。

| 関数 / メソッド | 図 |
|---|---|
| `plot(design)` | 水準バランス、属性間相関ヒートマップ、D効率の探索履歴 |
| `plot(fs_ame)` / `fs_plot_ame()` | **AME/AMCE の forest plot**（属性ごとにパネル分割、参照水準を明示、95%CI） |
| `plot(fs_mwtp)` | MWTP の forest plot（信頼区間の算出法をキャプションに表示） |
| `fs_plot_predict(fit, scenarios)` | シナリオ別の予測評定値（点＋区間、ドットプロット） |
| `fs_plot_interaction(fit, x, by)` | 属性 × 回答者共変量の交互作用（条件付き効果曲線） |
| `fs_plot_ranef(fit)` | **caterpillar plot**（回答者別ランダム効果を順位付きで） |
| `fs_plot_heterogeneity(fit)` | ランダム係数の**個人別事後推定値の分布**（属性ごとのリッジプロット／ヒストグラム）。「価格に敏感な人が何割いるか」が一目でわかる図 |
| `fs_plot_classes(fit)` | **潜在クラス別のプロファイル図**（クラス×属性の効果を並べる）＋クラス規模の帯グラフ |
| `fs_plot_class_probs(fit)` | 事後所属確率の分布（クラス分離の良さの診断） |
| `fs_plot_ic(fit_list)` | クラス数選択のための AIC/BIC/ICL 比較図 |
| `theme_fs()` | 論文投稿に耐える統一テーマ（白黒印刷でも読める配色） |

> 設計上の注意: すべての `fs_plot_*()` は **`ggplot` オブジェクトを返す**こと（描画のみで返り値なしにしない）。ユーザーが `+ labs()` などで加工できることが R パッケージとしての作法です。また `plot()` メソッド内で `par()` を触らないこと（CRAN Policy）。

---

### 6.4 回答者レベルでの異質性（3本立て）

#### (a) マルチレベル（混合効果）モデル — 標準装備にすべき既定路線

```r
fs_fit(rating ~ attr1 + attr2 + price,
       data   = d,
       family = "ordbeta",
       random = ~ 1 | respondent_id)          # ランダム切片
```

- バックエンド: `glmmTMB`（`beta_family()` / `ordbeta()` / gaussian をすべて扱える）
- 出力: 分散成分、**ICC（回答者間分散の割合）**、ランダム効果の BLUP、caterpillar plot
- 回答者共変量とのクロスレベル交互作用: `rating ~ attr1 * age_group + ...`

#### (b) ランダムパラメータ（連続分布）

```r
fs_fit(rating ~ attr1 + attr2 + price,
       data   = d,
       family = "ordbeta",
       random = ~ attr1 + attr2 + price | respondent_id)   # ランダム係数
```

- 出力: 係数の平均と標準偏差、係数間の相関行列
- **個人別条件付き推定値**（`fs_ranef(fit, conditional = TRUE)`）→ そこから**個人別 MWTP の分布**を算出（`fs_mwtp(fit, level = "individual")`）
- 「WTP空間での推定」（価格係数で基準化してから推定）を将来オプションとして検討。WTP の分布が裾で発散する問題を回避できます

#### (c) 有限混合モデル（潜在クラス）

```r
fs_mixture(rating ~ attr1 + attr2 + price,
           data = d,
           k    = 1:5,                 # クラス数を探索
           concomitant = ~ age + income)   # クラス所属の説明変数
```

- バックエンド: **`betareg::betamix()`**（`flexmix` ベース。ベータ回帰の有限混合をそのまま提供しており、実装コストが最小）
- 出力:
  - クラス数選択の比較表（logLik, AIC, BIC, **ICL**, エントロピー R²）
  - クラス別の係数表とクラス規模
  - **事後所属確率**（回答者 × クラス）と modal assignment
  - **クラス別 MWTP**
  - concomitant variable model による「どんな属性の人がどのクラスに入りやすいか」
- 実装上の注意: EM は**局所解に落ちる**ため、複数初期値からの再スタート（`nrep`）を既定で複数回にし、収束の安定性を報告すること

#### (d) 共変量による調整（補助的だが実務価値が高い）

`fs_ame(fit, by = "age_group")` でサブグループ別の平均限界効果を出す。`marginaleffects` の `by=` 引数にそのまま乗るので実装は軽量です。

---

### 6.5 推定結果にもとづく予測・シミュレーション

| 関数 | 内容 |
|---|---|
| `predict(fit, newdata, type, re.form)` | `type = c("response", "link", "scale")`。**`"scale"` は元の評定尺度（0〜10点）へ戻した予測値**。`re.form = NA` で母集団平均、`NULL` で回答者別 |
| `fs_scenario(fit, ...)` | 属性の組み合わせを指定して予測評定値・その区間を返す。`fs_scenario(fit, 立地 = "駅近", 価格 = c(1000, 3000, 5000))` のような書き方 |
| `fs_simulate(fit, n_sim = 1000, ...)` | **パラメータ不確実性を含むモンテカルロ**。係数を推定分布（MVN）からドローし、予測分布を得る。ランダムパラメータ／潜在クラスモデルでは個人の異質性も同時に反映 |
| `fs_welfare(fit, from, to)` | **政策シナリオ間の厚生変化を WTP 換算**。「現状 → 改善案」で1人あたり年間いくらの便益か、母集団に外挿していくらか。**環境経済・防災投資の費用便益分析に直結** |
| `fs_share(fit, alternatives)` | 複数のビネットを選択肢とみなしたときの選好シェア（評定値 → 順位 → シェア）。市場シミュレーション的な使い方 |
| `fs_optimize(fit, constraints)` | **制約下での最適属性組み合わせ探索**。「年間費用3,000円以内で評定値が最大になる機能の組み合わせは？」を全探索 or 貪欲法で解く |
| `fs_sensitivity(fit, variable)` | 1属性を動かしたときの予測値の感度曲線（tornado plot 付き） |

> `fs_welfare()` と `fs_optimize()` は他パッケージに無く、**防災サービスの機能選択や環境政策の便益評価**という実際の研究課題に直結します。パッケージの「売り」になる部分なので、vignette でも1本立てて丁寧に説明する価値があります。

---

## 7. 命名規約と API の一貫性（今のうちに決めておくべきこと）

CRAN 公開後は関数名の変更が非常に面倒になります。**v1.0 の前に以下を固定してください。**

```
接頭辞:  すべて fs_
設計:    fs_design(), fs_design_eval(), fs_vignette_text(), fs_export(),
         fs_assign(), fs_power()
推定:    fs_fit(), fs_mixture()
事後:    fs_ame(), fs_contrast(), fs_mwtp(), fs_gof(), fs_vcov(),
         fs_ranef(), fs_check()
予測:    predict(), fs_scenario(), fs_simulate(), fs_welfare(),
         fs_share(), fs_optimize(), fs_sensitivity()
可視化:  plot() メソッド優先 + fs_plot_*() 、theme_fs()
補助:    set_labels()   ← set_lavels からリネーム

S3クラス: fs_design / fs_fit / fs_ame / fs_mwtp / fs_sim / fs_mixture
引数名の統一: data, formula, verbose, seed, level(信頼水準), n_sim, method
```

**引数の順序も統一**してください（`formula, data, ...` の順。R の慣例）。現行の `fs_mwtp(model_results, variable_name, price_variable_name)` は `fs_mwtp(fit, variable, price)` に短縮するのが R らしい命名です（`_name` サフィックスは冗長）。

---

## 8. 開発インフラの整備

| 項目 | 内容 |
|---|---|
| **DESCRIPTION** | 下記の雛形に差し替え |
| **README.md** | `usethis::use_readme_rmd()`。バッジ、インストール方法、5行で動く最小例、引用情報 |
| **NEWS.md** | `usethis::use_news_md()`。CRAN 提出時のレビューでも見られます |
| **vignette 3本** | ①`design-a-factorial-survey` ②`analyze-vignette-ratings` ③`heterogeneity-and-wtp`。`usethis::use_vignette()` |
| **サンプルデータ** | `data/` に**実データ由来の匿名化データセット1本**（例: 防災アプリのビネット調査）＋シミュレーションデータ生成関数 `fs_simulate_data()`。CRAN レビュアーは実行可能な例を重視します。`R/data.R` に roxygen ドキュメントを書き、`LazyData: true` |
| **テスト** | `test-fs_defficient.R` を作り直し、カバレッジ目標70%以上。`covr` + Codecov。乱数を使うテストは `withr::with_seed()` で固定。`skip_if_not_installed()` で Suggests 依存のテストを保護 |
| **CI** | `usethis::use_github_action("check-standard")`（Windows/macOS/Ubuntu × release/devel/oldrel）。`use_github_action("test-coverage")`、`use_github_action("pkgdown")` |
| **pkgdown** | `usethis::use_pkgdown_github_pages()`。関数リファレンスを設計/推定/事後/予測/可視化のグループに整理 |
| **spell check** | `usethis::use_spell_check()`（CRAN の初回提出で綴りは高確率で指摘されます） |
| **引用** | `usethis::use_citation()` + `CITATION.cff`。将来 JOSS / R Journal / JSS 投稿を視野に |
| **ORCID** | `Authors@R` の `comment = c(ORCID = "...")` |

### DESCRIPTION の雛形

```
Package: FactorialSurvey
Type: Package
Title: Design and Analysis of Factorial Survey (Vignette) Experiments
Version: 0.1.0
Authors@R: c(
    person("Katsuya", "Tanaka", , "katsuya.dm@gmail.com",
           role = c("aut", "cre"),
           comment = c(ORCID = "0000-0000-0000-0000")))
Description: Provides an integrated workflow for factorial survey (vignette)
    experiments: constructing D-efficient vignette designs with optional
    restrictions and blocking, exporting them to survey platforms, estimating
    rating outcomes with beta, ordered beta and mixed-effects models,
    accommodating respondent-level preference heterogeneity through
    multilevel, random-parameter and finite-mixture specifications, computing
    marginal effects and marginal willingness to pay, and simulating policy
    scenarios. Methods for efficient designs follow Federov (1972,
    ISBN:0122507509); beta regression follows Ferrari and Cribari-Neto (2004)
    <doi:10.1080/0266476042000214501>; the ordered beta model follows
    Kubinec (2023) <doi:10.1017/pan.2022.20>.
License: MIT + file LICENSE
Encoding: UTF-8
LazyData: true
Depends: R (>= 4.1)
Imports:
    stats,
    utils,
    AlgDesign,
    betareg,
    ggplot2
Suggests:
    glmmTMB,
    flexmix,
    marginaleffects,
    sandwich,
    ordinal,
    lme4,
    DHARMa,
    testthat (>= 3.0.0),
    withr,
    knitr,
    rmarkdown,
    vdiffr,
    covr
VignetteBuilder: knitr
Config/testthat/edition: 3
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.3.2
URL: https://github.com/tanakak/FactorialSurvey,
    https://tanakak.github.io/FactorialSurvey/
BugReports: https://github.com/tanakak/FactorialSurvey/issues
```

> `Description` の書き方は CRAN のチェックが厳しい部分です: ①完全な文にする ②パッケージ名で始めない（"This package..." も禁止） ③ソフトウェア名は 'AlgDesign' のように**シングルクォートで囲む** ④文献は `Author (Year) <doi:...>` の形式（DOI にリンクを付けない）。上記雛形はこの規則に沿っています。ただし DOI は投稿前に実際に有効か必ず確認してください。

---

## 9. CRAN 提出チェックリスト

### コード側
- [ ] `R CMD check --as-cran` が **0 ERROR / 0 WARNING / NOTE は "New submission" のみ**
- [ ] `_R_CHECK_DEPENDS_ONLY_=true` でも check が通る（Suggests が無い環境で壊れない）
- [ ] すべての export 関数に `@title` `@description` `@param` `@return` `@examples`
- [ ] **`@return` が全関数にある**（CRAN が最近特に厳しくチェックする項目）
- [ ] 例は `\dontrun{}` ではなく `\donttest{}` を使う。1例あたり5秒以内
- [ ] **check 全体が10分以内**（重い推定は `\donttest{}` へ）
- [ ] `set.seed()` / `options()` / `par()` / `setwd()` でグローバル状態を変更しない（変更する場合は `on.exit()` で復元）
- [ ] **ファイル書き込みは `tempdir()` のみ**（`fs_design(export_csv=TRUE)` の例は必ず `tempdir()` に書く）
- [ ] `cat()` / `print()` を無条件に呼ばない。`verbose` 引数か `message()` に
- [ ] R コード・Rd ファイルに非 ASCII 文字を含めない
- [ ] Suggests のパッケージは `requireNamespace(..., quietly = TRUE)` で保護
- [ ] `.Rbuildignore` に開発用ファイルをすべて登録
- [ ] `git` 空ファイル、`fs_betaregのコピー.R` を削除

### 提出プロセス
- [ ] `devtools::check_win_devel()` と `check_win_release()`
- [ ] `rhub::rhub_check()` で macOS / Linux / Windows
- [ ] `urlchecker::url_check()` で URL と DOI の有効性
- [ ] `devtools::spell_check()`
- [ ] `cran-comments.md` を作成（テスト環境一覧、NOTE への説明）
- [ ] `usethis::use_release_issue()` でリリース手順をチェックリスト化
- [ ] `devtools::release()` で提出

> **心構え**: 初回提出は9割方リジェクトされます。返ってくる指摘は「Description の書式」「`@return` 欠落」「`\dontrun{}` の使用」「グローバル状態の変更」がほぼ定番です。**上のチェックリストを潰しておけば1〜2往復で通ります。** 提出から返答までは通常1〜10日です。

---

## 10. マイルストーン

| バージョン | 内容 | 目安 |
|---|---|---|
| **v0.1.0** | **基盤の立て直し**。ローカル復旧、B1〜B11 の解消、`set_labels` へ改名、テスト作り直し、`R CMD check` クリーン、GHA、README、pkgdown | 2〜3週 |
| **v0.2.0** | **設計機能**。`fs_design()` に restrictions / 交互作用 / optBlock / Williams design、`fs_design_eval()`、`fs_vignette_text()`、`fs_export()`、`plot.fs_design()` | 3〜4週 |
| **v0.3.0** | **推定の統一**。`fs_fit()` と `fs_fit` クラス、ordbeta 対応、尺度の一般化、クラスター頑健SE、broom メソッド。`fs_betareg()` のバグ修正 | 3〜4週 |
| **v0.4.0** | **Post-estimation と可視化**。`marginaleffects` への移行、`fs_ame()` / `fs_contrast()` / `fs_gof()`、`fs_mwtp()` を全属性一括＋Krinsky-Robb / Fieller 対応、forest plot 群 | 3〜4週 |
| **v0.5.0** | **異質性**。マルチレベル、ランダムパラメータ、個人別 MWTP、`fs_mixture()`（クラス数選択・事後確率・クラス別WTP）、caterpillar / class profile plot | 4〜5週 |
| **v0.6.0** | **予測とシミュレーション**。`predict()`、`fs_scenario()`、`fs_simulate()`、`fs_welfare()`、`fs_optimize()`、`fs_power()` | 3〜4週 |
| **v0.9.0** | **CRAN 準備**。vignette 3本、サンプルデータ、全関数の examples、カバレッジ70%、多プラットフォーム check、cran-comments | 3〜4週 |
| **v1.0.0** | **CRAN 提出**。同時に JOSS または R Journal への論文投稿を検討 | — |

合計の目安: **6〜7か月**（他の業務と並行する前提）。急ぐ場合は v0.2 と v0.4 を優先し、異質性（v0.5）を v1.1 に回して**最小構成で先に CRAN に載せる**戦略も有効です。CRAN に一度載せてしまえば、その後の機能追加は更新提出だけで済みます。

---

## 11. 今すぐ着手できる3つのこと

1. **ローカルフォルダの復旧**（§1 のコマンド）— 5分。放置すると誤って壊れたファイルを push するリスクがあります
2. **`R/fs_betaregのコピー.R` の削除と `test-fs_defficient.R` の書き直し** — この2つを直すだけで `R CMD check` の ERROR が消えます
3. **DESCRIPTION の差し替え**（§8 の雛形）— テンプレート文言が残っているとレビューに出せません

---

## 参考文献・情報源

- [CRAN Task View: Design of Experiments (DoE) & Analysis of Experimental Data](https://cran.r-project.org/web/views/ExperimentalDesign.html)
- [factorEx: Design and Analysis for Factorial Experiments（CRAN から削除済み）](https://cran.r-project.org/web/packages/factorEx/index.html)
- [cregg: Simple Conjoint Tidying, Analysis, and Visualization](https://thomasleeper.com/cregg/)
- [idefix: Efficient Designs for Discrete Choice Experiments](https://cran.r-project.org/web/packages/idefix/index.html)
- [Traets, Gil Sanchez & Vandebroek (2020) "Generating Optimal Designs for Discrete Choice Experiments in R: The idefix Package", JSS](https://www.jstatsoft.org/article/view/v096i03)
- [betareg: betamix — Finite Mixtures of Beta Regression](https://search.r-project.org/CRAN/refmans/betareg/html/betamix.html)
- [betareg: betareg — 返り値の構成要素](https://search.r-project.org/CRAN/refmans/betareg/html/betareg.html)
- [glmmTMB: Family functions（`ordbeta`, `beta_family`）](https://glmmtmb.github.io/glmmTMB/reference/nbinom2.html)
- [ordbetareg: Ordered Beta Regression Models with 'brms'](https://cran.r-project.org/web/packages/ordbetareg/index.html)
- [marginaleffects: Predictions, Comparisons, Slopes, Marginal Means, and Hypothesis Tests](https://cran.r-project.org/package=marginaleffects)
- [margins: Marginal Effects for Model Objects（最終更新 2017-03-22）](https://cran.r-project.org/web/packages/margins/index.html)
