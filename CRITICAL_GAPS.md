# Synotra実用化に向けて致命的な欠陥

## 🔴 **致命的レベル（これがないと実用不可）**

### 1. エラーハンドリング機構が完全に欠如
**現状:**
- try/catch構文なし
- Result型/Option型なし
- エラーの伝播・処理方法が存在しない

**問題:**
```synotra
// ファイルが存在しない場合どうする？
io fun readFile(path: String) {
    // エラーが起きたらプログラムがクラッシュするしかない
}

// ゼロ除算は？
fun divide(a: Int, b: Int): Int {
    return a / b  // b=0の場合、どう処理する？
}
```

**必要な機能:**
- `Result<T, E>`型または`Option<T>`型
- パターンマッチング（後述）
- または`try/catch`構文

---

### 2. パターンマッチング/match式がない
**現状:**
- データメッセージを受け取っても、型による分岐ができない
- 複数のメッセージタイプを処理できない

**問題:**
```synotra
data message TaskAdded(val id: Int)
data message TaskCompleted(val id: Int)
data message TaskFailed(val id: Int, val error: String)

actor Worker(id: Int) {
    // 現在はメッセージタイプごとに別の関数が必要
    io fun receive(msg: TaskAdded) { ... }
    // しかし、TaskCompleted も処理したい場合は？
    // → 不可能！
}
```

**必要な機能:**
```synotra
// 望ましい構文（未実装）
actor Worker(id: Int) {
    io fun receive(msg: Message) {
        match msg {
            TaskAdded(id) => { ... }
            TaskCompleted(id) => { ... }
            TaskFailed(id, error) => { ... }
        }
    }
}
```

---

### 3. 基本的なI/O APIが存在しない
**現状:**
- `println`のみ存在
- ファイル読み書きAPI: **なし**
- ネットワークI/O: **なし**
- 標準入力: **なし**

**問題:**
```synotra
// 以下のような基本的な処理ができない
io fun processFile() {
    // ❌ ファイルを読む方法がない
    // ❌ ファイルに書く方法がない
    // ❌ HTTPリクエストを送る方法がない
    
    println("これしかできない")
}
```

**必要な機能:**
- File I/O API (`readFile`, `writeFile`)
- Network I/O API (`httpGet`, `httpPost`)
- Standard input (`readLine`)

---

## 🟡 **重大レベル（実用性が著しく低下）**

### 4. コレクション操作が極めて限定的
**現状:**
- `List<T>`, `Map<K, V>`は型として存在
- しかし、操作メソッド（map, filter, reduce, forEach, etc.）が**不明**

**問題:**
```synotra
// リストを作れない
var numbers = ???  // List<Int>のリテラルがない

// 仮にリストがあっても...
// ❌ numbers.map(x => x * 2)
// ❌ numbers.filter(x => x > 5)
// ❌ numbers.length()
// ❌ numbers.push(10)
```

**必要な機能:**
- コレクションリテラル: `[1, 2, 3]`, `{a: 1, b: 2}`
- メソッドチェーン: `list.map(...).filter(...)`
- または関数型API: `map(list, func)`

---

### 5. ユーザー定義型（データメッセージ以外）が作れない
**現状:**
- `data message`のみ定義可能
- **struct/class/enum**が存在しない

**問題:**
```synotra
// ビジネスロジックで使う型が定義できない
// ❌ struct User { ... }
// ❌ enum Status { Active, Inactive, Suspended }
// ❌ class Account { ... }

// 全てdata messageにするしかない
data message User(val id: Int, val name: String)
// しかし、これは「メッセージ」であって「データ型」ではない
```

**必要な機能:**
- 構造体: `struct User { ... }`
- 列挙型: `enum Status { ... }`
- または既存の`data message`を汎用データ型として拡張

---

### 6. 型エイリアスとジェネリック型定義ができない
**現状:**
- ジェネリック型は**使用**できる（`List<Int>`）
- しかし**定義**ができない

**問題:**
```synotra
// 型エイリアスが作れない
// ❌ type UserId = Int
// ❌ type Result<T> = ...

// 自分自身のジェネリック型を定義できない
// ❌ data Either<L, R> = Left(L) | Right(R)
```

---

## 🟢 **改善推奨レベル（あると便利）**

### 7. 並行処理の制御機構
- タイムアウト設定（`ask`で永遠に待つ可能性）
- メッセージの優先度制御
- アクターのライフサイクル管理（停止、再起動）

### 8. デバッグ支援
- アサーション: `assert(x > 0)`
- デバッグ出力: `debug!("value: {}", x)`

### 9. テスト機構
- テストフレームワーク
- モックアクター

---

## 📊 優先度付きロードマップ

### Phase 1: 緊急（これがないと実用不可）
1. **エラーハンドリング**: Result/Option型 + パターンマッチング
2. **基本I/O API**: ファイル、ネットワーク、標準入力
3. **コレクションリテラルと基本操作**

### Phase 2: 重要（実用性向上）
4. **ユーザー定義型**: struct/enum
5. **モジュールシステム**: コードの再利用と組織化
6. **型エイリアスとジェネリック定義**

### Phase 3: 改善（品質向上）
7. **並行処理制御**
8. **デバッグ支援**
9. **テスト機構**

---

## 結論

**最も致命的な3つ:**
1. ✋ **エラーハンドリング機構の欠如** → どんなトリビアルなプログラムでもエラーは発生する
2. ✋ **パターンマッチングの欠如** → メッセージベースの設計なのに、複数メッセージタイプを処理できない
3. ✋ **基本I/O APIの欠如** → `println`以外何もできない

これら3つを解決しない限り、「Hello World」以上のプログラムを書くことは実質不可能です。
