# Synotra

[![coming soon](https://img.shields.io/badge/status-coming%20soon-orange)](https://github.com/yourusername/synotra)

**Synotra** is a new programming language designed to simplify distributed computing, created for developers who find MPI or Kubernetes too complex.  

---

## 🎯 Goal

- Run your code across multiple processes **without complex cluster setup**  
- Designed with a Rust/Kotlin-like syntax for readability  
- Built-in language support for distributed and cooperative tasks, making it safe and efficient on supercomputers or multi-node environments  

---

## 💡 Features

- **Simple distributed syntax**
  ```sy
  task hello() distributed {
      print("Hello from node ${Node.id}")
  }

# 日本語版

**Synotra** は、MPI や Kubernetes の複雑さに悩むエンジニアのために開発されている、新しい分散処理対応のプログラミング言語です。  

---

## 🎯 目的

- 複雑なクラスタ構築や分散処理の設定なしで、**あなたのコードをマルチプロセスで動かす**ことが可能  
- Swift や Kotlin のような書きやすい構文をベースに設計  
- 分散・協調処理を言語仕様でサポートし、スパコンや複数ノードの環境でも安全かつ効率的に動作  

---

## 💡 特徴

- **シンプルな分散構文**  
  ```sy
  task hello() distributed {
      print("Hello from node ${Node.id}")
  }