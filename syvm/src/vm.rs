use rayon::prelude::*;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::sync::mpsc::channel;

/// VM 上のタスク構造
/// 
/// ジェネリック型 T を使用して、任意の戻り値型を持つタスクを表現できます。
/// タスクは依存関係を持ち、依存するタスクが完了してから実行されます。
#[derive(Clone)]
pub struct Task<T> 
where
    T: Send + Sync + Clone,
{
    pub id: usize,
    pub depends_on: Vec<usize>,
    func: Arc<dyn Fn() -> T + Send + Sync>,
}

impl<T> Task<T>
where
    T: Send + Sync + Clone,
{
    /// 新しいタスクを作成
    pub fn new<F>(id: usize, depends_on: Vec<usize>, func: F) -> Self
    where
        F: Fn() -> T + Send + Sync + 'static,
    {
        Task {
            id,
            depends_on,
            func: Arc::new(func),
        }
    }

    /// タスクを実行
    pub fn execute(&self) -> T {
        (self.func)()
    }
}

/// タスクを並列実行するための仮想マシン
/// 
/// タスク間の依存関係を解決し、可能な限り並列で実行します。
pub struct VM<T>
where
    T: Send + Sync + Clone + std::fmt::Debug + 'static,
{
    tasks: Vec<Task<T>>,
}

impl<T> VM<T>
where
    T: Send + Sync + Clone + std::fmt::Debug + 'static,
{
    /// 新しい VM インスタンスを作成
    pub fn new() -> Self {
        VM { tasks: vec![] }
    }

    /// タスクを VM に追加
    pub fn add_task<F>(&mut self, id: usize, depends_on: Vec<usize>, func: F)
    where
        F: Fn() -> T + Send + Sync + 'static,
    {
        self.tasks.push(Task::new(id, depends_on, func));
    }

    /// タスクを並列実行
    /// 
    /// 依存関係を解決しながら、実行可能なタスクを並列で実行します。
    /// 結果は HashMap<タスクID, 結果> の形式で返されます。
    pub fn run_parallel(&self) -> HashMap<usize, T> {
        let results: Arc<Mutex<HashMap<usize, T>>> = Arc::new(Mutex::new(HashMap::new()));
        let mut pending: HashMap<usize, Task<T>> = 
            self.tasks.iter().map(|t| (t.id, t.clone())).collect();

        let (tx, rx) = channel::<usize>();

        while !pending.is_empty() {
            // 実行可能なタスクを探す（依存関係がすべて満たされているタスク）
            let ready: Vec<Task<T>> = pending
                .values()
                .filter(|t| t.depends_on.iter().all(|d| results.lock().unwrap().contains_key(d)))
                .cloned()
                .collect();

            if ready.is_empty() {
                // 実行可能なタスクがない場合は、完了通知を待つ
                let finished_id = rx.recv().unwrap();
                pending.remove(&finished_id);
                continue;
            }

            // 実行済みタスクの ID を保存（ready がムーブされる前に）
            let ready_ids: Vec<usize> = ready.iter().map(|t| t.id).collect();

            // 並列で実行
            ready.into_par_iter().for_each(|t| {
                let res = t.execute();
                results.lock().unwrap().insert(t.id, res);
                tx.send(t.id).unwrap();
            });

            // 実行済みタスクを pending から削除
            for id in ready_ids {
                pending.remove(&id);
            }
        }

        Arc::try_unwrap(results).unwrap().into_inner().unwrap()
    }

    /// タスクをシングルスレッドで実行
    /// 
    /// 依存関係を解決しながら、順次実行します。
    /// デバッグやベンチマーク用途で使用できます。
    pub fn run_single(&self) -> HashMap<usize, T> {
        let mut results = HashMap::new();
        let mut pending: HashMap<usize, Task<T>> = 
            self.tasks.iter().map(|t| (t.id, t.clone())).collect();

        while !pending.is_empty() {
            let ready: Vec<Task<T>> = pending
                .values()
                .filter(|t| t.depends_on.iter().all(|d| results.contains_key(d)))
                .cloned()
                .collect();

            if ready.is_empty() {
                panic!("循環依存があります！タスクの依存関係を確認してください。");
            }

            for t in ready {
                let res = t.execute();
                results.insert(t.id, res);
                pending.remove(&t.id);
            }
        }

        results
    }
}

impl<T> Default for VM<T>
where
    T: Send + Sync + Clone + std::fmt::Debug + 'static,
{
    fn default() -> Self {
        Self::new()
    }
}
