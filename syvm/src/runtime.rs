use crate::bytecode::*;
use crate::actor::{Actor, Message};
use std::collections::HashMap;
use std::sync::mpsc;
use std::thread::JoinHandle;

use std::sync::{Arc, RwLock, Mutex, Condvar};
use std::sync::atomic::{AtomicUsize, Ordering};

pub struct Runtime {
    actors: HashMap<String, IrActor>,
    // Shared router for actor lookups
    router: Arc<RwLock<HashMap<String, mpsc::Sender<Message>>>>,
    // Thread handles for joining
    handles: Vec<JoinHandle<()>>,
    // Shutdown signaling
    shutdown_tx: mpsc::Sender<()>,
    shutdown_rx: mpsc::Receiver<()>,
    // Busy count for implicit termination
    busy_count: Arc<AtomicUsize>,
    completion_cond: Arc<(Mutex<bool>, Condvar)>,
}

impl Runtime {
    pub fn new(program: IrProgram) -> Self {
        let mut actors = HashMap::new();
        for actor in program.actors {
            actors.insert(actor.name.clone(), actor);
        }
        let (shutdown_tx, shutdown_rx) = mpsc::channel();
        Runtime {
            actors,
            router: Arc::new(RwLock::new(HashMap::new())),
            handles: Vec::new(),
            shutdown_tx,
            shutdown_rx,
            busy_count: Arc::new(AtomicUsize::new(0)),
            completion_cond: Arc::new((Mutex::new(false), Condvar::new())),
        }
    }

    pub fn spawn_all(&mut self) {
        // Collect all actor names first to avoid borrow checker issues
        let actor_names: Vec<String> = self.actors.keys().cloned().collect();
        
        for name in actor_names {
            if let Some(def) = self.actors.get(&name) {
                self.spawn(def.clone(), HashMap::new());
            }
        }
    }

    pub fn spawn(&mut self, definition: IrActor, initial_state: HashMap<String, Value>) {
        let (tx, rx) = mpsc::channel();
        let name = definition.name.clone();
        
        {
            let mut router = self.router.write().unwrap();
            router.insert(name.clone(), tx);
        }

        let router_clone = self.router.clone();
        let shutdown_signal = self.shutdown_tx.clone();
        let busy_count = self.busy_count.clone();
        let completion_cond = self.completion_cond.clone();
        
        let mut actor = Actor::new(definition, rx, router_clone, shutdown_signal, busy_count, completion_cond);
        actor.state = initial_state;

        // Spawn on OS thread and save the handle
        let handle = std::thread::spawn(move || {
            actor.run();
        });
        
        self.handles.push(handle);
    }

    pub fn send(&self, target: &str, msg: Message) {
        let router = self.router.read().unwrap();
        if let Some(tx) = router.get(target) {
            // Increment busy count
            self.busy_count.fetch_add(1, Ordering::SeqCst);
            
            // Unbounded send is non-blocking
            let _ = tx.send(msg);
        } else {
            println!("Runtime: Actor {} not found", target);
        }
    }

    pub fn shutdown(&mut self) {
        // Clear the router to close all channels, causing actors to exit their loops
        let mut router = self.router.write().unwrap();
        router.clear();
    }

    pub fn wait_for_exit(&self) {
        // Block until any actor calls exit()
        let _ = self.shutdown_rx.recv();
    }

    pub fn wait_for_completion(&mut self) {
        // Join all actor threads
        while let Some(handle) = self.handles.pop() {
            let _ = handle.join();
        }
    }

    pub fn actor_count(&self) -> usize {
        self.router.read().unwrap().len()
    }

    pub fn wait_for_idle(&self) {
        let (lock, cvar) = &*self.completion_cond;
        let mut finished = lock.lock().unwrap();
        while !*finished {
            finished = cvar.wait(finished).unwrap();
        }
    }
}
