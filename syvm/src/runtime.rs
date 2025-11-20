use crate::bytecode::*;
use crate::actor::{Actor, Message};
use std::collections::HashMap;
use tokio::sync::mpsc;

pub struct Runtime {
    senders: HashMap<String, mpsc::Sender<Message>>,
}

impl Runtime {
    pub fn new() -> Self {
        Runtime {
            senders: HashMap::new(),
        }
    }

    pub fn spawn(&mut self, definition: IrActor, initial_state: HashMap<String, Value>) {
        let (tx, rx) = mpsc::channel(32);
        let name = definition.name.clone();
        self.senders.insert(name.clone(), tx);

        let mut actor = Actor::new(definition, rx);
        actor.state = initial_state;

        // Spawn on blocking thread pool for true parallelism
        tokio::task::spawn_blocking(move || {
            let rt = tokio::runtime::Handle::current();
            rt.block_on(async move {
                actor.run().await;
            });
        });
    }

    pub fn spawn_all(&mut self, actors: Vec<IrActor>, state_fn: impl Fn(&str) -> HashMap<String, Value>) {
        for actor_def in actors {
            let name = actor_def.name.clone();
            let initial_state = state_fn(&name);
            self.spawn(actor_def, initial_state);
        }
    }

    pub async fn send(&self, target: &str, msg: Message) {
        if let Some(tx) = self.senders.get(target) {
            tx.send(msg).await.unwrap();
        } else {
            println!("Runtime: Target actor {} not found", target);
        }
    }

    pub fn actor_count(&self) -> usize {
        self.senders.len()
    }
}
