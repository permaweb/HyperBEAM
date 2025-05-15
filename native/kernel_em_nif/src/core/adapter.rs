use serde::{Deserialize, Serialize};
use wgpu::AdapterInfo;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct AdapterInfoRes {
    // todo: add device_type
    pub name: String,
    pub vendor: u32,
    pub device: u32,
    pub driver: String,
    pub backend: String,
    pub driver_info: String,
}

impl AdapterInfoRes {
    pub fn from(adapter: AdapterInfo) -> Self {
        Self {
            name: adapter.name,
            vendor: adapter.vendor,
            device: adapter.device,
            driver: adapter.driver,
            driver_info: adapter.driver_info,
            backend: adapter.backend.to_string(),
        }
    }
}