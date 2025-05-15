#[cfg(test)]
mod tests {

    use crate::core::execution_machine::KernelExecutor;

    #[test]

    fn test_adapter_info() {
        let adapter = pollster::block_on(KernelExecutor::get_adapter_info());
        println!("{}", adapter);
    }
}