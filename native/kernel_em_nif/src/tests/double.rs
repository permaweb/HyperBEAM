#[cfg(test)]
mod tests {

    use crate::core::arweave::retrieve_kernel_src;
    use crate::core::execution_machine::{KernelExecutor, KernelExecutorOpts};

    #[test]
    fn test_double_kernel() {
        let kernel_src =
            retrieve_kernel_src("btSvNclyu2me_zGh4X9ULVRZqwze9l2DpkcVHcLw9Eg").unwrap();
        let input_data = vec![1, 3, 5, 7];
        // println!("SOURCE CODE: {:?}", kernel_src);
        let kem = pollster::block_on(KernelExecutor::new(KernelExecutorOpts::default()));
        let result = kem.execute_kernel_default(&kernel_src, &input_data, Some(1));
        println!("{:?}", result);
    }
}
