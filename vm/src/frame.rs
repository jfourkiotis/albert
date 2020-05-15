pub struct Frame {
    // closure index in
    // vm.closures
    pub ci: usize,
    // current instruction pointer
    pub ip: usize,
    // total number of instructions
    pub len: usize,
    // stack pointer BEFORE calling the function
    pub base_pointer: usize,
}
