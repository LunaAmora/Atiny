use std::fs::{self, DirEntry};

pub struct Test {
    pub directory: &'static str,
    pub run: fn(source: String) -> String,
}

pub fn split_name(file: &DirEntry) -> (String, String) {
    let name = file.file_name();
    let path = name.to_string_lossy();
    let mut path = path.split('.').collect::<Vec<_>>();

    let typ = path.pop().unwrap();
    (path.join("."), typ.to_string())
}

pub fn test_runner(tests: &[&Test]) {
    use std::panic;

    use rustc_test::{TestDesc, TestDescAndFn, TestName};

    let args = std::env::args().collect::<Vec<_>>();
    let parsed = rustc_test::test::parse_opts(&args);

    let mut opts = match parsed {
        Some(Ok(o)) => o,
        Some(Err(msg)) => panic!("{:?}", msg),
        None => return,
    };

    opts.verbose = false;

    let mut rendered: Vec<TestDescAndFn> = Vec::new();

    for test in tests {
        let function = test.run;
        let directory = fs::read_dir(test.directory).unwrap();

        for file in directory {
            if let Ok(file) = file {
                let (file_name, typ) = split_name(&file);

                if typ != "at" {
                    break;
                }

                if file.file_type().unwrap().is_file() {
                    rendered.push(TestDescAndFn {
                        desc: TestDesc {
                            name: TestName::DynTestName(file_name.clone()),
                            ignore: false,
                            should_panic: rustc_test::ShouldPanic::No,
                            allow_fail: true,
                        },
                        testfn: rustc_test::TestFn::DynTestFn(Box::new(move || {
                            println!("testing '{}'", file_name);

                            let content = fs::read_to_string(file.path()).unwrap();

                            let mut path = file.path();
                            path.pop();

                            let path = path.join(format!("{}.{}", file_name, "expect"));

                            let result = function(content);

                            if let Ok(expects) = fs::read_to_string(path.clone()) {
                                assert_eq!(expects, result)
                            } else {
                                fs::write(path, result).unwrap();
                            }
                        })),
                    })
                }
            } else {
                break;
            }
        }
    }

    match rustc_test::run_tests_console(&opts, rendered) {
        Ok(true) => {
            println!();
        }
        Ok(false) => panic!("some tests failed"),
        Err(e) => panic!("io error when running tests: {:?}", e),
    }
}

#[macro_export]
macro_rules! mk_test {
    ($directory:expr, $code:expr) => {
        #[test_case]
        const TEST: atiny_tests::Test = atiny_tests::Test {
            directory: $directory,
            run: $code,
        };
    };
}
