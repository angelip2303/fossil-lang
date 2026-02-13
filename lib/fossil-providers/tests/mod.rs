use crate::common::error::TestSuiteError;
use crate::common::test::*;

mod common;

// --- Implemented tests ---

#[test]
pub fn rmltc0000_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0000-CSV")
}

#[test]
pub fn rmltc0001a_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0001a-CSV")
}

#[test]
pub fn rmltc0001b_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0001b-CSV")
}

#[test]
pub fn rmltc0002a_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0002a-CSV")
}

#[test]
pub fn rmltc0002b_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0002b-CSV")
}

#[test]
pub fn rmltc0002c_csv() -> Result<(), TestSuiteError> {
    test_negative("RMLTC0002c-CSV")
}

#[test]
pub fn rmltc0002e_csv() -> Result<(), TestSuiteError> {
    test_negative("RMLTC0002e-CSV")
}

#[test]
pub fn rmltc0003c_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0003c-CSV")
}

#[test]
pub fn rmltc0004a_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0004a-CSV")
}

#[test]
pub fn rmltc0004b_csv() -> Result<(), TestSuiteError> {
    test_negative("RMLTC0004b-CSV")
}

#[test]
pub fn rmltc0005a_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0005a-CSV")
}

// --- Not yet implemented (mapping.fossil empty) ---

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0006a_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0006a-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0007a_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0007a-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0007c_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0007c-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0007d_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0007d-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0007e_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0007e-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0007f_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0007f-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0007g_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0007g-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0007h_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0007h-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0008a_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0008a-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0008b_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0008b-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0008c_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0008c-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0009a_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0009a-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0009b_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0009b-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0010a_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0010a-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0010c_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0010c-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0011b_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0011b-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0012a_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0012a-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0012b_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0012b-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0012c_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0012c-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0012d_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0012d-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0015a_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0015a-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0015b_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0015b-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0019a_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0019a-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0019b_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0019b-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0020a_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0020a-CSV")
}

#[test]
#[ignore = "mapping not yet implemented"]
pub fn rmltc0020b_csv() -> Result<(), TestSuiteError> {
    test_positive("RMLTC0020b-CSV")
}
