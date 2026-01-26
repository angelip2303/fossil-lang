use crate::common::error::TestSuiteError;
use crate::common::test::{test_negative, test_positive};

mod common;

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
