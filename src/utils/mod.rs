use email_address::EmailAddress;
use lazy_static::lazy_static;
use regex::Regex;
use url::Url;

pub(crate) mod cursor;
pub(crate) mod stack;

// This regex is used to match a string with double quotes("") or single quotes('')
lazy_static! {
    static ref D_QUOTED_STRING_RE: Regex = Regex::new("^\"([^\"\\\\]|\\\\.)*\"$").unwrap();
    static ref S_QUOTED_STRING_RE: Regex = Regex::new("^\'([^\'\\\\]|\\\\.)*\'$").unwrap();
}

pub fn is_quoted_string(s: &str) -> bool {
    D_QUOTED_STRING_RE.is_match(s) || S_QUOTED_STRING_RE.is_match(s)
}

pub fn is_url(s: &str) -> bool {
    Url::try_from(s).is_ok()
}

pub fn is_email(s: &str) -> bool {
    EmailAddress::is_valid(s)
}
