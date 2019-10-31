use std::string::ToString;

pub fn stringfy<T: ToString>(v: &Vec<T>, sep: &str) -> String {
    v.iter()
     .map(|t| t.to_string())
     .collect::<Vec<String>>()
     .join(sep)
}
