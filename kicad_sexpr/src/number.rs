use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::satisfy,
    combinator::{opt, recognize},
    multi::many1,
    sequence::tuple,
    IResult,
};

fn is_digit(c: char) -> bool {
    c.is_ascii_digit()
}

fn parse_float(input: &str) -> IResult<&str, f32> {
    let (input, float) = recognize(tuple((
        opt(tag("-")),
        many1(satisfy(is_digit)),
        tag("."),
        many1(satisfy(is_digit)),
    )))(input)?;

    Ok((
        input,
        float
            .parse()
            .unwrap_or_else(|_| panic!("Failed to parse float {float}!")),
    ))
}

fn parse_int(input: &str) -> IResult<&str, f32> {
    let (input, int) = recognize(tuple((opt(tag("-")), many1(satisfy(is_digit)))))(input)?;

    Ok((
        input,
        int.parse()
            .unwrap_or_else(|_| panic!("Failed to parse int {int}!")),
    ))
}

pub fn parse_number(input: &str) -> IResult<&str, f32> {
    let (input, number) = alt((parse_float, parse_int))(input)?;

    Ok((input, number))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_number() {
        assert_eq!(parse_number("123"), Ok(("", 123.0)));
        assert_eq!(parse_number("-123"), Ok(("", -123.0)));
        assert_eq!(parse_number("123.456"), Ok(("", 123.456)));
        assert_eq!(parse_number("-123.456"), Ok(("", -123.456)));
    }
}
