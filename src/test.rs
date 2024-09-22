use crate::{match_pattern, Group};
use crate::{Alternation, Ast, CharacterSet, Class, Count, Pattern};

fn t(items: impl IntoIterator<Item = Ast>) -> Pattern {
    Pattern {
        start: false,
        end: false,
        items: items.into_iter().collect(),
    }
}

fn with_start(mut pattern: Pattern) -> Pattern {
    pattern.start = true;
    pattern
}

fn with_end(mut pattern: Pattern) -> Pattern {
    pattern.end = true;
    pattern
}

#[test]
fn test_parse() {
    assert_eq!(Pattern::parse(""), Ok(t([])));
    assert_eq!(Pattern::parse("a"), Ok(t([Ast::Literal(Count::One, 'a')])));
    assert_eq!(
        Pattern::parse("ab"),
        Ok(t([
            Ast::Literal(Count::One, 'a'),
            Ast::Literal(Count::One, 'b')
        ]))
    );
    assert_eq!(
        Pattern::parse("\\d"),
        Ok(t([Ast::Class(Count::One, Class::Digit)]))
    );
    assert_eq!(
        Pattern::parse("\\w"),
        Ok(t([Ast::Class(Count::One, Class::Alphanumeric)]))
    );
    assert_eq!(
        Pattern::parse("a?"),
        Ok(t([Ast::Literal(Count::ZeroOrOne, 'a')]))
    );
    assert_eq!(
        Pattern::parse("a+"),
        Ok(t([Ast::Literal(Count::OneOrMore, 'a')]))
    );
    assert_eq!(
        Pattern::parse("[abc]"),
        Ok(t([Ast::CharacterSet(CharacterSet {
            negated: false,
            count: Count::One,
            chars: "abc".to_string(),
        })]))
    );
    assert_eq!(
        Pattern::parse("[^xyz]"),
        Ok(t([Ast::CharacterSet(CharacterSet {
            negated: true,
            count: Count::One,
            chars: "xyz".to_string(),
        })]))
    );
    assert_eq!(
        Pattern::parse("^abc"),
        Ok(with_start(t([
            Ast::Literal(Count::One, 'a'),
            Ast::Literal(Count::One, 'b'),
            Ast::Literal(Count::One, 'c')
        ])))
    );
    assert_eq!(
        Pattern::parse("abc$"),
        Ok(with_end(t([
            Ast::Literal(Count::One, 'a'),
            Ast::Literal(Count::One, 'b'),
            Ast::Literal(Count::One, 'c')
        ])))
    );
    assert_eq!(
        Pattern::parse("(a|b|c)"),
        Ok(t([Ast::Alternation(Alternation {
            idx: 1,
            alternatives: vec![
                vec![Ast::Literal(Count::One, 'a')],
                vec![Ast::Literal(Count::One, 'b')],
                vec![Ast::Literal(Count::One, 'c')],
            ],
        })]))
    );
    assert_eq!(
        Pattern::parse("(a|b|)"),
        Ok(t([Ast::Alternation(Alternation {
            idx: 1,
            alternatives: vec![
                vec![Ast::Literal(Count::One, 'a')],
                vec![Ast::Literal(Count::One, 'b')],
                vec![],
            ],
        })]))
    );
    assert_eq!(
        Pattern::parse("(\\w+) and \\1"),
        Ok(t([
            Ast::Group(Group {
                idx: 1,
                items: vec![Ast::Class(Count::OneOrMore, Class::Alphanumeric)],
            }),
            Ast::Literal(Count::One, ' '),
            Ast::Literal(Count::One, 'a'),
            Ast::Literal(Count::One, 'n'),
            Ast::Literal(Count::One, 'd'),
            Ast::Literal(Count::One, ' '),
            Ast::Backreference(1)
        ]))
    );
    assert_eq!(
        Pattern::parse(r#"((\w)\2)_\1"#),
        Ok(t([
            Ast::Group(Group {
                idx: 1,
                items: vec![
                    Ast::Group(Group {
                        idx: 2,
                        items: vec![Ast::Class(Count::One, Class::Alphanumeric)]
                    }),
                    Ast::Backreference(2)
                ]
            }),
            Ast::Literal(Count::One, '_'),
            Ast::Backreference(1)
        ]))
    );
}

#[test]
fn nested_backreferences() {
    assert!(match_pattern(
        "'cat and cat' is the same as 'cat and cat'",
        r#"('(cat) and \2') is the same as \1"#
    ));
    assert!(!match_pattern(
        "'cat and cat' is the same as 'cat and dog'",
        r#"('(cat) and \2') is the same as \1"#
    ));
}

#[test]
fn multiple_backreferences() {
    assert!(match_pattern(
        "3 red squares and 3 red circles",
        r#"(\d+) (\w+) squares and \1 \2 circles"#
    ));
    assert!(!match_pattern(
        "3 red squares and 4 red circles",
        r#"(\d+) (\w+) squares and \1 \2 circles"#
    ));
    assert!(match_pattern("abc-def is abc-def, not efg, abc, or def", r#"(([abc]+)-([def]+)) is \1, not ([^xyz]+), \2, or \3"#));
    assert!(match_pattern("'howwdy hey there' is made up of 'howwdy' and 'hey'. howwdy hey there", r#"'((how+dy) (he?y) there)' is made up of '\2' and '\3'. \1"#));
    assert!(!match_pattern("'hody hey there' is made up of 'hody' and 'hey'. hody hey there", r#"'((how+dy) (he?y) there)' is made up of '\2' and '\3'. \1"#));
    assert!(!match_pattern("'howwdy heeey there' is made up of 'howwdy' and 'heeey'. howwdy heeey there", r#"'((how+dy) (he?y) there)' is made up of '\2' and '\3'. \1"#));
    assert!(match_pattern("cat and fish, cat with fish, cat and fish", r#"((c.t|d.g) and (f..h|b..d)), \2 with \3, \1"#));
}

#[test]
fn single_backreference() {
    assert!(match_pattern("cat and cat", "(cat) and \\1"));
    assert!(!match_pattern("cat and dog", "(cat) and \\1"));
    assert!(match_pattern("dog and dog", "(cat|dog) and \\1"));
    assert!(match_pattern("cat and cat", "(cat|dog) and \\1"));
    assert!(!match_pattern("cat and dog", "(cat|dog) and \\1"));
    assert!(match_pattern("cat and cat", "(\\w+) and \\1"));
    assert!(match_pattern("dog and dog", "(\\w+) and \\1"));
    assert!(!match_pattern("cat and dog", "(\\w+) and \\1"));

    assert!(match_pattern(
        "grep 101 is doing grep 101 times",
        r#"(\w\w\w\w \d\d\d) is doing \1 times"#
    ));
    assert!(!match_pattern(
        "$?! 101 is doing $?! 101 times",
        r#"(\w\w\w\w \d\d\d) is doing \1 times"#
    ));
    assert!(!match_pattern(
        "grep yes is doing grep yes times",
        r#"(\w\w\w\w \d\d\d) is doing \1 times"#
    ));
    assert!(match_pattern(
        "abcd is abcd, not efg",
        r#"([abcd]+) is \1, not [^xyz]+"#
    ));
}

#[test]
fn alternation() {
    assert!(match_pattern("dog", "(cat|dog)"));
    assert!(match_pattern("cat", "(cat|dog)"));
    assert!(!match_pattern("apple", "(cat|dog)"));
    assert!(match_pattern("aa", "a(|b)a"));
    //assert!(match_pattern("aba", "a(|b)a"));
    assert!(match_pattern("aba", "a(b|bb)a"));
    //assert!(match_pattern("abba", "a(b|bb)a"));
}

#[test]
fn group() {
    assert!(match_pattern("abcd", "((ab)c)d"));
}

#[test]
fn wildcard() {
    assert!(match_pattern("dog", "d.g"));
    assert!(!match_pattern("cog", "d.g"));
}

#[test]
fn match_zero_or_one_times() {
    assert!(match_pattern("dogs", "dogs?"));
    assert!(match_pattern("dog", "dogs?"));
    assert!(!match_pattern("cat", "dogs?"));
}

#[test]
fn match_one_or_more_times() {
    assert!(match_pattern("SaaS", "a+"));
    assert!(!match_pattern("dog", "a+"));
    assert!(match_pattern("caats", "ca+ts"));

    assert!(match_pattern("aaabb", "a+"));
    //assert!(match_pattern("baaabb", "ba+ab"));
    //assert!(match_pattern("aaabb", "aa+ab"));
    //assert!(!match_pattern("aaabb", "aa+aa"));

    assert!(match_pattern("0123", "^\\d+$"));
}

#[test]
fn end_of_string_anchor() {
    assert!(match_pattern("dog", "dog$"));
    assert!(!match_pattern("dogs", "dog$"));
}

#[test]
fn start_of_string_anchor() {
    assert!(match_pattern("log", "^log"));
    assert!(!match_pattern("slog", "^log"));
}

#[test]
fn combining_character_classes() {
    assert!(match_pattern("1 apple", "\\d apple"));
    assert!(!match_pattern("1 orange", "\\d apple"));
    assert!(match_pattern("100 apples", "\\d\\d\\d apple"));
    assert!(!match_pattern("1 apple", "\\d\\d\\d apple"));
    assert!(match_pattern("3 dogs", "\\d \\w\\w\\ws"));
    assert!(match_pattern("4 cats", "\\d \\w\\w\\ws"));
    assert!(!match_pattern("1 dog", "\\d \\w\\w\\ws"));
}
#[test]
fn test() {
    assert!(match_pattern("sally has 3 apples", "\\d apple"));
    assert!(match_pattern("apple", "[^xyz]"));
    assert!(!match_pattern("banana", "[^anb]"));
    assert!(match_pattern("a", "[abcd]"));
    assert!(!match_pattern("efgh", "[abcd]"));
    assert!(match_pattern("word", "\\w"));
    assert!(!match_pattern("$!?", "\\w"));
    assert!(match_pattern("123", "\\d"));
    assert!(!match_pattern("apple", "\\d"));
    assert!(match_pattern("dog", "d"));
    assert!(!match_pattern("dog", "f"));
}
