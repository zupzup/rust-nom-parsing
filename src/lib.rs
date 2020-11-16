use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take},
    character::complete::{alpha1, alphanumeric1, one_of},
    combinator::opt,
    error::Error,
    error::ErrorKind,
    multi::{count, many0, many1, many_m_n},
    sequence::{preceded, separated_pair, terminated, tuple},
    AsChar, Err as NomErr, IResult, InputTakeAtPosition,
};

#[derive(Debug, PartialEq, Eq)]
pub struct URI<'a> {
    scheme: Scheme,
    authority: Option<Authority<'a>>,
    host: Host,
    port: Option<u16>,
    path: Option<Vec<&'a str>>,
    query: Option<QueryParams<'a>>,
    fragment: Option<&'a str>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Scheme {
    HTTP,
    HTTPS,
}

impl From<&str> for Scheme {
    fn from(i: &str) -> Self {
        match i.to_lowercase().as_str() {
            "http://" => Scheme::HTTP,
            "https://" => Scheme::HTTPS,
            _ => unimplemented!("no other schemes supported"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Host {
    HOST(String),
    IP([u8; 4]),
}

pub type Authority<'a> = (&'a str, Option<&'a str>);
pub type QueryParam<'a> = (&'a str, &'a str);
pub type QueryParams<'a> = Vec<QueryParam<'a>>;

fn scheme(input: &str) -> IResult<&str, Scheme> {
    alt((tag_no_case("HTTP://"), tag_no_case("HTTPS://")))(input)
        .map(|(next_input, res)| (next_input, res.into()))
}

fn authority(input: &str) -> IResult<&str, (&str, Option<&str>)> {
    terminated(
        separated_pair(alphanumeric1, opt(tag(":")), opt(alphanumeric1)),
        tag("@"),
    )(input)
}

fn host_or_ip(input: &str) -> IResult<&str, Host> {
    alt((host, ip))(input)
}

fn host(input: &str) -> IResult<&str, Host> {
    alt((
        tuple((many1(terminated(alphanumerichyphen1, tag("."))), alpha1)),
        tuple((many_m_n(1, 1, alphanumerichyphen1), take(0 as usize))),
    ))(input)
    .map(|(next_input, mut res)| {
        println!("res: {:?}", res);
        if !res.1.is_empty() {
            res.0.push(res.1);
        }
        (next_input, Host::HOST(res.0.join(".")))
    })
}

fn ip(input: &str) -> IResult<&str, Host> {
    tuple((count(terminated(ip_num, tag(".")), 3), ip_num))(input).map(|(next_input, res)| {
        let mut result: [u8; 4] = [0, 0, 0, 0];
        res.0
            .into_iter()
            .enumerate()
            .for_each(|(i, v)| result[i] = v);
        result[3] = res.1;
        (next_input, Host::IP(result))
    })
}

fn ip_num(input: &str) -> IResult<&str, u8> {
    n_to_m_digits(1, 3)(input).and_then(|(next_input, result)| match result.parse::<u8>() {
        Ok(n) => Ok((next_input, n)),
        Err(_) => Err(NomErr::Error(Error::new(next_input, ErrorKind::Digit))),
    })
}

fn port(input: &str) -> IResult<&str, u16> {
    preceded(tag(":"), n_to_m_digits(2, 4))(input).and_then(|(next_input, res)| {
        match res.parse::<u16>() {
            Ok(n) => Ok((next_input, n)),
            Err(_) => Err(NomErr::Error(Error::new(next_input, ErrorKind::Digit))),
        }
    })
}

fn path(input: &str) -> IResult<&str, Vec<&str>> {
    tuple((
        tag("/"),
        many0(terminated(url_code_points, tag("/"))),
        opt(url_code_points),
    ))(input)
    .map(|(next_input, res)| {
        println!("res: {:?}", res);
        let mut path: Vec<&str> = res.1.iter().map(|p| p.to_owned()).collect();
        if let Some(last) = res.2 {
            path.push(last);
        }
        (next_input, path)
    })
}

fn query_params(input: &str) -> IResult<&str, QueryParams> {
    tuple((
        tag("?"),
        url_code_points,
        tag("="),
        url_code_points,
        many0(tuple((
            tag("&"),
            url_code_points,
            tag("="),
            url_code_points,
        ))),
    ))(input)
    .map(|(next_input, res)| {
        let mut qps = Vec::new();
        qps.push((res.1, res.3));
        for qp in res.4 {
            qps.push((qp.1, qp.3));
        }
        (next_input, qps)
    })
}

fn fragment(input: &str) -> IResult<&str, &str> {
    tuple((tag("#"), url_code_points))(input).map(|(next_input, res)| (next_input, res.1))
}

pub fn uri(input: &str) -> IResult<&str, URI> {
    tuple((
        scheme,
        opt(authority),
        host_or_ip,
        opt(port),
        opt(path),
        opt(query_params),
        opt(fragment),
    ))(input)
    .map(|(next_input, res)| {
        let (scheme, authority, host, port, path, query, fragment) = res;
        (
            next_input,
            URI {
                scheme,
                authority,
                host,
                port,
                path,
                query,
                fragment,
            },
        )
    })
}

fn alphanumerichyphen1<T>(i: T) -> IResult<T, T>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-') && !char_item.is_alphanum()
        },
        ErrorKind::AlphaNumeric,
    )
}

fn url_code_points<T>(i: T) -> IResult<T, T>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-') && !char_item.is_alphanum() && !(char_item == '.')
            // ... actual ascii code points and url encoding...: https://infra.spec.whatwg.org/#ascii-code-point
        },
        ErrorKind::AlphaNumeric,
    )
}

fn n_to_m_digits<'a>(n: usize, m: usize) -> impl FnMut(&'a str) -> IResult<&'a str, String> {
    move |input| {
        many_m_n(n, m, one_of("0123456789"))(input)
            .map(|(next_input, result)| (next_input, result.into_iter().collect()))
    }
}

#[test]
fn test_scheme() {
    assert_eq!(scheme("https://yay"), Ok(("yay", Scheme::HTTPS)));
    assert_eq!(scheme("http://yay"), Ok(("yay", Scheme::HTTP)));
    assert_eq!(
        scheme("bla://yay"),
        Err(NomErr::Error(Error::new("bla://yay", ErrorKind::Tag)))
    );
}

#[test]
fn test_authority() {
    assert_eq!(
        authority("username:password@zupzup.org"),
        Ok(("zupzup.org", ("username", Some("password"))))
    );
    assert_eq!(
        authority("username@zupzup.org"),
        Ok(("zupzup.org", ("username", None)))
    );
    assert_eq!(
        authority("zupzup.org"),
        Err(NomErr::Error(Error::new(".org", ErrorKind::Tag)))
    );
    assert_eq!(
        authority(":zupzup.org"),
        Err(NomErr::Error(Error::new(
            ":zupzup.org",
            ErrorKind::AlphaNumeric
        )))
    );
    assert_eq!(
        authority("username:passwordzupzup.org"),
        Err(NomErr::Error(Error::new(".org", ErrorKind::Tag)))
    );
    assert_eq!(
        authority("@zupzup.org"),
        Err(NomErr::Error(Error::new(
            "@zupzup.org",
            ErrorKind::AlphaNumeric
        )))
    )
}

#[test]
fn test_host() {
    assert_eq!(
        host("localhost:8080"),
        Ok((":8080", Host::HOST("localhost".to_string())))
    );
    assert_eq!(
        host("example.org:8080"),
        Ok((":8080", Host::HOST("example.org".to_string())))
    );
    assert_eq!(
        host("some-subsite.example.org:8080"),
        Ok((":8080", Host::HOST("some-subsite.example.org".to_string())))
    );
    assert_eq!(
        host("example.123"),
        Ok((".123", Host::HOST("example".to_string())))
    );
    assert_eq!(
        host("$$$.com"),
        Err(NomErr::Error(Error::new(
            "$$$.com",
            ErrorKind::AlphaNumeric
        )))
    );
    assert_eq!(
        host(".com"),
        Err(NomErr::Error(Error::new(".com", ErrorKind::AlphaNumeric)))
    );
}

#[test]
fn test_ipv4() {
    assert_eq!(
        ip("192.168.0.1:8080"),
        Ok((":8080", Host::IP([192, 168, 0, 1])))
    );
    assert_eq!(ip("0.0.0.0:8080"), Ok((":8080", Host::IP([0, 0, 0, 0]))));
    assert_eq!(
        ip("1924.168.0.1:8080"),
        Err(NomErr::Error(Error::new("4.168.0.1:8080", ErrorKind::Tag)))
    );
    assert_eq!(
        ip("192.168.0000.144:8080"),
        Err(NomErr::Error(Error::new("0.144:8080", ErrorKind::Tag)))
    );
    assert_eq!(
        ip("192.168.0.1444:8080"),
        Ok(("4:8080", Host::IP([192, 168, 0, 144])))
    );
    assert_eq!(
        ip("192.168.0:8080"),
        Err(NomErr::Error(Error::new(":8080", ErrorKind::Tag)))
    );
    assert_eq!(
        ip("999.168.0.0:8080"),
        Err(NomErr::Error(Error::new(".168.0.0:8080", ErrorKind::Digit)))
    );
}

#[test]
fn test_path() {
    assert_eq!(path("/a/b/c?d"), Ok(("?d", vec!["a", "b", "c"])));
    assert_eq!(path("/a/b/c/?d"), Ok(("?d", vec!["a", "b", "c"])));
    assert_eq!(path("/a/b-c-d/c/?d"), Ok(("?d", vec!["a", "b-c-d", "c"])));
    assert_eq!(path("/a/1234/c/?d"), Ok(("?d", vec!["a", "1234", "c"])));
    assert_eq!(
        path("/a/1234/c.txt?d"),
        Ok(("?d", vec!["a", "1234", "c.txt"]))
    );
}

#[test]
fn test_query_params() {
    assert_eq!(
        query_params("?bla=5&blub=val#yay"),
        Ok(("#yay", vec![("bla", "5"), ("blub", "val")]))
    );

    assert_eq!(
        query_params("?bla-blub=arr-arr#yay"),
        Ok(("#yay", vec![("bla-blub", "arr-arr"),]))
    );
}

#[test]
fn test_fragment() {
    assert_eq!(fragment("#bla"), Ok(("", "bla")));
    assert_eq!(fragment("#bla-blub"), Ok(("", "bla-blub")));
}

#[test]
fn test_uri() {
    assert_eq!(
        uri("https://www.zupzup.org/about/"),
        Ok((
            "",
            URI {
                scheme: Scheme::HTTPS,
                authority: None,
                host: Host::HOST("www.zupzup.org".to_string()),
                port: None,
                path: Some(vec!["about"]),
                query: None,
                fragment: None
            }
        ))
    );

    assert_eq!(
        uri("https://www.zupzup.org:443/about/?someVal=5#anchor"),
        Ok((
            "",
            URI {
                scheme: Scheme::HTTPS,
                authority: None,
                host: Host::HOST("www.zupzup.org".to_string()),
                port: Some(443),
                path: Some(vec!["about"]),
                query: Some(vec![("someVal", "5")]),
                fragment: Some("anchor")
            }
        ))
    );
}
