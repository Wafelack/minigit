pub fn build_page(title: &str, body: &[String], css: &str) -> String {
    format!(
        r#"<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="utf8" />
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />
        <link rel="stylesheet" type="text/css" href="{}" />
        <title>{}</title>
    </head>
    <body>
        <h1>{}</h1>
        {}
    </body>
</html>"#,
        css,
        title,
        title,
        body.iter()
            .fold("".to_string(), |acc, v| format!("{}{}", acc, v))
    )
}

pub fn build_repo_page(title: &str, body: &[String], css: &str) -> String {
    let mut new_body = vec!["<table class=\"links\"><tbody><tr><td><a href=\"./log.html\">Log</a></td><td><a href=\"/files.html\">Files</a></td></tr></tbody></table>".to_string()];
    new_body.extend_from_slice(body);
    build_page(title, &new_body, css)
}
