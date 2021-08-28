use crate::{
    repositories::MinigitCommit,
    templates::{build_page, build_repo_page},
};
use anyhow::Result;
use std::{
    fs::{self, File},
    io::Write,
};

pub fn render_index(out: &str, index: &[String], css: &str) -> Result<()> {
    let outf = format!("{}/index.html", out);
    let mut file = File::create(&outf)?;
    file.write_all(
        build_page(
            "Minigit Index",
            &index
                .iter()
                .map(|s| {
                    let s = s.split('/').last().unwrap_or("");
                    format!("<a href=\"./{}/log.html\">{}</a>\n", s, s)
                })
                .collect::<Vec<String>>(),
            css,
        )
        .as_bytes(),
    )?;
    Ok(())
}

pub fn render_repo_commits(
    repo: &str,
    out: &str,
    commits: &[MinigitCommit],
    css: &str,
) -> Result<()> {
    let outf = format!("{}/{}/log.html", out, repo);
    let mut file = File::create(&outf)?;
    let mut body = vec!["<table><thead><tr><td><b>Date</b></td><td><b>Summary</b></td><td><b>Author</b></td></thead><tbody>".to_string()];
    body.extend(commits.iter().map(|commit| format!("<tr><td class=\"date-summary\">{}</td><td class=\"date-summary\"><a href=\"./{}.html\">{}</a></td><td>{}</td></tr>", commit.time.format("%Y-%m-%d %H:%M"), commit.oid, commit.summary, commit.author)));
    body.push("</tbody></table>".to_string());
    file.write_all(build_repo_page(repo, &body, css).as_bytes())?;
    Ok(())
}
