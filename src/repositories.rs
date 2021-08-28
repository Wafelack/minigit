use anyhow::Result;
use chrono::{Duration, NaiveDateTime};
use git2::{Commit, Oid, Repository, TreeWalkMode, TreeWalkResult};
use std::{
    fs,
    io::{stdout, Write, self},
    path::Path,
};

#[derive(Debug)]
pub struct MinigitCommit {
    pub oid: String,
    pub author: String,
    pub summary: String,
    pub time: NaiveDateTime,
}

#[derive(Debug)]
pub struct MinigitEntry {
    pub name: String,
    pub id: Oid,
}

pub fn get_repos(root: &str) -> Result<Vec<String>> {
    let mut repositories = vec![];
    let entries = fs::read_dir(root)?;
    print!("Processing {} ... ", root);
    stdout().flush()?;
    for entry in entries {
        let entry = entry?;
        let stringified = entry.path().to_string_lossy().to_string();
        let dotgit = format!("{}/.git", &stringified);

        if Path::new(&dotgit).exists() {
            repositories.push(stringified);
        }
    }
    println!("done\n{} repositories found.", repositories.len());
    Ok(repositories)
}
fn process_commit(commit: &Commit) -> Result<MinigitCommit> {
    let sig = commit.author();

    Ok(MinigitCommit {
        oid: commit
            .id()
            .as_bytes()
            .iter()
            .map(|b| format!("{:x}", b))
            .fold("".to_string(), |acc, v| format!("{}{}", acc, v)),
        author: sig.name().unwrap_or("").to_string(),
        summary: commit.summary().unwrap_or("").to_string(),
        time: NaiveDateTime::from_timestamp(0, 0) + Duration::seconds(sig.when().seconds()),
    })
}
pub fn list_commits(repo_path: &str) -> Result<Vec<MinigitCommit>> {
    let repository = Repository::open(repo_path)?;
    let oid = match repository.head()?.target() {
        Some(id) => id,
        None => return Ok(vec![]),
    };
    let mut commit = repository.find_commit(oid)?;
    print!("Processing {} ... ", repo_path);
    let mut commits = vec![process_commit(&commit)?];
    stdout().flush()?;
    while commit.parent_count() > 0 {
        commit = commit.parent(0)?;
        commits.push(process_commit(&commit)?);
    }
    println!("done\n{} commits processed.", commits.len());
    Ok(commits)
}

pub fn create_repos_folders(out: &str, repos: &[String]) -> Result<()> {
    repos
        .iter()
        .map(|repo| fs::create_dir_all(&format!("{}/{}", out, repo.split('/').last().unwrap())))
        .collect::<io::Result<_>>()?;
    Ok(())
}
