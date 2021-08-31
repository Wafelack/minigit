mod renderer;
mod repositories;
mod templates;

use anyhow::Result;
use renderer::{render_index, render_repo_commits};
use repositories::{create_repos_folders, get_repos, list_commits};
use structopt::StructOpt;

#[derive(StructOpt)]
#[structopt(
    name = "minigit",
    author = "Wafelack <wafelack@riseup.net>",
    about = "Static git page generator",
    after_help = "NOTE: You must use absolute paths with the `--in`, `--out` and `--css` options."
)]
struct Minigit {
    #[structopt(short = "o", long = "out")]
    output: String,
    #[structopt(short = "i", long = "in")]
    input: String,
    #[structopt(short = "s", long = "css")]
    css: String,
}

fn main() -> Result<()> {
    let opt = Minigit::from_args();

    let repos = get_repos(&opt.input)?;
    render_index(
        &opt.output,
        &repos,
        &opt.css, 
    )?;
    create_repos_folders(&opt.output, &repos)?;
    repos
        .into_iter()
        .map(|repo| render_repo_commits(repo.split('/').last().unwrap(), &opt.output, &list_commits(&repo)?, &opt.css))
        .collect::<Result<_>>()?;

    Ok(())
}
