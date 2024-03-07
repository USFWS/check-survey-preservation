<!-- badges: start -->

<!-- For more info: https://usethis.r-lib.org/reference/badges.html -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

# R7 (Alaska) GitHub Repository Template

## Overview

The **r7-repo-template** is a [GitHub repository template](https://docs.github.com/en/repositories/creating-and-managing-repositories/creating-a-repository-from-a-template) for USFWS Region 7 (Alaska) projects. The template is used to create a repository containing metadata files that meet [DOI GitHub Enterprise Cloud (DGEC) development guidance](https://doimspp.sharepoint.com/sites/ocio-DOI-GitHub-Enterprise/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2Focio%2DDOI%2DGitHub%2DEnterprise%2FShared%20Documents%2FGeneral%2FRules%20of%20Behavior%2FDGEC%20Development%20Guidance%2Epdf&parent=%2Fsites%2Focio%2DDOI%2DGitHub%2DEnterprise%2FShared%20Documents%2FGeneral%2FRules%20of%20Behavior) requirements. These metadata files should be customized to the specific repository by following the instructions under [Customize metadata files](#update-metadata-files) below.

## Installation

No installation is necessary. Follow the instructions under [Usage](#usage) below to create a repository using this template.

## Usage

According to the DGEC Rules of Behavior, **you must hold a [Maintain Role](https://doimspp.sharepoint.com/sites/ocio-DOI-GitHub-Enterprise/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2Focio%2DDOI%2DGitHub%2DEnterprise%2FShared%20Documents%2FGeneral%2FRules%20of%20Behavior%2FDGEC%20Rules%20of%20Behavior%20%2D%20Maintain%2Epdf&parent=%2Fsites%2Focio%2DDOI%2DGitHub%2DEnterprise%2FShared%20Documents%2FGeneral%2FRules%20of%20Behavior) to create repositories within the DGEC.** There are two ways to create a new repository using this template. If you have a DGEC Maintain Role, you can follow either of the options below.

### Create a new repository

#### Option 1. Create a new repository from the FWS GitHub organization home page

1.  From the home page of the FWS GitHub organization, select the [Repositories tab](https://github.com/orgs/USFWS/repositories).
2.  Select the **New repository** green button.
3.  Under Repository template, select **USFWS/r7-repo-template**. Leave **Include all branches** unchecked.
4.  Give your new repository a name. A repository name should be descriptive, readable, consistent, contextual, and brief. For repositories that are not R packages, the best practice is to name repositories using lower case alphanumeric characters separated by a dash (-) rather than spaces or underscores. R package names must only consist or letters, numbers, and periods. They must start with a letter and can not end with a period. For other things to consider, refer to Hadley Wickham's [R Packages](https://r-pkgs.org/workflow101.html#name-your-package) book. 
5.  Select the [repository visibility](https://docs.github.com/en/enterprise-cloud@latest/repositories/managing-your-repositorys-settings-and-features/managing-repository-settings/setting-repository-visibility) (i.e., Public, Internal, or Private). See **2.1 Repository Classification** in the [DOI DGEC Development Guidance document](https://doimspp.sharepoint.com/sites/ocio-DOI-GitHub-Enterprise/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2Focio%2DDOI%2DGitHub%2DEnterprise%2FShared%20Documents%2FGeneral%2FRules%20of%20Behavior%2FDGEC%20Development%20Guidance%2Epdf&parent=%2Fsites%2Focio%2DDOI%2DGitHub%2DEnterprise%2FShared%20Documents%2FGeneral%2FRules%20of%20Behavior) for information on an appropriate level of visibility for your repository.
6.  Select **Create repository from template**.

#### Option 2. Create a new repository from the r7-repo-template repository

1.  Navigate to the [r7-repo-template repository](https://github.com/USFWS/r7_DGEC_template).
2.  Select the **Use this template** green button.
3.  Choose "Create a new repository" from the drop down menu.
4.  Give your new repository a name. A repository name should be descriptive, readable, consistent, contextual, and brief. For repositories that are not R packages, the best practice is to name repositories using lower case alphanumeric characters separated by a dash (-) rather than spaces or underscores. R package names must only consist or letters, numbers, and periods. They must start with a letter and can not end with a period. For other things to consider, refer to Hadley Wickham's [R Packages](https://r-pkgs.org/workflow101.html#name-your-package) book. 
5.  Select the [repository visibility](https://docs.github.com/en/enterprise-cloud@latest/repositories/managing-your-repositorys-settings-and-features/managing-repository-settings/setting-repository-visibility) (i.e., Public, Internal, or Private). See **2.1 Repository Classification** in the [DOI DGEC Development Guidance document](https://doimspp.sharepoint.com/sites/ocio-DOI-GitHub-Enterprise/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2Focio%2DDOI%2DGitHub%2DEnterprise%2FShared%20Documents%2FGeneral%2FRules%20of%20Behavior%2FDGEC%20Development%20Guidance%2Epdf&parent=%2Fsites%2Focio%2DDOI%2DGitHub%2DEnterprise%2FShared%20Documents%2FGeneral%2FRules%20of%20Behavior) for information on an appropriate level of visibility for your repository.
6.  Select **Create repository from template**.

### Update metadata files

The **r7-repo-template** contains metadata files that are required under the [DOI DGEC Development Guidance document](https://doimspp.sharepoint.com/sites/ocio-DOI-GitHub-Enterprise/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2Focio%2DDOI%2DGitHub%2DEnterprise%2FShared%20Documents%2FGeneral%2FRules%20of%20Behavior%2FDGEC%20Development%20Guidance%2Epdf&parent=%2Fsites%2Focio%2DDOI%2DGitHub%2DEnterprise%2FShared%20Documents%2FGeneral%2FRules%20of%20Behavior). The files described in the table below should be updated when creating a repository from this template.

| File name     | Required?   | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|-------------------|-------------------|----------------------------------|
| README.md     | Required    | Includes a project title and description, installation instructions, and usage instructions. It should also include contributing guidelines (e.g., a statement stating if and how contributions will be accepted), unless the repository has a CONTRIBUTING.md file.                                                                                                                                                                                                                                                                                                                                                                         |
| LICENSE       | Required    | Identifies the content license types. Refer to the [DOI OSS Policy](https://doimspp.sharepoint.com/sites/doi-imt-services/Memorandums%20and%20Directives/Forms/Date%20Sorted.aspx?id=%2Fsites%2Fdoi%2Dimt%2Dservices%2FMemorandums%20and%20Directives%2FFY2022%2FOCIO%20Memo%5FOpen%20Source%20Software%20Policy%5FSigned%2011082021%2Epdf&parent=%2Fsites%2Fdoi%2Dimt%2Dservices%2FMemorandums%20and%20Directives%2FFY2022) for a list of recommended license types. The default license is [Creative Commons Zero v1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/).                             |
| DISCLAIMER.md | Required    | A generic disclaimer file. The template disclaimer is adapted from one approved by the [USGS Office of Science Quality and Integrity](https://www.usgs.gov/about/organization/science-support/office-science-quality-and-integrity/fundamental-science-5#5).                                                                                                                                                                                                                                                                                                                                                                         |
| NEWS.md       | Required    | A description of the changes made between each version of software, up until the latest version. It is used to log things such as new features that have been added or bugs that have been fixed.                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| code.json     | Required    | Supports code.gov integration                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| gitignore     | Recommended | Specifies intentionally untracked files that Git should ignore. Files already tracked by Git are not affected. See [here](https://git-scm.com/docs/gitignore) for more info.                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| CODEOWNERS    | Optional    | Defines individuals or teams that are responsible for code in a repository. Is required for using [GitHub Actions](https://doimspp.sharepoint.com/sites/ocio-DOI-GitHub-Enterprise/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2Focio%2DDOI%2DGitHub%2DEnterprise%2FShared%20Documents%2FGeneral%2FRules%20of%20Behavior%2FDGEC%20GitHub%20Actions%2Epdf&parent=%2Fsites%2Focio%2DDOI%2DGitHub%2DEnterprise%2FShared%20Documents%2FGeneral%2FRules%20of%20Behavior). See [here](https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/customizing-your-repository/about-code-owners) for more info. |

## Getting help

Contact the [project maintainer](emailto:firstname_lastname@fws.gov) for help with this repository. If you have general questions on creating repositories in the USFWS DGEC, reach out to a USFWS DGEC [owner](https://github.com/orgs/USFWS/people?query=role%3Aowner).

## Contribute

Contact the project maintainer for information about contributing to this repository. Submit a [GitHub Issue](https://github.com/USFWS/r7-repo-template/issues) to report a bug or request a feature or enhancement.
