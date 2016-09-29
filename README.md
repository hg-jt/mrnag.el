# mrnag ("Mr. Nag")

*mrnag* is an Emacs package for aggregating information about open merge
requests in GitLab.

The default configuration publishes information about open merge requests to an
org-mode buffer, but this backend can be replaced with a custom publisher.


## Installation

    M-x package-install-file <path-to-mrnag>/mrnag.el


## Configuration

You will need the following information to configure *mrnag*:

* The base URL for GitLab
* A GitLab authentication token
* The name and GitLab project id for one or more projects

To configure *mrnag* add the following to your init.el:

```lisp
(eval-after-load "mrnag"
  '(setq mrnag-gitlab-baseurl "https://gitlab.example.com/api/v3"
         mrnag-gitlab-token   "<your-token-here>"
         mrnag-projects-alist '((foo . 123)
                                (bar . 456))))
```


## Usage

To generate a report of open merge requests:

    M-x mrnag
