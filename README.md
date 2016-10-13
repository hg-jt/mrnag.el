# mrnag ("Mr. Nag")

*mrnag* is an Emacs package that aggregates information about open merge
requests in one or more GitLab projects.

The default configuration publishes information about these merge requests to an
org-mode buffer, but this backend can be replaced with a custom publisher.


## Installation

*mrnag* does not have any external dependencies and fully integrates with the
Emacs Package Manager

To install *mrnag*:

    M-x package-install-file <path-to-mrnag>/mrnag.el


## Configuration

You will need the following information to configure *mrnag*:

* The base URL for GitLab
* A GitLab [authentication token](https://docs.gitlab.com/ce/api/#authentication)
* The name and GitLab project id for one or more projects

To configure *mrnag* use the `eval-after-load` function. This ensures that the
package has been loaded before it is configured.

```lisp
(eval-after-load "mrnag"
  (lambda ()
    (setq mrnag-gitlab-baseurl "https://gitlab.example.com/api/v3"
          mrnag-gitlab-token   "<your-token-here>"
          mrnag-projects-alist '(("foo" . 123)
                                 ("bar" . 456)))))
```


## Usage

To generate a report of open merge requests:

    M-x mrnag


## Custom Publisher Backends

Custom publishers can be defined by implementing a function that accepts a *list
of project association lists* and registering that function with *mrnag*.

Project association lists contains up to five keys:

* *id*

    The numeric id or namespace/projectname in Gitlab.

* *name*

    A displayable name for the project.

* *merge-requests*

    A vector of open merge requests. Each item in the vector is an alist
    representation of a merge request in GitLab's [List Merge Requests][1] API.

* *project-url*

    The project's browsable URL.

* *avatar_url*

    The link to the project's avatar, if one is defined.


#### Registering Publishers

To register a publisher, add it to `mrnag-publishers-alist`. This variable is an
alist whose elements in the form `(symbol . function)`:


#### Activating Publishers

To activate your publisher, set the `mrnag-publisher` variable to it's
registered symbol in `mrnag-publishers-alist`.


[1]: https://docs.gitlab.com/ce/api/merge_requests.html#list-merge-requests
