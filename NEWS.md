# susoapi 0.2.0

## New features

- Credentials now include the workspace. `set_credentials()` now has a `workspace` argument. All functions that target workspaces now draw from these credentials (#24, #26, h/t @ashwinikalantri). 
- Interview attributes can now be user-specified. Through the `nodes` argument of `get_interviews()`, users may specify as few or as many nodes of information about interviews as needed. If nothing is specified, the default is to fetch all nodes. If something is specified, it must include `id` and may include other things (#23).

## Breaking changes

- Credentials used in all functions now expect workspace. Users will need to use `set_credentials()` to add this component to their `.Renviron` file.

## Minor improvements and bug fixes

- Fixed help pages where documentation suggested that logical values be provided as strings rather than as the logical values that the code required (#25, h/t @ashwinikalantri).
- Fixed issue where failure to specify `qnr_version` in `get_assignments()` lead to the function returning all assignments for all questionnaires (#27, h/t @ashwinikalantri).
- Added checks for arguments throughout--typically logical values, user names, and dates for time intervals--and tests to ensure those checks work correctly.
- Convert `NA_character_` comments to empty strings before forming the request in interview functions.
- Added a private function `is_user_name()` to check whether user-provided user names followed Survey Solutions rules about character length and content.
- Revised order of arguments and documentation for credentials: server, workspace, user, password.

# susoapi 0.1.3

- Fix `check_credentials()`. Check credentials by getting API user's details rather than fetching workspaces to which the user has access. The former only requires simple API access for the workspace. The latter requires admin API access.

# susoapi 0.1.2

- Fix `set_credentials()`. Better managment of pre-existing `.Renviron` files. Previously, the function could delete several or all key-value pairs in `.Renviron` while adding/updating Survey Solutions entries. Now, the function preserves all prior key-value entries. Also, tests have been added to check that this function works correctly going forward. A huge thanks to @petbrueck for spotting this and suggesting a fix (#20).
- Revise `check_workspace_param()`, a private function used to check that the API user has access to the user-specified workspace, to work better when the API user has access to few or only one workspace. Previously, the function fetched all workspaces to which the user had access. This only worked when the API user had admin rights to access the workspace endpoints. Now, the function checks whether the API user has access to the target workspace by fetching its user details for that workspace.

# susoapi 0.1.1

- Fix `get_interviewers()`. A private function was missing the workspaces argument. (h/t @ashwinikalantri)

# susoapi 0.1.0

- Scope all functions to workspaces. If no workspace is specified, the function will take `"primary"` as the default value. If a workspace is specified, the function will attempt to execute within the designated workspace. The function will only yield a result if the user is authorized to access that workspace. If the function fails, it will both indicate that the target workspace is invalid/inaccessible and will say which workspaces the user credentials can access. To pre-empt function failure, the user can also use `check_credentials()` and specify the target workspace.
- Re-implement several functions with GraphQL API, where the REST endpoint is either deprecated or a less good solution than GraphQL. These include: `get_questionnaires`, `get_interviews_for_questionnaire`, and `get_interviews`.
- Minor fixes
