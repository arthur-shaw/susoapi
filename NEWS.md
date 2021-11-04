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
