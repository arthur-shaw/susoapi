# susoapi 0.1.1

- Fix `get_interviewers()`. A private function was missing the workspaces argument. (h/t @ashwinikalantri)

# susoapi 0.1.0

- Scope all functions to workspaces. If no workspace is specified, the function will take `"primary"` as the default value. If a workspace is specified, the function will attempt to execute within the designated workspace. The function will only yield a result if the user is authorized to access that workspace. If the function fails, it will both indicate that the target workspace is invalid/inaccessible and will say which workspaces the user credentials can access. To pre-empt function failure, the user can also use `check_credentials()` and specify the target workspace.
- Re-implement several functions with GraphQL API, where the REST endpoint is either deprecated or a less good solution than GraphQL. These include: `get_questionnaires`, `get_interviews_for_questionnaire`, and `get_interviews`.
- Minor fixes
