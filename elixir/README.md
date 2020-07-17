# StarterKit app for ICFP 2020

This is a Mix project setup to build a Release named `app` using the official
Dockerfile.

You can customize the build step with `app/bin/build` and the run step with
`app/bin/run`.

Both of these are copied into the final release directory using an overlay
config in `app/mix.exs`

The Dockerfile copies the everything commited within the `/app` directory
and runs the `app/bin/build` script.

Depending on your .gitignore settings, you can include the `deps/` directory
for any compilation of third party libraries.

You should not include the `app/_build` directory.
