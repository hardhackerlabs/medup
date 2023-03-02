FROM rust:1.67.1-slim-buster as build

# create a new empty shell project
RUN USER=root cargo new --bin medup
WORKDIR /medup

# copy over your manifests
COPY ./Cargo.lock ./Cargo.lock
COPY ./Cargo.toml ./Cargo.toml

# this build step will cache your dependencies
RUN cargo build --release && rm -r ./src

# copy your source tree
COPY ./src ./src
COPY ./docs ./docs
COPY ./themes ./themes

# build for release
RUN rm ./target/release/deps/medup* && cargo build --release

# our final base
FROM rust:1.67.1-slim-buster

# copy the build artifact from the build stage
COPY --from=build /medup/target/release/medup ./medup/
COPY --from=build /medup/docs ./medup/docs/
COPY --from=build /medup/themes ./medup/themes/

# set the startup command to run your binary
ENTRYPOINT [ "/medup/medup" ]
CMD ["serve", "--config-path", "/medup/themes/notion/config.json", "--dir", "/medup/docs", "--static-dir", "/medup/themes"]