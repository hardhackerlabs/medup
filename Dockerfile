FROM rust:1.67.1-slim-buster as build

RUN USER=root cargo new --bin medup
WORKDIR /medup

COPY ./Cargo.lock ./Cargo.lock
COPY ./Cargo.toml ./Cargo.toml

RUN cargo build --release && rm -r ./src

COPY ./src ./src
COPY ./docs ./docs
COPY ./themes ./themes

RUN rm ./target/release/deps/medup* && cargo build --release

FROM rust:1.67.1-slim-buster

COPY --from=build /medup/target/release/medup ./medup/
COPY --from=build /medup/docs ./medup/docs/
COPY --from=build /medup/themes ./medup/themes/

EXPOSE 8181
ENTRYPOINT [ "/medup/medup" ]
CMD ["serve", "--config-path", "/medup/themes/notion/config.json", "--dir", "/medup/docs", "--static-dir", "/medup/themes"]
