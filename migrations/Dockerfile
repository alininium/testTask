FROM alpine
RUN apk update && apk add postgresql-client
COPY . .
CMD ["./migrate.sh"]
