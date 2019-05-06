# build

FROM node:10 as build

WORKDIR /build

COPY package.json yarn.lock ./
RUN yarn install

COPY bower.json ./
RUN echo '{ "allow_root": true }' > /root/.bowerrc && yarn bower install

COPY . ./
RUN yarn pulp build

# prod

FROM node:10-alpine

ENV NODE_ENV=production

WORKDIR /w

COPY --from=build /build/package.json /build/yarn.lock ./
RUN yarn install --production && yarn cache clean

COPY --from=build /build/public ./public
COPY --from=build /build/output ./output
COPY --from=build /build/src ./src
COPY --from=build /build/server.js .

EXPOSE 3000

CMD ["node", "server.js"]
