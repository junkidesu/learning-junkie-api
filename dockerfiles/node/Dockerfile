FROM node:16

ARG filepath 

WORKDIR /usr/app 

COPY --chown=node:node . .

RUN echo "#!/bin/sh\n" >> ./entrypoint.sh
RUN echo "node ./$filepath" >> ./entrypoint.sh
RUN chmod +x ./entrypoint.sh

USER node

ENTRYPOINT ["./entrypoint.sh"]
