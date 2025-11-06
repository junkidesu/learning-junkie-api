FROM haskell:9.6.4 

ARG filepath 

WORKDIR /usr/space

COPY . . 

RUN echo "#!/bin/sh\n" >> ./entrypoint.sh
RUN echo stack runhaskell ./$filepath >> ./entrypoint.sh

RUN chmod +x ./entrypoint.sh


ENTRYPOINT ["./entrypoint.sh"]
