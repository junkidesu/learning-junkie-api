FROM python:3.13.9

ARG filepath 

WORKDIR /usr/app 

COPY . .

RUN echo "#!/bin/sh\n" >> ./entrypoint.sh
RUN echo "python ./$filepath" >> ./entrypoint.sh

RUN chmod +x ./entrypoint.sh

ENTRYPOINT ["./entrypoint.sh"]
