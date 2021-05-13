FROM java:8
MAINTAINER Chopper
ADD xxl-job-admin-2.3.0-SNAPSHOT.jar /xxl-job.jar
ADD application.properties /application.properties
EXPOSE 9001
RUN "java -jar /xxl-job.jar --spring.config.location=/application.properties"
#"Args": [
#    "-c",
#    "java -jar /app.jar $0 $@",
#    "--spring.config.location=application.properties"
#],
#ENTRYPOINT ["java","-jar","xxl-job.jar"]