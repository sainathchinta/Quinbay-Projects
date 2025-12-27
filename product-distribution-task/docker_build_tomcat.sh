#!/bin/bash

SERVICE_NAME=$(grep "service-name" docker_build.properties|cut -d'=' -f2)
SERVICE_TYPE=$(grep "service-type" docker_build.properties|cut -d'=' -f2)
MAIN_MODULE=$(grep "main-module" docker_build.properties|cut -d'=' -f2)
CONTEXT_PATH=$(grep "context-path" docker_build.properties|cut -d'=' -f2)

GCR_DNS=localhost:5000
GCR_PROJECT_ID=nonprod-utility-233414
GCR_APP_PATH_PREFIX=blibli-apps
GCR_APP_PATH=${SERVICE_NAME}

if [[ ! -z "${ENV_GCR_DNS}" ]]; then
  GCR_DNS=${ENV_GCR_DNS}
fi
if [[ ! -z "${ENV_GCR_PROJECT_ID}" ]]; then
  GCR_PROJECT_ID=${ENV_GCR_PROJECT_ID}
fi
if [[ ! -z "${ENV_GCR_APP_PATH_PREFIX}" ]]; then
  GCR_APP_PATH_PREFIX=${ENV_GCR_APP_PATH_PREFIX}
fi

if [[ ${MAIN_MODULE} = *[!\ ]* ]]; then
  VERSION=$(grep "version" ./${MAIN_MODULE}/target/maven-archiver/pom.properties|cut -d'=' -f2)
  WAR_FILES=( ./${MAIN_MODULE}/target/*.war )
else
  VERSION=$(grep "version" ./target/maven-archiver/pom.properties|cut -d'=' -f2)
  WAR_FILES=( ./target/*.war )
fi

REPO=${GCR_DNS}/${GCR_PROJECT_ID}/${GCR_APP_PATH_PREFIX}/${GCR_APP_PATH}
TAG=${VERSION}

if [[ ! -z "${ENV_GCR_JSON_PATH}" ]]; then
  cat ${ENV_GCR_JSON_PATH} | docker login -u _json_key --password-stdin https://${REPO}
fi

mv ${WAR_FILES[0]} ${CONTEXT_PATH}.war

docker build --build-arg WARFILE=${CONTEXT_PATH}.war -t ${REPO}:${TAG} .
docker push ${REPO}:${TAG}