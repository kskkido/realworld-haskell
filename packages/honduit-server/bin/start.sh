function main {
  SCRIPT_DIR=$(cd "$(dirname $0)"; pwd)
  PROJECT_DIR=$(dirname $SCRIPT_DIR)
  cd ${PROJECT_DIR}
  STAGE=${1:?"Missing stage"}
  BUILD_ENV=$(cat configs/.env.${STAGE})
  export $(echo ${BUILD_ENV} | xargs)
  cabal run
}

main $*

