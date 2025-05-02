# Supply chain action plan

## Pre-requisites
- Java 17
- Node LTS + NPM
- [Docker for Windows](https://hub.docker.com/editions/community/docker-ce-desktop-windows)
  (See [Docker setup](https://confluence.fivium.co.uk/display/JAVA/Java+development+environment+setup#Javadevelopmentenvironmentsetup-Docker)
  for further information about adding your account to the `docker-users` group)

## Setup

### 1. Run the backend services
- Ensure that you have [Docker for Windows](https://hub.docker.com/editions/community/docker-ce-desktop-windows)
  installed and running (or an alternative way of running docker).  
- Run the backing services defined in the `local-dev-compose.yml`. This can be done by clicking the run icon
  next to `services` when in the file.
  - If IntelliJ doesn't detect the file as a docker compose file automatically you may need to 
    [Associate docker-compose as file type](https://intellij-support.jetbrains.com/hc/en-us/community/posts/360009394620-Associate-docker-compose-as-file-type) manually.

### 2. Add the required profile

### Development
- In your IntelliJ run configuration for the Spring app, include `development` in your active profiles
- The following environment variables are required when using this profile:

| Environment Variable        | Description                                                                                 |
|-----------------------------|---------------------------------------------------------------------------------------------|
| NOTIFY_TEST_EMAIL_RECIPIENT | The email you want to receive emails on when running locally                                |
| NOTIFY_API_KEY              | The GovUK notify development key from TPM: https://tpm.fivium.co.uk/index.php/pwd/view/2082 |


### Production
- In your IntelliJ run configuration for the Spring app, include `production` in your active profiles
- The following environment variables are required when using this profile:

| Environment Variable           | Description                                                                                                                  |
|--------------------------------|------------------------------------------------------------------------------------------------------------------------------|
| SCAP_DATABASE_URL              | The URL to the database the service connect to                                                                               |
| SCAP_DATABASE_PASSWORD         | Database schema password for the `scap_app` user                                                                             |
| S3_ACCESS_KEY                  | S3 username for document uploads / downloads                                                                                 |
| S3_SECRET_KEY                  | S3 secret for document uploads / downloads                                                                                   |
| CLAMAV_HOST                    | Virus scanner host location                                                                                                  |
| FILE_UPLOAD_MAX_ALLOWED_SIZE   | Maximum file upload size in bytes                                                                                            |
| FILE_UPLOAD_ALLOWED_EXTENSIONS | Allowed file extensions for document uploads                                                                                 |
| NOTIFY_EMAIL_MODE              | Can be test or production. Test mode will redirect all outbound emails to the test recipient(s)                              |
| NOTIFY_TEST_EMAIL_RECIPIENT    | If email is test mode, who to send emails to. Value can be a CSV list                                                        |
| NOTIFY_API_KEY                 | The GOV.UK Notify key                                                                                                        |
| ENABLE_STATSD                  | Whether or not to export stats to Grafana, generally set to false on local and true anywhere else                            |
| STATSD_HOST                    | The hostname for the endpoint that takes StatsD metrics                                                                      |
| STATSD_PORT                    | The port for the endpoint that takes StatsD metrics                                                                          |
| METRICS_EXPORT_TYPE            | The 'flavour' of metrics, one of STATSD or DATADOG. Use DATADOG as it has better out-of-the-box metrics tagging capabilities |
| METRICS_INSTANCE_TAG           | Tag to help with filtering stats, set to the name of this instance, e.g. dev, st, uat, prod                                  |
| METRICS_SOURCE_TYPE_TAG        | Tag to help with filtering stats, set to the name of this project, i.e. scap                                                 |

### 3. Initialise the Fivium Design System
- `git submodule update --init --recursive`
- `cd fivium-design-system-core && npm install && npx gulp build && cd ..`

#### 3.1. Update the Fivium Design System
If FDS is not on the latest version, follow the instructions to update it [here](https://fivium.atlassian.net/wiki/spaces/FDS/pages/10354845/Releases)

### 4. Build frontend components
- `npm install`
- `npx gulp buildAll`

### 6. Generate Jooq classes
_This requires docker to be running on your machine, but have nothing running in docker_

Execute the gradle task `generateJooq`. You will need to re-generate when you change the database.

### 7. Bootstrap your local DB
Run the script in `devtools/create_dev_users.sql` to bootstrap your DB.

### 8. Run the app
Create a run configuration for the Spring app and start the application.

The application will be running on `localhost:8080/scap/<endpoint>`

### Checkstyle
1. In Intellij install the Checkstyle-IDEA plugin (from third-party repositories)
2. Go to File > Settings > Tools > Checkstyle 
3. Click the plus icon under "Configuration File"
4. Select "Use a local Checkstyle file"
5. Select `devtools/checkstyle.xml`
6. Check the "Active" box next to the new profile

Note that Checkstyle rules are checked during the build process and any broken rules will fail the build.

## End to End Tests
This project uses WDIO and Selenium to run Screen Based E2E tests.

The test suite uses Test Containers for Node to start up the applications, as well as selenium-chromium.
This keeps the running context very similar between local and drone runs.

### Running the tests
To run the tests for the first time, simply run:
Notify API key - https://tpm.fivium.co.uk/index.php/pwd/view/2082
```shell
./gradlew bootJar
npx gulp buildAll
cd e2eTests
npm ci
export NOTIFY_API_KEY=${add api key here}
npm run wdio
cd ..
```

After that you can just run:
Notify API key - https://tpm.fivium.co.uk/index.php/pwd/view/2082
```shell
cd e2eTests
rm -rf reports
export NOTIFY_API_KEY=${add api key here} 
npm run wdio
cd ..
```

You can also install the [WebDriverIO IntelliJ plugin](https://plugins.jetbrains.com/plugin/16147-webdriverio) and add a
run configuration but this doesn't do much and is a bit buggy. You'll also need to set:
- workingDirectory: e2eTests (needs to be a full path)
- wdioConfigFile: e2eTests/wdio.conf.ts (needs to be a full path)

By default, the test suite will create a new docker image each time which is slow. If you're running the tests a lot,
you can set an environment variable to tell the test suite to use that image rather than make its own.
```shell
./gradlew bootJar 
docker build -t supply-chain-action-plan:e2eTests . 
export APP_IMAGE_TO_TEST=supply-chain-action-plan:e2eTests
```
Alteratively, set the variable as an environment variable in your run configuration if using the plugin.

### Debugging the tests
Full screenshots and recordings are captured in the `e2eTests/reports` folder.

When using the Plugin, you can also add breakpoints to the Typescript.

There is also a log line: `Started Selenium - VNC: localhost:...` Navigate to that URL to connect to the chrome
container and interact with the browser.

### Authoring tests
- The FDS testing library can be accessed using `import {FdsButton} from "@fdsTestingLibrary/page-objects/components/FdsButton.ts";`