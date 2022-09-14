# Template SpringBoot Project

## Configuring the project

This template project can be used to help start your new Digital project. This README will contain details on what needs to be changed to get your project configured.
Here are the steps required for you to configure this template into your specific project: 
- Replace usages of the `XYZ-TEMPLATE`, `xyzt`, `XYZ Template` 
- Update `https://hooks.slack.com/services/UPDATE_WEBHOOK` with the new slack webhook 
- Rename the packages (currently it's `uk.co.nstauthority.scap`)
- Rename the class `XyzTemplateApplication`
- Build the secrets and add to `.drone.yml`
- Rename the project : File > Project Structure > Project > Project Name
- Update banner.txt
- Update the README

Note that this template uses a Postgres database, so make sure that the database is running correctly after you have renamed it by following the first Setup step below. 

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

### Production
- In your IntelliJ run configuration for the Spring app, include `production` in your active profiles
- The following environment variables are required when using this profile:

| Environment Variable   | Description                                    |
|------------------------|------------------------------------------------|
| SCAP_DATABASE_URL      | The URL to the database the service connect to |
| SCAP_DATABASE_PASSWORD | Database schema password for the `XYZT` user   |

### 3. Initialise the Fivium Design System
- `git submodule update --init --recursive`
- `cd fivium-design-system-core && npm install && npx gulp build && cd ..`

### 4. Build frontend components
- `npm install`
- `npx gulp buildAll`

### 5. Run the app
Create a run configuration for the Spring app and start the application.

The application will be running on `localhost:8080/SCAP/<endpoint>`

## Development setup

### Checkstyle
1. In Intellij install the Checkstyle-IDEA plugin (from third-party repositories)
2. Go to File > Settings > Tools > Checkstyle 
3. Click the plus icon under "Configuration File"
4. Select "Use a local Checkstyle file"
5. Select `devtools/checkstyle.xml`
6. Check the "Active" box next to the new profile

Note that Checkstyle rules are checked during the build process and any broken rules will fail the build.

