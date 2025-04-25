import {
    GenericContainer,
    Wait,
    getContainerRuntimeClient,
    StartedTestContainer,
} from "testcontainers"
import {PostgreSqlContainer, StartedPostgreSqlContainer} from "@testcontainers/postgresql";
import Docker from "dockerode";
import getPort from 'get-port';
import {promises as dns} from "dns";

const docker = new Docker();
const networkName = "scap-e2e-test-network";

/**
 * This method will start a network, and optionally join node container to this network
 * Without putting the node container on the new network the node container cannot reach selenium
 * This is important for running on Drone
 * When running locally, there is no node container as the tests aren't run inside a container
 */
export async function startNetwork() {
    const startedNetwork = await getOrCreateNetwork();
    const nodeContainerInfo = await getNodeContainer();
    if (!nodeContainerInfo) {
        // Must be running locally, no need to join the node container
        return startedNetwork;
    }
    const runtimeClient = await getContainerRuntimeClient();
    const nodeContainer = runtimeClient.container.getById(nodeContainerInfo.Id);
    const network = runtimeClient.network.getById(startedNetwork.id);
    if (!(await nodeContainerIsAlreadyOnNetwork(nodeContainerInfo))) {
        await runtimeClient.container.connectToNetwork(nodeContainer, network, []);
    }
    return startedNetwork;
}

const nodeContainerIsAlreadyOnNetwork = async (nodeContainerInfo: Docker.ContainerInfo) => {
    const containerInfo = await docker.getContainer(nodeContainerInfo.Id).inspect();
    const networkNames = Object.keys(containerInfo.NetworkSettings.Networks);
    return networkNames.find(currentNetworkName => currentNetworkName === networkName);
}

export async function startupSelenium() {
    const startedContainer = await new GenericContainer("selenium/standalone-chromium:125.0-chromedriver-125.0")
        .withNetworkMode(networkName)
        .withName("selenium")
        // need to start container with root privileges, to avoid using sudo when installing ncat, so that this works on drone
        .withUser("root")
         // .withLogConsumer(stream => {
         //     stream.on("data", line => console.log("Selenium: " + line));
         //     stream.on("err", line => console.error("Selenium: " + line));
         //     stream.on("end", () => console.log("Selenium: " + "Stream closed"));
         // })
        // .withEnvironment({
        //     "SE_LOG_LEVEL": "INFO"
        // })
        .withExposedPorts(
            4444,
            {
                container: 54322,
                host: await getPort({port: 54322}),
            },
            {
                container: 7900,
                host: await getPort({port: 54321})
            }
        )
        .withWaitStrategy(Wait.forLogMessage("Started Selenium Standalone"))
        // need to allocate container more cpu, memory, and /dev/shm memory, for the lighthouse performance tests to not crash the tab.
        .withSharedMemorySize(512 * 1024 * 1024 * 4) // 512MB * 4 = ~2GB
        .withResourcesQuota({
            cpu: 2,
            memory: 4
        })
        .withReuse()
        .start();

    console.log(`Started Selenium - VNC: http://localhost:${startedContainer.getMappedPort(7900)}/?autoconnect=1&resize=scale&password=secret`)
    return startedContainer;
}

export async function startupAppContainer(database: StartedPostgreSqlContainer, s3MockContainer: StartedTestContainer, clamAvContainer: StartedTestContainer) {

    let container: GenericContainer;
    if (process.env.APP_IMAGE_TO_TEST) {
        container = new GenericContainer(process.env.APP_IMAGE_TO_TEST);
        console.log("Using prebuilt image: " + process.env.APP_IMAGE_TO_TEST);
    } else {
        container = await GenericContainer.fromDockerfile("../")
            .build("supply-chain-action-plan-e2e-tests");
        console.log("Built fresh image")
    }

    return container.withEnvironment({
            SPRING_PROFILES_ACTIVE: "development",
            DATABASE_URL: `jdbc:postgresql://postgres:5432/${database.getDatabase()}`,
            SCHEMA_PASSWORD: database.getPassword(),
            "SAML_CONSUMER-SERVICE-LOCATION" : `http://e2e-app:8080/scap/login/saml2/sso/saml`,
            "FILE-UPLOAD_S3_ENDPOINT": `${stripLeadingSlashFromContainerName(s3MockContainer.getName())}:9090`,
            "FILE-UPLOAD_CLAMAV_HOST" : stripLeadingSlashFromContainerName(clamAvContainer.getName()),
            "FILE-UPLOAD_CLAMAV_PORT" : "3310",
            "NOTIFY_API_KEY":process.env.NOTIFY_API_KEY,
            "NOTIFY_TEST_EMAIL_RECIPIENT":"test@test.co.uk",
    })
            .withDefaultLogDriver()
            // .withLogConsumer(stream => {
            //     stream.on("data", line => console.log("App: " + line));
            //     stream.on("err", line => console.error("App: " + line));
            //     stream.on("end", () => console.log("App: " + "Stream closed"));
            // })
            .withNetworkMode(networkName)
            // This name is whitelisted in the EnergyPortal CORS policy
            .withName("e2e-app")
            .withStartupTimeout(120000)
            .withWaitStrategy(Wait.forLogMessage("Started ScapApplication"))
            .withReuse()
            .start();
}

export async function startUpDatabaseContainer() {
    return new PostgreSqlContainer()
        .withUsername("scap_app")
        .withPassword("EndToEndTests")
        .withDatabase("scap")
        .withName("postgres")
        .withExposedPorts(5432)
        .withNetworkMode(networkName)
        // .withLogConsumer(stream => {
        //     stream.on("data", line => console.log("Database: " + line));
        //     stream.on("err", line => console.error("Database: " + line));
        //     stream.on("end", () => console.log("Database: " + "Stream closed"));
        // })
        .withReuse()
        .start();
}

export async function startUpS3MockContainer() {
    return new GenericContainer("adobe/s3mock:latest")
        .withName("s3mock")
        .withExposedPorts(9090)
        .withEnvironment({
            initialBuckets: "supply-chain-action-plan",
            // debug: "true"
        })
        .withNetworkMode(networkName)
        // .withLogConsumer(stream => {
        //     stream.on("data", line => console.log("s3mock: " + line));
        //     stream.on("err", line => console.error("s3mock: " + line));
        //     stream.on("end", () => console.log("s3mock: " + "Stream closed"));
        // })
        .withReuse()
        .start();
}

export async function startUpClamAvContainer() {
    return new GenericContainer("clamav/clamav:stable")
        .withName("clamav")
        .withExposedPorts(3310)
        .withNetworkMode(networkName)
        // .withLogConsumer(stream => {
        //     stream.on("data", line => console.log("clamAv: " + line));
        //     stream.on("err", line => console.error("clamAv: " + line));
        //     stream.on("end", () => console.log("clamAv: " + "Stream closed"));
        // })
        .withReuse()
        .start();
}

async function getOrCreateNetwork() {
    const networks = await docker.listNetworks();

    const existingNetwork = networks.find((n) => n.Name === networkName);

    if (existingNetwork) {
        console.log(`Using existing network of name ${existingNetwork.Name}, id ${existingNetwork.Id}`);
        return docker.getNetwork(existingNetwork.Id);
    }

    console.log(`Creating new network of name ${networkName}`);
    return await docker.createNetwork({
        Name: networkName,
        CheckDuplicate: true
    });
}

export function stripLeadingSlashFromContainerName(containerName: string) {
    return containerName.slice(1);
}

async function getNodeContainer() {
    return (await (await getContainerRuntimeClient()).container.list()).find(container => container.Image.includes("node"));
}

export async function isRunningOnDrone() {
    return await getNodeContainer() || process.env.DRONE_BUILD_NUMBER;
}

export async function convertContainerHostnameToIpAddress(containerName: string) {
    // This pipes the hostname through DNS lookup and resolves it to an IP address
    const { address: hostname } = await dns.lookup(containerName);
    return hostname;
}
