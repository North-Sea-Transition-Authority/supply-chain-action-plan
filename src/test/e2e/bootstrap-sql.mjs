import pg from 'pg'
const {Client} = pg
import fs from 'fs'

const client = new Client({
  user: 'scap_app',
  host: 'e2e-db',
  database: 'scap',
  password: 'dev1',
  port: 5432,
})
await client.connect()

const sql = fs.readFileSync('../../../devtools/create_dev_users.sql').toString();

try {
  const res = await client.query(sql)
  console.log(res)
} catch (err) {
  console.error(err);
} finally {
  await client.end()
}
