# ChulaSSOMock

**ChulaSSOMock** is a mock authentication server that replicate the functionality of [https://account.it.chula.ac.th](https://account.it.chula.ac.th/). For more information on how ChulaSSO work, see [ChulaSSO Wiki](https://account.it.chula.ac.th/wiki/doku.php).

## Functionality

*ChulaSSOMock* can be used as a substitute to *ChulaSSO* when developing a service that need to authenticate Chula student.
*ChulaSSOMock* can offer following advantages :

- Reduced security risk. No need to distribute the real app secret.
- Impersonate any student's id and name
- Accurate behaviour

## Usage

Docker image is available at [Docker](https://hub.docker.com/r/pay2630/chulassomock)

1. Install docker
2. Download the image: `docker pull pay2630/chulassomock`
3. Run: `docker run pay2630/chulassomock`

## Configuration

The configuration is done by the following environment variables. (see [`docker run`](https://docs.docker.com/engine/reference/commandline/run/#set-environment-variables--e---env---env-file) on how to set it)

| Name  | Environment Variable | Default |
|-------|----------------------|---------|
| DeeAppId | APPID             | APPID   |
| DeeAppSecret | APPSECRET     | APPSECRET|
|Port   | PORT                 | 8080    |
