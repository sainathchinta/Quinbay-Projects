# pcu-master-API

Pcu-Master-API is used as gateway service for the Internal portal used by the internal team.
This service provides all the APIs to manage category, attribute, size chart, restricted keyword etc.
This service interacts with downstream services to fetch category or attribute related details and update product level data for the managed data.
## Architecture Overview
- **Downstream Service**: `x-product, product-business-partner, product-category-base, business-partner`
## Jenkins Pipeline
The Jenkins pipeline is used for CI/CD operations for the Pcu Master API service.
- [Jenkins Build](https://jenkins-build-ci.gdn-app.com/job/MC/job/pcu-master-api/)
## API Documentation
Swagger links are available for various environments:
- [QA2](http://pcu-master-api.qa2-sg.cld/swagger-ui/index.html#/)
- [PREPROD](http://pcu-master-api.preprod-sg.cld/swagger-ui/index.html#/)
## Deployment Repo
- [QA2](https://stash.gdn-app.com/projects/NONPROD-SELLER-MASTER-CATALOG/repos/pcu-master-api/browse?at=refs%2Fheads%2Fqa2)
- [PREPROD](https://stash.gdn-app.com/projects/NONPROD-SELLER-MASTER-CATALOG/repos/pcu-master-api/browse?at=refs%2Fheads%2Fpreprod)
