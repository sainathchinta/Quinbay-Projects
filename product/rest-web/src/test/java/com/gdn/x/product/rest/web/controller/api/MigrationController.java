package com.gdn.x.product.rest.web.controller.api;

import java.util.Date;
import java.util.List;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.service.api.migration.MigrationService;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = "migration")
@Tag(name = "Migration Controller",
    description = "Migration Service API (Need extra caution on executing this API)")
public class MigrationController {

  private static final Logger LOGGER = LoggerFactory.getLogger(MigrationController.class);

  @Autowired
  private MigrationService migrationService;


  @RequestMapping(value = {"attribute"}, method = {RequestMethod.GET},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  public GdnBaseRestResponse fixAttribute(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String fileName) throws Exception {
    MigrationController.LOGGER.error("start fixing attribute {}", new Date());
    this.migrationService.fixAttributeData(requestId, fileName);
    MigrationController.LOGGER.error("end fixing attribute {}", new Date());
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = ProductApiPath.GENERATE_SPEC_DETAIL, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "generate product spec detail", description = "generate product spec detail")
  public GdnBaseRestResponse generateSpecificationDetail(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody List<String> productSkus) {
    MigrationController.LOGGER.info("generate product spec detail with productSku = {}",
        new Object[] {productSkus});

    try {
      this.migrationService.generateSpecificationDetail(storeId, productSkus);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      MigrationController.LOGGER.error("#generateProductSpec with productSku {} , {}", productSkus,
          e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(),
          ProductErrorCodesEnum.DELETE_PRODUCT.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = {"product"}, method = {RequestMethod.GET},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnBaseRestResponse migrateProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String fileName) throws Exception {
    MigrationController.LOGGER.error("start migrating {}", new Date());
    this.migrationService.migrateProductData(fileName, 0);
    MigrationController.LOGGER.error("end migrating {}", new Date());
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = {"ticket"}, method = {RequestMethod.GET},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnBaseRestResponse migrateTicketTemplate(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username)
      throws Exception {
    this.migrationService.migrateTicketTemplate();
    return new GdnBaseRestResponse(true);
  }

}
