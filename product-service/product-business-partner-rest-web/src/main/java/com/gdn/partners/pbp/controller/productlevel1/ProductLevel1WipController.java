package com.gdn.partners.pbp.controller.productlevel1;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pbp.dto.productlevel1.ProductLevel1WipDeleteRequest;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3WipServiceBean;
import com.gdn.partners.pbp.web.model.ProductLevel1WipControllerErrorMessage;
import com.gdn.partners.pbp.web.model.ProductLevel1WipControllerPath;
import com.gdn.partners.pbp.workflow.product.ProductWfService;

import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;

@RestController
@RequestMapping(value = ProductLevel1WipControllerPath.BASE_PATH)
@Tag(name = "Product Level 1 Wip", description = "Product Level 1 Wip Service API")
public class ProductLevel1WipController {
  private static final Logger LOGGER = LoggerFactory.getLogger(ProductLevel3WipServiceBean.class);

  @Autowired
  private ProductWfService productWfService;

  @RequestMapping(value = ProductLevel1WipControllerPath.DELETE_BY_PRODUCT_CODES,
      method = RequestMethod.POST, consumes = {MediaType.APPLICATION_JSON_VALUE},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "delete by product codes", description = "delete wip by product codes")
  @ResponseBody
  public GdnBaseRestResponse deleteByProductCodes(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody ProductLevel1WipDeleteRequest request)
      throws Exception {
    ProductLevel1WipController.LOGGER.info(
        "Invoke deleteByProductCodes with mandatoryParameter= {} and ProductLevel1WipDeleteRequest= {}",
        MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId,
            username, null),
        request);
    GdnPreconditions.checkArgument(!CollectionUtils.isEmpty(request.getProductCodes()),
        ProductLevel1WipControllerErrorMessage.PRODUCT_CODES_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getNotes()),
        ProductLevel1WipControllerErrorMessage.NOTES_MUST_NOT_BE_BLANK);
    this.productWfService.delete(request.getProductCodes(), request.getNotes());
    return new GdnBaseRestResponse(requestId);
  }
}
