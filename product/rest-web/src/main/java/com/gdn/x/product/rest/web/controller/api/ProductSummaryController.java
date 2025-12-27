package com.gdn.x.product.rest.web.controller.api;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.x.product.model.vo.ItemPickupPointListingUpdateRequestVo;
import com.gdn.x.product.service.util.ModelConverter;
import com.gdn.x.product.rest.web.model.ProductSummaryControllerPath;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingUpdateRequest;
import com.gdn.x.product.service.api.ItemPickupPointSummaryService;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping(value = ProductSummaryControllerPath.SUMMARY_V2)
@Tag(name = "Summary Controller", description = "Product And Item Pickup Point Summary APIs")
@Slf4j
public class ProductSummaryController {

  @Autowired
  private ModelConverter modelConverter;

  @Autowired
  private ItemPickupPointSummaryService pickupPointSummaryService;

  @RequestMapping(value = {
    ProductSummaryControllerPath.LISTING_UPDATE}, method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse updateItemPickupPointListing(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username, @PathVariable("productSku") String productSku,
    @RequestBody ItemPickupPointListingUpdateRequest pickupPointListingUpdateRequest) throws Exception {
    log.info("Updating item pickupPoint listing for productSku {} request {}", productSku,
      new Object[] {pickupPointListingUpdateRequest});
    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, username);
    List<ItemPickupPointListingUpdateRequestVo> requests = modelConverter
      .convertToItemPickupPointListingUpdateRequestVo(pickupPointListingUpdateRequest);
    try {
      pickupPointSummaryService.validateAndUpdateItemPickupPointListing(mandatoryRequestParam,
          productSku, pickupPointListingUpdateRequest.getProductType(), requests);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception ex) {
      log.error("Error on updating item pickupPoint listing {} {} {}", productSku,
        new Object[] {pickupPointListingUpdateRequest}, ex);
      return new GdnBaseRestResponse(ex.getMessage(), ProductErrorCodesEnum.UPDATE_ITEM.getCode(),
        false, requestId);
    }
  }
}
