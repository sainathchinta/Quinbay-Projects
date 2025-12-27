package com.gdn.x.productcategorybase.controller;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.productcategorybase.ProductApiPath;
import com.gdn.x.productcategorybase.dto.request.OmniChannelSkuRequest;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;
import com.gdn.x.productcategorybase.service.ProductService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@RequestMapping(value = ProductApiPath.V2_BASE_PATH)
@Tag(name = "ProductV2Controller", description = "Product V2 API")
public class ProductV2Controller {

  @Autowired
  private ProductService productService;

  @PostMapping(value =
                   ProductApiPath.CHECK_OMNI_CHANNEL_SKU_EXISTS_OR_NOT_BY_SELLER_CODE_AND_SKU_LIST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get duplicate product names or by upc code", description =
      "get duplicate product items starting or ending with product name or upc code and "
          + "pageable")
  public GdnRestSingleResponse<ValidOmniChannelSkuResponse> checkOmniChannelSkuExistsInSeller(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(required = false, defaultValue = "false") boolean needUomInfo,
      @RequestBody OmniChannelSkuRequest omniChannelSkuRequest) {
    log.info("Checking if omni channel SKU exists for seller code: {} and omniChannelSkus: {} ",
        omniChannelSkuRequest.getSellerCode(), omniChannelSkuRequest.getOmniChannelSkus());
    return new GdnRestSingleResponse<>(null, null, true,
        productService.checkOmniChannelSkuExistsInSeller(storeId, omniChannelSkuRequest, needUomInfo),
        requestId);
  }
}
