package com.gdn.x.product.rest.web.controller.api;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.model.vo.SimpleMasterDataDetailWithProductAndItemsV2ResponseVo;
import com.gdn.x.product.service.util.ModelConverter;
import com.gdn.x.product.rest.web.model.ProductReindexV2ApiPath;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.ProductAndItemSkusRequest;
import com.gdn.x.product.rest.web.model.response.SimpleProductAndItemsMasterDataDetailV2Response;
import com.gdn.x.product.service.api.ProductReindexingService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@RequestMapping(value = ProductReindexV2ApiPath.BASE_PATH)
@Tag(name = "ProductReindexV2 Controller", description = "Product Reindex V2 Service API")
public class ProductReindexV2Controller {

  @Autowired
  private ModelConverter modelConverter;

  @Autowired
  private ProductReindexingService productReindexingService;

  @RequestMapping(value = ProductReindexV2ApiPath.FULL_REINDEX_BY_ITEM_SKU_AND_PICKUP_POINT, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Full Reindex by Item Sku and PP code")
  public GdnRestSingleResponse<SimpleProductAndItemsMasterDataDetailV2Response> fullReindexByItemSkuAndPickupPointCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String itemSku, @RequestParam String pickupPointCode,
      @RequestParam(required = false) String fetchViewConfigByChannel) throws Exception {
    try {
      SimpleMasterDataDetailWithProductAndItemsV2ResponseVo result = this.productReindexingService
          .getMasterDataProductDetailV2ResponseByItemSkuAndPickupPointCode(storeId, username, requestId, itemSku, pickupPointCode, fetchViewConfigByChannel);
      SimpleProductAndItemsMasterDataDetailV2Response response =
          modelConverter.toSimpleProductAndItemsMasterDataDetailV2Response(result);
      loggerFunction(response, itemSku);
      return new GdnRestSingleResponse<>(null, null, true, response, requestId);
    } catch (ApiIncorrectInputDataException e) {
      return new GdnRestSingleResponse<>(e.getErrorMessage(), e.getErrorCode(), false, null, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("#fullReindexByItemSku with itemSku: {} ", itemSku, e);
      return new GdnRestSingleResponse<>(
          e.getMessage(), ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null,
          requestId);
    } catch (Exception e) {
      log.error("#fullReindexByItemSku with itemSku : {} ", itemSku, e);
      return new GdnRestSingleResponse<SimpleProductAndItemsMasterDataDetailV2Response>(
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductReindexV2ApiPath.ITEM_PICKUP_POINT, method =
      RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "full Reindex ByProduct Or ItemSkus")
  public GdnRestSingleResponse<SimpleProductAndItemsMasterDataDetailV2Response> fullReindexByProductOrItemSkus(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody ProductAndItemSkusRequest productAndItemSkusRequest, @RequestParam int page,
      @RequestParam(defaultValue = "10") int size, @RequestParam(defaultValue = "false") boolean showDeleted,
      @RequestParam(required = false) String fetchViewConfigByChannel)
      throws Exception {
    try {
      SimpleMasterDataDetailWithProductAndItemsV2ResponseVo result = this.productReindexingService
          .getMasterDataProductDetailV2ResponseByProductSkusOrItemSku(storeId, username, requestId,
              productAndItemSkusRequest.getProductSkus(), productAndItemSkusRequest.getItemSkus(), showDeleted, page, size, fetchViewConfigByChannel);
      SimpleProductAndItemsMasterDataDetailV2Response response =
          modelConverter.toSimpleProductAndItemsMasterDataDetailV2Response(result);
      loggerFunction(response, String.valueOf(productAndItemSkusRequest.getItemSkus() +
              String.valueOf(productAndItemSkusRequest.getProductSkus())));
      response.setPageMetaData(new PageMetaData(size, page, result.getTotalL5Count()));
      return new GdnRestSingleResponse<>(null, null, true, response, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("#fullReindexByProductSkus with productOrItemSkus : {}", productAndItemSkusRequest, e);
      return new GdnRestSingleResponse<>(
          e.getMessage(), ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null,
          requestId);
    } catch (Exception e) {
      log.error("#fullReindexByProductSkus with productOrItemSkus : {}", productAndItemSkusRequest, e);
      return new GdnRestSingleResponse<>(
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    }
  }

  private void loggerFunction(SimpleProductAndItemsMasterDataDetailV2Response response, String key) {
    try {
      response.getProductAndItems().forEach(simpleProductAndItemsDTO -> {
        simpleProductAndItemsDTO.getSimpleItems().forEach(simpleItemResponseDTO -> {
          log.info(
              "Response from Reindexing API for itemSku : {}, pristineData : {}",
              simpleItemResponseDTO.getItemSku(),
              simpleItemResponseDTO.getSimplePristineDataItem());
        });
      });
      response.getProductAndItems().forEach(simpleProductAndItemsDTO -> {
        simpleProductAndItemsDTO.getItemPickupPoints().forEach(itemPickupPointResponseDTO -> {
          log.info("Response from Reindexing API for itemSku : {}, pickupPointCode : {}, price : {},"
                  + " itemViewConfig : {}, activePromoBundlings : {}",
              itemPickupPointResponseDTO.getItemSku(), itemPickupPointResponseDTO.getPickupPointCode(),
              itemPickupPointResponseDTO.getPrices(), itemPickupPointResponseDTO.getItemViewConfigs(),
              itemPickupPointResponseDTO.getActivePromoBundlings());
        });
      });
    } catch (Exception e) {
      log.error("Exception caught while writing logs for key : {} ", key, e);
    }
  }

}
