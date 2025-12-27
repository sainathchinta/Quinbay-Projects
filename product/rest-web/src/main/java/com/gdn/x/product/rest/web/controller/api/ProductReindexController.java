package com.gdn.x.product.rest.web.controller.api;


import java.util.List;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.product.model.vo.OfflineItemDetailVo;
import com.gdn.x.product.model.vo.SimpleMasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.service.util.ModelConverter;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.SimpleSetStringRequest;
import com.gdn.x.product.rest.web.model.response.OfflineItemDetailResponse;
import com.gdn.x.product.rest.web.model.response.SimpleProductAndItemsMasterDataDetailResponse;
import com.gdn.x.product.service.api.ProductReindexingService;
import org.springframework.web.bind.annotation.RestController;


@RestController
@RequestMapping(value = ProductApiPath.PRODUCT_REINDEX)
@Tag(name = "Product Reindexing", description = "Product Reindexing. Search will use.")
public class ProductReindexController {

  @Autowired
  private ModelConverter modelConverter;

  @Autowired
  private ProductReindexingService productReindexingService;

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductReindexController.class);


  @RequestMapping(value = ProductApiPath.FULL_REINDEX_BY_PRODUCT_CODES, method = RequestMethod
      .POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType
      .APPLICATION_JSON_VALUE)
  public GdnRestSingleResponse<SimpleProductAndItemsMasterDataDetailResponse>
  fullReindexByProductCodes(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody SimpleSetStringRequest productCodes) throws Exception {
    try {
      SimpleMasterDataDetailWithProductAndItemsResponseVo result = this.productReindexingService
          .getMasterDataProductDetailResponseByProductCodes(storeId, username, requestId,
              productCodes.getValue());
      SimpleProductAndItemsMasterDataDetailResponse response =
          modelConverter.toSimpleProductAndItemsMasterDataDetailResponse(result);
      loggerFunction(response, String.valueOf(productCodes.getValue()));
      return new GdnRestSingleResponse<>(null, null, true, response, requestId);
    } catch (ApplicationRuntimeException e) {
      LOGGER.error("#fullReindexByProductCodes with productCodes : {}", productCodes, e);
      return new GdnRestSingleResponse<>(
          e.getMessage(), ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null,
          requestId);
    } catch (Exception e) {
      LOGGER.error("#fullReindexByProductCodes with productCodes : {} ", productCodes, e);
      return new GdnRestSingleResponse<>(
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.FULL_REINDEX_BY_PRODUCT_SKUS, method =
      RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE) @ResponseBody
  public GdnRestSingleResponse<SimpleProductAndItemsMasterDataDetailResponse> fullReindexByProductSkus(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody SimpleSetStringRequest productSkus) throws Exception {
    try {
      SimpleMasterDataDetailWithProductAndItemsResponseVo result = this.productReindexingService
          .getMasterDataProductDetailResponseByProductSkus(storeId, username, requestId,
              productSkus.getValue());
      SimpleProductAndItemsMasterDataDetailResponse response =
          modelConverter.toSimpleProductAndItemsMasterDataDetailResponse(result);
      loggerFunction(response, String.valueOf(productSkus.getValue()));
      return new GdnRestSingleResponse<>(null, null, true, response, requestId);
    } catch (ApplicationRuntimeException e) {
      LOGGER.error("#fullReindexByProductSkus with productSkus : {}", productSkus, e);
      return new GdnRestSingleResponse<>(
          e.getMessage(), ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null,
          requestId);
    } catch (Exception e) {
      LOGGER.error("#fullReindexByProductSkus with productSkus : {}", productSkus, e);
      return new GdnRestSingleResponse<>(
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.FULL_REINDEX_BY_ITEM_SKU, method = RequestMethod.GET,
                  produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSingleResponse<SimpleProductAndItemsMasterDataDetailResponse> fullReindexByItemSku(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String itemSku, @RequestParam(required = false) boolean instantPickup,
      @RequestParam(required = false) String pickupPointCode) throws Exception {
    try {
      SimpleMasterDataDetailWithProductAndItemsResponseVo result = this.productReindexingService
          .getMasterDataProductDetailResponseByItemSku(storeId, username, requestId, itemSku, instantPickup, pickupPointCode);
      SimpleProductAndItemsMasterDataDetailResponse response =
          modelConverter.toSimpleProductAndItemsMasterDataDetailResponse(result);
      loggerFunction(response, itemSku);
      return new GdnRestSingleResponse<>(null, null, true, response, requestId);
    } catch (ApplicationRuntimeException e) {
      LOGGER.error("#fullReindexByItemSku with itemSku: {}", itemSku, e);
      return new GdnRestSingleResponse<>(
          e.getMessage(), ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null,
          requestId);
    } catch (Exception e) {
      LOGGER.error("#fullReindexByItemSku with itemSku : {}", itemSku, e);
      return new GdnRestSingleResponse<SimpleProductAndItemsMasterDataDetailResponse>(
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getMessage(),
          ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_OFFLINE_ITEMS_BY_ITEM_SKU, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestListResponse<OfflineItemDetailResponse> getOfflineItemsByItemSku(
      @PathVariable("itemSku") String itemSku, @RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(required = false, defaultValue = "0") int page,
      @RequestParam(required = false, defaultValue = "100") int size) throws Exception {
    try {
      Page<OfflineItemDetailVo> offlineItemDetailVoPage = this.productReindexingService
          .getOfflineItemsByItemSku(storeId, username, requestId, itemSku, PageRequest.of(page,
        size));
      List<OfflineItemDetailResponse> response =
          modelConverter.toOfflineItemDetailResponse(offlineItemDetailVoPage.getContent());
      loggerFunctionForOfflineItem(response, itemSku);
      return new GdnRestListResponse<>(response,
          new PageMetaData(offlineItemDetailVoPage.getSize(), offlineItemDetailVoPage.getNumber(),
              offlineItemDetailVoPage.getTotalElements()), requestId);
    } catch (ApplicationRuntimeException e) {
      LOGGER.error("#getOfflineItemsByItemSku with itemSku: {}", itemSku, e);
      return new GdnRestListResponse<>(e.getMessage(),
          ProductErrorCodesEnum.GET_OFFLINE_ITEMS_BY_ITEM_SKU.getCode(), false, requestId);
    } catch (Exception e) {
      LOGGER.error("#getOfflineItemsByItemSku with itemSku : {}", itemSku, e);
      return new GdnRestListResponse<>(ProductErrorCodesEnum.GET_OFFLINE_ITEMS_BY_ITEM_SKU.getMessage(),
          ProductErrorCodesEnum.GET_OFFLINE_ITEMS_BY_ITEM_SKU.getCode(), false, requestId);
    }
  }

  private void loggerFunction(SimpleProductAndItemsMasterDataDetailResponse response, String key) {
    try {
      response.getProductAndItems().forEach(simpleProductAndItemsDTO -> {
        simpleProductAndItemsDTO.getSimpleItems().forEach(simpleItemResponseDTO -> {
          LOGGER.info(
              "Response from Reindexing API for itemSku : {}, itemViewConfigs : {}, price : {}, activePromoBundlings : {}, pristineData : {}",
              simpleItemResponseDTO.getItemSku(), simpleItemResponseDTO.getItemViewConfigs(),
              simpleItemResponseDTO.getPrices(), simpleItemResponseDTO.getActivePromoBundlings(),
              simpleItemResponseDTO.getSimplePristineDataItem());
        });
      });
    } catch (Exception e) {
      LOGGER.error("Exception caught while writing logs for key : {}", key, e);
    }
  }

  private void loggerFunctionForOfflineItem(List<OfflineItemDetailResponse> response, String key) {
    try {
      response.forEach(items -> {
        LOGGER
            .info("Response from Reindexing API for itemSku : {}, price : {}, viewConfigs : {}", key, items.getPrices(),
                items.getItemViewConfigs());
      });
    } catch (Exception e) {
      LOGGER.error("Exception caught while writing logs for getOfflineItems for itemSku: {}", key, e);
    }
  }

}
