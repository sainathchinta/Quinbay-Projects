package com.gdn.x.product.rest.web.controller.api;

import static com.gdn.x.product.enums.Constants.REGULAR_MERCHANT_CLIENT_ID;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import com.gdn.x.product.model.vo.ItemPickupPointPriceVo;
import com.gdn.x.product.service.api.ItemPickupPointService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.apache.commons.collections.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.vo.DeleteOfflineItemVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.UpsertOfflineItemPriceResponseVO;
import com.gdn.x.product.service.util.ModelConverter;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.DeleteOfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.OfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.UpdateOfflineItemPriceRequest;
import com.gdn.x.product.rest.web.model.request.UpsertOfflineItemRequest;
import com.gdn.x.product.rest.web.model.response.DeleteOfflineItemResponse;
import com.gdn.x.product.rest.web.model.response.ItemTypeResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponseDetail;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.UpsertOfflineItemPriceResponse;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.OfflineItemService;
import com.gdn.x.product.service.api.ProductService;
import org.springframework.web.bind.annotation.RestController;


@RestController
@RequestMapping(value = ProductApiPath.OFFLINE_ITEM)
@Tag(name = "Offline Item Controller", description = "Offline Item Service API")
public class OfflineItemController {
  private static final Logger LOG = LoggerFactory.getLogger(OfflineItemController.class);

  @Autowired
  private OfflineItemService service;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private ModelConverter modelConverter;

  @Autowired
  private ItemService itemService;

  @Autowired
  private ProductService productService;

  @Deprecated
  @RequestMapping(value = {ProductApiPath.UPSERT_OFFLINE_ITEM}, method = {RequestMethod.POST}, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Upsert offline item", description = "update or insert offline item price to "
    + "prd_item_offline collection from MTA UI")
  public GdnRestListResponse<UpsertOfflineItemPriceResponse> upsertOfflineItem(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String merchantCode,
      @RequestBody List<UpsertOfflineItemRequest> upsertOfflineItemRequests) throws Exception {

    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, "");
    mandatoryRequestParam.setClientId(REGULAR_MERCHANT_CLIENT_ID);
    OfflineItemController.LOG.info(
        "#upsertOfflineItem - mandatoryRequestParam = {}, upsertOfflineItemRequests = {}, and merchantCode = {}",
        mandatoryRequestParam, upsertOfflineItemRequests, merchantCode);
    try {
      List<UpsertOfflineItemPriceResponseVO> result =
          this.service.upsertOfflineItem(mandatoryRequestParam, username, merchantCode, upsertOfflineItemRequests);
      List<UpsertOfflineItemPriceResponse> content =
          modelConverter.convertListToResponse(result, UpsertOfflineItemPriceResponse.class);
      return new GdnRestListResponse<>(content, new PageMetaData(1, 0, result.size()), requestId);
    } catch (Exception e) {
      LOG.error(
          "[FAILED] - #upsertOfflineItem - requestId = {}, merchantCode = {}, requests = {}, errorMessage = {}, error = ",
          requestId, merchantCode, upsertOfflineItemRequests, e.getMessage(), e);
      return new GdnRestListResponse<>(e.getMessage(), ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), false,
          requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = {ProductApiPath.UPSERT_OFFLINE_ITEM_PRICE}, method = {RequestMethod.POST},
      produces = {MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Upsert offline item price",
      description = "update or insert offline item price to prd_item_offline collection from X-Product-Integrator")
  public GdnRestListResponse<UpsertOfflineItemPriceResponse> upsertOfflineItemPrice(
      @RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String merchantCode,
      @RequestBody List<OfflineItemRequest> offlineItemRequest) throws Exception {

    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, "");

    OfflineItemController.LOG
        .info("Upsert offline item price with mandatoryRequestParam = {}, offlineItemRequest = {},"
            + "merchantCode = {}", mandatoryRequestParam, offlineItemRequest, merchantCode);

    try {
      List<UpsertOfflineItemRequest> upsertOfflineItemRequests =
          this.modelConverter.convertToOfflineItemList(offlineItemRequest, merchantCode, storeId);
      List<UpsertOfflineItemPriceResponseVO> result =
          this.service.upsertOfflineItem(mandatoryRequestParam, username, merchantCode, upsertOfflineItemRequests);
      List<UpsertOfflineItemPriceResponse> content =  modelConverter
          .convertListToResponse(result, UpsertOfflineItemPriceResponse.class);
      return new GdnRestListResponse<>(content, new PageMetaData(1, 0, result.size()), requestId);
    } catch (Exception e) {
      LOG.error("#upsertOfflineItemPrice merchantCode = {}, request = {}, failed with error message= {}, error = {}",
          merchantCode, offlineItemRequest, e.getMessage(), e);
      return new GdnRestListResponse<>(e.getMessage(), ProductErrorCodesEnum.INTERNAL_SERVER.getCode(),
          false, requestId);
    }
  }

  @RequestMapping(
      value = {ProductApiPath.DELETE_OFFLINE_ITEM}, method = {RequestMethod.POST},
      produces = {MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(
      summary = "delete offline item",
      description = "Called by PBP for MTA - CnC Product List per SKU & per Location")
  public GdnBaseRestResponse deleteOfflineItem(
      @RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String merchantCode,
      @RequestBody List<DeleteOfflineItemRequest> deleteOfflineItemRequests) {
    LOG.info("Delete offline items with storeId = {}, merchantCode = {}, deleteOfflineItemRequests = {},",
        new Object[] {storeId, merchantCode, deleteOfflineItemRequests});

    try {
      this.itemPickupPointService.delete(storeId, merchantCode, deleteOfflineItemRequests, username);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e){
      LOG.error("#deleteOfflineItem merchantCode = {}, deleteOfflineItemRequests = {}",
          merchantCode, deleteOfflineItemRequests, e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.INTERNAL_SERVER.getCode(),
          false, requestId);
    }
  }

  @RequestMapping(
      value = {ProductApiPath.BULK_DELETE_OFFLINE_ITEM}, method = {RequestMethod.POST},
      produces = {MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(
      summary = "bulk delete offline item with returning succeeded and failed offline items",
      description = "Called by PBP for MTA - CnC Product delete by excel sheet")
  public GdnRestListResponse<DeleteOfflineItemResponse> bulkDeleteOfflineItem(
      @RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String merchantCode,
      @RequestBody List<DeleteOfflineItemRequest> deleteOfflineItemRequests) {
    LOG.info("Bulk delete offline items with storeId = {}, merchantCode = {}, deleteOfflineItemRequests = {},",
        storeId, merchantCode, deleteOfflineItemRequests);

    try {
      List<DeleteOfflineItemVO> offlineItemToDelete = deleteOfflineItemRequests.stream()
          .map(request -> DeleteOfflineItemVO.builder()
              .itemSku(request.getItemSku()).pickupPointCode(request.getPickupPointCode()).build())
          .collect(Collectors.toList());
      List<DeleteOfflineItemVO> vos = itemPickupPointService.bulkDelete(storeId, merchantCode, offlineItemToDelete,
        username);
      List<DeleteOfflineItemResponse> content =
          modelConverter.convertListToResponse(vos, DeleteOfflineItemResponse.class);
      return new GdnRestListResponse<>(content, new PageMetaData(1, 0, vos.size()), requestId);
    } catch (Exception e) {
      LOG.error("[FAILED] #deleteOfflineItem merchantCode = {}, deleteOfflineItemRequests = {}",
          merchantCode, deleteOfflineItemRequests, e);
      return new GdnRestListResponse<>(e.getMessage(),
          ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), false, requestId);
    }
  }

  @Deprecated
  @RequestMapping(
      value = {ProductApiPath.OFFLINE_ITEM_PUBLISH_IN_BATCH}, method = {RequestMethod.GET},
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(
      summary = "publish all offline items in batch / by item skus if specified",
      description = "only to initialize pre-existing offline items to aggregate data platform, if failed then will publish "
          + "from failing point (offset) stored in system parameter")
  public GdnBaseRestResponse publishOfflineItem(
      @RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam int batchSize,
      @RequestParam(required = false) List<String> itemSkus) {
    LOG.info("Publish offline items with storeId = {}, batchSize = {}, itemSkus = {}",
        new Object[] {storeId, batchSize, itemSkus});

    try {
      this.service.publishOfflineItems(storeId, batchSize, itemSkus);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e){
      LOG.error("#publishOfflineItem batchSize = {}", batchSize, e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.INTERNAL_SERVER.getCode(),
          false, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = {
      ProductApiPath.OFFLINE_ITEM_REPUBLISH_BY_MERCHANT_CODES_IN_BATCH}, method = {
      RequestMethod.POST}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "republish all offline items in batch by merchant codes") @ResponseBody
  public GdnBaseRestResponse republishOfflineItemByMerchantCodes(
      @RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam int batchSize,
      @RequestBody SimpleListStringRequest merchantCodes) {
    LOG.info(
        "Republish offline items by merchant codes with storeId = {}, batchSize = {}, merchantCodes = {}",
        storeId, batchSize, merchantCodes);

    try {
      this.service
          .republishOfflineItemsByMerchantCodes(storeId, batchSize, merchantCodes.getValue());
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      LOG.error("#republishOfflineItemByMerchantCodes batchSize = {}", batchSize, e);
      return new GdnBaseRestResponse(e.getMessage(),
          ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), false, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.FIND_BY_MERCHANT_CODE_AND_MERCHANT_SKUS,
      method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "find Product by Merchant Code and Merchant SKU",
      description = "used by PBP to find if the product is exist or not")
  public GdnRestSingleResponse<OfflineItemResponse> findByMerchantCodeAndMerchantSkus(
      @RequestParam String storeId, @RequestParam String requestId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam(required = false) String username, @RequestParam String merchantCode,
      @RequestBody SimpleListStringRequest merchantSkuRequest) {

    List<OfflineItemResponseDetail> response = new ArrayList<>();
    List<String> merchantSkus = merchantSkuRequest.getValue();

    try{
      List<Item> items = itemService.getItemsByMerchantCodeAndMerchantSkus(
          storeId, requestId, username, merchantCode, merchantSkus);

      Map<String, ProductType> productTypeByProductSkuMap = new HashMap<>();

      if (CollectionUtils.isNotEmpty(items)) {
        Set<String> productSkus = items.stream().map(Item::getProductSku)
            .collect(Collectors.toSet());

        productService.getProducts(storeId, productSkus).forEach(
            product -> productTypeByProductSkuMap.put(product.getProductSku(), product.getProductType()));
      }

      response = modelConverter.convertItemToOfflineProductResponse(merchantSkus, items, productTypeByProductSkuMap);
      return new GdnRestSingleResponse<OfflineItemResponse>(new OfflineItemResponse(response), requestId);

    } catch (Exception e) {
      LOG.error("#findByMerchantCodeAndMerchantSkus of merchant code = {}, failed with error = {}",
          merchantCode, e.getMessage(), e);
      return new GdnRestSingleResponse<OfflineItemResponse>(e.getMessage(),
          ProductErrorCodesEnum.FIND_BY_MERCHANT_CODE_AND_MERCHANT_SKUS.getCode(), false,
          null, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.FIND_BY_MERCHANT_CODE_AND_ITEM_SKU, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "find offline price by Merchant Code and Item SKU",
      description = "Called by PBP for MTA - CnC Product List per SKU detail")
  public GdnRestListResponse<OfflineItemPriceResponse> findOfflinePriceByMerchantCodeAndItemSku(
      @RequestParam String storeId, @RequestParam String requestId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam(required = false) String username, @RequestParam String merchantCode,
      @RequestParam String itemSku) {

    try {
      List<ItemPickupPointPriceVo> itemPickupPointPriceVoList =
        service.findItemPickupPointByMerchantCodeAndItemSku(storeId, merchantCode, itemSku);

      List<OfflineItemPriceResponse> responses = modelConverter.convertListToResponse(
        itemPickupPointPriceVoList, OfflineItemPriceResponse.class);

      return new GdnRestListResponse<>(responses, new PageMetaData(
          1, 0, responses.size()), requestId);
    } catch (Exception e) {
      LOG.error("#findOfflinePriceByMerchantCodeAndItemSku of merchant code = {}, itemSku = {}",
          merchantCode, itemSku, e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.FIND_ALL_OFFLINE_ITEMS_OF_MULTIPLE_MERCHANTS, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(
      summary = "Find all offline items of multiple merchants",
      description = "Called by Pyeongyang Product for CnC Location Popup in Product Detail")
  public GdnRestListResponse<OfflineItemPriceResponse> findAllOfflineItemsOfMultipleMerchants(
      @RequestParam String storeId, @RequestParam String requestId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam(required = false) String username,
      @RequestParam String itemSku) {
    try {
      List<ItemPickupPointPriceVo> itemPickupPointPriceVoList =
        service.findMultipleMerchantsOfflineItemByItemSku(storeId, itemSku);

      List<OfflineItemPriceResponse> responses = modelConverter.convertListToResponse(
          itemPickupPointPriceVoList, OfflineItemPriceResponse.class);

      return new GdnRestListResponse<>(responses, new PageMetaData(
          1, 0, responses.size()), requestId);
    } catch (Exception e) {
      LOG.error("error findAllOfflineItemsOfMultipleMerchants itemSku = {}", itemSku, e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.FIND_ALL_OFFLINE_ITEMS_OF_SPECIFIC_MERCHANT, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(
      summary = "Find all offline items of specific merchant",
      description = "Called by Pyeongyang Product for CnC Location Popup in Checkout Page")
  @ResponseBody
  public GdnRestListResponse<OfflineItemPriceResponse> findAllOfflineItemsOfSpecificMerchant(
      @RequestParam String storeId, @RequestParam String requestId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam(required = false) String username,
      @RequestParam String itemSku) {
    try {
      Set<String> itemSkuSet = new HashSet<>(Arrays.asList(itemSku));
      List<OfflineItem> offlineItems = service.findByItemSkusAndMarkForDeleteFalseWithLimit(storeId, itemSkuSet);

      List<OfflineItemPriceResponse> responses = modelConverter.convertListToResponse(
          offlineItems, OfflineItemPriceResponse.class);

      return new GdnRestListResponse<>(responses, new PageMetaData(
          1, 0, responses.size()), requestId);
    } catch (Exception e) {
      LOG.error("error findAllOfflineItemsOfSpecificMerchant itemSku = {}", itemSku, e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.FIND_BY_PRODUCT_SKUS, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "find offline price by List of product SKU",
      description = "Siva to get offline product info for product set")
  @ResponseBody
  public GdnRestListResponse<ProductAndItemsResponse> findOfflineItemsByProductSkus(
      @RequestParam String storeId, @RequestParam String requestId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam(required = false) String username,
      @RequestBody List<String> productSkus) {

    try {
      List<ProductAndItemsVO> productAndItemsVOs =
          service.findOfflineItemsByProductSkus(storeId, username, requestId, productSkus);
      List<ProductAndItemsResponse> responses =
          modelConverter.convertToProductAndItemsDTOs(productAndItemsVOs);

      return new GdnRestListResponse<>(responses,
          new PageMetaData(responses.size(), 0, responses.size()), requestId);

    } catch (Exception e) {
      LOG.error("#findOfflineItemByProductSkus with list Product SKUs : {}, error : {}",
          productSkus, e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = ProductApiPath.FIND_BY_OFFLINE_ITEM_ID, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "find offline price by offline item id")
  @ResponseBody
  public GdnRestSingleResponse<OfflineItemPriceResponse> findOfflinePriceByOfflineItemId(
      @RequestParam String storeId, @RequestParam String requestId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam(required = false) String username, @RequestParam String offlineItemId) {

    try {
      ItemPickupPoint itemPickupPoint = itemPickupPointService.findByStoreIdAndOfflineItemId(storeId, offlineItemId);
      OfflineItemPriceResponse response =
          modelConverter.convertToResponse(itemPickupPoint, OfflineItemPriceResponse.class);
      response.setListPrice(itemPickupPoint.getPrice().stream().findFirst().get().getListPrice());
      response.setOfferPrice(itemPickupPoint.getPrice().stream().findFirst().get().getOfferPrice());

      return new GdnRestSingleResponse<>(response, requestId);
    } catch (Exception e) {
      LOG.error("#findOfflinePriceByOfflineItemId offline item id = {}, {}",
          offlineItemId, e);
      return new GdnRestSingleResponse<>(ProductErrorCodesEnum.FIND_OFFLINE_ITEM_PRICE_BY_OFFLINE_ITEM_ID.getMessage(),
          ProductErrorCodesEnum.FIND_OFFLINE_ITEM_PRICE_BY_OFFLINE_ITEM_ID.getCode(), false,
          null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.CHECK_UNIQUE_ID_TYPE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(
      summary = "check uniqueId type",
      description = "Check if the requested unique Id is online or offline item, called by PY Product")
  @ResponseBody
  public GdnRestSingleResponse<ItemTypeResponse> checkUniqueIdType(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String uniqueId) {
    try {
      OfflineItemController.LOG.info("checkUniqueIdType uniqueId = {}", uniqueId);
      boolean isOfflineItem = service.isOfflineItem(storeId, uniqueId);
      ItemTypeResponse response = ItemTypeResponse.builder().offlineItem(isOfflineItem).build();
      return new GdnRestSingleResponse<>(null, null, true, response, requestId);
    } catch (ApplicationRuntimeException e) {
      LOG.error("#checkUniqueIdType uniqueId = {}, {}", uniqueId, e.getMessage());
      ItemTypeResponse response = ItemTypeResponse.builder().offlineItem(false).build();
      return new GdnRestSingleResponse<>(ProductErrorCodesEnum.CHECK_UNIQUE_ID_TYPE.getMessage(),
          ProductErrorCodesEnum.CHECK_UNIQUE_ID_TYPE.getCode(), false, response, requestId);
    } catch (Exception e) {
      LOG.error("#checkUniqueIdType uniqueId = {}", uniqueId, e);
      ItemTypeResponse response =ItemTypeResponse.builder().offlineItem(false).build();
      return new GdnRestSingleResponse<>(ProductErrorCodesEnum.CHECK_UNIQUE_ID_TYPE.getMessage(),
          ProductErrorCodesEnum.CHECK_UNIQUE_ID_TYPE.getCode(), false, response, requestId);
    }
  }

  @Deprecated
  @RequestMapping(value = {ProductApiPath.UPDATE_OFFLINE_ITEM_PRICE_BY_ITEM_SKU},
      method = {RequestMethod.POST}, produces = {MediaType.APPLICATION_JSON_VALUE},
      consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Update offline item price by item sku",
      description = "Update offline item listPrice and offerPrice to prd_item_offline collection by itemSku")
  @ResponseBody
  public GdnBaseRestResponse updateOfflineItemPriceByItemSku(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String merchantCode,
      @RequestBody UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest) throws Exception {
    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, "");
    LOG.info(
        "#OfflineItemController-updateOfflineItemPriceByItemSku - mandatoryRequestParam: {}, merchantCode: {},"
            + "updateOfflineItemPriceRequest: {}",
        mandatoryRequestParam, updateOfflineItemPriceRequest, merchantCode);
    try {
      this.service.updateOfflineItemPriceByItemSku(mandatoryRequestParam, merchantCode, updateOfflineItemPriceRequest);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      LOG.error(
          "[FAILED] - #OfflineItemController-updateOfflineItemPriceByItemSku - mandatoryRequestParam: {}, merchantCode: {} and updateOfflineItemPriceRequest: {} - error message: {} and error: ",
          mandatoryRequestParam, merchantCode, updateOfflineItemPriceRequest, e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(),
          ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), false, requestId);
    }
  }
}
