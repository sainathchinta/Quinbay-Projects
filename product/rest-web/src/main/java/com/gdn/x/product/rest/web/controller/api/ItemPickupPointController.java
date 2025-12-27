package com.gdn.x.product.rest.web.controller.api;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.rest.web.model.request.AutoCreatePickupPointRequestList;
import com.gdn.x.product.rest.web.model.request.CreateFbbPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.DeleteItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.EanUpcPickupPointCodeRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.SimpleSetStringRequest;
import com.gdn.x.product.rest.web.model.response.AutoCreatePickupPointListResponse;
import com.gdn.x.product.rest.web.model.response.CreateFbbPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.DeleteItemPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.EanUpcPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointBasicResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointL5Response;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointPriceResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.PriceUpdatedInTimeRangeL5Response;
import com.gdn.x.product.rest.web.model.response.ProductL5DetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductSkuPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ProductSkuPickupPointResponseV2;
import com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse;
import com.gdn.x.product.rest.web.model.response.ViewConfigResponse;
import io.lettuce.core.dynamic.annotation.Param;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import com.gdn.x.product.rest.web.model.response.ItemSkuAndPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.MinMaxItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.ProductSkuPickupPointResponse;
import com.gdn.x.product.rest.web.model.util.GdnRestSimpleResponse;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.product.model.vo.ItemPickupPointTransactionResponse;

import com.gdn.x.product.model.vo.ItemPickupPointUpdateRequestVo;
import com.gdn.x.product.service.util.ModelConverter;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointViewConfigBaseRequest;

import com.gdn.x.product.rest.web.model.request.ItemPickupPointUpdateRequest;

import com.gdn.x.product.rest.web.model.response.ItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.SimpleLongResponse;
import com.gdn.x.product.rest.web.model.util.ItemPickupPointApiPath;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPickupPointWrapperService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ProductSearchService;
import lombok.extern.slf4j.Slf4j;

@RestController
@Tag(name = "ItemPickupPointController", description = "ItemPickupPointController Apis")
@Slf4j
@RequestMapping(value = ItemPickupPointApiPath.BASE_PATH)
public class ItemPickupPointController {

  @Autowired
  private ItemPickupPointWrapperService itemPickupPointWrapperService;

  @Autowired
  private ItemService itemService;

  @Autowired
  private ModelConverter modelConverter;

  @Autowired
  private ProductSearchService productSearchService;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @RequestMapping(value = ItemPickupPointApiPath.GET_PRODUCT_AND_ITEM_BY_ITEM_SKU_AND_PICKUP_POINT_CODE, method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Scheduler to migrate to new L5 collection [CART API]",
      description = "Scheduler to migrate to new L5 collection [CART API]")
  public GdnRestListResponse<ItemPickupPointTransactionResponse> findProductAndItemByItemSkuAndPickupPointCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody List<ItemPickupPointRequest> itemPickupPointRequest) {
    log.info("findProductAndItemByItemSkuAndPickupPointCode request : {} ", itemPickupPointRequest);
    try {
      List<ItemPickupPointTransactionResponse> itemPickupPointTransactionResponseList =
          itemPickupPointWrapperService.findProductForTransactionByItemSkusAndPickupPointCode(storeId, requestId,
              username, itemPickupPointRequest);
      return new GdnRestListResponse<>(itemPickupPointTransactionResponseList, null, requestId);
    } catch (Exception e) {
      log.error("Error findProductAndItemByItemSkuAndPickupPointCode request : {}, error {} ", itemPickupPointRequest, e);
      return new GdnRestListResponse<>(null, null, true, Collections.emptyList(), null, requestId);
    }
  }

  @RequestMapping(value = ItemPickupPointApiPath.GET_ITEM_SUMMARY_BY_ITEM_SKU_AND_PICKUP_POINT_CODE, method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Scheduler to migrate to new L5 collection",
      description = "Scheduler to migrate to new L5 collection")
  public GdnRestListResponse<ItemSummaryListResponse> findItemSummaryByItemSkuAndPickupPointCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(required = false) String fetchViewConfigByChannel,
      @RequestParam(required = false, defaultValue = "false") boolean withValueAndValueTypes,
      @RequestParam(required = false, defaultValue = "ALL") String catalogType,
      @RequestParam(required = false, defaultValue = "false") boolean excludeDistributionPickupPoint,
      @RequestBody List<ItemPickupPointRequest> itemPickupPointRequest) {
    log.info("findItemSummaryByItemSkuAndPickupPointCode request : {} ", itemPickupPointRequest);
    try {
      GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(itemPickupPointRequest),
        ErrorMessages.ITEM_AKU_AND_PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
      List<ItemSummaryListResponse> itemSummaryListResponseList =
        itemPickupPointWrapperService.fetchItemSummaryByItemSkusAndPickupPointCode(storeId,
          requestId, username, withValueAndValueTypes, itemPickupPointRequest,
          fetchViewConfigByChannel, clientId, catalogType, excludeDistributionPickupPoint);
      return new GdnRestListResponse<>(itemSummaryListResponseList,
        new PageMetaData(itemPickupPointRequest.size(), 0, itemSummaryListResponseList.size()),
        requestId);
   } catch (ApiIncorrectInputDataException e) {
      log.error("Error findItemSummaryByItemSkuAndPickupPointCode request : {}, error {} ", itemPickupPointRequest, e);
      return new GdnRestListResponse<>(e.getErrorMessage(), e.getErrorCode(), false, Collections.emptyList(), null,
          requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error findItemSummaryByItemSkuAndPickupPointCode request : {}, error {} ",
        itemPickupPointRequest, e);
      return new GdnRestListResponse<>(e.getErrorMessage(), e.getErrorMessage(), false,
        Collections.emptyList(), null, requestId);
    } catch (Exception e) {
      log.error("Error findItemSummaryByItemSkuAndPickupPointCode request : {}, error {} ", itemPickupPointRequest, e);
      return new GdnRestListResponse<>(e.getMessage(), e.getMessage(), false, Collections.emptyList(), null, requestId);
    }
  }

  @RequestMapping(value = ItemPickupPointApiPath.UPDATE_VIEW_CONFIG_WITH_PRODUCT_STATUS_IN_ITEMPICKUPPOINT, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update item pickup point view config in L5", description = "update item pickup "
    + "point view config by checking the status of item in L5")
  public GdnBaseRestResponse updateItemPickupPointViewConfigWithProductStatus(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @PathVariable("productSku") String productSku,
      @RequestBody ItemPickupPointViewConfigBaseRequest itemPickupPointViewConfigBaseRequest) {
    log.info("Update itemPickupPoint view config with productSku = {}, itemPickupPointViewConfigRequest = {}",
        productSku, itemPickupPointViewConfigBaseRequest);
    try {
      this.itemPickupPointWrapperService.updateItemPickupPointViewConfigWithProductStatus(
          storeId, username, requestId,
          productSku, itemPickupPointViewConfigBaseRequest);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("Error while updating itemPickupPoint view config ProductSku: {} error : {} ", productSku,
          e.getMessage(), e);
      return new GdnBaseRestResponse(e.getMessage(), ProductErrorCodesEnum.UPDATE_ITEM_VIEW_CONFIG.getCode(), false,
          requestId);
    }
  }

  @RequestMapping(value = ItemPickupPointApiPath.GET_ITEM_SKUS_BY_ITEM_CODE_AND_PICKUPPOINT_CODE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get all item for item code", description = "get all item for item code")
  public GdnRestListResponse<ItemPriceResponse> getItemSkusByItemCodeAndPickupPointCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @PathVariable("itemCode") String itemCode,
      @PathVariable("pickupPointCode") String pickupPointCode, @RequestParam(required = false) boolean fetchAll) {
    try {
      List<ItemPriceResponse> itemPriceResponses =
          itemService.getAllItemSkuByItemCodeAndPickupPointCode(storeId, itemCode, pickupPointCode, fetchAll);
      return new GdnRestListResponse<>(null, null, true, itemPriceResponses,
          new PageMetaData(itemPriceResponses.size(), 0, itemPriceResponses.size()), requestId);
    } catch (Exception ex) {
      log.error("Error in fetching item skus by item code : {} and pickup point code : {}, error {} ", itemCode,
          pickupPointCode, ex);
      return new GdnRestListResponse<>(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage(),
          ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), Boolean.FALSE, null, new PageMetaData(1, 0, 0), requestId);
    }
  }

  @RequestMapping(value = ItemPickupPointApiPath.GET_L5S_CREATED_IN_TIME_RANGE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Getting L5s created in time range", description = "Getting L5s created in time range")
  public GdnRestListResponse<ItemSkuAndPickupPointCodeResponse> getItemSkuAndPickupPointCreatedInTimeRange(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username, @RequestParam String startDate,
      @RequestParam String endDate, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "10") int size) {
    try {
      SimpleDateFormat format = new SimpleDateFormat(Constants.DATE_TIME_FORMAT);
      return itemPickupPointService.getL5sCreatedInTimeRange(storeId, format.parse(startDate), format.parse(endDate), page,
              size, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error(
          "Error while getting L5s create in time range. startDate : {} , endDate : {} , requestId : {} , error - ",
          startDate, endDate, requestId, e);
      return new GdnRestListResponse<>(e.getMessage(), e.getErrorCodes().getCode(), false, Collections.emptyList(),
          null, requestId);
    } catch (Exception e) {
      log.error(
          "Error while getting L5s create in time range. startDate : {} , endDate : {} , requestId : {} , error - ",
          startDate, endDate, requestId, e);
      return new GdnRestListResponse<>(e.getMessage(), e.getMessage(), false, Collections.emptyList(), null, requestId);
    }
  }

  @RequestMapping(value = ItemPickupPointApiPath.GET_L5S_PRICE_UPDATED_IN_TIME_RANGE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Getting L5s price updated in time range", description = "Getting L5s price updated in time range")
  public GdnRestListResponse<PriceUpdatedInTimeRangeL5Response> getL5sPriceUpdatedInTimeRange(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username, @RequestParam String startDate,
      @RequestParam String endDate, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "10") int size) {
    try {
      SimpleDateFormat format = new SimpleDateFormat(Constants.DATE_TIME_FORMAT);
      return itemPickupPointService.getL5sPriceUpdatedInTimeRange(storeId, format.parse(startDate),
          format.parse(endDate), page, size, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error(
          "Error while getting L5s price updated in time range. startDate : {} , endDate : {} , requestId : {} , error - ",
          startDate, endDate, requestId, e);
      return new GdnRestListResponse<>(e.getMessage(), e.getErrorCodes().getCode(), false, Collections.emptyList(),
          null, requestId);
    } catch (Exception e) {
      log.error(
          "Error while getting L5s price updated in time range. startDate : {} , endDate : {} , requestId : {} , error - ",
          startDate, endDate, requestId, e);
      return new GdnRestListResponse<>(e.getMessage(), e.getMessage(), false, Collections.emptyList(), null, requestId);
    }
  }

  @RequestMapping(value = ItemPickupPointApiPath.UPDATE_ITEM_PICKUP_POINTS, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Modify/add/delete item Pickup Points", description = "Modify/add/delete item "
    + "Pickup Points")
  public GdnRestSingleResponse<EditItemResponse> updateItemPickupPoints(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody ItemPickupPointUpdateRequest itemPickupPointUpdateRequest) {
    log.info("Modify/add/delete item Pickup Points request : {} ", itemPickupPointUpdateRequest);
    EditItemResponse editItemResponse = new EditItemResponse();
    try {
      MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, username);
      ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo =
          modelConverter.covertToItemPickupPointUpdateRequestVo(itemPickupPointUpdateRequest);
      editItemResponse =
          itemPickupPointWrapperService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
              null);
      if (Objects.nonNull(editItemResponse.getApiErrorCode())) {
        log.error("Error while updating the edit info for productSku : {} with request : {} error code : {}",
            itemPickupPointUpdateRequest.getProductSku(), itemPickupPointUpdateRequest, editItemResponse);
        return new GdnRestSingleResponse<>(editItemResponse.getApiErrorCode().getDesc(),
            editItemResponse.getApiErrorCode().getCode(), false, editItemResponse, requestId);
      }
      return new GdnRestSingleResponse<>(null, null, true, editItemResponse, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error findItemSummaryByItemSkuAndPickupPointCode request : {}, error  ", itemPickupPointUpdateRequest,
          e);
      return new GdnRestSingleResponse<>(e.getMessage(), e.getErrorCodes().getCode(), false, editItemResponse, requestId);
    }catch (Exception e) {
      log.error("Error findItemSummaryByItemSkuAndPickupPointCode request : {}, error  ", itemPickupPointUpdateRequest,
          e);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, editItemResponse, requestId);
    }
  }

  @RequestMapping(value = ItemPickupPointApiPath.GET_ITEM_SKUS_BY_PRISTINE_ID_AND_PICKUPPOINT_CODE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary= "get all item for pristineId", description = "get all item for pristineId")
  public GdnRestListResponse<ItemPriceResponse> getItemSkusByPristineIdAndPickupPointCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @PathVariable("pristineId") String pristineId,
      @PathVariable("pickupPointCode") String pickupPointCode) {
    try {
      List<ItemPriceResponse> itemPriceResponses =
          itemService.getAllItemSkuByPristineIdAndPickupPointCode(storeId, pristineId, pickupPointCode);
      return new GdnRestListResponse<>(null, null, true, itemPriceResponses,
          new PageMetaData(itemPriceResponses.size(), 0, itemPriceResponses.size()), requestId);
    } catch (Exception ex) {
      log.error("Error in fetching item skus by pristine id : {} and pickup point code : {}, error {} ", pristineId,
          pickupPointCode, ex);
      return new GdnRestListResponse<>(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage(),
          ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), Boolean.FALSE, null, new PageMetaData(1, 0, 0), requestId);
    }
  }

  @RequestMapping(value = ItemPickupPointApiPath.GET_L5_COUNT_BY_ITEM_SKU, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get L5 count based on item sku", description = "get L5 count based on item sku")
  public GdnRestSingleResponse<SimpleLongResponse> getL5CountByItemSku(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @PathVariable("itemSku") String itemSku) {
    try {
      Long L5Count = this.itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku);
      return new GdnRestSingleResponse<SimpleLongResponse>(null, null, true, new SimpleLongResponse(L5Count), requestId);
    } catch (Exception e) {
      log.error("Error in fetching L5 count for itemSku {}, error {} ", itemSku, e);
      return new GdnRestSingleResponse<SimpleLongResponse>(e.getMessage(), null, false, null,
          requestId);
    }
  }

  @RequestMapping(value = ItemPickupPointApiPath.GET_PRICE_DETAIL_BY_ITEM_SKU_AND_PICKUP_POINT_CODE, method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetch Price Details for list of L5s", description = "Detailed price response "
    + "for list of L5s")
  public GdnRestListResponse<ItemPickupPointPriceResponse> findPriceDetailsByItemSkuAndPickupPointCode(
    @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam(required = false) String username,
    @RequestBody List<ItemPickupPointRequest> itemPickupPointRequests) {
    log.info("#findPriceDetailsByItemSkuAndPickupPointCode for requestId : {} with payload : {} ",
      requestId, itemPickupPointRequests);
    try {
      List<ItemPickupPointPriceResponse> itemPriceDetailResponses =
        itemPickupPointWrapperService.findPriceDetailsByItemSkuAndPickupPointCode(storeId,
          itemPickupPointRequests);
      return new GdnRestListResponse<>(itemPriceDetailResponses,
        new PageMetaData(itemPriceDetailResponses.size(), 0, itemPriceDetailResponses.size()),
        requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error in findPriceDetailsByItemSkuAndPickupPointCode request : {}, error - ",
        itemPickupPointRequests, e);
      return new GdnRestListResponse<>(e.getMessage(),
        ErrorCategory.VALIDATION.getCode(), false, Collections.emptyList(), null, requestId);
    } catch (Exception e) {
      log.error("Error in findPriceDetailsByItemSkuAndPickupPointCode request : {}, error - ",
        itemPickupPointRequests, e);
      return new GdnRestListResponse<>(e.getMessage(), e.getMessage(), false,
        Collections.emptyList(), null, requestId);
    }
  }

  @RequestMapping(value = ItemPickupPointApiPath.DELETE_ITEM_PICKUP_POINT_BY_PICKUPPOINT_CODE, method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Delete L5's by pickup point code", description = "Delete L5's by pickup point code")
  public GdnRestListResponse<DeleteItemPickupPointResponse> deleteItemPickupPointByPickupPointCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody DeleteItemPickupPointRequest deleteItemPickupPointRequest) {
    log.info("Delete item pickup point by pickupPointCode  with deleteItemPickupPointRequest = {} ",
        deleteItemPickupPointRequest);
    try {
      List<DeleteItemPickupPointResponse> deleteItemPickupPointResponseList =
          itemPickupPointWrapperService.deleteItemPickupPointsByPickupPointCode(storeId, deleteItemPickupPointRequest);
      return new GdnRestListResponse<>(deleteItemPickupPointResponseList,
          new PageMetaData(deleteItemPickupPointResponseList.size(), 0, deleteItemPickupPointResponseList.size()),
          requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error in delete itemPickupPoint by pickupPointCode = {} ,  error -  ",
          deleteItemPickupPointRequest.getPickupPointCode(), e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.VALIDATION.getCode(), false,
          Collections.emptyList(), null, requestId);
    } catch (Exception e) {
      log.error("Error in delete itemPickupPoint by pickupPointCode = {} ,  error -  ",
          deleteItemPickupPointRequest.getPickupPointCode(), e);
      return new GdnRestListResponse<>(e.getMessage(), e.getMessage(), false, Collections.emptyList(), null, requestId);
    }
  }

  @RequestMapping(value = ItemPickupPointApiPath.FIND_BY_ITEM_SKU_LIST, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Find L5's by itemSkus", description = "Find L5's by itemSkus")
  public GdnRestListResponse<ItemPickupPointL5Response> findL5sByItemSkuList(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "50") int size,
      @RequestParam(required = false) String fetchViewConfigByChannel,
      @RequestBody SimpleListStringRequest simpleListStringRequest) {
    log.info("Delete item pickup point by pickupPointCode  with deleteItemPickupPointRequest = {} ",
        simpleListStringRequest);
    try {
      Page<ItemPickupPointL5Response> itemPickupPointL5Responses =
          itemPickupPointWrapperService.findItemPickupPointsByItemSkus(storeId, simpleListStringRequest,
              page, size, fetchViewConfigByChannel);
      return new GdnRestListResponse<>(itemPickupPointL5Responses.getContent(),
          new PageMetaData(itemPickupPointL5Responses.getSize(), 0, itemPickupPointL5Responses.getTotalElements()),
          requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error in fetch itemPickupPoint by skus = {} ,  error -  ",
          simpleListStringRequest, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.VALIDATION.getCode(), false,
          Collections.emptyList(), null, requestId);
    } catch (Exception e) {
      log.error("Error in fetch itemPickupPoint by skus = {} ,  error -  ",
          simpleListStringRequest, e);
      return new GdnRestListResponse<>(e.getMessage(), e.getMessage(), false, Collections.emptyList(), null, requestId);
    }
  }

  @RequestMapping(value = ItemPickupPointApiPath.GET_PRODUCT_SKU_LIST_BY_BP_AND_PP_CODE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get product sku list by business partner code and pp code", description = "Get "
    + "product sku list by business partner code and pp code")
  public GdnRestListResponse<ProductSkuPickupPointResponseV2> findProductSkuListByBusinessPartnerAndPickupPointCode(
    @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam(defaultValue = "0") int page,
    @RequestParam(defaultValue = "50") int size, @RequestParam String businessPartnerCode,
    @RequestParam String pickupPointCode) {
    log.info("findProductSkuListByBusinessPartnerAndPickupPointCode bp : {} & ppCode : {} ",
      businessPartnerCode, pickupPointCode);
    try {
      Page<ProductSkuPickupPointResponseV2> productSkuPickupPointResponses = itemPickupPointService
        .getProductSkuListByBusinessPartnerAndPickupPointCode(storeId, businessPartnerCode,
          pickupPointCode, page, size);
      return new GdnRestListResponse<>(productSkuPickupPointResponses.getContent(),
        new PageMetaData(size, page, productSkuPickupPointResponses.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error("Error findItemSummaryByItemSkuAndPickupPointCode bp code : {}, ppCode : {}, error ",
          businessPartnerCode, pickupPointCode, e);
      return new GdnRestListResponse<>(e.getMessage(), e.getMessage(), false,
        Collections.emptyList(), null, requestId);
    }
  }

  @RequestMapping(value = ItemPickupPointApiPath.GET_MIN_AND_MAX_OFFER_PRICE, method = RequestMethod.GET, produces =
    MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get min and max prices for an L3")
  public GdnRestSimpleResponse<MinMaxItemPriceResponse> getMinAndMaxOfferPrice(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username,
    @PathVariable("productCode") String productCode){
    log.info("Fetching Min and Max Price for product Code : {} for requestId : {}", productCode,
      requestId);
    try{
      MinMaxItemPriceResponse minMaxItemPriceResponse =
        itemPickupPointService.getMinAndMaxOfferPrice(storeId, productCode);
      return new GdnRestSimpleResponse<>(null, null, true, requestId, minMaxItemPriceResponse);
    }
    catch (Exception e){
      log.error("Error Fetching min and max price for product Code : {} with Error - ",
        productCode,e);
      return new GdnRestSimpleResponse<>(e.getMessage(), e.getMessage(), false, requestId, null);
    }
  }

  @RequestMapping(value = ItemPickupPointApiPath.CREATE_FBB_PICKUP_POINT, method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Create Fbb pickup point for L4", description = "Create Fbb pickup point for L4")
  public GdnRestSingleResponse<CreateFbbPickupPointResponse> createFbbPickupPoint(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestBody CreateFbbPickupPointRequest createFbbPickupPointRequest) {
    log.info("Create FBB pickup point with createFbbPickupPointRequest = {} ", createFbbPickupPointRequest);
    try {
      CreateFbbPickupPointResponse createFbbPickupPointResponse =
          itemPickupPointWrapperService.createFbbPickupPoint(storeId, createFbbPickupPointRequest);
      return new GdnRestSingleResponse<>(createFbbPickupPointResponse, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error in creating fbb pickup point with itemSku = {} , pickupPointCode = {}  ",
          createFbbPickupPointRequest.getItemSku(), createFbbPickupPointRequest.getPickupPointCode(), e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.VALIDATION.getCode(), false, null, requestId);
    } catch (Exception e) {
      log.error("Error in creating fbb pickup point with itemSku = {} , pickupPointCode = {}  ",
          createFbbPickupPointRequest.getItemSku(), createFbbPickupPointRequest.getPickupPointCode(), e);
      return new GdnRestSingleResponse<>(e.getMessage(), e.getMessage(), false, null, requestId);
    }
  }

  @RequestMapping(value = ItemPickupPointApiPath.GET_PRODUCT_L5_DETAIL_BY_ITEM_SKU_AND_PICKUP_POINT_CODE, method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Find Product L5 Detail by ItemSku and PP Code", description = "Find Product L5 Detail by ItemSku and PP Code")
  public GdnRestListResponse<ProductL5DetailResponse> findProductL5DetailItemSkuAndPickupPointCode(
    @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody List<ItemPickupPointRequest> itemPickupPointRequest,
      @RequestParam(required = false) boolean inAllProducts,
      @RequestParam(required = false) String fetchViewConfigByChannel) {
    log.info("findProductAndItemByItemSkuAndPickupPointCode request : {} ", itemPickupPointRequest);
    try {
      List<ProductL5DetailResponse> itemPickupPointTransactionResponseList =
        itemPickupPointWrapperService.findProductL5DetailByItemSkusAndPickupPointCode(storeId,
          itemPickupPointRequest, inAllProducts, fetchViewConfigByChannel);
      return new GdnRestListResponse<>(itemPickupPointTransactionResponseList, null, requestId);
    } catch (Exception e) {
      log.error("Error findProductAndItemByItemSkuAndPickupPointCode request : {}, error ",
        itemPickupPointRequest, e);
      return new GdnRestListResponse<>(null, null, true, Collections.emptyList(), null, requestId);
    }
  }

  @RequestMapping(value = ItemPickupPointApiPath.AUTO_CREATE_L5_BY_ITEM_SKU_AND_PP_CODE, method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Auto create L5 by item sku and pp code", description = "Auto create L5 by item sku and pp code")
  public GdnRestSingleResponse<AutoCreatePickupPointListResponse> autoCreatePickupPoint(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestBody AutoCreatePickupPointRequestList autoCreatePickupPointRequestList) {
    log.info("Auto Create L5 with autoCreatePickupPointRequest = {} ", autoCreatePickupPointRequestList);
    try {
      MandatoryRequestParam mandatoryRequestParam =
          MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username,
              username);
      AutoCreatePickupPointListResponse autoCreatePickupPointListResponse =
          itemPickupPointWrapperService.autoCreatePickupPoint(autoCreatePickupPointRequestList, mandatoryRequestParam);
      return new GdnRestSingleResponse<>(autoCreatePickupPointListResponse, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error in auto creating L5 with autoCreatePickupPointRequest = {} ", autoCreatePickupPointRequestList,
          e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.VALIDATION.getCode(), false, null, requestId);
    } catch (Exception e) {
      log.error("Error in auto creating L5 with autoCreatePickupPointRequest = {} ", autoCreatePickupPointRequestList,
          e);
      return new GdnRestSingleResponse<>(e.getMessage(), e.getMessage(), false, null, requestId);
    }
  }

  @RequestMapping(value = ItemPickupPointApiPath.FIND_FBB_TRUE_ONLINE_L5_BY_ITEM_SKUS, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Find fbb true and online L5s", description = "Find fbb true and online L5s")
  public GdnRestListResponse<ItemSkuPickupPointCodeResponse> findFbbTrueAndOnlineItemPickupPoints(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody SimpleSetStringRequest simpleSetStringRequest) {
    log.info("Get online fbb L5 with request = {} ", simpleSetStringRequest);
    try {
      List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointCodeResponseList =
          itemPickupPointWrapperService.findFbbTrueOnlinePickupPointsAndItemSkusIn(storeId,
              new ArrayList<>(simpleSetStringRequest.getValue()));
      return new GdnRestListResponse<>(itemSkuPickupPointCodeResponseList,
          new PageMetaData(itemSkuPickupPointCodeResponseList.size(), 0, itemSkuPickupPointCodeResponseList.size()),
          requestId);
    } catch (Exception e) {
      log.error("Error in auto creating L5 with autoCreatePickupPointRequest = {} ", simpleSetStringRequest, e);
      return new GdnRestListResponse<>(e.getMessage(), e.getMessage(), false, Collections.emptyList(), null, requestId);
    }
  }

  @PostMapping(value = ItemPickupPointApiPath.FETCH_BASIC_DETAILS_BY_BY_ITEM_SKU_AND_PICKUP_POINT_CODE,
    consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Find view config response by ItemSku and PP Code",
    description = "Find view config response by ItemSku and PP Code")
  public GdnRestListResponse<ItemPickupPointBasicResponse> fetchViewConfigsByItemSkuAndPickupPointCodeList(
    @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam(required = false) String username,
    @RequestBody List<ItemPickupPointRequest> itemPickupPointRequest) {
    log.info("fetchViewConfigsByItemSkuAndPickupPointCode request : {} ", itemPickupPointRequest);
    try {
      List<ItemPickupPointBasicResponse> viewConfigResponses =
        itemPickupPointWrapperService.fetchBasicDetailsByItemSkuAndPickupPointCodeList(storeId,
          itemPickupPointRequest);
      return new GdnRestListResponse<>(viewConfigResponses, null, requestId);
    } catch (Exception e) {
      log.error("Error fetchViewConfigsByItemSkuAndPickupPointCode request : {}, error ",
        itemPickupPointRequest, e);
      return new GdnRestListResponse<>(null, null, true, Collections.emptyList(), null, requestId);
    }
  }

  @PostMapping(value = ItemPickupPointApiPath.FETCH_BY_EAN_CODE_AND_PICKUP_POINT_CODE, consumes =
      MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Find itemPickupPoint by UPC code and PP Code", description = "Find "
      + "itemPickupPoint by UPC code and PP Code")
  public GdnRestListResponse<EanUpcPickupPointCodeResponse> fetchByEanCodeAndPickupPointCodes(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody EanUpcPickupPointCodeRequest upcPickupPointCodeRequest,
      @RequestParam(required = false, defaultValue = "CNC") String fetchViewConfigByChannel) {
    log.info("fetchByEanCodeAndPickupPointCodes request : {} ", upcPickupPointCodeRequest);
    try {
      List<EanUpcPickupPointCodeResponse> responses =
          itemPickupPointWrapperService.fetchItemDetailsByEanUpcCode(storeId,
              upcPickupPointCodeRequest, fetchViewConfigByChannel);
      return new GdnRestListResponse<>(responses, null, requestId);
    } catch (Exception e) {
      log.error("Error fetchByEanCodeAndPickupPointCodes request : {}, error ",
          upcPickupPointCodeRequest, e);
      return new GdnRestListResponse<EanUpcPickupPointCodeResponse>(null, null, false,
          Collections.emptyList(), null, requestId);
    }
  }

  @GetMapping(value = ItemPickupPointApiPath.GET_CNC_AT_L5_BY_PRODUCT_SKU,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get Cnc At L5", description = "Get Cnc At L5")
  public GdnRestSingleResponse<SimpleBooleanResponse> getCncAtL5ByProductSku(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @Param("productSku") String productSku) {
    try {
      boolean cncAtL5Response = itemPickupPointWrapperService.getCncAtL5ByProductSku(storeId, productSku);
      return new GdnRestSingleResponse<>(null, null, true, new SimpleBooleanResponse(cncAtL5Response), requestId);
    } catch (Exception e) {
      log.error("Error while getting Cnc At L5 for productSku: {} error : {}", productSku, e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
  }
}
