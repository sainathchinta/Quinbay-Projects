package com.gdn.partners.pbp.controller.offlineitem;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.gdn.partners.pbp.dto.offlineitem.OfflineItemResponseDetailResponse;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponseDetail;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.param.PageableHelper;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.converter.offlineitem.OfflineItemRequestConverter;
import com.gdn.mta.product.converter.offlineitem.OfflineItemResponseConverter;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.partner.pbp.logger.standar.LoggerAspect;
import com.gdn.partner.pbp.logger.standar.LoggerAttributeModel;
import com.gdn.partner.pbp.logger.standar.LoggerParam;
import com.gdn.partner.pbp.logger.standar.LoggerStandard;
import com.gdn.partners.pbp.dto.common.ListRequestDTO;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemDetailResponse;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemRequest;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.FailedOfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.SuccessOfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.ListRequest;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemAttributeRequest;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemDetailResponse;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupBulkDownloadResponse;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupRequest;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupResponse;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupV2Request;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupV2Response;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemDetailResponse;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemFailedResponse;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemRequest;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemResponse;
import com.gdn.partners.pbp.model.offlineitem.DeleteOfflineItem;
import com.gdn.partners.pbp.model.offlineitem.OfflineItem;
import com.gdn.partners.pbp.model.offlineitem.OfflineItemDetail;
import com.gdn.partners.pbp.model.offlineitem.UpsertOfflineItem;
import com.gdn.partners.pbp.service.offlineitem.OfflineItemService;
import com.gdn.partners.pbp.web.model.OfflineItemControllerPath;

import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;

@RestController
@RequestMapping(value = OfflineItemControllerPath.BASE_PATH)
@Tag(name = "OfflineItemController", description = "Offline Item Service API")
public class OfflineItemController {

  private static final String OFFLINE_ITEM_REQUEST_MUST_NOT_NULL =
      "OfflineItemRequest must not be null";
  private static final String OFFLINE_ITEM_LIST_MUST_NOT_EMPTY =
      "OfflineItemAttributeRequestList must not empty";
  private static final String MERCHANT_CODE_MUST_NOT_BE_EMPTY = "merchant code must not empty";
  private static final String BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY =
      "businessPartnerCode must not be empty";
  private static final String REQUEST_MUST_NOT_BE_NULL = "request must not be null";
  private static final String REQUEST_MUST_NOT_BE_EMPTY = "request must not be empty";
  private static final String OFFLINE_ITEM_REQUEST_LIST_MUST_NOT_BE_EMPTY =
      "UpsertOfflineItemRequest List must not be empty";

  private static final Logger LOGGER = LoggerFactory.getLogger(OfflineItemController.class);

  @Autowired
  private OfflineItemService offlineItemService;

  @Autowired
  private OfflineItemResponseConverter offlineItemResponseConverter;

  @Autowired
  private OfflineItemRequestConverter offlineItemRequestConverter;

  @RequestMapping(value = OfflineItemControllerPath.FILTER_OFFLINE_ITEM_PRODUCT_BY_MERCHANT_SKUS,
      method = RequestMethod.POST,
      produces = {MediaType.APPLICATION_JSON_VALUE},
      consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter offline item product by merchant skus",
      description = "filter offline item product by merchant skus")
  @ResponseBody
  public GdnRestListResponse<OfflineItemResponseDetailResponse> filterOfflineItemProductByMerchantSkus(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String businessPartnerCode, @RequestBody SimpleListStringRequest request)
      throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(businessPartnerCode),
        BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(Objects.nonNull(request), REQUEST_MUST_NOT_BE_NULL);
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(request.getValue()),
        REQUEST_MUST_NOT_BE_EMPTY);

    List<OfflineItemResponseDetail> offlineItemDetails =
        offlineItemService.findOfflineItemProductByMerchantSkus(businessPartnerCode, request.getValue());

    List<OfflineItemResponseDetailResponse> responses =
        offlineItemResponseConverter.convertOfflineItemResponseDetailResponses(offlineItemDetails);

    return new GdnRestListResponse<>(responses, new PageMetaData(
        responses.size(), 0, responses.size()), requestId);
  }

  @RequestMapping(value = OfflineItemControllerPath.UPSERT_PATH, method = RequestMethod.POST,
      produces = {MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  @Operation(summary = "Upsert Offline Items value", description = "upsert stock and price from mta")
  public GdnRestSingleResponse<UpsertOfflineItemResponse> upsertOfflineItems(
      @RequestParam String storeId, @RequestParam String clientId, @RequestParam String channelId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String merchantCode, @RequestParam(required = false, defaultValue = "true") boolean updateStock,
      @RequestBody ListRequestDTO<UpsertOfflineItemRequest> request) throws Exception {

    try {
      GdnPreconditions.checkArgument(request != null, OFFLINE_ITEM_REQUEST_MUST_NOT_NULL);
      GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(request.getList()),
          OFFLINE_ITEM_REQUEST_LIST_MUST_NOT_BE_EMPTY);

      List<UpsertOfflineItem> upsertOfflineItems =
          offlineItemRequestConverter.convertToUpsertOfflineItems(request.getList());

      Pair<List<UpsertOfflineItemDetailResponse>, List<UpsertOfflineItemFailedResponse>> pairOfUpsertOfflineItemErrorResponse =
          offlineItemService.upsertOfflineItems(requestId, username, merchantCode,
              upsertOfflineItems, updateStock);

      UpsertOfflineItemResponse response = new UpsertOfflineItemResponse(merchantCode,
          pairOfUpsertOfflineItemErrorResponse.getLeft(),
          pairOfUpsertOfflineItemErrorResponse.getRight());

      return new GdnRestSingleResponse<>(response, requestId);
    } catch (Exception e) {
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          null, requestId);
    }
  }

  @RequestMapping(value = OfflineItemControllerPath.UPDATE_PRICE_BY_ITEM_SKU,
      method = RequestMethod.POST, produces = {MediaType.APPLICATION_JSON_VALUE},
      consumes = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  @Operation(summary = "Update Offline Item Price of one SKU for all stores",
      description = "updating price from MTA")
  public GdnBaseRestResponse updateOfflineItemPriceByItemSku(@RequestParam String storeId,
      @RequestParam String clientId, @RequestParam String channelId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String merchantCode,
      @RequestParam String itemSku, @RequestParam(required = false) Double listPrice,
      @RequestParam double offerPrice) throws Exception {
    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
    LOGGER.debug(
        "updateOfflineItemPriceByItemSku mandatoryRequestParam: {}, merchantCode: {}, itemSku: {}, listPrice: {}, offerPrice: {}",
        mandatoryRequestParam, merchantCode, itemSku, listPrice, offerPrice);
    try {
      boolean success = this.offlineItemService.updateOfflineItemPriceByItemSku(merchantCode,
          itemSku, listPrice, offerPrice);
      return new GdnBaseRestResponse(null, null, success, requestId);
    } catch (Exception e) {
      LOGGER.error(
          "#error updateOfflineItemPriceByItemSku mandatoryRequestParam: {}, merchantCode: {}, itemSku: {}, listPrice: {}, offerPrice: {}",
          mandatoryRequestParam, merchantCode, itemSku, listPrice, offerPrice, e);
      return new GdnBaseRestResponse(e.getMessage(), e.getMessage(), false, requestId);
    }
  }


  @RequestMapping(value = OfflineItemControllerPath.DELETE_OFFLINE_ITEM, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "delete offline item", description = "delete offline item")
  @ResponseBody
  public GdnBaseRestResponse deleteOfflineItem(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String merchantCode,
      @RequestBody ListRequest<DeleteOfflineItemRequest> request) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "deleteOfflineItem",
        merchantCode, username, requestId, storeId, channelId, clientId,
        LoggerAspect.OFFLINE_ITEM_DELETE, null, request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(),
        LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(merchantCode)),
        MERCHANT_CODE_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(!(CollectionUtils.isEmpty(request.getList())),
        REQUEST_MUST_NOT_BE_EMPTY);

    List<DeleteOfflineItem> deleteOfflineItems =
        this.offlineItemRequestConverter.convertToDeleteOfflineItem(request.getList());
    this.offlineItemService.bulkDeleteOfflineItem(requestId, username, merchantCode, deleteOfflineItems);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = OfflineItemControllerPath.BULK_DELETE_OFFLINE_ITEM, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "bulk delete offline item", description = "bulk delete offline item")
  @ResponseBody
  public GdnRestSingleResponse<DeleteOfflineItemResponse> bulkDeleteOfflineItem(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String merchantCode,
      @RequestBody ListRequest<DeleteOfflineItemRequest> request) throws Exception {
    try {
      LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "deleteOfflineItem",
          merchantCode, username, requestId, storeId, channelId, clientId,
          LoggerAspect.OFFLINE_ITEM_DELETE, null, request.toString());

      MDC.put(LoggerParam.GENERIC_LOGGER.getParam(),
          LoggerStandard.getGenericLogTemplate(loggerAttribute));
      LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

      GdnPreconditions.checkArgument(!(StringUtils.isEmpty(merchantCode)),
          MERCHANT_CODE_MUST_NOT_BE_EMPTY);
      GdnPreconditions.checkArgument(!(CollectionUtils.isEmpty(request.getList())),
          REQUEST_MUST_NOT_BE_EMPTY);

      List<DeleteOfflineItem> deleteOfflineItems =
          this.offlineItemRequestConverter.convertToDeleteOfflineItem(request.getList());
      Pair<List<DeleteOfflineItemDetailResponse>, List<DeleteOfflineItemDetailResponse>> succeededAndFailedOfflineItems =
          offlineItemService.bulkDeleteOfflineItem(requestId, username, merchantCode, deleteOfflineItems);
      DeleteOfflineItemResponse response = new DeleteOfflineItemResponse(merchantCode,
          succeededAndFailedOfflineItems.getLeft(), succeededAndFailedOfflineItems.getRight());

      return new GdnRestSingleResponse<>(response, requestId);
    } catch (Exception e) {
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(),
          false, null, requestId);
    }
  }
}
