package com.gdn.partners.pcu.external.web.controller;

import com.gda.mta.product.dto.response.ProductCountResponse;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.tuple.Pair;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.dto.BulkProcessResponse;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.client.model.OmniChannelSkuWebRequest;
import com.gdn.partners.pcu.external.client.model.ValidOmniChannelSkuWebResponse;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.model.ProductV2ApiPath;
import com.gdn.partners.pcu.external.service.ProductV2Service;
import com.gdn.partners.pcu.external.service.ProductWrapperV2Service;
import com.gdn.partners.pcu.external.validation.validator.Annotations.EditProductRequestValid;
import com.gdn.partners.pcu.external.web.model.request.ItemPickupPointListingL3WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductEditInfoV2WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ListingUpdateV2WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductSummaryV2WebRequest;
import com.gdn.partners.pcu.external.web.model.request.WarehouseStockDetailsRequest;
import com.gdn.partners.pcu.external.web.model.request.WholesaleStatusV2Request;
import com.gdn.partners.pcu.external.web.model.response.BulkProcessStatusListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ConsignmentDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.EditProductWebResponse;
import com.gdn.partners.pcu.external.web.model.response.InventorySummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemCodeBasicDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemPickupPointListingL3WebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3CountWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3ListingV2WebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3V2WebResponse;
import com.gdn.partners.pcu.external.web.model.response.WarehouseStockDetailsWebResponse;
import com.gdn.partners.pcu.external.web.model.response.WholesalePromoV2Response;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.hibernate.validator.constraints.NotBlank;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

import javax.validation.Valid;

@Slf4j
@Tag(name ="Product V2 API")
@RestController
@RequestMapping(value = ProductV2ApiPath.BASE_PATH)
@Validated
public class ProductV2Controller {

  @Autowired
  private ProductV2Service productV2Service;

  @Autowired
  private ProductWrapperV2Service productWrapperV2Service;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Operation(summary ="V2 API to update the product details")
  @PutMapping(value = ProductV2ApiPath.LISTING_UPDATE, consumes = MediaType.APPLICATION_JSON_VALUE,
    produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateItemListing(@PathVariable("productSku") String productSku,
    @RequestBody ListingUpdateV2WebRequest listingUpdateV2WebRequest) throws Exception {
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Method :updateItemListing by : {} for request: {}", requestId,
      listingUpdateV2WebRequest);
    productWrapperV2Service.updateItemListing(productSku,
      listingUpdateV2WebRequest.getQuickEditRequests());
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary ="API to get L3 product list summary")
  @PostMapping(value = ProductV2ApiPath.L3_LISTING)
  public ListBaseResponse<ProductLevel3ListingV2WebResponse> getFilterSummaryL3(
    @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "50") Integer size,
    @RequestParam(required = false, defaultValue = "true") boolean onlyDefaultViewConfig,
    @RequestBody ProductSummaryV2WebRequest request) throws Exception {
    log.info("Fetching product level3 v2 list. Request : {} ", request);
    String requestId = mandatoryParameterHelper.getRequestId();
    request.setMerchantCode(mandatoryParameterHelper.getBusinessPartnerCode());
    Page<ProductLevel3ListingV2WebResponse> response =
      this.productWrapperV2Service.getProductL3Listing(request, page, size, onlyDefaultViewConfig);
    return new ListBaseResponse<>(null, null, true, requestId, response.getContent(),
      new Metadata(page, size, response.getTotalElements()));
  }

  @Operation(summary ="Fetch the L3 product secondary counts")
  @GetMapping(value = ProductV2ApiPath.GET_SECONDARY_COUNTS, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<ProductL3CountWebResponse> getL3ProductSecondaryCounts(@RequestParam @NotBlank String type) {
    String requestId = mandatoryParameterHelper.getRequestId();
    String merchantCode = mandatoryParameterHelper.getBusinessPartnerCode();
    log.info("Method : Get the L3 product secondary counts for the type: {} and merchant code: {}", type, merchantCode);
    ProductL3CountWebResponse response = productV2Service.getL3CountsByType(merchantCode, type);
    return new SingleBaseResponse<>(null, null, true, requestId, response);
  }

  @Operation(summary ="Fetch the L3 product primary counts")
  @GetMapping(value = ProductV2ApiPath.GET_PRIMARY_COUNTS, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<ProductL3CountWebResponse> getL3ProductPrimaryCounts() {
    String requestId = mandatoryParameterHelper.getRequestId();
    String merchantCode = mandatoryParameterHelper.getBusinessPartnerCode();
    log.info("Method : Get the L3 product primary counts for merchant code: {}", merchantCode);
    ProductL3CountWebResponse response =
      productV2Service.getL3PrimaryCountsByMerchantCode(merchantCode);
    return new SingleBaseResponse<>(null, null, true, requestId, response);
  }

 @Operation(summary = "Get Product Count", description = "Get Products count for Product Limit")
 @GetMapping(value = ProductV2ApiPath.PRODUCT_COUNT, produces = MediaType.APPLICATION_JSON_VALUE)
 public GdnRestSingleResponse<ProductCountResponse> getProductCounts() {
   String requestId = mandatoryParameterHelper.getRequestId();
   String merchantCode = mandatoryParameterHelper.getBusinessPartnerCode();
   log.info("Get product count for merchantCode {} ", merchantCode);
   ProductCountResponse productCountResponse =
       productV2Service.getProductCountForProductLimit(mandatoryParameterHelper.getStoreId(), merchantCode);
   return new GdnRestSingleResponse<>(null, null, true, productCountResponse, requestId);
 }

  @Operation(summary = "Get Bulk Process Notes", description = "Get Bulk Process Notes")
  @GetMapping(value = ProductV2ApiPath.BULK_PROCESS_NOTES, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSingleResponse<BulkProcessResponse> getBulkProcessByProcessCode(@PathVariable("bulkProcessCode") String bulkProcessCode) {
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Get Bulk Process Notes {} ", bulkProcessCode);
    BulkProcessResponse bulkProcessResponse = productV2Service.getBulkProcessResponse(bulkProcessCode);
    return new GdnRestSingleResponse<>(null, null, true, bulkProcessResponse, requestId);
  }

  @Operation(summary ="Fetch wholesale status")
  @PostMapping(value = ProductV2ApiPath.WHOLESALE_STATUS, produces =
    MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<WholesalePromoV2Response> getWholesalePromoStatus(
    @RequestBody WholesaleStatusV2Request wholesaleStatusV2Request) {
    String requestId = mandatoryParameterHelper.getRequestId();
    String storeId = mandatoryParameterHelper.getStoreId();
    log.info("Fetch wholesale status for request : {}", wholesaleStatusV2Request);
    List<WholesalePromoV2Response> response =
      this.productV2Service.getWholesaleStatusByRequest(storeId, requestId,
        wholesaleStatusV2Request);
    return new ListBaseResponse<>(null, null, true, requestId, response, new Metadata(0,
      response.size(), (long) response.size()));
  }

  @Operation(summary ="API to fetch inventory summary for reserved stock")
  @GetMapping(value = ProductV2ApiPath.GET_INVENTORY_SUMMARY)
  public SingleBaseResponse<InventorySummaryWebResponse> getInventorySummary(@PathVariable("itemSku") String itemSku,
      @PathVariable("pickupPointCode") String pickupPointCode, @RequestParam("isWareHouse") boolean isWareHouse) throws Exception {
    String requestId = mandatoryParameterHelper.getRequestId();
    String merchantCode = mandatoryParameterHelper.getBusinessPartnerCode();
    log.info("Fetching inventory summary for itemSku : {} pickupPointCode : {} ", itemSku, pickupPointCode);
    InventorySummaryWebResponse inventorySummaryWebResponse =
        this.productV2Service.getInventorySummary(itemSku, isWareHouse, merchantCode, pickupPointCode);
    return new SingleBaseResponse<>(null, null, true, requestId, inventorySummaryWebResponse);
  }

  @Operation(summary ="API to edit the product info")
  @PutMapping(value = ProductV2ApiPath.EDIT_PRODUCT_INFO, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<EditProductWebResponse> editProductV2Info(@PathVariable("productSku") String productSku,
      @RequestBody @Valid @EditProductRequestValid(message = "Invalid Edit Request")
          ProductEditInfoV2WebRequest productEditInfoV2WebRequest) throws Exception {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    log.info("Method : edit product info for productSku :{} with request : {} ",
        productEditInfoV2WebRequest.getProductSku(), productEditInfoV2WebRequest);
    boolean isOnlyExternal = Boolean.parseBoolean(mandatoryParameterHelper.isExternalOnly());
    EditProductWebResponse editProductWebResponse =
      productV2Service.editProductV2Info(productEditInfoV2WebRequest, businessPartnerCode, isOnlyExternal);
    return new SingleBaseResponse<>(CollectionUtils.isNotEmpty(editProductWebResponse.getVariantsErrorList()) ?
        editProductWebResponse.getVariantsErrorList().get(0).getMessage() :
        null, CollectionUtils.isNotEmpty(editProductWebResponse.getVariantsErrorList()) ?
        editProductWebResponse.getVariantsErrorList().get(0).getCode() :
        null, (Objects.isNull(editProductWebResponse.getApiErrorCode()) && (CollectionUtils
        .isEmpty(editProductWebResponse.getVariantsErrorList()))), mandatoryParameterHelper.getRequestId(),
        editProductWebResponse);
  }

  @Operation(summary ="API to fetch the L3 details of a product by product sku")
  @GetMapping(value = ProductV2ApiPath.FETCH_L3_DETAILS)
  public SingleBaseResponse<ProductLevel3V2WebResponse> fetchL3ProductDetailsByProductSku(
    @PathVariable("productSku") String productSku,
    @RequestParam(required = false, defaultValue = "false") boolean isNeedCorrection)
    throws Exception {
    String storeId = mandatoryParameterHelper.getStoreId();
    String businessPartnerCode =
      mandatoryParameterHelper.getBusinessPartnerCode();
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Fetching product L3 details for productSku : {}, businessPartnerCode : {}, "
      + "isNeedCorrection: {}", productSku, businessPartnerCode, isNeedCorrection);
    ProductLevel3V2WebResponse productLevel3V2WebResponse = this.productV2Service
      .fetchL3DetailsByProductSku(storeId, productSku, businessPartnerCode, isNeedCorrection);
    return new SingleBaseResponse<>(null, null, true, requestId, productLevel3V2WebResponse);
  }

  @Operation(summary ="API to fetch L5 details of a L3")
  @PostMapping(value = ProductV2ApiPath.FETCH_L5_DETAILS, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ItemPickupPointListingL3WebResponse> getProductItemPickupPointsByProductSku(
    @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "50") int size,
    @PathVariable("productSku") String productSku,
    @RequestBody ItemPickupPointListingL3WebRequest request) {
    request.setBusinessPartnerCode(
      mandatoryParameterHelper.getBusinessPartnerCode());
    log.info("Get item pickup point list for l3 for merchant : {}, request : {} ",
      request.getBusinessPartnerCode(), request);
    Page<ItemPickupPointListingL3WebResponse> response =
      productV2Service.getItemPickupPointListingByProductSku(page, size, productSku, request);
    return new ListBaseResponse<>(null, null, true,
      mandatoryParameterHelper.getRequestId(), response.getContent(),
      new Metadata(page, size, response.getTotalElements()));
  }

  @Operation(summary = "API to fetch Bulk Process listing for a merchant with bulk process type")
  @GetMapping(value = ProductV2ApiPath.BULK_PROCESS_LISTING, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestListResponse<BulkProcessStatusListingWebResponse> fetchBulkProcessListingResponse(
    @PathVariable("bulkProcessType") String bulkProcessType,
    @RequestParam(required = false) Optional<List<String>> bulkProcessCodes,
    @RequestParam(required = false) boolean estimationsNeeded,
    @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "50") int size)
    throws Exception {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    String requestId = mandatoryParameterHelper.getRequestId();
    String storeId = mandatoryParameterHelper.getStoreId();
    log.info("Fetching Listing response for merchant : {} for Request : {} ", businessPartnerCode,
      requestId);
    Page<BulkProcessStatusListingWebResponse> webResponsePage =
      productV2Service.fetchBulkProcessListingResponse(storeId, requestId, businessPartnerCode,
        bulkProcessType, bulkProcessCodes, estimationsNeeded, page, size);
    return new GdnRestListResponse<>(null, null, true, webResponsePage.getContent(),
      new PageMetaData(size, page,
        webResponsePage.getTotalElements()), requestId);
  }

  @Operation(summary = "API to fetch Consignment Form Details corresponding to an L4")
  @GetMapping(value = ProductV2ApiPath.FETCH_CONSIGNMENT_DETAIL_BY_ITEM_SKU, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestListResponse<ConsignmentDetailWebResponse> fetchConsignmentDetailsByItemSku(
    @PathVariable("itemSku") String itemSku, @RequestParam(defaultValue = "0") int page,
    @RequestParam(defaultValue = "5") int size) throws Exception {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    String requestId = mandatoryParameterHelper.getRequestId();
    String storeId = mandatoryParameterHelper.getStoreId();
    log.info("Fetching Consignment Form Listing  for itemSku : {} for Request : {} ", itemSku,
      requestId);
    Pair<List<ConsignmentDetailWebResponse>, Integer> consignmentDetailWebResponses =
      productV2Service.fetchConsignmentDetailResponse(storeId, businessPartnerCode, itemSku, page,
        size);
    return new GdnRestListResponse<>(null, null, true, consignmentDetailWebResponses.getLeft(),
      new PageMetaData(size, page, consignmentDetailWebResponses.getRight()), requestId);
  }

  @Operation(summary = "API to fetch fetch Basic Item Details By ItemCode")
  @GetMapping(value = ProductV2ApiPath.FETCH_BASIC_ITEM_DETAILS_BY_ITEM_CODE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestListResponse<ItemCodeBasicDetailWebResponse> fetchBasicItemDetailsByItemCodes(
    @PathVariable("itemCode") String itemCode,
    @RequestParam(required = false, defaultValue = StringUtils.EMPTY) String searchKey,
    @RequestParam int page, @RequestParam int size) throws Exception {
    String requestId = mandatoryParameterHelper.getRequestId();
    String username = mandatoryParameterHelper.getUsername();
    String storeId = mandatoryParameterHelper.getStoreId();
    log.info("Fetching Basic Item Details By ItemCodes  for itemCode : {} , requestId : {}",
      itemCode, requestId);
    Page<ItemCodeBasicDetailWebResponse> webResponsePage =
      productV2Service.fetchBasicItemDetailsByItemCodes(storeId, requestId, username, itemCode,
        searchKey, page, size);
    return new GdnRestListResponse<>(null, null, true, webResponsePage.getContent(),
      new PageMetaData(size, page, webResponsePage.getTotalElements()), requestId);
  }

  @Operation(summary = "API to fetch stock details by warehouse item sku")
  @PostMapping(value = ProductV2ApiPath.FETCH_STOCK_DETAILS_BY_WAREHOUSE_ITEM_SKU, produces =
    MediaType.APPLICATION_JSON_VALUE)
  public GdnRestListResponse<WarehouseStockDetailsWebResponse> fetchStockDetailsByWarehouseItemSku(
    @RequestBody WarehouseStockDetailsRequest warehouseStockDetailsRequest) throws Exception {
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Fetching warehouse stock details by ItemCodes for itemCodes : {} , requestId : "
        + "{}",
      warehouseStockDetailsRequest, requestId);
    List<WarehouseStockDetailsWebResponse> webResponse =
      productV2Service.fetchWarehouseStockStatusByItemCode(
        warehouseStockDetailsRequest.getItemCodes());
    return new GdnRestListResponse<>(null, null, true, webResponse, null, requestId);
  }

  @Operation(summary = "Check omni channel sku exists")
  @PostMapping(value = ProductV2ApiPath.CHECK_IF_SELLER_SKU_EXISTS, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSingleResponse<ValidOmniChannelSkuWebResponse> checkOmniChannelSkuExistsInSeller(
    @RequestBody OmniChannelSkuWebRequest omniChannelSkuWebRequest) {
    GdnPreconditions.checkArgument(Optional.ofNullable(omniChannelSkuWebRequest)
      .map(OmniChannelSkuWebRequest::getOmniChannelSkus).filter(CollectionUtils::isNotEmpty)
      .isPresent(), ErrorMessages.ERR_EMPTY_OMNI_CHANNEL_SKU_LIST);
    String requestId = mandatoryParameterHelper.getRequestId();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    omniChannelSkuWebRequest.setSellerCode(businessPartnerCode);
    ValidOmniChannelSkuWebResponse validOmniChannelSkuWebResponse =
      productV2Service.checkOmniChannelSkuExistsInSeller(omniChannelSkuWebRequest);
    return new GdnRestSingleResponse<>(null, null, true, validOmniChannelSkuWebResponse, requestId);
  }
}
