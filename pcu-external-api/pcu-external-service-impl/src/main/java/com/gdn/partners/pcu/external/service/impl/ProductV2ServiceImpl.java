package com.gdn.partners.pcu.external.service.impl;

import static com.gdn.partners.pcu.external.model.Constants.ACTIVE;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;


import com.gda.mta.product.dto.OmniChannelExistsRequest;
import com.gda.mta.product.dto.ProductDataAutoFixHistoryDto;
import com.gda.mta.product.dto.response.OmniChannelMapAndSkuResponse;
import com.gda.mta.product.dto.response.ProductDataAutoFixHistoryListRequest;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.client.model.InventoryDetailStockInfoRequestDTO;
import com.gdn.partners.pcu.external.client.model.OmniChannelSkuWebRequest;
import com.gdn.partners.pcu.external.client.model.ProductL3UpdateRequest;
import com.gdn.partners.pcu.external.client.model.ValidOmniChannelSkuWebResponse;
import com.gdn.partners.pcu.external.service.UserPicService;
import com.gdn.partners.pcu.external.service.impl.config.KafkaPublisher;
import com.gdn.partners.pcu.external.service.impl.config.KafkaTopicProperties;
import com.gdn.partners.pcu.external.service.impl.exception.ValidationException;
import com.gdn.partners.pcu.external.web.model.request.ItemPickupPointDeleteWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductVariantPriceStockAndImagesWebRequest;
import com.gdn.partners.pcu.external.web.model.response.L2StockDetailResponse;
import com.gdn.partners.pcu.external.web.model.response.WarehouseStockDetailsWebResponse;
import com.gdn.partners.pcu.external.web.model.response.XProductL3SolrAutoHealEventModel;
import com.gdn.x.productcategorybase.dto.request.OmniChannelSkuRequest;
import com.gdn.x.productcategorybase.dto.response.BasicSizeChartDetailMapResponse;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;
import com.google.common.collect.Lists;
import lombok.SneakyThrows;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import com.blibli.oss.common.response.Response;
import com.gda.mta.product.dto.EditProductV2Response;
import com.gda.mta.product.dto.ProductLevel3QuickEditV2Request;
import com.gda.mta.product.dto.ProductSkuListRequest;
import com.gda.mta.product.dto.ProductSystemParameterResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailsV2Response;
import com.gda.mta.product.dto.response.ProductSuspensionHistoryResponse;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.tuple.Pair;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.fbb.core.constant.SortOrder;
import com.gdn.fbb.core.web.model.request.CountConsignmentFormsByItemSkusRequest;
import com.gdn.fbb.core.web.model.response.v3.ConsignmentStatusResponse;
import com.gdn.fbb.core.web.model.response.v3.CountConsignmentFormsByItemSkuResponse;
import com.gdn.mta.bulk.dto.BulkProcessResponse;
import com.gdn.mta.product.commons.constant.ProductLevel3SummaryCriteria;
import com.gdn.mta.product.commons.constant.UpdateProductAccessChannel;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3CountResponse;
import com.gdn.partners.pcu.external.client.feign.FbbFeign;
import com.gdn.partners.pcu.external.client.feign.PBPFeign;
import com.gdn.partners.pcu.external.client.feign.PCBFeign;
import com.gdn.partners.pcu.external.client.feign.ProductPricingFeign;
import com.gdn.partners.pcu.external.client.feign.XCampaignFeign;
import com.gdn.partners.pcu.external.client.feign.XInventoryFeign;
import com.gdn.partners.pcu.external.client.feign.XProductFeign;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.service.BulkProcessService;
import com.gdn.partners.pcu.external.service.BusinessPartnerService;
import com.gdn.partners.pcu.external.service.ProductService;
import com.gdn.partners.pcu.external.service.ProductV2Service;
import com.gdn.partners.pcu.external.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.external.web.model.request.ItemPickupPointListingL3WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemPickupPointWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductEditInfoV2WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductSummaryV2WebRequest;
import com.gdn.partners.pcu.external.web.model.request.QuickEditV2WebRequest;
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
import com.gdn.partners.pcu.external.web.model.response.WholesalePromoV2Response;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceSkuDetailListRequest;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.campaign.request.CampaignPriceRequest;
import com.gdn.x.campaign.request.CampaignPriceSkuRequest;
import com.gdn.x.campaign.response.CampaignPriceResponse;
import com.gdn.x.campaign.response.CampaignPriceSkuResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.ListRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryDetailInfoResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryStockInfoDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.ReservedStockSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.response.ItemCodeBasicDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemL4SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductCountResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.gdn.x.productcategorybase.dto.request.CategoryMultipleIdRequest;
import com.gdn.x.productcategorybase.dto.response.BasicSizeChartDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductV2ServiceImpl implements ProductV2Service {

  @Autowired
  private XProductFeign xProductFeign;

  @Autowired
  private PBPFeign pbpFeign;

  @Autowired
  private PCBFeign pcbFeign;

  @Autowired
  private XInventoryFeign xInventoryFeign;

  @Autowired
  private XCampaignFeign xCampaignFeign;

  @Autowired
  private ProductPricingFeign productPricingFeign;

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private BusinessPartnerService businessPartnerService;

  @Autowired
  private ProductService productService;

  @Autowired
  private FbbFeign fbbFeign;

  @Autowired
  private UserPicService userPicService;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Value("${inventory.api.batch.size}")
  private int inventoryApiBatchSize;

  @Value("${product.detail.page.url.prefix}")
  private String productDetailPageUrlPrefix;

  @Value("${multipickuppoint.workflow.enabled:false}")
  private boolean multiPickupPointEnabled;

  @Value("${pricing.multipickuppoint.workflow.enabled}")
  private boolean pricingMultiPickupPointEnabled;

  @Value("${cf.count.listing.enabled}")
  private boolean cfCountAtListingEnabled;

  @Value("${consignment.detail.max.fetch.days}")
  private String consignmentDetailsMaxFetchDays;

  @Value("${sort.consignment.form.detail}")
  private String sortByForConsignmentDetailListing;

  @Value("${header.Authenticator.fbb}")
  private String headerAuthenticatorFbb;

  @Value("${validate.response.for.product.l3.listing}")
  private boolean validateResponseForProductL3Listing;

  @Value("${fetch.basic.item.details.order.by}")
  private String fetchBasicItemDetailsOrderBy;

  @Value("${fetch.basic.item.details.sort.by}")
  private String fetchBasicItemDetailsSortBy;

  @Value("${size.chart.addition.for.product}")
  private boolean sizeChartAdditionForProduct;

  @Value("${validate.product.edit.accessibility}")
  private boolean validateProductEditAccessibility;

  @Value("#{${validate.product.edit.accessibility.exclusion.list}}")
  private Set<String> validateProductEditAccessibilityExclusionList;

  @Value("${auto.heal.oos.products.with.stock}")
  private boolean autoHealOosProductsWithStock;

  @Value("${populate.label.for.pwp.promo}")
  private boolean populateLabelForPwpPromo;

  @Value("${ranch.integration.enabled}")
  private boolean ranchIntegrationEnabled;

  @Override
  public Page<ProductLevel3ListingV2WebResponse> getProductL3List(
    ProductSummaryV2WebRequest request, Integer page, Integer size, boolean onlyDefaultViewConfig) {
    List<ProductLevel3ListingV2WebResponse> productLevel3ListingV2WebResponseList =
      new ArrayList<>();
    setPureExternalUserInRequestBasedOnRanchSwitch(request);
    GdnRestListResponse<ProductL3SummaryResponse> productL3SummaryResponse =
        xProductFeign.getFilterSummaryL3(page, size, onlyDefaultViewConfig,
            RequestHelper.toProductSummaryRequest(request));
    if (validateResponseForProductL3Listing) {
      ResponseHelper.validateResponseForGetProductL3(productL3SummaryResponse);
    } else {
      ResponseHelper.validateResponseForErrorCodeV2(productL3SummaryResponse);
    }
    if (CollectionUtils.isNotEmpty(productL3SummaryResponse.getContent())) {
      Map<String, InventoryStockInfoDTO> productSkuInventoryMap = new HashMap<>();
      Map<String, InventoryDetailInfoResponseDTO> itemInventoryMapL5 = new HashMap<>();
      Map<String, CampaignPriceSkuResponse> itemCampaignMap = new HashMap<>();
      Map<String, String> skuXSuspensionReasonMap = new HashMap<>();
      Map<String, String> skuXCategoryName =
        this.fetchProductSkuToMasterCategoryMap(productL3SummaryResponse.getContent());
      if (Boolean.TRUE.equals(request.getSuspended())) {
        List<String> productSkuList = productL3SummaryResponse.getContent().stream()
          .map(ProductL3SummaryResponse::getProductSku).collect(Collectors.toList());
        skuXSuspensionReasonMap.putAll(this.fetchProductSkuToSuspensionReasonMap(productSkuList));
      } else {
        GdnRestSingleResponse<ProductSystemParameterResponse> l3StockSwitch =
          pbpFeign.findSystemParameter(Constants.SHOW_L3_STOCK);
        ResponseHelper.validateResponse(l3StockSwitch);
        if (Boolean.parseBoolean(l3StockSwitch.getValue().getValue())) {
          List<String> skusForL3Stock = productL3SummaryResponse.getContent().stream().filter(
              productSummary -> (productSummary.getL5Count() != Constants.NO_VARIANTS_COUNT))
            .map(ProductL3SummaryResponse::getProductSku).collect(Collectors.toList());
          productSkuInventoryMap.putAll(
            fetchProductSkuToL3InventoryDetailMap(skusForL3Stock, request.getMerchantCode(), request.getPickupPointCodes()));
        }
        this.fetchL5IdToInventoryAndCampaignDetailsMap(productL3SummaryResponse.getContent(), itemInventoryMapL5,
          itemCampaignMap);
      }
      Map<String, Integer> pendingConsignmentCountMap =
        fetchPendingConsignmentCountMap(productL3SummaryResponse);
      productLevel3ListingV2WebResponseList = productL3SummaryResponse.getContent().stream().map(
          response -> ResponseHelper.toProductLevel3ListingV2WebResponse(response, skuXCategoryName,
              skuXSuspensionReasonMap, productSkuInventoryMap, itemInventoryMapL5, itemCampaignMap,
              pendingConsignmentCountMap,
              RequestHelper.toProductDetailPage(response.getProductSku(), productDetailPageUrlPrefix),
              populateLabelForPwpPromo)).collect(Collectors.toList());
      List<String> sizeChartCodes = productLevel3ListingV2WebResponseList.stream()
          .map(ProductLevel3ListingV2WebResponse::getSizeChartCode).filter(Objects::nonNull)
          .distinct().collect(Collectors.toList());
      Map<String, BasicSizeChartDetailResponse> basicSizeChartDetailResponseMap;
      if (sizeChartAdditionForProduct && request.isFetchSizeChartDetails()
          && CollectionUtils.isNotEmpty(sizeChartCodes)) {
        GdnRestSingleResponse<BasicSizeChartDetailMapResponse> sizeChartDetailResponse =
            pcbFeign.getBasicSizeChartDetails(sizeChartCodes);
        ResponseHelper.validateResponse(sizeChartDetailResponse);
        basicSizeChartDetailResponseMap =
            sizeChartDetailResponse.getValue().getBasicSizeChartDetailResponseMap();
        for (ProductLevel3ListingV2WebResponse productLevel3ListingV2WebResponse :
            productLevel3ListingV2WebResponseList) {
          Optional.ofNullable(productLevel3ListingV2WebResponse.getSizeChartCode())
              .map(basicSizeChartDetailResponseMap::get)
              .map(BasicSizeChartDetailResponse::getSizeChartName)
              .ifPresent(productLevel3ListingV2WebResponse::setSizeChartName);
        }
      }
      autoHealOosProductsWithStock(productL3SummaryResponse, productLevel3ListingV2WebResponseList);
      return new PageImpl<>(productLevel3ListingV2WebResponseList, PageRequest.of(page, size),
        productL3SummaryResponse.getPageMetaData().getTotalRecords());
    }
    return new PageImpl<>(productLevel3ListingV2WebResponseList, PageRequest.of(page, size),
      productL3SummaryResponse.getPageMetaData().getTotalRecords());
  }

  private void setPureExternalUserInRequestBasedOnRanchSwitch(ProductSummaryV2WebRequest request) {
    if (ranchIntegrationEnabled) {
      request.setPureExternalUser(Boolean.valueOf(mandatoryParameterHelper.isExternalOnly()));
    }
  }

  private void autoHealOosProductsWithStock(GdnRestListResponse<ProductL3SummaryResponse> productL3SummaryResponse,
      List<ProductLevel3ListingV2WebResponse> productLevel3ListingV2WebResponseList) {
    if (autoHealOosProductsWithStock) {
      try {
        Set<String> oosProductSkus = productL3SummaryResponse.getContent().stream().filter(l3SummaryResponse -> Boolean.FALSE.equals(l3SummaryResponse.getInStock()))
            .map(ProductL3SummaryResponse::getProductSku).collect(Collectors.toSet());
        Set<String> productsWithTotalStockMoreThanZero =
            productLevel3ListingV2WebResponseList.stream().filter(response -> response.getTotalStock() > 0).map(ProductLevel3ListingV2WebResponse::getProductSku).collect(Collectors.toSet());
        // This is for products with single variant
        for (ProductLevel3ListingV2WebResponse productLevel3ListingV2WebResponse : productLevel3ListingV2WebResponseList) {
          if (Objects.nonNull(productLevel3ListingV2WebResponse.getItemPickupPointSummary())) {
            int totalStock = productLevel3ListingV2WebResponse.getItemPickupPointSummary().getAvailableStockLevel1() + productLevel3ListingV2WebResponse.getItemPickupPointSummary().getAvailableStockLevel2();
            if (totalStock > 0) {
              productsWithTotalStockMoreThanZero.add(productLevel3ListingV2WebResponse.getProductSku());
            }
          }
        }
        log.info("Auto-heal for OOS products : {} ", oosProductSkus);
        oosProductSkus.stream().filter(productsWithTotalStockMoreThanZero::contains).forEach(oosProduct -> {
          kafkaProducer.send(kafkaTopicProperties.getXProductAutoHealEvent(), oosProduct,
              new XProductL3SolrAutoHealEventModel(oosProduct, mandatoryParameterHelper.getStoreId(), false,
                  System.currentTimeMillis()));
          kafkaProducer.send(kafkaTopicProperties.getPbpAutoFixHistory(), oosProduct,
              new ProductDataAutoFixHistoryListRequest(Collections.singletonList(
                  new ProductDataAutoFixHistoryDto(oosProduct, Constants.OOS_AUTO_HEAL, StringUtils.EMPTY))));
        });
      } catch (Exception e) {
        log.error("Error while trying to auto heal OOS products : ", e);
      }
    }
  }

  private Map<String, Integer> fetchPendingConsignmentCountMap(
    GdnRestListResponse<ProductL3SummaryResponse> productL3SummaryResponses) {

    List<String> fbbActiveSingleVariantL4s = productL3SummaryResponses.getContent().stream()
          .filter(productSummary -> Optional.ofNullable(productSummary)
          .filter(ProductL3SummaryResponse::isFbbActivated)
          .filter(summary -> summary.getVariantCount() == Constants.NO_VARIANTS_COUNT).isPresent())
      .map(ProductL3SummaryResponse::getItemL4SummaryResponse).filter(Objects::nonNull)
      .map(ItemL4SummaryResponse::getItemSku).filter(Objects::nonNull).collect(Collectors.toList());

    if (CollectionUtils.isEmpty(fbbActiveSingleVariantL4s) || !cfCountAtListingEnabled) {
      return Collections.emptyMap();
    }

    String merchantCode = productL3SummaryResponses.getContent().get(0).getMerchantCode();
    Response<List<CountConsignmentFormsByItemSkuResponse>> inProgressConsignmentForms =
      fbbFeign.countInProgressConsignmentFormsByItemSkus(
        CountConsignmentFormsByItemSkusRequest.builder().businessPartnerCode(merchantCode)
          .itemSkus(fbbActiveSingleVariantL4s).build(), headerAuthenticatorFbb);

    if(!ResponseHelper.validateResponseForConsignmentCount(inProgressConsignmentForms)){
      return Collections.emptyMap();
    }

    return inProgressConsignmentForms.getData().stream().collect(
      Collectors.toMap(CountConsignmentFormsByItemSkuResponse::getItemSku,
        CountConsignmentFormsByItemSkuResponse::getTotal,
        (existingCFCount, newCFCount) -> newCFCount));
  }


  @Override
  public ProductL3CountWebResponse getL3PrimaryCountsByMerchantCode(String merchantCode) {
    GdnRestSingleResponse<ProductCountResponse> productCountResponse =
      xProductFeign.getSecondaryProductCountByType(Constants.ACTIVE_STATUS, merchantCode);
    ResponseHelper.validateResponse(productCountResponse);
    GdnRestSingleResponse<ProductLevel3CountResponse> nonActiveProductCount =
      pbpFeign.getNonActiveProductCount(merchantCode, Constants.PRIMARY);
    ResponseHelper.validateResponse(nonActiveProductCount);
    Map<ProductLevel3SummaryCriteria, Long> totalItemsByCriterias =
      nonActiveProductCount.getValue().getTotalItemsByCriterias();
    log.info("IN_PROGRESS : {}, NR : {}",
      totalItemsByCriterias.get(ProductLevel3SummaryCriteria.IN_PROGRESS),
      totalItemsByCriterias.get(ProductLevel3SummaryCriteria.NEED_CORRECTION));
    return ProductL3CountWebResponse.builder().all(
        productCountResponse.getValue().getActive() + productCountResponse.getValue().getOutOfStock())
      .inReview(totalItemsByCriterias.get(ProductLevel3SummaryCriteria.IN_PROGRESS))
      .needCorrection(totalItemsByCriterias.get(ProductLevel3SummaryCriteria.NEED_CORRECTION))
      .businessPartnerCode(merchantCode).build();
  }

  @Override
  public List<WholesalePromoV2Response> getWholesaleStatusByRequest(String storeId,
    String requestId, WholesaleStatusV2Request wholesaleStatusV2Request) {
    GdnRestListResponse<WholesalePriceSkuResponse> wholesalePriceSkuResponses;
    if (multiPickupPointEnabled || pricingMultiPickupPointEnabled) {
      wholesalePriceSkuResponses = this.productPricingFeign.getWholesalePriceSkuDetailV2(storeId, requestId,
          new WholesalePriceSkuDetailListRequest(wholesaleStatusV2Request.getItemPickupPointWebRequestList().stream()
              .map(ItemPickupPointWebRequest::getItemSku).collect(Collectors.toSet()), RequestHelper
              .toItemDTOLIstFromItemPickupPointRequest(wholesaleStatusV2Request.getItemPickupPointWebRequestList())));
    } else {
      wholesalePriceSkuResponses = this.productPricingFeign.getWholesalePriceSkuDetail(storeId, requestId,
          new WholesalePriceSkuDetailListRequest(wholesaleStatusV2Request.getItemPickupPointWebRequestList().stream()
              .map(ItemPickupPointWebRequest::getItemSku).collect(Collectors.toSet()), RequestHelper
              .toItemDTOLIstFromItemPickupPointRequest(wholesaleStatusV2Request.getItemPickupPointWebRequestList())));
    }
    ResponseHelper.validateResponse(wholesalePriceSkuResponses);
    return ResponseHelper.toWholesalePromoV2ResponseList(wholesalePriceSkuResponses.getContent());
  }

  @Override
  public ProductL3CountWebResponse getL3CountsByType(String businessPartnerCode, String type) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(businessPartnerCode), ErrorMessages.ERR_MER_CODE_NULL);
    log.info("Get the l3 product counts for the type: {} and merchantCode : {}", type, businessPartnerCode);
    if (Constants.ACTIVE_STATUS.equalsIgnoreCase(type)) {
      GdnRestSingleResponse<ProductCountResponse> productCountResponse =
          xProductFeign.getSecondaryProductCountByType(type, businessPartnerCode);
      ResponseHelper.validateResponse(productCountResponse);
      return ResponseHelper.toProductL3CountWebResponseByType(productCountResponse.getValue(), businessPartnerCode,
          type, null);
    } else if (Constants.INACTIVE_STATUS.equalsIgnoreCase(type)) {
      GdnRestSingleResponse<ProductCountResponse> productCountResponse =
          xProductFeign.getSecondaryProductCountByType(type, businessPartnerCode);
      ResponseHelper.validateResponse(productCountResponse);
      GdnRestSingleResponse<ProductLevel3CountResponse> nonActiveProductCount =
          pbpFeign.getNonActiveProductCount(businessPartnerCode, Constants.SECONDARY);
      ResponseHelper.validateResponse(nonActiveProductCount);
      return ResponseHelper.toProductL3CountWebResponseByType(productCountResponse.getValue(), businessPartnerCode,
          type, nonActiveProductCount.getValue());
    } else {
      log.error("Not a valid parameter type for L3 product counts : {}", type);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.INVALID_REQUEST);
    }
  }

  @Override
  public InventorySummaryWebResponse getInventorySummary(String itemSku, boolean isWareHouse, String merchantCode,
      String pickupPointCode) throws Exception {
    InventorySummaryWebResponse response = new InventorySummaryWebResponse();
    InventoryDetailInfoRequestDTO infoRequestDTO = new InventoryDetailInfoRequestDTO();
    infoRequestDTO.setWebMerchantCode(merchantCode);
    infoRequestDTO.setWebItemSku(itemSku);
    infoRequestDTO.setPickupPointCode(pickupPointCode);
    GdnRestListResponse<InventoryDetailInfoResponseDTO> inventoryInfoResponseListResponse =
        xInventoryFeign.findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Arrays.asList(infoRequestDTO)));
    ResponseHelper.validateResponse(inventoryInfoResponseListResponse);
    GdnRestSingleResponse<ReservedStockSummaryResponse> reservedStockResponse =
        xInventoryFeign.reservedStockSummaryByWebSKUAndPickupPointCode(itemSku, pickupPointCode, isWareHouse);
    ResponseHelper.validateResponse(reservedStockResponse);
    if (CollectionUtils.isNotEmpty(inventoryInfoResponseListResponse.getContent())) {
      response = ResponseHelper.getInventorySummary(inventoryInfoResponseListResponse.getContent().get(0),
          reservedStockResponse.getValue(), isWareHouse);
    }
    return response;
  }

  @Override
  public EditProductWebResponse editProductV2Info(ProductEditInfoV2WebRequest webRequest,
    String businessPartnerCode, boolean isOnlyExternal) throws Exception {
    RequestHelper.validateProductEditAccessibility(validateProductEditAccessibility,
        validateProductEditAccessibilityExclusionList, mandatoryParameterHelper.getChannelId());
    validateDefaultPickupPoints(webRequest.getDefaultPickupPointCode());
    validateDeletePickupPoints(webRequest.getDeletePickupPoints());
    validateAddPickupPoints(webRequest.getAddPickupPoints());
    validateProductItems(webRequest.getProductItems());
    ProductL3UpdateRequest request =
        RequestHelper.toProductL3RequestFromProductEditInfoWebRequest(webRequest, ranchIntegrationEnabled);
      ProfileResponse profileResponse =
        businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
      RequestHelper.profileResponseAndRequestValidation(profileResponse, request);
      request.setBusinessPartnerCode(businessPartnerCode);
      request.setAccessChannel(UpdateProductAccessChannel.MTA_WEB_UPDATE_DETAIL.getDesc());
      request.setCnc(webRequest.getCncActivated());
      request.setFbbActiveAtL3Level(webRequest.getFbb());
      GdnRestSingleResponse<EditProductV2Response> response = pbpFeign.editProductV2Info(request, request.getProductSku(), isOnlyExternal);
      ResponseHelper.validateResponseForErrorCodeV2(response);
      return ResponseHelper.getEditProductWebResponseWebResponse(response.getValue());
  }

  private void validateDefaultPickupPoints(String pickupPoint) throws Exception {
    try {
      if (StringUtils.isEmpty(pickupPoint)) {
        return;
      }
      HashSet<String> defaultPickupPointHashset = new HashSet<>();
      defaultPickupPointHashset.add(pickupPoint);
      userPicService.validateUserPicPickupPoints(defaultPickupPointHashset);
    } catch (ValidationException validationException) {
      log.warn("#ProductV2ServiceImpl validateDefaultPickupPoints with pickupPoint : {}",
          pickupPoint);
      throw new ValidationException(ErrorMessages.NO_ACCESS_TO_PICKUP_POINT);
    }
  }

  private void validateDeletePickupPoints(
      List<ItemPickupPointDeleteWebRequest> pickupPointDeleteRequests) throws Exception {
    try {
      if (CollectionUtils.isEmpty(pickupPointDeleteRequests)) {
        return;
      }
      Set<String> pickupPointRequest =
          pickupPointDeleteRequests.stream().map(ItemPickupPointDeleteWebRequest::getPickupPointId)
              .collect(Collectors.toSet());
      userPicService.validateUserPicPickupPoints(pickupPointRequest);
    } catch (ValidationException validationException) {
      log.warn(
          "#ProductV2ServiceImpl validateDeletePickupPoints with pickupPointDeleteRequests : {}",
          pickupPointDeleteRequests);
      throw new ValidationException(ErrorMessages.NO_ACCESS_TO_PICKUP_POINT);
    }
  }

  @SneakyThrows
  private void validateAddPickupPoints(
      List<ItemPickupPointWebRequest> itemPickupPointWebRequestList) throws Exception {
    try {
      if (CollectionUtils.isEmpty(itemPickupPointWebRequestList)) {
        return;
      }
      Set<String> pickupPointRequest =
          itemPickupPointWebRequestList.stream().map(ItemPickupPointWebRequest::getPickupPointId)
              .collect(Collectors.toSet());
      userPicService.validateUserPicPickupPoints(pickupPointRequest);
    } catch (ValidationException validationException) {
      log.warn(
          "#ProductV2ServiceImpl validateAddPickupPoints with itemPickupPointWebRequestList : {}",
          itemPickupPointWebRequestList);
      throw new ValidationException(ErrorMessages.NO_ACCESS_TO_PICKUP_POINT);
    }
  }

  private void validateProductItems(
      List<ProductVariantPriceStockAndImagesWebRequest> productVariantPriceStockAndImagesWebRequestList)
      throws Exception {
    try {
      if (CollectionUtils.isEmpty(productVariantPriceStockAndImagesWebRequestList)) {
        return;
      }

      Set<String> pickupPointRequest = RequestHelper.validateModifiedPickupPoints(
          productVariantPriceStockAndImagesWebRequestList);

      if(CollectionUtils.isEmpty(pickupPointRequest)) {
        return;
      }

      userPicService.validateUserPicPickupPoints(pickupPointRequest);
    } catch (ValidationException validationException) {
      log.warn(
          "#ProductV2ServiceImpl validateProductItems with "
              + "productVariantPriceStockAndImagesWebRequestList : {}",
          productVariantPriceStockAndImagesWebRequestList);
      throw new ValidationException(ErrorMessages.NO_ACCESS_TO_PICKUP_POINT);
    }
  }

  @Override
  public ProductLevel3V2WebResponse fetchL3DetailsByProductSku(String storeId, String productSku,
    String businessPartnerCode, boolean isNeedCorrection) throws Exception {
    GdnPreconditions
      .checkArgument(productSku.startsWith(businessPartnerCode), ErrorMessages.INVALID_GDN_SKU);
    ProfileResponse businessPartner =
      businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    if (Objects.isNull(businessPartner) || !ACTIVE
      .equals(businessPartner.getMerchantStatus())) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
        ErrorMessages.BUSINESS_PARTNER_NOT_ACTIVE);
    }
    GdnRestSingleResponse<ProductLevel3DetailsV2Response> response =
      pbpFeign.fetchL3V2ProductDetailsByProductSku(productSku, isNeedCorrection);
    ResponseHelper.validateResponse(response);
    ProductLevel3V2WebResponse productL3DetailWebResponse =
      ResponseHelper.toProductL3DetailWebResponse(response.getValue());
    String merchantType = ResponseHelper.getMerchantTypeFromPurchaseTermAndInventoryFulfillment(
      businessPartner.getCompany().getPurchaseTerm(),
      businessPartner.getCompany().getInventoryFulfillment());
    if (Constants.MERCHANT_TYPE_TD.equalsIgnoreCase(merchantType)) {
      productL3DetailWebResponse.setCogsViewable(true);
    }
    return productL3DetailWebResponse;
  }

  @Override
  public Page<ItemPickupPointListingL3WebResponse> getItemPickupPointListingByProductSku(int page,
    int size, String productSku,
    ItemPickupPointListingL3WebRequest itemPickupPointListingL3WebRequest) {
    return productService.getItemPickupPointListingByProductSku(page, size, false, productSku,
      itemPickupPointListingL3WebRequest, false);
  }

  @Override
  public Page<BulkProcessStatusListingWebResponse> fetchBulkProcessListingResponse(String storeId,
    String requestId, String businessPartnerCode, String bulkProcessType,
    Optional<List<String>> bulkProcessCodes, boolean estimationsNeeded, int page, int size)
    throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(businessPartnerCode),
      ErrorMessages.ERR_MER_CODE_NULL);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(bulkProcessType),
      ErrorMessages.EMPTY_BULK_PROCESS_TYPE);
    ProfileResponse profileResponse =
      businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    GdnPreconditions.checkArgument(
      Objects.nonNull(profileResponse) && ACTIVE.equals(profileResponse.getMerchantStatus()),
      ErrorMessages.INVALID_BUSINESS_PARTNER_CODE + businessPartnerCode);
    return bulkProcessService.fetchBulkProcessListingWebResponse(storeId, requestId,
      businessPartnerCode, bulkProcessType, bulkProcessCodes, estimationsNeeded, page, size);
  }

  @Override
  public Pair<List<ConsignmentDetailWebResponse>, Integer> fetchConsignmentDetailResponse(String storeId,
    String businessPartnerCode, String itemSku, int page, int size) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(businessPartnerCode),
      ErrorMessages.ERR_MER_CODE_NULL);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(itemSku),
      ErrorMessages.ERR_ITEM_SKU_NULL);
    int incrementedPageForFbbFeign = ++page;
    Pair<String, String> dateRange = getConsignmentDateRange(consignmentDetailsMaxFetchDays);
    // fbb core ciel 0 page number to 1, handling feign request
    Response<List<ConsignmentStatusResponse>> consignmentFormsDetail =
      fbbFeign.partnerConsignmentFormsByFilterForProduct(sortByForConsignmentDetailListing,
        SortOrder.ASC, incrementedPageForFbbFeign, size, itemSku, businessPartnerCode,
        dateRange.getLeft(), dateRange.getRight(), headerAuthenticatorFbb);
    ResponseHelper.validateResponseForFbbConsignmentForm(consignmentFormsDetail);
    log.info("Response From Fbb for CF corresponding to L4 : {} was : {} ", itemSku,
      consignmentFormsDetail.getData());
    List<ConsignmentDetailWebResponse> consignmentDetailWebResponses =
      ResponseHelper.toConsignmentDetailWebResponse(consignmentFormsDetail);
    return Pair.of(consignmentDetailWebResponses,
      consignmentFormsDetail.getPaging().getTotalItem());
  }

  @Override
  public com.gda.mta.product.dto.response.ProductCountResponse getProductCountForProductLimit(String storeId,
      String businessPartnerCode) {
    GdnRestSingleResponse<com.gda.mta.product.dto.response.ProductCountResponse> productCountResponse =
        pbpFeign.getProductCountForProductLimit(businessPartnerCode);
    if (Objects.isNull(productCountResponse) || !productCountResponse.isSuccess() || Objects
        .isNull(productCountResponse.getValue())) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.ERROR_PRODUCT_COUNT);
    }
    return productCountResponse.getValue();
  }

  @Override
  public Page<ItemCodeBasicDetailWebResponse> fetchBasicItemDetailsByItemCodes(String storeId,
    String requestId, String username, String itemCode, String searchKey, int page, int size) {
    GdnRestListResponse<ItemCodeBasicDetailResponse> basicItemDetailsByItemCodes =
        xProductFeign.fetchBasicItemDetailsByItemCodes(storeId, requestId, username, page, size,
            fetchBasicItemDetailsSortBy, fetchBasicItemDetailsOrderBy, itemCode, searchKey);
    ResponseHelper.validateResponse(basicItemDetailsByItemCodes);
    return ResponseHelper.toBasicItemDetailsWebResponse(basicItemDetailsByItemCodes,
      productDetailPageUrlPrefix, page, size);
  }

  /**
   * @param itemCodes not null
   * @return
   */
  @Override
  public List<WarehouseStockDetailsWebResponse> fetchWarehouseStockStatusByItemCode(List<String> itemCodes) {
    List<WarehouseStockDetailsWebResponse> warehouseStockDetailsResponse = new ArrayList<>();
    for (String itemCode : itemCodes) {
      GdnRestSingleResponse<L2StockDetailResponse> response =
        xInventoryFeign.findStockAvailabilityByWarehouseItemSku(itemCode);
      ResponseHelper.validateResponse(response);
      warehouseStockDetailsResponse.add(WarehouseStockDetailsWebResponse.builder().itemCode(itemCode)
        .warehouseStockAvailable(
          response.getValue().isDistributionWarehouseAvailable() || response.getValue()
            .isNonDistributionWarehouseAvailable()).build());
    }
    return warehouseStockDetailsResponse;
  }

  @Override
  public BulkProcessResponse getBulkProcessResponse(String bulkProcessCode) {
    return bulkProcessService.getBulkProcessResponseByProcessCode( bulkProcessCode);
  }

  private static Pair<String, String> getConsignmentDateRange(String addDays) {
    try {
      SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
      Calendar cal = Calendar.getInstance();
      String endDate = dateFormat.format(cal.getTime());
      cal.add(Calendar.DATE, Integer.valueOf(addDays) * -1);
      String startDate = dateFormat.format(cal.getTime());
      return Pair.of(startDate, endDate);
    } catch (Exception e) {
      log.error("Error while retrieving Start Date and End Date. ", e);
      return Pair.of("", "");
    }
  }

  private void fetchL5IdToInventoryAndCampaignDetailsMap(
    List<ProductL3SummaryResponse> productSummaryList,
    Map<String, InventoryDetailInfoResponseDTO> itemInventoryMapL5,
    Map<String, CampaignPriceSkuResponse> itemCampaignMap) {
    List<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestList = new ArrayList<>();
    List<CampaignPriceSkuRequest> campaignPriceSkuRequests = new ArrayList<>();
    for (ProductL3SummaryResponse productSummary : productSummaryList) {
      if (productSummary.getL5Count() == Constants.NO_VARIANTS_COUNT && Objects.nonNull(
        productSummary.getItemL4SummaryResponse())) {
        InventoryDetailInfoRequestDTO inventoryRequest = new InventoryDetailInfoRequestDTO();
        inventoryRequest.setWebItemSku(productSummary.getItemL4SummaryResponse().getItemSku());
        inventoryRequest.setWebMerchantCode(productSummary.getMerchantCode());
        inventoryRequest.setPickupPointCode(
          productSummary.getItemL4SummaryResponse().getPickupPointCode());
        inventoryDetailInfoRequestList.add(inventoryRequest);
        CampaignPriceSkuRequest campaignPriceRequest =
          new CampaignPriceSkuRequest(productSummary.getItemL4SummaryResponse().getItemSku(),
            productSummary.getCategoryCode(),
            productSummary.getItemL4SummaryResponse().getPickupPointCode(), productSummary.getItemL4SummaryResponse().getPrice()
              .stream().findFirst().orElse(new PriceDTO()).getOfferPrice());
        campaignPriceSkuRequests.add(campaignPriceRequest);
      }
    }
    if (CollectionUtils.isNotEmpty(inventoryDetailInfoRequestList)) {
      GdnRestListResponse<InventoryDetailInfoResponseDTO> inventoryL5Response =
        xInventoryFeign.findDetailByWebMerchantCodeAndWebItemSku(
          new ListRequestDTO<>(inventoryDetailInfoRequestList));
      ResponseHelper.validateResponse(inventoryL5Response);
      itemInventoryMapL5.putAll(inventoryL5Response.getContent().stream().filter(
        inventoryDetailInfoResponseDTO -> Objects.nonNull(
          inventoryDetailInfoResponseDTO.getWebInventoryResponse())).collect(Collectors.toMap(
        inventoryL5 -> RequestHelper.toL5Id(inventoryL5.getWebInventoryResponse().getWebItemSku(),
          inventoryL5.getWebInventoryResponse().getPickupPointCode()), Function.identity())));
    }
    if (CollectionUtils.isNotEmpty(campaignPriceSkuRequests)) {
      GdnRestSingleResponse<CampaignPriceResponse> campaignL5Response =
        xCampaignFeign.getCampaignPriceInfoV2(
          CampaignPriceRequest.builder().campaignPriceSkuRequestList(campaignPriceSkuRequests)
            .build());
      ResponseHelper.validateResponse(campaignL5Response);
      if (CollectionUtils.isNotEmpty(campaignL5Response.getValue().getItemInfoToPriceResponse())) {
      itemCampaignMap.putAll(campaignL5Response.getValue().getItemInfoToPriceResponse().stream()
        .collect(Collectors.toMap(
          campaignPriceSkuResponse -> RequestHelper.toL5Id(campaignPriceSkuResponse.getItemSku(),
            campaignPriceSkuResponse.getPickUpPointCode()), Function.identity())));
      }
    }
  }

  private Map<String, InventoryStockInfoDTO> fetchProductSkuToL3InventoryDetailMap(
    List<String> productSkuList, String merchantCode, List<String> pickupPointList) {
    List<List<String>> productSkuPartition =
      Lists.partition(productSkuList, inventoryApiBatchSize);
    List<InventoryStockInfoDTO> inventoryDTOList = new ArrayList<>();
    for (List<String> productSkus : productSkuPartition) {
      GdnRestListResponse<InventoryStockInfoDTO> inventoryL3Response =
          xInventoryFeign.findDetailByWebProductSkus(!Boolean.parseBoolean(mandatoryParameterHelper.isExternalOnly()),
              new InventoryDetailStockInfoRequestDTO(merchantCode, productSkus, pickupPointList));
      ResponseHelper.validateResponse(inventoryL3Response);
      inventoryDTOList.addAll(inventoryL3Response.getContent());
    }
    return inventoryDTOList.stream()
      .collect(Collectors.toMap(InventoryStockInfoDTO::getWebProductSku, Function.identity()));
  }

  private Map<String, String> fetchProductSkuToMasterCategoryMap(
    List<ProductL3SummaryResponse> summaryResponseList) {
    Map<String, String> categoryMap = new HashMap<>();
    Set<String> categoryCodeSet = summaryResponseList.stream().filter(
        productL3SummaryResponse -> Objects.nonNull(productL3SummaryResponse.getCategoryCode()))
      .map(ProductL3SummaryResponse::getCategoryCode).collect(Collectors.toSet());
    CategoryMultipleIdRequest categoryNameRequest = new CategoryMultipleIdRequest();
    categoryNameRequest.setCategoryCode(new ArrayList<>(categoryCodeSet));
    GdnRestSingleResponse<CategoryNamesResponse> categoryResponse =
      pcbFeign.getCategoryNames(categoryNameRequest, 0, 50);
    ResponseHelper.validateResponse(categoryResponse);
    summaryResponseList.forEach(productResponse -> categoryMap.put(productResponse.getProductSku(),
      categoryResponse.getValue().getCategoryMap().get(productResponse.getCategoryCode())));
    return categoryMap;
  }

  private Map<String, String> fetchProductSkuToSuspensionReasonMap(List<String> productSkuList) {
    GdnRestListResponse<ProductSuspensionHistoryResponse> suspensionResponseList =
      pbpFeign.fetchProductSuspensionHistory(new ProductSkuListRequest(productSkuList));
    ResponseHelper.validateResponse(suspensionResponseList);
    return suspensionResponseList.getContent().stream().collect(
      Collectors.toMap(ProductSuspensionHistoryResponse::getProductSku,
        ProductSuspensionHistoryResponse::getReason));
  }

  @Override
  public void updateItemListing(String productSku, List<QuickEditV2WebRequest> quickEditV2WebRequests,
      boolean isExternalOnly) {
    ProductLevel3QuickEditV2Request productLevel3QuickEditV2Request =
      ProductLevel3QuickEditV2Request.builder()
        .quickEditV2Requests(RequestHelper.toQuickEditV2Requests(quickEditV2WebRequests)).build();
    GdnBaseRestResponse response =
      pbpFeign.itemListingUpdateV2(productSku, productLevel3QuickEditV2Request, isExternalOnly);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public ValidOmniChannelSkuWebResponse checkOmniChannelSkuExistsInSeller(
    OmniChannelSkuWebRequest omniChannelSkuWebRequest) {
    OmniChannelExistsRequest omniChannelSkuRequest = RequestHelper.mapToOmniChannelSkuRequest(omniChannelSkuWebRequest);
    GdnRestSingleResponse<OmniChannelMapAndSkuResponse> response =
        pbpFeign.checkOmniChannelSkuExistsInSeller(omniChannelSkuRequest);
    ResponseHelper.validateResponse(response);
    return ResponseHelper.mapToValidOmniChannelSkuWebResponse(response.getValue());
  }
}
