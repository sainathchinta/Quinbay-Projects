package com.gdn.partners.pbp.service.productlevel3;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import com.gdn.mta.product.service.ProductAppealService;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.gda.mta.product.dto.AuditTrailDto;
import com.gda.mta.product.dto.ItemSkuPickupPointSyncStockDto;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.response.AvailableToCopyItemDetailsResponse;
import com.gda.mta.product.dto.response.AvailableToCopyProductDetailsResponse;
import com.gda.mta.product.dto.response.ProductBusinessPartnerAndItemViewConfigDto;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.ResignSellerDomainEvent;
import com.gdn.mta.product.commons.constant.ProductLevel3WipSummaryCriteria;
import com.gdn.mta.product.config.ApplicationProperties;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductItemSyncStatus;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;
import com.gdn.mta.product.entity.ProductLevel3Image;
import com.gdn.mta.product.entity.ProductLevel3ImageBundle;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.entity.ProductLevel3SummaryMinified;
import com.gdn.mta.product.enums.ProductSyncStatus;
import com.gdn.mta.product.enums.SuspensionStatus;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductLevel3Repository;
import com.gdn.mta.product.repository.ProductStockAlertRepository;
import com.gdn.mta.product.service.ErrorMessages;
import com.gdn.mta.product.service.ProductBusinessPartnerService;
import com.gdn.mta.product.service.ProductItemBusinessPartnerService;
import com.gdn.mta.product.service.ProductItemSyncService;
import com.gdn.mta.product.service.ProductSystemParameterService;
import com.gdn.mta.product.service.UpdatedProductHistoryService;
import com.gdn.mta.product.service.util.MapperUtil;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Item;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3ItemSearch;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.productPricing.ProductPricingOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.product.pricing.web.model.dto.ItemInfoDto;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import com.gdn.x.businesspartner.commons.enums.InventoryFulfillment;
import com.gdn.x.businesspartner.commons.enums.PurchaseTerms;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryBaseRequest;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.UpdateSyncStockByWebItemSkuRequestDTO;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import com.gdn.x.product.rest.web.model.request.ItemsSummaryDetailRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.SimpleSetStringRequest;
import com.gdn.x.product.rest.web.model.response.ActiveProductResponse;
import com.gdn.x.product.rest.web.model.response.ItemImagesListResponse;
import com.gdn.x.product.rest.web.model.response.ItemImagesResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.SimpleMapStringResponse;
import com.gdn.x.productcategorybase.domain.event.model.VatUpdateHistoryDomainEventModel;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.google.common.collect.Lists;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductLevel3ServiceBean implements ProductLevel3Service {

  private static final String UPDATE_ARCHIVED_ERROR =
      "Tidak berhasil update  product yang telah diarsipkan";

  private static final String SKU_SEPARATOR = "-";

  private static final String DOT_SEPARATOR = ".";

  private static final String HTML = "html";

  @Autowired
  private ProductLevel3InventoryService productLevel3InventoryService;

  @Autowired
  ProductLevel3Repository productLevel3Repository;

  @Autowired
  private ProductLevel3Converter productLevel3Converter;

  @Autowired
  private UpdatedProductHistoryService updatedProductHistoryService;

  @Autowired
  @Qualifier("productLevel3DirectAggregatorService")
  private ProductLevel3AggregatorServiceOld productLevel3DirectAggregatorService;

  @Autowired
  @Qualifier("productLevel3MaterializedViewAggregatorService")
  private ProductLevel3AggregatorServiceOld productLevel3MaterializedViewAggregatorService;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private ProductStockAlertRepository productStockAlertRepository;

  @Autowired
  private ProductItemSyncService productItemSyncService;

  @Autowired
  private ApplicationProperties applicationProperties;

  @Autowired
  private ProductOutbound productOutbound;

  @Autowired
  private ProductCollectionRepository productCollectionRepository;

  @Autowired
  private ProductSystemParameterService productSystemParameterService;

  @Autowired
  private XProductOutbound xProductOutbound;

  @Autowired
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Autowired
  private ProductItemWholesalePriceService productItemWholesalePriceService;

  @Autowired
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Autowired
  private ProductItemBusinessPartnerService productItemBusinessPartnerService;

  @Autowired
  private ProductAppealService productAppealService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private ProductPricingOutbound productPricingOutbound;

  @Autowired
  private MapperUtil mapperUtil;

  @Value("${set.product.name.in.audit}")
  private boolean setProductNameInAudit;

  @Value("${validate.product.activation.done.once}")
  private boolean validateProductActivationDoneOnce;

  @Value(value = "${schedules.add.edit.enabled}")
  private boolean schedulesAddEditEnabled;

  @Value(value = "${skip.setting.product.item.id.for.null.case}")
  private boolean skipSettingProductItemIdForNullCase;

  @Value("${wholesale.price.fetch.batch.size}")
  private int wholesalePriceFetchBatchSize;

  @Value("${new.wholesale.flow.nr.enabled}")
  private boolean newWholesaleFlowNrEnabled;

  @Override
  public void updateSyncStockByBusinessPartnerCode(String businessPartnerCode, boolean syncStock)
      throws Exception {
    this.productLevel3InventoryService.updateSyncStockByBusinessPartnerCode(businessPartnerCode,
        syncStock);
  }

  @Override
  public void updateSyncStockByBusinessPartnerCodeAndGdnSku(String businessPartnerCode,
      String gdnSku, boolean syncStock) throws Exception {
    ItemSummaryResponse savedProductData = productLevel3Repository.findSummaryByGdnSku(gdnSku);
    GdnPreconditions.checkArgument(!savedProductData.getArchived(), UPDATE_ARCHIVED_ERROR);
    UpdateSyncStockByWebItemSkuRequestDTO request = new UpdateSyncStockByWebItemSkuRequestDTO();
    request.setWebItemSku(gdnSku);
    request.setWebMerchantCode(businessPartnerCode);
    request.setSyncStock(syncStock);
    request.setInventoryBaseRequest(new InventoryBaseRequest());
    productLevel3InventoryService.updateSyncStockByBusinessPartnerCodeAndGdnSku(businessPartnerCode, gdnSku, syncStock,
        Collections.singletonList(
            new ItemSkuPickupPointSyncStockDto(gdnSku, savedProductData.getPickupPointCode(), syncStock)));
  }

  @Override
  public Page<ProductLevel3ImageBundle> findImageBundleByFilter(ItemSummaryRequest filter,
      Pageable pageRequest, SortOrder sort) throws Exception {
    Boolean value = Boolean.valueOf(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.IMAGE_BUNDLE_SWITCH).getValue());
    if (value) {
      List<ProductLevel3ImageBundle> imageBundles = new ArrayList<>();
      SimpleSetStringRequest simpleSetStringRequest = new SimpleSetStringRequest();
      simpleSetStringRequest.setValue(new HashSet<>(filter.getItemSkus()));
      List<ItemImagesListResponse> listOfImagesByItemSkus =
          xProductOutbound.getListOfImagesByItemSkus(simpleSetStringRequest);
      for (ItemImagesListResponse itemImagesListResponse : listOfImagesByItemSkus) {
        ProductLevel3ImageBundle imageBundle = new ProductLevel3ImageBundle();
        imageBundle.setGdnSku(itemImagesListResponse.getItemSku());
        imageBundle.setImages(new ArrayList<>());
        for (ItemImagesResponse masterDataImage : itemImagesListResponse.getImagesResponseList()) {
          imageBundle.getImages().add(
              new ProductLevel3Image(masterDataImage.isMainImage(), masterDataImage.getSequence(),
                  masterDataImage.getLocationPath()));
        }
        imageBundles.add(imageBundle);
      }
      return new PageImpl<>(imageBundles, pageRequest, listOfImagesByItemSkus.size());
    } else {
      List<ProductLevel3ImageBundle> imageBundles = new ArrayList<>();
      Page<ItemSummaryResponse> productDatas =
          this.productLevel3Repository.findSummaryByFilter(filter, pageRequest, sort);
      for (ItemSummaryResponse productData : productDatas.getContent()) {
        ProductLevel3ImageBundle imageBundle = new ProductLevel3ImageBundle();
        imageBundle.setGdnSku(productData.getItemSku());
        imageBundle.setImages(new ArrayList<>());
        for (MasterDataItemImageDTO masterDataImage : productData.getMasterDataItemImages()) {
          imageBundle.getImages().add(
              new ProductLevel3Image(masterDataImage.isMainImage(), masterDataImage.getSequence(),
                  masterDataImage.getLocationPath()));
        }
        imageBundles.add(imageBundle);

      }
      return new PageImpl<>(imageBundles, pageRequest, productDatas.getTotalElements());
    }
  }

  @Override
  public Page<ProductLevel3Summary> findSummaryByFilter(ProductLevel3SummaryFilter filterRequest, Pageable pageRequest,
      SortOrder sort) throws Exception {
    return productLevel3DirectAggregatorService.aggregateProductLevel3Summary(filterRequest, pageRequest, sort);
  }

  @Override
  public Page<AvailableToCopyProductDetailsResponse> productsAvailableToBeCopied(ProductLevel3SummaryFilter filterRequest,
    String businessPartnerCode, Pageable pageRequest) throws Exception {

    SummaryFilterRequest summaryFilterRequest = SummaryFilterRequest.builder()
      .businessPartnerCode(filterRequest.getBusinessPartnerCode())
      .suspensionStatus(SuspensionStatus.ACTIVE.toString())
      .searchKeyword(Optional.ofNullable(filterRequest.getSearchKey())
        .map(String::trim)
        .map(String::toUpperCase)
        .orElse(StringUtils.EMPTY))
      .nameKey(Optional.ofNullable(filterRequest.getItemName()).orElse(StringUtils.EMPTY))
      .pickupPointCodes(Collections.emptyList())
      .categoryCodes(
        Optional.ofNullable(filterRequest.getCategoryCode()).map(Arrays::asList).orElse(Collections.emptyList()))
      .build();
    log.info("SummaryFilterRequest:{}", summaryFilterRequest);

    Page<ActiveProductResponse> activeProductResponses = productLevel3Repository
      .getAllProducts(summaryFilterRequest, pageRequest, summaryFilterRequest.getCategoryCodes(), StringUtils.EMPTY, false);
    log.info("activeProductResponses:{} for businessPartnerCode {} and searchKey {}", activeProductResponses,
      filterRequest.getBusinessPartnerCode(), filterRequest.getSearchKey());
    activeProductResponses = excludeNullProductCodeProductsAndReviewPendingProducts(activeProductResponses, pageRequest,
      filterRequest.getBusinessPartnerCode());
    log.info("after excluding null ProductCode products and review pending product activeProductResponses:{}", activeProductResponses);
    List<AvailableToCopyProductDetailsResponse> availableCopiedProductResponses = new ArrayList<>();
    if (Objects.nonNull(activeProductResponses.getContent()) && CollectionUtils.isNotEmpty(activeProductResponses.getContent())) {

      int totalItemSkuCount = activeProductResponses.getContent()
        .stream()
        .mapToInt(ActiveProductResponse::getItemCount)
        .sum();

      Map<String, ActiveProductResponse> activeProductResponseMap = activeProductResponses.getContent()
        .stream()
        .collect(Collectors.toMap(ActiveProductResponse::getProductSku, Function.identity(), (first, second) -> second));

      List<AvailableToCopyProductDetailsResponse> availableCopiedProductDetails = this.getAllItemSkusByProductSkus(
        activeProductResponseMap, totalItemSkuCount, filterRequest.getBusinessPartnerCode(),
        filterRequest.getSearchKey());

      List<String> availableProductItems = new ArrayList<>();

      availableCopiedProductDetails.stream()
        .map(
          productDetails -> productDetails.getItemDetails().stream().map(AvailableToCopyItemDetailsResponse::getItemSku)
            .collect(Collectors.toList()))
        .forEach(availableProductItems::addAll);

      List<ProductItemSyncStatus> productItemSyncStatus = productItemSyncService
        .findSyncStatusByItemSkuAndLinkedPartner(filterRequest.getStoreId(), businessPartnerCode,
          filterRequest.getBusinessPartnerCode(), availableProductItems);

      Map<String, ProductItemSyncStatus> availableProductStatus = productItemSyncStatus.stream()
        .collect(Collectors.toMap(ProductItemSyncStatus::getGdnItemSku, Function.identity(), (first, second) -> second));

      List<String> alreadyCopiedItemSku = availableProductStatus.entrySet().stream()
        .filter(entry -> entry.getValue().getProductSyncStatus().equals(ProductSyncStatus.SUCCESS))
        .map(Map.Entry::getKey)
        .collect(Collectors.toList());

      availableCopiedProductResponses = removeAlreadyCopiedItemSkuAndSetStatus(availableCopiedProductDetails,
        availableProductStatus, alreadyCopiedItemSku);
    }
    return new PageImpl<>(availableCopiedProductResponses, pageRequest, activeProductResponses.getTotalElements());
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void takeDownNeedForCorrectionProduct(String productCode, Map<String, String> itemCodeIdMap,
      ProductDetailResponse productDetailResponse) throws Exception {
    List<ProductBusinessPartner> productBusinessPartners =
        productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(productCode);
    boolean scheduleRemoval = false;
    if (CollectionUtils.isNotEmpty(productBusinessPartners) && isProductActivatedOnce(productBusinessPartners)) {
      for (ProductBusinessPartner productBusinessPartner : productBusinessPartners) {
        ProductL3Response productL3Response = xProductOutbound.getProductDetailsByProductSku(productBusinessPartner.getGdnProductSku()).getValue();
        List<ItemSummaryDetailResponse> itemSummaryDetailResponses = getItemSummaryResponses(
            productBusinessPartner.getStoreId(), productBusinessPartner.getGdnProductSku());
        if (Objects.nonNull(productL3Response) && CollectionUtils.isNotEmpty(itemSummaryDetailResponses)) {
          List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuRequests = new ArrayList<>();
          ProductBusinessPartnerAndItemViewConfigDto
              productBusinessPartnerAndItemViewConfigDto = productBusinessPartnerService.
              updateProductItemBusinessPartnerStateTakeDownTrue(itemSummaryDetailResponses, itemViewConfigAndItemSkuRequests, productBusinessPartner);
          productBusinessPartner = productBusinessPartnerAndItemViewConfigDto.getProductBusinessPartner();
          itemViewConfigAndItemSkuRequests = productBusinessPartnerAndItemViewConfigDto.getItemViewConfigAndItemSkuRequests();
          List<ProductItemWholesalePrice> productItemWholesalePrices =
              updateValuesInProductData(productBusinessPartner, productL3Response, itemSummaryDetailResponses, productDetailResponse);
          if (MapUtils.isNotEmpty(itemCodeIdMap)) {
            Map<String, String> itemSkuIdMap = itemSummaryDetailResponses.stream()
                .collect(Collectors.toMap(ItemSummaryDetailResponse::getItemSku,
                    itemResponse -> itemCodeIdMap.get(itemResponse.getItemCode()), (itemCode1, itemCode2) -> itemCode1));
            for (ProductItemBusinessPartner item : productBusinessPartner.getProductItemBusinessPartners()) {
              if (skipSettingProductItemIdForNullCase && StringUtils.isEmpty(
                  itemSkuIdMap.get(item.getGdnProductItemSku()))) {
                continue;
              }
              item.setProductItemId(itemSkuIdMap.get(item.getGdnProductItemSku()));
            }
          }
          if(schedulesAddEditEnabled){
            scheduleRemoval = productBusinessPartner.isFreeSample() || !productBusinessPartner.isOnline();
          }
          if (!productL3Response.isForceReview()) {
            xProductOutbound.updateItemViewConfigAndForceReview(true,
                itemViewConfigAndItemSkuRequests, false, scheduleRemoval);
          }
          if (productBusinessPartner.isAppealedProduct()) {
            productBusinessPartner.setAppealedProduct(false);
            productAppealService.decrementCounterForProductAppeal(Constants.DEFAULT_STORE_ID,
                productBusinessPartner.getBusinessPartnerId());
          }
          productBusinessPartner.setSizeChartCode(productL3Response.getSizeChartCode());
          productBusinessPartnerRepository.save(productBusinessPartner);
          if (CollectionUtils.isNotEmpty(productItemWholesalePrices)) {
            productItemWholesalePriceService.saveWholesalePriceNew(productItemWholesalePrices);
          }
        }
      }
    }
  }

  private boolean isProductActivatedOnce(List<ProductBusinessPartner> productBusinessPartners) {
    boolean isProductActivated = true;
    if (validateProductActivationDoneOnce) {
      for (ProductBusinessPartner productBusinessPartner : productBusinessPartners) {
        isProductActivated = isProductActivated && Objects.nonNull(
            xProductOutbound.getBasicProductInfoV2(productBusinessPartner.getGdnProductSku()));
      }
    }
    return isProductActivated;
  }

  @Override
  public void addVatUpdateExternalHistory(VatUpdateHistoryDomainEventModel vatUpdateHistoryDomainEventModel)
      throws JsonProcessingException {
    List<ProductItemBusinessPartner> productItemBusinessPartnerList = productItemBusinessPartnerService
        .findProductItemByProductItemId(vatUpdateHistoryDomainEventModel.getStoreId(),
            vatUpdateHistoryDomainEventModel.getProductItemId());
    if (CollectionUtils.isNotEmpty(productItemBusinessPartnerList)) {
      updatedProductHistoryService
          .addAuditLogsForVatUpdate(productItemBusinessPartnerList, vatUpdateHistoryDomainEventModel.getItemName(),
              vatUpdateHistoryDomainEventModel.getOldValue(), vatUpdateHistoryDomainEventModel.getNewValue());
      log.info("Vat external history updated for itemCode = {} ", vatUpdateHistoryDomainEventModel.getItemCode());
    } else {
      log.error("No productItemBusinessPartner found for productItemId = {} ",
          vatUpdateHistoryDomainEventModel.getProductItemId());
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, String
          .format(ErrorMessages.PRODUCT_ITEM_BUSINESS_PARTNER_NOT_FOUND,
              vatUpdateHistoryDomainEventModel.getProductItemId()));
    }
  }

  private void updateProductItemWholesalePrice(Map<String, Map<String, ProductItemWholesalePrice>> itemSkuPickupPointAndProductItemWholesalePriceMap,
      ItemSummaryDetailResponse itemSummaryDetailResponse, List<ProductItemWholesalePrice> productItemWholesalePrices) {
    if (Objects.nonNull(itemSkuPickupPointAndProductItemWholesalePriceMap.getOrDefault(
        itemSummaryDetailResponse.getItemSku(), null))){
      ProductItemWholesalePrice productItemWholesalePrice =
          itemSkuPickupPointAndProductItemWholesalePriceMap.get(itemSummaryDetailResponse.getItemSku())
              .getOrDefault(itemSummaryDetailResponse.getPickupPointCode(), null);
      if (Objects.nonNull(productItemWholesalePrice)) {
        productItemWholesalePrice.setWholesalePriceActivated(
            Objects.nonNull(itemSummaryDetailResponse.getWholesalePriceActivated()) && itemSummaryDetailResponse.getWholesalePriceActivated());
        productItemWholesalePrices.add(productItemWholesalePrice);
      }
    }
  }

  private List<ProductItemWholesalePrice> updateValuesInProductData(ProductBusinessPartner productBusinessPartner,
      ProductL3Response productL3Response, List<ItemSummaryDetailResponse> itemSummaryDetailResponses,
      ProductDetailResponse productDetailResponse)
      throws Exception {
    updatePreOrderValuesInProductData(productBusinessPartner, productL3Response);
    List<ProductItemWholesalePrice> productItemWholesalePrices = new ArrayList<>();
    Map<String, Map<String, ItemSummaryDetailResponse>> itemSkuPickupPointAndItemSummaryDetailResponseMap =
        getItemPickupPointSummaryDetailResponseMap(itemSummaryDetailResponses);
    if(newWholesaleFlowNrEnabled) {
      populateProductItemBusinessPartnerNeedRevision(productBusinessPartner, productItemWholesalePrices, productDetailResponse);
    } else {
    Map<String, Map<String, ProductItemWholesalePrice>> itemSkuPickupPointAndProductItemWholesalePriceMap =
        getProductItemWholesalePriceMap(new ArrayList<>(itemSummaryDetailResponses.stream()
            .map(ItemSummaryDetailResponse::getItemSku).collect(Collectors.toSet())));
    for (ProductItemBusinessPartner productItemBusinessPartner : productBusinessPartner
        .getProductItemBusinessPartners()) {
      if (!productItemBusinessPartner.isMarkForDelete()) {
        ItemSummaryDetailResponse itemSummaryDetailResponse = getItemSummaryDetailResponse(productItemBusinessPartner.getGdnProductItemSku(),
            productItemBusinessPartner.getPickupPointId(), itemSkuPickupPointAndItemSummaryDetailResponseMap);
        if (Objects.nonNull(itemSummaryDetailResponse)) {
          updateProductItemWholesalePrice(itemSkuPickupPointAndProductItemWholesalePriceMap, itemSummaryDetailResponse, productItemWholesalePrices);
        }
      }
     }
    }
    productBusinessPartner.setOnline(productL3Response.isOnline());
    productBusinessPartner.setCncActivated(productL3Response.isCncActivated());
    productBusinessPartner.setFbbActivated(productL3Response.isFbbActivated());
    productBusinessPartner.setState(ProductLevel3WipSummaryCriteria.NEED_CORRECTION.name());
    productBusinessPartner.setB2bActivated(productL3Response.isB2bActivated());
    productBusinessPartner.setB2cActivated(productL3Response.isB2cActivated());
    productBusinessPartner.setBundleProduct(productL3Response.isBundleProduct());
    productBusinessPartner.setMarkForDelete(false);
    return productItemWholesalePrices;
  }

  private void populateProductItemBusinessPartnerNeedRevision(ProductBusinessPartner productBusinessPartner,
      List<ProductItemWholesalePrice> productItemWholesalePrices, ProductDetailResponse productDetailResponse) throws Exception {
    List<ItemInfoDto> itemInfoDtos = productBusinessPartner.getProductItemBusinessPartners().stream()
        .filter(Predicate.not(ProductItemBusinessPartner::isMarkForDelete)).map(
            item -> new ItemInfoDto(item.getGdnProductItemSku(), item.getPickupPointId(),
                item.getGdnProductItemSku() + Constants.HYPHEN + item.getPickupPointId())).toList();
    Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap = new HashMap<>();
    getWholesalePriceSkuResponseMapInBatches(itemInfoDtos, wholesalePriceSkuResponseMap);
    List<ProductItemWholesalePrice> productItemWholesalePriceList =
        productItemWholesalePriceService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID,
            productBusinessPartner.getProductItemBusinessPartners().stream()
                .filter(Predicate.not(ProductItemBusinessPartner::isMarkForDelete))
                .map(ProductItemBusinessPartner::getGdnProductItemSku).collect(Collectors.toList()));
    Map<String, ProductItemWholesalePrice> productItemWholesalePriceMap = productItemWholesalePriceList.stream()
        .collect(Collectors.toMap(productItemWholesalePrice -> productItemWholesalePrice.getItemSku() + Constants.HYPHEN
            + productItemWholesalePrice.getPickupPointCode(), Function.identity()));

    if (MapUtils.isNotEmpty(wholesalePriceSkuResponseMap)) {
      for (Map.Entry<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseEntry : wholesalePriceSkuResponseMap.entrySet()) {
        if (productItemWholesalePriceMap.containsKey(wholesalePriceSkuResponseEntry.getKey())) {
          ProductItemWholesalePrice productItemWholesalePrice =
              productItemWholesalePriceMap.get(wholesalePriceSkuResponseEntry.getKey());
          productItemWholesalePrice.setWholesalePriceActivated(
              Constants.ACTIVE_STATUS.equals(wholesalePriceSkuResponseEntry.getValue().getSkuStatus()));
          productItemWholesalePrice.setWholesaleRules(
              mapperUtil.mapRequestToString(wholesalePriceSkuResponseEntry.getValue().getWholesaleRules()));
        } else {
          ProductItemWholesalePrice productItemWholesalePrice =
              getProductItemWholesalePrice(productBusinessPartner, productDetailResponse,
                  wholesalePriceSkuResponseEntry);
          productItemWholesalePrices.add(productItemWholesalePrice);
        }
      }
      for (ProductItemWholesalePrice productItemWholesalePrice : productItemWholesalePriceList) {
        if (!wholesalePriceSkuResponseMap.containsKey(productItemWholesalePrice.getItemSku() + Constants.HYPHEN
            + productItemWholesalePrice.getPickupPointCode())) {
          productItemWholesalePrice.setMarkForDelete(true);
          productItemWholesalePrices.add(productItemWholesalePrice);
        }
      }
    }
  }

  private ProductItemWholesalePrice getProductItemWholesalePrice(ProductBusinessPartner productBusinessPartner,
      ProductDetailResponse productDetailResponse,
      Map.Entry<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseEntry) throws Exception {
    ProductItemWholesalePrice productItemWholesalePrice = new ProductItemWholesalePrice();
    productItemWholesalePrice.setStoreId(Constants.DEFAULT_STORE_ID);
    productItemWholesalePrice.setItemSku(wholesalePriceSkuResponseEntry.getValue().getItemSku());
    productItemWholesalePrice.setPickupPointCode(wholesalePriceSkuResponseEntry.getValue().getPickUpPointCode());
    productItemWholesalePrice.setWholesaleRules(
        mapperUtil.mapRequestToString(wholesalePriceSkuResponseEntry.getValue().getWholesaleRules()));
    productItemWholesalePrice.setWholesalePriceActivated(
        Constants.ACTIVE_STATUS.equals(wholesalePriceSkuResponseEntry.getValue().getSkuStatus()));
    productItemWholesalePrice.setProductItemId(productBusinessPartner.getProductItemBusinessPartners().stream()
        .filter(item -> item.getGdnProductItemSku().equals(wholesalePriceSkuResponseEntry.getValue().getItemSku())
            && item.getPickupPointId().equals(wholesalePriceSkuResponseEntry.getValue().getPickUpPointCode()))
        .findFirst().map(ProductItemBusinessPartner::getProductItemId).orElse(StringUtils.EMPTY));
    productItemWholesalePrice.setItemCode(Optional.ofNullable(
            Optional.ofNullable(productDetailResponse).orElse(new ProductDetailResponse()).getProductItemResponses())
        .orElse(new HashSet<>()).stream().filter(
            productItemResponse -> productItemWholesalePrice.getProductItemId()
                .equals(productItemResponse.getId())).findFirst().map(
            productItemResponse -> Optional.ofNullable(productItemResponse.getSkuCode())
                .orElse(StringUtils.EMPTY)).orElse(StringUtils.EMPTY));
    return productItemWholesalePrice;
  }

  private void getWholesalePriceSkuResponseMapInBatches(List<ItemInfoDto> itemInfoDtoList,
      Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap) throws Exception {
    List<WholesalePriceSkuResponse> wholesalePriceSkuResponseList;
    if (CollectionUtils.isNotEmpty(itemInfoDtoList)) {
      List<List<ItemInfoDto>> itemInfoDtosPartitions = Lists.partition(itemInfoDtoList, wholesalePriceFetchBatchSize);
      for (List<ItemInfoDto> itemInfoDtosPartition : itemInfoDtosPartitions) {
        wholesalePriceSkuResponseList =
            productPricingOutbound.getWholesalePriceByItemSkuAndPickupPointCode(itemInfoDtosPartition);
        Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMapForPartition =
            wholesalePriceSkuResponseList.stream()
                .collect(Collectors.toMap(WholesalePriceSkuResponse::getItemPickupPointId, Function.identity()));
        wholesalePriceSkuResponseMap.putAll(wholesalePriceSkuResponseMapForPartition);
      }
    }
  }

  private void updatePreOrderValuesInProductData(ProductBusinessPartner productBusinessPartner,
      ProductL3Response productL3Response) {
    if (Objects.nonNull(productL3Response.getPreOrderDTO())) {
      productBusinessPartner.setPreOrder(Boolean.TRUE.equals(productL3Response.getPreOrderDTO().getIsPreOrder()));
      productBusinessPartner.setPreOrderType(productL3Response.getPreOrderDTO().getPreOrderType());
      productBusinessPartner.setPreOrderValue(productL3Response.getPreOrderDTO().getPreOrderValue());
      productBusinessPartner.setPreOrderDate(productL3Response.getPreOrderDTO().getPreOrderDate());
    } else {
      productBusinessPartner.setPreOrder(null);
      productBusinessPartner.setPreOrderType(null);
      productBusinessPartner.setPreOrderValue(null);
      productBusinessPartner.setPreOrderDate(null);
    }
  }

  private Map<String, Map<String, ProductItemWholesalePrice>> getProductItemWholesalePriceMap(List<String> itemSkus) {
    Map<String, Map<String, ProductItemWholesalePrice>> itemSkuPickupPointAndProductItemWholesalePriceMap = new HashMap<>();
    List<ProductItemWholesalePrice> productItemWholesalePrices =
        productItemWholesalePriceService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, itemSkus);
    for (ProductItemWholesalePrice productItemWholesalePrice : productItemWholesalePrices) {
      Map<String, ProductItemWholesalePrice> productItemWholesalePriceMap =
          itemSkuPickupPointAndProductItemWholesalePriceMap.getOrDefault(productItemWholesalePrice.getItemSku(), new HashMap<>());
      productItemWholesalePriceMap.put(productItemWholesalePrice.getPickupPointCode(), productItemWholesalePrice);
      itemSkuPickupPointAndProductItemWholesalePriceMap.put(productItemWholesalePrice.getItemSku(), productItemWholesalePriceMap);
    }
    return itemSkuPickupPointAndProductItemWholesalePriceMap;
  }

  private Map<String, Map<String, ItemSummaryDetailResponse>> getItemPickupPointSummaryDetailResponseMap(
      List<ItemSummaryDetailResponse> itemSummaryDetailResponses) {
    Map<String, Map<String, ItemSummaryDetailResponse>> itemPickupPointSummaryDetailResponseMap = new HashMap<>();
    for (ItemSummaryDetailResponse item : itemSummaryDetailResponses) {
      Map<String, ItemSummaryDetailResponse> itemSummaryDetailResponseMap =
          itemPickupPointSummaryDetailResponseMap.getOrDefault(item.getItemSku(), new HashMap<>());
      itemSummaryDetailResponseMap.put(item.getPickupPointCode(), item);
      itemPickupPointSummaryDetailResponseMap.put(item.getItemSku(), itemSummaryDetailResponseMap);
    }
    return itemPickupPointSummaryDetailResponseMap;
  }

  private ItemSummaryDetailResponse getItemSummaryDetailResponse(String itemSku, String pickupPointCode,
      Map<String, Map<String, ItemSummaryDetailResponse>> itemPickupPointSummaryDetailResponseMap) {
    if (Objects.nonNull(itemPickupPointSummaryDetailResponseMap.getOrDefault(itemSku, null))){
      ItemSummaryDetailResponse itemSummaryDetailResponse =
          itemPickupPointSummaryDetailResponseMap.get(itemSku).getOrDefault(pickupPointCode, null);
      return itemSummaryDetailResponse;
    }
    return null;

  }

  private void updateProductItemDetailInProductData(ProductItemBusinessPartner productItemBusinessPartner,
      ItemSummaryDetailResponse itemResponse) {
    PriceDTO priceDTO = new ArrayList<>(itemResponse.getPrice()).get(0);

    productItemBusinessPartner.setMerchantSku(itemResponse.getMerchantSku());
    productItemBusinessPartner.setPrice(priceDTO.getListPrice());
    productItemBusinessPartner.setSalePrice(priceDTO.getOfferPrice());
    productItemBusinessPartner.setPickupPointId(itemResponse.getPickupPointCode());
  }

  private List<ItemSummaryDetailResponse> getItemSummaryResponses(String storeId, String productSku) throws Exception {
    int page = 0;
    int size = Integer.valueOf(productSystemParameterService
        .findByStoreIdAndVariable(storeId, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE).getValue());
    Page<ItemSummaryDetailResponse> itemSummaryDetailResponses;
    List<ItemSummaryDetailResponse> itemSummaryDetailResponseList = new ArrayList<>();
    do {
      ItemsSummaryDetailRequest itemsSummaryDetailRequest = new ItemsSummaryDetailRequest();
      itemsSummaryDetailRequest.setProductSku(productSku);
      itemsSummaryDetailRequest.setResponseWithoutPickupPoint(true);
      itemSummaryDetailResponses = xProductOutbound
          .findSummaryDetailsByFilter(itemsSummaryDetailRequest, PageRequest.of(page, size));
      itemSummaryDetailResponseList.addAll(itemSummaryDetailResponses.getContent());
      page++;
    } while (page * size < itemSummaryDetailResponses.getTotalElements());
    return itemSummaryDetailResponseList;
  }

  @Override
  public Page<ProductLevel3SummaryMinified> findSummaryMinifiedByFilter(
      ProductLevel3SummaryFilter filterRequest, Pageable pageRequest, SortOrder sort)
      throws Exception {
    return productLevel3DirectAggregatorService.aggregateProductLevel3SummaryMinified(filterRequest, pageRequest, sort);
  }

  @Override
  public void updateResignBusinessPartnerItems(String storeId, String businessPartnerCode) throws Exception {
    boolean resignSellerSwitch =
      Boolean.parseBoolean(this.productSystemParameterService.findByStoreIdAndVariable(storeId,
        SystemParameterConstants.RESIGN_SELLER_SWITCH).getValue());
    if (resignSellerSwitch) {
      log.info("Publishing event to x-bulk for resign seller");
      this.kafkaProducer.send(DomainEventName.RESIGN_SELLER_EVENT,
        ResignSellerDomainEvent.builder().storeId(storeId).businessPartnerCode(businessPartnerCode).build());
    } else {
      this.productLevel3Repository.updateResignBusinessPartnerItems(businessPartnerCode);
    }
  }

  @Override
  public Page<ProductLevel3Item> findItemBySearch(ProductLevel3ItemSearch search, Pageable pageable,
      SortOrder sort) throws Exception {
    this.prepareItemSearch(search);
    ItemSummaryRequest itemFilterRequest =
        this.productLevel3Converter.convertProductLevel3ItemSearchToItemSummaryRequest(search);
    Page<ItemSummaryResponse> itemDatas =
        this.productLevel3Repository.findSummaryByFilter(itemFilterRequest, pageable, sort);
    return itemDatas.map(itemData -> this.productLevel3Converter
        .convertItemSummaryResponseToProductLevel3Item(itemData));
  }

  @Override
  public Boolean checkAvailableStock(String businessPartnerCode) throws Exception {
    Integer checkedStock =
        this.productStockAlertRepository.findAvailableStockByBusinessPartnerCode(businessPartnerCode);
    return !Objects.isNull(checkedStock) && checkedStock > 0;
  }

  private void prepareItemSearch(ProductLevel3ItemSearch search) throws Exception {
    if (search.isIncludeAllTradingProduct()) {
      ProfileResponse bp = this.businessPartnerRepository
          .filterDetailByBusinessPartnerCode(search.getBusinessPartnerCode());
      if (!this.isBlibliTrading(bp)) {
        search.setIncludeAllTradingProduct(false);
      }
    }
  }

  private boolean isBlibliTrading(ProfileResponse bp) {
    if (InventoryFulfillment.BLIBLI.getName().equals(bp.getCompany().getInventoryFulfillment())
        && PurchaseTerms.TRADING.getName().equals(bp.getCompany().getPurchaseTerm())) {
      return true;
    }
    return false;
  }

  private List<AvailableToCopyProductDetailsResponse> removeAlreadyCopiedItemSkuAndSetStatus(
    List<AvailableToCopyProductDetailsResponse> availableToCopyProductList,
    Map<String, ProductItemSyncStatus> alreadyExecutedItemSku, List<String> alreadyCopiedItemSku) {
    List<AvailableToCopyProductDetailsResponse> productsAvailableToBeCopiedList = new ArrayList<>();
    for (AvailableToCopyProductDetailsResponse availableToCopyProduct : availableToCopyProductList) {
      if (CollectionUtils.isNotEmpty(alreadyCopiedItemSku)) {
        availableToCopyProduct.getItemDetails()
          .removeIf(itemDetails -> alreadyCopiedItemSku.contains(itemDetails.getItemSku()));

        availableToCopyProduct.setTotalItemSkuCount(availableToCopyProduct.getItemDetails().size());
      }
      if (CollectionUtils.isNotEmpty(availableToCopyProduct.getItemDetails()) && MapUtils.isNotEmpty(alreadyExecutedItemSku)) {
        boolean isInReviewStatus = availableToCopyProduct.getItemDetails()
          .stream()
          .anyMatch(itemDetails -> alreadyExecutedItemSku.containsKey(itemDetails.getItemSku())
            && ProductSyncStatus.IN_PROGRESS.equals(alreadyExecutedItemSku.get(itemDetails.getItemSku()).getProductSyncStatus()));
        if (isInReviewStatus) {
          availableToCopyProduct.setStatus(ProductSyncStatus.IN_PROGRESS.toString());
        } else {
          availableToCopyProduct.getItemDetails()
            .stream()
            .filter(itemDetails -> alreadyExecutedItemSku.containsKey(itemDetails.getItemSku())
              && ProductSyncStatus.FAIL.equals(alreadyExecutedItemSku.get(itemDetails.getItemSku()).getProductSyncStatus()))
            .findFirst()
            .ifPresent(itemSku -> availableToCopyProduct.setStatus(ProductSyncStatus.FAIL.toString()));
        }
      }
      if (CollectionUtils.isNotEmpty(availableToCopyProduct.getItemDetails())) {
        productsAvailableToBeCopiedList.add(availableToCopyProduct);
      }
    }
    return productsAvailableToBeCopiedList;
  }

  private List<AvailableToCopyProductDetailsResponse> getAllItemSkusByProductSkus(
    Map<String, ActiveProductResponse> activeProductResponseMap, int totalItemSkuCount, String merchantCode,
    String searchKey)
    throws Exception {
    PageRequest pageRequest = PageRequest.of(0, totalItemSkuCount);
    List<String> productSkus = Optional.ofNullable(searchKey)
      .map(String::toUpperCase)
      .map(String::trim)
      .map(Arrays::asList)
      .orElse(new ArrayList<>(activeProductResponseMap.keySet()));

    List<AvailableToCopyProductDetailsResponse> availableToCopyProductDetailsResponsesList = new ArrayList<>();

    ItemSummaryRequest itemSummaryRequest = new ItemSummaryRequest();
    itemSummaryRequest.setArchived(false);
    itemSummaryRequest.setMerchantCode(merchantCode);
    itemSummaryRequest.setProductSkus(productSkus);
    log.info("itemSummaryRequest:{}", itemSummaryRequest);

    Page<ItemSummaryResponse> itemSummaryResponses = productLevel3Repository
      .findSummaryByFilter(itemSummaryRequest, pageRequest, null);
    log.info("itemSummaryResponses {} found for businessPartnerCode {} and productSkus {}", itemSummaryResponses,
      merchantCode, productSkus);

    if (Objects.nonNull(itemSummaryResponses)) {
      Map<String, List<ItemSummaryResponse>> itemSummaryResponseMap = itemSummaryResponses.getContent().stream()
        .collect(Collectors.groupingBy(ItemSummaryResponse::getProductSku));

      itemSummaryResponseMap.entrySet()
        .stream()
        .map(entry -> {
          ActiveProductResponse activeProductResponse = activeProductResponseMap.get(entry.getKey());

          List<AvailableToCopyItemDetailsResponse> itemDetails = entry.getValue().stream()
            .map(item -> AvailableToCopyItemDetailsResponse.builder()
              .itemName(item.getGeneratedItemName())
              .itemSku(item.getItemSku())
              .productDetailPageLink(toProductDetailPage(item.getItemSku()))
              .build())
            .collect(Collectors.toList());

          return AvailableToCopyProductDetailsResponse.builder()
            .productSku(activeProductResponse.getProductSku())
            .productName(activeProductResponse.getProductName())
            .totalItemSkuCount(activeProductResponse.getItemCount())
            .itemDetails(itemDetails)
            .categoryName(entry.getValue().stream()
              .findFirst()
              .map(summaryResponse -> summaryResponse.getMasterCatalog().getCategory().getCategoryCode())
              .map(this::getCategoryNameByCategoryCode)
              .orElse(StringUtils.EMPTY))
            .build();
        })
        .forEach(availableToCopyProductDetailsResponsesList::add);
    }
    return availableToCopyProductDetailsResponsesList;
  }

  private String toProductDetailPage(String itemSKU) {
    return String
      .join(SKU_SEPARATOR, applicationProperties.getProductDetailPageUrlPrefix(), toProductDetailsSKU(itemSKU));
  }

  private String toProductDetailsSKU(String itemSKU) {
    return Optional.ofNullable(itemSKU).map(sku -> sku.substring(0, itemSKU.lastIndexOf(SKU_SEPARATOR)))
      .map(sku -> sku.replace(SKU_SEPARATOR, DOT_SEPARATOR)).map(sku -> String.join(DOT_SEPARATOR, sku, HTML))
      .orElse(StringUtils.EMPTY);
  }

  private String getCategoryNameByCategoryCode(String categoryCode) {
    return Optional.ofNullable(productOutbound.getCategoryDetailByCategoryCode(categoryCode))
      .map(CategoryDetailResponse::getName)
      .orElse(StringUtils.EMPTY);
  }

  private Page<ActiveProductResponse> excludeNullProductCodeProductsAndReviewPendingProducts(
    Page<ActiveProductResponse> activeProductResponses, Pageable page, String businessPartnerCode) {
    List<ActiveProductResponse> activeProductResponseWithoutReviewPendingAndNullProductCodeProducts = new ArrayList<>();
    if (Objects.nonNull(activeProductResponses.getContent())) {
      List<String> productCodes = activeProductResponses.getContent()
        .stream()
        .filter(activeProductResponse -> Objects.nonNull(activeProductResponse.getProductCode()))
        .map(ActiveProductResponse::getProductCode)
        .collect(Collectors.toList());

      List<ProductCollection> productCollectionList = productCollectionRepository
        .findByBusinessPartnerCodeAndProductCodeIn(businessPartnerCode, productCodes);
      log.info("productCollectionList details:{}", productCollectionList);

      Map<String, ProductCollection> productCollectionMap = productCollectionList.stream()
        .collect(Collectors.toMap(ProductCollection::getProductCode, Function.identity(), (first, second) -> second));

      activeProductResponses.getContent()
        .stream()
        .filter(activeProductResponse -> Objects.nonNull(activeProductResponse.getProductCode()))
        .filter(activeProductResponse -> !isReviewPending(activeProductResponse, productCollectionMap))
        .forEach(activeProductResponseWithoutReviewPendingAndNullProductCodeProducts::add);
    }
    return new PageImpl<ActiveProductResponse>(activeProductResponseWithoutReviewPendingAndNullProductCodeProducts, page,
      activeProductResponses.getTotalElements());
  }

  private boolean isReviewPending(ActiveProductResponse activeProductResponse,
    Map<String, ProductCollection> productCollectionMap) {
    if (productCollectionMap.containsKey(activeProductResponse.getProductCode())) {
      ProductCollection productCollection = productCollectionMap.get(activeProductResponse.getProductCode());
      return productCollection.isReviewPending();
    }
    return false;
  }

  @Override
  public void setProductNameInHistoryIfEmpty(List<AuditTrailDto> auditTrailRequests) throws ApplicationException {
    List<AuditTrailDto> auditTrailDtosEmptyProductName =
        Optional.ofNullable(auditTrailRequests).orElse(new ArrayList<>()).stream()
            .filter(auditTrailDto -> StringUtils.isBlank(auditTrailDto.getName())).collect(Collectors.toList());
    if (CollectionUtils.isNotEmpty(auditTrailDtosEmptyProductName)) {
      if (setProductNameInAudit) {
        List<String> productSkus = auditTrailDtosEmptyProductName.stream().map(AuditTrailDto::getProductSku).distinct()
            .collect(Collectors.toList());
        List<ProductBusinessPartner> productBusinessPartnerList =
            productBusinessPartnerRepository.findByStoreIdAndGdnProductSkuIn(Constants.DEFAULT_STORE_ID, productSkus);
        Map<String, String> productSkuAndProductNameMap =
            Optional.ofNullable(productBusinessPartnerList).orElse(new ArrayList<>()).stream().collect(
                Collectors.toMap(ProductBusinessPartner::getGdnProductSku, ProductBusinessPartner::getProductName));
        auditTrailDtosEmptyProductName.forEach(
            auditTrailDto -> auditTrailDto.setName(productSkuAndProductNameMap.get(auditTrailDto.getProductSku())));
      } else {
        List<String> itemSkus = auditTrailDtosEmptyProductName.stream().map(AuditTrailDto::getGdnSku).distinct()
            .collect(Collectors.toList());
        GdnRestSingleResponse<SimpleMapStringResponse> response =
            xProductOutbound.getItemNameByItemSkus(new SimpleListStringRequest(itemSkus), true);
        auditTrailDtosEmptyProductName.forEach(
            auditTrailDto -> auditTrailDto.setName(response.getValue().getValue().get(auditTrailDto.getGdnSku())));
      }
    }
  }

}
