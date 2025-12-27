package com.gdn.partners.pbp.service.offlineitem;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.x.product.domain.event.config.ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.response.AuditTrailListRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.commons.constant.UpdateProductActivity;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.CategoryRepository;
import com.gdn.mta.product.repository.OfflineItemRepository;
import com.gdn.mta.product.repository.PickupPointRepository;
import com.gdn.mta.product.service.ProductLevel3Service;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.ProductSystemParameterService;
import com.gdn.mta.product.service.UpdatedProductHistoryService;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.partners.pbp.converter.OfflineItemConverter;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemDetailResponse;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupBulkDownloadResponse;
import com.gdn.partners.pbp.dto.offlineitem.OfflineProductResponse;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemDetailResponse;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemFailedResponse;
import com.gdn.partners.pbp.model.offlineitem.DeleteOfflineItem;
import com.gdn.partners.pbp.model.offlineitem.OfflineItemDetail;
import com.gdn.partners.pbp.model.offlineitem.UpsertOfflineItem;
import com.gdn.partners.pbp.model.offlineitem.ValidOfflineItem;
import com.gdn.partners.pbp.outbound.inventory.InventoryOutbound;
import com.gdn.partners.pbp.outbound.pickuppoint.PickupPointOutbound;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.OfflineItemPriceDTO;
import com.gdn.x.product.rest.web.model.request.DeleteOfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.UpdateOfflineItemPriceRequest;
import com.gdn.x.product.rest.web.model.response.DeleteOfflineItemResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponseDetail;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class OfflineItemServiceBean implements OfflineItemService {

  private static final Logger LOGGER = LoggerFactory.getLogger(OfflineItemServiceBean.class);
  private static final String COMMA_DELIMITER = ",";
  private static final String STRIP_DELIMITER = "-";

  @Value("${pickup.point.code.fetch.size:1000}")
  public Integer pickupPointCodeFetchSize;

  @Value("${product.history.update.event}")
  private boolean productHistoryUpdateThroughEvent;

  @Autowired
  private ProductOutbound productOutbound;

  @Autowired
  private PickupPointOutbound pickupPointOutbound;

  @Autowired
  private InventoryOutbound inventoryOutbound;

  @Autowired
  private OfflineItemConverter offlineItemConverter;

  @Autowired
  private OfflineItemHelperService offlineItemHelperService;

  @Autowired
  protected CategoryRepository categoryRepository;

  @Autowired
  protected PickupPointRepository pickupPointRepository;

  @Autowired
  private OfflineItemRepository offlineItemRepository;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private XProductOutbound xProductOutbound;

  @Autowired
  private ProductSystemParameterService productSystemParameterService;

  @Autowired
  private ProductLevel3Service productLevel3Service;

  @Autowired
  private ProductService productService;

  @Autowired
  private UpdatedProductHistoryService updatedProductHistoryService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private ObjectMapper objectMapper;

  public static final String BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK =
      "Business partner code must not be blank.";
  public static final String GDN_SKU_MUST_NOT_BE_BLANK = "Business partner code must not be blank.";
  private static final String MERCHANT_CODE_MUST_NOT_BE_BLANK = "Merchant code must not be blank";
  private static final String DELETE_OFFLINE_ITEM_LIST_MUST_NOT_BE_EMPTY =
      "Delete offline item list must not be empty";
  private static final String ITEM_SKU_MUST_NOT_BE_BLANK = "Item SKU must not be blank.";
  private static final String MERCHANT_SKUS_MUST_NOT_BE_EMPTY = "Merchant SKUs must not be empty";
  private static final String PICKUP_POINT_CODE_MUST_NOT_BE_BLANK =
      "Pickup point code must not be blank.";
  public static final String MERCHANT_CODE_MUST_NOT_BE_EMPTY = "Merchant code must not be empty";
  public static final String ITEM_SKU_MUST_NOT_BE_EMPTY = "Item SKU must not be empty";
  public static final String SAFETY_STOCK_MUST_NOT_BE_LESS_THAN_ZERO = "Safety stock must not be less than zero";
  public static final String OFFER_PRICE_MUST_BE_FILLED = "Offer price must be filled";
  public static final String AVAILABLE_STOCK = "availableStock";
  public static final String ORIGINAL_STOCK = "originalStock";
  public static final String MPP_NOT_ALLOWED_FOR_SELLER = "Multi pickup point not allowed for this seller.";
  private static final String ITEM_SKU_DOES_NOT_BELONG_TO_THE_MERCHANT =
      "Item sku does not belong to the merchant";

  @Override
  public List<OfflineItemResponseDetail> findOfflineItemProductByMerchantSkus(String businessPartnerCode,
      List<String> merchantSkus) throws Exception {
    checkArgument(StringUtils.isNotBlank(businessPartnerCode),
        BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(merchantSkus), MERCHANT_SKUS_MUST_NOT_BE_EMPTY);

    OfflineItemResponse offlineItemResponse = offlineItemRepository
        .findOfflineItemByBusinessPartnerCodeAndMerchantSkus(businessPartnerCode, merchantSkus);

    return offlineItemResponse.getOfflineProducts();
  }

  @Override
  public Pair<List<UpsertOfflineItemDetailResponse>, List<UpsertOfflineItemFailedResponse>> upsertOfflineItems(
      String requestId, String username, String merchantCode, List<UpsertOfflineItem> upsertOfflineItems,
      boolean updateStock) throws Exception {
      return upsertL5Items(requestId, username, merchantCode, upsertOfflineItems, updateStock);
  }

  @Override
  public Pair<List<UpsertOfflineItemDetailResponse>, List<UpsertOfflineItemFailedResponse>> upsertL5Items(
      String requestId, String username, String merchantCode, List<UpsertOfflineItem> upsertOfflineItems,
      boolean updateStock) throws Exception {
    ProfileResponse profileResponse = businessPartnerRepository.filterDetailByBusinessPartnerCode(merchantCode);
    if (!productService.checkIfMPPIsAllowed(profileResponse)) {
      LOGGER.error("Merchant not eligible for MPP upsert : {}, merchantCode");
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, MPP_NOT_ALLOWED_FOR_SELLER);
    }

    List<UpsertOfflineItemFailedResponse> failedUpsertOfflineItemResponses = new ArrayList<>();
    for (UpsertOfflineItem upsertOfflineItem : upsertOfflineItems) {
      UpsertOfflineItemFailedResponse upsertOfflineItemFailedResponse =
          validateUpsertOfflineRequest(upsertOfflineItem, merchantCode);
      if (Objects.nonNull(upsertOfflineItemFailedResponse)) {
        failedUpsertOfflineItemResponses.add(upsertOfflineItemFailedResponse);
      }
    }

    if (failedUpsertOfflineItemResponses.size() == upsertOfflineItems.size()) {
      return Pair.of(new ArrayList<>(), failedUpsertOfflineItemResponses);
    }

    List<UpsertOfflineItem> successL5Items =
        offlineItemHelperService.upsertL5ItemsInXProduct(requestId, username, merchantCode, upsertOfflineItems,
            failedUpsertOfflineItemResponses);

    if (CollectionUtils.isEmpty(successL5Items)) {
      return Pair.of(new ArrayList<>(), failedUpsertOfflineItemResponses);
    }

    if (updateStock) {
      successL5Items = productLevel3Service
          .upsertL5StockInInventory(merchantCode, successL5Items, failedUpsertOfflineItemResponses, profileResponse);
    }

    List<UpsertOfflineItemDetailResponse> successUpsertOfflineItemResponses = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(successL5Items)) {
      for (UpsertOfflineItem upsertOfflineItem : successL5Items) {
        UpsertOfflineItemDetailResponse successUpsertOfflineItemResponse = new UpsertOfflineItemDetailResponse();
        BeanUtils.copyProperties(upsertOfflineItem, successUpsertOfflineItemResponse);
        successUpsertOfflineItemResponses.add(successUpsertOfflineItemResponse);
      }
    }
    return Pair.of(successUpsertOfflineItemResponses, failedUpsertOfflineItemResponses);
  }


  private UpsertOfflineItemFailedResponse validateUpsertOfflineRequest(UpsertOfflineItem upsertOfflineItem,
      String merchantCode) {
    StringBuilder errorMessage = new StringBuilder();
    if (StringUtils.isBlank(upsertOfflineItem.getItemSku())) {
      errorMessage.append(ITEM_SKU_MUST_NOT_BE_EMPTY);
    }
    if (StringUtils.isBlank(upsertOfflineItem.getPickupPointCode())) {
      errorMessage.append(PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    }
    if (StringUtils.isNotBlank(upsertOfflineItem.getItemSku()) && !upsertOfflineItem.getItemSku()
        .startsWith(merchantCode)) {
      errorMessage.append(ITEM_SKU_DOES_NOT_BELONG_TO_THE_MERCHANT);
    }
    if (StringUtils.isNotBlank(errorMessage)) {
      LOGGER.error("Validation failed for upsertOfflineItem {} for merchant : {}, error : {} ", upsertOfflineItem,
          merchantCode, errorMessage);
      return new UpsertOfflineItemFailedResponse(upsertOfflineItem.getItemSku(), upsertOfflineItem.getPickupPointCode(),
          errorMessage.toString());
    }
    return null;
  }

  private Double getListPrice(OfflineItemPriceDTO offlineItemPriceDTO) {
    double listPrice = offlineItemPriceDTO.getListPrice();
    double offerPrice = offlineItemPriceDTO.getOfferPrice();
    if (listPrice < offerPrice) {
      listPrice = offerPrice;
    }
    return listPrice;
  }

  private Map<String, OfflineItemPriceResponse> getPickupPointPriceMap(
    List<OfflineItemPriceResponse> offlineItemPriceResponses) {
    return offlineItemPriceResponses.stream().collect(Collectors
      .toMap(OfflineItemPriceResponse::getPickupPointCode, Function.identity(),
        (price1, price2) -> price1));
  }

  private Map<String, PickupPointResponse> getPickupPointDetailMap(
    List<OfflineItemPriceResponse> offlineItemPriceResponses) throws Exception {
    List<String> distinctPickupPointCodes = offlineItemPriceResponses.stream()
        .filter(content -> StringUtils.isNotBlank(content.getPickupPointCode()))
        .map(OfflineItemPriceResponse::getPickupPointCode).distinct()
        .collect(Collectors.toList());
    List<List<String>> subListsOfPickupPointCodes =
        ListUtils.partition(distinctPickupPointCodes, pickupPointCodeFetchSize);

    List<PickupPointResponse> pickupPointResponses = new ArrayList<>();
    for (List<String> pickupPointCodes : subListsOfPickupPointCodes) {
      pickupPointResponses.addAll(pickupPointOutbound.getByPickupPointCodes(
          GdnMandatoryRequestParameterUtil.getRequestId(), pickupPointCodes));
    }

    return pickupPointResponses.stream()
        .collect(Collectors.toMap(PickupPointResponse::getCode, Function.identity()));
  }

  private List<OfflineItemInstantPickupBulkDownloadResponse> constructResponsesForBulkDownloadPerItemSku(
      ItemSummaryResponse item, List<OfflineItemPriceResponse> offlineItemPriceResponses,
      Map<String, OfflineItemPriceResponse> pricePerPickupPointCodeMap,
      Map<String, Integer> availableStockPerPickupPointCodeMap,
      Map<String, PickupPointResponse> detailPerPickupPointCodeMap) {
    List<OfflineItemInstantPickupBulkDownloadResponse> responses = new ArrayList<>();
    offlineItemPriceResponses.forEach(offlineItemPriceResponse -> {
      String pickupPointCodePerItemSku = offlineItemPriceResponse.getPickupPointCode();
      Double offerPrice = pricePerPickupPointCodeMap.get(pickupPointCodePerItemSku).getOfferPrice();
      Double listPrice = Optional
          .ofNullable(pricePerPickupPointCodeMap.get(pickupPointCodePerItemSku).getListPrice())
          .orElse(offerPrice);
      Integer stock = availableStockPerPickupPointCodeMap.get(pickupPointCodePerItemSku);
      PickupPointResponse pickupPoint = detailPerPickupPointCodeMap.get(pickupPointCodePerItemSku);

      if (Objects.isNull(listPrice) || Objects.isNull(offerPrice) || Objects.isNull(stock)
          || Objects.isNull(pickupPoint)) {
        return;
      }

      OfflineItemInstantPickupBulkDownloadResponse response =
          OfflineItemInstantPickupBulkDownloadResponse.builder().itemSku(item.getItemSku())
              .itemName(item.getGeneratedItemName()).pickupPointCode(pickupPointCodePerItemSku)
              .pickupPointName(pickupPoint.getName()).listPrice(listPrice).price(offerPrice)
              .offlineAvailableStock(stock).build();
      responses.add(response);
    });
    return responses;
  }

  protected Map<String, List<CategoryResponse>> generateItemCategoriesData(
      Collection<ItemSummaryResponse> itemSummaryResponses) throws Exception {
    Map<String, List<CategoryResponse>> categories = new HashMap<>();
    for (ItemSummaryResponse item : itemSummaryResponses) {
      String categoryCode = "";
      MasterCatalogDTO catalogDTO = item.getMasterCatalog();
      if (catalogDTO != null && catalogDTO.getCategory() != null) {
        categoryCode = catalogDTO.getCategory().getCategoryCode();
      }
      if (categories.get(categoryCode) == null) {
        try {
          List<CategoryResponse> categoryResponses =
              categoryRepository.findHierarchyByCategoryCode(categoryCode);
          categories.put(categoryCode, categoryResponses);
        } catch (Exception e) {
          LOGGER.error("Error while retrieving item categories. ItemSku: {}, CategoryCode: {} ",
              item.getItemSku(), categoryCode);
        }
      }
    }
    return categories;
  }

  protected Map<String, PickupPointResponse> generateItemPickupPointsData(
      Collection<ItemSummaryResponse> itemSummaryResponses) throws Exception {
    Map<String, PickupPointResponse> pickupPoints = new HashMap<>();
    if (CollectionUtils.isNotEmpty(itemSummaryResponses)) {
      for (ItemSummaryResponse item : itemSummaryResponses) {
        String pickupPointCode = item.getPickupPointCode();
        if (pickupPoints.get(pickupPointCode) == null) {
          PickupPointResponse pickupPointData = pickupPointRepository.findByPickupPointCode(pickupPointCode);
          pickupPoints.put(pickupPointCode, pickupPointData);
        }
      }
    }
    return pickupPoints;
  }

  @Override
  public Pair<List<DeleteOfflineItemDetailResponse>, List<DeleteOfflineItemDetailResponse>>
  bulkDeleteOfflineItem(String requestId, String username, String merchantCode, List<DeleteOfflineItem> deleteOfflineItems)
      throws Exception {
    validateDeleteRequest(merchantCode, deleteOfflineItems);
    List<UpdatedProductHistory> updatedProductHistoryList = new ArrayList<>();
    List<DeleteOfflineItemRequest> deleteOfflineItemRequests =
        offlineItemConverter.convertToDeleteOfflineItemRequest(deleteOfflineItems);
    List<String> itemSkus = deleteOfflineItemRequests.stream().map(DeleteOfflineItemRequest::getItemSku).collect(Collectors.toList());
    List<DeleteOfflineItemResponse> deleteOfflineItemResponses =
        xProductOutbound.bulkDeleteOfflineItem(merchantCode, deleteOfflineItemRequests);
    xProductOutbound.updateCncActivationFlag(itemSkus);
    List<DeleteOfflineItemDetailResponse> successProducts = new ArrayList<>();
    List<DeleteOfflineItemDetailResponse> failedProducts = new ArrayList<>();
    deleteOfflineItemResponses.forEach(deleteResponse -> {
      if (deleteResponse.isSuccess()) {
        successProducts.add(constructDeleteDetail(deleteResponse));
          updatedProductHistoryList.add(updatedProductHistoryService.addToUpdatedProductHistory(deleteResponse.getItemSku(),
              UpdateProductActivity.PICKUP_POINT_DELETED.name(),  merchantCode, requestId, username,
              deleteResponse.getPickupPointCode(),  StringUtils.EMPTY, deleteResponse.getProductSku(),
              deleteResponse.getItemName(), deleteResponse.getPickupPointCode(), false));
      } else {
        failedProducts.add(constructDeleteDetail(deleteResponse));
          updateHistoryInCaseOfFlagChanges(requestId, username, merchantCode, deleteResponse, updatedProductHistoryList);
      }
    });
    if (productHistoryUpdateThroughEvent) {
      updatedProductHistoryList.forEach(updatedProductHistory -> {
        AuditTrailListRequest auditTrailListRequest = CommonUtils
            .getAuditTrailRequestForL3History(updatedProductHistory.getBusinessPartnerCode(),
                updatedProductHistory.getProductSku(), updatedProductHistory.getGdnName(),
                updatedProductHistory.getActivity(), StringUtils.EMPTY, updatedProductHistory.getOldValues(),
                updatedProductHistory.getNewValues(), updatedProductHistory.getChangedBy(),
                updatedProductHistory.getRequestId(), updatedProductHistory.getClientHost(),
                updatedProductHistory.getGdnSku(), updatedProductHistory.getPickupPointCode());
        log.info("Publishing the event {} for updating history of productSku {} ", PRODUCT_SKU_UPDATE_HISTORY,
            updatedProductHistory.getProductSku());
        kafkaProducer.send(PRODUCT_SKU_UPDATE_HISTORY, updatedProductHistory.getProductSku(), auditTrailListRequest);
      });
    } else {
      updatedProductHistoryService.createAudit(updatedProductHistoryList, false);
    }
      return Pair.of(successProducts, failedProducts);
  }

  private void updateHistoryInCaseOfFlagChanges(String requestId, String username, String merchantCode,
      DeleteOfflineItemResponse deleteResponse, List<UpdatedProductHistory> updatedProductHistoryList) {
    if (deleteResponse.isCncUpdated()) {
      updatedProductHistoryList.add(updatedProductHistoryService.addToUpdatedProductHistory(deleteResponse.getItemSku(),
          UpdateProductActivity.CNC.getDesc(),  merchantCode, requestId, username,
          Boolean.TRUE.toString(), Boolean.FALSE.toString() , deleteResponse.getProductSku(),
          deleteResponse.getItemName(), deleteResponse.getPickupPointCode(), false));
    }
    if (deleteResponse.isBuyableUpdated()) {
      updatedProductHistoryList.add(updatedProductHistoryService.addToUpdatedProductHistory(deleteResponse.getItemSku(),
          UpdateProductActivity.BUYABLE.getDesc(),  merchantCode, requestId, username,
          Boolean.TRUE.toString(), Boolean.FALSE.toString() , deleteResponse.getProductSku(),
          deleteResponse.getItemName(), deleteResponse.getPickupPointCode(), false));
    }
    if (deleteResponse.isDiscoverableUpdated()) {
      updatedProductHistoryList.add(updatedProductHistoryService.addToUpdatedProductHistory(deleteResponse.getItemSku(),
          UpdateProductActivity.DISPLAYABLE.getDesc(),  merchantCode, requestId, username,
          Boolean.TRUE.toString(), Boolean.FALSE.toString() , deleteResponse.getProductSku(),
          deleteResponse.getItemName(), deleteResponse.getPickupPointCode(), false));
    }
  }

  private void validateDeleteRequest(String merchantCode,
      List<DeleteOfflineItem> deleteOfflineItems) {
    checkArgument(StringUtils.isNotBlank(merchantCode), MERCHANT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(!CollectionUtils.isEmpty(deleteOfflineItems),
        DELETE_OFFLINE_ITEM_LIST_MUST_NOT_BE_EMPTY);

    for (DeleteOfflineItem deleteOfflineItem : deleteOfflineItems) {
      checkArgument(StringUtils.isNotBlank(deleteOfflineItem.getItemSku()),
          ITEM_SKU_MUST_NOT_BE_BLANK);
      checkArgument(StringUtils.isNotBlank(deleteOfflineItem.getPickupPointCode()),
          PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    }
  }

  private DeleteOfflineItemDetailResponse constructDeleteDetail(DeleteOfflineItemResponse response) {
    return DeleteOfflineItemDetailResponse.builder()
        .itemSku(response.getItemSku())
        .pickupPointCode(response.getPickupPointCode())
        .errorMessage(response.getErrorMessage())
        .build();
  }


  @Override
  public boolean updateOfflineItemPriceByItemSku(String merchantCode, String itemSku,
      Double listPrice, Double offerPrice) throws Exception {
    checkArgument(StringUtils.isNotBlank(merchantCode), MERCHANT_CODE_MUST_NOT_BE_EMPTY);
    checkArgument(StringUtils.isNotBlank(itemSku), ITEM_SKU_MUST_NOT_BE_EMPTY);
    checkArgument(Objects.nonNull(offerPrice), OFFER_PRICE_MUST_BE_FILLED);

    UpdateOfflineItemPriceRequest request = new UpdateOfflineItemPriceRequest();
    request.setItemSku(itemSku);
    request.setListPrice(Optional.ofNullable(listPrice).orElse(offerPrice));
    request.setOfferPrice(offerPrice);
    return this.offlineItemRepository.updateOfflineItemPriceByItemSku(merchantCode, request)
        .isSuccess();
  }
}
