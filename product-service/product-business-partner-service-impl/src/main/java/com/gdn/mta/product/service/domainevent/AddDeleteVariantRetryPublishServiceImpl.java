package com.gdn.mta.product.service.domainevent;

import static com.gdn.mta.product.util.CommonUtils.getItemIdItemSkuMap;
import static com.gdn.mta.product.util.CommonUtils.getItemSkuItemCodeMap;

import java.util.Date;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import com.gda.mta.product.dto.ItemPickupPointDto;
import com.gdn.mta.product.service.config.PreOrderConfig;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.product.model.entity.PreOrder;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.mta.product.service.ScheduledJobService;
import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gdn.mta.domain.event.modal.AddDeleteVariantRetryPublishEventModel;
import com.gdn.mta.domain.event.modal.AddEditedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.AddProductToVendorCombinedEventModel;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.mta.product.enums.AddDeleteVariantStatus;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductRepositoryBean;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.gdn.mta.product.service.domainevent.publisher.AddDeleteVariantRetryPublishService;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.pbp.commons.constants.EditedReviewTypeConstants;
import com.gdn.partners.pbp.helper.ProductItemCreationRequestHelper;
import com.gdn.partners.pbp.helper.RequestHelper;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.pbp.outbound.pickuppoint.PickupPointOutbound;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3InventoryService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3LogisticsService;
import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.request.AddDeleteVariantRetryRequest;
import com.gdn.x.product.rest.web.model.request.ItemActivationRequest;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointCodeResponse;
import com.gdn.x.productcategorybase.dto.request.SimpleStringListRequest;
import com.gdn.x.productcategorybase.dto.response.SimpleStringMapResponse;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class AddDeleteVariantRetryPublishServiceImpl implements AddDeleteVariantRetryPublishService {

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private ProductService productService;

  @Autowired
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Autowired
  private ProductRepositoryBean productRepositoryBean;

  @Autowired
  private PCBFeign pcbFeign;

  @Autowired
  private XProductOutbound xProductOutbound;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private ScheduledJobService scheduledJobService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private PickupPointOutbound pickupPointOutbound;

  @Autowired
  private ProductLevel3InventoryService productLevel3InventoryService;

  @Autowired
  private ProductLevel3LogisticsService productLevel3LogisticsService;

  @Autowired
  private PreOrderConfig preOrderConfig;

  @Value("${mpp.for.wh.enabled}")
  private boolean mppForWhEnabled;

  @Value("${inventory.insert.batch.size.edit}")
  private int inventoryInsertBatchSizeForEdit;

  @Value("${pickup.point.fetch.batch.size.reconciliation}")
  private int pickupPointFetchBatchSizeForReconcilation;

  @Value("${price.info.max.variant.limit}")
  private int priceInfoMaxVariantLimit;

  @Value("${price.info.vendor.edited.enabled}")
  private boolean priceInfoVendorEditedEnabled;

  @Value("${faas.feature.switch}")
  private boolean faasFeatureSwitch;


  @Override
  public void processAddDeleteVariantRetryEvents(
      AddDeleteVariantRetryPublishEventModel addDeleteVariantRetryPublishEventModel) {
    String productCode = addDeleteVariantRetryPublishEventModel.getProductCode();
    try {
      if(StringUtils.isNotEmpty(productCode)){
        ProductCollection productCollection =
            productService.updateAddDeleteVariantStatusForListener(productCode, AddDeleteVariantStatus.IN_PROGRESS);
        Optional<ProductBusinessPartner> productBusinessPartner =
            Optional.ofNullable(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(productCode))
                .orElse(new ArrayList<>()).stream().findFirst();
        if (productBusinessPartner.isPresent() && Objects.nonNull(productCollection)) {
          String productSku = productBusinessPartner.get().getGdnProductSku();
          String businessPartnerCode = productBusinessPartner.get().getBusinessPartnerId();
          List<ProductItemBusinessPartner> productItemBusinessPartners =
              Optional.ofNullable(productBusinessPartner.get().getProductItemBusinessPartners())
                  .orElse(new ArrayList<>());
          Map<String, Boolean> itemSkuMarkForDeleteMap =
              CommonUtils.getItemSkuMarkForDeleteMapping(productItemBusinessPartners);
          Map<String, String> itemIdItemSkuMap = getItemIdItemSkuMap(productItemBusinessPartners);
          SimpleStringMapResponse itemIdItemCodeMap = getItemIdItemCodeMap(itemIdItemSkuMap);
          Map<String, String> itemSkuItemCodeMap = getItemSkuItemCodeMap(itemIdItemSkuMap, itemIdItemCodeMap);
          List<ItemActivationRequest> itemActivationRequestList =
              ProductItemCreationRequestHelper.getListOfItemActivationRequest(productBusinessPartner.get(),
                  new HashMap<>(), new HashMap<>(), false);
          com.gdn.x.product.rest.web.model.request.AddDeleteVariantRetryRequest addDeleteVariantRetryRequest =
              setAddDeleteVariantRetryRequest(productCode, productSku, itemSkuMarkForDeleteMap, itemSkuItemCodeMap,
                  itemActivationRequestList);
          log.info("Reconciliation of variants for product sku : {} and request : {}", productSku,
              addDeleteVariantRetryRequest);
          List<ItemPickupPointCodeResponse> itemPickupPointCodeResponseList =
              xProductOutbound.reconcileProductVariants(addDeleteVariantRetryRequest, productSku);
          Map<String, List<String>> itemSkuAndPickupPointMap =
              CommonUtils.getItemSkuAndPickupPointMap(itemPickupPointCodeResponseList);
          ProfileResponse profileResponse =
              this.businessPartnerRepository.filterDetailByBusinessPartnerCode(businessPartnerCode);
          updateInventoryAndLogistics(productSku, businessPartnerCode, itemSkuItemCodeMap,
              itemSkuAndPickupPointMap, profileResponse, itemPickupPointCodeResponseList);
          vendorCombinedEventToPDT(productCollection, productCode, productBusinessPartner, profileResponse);
          productService.updateAddDeleteVariantStatusForListener(productCode, AddDeleteVariantStatus.SUCCESS);
        }
      }
    } catch (Exception ex) {
      log.error("Exception while reconcile product variants error :", ex);
      productService.updateAddDeleteVariantStatusForListener(productCode,AddDeleteVariantStatus.FAILED);
    }
  }


  private void updateInventoryAndLogistics(String productSku, String businessPartnerCode,
      Map<String, String> itemSkuItemCodeMap, Map<String, List<String>> itemSkuAndPickupPointMap,
      ProfileResponse profileResponse,
      List<ItemPickupPointCodeResponse> itemPickupPointCodeResponseList) throws Exception {
    if (MapUtils.isNotEmpty(itemSkuAndPickupPointMap)) {
      List<String> pickupPointList =
          itemSkuAndPickupPointMap.values().stream().flatMap(List::stream).distinct().collect(Collectors.toList());
      List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
      List<List<String>> pickupPoints = Lists.partition(pickupPointList, pickupPointFetchBatchSizeForReconcilation);
      for (List<String> batchOfPickupPoints : pickupPoints) {
        List<PickupPointResponse> batchResponse =
            pickupPointOutbound.getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getRequestId(),
                batchOfPickupPoints);
        pickupPointResponseList.addAll(batchResponse);
      }
      updateInventory(productSku, itemSkuItemCodeMap, profileResponse, itemSkuAndPickupPointMap,
          pickupPointResponseList, itemPickupPointCodeResponseList);
      updateLogistics(businessPartnerCode, profileResponse, itemSkuAndPickupPointMap);
    }
  }


  private com.gdn.x.product.rest.web.model.request.AddDeleteVariantRetryRequest setAddDeleteVariantRetryRequest(
      String productCode, String productSku, Map<String, Boolean> itemSkuMarkForDeleteMap,
      Map<String, String> itemSkuItemCodeMap, List<ItemActivationRequest> itemActivationRequestList) {
    com.gdn.x.product.rest.web.model.request.AddDeleteVariantRetryRequest addDeleteVariantRetryRequest =
        new AddDeleteVariantRetryRequest();
    addDeleteVariantRetryRequest.setProductCode(productCode);
    addDeleteVariantRetryRequest.setProductSku(productSku);
    List<ItemActivationRequest> ItemsForAddDeleteRetryList =
        RequestHelper.getItemsForAddDeleteRetryList(itemSkuMarkForDeleteMap, itemSkuItemCodeMap,
            itemActivationRequestList);
    addDeleteVariantRetryRequest.setItemActivationRequestList(ItemsForAddDeleteRetryList);
    return addDeleteVariantRetryRequest;
  }

  private void vendorCombinedEventToPDT(ProductCollection productCollection, String productCode,
      Optional<ProductBusinessPartner> productBusinessPartner, ProfileResponse profileResponse) {
    boolean productExistsInPDT = productService.checkIfProductExistsInPDT(productCode, false);
    if (productExistsInPDT) {
      AddEditedProductToPDTEvent addEditedProductToPDTEvent =
          ConverterUtil.toAddEditedProductToPDTEvent(GdnMandatoryRequestParameterUtil.getStoreId(),
              EditedReviewTypeConstants.CONTENT_EDIT, productCollection, null, profileResponse,
              productBusinessPartner.get(), priceInfoVendorEditedEnabled, priceInfoMaxVariantLimit);
      SellerDetailResponse sellerDetailResponse =
          scheduledJobService.fetchSellerDetailResponse(productCollection.getBusinessPartnerCode());
      if (Objects.nonNull(sellerDetailResponse)) {
        addEditedProductToPDTEvent.setSellerBadge(sellerDetailResponse.getSellerBadge());
      }
      log.info("Vendor combined event published for product code :{} and message : {}", productCode,
          addEditedProductToPDTEvent);
      kafkaProducer.send(kafkaTopicProperties.getVendorCombinedEventNoPriority(), productCollection.getProductCode(),
          AddProductToVendorCombinedEventModel.builder().addEditedProductToPDTEvent(addEditedProductToPDTEvent)
              .build());
    }
  }

  private void updateLogistics(String businessPartnerCode, ProfileResponse profileResponse,
      Map<String, List<String>> itemSkuAndPickupPointMap) {
    List<String> itemSkuList = new ArrayList<>(itemSkuAndPickupPointMap.keySet());
    List<ProductLevel3Logistics> productLevel3LogisticsList = new ArrayList<>();
    for (String itemSku : itemSkuList) {
      productLevel3LogisticsList = productLevel3LogisticsService.findLogisticsByItemSku(itemSku, businessPartnerCode,
          profileResponse.getCompany().getMerchantDeliveryType());
    }
    List<ProductLevel3Logistics> logistics = new ArrayList<>();
    for (ProductLevel3Logistics productLevel3Logistics : productLevel3LogisticsList) {
      ProductLevel3Logistics productLevel3LogisticsRequest = new ProductLevel3Logistics();
      productLevel3LogisticsRequest.setSelected(productLevel3Logistics.isSelected());
      productLevel3LogisticsRequest.setLogisticProductCode(productLevel3Logistics.getLogisticProductCode());
      logistics.add(productLevel3LogisticsRequest);
    }
    log.info("update logistics for itemSkus : {} for request : {}", itemSkuList, logistics);
    this.productLevel3LogisticsService.saveLogisticsByItemSku(itemSkuList, businessPartnerCode, logistics, true);
  }


  private void updateInventory(String productSku, Map<String, String> itemSkuItemCodeMap,
      ProfileResponse profileResponse, Map<String, List<String>> itemSkuAndPickupPointMap,
      List<PickupPointResponse> pickupPointResponseList,
      List<ItemPickupPointCodeResponse> itemPickupPointCodeResponseList) throws Exception {

    Map<String, Date> preOrderDateMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(itemPickupPointCodeResponseList)) {
      itemPickupPointCodeResponseList.forEach(response -> {
        if (Objects.nonNull(response.getPreOrderDate())) {
          String key = CommonUtils.toL5Id(response.getItemSku(), response.getPickupPointCode());
          preOrderDateMap.put(key, response.getPreOrderDate());
        }
      });
    }

    List<ProductLevel3Inventory> productLevel3InventoryList = new ArrayList<>();
    for (Map.Entry<String, List<String>> entry : itemSkuAndPickupPointMap.entrySet()) {
      String itemSku = entry.getKey();
      List<String> pickupPoints = entry.getValue();
      for (String pickupPoint : pickupPoints) {
        Optional<PickupPointResponse> pickupPointResponse = pickupPointResponseList.stream()
            .filter(pickupPointResponse1 -> pickupPointResponse1.getCode().equals(pickupPoint)).findFirst();
        if (pickupPointResponse.isPresent()) {
          String preOrderDateKey = CommonUtils.toL5Id(itemSku, pickupPoint);
          Date preOrderDate = preOrderDateMap.get(preOrderDateKey);
          CommonUtils.setProductLevel3InventoryList(
              ItemPickupPointDto.builder().productSku(productSku).itemSku(itemSku)
                  .pickupPointCode(pickupPoint).build(), itemSkuItemCodeMap, profileResponse,
              productLevel3InventoryList, pickupPointResponse, preOrderDate, mppForWhEnabled,
              faasFeatureSwitch, preOrderConfig.isPoQuotaFeatureSwitch());
        }
      }
    }
    List<List<ProductLevel3Inventory>> inventoryPartitionRequest =
        Lists.partition(productLevel3InventoryList, inventoryInsertBatchSizeForEdit);
    log.info("update inventory for request : {}", inventoryPartitionRequest);
    for (List<ProductLevel3Inventory> inventoryList : inventoryPartitionRequest) {
      productLevel3InventoryService.insertInventory(inventoryList);
    }
  }

  private SimpleStringMapResponse getItemIdItemCodeMap(Map<String, String> itemIdItemSkuMap) throws Exception {
    List<String> itemIds = new ArrayList<>(itemIdItemSkuMap.keySet());
    SimpleStringListRequest simpleStringListRequest = new SimpleStringListRequest();
    simpleStringListRequest.setRequest(itemIds);
    return Optional.ofNullable(productRepositoryBean.getSkuCodesByProductItemIds(itemIds)).orElse(new SimpleStringMapResponse());
  }
}
