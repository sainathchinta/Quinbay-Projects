package com.gdn.mta.product.service;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gdn.mta.product.service.config.PreOrderConfig;
import jakarta.persistence.PersistenceException;

import com.gda.mta.product.dto.ItemPickupPointRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.response.InProgressProductResponse;
import com.gda.mta.product.dto.response.ProductBusinessPartnerAndItemViewConfigDto;
import com.gdn.mta.product.commons.constant.ProductLevel3WipSummaryCriteria;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductLevel3Attribute;
import com.gdn.mta.product.entity.ProductSkuBusinessPartnerDTO;
import com.gdn.mta.product.enums.ProductLevel3RetryStatus;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.gda.mta.product.dto.ProductItemBusinessPartnerLogisticsRequest;
import com.gda.mta.product.dto.ProductItemBusinessPartnerRequest;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ItemFlagDetails;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductBusinessPartnerAttribute;
import com.gdn.mta.product.entity.ProductFLow2Audit;
import com.gdn.mta.product.entity.ProductFlow2Status;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductItemSyncProcess;
import com.gdn.mta.product.entity.ProductItemSyncProcessSummary;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.entity.RejectedSkuProductCollection;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.ProductSyncStatus;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductBusinessPartnerCustomRepository;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductFlow2AuditRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import com.gdn.mta.product.service.util.MapperUtil;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.partners.pbp.calendar.CalendarService;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import com.gdn.partners.pbp.commons.util.CommonUtils;
import com.gdn.partners.pbp.dto.productlevel3.SuspensionItemResponse;
import com.gdn.partners.pbp.helper.RequestHelper;
import com.gdn.partners.pbp.model.productlevel3.CreateProductLevel3Response;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.pbp.service.notification.ProductNotificationService;
import com.gdn.partners.pbp.service.productlevel3.ProductItemWholesalePriceService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3AggregatorServiceOld;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3InventoryService;
import com.gdn.partners.pbp.service.sysparam.SystemParameterService;
import com.gdn.partners.product.orchestrator.constant.ProductLevel1State;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.gdn.x.product.rest.web.model.dto.B2bFieldsDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import com.gdn.x.product.rest.web.model.response.ItemSummaryDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductTypeResponse;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@SuppressWarnings("deprecation")
@Lazy
@Transactional(readOnly = true, rollbackFor = Exception.class)
public class ProductBusinessPartnerServiceBean implements ProductBusinessPartnerService {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductBusinessPartnerServiceBean.class);

  private static final String IN_PROGRESS_STATE = "IN_PROGRESS";
  private static final String CREATED_DATE = "createdDate";
  private static final String UPDATED_DATE = "updatedDate";
  private static final String ASC = "asc";
  private static final String DESC = "desc";
  private static final String CONTENT_NOT_AUTHORIZED = "You don't have authorize for this content";
  private static final String PRODUCT_SUCCESSFULLY_ADDED_AS_FBB = "Semua produk tersalin ke FBB. ";
  private static final String PRODUCT_PARTIALLY_ADDED_AS_FBB = "%s dari %s produk tersalin ke FBB. ";
  private static final long DEFAULT_SYNC_RETRY_DURATION = 24;
  private static final String BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY = "Business partner code cannot be empty";

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Autowired
  private ProductBusinessPartnerCustomRepository productBusinessPartnerCustomRepository;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  @Lazy
  private ProductLevel3Service productLevel3Service;

  @Autowired
  private ProductWorkflowService productWorkflowService;

  @Autowired
  private ProductFlow2AuditRepository productFlow2AuditRepository;

  @Autowired
  private ProductGdnSkuGeneratorService productGdnSkuGeneratorService;

  @Autowired
  private ProductNotificationService productNotificationService;

  @Autowired
  private ProductLevel3InventoryService productLevel3InventoryService;

  @Autowired
  private CalendarService calendarService;

  @Autowired
  private ProductStatusPublisherService productStatusPublisherService;

  @Autowired
  private ProductItemSyncService syncStatusService;

  @Autowired
  private ProductItemSyncProcessService syncProcessService;

  @Autowired
  private ProductSystemParameterService productSystemParameterService;

  @Autowired
  @Qualifier("productLevel3DirectAggregatorService")
  private ProductLevel3AggregatorServiceOld productLevel3DirectAggregatorService;

  @Autowired
  @Qualifier("globalSystemParameterService")
  private SystemParameterService systemParameterService;

  @Value("${use.original.price.enabled}")
  private boolean useOriginalPriceEnabled;

  @Value("${mpp.for.wh.enabled}")
  private boolean mppForWhEnabled;

  @Value("${make.inactive.seller.products.offline}")
  private boolean makeInactiveSellerProductOffline;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${instore.new.flow.enabled}")
  private boolean instoreNewFlowEnabled;

  @Autowired
  private MapperUtil mapperUtil;

  @Autowired
  private ProductItemWholesalePriceService productItemWholesalePriceService;

  @Autowired
  private XProductOutbound xProductOutbound;

  @Autowired
  private ProductLevel3RetryService productLevel3RetryService;

  @Autowired
  private BundleRecipeService bundleRecipeService;

  @Autowired
  private PreOrderConfig preOrderConfig;

  private CreateProductLevel3Response createProductLevel3(String storeId,
      String productBusinessPartnerId, ProductDetailResponse productData,
      String businessPartnerCode, boolean isRetry, boolean skipNotification,
      List<ProductLevel3Logistics> logistics) {
    ProductBusinessPartner productBusinessPartner =
        this.productBusinessPartnerRepository.findById(productBusinessPartnerId).orElse(null);
    if (StringUtils.isEmpty(productBusinessPartner.getGdnProductSku())) {
      this.productGdnSkuGeneratorService.generateGdnSkuOnProduct(productBusinessPartner, false);
    }
    productBusinessPartner.setActivated(true);
    boolean isSuccess = true;
    boolean isLogisticsSaveSuccess = true;
    try {
      if (!isRetry || productLevel3Service
          .findDetailByProductSku(productBusinessPartner.getGdnProductSku()) == null) {
        isLogisticsSaveSuccess =
            this.productLevel3Service.create(productBusinessPartner.getBusinessPartnerId(),
                productData, productBusinessPartner, false, logistics);
        productLevel3RetryService.updateCompletedOrOmittedState(storeId, productBusinessPartner.getGdnProductSku(),
          ProductLevel3RetryStatus.COMPLETED.name());
      } else {
        productLevel3RetryService.updateCompletedOrOmittedState(storeId, productBusinessPartner.getGdnProductSku(),
          ProductLevel3RetryStatus.OMITTED.name());
      }
      productBusinessPartner.setMarkForDelete(true);
      productBusinessPartner.setState("ACTIVE");
      if (!skipNotification) {
        productNotificationService.sendProductActiveNotification(businessPartnerCode,
            productBusinessPartner.getGdnProductSku());
      }
    } catch (Exception e) {
      isSuccess = false;
      String generatedGdnSkus = this.productGdnSkuGeneratorService.convertToGeneratedGdnSkus(productBusinessPartner);
      ProductBusinessPartnerServiceBean.LOGGER.error(
          "error invoking create product level 3 at service. Business Partner Code : " + productBusinessPartner
              .getBusinessPartnerId() + ". Product Id : " + productBusinessPartnerId + ". Product Code : " + productData
              .getProductCode() + "Generated GDN SKU :" + generatedGdnSkus, e);
    }
    LOGGER.debug("updating product business partner post level 3 creation : {}", productBusinessPartner);
    this.productBusinessPartnerRepository.save(productBusinessPartner);
    return CreateProductLevel3Response.builder().isSuccess(isSuccess)
        .isLogisticsSaveSuccess(isLogisticsSaveSuccess).build();
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void delete(String id) throws Exception {
    ProductBusinessPartner savedProductBusinessPartner = this.productBusinessPartnerRepository.findById(id).orElse(null);
    if (savedProductBusinessPartner == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, "at product business partner with id : " + id);
    }
    try {
      this.productBusinessPartnerRepository.delete(savedProductBusinessPartner);
    } catch (Exception e) {
      throw new ApplicationException(ErrorCategory.DATA_ACCESS, "product business partner", e);
    }
  }


  @Override
  public Page<ProductBusinessPartner> findByActivatedTrue(String storeId, Pageable pageable) throws Exception {
    return this.productBusinessPartnerRepository.findByStoreIdAndActivatedTrueAndMarkForDeleteFalse(storeId, pageable);
  }

  @Override
  public Page<ProductBusinessPartner> findActiveProductsByBusinessPartnerId(String storeId, String businessPartnerId,
      Pageable pageable) {
    return this.productBusinessPartnerRepository
        .findByStoreIdAndBusinessPartnerIdAndMarkForDeleteTrueOrderByCreatedDateDesc(storeId, businessPartnerId,
            pageable);
  }

  @Override
  public Page<ProductBusinessPartner> findByBusinessPartnerId(String storeId, String businessPartnerId,
      Pageable pageable) {
    return this.productBusinessPartnerRepository
        .findByStoreIdAndBusinessPartnerIdAndMarkForDeleteFalseOrderByCreatedDateDesc(storeId, businessPartnerId,
            pageable);
  }

  @Override
  public String findProductStateByStoreIdAndItemSku(String storeId, String itemSku) throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_GDN_SKU_MUST_NOT_BE_BLANK);
    return this.productBusinessPartnerRepository.findProductStateByStoreIdAndItemSku(storeId, itemSku);
  }

  @Override
  public String validateProductSku(String gdnProductSku) throws Exception {
    try {
      ProductBusinessPartner productBusinessPartner =
          productBusinessPartnerRepository.findFirstByGdnProductSku(gdnProductSku);

      if (productBusinessPartner != null) {
        return productBusinessPartner.getBusinessPartnerId();
      } else {
        throw new Exception("productBusinessPartner is null");
      }
    } catch (Exception e) {
      LOGGER.error("PBP repository error:" + e.getMessage(), e);
      throw new Exception("PBP repository error:" + e.getMessage());
    }
  }

  @Override
  public Page<RejectedSkuProductCollection> findRejectedProductsByBusinessPartnerId(String storeId,
      String businessPartnerId, Pageable pageable, String searchCriteria) throws Exception {
    if (StringUtils.isNotEmpty(searchCriteria)) {
      return this.productBusinessPartnerRepository
          .findRejectedProductsByBusinessPartnerIdAndProductName(storeId, businessPartnerId, searchCriteria, pageable);
    }
    return this.productBusinessPartnerRepository
        .findRejectedProductsByBusinessPartnerId(storeId, businessPartnerId, pageable);
  }

  @Override
  public Page<RejectedSkuProductCollection> findRejectedProductsByBusinessPartnerIdAndMerchantSku(String storeId,
      String businessPartnerId, Pageable pageable, String merchantSku) throws Exception {
    return this.productBusinessPartnerRepository
        .findRejectedProductsByBusinessPartnerIdAndMerchantSku(storeId, businessPartnerId, merchantSku, pageable);
  }

  @Override
  public ProductBusinessPartner findById(String id) throws Exception {
    return this.productBusinessPartnerRepository.findById(id).orElse(null);
  }

  @Override
  public Page<ProductBusinessPartner> findByPickupPointId(String storeId, String pickupPointId, Pageable pageable)
      throws PersistenceException {
    return this.productBusinessPartnerRepository
        .findByStoreIdAndPickupPointIdAndMarkForDeleteFalse(storeId, pickupPointId, pageable);
  }

  @Override
  public Page<ProductBusinessPartner> findByStoreId(String storeId, Pageable pageable) throws Exception {
    return this.productBusinessPartnerRepository.findByStoreIdAndMarkForDeleteFalse(storeId, pageable);
  }

  private List<ProductLevel3Inventory> generateInventoryForInsert(Product product,
      ProductBusinessPartner productBusinessPartner, ProfileResponse businessPartner) throws Exception {
    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    for (ProductItemBusinessPartner productItemBusinessPartner : productBusinessPartner
        .getProductItemBusinessPartners()) {
      ProductItem savedProductItem = null;
      for (ProductItem productItem : product.getProductItems()) {
        if (productItem.getId().equals(productItemBusinessPartner.getProductItemId())) {
          savedProductItem = productItem;
          break;
        }
      }
      ProductLevel3Inventory inventory = new ProductLevel3Inventory();
      if (isPurchaseOrderPurchaseTerm(businessPartner)) {
        inventory.setWarehouseMerchantCode(GdnBaseLookup.DEFAULT_BUSINESS_PARTNER_CODE);
      } else {
        inventory.setWarehouseMerchantCode(businessPartner.getBusinessPartnerCode());
      }
      if (savedProductItem == null) {
        throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
            "[productItem for:" + productItemBusinessPartner.getGdnProductItemSku() + " not found]");
      }
      inventory.setWarehouseItemSku(savedProductItem.getSkuCode());
      inventory.setWebMerchantCode(businessPartner.getBusinessPartnerCode());
      inventory.setWebItemSku(productItemBusinessPartner.getGdnProductItemSku());
      inventory.setWebAvailable(productItemBusinessPartner.getStock());
      inventory.setWebMinAlert(productItemBusinessPartner.getMinimumStock());
      inventory.setWebSyncStock(false);
      inventory.setWebPickupPointCode(productItemBusinessPartner.getPickupPointId());
      inventory.setProductSku(productBusinessPartner.getGdnProductSku());
      inventory.setFbbPP(productItemBusinessPartner.isFbbActive() && mppForWhEnabled);
      inventory.setDistributionPickupPoint(productItemBusinessPartner.isDistribution());
      com.gdn.mta.product.util.CommonUtils.setPreOrderFields(
          preOrderConfig.isPoQuotaFeatureSwitch(), businessPartner, productBusinessPartner.getPreOrderDate(),
          inventory, productItemBusinessPartner.getPreOrderQuota());
      inventories.add(inventory);
    }
    return inventories;
  }

  private boolean isPurchaseOrderPurchaseTerm(ProfileResponse businessPartner) throws Exception {
    return GdnBaseLookup.PURCHASE_TERM_PURCHASE_ORDER.equals(businessPartner.getCompany().getPurchaseTerm());
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void retrySave(ProductBusinessPartner productBusinessPartner) throws Exception {
    ProductBusinessPartner savedProductBusinessPartner =
        this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()).orElse(null);
    if (savedProductBusinessPartner == null) {
      throw new ApplicationException(ErrorCategory.DATA_ACCESS, CONTENT_NOT_AUTHORIZED);
    }
    Product product = this.productRepository.findOne(savedProductBusinessPartner.getProductId());
    if (!product.isViewable()) {
      throw new ApplicationException(ErrorCategory.INVALID_STATE,
          "at product business partner with product viewable false, but try to save it");
    }
    ProfileResponse businessPartner = this.businessPartnerRepository
        .filterDetailByBusinessPartnerCode(savedProductBusinessPartner.getBusinessPartnerId());
    validateShippingWeightAndBusinessPartner(savedProductBusinessPartner, businessPartner, product.getShippingWeight());

    this.productLevel3Service.create(businessPartner.getBusinessPartnerCode(), product, savedProductBusinessPartner);
    List<ProductLevel3Inventory> inventories =
        generateInventoryForInsert(product, savedProductBusinessPartner, businessPartner);
    if (CollectionUtils.isNotEmpty(inventories)) {
      this.productLevel3InventoryService.insertInventory(inventories);
    }
    savedProductBusinessPartner.setUpdatedBy(productBusinessPartner.getUpdatedBy());
    savedProductBusinessPartner.setUpdatedDate(productBusinessPartner.getUpdatedDate());
    savedProductBusinessPartner.setMarkForDelete(true);
    for (ProductItemBusinessPartner productItemBusinessPartner : savedProductBusinessPartner
        .getProductItemBusinessPartners()) {
      productItemBusinessPartner.setMarkForDelete(true);
    }
    this.productBusinessPartnerRepository.save(savedProductBusinessPartner);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public String save(ProductBusinessPartner productBusinessPartner) throws Exception {
    boolean masterDataUpdated = false;
    String businessPartnerId = StringUtils.EMPTY;
    if (!StringUtils.isEmpty(productBusinessPartner.getId())) {
      ProductBusinessPartner savedProductBusinessPartner =
          this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()).orElse(null);
      if (savedProductBusinessPartner != null) {
        throw new ApplicationException(ErrorCategory.DATA_ACCESS,
            "duplicate data detected at product business partner with id : " + productBusinessPartner.getId());
      }
    }

    // Validate product activation and shipping weight of product item business partner
    Product product = this.productRepository.findOne(productBusinessPartner.getProductId());
    if (!product.isActivated()) {
      LOGGER.error("Product is not activated with product code : {}", product.getProductCode());
      throw new ApplicationException(ErrorCategory.INVALID_STATE,
          "at product business partner with product activated false, but try to save it");
    }
    // Validate business partner activation of product item business partner
    ProfileResponse businessPartner =
        this.businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId());
    validateShippingWeightAndBusinessPartner(productBusinessPartner, businessPartner, product.getShippingWeight());

    // Create new product to wcs and x-inventory
    try {
      LOGGER.info("Saving product data in X-Product for productCode : {} in save api:", product.getProductCode());
      this.productLevel3Service.create(businessPartner.getBusinessPartnerCode(), product, productBusinessPartner);
      LOGGER.info("Product data saved successfully in X-Product for productCode : {} in save api:", product.getProductCode());
      LOGGER.info("Saving item data in X-inventory for product code in save api: {}", product.getProductCode());
      List<ProductLevel3Inventory> inventories =
          generateInventoryForInsert(product, productBusinessPartner, businessPartner);
      this.productLevel3InventoryService.insertInventory(inventories);
      LOGGER.info("Items successfully saved in x-inventory for productCode in save api: {}", product.getProductCode());
      productBusinessPartner.setMarkForDelete(true);
      masterDataUpdated = true;
    } catch (Exception e) {
      LOGGER.error("error invoking save new product to wcs at service in save api for product code : {}", product.getProductCode(),
          e);
    }
    businessPartnerId = this.productBusinessPartnerRepository.saveAndFlush(productBusinessPartner).getId();
    createFlow2Audit(product.getProductCode(), businessPartner.getBusinessPartnerCode(), masterDataUpdated);
    return businessPartnerId;
  }

  private void createFlow2Audit(String productCode, String businessPartnerCode, boolean masterDataUpdated) {
    ProductFlow2Status status = ProductFlow2Status.SUCCESS;
    if (!masterDataUpdated) {
      status = ProductFlow2Status.SUCCESS_MASTER_DATA_FAILED;
    }
    ProductFLow2Audit audit = new ProductFLow2Audit(businessPartnerCode, StringUtils.EMPTY, status.name(), productCode);
    productFlow2AuditRepository.save(audit);

  }

  @Override
  public List<ProductBusinessPartner> findByStoreIdAndProductIdAndMarkForDeleteFalse(String storeId, String productId)
      throws Exception {
    Pageable pageable = PageRequest.of(0, 1000);
    Page<ProductBusinessPartner> productBusinessPartners = this.productBusinessPartnerRepository
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(storeId, productId, pageable);
    return productBusinessPartners.getContent();
  }

  @Override
  public List<ProductBusinessPartner> findByStoreIdAndProductId(String storeId, String productId) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(productId), ErrorMessages.PRODUCT_ID_BLANK);
    List<ProductBusinessPartner> productBusinessPartners = this.productBusinessPartnerRepository
        .findByStoreIdAndProductId(storeId, productId);
    return productBusinessPartners;
  }

  @Override
  public List<ProductBusinessPartner> findInactiveProductBusinessPartnersOfActiveL1(String storeId, String productId)
      throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(productId), ErrorMessages.PRODUCT_ID_BLANK);
    return this.productBusinessPartnerRepository.findByStoreIdAndProductIdAndStateAndMarkForDeleteFalse(storeId, productId,
        IN_PROGRESS_STATE);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public String saveWithActivatedFalse(ProductBusinessPartner productBusinessPartner) throws Exception {
    if (!StringUtils.isEmpty(productBusinessPartner.getId())) {
      ProductBusinessPartner savedProductBusinessPartner =
          this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()).orElse(null);
      if (savedProductBusinessPartner != null) {
        throw new ApplicationException(ErrorCategory.DATA_ACCESS,
            "duplicate data detected at product business partner with id : " + productBusinessPartner.getId());
      }
    }

    ProfileResponse businessPartner =
        this.businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId());

    // Validate shipping weight of product item business partner and product type international seller
    Product product = this.productRepository.findOne(productBusinessPartner.getProductId());
    validateShippingWeightAndBusinessPartner(productBusinessPartner, businessPartner, product.getShippingWeight());

    this.productGdnSkuGeneratorService.generateGdnSkuOnProduct(productBusinessPartner, false);
    productBusinessPartner.setState(IN_PROGRESS_STATE);
    productBusinessPartner.setExpectedActivationDate(getExpectedDateOfActivation(product));
    productBusinessPartner.setSubmittedDate(product.getCreatedDate());

    try {
      this.productBusinessPartnerRepository.saveAndFlush(productBusinessPartner);
    } catch (Exception e) {
      LOGGER.error("error invoking saveWithActivatedFalse with productBusinessPartner : {}", productBusinessPartner, e);
      throw new Exception("saveWithActivatedFalse error:" + e.getMessage());
    }
    return productBusinessPartner.getId();
  }

  private void validateShippingWeightAndBusinessPartner(ProductBusinessPartner productBusinessPartner,
      ProfileResponse businessPartner, Double shippingWeight) throws Exception {
    // Validate business partner activation of product item business partner

    if (!makeInactiveSellerProductOffline) {
      GdnPreconditions.checkArgument(
          Objects.nonNull(businessPartner) && Constants.ACTIVE.equalsIgnoreCase(businessPartner.getMerchantStatus()),
          ErrorMessages.INACTIVE_BUSINESS_PARTNER_MSG + productBusinessPartner.getProductId());
    }

    boolean isSellerInActive =
        Objects.nonNull(businessPartner) && !Constants.ACTIVE.equalsIgnoreCase(businessPartner.getMerchantStatus());
    boolean pureInstoreProduct =
        instoreNewFlowEnabled
            && Boolean.TRUE.equals(productBusinessPartner.isOff2OnChannelActive())
            && Boolean.FALSE.equals(productBusinessPartner.isB2cActivated());

    for (ProductItemBusinessPartner productItemBusinessPartner : productBusinessPartner
        .getProductItemBusinessPartners()) {
      if (makeInactiveSellerProductOffline && isSellerInActive) {
        productItemBusinessPartner.setBuyable(false);
        productItemBusinessPartner.setDisplay(false);
      }
      if (productItemBusinessPartner.getProductType() != GdnBaseLookup.PRODUCT_TYPE_REGULAR.intValue()
          && productItemBusinessPartner.getProductType() != GdnBaseLookup.PRODUCT_TYPE_BIG_PRODUCT.intValue()){
        productItemBusinessPartner.setProductType(GdnBaseLookup.PRODUCT_TYPE_BOPIS);
      }

      if (pureInstoreProduct && (shippingWeight == null || shippingWeight <= 0.0)) {
        productItemBusinessPartner.setProductType(GdnBaseLookup.PRODUCT_TYPE_REGULAR);
      } else {
        if (productItemBusinessPartner.getProductType() != GdnBaseLookup.PRODUCT_TYPE_BOPIS.intValue() && (
            shippingWeight == null || shippingWeight <= 0.0)) {
          log.error("Shipping weight validation failed for product : {} and shipping weight : {} ",
              productBusinessPartner, shippingWeight);
          throw new ApplicationException(ErrorCategory.INVALID_STATE, ApiErrorCode.SHIPPING_WEIGHT_NOT_VALID.getDesc());
        }
      }
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public ProductBusinessPartner saveBusinessPartner(ProductBusinessPartner productBusinessPartner,
      ProductDetailResponse productDetailResponse, boolean isMPPFlow) throws Exception {

    ProfileResponse businessPartner =
        this.businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId());
    validateShippingWeightAndBusinessPartner(productBusinessPartner, businessPartner,
        productDetailResponse.getShippingWeight());

    this.productGdnSkuGeneratorService.generateGdnSkuOnProduct(productBusinessPartner, isMPPFlow);
    productBusinessPartner.setState(IN_PROGRESS_STATE);
    productBusinessPartner.setExpectedActivationDate(getExpectedDateOfActivation(productDetailResponse));
    productBusinessPartner.setSubmittedDate(productDetailResponse.getCreatedDate());
    return this.productBusinessPartnerRepository.save(productBusinessPartner);
  }

  /**
   * return expected activation date on the basis of category with the help of calendar service
   *
   * @param product master product detail
   * @return expected activation date for product
   */
  private Date getExpectedDateOfActivation(Product product) {
    Date expectedDate = null;
    if (CollectionUtils.isNotEmpty(product.getProductCategories())) {
      String categoryCode = product.getProductCategories().get(0).getCategory().getCategoryCode();
      expectedDate = calendarService.getExpectedActivationDateByCategoryCode(categoryCode, product.getCreatedDate());
    }
    return expectedDate;
  }

  /**
   * return expected activation date on the basis of category with the help of calendar service
   *
   * @param product master product detail response
   * @return expected activation date for product
   */
  private Date getExpectedDateOfActivation(ProductDetailResponse product) {
    Date expectedDate = null;
    if (CollectionUtils.isNotEmpty(product.getProductCategoryResponses())) {
      String categoryCode = product.getProductCategoryResponses().get(0).getCategory().getCategoryCode();
      expectedDate = calendarService.getExpectedActivationDateByCategoryCode(categoryCode, product.getCreatedDate());
    }
    return expectedDate;
  }

  public List<ProductBusinessPartnerAttribute> getSkuValueTrueAttributeList(List<String> attributeIdList,
      String productId) {
    return this.productBusinessPartnerRepository.findByProductId(attributeIdList, productId);
  }

  public Integer getProductTypeBasedOnProductId(String productId) {
    return this.productBusinessPartnerRepository.getProductTypeBasedOnProductId(productId);
  }

  @Override
  public Integer getProductTypeBasedOnProductCodeOrId(String productCode, String productId) {
    ProductTypeResponse productType = xProductOutbound.getProductTypeByProductCode(productCode);
    if (Objects.nonNull(productType)) {
      return ConverterUtil.getCodeFromProductType(productType.getProductType());
    }
    return this.productBusinessPartnerRepository.getProductTypeBasedOnProductId(productId);
  }

  public void setBusinessPartnerRepository(BusinessPartnerRepository businessPartnerRepository) {
    this.businessPartnerRepository = businessPartnerRepository;
  }

  public void setProductBusinessPartnerRepository(ProductBusinessPartnerRepository productBusinessPartnerRepository) {
    this.productBusinessPartnerRepository = productBusinessPartnerRepository;
  }

  public void setProductRepository(ProductRepository productRepository) {
    this.productRepository = productRepository;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void update(ProductBusinessPartner productBusinessPartner) throws Exception {
    ProductBusinessPartner savedProductBusinessPartner =
        this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()).orElse(null);
    if (savedProductBusinessPartner == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "at product business partner, but try to update it with id : " + productBusinessPartner.getId());
    }
    BeanUtils.copyProperties(productBusinessPartner, savedProductBusinessPartner, "createdDate", "createdBy");
    this.productBusinessPartnerRepository.save(savedProductBusinessPartner);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void saveProductBusinessPartner(ProductBusinessPartner productBusinessPartner) {
    this.productBusinessPartnerRepository.save(productBusinessPartner);
  }

  @Override
  public Date getExpectedActivationDateByCategoryCode(ProductDetailResponse productDetailResponse, Date submitDate) {
    return CollectionUtils.isNotEmpty(productDetailResponse.getCategoryCodes()) ?
        calendarService
            .getExpectedActivationDateByCategoryCode(productDetailResponse.getCategoryCodes().get(0), submitDate) :
        null;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public CreateProductLevel3Response create(String storeId, ProductBusinessPartner productBusinessPartner, boolean skipNotification,
      List<ProductItemBusinessPartnerRequest> businessPartnerRequestList)
      throws Exception {
    if (!StringUtils.isEmpty(productBusinessPartner.getId())) {
      ProductBusinessPartner existProductBusinessPartner =
          this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()).orElse(null);
      if (existProductBusinessPartner != null) {
        throw new ApplicationException(ErrorCategory.DATA_ACCESS,
            "Product Id : " + productBusinessPartner.getId() + ". Error Code : product business partner already exist");
      }
    }
    ProductDetailResponse productData = this.productRepository.findDetailById(productBusinessPartner.getProductId());
    if (!productData.isViewable()) {
      throw new ApplicationException(ErrorCategory.INVALID_STATE,
          "Product Code : " + productData.getProductCode() + ". Error Code : state of product is invalid");
    }
    ProfileResponse businessPartner =
        this.businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId());
    validateShippingWeightAndBusinessPartner(productBusinessPartner, businessPartner, productData.getShippingWeight());
    this.productGdnSkuGeneratorService.generateGdnSkuOnProduct(productBusinessPartner, false);
    productBusinessPartner.setState(IN_PROGRESS_STATE);

    productBusinessPartner = this.productBusinessPartnerRepository.saveAndFlush(productBusinessPartner);

    List<ProductItemWholesalePrice> productItemWholesalePrices =
        getProductItemWholesalePrices(productBusinessPartner, businessPartnerRequestList);

    if (org.apache.commons.collections.CollectionUtils.isNotEmpty(productItemWholesalePrices)) {
      productItemWholesalePriceService.saveWholesalePrice(productItemWholesalePrices);
    }

    List<ProductLevel3Logistics> logistics = getProductItemLogistics(businessPartnerRequestList);

    LOGGER.info("creating level 3 product with product business partner : {}", productBusinessPartner.getGdnProductSku());
    CreateProductLevel3Response response = createProductLevel3(storeId,
        productBusinessPartner.getId(), productData, productBusinessPartner.getBusinessPartnerId(),
        Boolean.FALSE, skipNotification, logistics);

    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "Product Id : " + productBusinessPartner.getId() + ". Error Code : failed to create product level 3");
    }
    createFlow2Audit(productData.getProductCode(), productBusinessPartner.getBusinessPartnerId(), true);
    return response;
  }

  private List<ProductItemWholesalePrice> getProductItemWholesalePrices(ProductBusinessPartner productBusinessPartner,
      List<ProductItemBusinessPartnerRequest> businessPartnerRequestList) throws Exception {
    Map<String, String> itemIdToGdnSkuMap = productBusinessPartner.getProductItemBusinessPartners().stream().collect(
        Collectors
            .toMap(ProductItemBusinessPartner::getProductItemId, ProductItemBusinessPartner::getGdnProductItemSku));
    List<ProductItemWholesalePrice> productItemWholesalePrices = new ArrayList<>();
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : businessPartnerRequestList) {
      if (org.apache.commons.collections.CollectionUtils
          .isNotEmpty(productItemBusinessPartnerRequest.getProductItemWholesalePriceRequests())) {
        ProductItemWholesalePrice productItemWholesalePrice = new ProductItemWholesalePrice();
        productItemWholesalePrice.setItemCode(productItemBusinessPartnerRequest.getItemCode());
        if (Objects.nonNull(productItemBusinessPartnerRequest.getWholesalePriceActivated())) {
          productItemWholesalePrice.setWholesalePriceActivated(productItemBusinessPartnerRequest.getWholesalePriceActivated());
        }
        productItemWholesalePrice.setProductItemId(productItemBusinessPartnerRequest.getProductItemId());
        productItemWholesalePrice.setItemSku(itemIdToGdnSkuMap.get(productItemBusinessPartnerRequest.getProductItemId()));
        productItemWholesalePrice.setStoreId(productBusinessPartner.getStoreId());
        productItemWholesalePrice.setWholesaleRules(
            mapperUtil.mapRequestToString(productItemBusinessPartnerRequest.getProductItemWholesalePriceRequests()));
        productItemWholesalePrices.add(productItemWholesalePrice);
      }
    }
    return productItemWholesalePrices;
  }

  private List<ProductLevel3Logistics> getProductItemLogistics(
      List<ProductItemBusinessPartnerRequest> businessPartnerRequestList) {
    List<ProductLevel3Logistics> productLevel3LogisticsList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(businessPartnerRequestList)) {
      for (ProductItemBusinessPartnerLogisticsRequest productItemBusinessPartnerLogisticsRequest : businessPartnerRequestList
          .get(0).getProductItemBusinessPartnerLogisticsRequests()) {
        ProductLevel3Logistics productLevel3Logistics = ProductLevel3Logistics.builder()
            .logisticProductCode(
                productItemBusinessPartnerLogisticsRequest.getLogisticProductCode())
            .selected(productItemBusinessPartnerLogisticsRequest.isSelected()).build();
        productLevel3LogisticsList.add(productLevel3Logistics);
      }
    }
    return productLevel3LogisticsList;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void retryCreate(String storeId, String productBusinessPartnerId, ProductBusinessPartner productBusinessPartner)
    throws Exception {
    if (Objects.isNull(productBusinessPartner)) {
      productBusinessPartner = this.productBusinessPartnerRepository.findById(productBusinessPartnerId).orElse(null);
    }
    if (productBusinessPartner == null) {
      throw new ApplicationException(ErrorCategory.DATA_ACCESS,
          "Product Id : " + productBusinessPartnerId + ". Error Code : product business partner is not exist");
    }
    ProductDetailResponse productData = this.productRepository.findDetailById(productBusinessPartner.getProductId());
    if (Objects.isNull(productData)) {
      productLevel3RetryService.updateCompletedOrOmittedState(storeId, productBusinessPartner.getGdnProductSku(),
        ProductLevel3RetryStatus.OMITTED.name());
      return;
    }
    ProfileResponse businessPartner =
      this.businessPartnerRepository.filterDetailByBusinessPartnerCode(
        productBusinessPartner.getBusinessPartnerId());
    validateShippingWeightAndBusinessPartner(productBusinessPartner, businessPartner, productData.getShippingWeight());
    CreateProductLevel3Response response = createProductLevel3(storeId, productBusinessPartnerId, productData,
        productBusinessPartner.getBusinessPartnerId(), Boolean.TRUE, false, new ArrayList<>());
    if (!response.isSuccess()) {
      this.productLevel3RetryService.upsertProductLevel3FailureLog(storeId, productBusinessPartner.getGdnProductSku());
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "Product Id : " + productBusinessPartnerId + ". Error Code : failed to create product level 3");
    }
  }

  public void setProductFlow2AuditRepository(ProductFlow2AuditRepository productFlow2AuditRepository) {
    this.productFlow2AuditRepository = productFlow2AuditRepository;
  }

  @Override
  public Integer getMinimumStockByGdnProductItemSku(String itemSku) throws Exception {
    return this.productBusinessPartnerRepository.getMinimumStockByGdnProductItemSku(itemSku);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public void updateMinimumStockByGdnProductItemSku(String itemSku, Integer minimumStock) throws Exception {
    try {
      this.productBusinessPartnerRepository.updateMinimumStockByGdnProductItemSku(itemSku, minimumStock);
    } catch (Exception e) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "Failed to update minimum stock product with gdn sku " + itemSku + "and minimum stock " + minimumStock, e);
    }
  }

  @Trace(dispatcher = true)
  @Async
  @Override
  @Transactional(readOnly = false)
  public void markItemsAsUnBuyableAndUnViewable(String productId) {
    LOGGER.info("making items unubuyable for items of productId : {}", productId);
    int response = productBusinessPartnerRepository.markItemsAsUnBuyableAndUnViewable(productId);
    if (response == 0) {
      LOGGER.error("Items of productId : {} were not marked unbuyable and unDisplayable");
    }
  }

  @Trace(dispatcher = true)
  @Async
  @Override
  @Transactional(readOnly = false)
  public void updateSkuValueTrueInProductBusinessPartnerAttribute(List<ProductAttribute> productAttributeList,
      String productId) {
    LOGGER.info("Updating sku value true attributes for productId : {}", productId);
    for (ProductAttribute productAttribute : productAttributeList) {
      if (DescriptiveAttributeValueType.SINGLE
          .equals(productAttribute.getProductAttributeValues().get(0).getDescriptiveAttributeValueType())) {
        productBusinessPartnerRepository.updateSkuTrueAttributeInProductBusinessPartnerAttribute(
            productAttribute.getProductAttributeValues().get(0).getDescriptiveAttributeValue(),
            productAttribute.getAttribute().getId(), productId);
      }
    }
  }

  @Trace(dispatcher = true)
  @Async
  @Override
  @Transactional(readOnly = false)
  public void markItemsAsDeletedOnProductResubmission(String businessPartnerId, String productId) {
    try {
      this.productBusinessPartnerRepository.markItemsAsDeletedOnProductResubmission(businessPartnerId, productId);
      this.productBusinessPartnerRepository
          .markBusinessPartnerAttributesAsDeletedOnProductResubmission(businessPartnerId, productId);
      this.productBusinessPartnerRepository.markProductAsDeletedOnProductResubmission(businessPartnerId, productId);
    } catch (Exception e) {
      LOGGER.error(
          "Error while updating product business partners of revised products, oldProductId:{}, businessPartnerId :{}",
          productId, businessPartnerId);
    }
  }

  @Async
  @Override
  @Transactional(readOnly = false, propagation = Propagation.REQUIRES_NEW)
  public void copyAllProducts(String storeId, String username, String linkedPartnerCode, String partnerCode,
      String pickupPointCode) throws Exception {
    Integer itemEligibleForSync = 0;
    int currentPage = 0, pageSize = 20;
    String processId = UUID.randomUUID().toString();

    List<String> alreadyInSync =
        syncStatusService.findItemsAlreadyInSyncProcess(storeId, partnerCode, linkedPartnerCode);

    ProductLevel3SummaryFilter filterRequest =
        ProductLevel3SummaryFilter.builder().businessPartnerCode(linkedPartnerCode).storeId(storeId)
            .archived(false).excludedItemSkus(alreadyInSync).build();

    Page<ProductLevel3Summary> summary = null;
    do {
      Pageable pageable = PageRequest.of(currentPage, pageSize);
      summary = productLevel3DirectAggregatorService.aggregateProductSummaryWithoutInventory(filterRequest, pageable);

      Map<String, List<String>> gdnProductItemSkus = summary.getContent().stream().collect(Collectors
        .groupingBy(ProductLevel3Summary::getProductSku,
          Collectors.mapping(ProductLevel3Summary::getItemSku, Collectors.toList())));

      itemEligibleForSync += syncStatusService
          .copy(storeId, username, partnerCode, pickupPointCode, false, processId, gdnProductItemSkus, linkedPartnerCode);
      currentPage = currentPage + 1;
    } while (Objects.nonNull(summary) && currentPage < summary.getTotalPages());

    LOGGER.info("items eligible for sync process {}", itemEligibleForSync);
    if (itemEligibleForSync > 0) {
      ProductItemSyncProcess productItemSyncProcess =
          ProductItemSyncProcess.builder().isUserNotified(false).processId(processId).build();
      productItemSyncProcess.setStoreId(storeId);
      syncProcessService.save(productItemSyncProcess);
    } else {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, ERR_CODE_NO_AVAILABLE_VALID_ITEMS);
    }
  }

  @Override
  @Transactional(readOnly = false)
  public void notifyForProductCopyingProcess(String storeId) {
    List<ProductItemSyncProcess> eligibleProcesses = syncProcessService.findAllProcessEligibleForNotification(storeId);
    LOGGER.debug("eligible product copying  processes for notification {}", eligibleProcesses);

    for (ProductItemSyncProcess productItemSyncProcess : eligibleProcesses) {
      List<ProductItemSyncProcessSummary> statusCount =
          syncStatusService.productCopyStatusForProcessID(storeId, productItemSyncProcess.getProcessId());
      LOGGER.debug("count of status products for a process {}", statusCount);

      Optional<ProductItemSyncProcessSummary> processInProgress = statusCount.stream()
          .filter(statusSummary -> ProductSyncStatus.IN_PROGRESS.equals(statusSummary.getProductSyncStatus()))
          .findAny();

      if (!statusCount.isEmpty() && !processInProgress.isPresent()) {
        this.sendProductItemSyncNotification(statusCount, productItemSyncProcess,
            statusCount.get(0).getBusinessPartnerCode());
      }
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void copy(String storeId, String username, String partnerCode, String pickupPointCode, boolean isRetryAttempt,
      String processId, Map<String, List<String>> itemSkus, String linkedPartnerCode) {
    if (MapUtils.isNotEmpty(itemSkus)) {
      Integer itemEligibleForSync = syncStatusService
          .copy(storeId, username, partnerCode, pickupPointCode, isRetryAttempt, processId, itemSkus,
              linkedPartnerCode);
      LOGGER.info("item eligible for sync process {}", itemEligibleForSync);

      if (itemEligibleForSync > 0) {
        ProductItemSyncProcess productItemSyncProcess =
            ProductItemSyncProcess.builder().isUserNotified(false).processId(processId).build();
        productItemSyncProcess.setStoreId(storeId);
        syncProcessService.save(productItemSyncProcess);
        return;
      }
    }
    throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, ERR_CODE_NO_AVAILABLE_VALID_ITEMS);
  }

  private void sendProductItemSyncNotification(List<ProductItemSyncProcessSummary> statusCount,
      ProductItemSyncProcess productItemSyncProcess, String businessPartnerCode) {
    try {
      int totalFailedSku = 0;
      int totalSuccessSku = 0;
      for (ProductItemSyncProcessSummary statusSummary : statusCount) {
        if (ProductSyncStatus.SUCCESS.equals(statusSummary.getProductSyncStatus())) {
          totalSuccessSku = (int) statusSummary.getCount();
        } else if (ProductSyncStatus.FAIL.equals(statusSummary.getProductSyncStatus())) {
          totalFailedSku = (int) statusSummary.getCount();
        }
      }

      String message;
      if (totalFailedSku == 0) {
        message = String.format(PRODUCT_SUCCESSFULLY_ADDED_AS_FBB);
      } else {
        message = String.format(PRODUCT_PARTIALLY_ADDED_AS_FBB, totalSuccessSku, totalFailedSku + totalSuccessSku);
      }

      this.productNotificationService.sendProductSyncNotification(businessPartnerCode, message);

      productItemSyncProcess.setUserNotified(true);
      productItemSyncProcess.setMarkForDelete(true);
      this.syncProcessService.save(productItemSyncProcess);
      LOGGER
          .debug("notification message {} to business partner {} and notified flag : {}", message, businessPartnerCode,
              productItemSyncProcess);

    } catch (Exception e) {
      LOGGER.error("error sending notification to business partner {}", statusCount);
    }
  }

  @Override
  @Transactional(readOnly = false)
  public void resetProductItemSyncStatus(String storeId) {

    String syncRetryDuration = systemParameterService.getParameter("sync-retry-duration");

    LOGGER.debug("update IN_PROGRESS sync status to FAIL updated before {} hours", syncRetryDuration);
    syncStatusService.updateProductItemSyncStatus(storeId,
        StringUtils.isEmpty(syncRetryDuration) ? DEFAULT_SYNC_RETRY_DURATION : Long.parseLong(syncRetryDuration));
  }

  @Override
  public long countRejectedProductsByBusinessPartnerId(String storeId, String businessPartnerId) {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(businessPartnerId), BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY);
    LOGGER.info("Fetching the rejected product counts for merchant code : {}", businessPartnerId);
    boolean rejectedApiSwitch = Boolean.parseBoolean(
        productSystemParameterService.findByStoreIdAndVariable(storeId, SystemParameterConstants.REJECTED_API_SWITCH)
            .getValue());
    if (rejectedApiSwitch) {
      return Long.parseLong(
          productBusinessPartnerRepository.countRejectedProductsByBusinessPartnerIdFromPbp(storeId, businessPartnerId)
              .toString());
    } else {
      return Long.parseLong(
          productBusinessPartnerRepository.countRejectedProductsByBusinessPartnerId(storeId, businessPartnerId)
              .toString());
    }
  }

  @Override
  public long countSuspendedProductsByBusinessPartnerCode(String storeId, String requestId, String userName,
      String businessPartnerCode) throws Exception {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(businessPartnerCode),
        BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY);
    LOGGER.info("Fetching the suspended product counts for merchant code : {}", businessPartnerCode );
    SummaryFilterRequest summaryFilterRequest =
        SummaryFilterRequest.builder().businessPartnerCode(businessPartnerCode).build();
    Page<SuspensionItemResponse> response = this.productLevel3Service
        .getSuspendedItems(summaryFilterRequest, requestId, userName, storeId, PageRequest.of(0, 1));
    return response.getTotalElements();
  }

  @Override
  public Page<RejectedSkuProductCollection> findRejectedProductsByBusinessPartnerIdWithOrderByAndSortBy(String storeId,
      String businessPartnerId, Pageable pageable, String searchCriteria, String orderBy, String sortBy) {
    boolean rejectedApiSwitch = Boolean.parseBoolean(
        productSystemParameterService.findByStoreIdAndVariable(storeId, SystemParameterConstants.REJECTED_API_SWITCH)
            .getValue());
    if (rejectedApiSwitch) {
      List<RejectedSkuProductCollection> rejectedSkuProductCollections = new ArrayList<>();
      Page<ProductBusinessPartner> rejectedSkuProducts = this.productBusinessPartnerCustomRepository
          .findRejectedProductsByStoreIdAndBusinessPartnerIdAndProductName(storeId, businessPartnerId, searchCriteria,
              pageable, orderBy, sortBy);
      if (CollectionUtils.isNotEmpty(rejectedSkuProducts.getContent())) {
        List<String> productId = rejectedSkuProducts.getContent().stream().map(ProductBusinessPartner::getProductId)
            .collect(Collectors.toList());
        Map<String, String> productCodeIdMap = this.productLevel3Service.getProductIdProductCodeMap(storeId, productId);
        Map<String, String> productIdNotesMap = this.productWorkflowService.getRejectedNotesByProductIds(productId);
        for (ProductBusinessPartner productBusinessPartner : rejectedSkuProducts) {
          RejectedSkuProductCollection rejectedSkuProductCollection = new RejectedSkuProductCollection();
          BeanUtils.copyProperties(productBusinessPartner, rejectedSkuProductCollection);
          rejectedSkuProductCollection.setInitiator(productBusinessPartner.getCreatedBy());
          rejectedSkuProductCollection.setSubmitDate(productBusinessPartner.getCreatedDate());
          rejectedSkuProductCollection.setRejectedDate(productBusinessPartner.getUpdatedDate());
          rejectedSkuProductCollection.setProductCode(productCodeIdMap.get(productBusinessPartner.getProductId()));
          rejectedSkuProductCollection.setRejectedReason(productIdNotesMap.get(productBusinessPartner.getProductId()));
          rejectedSkuProductCollections.add(rejectedSkuProductCollection);
        }
      }
      return new PageImpl<>(rejectedSkuProductCollections, pageable, rejectedSkuProducts.getTotalElements());
    } else {
      if (StringUtils.isNotEmpty(searchCriteria)) {
        if (UPDATED_DATE.equalsIgnoreCase(orderBy) && ASC.equalsIgnoreCase(sortBy)) {
          return this.productBusinessPartnerRepository
              .findRejectedProductsByStoreIdAndBusinessPartnerIdAndProductNameAndUpdatedDateAsc(storeId,
                  businessPartnerId, searchCriteria, pageable);
        } else if (CREATED_DATE.equalsIgnoreCase(orderBy) && DESC.equalsIgnoreCase(sortBy)) {
          return this.productBusinessPartnerRepository
              .findRejectedProductsByStoreIdAndBusinessPartnerIdAndProductNameAndCreatedDateDesc(storeId,
                  businessPartnerId, searchCriteria, pageable);
        } else if (CREATED_DATE.equalsIgnoreCase(orderBy) && ASC.equalsIgnoreCase(sortBy)) {
          return this.productBusinessPartnerRepository
              .findRejectedProductsByStoreIdAndBusinessPartnerIdAndProductNameAndCreatedDateAsc(storeId,
                  businessPartnerId, searchCriteria, pageable);
        } else
          return this.productBusinessPartnerRepository
              .findRejectedProductsByBusinessPartnerIdAndProductName(storeId, businessPartnerId, searchCriteria,
                  pageable);
      }
      if (UPDATED_DATE.equalsIgnoreCase(orderBy) && ASC.equalsIgnoreCase(sortBy)) {
        return this.productBusinessPartnerRepository
            .findRejectedProductsByStoreIdAndBusinessPartnerIdAndUpdatedDateAsc(storeId, businessPartnerId, pageable);
      } else if (CREATED_DATE.equalsIgnoreCase(orderBy) && DESC.equalsIgnoreCase(sortBy)) {
        return this.productBusinessPartnerRepository
            .findRejectedProductsByStoreIdAndBusinessPartnerIdAndCreatedDateDesc(storeId, businessPartnerId, pageable);
      } else if (CREATED_DATE.equalsIgnoreCase(orderBy) && ASC.equalsIgnoreCase(sortBy)) {
        return this.productBusinessPartnerRepository
            .findRejectedProductsByStoreIdAndBusinessPartnerIdAndCreatedDateAsc(storeId, businessPartnerId, pageable);
      } else
        return this.productBusinessPartnerRepository
            .findRejectedProductsByBusinessPartnerId(storeId, businessPartnerId, pageable);
    }
  }

  @Override
  public boolean isProductMappedToMerchant(String storeId, String merchantCode) {
    ProductBusinessPartner productBusinessPartner =
        productBusinessPartnerRepository.findFirstByStoreIdAndBusinessPartnerId(storeId, merchantCode);
    return Objects.nonNull(productBusinessPartner);
  }

  @Override
  public List<ItemFlagDetails> getAllItemSkusViewConfigByProductId(String productId) {
    return productBusinessPartnerRepository.getAllItemSkusViewConfigByProductId(productId);
  }

  @Override
  public Integer getMinimumPrice(String storeId) {
    ProductSystemParameter minimumPrice =
        productSystemParameterService.findByStoreIdAndVariable(storeId, Constants.MINIMUM_PRICE);
    if (Objects.nonNull(minimumPrice)) {
      return Integer.parseInt(minimumPrice.getValue());
    }
    return null;
  }

  @Override
  public List<String> getProductSkusByProductCode(String productCode) {
    return productBusinessPartnerRepository.getGdnSkuByProductCode(productCode);
  }

  @Override
  public List<ProductSkuBusinessPartnerDTO> getGdnSkuListByProductCodes(
      List<String> productCodeList) {
    return productBusinessPartnerRepository.getGdnSkuListByProductCodes(productCodeList);
  }

  @Transactional(readOnly = false, rollbackFor = Exception.class)
  @Override
  public void updateProductMasterData(String productSku, String productName, String categoryCode, String categoryName,
      String sizeChartCode, String updatedBy, boolean sizeChartChanged, String brandName) {
    if (sizeChartChanged) {
      productBusinessPartnerRepository.updateProductMasterDataAndSizeChart(productSku, productName, categoryCode,
          categoryName, sizeChartCode, updatedBy, brandName);
    }
    else {
      productBusinessPartnerRepository.updateProductMasterData(productSku, productName, categoryCode, categoryName, brandName);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updateSizeChartDetailsAndBrandDetails(String productSku, String sizeChartCode,
      String updatedBy, boolean sizeChartChanged, boolean brandUpdated, String brand) {
    if (sizeChartChanged && brandUpdated) {
      productBusinessPartnerRepository.updateSizeChartDetailsAndBrand(productSku, sizeChartCode,
          brand, updatedBy);
    } else if (sizeChartChanged) {
      productBusinessPartnerRepository.updateSizeChartDetails(productSku, sizeChartCode, updatedBy);
    } else if (brandUpdated) {
      productBusinessPartnerRepository.updateBrand(productSku, brand, updatedBy);
    }
  }

  @Override
  public List<ProductBusinessPartner> findByProductSkuList(String storeId, List<String> productSkuList) {
    return this.productBusinessPartnerRepository.findByStoreIdAndGdnProductSkuIn(storeId, productSkuList);
  }

  @Override
  public ProductBusinessPartner updateProductBusinessPartnerState(ProductBusinessPartner productBusinessPartner, boolean isTakeDown,
      String categoryCode, ProductL3Response savedProductData) {
    if (isTakeDown) {
      productBusinessPartner.setState(ProductLevel3WipSummaryCriteria.IN_PROGRESS.name());
      productBusinessPartner.setActivated(false);
      productBusinessPartner.setMarkForDelete(false);
      if (Objects.nonNull(savedProductData)) {
        productBusinessPartner.setOnline(savedProductData.isOnline());
        productBusinessPartner.setOff2OnChannelActive(savedProductData.isOff2OnChannelActive());
        productBusinessPartner.setB2cActivated(savedProductData.isB2cActivated());
        productBusinessPartner.setB2bActivated(savedProductData.isB2bActivated());
        productBusinessPartner.setCncActivated(savedProductData.isCncActivated());
        productBusinessPartner.setSizeChartCode(savedProductData.getSizeChartCode());
      }

      Date submitDate = new Date();
      productBusinessPartner.setSubmittedDate(submitDate);
      productBusinessPartner
          .setExpectedActivationDate(calendarService.getExpectedActivationDateByCategoryCode(categoryCode, submitDate));
    } else {
      productBusinessPartner.setState(ProductLevel1State.ACTIVE);
      productBusinessPartner.setActivated(true);
      productBusinessPartner.setMarkForDelete(true);
    }
    return productBusinessPartner;
  }

  @Override
  public ProductBusinessPartnerAndItemViewConfigDto updateProductItemBusinessPartnerStateTakeDownTrue(List<ItemSummaryDetailResponse> itemSummaryDetailResponses,
      List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuRequests, ProductBusinessPartner productBusinessPartner)
      throws Exception {
    List<ProductItemBusinessPartner> productItemBusinessPartners = productBusinessPartner.getProductItemBusinessPartners();
    Map<String, Map<String, ProductItemBusinessPartner>> productItemBusinessPartnerMap = new HashMap<>();
    Map<String, ProductItemBusinessPartner> itemSkuProductItemBusinessPartnerMap = new HashMap<>();
    for (ProductItemBusinessPartner productItemBusinessPartner : productItemBusinessPartners) {
      if (!productItemBusinessPartner.isMarkForDelete()) {
        itemSkuProductItemBusinessPartnerMap.put(productItemBusinessPartner.getGdnProductItemSku(), productItemBusinessPartner);
        productItemBusinessPartner.setMarkForDelete(true);
      }
      if(StringUtils.isNotBlank(productItemBusinessPartner.getPickupPointId())){
        Map<String, ProductItemBusinessPartner> productItemBusinessPartnerPickUpPointMap =
            productItemBusinessPartnerMap.getOrDefault(productItemBusinessPartner.getGdnProductItemSku(), new HashMap<>());
        productItemBusinessPartnerPickUpPointMap.put(productItemBusinessPartner.getPickupPointId(), productItemBusinessPartner);
        productItemBusinessPartnerMap.put(productItemBusinessPartner.getGdnProductItemSku(), productItemBusinessPartnerPickUpPointMap);
      }
    }
    List<ProductItemBusinessPartner> productItemBusinessPartnerUpdates = new ArrayList<>();
    for (ItemSummaryDetailResponse itemSummaryDetailResponse : itemSummaryDetailResponses) {
      Map<String, ProductItemBusinessPartner> productItemBusinessPartnerPickUpPointMap =
          productItemBusinessPartnerMap.getOrDefault(itemSummaryDetailResponse.getItemSku(), new HashMap<>());
      ProductItemBusinessPartner productItemBusinessPartnerNew = new ProductItemBusinessPartner();
      productItemBusinessPartnerNew.setMinimumStock(itemSkuProductItemBusinessPartnerMap.get(
          itemSummaryDetailResponse.getItemSku()).getMinimumStock());
      productItemBusinessPartnerNew.setProductItemId(itemSkuProductItemBusinessPartnerMap.get(
          itemSummaryDetailResponse.getItemSku()).getProductItemId());
      productItemBusinessPartnerNew.setStock(itemSkuProductItemBusinessPartnerMap.get(
          itemSummaryDetailResponse.getItemSku()).getStock());
      productItemBusinessPartnerNew.setProductBusinessPartnerId(itemSkuProductItemBusinessPartnerMap.get(
          itemSummaryDetailResponse.getItemSku()).getProductBusinessPartnerId());
      productItemBusinessPartnerNew.setProductBusinessPartner(itemSkuProductItemBusinessPartnerMap.get(
          itemSummaryDetailResponse.getItemSku()).getProductBusinessPartner());
      productItemBusinessPartnerNew.setStoreId(itemSkuProductItemBusinessPartnerMap.get(
          itemSummaryDetailResponse.getItemSku()).getStoreId());
      ProductItemBusinessPartner productItemBusinessPartner = productItemBusinessPartnerPickUpPointMap.
          getOrDefault(itemSummaryDetailResponse.getPickupPointCode(), productItemBusinessPartnerNew);
      productItemBusinessPartner.setMarkForDelete(false);
      productItemBusinessPartner.setMerchantSku(itemSummaryDetailResponse.getMerchantSku());
      productItemBusinessPartner.setGdnProductItemSku(itemSummaryDetailResponse.getItemSku());
      productItemBusinessPartner.setDistribution(itemSummaryDetailResponse.isDistribution());
      productItemBusinessPartner.setPickupPointId(itemSummaryDetailResponse.getPickupPointCode());
      this.setDefaultItemViewConfigData(productItemBusinessPartner, itemSummaryDetailResponse);
      productItemBusinessPartner.setProductType(itemSummaryDetailResponse.getProductType().getCode());

      this.setCncItemViewConfigData(productItemBusinessPartner, itemSummaryDetailResponse,
          itemViewConfigAndItemSkuRequests);
      productItemBusinessPartner.setFbbActive(itemSummaryDetailResponse.isFbbActivated());
      productItemBusinessPartner.setB2bManaged(
          Optional.ofNullable(itemSummaryDetailResponse).map(ItemSummaryDetailResponse::getB2bFields)
              .map(B2bFieldsDTO::isManaged).orElse(false));
      productItemBusinessPartner.setB2bPrice(
          Optional.ofNullable(itemSummaryDetailResponse).map(ItemSummaryDetailResponse::getB2bFields)
              .map(B2bFieldsDTO::getBasePrice).orElse(null));
      if (CollectionUtils.isNotEmpty(itemSummaryDetailResponse.getItemViewConfigB2b())) {
        productItemBusinessPartner.setB2bBuyable(
            itemSummaryDetailResponse.getItemViewConfigB2b().iterator().next().isBuyable());
        productItemBusinessPartner.setB2bDiscoverable(
            itemSummaryDetailResponse.getItemViewConfigB2b().iterator().next().isDiscoverable());
        itemViewConfigAndItemSkuRequests.add(
            ConverterUtil.getItemViewConfigRequestB2b(productItemBusinessPartner.getGdnProductItemSku(),
                productItemBusinessPartner.getPickupPointId(), false, false));
      }
      PriceDTO priceDTO = new ArrayList<>(itemSummaryDetailResponse.getPrice()).get(0);
      productItemBusinessPartner.setPrice(priceDTO.getListPrice());
      if (useOriginalPriceEnabled && itemSummaryDetailResponse.getOriginalPrice() > 0) {
        productItemBusinessPartner.setSalePrice(itemSummaryDetailResponse.getOriginalPrice());
      } else {
        productItemBusinessPartner.setSalePrice(priceDTO.getOfferPrice());
      }
      itemViewConfigAndItemSkuRequests.add(ConverterUtil.getItemViewConfigRequest(
          productItemBusinessPartner.getGdnProductItemSku(), productItemBusinessPartner.getPickupPointId(), false, false));
      productItemBusinessPartner.setBundleRecipe(bundleRecipeService.convertBundleRecipeToString(
          itemSummaryDetailResponse.getBundleRecipe()));
      productItemBusinessPartnerUpdates.add(productItemBusinessPartner);
    }
    Set<ProductItemBusinessPartner> productItemBusinessPartnersSet = new HashSet<>(productItemBusinessPartners);
    productItemBusinessPartnersSet.addAll(productItemBusinessPartnerUpdates);
    productBusinessPartner.setProductItemBusinessPartners(productItemBusinessPartnersSet.stream().collect(Collectors.toList()));
    setStockInfoOnNeedCorrectionTakeDown(productBusinessPartner);
    return new ProductBusinessPartnerAndItemViewConfigDto(productBusinessPartner, itemViewConfigAndItemSkuRequests);
  }

  private void setCncItemViewConfigData(ProductItemBusinessPartner productItemBusinessPartner,
      ItemSummaryDetailResponse itemSummaryDetailResponse,
      List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuRequests) {
    if (cncForWarehouseFeatureSwitch) {
      ItemViewConfigDTO cncItemViewConfigDTO =
          ConverterUtil.getSingleItemViewConfigByChannel(Constants.CNC_CHANNEL,
              itemSummaryDetailResponse.getItemViewConfigs());
      productItemBusinessPartner.setCncActivated(cncItemViewConfigDTO.isBuyable());
      productItemBusinessPartner.setCncBuyable(cncItemViewConfigDTO.isBuyable());
      productItemBusinessPartner.setCncDiscoverable(cncItemViewConfigDTO.isDiscoverable());
      itemViewConfigAndItemSkuRequests.add(ConverterUtil.getItemViewConfigRequestCnc(
          productItemBusinessPartner.getGdnProductItemSku(),
          productItemBusinessPartner.getPickupPointId(), false, false));
    } else {
      productItemBusinessPartner.setCncActivated(itemSummaryDetailResponse.isCncActivated());
    }
  }

  private void setDefaultItemViewConfigData(ProductItemBusinessPartner productItemBusinessPartner,
      ItemSummaryDetailResponse itemSummaryDetailResponse) {
    ItemViewConfigDTO defaultItemViewConfigDTO =
        ConverterUtil.getSingleItemViewConfigByChannel(Constants.DEFAULT,
            itemSummaryDetailResponse.getItemViewConfigs());
    productItemBusinessPartner.setDisplay(defaultItemViewConfigDTO.isDiscoverable());
    productItemBusinessPartner.setBuyable(defaultItemViewConfigDTO.isBuyable());
  }

  private void setStockInfoOnNeedCorrectionTakeDown(ProductBusinessPartner productBusinessPartner) throws Exception {
    List<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTOList =
        productBusinessPartner.getProductItemBusinessPartners().stream().map(
            productItemBusinessPartner -> RequestHelper.inventoryDetailInfoRequestDTO(productItemBusinessPartner,
                productBusinessPartner.getBusinessPartnerId())).collect(Collectors.toList());
    List<ProductLevel3Inventory> productLevel3InventoryList =
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
            inventoryDetailInfoRequestDTOList);
    Map<String, ProductLevel3Inventory> productLevel3InventoryMap = productLevel3InventoryList.stream().collect(
        Collectors.toMap(
            productLevel3Inventory -> CommonUtils.getItemSkuAndPickupPointKey(productLevel3Inventory.getWebItemSku(),
                productLevel3Inventory.getWebPickupPointCode()), Function.identity(), (v1, v2) -> v2));
    for (ProductItemBusinessPartner productItemBusinessPartner : productBusinessPartner.getProductItemBusinessPartners()) {
      ProductLevel3Inventory productLevel3Inventory = productLevel3InventoryMap.get(
          CommonUtils.getItemSkuAndPickupPointKey(productItemBusinessPartner.getGdnProductItemSku(),
              productItemBusinessPartner.getPickupPointId()));
      if (Objects.nonNull(productLevel3Inventory)) {
        productItemBusinessPartner.setStock(productLevel3Inventory.getActualAvailableStock());
        productItemBusinessPartner.setMinimumStock(productLevel3Inventory.getWebMinAlert());
        productItemBusinessPartner.setPreOrderQuota(productLevel3Inventory.getInitialPreOrderQuota());
      }
    }
  }

  @Override
  public List<ItemViewConfigAndItemSkuRequest> updateProductItemBusinessPartnerStateTakeDownFalse(
      List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuRequests,
      ProductBusinessPartner productBusinessPartner, boolean businessPartnerActive) {
    for (ProductItemBusinessPartner productItemBusinessPartner : productBusinessPartner.getProductItemBusinessPartners()) {
      if (!productItemBusinessPartner.isMarkForDelete()) {
        // If business partner is inactive, need to active L3 and L4 in offline status
        itemViewConfigAndItemSkuRequests.add(ConverterUtil.getItemViewConfigRequest(
            productItemBusinessPartner.getGdnProductItemSku(), productItemBusinessPartner.getPickupPointId(),
            productItemBusinessPartner.isBuyable() && businessPartnerActive,
            productItemBusinessPartner.isDisplay() && businessPartnerActive));
        if (cncForWarehouseFeatureSwitch) {
          itemViewConfigAndItemSkuRequests.add(
              ConverterUtil.getItemViewConfigRequestCnc(productItemBusinessPartner.getGdnProductItemSku(),
                  productItemBusinessPartner.getPickupPointId(), productItemBusinessPartner.isCncBuyable(),
                  productItemBusinessPartner.isCncDiscoverable()));
        }
      } else {
        itemViewConfigAndItemSkuRequests.add(
            ConverterUtil.getItemViewConfigRequest(productItemBusinessPartner.getGdnProductItemSku(),
                productItemBusinessPartner.getPickupPointId(), false, false));
        if (cncForWarehouseFeatureSwitch) {
          itemViewConfigAndItemSkuRequests.add(
              ConverterUtil.getItemViewConfigRequestCnc(productItemBusinessPartner.getGdnProductItemSku(),
                  productItemBusinessPartner.getPickupPointId(), false, false));
        }
      }
    }
    return itemViewConfigAndItemSkuRequests;
  }

  @Override
  public List<InProgressProductResponse> findByStoreIdAndBusinessPartnerIdAndStateAndMarkForDeleteFalse(
    String storeId, String merchantCode, List<String> inProgressState) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(merchantCode), ErrorMessages.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(inProgressState),
      ErrorMessages.PRODUCT_STATE_INVALID);
    return Optional.ofNullable(
        this.productBusinessPartnerRepository.findByStoreIdAndAndBusinessPartnerIdAndStateInAndMarkForDeleteFalseOrderByUpdatedDate(
          storeId, merchantCode, inProgressState)).orElse(new ArrayList<>()).stream()
      .map(objects -> new InProgressProductResponse((String) objects[0], (String) objects[1]))
      .collect(Collectors.toList());
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updateNeedRevisionL3Details(String productSku, ProductLevel3 product) {
    ProductBusinessPartner productBusinessPartner =
        this.productBusinessPartnerRepository.findFirstByGdnProductSku(productSku);
    boolean specialAttributeUpdate = product.getAttributes().stream()
        .anyMatch(productLevel3Attribute -> Objects.nonNull(productLevel3Attribute.getId()));
    Map<String, ProductLevel3Attribute> attributeIdToProductAttributeMap = Optional.of(product.getAttributes().stream()
            .filter(productLevel3Attribute -> Objects.nonNull(productLevel3Attribute.getSkuValue())
                && productLevel3Attribute.getSkuValue())
            .collect(Collectors.toMap(ProductLevel3Attribute::getId, Function.identity(), (a, b) -> a)))
        .orElse(new HashMap<>());
    Map<String, ProductBusinessPartnerAttribute> productBusinessPartnerAttributeMap = Optional.of(
            productBusinessPartner.getProductBusinessPartnerAttributes().stream().collect(
                Collectors.toMap(ProductBusinessPartnerAttribute::getAttributeId, Function.identity(), (a, b) -> a)))
        .orElse(new HashMap<>());
    if (isProductNameOrFreeSampleOrOff2OnChannelActiveOrSpecialAttributesUpdatedOrAddedOrDeleted(product,
        productBusinessPartner, attributeIdToProductAttributeMap, productBusinessPartnerAttributeMap,
        specialAttributeUpdate) || product.isSizeChartChanged() || product.isBrandUpdated()
        || product.isCategoryUpdated()) {
      productBusinessPartner.setProductName(product.getProductName());
      productBusinessPartner.setFreeSample(product.isFreeSample());
      productBusinessPartner.setOff2OnChannelActive(product.isOff2OnChannelActive());
      productBusinessPartner.setSizeChartCode(product.isSizeChartChanged() ?
        product.getSizeChartCode() :
        productBusinessPartner.getSizeChartCode());
      productBusinessPartner.setCategoryCode(product.getCategoryCode());
      productBusinessPartner.setCategoryName(product.getCategoryName());
      productBusinessPartner.setBrand(product.getBrand());
      this.productBusinessPartnerRepository.save(productBusinessPartner);
    }

  }

  private boolean isProductNameOrFreeSampleOrOff2OnChannelActiveOrSpecialAttributesUpdatedOrAddedOrDeleted(
      ProductLevel3 product, ProductBusinessPartner productBusinessPartner,
      Map<String, ProductLevel3Attribute> attributeIdToProductAttributeMap,
      Map<String, ProductBusinessPartnerAttribute> productBusinessPartnerAttributeMap, boolean specialAttributeUpdate) {
    return ((!StringUtils.equals(product.getProductName(), productBusinessPartner.getProductName())) || (
        product.isFreeSample() != productBusinessPartner.isFreeSample()) || (product.isOff2OnChannelActive()
        != productBusinessPartner.isOff2OnChannelActive()) || specialAttributeUpdate && (
        isSpecialAttributeUpdatedOrDeleted(product, productBusinessPartner, attributeIdToProductAttributeMap)
            || isNewSpecialAttributeAdded(productBusinessPartner, productBusinessPartnerAttributeMap,
            attributeIdToProductAttributeMap)));
  }

  private boolean isSpecialAttributeUpdatedOrDeleted(ProductLevel3 productLevel3,
      ProductBusinessPartner productBusinessPartner,
      Map<String, ProductLevel3Attribute> attributeIdToProductAttributeMap) {
    boolean result = false;
    for (ProductBusinessPartnerAttribute productBusinessPartnerAttribute : productBusinessPartner.getProductBusinessPartnerAttributes()) {
      if (attributeIdToProductAttributeMap.containsKey(productBusinessPartnerAttribute.getAttributeId())) {
        ProductLevel3Attribute productLevel3Attribute =
            attributeIdToProductAttributeMap.get(productBusinessPartnerAttribute.getAttributeId());
        if (!productLevel3Attribute.getValues().contains(productBusinessPartnerAttribute.getValue())) {
          productBusinessPartnerAttribute.setValue(
              productLevel3Attribute.getValues().stream().findFirst().orElse(Constants.HYPHEN));
          result = true;
        }
      } else {
        productBusinessPartnerAttribute.setMarkForDelete(true);
        result = true;
      }
    }
    return result;
  }

  private boolean isNewSpecialAttributeAdded(ProductBusinessPartner productBusinessPartner,
      Map<String, ProductBusinessPartnerAttribute> productBusinessPartnerAttributeMap,
      Map<String, ProductLevel3Attribute> attributeIdToProductAttributeMap) {
    boolean result = false;
    for (Map.Entry<String, ProductLevel3Attribute> entry : attributeIdToProductAttributeMap.entrySet()) {
      if (!productBusinessPartnerAttributeMap.containsKey(entry.getKey())) {
        result = true;
        ProductBusinessPartnerAttribute productBusinessPartnerAttribute = new ProductBusinessPartnerAttribute();
        productBusinessPartnerAttribute.setAttributeId(entry.getKey());
        productBusinessPartnerAttribute.setValue(
            entry.getValue().getValues().stream().findFirst().orElse(Constants.HYPHEN));
        productBusinessPartnerAttribute.setProductBusinessPartner(productBusinessPartner);
        productBusinessPartner.getProductBusinessPartnerAttributes().add(productBusinessPartnerAttribute);
      }
    }
    return result;
  }

  @Override
  public List<ProductItemBusinessPartner> generateNewProductItemBusinessPartnerData(List<ProductVariantPriceStockAndImagesRequest>
      newlyAddedProductItemRequests, Integer productType) {
    List<ProductItemBusinessPartner> productItemBusinessPartners = new ArrayList<>();
    ProductBusinessPartner productBusinessPartner = null;
    if (CollectionUtils.isNotEmpty(newlyAddedProductItemRequests)) {
      for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : newlyAddedProductItemRequests) {
        for (ItemPickupPointRequest itemPickupPointRequest : productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints()) {
          if (Objects.isNull(productBusinessPartner)) {
            productBusinessPartner = productBusinessPartnerRepository.findFirstByGdnProductSku(
                productVariantPriceStockAndImagesRequest.getProductSku());
          }
          productItemBusinessPartners.add(createNewItemBusinessPartnerEntity(productBusinessPartner,
              productVariantPriceStockAndImagesRequest, itemPickupPointRequest, productType));
        }
      }
    }
    return productItemBusinessPartners;
  }

  private ProductItemBusinessPartner createNewItemBusinessPartnerEntity(ProductBusinessPartner productBusinessPartner,
      ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest, ItemPickupPointRequest itemPickupPointRequest,
      Integer productType) {
    ProductItemBusinessPartner productItemBusinessPartnerNew = new ProductItemBusinessPartner();
    productItemBusinessPartnerNew.setMinimumStock(itemPickupPointRequest.getMinimumStock());
    productItemBusinessPartnerNew.setStock(itemPickupPointRequest.getStock());
    productItemBusinessPartnerNew.setProductBusinessPartnerId(productBusinessPartner.getId());
    productItemBusinessPartnerNew.setProductBusinessPartner(productBusinessPartner);
    productItemBusinessPartnerNew.setStoreId(productBusinessPartner.getStoreId());
    productItemBusinessPartnerNew.setProductItemId(productVariantPriceStockAndImagesRequest.getProductItemId());
    productItemBusinessPartnerNew.setMerchantSku(productVariantPriceStockAndImagesRequest.getMerchantSku());
    productItemBusinessPartnerNew.setGdnProductItemSku(productVariantPriceStockAndImagesRequest.getItemSku());
    productItemBusinessPartnerNew.setPickupPointId(itemPickupPointRequest.getPickupPointId());
    productItemBusinessPartnerNew.setDisplay(itemPickupPointRequest.isDisplay());
    productItemBusinessPartnerNew.setBuyable(itemPickupPointRequest.isBuyable());
    productItemBusinessPartnerNew.setProductType(productType);
    productItemBusinessPartnerNew.setCncActivated(itemPickupPointRequest.isCncActive());
    productItemBusinessPartnerNew.setFbbActive(itemPickupPointRequest.getFbbActive());
    productItemBusinessPartnerNew.setPrice(itemPickupPointRequest.getPrice());
    productItemBusinessPartnerNew.setSalePrice(itemPickupPointRequest.getSalePrice());
    return productItemBusinessPartnerNew;
  }

  @Override
  public ProductBusinessPartner findFirstByStoreIdAndProductId(String storeId, String productId) {
    return productBusinessPartnerRepository.findFirstByStoreIdAndProductId(storeId, productId);
  }

  @Override
  public void deleteProductBusinessPartnerByStoreIdAndProductId(String storeId, String productId){
    productBusinessPartnerRepository.deleteByStoreIdAndProductId(storeId, productId);
  }

  @Override
  public ProductBusinessPartner findFirstByProductSku(String productSku){
    return productBusinessPartnerRepository.findFirstByGdnProductSku(productSku);
  }

  @Override
  public List<ProductBusinessPartner> findByProductCode(String productCode){
    return productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(productCode);
  }

  @Override
  public void updateBrand(List<ProductBusinessPartner> productBusinessPartners, String brandName) {
    for (ProductBusinessPartner productBusinessPartner : productBusinessPartners) {
      productBusinessPartner.setBrand(brandName);
      productBusinessPartnerRepository.save(productBusinessPartner);
    }
  }
}
