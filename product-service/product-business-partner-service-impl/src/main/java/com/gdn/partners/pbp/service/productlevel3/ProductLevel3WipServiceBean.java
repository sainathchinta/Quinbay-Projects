package com.gdn.partners.pbp.service.productlevel3;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gdn.mta.product.entity.StateCountDTO;
import com.gdn.mta.product.service.AutoApprovalService;
import com.gdn.partners.pbp.model.vo.CacheKeys;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.hibernate.Hibernate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ProductBusinessPartnerConfigRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.commons.constant.ProductLevel3SummaryCriteria;
import com.gdn.mta.product.commons.constant.ProductLevel3WipSummaryCriteria;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.entity.ProductImagePrediction;
import com.gdn.mta.product.entity.ProductImageQcProcessingResponse;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductHistoryRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.service.EmailNotificationService;
import com.gdn.mta.product.service.ProductBusinessPartnerService;
import com.gdn.mta.product.service.ProductImagePredictionService;
import com.gdn.mta.product.service.ProductImageQcProcessingResponseService;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.mta.product.valueobject.ForceReviewImageViolation;
import com.gdn.mta.product.valueobject.ProductLevel3WipDTO;
import com.gdn.partners.pbp.calendar.CalendarService;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3CountResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipSummaryRequest;
import com.gdn.partners.pbp.entity.productlevel3.CountProductLevel3Wip;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3AttributeWip;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3ItemWip;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Wip;
import com.gdn.partners.pbp.model.productlevel3wip.UpdateProductLevel3AttributeWip;
import com.gdn.partners.pbp.model.productlevel3wip.UpdateProductLevel3ItemWip;
import com.gdn.partners.pbp.model.productlevel3wip.UpdateProductLevel3Wip;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.repository.productlevel3.ProductLevel3WipRepository;
import com.gdn.partners.pbp.service.bpconfig.ProductBusinessPartnerConfigService;
import com.gdn.partners.pbp.service.notification.ProductNotificationService;
import com.gdn.partners.pbp.util.ProductLevel3WipUtil;
import com.gdn.partners.pbp.workflow.WorkflowProcessCode;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class ProductLevel3WipServiceBean implements ProductLevel3WipService {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductLevel3WipServiceBean.class);

  private static final String DATE_PATTERN = "dd/MM/yyyy";

  @Autowired
  private ProductLevel3WipRepository productLevel3WipRepository;

  @Autowired
  private ProductHistoryRepository productHistoryRepository;

  @Autowired
  private ProductLevel3WipUtil productLevel3WipUtil;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductCollectionRepository productCollectionRepository;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private CalendarService calendarService;

  @Autowired
  private ProductNotificationService productNotificationService;

  @Lazy
  @Autowired
  private ProductBusinessPartnerConfigService productBusinessPartnerConfigService;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Lazy
  @Autowired
  private ProductImageQcProcessingResponseService processingResponseService;

  @Autowired
  private ProductOutbound productOutbound;

  @Autowired
  @Lazy
  private ProductImagePredictionService productImagePredictionService;

  @Autowired
  private EmailNotificationService emailNotificationService;

  @Autowired
  @Lazy
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Value("${new.flow.level3.count.fetch.enabled}")
  private boolean newFlowForFetchingLevel3CountEnabled;

  @Override
  public Page<ProductLevel3WipDTO> findSummaryByFilterWithState(ProductLevel3WipSummaryRequest request,
      Pageable pageable) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    Page<ProductLevel3Wip> productLevel3Wips =
        this.productLevel3WipRepository.findSummaryByFilterWithState(storeId, request, pageable);
    return this.generateProductLevel3WipWithNotes(storeId, productLevel3Wips, pageable);
  }

  private Page<ProductLevel3WipDTO> generateProductLevel3WipWithNotes(String storeId,
      Page<ProductLevel3Wip> productLevel3Wips,
      Pageable pageable) {
    if (productLevel3Wips != null && !CollectionUtils.isEmpty(productLevel3Wips.getContent())) {
      List<ProductLevel3WipDTO> productLevel3WipDTOs =
          generateListOfProductLevel3ForInProcess(storeId, productLevel3Wips.getContent());
      return new PageImpl<>(productLevel3WipDTOs, pageable, productLevel3Wips.getTotalElements());
    }
    return new PageImpl<>(new ArrayList<>(), pageable, 0L);
  }

  private Map<String, List<ProductImageResponse>> toProductMainImageMap (List<String> productIds) {
    Map<String, List<ProductImageResponse>> imageMap = new HashMap<>();
    List<ProductImageResponse> imageResponses = productOutbound.filterProductImagesByProductIds(productIds, true);
    log.info("Images from PCB : {}", imageResponses);
    if (CollectionUtils.isNotEmpty(imageResponses)) {
      imageMap = imageResponses.stream().collect(Collectors.groupingBy(ProductImageResponse::getProductId));
    }
    return imageMap;
  }

  private List<ProductLevel3WipDTO> generateListOfProductLevel3WipDTO(String storeId,
      List<ProductLevel3Wip> productLevel3Wips) {
    List<ProductLevel3WipDTO> productLevel3WipDtos = new ArrayList<>();
    for (ProductLevel3Wip productLevel3Wip : productLevel3Wips) {
      ProductLevel3WipDTO productLevel3WipDTO = new ProductLevel3WipDTO();
      BeanUtils.copyProperties(productLevel3Wip, productLevel3WipDTO);
      if (ProductLevel3WipSummaryCriteria.NEED_CORRECTION.name().equals(productLevel3Wip.getState())) {
        ProductHistory history = this.productHistoryRepository
            .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(storeId,
                productLevel3Wip.getProductLevel1Id(), WorkflowProcessCode.RETURN_FOR_CORRECTION.getDesc());
        if (Objects.nonNull(history)) {
          productLevel3WipDTO.setNotes(history.getNotes());
        }
      }
      productLevel3WipDtos.add(productLevel3WipDTO);
    }
    return productLevel3WipDtos;
  }

  private Map<String, ProductImageQcProcessingResponse> getProductCodeImageQcResponseMap(String storeId,
      List<String> productIds, Map<String, String> productIdProductCode) {
    List<ProductCollection> productCollections =
        productCollectionRepository.findByStoreIdAndProductIds(storeId, productIds);
    Map<String, String> productCodeProductIdMap = productCollections.stream()
        .collect(Collectors.toMap(ProductCollection::getProductCode, ProductCollection::getProductId));
    productIdProductCode.putAll(productCollections.stream()
        .collect(Collectors.toMap(ProductCollection::getProductId, ProductCollection::getProductCode)));
    List<ProductImageQcProcessingResponse> processingResponses = processingResponseService
        .findByStoreIdAndProductCodeIn(storeId, new ArrayList<>(productCodeProductIdMap.keySet()));
    return processingResponses.stream().filter(productImageQcProcessingResponse -> Objects
        .nonNull(productCodeProductIdMap.get(productImageQcProcessingResponse.getProductCode()))).collect(Collectors
        .toMap(productImageQcProcessingResponse -> productCodeProductIdMap
            .get(productImageQcProcessingResponse.getProductCode()), Function.identity()));
  }

  private void setProductForceReviewAndImageViolations(String storeId,
      Map<String, ProductImageQcProcessingResponse> productImageQcProcessingResponseMap,
      ProductLevel3Wip productLevel3Wip, ProductLevel3WipDTO productLevel3WipDTO) {
    Map<String, ProductImagePrediction> productImagePredictionMap =
        getForceReviewImagePredictionMap(storeId, productImageQcProcessingResponseMap);
    ProductImageQcProcessingResponse productImageQcProcessingResponse =
        productImageQcProcessingResponseMap.get(productLevel3Wip.getProductLevel1Id());
    if (Objects.nonNull(productImageQcProcessingResponse)) {
      productLevel3WipDTO.setForceReview(productImageQcProcessingResponse.isForceReview());
      productLevel3WipDTO.setForceReviewImageViolations(new ArrayList<>());
      if (productImageQcProcessingResponse.isForceReview()) {
        HashSet<String> violations = getViolations(productImageQcProcessingResponse);
        Arrays.stream(violations.toArray()).map(imageViolation -> productImagePredictionMap.get(imageViolation))
            .filter(Objects::nonNull).forEach(
            productImagePrediction -> productLevel3WipDTO.getForceReviewImageViolations().add(
                new ForceReviewImageViolation(productImagePrediction.getDisplayName(),
                    productImagePrediction.getDisplayNameIn())));
      }
      if (StringUtils.isNotEmpty(productImageQcProcessingResponse.getPredictedBrand())) {
        productLevel3WipDTO.getForceReviewImageViolations()
            .add(new ForceReviewImageViolation(Constants.SUSPICIOUS_BRAND, Constants.SUSPICIOUS_BRAND_IN));
      }
      if (StringUtils.isNotEmpty(productImageQcProcessingResponse.getImageViolations())
        && Arrays.asList(
          productImageQcProcessingResponse.getImageViolations().split(Constants.COMMA))
        .contains(Constants.CATEGORY_MISMATCH)) {
        productLevel3WipDTO.getForceReviewImageViolations().add(
          new ForceReviewImageViolation(Constants.CATEGORY_MISMATCH,
            Constants.CATEGORY_MISMATCH_IN));
      }
    }
  }

  private HashSet<String> getViolations(ProductImageQcProcessingResponse productImageQcProcessingResponse) {
    HashSet<String> violations = new HashSet<>(Arrays.asList(
        (StringUtils.isNotEmpty(productImageQcProcessingResponse.getTextViolations()) ?
            productImageQcProcessingResponse.getTextViolations() :
            StringUtils.EMPTY).split(Constants.COMMA)));
    violations.addAll(Arrays.asList((StringUtils.isNotEmpty(productImageQcProcessingResponse.getImageViolations()) ?
        productImageQcProcessingResponse.getImageViolations() :
        StringUtils.EMPTY).split(Constants.COMMA)));
    return violations;
  }

  private Map<String, ProductImagePrediction> getForceReviewImagePredictionMap(String storeId,
      Map<String, ProductImageQcProcessingResponse> productImageQcProcessingResponseMap) {
    Map<String, ProductImagePrediction> productImagePredictionMap = new HashMap<>();

    //Call db only if any entry is force review true
    boolean forceReviewPresent = productImageQcProcessingResponseMap.values().stream()
        .anyMatch(ProductImageQcProcessingResponse::isForceReview);
    if (forceReviewPresent) {
      List<ProductImagePrediction> productImagePredictions =
          productImagePredictionService.findByStoreIdAndForceReviewTrue(storeId);
      if (CollectionUtils.isNotEmpty(productImagePredictions)) {
        productImagePredictions.forEach(productImagePrediction -> productImagePredictionMap
            .put(productImagePrediction.getDisplayName(), productImagePrediction));
      }
    }
    return productImagePredictionMap;
  }

  private List<ProductLevel3WipDTO> generateListOfProductLevel3ForInProcess(String storeId,
      List<ProductLevel3Wip> productLevel3Wips) {
    List<ProductLevel3WipDTO> productLevel3WipDtos = new ArrayList<>();
    List<String> productIds =
        productLevel3Wips.stream().map(ProductLevel3Wip::getProductLevel1Id).collect(Collectors.toList());
    Map<String, String> productIdProductCodeMap = new HashMap<>();
    Map<String, List<ProductImageResponse>> productMainImageMap = this.toProductMainImageMap(productIds);
    Map<String, ProductImageQcProcessingResponse> productImageQcProcessingResponseMap =
        this.getProductCodeImageQcResponseMap(storeId, productIds, productIdProductCodeMap);
    for (ProductLevel3Wip productLevel3Wip : productLevel3Wips) {
      ProductLevel3WipDTO productLevel3WipDTO = new ProductLevel3WipDTO();
      BeanUtils.copyProperties(productLevel3Wip, productLevel3WipDTO);
      if (ProductLevel3WipSummaryCriteria.NEED_CORRECTION.name().equals(productLevel3Wip.getState())) {
        ProductHistory history = this.productHistoryRepository
            .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(storeId,
                productLevel3Wip.getProductLevel1Id(), WorkflowProcessCode.RETURN_FOR_CORRECTION.getDesc());
        if (Objects.nonNull(history)) {
          productLevel3WipDTO.setNotes(history.getNotes());
        }
      }
      productLevel3WipDTO.setProductCode(productIdProductCodeMap.get(productLevel3Wip.getProductLevel1Id()));
      if (CollectionUtils.isNotEmpty(productMainImageMap.values())) {
        if (Objects.nonNull(productMainImageMap.get(productLevel3Wip.getProductLevel1Id()))) {
          ProductImageResponse mainImageResponse = productMainImageMap.get(productLevel3Wip.getProductLevel1Id()).stream()
            .filter(productImageResponse -> Boolean.FALSE.equals(productImageResponse.getOriginalImage())).findFirst().orElse(new ProductImageResponse());
          if (StringUtils.isNotBlank(mainImageResponse.getLocationPath())) {
            productLevel3WipDTO.setProductMainImage(mainImageResponse.getLocationPath());
          } else {
            ProductImageResponse mainImageResponseWithOriginalImage =
                productMainImageMap.get(productLevel3Wip.getProductLevel1Id()).stream()
                    .filter(productImageResponse -> Boolean.TRUE.equals(productImageResponse.getOriginalImage()))
                    .findFirst().orElse(new ProductImageResponse());
            productLevel3WipDTO.setProductMainImage(mainImageResponseWithOriginalImage.getLocationPath());
          }
          productLevel3WipDTO.setActiveImage(mainImageResponse.isActive());
        }
      }
      setProductForceReviewAndImageViolations(storeId, productImageQcProcessingResponseMap, productLevel3Wip,
          productLevel3WipDTO);
      productLevel3WipDtos.add(productLevel3WipDTO);
    }
    return productLevel3WipDtos;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void update(String productCode) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    ProductDetailResponse productData = this.productRepository.findProductDetailByProductCode(productCode);
    List<CategoryResponse> categories = new ArrayList<CategoryResponse>();
    for (ProductCategoryResponse productCategory : productData.getProductCategoryResponses()) {
      if (!productCategory.isMarkForDelete()) {
        categories.add(productCategory.getCategory());
      }
    }
    List<ProductLevel3Wip> productLevel3Wips =
        this.productLevel3WipRepository.findByStoreIdAndProductLevel1IdAndMarkForDeleteFalse(storeId,
            productData.getId());
    for (ProductLevel3Wip productLevel3Wip : productLevel3Wips) {
      productLevel3Wip.setProductName(productData.getName());
      productLevel3Wip.setCategoryName(categories.get(0).getName());
      productLevel3Wip.setBrandName(productData.getBrand());
      Hibernate.initialize(productLevel3Wip.getItems());
      Hibernate.initialize(productLevel3Wip.getAttributes());
    }
    this.productLevel3WipRepository.saveAll(productLevel3Wips);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void delete(String productCode) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    ProductCollection productData = this.productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    if (Objects.nonNull(productData)) {
      List<ProductLevel3Wip> productLevel3Wips = this.productLevel3WipRepository
          .findByStoreIdAndProductLevel1IdAndMarkForDeleteFalse(storeId,
              productData.getProductId());
      if (!productLevel3Wips.isEmpty()) {
        for (ProductLevel3Wip productLevel3Wip : productLevel3Wips) {
          Hibernate.initialize(productLevel3Wip.getItems());
          Hibernate.initialize(productLevel3Wip.getAttributes());
          productLevel3Wip.setMarkForDelete(true);
          productLevel3Wip.setState("DELETED");
          for (ProductLevel3ItemWip productLevel3ItemWip : productLevel3Wip.getItems()) {
            productLevel3ItemWip.setMarkForDelete(true);
          }
          for (ProductLevel3AttributeWip productLevel3AttributeWip : productLevel3Wip
              .getAttributes()) {
            productLevel3AttributeWip.setMarkForDelete(true);
          }
        }
        this.productLevel3WipRepository.saveAll(productLevel3Wips);
      }
    } else {
      LOGGER.warn("product already marked as deleted, productCode : {}", productCode);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void returnDraftForCorrection(String productCode, String notes) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    ProductCollection productCollection = this.productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    List<ProductLevel3Wip> productLevel3Wips =
        this.productLevel3WipRepository.findByStoreIdAndProductLevel1IdAndMarkForDeleteFalse(
            storeId, productCollection.getProductId());
    if (!CollectionUtils.isEmpty(productLevel3Wips)) {
      for (ProductLevel3Wip productLevel3Wip : productLevel3Wips) {
        productLevel3Wip.setState("NEED_CORRECTION");
        productLevel3Wip.setProductName(productCollection.getProductName());
        productLevel3Wip.setExpectedActivationDate(null);
        this.productNotificationService.sendProductReturnForCorrectionNotification(
            productLevel3Wip.getBusinessPartnerCode(), productLevel3Wip.getProductName(),
            productLevel3Wip.getProductSku(), notes);
      }
      this.productLevel3WipRepository.saveAll(productLevel3Wips);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void resubmit(ProductRequest productRequest, UpdateProductLevel3Wip updateProductLevel3Wip,
      Date submitDate) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    ProductLevel3Wip productLevel3Wip = this.productLevel3WipRepository
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, updateProductLevel3Wip.getProductSku());
    if (productLevel3Wip == null) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, "product not found");
    }
    Hibernate.initialize(productLevel3Wip.getItems());
    Hibernate.initialize(productLevel3Wip.getAttributes());
    productLevel3Wip.setProductName(updateProductLevel3Wip.getProductName());
    productLevel3Wip.setBrandName(updateProductLevel3Wip.getBrandName());
    Map<String, UpdateProductLevel3ItemWip> updateItemMap = new HashMap<>();
    for (UpdateProductLevel3ItemWip updateProductLevel3ItemWip : updateProductLevel3Wip.getItems()) {
      updateItemMap.put(updateProductLevel3ItemWip.getGdnSku(), updateProductLevel3ItemWip);
    }
    if (CollectionUtils.isNotEmpty(productLevel3Wip.getItems())) {
      for (ProductLevel3ItemWip itemWip : productLevel3Wip.getItems()) {
        UpdateProductLevel3ItemWip updateItem = updateItemMap.get(itemWip.getGdnSku());
        if (updateItem != null) {
          BeanUtils.copyProperties(updateItem, itemWip);
        }
      }
    }
    Map<String, UpdateProductLevel3AttributeWip> updateAttributeMap = new HashMap<>();
    for (UpdateProductLevel3AttributeWip attributeWip : updateProductLevel3Wip.getAttributes()) {
      updateAttributeMap.put(attributeWip.getAttributeId(), attributeWip);
    }
    if (CollectionUtils.isNotEmpty(productLevel3Wip.getAttributes())) {
      for (ProductLevel3AttributeWip attributeWip : productLevel3Wip.getAttributes()) {
        UpdateProductLevel3AttributeWip updateAttribute = updateAttributeMap.get(attributeWip.getAttributeId());
        if (updateAttribute != null) {
          BeanUtils.copyProperties(updateAttribute, attributeWip);
        }
      }
    }
    productLevel3Wip.setState("DRAFT");
    productLevel3Wip.setSubmittedDate(submitDate);
    productLevel3Wip.setExpectedActivationDate(getExpectedDateOfActivation(productRequest, submitDate));
    this.productLevel3WipRepository.save(productLevel3Wip);
  }

  /**
   * return expected activation date on the basis of category with the help of calendar service
   *
   * @param productRequest master product detail must not null
   * @param submitDate date of submit product must not null
   * @return expected activation date for product
   */
  private Date getExpectedDateOfActivation(ProductRequest productRequest, Date submitDate) {
    Date expectedDate = null;
    if (CollectionUtils.isNotEmpty(productRequest.getProductCategories())) {
      String categoryCode =
          productRequest.getProductCategories().get(0).getCategory().getCategoryCode();
      expectedDate =
          calendarService.getExpectedActivationDateByCategoryCode(categoryCode, submitDate);
    }
    return expectedDate;
  }

  private CountProductLevel3Wip generateCountProductLevel3Wip(List<Object[]> countProductLevel3WipRaws)
      throws Exception {
    CountProductLevel3Wip countProductLevel3Wip = new CountProductLevel3Wip();
    Long totalItems = 0L;
    countProductLevel3Wip.getTotalItemsByCriterias().put(ProductLevel3WipSummaryCriteria.FAILED, 0L);
    countProductLevel3Wip.getTotalItemsByCriterias().put(ProductLevel3WipSummaryCriteria.IN_PROGRESS, 0L);
    for (Object[] countProductLevel3WipRaw : countProductLevel3WipRaws) {
      boolean active = Boolean.parseBoolean(String.valueOf(countProductLevel3WipRaw[0]));
      long totalItemsByCriteria = Long.parseLong(String.valueOf(countProductLevel3WipRaw[1]));
      if (active) {
        countProductLevel3Wip.getTotalItemsByCriterias().put(ProductLevel3WipSummaryCriteria.FAILED,
            totalItemsByCriteria);
      } else {
        countProductLevel3Wip.getTotalItemsByCriterias().put(ProductLevel3WipSummaryCriteria.IN_PROGRESS,
            totalItemsByCriteria);
      }
      totalItems += totalItemsByCriteria;
    }
    countProductLevel3Wip.setTotalItems(totalItems);
    return countProductLevel3Wip;
  }

  private Map<String, List<List<String>>> generateDeletionNotification(ProductDetailResponse productData,
      List<ProductLevel3Wip> productLevel3Wips) throws Exception {
    Map<String, String> productLevel1ItemNames = new HashMap<String, String>();
    for (ProductItemResponse productItemData : productData.getProductItemResponses()) {
      productLevel1ItemNames.put(productItemData.getId(), productItemData.getGeneratedItemName());
    }
    Map<String, List<List<String>>> groups = new HashMap<String, List<List<String>>>();
    for (ProductLevel3Wip productLevel3Wip : productLevel3Wips) {
      if (groups.get(productLevel3Wip.getBusinessPartnerCode()) == null) {
        groups.put(productLevel3Wip.getBusinessPartnerCode(), new ArrayList<List<String>>());
      }
      Hibernate.initialize(productLevel3Wip.getItems());
      for (ProductLevel3ItemWip productLevel3ItemWip : productLevel3Wip.getItems()) {
        String productLevel1ItemName = productLevel1ItemNames.get(productLevel3ItemWip.getProductLevel1ItemId());
        String[] contents =
            new String[] {productLevel3Wip.getCreatedDate().toString(), productLevel3ItemWip.getGdnSku(),
                productLevel1ItemName};
        groups.get(productLevel3Wip.getBusinessPartnerCode()).add(Arrays.asList(contents));
      }
    }
    return groups;
  }

  @Override
  public CountProductLevel3Wip countSummaryWithState(String businessPartnerCode) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    return productLevel3WipUtil
        .generateCountProductLevel3WipWithState(this.productLevel3WipRepository
            .countByStoreIdAndBusinessPartnerCodeAndActiveAndStateAndMarkForDeleteFalse(storeId,
                businessPartnerCode));
  }

  @Override
  public ProductLevel3CountResponse countSummaryByFilterType(String businessPartnerCode, String storeId, String type)
      throws Exception {
    if (Constants.PRIMARY.equalsIgnoreCase(type))
      return getPrimaryProductLevel3CountResponse(businessPartnerCode, storeId);
    else {
      long rejectedCount =
          productBusinessPartnerService.countRejectedProductsByBusinessPartnerId(storeId, businessPartnerCode);
      ProductLevel3CountResponse productLevel3CountResponse = new ProductLevel3CountResponse();
      productLevel3CountResponse.getTotalItemsByCriterias().put(ProductLevel3SummaryCriteria.REJECTED, rejectedCount);
      return productLevel3CountResponse;
    }
  }

  private ProductLevel3CountResponse getPrimaryProductLevel3CountResponse(String businessPartnerCode,
    String storeId) {
    if(newFlowForFetchingLevel3CountEnabled){
      List<StateCountDTO> productLevel3CountResponses =
        productLevel3WipRepository.countByStoreIdAndBusinessPartnerCodeAndActivatedFalseAndMarkForDeleteFalse(
          storeId, businessPartnerCode);
      return productLevel3WipUtil.generateProductLevel3CountByState(productLevel3CountResponses);
    }
    else {
      List<Object[]> productLevel3CountResponses =
        productLevel3WipRepository.countByStoreIdAndBusinessPartnerCodeAndActivatedAndMarkForDeleteFalse(
          storeId, businessPartnerCode);
      return productLevel3WipUtil.generateCountProductLevel3ByState(productLevel3CountResponses);
    }
  }

  @Override
  public ProductLevel3Wip findByProductSku(String productSku, boolean isActive) throws Exception {
    ProductLevel3Wip productLevel3Wip = null;
    if (isActive) {
      productLevel3Wip = this.productLevel3WipRepository
          .findByStoreIdAndProductSkuAndMarkForDeleteTrue(GdnMandatoryRequestParameterUtil.getStoreId(), productSku);
    } else {
      productLevel3Wip = this.productLevel3WipRepository
          .findByStoreIdAndProductSkuAndMarkForDeleteFalse(GdnMandatoryRequestParameterUtil.getStoreId(), productSku);
    }
    if (Objects.isNull(productLevel3Wip)) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "Product Level 3 Wip Detail with productSku " + productSku + " is not found");
    }
    Hibernate.initialize(productLevel3Wip.getItems());
    Hibernate.initialize(productLevel3Wip.getAttributes());
    return productLevel3Wip;
  }

  @Override
  public Integer findCountByExceedingActivationDate(String storeId, String businessPartnerCode,
      Date curr) {
    return productLevel3WipRepository
        .findProductWipCountByExpectationActivationDateGreater(storeId, businessPartnerCode, curr);
  }

  @Override
  public List<ProductLevel3WipDTO> findProductWipByExpectationActivationDateGreater(String storeId,
      String businessPartnerCode, Date curr) {
    List<ProductLevel3Wip> productLevel3Wips = productLevel3WipRepository
        .findProductWipByExpectationActivationDateGreater(storeId, businessPartnerCode, curr);
    return generateListOfProductLevel3WipDTO(storeId, productLevel3Wips);
  }

  @Override
  public List<ProductLevel3Wip> findProductL3WipByStoreIdAndProductCode(String storeId,
      String productCode) throws Exception {
    ProductCollection productData = this.productCollectionRepository
        .findByStoreIdAndProductCode(storeId, productCode);
    if(Objects.isNull(productData)) {
      LOGGER.error("Not able to find product data for {} ", productCode);
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND , "Error while getting Product L1" +
          "Data for product code " + productCode);
    }
    return this.productLevel3WipRepository.
        findByStoreIdAndProductLevel1Id(storeId, productData.getProductId());
  }

  @Override
  @Transactional(readOnly = false)
  public void sendMailForEmailExceededActivation(String businessPartnerCode, String username) throws Exception {
    ProductBusinessPartnerConfigRequest request =
        new ProductBusinessPartnerConfigRequest(businessPartnerCode, Calendar.getInstance().getTime());
    ProfileResponse profileResponse = businessPartnerRepository.filterDetailByBusinessPartnerCode(businessPartnerCode);
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    log.info("Fetching list of products which exceeded activation for business partner : {}", businessPartnerCode);
    List<ProductLevel3WipDTO> productLevel3Wips =
        this.findProductWipByExpectationActivationDateGreater(storeId, businessPartnerCode,
            Calendar.getInstance().getTime());
    Date firstSubmissionDate =
        productLevel3Wips.stream().sorted(Comparator.comparing(ProductLevel3WipDTO::getSubmittedDate)).findFirst()
            .orElse(new ProductLevel3WipDTO()).getSubmittedDate();
    DateFormat dateFormat = new SimpleDateFormat(DATE_PATTERN);
    firstSubmissionDate = firstSubmissionDate == null ? Calendar.getInstance().getTime() : firstSubmissionDate;
    String submissionDateString = dateFormat.format(firstSubmissionDate);
    emailNotificationService.sendExceedActivationEmail(profileResponse, username, request, productLevel3Wips, submissionDateString);
    log.info("Saving product business partner config for request : {} and storeId : {}", request, storeId);
    productBusinessPartnerConfigService.save(storeId, username, request);
  }

  @Transactional(readOnly = false)
  @Override
  public ProductLevel3Wip getProductLevel3WipByProductSkuWithItemsInitialised(String storeId, String productSku) {
    ProductLevel3Wip productLevel3Wip = productLevel3WipRepository.findByStoreIdAndProductSku(storeId, productSku);
    Hibernate.initialize(productLevel3Wip.getItems());
    return productLevel3Wip;
  }

  @Override
  @Cacheable(cacheManager = Constants.PRODUCT_LEVEL_3_COUNT_CACHE, value = CacheKeys.COUNT_PRODUCT_LEVEL3_WIP_GROUP_BY_ACTIVE_AND_STATE,
    key = "#businessPartnerCode", unless = "#result == null")
  public CountProductLevel3Wip countSummaryWithStateCached(String businessPartnerCode)
    throws Exception {
    return countSummaryWithState(businessPartnerCode);
  }

  @Override
  @Cacheable(cacheManager = Constants.PRODUCT_LEVEL_3_COUNT_CACHE, value = CacheKeys.COUNT_PRODUCT_LEVEL3_WIP_FOR_INACTIVE_PRODUCTS_GROUP_BY_STATE,
    key = "#storeId + '-' + #businessPartnerCode + '-' + #type", unless = "#result == null")
  public ProductLevel3CountResponse countSummaryByFilterTypeCached(String businessPartnerCode,
    String storeId, String type) throws Exception {
    return countSummaryByFilterType(businessPartnerCode, storeId,
      type);
  }

}
