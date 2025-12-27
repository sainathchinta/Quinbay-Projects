package com.gdn.x.mta.distributiontask.service.impl;

import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gdn.x.message.model.constants.KafkaEventNames;
import com.gdn.x.message.mq.model.MessageEmailRequest;
import com.gdn.x.mta.distributiontask.dao.api.SolrVendorProductCollectionRepository;
import com.gdn.x.mta.distributiontask.dao.util.VendorProductSolrHelper;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrAddDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrBatchAddDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrDeleteDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductUpdateProductToSolrEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.VendorSearchAutoHealEventModel;
import com.gdn.x.mta.distributiontask.model.ConfigProperties;
import com.gdn.x.mta.distributiontask.model.ConfigPropertiesConstants;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.cache.CacheKeys;
import com.gdn.x.mta.distributiontask.model.dto.DistributionTaskMultipleFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.PrimaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.ProductAndReviewerDetailsDTO;
import com.gdn.x.mta.distributiontask.model.dto.ProductBusinessPartnerMapper;
import com.gdn.x.mta.distributiontask.model.dto.SummaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.enums.VendorProductSolrFieldNames;
import com.gdn.x.mta.distributiontask.model.solr.SolrConstants;
import com.gdn.x.mta.distributiontask.model.solr.VendorProductSolr;
import com.gdn.x.mta.distributiontask.model.type.BrandApprovalStatus;
import com.gdn.x.mta.distributiontask.model.type.ProductReviewType;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.response.ProductCodeResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.mta.distributiontask.service.api.ConfigPropertiesService;
import com.gdn.x.mta.distributiontask.service.api.ProductReviewerService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.SolrVendorCollectionService;
import com.gdn.x.mta.distributiontask.service.api.publisher.SolrReindexPublisherService;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import com.gdn.x.mta.distributiontask.service.impl.util.ConverterUtil;
import com.gdn.x.mta.distributiontask.service.impl.util.ProductUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.response.FacetField;
import org.apache.solr.client.solrj.response.IntervalFacet;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrException;
import org.apache.solr.common.SolrInputDocument;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
@Slf4j
public class SolrVendorCollectionServiceImpl implements SolrVendorCollectionService {

  private static final Pageable PAGEABLE = PageRequest.of(0, 100);
  private static final String DEFAULT_USERNAME = "system";
  private static final String CONTENT = "content";
  private static final String IMAGE = "image";
  private static final String EMAIL_MESSAGE = "resetting last updated date for delta reindex";

  private List<String> pendingStates = Arrays
      .asList(WorkflowState.IN_REVIEW.name(), WorkflowState.EXCEEDED_SLA.name(), WorkflowState.REJECTED.name(),
          WorkflowState.QC_REJECTED.name());
  private List<String> imagePendingStates =
      Arrays.asList(WorkflowState.CONTENT_NEED_CORRECTION.name());
  private List<String> contentPendingStates = new ArrayList<>(
      Arrays.asList(WorkflowState.IMAGE_NEED_CORRECTION.name()));


  private static final String CONTENT_APPROVER_ASSIGNEE_IMAGE_APPROVER_ASSIGNEE =
      "contentApproverAssignee,imageApproverAssignee";
  private static final String PRE_LIVE = "preLive";
  private static final String POST_LIVE = "postLive";
  private static final String EDITED = "edited";
  private static final String NEWLY_ADDED = "newlyAdded";
  private static final String REVISED = "revised";
  private static final long MILLI_SECONDS = 1000;
  private static final long SECONDS = 60;

  @Value("${solr.pdt.collection.batch.size}")
  private int pdtCollectionReindexBatchSize;

  @Value("${vendor.search.auto.heal}")
  private boolean vendorSearchAutoHeal;

  @Value("${distribution.list.search.auto.heal}")
  private boolean distributionListSearchAutoHeal;

  @Value("${reset.last.updated.delta.reindex.email.address}")
  private String resetLastUpdatedDeltaReindexEmailAddress;

  @Value("${reset.last.updated.delta.reindex.email.address.cc}")
  private String resetLastUpdatedDeltaReindexEmailAddressCc;

  @Value("${reset.last.updated.delta.reindex.hours.before.check}")
  private long resetLastUpdatedDeltaReindexHoursBeforeCheck;

  @Value("${reset.last.updated.delta.reindex.hours.before.set}")
  private long resetLastUpdatedDeltaReindexHoursBeforeSet;

  @Autowired
  private SolrVendorProductCollectionRepository solrVendorProductCollectionRepository;

  @Autowired
  @Lazy
  private ProductService productService;

  @Autowired
  private SolrReindexPublisherService solrReindexPublisherService;

  @Autowired
  private ConfigPropertiesService configPropertiesService;

  @Autowired
  private ProductReviewerService productReviewerService;

  @Autowired
  private ProductUtils productUtils;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Value("${replace.empty.review.type}")
  private boolean replaceEmptyReviewType;

  @Override
  @Async
  public void fullReindexPDTProductSolr(String storeId) {
    try {
      solrVendorProductCollectionRepository.deleteDocumentFromSolr(new ArrayList<>(), true);
      Page<Product> products;
      Pageable pageable = PAGEABLE;
      List<ProductAndReviewerDetailsDTO> productAndReviewerDetailsDTOList = new ArrayList<>();
      Map<String, ProductReviewer> productReviewerMap = new HashMap<>();
      do {
        products = productService
            .findByStoreIdAndMarkForDeleteFalseOrderByCreatedDateDesc(storeId, pageable);
        Map<String, ProductReviewer> productReviewerByProductCodes = Optional.ofNullable(
          productReviewerService.findProductReviewerByProductCodesAndMarkForDeleteFalse(storeId,
            products.getContent().stream().map(Product::getProductCode)
              .collect(Collectors.toList()))).orElse(new ArrayList<>()).stream().collect(
          Collectors.toMap(ProductReviewer::getProductCode, Function.identity(), (a, b) -> b));
        for (Product product : products) {
          if (productAndReviewerDetailsDTOList.size() > pdtCollectionReindexBatchSize) {
            publishSolrAddPDTProductBatchEvent(productAndReviewerDetailsDTOList);
            productAndReviewerDetailsDTOList.clear();
          }
          productAndReviewerDetailsDTOList.add(new ProductAndReviewerDetailsDTO(product,
            productReviewerByProductCodes.get(product.getProductCode())));
        }
        pageable = products.nextPageable();
      } while (products.hasNext());
      if (CollectionUtils.isNotEmpty(productAndReviewerDetailsDTOList)) {
        publishSolrAddPDTProductBatchEvent(productAndReviewerDetailsDTOList);
      }
      updateApplicationConfigProperties(storeId,
          ConfigPropertiesConstants.SOLR_PDT_COLLECTION_LAST_DELTA_RUN_TIME);
    } catch (Exception e) {
      log.error("Error while running full reindex. ", e);
    }
  }

  @Override
  @Async
  public void deltaReindexPDTProductSolr(String storeId, String productCode) {
    try {
      if (StringUtils.isNotBlank(productCode)) {
        deltaReindexProductCode(productCode);
      } else {
        deltaReindexByDateRange(storeId);
      }
    } catch (Exception e) {
      log.error("Error while running delta reindex. productCode:{} ", productCode, e);
    }
  }

  private void deltaReindexByDateRange(String storeId) throws Exception {
    Pageable pageable = PageRequest.of(0, 100, Sort.by(Sort.Order.asc(Constants.PRODUCT_CODE),
        Sort.Order.asc(Constants.UPDATED_DATE)));
    ConfigProperties configProperties = configPropertiesService
        .findByStoreIdAndPropertyNameAndMarkForDeleteFalse(storeId,
            ConfigPropertiesConstants.SOLR_PDT_COLLECTION_LAST_DELTA_RUN_TIME);
    Date lastUpdatedDate = new Date(Long.parseLong(configProperties.getValue()));
    Date currentTime = new Date();
    long timeDifferenceMillis = currentTime.getTime() - lastUpdatedDate.getTime();
    long timeDifferenceHours = timeDifferenceMillis / (MILLI_SECONDS * SECONDS * SECONDS);
    if (timeDifferenceHours > resetLastUpdatedDeltaReindexHoursBeforeCheck) {
      long twoHoursAgoMillis = currentTime.getTime() -
          (resetLastUpdatedDeltaReindexHoursBeforeSet * SECONDS * SECONDS * MILLI_SECONDS);
      lastUpdatedDate.setTime(twoHoursAgoMillis);
      sendEmailForResettingLastUpdatedTime();
    }
    Date currentDate = new Date();
    Page<Product> products;
    do {
      products = productService.
              findByStoreIdAndUpdatedDateBetweenOrderByProductCodeAscAndUpdatedDateAsc(storeId, lastUpdatedDate,
                      currentDate, pageable);
      Map<String, ProductReviewer> productReviewerByProductCodes = Optional.ofNullable(
        productReviewerService.findProductReviewerByProductCodes(storeId,
          products.getContent().stream().map(Product::getProductCode)
            .collect(Collectors.toList()))).orElse(new ArrayList<>()).stream().collect(
        Collectors.toMap(ProductReviewer::getProductCode, Function.identity(), (a, b) -> b));
      publishSolrAddPDTProductBatchEvent(
        products.getContent().stream().filter(product -> !product.isMarkForDelete()).map(
            product -> new ProductAndReviewerDetailsDTO(product,
              productReviewerByProductCodes.get(product.getProductCode())))
          .collect(Collectors.toList()));
      publishSolrDeletePDTProductBatchEvent(
          products.getContent().stream().filter(product -> product.isMarkForDelete()).map(Product::getProductCode)
              .collect(Collectors.toList()));
      pageable = products.nextPageable();
    } while (products.hasNext());
    configProperties.setValue(String.valueOf(currentDate.getTime()));
    configProperties.setUpdatedBy(DEFAULT_USERNAME);
    configProperties.setUpdatedDate(new Date());
    configProperties.setMarkForDelete(false);
    configPropertiesService.save(configProperties);
  }

  private void sendEmailForResettingLastUpdatedTime() {
    MessageEmailRequest email = ConverterUtil.getResetDeltaReindexMessageEmailRequest(
            resetLastUpdatedDeltaReindexEmailAddress, resetLastUpdatedDeltaReindexEmailAddressCc,
            EMAIL_MESSAGE);
    log.info("Sending event {} for resetting last updated time for delta reindex with payload {}",
        KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT, email);
    try {
      kafkaProducer.send(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT, email.getMessageIdentifierValue(), email);
    } catch (Exception e) {
      log.error("Product Retry Summary Email failed to send, {} ", email, e);
    }
  }

  private void deltaReindexProductCode(String productCode) throws Exception {
    Product product = productService.getProductByCode(productCode);
    ProductReviewer productReviewer =
      productReviewerService.findProductReviewerByProductCode(productCode);
    if (Objects.nonNull(product) && !product.isMarkForDelete()) {
      publishSolrAddPDTProductBatchEvent(
        Arrays.asList(new ProductAndReviewerDetailsDTO(product, productReviewer)));
    } else {
      solrVendorProductCollectionRepository.deleteDocumentFromSolr(Arrays.asList(productCode), false);
    }
  }

  private void updateApplicationConfigProperties(String storeId,
      String solrPdtCollectionLastDeltaRunTime) {
    ConfigProperties configProperties = configPropertiesService
        .findByStoreIdAndPropertyNameAndMarkForDeleteFalse(storeId,
            solrPdtCollectionLastDeltaRunTime);
    if (Objects.isNull(configProperties)) {
      configProperties = ConfigProperties.builder().propertyName(solrPdtCollectionLastDeltaRunTime)
          .description(
              ConfigPropertiesConstants.SOLR_PDT_COLLECTION_LAST_DELTA_RUN_TIME_DESCRIPTION)
          .build();
      configProperties.setStoreId(storeId);
      configProperties.setCreatedBy(DEFAULT_USERNAME);
      configProperties.setCreatedDate(new Date());
    }
    configProperties.setValue(String.valueOf(new Date().getTime()));
    configProperties.setUpdatedBy(DEFAULT_USERNAME);
    configProperties.setUpdatedDate(new Date());
    configProperties.setMarkForDelete(false);
    configPropertiesService.save(configProperties);
  }

  @Override
  public void publishSolrAddPDTProductBatchEvent(
    List<ProductAndReviewerDetailsDTO> productAndReviewerDetailsDTOList) throws Exception {
    handleNullReviewType(productAndReviewerDetailsDTOList);
    List<PDTProductSolrAddDomainEventModel> pdtProductSolrAddDomainEventModelList =
        VendorProductSolrHelper.toPDTProductSolrAddDomainEventModelList(productAndReviewerDetailsDTOList);
    PDTProductSolrBatchAddDomainEventModel pdtProductSolrBatchAddDomainEventModel =
        PDTProductSolrBatchAddDomainEventModel.builder()
            .pdtProductSolrAddDomainEventModelList(pdtProductSolrAddDomainEventModelList).build();
    solrReindexPublisherService.publishPDTProductSolrBatchAddDomainEventModelForReindex(
        pdtProductSolrBatchAddDomainEventModel);
  }

  private void handleNullReviewType(
    List<ProductAndReviewerDetailsDTO> productAndReviewerDetailsDTOList) {
    if (CollectionUtils.isNotEmpty(productAndReviewerDetailsDTOList) && replaceEmptyReviewType) {
      productAndReviewerDetailsDTOList.stream().filter(Objects::nonNull)
        .map(ProductAndReviewerDetailsDTO::getProduct).filter(Objects::nonNull)
        .forEach(this::setDefaultReviewTypeIfNull);
    }
  }

  private void setDefaultReviewTypeIfNull(Product product) {
    if (Objects.isNull(product.getReviewType())) {
      product.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    }
  }


  @Override
  public Page<ProductCodeResponse> getProductResponseForAutoAssignment(String storeId,
    SummaryFilterDTO summaryFilterDTO, Pageable pageable) throws SolrServerException, IOException {
    Page<VendorProductSolr> filteredAndBoostedProductsFromSolr =
      this.solrVendorProductCollectionRepository.getFilteredAndBoostedProductsFromSolr(storeId,
        summaryFilterDTO, pageable);
    return new PageImpl<>(filteredAndBoostedProductsFromSolr.getContent().parallelStream()
      .map(solrDoc -> ProductCodeResponse.builder().productCodes(solrDoc.getProductCode()).build())
      .collect(Collectors.toList()), pageable,
      filteredAndBoostedProductsFromSolr.getTotalElements());
  }

  private void publishSolrDeletePDTProductBatchEvent(List<String> productCodes) throws Exception {
    PDTProductSolrDeleteDomainEventModel pdtProductSolrBatchAddDomainEventModel =
        PDTProductSolrDeleteDomainEventModel.builder().productCodes(productCodes).build();
    solrReindexPublisherService
        .publishPDTProductSolrBatchDeleteDomainEventModelForReindex(pdtProductSolrBatchAddDomainEventModel);
  }

  @Override
  public Page<ProductAndReviewerDetailsDTO> getVendorProductsList(String storeId,
    SummaryFilterDTO summaryFilterDTO, List<WorkflowState> states, Pageable pageable)
    throws Exception {
    Page<VendorProductSolr> vendorProductSolrPage = this.solrVendorProductCollectionRepository
        .getVendorProductsBySolrQuery(storeId, summaryFilterDTO, states, pageable);
    vendorSearchAutoHealIfEligible(storeId, summaryFilterDTO, vendorProductSolrPage);
    return new PageImpl<>(vendorProductSolrPage.getContent().stream()
      .map(VendorProductSolrHelper::toProductAndReviewerDetailsDTO).collect(Collectors.toList()),
      pageable, vendorProductSolrPage.getTotalElements());
  }

  private void vendorSearchAutoHealIfEligible(String storeId, SummaryFilterDTO summaryFilterDTO,
      Page<VendorProductSolr> vendorProductSolrPage) {
    if (vendorSearchAutoHeal && CollectionUtils.isEmpty(vendorProductSolrPage.getContent()) && StringUtils.isNotEmpty(
        summaryFilterDTO.getKeyword()) && productUtils.isProductCode(summaryFilterDTO.getKeyword())
        && StringUtils.isEmpty(productService.checkIfVendorAutoHealKeyExists(summaryFilterDTO.getKeyword()))) {
      log.info("Vendor auto heal for search keyword : {} ", summaryFilterDTO.getKeyword());
      productService.cacheVendorAutoHealKey(summaryFilterDTO.getKeyword());
      VendorSearchAutoHealEventModel vendorSearchAutoHealEventModel = new VendorSearchAutoHealEventModel();
      vendorSearchAutoHealEventModel.setStoreId(storeId);
      vendorSearchAutoHealEventModel.setProductCode(summaryFilterDTO.getKeyword());
      kafkaProducer.send(DomainEventName.VENDOR_SEARCH_AUTO_HEAL_EVENT_NAME, summaryFilterDTO.getKeyword(),
          vendorSearchAutoHealEventModel);
    }
  }

  private void vendorSearchAutoHealDistributionList(String storeId,
      DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO,
      Page<VendorProductSolr> vendorProductSolrPage) {
    if (distributionListSearchAutoHeal && CollectionUtils.isEmpty(
        vendorProductSolrPage.getContent()) && StringUtils.isNotEmpty(
        distributionTaskMultipleFilterDTO.getProductName()) && productUtils.isProductCode(
        distributionTaskMultipleFilterDTO.getProductName()) && StringUtils.isEmpty(
        productService.checkIfVendorAutoHealKeyExists(
            distributionTaskMultipleFilterDTO.getProductName()))) {
      log.info("Distribution list auto heal for search keyword : {} ",
          distributionTaskMultipleFilterDTO.getProductName());
      productService.cacheVendorAutoHealKey(distributionTaskMultipleFilterDTO.getProductName());
      VendorSearchAutoHealEventModel vendorSearchAutoHealEventModel =
          new VendorSearchAutoHealEventModel();
      vendorSearchAutoHealEventModel.setStoreId(storeId);
      vendorSearchAutoHealEventModel.setProductCode(
          distributionTaskMultipleFilterDTO.getProductName());
      kafkaProducer.send(DomainEventName.VENDOR_SEARCH_AUTO_HEAL_EVENT_NAME,
          distributionTaskMultipleFilterDTO.getProductName(), vendorSearchAutoHealEventModel);
    }
  }

  @Override
  public Page<Product> getAllProductDetailsWithMultipleFilterSolr(
      DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO, Pageable pageable,
      String storeId) throws Exception {
    Page<VendorProductSolr> vendorProductSolrPage =
        this.solrVendorProductCollectionRepository.getAllProductDetailsWithMultipleFilterForDistributionListBySolrQuery(
            distributionTaskMultipleFilterDTO, pageable);
    vendorSearchAutoHealDistributionList(storeId, distributionTaskMultipleFilterDTO,
        vendorProductSolrPage);
    return new PageImpl<>(vendorProductSolrPage.getContent().stream()
        .map(VendorProductSolrHelper::toProduct)
        .collect(Collectors.toList()), pageable, vendorProductSolrPage.getTotalElements());
  }

  @Override
  public Page<ProductAndReviewerDetailsDTO> getFilterProductSummary(String storeId, ProductListRequest productListRequest,
      List<WorkflowState> states, Pageable pageable) throws Exception {
    Page<VendorProductSolr> vendorProductSolrPage = this.solrVendorProductCollectionRepository
        .getFilterProductSummaryBySolrQuery(storeId, productListRequest, states, pageable);
    return new PageImpl<>(vendorProductSolrPage.getContent().stream()
      .map(VendorProductSolrHelper::toProductAndReviewerDetailsDTO).collect(Collectors.toList()),
      pageable, vendorProductSolrPage.getTotalElements());
  }


  @Override
  @Cacheable(value = CacheKeys.VENDOR_PRODUCT_COUNT,
      key = "#storeId +'_'+ #vendorCode +'_'+ #edited +'_'+ #postLive +'_'+ #revised", unless = "#result == null")
  public Map<String, Object> getFilterCounts(String storeId, String vendorCode, Boolean edited,
    Boolean postLive, Boolean revised) throws IOException, SolrServerException {
    QueryResponse filterCounts = solrVendorProductCollectionRepository
        .getFilterCounts(storeId, vendorCode, edited, postLive, revised);
    Map<String, Object> countResponse = new HashMap<>();
    if (Objects.nonNull(filterCounts)) {
      com.gdn.x.mta.distributiontask.dao.util.ConverterUtil.processIntervalFacets(countResponse,
        filterCounts);
      processFacetFields(countResponse, filterCounts);
    }
    return countResponse;
  }

  @Override
  public Map<String, Object> getFinalQcCounts(String storeId) throws IOException, SolrServerException {
    QueryResponse finalQCCount = solrVendorProductCollectionRepository.getFinalQCCounts(storeId);
    Map<String, Object> countResponse = new LinkedHashMap<>();
    if (Objects.nonNull(finalQCCount)) {
      processDistributionListIntervalFacets(countResponse, finalQCCount);
    }
    return countResponse;
  }

  @Override
  public void getDistributionListCounts(String storeId, Map<String, Object> countMap)
      throws IOException, SolrServerException {
    QueryResponse finalQCCount = solrVendorProductCollectionRepository.getDistributionListCounts(storeId);
    Map<String, Object> intervalCount = new LinkedHashMap<>();
    Map<String, Object> statusCount = new LinkedHashMap<>();
    if (Objects.nonNull(finalQCCount)) {
      processDistributionListIntervalFacets(intervalCount, finalQCCount);
      processStateFacetFields(statusCount, finalQCCount);
    }
    countMap.put(Constants.DAYS_ADDED, intervalCount);
    countMap.put(Constants.STATUS, statusCount);
  }

  @Override
  public List<ProductBusinessPartnerMapperResponse> getBusinessPartnerList(String storeId,
      PrimaryFilterDTO primaryFilterDTO, List<WorkflowState> states, int page, int size)
      throws IOException, SolrServerException {
    List<ProductBusinessPartnerMapperResponse> businessPartnerMapperResponses = new ArrayList<>();
    QueryResponse businessPartnerList =
        solrVendorProductCollectionRepository.getBusinessPartnerList(storeId, primaryFilterDTO, states, page, size);
    if (CollectionUtils.isNotEmpty(businessPartnerList.getResults())) {
      return businessPartnerList.getResults().stream().map(this::toProductBusinessPartnerMapperResponse)
          .collect(Collectors.toList());
    }
    return businessPartnerMapperResponses;
  }

  @Override
  public void addProductToSolr(Product product, ProductReviewer productReviewer,
    ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse) {
    VendorProductSolr vendorProductSolr = VendorProductSolrHelper.toVendorProductSolr(product,
      productReviewer, imageQcProcessedAndBrandResponse);
    solrVendorProductCollectionRepository
        .addDocumentToSolrWithCategoryHierarchy(Arrays.asList(vendorProductSolr));
  }

  @Override
  public void deleteProductFromSolr(String productCode) throws Exception {
    PDTProductSolrDeleteDomainEventModel pdtProductSolrDeleteDomainEventModel =
        PDTProductSolrDeleteDomainEventModel.builder().productCodes(Arrays.asList(productCode)).build();
    solrReindexPublisherService.publishPDTProductSolrBatchDeleteDomainEventModelForReindex(
        pdtProductSolrDeleteDomainEventModel);
  }

  @Override
  @Async
  public void updateSolrOnApprovalOrSave(Product product, ProductReviewer productReviewer, String type) {
    try {
      if (CONTENT.equalsIgnoreCase(type)) {
        boolean categoryChanged = false;
        SolrDocument solrDocument = solrVendorProductCollectionRepository.getSolrDocument(product.getProductCode());
        List<String> categoryCodes =
            (List<String>) solrDocument.getFieldValue(VendorProductSolrFieldNames.CATEGORY_CODES);
        if (!categoryCodes.contains(product.getCategoryCode())) {
          categoryChanged = true;
        }
        solrVendorProductCollectionRepository.updateSolrOnContentApprovalOrSave(product, productReviewer, categoryChanged);
      } else if (IMAGE.equalsIgnoreCase(type)) {
        solrVendorProductCollectionRepository.updateSolrOnImageApproval(product, productReviewer);
      }
    } catch (IOException | SolrServerException | SolrException e) {
      log.error("Exception caught while updating solr on approval/save", e);
    }
  }

  private ProductBusinessPartnerMapperResponse toProductBusinessPartnerMapperResponse(
      SolrDocument solrDocument) {
    ProductBusinessPartnerMapperResponse mapperResponse = new ProductBusinessPartnerMapperResponse();
    mapperResponse.setBusinessPartnerCode(
        String.valueOf(solrDocument.getFieldValue(VendorProductSolrFieldNames.BUSINESS_PARTNER_CODE)));
    mapperResponse.setBusinessPartnerName(
        String.valueOf(solrDocument.getFieldValue(VendorProductSolrFieldNames.BUSINESS_PARTNER_NAME)));
    return mapperResponse;
  }

  private void processDistributionListIntervalFacets(Map<String, Object> countResponse, QueryResponse response) {
    List<IntervalFacet> intervalFacets = response.getIntervalFacets();
    Map<String, Integer> facetCountMap = intervalFacets.get(0).getIntervals().stream()
        .collect(Collectors.toMap(IntervalFacet.Count::getKey, IntervalFacet.Count::getCount));
    countResponse.put(SolrConstants.TODAY, facetCountMap.get(SolrConstants.TODAY_FACET_INTERVAL));
    countResponse.put(SolrConstants.YESTERDAY, facetCountMap.get(SolrConstants.YESTERDAY_FACET_INTERVAL));
    countResponse.put(SolrConstants.TWO_UNTIL_THREE_DAYS_AGO,
        facetCountMap.get(SolrConstants.TWO_TO_THREE_DAYS_AGO_FACET_INTERVAL));
    countResponse.put(SolrConstants.MORE_THAN_3_DAYS, facetCountMap.get(SolrConstants.THREE_DAYS_AGO_FACET_INTERVAL));
  }

  private void processFacetFields(Map<String, Object> countResponse, QueryResponse response) {
    for (FacetField facetField : response.getFacetFields()) {
      switch (facetField.getName()) {
        case VendorProductSolrFieldNames.BRAND_APPROVAL_STATUS:
          countResponse.put(SolrConstants.BRAND_NOT_APPROVED, facetField.getValues().stream()
            .filter(
              count -> !count.getName().equalsIgnoreCase(BrandApprovalStatus.APPROVED.getValue()))
            .map(FacetField.Count::getCount).mapToInt(Long::intValue).sum());
          break;
        case VendorProductSolrFieldNames.ASSIGNED: {
          for (FacetField.Count count : facetField.getValues()) {
            if (Boolean.parseBoolean(count.getName())) {
              countResponse.put(SolrConstants.ASSIGNED, count.getCount());
            } else {
              countResponse.put(SolrConstants.UNASSIGNED, count.getCount());
            }
          }
          break;
        }
        case VendorProductSolrFieldNames.REVIEW_TYPE: {
          for (FacetField.Count count : facetField.getValues()) {
            if (count.getName().equals(String.valueOf(ReviewType.CONTENT.getValue()))) {
              countResponse.put(SolrConstants.CONTENT_PENDING, count.getCount());
            } else if (count.getName().equals(String.valueOf(ReviewType.IMAGE.getValue()))) {
              countResponse.put(SolrConstants.IMAGE_PENDING, count.getCount());
            }
          }
          break;
        }
        case VendorProductSolrFieldNames.RESTRICTED_KEYWORDS_PRESENT: {
          for (FacetField.Count count : facetField.getValues()) {
            if (Boolean.TRUE.equals(Boolean.parseBoolean(count.getName()))) {
              countResponse.put(SolrConstants.RESTRICTED_KEYWORD,
                count.getCount());
            }
          }
          break;
        }
        default:
          log.info("Encountered facet : {}", facetField.getName());
          break;
      }
    }
  }

  private void processStateFacetFields(Map<String, Object> countResponse, QueryResponse response) {
    for (FacetField facetField : response.getFacetFields()) {
      if (facetField.getName().equalsIgnoreCase(VendorProductSolrFieldNames.STATE)) {
        for (FacetField.Count count : facetField.getValues()) {
          if (count.getName().equalsIgnoreCase(WorkflowState.UNASSIGNED.name())) {
            countResponse.put(Constants.BARU, (int) count.getCount());
          } else if (count.getName().equalsIgnoreCase(WorkflowState.IN_REVIEW.name())) {
            countResponse.put(Constants.IN_REVIEW, (int) count.getCount());
          }
        }
      }
    }
  }

  @Override
  @Async
  public void updateSolrOnBrandApprovalAndRejection(List<String> productCodes, String brand, String brandStatus) {
    for (String productCode : productCodes) {
      try {
        solrVendorProductCollectionRepository.updateSolrOnBrandApprovalOrRejection(productCode, brand, brandStatus);
      } catch (IOException | SolrServerException | SolrException e) {
        log.error("Error when updating solr for brand update for productCode : {} ", productCode, e);
      }
    }
  }

  @Override
  @Async
  public void updateSolrOnProductSentBackToVendor(Product product) throws Exception {
    ProductReviewer productReviewer =
      this.productReviewerService.findProductReviewerByProductCode(product.getProductCode());
    publishSolrAddPDTProductBatchEvent(
      Collections.singletonList(new ProductAndReviewerDetailsDTO(product, productReviewer)));
  }

  @Override
  @Async
  public void clearReviewerDetailsAndUpdateState(String productCode, WorkflowState state) throws Exception {
    SolrInputDocument solrInputDocument = VendorProductSolrHelper
        .getSolrInputDocumentForClearReviewerDetailsAndUpdateState(productCode, state);
    this.solrVendorProductCollectionRepository.executeAtomicUpdate(solrInputDocument);
  }

  @Override
  @Async
  public void autoApprovalReviewerDetailsAndUpdateState(String productCode, WorkflowState state) throws Exception {
    SolrInputDocument solrInputDocument = VendorProductSolrHelper
        .getSolrInputDocumentForAutoApprovalAssigneeAndUpdateState(productCode, state);
    this.solrVendorProductCollectionRepository.executeAtomicUpdate(solrInputDocument);
  }

  @Override
  @Async
  public void updateStateOnSolr(String productCode, WorkflowState stateForProduct) {
    SolrInputDocument solrInputDocument =
        VendorProductSolrHelper.getSolrInputDocumentForUpdateState(productCode, stateForProduct);
    this.solrVendorProductCollectionRepository.executeAtomicUpdate(solrInputDocument);
  }

  @Override
  @Async
  public void updateStateAndMfdTrueOnSolr(String productCode, WorkflowState stateForProduct) {
    SolrInputDocument solrInputDocument =
        VendorProductSolrHelper.getSolrInputDocumentForUpdateStateAndMfdTrue(productCode, stateForProduct);
    this.solrVendorProductCollectionRepository.executeAtomicUpdate(solrInputDocument);
  }


  @Override
  public void assignProductToVendorAtomicUpdate(String storeId, String vendorCode,
      List<String> productCodeList) {
    List<SolrInputDocument> solrInputDocumentList =
        VendorProductSolrHelper.getInputForVendorMappingAtomicUpdate(vendorCode, productCodeList);
    solrVendorProductCollectionRepository
        .executeAtomicUpdateForListOfInputDocuments(solrInputDocumentList);
  }

  @Override
  public void updateReviewerByProductCodes(List<String> productCodeList, String assignedTo,
      Date assignedDate) {
    List<SolrInputDocument> solrInputDocumentList =
        Optional.ofNullable(productCodeList).orElse(new ArrayList<>()).stream().map(
            productCode -> VendorProductSolrHelper
                .getInputForReviewerUpdate(productCode, assignedTo, assignedDate))
            .collect(Collectors.toList());
    solrVendorProductCollectionRepository.executeAtomicUpdateForListOfInputDocuments(solrInputDocumentList);
  }

  @Override
  public void updateImageQcResponseToSolr(Product product, ProductReviewer productReviewer) {
    SolrInputDocument solrInputDocument = VendorProductSolrHelper.getInputForImageQcResponse(product, productReviewer);
    solrVendorProductCollectionRepository.executeAtomicUpdate(solrInputDocument);
  }

  @Override
  @Async
  public void updatePostLiveFlag(String productCode, boolean postLive) {
    SolrInputDocument solrInputDocument =
        VendorProductSolrHelper.getInputForPostLiveFlagUpdate(productCode, postLive);
    solrVendorProductCollectionRepository.executeAtomicUpdate(solrInputDocument);
  }

  @Override
  public Map<String, Object> getReviewConfigCountsByVendor(String storeId, String vendorCode)
      throws Exception {
    QueryResponse queryResponse =
        solrVendorProductCollectionRepository.getReviewConfigCountsByVendor(storeId, vendorCode);
    return setReviewConfigCount(queryResponse);
  }

  private Map<String, Object> setReviewConfigCount(QueryResponse queryResponse) {
    Map<String, Object> response = new HashMap<>();
    for (FacetField facetField : queryResponse.getFacetFields()) {
      if (VendorProductSolrFieldNames.POST_LIVE.equals(facetField.getName())) {
        for (FacetField.Count count : facetField.getValues()) {
          if (Boolean.TRUE.toString().equalsIgnoreCase(count.getName())) {
            response.put(POST_LIVE, count.getCount());
          } else {
            response.put(PRE_LIVE, count.getCount());
          }
        }
      }
    }
    return response;
  }

  @Override
  public Map<String, Object> getReviewConfigCountsByVendorAndConfig(String storeId, String vendorCode, boolean postLive)
      throws Exception {
    QueryResponse queryResponse =
        solrVendorProductCollectionRepository.getReviewConfigCountsByVendorAndConfig(storeId, vendorCode, postLive);
    return setProductCountForConfig(queryResponse);
  }

  @Override
  public void updateSolrOnApprovalOrSave(PDTProductUpdateProductToSolrEventModel product) throws Exception {
    boolean categoryChanged = false;
    SolrDocument solrDocument =
        solrVendorProductCollectionRepository.getSolrDocument(product.getProductCode());
    if (Objects.isNull(solrDocument)) {
      return;
    }
    List<String> categoryCodes =
        (List<String>) solrDocument.getFieldValue(VendorProductSolrFieldNames.CATEGORY_CODES);
    if (!categoryCodes.contains(product.getCategoryCodes().get(0))) {
      categoryChanged = true;
    }
    log.info("fetching products from solr  PDTProductUpdateProductToSolrEventModel = {} ", product);
    solrVendorProductCollectionRepository.updateSolrOnApprovalOrSave(product,
        categoryChanged);
  }

  @Override
  public void updateProductOnRetryStatusUpdate(Product updatedProduct) throws Exception {
    if (!updatedProduct.isMarkForDelete()) {
      SolrInputDocument solrInputDocument = VendorProductSolrHelper.getSolrInputDocumentForProductRetryUpdate(updatedProduct);
      this.solrVendorProductCollectionRepository.executeAtomicUpdate(solrInputDocument);
    } else {
      solrVendorProductCollectionRepository.deleteDocumentFromSolr(
        Collections.singletonList(updatedProduct.getProductCode()), false);
    }
  }

  @Override
  public Page<ProductBusinessPartnerMapper> findProductBusinessPartnerMapper(List<WorkflowState> workflowStateList,
      String searchCriteria, Pageable pageable, String storeId) throws Exception {
    QueryResponse queryResponse = solrVendorProductCollectionRepository
        .findProductBusinessPartnerMapper(workflowStateList, searchCriteria, pageable, storeId);
    return new PageImpl<>(queryResponse.getResults().stream().map(VendorProductSolrHelper::toProductBusinessPartnerList)
        .collect(Collectors.toList()), pageable, queryResponse.getResults().getNumFound());
  }

  private Map<String, Object> setProductCountForConfig(QueryResponse queryResponse) {
    Map<String, Object> response = new HashMap<>();
    for (FacetField facetField : queryResponse.getFacetFields()) {
      if (VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE.equals(facetField.getName())) {
        for (FacetField.Count count : facetField.getValues()) {
          if (ProductReviewType.NEWLY_ADDED.getValue().equalsIgnoreCase(count.getName())) {
            response.put(NEWLY_ADDED, count.getCount());
          } else if (ProductReviewType.EDITED.getValue().equalsIgnoreCase(count.getName())) {
            response.put(EDITED, count.getCount());
          } else if (ProductReviewType.REVISED.getValue().equalsIgnoreCase(count.getName())) {
            response.put(REVISED, count.getCount());
          }
        }
      }
    }
    return response;
  }
}
