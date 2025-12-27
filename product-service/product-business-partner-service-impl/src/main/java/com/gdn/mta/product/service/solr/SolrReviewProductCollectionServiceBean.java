package com.gdn.mta.product.service.solr;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.time.DateUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.IntervalFacet;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.SolrException;
import org.apache.solr.common.SolrInputDocument;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gda.mta.product.dto.DalamProductListRequest;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionAddEvent;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionAddEventFields;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionDeleteEvent;
import com.gdn.mta.product.entity.ApplicationConfigProperties;
import com.gdn.mta.product.entity.ProductBusinessPartnerMapper;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.WorkflowStates;
import com.gdn.mta.product.repository.CategoryRepository;
import com.gdn.mta.product.repository.SolrReviewProductCollectionRepository;
import com.gdn.mta.product.service.ApplicationConfigPropertiesService;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.ProductSystemParameterService;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.valueobject.SolrProductCollectionDTO;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import com.gdn.partners.pbp.commons.util.SolrConstants;
import com.gdn.partners.pbp.commons.util.SolrFieldNames;
import com.gdn.partners.pbp.model.productlevel3.ProductCollectionCountResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class SolrReviewProductCollectionServiceBean implements SolrReviewProductCollectionService {

  @Lazy
  @Autowired
  private ProductService productService;

  @Autowired
  private CategoryRepository categoryRepository;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private ApplicationConfigPropertiesService applicationConfigPropertiesService;

  @Autowired
  private SolrReviewProductCollectionRepository solrReviewProductCollectionRepository;

  @Autowired
  private ProductSystemParameterService productSystemParameterService;

  private static final String IN_PROGRESS = "IN_PROGRESS";
  private static final String DELETED = "DELETED";
  private static final String DEFAULT_USERANME = "system";

  @Value("${review.product.collection.solr.event.enabled}")
  private boolean reviewProductCollectionSolrEventEnabled;

  @Trace(dispatcher=true)
  @Override
  @Async
  public void fullReindexCollection(String storeId, boolean isScreeningReindex) throws Exception {
    if (isScreeningReindex) {
      this.solrReviewProductCollectionRepository.deleteAllScreeningDocumentsFromSolr();
      Pageable pageable = PageRequest.of(0, 100);
      Page<ProductCollection> productCollectionPage;
      do {
        productCollectionPage =
            productService.getProductsByStoreIdAndActivatedAndViewable(storeId, false, false, pageable);
        publishSolrEvents(productCollectionPage, isScreeningReindex);
        pageable = productCollectionPage.nextPageable();
      } while (productCollectionPage.hasNext());
    } else {
      this.solrReviewProductCollectionRepository.deleteAllInProgressDocumentsFromSolr();
      Pageable pageable = PageRequest.of(0, 100);
      Page<ProductCollection> productCollectionPage;
      do {
        productCollectionPage =
            productService.getProductsByStoreIdAndActivatedAndReviewPending(storeId, true, true, pageable);
        publishSolrEvents(productCollectionPage, isScreeningReindex);
        pageable = productCollectionPage.nextPageable();
      } while (productCollectionPage.hasNext());
    }
  }

  @Trace(dispatcher = true)
  @Override
  @Async
  public void deltaReindexCollection(String storeId) throws Exception {
    int hourThreshold = Integer.parseInt(productSystemParameterService.findByStoreIdAndVariable(storeId,
        SystemParameterConstants.DELTA_REINDEX_HOUR_THRESHOLD).getValue());
    int batchSize = Integer.parseInt(productSystemParameterService.findByStoreIdAndVariable(storeId,
        SystemParameterConstants.DELTA_REINDEX_BATCH_SIZE).getValue());
    Pageable pageable = PageRequest.of(0, batchSize);
    Page<ProductCollection> productCollectionPage;
    Date currentDate = new Date();
    Date lastUpdatedDate = DateUtils.addHours(currentDate, -hourThreshold);
    do {
      productCollectionPage =
          productService.getProductsByStoreIdAndUpdatedDateBetween(storeId, lastUpdatedDate, currentDate, pageable);
      log.info("Partial reindexing, productCollections:{}", productCollectionPage.getContent());
      publishSolrEventsToDeltaReindexCollection(productCollectionPage);
      pageable = productCollectionPage.nextPageable();
    } while (productCollectionPage.hasNext());
  }

  @Override
  public void deltaReindexProductCode(String storeId, String productCode) throws Exception {
    Page<ProductCollection> productCollectionPage =
        productService.getProductByStoreIdAndProductCode(storeId, productCode, PageRequest.of(0, 1));
    publishSolrEventsToDeltaReindexProductCode(productCollectionPage);
  }

  @Trace(dispatcher=true)
  @Override
  @Async
  public void publishKafkaEventToAddProductToReviewProductCollection(ProductCollection productCollection) throws Exception {
    SolrReviewProductCollectionAddEventFields solrReviewProductCollectionAddEventFields =
        ConverterUtil.toScreeningSolrProductCollectionAddEvent(productCollection);
    addCategoryInfoToSolrEvent(solrReviewProductCollectionAddEventFields, productCollection.getCategoryCode());
    SolrReviewProductCollectionAddEvent solrReviewProductCollectionAddEvent =
        SolrReviewProductCollectionAddEvent.builder()
            .solrReviewProductCollectionAddEventFieldsList(Arrays.asList(solrReviewProductCollectionAddEventFields))
            .build();
    kafkaProducer.send(DomainEventName.SOLR_ADD_REVIEW_PRODUCT_REQUEST, solrReviewProductCollectionAddEvent);
  }

  @Override
  public void addProductToReviewProductCollection(ProductCollection productCollection) throws Exception {
    SolrReviewProductCollectionAddEventFields solrReviewProductCollectionAddEventFields =
        ConverterUtil.toScreeningSolrProductCollectionAddEvent(productCollection);
    addCategoryInfoToSolrEvent(solrReviewProductCollectionAddEventFields, productCollection.getCategoryCode());
    try {
      SolrInputDocument solrInputDocument = ConverterUtil.toSolrInputDocument(solrReviewProductCollectionAddEventFields);
      if (reviewProductCollectionSolrEventEnabled) {
        SolrReviewProductCollectionAddEvent solrReviewProductCollectionAddEvent =
            new SolrReviewProductCollectionAddEvent();
        solrReviewProductCollectionAddEvent.setSolrReviewProductCollectionAddEventFieldsList(
            Collections.singletonList(solrReviewProductCollectionAddEventFields));
        kafkaProducer.send(DomainEventName.SOLR_ADD_REVIEW_PRODUCT_REQUEST, productCollection.getProductCode(),
            solrReviewProductCollectionAddEvent);
      } else {
        this.solrReviewProductCollectionRepository.updateProductToSolrCollection(solrInputDocument);
      }
    } catch (SolrException | SolrServerException | IOException e) {
      log.error("Exception caught while adding product in solr, solrDocument: {} ", solrReviewProductCollectionAddEventFields,
          e);
    }
  }

  @Override
  public void deleteProductFromReviewProductCollection(String id) {
    try {
      SolrReviewProductCollectionDeleteEvent solrReviewProductCollectionDeleteEvent =
          new SolrReviewProductCollectionDeleteEvent();
      solrReviewProductCollectionDeleteEvent.setIds(Collections.singletonList(id));
      kafkaProducer.send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
          solrReviewProductCollectionDeleteEvent);
    } catch (Exception e) {
      log.error("Exception caught while deleting product in solr, id: {}", id, e);
    }
  }

  @Override
  public void updateBrandApprovedInReviewProductCollection(String id, boolean brandApproved, String brandName) {
    try {
      SolrReviewProductCollectionAddEvent solrReviewProductCollectionAddEvent =
          CommonUtils.getSolrReviewProductCollectionAddEvent(id, brandApproved, brandName);
      kafkaProducer.send(DomainEventName.SOLR_ADD_REVIEW_PRODUCT_REQUEST, solrReviewProductCollectionAddEvent);
    } catch (Exception e) {
      log.error("Exception caught while updating brandApproved in solr, id:{}", id, e);
    }
  }

  @Override
  public ProductCollectionCountResponse getInReviewProducts(String storeId, String keyword, String businessPartnerCode,
      String categoryCode) throws IOException, SolrServerException {
    QueryResponse queryResponse =
        this.solrReviewProductCollectionRepository.getInReviewProducts(storeId, keyword, businessPartnerCode, categoryCode);
    return convertQueryResponseToProductCollectionCountResponse(queryResponse);
  }

  private ProductCollectionCountResponse convertQueryResponseToProductCollectionCountResponse(
      QueryResponse queryResponse) {
    ProductCollectionCountResponse productCollectionCountResponse = new ProductCollectionCountResponse();
    List<IntervalFacet> intervalFacets = queryResponse.getIntervalFacets();
    Map<String, Integer> facetCountMap = intervalFacets.get(0).getIntervals().stream()
        .collect(Collectors.toMap(IntervalFacet.Count::getKey, IntervalFacet.Count::getCount));
    productCollectionCountResponse.setToday(facetCountMap.get(SolrConstants.TODAY_FACET_INTERVAL));
    productCollectionCountResponse.setYesterday(facetCountMap.get(SolrConstants.YESTERDAY_FACET_INTERVAL));
    productCollectionCountResponse.setTwoDaysAgo(facetCountMap.get(SolrConstants.TWO_DAYS_AGO_FACET_INTERVAL));
    productCollectionCountResponse.setThreeUntilFiveDaysAgo(facetCountMap.get(SolrConstants.THREE_TO_FIVE_DAYS_AGO_FACET_INTERVAL));
    productCollectionCountResponse.setMoreThan5Days(facetCountMap.get(SolrConstants.FIVE_DAYS_AGO_FACET_INTERVAL));
    return productCollectionCountResponse;
  }

  private void updateApplicationConfigProperties(String storeId, String deltaRunTime) {
    ApplicationConfigProperties applicationConfigProperties = applicationConfigPropertiesService
        .findByStoreIdAndPropertyNameAndMarkForDeleteFalse(storeId, deltaRunTime);
    applicationConfigProperties.setValue(String.valueOf(new Date().getTime()));
    applicationConfigProperties.setUpdatedBy(DEFAULT_USERANME);
    applicationConfigProperties.setUpdatedDate(new Date());
    applicationConfigProperties.setMarkForDelete(false);
    applicationConfigPropertiesService.save(applicationConfigProperties);
  }

  @Override
  public void updateAssignedToInReviewProductCollection(String id, String assignedTo) {
    try {
      this.solrReviewProductCollectionRepository.updateAssignedToInSolrCollection(id, assignedTo);
    } catch (CloudSolrClient.RouteException | SolrServerException | IOException e) {
      log.error("Exception caught while updating assignedTo in solr, id: {}", id, e);
    }
  }

  private void publishSolrEvents(Page<ProductCollection> productCollectionPage, boolean isScreeningReindex)
      throws Exception {
    SolrReviewProductCollectionAddEvent solrReviewProductCollectionAddEvent =
        SolrReviewProductCollectionAddEvent.builder().solrReviewProductCollectionAddEventFieldsList(new ArrayList<>())
            .build();
    if (!isScreeningReindex) {
      for (ProductCollection productCollection : productCollectionPage.getContent()) {
        if (!WorkflowStates.NEED_CORRECTION.getValue().equals(productCollection.getState())) {
          if (!productCollection.isMarkForDelete()) {
            addProductToSolr(solrReviewProductCollectionAddEvent, productCollection);
          }
        }
      }
    } else {
      for (ProductCollection productCollection : productCollectionPage.getContent()) {
        if (WorkflowStates.DRAFT.getValue().equals(productCollection.getState())) {
          if (!productCollection.isMarkForDelete()) {
            addProductToSolr(solrReviewProductCollectionAddEvent, productCollection);
          }
        }
      }
    }
    kafkaProducer.send(DomainEventName.SOLR_ADD_REVIEW_PRODUCT_REQUEST, solrReviewProductCollectionAddEvent);
  }

  private void addProductToSolr(SolrReviewProductCollectionAddEvent solrReviewProductCollectionAddEvent,
      ProductCollection productCollection) throws Exception {
    SolrReviewProductCollectionAddEventFields solrReviewProductCollectionAddEventFields =
        ConverterUtil.toScreeningSolrProductCollectionAddEvent(productCollection);
    addCategoryInfoToSolrEvent(solrReviewProductCollectionAddEventFields, productCollection.getCategoryCode());
    solrReviewProductCollectionAddEvent.getSolrReviewProductCollectionAddEventFieldsList()
        .add(solrReviewProductCollectionAddEventFields);
  }

  private void addProductToSolrEvent(SolrReviewProductCollectionAddEvent solrReviewProductCollectionAddEvent,
      List<ProductCollection> productCollections) throws Exception {
    Map<String, Pair<List<String>, List<String>>> categoryDataMap = getCategoryHierarchy(productCollections);
    for (ProductCollection productCollection : productCollections) {
      SolrReviewProductCollectionAddEventFields solrReviewProductCollectionAddEventFields =
          ConverterUtil.toScreeningSolrProductCollectionAddEvent(productCollection);
      addCategoryInfoToSolrEvent(solrReviewProductCollectionAddEventFields, productCollection, categoryDataMap);
      solrReviewProductCollectionAddEvent.getSolrReviewProductCollectionAddEventFieldsList()
          .add(solrReviewProductCollectionAddEventFields);
    }
  }

  private void publishSolrEventsToDeltaReindexCollection(Page<ProductCollection> productCollectionPage)
      throws Exception {
    SolrReviewProductCollectionAddEvent solrReviewProductCollectionAddEvent =
        SolrReviewProductCollectionAddEvent.builder().solrReviewProductCollectionAddEventFieldsList(new ArrayList<>())
            .build();
    SolrReviewProductCollectionDeleteEvent solrReviewProductCollectionDeleteEvent =
        SolrReviewProductCollectionDeleteEvent.builder().ids(new ArrayList<>()).build();
    List<ProductCollection> addProductToSolr = new ArrayList<>();
    for (ProductCollection productCollection : productCollectionPage.getContent()) {
      if ((WorkflowStates.DRAFT.getValue().equals(productCollection.getState()) && !productCollection.isActivated()
          && !productCollection.isViewable())
          || (productCollection.isActivated() && productCollection.isReviewPending()) && !WorkflowStates.NEED_CORRECTION
          .getValue().equals(productCollection.getState())) {
        if (!productCollection.isMarkForDelete()) {
          addProductToSolr.add(productCollection);
        } else {
          solrReviewProductCollectionDeleteEvent.getIds().add(productCollection.getId());
        }
      } else if (WorkflowStates.NEED_CORRECTION.getValue().equals(productCollection.getState())) {
        solrReviewProductCollectionDeleteEvent.getIds().add(productCollection.getId());
      } else if (DELETED.equals(productCollection.getState())) {
        solrReviewProductCollectionDeleteEvent.getIds().add(productCollection.getId());
      } else if (WorkflowStates.ACTIVE.getValue().equals(productCollection.getState()) && !productCollection
          .isReviewPending()) {
        solrReviewProductCollectionDeleteEvent.getIds().add(productCollection.getId());
      }
    }
    if (CollectionUtils.isNotEmpty(addProductToSolr)) {
      addProductToSolrEvent(solrReviewProductCollectionAddEvent, addProductToSolr);
    }
    if (CollectionUtils.isNotEmpty(solrReviewProductCollectionAddEvent.getSolrReviewProductCollectionAddEventFieldsList())) {
      kafkaProducer.send(DomainEventName.SOLR_ADD_REVIEW_PRODUCT_REQUEST, solrReviewProductCollectionAddEvent);
    }
    if (CollectionUtils.isNotEmpty(solrReviewProductCollectionDeleteEvent.getIds())) {
      kafkaProducer.send(DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST, solrReviewProductCollectionDeleteEvent);
    }
  }

  private void publishSolrEventsToDeltaReindexProductCode(Page<ProductCollection> productCollectionPage)
      throws Exception {
    SolrReviewProductCollectionAddEvent solrReviewProductCollectionAddEvent =
        SolrReviewProductCollectionAddEvent.builder().solrReviewProductCollectionAddEventFieldsList(new ArrayList<>())
            .build();
    SolrReviewProductCollectionDeleteEvent solrReviewProductCollectionDeleteEvent =
        SolrReviewProductCollectionDeleteEvent.builder().ids(new ArrayList<>()).build();
    ProductCollection productCollection = productCollectionPage.getContent().get(0);
    if (WorkflowStates.NEED_CORRECTION.getValue().equals(productCollection.getState()) || productCollection
        .isMarkForDelete() || !productCollection.isReviewPending())
      solrReviewProductCollectionDeleteEvent.getIds().add(productCollection.getId());
    else
      addProductToSolr(solrReviewProductCollectionAddEvent, productCollection);
    if (CollectionUtils
        .isNotEmpty(solrReviewProductCollectionAddEvent.getSolrReviewProductCollectionAddEventFieldsList())) {
      kafkaProducer.send(DomainEventName.SOLR_ADD_REVIEW_PRODUCT_REQUEST, solrReviewProductCollectionAddEvent);
    }
    if (CollectionUtils.isNotEmpty(solrReviewProductCollectionDeleteEvent.getIds())) {
      kafkaProducer.send(DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST, solrReviewProductCollectionDeleteEvent);
    }

  }

  private void addCategoryInfoToSolrEvent(SolrReviewProductCollectionAddEventFields solrProductCollectionAddEvent,
      String categoryCode) throws Exception {
    List<CategoryResponse> categoryResponseList =
        categoryRepository.findHierarchyByCategoryCode(categoryCode);
    for (CategoryResponse categoryResponse: categoryResponseList) {
      solrProductCollectionAddEvent.getCategoryCodes().add(categoryResponse.getCategoryCode());
      solrProductCollectionAddEvent.getCategoryNames().add(categoryResponse.getName());
    }
  }

  private Map<String, Pair<List<String>, List<String>>> getCategoryHierarchy(
      List<ProductCollection> productCollectionList) throws Exception {
    Map<String, Pair<List<String>, List<String>>> categoryDataMap = new HashMap<>();
    boolean usePcb = Boolean.parseBoolean(
        productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
            SystemParameterConstants.USE_PCB_SWITCH).getValue());
    if (usePcb) {
      Set<String> distinctCategoryCodes =
          productCollectionList.stream().map(ProductCollection::getCategoryCode).collect(Collectors.toSet());
      for (String categoryCode : distinctCategoryCodes) {
        List<String> categoryCodes = new ArrayList<>();
        List<String> categoryNames = new ArrayList<>();
        List<CategoryResponse> categoryResponseList = categoryRepository.findHierarchyByCategoryCode(categoryCode);
        for (CategoryResponse categoryResponse : categoryResponseList) {
          categoryCodes.add(categoryResponse.getCategoryCode());
          categoryNames.add(categoryResponse.getName());
        }
        categoryDataMap.put(categoryCode, Pair.of(categoryCodes, categoryNames));
      }
    } else {
      for (ProductCollection productCollection : productCollectionList) {
        categoryDataMap.put(productCollection.getCategoryCode(),
            Pair.of(Arrays.asList(productCollection.getCategoryCode()),
                Arrays.asList(productCollection.getCategoryName())));
      }
    }
    return categoryDataMap;
  }

  private void addCategoryInfoToSolrEvent(SolrReviewProductCollectionAddEventFields solrProductCollectionAddEvent,
      ProductCollection productCollection, Map<String, Pair<List<String>, List<String>>> categoryDataMap) {
    Pair<List<String>, List<String>> categoryCodeAndNamePair = categoryDataMap.get(productCollection.getCategoryCode());
    solrProductCollectionAddEvent.setCategoryCodes(categoryCodeAndNamePair.getLeft());
    solrProductCollectionAddEvent.setCategoryNames(categoryCodeAndNamePair.getRight());
  }

  @Override
  public Page<ProductCollection> findProductsForDalamProcess(DalamProductListRequest dalamProductListRequest,
      Pageable pageable) throws IOException, SolrServerException {
    Page<SolrProductCollectionDTO> solrProductCollectionDTOPage =
        this.solrReviewProductCollectionRepository.findDalamProductsList(dalamProductListRequest, pageable);
    List<ProductCollection> productCollectionList =
        getProductCollectionFromSolrProductCollection(solrProductCollectionDTOPage, dalamProductListRequest);
    return new PageImpl<>(productCollectionList, pageable, solrProductCollectionDTOPage.getTotalElements());
  }

  private List<ProductCollection> getProductCollectionFromSolrProductCollection(
      Page<SolrProductCollectionDTO> solrProductCollectionDTOPage, DalamProductListRequest request) {
    List<ProductCollection> productCollectionList = new ArrayList<>();
    solrProductCollectionDTOPage.getContent().forEach(solrProductCollectionDTO -> {
      ProductCollection productCollection = new ProductCollection();
      BeanUtils.copyProperties(solrProductCollectionDTO, productCollection);
      productCollection.setViewable(request.getViewable());
      productCollection.setActivated(request.getActivated());
      productCollectionList.add(productCollection);
    });
    return productCollectionList;
  }

  @Override
  public Page<ProductBusinessPartnerMapper> findProductBusinessPartnerMapper(String storeId, boolean activated,
      boolean viewable, boolean isSearch, String searchCriteria, Pageable pageable) throws Exception {
    return solrReviewProductCollectionRepository
        .findProductBusinessPartnerMapper(storeId, activated, viewable, isSearch, searchCriteria, pageable);
  }
}
