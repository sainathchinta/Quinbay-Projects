package com.gdn.x.mta.distributiontask.dao.api.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import com.gdn.x.mta.distributiontask.dao.util.CustomBoostQueryBuilder;
import org.apache.commons.collections.CollectionUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrException;
import org.apache.solr.common.SolrInputDocument;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.dao.api.SolrVendorProductCollectionRepository;
import com.gdn.x.mta.distributiontask.dao.util.VendorProductSolrHelper;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductUpdateProductToSolrEventModel;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.dto.DistributionTaskMultipleFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.PrimaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.SummaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.enums.VendorProductSolrFieldNames;
import com.gdn.x.mta.distributiontask.model.solr.SolrConstants;
import com.gdn.x.mta.distributiontask.model.solr.VendorProductSolr;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Repository
public class SolrVendorProductCollectionRepositoryImpl implements SolrVendorProductCollectionRepository {

  @Autowired
  @Qualifier(value = "vendorProductCollectionClient")
  private CloudSolrClient vendorProductCollectionClient;

  @Autowired
  private ProductServiceRepository productServiceRepository;

  @Autowired
  private CustomBoostQueryBuilder customBoostQueryBuilder;

  @Value("${appeal.product.enabled}")
  private boolean appealProductEnabled;

  @Override
  public void deleteDocumentFromSolr(List<String> productCodes, boolean deleteAll) throws Exception {
    try {
      if (CollectionUtils.isNotEmpty(productCodes)) {
        vendorProductCollectionClient.deleteById(productCodes);
      } else if (deleteAll) {
        vendorProductCollectionClient.deleteByQuery(VendorProductSolrHelper.getQueryForDeleteAll());
      }
    } catch (Exception e) {
      log.error("Error deleting documents by query : {} ", productCodes, e);
      throw e;
    }
  }

  @Override
  public void addDocumentToSolrWithCategoryHierarchy(
      List<VendorProductSolr> vendorProductSolrList) {
    List<SolrInputDocument> solrInputDocumentList = new ArrayList<>();
    List<String> categoryCodes =
        Optional.ofNullable(vendorProductSolrList).orElse(new ArrayList<>()).stream()
            .map(vendorProductSolr -> vendorProductSolr.getCategoryCodes().get(0)).distinct()
            .collect(Collectors.toList());
    CategoryCodeRequest categoryCodeRequest = new CategoryCodeRequest(categoryCodes);
    List<CategoryHierarchyResponse> categoryHierarchyResponseList =
        this.productServiceRepository.getCategoryHierarchyByCategoryCodes(categoryCodeRequest);
    Map<String, List<CategoryResponse>> mapCategoryCodeAndHierarchy =
        categoryHierarchyResponseList.stream().collect(Collectors
            .toMap(categoryHierarchyResponse -> categoryHierarchyResponse.getCategoryCode(),
                categoryHierarchyResponse -> categoryHierarchyResponse.getCategoryHierarchy()));
    for (VendorProductSolr vendorProductSolr : vendorProductSolrList) {
      List<CategoryResponse> categoryHierarchy =
          mapCategoryCodeAndHierarchy.get(vendorProductSolr.getCategoryCodes().get(0));
      vendorProductSolr.setCategoryCodes(
          categoryHierarchy.stream().map(categoryResponse -> categoryResponse.getCategoryCode())
              .collect(Collectors.toList()));
      vendorProductSolr.setCategoryNames(
          categoryHierarchy.stream().map(categoryResponse -> categoryResponse.getName())
              .collect(Collectors.toList()));
      SolrInputDocument solrInputDocument =
          VendorProductSolrHelper.toVendorSolrInputDocument(vendorProductSolr);
      solrInputDocumentList.add(solrInputDocument);
    }
    try {
      if (CollectionUtils.isNotEmpty(solrInputDocumentList)) {
        vendorProductCollectionClient.add(solrInputDocumentList);
      }
    } catch (Exception e) {
      log.error("Error while adding list of documents to pdt_product solr : {} ",
          vendorProductSolrList.stream().map(VendorProductSolr::getProductCode)
              .collect(Collectors.toList()), e);
    }
  }

  @Override
  public Page<VendorProductSolr> getVendorProductsBySolrQuery(String storeId,
      SummaryFilterDTO summaryFilterDTO, List<WorkflowState> states, Pageable pageable)
      throws IOException, SolrServerException, SolrException {
    List<VendorProductSolr> vendorProductSolrList = new ArrayList<>();
    SolrQuery solrQuery = VendorProductSolrHelper
        .getSolrQueryForVendorList(storeId, summaryFilterDTO, states, pageable);
    log.info("#getVendorProductsBySolrQuery : solrQuery was : {} ", solrQuery);
    QueryResponse queryResponse = this.vendorProductCollectionClient.query(solrQuery);
    long totalRecords = queryResponse.getResults().getNumFound();
    queryResponse.getResults().forEach(solrDocument -> {
      VendorProductSolr vendorProductSolr =
          VendorProductSolrHelper.getSolrProductCollectionDTO(solrDocument);
      vendorProductSolrList.add(vendorProductSolr);
    });
    return new PageImpl<>(vendorProductSolrList, pageable, totalRecords);
  }

  @Override
  public Page<VendorProductSolr> getAllProductDetailsWithMultipleFilterForDistributionListBySolrQuery(
      DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO, Pageable pageable)
      throws IOException, SolrServerException, SolrException {
    List<VendorProductSolr> vendorProductSolrList = new ArrayList<>();
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    QueryResponse queryResponse = this.vendorProductCollectionClient.query(solrQuery);
    long totalRecords = queryResponse.getResults().getNumFound();
    queryResponse.getResults().forEach(solrDocument -> {
      VendorProductSolr vendorProductSolr =
          VendorProductSolrHelper.getSolrProductCollectionDTO(solrDocument);
      vendorProductSolrList.add(vendorProductSolr);
    });
    return new PageImpl<>(vendorProductSolrList, pageable, totalRecords);
  }

  @Override
  public Page<VendorProductSolr> getFilterProductSummaryBySolrQuery(String storeId, ProductListRequest productListRequest,
      List<WorkflowState> states, Pageable pageable) throws IOException, SolrServerException, SolrException {
    List<VendorProductSolr> vendorProductSolrList = new ArrayList<>();
    SolrQuery solrQuery = VendorProductSolrHelper.getSolrQueryForFilterProductSummary(storeId, productListRequest,
        states, pageable);
    QueryResponse queryResponse = this.vendorProductCollectionClient.query(solrQuery);
    long totalRecords = queryResponse.getResults().getNumFound();
    queryResponse.getResults().forEach(solrDocument -> {
      VendorProductSolr vendorProductSolr =
          VendorProductSolrHelper.getSolrProductCollectionDTO(solrDocument);
      vendorProductSolrList.add(vendorProductSolr);
    });
    return new PageImpl<>(vendorProductSolrList, pageable, totalRecords);
  }

  @Override
  public QueryResponse getFilterCounts(String storeId, String vendorCode, Boolean edited,
    Boolean postLive, Boolean revised) throws IOException, SolrServerException {
    SolrQuery solrQuery = VendorProductSolrHelper.getSolrQueryForFilterCounts(storeId, vendorCode
      , edited, postLive, revised);
    log.info("solrQuery for getFilterCounts is : {} ", solrQuery);
    return vendorProductCollectionClient.query(solrQuery);
  }

  @Override
  public QueryResponse getFinalQCCounts(String storeId) throws IOException, SolrServerException {
    SolrQuery solrQuery = VendorProductSolrHelper.getSolrQueryForFinalQcCounts(storeId);
    return vendorProductCollectionClient.query(solrQuery);
  }

  @Override
  public QueryResponse getDistributionListCounts(String storeId) throws IOException, SolrServerException {
    SolrQuery solrQuery = VendorProductSolrHelper.getSolrQueryForDistributionListCounts(storeId);
    return vendorProductCollectionClient.query(solrQuery);
  }

  @Override
  public QueryResponse getBusinessPartnerList(String storeId, PrimaryFilterDTO primaryFilterDTO,
      List<WorkflowState> states, int page, int size) throws IOException, SolrServerException {
    List<String> stateList = states.stream().map(WorkflowState::name).collect(Collectors.toList());
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForBusinessPartnerList(storeId, primaryFilterDTO, stateList);
    solrQuery.setStart(page * size);
    solrQuery.setRows(size);
    solrQuery.setFields(new String[] {VendorProductSolrFieldNames.BUSINESS_PARTNER_CODE, VendorProductSolrFieldNames.BUSINESS_PARTNER_NAME});
    return vendorProductCollectionClient.query(solrQuery);
  }

  @Override
  public void updateSolrOnContentApprovalOrSave(Product product, ProductReviewer productReviewer, boolean categoryChanged)
      throws IOException, SolrServerException {
    List<CategoryResponse> hierarchy = new ArrayList<>();
    if (categoryChanged) {
      List<CategoryHierarchyResponse> hierarchyResponses = productServiceRepository
          .getCategoryHierarchyByCategoryCodes(new CategoryCodeRequest(Arrays.asList(product.getCategoryCode())));
      hierarchy = hierarchyResponses.stream()
          .filter(hierarchyResponse -> product.getCategoryCode().equals(hierarchyResponse.getCategoryCode()))
          .map(CategoryHierarchyResponse::getCategoryHierarchy).flatMap(List::stream).collect(Collectors.toList());
    }
    vendorProductCollectionClient
        .add(VendorProductSolrHelper.getSolrInputDocumentOnContentApprovalOrSave(product, productReviewer, hierarchy));
  }

  @Override
  public void updateSolrOnApprovalOrSave(PDTProductUpdateProductToSolrEventModel product, boolean categoryChanged)
      throws IOException, SolrServerException {
    List<CategoryResponse> hierarchy = new ArrayList<>();
    if (categoryChanged) {
      List<CategoryHierarchyResponse> hierarchyResponses = productServiceRepository.getCategoryHierarchyByCategoryCodes(
          new CategoryCodeRequest(product.getCategoryCodes()));
      hierarchy = hierarchyResponses.stream()
          .filter(hierarchyResponse -> product.getCategoryCodes().contains(hierarchyResponse.getCategoryCode()))
          .map(CategoryHierarchyResponse::getCategoryHierarchy).flatMap(List::stream).collect(Collectors.toList());
    }
    vendorProductCollectionClient.add(VendorProductSolrHelper.getSolrInputDocumentOnApprovalOrSave(product, hierarchy));
  }

  @Override
  public void updateSolrOnImageApproval(Product product, ProductReviewer productReviewer) throws IOException, SolrServerException {
    vendorProductCollectionClient.add(VendorProductSolrHelper.getSolrInputDocumentOnImageApproval(product, productReviewer));
  }

  @Override
  public SolrDocument getSolrDocument(String productCode) throws IOException, SolrServerException {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(VendorProductSolrFieldNames.PRODUCT_CODE + SolrConstants.COLON + VendorProductSolrHelper
        .appendDoubleQuotes(productCode));
    QueryResponse queryResponse = vendorProductCollectionClient.query(solrQuery);
    if (Objects.nonNull(queryResponse) && CollectionUtils.isNotEmpty(queryResponse.getResults())) {
      return queryResponse.getResults().get(0);
    }
    return null;
  }

  @Override
  public void updateSolrOnBrandApprovalOrRejection(String productCode, String brand, String brandStatus)
      throws IOException, SolrServerException {
    vendorProductCollectionClient
        .add(VendorProductSolrHelper.getSolrInputDocumentOnBrandApprovalOrRejection(productCode, brand, brandStatus));
  }

  @Override
  public void executeAtomicUpdate(SolrInputDocument solrInputDocument) {
    try {
      vendorProductCollectionClient.add(solrInputDocument);
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Error while performing atomic update for document document : {}, exxception - ",
          solrInputDocument, e);
    }
  }

  @Override
  public void executeAtomicUpdateForListOfInputDocuments(
      List<SolrInputDocument> solrInputDocumentList) {
    try {
      vendorProductCollectionClient.add(solrInputDocumentList);
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Error while performing atomic update for document document : {}, exception - ",
          solrInputDocumentList, e);
    }
  }

  @Override
  public QueryResponse getReviewConfigCountsByVendor(String storeId, String vendorCode)
      throws IOException, SolrServerException, SolrException {
    SolrQuery solrQuery =
        VendorProductSolrHelper.getQueryForReviewConfigCounts(storeId, vendorCode);
    try {
      return vendorProductCollectionClient.query(solrQuery);
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Error while fetching review config counts ", e);
      throw e;
    }
  }

  @Override
  public QueryResponse getReviewConfigCountsByVendorAndConfig(String storeId, String vendorCode, boolean postLive)
      throws IOException, SolrServerException, SolrException {
    SolrQuery solrQuery = VendorProductSolrHelper.getQueryForReviewCountsForConfig(storeId, vendorCode, postLive);
    try {
      return vendorProductCollectionClient.query(solrQuery);
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Error while fetching review config counts ", e);
      throw e;
    }
  }

  @Override
  public QueryResponse findProductBusinessPartnerMapper(List<WorkflowState> workflowStateList, String searchCriteria,
      Pageable pageable, String storeId) throws IOException, SolrServerException {
    List<String> stateList = workflowStateList.stream().map(WorkflowState::name).collect(Collectors.toList());
    SolrQuery solrQuery =
        VendorProductSolrHelper.getQueryForBusinessPartnerMapper(stateList, searchCriteria, pageable, storeId);
    try {
      return vendorProductCollectionClient.query(solrQuery);
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Error while fetching review config counts ", e);
      throw e;
    }
  }

  @Override
  public Page<VendorProductSolr> getFilteredAndBoostedProductsFromSolr(String storeId,
    SummaryFilterDTO summaryFilterDTO, Pageable pageable) throws SolrServerException, IOException {
    SolrQuery boostQueryForAutoAssignment =
      customBoostQueryBuilder.getBoostQueryForAutoAssignment(appealProductEnabled);
    List<VendorProductSolr> vendorProductSolrList = new ArrayList<>();
    SolrQuery filteredBoostedProducts =
      VendorProductSolrHelper.getSolrQueryForFilteredBoostedProducts(summaryFilterDTO, pageable,
        storeId, boostQueryForAutoAssignment);
    filteredBoostedProducts.setFields(VendorProductSolrFieldNames.PRODUCT_CODE);
    log.info("boostQueryForAutoAssignment is {} ", boostQueryForAutoAssignment);
    QueryResponse queryResponse = vendorProductCollectionClient.query(filteredBoostedProducts);
    if (Objects.nonNull(queryResponse) && (CollectionUtils.isNotEmpty(
      queryResponse.getResults()))) {
      long totalRecords = queryResponse.getResults().getNumFound();
      queryResponse.getResults().forEach(solrDocument -> {
        VendorProductSolr vendorProductSolr =
          VendorProductSolrHelper.getSolrProductCollectionDTO(solrDocument);
        vendorProductSolrList.add(vendorProductSolr);
      });

      return new PageImpl<>(vendorProductSolrList, pageable, totalRecords);
    }
    return null;
  }
}
