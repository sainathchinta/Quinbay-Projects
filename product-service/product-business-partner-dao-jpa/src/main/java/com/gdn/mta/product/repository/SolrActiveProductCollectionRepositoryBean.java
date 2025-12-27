package com.gdn.mta.product.repository;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.SolrProductCollectionUpdateEvent;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.FacetField;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.util.ClientUtils;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrException;
import org.apache.solr.common.SolrInputDocument;
import org.apache.solr.common.params.CommonParams;
import org.apache.solr.common.params.CursorMarkParams;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import com.gda.mta.product.dto.ProductFilterRequest;
import com.gdn.mta.product.valueobject.SolrCategoryCodeDTO;
import com.gdn.mta.product.valueobject.SolrProductCodeDTO;
import com.gdn.mta.product.valueobject.SolrProductCollectionDTO;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.util.SolrConstants;
import com.gdn.partners.pbp.commons.util.SolrFieldNames;

/**
 * Created by virajjasani on 09/12/16.
 */
@Repository
public class SolrActiveProductCollectionRepositoryBean implements
    SolrActiveProductCollectionRepository {

  private static final String QUERY_ALL_PARAMETERS = "*:*";
  private static final String QUERY_ALL_VALUES = "*";
  private static final String AND_PREDICATE = " AND ";
  private static final String OR_PREDICATE = " OR ";
  private static final String STORE_ID_FIELD = "store_id:";
  private static final String PARENTHESES_OPEN = "(";
  private static final String PARENTHESES_CLOSE = ")";
  private static final String UPDATED_STEP_DATE = "updated_step_date";
  private static final String PRODUCT_CODE_FIELD = "product_code:";
  private static final String PRODUCT_CODE= "product_code";
  private static final String PRODUCT_NAME_FIELD = "product_name:";
  private static final String CATEGORY_CODE_FIELD = "category_code:";
  private static final String REVIEW_PENDING_FIELD = "review_pending:";
  private static final String NOT_PREDICATE = "-";
  private static final String CREATED_BY_FIELD = "created_by:";
  private static final String BRAND_FIELD = "brand:";
  private static final String DOUBLE_QUOTE = "\"";
  private static final String ALPHANUMERIC_REGEX = "[^a-zA-Z0-9']+";
  private static final String NUMERIC_REGEX = "(?<=\\D)(?=\\d)";

  private static final Logger LOGGER =
      LoggerFactory.getLogger(SolrActiveProductCollectionRepositoryBean.class);

  @Autowired
  @Qualifier(value = "prdCollectionClient")
  private CloudSolrClient prdCollectionClient;

  @Value("${solr.batch.size}")
  private String batchSize;

  @Value("${b2b.exclusive.switch}")
  private boolean b2bExclusiveSwitch;

  @Value("${product.collection.delete.solr.event.enabled}")
  private boolean productCollectionDeleteSolrEventEnabled;


  @Autowired
  private ProductOutbound productOutbound;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Override
  public Page<SolrProductCollectionDTO> getProductCollectionListFromSolrCollection(String storeId, String keyword,
      String categoryCode, Boolean reviewPending, String sortBy, Pageable pageable) throws Exception {
    List<SolrProductCollectionDTO> solrProductCollectionDTOList = new ArrayList<>();
    long totalRecords = 0;
    try {
      SolrQuery solrQuery = new SolrQuery();
      StringBuilder filterQuery = getFilterQuery(storeId, categoryCode, reviewPending);
      String queryString = QUERY_ALL_PARAMETERS;
      solrQuery.setStart(pageable.getPageNumber() * pageable.getPageSize());
      solrQuery.setRows(pageable.getPageSize());
      if (SolrQuery.ORDER.asc.toString().equals(sortBy)) {
        solrQuery.setSort(UPDATED_STEP_DATE, SolrQuery.ORDER.asc);
      } else {
        solrQuery.setSort(UPDATED_STEP_DATE, SolrQuery.ORDER.desc);
      }
      if (StringUtils.isNotBlank(keyword)) {
        queryString = getProductNameOrProductCodeOrCreatedByQuery(keyword).toString();
      }
      solrQuery.setQuery(queryString);
      solrQuery.setFilterQueries(filterQuery.toString());
      QueryResponse queryResponse = this.prdCollectionClient.query(solrQuery);
      totalRecords = queryResponse.getResults().getNumFound();
      queryResponse.getResults().forEach(solrDocument -> {
        SolrProductCollectionDTO solrProductCollectionDTO = getSolrProductCollectionDTO(solrDocument);
        solrProductCollectionDTOList.add(solrProductCollectionDTO);
      });
    } catch (SolrServerException | IOException e) {
      LOGGER.error("error while retrieving product collection values from solr core: prd_collection.", e);
      throw e;
    }
    return new PageImpl<>(solrProductCollectionDTOList, pageable, totalRecords);
  }

  @Override
  public List<String> getProductCodesListFromSolr(String storeId, String keyword, String categoryCode,
      Boolean reviewPending, String sortBy, Pageable pageable) {
    List<String> productCodes = new ArrayList<>();
    String queryString = QUERY_ALL_PARAMETERS;
    try {
      SolrQuery solrQuery = new SolrQuery();
      StringBuilder filterQuery = getFilterQuery(storeId, categoryCode, reviewPending);
      solrQuery.setStart(pageable.getPageNumber() * pageable.getPageSize());
      solrQuery.setRows(pageable.getPageSize());
      if (SolrQuery.ORDER.asc.toString().equals(sortBy)) {
        solrQuery.setSort(UPDATED_STEP_DATE, SolrQuery.ORDER.asc);
      } else {
        solrQuery.setSort(UPDATED_STEP_DATE, SolrQuery.ORDER.desc);
      }
      if (StringUtils.isNotBlank(keyword)) {
        queryString = getProductNameOrProductCodeOrCreatedByQuery(keyword).toString();
      }
      solrQuery.setQuery(queryString);
      solrQuery.setFilterQueries(filterQuery.toString());
      solrQuery.setFields(PRODUCT_CODE);
      QueryResponse queryResponse = this.prdCollectionClient.query(solrQuery);
      queryResponse.getResults().stream().map(solrDocument -> (String) solrDocument.getFieldValue(PRODUCT_CODE)).
              collect(Collectors.toCollection(() -> productCodes));
    } catch (Exception e) {
      LOGGER.error("Exception caught while executing Solr query: {}", queryString, e);
    }
    return productCodes;
  }

  @Override
  public Integer getProductCountByStoreId(String storeId) throws Exception {
    int productCount = 0;
    StringBuilder query = new StringBuilder();
    try {
      SolrQuery solrQuery = new SolrQuery();
      query = new StringBuilder(STORE_ID_FIELD).append(DOUBLE_QUOTE).append(storeId).append(DOUBLE_QUOTE);
      solrQuery.setQuery(query.toString());
      solrQuery.setStart(0);
      solrQuery.setRows(1);
      QueryResponse queryResponse = this.prdCollectionClient.query(solrQuery);
      Long count = queryResponse.getResults().getNumFound();
      productCount = Math.toIntExact(count);

    } catch (Exception e) {
      LOGGER.error("Exception caught while executing Solr query: {} ", query.toString(), e);
    }
    return productCount;
  }

  @Override
  public Set<SolrCategoryCodeDTO> getCategoryCodesSolrByKeyword(String keyword, Integer rows, String businessPartnerCode) throws Exception {
    Set<SolrCategoryCodeDTO> categoryCodes = new LinkedHashSet<>();
    try {
      SolrQuery solrQuery = new SolrQuery();
      StringBuilder query = getProductNameAndProductCodeQuery(keyword);
      solrQuery.setQuery(query.toString());
      solrQuery.addFilterQuery(
          new StringBuilder().append(Constants.DASH_DELIMITER + Constants.REVIEW_PENDING).append(Constants.COLON)
              .append(Boolean.TRUE).toString());
      addB2bExclusiveFilterQuery(businessPartnerCode, solrQuery);
      solrQuery.setStart(0);
      solrQuery.setRows(1);
      solrQuery.setFields(SolrFieldNames.PRODUCT_CODE);
      solrQuery.set(Constants.FACET, String.valueOf(Boolean.TRUE));
      solrQuery.set(Constants.FACET_FIELD, Constants.CATEGORY_CODE);
      solrQuery.set(Constants.FACET_LIMIT, rows);
      solrQuery.set(Constants.FACET_MIN_COUNT, 1);

      QueryResponse queryResponse = this.prdCollectionClient.query(solrQuery);

      LOGGER.info("query response : {}, {}", queryResponse);

      if (Objects.nonNull(queryResponse) && CollectionUtils.isNotEmpty(queryResponse.getFacetFields())
          && CollectionUtils.isNotEmpty(queryResponse.getFacetFields().get(0).getValues())) {
        for (FacetField.Count count : queryResponse.getFacetFields().get(0).getValues()) {
          categoryCodes.add(new SolrCategoryCodeDTO(count.getName(), count.getCount()));
        }
      }
    } catch (Exception e) {
      LOGGER.error(
          "error while retrieving product collection values from solr core: prd_collection.",
          e);
    }
    return categoryCodes;
  }

  private void addB2bExclusiveFilterQuery(String businessPartnerCode, SolrQuery solrQuery)
    throws Exception {
    List<String> salesChannel = new ArrayList<>();
    if (b2bExclusiveSwitch && StringUtils.isNotEmpty(businessPartnerCode)) {
      ProfileResponse profileResponse =
        this.businessPartnerRepository.filterDetailByBusinessPartnerCode(businessPartnerCode);

      if (Objects.nonNull(profileResponse) && Objects.nonNull(profileResponse.getCompany())
        && CollectionUtils.isNotEmpty(profileResponse.getCompany().getSalesChannel())) {
        salesChannel = profileResponse.getCompany().getSalesChannel();
      }
    }
    if (!salesChannel.contains(Constants.B2B_SELLER_CHANNEL)) {
      solrQuery.addFilterQuery(
        new StringBuilder().append(Constants.DASH_DELIMITER + SolrFieldNames.B2B_EXCLUSIVE)
          .append(Constants.COLON).append(Boolean.TRUE).toString());
    }
  }

  @Override
  public SolrProductCodeDTO getProductCodesFromSolrByKeywordAndCategoryCode(
      String keyword, String categoryCode, Pageable pageable) throws Exception {
    Set<String> productCodes = new LinkedHashSet<>();
    SolrQuery solrQuery = new SolrQuery();
    StringBuilder keywordQuery =
        new StringBuilder(PRODUCT_NAME_FIELD).append(DOUBLE_QUOTE).append(keyword)
            .append(DOUBLE_QUOTE).append(OR_PREDICATE).append(PRODUCT_CODE_FIELD)
            .append(DOUBLE_QUOTE).append(keyword).append(DOUBLE_QUOTE);
    StringBuilder categoryCodeQuery =
        new StringBuilder(CATEGORY_CODE_FIELD).append(DOUBLE_QUOTE).append(categoryCode)
            .append(DOUBLE_QUOTE);
    solrQuery.add(CommonParams.Q, keywordQuery.toString());
    solrQuery.setFilterQueries(categoryCodeQuery.toString());
    solrQuery.setStart(pageable.getPageNumber() * pageable.getPageSize());
    solrQuery.setRows(pageable.getPageSize());
    QueryResponse queryResponse = this.prdCollectionClient.query(solrQuery);
    queryResponse.getResults().forEach(solrDocument -> productCodes
        .add(String.valueOf(solrDocument.getFieldValue("product_code"))));
    Long totalCount = queryResponse.getResults().getNumFound();
    return new SolrProductCodeDTO(productCodes, totalCount);
  }

  private String generateCategoryCodeQuery(List<String> categoryCodes) {
    if (CollectionUtils.isNotEmpty(categoryCodes)) {
      StringBuilder query = new StringBuilder().append(PARENTHESES_OPEN).append(DOUBLE_QUOTE)
          .append(String.join(DOUBLE_QUOTE + SolrConstants.COMMA + DOUBLE_QUOTE, categoryCodes)).append(DOUBLE_QUOTE)
          .append(PARENTHESES_CLOSE);
      return query.toString();
    } else {
      return null;
    }
  }

  @Override
  public SolrProductCodeDTO findProductCodesByKeywordAndCategoryCodes(String keyword, List<String> categoryCodes,
      Pageable pageable) throws Exception {
    Set<String> productCodes = new LinkedHashSet<>();
    String categoryQuery = generateCategoryCodeQuery(categoryCodes);
    SolrQuery solrQuery = new SolrQuery();
    StringBuilder query = getProductNameAndProductCodeQuery(keyword);
    solrQuery.setQuery(query.toString());
    solrQuery.setSort(SolrConstants.SCORE, SolrQuery.ORDER.desc);
    solrQuery.add(Constants.STOPWORDS, String.valueOf(Boolean.TRUE));
    solrQuery.add(Constants.LOWERCASE_OPERATORS, String.valueOf(Boolean.TRUE));
    if (StringUtils.isNotEmpty(categoryQuery)) {
      solrQuery.addFilterQuery(CATEGORY_CODE_FIELD + categoryQuery);
    }
    solrQuery.addFilterQuery(
        new StringBuilder().append(Constants.DASH_DELIMITER + Constants.REVIEW_PENDING).append(Constants.COLON)
            .append(Boolean.TRUE).toString());
    solrQuery.setStart(pageable.getPageNumber() * pageable.getPageSize());
    solrQuery.setRows(pageable.getPageSize());
    QueryResponse queryResponse = prdCollectionClient.query(solrQuery);
    Long totalCount = 0L;
    if (Objects.nonNull(queryResponse)) {
      queryResponse.getResults().forEach(solrDocument -> productCodes.add(String.valueOf(solrDocument.getFieldValue
          (Constants.PRODUCT_CODE))));
      totalCount = queryResponse.getResults().getNumFound();
    }
    return new SolrProductCodeDTO(productCodes, totalCount);
  }

  private StringBuilder getProductNameAndProductCodeQuery(String keyword) {
    String productNameQuery = getProductNameQuery(keyword);
    StringBuilder query = new StringBuilder();
    query.append(Constants.PRODUCT_CODE).append(Constants.COLON)
        .append(DOUBLE_QUOTE + keyword.trim() + DOUBLE_QUOTE).append(SolrConstants.BOOST).append(4);
        if (StringUtils.isNotBlank(productNameQuery)){
          query.append(StringUtils.SPACE + SolrConstants.OR + StringUtils.SPACE).append(Constants.PRODUCT_NAME)
        .append(Constants.COLON).append(PARENTHESES_OPEN).append(productNameQuery).append(PARENTHESES_CLOSE)
        .append(SolrConstants.BOOST).append(2).append(StringUtils.SPACE + SolrConstants.OR + StringUtils.SPACE)
        .append(Constants.PRODUCT_NAME).append(Constants.COLON).append(PARENTHESES_OPEN).append(productNameQuery)
        .append(SolrConstants.LIKE_QUERY).append(PARENTHESES_CLOSE);
        }
    return query;
  }

  private StringBuilder getProductNameOrProductCodeOrCreatedByQuery(String keyword) {
    String productNameQuery = getProductNameQuery(keyword);
    return new StringBuilder().append(Constants.PRODUCT_CODE).append(Constants.COLON)
        .append(DOUBLE_QUOTE + keyword.trim() + DOUBLE_QUOTE)
        .append(StringUtils.SPACE + SolrConstants.OR + StringUtils.SPACE).append(Constants.PRODUCT_NAME)
        .append(Constants.COLON).append(PARENTHESES_OPEN).append(productNameQuery).append(PARENTHESES_CLOSE)
        .append(StringUtils.SPACE + SolrConstants.OR + StringUtils.SPACE).append(SolrFieldNames.CREATED_BY)
        .append(Constants.COLON).append(DOUBLE_QUOTE).append(keyword.trim()).append(DOUBLE_QUOTE);
  }

  private String getProductNameQuery(String keyword) {
    return Arrays.stream(keyword.trim().split(ALPHANUMERIC_REGEX))
        .filter(s -> !s.trim().isEmpty())
        .flatMap(s -> Arrays.stream(s.trim().split(NUMERIC_REGEX)))
        .filter(s -> !s.trim().isEmpty())
        .collect(Collectors.joining(StringUtils.SPACE + SolrConstants.AND + StringUtils.SPACE));
  }

  private StringBuilder getFilterQuery(String storeId, String categoryCode) {
    StringBuilder filterQuery =
        new StringBuilder(STORE_ID_FIELD).append(storeId);
    if (!StringUtils.isEmpty(categoryCode)) {
      filterQuery.append(AND_PREDICATE).append(CATEGORY_CODE_FIELD).append(categoryCode);
    }
    return filterQuery;
  }

  private StringBuilder getFilterQuery(String storeId, String categoryCode, Boolean reviewPending) {
    StringBuilder filterQuery = new StringBuilder(STORE_ID_FIELD).append(storeId);
    if (Objects.nonNull(reviewPending)) {
      if (!reviewPending) {
        filterQuery.append(AND_PREDICATE).append(NOT_PREDICATE).append(REVIEW_PENDING_FIELD).append(true);
      } else {
        filterQuery.append(AND_PREDICATE).append(REVIEW_PENDING_FIELD).append(reviewPending);
      }
    }
    if (!StringUtils.isEmpty(categoryCode)) {
      filterQuery.append(AND_PREDICATE).append(CATEGORY_CODE_FIELD).append(categoryCode);
    }
    return filterQuery;
  }

  private SolrProductCollectionDTO getSolrProductCollectionDTO(SolrDocument solrDocument) {
    SolrProductCollectionDTO solrProductCollectionDTO = new SolrProductCollectionDTO();
    solrProductCollectionDTO.setId(String.valueOf(solrDocument.getFieldValue("id")));
    solrProductCollectionDTO.setStoreId(String.valueOf(solrDocument.getFieldValue("store_id")));
    solrProductCollectionDTO
        .setProductId(String.valueOf(solrDocument.getFieldValue("product_id")));
    solrProductCollectionDTO
        .setProductCode(String.valueOf(solrDocument.getFieldValue("product_code")));
    solrProductCollectionDTO
        .setProductName(String.valueOf(solrDocument.getFieldValue("product_name")));
    solrProductCollectionDTO.setBrand(String.valueOf(solrDocument.getFieldValue("brand")));
    solrProductCollectionDTO
        .setCategoryCode(String.valueOf(solrDocument.getFieldValue("category_code")));
    solrProductCollectionDTO
        .setCategoryName(String.valueOf(solrDocument.getFieldValue("category_name")));
    solrProductCollectionDTO.setBusinessPartnerCode(
        String.valueOf(solrDocument.getFieldValue("business_partner_code")));
    solrProductCollectionDTO.setBusinessPartnerName(
        String.valueOf(solrDocument.getFieldValue("business_partner_name")));
    solrProductCollectionDTO.setMarkForDelete(
        (Boolean) solrDocument.getFieldValue("mark_for_delete"));
    setSolrProductCollectionDateInfo(solrDocument, solrProductCollectionDTO);
    solrProductCollectionDTO
        .setUpdatedBy(String.valueOf(solrDocument.getFieldValue("updated_by")));
    solrProductCollectionDTO
        .setCreatedBy(String.valueOf(solrDocument.getFieldValue("created_by")));
    if (Objects.nonNull(solrDocument.getFieldValue("review_pending"))) {
      solrProductCollectionDTO.setReviewPending((Boolean) solrDocument.getFieldValue("review_pending"));
    }
    return solrProductCollectionDTO;
  }

  private void setSolrProductCollectionDateInfo(SolrDocument solrDocument,
      SolrProductCollectionDTO solrProductCollectionDTO) {
    solrProductCollectionDTO
        .setUpdatedStepDate((Date) solrDocument.getFieldValue("updated_step_date"));
    solrProductCollectionDTO.setUpdatedDate((Date) solrDocument.getFieldValue("updated_date"));
    solrProductCollectionDTO.setCreatedDate((Date) solrDocument.getFieldValue("created_date"));
  }

  @Override
  public void addSolrProductCollectionDocument(SolrProductCollectionDTO solrProductCollectionDTO) {
    try {
      if (b2bExclusiveSwitch) {
        CategoryResponse categoryResponse = productOutbound.getCategoryBasicDetailByCategoryCode(
          solrProductCollectionDTO.getCategoryCode());
        if (categoryResponse != null) {
          solrProductCollectionDTO.setB2bExclusive(categoryResponse.isB2bExclusive());
        }
      }
      SolrInputDocument solrInputDocument =
          getSolrInputDocumentFromProductCollectionDTO(solrProductCollectionDTO);
      this.prdCollectionClient.add(solrInputDocument);
    } catch (Exception e) {
      LOGGER.error(
          "error while saving productCollection solr document to solr core: prd_collection. "
              + "docId: {}, productCode: {}",
          solrProductCollectionDTO.getId(), solrProductCollectionDTO.getProductCode(), e);
    }
  }

  private SolrInputDocument getSolrInputDocumentFromProductCollectionDTO(
      SolrProductCollectionDTO solrProductCollectionDTO) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.addField("id", solrProductCollectionDTO.getId());
    solrInputDocument.addField("store_id", solrProductCollectionDTO.getStoreId());
    solrInputDocument.addField("product_id", solrProductCollectionDTO.getProductId());
    solrInputDocument.addField("product_code", solrProductCollectionDTO.getProductCode());
    solrInputDocument.addField("product_name", solrProductCollectionDTO.getProductName());
    solrInputDocument.addField("brand", solrProductCollectionDTO.getBrand());
    solrInputDocument.addField("category_code", solrProductCollectionDTO.getCategoryCode());
    solrInputDocument.addField("category_name", solrProductCollectionDTO.getCategoryName());
    solrInputDocument
        .addField("business_partner_code", solrProductCollectionDTO.getBusinessPartnerCode());
    solrInputDocument
        .addField("business_partner_name", solrProductCollectionDTO.getBusinessPartnerName());
    solrInputDocument.addField("mark_for_delete", solrProductCollectionDTO.isMarkForDelete());
    solrInputDocument.addField("updated_step_date", solrProductCollectionDTO.getUpdatedStepDate());
    solrInputDocument.addField("updated_date", solrProductCollectionDTO.getUpdatedDate());
    solrInputDocument.addField("created_date", solrProductCollectionDTO.getCreatedDate());
    solrInputDocument.addField("updated_by", solrProductCollectionDTO.getUpdatedBy());
    solrInputDocument.addField("created_by", solrProductCollectionDTO.getCreatedBy());
    solrInputDocument.addField("review_pending", solrProductCollectionDTO.isReviewPending());
    solrInputDocument.addField("b2b_exclusive", solrProductCollectionDTO.isB2bExclusive());
    return solrInputDocument;
  }

  private static SolrProductCollectionUpdateEvent getSolrProductCollectionUpdateEvent(String documentId) {
    SolrProductCollectionUpdateEvent solrProductCollectionUpdateEvent = new SolrProductCollectionUpdateEvent();
    solrProductCollectionUpdateEvent.setDocumentId(documentId);
    return solrProductCollectionUpdateEvent;
  }

  @Override
  public List<String> getAllActiveBrandsByCategoryCodes(List<String> categoryCodes) {
    try {
      boolean done = false;
      SolrQuery solrQuery = new SolrQuery();
      solrQuery.setQuery(SolrFieldNames.CATEGORY_CODE + SolrConstants.COLON + getQueryForListOfStrings(categoryCodes));
      String cursorMark = CursorMarkParams.CURSOR_MARK_START;
      solrQuery.addFilterQuery(SolrConstants.COLLAPSE_FQ_FOR_BRAND);
      solrQuery.addSort(SolrFieldNames.BRAND, SolrQuery.ORDER.asc);
      solrQuery.addSort(SolrFieldNames.ID, SolrQuery.ORDER.asc);
      solrQuery.setFields(SolrFieldNames.BRAND);
      solrQuery.setRows(Integer.valueOf(batchSize));
      List<String> brands = new ArrayList<>();
      while (!done) {
        solrQuery.set(CursorMarkParams.CURSOR_MARK_PARAM , cursorMark);
        QueryResponse queryResponse = prdCollectionClient.query(solrQuery);
        brands.addAll(getBrandsFromSolrDocument(queryResponse.getResults()));
        if (cursorMark.equals(queryResponse.getNextCursorMark())) {
          done = true;
        }
        cursorMark = queryResponse.getNextCursorMark();
      }
      return brands;
    } catch (SolrServerException | IOException | SolrException e) {
      return Collections.emptyList();
    }
  }

  @Override
  public Page<SolrProductCollectionDTO> getProductCollectionListForFilterRequest(String storeId,
      ProductFilterRequest productFilterRequest, Pageable pageable) throws Exception {
    List<SolrProductCollectionDTO> solrProductCollectionDTOList;
    long totalRecords = 0;
    SolrQuery solrQuery = new SolrQuery();
    StringBuilder filterQuery = getFilterQueryForProductFilterRequest(storeId, productFilterRequest);
    String queryString = getProductFilterRequestQueryString(productFilterRequest, solrQuery);
    solrQuery.setStart(pageable.getPageNumber() * pageable.getPageSize());
    solrQuery.setRows(pageable.getPageSize());
    solrQuery.setSort(UPDATED_STEP_DATE, SolrQuery.ORDER.desc);
    solrQuery.setQuery(queryString);
    solrQuery.setFilterQueries(filterQuery.toString());
    solrQuery.setFields(SolrFieldNames.PRODUCT_CODE, SolrFieldNames.PRODUCT_NAME, SolrFieldNames.BRAND,
        SolrFieldNames.CATEGORY_NAME, SolrFieldNames.MARK_FOR_DELETE);
    QueryResponse queryResponse = this.prdCollectionClient.query(solrQuery);
    totalRecords = queryResponse.getResults().getNumFound();
    solrProductCollectionDTOList =
        Optional.ofNullable(queryResponse.getResults()).orElse(new SolrDocumentList()).stream()
            .map(this::getSolrProductCollectionDTO).collect(Collectors.toList());
    return new PageImpl<>(solrProductCollectionDTOList, pageable, totalRecords);
  }

  @Override
  public void deleteSolrProductCollectionByDocumentId(String documentId) {
    try {
      prdCollectionClient.deleteById(documentId);
    } catch (Exception e) {
      LOGGER.error(
          "error while deleting productCollection solr document from solr core: " + "prd_collection. docId: {} ",
          documentId, e);
    }
  }

  private StringBuilder getFilterQueryForProductFilterRequest(String storeId,
      ProductFilterRequest productFilterRequest) {
    StringBuilder filterQuery =
        new StringBuilder(STORE_ID_FIELD).append(storeId);
    if (!StringUtils.isEmpty(productFilterRequest.getCategoryCode())) {
      filterQuery.append(AND_PREDICATE).append(CATEGORY_CODE_FIELD)
          .append(SolrActiveProductCollectionRepositoryBean.DOUBLE_QUOTE)
          .append(productFilterRequest.getCategoryCode())
          .append(SolrActiveProductCollectionRepositoryBean.DOUBLE_QUOTE);
    }
    if (!StringUtils.isEmpty(productFilterRequest.getBrandName())) {
      filterQuery.append(AND_PREDICATE).append(BRAND_FIELD)
          .append(SolrActiveProductCollectionRepositoryBean.DOUBLE_QUOTE)
          .append(ClientUtils.escapeQueryChars(productFilterRequest.getBrandName()))
          .append(SolrActiveProductCollectionRepositoryBean.DOUBLE_QUOTE);
    }
    return filterQuery;
  }

  private String getProductFilterRequestQueryString(ProductFilterRequest productFilterRequest, SolrQuery solrQuery) {
    String queryString = StringUtils.EMPTY;
    if (StringUtils.isNotBlank(productFilterRequest.getProductCode())) {
      queryString = new StringBuilder(SolrActiveProductCollectionRepositoryBean.PARENTHESES_OPEN)
          .append(SolrActiveProductCollectionRepositoryBean.PRODUCT_CODE_FIELD)
          .append(SolrActiveProductCollectionRepositoryBean.DOUBLE_QUOTE).append(productFilterRequest.getProductCode())
          .append(SolrActiveProductCollectionRepositoryBean.DOUBLE_QUOTE)
          .append(SolrActiveProductCollectionRepositoryBean.PARENTHESES_CLOSE).toString();
    }
    if (StringUtils.isNotEmpty(productFilterRequest.getProductName())) {
      solrQuery.set(SolrConstants.QF, SolrFieldNames.PRODUCT_NAME);
      solrQuery.set(SolrConstants.MM, 100);
      solrQuery.set(SolrConstants.DEF_TYPE, SolrConstants.EDISMAX);
      if (StringUtils.isNotEmpty(queryString)) {
        queryString = new StringBuilder(queryString).append(SolrActiveProductCollectionRepositoryBean.AND_PREDICATE)
            .append(SolrActiveProductCollectionRepositoryBean.PARENTHESES_OPEN)
            .append(getProductNameQuery(ClientUtils.escapeQueryChars(productFilterRequest.getProductName())))
            .append(SolrActiveProductCollectionRepositoryBean.PARENTHESES_CLOSE).toString();
      } else {
        queryString =
            new StringBuilder(getProductNameQuery(ClientUtils.escapeQueryChars(productFilterRequest.getProductName())))
                .toString();
      }
    }
    return StringUtils.isNotEmpty(queryString) ? queryString : QUERY_ALL_PARAMETERS;
  }

  private String getQueryForListOfStrings(List<String> strings) {
    return new StringBuilder().append(SolrConstants.OPEN_BRACKET).append(SolrConstants.DOUBLE_QUOTES)
        .append(String.join(SolrConstants.COMMA_WITH_QUOTES, strings)).append(SolrConstants.DOUBLE_QUOTES)
        .append(SolrConstants.CLOSE_BRACKET).toString();
  }

  private List<String> getBrandsFromSolrDocument(SolrDocumentList solrDocuments) {
    return solrDocuments.stream().map(solrDocument -> String.valueOf(solrDocument.getFieldValue(SolrFieldNames.BRAND)))
        .collect(Collectors.toList());
  }
}
