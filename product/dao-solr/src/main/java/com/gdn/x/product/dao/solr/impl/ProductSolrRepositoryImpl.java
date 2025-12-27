package com.gdn.x.product.dao.solr.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.enums.CurationStatus;
import com.gdn.x.product.enums.DistributionStatus;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.vo.ProductSkuSizeChartResponse;
import com.gdn.x.product.model.vo.ReelProductListingRequestVo;
import com.gdn.x.product.service.config.KafkaPublisher;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.FacetField;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.util.ClientUtils;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrException;
import org.apache.solr.common.SolrInputDocument;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;

import com.gdn.x.product.dao.solr.api.ProductSolrRepository;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.SolrConstants;
import com.gdn.x.product.enums.SolrFieldNames;
import com.gdn.x.product.exception.SolrCustomException;
import com.gdn.x.product.model.solr.ProductSolr;
import com.gdn.x.product.model.vo.HalalDashboardFilterRequestVo;
import com.gdn.x.product.model.vo.HalalDashboardProductsResponseVo;
import com.gdn.x.product.model.vo.ProductCenterSummaryRequest;
import com.gdn.x.product.model.vo.ProductCountResponseVo;
import com.gdn.x.product.model.vo.ProductSkuSummaryRequestVo;
import com.gdn.x.product.model.vo.ProductSummaryRequestV2Vo;
import com.gdn.x.product.model.vo.ProductSummaryRequestVo;
import com.gdn.x.product.model.vo.ProductSummaryResponseV2Vo;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.product.service.util.ResponseHelper;

import lombok.extern.slf4j.Slf4j;

@Component
@Slf4j
public class ProductSolrRepositoryImpl implements ProductSolrRepository {

  @Autowired
  @Qualifier("xproductL3Client")
  private CloudSolrClient cloudSolrClient;

  @Value("${solr.string.delimiter}")
  private String solrStringDelimiter;

  @Value("${channel.default.value}")
  private String channelDefaultValue;

  @Value("${solr.max.row.size}")
  private int solrMaxRowSize;

  @Value("${promo.filter.enabled}")
  private boolean promoFilterEnabled;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Value("${event.based.solr.update.enabled}")
  private boolean eventBasedSolrUpdateEnable;

  @Value("${price.range.solr.query}")
  private boolean priceRangeSolrQuery;

  @Value("${distribution.seller.list}")
  private Set<String> distributionSellerList;

  @Value("${ranch.integration.enabled}")
  private boolean ranchIntegrationEnabled;

  @Override
  public ProductSolr findOneByProductSkuAndMarkForDeleteFalse(String productSku) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + productSku);
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.setStart(0);
    solrQuery.setRows(1);
    try {
      QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
      ProductSolr productSolr = new ProductSolr();
      if (CollectionUtils.isNotEmpty(queryResponse.getResults())) {
        productSolr =
            queryResponse.getResults().stream().map(CommonUtil::toProductSolr).findFirst().orElse(null);
      }
      return productSolr;
    } catch (SolrServerException | SolrException | IOException e) {
      log.error("Exception caught while fetching solr documents by productSku : {}", productSku, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public List<ProductSolr> findByProductCode(String productCode, Boolean markForDelete) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrFieldNames.PRODUCT_CODE + SolrConstants.COLON + productCode);
    if (Objects.nonNull(markForDelete)) {
      solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + markForDelete);
    }
    solrQuery.setRows(solrMaxRowSize);
    try {
      QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
      List<ProductSolr> productSolrList = new ArrayList<>();
      if (CollectionUtils.isNotEmpty(queryResponse.getResults())) {
        productSolrList.addAll(Optional.ofNullable(queryResponse.getResults()).orElse(new SolrDocumentList()).stream()
            .map(CommonUtil::toProductSolr).collect(Collectors.toList()));
      }
      return productSolrList;
    } catch (SolrServerException | SolrException | IOException e) {
      log.error("Exception caught while fetching solr documents by product code : {} ", productCode, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public ProductSolr findByProductSku(String merchantCode, String productSku) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + productSku);
    solrQuery.setStart(0);
    solrQuery.setRows(1);
    if (StringUtils.isNotBlank(merchantCode)) {
      solrQuery.set(SolrConstants.ROUTE_KEY, merchantCode);
    }
    try {
      QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
      ProductSolr productSolr = new ProductSolr();
      if (CollectionUtils.isNotEmpty(queryResponse.getResults())) {
        productSolr =
            queryResponse.getResults().stream().map(CommonUtil::toProductSolr).findFirst().orElse(null);
      }
      return productSolr;
    } catch (SolrServerException | SolrException | IOException e) {
      log.error("Exception caught while fetching solr documents by productSku : {}", productSku, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public void deleteSolrDocumentsByListOfProductSku(Set<String> productSkuList) {
    try {
      if (CollectionUtils.isNotEmpty(productSkuList)) {
        cloudSolrClient.deleteById(new ArrayList<>(productSkuList));
      }
    } catch (SolrServerException | SolrException | IOException e) {
      log.error("Exception caught while deleting solr documents by productSku : {} ", productSkuList, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public Page<ProductSolr> getProductCenterSummary(String storeId,
      ProductCenterSummaryRequest productCenterSummaryRequest, PageRequest pageRequest) throws Exception {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery = constructSolrQuery(storeId, productCenterSummaryRequest.getCategoryCode(),
        productCenterSummaryRequest.getKeyword(), solrQuery);
    solrQuery.setStart(pageRequest.getPageNumber() * pageRequest.getPageSize());
    solrQuery.setRows(pageRequest.getPageSize());
    try {
      QueryResponse queryResponse = cloudSolrClient.query(solrQuery);
      long totalNumFound = queryResponse.getResults().getNumFound();
      List<ProductSolr> productSolrs =
          queryResponse.getResults().stream().map(CommonUtil::toProductSolr).collect(Collectors.toList());
      return new PageImpl<>(productSolrs, pageRequest, totalNumFound);
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data from solr, ProductCenterFilter {}", productCenterSummaryRequest,
          e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public List<ProductSolr> getUnmappedProductSKusByCategoryCodes(String storeId, List<String> masterCategoryCodes)
      throws Exception {
    String unmappedSkuQuery = SolrFieldNames.MASTER_CATALOG + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + String
        .join(SolrConstants.OR_CLAUSE, masterCategoryCodes) + SolrConstants.CLOSING_BRACKET + SolrConstants.AND_CLAUSE
        + SolrConstants.NULL_SALES_CATALOG;
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(unmappedSkuQuery);
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    List<ProductSolr> productSolrs = new ArrayList<>();
    int start = 0;
    long totalNumFound = 0;
    try {
      do {
        solrQuery.setStart(start * solrMaxRowSize);
        solrQuery.setRows(solrMaxRowSize);
        QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
        totalNumFound = queryResponse.getResults().getNumFound();
        productSolrs
            .addAll(queryResponse.getResults().stream().map(CommonUtil::toProductSolr).collect(Collectors.toList()));
        start++;
      } while ((start * solrMaxRowSize) < totalNumFound);
      return productSolrs;
    } catch (SolrServerException | SolrException | IOException e) {
      log.error("Exception caught while fetching solr documents by category code : {}", masterCategoryCodes, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public Long countByProductCode(String productCode) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrFieldNames.PRODUCT_CODE + SolrConstants.COLON + productCode);
    solrQuery.setRows(Constants.SINGLE_ROW);
    try {
      QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
      return queryResponse.getResults().getNumFound();
    } catch (SolrServerException | SolrException | IOException e) {
      log.error("Exception caught while fetching solr product l3 documents by productCode : {}", productCode, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public Page<ProductSolr> getProductSkuSummary(String storeId,
      ProductSkuSummaryRequestVo productSkuSummaryRequestVo, String businessPartnerCode, int page, int size) {
    SolrQuery solrQuery =
        constructProductSkuSummaryQuery(productSkuSummaryRequestVo, businessPartnerCode);
    solrQuery.setSort(SolrFieldNames.CREATED_DATE,
        StringUtils.equalsIgnoreCase(productSkuSummaryRequestVo.getSortOrder(), SolrConstants.ASC) ?
            SolrQuery.ORDER.asc :
            SolrQuery.ORDER.desc);
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    solrQuery.addFilterQuery(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.setStart(page * size);
    solrQuery.setRows(size);
    solrQuery.set(SolrConstants.MM, 100);
    solrQuery.setFields(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_NAME,
        SolrFieldNames.PRODUCT_MAIN_IMAGE, SolrFieldNames.MASTER_CATALOG, SolrFieldNames.BRAND,
        SolrFieldNames.IS_ARCHIVED, SolrFieldNames.PRODUCT_CODE);
    try {
      QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
      long totalNumFound = queryResponse.getResults().getNumFound();
      return new PageImpl<>(queryResponse.getResults().stream().map(CommonUtil::toProductSolr)
          .collect(Collectors.toList()), PageRequest.of(page, size), totalNumFound);
    } catch (SolrServerException | SolrException | IOException e) {
      log.error(
          "Exception caught while fetching solr documents for productSkuSummaryRequestVo : {}",
          productSkuSummaryRequestVo, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public void updateStockStatus(String productSku, boolean status, String merchantCode) {
    if (eventBasedSolrUpdateEnable) {
      Map<String, Object> fieldsAndValues = new HashMap<>();
      fieldsAndValues.put(SolrFieldNames.IS_IN_STOCK, Collections.singletonMap(SolrConstants.SET_CLAUSE, status));
      log.info("Publishing event : {}, product-sku : {} and fieldsAndValues : {}",
          ProductDomainEventName.UPDATE_TO_SOLR, productSku, fieldsAndValues);
      kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR, productSku,
        new ProductAndItemEventModel(productSku, fieldsAndValues, merchantCode));
    } else {
      SolrInputDocument solrInputFields = new SolrInputDocument();
      solrInputFields.setField(SolrFieldNames.PRODUCT_SKU, productSku);
      solrInputFields
          .setField(SolrFieldNames.IS_IN_STOCK, Collections.singletonMap(SolrConstants.SET_CLAUSE, status));
      solrInputFields.setField(SolrFieldNames.UPDATED_DATE,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, Calendar.getInstance().getTime()));
      try {
        this.cloudSolrClient.add(solrInputFields);
      } catch (SolrServerException | SolrException | IOException e) {
        log.error("Exception caught while updating stock status in solr: {} ", productSku, e);
      }
    }
  }

  @Override
  public <T> void updatePromoOrWholesaleItemSkus(List<T> itemsOrItemPickupPoints, boolean isPromoUpdateOnly) {
    if (eventBasedSolrUpdateEnable && CollectionUtils.isNotEmpty(itemsOrItemPickupPoints) && itemsOrItemPickupPoints.get(0) instanceof Item) {
      String merchantCode = null;
      Map<String, Map<String, Object>> productSkuItemMap = CommonUtil.getFieldsAndValuesForPromoUpdates((List<Item>) itemsOrItemPickupPoints,
        isPromoUpdateOnly);
      for (String productSku : productSkuItemMap.keySet()) {
        log.info("Publishing event : {}, product-sku : {} and fieldsAndValues : {}",
          ProductDomainEventName.UPDATE_TO_SOLR, productSku, productSkuItemMap.get(productSku));
          Item firstItem = (Item) itemsOrItemPickupPoints.get(0);
          merchantCode = firstItem.getMerchantCode();
        kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR, productSku,
            new ProductAndItemEventModel(productSku, productSkuItemMap.get(productSku),
              merchantCode));
      }
      } else if(CollectionUtils.isNotEmpty(itemsOrItemPickupPoints) && itemsOrItemPickupPoints.get(0) instanceof Item){
        List<SolrInputDocument> solrInputDocuments =
          CommonUtil.getSolrInputDocsForPromoUpdates((List<Item>) itemsOrItemPickupPoints,
            isPromoUpdateOnly);
        try {
          this.cloudSolrClient.add(solrInputDocuments);
        } catch (SolrServerException | SolrException | IOException e) {
          log.error("Exception caught while updating promo and wholesale status in solr: {} ",
            itemsOrItemPickupPoints, e);
        }
      }
    }

  @Override
  public Page<ProductSolr> getL3ProductSummaryByProductSummaryRequest(String storeId,
      ProductSummaryRequestVo productSummaryRequest, PageRequest pageRequest) {
    SolrQuery solrQuery = getSolrQueryForL3Listing(storeId, productSummaryRequest, pageRequest);
    QueryResponse queryResponse;
    try {
      queryResponse = cloudSolrClient.query(solrQuery);
      if (Objects.nonNull(queryResponse) && CollectionUtils.isNotEmpty(queryResponse.getResults())) {
        return new PageImpl<>(queryResponse.getResults().stream().map(CommonUtil::toProductSolr).collect(Collectors.toList()),
            PageRequest.of(pageRequest.getPageNumber(), pageRequest.getPageSize()),
            queryResponse.getResults().getNumFound());
      }
    } catch (SolrServerException | SolrException | IOException e) {
        log.error("Exception caught while fetching L3 summary merchantCode : {} ",
            productSummaryRequest.getMerchantCode(), e);
    }
    return new PageImpl<>(new ArrayList<>(), pageRequest, 0);
  }

  @Override
  public Page<ProductSolr> getL3ProductsForReelsByReelProductListingRequest(String storeId,
      ReelProductListingRequestVo reelProductListingRequestVo, PageRequest pageRequest) {
    SolrQuery solrQuery;
    if (CollectionUtils.isNotEmpty(reelProductListingRequestVo.getProductSkuList())) {
      solrQuery = getSolrQueryForReelsListingByProductSkuList(
          reelProductListingRequestVo.getProductSkuList());
    } else {
      solrQuery = getSolrQueryForReelsListing(storeId, reelProductListingRequestVo, pageRequest);
    }
    solrQuery.setFields(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_NAME,
        SolrFieldNames.MINIMUM_SELLING_PRICE, SolrFieldNames.MAXIMUM_SELLING_PRICE,
        SolrFieldNames.IS_IN_STOCK, SolrFieldNames.PRODUCT_MAIN_IMAGE, SolrFieldNames.IS_SUSPENDED,
        SolrFieldNames.IS_ARCHIVED);
    QueryResponse queryResponse;
    try {
      queryResponse = cloudSolrClient.query(solrQuery);
      if (Objects.nonNull(queryResponse) && CollectionUtils.isNotEmpty(
          queryResponse.getResults())) {
        return new PageImpl<>(queryResponse.getResults().stream().map(CommonUtil::toProductSolr)
            .collect(Collectors.toList()),
            PageRequest.of(pageRequest.getPageNumber(), pageRequest.getPageSize()),
            queryResponse.getResults().getNumFound());
      }
    } catch (SolrServerException | SolrException | IOException e) {
      log.error("Exception caught while fetching products for reels listing with request : {} ",
          reelProductListingRequestVo, e);
    }
    return new PageImpl<>(new ArrayList<>(), pageRequest, 0);
  }

  private SolrQuery getSolrQueryForReelsListingByProductSkuList(List<String> productSkuList) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(
        SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + String.join(
            SolrConstants.OR_CLAUSE, productSkuList) + SolrConstants.CLOSING_BRACKET);
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + false);
    solrQuery.addFilterQuery(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + false);
    solrQuery.addFilterQuery(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + false);
    return solrQuery;
  }


  private SolrQuery getSolrQueryForReelsListing(String storeId,
      ReelProductListingRequestVo reelProductListingRequestVo, PageRequest pageRequest) {
    SolrQuery solrQuery = new SolrQuery();
    StringBuilder solrFilterQuery = new StringBuilder();
    StringBuilder solrQueryBuilder = new StringBuilder();
    solrQuery.addFilterQuery(SolrFieldNames.STORE_ID + SolrConstants.COLON + storeId);
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + false);
    solrQuery.addFilterQuery(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + false);

    if (Objects.nonNull(reelProductListingRequestVo.getMerchantCode())) {
      solrQueryBuilder.append(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON)
          .append(reelProductListingRequestVo.getMerchantCode());
      solrQuery.set(SolrConstants.ROUTE_KEY, reelProductListingRequestVo.getMerchantCode());
    } else {
      log.error("Merchant code is null for reel product listing request {} ",
          reelProductListingRequestVo);
    }
    // Keyword Search (productName or productSku)
    if (StringUtils.isNotBlank(reelProductListingRequestVo.getKeyword())) {
      solrQuery.set(SolrConstants.DEF_TYPE, SolrConstants.EDISMAX);
      String query =
          Optional.ofNullable(solrQuery.getQuery()).orElse(StringUtils.EMPTY) + Constants.SPACE
              + ClientUtils.escapeQueryChars(reelProductListingRequestVo.getKeyword().trim());
      solrQueryBuilder.append(query);
      solrQuery.set(SolrConstants.QF,
          SolrFieldNames.PRODUCT_NAME + Constants.SPACE + SolrFieldNames.PRODUCT_SKU);
      solrQuery.set(SolrConstants.MM, SolrConstants.HUNDRED);
    }

    // Categories FQ
    if (CollectionUtils.isNotEmpty(reelProductListingRequestVo.getCategoryCodes())) {
      solrFilterQuery.append(SolrFieldNames.MASTER_CATALOG).append(SolrConstants.COLON)
          .append(SolrConstants.OPEN_BRACKET).append(
              String.join(SolrConstants.OR_CLAUSE, reelProductListingRequestVo.getCategoryCodes()))
          .append(SolrConstants.CLOSING_BRACKET);
    }
    // InStock  FQ
    if (Objects.nonNull(reelProductListingRequestVo.getInStock())) {
      solrQuery.addFilterQuery(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON
          + (reelProductListingRequestVo.getInStock()));
    }
    // Trading Product FQ
    if (Objects.nonNull(reelProductListingRequestVo.getTradingProduct())) {
      solrQueryBuilder.append(SolrConstants.SPACE).append(SolrFieldNames.TRADING_PRODUCT + SolrConstants.COLON)
          .append(reelProductListingRequestVo.getTradingProduct());
    }
    solrQuery.setSort(SolrFieldNames.CREATED_DATE, SolrQuery.ORDER.desc);
    solrQuery.setStart(pageRequest.getPageNumber() * pageRequest.getPageSize());
    solrQuery.setRows(pageRequest.getPageSize());
    if (StringUtils.isNotBlank(solrFilterQuery.toString())) {
      solrQuery.addFilterQuery(solrFilterQuery.toString());
    }
    solrQuery.setQuery(StringUtils.trimToEmpty(solrQueryBuilder.toString()));
    return solrQuery;
  }

  @Override
  public Page<ProductSolr> getProductNameByProductSummaryRequest(String storeId,
      ProductSummaryRequestVo productSummaryRequest, PageRequest pageRequest) {
    SolrQuery solrQuery = getSolrQueryForL3Listing(storeId, productSummaryRequest, pageRequest);
    solrQuery.setFields(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_NAME);
    QueryResponse queryResponse;
    try {
      queryResponse = cloudSolrClient.query(solrQuery);
      if (Objects.nonNull(queryResponse) && CollectionUtils.isNotEmpty(queryResponse.getResults())) {
        return new PageImpl<>(queryResponse.getResults().stream().map(CommonUtil::toProductSolr).collect(Collectors.toList()),
            PageRequest.of(pageRequest.getPageNumber(), pageRequest.getPageSize()),
            queryResponse.getResults().getNumFound());
      }
    } catch (SolrServerException | SolrException | IOException e) {
      log.error("Exception caught while fetching suggestion for merchantCode : {} and keyword : {}",
          productSummaryRequest.getMerchantCode(), productSummaryRequest.getKeyword(), e);
    }
    return new PageImpl<>(new ArrayList<>(), pageRequest, 0);
  }

  @Override
  public ProductCountResponseVo getActiveAndOosProductCount(String merchantCode) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.set(SolrConstants.ROUTE_KEY, merchantCode);
    solrQuery.setQuery(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + appendDoubleQuotes(merchantCode));
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + false);
    solrQuery.addFilterQuery(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + false);
    solrQuery.setStart(0);
    solrQuery.setRows(1);
    solrQuery.setFacet(Boolean.TRUE);
    solrQuery.addFacetField(SolrFieldNames.IS_IN_STOCK);
    ProductCountResponseVo productCountResponseVo = new ProductCountResponseVo();
    try {
      QueryResponse queryResponse = cloudSolrClient.query(solrQuery);
      FacetField facetField = queryResponse.getFacetField(SolrFieldNames.IS_IN_STOCK);
      long oosCount = (long) Optional.ofNullable(
          facetField.getValues().stream().filter(count -> count.getName().equals(Boolean.FALSE.toString())).findFirst())
          .get().orElse(new FacetField.Count(new FacetField(SolrFieldNames.IS_IN_STOCK), Boolean.FALSE.toString(), 0))
          .getCount();
      productCountResponseVo.setActive(queryResponse.getResults().getNumFound() - oosCount);
      productCountResponseVo.setOutOfStock(oosCount);
    } catch (SolrServerException | SolrException | IOException e) {
      log.error("Error while fetching count archived and suspended product count. merchantCode: {} ", merchantCode, e);
    }
    return productCountResponseVo;
  }

  @Override
  public ProductCountResponseVo getSuspendedAndArchivedProductCount(String merchantCode,
      ProductCountResponseVo productCountResponseVo) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.set(SolrConstants.ROUTE_KEY, merchantCode);
    solrQuery.setQuery(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + appendDoubleQuotes(merchantCode));
    solrQuery.addFilterQuery(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + true);
    solrQuery.setStart(0);
    solrQuery.setRows(1);
    solrQuery.setFacet(Boolean.TRUE);
    solrQuery.addFacetField(SolrFieldNames.IS_SUSPENDED);
    solrQuery.addFacetField(SolrFieldNames.MARK_FOR_DELETE);
    try {
      QueryResponse queryResponse = cloudSolrClient.query(solrQuery);
      FacetField suspensionFacetField = queryResponse.getFacetField(SolrFieldNames.IS_SUSPENDED);
      FacetField markForDeleteFacetField = queryResponse.getFacetField(SolrFieldNames.MARK_FOR_DELETE);
      long suspendedCount = (long) Optional.ofNullable(
          suspensionFacetField.getValues().stream().filter(count -> count.getName().equals(Boolean.TRUE.toString()))
              .findFirst()).get()
          .orElse(new FacetField.Count(new FacetField(SolrFieldNames.IS_SUSPENDED), Boolean.TRUE.toString(), 0))
          .getCount();
      long archivedCount = (long) Optional.ofNullable(
          markForDeleteFacetField.getValues().stream().filter(count -> count.getName().equals(Boolean.FALSE.toString()))
              .findFirst()).get()
          .orElse(new FacetField.Count(new FacetField(SolrFieldNames.MARK_FOR_DELETE), Boolean.FALSE.toString(), 0))
          .getCount();
      productCountResponseVo.setSuspended(suspendedCount);
      productCountResponseVo.setArchived(archivedCount);
    } catch (SolrServerException | SolrException | IOException e) {
      log.error("Error while fetching count active product count. merchantCode: {} ", merchantCode, e);
    }
    return productCountResponseVo;
  }

  @Override
  public Page<ProductSummaryResponseV2Vo> getProductSummaryV2(String storeId, int page, int size,
      ProductSummaryRequestV2Vo productSummaryRequestV2Vo) {
    SolrQuery solrQuery = getSolrQueryForProductSummaryV2(storeId, page, size, productSummaryRequestV2Vo);
    try {
      QueryResponse queryResponse = cloudSolrClient.query(solrQuery);
      if (Objects.nonNull(queryResponse) && CollectionUtils.isNotEmpty(queryResponse.getResults())) {
        List<ProductSolr> productSolrs =
            queryResponse.getResults().stream().map(CommonUtil::toProductSolr).collect(Collectors.toList());
        return new PageImpl<>(ResponseHelper.toProductSummaryResponseV2Vo(productSolrs, solrStringDelimiter),
            PageRequest.of(page, size), queryResponse.getResults().getNumFound());
      }
    } catch (SolrServerException | SolrException | IOException e) {
      log.error("Error while fetching productSummaryV2 : {} ", productSummaryRequestV2Vo, e);
    }
    return new PageImpl<>(new ArrayList<>(), PageRequest.of(page, size), 0);
  }

  @Override
  public Page<HalalDashboardProductsResponseVo> getHalalDashboardProductsResponse(String storeId, int page, int size,
      HalalDashboardFilterRequestVo halalDashboardFilterRequestVo) {
    SolrQuery solrQuery = getSolrQueryForHalalDashboardProducts(storeId, page, size, halalDashboardFilterRequestVo);
    try {
      QueryResponse queryResponse = cloudSolrClient.query(solrQuery);
      if (Objects.nonNull(queryResponse) && CollectionUtils.isNotEmpty(queryResponse.getResults())) {
        List<ProductSolr> productSolrs =
            queryResponse.getResults().stream().map(CommonUtil::toProductSolr).collect(Collectors.toList());
        return new PageImpl<>(ResponseHelper.toHalalDashboardProductsResponseVo(productSolrs, solrStringDelimiter),
            PageRequest.of(page, size), queryResponse.getResults().getNumFound());
      }
    } catch (SolrServerException | SolrException | IOException e) {
      log.error("Error while fetching products for Halal Dashboard with halalDashboardFilterRequestVo : {} ",
          halalDashboardFilterRequestVo, e);
    }
    return new PageImpl<>(new ArrayList<>(), PageRequest.of(page, size), 0);
  }

  private SolrQuery getSolrQueryForProductSummaryV2(String storeId, int page, int size,
      ProductSummaryRequestV2Vo productSummaryRequestV2Vo) {
    SolrQuery solrQuery = new SolrQuery();
    StringBuilder query = new StringBuilder();

    query.append(getStoreIdQuery(storeId));

    //add query for merchant
    if (StringUtils.isNotBlank(productSummaryRequestV2Vo.getMerchantCode())) {
      solrQuery.set(SolrConstants.ROUTE_KEY, productSummaryRequestV2Vo.getMerchantCode());
      query.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.MERCHANT_CODE).append(SolrConstants.COLON)
          .append(appendDoubleQuotes(productSummaryRequestV2Vo.getMerchantCode()));
    }

    if (priceRangeSolrQuery) {
      //add query with range for min price
      if (Objects.nonNull(productSummaryRequestV2Vo.getMinPrice())) {
        query.append(SolrConstants.AND_CLAUSE).append(SolrConstants.OPEN_BRACKET)
          .append(SolrConstants.OPEN_BRACKET).append(SolrFieldNames.MINIMUM_LIST_PRICE)
          .append(SolrConstants.COLON).append(String.format(SolrConstants.PRICE_QUERY_WITH_RANGE,
            productSummaryRequestV2Vo.getMinPrice(), productSummaryRequestV2Vo.getMaxPrice()))
          .append(SolrConstants.OR_CLAUSE).append(SolrFieldNames.MINIMUM_SELLING_PRICE)
          .append(SolrConstants.COLON).append(String.format(SolrConstants.PRICE_QUERY_WITH_RANGE,
            productSummaryRequestV2Vo.getMinPrice(), productSummaryRequestV2Vo.getMaxPrice()))
          .append(SolrConstants.CLOSING_BRACKET);
      }

      //add query with range for max price
      if (Objects.nonNull(productSummaryRequestV2Vo.getMaxPrice())) {
        query.append(SolrConstants.OR_CLAUSE).append(SolrConstants.OPEN_BRACKET)
          .append(SolrFieldNames.MAXIMUM_LIST_PRICE).append(SolrConstants.COLON).append(
            String.format(SolrConstants.PRICE_QUERY_WITH_RANGE,
              productSummaryRequestV2Vo.getMinPrice(), productSummaryRequestV2Vo.getMaxPrice()))
          .append(SolrConstants.OR_CLAUSE).append(SolrFieldNames.MAXIMUM_SELLING_PRICE)
          .append(SolrConstants.COLON).append(String.format(SolrConstants.PRICE_QUERY_WITH_RANGE,
            productSummaryRequestV2Vo.getMinPrice(), productSummaryRequestV2Vo.getMaxPrice()))
          .append(SolrConstants.CLOSING_BRACKET).append(SolrConstants.CLOSING_BRACKET);
      }
    } else {

      //add query for min price
      if (Objects.nonNull(productSummaryRequestV2Vo.getMinPrice())) {
        query.append(SolrConstants.AND_CLAUSE).append(SolrConstants.OPEN_BRACKET)
          .append(SolrFieldNames.MINIMUM_LIST_PRICE).append(SolrConstants.COLON).append(
            String.format(SolrConstants.GREATER_THAN_QUERY, productSummaryRequestV2Vo.getMinPrice()))
          .append(SolrConstants.OR_CLAUSE).append(SolrFieldNames.MINIMUM_SELLING_PRICE)
          .append(SolrConstants.COLON).append(
            String.format(SolrConstants.GREATER_THAN_QUERY, productSummaryRequestV2Vo.getMinPrice()))
          .append(SolrConstants.CLOSING_BRACKET);
      }

      //add query for max price
      if (Objects.nonNull(productSummaryRequestV2Vo.getMaxPrice())) {
        query.append(SolrConstants.AND_CLAUSE).append(SolrConstants.OPEN_BRACKET)
          .append(SolrFieldNames.MAXIMUM_LIST_PRICE).append(SolrConstants.COLON).append(
            String.format(SolrConstants.LESS_THAN_QUERY, productSummaryRequestV2Vo.getMaxPrice()))
          .append(SolrConstants.OR_CLAUSE).append(SolrFieldNames.MAXIMUM_SELLING_PRICE)
          .append(SolrConstants.COLON).append(
            String.format(SolrConstants.LESS_THAN_QUERY, productSummaryRequestV2Vo.getMaxPrice()))
          .append(SolrConstants.CLOSING_BRACKET);
      }
    }

    //add query for productSku list
    if (CollectionUtils.isNotEmpty(productSummaryRequestV2Vo.getProductSkuList())) {
      query.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.PRODUCT_SKU).append(SolrConstants.COLON)
          .append(SolrConstants.OPEN_BRACKET)
          .append(String.join(SolrConstants.OR_CLAUSE, productSummaryRequestV2Vo.getProductSkuList()))
          .append(SolrConstants.CLOSING_BRACKET);
    }

    //add query for categoryCode list
    if (CollectionUtils.isNotEmpty(productSummaryRequestV2Vo.getCategoryCodes())) {
      query.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.MASTER_CATALOG).append(SolrConstants.COLON)
          .append(SolrConstants.OPEN_BRACKET)
          .append(String.join(SolrConstants.OR_CLAUSE, productSummaryRequestV2Vo.getCategoryCodes()))
          .append(SolrConstants.CLOSING_BRACKET);
    }

    //add query for brand list
    if (CollectionUtils.isNotEmpty(productSummaryRequestV2Vo.getBrands())) {
      String brandQuery = productSummaryRequestV2Vo.getBrands().stream()
        .map(brand -> SolrConstants.QUOTES.concat(brand).concat(SolrConstants.QUOTES))
        .collect(Collectors.joining(SolrConstants.OR_CLAUSE));

      query.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.BRAND)
        .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET).append(brandQuery)
        .append(SolrConstants.CLOSING_BRACKET);
    }

    //add query for pickupPointCode list
    if (CollectionUtils.isNotEmpty(productSummaryRequestV2Vo.getPickupPointCodes())) {
      query.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.PICKUP_POINT_CODES).append(SolrConstants.COLON)
          .append(SolrConstants.OPEN_BRACKET)
          .append(String.join(SolrConstants.OR_CLAUSE, productSummaryRequestV2Vo.getPickupPointCodes()))
          .append(SolrConstants.CLOSING_BRACKET);
    }

    //add query for keyword
    if (StringUtils.isNotBlank(productSummaryRequestV2Vo.getKeyword())) {
      query.append(SolrConstants.AND_CLAUSE).append(SolrConstants.OPEN_BRACKET)
          .append(ClientUtils.escapeQueryChars(productSummaryRequestV2Vo.getKeyword().trim()))
          .append(SolrConstants.CLOSING_BRACKET);
      solrQuery.set(SolrConstants.QF, SolrFieldNames.PRODUCT_NAME + StringUtils.SPACE + SolrFieldNames.PRODUCT_SKU);
      solrQuery.set(SolrConstants.MM, 100);
    }

    //set query
    if (StringUtils.isBlank(query.toString())) {
      solrQuery.setQuery(new StringBuilder(SolrFieldNames.PRODUCT_SKU).append(SolrConstants.COLON)
          .append(SolrConstants.SOLR_EXPRESSION_DELIMITER).toString());
    } else {
      solrQuery.setQuery(query.toString());
    }

    setFilterQueriesForProductSummaryV2(solrQuery, productSummaryRequestV2Vo);
    setSortForProductSummaryV2(solrQuery, productSummaryRequestV2Vo);

    solrQuery.setStart(page * size);
    solrQuery.setRows(size);

    return solrQuery;
  }

  private SolrQuery getSolrQueryForHalalDashboardProducts(String storeId, int page, int size,
      HalalDashboardFilterRequestVo halalDashboardFilterRequestVo) {
    SolrQuery solrQuery = new SolrQuery();
    StringBuilder query = new StringBuilder();
    query.append(getStoreIdQuery(storeId));
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);

    //this is the primary filter for Halal dashboard  when no filters are selected
    if (CollectionUtils.isEmpty(halalDashboardFilterRequestVo.getCurationStatus())) {
      solrQuery.addFilterQuery(SolrFieldNames.CURATION_STATUS + SolrConstants.COLON + SolrConstants.OPEN_SQUARE_BRACES
          + CurationStatus.NEED_CURATION.getValue() + SolrConstants.TO_CLAUSE + CurationStatus.REJECTED.getValue()
          + SolrConstants.CLOSE_SQUARE_BRACES);
    } else {
      List<Integer> curationStatusFilters = halalDashboardFilterRequestVo.getCurationStatus();
      query.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.CURATION_STATUS).append(SolrConstants.COLON)
          .append(SolrConstants.OPEN_BRACKET);
      for (int i = 0; i < curationStatusFilters.size(); i++) {
        if (i < curationStatusFilters.size() - 1) {
          query.append(curationStatusFilters.get(i)).append(SolrConstants.OR_CLAUSE);
        } else {
          query.append(curationStatusFilters.get(i)).append(SolrConstants.CLOSING_BRACKET);
        }
      }
    }

    //add query for categoryCode list
    if (CollectionUtils.isNotEmpty(halalDashboardFilterRequestVo.getCategories())) {
      query.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.MASTER_CATALOG).append(SolrConstants.COLON)
          .append(SolrConstants.OPEN_BRACKET)
          .append(String.join(SolrConstants.OR_CLAUSE, halalDashboardFilterRequestVo.getCategories()))
          .append(SolrConstants.CLOSING_BRACKET);
    }

    //add query for brand list
    if (CollectionUtils.isNotEmpty(halalDashboardFilterRequestVo.getBrands())) {
      query.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.BRAND).append(SolrConstants.COLON)
          .append(SolrConstants.OPEN_BRACKET)
          .append(String.join(SolrConstants.OR_CLAUSE, halalDashboardFilterRequestVo.getBrands()))
          .append(SolrConstants.CLOSING_BRACKET);
    }

    //add query for keyword
    if (StringUtils.isNotBlank(halalDashboardFilterRequestVo.getKeyword())) {
      query.append(SolrConstants.AND_CLAUSE).append(SolrConstants.OPEN_BRACKET)
          .append(ClientUtils.escapeQueryChars(halalDashboardFilterRequestVo.getKeyword().trim()))
          .append(SolrConstants.CLOSING_BRACKET);
      solrQuery.set(SolrConstants.QF, SolrFieldNames.PRODUCT_NAME + StringUtils.SPACE + SolrFieldNames.PRODUCT_SKU);
      solrQuery.set(SolrConstants.MM, 100);
    }

    solrQuery.setQuery(query.toString());
    solrQuery.setStart(page * size);
    solrQuery.setRows(size);
    return solrQuery;
  }

  private void setFilterQueriesForProductSummaryV2(SolrQuery solrQuery,
      ProductSummaryRequestV2Vo productSummaryRequestV2Vo) {

    //markForDelete filter query
    solrQuery.addFilterQuery(new StringBuilder(SolrFieldNames.MARK_FOR_DELETE).append(SolrConstants.COLON)
        .append(Boolean.FALSE).toString());

    //isArchived filter query
    if (Objects.nonNull(productSummaryRequestV2Vo.getIsArchived())) {
      solrQuery.addFilterQuery(new StringBuilder(SolrFieldNames.IS_ARCHIVED).append(SolrConstants.COLON)
          .append(productSummaryRequestV2Vo.getIsArchived()).toString());
    }

    //off2OnChannelActive filter query
    if (Objects.nonNull(productSummaryRequestV2Vo.getOff2OnChannelActive())) {
      solrQuery.addFilterQuery(new StringBuilder(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE).append(SolrConstants.COLON)
          .append(productSummaryRequestV2Vo.getOff2OnChannelActive()).toString());
    }

    //freeSample filter query
    if (Objects.nonNull(productSummaryRequestV2Vo.getFreeSample())) {
      solrQuery.addFilterQuery(new StringBuilder(SolrFieldNames.FREE_SAMPLE).append(SolrConstants.COLON)
          .append(productSummaryRequestV2Vo.getFreeSample()).toString());
    }

    //tradingProduct filter query
    if (Objects.nonNull(productSummaryRequestV2Vo.getTradingProduct())) {
      solrQuery.addFilterQuery(new StringBuilder(SolrFieldNames.TRADING_PRODUCT).append(SolrConstants.COLON)
          .append(productSummaryRequestV2Vo.getTradingProduct()).toString());
    }

    if (Objects.nonNull(productSummaryRequestV2Vo.getInStock())) {
      solrQuery.addFilterQuery(String.valueOf(new StringBuilder(SolrFieldNames.IS_IN_STOCK)
        .append(SolrConstants.COLON)
        .append((productSummaryRequestV2Vo.getInStock()))).toString());
    }

  }

  private void setSortForProductSummaryV2(SolrQuery solrQuery, ProductSummaryRequestV2Vo productSummaryRequestV2Vo) {
    if (StringUtils.isNotBlank(productSummaryRequestV2Vo.getSortField())) {
      solrQuery.setSort(productSummaryRequestV2Vo.getSortField(),
          StringUtils.isNotBlank(productSummaryRequestV2Vo.getSortOrder()) ?
              (StringUtils.equalsIgnoreCase(productSummaryRequestV2Vo.getSortOrder(), SolrConstants.ASC) ?
                  SolrQuery.ORDER.asc :
                  SolrQuery.ORDER.desc) :
              SolrQuery.ORDER.asc);
    } else {
      solrQuery.setSort(SolrFieldNames.PRODUCT_SKU, StringUtils.isNotBlank(productSummaryRequestV2Vo.getSortOrder()) ?
          (StringUtils.equalsIgnoreCase(productSummaryRequestV2Vo.getSortOrder(), SolrConstants.ASC) ?
              SolrQuery.ORDER.asc :
              SolrQuery.ORDER.desc) :
          SolrQuery.ORDER.asc);
    }
  }

  private SolrQuery getSolrQueryForL3Listing(String storeId, ProductSummaryRequestVo productSummaryRequest,
      PageRequest pageable) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.set(SolrConstants.ROUTE_KEY, productSummaryRequest.getMerchantCode());
    if (StringUtils.isBlank(productSummaryRequest.getSizeChartCode())) {
      solrQuery.setQuery(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + appendDoubleQuotes(
          productSummaryRequest.getMerchantCode()));
    } else {
      getSolrQueryToFetchBasedOnSizeChartCodeAndMerchantCode(productSummaryRequest, solrQuery);
    }
    StringBuilder query = new StringBuilder(solrQuery.getQuery());
    addFilterQueries(storeId, productSummaryRequest, solrQuery);

    if (CollectionUtils.isNotEmpty(productSummaryRequest.getCategoryCodes())) {
      query.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.MASTER_CATALOG).append(SolrConstants.COLON)
          .append(SolrConstants.OPEN_BRACKET)
          .append(String.join(SolrConstants.OR_CLAUSE, productSummaryRequest.getCategoryCodes()))
          .append(SolrConstants.CLOSING_BRACKET);
    }
    if (CollectionUtils.isNotEmpty(productSummaryRequest.getPickupPointCodes())) {
      query.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.PICKUP_POINT_CODES).append(SolrConstants.COLON)
          .append(SolrConstants.OPEN_BRACKET)
          .append(String.join(SolrConstants.OR_CLAUSE, productSummaryRequest.getPickupPointCodes()))
          .append(SolrConstants.CLOSING_BRACKET);
    }
    if (CollectionUtils.isNotEmpty(productSummaryRequest.getPromoTypes())) {
      List<String> promoQueries = new ArrayList<>();
      for (String reviewType : productSummaryRequest.getPromoTypes()) {
        String promoQuery = setPromoTypesQuery(reviewType);
        if (Objects.nonNull(promoQuery)) {
          promoQueries.add(promoQuery);
        }
      }
      if (CollectionUtils.isNotEmpty(promoQueries)) {
        query.append(SolrConstants.AND_CLAUSE).append(new String(SolrConstants.OPEN_BRACKET))
            .append(String.join(SolrConstants.OR_CLAUSE, promoQueries)).append(SolrConstants.CLOSING_BRACKET);
      }
    }
    if (StringUtils.isNotBlank(productSummaryRequest.getKeyword())) {
      query.append(SolrConstants.AND_CLAUSE).append(SolrConstants.OPEN_BRACKET)
          .append(ClientUtils.escapeQueryChars(productSummaryRequest.getKeyword().trim()))
          .append(SolrConstants.CLOSING_BRACKET);
      solrQuery.set(SolrConstants.QF, SolrFieldNames.PRODUCT_NAME + StringUtils.SPACE + SolrFieldNames.PRODUCT_SKU);
      solrQuery.set(SolrConstants.MM, 100);
    }
    boolean distributionSeller = ranchIntegrationEnabled && distributionSellerList.contains(productSummaryRequest.getMerchantCode());
    if (distributionSeller && Boolean.TRUE.equals(productSummaryRequest.getPureExternalUser())) {
      query.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.DISTRIBUTION_STATUS).append(SolrConstants.COLON)
          .append(SolrConstants.OPEN_BRACKET).append(DistributionStatus.NON_DISTRIBUTION.getCode())
          .append(SolrConstants.SPACE).append(SolrConstants.OR_CLAUSE.trim()).append(SolrConstants.SPACE)
          .append(DistributionStatus.DISTRIBUTION.getCode()).append(SolrConstants.CLOSING_BRACKET);
    }
    setSort(productSummaryRequest, solrQuery);
    solrQuery.setStart(pageable.getPageNumber() * pageable.getPageSize());
    solrQuery.setRows(pageable.getPageSize());
    solrQuery.setQuery(query.toString());
    return solrQuery;
  }

  private void getSolrQueryToFetchBasedOnSizeChartCodeAndMerchantCode(ProductSummaryRequestVo productSummaryRequest,
      SolrQuery solrQuery) {
    StringBuilder solrQueryString = new StringBuilder();
    solrQueryString.append(SolrConstants.OPEN_BRACKET).append(SolrFieldNames.MERCHANT_CODE).append(SolrConstants.COLON)
        .append(appendDoubleQuotes(productSummaryRequest.getMerchantCode())).append(SolrConstants.CLOSING_BRACKET)
        .append(SolrConstants.SPACE + SolrConstants.AND_CLAUSE).append(SolrConstants.SPACE)
        .append(SolrConstants.OPEN_BRACKET).append(SolrConstants.SIZE_CHART_CODE).append(SolrConstants.COLON)
        .append(appendDoubleQuotes(productSummaryRequest.getSizeChartCode())).append(SolrConstants.CLOSING_BRACKET);
    solrQuery.setQuery(solrQueryString.toString());
    log.info("Solr Query for merchantCode {} at {} is {} ", productSummaryRequest.getMerchantCode(), new Date(),
        solrQuery);
  }

  private static void addFilterQueries(String storeId, ProductSummaryRequestVo productSummaryRequest, SolrQuery solrQuery) {
    if ((Objects.nonNull(productSummaryRequest.getSuspended())
        && !productSummaryRequest.getSuspended()) || Objects.isNull(productSummaryRequest.getSuspended())) {
      solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    }
    solrQuery.addFilterQuery(SolrFieldNames.STORE_ID + SolrConstants.COLON + storeId);
    if (Objects.nonNull(productSummaryRequest.getArchived())) {
      solrQuery.addFilterQuery(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + productSummaryRequest.getArchived());
    }
    if (Objects.nonNull(productSummaryRequest.getSuspended())) {
      solrQuery.addFilterQuery(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + productSummaryRequest.getSuspended());
    }
    if (Objects.nonNull(productSummaryRequest.getInStock())) {
      solrQuery.addFilterQuery(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + productSummaryRequest.getInStock());
    }

    addB2bAndB2cFilters(productSummaryRequest, solrQuery);
    if (Objects.nonNull(productSummaryRequest.getBundleProduct())) {
      if (productSummaryRequest.getBundleProduct()) {
        solrQuery.addFilterQuery(SolrFieldNames.BUNDLE_PRODUCT + SolrConstants.COLON + true);
      } else {
        solrQuery.addFilterQuery(SolrFieldNames.BUNDLE_PRODUCT + SolrConstants.COLON + false);
      }
    }
  }

  private static void addB2bAndB2cFilters(ProductSummaryRequestVo productSummaryRequest, SolrQuery solrQuery) {
    if (Objects.nonNull(productSummaryRequest.getB2bActivated())) {
      if (productSummaryRequest.getB2bActivated()) {
        solrQuery.addFilterQuery(SolrFieldNames.B2B_ACTIVATED + SolrConstants.COLON + true);
      } else {
        solrQuery.addFilterQuery(SolrFieldNames.B2B_ACTIVATED + SolrConstants.COLON + false);
      }
    }
    if (Objects.nonNull(productSummaryRequest.getB2cActivated())) {
      if (productSummaryRequest.getB2cActivated()) {
        solrQuery.addFilterQuery(SolrConstants.NOT_IN + SolrFieldNames.B2C_ACTIVATED + SolrConstants.COLON + false);
      } else {
        solrQuery.addFilterQuery(SolrFieldNames.B2C_ACTIVATED + SolrConstants.COLON + false);
      }
    }
  }

  private void setSort(ProductSummaryRequestVo productSummaryRequest, SolrQuery solrQuery) {
    if (StringUtils.isNotBlank(productSummaryRequest.getSortField())) {
      switch (productSummaryRequest.getSortField()) {
        case SolrConstants.SELLING_PRICE_SORT: {
          solrQuery.setSort(SolrFieldNames.MINIMUM_SELLING_PRICE,
              StringUtils.isNotBlank(productSummaryRequest.getSortOrder()) ?
                  (StringUtils.equalsIgnoreCase(productSummaryRequest.getSortOrder(), SolrConstants.ASC) ?
                      SolrQuery.ORDER.asc :
                      SolrQuery.ORDER.desc) :
                  SolrQuery.ORDER.desc);
          break;
        }
        case SolrConstants.NORMAL_PRICE_SORT: {
          solrQuery.setSort(SolrFieldNames.MINIMUM_LIST_PRICE,
              StringUtils.isNotBlank(productSummaryRequest.getSortOrder()) ?
                  (StringUtils.equalsIgnoreCase(productSummaryRequest.getSortOrder(), SolrConstants.ASC) ?
                      SolrQuery.ORDER.asc :
                      SolrQuery.ORDER.desc) :
                  SolrQuery.ORDER.desc);
          break;
        }
        //Adding this sort for suspension page
        case SolrConstants.UPDATED_DATE_SORT: {
          solrQuery.setSort(SolrFieldNames.UPDATED_DATE,
              StringUtils.isNotBlank(productSummaryRequest.getSortOrder()) ?
                  (StringUtils.equalsIgnoreCase(productSummaryRequest.getSortOrder(), SolrConstants.ASC) ?
                      SolrQuery.ORDER.asc :
                      SolrQuery.ORDER.desc) :
                  SolrQuery.ORDER.desc);
          break;
        }
        default: {
          solrQuery.setSort(SolrFieldNames.PRODUCT_SKU, StringUtils.isNotBlank(productSummaryRequest.getSortOrder()) ?
              (StringUtils.equalsIgnoreCase(productSummaryRequest.getSortOrder(), SolrConstants.ASC) ?
                  SolrQuery.ORDER.asc :
                  SolrQuery.ORDER.desc) :
              SolrQuery.ORDER.desc);
        }
      }
    } else {
      solrQuery.setSort(SolrFieldNames.PRODUCT_SKU, StringUtils.isNotBlank(productSummaryRequest.getSortOrder()) ?
          (StringUtils.equalsIgnoreCase(productSummaryRequest.getSortOrder(), SolrConstants.ASC) ?
              SolrQuery.ORDER.asc :
              SolrQuery.ORDER.desc) :
          SolrQuery.ORDER.desc);
    }
  }

  private String setPromoTypesQuery(String reviewType) {
    switch (reviewType) {
      case SolrConstants.IN_STORE_PROMO_TYPE : {
        return SolrFieldNames.OFF2ON_CHANNEL_ACTIVE + SolrConstants.COLON + Boolean.TRUE;
      }
      case SolrConstants.PREORDER_PROMO_TYPE : {
        return SolrFieldNames.IS_PRE_ORDER_ACTIVE + SolrConstants.COLON + Boolean.TRUE;
      }
      case SolrConstants.BUNDLE_PRODUCT_TYPE: {
        return SolrFieldNames.BUNDLE_PRODUCT + SolrConstants.COLON + Boolean.TRUE;
      }
      case SolrConstants.PROMO_PROMO_TYPE : {
        if (promoFilterEnabled) {
          return new StringBuilder(SolrFieldNames.PROMO_ITEM_SKUS).append(SolrConstants.COLON)
              .append(SolrConstants.SOLR_EXPRESSION_DELIMITER).toString();
        }
        return null;
      }
      case SolrConstants.WHOLESALE_PROMO_TYPE : {
        if (promoFilterEnabled) {
          return new StringBuilder(SolrFieldNames.WHOLESALE_ITEM_SKUS).append(SolrConstants.COLON)
              .append(SolrConstants.SOLR_EXPRESSION_DELIMITER).toString();
        }
        return null;
      }
    }
    return null;
  }

  private String appendDoubleQuotes(String string) {
    return new StringBuilder(SolrConstants.DOUBLE_QUOTE).append(string).append(SolrConstants.DOUBLE_QUOTE).toString();
  }

  private SolrQuery constructProductSkuSummaryQuery(
      ProductSkuSummaryRequestVo productSkuSummaryRequestVo, String businessPartnerCode) {
    SolrQuery solrQuery = new SolrQuery();
    StringBuilder solrQueryStringBuilder = null;

    if (StringUtils.isNotBlank(businessPartnerCode)) {
      solrQuery.set(SolrConstants.ROUTE_KEY, businessPartnerCode);
      solrQueryStringBuilder = new StringBuilder().append(SolrFieldNames.MERCHANT_CODE).append(SolrConstants.COLON)
          .append(businessPartnerCode);
    }

    if (StringUtils.isNotBlank(productSkuSummaryRequestVo.getProductSkuName())) {
      if (Objects.nonNull(solrQueryStringBuilder)) {
        solrQueryStringBuilder.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.PRODUCT_NAME)
            .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET)
            .append(ClientUtils.escapeQueryChars(productSkuSummaryRequestVo.getProductSkuName()))
            .append(SolrConstants.CLOSING_BRACKET);
      } else {
        solrQueryStringBuilder = new StringBuilder().append(SolrFieldNames.PRODUCT_NAME)
            .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET)
            .append(ClientUtils.escapeQueryChars(productSkuSummaryRequestVo.getProductSkuName()))
            .append(SolrConstants.CLOSING_BRACKET);
      }
    }

    if (CollectionUtils.isNotEmpty(productSkuSummaryRequestVo.getProductSkus())) {
      if (Objects.nonNull(solrQueryStringBuilder)) {
        solrQueryStringBuilder.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.PRODUCT_SKU)
            .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET)
            .append(String.join(SolrConstants.OR_CLAUSE, productSkuSummaryRequestVo.getProductSkus()))
            .append(SolrConstants.CLOSING_BRACKET);
      } else {
        solrQueryStringBuilder = new StringBuilder().append(SolrFieldNames.PRODUCT_SKU)
            .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET)
            .append(String.join(SolrConstants.OR_CLAUSE, productSkuSummaryRequestVo.getProductSkus()))
            .append(SolrConstants.CLOSING_BRACKET);
      }
    }

    if (CollectionUtils.isNotEmpty(productSkuSummaryRequestVo.getCategoryCodes())) {
      if (Objects.nonNull(solrQueryStringBuilder)) {
        solrQueryStringBuilder.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.MASTER_CATALOG)
            .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET)
            .append(String.join(SolrConstants.OR_CLAUSE, productSkuSummaryRequestVo.getCategoryCodes()))
            .append(SolrConstants.CLOSING_BRACKET);
      } else {
        solrQueryStringBuilder = new StringBuilder().append(SolrFieldNames.MASTER_CATALOG)
            .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET)
            .append(String.join(SolrConstants.OR_CLAUSE, productSkuSummaryRequestVo.getCategoryCodes()))
            .append(SolrConstants.CLOSING_BRACKET);
      }
    }

    if (CollectionUtils.isNotEmpty(productSkuSummaryRequestVo.getBrand())) {
      if (Objects.nonNull(solrQueryStringBuilder)) {
        solrQueryStringBuilder.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.BRAND).append(SolrConstants.COLON)
            .append(SolrConstants.OPEN_BRACKET)
            .append(String.join(SolrConstants.OR_CLAUSE, productSkuSummaryRequestVo.getBrand()))
            .append(SolrConstants.CLOSING_BRACKET);
      } else {
        solrQueryStringBuilder = new StringBuilder().append(SolrFieldNames.BRAND).append(SolrConstants.COLON)
            .append(SolrConstants.OPEN_BRACKET)
            .append(String.join(SolrConstants.OR_CLAUSE, productSkuSummaryRequestVo.getBrand()))
            .append(SolrConstants.CLOSING_BRACKET);
      }
    }


    if (Objects.nonNull(productSkuSummaryRequestVo.getIsArchived())) {
      solrQuery.addFilterQuery(
          new StringBuilder().append(SolrFieldNames.IS_ARCHIVED).append(SolrConstants.COLON)
              .append(productSkuSummaryRequestVo.getIsArchived()).toString());
    }

    solrQuery.setQuery(solrQueryStringBuilder.toString());
    return solrQuery;
  }

  private SolrQuery constructSolrQuery(String storeId, String categoryCode, String keyword, SolrQuery solrQuery) {
    solrQuery.setQuery(getStoreIdQuery(storeId));
    if (StringUtils.isNotBlank(categoryCode)) {
      solrQuery
          .setQuery(new StringBuilder(SolrFieldNames.SALES_CATALOG + SolrConstants.COLON + categoryCode).toString());
      solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    }
    if (StringUtils.isNotBlank(keyword)) {
      String[] keywordList = keyword.split(SolrConstants.COMMA);
      if (isQueryHasStoreId(solrQuery.getQuery(), storeId)) {
        solrQuery.setQuery(getKeywordQuery(keywordList));
        solrQuery.addFilterQuery(getStoreIdQuery(storeId));
      } else {
        String defaultQuery = solrQuery.getQuery();
        solrQuery.setQuery(defaultQuery + SolrConstants.AND_CLAUSE + getKeywordQuery(keywordList));
      }
    }
    return solrQuery;
  }

  private boolean isQueryHasStoreId(String query, String storeId) {
    return StringUtils.equals(query, getStoreIdQuery(storeId));
  }

  private String getStoreIdQuery(String storeId) {
    return SolrFieldNames.STORE_ID + SolrConstants.COLON + SolrConstants.DOUBLE_QUOTE + storeId
        + SolrConstants.DOUBLE_QUOTE;
  }

  private String getKeywordQuery(String[] keywordList) {
    StringBuilder defaultQuery = new StringBuilder();
    if (keywordList.length == 1) {
      defaultQuery.append(SolrConstants.OPEN_BRACKET).append(SolrFieldNames.PRODUCT_SKU).append(SolrConstants.COLON)
          .append(SolrConstants.DOUBLE_QUOTE).append(keywordList[0]).append(SolrConstants.DOUBLE_QUOTE)
          .append(SolrConstants.OR_CLAUSE).append(SolrFieldNames.PRODUCT_NAME).append(SolrConstants.COLON)
          .append(SolrConstants.DOUBLE_QUOTE).append(keywordList[0]).append(SolrConstants.DOUBLE_QUOTE)
          .append(SolrConstants.CLOSING_BRACKET);
    } else {
      for (int i = 0; i < keywordList.length; i++) {
        keywordList[i] = SolrConstants.DOUBLE_QUOTE + keywordList[i] + SolrConstants.DOUBLE_QUOTE;
      }
      defaultQuery.append(SolrFieldNames.PRODUCT_SKU).append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET)
          .append(String.join(SolrConstants.OR_CLAUSE, keywordList)).append(SolrConstants.CLOSING_BRACKET);
    }
    return defaultQuery.toString();
  }

  @Override
  public void executeSolrDocumentsAtomicUpdate(List<SolrInputDocument> solrInputDocuments) {
    try {
      cloudSolrClient.add(solrInputDocuments);
    } catch (SolrServerException | IOException | CloudSolrClient.RouteException e) {
      log.error("Failure while performing atomic updates on solrInputDocuments: {}", solrInputDocuments, e);
    }
  }

  @Override
  public List<ProductSolr> findByProductSkuList(String storeId, String merchantCode, Set<String> productSkus) {
    if (CollectionUtils.isEmpty(productSkus)) {
      return Collections.emptyList();
    }
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.set(SolrConstants.ROUTE_KEY, merchantCode);
    solrQuery.setQuery(
        SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON +
            getFQForListOfStrings(new ArrayList<>(productSkus)));
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.setFields(SolrFieldNames.MINIMUM_SELLING_PRICE,
        SolrFieldNames.MAXIMUM_SELLING_PRICE,
        SolrFieldNames.PRODUCT_SKU,
        SolrFieldNames.PRODUCT_NAME,
        SolrFieldNames.PROMO_ITEM_SKUS);

    return getProductSolrList(productSkus, solrQuery);
  }

  @Override
  public List<ProductSolr> findByProductSkuListForMFDTrueAndFalse(String storeId, String merchantCode,
      Set<String> productSkus) {
    if (CollectionUtils.isEmpty(productSkus)) {
      return Collections.emptyList();
    }
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.set(SolrConstants.ROUTE_KEY, merchantCode);
    solrQuery.setQuery(
        SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + getFQForListOfStrings(new ArrayList<>(productSkus)));
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    solrQuery.setFields(SolrFieldNames.MINIMUM_SELLING_PRICE, SolrFieldNames.MAXIMUM_SELLING_PRICE,
        SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_NAME, SolrFieldNames.PROMO_ITEM_SKUS);
    return getProductSolrList(productSkus, solrQuery);
  }

  private List<ProductSolr> getProductSolrList(Set<String> productSkus, SolrQuery solrQuery) {
    try {
      List<ProductSolr> productSolrsList = new ArrayList<>();
      int start = 0;
      long totalNumFound;
      do {
        solrQuery.setStart(start * solrMaxRowSize);
        solrQuery.setRows(solrMaxRowSize);
        QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
        totalNumFound = queryResponse.getResults().getNumFound();
        productSolrsList.addAll(queryResponse.getResults()
            .stream()
            .map(CommonUtil::toProductSolr)
            .collect(Collectors.toList()));
        start++;
      } while ((start * solrMaxRowSize) < totalNumFound);
      return productSolrsList;
    } catch (SolrServerException | IOException |
             SolrException e) {
      log.error("Exception caught while getting data by storeId, productSkus:{}", productSkus, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public Page<ProductSkuSizeChartResponse> getActiveProductsByStoreIdAndSizeChartCode(String sizeChartCode, int page,
      int size) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrFieldNames.SIZE_CHART_CODE + SolrConstants.COLON + appendDoubleQuotes(sizeChartCode));
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + false);
    solrQuery.setStart(page * size);
    solrQuery.setRows(size);
    try {
      QueryResponse queryResponse = cloudSolrClient.query(solrQuery);
      if (Objects.nonNull(queryResponse) && CollectionUtils.isNotEmpty(queryResponse.getResults())) {
        List<ProductSkuSizeChartResponse> productSkuSizeChartResponseList =
            queryResponse.getResults().stream().map(CommonUtil::toProductSkuSizeChartCode).collect(Collectors.toList());
        return new PageImpl<>(productSkuSizeChartResponseList, PageRequest.of(page, size),
            queryResponse.getResults().getNumFound());
      }
    } catch (SolrServerException | SolrException | IOException e) {
      log.error("Error while fetching products with size chart code : {} ", sizeChartCode, e);
    }
    return new PageImpl<>(new ArrayList<>(), PageRequest.of(page, size), 0);
  }

  private String getFQForListOfStrings(List<String> strings) {
    return new StringBuilder().append(SolrConstants.OPEN_BRACKET)
        .append(SolrConstants.DOUBLE_QUOTE)
        .append(String.join(SolrConstants.COMMA_WITH_QUOTES, strings))
        .append(SolrConstants.DOUBLE_QUOTE)
        .append(SolrConstants.CLOSING_BRACKET)
        .toString();
  }
}